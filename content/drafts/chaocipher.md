+++
title = "Chaocipher"
description = ""
date = 2019-10-06T22:19:40Z
aliases = []
[extra]
id = 21806
[taxonomies]
categories = []
tags = []
+++

{{task}}

'''Description'''

The [[wp:Chaocipher|Chaocipher]] was invented by J.F.Byrne in 1918 and, although simple by modern cryptographic standards, does not appear to have been broken until the algorithm was finally disclosed by his family in 2010.

The algorithm is described in [http://www.mountainvistasoft.com/chaocipher/ActualChaocipher/Chaocipher-Revealed-Algorithm.pdf this paper] by M.Rubin in 2010 and there is a C# implementation [https://www.c-sharpcorner.com/UploadFile/b942f9/implementing-the-chaocipher-in-C-Sharp/ here].


'''Task'''

The task is to code the algorithm in your language and to test that it works with the plaintext 'WELLDONEISBETTERTHANWELLSAID' used in the paper itself.


## Arc


```arc
(= lshift '((0 1) (2 14) (1 2) (14 26)))
(= rshift '((1 3) (4 15) (3 4) (15 26) (0 1)))

(= rot (fn (alpha shift)
  (let shift (mod shift 26)
    (string (cut alpha shift) (cut alpha 0 shift)))))

(= scramble-wheel (fn (alpha moves)
  (= oput '())
  (up i 0 (- (len moves) 1)
      (push (cut alpha ((moves i) 0) ((moves i) 1)) oput))
  (= oput (string (rev oput)))))

(= chaocipher (fn (left right msg (o crypted) (o dec?))
  (unless crypted
    (prn "Encoding " msg " with chaocipher")
    (prn left " " right))
  (when dec? (swap left right))
    (= offset ((positions (msg 0) right) 0))
    (= left (rot left offset))
    (= right (rot right offset))
    (push (cut left 0 1) crypted)
  (when dec? (swap left right))
  (prn (scramble-wheel left lshift)
   " " (scramble-wheel right rshift))
  (if (> (len msg) 1)
      (chaocipher (scramble-wheel left lshift)
             (scramble-wheel right rshift)
                  (cut msg 1) crypted dec?)
      (string (rev crypted)))))

(chaocipher "HXUCZVAMDSLKPEFJRIGTWOBNYQ" "PTLNBQDEOYSFAVZKGJRIHWXUMC"
            "WELLDONEISBETTERTHANWELLSAID")
(chaocipher "HXUCZVAMDSLKPEFJRIGTWOBNYQ" "PTLNBQDEOYSFAVZKGJRIHWXUMC"
            "OAHQHCNYNXTSZJRRHJBYHQKSOUJY" nil 1)

```


{{output}}

```arc

arc> (chaocipher "HXUCZVAMDSLKPEFJRIGTWOBNYQ" "PTLNBQDEOYSFAVZKGJRIHWXUMC"
                 "WELLDONEISBETTERTHANWELLSAID")
Encoding WELLDONEISBETTERTHANWELLSAID with chaocipher
HXUCZVAMDSLKPEFJRIGTWOBNYQ PTLNBQDEOYSFAVZKGJRIHWXUMC
ONYQHXUCZVAMDBSLKPEFJRIGTW XUCPTLNBQDEOYMSFAVZKGJRIHW
ADBSLKPEFJRIGMTWONYQHXUCZV OYSFAVZKGJRIHMWXUCPTLNBQDE
HUCZVADBSLKPEXFJRIGMTWONYQ NBDEOYSFAVZKGQJRIHMWXUCPTL
QUCZVADBSLKPEHXFJRIGMTWONY NBEOYSFAVZKGQDJRIHMWXUCPTL
HFJRIGMTWONYQXUCZVADBSLKPE JRHMWXUCPTLNBIEOYSFAVZKGQD
CVADBSLKPEHFJZRIGMTWONYQXU YSAVZKGQDJRHMFWXUCPTLNBIEO
NQXUCVADBSLKPYEHFJZRIGMTWO BIOYSAVZKGQDJERHMFWXUCPTLN
YHFJZRIGMTWONEQXUCVADBSLKP RHFWXUCPTLNBIMOYSAVZKGQDJE
NQXUCVADBSLKPEYHFJZRIGMTWO MOSAVZKGQDJERYHFWXUCPTLNBI
XCVADBSLKPEYHUFJZRIGMTWONQ AVKGQDJERYHFWZXUCPTLNBIMOS
TONQXCVADBSLKWPEYHUFJZRIGM IMSAVKGQDJERYOHFWZXUCPTLNB
SKWPEYHUFJZRILGMTONQXCVADB RYHFWZXUCPTLNOBIMSAVKGQDJE
ZILGMTONQXCVARDBSKWPEYHUFJ LNBIMSAVKGQDJOERYHFWZXUCPT
JILGMTONQXCVAZRDBSKWPEYHUF LNIMSAVKGQDJOBERYHFWZXUCPT
RBSKWPEYHUFJIDLGMTONQXCVAZ RYFWZXUCPTLNIHMSAVKGQDJOBE
RSKWPEYHUFJIDBLGMTONQXCVAZ YFZXUCPTLNIHMWSAVKGQDJOBER
HFJIDBLGMTONQUXCVAZRSKWPEY LNHMWSAVKGQDJIOBERYFZXUCPT
JDBLGMTONQUXCIVAZRSKWPEYHF MWAVKGQDJIOBESRYFZXUCPTLNH
BGMTONQUXCIVALZRSKWPEYHFJD VKQDJIOBESRYFGZXUCPTLNHMWA
YFJDBGMTONQUXHCIVALZRSKWPE HMAVKQDJIOBESWRYFGZXUCPTLN
HIVALZRSKWPEYCFJDBGMTONQUX RYGZXUCPTLNHMFAVKQDJIOBESW
QXHIVALZRSKWPUEYCFJDBGMTON SWYGZXUCPTLNHRMFAVKQDJIOBE
KPUEYCFJDBGMTWONQXHIVALZRS NHMFAVKQDJIOBRESWYGZXUCPTL
SPUEYCFJDBGMTKWONQXHIVALZR NHFAVKQDJIOBRMESWYGZXUCPTL
OQXHIVALZRSPUNEYCFJDBGMTKW WYZXUCPTLNHFAGVKQDJIOBRMES
UEYCFJDBGMTKWNOQXHIVALZRSP GVQDJIOBRMESWKYZXUCPTLNHFA
JBGMTKWNOQXHIDVALZRSPUEYCF OBMESWKYZXUCPRTLNHFAGVQDJI
YFJBGMTKWNOQXCHIDVALZRSPUE JIBMESWKYZXUCOPRTLNHFAGVQD
"OAHQHCNYNXTSZJRRHJBYHQKSOUJY"

```



## C

{{trans|Kotlin}}

```c>#include <stdio.h

#include <string.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0

typedef int bool;
typedef enum { ENCRYPT, DECRYPT } cmode;

const char *l_alphabet = "HXUCZVAMDSLKPEFJRIGTWOBNYQ";
const char *r_alphabet = "PTLNBQDEOYSFAVZKGJRIHWXUMC";

void chao(const char *in, char *out, cmode mode, bool show_steps) {
    int i, j, index;
    char store;
    size_t len = strlen(in);
    char left[27], right[27], temp[27];
    strcpy(left, l_alphabet);
    strcpy(right, r_alphabet);
    temp[26] = '\0';

    for (i = 0; i < len; ++i ) {
        if (show_steps) printf("%s  %s\n", left, right);
        if (mode == ENCRYPT) {
            index = strchr(right, in[i]) - right;
            out[i] = left[index];
        }
        else {
            index = strchr(left, in[i]) - left;
            out[i] = right[index];
        }
        if (i == len - 1) break;

        /* permute left */

        for (j = index; j < 26; ++j) temp[j - index] = left[j];
        for (j = 0; j < index; ++j) temp[26 - index + j] = left[j];
        store = temp[1];
        for (j = 2; j < 14; ++j) temp[j - 1] = temp[j];
        temp[13] = store;
        strcpy(left, temp);

        /* permute right */

        for (j = index; j < 26; ++j) temp[j - index] = right[j];
        for (j = 0; j < index; ++j) temp[26 - index + j] = right[j];
        store = temp[0];
        for (j = 1; j < 26; ++j) temp[j - 1] = temp[j];
        temp[25] = store;
        store = temp[2];
        for (j = 3; j < 14; ++j) temp[j - 1] = temp[j];
        temp[13] = store;
        strcpy(right, temp);
    }
}

int main() {
    const char *plain_text = "WELLDONEISBETTERTHANWELLSAID";
    char *cipher_text = malloc(strlen(plain_text) + 1);
    char *plain_text2 = malloc(strlen(plain_text) + 1);
    printf("The original plaintext is : %s\n", plain_text);
    printf("\nThe left and right alphabets after each permutation"
           " during encryption are :\n\n");
    chao(plain_text, cipher_text, ENCRYPT, TRUE);
    printf("\nThe ciphertext is : %s\n", cipher_text);
    chao(cipher_text, plain_text2, DECRYPT, FALSE);
    printf("\nThe recovered plaintext is : %s\n", plain_text2);
    free(cipher_text);
    free(plain_text2);
    return 0;
}
```


{{output}}

```txt

The original plaintext is : WELLDONEISBETTERTHANWELLSAID

The left and right alphabets after each permutation during encryption are :

HXUCZVAMDSLKPEFJRIGTWOBNYQ  PTLNBQDEOYSFAVZKGJRIHWXUMC
ONYQHXUCZVAMDBSLKPEFJRIGTW  XUCPTLNBQDEOYMSFAVZKGJRIHW
ADBSLKPEFJRIGMTWONYQHXUCZV  OYSFAVZKGJRIHMWXUCPTLNBQDE
HUCZVADBSLKPEXFJRIGMTWONYQ  NBDEOYSFAVZKGQJRIHMWXUCPTL
QUCZVADBSLKPEHXFJRIGMTWONY  NBEOYSFAVZKGQDJRIHMWXUCPTL
HFJRIGMTWONYQXUCZVADBSLKPE  JRHMWXUCPTLNBIEOYSFAVZKGQD
CVADBSLKPEHFJZRIGMTWONYQXU  YSAVZKGQDJRHMFWXUCPTLNBIEO
NQXUCVADBSLKPYEHFJZRIGMTWO  BIOYSAVZKGQDJERHMFWXUCPTLN
YHFJZRIGMTWONEQXUCVADBSLKP  RHFWXUCPTLNBIMOYSAVZKGQDJE
NQXUCVADBSLKPEYHFJZRIGMTWO  MOSAVZKGQDJERYHFWXUCPTLNBI
XCVADBSLKPEYHUFJZRIGMTWONQ  AVKGQDJERYHFWZXUCPTLNBIMOS
TONQXCVADBSLKWPEYHUFJZRIGM  IMSAVKGQDJERYOHFWZXUCPTLNB
SKWPEYHUFJZRILGMTONQXCVADB  RYHFWZXUCPTLNOBIMSAVKGQDJE
ZILGMTONQXCVARDBSKWPEYHUFJ  LNBIMSAVKGQDJOERYHFWZXUCPT
JILGMTONQXCVAZRDBSKWPEYHUF  LNIMSAVKGQDJOBERYHFWZXUCPT
RBSKWPEYHUFJIDLGMTONQXCVAZ  RYFWZXUCPTLNIHMSAVKGQDJOBE
RSKWPEYHUFJIDBLGMTONQXCVAZ  YFZXUCPTLNIHMWSAVKGQDJOBER
HFJIDBLGMTONQUXCVAZRSKWPEY  LNHMWSAVKGQDJIOBERYFZXUCPT
JDBLGMTONQUXCIVAZRSKWPEYHF  MWAVKGQDJIOBESRYFZXUCPTLNH
BGMTONQUXCIVALZRSKWPEYHFJD  VKQDJIOBESRYFGZXUCPTLNHMWA
YFJDBGMTONQUXHCIVALZRSKWPE  HMAVKQDJIOBESWRYFGZXUCPTLN
HIVALZRSKWPEYCFJDBGMTONQUX  RYGZXUCPTLNHMFAVKQDJIOBESW
QXHIVALZRSKWPUEYCFJDBGMTON  SWYGZXUCPTLNHRMFAVKQDJIOBE
KPUEYCFJDBGMTWONQXHIVALZRS  NHMFAVKQDJIOBRESWYGZXUCPTL
SPUEYCFJDBGMTKWONQXHIVALZR  NHFAVKQDJIOBRMESWYGZXUCPTL
OQXHIVALZRSPUNEYCFJDBGMTKW  WYZXUCPTLNHFAGVKQDJIOBRMES
UEYCFJDBGMTKWNOQXHIVALZRSP  GVQDJIOBRMESWKYZXUCPTLNHFA
JBGMTKWNOQXHIDVALZRSPUEYCF  OBMESWKYZXUCPRTLNHFAGVQDJI

The ciphertext is : OAHQHCNYNXTSZJRRHJBYHQKSOUJY

The recovered plaintext is : WELLDONEISBETTERTHANWELLSAID

```


=={{header|C#|C sharp}}==
{{trans|D}}

```csharp
using System;

namespace Chaocipher {
    enum Mode {
        ENCRYPT,
        DECRYPT,
    }

    class Program {
        const string L_ALPHABET = "HXUCZVAMDSLKPEFJRIGTWOBNYQ";
        const string R_ALPHABET = "PTLNBQDEOYSFAVZKGJRIHWXUMC";

        static string Exec(string text, Mode mode, bool showSteps = false) {
            char[] left = L_ALPHABET.ToCharArray();
            char[] right = R_ALPHABET.ToCharArray();
            char[] eText = new char[text.Length];
            char[] temp = new char[26];

            for (int i = 0; i < text.Length; ++i) {
                if (showSteps) Console.WriteLine("{0} {1}", string.Join("", left), string.Join("", right));
                int index = 0;
                if (mode == Mode.ENCRYPT) {
                    index = Array.IndexOf(right, text[i]);
                    eText[i] = left[index];
                } else {
                    index = Array.IndexOf(left, text[i]);
                    eText[i] = right[index];
                }
                if (i == text.Length - 1) break;

                // permute left

                for (int j = index; j < 26; ++j) temp[j - index] = left[j];
                for (int j = 0; j < index; ++j) temp[26 - index + j] = left[j];
                var store = temp[1];
                for (int j = 2; j < 14; ++j) temp[j - 1] = temp[j];
                temp[13] = store;
                temp.CopyTo(left, 0);

                // permute right

                for (int j = index; j < 26; ++j) temp[j - index] = right[j];
                for (int j = 0; j < index; ++j) temp[26 - index + j] = right[j];
                store = temp[0];
                for (int j = 1; j < 26; ++j) temp[j - 1] = temp[j];
                temp[25] = store;
                store = temp[2];
                for (int j = 3; j < 14; ++j) temp[j - 1] = temp[j];
                temp[13] = store;
                temp.CopyTo(right, 0);
            }

            return new string(eText);
        }

        static void Main(string[] args) {
            var plainText = "WELLDONEISBETTERTHANWELLSAID";
            Console.WriteLine("The original plaintext is : {0}", plainText);
            Console.WriteLine("\nThe left and right alphabets after each permutation during encryption are :\n");
            var cipherText = Exec(plainText, Mode.ENCRYPT, true);
            Console.WriteLine("\nThe ciphertext is : {0}", cipherText);
            var plainText2 = Exec(cipherText, Mode.DECRYPT);
            Console.WriteLine("\nThe recovered plaintext is : {0}", plainText2);
        }
    }
}
```

{{out}}

```txt
The original plaintext is : WELLDONEISBETTERTHANWELLSAID

The left and right alphabets after each permutation during encryption are :

HXUCZVAMDSLKPEFJRIGTWOBNYQ PTLNBQDEOYSFAVZKGJRIHWXUMC
ONYQHXUCZVAMDBSLKPEFJRIGTW XUCPTLNBQDEOYMSFAVZKGJRIHW
ADBSLKPEFJRIGMTWONYQHXUCZV OYSFAVZKGJRIHMWXUCPTLNBQDE
HUCZVADBSLKPEXFJRIGMTWONYQ NBDEOYSFAVZKGQJRIHMWXUCPTL
QUCZVADBSLKPEHXFJRIGMTWONY NBEOYSFAVZKGQDJRIHMWXUCPTL
HFJRIGMTWONYQXUCZVADBSLKPE JRHMWXUCPTLNBIEOYSFAVZKGQD
CVADBSLKPEHFJZRIGMTWONYQXU YSAVZKGQDJRHMFWXUCPTLNBIEO
NQXUCVADBSLKPYEHFJZRIGMTWO BIOYSAVZKGQDJERHMFWXUCPTLN
YHFJZRIGMTWONEQXUCVADBSLKP RHFWXUCPTLNBIMOYSAVZKGQDJE
NQXUCVADBSLKPEYHFJZRIGMTWO MOSAVZKGQDJERYHFWXUCPTLNBI
XCVADBSLKPEYHUFJZRIGMTWONQ AVKGQDJERYHFWZXUCPTLNBIMOS
TONQXCVADBSLKWPEYHUFJZRIGM IMSAVKGQDJERYOHFWZXUCPTLNB
SKWPEYHUFJZRILGMTONQXCVADB RYHFWZXUCPTLNOBIMSAVKGQDJE
ZILGMTONQXCVARDBSKWPEYHUFJ LNBIMSAVKGQDJOERYHFWZXUCPT
JILGMTONQXCVAZRDBSKWPEYHUF LNIMSAVKGQDJOBERYHFWZXUCPT
RBSKWPEYHUFJIDLGMTONQXCVAZ RYFWZXUCPTLNIHMSAVKGQDJOBE
RSKWPEYHUFJIDBLGMTONQXCVAZ YFZXUCPTLNIHMWSAVKGQDJOBER
HFJIDBLGMTONQUXCVAZRSKWPEY LNHMWSAVKGQDJIOBERYFZXUCPT
JDBLGMTONQUXCIVAZRSKWPEYHF MWAVKGQDJIOBESRYFZXUCPTLNH
BGMTONQUXCIVALZRSKWPEYHFJD VKQDJIOBESRYFGZXUCPTLNHMWA
YFJDBGMTONQUXHCIVALZRSKWPE HMAVKQDJIOBESWRYFGZXUCPTLN
HIVALZRSKWPEYCFJDBGMTONQUX RYGZXUCPTLNHMFAVKQDJIOBESW
QXHIVALZRSKWPUEYCFJDBGMTON SWYGZXUCPTLNHRMFAVKQDJIOBE
KPUEYCFJDBGMTWONQXHIVALZRS NHMFAVKQDJIOBRESWYGZXUCPTL
SPUEYCFJDBGMTKWONQXHIVALZR NHFAVKQDJIOBRMESWYGZXUCPTL
OQXHIVALZRSPUNEYCFJDBGMTKW WYZXUCPTLNHFAGVKQDJIOBRMES
UEYCFJDBGMTKWNOQXHIVALZRSP GVQDJIOBRMESWKYZXUCPTLNHFA
JBGMTKWNOQXHIDVALZRSPUEYCF OBMESWKYZXUCPRTLNHFAGVQDJI

The ciphertext is : OAHQHCNYNXTSZJRRHJBYHQKSOUJY

The recovered plaintext is : WELLDONEISBETTERTHANWELLSAID
```



## D

{{trans|Kotlin}}

```d
import std.stdio;
import std.string;

immutable L_ALPHABET = "HXUCZVAMDSLKPEFJRIGTWOBNYQ";
immutable R_ALPHABET = "PTLNBQDEOYSFAVZKGJRIHWXUMC";

enum Mode {
    ENCRYPT,
    DECRYPT,
}

string exec(string text, Mode mode, bool showSteps = false) {
    char[] left = L_ALPHABET.dup;
    char[] right = R_ALPHABET.dup;
    char[] eText;
    eText.length = text.length;
    char[26] temp;

    foreach (i; 0..text.length) {
        if (showSteps) writeln(left, ' ', right);
        int index;
        if (mode == Mode.ENCRYPT) {
            index = right.indexOf(text[i]);
            eText[i] = left[index];
        } else {
            index = left.indexOf(text[i]);
            eText[i] = right[index];
        }
        if (i == text.length - 1) break;

        // permute left

        foreach (j; index..26) temp[j - index] = left[j];
        foreach (j; 0..index) temp[26 - index + j] = left[j];
        auto store = temp[1];
        foreach (j; 2..14) temp[j - 1] = temp[j];
        temp[13] = store;
        left = temp.dup;

        // permute right

        foreach (j; index..26) temp[j - index] = right[j];
        foreach (j; 0..index) temp[26 - index + j] = right[j];
        store = temp[0];
        foreach (j; 1..26) temp[j - 1] = temp[j];
        temp[25] = store;
        store = temp[2];
        foreach (j; 3..14) temp[j - 1] = temp[j];
        temp[13] = store;
        right = temp.dup;
    }

    return eText.idup;
}

void main() {
    auto plainText = "WELLDONEISBETTERTHANWELLSAID";
    writeln("The original plaintext is : ", plainText);
    writeln("\nThe left and right alphabets after each permutation during encryption are :\n");
    auto cipherText = exec(plainText, Mode.ENCRYPT, true);
    writeln("\nThe ciphertext is : ", cipherText);
    auto plainText2 = exec(cipherText, Mode.DECRYPT);
    writeln("\nThe recovered plaintext is : ", plainText2);
}
```

{{out}}

```txt
The original plaintext is : WELLDONEISBETTERTHANWELLSAID

The left and right alphabets after each permutation during encryption are :

HXUCZVAMDSLKPEFJRIGTWOBNYQ PTLNBQDEOYSFAVZKGJRIHWXUMC
ONYQHXUCZVAMDBSLKPEFJRIGTW XUCPTLNBQDEOYMSFAVZKGJRIHW
ADBSLKPEFJRIGMTWONYQHXUCZV OYSFAVZKGJRIHMWXUCPTLNBQDE
HUCZVADBSLKPEXFJRIGMTWONYQ NBDEOYSFAVZKGQJRIHMWXUCPTL
QUCZVADBSLKPEHXFJRIGMTWONY NBEOYSFAVZKGQDJRIHMWXUCPTL
HFJRIGMTWONYQXUCZVADBSLKPE JRHMWXUCPTLNBIEOYSFAVZKGQD
CVADBSLKPEHFJZRIGMTWONYQXU YSAVZKGQDJRHMFWXUCPTLNBIEO
NQXUCVADBSLKPYEHFJZRIGMTWO BIOYSAVZKGQDJERHMFWXUCPTLN
YHFJZRIGMTWONEQXUCVADBSLKP RHFWXUCPTLNBIMOYSAVZKGQDJE
NQXUCVADBSLKPEYHFJZRIGMTWO MOSAVZKGQDJERYHFWXUCPTLNBI
XCVADBSLKPEYHUFJZRIGMTWONQ AVKGQDJERYHFWZXUCPTLNBIMOS
TONQXCVADBSLKWPEYHUFJZRIGM IMSAVKGQDJERYOHFWZXUCPTLNB
SKWPEYHUFJZRILGMTONQXCVADB RYHFWZXUCPTLNOBIMSAVKGQDJE
ZILGMTONQXCVARDBSKWPEYHUFJ LNBIMSAVKGQDJOERYHFWZXUCPT
JILGMTONQXCVAZRDBSKWPEYHUF LNIMSAVKGQDJOBERYHFWZXUCPT
RBSKWPEYHUFJIDLGMTONQXCVAZ RYFWZXUCPTLNIHMSAVKGQDJOBE
RSKWPEYHUFJIDBLGMTONQXCVAZ YFZXUCPTLNIHMWSAVKGQDJOBER
HFJIDBLGMTONQUXCVAZRSKWPEY LNHMWSAVKGQDJIOBERYFZXUCPT
JDBLGMTONQUXCIVAZRSKWPEYHF MWAVKGQDJIOBESRYFZXUCPTLNH
BGMTONQUXCIVALZRSKWPEYHFJD VKQDJIOBESRYFGZXUCPTLNHMWA
YFJDBGMTONQUXHCIVALZRSKWPE HMAVKQDJIOBESWRYFGZXUCPTLN
HIVALZRSKWPEYCFJDBGMTONQUX RYGZXUCPTLNHMFAVKQDJIOBESW
QXHIVALZRSKWPUEYCFJDBGMTON SWYGZXUCPTLNHRMFAVKQDJIOBE
KPUEYCFJDBGMTWONQXHIVALZRS NHMFAVKQDJIOBRESWYGZXUCPTL
SPUEYCFJDBGMTKWONQXHIVALZR NHFAVKQDJIOBRMESWYGZXUCPTL
OQXHIVALZRSPUNEYCFJDBGMTKW WYZXUCPTLNHFAGVKQDJIOBRMES
UEYCFJDBGMTKWNOQXHIVALZRSP GVQDJIOBRMESWKYZXUCPTLNHFA
JBGMTKWNOQXHIDVALZRSPUEYCF OBMESWKYZXUCPRTLNHFAGVQDJI

The ciphertext is : OAHQHCNYNXTSZJRRHJBYHQKSOUJY

The recovered plaintext is : WELLDONEISBETTERTHANWELLSAID
```


=={{header|F_Sharp|F#}}==

### The Functions


```fsharp

// Implement Chaocipher. Nigel Galloway: July 13th., 2019
let pL n=function g when g=n->0 |g when g=(n+1)%26->13 |g->let x=(25+g-n)%26 in if x<13 then x else x+1
let pR n=function g when g=n->25 |g when g=(n+3)%26->13 |g when g=(n+1)%26->0 |g when g=(n+2)%26->1 |g->let x=(24+g-n)%26 in if x<13 then x else x+1
let encrypt lW rW txt=Array.scan(fun (lW,rW) t->let n=Array.findIndex(fun n->n=t) rW in ((Array.permute(pL n) lW,(Array.permute(pR n) rW))))(lW,rW) txt
                        |>Array.skip 1|>Array.map(fun(n,_)->n.[0])|>System.String
let decrypt lW rW txt=Array.scan(fun (_,lW,rW) t->let n=Array.findIndex(fun n->n=t) lW in ((Array.item n rW,Array.permute(pL n) lW,(Array.permute(pR n) rW))))('0',lW,rW) txt
                        |>Array.skip 1|>Array.map(fun(n,_,_)->n)|>System.String

```



### The Task


```fsharp

printfn "%s" (encrypt ("HXUCZVAMDSLKPEFJRIGTWOBNYQ".ToCharArray()) ("PTLNBQDEOYSFAVZKGJRIHWXUMC".ToCharArray()) ("WELLDONEISBETTERTHANWELLSAID".ToCharArray()))
printfn "%s" (decrypt ("HXUCZVAMDSLKPEFJRIGTWOBNYQ".ToCharArray()) ("PTLNBQDEOYSFAVZKGJRIHWXUMC".ToCharArray()) ("OAHQHCNYNXTSZJRRHJBYHQKSOUJY".ToCharArray()))

```

{{out}}

```txt

OAHQHCNYNXTSZJRRHJBYHQKSOUJY
WELLDONEISBETTERTHANWELLSAID

```



## Factor


```factor
USING: arrays combinators fry io kernel locals math namespaces
prettyprint sequences sequences.extras strings ;
IN: rosetta-code.chaocipher

CONSTANT: zenith 0
CONSTANT: nadir  13

SYMBOLS: l-alphabet r-alphabet last-index ;

: init-alphabets ( -- )
    "HXUCZVAMDSLKPEFJRIGTWOBNYQ" l-alphabet
    "PTLNBQDEOYSFAVZKGJRIHWXUMC" r-alphabet [ set ] 2bi@ ;
    
: zero-alphabet ( seq -- seq' )
    last-index get rotate ;
    
: 3append ( a b c d -- abcd )
    append append append ;
    
:: permute-l-alphabet ( -- )
    l-alphabet get zero-alphabet dup
    zenith 1 + swap nth :> extracted-char
    {
        [ 1 head ]
        [ nadir 1 + head 2 tail ]
        [ drop extracted-char 1string ]
        [ nadir 1 + tail ]
    } cleave
    3append l-alphabet set ;
      
:: permute-r-alphabet ( -- )
    r-alphabet get zero-alphabet
    1 rotate dup
    zenith 2 + swap nth :> extracted-char
    {
        [ 2 head ]
        [ nadir 1 + head 3 tail ]
        [ drop extracted-char 1string ]
        [ nadir 1 + tail ]
    } cleave
    3append r-alphabet set ;
      
: encipher-char ( char alpha1 alpha2 -- char' )
    '[ _ get index dup last-index set _ get nth ] call ;
    
: encipher ( str quot -- str' )
    [ permute-l-alphabet permute-r-alphabet ] compose map
    init-alphabets ; inline
    
: encrypt ( str -- str' )
    [ r-alphabet l-alphabet encipher-char ] encipher ;
    
: decrypt ( str -- str' )
    [ l-alphabet r-alphabet encipher-char ] encipher ;

: main ( -- )
    init-alphabets
    "WELLDONEISBETTERTHANWELLSAID" encrypt dup decrypt
    [ print ] bi@ ;
    
MAIN: main
```

{{out}}

```txt

OAHQHCNYNXTSZJRRHJBYHQKSOUJY
WELLDONEISBETTERTHANWELLSAID

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Chaocipher this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go

{{trans|Kotlin}}

```go
package main

import(
    "fmt"
    "strings"
    "unicode/utf8"
)

type Mode int

const(
    Encrypt Mode = iota
    Decrypt
)

const(
    lAlphabet = "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
    rAlphabet = "PTLNBQDEOYSFAVZKGJRIHWXUMC"
)

func Chao(text string, mode Mode, showSteps bool) string {
    len := len(text)
    if utf8.RuneCountInString(text) != len {
        fmt.Println("Text contains non-ASCII characters")
        return ""
    }
    left  := lAlphabet
    right := rAlphabet
    eText := make([]byte, len)
    temp  := make([]byte, 26)

    for i := 0; i < len; i++ {
        if showSteps {
            fmt.Println(left, " ", right)
        }
        var index int
        if mode == Encrypt {
            index = strings.IndexByte(right, text[i])
            eText[i] = left[index]
        } else {
            index = strings.IndexByte(left, text[i])
            eText[i] = right[index]
        }
        if i == len - 1 {
            break
        }

        // permute left
        for j := index; j < 26; j++ {
            temp[j - index] = left[j]
        }
        for j := 0; j < index; j++ {
            temp[26 - index + j] = left[j]
        }
        store := temp[1]
        for j := 2; j < 14; j++ {
            temp[j - 1] = temp[j]
        }
        temp[13] = store
        left = string(temp[:])

        // permute right

        for j := index; j < 26; j++ {
            temp[j - index] = right[j]
        }
        for j := 0; j < index; j++ {
            temp[26 - index + j] = right[j]
        }
        store = temp[0]
        for j := 1; j < 26; j++ {
            temp[j - 1] = temp[j]
        }
        temp[25] = store
        store = temp[2]
        for j := 3; j < 14; j++ {
            temp[j - 1] = temp[j]
        }
        temp[13] = store
        right = string(temp[:])
    }

    return string(eText[:])
}

func main() {
    plainText := "WELLDONEISBETTERTHANWELLSAID"
    fmt.Println("The original plaintext is :", plainText)
    fmt.Print("\nThe left and right alphabets after each permutation ")
    fmt.Println("during encryption are :\n")
    cipherText := Chao(plainText, Encrypt, true)
    fmt.Println("\nThe ciphertext is :",  cipherText)
    plainText2 := Chao(cipherText, Decrypt, false)
    fmt.Println("\nThe recovered plaintext is :", plainText2)
}
```


{{out}}

```txt

Same as Kotlin entry.

```



## Haskell


```haskell
import Data.List (elemIndex)

chao :: Eq a => [a] -> [a] -> Bool -> [a] -> [a]
chao _ _ _ [] = []
chao l r plain (x:xs) = maybe [] go (elemIndex x src)
  where
    (src, dst)
      | plain = (l, r)
      | otherwise = (r, l)
    go n =
      dst !! n :
      chao
        (shifted 1 14 (rotated n l))
        ((shifted 2 14 . shifted 0 26) (rotated n r))
        plain
        xs

rotated :: Int -> [a] -> [a]
rotated z s = take (length s) (drop z (cycle s))

shifted :: Int -> Int -> [a] -> [a]
shifted src dst s =
  let (a, b) = splitAt dst s
      (x, y) = splitAt src a
  in concat [x, rotated 1 y, b]

encode = False

decode = True

main :: IO ()
main = do
  let chaoWheels =
        chao "HXUCZVAMDSLKPEFJRIGTWOBNYQ" "PTLNBQDEOYSFAVZKGJRIHWXUMC"
      plainText = "WELLDONEISBETTERTHANWELLSAID"
      cipherText = chaoWheels encode plainText
  print plainText
  print cipherText
  print $ chaoWheels decode cipherText
```

{{Out}}

```txt

"WELLDONEISBETTERTHANWELLSAID"
"OAHQHCNYNXTSZJRRHJBYHQKSOUJY"
"WELLDONEISBETTERTHANWELLSAID"
```



## JavaScript

{{trans|C}}
Script source

```javascript
const L_ALPHABET = "HXUCZVAMDSLKPEFJRIGTWOBNYQ";
const R_ALPHABET = "PTLNBQDEOYSFAVZKGJRIHWXUMC";

const ENCRYPT = 0;
const DECRYPT = 1;

function setCharAt(str, index, chr) {
    if (index > str.length - 1) return str;
    return str.substr(0, index) + chr + str.substr(index + 1);
}

function chao(text, mode, show_steps) {
    var left = L_ALPHABET;
    var right = R_ALPHABET;
    var out = text;
    var temp = "01234567890123456789012345";
    var i = 0;
    var index, j, store;

    if (show_steps) {
        console.log("The left and right alphabets after each permutation during encryption are :");
    }
    while (i < text.length) {
        if (show_steps) {
            console.log(left + "  " + right);
        }
        if (mode == ENCRYPT) {
            index = right.indexOf(text[i]);
            out = setCharAt(out, i, left[index]);
        } else {
            index = left.indexOf(text[i]);
            out = setCharAt(out, i, right[index]);
        }
        if (i == text.length - 1) {
            break;
        }

        //permute left
        j = index;
        while (j < 26) {
            temp = setCharAt(temp, j - index, left[j])
            j += 1;
        }
        j = 0;
        while (j < index) {
            temp = setCharAt(temp, 26 - index + j, left[j]);
            j += 1;
        }
        store = temp[1];
        j = 2;
        while (j < 14) {
            temp = setCharAt(temp, j - 1, temp[j]);
            j += 1;
        }
        temp = setCharAt(temp, 13, store);
        left = temp;

        //permute right
        j = index;
        while (j < 26) {
            temp = setCharAt(temp, j - index, right[j]);
            j += 1;
        }
        j = 0;
        while (j < index) {
            temp = setCharAt(temp, 26 - index + j, right[j]);
            j += 1;
        }
        store = temp[0];
        j = 1;
        while (j < 26) {
            temp = setCharAt(temp, j - 1, temp[j]);
            j += 1;
        }
        temp = setCharAt(temp, 25, store);
        store = temp[2];
        j = 3;
        while (j < 14) {
            temp = setCharAt(temp, j - 1, temp[j]);
            j += 1;
        }
        temp = setCharAt(temp, 13, store);
        right = temp;

        i += 1;
    }

    return out;
}

function main() {
    var out = document.getElementById("content");
    const plain_text = "WELLDONEISBETTERTHANWELLSAID";

    out.innerHTML = "<p>The original plaintext is : " + plain_text + "</p>";
    var cipher_text = chao(plain_text, ENCRYPT, true);
    out.innerHTML += "<p>The ciphertext is : " + cipher_text + "</p>";
    var decipher_text = chao(cipher_text, DECRYPT, false);
    out.innerHTML += "<p>The recovered plaintext is : " + decipher_text + "</p>";
}
```


Solution page

```html
<!DOCTYPE html>
<html>
    <head>
        <title>Chaocipher</title>
        <script src="chaocipher.js"></script>
    </head>
    <body onload="main()">
        <div id="content"></div>
    </body>
</html>
```

{{out}}

```txt
The original plaintext is : WELLDONEISBETTERTHANWELLSAID
The ciphertext is : OAHQHCNYNXTSZJRRHJBYHQKSOUJY
The recovered plaintext is : WELLDONEISBETTERTHANWELLSAID
```



## Julia

Modified from the Kotlin and Perl 6 entries.

```julia
const leftalphabet = "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
const rightalphabet = "PTLNBQDEOYSFAVZKGJRIHWXUMC"

function chacocoding(text, encoding, verbose=false)
    left, right = Vector{Char}(leftalphabet), Vector{Char}(rightalphabet)
    len, coded = length(text), similar(Vector{Char}(text))
    for i in 1:len
        verbose && println(String(left), "   ", String(right))
        n = indexin(text[i], encoding ? right : left)[1]
        coded[i] = encoding ? left[n] : right[n]
        if i < len
            left .= circshift(left, -n + 1)
            left[2:14] .= circshift(left[2:14], -1)
            right .= circshift(right, -n)
            right[3:14] .= circshift(right[3:14], -1)
        end
    end
    String(coded)
end

function testchacocipher(txt)
    println("The original plaintext is: $txt")
    println("\nThe left and right alphabets for each character during encryption are:")
    encoded = chacocoding(txt, true, true)
    println("\nThe encoded ciphertext is: $encoded")
    decoded = chacocoding(encoded, false)
    println("\nDecoded, the recovered plaintext is: $decoded")
end

testchacocipher("WELLDONEISBETTERTHANWELLSAID")

```
{{out}}

```txt

The original plaintext is: WELLDONEISBETTERTHANWELLSAID

The left and right alphabets for each character during encryption are:
HXUCZVAMDSLKPEFJRIGTWOBNYQ   PTLNBQDEOYSFAVZKGJRIHWXUMC
ONYQHXUCZVAMDBSLKPEFJRIGTW   XUCPTLNBQDEOYMSFAVZKGJRIHW
ADBSLKPEFJRIGMTWONYQHXUCZV   OYSFAVZKGJRIHMWXUCPTLNBQDE
HUCZVADBSLKPEXFJRIGMTWONYQ   NBDEOYSFAVZKGQJRIHMWXUCPTL
QUCZVADBSLKPEHXFJRIGMTWONY   NBEOYSFAVZKGQDJRIHMWXUCPTL
HFJRIGMTWONYQXUCZVADBSLKPE   JRHMWXUCPTLNBIEOYSFAVZKGQD
CVADBSLKPEHFJZRIGMTWONYQXU   YSAVZKGQDJRHMFWXUCPTLNBIEO
NQXUCVADBSLKPYEHFJZRIGMTWO   BIOYSAVZKGQDJERHMFWXUCPTLN
YHFJZRIGMTWONEQXUCVADBSLKP   RHFWXUCPTLNBIMOYSAVZKGQDJE
NQXUCVADBSLKPEYHFJZRIGMTWO   MOSAVZKGQDJERYHFWXUCPTLNBI
XCVADBSLKPEYHUFJZRIGMTWONQ   AVKGQDJERYHFWZXUCPTLNBIMOS
TONQXCVADBSLKWPEYHUFJZRIGM   IMSAVKGQDJERYOHFWZXUCPTLNB
SKWPEYHUFJZRILGMTONQXCVADB   RYHFWZXUCPTLNOBIMSAVKGQDJE
ZILGMTONQXCVARDBSKWPEYHUFJ   LNBIMSAVKGQDJOERYHFWZXUCPT
JILGMTONQXCVAZRDBSKWPEYHUF   LNIMSAVKGQDJOBERYHFWZXUCPT
RBSKWPEYHUFJIDLGMTONQXCVAZ   RYFWZXUCPTLNIHMSAVKGQDJOBE
RSKWPEYHUFJIDBLGMTONQXCVAZ   YFZXUCPTLNIHMWSAVKGQDJOBER
HFJIDBLGMTONQUXCVAZRSKWPEY   LNHMWSAVKGQDJIOBERYFZXUCPT
JDBLGMTONQUXCIVAZRSKWPEYHF   MWAVKGQDJIOBESRYFZXUCPTLNH
BGMTONQUXCIVALZRSKWPEYHFJD   VKQDJIOBESRYFGZXUCPTLNHMWA
YFJDBGMTONQUXHCIVALZRSKWPE   HMAVKQDJIOBESWRYFGZXUCPTLN
HIVALZRSKWPEYCFJDBGMTONQUX   RYGZXUCPTLNHMFAVKQDJIOBESW
QXHIVALZRSKWPUEYCFJDBGMTON   SWYGZXUCPTLNHRMFAVKQDJIOBE
KPUEYCFJDBGMTWONQXHIVALZRS   NHMFAVKQDJIOBRESWYGZXUCPTL
SPUEYCFJDBGMTKWONQXHIVALZR   NHFAVKQDJIOBRMESWYGZXUCPTL
OQXHIVALZRSPUNEYCFJDBGMTKW   WYZXUCPTLNHFAGVKQDJIOBRMES
UEYCFJDBGMTKWNOQXHIVALZRSP   GVQDJIOBRMESWKYZXUCPTLNHFA
JBGMTKWNOQXHIDVALZRSPUEYCF   OBMESWKYZXUCPRTLNHFAGVQDJI

The encoded ciphertext is: OAHQHCNYNXTSZJRRHJBYHQKSOUJY

Decoded, the recovered plaintext is: WELLDONEISBETTERTHANWELLSAID

```



## Kotlin

This is based on the C# implementation referred to in the task description, except that the encrypt and decrypt operations are combined into a single method.

```scala
// Version 1.2.40

enum class Mode { ENCRYPT, DECRYPT }

object Chao {
    private val lAlphabet = "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
    private val rAlphabet = "PTLNBQDEOYSFAVZKGJRIHWXUMC"

    fun exec(text: String, mode: Mode, showSteps: Boolean = false): String {
        var left  = lAlphabet
        var right = rAlphabet
        val eText = CharArray(text.length)
        val temp  = CharArray(26)

        for (i in 0 until text.length) {
            if (showSteps) println("$left  $right")
            var index: Int
            if (mode == Mode.ENCRYPT) {
                index = right.indexOf(text[i])
                eText[i] = left[index]
            }
            else {
                index = left.indexOf(text[i])
                eText[i] = right[index]
            }
            if (i == text.length - 1) break

            // permute left

            for (j in index..25) temp[j - index] = left[j]
            for (j in 0 until index) temp[26 - index + j] = left[j]
            var store = temp[1]
            for (j in 2..13) temp[j - 1] = temp[j]
            temp[13] = store
            left = String(temp)

            // permute right

            for (j in index..25) temp[j - index] = right[j]
            for (j in 0 until index) temp[26 - index + j] = right[j]
            store = temp[0]
            for (j in 1..25) temp[j - 1] = temp[j]
            temp[25] = store
            store = temp[2]
            for (j in 3..13) temp[j - 1] = temp[j]
            temp[13] = store
            right = String(temp)
        }

        return String(eText)
    }
}

fun main(args: Array<String>) {
    val plainText = "WELLDONEISBETTERTHANWELLSAID"
    println("The original plaintext is : $plainText")
    println("\nThe left and right alphabets after each permutation" +
             " during encryption are :\n")
    val cipherText = Chao.exec(plainText, Mode.ENCRYPT, true)
    println("\nThe ciphertext is : $cipherText")
    val plainText2 = Chao.exec(cipherText, Mode.DECRYPT)
    println("\nThe recovered plaintext is : $plainText2")
}
```


{{output}}

```txt

The original plaintext is : WELLDONEISBETTERTHANWELLSAID

The left and right alphabets after each permutation during encryption are :

HXUCZVAMDSLKPEFJRIGTWOBNYQ  PTLNBQDEOYSFAVZKGJRIHWXUMC
ONYQHXUCZVAMDBSLKPEFJRIGTW  XUCPTLNBQDEOYMSFAVZKGJRIHW
ADBSLKPEFJRIGMTWONYQHXUCZV  OYSFAVZKGJRIHMWXUCPTLNBQDE
HUCZVADBSLKPEXFJRIGMTWONYQ  NBDEOYSFAVZKGQJRIHMWXUCPTL
QUCZVADBSLKPEHXFJRIGMTWONY  NBEOYSFAVZKGQDJRIHMWXUCPTL
HFJRIGMTWONYQXUCZVADBSLKPE  JRHMWXUCPTLNBIEOYSFAVZKGQD
CVADBSLKPEHFJZRIGMTWONYQXU  YSAVZKGQDJRHMFWXUCPTLNBIEO
NQXUCVADBSLKPYEHFJZRIGMTWO  BIOYSAVZKGQDJERHMFWXUCPTLN
YHFJZRIGMTWONEQXUCVADBSLKP  RHFWXUCPTLNBIMOYSAVZKGQDJE
NQXUCVADBSLKPEYHFJZRIGMTWO  MOSAVZKGQDJERYHFWXUCPTLNBI
XCVADBSLKPEYHUFJZRIGMTWONQ  AVKGQDJERYHFWZXUCPTLNBIMOS
TONQXCVADBSLKWPEYHUFJZRIGM  IMSAVKGQDJERYOHFWZXUCPTLNB
SKWPEYHUFJZRILGMTONQXCVADB  RYHFWZXUCPTLNOBIMSAVKGQDJE
ZILGMTONQXCVARDBSKWPEYHUFJ  LNBIMSAVKGQDJOERYHFWZXUCPT
JILGMTONQXCVAZRDBSKWPEYHUF  LNIMSAVKGQDJOBERYHFWZXUCPT
RBSKWPEYHUFJIDLGMTONQXCVAZ  RYFWZXUCPTLNIHMSAVKGQDJOBE
RSKWPEYHUFJIDBLGMTONQXCVAZ  YFZXUCPTLNIHMWSAVKGQDJOBER
HFJIDBLGMTONQUXCVAZRSKWPEY  LNHMWSAVKGQDJIOBERYFZXUCPT
JDBLGMTONQUXCIVAZRSKWPEYHF  MWAVKGQDJIOBESRYFZXUCPTLNH
BGMTONQUXCIVALZRSKWPEYHFJD  VKQDJIOBESRYFGZXUCPTLNHMWA
YFJDBGMTONQUXHCIVALZRSKWPE  HMAVKQDJIOBESWRYFGZXUCPTLN
HIVALZRSKWPEYCFJDBGMTONQUX  RYGZXUCPTLNHMFAVKQDJIOBESW
QXHIVALZRSKWPUEYCFJDBGMTON  SWYGZXUCPTLNHRMFAVKQDJIOBE
KPUEYCFJDBGMTWONQXHIVALZRS  NHMFAVKQDJIOBRESWYGZXUCPTL
SPUEYCFJDBGMTKWONQXHIVALZR  NHFAVKQDJIOBRMESWYGZXUCPTL
OQXHIVALZRSPUNEYCFJDBGMTKW  WYZXUCPTLNHFAGVKQDJIOBRMES
UEYCFJDBGMTKWNOQXHIVALZRSP  GVQDJIOBRMESWKYZXUCPTLNHFA
JBGMTKWNOQXHIDVALZRSPUEYCF  OBMESWKYZXUCPRTLNHFAGVQDJI

The ciphertext is : OAHQHCNYNXTSZJRRHJBYHQKSOUJY

The recovered plaintext is : WELLDONEISBETTERTHANWELLSAID

```



## Objeck

{{trans|Kotlin}}

```objeck
class Chaocipher {
  L_ALPHABET : static : Char[];
  R_ALPHABET : static : Char[];

  function : Main(args : String[]) ~ Nil {
    L_ALPHABET := "HXUCZVAMDSLKPEFJRIGTWOBNYQ"->ToCharArray();
    R_ALPHABET := "PTLNBQDEOYSFAVZKGJRIHWXUMC"->ToCharArray();
    plainText := "WELLDONEISBETTERTHANWELLSAID"->ToCharArray();

    System.IO.Console->Print("The original plaintext is: ")->PrintLine(plainText);
    "\nThe left and right alphabets after each permutation during encryption are:\n"->PrintLine();
    cipherText := Chao(plainText, Mode->ENCRYPT, true);
    System.IO.Console->Print("\nThe ciphertext is: ")->PrintLine(cipherText);
    plainText2 := Chao(cipherText, Mode->DECRYPT, false);
    System.IO.Console->Print("The recovered plaintext is: ")->PrintLine(plainText2);
  }

  function : Chao(in : Char[], mode : Mode, show_steps : Bool) ~ Char[] {  
    i : Int; j : Int; index : Int;
    store : Char;
    len := in->Size();
    left := Char->New[26];   right := Char->New[26]; temp := Char->New[26];
    eText := Char->New[len];

    Runtime->Copy(left, 0, L_ALPHABET, 0, L_ALPHABET->Size());
    Runtime->Copy(right, 0, R_ALPHABET, 0, R_ALPHABET->Size());

    for(i := 0; i < len; i += 1;) {
      if (show_steps) {
        System.IO.Console->Print(left)->Print(' ')->PrintLine(right);
      };
      if (mode = Mode->ENCRYPT) {
        index := IndexOf(right, in[i]);
        eText[i] := left[index];
      }
      else {
        index := IndexOf(left, in[i]);
        eText[i] := right[index];
      };

      if (i = len - 1) {
        break;
      };

      # left
      for(j := index; j < 26; j += 1;) { temp[j - index] := left[j]; };
      for(j :=0; j < index; j += 1;) { temp[26 - index + j] := left[j]; };
      store := temp[1];
      for(j := 2; j < 14; j += 1;) { temp[j - 1] := temp[j]; };
      temp[13] := store;
      Runtime->Copy(left, 0, temp, 0, temp->Size());

      # right
      for(j := index; j < 26; j += 1;) { temp[j - index] := right[j]; };
      for(j :=0; j < index; j += 1;) { temp[26 - index + j] := right[j]; };
      store := temp[0];
      for(j :=1; j < 26; j += 1;) { temp[j - 1] := temp[j]; };
          temp[25] := store;
          store := temp[2];
          for(j := 3; j < 14; j += 1;) { temp[j - 1] := temp[j]; };
          temp[13] := store;
          Runtime->Copy(right, 0, temp, 0, temp->Size());
    };
        
    return eText;
  }

  function : IndexOf(str : Char[], c : Char) ~ Int {
    for(i := 0; i < str->Size(); i += 1;) {
      if(c = str[i]) {
        return i;
      };
    };

    return -1;
  }

  enum Mode { ENCRYPT, DECRYPT }  
}
```


{{output}}

```txt

The original plaintext is: WELLDONEISBETTERTHANWELLSAID

The left and right alphabets after each permutation during encryption are:

HXUCZVAMDSLKPEFJRIGTWOBNYQ PTLNBQDEOYSFAVZKGJRIHWXUMC
ONYQHXUCZVAMDBSLKPEFJRIGTW XUCPTLNBQDEOYMSFAVZKGJRIHW
ADBSLKPEFJRIGMTWONYQHXUCZV OYSFAVZKGJRIHMWXUCPTLNBQDE
HUCZVADBSLKPEXFJRIGMTWONYQ NBDEOYSFAVZKGQJRIHMWXUCPTL
QUCZVADBSLKPEHXFJRIGMTWONY NBEOYSFAVZKGQDJRIHMWXUCPTL
HFJRIGMTWONYQXUCZVADBSLKPE JRHMWXUCPTLNBIEOYSFAVZKGQD
CVADBSLKPEHFJZRIGMTWONYQXU YSAVZKGQDJRHMFWXUCPTLNBIEO
NQXUCVADBSLKPYEHFJZRIGMTWO BIOYSAVZKGQDJERHMFWXUCPTLN
YHFJZRIGMTWONEQXUCVADBSLKP RHFWXUCPTLNBIMOYSAVZKGQDJE
NQXUCVADBSLKPEYHFJZRIGMTWO MOSAVZKGQDJERYHFWXUCPTLNBI
XCVADBSLKPEYHUFJZRIGMTWONQ AVKGQDJERYHFWZXUCPTLNBIMOS
TONQXCVADBSLKWPEYHUFJZRIGM IMSAVKGQDJERYOHFWZXUCPTLNB
SKWPEYHUFJZRILGMTONQXCVADB RYHFWZXUCPTLNOBIMSAVKGQDJE
ZILGMTONQXCVARDBSKWPEYHUFJ LNBIMSAVKGQDJOERYHFWZXUCPT
JILGMTONQXCVAZRDBSKWPEYHUF LNIMSAVKGQDJOBERYHFWZXUCPT
RBSKWPEYHUFJIDLGMTONQXCVAZ RYFWZXUCPTLNIHMSAVKGQDJOBE
RSKWPEYHUFJIDBLGMTONQXCVAZ YFZXUCPTLNIHMWSAVKGQDJOBER
HFJIDBLGMTONQUXCVAZRSKWPEY LNHMWSAVKGQDJIOBERYFZXUCPT
JDBLGMTONQUXCIVAZRSKWPEYHF MWAVKGQDJIOBESRYFZXUCPTLNH
BGMTONQUXCIVALZRSKWPEYHFJD VKQDJIOBESRYFGZXUCPTLNHMWA
YFJDBGMTONQUXHCIVALZRSKWPE HMAVKQDJIOBESWRYFGZXUCPTLN
HIVALZRSKWPEYCFJDBGMTONQUX RYGZXUCPTLNHMFAVKQDJIOBESW
QXHIVALZRSKWPUEYCFJDBGMTON SWYGZXUCPTLNHRMFAVKQDJIOBE
KPUEYCFJDBGMTWONQXHIVALZRS NHMFAVKQDJIOBRESWYGZXUCPTL
SPUEYCFJDBGMTKWONQXHIVALZR NHFAVKQDJIOBRMESWYGZXUCPTL
OQXHIVALZRSPUNEYCFJDBGMTKW WYZXUCPTLNHFAGVKQDJIOBRMES
UEYCFJDBGMTKWNOQXHIVALZRSP GVQDJIOBRMESWKYZXUCPTLNHFA
JBGMTKWNOQXHIDVALZRSPUEYCF OBMESWKYZXUCPRTLNHFAGVQDJI

The ciphertext is: OAHQHCNYNXTSZJRRHJBYHQKSOUJY
The recovered plaintext is: WELLDONEISBETTERTHANWELLSAID

```



## Perl

{{trans|Perl 6}}
Since <tt>rotate</tt> is not a built-in in Perl, using a custom one, not general-purpose but sufficient for this task.

```perl
sub init {
    @left  = split '', 'HXUCZVAMDSLKPEFJRIGTWOBNYQ';
    @right = split '', 'PTLNBQDEOYSFAVZKGJRIHWXUMC';
}

sub encode {
    my($letter) = @_;
    my $index = index join('', @right), $letter;
    my $enc   = $left[$index];
    left_permute($index);
    right_permute($index);
    $enc
}

sub decode {
    my($letter) = @_;
    my $index = index join('', @left), $letter;
    my $dec   = $right[$index];
    left_permute($index);
    right_permute($index);
    $dec
}

sub right_permute {
    my($index) = @_;
    rotate(\@right, $index + 1);
    rotate(\@right, 1, 2, 13);
}

sub left_permute {
    my($index) = @_;
    rotate(\@left, $index);
    rotate(\@left, 1, 1, 13);
}

sub rotate {
    our @list; local *list = shift;
    my($n,$s,$e) = @_;
    @list = $s ? @list[0..$s-1, $s+$n..$e+$n-1, $s..$s+$n-1, $e+1..$#list]
               : @list[$n..$#list, 0..$n-1]
}

init; $e_msg .= encode($_) for split '', 'WELLDONEISBETTERTHANWELLSAID';
init; $d_msg .= decode($_) for split '', $e_msg;

print "$e_msg\n";
print "$d_msg\n";
```

{{out}}

```txt
OAHQHCNYNXTSZJRRHJBYHQKSOUJY
WELLDONEISBETTERTHANWELLSAID
```



## Perl 6

{{works with|Rakudo|2018.03}}


```perl6
my @left;
my @right;

sub reset {
    @left  = <HXUCZVAMDSLKPEFJRIGTWOBNYQ>.comb;
    @right = <PTLNBQDEOYSFAVZKGJRIHWXUMC>.comb;
}

sub encode ($letter) {
    my $index = @right.first: $letter.uc, :k;
    my $enc   = @left[$index];
    $index.&permute;
    $enc
}

sub decode ($letter) {
    my $index = @left.first: $letter.uc, :k;
    my $dec   = @right[$index];
    $index.&permute;
    $dec
}

sub permute ($index) {
    @left.=rotate: $index;
    @left[1..13].=rotate;
    @right.=rotate: $index + 1;
    @right[2..13].=rotate;
}

reset;
say 'WELLDONEISBETTERTHANWELLSAID'.comb».&encode.join;
reset;
say 'OAHQHCNYNXTSZJRRHJBYHQKSOUJY'.comb».&decode.join;
```

{{out}}

```txt
OAHQHCNYNXTSZJRRHJBYHQKSOUJY
WELLDONEISBETTERTHANWELLSAID
```



## Phix

Originally translated from C, but ended up more of a direct implementation of the algorithm in the pdf.

```Phix
-- demo\rosetta\Chao_cipher.exw
constant l_alphabet = "HXUCZVAMDSLKPEFJRIGTWOBNYQ",
         r_alphabet = "PTLNBQDEOYSFAVZKGJRIHWXUMC"

enum ENCRYPT, DECRYPT
 
function chao_cipher(string in, integer mode, bool show_steps)
    integer len = length(in)
    string out = repeat(' ',len)
    string left = l_alphabet,
           right = r_alphabet
    for i=1 to len do
        if show_steps then printf(1,"%s  %s\n", {left, right}) end if
        integer index = find(in[i],iff(mode==ENCRYPT?right:left))
        out[i] = iff(mode==ENCRYPT?left:right)[index]

        if i==len then exit end if
 
        /* permute left */
        left = left[index..26]&left[1..index-1]
        left[2..14] = left[3..14]&left[2]

        /* permute right */
        right = right[index+1..26]&right[1..index]
        right[3..14] = right[4..14]&right[3]
    end for
    return out
end function
 
string plain_text = "WELLDONEISBETTERTHANWELLSAID"
printf(1,"The original plaintext is : %s\n", {plain_text})

--printf(1,"\nThe left and right alphabets after each permutation"&
--         " during encryption are :\n\n")
--string cipher_text = chao_cipher(plain_text, ENCRYPT, true)
string cipher_text = chao_cipher(plain_text, ENCRYPT, false)
printf(1,"\nThe ciphertext is : %s\n", {cipher_text})

string plain_text2 = chao_cipher(cipher_text, DECRYPT, false)
printf(1,"\nThe recovered plaintext is : %s\n", {plain_text2})
```

{{out}}

```txt

The original plaintext is : WELLDONEISBETTERTHANWELLSAID

The ciphertext is : OAHQHCNYNXTSZJRRHJBYHQKSOUJY

The recovered plaintext is : WELLDONEISBETTERTHANWELLSAID

```



## Python


### Procedural


```python
# Python3 implementation of Chaocipher 
# left wheel = ciphertext wheel
# right wheel = plaintext wheel

def main():
    # letters only! makealpha(key) helps generate lalpha/ralpha. 
    lalpha = "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
    ralpha = "PTLNBQDEOYSFAVZKGJRIHWXUMC"
    msg = "WELLDONEISBETTERTHANWELLSAID"

    print("L:", lalpha)
    print("R:", ralpha)
    print("I:", msg)
    print("O:", do_chao(msg, lalpha, ralpha, 1, 0), "\n")
    
    do_chao(msg, lalpha, ralpha, 1, 1)

def do_chao(msg, lalpha, ralpha, en=1, show=0):
    msg = correct_case(msg)
    out = ""    
    if show:
        print("="*54)        
        print(10*" " + "left:" + 21*" " + "right: ")
        print("="*54)        
        print(lalpha, ralpha, "\n")
    for L in msg:
        if en:
            lalpha, ralpha = rotate_wheels(lalpha, ralpha, L)
            out += lalpha[0]
        else:
            ralpha, lalpha = rotate_wheels(ralpha, lalpha, L)
            out += ralpha[0]
        lalpha, ralpha = scramble_wheels(lalpha, ralpha)
        if show:
            print(lalpha, ralpha)            
    return out
    
def makealpha(key=""):
    alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    z = set()
    key = [x.upper() for x in (key + alpha[::-1])
           if not (x.upper() in z or z.add(x.upper()))]
    return "".join(key)

def correct_case(string):
    return "".join([s.upper() for s in string if s.isalpha()])

def permu(alp, num):
    alp = alp[:num], alp[num:]
    return "".join(alp[::-1])

def rotate_wheels(lalph, ralph, key):
    newin = ralph.index(key)
    return permu(lalph, newin), permu(ralph, newin)    

def scramble_wheels(lalph, ralph):
    # LEFT = cipher wheel 
    # Cycle second[1] through nadir[14] forward
    lalph = list(lalph)
    lalph = "".join([*lalph[0],
                    *lalph[2:14],
                    lalph[1],
                    *lalph[14:]])
    
    # RIGHT = plain wheel                    
    # Send the zenith[0] character to the end[25],
    # cycle third[2] through nadir[14] characters forward
    ralph = list(ralph)
    ralph = "".join([*ralph[1:3],
                     *ralph[4:15],
                     ralph[3],
                     *ralph[15:],
                     ralph[0]])
    return lalph, ralph

main()
```


```txt
L: HXUCZVAMDSLKPEFJRIGTWOBNYQ
R: PTLNBQDEOYSFAVZKGJRIHWXUMC
I: WELLDONEISBETTERTHANWELLSAID
O: OAHQHCNYNXTSZJRRHJBYHQKSOUJY 


### ================================================

          left:                     right: 

### ================================================

HXUCZVAMDSLKPEFJRIGTWOBNYQ PTLNBQDEOYSFAVZKGJRIHWXUMC 

ONYQHXUCZVAMDBSLKPEFJRIGTW XUCPTLNBQDEOYMSFAVZKGJRIHW
ADBSLKPEFJRIGMTWONYQHXUCZV OYSFAVZKGJRIHMWXUCPTLNBQDE
HUCZVADBSLKPEXFJRIGMTWONYQ NBDEOYSFAVZKGQJRIHMWXUCPTL
QUCZVADBSLKPEHXFJRIGMTWONY NBEOYSFAVZKGQDJRIHMWXUCPTL
HFJRIGMTWONYQXUCZVADBSLKPE JRHMWXUCPTLNBIEOYSFAVZKGQD
CVADBSLKPEHFJZRIGMTWONYQXU YSAVZKGQDJRHMFWXUCPTLNBIEO
NQXUCVADBSLKPYEHFJZRIGMTWO BIOYSAVZKGQDJERHMFWXUCPTLN
YHFJZRIGMTWONEQXUCVADBSLKP RHFWXUCPTLNBIMOYSAVZKGQDJE
NQXUCVADBSLKPEYHFJZRIGMTWO MOSAVZKGQDJERYHFWXUCPTLNBI
XCVADBSLKPEYHUFJZRIGMTWONQ AVKGQDJERYHFWZXUCPTLNBIMOS
TONQXCVADBSLKWPEYHUFJZRIGM IMSAVKGQDJERYOHFWZXUCPTLNB
SKWPEYHUFJZRILGMTONQXCVADB RYHFWZXUCPTLNOBIMSAVKGQDJE
ZILGMTONQXCVARDBSKWPEYHUFJ LNBIMSAVKGQDJOERYHFWZXUCPT
JILGMTONQXCVAZRDBSKWPEYHUF LNIMSAVKGQDJOBERYHFWZXUCPT
RBSKWPEYHUFJIDLGMTONQXCVAZ RYFWZXUCPTLNIHMSAVKGQDJOBE
RSKWPEYHUFJIDBLGMTONQXCVAZ YFZXUCPTLNIHMWSAVKGQDJOBER
HFJIDBLGMTONQUXCVAZRSKWPEY LNHMWSAVKGQDJIOBERYFZXUCPT
JDBLGMTONQUXCIVAZRSKWPEYHF MWAVKGQDJIOBESRYFZXUCPTLNH
BGMTONQUXCIVALZRSKWPEYHFJD VKQDJIOBESRYFGZXUCPTLNHMWA
YFJDBGMTONQUXHCIVALZRSKWPE HMAVKQDJIOBESWRYFGZXUCPTLN
HIVALZRSKWPEYCFJDBGMTONQUX RYGZXUCPTLNHMFAVKQDJIOBESW
QXHIVALZRSKWPUEYCFJDBGMTON SWYGZXUCPTLNHRMFAVKQDJIOBE
KPUEYCFJDBGMTWONQXHIVALZRS NHMFAVKQDJIOBRESWYGZXUCPTL
SPUEYCFJDBGMTKWONQXHIVALZR NHFAVKQDJIOBRMESWYGZXUCPTL
OQXHIVALZRSPUNEYCFJDBGMTKW WYZXUCPTLNHFAGVKQDJIOBRMES
UEYCFJDBGMTKWNOQXHIVALZRSP GVQDJIOBRMESWKYZXUCPTLNHFA
JBGMTKWNOQXHIDVALZRSPUEYCF OBMESWKYZXUCPRTLNHFAGVQDJI
YFJBGMTKWNOQXCHIDVALZRSPUE JIBMESWKYZXUCOPRTLNHFAGVQD

OAHQHCNYNXTSZJRRHJBYHQKSOUJY
WELLDONEISBETTERTHANWELLSAID
```



### Functional

{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Chaocipher'''

from itertools import chain, cycle, islice


# chao :: String -> String -> Bool -> String -> String
def chao(l):
    '''Chaocipher encoding or decoding for the given
       left and right 'wheels'.
       A ciphertext is returned if the boolean flag
       is True, and a plaintext if the flag is False.
    '''
    def go(l, r, plain, xxs):
        if xxs:
            (src, dst) = (l, r) if plain else (r, l)
            (x, xs) = (xxs[0], xxs[1:])

            def chaoProcess(n):
                return [dst[n]] + go(
                    shifted(1)(14)(rotated(n, l)),
                    compose(shifted(2)(14))(shifted(0)(26))(
                        rotated(n, r)
                    ),
                    plain,
                    xs
                )

            return maybe('')(chaoProcess)(
                elemIndex(x)(src)
            )
        else:
            return []
    return lambda r: lambda plain: lambda xxs: concat(go(
        l, r, plain, xxs
    ))


# rotated :: Int -> [a] -> [a]
def rotated(z, s):
    '''Rotation of string s by z characters.'''
    return take(len(s))(
        drop(z)(
            cycle(s)
        )
    )


# shifted :: Int -> Int -> [a] -> [a]
def shifted(src):
    '''The string s with a set of its characters cyclically
       shifted from a source index to a destination index.
    '''
    def go(dst, s):
        (a, b) = splitAt(dst)(s)
        (x, y) = splitAt(src)(a)
        return concat([x, rotated(1, y), b])
    return lambda dst: lambda s: go(dst, s)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Print the plain text, followed by
       a corresponding cipher text,
       and a decode of that cipher text.
    '''
    chaoWheels = chao(
        "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
    )(
        "PTLNBQDEOYSFAVZKGJRIHWXUMC"
    )
    plainText = "WELLDONEISBETTERTHANWELLSAID"
    cipherText = chaoWheels(False)(plainText)

    print(plainText)
    print(cipherText)
    print(
        chaoWheels(True)(cipherText)
    )


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.
       Wrapper containing the result of a computation.
    '''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.
       Empty wrapper returned where a computation is not possible.
    '''
    return {'type': 'Maybe', 'Nothing': True}


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xs):
    '''The concatenation of all the elements
       in a list or iterable.
    '''
    def f(ys):
        zs = list(chain(*ys))
        return ''.join(zs) if isinstance(ys[0], str) else zs

    return (
        f(xs) if isinstance(xs, list) else (
            chain.from_iterable(xs)
        )
    ) if xs else []


# drop :: Int -> [a] -> [a]
# drop :: Int -> String -> String
def drop(n):
    '''The sublist of xs beginning at
       (zero-based) index n.
    '''
    def go(xs):
        if isinstance(xs, (list, tuple, str)):
            return xs[n:]
        else:
            take(n)(xs)
            return xs
    return lambda xs: go(xs)


# elemIndex :: Eq a => a -> [a] -> Maybe Int
def elemIndex(x):
    '''Just the index of the first element in xs
       which is equal to x,
       or Nothing if there is no such element.
    '''
    def go(xs):
        try:
            return Just(xs.index(x))
        except ValueError:
            return Nothing()
    return lambda xs: go(xs)


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    '''Either the default value v, if m is Nothing,
       or the application of f to x,
       where m is Just(x).
    '''
    return lambda f: lambda m: v if None is m or m.get('Nothing') else (
        f(m.get('Just'))
    )


# splitAt :: Int -> [a] -> ([a], [a])
def splitAt(n):
    '''A tuple pairing the prefix of length n
       with the rest of xs.
    '''
    return lambda xs: (xs[0:n], xs[n:])


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.
    '''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, (list, tuple))
        else list(islice(xs, n))
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
WELLDONEISBETTERTHANWELLSAID
OAHQHCNYNXTSZJRRHJBYHQKSOUJY
WELLDONEISBETTERTHANWELLSAID
```



## Ruby


```ruby
txt    = "WELLDONEISBETTERTHANWELLSAID"
@left  = "HXUCZVAMDSLKPEFJRIGTWOBNYQ".chars
@right = "PTLNBQDEOYSFAVZKGJRIHWXUMC".chars

def encrypt(char)
  coded_char = @left[@right.index(char)]

  @left.rotate!(@left.index(coded_char))
  part = @left.slice!(1,13).rotate
  @left.insert(1, *part)

  @right.rotate!(@right.index(char)+1)
  part = @right.slice!(2,12).rotate
  @right.insert(2, *part)
  
  @left[0]
end

puts txt.each_char.map{|c| encrypt(c) }.join 

```

{{out}}

```txt
OAHQHCNYNXTSZJRRHJBYHQKSOUJY

```



## Rust


```rust
const LEFT_ALPHABET_CT: &str = "HXUCZVAMDSLKPEFJRIGTWOBNYQ";
const RIGHT_ALPHABET_PT: &str = "PTLNBQDEOYSFAVZKGJRIHWXUMC";
const ZENITH: usize = 0;
const NADIR: usize = 12;
const SEQUENCE: &str = "WELLDONEISBETTERTHANWELLSAID";

fn cipher(letter: &char, left: &String, right: &String) -> (usize, char) {
    let pos = right.find(*letter).unwrap();
    let cipher = left.chars().nth(pos).unwrap();
    (pos, cipher)
}

fn main() {
    let mut left = LEFT_ALPHABET_CT.to_string();
    let mut right = RIGHT_ALPHABET_PT.to_string();

    let ciphertext = SEQUENCE.chars()
        .map(|letter| {
            let (pos, cipher_char) = cipher(&letter, &left, &right);
            left = format!("{}{}", &left[pos..], &left[..pos]);
            left = format!("{}{}{}{}", &left[ZENITH..1], &left[2..NADIR+2], &left[1..2], &left[NADIR+2..]);
            if pos != right.len() - 1 {
                right = format!("{}{}", &right[pos + 1..], &right[..pos + 1]);
            }
            right = format!("{}{}{}{}", &right[ZENITH..2], &right[3..NADIR+2], &right[2..3], &right[NADIR+2..]);
            cipher_char
        })
        .collect::<String>();

    println!("Plaintext: {}", SEQUENCE);
    println!("Ciphertext: {}", ciphertext);
}
```

{{out}}

```txt
Plaintext: WELLDONEISBETTERTHANWELLSAID
Ciphertext: OAHQHCNYNXTSZJRRHJBYHQKSOUJY
```



## Tailspin


```tailspin

templates chaocipher@{left:,right:,decode:}
  templates permute
    def ctshift: [ $@chaocipher.ct($..-1)..., $@chaocipher.ct(1..$-1)...];
    def p1: $ mod 26 + 1;
    def ptshift: [ $@chaocipher.pt($p1..-1)..., $@chaocipher.pt(1..$p1-1)...];
    ..|@chaocipher: { ct: [ $ctshift(1), $ctshift(3..14)..., $ctshift(2), $ctshift(15..-1)...],
      pt: [ $ptshift(1..2)..., $ptshift(4..14)..., $ptshift(3), $ptshift(15..-1)...] };
  end permute
 
  @: {ct:[ $left... ], pt: [ $right... ], result:[]};
  $... -> #
  '$@.result...;' !
 
  <?($decode <0>)>
    def plain: $;
    def index: $@.pt -> [i](<$plain> $i!) -> $(1);
    ..|@.result: $@.ct($index);
    $index -> permute -> !VOID
  <>
    def cipher: $;
    def index: $@.ct -> [i](<$cipher> $i!) -> $(1);
    ..|@.result: $@.pt($index);
    $index -> permute -> !VOID
end chaocipher
 
'WELLDONEISBETTERTHANWELLSAID' -> chaocipher@{left:'HXUCZVAMDSLKPEFJRIGTWOBNYQ', right:'PTLNBQDEOYSFAVZKGJRIHWXUMC',decode:0} -> '$;
' -> !OUT::write
 
'OAHQHCNYNXTSZJRRHJBYHQKSOUJY' -> chaocipher@{left:'HXUCZVAMDSLKPEFJRIGTWOBNYQ', right:'PTLNBQDEOYSFAVZKGJRIHWXUMC',decode:1} -> '$;
' -> !OUT::write

```


{{out}}

```txt

OAHQHCNYNXTSZJRRHJBYHQKSOUJY
WELLDONEISBETTERTHANWELLSAID

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    ReadOnly L_ALPHABET As String = "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
    ReadOnly R_ALPHABET As String = "PTLNBQDEOYSFAVZKGJRIHWXUMC"

    Enum Mode
        ENCRYPT
        DECRYPT
    End Enum

    Function Exec(text As String, mode As Mode, Optional showSteps As Boolean = False) As String
        Dim left = L_ALPHABET.ToCharArray()
        Dim right = R_ALPHABET.ToCharArray()
        Dim eText(text.Length - 1) As Char
        Dim temp(25) As Char

        For i = 0 To text.Length - 1
            If showSteps Then Console.WriteLine("{0} {1}", String.Join("", left), String.Join("", right))
            Dim index As Integer
            If mode = Mode.ENCRYPT Then
                index = Array.IndexOf(right, text(i))
                eText(i) = left(index)
            Else
                index = Array.IndexOf(left, text(i))
                eText(i) = right(index)
            End If
            If i = text.Length - 1 Then Exit For

            'permute left

            For j = index To 25
                temp(j - index) = left(j)
            Next
            For j = 0 To index - 1
                temp(26 - index + j) = left(j)
            Next
            Dim store = temp(1)
            For j = 2 To 13
                temp(j - 1) = temp(j)
            Next
            temp(13) = store
            temp.CopyTo(left, 0)

            'permute right

            For j = index To 25
                temp(j - index) = right(j)
            Next
            For j = 0 To index - 1
                temp(26 - index + j) = right(j)
            Next
            store = temp(0)
            For j = 1 To 25
                temp(j - 1) = temp(j)
            Next
            temp(25) = store
            store = temp(2)
            For j = 3 To 13
                temp(j - 1) = temp(j)
            Next
            temp(13) = store
            temp.CopyTo(right, 0)
        Next

        Return eText
    End Function

    Sub Main()
        Dim plainText = "WELLDONEISBETTERTHANWELLSAID"
        Console.WriteLine("The original plaintext is : {0}", plainText)
        Console.WriteLine(vbNewLine + "The left and right alphabets after each permutation during encryption are :" + vbNewLine)
        Dim cipherText = Exec(plainText, Mode.ENCRYPT, True)
        Console.WriteLine(vbNewLine + "The ciphertext is : {0}", cipherText)
        Dim plainText2 = Exec(cipherText, Mode.DECRYPT)
        Console.WriteLine(vbNewLine + "The recovered plaintext is : {0}", plainText2)
    End Sub

End Module
```

{{out}}

```txt
The original plaintext is : WELLDONEISBETTERTHANWELLSAID

The left and right alphabets after each permutation during encryption are :

HXUCZVAMDSLKPEFJRIGTWOBNYQ PTLNBQDEOYSFAVZKGJRIHWXUMC
ONYQHXUCZVAMDBSLKPEFJRIGTW XUCPTLNBQDEOYMSFAVZKGJRIHW
ADBSLKPEFJRIGMTWONYQHXUCZV OYSFAVZKGJRIHMWXUCPTLNBQDE
HUCZVADBSLKPEXFJRIGMTWONYQ NBDEOYSFAVZKGQJRIHMWXUCPTL
QUCZVADBSLKPEHXFJRIGMTWONY NBEOYSFAVZKGQDJRIHMWXUCPTL
HFJRIGMTWONYQXUCZVADBSLKPE JRHMWXUCPTLNBIEOYSFAVZKGQD
CVADBSLKPEHFJZRIGMTWONYQXU YSAVZKGQDJRHMFWXUCPTLNBIEO
NQXUCVADBSLKPYEHFJZRIGMTWO BIOYSAVZKGQDJERHMFWXUCPTLN
YHFJZRIGMTWONEQXUCVADBSLKP RHFWXUCPTLNBIMOYSAVZKGQDJE
NQXUCVADBSLKPEYHFJZRIGMTWO MOSAVZKGQDJERYHFWXUCPTLNBI
XCVADBSLKPEYHUFJZRIGMTWONQ AVKGQDJERYHFWZXUCPTLNBIMOS
TONQXCVADBSLKWPEYHUFJZRIGM IMSAVKGQDJERYOHFWZXUCPTLNB
SKWPEYHUFJZRILGMTONQXCVADB RYHFWZXUCPTLNOBIMSAVKGQDJE
ZILGMTONQXCVARDBSKWPEYHUFJ LNBIMSAVKGQDJOERYHFWZXUCPT
JILGMTONQXCVAZRDBSKWPEYHUF LNIMSAVKGQDJOBERYHFWZXUCPT
RBSKWPEYHUFJIDLGMTONQXCVAZ RYFWZXUCPTLNIHMSAVKGQDJOBE
RSKWPEYHUFJIDBLGMTONQXCVAZ YFZXUCPTLNIHMWSAVKGQDJOBER
HFJIDBLGMTONQUXCVAZRSKWPEY LNHMWSAVKGQDJIOBERYFZXUCPT
JDBLGMTONQUXCIVAZRSKWPEYHF MWAVKGQDJIOBESRYFZXUCPTLNH
BGMTONQUXCIVALZRSKWPEYHFJD VKQDJIOBESRYFGZXUCPTLNHMWA
YFJDBGMTONQUXHCIVALZRSKWPE HMAVKQDJIOBESWRYFGZXUCPTLN
HIVALZRSKWPEYCFJDBGMTONQUX RYGZXUCPTLNHMFAVKQDJIOBESW
QXHIVALZRSKWPUEYCFJDBGMTON SWYGZXUCPTLNHRMFAVKQDJIOBE
KPUEYCFJDBGMTWONQXHIVALZRS NHMFAVKQDJIOBRESWYGZXUCPTL
SPUEYCFJDBGMTKWONQXHIVALZR NHFAVKQDJIOBRMESWYGZXUCPTL
OQXHIVALZRSPUNEYCFJDBGMTKW WYZXUCPTLNHFAGVKQDJIOBRMES
UEYCFJDBGMTKWNOQXHIVALZRSP GVQDJIOBRMESWKYZXUCPTLNHFA
JBGMTKWNOQXHIDVALZRSPUEYCF OBMESWKYZXUCPRTLNHFAGVQDJI

The ciphertext is : OAHQHCNYNXTSZJRRHJBYHQKSOUJY

The recovered plaintext is : WELLDONEISBETTERTHANWELLSAID
```



## zkl

{{trans|perl6}}

```zkl
class Chao{
   var [const private] lAlphabet = "HXUCZVAMDSLKPEFJRIGTWOBNYQ",
		       rAlphabet = "PTLNBQDEOYSFAVZKGJRIHWXUMC";
   fcn encode(text){ code(text,encodeL); }
   fcn decode(text){ code(text,decodeL); }
   // reset alphabets each [en|de]code and maintain re-entrancy
   fcn code(text,f){ text.apply(f,Data(Void,lAlphabet),Data(Void,rAlphabet)) }
   fcn [private] encodeL(letter,left,right){  // encode a letter
      index:=right.index(letter);
      enc  :=left[index].toChar();
      permute(left,right,index);
      println(left.text," ",right.text,"  ",index);
      enc
   }
   fcn [private] decodeL(letter,left,right){  // decode a letter
      index:=left.index(letter);
      dec  :=right[index].toChar();
      permute(left,right,index);
      dec
   }
   fcn [private] permute(left,right,index){
      left.append(left.pop(0,index));		// rotate index times
      left.insert(13,left.pop(1));		// rotate [1..13] once

      right.append(right.pop(0,index+1)); # rotate index+1 times, idx==25==noop
      right.insert(13,right.pop(2));		// rotate [2..13] once
   }
}
```


```zkl
plainText:="WELLDONEISBETTERTHANWELLSAID";
println("The original plaintext is : ",plainText);
println("\nThe left and right alphabets after each permutation"
         " during encryption are:");
cipherText:=Chao.encode(plainText);
println("\nThe ciphertext is : ",cipherText);

plainText2:=Chao.decode(cipherText);
println("\nThe recovered plaintext is : ",plainText2);
```

{{out}}
<pre style="height:45ex">
The original plaintext is : WELLDONEISBETTERTHANWELLSAID

The left and right alphabets after each permutation during encryption are:
ONYQHXUCZVAMDBSLKPEFJRIGTW XUCPTLNBQDEOYMSFAVZKGJRIHW  21
ADBSLKPEFJRIGMTWONYQHXUCZV OYSFAVZKGJRIHMWXUCPTLNBQDE  10
HUCZVADBSLKPEXFJRIGMTWONYQ NBDEOYSFAVZKGQJRIHMWXUCPTL  20
QUCZVADBSLKPEHXFJRIGMTWONY NBEOYSFAVZKGQDJRIHMWXUCPTL  25
HFJRIGMTWONYQXUCZVADBSLKPE JRHMWXUCPTLNBIEOYSFAVZKGQD  13
CVADBSLKPEHFJZRIGMTWONYQXU YSAVZKGQDJRHMFWXUCPTLNBIEO  15
NQXUCVADBSLKPYEHFJZRIGMTWO BIOYSAVZKGQDJERHMFWXUCPTLN  21
YHFJZRIGMTWONEQXUCVADBSLKP RHFWXUCPTLNBIMOYSAVZKGQDJE  13
NQXUCVADBSLKPEYHFJZRIGMTWO MOSAVZKGQDJERYHFWXUCPTLNBI  12
XCVADBSLKPEYHUFJZRIGMTWONQ AVKGQDJERYHFWZXUCPTLNBIMOS  2
TONQXCVADBSLKWPEYHUFJZRIGM IMSAVKGQDJERYOHFWZXUCPTLNB  21
SKWPEYHUFJZRILGMTONQXCVADB RYHFWZXUCPTLNOBIMSAVKGQDJE  10
ZILGMTONQXCVARDBSKWPEYHUFJ LNBIMSAVKGQDJOERYHFWZXUCPT  10
JILGMTONQXCVAZRDBSKWPEYHUF LNIMSAVKGQDJOBERYHFWZXUCPT  25
RBSKWPEYHUFJIDLGMTONQXCVAZ RYFWZXUCPTLNIHMSAVKGQDJOBE  14
RSKWPEYHUFJIDBLGMTONQXCVAZ YFZXUCPTLNIHMWSAVKGQDJOBER  0
HFJIDBLGMTONQUXCVAZRSKWPEY LNHMWSAVKGQDJIOBERYFZXUCPT  7
JDBLGMTONQUXCIVAZRSKWPEYHF MWAVKGQDJIOBESRYFZXUCPTLNH  2
BGMTONQUXCIVALZRSKWPEYHFJD VKQDJIOBESRYFGZXUCPTLNHMWA  2
YFJDBGMTONQUXHCIVALZRSKWPE HMAVKQDJIOBESWRYFGZXUCPTLN  21
HIVALZRSKWPEYCFJDBGMTONQUX RYGZXUCPTLNHMFAVKQDJIOBESW  13
QXHIVALZRSKWPUEYCFJDBGMTON SWYGZXUCPTLNHRMFAVKQDJIOBE  23
KPUEYCFJDBGMTWONQXHIVALZRS NHMFAVKQDJIOBRESWYGZXUCPTL  10
SPUEYCFJDBGMTKWONQXHIVALZR NHFAVKQDJIOBRMESWYGZXUCPTL  25
OQXHIVALZRSPUNEYCFJDBGMTKW WYZXUCPTLNHFAGVKQDJIOBRMES  15
UEYCFJDBGMTKWNOQXHIVALZRSP GVQDJIOBRMESWKYZXUCPTLNHFA  12
JBGMTKWNOQXHIDVALZRSPUEYCF OBMESWKYZXUCPRTLNHFAGVQDJI  5
YFJBGMTKWNOQXCHIDVALZRSPUE JIBMESWKYZXUCOPRTLNHFAGVQD  23

The ciphertext is : OAHQHCNYNXTSZJRRHJBYHQKSOUJY

The recovered plaintext is : WELLDONEISBETTERTHANWELLSAID

```

