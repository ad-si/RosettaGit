+++
title = "Vigenère cipher"
description = ""
date = 2019-10-14T13:30:22Z
aliases = []
[extra]
id = 9836
[taxonomies]
categories = ["task", "Encryption"]
tags = []
+++

## Task

Implement a   [[wp:Vigen%C3%A8re_cipher|Vigenère cypher]],   both encryption and decryption.

The program should handle keys and text of unequal length,
and should capitalize everything and discard non-alphabetic characters.

(If your program handles non-alphabetic characters in another way,
make a note of it.)


## Related tasks

*   [[Caesar cipher]]
*   [[Rot-13]]
*   [[Substitution Cipher]]





## Ada



```Ada
WITH Ada.Text_IO, Ada.Characters.Handling;
USE Ada.Text_IO, Ada.Characters.Handling;

PROCEDURE Main IS
   SUBTYPE Alpha IS Character RANGE 'A' .. 'Z';
   TYPE Ring IS MOD (Alpha'Pos (Alpha'Last)-Alpha'Pos (Alpha'First) + 1);
   TYPE Seq IS ARRAY (Integer RANGE <>) OF Ring;

   FUNCTION "+" (S, Key : Seq) RETURN Seq IS
      R : Seq (S'Range);
   BEGIN
      FOR I IN R'Range LOOP
         R (I) := S (I) + Key (Key'First + (I - R'First) MOD Key'Length);
      END LOOP;
      RETURN R;
   END "+";

   FUNCTION "-" (S : Seq) RETURN Seq IS
      R : Seq (S'Range);
   BEGIN
      FOR I IN R'Range LOOP
         R (I) := - S (I);
      END LOOP;
      RETURN R;
   END "-";

   FUNCTION To_Seq (S : String) RETURN Seq IS
      R  : Seq (S'Range);
      I  : Integer := R'First;
   BEGIN
      FOR C OF To_Upper (S) LOOP
         IF C IN Alpha THEN
            R (I) := Ring'Mod (Alpha'Pos (C) - Alpha'Pos (Alpha'First));
            I := I + 1;
         END IF;
      END LOOP;
      RETURN R (R'First .. I - 1);
   END To_Seq;

   FUNCTION To_String (S : Seq ) RETURN String IS
      R : String (S'Range);
   BEGIN
      FOR I IN R'Range LOOP
         R (I) := Alpha'Val ( Integer (S (I)) + Alpha'Pos (Alpha'First));
      END LOOP;
      RETURN R;
   END To_String;

   Input : Seq := To_Seq (Get_Line);
   Key : Seq := To_Seq (Get_Line);
   Crypt : Seq := Input + Key;
BEGIN
   Put_Line ("Encrypted: " & To_String (Crypt));
   Put_Line ("Decrypted: " & To_String (Crypt + (-Key)));
END Main;


```


```txt
Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
VIGENERECIPHER
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## ALGOL 68

{{trans|C++}} Note: This specimen retains the original [[Vigenère_Cipher#C++|C++]] coding style.
```algol68
STRING key := "";

PROC vigenere cipher = (REF STRING key)VOID:
(
  FOR i FROM LWB key TO UPB key DO
    IF key[i] >= "A" AND key[i] <= "Z" THEN
      key +:= key[i] FI;
    IF key[i] >= "a" AND key[i] <= "z" THEN
      key +:= REPR(ABS key[i] + ABS"A" - ABS"a") FI
  OD
);

PROC encrypt = (STRING text)STRING:
(
  STRING out := "";

  INT j := LWB text;
  FOR i FROM LWB text TO UPB text DO
    CHAR c := text[i];

    IF c >= "a" AND c <= "z" THEN
      c := REPR(ABS c + (ABS"A" - ABS"a")) FI;
    IF c >= "A" AND c <= "Z" THEN
      out +:= REPR((ABS c + ABS key[j] - 2*ABS"A") MOD 26 + ABS"A");
      j := j MOD UPB key + 1
    FI
  OD;

  out
);

PROC decrypt = (STRING text)STRING:
(
  STRING out;

  INT j := LWB text;
  FOR i FROM LWB text TO UPB text DO
    CHAR c := text[i];

    IF c >= "a" AND c <= "z" THEN
      c := REPR(ABS c + (ABS"A" - ABS"a")) FI;
    IF c >= "A" AND c <= "Z" THEN
      out +:= REPR((ABS c - ABS key[j] + 26) MOD 26 + ABS"A");
      j := j MOD UPB key + 1
    FI
  OD;

  out
);

main:
(
  vigenere cipher(key:="VIGENERECIPHER");

  STRING original := "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
  STRING encrypted := encrypt(original);
  STRING decrypted := decrypt(encrypted);

  print((original, new line));
  print(("Encrypted: ", encrypted, new line));
  print(("Decrypted: ", decrypted, new line))
)
```

```txt

Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Applesoft BASIC

Lines <code>340,350,430,440</code> could probably been put into some DEF FN, but it would probably have made it harder to read. The maximum length for a string in AppleSoft BASIC is 255 characters.
I have not used the DEF FN MOD(A) function in line <code>450</code> on purpose, as I still would have had to correct for a possible negative value.

```Applesoft BASIC

  100 :
  110  REM  VIGENERE CIPHER
  120 :
  200  REM  SET-UP
  210 K$ = "LEMON": PRINT "KEY: "; K$
  220 PT$ = "ATTACK AT DAWN": PRINT "PLAIN TEXT: ";PT$
  230  DEF  FN MOD(A) = A -  INT (A / 26) * 26
  300  REM  ENCODING
  310 K = 1
  320  FOR I = 1 TO  LEN (PT$)
  330  IF  ASC ( MID$ (PT$,I,1)) < 65
       OR  ASC ( MID$ (PT$,I,1)) > 90 THEN  NEXT I
  340 TV =  ASC ( MID$ (PT$,I,1)) - 65
  350 KV =  ASC ( MID$ (K$,K,1)) - 65
  360 CT$ = CT$ +  CHR$ ( FN MOD(TV + KV) + 65)
  370 K = K + 1: IF K >  LEN (K$) THEN K = 1
  380  NEXT I
  390  PRINT "CIPHER TEXT: ";CT$
  400  REM  DECODING
  410 K = 1
  420  FOR I = 1 TO  LEN (CT$)
  430 TV =  ASC ( MID$ (CT$,I,1)) - 65
  440 KV =  ASC ( MID$ (K$,K,1)) -  65
  450 T = TV - KV: IF T < 0 THEN T = T + 26
  460 DT$ = DT$ +  CHR$ (T + 65)
  470 K = K + 1: IF K >  LEN (K$) THEN K = 1
  480  NEXT I
  490  PRINT "DECRYPTED TEXT: ";DT$
```

  KEY: LEMON
  PLAIN TEXT: ATTACK AT DAWN
  CIPHER TEXT: LXFOPVEFRNHR
  DECRYPTED TEXT: ATTACKATDAWN


## AutoHotkey


```AutoHotkey
Key = VIGENERECIPHER
Text= Beware the Jabberwock, my son! The jaws that bite, the claws that catch!

out := "Input      =" text "`nkey        =" key "`nCiphertext =" (c := VigenereCipher(Text, Key)) "`nDecrypted  =" VigenereDecipher(c, key)
MsgBox % clipboard := out

VigenereCipher(Text, Key){
   StringUpper, Text, Text
   Text := RegExReplace(Text, "[^A-Z]")
   Loop Parse, Text
   {
      a   := Asc(A_LoopField)                                   - Asc("A")
      b   := Asc(SubStr(Key, 1+Mod(A_Index-1, StrLen(Key)), 1)) - Asc("A")
      out .= Chr(Mod(a+b,26)+Asc("A"))
   }
   return out
}

VigenereDecipher(Text, key){
   Loop Parse, key
      decoderKey .= Chr(26-(Asc(A_LoopField)-65)+65)
   return VigenereCipher(Text, decoderKey)
}
```

```txt
Input      =Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
key        =VIGENERECIPHER
Ciphertext =WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted  =BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## BBC BASIC


```bbcbasic
      key$ = "LEMON"
      plaintext$ = "ATTACK AT DAWN"
      ciphertext$ = FNencrypt(plaintext$, key$)
      PRINT "Key = """ key$ """"
      PRINT "Plaintext  = """ plaintext$ """"
      PRINT "Ciphertext = """ ciphertext$ """"
      PRINT "Decrypted  = """ FNdecrypt(ciphertext$, key$) """"
      END

      DEF FNencrypt(plain$, key$)
      LOCAL i%, k%, n%, o$
      plain$ = FNupper(plain$)
      key$ = FNupper(key$)
      FOR i% = 1 TO LEN(plain$)
        n% = ASCMID$(plain$, i%)
        IF n% >= 65 IF n% <= 90 THEN
          o$ += CHR$(65 + (n% + ASCMID$(key$, k%+1)) MOD 26)
          k% = (k% + 1) MOD LEN(key$)
        ENDIF
      NEXT
      = o$

      DEF FNdecrypt(cipher$, key$)
      LOCAL i%, k%, n%, o$
      cipher$ = FNupper(cipher$)
      key$ = FNupper(key$)
      FOR i% = 1 TO LEN(cipher$)
        n% = ASCMID$(cipher$, i%)
        o$ += CHR$(65 + (n% + 26 - ASCMID$(key$, k%+1)) MOD 26)
        k% = (k% + 1) MOD LEN(key$)
      NEXT
      = o$

      DEF FNupper(A$)
      LOCAL A%,C%
      FOR A% = 1 TO LEN(A$)
        C% = ASCMID$(A$,A%)
        IF C% >= 97 IF C% <= 122 MID$(A$,A%,1) = CHR$(C%-32)
      NEXT
      = A$
```

```txt

Key = "LEMON"
Plaintext  = "ATTACK AT DAWN"
Ciphertext = "LXFOPVEFRNHR"
Decrypted  = "ATTACKATDAWN"

```



## Befunge

The text to encrypt is read from stdin. The key is the string literal at the start of the program.


```befunge
"VIGENERECIPHER">>>>1\:!v>"A"-\:00p0v
>>!#:0#-0#1g#,*#<+:v:-1$_^#!:\+1g00p<
\"{"\v>9+2*%"A"+^>$>~>:48*\`#@_::"`"`
*84*`<^4+"4"+g0\_^#!+`*55\`\0::-"A"-*
```


```txt
Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
```


The decrypter is essentially identical, except for a change of sign on the last line.


```befunge
"VIGENERECIPHER">>>>1\:!v>"A"-\:00p0v
>>!#:0#-0#1g#,*#<+:v:-1$_^#!:\+1g00p<
\"{"\v>9+2*%"A"+^>$>~>:48*\`#@_::"`"`
*84*`<^4+"4"-g0\_^#!+`*55\`\0::-"A"-*
```


```txt
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## C

This program skips non-alphabetical characters, preserves case, and when run with the <code>-d</code> command line flag, decrypts the message rather than encrypting.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <getopt.h>

#define NUMLETTERS 26
#define BUFSIZE 4096

char *get_input(void);

int main(int argc, char *argv[])
{
    char const usage[] = "Usage: vinigere [-d] key";
    char sign = 1;
    char const plainmsg[] = "Plain text:  ";
    char const cryptmsg[] = "Cipher text: ";
    bool encrypt = true;
    int opt;

    while ((opt = getopt(argc, argv, "d")) != -1) {
        switch (opt) {
        case 'd':
            sign = -1;
            encrypt = false;
            break;
        default:
            fprintf(stderr, "Unrecogized command line argument:'-%i'\n", opt);
            fprintf(stderr, "\n%s\n", usage);
            return 1;
        }
    }

    if (argc - optind != 1) {
        fprintf(stderr, "%s requires one argument and one only\n", argv[0]);
        fprintf(stderr, "\n%s\n", usage);
        return 1;
    }


    // Convert argument into array of shifts
    char const *const restrict key = argv[optind];
    size_t const keylen = strlen(key);
    char shifts[keylen];

    char const *restrict plaintext = NULL;
    for (size_t i = 0; i < keylen; i++) {
        if (!(isalpha(key[i]))) {
            fprintf(stderr, "Invalid key\n");
            return 2;
        }
        char const charcase = (isupper(key[i])) ? 'A' : 'a';
        // If decrypting, shifts will be negative.
        // This line would turn "bacon" into {1, 0, 2, 14, 13}
        shifts[i] = (key[i] - charcase) * sign;
    }

    do {
        fflush(stdout);
        // Print "Plain text: " if encrypting and "Cipher text:  " if
        // decrypting
        printf("%s", (encrypt) ? plainmsg : cryptmsg);
        plaintext = get_input();
        if (plaintext == NULL) {
            fprintf(stderr, "Error getting input\n");
            return 4;
        }
    } while (strcmp(plaintext, "") == 0); // Reprompt if entry is empty

    size_t const plainlen = strlen(plaintext);

    char* const restrict ciphertext = calloc(plainlen + 1, sizeof *ciphertext);
    if (ciphertext == NULL) {
        fprintf(stderr, "Memory error\n");
        return 5;
    }

    for (size_t i = 0, j = 0; i < plainlen; i++) {
        // Skip non-alphabetical characters
        if (!(isalpha(plaintext[i]))) {
            ciphertext[i] = plaintext[i];
            continue;
        }
        // Check case
        char const charcase = (isupper(plaintext[i])) ? 'A' : 'a';
        // Wrapping conversion algorithm
        ciphertext[i] = ((plaintext[i] + shifts[j] - charcase + NUMLETTERS) % NUMLETTERS) + charcase;
        j = (j+1) % keylen;
    }
    ciphertext[plainlen] = '\0';
    printf("%s%s\n", (encrypt) ? cryptmsg : plainmsg, ciphertext);

    free(ciphertext);
    // Silence warnings about const not being maintained in cast to void*
    free((char*) plaintext);
    return 0;
}
char *get_input(void) {

    char *const restrict buf = malloc(BUFSIZE * sizeof (char));
    if (buf == NULL) {
        return NULL;
    }

    fgets(buf, BUFSIZE, stdin);

    // Get rid of newline
    size_t const len = strlen(buf);
    if (buf[len - 1] == '\n') buf[len - 1] = '\0';

    return buf;
}
```


```txt
$ ./vigenere VIGENERECIPHER
Plain text:  Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Cipher text: Wmceei klg Rpifvmeugx, qp wqv! Ioi avey xuek fkbt, alv xtgaf xyev kpagy!

$ ./vigenere -d VIGENERECIPHER
Cipher text: Wmceei klg Rpifvmeugx, qp wqv! Ioi avey xuek fkbt, alv xtgaf xyev kpagy!
Plain text:  Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
```



## C++


```cpp
#include <iostream>
#include <string>
using namespace std;

class Vigenere
{
public:
  string key;

  Vigenere(string key)
  {
    for(int i = 0; i < key.size(); ++i)
    {
      if(key[i] >= 'A' && key[i] <= 'Z')
        this->key += key[i];
      else if(key[i] >= 'a' && key[i] <= 'z')
        this->key += key[i] + 'A' - 'a';
    }
  }

  string encrypt(string text)
  {
    string out;

    for(int i = 0, j = 0; i < text.length(); ++i)
    {
      char c = text[i];

      if(c >= 'a' && c <= 'z')
        c += 'A' - 'a';
      else if(c < 'A' || c > 'Z')
        continue;

      out += (c + key[j] - 2*'A') % 26 + 'A';
      j = (j + 1) % key.length();
    }

    return out;
  }

  string decrypt(string text)
  {
    string out;

    for(int i = 0, j = 0; i < text.length(); ++i)
    {
      char c = text[i];

      if(c >= 'a' && c <= 'z')
        c += 'A' - 'a';
      else if(c < 'A' || c > 'Z')
        continue;

      out += (c - key[j] + 26) % 26 + 'A';
      j = (j + 1) % key.length();
    }

    return out;
  }
};

int main()
{
  Vigenere cipher("VIGENERECIPHER");

  string original = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
  string encrypted = cipher.encrypt(original);
  string decrypted = cipher.decrypt(encrypted);

  cout << original << endl;
  cout << "Encrypted: " << encrypted << endl;
  cout << "Decrypted: " << decrypted << endl;
}
```


```txt

Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```


## C#

```c#

using System;

namespace VigenereCipher
{
    class VCipher
    {
        public string encrypt(string txt, string pw, int d)
        {
            int pwi = 0, tmp;
            string ns = "";
            txt = txt.ToUpper();
            pw = pw.ToUpper();
            foreach (char t in txt)
            {
                if (t < 65) continue;
                tmp = t - 65 + d * (pw[pwi] - 65);
                if (tmp < 0) tmp += 26;
                ns += Convert.ToChar(65 + ( tmp % 26) );
                if (++pwi == pw.Length) pwi = 0;
            }

            return ns;
        }
    };

    class Program
    {
        static void Main(string[] args)
        {
            VCipher v = new VCipher();

            string s0 = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!",
                   pw = "VIGENERECIPHER";

            Console.WriteLine(s0 + "\n" + pw + "\n");
            string s1 = v.encrypt(s0, pw, 1);
            Console.WriteLine("Encrypted: " + s1);
            s1 = v.encrypt(s1, "VIGENERECIPHER", -1);
            Console.WriteLine("Decrypted: " + s1);
            Console.WriteLine("\nPress any key to continue...");
            Console.ReadKey();
        }
    }
}

```

```txt

Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
VIGENERECIPHER

Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Ceylon


```ceylon
shared void run() {

    function normalize(String text) => text.uppercased.filter(Character.letter);

    function crypt(String text, String key, Character(Character, Character) transform) => String {
        for ([a, b] in zipPairs(normalize(text), normalize(key).cycled))
        transform(a, b)
    };

    function encrypt(String clearText, String key) =>
            crypt(clearText, key, (Character a, Character b) =>
				('A'.integer + ((a.integer + b.integer - 130) % 26)).character);

    function decrypt(String cipherText, String key) =>
            crypt(cipherText, key, (Character a, Character b) =>
        		('A'.integer + ((a.integer - b.integer + 26) % 26)).character);

    value key = "VIGENERECIPHER";
    value message = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
    value encrypted = encrypt(message, key);
    value decrypted = decrypt(encrypted, key);

    print(encrypted);
    print(decrypted);
}
```



## Common Lisp

This doesn't assume anything about character codes other than A-Z being a contiguous block (but still, we could be using EBCDIC.  Who knows.)

```lisp
(defun strip (s)
  (remove-if-not
    (lambda (c) (char<= #\A c #\Z))
    (string-upcase s)))

(defun vigenère (s key &key decipher
			&aux (A (char-code #\A))
			     (op (if decipher #'- #'+)))
  (labels
    ((to-char (c) (code-char (+ c A)))
     (to-code (c) (- (char-code c) A)))
    (let ((k (map 'list #'to-code (strip key))))
      (setf (cdr (last k)) k)
      (map 'string
	   (lambda (c)
	     (prog1
	       (to-char
		 (mod (funcall op (to-code c) (car k)) 26))
	       (setf k (cdr k))))
	   (strip s)))))

(let* ((msg "Beware the Jabberwock... The jaws that... the claws that catch!")
       (key "vigenere cipher")
       (enc (vigenère msg key))
       (dec (vigenère enc key :decipher t)))
  (format t "msg: ~a~%enc: ~a~%dec: ~a~%" msg enc dec))
```

```txt
msg: Beware the Jabberwock... The jaws that... the claws that catch!
enc: WMCEEIKLGRPIFVMEUGXXYILILZXYVBZLRGCEYAIOEKXIZGU
dec: BEWARETHEJABBERWOCKTHEJAWSTHATTHECLAWSTHATCATCH
```



## Clojure

Requires Clojure 1.2.

```clojure
(ns org.rosettacode.clojure.vigenere
  (:require [clojure.string :as string]))

; convert letter to offset from \A
(defn to-num [char] (- (int char) (int \A)))

; convert number to letter, treating it as modulo 26 offset from \A
(defn from-num [num] (char (+ (mod num 26) (int \A))))

; Convert a string to a sequence of just the letters as uppercase chars
(defn to-normalized-seq [str]
  (map #'first (re-seq #"[A-Z]" (string/upper-case str))))

; add (op=+) or subtract (op=-) the numerical value of the key letter from the
; text letter.
(defn crypt1 [op text key]
  (from-num (apply op (list (to-num text) (to-num key)))))

(defn crypt [op text key]
  (let [xcrypt1 (partial #'crypt1 op)]
    (apply #'str
      (map xcrypt1 (to-normalized-seq text)
                   (cycle (to-normalized-seq key))))))

; encipher a text
(defn encrypt [plaintext key] (crypt #'+ plaintext key))

; decipher a text
(defn decrypt [ciphertext key] (crypt #'- ciphertext key))
```


Demonstration code:

```clojure
(ns org.rosettacode.clojure.test-vigenere
  (:require [org.rosettacode.clojure.vigenere :as vigenere]))

(let
 [ plaintext  "Beware the Jabberwock, my son!  The jaws that bite, the claws that catch!"
   key        "Vigenere cipher"
   ciphertext (vigenere/encrypt plaintext  key)
   recovered  (vigenere/decrypt ciphertext key) ]

  (doall (map (fn [[k v]] (printf "%9s: %s\n" k v))
   [ ["Original" plaintext] ["Key" key] ["Encrypted" ciphertext] ["Decrypted" recovered] ])))

```


```txt
 Original: Beware the Jabberwock, my son!  The jaws that bite, the claws that catch!
      Key: Vigenere cipher
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## CoffeeScript

```coffeescript
# Simple helper since charCodeAt is quite long to write.
code = (char) -> char.charCodeAt()

encrypt = (text, key) ->
	res = []
	j = 0

	for c in text.toUpperCase()
		continue if c < 'A' or c > 'Z'

		res.push ((code c) + (code key[j]) - 130) % 26 + 65
		j = ++j % key.length

	String.fromCharCode res...

decrypt = (text, key) ->
	res = []
	j = 0

	for c in text.toUpperCase()
		continue if c < 'A' or c > 'Z'

		res.push ((code c) - (code key[j]) + 26) % 26 + 65
		j = ++j % key.length

	String.fromCharCode res...

# Trying it out
key       = "VIGENERECIPHER"
original  = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
encrypted = encrypt original, key

console.log "Original  : #{original}"
console.log "Encrypted : #{encrypted}"
console.log "Decrypted : #{decrypt encrypted, key}"
```


```txt
Original  : Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Encrypted : WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted : BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## D


```d
import std.stdio, std.string;

string encrypt(in string txt, in string key) pure @safe
in {
    assert(key.removechars("^A-Z") == key);
} body {
    string res;
    foreach (immutable i, immutable c; txt.toUpper.removechars("^A-Z"))
        res ~= (c + key[i % $] - 2 * 'A') % 26 + 'A';
    return res;
}

string decrypt(in string txt, in string key) pure @safe
in {
    assert(key.removechars("^A-Z") == key);
} body {
    string res;
    foreach (immutable i, immutable c; txt.toUpper.removechars("^A-Z"))
       res ~= (c - key[i % $] + 26) % 26 + 'A';
    return res;
}

void main() {
    immutable key = "VIGENERECIPHER";
    immutable original = "Beware the Jabberwock, my son!" ~
                         " The jaws that bite, the claws that catch!";
    immutable encoded = original.encrypt(key);
    writeln(encoded, "\n", encoded.decrypt(key));
}
```

```txt
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



### Alternative Version

```d
import std.stdio, std.range, std.ascii, std.string, std.algorithm,
       std.conv;

immutable mod = (in int m, in int n) pure nothrow @safe @nogc =>
    ((m % n) + n) % n;

immutable _s2v = (in string s) pure /*nothrow*/ @safe =>
    s.toUpper.removechars("^A-Z").map!q{ a - 'A' };

string _v2s(R)(R v) pure /*nothrow*/ @safe {
    return v.map!(x => uppercase[x.mod(26)]).text;
}

immutable encrypt = (in string txt, in string key) pure /*nothrow*/ @safe =>
    txt._s2v.zip(key._s2v.cycle).map!q{ a[0] + a[1] }._v2s;

immutable decrypt = (in string txt, in string key) pure /*nothrow*/ @safe =>
    txt._s2v.zip(key._s2v.cycle).map!q{ a[0] - a[1] }._v2s;

void main() {
    immutable key = "Vigenere Cipher!!!";
    immutable original = "Beware the Jabberwock, my son!" ~
                         " The jaws that bite, the claws that catch!";
    immutable encoded = original.encrypt(key);
    writeln(encoded, "\n", encoded.decrypt(key));
}
```

The output is the same.


## Elena

ELENA 4.x :

```elena
import system'text;
import system'math;
import system'routines;
import extensions;

class VCipher
{
    string encrypt(string txt, string pw, int d)
    {
        auto output := new TextBuilder();
        int pwi := 0;

        string PW := pw.upperCase();

        txt.upperCase().forEach:(t)
        {
            if(t >= $65)
            {
                int tmp := t.toInt() - 65 + d * (PW[pwi].toInt() - 65);
                if (tmp < 0)
                {
                    tmp += 26
                };
                output.write((65 + tmp.mod:26).toChar());
                pwi += 1;
                if (pwi == PW.Length) { pwi := 0 }
            }
        };

        ^ output.Value
    }
}

public program()
{
    var v := new VCipher();

    var s0 := "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
    var pw := "VIGENERECIPHER";

    console.printLine(s0,newLine,pw,newLine);
    var s1 := v.encrypt(s0, pw, 1);
    console.printLine("Encrypted:",s1);
    s1 := v.encrypt(s1, "VIGENERECIPHER", -1);
    console.printLine("Decrypted:",s1);
    console.printLine("Press any key to continue..");
    console.readChar()
}
```

```txt

Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
VIGENERECIPHER

Encrypted:WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted:BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
Press any key to continue..

```



## Elixir

```elixir
defmodule VigenereCipher do
  @base  ?A
  @size  ?Z - @base + 1

  def encrypt(text, key), do: crypt(text, key, 1)

  def decrypt(text, key), do: crypt(text, key, -1)

  defp crypt(text, key, dir) do
    text = String.upcase(text) |> String.replace(~r/[^A-Z]/, "") |> to_char_list
    key_iterator = String.upcase(key) |> String.replace(~r/[^A-Z]/, "") |> to_char_list
                   |> Enum.map(fn c -> (c - @base) * dir end) |> Stream.cycle
    Enum.zip(text, key_iterator)
    |> Enum.reduce('', fn {char, offset}, ciphertext ->
         [rem(char - @base + offset + @size, @size) + @base | ciphertext]
       end)
    |> Enum.reverse |> List.to_string
  end
end

plaintext = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
key = "Vigenere cipher"
ciphertext = VigenereCipher.encrypt(plaintext, key)
recovered  = VigenereCipher.decrypt(ciphertext, key)

IO.puts "Original: #{plaintext}"
IO.puts "Encrypted: #{ciphertext}"
IO.puts "Decrypted: #{recovered}"
```


```txt

Original: Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Erlang

Erlang is not ideal for string manipulation, but with some utility function definitions it can express this fairly elegantly:

```erlang
% Erlang implementation of Vigenère cipher
-module(vigenere).
-export([encrypt/2, decrypt/2]).
-import(lists, [append/2, filter/2, map/2, zipwith/3]).

% Utility functions for character tests and conversions
isupper([C|_]) -> isupper(C);
isupper(C)     -> (C >= $A) and (C =< $Z).

islower([C|_]) -> islower(C);
islower(C)     -> (C >= $a) and (C =< $z).

isalpha([C|_]) -> isalpha(C);
isalpha(C)     -> isupper(C) or islower(C).

toupper(S) when is_list(S) -> lists:map(fun toupper/1, S);
toupper(C) when (C >= $a) and (C =< $z) -> C - $a + $A;
toupper(C) -> C.

% modulo function that normalizes into positive range for positive divisor
mod(X,Y) -> (X rem Y + Y) rem Y.

% convert letter to position in alphabet (A=0,B=1,...,Y=24,Z=25).
to_pos(L) when L >= $A, L =< $Z -> L - $A.

% convert position in alphabet back to letter
from_pos(N) -> mod(N, 26) + $A.

% encode the given letter given the single-letter key
encipher(P, K) -> from_pos(to_pos(P) + to_pos(K)).

% decode the given letter given the single-letter key
decipher(C, K) -> from_pos(to_pos(C) - to_pos(K)).

% extend a list by repeating it until it is at least N elements long
cycle_to(N, List) when length(List) >= N -> List;
cycle_to(N, List) -> append(List, cycle_to(N-length(List), List)).

% Encryption prep: reduce string to only its letters, in uppercase
normalize(Str) -> toupper(filter(fun isalpha/1, Str)).

crypt(RawText, RawKey, Func) ->
  PlainText = normalize(RawText),
  zipwith(Func, PlainText, cycle_to(length(PlainText), normalize(RawKey))).

encrypt(Text, Key) -> crypt(Text, Key, fun encipher/2).
decrypt(Text, Key) -> crypt(Text, Key, fun decipher/2).
```


Demonstration code:

```erlang
-module(testvigenere).
-import(vigenere,[encrypt/2, decrypt/2]).
main(_) ->
  Key = "Vigenere cipher",
  CipherText = encrypt("Beware the Jabberwock, my son! The jaws that bite, the claws that catch!", Key),
  RecoveredText = decrypt(CipherText, Key),
  io:fwrite("Ciphertext: ~s~nDecrypted:  ~s~n", [CipherText, RecoveredText]).
```


```txt
Ciphertext: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted:  BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## Factor

<lang>USING: arrays ascii formatting kernel math math.functions
math.order sequences ;
IN: rosetta-code.vigenere-cipher

: mult-pad ( key input -- x )
    [ length ] bi@ 2dup < [ swap ] when / ceiling ;

: lengthen-pad ( key input -- rep-key input )
    [ mult-pad ] 2keep [ <repetition> concat ] dip
    [ length ] keep [ head ] dip ;

: normalize ( str -- only-upper-letters )
    >upper [ LETTER? ] filter ;

: vigenere-encrypt ( key input -- ecrypted )
    [ normalize ] bi@ lengthen-pad
    [ [ CHAR: A - ] map ] bi@ [ + 26 mod CHAR: A + ] 2map ;

: vigenere-decrypt ( key input -- decrypted )
    [ normalize ] bi@ lengthen-pad [ [ CHAR: A - ] map ] bi@
    [ - 26 - abs 26 mod CHAR: A + ] 2map ;

: main ( -- )
    "Vigenere cipher" dup
    "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
    2dup "Key: %s\nInput: %s\n" printf
    vigenere-encrypt dup "Encrypted: %s\n" printf
    vigenere-decrypt "Decrypted: %s\n" printf ;

MAIN: main
```

```txt

Key: Vigenere cipher
Input: Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Fortran

```fortran
program vigenere_cipher
  implicit none

  character(80) :: plaintext = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!", &
                   ciphertext = ""
  character(14) :: key = "VIGENERECIPHER"


  call encrypt(plaintext, ciphertext, key)
  write(*,*) plaintext
  write(*,*) ciphertext
  call decrypt(ciphertext, plaintext, key)
  write(*,*) plaintext

contains

subroutine encrypt(intxt, outtxt, k)
  character(*), intent(in)  :: intxt, k
  character(*), intent(out) :: outtxt
  integer :: chrn
  integer :: cp = 1, kp = 1
  integer :: i

  outtxt = ""
  do i = 1, len(trim(intxt))
    select case(intxt(i:i))
      case ("A":"Z", "a":"z")
        select case(intxt(i:i))
          case("a":"z")
            chrn = iachar(intxt(i:i)) - 32

          case default
            chrn = iachar(intxt(i:i))

        end select

        outtxt(cp:cp) = achar(modulo(chrn + iachar(k(kp:kp)), 26) + 65)
        cp = cp + 1
        kp = kp + 1
        if(kp > len(k)) kp = kp - len(k)

    end select
  end do
end subroutine

subroutine decrypt(intxt, outtxt, k)
  character(*), intent(in)  :: intxt, k
  character(*), intent(out) :: outtxt
  integer :: chrn
  integer :: cp = 1, kp = 1
  integer :: i

  outtxt = ""
  do i = 1, len(trim(intxt))
    chrn = iachar(intxt(i:i))
    outtxt(cp:cp) = achar(modulo(chrn - iachar(k(kp:kp)), 26) + 65)
    cp = cp + 1
    kp = kp + 1
    if(kp > len(k)) kp = kp - len(k)
   end do
end subroutine
end program
```

```txt
 Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
 WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
 BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```


=={{header|F_Sharp|F#}}==

```fsharp

module vigenere =
    let keyschedule (key:string) =
        let s = key.ToUpper().ToCharArray() |> Array.filter System.Char.IsLetter
        let l = Array.length s
        (fun n -> int s.[n % l])

    let enc k c = ((c + k - 130) % 26) + 65
    let dec k c = ((c - k + 130) % 26) + 65
    let crypt f key = Array.mapi (fun n c -> f (key n) c |> char)

    let encrypt key (plaintext:string) =
        plaintext.ToUpper().ToCharArray()
        |> Array.filter System.Char.IsLetter
        |> Array.map int
        |> crypt enc (keyschedule key)
        |> (fun a -> new string(a))

    let decrypt key (ciphertext:string) =
        ciphertext.ToUpper().ToCharArray()
        |> Array.map int
        |> crypt dec (keyschedule key)
        |> (fun a -> new string(a))

let passwd = "Vigenere Cipher"
let cipher = vigenere.encrypt passwd "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
let plain = vigenere.decrypt passwd cipher
printfn "%s\n%s" cipher plain

```



```txt
C:\src\fsharp>fsi vigenere.fsx
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Go


```go
package main

import "fmt"

type vkey string

func newVigenère(key string) (vkey, bool) {
    v := vkey(upperOnly(key))
    return v, len(v) > 0 // key length 0 invalid
}

func (k vkey) encipher(pt string) string {
    ct := upperOnly(pt)
    for i, c := range ct {
        ct[i] = 'A' + (c-'A'+k[i%len(k)]-'A')%26
    }
    return string(ct)
}

func (k vkey) decipher(ct string) (string, bool) {
    pt := make([]byte, len(ct))
    for i := range pt {
        c := ct[i]
        if c < 'A' || c > 'Z' {
            return "", false // invalid ciphertext
        }
        pt[i] = 'A' + (c-k[i%len(k)]+26)%26
    }
    return string(pt), true
}

// upperOnly extracts letters A-Z, a-z from a string and
// returns them all upper case in a byte slice.
// Useful for vkey constructor and encipher function.
func upperOnly(s string) []byte {
    u := make([]byte, 0, len(s))
    for i := 0; i < len(s); i++ {
        c := s[i]
        if c >= 'A' && c <= 'Z' {
            u = append(u, c)
        } else if c >= 'a' && c <= 'z' {
            u = append(u, c-32)
        }
    }
    return u
}

const testKey = "Vigenère Cipher"
const testPT = `Beware the Jabberwock, my son!
    The jaws that bite, the claws that catch!`

func main() {
    fmt.Println("Supplied key: ", testKey)
    v, ok := newVigenère(testKey)
    if !ok {
        fmt.Println("Invalid key")
        return
    }
    fmt.Println("Effective key:", v)
    fmt.Println("Plain text:", testPT)
    ct := v.encipher(testPT)
    fmt.Println("Enciphered:", ct)
    dt, ok := v.decipher(ct)
    if !ok {
        fmt.Println("Invalid ciphertext")
        return
    }
    fmt.Println("Deciphered:", dt)
}
```

```txt

Supplied key:  Vigenère Cipher
Effective key: VIGENRECIPHER
Plain text: Beware the Jabberwock, my son!
        The jaws that bite, the claws that catch!
Enciphered: WMCEEVXJMYHFSZZCSPBQAADUXYZRGAFKLCBQPXVOPKGYRAUBWHXTVBIL
Deciphered: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Haskell


```haskell
import Data.Char
import Text.Printf

-- Perform encryption or decryption, depending on f.
crypt f key = map toLetter . zipWith f (cycle key)
  where toLetter = chr . (+) (ord 'A')

-- Encrypt or decrypt one letter.
enc k c = (ord k + ord c) `mod` 26
dec k c = (ord c - ord k) `mod` 26

-- Given a key, encrypt or decrypt an input string.
encrypt = crypt enc
decrypt = crypt dec

-- Convert a string to have only upper case letters.
convert = map toUpper . filter isLetter

main = do
  let key  = "VIGENERECIPHER"
      text = "Beware the Jabberwock, my son! The jaws that bite, "
             ++ "the claws that catch!"
      encr = encrypt key $ convert text
      decr = decrypt key encr
  printf "    Input: %s\n      Key: %s\nEncrypted: %s\nDecrypted: %s\n"
    text key encr decr
```

```txt

    Input: Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
      Key: VIGENERECIPHER
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
   ptext := "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
   write("Key        = ",ekey := "VIGENERECIPHER")
   write("Plain Text = ",ptext)
   write("Normalized = ",GFormat(ptext := NormalizeText(ptext)))
   write("Enciphered = ",GFormat(ctext := Vignere("e",ekey,ptext)))
   write("Deciphered = ",GFormat(ptext := Vignere("d",ekey,ctext)))
end

procedure Vignere(mode,ekey,ptext,alpha)   #: Vignere cipher
   /alpha := &ucase                                              # default
   if *alpha ~= *cset(alpha) then runerr(205,alpha)              # no dups
   alpha ||:= alpha                                              # unobstructed

   every ctext:="" & p:=ptext[i := 1 to *ptext] & k:=ekey[(i-1)%*ekey+1] do
      case mode of {
         "e"|"encrypt":
            ctext||:=map(p,alpha[1+:*alpha/2],alpha[find(k,alpha)+:(*alpha/2)])
         "d"|"decrypt":
            ctext||:=map(p,alpha[find(k,alpha)+:(*alpha/2)],alpha[1+:*alpha/2])
         default: runerr(205,mode)
      }
return ctext
end
```


The following helper procedures will be of general use with classical cryptography tasks.

```Icon

link strings

procedure NormalizeText(ptext,alpha)       #: text/case classical crypto helper
   /alpha := &ucase                                              # default
   if &lcase === (alpha := cset(alpha)) then ptext := map(ptext) # lower
   if &ucase === alpha  then ptext := map(ptext,&lcase,&ucase)   # upper
   return deletec(ptext,&cset--alpha)                            # only alphas
end

procedure GFormat(text)                    #: 5 letter group formatting helper
   text ?  (s := "", until pos(0) do s ||:= " " || move(5)|tab(0))
   return s[2:0]
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings.icn provides deletec]

```txt
Key        = VIGENERECIPHER
Plain Text = Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Normalized = BEWAR ETHEJ ABBER WOCKM YSONT HEJAW STHAT BITET HECLA WSTHA TCATCH
Enciphered = WMCEE IKLGR PIFVM EUGXQ PWQVI OIAVE YXUEK FKBTA LVXTG AFXYE VKPAGY
Deciphered = BEWAR ETHEJ ABBER WOCKM YSONT HEJAW STHAT BITET HECLA WSTHA TCATCH
```



## J

'''Solution:'''

Using <code>vig</code> from the [[j:Addons/convert/misc/vig|convert/misc/vig addon]]:

```j
NB.*vig c Vigenère cipher
NB. cipher=. key 0 vig charset plain
NB. plain=. key 1 vig charset cipher
vig=: conjunction define
:
  r=. (#y) $ n i.x
  n {~ (#n) | (r*_1^m) + n i.y
)

ALPHA=: (65,:26) ];.0 a.               NB. Character Set
preprocess=: (#~ e.&ALPHA)@toupper     NB. force uppercase and discard non-alpha chars
vigEncryptRC=: 0 vig ALPHA preprocess
vigDecryptRC=: 1 vig ALPHA preprocess
```


'''Example Use:'''

```j
   'VIGENERECIPHER' vigEncryptRC 'Beware the Jabberwock, my son! The jaws that bite, the claws that catch!'
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
   'VIGENERECIPHER' vigDecryptRC 'WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY'
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## Java

```java
public class VigenereCipher {
    public static void main(String[] args) {
        String key = "VIGENERECIPHER";
        String ori = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
        String enc = encrypt(ori, key);
        System.out.println(enc);
        System.out.println(decrypt(enc, key));
    }

    static String encrypt(String text, final String key) {
        String res = "";
        text = text.toUpperCase();
        for (int i = 0, j = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c < 'A' || c > 'Z') continue;
            res += (char)((c + key.charAt(j) - 2 * 'A') % 26 + 'A');
            j = ++j % key.length();
        }
        return res;
    }

    static String decrypt(String text, final String key) {
        String res = "";
        text = text.toUpperCase();
        for (int i = 0, j = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c < 'A' || c > 'Z') continue;
            res += (char)((c - key.charAt(j) + 26) % 26 + 'A');
            j = ++j % key.length();
        }
        return res;
    }
}
```



```txt
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## JavaScript


```javascript
// helpers
// helper
function ordA(a) {
  return a.charCodeAt(0) - 65;
}

// vigenere
function vigenere(text, key, decode) {
  var i = 0, b;
  key = key.toUpperCase().replace(/[^A-Z]/g, '');
  return text.toUpperCase().replace(/[^A-Z]/g, '').replace(/[A-Z]/g, function(a) {
    b = key[i++ % key.length];
    return String.fromCharCode(((ordA(a) + (decode ? 26 - ordA(b) : ordA(b))) % 26 + 65));
  });
}

// example
var text = "The quick brown fox Jumped over the lazy Dog the lazy dog lazy dog dog";
var key = 'alex';
var enc = vigenere(text,key);
var dec = vigenere(enc,key,true);

console.log(enc);
console.log(dec);
```



## Jsish

From Javascript entry.

```javascript
/* Vigenère cipher, in Jsish */
"use strict";

function ordA(a:string):number {
    return a.charCodeAt(0) - 65;
}

// vigenere
function vigenereCipher(text:string, key:string, decode:boolean=false):string {
    var i = 0, b;
    key = key.toUpperCase().replace(/[^A-Z]/g, '');
    return text.toUpperCase().replace(/[^A-Z]/g, '').replace(/[A-Z]/g,
        function(a:string, idx:number, str:string) {
            b = key[i++ % key.length];
            return String.fromCharCode(((ordA(a) + (decode ? 26 - ordA(b) : ordA(b))) % 26 + 65));
        });
}

provide('vigenereCipher', 1);

if (Interp.conf('unitTest')) {
    var text = "The quick brown fox Jumped over the lazy Dog the lazy dog lazy dog dog";
    var key = 'jsish';
    var enc = vigenereCipher(text, key);
;    text;
;    enc;
;    vigenereCipher(enc, key, true);
}

/*
=!EXPECTSTART!=
text ==> The quick brown fox Jumped over the lazy Dog the lazy dog lazy dog dog
enc ==> CZMIBRUSTYXOVXVGBCEWNVWNLALPWSJRGVVPLPWSJRGVVPDIRFMGOVVP
vigenere(enc, key, true) ==> THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOGTHELAZYDOGLAZYDOGDOG
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u vigenereCipher.jsi
[PASS] vigenereCipher.jsi
```



## Julia

```Julia
function encrypt(msg::AbstractString, key::AbstractString)
    msg    = uppercase(join(filter(isalpha, collect(msg))))
    key    = uppercase(join(filter(isalpha, collect(key))))
    msglen = length(msg)
    keylen = length(key)

    if keylen < msglen
        key = repeat(key, div(msglen - keylen, keylen) + 2)[1:msglen]
    end

    enc = Vector{Char}(msglen)

    @inbounds for i in 1:length(msg)
        enc[i] = Char((Int(msg[i]) + Int(key[i]) - 130) % 26 + 65)
    end

    return join(enc)
end

function decrypt(enc::AbstractString, key::AbstractString)
    enc    = uppercase(join(filter(isalpha, collect(enc))))
    key    = uppercase(join(filter(isalpha, collect(key))))
    msglen = length(enc)
    keylen = length(key)

    if keylen < msglen
        key = repeat(key, div(msglen - keylen, keylen) + 2)[1:msglen]
    end

    msg = Vector{Char}(msglen)

    @inbounds for i in 1:length(enc)
        msg[i] = Char((Int(enc[i]) - Int(key[i]) + 26) % 26 + 65)
    end

    return join(msg)
end

const messages = ("Attack at dawn.", "Don't attack.", "The war is over.")
const key = "LEMON"

for msg in messages
    enc = encrypt(msg, key)
    dec = decrypt(enc, key)
    println("Original: $msg\n -> encrypted: $enc\n -> decrypted: $dec")
end
```


```txt
Original: Attack at dawn.
 -> encrypted: LXFOPVEFRNHR
 -> decrypted: ATTACKATDAWN
Original: Don't attack.
 -> encrypted: OSZHNEXMQX
 -> decrypted: DONTATTACK
Original: The war is over.
 -> encrypted: ELQKNCMECIPV
 -> decrypted: THEWARISOVER
```



## Kotlin


```scala
// version 1.1.3

fun vigenere(text: String, key: String, encrypt: Boolean = true): String {
    val t = if (encrypt) text.toUpperCase() else text
    val sb = StringBuilder()
    var ki = 0
    for (c in t) {
        if (c !in 'A'..'Z') continue
        val ci = if (encrypt)
            (c.toInt() + key[ki].toInt() - 130) % 26
        else
            (c.toInt() - key[ki].toInt() +  26) % 26
        sb.append((ci + 65).toChar())
        ki = (ki + 1) % key.length
    }
    return sb.toString()
}

fun main(args: Array<String>) {
    val key = "VIGENERECIPHER"
    val text = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
    val encoded = vigenere(text, key)
    println(encoded)
    val decoded = vigenere(encoded, key, false)
    println(decoded)
}
```


```txt

WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Liberty BASIC


```lb

ori$ = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
key$ = filter$("vigenerecipher")
print ori$
print key$
enc$ = encrypt$(ori$, key$)
print enc$
dec$ = decrypt$(enc$, key$)
print dec$

end

function encrypt$(text$, key$)
    flt$ = filter$(text$)
    encrypt$ = ""
    j = 1
    for i = 1 to len(flt$)
        m$ = mid$(flt$, i, 1)
        m = asc(m$)-asc("A")
        k$ = mid$(key$, j, 1)
        k = asc(k$)-asc("A")
        j = (j mod len(key$)) + 1
        c = (m + k) mod 26
        c$=chr$(asc("A")+c)
        encrypt$=encrypt$+c$
    next
end function

function decrypt$(flt$, key$)
    decrypt$ = ""
    j = 1
    for i = 1 to len(flt$)
        m$ = mid$(flt$, i, 1)
        m = asc(m$)-asc("A")
        k$ = mid$(key$, j, 1)
        k = asc(k$)-asc("A")
        j = (j mod len(key$)) + 1
        c = (m - k + 26) mod 26
        c$=chr$(asc("A")+c)
        decrypt$=decrypt$+c$
    next
end function

function filter$(ori$)
'a..z A..Z go caps, other skipped
    filter$=""
    for i = 1 to len(ori$)
        c$ = upper$(mid$(ori$,i,1))
        if instr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c$) then filter$ = filter$ + c$
    next
end function

```


 Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
 VIGENERECIPHER
 WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
 BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH


## Lua


```lua
function Encrypt( _msg, _key )
    local msg = { _msg:upper():byte( 1, -1 ) }
    local key = { _key:upper():byte( 1, -1 ) }
    local enc = {}

    local j, k = 1, 1
    for i = 1, #msg do
        if msg[i] >= string.byte('A') and msg[i] <= string.byte('Z') then
            enc[k] = ( msg[i] + key[j] - 2*string.byte('A') ) % 26 + string.byte('A')

            k = k + 1
            if j == #key then j = 1 else j = j + 1 end
        end
    end

    return string.char( unpack(enc) )
end

function Decrypt( _msg, _key )
    local msg = { _msg:byte( 1, -1 ) }
    local key = { _key:upper():byte( 1, -1 ) }
    local dec = {}

    local j = 1
    for i = 1, #msg do
       dec[i] = ( msg[i] - key[j] + 26 ) % 26 + string.byte('A')

       if j == #key then j = 1 else j = j + 1 end
    end

    return string.char( unpack(dec) )
end


original = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
key = "VIGENERECIPHER";

encrypted = Encrypt( original, key )
decrypted = Decrypt( encrypted, key )

print( encrypted )
print( decrypted )
```


```txt
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## Mathematica


```Mathematica
encode[text_String, key_String] :=
 Module[{textCode, keyCode},
  textCode =
   Cases[ToCharacterCode[
      ToUpperCase@
       text], _?(IntervalMemberQ[Interval@{65, 90}, #] &)] - 65;
  keyCode =
   Cases[ToCharacterCode[
      ToUpperCase@
       key], _?(IntervalMemberQ[Interval@{65, 90}, #] &)] - 65;
  keyCode =
   If[Length[textCode] < Length[keyCode],
    keyCode[[;; Length@textCode]],
    PadRight[keyCode, Length@textCode, keyCode]];
  FromCharacterCode[Mod[textCode + keyCode, 26] + 65]]

decode[text_String, key_String] :=
 Module[{textCode, keyCode},
  textCode =
   Cases[ToCharacterCode[
      ToUpperCase@
       text], _?(IntervalMemberQ[Interval@{65, 90}, #] &)] - 65;
  keyCode =
   Cases[ToCharacterCode[
      ToUpperCase@
       key], _?(IntervalMemberQ[Interval@{65, 90}, #] &)] - 65;
  keyCode =
   If[Length[textCode] < Length[keyCode],
    keyCode[[;; Length@textCode]],
    PadRight[keyCode, Length@textCode, keyCode]];
  FromCharacterCode[Mod[textCode - keyCode, 26] + 65]]
```



```txt
key = "Vigenere Cipher";
text = "Beware the Jabberwock, my son! The jaws that bite, the claws \
that catch!";
code = encode[text, key]

WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY

decode[code, key]

BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH


```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

pt  = 'Attack at dawn!'
key = 'LEMON'
test(key, pt)

key = 'N' -- rot-13
test(key, pt)

key = 'B' -- Caesar
test(key, pt)

pt = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
key = 'A'
test(key, pt)

pt = sampledata()
key = 'Hamlet; Prince of Denmark'
test(key, pt)

return

method vigenere(meth, key, text) public static

  select
    when 'encipher'.abbrev(meth.lower, 1) then df = 1
    when 'decipher'.abbrev(meth.lower, 1) then df = -1
    otherwise signal IllegalArgumentException(meth 'must be "encipher" or "decipher"')
    end

  alpha = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

  text = stringscrubber(text)
  key  = stringscrubber(key)
  code = ''
  loop l_ = 1 to text.length()
    M = alpha.pos(text.substr(l_, 1)) - 1
    k_ = (l_ - 1) // key.length()
    K = alpha.pos(key.substr(k_ + 1, 1)) - 1
    C = mod((M + K * df), alpha.length())
    C = alpha.substr(C + 1, 1)
    code = code || C
    end l_

  return code

method vigenere_encipher(key, plaintext) public static

  return vigenere('encipher', key, plaintext)

method vigenere_decipher(key, ciphertext) public static

  return vigenere('decipher', key, ciphertext)

method mod(N = int, D = int) private static

return (D + (N // D)) // D

method stringscrubber(cleanup) private static

  alpha = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

  cleanup = cleanup.upper.space(0)
  loop label f_ forever
    x_ = cleanup.verify(alpha)
    if x_ = 0 then leave f_
    cleanup = cleanup.changestr(cleanup.substr(x_, 1), '')
    end f_

  return cleanup

method test(key, pt) private static

  ct = vigenere_encipher(key, pt)
  display(ct)
  dt = vigenere_decipher(key, ct)
  display(dt)

  return

method display(text) public static

  line = ''
  o_ = 0
  loop c_ = 1 to text.length()
    b_ = o_ // 5
    o_ = o_ + 1
    if b_ = 0 then line = line' '
    line = line || text.substr(c_, 1)
    end c_

  say '....+....|'.copies(8)
  loop label l_ forever
    parse line w1 w2 w3 w4 w5 w6 W7 w8 w9 w10 w11 w12 line
    pline = w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12
    say pline.strip()
    if line.strip().length() = 0 then leave l_
    end l_
  say

  return

method sampledata() private static returns Rexx

  NL = char('\n')
  antic_disposition = Rexx[]

  antic_disposition = [                                         -
    Rexx("To be, or not to be--that is the question:"        ), -
    Rexx("Whether 'tis nobler in the mind to suffer"         ), -
    Rexx("The slings and arrows of outrageous fortune"       ), -
    Rexx("Or to take arms against a sea of troubles"         ), -
    Rexx("And by opposing end them. To die, to sleep--"      ), -
    Rexx("No more--and by a sleep to say we end"             ), -
    Rexx("The heartache, and the thousand natural shocks"    ), -
    Rexx("That flesh is heir to. 'Tis a consummation"        ), -
    Rexx("Devoutly to be wished. To die, to sleep--"         ), -
    Rexx("To sleep--perchance to dream: ay, there's the rub,"), -
    Rexx("For in that sleep of death what dreams may come"   ), -
    Rexx("When we have shuffled off this mortal coil,"       ), -
    Rexx("Must give us pause. There's the respect"           ), -
    Rexx("That makes calamity of so long life."              ), -
    Rexx("For who would bear the whips and scorns of time,"  ), -
    Rexx("Th' oppressor's wrong, the proud man's contumely"  ), -
    Rexx("The pangs of despised love, the law's delay,"      ), -
    Rexx("The insolence of office, and the spurns"           ), -
    Rexx("That patient merit of th' unworthy takes,"         ), -
    Rexx("When he himself might his quietus make"            ), -
    Rexx("With a bare bodkin? Who would fardels bear,"       ), -
    Rexx("To grunt and sweat under a weary life,"            ), -
    Rexx("But that the dread of something after death,"      ), -
    Rexx("The undiscovered country, from whose bourn"        ), -
    Rexx("No traveller returns, puzzles the will,"           ), -
    Rexx("And makes us rather bear those ills we have"       ), -
    Rexx("Than fly to others that we know not of?"           ), -
    Rexx("Thus conscience does make cowards of us all,"      ), -
    Rexx("And thus the native hue of resolution"             ), -
    Rexx("Is sicklied o'er with the pale cast of thought,"   ), -
    Rexx("And enterprise of great pith and moment"           ), -
    Rexx("With this regard their currents turn awry"         ), -
    Rexx("And lose the name of action. -- Soft you now,"     ), -
    Rexx("The fair Ophelia! -- Nymph, in thy orisons"        ), -
    Rexx("Be all my sins remembered."                        )  -
    ]

    melancholy_dane = Rexx('')
    loop l_ = 0 for antic_disposition.length
      melancholy_dane = melancholy_dane || antic_disposition[l_] || NL
      end l_

    return melancholy_dane

```


<pre style="height: 60ex; overflow:scroll">
....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
LXFOP VEFRN HR

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ATTAC KATDA WN

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
NGGNP XNGQN JA

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ATTAC KATDA WN

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
BUUBD LBUEB XO

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ATTAC KATDA WN

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ABCDE FGHIJ KLMNO PQRST UVWXY Z

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ABCDE FGHIJ KLMNO PQRST UVWXY Z

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
AONPS KCFBG QFSYK EGUSK RLQGP WMXFV JJIHM HVGUS EYILQ CMGIY MZKRR YRWHR
FVBAH QDPBC XANPH OWUSJ EOWYB TDLKX DLASQ VHZQI BDTFD HKQLV FHRON KRGYD
WRMOW DYOGM PXHRV QDCCU SSFUN XOUDF SIFIF LVGXC XOIRB NFWVR QLRWQ PIFNI
SUWSF MYNOL NPELX WVIEV EQMHE APTYO AHAFW TCUVN VYFFO WUACB CAHME JETJP
VULSN UXBFI JKJOZ DYFBR BVQRR JYSHF LPDVB QHMLW VLGQH WJWSF XEVZA OEWIX
EGMEE LOSFI GADIO HMMJX ATIMF VLSWX FSARZ XAHME WETVX BHHSF WLJTA KNYEM
XWFPP KBOIK MHRJQ HRFLS TFJYA VLBHJ HMLWZ ARKKS CATPF JJBTK ZSZVT NGSVD
OEDPW MWVZR UTSHW XUMTD KREEN EEPDQ GASTX RPBZG CSMER ZVPWF EBWPR GHEEF
HVGOI BDEGS JKBTR GTIXV YEKRV PBCIL HFZFY VCSJX UQPIY BDYLR LRFVG WQSQB
XUQPR XNSAQ HXHGQ FGHZT YIGTE CKDSP PPTNK PRKRG TOIAO EFPVF RTGXP ELGJI
GUXVA ETYKA PMEMX CKURT MHTIX UGNNY YTTJX TZVAJ JIBMH LVYSV VMMUR LMWZA
DWMSY XWZMK VGPTT LFTGV JBFOW SZLBI OLVKF MCHXA JJRCV HTJVH ZTRXK SIPEM
JELRT EKJDV LXIWO IUFEL TIKPR FVSFG SSEOD OAHUY KTUKM EFIOY KXUQU ENPSO
ZZXGV LPQYB YUCSD ODGOO EPFHJ IVAQX FFYIY XEIBL TGCRL ELHMN IGYKI JULCK
UDYLO XHLAE CXVJU FRMRK RVSQT PEHNM UCZSY KEARL PDVOF SIKHK PNVAS PQSJZ
OKYMT TFWVD EAPKI BHHHB QSDKR EOZAT GUABH YGFOP NZDKR BSFSI GPKQI GLIJR
JEQSF VBTUZ RBHJQ PMPWJ GSRDW ZDOTT PTTAV KNUXC KWLBG GYDHN PPRMT IXEKW
STIKE QAKZP TTLRW BFURP XKNWL GTIJB LGMCH MWVQE EYFWH RGETL BUAIC CTCUT
BUIHM HRNYE FPHCF TSGHF NGASI SRAGT EWKPR AALXA ZIAAQ DMLRG TYFBP SAYWU
TRTYO CGNQW EQMVW IEDPH

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
TOBEO RNOTT OBETH ATIST HEQUE STION WHETH ERTIS NOBLE RINTH EMIND TOSUF
FERTH ESLIN GSAND ARROW SOFOU TRAGE OUSFO RTUNE ORTOT AKEAR MSAGA INSTA
SEAOF TROUB LESAN DBYOP POSIN GENDT HEMTO DIETO SLEEP NOMOR EANDB YASLE
EPTOS AYWEE NDTHE HEART ACHEA NDTHE THOUS ANDNA TURAL SHOCK STHAT FLESH
ISHEI RTOTI SACON SUMMA TIOND EVOUT LYTOB EWISH EDTOD IETOS LEEPT OSLEE
PPERC HANCE TODRE AMAYT HERES THERU BFORI NTHAT SLEEP OFDEA THWHA TDREA
MSMAY COMEW HENWE HAVES HUFFL EDOFF THISM ORTAL COILM USTGI VEUSP AUSET
HERES THERE SPECT THATM AKESC ALAMI TYOFS OLONG LIFEF ORWHO WOULD BEART
HEWHI PSAND SCORN SOFTI METHO PPRES SORSW RONGT HEPRO UDMAN SCONT UMELY
THEPA NGSOF DESPI SEDLO VETHE LAWSD ELAYT HEINS OLENC EOFOF FICEA NDTHE
SPURN STHAT PATIE NTMER ITOFT HUNWO RTHYT AKESW HENHE HIMSE LFMIG HTHIS
QUIET USMAK EWITH ABARE BODKI NWHOW OULDF ARDEL SBEAR TOGRU NTAND SWEAT
UNDER AWEAR YLIFE BUTTH ATTHE DREAD OFSOM ETHIN GAFTE RDEAT HTHEU NDISC
OVERE DCOUN TRYFR OMWHO SEBOU RNNOT RAVEL LERRE TURNS PUZZL ESTHE WILLA
NDMAK ESUSR ATHER BEART HOSEI LLSWE HAVET HANFL YTOOT HERST HATWE KNOWN
OTOFT HUSCO NSCIE NCEDO ESMAK ECOWA RDSOF USALL ANDTH USTHE NATIV EHUEO
FRESO LUTIO NISSI CKLIE DOERW ITHTH EPALE CASTO FTHOU GHTAN DENTE RPRIS
EOFGR EATPI THAND MOMEN TWITH THISR EGARD THEIR CURRE NTSTU RNAWR YANDL
OSETH ENAME OFACT IONSO FTYOU NOWTH EFAIR OPHEL IANYM PHINT HYORI SONSB
EALLM YSINS REMEM BERED

```



## Nim


```nim
import strutils

proc isAlpha(c): bool = c in 'a'..'z' or c in 'A'..'Z'

proc encrypt(msg, key): string =
  result = ""
  var pos = 0
  for c in msg:
    if isAlpha c:
      result.add chr(((ord(key[pos]) + ord(toUpper c)) mod 26) + ord('A'))
      pos = (pos + 1) mod key.len

proc decrypt(msg, key): string =
  result = ""
  var pos = 0
  for c in msg:
    result.add chr(((26 + ord(c) - ord(key[pos])) mod 26) + ord('A'))
    pos = (pos + 1) mod key.len

const text = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
const key = "VIGENERECIPHER"

let encr = encrypt(text, key)
let decr = decrypt(encr, key)

echo text
echo encr
echo decr
```

```txt
Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## Objeck

```objeck

bundle Default {
   class VigenereCipher {
      function : Main(args : String[]) ~ Nil {
         key := "VIGENERECIPHER";
         ori := "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
         enc := encrypt(ori, key);
         IO.Console->Print("encrypt: ")->PrintLine(enc);
         IO.Console->Print("decrypt: ")->PrintLine(decrypt(enc, key));
      }

      function : native : encrypt(text : String, key : String) ~ String {
         res := "";
         text := text->ToUpper();
         j := 0;

         each(i : text) {
            c := text->Get(i);
            if(c >= 'A' & c <= 'Z') {
               res->Append(((c + key->Get(j) - 2 * 'A') % 26 + 'A')->As(Char));
               j += 1;
               j := j % key->Size();
            };
        };

        return res;
      }

      function : native : decrypt(text : String, key : String) ~ String {
         res := "";
         text := text->ToUpper();
         j := 0;

         each(i : text) {
            c := text->Get(i);
            if(c >= 'A' & c <= 'Z') {
               res->Append(((c - key->Get(j) + 26) % 26 + 'A')->As(Char));
               j += 1;
               j := j % key->Size();
            };
         };

         return res;
      }
   }
}

```


```txt

encrypt: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
decrypt: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## OCaml

```ocaml
let cipher src key crypt =
  let str = String.uppercase src in
  let key = String.uppercase key in

  (* strip out non-letters *)
  let len = String.length str in
  let rec aux i j =
    if j >= len then String.sub str 0 i else
    if str.[j] >= 'A' && str.[j] <= 'Z'
    then (str.[i] <- str.[j]; aux (succ i) (succ j))
    else aux i (succ j)
  in
  let res = aux 0 0 in

  let slen = String.length res in
  let klen = String.length key in

  let d = int_of_char in
  let f =
    if crypt
    then fun i -> d res.[i] - d 'A' + d key.[i mod klen] - d 'A'
    else fun i -> d res.[i] - d key.[i mod klen] + 26
  in
  for i = 0 to pred slen do
    res.[i] <- char_of_int (d 'A' + (f i) mod 26)
  done;
  (res)

let () =
  let str = "Beware the Jabberwock, my son! The jaws that bite, \
             the claws that catch!" in
  let key = "VIGENERECIPHER" in

  let cod = cipher str key true in
  let dec = cipher cod key false in

  Printf.printf "Text: %s\n" str;
  Printf.printf "key:  %s\n" key;
  Printf.printf "Code: %s\n" cod;
  Printf.printf "Back: %s\n" dec;
;;
```


Run:


```txt
$ ocaml vigenere_cipher.ml
Text: Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
key:  VIGENERECIPHER
Code: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Back: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## ooRexx

A reworking of the [[#NetRexx|NetRexx]] version using Open Object Rexx but shouldn't take much to translate to Classic Rexx.

```REXX
/* Rexx */
Do
  alpha = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  key   = 'LEMON'

  pt = 'Attack at dawn!'
  Call test key, pt

  key = 'N'
  Call test key, pt

  key = 'B'
  Call test key, pt

  pt = alpha
  key = 'A'
  Call test key, pt

  pt = sampledata()
  key = 'Hamlet; Prince of Denmark'
  Call test key, pt

  Return
End
Exit

vigenere:
Procedure Expose alpha
Do
  Parse upper Arg meth, key, text

  Select
    When 'ENCIPHER'~abbrev(meth, 1) = 1 then df = 1
    When 'DECIPHER'~abbrev(meth, 1) = 1 then df = -1
    Otherwise Do
      Say meth 'invalid.  Must be "ENCIPHER" or "DECIPHER"'
      Exit
      End
    End

  text = stringscrubber(text)
  key  = stringscrubber(key)
  code = ''

  Do l_ = 1 to text~length()
    M = alpha~pos(text~substr(l_, 1)) - 1
    k_ = (l_ - 1) // key~length()
    K = alpha~pos(key~substr(k_ + 1, 1)) - 1
    C = mod((M + K * df), alpha~length())
    C = alpha~substr(C + 1, 1)
    code = code || C
    End l_

  Return code

  Return
End
Exit

vigenere_encipher:
Procedure Expose alpha
Do
  Parse upper Arg key, plaintext

  Return vigenere('ENCIPHER', key, plaintext)
End
Exit

vigenere_decipher:
Procedure Expose alpha
Do
  Parse upper Arg key, ciphertext

  Return vigenere('DECIPHER', key, ciphertext)
End
Exit

mod:
Procedure
Do
  Parse Arg N, D

  Return (D + (N // D)) // D
End
Exit

stringscrubber:
Procedure Expose alpha
Do
  Parse upper Arg cleanup

  cleanup = cleanup~space(0)
  Do label f_ forever
    x_ = cleanup~verify(alpha)
    If x_ = 0 then Leave f_
    cleanup = cleanup~changestr(cleanup~substr(x_, 1), '')
    end f_

  Return cleanup
End
Exit

test:
Procedure Expose alpha
Do
  Parse Arg key, pt

  ct = vigenere_encipher(key, pt)
  Call display ct
  dt = vigenere_decipher(key, ct)
  Call display dt

  Return
End
Exit

display:
Procedure
Do
  Parse Arg text

  line = ''
  o_ = 0
  Do c_ = 1 to text~length()
    b_ = o_ // 5
    o_ = o_ + 1
    If b_ = 0 then line = line' '
    line = line || text~substr(c_, 1)
    End c_

  Say '....+....|'~copies(8)
  Do label l_ forever
    Parse Var line w1 w2 w3 w4 w5 w6 W7 w8 w9 w10 w11 w12 line
    pline = w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12
    Say pline~strip()
    If line~strip()~length() = 0 then Leave l_
    End l_
  Say

  Return
End
Exit

sampledata:
Procedure
Do

  NL = '0a'x
  X = 0
  antic_disposition. = ''
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "To be, or not to be--that is the question:"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "Whether 'tis nobler in the mind to suffer"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "The slings and arrows of outrageous fortune"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "Or to take arms against a sea of troubles"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "And by opposing end them. To die, to sleep--"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "No more--and by a sleep to say we end"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "The heartache, and the thousand natural shocks"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "That flesh is heir to. 'Tis a consummation"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "Devoutly to be wished. To die, to sleep--"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "To sleep--perchance to dream: ay, there's the rub,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "For in that sleep of death what dreams may come"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "When we have shuffled off this mortal coil,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "Must give us pause. There's the respect"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "That makes calamity of so long life."
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "For who would bear the whips and scorns of time,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "Th' oppressor's wrong, the proud man's contumely"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "The pangs of despised love, the law's delay,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "The insolence of office, and the spurns"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "That patient merit of th' unworthy takes,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "When he himself might his quietus make"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "With a bare bodkin? Who would fardels bear,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "To grunt and sweat under a weary life,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "But that the dread of something after death,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "The undiscovered country, from whose bourn"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "No traveller returns, puzzles the will,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "And makes us rather bear those ills we have"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "Than fly to others that we know not of?"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "Thus conscience does make cowards of us all,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "And thus the native hue of resolution"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "Is sicklied o'er with the pale cast of thought,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "And enterprise of great pith and moment"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "With this regard their currents turn awry"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "And lose the name of action. -- Soft you now,"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "The fair Ophelia! -- Nymph, in thy orisons"
  X = X + 1; antic_disposition.0 = X; antic_disposition.X = "Be all my sins remembered."

  melancholy_dane = ''
  Do l_ = 1 for antic_disposition.0
    melancholy_dane = melancholy_dane || antic_disposition.l_ || NL
    End l_

  Return melancholy_dane
End
Exit

```


<pre style="height: 60ex; overflow: scroll;">
....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
LXFOP VEFRN HR

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ATTAC KATDA WN

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
NGGNP XNGQN JA

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ATTAC KATDA WN

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
BUUBD LBUEB XO

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ATTAC KATDA WN

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ABCDE FGHIJ KLMNO PQRST UVWXY Z

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
ABCDE FGHIJ KLMNO PQRST UVWXY Z

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
AONPS KCFBG QFSYK EGUSK RLQGP WMXFV JJIHM HVGUS EYILQ CMGIY MZKRR YRWHR
FVBAH QDPBC XANPH OWUSJ EOWYB TDLKX DLASQ VHZQI BDTFD HKQLV FHRON KRGYD
WRMOW DYOGM PXHRV QDCCU SSFUN XOUDF SIFIF LVGXC XOIRB NFWVR QLRWQ PIFNI
SUWSF MYNOL NPELX WVIEV EQMHE APTYO AHAFW TCUVN VYFFO WUACB CAHME JETJP
VULSN UXBFI JKJOZ DYFBR BVQRR JYSHF LPDVB QHMLW VLGQH WJWSF XEVZA OEWIX
EGMEE LOSFI GADIO HMMJX ATIMF VLSWX FSARZ XAHME WETVX BHHSF WLJTA KNYEM
XWFPP KBOIK MHRJQ HRFLS TFJYA VLBHJ HMLWZ ARKKS CATPF JJBTK ZSZVT NGSVD
OEDPW MWVZR UTSHW XUMTD KREEN EEPDQ GASTX RPBZG CSMER ZVPWF EBWPR GHEEF
HVGOI BDEGS JKBTR GTIXV YEKRV PBCIL HFZFY VCSJX UQPIY BDYLR LRFVG WQSQB
XUQPR XNSAQ HXHGQ FGHZT YIGTE CKDSP PPTNK PRKRG TOIAO EFPVF RTGXP ELGJI
GUXVA ETYKA PMEMX CKURT MHTIX UGNNY YTTJX TZVAJ JIBMH LVYSV VMMUR LMWZA
DWMSY XWZMK VGPTT LFTGV JBFOW SZLBI OLVKF MCHXA JJRCV HTJVH ZTRXK SIPEM
JELRT EKJDV LXIWO IUFEL TIKPR FVSFG SSEOD OAHUY KTUKM EFIOY KXUQU ENPSO
ZZXGV LPQYB YUCSD ODGOO EPFHJ IVAQX FFYIY XEIBL TGCRL ELHMN IGYKI JULCK
UDYLO XHLAE CXVJU FRMRK RVSQT PEHNM UCZSY KEARL PDVOF SIKHK PNVAS PQSJZ
OKYMT TFWVD EAPKI BHHHB QSDKR EOZAT GUABH YGFOP NZDKR BSFSI GPKQI GLIJR
JEQSF VBTUZ RBHJQ PMPWJ GSRDW ZDOTT PTTAV KNUXC KWLBG GYDHN PPRMT IXEKW
STIKE QAKZP TTLRW BFURP XKNWL GTIJB LGMCH MWVQE EYFWH RGETL BUAIC CTCUT
BUIHM HRNYE FPHCF TSGHF NGASI SRAGT EWKPR AALXA ZIAAQ DMLRG TYFBP SAYWU
TRTYO CGNQW EQMVW IEDPH

....+....|....+....|....+....|....+....|....+....|....+....|....+....|....+....|
TOBEO RNOTT OBETH ATIST HEQUE STION WHETH ERTIS NOBLE RINTH EMIND TOSUF
FERTH ESLIN GSAND ARROW SOFOU TRAGE OUSFO RTUNE ORTOT AKEAR MSAGA INSTA
SEAOF TROUB LESAN DBYOP POSIN GENDT HEMTO DIETO SLEEP NOMOR EANDB YASLE
EPTOS AYWEE NDTHE HEART ACHEA NDTHE THOUS ANDNA TURAL SHOCK STHAT FLESH
ISHEI RTOTI SACON SUMMA TIOND EVOUT LYTOB EWISH EDTOD IETOS LEEPT OSLEE
PPERC HANCE TODRE AMAYT HERES THERU BFORI NTHAT SLEEP OFDEA THWHA TDREA
MSMAY COMEW HENWE HAVES HUFFL EDOFF THISM ORTAL COILM USTGI VEUSP AUSET
HERES THERE SPECT THATM AKESC ALAMI TYOFS OLONG LIFEF ORWHO WOULD BEART
HEWHI PSAND SCORN SOFTI METHO PPRES SORSW RONGT HEPRO UDMAN SCONT UMELY
THEPA NGSOF DESPI SEDLO VETHE LAWSD ELAYT HEINS OLENC EOFOF FICEA NDTHE
SPURN STHAT PATIE NTMER ITOFT HUNWO RTHYT AKESW HENHE HIMSE LFMIG HTHIS
QUIET USMAK EWITH ABARE BODKI NWHOW OULDF ARDEL SBEAR TOGRU NTAND SWEAT
UNDER AWEAR YLIFE BUTTH ATTHE DREAD OFSOM ETHIN GAFTE RDEAT HTHEU NDISC
OVERE DCOUN TRYFR OMWHO SEBOU RNNOT RAVEL LERRE TURNS PUZZL ESTHE WILLA
NDMAK ESUSR ATHER BEART HOSEI LLSWE HAVET HANFL YTOOT HERST HATWE KNOWN
OTOFT HUSCO NSCIE NCEDO ESMAK ECOWA RDSOF USALL ANDTH USTHE NATIV EHUEO
FRESO LUTIO NISSI CKLIE DOERW ITHTH EPALE CASTO FTHOU GHTAN DENTE RPRIS
EOFGR EATPI THAND MOMEN TWITH THISR EGARD THEIR CURRE NTSTU RNAWR YANDL
OSETH ENAME OFACT IONSO FTYOU NOWTH EFAIR OPHEL IANYM PHINT HYORI SONSB
EALLM YSINS REMEM BERED

```



## Pascal


```pascal

// The Vigenere cipher in reasonably standard Pascal
// <no library functions: all conversions hand-coded>
PROGRAM Vigenere;

// get a letter's alphabetic position (A=0)
FUNCTION letternum(letter: CHAR): BYTE;
	BEGIN
		letternum := (ord(letter)-ord('A'));
	END;

// convert a character to uppercase
FUNCTION uch(ch: CHAR): CHAR;
	BEGIN
		uch := ch;
		IF ch IN ['a'..'z'] THEN
			uch := chr(ord(ch) AND $5F);
	END;

// convert a string to uppercase
FUNCTION ucase(str: STRING): STRING;
	VAR i: BYTE;
	BEGIN
		ucase := '';
		FOR i := 1 TO Length(str) DO
			ucase := ucase + uch(str[i]);
	END;

// construct a Vigenere-compatible string:
// uppercase; no spaces or punctuation.
FUNCTION vstr(pt: STRING): STRING;
	VAR c: Cardinal;
		s: STRING;
	BEGIN
		vstr:= '';
		s 	:= ucase(pt);
		FOR c := 1 TO Length(s) DO BEGIN
			IF s[c] IN ['A'..'Z'] THEN
				vstr += s[c];
		END;
	END;

// construct a repeating Vigenere key
FUNCTION vkey(pt, key: STRING): STRING;
	VAR c,n: Cardinal;
		k  : STRING;
	BEGIN
		k    := vstr(key);
		vkey := '';
		FOR c := 1 TO Length(pt) DO BEGIN
			n := c mod Length(k);
			IF n>0 THEN vkey += k[n] ELSE vkey += k[Length(k)];
		END;
	END;

// Vigenere encipher
FUNCTION enVig(pt,key:STRING): STRING;
	VAR ct: STRING;
		c,n	 : Cardinal;
	BEGIN
		ct := pt;
		FOR c := 1 TO Length(pt) DO BEGIN
			n := letternum(pt[c])+letternum(key[c]);
			n := n mod 26;
			ct[c]:=chr(ord('A')+n);
		END;
		enVig := ct;
	END;

// Vigenere decipher
FUNCTION deVig(ct,key:STRING): STRING;
	VAR pt	: STRING;
		c,n	: INTEGER;
	BEGIN
		pt := ct;
		FOR c := 1 TO Length(ct) DO BEGIN
			n := letternum(ct[c])-letternum(key[c]);
			IF n<0 THEN n:=26+n;
			pt[c]:=chr(ord('A')+n);
		END;
		deVig := pt;
	END;


VAR 	key: STRING = 'Vigenere cipher';
		msg: STRING = 'Beware the Jabberwock! The jaws that bite, the claws that catch!';
		vtx: STRING = '';
		ctx: STRING = '';
		ptx: STRING = '';

BEGIN
	// make Vigenere-compatible
	vtx := vstr(msg);
	key := vkey(vtx,key);
	// Vigenere encipher / decipher
	ctx := enVig(vtx,key);
	ptx := deVig(ctx,key);
	// display results
	Writeln('Message      : ',msg);
	Writeln('Plaintext    : ',vtx);
	Writeln('Key          : ',key);
	Writeln('Ciphertext   : ',ctx);
	Writeln('Plaintext    : ',ptx);
END.


```


```txt

Message      : Beware the Jabberwock! The jaws that bite, the claws that catch!
Plaintext    : BEWARETHEJABBERWOCKTHEJAWSTHATBITETHECLAWSTHATCATCH
Key          : VIGENERECIPHERVIGENERECIPHERVIGENERECIPHERVIGENEREC
Ciphertext   : WMCEEIKLGRPIFVMEUGXXYILILZXYVBHMGIKLGKAHAJOPGXPEKGJ
Plaintext    : BEWARETHEJABBERWOCKTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Perl


```perl
if( @ARGV != 3 ){
  printHelp();
}

  # translate to upper-case, remove anything else
map( (tr/a-z/A-Z/, s/[^A-Z]//g), @ARGV );

my $cipher_decipher = $ARGV[ 0 ];

if( $cipher_decipher !~ /ENC|DEC/ ){
  printHelp();  # user should say what to do
}

print "Key: " . (my $key = $ARGV[ 2 ]) . "\n";

if( $cipher_decipher =~ /ENC/ ){
  print "Plain-text: " . (my $plain = $ARGV[ 1 ]) . "\n";
  print "Encrypted: " . Vigenere( 1, $key, $plain ) . "\n";
}elsif( $cipher_decipher =~ /DEC/ ){
  print "Cipher-text: " . (my $cipher = $ARGV[ 1 ]) . "\n";
  print "Decrypted: " . Vigenere( -1, $key, $cipher ) . "\n";
}

sub printHelp{
  print "Usage:\n" .
        "Encrypting:\n perl cipher.pl ENC (plain text) (key)\n" .
        "Decrypting:\n perl cipher.pl DEC (cipher text) (key)\n";
  exit -1;
}

sub Vigenere{
  my ($direction, $key, $text) = @_;
  for( my $count = 0; $count < length $text; $count ++ ){
    $key_offset = $direction * ord substr( $key, $count % length $key, 1);
    $char_offset = ord substr( $text, $count, 1 );
    $cipher .= chr 65 + ((($char_offset % 26) + ($key_offset % 26)) % 26);
      # 65 is the ASCII character code for 'A'
  }
  return $cipher;
}
```


Demonstration:


```txt
$ perl cipher.pl ENC 'Beware the Jabberwock, my son! The jaws that bite, the claws that catch!' VIGENERECIPHER
Key: VIGENERECIPHER
Plain-text: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY

$ perl cipher.pl DEC WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY VIGENERECIPHER
Key: VIGENERECIPHER
Cipher-text: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

$ perl cipher.pl FOO WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY VIGENERECIPHER
Usage:
Encrypting:
 perl cipher.pl ENC (plain text) (key)
Decrypting:
 perl cipher.pl DEC (cipher text) (key)
```



## Perl 6

```perl6
sub s2v ($s) { $s.uc.comb(/ <[ A..Z ]> /)».ord »-» 65 }
sub v2s (@v) { (@v »%» 26 »+» 65)».chr.join }

sub blacken ($red, $key) { v2s(s2v($red) »+» s2v($key)) }
sub redden  ($blk, $key) { v2s(s2v($blk) »-» s2v($key)) }

my $red = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
my $key = "Vigenere Cipher!!!";

say $red;
say my $black = blacken($red, $key);
say redden($black, $key);
```

```txt
Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```

This is a natural job for hyperoperators, which can vectorize any operator.
For infix operators the pointy end indicates which side to "dwim", repeating
elements on that side until the other side runs out.  In particular, repeating
the key naturally falls out of this cyclic dwimmery, as does repeating the various constants to be applied with any of several operations to every element of the list. Factoring out the canonicalization and decanonicalization lets us see quite clearly that the only difference between encryption and decryptions is the sign of the vector addition/subtraction.  Since hyperops are inherently parallelizable, this algorithm might run well in your GPU.


## Phix


```Phix
enum type mode ENCRYPT = +1, DECRYPT = -1 end type

function Vigenere(string s, string key, mode m)
string res = ""
integer k = 1, ch
    s = upper(s)
    for i=1 to length(s) do
        ch = s[i]
        if ch>='A' and ch<='Z' then
            res &= 'A'+mod(ch+m*(key[k]+26),26)
            k = mod(k,length(key))+1
        end if
    end for
    return res
end function

constant key = "LEMON",
         s = "ATTACK AT DAWN",
         e = Vigenere(s,key,ENCRYPT),
         d = Vigenere(e,key,DECRYPT)

printf(1,"Original: %s\nEncrypted: %s\nDecrypted: %s\n",{s,e,d})
```

```txt

Original: ATTACK AT DAWN
Encrypted: LXFOPVEFRNHR
Decrypted: ATTACKATDAWN

```



## PHP

```PHP
<?php

$str = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
$key = "VIGENERECIPHER";

printf("Text: %s\n", $str);
printf("key:  %s\n", $key);

$cod = encipher($str, $key, true); printf("Code: %s\n", $cod);
$dec = encipher($cod, $key, false); printf("Back: %s\n", $dec);

function encipher($src, $key, $is_encode)
{
    $key = strtoupper($key);
    $src = strtoupper($src);
    $dest = '';

    /* strip out non-letters */
    for($i = 0; $i <= strlen($src); $i++) {
        $char = substr($src, $i, 1);
        if(ctype_upper($char)) {
            $dest .= $char;
        }
    }

    for($i = 0; $i <= strlen($dest); $i++) {
        $char = substr($dest, $i, 1);
        if(!ctype_upper($char)) {
            continue;
        }
        $dest = substr_replace($dest,
            chr (
                ord('A') +
                ($is_encode
                   ? ord($char) - ord('A') + ord($key[$i % strlen($key)]) - ord('A')
                   : ord($char) - ord($key[$i % strlen($key)]) + 26
                ) % 26
            )
        , $i, 1);
    }

    return $dest;
}

?>

```

```txt
Text: Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
key:  VIGENERECIPHER
Code: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Back: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## PicoLisp


```PicoLisp
(de vigenereKey (Str)
   (extract
      '((C)
         (when (>= "Z" (uppc C) "A")
            (- (char (uppc C)) 65) ) )
      (chop Str) ) )

(de vigenereEncrypt (Str Key)
   (pack
      (mapcar
         '((C K)
            (char (+ 65 (% (+ C K) 26))) )
         (vigenereKey Str)
         (apply circ (vigenereKey Key)) ) ) )

(de vigenereDecrypt (Str Key)
   (pack
      (mapcar
         '((C K)
            (char (+ 65 (% (+ 26 (- C K)) 26))) )
         (vigenereKey Str)
         (apply circ (vigenereKey Key)) ) ) )
```

Test:

```txt
: (vigenereEncrypt
   "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
   "VIGENERECIPHER" )
-> "WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY"

: (vigenereDecrypt @ "VIGENERECIPHER")
-> "BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH"
```



## PL/I


```PL/I

cypher: procedure options (main);      /* 21 September 2012 */
   declare t(26) character (26);
   declare (i, j, k, L) fixed binary;
   declare (original, encoded, coder) character (1000) varying initial ('');
   declare cypher character (30) varying;
   declare (co, ct, cc) character (1);

   /* Set up cypher table. */
   t(1) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
   do i = 2 to 26;
      t(i) =  substr(t(i-1), 2, 25) || substr(t(i-1), 1, 1);
   end;

   cypher = 'VIGILANCE';
   original = 'Meet me on Tuesday evening at seven.';
   put edit ('Message=', original) (a);
   original = uppercase(original);

   /* Create the cypher text, same length as original, or longer. */
   coder = repeat(cypher, length(original)/length(cypher));

   /* Encode the original message, character by character. */
   /* Non-alphabetic characters are ignored. */
   L = 0;
   do i = 1 to length(original);
      co = substr(original, i, 1);
      j = index(t(1), co);
      if j = 0 then iterate; /* Ignore non-alphabetic character */
      L = L + 1;
      ct = substr(coder, L, 1);
      k = index(t(1), ct);
      encoded = encoded || substr(t(j), k, 1);
   end;
   put skip data (encoded);

   /* DECODING. */
   put skip list ('Decoded=');
   do i = 1 to length(encoded);
      cc = substr(coder, i, 1);
      j = index(t(1), cc);
      k = index(t(j), substr(encoded, i, 1));
      put edit (substr(t(1), k, 1) ) (a(1));
   end;
end cypher;

```

```txt

Message=Meet me on Tuesday evening at seven.
ENCODED='HMKBXEBPXPMYLLYRXIIQTOLTFGZZV';
Decoded= MEETMEONTUESDAYEVENINGATSEVEN

```



## PowerShell


```Powershell
# Author: D. Cudnohufsky
function Get-VigenereCipher
{
    Param
    (
        [Parameter(Mandatory=$true)]
        [string] $Text,

        [Parameter(Mandatory=$true)]
        [string] $Key,

        [switch] $Decode
    )

    begin
    {
        $map = [char]'A'..[char]'Z'
    }

    process
    {
        $Key = $Key -replace '[^a-zA-Z]',''
        $Text = $Text -replace '[^a-zA-Z]',''

        $keyChars = $Key.toUpper().ToCharArray()
        $Chars = $Text.toUpper().ToCharArray()

        function encode
        {

            param
            (
                $Char,
                $keyChar,
                $Alpha = [char]'A'..[char]'Z'
            )

            $charIndex = $Alpha.IndexOf([int]$Char)
            $keyIndex = $Alpha.IndexOf([int]$keyChar)
            $NewIndex = ($charIndex + $KeyIndex) - $Alpha.Length
            $Alpha[$NewIndex]

        }

        function decode
        {

            param
            (
                $Char,
                $keyChar,
                $Alpha = [char]'A'..[char]'Z'
            )

            $charIndex = $Alpha.IndexOf([int]$Char)
            $keyIndex = $Alpha.IndexOf([int]$keyChar)
            $int = $charIndex - $keyIndex
            if ($int -lt 0) { $NewIndex = $int + $Alpha.Length }
            else { $NewIndex = $int }
            $Alpha[$NewIndex]
        }

        while ( $keyChars.Length -lt $Chars.Length )
        {
            $keyChars = $keyChars + $keyChars
        }

        for ( $i = 0; $i -lt $Chars.Length; $i++ )
        {

            if ( [int]$Chars[$i] -in $map -and [int]$keyChars[$i] -in $map )
            {
                if ($Decode) {$Chars[$i] = decode $Chars[$i] $keyChars[$i] $map}
                else {$Chars[$i] = encode $Chars[$i] $keyChars[$i] $map}

                $Chars[$i] = [char]$Chars[$i]
                [string]$OutText += $Chars[$i]
            }

        }

        $OutText
        $OutText = $null
    }
}
```

Usage examples:

```txt

Encode:
PS C:\> Get-VigenereCipher 'We attack at dawn.' 'lemon'
HIMHGLGWOGOEIB

Decode:
PS C:\> Get-VigenereCipher 'HIMHGLGWOGOEIB' 'lemon' -Decode
WEATTACKATDAWN

```



## PureBasic


```PureBasic
Procedure prepString(text.s, Array letters(1))
  ;convert characters to an ordinal (0-25) and remove non-alphabetic characters,
  ;returns dimension size of result array letters()
  Protected *letter.Character, index
  Dim letters(Len(text))
  text = UCase(text)
  *letter = @text
  While *letter\c
    Select *letter\c
      Case 'A' To 'Z'
        letters(index) = *letter\c - 65
        index + 1
    EndSelect
    *letter + SizeOf(Character)
  Wend
  If index > 0
    Redim letters(index - 1)
  EndIf
  ProcedureReturn index - 1
EndProcedure

Procedure.s VC_encrypt(text.s, keyText.s, reverse = 0)
  ;if reverse <> 0 then reverse the key (decrypt)
  Protected *letter.Character
  Dim text(0)
  Dim keyText(0)
  If prepString(text, text()) < 0 Or prepString(keyText, keyText()) < 0: ProcedureReturn: EndIf ;exit, nothing to work with

  Protected i, keyLength = ArraySize(keyText())
  If reverse
    For i = 0 To keyLength
      keyText(i) = 26 - keyText(i)
    Next
  EndIf

  Protected textLength = ArraySize(text()) ;zero-based length
  Protected result.s = Space(textLength + 1), *resultLetter.Character
  keyLength + 1 ;convert from zero-based to one-based count
  *resultLetter = @result
  For i = 0 To textLength
    *resultLetter\c = ((text(i) + keyText(i % keyLength)) % 26) + 65
    *resultLetter + SizeOf(Character)
  Next
  ProcedureReturn result
EndProcedure

Procedure.s VC_decrypt(cypherText.s, keyText.s)
  ProcedureReturn VC_encrypt(cypherText, keyText.s, 1)
EndProcedure

If OpenConsole()
  Define VignereCipher.s, plainText.s, encryptedText.s, decryptedText.s

  VignereCipher.s = "VIGNERECIPHER"
  plainText = "The quick brown fox jumped over the lazy dogs.": PrintN(RSet("Plain text = ", 17) + #DQUOTE$ + plainText + #DQUOTE$)
  encryptedText = VC_encrypt(plainText, VignereCipher): PrintN(RSet("Encrypted text = ", 17) + #DQUOTE$ + encryptedText + #DQUOTE$)
  decryptedText = VC_decrypt(encryptedText, VignereCipher): PrintN(RSet("Decrypted text = ", 17) + #DQUOTE$ + decryptedText + #DQUOTE$)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

```txt
    Plain text = "The quick brown fox jumped over the lazy dogs."
Encrypted text = "OPKDYZGMJGVAEAWDWYDTGLDCIIOPKYEQCFWVZ"
Decrypted text = "THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOGS"
```



## Python

```python
'''Vigenere encryption and decryption'''

from itertools import starmap, cycle


def encrypt(message, key):
    '''Vigenere encryption of message using key.'''

    # Converted to uppercase.
    # Non-alpha characters stripped out.
    message = filter(str.isalpha, message.upper())

    def enc(c, k):
        '''Single letter encryption.'''

        return chr(((ord(k) + ord(c) - 2 * ord('A')) % 26) + ord('A'))

    return ''.join(starmap(enc, zip(message, cycle(key))))


def decrypt(message, key):
    '''Vigenere decryption of message using key.'''

    def dec(c, k):
        '''Single letter decryption.'''

        return chr(((ord(c) - ord(k) - 2 * ord('A')) % 26) + ord('A'))

    return ''.join(starmap(dec, zip(message, cycle(key))))


def main():
    '''Demonstration'''

    text = 'Beware the Jabberwock, my son! The jaws that bite, ' + (
           'the claws that catch!'
    )
    key = 'VIGENERECIPHER'

    encr = encrypt(text, key)
    decr = decrypt(encr, key)

    print(text)
    print(encr)
    print(decr)


if __name__ == '__main__':
    main()

```

```txt
Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## R


```r
mod1 = function(v, n)
# mod1(1:20, 6)   =>   1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2
    ((v - 1) %% n) + 1

str2ints = function(s)
    as.integer(Filter(Negate(is.na),
        factor(levels = LETTERS, strsplit(toupper(s), "")[[1]])))

vigen = function(input, key, decrypt = F)
   {input = str2ints(input)
    key = rep(str2ints(key), len = length(input)) - 1
    paste(collapse = "", LETTERS[
        mod1(input + (if (decrypt) -1 else 1)*key, length(LETTERS))])}

message(vigen("Beware the Jabberwock, my son! The jaws that bite, the claws that catch!", "vigenerecipher"))
  # WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
message(vigen("WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY", "vigenerecipher", decrypt = T))
  # BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH
```



## Racket


```racket

#lang racket
(define chr integer->char)
(define ord char->integer)

(define (encrypt msg key)
  (define cleaned
    (list->string
     (for/list ([c (string-upcase msg)]
                #:when (char-alphabetic? c)) c)))
  (list->string
   (for/list ([c cleaned] [k (in-cycle key)])
     (chr (+ (modulo (+ (ord c) (ord k)) 26) (ord #\A))))))

(define (decrypt msg key)
  (list->string
   (for/list ([c msg] [k (in-cycle key)])
     (chr (+ (modulo (- (ord c) (ord k)) 26) (ord #\A))))))

(decrypt (encrypt "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
                  "VIGENERECIPHER")
         "VIGENERECIPHER")

```

```racket

"BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH"

```


## Red

note: this program is much longer than it needed to be - because i couldn't resist
to add some more features to make it actually "useful" :-)  So not only can u encrypt any character (because the crypted message will be base 64 encoded), but it also includes a Gui.
the Gui window has buttons to access the clipboard too - so u can get the original text from clipboard and put the crypted message back again. To execute it,simply  download the latest red.exe (about 1,1 MB size! ) from red-lang.org. This program can also be compiled to an .exe (+ red runtime.dll ) by simply execute
```txt
red.exe -c vign1.red
```
 or
```txt
red.exe -r vign1.red
```
 which creates a single .exe file whithout the need for any .dll  . should be working on windows , linux ( under wine ) and mac OS.

```Red
Red [needs: 'view]

CRLF: copy "^M^/" ;; constant for 0D 0A line feed
;;------------------------------------
crypt: func ["function to en- or decrypt message from textarea tx1"
    /decrypt "decrypting switch/refinement" ][
;;------------------------------------

;; when decrypting we have to remove the superflous newlines
;; and undo the base64 encoding first ...
txt: either decrypt [     ;; message to en- or decrypt
   s: copy tx1/text
   ;; newline could be single 0a byte or crlf sequence when copied from clipboard...
   debase replace/all s either find s CRLF  [CRLF ] [ newline ] ""
] [
    tx1/text ;; plaintext message
]

txt: to-binary txt          ;; handle message as binary
key: to-binary key1/text  ;; handle key also as binary

bin: copy either decrypt [  ""  ][ #{} ] ;; buffer for output

code: copy #{} ;; temp field to collect utf8 bytes when decrypting

;; loop over  length of binary! message ...
repeat pos length? txt [
 k: to-integer key/(1 + modulo pos length? key)   ;; get corresponding key byte
 c: to-integer txt/:pos       ;; get integer value from message byte at position pos

 either decrypt [                         ;; decrypting ?
    c:  modulo ( 256 + c - k ) 256      ;; compute original byte value
    case [
      ;; byte starting with 11.... ( >= 192 dec ) is utf8 startbyte
      ;; byte starting with 10... ( >= 128 dec) is utf8 follow up byte , below is single ascii byte
        ( c >= 192 ) or ( c < 128 )  [    ;; starting utf8 sequence byte or below 128 normal ascii ?
            ;; append last code to buffer, maybe normal ascii or utf8 sequence...
            if not empty? code [ append bin to-char code ]  ;; save previous code first
            code: append copy #{} c     ;; start new code
        ]
        true [ append code c ]  ;; otherwise utf8 follow up byte, append to startbyte
      ]
  ][
    append bin modulo ( c + k ) 256   ;; encrypting , simply collect binary bytes
  ]
] ;; close repeat loop

either decrypt [                ;; collect utf-8 characters
   append bin to-char code    ;; append last code
  tx2/text: to-string bin      ;; create valid utf8 string when decrypting
][  ;; base64 encoding of crypted binary to get readable text string...
    s: enbase copy  bin      ;; base 64 is default
    while [40 < length? s ] [ ;; insert newlines for better "readability"
        s: skip s either head? s [40][41]   ;; ... every 40 characters
        insert s newline
    ]
    tx2/text: head s    ;; reset s pointing to head again
  ]
]
;----------------------------------------------------------
; start of program
;----------------------------------------------------------
view layout [title  "vigenere cyphre"	;Define nice GUI :- )
;----------------------------------------------------------
  backdrop silver      ;; define window background colour
  text "message:" pad 99x1 button "get-clip" [tx1/text: read-clipboard]
  ;; code in brackets will be executed, when button is clicked:
  button "clear" [tx1/text: copy "" ] return
  tx1: area  330x80 "" return
  text 25x20  "Key:" key1:  field 290x20 "secretkey" return
  button "crypt" [crypt ]  button "decrypt" [crypt/decrypt ]
  button "swap" [tx1/text: copy tx2/text tx2/text: copy "" ] return
  text "de-/encrypted message:" pad 50x1  button "copy clip"  [ write-clipboard tx2/text]
  button "clear" [tx2/text: copy "" ] return
  tx2: area  330x100 return
  pad 270x1 button "Quit " [quit]
]

```

the message
```txt
Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
```
 with key "VIGENERECIPHER" translates to
```txt
i6y8r7e3ZbextWiPs7irrLfFtLWwb2m9wWXFxbdo
ZaKtt2Wtqse7Zca+qrtlsK7Gqm9pxLCqcrm1qLzBZcatpL1wq6bGubFo
```

decrypting returns the original message
```txt
Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
```



## REXX


### uppercase text only


```rexx
/*REXX program  encrypts  (and displays)  uppercased text  using  the  Vigenère  cypher.*/
@.1 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
L=length(@.1)
                         do j=2  to L;                        jm=j-1;    q=@.jm
                         @.j=substr(q, 2, L - 1)left(q, 1)
                         end   /*j*/

cypher = space('WHOOP DE DOO    NO BIG DEAL HERE OR THERE', 0)
oMsg   = 'People solve problems by trial and error; judgement helps pick the trial.'
oMsgU  = oMsg;    upper oMsgU
cypher_= copies(cypher, length(oMsg) % length(cypher) )
                                say '   original text ='   oMsg
   xMsg= Ncypher(oMsgU);        say '   cyphered text ='   xMsg
   bMsg= Dcypher(xMsg) ;        say 're-cyphered text ='   bMsg
exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
Ncypher:  parse arg x;    nMsg=;       #=1      /*unsupported char? ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓*/
              do i=1  for length(x);   j=pos(substr(x,i,1), @.1);   if j==0  then iterate
              nMsg=nMsg || substr(@.j, pos( substr( cypher_, #, 1), @.1), 1);     #=#+1
              end   /*j*/
          return nMsg
/*──────────────────────────────────────────────────────────────────────────────────────*/
Dcypher:  parse arg x;    dMsg=
              do i=1  for length(x);   j=pos(substr(cypher_, i, 1),  @.1)
              dMsg=dMsg || substr(@.1, pos( substr(x, i, 1), @.j),   1  )
              end   /*j*/
          return dMsg
```

```txt

   original text = People solve problems by trial and error; judgement helps pick the trial.
   cyphered text = LLCDAHWRZJRDSWHOIMDICKVWREHRUINYCFYXHJSARBUPKOTSAPGBXVVMYMRP
re-cyphered text = PEOPLESOLVEPROBLEMSBYTRIALANDERRORJUDGEMENTHELPSPICKTHETRIAL

```



### supports most characters

This version supports all characters on the   IBM Model M   keyboard, including blanks,   but any other

characters can be added as long as they're viewable.

Additional characters can be added by simply appending them to the   <big>'''@.1'''</big>   variable.

```rexx
/*REXX program  encrypts  (and displays)  most text  using  the  Vigenère  cypher.      */
@abc= 'abcdefghijklmnopqrstuvwxyz';       @abcU=@abc;    upper @abcU
@.1 = @abcU || @abc'0123456789~`!@#$%^&*()_-+={}|[]\:;<>?,./" '''
L=length(@.1)
                         do j=2  to length(@.1);                jm=j - 1;          q=@.jm
                         @.j=substr(q, 2, L - 1)left(q, 1)
                         end   /*j*/

cypher = space('WHOOP DE DOO    NO BIG DEAL HERE OR THERE', 0)
oMsg   = 'Making things easy is just knowing the shortcuts. --- Gerard J. Schildberger'
cypher_= copies(cypher, length(oMsg) % length(cypher) )
                                say '   original text ='   oMsg
   xMsg= Ncypher(oMsg);         say '   cyphered text ='   xMsg
   bMsg= Dcypher(xMsg);         say 're-cyphered text ='   bMsg
exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
Ncypher:  parse arg x;    nMsg=;       #=1      /*unsupported char? ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓*/
              do i=1  for length(x);   j=pos(substr(x,i,1), @.1);   if j==0  then iterate
              nMsg=nMsg || substr(@.j, pos( substr( cypher_, #, 1), @.1), 1);     #=# + 1
              end   /*j*/
          return nMsg
/*──────────────────────────────────────────────────────────────────────────────────────*/
Dcypher:  parse arg x;    dMsg=
              do i=1  for length(x);   j=pos(substr(cypher_, i, 1),  @.1)
              dMsg=dMsg || substr(@.1, pos( substr(x, i, 1), @.j),   1  )
              end   /*j*/
          return dMsg
```

```txt

   original text = Making things easy is just knowing the shortcuts. --- Gerard J. Schildberger
   cyphered text = ihyw2jCwvw0utGkdwyJpwPn89!Fo4s&p1uNwlhM6u2s1ixxsGF}"}MXxye8h/H?/QafgjbZcpecp
re-cyphered text = Making things easy is just knowing the shortcuts. --- Gerard J. Schildberger

```



## Ring


```ring

# Project : Vigenère cipher

key = "LEMON"
plaintext = "ATTACK AT DAWN"
ciphertext = encrypt(plaintext, key)
see "key = "+ key + nl
see "plaintext  = " + plaintext + nl
see "ciphertext = " + ciphertext + nl
see "decrypted  = " + decrypt(ciphertext, key) + nl


func encrypt(plain, key)
        o = ""
        k = 0
        plain = fnupper(plain)
        key = fnupper(key)
        for i = 1 to len(plain)
             n = ascii(plain[i])
             if n >= 65 and n <= 90
                o = o + char(65 + (n + ascii(key[k+1])) % 26)
                k = (k + 1) % len(key)
             ok
        next
        return o

func decrypt(cipher, key)
        o = ""
        k = 0
        cipher = fnupper(cipher)
        key = fnupper(key)
        for i = 1 to len(cipher)
             n = ascii(cipher[i])
             o = o + char(65 + (n + 26 - ascii(key[k+1])) % 26)
             k = (k + 1) % len(key)
        next
        return o

func fnupper(a)
        for aa = 1 to len(a)
             c = ascii(a[aa])
            if c >= 97 and c <= 122
               a[aa] = char(c-32)
           ok
        next
        return a

```

Output:

```txt

key = LEMON
plaintext  = ATTACK AT DAWN
ciphertext = LXFOPVEFRNHR
decrypted  = ATTACKATDAWN

```



## Ruby


```Ruby
module VigenereCipher

  BASE = 'A'.ord
  SIZE = 'Z'.ord - BASE + 1

  def encrypt(text, key)
    crypt(text, key, :+)
  end

  def decrypt(text, key)
    crypt(text, key, :-)
  end

  def crypt(text, key, dir)
    text = text.upcase.gsub(/[^A-Z]/, '')
    key_iterator = key.upcase.gsub(/[^A-Z]/, '').chars.map{|c| c.ord - BASE}.cycle
    text.each_char.inject('') do |ciphertext, char|
      offset = key_iterator.next
      ciphertext << ((char.ord - BASE).send(dir, offset) % SIZE + BASE).chr
    end
  end

end
```


Demonstration:


```Ruby
include VigenereCipher

plaintext = 'Beware the Jabberwock, my son! The jaws that bite, the claws that catch!'
key = 'Vigenere cipher'
ciphertext = VigenereCipher.encrypt(plaintext, key)
recovered  = VigenereCipher.decrypt(ciphertext, key)

puts "Original: #{plaintext}"
puts "Encrypted: #{ciphertext}"
puts "Decrypted: #{recovered}"
```


```txt

Original: Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Rust


```rust
use std::ascii::AsciiExt;

static A: u8 = 'A' as u8;

fn uppercase_and_filter(input: &str) -> Vec<u8> {
    let alphabet = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut result = Vec::new();

    for c in input.chars() {
        // Ignore anything that is not in our short list of chars. We can then safely cast to u8.
        if alphabet.iter().any(|&x| x as char == c) {
            result.push(c.to_ascii_uppercase() as u8);
        }
    }

    return result;
}

fn vigenere(key: &str, text: &str, is_encoding: bool) -> String {

    let key_bytes = uppercase_and_filter(key);
    let text_bytes = uppercase_and_filter(text);

    let mut result_bytes = Vec::new();

    for (i, c) in text_bytes.iter().enumerate() {
        let c2 = if is_encoding {
            (c + key_bytes[i % key_bytes.len()] - 2 * A) % 26 + A
        } else {
            (c + 26 - key_bytes[i % key_bytes.len()]) % 26 + A
        };
        result_bytes.push(c2);
    }

    String::from_utf8(result_bytes).unwrap()
}

fn main() {
    let text = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
    let key = "VIGENERECIPHER";

    println!("Text: {}", text);
    println!("Key:  {}", key);

    let encoded = vigenere(key, text, true);
    println!("Code: {}", encoded);
    let decoded = vigenere(key, &encoded, false);
    println!("Back: {}", decoded);
}
```



## Scala

Valid characters for messages: A through Z, zero, 1 to 9, and full-stop (.)

```scala

object Vigenere {
  def encrypt(msg: String, key: String) : String = {
    var result: String = ""
    var j = 0

    for (i <- 0 to msg.length - 1) {
      val c = msg.charAt(i)
      if (c >= 'A' && c <= 'Z') {
        result += ((c + key.charAt(j) - 2 * 'A') % 26 + 'A').toChar
        j = (j + 1) % key.length
      }
    }

    return result
  }

  def decrypt(msg: String, key: String) : String = {
    var result: String = ""
    var j = 0

    for (i <- 0 to msg.length - 1) {
      val c = msg.charAt(i)
      if (c >= 'A' && c <= 'Z') {
        result += ((c - key.charAt(j) + 26) % 26 + 'A').toChar
        j = (j + 1) % key.length
      }
    }

    return result
  }
}

println("Encrypt text ABC => " + Vigenere.encrypt("ABC", "KEY"))
println("Decrypt text KFA => " + Vigenere.decrypt("KFA", "KEY"))

```


```txt
scala> Encrypt text ABC => KFA
scala> Decrypt text KFA => ABC
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: vigenereCipher (in string: source, in var string: keyword) is func
  result
    var string: dest is "";
  local
    var char: ch is ' ';
    var integer: index is 1;
    var integer: shift is 0;
  begin
    keyword := upper(keyword);
    for ch range source do
      if ch in {'A' .. 'Z'} | {'a' .. 'z'} then
        shift := ord(keyword[succ(pred(index) rem length(keyword))]) - ord('A');
        dest &:= chr(ord('A') + (ord(upper(ch)) - ord('A') + shift) rem 26);
        incr(index);
      end if;
    end for;
  end func;

const func string: vigenereDecipher (in string: source, in var string: keyword) is func
  result
    var string: dest is "";
  local
    var char: ch is ' ';
    var integer: index is 0;
    var integer: shift is 0;
  begin
    keyword := upper(keyword);
    for ch key index range source do
      if ch in {'A' .. 'Z'} | {'a' .. 'z'} then
        shift := ord(keyword[succ(pred(index) rem length(keyword))]) - ord('A');
        dest &:= chr(ord('A') + (ord(upper(ch)) - ord('A') - shift) mod 26);
      end if;
    end for;
  end func;

const proc: main is func
  local
    const string: input is "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
    const string: keyword is "VIGENERECIPHER";
    var string: encrypted is "";
    var string: decrypted is "";
  begin
    writeln("Input:     " <& input);
    writeln("key:       " <& keyword);
    encrypted := vigenereCipher(input, keyword);
    writeln("Encrypted: " <& encrypted);
    decrypted := vigenereDecipher(encrypted, keyword);
    writeln("Decrypted: " <& decrypted);
  end func;
```


```txt

Input:     Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
key:       VIGENERECIPHER
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Sidef

```ruby
func s2v(s) { s.uc.scan(/[A-Z]/).map{.ord} »-» 65 }
func v2s(v) { v »%» 26 »+» 65 -> map{.chr}.join   }
 
func blacken (red, key) { v2s(s2v(red) »+« s2v(key)) }
func redden  (blk, key) { v2s(s2v(blk) »-« s2v(key)) }
 
var red = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
var key = "Vigenere Cipher!!!"
 
say red
say (var black = blacken(red, key))
say redden(black, key)
```

```txt

Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## Smalltalk

in the following code, the cypher should consist of upper-case characters only. If that is not guaranteed, apply prep to it before passing it to encrypt/decrypt..
```smalltalk

prep := [:s | s select:[:ch | ch isLetter] thenCollect:[:ch | ch asUppercase]].
encrypt := [:s :cypher | (prep value:s) keysAndValuesCollect:[:i :ch | ch rot:((cypher at:((i-1)\\key size+1))-$A) ]].
decrypt := [:s :cypher | (prep value:s) keysAndValuesCollect:[:i :ch | ch rot:26-((cypher at:((i-1)\\key size+1))-$A) ]].

```

Test:

```smalltalk

plain := 'Beware the Jabberwock, my son! The jaws that bite, the claws that catch!'.
cypher := 'VIGENERECIPHER'.
crypted := encrypt value:plain value:cypher.
plain2 := decrypt value:crypted value:cypher.

crypted -> 'WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY'
plain2  -> 'BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH'

```



## Swift


Can support a larger range of characters, if desired


```swift
public func convertToUnicodeScalars(
  str: String,
  minChar: UInt32,
  maxChar: UInt32
) -> [UInt32] {
  var scalars = [UInt32]()

  for scalar in str.unicodeScalars {
    let val = scalar.value

    guard val >= minChar && val <= maxChar else {
      continue
    }

    scalars.append(val)
  }

  return scalars
}

public struct Vigenere {
  private let keyScalars: [UInt32]
  private let smallestScalar: UInt32
  private let largestScalar: UInt32
  private let sizeAlphabet: UInt32

  public init?(key: String, smallestCharacter: Character = "A", largestCharacter:  Character = "Z") {
    let smallScalars = smallestCharacter.unicodeScalars
    let largeScalars = largestCharacter.unicodeScalars

    guard smallScalars.count == 1, largeScalars.count == 1 else {
      return nil
    }

    self.smallestScalar = smallScalars.first!.value
    self.largestScalar = largeScalars.first!.value
    self.sizeAlphabet = (largestScalar - smallestScalar) + 1

    let scalars = convertToUnicodeScalars(str: key, minChar: smallestScalar, maxChar: largestScalar)

    guard !scalars.isEmpty else {
      return nil
    }

    self.keyScalars = scalars

  }

  public func decrypt(_ str: String) -> String? {
    let txtBytes = convertToUnicodeScalars(str: str, minChar: smallestScalar, maxChar: largestScalar)

    guard !txtBytes.isEmpty else {
      return nil
    }

    var res = ""

    for (i, c) in txtBytes.enumerated() where c >= smallestScalar && c <= largestScalar {
      guard let char =
        UnicodeScalar((c &+ sizeAlphabet &- keyScalars[i % keyScalars.count]) % sizeAlphabet &+ smallestScalar)
      else {
        return nil
      }

      res += String(char)
    }

    return res
  }

  public func encrypt(_ str: String) -> String? {
    let txtBytes = convertToUnicodeScalars(str: str, minChar: smallestScalar, maxChar: largestScalar)

    guard !txtBytes.isEmpty else {
      return nil
    }

    var res = ""

    for (i, c) in txtBytes.enumerated() where c >= smallestScalar && c <= largestScalar {
      guard let char =
        UnicodeScalar((c &+ keyScalars[i % keyScalars.count] &- 2 &* smallestScalar) % sizeAlphabet &+ smallestScalar)
      else {
        return nil
      }

      res += String(char)
    }

    return res
  }
}

let text = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
let key = "VIGENERECIPHER";
let cipher = Vigenere(key: key)!

print("Key: \(key)")
print("Plain Text: \(text)")

let encoded = cipher.encrypt(text.uppercased())!

print("Cipher Text: \(encoded)")

let decoded = cipher.decrypt(encoded)!

print("Decoded: \(decoded)")

print("\nLarger set:")

let key2 = "Vigenère cipher"
let text2 = "This is a ünicode string 😃"

let cipher2 = Vigenere(key: key2, smallestCharacter: " ", largestCharacter: "🛹")!

print("Key: \(key2)")
print("Plain Text: \(text2)")

let encoded2 = cipher2.encrypt(text2)!

print("Cipher Text: \(encoded2)")

let decoded2 = cipher2.decrypt(encoded2)!

print("Decoded: \(decoded2)")
```


```txt
Key: VIGENERECIPHER
Plain Text: Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Cipher Text: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decoded: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

Larger set:
Key: Vigenère cipher
Plain Text: This is a ünicode string 😃
Cipher Text: ±°¸nıÅeacŅ¾±¨Á®g¸Âĺ»³gc🙌
Decoded: This is a ünicode string 😃
```



## Tcl

```tcl
package require Tcl 8.6

oo::class create Vigenere {
    variable key
    constructor {protoKey} {
	foreach c [split $protoKey ""] {
	    if {[regexp {[A-Za-z]} $c]} {
		lappend key [scan [string toupper $c] %c]
	    }
	}
    }

    method encrypt {text} {
	set out ""
	set j 0
	foreach c [split $text ""] {
	    if {[regexp {[^a-zA-Z]} $c]} continue
	    scan [string toupper $c] %c c
	    append out [format %c [expr {($c+[lindex $key $j]-130)%26+65}]]
	    set j [expr {($j+1) % [llength $key]}]
	}
	return $out
    }

    method decrypt {text} {
	set out ""
	set j 0
	foreach c [split $text ""] {
	    if {[regexp {[^A-Z]} $c]} continue
	    scan $c %c c
	    append out [format %c [expr {($c-[lindex $key $j]+26)%26+65}]]
	    set j [expr {($j+1) % [llength $key]}]
	}
	return $out
    }
}
```

Demonstrating:

```tcl
set cypher [Vigenere new "Vigenere Cipher"]
set original "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
set encrypted [$cypher encrypt $original]
set decrypted [$cypher decrypt $encrypted]
puts $original
puts "Encrypted: $encrypted"
puts "Decrypted: $decrypted"
```

```txt

Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## TXR



```txr
@(next :args)
@(do
   (defun vig-op (plus-or-minus)
     (op + #\A [mod [plus-or-minus (- @1 #\A) (- @2 #\A)] 26]))

   (defun vig (msg key encrypt)
     (mapcar (vig-op [if encrypt + -]) msg (repeat key))))
@(coll)@{key /[A-Za-z]/}@(end)
@(coll)@{msg /[A-Za-z]/}@(end)
@(cat key "")
@(filter :upcase key)
@(cat msg "")
@(filter :upcase msg)
@(bind encoded @(vig msg key t))
@(bind decoded @(vig msg key nil))
@(bind check @(vig encoded key nil))
@(output)
text:  @msg
key:   @key
enc:   @encoded
dec:   @decoded
check: @check
@(end)
```


Here, the TXR pattern language is used to scan letters out of two arguments,
and convert them to upper case.
The embedded TXR Lisp dialect handles the Vigenère logic,
in just a few lines of code.

Lisp programmers may do a "double take" at what is going on here: yes <code>mapcar</code> can operate on strings and return strings in TXR Lisp. <code>(repeat key)</code> produces an infinite lazy list; but that's okay because <code>mapcar</code> stops after the shortest input runs out of items.

Run:


```txt
$ txr vigenere.txr 'vigenere cipher' 'Beware the Jabberwock... The jaws that... the claws that catch!'
text:  BEWARETHEJABBERWOCKTHEJAWSTHATTHECLAWSTHATCATCH
key:   VIGENERECIPHER
enc:   WMCEEIKLGRPIFVMEUGXXYILILZXYVBZLRGCEYAIOEKXIZGU
dec:   GWQWEACDCBLUXNWOIYXPQAHSHLPQFLNDRYUWUKEAWCHSNYU
check: BEWARETHEJABBERWOCKTHEJAWSTHATTHECLAWSTHATCATCH
```



## TypeScript


```JavaScript
class Vigenere {

    key: string

    /** Create new cipher based on key */
    constructor(key: string) {
        this.key = Vigenere.formatText(key)
    }

    /** Enrypt a given text using key */
    encrypt(plainText: string): string {
        return Array.prototype.map.call(Vigenere.formatText(plainText), (letter: string, index: number): string => {
            return String.fromCharCode((letter.charCodeAt(0) + this.key.charCodeAt(index % this.key.length) - 130) % 26 + 65)
        }).join('')
    }

    /** Decrypt ciphertext based on key */
    decrypt(cipherText: string): string {
        return Array.prototype.map.call(Vigenere.formatText(cipherText), (letter: string, index: number): string => {
            return String.fromCharCode((letter.charCodeAt(0) - this.key.charCodeAt(index % this.key.length) + 26) % 26 + 65)
        }).join('')
    }

    /** Converts to uppercase and removes non characters */
    private static formatText(text: string): string {
        return text.toUpperCase().replace(/[^A-Z]/g, "")
    }

}

/** Example usage */
(() => {
    let original: string = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book."

    console.log(`Original: ${original}`)

    let vig: Vigenere = new Vigenere("vigenere")

    let encoded: string = vig.encrypt(original)

    console.log(`After encryption: ${encoded}`)

    let back: string = vig.decrypt(encoded)

    console.log(`After decryption: ${back}`)

})()

```



## VBA


```vb
Option Explicit

Sub test()
Dim Encryp As String
   Encryp = Vigenere("Beware the Jabberwock, my son! The jaws that bite, the claws that catch!", "vigenerecipher", True)
   Debug.Print "Encrypt:= """ & Encryp & """"
   Debug.Print "Decrypt:= """ & Vigenere(Encryp, "vigenerecipher", False) & """"
End Sub

Private Function Vigenere(sWord As String, sKey As String, Enc As Boolean) As String
Dim bw() As Byte, bk() As Byte, i As Long, c As Long
Const sW As String = "ÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ"
Const sWo As String = "AAAAACEEEEIIIINOOOOOUUUUY"
Const A As Long = 65
Const N As Long = 26

   c = Len(sKey)
   i = Len(sWord)
   sKey = Left(IIf(c < i, StrRept(sKey, (i / c) + 1), sKey), i)
   sKey = StrConv(sKey, vbUpperCase)         'Upper case
   sWord = StrConv(sWord, vbUpperCase)
   sKey = StrReplace(sKey, sW, sWo)          'Replace accented characters
   sWord = StrReplace(sWord, sW, sWo)
   sKey = RemoveChars(sKey)                  'Remove characters (numerics, spaces, comas, ...)
   sWord = RemoveChars(sWord)
   bk = CharToAscii(sKey)                     'To work with Bytes instead of String
   bw = CharToAscii(sWord)
   For i = LBound(bw) To UBound(bw)
      Vigenere = Vigenere & Chr((IIf(Enc, ((bw(i) - A) + (bk(i) - A)), ((bw(i) - A) - (bk(i) - A)) + N) Mod N) + A)
   Next i
End Function

Private Function StrRept(s As String, N As Long) As String
Dim j As Long, c As String
   For j = 1 To N
      c = c & s
   Next
   StrRept = c
End Function

Private Function StrReplace(s As String, What As String, By As String) As String
Dim t() As String, u() As String, i As Long
   t = SplitString(What)
   u = SplitString(By)
   StrReplace = s
   For i = LBound(t) To UBound(t)
      StrReplace = Replace(StrReplace, t(i), u(i))
   Next i
End Function

Private Function SplitString(s As String) As String()
   SplitString = Split(StrConv(s, vbUnicode), Chr(0))
End Function

Private Function RemoveChars(str As String) As String
Dim b() As Byte, i As Long
   b = CharToAscii(str)
   For i = LBound(b) To UBound(b)
      If b(i) >= 65 And b(i) <= 90 Then RemoveChars = RemoveChars & Chr(b(i))
   Next i
End Function

Private Function CharToAscii(s As String) As Byte()
   CharToAscii = StrConv(s, vbFromUnicode)
End Function
```


```txt
Encrypt:= "WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY"
Decrypt:= "BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH"
```



## VBScript

```vb

Function Encrypt(text,key)
	text = OnlyCaps(text)
	key = OnlyCaps(key)
	j = 1
	For i = 1 To Len(text)
		ms = Mid(text,i,1)
		m = Asc(ms) - Asc("A")
		ks = Mid(key,j,1)
		k = Asc(ks) - Asc("A")
		j = (j Mod Len(key)) + 1
		c = (m + k) Mod 26
		c = Chr(Asc("A")+c)
		Encrypt = Encrypt & c
	Next
End Function

Function Decrypt(text,key)
	key = OnlyCaps(key)
	j = 1
	For i = 1 To Len(text)
		ms = Mid(text,i,1)
		m = Asc(ms) - Asc("A")
		ks = Mid(key,j,1)
		k = Asc(ks) - Asc("A")
		j = (j Mod Len(key)) + 1
		c = (m - k + 26) Mod 26
		c = Chr(Asc("A")+c)
		Decrypt = Decrypt & c
	Next
End Function

Function OnlyCaps(s)
	For i = 1 To Len(s)
		char = UCase(Mid(s,i,1))
		If Asc(char) >= 65 And Asc(char) <= 90 Then
			OnlyCaps = OnlyCaps & char
		End If
	Next
End Function

'testing the functions
orig_text = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
orig_key = "vigenerecipher"
WScript.StdOut.WriteLine "Original: " & orig_text
WScript.StdOut.WriteLine "Key: " & orig_key
WScript.StdOut.WriteLine "Encrypted: " & Encrypt(orig_text,orig_key)
WScript.StdOut.WriteLine "Decrypted: " & Decrypt(Encrypt(orig_text,orig_key),orig_key)

```


```txt

Original: Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Key: vigenerecipher
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```


An alternate implementation using RegExp to filter the input

```vb

'vigenere cypher
option explicit
const asca =65  'ascii(a)

function filter(s)
    with new regexp
      .pattern="[^A-Z]"
      .global=1
      filter=.replace(ucase(s),"")
     end with
end function

function vigenere (s,k,sign)
dim s1,i,a,b
  for i=0 to len(s)-1
    a=asc(mid(s,i+1,1))-asca
    b=sign * (asc(mid(k,(i mod len(k))+1,1))-asca)
    s1=s1 & chr(((a+b+26) mod 26) +asca)
  next
  vigenere=s1
end function

function encrypt(s,k): encrypt=vigenere(s,k,1) :end function
function decrypt(s,k): decrypt=vigenere(s,k,-1) :end function

'test--------------------------
dim plaintext,filtered,key,encoded
key="VIGENERECYPHER"
plaintext = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!"
filtered= filter(plaintext)
wscript.echo filtered
encoded=encrypt(filtered,key)
wscript.echo encoded
wscript.echo decrypt(encoded,key)


```



## Vedit macro language

Encrypts and then decrypts one line of text on current edit buffer,
starting from cursor location.
The user enters the keyword (upper or lower case).

```vedit
Get_Input(10, "Key: ", STATLINE+NOCR)		// @10 = key
Reg_Copy_Block(11, Cur_Pos, EOL_Pos)		// @11 = copy of original text
EOL Ins_Newline
Ins_Text("Key = ") Reg_Ins(10) Ins_Newline

// Prepare the key into numeric registers #130..:
Buf_Switch(Buf_Free)
Reg_Ins(10)
Case_Upper_Block(0, Cur_Pos)
BOF
#2 = Reg_Size(10)				// #2 = key length
for (#3=130; #3 < 130+#2; #3++) {
    #@3 = Cur_Char
    Char(1)
}
Buf_Quit(OK)

Ins_Text("Encrypted: ")
#4 = Cur_Pos
Reg_Ins(11)					// copy of original text
Replace_Block("|!|A", "", #4, EOL_Pos, BEGIN+ALL+NOERR) // remove non-alpha chars
Case_Upper_Block(#4, EOL_Pos)			// convert to upper case
Goto_Pos(#4)
#1 = 1; Call("ENCRYPT_DECRYPT")			// Encrypt the line
Reg_Copy_Block(11, #4, Cur_Pos)			// Copy encrypted text text to next line
Ins_Newline
Ins_Text("Decrypted: ")
Reg_Ins(11, BEGIN)
#1 = -1; Call("ENCRYPT_DECRYPT")		// Decrypt the line

Return

// Encrypt or decrypt text on current line in-place, starting from cursor position.
//   in: #1 = direction (1=encrypt, -1=decrypt)
//       #2 = key length,  #130...#189 = the key
//
:ENCRYPT_DECRYPT:
    Num_Push(6,9)
    #6 = 0
    While (!At_EOL) {
       #7 = #6+130				// pointer to key array
       #8 = #@7					// get key character
       #9 = (Cur_Char + #8*#1 + 26) % 26 + 'A'	// decrypt/encrypt
       Ins_Char(#9, OVERWRITE)			// write the converted char
       #6 = (#6+1) % #2
    }
    Num_Pop(6,9)
Return
```


```txt

Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
Key = vigenerecipher
Encrypted: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Decrypted: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```



## XPL0

The KEYWORD must be UPPERCASE. Precede it with a minus sign to decrypt a file.
Usage: vigenere KEYWORD <infile.txt >outfile.xxx


```XPL0
code ChIn=7, ChOut=8;
int  Neg, C, Len, I, Key;
char KeyWord(80);
[Neg:= false;                                   \skip to KEYWORD
repeat C:= ChIn(8);  if C=^- then Neg:= true;  until C>=^A & C<=^Z;
Len:= 0;                                        \read in KEYWORD
repeat KeyWord(Len):= C-^A;  Len:= Len+1;  C:= ChIn(8);  until C<^A ! C>^Z;
I:= 0;                                          \initialize cycling index
repeat  C:= ChIn(1);
        if C>=^a & C<=^z then C:= C-$20;        \capitalize
        if C>=^A & C<=^Z then                   \discard non-alphas
            [Key:= KeyWord(I);  I:= I+1;  if I>=Len then I:= 0;
            if Neg then Key:= -Key;             \decrypting?
            C:= C+Key;
            if C>^Z then C:= C-26
            else if C<^A then C:= C+26;
            ChOut(0, C);
            ];
until   C=$1A;  \EOF
ChOut(0, $1A);  \encrypted file must end with EOF otherwise the decode will hang
]
```


```txt

KEYWORD = ACE
infile  = Pack my box with five dozen liquor jugs.
outfile = PCGKOCBQBWKXHHMVGHOBINNMQWSRLYGU
KEYWORD = -ACE
outfile = PACKMYBOXWITHFIVEDOZENLIQUORJUGS

```



## zkl

```zkl
fcn encipher(src,key,is_encode){
   upperCase:=["A".."Z"].pump(String);
   src=src.toUpper().inCommon(upperCase);  // only uppercase
   key=key.toUpper().inCommon(upperCase).pump(List,"toAsc");

   const A="A".toAsc();
   klen:=Walker.cycle(key.len());  // 0,1,2,3,..,keyLen-1,0,1,2,3, ...
   src.pump(String,'wrap(c){ i:=klen.next(); c=c.toAsc();
      (A + ( if(is_encode) c - A + key[i] - A;
      	     else	   c - key[i] + 26 ) % 26).toChar()
   });
}
```


```zkl
str := "Beware the Jabberwock, my son! The jaws that bite, "
                    "the claws that catch!";
key := "Vigenere Cipher";

println("Text: ", str);
println("key:  ", key);

cod := encipher(str, key, True);  println("Code: ", cod);
dec := encipher(cod, key, False); println("Back: ", dec);
```

```txt

Text: Beware the Jabberwock, my son! The jaws that bite, the claws that catch!
key:  Vigenere Cipher
Code: WMCEEIKLGRPIFVMEUGXQPWQVIOIAVEYXUEKFKBTALVXTGAFXYEVKPAGY
Back: BEWARETHEJABBERWOCKMYSONTHEJAWSTHATBITETHECLAWSTHATCATCH

```

