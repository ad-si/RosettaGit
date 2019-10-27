+++
title = "RSA code"
description = ""
date = 2019-09-13T07:40:42Z
aliases = []
[extra]
id = 9384
[taxonomies]
categories = []
tags = []
+++

{{task|Encryption}}
Given an [[wp:RSA|RSA]] key (n,e,d), construct a program to encrypt and decrypt plaintext messages strings. 

'''Background'''

RSA code is used to encode secret messages. It is named after Ron Rivest, Adi Shamir, and Leonard Adleman who published it at MIT in 1977. The advantage of this type of encryption is that you can distribute the number “<math>n</math>” and “<math>e</math>” (which makes up the Public Key used for encryption) to everyone. The Private Key used for decryption “<math>d</math>” is kept secret, so that only the recipient can read the encrypted plaintext. 

The process by which this is done is that a message, for example “Hello World” is encoded as numbers (This could be encoding as ASCII or as a subset of characters <math>a=01,b=02,...,z=26</math>). This yields a string of numbers, generally referred to as "numerical plaintext", “<math>P</math>”. For example, “Hello World” encoded with a=1,...,z=26 by hundreds would yield <math>0805 1212 1523 1518 1204</math>. 

The plaintext must also be split into blocks so that the numerical plaintext is smaller than <math>n</math> otherwise the decryption will fail. 

The ciphertext, <math>C</math>, is then computed by taking each block of <math>P</math>, and computing
: <math>C \equiv  P^e \mod n</math>
Similarly, to decode, one computes
: <math>P \equiv  C^d \mod n</math> 

To generate a key, one finds 2 (ideally large) primes <math>p</math> and <math>q</math>. the value “<math>n</math>” is simply: <math>n = p \times q</math>. 
One must then choose an “<math>e</math>” such that <math>\gcd(e, (p-1)\times(q-1) ) = 1</math>. That is to say, <math>e</math> and <math>(p-1)\times(q-1)</math> are relatively prime to each other. 

The decryption value <math>d</math> is then found by solving
: <math>d\times e \equiv  1 \mod (p-1)\times(q-1)</math>

The security of the code is based on the secrecy of the Private Key (decryption exponent) “<math>d</math>” and the difficulty in factoring “<math>n</math>”.  Research into RSA facilitated advances in factoring and a number of [http://www.rsa.com/rsalabs/node.asp?id=2092 factoring challenges].   Keys of 768 bits have been successfully factored.  While factoring of keys of 1024 bits has not been demonstrated, NIST expected them to be factorable by 2010 and now recommends 2048 bit keys going forward (see [[wp:Key_size#Asymmetric_algorithm_key_lengths|Asymmetric algorithm key lengths]] or [http://csrc.nist.gov/publications/nistpubs/800-57/sp800-57-Part1-revised2_Mar08-2007.pdf NIST 800-57 Pt 1 Revised Table 4: Recommended algorithms and minimum key sizes]).

'''Summary of the task requirements:'''

*  Encrypt and Decrypt a short message or two using RSA with a demonstration key.  
*  Implement RSA do not call a library.
*  Encode and decode the message using any reversible method of your choice (ASCII or a=1,..,z=26 are equally fine). 
*  Either support blocking or give an error if the message would require blocking)
*  Demonstrate that your solution could support real keys by using a non-trivial key that requires large integer support (built-in or libraries).  There is no need to include library code but it must be referenced unless it is built into the language. The following keys will be meet this requirement;however, they are NOT long enough to be considered secure:
:: n = 9516311845790656153499716760847001433441357
:: e = 65537
:: d = 5617843187844953170308463622230283376298685
*  Messages can be hard-coded into the program, there is no need for elaborate input coding.
*  Demonstrate that your implementation works by showing plaintext, intermediate results, encrypted text, and decrypted text.

{{alertbox|#ffff70|'''<big>Warning</big>'''<br/>Rosetta Code is '''not''' a place you should rely on for examples of code in critical roles, including security.<br/>Cryptographic routines should be validated before being used.<br/>For a discussion of limitations and please refer to [[Talk:RSA_code#Difference_from_practical_cryptographical_version]].}}


## Ada

The code below uses a thik and a thin binding of gmp.


```ada

WITH GMP, GMP.Integers, Ada.Text_IO, GMP.Integers.Aliased_Internal_Value, Interfaces.C;
USE GMP, Gmp.Integers, Ada.Text_IO, Interfaces.C;

PROCEDURE Main IS
   FUNCTION "+" (U : Unbounded_Integer) RETURN Mpz_T IS (Aliased_Internal_Value (U));
   FUNCTION "+" (S : String) RETURN Unbounded_Integer IS (To_Unbounded_Integer (S));
   FUNCTION Image_Cleared (M : Mpz_T) RETURN String IS (Image (To_Unbounded_Integer (M)));
   N                 : Unbounded_Integer := +"9516311845790656153499716760847001433441357";
   E                 : Unbounded_Integer := +"65537";
   D                 : Unbounded_Integer := +"5617843187844953170308463622230283376298685";
   Plain_Text        : CONSTANT String := "Rosetta Code";
   M, M_C, M_D       : Mpz_T;
-- We import two C functions from the GMP library which are not in the specs of the gmp package
   PROCEDURE Mpz_Import
     (Rop   : Mpz_T; Count : Size_T; Order : Int; Size : Size_T; Endian : Int;
      Nails : Size_T; Op : Char_Array);
   PRAGMA Import (C, Mpz_Import, "__gmpz_import");

   PROCEDURE Mpz_Export
     (Rop    : OUT Char_Array; Count : ACCESS Size_T; Order : Int; Size : Size_T;
      Endian : Int; Nails : Size_T; Op : Mpz_T);
   PRAGMA Import (C, Mpz_Export, "__gmpz_export");
BEGIN
   Mpz_Init (M);
   Mpz_Init (M_C);
   Mpz_Init (M_D);
   Mpz_Import (M, Plain_Text'Length + 1, 1, 1, 0, 0, To_C (Plain_Text));
   Mpz_Powm (M_C, M, +E, +N);
   Mpz_Powm (M_D, M_C, +D, +N);   
   Put_Line ("Encoded plain text: " & Image_Cleared (M));
   DECLARE Decrypted : Char_Array (1 .. Mpz_Sizeinbase (M_C, 256));
   BEGIN      
      Put_Line ("Encryption of this encoding: " & Image_Cleared (M_C));
      Mpz_Export (Decrypted, NULL, 1, 1, 0, 0, M_D);
      Put_Line ("Decryption of the encoding: " & Image_Cleared (M_D));
      Put_Line ("Final decryption: " &  To_Ada (Decrypted));
   END;
END Main;

```


{{out}}

```txt

Encoded plain text: 6531201667836323769493764728064
Encryption of this encoding: 8527003485686414372697775926080309566820293
Decryption of the encoding: 6531201667836323769493764728064
Final decryption: Rosetta Code

```



## ALGOL 68

The code below uses Algol 68 Genie which provides arbitrary precision arithmetic for LONG LONG modes.


```algol68

COMMENT
   First cut.  Doesn't yet do blocking and deblocking.  Also, as
   encryption and decryption are identical operations but for the
   reciprocal exponents used, only one has been implemented below.

   A later release will address these issues.
COMMENT

BEGIN
   PR precision=1000 PR
   MODE LLI = LONG LONG INT;    CO For brevity CO
   PROC mod power = (LLI base, exponent, modulus) LLI :
   BEGIN
      LLI result := 1, b := base, e := exponent;
      IF exponent < 0
      THEN
	 put (stand error, (("Negative exponent", exponent, newline)))
      ELSE
	 WHILE e > 0
	 DO
	    (ODD e | result := (result * b) MOD modulus);
	    e OVERAB 2; b := (b * b) MOD modulus
	 OD
      FI;
      result
   END;
   PROC modular inverse = (LLI a, m) LLI :
   BEGIN
      PROC extended gcd = (LLI x, y) []LLI :
      BEGIN
	 LLI v := 1, a := 1, u := 0, b := 0, g := x, w := y;
	 WHILE w>0
	 DO
	    LLI q := g % w, t := a - q * u;
	    a := u; u := t;
	    t := b - q * v;
	    b := v; v := t;
	    t := g - q * w;
	    g := w; w := t
	 OD;
	 a PLUSAB (a < 0 | u | 0);
	 (a, b, g)
      END;
      [] LLI egcd = extended gcd (a, m);
      (egcd[3] > 1 | 0 | egcd[1] MOD m)
   END;
   PROC number to string = (LLI number) STRING :
   BEGIN
      [] CHAR map = (blank + "ABCDEFGHIJKLMNOPQRSTUVWXYZ")[@0];
      LLI local number := number;
      INT length := SHORTEN SHORTEN ENTIER long long log(number) + 1;
      (ODD length | length PLUSAB 1);
      [length % 2] CHAR text;
      FOR i FROM length % 2 BY -1 TO 1
      DO
	 INT index = SHORTEN SHORTEN (local number MOD 100);
	 text[i] := (index > 26 | "?" | map[index]);
	 local number := local number % 100
      OD;
      text
   END;
CO The parameters of a particular RSA cryptosystem CO
   LLI p = 3490529510847650949147849619903898133417764638493387843990820577;
   LLI q = 32769132993266709549961988190834461413177642967992942539798288533;
   LLI n = p * q;
   LLI phi n = (p-1) * (q-1);
   LLI e = 9007;
   LLI d = modular inverse (e, phi n);
CO A ciphertext CO
   LLI cipher text = 96869613754622061477140922254355882905759991124574319874695120930816298225145708356931476622883989628013391990551829945157815154;
CO Print out the corresponding plain text CO
   print (number to string (mod power (ciphertext, d, n)))
END

```

{{out}}

```txt

THE MAGIC WORDS ARE SQUEAMISH OSSIFRAGE

```



## C

{{libheader|GMP}}

```C

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gmp.h>

int main(void)
{
    mpz_t n, d, e, pt, ct;

    mpz_init(pt);
    mpz_init(ct);
    mpz_init_set_str(n, "9516311845790656153499716760847001433441357", 10);
    mpz_init_set_str(e, "65537", 10);
    mpz_init_set_str(d, "5617843187844953170308463622230283376298685", 10);

    const char *plaintext = "Rossetta Code";
    mpz_import(pt, strlen(plaintext), 1, 1, 0, 0, plaintext);

    if (mpz_cmp(pt, n) > 0)
        abort();

    mpz_powm(ct, pt, e, n);
    gmp_printf("Encoded:   %Zd\n", ct);

    mpz_powm(pt, ct, d, n);
    gmp_printf("Decoded:   %Zd\n", pt);

    char buffer[64];
    mpz_export(buffer, NULL, 1, 1, 0, 0, pt);
    printf("As String: %s\n", buffer);

    mpz_clears(pt, ct, n, e, d, NULL);
    return 0;
}

```

{{out}}

```txt

Encoded:   5278143020249600501803788468419399384934220
Decoded:   6531201733672758787904906421349
As String: Rossetta Code

```



=={{header|C sharp|C#}}==
 
{{libheader|System.Numerics}}


```csharp
using System;
using System.Numerics;
using System.Text;

class Program
{
    static void Main(string[] args)
    {
        BigInteger n = BigInteger.Parse("9516311845790656153499716760847001433441357");
        BigInteger e = 65537;
        BigInteger d = BigInteger.Parse("5617843187844953170308463622230283376298685");

        const string plaintextstring = "Hello, Rosetta!";
        byte[] plaintext = ASCIIEncoding.ASCII.GetBytes(plaintextstring);
        BigInteger pt = new BigInteger(plaintext);
        if (pt > n)
            throw new Exception();

        BigInteger ct = BigInteger.ModPow(pt, e, n);
        Console.WriteLine("Encoded:  " + ct);

        BigInteger dc = BigInteger.ModPow(ct, d, n);
        Console.WriteLine("Decoded:  " + dc);

        string decoded = ASCIIEncoding.ASCII.GetString(dc.ToByteArray());
        Console.WriteLine("As ASCII: " + decoded);
    }
}
```


{{out}}

```txt
Encoded:  8545729659809274764853392532557102329563535
Decoded:  173322416552962951144796590453843272
As ASCII: Hello, Rosetta!
```



## Common Lisp

In this example, the functions encode-string and decode-string are responsible for converting the string to an integer (which can be encoded) and back. They are not very important to the RSA algorithm, which happens in encode-rsa, decode-rsa, and mod-exp. 

The string is encoded as follows: each character is converted into 2 digits based on ASCII value (subtracting 32, so that SPACE=00, and so on.) To decode we simply read every 2 digits from the given integer in order, adding 32 and converting back into characters.


```lisp
(defparameter *n* 9516311845790656153499716760847001433441357)
(defparameter *e* 65537)
(defparameter *d* 5617843187844953170308463622230283376298685)

;; magic
(defun encode-string (message) 
  (parse-integer (reduce #'(lambda (x y) (concatenate 'string x y))
     (loop for c across message collect (format nil "~2,'0d" (- (char-code c) 32))))))

;; sorcery
(defun decode-string (message) (coerce (loop for (a b) on 
  (loop for char across (write-to-string message) collect char) 
    by #'cddr collect (code-char (+ (parse-integer (coerce (list a b) 'string)) 32))) 'string))

;; ACTUAL RSA ALGORITHM STARTS HERE ;;

;; fast modular exponentiation: runs in O(log exponent)
;; acc is initially 1 and contains the result by the end
(defun mod-exp (base exponent modulus acc) 
  (if (= exponent 0) acc 
    (mod-exp (mod (* base base) modulus) (ash exponent -1) modulus 
	     (if (= (mod exponent 2) 1) (mod (* acc base) modulus) acc))))

;; to encode a message, we first convert it to its integer form. 
;; then, we raise it to the *e* power, modulo *n*
(defun encode-rsa (message) 
  (mod-exp (encode-string message) *e* *n* 1))

;; to decode a message, we raise it to *d* power, modulo *n*
;; and then convert it back into a string
(defun decode-rsa (message) 
  (decode-string (mod-exp message *d* *n* 1)))

```

Interpreter output (the star * represents the interpreter prompt):

```txt

* (load "rsa.lisp")

T
* (encode-rsa "Rosetta Code")

4330737636866106722999010287941987299297557
* (decode-rsa 4330737636866106722999010287941987299297557) 

"Rosetta Code"

```



## D

This used the D module of the Modular Exponentiation Task.
{{trans|Go}}

```d
void main() {
    import std.stdio, std.bigint, std.algorithm, std.string, std.range,
           modular_exponentiation;

    immutable txt = "Rosetta Code";
    writeln("Plain text:             ", txt);

    // A key set big enough to hold 16 bytes of plain text in
    // a single block (to simplify the example) and also big enough
    // to demonstrate efficiency of modular exponentiation.
    immutable BigInt n = "2463574872878749457479".BigInt *
                         "3862806018422572001483".BigInt;
    immutable BigInt e = 2 ^^ 16 + 1;
    immutable BigInt d = "5617843187844953170308463622230283376298685";

    // Convert plain text to a number.
    immutable txtN = reduce!q{ (a << 8) | uint(b) }(0.BigInt, txt);
    if (txtN >= n)
        return writeln("Plain text message too long.");
    writeln("Plain text as a number: ", txtN);

    // Encode a single number.
    immutable enc = txtN.powMod(e, n);
    writeln("Encoded:                ", enc);

    // Decode a single number.
    auto dec = enc.powMod(d, n);
    writeln("Decoded:                ", dec);

    // Convert number to text.
    char[] decTxt;
    for (; dec; dec >>= 8)
        decTxt ~= (dec & 0xff).toInt;
    writeln("Decoded number as text: ", decTxt.retro);
}
```

{{out}}

```txt
Plain text:             Rosetta Code
Plain text as a number: 25512506514985639724585018469
Encoded:                916709442744356653386978770799029131264344
Decoded:                25512506514985639724585018469
Decoded number as text: Rosetta Code
```

=={{header|F Sharp|F#}}==

```fsharp

//Nigel Galloway February 12th., 2018
let RSA n g l = bigint.ModPow(l,n,g)
let encrypt = RSA 65537I 9516311845790656153499716760847001433441357I
let m_in = System.Text.Encoding.ASCII.GetBytes "The magic words are SQUEAMISH OSSIFRAGE"|>Array.chunkBySize 16|>Array.map(Array.fold(fun n g ->(n*256I)+(bigint(int g))) 0I)
let n = Array.map encrypt m_in
let decrypt = RSA 5617843187844953170308463622230283376298685I 9516311845790656153499716760847001433441357I
let g = Array.map decrypt n
let m_out = Array.collect(fun n->Array.unfold(fun n->if n>0I then Some(byte(int (n%256I)),n/256I) else None) n|>Array.rev) g|>System.Text.Encoding.ASCII.GetString
printfn "'The magic words are SQUEAMISH OSSIFRAGE' as numbers -> %A\nEncrypted -> %A\nDecrypted -> %A\nAs text -> %A" m_in n g m_out

```

{{out}}

```txt

'The magic words are SQUEAMISH OSSIFRAGE' as numbers -> [|112197201611743344895286521511035564832;  129529088517466560781735691575334293331; 23442989443532613|]
Encrypted -> [|3493129515654757560886946157927565680562316;  5582490186309277335090560762784439391588703;  8700785834706594190338047528968122486721264|]
Decrypted -> [|112197201611743344895286521511035564832;  129529088517466560781735691575334293331; 23442989443532613|]
As text -> "The magic words are SQUEAMISH OSSIFRAGE"

```


## FreeBASIC

{{trans|C}}

```freebasic
' version 17-01-2017
' compile with: fbc -s console

#Include Once "gmp.bi"

Dim As Mpz_ptr e, d, n, pt, ct

e  = Allocate(Len(__mpz_struct))
d  = Allocate(Len(__mpz_struct))
n  = Allocate(Len(__mpz_struct))
pt = Allocate(Len(__mpz_struct)) : mpz_init(pt)
ct = Allocate(Len(__mpz_struct)) : mpz_init(ct)

mpz_init_set_str(e, "65537", 10)
mpz_init_set_str(d, "5617843187844953170308463622230283376298685", 10)
mpz_init_set_str(n, "9516311845790656153499716760847001433441357", 10)

Dim As ZString Ptr plaintext : plaintext = Allocate(1000)
Dim As ZString Ptr text      : text      = Allocate(1000)
*plaintext = "Rosetta Code"

mpz_import(pt, Len(*plaintext), 1, 1, 0, 0, plaintext)

If mpz_cmp(pt, n) > 0 Then GoTo clean_up

mpz_powm(ct, pt, e, n)
gmp_printf(!"  Encoded: %Zd\n", ct)

mpz_powm(pt, ct, d, n)
gmp_printf(!"  Decoded: %Zd\n", pt)

mpz_export(text, NULL, 1, 1, 0, 0, pt)
Print "As string: "; *text

clean_up:
DeAllocate(plaintext) : DeAllocate(text)
mpz_clear(e) : mpz_clear(d) : mpz_clear(n)
mpz_clear(pt) : mpz_clear(ct)

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
  Encoded: 916709442744356653386978770799029131264344
  Decoded: 25512506514985639724585018469
As string: Rosetta Code
```



## Go

Note: see the [https://golang.org/pkg/crypto/rsa/ crypto/rsa] package
included with Go for a full implementation.

```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    var n, e, d, bb, ptn, etn, dtn big.Int
    pt := "Rosetta Code"
    fmt.Println("Plain text:            ", pt)

    // a key set big enough to hold 16 bytes of plain text in
    // a single block (to simplify the example) and also big enough
    // to demonstrate efficiency of modular exponentiation.
    n.SetString("9516311845790656153499716760847001433441357", 10)
    e.SetString("65537", 10)
    d.SetString("5617843187844953170308463622230283376298685", 10)

    // convert plain text to a number
    for _, b := range []byte(pt) {
        ptn.Or(ptn.Lsh(&ptn, 8), bb.SetInt64(int64(b)))
    }
    if ptn.Cmp(&n) >= 0 {
        fmt.Println("Plain text message too long")
        return
    }
    fmt.Println("Plain text as a number:", &ptn)

    // encode a single number
    etn.Exp(&ptn, &e, &n)
    fmt.Println("Encoded:               ", &etn)

    // decode a single number
    dtn.Exp(&etn, &d, &n)
    fmt.Println("Decoded:               ", &dtn)

    // convert number to text
    var db [16]byte
    dx := 16
    bff := big.NewInt(0xff)
    for dtn.BitLen() > 0 {
        dx--
        db[dx] = byte(bb.And(&dtn, bff).Int64())
        dtn.Rsh(&dtn, 8)
    }
    fmt.Println("Decoded number as text:", string(db[dx:]))
}
```

Output:

```txt

Plain text:             Rosetta Code
Plain text as a number: 25512506514985639724585018469
Encoded:                916709442744356653386978770799029131264344
Decoded:                25512506514985639724585018469
Decoded number as text: Rosetta Code

```



## Haskell


```Haskell
module RSAMaker 
   where
import Data.Char ( chr )

encode :: String -> [Integer]
encode s = map (toInteger . fromEnum ) s

rsa_encode :: Integer -> Integer -> [Integer] -> [Integer]
rsa_encode n e numbers = map (\num -> mod ( num ^ e ) n ) numbers

rsa_decode :: Integer -> Integer -> [Integer] -> [Integer]
rsa_decode d n ciphers = map (\c -> mod ( c ^ d ) n ) ciphers

decode :: [Integer] -> String
decode encoded = map ( chr . fromInteger ) encoded 

divisors :: Integer -> [Integer]
divisors n = [m | m <- [1..n] , mod n m == 0 ]

isPrime :: Integer -> Bool
isPrime n = divisors n == [1,n]

totient :: Integer -> Integer -> Integer
totient prime1 prime2 = (prime1 - 1 ) * ( prime2 - 1 ) 

myE :: Integer -> Integer
myE tot = head [n | n <- [2..tot - 1] , gcd n tot == 1]

myD :: Integer -> Integer -> Integer  -> Integer
myD e n phi = head [d | d <- [1..n] , mod ( d * e ) phi == 1]

main = do 
   putStrLn "Enter a test text!"
   text <- getLine
   let primes = take 90 $ filter isPrime [1..]
       p1     = last primes
       p2     = last $ init primes
       tot    = totient p1 p2
       e      =  myE tot
       n   = p1  * p2 
       rsa_encoded  =  rsa_encode n e $ encode text
       d  =  myD e n tot
       encrypted = concatMap show rsa_encoded
       decrypted = decode $ rsa_decode d n rsa_encoded 
   putStrLn ("Encrypted: " ++ encrypted ) 
   putStrLn ("And now decrypted: " ++ decrypted )
```

{{out}}

```txt
Enter a test text!
Rosettacode
Encrypted: 65646265111107071564791028551028551458331139502651145035156479
And now decrypted: Rosettacode

```


=={{header|Icon}} and {{header|Unicon}}==
Please read talk pages.


```Icon
procedure main()  # rsa demonstration
   
    n := 9516311845790656153499716760847001433441357
    e := 65537
    d := 5617843187844953170308463622230283376298685
    b := 2^integer(log(n,2))   # for blocking 
    write("RSA Demo using\n   n = ",n,"\n   e = ",e,"\n   d = ",d,"\n   b = ",b)

    every m := !["Rosetta Code", "Hello Word!", 
                 "This message is too long.", repl("x",*decode(n+1))] do {  
       write("\nMessage = ",image(m))
       write(  "Encoded = ",m := encode(m))
       if m := rsa(m,e,n) then {               # unblocked
          write(  "Encrypt = ",m)
          write(  "Decrypt = ",m := rsa(m,d,n))
          }
       else {                                  # blocked
          every put(C := [], rsa(!block(m,b),e,n))
          writes("Encrypt = ") ; every writes(!C," ") ; write()
          every put(P := [], rsa(!C,d,n))
          writes("Decrypt = ") ; every writes(!P," ") ; write()                 
          write("Unblocked = ",m := unblock(P,b))          
          }
       write(  "Decoded = ",image(decode(m)))  
       }    
end

procedure mod_power(base, exponent, modulus)   # fast modular exponentation 
   result := 1
   while exponent > 0 do {
      if exponent % 2 = 1 then 
         result := (result * base) % modulus
      exponent /:= 2   
      base := base ^ 2 % modulus
      }  
   return result
end

procedure rsa(text,e,n)  # return rsa encryption of numerically encoded message; fail if text < n
return mod_power(text,e,text < n)
end

procedure encode(text)  # numerically encode ascii text as int
   every (message := 0) := ord(!text) + 256 * message
   return message
end

procedure decode(message)  # numerically decode int to ascii text
   text := ""
   while text ||:= char((0 < message) % 256) do 
      message /:= 256
   return reverse(text)
end

procedure block(m,b)   # break lg int into blocks of size b
   M := []
   while push(M, x := (0 < m) % b) do
      m /:= b
   return M
end
     
procedure unblock(M,b)  # reassemble blocks of size b into lg int
   every (m := 0) := !M + b * m
   return m
end
```


Output:


```txt
RSA Demo using
   n = 9516311845790656153499716760847001433441357
   e = 65537
   d = 5617843187844953170308463622230283376298685
   b = 5575186299632655785383929568162090376495104

Message = "Rosetta Code"
Encoded = 25512506514985639724585018469
Encrypt = 916709442744356653386978770799029131264344
Decrypt = 25512506514985639724585018469
Decoded = "Rosetta Code"

Message = "Hello Word!"
Encoded = 87521618088882533792113697
Encrypt = 1798900477268307339588642263628429901019383
Decrypt = 87521618088882533792113697
Decoded = "Hello Word!"

Message = "This message is too long."
Encoded = 529836718428469753460978059376661024804668788418205881100078
Encrypt = 3376966937987363040878203966915676619521252 7002174816151673360605669161609885530980579 
Decrypt = 95034800624219541 4481988526688939374478063610382714873472814 
Unblocked = 529836718428469753460978059376661024804668788418205881100078
Decoded = "This message is too long."

Message = "xxxxxxxxxxxxxxxxxx"
Encoded = 10494468328720293243075632128305111296931960
Encrypt = 1 829820657892505002815717051746917810425013 
Decrypt = 1 4919282029087637457691702560143020920436856 
Unblocked = 10494468328720293243075632128305111296931960
Decoded = "xxxxxxxxxxxxxxxxxx"
```



## J


Note, for an implementation with blocking (and a much smaller key) see [http://rosettacode.org/mw/index.php?title=RSA_code&oldid=103802]


```j
   N=: 9516311845790656153499716760847001433441357x
   E=: 65537x
   D=: 5617843187844953170308463622230283376298685x
   
   ] text=: 'Rosetta Code'
Rosetta Code
   ] num=: 256x #. a.i.text
25512506514985639724585018469
   num >: N  NB. check if blocking is necessary (0 means no)
0
   ] enc=: N&|@^&E num
916709442744356653386978770799029131264344
   ] dec=: N&|@^&D enc
25512506514985639724585018469
   ] final=: a. {~ 256x #.inv dec
Rosetta Code
```


Note: as indicated at http://www.jsoftware.com/help/dictionary/special.htm, <code>N&|@^</code> does not bother with creating the exponential intermediate result.


## Java



```java

public static void main(String[] args) {
    /*
    This is probably not the best method...or even the most optimized way...however it works since n and d are too big to be ints or longs
    This was also only tested with 'Rosetta Code' and 'Hello World'
    It's also pretty limited on plainText size (anything bigger than the above will fail)
    */
    BigInteger n = new BigInteger("9516311845790656153499716760847001433441357");
    BigInteger e = new BigInteger("65537");
    BigInteger d = new BigInteger("5617843187844953170308463622230283376298685");
    Charset c = Charsets.UTF_8;
    String plainText = "Rosetta Code";
    System.out.println("PlainText : " + plainText);
    byte[] bytes = plainText.getBytes();
    BigInteger plainNum = new BigInteger(bytes);
    System.out.println("As number : " + plainNum);
    BigInteger Bytes = new BigInteger(bytes);
    if (Bytes.compareTo(n) == 1) {
        System.out.println("Plaintext is too long");
        return;
    }
    BigInteger enc = plainNum.modPow(e, n);
    System.out.println("Encoded: " + enc);
    BigInteger dec = enc.modPow(d, n);
    System.out.println("Decoded: " + dec);
    String decText = new String(dec.toByteArray(), c);
    System.out.println("As text: " + decText);
}

```



## Julia

{{works with|Julia|0.6}}


```julia
function rsaencode(clearmsg::AbstractString, nmod::Integer, expub::Integer)
    bytes = parse(BigInt, "0x" * bytes2hex(collect(UInt8, clearmsg)))
    return powermod(bytes, expub, nmod)
end

function rsadecode(cryptmsg::Integer, nmod::Integer, dsecr::Integer)
    decoded = powermod(encoded, dsecr, nmod)
    return join(Char.(hex2bytes(hex(decoded))))
end

msg = "Rosetta Code."
nmod = big"9516311845790656153499716760847001433441357"
expub = 65537
dsecr = big"5617843187844953170308463622230283376298685"

encoded = rsaencode(msg, nmod, expub)
decoded = rsadecode(encoded, nmod, dsecr)
println("\n# $msg\n -> ENCODED: $encoded\n -> DECODED: $decoded")
```


{{out}}

```txt
# Rosetta Code.
 -> ENCODED: 2440331969632134446717000067136916252596373
 -> DECODED: Rosetta Code.
```



## Kotlin


```scala
// version 1.1.4-3

import java.math.BigInteger

fun main(args: Array<String>) {
    val n = BigInteger("9516311845790656153499716760847001433441357")
    val e = BigInteger("65537")
    val d = BigInteger("5617843187844953170308463622230283376298685")
    val c = Charsets.UTF_8
    val plainText = "Rosetta Code"
    println("PlainText : $plainText")
    val bytes = plainText.toByteArray(c)
    val plainNum = BigInteger(bytes)
    println("As number : $plainNum")
    if (plainNum > n) {
        println("Plaintext is too long")
        return
    }

    val enc = plainNum.modPow(e, n)
    println("Encoded   : $enc")

    val dec = enc.modPow(d, n)
    println("Decoded   : $dec")
 
    val decText = dec.toByteArray().toString(c)
    println("As text   : $decText")
}
```


{{out}}

```txt

PlainText : Rosetta Code
As number : 25512506514985639724585018469
Encoded   : 916709442744356653386978770799029131264344
Decoded   : 25512506514985639724585018469
As text   : Rosetta Code

```



## Mathematica

Does not support blocking.
<lang>toNumPlTxt[s_] := FromDigits[ToCharacterCode[s], 256];
fromNumPlTxt[plTxt_] := FromCharacterCode[IntegerDigits[plTxt, 256]];
enc::longmess = "Message '``' is too long for n = ``.";
enc[n_, _, mess_] /; 
   toNumPlTxt[mess] >= n := (Message[enc::longmess, mess, n]; $Failed);
enc[n_, e_, mess_] := PowerMod[toNumPlTxt[mess], e, n];
dec[n_, d_, en_] := fromNumPlTxt[PowerMod[en, d, n]];
text = "The cake is a lie!";
n = 9516311845790656153499716760847001433441357;
e = 65537;
d = 5617843187844953170308463622230283376298685;
en = enc[n, e, text];
de = dec[n, d, en];
Print["Text: '" <> text <> "'"];
Print["n = " <> IntegerString[n]];
Print["e = " <> IntegerString[e]];
Print["d = " <> IntegerString[d]];
Print["Numeric plaintext: " <> IntegerString[toNumPlTxt[text]]];
Print["Encoded: " <> IntegerString[en]];
Print["Decoded: '" <> de <> "'"];
```

{{out}}

```txt
Text: 'The cake is a lie!'
n = 9516311845790656153499716760847001433441357
e = 65537
d = 5617843187844953170308463622230283376298685
Numeric plaintext: 7352955804624388987810264523908743852287265
Encoded: 199505409518408949879682159958576932863989
Decoded: 'The cake is a lie!'
```



## PARI/GP



```parigp
stigid(V,b)=subst(Pol(V),'x,b);		\\ inverse function digits(...)

n = 9516311845790656153499716760847001433441357;
e = 65537;
d = 5617843187844953170308463622230283376298685;

text = "Rosetta Code"

inttext = stigid(Vecsmall(text),256)	\\ message as an integer
encoded = lift(Mod(inttext, n) ^ e)	\\ encrypted message
decoded = lift(Mod(encoded, n) ^ d)	\\ decrypted message
message = Strchr(digits(decoded, 256))	\\ readable message
```


Output:
```txt

text:	 "Rosetta Code"
inttext: 25512506514985639724585018469
encoded: 916709442744356653386978770799029131264344
decoded: 25512506514985639724585018469
message: "Rosetta Code"

```


If inttext is equal or greater than ''b = 2^(log(n)/log(2)\1)'' use ''block = inttext % b; inttext /= b;'' to break inttext into blocks and encode piece by piece. Decode in reverse order. 

As a check: it's easy to crack this weak encrypted message without knowing secret key 'd'

```parigp
f = factor(n);				\\ factorize public key 'n'

crack = Strchr(digits(lift(Mod(encoded,n) ^ lift(Mod(1,(f[1,1]-1)*(f[2,1]-1)) / e)),256))
```


Output:
```txt
crack: "Rosetta Code"
```



## Perl

{{trans|Perl 6}}

```perl
use bigint;

$n = 9516311845790656153499716760847001433441357;
$e = 65537;
$d = 5617843187844953170308463622230283376298685;

package Message {
    my @alphabet;
    push @alphabet, $_ for 'A' .. 'Z', ' ';
    my $rad = +@alphabet;
    $code{$alphabet[$_]} = $_ for 0..$rad-1;

    sub encode {
        my($t) = @_;
        my $cnt = my $sum = 0;
        for (split '', reverse $t) {
            $sum += $code{$_} * $rad**$cnt;
            $cnt++;
        }
        $sum;
    }

    sub decode {
        my($n) = @_;
        my(@i);
        while () {
            push @i, $n % $rad;
            last if  $n < $rad;
            $n = int $n / $rad;
        }
        reverse join '', @alphabet[@i];
    }

    sub expmod {
    my($a, $b, $n) = @_;
    my $c = 1;
    do {
        ($c *= $a) %= $n if $b % 2;
        ($a *= $a) %= $n;
    } while ($b = int $b/2);
    $c;
}

}

my $secret_message = "ROSETTA CODE";

$numeric_message  = Message::encode $secret_message;
$numeric_cipher   = Message::expmod $numeric_message, $e, $n;
$text_cipher      = Message::decode $numeric_cipher;
$numeric_cipher2  = Message::encode $text_cipher;
$numeric_message2 = Message::expmod $numeric_cipher2, $d, $n;
$secret_message2  = Message::decode $numeric_message2;

print <<"EOT";
Secret message is $secret_message
Secret message in integer form is $numeric_message
After exponentiation with public exponent we get: $numeric_cipher
This turns into the string $text_cipher
If we re-encode it in integer form we get $numeric_cipher2
After exponentiation with SECRET exponent we get: $numeric_message2
This turns into the string $secret_message2
EOT
```

{{out}}

```txt
Secret message is ROSETTA CODE
Secret message in integer form is 97525102075211938
After exponentiation with public exponent we get: 8326171774113983822045243488956318758396426
This turns into the string ZULYDCEZOWTFXFRRNLIMGNUPHVCJSX
If we re-encode it in integer form we get 8326171774113983822045243488956318758396426
After exponentiation with SECRET exponent we get: 97525102075211938
This turns into the string ROSETTA CODE
```



## Perl 6

{{Works with|rakudo|2015-11-04}}
No blocking here.  Algorithm doesn't really work if either red or black text begins with 'A'.

```perl6
constant $n = 9516311845790656153499716760847001433441357;
constant $e = 65537;
constant $d = 5617843187844953170308463622230283376298685;
 
my $secret-message = "ROSETTA CODE";
 
package Message {
    my @alphabet = slip('A' .. 'Z'), ' ';
    my $rad = +@alphabet;
    my %code = @alphabet Z=> 0 .. *;
    subset Text of Str where /^^ @alphabet+ $$/;
    our sub encode(Text $t) {
	[+] %code{$t.flip.comb} Z* (1, $rad, $rad*$rad ... *);
    }
    our sub decode(Int $n is copy) {
	@alphabet[
	    gather loop {
		take $n % $rad;
		last if $n < $rad;
		$n div= $rad;
	    }
	].join.flip;
    }
}
 
use Test;
plan 1;
 
say "Secret message is $secret-message";
say "Secret message in integer form is $_" given
    my $numeric-message = Message::encode $secret-message;
say "After exponentiation with public exponent we get: $_" given
    my $numeric-cipher = expmod $numeric-message, $e, $n;
say "This turns into the string $_" given
    my $text-cipher = Message::decode $numeric-cipher;
 
say "If we re-encode it in integer form we get $_" given
    my $numeric-cipher2 = Message::encode $text-cipher;
say "After exponentiation with SECRET exponent we get: $_" given
    my $numeric-message2 = expmod $numeric-cipher2, $d, $n;
say "This turns into the string $_" given
    my $secret-message2 = Message::decode $numeric-message2;
 
is $secret-message, $secret-message2, "the message has been correctly decrypted";
```

{{out}}

```txt
1..1
Secret message is ROSETTA CODE
Secret message in integer form is 97525102075211938
After exponentiation with public exponent we get: 8326171774113983822045243488956318758396426
This turns into the string ZULYDCEZOWTFXFRRNLIMGNUPHVCJSX
If we re-encode it in integer form we get 8326171774113983822045243488956318758396426
After exponentiation with SECRET exponent we get: 97525102075211938
This turns into the string ROSETTA CODE
ok 1 - the message has been correctly decrypted
```



## Phix

{{libheader|mpfr}}
{{trans|C}}

```Phix

include builtins/mpfr.e

mpz n = mpz_init("9516311845790656153499716760847001433441357"),
    e = mpz_init("65537"),
    d = mpz_init("5617843187844953170308463622230283376298685"),
    pt = mpz_init(),
    ct = mpz_init()
 
string plaintext = "Rossetta Code" -- matches C/zkl
--                 "Rosetta Code" -- matches D/FreeBasic/Go/Icon/J/Kotlin/Seed7.

mpz_import(pt, length(plaintext), 1, 1, 0, 0, plaintext)
 
if mpz_cmp(pt, n)>0 then ?9/0 end if
 
mpz_powm(ct, pt, e, n);
printf(1,"Encoded:   %s\n", {mpz_get_str(ct)})
 
mpz_powm(pt, ct, d, n);
printf(1,"Decoded:   %s\n", {mpz_get_str(pt)})
 
integer size =floor((mpz_sizeinbase(pt,2)+7)/8)
atom pMem = allocate(size,true)
integer count = mpz_export(pMem, 1, 1, 0, 0, pt)
if count>size then ?9/0 end if

printf(1,"As String: %s\n", {peek({pMem,count})})
 
{pt, ct, n, e, d} = mpz_free({pt, ct, n, e, d})
```

{{out}}

```txt

Encoded:   5278143020249600501803788468419399384934220
Decoded:   6531201733672758787904906421349
As String: Rossetta Code

```



## PowerShell

{{trans|C#}}

```PowerShell

$n = [BigInt]::Parse("9516311845790656153499716760847001433441357")
$e = [BigInt]::new(65537)
$d = [BigInt]::Parse("5617843187844953170308463622230283376298685")
$plaintextstring = "Hello, Rosetta!"
$plaintext = [Text.ASCIIEncoding]::ASCII.GetBytes($plaintextstring)
[BigInt]$pt = [BigInt]::new($plaintext)
if ($n -lt $pt) {throw "`$n = $n < $pt = `$pt"}
$ct = [BigInt]::ModPow($pt, $e, $n)
"Encoded:  $ct"
$dc = [BigInt]::ModPow($ct, $d, $n)
"Decoded:  $dc"
$decoded = [Text.ASCIIEncoding]::ASCII.GetString($dc.ToByteArray())
"As ASCII: $decoded"

```


{{out}}

```txt

Encoded:  8545729659809274764853392532557102329563535
Decoded:  173322416552962951144796590453843272
As ASCII: Hello, Rosetta!

```



## PicoLisp

PicoLisp comes with an RSA library:

```PicoLisp
### This is a copy of "lib/rsa.l" ###

# Generate long random number
(de longRand (N)
   (use (R D)
      (while (=0 (setq R (abs (rand)))))
      (until (> R N)
         (unless (=0 (setq D (abs (rand))))
            (setq R (* R D)) ) )
      (% R N) ) )

# X power Y modulus N
(de **Mod (X Y N)
   (let M 1
      (loop
         (when (bit? 1 Y)
            (setq M (% (* M X) N)) )
         (T (=0 (setq Y (>> 1 Y)))
            M )
         (setq X (% (* X X) N)) ) ) )

# Probabilistic prime check
(de prime? (N)
   (and
      (> N 1)
      (bit? 1 N)
      (let (Q (dec N)  K 0)
         (until (bit? 1 Q)
            (setq
               Q  (>> 1 Q)
               K  (inc K) ) )
         (do 50
            (NIL (_prim? N Q K))
            T ) ) ) )

# (Knuth Vol.2, p.379)
(de _prim? (N Q K)
   (use (X J Y)
      (while (> 2 (setq X (longRand N))))
      (setq
         J 0
         Y (**Mod X Q N) )
      (loop
         (T
            (or
               (and (=0 J) (= 1 Y))
               (= Y (dec N)) )
            T )
         (T
            (or
               (and (> J 0) (= 1 Y))
               (<= K (inc 'J)) )
            NIL )
         (setq Y (% (* Y Y) N)) ) ) )

# Find a prime number with `Len' digits
(de prime (Len)
   (let P (longRand (** 10 (*/ Len 2 3)))
      (unless (bit? 1 P)
         (inc 'P) )
      (until (prime? P)  # P: Prime number of size 2/3 Len
         (inc 'P 2) )
      # R: Random number of size 1/3 Len
      (let (R (longRand (** 10 (/ Len 3)))  K (+ R (% (- P R) 3)))
         (when (bit? 1 K)
            (inc 'K 3) )
         (until (prime? (setq R (inc (* K P))))
            (inc 'K 6) )
         R ) ) )

# Generate RSA key
(de rsaKey (N)  #> (Encrypt . Decrypt)
   (let (P (prime (*/ N 5 10))  Q (prime (*/ N 6 10)))
      (cons
         (* P Q)
         (/
            (inc (* 2 (dec P) (dec Q)))
            3 ) ) ) )

# Encrypt a list of characters
(de encrypt (Key Lst)
   (let Siz (>> 1 (size Key))
      (make
         (while Lst
            (let N (char (pop 'Lst))
               (while (> Siz (size N))
                  (setq N (>> -16 N))
                  (inc 'N (char (pop 'Lst))) )
               (link (**Mod N 3 Key)) ) ) ) ) )

# Decrypt a list of numbers
(de decrypt (Keys Lst)
   (mapcan
      '((N)
         (let Res NIL
            (setq N (**Mod N (cdr Keys) (car Keys)))
            (until (=0 N)
               (push 'Res (char (& `(dec (** 2 16)) N)))
               (setq N (>> 16 N)) )
            Res ) )
      Lst ) )
### End of "lib/rsa.l" ###

# Generate 100-digit keys (private . public)
: (setq Keys (rsaKey 100))
-> (14394597526321726957429995133376978449624406217727317004742182671030....

# Encrypt
: (setq CryptText
   (encrypt (car Keys)
      (chop "The quick brown fox jumped over the lazy dog's back") ) )
-> (72521958974980041245760752728037044798830723189142175108602418861716...

# Decrypt
: (pack (decrypt Keys CryptText))
-> "The quick brown fox jumped over the lazy dog's back"
```



## Python



```python
import binascii

n = 9516311845790656153499716760847001433441357    # p*q = modulus
e = 65537
d = 5617843187844953170308463622230283376298685

message='Rosetta Code!'
print('message                 ', message)

hex_data   = binascii.hexlify(message.encode())
print('hex data                ', hex_data)

plain_text = int(hex_data, 16)
print('plain text integer      ', plain_text)

if plain_text > n:
  raise Exception('plain text too large for key')

encrypted_text = pow(plain_text,     e, n)
print('encrypted text integer  ', encrypted_text)

decrypted_text = pow(encrypted_text, d, n)
print('decrypted text integer  ', decrypted_text)

print('message                 ', binascii.unhexlify(hex(decrypted_text)[2:]).decode())     # [2:] slicing, to strip the 0x part 


```

{{output}}

```txt

message                  Rosetta Code!
hex data                 b'526f736574746120436f646521'
plain text integer       6531201667836323769493764728097
encrypted text integer   5307878626309103053766094186556322974789734
decrypted text integer   6531201667836323769493764728097
message                  Rosetta Code!

```



## Racket


This implementation does key generation and demonstrates digital signature as a freebie.

Thanks again to the wonderful math/number-theory package (distributed as standard).

Cutting messages into blocks has not been done.


```racket
#lang racket
(require math/number-theory)
(define-logger rsa)
(current-logger rsa-logger)

;; -| STRING TO NUMBER MAPPING |----------------------------------------------------------------------
(define (bytes->number B) ; We'll need our data in numerical form ..
  (for/fold ((rv 0)) ((b B)) (+ b (* rv 256))))

(define (number->bytes N) ; .. and back again
  (define (inr n b) (if (zero? n) b (inr (quotient n 256) (bytes-append (bytes (modulo n 256)) b))))
  (inr N (bytes)))

;; -| RSA PUBLIC / PRIVATE FUNCTIONS |----------------------------------------------------------------
;; The basic definitions... pretty well lifted from the text book!
(define ((C e n) p)
  ;; Just do the arithmetic to demonstrate RSA...
  ;; breaking large messages into blocks is something for another day.
  (unless (< p n) (raise-argument-error 'C (format "(and/c integer? (</c ~a))" n) p))
  (modular-expt p e n))

(define ((P d n) c)
  (modular-expt c d n))

;; -| RSA KEY GENERATION |----------------------------------------------------------------------------
;; Key generation
;; Full description of the steps can be found on Wikipedia
(define (RSA-keyset function-base-name)
  (log-info "RSA-keyset: ~s" function-base-name)
  (define max-k 4294967087)
  ;; I'm guessing this RNG is about as cryptographically strong as replacing spaces with tabs.
  (define (big-random n-rolls)
    (for/fold ((rv 1)) ((roll (in-range n-rolls 0 -1))) (+ (* rv (add1 max-k)) 1 (random max-k))))
  (define (big-random-prime)
    (define start-number (big-random (/ 1024 32)))
    (log-debug "got large (possibly non-prime) number, finding next prime")
    (next-prime (match start-number ((? odd? o) o) ((app add1 e) e))))
  
  ;; [1] Choose two distinct prime numbers p and q.
  (log-debug "generating p")
  (define p (big-random-prime))
  (log-debug "p generated")
  (log-debug "generating q")
  (define q (big-random-prime))
  (log-debug "q generated")
  (log-info "primes generated")
  
  ;; [2] Compute n = pq.
  (define n (* p q))
  
  ;; [3] Compute φ(n) = φ(p)φ(q) = (p − 1)(q − 1) = n - (p + q -1),
  ;;                    where φ is Euler's totient function.
  (define φ (- n (+ p q -1)))
  
  ;; [4] Choose an integer e such that 1 < e < φ(n) and gcd(e, φ(n)) = 1; i.e., e and φ(n) are
  ;;     coprime. ... most commonly 2^16 + 1 = 65,537 ...
  (define e (+ (expt 2 16) 1))
  
  ;; [5] Determine d as d ≡ e−1 (mod φ(n)); i.e., d is the multiplicative inverse of e (modulo φ(n)).
  (log-debug "generating d")
  (define d (modular-inverse e φ))
  (log-info "d generated")
  (values n e d))

;; -| GIVE A USABLE SET OF PRIVATE STUFF TO A USER |--------------------------------------------------
;; six values: the public (encrypt) function (numeric) 
;;             the private (decrypt) function (numeric)
;;             the public (encrypt) function (bytes) 
;;             the private (decrypt) function (bytes)
;;             private (list n e d)
;;             public (list n e)
(define (RSA-key-pack #:function-base-name function-base-name)
  (define (rnm-fn f s) (procedure-rename f (string->symbol (format "~a-~a" function-base-name s))))
  (define-values (n e d) (RSA-keyset function-base-name))
  (define my-C (rnm-fn (C e n) "C"))
  (define my-P (rnm-fn (P d n) "P"))
  (define my-encrypt (rnm-fn (compose number->bytes my-C bytes->number) "encrypt"))
  (define my-decrypt (rnm-fn (compose number->bytes my-P bytes->number) "decrypt"))
  (values my-C my-P my-encrypt my-decrypt (list n e d) (list n e)))

;; -| HEREON IS JUST A LOAD OF CHATTY DEMOS |---------------------------------------------------------
(define (narrated-encrypt-bytes C who plain-text)
  (define plain-n (bytes->number plain-text))
  (define cypher-n (C plain-n))
  (define cypher-text (number->bytes cypher-n))
  (printf #<<EOS
~a wants to send plain text: ~s
  as number: ~s
  cyphered number: ~s
sent by ~a over the public interwebs:
~s
...


EOS
          who plain-text plain-n cypher-n who cypher-text)
  cypher-text)

(define (narrated-decrypt-bytes P who cypher-text)
  (define cypher-n (bytes->number cypher-text))
  (define plain-n (P cypher-n))
  (define plain-text (number->bytes plain-n))
  (printf #<<EOS
...
~s
  received by ~a
  as number: ~s
  decyphered (with P) number: ~s
decyphered text:
~s


EOS
          cypher-text who cypher-n plain-n plain-text)
  plain-text)

;; ENCRYPT AND DECRYPT A MESSAGE WITH THE e.g. KEYS
(define-values (given-n given-e given-d)
  (values 9516311845790656153499716760847001433441357
          65537
          5617843187844953170308463622230283376298685))

;; Get the keys specific RSA functions
(for ((message-text (list #"hello world" #"TOP SECRET!")))
  (define Bobs-public-function (C given-e given-n))
  (define Bobs-private-function (P given-d given-n))
  (define cypher-text (narrated-encrypt-bytes Bobs-public-function "Alice" message-text))
  (define plain-text (narrated-decrypt-bytes Bobs-private-function "Bob" cypher-text))
  plain-text)

;; Demonstrate with larger keys.
;; (And include a free recap on digital signatures, too)
(define-values (A-pub-C A-pvt-P A-pub-encrypt A-pvt-decrypt A-pvt-keys A-pub-keys)
  (RSA-key-pack #:function-base-name 'Alice))
(define-values (B-pub-C B-pvt-P B-pub-encrypt B-pvt-decrypt B-pvt-keys B-pub-keys)
  (RSA-key-pack #:function-base-name 'Bob))

;; Since p and q are random, it is possible that message' = "message modulo {A,B}-key-n" will be too
;; big for "message' modulo {B,A}-key-n", if that happens then I run the program again until it
;; works. Strictly, we need blocking of the signed message -- which is not yet implemented.
(let* ((plain-A-to-B #"Dear Bob, meet you in Lymm at 1200, Alice")
       (signed-A-to-B         (A-pvt-decrypt plain-A-to-B))
       (unsigned-A-to-B       (A-pub-encrypt signed-A-to-B))
       (crypt-signed-A-to-B   (B-pub-encrypt signed-A-to-B))
       (decrypt-signed-A-to-B (B-pvt-decrypt crypt-signed-A-to-B))
       (decrypt-verified-B    (A-pub-encrypt decrypt-signed-A-to-B)))
  (printf
   #<<EOS
Alice wants to send ~s to Bob.
She "encrypts" with her private "decryption" key.
(A-prv msg) -> ~s
Only she could have done this (only she has the her private key data) -- so this is a signature on the
message. Anyone can verify the signature by "decrypting" the message with the public "encryption" key.
(A-pub (A-prv msg)) -> ~s
But anyone is able to do this, so there is no privacy here.
Everyone knows that it can only be Alice at Lymm at noon, but this message is for Bob's eyes only.
We need to encrypt this with his public key:
(B-pub (A-prv msg)) -> ~s
Which is what gets posted to alt.chat.secret-rendezvous
Bob decrypts this to get the signed message from Alice:
(B-prv (B-pub (A-prv msg))) -> ~s
And verifies Alice's signature:
(A-pub (B-prv (B-pub (A-prv msg)))) -> ~s
Alice genuinely sent the message.
And nobody else (on a.c.s-r, at least) has read it.

KEYS:
 Alice's full set: ~s
 Bob's full set: ~s
EOS
   plain-A-to-B signed-A-to-B unsigned-A-to-B crypt-signed-A-to-B decrypt-signed-A-to-B
   decrypt-verified-B A-pvt-keys B-pvt-keys))
```


{{out}}

```txt
Alice wants to send plain text: #"hello world"
  as number: 126207244316550804821666916
  cyphered number: 4109627268073579506944196826730512948879423
sent by Alice over the public interwebs:
#"/-\e\355\225\327\244\222<R@\20\4\233\275\333`?"
...

...
#"/-\e\355\225\327\244\222<R@\20\4\233\275\333`?"
  received by Bob
  as number: 4109627268073579506944196826730512948879423
  decyphered (with P) number: 126207244316550804821666916
decyphered text:
#"hello world"

Alice wants to send plain text: #"TOP SECRET!"
  as number: 101924313868583037137409057
  cyphered number: 5346093164296793050289700489360581430628365
sent by Alice over the public interwebs:
#"=^\301{\17p\201AE\341D\357 \237IPP\r"
...

...
#"=^\301{\17p\201AE\341D\357 \237IPP\r"
  received by Bob
  as number: 5346093164296793050289700489360581430628365
  decyphered (with P) number: 101924313868583037137409057
decyphered text:
#"TOP SECRET!"

Alice wants to send #"Dear Bob, meet you in Lymm at 1200, Alice" to Bob.
She "encrypts" with her private "decryption" key.
(A-prv msg) -> #"B}\4<G\373-\217\350;\0214\226\233\236\333\215\226\225=\236\350\277X\241*J\356\302\250\350fO\5\375u\367\365\315\270\312\334\204U\332\224\322\357u=\262\326\274e\31\301\321\210:i\361\361g\361\16\5a\304X\306\313\350(^\374 \353\350t\2662\305\346a\300\244b\337JI\343\335\21j\202\236\242\335<rA\a\233\375\23\t\32(d`\237i\267\336\270\340L\26\f\260\346&\t\301\326\331k@\253\242\241VKw\365 \204U\270*\r,\334h=\257\230\320V\357\304\242\4B\240\356\200\204\252\35\20c\220LJ}\275x!\25\23\262\325{\246\304?\36\272\343\17\230\2449Q[y\334(m1\252N<\253?^#\236p\311\3006\f\245M*<\273H\333\225\256\317\322\363\273\335\303\243\354\a\253\342\312\302\372vTQ\247\r\210\343\264\323*E\364\2\ba\305Z79\273M\327\310F\301,\235\32\323"
Only she could have done this (only she has the her private key data) -- so this is a signature on the
message. Anyone can verify the signature by "decrypting" the message with the public "encryption" key.
(A-pub (A-prv msg)) -> #"Dear Bob, meet you in Lymm at 1200, Alice"
But anyone is able to do this, so there is no privacy here.
Everyone knows that it can only be Alice at Lymm at noon, but this message is for Bob's eyes only.
We need to encrypt this with his public key:
(B-pub (A-prv msg)) -> #"\eXq\4/\207\250hs\244<ym\3716\210\357'0\351E\202D\360\177\361\325\24\310+s\340j1\36\0\213\353\254\314*\212a;\300\210\\\347\371z`\226\230 \230A\337d\31\262nwp\6m\312\320D@h\232d]{sN\312xAW\216'c\27V\5\270\267>@\305\312\210\262|tGU?\266\325\250\227\270X\235\6C\307\323D\301q{\266S\351,i\211,~X\341\225z4\320F\353\361I\313M\270&d\267m\207 \2736s_\272\307\275\31T\301\247\317@\16D\263X\"\340\262\204\277g\30\337\311o\205\236\34\370)\323\275\5\1\301>\226Q,\255\213\\\2\307\215c\342\323\16\226a\3U\254\214\275\274\214\325\f\226\347\325\225\354~\32z)\340re5I\321\254\34'T\n\220p\316#\1\347\6;*\347\303\351\342\221\244\eey\31\275y\271\2605y\344\261\202B\321E\335\212"
Which is what gets posted to alt.chat.secret-rendezvous
Bob decrypts this to get the signed message from Alice:
(B-prv (B-pub (A-prv msg))) -> #"B}\4<G\373-\217\350;\0214\226\233\236\333\215\226\225=\236\350\277X\241*J\356\302\250\350fO\5\375u\367\365\315\270\312\334\204U\332\224\322\357u=\262\326\274e\31\301\321\210:i\361\361g\361\16\5a\304X\306\313\350(^\374 \353\350t\2662\305\346a\300\244b\337JI\343\335\21j\202\236\242\335<rA\a\233\375\23\t\32(d`\237i\267\336\270\340L\26\f\260\346&\t\301\326\331k@\253\242\241VKw\365 \204U\270*\r,\334h=\257\230\320V\357\304\242\4B\240\356\200\204\252\35\20c\220LJ}\275x!\25\23\262\325{\246\304?\36\272\343\17\230\2449Q[y\334(m1\252N<\253?^#\236p\311\3006\f\245M*<\273H\333\225\256\317\322\363\273\335\303\243\354\a\253\342\312\302\372vTQ\247\r\210\343\264\323*E\364\2\ba\305Z79\273M\327\310F\301,\235\32\323"
And verifies Alice's signature:
(A-pub (B-prv (B-pub (A-prv msg)))) -> #"Dear Bob, meet you in Lymm at 1200, Alice"
Alice genuinely sent the message.
And nobody else (on a.c.s-r, at least) has read it.

KEYS:
 Alice's full set: (104685401856522903402850023081275254628934665755538824520013952439504139625115997713509448983980532229245117213463882915048453185855818576471069794363975118091990355601850994380420233190180031768156385491949949615002445375960925665247160785747787333124376845288066200576472845984390877385677186819996627676676634639097055255729270941154875343472575964213139374388405182305309760553726485659960423106167598201105611690471457085541574734821426421348095213778701793437599877207476950721505461275161623234401077010666082709757697115644957960465476529769011591060857755043525709942747005873747546230596592939772042294065813 65537 2717089103223300710869400327467677924284264864888101995949520623458451584636500177162207191689440855119183734075439291369721208922299577316283774359722319847940485449116507338466741179127763462435479373816422239271238530669843516739939583694050479174276592059981393826091830737132442474221232201364332570578846365932454353567371199951546467136016124284306002875511872761969350668537869576342139916560099770887726733315363999637008436783521647128919138309876487426046116064403375745886449924998134881929018589019826525209406457786746758225025770316010686323085528641486882271019011944746635302695471929361680924424193)
 Bob's full set: (66453628687555934925745945778628604864426900808865010224295559035622434292221884343034084372211796572669001844069872620681089178469116242843088008182594366670325110214571956032739850447309116856946000976960287962443982329056724158946509616005091553738944083845350969457626620995817549009173612137879065054069514627974682257866567361928526254468372096924822844681482943840826594855542477801468633973217959659919448029041762191501507078772425126221108904380853918736930072466664169841898502073182029626689571748335731893924787111612947107622008163390629359277338946499436753837554940480109336784185532913066463754681017 65537 40065645823449313467522156271281139875308606308813084959528059327930012759030216763756439504389958618427304726562596348031980052624474573194667691034359998325290386803002604616043604539794698176121997293172282195706991070204897106862588071733664686557012032668284371518061563321600604452438728297053809260134398419618573423334221995863281726034127469856978901426632431740449516515840848181614406772903883730243825332015822633960435748349249507976445585019837204822017018926054553157019477128528700400452557614873387735038266042290443427594857847964184247833486305612600841625002197933394323058877499979801727736278613)
```


The burden seems to be in finding (proving) the next large prime after the big random number.
However, once keys are generated, things are pretty snappy.

## Ruby


```ruby

#!/usr/bin/ruby

require 'openssl' # for mod_exp only
require 'prime'

def rsa_encode blocks, e, n
  blocks.map{|b| b.to_bn.mod_exp(e, n).to_i}
end

def rsa_decode ciphers, d, n
  rsa_encode ciphers, d, n
end

# all numbers in blocks have to be < modulus, or information is lost
# for secure encryption only use big modulus and blocksizes
def text_to_blocks text, blocksize=64 # 1 hex = 4 bit => default is 256bit
  text.each_byte.reduce(""){|acc,b| acc << b.to_s(16).rjust(2, "0")} # convert text to hex (preserving leading 0 chars)
      .each_char.each_slice(blocksize).to_a                          # slice hexnumbers in pieces of blocksize
      .map{|a| a.join("").to_i(16)}                                  # convert each slice into internal number
end

def blocks_to_text blocks
  blocks.map{|d| d.to_s(16)}.join("")                                # join all blocks into one hex-string
        .each_char.each_slice(2).to_a                                # group into pairs
	.map{|s| s.join("").to_i(16)}                                # number from 2 hexdigits is byte
	.flatten.pack("C*")                                          # pack bytes into ruby-string
	.force_encoding(Encoding::default_external)                  # reset encoding
end

def generate_keys p1, p2
  n = p1 * p2
  t = (p1 - 1) * (p2 - 1)
  e = 2.step.each do |i|
    break i if i.gcd(t) == 1
  end
  d = 1.step.each do |i|
    break i if (i * e) % t == 1
  end
  return e, d, n
end

p1, p2 = Prime.take(100).last(2)
public_key, private_key, modulus =
  generate_keys p1, p2

print "Message: "
message = gets
blocks = text_to_blocks message, 4 # very small primes
print "Numbers: "; p blocks
encoded = rsa_encode(blocks, public_key, modulus)
print "Encrypted as: "; p encoded
decoded = rsa_decode(encoded, private_key, modulus)
print "Decrypted to: "; p decoded
final = blocks_to_text(decoded)
print "Decrypted Message: "; puts final

```

{{out}}

```txt

% echo "☆ rosettacode.org ✓" | ./rsa.rb
Message: Numbers: [58008, 34336, 29295, 29541, 29812, 24931, 28516, 25902, 28530, 26400, 58012, 37642]
Encrypted as: [8509, 99626, 21784, 139807, 81066, 67678, 183438, 147659, 261822, 85962, 150227, 167121]
Decrypted to: [58008, 34336, 29295, 29541, 29812, 24931, 28516, 25902, 28530, 26400, 58012, 37642]
Decrypted Message: ☆ rosettacode.org ✓

```



## Scala

The code below demonstrates RSA encryption and decryption in Scala. Text to integer encryption using ASCII code.

```Scala

object RSA_saket{
    val d = BigInt("5617843187844953170308463622230283376298685")
    val n = BigInt("9516311845790656153499716760847001433441357")
    val e = 65537
    val text = "Rosetta Code"
    val encode = (msg:BigInt) => pow_mod(msg,e,n)
    val decode = (msg:BigInt) => pow_mod(msg,d,n)
    val getmsg = (txt:String) => BigInt(txt.map(x => "%03d".format(x.toInt)).reduceLeft(_+_))
    def pow_mod(p:BigInt, q:BigInt, n:BigInt):BigInt = {
        if(q==0)            BigInt(1)
        else if(q==1)       p
        else if(q%2 == 1)   pow_mod(p,q-1,n)*p % n
        else                pow_mod(p*p % n,q/2,n)    
    }
    def gettxt(num:String) = {
        if(num.size%3==2)
            ("0" + num).grouped(3).toList.foldLeft("")(_ + _.toInt.toChar)
        else
            num.grouped(3).toList.foldLeft("")(_ + _.toInt.toChar)
    }
    def main(args: Array[String]): Unit = {
        println(f"Original String \t: "+text)
        val msg = getmsg(text)
        println(f"Converted Signal \t: "+msg)
        val enc_sig = encode(msg) 
        println("Encoded Signal \t\t: "+ enc_sig)
        val dec_sig = decode(enc_sig)
        println("Decoded String \t\t: "+ dec_sig)
        val rec_msg = gettxt(dec_sig.toString)
        println("Retrieved Signal \t: "+rec_msg)
    }
}

```

{{out}}


```txt

    Ouput:
Original String         : Rosetta Code
Converted String        : 82111115101116116097032067111100101
Encoded Signal          : 5902559240035849005218240192859088445397686
Decoded String          : 82111115101116116097032067111100101
Retrieved String        : Rosetta Code

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";
  include "bytedata.s7i";

const proc: main is func
  local
    const string: plainText is "Rosetta Code";
    # Use a key big enough to hold 16 bytes of plain text in a single block.
    const bigInteger: modulus is 9516311845790656153499716760847001433441357_;
    const bigInteger: encode is 65537_;
    const bigInteger: decode is 5617843187844953170308463622230283376298685_;
    var bigInteger: plainTextNumber is 0_;
    var bigInteger: encodedNumber is 0_;
    var bigInteger: decodedNumber is 0_;
    var string: decodedText is "";
  begin
    writeln("Plain text:             " <& plainText);
    plainTextNumber := bytes2BigInt(plainText, UNSIGNED, BE);
    if plainTextNumber >= modulus then
      writeln("Plain text message too long");
    else
      writeln("Plain text as a number: " <& plainTextNumber);
      encodedNumber := modPow(plainTextNumber, encode, modulus);
      writeln("Encoded:                " <& encodedNumber);
      decodedNumber := modPow(encodedNumber, decode, modulus);
      writeln("Decoded:                " <& decodedNumber);
      decodedText := bytes(decodedNumber, UNSIGNED, BE);
      writeln("Decoded number as text: " <& decodedText);
    end if;
  end func;
```


{{out}}

```txt

Plain text:             Rosetta Code
Plain text as a number: 25512506514985639724585018469
Encoded:                916709442744356653386978770799029131264344
Decoded:                25512506514985639724585018469
Decoded number as text: Rosetta Code

```



## Sidef

{{trans|Perl 6}}

```ruby
const n = 9516311845790656153499716760847001433441357
const e = 65537
const d = 5617843187844953170308463622230283376298685

module Message {
    var alphabet = [('A' .. 'Z')..., ' ']
    var rad = alphabet.len
    var code = Hash(^rad -> map {|i| (alphabet[i], i) }...)
    func encode(String t) {
        [code{t.reverse.chars...}] ~Z* t.len.range.map { |i| rad**i } -> sum(0)
    }
    func decode(Number n) {
        ''.join(alphabet[
            gather {
                loop {
                    var (d, m) = n.divmod(rad)
                    take(m)
                    break if (n < rad)
                    n = d
                }
            }...]
        ).reverse
    }
}

var secret_message = "ROSETTA CODE"
say "Secret message is #{secret_message}"

var numeric_message = Message::encode(secret_message)
say "Secret message in integer form is #{numeric_message}"

var numeric_cipher = expmod(numeric_message, e, n)
say "After exponentiation with public exponent we get: #{numeric_cipher}"

var text_cipher = Message::decode(numeric_cipher)
say "This turns into the string #{text_cipher}"

var numeric_cipher2 = Message::encode(text_cipher)
say "If we re-encode it in integer form we get #{numeric_cipher2}"

var numeric_message2 = expmod(numeric_cipher2, d, n)
say "After exponentiation with SECRET exponent we get: #{numeric_message2}"

var secret_message2 = Message::decode(numeric_message2)
say "This turns into the string #{secret_message2}"
```

{{out}}

```txt

Secret message is ROSETTA CODE
Secret message in integer form is 97525102075211938
After exponentiation with public exponent we get: 8326171774113983822045243488956318758396426
This turns into the string ZULYDCEZOWTFXFRRNLIMGNUPHVCJSX
If we re-encode it in integer form we get 8326171774113983822045243488956318758396426
After exponentiation with SECRET exponent we get: 97525102075211938
This turns into the string ROSETTA CODE

```



## Tcl

This code is careful to avoid the assumption that the input string is in a single-byte encoding, instead forcing the encryption to be performed on the UTF-8 form of the text. <!-- NB: Doesn't print the intermediate encoded value; see talk page for discussion why. -->

```tcl
package require Tcl 8.5

# This is a straight-forward square-and-multiply implementation that relies on
# Tcl 8.5's bignum support (based on LibTomMath) for speed.
proc modexp {b expAndMod} {
    lassign $expAndMod -> e n
    if {$b >= $n} {puts stderr "WARNING: modulus too small"}
    for {set r 1} {$e != 0} {set e [expr {$e >> 1}]} {
	if {$e & 1} {
	    set r [expr {($r * $b) % $n}]
	}
	set b [expr {($b ** 2) % $n}]
    }
    return $r
}

# Assumes that messages are shorter than the modulus
proc rsa_encrypt {message publicKey} {
    if {[lindex $publicKey 0] ne "publicKey"} {error "key handling"}
    set toEnc 0
    foreach char [split [encoding convertto utf-8 $message] ""] {
	set toEnc [expr {$toEnc * 256 + [scan $char "%c"]}]
    }
    return [modexp $toEnc $publicKey]
}

proc rsa_decrypt {encrypted privateKey} {
    if {[lindex $privateKey 0] ne "privateKey"} {error "key handling"}
    set toDec [modexp $encrypted $privateKey]
    for {set message ""} {$toDec > 0} {set toDec [expr {$toDec >> 8}]} {
	append message [format "%c" [expr {$toDec & 255}]]
    }
    return [encoding convertfrom utf-8 [string reverse $message]]
}

# Assemble packaged public and private keys
set e 65537
set n 9516311845790656153499716760847001433441357
set d 5617843187844953170308463622230283376298685
set publicKey  [list "publicKey"  $e $n]
set privateKey [list "privateKey" $d $n]

# Test on some input strings
foreach input {"Rosetta Code" "UTF-8 \u263a test"} {
    set enc [rsa_encrypt $input $publicKey]
    set dec [rsa_decrypt $enc $privateKey]
    puts "$input -> $enc -> $dec"
}
```

Output:

```txt

Rosetta Code -> 916709442744356653386978770799029131264344 -> Rosetta Code
UTF-8 ☺ test -> 3905697186829810541171404594906488782823186 -> UTF-8 ☺ test

```



## Visual Basic .NET

{{trans|C#}}
{{libheader|System.Numerics}}

```vbnet
Imports System
Imports System.Numerics
Imports System.Text

Module Module1
    Sub Main()
        Dim n As BigInteger = BigInteger.Parse("9516311845790656153499716760847001433441357")
        Dim e As BigInteger = 65537
        Dim d As BigInteger = BigInteger.Parse("5617843187844953170308463622230283376298685")
        Dim plainTextStr As String = "Hello, Rosetta!"
        Dim plainTextBA As Byte() = ASCIIEncoding.ASCII.GetBytes(plainTextStr)
        Dim pt As BigInteger = New BigInteger(plainTextBA)
        If pt > n Then Throw New Exception() ' Blocking not implemented
        Dim ct As BigInteger = BigInteger.ModPow(pt, e, n)
        Console.WriteLine(" Encoded: " & ct.ToString("X"))
        Dim dc As BigInteger = BigInteger.ModPow(ct, d, n)
        Console.WriteLine(" Decoded: " & dc.ToString("X"))
        Dim decoded As String = ASCIIEncoding.ASCII.GetString(dc.ToByteArray())
        Console.WriteLine("As ASCII: " & decoded)
    End Sub
End Module
```

{{out}}

```txt
 Encoded: 6219A470D8B319A31C8E13F612B31337098F
 Decoded: 2161747465736F52202C6F6C6C6548
As ASCII: Hello, Rosetta!
```



## zkl

{{trans|C}}
{{libheader|GMP}}
No blocking.

```zkl
var BN=Import.lib("zklBigNum");

n:=BN("9516311845790656153499716760847001433441357");
e:=BN("65537");
d:=BN("5617843187844953170308463622230283376298685");

const plaintext="Rossetta Code";
pt:=BN(Data(Int,0,plaintext));  // convert string (as stream of bytes) to big int
if(pt>n) throw(Exception.ValueError("Message is too large"));
 
println("Plain text: ",plaintext);
println("As Int:     ",pt);
ct:=pt.powm(e,n);  println("Encoded:    ",ct);
pt =ct.powm(d,n);  println("Decoded:    ",pt);
txt:=pt.toData().text; // convert big int to bytes, treat as string
println("As String:  ",txt);
```

{{out}}

```txt

Plain text: Rossetta Code
As Int:     6531201733672758787904906421349
Encoded:    5278143020249600501803788468419399384934220
Decoded:    6531201733672758787904906421349
As String:  Rossetta Code

```

