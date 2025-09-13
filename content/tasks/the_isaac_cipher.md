+++
title = "The ISAAC Cipher"
description = ""
date = 2019-04-08T16:03:11Z
aliases = []
[extra]
id = 17790
[taxonomies]
categories = ["task", "Encryption"]
tags = []
+++

ISAAC is a cryptographically secure pseudo-random number generator (CSPRNG) and stream cipher. It was developed by Bob Jenkins from 1993 (http://burtleburtle.net/bob/rand/isaac.html) and placed in the Public Domain. ISAAC is fast - especially when optimised - and portable to most architectures in nearly all programming and scripting languages.
It is also simple and succinct, using as it does just two 256-word arrays for its state.

ISAAC stands for "Indirection, Shift, Accumulate, Add, and Count" which are the principal bitwise operations employed.
To date - and that's after more than 20 years of existence - ISAAC has not been broken (unless GCHQ or NSA did it, but they wouldn't be telling).
ISAAC thus deserves a lot more attention than it has hitherto received and it would be salutary to see it more universally implemented.


## Task

Translate ISAAC's reference C or Pascal code into your language of choice.

The RNG should then be seeded with the string "this is my secret key" and
finally the message "a Top Secret secret" should be encrypted on that key.
Your program's output cipher-text will be a string of hexadecimal digits.

Optional: Include a decryption check by re-initializing ISAAC and performing
the same encryption pass on the cipher-text.

Please use the C or Pascal as a reference guide to these operations.

Two encryption schemes are possible:
(1) XOR (Vernam) or
(2) Caesar-shift mod 95 (Vigenère).
XOR is the simplest; C-shifting offers greater security.

You may choose either scheme, or both, but please specify which you used.
Here are the alternative sample outputs for checking purposes:


```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
MOD    : 734270227D36772A783B4F2A5F206266236978
XOR dcr: a Top Secret secret
MOD dcr: a Top Secret secret

```


No official seeding method for ISAAC has been published, but for this task
we may as well just inject the bytes of our key into the randrsl array,
padding with zeroes before mixing, like so:


```txt

// zeroise mm array
FOR i:= 0 TO 255 DO mm[i]:=0;
// check seed's highest array element
m := High(seed);
// inject the seed
FOR i:= 0 TO 255 DO BEGIN
	// in case seed[] has less than 256 elements.
	IF i>m THEN randrsl[i]:=0
		ELSE randrsl[i]:=seed[i];
END;
// initialize ISAAC with seed
RandInit(true);

```


ISAAC can of course also be initialized with a single 32-bit unsigned integer in the manner of traditional RNGs, and indeed used as such for research and gaming purposes.
But building a strong and simple ISAAC-based stream cipher - replacing the irreparably broken RC4 - is our goal here: ISAAC's intended purpose.





## C

At the top is Bob Jenkins' reference code for ISAAC.
Below and in main() is the task's complete solution for XOR and MOD.

```C

/* Known to compile and work with tcc in win32 & gcc on Linux (with warnings)
------------------------------------------------------------------------------
readable.c: My random number generator, ISAAC.
(c) Bob Jenkins, March 1996, Public Domain
You may use this code in any way you wish, and it is free.  No warrantee.
------------------------------------------------------------------------------
*/
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#ifdef _MSC_VER
  typedef unsigned __int32 uint32_t;
#else
  #include <stdint.h>
#endif

/* a ub4 is an unsigned 4-byte quantity */
typedef  uint32_t  ub4;

/* external results */
ub4 randrsl[256], randcnt;

/* internal state */
static    ub4 mm[256];
static    ub4 aa=0, bb=0, cc=0;

void isaac()
{
   register ub4 i,x,y;

   cc = cc + 1;    /* cc just gets incremented once per 256 results */
   bb = bb + cc;   /* then combined with bb */

   for (i=0; i<256; ++i)
   {
     x = mm[i];
     switch (i%4)
     {
     case 0: aa = aa^(aa<<13); break;
     case 1: aa = aa^(aa>>6); break;
     case 2: aa = aa^(aa<<2); break;
     case 3: aa = aa^(aa>>16); break;
     }
     aa              = mm[(i+128)%256] + aa;
     mm[i]      = y  = mm[(x>>2)%256] + aa + bb;
     randrsl[i] = bb = mm[(y>>10)%256] + x;
   }
   // not in original readable.c
   randcnt = 0;
}

/* if (flag!=0), then use the contents of randrsl[] to initialize mm[]. */
#define mix(a,b,c,d,e,f,g,h) \
{ \
   a^=b<<11; d+=a; b+=c; \
   b^=c>>2;  e+=b; c+=d; \
   c^=d<<8;  f+=c; d+=e; \
   d^=e>>16; g+=d; e+=f; \
   e^=f<<10; h+=e; f+=g; \
   f^=g>>4;  a+=f; g+=h; \
   g^=h<<8;  b+=g; h+=a; \
   h^=a>>9;  c+=h; a+=b; \
}

void randinit(int flag)
{
   register int i;
   ub4 a,b,c,d,e,f,g,h;
   aa=bb=cc=0;
   a=b=c=d=e=f=g=h=0x9e3779b9;  /* the golden ratio */

   for (i=0; i<4; ++i)          /* scramble it */
   {
     mix(a,b,c,d,e,f,g,h);
   }

   for (i=0; i<256; i+=8)   /* fill in mm[] with messy stuff */
   {
     if (flag)                  /* use all the information in the seed */
	 {
       a+=randrsl[i  ]; b+=randrsl[i+1]; c+=randrsl[i+2]; d+=randrsl[i+3];
       e+=randrsl[i+4]; f+=randrsl[i+5]; g+=randrsl[i+6]; h+=randrsl[i+7];
     }
     mix(a,b,c,d,e,f,g,h);
     mm[i  ]=a; mm[i+1]=b; mm[i+2]=c; mm[i+3]=d;
     mm[i+4]=e; mm[i+5]=f; mm[i+6]=g; mm[i+7]=h;
   }

   if (flag)
   {        /* do a second pass to make all of the seed affect all of mm */
	 for (i=0; i<256; i+=8)
     {
       a+=mm[i  ]; b+=mm[i+1]; c+=mm[i+2]; d+=mm[i+3];
       e+=mm[i+4]; f+=mm[i+5]; g+=mm[i+6]; h+=mm[i+7];
       mix(a,b,c,d,e,f,g,h);
       mm[i  ]=a; mm[i+1]=b; mm[i+2]=c; mm[i+3]=d;
       mm[i+4]=e; mm[i+5]=f; mm[i+6]=g; mm[i+7]=h;
     }
   }

   isaac();            /* fill in the first set of results */
   randcnt=0;        /* prepare to use the first set of results */
}


// Get a random 32-bit value 0..MAXINT
ub4 iRandom()
{
	ub4 r = randrsl[randcnt];
	++randcnt;
	if (randcnt >255) {
		isaac();
		randcnt = 0;
	}
	return r;
}


// Get a random character in printable ASCII range
char iRandA()
{
	return iRandom() % 95 + 32;
}


// Seed ISAAC with a string
void iSeed(char *seed, int flag)
{
	register ub4 i,m;
	for (i=0; i<256; i++) mm[i]=0;
	m = strlen(seed);
	for (i=0; i<256; i++)
	{
	// in case seed has less than 256 elements
        if (i>m) randrsl[i]=0;  else randrsl[i] = seed[i];
	}
	// initialize ISAAC with seed
	randinit(flag);
}


// maximum length of message
#define MAXMSG 4096
#define MOD 95
#define START 32
// cipher modes for Caesar
enum ciphermode {
	mEncipher, mDecipher, mNone
};


// XOR cipher on random stream. Output: ASCII string
char v[MAXMSG];
char* Vernam(char *msg)
	{
		register ub4 i,l;
		l = strlen(msg);
		// zeroise v
		memset(v,'\0',l+1);
		// XOR message
		for (i=0; i<l; i++)
			v[i] = iRandA() ^ msg[i];
		return v;
	}


// Caesar-shift a printable character
char Caesar(enum ciphermode m, char ch, char shift, char modulo, char start)
	{
		register int n;
		if (m == mDecipher) shift = -shift;
		n = (ch-start) + shift;
		n = n % modulo;
		if (n<0) n += modulo;
		return start+n;
	}

// Caesar-shift a string on a pseudo-random stream
char c[MAXMSG];
char* CaesarStr(enum ciphermode m, char *msg, char modulo, char start)
	{
		register ub4 i,l;
		l = strlen(msg);
		// zeroise c
		memset(c,'\0',l+1);
		// Caesar-shift message
		for (i=0; i<l; i++)
			c[i] = Caesar(m, msg[i], iRandA(), modulo, start);
		return c;
	}


int main()
{
	register ub4 n,l;
	// input: message and key
	char *msg = "a Top Secret secret";
	char *key = "this is my secret key";
	// Vernam ciphertext & plaintext
	char vctx[MAXMSG], vptx[MAXMSG];
	// Caesar ciphertext & plaintext
	char cctx[MAXMSG], cptx[MAXMSG];
	l = strlen(msg);
	// Encrypt: Vernam XOR
	iSeed(key,1);
	strcpy(vctx, Vernam(msg));
	// Encrypt: Caesar
	strcpy(cctx, CaesarStr(mEncipher, msg, MOD, START));
	// Decrypt: Vernam XOR
	iSeed(key,1);
	strcpy(vptx, Vernam(vctx));
	// Decrypt: Caesar
	strcpy(cptx, CaesarStr(mDecipher,cctx, MOD, START));
	// Program output
	printf("Message: %s\n",msg);
	printf("Key    : %s\n",key);
	printf("XOR    : ");
	// Output Vernam ciphertext as a string of hex digits
	for (n=0; n<l; n++) printf("%02X",vctx[n]);
	printf("\n");
	// Output Vernam decrypted plaintext
	printf("XOR dcr: %s\n",vptx);
	// Caesar
	printf("MOD    : ");
	// Output Caesar ciphertext as a string of hex digits
	for (n=0; n<l; n++) printf("%02X",cctx[n]);
	printf("\n");
	// Output Caesar decrypted plaintext
	printf("MOD dcr: %s\n",cptx);
	return 0;
}

```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr: a Top Secret secret
MOD    : 734270227D36772A783B4F2A5F206266236978
MOD dcr: a Top Secret secret

```



## C++

```cpp

#include <iomanip>
#include <iostream>
#include <sstream>
using namespace std;

enum CipherMode {ENCRYPT, DECRYPT};

// External results
uint32_t randRsl[256];
uint32_t randCnt;

// Internal state
uint32_t mm[256];
uint32_t aa = 0, bb = 0, cc = 0;

void isaac()
{
    ++cc;  // cc just gets incremented once per 256 results
    bb += cc; // then combined with bb

    for (uint32_t i = 0; i < 256; ++i)
    {
        uint32_t x, y;

        x = mm[i];
        switch (i % 4)
        {
            case 0:
                aa = aa ^ (aa << 13);
                break;
            case 1:
                aa = aa ^ (aa >> 6);
                break;
            case 2:
                aa = aa ^ (aa << 2);
                break;
            case 3:
                aa = aa ^ (aa >> 16);
                break;
        }
        aa = mm[(i + 128) % 256] + aa;
        y = mm[(x >> 2) % 256] + aa + bb;
        mm[i] = y;
        bb = mm[(y >> 10) % 256] + x;
        randRsl[i] = bb;
    }
    randCnt = 0; // Prepare to use the first set of results.
}

void mix(uint32_t a[])
{
    a[0] = a[0] ^ a[1] << 11; a[3] += a[0]; a[1] += a[2];
    a[1] = a[1] ^ a[2] >>  2; a[4] += a[1]; a[2] += a[3];
    a[2] = a[2] ^ a[3] <<  8; a[5] += a[2]; a[3] += a[4];
    a[3] = a[3] ^ a[4] >> 16; a[6] += a[3]; a[4] += a[5];
    a[4] = a[4] ^ a[5] << 10; a[7] += a[4]; a[5] += a[6];
    a[5] = a[5] ^ a[6] >>  4; a[0] += a[5]; a[6] += a[7];
    a[6] = a[6] ^ a[7] <<  8; a[1] += a[6]; a[7] += a[0];
    a[7] = a[7] ^ a[0] >>  9; a[2] += a[7]; a[0] += a[1];
}

void randInit(bool flag)
{
    uint32_t a[8];
    aa = bb = cc = 0;

    a[0] = 2654435769UL; // 0x9e3779b9: the golden ratio
    for (uint32_t j = 1; j < 8; ++j)
        a[j] = a[0];

    for (uint32_t i = 0; i < 4; ++i) // Scramble it.
        mix(a);
    for (uint32_t i = 0; i < 256; i += 8) // Fill in mm[] with messy stuff.
    {
        if (flag) // Use all the information in the seed.
            for (uint32_t j = 0; j < 8; ++j)
                a[j] += randRsl[i + j];
        mix(a);
        for (uint32_t j = 0; j < 8; ++j)
            mm[i + j] = a[j];
    }

    if (flag)
    {   // Do a second pass to make all of the seed affect all of mm.
        for (uint32_t i = 0; i < 256; i += 8)
        {
            for (uint32_t j = 0; j < 8; ++j)
                a[j] += mm[i + j];
            mix(a);
            for (uint32_t j = 0; j < 8; ++j)
                mm[i + j] = a[j];
        }
    }
    isaac(); // Fill in the first set of results.
    randCnt = 0; // Prepare to use the first set of results.
}

// Seed ISAAC with a given string.
// The string can be any size. The first 256 values will be used.
void seedIsaac(string seed, bool flag)
{
    uint32_t seedLength = seed.length();
    for (uint32_t i = 0; i < 256; i++)
        mm[i] = 0;
    for (uint32_t i = 0; i < 256; i++)
        // In case seed has less than 256 elements
        randRsl[i] = i > seedLength ? 0 : seed[i];
    // Initialize ISAAC with seed
    randInit(flag);
}

// Get a random 32-bit value 0..MAXINT
uint32_t getRandom32Bit()
{
    uint32_t result = randRsl[randCnt];
    ++randCnt;
    if (randCnt > 255)
    {
        isaac();
        randCnt = 0;
    }
    return result;
}

// Get a random character in printable ASCII range
char getRandomChar()
{
    return getRandom32Bit() % 95 + 32;
}

// Convert an ASCII string to a hexadecimal string.
string ascii2hex(string source)
{
    uint32_t sourceLength = source.length();
    stringstream ss;
    for (uint32_t i = 0; i < sourceLength; i++)
        ss << setfill ('0') << setw(2) << hex << (int) source[i];
    return ss.str();
}

// XOR encrypt on random stream.
string vernam(string msg)
{
    uint32_t msgLength = msg.length();
    string destination = msg;
    for (uint32_t i = 0; i < msgLength; i++)
        destination[i] = getRandomChar() ^ msg[i];
    return destination;
}

// Caesar-shift a character <shift> places: Generalized Vigenere
char caesar(CipherMode m, char ch, char shift, char modulo, char start)
{
    int n;
    if (m == DECRYPT)
        shift = -shift;
    n = (ch - start) + shift;
    n %= modulo;
    if (n < 0)
        n += modulo;
    return start + n;
}

// Vigenere mod 95 encryption & decryption.
string vigenere(string msg, CipherMode m)
{
    uint32_t msgLength = msg.length();
    string destination = msg;
    // Caesar-shift message
    for (uint32_t i = 0; i < msgLength; ++i)
        destination[i] = caesar(m, msg[i], getRandomChar(), 95, ' ');
    return destination;
}

int main()
{
    // TASK globals
    string msg = "a Top Secret secret";
    string key = "this is my secret key";
    string xorCipherText, modCipherText, xorPlainText, modPlainText;

    // (1) Seed ISAAC with the key
    seedIsaac(key, true);
    // (2) Encryption
    // (a) XOR (Vernam)
    xorCipherText = vernam(msg);
    // (b) MOD (Vigenere)
    modCipherText = vigenere(msg, ENCRYPT);
    // (3) Decryption
    seedIsaac(key, true);
    // (a) XOR (Vernam)
    xorPlainText = vernam(xorCipherText);
    // (b) MOD (Vigenere)
    modPlainText = vigenere(modCipherText, DECRYPT);
    // Program output
    cout << "Message: " << msg << endl;
    cout << "Key    : " << key << endl;
    cout << "XOR    : " << ascii2hex(xorCipherText) << endl;
    cout << "MOD    : " << ascii2hex(modCipherText) << endl;
    cout << "XOR dcr: " << xorPlainText << endl;
    cout << "MOD dcr: " << modPlainText << endl;
}

```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1c0636190b1260233b35125f1e1d0e2f4c5422
MOD    : 734270227d36772a783b4f2a5f206266236978
XOR dcr: a Top Secret secret
MOD dcr: a Top Secret secret

```



## C#

XOR with decryption check.

```C sharp

using System;

namespace cipher {

static class Cipher {

// external results
static uint[] randrsl = new uint[256];
static uint randcnt;
// internal state
static uint[] mm = new uint[256];
static uint aa=0, bb=0, cc=0;


static void isaac() {
   uint i,x,y;
   cc++;    // cc just gets incremented once per 256 results
   bb+=cc;   // then combined with bb

   for (i=0; i<=255; i++) {
     x = mm[i];
     switch (i & 3) {
      case 0: aa = aa ^ (aa << 13); break;
      case 1: aa = aa ^ (aa >> 6); break;
      case 2: aa = aa ^ (aa << 2); break;
      case 3: aa = aa ^ (aa >> 16); break;
     }
     aa = mm[(i+128) & 255] + aa;
     y  = mm[(x >> 2) & 255] + aa + bb;
     mm[i] = y;
     bb = mm[(y >> 10) & 255] + x;
     randrsl[i]= bb;
   }
}


// if (flag==TRUE), then use the contents of randrsl[] to initialize mm[].
static void mix(ref uint a, ref uint b, ref uint c, ref uint d, ref uint e, ref uint f, ref uint g, ref uint h) {
   a = a ^ b << 11; d+=a; b+=c;
   b = b ^ c >> 2;  e+=b; c+=d;
   c = c ^ d << 8;  f+=c; d+=e;
   d = d ^ e >> 16; g+=d; e+=f;
   e = e ^ f << 10; h+=e; f+=g;
   f = f ^ g >> 4;  a+=f; g+=h;
   g = g ^ h << 8;  b+=g; h+=a;
   h = h ^ a >> 9;  c+=h; a+=b;
}


static void Init(bool flag) {
  short i; uint a,b,c,d,e,f,g,h;

   aa=0; bb=0; cc=0;
   a=0x9e3779b9; b=a; c=a; d=a;
   e=a; f=a; g=a; h=a;

   for (i=0; i<=3; i++)           // scramble it
        mix(ref a,ref b,ref c,ref d,ref e,ref f,ref g,ref h);

   i=0;
   do  { // fill in mm[] with messy stuff
          if (flag) {     // use all the information in the seed
            a+=randrsl[i  ]; b+=randrsl[i+1]; c+=randrsl[i+2]; d+=randrsl[i+3];
            e+=randrsl[i+4]; f+=randrsl[i+5]; g+=randrsl[i+6]; h+=randrsl[i+7];
          } // if flag

      mix(ref a,ref b,ref c,ref d,ref e,ref f,ref g,ref h);
      mm[i  ]=a; mm[i+1]=b; mm[i+2]=c; mm[i+3]=d;
      mm[i+4]=e; mm[i+5]=f; mm[i+6]=g; mm[i+7]=h;
      i+=8;
      }
   while (i<255);

   if (flag) {
   // do a second pass to make all of the seed affect all of mm
     i=0;
     do {
      a+=mm[i  ]; b+=mm[i+1]; c+=mm[i+2]; d+=mm[i+3];
      e+=mm[i+4]; f+=mm[i+5]; g+=mm[i+6]; h+=mm[i+7];
      mix(ref a,ref b,ref c,ref d,ref e,ref f,ref g,ref h);
      mm[i  ]=a; mm[i+1]=b; mm[i+2]=c; mm[i+3]=d;
      mm[i+4]=e; mm[i+5]=f; mm[i+6]=g; mm[i+7]=h;
      i+=8;
        }
     while (i<255);
   }
   isaac();           // fill in the first set of results
   randcnt=0;       // prepare to use the first set of results
}


// Seed ISAAC with a string
static void Seed(string seed, bool flag) {
	for (int i=0; i<256; i++) mm[i]=0;
	for (int i=0; i<256; i++) randrsl[i]=0;
	int m = seed.Length;
	for (int i=0; i<m; i++) {
        randrsl[i] = seed[i];
	}
	// initialize ISAAC with seed
	Init(flag);
}


// Get a random 32-bit value
static uint Random() {
    uint result = randrsl[randcnt];
    randcnt++;
    if (randcnt>255) {
         isaac(); randcnt=0;
    }
    return result;
}


// Get a random character in printable ASCII range
static byte RandA() {
	return (byte)(Random() % 95 + 32);
}


// XOR encrypt on random stream. Output: ASCII byte array
static byte[] Vernam(string msg)
	{
		int n,l;
		byte[] v = new byte[msg.Length];
		l = msg.Length;
		// XOR message
		for (n=0; n<l; n++) {
			v[n] = (byte) (RandA() ^ (byte)msg[n]);
		}
		return v;
	}


	public static void Main() {
		string msg = "a Top Secret secret";
		string key = "this is my secret key";
		byte[] xctx= new byte[msg.Length];
		byte[] xptx= new byte[msg.Length];
		string xtcx= "*******************";
		string xtpx= "*******************";
		Seed(key,true);
		// XOR encrypt
		xctx = Vernam(msg);
		xtcx = System.Text.Encoding.ASCII.GetString(xctx);
		// XOR decrypt
		Seed(key,true);
		xptx = Vernam(xtcx);
		xtpx = System.Text.Encoding.ASCII.GetString(xptx);
		Console.WriteLine("Message: "+msg);
		Console.WriteLine("Key    : "+key);
		Console.Write    ("XOR    : ");
		// output ciphertext as a string of hexadecimal digits
		for (int n=0; n<xctx.Length; n++) Console.Write("{0:X2}", xctx[n]);
		Console.WriteLine("\nXOR dcr: "+xtpx);
	}
}
}

```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr: a Top Secret secret

```



## Common Lisp


```lisp
(defpackage isaac
  (:use cl))

(in-package isaac)

(deftype uint32 () '(unsigned-byte 32))
(deftype arru32 () '(simple-array uint32))

(defstruct state
  (randrsl (make-array 256 :element-type 'uint32) :type arru32)
  (randcnt 0 :type uint32)
  (mm (make-array 256 :element-type 'uint32) :type arru32)
  (aa 0 :type uint32)
  (bb 0 :type uint32)
  (cc 0 :type uint32))

(defparameter *global-state* (make-state))

;; Some helper functions to force 32-bit arithmetic.
;; COERCE32 will be used to ensure the 32-bit results from
;; the given operations.
(declaim (inline lsh32 rsh32 add32 mod32 xor32))

(defmacro coerce32 (thing)
  `(ldb (byte 32 0) ,thing))

;; ASH is split into lsh32 and rsh32 to satisfy the compiler and
;; allow inlining.
(declaim (ftype (function (uint32 (unsigned-byte 6)) uint32) lsh32))
(defun lsh32 (integer count)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (coerce32 (ash integer count)))

(declaim (ftype (function (uint32 uint32) uint32) rsh32 add32 mod32 xor32))
(defun rsh32 (integer count)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (coerce32 (ash integer (- count))))

(defun add32 (x y)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (coerce32 (+ x y)))

(defun mod32 (number divisor)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (coerce32 (mod number divisor)))

(defun xor32 (x y)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (coerce32 (logxor x y)))

(defmacro incf32 (place &optional (delta 1))
  `(setf ,place (add32 ,place ,delta)))

(defun isaac (&optional (state *global-state*))
  "The ISAAC function."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type state state))
  (with-slots (randrsl randcnt mm aa bb cc) state
    (incf32 cc)
    (incf32 bb cc)
    (dotimes (i 256)
      (let ((x (aref mm i)))
        (setf aa (add32 (aref mm (mod32 (add32 i 128) 256))
                        (xor32 aa
                               (ecase (mod32 i 4)
                                 (0 (lsh32 aa 13))
                                 (1 (rsh32 aa 6))
                                 (2 (lsh32 aa 2))
                                 (3 (rsh32 aa 16))))))
        (let ((y (add32 (aref mm (mod32 (rsh32 x 2) 256))
                        (add32 aa
                               bb))))
          (setf (aref mm i) y)
          (setf bb (add32 (aref mm (mod32 (rsh32 y 10) 256))
                          x))
          (setf (aref randrsl i) bb))))
    (setf randcnt 0)
    (values)))

(defmacro mix (&rest places)
  "The magic mixer that spits out code to mix the given places."
  (let ((len (length places))
        (kernel '#0=(11 -2 8 -16 10 -4 8 -9 . #0#)))
    (rplacd (last places) places)
    `(progn
       ,@(loop
            for i from 0
            for n in kernel
            until (= i len)
            append
              (destructuring-bind (a b c d . rest) places
                (declare (ignore rest))
                (pop places)
                `((setf ,a (xor32 ,a ,(if (> n 0) `(lsh32 ,b ,n) `(rsh32 ,b ,(- n)))))
                  (incf32 ,d ,a)
                  (incf32 ,b ,c)))))))

(defun replace-tree (value replacement tree)
  "Replace all of the values in the given expression with the replacement."
  (if (atom tree)
      (if (equal tree value)
          replacement
          tree)
      (cons (replace-tree value replacement (car tree))
            (if (null (cdr tree))
                nil
                (replace-tree value replacement (cdr tree))))))

(defmacro unroller (index-name place-name places &body body)
  "A helper for unrolling a section of a loop's index with the given places."
  `(progn ,@(loop
               for place in places
               for i from 0 below (length places) append
                 `(,@(if (= i 0)
                         (replace-tree place-name place body)
                         (replace-tree index-name `(add32 ,index-name ,i)
                                       (replace-tree place-name place body)))))))

(defun randinit (flag &optional (state *global-state*))
  "Initialize the given state."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type state state))
  (with-slots (randrsl randcnt mm aa bb cc) state
    (let* ((a #x9e3779b9) (b a) (c a) (d a) (e a) (f a) (g a) (h a))
      (setf aa 0)
      (setf bb 0)
      (setf cc 0)
      (loop repeat 4 do
           (mix a b c d e f g h))
      (loop for idx from 0 below 256 by 8 do
           (when flag
             (unroller idx place (a b c d e f g h)
               (incf32 place (aref randrsl idx))))
           (mix a b c d e f g h)
           (unroller idx place (a b c d e f g h)
             (setf (aref mm idx) place)))
      (when flag
        (loop for idx from 0 below 256 by 8 do
             (unroller idx place (a b c d e f g h)
               (incf32 place (aref mm idx)))
             (mix a b c d e f g h)
             (unroller idx place (a b c d e f g h)
               (setf (aref mm idx) place)))))
    (isaac state)
    (setf randcnt 0)
    (values)))

(defun i-random (&optional (state *global-state*))
  "Get a random integer from the given state."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type state state))
  (with-slots (randrsl randcnt) state
    (prog1 (aref randrsl randcnt)
      (incf32 randcnt)
      (when (> randcnt 255)
        (isaac state)
        (setf randcnt 0)))))

(defun i-rand-a (&optional (state *global-state*))
  "Get a random printable character from the given state."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type state state))
  (add32 (mod32 (i-random state) 95) 32))

(defun i-seed (seed flag &optional (state *global-state*))
  "Seed the given state with a string of up to 256 characters."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type state state)
           (type string seed))
  (with-slots (randrsl mm) state
    (dotimes (i 256)
      (setf (aref mm i) 0))
    (let ((m (length seed)))
      (dotimes (i 256)
        (setf (aref randrsl i)
              (if (>= i m)
                  0
                  (char-code (char seed i))))))
    (randinit flag state)
    (values)))

(defun vernam (msg &optional (state *global-state*))
  "Vernam encode MSG with STATE."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type state state)
           (type string msg))
  (let* ((l (length msg))
         (v (make-string l)))
    (dotimes (i l)
      (setf (aref v i) (code-char (logxor (i-rand-a state) (char-code (char msg i))))))
    v))

;; Cipher modes: encipher, decipher, none
(defconstant +mod+ 95)
(defconstant +start+ 32)

(defun caesar (mode char shift modulo start)
  "Caesar encode the given character."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type uint32 char shift modulo start))
  (when (eq mode 'decipher)
    (setf shift (- shift)))
  (let ((n (mod (+ (- char start) shift) modulo)))
    (when (< n 0)
      (incf n modulo))
    (+ start n)))

(defun caesar-str (mode msg modulo start &optional (state *global-state*))
  "Caesar encode or decode MSG with STATE."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type string msg)
           (type fixnum modulo start)
           (type state state))
  (let* ((l (length msg))
         (c (make-string l)))
    (dotimes (i l)
      (setf (aref c i) (code-char (caesar mode (char-code (char msg i)) (i-rand-a state) modulo start))))
    c))

(defun print-hex (string)
  (loop for c across string do (format t "~2,'0x" (char-code c))))

(defun main-test ()
  (let ((state (make-state))
        (msg "a Top Secret secret")
        (key "this is my secret key"))
    (i-seed key t state)
    (let ((vctx (vernam msg state))
          (cctx (caesar-str 'encipher msg +mod+ +start+ state)))
      (i-seed key t state)
      (let ((vptx (vernam vctx state))
            (cptx (caesar-str 'decipher cctx +mod+ +start+ state)))
        (format t "Message: ~a~%" msg)
        (format t "Key    : ~a~%" key)
        (format t "XOR    : ")
        (print-hex vctx)
        (terpri)
        (format t "XOR dcr: ~a~%" vptx)
        (format t "MOD    : ")
        (print-hex cctx)
        (terpri)
        (format t "MOD dcr: ~a~%" cptx))))
  (values))
```

```txt
ISAAC> (main-test)
Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr: a Top Secret secret
MOD    : 734270227D36772A783B4F2A5F206266236978
MOD dcr: a Top Secret secret
```



## D

Improved from the C# version. XOR with decryption check.

```d
import std.algorithm: min;
import std.algorithm: copy;
import std.typetuple: TypeTuple;
import std.typecons: staticIota;

struct ISAAC {
    // External results.
    private uint[mm.length] randResult;
    private uint randCount;

    // Internal state.
    private uint[256] mm;
    private uint aa, bb, cc;


    private void isaac() pure nothrow @safe @nogc {
        cc++;         // cc just gets incremented once per mm.length results.
        bb = bb + cc; // Then combined with bb.

        foreach (immutable i, ref mmi; mm) {
            immutable x = mm[i];
            final switch (i % 4) { // Not enforced final switch.
                case 0: aa ^= (aa << 13); break;
                case 1: aa ^= (aa >>  6); break;
                case 2: aa ^= (aa <<  2); break;
                case 3: aa ^= (aa >> 16); break;
            }

            aa = mm[(i + 128) % $] + aa;
            immutable y = mm[(x >> 2) % $] + aa + bb;
            bb = mm[(y >> 10) % $] + x;
            randResult[i] = bb;
        }

        randCount = 0;
    }


    // If flag is true then use the contents of randResult to initialize mm.
    private pure nothrow @safe @nogc static void mix(ref uint[8] a)  {
        alias shifts = TypeTuple!(11, 2, 8, 16, 10, 4, 8, 9);
        /*static*/ foreach (immutable i, immutable sh; shifts) {
            static if (i % 2 == 0)
                a[i] ^= a[(i + 1) % $] << sh;
            else
                a[i] ^= a[(i + 1) % $] >> sh;
            a[(i + 3) % $] += a[i];
            a[(i + 1) % $] += a[(i + 2) % $];
        }
    }


    private void randInit(bool flag)() pure nothrow @safe @nogc {
        uint[8] a = 0x9E37_79B9; // The Golden Ratio.
        aa = bb = cc = 0;

        // Scramble it.
        /*static*/ foreach (immutable i; staticIota!(0, 4))
            mix(a);

        // Fill in mm with messy stuff. Use all the information in the seed.
        for (size_t i = 0; i < mm.length; i += 8) {
            static if (flag)
                a[] += randResult[i .. i + 8];
            mix(a);
            mm[i .. i + 8] = a[];
        }

        // Do a second pass to make all of the seed affect all of mm.
        static if (flag) {
            for (size_t i = 0; i < mm.length; i += 8) {
                a[] += mm[i .. i + 8];
                mix(a);
                mm[i .. i + 8] = a[];
            }
        }

        isaac();       // Fill in the first set of results.
        randCount = 0; // Prepare to use the first set of results.
    }


    /// Seed ISAAC with a string.
    /// Uses only the first randResult.length ubytes.
    public void iSeed(bool flag)(in ubyte[] seed) pure nothrow @safe @nogc {
        mm[] = 0;
        randResult[] = 0;

        immutable n = min(randResult.length, seed.length);
        copy(seed[0 .. n], randResult[0 .. n]);

        randInit!flag(); // Initialize ISAAC with seed.
    }


    /// Get a random uint.
    private uint iRandom() pure nothrow @safe @nogc {
        immutable result = randResult[randCount];

        randCount++;
        if (randCount > (randResult.length - 1)) {
            isaac();
            randCount = 0;
        }

        return result;
    }


    /// Get a random character in printable ASCII range.
    private ubyte iRandA() pure nothrow @safe @nogc {
        return iRandom() % 95 + 32;
    }


    /// XOR encrypt on random stream.
    /// buffer must be as large as message or larger.
    public ubyte[] vernam(in ubyte[] message, ubyte[] buffer)
    pure nothrow @safe @nogc
    in {
        assert(buffer.length >= message.length);
    } out(result) {
        assert(result.length == message.length);
    } body {
        auto v = buffer[0 .. message.length];

        // XOR message.
        foreach (immutable i, immutable msgi; message)
            v[i] = (iRandA() ^ msgi);
        return v;
    }


    /// XOR encrypt on random stream.
    public ubyte[] vernam(in ubyte[] message) pure nothrow @safe {
        return vernam(message, new ubyte[message.length]);
    }
}


void main() {
    import std.stdio, std.string;

    immutable message = "a Top Secret secret";
    immutable key = "this is my secret key";

    writeln("Message  : ", message);
    writeln("Key      : ", key);

    ISAAC cipher;

    // Encrypt.
    // iSeed uses only the first ISAAC.randResult.length ubytes.
    cipher.iSeed!true(key.representation);
    const encrypted = cipher.vernam(message.representation);

    // Output ciphertext as a string of hexadecimal digits.
    writefln("Encrypted: %(%02X%)", encrypted);

    // Decrypt.
    cipher.iSeed!true(key.representation);
    const decrypted = cipher.vernam(encrypted);

    writeln("Decrypted: ", decrypted.assumeUTF);
}
```

```txt
Message  : a Top Secret secret
Key      : this is my secret key
Encrypted: 1C0636190B1260233B35125F1E1D0E2F4C5422
Decrypted: a Top Secret secret
```



## Delphi

Translation of Pascal.

```Delphi

{$apptype console}
PROGRAM RosettaIsaac;
USES SysUtils;

// TASK globals
VAR msg : STRING = 'a Top Secret secret';
VAR key : STRING = 'this is my secret key';
VAR xctx: STRING = ''; // XOR ciphertext
VAR mctx: STRING = ''; // MOD ciphertext

// ISAAC globals
// external results
VAR randrsl: ARRAY[0..256] OF CARDINAL;
VAR randcnt: cardinal;
// internal state
VAR mm: ARRAY[0..256] OF CARDINAL;
VAR aa: CARDINAL=0; bb: CARDINAL=0; cc: CARDINAL=0;


PROCEDURE Isaac;
VAR i,x,y: CARDINAL;
BEGIN
   cc := cc + 1;    // cc just gets incremented once per 256 results
   bb := bb + cc;   // then combined with bb

   FOR i := 0 TO 255 DO BEGIN
     x := mm[i];
     CASE (i mod 4) OF
		0: aa := aa xor (aa shl 13);
		1: aa := aa xor (aa shr 6);
		2: aa := aa xor (aa shl 2);
		3: aa := aa xor (aa shr 16);
     END;
     aa := mm[(i+128) mod 256] + aa;
	 y  := mm[(x shr 2) mod 256] + aa + bb;
     mm[i] := y;
     bb := mm[(y shr 10) mod 256] + x;
     randrsl[i]:= bb;
   END;
   // this reset was not in original readable.c!
   randcnt:=0;  // prepare to use the first set of results
END; {Isaac}


// if (flag==TRUE), then use the contents of randrsl[] to initialize mm[].
PROCEDURE mix(VAR a,b,c,d,e,f,g,h: CARDINAL);
BEGIN
	a := a xor b shl 11; d:=d+a; b:=b+c;
	b := b xor c shr  2; e:=e+b; c:=c+d;
	c := c xor d shl  8; f:=f+c; d:=d+e;
	d := d xor e shr 16; g:=g+d; e:=e+f;
	e := e xor f shl 10; h:=h+e; f:=f+g;
	f := f xor g shr  4; a:=a+f; g:=g+h;
	g := g xor h shl  8; b:=b+g; h:=h+a;
	h := h xor a shr  9; c:=c+h; a:=a+b;
END; {mix}


PROCEDURE iRandInit(flag: BOOLEAN);
VAR i,a,b,c,d,e,f,g,h: CARDINAL;
BEGIN
   aa:=0; bb:=0; cc:=0;
   a:=$9e3779b9; 	// the golden ratio

   b:=a; c:=a; d:=a; e:=a; f:=a; g:=a; h:=a;

   FOR i := 0 TO 3 DO          // scramble it
        mix(a,b,c,d,e,f,g,h);

   i:=0;
   REPEAT  // fill in mm[] with messy stuff
	IF flag THEN BEGIN     // use all the information in the seed
       a:=a+randrsl[i  ]; b:=b+randrsl[i+1]; c:=c+randrsl[i+2]; d:=d+randrsl[i+3];
       e:=e+randrsl[i+4]; f:=f+randrsl[i+5]; g:=g+randrsl[i+6]; h:=h+randrsl[i+7];
    END;

    mix(a,b,c,d,e,f,g,h);
    mm[i  ]:=a; mm[i+1]:=b; mm[i+2]:=c; mm[i+3]:=d;
    mm[i+4]:=e; mm[i+5]:=f; mm[i+6]:=g; mm[i+7]:=h;
	i:=i+8;
   UNTIL i>255;

   IF (flag) THEN BEGIN
   // do a second pass to make all of the seed affect all of mm
     i:=0;
     REPEAT
      a:=a+mm[i  ]; b:=b+mm[i+1]; c:=c+mm[i+2]; d:=d+mm[i+3];
      e:=e+mm[i+4]; f:=f+mm[i+5]; g:=g+mm[i+6]; h:=h+mm[i+7];
      mix(a,b,c,d,e,f,g,h);
      mm[i  ]:=a; mm[i+1]:=b; mm[i+2]:=c; mm[i+3]:=d;
      mm[i+4]:=e; mm[i+5]:=f; mm[i+6]:=g; mm[i+7]:=h;
      i:=i+8;
     UNTIL i>255;
   END;
   isaac();           // fill in the first set of results
   randcnt:=0;       // prepare to use the first set of results
END; {randinit}


{ Seed ISAAC with a given string.
  The string can be any size. The first 256 values will be used.}
PROCEDURE iSeed(seed: STRING; flag: BOOLEAN);
VAR i,m: CARDINAL;
BEGIN
	FOR i:= 0 TO 255 DO mm[i]:=0;
	m := Length(seed)-1;
	FOR i:= 0 TO 255 DO BEGIN
	// in case seed has less than 256 elements
        IF i>m THEN randrsl[i]:=0
			// Pascal strings are 1-based
			ELSE randrsl[i]:=ord(seed[i+1]);
	END;
	// initialize ISAAC with seed
	iRandInit(flag);
END; {iSeed}


{ Get a random 32-bit value 0..MAXINT }
FUNCTION iRandom : Cardinal;
BEGIN
	result := randrsl[randcnt];
	inc(randcnt);
	IF (randcnt >255) THEN BEGIN
		Isaac();
		randcnt := 0;
	END;
END; {iRandom}


{ Get a random character in printable ASCII range }
FUNCTION iRandA: BYTE;
	BEGIN
		result := iRandom mod 95 + 32;
	END;


{ convert an ASCII string to a hexadecimal string }
FUNCTION ascii2hex(s: STRING): STRING;
	VAR i,l: CARDINAL;
	BEGIN
		result := '';
			l := Length(s);
			FOR i := 1 TO l DO
				result := result + IntToHex(ord(s[i]),2);
	END;


{ XOR encrypt on random stream. Output: string of hex chars }
FUNCTION Vernam(msg: STRING): STRING;
	VAR	i: CARDINAL;
	BEGIN
		result := '';
		FOR i := 1 to length(msg) DO
			result := result + chr(iRandA xor ord(msg[i]));
	result := ascii2hex(result);
	END;


{ Get position of the letter in chosen alphabet }
FUNCTION letternum(letter, start: CHAR): byte;
	BEGIN
		result := (ord(letter)-ord(start));
	END;


{ Caesar-shift a character <shift> places: Generalized Vigenere }
FUNCTION Caesar(ch: CHAR; shift, modulo: INTEGER; start: CHAR): CHAR;
	VAR n: INTEGER;
	BEGIN
		n := letternum(ch,start) + shift;
		n := n MOD modulo;
		result := chr(ord(start)+n);
	END;

{ Vigenere mod 95 encryption. Output: string of hex chars }
FUNCTION Vigenere(msg: STRING): STRING;
	VAR i: CARDINAL;
	BEGIN
		result := '';
		FOR i := 1 to length(msg) DO
			result := result + Caesar(msg[i],iRandA,95,' ');
		result := ascii2hex(result);
	END;


BEGIN
	// 1) seed ISAAC with the key
	iSeed(key,true);
	// 2) Vernam XOR encryption
	xctx := Vernam(msg);
	// 3) MOD encryption
	mctx := Vigenere(msg);
	// program output
	Writeln('Message: ',msg);
	Writeln('Key    : ',key);
	Writeln('XOR    : ',xctx);
	Writeln('MOD    : ',mctx);
END.

```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
MOD    : 734270227D36772A783B4F2A5F206266236978

```



## ECMAScript

```ecmascript
randrsl = new Uint32Array(256);
randcnt = 0;
mm = new Uint32Array(256);
aa = 0;
bb = 0;
cc = 0;

function isaac() {
        cc++;
        bb += cc;
        for(var i = 0; i < 256; i++) {
                var x = mm[i];
                var sw = i & 3;
                if(sw == 0) aa = aa ^ (aa << 13);
                else if(sw == 1) aa = aa ^ (aa >>> 6);
                else if(sw == 2) aa = aa ^ (aa << 2);
                else if(sw == 3) aa = aa ^ (aa >>> 16);
                aa = mm[(i+128) & 255] + aa;
                mm[i] = mm[(x >>> 2) & 255] + aa + bb;
                bb = mm[(mm[i] >>> 10) & 255] + x;
                randrsl[i] = bb;
        }
}

function isaac_mix(x) {
        x[0] = x[0] ^ x[1] << 11;  x[3]+=x[0]; x[1]+=x[2];
        x[1] = x[1] ^ x[2] >>> 2;  x[4]+=x[1]; x[2]+=x[3];
        x[2] = x[2] ^ x[3] << 8;   x[5]+=x[2]; x[3]+=x[4];
        x[3] = x[3] ^ x[4] >>> 16; x[6]+=x[3]; x[4]+=x[5];
        x[4] = x[4] ^ x[5] << 10;  x[7]+=x[4]; x[5]+=x[6];
        x[5] = x[5] ^ x[6] >>> 4;  x[0]+=x[5]; x[6]+=x[7];
        x[6] = x[6] ^ x[7] << 8;   x[1]+=x[6]; x[7]+=x[0];
        x[7] = x[7] ^ x[0] >>> 9;  x[2]+=x[7]; x[0]+=x[1];
}

function isaac_init(flag) {
        var x = Uint32Array([2654435769, 2654435769, 2654435769, 2654435769,
                             2654435769, 2654435769, 2654435769, 2654435769]);
        aa=0, bb=0, cc=0;
        isaac_mix(x); isaac_mix(x); isaac_mix(x); isaac_mix(x);
        var i = 0;
        while(i < 255) {
                if(flag) for(var j = 0; j < 8; j++) x[j] += randrsl[i+j];
                isaac_mix(x);
                for(var j = 0; j < 8; j++) mm[i+j] = x[j];
                i += 8;
        }
        if(flag) {
                var i = 0;
                while(i < 255) {
                        for(var j = 0; j < 8; j++) x[j] += mm[i+j];
                        isaac_mix(x);
                        for(var j = 0; j < 8; j++) mm[i+j] = x[j];
                        i += 8;
                }
        }
        isaac();
        randcnt = 0;
}

function isaac_seed(string, flag) {
        mm = new Uint32Array(256);
        randrsl = new Uint32Array(256);
        var m = string.length;
        for(var i = 0; i < m; i++) randrsl[i] = string.charCodeAt(i);
        isaac_init(flag);
}

function isaac_random() {
        var out = randrsl[randcnt++];
        if(randcnt > 255) {
                isaac();
                randcnt = 0;
        }
        return out
}

function vernam(msg) {
        var out = "";
        for(var i = 0; i < msg.length; i++) {
                var ra = isaac_random() % 95 + 32;
                out += String.fromCharCode(ra ^ msg.charCodeAt(i));
        }
        return out;
}

function printable_hex(s) {
        out = "";
        for(var i = 0; i < s.length; i++)
                out += (s.charCodeAt(i) / 16 > 1 ? ''  : '0') + s.charCodeAt(i).toString(16);
        return out;
}

function run_isaac(key, msg)
{
        isaac_seed(key, true);

        // XOR encrypt
        var xctx = vernam(msg);

        // XOR decrypt
        isaac_seed(key, true);
        var xptx = vernam(xctx);

        return [xctx, xptx]
}

var key = 'this is my secret key'
var msg = 'a Top Secret secret'
console.log('key: '+key)
console.log('msg: '+msg)
var z = run_isaac(key, msg)
xctx = z[0];
xptx = z[1];
console.log('xor: '+printable_hex(xctx))
console.log('decrypted: '+xptx)
```

```txt

key: this is my secret key
msg: a Top Secret secret
xor: 1c0636190b1260233b35125f1e1d0e2f4c5422
decrypted: a Top Secret secret

```


## FreeBASIC

```freebasic
' version 03-11-2016
' compile with: fbc -s console

Dim Shared As UInteger<32> randrsl(256), randcnt
Static Shared As UInteger<32> mm(256)
Static Shared As UInteger<32> aa, bb ,cc

Sub ISAAC()

    Dim As UInteger<32> i, x, y

    cc = cc + 1
    bb = bb + cc

    For i = 0 To 256 -1
        x = mm(i)
        Select Case (i Mod 4)
            Case 0 : aa = aa Xor (aa Shl 13)
            Case 1 : aa = aa Xor (aa Shr 6)
            Case 2 : aa = aa Xor (aa Shl 2)
            Case 3 : aa = aa Xor (aa Shr 16)
        End Select
        aa = mm((i+128) Mod 256) + aa
        y = mm((x Shr 2) Mod 256) + aa + bb : mm(i) = y
        bb = mm((y Shr 10) Mod 256) + x : randrsl(i) = bb
    Next

    randcnt = 0

End Sub


#Macro mix(a, b, c, d, e, f, g, h)

    a Xor= b Shl 11 : d += a : b += c
    b Xor= c Shr 2  : e += b : c += d
    c Xor= d Shl 8  : f += c : d += e
    d Xor= e Shr 16 : g += d : e += f
    e Xor= f Shl 10 : h += e : f += g
    f Xor= g Shr 4  : a += f : g += h
    g Xor= h Shl 8  : b += g : h += a
    h Xor= a Shr 9  : c += h : a += b

#EndMacro

Sub randinit(flag As Long)

    Dim As Long i
    Dim As UInteger<32> a = &H9e3779b9   '/* the golden ratio *
    Dim As UInteger<32> b = &H9e3779b9
    Dim As UInteger<32> c = &H9e3779b9
    Dim As UInteger<32> d = &H9e3779b9
    Dim As UInteger<32> e = &H9e3779b9
    Dim As UInteger<32> f = &H9e3779b9
    Dim As UInteger<32> g = &H9e3779b9
    Dim As UInteger<32> h = &H9e3779b9
    aa = 0 : bb = 0 : cc = 0

    For i = 0 To 3
        mix(a, b, c, d, e, f, g, h)
    Next

    For i = 0 To 255 Step 8
        If flag = 1 Then
            a += randrsl(i   ) : b += randrsl(i +1)
            c += randrsl(i +2) : d += randrsl(i +3)
            e += randrsl(i +4) : f += randrsl(i +5)
            g += randrsl(i +6) : h += randrsl(i +7)

            mix(a, b, c, d, e, f, g, h)
            mm(i   ) = a : mm(i +1) = b : mm(i +2) = c : mm(i +3) = d
            mm(i +4) = e : mm(i +5) = f : mm(i +6) = g : mm(i +7) = h
        End If
    Next

    If flag = 1 Then
        For i = 0 To 255 Step 8
            a += mm(i   ) : b += mm(i +1)
            c += mm(i +2) : d += mm(i +3)
            e += mm(i +4) : f += mm(i +5)
            g += mm(i +6) : h += mm(i +7)

            mix(a, b, c, d, e, f, g, h)
            mm(i   )= a : mm(i +1) = b : mm(i +2) = c : mm(i +3) = d
            mm(i +4)= e : mm(i +5) = f : mm(i +6) = g : mm(i +7) = h
        Next
    End If

    ISAAC()
    randcnt = 0

End Sub

' // Get a random 32-bit value 0..MAXINT
Function iRandom() As UInteger<32>

Dim As UInteger<32> r = randrsl(randcnt)
randcnt += 1
If randcnt > 255 Then
    ISAAC()
    randcnt = 0
End If

Return r

End Function

' // Get a random character in printable ASCII range
Function iRandA() As UByte

Return iRandom() Mod 95 +32

End Function

' // Seed ISAAC with a string
Sub iSeed(seed As String, flag As Long)

Dim As ULong i, m = Len(seed) -1

For i = 0 To 255
    mm(i) = 0
Next

For i = 0 To 255

    If i > m Then
        randrsl(i) = 0
    Else
        randrsl(i) = seed[i]
    End If

Next

randinit(flag)

End Sub

' // maximum length of message
'#define MAXMSG 4096
#Define _MOD_ 95      ' mod is FreeBASIC keyword
#Define _START_ 32    ' start is used as variable name

' // cipher modes for Caesar
Enum ciphermode
    mEncipher
    mDecipher
    mNone
End Enum

' // XOR cipher on random stream. Output: ASCII string
' no maximum lenght for input and output string
Function Vernam(msg As String) As String

Dim As ULong i
Dim As String v

For i = 0 To Len(msg) -1
    v += Chr(iRandA() Xor msg[i])
Next

Return v

End Function

' // Caesar-shift a printable character
Function Ceasar(m As ciphermode, ch As UByte, shift As UByte, modulo As UByte, _
                                                     start As UByte) As UByte

' FreeBASIC Mod does not handle negative numbers correctly
' also there is litte problem with shift (declared UByte)
' the IIF() statement helps with shift
' to avoid a negative n a 8 times modulo is added
' modulo * 8 get translateted by FreeBASIC to modulo shl 3
Dim As Long n = (ch - start) + IIf(m = mDecipher, -shift, shift) + modulo * 8
n = n Mod modulo
Return start + n

End Function

' // Caesar-shift a string on a pseudo-random stream
Function CeasarStr(m As ciphermode, msg As String, modulo As UByte, _
                                                    start As UByte) As String

Dim As Long i
Dim As String v

For i = 0 To Len(msg) -1
    v += Chr(Ceasar(m, msg[i], iRandA(), modulo, start))
Next

Return v

End Function

' ------=< MAIN >=------

Dim As Long n, l
Dim As String msg = "a Top Secret secret"
Dim As String key = "this is my secret key"

Dim As String vctx, vptx
Dim As String cctx, cptx

l = Len(msg)
' // Encrypt: Vernam XOR
iSeed(key, 1)
vctx = Vernam(msg)
' // Encrypt: Caesar
cctx = CeasarStr(mEncipher, msg, _mod_, _start_)
' // Decrypt: Vernam XOR
iSeed(key, 1)
vptx = Vernam(vctx)
' // Decrypt: Caesar
cptx = CeasarStr(mDecipher, cctx, _mod_, _start_)
Print "message: "; msg
Print "    key: "; key
Print "    XOR: ";
' // Output Vernam ciphertext as a string of hex digits
For n = 0 To l -1
    Print Hex(vctx[n], 2);
Next
Print
' // Output Vernam decrypted plaintext
Print "XOR dcr: "; vptx
' // Caesar
Print "    MOD: ";
' // Output Caesar ciphertext as a string of hex digits
For n= 0 To l -1
    Print Hex(cctx[n], 2);
Next
Print
' // Output Caesar decrypted plaintext
Print "MOD dcr: " ; cptx

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
message: a Top Secret secret
    key: this is my secret key
    XOR: 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr: a Top Secret secret
    MOD: 734270227D36772A783B4F2A5F206266236978
MOD dcr: a Top Secret secret
```



## Go

XOR version

```go
package main

import "fmt"

const (
    msg = "a Top Secret secret"
    key = "this is my secret key"
)

func main() {
    var z state
    z.seed(key)
    fmt.Println("Message: ", msg)
    fmt.Println("Key    : ", key)
    fmt.Println("XOR    : ", z.vernam(msg))
}

type state struct {
    aa, bb, cc uint32
    mm         [256]uint32
    randrsl    [256]uint32
    randcnt    int
}

func (z *state) isaac() {
    z.cc++
    z.bb += z.cc
    for i, x := range z.mm {
        switch i % 4 {
        case 0:
            z.aa = z.aa ^ z.aa<<13
        case 1:
            z.aa = z.aa ^ z.aa>>6
        case 2:
            z.aa = z.aa ^ z.aa<<2
        case 3:
            z.aa = z.aa ^ z.aa>>16
        }
        z.aa += z.mm[(i+128)%256]
        y := z.mm[x>>2%256] + z.aa + z.bb
        z.mm[i] = y
        z.bb = z.mm[y>>10%256] + x
        z.randrsl[i] = z.bb
    }
}

func (z *state) randInit() {
    const gold = uint32(0x9e3779b9)
    a := [8]uint32{gold, gold, gold, gold, gold, gold, gold, gold}
    mix1 := func(i int, v uint32) {
        a[i] ^= v
        a[(i+3)%8] += a[i]
        a[(i+1)%8] += a[(i+2)%8]
    }
    mix := func() {
        mix1(0, a[1]<<11)
        mix1(1, a[2]>>2)
        mix1(2, a[3]<<8)
        mix1(3, a[4]>>16)
        mix1(4, a[5]<<10)
        mix1(5, a[6]>>4)
        mix1(6, a[7]<<8)
        mix1(7, a[0]>>9)
    }
    for i := 0; i < 4; i++ {
        mix()
    }
    for i := 0; i < 256; i += 8 {
        for j, rj := range z.randrsl[i : i+8] {
            a[j] += rj
        }
        mix()
        for j, aj := range a {
            z.mm[i+j] = aj
        }
    }
    for i := 0; i < 256; i += 8 {
        for j, mj := range z.mm[i : i+8] {
            a[j] += mj
        }
        mix()
        for j, aj := range a {
            z.mm[i+j] = aj
        }
    }
    z.isaac()
}

func (z *state) seed(seed string) {
    for i, r := range seed {
        if i == 256 {
            break
        }
        z.randrsl[i] = uint32(r)
    }
    z.randInit()
}

func (z *state) random() (r uint32) {
    r = z.randrsl[z.randcnt]
    z.randcnt++
    if z.randcnt == 256 {
        z.isaac()
        z.randcnt = 0
    }
    return
}

func (z *state) randA() byte {
    return byte(z.random()%95 + 32)
}

func (z *state) vernam(msg string) string {
    b := []byte(msg)
    for i := range b {
        b[i] ^= z.randA()
    }
    return fmt.Sprintf("%X", b)
}
```

```txt

Message:  a Top Secret secret
Key    :  this is my secret key
XOR    :  1C0636190B1260233B35125F1E1D0E2F4C5422

```



## Haskell


```Haskell
import Data.Array (Array, (!), (//), array, elems)
import Data.Word (Word, Word32)
import Data.Bits (shift, xor)
import Data.Char (toUpper)
import Data.List (unfoldr)
import Numeric (showHex)

type IArray = Array Word32 Word32

data IsaacState = IState
  { randrsl :: IArray
  , randcnt :: Word32
  , mm :: IArray
  , aa :: Word32
  , bb :: Word32
  , cc :: Word32
  }

instance Show IsaacState where
  show (IState _ cnt _ a b c) =
    show cnt ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c

toHex :: Char -> String
toHex c = showHex (fromEnum c) ""

hexify :: String -> String
hexify = map toUpper . concatMap toHex

toNum :: Char -> Word32
toNum = fromIntegral . fromEnum

toChar :: Word32 -> Char
toChar = toEnum . fromIntegral

golden :: Word32
golden = 0x9e3779b9

-- Mix up an ordering of words.
mix :: [Word32] -> [Word32]
mix set = foldl aux set [11, -2, 8, -16, 10, -4, 8, -9]
  where
    aux [a, b, c, d, e, f, g, h] x = [b + c, c, d + a_, e, f, g, h, a_]
      where
        a_ = a `xor` (b `shift` x)

-- Generate the next 256 words.
isaac :: IsaacState -> IsaacState
isaac (IState rsl _ m a b c) = IState rsl_ 0 m_ a_ b_ c_
  where
    c_ = c + 1
    (rsl_, m_, a_, b_) =
      foldl aux (rsl, m, a, b) $ zip [0 .. 255] $ cycle [13, -6, 2, -16]
    aux (rsl, m, a, b) (i, s) = (rsl_, m_, a_, b_)
      where
        x = m ! i
        a_ = (a `xor` (a `shift` s)) + m ! ((i + 128) `mod` 256)
        y = a_ + b + m ! ((x `shift` (-2)) `mod` 256)
        m_ = m // [(i, y)]
        b_ = x + m_ ! ((y `shift` (-10)) `mod` 256)
        rsl_ = rsl // [(i, b_)]

-- Given a seed value in randrsl, initialize/mixup the state.
randinit :: IsaacState -> Bool -> IsaacState
randinit state flag = isaac (IState randrsl_ 0 m 0 0 0)
  where
    firstSet = iterate mix (replicate 8 golden) !! 4
    iter _ _ [] = []
    iter flag set rsl =
      let (rslH, rslT) = splitAt 8 rsl
          set_ =
            mix $
            if flag
              then zipWith (+) set rslH
              else set
      in set_ ++ iter flag set_ rslT
    randrsl_ = randrsl state
    firstPass = iter flag firstSet $ elems randrsl_
    set_ = drop (256 - 8) firstPass
    secondPass =
      if flag
        then iter True set_ firstPass
        else firstPass
    m = array (0, 255) $ zip [0 ..] secondPass

-- Given a string seed, optionaly use it to generate a new state.
seed :: String -> Bool -> IsaacState
seed key flag =
  let m = array (0, 255) $ zip [0 .. 255] $ repeat 0
      rsl = m // zip [0 ..] (map toNum key)
      state = IState rsl 0 m 0 0 0
  in randinit state flag

-- Produce a random word and the next state from the given state.
random :: IsaacState -> (Word32, IsaacState)
random state@(IState rsl cnt m a b c) =
  let r = rsl ! cnt
      state_ =
        if cnt + 1 > 255
          then isaac $ IState rsl 0 m a b c
          else IState rsl (cnt + 1) m a b c
  in (r, state_)

-- Produce a stream of random words from the given state.
randoms :: IsaacState -> [Word32]
randoms = unfoldr $ Just . random

-- Produce a random printable/typable character in the ascii range
-- and the next state from the given state.
randA :: IsaacState -> (Char, IsaacState)
randA state =
  let (r, state_) = random state
  in (toEnum $ fromIntegral $ (r `mod` 95) + 32, state_)

-- Produce a stream of printable characters from the given state.
randAs :: IsaacState -> String
randAs = unfoldr $ Just . randA

-- Vernam encode/decode a string with the given state.
vernam :: IsaacState -> String -> String
vernam state msg = map toChar $ zipWith xor msg_ randAs_
  where
    msg_ = map toNum msg
    randAs_ = map toNum $ randAs state

main :: IO ()
main = do
  let msg = "a Top Secret secret"
      key = "this is my secret key"
      st = seed key True
      ver = vernam st msg
      unver = vernam st ver
  putStrLn $ "Message: " ++ msg
  putStrLn $ "Key    : " ++ key
  putStrLn $ "XOR    : " ++ hexify ver
  putStrLn $ "XOR dcr: " ++ unver
```

```txt
Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr: a Top Secret secret
```



## Haxe

Used a signed type rather then unsigned as unsigned 32bit type
is not part of the default library.
The effect of all operations with the exception of compare and mod
are identical anyways.
It is possible in Haxe to create your own 32bit unsigned type,
but that is outside this exercise.

```Haxe

package src ;
import haxe.Int32;
import haxe.macro.Expr;
import haxe.ds.Vector;

typedef Ub4 = Int32;

enum Ciphermode {
	mEncipher;
	mDecipher;
	mNone;
}

class Isaac
{
	public var randrsl = new Vector<Ub4>(256);
	public var randcnt:Ub4;

	var mm = new Vector<Ub4>(256);
	var aa:Ub4 = 0;
	var bb:Ub4 = 0;
	var cc:Ub4 = 0;

	public function isaac():Void {
		var x, y;
		cc++;
		bb += cc;
		for (i in 0...256) {
			x = mm[i];
			aa ^= switch (i % 4) {//Haxe unification
				case 0: aa << 13;
				case 1: aa >>> 6;
				case 2: aa << 2;
				case 3: aa >>> 16;
				default: 0;//never happens
			}
			aa              = mm[(i + 128) % 256] + aa;
			mm[i]      = y  = mm[(x >>> 2) % 256] + aa + bb;
			randrsl[i] = bb = mm[(y >>> 10) % 256] + x;
		}
	}

	macro static function mix(a:ExprOf<Ub4>, b:ExprOf<Ub4>, c:ExprOf<Ub4>, d:ExprOf<Ub4>,
	                          e:ExprOf<Ub4>, f:ExprOf<Ub4>, g:ExprOf<Ub4>, h:ExprOf<Ub4>) {
		return macro {
			$a ^= $b << 11; $d += $a; $b += $c;
			$b ^= $c >>> 2; $e += $b; $c += $d;
			$c ^= $d << 8; $f += $c; $d += $e;
			$d ^= $e >>> 16; $g += $d; $e += $f;
			$e ^= $f << 10; $h += $e; $f += $g;
			$f ^= $g >>> 4; $a += $f; $g += $h;
			$g ^= $h << 8; $b += $g; $h += $a;
			$h ^= $a >>> 9; $c += $h; $a += $b;
		};
	}

	public function randinit(flag:Bool):Void {
		var a, b, c, d, e, f, g, h, i;
		aa = bb = cc = (0:Ub4);
		a = b = c = d = e = f = g = h = (0x9e3779b9:Ub4); /* the golden ratio */
		for (i in 0...4) mix(a, b, c, d, e, f, g, h); /* scramble it */
		i = 0;
		while (i < 256) { /* fill in mm[] with messy stuff */
			if (flag) { /* use all the information in the seed */
				a += randrsl[i]; b += randrsl[i + 1];
				c += randrsl[i + 2]; d += randrsl[i + 3];
				e += randrsl[i + 4]; f += randrsl[i + 5];
				g += randrsl[i + 6]; h += randrsl[i + 7];
			}
			mix(a, b, c, d, e, f, g, h);
			mm[i] = a; mm[i + 1] = b; mm[i + 2] = c; mm[i + 3] = d;
			mm[i + 4] = e; mm[i + 5] = f; mm[i + 6] = g; mm[i + 7] = h;
			i += 8;
		}
		if (flag) { /* do a second pass to make all of the seed affect all of mm */
			i = 0;
			while (i<256) {
				a += mm[i]; b += mm[i + 1]; c += mm[i + 2]; d += mm[i + 3];
				e += mm[i + 4]; f += mm[i + 5]; g += mm[i + 6]; h += mm[i + 7];
				mix(a, b, c, d, e, f, g, h);
				mm[i] = a; mm[i + 1] = b; mm[i + 2] = c; mm[i + 3] = d;
				mm[i + 4] = e; mm[i + 5] = f; mm[i + 6] = g; mm[i + 7] = h;
				i += 8;
			}
		}
		isaac();
		randcnt = 0;
	}

	public function iRandom():Ub4 {
		var r = randrsl[randcnt];
		++randcnt;
		if (randcnt > 255) {
			isaac();
			randcnt = 0;
		}
		return r;
	}

	public function iRandA():Int32 {
		return cast(cast(iRandom(),UInt) % 95 + 32,Int32);
	}

	public function iSeed(seed:String, flag:Bool):Void {
		var m=seed.length-1;
		for (i in 0...256) mm[i] = 0;
		for (i in 0...256) if (i > m) randrsl[i] = 0; else randrsl[i] = seed.charCodeAt(i);
		randinit(flag);
	}

	inline static var modC = 95;
	inline static var startC = 32;

	public function vernam (msg:String):String {
		var v="";
		for (i in 0...msg.length) v += String.fromCharCode(iRandA() ^ msg.charCodeAt(i));
		return v;
	}

	public function caesar(m:Ciphermode, ch:Int32, shift:Int32,
	                       modulo:Int32, start:Int32):String {
		var n:Int32;
		if (m == mDecipher) n = ch - start - cast(shift,Int32);
		else n = ch - start + cast(shift,Int32);
		n %= modulo;
		if (n < 0) n += modulo;
		return String.fromCharCode(start + cast(n,Ub4));
	}

	public function caesarStr(m:Ciphermode, msg:String, modulo:Int32, start:Int32):String {
		var c = "";
		for (i in 0...msg.length)
			c += caesar(m,msg.charCodeAt(i),iRandA(),modulo,start);
		return c;
	}

	static public function main():Void {
		var msg = "a Top Secret secret";
		var key = "this is my secret key";
		var cIsaac = new Isaac();
		var vctx, vptx, cctx, cptx;
		cIsaac.iSeed(key, true);
		vctx = cIsaac.vernam(msg);
		cctx = cIsaac.caesarStr(mEncipher, msg, modC, startC);

		cIsaac.iSeed(key, true);
		vptx = cIsaac.vernam(vctx);
		cptx = cIsaac.caesarStr(mDecipher, cctx, modC, startC);

		Sys.println("Message: " + msg);
		Sys.println("Key    : " + key);
		var hex = "";
		for (i in 0...vctx.length) hex += StringTools.hex(vctx.charCodeAt(i), 2);
		Sys.println("XOR    : " + hex);
		Sys.println("XOR dcr: " + vptx);
		hex = "";
		for (i in 0...cctx.length) hex += StringTools.hex(cctx.charCodeAt(i), 2);
		Sys.println("MOD    : " + hex);
		Sys.println("MOD dcr: " + cptx);
	}
}

```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr: a Top Secret secret
MOD    : 734270227D36772A783B4F2A5F206266236978
MOD dcr: a Top Secret secret

```



## Java


Java doesn't have unsigned data types, so it's important to use the logical right shift operator (>>>) instead of the arithmetic right shift operator (>>) on every right shift to maintain original semantics. Luckily, addition yields the same bits regardless of signedness, so most operations aren't affected.

This implementation extends the java.util.Random class, so it inherits methods that generate booleans, floats, doubles and longs, and can also generate numbers with Gaussian and uniform distribution. It can also be plugged in to standard library methods that receive a Random instance as a source of randomness. The toHexString() and main() methods are for demo purposes only and can be removed without changing main functionality.


```Java
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Random;


public class IsaacRandom extends Random {

	private static final long serialVersionUID = 1L;

	private final int[] randResult = new int[256];    // output of last generation
	private int valuesUsed;                           // the number of values already used up from randResult

	// internal generator state
	private final int[] mm = new int[256];
	private int aa, bb, cc;

	public IsaacRandom() {
		super(0);
		init(null);
	}

	public IsaacRandom(int[] seed) {
		super(0);
		setSeed(seed);
	}

	public IsaacRandom(String seed) {
		super(0);
		setSeed(seed);
	}

	private void generateMoreResults() {
		cc++;
		bb += cc;

		for (int i=0; i<256; i++) {
			int x = mm[i];
			switch (i&3) {
			case 0:
				aa = aa^(aa<<13);
				break;
			case 1:
				aa = aa^(aa>>>6);
				break;
			case 2:
				aa = aa^(aa<<2);
				break;
			case 3:
				aa = aa^(aa>>>16);
				break;
			}
			aa = mm[i^128] + aa;
			int y = mm[i] = mm[(x>>>2) & 0xFF] + aa + bb;
			randResult[i] = bb = mm[(y>>>10) & 0xFF] + x;
		}

		valuesUsed = 0;
	}

	private static void mix(int[] s) {
		   s[0]^=s[1]<<11;  s[3]+=s[0]; s[1]+=s[2];
		   s[1]^=s[2]>>>2;  s[4]+=s[1]; s[2]+=s[3];
		   s[2]^=s[3]<<8;   s[5]+=s[2]; s[3]+=s[4];
		   s[3]^=s[4]>>>16; s[6]+=s[3]; s[4]+=s[5];
		   s[4]^=s[5]<<10;  s[7]+=s[4]; s[5]+=s[6];
		   s[5]^=s[6]>>>4;  s[0]+=s[5]; s[6]+=s[7];
		   s[6]^=s[7]<<8;   s[1]+=s[6]; s[7]+=s[0];
		   s[7]^=s[0]>>>9;  s[2]+=s[7]; s[0]+=s[1];
	}

	private void init(int[] seed) {
		if (seed != null && seed.length != 256) {
			seed = Arrays.copyOf(seed, 256);
		}
		aa = bb = cc = 0;
		int[] initState = new int[8];
		Arrays.fill(initState, 0x9e3779b9);	// the golden ratio

		for (int i=0; i<4; i++) {
			mix(initState);
		}

		for (int i=0; i<256; i+=8) {
			if (seed != null) {
				for (int j=0; j<8; j++) {
					initState[j] += seed[i+j];
				}
			}
			mix(initState);
			for (int j=0; j<8; j++) {
				mm[i+j] = initState[j];
			}
		}

		if (seed != null) {
			for (int i=0; i<256; i+=8) {
				for (int j=0; j<8; j++) {
					initState[j] += mm[i+j];
				}

				mix(initState);

				for (int j=0; j<8; j++) {
					mm[i+j] = initState[j];
				}
			}
		}

		valuesUsed = 256;	// Make sure generateMoreResults() will be called by the next next() call.
	}

	@Override
	protected int next(int bits) {
		if (valuesUsed == 256) {
			generateMoreResults();
			assert(valuesUsed == 0);
		}
		int value = randResult[valuesUsed];
		valuesUsed++;
		return value >>> (32-bits);
	}

	@Override
	public synchronized void setSeed(long seed) {
		super.setSeed(0);
		if (mm == null) {
			// We're being called from the superclass constructor. We don't have our
			// state arrays instantiated yet, and we're going to do proper initialization
			// later in our own constructor anyway, so just ignore this call.
			return;
		}
		int[] arraySeed = new int[256];
		arraySeed[0] = (int) (seed & 0xFFFFFFFF);
		arraySeed[1] = (int) (seed >>> 32);
		init(arraySeed);
	}

	public synchronized void setSeed(int[] seed) {
		super.setSeed(0);
		init(seed);
	}

	public synchronized void setSeed(String seed) {
		super.setSeed(0);
		char[] charSeed = seed.toCharArray();
		int[] intSeed = new int[charSeed.length];
		for (int i=0; i<charSeed.length; i++) {
			intSeed[i] = charSeed[i];
		}
		init(intSeed);
	}

	public int randomChar() {
		long unsignedNext = nextInt() & 0xFFFFFFFFL;	// The only way to force unsigned modulo behavior in Java is to convert to a long and mask off the copies of the sign bit.
		return (int) (unsignedNext % 95 + 32);		    // nextInt(95) + 32 would yield a more equal distribution, but then we would be incompatible with the original C code
	}

	public enum CipherMode { ENCIPHER, DECIPHER, NONE };

	public byte[] vernamCipher(byte[] input) {
		byte[] result = new byte[input.length];
		for (int i=0; i<input.length; i++) {
			result[i] = (byte) (randomChar() ^ input[i]);
		}
		return result;
	}

	private static byte caesarShift(CipherMode mode, byte ch, int shift, byte modulo, byte start) {
		if (mode == CipherMode.DECIPHER) {
			shift = -shift;
		}
		int n = (ch-start) + shift;
		n %= modulo;
		if (n<0) {
			n += modulo;
		}
		return (byte) (start + n);
	}

	public byte[] caesarCipher(CipherMode mode, byte[] input, byte modulo, byte start) {
		byte[] result = new byte[input.length];
		for (int i=0; i<input.length; i++) {
			result[i] = caesarShift(mode, input[i], randomChar(), modulo, start);
		}
		return result;
	}

	private static String toHexString(byte[] input) {
		// NOTE: This method prefers simplicity over performance.
		StringBuilder sb = new StringBuilder(input.length*2);
		for (byte b : input) {
			sb.append(String.format("%02X", b));
		}
		return sb.toString();
	}

	public static void main(String[] args) {
		final byte MOD = 95;
		final byte START = 32;

		String secret = "a Top Secret secret";
		String key = "this is my secret key";

		IsaacRandom random = new IsaacRandom(key);
		byte[] vernamResult;
		byte[] caesarResult;
		String vernamDecrypted;
		String caesarDecrypted;
		try {
			vernamResult = random.vernamCipher(secret.getBytes("ASCII"));
			caesarResult = random.caesarCipher(CipherMode.ENCIPHER, secret.getBytes("ASCII"), MOD, START);
			random.setSeed(key);
			vernamDecrypted = new String(random.vernamCipher(vernamResult), "ASCII");
			caesarDecrypted = new String(random.caesarCipher(CipherMode.DECIPHER, caesarResult, MOD, START), "ASCII");
		} catch (UnsupportedEncodingException e) {
			throw new InternalError("JVM isn't conforming - ASCII encoding isn't available");
		}
		System.out.printf("Message: %s\n", secret);
		System.out.printf("Key    : %s\n", key);
		System.out.printf("XOR    : %s\n", toHexString(vernamResult));
		System.out.printf("XOR dcr: %s\n", vernamDecrypted);
		System.out.printf("MOD    : %s\n", toHexString(caesarResult));
		System.out.printf("MOD dcr: %s\n", caesarDecrypted);
	}
}
```



## Julia


```Julia

"""
Julia translation of code from the following:
------------------------------------------------------------------------------
readable.c: My random number generator, ISAAC.
(c) Bob Jenkins, March 1996, Public Domain
You may use this code in any way you wish, and it is free.  No warrantee.
------------------------------------------------------------------------------
"""
# maximum length of message here is set to 4096
const MAXMSG = 4096

# cipher modes for encryption versus decryption modes
@enum CipherMode mEncipher mDecipher mNone

# external results
mutable struct IState
    randrsl::Array{UInt32, 1}
    randcnt::UInt32
    mm::Array{UInt32, 1}
    aa::UInt32
    bb::UInt32
    cc::UInt32
    function IState()
        this = new()
        this.randrsl = zeros(UInt32, 256)
        this.randcnt = UInt32(0)
        this.mm = zeros(UInt32, 256)
        this.aa = this.bb = this.cc = UInt32(0)
        this
    end
end

"""
    isaac
Randomize the pool
"""
function isaac(istate)
    istate.cc +=  1             # cc gets incremented once per 256 results
    istate.bb += istate.cc      # then combined with bb

    for (j, c) in enumerate(istate.mm)   # Julia NB: indexing ahead so use i for c indexing
        i = j - 1
        xmod4 = i % 4
        if(xmod4 == 0)
            istate.aa ⊻= istate.aa << 13
        elseif(xmod4 == 1)
            istate.aa ⊻= istate.aa >> 6
        elseif(xmod4 == 2)
            istate.aa ⊻= istate.aa << 2
        else
            istate.aa ⊻= istate.aa >> 16
        end
        istate.aa += istate.mm[(i + 128) % 256 + 1]
        y = istate.mm[(c >> 2) % 256 + 1] + istate.aa + istate.bb
        istate.mm[j] = y
        istate.bb = istate.mm[(y >> 10) % 256 + 1] + c
        istate.randrsl[j] = istate.bb
    end
    # not in original readable.c
    istate.randcnt = 0
end


"""
    mix
Mix the bytes in a reversible way.
"""
function mix(arr)              # Julia NB: use E for e in c code here
   (a,b,c,d,E,f,g,h) = arr
   a⊻=b<<11; d+=a; b+=c;
   b⊻=c>>2;  E+=b; c+=d;
   c⊻=d<<8;  f+=c; d+=E;
   d⊻=E>>16; g+=d; E+=f;
   E⊻=f<<10; h+=E; f+=g;
   f⊻=g>>4;  a+=f; g+=h;
   g⊻=h<<8;  b+=g; h+=a;
   h⊻=a>>9;  c+=h; a+=b;
   (a,b,c,d,E,f,g,h)
end


"""
    randinit
Make a random UInt32 array.
If flag is true, use the contents of randrsl[] to initialize mm[].
"""
function randinit(istate, flag::Bool)
    istate.aa = istate.bb = istate.cc = 0
    mixer = Array{UInt32,1}(8)
    fill!(mixer, 0x9e3779b9)             # the golden ratio
    for i in 1:4                         # scramble it
        mixer = mix(mixer)
    end
    for i in 0:8:255                     # fill in mm[] with messy stuff
        if(flag)                         # use all the information in the seed
            mixer = [mixer[j] + istate.randrsl[i+j] for j in 1:8]
        end
        mixer = mix(mixer)
        istate.mm[i+1:i+8] .= mixer
    end
    if(flag)                             # do a second pass to seed all of mm
        for i in 0:8:255
            mixer = [mixer[j] + istate.mm[i+j] for j in 1:8]
            mixer = mix(mixer)
            istate.mm[i+1:i+8] .= mixer
        end
    end
    isaac(istate)                        # fill in the first set of results
    istate.randcnt = 0
end


"""
    Get a random 32-bit value 0..MAXINT
"""
function irandom(istate)
    retval::UInt32 = istate.randrsl[istate.randcnt+1]
    istate.randcnt += 1
    if(istate.randcnt > 255)
        isaac(istate)
        istate.randcnt = 0
    end
    retval
end


"""
    Get a random character in printable ASCII range
"""
iranda(istate) = UInt8(irandom(istate) % 95 + 32)


"""
    Do XOR cipher on random stream.
    Output: UInt8 array
"""
vernam(istate, msg) = [UInt8(iranda(istate) ⊻ c) for c in msg]


"""
    Seed ISAAC with a string
"""
function iseed(istate, seed, flag)
    fill!(istate.mm, 0)
    fill!(istate.randrsl, 0)
    len = min(length(seed), length(istate.randrsl))
    istate.randrsl[1:len] .= seed[1:len]
    randinit(istate, flag)   # initialize ISAAC with seed
end


tohexstring(arr::Array{UInt8,1}) = join([hex(i, 2) for i in arr])


function test(istate, msg, key)
    # Vernam ciphertext & plaintext
    vctx = zeros(UInt8, MAXMSG)
    vptx = zeros(UInt8, MAXMSG)
    # Encrypt: Vernam XOR
    iseed(istate, Vector{UInt8}(key), true)
    vctx = vernam(istate, Vector{UInt8}(msg))
    # Decrypt: Vernam XOR
    iseed(istate, Vector{UInt8}(key), true)
    vptx = vernam(istate, vctx)
    # Program output
    println("Message: $msg")
    println("Key    : $key")
    println("XOR    : $(tohexstring(vctx))")
    # Output Vernam decrypted plaintext
    println("XOR dcr: $(join(map(c -> Char(c), vptx)))")
    0
end


"""
Test the above.
"""
const msg = "a Top Secret secret"
const key = "this is my secret key"
test(IState(), msg, key)

```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1c0636190b1260233b35125f1e1d0e2f4c5422
XOR dcr: a Top Secret secret

```



## Kotlin

```scala
// version 1.1.3

/* external results */
val randrsl = IntArray(256)
var randcnt = 0

/* internal state */
val mm = IntArray(256)
var aa = 0
var bb = 0
var cc = 0

const val GOLDEN_RATIO = 0x9e3779b9.toInt()

fun isaac() {
    cc++       // cc just gets incremented once per 256 results
    bb += cc   // then combined with bb
    for (i in 0..255) {
        val x = mm[i]
        when (i % 4) {
            0 -> aa = aa xor (aa shl 13)
            1 -> aa = aa xor (aa ushr 6)
            2 -> aa = aa xor (aa shl 2)
            3 -> aa = aa xor (aa ushr 16)
        }
        aa += mm[(i + 128) % 256]
        val y = mm[(x ushr 2) % 256] + aa + bb
        mm[i] = y
        bb = mm[(y ushr 10) % 256] + x
        randrsl[i] = bb
    }
    randcnt = 0
}

/* if (flag == true), then use the contents of randrsl to initialize mm. */
fun mix(n: IntArray) {
    n[0] = n[0] xor (n[1]  shl 11); n[3] += n[0]; n[1] += n[2]
    n[1] = n[1] xor (n[2] ushr  2); n[4] += n[1]; n[2] += n[3]
    n[2] = n[2] xor (n[3]  shl  8); n[5] += n[2]; n[3] += n[4]
    n[3] = n[3] xor (n[4] ushr 16); n[6] += n[3]; n[4] += n[5]
    n[4] = n[4] xor (n[5]  shl 10); n[7] += n[4]; n[5] += n[6]
    n[5] = n[5] xor (n[6] ushr  4); n[0] += n[5]; n[6] += n[7]
    n[6] = n[6] xor (n[7]  shl  8); n[1] += n[6]; n[7] += n[0]
    n[7] = n[7] xor (n[0] ushr  9); n[2] += n[7]; n[0] += n[1]
}

fun randinit(flag: Boolean) {
    aa = 0
    bb = 0
    cc = 0
    val n = IntArray(8) { GOLDEN_RATIO }
    for (i in 0..3) mix(n)      // scramble the array

    for (i in 0..255 step 8) {  // fill in mm with messy stuff
        if (flag) {             // use all the information in the seed
           for (j in 0..7) n[j] += randrsl[i + j]
        }
        mix(n)
        for (j in 0..7) mm[i + j] = n[j]
    }

    if (flag) {
        /* do a second pass to make all of the seed affect all of mm */
        for (i in 0..255 step 8) {
            for (j in 0..7) n[j] += mm[i + j]
            mix(n)
            for (j in 0..7) mm[i + j] = n[j]
        }
    }

    isaac()       // fill in the first set of results
    randcnt = 0  // prepare to use the first set of results
}

/* As Kotlin doesn't (yet) support unsigned types, we need to use
   Long here to get a random value in the range of a UInt */
fun iRandom(): Long {
    val r = randrsl[randcnt++]
    if (randcnt > 255) {
        isaac()
        randcnt = 0
    }
    return r.toLong() and 0xFFFFFFFFL
}

/* Get a random character (as Int) in printable ASCII range */
fun iRandA() = (iRandom() % 95 + 32).toInt()

/* Seed ISAAC with a string */
fun iSeed(seed: String, flag: Boolean) {
    for (i in 0..255) mm[i] = 0
    val m = seed.length
    for (i in 0..255) {
        /* in case seed has less than 256 elements */
        randrsl[i] = if (i >= m) 0 else seed[i].toInt()
    }
    /* initialize ISAAC with seed */
    randinit(flag)
}

/* XOR cipher on random stream. Output: ASCII string */
fun vernam(msg: String) : String {
    val len = msg.length
    val v = ByteArray(len)
    for (i in 0 until len) {
        v[i] = (iRandA() xor msg[i].toInt()).toByte()
    }
    return v.toString(charset("ASCII"))
}

/* constants for Caesar */
const val MOD = 95
const val START = 32

/* cipher modes for Caesar */
enum class CipherMode {
    ENCIPHER, DECIPHER, NONE
}

/* Caesar-shift a printable character */
fun caesar(m: CipherMode, ch: Int, shift: Int, modulo: Int, start: Int): Char {
    val sh = if (m == CipherMode.DECIPHER) -shift else shift
    var n = (ch - start) + sh
    n %= modulo
    if (n < 0) n += modulo
    return (start + n).toChar()
}

/* Caesar-shift a string on a pseudo-random stream */
fun caesarStr(m: CipherMode, msg: String, modulo: Int, start: Int): String {
    val sb = StringBuilder(msg.length)
    /* Caesar-shift message */
    for (c in msg) {
        sb.append(caesar(m, c.toInt(), iRandA(), modulo, start))
    }
    return sb.toString()
}

fun String.toHexByteString() =
    this.map { "%02X".format(it.toInt()) }.joinToString("")

fun main(args: Array<String>) {
    val msg = "a Top Secret secret"
    val key = "this is my secret key"

    // Vernam & Caesar ciphertext
    iSeed(key, true)
    val vctx = vernam(msg)
    val cctx = caesarStr(CipherMode.ENCIPHER, msg,  MOD, START)

    // Vernam & Caesar plaintext
    iSeed(key, true)
    val vptx = vernam(vctx)
    val cptx = caesarStr(CipherMode.DECIPHER, cctx, MOD, START)

    // Program output
    println("Message : $msg")
    println("Key     : $key")
    println("XOR     : ${vctx.toHexByteString()}")
    println("XOR dcr : $vptx")
    println("MOD     : ${cctx.toHexByteString()}")
    println("MOD dcr : $cptx")
}
```


```txt

Message : a Top Secret secret
Key     : this is my secret key
XOR     : 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr : a Top Secret secret
MOD     : 734270227D36772A783B4F2A5F206266236978
MOD dcr : a Top Secret secret

```


=={{header|Modula-2}}==
I changed the identifiers to clearer ones and I changed the variables <code>a</code>, <code>b</code>, ..., <code>h</code> to an array, because they made my blood boil.

```modula2

MODULE RosettaIsaac;

FROM Strings IMPORT
  Length, Assign, Append;
FROM STextIO IMPORT
  WriteString, WriteLn;
FROM Conversions IMPORT
  CardBaseToStr;

CONST
  MaxStrLength = 256;

TYPE
  TMode = (iEncrypt, iDecrypt);
  TString = ARRAY [0 .. MaxStrLength - 1] OF CHAR;
  TCardIndexedFrom0To7 = ARRAY [0 .. 7] OF CARDINAL;

VAR
(* TASK globals *)
  Msg: TString = 'a Top Secret secret';
  Key: TString = 'this is my secret key';
  XorCipherText: TString = '';
  ModCipherText: TString = '';
  XorPlainText: TString = '';
  ModPlainText: TString = '';
  Mode: TMode = iEncrypt;
  HexText: TString;

(* ISAAC globals *)
(* external results *)
  RandRsl: ARRAY [0 .. 256] OF CARDINAL;
  RandCnt: CARDINAL;

(* internal state *)
  MM: ARRAY [0 .. 256] OF CARDINAL;
  AA: CARDINAL = 0;
  BB: CARDINAL = 0;
  CC: CARDINAL = 0;

PROCEDURE Isaac;
VAR
  I, X, Y: CARDINAL;
BEGIN
   CC := CC + 1;    (* CC just gets incremented once per 256 results *)
   BB := BB + CC;   (* then combined with BB *)
   FOR I := 0 TO 255 DO
     X := MM[I];
     CASE (I MOD 4) OF
        0: AA := AA BXOR (AA SHL 13); |
        1: AA := AA BXOR (AA SHR 6); |
        2: AA := AA BXOR (AA SHL 2); |
        3: AA := AA BXOR (AA SHR 16);
     ELSE
     END;
     AA := MM[(I + 128) MOD 256] + AA;
     Y  := MM[(X SHR 2) MOD 256] + AA + BB;
     MM[I] := Y;
     BB := MM[(Y SHR 10) MOD 256] + X;
     RandRsl[I] := BB;
   END; (* FOR *)
   RandCnt := 0;  (* Prepare to use the first set of results. *)
END Isaac;

PROCEDURE Mix(VAR A: TCardIndexedFrom0To7);
BEGIN
  A[0] := A[0] BXOR A[1] SHL 11; A[3] := A[3] + A[0]; A[1] := A[1] + A[2];
  A[1] := A[1] BXOR A[2] SHR  2; A[4] := A[4] + A[1]; A[2] := A[2] + A[3];
  A[2] := A[2] BXOR A[3] SHL  8; A[5] := A[5] + A[2]; A[3] := A[3] + A[4];
  A[3] := A[3] BXOR A[4] SHR 16; A[6] := A[6] + A[3]; A[4] := A[4] + A[5];
  A[4] := A[4] BXOR A[5] SHL 10; A[7] := A[7] + A[4]; A[5] := A[5] + A[6];
  A[5] := A[5] BXOR A[6] SHR  4; A[0] := A[0] + A[5]; A[6] := A[6] + A[7];
  A[6] := A[6] BXOR A[7] SHL  8; A[1] := A[1] + A[6]; A[7] := A[7] + A[0];
  A[7] := A[7] BXOR A[0] SHR  9; A[2] := A[2] + A[7]; A[0] := A[0] + A[1];
END Mix;

PROCEDURE RandInit(Flag: BOOLEAN);
VAR
  I, J: CARDINAL;
  A: TCardIndexedFrom0To7;
BEGIN
  AA := 0; BB := 0; CC := 0;
  A[0] := 2654435769; (* $9e3779b9: the golden ratio *)
  FOR J := 1 TO 7 DO
    A[J] := A[0];
  END;

  FOR I := 0 TO 3 DO (* Scramble it *)
    Mix(A);
  END;
  FOR I := 0 TO 255 BY 8 DO (* Fill in MM[] with messy stuff. *)
    IF Flag THEN (* Use all the information in the seed. *)
      FOR J := 0 TO 7 DO
        A[J] := A[J] + RandRsl[I + J];
      END;
    END;
    Mix(A);
    FOR J := 0 TO 7 DO
      MM[I + J] := A[J];
    END;
  END; (* FOR I*)

  IF Flag THEN
    (* Do a second pass to make all of the Seed affect all of MM *)
    FOR I := 0 TO 255 BY 8 DO
      FOR J := 0 TO 7 DO
        A[J] := A[J] + MM[I + J];
      END;
      Mix(A);
      FOR J := 0 TO 7 DO
        MM[I + J] := A[J];
      END;
    END; (* FOR I *)
  END;
  Isaac(); (* Fill in the first set of results *)
  RandCnt := 0; (* Prepare to use the first set of results *)
END RandInit;

(* Seed ISAAC with a given string.
  The string can be any size. The first 256 values will be used. *)
PROCEDURE SeedIsaac(Seed: ARRAY OF CHAR; Flag: BOOLEAN);
VAR
  I, M: CARDINAL;
BEGIN
  FOR I := 0 TO 255 DO
    MM[I] := 0;
  END;
  M := Length(Seed);
  FOR I := 0 TO 255 DO
  (* In case seed has less than 256 elements *)
    IF I > M THEN
      RandRsl[I] := 0
    ELSE
      (* Modula-2 strings are 0-based (at least, in this case). *)
      RandRsl[I] := ORD(Seed[I]);
    END;
  END;
  (* Initialize ISAAC with seed. *)
  RandInit(Flag);
END SeedIsaac;

(* Get a random 32-bit value 0..MAXINT *)
PROCEDURE GetRandom32Bit(): CARDINAL;
VAR
  Result: CARDINAL;
BEGIN
  Result := RandRsl[RandCnt];
  INC(RandCnt);
  IF RandCnt > 255 THEN
    Isaac();
    RandCnt := 0;
  END;
  RETURN Result;
END GetRandom32Bit;

(* Get a random character in printable ASCII range. *)
PROCEDURE GetRandomChar(): SHORTCARD;
BEGIN
  RETURN GetRandom32Bit() MOD 95 + 32;
END GetRandomChar;

(* Convert an ASCII string to a hexadecimal string. *)
PROCEDURE ASCII2Hex(Source: ARRAY OF CHAR; VAR OUT Destination: ARRAY OF CHAR);
VAR
  I: CARDINAL;
  NumbHex: ARRAY [0 .. 1] OF CHAR;
BEGIN
  Assign('', Destination);
  FOR I := 0 TO Length(Source) - 1 DO
    CardBaseToStr(ORD(Source[I]), 16, NumbHex);
    IF Length(NumbHex) <= 1 THEN
      Append('0', Destination);
    END;
    Append(NumbHex, Destination);
  END;
END ASCII2Hex;

(* XOR encrypt on random stream. *)
PROCEDURE Vernam(Msg: ARRAY OF CHAR; VAR OUT Destination: ARRAY OF CHAR);
VAR
  I: CARDINAL;
  OrdMsgI: SHORTCARD;
BEGIN
  Assign('', Destination);
  FOR I := 0 TO Length(Msg) - 1 DO
    OrdMsgI := ORD(Msg[I]);
    Append(CHR(GetRandomChar() BXOR OrdMsgI), Destination);
  END;
END Vernam;

(* Get position of the letter in chosen alphabet *)
PROCEDURE LetterNum(Letter, Start: CHAR): SHORTCARD;
BEGIN
  RETURN ORD(Letter) - ORD(Start);
END LetterNum;

(* Caesar-shift a character <Shift> places: Generalized Vigenere *)
PROCEDURE Caesar(M: TMode; Ch: CHAR; Shift, Modulo: INTEGER; Start: CHAR): CHAR;
VAR
  N, IntOrdStart: INTEGER;
BEGIN
  IF M = iDecrypt THEN
    Shift := -Shift;
  END;
  N := LetterNum(Ch, Start);
  N := N + Shift;
  N := N MOD Modulo;
  IF N < 0 THEN
    N := N + Modulo;
  END;
  IntOrdStart := ORD(Start);
  RETURN CHR(IntOrdStart + N);
END Caesar;

(* Vigenere mod 95 encryption & decryption. *)
PROCEDURE Vigenere(Msg: ARRAY OF CHAR; M: TMode; VAR OUT Destination: ARRAY OF CHAR);
VAR
  I: CARDINAL;
BEGIN
  Assign('', Destination);
  FOR I := 0 TO Length(Msg) - 1 DO
    Append(Caesar(M, Msg[I], GetRandomChar(), 95, ' '), Destination);
  END;
END Vigenere;

BEGIN
  (* (1) Seed ISAAC with the key *)
  SeedIsaac(Key, TRUE);
  (* (2) Encryption *)
  Mode := iEncrypt;
  (* (a) XOR (Vernam) *)
  Vernam(Msg, XorCipherText);
  (* (b) MOD (Vigenere) *)
  Vigenere(Msg, Mode, ModCipherText);
  (* (3) Decryption *)
  Mode := iDecrypt;
  SeedIsaac(Key, TRUE);
  (* (a) XOR (Vernam) *)
  Vernam(XorCipherText, XorPlainText);
  (* (b) MOD (Vigenere) *)
  Vigenere(ModCipherText, Mode, ModPlainText);
  (* program output *)
  WriteString('Message: '); WriteString(Msg); WriteLn;
  WriteString('Key    : '); WriteString(Key); WriteLn;
  ASCII2Hex(XorCipherText, HexText);
  WriteString('XOR    : '); WriteString(HexText); WriteLn;
  ASCII2Hex(ModCipherText, HexText);
  WriteString('MOD    : '); WriteString(HexText); WriteLn;
  WriteString('XOR dcr: '); WriteString(XorPlainText); WriteLn;
  WriteString('MOD dcr: '); WriteString(ModPlainText); WriteLn;
END RosettaIsaac.

```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
MOD    : 734270227D36772A783B4F2A5F206266236978
XOR dcr: a Top Secret secret
MOD dcr: a Top Secret secret

```



## Pascal

Free Pascal. A fully functional and complete reference solution of the task.

```Pascal

PROGRAM RosettaIsaac;
USES
  StrUtils;

TYPE
  iMode = (iEncrypt, iDecrypt);

// TASK globals
VAR
  msg : String = 'a Top Secret secret';
  key : String = 'this is my secret key';
  xctx: String = ''; // XOR ciphertext
  mctx: String = ''; // MOD ciphertext
  xptx: String = ''; // XOR decryption (plaintext)
  mptx: String = ''; // MOD decryption (plaintext)

// ISAAC globals
VAR
  // external results
  randrsl: ARRAY[0 .. 255] OF Cardinal;
  randcnt: Cardinal;

  // internal state
  mm: ARRAY[0 .. 255] OF Cardinal;
  aa: Cardinal = 0;
  bb: Cardinal = 0;
  cc: Cardinal = 0;

PROCEDURE Isaac;
VAR
  i, x, y: Cardinal;
BEGIN
  cc := cc + 1; // cc just gets incremented once per 256 results
  bb := bb + cc; // then combined with bb

  FOR i := 0 TO 255 DO
  BEGIN
    x := mm[i];
    CASE (i MOD 4) OF
      0: aa := aa XOR (aa SHL 13);
      1: aa := aa XOR (aa SHR 6);
      2: aa := aa XOR (aa SHL 2);
      3: aa := aa XOR (aa SHR 16);
    END;
    aa := mm[(i + 128) MOD 256] + aa;
    y  := mm[(x SHR 2) MOD 256] + aa + bb;
    mm[i] := y;
    bb := mm[(y SHR 10) MOD 256] + x;
    randrsl[i] := bb;
  END;
  randcnt := 0; // prepare to use the first set of results
END; // Isaac

PROCEDURE Mix(VAR a, b, c, d, e, f, g, h: Cardinal);
BEGIN
  a := a XOR b SHL 11; d := d + a; b := b + c;
  b := b XOR c SHR  2; e := e + b; c := c + d;
  c := c XOR d SHL  8; f := f + c; d := d + e;
  d := d XOR e SHR 16; g := g + d; e := e + f;
  e := e XOR f SHL 10; h := h + e; f := f + g;
  f := f XOR g SHR  4; a := a + f; g := g + h;
  g := g XOR h SHL  8; b := b + g; h := h + a;
  h := h XOR a SHR  9; c := c + h; a := a + b;
END; // Mix

PROCEDURE iRandInit(flag: Boolean);
VAR
  i, a, b, c, d, e, f, g, h: Cardinal;
BEGIN
  aa := 0; bb := 0; cc := 0;
  a := $9e3779b9; // the golden ratio
  b := a; c := a; d := a; e := a; f := a; g := a; h := a;

  FOR i := 0 TO 3 DO // scramble it
    Mix(a, b, c, d, e, f, g, h);

  i := 0;
  REPEAT // fill in mm[] with messy stuff
    IF flag THEN
    BEGIN // use all the information in the seed
      a += randrsl[i    ]; b += randrsl[i + 1];
      c += randrsl[i + 2]; d += randrsl[i + 3];
      e += randrsl[i + 4]; f += randrsl[i + 5];
      g += randrsl[i + 6]; h += randrsl[i + 7];
    END;

    Mix(a, b, c, d, e, f, g, h);
    mm[i    ] := a; mm[i + 1] := b; mm[i + 2] := c; mm[i + 3] := d;
    mm[i + 4] := e; mm[i + 5] := f; mm[i + 6] := g; mm[i + 7] := h;
    i += 8;
  UNTIL i > 255;

  IF flag THEN
  BEGIN
    // do a second pass to make all of the seed affect all of mm
    i := 0;
    REPEAT
      a += mm[i    ]; b += mm[i + 1]; c += mm[i + 2]; d += mm[i + 3];
      e += mm[i + 4]; f += mm[i + 5]; g += mm[i + 6]; h += mm[i + 7];
      Mix(a, b, c, d, e, f, g, h);
      mm[i    ] := a; mm[i + 1] := b; mm[i + 2] := c; mm[i + 3] := d;
      mm[i + 4] := e; mm[i + 5] := f; mm[i + 6] := g; mm[i + 7] := h;
      i += 8;
    UNTIL i > 255;
  END;
  Isaac(); // fill in the first set of results
  randcnt := 0; // prepare to use the first set of results
END; // iRandInit

// Seed ISAAC with a given string.
// The string can be any size. The first 256 values will be used.
PROCEDURE iSeed(seed: String; flag: Boolean);
VAR
  i, m: Cardinal;
BEGIN
  FOR i := 0 TO 255 DO
    mm[i] := 0;
  m := Length(seed) - 1;
  FOR i := 0 TO 255 DO
  BEGIN
    // in case seed has less than 256 elements
    IF i > m THEN
      randrsl[i] := 0
      // Pascal strings are 1-based
    ELSE
      randrsl[i] := Ord(seed[i + 1]);
  END;
  // initialize ISAAC with seed
  iRandInit(flag);
END; // iSeed

// Get a random 32-bit value 0..MAXINT
FUNCTION iRandom: Cardinal;
BEGIN
  iRandom := randrsl[randcnt];
  inc(randcnt);
  IF (randcnt > 255) THEN
  BEGIN
    Isaac;
    randcnt := 0;
  END;
END; // iRandom

// Get a random character in printable ASCII range
FUNCTION iRandA: Byte;
BEGIN
  iRandA := iRandom MOD 95 + 32;
END;

// Convert an ASCII string to a hexadecimal string
FUNCTION Ascii2Hex(s: String): String;
VAR
  i: Cardinal;
BEGIN
  Ascii2Hex := '';
  FOR i := 1 TO Length(s) DO
    Ascii2Hex += Dec2Numb(Ord(s[i]), 2, 16);
END; // Ascii2Hex

// XOR encrypt on random stream. Output: ASCII string
FUNCTION Vernam(msg: String): String;
VAR
  i: Cardinal;
BEGIN
  Vernam := '';
  FOR i := 1 to Length(msg) DO
    Vernam += Chr(iRandA XOR Ord(msg[i]));
END; // Vernam

// Get position of the letter in chosen alphabet
FUNCTION LetterNum(letter, start: Char): Byte;
BEGIN
  LetterNum := (Ord(letter) - Ord(start));
END; // LetterNum

// Caesar-shift a character <shift> places: Generalized Vigenere
FUNCTION Caesar(m: iMode; ch: Char; shift, modulo: Integer; start: Char): Char;
VAR
  n: Integer;
BEGIN
  IF m = iDecrypt THEN
    shift := -shift;
  n := LetterNum(ch, start) + shift;
  n := n MOD modulo;
  IF n < 0 THEN
    n += modulo;
  Caesar := Chr(Ord(start) + n);
END; // Caesar

// Vigenere MOD 95 encryption & decryption. Output: ASCII string
FUNCTION Vigenere(msg: String; m: iMode): String;
VAR
  i: Cardinal;
BEGIN
  Vigenere := '';
  FOR i := 1 to Length(msg) DO
    Vigenere += Caesar(m, msg[i], iRandA, 95, ' ');
END; // Vigenere

BEGIN
  // 1) seed ISAAC with the key
  iSeed(key, true);
  // 2) Encryption
  // a) XOR (Vernam)
  xctx := Vernam(msg);
  // b) MOD (Vigenere)
  mctx := Vigenere(msg, iEncrypt);
  // 3) Decryption
  iSeed(key, true);
  // a) XOR (Vernam)
  xptx := Vernam(xctx);
  // b) MOD (Vigenere)
  mptx := Vigenere(mctx, iDecrypt);
  // program output
  Writeln('Message: ', msg);
  Writeln('Key    : ', key);
  Writeln('XOR    : ', Ascii2Hex(xctx));
  Writeln('MOD    : ', Ascii2Hex(mctx));
  Writeln('XOR dcr: ', xptx);
  Writeln('MOD dcr: ', mptx);
END.

```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
MOD    : 734270227D36772A783B4F2A5F206266236978
XOR dcr: a Top Secret secret
MOD dcr: a Top Secret secret

```



## Perl

Perl has had an ISAAC module for a few years, and it is the recommended way to use ISAAC.  This example uses [https://metacpan.org/pod/Math::Random::ISAAC Math::Random::ISAAC] which is a pure Perl implementation, but will also allow faster operation if the [https://metacpan.org/pod/Math::Random::ISAAC::XS Math::Random::ISAAC::XS] module is installed.

Since ISAAC does not do its own seeding, the [https://metacpan.org/pod/Bytes::Random::Secure Bytes::Random::Secure] module is recommended
for general use as it includes ISAAC plus a portable way to get good entropy,
as well as additional convenience functions.


```perl
use warnings;
use strict;
use Math::Random::ISAAC;

my $message = "a Top Secret secret";
my $key = "this is my secret key";

my $enc = xor_isaac($key, $message);
my $dec = xor_isaac($key, join "", pack "H*", $enc);

print "Message: $message\n";
print "Key    : $key\n";
print "XOR    : $enc\n";
print "XOR dcr: ", join("", pack "H*", $dec), "\n";

sub xor_isaac {
  my($key, $msg) = @_;

  # Make an ISAAC stream with the desired seed
  my $rng = Math::Random::ISAAC->new( map { ord } split "",$key );

  # Get ISAAC output in the order the task wants
  my @iranda = map { $_ % 95 + 32 }  # Alpha-tize as the task desires
               reverse               # MRI gives state from the end
               map { $rng->irand }   # Get random inputs...
               0..255;               # a state chunk at a time
  # Encode:
  join "", map { sprintf "%02X",$_ }         # join hex digits
           map { ord($_) ^ shift(@iranda) }  # xor it with rand char
           split "", $msg;                   # Take each character
}
```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr: a Top Secret secret

```



## Perl 6


```perl6
#!/usr/bin/env perl6

use v6.d;

my uint32 (@mm, @randrsl, $randcnt, $aa, $bb, $cc);
my \ϕ := 2654435769; constant MOD = 95; constant START = 32;

constant MAXINT = uint.Range.max;
enum CipherMode < ENCIPHER DECIPHER NONE >;

sub mix (\n) {
   sub mix1 (\i, \v) {
      n[i] +^= v;
      n[(i+3)%8] += n[i];
      n[(i+1)%8] += n[(i+2)%8];
   }
   mix1 0, n[1]+<11; mix1 1, n[2]+>2; mix1 2, n[3]+<8; mix1 3, n[4]+>16;
   mix1 4, n[5]+<10; mix1 5, n[6]+>4; mix1 6, n[7]+<8; mix1 7, n[0]+>9 ;
}

sub randinit(\flag) {
   $aa = $bb = $cc = 0;
   my uint32 @n = [^8].map({ ϕ });
   for ^4 { mix @n };
   for 0,8 … 255 -> $i {
      { for (0..7) { @n[$^j] += @randrsl[$i + $^j] } } if flag;
      mix @n;
      for (0..7) { @mm[$i + $^j] = @n[$^j] }
   }
   if flag {
      for 0,8 … 255 -> $i {
         for ^8 { @n[$^j] += @mm[$i + $^j] };
         mix @n;
         for ^8 { @mm[$i + $^j] = @n[$^j] };
      }
   }
   isaac;
   $randcnt = 0;
}

sub isaac() {
   $cc++;
   $bb += $cc;
   for ^256 -> $i {
      my $x = @mm[$i];
      given ($i % 4) {
         when 0 { $aa +^= ($aa +< 13) }
         when 1 { $aa +^= (($aa +& MAXINT) +>  6) }
         when 2 { $aa +^= ($aa +<  2) }
         when 3 { $aa +^= (($aa +& MAXINT) +> 16) }
      }
      $aa += @mm[($i + 128) % 256];
      my $y = @mm[(($x +& MAXINT) +> 2) % 256] + $aa + $bb;
      @mm[$i] = $y;
      $bb = @mm[(($y +& MAXINT) +> 10) % 256] + $x;
      @randrsl[$i] = $bb;
   }
   $randcnt = 0;
}

sub iRandom {
   my $result = @randrsl[$randcnt++];
   if ($randcnt > 255) {
      isaac;
      $randcnt = 0;
   }
   return $result;
}

sub iSeed(\seed, \flag) {
    @mm = [^256].race.map({0});
    my \m = seed.chars;
    @randrsl = [^256].hyper.map({ $^i ≥ m ?? 0 !! seed.substr($^i,1).ord });
    randinit(flag);
}

sub iRandA { return iRandom() % MOD + START };

sub vernam(\M) { ( map { (iRandA() +^ .ord ).chr }, M.comb ).join };

sub caesar(CipherMode \m, \ch, $shift is copy, \Modulo, \Start) {
    $shift = -$shift if m == DECIPHER;
    my $n = (ch.ord - Start) + $shift;
    $n %= Modulo;
    $n += Modulo if $n < 0;
    return (Start + $n).chr;
}

sub caesarStr(CipherMode \m, \msg, \Modulo, \Start) {
   my $sb = '';
   for msg.comb {
        $sb ~= caesar m, $^c, iRandA(), Modulo, Start;
   }
   return $sb;
}

multi MAIN () {
   my \msg = "a Top Secret secret";
   my \key = "this is my secret key";

   iSeed key, True ;
   my $vctx = vernam msg;
   my $cctx = caesarStr ENCIPHER, msg,  MOD, START;

   iSeed key, True ;
   my $vptx = vernam $vctx;
   my $cptx = caesarStr DECIPHER, $cctx, MOD, START;

   my $vctx2hex = ( map { .ord.fmt('%02X') }, $vctx.comb ).join('');
   my $cctx2hex = ( map { .ord.fmt('%02X') }, $cctx.comb ).join('');

   say "Message : ", msg;
   say "Key     : ", key;
   say "XOR     : ", $vctx2hex;
   say "XOR dcr : ", $vptx;
   say "MOD     : ", $cctx2hex;
   say "MOD dcr : ", $cptx;
}
```

```txt

Message : a Top Secret secret
Key     : this is my secret key
XOR     : 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr : a Top Secret secret
MOD     : 734270227D36772A783B4F2A5F206266236978
MOD dcr : a Top Secret secret

```



## Phix

We need the r32() function to convert our common sense maths into the needed unsigned_and_throw_away_any_high_bits maths.

```Phix
-- demo\rosetta\ISAAC_Cipher.exw

sequence randrsl = repeat(0,256)
integer randcnt

sequence mm
atom aa,bb,cc

function r32(object a)
    if sequence(a) then
        for i=1 to length(a) do
            a[i] = r32(a[i])
        end for
        return a
    end if
    if a<0 then a+=#100000000 end if
    return remainder(a,#100000000)
end function

function shl(atom word, integer bits)
    return r32(word*power(2,bits))
end function

function shr(atom v, integer bits)
    return floor(v/power(2,bits))
end function

procedure Isaac()
    cc += 1;    -- cc just gets incremented once per 256 results
    bb += cc;   -- then combined with bb
    for i=1 to 256 do
        atom x = mm[i]
        switch mod(i-1,4) do
            case 0: aa := xor_bits(aa,shl(aa,13))
            case 1: aa := xor_bits(aa,shr(aa, 6))
            case 2: aa := xor_bits(aa,shl(aa, 2))
            case 3: aa := xor_bits(aa,shr(aa,16))
        end switch
        aa = r32(mm[xor_bits(i-1,#80)+1]+aa)
        atom y := mm[and_bits(shr(x,2),#FF)+1]+aa+bb
        mm[i] := y;
        bb := r32(mm[and_bits(shr(y,10),#FF)+1] + x)
        randrsl[i]:= bb;
    end for
    randcnt = 1
end procedure

function mix(sequence a8)
    atom {a,b,c,d,e,f,g,h} = a8
    a = xor_bits(a,shl(b,11));  {d,b} = r32({d+a,b+c});
    b = xor_bits(b,shr(c, 2));  {e,c} = r32({e+b,c+d});
    c = xor_bits(c,shl(d, 8));  {f,d} = r32({f+c,d+e});
    d = xor_bits(d,shr(e,16));  {g,e} = r32({g+d,e+f});
    e = xor_bits(e,shl(f,10));  {h,f} = r32({h+e,f+g});
    f = xor_bits(f,shr(g, 4));  {a,g} = r32({a+f,g+h});
    g = xor_bits(g,shl(h, 8));  {b,h} = r32({b+g,h+a});
    h = xor_bits(h,shr(a, 9));  {c,a} = r32({c+h,a+b});
    a8 = {a,b,c,d,e,f,g,h}
    return a8
end function

procedure iRandInit()
    {aa,bb,cc} = {0,0,0}
    sequence a8 = repeat(#9e3779b9,8)   -- the golden ratio
    for i=1 to 4 do        -- scramble it
        a8 = mix(a8)
    end for
    for i=1 to 255 by 8 do
        a8 = mix(sq_add(a8,randrsl[i..i+7]))
        mm[i..i+7] = a8
    end for
    for i=1 to 255 by 8 do
        a8 = mix(r32(sq_add(a8,mm[i..i+7])))
        mm[i..i+7] = a8
    end for
    Isaac()         -- fill in the first set of results
end procedure

procedure iSeed(string seed)
    mm = repeat(0,256)
    randrsl = repeat(0,256)
    randrsl[1..min(length(seed),256)] = seed
    iRandInit()
end procedure

function randch()
    atom res = mod(randrsl[randcnt],95)+32
    randcnt += 1
    if randcnt>256 then
        Isaac()
    end if
    return res
end function

function Vernam(string msg)
    string res = ""
    for i=1 to length(msg) do
        res &= xor_bits(msg[i],randch())
    end for
    return res
end function

function Caesar(integer ch, shift)
    return ' '+mod(ch-' '+shift,95)
end function

enum ENCRYPT = +1,
     DECRYPT = -1

function Vigenere(string msg, integer mode)
    string res = ""
    for i=1 to length(msg) do
        res &= Caesar(msg[i],randch()*mode)
    end for
    return res
end function

constant string msg = "a Top Secret secret",
                key = "this is my secret key"

iSeed(key)
string xctx := Vernam(msg),
       mctx := Vigenere(msg,ENCRYPT)

iSeed(key)
string xptx := Vernam(xctx),
       mptx := Vigenere(mctx,DECRYPT)

function ascii2hex(string s)
    string res = ""
    for i=1 to length(s) do
        res &= sprintf("%02x",s[i])
    end for
    return res
end function

printf(1,"Message: %s\n",{msg})
printf(1,"Key    : %s\n",{key})
printf(1,"XOR    : %s\n",{ascii2hex(xctx)})
printf(1,"MOD    : %s\n",{ascii2hex(mctx)})
printf(1,"XOR dcr: %s\n",{xptx})
printf(1,"MOD dcr: %s\n",{mptx})
```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
MOD    : 734270227D36772A783B4F2A5F206266236978
XOR dcr: a Top Secret secret
MOD dcr: a Top Secret secret

```



## PicoLisp


```PicoLisp
(de add32 @
   (mod32 (pass +)) )

(de mod32 (N)
   (& N `(hex "FFFFFFFF")) )

(de isaac()
   (let (Y 0  S (-13 6 -2 16 .))
      (setq *CC (add32 *CC 1))
      (setq *BB (add32 *BB *CC))
      (for (I . X) *MM
         (set (nth *MM I)
            (setq Y
               (add32
                  (get *MM (inc (% (>> 2 X) 256)))
                  (setq *AA
                     (add32
                        (x| *AA (>> (pop 'S) *AA))
                        (get *MM (inc (% (+ 127 I) 256))) ) )
                  *BB ) ) )
         (set (nth *RR I)
            (setq *BB
               (add32
                  (get *MM (inc (% (>> 10 Y) 256)))
                  X ) ) ) ) ) )

(de mixA()
   (let S (-11 2 -8 16 -10 4 -8 9 .)
      (for I 8
         (set (nth *A I)
            (mod32
               (x|
                  (get *A I)
                  (mod32
                     (>>
                        (pop 'S)
                        (get *A (inc (% I 8))) ) ) ) ) )
         (set (nth *A (inc (% (+ 2 I) 8)))
            (add32
               (get *A (inc (% (+ 2 I) 8)))
               (get *A I) ) )
         (set (nth *A (inc (% I 8)))
            (add32
               (get *A (inc (% I 8)))
               (get *A (inc (% (inc I) 8))) ) ) ) ) )

(de iseed ()
   (do 4
      (mixA) )
   (for (I 1 (> 256 I) (inc 'I 8))
      (for (J I (> (+ 8 I) J) (inc J))
         (set (nth *A (inc (% (dec J) 8)))
            (add32
               (get *A (inc (% (dec J) 8)))
               (get *RR J) ) ) )
      (mixA)
      (for (J I (> (+ 8 I) J) (inc J))
         (set (nth *MM J)
            (get *A (inc (% (dec J) 8))) ) ) )
   (for (I 1 (> 256 I) (inc 'I 8))
      (for (J I (> (+ 8 I) J) (inc J))
         (set (nth *A (inc (% (dec J) 8)))
            (add32
               (get *A (inc (% (dec J) 8)))
               (get *MM J) ) ) )
      (mixA)
      (for (J I (> (+ 8 I) J) (inc J))
         (set (nth *MM J)
            (get *A (inc (% (dec J) 8))) ) ) )
   (isaac) )

(let
   (*AA 0
      *BB 0
      *CC 0
      *MM (need 256 0)
      *RC 0
      *RR (need
            -256
            (mapcar
               char
               (head 256 (chop "this is my secret key")) ) 0 )
      *A (need 8 `(hex "9E3779B9")) )
   (iseed)
   (println
      (pack
         (mapcar
            '((B) (pad 2 (hex B)))
            (make
               (for I (mapcar char (chop "a Top Secret secret"))
                  (link
                     (x|
                        I
                        (+
                           32
                           (%
                              (get
                                 *RR
                                 (if (>= 256 (inc '*RC))
                                    *RC
                                    (isaac)
                                    (one *RC) ) )
                              95 ) ) ) ) ) ) ) ) ) )

(bye)
```



## Python


Python doesn't have integer overflow (integers are handled as bignums if they don't fit into a machine word), so we need to emulate it manually by masking off the high bits after each addition and left shift.

This implementation extends the Random class of the built-in random module, so it automatically inherits methods for generating several distributions, as well as support for shuffling and sampling collections.


```Python
import random
import collections

INT_MASK = 0xFFFFFFFF       # we use this to emulate 32-bit overflow semantics by masking off higher bits after operations

class IsaacRandom(random.Random):
    """
    Random number generator using the ISAAC algorithm.
    """

    def seed(self, seed=None):
        """
        Initialize internal state.

        The seed, if given, can be a string, an integer, or an iterable that contains
        integers only. If no seed is given, a fixed default state is set up; unlike
        our superclass, this class will not attempt to randomize the seed from outside sources.
        """
        def mix():
            init_state[0] ^= ((init_state[1]<<11)&INT_MASK); init_state[3] += init_state[0]; init_state[3] &= INT_MASK; init_state[1] += init_state[2]; init_state[1] &= INT_MASK
            init_state[1] ^=  (init_state[2]>>2)           ; init_state[4] += init_state[1]; init_state[4] &= INT_MASK; init_state[2] += init_state[3]; init_state[2] &= INT_MASK
            init_state[2] ^= ((init_state[3]<<8 )&INT_MASK); init_state[5] += init_state[2]; init_state[5] &= INT_MASK; init_state[3] += init_state[4]; init_state[3] &= INT_MASK
            init_state[3] ^=  (init_state[4]>>16)          ; init_state[6] += init_state[3]; init_state[6] &= INT_MASK; init_state[4] += init_state[5]; init_state[4] &= INT_MASK
            init_state[4] ^= ((init_state[5]<<10)&INT_MASK); init_state[7] += init_state[4]; init_state[7] &= INT_MASK; init_state[5] += init_state[6]; init_state[5] &= INT_MASK
            init_state[5] ^=  (init_state[6]>>4 )          ; init_state[0] += init_state[5]; init_state[0] &= INT_MASK; init_state[6] += init_state[7]; init_state[6] &= INT_MASK
            init_state[6] ^= ((init_state[7]<<8 )&INT_MASK); init_state[1] += init_state[6]; init_state[1] &= INT_MASK; init_state[7] += init_state[0]; init_state[7] &= INT_MASK
            init_state[7] ^=  (init_state[0]>>9 )          ; init_state[2] += init_state[7]; init_state[2] &= INT_MASK; init_state[0] += init_state[1]; init_state[0] &= INT_MASK

        super().seed(0) # give a chance for the superclass to reset its state - the actual seed given to it doesn't matter
        if seed is not None:
            if isinstance(seed, str):
                seed = [ord(x) for x in seed]
            elif isinstance(seed, collections.Iterable):
                seed = [x & INT_MASK for x in seed]
            elif isinstance(seed, int):
                val = abs(seed)
                seed = []
                while val:
                    seed.append(val & INT_MASK)
                    val >>= 32
            else:
                raise TypeError('Seed must be string, integer or iterable of integer')

            # make sure the seed list is exactly 256 elements long
            if len(seed)>256:
                del seed[256:]
            elif len(seed)<256:
                seed.extend([0]*(256-len(seed)))

        self.aa = self.bb = self.cc = 0
        self.mm = []
        init_state = [0x9e3779b9]*8

        for _ in range(4):
            mix()

        for i in range(0, 256, 8):
            if seed is not None:
                for j in range(8):
                    init_state[j] += seed[i+j]
                    init_state[j] &= INT_MASK
            mix()
            self.mm += init_state

        if seed is not None:
            for i in range(0, 256, 8):
                for j in range(8):
                    init_state[j] += self.mm[i+j]
                    init_state[j] &= INT_MASK
                mix()
                for j in range(8):
                    self.mm[i+j] = init_state[j]

        self.rand_count = 256
        self.rand_result = [0]*256

    def getstate(self):
        return super().getstate(), self.aa, self.bb, self.cc, self.mm, self.rand_count, self.rand_result

    def setstate(self, state):
        super().setstate(state[0])
        _, self.aa, self.bb, self.cc, self.mm, self.rand_count, self.rand_result = state

    def _generate(self):
        # Generate 256 random 32-bit values and save them in an internal field.
        # The actual random functions will dish out these values to callers.
        self.cc = (self.cc + 1) & INT_MASK
        self.bb = (self.bb + self.cc) & INT_MASK

        for i in range(256):
            x = self.mm[i]
            mod = i & 3
            if mod==0:
                self.aa ^= ((self.aa << 13) & INT_MASK)
            elif mod==1:
                self.aa ^= (self.aa >> 6)
            elif mod==2:
                self.aa ^= ((self.aa << 2) & INT_MASK)
            else: # mod == 3
                self.aa ^= (self.aa >> 16)
            self.aa = (self.mm[i^128] + self.aa) & INT_MASK
            y = self.mm[i] = (self.mm[(x>>2) & 0xFF] + self.aa + self.bb) & INT_MASK
            self.rand_result[i] = self.bb = (self.mm[(y>>10) & 0xFF] + x) & INT_MASK

        self.rand_count = 0

    def next_int(self):
        """Return a random integer between 0 (inclusive) and 2**32 (exclusive)."""
        if self.rand_count == 256:
            self._generate()
        result = self.rand_result[self.rand_count]
        self.rand_count += 1
        return result

    def getrandbits(self, k):
        """Return a random integer between 0 (inclusive) and 2**k (exclusive)."""
        result = 0
        ints_needed = (k+31)//32
        ints_used = 0
        while ints_used < ints_needed:
            if self.rand_count == 256:
                self._generate()
            ints_to_take = min(256-self.rand_count, ints_needed)
            for val in self.rand_result[self.rand_count : self.rand_count+ints_to_take]:
                result = (result << 32) | val
            self.rand_count += ints_to_take
            ints_used += ints_to_take
        result &= ((1<<k)-1)    # mask off extra bits, if any
        return result

    def random(self):
        """Return a random float between 0 (inclusive) and 1 (exclusive)."""
        # A double stores 53 significant bits, so scale a 53-bit integer into the [0..1) range.
        return self.getrandbits(53) * (2**-53)

    def rand_char(self):
        """Return a random integer from the printable ASCII range [32..126]."""
        return self.next_int() % 95 + 32

    def vernam(self, msg):
        """
        Encrypt/decrypt the given bytes object with the XOR algorithm, using the current generator state.

        To decrypt an encrypted string, restore the state of the generator to the state it had
        during encryption, then call this method with the encrypted string.
        """
        return bytes((self.rand_char() & 0xFF) ^ x for x in msg)

    # Constants for selecting Caesar operation modes.
    ENCIPHER = 'encipher'
    DECIPHER = 'decipher'

    @staticmethod
    def _caesar(ciphermode, ch, shift, modulo, start):
        if ciphermode == IsaacRandom.DECIPHER:
            shift = -shift
        n = ((ch-start)+shift) % modulo
        if n<0:
            n += modulo
        return start+n

    def caesar(self, ciphermode, msg, modulo, start):
        """
        Encrypt/decrypt a string using the Caesar algorithm.

        For decryption to work, the generator must be in the same state it was during encryption,
        and the same modulo and start parameters must be used.

        ciphermode must be one of IsaacRandom.ENCIPHER or IsaacRandom.DECIPHER.
        """
        return bytes(self._caesar(ciphermode, ch, self.rand_char(), modulo, start) for ch in msg)

if __name__=='__main__':
    import binascii

    def hexify(b):
        return binascii.hexlify(b).decode('ascii').upper()

    MOD = 95
    START = 32

    msg = 'a Top Secret secret'
    key = 'this is my secret key'
    isaac_random = IsaacRandom(key)
    vernam_encoded = isaac_random.vernam(msg.encode('ascii'))
    caesar_encoded = isaac_random.caesar(IsaacRandom.ENCIPHER, msg.encode('ascii'), MOD, START)
    isaac_random.seed(key)
    vernam_decoded = isaac_random.vernam(vernam_encoded).decode('ascii')
    caesar_decoded = isaac_random.caesar(IsaacRandom.DECIPHER, caesar_encoded, MOD, START).decode('ascii')

    print('Message:', msg)
    print('Key    :', key)
    print('XOR    :', hexify(vernam_encoded))
    print('XOR dcr:', vernam_decoded)
    print('MOD    :', hexify(caesar_encoded))
    print('MOD dcr:', caesar_decoded)

```



## Racket


- Imperative version: {{trans|C}}
- Vigenère:           {{trans|Pascal}}

In the Pascal (and reference version) of the Vigenère encryption,
the state engine is not reset after having been used for the XOR version.
There are two sets of MOD results below... one with the state engine
left from after the XOR, and one with a cleanly reseeded state engine.


```racket
#lang racket
;; Imperative version: Translation of C
;; Vigenère:           Translation of Pascal
(module+ test (require tests/eli-tester))

;; ---------------------------------------------------------------------------------------------------
;; standard.h: Standard definitions and types, Bob Jenkins
(define UB4MAXVAL #xffffffff)
(define-syntax-rule (bit target mask) (bitwise-and target mask))
;; C-like operators
(define-syntax-rule (u4-truncate x) (bit x UB4MAXVAL))
(define-syntax-rule (u4<< a b)      (u4-truncate (arithmetic-shift a b)))
(define-syntax-rule (u4>> a b)      (u4-truncate (arithmetic-shift a (- b))))
(define-syntax-rule (_++ i)         (let ((rv i)) (set! i (u4-truncate (add1 i))) rv))
(define-syntax-rule (u4+= a b)      (begin (set! a (u4-truncate (+ a b))) a))
(define-syntax-rule (^= a b)        (begin (set! a (u4-truncate (bitwise-xor a b))) a))

;; ---------------------------------------------------------------------------------------------------
;; rand.h: definitions for a random number generator
(define RANDSIZL  8)
(define RANDSIZ   (u4<< 1 RANDSIZL))
(define RANDSIZ-1 (sub1 RANDSIZ))

(struct randctx
  (cnt
   rsl ; RANDSIZ*4 bytes (makes u4's)
   mem ; RANDSIZ*4 bytes (makes u4's)
   a b c) #:mutable)

(define (new-randctx)
  (randctx 0 (make-bytes (* 4 RANDSIZ) 0) (make-bytes (* 4 RANDSIZ) 0) 0 0 0))

(define (bytes->hex-string B (start 0) (end #f) #:join (join "") #:show-bytes? (show-bytes? #f))
  (define hexes
    (for/list ((b (in-bytes B start end)))
      (~a (number->string b 16) #:width 2 #:align 'right #:pad-string "0")))
  (string-join
   (append hexes (if show-bytes?
                     (list " \"" (bytes->string/utf-8 B #f start (or end (bytes-length B))) "\"")
                     null))
   join))

(define format-randctx
  (match-lambda
    [(randctx C (app bytes->hex-string R) (app bytes->hex-string M) a b c)
     (format "randctx: cnt:~a~%rsl:~s~%mem:~s~%a:~a b:~a c:~a" C R M a b c)]))

(define be? (system-big-endian?))

(define (bytes->u4 ary idx)
  (integer-bytes->integer ary #f be? (* idx 4) (* (add1 idx) 4)))

(define (u4->bytes! ary idx v)
  (integer->integer-bytes (bit v UB4MAXVAL) 4 #f be? ary (* idx 4)))

;; ---------------------------------------------------------------------------------------------------
;; rand.c: "By Bob Jenkins.  My random number generator, ISAAC.  Public Domain."
(define (ind mm x)
  (define idx (bitwise-and x (u4<< RANDSIZ-1 2)))
  (integer-bytes->integer mm #f be? idx (+ idx 4)))

(define (isaac C)
  (define M (randctx-mem C))
  (define R (randctx-rsl C))
  (define mm 0)
  (define r  0)
  (define-syntax-rule (rng-step mix)
    (begin
      (define x (bytes->u4 M m))
      (set! a (u4-truncate (+ (bitwise-xor a mix) (bytes->u4 M (_++ m2)))))
      (define y (+ (ind M x) a b))
      (u4->bytes! M (_++ m) y)
      (set! b (u4-truncate (+ (ind M (u4>> y RANDSIZL)) x)))
      (u4->bytes! R (_++ r) b)))

  (define a (randctx-a C))

  (set-randctx-c! C (add1 (randctx-c C)))

  (define b (u4-truncate (+ (randctx-b C) (randctx-c C))))

  (define m mm)
  (define m2 (+ m (/ RANDSIZ 2)))
  (define mend m2)

  (define-syntax-rule (4-step-loop variant)
    (let loop ()
      (when (< variant mend)
        (rng-step (u4<< a 13)) (rng-step (u4>> a 6))
        (rng-step (u4<< a  2)) (rng-step (u4>> a 16))
        (loop))))

  (4-step-loop m)
  (set! m2 mm)
  (4-step-loop m2)

  (set-randctx-b! C b)
  (set-randctx-a! C a))

;; dot infix notation because I'm too lazy to move the operators left!
(define-syntax-rule (mix-line<< A B N D C)
  (begin (A . ^= . (B . u4<< . N)) (D . u4+= . A) (B . u4+= . C)))
(define-syntax-rule (mix-line>> A B N D C)
  (begin (A . ^= . (B . u4>> . N)) (D . u4+= . A) (B . u4+= . C)))

(define-syntax-rule (mix a b c d e f g h)
  (begin (mix-line<< a b 11 d c) (mix-line>> b c  2 e d)
         (mix-line<< c d  8 f e) (mix-line>> d e 16 g f)
         (mix-line<< e f 10 h g) (mix-line>> f g  4 a h)
         (mix-line<< g h  8 b a) (mix-line>> h a  9 c b)))

;; if (flag==TRUE), then use the contents of randrsl[] to initialize mm[].
(define (rand-init C flag?)
  (set-randctx-a! C 0)
  (set-randctx-b! C 0)
  (set-randctx-c! C 0)

  ;; seed-ctx should set these up (with the seed!):
  ;;   (set-ctx-rsl! C (make-bytes (* 4 RANDSIZ) 0))
  ;;   (set-ctx-mem! C (make-bytes (* 4 RANDSIZ) 0))
  (define R (randctx-rsl C))
  (define M (randctx-mem C))

  (define φ #x9e3779b9) ; the golden ratio
  (match-define (list a b c d e f g h) (make-list 8 φ))

  (for ((_ 4)) (mix a b c d e f g h)) ; scramble it

  (define-syntax-rule (mix-and-assign i M2)
    (begin
      (mix a b c d e f g h)
      (u4->bytes! M2 (+ i 0) a) (u4->bytes! M2 (+ i 1) b)
      (u4->bytes! M2 (+ i 2) c) (u4->bytes! M2 (+ i 3) d)
      (u4->bytes! M2 (+ i 4) e) (u4->bytes! M2 (+ i 5) f)
      (u4->bytes! M2 (+ i 6) g) (u4->bytes! M2 (+ i 7) h)))

  (define-syntax-rule (mix-with-mem M1 M2)
    (for ((i (in-range 0 RANDSIZ 8)))
      (a . u4+= . (bytes->u4 M1 (+ i 0))) (b . u4+= . (bytes->u4 M1 (+ i 1)))
      (c . u4+= . (bytes->u4 M1 (+ i 2))) (d . u4+= . (bytes->u4 M1 (+ i 3)))
      (e . u4+= . (bytes->u4 M1 (+ i 4))) (f . u4+= . (bytes->u4 M1 (+ i 5)))
      (g . u4+= . (bytes->u4 M1 (+ i 6))) (h . u4+= . (bytes->u4 M1 (+ i 7)))
      (mix-and-assign i M2)))

  (cond
    [flag? ; initialize using the contents of r[] as the seed
     (mix-with-mem R M)
     (mix-with-mem M M)] ; do a second pass to make all of the seed affect all of m
    [else ; fill in m[] with messy stuff
     (for ((i (in-range 0 RANDSIZ 8))) (mix-and-assign i M))])

  (isaac C)  ; fill in the first set of results
  (set-randctx-cnt! C 0)) ; prepare to use the first set of results

(define (seed-ctx C key #:flag? (flag? #t))
  (bytes-fill! (randctx-mem C) 0)
  (define R (randctx-rsl C))
  (bytes-fill! (randctx-rsl C) 0)
  (for ((k (in-bytes key)) (i (in-range (quotient (bytes-length R) 4)))) (u4->bytes! R i k))
  (rand-init C flag?))

;; Get a random 32-bit value 0..MAXINT
(define (i-random C)
  (define cnt (randctx-cnt C))
  (define r (bytes->u4 (randctx-rsl C) cnt))
  (define cnt+1 (add1 cnt))
  (cond [(>= cnt+1 RANDSIZ) (isaac C) (set-randctx-cnt! C 0)]
        [else (set-randctx-cnt! C cnt+1)])
  r)

;; Get a random character in printable ASCII range
(define ((i-rand-a C))
  (+ 32 (modulo (i-random C) 95)))

(define (Vernham rnd-fn msg)
  (define gsm (make-bytes (bytes-length msg)))
  (for ((i (in-naturals)) (m (in-bytes msg)))
    (define r (rnd-fn))
    (define b (bitwise-xor m r))
    (bytes-set! gsm i b))
  gsm)

;; Get position of the letter in chosen alphabet
;; Caesar-shift a character <shift> places: Generalized Vigenere
(define ((Caesar mod-n start) encrypt? shift ch)
  (define (letter-num letter/byte)
    (- letter/byte (char->integer start)))

  (define shift-fn (if encrypt? + -))
  (+ (char->integer start) (modulo (shift-fn (letter-num ch) shift) mod-n)))

;; Vigenère mod 95 encryption & decryption. Output: bytes
(define Vigenère-Caeser (Caesar 95 #\space))
(define (Vigenère encrypt? rand-fn msg)
  (list->bytes
   (for/list ((b (in-bytes msg)))
     (Vigenère-Caeser encrypt? (rand-fn) b))))

{module+ main
  (define message #"a Top Secret secret")
  (define key     #"this is my secret key")
  (define C (new-randctx))
  (seed-ctx C key)
  (define vern.msg (Vernham (i-rand-a C) message))
  ;; Pascal doesn't reset the context betwen XOR and MOD
  ;; (seed-ctx C key)
  (define vigen.msg (Vigenère #t (i-rand-a C) message))
  (seed-ctx C key)
  (define vern2.msg (Vernham (i-rand-a C) vern.msg))
  ;; Pascal doesn't reset the context betwen XOR and MOD
  ;; (seed-ctx C key)
  (define unvigen.msg (Vigenère #f (i-rand-a C) vigen.msg))
  ;; This is what MOD looks like from the context as seeded with key
  (seed-ctx C key)
  (define vigen-at-seed.msg (Vigenère #t (i-rand-a C) message))
  (seed-ctx C key)
  (define unvigen-at-seed.msg (Vigenère #f (i-rand-a C) vigen-at-seed.msg))

  (printf #<<EOS
Message:            [~a]
Key:                [~a]

                    < context reseeded
Vernham (XOR):      [~a]
Vigenère (MOD):     [~a]

                    < context reseeded
Vernham (XOR(XOR)): [~a]
Vigenère (-MOD):    [~a]

                    < context reseeded (different to Pascal Vigenère encryption)
Vigenère (MOD):     [~a]
                    < context reseeded
Vigenère (-MOD):    [~a]
EOS
          message
          key
          (bytes->hex-string vern.msg)
          (bytes->hex-string vigen.msg #:show-bytes? #t)
          (bytes->hex-string vern2.msg #:show-bytes? #t)
          (bytes->hex-string unvigen.msg #:show-bytes? #t)
          (bytes->hex-string vigen-at-seed.msg #:show-bytes? #t)
          (bytes->hex-string unvigen-at-seed.msg #:show-bytes? #t)
          )}

{module+ test
  ;; "If the initial internal state is all zero, after ten calls the values of aa, bb, and cc in
  ;; hexadecimal will be d4d3f473, 902c0691, and 0000000a."
  (let ()
    (define C (new-randctx))
    (for ((_ 10)) (isaac C))
    (test (randctx-a C) => #xd4d3f473
          (randctx-b C) => #x902c0691
          (randctx-c C) => 10))
  }
```


```txt
Message:            [a Top Secret secret]
Key:                [this is my secret key]

                    < context reseeded
Vernham (XOR):      [1c0636190b1260233b35125f1e1d0e2f4c5422]
Vigenère (MOD):     [734270227d36772a783b4f2a5f206266236978 "sBp"}6w*x;O*_ bf#ix"]

                    < context reseeded
Vernham (XOR(XOR)): [6120546f702053656372657420736563726574 "a Top Secret secret"]
Vigenère (-MOD):    [6120546f702053656372657420736563726574 "a Top Secret secret"]

                    < context reseeded (different to Pascal Vigenère encryption)
Vigenère (MOD):     [204657272d52274c5c5a7d405e23715051376b " FW'-R'L\Z}@^#qPQ7k"]
                    < context reseeded
Vigenère (-MOD):    [6120546f702053656372657420736563726574 "a Top Secret secret"]
```



## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* 24.07.2014 Walter Pachl translated from Pascal
*            extend with decryption (following Pascal)
* 25.07.2014 WP changed i+=8 to I=I+8 (courtesy GS)
* 26.07-2014 WP removed extraneous semicolons
*--------------------------------------------------------------------*/
Numeric Digits 32
aa=0
bb=0
cc=0
mm.=0
randcnt=0
randrsl.=0
msg='a Top Secret secret'
key='this is my secret key'
iMode='iEncrypt'

Call iSeed key,1          /* 1) seed ISAAC with the key */
xctx=Vernam(msg)          /* 2) Vernam XOR encryption   */
mode='iEncrypt'
mctx=Vigenere(msg,mode)   /* 3) MOD encryption          */
Call iSeed key,1
xptx=Vernam(xctx)         /* a) XOR (Vernam)            */
mode='iDecrypt'
mptx=Vigenere(mctx,mode)  /* b) MOD (Vigenere)          */
                          /* program output             */
Say 'Message: 'msg
Say 'Key    : 'key
Say 'XOR    : 'c2x(xctx)
Say 'MOD    : 'c2x(mctx)
Say 'XOR dcr: 'xptx
Say 'MOD dcr: 'mptx
Exit

isaac: Procedure      Expose mm. aa bb cc randrsl. randcnt
  cc=add(cc,1)
  bb=add(bb,cc)
  Do i=0 To 255
    x=mm.i
    im4=i//4
    Select
      When im4=0 Then aa=xor(aa,shl(aa,13))
      When im4=1 Then aa=xor(aa,shr(aa, 6))
      When im4=2 Then aa=xor(aa,shl(aa, 2))
      When im4=3 Then aa=xor(aa,shr(aa,16))
      End
    z=(i+128)//256
    aa=add(mm.z,aa)
    z=shr(x,2)//256
    y=add(mm.z,aa,bb)
    mm.i=y
    z=shr(y,10)//256
    bb=add(mm.z,x)
    randrsl.i=bb
    End
  randcnt=0
  Return

mix: Procedure        Expose a b c d e f g h mm. aa bb cc randrsl. randcnt
  a=xor(a,shl(b,11)); d=add(d,a); b=add(b,c)
  b=xor(b,shr(c, 2)); e=add(e,b); c=add(c,d)
  c=xor(c,shl(d, 8)); f=add(f,c); d=add(d,e)
  d=xor(d,shr(e,16)); g=add(g,d); e=add(e,f)
  e=xor(e,shl(f,10)); h=add(h,e); f=add(f,g)
  f=xor(f,shr(g, 4)); a=add(a,f); g=add(g,h)
  g=xor(g,shl(h, 8)); b=add(b,g); h=add(h,a)
  h=xor(h,shr(a, 9)); c=add(c,h); a=add(a,b)
  Return

iRandInit: Procedure  Expose mm. randrsl. randcnt
  Parse Arg flag
  aa=0; bb=0; cc=0
  a= 2654435769 /* $9e3779b9;        // the golden ratio */

  b=a; c=a; d=a; e=a; f=a; g=a; h=a

  do i=0 TO 3
    Call mix
    End

  i=0
  do until i>255    /* fill in mm[] with messy stuff */
    IF flag THEN Do /* use all the information in the seed */
      Call setix
      a=add(a,randrsl.i);  b=add(b,randrsl.i1)
      c=add(c,randrsl.i2); d=add(d,randrsl.i3)
      e=add(e,randrsl.i4); f=add(f,randrsl.i5)
      g=add(g,randrsl.i6); h=add(h,randrsl.i7)
      End
    Call mix
    mm.i=a;  mm.i1=b; mm.i2=c; mm.i3=d
    mm.i4=e; mm.i5=f; mm.i6=g; mm.i7=h
    i=i+8
    End

  IF flag THEN Do /* do a second pass to make all of the seed affect all of mm  */
    i=0
    do until i>255    /* fill in mm[] with messy stuff */
      Call setix
      a=add(a,mm.i);  b=add(b,mm.i1); c=add(c,mm.i2); d=add(d,mm.i3)
      e=add(e,mm.i4); f=add(f,mm.i5); g=add(g,mm.i6); h=add(h,mm.i7)
      Call mix
      mm.i=a;  mm.i1=b; mm.i2=c; mm.i3=d
      mm.i4=e; mm.i5=f; mm.i6=g; mm.i7=h
      i=i+8
      End
    End
  Call isaac       /* fill in the first set of results        */
  randcnt=0;       /* prepare to use the first set of results */
  Return

iseed: Procedure      Expose aa bb cc randcnt randrsl. mm.
/*---------------------------------------------------------------------
* Seed ISAAC with a given string.
*  The string can be any size. The first 256 values will be used.
*--------------------------------------------------------------------*/
  Parse Arg seed,flag
  mm.=0
  m=Length(seed)-1
  Do i=0 TO 255
    IF i>m THEN   /* in case seed has less than 256 elements */
      randrsl.i=0
    ELSE
      randrsl.i=c2d(substr(seed,i+1,1))
    end
  Call iRandInit flag   /* initialize ISAAC with seed */
  Return

iRandom: Procedure    Expose aa bb cc randcnt randrsl. mm.
/* Get a random 32-bit value 0..MAXINT */
  iRandom=randrsl.randcnt
  randcnt=randcnt+1
  If randcnt>255 Then Do
    Call isaac
    randcnt=0
    End
  Return irandom

iRandA: Procedure     Expose aa bb cc randcnt randrsl. mm.
/* Get a random character in printable ASCII range */
  iRandA=iRandom()//95+32
  Return iRandA

xor: Procedure        Expose aa bb cc randcnt randrsl. mm.
  Parse Arg a,b
  ac=d2c(a,4)
  bc=d2c(b,4)
  res=c2d(bitxor(ac,bc))
  return res//4294967296

Vernam: Procedure     Expose aa bb cc randcnt randrsl. mm.
/* XOR encrypt on random stream. Output: string of hex chars */
  Parse Arg msg
  Vernam=''
  Do i=1 to length(msg)
    Vernam=Vernam||d2c(xor(iRandA(),c2d(substr(msg,i,1))))
    End
  Return Vernam

letternum: Procedure  Expose aa bb cc randcnt randrsl. mm.
/* Get position of the letter in chosen alphabet */
  Parse Arg letter,start
  letternum=c2d(letter)-c2d(start)
  Return letternum

Caesar: Procedure     Expose aa bb cc randcnt randrsl. mm.
/* Caesar-shift a character <shift> places: Generalized Vigenere */
  Parse Arg m,ch,shift,modulo,start
  IF m='iDecrypt' TheN shift=-shift
  n=letternum(ch,start)+shift
  n=n//modulo
  IF n<0 Then n=n+modulo
  Caesar=d2c(c2d(start)+n)
  Return Caesar

Vigenere: Procedure   Expose aa bb cc randcnt randrsl. mm.
/* Vigenere mod 95 encryption. Output: string of hex chars */
  Parse Arg msg,m
  Vigenere=''
  Do i=1 to length(msg)
    Vigenere=Vigenere||Caesar(m,substr(msg,i,1),iRandA(),95,' ')
    End
  Return Vigenere

shl: Procedure
  res=arg(1)*(2**arg(2))
  return res//4294967296

shr: Procedure
  res=arg(1)%(2**arg(2))
  return res//4294967296

setix:
  i1=i+1
  i2=i+2
  i3=i+3
  i4=i+4
  i5=i+5
  i6=i+6
  i7=i+7
  Return

add: Procedure
/* add argumemnts modulo 4294967296 */
  res=arg(1)+arg(2)
  If arg(3)<>'' Then
    res=res+arg(3)
  return res//4294967296
```

```txt
Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
MOD    : 734270227D36772A783B4F2A5F206266236978
XOR dcr: a Top Secret secret
MOD dcr: a Top Secret secret
```



### version 2

This can be used to encrypt a file and thereafter decrypt it.

```rexx
/* REXX ---------------------------------------------------------------
* 25.07.2014 Walter Pachl framing version 1 for processing a file
*--------------------------------------------------------------------*/
Parse Arg fid
select
  When fid='' Then
    fid='test_file.txt'
  When fid='?' Then Do
    Say 'rexx iscf file prompts you for a key,'
    Say 'encrypts file into fn.enc'
    Say 'and decrypts fn.enc into fn.dec'
    Exit
    End
  Otherwise
    Nop
  End
Say 'Please enter a key'
Parse Pull key
enc=fn(fid)'.enc' ; 'erase' enc
dec=fn(fid)'.dec' ; 'erase' dec
Do While lines(fid)>0
  l=linein(fid)
  Call lineout enc,iscx(l,key,'e')
  End
Call lineout enc
Do While lines(enc)>0
  l=linein(enc)
  Call lineout dec,iscx(l,key,'d')
  End
Call lineout dec
Say 'original:'
'type' fid
Say 'encrypted:'
'type' enc
Say 'decrypted:'
'type' dec
Exit
iscx: Procedure
/* REXX ---------------------------------------------------------------
* 24.07.2014 Walter Pachl translated from Pascal
*            extend with decoding
*--------------------------------------------------------------------*/
Numeric Digits 32
aa=0
bb=0
cc=0
mm.=0
randcnt=0
randrsl.=0
Parse Arg msg,key,mode

Call iSeed key,1          /* 1) seed ISAAC with the key */
If mode='e' Then
  mode='iEncrypt'
Else
  mode='iDecrypt'
mctx=Vigenere(msg,mode)   /* 3) MOD encryption          */
Return mctx

isaac: Procedure      Expose mm. aa bb cc randrsl. randcnt
  cc=add(cc,1)
  bb=add(bb,cc)
  Do i=0 To 255
    x=mm.i
    im4=i//4
    Select
      When im4=0 Then aa=xor(aa,shl(aa,13))
      When im4=1 Then aa=xor(aa,shr(aa, 6))
      When im4=2 Then aa=xor(aa,shl(aa, 2))
      When im4=3 Then aa=xor(aa,shr(aa,16))
      End
    z=(i+128)//256
    aa=add(mm.z,aa)
    z=shr(x,2)//256
    y=add(mm.z,aa,bb)
    mm.i=y
    z=shr(y,10)//256
    bb=add(mm.z,x)
    randrsl.i=bb
    End
  randcnt=0
  Return

mix: Procedure        Expose a b c d e f g h mm. aa bb cc randrsl. randcnt
  a=xor(a,shl(b,11)); d=add(d,a); b=add(b,c)
  b=xor(b,shr(c, 2)); e=add(e,b); c=add(c,d)
  c=xor(c,shl(d, 8)); f=add(f,c); d=add(d,e)
  d=xor(d,shr(e,16)); g=add(g,d); e=add(e,f)
  e=xor(e,shl(f,10)); h=add(h,e); f=add(f,g)
  f=xor(f,shr(g, 4)); a=add(a,f); g=add(g,h)
  g=xor(g,shl(h, 8)); b=add(b,g); h=add(h,a)
  h=xor(h,shr(a, 9)); c=add(c,h); a=add(a,b)
  Return

iRandInit: Procedure  Expose mm. randrsl. randcnt
  Parse Arg flag
  aa=0; bb=0; cc=0
  a= 2654435769 /* $9e3779b9;        // the golden ratio */

  b=a; c=a; d=a; e=a; f=a; g=a; h=a

  do i=0 TO 3
    Call mix
    End

  i=0
  do until i>255    /* fill in mm[] with messy stuff */
    IF flag THEN Do /* use all the information in the seed */
      Call setix
      a=add(a,randrsl.i);  b=add(b,randrsl.i1)
      c=add(c,randrsl.i2); d=add(d,randrsl.i3)
      e=add(e,randrsl.i4); f=add(f,randrsl.i5)
      g=add(g,randrsl.i6); h=add(h,randrsl.i7)
      End
    Call mix
    mm.i=a;  mm.i1=b; mm.i2=c; mm.i3=d
    mm.i4=e; mm.i5=f; mm.i6=g; mm.i7=h
    i+=8
    End

  IF flag THEN Do /* do a second pass to make all of the seed affect all of mm  */
    i=0
    do until i>255    /* fill in mm[] with messy stuff */
      Call setix
      a=add(a,mm.i);  b=add(b,mm.i1); c=add(c,mm.i2); d=add(d,mm.i3)
      e=add(e,mm.i4); f=add(f,mm.i5); g=add(g,mm.i6); h=add(h,mm.i7)
      Call mix
      mm.i=a;  mm.i1=b; mm.i2=c; mm.i3=d
      mm.i4=e; mm.i5=f; mm.i6=g; mm.i7=h
      i+=8
      End
    End
  Call isaac       /* fill in the first set of results        */
  randcnt=0;       /* prepare to use the first set of results */
  Return

iseed: Procedure      Expose aa bb cc randcnt randrsl. mm.
/*---------------------------------------------------------------------
* Seed ISAAC with a given string.
*  The string can be any size. The first 256 values will be used.
*--------------------------------------------------------------------*/
  Parse Arg seed,flag
  mm.=0
  m=Length(seed)-1
  Do i=0 TO 255
    IF i>m THEN   /* in case seed has less than 256 elements */
      randrsl.i=0
    ELSE
      randrsl.i=c2d(substr(seed,i+1,1))
    end
  Call iRandInit flag   /* initialize ISAAC with seed */
  Return

iRandom: Procedure    Expose aa bb cc randcnt randrsl. mm.
/* Get a random 32-bit value 0..MAXINT */
  iRandom=randrsl.randcnt
  randcnt=randcnt+1
  If randcnt>255 Then Do
    Call isaac
    randcnt=0
    End
  Return irandom

iRandA: Procedure     Expose aa bb cc randcnt randrsl. mm.
/* Get a random character in printable ASCII range */
  iRandA=iRandom()//95+32
  Return iRandA

xor: Procedure        Expose aa bb cc randcnt randrsl. mm.
  Parse Arg a,b
  ac=d2c(a,4)
  bc=d2c(b,4)
  res=c2d(bitxor(ac,bc))
  return res//4294967296

Vernam: Procedure     Expose aa bb cc randcnt randrsl. mm.
/* XOR encrypt on random stream. Output: string of hex chars */
  Parse Arg msg
  Vernam=''
  Do i=1 to length(msg)
    Vernam=Vernam||d2c(xor(iRandA(),c2d(substr(msg,i,1))))
    End
  Return Vernam

letternum: Procedure  Expose aa bb cc randcnt randrsl. mm.
/* Get position of the letter in chosen alphabet */
  Parse Arg letter,start
  letternum=c2d(letter)-c2d(start)
  Return letternum

Caesar: Procedure     Expose aa bb cc randcnt randrsl. mm.
/* Caesar-shift a character <shift> places: Generalized Vigenere */
  Parse Arg m,ch,shift,modulo,start
  IF m='iDecrypt' TheN shift=-shift
  n=letternum(ch,start)+shift
  n=n//modulo
  IF n<0 Then n=n+modulo
  Caesar=d2c(c2d(start)+n)
  Return Caesar

Vigenere: Procedure   Expose aa bb cc randcnt randrsl. mm.
/* Vigenere mod 95 encryption. Output: string of hex chars */
  Parse Arg msg,m
  Vigenere=''
  Do i=1 to length(msg)
    Vigenere=Vigenere||Caesar(m,substr(msg,i,1),iRandA(),95,' ')
    End
  Return Vigenere

shl: Procedure
  res=arg(1)*(2**arg(2))
  return res//4294967296

shr: Procedure
  res=arg(1)%(2**arg(2))
  return res//4294967296

setix:
  i1=i+1
  i2=i+2
  i3=i+3
  i4=i+4
  i5=i+5
  i6=i+6
  i7=i+7
  Return

add: Procedure
/* add argumemnts modulo 4294967296 */
  res=arg(1)+arg(2)
  If arg(3)<>'' Then
    res=res+arg(3)
  return res//4294967296

fn: Procedure
/* REXX */
parse Arg fid
Parse Var fid fn '.' ft
Return fn
```

```txt
Please enter a key
original:
This is a little test file
that shows my encryption
encrypted:
KrG"3n(sr_5=OiziiWnJ.`5RY=
kr?#3x|c)SHATtsrMU#G.J>W
decrypted:
This is a little test file
that shows my encryption
```



## Sidef

```ruby
require('Math::Random::ISAAC')

func xor_isaac(key, msg) {
  var rng = %O<Math::Random::ISAAC>.new(unpack('C*', key))

  msg.chars»ord()»                                          \
    -> »^« 256.of{ rng.irand % 95 + 32 }.last(msg.len).flip \
    -> «%« '%02X' -> join
}

var msg = 'a Top Secret secret'
var key = 'this is my secret key'

var enc = xor_isaac(key, msg)
var dec = xor_isaac(key, pack('H*', enc))

say "Message: #{msg}"
say "Key    : #{key}"
say "XOR    : #{enc}"
say "XOR dcr: #{pack('H*', dec)}"
```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1C0636190B1260233B35125F1E1D0E2F4C5422
XOR dcr: a Top Secret secret

```



## Tcl

```tcl
package require Tcl 8.6

oo::class create ISAAC {
    variable aa bb cc mm randrsl randcnt

    constructor {seed} {
	namespace eval tcl {
	    namespace eval mathfunc {
		proc mm {idx} {
		    upvar 1 mm list
		    lindex $list [expr {$idx % [llength $list]}]
		}
		proc clamp {value} {
		    expr {$value & 0xFFFFFFFF}
		}
	    }
	}
	proc mix1 {i v} {
	    upvar 1 a a
	    lset a $i [expr {clamp([lindex $a $i] ^ $v)}]
	    lset a [set idx [expr {($i+3)%8}]] \
		[expr {clamp([lindex $a $idx] + [lindex $a $i])}]
	    lset a [set idx [expr {($i+1)%8}]] \
		[expr {clamp([lindex $a $idx] + [lindex $a [expr {($i+2)%8}]])}]
	}

	binary scan $seed[string repeat \u0000 256] c256 randrsl
	set mm [lrepeat 256 0]
	set randcnt [set aa [set bb [set cc 0]]]

	set a [lrepeat 8 0x9e3779b9]
	foreach i {1 2 3 4} {
	    mix1 0 [expr {[lindex $a 1] << 11}]
	    mix1 1 [expr {[lindex $a 2] >> 2}]
	    mix1 2 [expr {[lindex $a 3] << 8}]
	    mix1 3 [expr {[lindex $a 4] >> 16}]
	    mix1 4 [expr {[lindex $a 5] << 10}]
	    mix1 5 [expr {[lindex $a 6] >> 4}]
	    mix1 6 [expr {[lindex $a 7] << 8}]
	    mix1 7 [expr {[lindex $a 0] >> 9}]
	}
	for {set i 0} {$i < 256} {incr i 8} {
	    set a [lmap av $a bv [lrange $randrsl $i [expr {$i+7}]] {
		expr {clamp($av + $bv)}
	    }]
	    mix1 0 [expr {[lindex $a 1] << 11}]
	    mix1 1 [expr {[lindex $a 2] >> 2}]
	    mix1 2 [expr {[lindex $a 3] << 8}]
	    mix1 3 [expr {[lindex $a 4] >> 16}]
	    mix1 4 [expr {[lindex $a 5] << 10}]
	    mix1 5 [expr {[lindex $a 6] >> 4}]
	    mix1 6 [expr {[lindex $a 7] << 8}]
	    mix1 7 [expr {[lindex $a 0] >> 9}]
	    for {set j 0} {$j < 8} {incr j} {
		lset mm [expr {$i+$j}] [lindex $a $j]
	    }
	}
	for {set i 0} {$i < 256} {incr i 8} {
	    set a [lmap av $a bv [lrange $mm $i [expr {$i+7}]] {
		expr {clamp($av + $bv)}
	    }]
	    mix1 0 [expr {[lindex $a 1] << 11}]
	    mix1 1 [expr {[lindex $a 2] >> 2}]
	    mix1 2 [expr {[lindex $a 3] << 8}]
	    mix1 3 [expr {[lindex $a 4] >> 16}]
	    mix1 4 [expr {[lindex $a 5] << 10}]
	    mix1 5 [expr {[lindex $a 6] >> 4}]
	    mix1 6 [expr {[lindex $a 7] << 8}]
	    mix1 7 [expr {[lindex $a 0] >> 9}]
	    for {set j 0} {$j < 8} {incr j} {
		lset mm [expr {$i+$j}] [lindex $a $j]
	    }
	}
	my Step
    }

    method Step {} {
	incr bb [incr cc]
	set i -1
	foreach x $mm {
	    set shift [lindex {13 -6 2 -16} [expr {[incr i] % 4}]]
	    set aa [expr {$aa ^ ($shift>0 ? $aa<<$shift : $aa>>-$shift)}]
	    set aa [expr {clamp($aa + mm($i+128))}]
	    set y [expr {clamp(mm($x>>2) + $aa + $bb)}]
	    lset mm $i $y
	    set bb [expr {clamp(mm($y>>10) + $x)}]
	    lset randrsl $i $bb
	}
    }

    method random {} {
	set r [lindex $randrsl $randcnt]
	if {[incr randcnt] == 256} {
	    my Step
	    set randcnt 0
	}
	return $r
    }

    method RandA {} {
	expr {([my random] % 95) + 32}
    }
    method vernam {msg} {
	binary scan $msg c* b
	for {set i 0} {$i < [llength $b]} {incr i} {
	    lset b $i [expr {[lindex $b $i] & 255 ^ [my RandA]}]
	}
	return [binary encode hex [binary format c* $b]]
    }
}
```

Demonstrating:

```tcl
set key "this is my secret key"
set msg "a Top Secret secret"
ISAAC create demo $key
puts "Message: $msg"
puts "Key    : $key"
puts "XOR    : [demo vernam $msg]"
```

```txt

Message: a Top Secret secret
Key    : this is my secret key
XOR    : 1c0636190b1260233b35125f1e1d0e2f4c5422

```

