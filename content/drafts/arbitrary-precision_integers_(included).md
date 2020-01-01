+++
title = "Arbitrary-precision integers (included)"
description = ""
date = 2019-09-16T20:44:55Z
aliases = []
[extra]
id = 6036
[taxonomies]
categories = []
tags = []
+++

{{task|Arbitrary precision}}

Using the in-built capabilities of your language, calculate the integer value of:

         <big><big><math>5^{4^{3^2}}</math></big></big>

*  Confirm that the first and last twenty digits of the answer are:
      62060698786608744707...92256259918212890625
*  Find and show the number of decimal digits in the answer.



<small>Note: <ul><li>Do not submit an ''implementation'' of [[wp:arbitrary precision arithmetic|arbitrary precision arithmetic]]. The intention is to show the capabilities of the language as supplied. If a language has a [[Talk:Arbitrary-precision integers (included)#Use of external libraries|single, overwhelming, library]] of varied modules that is endorsed by its home site – such as [[CPAN]] for Perl or [[Boost]] for C++ – then that ''may'' be used instead.
</li><li>Strictly speaking, this should not be solved by fixed-precision numeric libraries where the precision has to be manually set to a large value; although if this is the '''only''' recourse then it may be used with a note explaining that the precision must be set manually to a large enough value.</li></ul></small>


;Related tasks:
*   [[Long multiplication]]
*   [[Exponentiation order]]





## 8th


```forth

200000 n#
5 4 3 2 bfloat ^ ^ ^
"%.0f" s:strfmt
dup s:len . " digits" . cr
dup 20 s:lsub . "..." .  20 s:rsub . cr

```

{{out}}

```txt

183231 digits
62060698786608744707...92256259918212890625

```



## ACL2


```Lisp
(in-package "ACL2")

(include-book "arithmetic-3/floor-mod/floor-mod" :dir :system)

(set-print-length 0 state)

(defun arbitrary-precision ()
   (declare (xargs :mode :program))
   (let* ((x (expt 5 (expt 4 (expt 3 2))))
          (s (mv-let (col str)
                     (fmt1-to-string "~xx"
                                     (list (cons #\x x))
                                     0)
                (declare (ignore col))
                str)))
         (cw "~s0 ... ~x1 (~x2 digits)~%"
             (subseq s 0 20)
             (mod x (expt 10 20))
             (1- (length s)))))
```


{{out}}

```txt
62060698786608744707 ... 92256259918212890625 (183231 digits)
```



## Ada

{{libheader|GMP}} Using GMP, Ada bindings provided in GNATColl

```Ada
with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.GMP; use GNATCOLL.GMP;
with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;
procedure ArbitraryInt is
   type stracc is access String;
   BigInt : Big_Integer;
   len : Natural;
   str : stracc;
begin
   Set (BigInt, 5);
   Raise_To_N (BigInt, Unsigned_Long (4**(3**2)));
   str := new String'(Image (BigInt));
   len := str'Length;
   Put_Line ("Size is:"& Natural'Image (len));
   Put_Line (str (1 .. 20) & "....." & str (len - 19 .. len));
end ArbitraryInt;
```

{{out}}

```txt
Size is: 183231
62060698786608744707.....92256259918212890625
```



## ALGOL 68


```algol68

BEGIN
COMMENT
   The task specifies

   "Strictly speaking, this should not be solved by fixed-precision
   numeric libraries where the precision has to be manually set to a
   large value; although if this is the only recourse then it may be
   used with a note explaining that the precision must be set manually
   to a large enough value."

   Now one should always speak strictly, especially to animals and
   small children and, strictly speaking, Algol 68 Genie requires that
   a non-default numeric precision for a LONG LONG INT be specified by
   "precision=<integral denotation>" either in a source code PRAGMAT
   or as a command line argument.  However, that specification need
   not be made manually.  This snippet of code outputs an appropriate
   PRAGMAT

   printf (($gg(0)xgl$, "PR precision=",
	    ENTIER (1.0 + log (5) * 4^(3^(2))), "PR"));

   and the technique shown in the "Call a foreign-language function"
   task used to write, compile and run an Algol 68 program in which
   the precision is programmatically determined.

   The default stack size on this machine is also inadequate but twice
   the default is sufficient.  The PRAGMAT below can be machine
   generated with

   printf (($gg(0)xgl$, "PR stack=", 2 * system stack size, "PR"));

COMMENT
   PR precision=183231 PR
   PR stack=16777216 PR
   INT digits = ENTIER (1.0 + log (5) * 4^(3^(2))), exponent = 4^(3^2);
   LONG LONG INT big = LONG LONG 5^exponent;
   printf (($gxg(0)l$, " First 20 digits:", big % LONG LONG 10 ^ (digits - 20)));
   printf (($gxg(0)l$, "  Last 20 digits:", big MOD LONG LONG 10 ^ 20));
   printf (($gxg(0)l$, "Number of digits:", digits))
END

```
 {{out}}

```txt

 First 20 digits: 62060698786608744707
  Last 20 digits: 92256259918212890625
Number of digits: 183231

```



## Alore


```Alore
def Main()
  var len as Int
  var result as Str
  result = Str(5**4**3**2)
  len = result.length()
  Print(len)
  Print(result[:20])
  Print(result[len-20:])
end
```



## bc


```bc
/* 5432.bc */

y = 5 ^ 4 ^ 3 ^ 2
c = length(y)
" First 20 digits: "; y / (10 ^ (c - 20))
"  Last 20 digits: "; y % (10 ^ 20)
"Number of digits: "; c
quit
```


Output:
```txt
$ time bc 5432.bc
 First 20 digits: 62060698786608744707
  Last 20 digits: 92256259918212890625
Number of digits: 183231
    0m24.81s real     0m24.81s user     0m0.00s system
```


{{omit from|BBC BASIC}}


## Bracmat

At the prompt type the following one-liner:

```bracmat
{?} @(5^4^3^2:?first [20 ? [-21 ?last [?length)&str$(!first "..." !last "\nlength " !length)
{!} 62060698786608744707...92256259918212890625
length 183231
    S   2,46 sec
```



## C

=== {{libheader|GMP}} ===

```c
#include <gmp.h>
#include <stdio.h>
#include <string.h>

int main()
{
	mpz_t a;
	mpz_init_set_ui(a, 5);
	mpz_pow_ui(a, a, 1 << 18); /* 2**18 == 4**9 */

	int len = mpz_sizeinbase(a, 10);
	printf("GMP says size is: %d\n", len);

	/* because GMP may report size 1 too big; see doc */
	char *s = mpz_get_str(0, 10, a);
	printf("size really is %d\n", len = strlen(s));
	printf("Digits: %.20s...%s\n", s, s + len - 20);

	// free(s); /* we could, but we won't. we are exiting anyway */
	return 0;
}
```

{{out}}

```txt
GMP says size is: 183231
size really is 183231
Digits: 62060698786608744707...92256259918212890625
```

==={{libheader|OpenSSL}}===
OpenSSL is about 17 times slower than GMP (on one computer), but still fast enough for this small task.

```c
/* 5432.c */

#include <openssl/bn.h>		/* BN_*() */
#include <openssl/err.h>	/* ERR_*() */
#include <stdlib.h>		/* exit() */
#include <stdio.h>		/* fprintf() */
#include <string.h>		/* strlen() */

void
fail(const char *message)
{
	fprintf(stderr, "%s: error 0x%08lx\n", ERR_get_error());
	exit(1);
}

int
main()
{
	BIGNUM two, three, four, five;
	BIGNUM answer;
	BN_CTX *context;
	size_t length;
	char *string;

	context = BN_CTX_new();
	if (context == NULL)
		fail("BN_CTX_new");

	/* answer = 5 ** 4 ** 3 ** 2 */
	BN_init(&two);
	BN_init(&three);
	BN_init(&four);
	BN_init(&five);
	if (BN_set_word(&two, 2) == 0 ||
	    BN_set_word(&three, 3) == 0 ||
	    BN_set_word(&four, 4) == 0 ||
	    BN_set_word(&five, 5) == 0)
		fail("BN_set_word");
	BN_init(&answer);
	if (BN_exp(&answer, &three, &two, context) == 0 ||
	    BN_exp(&answer, &four, &answer, context) == 0 ||
	    BN_exp(&answer, &five, &answer, context) == 0)
		fail("BN_exp");

	/* string = decimal answer */
	string = BN_bn2dec(&answer);
	if (string == NULL)
		fail("BN_bn2dec");

	length = strlen(string);
	printf(" First 20 digits: %.20s\n", string);
	if (length >= 20)
		printf("  Last 20 digits: %.20s\n", string + length - 20);
	printf("Number of digits: %zd\n", length);

	OPENSSL_free(string);
	BN_free(&answer);
	BN_free(&five);
	BN_free(&four);
	BN_free(&three);
	BN_free(&two);
	BN_CTX_free(context);

	return 0;
}
```

{{out}}

```txt
$ make LDLIBS=-lcrypto 5432
cc -O2 -pipe    -o 5432 5432.c -lcrypto
$ time ./5432
 First 20 digits: 62060698786608744707
  Last 20 digits: 92256259918212890625
Number of digits: 183231
    0m1.30s real     0m1.30s user     0m0.00s system
```





### Pure C

{{incorrect|C|Task states that an implementation of big ints should not be done}}
This is done in pure C.  The multiplication code is slow (grade school, just done using strings),
but since the text representation was required, it is MUCH easier to do it this way, than to have
to implement division and modulus to convert a base 2^32 number into base-10.


```c
/* 5432_pure.c */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

/* return = a * b.  Caller is responsible for freeing memory.
 * Handling of negatives, and zeros is not here, since not needed.
 */
unsigned char *str_mult(const unsigned char *A, const unsigned char *B)
{
	int ax = 0, bx = 0, rx = 0, al, bl;
	unsigned char *a, *b, *r; /* result */

	al = strlen(A); bl = strlen(B);
	r = calloc(al + bl + 1, 1);
	/* convert A and B from ASCII string numbers, into numeric */
	a = malloc(al+1); strcpy(a, A); for (ax = 0; ax < al; ++ax) a[ax] -= '0';
	b = malloc(bl+1); strcpy(b, B); for (bx = 0; bx < bl; ++bx) b[bx] -= '0';

	/* grade-school method of multiplication */
	for (ax = al - 1; ax >= 0; ax--) {
		int carry = 0;
		for (bx = bl - 1, rx = ax + bx + 1; bx >= 0; bx--, rx--) {
			int n = a[ax] * b[bx] + r[rx] + carry;
			r[rx] = (n % 10);
			carry = n / 10;
		}
		r[rx] += carry;
	}
	/* convert result from numeric into ASCII string numeric */
	for (rx = 0; rx < al + bl; ++rx)
		r[rx] += '0';
	while (r[0] == '0')
	    memmove(r, &r[1], al + bl);
	free(b); free(a);
	return r;
}

unsigned char *str_exp(int b, int n) {
  unsigned char *r, *tmp, *a;

  r = malloc(2); strcpy(r, "1");
  a = malloc(24); sprintf(a, "%d", b);

  while (n!=1) {
    if (n%2==1) {
	  tmp = str_mult(r, a);
	  free(r);
	  r = tmp;
    }
    n >>= 1;
	tmp = str_mult(a, a);
	free(a);
	a = tmp;
  }
  free(r);
  return a;
}

/* compute 5^4^3^2   which == 5^262144 */
int main() {
	unsigned char *r = str_exp(5,262144);
	printf ("Length of 5^4^3^2 is %d\n", strlen(r));
	printf ("First 20 digits:  %20.20s\n", r);
	printf ("Last 20 digits:   %s\n", &r[strlen(r)-20]);
	free(r);
	printf ("This took %.2f seconds\n", ((double)clock())/CLOCKS_PER_SEC);
}
```


{{out}}

```txt
$ gcc -O2 5432_pure.c -o 5432_pure
$ /.5432_pure
Length of 5^4^3^2 is 183231
First 20 digits:  62060698786608744707
Last 20 digits:   92256259918212890625
This took 27.95 seconds

```



## C++

=== {{libheader|Boost-Multiprecision (GMP Backend)}} ===
To compile link with GMP <code>-lgmp</code>

```cpp
#include <iostream>
#include <boost/multiprecision/gmp.hpp>
#include <string>

namespace mp = boost::multiprecision;

int main(int argc, char const *argv[])
{
    // We could just use (1 << 18) instead of tmpres, but let's point out one
    // pecularity with gmp and hence boost::multiprecision: they won't accept
    // a second mpz_int with pow(). Therefore, if we stick to multiprecision
    // pow we need to convert_to<uint64_t>().
    uint64_t tmpres = mp::pow(mp::mpz_int(4)
                            , mp::pow(mp::mpz_int(3)
                                    , 2).convert_to<uint64_t>()
                                      ).convert_to<uint64_t>();
    mp::mpz_int res = mp::pow(mp::mpz_int(5), tmpres);
    std::string s = res.str();
    std::cout << s.substr(0, 20)
              << "..."
              << s.substr(s.length() - 20, 20) << std::endl;
    return 0;
}

```

{{out}}

```txt
62060698786608744707...92256259918212890625
```



## C sharp

<code>System.Numerics.BigInteger</code> was added in C# 4. The exponent of <code>BigInteger.Pow()</code> is limited to a 32-bit signed integer, which is not a problem in this specific task.
{{works with|C sharp|C#|4+}}

```csharp
using System;
using System.Diagnostics;
using System.Linq;
using System.Numerics;

static class Program {
    static void Main() {
        BigInteger n = BigInteger.Pow(5, (int)BigInteger.Pow(4, (int)BigInteger.Pow(3, 2)));
        string result = n.ToString();

        Debug.Assert(result.Length == 183231);
        Debug.Assert(result.StartsWith("62060698786608744707"));
        Debug.Assert(result.EndsWith("92256259918212890625"));

        Console.WriteLine("n = 5^4^3^2");
        Console.WriteLine("n = {0}...{1}",
            result.Substring(0, 20),
            result.Substring(result.Length - 20, 20)
            );

        Console.WriteLine("n digits = {0}", result.Length);
    }
}
```

{{out}}

```txt

n = 5^4^3^2
n = 62060698786608744707...92256259918212890625
n digits = 183231

```



## Ceylon

Be sure to import ceylon.whole in your module.ceylon file.

```ceylon
import ceylon.whole {
    wholeNumber,
    two
}

shared void run() {

    value five = wholeNumber(5);
    value four = wholeNumber(4);
    value three = wholeNumber(3);

    value bigNumber = five ^ four ^ three ^ two;

    value firstTwenty = "62060698786608744707";
    value lastTwenty =  "92256259918212890625";
    value bigString = bigNumber.string;

    "The number must start with ``firstTwenty`` and end with ``lastTwenty``"
    assert(bigString.startsWith(firstTwenty), bigString.endsWith(lastTwenty));

    value bigSize = bigString.size;
    print("The first twenty digits are ``bigString[...19]``");
    print("The last twenty digits are ``bigString[(bigSize - 20)...]``");
    print("The number of digits in 5^4^3^2 is ``bigSize``");
}
```

{{output}}

```txt
The first twenty digits are 62060698786608744707
The last twenty digits are 92256259918212890625
The number of digits in 5^4^3^2 is 183231
```



## Clojure


```clojure
(defn exp [n k] (reduce * (repeat k n)))

(def big (->> 2 (exp 3) (exp 4) (exp 5)))
(def sbig (str big))

(assert (= "62060698786608744707" (.substring sbig 0 20)))
(assert (= "92256259918212890625" (.substring sbig (- (count sbig) 20))))
(println (count sbig) "digits")

(println (str (.substring sbig 0 20) ".."
	      (.substring sbig (- (count sbig) 20)))
	 (str "(" (count sbig) " digits)"))
```

{{out}}

```txt
output> 62060698786608744707..92256259918212890625 (183231 digits)
```

Redefining ''exp'' as follows speeds up the calculation of ''big'' about a hundred times:

```clojure
(defn exp [n k]
  (cond
    (zero? (mod k 2)) (recur (* n n) (/ k 2))
    (zero? (mod k 3)) (recur (* n n n) (/ k 3))
    :else (reduce * (repeat k n))))
```



## COBOL

This entry might be pushing the limits of the spirit of the task.  COBOL does not have arbitrary-precision integers in the spec, but it does mandate a precision of some 1000 digits with intermediate results, from 10^-999 through 10^1000, for purposes of rounding financially sound decimal arithmetic.  GnuCOBOL uses libgmp or equivalent to meet and surpass this requirement, but this precision is not exposed to general programming in the language.  The capabilities are included in the GnuCOBOL implementation run-time support, but require access to some of the opaque features of libgmp for use in this task.

This listing includes a few calculations, 12345**9 is an example that demonstrates the difference between the library's view of certain string lengths and a native C view of the data.

{{works with|GnuCOBOL}}
{{libheader|GMP}}

```COBOL
       identification division.
       program-id. arbitrary-precision-integers.
       remarks. Uses opaque libgmp internals that are built into libcob.

       data division.
       working-storage section.
       01 gmp-number.
          05 mp-alloc          usage binary-long.
          05 mp-size           usage binary-long.
          05 mp-limb           usage pointer.
       01 gmp-build.
          05 mp-alloc          usage binary-long.
          05 mp-size           usage binary-long.
          05 mp-limb           usage pointer.

       01 the-int              usage binary-c-long unsigned.
       01 the-exponent         usage binary-c-long unsigned.
       01 valid-exponent       usage binary-long value 1.
          88 cant-use          value 0 when set to false 1.

       01 number-string        usage pointer.
       01 number-length        usage binary-long.

       01 window-width         constant as 20.
       01 limit-width          usage binary-long.
       01 number-buffer        pic x(window-width) based.

       procedure division.
       arbitrary-main.

      *> calculate 10 ** 19
       perform initialize-integers.
       display "10 ** 19        : " with no advancing
       move 10 to the-int
       move 19 to the-exponent
       perform raise-pow-accrete-exponent
       perform show-all-or-portion
       perform clean-up

      *> calculate 12345 ** 9
       perform initialize-integers.
       display "12345 ** 9      : " with no advancing
       move 12345 to the-int
       move 9 to the-exponent
       perform raise-pow-accrete-exponent
       perform show-all-or-portion
       perform clean-up

      *> calculate 5 ** 4 ** 3 ** 2
       perform initialize-integers.
       display "5 ** 4 ** 3 ** 2: " with no advancing
       move 3 to the-int
       move 2 to the-exponent
       perform raise-pow-accrete-exponent
       move 4 to the-int
       perform raise-pow-accrete-exponent
       move 5 to the-int
       perform raise-pow-accrete-exponent
       perform show-all-or-portion
       perform clean-up
       goback.
      *> **************************************************************

       initialize-integers.
       call "__gmpz_init" using gmp-number returning omitted
       call "__gmpz_init" using gmp-build returning omitted
       .

       raise-pow-accrete-exponent.
      *> check before using previously overflowed exponent intermediate
       if cant-use then
           display "Error: intermediate overflow occured at "
                   the-exponent upon syserr
           goback
       end-if
       call "__gmpz_set_ui" using gmp-number by value 0
           returning omitted
       call "__gmpz_set_ui" using gmp-build by value the-int
           returning omitted
       call "__gmpz_pow_ui" using gmp-number gmp-build
           by value the-exponent
           returning omitted
       call "__gmpz_set_ui" using gmp-build by value 0
           returning omitted
       call "__gmpz_get_ui" using gmp-number returning the-exponent
       call "__gmpz_fits_ulong_p" using gmp-number
           returning valid-exponent
       .

      *> get string representation, base 10
       show-all-or-portion.
       call "__gmpz_sizeinbase" using gmp-number
           by value 10
           returning number-length
       display "GMP length: " number-length ", " with no advancing

       call "__gmpz_get_str" using null by value 10
           by reference gmp-number
           returning number-string
       call "strlen" using by value number-string
           returning number-length
       display "strlen: " number-length

      *> slide based string across first and last of buffer
       move window-width to limit-width
       set address of number-buffer to number-string
       if number-length <= window-width then
           move number-length to limit-width
           display number-buffer(1:limit-width)
       else
           display number-buffer with no advancing
           subtract window-width from number-length
           move function max(0, number-length) to number-length
           if number-length <= window-width then
               move number-length to limit-width
           else
               display "..." with no advancing
           end-if
           set address of number-buffer up by
               function max(window-width, number-length)
           display number-buffer(1:limit-width)
       end-if
       .

       clean-up.
       call "free" using by value number-string returning omitted
       call "__gmpz_clear" using gmp-number returning omitted
       call "__gmpz_clear" using gmp-build returning omitted
       set address of number-buffer to null
       set cant-use to false
       .

       end program arbitrary-precision-integers.

```


{{out}}

```txt

prompt$ cobc -xj arbitrary-integer.cob
10 ** 19        : GMP length: +0000000020, strlen: +0000000020
10000000000000000000
12345 ** 9      : GMP length: +0000000038, strlen: +0000000037
6659166111488656281486807152009765625
5 ** 4 ** 3 ** 2: GMP length: +0000183231, strlen: +0000183231
62060698786608744707...92256259918212890625

```



## Common Lisp

Common Lisp has arbitrary precision integers, inherited from MacLisp: "[B]ignums—arbitrary precision integer arithmetic—were added [to MacLisp] in 1970 or 1971 to meet the needs of Macsyma users." [''Evolution of Lisp'' [http://dreamsongs.com/Files/Hopl2.pdf], 2.2.2]

```lisp
(let ((s (format () "~s" (expt 5 (expt 4 (expt 3 2))))))
  (format t "~a...~a, length ~a" (subseq s 0 20)
          (subseq s (- (length s) 20)) (length s)))
```

{{out}}

```txt

62060698786608744707...92256259918212890625, length 183231

```



## D


```d
void main() {
  import std.stdio, std.bigint, std.conv;

  auto s = text(5.BigInt ^^ 4 ^^ 3 ^^ 2);
  writefln("5^4^3^2 = %s..%s (%d digits)", s[0..20], s[$-20..$], s.length);
}
```

{{out}}

```txt
5^4^3^2 = 62060698786608744707..92256259918212890625 (183231 digits)
```

With dmd about 0.55 seconds compilation time (-release -noboundscheck) and about 3.3 seconds run time.


## Dart

Dart's only integral type '''int''' supports arbitrary length integers.

```dart
void main() {
  var s = pow(5, pow(4, pow(3, 2))).toString();

  print('contains given digits: ${s.startsWith('62060698786608744707') && s.endsWith('92256259918212890625')}');
  print('number of digits: ${s.length}');
}
```

{{out}}

```txt
contains given digits: true
number of digits: 183231
```



## dc

{{trans|bc}}

```dc
[5432.dc]sz

5 4 3 2 ^ ^ ^ sy				[y = 5 ^ 4 ^ 3 ^ 2]sz
ly Z sc						[c = length of y]sz
[ First 20 digits: ]P ly 10 lc 20 - ^ / p sz	[y / (10 ^ (c - 20))]sz
[  Last 20 digits: ]P ly 10 20 ^ % p sz		[y % (10 ^ 20)]sz
[Number of digits: ]P lc p sz
```

{{out}}

```txt
$ time dc 5432.dc
 First 20 digits: 62060698786608744707
  Last 20 digits: 92256259918212890625
Number of digits: 183231
    0m24.80s real     0m24.81s user     0m0.00s system
```



## E

E implementations are required to support arbitrary-size integers transparently.

```e
? def value := 5**(4**(3**2)); null
? def decimal := value.toString(10); null
? decimal(0, 20)
# value: "62060698786608744707"

? decimal(decimal.size() - 20)
# value: "92256259918212890625"

? decimal.size()
# value: 183231
```



## EchoLisp


```scheme

;; to save space and time, we do'nt stringify Ω = 5^4^3^2 ,
;; but directly extract tail and head and number of decimal digits

(lib 'bigint) ;; arbitrary size integers

(define e10000 (expt 10 10000)) ;; 10^10000

(define (last-n big (n 20))
(string-append "..." (number->string (modulo big (expt 10 n)))))

(define (first-n big (n 20))
	(while (> big e10000)
		(set! big (/ big e10000))) ;; cut 10000 digits at a time
	(string-append (take (number->string big) n) "..."))

;; faster than directly using (number-length big)
(define (digits big (digits 0))
	(while (> big e10000)
		(set! big (/ big e10000))
		(set! digits (1+ digits)))
	(+ (* digits 10000) (number-length big)))

(define Ω (expt 5 (expt 4 (expt 3 2))))

(last-n Ω )
    → "...92256259918212890625"
(first-n Ω )
    → "62060698786608744707..."
(digits Ω )
    → 183231



```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Arbitrary do
  def pow(_,0), do: 1
  def pow(b,e) when e > 0, do: pow(b,e,1)

  defp pow(b,1,acc), do: acc * b
  defp pow(b,p,acc) when rem(p,2)==0, do: pow(b*b,div(p,2),acc)
  defp pow(b,p,acc), do: pow(b,p-1,acc*b)

  def test do
    s = pow(5,pow(4,pow(3,2))) |> to_string
    l = String.length(s)
    prefix = String.slice(s,0,20)
    suffix = String.slice(s,-20,20)
    IO.puts "Length: #{l}\nPrefix:#{prefix}\nSuffix:#{suffix}"
  end
end
Arbitrary.test
```


{{out}}

```txt

Length: 183231
Prefix:62060698786608744707
Suffix:92256259918212890625

```



## Emacs Lisp

Emacs Lisp has no big integers (bignums) in the language; but GNU Emacs 22 includes Calc, a library that implements big integers.  The <code>calc-eval</code> function takes an algebraic formula in a string, and returns the result in a string.

{{libheader|Calc}}

```lisp
(let* ((answer (calc-eval "5**4**3**2"))
       (length (length answer)))
  (message "%s has %d digits"
	   (if (> length 40)
	       (format "%s...%s"
		       (substring answer 0 20)
		       (substring answer (- length 20) length))
	     answer)
	   length))
```


This implementation is ''very slow''; one computer, running GNU Emacs 23.4.1, needed about seven minutes to find the answer.

{{out}}
 62060698786608744707...92256259918212890625 has 183231 digits


## Erlang


Erlang supports arbitrary precision integers. However, the math:pow function returns a float. This implementation includes an implementation of pow for integers with exponent greater than 0.

```erlang

-module(arbitrary).
-compile([export_all]).

pow(B,E) when E > 0 ->
    pow(B,E,1).

pow(_,0,_) -> 0;
pow(B,1,Acc) -> Acc * B;
pow(B,P,Acc) when P rem 2 == 0 ->
    pow(B*B,P div 2, Acc);
pow(B,P,Acc) ->
    pow(B,P-1,Acc*B).

test() ->
    I = pow(5,pow(4,pow(3,2))),
    S = integer_to_list(I),
    L = length(S),
    Prefix = lists:sublist(S,20),
    Suffix = lists:sublist(S,L-19,20),
    io:format("Length: ~b~nPrefix:~s~nSuffix:~s~n",[L,Prefix,Suffix]).

```

{{out}}
23> arbitrary:test().
 Length: 183231
 Prefix:62060698786608744707
 Suffix:92256259918212890625
 ok

=={{header|F Sharp|F#}}==
You can specifiy arbitrary-precision integers (bigint or System.Numeric.BigInteger) in F# by postfixing the number with the letter 'I'. While '**' is the power function, two things should be noted:
* bigint does not support raising to a power of a bigint
* The int type does not support the power method

```fsharp
let () =
    let answer = 5I **(int (4I ** (int (3I ** 2))))
    let sans = answer.ToString()
    printfn "Length = %d, digits %s ... %s" sans.Length (sans.Substring(0,20)) (sans.Substring(sans.Length-20))
;;
Length = 183231, digits 62060698786608744707 ... 92256259918212890625
```



## Factor

Factor has built-in bignum support. Operations on integers overflow to bignums.

```factor
USING: formatting kernel math.functions math.parser sequences ;
IN: rosettacode.bignums

: test-bignums ( -- )
    5 4 3 2 ^ ^ ^ number>string
    [ 20 head ] [ 20 tail* ] [ length ] tri
    "5^4^3^2 is %s...%s and has %d digits\n" printf ;
```

It prints: <code>5^4^3^2 is 62060698786608744707...92256259918212890625 and has 183231 digits</code>

=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Arbitrary-precision_integers_(included) this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran

Modern Fortran has no in-built facility for arbitrarily-sized numbers, but libraries are available.


### FM library

Here is a solution using David M. Smith's FM library, available [http://myweb.lmu.edu/dmsmith/fmlib.html here].


```fortran
program bignum
    use fmzm
    implicit none
    type(im) :: a
    integer :: n

    call fm_set(50)
    a = to_im(5)**(to_im(4)**(to_im(3)**to_im(2)))
    n = to_int(floor(log10(to_fm(a))))
    call im_print(a / to_im(10)**(n - 19))
    call im_print(mod(a, to_im(10)**20))
end program
```



```txt

62060698786608744707
92256259918212890625

```



## FreeBASIC

freebasic has it's own gmp static library.
Here, a power function operates via a string and uinteger.

```FreeBASIC
#Include once "gmp.bi"
Dim Shared As Zstring * 100000000 outtext

Function  Power(number As String,n As Uinteger) As String'automate precision
    #define dp 3321921
    Dim As __mpf_struct _number,FloatAnswer
    Dim As Ulongint ln=Len(number)*(n)*4
    If ln>dp Then ln=dp
    mpf_init2(@FloatAnswer,ln)
    mpf_init2(@_number,ln)
    mpf_set_str(@_number,number,10)
    mpf_pow_ui(@Floatanswer,@_number,n)
    gmp_sprintf( @outtext,"%." & Str(n) & "Ff",@FloatAnswer )
    Var outtxt=Trim(outtext)
    If Instr(outtxt,".") Then outtxt= Rtrim(outtxt,"0"):outtxt=Rtrim(outtxt,".")
    Return Trim(outtxt)
End Function

Extern gmp_version Alias "__gmp_version" As Zstring Ptr
Print "GMP version ";*gmp_version
Print

var ans=power("5",(4^(3^2)))
Print Left(ans,20) + " ... "+Right(ans,20)
Print "Number of digits ";Len(ans)
Sleep
```

{{out}}

```txt
GMP version 5.1.1

62060698786608744707 ... 92256259918212890625
Number of digits  183231
```



## Frink

Frink has built-in arbitrary-precision integers and all operations automatically promote to arbitrary precision when needed.

Fun Fact:  The drastically faster arbitrary-precision integer operations that landed in Java 8 (for much faster multiplication, exponentiation, and toString) were taken from Frink's implementation and contributed to Java.  Another fun fact is that it took employees from Java 11 years to integrate the improvements.

```frink
a = 5^4^3^2
as = "$a"     // Coerce to string
println["Length=" + length[as] + ", " + left[as,20] + "..." + right[as,20]]
```

This prints <CODE>Length=183231, 62060698786608744707...92256259918212890625</CODE>


## GAP


```gap
n:=5^(4^(3^2));;
s := String(n);;
m := Length(s);
# 183231
s{[1..20]};
# "62060698786608744707"
s{[m-19..m]};
# "92256259918212890625"
```



## Go

Using <code>math/big</code>'s
<code>[https://golang.org/pkg/math/big/#Int.Exp Int.Exp]</code>.

```go
package main

import (
	"fmt"
	"math/big"
)

func main() {
	x := big.NewInt(2)
	x = x.Exp(big.NewInt(3), x, nil)
	x = x.Exp(big.NewInt(4), x, nil)
	x = x.Exp(big.NewInt(5), x, nil)
	str := x.String()
	fmt.Printf("5^(4^(3^2)) has %d digits: %s ... %s\n",
		len(str),
		str[:20],
		str[len(str)-20:],
	)
}
```

{{out}}

```txt

5^(4^(3^2)) has 183231 digits: 62060698786608744707 ... 92256259918212890625

```



## Golfscript


```golfscript
5 4 3 2???  # Calculate 5^(4^(3^2))
`..         # Convert to string and make two copies
20<p        # Print the first 20 digits
-20>p       # Print the last 20 digits
,p          # Print the length
```

The ''p'' command prints the top element from the stack, so the output of this program is just three lines:

```txt
"62060698786608744707"
"92256259918212890625"
183231
```



## Groovy

Solution:

```groovy
def bigNumber = 5G ** (4 ** (3 ** 2))
```

Test:

```groovy
def bigString = bigNumber.toString()

assert bigString[0..<20] == "62060698786608744707"
assert bigString[-20..-1] == "92256259918212890625"

println bigString.size()
```

{{out}}

```txt
183231
```



## Haskell

Haskell comes with built-in support for arbitrary precision integers. The type of arbitrary precision integers is <tt>Integer</tt>.

```haskell
main = do
    let y = show ( 5^4^3^2 )
    let l = length y
    putStrLn ("5**4**3**2 = " ++ take 20 y ++ "..." ++ drop (l-20) y ++ " and has " ++ show l ++ " digits")
```

{{out}}

```txt
5**4**3**2 = 62060698786608744707...92256259918212890625 and has 183231 digits
```



## Hoon


```Hoon

=+  big=(pow 5 (pow 4 (pow 3 2)))
=+  digits=(lent (skip <big> |=(a/* ?:(=(a '.') & |))))
[digits (div big (pow 10 (sub digits 20))) (mod big (pow 10 20))]

```

{{out}}
```txt
[183.231 62.060.698.786.608.744.707 92.256.259.918.212.890.625]
```


As of 23 July 2016, the standard library lacks a base-10 logarithm, so the length is computed by pretty-printing the number and counting the length of the resulting string without grouping dots.

=={{header|Icon}} and {{header|Unicon}}==
Both Icon and Unicon have built-in support for bignums.

Note: It takes far longer to convert the result to a string than it does to do the computation itself.

```icon
procedure main()
    x := 5^4^3^2
    write("done with computation")
    x := string(x)
    write("5 ^ 4 ^ 3 ^ 2 has ",*x," digits")
    write("The first twenty digits are ",x[1+:20])
    write("The last twenty digits are  ",x[0-:20])
end
```

{{out|Sample run}}

```txt
->ap
done with computation
5 ^ 4 ^ 3 ^ 2 has 183231 digits
The first twenty digits are 62060698786608744707
The last twenty digits are  92256259918212890625
->
```



## J

J has built-in support for extended precision integers. See also [[J:Essays/Extended%20Precision%20Functions]].

```j
   Pow5432=: 5^4^3^2x
   Pow5432=: ^/ 5 4 3 2x                    NB. alternate J solution
   # ": Pow5432                             NB. number of digits
183231
   20 ({. , '...' , -@[ {. ]) ": Pow5432    NB. 20 first & 20 last digits
62060698786608744707...92256259918212890625
```



## Java

Java library's <tt>BigInteger</tt> class provides support for arbitrary precision integers.

```java
import java.math.BigInteger;

class IntegerPower {
    public static void main(String[] args) {
        BigInteger power = BigInteger.valueOf(5).pow(BigInteger.valueOf(4).pow(BigInteger.valueOf(3).pow(2).intValueExact()).intValueExact());
        String str = power.toString();
        int len = str.length();
        System.out.printf("5**4**3**2 = %s...%s and has %d digits%n",
                str.substring(0, 20), str.substring(len - 20), len);
    }
}
```

{{out}}

```txt
5**4**3**2 = 62060698786608744707...92256259918212890625 and has 183231 digits
```



## Klong

    n::$5^4^3^2
    .p("5^4^3^2 = ",(20#n),"...",((-20)#n)," and has ",($#n)," digits")
{{out}}
5^4^3^2 = 62060698786608744707...92256259918212890625 and has 183231 digits


## Kotlin

{{trans|Java}}

```scala
import java.math.BigInteger

fun main(args: Array<String>) {
    val x = BigInteger.valueOf(5).pow(Math.pow(4.0, 3.0 * 3.0).toInt())
    val y = x.toString()
    val len = y.length
    println("5^4^3^2 = ${y.substring(0, 20)}...${y.substring(len - 20)} and has $len digits")
}
```


{{out}}

```txt

5^4^3^2 = 62060698786608744707...92256259918212890625 and has 183231 digits

```



## Liberty BASIC

Interestingly this takes a LONG time in LB.

It takes however only seconds in RunBASIC, which is written by the same author, shares most of LB's syntax, and is based on later Smalltalk implementation.

Note the brackets are needed to enforce the desired order of exponentiating.

```lb
a$ = str$( 5^(4^(3^2)))
print len( a$)
print left$( a$, 20); "......"; right$( a$, 20)
```

{{out}}
 183231
 62060698786608744707......92256259918212890625


## Julia

Julia includes built-in support for arbitrary-precision arithmetic using the [http://gmplib.org/ GMP] (integer) and [http://www.mpfr.org/ GNU MPFR] (floating-point) libraries, wrapped by the built-in <code>BigInt</code> and <code>BigFloat</code> types, respectively.


```julia>julia
 @elapsed bigstr = string(BigInt(5)^4^3^2)
0.017507363

julia> length(bigstr)
183231

julia> bigstr[1:20]
"62060698786608744707"

julia> bigstr[end-20:end]
"892256259918212890625"
```



## Lasso

Interestingly, we have to define our own method for integer powers.

```lasso
define integer->pow(factor::integer) => {
    #factor <= 0
        ? return 0

    local(retVal) = 1

    loop(#factor) => { #retVal *= self }

    return #retVal
}

local(bigint) = string(5->pow(4->pow(3->pow(2))))
#bigint->sub(1,20) + ` ... ` + #bigint->sub(#bigint->size - 19)
"\n"
`Number of digits: ` + #bigint->size
```


{{out}}

```txt
62060698786608744707 ... 92256259918212890625
Number of digits: 183231
```




## Maple

Maple supports large integer arithmetic natively.

```Maple

> n := 5^(4^(3^2)):
> length( n ); # number of digits
                                 183231

> s := convert( n, 'string' ):
> s[ 1 .. 20 ], s[ -20 .. -1 ]; # extract first and last twenty digits
             "62060698786608744707", "92256259918212890625"

```

In the Maple graphical user interface it is also possible to set things up so that only (say) the first and last 20 digits of a large integer are displayed explicitly.  This is done as follows.

```Maple

> interface( elisiondigitsbefore = 20, elisiondigitsafter = 20 ):
> 5^(4^(3^2)):
             62060698786608744707[...183191 digits...]92256259918212890625

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica can handle arbitrary precision integers on almost any size without further declarations.
To view only the first and last twenty digits:

```Mathematica
s:=ToString[5^4^3^2];
Print[StringTake[s,20]<>"..."<>StringTake[s,-20]<>" ("<>ToString@StringLength@s<>" digits)"];
```

{{out}}
 62060698786608744707...92256259918212890625 (183231 digits)


## MATLAB

Using the [http://www.mathworks.com/matlabcentral/fileexchange/22725-variable-precision-integer-arithmetic Variable Precision Integer] library this task is accomplished thusly:

```MATLAB>>
 answer = vpi(5)^(vpi(4)^(vpi(3)^vpi(2)));
>> numDigits = order(answer) + 1

numDigits =

      183231

>> [sprintf('%d',leadingdigit(answer,20)) '...' sprintf('%d',trailingdigit(answer,20))]
%First and Last 20 Digits

ans =

62060698786608744707...92256259918212890625
```



## Maxima


```maxima
block([s, n], s: string(5^4^3^2), n: slength(s), print(substring(s, 1, 21), "...", substring(s, n - 19)), n);
/* 62060698786608744707...92256259918212890625
183231 */
```



## Nemerle

{{trans|C#}}

```Nemerle
using System.Console;
using System.Numerics;
using System.Numerics.BigInteger;

module BigInt
{
    Main() : void
    {
        def n = Pow(5, Pow(4, Pow(3, 2) :> int) :> int).ToString();
        def len = n.Length;
        def first20 = n.Substring(0, 20);
        def last20 = n.Substring(len - 20, 20);

        assert (first20 == "62060698786608744707", "High order digits are incorrect");
        assert (last20 == "92256259918212890625", "Low order digits are incorrect");
        assert (len == 183231, "Result contains wrong number of digits");

        WriteLine("Result: {0} ... {1}", first20, last20);
        WriteLine($"Length of result: $len digits");
    }
}
```

Output:

```txt
Result: 62060698786608744707 ... 92256259918212890625
Length of result: 183231 digits
```



## NetRexx

=== Using Java's BigInteger Class ===

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols

import java.math.BigInteger

numeric digits 30 -- needed to report the run-time

nanoFactor = 10 ** 9

t1 = System.nanoTime
x = BigInteger.valueOf(5)
x = x.pow(BigInteger.valueOf(4).pow(BigInteger.valueOf(3).pow(2).intValue()).intValue())
n = Rexx(x.toString)
t2 = System.nanoTime
td = t2 - t1
say "Run time in seconds:" td / nanoFactor
say

check = "62060698786608744707...92256259918212890625"
sample = n.left(20)"..."n.right(20)

say "Expected result:" check
say "  Actual result:" sample
say "         digits:" n.length
say

if check = sample
then
  say "Result confirmed"
else
  say "Result does not satisfy test"

return
```

{{out}}

```txt

Run time in seconds: 6.696671

Expected result: 62060698786608744707...92256259918212890625
  Actual result: 62060698786608744707...92256259918212890625
         digits: 183231

Result confirmed

```

=== Using Java's BigDecimal Class ===

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols

import java.math.BigDecimal

numeric digits 30 -- needed to report the run-time

nanoFactor = 10 ** 9

t1 = System.nanoTime
x = BigDecimal.valueOf(5)
x = x.pow(BigDecimal.valueOf(4).pow(BigDecimal.valueOf(3).pow(2).intValue()).intValue())
n = Rexx(x.toString)
t2 = System.nanoTime
td = t2 - t1
say "Run time in seconds:" td / nanoFactor
say

check = "62060698786608744707...92256259918212890625"
sample = n.left(20)"..."n.right(20)

say "Expected result:" check
say "  Actual result:" sample
say "         digits:" n.length
say

if check = sample
then
  say "Result confirmed"
else
  say "Result does not satisfy test"

return
```

{{out}}

```txt

Run time in seconds: 7.103424

Expected result: 62060698786608744707...92256259918212890625
  Actual result: 62060698786608744707...92256259918212890625
         digits: 183231

Result confirmed

```

=== Using NetRexx Built-In Math ===
Like [[REXX|Rexx]], NetRexx comes with built-in support for numbers that can be manually set to very large values of precision.
Compared to the two methods shown above however, the performance is extremely poor.

### = Note =

{{trans|REXX}}

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols

/* precision must be set manually */

numeric digits 190000

nanoFactor = 10 ** 9

t1 = System.nanoTime
n = 5 ** (4  ** (3 ** 2))
t2 = System.nanoTime
td = t2 - t1
say "Run time in seconds:" td / nanoFactor
say

check = "62060698786608744707...92256259918212890625"
sample = n.left(20)"..."n.right(20)

say "Expected result:" check
say "  Actual result:" sample
say "         digits:" n.length
say

if check = sample
then
  say "Result confirmed"
else
  say "Result does not satisfy test"
```

{{out}}

```txt

Run time in seconds: 719.660995

Expected result: 62060698786608744707...92256259918212890625
  Actual result: 62060698786608744707...92256259918212890625
         digits: 183231

Result confirmed

```



## Nim


{{libheader|bigints}}

```nim
import bigints

var x = 5.pow 4.pow 3.pow 2
var s = $x

echo s[0..19]
echo s[s.high - 19 .. s.high]
echo s.len
```

Output:

```txt

62060698786608744707
92256259918212890625
183231
```


## OCaml


```ocaml
open Num
open Str
open String

let () =
  let answer = (Int 5) **/ (Int 4) **/ (Int 3) **/ (Int 2) in
  let answer_string = string_of_num answer in
  Printf.printf "has %d digits: %s ... %s\n"
                (length answer_string)
                (first_chars answer_string 20)
                (last_chars answer_string 20)
```


A more readable program can be obtained using [http://forge.ocamlcore.org/projects/pa-do/ Delimited Overloading]:

```ocaml
let () =
  let answer = Num.(5**4**3**2) in
  let s = Num.(to_string answer) in
  Printf.printf "has %d digits: %s ... %s\n"
    (String.length s) (Str.first_chars s 20) (Str.last_chars s 20)
```

{{out}}

```txt

has 183231 digits: 62060698786608744707 ... 92256259918212890625

```



## Oforth


Oforth handles arbitrary precision integers :


```Oforth
import: mapping

5 4 3 2 pow pow pow >string dup left( 20 ) . dup right( 20 ) . size .
```


{{out}}

```txt

62060698786608744707 92256259918212890625 183231

```



## Ol


```scheme

(define x (expt 5 (expt 4 (expt 3 2))))
(print
   (div x (expt 10 (- (log 10 x) 20)))
   "..."
   (mod x (expt 10 20)))
(print "totally digits: " (log 10 x))

```

{{out}}

```txt

62060698786608744707...92256259918212890625
totally digits: 183231

```



## ooRexx

{{trans|REXX}}


```ooRexx

--REXX program to show arbitrary precision integers.
numeric digits 200000
check = '62060698786608744707...92256259918212890625'

start = .datetime~new
n = 5 ** (4 ** (3**2))
time = .datetime~new - start
say 'elapsed time for the calculation:' time
say
sampl = left(n, 20)"..."right(n, 20)

say ' check:' check
say 'Sample:' sampl
say 'digits:' length(n)
say

if check=sampl then say 'passed!'
               else say 'failed!'

```


{{out}}

```txt

prompt$ rexx rexx-arbitrary.rexx
elapsed time for the calculation: 00:00:45.373140

 check: 62060698786608744707...92256259918212890625
Sample: 62060698786608744707...92256259918212890625
digits: 183231

passed!
```



## Oz


```oz
declare
  Pow5432 = {Pow 5 {Pow 4 {Pow 3 2}}}
  S = {Int.toString Pow5432}
  Len = {Length S}
in
  {System.showInfo
   {List.take S 20}#"..."#
   {List.drop S Len-20}#" ("#Len#" Digits)"}
```

{{out}}

```txt

62060698786608744707...92256259918212890625 (183231 Digits)

```



## PARI/GP

An alternate, but slightly slower, method for counting decimal digits is <code>#Str(n)</code>. Note that <code>sizedigit</code> is not exact&mdash;in particular, it may be off by one (thus the function below).

```parigp
digits(x)={
	my(s=sizedigit(x)-1);
	if(x<10^s,s,s+1)
};

N=5^(4^(3^2));
[precision(N*1.,20), Mod(N,10^20), digits(N)]
```

{{out}}

```txt
[6.20606987866087447074832055728 E183230, Mod(92256259918212890625, 100000000000000000000), 183231]
```



## Pascal

{{works with|Free_Pascal}}
{{libheader|math}}
{{libheader|GMP}}
FreePascal comes with a header unit for gmp. Starting from the C program,  this is a Pascal version:

```pascal
program GMP_Demo;

uses
  math, gmp;

var
  a:   mpz_t;
  out: pchar;
  len: longint;
  i:   longint;

begin
  mpz_init_set_ui(a, 5);
  mpz_pow_ui(a, a, 4 ** (3 ** 2));
  len := mpz_sizeinbase(a, 10);
  writeln('GMP says size is: ', len);
  out := mpz_get_str(NIL, 10, a);
  writeln('Actual size is:   ', length(out));
  write('Digits: ');
  for i := 0 to 19 do
    write(out[i]);
  write ('...');
  for i := len - 20 to len do
    write(out[i]);
  writeln;
end.
```

{{out}}

```txt

GMP says size is: 183231
Actual size is:   183231
Digits: 62060698786608744707...92256259918212890625

```



## Perl

Perl's <tt>Math::BigInt</tt> core module handles big integers:

```perl
use Math::BigInt;
my $x = Math::BigInt->new('5') ** Math::BigInt->new('4') ** Math::BigInt->new('3') ** Math::BigInt->new('2');
my $y = "$x";
printf("5**4**3**2 = %s...%s and has %i digits\n", substr($y,0,20), substr($y,-20), length($y));
```

You can enable "transparent" big integer support by enabling the <tt>bigint</tt> pragma:

```perl
use bigint;
my $x = 5**4**3**2;
my $y = "$x";
printf("5**4**3**2 = %s...%s and has %i digits\n", substr($y,0,20), substr($y,-20), length($y));
```


<tt>Math::BigInt</tt> is very slow. Perl 5.10 was about 120 times slower than Ruby 1.9.2 (on one computer); Perl used more than one minute, but Ruby used less than one second.
{{out}}

```perl
$ time perl transparent-bigint.pl
5**4**3**2 = 62060698786608744707...92256259918212890625 and has 183231 digits
    1m4.28s real     1m4.30s user     0m0.00s system
```



## Perl 6

{{works with|Rakudo|2015.12}}


```perl6
given ~[**] 5, 4, 3, 2 {
   say "5**4**3**2 = {.substr: 0,20}...{.substr: *-20} and has {.chars} digits";
}
```

{{out}}

```txt
5**4**3**2 = 62060698786608744707...92256259918212890625 and has 183231 digits
```



## Phix

{{libheader|mpfr}}

```Phix
include mpfr.e
atom t0 = time()
mpz res = mpz_init()
mpz_ui_pow_ui(res,5,power(4,power(3,2)))
string s = mpz_get_str(res)
integer l = length(s)
if l>40 then s[21..-21] = "..." end if
printf(1,"5^4^3^2 = %s and has %d digits (%s)\n",{s,l,elapsed(time()-t0)})
```

{{out}}

```txt

5^4^3^2 = 62060698786608744707...92256259918212890625 and has 183231 digits (0.1s)

```



## PHP

PHP has two separate arbitrary-precision integer services.

The first is the BC library.[http://us3.php.net/manual/en/book.bc.php] It represents the integers as strings, so may not be very efficient. The advantage is that it is more likely to be included with PHP.

```php
<?php
$y = bcpow('5', bcpow('4', bcpow('3', '2')));
printf("5**4**3**2 = %s...%s and has %d digits\n", substr($y,0,20), substr($y,-20), strlen($y));
?>
```

{{out}}

```txt

5**4**3**2 = 62060698786608744707...92256259918212890625 and has 183231 digits

```

The second is the GMP library.[http://us3.php.net/manual/en/book.gmp.php] It represents the integers as an opaque type, so may be faster. However, it is less likely to be compiled into your version of PHP (it isn't compiled into mine).


## PicoLisp


```PicoLisp
(let L (chop (** 5 (** 4 (** 3 2))))
   (prinl (head 20 L) "..." (tail 20 L))
   (length L) )
```

{{out}}

```txt
62060698786608744707...92256259918212890625
-> 183231
```



## Pike


```Pike>
 string res = (string)pow(5,pow(4,pow(3,2)));
> res[..19] == "62060698786608744707";
Result: 1
> res[<19..] == "92256259918212890625";
Result: 1
> sizeof(result);
Result: 183231
```



## PowerShell


```PowerShell
#  Perform calculation
$BigNumber = [BigInt]::Pow( 5, [BigInt]::Pow( 4, [BigInt]::Pow( 3, 2 ) ) )

#  Display first and last 20 digits
$BigNumberString = [string]$BigNumber
$BigNumberString.Substring( 0, 20 ) + "..." + $BigNumberString.Substring( $BigNumberString.Length - 20, 20 )

#  Display number of digits
$BigNumberString.Length
```

{{out}}

```txt
62060698786608744707...92256259918212890625
183231
```



## Prolog


{{works with|SWI-Prolog|6.6}}


```prolog

task(Length) :-
    N is 5^4^3^2,

    number_codes(N, Codes),
    append(`62060698786608744707`, _,  Codes),
    append(_, `92256259918212890625`, Codes),

    length(Codes, Length).

```


Query like so:


```prolog

?- task(N).
N = 183231 ;
false.

```



## PureBasic

PureBasic has in its current version (today 4.50) no internal support for large numbers, but there are several free libraries for this.

Using [http://www.purebasic.fr/english/viewtopic.php?p=309763#p309763 Decimal.pbi], e.g. the same included library as in [[Long multiplication#PureBasic]], this task is solved as below.

```PureBasic
IncludeFile "Decimal.pbi"

;- Declare the variables that will be used
Define.Decimal *a
Define n, L$, R$, out$, digits.s

;- 4^3^2 is withing 32 bit range, so normal procedures can be used
n=Pow(4,Pow(3,2))

;- 5^n is larger then 31^2, so the same library call as in the "Long multiplication" task is used
*a=PowerDecimal(IntegerToDecimal(5),IntegerToDecimal(n))

;- Convert the large number into a string & present the results
out$=DecimalToString(*a)
L$ = Left(out$,20)
R$ = Right(out$,20)
digits=Str(Len(out$))
out$="First 20 & last 20 chars of 5^4^3^2 are;"+#CRLF$+L$+#CRLF$+R$+#CRLF$
out$+"and the result is "+digits+" digits long."

MessageRequester("Arbitrary-precision integers, PureBasic",out$)
```

[[Image:Arbitrary-precision_integers,_PureBasic.png]]


## Python

Python comes with built-in support for arbitrary precision integers. The type of arbitrary precision integers is <tt>[http://docs.python.org/library/stdtypes.html#typesnumeric long]</tt> in Python 2.x (overflowing operations on <tt>int</tt>'s are automatically converted into <tt>long</tt>'s), and <tt>[http://docs.python.org/3.1/library/stdtypes.html#typesnumeric int]</tt> in Python 3.x.

```python>>>
 y = str( 5**4**3**2 )
>>> print ("5**4**3**2 = %s...%s and has %i digits" % (y[:20], y[-20:], len(y)))
5**4**3**2 = 62060698786608744707...92256259918212890625 and has 183231 digits
```



## R

R does not come with built-in support for arbitrary precision integers, but it can be implemented with the GMP library (there is also an interface to bc).

```R
library(gmp)
large=pow.bigz(5,pow.bigz(4,pow.bigz(3,2)))
largestr=as.character(large)
cat("first 20 digits:",substr(largestr,1,20),"\n",
    "last 20 digits:",substr(largestr,nchar(largestr)-19,nchar(largestr)),"\n",
    "number of digits: ",nchar(largestr),"\n")
```

{{out}}

```txt
first 20 digits: 62060698786608744707
 last 20 digits: 92256259918212890625
 number of digits:  183231
```



## Racket


```Racket
#lang racket

(define answer (number->string (foldr expt 1 '(5 4 3 2))))
(define len (string-length answer))

(printf "Got ~a digits~n" len)
(printf "~a ... ~a~n"
        (substring answer 0 20)
        (substring answer (- len 20) len))

```

{{out}}
```txt
Got 183231 digits
62060698786608744707 ... 92256259918212890625
```



## REXX

REXX comes with built-in support for fixed precision integers that can be manually set to a large value of precision (digits).

Most REXXes have a practical limit of around eight million bytes, but that is mostly an underlying limitation of addressing virtual storage.

### manual setting of decimal digits

Note:  both REXX versions (below) don't work with:
:::*   PC/REXX
:::*   Personal REXX
as those REXX versions have a practical maximum of around '''3,700''' or less for '''numeric digits'''   (officially, it's '''4K''').

The '''3,700''' limit is based on the setting of RXISA, program size, and the amount of storage used by REXX variables.


Both (below) REXX programs have been tested with:
:::*   PC/REXX             (can't execute correctly)
:::*   Personal REXX                           (can't execute correctly)
:::*   Regina REXX
:::*   R4
:::*   ROO
:::*   ooRexx                 (tested by Walter Pachl)

```rexx
/*REXX program calculates and demonstrates  arbitrary precision numbers (using powers). */
numeric digits 200000                            /*two hundred thousand decimal digits. */

    # = 5 ** (4 ** (3 ** 2) )                    /*calculate multiple exponentiations.  */

true=62060698786608744707...92256259918212890625 /*what answer is supposed to look like.*/
rexx= left(#, 20)'...'right(#, 20)               /*the left and right 20 decimal digits.*/

say  '  true:'    true                           /*show what the  "true"  answer is.    */
say  '  REXX:'    rexx                           /*  "    "   "    REXX      "    "     */
say  'digits:'    length(#)                      /*  "    "   "   length  of answer is. */
say
if true == rexx   then say 'passed!'             /*either it passed,  ···               */
                  else say 'failed!'             /*    or it didn't.                    */
                                                 /*stick a fork in it,  we're all done. */
```

{{output|output}}

```txt

 check: 62060698786608744707...92256259918212890625
sample: 62060698786608744707...92256259918212890625
digits: 183231

passed!

```



### automatic setting of decimal digits


```rexx
/*REXX program calculates and demonstrates  arbitrary precision numbers (using powers). */
numeric digits 5                                 /*just use enough digits for 1st time. */

                  #=5** (4** (3** 2) )           /*calculate multiple exponentiations.  */

parse var  #  'E'  pow  .                        /*POW   might be null,  so   N  is OK. */

if pow\==''  then do                             /*general case:   POW  might be < zero.*/
                  numeric digits  abs(pow) + 9   /*recalculate with more decimal digits.*/
                  #=5** (4** (3** 2) )           /*calculate multiple exponentiations.  */
                  end                            /* [↑]  calculation is the real McCoy. */

true=62060698786608744707...92256259918212890625 /*what answer is supposed to look like.*/
rexx= left(#, 20)'...'right(#, 20)               /*the left and right 20 decimal digits.*/

say  '  true:'    true                           /*show what the  "true"  answer is.    */
say  '  REXX:'    rexx                           /*  "    "   "    REXX      "    "     */
say  'digits:'    length(#)                      /*  "    "   "   length  of answer is. */
say
if true == rexx   then say 'passed!'             /*either it passed,  ···               */
                  else say 'failed!'             /*    or it didn't.                    */
                                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=  is the same as the 1<sup>st</sup> REXX version.}}





## Ruby

Ruby comes with built-in support for arbitrary precision integers.


```ruby
irb(main):001:0> y = ( 5**4**3**2 ).to_s
puts "5**4**3**2 = #{y[0..19]}...#{y[-20..-1]} and has #{y.length} digits"

```

{{out}}

```txt

5**4**3**2 = 62060698786608744707...92256259918212890625 and has 183231 digits

```



## Run BASIC


```runbasic
x$ = str$( 5^(4^(3^2)))
print "Length:";len( x$)
print left$( x$, 20); "......"; right$( x$, 20)
```

{{out}}

```txt
Length:183231
62060698786608744707......92256259918212890625
```



## Rust

This is accomplished via the `num` crate. This used to be part of the standard library, but was relegated to an external crate when Rust hit 1.0. It is still owned and maintained by members of the Rust core team and is the de-facto library for numerical generics and arbitrary precision arithmetic.


```rust
extern crate num;
use num::bigint::BigUint;
use num::FromPrimitive;
use num::pow::pow;

fn main() {
    let big = BigUint::from_u8(5).unwrap();
    let answer_as_string = format!("{}", pow(big,pow(4,pow(3,2))));

      // The rest is output formatting.
    let first_twenty: String = answer_as_string.chars().take(20).collect();
    let last_twenty_reversed: Vec<char> = answer_as_string.chars().rev().take(20).collect();
    let last_twenty: String = last_twenty_reversed.into_iter().rev().collect();
    println!("Number of digits: {}", answer_as_string.len());
    println!("First and last digits: {:?}..{:?}", first_twenty, last_twenty);
}
```

{{out}}

```txt

Number of digits: 183231
First and last digits: "62060698786608744707".."92256259918212890625"

```



## Sather


```sather
class MAIN is
  main is
    r:INTI;
    p1 ::= "62060698786608744707";
    p2 ::= "92256259918212890625";

    -- computing 5^(4^(3^2)), it could be written
    -- also e.g. (5.inti)^((4.inti)^((3.inti)^(2.inti)))
    r  := (3.pow(2)).inti;
    r  := (4.inti).pow(r);
    r  := (5.inti).pow(r);

    sr ::= r.str; -- string rappr. of the number
    if sr.head(p1.size) = p1
       and sr.tail(p2.size) = p2 then
         #OUT + "result is ok..\n";
    else
         #OUT + "oops\n";
    end;
    #OUT + "# of digits: " + sr.size + "\n";
  end;
end;
```

{{out}}

```txt
result is ok..
# of digits: 183231
```



## Scala

Scala does not come with support for arbitrary precision integers powered to arbitrary precision integers, except if performed on a module. It can use arbitrary precision integers in other ways, including powering them to 32-bits integers.

```scala>scala
 BigInt(5) modPow (BigInt(4) pow (BigInt(3) pow 2).toInt, BigInt(10) pow 20)
res21: scala.math.BigInt = 92256259918212890625

scala> (BigInt(5) pow (BigInt(4) pow (BigInt(3) pow 2).toInt).toInt).toString
res22: String = 6206069878660874470748320557284679309194219265199117173177383244
78446890420544620839553285931321349485035253770303663683982841794590287939217907
89641300156281305613064874236198955114921296922487632406742326659692228562195387
46210423235340883954495598715281862895110697243759768434501295076608139350684049
01191160699929926568099301259938271975526587719565309995276438998093283175080241
55833224724855977970015112594128926594587205662421861723789001208275184293399910
13912158886504596553858675842231519094813553261073608575593794241686443569888058
92732524316323249492420512640962691673104618378381545202638771401061171968052873
21414945463925055899307933774904078819911387324217976311238875802878310483037255
33789567769926391314746986316354035923183981697660495275234703657750678459919...
scala> res22 take 20
res23: String = 62060698786608744707

scala> res22 length
res24: Int = 183231

scala>
```



## Scheme

[http://people.csail.mit.edu/jaffer/r4rs_8.html#SEC52 R<sup>4</sup>RS] and [http://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.3 R<sup>5</sup>RS] encourage, and [http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-6.html#node_sec_3.4 R<sup>6</sup>RS] requires, that exact integers be of arbitrary precision.

```scheme
(define x (expt 5 (expt 4 (expt 3 2))))
(define y (number->string x))
(define l (string-length y))
(display (string-append "5**4**3**2 = " (substring y 0 20) "..." (substring y (- l 20) l) " and has " (number->string l) " digits"))
(newline)
```

{{out}}

```txt
5**4**3**2 = 62060698786608744707...92256259918212890625 and has 183231 digits
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const proc: main is func
  local
    var bigInteger: fiveToThePowerOf262144 is 5_ ** 4 ** 3 ** 2;
    var string: numberAsString is str(fiveToThePowerOf262144);
  begin
    writeln("5**4**3**2 = " <& numberAsString[..20] <&
            "..." <& numberAsString[length(numberAsString) - 19 ..]);
    writeln("decimal digits: " <& length(numberAsString));
  end func;
```

{{out}}

```txt

5**4**3**2 = 62060698786608744707...92256259918212890625
decimal digits: 183231

```



## Sidef


```ruby
var x = 5**(4**(3**2));
var y = x.to_s;
printf("5**4**3**2 = %s...%s and has %i digits\n", y.ft(0,19), y.ft(-20), y.len);
```

{{out}}

```txt

5**4**3**2 = 62060698786608744707...92256259918212890625 and has 183231 digits

```



## SIMPOL

{{incomplete|SIMPOL|Number of digits in result not given.}}
SIMPOL supports arbitrary precision integers powered to arbitrary precision integers. This is the only integer data type in SIMPOL. SIMPOL supports conversion from its integer data type to other formats when calling external library functions.

```simpol
constant FIRST20 "62060698786608744707"
constant LAST20  "92256259918212890625"

function main()
  integer i
  string s, s2

  i = .ipower(5, .ipower(4, .ipower(3, 2)))
  s2 = .tostr(i, 10)
  if .lstr(s2, 20) == FIRST20 and .rstr(s2, 20) == LAST20
    s = "Success! The integer matches both the first 20 and the last 20 digits. There are " + .tostr(.len(s2), 10) + " digits in the result.{d}{a}"
  else
    s = ""
    if .lstr(s2, 20) != FIRST20
      s = "Failure! The first 20 digits are: " + .lstr(s2, 20) + " but they should be: " + FIRST20 + "{d}{a}"
    end if
    if .rstr(s2, 20) != LAST20
      s = s + "Failure! The first 20 digits are: " + .lstr(s2, 20) + " but they should be: " + LAST20 + "{d}{a}"
    end if
  end if
end function s
```



## Smalltalk

This code in Squeak Smalltalk returns a string containing the first 20 digits, last 20 digits and length of the result.

A very simple approach:

```smalltalk
|num|
num := (5 raisedTo: (4 raisedTo: (3 raisedTo: 2))) asString.
Transcript
   show: (num first: 20), '...', (num last: 20); cr;
   show: 'digits: ', num size asString.
```


On a Transcript window:

```txt
62060698786608744707...92256259918212890625
digits: 183231
```

And a more advanced one:

```smalltalk
|num numstr|
num := (2 to: 5) fold: [:exp :base| base raisedTo: exp].
numstr := num asString.
'<1s>...<2s>  digits:<3p>'
   expandMacrosWith: (numstr first: 20)
   with: (numstr last: 20)
   with: numstr size.
```

{{out}}

```txt
'62060698786608744707...92256259918212890625  digits: 183231'
```



## SPL


```spl
t = #.str(5^(4^(3^2)))
n = #.size(t)
#.output(n," digits")
#.output(#.mid(t,1,20),"...",#.mid(t,n-19,20))
```

{{out}}

```txt

183231 digits
62060698786608744707...92256259918212890625

```



## Standard ML


```sml
let
  val answer = IntInf.pow (5, IntInf.toInt (IntInf.pow (4, IntInf.toInt (IntInf.pow (3, 2)))))
  val s = IntInf.toString answer
  val len = size s
in
  print ("has " ^ Int.toString len ^ " digits: " ^
         substring (s, 0, 20) ^ " ... " ^
         substring (s, len-20, 20) ^ "\n")
end;
```

it took too long to run


###  mLite

mLite does not have a logarithm function so one was constructed (see fun log10)

```sml

fun
	ntol (0, x) = if len x < 1 then [0] else x
       | (n, x) = ntol (n div 10, (n mod 10) :: x)
       | n      = ntol (n, [])
and
	powers_of_10 9 = 1000000000
               | 8 = 100000000
               | 7 = 10000000
               | 6 = 1000000
               | 5 = 100000
               | 4 = 10000
               | 3 = 1000
               | 2 = 100
               | 1 = 10
               | 0 = 1
and
	size (c, 0) = c
       | (c, n > 9999999999) = size (c + 10, trunc (n / 10000000000))
       | (c, n)              = size (c +  1, trunc (n / 10))
       | n                   = size (     0, trunc (n / 10))
and
	makeVisible L = map (fn x = if int x then chr (x + 48) else x) L
and
	log10 (n, 0, x) = ston ` implode ` makeVisible ` rev x
        | (n, c, x) =
            let val n' = n^10;
              val size_n' = size n'
            in
              log10 (n' / powers_of_10 size_n', c - 1, size_n' :: x)
			end
        | (n, c) =
            let
              val size_n = size n
            in
              log10 (n / 10^size_n, c, #"." :: rev (ntol size_n) @ [])
            end
;
val fourThreeTwo = 4^3^2;
val fiveFourThreeTwo = 5^fourThreeTwo;

val digitCount = trunc (log10(5,6) * fourThreeTwo + 0.5);
print "Count  = "; println digitCount;

val end20 = fiveFourThreeTwo mod (10^20);
print "End 20 = "; println end20;

val top20 = fiveFourThreeTwo div (10^(digitCount - 20));
print "Top 20 = "; println top20;

```

Output

```txt

 Count = 183231
 End 20 = 92256259918212890625
 Top 20 = 62060698786608744707

```

Took 1 hour and 9 minutes to run (AMD A6, Windows 10)


## Tcl

Tcl supports arbitrary precision integers (and an exponentiation operator) from 8.5 onwards.
{{works with|Tcl|8.5}}

```tcl
set bigValue [expr {5**4**3**2}]
puts "5**4**3**2 has [string length $bigValue] digits"
if {[string match "62060698786608744707*92256259918212890625" $bigValue]} {
    puts "Value starts with 62060698786608744707, ends with 92256259918212890625"
} else {
    puts "Value does not match 62060698786608744707...92256259918212890625"
}
```

{{out}}

```txt

5**4**3**2 has 183231 digits
Value starts with 62060698786608744707, ends with 92256259918212890625

```



## TXR


```txr
@(bind (f20 l20 ndig)
       @(let* ((str (tostring (expt 5 4 3 2)))
               (len (length str)))
          (list [str :..20] [str -20..:] len)))
@(bind f20 "62060698786608744707")
@(bind l20 "92256259918212890625")
@(output)
@f20...@l20
ndigits=@ndig
@(end)
```

{{out}}

```txt
62060698786608744707...92256259918212890625
ndigits=183231
```



## Ursa

The Ursa standard library provides the module <code>unbounded_int</code> which contains the definition of the <code>unbounded_int</code> type. In Cygnus/X Ursa, <code>unbounded_int</code> is essentially a wrapper for <code>java.math.BigInteger</code>


### Usage


```ursa
import "unbounded_int"
decl unbounded_int x
x.set ((x.valueof 5).pow ((x.valueof 4).pow ((x.valueof 3).pow 2)))

decl string first last xstr
set xstr (string x)

# get the first twenty digits
decl int i
for (set i 0) (< i 20) (inc i)
	set first (+ first xstr<i>)
end for

# get the last twenty digits
for (set i (- (size xstr) 20)) (< i (size xstr)) (inc i)
	set last (+ last xstr<i>)
end for

out "the first and last digits of 5^(4^(3^2)) are " first "..." console
out last " (the result was " (size xstr) " digits long)" endl endl console

if (and (and (= first "62060698786608744707") (= last "92256259918212890625")) (= (size xstr) 183231))
	out "(pass)" endl console
else
	out "FAIL" endl console
end if
```



### Output

{{out}}

```txt
the first and last digits of 5^(4^(3^2)) are 62060698786608744707...92256259918212890625 (the result was 183231 digits long)

(pass)
```



## Ursala

There are no infix arithmetic operators in the language, but there is a <code>power</code> function in the <code>bcd</code> library, which is part of the standard distribution from the home site.

There is no distinction between ordinary and arbitrary precision integers, but the binary converted decimal representation used here is more efficient than the usual binary representation in calculations that would otherwise be dominated by the conversion to decimal output.

```Ursala
#import std
#import nat
#import bcd

#show+

main = <.@ixtPX take/$20; ^|T/~& '...'--@x,'length: '--@h+ %nP+ length@t>@h %vP power=> <5_,4_,3_,2_>
```

With this calculation taking about a day to run, correct results are attainable but not performant.

```txt
62060698786608744707...92256259918212890625
length: 183231
```



## Visual Basic .NET

{{trans|C#}}
{{libheader|System.Numerics}}
Addressing the issue of the '''BigInteger.Pow()''' function having the exponent value limited to '''Int32.MaxValue''' (2147483647), here are a couple of alternative implementations using a '''BigInteger''' for the exponent.

```vbnet
Imports System.Numerics

Module Program

    Dim Implems() As String = {"Built-In", "Recursive", "Iterative"}
    Dim powers() As Integer = {5, 4, 3, 2}

    Function intPowR(val As BigInteger, exp As BigInteger) As BigInteger
        Debug.WriteLine(exp)
        If exp = 0 Then Return 1
        Dim ne As BigInteger, vs As BigInteger = val * val
        If exp.IsEven Then
            ne = exp / 2 : If ne > 1 Then Return intPowR(vs, ne) Else Return vs
        End If
        ne = (exp - 1) / 2 : If ne > 1 Then Return val * intPowR(vs, ne) Else Return vs * val
    End Function

    Function intPowI(val As BigInteger, exp As BigInteger) As BigInteger
        intPowI = 1 : While (exp > 0) : If Not exp.IsEven Then intPowI *= val
            val *= val : exp >>= 1 : End While
    End Function

    Sub DoOne(selection As Integer, p() As Integer)
        Dim st As DateTime = DateTime.Now, result As String
        Select Case (selection)
            Case 0
                result = BigInteger.Pow(p(0), BigInteger.Pow(p(1), BigInteger.Pow(p(2), p(3)))).ToString()
            Case 1
                result = intPowR(p(0), intPowR(p(1), intPowR(p(2), p(3)))).ToString()
            Case Else
                result = intPowI(p(0), intPowI(p(1), intPowI(p(2), p(3)))).ToString()
        End Select
        Dim et As TimeSpan = DateTime.Now - st
        Debug.Assert(result.Length = 183231)
        Debug.Assert(result.StartsWith("62060698786608744707"))
        Debug.Assert(result.EndsWith("92256259918212890625"))
        Console.WriteLine("n = {0}^{1}^{2}^{3}", powers(0), powers(1), powers(2), powers(3))
        Console.WriteLine("n = {0}...{1}", result.Substring(0, 20), result.Substring(result.Length - 20, 20))
        Console.WriteLine("n digits = {0}", result.Length)
        Console.WriteLine("{0} elasped: {1} milliseconds.", Implems(selection), et.TotalMilliseconds)
    End Sub

    Sub Main()
        For i As Integer = 0 To 2 : DoOne(i, powers) : Next
        If Debugger.IsAttached Then Console.ReadKey()
    End Sub
End Module
```

{{out}}

```txt
n = 5^4^3^2
n = 62060698786608744707...92256259918212890625
n digits = 183231
Built-In elasped: 2487.4002 milliseconds.
n = 5^4^3^2
n = 62060698786608744707...92256259918212890625
n digits = 183231
Recursive elasped: 2413.0434 milliseconds.
n = 5^4^3^2
n = 62060698786608744707...92256259918212890625
n digits = 183231
Iterative elasped: 2412.5477 milliseconds.
```
'''Remarks:''' Not much difference in execution times for three methods.  But the exponents are relatively small.  If one does need to evaluate an exponent greater than '''Int32.MaxValue''', the execution time will be measured in hours.


## zkl

Using the GNU big num library:

```zkl
var BN=Import("zklBigNum");
n:=BN(5).pow(BN(4).pow(BN(3).pow(2)));
s:=n.toString();
"%,d".fmt(s.len()).println();
println(s[0,20],"...",s[-20,*]);
```

{{out}}

```txt

183,231
62060698786608744707...92256259918212890625

```


{{omit from|AWK|Only has double-precision floating-point numbers.}}
{{omit from|AutoHotkey}}
{{omit from|Batch File}}
{{omit from|Brainfuck}}
{{omit from|Forth|No support for big integers beyond double-cell integers}}
{{omit from|PostScript}}
{{omit from|SAS}}
{{omit from|Scratch|No support for big integers}}
{{omit from|sed|No numbers.}}
{{omit from|Stata}}
{{omit from|ZX Spectrum Basic|No support for big integers}}
