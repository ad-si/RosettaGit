+++
title = "Time-based One-time Password Algorithm"
description = ""
date = 2019-03-31T15:02:45Z
aliases = []
[extra]
id = 17949
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "csharp",
  "go",
  "perl_6",
  "picolisp",
  "racket",
  "tcl",
  "zkl",
]
+++

A Time-based One-time Password Algorithm (TOTP) is an algorithm that computes a one-time password from a shared secret key and the current time.  It is the cornerstone of [[wp:Initiative_For_Open_Authentication|Initiative For Open Authentication (OATH)]] and is used in a number of two factor authentication systems.  Essentially, both the server and the client compute the time-limited token, then the server checks if the token supplied by the client matches the locally generated token.

The task here is to implement this algorithm using 'HMAC-SHA1' and an optional step is to generate the random [[wp:Base32|Base-32]] string used as the secret key, but this is not a requirement.  A reference implementation, based on JavaScript, can be found at the following location:

[http://blog.tinisles.com/2011/10/google-authenticator-one-time-password-algorithm-in-javascript http://blog.tinisles.com/2011/10/google-authenticator-one-time-password-algorithm-in-javascript]

According to RFC 6238, the reference implementation is as follows:
* Generate a key, K, which is an arbitrary bytestring, and share it securely with the client.
* Agree upon an epoch, T0, and an interval, TI, which will be used to calculate the value of the counter C (defaults are the Unix epoch as T0 and 30 seconds as TI)
* Agree upon a cryptographic hash method (default is SHA-1)
* Agree upon a token length, N (default is 6)

Although RFC 6238 allows different parameters to be used, the Google implementation of the authenticator app does not support T0, TI values, hash methods and token lengths different from the default. It also expects the K secret key to be entered (or supplied in a QR code) in base-32 encoding according to RFC 3548.

* [https://itunes.apple.com/gb/app/google-authenticator/id388497605 Google Authenticator App (Apple iOS)]
* [https://play.google.com/store/apps/details?id=com.google.android.apps.authenticator2 Google Authenticator App (Google Android)]
* [http://www.windowsphone.com/en-us/store/app/authenticator/e7994dbc-2336-4950-91ba-ca22d653759b Microsoft Authenticator App (Windows Phone)]

=={{header|Caché ObjectScript}}==


```cos

Class Utils.Security [ Abstract ]
{

ClassMethod GetOTP(b32secret As %String) As %String
{
	// convert base32 secret into string
	Set key=..B32ToStr(b32secret)

	// get the unix time, divide by 30 and convert into eight-byte string
	Set epoch=..GetUnixTime()
	Set val=$Reverse($ZQChar(epoch\30))

	// compute the HMAC SHA-1 hash and get the last nibble...
	Set hmac=$System.Encryption.HMACSHA1(val, key)
	Set last=$ASCII($Extract(hmac, *))

	// calculate the offset and get one-time password string
	Set offset=$ZBoolean(last, $Char(15), 1)  // logical 'AND' operation
	Set otpstr=$ZBoolean($Extract(hmac, offset+1, offset+4), $Char(127,255,255,255), 1)

	// convert string into decimal and return last six digits
	Set otpdec=$ZLASCII($Reverse(otpstr))
	Quit ..LeftPad(otpdec, 6)
}

ClassMethod GetUnixTime() As %Integer [ Private ]
{
	// current date and time in UTC time format
	Set now=$ZTimeStamp
	Set daydiff=(now - $ZDateH("1970-01-01", 3))
	Set secs=$Piece(now, ",", 2)\1
	Quit (daydiff*60*60*24)+secs
}

ClassMethod LeftPad(str As %String, len As %Integer, pad As %String = 0) As %String [ Private ]
{
	Quit $Extract($Translate($Justify(str, len), " ", pad), *-(len-1), *)
}

ClassMethod ConvertBase10ToN(pNum As %Integer = "", pBase As %Integer = "", pBaseStr As %String = "", pPos As %Integer = 0) As %String [ Private ]
{
	If pNum=0 Quit ""
	Set str=..ConvertBase10ToN(pNum\pBase, pBase, pBaseStr, pPos+1)
	Quit str_$Extract(pBaseStr, pNum#pBase+1)
}

ClassMethod ConvertBaseNTo10(pStr As %String = "", pBase As %Integer = "", pBaseStr As %String = "", pPos As %Integer = 0) As %Integer [ Private ]
{
	If pStr="" Quit 0
	Set num=..ConvertBaseNTo10($Extract(pStr, 1, *-1), pBase, pBaseStr, pPos+1)
	Set dec=$Find(pBaseStr, $Extract(pStr, *))-2
	Quit num+(dec*(pBase**pPos))
}

ClassMethod B32ToStr(b32str As %String) As %String [ Private ]
{
	Set b32str=$ZConvert(b32str,"U")
	Set b32alp="ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
	Set (bits,str)=""
	For i=1:1:$Length(b32str) {
		Set val=$Find(b32alp, $Extract(b32str, i))-2
		Set bits=bits_..LeftPad(..ConvertBase10ToN(val, 2, "01"), 5)
	}
	For i=1:8:$Length(bits) {
		Set chunk=$Extract(bits, i, i+7)
		Set str=str_$Char(..ConvertBaseNTo10(chunk, 2, "01"))
	}
	Quit str
}

ClassMethod GenerateSecret() As %String
{
	// initialise base 32 string and alphabet
	Set b32str="", b32alp="ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

	// build a large base 32 string
	For pass=1:1:4 {
		Set b32str=b32str_..ConvertBase10ToN($System.Encryption.GenCryptToken(), 32, b32alp)
	}

	// return randomly generated password
	Quit ..LeftPad(b32str, 16)
}

}

```

```txt

DEMO>for i=1:1:5 write ##class(Utils.Security).GetOTP("JBSWY3DPEHPK3PXP"),! hang 15  // wait fifteen seconds
992374
992374
169898
169898
487462

DEMO>write ##class(User.View.Security).GenerateSecret()
5FWQZLQVXIBCKKMJ
DEMO>write ##class(User.View.Security).GenerateSecret()
M4AKQBFI252H4BWO

```



## C#

```c#
using System;
using System.Security.Cryptography;

namespace RosettaTOTP
{
    public class TOTP_SHA1
    {
        private byte[] K;
        public TOTP_SHA1()
        {
            GenerateKey();
        }
        public void GenerateKey()
        {
            using (RandomNumberGenerator rng = new RNGCryptoServiceProvider())
            {
                /*    Keys SHOULD be of the length of the HMAC output to facilitate
                      interoperability.*/
                K = new byte[HMACSHA1.Create().HashSize / 8];
                rng.GetBytes(K);
            }
        }
        public int HOTP(UInt64 C, int digits = 6)
        {
            var hmac = HMACSHA1.Create();
            hmac.Key = K;
            hmac.ComputeHash(BitConverter.GetBytes(C));
            return Truncate(hmac.Hash, digits);
        }
        public UInt64 CounterNow(int T1 = 30)
        {
            var secondsSinceEpoch = (DateTime.UtcNow - new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds;
            return (UInt64)Math.Floor(secondsSinceEpoch / T1);
        }
        private int DT(byte[] hmac_result)
        {
            int offset = hmac_result[19] & 0xf;
            int bin_code = (hmac_result[offset] & 0x7f) << 24
               | (hmac_result[offset + 1] & 0xff) << 16
               | (hmac_result[offset + 2] & 0xff) << 8
               | (hmac_result[offset + 3] & 0xff);
            return bin_code;
        }

        private int Truncate(byte[] hmac_result, int digits)
        {
            var Snum = DT(hmac_result);
            return Snum % (int)Math.Pow(10, digits);
        }
    }


    class Program
    {
        static void Main(string[] args)
        {
            var totp = new TOTP_SHA1();
            Console.WriteLine(totp.HOTP(totp.CounterNow()));
        }
    }
}

```



## Go

A slightly [https://github.com/gwwfps/onetime/pull/1 fixed] version of a [https://github.com/gwwfps/onetime package by Zitao Zhang] (released under a [https://github.com/gwwfps/onetime/blob/master/LICENSE simplified BSD license]).

```go
// Package onetime provides a library for one-time password generation,
// implementing the HOTP and TOTP algorithms as specified by IETF RFC-4226
// and RFC-6238.
package onetime

import (
	"crypto/hmac"
	"crypto/sha1"
	"encoding/binary"
	"errors"
	"hash"
	"math"
	"time"
)

// OneTimePassword stores the configuration values relevant to HOTP/TOTP calculations.
type OneTimePassword struct {
	Digit    int              // Length of code generated
	TimeStep time.Duration    // Length of each time step for TOTP
	BaseTime time.Time        // The start time for TOTP step calculation
	Hash     func() hash.Hash // Hash algorithm used with HMAC
}

// HOTP returns a HOTP code with the given secret and counter.
func (otp *OneTimePassword) HOTP(secret []byte, count uint64) uint {
	hs := otp.hmacSum(secret, count)
	return otp.truncate(hs)
}

func (otp *OneTimePassword) hmacSum(secret []byte, count uint64) []byte {
	mac := hmac.New(otp.Hash, secret)
	binary.Write(mac, binary.BigEndian, count)
	return mac.Sum(nil)
}

func (otp *OneTimePassword) truncate(hs []byte) uint {
	sbits := dt(hs)
	snum := uint(sbits[3]) | uint(sbits[2])<<8
	snum |= uint(sbits[1])<<16 | uint(sbits[0])<<24
	return snum % uint(math.Pow(10, float64(otp.Digit)))
}

// Simple returns a new OneTimePassword with the specified HTOP code length,
// SHA-1 as the HMAC hash algorithm, the Unix epoch as the base time, and
// 30 seconds as the step length.
func Simple(digit int) (otp OneTimePassword, err error) {
	if digit < 6 {
		err = errors.New("minimum of 6 digits is required for a valid HTOP code")
		return
	} else if digit > 9 {
		err = errors.New("HTOP code cannot be longer than 9 digits")
		return
	}
	const step = 30 * time.Second
	otp = OneTimePassword{digit, step, time.Unix(0, 0), sha1.New}
	return
}

// TOTP returns a TOTP code calculated with the current time and the given secret.
func (otp *OneTimePassword) TOTP(secret []byte) uint {
	return otp.HOTP(secret, otp.steps(time.Now()))
}

func (otp *OneTimePassword) steps(now time.Time) uint64 {
	elapsed := now.Unix() - otp.BaseTime.Unix()
	return uint64(float64(elapsed) / otp.TimeStep.Seconds())
}

func dt(hs []byte) []byte {
	offset := int(hs[len(hs)-1] & 0xf)
	p := hs[offset : offset+4]
	p[0] &= 0x7f
	return p
}
```

(in a format that gets put into the [https://godoc.org/github.com/dchapes/onetime generated documentation])

```go
package onetime

import (
	"crypto/sha256"
	"fmt"
	"time"

	"github.com/gwwfps/onetime"
)

func Example_simple() {
	// Simple 6-digit HOTP code:
	var secret = []byte("SOME_SECRET")
	var counter uint64 = 123456
	var otp, _ = onetime.Simple(6)
	var code = otp.HOTP(secret, counter)
	fmt.Println(code)
	// Output:
	// 260040
}

func Example_authenticator() {
	// Google authenticator style 8-digit TOTP code:
	var secret = []byte("SOME_SECRET")
	var otp, _ = onetime.Simple(8)
	var code = otp.TOTP(secret)
	fmt.Println(code)
}

func Example_custom() {
	// 9-digit 5-second-step TOTP starting on midnight 2000-01-01 UTC, using SHA-256:
	var secret = []byte("SOME_SECRET")
	const ts = 5 * time.Second
	var t = time.Date(2000, time.January, 1, 0, 0, 0, 0, time.UTC)
	var otp = onetime.OneTimePassword{
		Digit: 9, TimeStep: ts, BaseTime: t, Hash: sha256.New}
	var code = otp.TOTP(secret)
	fmt.Println(code)
}
```



## Perl 6

This is a minimal attempt that covers only the "Time-based" part of the requirement.

```perl6
#!/usr/bin/env perl6

# Reference:
# https://github.com/retupmoca/P6-Digest-HMAC

use v6.d;
use Digest::HMAC;
use Digest::SHA;

sub totp (Str \secret, DateTime \counter, Int \T0=0, Int \T1=30 --> Str) {
   my \key = ( counter - DateTime.new(T0) ).Int div T1;
   return hmac-hex(key.Str, secret, &sha1).substr(0,6) # first 6 chars of sha1
}

my $message = "show me the monKey";

say "Deterministic output at ", DateTime.new(2177452800), " with fixed checks,";
loop (my $t = 2177452800 ; $t < 2177452900 ; $t+= 17 ) { # Y2038 safe
   say totp $message, DateTime.new($t);
}

say "Current time output at ", DateTime.new(now), " with random checks,";
loop (my $n = 0 ; $n < 6 ; $n++, sleep (13..23).roll ) {
   say totp $message, DateTime.new(now);
}

```

```txt
Deterministic output at 2039-01-01T00:00:00Z with fixed checks,
34ca2a
acfa3f
950fc3
950fc3
a2d4ea
a2d4ea
Current time output at 2019-03-31T15:00:01.765312Z with random checks,
4e36de
d4e9f8
d4e9f8
077e2c
63bbb5
63bbb5
```



## PicoLisp

Using the <tt>sha1</tt> function defined at ''[[SHA-1#PicoLisp]]'':

```PicoLisp
(load "sha1.l")

(de hmac ("Fun" Msg Key)
   (let (Key (copy Key)  Len (length Key))
      (and
         (> Len 64)
         (setq Key ("Fun" Key)) )
      (setq Key (need -64 Key 0))
      ("Fun"
         (append
            (mapcar x| (need 64 `(hex "5C")) Key)
            ("Fun" (append (mapcar x| (need 64 `(hex "36")) Key) Msg)) ) ) ) )

(de endian64 (N)
   (make
      (do 8
         (yoke (& N 255))
         (setq N (>> 8 N)) ) ) )

(de endian32 (L)
   (apply
      |
      (mapcar >> (-24 -16 -8 0) L) ) )

(de truncate (Lst D)
   (let L (nth Lst (inc (& (last Lst) `(hex "F"))))
      (set L (& (car L) `(hex "7F")))
      (% (endian32 (head 4 L)) (** 10 D)) ) )

(de hotp (K N D)
   (default D 6)
   (truncate
      (hmac 'sha1 (endian64 N) K)
      D ) )

(def 'totp hotp)

# RFC4226
(for
   (I . N)
   (755224 287082 359152 969429 338314
      254676 287922 162583 399871 520489 )
   (test
      N
      (hotp (mapcar char (chop "12345678901234567890")) (dec I)) ) )

# RFC6238
(for L
   (quote
      (1 . 94287082) (37037036 . 7081804) (37037037 . 14050471)
      (41152263 . 89005924) (66666666 . 69279037) (666666666 . 65353130) )
   (test
      (cdr L)
      (totp (mapcar char (chop "12345678901234567890")) (car L) 8) ) )
```



## Racket

This includes BASE32 encoding, token based authentication and other such stuff.


```racket
#lang racket
(require (only-in web-server/stuffers/hmac-sha1 HMAC-SHA1))

(define << arithmetic-shift) ; deep down, there lurks a C programmer in me
(define && bitwise-and)

;; These are the parameters available through RFC6238
(define T0 (make-parameter current-seconds)) ; produces unix epoch times. parameterised for testing
(define X  (make-parameter 30))
(define H  (make-parameter (lambda (k d) (HMAC-SHA1 k d))))
(define N  (make-parameter 6))

;; http://tools.ietf.org/html/rfc4226#section-5.3
(define (HOTP sha1-bytes (Digit (N)))
  (define (DT b)
    (define offset (&& #b1111 (bytes-ref b 19)))
    (define P/32 (subbytes b offset (+ offset 4)))
    (+ (<< (bytes-ref P/32 3) 0) (<< (bytes-ref P/32 2) 8) (<< (bytes-ref P/32 1) 16)
       (<< (&& #b01111111 (bytes-ref P/32 0)) 24)))
  (define s-bits (DT sha1-bytes))
  (modulo s-bits (expt 10 Digit)))

(define (Generate-HOTP K C (Digit (N)))
  (HOTP ((H) K (integer->integer-bytes C 8 #t)) Digit))

;; http://tools.ietf.org/html/rfc6238
(define (T #:previous-timeframe (T- 0))
  (- (quotient ((T0)) (X)) T-))

(define (TOTP K #:previous-timeframe (T- 0)) (Generate-HOTP K (T #:previous-timeframe T-) (N)))

;; RFC 3548
(define (pad-needed bits)
  (modulo (- 5 bits) 5))

(define (5-bits->base32-char n)
  (string-ref "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" n))

(define (base32-encode-block bs)
  (define v (for/fold ((v 0)) ((b bs)) (+ (<< v 8) b)))
  (define v-bits (* 8 (bytes-length bs)))
  (define pad (pad-needed v-bits))
  (define padded-bits (+ v-bits pad))
  (define v-padded (<< v pad))
  (for ((end-bit (in-range padded-bits 4 -5)))
    (write-char (5-bits->base32-char (bitwise-bit-field v-padded (- end-bit 5) end-bit))))
  (write-string (make-string (- 8 (/ padded-bits 5)) #\=)))

(define A-char (char->integer #\A))
(define Z-char (char->integer #\Z))
(define 2-char (char->integer #\2))
(define 7-char (char->integer #\7))
(define =-char (char->integer #\=))
(define (byte->5bit b)
  (cond
    [(<= A-char b Z-char) (- b A-char)]
    [(<= 2-char b 7-char) (+ 26 (- b 2-char))]
    [else #f]))

(define (base32-decode-block bs)
  (for*/fold ((v 0) (b 0)) ((bt bs) (b5 (in-value (byte->5bit bt))) #:break (not b5))
    (define v+ (+ (<< v 5) b5))
    (define b+ (+ b 5))
    (cond
      [(< b+ 8) (values v+ b+)]
      [else
       (define start-bit (- b+ 8))
       (write-byte (&& 255 (<< v+ (- start-bit))))
       (values (bitwise-bit-field v+ 0 start-bit) start-bit)])))

(define (base32-encode) (for ((bs (in-port (curry read-bytes 5)))) (base32-encode-block bs)))
(define (base32-decode) (for ((bs (in-port (curry read-bytes 8)))) (base32-decode-block bs)))

(define (base32-encode-bytes b) (with-input-from-bytes b (λ () (with-output-to-bytes base32-encode))))
(define (base32-decode-bytes b) (with-input-from-bytes b (λ () (with-output-to-bytes base32-decode))))

(module+ main
  (require racket/date)
  ;; my secret, as stuck on a postit note on my monitor
  (define Tims-K #"Super Secret Password Key 88!")

  (define ((pseudo-time-now (offset 0))) (+ 1413976828 offset))
  (define totp #f)
  (parameterize ((T0 (pseudo-time-now)))
    (printf "I want authentication at: ~a ~s~%" ((T0)) (date->string (seconds->date ((T0))) #t))
    (set! totp (TOTP (base32-encode-bytes Tims-K)))
    (printf "My TOTP is: ~a~%" totp)
    (printf "sent to authentication service...~%"))

  ;; as stored on authenticator
  (define K/base32 (base32-encode-bytes Tims-K))
  (printf "K/base32: ~a~%" K/base32)

  (parameterize ((T0 (pseudo-time-now 1)))
    (printf "1 second later... authentication service checks against: ~a~%" totp)
    (define auth-totp (TOTP K/base32))
    (printf "~a is the same? ~a~%" auth-totp (= totp auth-totp)))

  (parameterize ((T0 (pseudo-time-now 3)))
    (printf "but 3 seconds later... authentication service checks against: ~a~%" totp)
    (define auth-totp (TOTP K/base32))
    (printf "~a is the same? ~a~%" auth-totp (= totp auth-totp))
    (printf "oh dear... fall back one time-frame...~%")
    (define auth-totp-1 (TOTP K/base32 #:previous-timeframe 1))
    (printf "~a is *that* the same? ~a~%" auth-totp-1 (= totp auth-totp-1))))

(module+ test
  (require tests/eli-tester)
  (test
   ;; From RFC4226 Page 7
   (HOTP (bytes
          #x1f #x86 #x98 #x69 #x0e #x02 #xca #x16 #x61 #x85
          #x50 #xef #x7f #x19 #xda #x8e #x94 #x5b #x55 #x5a)
         6)
   => 872921

   (pad-needed 0) => 0
   (pad-needed 2) => 3
   (pad-needed 4) => 1
   (pad-needed 6) => 4
   (pad-needed 8) => 2
   (pad-needed 10) => 0
   (pad-needed 12) => 3

   ;; http://commons.apache.org/proper/commons-codec/xref-test/org/apache/commons/codec/binary/Base32Test.html
   (base32-encode-bytes #"")       => #""
   (base32-encode-bytes #"f")      => #"MY======"
   (base32-encode-bytes #"fo")     => #"MZXQ===="
   (base32-encode-bytes #"foo")    => #"MZXW6==="
   (base32-encode-bytes #"foob")   => #"MZXW6YQ="
   (base32-encode-bytes #"fooba")  => #"MZXW6YTB"
   (base32-encode-bytes #"foobar") => #"MZXW6YTBOI======"

   (base32-decode-bytes #"")                 => #""
   (base32-decode-bytes #"MY======")         => #"f"
   (base32-decode-bytes #"MZXQ====")         => #"fo"
   (base32-decode-bytes #"MZXW6===")         => #"foo"
   (base32-decode-bytes #"MZXW6YQ=")         => #"foob"
   (base32-decode-bytes #"MZXW6YTB")         => #"fooba"
   (base32-decode-bytes #"MZXW6YTBOI======") => #"foobar"

   (base32-encode-bytes #"Super Secret Password Key 88!")
   => #"KN2XAZLSEBJWKY3SMV2CAUDBONZXO33SMQQEWZLZEA4DQII="
   ))
```


```txt
23 tests passed
I want authentication at: 1413976828 "Wednesday, October 22nd, 2014 12:20:28pm"
My TOTP is: 742249
sent to authentication service...
K/base32: KN2XAZLSEBJWKY3SMV2CAUDBONZXO33SMQQEWZLZEA4DQII=
1 second later... authentication service checks against: 742249
742249 is the same? #t
but 3 seconds later... authentication service checks against: 742249
317129 is the same? #f
oh dear... fall back one time-frame...
742249 is *that* the same? #t
```



## Tcl


This TOTP/HOTP module clocks in small by taking advantage of [https://core.tcl.tk/tcllib/doc/trunk/embedded/www/toc.html tcllib's] existing hashing and base32 modules.


```Tcl

# rfc6238 contains examples of hotp with 8-digit modulus and sha1/sha256/sha512 hmac
#
# these require options handling, perhaps http://wiki.tcl.tk/38965
#
catch {namespace delete ::totp}
namespace eval ::totp {
    package require sha1

    oo::class create totp {
        variable Secret
        variable Interval
        variable Window
        constructor {secret {interval 30} {window 300}} {
            if {![string is digit $interval]} {
                set interval [expr {[clock scan $interval] - [clock scan now]}]
            }
            if {![string is digit $window]} {
                set window [expr {[clock scan $window] - [clock scan now]}]
            }
            if {$window % $interval} {
                throw {TOTP BADARGS} "$window is not a multiple of $interval"
            }
            set window [expr {$window / $interval}]
            set Secret $secret
            set Interval $interval
            set Window $window
        }
        method totp {{when now}} {
            if {![string is integer $when]} {
                set when [clock scan $when]
            }
            set when [expr {$when / $Interval}]
            set bytes [binary format W $when]
            binary scan $bytes H* when
            hotp $Secret $bytes
        }

    }

    proc hotp {secret bytes} {
        set hmac [sha1::hmac -bin $secret $bytes]
        set ofs [string index $hmac end]
        binary scan $ofs c ofs
        set ofs [expr {$ofs & 0xf}]
        set chunk [string range $hmac $ofs $ofs+4]
        binary scan $chunk I code
        return [expr {($code & 0x7fffffff) % 1000000}]
    }

    namespace export *
}
namespace import ::totp::*

if 0 {  ;# tests
    if {[info commands assert] eq ""} {
        proc assert {what} {
            puts [uplevel 1 [list subst $what]]
        }
    }
    totp::totp create t 12345678901234567890
    assert {287082 eq [t totp 59]}

    t destroy
    package require base32
    totp::totp create t [base32::decode AAAAAAAAAAAAAAAA]
    proc google {when} {
        list [t totp [expr {$when-30}]] [t totp $when] [t totp [expr {$when+30}]]
    }
    assert {{306281 553572 304383} eq [google 1400000000]}
}
```



## zkl

Uses the MsgHash dll, which includes SHA-1, SHA-256 hashes and HMAC routines for SHA-* and MD5.
```zkl
var [const] MsgHash = Import.lib("zklMsgHash");

// OneTimePassword stores the configuration values relevant to HOTP/TOTP calculations.
class OneTimePassword{
   fcn init(Digit,TimeStep,BaseTime,HMAC){
      var digit   =Digit,	// Length of code generated, # digits
	  timeStep=TimeStep,	// Length of each time step for TOTP, in seconds
	  baseTime=BaseTime, 	// The start time for TOTP step calculation (seconds since Unix epoch)
	  hmac    =HMAC;	// Hash algorithm used with HMAC --> bytes
   }
   // hotp returns a HOTP code with the given secret and counter.
   fcn hotp(secret,count){ // eg ("SOME_SECRET",123456)
      hmac(secret,count.toBigEndian(8)) : // (key,msg), msg is count as 8 bytes
      // --> 20 bytes (SHA1), eg (de,7c,9b,85,b8,b7,8a,a6,bc,8a,7a,36,f7,0a,90,70,1c,9d,b4,d9)
      truncate(_)
   }
   fcn truncate(hs)  // pick off bottom digit digits
      { dt(hs) % (10).pow(digit) }
   fcn dt(hs){
      hs[-1].bitAnd(0xf) : // bottom 4 bits (0-15) of LSB of hash to index
      hs.toBigEndian(_,4)  // 4 bytes of hash to 32 bit unsigned int
   }

   // Simple returns a new OneTimePassword with the specified HTOP code length,
   // SHA-1 as the HMAC hash algorithm, the Unix epoch as the base time, and
   // 30 seconds as the step length.
   fcn simple(digit){ //--> OneTimePassword
      if(digit<6)
	 throw(Exception.ValueError("minimum of 6 digits is required for a valid HTOP code"));
      if(digit>9)
	 throw(Exception.ValueError("HTOP code cannot be longer than 9 digits"));
      self(digit,30,0,MsgHash.extra.hmacSHA1.fp2(False))
   }
   // TOTP returns a TOTP code calculated with the current time and the given secret.
   fcn totp(secret){ hotp(secret, steps(Time.Clock.time())) }
   fcn steps(now)  { (now - baseTime)/timeStep } // elapsed time chunked
} // OneTimePassword
```

Note: MsgHash hashes return a string by default, they can also return the hash as bytes. Ditto the HMAC routines, it is the third parameter. So, to create a hmac that returns bytes, use (eg) MsgHash.extra.hmacSHA1.fp2(False), this creates a partial application (closure) of the hmac using SHA-1 fixing the third parameter as False.
```zkl
fcn example_simple{
   // Simple 6-digit HOTP code:
   secret  := "SOME_SECRET";
   counter := 123456;
   otp     := OneTimePassword.simple(6);
   code    := otp.hotp(secret, counter);
   println(code);  //-->260040 (const)
}();

fcn example_authenticator{
   // Google authenticator style 8-digit TOTP code:
   secret := "SOME_SECRET";
   otp    := OneTimePassword.simple(8);
   code   :=otp.totp(secret);
   println(code)  //-->eg 44653788
}();

fcn example_custom{
   // 9-digit 5-second-step TOTP starting on midnight 2000-01-01 UTC, using SHA-256:
   secret := "SOME_SECRET";
   ts     := 5;  // seconds
   t      := Time.Clock.mktime(2000,1,1, 0,0,0); //(Y,M,D, h,m,s)
   otp    := OneTimePassword(9,ts,t,MsgHash.extra.hmacSHA256.fp2(False));
   code   := otp.totp(secret);
   println(code)  //-->eg 707355416
}();
```

Showing how to sync with changes over time. A six digit OTP w/MD5 changing every 17 seconds. Latency can effect the result when totp is called at a time boundary, so a retry may be required.

```zkl
fcn overTime{
   secret,ts:= "SOME_SECRET",17;
   otp      := OneTimePassword(6,ts,Time.Clock.time(),MsgHash.extra.hmacMD5.fp2(False));
   chg,s    := 0,"";
   while(1){
      code,t := otp.totp(secret),Time.Clock.time() - otp.baseTime;
      if(t/ts!=chg){ chg = t/ts; s=" (should change)"; }
      println("%4d: %6d %s".fmt(t,code,s)); s = "";
      Atomic.sleep(10);
   }
}
```

```txt

   0:  53454
  10:  53454
  20:   2947  (should change)
  30:   2947
  40: 287972  (should change)
  50: 287972
...
 220: 510180
 230:    207  (should change)
 240: 380959  (should change)
 250: 380959

```

The MsgHash HMAC routines are pretty simple (the hash code is C), included here for completeness:

```zkl
// https://en.wikipedia.org/wiki/Hash-based_message_authentication_code
fcn hmac(key,msg,hashFcn,asString){
   blkSz,H,Hx := 64,hashFcn.fp1(1,Data()), (asString and hashFcn or H);
   kn:=key.len();
   if(kn>blkSz) key=H(key).howza(0);
   else         key=Data().fill(0,blkSz-kn).write(key).howza(0);
   opad:=key.pump(Data,(0x5c).bitXor);
   ipad:=key.pump(Data,(0x36).bitXor);

   Hx(opad + H(ipad + msg))  //-->String|Data
}
fcn hmacSHA1(  key,msg,asString=True){ hmac(key,msg,MsgHash.SHA1,  asString) }
fcn hmacSHA256(key,msg,asString=True){ hmac(key,msg,MsgHash.SHA256,asString) }
fcn hmacMD5(   key,msg,asString=True){ hmac(key,msg,Utils.MD5.calc,asString) }
```

