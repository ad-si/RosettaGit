+++
title = "Parallel Brute Force"
description = ""
date = 2019-10-11T02:32:55Z
aliases = []
[extra]
id = 21299
[taxonomies]
categories = []
tags = []
+++

{{task|Parallel Brute Force}}

;Task:

Find, through brute force, the five-letter passwords corresponding with the following [[wp:SHA-256|SHA-256]] hashes:

 1. 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
 2. 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
 3. 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f

Your program should naively iterate through all possible passwords consisting only of five lower-case ASCII English letters.  It should use concurrent or parallel processing, if your language supports that feature.  You may calculate SHA-256 hashes by calling a library or through a custom implementation. Print each matching password, along with its SHA-256 hash. 

Related task:  [[SHA-256]]


## BaCon


```qbasic>PRAGMA INCLUDE <openssl/sha.h

PRAGMA LDFLAGS -lcrypto

OPTION MEMTYPE unsigned char

LOCAL buffer[32], passwd[5] TYPE unsigned char
LOCAL result TYPE unsigned char*
LOCAL a,b,c,d,e TYPE int

DATA "a13bbac91141bb5cfc6f1dc723256243775aeb3517671b350ce3f4c607d693c7", "ea125efa275b675155f4f1a00ae8b6e361ea2ed486db1a55b805e4fc5f47441a", "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"

WHILE TRUE

    READ secret$
    IF NOT(LEN(secret$)) THEN BREAK

    FOR i = 0 TO 31
        buffer[i] = DEC(MID$(secret$, i*2+1, 2))
    NEXT

    FOR a = 97 TO 122
        FOR b = 97 TO 122
            FOR c = 97 TO 122
                FOR d = 97 TO 122
                    FOR e = 97 TO 122
                        passwd[0] = a
                        passwd[1] = b
                        passwd[2] = c
                        passwd[3] = d
                        passwd[4] = e

                        result = SHA256(passwd, 5, 0)

                        FOR i = 0 TO SHA256_DIGEST_LENGTH-1
                            IF PEEK(result+i) != buffer[i] THEN BREAK
                        NEXT
                        IF i = SHA256_DIGEST_LENGTH THEN
                            PRINT a,b,c,d,e,secret$ FORMAT "%c%c%c%c%c:%s\n"
                            BREAK 5
                        END IF
                    NEXT
                NEXT
            NEXT
        NEXT
    NEXT
WEND
```

{{out}}

```txt

apple:3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
mmmmm:74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
zyzzx:1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad

```



## C

{{trans|C#}}

```c
// $ gcc -o parabrutfor parabrutfor.c -fopenmp -lssl -lcrypto
// $ export OMP_NUM_THREADS=4
// $ ./parabrutfor

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <omp.h>
#include <openssl/sha.h>

typedef unsigned char byte;

int matches(byte *a, byte* b) {
	for (int i = 0; i < 32; i++)
		if (a[i] != b[i])
			return 0;
	return 1;
}


byte* StringHashToByteArray(const char* s) {
	byte* hash = (byte*) malloc(32);
	char two[3];
	two[2] = 0;
	for (int i = 0; i < 32; i++) {
		two[0] = s[i * 2];
		two[1] = s[i * 2 + 1];
		hash[i] = (byte)strtol(two, 0, 16);
	}
	return hash;
}

void printResult(byte* password, byte* hash) {
	char sPass[6];
	memcpy(sPass, password, 5);
	sPass[5] = 0;
	printf("%s => ", sPass);
	for (int i = 0; i < SHA256_DIGEST_LENGTH; i++)
		printf("%02x", hash[i]);
	printf("\n");
}

int main(int argc, char **argv)
{

#pragma omp parallel
	{

#pragma omp for
		for (int a = 0; a < 26; a++)
		{
			byte password[5] = { 97 + a };
			byte* one =   StringHashToByteArray("1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad");
			byte* two =   StringHashToByteArray("3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b");
			byte* three = StringHashToByteArray("74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f");
			for (password[1] = 97; password[1] < 123; password[1]++)
				for (password[2] = 97; password[2] < 123; password[2]++)
					for (password[3] = 97; password[3] < 123; password[3]++)
						for (password[4] = 97; password[4] < 123; password[4]++) {
							byte *hash = SHA256(password, 5, 0);
							if (matches(one, hash) || matches(two, hash) || matches(three, hash))
								printResult(password, hash);
						}
			free(one);
			free(two);
			free(three);
		}
	}

	return 0;
}
```

{{out}}

```txt
apple => 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
mmmmm => 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
zyzzx => 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
```


=={{header|C sharp|C#}}==
 

```csharp
using System;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static void Main(string[] args)
    {
        Parallel.For(0, 26, a => {
            byte[] password = new byte[5];
            byte[] hash;
            byte[] one = StringHashToByteArray("1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad");
            byte[] two = StringHashToByteArray("3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b");
            byte[] three = StringHashToByteArray("74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f");
            password[0] = (byte)(97 + a);
            var sha = System.Security.Cryptography.SHA256.Create();
            for (password[1] = 97; password[1] < 123; password[1]++)
                for (password[2] = 97; password[2] < 123; password[2]++)
                    for (password[3] = 97; password[3] < 123; password[3]++)
                        for (password[4] = 97; password[4] < 123; password[4]++)
                        {
                            hash = sha.ComputeHash(password);
                            if (matches(one, hash) || matches(two, hash) || matches(three, hash))
                                Console.WriteLine(Encoding.ASCII.GetString(password) + " => "
                                    + BitConverter.ToString(hash).ToLower().Replace("-", ""));
                        }
        });
    }
    static byte[] StringHashToByteArray(string s)
    {
        return Enumerable.Range(0, s.Length / 2).Select(i => (byte)Convert.ToInt16(s.Substring(i * 2, 2), 16)).ToArray();
    }
    static bool matches(byte[] a, byte[] b)
    {
        for (int i = 0; i < 32; i++)
            if (a[i] != b[i])
                return false;
        return true;
    }
}
```


{{out}}

```txt
apple => 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
mmmmm => 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
zyzzx => 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
```



## Clojure

{{libheader|clojure.math.combinatorics}}

```Clojure
(ns rosetta.brute-force
  (:require [clojure.math.combinatorics :refer [selections]]) ;; https://github.com/clojure/math.combinatorics
  (:import  [java.util Arrays]
            [java.security MessageDigest]))

;;https://rosettacode.org/wiki/Parallel_Brute_Force

(def targets ;; length = 5
  ["1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"
   "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"
   "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"])

;; HELPER/UTIL fns
;;
### ===========


(defn digest
  "Given a byte-array <bs> returns its hash (also a byte-array)."
  ^bytes [^MessageDigest md ^bytes bs]
  (.digest md bs))

(defn char-range
  "Helper fn for easily producing character ranges."
  [start end]
  (map char (range (int start)
                   (inc (int end)))))

(def low-case-eng-bytes
  "Our search-space (all lower case english characters converted to bytes)."
  (map byte (char-range \a \z)))

(defn hex->bytes
  "Converts a hex string to a byte-array."
  ^bytes [^String hex]
  (let [len (.length hex)
        ret (byte-array (/ len 2))]
    (run! (fn [i]
            (aset ret
                  (/ i 2)
                  ^byte (unchecked-add-int
                          (bit-shift-left
                            (Character/digit (.charAt hex i) 16)
                            4)
                          (Character/digit (.charAt hex (inc i)) 16))))
          (range 0 len 2))
    ret))

(defn bytes->hex
  "Converts a byte-array to a hex string."
  [^bytes bs]
  (.toString
    ^StringBuilder
    (areduce bs idx ret (StringBuilder.)
      (doto ret (.append (format "%02x" (aget bs idx)))))))

;; MAIN LOGIC
;;
### =====


(defn check-candidate
  "Checks whether the SHA256 hash of <candidate> (a list of 5 bytes),
   matches <target>. If it does, returns that hash as a hex-encoded String.
   Otherwise returns nil."
  [^bytes target sha256 candidate]
  (let [candidate-bytes (byte-array candidate)
        ^bytes candidate-hash (sha256 candidate-bytes)]
    (when (Arrays/equals target candidate-hash)
      (let [answer (String. candidate-bytes)]
        (println "Answer found for:" (bytes->hex candidate-hash) "=>" answer)
        answer))))

(defn sha256-brute-force
  "Top level function. Returns a list with the 3 answers."
  [space hex-hashes]
  (->> hex-hashes
       (map hex->bytes) ;; convert the hex strings to bytes
       (pmap            ;; parallel map the checker-fn
         (fn [target-bytes]
           (let [message-digest (MessageDigest/getInstance "SHA-256") ;; new digest instance per thread
                 sha256 (partial digest message-digest)]
             (some (partial check-candidate target-bytes sha256)
                   (selections space 5)))))))

```

{{out}}

```txt
Answer found for: 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b => apple
Answer found for: 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f => mmmmm
Answer found for: 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad => zyzzx
```



## Common Lisp

{{libheader|lparallel}}
{{libheader|ironclad}}

```lisp
(defpackage #:parallel-brute-force
  (:use #:cl
        #:lparallel))

(in-package #:parallel-brute-force)

(defparameter *alphabet* "abcdefghijklmnopqrstuvwxyz")
(defparameter *hash0* "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad")
(defparameter *hash1* "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b")
(defparameter *hash2* "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f")
(defparameter *kernel-size* 7)

(defun sha-256 (input)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array input))))

(defun call-with-5-char-string (fun first-char)
  (loop with str = (make-array 5 :element-type 'character :initial-element first-char)
        for c1 across *alphabet*
        do (setf (char str 1) c1)
           (loop for c2 across *alphabet*
                 do (setf (char str 2) c2)
                    (loop for c3 across *alphabet*
                          do (setf (char str 3) c3)
                             (loop for c4 across *alphabet*
                                   do (setf (char str 4) c4)
                                      (funcall fun (copy-seq str)))))))

(defmacro with-5-char-string ((str first-char) &body body)
  `(call-with-5-char-string (lambda (,str) ,@body) ,first-char))

(defun find-passwords-with (first-char)
  (let (results)
    (with-5-char-string (str first-char)
      (let ((hash (sha-256 str)))
        (when (or (string= hash *hash0*) (string= hash *hash1*) (string= hash *hash2*))
          (push (list str hash) results))))
    (nreverse results)))

(defun find-passwords ()
  (setf *kernel* (make-kernel *kernel-size*))
  (let ((results (unwind-protect
                      (pmapcan #'find-passwords-with *alphabet*)
                   (end-kernel))))
    (dolist (r results)
      (format t "~A: ~A~%" (first r) (second r)))))
```

{{out}}

```txt
apple: 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
mmmmm: 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
zyzzx: 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
```



## D

There is at least one more method not shown for doing the task in parallel, which uses the std.concurrency module instead.

```D
import std.digest.sha;
import std.parallelism;
import std.range;
import std.stdio;

// Find the five lower-case letter strings representing the following sha256 hashes
immutable p1 = cast(ubyte[32]) x"1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad";
immutable p2 = cast(ubyte[32]) x"3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b";
immutable p3 = cast(ubyte[32]) x"74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f";

void main() {
    import std.datetime.stopwatch;

    auto sw = StopWatch(AutoStart.yes);
    // Switch these top loops to toggle between non-parallel and parrallel solutions.
    // foreach(char a; 'a'..'z'+1) {
    foreach(i, a; taskPool.parallel(iota('a', 'z'+1))) {
        char[5] psw;
        psw[0] = cast(char) a;
        foreach(char b; 'a'..'z'+1) {
            psw[1] = b;
            foreach(char c; 'a'..'z'+1) {
                psw[2] = c;
                foreach(char d; 'a'..'z'+1) {
                    psw[3] = d;
                    foreach(char e; 'a'..'z'+1) {
                        psw[4] = e;
                        auto hash = psw.sha256Of;
                        if (equal(hash, p1) || equal(hash, p2) || equal(hash, p3)) {
                            writefln("%s <=> %(%x%)", psw, hash);
                        }
                    }
                }
            }
        }
    }
    sw.stop;
    writeln(sw.peek);
}

//Specialization that supports static arrays too
bool equal(T)(const T[] p, const T[] q) {
    if (p.length != q.length) {
        return false;
    }

    for(int i=0; i<p.length; i++) {
        if (p[i] != q[i]) {
            return false;
        }
    }

    return true;
}
```


{{out}}
Parallel run time:    9 secs, 684 ms,     678 ╬╝s, and 6 hnsecs

Sequential run time: 29 secs, 298 ms, and 837 ╬╝s

```txt
apple <=> 3a7bd3e236a3d29eea436fcfb7e44c735d117c42d1c183542b6b9942dd4f1b
mmmmm <=> 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
zyzzx <=> 1115dd80feaacefdf481f1f9070374a2a81e27880f187396db67958b27cbad
```


=={{header|F_Sharp|F#}}==

```fsharp

(*
Nigel Galloway February 21st., 2017
*)
let N n i g e l = 
  let G = function
    |"3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"->Some(string n+string i+string g+string e+string l)
    |"74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"->Some(string n+string i+string g+string e+string l)
    |"1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"->Some(string n+string i+string g+string e+string l)
    |_->None
  G ([|byte n;byte i;byte g;byte e;byte l|]|>System.Security.Cryptography.SHA256.Create().ComputeHash|>Array.map(fun (x:byte)->System.String.Format("{0:x2}",x))|>String.concat "")
open System.Threading.Tasks
let n1 = Task.Factory.StartNew(fun ()->['a'..'m']|>List.collect(fun n->['a'..'m']|>List.collect(fun i->['a'..'m']|>List.collect(fun g->['a'..'z']|>List.collect(fun e->['a'..'z']|>List.choose(fun l->N n i g e l))))))
let n2 = Task.Factory.StartNew(fun ()->['a'..'m']|>List.collect(fun n->['a'..'m']|>List.collect(fun i->['n'..'z']|>List.collect(fun g->['a'..'z']|>List.collect(fun e->['a'..'z']|>List.choose(fun l->N n i g e l))))))
let n3 = Task.Factory.StartNew(fun ()->['a'..'m']|>List.collect(fun n->['n'..'z']|>List.collect(fun i->['a'..'m']|>List.collect(fun g->['a'..'z']|>List.collect(fun e->['a'..'z']|>List.choose(fun l->N n i g e l))))))
let n4 = Task.Factory.StartNew(fun ()->['a'..'m']|>List.collect(fun n->['n'..'z']|>List.collect(fun i->['n'..'z']|>List.collect(fun g->['a'..'z']|>List.collect(fun e->['a'..'z']|>List.choose(fun l->N n i g e l))))))
let n5 = Task.Factory.StartNew(fun ()->['n'..'z']|>List.collect(fun n->['a'..'m']|>List.collect(fun i->['a'..'m']|>List.collect(fun g->['a'..'z']|>List.collect(fun e->['a'..'z']|>List.choose(fun l->N n i g e l))))))
let n6 = Task.Factory.StartNew(fun ()->['n'..'z']|>List.collect(fun n->['a'..'m']|>List.collect(fun i->['n'..'z']|>List.collect(fun g->['a'..'z']|>List.collect(fun e->['a'..'z']|>List.choose(fun l->N n i g e l))))))
let n7 = Task.Factory.StartNew(fun ()->['n'..'z']|>List.collect(fun n->['n'..'z']|>List.collect(fun i->['a'..'m']|>List.collect(fun g->['a'..'z']|>List.collect(fun e->['a'..'z']|>List.choose(fun l->N n i g e l))))))
let n8 = Task.Factory.StartNew(fun ()->['n'..'z']|>List.collect(fun n->['n'..'z']|>List.collect(fun i->['n'..'z']|>List.collect(fun g->['a'..'z']|>List.collect(fun e->['a'..'z']|>List.choose(fun l->N n i g e l))))))

for r in n1.Result@n2.Result@n3.Result@n4.Result@n5.Result@n6.Result@n7.Result@n8.Result do printfn "%s" r

```

{{out}}

```txt


mmmmm
apple
zyzzx

```



## Go

This solution runs 26 goroutines, one for each possible password first letter.
Goroutines run in parallel on a multicore system.

```go
package main

import (
    "crypto/sha256"
    "encoding/hex"
    "log"
    "sync"
)

var hh = []string{
    "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad",
    "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b",
    "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f",
}

func main() {
    log.SetFlags(0)
    hd := make([][sha256.Size]byte, len(hh))
    for i, h := range hh {
        hex.Decode(hd[i][:], []byte(h))
    }
    var wg sync.WaitGroup
    wg.Add(26)
    for c := byte('a'); c <= 'z'; c++ {
        go bf4(c, hd, &wg)
    }
    wg.Wait()
}

func bf4(c byte, hd [][sha256.Size]byte, wg *sync.WaitGroup) {
    p := []byte("aaaaa")
    p[0] = c
    p1 := p[1:]
p:
    for {
        ph := sha256.Sum256(p)
        for i, h := range hd {
            if h == ph {
                log.Println(string(p), hh[i])
            }
        }
        for i, v := range p1 {
            if v < 'z' {
                p1[i]++
                continue p
            }
            p1[i] = 'a'
        }
        wg.Done()
        return
    }
}
```

{{out}}

```txt

zyzzx 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
apple 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
mmmmm 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f

```



## Java

{{works with |Java| 1.5}}
This example uses Java's multithreading capabilities. Note that the Java Virtual Machine will use native Threads if the underlying platform supprts them. If there is no native thread support, the Java VM will emulate threads.
This implementation runs 3 threads (one per hash to crack), and short-stops when a match for a hash is found.


```Java
import javax.xml.bind.DatatypeConverter;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * "Main Program" that does the parallel processing
 */
public class ParallelBruteForce {

    public static void main(String[] args) throws NoSuchAlgorithmException {

        //the hashes to be cracked
        String[] hashes = {"1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad",
                "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b",
                "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"};

        //An ExecutorService is a high-level parallel programming facility, that can execute a number of tasks
        //the FixedThreadPool is an ExecutorService that uses a configurable number of parallel threads
        ExecutorService executorService = Executors.newFixedThreadPool(3);

        //Submit one Task per hash to the thread po
        for (String hash : hashes) {
            executorService.submit(new Forcer(hash));
        }

        //An ExecutorSerice must be shut down properly (this also causes the program to await termination of
        // all pending tasks in the thread pool)
        executorService.shutdown();
    }
}

/**
 * The Class that contains the actual brute-forcing task.
 * <p>
 * It implements the build-in Interface "Runnable", so it can be run on a Thread or a Thread-Execution-Facility
 * (such as an ExecutorService).
 */
class Forcer implements Runnable {

    private static final int LENGTH = 5;

    //These will sore the hash to be cracked in both bytes (required for comparison) and String representation
    // (required for output)
    private final byte[] crackMe;
    private final String crackMeString;

    //The MessageDigest does the SHA-256 caclulation. Note that this may throw a NoSuchAlgorithmException when there
    // is no SHA-256 implementation in the local standard libraries (but that algorithm is mandatory, so this code
    // probably will never throw that Excpetion
    private final MessageDigest digest = MessageDigest.getInstance("SHA-256");

    public Forcer(String crackMe) throws NoSuchAlgorithmException {
        this.crackMeString = crackMe;
        this.crackMe = DatatypeConverter.parseHexBinary(crackMe);
    }

    @Override
    public void run() {

        String match = "";

        //all loops use this array for their counters. This is very dirty and should never be done in production!
        char[] chars = new char[LENGTH];

        //used for short-stopping when a match is found - one could abuse the match-variable for this, but this is
        // much clearer
        boolean done = false;

        for (chars[0] = 'a'; chars[0] <= 'z' && !done; chars[0]++) {
            for (chars[1] = 'a'; chars[1] <= 'z' && !done; chars[1]++) {
                for (chars[2] = 'a'; chars[2] <= 'z' && !done; chars[2]++) {
                    for (chars[3] = 'a'; chars[3] <= 'z' && !done; chars[3]++) {
                        for (chars[4] = 'a'; chars[4] <= 'z' && !done; chars[4]++) {
                            //the String creation is necessary to get the encoding right
                            String canidate = new String(chars);
                            //genenrate SHA-256 hash using Java's standard facilities
                            byte[] hash = digest.digest(canidate.getBytes());
                            if (Arrays.equals(hash, crackMe)) {
                                match = canidate;
                                done = true;
                            }

                        }
                    }
                }
            }
        }
        System.out.println(String.format("Hash %s has the following match : %s", crackMeString, match));
    }
}



```

{{out}}
```txt
Hash 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b has the following match : apple
Hash 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f has the following match : mmmmm
Hash 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad has the following match : zyzzx
```





## Julia


```julia
@everywhere using SHA

@everywhere function bruteForceRange(startSerial, numberToDo)
  targets = ["1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad",
             "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b",
             "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"]
  targets = map(hex2bytes, targets)
  for count = 1 : numberToDo
    password = [UInt8(97 + x) for x in digits(UInt8, startSerial + count, 26, 5)]
    hashbytes = sha256(password)
    if (hashbytes[1] == 0x11 || hashbytes[1] == 0x3a || hashbytes[1] == 0x74) && findfirst(targets, hashbytes) > 0
      hexstring = join(hex(x,2) for x in hashbytes)
      passwordstring = join(map(Char, password))
      println("$passwordstring --> $hexstring")
    end
  end
  return 0
end

@everywhere perThread = div(26^5, Sys.CPU_CORES)
pmap(x -> bruteForceRange(x * perThread, perThread), 0:Sys.CPU_CORES-1)

```

{{out}}
```txt
From worker 2:  apple --> 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
From worker 3:  zyzzx --> 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
From worker 4:  mmmmm --> 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
```



## Kotlin

{{trans|C#}}

```scala
// version 1.1.51

import java.security.MessageDigest

fun stringHashToByteHash(hash: String): ByteArray {
    val ba = ByteArray(32)
    for (i in 0 until 64 step 2) ba[i / 2] = hash.substring(i, i + 2).toInt(16).toByte()
    return ba
}

fun ByteArray.matches(other: ByteArray): Boolean {
    for (i in 0 until 32) {
        if (this[i] != other[i]) return false
    }
    return true
}

fun main(args: Array<String>) {
    val stringHashes = listOf(
        "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad",
        "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b",
        "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"
    )
    val byteHashes = List(3) { stringHashToByteHash(stringHashes[it]) }
    val letters = List(26) { (97 + it).toByte() }

    letters.stream().parallel().forEach {
        val md = MessageDigest.getInstance("SHA-256")
        val range = 97..122
        val pwd = ByteArray(5)
        pwd[0] = it
        for (i1 in range) {
            pwd[1] = i1.toByte()    
            for (i2 in range) {
                pwd[2] = i2.toByte()
                for (i3 in range) {
                    pwd[3] = i3.toByte()
                    for (i4 in range) {
                        pwd[4] = i4.toByte()
                        val ba = md.digest(pwd)
                        for (j in 0..2) {
                            if (ba.matches(byteHashes[j])) {          
                                val password = pwd.toString(Charsets.US_ASCII)
                                println("$password => ${stringHashes[j]}")
                                break
                            }
                        }
                    }
                }
            }
        }
    }
}
```


{{out}}

```txt

mmmmm => 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
apple => 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
zyzzx => 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad

```



## Mathematica



```Mathematica
testPassword[pass_String] := 
 If[MemberQ[{16^^1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad,
    16^^3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b,
    16^^74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f},
   Hash[pass, "SHA256"]], Print[pass]];

chars=CharacterRange["a","z"];

ParallelDo[
    testPassword[StringJoin[a, b, c, d, e]],
 {a, chars}, {b, chars}, {c, chars}, {d, chars}, {e, chars}]
```


=={{header|Modula-2}}==

```modula2
MODULE PBF;
FROM FormatString IMPORT FormatString;
FROM SHA256 IMPORT SHA256,Create,Destroy,HashBytes,Finalize,GetHash;
FROM SYSTEM IMPORT ADR,ADDRESS,BYTE;
FROM Terminal IMPORT Write,WriteString,WriteLn,ReadChar;
FROM Threads IMPORT Thread,CreateThread,WaitForThreadTermination;

PROCEDURE PrintHexBytes(str : ARRAY OF BYTE; limit : INTEGER);
VAR
    buf : ARRAY[0..7] OF CHAR;
    i,v : INTEGER;
BEGIN
    i := 0;
    WHILE i<limit DO
        v := ORD(str[i]);
        IF v < 16 THEN
            WriteString("0")
        END;
        FormatString("%h", buf, v);
        WriteString(buf);
        INC(i);
    END
END PrintHexBytes;

PROCEDURE Check(str : ARRAY OF CHAR);
TYPE
    HA = ARRAY[0..31] OF BYTE;
CONST
    h1 = HA{3aH, 7bH, 0d3H, 0e2H, 36H, 0aH, 3dH, 29H, 0eeH, 0a4H, 36H, 0fcH, 0fbH, 7eH, 44H, 0c7H, 35H, 0d1H, 17H, 0c4H, 2dH, 1cH, 18H, 35H, 42H, 0bH, 6bH, 99H, 42H, 0ddH, 4fH, 1bH};
    h2 = HA{74H, 0e1H, 0bbH, 62H, 0f8H, 0daH, 0bbH, 81H, 25H, 0a5H, 88H, 52H, 0b6H, 3bH, 0dfH, 6eH, 0aeH, 0f6H, 67H, 0cbH, 56H, 0acH, 7fH, 7cH, 0dbH, 0a6H, 0d7H, 30H, 5cH, 50H, 0a2H, 2fH};
    h3 = HA{11H, 15H, 0ddH, 80H, 0fH, 0eaH, 0acH, 0efH, 0dfH, 48H, 1fH, 1fH, 90H, 70H, 37H, 4aH, 2aH, 81H, 0e2H, 78H, 80H, 0f1H, 87H, 39H, 6dH, 0b6H, 79H, 58H, 0b2H, 07H, 0cbH, 0adH};
VAR
    hash : SHA256;
    out : ARRAY[0..31] OF BYTE;
    i : CARDINAL;
    match : BOOLEAN;
BEGIN
    hash := Create();

    HashBytes(hash, ADR(str), HIGH(str)+1);
    Finalize(hash);

    GetHash(hash, out);
    Destroy(hash);

    match := TRUE;
    FOR i:=0 TO HIGH(out) DO
        IF out[i] # h1[i] THEN
            match := FALSE;
            BREAK
        END
    END;
    IF match THEN
        WriteString(str);
        WriteString(" ");
        PrintHexBytes(out, 32);
        WriteLn;
        RETURN
    END;

    match := TRUE;
    FOR i:=0 TO HIGH(out) DO
        IF out[i] # h2[i] THEN
            match := FALSE;
            BREAK
        END
    END;
    IF match THEN
        WriteString(str);
        WriteString(" ");
        PrintHexBytes(out, 32);
        WriteLn;
        RETURN
    END;

    match := TRUE;
    FOR i:=0 TO HIGH(out) DO
        IF out[i] # h3[i] THEN
            match := FALSE;
            BREAK
        END
    END;
    IF match THEN
        WriteString(str);
        WriteString(" ");
        PrintHexBytes(out, 32);
        WriteLn
    END
END Check;

PROCEDURE CheckWords(a : CHAR);
VAR
    word : ARRAY[0..4] OF CHAR;
    b,c,d,e : CHAR;
BEGIN
    word[0] := a;
    FOR b:='a' TO 'z' DO
        word[1] := b;
        FOR c:='a' TO 'z' DO
            word[2] := c;
            FOR d:='a' TO 'z' DO
                word[3] := d;
                FOR e:='a' TO 'z' DO
                    word[4] := e;
                    Check(word)
                END
            END
        END
    END
END CheckWords;

PROCEDURE CheckAF(ptr : ADDRESS) : CARDINAL;
VAR a : CHAR;
BEGIN
    FOR a:='a' TO 'f' DO
        CheckWords(a)
    END;
    RETURN 0
END CheckAF;

PROCEDURE CheckGM(ptr : ADDRESS) : CARDINAL;
VAR a : CHAR;
BEGIN
    FOR a:='g' TO 'm' DO
        CheckWords(a)
    END;
    RETURN 0
END CheckGM;

PROCEDURE CheckNS(ptr : ADDRESS) : CARDINAL;
VAR a : CHAR;
BEGIN
    FOR a:='n' TO 's' DO
        CheckWords(a)
    END;
    RETURN 0
END CheckNS;

PROCEDURE CheckTZ(ptr : ADDRESS) : CARDINAL;
VAR a : CHAR;
BEGIN
    FOR a:='t' TO 'z' DO
        CheckWords(a)
    END;
    RETURN 0
END CheckTZ;

VAR
    t1,t2,t3,t4 : Thread;
    s1,s2,s3,s4 : CARDINAL;
BEGIN
    CreateThread(t1,CheckAF,NIL,0,TRUE);
    CreateThread(t2,CheckGM,NIL,0,TRUE);
    CreateThread(t3,CheckNS,NIL,0,TRUE);
    CreateThread(t4,CheckTZ,NIL,0,TRUE);

    WaitForThreadTermination(t1,-1,s1);
    WaitForThreadTermination(t2,-1,s2);
    WaitForThreadTermination(t3,-1,s3);
    WaitForThreadTermination(t4,-1,s4);

    WriteString("Done");
    WriteLn;
    ReadChar
END PBF.
```

{{out}}

```txt
apple 3A7BD3E2360A3D29EEA436FCFB7E44C735D117C42D1C1835420B6B9942DD4F1B
mmmmm 74E1BB62F8DABB8125A58852B63BDF6EAEF667CB56AC7F7CDBA6D7305C50A22F
zyzzx 1115DD800FEAACEFDF481F1F9070374A2A81E27880F187396DB67958B207CBAD
Done
```



## Perl

Uses threads library to do naive search using 26 threads ("aaaaa" .. "azzzz", "baaaa" .. "bzzzz", etc.).  No effort is made to early exit.
<lang>use Digest::SHA qw/sha256_hex/;
use threads;
use threads::shared;
my @results :shared;

print "$_ : ",join(" ",search($_)), "\n" for (qw/
  1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
  3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
  74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
/);


sub search {
  my $hash = shift;
  @results = ();
  $_->join() for map { threads->create('tsearch', $_, $hash) } 0..25;
  return @results;
}

sub tsearch {
  my($tnum, $hash) = @_;
  my $s = chr(ord("a")+$tnum) . "aaaa";

  for (1..456976) { # 26^4
    push @results, $s if sha256_hex($s) eq $hash;
    $s++;
  }
}
```

{{out}}

```txt

1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad : zyzzx
3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b : apple
74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f : mmmmm

```



## Perl 6

This solution can be changed from parallel to serial by removing the <code>.race</code> method.

```perl6
use Digest::SHA;
constant @alpha2 = [X~] <a   m p y z> xx 2;
constant @alpha3 = [X~] <e l m p x z> xx 3;

my %WANTED = set <
    3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
    74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
    1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
>;

sub find_it ( $first_two ) {
    return gather for $first_two «~« @alpha3 -> $password {
        my $digest_hex = sha256($password).list.fmt('%02x', '');
        take "$password => $digest_hex" if %WANTED{$digest_hex};
    }
}

.say for flat @alpha2.race(:1batch).map: {.&find_it.cache};
```

{{Out}}

```txt
apple => 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
mmmmm => 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
zyzzx => 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
```


Testers can adjust the run speed by replacing the @alpha constants with any of the below:

```perl6

# True to actual RC task, but slowest
constant @alpha2 = 'aa'  ..  'zz';
constant @alpha3 = 'aaa' .. 'zzz';
# Reduced alphabets for speed during development
constant @alpha2 = [X~] <a   m p y z> xx 2;
constant @alpha3 = [X~] <e l m p x z> xx 3;
# Alphabets reduced by position for even more speed
constant @alpha2 = [X~] <a m z>, <p m y>;
constant @alpha3 = [X~] <m p z>, <l m z>, <e m x>;
# Completely cheating
constant @alpha2 = <ap  mm  zy>;
constant @alpha3 = <ple mmm zzx>;
```



## Phix

Each thread processes one start letter at a time, until they are all done.

```Phix
include builtins\sha256.e
include builtins\VM\pThreadN.e -- (shd not be rqd on 0.8.1+)
 
function asHex(string s)
string res = ""
    for i=1 to length(s) do
        res &= sprintf("%02X",s[i])
    end for
    return res
end function
 
sequence starts
constant start_cs = init_cs(),      -- critical section
         hashes = {x"1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad",
                   x"3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b",
                   x"74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"}

procedure find_passwords()
    sequence thrashes = {}  -- thread-safe copy of hashes
    enter_cs(start_cs)
    for i=1 to length(hashes) do
        thrashes = append(thrashes,thread_safe_string(hashes[i]))
    end for
    leave_cs(start_cs)
    while true do
        string pwd
        enter_cs(start_cs)
        if length(starts)=0 then
            leave_cs(start_cs)
            exit
        end if
        pwd = starts[$]&repeat('a',4)
        starts = starts[1..$-1]
        leave_cs(start_cs)
        while length(pwd) do
            string hash = sha256(pwd)
            if find(hash,thrashes) then ?{pwd,asHex(hash)} end if
            for i=5 to 2 by -1 do
                if pwd[i]!='z' then
                    pwd[i] += 1
                    exit
                end if
                pwd[i] = 'a'
                if i=2 then pwd = "" exit end if
            end for
        end while
    end while
    exit_thread(0)
end procedure

for nthreads=4 to 4 do
    atom t0 = time()
    starts = tagset('a','z',-1)
    sequence threads = {}
    for i=1 to nthreads do
        threads = append(threads,create_thread(routine_id("find_passwords"),{}))
    end for
    wait_thread(threads)
    string e = elapsed(time()-t0)
    printf(1,"completed with %d threads in %s\n",{nthreads,e})
end for
```

{{out}} (with nthreads loop from 1 to 4, and for that case CPU use in Task Manager shows a very clear step pattern.)

```txt

{"apple","3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"}
{"mmmmm","74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"}
{"zyzzx","1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"}
completed with 1 threads in 29.1s
{"apple","3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"}
{"mmmmm","74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"}
{"zyzzx","1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"}
completed with 2 threads in 16.1s
{"apple","3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"}
{"mmmmm","74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"}
{"zyzzx","1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"}
completed with 3 threads in 13.8s
{"apple","3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"}
{"mmmmm","74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"}
{"zyzzx","1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"}
completed with 4 threads in 12.7s

```



## Python



```python
import multiprocessing
from hashlib import sha256


def HashFromSerial(serial):
    divisor = 456976
    letters = []
    for i in range(5):
        letter, serial = divmod(serial, divisor)
        letters.append( 97 + int(letter) )
        divisor /= 26
    return (letters, sha256(bytes(letters)).digest())


def main():
    h1 = bytes().fromhex("1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad")
    h2 = bytes().fromhex("3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b")
    h3 = bytes().fromhex("74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f")
    numpasswords = int(26 ** 5)
    chunksize = int(numpasswords / multiprocessing.cpu_count())
    with multiprocessing.Pool() as p:
        for (letters, digest) in p.imap_unordered(HashFromSerial, range(numpasswords), chunksize):
            if digest == h1 or digest == h2 or digest == h3:
                password = "".join(chr(x) for x in letters)
                print(password + " => " + digest.hex())


if __name__ == "__main__":
    main()
```


{{out}}

```txt
apple => 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
mmmmm => 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
zyzzx => 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
```



## Racket


Tests are included firstly to check it works, but they also provide an opportunity to time
the single threaded version.


```racket
#lang racket/base
(require racket/place
         racket/list
         racket/match
         ;; requires sha package. install it in DrRacket's "File/Install Package..."
         ;; or with raco:
         ;; % raco pkg install sha
         sha
         (only-in openssl/sha1 hex-string->bytes))

(define (brute css targs)
  (define (sub-work i) (let ((cs (list-ref css i))) (in-range (car cs) (cdr cs))))
  (define-values (as bs cs ds es) (apply values (map sub-work (range 5))))
  (define s (make-bytes 5))
  (for*/list ((a as) #:when (bytes-set! s 0 a)
                     (b bs) #:when (bytes-set! s 1 b)
                     (c cs) #:when (bytes-set! s 2 c)
                     (d ds) #:when (bytes-set! s 3 d)
                     (e es) #:when (bytes-set! s 4 e)
                     (h (in-value (sha256 s)))
                     (t (in-list targs))
                     #:when (bytes=? t h))
    (eprintf "found ~s -> ~s~%" t s)
    (cons (bytes-copy s) t)))

;; ---------------------------------------------------------------------------------------------------
(unless (place-enabled?) (error "We're using places... they're not enabled!"))

(define target-list
  (map hex-string->bytes
       (list "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"
             "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"
             "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f")))

(define (run-place/assign-task sub-task)
  (define there (place here
                       (match-define (cons work targs) (place-channel-get here))
                       (place-channel-put here (brute work targs))))
  (place-channel-put there (cons sub-task target-list))
  there)

(define (task->subtasks css n-tasks)
  (match css
    [(list (and initial-range (cons A Z+)) common-tail ...)
     (define step (quotient (+ n-tasks (- Z+ A)) n-tasks))
     (for/list ((a (in-range A Z+ step)))
       ;; replace the head with a sub-task head
       (cons (cons a (min (+ a step) Z+)) common-tail))]))

(define readable-pair (match-lambda [(cons x (app bytes->hex-string s)) (cons x s)]))

(define (parallel-brute css (n-tasks (processor-count)))
  (define the-places (map run-place/assign-task (task->subtasks css n-tasks)))
  (define collected-results (append* (map place-channel-get the-places)))
  (map readable-pair collected-results))

(define 5-char-lowercase-work
  (make-list 5 (cons (char->integer #\a) (add1 (char->integer #\z)))))

;; ---------------------------------------------------------------------------------------------------
(module+ main
  (time (parallel-brute 5-char-lowercase-work)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (check-equal?
   (bytes->hex-string (sha256 #"mmmmm"))
   "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"
   "SHA-256 works as expected")

  (check-equal?
   (hex-string->bytes "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f")
   #"t\341\273b\370\332\273\201%\245\210R\266;\337n\256\366g\313V\254\177|\333\246\3270\\P\242/"
   "This is the raw value we'll be hashing to")
  
  (define m-idx (char->integer #\m))
  (define m-idx+ (add1 m-idx))
  (check-equal?
   (brute (make-list 5 (cons m-idx m-idx+)) target-list)
   (list
    (cons
     #"mmmmm"
     #"t\341\273b\370\332\273\201%\245\210R\266;\337n\256\366g\313V\254\177|\333\246\3270\\P\242/")))

  ;; Brute works without parallelism
  ;; check when you have the time... it takes a minute (literally)
  (check-equal?
   (time
    (brute 5-char-lowercase-work target-list))
   '((#"apple"
      . #":{\323\3426\n=)\356\2446\374\373~D\3075\321\27\304-\34\0305B\vk\231B\335O\e")
     (#"mmmmm"
      .
      #"t\341\273b\370\332\273\201%\245\210R\266;\337n\256\366g\313V\254\177|\333\246\3270\\P\242/")
     (#"zyzzx"
      .
      #"\21\25\335\200\17\352\254\357\337H\37\37\220p7J*\201\342x\200\361\2079m\266yX\262\a\313\255"))
   "without parallelism, it works"))
```


{{out}}

Test phase of run:

```txt
found #"t\341\273b\370\332\273\201%\245\210R\266;\337n\256\366g\313V\254\177|\333\246\3270\\P\242/" -> #"mmmmm"
found #":{\323\3426\n=)\356\2446\374\373~D\3075\321\27\304-\34\0305B\vk\231B\335O\e" -> #"apple"
found #"t\341\273b\370\332\273\201%\245\210R\266;\337n\256\366g\313V\254\177|\333\246\3270\\P\242/" -> #"mmmmm"
found #"\21\25\335\200\17\352\254\357\337H\37\37\220p7J*\201\342x\200\361\2079m\266yX\262\a\313\255" -> #"zyzzx"
cpu time: 19593 real time: 19581 gc time: 2247

```


Main phase of run:

```txt
found #"\21\25\335\200\17\352\254\357\337H\37\37\220p7J*\201\342x\200\361\2079m\266yX\262\a\313\255" -> #"zyzzx"
found #":{\323\3426\n=)\356\2446\374\373~D\3075\321\27\304-\34\0305B\vk\231B\335O\e" -> #"apple"
found #"t\341\273b\370\332\273\201%\245\210R\266;\337n\256\366g\313V\254\177|\333\246\3270\\P\242/" -> #"mmmmm"
cpu time: 30641 real time: 4681 gc time: 0
'((#"apple" . "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b")
  (#"mmmmm" . "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f")
  (#"zyzzx" . "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"))
```



## Rust


In this solution the number of threads is the number of logical processors on the machine. `distribute_work()` distributes the work more or less equally between the threads.


```Rust
// [dependencies]
// rust-crypto = "0.2.36"
// num_cpus = "1.7.0"
// hex = "0.2.0"

extern crate crypto;
extern crate num_cpus;
extern crate hex;

use std::thread;
use std::cmp::min;
use crypto::sha2::Sha256;
use crypto::digest::Digest;
use hex::{FromHex, ToHex};

fn main() {
    let hashes = vec![
        decode("1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"),
        decode("3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"),
        decode("74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"),
    ];

    let mut threads = Vec::new();
    let mut ranges = distribute_work();

    while let Some(range) = ranges.pop() {
        let hashes = hashes.clone();
        threads.push(thread::spawn(
            move || search(range.0, range.1, hashes.clone()),
        ));
    }

    while let Some(t) = threads.pop() {
        t.join().ok();
    }
}

fn search(from: [u8; 5], to: [u8; 5], hashes: Vec<[u8; 256 / 8]>) {

    let mut password = from.clone();

    while password <= to {
        let mut sha256 = Sha256::new();
        sha256.input(&password);
        let mut result = [0u8; 256 / 8];
        sha256.result(&mut result);

        for hash in hashes.iter() {
            if *hash == result {
                println!(
                    "{}{}{}{}{} {}",
                    password[0] as char,
                    password[1] as char,
                    password[2] as char,
                    password[3] as char,
                    password[4] as char,
                    hash.to_hex()
                );
            }
        }

        password = next(&password);
    }

}

fn next(password: &[u8; 5]) -> [u8; 5] {
    let mut result = password.clone();
    for i in (0..result.len()).rev() {
        if result[i] == b'z' {
            if i == 0 {
                result[i] = b'z' + 1;
            } else {
                result[i] = b'a';
            }
        } else {
            result[i] += 1;
            break;
        }
    }
    result.clone()
}

fn distribute_work() -> Vec<([u8; 5], [u8; 5])> {
    let mut ranges = Vec::new();
    let num_cpus = min(num_cpus::get(), 26) as u8;

    let div = 25 / num_cpus;
    let mut remainder = 25 % num_cpus;
    let mut from = b'a';
    while from < b'z' {

        let to = from + div +
            if remainder > 0 {
                remainder -= 1;
                1
            } else {
                0
            };

        ranges.push((
            [from, from, from, from, from + 1].clone(),
            [to, to, to, to, to].clone(),
        ));

        from = to;
    }
    ranges[0].0[4] = b'a';

    ranges.clone()
}

fn decode(string: &str) -> [u8; 256 / 8] {
    let mut result = [0; 256 / 8];
    let vec = Vec::from_hex(string).unwrap();
    for i in 0..result.len() {
        result[i] = vec[i];
    }
    result.clone()
}
```


{{out}}


```txt

apple 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
zyzzx 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
mmmmm 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f

```



## Scala


Scala has an excellent implementation of parallel collections, which allow you to take advantage of parallel processing with only minor modifications to your code.

This example converts the collection of candidate strings into a ParVector as soon as possible, speeding up both the final step to generating the candidates and the search.


```scala
import java.security.MessageDigest

import scala.collection.parallel.immutable.ParVector

object EncryptionCracker {
  def main(args: Array[String]): Unit = {
    val hash1 = "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"
    val hash2 = "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"
    val hash3 = "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"
    
    val charSet = ('a' to 'z').toVector
    val num = 5
    
    for(tmp <- List(hash1, hash2, hash3)){
      println(tmp)
      crack(tmp, charSet, num) match{
        case Some(s) => println(s"String: $s\n")
        case None => println("Failed\n")
      }
    }
  }
  
  def crack(hash: String, charSet: Vector[Char], num: Int): Option[String] = {
    val perms = charSet
      .flatMap(c => Vector.fill(num)(c)).combinations(num)  //Generate distinct sets of letters
      .to(ParVector)                                        //Convert to ParVector
      .flatMap(_.permutations.map(_.mkString))              //Finish generating candidates
    
    perms.find(str => getHash(str).equalsIgnoreCase(hash))  //Search for a matching string
  }
  
  def getHash(str: String): String = {
    val digester = MessageDigest.getInstance("SHA-256")
    digester.digest(str.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }
}
```


An unfortunate side-effect of jumping straight into a ParVector, though, is that the entire list of candidate strings must be computed before attempting to find a match. This means that even modestly large charsets and/or strings can make the memory usage and runtime blow up.

To solve that issue, this next example implements lazy lists. A lazy list is evaluated as needed, meaning you can generate the list of candidates as you check them. In order to still take advantage of parallel processing, this code takes fixed-size chunks off the front of the LazyList and converts them to ParVectors, repeating until either a match is found or the LazyList is empty.

Notice that def is used in place of val when working with the list of candidates. This is because val holds onto the head, which means it would fill up memory over time with the backlog of candidates already checked. Using def lets the program discard candidates after they are checked.


```scala
import java.security.MessageDigest

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector

object EncryptionCracker {
  def main(args: Array[String]): Unit = {
    val hash1 = "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"
    val hash2 = "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"
    val hash3 = "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"
    
    val charSet = ('a' to 'z').toVector
    val len = 5
    val num = 1000000
    
    for(tmp <- List(hash1, hash2, hash3)){
      println(tmp)
      crackLazy(tmp, charSet, len, num) match{
        case Some(s) => println(s"String: $s\n")
        case None => println("Failed\n")
      }
    }
  }
  
  def crackLazy(hash: String, charSet: Vector[Char], len: Int, num: Int): Option[String] = {
    @tailrec
    def getMatch(lst: LazyList[String]): Option[String] = {
      def hit = lst.take(num).to(ParVector).find(str => getHash(str).equalsIgnoreCase(hash))
      def nxt = lst.drop(num)
      hit match{
        case Some(str) => Some(str)
        case None if nxt.nonEmpty => getMatch(nxt)
        case None => None
      }
    }
    
    def perms = charSet
      .flatMap(Vector.fill(len)(_))
      .combinations(len)
      .flatMap(_.permutations.map(_.mkString)).to(LazyList)
    
    getMatch(perms)
  }
  
  def getHash(str: String): String = {
    val digester = MessageDigest.getInstance("SHA-256")
    digester.digest(str.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }
}
```


As a final example, we can clean this code up with some method chaining and currying to get this:


```scala
import java.security.MessageDigest

import scala.collection.parallel.immutable.ParVector

object EncryptionCracker {
  def main(args: Array[String]): Unit = {
    val hash1 = "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad"
    val hash2 = "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b"
    val hash3 = "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f"
    
    val cracker: String => Option[String] = crackLazy('a' to 'z', 5, 1000000)
    
    for(tmp <- Seq(hash2, hash1, hash3)){
      println(s"$tmp")
      cracker(tmp) match{
        case Some(s) => println(s"String: $s\n")
        case None => println("Failed\n")
      }
    }
  }
  
  def getHash(str: String): String = MessageDigest
    .getInstance("SHA-256")
    .digest(str.getBytes("UTF-8"))
    .map("%02x".format(_)).mkString
  
  def crackLazy(charSet: Seq[Char], len: Int, num: Int)(hash: String): Option[String] = charSet
    .flatMap(Vector.fill(len)(_))                           //Duplicate characters so they can be used any number of times
    .combinations(len)                                      //Generate distinct sets of characters
    .flatMap(_.permutations.map(_.mkString))                //Generate all permutations for each character set
    .grouped(num)                                           //Partition into bite-size chunks
    .map(_.to(ParVector).find(str => getHash(str) == hash)) //Convert each chunk into a ParVector and search it
    .collectFirst{case Some(res) => res}                    //Get the first hit if one is found
}
```


{{out}}

```txt

1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad
String: zyzzx

3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
String: apple

74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
String: mmmmm

```



## zkl

The built in thread message passing object uses the OS to do the heavy
lifting and, as a result, isn't suited to high through put (ie passing
passwords to cracking threads, equally distributing passwords to each
cracking thread).  Instead, each thread gets a range of passwords to
crack and use signals to coordinate.  Way more code with the drawback
that one thread may have to do all the work.

This was run on a Intel i7 4 core 8 thread Linux box.

Uses the message hashing extension library (DLL).
{{trans|==C sharp|C#}}

```zkl
var [const] MsgHash=Import.lib("zklMsgHash");
var [const] gotEm=Atomic.Int();	// global signal for all threads

const THREADS=9,   // how we will split task, THREADS<=26
      CHR_a="a".toAsc();

fcn crack(c,n,hashes){	// thread
   sha256:=MsgHash.SHA256; // the SHA-256 hash method, byte bucket
   bytes,hash := Data(),Data().howza(0); // byte buckets to reduce garbage production
   firstLtrs:=(c+CHR_a).walker(n);
   ltrs:=CHR_a.walker;	// iterator starting at 97/"a"
   foreach a,b,c,d,e in (firstLtrs,ltrs(26),ltrs(26),ltrs(26),ltrs(26)){ 
      if(not hashes2go) return(); // all cracked, stop, not really needed
      bytes.clear(a,b,c,d,e);     // recycle Data, faster than creating Strings
      sha256(bytes,1,hash);	  // put hash in hash
      if(hashes.holds(hash)){
         println(bytes.text," --> ",hash.pump(String,"%02x".fmt));
	 hashes2go.dec();	// I cracked one, let mom thread know
      }
   }
}
```


```zkl
hashes:=T("3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b",
          "74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f",
	  "1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad");
// convert hex strings to binary; cuts down conversions during crack
fcn hex2binary(s){ s.pump(Data,Void.Read,fcn(a,b){ (a+b).toInt(16) }) }
hashes:=hashes.apply(hex2binary);

hashes2go.set(hashes.len());	// number of codes to crack
num,xtra := 26/THREADS, 26%THREADS; // try for the most even spread over threads
s:=0; do(THREADS){  // start threads
   n:=num + ((xtra-=1)>=0); 
   crack.launch(s.toInt(),n,hashes); 
   s+=n;
}
hashes2go.waitFor(0);	// wait until all cracked, just exit, OS kills threads
```


```txt

mmmmm --> 74e1bb62f8dabb8125a58852b63bdf6eaef667cb56ac7f7cdba6d7305c50a22f
apple --> 3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b
zyzzx --> 1115dd800feaacefdf481f1f9070374a2a81e27880f187396db67958b207cbad

real	0m3.261s
user	0m22.160s
sys	0m0.140s

```

