+++
title = "Railway circuit"
description = ""
date = 2018-12-05T20:07:09Z
aliases = []
[extra]
id = 19923
[taxonomies]
categories = []
tags = []
+++

{{draft task}} 
'''Railway circuit'''

Given n sections of curve tracks, each one being an arc of 30° of radius R, the goal is to build and count all possible different railway circuits.

'''Constraints''' :

* n = 12 + k*4 (k = 0, 1 , ...)
* The circuit must be a closed, connected graph, and the last arc must joint the first one
* Duplicates, either by symmetry, translation, reflexion or rotation must be eliminated.
* Paths may overlap or cross each other.
* All tracks must be used.
	

'''Illustrations''' : http://www.echolalie.org/echolisp/duplo.html
	
'''Task:'''

	Write a function which counts and displays all possible circuits Cn for n = 12, 16 , 20. Extra credit for n = 24, 28, ... 48 (no display, only counts). A circuit Cn will be displayed as a list, or sequence of n Right=1/Left=-1 turns.
	
Example:

	C12 = (1,1,1,1,1,1,1,1,1,1,1,1) or C12 = (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1)
	
'''Straight tracks (extra-extra credit)'''

Suppose we have m = k*2 sections of straight tracks, each of length L. Such a circuit is denoted Cn,m . A circuit is a sequence of +1,-1, or 0 = straight move.  Count the number of circuits Cn,m  with n same as above and m = 2 to 8 .


## EchoLisp


```scheme

;; R is turn counter in right direction
;; The nb of right turns in direction i
;; must be = to nb of right turns in direction i+6 (opposite)
(define (legal? R)
	(for ((i 6))
		#:break (!= (vector-ref R i) (vector-ref R (+ i 6))) => #f
		#t))
		

;; equal circuits by rotation ?
(define (circuit-eq? Ca Cb)
	(for [(i (vector-length Cb))]
		#:break (eqv? Ca (vector-rotate! Cb 1)) => #t
		#f))
		
;; check a result vector RV  of circuits
;; Remove equivalent circuits

(define (check-circuits RV)
	(define n (vector-length RV))
	(for ((i (1- n)))
		#:continue (null? (vector-ref RV i))
	(for ((j (in-range (1+ i) n )))
		#:continue (null? (vector-ref RV j))
		(when (circuit-eq? (vector-ref RV i) (vector-ref RV j))
			  (vector-set! RV j null)))))
		
	
;; global
;; *circuits* = result set = a vector
(define-values (*count* *calls*  *circuits*) (values 0 0 null))

;; generation of circuit C[i] i = 0 .... maxn including straight (may be 0) tracks
(define (circuits C Rct R D n maxn  straight )
(define _Rct Rct) ;; save area
(define _Rn (vector-ref R Rct))
(++ *calls* )

	(cond
    [(> *calls* 4_000_000) #f] ;; enough for maxn=24
    
    ;; hit !! legal solution
    [(and (= n maxn) ( zero? Rct ) (legal? R) (legal? D))
		(++ *count*)
		(vector-push *circuits* (vector-dup C))];; save solution
		
     ;; stop
     [( = n maxn) #f]
	
     ;; important cutter - not enough right turns
     [(and (!zero? Rct) (< (+ Rct maxn ) (+ n straight 11))) #f] 
	
     [else
		;; play right
			(vector+= R Rct 1) ; R[Rct] += 1
			(set! Rct (modulo (1+ Rct) 12))
			(vector-set! C n 1)
			(circuits C Rct R  D (1+ n) maxn straight)
			
		;;	unplay it - restore values
			(set! Rct _Rct)
			(vector-set! R Rct _Rn) 
			(vector-set! C n '-)
			
		;; play left
			(set! Rct (modulo (1- Rct) 12))
			(vector-set! C n -1)
			(circuits C Rct R D (1+ n) maxn straight)
			
		;;	unplay
			(set! Rct _Rct)
			(vector-set! R Rct _Rn) 
			(vector-set! C n '-)
			
		;; play straight line 
			(when (!zero? straight)
			(vector-set! C n 0)
			(vector+= D Rct 1)
			(circuits C Rct R D (1+ n) maxn (1- straight))
			
		;;	unplay
			(vector+= D Rct -1)
			(vector-set! C n '-)) ]))
		
;; generate maxn tracks  [ +  straight])
;; i ( 0 .. 11) * 30° are the possible directions
(define (gen (maxn 20) (straight 0))
	(define R (make-vector 12)) ;; count number of right turns in direction i
	(define D (make-vector 12)) ;; count number of straight tracks in direction i
	(define C (make-vector (+ maxn straight) '-))
	(set!-values (*count* *calls*  *circuits*) (values 0  0 (make-vector 0)))
	(vector-set! R 0 1) ;; play starter (always right)
	(vector-set! C 0 1)
	(circuits C 1 R D 1 (+ maxn straight) straight)
	(writeln 'gen-counters (cons *calls* *count*))
	
	(check-circuits *circuits*)
	(set! *circuits* (for/vector ((c *circuits*)) #:continue (null? c) c))
	(if (zero? straight)
		(printf "Number of circuits C%d : %d" maxn (vector-length *circuits*))
		(printf "Number of circuits C%d,%d : %d" maxn  straight (vector-length *circuits*)))
	(when (< (vector-length *circuits*) 20) (for-each writeln *circuits*)))

```

{{out}}

```txt

(gen 12)
gen-counters     (331 . 1)    
Number of circuits C12 : 1
#( 1 1 1 1 1 1 1 1 1 1 1 1)    

(gen 16)
gen-counters     (8175 . 6)    
Number of circuits C16 : 1
#( 1 1 1 1 1 1 -1 1 1 1 1 1 1 1 -1 1)  
  
(gen 20)
gen-counters     (150311 . 39)    
Number of circuits C20 : 6
#( 1 1 1 1 1 1 -1 1 -1 1 1 1 1 1 1 1 -1 1 -1 1)    
#( 1 1 1 1 1 1 -1 -1 1 1 1 1 1 1 1 1 -1 -1 1 1)    
#( 1 1 1 1 1 1 -1 -1 1 1 1 1 1 1 1 -1 1 1 -1 1)    
#( 1 1 1 1 1 -1 1 1 -1 1 1 1 1 1 1 -1 1 1 -1 1)    
#( 1 1 1 1 -1 1 1 1 -1 1 1 1 1 1 -1 1 1 1 -1 1)    
#( 1 1 1 -1 1 1 1 1 -1 1 1 1 1 -1 1 1 1 1 -1 1)  
  
(gen 24)
gen-counters     (2574175 . 286)    
Number of circuits C24 : 35

(gen 12 4)  
Number of circuits C12,4 : 4
#( 1 1 1 1 1 1 0 0 1 1 1 1 1 1 0 0)    
#( 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 0)    
#( 1 1 1 1 0 1 1 0 1 1 1 1 0 1 1 0)    
#( 1 1 1 0 1 1 1 0 1 1 1 0 1 1 1 0)    

```



## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

const (
    right    = 1
    left     = -1
    straight = 0
)

func normalize(tracks []int) string {
    size := len(tracks)
    a := make([]byte, size)
    for i := 0; i < size; i++ {
        a[i] = "abc"[tracks[i]+1]
    }

    /* Rotate the array and find the lexicographically lowest order
       to allow the hashmap to weed out duplicate solutions. */

    norm := string(a)
    for i := 0; i < size; i++ {
        s := string(a)
        if s < norm {
            norm = s
        }
        tmp := a[0]
        copy(a, a[1:])
        a[size-1] = tmp
    }
    return norm
}

func fullCircleStraight(tracks []int, nStraight int) bool {
    if nStraight == 0 {
        return true
    }

    // do we have the requested number of straight tracks
    count := 0
    for _, track := range tracks {
        if track == straight {
            count++
        }
    }
    if count != nStraight {
        return false
    }

    // check symmetry of straight tracks: i and i + 6, i and i + 4
    var straightTracks [12]int
    for i, idx := 0, 0; i < len(tracks) && idx >= 0; i++ {
        if tracks[i] == straight {
            straightTracks[idx%12]++
        }
        idx += tracks[i]
    }
    any1, any2 := false, false
    for i := 0; i <= 5; i++ {
        if straightTracks[i] != straightTracks[i+6] {
            any1 = true
            break
        }
    }
    for i := 0; i <= 7; i++ {
        if straightTracks[i] != straightTracks[i+4] {
            any2 = true
            break
        }
    }
    return !any1 || !any2
}

func fullCircleRight(tracks []int) bool {
    // all tracks need to add up to a multiple of 360
    sum := 0
    for _, track := range tracks {
        sum += track * 30
    }
    if sum%360 != 0 {
        return false
    }

    // check symmetry of right turns: i and i + 6, i and i + 4
    var rTurns [12]int
    for i, idx := 0, 0; i < len(tracks) && idx >= 0; i++ {
        if tracks[i] == right {
            rTurns[idx%12]++
        }
        idx += tracks[i]
    }
    any1, any2 := false, false
    for i := 0; i <= 5; i++ {
        if rTurns[i] != rTurns[i+6] {
            any1 = true
            break
        }
    }
    for i := 0; i <= 7; i++ {
        if rTurns[i] != rTurns[i+4] {
            any2 = true
            break
        }
    }
    return !any1 || !any2
}

func circuits(nCurved, nStraight int) {
    solutions := make(map[string][]int)
    gen := getPermutationsGen(nCurved, nStraight)
    for gen.hasNext() {
        tracks := gen.next()
        if !fullCircleStraight(tracks, nStraight) {
            continue
        }
        if !fullCircleRight(tracks) {
            continue
        }
        tracks2 := make([]int, len(tracks))
        copy(tracks2, tracks)
        solutions[normalize(tracks)] = tracks2
    }
    report(solutions, nCurved, nStraight)
}

func getPermutationsGen(nCurved, nStraight int) PermutationsGen {
    if (nCurved+nStraight-12)%4 != 0 {
        panic("input must be 12 + k * 4")
    }
    var trackTypes []int
    switch nStraight {
    case 0:
        trackTypes = []int{right, left}
    case 12:
        trackTypes = []int{right, straight}
    default:
        trackTypes = []int{right, left, straight}
    }
    return NewPermutationsGen(nCurved+nStraight, trackTypes)
}

func report(sol map[string][]int, numC, numS int) {
    size := len(sol)
    fmt.Printf("\n%d solution(s) for C%d,%d \n", size, numC, numS)
    if numC <= 20 {
        for _, tracks := range sol {
            for _, track := range tracks {
                fmt.Printf("%2d ", track)
            }
            fmt.Println()
        }
    }
}

// not thread safe
type PermutationsGen struct {
    NumPositions int
    choices      []int
    indices      []int
    sequence     []int
    carry        int
}

func NewPermutationsGen(numPositions int, choices []int) PermutationsGen {
    indices := make([]int, numPositions)
    sequence := make([]int, numPositions)
    carry := 0
    return PermutationsGen{numPositions, choices, indices, sequence, carry}
}

func (p *PermutationsGen) next() []int {
    p.carry = 1

    /* The generator skips the first index, so the result will always start
       with a right turn (0) and we avoid clockwise/counter-clockwise
       duplicate solutions. */
    for i := 1; i < len(p.indices) && p.carry > 0; i++ {
        p.indices[i] += p.carry
        p.carry = 0
        if p.indices[i] == len(p.choices) {
            p.carry = 1
            p.indices[i] = 0
        }
    }
    for j := 0; j < len(p.indices); j++ {
        p.sequence[j] = p.choices[p.indices[j]]
    }
    return p.sequence
}

func (p *PermutationsGen) hasNext() bool {
    return p.carry != 1
}

func main() {
    for n := 12; n <= 28; n += 4 {
        circuits(n, 0)
    }
    circuits(12, 4)
}
```


{{out}}

```txt

1 solution(s) for C12,0 
 1  1  1  1  1  1  1  1  1  1  1  1 

1 solution(s) for C16,0 
 1  1  1  1  1  1  1 -1  1  1  1  1  1  1  1 -1 

6 solution(s) for C20,0 
 1  1  1  1  1  1  1 -1  1 -1  1  1  1  1  1  1  1 -1  1 -1 
 1  1  1  1  1  1  1  1 -1 -1  1  1  1  1  1  1  1  1 -1 -1 
 1  1  1  1  1  1  1 -1  1  1 -1  1  1  1  1  1  1  1 -1 -1 
 1  1  1  1  1  1 -1  1  1 -1  1  1  1  1  1  1 -1  1  1 -1 
 1  1  1  1  1 -1  1  1  1 -1  1  1  1  1  1 -1  1  1  1 -1 
 1  1  1  1 -1  1  1  1  1 -1  1  1  1  1 -1  1  1  1  1 -1 

40 solution(s) for C24,0 

243 solution(s) for C28,0 

2134 solution(s) for C32,0 

4 solution(s) for C12,4 
 1  1  1  1  1  1  0  0  1  1  1  1  1  1  0  0 
 1  1  1  1  1  0  1  0  1  1  1  1  1  0  1  0 
 1  1  1  1  0  1  1  0  1  1  1  1  0  1  1  0 
 1  1  1  0  1  1  1  0  1  1  1  0  1  1  1  0 

```



## Java

{{works with|Java|8}}

```java
package railwaycircuit;

import static java.util.Arrays.stream;
import java.util.*;
import static java.util.stream.IntStream.range;

public class RailwayCircuit {
    final static int RIGHT = 1, LEFT = -1, STRAIGHT = 0;

    static String normalize(int[] tracks) {
        char[] a = new char[tracks.length];
        for (int i = 0; i < a.length; i++)
            a[i] = "abc".charAt(tracks[i] + 1);

        /* Rotate the array and find the lexicographically lowest order
        to allow the hashmap to weed out duplicate solutions. */
        String norm = new String(a);
        for (int i = 0, len = a.length; i < len; i++) {

            String s = new String(a);
            if (s.compareTo(norm) < 0)
                norm = s;

            char tmp = a[0];
            for (int j = 1; j < a.length; j++)
                a[j - 1] = a[j];
            a[len - 1] = tmp;
        }
        return norm;
    }

    static boolean fullCircleStraight(int[] tracks, int nStraight) {
        if (nStraight == 0)
            return true;

        // do we have the requested number of straight tracks
        if (stream(tracks).filter(i -> i == STRAIGHT).count() != nStraight)
            return false;

        // check symmetry of straight tracks: i and i + 6, i and i + 4
        int[] straight = new int[12];
        for (int i = 0, idx = 0; i < tracks.length && idx >= 0; i++) {
            if (tracks[i] == STRAIGHT)
                straight[idx % 12]++;
            idx += tracks[i];
        }

        return !(range(0, 6).anyMatch(i -> straight[i] != straight[i + 6])
                && range(0, 8).anyMatch(i -> straight[i] != straight[i + 4]));
    }

    static boolean fullCircleRight(int[] tracks) {

        // all tracks need to add up to a multiple of 360
        if (stream(tracks).map(i -> i * 30).sum() % 360 != 0)
            return false;

        // check symmetry of right turns: i and i + 6, i and i + 4
        int[] rTurns = new int[12];
        for (int i = 0, idx = 0; i < tracks.length && idx >= 0; i++) {
            if (tracks[i] == RIGHT)
                rTurns[idx % 12]++;
            idx += tracks[i];
        }

        return !(range(0, 6).anyMatch(i -> rTurns[i] != rTurns[i + 6])
                && range(0, 8).anyMatch(i -> rTurns[i] != rTurns[i + 4]));
    }

    static void circuits(int nCurved, int nStraight) {
        Map<String, int[]> solutions = new HashMap<>();

        PermutationsGen gen = getPermutationsGen(nCurved, nStraight);
        while (gen.hasNext()) {

            int[] tracks = gen.next();

            if (!fullCircleStraight(tracks, nStraight))
                continue;

            if (!fullCircleRight(tracks))
                continue;

            solutions.put(normalize(tracks), tracks.clone());
        }
        report(solutions, nCurved, nStraight);
    }

    static PermutationsGen getPermutationsGen(int nCurved, int nStraight) {
        assert (nCurved + nStraight - 12) % 4 == 0 : "input must be 12 + k * 4";

        int[] trackTypes = new int[]{RIGHT, LEFT};

        if (nStraight != 0) {
            if (nCurved == 12)
                trackTypes = new int[]{RIGHT, STRAIGHT};
            else
                trackTypes = new int[]{RIGHT, LEFT, STRAIGHT};
        }

        return new PermutationsGen(nCurved + nStraight, trackTypes);
    }

    static void report(Map<String, int[]> sol, int numC, int numS) {

        int size = sol.size();
        System.out.printf("%n%d solution(s) for C%d,%d %n", size, numC, numS);

        if (size < 10)
            sol.values().stream().forEach(tracks -> {
                stream(tracks).forEach(i -> System.out.printf("%2d ", i));
                System.out.println();
            });
    }

    public static void main(String[] args) {
        circuits(12, 0);
        circuits(16, 0);
        circuits(20, 0);
        circuits(24, 0);
        circuits(12, 4);
    }
}

class PermutationsGen {
    // not thread safe
    private int[] indices;
    private int[] choices;
    private int[] sequence;
    private int carry;

    PermutationsGen(int numPositions, int[] choices) {
        indices = new int[numPositions];
        sequence = new int[numPositions];
        this.choices = choices;
    }

    int[] next() {
        carry = 1;
        /* The generator skips the first index, so the result will always start
        with a right turn (0) and we avoid clockwise/counter-clockwise
        duplicate solutions. */
        for (int i = 1; i < indices.length && carry > 0; i++) {
            indices[i] += carry;
            carry = 0;

            if (indices[i] == choices.length) {
                carry = 1;
                indices[i] = 0;
            }
        }

        for (int i = 0; i < indices.length; i++)
            sequence[i] = choices[indices[i]];

        return sequence;
    }

    boolean hasNext() {
        return carry != 1;
    }
}
```


```txt
1 solution(s) for C12,0 
 1  1  1  1  1  1  1  1  1  1  1  1 

1 solution(s) for C16,0 
 1  1  1  1  1  1  1 -1  1  1  1  1  1  1  1 -1 

6 solution(s) for C20,0 
 1  1  1  1  1  1 -1  1  1 -1  1  1  1  1  1  1 -1  1  1 -1 
 1  1  1  1  1  1  1 -1  1 -1  1  1  1  1  1  1  1 -1  1 -1 
 1  1  1  1  1  1  1  1 -1 -1  1  1  1  1  1  1  1  1 -1 -1 
 1  1  1  1  1  1  1 -1  1  1 -1  1  1  1  1  1  1  1 -1 -1 
 1  1  1  1  1 -1  1  1  1 -1  1  1  1  1  1 -1  1  1  1 -1 
 1  1  1  1 -1  1  1  1  1 -1  1  1  1  1 -1  1  1  1  1 -1 

40 solution(s) for C24,0 
(35 solutions listed on talk page, plus 5)
1 1 1 -1 -1 1 1 1 1 1 -1 1 1 -1 1 1 1 1 1 -1 -1 1 1 1
1 1 -1 1 1 -1 1 1 1 1 1 -1 -1 1 1 1 1 1 -1 1 1 -1 1 1
1 1 -1 1 1 -1 1 1 1 1 -1 1 1 -1 1 1 1 1 -1 1 1 -1 1 1
1 1 1 -1 -1 1 1 1 1 1 1 -1 -1 1 1 1 1 1 1 -1 -1 1 1 1
1 1 -1 1 -1 1 1 1 1 1 -1 1 -1 1 1 1 1 1 -1 1 -1 1 1 1

4 solution(s) for C12,4 
 1  1  1  1  1  0  1  0  1  1  1  1  1  0  1  0 
 1  1  1  0  1  1  1  0  1  1  1  0  1  1  1  0 
 1  1  1  1  1  1  0  0  1  1  1  1  1  1  0  0 
 1  1  1  1  0  1  1  0  1  1  1  1  0  1  1  0 

```



## Kotlin

{{trans|Java}}
It takes several minutes to get up to n = 32. I called it a day after that!

```scala
// Version 1.2.31

const val RIGHT = 1
const val LEFT = -1
const val STRAIGHT = 0

fun normalize(tracks: IntArray): String {
    val size = tracks.size
    val a = CharArray(size) { "abc"[tracks[it] + 1] }

    /* Rotate the array and find the lexicographically lowest order
       to allow the hashmap to weed out duplicate solutions. */

    var norm = String(a)
    repeat(size) {
        val s = String(a)
        if (s < norm) norm = s
        val tmp = a[0]
        for (j in 1 until size) a[j - 1] = a[j]
        a[size - 1] = tmp
    }
    return norm
}

fun fullCircleStraight(tracks: IntArray, nStraight: Int): Boolean {
    if (nStraight == 0) return true

    // do we have the requested number of straight tracks
    if (tracks.filter { it == STRAIGHT }.count() != nStraight) return false

    // check symmetry of straight tracks: i and i + 6, i and i + 4
    val straight = IntArray(12)
    var i = 0
    var idx = 0
    while (i < tracks.size && idx >= 0) {
        if (tracks[i] == STRAIGHT) straight[idx % 12]++
        idx += tracks[i]
        i++
    }
    return !((0..5).any { straight[it] != straight[it + 6] } &&
             (0..7).any { straight[it] != straight[it + 4] })
}

fun fullCircleRight(tracks: IntArray): Boolean {
    // all tracks need to add up to a multiple of 360
    if (tracks.map { it * 30 }.sum() % 360 != 0) return false

    // check symmetry of right turns: i and i + 6, i and i + 4
    val rTurns = IntArray(12)
    var i = 0
    var idx = 0
    while (i < tracks.size && idx >= 0) {
        if (tracks[i] == RIGHT) rTurns[idx % 12]++
        idx += tracks[i]
        i++
    }
    return !((0..5).any { rTurns[it] != rTurns[it + 6] } &&
             (0..7).any { rTurns[it] != rTurns[it + 4] })
}

fun circuits(nCurved: Int, nStraight: Int) {
    val solutions = hashMapOf<String, IntArray>()
    val gen = getPermutationsGen(nCurved, nStraight)
    while (gen.hasNext()) {
        val tracks = gen.next()
        if (!fullCircleStraight(tracks, nStraight)) continue
        if (!fullCircleRight(tracks)) continue
        solutions.put(normalize(tracks), tracks.copyOf())
    }
    report(solutions, nCurved, nStraight)
}

fun getPermutationsGen(nCurved: Int, nStraight: Int): PermutationsGen {
    require((nCurved + nStraight - 12) % 4 == 0) { "input must be 12 + k * 4" }
    val trackTypes =
        if (nStraight  == 0)
            intArrayOf(RIGHT, LEFT)
        else if (nCurved == 12)
            intArrayOf(RIGHT, STRAIGHT)
        else
            intArrayOf(RIGHT, LEFT, STRAIGHT)
    return PermutationsGen(nCurved + nStraight, trackTypes)
}

fun report(sol: Map<String, IntArray>, numC: Int, numS: Int) {
    val size = sol.size
    System.out.printf("%n%d solution(s) for C%d,%d %n", size, numC, numS)
    if (numC <= 20) {
        sol.values.forEach { tracks ->
            tracks.forEach { print("%2d ".format(it)) }
            println()
        }
    }
}

class PermutationsGen(numPositions: Int, private val choices: IntArray) {
    // not thread safe
    private val indices = IntArray(numPositions)
    private val sequence = IntArray(numPositions)
    private var carry = 0

    fun next(): IntArray {
        carry = 1

        /* The generator skips the first index, so the result will always start
           with a right turn (0) and we avoid clockwise/counter-clockwise
           duplicate solutions. */
        var i = 1
        while (i < indices.size && carry > 0) {
            indices[i] += carry
            carry = 0
            if (indices[i] == choices.size) {
                carry = 1
                indices[i] = 0
            }
            i++
        }
        for (j in 0 until indices.size) sequence[j] = choices[indices[j]]
        return sequence
    }

    fun hasNext() = carry != 1
}

fun main(args: Array<String>) {
    for (n in 12..32 step 4) circuits(n, 0)
    circuits(12, 4)
}
```


{{out}}

```txt

1 solution(s) for C12,0
 1  1  1  1  1  1  1  1  1  1  1  1

1 solution(s) for C16,0
 1  1  1  1  1  1  1 -1  1  1  1  1  1  1  1 -1

6 solution(s) for C20,0
 1  1  1  1  1  1 -1  1  1 -1  1  1  1  1  1  1 -1  1  1 -1
 1  1  1  1  1  1  1 -1  1 -1  1  1  1  1  1  1  1 -1  1 -1
 1  1  1  1  1  1  1  1 -1 -1  1  1  1  1  1  1  1  1 -1 -1
 1  1  1  1  1  1  1 -1  1  1 -1  1  1  1  1  1  1  1 -1 -1
 1  1  1  1  1 -1  1  1  1 -1  1  1  1  1  1 -1  1  1  1 -1
 1  1  1  1 -1  1  1  1  1 -1  1  1  1  1 -1  1  1  1  1 -1

40 solution(s) for C24,0

243 solution(s) for C28,0

2134 solution(s) for C32,0

4 solution(s) for C12,4
 1  1  1  1  1  0  1  0  1  1  1  1  1  0  1  0
 1  1  1  0  1  1  1  0  1  1  1  0  1  1  1  0
 1  1  1  1  1  1  0  0  1  1  1  1  1  1  0  0
 1  1  1  1  0  1  1  0  1  1  1  1  0  1  1  0

```



## Phix

{{trans|Go}}

```Phix
constant right     =  1,
         left     = -1,
         straight =  0

function normalize(sequence tracks)
    integer size := length(tracks)
    string a := repeat('?',size)
    for i=1 to size do
        a[i] = "abc"[tracks[i]+2]
    end for
 
    /* Rotate the array and find the lexicographically lowest order
       to allow the hashmap to weed out duplicate solutions. */
 
    string norm = a
    for i=1 to size do
        if a < norm then
            norm = a
        end if
        a = a[2..$]&a[1]
    end for
    return norm
end function
 
function fullCircleStraight(sequence tracks, integer nStraight)
    if nStraight == 0  then
        return true
    end if
 
    -- do we have the requested number of straight tracks
    integer count := 0
    for i=1 to length(tracks) do
        if tracks[i] == straight then
            count += 1
        end if
    end for
    if count != nStraight then
        return false
    end if
 
    -- check symmetry of straight tracks: i and i + 6, i and i + 4
    sequence straightTracks =repeat(0,12)
    integer idx = 0
    for i=1 to length(tracks) do
        if tracks[i] == straight then
            straightTracks[mod(idx,12)+1] += 1
        end if
        idx += tracks[i]
        if idx<0 then exit end if
    end for
    bool any1 = false, any2 = false
    for i=1 to 6 do
        if straightTracks[i] != straightTracks[i+6] then
            any1 = true
            exit
        end if
    end for
    for i=1 to 8 do
        if straightTracks[i] != straightTracks[i+4] then
            any2 = true
            exit
        end if
    end for
    return not any1 or not any2
end function
 
function fullCircleRight(sequence tracks)
    -- all tracks need to add up to a multiple of 360
    integer tot := 0
    for i=1 to length(tracks) do
        tot += tracks[i] * 30
    end for
    if mod(tot,360)!=0 then
        return false
    end if
 
    -- check symmetry of right turns: i and i + 6, i and i + 4
    sequence rTurns = repeat(0,12)
    integer idx = 0
    for i=1 to length(tracks) do
        if tracks[i] == right then
            rTurns[mod(idx,12)+1] += 1
        end if
        idx += tracks[i]
        if idx<0 then exit end if
    end for
    bool any1 = false, any2 = false
    for i=1 to 6 do
        if rTurns[i] != rTurns[i+6] then
            any1 = true
            exit
        end if
    end for
    for i=1 to 8 do
        if rTurns[i] != rTurns[i+4] then
            any2 = true
            exit
        end if
    end for
    return not any1 or not any2
end function
 
procedure report(sequence sol, integer numC, numS)
    integer size := length(sol)
    string s = iff(size=1?"":"s")
    printf(1,"\n%d solution%s for C%d,%d \n", {size, s, numC, numS})
    if numC <= 20 then
        pp(sol)
    end if
end procedure
 
enum NumPositions, choices, indices, sequnce, carry
 
function next(sequence p)
    p[carry] = 1
 
    /* The generator skips the first index, so the result will always start
       with a right turn (0) and we avoid clockwise/counter-clockwise
       duplicate solutions. */
    for i=2 to length(p[indices]) do
        p[indices][i] += p[carry]
        p[carry] = 0
        if p[indices][i] != length(p[choices]) then exit end if
        p[carry] = 1
        p[indices][i] = 0
    end for
    for j=1 to length(p[indices]) do
        p[sequnce][j] = p[choices][p[indices][j]+1]
    end for
    return p
end function
 
function hasNext(sequence p)
    return p[carry] != 1
end function

function NewPermutationsGen(integer numPositions, sequence choices)
    sequence indices := repeat(0, numPositions),
             sequnce := repeat(0, numPositions)
    integer carry := 0
    return {numPositions, choices, indices, sequnce, carry}
end function

function getPermutationsGen(integer nCurved, nStraight)
    if mod(nCurved+nStraight-12,4)!=0 then
        crash("input must be 12 + k * 4")
    end if
    sequence trackTypes
    switch nStraight do
        case 0:  trackTypes = {right, left}
        case 12: trackTypes = {right, straight}
        default: trackTypes = {right, left, straight}
    end switch
    return NewPermutationsGen(nCurved+nStraight, trackTypes)
end function

procedure circuits(integer nCurved, nStraight)
    integer norms = new_dict()
    sequence solutions = {},
             gen := getPermutationsGen(nCurved, nStraight)
    while hasNext(gen) do
        gen = next(gen)
        sequence tracks := gen[sequnce]
        if fullCircleStraight(tracks, nStraight)
        and fullCircleRight(tracks) then
            string norm = normalize(tracks)
            if getd_index(norm,norms)=0 then
                setd(norm,true,norms)   -- (data (=true) is ignored)
                solutions = append(solutions,tracks)
            end if
        end if
    end while
    destroy_dict(norms)
    report(solutions, nCurved, nStraight)
end procedure

for n=12 to 28 by 4 do
    circuits(n, 0)
end for
circuits(12, 4)
```

{{out}}

```txt

1 solution for C12,0
{{1,1,1,1,1,1,1,1,1,1,1,1}}

1 solution for C16,0
{{1,-1,1,1,1,1,1,1,1,-1,1,1,1,1,1,1}}

6 solutions for C20,0
{{1,-1,1,-1,1,1,1,1,1,1,1,-1,1,-1,1,1,1,1,1,1},
 {1,1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,1,1,1,1,1,1},
 {1,-1,1,1,-1,1,1,1,1,1,1,1,-1,-1,1,1,1,1,1,1},
 {1,-1,1,1,-1,1,1,1,1,1,1,-1,1,1,-1,1,1,1,1,1},
 {1,-1,1,1,1,-1,1,1,1,1,1,-1,1,1,1,-1,1,1,1,1},
 {1,-1,1,1,1,1,-1,1,1,1,1,-1,1,1,1,1,-1,1,1,1}}

40 solutions for C24,0

243 solutions for C28,0

4 solutions for C12,4
{{1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1},
 {1,0,1,0,1,1,1,1,1,0,1,0,1,1,1,1},
 {1,0,1,1,0,1,1,1,1,0,1,1,0,1,1,1},
 {1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1}}

```



## Racket


{{trans|EchoLisp}}
Made functional, so builds the track up with lists. A bit more expense spent copying vectors, but this solution avoids mutation (except internally in <code>vector+=</code> . Also got rid of the maximum workload counter.


```racket
#lang racket

(define-syntax-rule (vector+= v idx i)
  (let ((v′ (vector-copy v))) (vector-set! v′ idx (+ (vector-ref v idx) i)) v′))

;; The nb of right turns in direction i
;; must be = to nb of right turns in direction i+6 (opposite)
(define legal? (match-lambda [(vector a b c d e f a b c d e f) #t] [_ #f]))

;; equal circuits by rotation ?
(define (circuit-eq? Ca Cb)
  (define different? (for/fold ((Cb Cb)) ((i (length Cb))
                                          #:break (not Cb))
                       (and (not (equal? Ca Cb)) (append (cdr Cb) (list (car Cb))))))
  (not different?))

;; generation of circuit C[i] i = 0 .... maxn including straight (may be 0) tracks
(define (walk-circuits C_0 Rct_0 R_0 D_0 maxn straight_0)
  (define (inr C Rct R D n strt)
    (cond
      ;; hit !! legal solution
      [(and (= n maxn) (zero? Rct) (legal? R) (legal? D)) (values (list C) 1)] ; save solution
      
      [(= n maxn) (values null 0)] ; stop - no more track
      
      ;; important cutter - not enough right turns
      [(and (not (zero? Rct)) (< (+ Rct maxn) (+ n strt 11))) (values null 0)] 
      
      [else
       (define n+ (add1 n))
       (define (clock x) (modulo x 12))
       ;; play right
       (define-values [Cs-r n-r] (inr (cons 1 C) (clock (add1 Rct)) (vector+= R Rct 1) D n+ strt))
       ;; play left
       (define-values [Cs-l n-l] (inr (cons -1 C) (clock (sub1 Rct)) (vector+= R Rct -1) D n+ strt))
       ;; play straight line (if available)
       (define-values [Cs-s n-s]
         (if (zero? strt)
             (values null 0)
             (inr (cons 0 C) Rct R (vector+= D Rct 1) n+ (sub1 strt))))
       
       (values (append Cs-r Cs-l Cs-s) (+ n-r n-l n-s))])) ; gather them together
  (inr C_0 Rct_0 R_0 D_0 1 straight_0))

;; generate maxn tracks  [ +  straight])
;; i ( 0 .. 11) * 30° are the possible directions
(define (gen (maxn 20) (straight 0))
  (define R (make-vector 12 0)) ; count number of right turns in direction i
  (vector-set! R 0 1); play starter (always right) into R
  (define D (make-vector 12 0)) ; count number of straight tracks in direction i
  (define-values (circuits count)
    (walk-circuits '(1) #| play starter (always right) |# 1 R D (+ maxn straight) straight))

  (define unique-circuits (remove-duplicates circuits circuit-eq?))
  (printf "gen-counters ~a~%" count)

  (if (zero? straight)
      (printf "Number of circuits C~a : ~a~%" maxn (length unique-circuits))
      (printf "Number of circuits C~a,~a : ~a~%" maxn straight (length unique-circuits)))
  (when (< (length unique-circuits) 20) (for ((c unique-circuits)) (writeln c)))
  (newline))

(module+ test
  (require rackunit)
  (check-true (circuit-eq? '(1 2 3) '(1 2 3)))
  (check-true (circuit-eq? '(1 2 3) '(2 3 1)))
  (gen 12)
  (gen 16)
  (gen 20)
  (gen 24)
  (gen 12 4))
```


{{out}}


```txt
gen-counters 1
Number of circuits C12 : 1
(1 1 1 1 1 1 1 1 1 1 1 1)

gen-counters 6
Number of circuits C16 : 1
(1 -1 1 1 1 1 1 1 1 -1 1 1 1 1 1 1)

gen-counters 39
Number of circuits C20 : 6
(1 -1 1 -1 1 1 1 1 1 1 1 -1 1 -1 1 1 1 1 1 1)
(1 1 -1 -1 1 1 1 1 1 1 1 1 -1 -1 1 1 1 1 1 1)
(1 -1 1 1 -1 1 1 1 1 1 1 1 -1 -1 1 1 1 1 1 1)
(1 -1 1 1 -1 1 1 1 1 1 1 -1 1 1 -1 1 1 1 1 1)
(1 -1 1 1 1 -1 1 1 1 1 1 -1 1 1 1 -1 1 1 1 1)
(1 -1 1 1 1 1 -1 1 1 1 1 -1 1 1 1 1 -1 1 1 1)

gen-counters 286
Number of circuits C24 : 35

gen-counters 21
Number of circuits C12,4 : 4
(0 0 1 1 1 1 1 1 0 0 1 1 1 1 1 1)
(0 1 0 1 1 1 1 1 0 1 0 1 1 1 1 1)
(0 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1)
(0 1 1 1 0 1 1 1 0 1 1 1 0 1 1 1)
```



## zkl

{{trans|EchoLisp}}

```zkl
    // R is turn counter in right direction
    // The nb of right turns in direction i
    // must be = to nb of right turns in direction i+6 (opposite)
fcn legal(R){
   foreach i in (6){ if(R[i]!=R[i+6]) return(False) }
   True
}
    // equal circuits by rotation ?
fcn circuit_eq(Ca,Cb){
   foreach i in (Cb.len()){ if(Ca==Cb.append(Cb.pop(0))) return(True) }
   False
}
    // check a result vector RV of circuits
    // Remove equivalent circuits
fcn check_circuits(RV){  // modifies RV
   n:=RV.len();
   foreach i in (n - 1){
      if(not RV[i]) continue;
      foreach j in ([i+1..n-1]){
         if(not RV[j]) continue;
         if(circuit_eq(RV[i],RV[j])) RV[j]=Void;
      }
   }
   RV
}
 
    // global variables
    // *circuits* = result set = a vector
var _count, _calls, _circuits;
 
   // generation of circuit C[i] i = 0 .... maxn including straight (may be 0) tracks
fcn circuits([List]C,[Int]Rct,[List]R,[List]D,n,maxn, straight){
   _Rct,_Rn:=Rct,R[Rct];	// save area
   _calls+=1;

   if(_calls>0d4_000_000) False;	// enough for maxn=24
   else if(n==maxn and 0==Rct and legal(R) and legal(D)){ // hit legal solution
       _count+=1;
       _circuits.append(C.copy());	// save solution
   }else if(n==maxn) False;	// stop
	// important cutter - not enough right turns
   else if(Rct and ((Rct + maxn) < (n + straight + 11))) False
   else{
      // play right
      R[Rct]+=1;   Rct=(Rct+1)%12;   C[n]=1;
      circuits(C,Rct,R,D,n+1, maxn, straight);

      Rct=_Rct;   R[Rct]=_Rn;   C[n]=Void;   // unplay it - restore values
 
      // play left
      Rct=(Rct - 1 + 12)%12;   C[n]=-1;   // -1%12 --> 11 in EchoLisp
      circuits(C,Rct,R,D,n+1,maxn,straight);
 
      Rct=_Rct;   R[Rct]=_Rn;   C[n]=Void;      // unplay
 
      if(straight){      // play straight line 
	 C[n]=0;   D[Rct]+=1;
	 circuits(C,Rct,R,D,n+1,maxn,straight-1);
	 D[Rct]+=-1;   C[n]=Void;    // unplay
      }
   }
}
 
    // (generate max-tracks  [ + max-straight])
fcn gen(maxn=20,straight=0){
   R,D:=(12).pump(List(),0), R.copy();  // vectors of zero
   C:=(maxn + straight).pump(List(),Void.noop);	// vector of Void
   _count,_calls,_circuits = 0,0,List();
   R[0]=C[0]=1;				// play starter (always right)
   circuits(C,1,R,D,1,maxn + straight,straight);
   println("gen-counters %,d . %d".fmt(_calls,_count));

   _circuits=check_circuits(_circuits).filter();
   if(0==straight)
        println("Number of circuits C%,d : %d".fmt(maxn,_circuits.len()));
   else println("Number of circuits C%,d,%d : %d".fmt(maxn,straight,_circuits.len()));
   if(_circuits.len()<20) _circuits.apply2(T(T("toString",*),"println"));
}
```


```zkl
gen(12); println();
gen(16); println();
gen(20); println();
gen(24); println();
gen(12,4);
```

{{out}}

```txt

gen-counters 331 . 1
Number of circuits C12 : 1
L(1,1,1,1,1,1,1,1,1,1,1,1)

gen-counters 8,175 . 6
Number of circuits C16 : 1
L(1,1,1,1,1,1,-1,1,1,1,1,1,1,1,-1,1)

gen-counters 150,311 . 39
Number of circuits C20 : 6
L(1,1,1,1,1,1,-1,1,-1,1,1,1,1,1,1,1,-1,1,-1,1)
L(1,1,1,1,1,1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,1,1)
L(1,1,1,1,1,1,-1,-1,1,1,1,1,1,1,1,-1,1,1,-1,1)
L(1,1,1,1,1,-1,1,1,-1,1,1,1,1,1,1,-1,1,1,-1,1)
L(1,1,1,1,-1,1,1,1,-1,1,1,1,1,1,-1,1,1,1,-1,1)
L(1,1,1,-1,1,1,1,1,-1,1,1,1,1,-1,1,1,1,1,-1,1)

gen-counters 2,574,175 . 286
Number of circuits C24 : 35

gen-counters 375,211 . 21
Number of circuits C12,4 : 4
L(1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0)
L(1,1,1,1,1,0,1,0,1,1,1,1,1,0,1,0)
L(1,1,1,1,0,1,1,0,1,1,1,1,0,1,1,0)
L(1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0)

```

