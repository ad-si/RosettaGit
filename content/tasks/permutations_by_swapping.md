+++
title = "Permutations by swapping"
description = ""
date = 2018-07-29T15:52:18Z
aliases = []
[extra]
id = 12099
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "d",
  "echolisp",
  "elixir",
  "forth",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "nim",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "tcl",
  "xpl0",
  "zkl",
]
+++

## Task

Generate permutations of n items in which successive permutations differ from each other by the swapping of any two items. 

Also generate the sign of the permutation which is +1 when the permutation is generated from an even number of swaps from the initial state, and -1 for odd. 

Show the permutations and signs of three items, in order of generation ''here''.

Such data are of use in generating the [[Matrix arithmetic|determinant]] of a square matrix and any functions created should bear this in mind.

Note: The Steinhaus–Johnson–Trotter algorithm generates successive permutations where ''adjacent'' items are swapped, but from [[wp:Parity_of_a_permutation#Example|this]] discussion adjacency is not a requirement.


## References

* [[wp:Steinhaus–Johnson–Trotter algorithm|Steinhaus–Johnson–Trotter algorithm]]
* [http://www.cut-the-knot.org/Curriculum/Combinatorics/JohnsonTrotter.shtml Johnson-Trotter Algorithm Listing All Permutations]
* [http://stackoverflow.com/a/29044942/10562 Correction to] Heap's algorithm as presented in Wikipedia and widely distributed.
* [http://www.gutenberg.org/files/18567/18567-h/18567-h.htm#ch7] Tintinnalogia


## Related tasks

*   [[Matrix arithmetic]]
*   [[Gray code]]





## AutoHotkey


```AutoHotkey
Permutations_By_Swapping(str, list:=""){
	ch := SubStr(str, 1, 1)								; get left-most charachter of str
	for i, line in StrSplit(list, "`n")					; for each line in list
		loop % StrLen(line) + 1							; loop each possible position
			Newlist .= RegExReplace(line, mod(i,2) ? "(?=.{" A_Index-1 "}$)" : "^.{" A_Index-1 "}\K", ch) "`n"
	list := Newlist ? Trim(Newlist, "`n") : ch			; recreate list
	if !str := SubStr(str, 2)							; remove charachter from left hand side
		return list										; done if str is empty
	return Permutations_By_Swapping(str, list)			; else recurse
}
```

Examples:
```AutoHotkey
for each, line in StrSplit(Permutations_By_Swapping(1234), "`n")
	result .= line "`tSign: " (mod(A_Index,2)? 1 : -1) "`n"
MsgBox, 262144, , % result
return
```

Outputs:
```txt
1234	Sign: 1
1243	Sign: -1
1423	Sign: 1
4123	Sign: -1
4132	Sign: 1
1432	Sign: -1
1342	Sign: 1
1324	Sign: -1
3124	Sign: 1
3142	Sign: -1
3412	Sign: 1
4312	Sign: -1
4321	Sign: 1
3421	Sign: -1
3241	Sign: 1
3214	Sign: -1
2314	Sign: 1
2341	Sign: -1
2431	Sign: 1
4231	Sign: -1
4213	Sign: 1
2413	Sign: -1
2143	Sign: 1
2134	Sign: -1
```



## BBC BASIC

```bbcbasic
      PROCperms(3)
      PRINT
      PROCperms(4)
      END
      
      DEF PROCperms(n%)
      LOCAL p%(), i%, k%, s%
      DIM p%(n%)
      FOR i% = 1 TO n%
        p%(i%) = -i%
      NEXT
      s% = 1
      REPEAT
        PRINT "Perm: [ ";
        FOR i% = 1 TO n%
          PRINT ;ABSp%(i%) " ";
        NEXT
        PRINT "] Sign: ";s%
        k% = 0
        FOR i% = 2 TO n%
          IF p%(i%)<0 IF ABSp%(i%)>ABSp%(i%-1) IF ABSp%(i%)>ABSp%(k%) k% = i%
        NEXT
        FOR i% = 1 TO n%-1
          IF p%(i%)>0 IF ABSp%(i%)>ABSp%(i%+1) IF ABSp%(i%)>ABSp%(k%) k% = i%
        NEXT
        IF k% THEN
          FOR i% = 1 TO n%
            IF ABSp%(i%)>ABSp%(k%) p%(i%) *= -1
          NEXT
          i% = k%+SGNp%(k%)
          SWAP p%(k%),p%(i%)
          s% = -s%
        ENDIF
      UNTIL k% = 0
      ENDPROC
```

```txt

Perm: [ 1 2 3 ] Sign: 1
Perm: [ 1 3 2 ] Sign: -1
Perm: [ 3 1 2 ] Sign: 1
Perm: [ 3 2 1 ] Sign: -1
Perm: [ 2 3 1 ] Sign: 1
Perm: [ 2 1 3 ] Sign: -1

Perm: [ 1 2 3 4 ] Sign: 1
Perm: [ 1 2 4 3 ] Sign: -1
Perm: [ 1 4 2 3 ] Sign: 1
Perm: [ 4 1 2 3 ] Sign: -1
Perm: [ 4 1 3 2 ] Sign: 1
Perm: [ 1 4 3 2 ] Sign: -1
Perm: [ 1 3 4 2 ] Sign: 1
Perm: [ 1 3 2 4 ] Sign: -1
Perm: [ 3 1 2 4 ] Sign: 1
Perm: [ 3 1 4 2 ] Sign: -1
Perm: [ 3 4 1 2 ] Sign: 1
Perm: [ 4 3 1 2 ] Sign: -1
Perm: [ 4 3 2 1 ] Sign: 1
Perm: [ 3 4 2 1 ] Sign: -1
Perm: [ 3 2 4 1 ] Sign: 1
Perm: [ 3 2 1 4 ] Sign: -1
Perm: [ 2 3 1 4 ] Sign: 1
Perm: [ 2 3 4 1 ] Sign: -1
Perm: [ 2 4 3 1 ] Sign: 1
Perm: [ 4 2 3 1 ] Sign: -1
Perm: [ 4 2 1 3 ] Sign: 1
Perm: [ 2 4 1 3 ] Sign: -1
Perm: [ 2 1 4 3 ] Sign: 1
Perm: [ 2 1 3 4 ] Sign: -1

```



## C

Implementation of Heap's Algorithm, array length has to be passed as a parameter for non character arrays, as sizeof() will not give correct results when malloc is used. Prints usage on incorrect invocation.

```C

#include<stdlib.h>
#include<string.h>
#include<stdio.h>

int flag = 1;

void heapPermute(int n, int arr[],int arrLen){
	int temp;
	int i;
	
	if(n==1){
		printf("\n[");
		
		for(i=0;i<arrLen;i++)
			printf("%d,",arr[i]);
		printf("\b] Sign : %d",flag);
		
		flag*=-1;
	}
	else{
		for(i=0;i<n-1;i++){
			heapPermute(n-1,arr,arrLen);
			
			if(n%2==0){
				temp = arr[i];
				arr[i] = arr[n-1];
				arr[n-1] = temp;
			}
			else{
				temp = arr[0];
				arr[0] = arr[n-1];
				arr[n-1] = temp;
			}
		}
		heapPermute(n-1,arr,arrLen);
	}
}

int main(int argC,char* argV[0])
{
	int *arr, i=0, count = 1;
	char* token;
	
	if(argC==1)
		printf("Usage : %s <comma separated list of integers>",argV[0]);
	else{
		while(argV[1][i]!=00){
			if(argV[1][i++]==',')
				count++;
		}
		
		arr = (int*)malloc(count*sizeof(int));
		
		i = 0;
		
		token = strtok(argV[1],",");
		
		while(token!=NULL){
			arr[i++] = atoi(token);
			token = strtok(NULL,",");
		}
		
		heapPermute(i,arr,count);
	}
		
	return 0;
}

```

Output:

```txt

C:\rosettaCode>heapPermute.exe 1,2,3

[1,2,3] Sign : 1
[2,1,3] Sign : -1
[3,1,2] Sign : 1
[1,3,2] Sign : -1
[2,3,1] Sign : 1
[3,2,1] Sign : -1

```



## C++

Direct implementation of Johnson-Trotter algorithm from the reference link.  

```cpp

#include <iostream>
#include <vector>

using namespace std;

vector<int> UpTo(int n, int offset = 0)
{
	vector<int> retval(n);
	for (int ii = 0; ii < n; ++ii)
		retval[ii] = ii + offset;
	return retval;
}

struct JohnsonTrotterState_
{
	vector<int> values_;
	vector<int> positions_;	// size is n+1, first element is not used
	vector<bool> directions_;
	int sign_;

	JohnsonTrotterState_(int n) : values_(UpTo(n, 1)), positions_(UpTo(n + 1, -1)), directions_(n + 1, false), sign_(1) {}

	int LargestMobile() const	// returns 0 if no mobile integer exists
	{
		for (int r = values_.size(); r > 0; --r)
		{
			const int loc = positions_[r] + (directions_[r] ? 1 : -1);
			if (loc >= 0 && loc < values_.size() && values_[loc] < r)
				return r;
		}
		return 0;
	}

	bool IsComplete() const { return LargestMobile() == 0; }

	void operator++()	// implement Johnson-Trotter algorithm
	{
		const int r = LargestMobile();
		const int rLoc = positions_[r];
		const int lLoc = rLoc + (directions_[r] ? 1 : -1);
		const int l = values_[lLoc];
		// do the swap
		swap(values_[lLoc], values_[rLoc]);
		swap(positions_[l], positions_[r]);
		sign_ = -sign_;
		// change directions
		for (auto pd = directions_.begin() + r + 1; pd != directions_.end(); ++pd)
			*pd = !*pd;
	}
};

int main(void)
{
	JohnsonTrotterState_ state(4);
	do
	{
		for (auto v : state.values_)
			cout << v << " ";
		cout << "\n";
		++state;
	} while (!state.IsComplete());
}

```

```txt

(1 2 3 4 ); sign = 1
(1 2 4 3 ); sign = -1
(1 4 2 3 ); sign = 1
(4 1 2 3 ); sign = -1
(4 1 3 2 ); sign = 1
(1 4 3 2 ); sign = -1
(1 3 4 2 ); sign = 1
(1 3 2 4 ); sign = -1
(3 1 2 4 ); sign = 1
(3 1 4 2 ); sign = -1
(3 4 1 2 ); sign = 1
(4 3 1 2 ); sign = -1
(4 3 2 1 ); sign = 1
(3 4 2 1 ); sign = -1
(3 2 4 1 ); sign = 1
(3 2 1 4 ); sign = -1
(2 3 1 4 ); sign = 1
(2 3 4 1 ); sign = -1
(2 4 3 1 ); sign = 1
(4 2 3 1 ); sign = -1
(4 2 1 3 ); sign = 1
(2 4 1 3 ); sign = -1
(2 1 4 3 ); sign = 1
```



## Clojure


### Recursive version


```clojure

(defn permutation-swaps
  "List of swap indexes to generate all permutations of n elements"
  [n]
  (if (= n 2) `((0 1))
    (let [old-swaps (permutation-swaps (dec n))
          swaps-> (partition 2 1 (range n))
          swaps<- (reverse swaps->)]
      (mapcat (fn [old-swap side]
                (case side
                  :first swaps<-
                  :right (conj swaps<- old-swap)
                  :left (conj swaps-> (map inc old-swap))))
              (conj old-swaps nil)
              (cons :first (cycle '(:left :right)))))))


(defn swap [v [i j]]
  (-> v
      (assoc i (nth v j))
      (assoc j (nth v i))))


(defn permutations [n]
  (let [permutations (reduce
                       (fn [all-perms new-swap]
                         (conj all-perms (swap (last all-perms)
                                               new-swap)))
                       (vector (vec (range n)))
                       (permutation-swaps n))
        output (map vector
                    permutations
                    (cycle '(1 -1)))]
    output))


(doseq [n [2 3 4]]
  (dorun (map println (permutations n))))

```


```txt

[[0 1] 1]
[[1 0] -1]
[[0 1 2] 1]
[[0 2 1] -1]
[[2 0 1] 1]
[[2 1 0] -1]
[[1 2 0] 1]
[[1 0 2] -1]
[[0 1 2 3] 1]
[[0 1 3 2] -1]
[[0 3 1 2] 1]
[[3 0 1 2] -1]
[[3 0 2 1] 1]
[[0 3 2 1] -1]
[[0 2 3 1] 1]
[[0 2 1 3] -1]
[[2 0 1 3] 1]
[[2 0 3 1] -1]
[[2 3 0 1] 1]
[[3 2 0 1] -1]
[[3 2 1 0] 1]
[[2 3 1 0] -1]
[[2 1 3 0] 1]
[[2 1 0 3] -1]
[[1 2 0 3] 1]
[[1 2 3 0] -1]
[[1 3 2 0] 1]
[[3 1 2 0] -1]
[[3 1 0 2] 1]
[[1 3 0 2] -1]
[[1 0 3 2] 1]
[[1 0 2 3] -1]

```



### Modeled After Python version

```clojure

(ns test-p.core)

(defn numbers-only [x]
  " Just shows the numbers only for the pairs (i.e. drops the direction --used for display purposes when printing the result"
  (mapv first x))

(defn next-permutation
  " Generates next permutation from the current (p) using the Johnson-Trotter technique
    The code below translates the Python version which has the following steps:
     p of form [...[n dir]...] such as [[0 1] [1 1] [2 -1]], where n is a number and dir = direction (=1=right, -1=left, 0=don't move)
     Step: 1 finds the pair [n dir] with the largest value of n (where dir is not equal to 0 (done if none)
     Step: 2: swap the max pair found with its neighbor in the direction of the pair (i.e. +1 means swap to right, -1 means swap left
     Step 3: if swapping places the pair a the beginning or end of the list, set the direction = 0 (i.e. becomes non-mobile)
     Step 4: Set the directions of all pairs whose numbers are greater to the right of where the pair was moved to -1 and to the left to +1 "
  [p]
  (if (every? zero? (map second p))
    nil                                                                 ; no mobile elements (all directions are zero)
    (let [n (count p)
          ; Step 1
          fn-find-max (fn [m]
                        (first (apply max-key                           ; find the max mobile elment
                                   (fn [[i x]]
                                     (if (zero? (second x))
                                       -1
                                       (first x)))
                                              (map-indexed vector p))))
          i1 (fn-find-max p)                                            ; index of max
          [n1 d1] (p i1)                                                ; value and direction of max
          i2 (+ d1 i1)
          fn-swap (fn [m] (assoc m i2 (m i1) i1 (m i2)))                ; function to swap with neighbor in our step direction
          fn-update-max (fn [m] (if (or (contains? #{0 (dec n)} i2)     ; update direction of max (where max went)
                                        (> ((m (+ i2 d1)) 0) n1))
                                  (assoc-in m [i2 1] 0)
                                  m))
          fn-update-others (fn [[i3 [n3 d3]]]                            ; Updates directions of pairs to the left and right of max
                             (cond                                       ; direction reset to -1 if to right, +1 if to left
                               (<= n3 n1) [n3 d3]
                               (< i3 i2) [n3 1]
                               :else      [n3 -1]))]
      ; apply steps 2, 3, 4(using functions that where created for these steps)
      (mapv fn-update-others (map-indexed vector (fn-update-max (fn-swap p)))))))

(defn spermutations
  " Lazy sequence of permutations of n digits"
  ; Each element is two element vector (number direction)
  ; Startup case - generates sequence 0...(n-1) with move direction (1 = move right, -1 = move left, 0 = don't move)
  ([n] (spermutations 1
                      (into [] (for [i (range n)] (if (zero? i)
                                                    [i 0]               ; 0th element is not mobile yet
                                                    [i -1])))))         ; all others move left
  ([sign p]
   (when-let [s (seq p)]
             (cons [(numbers-only p) sign]
                   (spermutations (- sign) (next-permutation p))))))   ; recursively tag onto sequence


;; Print results for 2, 3, and 4 items
(doseq [n (range 2 5)]
  (do
    (println)
    (println (format "Permutations and sign of %d items " n))
  (doseq [q (spermutations n)] (println (format "Perm: %s Sign: %2d" (first q) (second q))))))

```


```txt

Permutations and sign of 2 items 
Perm: [0 1] Sign:  1
Perm: [1 0] Sign: -1

Permutations and sign of 3 items 
Perm: [0 1 2] Sign:  1
Perm: [0 2 1] Sign: -1
Perm: [2 0 1] Sign:  1
Perm: [2 1 0] Sign: -1
Perm: [1 2 0] Sign:  1
Perm: [1 0 2] Sign: -1

Permutations and sign of 4 items 
Perm: [0 1 2 3] Sign:  1
Perm: [0 1 3 2] Sign: -1
Perm: [0 3 1 2] Sign:  1
Perm: [3 0 1 2] Sign: -1
Perm: [3 0 2 1] Sign:  1
Perm: [0 3 2 1] Sign: -1
Perm: [0 2 3 1] Sign:  1
Perm: [0 2 1 3] Sign: -1
Perm: [2 0 1 3] Sign:  1
Perm: [2 0 3 1] Sign: -1
Perm: [2 3 0 1] Sign:  1
Perm: [3 2 0 1] Sign: -1
Perm: [3 2 1 0] Sign:  1
Perm: [2 3 1 0] Sign: -1
Perm: [2 1 3 0] Sign:  1
Perm: [2 1 0 3] Sign: -1
Perm: [1 2 0 3] Sign:  1
Perm: [1 2 3 0] Sign: -1
Perm: [1 3 2 0] Sign:  1
Perm: [3 1 2 0] Sign: -1
Perm: [3 1 0 2] Sign:  1
Perm: [1 3 0 2] Sign: -1
Perm: [1 0 3 2] Sign:  1
Perm: [1 0 2 3] Sign: -1

```



## Common Lisp


```lisp
(defstruct (directed-number (:conc-name dn-))
  (number nil :type integer)
  (direction nil :type (member :left :right)))

(defmethod print-object ((dn directed-number) stream)
  (ecase (dn-direction dn)
    (:left  (format stream "<~D" (dn-number dn)))
    (:right (format stream "~D>" (dn-number dn)))))

(defun dn> (dn1 dn2)
  (declare (directed-number dn1 dn2))
  (> (dn-number dn1) (dn-number dn2)))

(defun dn-reverse-direction (dn)
  (declare (directed-number dn))
  (setf (dn-direction dn) (ecase (dn-direction dn)
                            (:left  :right)
                            (:right :left))))

(defun make-directed-numbers-upto (upto)
  (let ((numbers (make-array upto :element-type 'integer)))
    (dotimes (n upto numbers)
      (setf (aref numbers n) (make-directed-number :number (1+ n) :direction :left)))))

(defun max-mobile-pos (numbers)
  (declare ((vector directed-number) numbers))
  (loop with pos-limit = (1- (length numbers))
        with max-value and max-pos
        for num across numbers
        for pos from 0
        do (ecase (dn-direction num)
             (:left  (when (and (plusp pos) (dn> num (aref numbers (1- pos)))
                                (or (null max-value) (dn> num max-value)))
                       (setf max-value num
                             max-pos   pos)))
             (:right (when (and (< pos pos-limit) (dn> num (aref numbers (1+ pos)))
                                (or (null max-value) (dn> num max-value)))
                       (setf max-value num
                             max-pos   pos))))
        finally (return max-pos)))

(defun permutations (upto)
  (loop with numbers = (make-directed-numbers-upto upto)
        for max-mobile-pos = (max-mobile-pos numbers)
        for sign = 1 then (- sign)
        do (format t "~A sign: ~:[~;+~]~D~%" numbers (plusp sign) sign)
        while max-mobile-pos
        do (let ((max-mobile-number (aref numbers max-mobile-pos)))
             (ecase (dn-direction max-mobile-number)
               (:left  (rotatef (aref numbers (1- max-mobile-pos))
                                (aref numbers max-mobile-pos)))
               (:right (rotatef (aref numbers max-mobile-pos)
                                (aref numbers (1+ max-mobile-pos)))))
             (loop for n across numbers
                   when (dn> n max-mobile-number)
                     do (dn-reverse-direction n)))))

(permutations 3)
(permutations 4)
```

```txt
#(<1 <2 <3) sign: +1
#(<1 <3 <2) sign: -1
#(<3 <1 <2) sign: +1
#(3> <2 <1) sign: -1
#(<2 3> <1) sign: +1
#(<2 <1 3>) sign: -1
#(<1 <2 <3 <4) sign: +1
#(<1 <2 <4 <3) sign: -1
#(<1 <4 <2 <3) sign: +1
#(<4 <1 <2 <3) sign: -1
#(4> <1 <3 <2) sign: +1
#(<1 4> <3 <2) sign: -1
#(<1 <3 4> <2) sign: +1
#(<1 <3 <2 4>) sign: -1
#(<3 <1 <2 <4) sign: +1
#(<3 <1 <4 <2) sign: -1
#(<3 <4 <1 <2) sign: +1
#(<4 <3 <1 <2) sign: -1
#(4> 3> <2 <1) sign: +1
#(3> 4> <2 <1) sign: -1
#(3> <2 4> <1) sign: +1
#(3> <2 <1 4>) sign: -1
#(<2 3> <1 <4) sign: +1
#(<2 3> <4 <1) sign: -1
#(<2 <4 3> <1) sign: +1
#(<4 <2 3> <1) sign: -1
#(4> <2 <1 3>) sign: +1
#(<2 4> <1 3>) sign: -1
#(<2 <1 4> 3>) sign: +1
#(<2 <1 3> 4>) sign: -1
```



## D


### Iterative Version

This isn't a Range yet.
```d
import std.algorithm, std.array, std.typecons, std.range;

struct Spermutations(bool doCopy=true) {
    private immutable uint n;
    alias TResult = Tuple!(int[], int);

    int opApply(in int delegate(in ref TResult) nothrow dg) nothrow {
        int result;

        int sign = 1;
        alias Int2 = Tuple!(int, int);
        auto p = n.iota.map!(i => Int2(i, i ? -1 : 0)).array;
        TResult aux;

        aux[0] = p.map!(pi => pi[0]).array;
        aux[1] = sign;
        result = dg(aux);
        if (result)
            goto END;

        while (p.any!q{ a[1] }) {
            // Failed to use std.algorithm here, too much complex.
            auto largest = Int2(-100, -100);
            int i1 = -1;
            foreach (immutable i, immutable pi; p)
                if (pi[1])
                    if (pi[0] > largest[0]) {
                        i1 = i;
                        largest = pi;
                    }
            immutable n1 = largest[0],
                      d1 = largest[1];

            sign *= -1;
            int i2;
            if (d1 == -1) {
                i2 = i1 - 1;
                p[i1].swap(p[i2]);
                if (i2 == 0 || p[i2 - 1][0] > n1)
                    p[i2][1] = 0;
            } else if (d1 == 1) {
                i2 = i1 + 1;
                p[i1].swap(p[i2]);
                if (i2 == n - 1 || p[i2 + 1][0] > n1)
                    p[i2][1] = 0;
            }

            if (doCopy) {
                aux[0] = p.map!(pi => pi[0]).array;
            } else {
                foreach (immutable i, immutable pi; p)
                    aux[0][i] = pi[0];
            }
            aux[1] = sign;
            result = dg(aux);
            if (result)
                goto END;

            foreach (immutable i3, ref pi; p) {
                immutable n3 = pi[0],
                          d3 = pi[1];
                if (n3 > n1)
                    pi[1] = (i3 < i2) ? 1 : -1;
            }
        }

        END: return result;
    }
}

Spermutations!doCopy spermutations(bool doCopy=true)(in uint n) {
    return typeof(return)(n);
}

version (permutations_by_swapping1) {
    void main() {
        import std.stdio;
        foreach (immutable n; [3, 4]) {
            writefln("\nPermutations and sign of %d items", n);
            foreach (const tp; n.spermutations)
                writefln("Perm: %s  Sign: %2d", tp[]);
        }
    }
}
```

Compile with version=permutations_by_swapping1 to see the demo output.
```txt

Permutations and sign of 3 items
Perm: [0, 1, 2]  Sign:  1
Perm: [0, 2, 1]  Sign: -1
Perm: [2, 0, 1]  Sign:  1
Perm: [2, 1, 0]  Sign: -1
Perm: [1, 2, 0]  Sign:  1
Perm: [1, 0, 2]  Sign: -1

Permutations and sign of 4 items
Perm: [0, 1, 2, 3]  Sign:  1
Perm: [0, 1, 3, 2]  Sign: -1
Perm: [0, 3, 1, 2]  Sign:  1
Perm: [3, 0, 1, 2]  Sign: -1
Perm: [3, 0, 2, 1]  Sign:  1
Perm: [0, 3, 2, 1]  Sign: -1
Perm: [0, 2, 3, 1]  Sign:  1
Perm: [0, 2, 1, 3]  Sign: -1
Perm: [2, 0, 1, 3]  Sign:  1
Perm: [2, 0, 3, 1]  Sign: -1
Perm: [2, 3, 0, 1]  Sign:  1
Perm: [3, 2, 0, 1]  Sign: -1
Perm: [3, 2, 1, 0]  Sign:  1
Perm: [2, 3, 1, 0]  Sign: -1
Perm: [2, 1, 3, 0]  Sign:  1
Perm: [2, 1, 0, 3]  Sign: -1
Perm: [1, 2, 0, 3]  Sign:  1
Perm: [1, 2, 3, 0]  Sign: -1
Perm: [1, 3, 2, 0]  Sign:  1
Perm: [3, 1, 2, 0]  Sign: -1
Perm: [3, 1, 0, 2]  Sign:  1
Perm: [1, 3, 0, 2]  Sign: -1
Perm: [1, 0, 3, 2]  Sign:  1
Perm: [1, 0, 2, 3]  Sign: -1
```



### Recursive Version

```d
import std.algorithm, std.array, std.typecons, std.range;

auto sPermutations(in uint n) pure nothrow @safe {
    static immutable(int[])[] inner(in int items) pure nothrow @safe {
        if (items <= 0)
            return [[]];
        typeof(return) r;
        foreach (immutable i, immutable item; inner(items - 1)) {
            //r.put((i % 2 ? iota(item.length.signed, -1, -1) :
            //               iota(item.length + 1))
            //      .map!(i => item[0 .. i] ~ (items - 1) ~ item[i .. $]));
            immutable f = (in size_t i) pure nothrow @safe =>
                item[0 .. i] ~ (items - 1) ~ item[i .. $];
            r ~= (i % 2) ?
                 //iota(item.length.signed, -1, -1).map!f.array :
                 iota(item.length + 1).retro.map!f.array :
                 iota(item.length + 1).map!f.array;
        }
        return r;
    }

    return inner(n).zip([1, -1].cycle);
}

void main() {
    import std.stdio;
    foreach (immutable n; [2, 3, 4]) {
        writefln("Permutations and sign of %d items:", n);
        foreach (immutable tp; n.sPermutations)
            writefln("  %s Sign: %2d", tp[]);
        writeln;
    }
}
```

```txt
Permutations and sign of 2 items:
  [1, 0] Sign:  1
  [0, 1] Sign: -1

Permutations and sign of 3 items:
  [2, 1, 0] Sign:  1
  [1, 2, 0] Sign: -1
  [1, 0, 2] Sign:  1
  [0, 1, 2] Sign: -1
  [0, 2, 1] Sign:  1
  [2, 0, 1] Sign: -1

Permutations and sign of 4 items:
  [3, 2, 1, 0] Sign:  1
  [2, 3, 1, 0] Sign: -1
  [2, 1, 3, 0] Sign:  1
  [2, 1, 0, 3] Sign: -1
  [1, 2, 0, 3] Sign:  1
  [1, 2, 3, 0] Sign: -1
  [1, 3, 2, 0] Sign:  1
  [3, 1, 2, 0] Sign: -1
  [3, 1, 0, 2] Sign:  1
  [1, 3, 0, 2] Sign: -1
  [1, 0, 3, 2] Sign:  1
  [1, 0, 2, 3] Sign: -1
  [0, 1, 2, 3] Sign:  1
  [0, 1, 3, 2] Sign: -1
  [0, 3, 1, 2] Sign:  1
  [3, 0, 1, 2] Sign: -1
  [3, 0, 2, 1] Sign:  1
  [0, 3, 2, 1] Sign: -1
  [0, 2, 3, 1] Sign:  1
  [0, 2, 1, 3] Sign: -1
  [2, 0, 1, 3] Sign:  1
  [2, 0, 3, 1] Sign: -1
  [2, 3, 0, 1] Sign:  1
  [3, 2, 0, 1] Sign: -1

```



## EchoLisp

The function '''(in-permutations n)''' returns a stream which delivers permutations according to the Steinhaus–Johnson–Trotter algorithm.

```lisp

(lib 'list)

(for/fold (sign 1) ((σ (in-permutations 4)) (count 100)) 
    (printf "perm: %a count:%4d sign:%4d" σ count sign) (* sign -1))

perm: (0 1 2 3) count:   0 sign:   1
perm: (0 1 3 2) count:   1 sign:  -1
perm: (0 3 1 2) count:   2 sign:   1
perm: (3 0 1 2) count:   3 sign:  -1
perm: (3 0 2 1) count:   4 sign:   1
perm: (0 3 2 1) count:   5 sign:  -1
perm: (0 2 3 1) count:   6 sign:   1
perm: (0 2 1 3) count:   7 sign:  -1
perm: (2 0 1 3) count:   8 sign:   1
perm: (2 0 3 1) count:   9 sign:  -1
perm: (2 3 0 1) count:  10 sign:   1
perm: (3 2 0 1) count:  11 sign:  -1
perm: (3 2 1 0) count:  12 sign:   1
perm: (2 3 1 0) count:  13 sign:  -1
perm: (2 1 3 0) count:  14 sign:   1
perm: (2 1 0 3) count:  15 sign:  -1
perm: (1 2 0 3) count:  16 sign:   1
perm: (1 2 3 0) count:  17 sign:  -1
perm: (1 3 2 0) count:  18 sign:   1
perm: (3 1 2 0) count:  19 sign:  -1
perm: (3 1 0 2) count:  20 sign:   1
perm: (1 3 0 2) count:  21 sign:  -1
perm: (1 0 3 2) count:  22 sign:   1
perm: (1 0 2 3) count:  23 sign:  -1

```



## Elixir

```elixir
defmodule Permutation do
  def by_swap(n) do
    p = Enum.to_list(0..-n) |> List.to_tuple
    by_swap(n, p, 1)
  end
  
  defp by_swap(n, p, s) do
    IO.puts "Perm: #{inspect for i <- 1..n, do: abs(elem(p,i))}  Sign: #{s}"
    k = 0 |> step_up(n, p) |> step_down(n, p)
    if k > 0 do
      pk = elem(p,k)
      i = if pk>0, do: k+1, else: k-1
      p = Enum.reduce(1..n, p, fn i,acc ->
        if abs(elem(p,i)) > abs(pk), do: put_elem(acc, i, -elem(acc,i)), else: acc
      end)
      pi = elem(p,i)
      p = put_elem(p,i,pk) |> put_elem(k,pi)            # swap
      by_swap(n, p, -s)
    end
  end
  
  defp step_up(k, n, p) do
    Enum.reduce(2..n, k, fn i,acc ->
      if elem(p,i)<0 and abs(elem(p,i))>abs(elem(p,i-1)) and abs(elem(p,i))>abs(elem(p,acc)),
        do: i, else: acc 
    end)
  end
  
  defp step_down(k, n, p) do
    Enum.reduce(1..n-1, k, fn i,acc ->
      if elem(p,i)>0 and abs(elem(p,i))>abs(elem(p,i+1)) and abs(elem(p,i))>abs(elem(p,acc)),
        do: i, else: acc 
    end)
  end
end

Enum.each(3..4, fn n ->
  Permutation.by_swap(n)
  IO.puts ""
end)
```


```txt

Perm: [1, 2, 3]  Sign: 1
Perm: [1, 3, 2]  Sign: -1
Perm: [3, 1, 2]  Sign: 1
Perm: [3, 2, 1]  Sign: -1
Perm: [2, 3, 1]  Sign: 1
Perm: [2, 1, 3]  Sign: -1

Perm: [1, 2, 3, 4]  Sign: 1
Perm: [1, 2, 4, 3]  Sign: -1
Perm: [1, 4, 2, 3]  Sign: 1
Perm: [4, 1, 2, 3]  Sign: -1
Perm: [4, 1, 3, 2]  Sign: 1
Perm: [1, 4, 3, 2]  Sign: -1
Perm: [1, 3, 4, 2]  Sign: 1
Perm: [1, 3, 2, 4]  Sign: -1
Perm: [3, 1, 2, 4]  Sign: 1
Perm: [3, 1, 4, 2]  Sign: -1
Perm: [3, 4, 1, 2]  Sign: 1
Perm: [4, 3, 1, 2]  Sign: -1
Perm: [4, 3, 2, 1]  Sign: 1
Perm: [3, 4, 2, 1]  Sign: -1
Perm: [3, 2, 4, 1]  Sign: 1
Perm: [3, 2, 1, 4]  Sign: -1
Perm: [2, 3, 1, 4]  Sign: 1
Perm: [2, 3, 4, 1]  Sign: -1
Perm: [2, 4, 3, 1]  Sign: 1
Perm: [4, 2, 3, 1]  Sign: -1
Perm: [4, 2, 1, 3]  Sign: 1
Perm: [2, 4, 1, 3]  Sign: -1
Perm: [2, 1, 4, 3]  Sign: 1
Perm: [2, 1, 3, 4]  Sign: -1

```

=={{header|F_Sharp|F#}}==
See [http://www.rosettacode.org/wiki/Zebra_puzzle#F.23] for an example using this module

```fsharp

(*Implement Johnson-Trotter algorithm
  Nigel Galloway January 24th 2017*)
module Ring
let PlainChanges (N:'n[]) = seq{
  let gn  = [|for n in N -> 1|]
  let ni  = [|for n in N -> 0|]
  let gel = Array.length(N)-1
  yield Some N
  let rec _Ni g e l = seq{
    match (l,g) with
    |_ when l<0   -> gn.[g] <- -gn.[g]; yield! _Ni (g-1) e (ni.[g-1] + gn.[g-1])
    |(1,0)        -> yield None
    |_ when l=g+1 -> gn.[g] <- -gn.[g]; yield! _Ni (g-1) (e+1) (ni.[g-1] + gn.[g-1])
    |_ -> let n = N.[g-ni.[g]+e];
          N.[g-ni.[g]+e] <- N.[g-l+e]; N.[g-l+e] <- n; yield Some N
          ni.[g] <- l; yield! _Ni gel 0 (ni.[gel] + gn.[gel])}
  yield! _Ni gel 0 1
}

```

A little code for the purpose of this task demonstrating the algorithm

```fsharp

for n in Ring.PlainChanges [|1;2;3;4|] do printfn "%A" n

```

```txt

Some [|1; 2; 3; 4|]
Some [|1; 2; 4; 3|]
Some [|1; 4; 2; 3|]
Some [|4; 1; 2; 3|]
Some [|4; 1; 3; 2|]
Some [|1; 4; 3; 2|]
Some [|1; 3; 4; 2|]
Some [|1; 3; 2; 4|]
Some [|3; 1; 2; 4|]
Some [|3; 1; 4; 2|]
Some [|3; 4; 1; 2|]
Some [|4; 3; 1; 2|]
Some [|4; 3; 2; 1|]
Some [|3; 4; 2; 1|]
Some [|3; 2; 4; 1|]
Some [|3; 2; 1; 4|]
Some [|2; 3; 1; 4|]
Some [|2; 3; 4; 1|]
Some [|2; 4; 3; 1|]
Some [|4; 2; 3; 1|]
Some [|4; 2; 1; 3|]
Some [|2; 4; 1; 3|]
Some [|2; 1; 4; 3|]
Some [|2; 1; 3; 4|]
<null>

```


## Forth

```forth
S" fsl-util.fs" REQUIRED
S" fsl/dynmem.seq" REQUIRED

cell darray p{

: sgn
  DUP 0 > IF
    DROP 1
  ELSE 0 < IF
    -1
  ELSE
    0
  THEN THEN ;
: arr-swap {: addr1 addr2 | tmp -- :}
  addr1 @ TO tmp
  addr2 @ addr1 !
  tmp addr2 ! ;
: perms {: n xt | my-i k s -- :}
  & p{ n 1+ }malloc malloc-fail? ABORT" perms :: out of memory"
  0 p{ 0 } !
  n 1+ 1 DO
    I NEGATE p{ I } !
  LOOP
  1 TO s
  BEGIN
    1 n 1+ DO
      p{ I } @ ABS
    -1 +LOOP
    n 1+ s xt EXECUTE
    0 TO k
    n 1+ 2 DO
      p{ I } @ 0 < ( flag )
      p{ I } @ ABS  p{ I 1- } @ ABS  > ( flag flag )
      p{ I } @ ABS p{ k } @ ABS > ( flag flag flag )
      AND AND IF
        I TO k
      THEN
    LOOP
    n 1 DO
      p{ I } @ 0 > ( flag )
      p{ I } @ ABS  p{ I 1+ } @ ABS  > ( flag flag )
      p{ I } @ ABS  p{ k } @ ABS  > ( flag flag flag )
      AND AND IF
        I TO k
      THEN
    LOOP
    k IF
      n 1+ 1 DO
        p{ I } @ ABS  p{ k } @ ABS  > IF
          p{ I } @ NEGATE p{ I } !
        THEN
      LOOP
      p{ k } @ sgn k + TO my-i
      p{ k } p{ my-i } arr-swap
      s NEGATE TO s
    THEN
  k 0 = UNTIL ;
: .perm ( p0 p1 p2 ... pn n s )
  >R
  ." Perm: [ "
  1 DO
    . SPACE
  LOOP
  R> ." ] Sign: " . CR ;

3 ' .perm perms CR
4 ' .perm perms
```



## FreeBASIC

```freebasic
' version 31-03-2017
' compile with: fbc -s console

Sub perms(n As ULong)

    Dim As Long p(n), i, k, s = 1

    For i = 1 To n
        p(i) = -i
    Next

    Do
        Print "Perm: [ ";
        For i = 1 To n
            Print Abs(p(i)); " ";
        Next
        Print "] Sign: "; s

        k = 0
        For i = 2 To n
            If p(i) < 0 Then
                If Abs(p(i)) > Abs(p(i -1)) Then
                    If Abs(p(i)) > Abs(p(k)) Then k = i
                End If
            End If
        Next

        For i = 1 To n -1
            If p(i) > 0 Then
                If Abs(p(i)) > Abs(p(i +1)) Then
                    If Abs(p(i)) > Abs(p(k)) Then k = i
                End If
            End If
        Next

        If k Then
            For  i = 1 To n
                If Abs(p(i)) > Abs(p(k)) Then p(i) = -p(i)
            Next
            i = k + Sgn(p(k))
            Swap p(k), p(i)
            s = -s
        End If

    Loop Until k = 0

End Sub

' ------=< MAIN >=------

perms(3)
print
perms(4)

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End

```

```txt
output is edited to show results side by side
       Perm: [  1  2  3 ] Sign:  1         Perm: [  1  2  3  4 ] Sign:  1
       Perm: [  1  3  2 ] Sign: -1         Perm: [  1  2  4  3 ] Sign: -1
       Perm: [  3  1  2 ] Sign:  1         Perm: [  1  4  2  3 ] Sign:  1
       Perm: [  3  2  1 ] Sign: -1         Perm: [  4  1  2  3 ] Sign: -1
       Perm: [  2  3  1 ] Sign:  1         Perm: [  4  1  3  2 ] Sign:  1
       Perm: [  2  1  3 ] Sign: -1         Perm: [  1  4  3  2 ] Sign: -1
                                           Perm: [  1  2  3  4 ] Sign:  1
                                           Perm: [  1  2  4  3 ] Sign: -1
                                           Perm: [  1  4  2  3 ] Sign:  1
                                           Perm: [  4  1  2  3 ] Sign: -1
                                           Perm: [  4  1  3  2 ] Sign:  1
                                           Perm: [  1  4  3  2 ] Sign: -1
                                           Perm: [  1  3  4  2 ] Sign:  1
                                           Perm: [  1  3  2  4 ] Sign: -1
                                           Perm: [  3  1  2  4 ] Sign:  1
                                           Perm: [  3  1  4  2 ] Sign: -1
                                           Perm: [  3  4  1  2 ] Sign:  1
                                           Perm: [  4  3  1  2 ] Sign: -1
                                           Perm: [  4  3  2  1 ] Sign:  1
                                           Perm: [  3  4  2  1 ] Sign: -1
                                           Perm: [  3  2  4  1 ] Sign:  1
                                           Perm: [  3  2  1  4 ] Sign: -1
                                           Perm: [  2  3  1  4 ] Sign:  1
                                           Perm: [  2  3  4  1 ] Sign: -1
                                           Perm: [  2  4  3  1 ] Sign:  1
                                           Perm: [  4  2  3  1 ] Sign: -1
                                           Perm: [  4  2  1  3 ] Sign:  1
                                           Perm: [  2  4  1  3 ] Sign: -1
                                           Perm: [  2  1  4  3 ] Sign:  1
                                           Perm: [  2  1  3  4 ] Sign: -1
```



## Go


```go
package permute

// Iter takes a slice p and returns an iterator function.  The iterator
// permutes p in place and returns the sign.  After all permutations have
// been generated, the iterator returns 0 and p is left in its initial order.
func Iter(p []int) func() int {
    f := pf(len(p))
    return func() int {
        return f(p)
    }
}

// Recursive function used by perm, returns a chain of closures that
// implement a loopless recursive SJT.
func pf(n int) func([]int) int {
    sign := 1
    switch n {
    case 0, 1:
        return func([]int) (s int) {
            s = sign
            sign = 0
            return
        }
    default:
        p0 := pf(n - 1)
        i := n
        var d int
        return func(p []int) int {
            switch {
            case sign == 0:
            case i == n:
                i--
                sign = p0(p[:i])
                d = -1
            case i == 0:
                i++
                sign *= p0(p[1:])
                d = 1
                if sign == 0 {
                    p[0], p[1] = p[1], p[0]
                }
            default:
                p[i], p[i-1] = p[i-1], p[i]
                sign = -sign
                i += d
            }
            return sign
        }
    }
}
```


```go
package main

import (
    "fmt"
    "permute"
)

func main() {
    p := []int{11, 22, 33}
    i := permute.Iter(p)
    for sign := i(); sign != 0; sign = i() {
        fmt.Println(p, sign)
    }
}
```

```txt

[11 22 33] 1
[11 33 22] -1
[33 11 22] 1
[33 22 11] -1
[22 33 11] 1
[22 11 33] -1

```



## Haskell


```haskell
sPermutations :: [a] -> [([a], Int)]
sPermutations = flip zip (cycle [-1, 1]) . foldr aux [[]]
  where
    aux x items = do
      (f, item) <- zip (repeat id) items
      f (insertEv x item)
    insertEv x [] = [[x]]
    insertEv x l@(y:ys) = (x : l) : ((y :) <$> insertEv x ys)

main :: IO ()
main = do
  putStrLn "3 items:"
  mapM_ print $ sPermutations [1 .. 3]
  putStrLn "\n4 items:"
  mapM_ print $ sPermutations [1 .. 4]
```

```txt
3 items:
([1,2,3],-1)
([2,1,3],1)
([2,3,1],-1)
([1,3,2],1)
([3,1,2],-1)
([3,2,1],1)

4 items:
([1,2,3,4],-1)
([2,1,3,4],1)
([2,3,1,4],-1)
([2,3,4,1],1)
([1,3,2,4],-1)
([3,1,2,4],1)
([3,2,1,4],-1)
([3,2,4,1],1)
([1,3,4,2],-1)
([3,1,4,2],1)
([3,4,1,2],-1)
([3,4,2,1],1)
([1,2,4,3],-1)
([2,1,4,3],1)
([2,4,1,3],-1)
([2,4,3,1],1)
([1,4,2,3],-1)
([4,1,2,3],1)
([4,2,1,3],-1)
([4,2,3,1],1)
([1,4,3,2],-1)
([4,1,3,2],1)
([4,3,1,2],-1)
([4,3,2,1],1)
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages.
```unicon
procedure main(A)
    every write("Permutations of length ",n := !A) do
       every p := permute(n) do write("\t",showList(p[1])," -> ",right(p[2],2))
end
 
procedure permute(n)
    items := [[]]
    every (j := 1 to n, new_items := []) do {
        every item := items[i := 1 to *items] do {
            if *item = 0 then put(new_items, [j])
            else if i%2 = 0 then
                every k := 1 to *item+1 do {
                    new_item := item[1:k] ||| [j] ||| item[k:0]
                    put(new_items, new_item)
                    }
            else
                every k := *item+1 to 1 by -1 do {
                    new_item := item[1:k] ||| [j] ||| item[k:0]
                    put(new_items, new_item)
                    }
            }
       items := new_items
       }
    suspend (i := 0, [!items, if (i+:=1)%2 = 0 then 1 else -1])
end

procedure showList(A)
    every (s := "[") ||:= image(!A)||", "
    return s[1:-2]||"]"
end
```


Sample run:

```txt

->pbs 3 4
Permutations of length 3
        [1, 2, 3] -> -1
        [1, 3, 2] ->  1
        [3, 1, 2] -> -1
        [3, 2, 1] ->  1
        [2, 3, 1] -> -1
        [2, 1, 3] ->  1
Permutations of length 4
        [1, 2, 3, 4] -> -1
        [1, 2, 4, 3] ->  1
        [1, 4, 2, 3] -> -1
        [4, 1, 2, 3] ->  1
        [4, 1, 3, 2] -> -1
        [1, 4, 3, 2] ->  1
        [1, 3, 4, 2] -> -1
        [1, 3, 2, 4] ->  1
        [3, 1, 2, 4] -> -1
        [3, 1, 4, 2] ->  1
        [3, 4, 1, 2] -> -1
        [4, 3, 1, 2] ->  1
        [4, 3, 2, 1] -> -1
        [3, 4, 2, 1] ->  1
        [3, 2, 4, 1] -> -1
        [3, 2, 1, 4] ->  1
        [2, 3, 1, 4] -> -1
        [2, 3, 4, 1] ->  1
        [2, 4, 3, 1] -> -1
        [4, 2, 3, 1] ->  1
        [4, 2, 1, 3] -> -1
        [2, 4, 1, 3] ->  1
        [2, 1, 4, 3] -> -1
        [2, 1, 3, 4] ->  1
->

```



## J

J has a built in mechanism for [[j:Vocabulary/acapdot|representing permutations]] for selecting a permutation of a given length with an integer, but this mechanism does not seem to have an obvious mapping to Steinhaus–Johnson–Trotter.  Perhaps someone with a sufficiently deep view of the subject of permutations can find a direct mapping?

Meanwhile, here's an inductive approach, using negative integers to look left and positive integers to look right:


```J
bfsjt0=: _1 - i.
lookingat=: 0 >. <:@# <. i.@# + * 
next=: | >./@:* | > | {~ lookingat
bfsjtn=: (((] <@, ] + *@{~) | i. next) C. ] * _1 ^ next < |)^:(*@next)
```


Here, bfsjt0 N gives the initial permutation of order N, and bfsjtn^:M bfsjt0 N gives the Mth Steinhaus–Johnson–Trotter permutation of order N.  (bf stands for "brute force".)

To convert from the Steinhaus–Johnson–Trotter representation of a permutation to J's representation, use <:@|, or to find J's anagram index of a Steinhaus–Johnson–Trotter representation of a permutation, use A.@:<:@:|

Example use:


```J
   bfsjtn^:(i.!3) bfjt0 3
_1 _2 _3
_1 _3 _2
_3 _1 _2
 3 _2 _1
_2  3 _1
_2 _1  3
   <:@| bfsjtn^:(i.!3) bfjt0 3
0 1 2
0 2 1
2 0 1
2 1 0
1 2 0
1 0 2
   A. <:@| bfsjtn^:(i.!3) bfjt0 3
0 1 4 5 3 2
```


Here's an example of the Steinhaus–Johnson–Trotter representation of 3 element permutation, with sign (sign is the first column):


```J
   (_1^2|i.!3),. bfsjtn^:(i.!3) bfjt0 3
 1 _1 _2 _3
_1 _1 _3 _2
 1 _3 _1 _2
_1  3 _2 _1
 1 _2  3 _1
_1 _2 _1  3
```


Alternatively, J defines [http://www.jsoftware.com/help/dictionary/dccapdot.htm C.!.2] as the parity of a permutation:


```J
   (,.~C.!.2)<:| bfsjtn^:(i.!3) bfjt0 3
 1 0 1 2
_1 0 2 1
 1 2 0 1
_1 2 1 0
 1 1 2 0
_1 1 0 2
```



### Recursive Implementation


This is based on the python recursive implementation:


```J
rsjt=: 3 :0
  if. 2>y do. i.2#y
  else.  ((!y)$(,~|.)-.=i.y)#inv!.(y-1)"1 y#rsjt y-1
  end.
)
```


Example use (here, prefixing each row with its parity):


```J
   (,.~ C.!.2) rsjt 3
 1 0 1 2
_1 0 2 1
 1 2 0 1
_1 2 1 0
 1 1 2 0
_1 1 0 2
```



## Java


Heap's Algorithm, recursive and looping implementations


```Java
package org.rosettacode.java;

import java.util.Arrays;
import java.util.stream.IntStream;

public class HeapsAlgorithm {

	public static void main(String[] args) {
		Object[] array = IntStream.range(0, 4)
				.boxed()
				.toArray();
		HeapsAlgorithm algorithm = new HeapsAlgorithm();
		algorithm.recursive(array);
		System.out.println();
		algorithm.loop(array);
	}

	void recursive(Object[] array) {
		recursive(array, array.length, true);
	}

	void recursive(Object[] array, int n, boolean plus) {
		if (n == 1) {
			output(array, plus);
		} else {
			for (int i = 0; i < n; i++) {
				recursive(array, n - 1, i == 0);
				swap(array, n % 2 == 0 ? i : 0, n - 1);
			}
		}
	}

	void output(Object[] array, boolean plus) {
		System.out.println(Arrays.toString(array) + (plus ? " +1" : " -1"));
	}

	void swap(Object[] array, int a, int b) {
		Object o = array[a];
		array[a] = array[b];
		array[b] = o;
	}

	void loop(Object[] array) {
		loop(array, array.length);
	}

	void loop(Object[] array, int n) {
		int[] c = new int[n];
		output(array, true);
		boolean plus = false;
		for (int i = 0; i < n; ) {
			if (c[i] < i) {
				if (i % 2 == 0) {
					swap(array, 0, i);
				} else {
					swap(array, c[i], i);
				}
				output(array, plus);
				plus = !plus;
				c[i]++;
				i = 0;
			} else {
				c[i] = 0;
				i++;
			}
		}
	}
}
```

```txt

[0, 1, 2, 3] +1
[1, 0, 2, 3] -1
[2, 0, 1, 3] +1
[0, 2, 1, 3] -1
[1, 2, 0, 3] +1
[2, 1, 0, 3] -1
[3, 1, 2, 0] +1
[1, 3, 2, 0] -1
[2, 3, 1, 0] +1
[3, 2, 1, 0] -1
[1, 2, 3, 0] +1
[2, 1, 3, 0] -1
[3, 0, 2, 1] +1
[0, 3, 2, 1] -1
[2, 3, 0, 1] +1
[3, 2, 0, 1] -1
[0, 2, 3, 1] +1
[2, 0, 3, 1] -1
[3, 0, 1, 2] +1
[0, 3, 1, 2] -1
[1, 3, 0, 2] +1
[3, 1, 0, 2] -1
[0, 1, 3, 2] +1
[1, 0, 3, 2] -1

[3, 0, 1, 2] +1
[0, 3, 1, 2] -1
[1, 3, 0, 2] +1
[3, 1, 0, 2] -1
[0, 1, 3, 2] +1
[1, 0, 3, 2] -1
[2, 0, 3, 1] +1
[0, 2, 3, 1] -1
[3, 2, 0, 1] +1
[2, 3, 0, 1] -1
[0, 3, 2, 1] +1
[3, 0, 2, 1] -1
[3, 1, 2, 0] +1
[1, 3, 2, 0] -1
[2, 3, 1, 0] +1
[3, 2, 1, 0] -1
[1, 2, 3, 0] +1
[2, 1, 3, 0] -1
[2, 1, 0, 3] +1
[1, 2, 0, 3] -1
[0, 2, 1, 3] +1
[2, 0, 1, 3] -1
[1, 0, 2, 3] +1
[0, 1, 2, 3] -1

```



## jq

Based on the ruby version - the sequence is generated by swapping adjacent elements.

"permutations" generates a stream of arrays of the form [par, perm], where "par" is the parity of the permutation "perm" of the
input array. This array may contain any JSON entities, which are regarded as distinct.


```jq
# The helper function, _recurse, is tail-recursive and therefore in
# versions of jq with TCO (tail call optimization) there is no
# overhead associated with the recursion.

def permutations:
  def abs: if . < 0 then -. else . end;
  def sign: if . < 0 then -1 elif . == 0 then 0 else 1 end;
  def swap(i;j): .[i] as $i | .[i] = .[j] | .[j] = $i;

  # input: [ parity, extendedPermutation]
  def _recurse:
    .[0] as $s | .[1] as $p | (($p | length) -1) as $n
    | [ $s, ($p[1:] | map(abs)) ],
      (reduce range(2; $n+1) as $i
         (0;
          if $p[$i] < 0 and -($p[$i]) > ($p[$i-1]|abs) and -($p[$i]) > ($p[.]|abs)
          then $i 
          else .
          end)) as $k
      | (reduce range(1; $n) as $i
           ($k;
            if $p[$i] > 0 and $p[$i] > ($p[$i+1]|abs) and $p[$i] > ($p[.]|abs)
            then $i 
            else .
            end)) as $k
      | if $k == 0 then empty
        else (reduce range(1; $n) as $i
	       ($p;
                if (.[$i]|abs) > (.[$k]|abs) then .[$i] *= -1 
                else .
                end )) as $p
        | ($k + ($p[$k]|sign)) as $i
        | ($p | swap($i; $k)) as $p
        | [ -($s), $p ] | _recurse
        end ;

  . as $in
  | length as $n
  | (reduce range(0; $n+1) as $i ([]; . + [ -$i ])) as $p
  # recurse state: [$s, $p]
  | [ 1, $p] | _recurse
  | .[1] as $p
  | .[1] = reduce range(0; $n) as $i ([]; . + [$in[$p[$i]  - 1]]) ;

def count(stream): reduce stream as $x (0; .+1);
```

'''Examples:'''

```jq
(["a", "b", "c"] | permutations),
"There are \(count( [range(1;6)] | permutations )) permutations of 5 items."
```

```sh
$ jq -c -n -f Permutations_by_swapping.jq
[1,["a","b","c"]]
[-1,["a","c","b"]]
[1,["c","a","b"]]
[-1,["c","b","a"]]
[1,["b","c","a"]]
[-1,["b","a","c"]]

"There are 32 permutations of 5 items."
```



## Julia

Nonrecursive (interative):

```julia

function johnsontrottermove!(ints, isleft)
    len = length(ints)
    function ismobile(pos)
        if isleft[pos] && (pos > 1) && (ints[pos-1] < ints[pos])
            return true
        elseif !isleft[pos] && (pos < len) && (ints[pos+1] < ints[pos])
            return true
        end
        false
    end
    function maxmobile()
        arr = [ints[pos] for pos in 1:len if ismobile(pos)]
        if isempty(arr)
            0, 0
        else
            maxmob = maximum(arr)
            maxmob, findfirst(x -> x == maxmob, ints)
        end
    end
    function directedswap(pos)
        tmp = ints[pos]
        tmpisleft = isleft[pos]
        if isleft[pos]
            ints[pos] = ints[pos-1]; ints[pos-1] = tmp
            isleft[pos] = isleft[pos-1]; isleft[pos-1] = tmpisleft
        else
            ints[pos] = ints[pos+1]; ints[pos+1] = tmp
            isleft[pos] = isleft[pos+1]; isleft[pos+1] = tmpisleft
        end
    end
    (moveint, movepos) = maxmobile()
    if movepos > 0
        directedswap(movepos)
        for (i, val) in enumerate(ints)
            if val > moveint
                isleft[i] = !isleft[i]
            end
        end
        ints, isleft, true
    else
        ints, isleft, false
    end
end
function johnsontrotter(low, high)
    ints = collect(low:high)
    isleft = [true for i in ints]
    firstconfig = copy(ints)
    iters = 0
    while true
        iters += 1
        println("$ints $(iters & 1 == 1 ? "+1" : "-1")")
        if johnsontrottermove!(ints, isleft)[3] == false
            break
        end
    end
    println("There were $iters iterations.")
end
johnsontrotter(1,4)

```

Recursive (note this uses memory of roughtly (n+1)! bytes, where n is the number of elements, in order to store the accumulated permutations in a list, and so the above, iterative solution is to be preferred for numbers of elements over 9 or so):

```julia

function johnsontrotter(low, high)
    function permutelevel(vec)
        if length(vec) < 2
            return [vec]
        end
        sequences = []
        endint = vec[end]
        smallersequences = permutelevel(vec[1:end-1])
        leftward = true
        for seq in smallersequences
            for pos in (leftward ? (length(seq)+1:-1:1): (1:length(seq)+1))
                push!(sequences, insert!(copy(seq), pos, endint))
            end
            leftward = !leftward
        end
        sequences
    end
    permutelevel(collect(low:high))
end

for (i, sequence) in enumerate(johnsontrotter(1,4))
    println("""$sequence, $(i & 1 == 1 ? "+1" : "-1")""")
end

```



## Kotlin

This is based on the recursive Java code found at http://introcs.cs.princeton.edu/java/23recursion/JohnsonTrotter.java.html 

```scala
// version 1.1.2

fun johnsonTrotter(n: Int): Pair<List<IntArray>, List<Int>> {
    val p = IntArray(n) { it }  // permutation
    val q = IntArray(n) { it }  // inverse permutation
    val d = IntArray(n) { -1 }  // direction = 1 or -1
    var sign = 1
    val perms = mutableListOf<IntArray>()
    val signs = mutableListOf<Int>()

    fun permute(k: Int) {
        if (k >= n) {
            perms.add(p.copyOf())
            signs.add(sign)
            sign *= -1
            return
        } 
        permute(k + 1)
        for (i in 0 until k) {
            val z = p[q[k] + d[k]]
            p[q[k]] = z
            p[q[k] + d[k]] = k
            q[z] = q[k]
            q[k] += d[k]
            permute(k + 1)
        }
        d[k] *= -1
    } 

    permute(0)
    return perms to signs
}

fun printPermsAndSigns(perms: List<IntArray>, signs: List<Int>) {
    for ((i, perm) in perms.withIndex()) {
        println("${perm.contentToString()} -> sign = ${signs[i]}")
    }
}

fun main(args: Array<String>) {
    val (perms, signs) = johnsonTrotter(3)
    printPermsAndSigns(perms, signs)
    println()
    val (perms2, signs2) = johnsonTrotter(4)
    printPermsAndSigns(perms2, signs2)
}
```


```txt

[0, 1, 2] -> sign = 1
[0, 2, 1] -> sign = -1
[2, 0, 1] -> sign = 1
[2, 1, 0] -> sign = -1
[1, 2, 0] -> sign = 1
[1, 0, 2] -> sign = -1

[0, 1, 2, 3] -> sign = 1
[0, 1, 3, 2] -> sign = -1
[0, 3, 1, 2] -> sign = 1
[3, 0, 1, 2] -> sign = -1
[3, 0, 2, 1] -> sign = 1
[0, 3, 2, 1] -> sign = -1
[0, 2, 3, 1] -> sign = 1
[0, 2, 1, 3] -> sign = -1
[2, 0, 1, 3] -> sign = 1
[2, 0, 3, 1] -> sign = -1
[2, 3, 0, 1] -> sign = 1
[3, 2, 0, 1] -> sign = -1
[3, 2, 1, 0] -> sign = 1
[2, 3, 1, 0] -> sign = -1
[2, 1, 3, 0] -> sign = 1
[2, 1, 0, 3] -> sign = -1
[1, 2, 0, 3] -> sign = 1
[1, 2, 3, 0] -> sign = -1
[1, 3, 2, 0] -> sign = 1
[3, 1, 2, 0] -> sign = -1
[3, 1, 0, 2] -> sign = 1
[1, 3, 0, 2] -> sign = -1
[1, 0, 3, 2] -> sign = 1
[1, 0, 2, 3] -> sign = -1

```



## Lua

```Lua
_JT={}
function JT(dim)
  local n={ values={}, positions={}, directions={}, sign=1 }
  setmetatable(n,{__index=_JT})
  for i=1,dim do
    n.values[i]=i
    n.positions[i]=i
    n.directions[i]=-1
  end
  return n
end

function _JT:largestMobile()
  for i=#self.values,1,-1 do
    local loc=self.positions[i]+self.directions[i]
    if loc >= 1 and loc <= #self.values and self.values[loc] < i then
      return i
    end
  end
  return 0
end

function _JT:next()
  local r=self:largestMobile()
  if r==0 then return false end
  local rloc=self.positions[r]
  local lloc=rloc+self.directions[r]
  local l=self.values[lloc]
  self.values[lloc],self.values[rloc] = self.values[rloc],self.values[lloc]
  self.positions[l],self.positions[r] = self.positions[r],self.positions[l]
  self.sign=-self.sign
  for i=r+1,#self.directions do self.directions[i]=-self.directions[i] end
  return true
end  

-- test

perm=JT(4)
repeat
  print(unpack(perm.values))
until not perm:next()
```

```txt
1       2       3       4
1       2       4       3
1       4       2       3
4       1       2       3
4       1       3       2
1       4       3       2
1       3       4       2
1       3       2       4
3       1       2       4
3       1       4       2
3       4       1       2
4       3       1       2
4       3       2       1
3       4       2       1
3       2       4       1
3       2       1       4
2       3       1       4
2       3       4       1
2       4       3       1
4       2       3       1
4       2       1       3
2       4       1       3
2       1       4       3
2       1       3       4
```


### Coroutine Implementation

This is adapted from the [https://www.lua.org/pil/9.3.html Lua Book ].

```lua
local wrap, yield = coroutine.wrap, coroutine.yield
local function perm(n)
    local r = {}
    for i=1,n do r[i]=i end    
    local sign = 1
  return wrap(function()
    local function swap(m)      
      if m==0 then  
        sign = -sign, yield(sign,r) 
      else
        for i=m,1,-1 do
          r[i],r[m]=r[m],r[i]
          swap(m-1)
          r[i],r[m]=r[m],r[i]
        end    
      end
    end
    swap(n)
  end)
end
for sign,r in perm(3) do print(sign,table.unpack(r))end
```



## Mathematica


###  Recursive 

<lang>perms[0] = {{{}, 1}}; 
perms[n_] := 
 Flatten[If[#2 == 1, Reverse, # &]@
     Table[{Insert[#1, n, i], (-1)^(n + i) #2}, {i, n}] & @@@ 
   perms[n - 1], 1];
```

Example:
<lang>Print["Perm: ", #[[1]], " Sign: ", #[[2]]] & /@ perms@4;
```

```txt
Perm: {1,2,3,4} Sign: 1
Perm: {1,2,4,3} Sign: -1
Perm: {1,4,2,3} Sign: 1
Perm: {4,1,2,3} Sign: -1
Perm: {4,1,3,2} Sign: 1
Perm: {1,4,3,2} Sign: -1
Perm: {1,3,4,2} Sign: 1
Perm: {1,3,2,4} Sign: -1
Perm: {3,1,2,4} Sign: 1
Perm: {3,1,4,2} Sign: -1
Perm: {3,4,1,2} Sign: 1
Perm: {4,3,1,2} Sign: -1
Perm: {4,3,2,1} Sign: 1
Perm: {3,4,2,1} Sign: -1
Perm: {3,2,4,1} Sign: 1
Perm: {3,2,1,4} Sign: -1
Perm: {2,3,1,4} Sign: 1
Perm: {2,3,4,1} Sign: -1
Perm: {2,4,3,1} Sign: 1
Perm: {4,2,3,1} Sign: -1
Perm: {4,2,1,3} Sign: 1
Perm: {2,4,1,3} Sign: -1
Perm: {2,1,4,3} Sign: 1
Perm: {2,1,3,4} Sign: -1
```



## Nim


```nim
# iterative Boothroyd method
iterator permutations*[T](ys: openarray[T]): tuple[perm: seq[T], sign: int] =
  var
    d = 1
    c = newSeq[int](ys.len)
    xs = newSeq[T](ys.len)
    sign = 1

  for i, y in ys: xs[i] = y
  yield (xs, sign)

  block outter:
    while true:
      while d > 1:
        dec d
        c[d] = 0
      while c[d] >= d:
        inc d
        if d >= ys.len: break outter

      let i = if (d and 1) == 1: c[d] else: 0
      swap xs[i], xs[d]
      sign *= -1
      yield (xs, sign)
      inc c[d]

if isMainModule:
  for i in permutations([0,1,2]):
    echo i

  echo ""

  for i in permutations([0,1,2,3]):
    echo i
```

```txt
(perm: @[0, 1, 2], sign: 1)
(perm: @[1, 0, 2], sign: -1)
(perm: @[2, 0, 1], sign: 1)
(perm: @[0, 2, 1], sign: -1)
(perm: @[1, 2, 0], sign: 1)
(perm: @[2, 1, 0], sign: -1)

(perm: @[0, 1, 2, 3], sign: 1)
(perm: @[1, 0, 2, 3], sign: -1)
(perm: @[2, 0, 1, 3], sign: 1)
(perm: @[0, 2, 1, 3], sign: -1)
(perm: @[1, 2, 0, 3], sign: 1)
(perm: @[2, 1, 0, 3], sign: -1)
(perm: @[3, 1, 0, 2], sign: 1)
(perm: @[1, 3, 0, 2], sign: -1)
(perm: @[0, 3, 1, 2], sign: 1)
(perm: @[3, 0, 1, 2], sign: -1)
(perm: @[1, 0, 3, 2], sign: 1)
(perm: @[0, 1, 3, 2], sign: -1)
(perm: @[0, 2, 3, 1], sign: 1)
(perm: @[2, 0, 3, 1], sign: -1)
(perm: @[3, 0, 2, 1], sign: 1)
(perm: @[0, 3, 2, 1], sign: -1)
(perm: @[2, 3, 0, 1], sign: 1)
(perm: @[3, 2, 0, 1], sign: -1)
(perm: @[3, 2, 1, 0], sign: 1)
(perm: @[2, 3, 1, 0], sign: -1)
(perm: @[1, 3, 2, 0], sign: 1)
(perm: @[3, 1, 2, 0], sign: -1)
(perm: @[2, 1, 3, 0], sign: 1)
(perm: @[1, 2, 3, 0], sign: -1)
```



## Perl


===S-J-T Based===

```perl

#!perl
use strict;
use warnings;

# This code uses "Even's Speedup," as described on
# the Wikipedia page about the Steinhaus–Johnson–
# Trotter algorithm.

# Any resemblance between this code and the Python
# code elsewhere on the page is purely a coincidence,
# caused by them both implementing the same algorithm.

# The code was written to be read relatively easily
# while demonstrating some common perl idioms.

sub perms(&@) {
   my $callback = shift;
   my @perm = map [$_, -1], @_;
   $perm[0][1] = 0;

   my $sign = 1;
   while( ) {
      $callback->($sign, map $_->[0], @perm);
      $sign *= -1;

      my ($chosen, $index) = (-1, -1);
      for my $i ( 0 .. $#perm ) {
         ($chosen, $index) = ($perm[$i][0], $i)
           if $perm[$i][1] and $perm[$i][0] > $chosen;
      }
      return if $index == -1;

      my $direction = $perm[$index][1];
      my $next = $index + $direction;

      @perm[ $index, $next ] = @perm[ $next, $index ];

      if( $next <= 0 or $next >= $#perm ) {
         $perm[$next][1] = 0;
      } elsif( $perm[$next + $direction][0] > $chosen ) {
         $perm[$next][1] = 0;
      }

      for my $i ( 0 .. $next - 1 ) {
         $perm[$i][1] = +1 if $perm[$i][0] > $chosen;
      }
      for my $i ( $next + 1 .. $#perm ) {
         $perm[$i][1] = -1 if $perm[$i][0] > $chosen;
      }
   }
}

my $n = shift(@ARGV) || 4;

perms {
   my ($sign, @perm) = @_;
   print "[", join(", ", @perm), "]";
   print $sign < 0 ? " => -1\n" : " => +1\n";   
} 1 .. $n;

```

```txt

[1, 2, 3, 4] => +1
[1, 2, 4, 3] => -1
[1, 4, 2, 3] => +1
[4, 1, 2, 3] => -1
[4, 1, 3, 2] => +1
[1, 4, 3, 2] => -1
[1, 3, 4, 2] => +1
[1, 3, 2, 4] => -1
[3, 1, 2, 4] => +1
[3, 1, 4, 2] => -1
[3, 4, 1, 2] => +1
[4, 3, 1, 2] => -1
[4, 3, 2, 1] => +1
[3, 4, 2, 1] => -1
[3, 2, 4, 1] => +1
[3, 2, 1, 4] => -1
[2, 3, 1, 4] => +1
[2, 3, 4, 1] => -1
[2, 4, 3, 1] => +1
[4, 2, 3, 1] => -1
[4, 2, 1, 3] => +1
[2, 4, 1, 3] => -1
[2, 1, 4, 3] => +1
[2, 1, 3, 4] => -1

```



###  Alternative Iterative version 

This is based on the perl6 recursive version, but without recursion.


```perl
#!perl
use strict;
use warnings;

sub perms {
   my ($xx) = (shift);
   my @perms = ([+1]);
   for my $x ( 1 .. $xx ) {
      my $sign = -1;
      @perms = map {
         my ($s, @p) = @$_;
         map [$sign *= -1, @p[0..$_-1], $x, @p[$_..$#p]],
            $s < 0 ? 0 .. @p : reverse 0 .. @p;
      } @perms;
   }
   @perms;
}

my $n = shift() || 4;

for( perms($n) ) {
   my $s = shift @$_;
   $s = '+1' if $s > 0;
   print "[", join(", ", @$_), "] => $s\n";
}

```

The output is the same as the first perl solution.


## Perl 6



###  Recursive 

```perl6
sub insert($x, @xs) { ([flat @xs[0 ..^ $_], $x, @xs[$_ .. *]] for 0 .. +@xs) }
sub order($sg, @xs) { $sg > 0 ?? @xs !! @xs.reverse }
 
multi perms([]) {
    [] => +1
}
 
multi perms([$x, *@xs]) {
    perms(@xs).map({ |order($_.value, insert($x, $_.key)) }) Z=> |(+1,-1) xx *
}
 
.say for perms([0..2]);
```


```txt
[0 1 2] => 1
[1 0 2] => -1
[1 2 0] => 1
[2 1 0] => -1
[2 0 1] => 1
[0 2 1] => -1
```



## Phix

Ad-hoc recursive solution, not (knowingly) based on any given algorithm, but instead on achieving the desired pattern.

Only once finished did I properly grasp that odd/even permutation idea, and that it is very nearly the same algorithm.

Only difference is my version directly calculates where to insert p, without using the parity (which I added in last).

```Phix
function spermutations(integer p, integer i)
-- generate the i'th permutation of [1..p]:
-- first obtain the appropriate permutation of [1..p-1],
-- then insert p/move it down k(=0..p-1) places from the end.
    integer k = mod(i-1,2*p)
    if k>=p then k=2*p-1-k  end if
    sequence res
    integer parity
    if p>1 then
        {res,parity} = spermutations(p-1,floor((i-1)/p)+1)
        res = res[1..length(res)-k]&p&res[length(res)-k+1..$]
    else
        res = {1}
    end if
    return {res,iff(and_bits(i,1)?1:-1)}
end function

for p=1 to 4 do
    printf(1,"==%d==\n",p)
    for i=1 to factorial(p) do
        ?{i,spermutations(p,i)}
    end for
end for
```

```txt

"started"
==1==
{1,{{1},1}}
==2==
{1,{{1,2},1}}
{2,{{2,1},-1}}
==3==
{1,{{1,2,3},1}}
{2,{{1,3,2},-1}}
{3,{{3,1,2},1}}
{4,{{3,2,1},-1}}
{5,{{2,3,1},1}}
{6,{{2,1,3},-1}}
==4==
{1,{{1,2,3,4},1}}
{2,{{1,2,4,3},-1}}
{3,{{1,4,2,3},1}}
{4,{{4,1,2,3},-1}}
{5,{{4,1,3,2},1}}
{6,{{1,4,3,2},-1}}
{7,{{1,3,4,2},1}}
{8,{{1,3,2,4},-1}}
{9,{{3,1,2,4},1}}
{10,{{3,1,4,2},-1}}
{11,{{3,4,1,2},1}}
{12,{{4,3,1,2},-1}}
{13,{{4,3,2,1},1}}
{14,{{3,4,2,1},-1}}
{15,{{3,2,4,1},1}}
{16,{{3,2,1,4},-1}}
{17,{{2,3,1,4},1}}
{18,{{2,3,4,1},-1}}
{19,{{2,4,3,1},1}}
{20,{{4,2,3,1},-1}}
{21,{{4,2,1,3},1}}
{22,{{2,4,1,3},-1}}
{23,{{2,1,4,3},1}}
{24,{{2,1,3,4},-1}}

```



## PicoLisp


```PicoLisp
(let
   (N 4
      L
      (mapcar
         '((I) (list I 0))
         (range 1 N) ) )
   (for I L
      (printsp (car I)) )
   (prinl)
   (while
      # find the lagest mobile integer
      (setq
         X
         (maxi
            '((I) (car (get L (car I))))
            (extract
               '((I J)
                  (let? Y
                     (get
                        L
                        ((if (=0 (cadr I)) dec inc) J) )
                     (when (> (car I) (car Y))
                        (list J (cadr I)) ) ) )
               L
               (range 1 N) ) )
         Y (get L (car X)) )
      # swap integer and adjacent int it is looking at
      (xchg
         (nth L (car X))
         (nth
            L
            ((if (=0 (cadr X)) dec inc) (car X)) ) )
      # reverse direction of all ints large than our
      (for I L
         (when (< (car Y) (car I))
            (set (cdr I)
               (if (=0 (cadr I)) 1 0) ) ) )
      # print current positions
      (for I L
         (printsp (car I)) )
      (prinl) ) )
(bye)
```



## PowerShell


```PowerShell

function permutation ($array) {
    function sign($A) {
        $size = $A.Count
        $sign = 1
        for($i = 0; $i -lt $size; $i++) {
            for($j = $i+1; $j -lt $size ; $j++) {
                if($A[$j] -lt $A[$i]) { $sign *= -1}
            }
        }
        $sign
    }
    function generate($n, $A, $i1, $i2, $cnt) {
        if($n -eq 1) {
            if($cnt -gt 0) {
                "$A -- swapped positions: $i1 $i2 -- sign = $(sign $A)`n"
            } else {
                "$A -- sign = $(sign $A)`n"
            }
        }
        else{
            for( $i = 0; $i -lt ($n - 1); $i += 1) {
                generate ($n - 1) $A $i1 $i2 $cnt
                if($n % 2 -eq 0){
                    $i1, $i2 = $i, ($n-1)
                    $A[$i1], $A[$i2] = $A[$i2], $A[$i1]
                    $cnt = 1
                }
                else{
                    $i1, $i2 = 0, ($n-1)
                    $A[$i1], $A[$i2] = $A[$i2], $A[$i1]
                    $cnt = 1
                }
            }
            generate ($n - 1) $A $i1 $i2 $cnt
        }
    }
    $n = $array.Count
    if($n -gt 0) {
        (generate $n $array  0 ($n-1) 0)
    } else {$array}
}
permutation @(1,2,3,4)

```

<b>Output:</b>

```txt

1 2 3 4 -- sign = 1

2 1 3 4 -- swapped positions: 0 1 -- sign = -1

3 1 2 4 -- swapped positions: 0 2 -- sign = 1

1 3 2 4 -- swapped positions: 0 1 -- sign = -1

2 3 1 4 -- swapped positions: 0 2 -- sign = 1

3 2 1 4 -- swapped positions: 0 1 -- sign = -1

4 2 1 3 -- swapped positions: 0 3 -- sign = 1

2 4 1 3 -- swapped positions: 0 1 -- sign = -1

1 4 2 3 -- swapped positions: 0 2 -- sign = 1

4 1 2 3 -- swapped positions: 0 1 -- sign = -1

2 1 4 3 -- swapped positions: 0 2 -- sign = 1

1 2 4 3 -- swapped positions: 0 1 -- sign = -1

1 3 4 2 -- swapped positions: 1 3 -- sign = 1

3 1 4 2 -- swapped positions: 0 1 -- sign = -1

4 1 3 2 -- swapped positions: 0 2 -- sign = 1

1 4 3 2 -- swapped positions: 0 1 -- sign = -1

3 4 1 2 -- swapped positions: 0 2 -- sign = 1

4 3 1 2 -- swapped positions: 0 1 -- sign = -1

4 3 2 1 -- swapped positions: 2 3 -- sign = 1

3 4 2 1 -- swapped positions: 0 1 -- sign = -1

2 4 3 1 -- swapped positions: 0 2 -- sign = 1

4 2 3 1 -- swapped positions: 0 1 -- sign = -1

3 2 4 1 -- swapped positions: 0 2 -- sign = 1

2 3 4 1 -- swapped positions: 0 1 -- sign = -1

```



## Python


### Python: iterative

When saved in a file called spermutations.py it is used in the Python example to the [[Matrix arithmetic#Python|Matrix arithmetic]] task and so any changes here should also be reflected and checked in that task example too.


```python
from operator import itemgetter
 
DEBUG = False # like the built-in __debug__
 
def spermutations(n):
    """permutations by swapping. Yields: perm, sign"""
    sign = 1
    p = [[i, 0 if i == 0 else -1] # [num, direction]
         for i in range(n)]
 
    if DEBUG: print ' #', p
    yield tuple(pp[0] for pp in p), sign
 
    while any(pp[1] for pp in p): # moving
        i1, (n1, d1) = max(((i, pp) for i, pp in enumerate(p) if pp[1]),
                           key=itemgetter(1))
        sign *= -1
        if d1 == -1:
            # Swap down
            i2 = i1 - 1
            p[i1], p[i2] = p[i2], p[i1]
            # If this causes the chosen element to reach the First or last
            # position within the permutation, or if the next element in the
            # same direction is larger than the chosen element:
            if i2 == 0 or p[i2 - 1][0] > n1:
                # The direction of the chosen element is set to zero
                p[i2][1] = 0
        elif d1 == 1:
            # Swap up
            i2 = i1 + 1
            p[i1], p[i2] = p[i2], p[i1]
            # If this causes the chosen element to reach the first or Last
            # position within the permutation, or if the next element in the
            # same direction is larger than the chosen element:
            if i2 == n - 1 or p[i2 + 1][0] > n1:
                # The direction of the chosen element is set to zero
                p[i2][1] = 0
        if DEBUG: print ' #', p
        yield tuple(pp[0] for pp in p), sign
 
        for i3, pp in enumerate(p):
            n3, d3 = pp
            if n3 > n1:
                pp[1] = 1 if i3 < i2 else -1
                if DEBUG: print ' # Set Moving'
 
 
if __name__ == '__main__':
    from itertools import permutations
 
    for n in (3, 4):
        print '\nPermutations and sign of %i items' % n
        sp = set()
        for i in spermutations(n):
            sp.add(i[0])
            print('Perm: %r Sign: %2i' % i)
            #if DEBUG: raw_input('?')
        # Test
        p = set(permutations(range(n)))
        assert sp == p, 'Two methods of generating permutations do not agree'
```

```txt
Permutations and sign of 3 items
Perm: (0, 1, 2) Sign:  1
Perm: (0, 2, 1) Sign: -1
Perm: (2, 0, 1) Sign:  1
Perm: (2, 1, 0) Sign: -1
Perm: (1, 2, 0) Sign:  1
Perm: (1, 0, 2) Sign: -1

Permutations and sign of 4 items
Perm: (0, 1, 2, 3) Sign:  1
Perm: (0, 1, 3, 2) Sign: -1
Perm: (0, 3, 1, 2) Sign:  1
Perm: (3, 0, 1, 2) Sign: -1
Perm: (3, 0, 2, 1) Sign:  1
Perm: (0, 3, 2, 1) Sign: -1
Perm: (0, 2, 3, 1) Sign:  1
Perm: (0, 2, 1, 3) Sign: -1
Perm: (2, 0, 1, 3) Sign:  1
Perm: (2, 0, 3, 1) Sign: -1
Perm: (2, 3, 0, 1) Sign:  1
Perm: (3, 2, 0, 1) Sign: -1
Perm: (3, 2, 1, 0) Sign:  1
Perm: (2, 3, 1, 0) Sign: -1
Perm: (2, 1, 3, 0) Sign:  1
Perm: (2, 1, 0, 3) Sign: -1
Perm: (1, 2, 0, 3) Sign:  1
Perm: (1, 2, 3, 0) Sign: -1
Perm: (1, 3, 2, 0) Sign:  1
Perm: (3, 1, 2, 0) Sign: -1
Perm: (3, 1, 0, 2) Sign:  1
Perm: (1, 3, 0, 2) Sign: -1
Perm: (1, 0, 3, 2) Sign:  1
Perm: (1, 0, 2, 3) Sign: -1
```



### Python: recursive

After spotting the pattern of highest number being inserted into each perm of lower numbers from right to left, then left to right, I developed this recursive function:

```python
def s_permutations(seq):
    def s_perm(seq):
        if not seq:
            return [[]]
        else:
            new_items = []
            for i, item in enumerate(s_perm(seq[:-1])):
                if i % 2:
                    # step up
                    new_items += [item[:i] + seq[-1:] + item[i:]
                                  for i in range(len(item) + 1)]
                else:
                    # step down
                    new_items += [item[:i] + seq[-1:] + item[i:]
                                  for i in range(len(item), -1, -1)]
            return new_items

    return [(tuple(item), -1 if i % 2 else 1)
            for i, item in enumerate(s_perm(seq))]
```


The output is the same as before except it is a list of all results rather than yielding each result from a generator function.


### Python: Iterative version of the recursive

Replacing the recursion in the example above produces this iterative version function:

```python
def s_permutations(seq):
    items = [[]]
    for j in seq:
        new_items = []
        for i, item in enumerate(items):
            if i % 2:
                # step up
                new_items += [item[:i] + [j] + item[i:]
                              for i in range(len(item) + 1)]
            else:
                # step down
                new_items += [item[:i] + [j] + item[i:]
                              for i in range(len(item), -1, -1)]
        items = new_items

    return [(tuple(item), -1 if i % 2 else 1)
            for i, item in enumerate(items)]
```


The output is the same as before and is a list of all results rather than yielding each result from a generator function.


## Racket


```Racket

#lang racket

(define (add-at l i x)
  (if (zero? i) (cons x l) (cons (car l) (add-at (cdr l) (sub1 i) x))))

(define (permutations l)
  (define (loop l)
    (cond [(null? l) '(())]
          [else (for*/list ([(p i) (in-indexed (loop (cdr l)))]
                            [i ((if (odd? i) identity reverse)
                                (range (add1 (length p))))])
                  (add-at p i (car l)))]))
  (for/list ([p (loop (reverse l))] [i (in-cycle '(1 -1))]) (cons i p)))

(define (show-permutations l)
  (printf "Permutations of ~s:\n" l)
  (for ([p (permutations l)])
    (printf "  ~a (~a)\n" (apply ~a (add-between (cdr p) ", ")) (car p))))

(for ([n (in-range 3 5)]) (show-permutations (range n)))

```


```txt

Permutations of (0 1 2):
  0, 1, 2 (1)
  0, 2, 1 (-1)
  2, 0, 1 (1)
  2, 1, 0 (-1)
  1, 2, 0 (1)
  1, 0, 2 (-1)
Permutations of (0 1 2 3):
  0, 1, 2, 3 (1)
  0, 1, 3, 2 (-1)
  0, 3, 1, 2 (1)
  3, 0, 1, 2 (-1)
  3, 0, 2, 1 (1)
  0, 3, 2, 1 (-1)
  0, 2, 3, 1 (1)
  0, 2, 1, 3 (-1)
  2, 0, 1, 3 (1)
  2, 0, 3, 1 (-1)
  2, 3, 0, 1 (1)
  3, 2, 0, 1 (-1)
  3, 2, 1, 0 (1)
  2, 3, 1, 0 (-1)
  2, 1, 3, 0 (1)
  2, 1, 0, 3 (-1)
  1, 2, 0, 3 (1)
  1, 2, 3, 0 (-1)
  1, 3, 2, 0 (1)
  3, 1, 2, 0 (-1)
  3, 1, 0, 2 (1)
  1, 3, 0, 2 (-1)
  1, 0, 3, 2 (1)
  1, 0, 2, 3 (-1)

```



## REXX


```rexx
/*REXX program  generates all  permutations  of   N   different objects by  swapping.   */
parse arg things bunch .                         /*obtain optional arguments from the CL*/
if things=='' | things==","  then things=4       /*Not specified?  Then use the default.*/
if bunch =='' | bunch ==","  then bunch =things  /* "      "         "   "   "     "    */
call permSets things, bunch                      /*invoke permutations by swapping sub. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!:        procedure;  !=1;        do j=2  to arg(1);    !=!*j;     end;           return !
/*──────────────────────────────────────────────────────────────────────────────────────*/
permSets: procedure; parse arg x,y               /*take   X  things   Y   at a time.    */
          !.=0;      pad=left('', x*y)           /*X can't be > length of below str (62)*/
          z=left('123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ', x);  q=z
          #=1                                    /*the number of permutations  (so far).*/
          !.z=1;    s=1;   times=!(x) % !(x-y)   /*calculate (#) TIMES  using factorial.*/
          w=max(length(z), length('permute') )   /*maximum width of  Z and also PERMUTE.*/
          say center('permutations for '   x   ' things taken '   y   " at a time",60,'═')
          say
          say   pad    'permutation'       center("permute", w, '─')         "sign"
          say   pad    '───────────'       center("───────", w, '─')         "────"
          say   pad    center(#, 11)       center(z        , w)              right(s, 4-1)

             do $=1   until  #==times            /*perform permutation until # of times.*/
               do   k=1    for x-1               /*step thru things for  things-1 times.*/
                 do m=k+1  to  x;      ?=        /*this method doesn't use  adjacency.  */
                     do n=1  for x               /*build the new permutation by swapping*/
                     if n\==k & n\==m  then               ? =  ?  ||  substr(z, n, 1)
                                       else if n==k  then ? =  ?  ||  substr(z, m, 1)
                                                     else ? =  ?  ||  substr(z, k, 1)
                     end   /*n*/
                 z=?                             /*save this permutation for next swap. */
                 if !.?  then iterate m          /*if defined before, then try next one.*/
                 _=0                             /* [↓]  count number of swapped symbols*/
                    do d=1  for x  while $\==1;  _= _ + (substr(?,d,1)\==substr(prev,d,1))
                    end   /*d*/
                 if _>2  then do;        _=z
                              a=$//x+1;  q=q + _ /* [← ↓]  this swapping tries adjacency*/
                              b=q//x+1;  if b==a  then b=a + 1;       if b>x  then b=a - 1
                              z=overlay( substr(z,b,1), overlay( substr(z,a,1), _, b),  a)
                              iterate $          /*now, try this particular permutation.*/
                              end
                 #=#+1;  s= -s;   say pad   center(#, 11)    center(?, w)    right(s, 4-1)
                 !.?=1;  prev=?;      iterate $  /*now, try another swapped permutation.*/
                 end   /*m*/
               end     /*k*/
             end       /*$*/
          return                                 /*we're all finished with permutating. */
```

```txt

══════permutations for  4  things taken  4  at a time═══════

                 permutation permute sign
                 ─────────── ─────── ────
                      1       1234     1
                      2       2134    -1
                      3       3124     1
                      4       1324    -1
                      5       1342     1
                      6       3142    -1
                      7       4132     1
                      8       1432    -1
                      9       2431     1
                     10       4231    -1
                     11       4321     1
                     12       3421    -1
                     13       3241     1
                     14       2341    -1
                     15       2314     1
                     16       3214    -1
                     17       3412     1
                     18       4312    -1
                     19       4213     1
                     20       2413    -1
                     21       2143     1
                     22       1243    -1
                     23       1423     1
                     24       4123    -1

```



## Ruby

```ruby
def perms(n)
  p = Array.new(n+1){|i| -i}
  s = 1
  loop do
    yield p[1..-1].map(&:abs), s
    k = 0
    for i in 2..n
      k = i if p[i] < 0 and p[i].abs > p[i-1].abs and p[i].abs > p[k].abs
    end
    for i in 1...n
      k = i if p[i] > 0 and p[i].abs > p[i+1].abs and  p[i].abs > p[k].abs
    end
    break if k.zero?
    for i in 1..n
      p[i] *= -1 if p[i].abs > p[k].abs
    end
    i = k + (p[k] <=> 0)
    p[k], p[i] = p[i], p[k]
    s = -s
  end
end

for i in 3..4
  perms(i){|perm, sign| puts "Perm: #{perm}  Sign: #{sign}"}
  puts
end
```

```txt

Perm: [1, 2, 3]  Sign: 1
Perm: [1, 3, 2]  Sign: -1
Perm: [3, 1, 2]  Sign: 1
Perm: [3, 2, 1]  Sign: -1
Perm: [2, 3, 1]  Sign: 1
Perm: [2, 1, 3]  Sign: -1

Perm: [1, 2, 3, 4]  Sign: 1
Perm: [1, 2, 4, 3]  Sign: -1
Perm: [1, 4, 2, 3]  Sign: 1
Perm: [4, 1, 2, 3]  Sign: -1
Perm: [4, 1, 3, 2]  Sign: 1
Perm: [1, 4, 3, 2]  Sign: -1
Perm: [1, 3, 4, 2]  Sign: 1
Perm: [1, 3, 2, 4]  Sign: -1
Perm: [3, 1, 2, 4]  Sign: 1
Perm: [3, 1, 4, 2]  Sign: -1
Perm: [3, 4, 1, 2]  Sign: 1
Perm: [4, 3, 1, 2]  Sign: -1
Perm: [4, 3, 2, 1]  Sign: 1
Perm: [3, 4, 2, 1]  Sign: -1
Perm: [3, 2, 4, 1]  Sign: 1
Perm: [3, 2, 1, 4]  Sign: -1
Perm: [2, 3, 1, 4]  Sign: 1
Perm: [2, 3, 4, 1]  Sign: -1
Perm: [2, 4, 3, 1]  Sign: 1
Perm: [4, 2, 3, 1]  Sign: -1
Perm: [4, 2, 1, 3]  Sign: 1
Perm: [2, 4, 1, 3]  Sign: -1
Perm: [2, 1, 4, 3]  Sign: 1
Perm: [2, 1, 3, 4]  Sign: -1

```



## Scala


```Scala
object JohnsonTrotter extends App {

  private def perm(n: Int): Unit = {
    val p = new Array[Int](n) // permutation
    val pi = new Array[Int](n) // inverse permutation
    val dir = new Array[Int](n) // direction = +1 or -1

    def perm(n: Int, p: Array[Int], pi: Array[Int], dir: Array[Int]): Unit = {
      if (n >= p.length) for (aP <- p) print(aP)
      else {
        perm(n + 1, p, pi, dir)
        for (i <- 0 until n) { // swap
          printf("   (%d %d)\n", pi(n), pi(n) + dir(n))
          val z = p(pi(n) + dir(n))
          p(pi(n)) = z
          p(pi(n) + dir(n)) = n
          pi(z) = pi(n)
          pi(n) = pi(n) + dir(n)
          perm(n + 1, p, pi, dir)
        }
        dir(n) = -dir(n)
      }
    }

    for (i <- 0 until n) {
      dir(i) = -1
      p(i) = i
      pi(i) = i
    }
    perm(0, p, pi, dir)
    print("   (0 1)\n")
  }

  perm(4)

}
```

{{Out}}See it in running in your browser by [https://scastie.scala-lang.org/DdM4xnUnQ2aNGP481zwcrw Scastie (JVM)].

## Sidef

```ruby
func perms(n) {
   var perms = [[+1]]
   for x in (1..n) {
      var sign = -1
      perms = gather {
        for s,*p in perms {
          var r = (0 .. p.len)
          take((s < 0 ? r : r.flip).map {|i|
            [sign *= -1, p[^i], x, p[i..p.end]]
          }...)
        }
      }
   }
   perms
}

var n = 4
for p in perms(n) {
    var s = p.shift
    s > 0 && (s = '+1')
    say "#{p} => #{s}"
}
```


```txt

[1, 2, 3, 4] => +1
[1, 2, 4, 3] => -1
[1, 4, 2, 3] => +1
[4, 1, 2, 3] => -1
[4, 1, 3, 2] => +1
[1, 4, 3, 2] => -1
[1, 3, 4, 2] => +1
[1, 3, 2, 4] => -1
[3, 1, 2, 4] => +1
[3, 1, 4, 2] => -1
[3, 4, 1, 2] => +1
[4, 3, 1, 2] => -1
[4, 3, 2, 1] => +1
[3, 4, 2, 1] => -1
[3, 2, 4, 1] => +1
[3, 2, 1, 4] => -1
[2, 3, 1, 4] => +1
[2, 3, 4, 1] => -1
[2, 4, 3, 1] => +1
[4, 2, 3, 1] => -1
[4, 2, 1, 3] => +1
[2, 4, 1, 3] => -1
[2, 1, 4, 3] => +1
[2, 1, 3, 4] => -1

```



## Tcl


```tcl
# A simple swap operation
proc swap {listvar i1 i2} {
    upvar 1 $listvar l
    set tmp [lindex $l $i1]
    lset l $i1 [lindex $l $i2]
    lset l $i2 $tmp
}

proc permswap {n v1 v2 body} {
    upvar 1 $v1 perm $v2 sign

    # Initialize
    set sign -1
    for {set i 0} {$i < $n} {incr i} {
	lappend items $i
	lappend dirs -1
    }

    while 1 {
	# Report via callback
	set perm $items
	set sign [expr {-$sign}]
	uplevel 1 $body

	# Find the largest mobile integer (lmi) and its index (idx)
	set i [set idx -1]
	foreach item $items dir $dirs {
	    set j [expr {[incr i] + $dir}]
	    if {$j < 0 || $j >= [llength $items]} continue
	    if {$item > [lindex $items $j] && ($idx == -1 || $item > $lmi)} {
		set lmi $item
		set idx $i
	    }
	}

	# If none, we're done
	if {$idx == -1} break

	# Swap the largest mobile integer with "what it is looking at"
	set nextIdx [expr {$idx + [lindex $dirs $idx]}]
	swap items $idx $nextIdx
	swap dirs $idx $nextIdx

	# Reverse directions on larger integers
	set i -1
	foreach item $items dir $dirs {
	    lset dirs [incr i] [expr {$item > $lmi ? -$dir : $dir}]
	}
    }
}
```

Demonstrating:

```tcl
permswap 4 p s {
    puts "$s\t$p"
}
```

```txt

1	0 1 2 3
-1	0 1 3 2
1	0 3 1 2
-1	3 0 1 2
1	3 0 2 1
-1	0 3 2 1
1	0 2 3 1
-1	0 2 1 3
1	2 0 1 3
-1	2 0 3 1
1	2 3 0 1
-1	3 2 0 1
1	3 2 1 0
-1	2 3 1 0
1	2 1 3 0
-1	2 1 0 3
1	1 2 0 3
-1	1 2 3 0
1	1 3 2 0
-1	3 1 2 0
1	3 1 0 2
-1	1 3 0 2
1	1 0 3 2
-1	1 0 2 3

```



## XPL0

Translation of BBC BASIC example, which uses the Johnson-Trotter algorithm.

```XPL0
include c:\cxpl\codes;

proc PERMS(N);
int  N;                         \number of elements
int  I, K, S, T, P;
[P:= Reserve((N+1)*4);
for I:= 0 to N do P(I):= -I;    \initialize facing left (also set P(0)=0)
S:= 1;
repeat  Text(0, "Perm: [ ");
        for I:= 1 to N do
                [IntOut(0, abs(P(I)));  ChOut(0, ^ )];
        Text(0, "] Sign: ");  IntOut(0, S);  CrLf(0);

        K:= 0;                  \find largest mobile element
        for I:= 2 to N do                         \for left-facing elements
            if P(I) < 0 and
                abs(P(I)) > abs(P(I-1)) and       \ greater than neighbor
                abs(P(I)) > abs(P(K)) then K:= I; \ get largest element
        for I:= 1 to N-1 do                       \for right-facing elements
            if P(I) > 0 and
                abs(P(I)) > abs(P(I+1)) and       \ greater than neighbor
                abs(P(I)) > abs(P(K)) then K:= I; \ get largest element
        if K # 0 then           \mobile element found
           [for I:= 1 to N do   \reverse elements > K
                if abs(P(I)) > abs(P(K)) then P(I):= P(I)*-1;
            I:= K + (if P(K)<0 then -1 else 1);
            T:= P(K);  P(K):= P(I);  P(I):= T;    \swap K with element looked at
            S:= -S;             \alternate signs
            ];
until   K = 0;                  \no mobile element remains
];

[PERMS(3);
CrLf(0);
PERMS(4);
]
```


```txt

Perm: [ 1 2 3 ] Sign: 1
Perm: [ 1 3 2 ] Sign: -1
Perm: [ 3 1 2 ] Sign: 1
Perm: [ 3 2 1 ] Sign: -1
Perm: [ 2 3 1 ] Sign: 1
Perm: [ 2 1 3 ] Sign: -1

Perm: [ 1 2 3 4 ] Sign: 1
Perm: [ 1 2 4 3 ] Sign: -1
Perm: [ 1 4 2 3 ] Sign: 1
Perm: [ 4 1 2 3 ] Sign: -1
Perm: [ 4 1 3 2 ] Sign: 1
Perm: [ 1 4 3 2 ] Sign: -1
Perm: [ 1 3 4 2 ] Sign: 1
Perm: [ 1 3 2 4 ] Sign: -1
Perm: [ 3 1 2 4 ] Sign: 1
Perm: [ 3 1 4 2 ] Sign: -1
Perm: [ 3 4 1 2 ] Sign: 1
Perm: [ 4 3 1 2 ] Sign: -1
Perm: [ 4 3 2 1 ] Sign: 1
Perm: [ 3 4 2 1 ] Sign: -1
Perm: [ 3 2 4 1 ] Sign: 1
Perm: [ 3 2 1 4 ] Sign: -1
Perm: [ 2 3 1 4 ] Sign: 1
Perm: [ 2 3 4 1 ] Sign: -1
Perm: [ 2 4 3 1 ] Sign: 1
Perm: [ 4 2 3 1 ] Sign: -1
Perm: [ 4 2 1 3 ] Sign: 1
Perm: [ 2 4 1 3 ] Sign: -1
Perm: [ 2 1 4 3 ] Sign: 1
Perm: [ 2 1 3 4 ] Sign: -1

```



## zkl

```zkl
fcn permute(seq)
{
   insertEverywhere := fcn(x,list){ //(x,(a,b))-->((x,a,b),(a,x,b),(a,b,x))
      (0).pump(list.len()+1,List,'wrap(n){list[0,n].extend(x,list[n,*]) })};
   insertEverywhereB := fcn(x,t){ //--> insertEverywhere().reverse()
      [t.len()..-1,-1].pump(t.len()+1,List,'wrap(n){t[0,n].extend(x,t[n,*])})};

   seq.reduce('wrap(items,x){
      f := Utils.Helpers.cycle(insertEverywhereB,insertEverywhere);
      items.pump(List,'wrap(item){f.next()(x,item)},
	      T.fp(Void.Write,Void.Write));
   },T(T));
}
```

A cycle of two "build list" functions is used to insert x forward or reverse. reduce loops over the items and retains the enlarging list of permuations. pump loops over the existing set of permutations and inserts/builds the next set (into a list sink). (Void.Write,Void.Write,list) is a sentinel that says to write the contents of the list to the sink (ie sink.extend(list)). T.fp is a partial application of ROList.create (read only list) and the parameters VW,VW. It will be called (by pump) with a list of lists --> T.create(VM,VM,list) --> list

```zkl
p := permute(T(1,2,3));
p.println();

p := permute([1..4]);
p.len().println();
p.toString(*).println()
```

```txt

L(L(1,2,3),L(1,3,2),L(3,1,2),L(3,2,1),L(2,3,1),L(2,1,3))

24
L(
L(1,2,3,4), L(1,2,4,3), L(1,4,2,3), L(4,1,2,3), L(4,1,3,2), L(1,4,3,2),
L(1,3,4,2), L(1,3,2,4), L(3,1,2,4), L(3,1,4,2), L(3,4,1,2), L(4,3,1,2), 
L(4,3,2,1), L(3,4,2,1), L(3,2,4,1), L(3,2,1,4), L(2,3,1,4), L(2,3,4,1), 
L(2,4,3,1), L(4,2,3,1), L(4,2,1,3), L(2,4,1,3), L(2,1,4,3), L(2,1,3,4) )

```

An iterative, lazy version, which is handy as the number of permutations is n!. Uses "Even's Speedup" as described in the Wikipedia article:

```zkl
 fcn [private] _permuteW(seq){	// lazy version
   N:=seq.len(); NM1:=N-1;
   ds:=(0).pump(N,List,T(Void,-1)).copy(); ds[0]=0; // direction to move e: -1,0,1
   es:=(0).pump(N,List).copy();  // enumerate seq

   while(1) {
      vm.yield(es.pump(List,seq.__sGet));

      // find biggest e with d!=0
      reg i=Void, c=-1;
      foreach n in (N){ if(ds[n] and es[n]>c) { c=es[n]; i=n; } }
      if(Void==i) return();

      d:=ds[i]; j:=i+d;
      es.swap(i,j); ds.swap(i,j);	// d tracks e
      if(j==NM1 or j==0 or es[j+d]>c) ds[j]=0;
      foreach e in (N){ if(es[e]>c) ds[e]=(i-e).sign }
   } 
} 

fcn permuteW(seq) { Utils.Generator(_permuteW,seq) }
```


```zkl
foreach p in (permuteW(T("a","b","c"))){ println(p) }
```

```txt

L("a","b","c")
L("a","c","b")
L("c","a","b")
L("c","b","a")
L("b","c","a")
L("b","a","c")

```

