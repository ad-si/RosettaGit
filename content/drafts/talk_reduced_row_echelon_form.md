+++
title = "Talk:Reduced row echelon form"
description = ""
date = 2016-03-12T03:20:24Z
aliases = []
[extra]
id = 4131
[taxonomies]
categories = []
tags = []
+++

=="Break" vs. "return" bug==

The original author of the Python example mistakenly translated the keyword <code>stop</code> that appears in the [http://en.wikipedia.org/wiki/Row_echelon_form#Pseudocode Wikipedia pseudocode] as <code>break</code> rather than the correct <code>return</code>. This created a control-flow bug that didn't manifest itself when the program was run on the example matrix given in the task description, but did cause an exception if the program was run on, e.g.,

```txt

 1  2  3  4  3  1
 2  4  6  2  6  2
 3  6 18  9  9 -6
 4  8 12 10 12  4
 5 10 24 11 15 -4

```

I noticed and fixed the bug a couple of days ago, but it seems that several of the other examples written before then (being, by and large, translations from the Python) copied the bug. Hence, I've marked all the examples that looked like they might have this bug with the needs-review template. Note that I erred on the side of false positives. That is, I'm pretty sure the examples I ''didn't'' mark are bug-free, but some of the ones I did mark may be fine. &mdash;[[User:Underscore|Underscore]] 00:25, 5 May 2009 (UTC)

I ran the ALGOL 68 version and got:
 (( 1.0000,  2.0000,  0.0000,  0.0000,  3.0000,  4.0000), 
  ( 0.0000,  0.0000,  1.0000,  0.0000,  0.0000, -1.0000), 
  ( 0.0000,  0.0000,  0.0000,  1.0000,  0.0000,  0.0000), 
  ( 0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000), 
  ( 0.0000,  0.0000,  0.0000,  0.0000,  0.0000,  0.0000))
Vs the current python version which got:
 1, 2, 0, 0, 3, 4
 0, 0, 1, 0, 0, -1
 0, 0, 0, 1, 0, 0
 0, 0, 0, 0, 0, 0
 0, 0, 0, 0, 0, 0
It looks like - currently- they are both getting the same answer... next step is it to get out a pencil.

[[User:NevilleDNZ|NevilleDNZ]] 01:46, 5 May 2009 (UTC)

== Bug in Common Lisp code ==

There is a bug in find-pivot that causes an infinite loop on some input. Here is a corrected version along with the matrix that results in an infinite loop with the current code. I tested the code with SBCL 1.0.40.0.debian on Ubuntu 10.10 64bit.


```lisp
(defun convert-to-row-echelon-form (matrix)
  (let* ((dimensions (array-dimensions matrix))
	 (row-count (first dimensions))
	 (column-count (second dimensions))
	 (lead 0))
    (labels ((find-pivot (start lead)
	       (let ((i start))
		 (loop 
		    :while (zerop (aref matrix i lead)) 
		    :do (progn
			  (incf i)
			  (when (= i row-count)
			    (setf i start)
			    (incf lead)
			    (when (= lead column-count)
			      (return-from convert-to-row-echelon-form matrix))))
		    :finally (return (values i lead)))))
	     (swap-rows (r1 r2)
	       (loop 
		  :for c :upfrom 0 :below column-count
		  :do (rotatef (aref matrix r1 c) (aref matrix r2 c))))
	     (divide-row (r value) 
	       (loop
		  :for c :upfrom 0 :below column-count
		  :do (setf (aref matrix r c)
			    (/ (aref matrix r c) value)))))
      (loop
	 :for r :upfrom 0 :below row-count
	 :when (<= column-count lead) 
	 :do (return matrix)
	 :do (multiple-value-bind (i nlead) (find-pivot r lead)
	       (setf lead nlead)
	       (swap-rows i r)
	       (divide-row r (aref matrix r lead))
	       (loop 
		  :for i :upfrom 0 :below row-count
		  :when (/= i r)
		  :do (let ((scale (aref matrix i lead)))
			(loop
			   :for c :upfrom 0 :below column-count
			   :do (decf (aref matrix i c)
				     (* scale (aref matrix r c))))))
	       (incf lead))
	 :finally (return matrix)))))



(defvar *M* '(( 1  0  0  0  0  0  1  0  0  0  0 -1  0  0  0  0  0  0)
    	      ( 1  0  0  0  0  0  0  1  0  0  0  0 -1  0  0  0  0  0)
    	      ( 1  0  0  0  0  0  0  0  1  0  0  0  0 -1  0  0  0  0)
    	      ( 0  1  0  0  0  0  1  0  0  0  0  0  0  0 -1  0  0  0)
    	      ( 0  1  0  0  0  0  0  0  1  0  0 -1  0  0  0  0  0  0)
    	      ( 0  1  0  0  0  0  0  0  0  0  1  0  0  0  0  0 -1  0)
    	      ( 0  0  1  0  0  0  1  0  0  0  0  0 -1  0  0  0  0  0)
    	      ( 0  0  1  0  0  0  0  0  0  1  0  0  0  0 -1  0  0  0)
    	      ( 0  0  0  1  0  0  0  1  0  0  0  0  0  0  0 -1  0  0)
    	      ( 0  0  0  1  0  0  0  0  0  1  0  0 -1  0  0  0  0  0)
    	      ( 0  0  0  0  1  0  0  1  0  0  0  0  0 -1  0  0  0  0)
    	      ( 0  0  0  0  1  0  0  0  1  0  0  0  0  0  0  0 -1  0)
    	      ( 0  0  0  0  1  0  0  0  0  0  1  0  0  0  0 -1  0  0)
    	      ( 0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0)
    	      ( 0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0)
    	      ( 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  1)
    	      ( 0  0  0  0  0  1  0  0  0  0  1  0  0  0 -1  0  0  0)))

(defvar *M-array* (make-array (list (length *M*) (length (first *M*))) :initial-contents *M*))

(print (convert-to-row-echelon-form *M-array*))

```


[[User:carlohamalainen|Carlo Hamalainen]] 3:30PM, 6 Dec 2010 (GMT+10)

Fixed on main page.

[[User:carlohamalainen|Carlo Hamalainen]] 9:39AM, 16 Mar 2011 (GMT+10)

== Code doesn't work for singular matrices ==

I tried putting in a singular matrix into the C# code and it is giving divide by zero errors. When I told it to skip when it is dividing by zero, the results keep changing. Also I corrected an error in the C# code that caused the lead value to be out of bounds.
: I won't be able to test it myself, but can you provide a sample matrix that reproduces the problem? --[[User:Short Circuit|Michael Mol]] 03:08, 10 October 2010 (UTC)

:: this gives a divide by zero:
::  1,0,1,0,1,0
::  1,0,1,0,0,1
::  1,0,0,1,1,0
::  1,0,0,1,0,1
::  0,1,0,1,1,0
::  0,1,0,1,0,1
::  0,1,1,0,1,0
::  0,1,1,0,0,1
:: I haven't looked into it in detail, but adding an "if (div != 0)" line before the offending division seems to make it behave reasonably.
::
:: edit: looking into it, the lead-- line seems to deviate from the original algorithm, replacing that with a "return matrix;" seems to fix it.

== New test cases ==

Recently, I found that the earlier Java version did not work as expected due to floating point errors.

I recommend including these in your respecting languages:
<lang>		double matrix_2 [][] = {
			{2, 0, -1, 0, 0},
			{1, 0, 0, -1, 0},
			{3, 0, 0, -2, -1},
			{0, 1, 0, 0, -2},
			{0, 1, -1, 0, 0}
		};

solution:
[[1, 0, 0, 0, -1]
[0, 1, 0, 0, -2]
[0, 0, 1, 0, -2]
[0, 0, 0, 1, -1]
[0, 0, 0, 0, 0]]

		double matrix_3 [][] = {
			{1, 2, 3, 4, 3, 1},
			{2, 4, 6, 2, 6, 2},
			{3, 6, 18, 9, 9, -6},
			{4, 8, 12, 10, 12, 4},
			{5, 10, 24, 11, 15, -4}
		};

solution:
[[1, 2, 0, 0, 3, 4]
[0, 0, 1, 0, 0, -1]
[0, 0, 0, 1, 0, 0]
[0, 0, 0, 0, 0, 0]
[0, 0, 0, 0, 0, 0]]

		double matrix_4 [][] = {
			{0, 1},
			{1, 2},
			{0,5}
		};

solution:
[[1, 0]
[0, 1]
[0, 0]]
```


: It's been ages since I have thought about this task.  If we get differing results, how do we determine which result is "better". --[[User:Rdm|Rdm]] 16:35, 16 January 2012 (UTC)
:: I think matrix three is designed to show up floating point errors. Using the optional left argument for the <code>gauss_jordan</code> verb used in the J solution hints at that and using extended precision gives the desired answer. --[[User:Tikkanz|Tikkanz]] 15:31, 26 January 2012 (UTC)

```j
   1e_14 gauss_jordan mat_3   
1 2 0 0 3          4
0 0 1 0 0         _1
0 0 0 1 0 1.4803e_16
0 0 0 0 0          0
0 0 0 0 0          0
   gauss_jordan x: mat_3
1 2 0 0 3  4
0 0 1 0 0 _1
0 0 0 1 0  0
0 0 0 0 0  0
0 0 0 0 0  0

```


== Still a bug in java version? ==

I tried to run the the java code on matrix

```txt

 1, 2, 3, 4, 3, 1	
 2, 4, 6, 2, 6, 2	
 3, 6,18, 9, 9,-6	
 4, 8,12,10,12, 4	
 5,10,24,11,15,-4

```

which correctly resulted in

```txt

1, 2, 0, 0, 3, 4
0, 0, 1, 0, 0,-1
0, 0, 0, 1, 0, 0
0, 0, 0, 0, 0, 0
0, 0, 0, 0, 0, 0

```

and then applied the code on this resulting matrix which generated an error. It seems that step 1 and step 2 do not recognize the correct pivot column for examples where there exist free variables in front of pivot columns (In the example above the second column of the resulting matrix). I corrected this by changing method isColumnZeroes not to consider the whole column:

```java

public boolean isColumnZeroes(Coordinate a) {
	for (int i = a.row; i < numRows; i++) {
		if (matrix.get(i).get(a.col).doubleValue() != 0.0) {
			return false;
		}
	}

	return true;
}

```

Now it works. All the other examples are also solved correctly. I have to admit that I changed the code so I am not sure if this error also occurs in the original version.

Edit: The new code is also not working, for instance matrix

```txt

 0, 0
 1, 1
-1, 0
 0,-1
 0, 0
 0, 0
 0, 0
 0, 0
 1, 1

```

results in

```txt

 1, 1	
 0, 0	
 0, 1	
 0,-1	
 0, 0	
 0, 0	
 0, 0	
 0, 0	
 0, 0

```

In contrast, the code from http://ic.ucsc.edu/~ptantalo/math21/Winter07/GaussJordan.java works fine even though it does not implement the pivot stuff. Moreover, this code is understandable since it is not using crap functions like "break" and "continue" in loops which make code unreadable and error prone.

== Bug in maxima code ==

For example, it fails with the following matrix:

```txt

1  1  1  1  1
0  1  1  1  1
0  0  0  0  1

```

rref produces:

```txt

1  0  0  0  0
0  1  1  1  1
0  0  0  0  1

```

The leading coefficient in the (3, 5)-th position is not the only nonzero element in its column.
