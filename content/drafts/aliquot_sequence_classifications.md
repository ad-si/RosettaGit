+++
title = "Aliquot sequence classifications"
description = ""
date = 2019-10-06T00:16:20Z
aliases = []
[extra]
id = 18401
[taxonomies]
categories = []
tags = []
+++

{{task}}
An [[wp:Aliquot sequence|aliquot sequence]] of a positive integer K is defined recursively as the first member
being K and subsequent members being the sum of the [[Proper divisors]] of the previous term.

:* If the terms eventually reach 0 then the series for K is said to '''terminate'''.

:
There are several classifications for non termination:
:* If the second term is K then all future terms are also K and so the sequence repeats from the first term with period 1 and K is called '''perfect'''.
:* If the third term ''would'' be repeating K then the sequence repeats with period 2 and K is called '''amicable'''.
:* If the N<sup>th</sup> term ''would'' be repeating K for the first time, with N > 3 then the sequence repeats with period N - 1 and K is called '''sociable'''.
:
Perfect, amicable and sociable numbers eventually repeat the original number K; there are other repetitions...
:* Some K have a sequence that eventually forms a periodic repetition of period 1 but of a number other than K, for example 95 which forms the sequence <code>95, 25, 6, 6, 6, ...</code> such K are called '''aspiring'''.
:* K that have a sequence that eventually forms a periodic repetition of period >= 2 but of a number other than K, for example 562 which forms the sequence <code>562, 284, 220, 284, 220, ...</code> such K are called '''cyclic'''.

:
And finally:
:* Some K form aliquot sequences that are not known to be either terminating or periodic; these K are to be called '''non-terminating'''. 
For the purposes of this task, K is to be classed as non-terminating if it has not been otherwise classed after generating '''16''' terms or if any term of the sequence is greater than 2**47 = 140,737,488,355,328. 


;Task:
# Create routine(s) to generate the aliquot sequence of a positive integer enough to classify it according to the classifications given above.
# Use it to display the classification and sequences of the numbers one to ten inclusive.
# Use it to show the classification and sequences of the following integers, in order:
:: 11, 12, 28, 496, 220, 1184,  12496, 1264460, 790, 909, 562, 1064, 1488, and optionally 15355717786080.

Show all output on this page.


;Related tasks:
*   [[Abundant, deficient and perfect number classifications]]. (Classifications from only the first two members of the whole sequence).
*   [[Proper divisors]]
*   [[Amicable pairs]]





## ALGOL 68

Assumes LONG INT is at least 64 bits, as in Algol 68G.

```algol68
BEGIN
    # aliquot sequence classification                                         #
    # maximum sequence length we consider                                     #
    INT max sequence length = 16;
    # possible classifications                                                #
    STRING         perfect classification    = "perfect        ";
    STRING        amicable classification    = "amicable       ";
    STRING        sociable classification    = "sociable       ";
    STRING        aspiring classification    = "aspiring       ";
    STRING          cyclic classification    = "cyclic         ";
    STRING     terminating classification    = "terminating    ";
    STRING non terminating classification    = "non terminating";
    # structure to hold an aliquot sequence and its classification            #
    MODE ALIQUOT = STRUCT( STRING                              classification
                         , [ 1 : max sequence length ]LONG INT sequence
                         , INT                                 length
                         );
    # maximum value for sequence elements - if any element is more than this, #
    # we assume it is non-teriminating                                        #
    LONG INT max element = 140 737 488 355 328;
    # returns the sum of the proper divisors of n                             #
    OP DIVISORSUM = ( LONG INT n )LONG INT:
       BEGIN
            LONG INT abs n = ABS n;
            IF abs n < 2 THEN
                0 # -1, 0 and 1 have no proper divisors                       #
            ELSE
                # have a number with possible divisors                        #
               LONG INT result := 1; # 1 is always a divisor                  #
               # a FOR loop counter can only be an INT, hence the WHILE loop  #
               LONG INT d      := ENTIER long sqrt( abs n );
               WHILE d > 1 DO
                   IF abs n MOD d = 0 THEN
                       # found another divisor                                #
                       result +:= d;
                       IF d * d /= abs n THEN
                           # add the other divisor                            #
                           result +:= abs n OVER d
                       FI
                   FI;
                   d -:= 1
               OD;
               result
            FI
       END # DIVISORSUM # ;
    # generates the aliquot sequence of the number k and its classification   #
    # at most max elements of the sequence are considered                     #
    OP CLASSIFY = ( LONG INT k )ALIQUOT :
       BEGIN
           ALIQUOT result;
           classification OF result := "non-terminating";
           INT lb = LWB sequence OF result;
           INT ub = UPB sequence OF result;
           ( sequence OF result )[ lb ] := k; # the first element is always k #
           length     OF result         := 1;
           FOR i FROM lb + 1 TO ub DO
               ( sequence OF result )[ i ] := 0
           OD;
           BOOL classified := FALSE;
           LONG INT prev k := k;
           FOR i FROM lb + 1 TO ub WHILE NOT classified DO
               length OF result +:= 1;
               LONG INT next k := ( sequence OF result )[ i ] := DIVISORSUM prev k;
               classified := TRUE;
               IF   next k = 0 THEN # the sequence terminates                 #
                    classification OF result := terminating classification
               ELIF next k > max element THEN # the sequence gets too large   #
                    classification OF result := non terminating classification
               ELIF next k = k THEN # the sequence that returns to k          #
                   classification OF result
                       := IF   i = lb + 1 THEN  perfect classification
                          ELIF i = lb + 2 THEN amicable classification
                          ELSE                 sociable classification
                          FI
               ELIF next k = prev k THEN # the sequence repeats with non-k    #
                   classification OF result := aspiring classification
               ELSE # check for repeating sequence with a period more than 1  #
                   classified := FALSE;
                   FOR prev pos FROM lb TO i - 2 WHILE NOT classified DO
                       IF classified := ( sequence OF result )[ prev pos ] = next k THEN
                           # found a repeatition                              #
                           classification OF result := cyclic classification
                       FI
                   OD
               FI;
               prev k := next k
           OD;
           result
       END # CLASSIFY # ;
    # test cases as per the task                                              #
    []LONG INT test cases =
        (   1,    2,   3,   4,   5,    6,     7,       8,   9,  10
        ,  11,   12,  28, 496, 220, 1184, 12496, 1264460, 790, 909
        , 562, 1064, 1488
        ,  15355717786080
        );
    FOR i FROM LWB test cases TO UPB test cases DO
        LONG INT k   := test cases[ i ];
        ALIQUOT  seq  = CLASSIFY k;
        print( ( whole( k, -14 ), ": ", classification OF seq, ":" ) );
        FOR e FROM LWB sequence OF seq + 1 TO length OF seq DO
            print( ( " ", whole( ( sequence OF seq )[ e ], 0 ) ) )
        OD;
        print( ( newline ) )
    OD
END
```

{{out}}

```txt

             1: terminating    : 0
             2: terminating    : 1 0
             3: terminating    : 1 0
             4: terminating    : 3 1 0
             5: terminating    : 1 0
             6: perfect        : 6
             7: terminating    : 1 0
             8: terminating    : 7 1 0
             9: terminating    : 4 3 1 0
            10: terminating    : 8 7 1 0
            11: terminating    : 1 0
            12: terminating    : 16 15 9 4 3 1 0
            28: perfect        : 28
           496: perfect        : 496
           220: amicable       : 284 220
          1184: amicable       : 1210 1184
         12496: sociable       : 14288 15472 14536 14264 12496
       1264460: sociable       : 1547860 1727636 1305184 1264460
           790: aspiring       : 650 652 496 496
           909: aspiring       : 417 143 25 6 6
           562: cyclic         : 284 220 284
          1064: cyclic         : 1336 1184 1210 1184
          1488: non-terminating: 2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384
15355717786080: non terminating: 44534663601120 144940087464480

```



## AWK



```awk

#!/bin/gawk -f
function sumprop(num,   i,sum,root) {
if (num == 1) return 0
sum=1
root=sqrt(num)
for ( i=2; i < root; i++) {
    if (num % i == 0 )
    { 
    sum = sum + i + num/i
    }
    }
if (num % root == 0) 
   {
    sum = sum + root
   }    
return sum
}
function class(k,    oldk,newk,seq){
# first term
oldk = k
seq = " "
# second term
newk = sumprop(oldk)
oldk = newk
seq = seq " " newk
if (newk == 0) return "terminating " seq
if (newk == k) return "perfect " seq
#  third term
newk = sumprop(oldk)
oldk = newk
seq = seq " " newk
if (newk == 0) return "terminating " seq
if (newk == k) return "amicable " seq
for (t=4; t<17; t++) {
newk = sumprop(oldk)
seq = seq " " newk
if (newk == 0) return "terminating " seq
if (newk == k) return "sociable (period " t-1 ") "seq
if (newk == oldk) return "aspiring " seq
if (index(seq," " newk " ") > 0) return "cyclic (at " newk ") " seq
if (newk > 140737488355328) return "non-terminating (term > 140737488355328) " seq
oldk = newk
}
return "non-terminating (after 16 terms)  " seq
}
BEGIN{
print "Number classification sequence"
for (j=1; j < 11; j++)
    {
    print j,class(j)}
    print 11,class(11)
    print 12,class(12)
    print 28,class(28)
    print 496,class(496)
    print 220,class(220)
    print 1184,class(1184)
    print 12496,class(12496)
    print 1264460,class(1264460)
    print 790,class(790)
    print 909,class(909)
    print 562,class(562)
    print 1064,class(1064)
    print 1488,class(1488)
    print 15355717786080,class(15355717786080)
    
}


```

{{out}}

```txt

Number classification sequence
1 terminating   0
2 terminating   1 0
3 terminating   1 0
4 terminating   3 1 0
5 terminating   1 0
6 perfect   6
7 terminating   1 0
8 terminating   7 1 0
9 terminating   4 3 1 0
10 terminating   8 7 1 0
11 terminating   1 0
12 terminating   16 15 9 4 3 1 0
28 perfect   28
496 perfect   496
220 amicable   284 220
1184 amicable   1210 1184
12496 sociable (period 5)   14288 15472 14536 14264 12496
1264460 sociable (period 4)   1547860 1727636 1305184 1264460
790 aspiring   650 652 496 496
909 aspiring   417 143 25 6 6
562 cyclic (at 284)   284 220 284
1064 cyclic (at 1184)   1336 1184 1210 1184
1488 non-terminating (after 16 terms)    2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384
1.53557e+13 non-terminating (term > 140737488355328)   4.45347e+13 1.4494e+14 4.71714e+14


```



## C

Both implementations can process integers or a file containing all the integers from the command line.

### Brute Force

The following implementation is a brute force method which takes a very, very long time for 15355717786080. To be fair to C, that's also true for many of the other implementations on this page which also implement the brute force method. See the next implementation for the best solution.

```C

#include<stdlib.h>
#include<string.h>
#include<stdio.h>

unsigned long long bruteForceProperDivisorSum(unsigned long long n){
	unsigned long long i,sum = 0;
	
	for(i=1;i<(n+1)/2;i++)
		if(n%i==0 && n!=i)
			sum += i;
		
	return sum;
}

void printSeries(unsigned long long* arr,int size,char* type){
	int i;
	
	printf("\nInteger : %llu, Type : %s, Series : ",arr[0],type);
	
	for(i=0;i<size-1;i++)
		printf("%llu, ",arr[i]);
	printf("%llu",arr[i]);
}

void aliquotClassifier(unsigned long long n){
	unsigned long long arr[16];
	int i,j;
	
	arr[0] = n;
	
	for(i=1;i<16;i++){
		arr[i] = bruteForceProperDivisorSum(arr[i-1]);
		
		if(arr[i]==0||arr[i]==n||(arr[i]==arr[i-1] && arr[i]!=n)){
			printSeries(arr,i+1,(arr[i]==0)?"Terminating":(arr[i]==n && i==1)?"Perfect":(arr[i]==n && i==2)?"Amicable":(arr[i]==arr[i-1] && arr[i]!=n)?"Aspiring":"Sociable");
			return;
		}
		
		for(j=1;j<i;j++){
			if(arr[j]==arr[i]){
				printSeries(arr,i+1,"Cyclic");
				return;
			}
		}
	}
	
	printSeries(arr,i+1,"Non-Terminating");
}

void processFile(char* fileName){
	FILE* fp = fopen(fileName,"r");
	char str[21];
	
	while(fgets(str,21,fp)!=NULL)
		aliquotClassifier(strtoull(str,(char**)NULL,10));
	
	fclose(fp);
}

int main(int argC,char* argV[])
{
    if(argC!=2)
		printf("Usage : %s <positive integer>",argV[0]);
	else{
		if(strchr(argV[1],'.')!=NULL)
			processFile(argV[1]);
		else
			aliquotClassifier(strtoull(argV[1],(char**)NULL,10));
	}
	return 0;
}

```

Input file, you can include 15355717786080 or similar numbers in this list but be prepared to wait for a very, very long time.:

```txt

1
2
3
4
5
6
7
8
9
10
11
12
28
496
220
1184
12496
1264460
790
909
562
1064
1488

```

Invocation and output for both individual number and input file:

```txt

C:\rosettaCode>bruteAliquot.exe 10

Integer : 10, Type : Terminating, Series : 10, 8, 7, 1, 0
C:\rosettaCode>bruteAliquot.exe aliquotData.txt

Integer : 1, Type : Terminating, Series : 1, 0
Integer : 2, Type : Terminating, Series : 2, 1, 0
Integer : 3, Type : Terminating, Series : 3, 1, 0
Integer : 4, Type : Terminating, Series : 4, 3, 1, 0
Integer : 5, Type : Terminating, Series : 5, 1, 0
Integer : 6, Type : Perfect, Series : 6, 6
Integer : 7, Type : Terminating, Series : 7, 1, 0
Integer : 8, Type : Terminating, Series : 8, 7, 1, 0
Integer : 9, Type : Terminating, Series : 9, 4, 3, 1, 0
Integer : 10, Type : Terminating, Series : 10, 8, 7, 1, 0
Integer : 11, Type : Terminating, Series : 11, 1, 0
Integer : 12, Type : Terminating, Series : 12, 16, 15, 9, 4, 3, 1, 0
Integer : 28, Type : Perfect, Series : 28, 28
Integer : 496, Type : Perfect, Series : 496, 496
Integer : 220, Type : Amicable, Series : 220, 284, 220
Integer : 1184, Type : Amicable, Series : 1184, 1210, 1184
Integer : 12496, Type : Sociable, Series : 12496, 14288, 15472, 14536, 14264, 12496
Integer : 1264460, Type : Sociable, Series : 1264460, 1547860, 1727636, 1305184, 1264460
Integer : 790, Type : Aspiring, Series : 790, 650, 652, 496, 496
Integer : 909, Type : Aspiring, Series : 909, 417, 143, 25, 6, 6
Integer : 562, Type : Cyclic, Series : 562, 284, 220, 284
Integer : 1064, Type : Cyclic, Series : 1064, 1336, 1184, 1210, 1184
Integer : 1488, Type : Non-Terminating, Series : 1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384, 68719476751

```



### Number Theoretic

The following implementation, based on Number Theory, is the best solution for such a problem. All cases are handled, including 15355717786080, with all the numbers being processed and the output written to console practically instantaneously. The above brute force implementation is the original one and it remains to serve as a comparison of the phenomenal difference the right approach can make to a problem.

```C

#include<string.h>
#include<stdlib.h>
#include<stdio.h>

unsigned long long raiseTo(unsigned long long base, unsigned long long power){
    unsigned long long result = 1,i;
    for (i=0; i<power;i++) {
        result*=base;
    }
    return result;
}

unsigned long long properDivisorSum(unsigned long long n){
	unsigned long long prod = 1; 
	unsigned long long temp = n,i,count = 0;

	while(n%2 == 0){
		count++;
		n /= 2;
	}
	
	if(count!=0)
		prod *= (raiseTo(2,count + 1) - 1);

	for(i=3;i*i<=n;i+=2){
		count = 0;
		
		while(n%i == 0){
			count++;
			n /= i;
		}
		
		if(count==1)
			prod *= (i+1);
		else if(count > 1)
			prod *= ((raiseTo(i,count + 1) - 1)/(i-1));
	}
	
	if(n>2)
		prod *= (n+1);

	return prod - temp;
}

void printSeries(unsigned long long* arr,int size,char* type){
	int i;
	
	printf("\nInteger : %llu, Type : %s, Series : ",arr[0],type);
	
	for(i=0;i<size-1;i++)
		printf("%llu, ",arr[i]);
	printf("%llu",arr[i]);
}

void aliquotClassifier(unsigned long long n){
	unsigned long long arr[16];
	int i,j;
	
	arr[0] = n;
	
	for(i=1;i<16;i++){
		arr[i] = properDivisorSum(arr[i-1]);
		
		if(arr[i]==0||arr[i]==n||(arr[i]==arr[i-1] && arr[i]!=n)){
			printSeries(arr,i+1,(arr[i]==0)?"Terminating":(arr[i]==n && i==1)?"Perfect":(arr[i]==n && i==2)?"Amicable":(arr[i]==arr[i-1] && arr[i]!=n)?"Aspiring":"Sociable");
			return;
		}
		
		for(j=1;j<i;j++){
			if(arr[j]==arr[i]){
				printSeries(arr,i+1,"Cyclic");
				return;
			}
		}
	}
	
	printSeries(arr,i+1,"Non-Terminating");
}

void processFile(char* fileName){
	FILE* fp = fopen(fileName,"r");
	char str[21];
	
	while(fgets(str,21,fp)!=NULL)
		aliquotClassifier(strtoull(str,(char**)NULL,10));
	
	fclose(fp);
}

int main(int argC,char* argV[])
{
    if(argC!=2)
		printf("Usage : %s <positive integer>",argV[0]);
	else{
		if(strchr(argV[1],'.')!=NULL)
			processFile(argV[1]);
		else
			aliquotClassifier(strtoull(argV[1],(char**)NULL,10));
	}
	return 0;
}

```

Input file, to emphasize the effectiveness of this approach, the last number in the file is 153557177860800, 10 times the special case mentioned in the task.

```txt

1
2
3
4
5
6
7
8
9
10
11
12
28
496
220
1184
12496
1264460
790
909
562
1064
1488
15355717786080
153557177860800

```

Invocation and output for both individual number and input file:

```txt

C:\rosettaCode>bruteAliquot.exe 10

Integer : 10, Type : Terminating, Series : 10, 8, 7, 1, 0
C:\rosettaCode>aliquotProper.exe aliquotData.txt

Integer : 1, Type : Terminating, Series : 1, 0
Integer : 2, Type : Terminating, Series : 2, 1, 0
Integer : 3, Type : Terminating, Series : 3, 1, 0
Integer : 4, Type : Terminating, Series : 4, 3, 1, 0
Integer : 5, Type : Terminating, Series : 5, 1, 0
Integer : 6, Type : Perfect, Series : 6, 6
Integer : 7, Type : Terminating, Series : 7, 1, 0
Integer : 8, Type : Terminating, Series : 8, 7, 1, 0
Integer : 9, Type : Terminating, Series : 9, 4, 3, 1, 0
Integer : 10, Type : Terminating, Series : 10, 8, 7, 1, 0
Integer : 11, Type : Terminating, Series : 11, 1, 0
Integer : 12, Type : Terminating, Series : 12, 16, 15, 9, 4, 3, 1, 0
Integer : 28, Type : Perfect, Series : 28, 28
Integer : 496, Type : Perfect, Series : 496, 496
Integer : 220, Type : Amicable, Series : 220, 284, 220
Integer : 1184, Type : Amicable, Series : 1184, 1210, 1184
Integer : 12496, Type : Sociable, Series : 12496, 14288, 15472, 14536, 14264, 12496
Integer : 1264460, Type : Sociable, Series : 1264460, 1547860, 1727636, 1305184, 1264460
Integer : 790, Type : Aspiring, Series : 790, 650, 652, 496, 496
Integer : 909, Type : Aspiring, Series : 909, 417, 143, 25, 6, 6
Integer : 562, Type : Cyclic, Series : 562, 284, 220, 284
Integer : 1064, Type : Cyclic, Series : 1064, 1336, 1184, 1210, 1184
Integer : 1488, Type : Non-Terminating, Series : 1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384, 68719476751
Integer : 15355717786080, Type : Non-Terminating, Series : 15355717786080, 44534663601120, 144940087464480, 471714103310688, 1130798979186912, 2688948041357088, 6050151708497568, 13613157922
102611548462968, 1977286128289819992, 3415126495450394808, 68719476751
Integer : 153557177860800, Type : Non-Terminating, Series : 153557177860800, 470221741508000, 685337334283120, 908681172226160, 1276860840159280, 1867115442105104, 1751034184622896, 16436297
336056, 1405725265675144, 1230017019320456, 68719476751

```



## Common Lisp

Uses the Lisp function '''proper-divisors-recursive''' from [[Proper_divisors#Common_Lisp|Task:Proper Divisors]].

```lisp
(defparameter *nlimit* 16)
(defparameter *klimit* (expt 2 47))
(defparameter *asht* (make-hash-table))
(load "proper-divisors")

(defun ht-insert (v n)
  (setf (gethash v *asht*) n))

(defun ht-find (v n)
   (let ((nprev (gethash v *asht*)))
    (if nprev (- n nprev) nil)))

(defun ht-list ()
  (defun sort-keys (&optional (res '()))
    (maphash #'(lambda (k v) (push (cons k v) res)) *asht*)
    (sort (copy-list res) #'< :key (lambda (p) (cdr p))))
  (let ((sorted (sort-keys)))
    (dotimes (i (length sorted)) (format t "~A " (car (nth i sorted))))))

(defun aliquot-generator (K1)
  "integer->function::fn to generate aliquot sequence"
  (let ((Kn K1))        
    #'(lambda () (setf Kn (reduce #'+ (proper-divisors-recursive Kn) :initial-value 0)))))

(defun aliquot (K1)
  "integer->symbol|nil::classify aliquot sequence"
  (defun aliquot-sym (Kn n)
    (let* ((period (ht-find Kn n))
           (sym (if period
                    (cond ; period event
                     ((= Kn K1)
                      (case period (1 'PERF) (2 'AMIC) (otherwise 'SOCI)))
                     ((= period 1) 'ASPI)
                     (t 'CYCL))
                    (cond ; else check for limit event
                     ((= Kn 0) 'TERM)
                     ((> Kn *klimit*) 'TLIM)
                     ((= n *nlimit*) 'NLIM)
                     (t nil)))))
      ;; if period event store the period, if no event insert the value      
      (if sym (when period (setf (symbol-plist sym) (list period)))                
          (ht-insert Kn n))
      sym))
   
  (defun aliquot-str (sym &optional (period 0))
    (case sym (TERM "terminating") (PERF "perfect") (AMIC "amicable") (ASPI "aspiring")
      (SOCI (format nil "sociable (period ~A)" (car (symbol-plist sym))))
      (CYCL (format nil "cyclic (period ~A)" (car (symbol-plist sym))))
      (NLIM (format nil "non-terminating (no classification before added term limit of ~A)" *nlimit*))
      (TLIM (format nil "non-terminating (term threshold of ~A exceeded)" *klimit*))
      (otherwise "unknown")))
             
  (clrhash *asht*)
  (let ((fgen (aliquot-generator K1)))
    (setf (symbol-function 'aliseq) #'(lambda () (funcall fgen))))
  (ht-insert K1 0)
  (do* ((n 1 (1+ n))
        (Kn (aliseq) (aliseq))
        (alisym (aliquot-sym Kn n) (aliquot-sym Kn n)))
       (alisym (format t "~A:" (aliquot-str alisym)) (ht-list) (format t "~A~%" Kn) alisym)))

(defun main ()
  (princ "The last item in each sequence triggers classification.") (terpri)
  (dotimes (k 10)
    (aliquot (+ k 1)))
  (dolist (k '(11 12 28 496 220 1184 12496 1264460 790 909 562 1064 1488 15355717786080))
    (aliquot k)))
```

{{out}}

```txt
CL-USER(45): (main)
The last item in each sequence triggers classification.
terminating:1 0
terminating:2 1 0
terminating:3 1 0
terminating:4 3 1 0
terminating:5 1 0
perfect:6 6
terminating:7 1 0
terminating:8 7 1 0
terminating:9 4 3 1 0
terminating:10 8 7 1 0
terminating:11 1 0
terminating:12 16 15 9 4 3 1 0
perfect:28 28
perfect:496 496
amicable:220 284 220
amicable:1184 1210 1184
sociable (period 5):12496 14288 15472 14536 14264 12496
sociable (period 4):1264460 1547860 1727636 1305184 1264460
aspiring:790 650 652 496 496
aspiring:909 417 143 25 6 6
cyclic (period 2):562 284 220 284
cyclic (period 2):1064 1336 1184 1210 1184
non-terminating (no classification before added term limit of 16):1488 2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384 1474608
non-terminating (term threshold of 140737488355328 exceeded):15355717786080 44534663601120 144940087464480
NIL

```



## D

{{trans|Python}}

```d
import std.stdio, std.range, std.algorithm, std.typecons, std.conv;

auto properDivisors(in ulong n) pure nothrow @safe /*@nogc*/ {
    return iota(1UL, (n + 1) / 2 + 1).filter!(x => n % x == 0 && n != x);
}

enum pDivsSum = (in ulong n) pure nothrow @safe /*@nogc*/ =>
    n.properDivisors.sum;

auto aliquot(in ulong n,
             in size_t maxLen=16,
             in ulong maxTerm=2UL^^47) pure nothrow @safe {
    if (n == 0)
        return tuple("Terminating", [0UL]);
    ulong[] s = [n];
    size_t sLen = 1;
    ulong newN = n;

    while (sLen <= maxLen && newN < maxTerm) {
        newN = s.back.pDivsSum;
        if (s.canFind(newN)) {
            if (s[0] == newN) {
                if (sLen == 1) {
                    return tuple("Perfect", s);
                } else if (sLen == 2) {
                    return tuple("Amicable", s);
                } else
                    return tuple(text("Sociable of length ", sLen), s);
            } else if (s.back == newN) {
                return tuple("Aspiring", s);
            } else
                return tuple(text("Cyclic back to ", newN), s);
        } else if (newN == 0) {
            return tuple("Terminating", s ~ 0);
        } else {
            s ~= newN;
            sLen++;
        }
    }

    return tuple("Non-terminating", s);
}

void main() {
    foreach (immutable n; 1 .. 11)
        writefln("%s: %s", n.aliquot[]);
    writeln;
    foreach (immutable n; [11, 12, 28, 496, 220, 1184,  12496, 1264460,
                           790, 909, 562, 1064, 1488])
        writefln("%s: %s", n.aliquot[]);
}
```

{{out}}

```txt
Terminating: [1, 0]
Terminating: [2, 1, 0]
Terminating: [3, 1, 0]
Terminating: [4, 3, 1, 0]
Terminating: [5, 1, 0]
Perfect: [6]
Terminating: [7, 1, 0]
Terminating: [8, 7, 1, 0]
Terminating: [9, 4, 3, 1, 0]
Terminating: [10, 8, 7, 1, 0]

Terminating: [11, 1, 0]
Terminating: [12, 16, 15, 9, 4, 3, 1, 0]
Perfect: [28]
Perfect: [496]
Amicable: [220, 284]
Amicable: [1184, 1210]
Sociable of length 5: [12496, 14288, 15472, 14536, 14264]
Sociable of length 4: [1264460, 1547860, 1727636, 1305184]
Aspiring: [790, 650, 652, 496]
Aspiring: [909, 417, 143, 25, 6]
Cyclic back to 284: [562, 284, 220]
Cyclic back to 1184: [1064, 1336, 1184, 1210]
Non-terminating: [1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384, 1474608]
```



## EchoLisp


```scheme

;; implementation of Floyd algorithm to find cycles in a graph
;; see Wikipedia https://en.wikipedia.org/wiki/Cycle_detection
;; returns (cycle-length cycle-starter steps)
;; steps = 0 if no cycle found
;; it's all about a tortoise  🐢 running at speed f(x) after a hare 🐰 at speed f(f (x))
;; when they meet, a cycle is found

(define (floyd f x0 steps maxvalue)
	(define lam 1) ; cycle length
	(define tortoise (f x0))
	(define hare (f (f x0)))
	
	;; cyclic  ? yes if steps > 0
	(while (and (!= tortoise hare) (> steps 0))
		(set!-values (tortoise hare) (values (f tortoise) (f (f hare))))
		#:break (and (> hare maxvalue) (set! steps 0))
		(set! steps (1- steps)))
		
	;; first repetition = cycle starter
	(set! tortoise x0)
	(while (and (!= tortoise hare) (> steps 0))
		(set!-values (tortoise hare) (values (f tortoise) (f hare))))
		
	;; length of shortest cycle
	(set! hare (f tortoise))
	(while (and (!= tortoise hare) (> steps 0))
		(set! hare (f hare))
		(set! lam (1+ lam)))
	(values lam tortoise steps))
	
;; find cycle and classify
(define (taxonomy n (steps 16) (maxvalue 140737488355328))
	  (define-values (cycle starter steps) (floyd sum-divisors n steps maxvalue))
	 (write  n
	 (cond 
	    (( = steps 0) 'non-terminating)
	    (( = starter 0) 'terminating)
	    ((and (= starter n) (= cycle 1)) 'perfect)
	    ((and (= starter n) (= cycle 2)) 'amicable)
            ((= starter n)  'sociable )
	    ((= cycle 1)  'aspiring )
	    (else 'cyclic)))
	
	(aliquote n starter)
	)
	
;; print sequence
(define (aliquote x0  (starter -1) (end -1 )(n 8))
  (for ((i n))
    (write x0)
    (set! x0 (sum-divisors x0))
    #:break (and (= x0 end) (write x0))
    (when (= x0 starter) (set! end starter)))
    (writeln ...))

```

{{out}}

```scheme

(lib 'math)
(lib 'bigint)

(for-each taxonomy (range 1 13))

1 terminating 1 0 0 ...    
2 terminating 2 1 0 0 ...    
3 terminating 3 1 0 0 ...    
4 terminating 4 3 1 0 0 ...    
5 terminating 5 1 0 0 ...    
6 perfect 6 6 6 ...    
7 terminating 7 1 0 0 ...    
8 terminating 8 7 1 0 0 ...    
9 terminating 9 4 3 1 0 0 ...    
10 terminating 10 8 7 1 0 0 ...    
11 terminating 11 1 0 0 ...    
12 terminating 12 16 15 9 4 3 1 0 0 ...    

(for-each taxonomy '( 28 496 220 1184 12496 1264460 790 909 562 1064 1488 15355717786080))

28 perfect 28 28 28 ...    
496 perfect 496 496 496 ...    
220 amicable 220 284 220 284 220 ...    
1184 amicable 1184 1210 1184 1210 1184 ...    
12496 sociable 12496 14288 15472 14536 14264 12496 14288 15472 ...    
1264460 sociable 1264460 1547860 1727636 1305184 1264460 1547860 1727636 1305184 1264460 ...    
790 aspiring 790 650 652 496 496 ...    
909 aspiring 909 417 143 25 6 6 ...    
562 cyclic 562 284 220 284 ...    
1064 cyclic 1064 1336 1184 1210 1184 ...    
1488 non-terminating 1488 2480 3472 4464 8432 9424 10416 21328 ...    
15355717786080 non-terminating 15355717786080 44534663601120 144940087464480 471714103310688 1130798979186912 2688948041357088 6050151708497568 13613157922639968 ... 

(taxonomy 1000) ;; 1000 non-terminating after 16 steps
1000 non-terminating 1000 1340 1516 1144 1376 1396 1054 674 ... 
   
(taxonomy 1000 32) ;; but terminating if we increase the number of steps
1000 terminating   
1000 1340 1516 1144 1376 1396 1054 674 340 416 466 236 184 176 196 203 37 1 0 0 ...       

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Proper do
  def divisors(1), do: []
  def divisors(n), do: [1 | divisors(2,n,:math.sqrt(n))] |> Enum.sort
  
  defp divisors(k,_n,q) when k>q, do: []
  defp divisors(k,n,q) when rem(n,k)>0, do: divisors(k+1,n,q)
  defp divisors(k,n,q) when k * k == n, do: [k | divisors(k+1,n,q)]
  defp divisors(k,n,q)                , do: [k,div(n,k) | divisors(k+1,n,q)]
end

defmodule Aliquot do
  def sequence(n, maxlen\\16, maxterm\\140737488355328)
  def sequence(0, _maxlen, _maxterm), do: "terminating"
  def sequence(n, maxlen, maxterm) do
    {msg, s} = sequence(n, maxlen, maxterm, [n])
    {msg, Enum.reverse(s)}
  end
  
  defp sequence(n, maxlen, maxterm, s) when length(s) < maxlen and n < maxterm do
    m = Proper.divisors(n) |> Enum.sum
    cond do
      m in s ->
        case {m, List.last(s), hd(s)} do
          {x,x,_} ->
            case length(s) do
              1 -> {"perfect", s}
              2 -> {"amicable", s}
              _ -> {"sociable of length #{length(s)}", s}
            end
          {x,_,x} -> {"aspiring", [m | s]}
          _       -> {"cyclic back to #{m}", [m | s]}
        end
      m == 0 -> {"terminating", [0 | s]}
      true -> sequence(m, maxlen, maxterm, [m | s])
    end
  end
  defp sequence(_, _, _, s), do: {"non-terminating", s}
end

Enum.each(1..10, fn n ->
  {msg, s} = Aliquot.sequence(n)
  :io.fwrite("~7w:~21s: ~p~n", [n, msg, s])
end)
IO.puts ""
[11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488, 15355717786080]
|> Enum.each(fn n ->
     {msg, s} = Aliquot.sequence(n)
     if n<10000000, do: :io.fwrite("~7w:~21s: ~p~n", [n, msg, s]),
                  else: :io.fwrite("~w: ~s: ~p~n", [n, msg, s])
   end)
```


{{out}}

```txt

      1:          terminating: [1,0]
      2:          terminating: [2,1,0]
      3:          terminating: [3,1,0]
      4:          terminating: [4,3,1,0]
      5:          terminating: [5,1,0]
      6:              perfect: [6]
      7:          terminating: [7,1,0]
      8:          terminating: [8,7,1,0]
      9:          terminating: [9,4,3,1,0]
     10:          terminating: [10,8,7,1,0]

     11:          terminating: [11,1,0]
     12:          terminating: [12,16,15,9,4,3,1,0]
     28:              perfect: [28]
    496:              perfect: [496]
    220:             amicable: [220,284]
   1184:             amicable: [1184,1210]
  12496: sociable of length 5: [12496,14288,15472,14536,14264]
1264460: sociable of length 4: [1264460,1547860,1727636,1305184]
    790:             aspiring: [790,650,652,496,496]
    909:             aspiring: [909,417,143,25,6,6]
    562:   cyclic back to 284: [562,284,220,284]
   1064:  cyclic back to 1184: [1064,1336,1184,1210,1184]
   1488:      non-terminating: [1488,2480,3472,4464,8432,9424,10416,21328,
                                22320,55056,95728,96720,236592,459792,881392,
                                882384]
15355717786080: non-terminating: [15355717786080,44534663601120,
                                  144940087464480]

```



## Factor

For convenience, the term that caused termination is always included in the output sequence.

```factor
USING: combinators combinators.short-circuit formatting kernel
literals locals math math.functions math.primes.factors
math.ranges namespaces pair-rocket sequences sets ;
FROM: namespaces => set ;
IN: rosetta-code.aliquot

SYMBOL: terms
CONSTANT: 2^47 $[ 2 47 ^ ]
CONSTANT: test-cases {
    11 12 28 496 220 1184 12496 1264460 790
    909 562 1064 1488 15355717786080
}

: next-term ( n -- m ) dup divisors sum swap - ;

: continue-aliquot? ( hs term -- hs term ? )
    {
        [ terms get 15 < ]
        [ swap in? not   ]
        [ nip zero? not  ]
        [ nip 2^47 <     ]
    } 2&& ;
    
: next-aliquot ( hs term -- hs next-term term )
    [ swap [ adjoin    ] keep ]
    [ dup  [ next-term ] dip  ] bi terms inc ;
    
: aliquot ( k -- seq )
    0 terms set HS{ } clone swap
    [ continue-aliquot? ] [ next-aliquot ] produce
    [ drop ] 2dip swap suffix ;
    
: non-terminating? ( seq -- ? )
    { [ length 15 > ] [ [ 2^47 > ] any? ] } 1|| ;
    
:: classify ( seq -- classification-str )
    {
        [ seq non-terminating? ] => [ "non-terminating" ]
        [ seq last zero?       ] => [ "terminating"     ]
        [ seq length 2 =       ] => [ "perfect"         ]
        [ seq length 3 =       ] => [ "amicable"        ]
        [ seq first seq last = ] => [ "sociable"        ]
        [ seq 2 tail* first2 = ] => [ "aspiring"        ]
        [ "cyclic" ]
    } cond ;
    
: .classify ( k -- )
    dup aliquot [ classify ] keep "%14u: %15s: %[%d, %]\n"
    printf ;
    
: main ( -- )
    10 [1,b] test-cases append [ .classify ] each ;

MAIN: main
```

{{out}}

```txt

             1:     terminating: { 1, 0 }
             2:     terminating: { 2, 1, 0 }
             3:     terminating: { 3, 1, 0 }
             4:     terminating: { 4, 3, 1, 0 }
             5:     terminating: { 5, 1, 0 }
             6:         perfect: { 6, 6 }
             7:     terminating: { 7, 1, 0 }
             8:     terminating: { 8, 7, 1, 0 }
             9:     terminating: { 9, 4, 3, 1, 0 }
            10:     terminating: { 10, 8, 7, 1, 0 }
            11:     terminating: { 11, 1, 0 }
            12:     terminating: { 12, 16, 15, 9, 4, 3, 1, 0 }
            28:         perfect: { 28, 28 }
           496:         perfect: { 496, 496 }
           220:        amicable: { 220, 284, 220 }
          1184:        amicable: { 1184, 1210, 1184 }
         12496:        sociable: { 12496, 14288, 15472, 14536, 14264, 12496 }
       1264460:        sociable: { 1264460, 1547860, 1727636, 1305184, 1264460 }
           790:        aspiring: { 790, 650, 652, 496, 496 }
           909:        aspiring: { 909, 417, 143, 25, 6, 6 }
           562:          cyclic: { 562, 284, 220, 284 }
          1064:          cyclic: { 1064, 1336, 1184, 1210, 1184 }
          1488: non-terminating: { 1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384 }
15355717786080: non-terminating: { 15355717786080, 44534663601120, 144940087464480 }

```



## Fortran

This is straightforward for Fortran compilers that allow 64-bit integers, as with INTEGER*8 - though one must have faith in the correct functioning of the computer for such large numbers....

Output:
        After 1, terminates! 1
        After 2, terminates! 2,1
        After 2, terminates! 3,1
        After 3, terminates! 4,3,1
        After 2, terminates! 5,1
                    Perfect! 6
        After 2, terminates! 7,1
        After 3, terminates! 8,7,1
        After 4, terminates! 9,4,3,1
        After 4, terminates! 10,8,7,1
        After 2, terminates! 11,1
        After 7, terminates! 12,16,15,9,4,3,1
                    Perfect! 28
                    Perfect! 496
                   Amicable: 220,284
                   Amicable: 1184,1210
                 Sociable 5: 12496,14288,15472,14536,14264
                 Sociable 4: 1264460,1547860,1727636,1305184
                   Aspiring: 790,650,652,496
                   Aspiring: 909,417,143,25,6
       Cyclic end 2, to 284: 562,284,220
      Cyclic end 2, to 1184: 1064,1336,1184,1210
  After 16, non-terminating? 1488,2480,3472,4464,8432,9424,10416,21328,22320,55056,95728,96720,
 236592,459792,881392,882384
         After 2, overflows! 15355717786080,44534663601120

Allowing more rope leads 1488 to overflow after the 83'rd value. Extending TOOBIG to 2**48 produces overflow from step 88, and the monster test value manages one more step, to 144940087464480 and confirmed via the Mathematica example.
Because the task involves only a few numbers to test, there is not so much advantage to be gained by pre-calculating a set of sums of proper divisors, but it does mean that no special tests are needed for N = 1 in function SUMF.

A more flexible syntax (such as Algol's) would enable the double scan of the TRAIL array to be avoided, as in if TRAIL[I:=MinLoc(Abs(TRAIL(1:L) - SF))] = SF then... That is, find the first index of array TRAIL such that ABS(TRAIL(1:L) - SF) is minimal, save that index in I, then access that element of TRAIL and test if it is equal to SF. The INDEX function could be use to find the first match, except that it is defined only for character variables. Alternatively, use an explicit DO-loop to search for equality, thus not employing fancy syntax, and not having to wonder if the ANY function will stop on the first match rather than wastefully continue the testing for all array elements. The modern style in manual writing is to employ vaguely general talk about arrays and omit specific details.


```Fortran

      MODULE FACTORSTUFF	!This protocol evades the need for multiple parameters, or COMMON, or one shapeless main line...
Concocted by R.N.McLean, MMXV.
c       INTEGER*4 I4LIMIT
c       PARAMETER (I4LIMIT = 2147483647)
       INTEGER*8 TOOBIG		!Some bounds.
       PARAMETER (TOOBIG = 2**47)	!Computer arithmetic is not with real numbers.
       INTEGER LOTS			!Nor is computer storage infinite.
       PARAMETER (LOTS = 10000)	!So there can't be all that many of these.
       INTEGER*8 KNOWNSUM(LOTS)	!If multiple references are expected, it is worthwhile calculating these.
       CONTAINS			!Assistants.
        INTEGER*8 FUNCTION SUMF(N)	!Sum of the proper divisors of N.
         INTEGER*8 N			!The number in question.
         INTEGER*8 F,F2		!Candidate factor, and its square.
         INTEGER*8 S,INC,BOOST		!Assistants.
          IF (N.LE.LOTS) THEN		!If we're within reach,
            SUMF = KNOWNSUM(N)			!The result is to hand.
           ELSE			!Otherwise, some on-the-spot effort ensues.
Could use SUMF in place of S, but some compilers have been confused by such usage.
            S = 1			!1 is always a factor of N, but N is deemed not proper.
            F = 1			!Prepare a crude search for factors.
            INC = 1			!One by plodding one.
            IF (MOD(N,2) .EQ. 1) INC = 2!Ah, but an odd number cannot have an even number as a divisor.
    1       F = F + INC			!So half the time we can doubleplod.
            F2 = F*F				!Up to F2 < N rather than F < SQRT(N) and worries over inexact arithmetic.
            IF (F2 .LT. N) THEN			!F2 = N handled below.
              IF (MOD(N,F) .EQ. 0) THEN		!Does F divide N?
                BOOST = F + N/F			!Yes. The divisor and its counterpart.
                IF (S .GT. TOOBIG - BOOST) GO TO 666	!Would their augmentation cause an overflow?
                S = S + BOOST			!No, so count in the two divisors just discovered.
              END IF				!So much for a divisor discovered.
              GO TO 1				!Try for another.
            END IF			!So much for N = p*q style factors.
            IF (F2 .EQ. N) THEN	!Special case: N may be a perfect square, not necessarily of a prime number.
              IF (S .GT. TOOBIG - F) GO TO 666	!It is. And it too might cause overflow.
              S = S + F			!But if not, count F once only.
            END IF			!All done.
            SUMF = S			!This is the result.
          END IF			!Whichever way obtained,
         RETURN			!Done.
Cannot calculate the sum, because it exceeds the INTEGER*8 limit.
  666     SUMF = -666		!An expression of dismay that the caller will notice.
        END FUNCTION SUMF	!Alternatively, find the prime factors, and combine them... 
         SUBROUTINE PREPARESUMF	!Initialise the KNOWNSUM array.
Convert the Sieve of Eratoshenes to have each slot contain the sum of the proper divisors of its slot number.
Changes to instead count the number of factors, or prime factors, etc. would be simple enough.
         INTEGER*8 F		!A factor for numbers such as 2F, 3F, 4F, 5F, ...
          KNOWNSUM(1) = 0		!Proper divisors of N do not include N.
          KNOWNSUM(2:LOTS) = 1		!So, although 1 divides all N without remainder, 1 is excluded for itself.
          DO F = 2,LOTS/2		!Step through all the possible divisors of numbers not exceeding LOTS.
            FORALL(I = F + F:LOTS:F) KNOWNSUM(I) = KNOWNSUM(I) + F	!And augment each corresponding slot.
          END DO			!Different divisors can hit the same slot. For instance, 6 by 2 and also by 3.
        END SUBROUTINE PREPARESUMF	!Could alternatively generate all products of prime numbers. 
         SUBROUTINE CLASSIFY(N)	!Traipse along the SumF trail.
         INTEGER*8 N		!The starter.
         INTEGER ROPE		!The size of my memory is not so great..
         PARAMETER(ROPE = 16)	!Indeed, this is strictly limited.
         INTEGER*8 TRAIL(ROPE)	!But the numbers can be large.
         INTEGER*8 SF		!The working sum of proper divisors.
         INTEGER I,L		!Indices, merely.
         CHARACTER*28 THIS	!A perfect scratchpad for remarks.
          L = 1		!Every journey starts with its first step.
          TRAIL(1) = N		!Which is this.
          SF = N		!Syncopation.
   10     SF = SUMF(SF)		!Step onwards.
          IF (SF .LT. 0) THEN		!Trouble?
            WRITE (THIS,11) L,"overflows!"	!Yes. Too big a number.
   11       FORMAT ("After ",I0,", ",A)		!Describe the situation.
            CALL REPORT(ADJUSTR(THIS))		!And give the report.
          ELSE IF (SF .EQ. 0) THEN		!Otherwise, a finish?
            WRITE (THIS,11) L,"terminates!"	!Yay!
            CALL REPORT(ADJUSTR(THIS))		!This sequence is finished.
          ELSE IF (ANY(TRAIL(1:L) .EQ. SF)) THEN	!Otherwise, is there an echo somewhere?
            IF (L .EQ. 1) THEN				!Yes!
              CALL REPORT("Perfect!")			!Are we at the start?
            ELSE IF (L .EQ. 2) THEN			!Or perhaps not far along.
              CALL REPORT("Amicable:")			!These are held special.
            ELSE					!Otherwise, we've wandered further along.
              I = MINLOC(ABS(TRAIL(1:L) - SF),DIM=1)	!Damnit, re-scan the array to finger the first matching element.
              IF (I .EQ. 1) THEN		!If all the way back to the start,
                WRITE (THIS,12) L		!Then there are this many elements in the sociable ring.
   12           FORMAT ("Sociable ",I0,":")	!Computers are good at counting.
                CALL REPORT(ADJUSTR(THIS))	!So, perform an added service.
              ELSE IF (I .EQ. L) THEN		!Perhaps we've hit a perfect number!
                CALL REPORT("Aspiring:")	!A cycle of length one.
              ELSE				!But otherwise,
                WRITE (THIS,13) L - I + 1,SF	!A longer cycle. Amicable, or sociable.
   13           FORMAT ("Cyclic end ",I0,", to ",I0,":")	!Name the flashback value too.
                CALL REPORT(ADJUSTR(THIS))	!Thus.
              END IF				!So much for cycles.
            END IF			!So much for finding an echo.
          ELSE				!Otherwise, nothing special has happened.
            IF (L .GE. ROPE) THEN		!So, how long is a piece of string?
              WRITE (THIS,11) L,"non-terminating?"	!Not long enough!
              CALL REPORT(ADJUSTR(THIS))		!So we give up.
             ELSE				!But if there is more scope,
              L = L + 1			!Advance one more step.
              TRAIL(L) = SF			!Save the latest result.
              GO TO 10				!And try for the next.
            END IF			!So much for continuing.
          END IF		!So much for the classification.
         RETURN		!Finished.
         CONTAINS		!Not quite.
          SUBROUTINE REPORT(WHAT)	!There is this service routine.
           CHARACTER*(*) WHAT		!Whatever the length of the text, the FORMAT's A28 shows 28 characters, right-aligned.
            WRITE (6,1) WHAT,TRAIL(1:L)!Mysteriously, a fresh line after every twelve elements.
    1       FORMAT (A28,1X,12(I0:","))	!And obviously, the : signifies "do not print what follows unless there is another number to go.
          END SUBROUTINE REPORT	!That was easy.
        END SUBROUTINE CLASSIFY	!Enough. 
       END MODULE FACTORSTUFF	!Enough assistants. 
       PROGRAM CLASSIFYTHEM	!Report on the nature of the sequence N, Sumf(N), Sumf(Sumf(N)), etc.
       USE FACTORSTUFF		!This should help.
       INTEGER*8 I,N		!Steppers.
       INTEGER*8 THIS(14)	!A testing collection.
       DATA THIS/11,12,28,496,220,1184,12496,1264460,790,909,     !Old-style continuation character in column six.
     1  562,1064,1488,15355717786080/	!Monster value far exceeds the INTEGER*4 limit 
         CALL PREPARESUMF		!Prepare for 1:LOTS, even though this test run will use only a few.
         DO I = 1,10			!As specified, the first ten integers.
          CALL CLASSIFY(I)
        END DO
         DO I = 1,SIZE(THIS)		!Now for the specified list.
          CALL CLASSIFY(THIS(I))
        END DO
       END			!Done.

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "math"
    "strings"
)

const threshold = uint64(1) << 47

func indexOf(s []uint64, search uint64) int {
    for i, e := range s {
        if e == search {
            return i
        }
    }
    return -1
}

func contains(s []uint64, search uint64) bool {
    return indexOf(s, search) > -1
}

func maxOf(i1, i2 int) int {
    if i1 > i2 {
        return i1
    }
    return i2
}

func sumProperDivisors(n uint64) uint64 {
    if n < 2 {
        return 0
    }
    sqrt := uint64(math.Sqrt(float64(n)))
    sum := uint64(1)
    for i := uint64(2); i <= sqrt; i++ {
        if n % i != 0 {
            continue
        }
        sum += i + n / i
    }
    if sqrt * sqrt == n {
        sum -= sqrt
    }
    return sum
}

func classifySequence(k uint64) ([]uint64, string) {
    if k == 0 {
        panic("Argument must be positive.")
    }
    last := k
    var seq []uint64
    seq = append(seq, k)
    for {
        last = sumProperDivisors(last)
        seq = append(seq, last)
        n := len(seq)
        aliquot := ""
        switch {
        case last == 0:
            aliquot = "Terminating"
        case n == 2 && last == k:
            aliquot = "Perfect"
        case n == 3 && last == k:
            aliquot = "Amicable"
        case n >= 4 && last == k:
            aliquot = fmt.Sprintf("Sociable[%d]", n - 1)
        case last == seq[n - 2]:
            aliquot = "Aspiring"
        case contains(seq[1 : maxOf(1, n - 2)], last):
            aliquot = fmt.Sprintf("Cyclic[%d]", n - 1 - indexOf(seq[:], last))
        case n == 16 || last > threshold:
            aliquot = "Non-Terminating"
        }
        if aliquot != "" {
            return seq, aliquot
        }
    }
}

func joinWithCommas(seq []uint64) string {
    res := fmt.Sprint(seq)
    res = strings.Replace(res, " ", ", ", -1)
    return res
}

func main() {
    fmt.Println("Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n")
    for k := uint64(1); k <= 10; k++ {
        seq, aliquot := classifySequence(k)
        fmt.Printf("%2d: %-15s %s\n", k, aliquot, joinWithCommas(seq))
    }
    fmt.Println()

    s := []uint64{
        11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488,
    }
    for _, k := range s {
        seq, aliquot := classifySequence(k)
        fmt.Printf("%7d: %-15s %s\n",  k, aliquot, joinWithCommas(seq))
    }
    fmt.Println()

    k := uint64(15355717786080)
    seq, aliquot := classifySequence(k)
    fmt.Printf("%d: %-15s %s\n", k, aliquot, joinWithCommas(seq))
}
```


{{out}}

```txt

Aliquot classifications - periods for Sociable/Cyclic in square brackets:

 1: Terminating     [1, 0]
 2: Terminating     [2, 1, 0]
 3: Terminating     [3, 1, 0]
 4: Terminating     [4, 3, 1, 0]
 5: Terminating     [5, 1, 0]
 6: Perfect         [6, 6]
 7: Terminating     [7, 1, 0]
 8: Terminating     [8, 7, 1, 0]
 9: Terminating     [9, 4, 3, 1, 0]
10: Terminating     [10, 8, 7, 1, 0]

     11: Terminating     [11, 1, 0]
     12: Terminating     [12, 16, 15, 9, 4, 3, 1, 0]
     28: Perfect         [28, 28]
    496: Perfect         [496, 496]
    220: Amicable        [220, 284, 220]
   1184: Amicable        [1184, 1210, 1184]
  12496: Sociable[5]     [12496, 14288, 15472, 14536, 14264, 12496]
1264460: Sociable[4]     [1264460, 1547860, 1727636, 1305184, 1264460]
    790: Aspiring        [790, 650, 652, 496, 496]
    909: Aspiring        [909, 417, 143, 25, 6, 6]
    562: Cyclic[2]       [562, 284, 220, 284]
   1064: Cyclic[2]       [1064, 1336, 1184, 1210, 1184]
   1488: Non-Terminating [1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384]

15355717786080: Non-Terminating [15355717786080, 44534663601120, 144940087464480]

```



## Haskell


```Haskell
divisors :: (Integral a) => a -> [a]
divisors n = filter ((0 ==) . (n `mod`)) [1 .. (n `div` 2)]

data Class
  = Terminating
  | Perfect
  | Amicable
  | Sociable
  | Aspiring
  | Cyclic
  | Nonterminating
  deriving (Show)

aliquot :: (Integral a) => a -> [a]
aliquot 0 = [0]
aliquot n = n : (aliquot $ sum $ divisors n)

classify :: (Num a, Eq a) => [a] -> Class
classify []             = Nonterminating
classify [0]            = Terminating
classify [_]            = Nonterminating
classify [a,b]
  | a == b              = Perfect
  | b == 0              = Terminating
  | otherwise           = Nonterminating
classify x@(a:b:c:_)
  | a == b              = Perfect
  | a == c              = Amicable
  | a `elem` (drop 1 x) = Sociable
  | otherwise           =
    case classify (drop 1 x) of
      Perfect  -> Aspiring
      Amicable -> Cyclic
      Sociable -> Cyclic
      d        -> d

main :: IO ()
main = do
  let cls n = let ali = take 16 $ aliquot n in (classify ali, ali)
  mapM_ (print . cls) $ [1..10] ++
    [11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488]
```

{{out}}

```txt
(Terminating,[1,0])
(Terminating,[2,1,0])
(Terminating,[3,1,0])
(Terminating,[4,3,1,0])
(Terminating,[5,1,0])
(Perfect,[6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6])
(Terminating,[7,1,0])
(Terminating,[8,7,1,0])
(Terminating,[9,4,3,1,0])
(Terminating,[10,8,7,1,0])
(Terminating,[11,1,0])
(Terminating,[12,16,15,9,4,3,1,0])
(Perfect,[28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28])
(Perfect,[496,496,496,496,496,496,496,496,496,496,496,496,496,496,496,496])
(Amicable,[220,284,220,284,220,284,220,284,220,284,220,284,220,284,220,284])
(Amicable,[1184,1210,1184,1210,1184,1210,1184,1210,1184,1210,1184,1210,1184,1210,1184,1210])
(Sociable,[12496,14288,15472,14536,14264,12496,14288,15472,14536,14264,12496,14288,15472,14536,14264,12496])
(Sociable,[1264460,1547860,1727636,1305184,1264460,1547860,1727636,1305184,1264460,1547860,1727636,1305184,1264460,1547860,1727636,1305184])
(Aspiring,[790,650,652,496,496,496,496,496,496,496,496,496,496,496,496,496])
(Aspiring,[909,417,143,25,6,6,6,6,6,6,6,6,6,6,6,6])
(Cyclic,[562,284,220,284,220,284,220,284,220,284,220,284,220,284,220,284])
(Cyclic,[1064,1336,1184,1210,1184,1210,1184,1210,1184,1210,1184,1210,1184,1210,1184,1210])
(Nonterminating,[1488,2480,3472,4464,8432,9424,10416,21328,22320,55056,95728,96720,236592,459792,881392,882384])
```



## J

Implementation:

```J
proper_divisors=: [: */@>@}:@,@{ [: (^ i.@>:)&.>/ 2 p: x:
aliquot=: +/@proper_divisors ::0:
rc_aliquot_sequence=: aliquot^:(i.16)&>
rc_classify=: 3 :0
      if. 16 ~:# y                 do. ' invalid        '
  elseif. 6 > {: y                 do. ' terminate      '
  elseif. (+./y>2^47) +. 16 = #~.y do. ' non-terminating'
  elseif. 1=#~. y                  do. ' perfect        '
  elseif. 8= st=. {.#/.~ y         do. ' amicable       '
  elseif. 1 < st                   do. ' sociable       '
  elseif. =/_2{. y                 do. ' aspiring       '
  elseif. 1                        do. ' cyclic         '
  end.
)
rc_display_aliquot_sequence=: (rc_classify,' ',":)@:rc_aliquot_sequence
```


Task example:

```J
   rc_display_aliquot_sequence&> >: i.10
 terminate       1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
 terminate       2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
 terminate       3 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
 terminate       4 3 1 0 0 0 0 0 0 0 0 0 0 0 0 0 
 terminate       5 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
 perfect         6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 
 terminate       7 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
 terminate       8 7 1 0 0 0 0 0 0 0 0 0 0 0 0 0 
 terminate       9 4 3 1 0 0 0 0 0 0 0 0 0 0 0 0 
 terminate       10 8 7 1 0 0 0 0 0 0 0 0 0 0 0 0

   rc_display_aliquot_sequence&>11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488, 15355717786080x
 terminate       11 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 terminate       12 16 15 9 4 3 1 0 0 0 0 0 0 0 0 0
 perfect         28 28 28 28 28 28 28 28 28 28 28 28 28 28 28 28
 perfect         496 496 496 496 496 496 496 496 496 496 496 496 496 496 496 496
 amicable        220 284 220 284 220 284 220 284 220 284 220 284 220 284 220 284
 amicable        1184 1210 1184 1210 1184 1210 1184 1210 1184 1210 1184 1210 1184 1210 1184 1210
 sociable        12496 14288 15472 14536 14264 12496 14288 15472 14536 14264 12496 14288 15472 14536 14264 12496
 sociable        1264460 1547860 1727636 1305184 1264460 1547860 1727636 1305184 1264460 1547860 1727636 1305184 1264460 1547860 1727636 1305184
 aspiring        790 650 652 496 496 496 496 496 496 496 496 496 496 496 496 496
 aspiring        909 417 143 25 6 6 6 6 6 6 6 6 6 6 6 6
 cyclic          562 284 220 284 220 284 220 284 220 284 220 284 220 284 220 284
 cyclic          1064 1336 1184 1210 1184 1210 1184 1210 1184 1210 1184 1210 1184 1210 1184 1210
 non-terminating 1488 2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384
 non-terminating 15355717786080 44534663601120 144940087464480 471714103310688 1130798979186912 2688948041357088 6050151708497568 13613157922639968 35513546724070632 74727605255142168 162658586225561832 353930992506879768 642678347124409032 1125102611548462968 1977286128289819992 3415126495450394808
```



## Java

Translation of [[Aliquot_sequence_classifications#Python|Python]] via [[Aliquot_sequence_classifications#D|D]]
{{works with|Java|8}}

```java
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.LongStream;

public class AliquotSequenceClassifications {

    private static Long properDivsSum(long n) {
        return LongStream.rangeClosed(1, (n + 1) / 2).filter(i -> n % i == 0 && n != i).sum();
    }

    static boolean aliquot(long n, int maxLen, long maxTerm) {
        List<Long> s = new ArrayList<>(maxLen);
        s.add(n);
        long newN = n;

        while (s.size() <= maxLen && newN < maxTerm) {

            newN = properDivsSum(s.get(s.size() - 1));

            if (s.contains(newN)) {

                if (s.get(0) == newN) {

                    switch (s.size()) {
                        case 1:
                            return report("Perfect", s);
                        case 2:
                            return report("Amicable", s);
                        default:
                            return report("Sociable of length " + s.size(), s);
                    }

                } else if (s.get(s.size() - 1) == newN) {
                    return report("Aspiring", s);

                } else
                    return report("Cyclic back to " + newN, s);

            } else {
                s.add(newN);
                if (newN == 0)
                    return report("Terminating", s);
            }
        }

        return report("Non-terminating", s);
    }

    static boolean report(String msg, List<Long> result) {
        System.out.println(msg + ": " + result);
        return false;
    }

    public static void main(String[] args) {
        long[] arr = {
                11, 12, 28, 496, 220, 1184, 12496, 1264460,
                790, 909, 562, 1064, 1488};

        LongStream.rangeClosed(1, 10).forEach(n -> aliquot(n, 16, 1L << 47));
        System.out.println();
        Arrays.stream(arr).forEach(n -> aliquot(n, 16, 1L << 47));
    }
}
```



```txt
Terminating: [1, 0]
Terminating: [2, 1, 0]
Terminating: [3, 1, 0]
Terminating: [4, 3, 1, 0]
Terminating: [5, 1, 0]
Perfect: [6]
Terminating: [7, 1, 0]
Terminating: [8, 7, 1, 0]
Terminating: [9, 4, 3, 1, 0]
Terminating: [10, 8, 7, 1, 0]

Terminating: [11, 1, 0]
Terminating: [12, 16, 15, 9, 4, 3, 1, 0]
Perfect: [28]
Perfect: [496]
Amicable: [220, 284]
Amicable: [1184, 1210]
Sociable of length 5: [12496, 14288, 15472, 14536, 14264]
Sociable of length 4: [1264460, 1547860, 1727636, 1305184]
Aspiring: [790, 650, 652, 496]
Aspiring: [909, 417, 143, 25, 6]
Cyclic back to 284: [562, 284, 220]
Cyclic back to 1184: [1064, 1336, 1184, 1210]
Non-terminating: [1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 
55056, 95728, 96720, 236592, 459792, 881392, 882384, 1474608]
```



## jq

{{works with|jq|1.4}}

```jq
# "until" is available in more recent versions of jq
# than jq 1.4
def until(cond; next):
  def _until:
    if cond then . else (next|_until) end;
  _until;

# unordered
def proper_divisors:
  . as $n
  | if $n > 1 then 1,
      ( range(2; 1 + (sqrt|floor)) as $i
        | if ($n % $i) == 0 then $i,
            (($n / $i) | if . == $i then empty else . end)
          else empty
          end)
    else empty
    end;

# sum of proper divisors, or 0
def pdsum:
  [proper_divisors] | add // 0;

# input is n
# maxlen defaults to 16;
# maxterm defaults to 2^47
def aliquot(maxlen; maxterm):
  (maxlen // 15) as $maxlen
  | (maxterm // 40737488355328) as $maxterm
  | if . == 0 then "terminating at 0" 
    else
    # [s, slen, new] = [[n], 1, n]
    [ [.], 1, .]
    | until( type == "string" or .[1] > $maxlen or .[2] > $maxterm;
             .[0] as $s | .[1] as $slen
             | ($s | .[length-1] | pdsum) as $new
             | if ($s|index($new)) then
                 if $s[0] == $new then
                     if $slen == 1 then "perfect \($s)"
                     elif $slen == 2 then "amicable: \($s)"
                     else "sociable of length \($slen): \($s)"
		     end
                 elif ($s | .[length-1]) == $new then "aspiring: \($s)"
                 else "cyclic back to \($new): \($s)"
		 end
               elif $new == 0 then "terminating: \($s + [0])"
               else [ ($s + [$new]), ($slen + 1), $new ]
               end )
    | if type == "string" then . else "non-terminating: \(.[0])" end
    end;
 
def task:
  def pp: "\(.): \(aliquot(null;null))";
     (range(1; 11) | pp),
     "",
     ((11, 12, 28, 496, 220, 1184, 12496, 1264460,
      790, 909, 562, 1064, 1488, 15355717786080) | pp);
        
task
```

{{out}}

```sh
$ jq -n -r -f aliquot.jq
1: terminating: [1,0]
2: terminating: [2,1,0]
3: terminating: [3,1,0]
4: terminating: [4,3,1,0]
5: terminating: [5,1,0]
6: perfect [6]
7: terminating: [7,1,0]
8: terminating: [8,7,1,0]
9: terminating: [9,4,3,1,0]
10: terminating: [10,8,7,1,0]

11: terminating: [11,1,0]
12: terminating: [12,16,15,9,4,3,1,0]
28: perfect [28]
496: perfect [496]
220: amicable: [220,284]
1184: amicable: [1184,1210]
12496: sociable of length 5: [12496,14288,15472,14536,14264]
1264460: sociable of length 4: [1264460,1547860,1727636,1305184]
790: aspiring: [790,650,652,496]
909: aspiring: [909,417,143,25,6]
562: cyclic back to 284: [562,284,220]
1064: cyclic back to 1184: [1064,1336,1184,1210]
1488: non-terminating: [1488,2480,3472,4464,8432,9424,10416,21328,22320,55056,95728,96720,236592,459792,881392,882384]
15355717786080: non-terminating: [15355717786080,44534663601120]
```



## Julia

'''Core Function'''

```Julia

function aliquotclassifier{T<:Integer}(n::T)
    a = T[n]
    b = divisorsum(a[end])
    len = 1
    while len < 17 && !(b in a) && 0 < b && b < 2^47+1
        push!(a, b)
        b = divisorsum(a[end])
        len += 1
    end
    if b in a
        1 < len || return ("Perfect", a)
        if b == a[1]
            2 < len || return ("Amicable", a)
            return ("Sociable", a)
        elseif b == a[end]
            return ("Aspiring", a)
        else
            return ("Cyclic", push!(a, b))
        end
    end
    push!(a, b)
    b != 0 || return ("Terminating", a)
    return ("Non-terminating", a)
end

```


'''Supporting Functions'''

```Julia

function pcontrib{T<:Integer}(p::T, a::T)
    n = one(T)
    pcon = one(T)
    for i in 1:a
        n *= p
        pcon += n
    end
    return pcon
end

function divisorsum{T<:Integer}(n::T)
    dsum = one(T)
    for (p, a) in factor(n)
        dsum *= pcontrib(p, a)
    end
    dsum -= n
end

```


'''Main'''

```Julia
using Printf

println("Classification Tests:")
tests = [1:12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488]
for i in tests
    (class, a) = aliquotclassifier(i)
    println(@sprintf("%8d => ", i), @sprintf("%16s, ", class), a)
end

```


{{out}}

```txt

Classification Tests:
       1 =>      Terminating, [1,0]
       2 =>      Terminating, [2,1,0]
       3 =>      Terminating, [3,1,0]
       4 =>      Terminating, [4,3,1,0]
       5 =>      Terminating, [5,1,0]
       6 =>          Perfect, [6]
       7 =>      Terminating, [7,1,0]
       8 =>      Terminating, [8,7,1,0]
       9 =>      Terminating, [9,4,3,1,0]
      10 =>      Terminating, [10,8,7,1,0]
      11 =>      Terminating, [11,1,0]
      12 =>      Terminating, [12,16,15,9,4,3,1,0]
      28 =>          Perfect, [28]
     496 =>          Perfect, [496]
     220 =>         Amicable, [220,284]
    1184 =>         Amicable, [1184,1210]
   12496 =>         Sociable, [12496,14288,15472,14536,14264]
 1264460 =>         Sociable, [1264460,1547860,1727636,1305184]
     790 =>         Aspiring, [790,650,652,496]
     909 =>         Aspiring, [909,417,143,25,6]
     562 =>           Cyclic, [562,284,220,284]
    1064 =>           Cyclic, [1064,1336,1184,1210,1184]
    1488 =>  Non-terminating, [1488,2480,3472,4464,8432,9424,10416,21328,22320,55056,95728,96720,236592,459792,881392,882384,1474608,2461648]

```



## Kotlin


```scala
// version 1.1.3

data class Classification(val sequence: List<Long>, val aliquot: String)

const val THRESHOLD = 1L shl 47

fun sumProperDivisors(n: Long): Long {
    if (n < 2L) return 0L
    val sqrt = Math.sqrt(n.toDouble()).toLong()
    var sum = 1L + (2L..sqrt)
        .filter { n % it == 0L }
        .map { it + n / it }
        .sum()
    if (sqrt * sqrt == n) sum -= sqrt
    return sum
}

fun classifySequence(k: Long): Classification {
    require(k > 0)
    var last = k
    val seq = mutableListOf(k)
    while (true) {
        last = sumProperDivisors(last)
        seq.add(last)
        val n = seq.size
        val aliquot = when {
            last == 0L                  -> "Terminating"
            n == 2 && last == k         -> "Perfect"
            n == 3 && last == k         -> "Amicable"
            n >= 4 && last == k         -> "Sociable[${n - 1}]"
            last == seq[n - 2]          -> "Aspiring"
            last in seq.slice(1..n - 3) -> "Cyclic[${n - 1 - seq.indexOf(last)}]"
            n == 16 || last > THRESHOLD -> "Non-Terminating"
            else                        -> ""
        }
        if (aliquot != "") return Classification(seq, aliquot)
    }
}

fun main(args: Array<String>) {
    println("Aliqot classifications - periods for Sociable/Cyclic in square brackets:\n")
    for (k in 1L..10) {
        val (seq, aliquot) = classifySequence(k)
        println("${"%2d".format(k)}: ${aliquot.padEnd(15)} $seq")
    }

    val la = longArrayOf(
        11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488
    )
    println()

    for (k in la) {
        val (seq, aliquot) = classifySequence(k)
        println("${"%7d".format(k)}: ${aliquot.padEnd(15)} $seq")
    }

    println()

    val k = 15355717786080L
    val (seq, aliquot) = classifySequence(k)
    println("$k: ${aliquot.padEnd(15)} $seq")
}
```


{{out}}

```txt

Aliqot classifications - periods for Sociable/Cyclic in square brackets:

 1: Terminating     [1, 0]
 2: Terminating     [2, 1, 0]
 3: Terminating     [3, 1, 0]
 4: Terminating     [4, 3, 1, 0]
 5: Terminating     [5, 1, 0]
 6: Perfect         [6, 6]
 7: Terminating     [7, 1, 0]
 8: Terminating     [8, 7, 1, 0]
 9: Terminating     [9, 4, 3, 1, 0]
10: Terminating     [10, 8, 7, 1, 0]

     11: Terminating     [11, 1, 0]
     12: Terminating     [12, 16, 15, 9, 4, 3, 1, 0]
     28: Perfect         [28, 28]
    496: Perfect         [496, 496]
    220: Amicable        [220, 284, 220]
   1184: Amicable        [1184, 1210, 1184]
  12496: Sociable[5]     [12496, 14288, 15472, 14536, 14264, 12496]
1264460: Sociable[4]     [1264460, 1547860, 1727636, 1305184, 1264460]
    790: Aspiring        [790, 650, 652, 496, 496]
    909: Aspiring        [909, 417, 143, 25, 6, 6]
    562: Cyclic[2]       [562, 284, 220, 284]
   1064: Cyclic[2]       [1064, 1336, 1184, 1210, 1184]
   1488: Non-Terminating [1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384]

15355717786080: Non-Terminating [15355717786080, 44534663601120, 144940087464480]

```



## Liberty BASIC

Based on my analysis of integers up to 10,000 I have revised the criteria for non-termination as follows: 52 elements, or 11 consecutive increases of elements, or an element greater than 30 million. This is not a perfect algorithm, but seems to me to be a reasonable compromise between accuracy and speed. I'll stay away from the really large numbers - at least for now.

Of integers below 10,000--

4004 is the longest non-terminating integer by the revised criteria. The elements range from a minimum of 2,440 to a maximum of 302,666. I suspect that if the sequence were run out far enough, it would terminate in some fashion.

4344 has the longest terminating sequence. 

6672 has the longest aspiring sequence.

6420 has the longest cyclic sequence.

8128 is the largest perfect integer.

There are no sociable sequences.

```lb

print "ROSETTA CODE - Aliquot sequence classifications"
[Start]
input "Enter an integer: "; K
K=abs(int(K)): if K=0 then goto [Quit]
call PrintAS K
goto [Start]

[Quit]
print "Program complete."
end

sub PrintAS K
    Length=52
    dim Aseq(Length)
    n=K: class=0
    for element=2 to Length
        Aseq(element)=PDtotal(n)
        print Aseq(element); " ";
        select case
            case Aseq(element)=0
                print " terminating": class=1: exit for
            case Aseq(element)=K and element=2
                print " perfect": class=2: exit for
            case Aseq(element)=K and element=3
                print " amicable": class=3: exit for
            case Aseq(element)=K and element>3
                print " sociable": class=4: exit for
            case Aseq(element)<>K and Aseq(element-1)=Aseq(element)
                print " aspiring": class=5: exit for
            case Aseq(element)<>K and Aseq(element-2)= Aseq(element)
                print " cyclic": class=6: exit for
        end select
        n=Aseq(element)
        if n>priorn then priorn=n: inc=inc+1 else inc=0: priorn=0
        if inc=11 or n>30000000 then exit for
    next element
    if class=0 then print " non-terminating"
end sub

function PDtotal(n)
    for y=2 to n
        if (n mod y)=0 then PDtotal=PDtotal+(n/y)
    next
end function

```


{{out}}

```txt

ROSETTA CODE - Aliquot sequence classifications
Enter an integer: 1
0  terminating
Enter an integer: 2
1 0  terminating
Enter an integer: 3
1 0  terminating
Enter an integer: 4
3 1 0  terminating
Enter an integer: 5
1 0  terminating
Enter an integer: 6
6  perfect
Enter an integer: 7
1 0  terminating
Enter an integer: 8
7 1 0  terminating
Enter an integer: 9
4 3 1 0  terminating
Enter an integer: 10
8 7 1 0  terminating
Enter an integer: 11
1 0  terminating
Enter an integer: 12
16 15 9 4 3 1 0  terminating
Enter an integer: 28
28  perfect
Enter an integer: 496
496  perfect
Enter an integer: 220
284 220  amicable
Enter an integer: 1184
1210 1184  amicable
Enter an integer: 12496
14288 15472 14536 14264 12496  sociable
Enter an integer: 1264460
1547860 1727636 1305184 1264460  sociable
Enter an integer: 790
650 652 496 496  aspiring
Enter an integer: 909
417 143 25 6 6  aspiring
Enter an integer: 562
284 220 284  cyclic
Enter an integer: 1064
1336 1184 1210 1184  cyclic
Enter an integer: 1488
2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720  non-terminating
- - - - - - - - - - - -
Enter an integer: 4004
5404 5460 13356 25956 49756 49812 83244 138964 144326 127978 67322 36250 34040 48040 60140 71572 58208 64264 60836 47692 35776 42456 69144 110376 244824 373356 594884 446170 356954 219706 118874 88720 117740 174916 174972 291844 302666 2564
38 217322 185014 92510 95626 49274 25894 17198 8602 6950 6070 4874 2440 3140  non-terminating
Enter an integer: 4344
6576 10536 15864 23856 47568 75440 112048 111152 104236 105428 79078 45842 22924 20924 15700 18586 9296 11536 14256 30756 47868 63852 94404 125900 147520 204524 153400 237200 333634 238334 121306 62438 31222 16514 9406 4706 2938 1850 1684 1
270 1034 694 350 394 200 265 59 1 0  terminating
Enter an integer: 6672
10688 10648 11312 13984 16256 16384 16383 6145 1235 445 95 25 6 6  aspiring
Enter an integer: 6420
11724 15660 34740 71184 112832 121864 106646 53326 45458 37486 18746 16198 14042 11878 5942 2974 1490 1210 1184 1210  cyclic
Enter an integer: 8128
8128  perfect
Enter an integer:
Program complete.

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
seq[n_] := 
  NestList[If[# == 0, 0, 
     DivisorSum[#, # &, Function[div, div != #]]] &, n, 16];
class[seq_] := 
  Which[Length[seq] < 2, "Non-terminating", MemberQ[seq, 0], 
   "Terminating", seq[[1]] == seq[[2]], "Perfect", 
   Length[seq] > 2 && seq[[1]] == seq[[3]], "Amicable", 
   Length[seq] > 3 && MemberQ[seq[[4 ;;]], seq[[1]]], "Sociable", 
   MatchQ[class[Rest[seq]], "Perfect" | "Aspiring"], "Aspiring", 
   MatchQ[class[Rest[seq]], "Amicable" | "Sociable" | "Cyclic"], 
   "Cyclic", True, "Non-terminating"];
notate[seq_] := 
  Which[seq == {}, {}, 
   MemberQ[Rest[seq], 
    seq[[1]]], {Prepend[TakeWhile[Rest[seq], # != seq[[1]] &], 
     seq[[1]]]}, True, Prepend[notate[Rest[seq]], seq[[1]]]];
Print[{#, class[seq[#]], notate[seq[#]] /. {0} -> 0}] & /@ {1, 2, 3, 4, 5, 6, 7, 
   8, 9, 10, 11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 
   562, 1064, 1488, 15355717786080};
```

{{out}}

```txt
{1, Terminating, {1, 0}}
{2, Terminating, {2, 1, 0}}
{3, Terminating, {3, 1, 0}}
{4, Terminating, {4, 3, 1, 0}}
{5, Terminating, {5, 1, 0}}
{6, Perfect, {{6}}}
{7, Terminating, {7, 1, 0}}
{8, Terminating, {8, 7, 1, 0}}
{9, Terminating, {9, 4, 3, 1, 0}}
{10, Terminating, {10, 8, 7, 1, 0}}
{11, Terminating, {11, 1, 0}}
{12, Terminating, {12, 16, 15, 9, 4, 3, 1, 0}}
{28, Perfect, {{28}}}
{496, Perfect, {{496}}}
{220, Amicable, {{220, 284}}}
{1184, Amicable, {{1184, 1210}}}
{12496, Sociable, {{12496, 14288, 15472, 14536, 14264}}}
{1264460, Sociable, {{1264460, 1547860, 1727636, 1305184}}}
{790, Aspiring, {790, 650, 652, {496}}}
{909, Aspiring, {909, 417, 143, 25, {6}}}
{562, Cyclic, {562, {284, 220}}}
{1064, Cyclic, {1064, 1336, {1184, 1210}}}
{1488, Non-terminating, {1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384, 1474608}}
{15355717786080, Non-terminating, {15355717786080, 44534663601120, 144940087464480, 471714103310688, 1130798979186912, 2688948041357088, 6050151708497568, 13613157922639968, 35513546724070632, 74727605255142168, 162658586225561832, 353930992506879768, 642678347124409032, 1125102611548462968, 1977286128289819992, 3415126495450394808, 7156435369823219592}}
```



## Oforth



```oforth
import: mapping
import: quicksort
import: math

Object method: sum ( coll -- m )
   #+ self reduce dup ifNull: [ drop 0 ] ;

Integer method: properDivs
| i l |
   Array new dup 1 over add ->l
   2 self nsqrt tuck for: i [ self i mod ifFalse: [ i l add  self i / l add ] ]
   sq self == ifTrue: [ l pop drop ]
   dup sort
;
 
: aliquot( n -- [] )	\ Returns aliquot sequence of n
| end l |
   2 47 pow ->end
   Array new dup n over add ->l
   while ( l size 16 <  l last 0 <> and  l last end <= and ) [ l last properDivs sum  l add ]
;
 
: aliquotClass( n -- [] s )   \ Returns aliquot sequence and classification 
| l i j |
   n aliquot dup ->l
   l last 0   == ifTrue: [ "terminate" return ]
   l second n == ifTrue: [ "perfect" return ]
   3 l at   n == ifTrue: [ "amicable" return ]
   l indexOfFrom(n, 2) ifNotNull: [ "sociable" return ]
 
   l size loop: i [ 
      l indexOfFrom(l at(i), i 1+ ) -> j
      j i 1+ == ifTrue: [ "aspiring" return ]
      j ifNotNull: [ "cyclic" return ]
      ]
   "non-terminating"
;
```


{{out}}

```txt

>#[ dup . aliquotClass . ":" . . printcr ] 10 each
1 terminate : [1, 0]
2 terminate : [2, 1, 0]
3 terminate : [3, 1, 0]
4 terminate : [4, 3, 1, 0]
5 terminate : [5, 1, 0]
6 perfect : [6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6]
7 terminate : [7, 1, 0]
8 terminate : [8, 7, 1, 0]
9 terminate : [9, 4, 3, 1, 0]
10 terminate : [10, 8, 7, 1, 0]
ok

```



```txt

>#[ dup . aliquotClass . ":" . . printcr ] [ 11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488, 15355717786080 ] apply
11 terminate : [11, 1, 0]
12 terminate : [12, 16, 15, 9, 4, 3, 1, 0]
28 perfect : [28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28]
496 perfect : [496, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496]
220 amicable : [220, 284, 220, 284, 220, 284, 220, 284, 220, 284, 220, 284, 220, 284, 220, 284]
1184 amicable : [1184, 1210, 1184, 1210, 1184, 1210, 1184, 1210, 1184, 1210, 1184, 1210, 1184, 1210, 1184, 1210]
12496 sociable : [12496, 14288, 15472, 14536, 14264, 12496, 14288, 15472, 14536, 14264, 12496, 14288, 15472, 14536, 14264, 12496]
1264460 sociable : [1264460, 1547860, 1727636, 1305184, 1264460, 1547860, 1727636, 1305184, 1264460, 1547860, 1727636, 1305184, 1264460, 1547860, 1727636, 1305184]
790 aspiring : [790, 650, 652, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496, 496]
909 aspiring : [909, 417, 143, 25, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6]
562 cyclic : [562, 284, 220, 284, 220, 284, 220, 284, 220, 284, 220, 284, 220, 284, 220, 284]
1064 cyclic : [1064, 1336, 1184, 1210, 1184, 1210, 1184, 1210, 1184, 1210, 1184, 1210, 1184, 1210, 1184, 1210]
1488 non-terminating : [1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384]
15355717786080 non-terminating : [15355717786080, 44534663601120, 144940087464480]
ok
>

```



## PARI/GP

Define function aliquot(). Works with recent versions of PARI/GP >= 2.8:

```parigp
aliquot(x) =
{
  my (L = List(x), M = Map(Mat([x,1])), k, m = "non-term.", n = x);

  for (i = 2, 16, n = vecsum(divisors(n)) - n;
    if (n > 2^47, break,
        n == 0, m = "terminates"; break,
        mapisdefined(M, n, &k),
        m = if (k == 1,
              if (i == 2, "perfect",
                  i == 3, "amicable",
                  i > 3, concat("sociable-",i-1)),
                k < i-1, concat("cyclic-",i-k),
              "aspiring"); break,
        mapput(M, n, i); listput(L, n));
  );
  printf("%16d: %10s, %s\n", x, m, Vec(L));
}
```


Output:

```txt
gp > apply(aliquot, concat([1..10],[11,12,28,496,220,1184,12496,1264460,790,909,562,1064,1488,15355717786080]));

               1: terminates, [1]
               2: terminates, [2, 1]
               3: terminates, [3, 1]
               4: terminates, [4, 3, 1]
               5: terminates, [5, 1]
               6:    perfect, [6]
               7: terminates, [7, 1]
               8: terminates, [8, 7, 1]
               9: terminates, [9, 4, 3, 1]
              10: terminates, [10, 8, 7, 1]
              11: terminates, [11, 1]
              12: terminates, [12, 16, 15, 9, 4, 3, 1]
              28:    perfect, [28]
             496:    perfect, [496]
             220:   amicable, [220, 284]
            1184:   amicable, [1184, 1210]
           12496: sociable-5, [12496, 14288, 15472, 14536, 14264]
         1264460: sociable-4, [1264460, 1547860, 1727636, 1305184]
             790:   aspiring, [790, 650, 652, 496]
             909:   aspiring, [909, 417, 143, 25, 6]
             562:   cyclic-2, [562, 284, 220]
            1064:   cyclic-2, [1064, 1336, 1184, 1210]
            1488:  non-term., [1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384]
  15355717786080:  non-term., [15355717786080, 44534663601120]
```



## Phix

Translated from the Python example

```Phix
function aliquot(atom n)
sequence s = {n}
integer k
    if n=0 then                             return {"terminating",{0}} end if
    while length(s)<16 
      and n<140737488355328 do
        n = sum(factors(n,-1))
        k = find(n,s)
        if k then
            if k=1 then
                if length(s)=1 then         return {"perfect",s}
                elsif length(s)=2 then      return {"amicable",s}
                end if                      return {"sociable",s}
            elsif k=length(s) then          return {"aspiring",s}
            end if                          return {"cyclic",append(s,n)}
        elsif n=0 then                      return {"terminating",s}
        end if
        s = append(s,n)
    end while
                                            return {"non-terminating",s}
end function

function flat_d(sequence s)
    for i=1 to length(s) do s[i] = sprintf("%d",s[i]) end for
    return join(s,",")
end function

constant n = tagset(12)&{28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488, 15355717786080}
sequence class, dseq
for i=1 to length(n) do
    {class, dseq} = aliquot(n[i])
    printf(1,"%14d => %15s, {%s}\n",{n[i],class,flat_d(dseq)})
end for
```

{{out}}

```txt

             1 =>     terminating, {1}
             2 =>     terminating, {2,1}
             3 =>     terminating, {3,1}
             4 =>     terminating, {4,3,1}
             5 =>     terminating, {5,1}
             6 =>         perfect, {6}
             7 =>     terminating, {7,1}
             8 =>     terminating, {8,7,1}
             9 =>     terminating, {9,4,3,1}
            10 =>     terminating, {10,8,7,1}
            11 =>     terminating, {11,1}
            12 =>     terminating, {12,16,15,9,4,3,1}
            28 =>         perfect, {28}
           496 =>         perfect, {496}
           220 =>        amicable, {220,284}
          1184 =>        amicable, {1184,1210}
         12496 =>        sociable, {12496,14288,15472,14536,14264}
       1264460 =>        sociable, {1264460,1547860,1727636,1305184}
           790 =>        aspiring, {790,650,652,496}
           909 =>        aspiring, {909,417,143,25,6}
           562 =>          cyclic, {562,284,220,284}
          1064 =>          cyclic, {1064,1336,1184,1210,1184}
          1488 => non-terminating, {1488,2480,3472,4464,8432,9424,10416,21328,22320,55056,95728,96720,236592,459792,881392,882384}
15355717786080 => non-terminating, {15355717786080,44534663601120,144940087464480}

```



## Perl

{{libheader|ntheory}}

```perl
use ntheory qw/divisor_sum/;

sub aliquot {
  my($n, $maxterms, $maxn) = @_;
  $maxterms = 16 unless defined $maxterms;
  $maxn = 2**47 unless defined $maxn;

  my %terms = ($n => 1);
  my @allterms = ($n);
  for my $term (2 .. $maxterms) {
    $n = divisor_sum($n)-$n;
    # push onto allterms here if we want the cyclic term to display
    last if $n > $maxn;
    return ("terminates",@allterms, 0) if $n == 0;
    if (defined $terms{$n}) {
      return ("perfect",@allterms)  if $term == 2 && $terms{$n} == 1;
      return ("amicible",@allterms) if $term == 3 && $terms{$n} == 1;
      return ("sociable-".($term-1),@allterms) if $term >  3 && $terms{$n} == 1;
      return ("aspiring",@allterms) if $terms{$n} == $term-1;
      return ("cyclic-".($term-$terms{$n}),@allterms)   if $terms{$n} < $term-1;
    }
    $terms{$n} = $term;
    push @allterms, $n;
  }
  ("non-term",@allterms);
}

for my $n (1..10) {
  my($class, @seq) = aliquot($n);
  printf "%14d %10s [@seq]\n", $n, $class;
}
print "\n";
for my $n (qw/11 12 28 496 220 1184 12496 1264460 790 909 562 1064 1488 15355717786080/) {
  my($class, @seq) = aliquot($n);
  printf "%14d %10s [@seq]\n", $n, $class;
}
```

{{out}}

```txt
             1 terminates [1 0]
             2 terminates [2 1 0]
             3 terminates [3 1 0]
             4 terminates [4 3 1 0]
             5 terminates [5 1 0]
             6    perfect [6]
             7 terminates [7 1 0]
             8 terminates [8 7 1 0]
             9 terminates [9 4 3 1 0]
            10 terminates [10 8 7 1 0]

            11 terminates [11 1 0]
            12 terminates [12 16 15 9 4 3 1 0]
            28    perfect [28]
           496    perfect [496]
           220   amicible [220 284]
          1184   amicible [1184 1210]
         12496 sociable-5 [12496 14288 15472 14536 14264]
       1264460 sociable-4 [1264460 1547860 1727636 1305184]
           790   aspiring [790 650 652 496]
           909   aspiring [909 417 143 25 6]
           562   cyclic-2 [562 284 220]
          1064   cyclic-2 [1064 1336 1184 1210]
          1488   non-term [1488 2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384]
15355717786080   non-term [15355717786080 44534663601120]
```



## Perl 6

{{works with|rakudo|2018.10}}

```perl6
sub propdivsum (\x) {
    my @l = x > 1;
    (2 .. x.sqrt.floor).map: -> \d {
        unless x % d { my \y = x div d; y == d ?? @l.push: d !! @l.append: d,y }
    }
    sum @l;
}

multi quality (0,1)  { 'perfect ' }
multi quality (0,2)  { 'amicable' }
multi quality (0,$n) { "sociable-$n" }
multi quality ($,1)  { 'aspiring' }
multi quality ($,$n) { "cyclic-$n" }

sub aliquotidian ($x) {
    my %seen;
    my @seq = $x, &propdivsum ... *;
    for 0..16 -> $to {
        my $this = @seq[$to] or return "$x\tterminating\t[@seq[^$to]]";
        last if $this > 140737488355328;
        if %seen{$this}:exists {
            my $from = %seen{$this};
            return "$x\t&quality($from, $to-$from)\t[@seq[^$to]]";
        }
        %seen{$this} = $to;
    }
    "$x non-terminating\t[{@seq}]";
}

aliquotidian($_).say for flat
    1..10,
    11, 12, 28, 496, 220, 1184, 12496, 1264460,
    790, 909, 562, 1064, 1488,
    15355717786080;
```

{{out}}

```txt
1	terminating	[1]
2	terminating	[2 1]
3	terminating	[3 1]
4	terminating	[4 3 1]
5	terminating	[5 1]
6	perfect 	[6]
7	terminating	[7 1]
8	terminating	[8 7 1]
9	terminating	[9 4 3 1]
10	terminating	[10 8 7 1]
11	terminating	[11 1]
12	terminating	[12 16 15 9 4 3 1]
28	perfect 	[28]
496	perfect 	[496]
220	amicable	[220 284]
1184	amicable	[1184 1210]
12496	sociable-5	[12496 14288 15472 14536 14264]
1264460	sociable-4	[1264460 1547860 1727636 1305184]
790	aspiring	[790 650 652 496]
909	aspiring	[909 417 143 25 6]
562	cyclic-2	[562 284 220]
1064	cyclic-2	[1064 1336 1184 1210]
1488 non-terminating	[1488 2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384 1474608 ...]
15355717786080 non-terminating	[15355717786080 44534663601120 144940087464480 ...]

```



## PowerShell

{{works with|PowerShell|2.0}}
To make the PowerShell 4.0 code below work with PowerShell 2.0:<br/>
Replace any instances of ".Where{...}" with " | Where {...}"<br/>
Replace any instances of ".ForEach{...}" with " | ForEach {...}"
{{works with|PowerShell|3.0}}
To make the PowerShell 4.0 code below work with PowerShell 3.0:<br/>
Replace any instances of ".Where{...}" with ".Where({...})"<br/>
Replace any instances of ".ForEach{...}" with ".ForEach({...})"
{{works with|PowerShell|4.0}}<br/>
<b>Simple</b>

```powershell
function Get-NextAliquot ( [int]$X )
    {
    If ( $X -gt 1 )
        {
        $NextAliquot = 0
        (1..($X/2)).Where{ $x % $_ -eq 0 }.ForEach{ $NextAliquot += $_ }.Where{ $_ }
        return $NextAliquot
        }
    }
 
function Get-AliquotSequence ( [int]$K, [int]$N )
    {
    $X = $K
    $X
    (1..($N-1)).ForEach{ $X = Get-NextAliquot $X; $X }
    }
 
function Classify-AlliquotSequence ( [int[]]$Sequence )
    {
    $K = $Sequence[0]
    $LastN = $Sequence.Count
    If ( $Sequence[-1] -eq 0 ) { return "terminating" }
    If ( $Sequence[-1] -eq 1 ) { return "terminating" }
    If ( $Sequence[1] -eq $K ) { return "perfect"     }
    If ( $Sequence[2] -eq $K ) { return "amicable"    }
    If ( $Sequence[3..($Sequence.Count-1)] -contains $K ) { return "sociable" }
    If ( $Sequence[-1] -eq $Sequence[-2] ) { return "aspiring" }
    If ( $Sequence.Count -gt ( $Sequence | Select -Unique ).Count ) { return "cyclic" }
    return "non-terminating and non-repeating through N = $($Sequence.Count)"
    }
 
(1..10).ForEach{ [string]$_ + " is " + ( Classify-AlliquotSequence -Sequence ( Get-AliquotSequence -K $_ -N 16 ) ) }
 
( 11, 12, 28, 496, 220, 1184, 790, 909, 562, 1064, 1488 ).ForEach{ [string]$_ + " is " + ( Classify-AlliquotSequence -Sequence ( Get-AliquotSequence -K $_ -N 16 ) ) }
```

<b>Optimized</b>

```powershell
function Get-NextAliquot ( [int]$X )
    {
    If ( $X -gt 1 )
        {
        $NextAliquot = 1
        If ( $X -gt 2 )
            {
            $XSquareRoot = [math]::Sqrt( $X )
 
            (2..$XSquareRoot).Where{ $X % $_ -eq 0 }.ForEach{ $NextAliquot += $_ + $x / $_ }
 
            If ( $XSquareRoot % 1 -eq 0 ) { $NextAliquot -= $XSquareRoot }
            }
        return $NextAliquot
        }
    }
 
function Get-AliquotSequence ( [int]$K, [int]$N )
    {
    $X = $K
    $X
    $i = 1
    While ( $X -and $i -lt $N )
        {
        $i++
        $Next = Get-NextAliquot $X
        If ( $Next )
            {
            If ( $X -eq $Next )
                {
                ($i..$N).ForEach{ $X }
                $i = $N
                }
            Else
                {
                $X = $Next
                $X
                }
            }
        Else
            {
            $i = $N
            }
        }
    }
 
function Classify-AlliquotSequence ( [int[]]$Sequence )
    {
    $K = $Sequence[0]
    $LastN = $Sequence.Count
    If ( $Sequence[-1] -eq 0 ) { return "terminating" }
    If ( $Sequence[-1] -eq 1 ) { return "terminating" }
    If ( $Sequence[1] -eq $K ) { return "perfect"     }
    If ( $Sequence[2] -eq $K ) { return "amicable"    }
    If ( $Sequence[3..($Sequence.Count-1)] -contains $K ) { return "sociable" }
    If ( $Sequence[-1] -eq $Sequence[-2] ) { return "aspiring" }
    If ( $Sequence.Count -gt ( $Sequence | Select -Unique ).Count ) { return "cyclic" }
    return "non-terminating and non-repeating through N = $($Sequence.Count)"
    }
 
(1..10).ForEach{ [string]$_ + " is " + ( Classify-AlliquotSequence -Sequence ( Get-AliquotSequence -K $_ -N 16 ) ) }
 
( 11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488 ).ForEach{ [string]$_ + " is " + ( Classify-AlliquotSequence -Sequence ( Get-AliquotSequence -K $_ -N 16 ) ) }
```

{{out}}

```txt
1 is terminating
2 is terminating
3 is terminating
4 is terminating
5 is terminating
6 is perfect
7 is terminating
8 is terminating
9 is terminating
10 is terminating
11 is terminating
12 is terminating
28 is perfect
496 is perfect
220 is amicable
1184 is amicable
12496 is sociable
1264460 is sociable
790 is aspiring
909 is aspiring
562 is cyclic
1064 is cyclic
1488 is non-terminating and non-repeating through N = 16
```



### Version 3.0


```PowerShell

function Get-Aliquot
{
    [CmdletBinding()]
    [OutputType([PScustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true)]
        [int]
        $InputObject
    )

    Begin
    {
        function Get-NextAliquot ([int]$X)
        {
            if ($X -gt 1)
            {
                $nextAliquot = 1

                if ($X -gt 2)
                {
                    $xSquareRoot = [Math]::Sqrt($X)
 
                    2..$xSquareRoot | Where-Object {$X % $_ -eq 0} | ForEach-Object {$nextAliquot += $_ + $x / $_}
 
                    if ($xSquareRoot % 1 -eq 0) {$nextAliquot -= $xSquareRoot}
                }

                $nextAliquot
            }
        }
 
        function Get-AliquotSequence ([int]$K, [int]$N)
        {
            $X = $K
            $X
            $i = 1

            while ($X -and $i -lt $N)
            {
                $i++
                $next = Get-NextAliquot $X

                if ($next)
                {
                    if ($X -eq $next)
                    {
                        $i..$N  | ForEach-Object {$X}
                        $i = $N
                    }
                    else
                    {
                        $X = $next
                        $X
                    }
                }
                else
                {
                    $i = $N
                }
            }
        }
 
        function Classify-AlliquotSequence ([int[]]$Sequence)
        {
            $k = $Sequence[0]

            if ($Sequence[-1] -eq 0)                                     {return "terminating"}
            if ($Sequence[-1] -eq 1)                                     {return "terminating"}
            if ($Sequence[1]  -eq $k)                                    {return "perfect"    }
            if ($Sequence[2]  -eq $k)                                    {return "amicable"   }
            if ($Sequence[3..($Sequence.Count-1)] -contains $k)          {return "sociable"   }
            if ($Sequence[-1] -eq $Sequence[-2] )                        {return "aspiring"   }
            if ($Sequence.Count -gt ($Sequence | Select -Unique).Count ) {return "cyclic"     }

            return "non-terminating and non-repeating through N = $($Sequence.Count)"
        }
    }
    Process
    {
        $_ | ForEach-Object {
            [PSCustomObject]@{
                Number         = $_
                Classification = (Classify-AlliquotSequence -Sequence (Get-AliquotSequence -K $_ -N 16))
            }
        }
    }
}

```


```PowerShell

$oneToTen = 1..10 | Get-Aliquot
$selected = 11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488 | Get-Aliquot

$numbers = $oneToTen, $selected
$numbers

```

{{Out}}

```txt

 Number Classification                                  
 ------ --------------                                  
      1 terminating                                     
      2 terminating                                     
      3 terminating                                     
      4 terminating                                     
      5 terminating                                     
      6 perfect                                         
      7 terminating                                     
      8 terminating                                     
      9 terminating                                     
     10 terminating                                     
     11 terminating                                     
     12 terminating                                     
     28 perfect                                         
    496 perfect                                         
    220 amicable                                        
   1184 amicable                                        
  12496 sociable                                        
1264460 sociable                                        
    790 aspiring                                        
    909 aspiring                                        
    562 cyclic                                          
   1064 cyclic                                          
   1488 non-terminating and non-repeating through N = 16

```



## Python

Importing [[Proper_divisors#Python:_From_prime_factors|Proper divisors from prime factors]]:


```python
from proper_divisors import proper_divs
from functools import lru_cache


@lru_cache()
def pdsum(n): 
    return sum(proper_divs(n))
    
    
def aliquot(n, maxlen=16, maxterm=2**47):
    if n == 0:
        return 'terminating', [0]
    s, slen, new = [n], 1, n
    while slen <= maxlen and new < maxterm:
        new = pdsum(s[-1])
        if new in s:
            if s[0] == new:
                if slen == 1:
                    return 'perfect', s
                elif slen == 2:
                    return 'amicable', s
                else:
                    return 'sociable of length %i' % slen, s
            elif s[-1] == new:
                return 'aspiring', s
            else:
                return 'cyclic back to %i' % new, s
        elif new == 0:
            return 'terminating', s + [0]
        else:
            s.append(new)
            slen += 1
    else:
        return 'non-terminating', s
                
if __name__ == '__main__':
    for n in range(1, 11): 
        print('%s: %r' % aliquot(n))
    print()
    for n in [11, 12, 28, 496, 220, 1184,  12496, 1264460, 790, 909, 562, 1064, 1488, 15355717786080]: 
        print('%s: %r' % aliquot(n))
```


{{out}}

```txt
terminating: [1, 0]
terminating: [2, 1, 0]
terminating: [3, 1, 0]
terminating: [4, 3, 1, 0]
terminating: [5, 1, 0]
perfect: [6]
terminating: [7, 1, 0]
terminating: [8, 7, 1, 0]
terminating: [9, 4, 3, 1, 0]
terminating: [10, 8, 7, 1, 0]

terminating: [11, 1, 0]
terminating: [12, 16, 15, 9, 4, 3, 1, 0]
perfect: [28]
perfect: [496]
amicable: [220, 284]
amicable: [1184, 1210]
sociable of length 5: [12496, 14288, 15472, 14536, 14264]
sociable of length 4: [1264460, 1547860, 1727636, 1305184]
aspiring: [790, 650, 652, 496]
aspiring: [909, 417, 143, 25, 6]
cyclic back to 284: [562, 284, 220]
cyclic back to 1184: [1064, 1336, 1184, 1210]
non-terminating: [1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384, 1474608]
non-terminating: [15355717786080, 44534663601120, 144940087464480]
```



## Racket


'''fold-divisors''' is used from [[Proper_divisors#Racket]], but for the truly big numbers, we use divisors from math/number-theory.


```racket
#lang racket
(require "proper-divisors.rkt" math/number-theory)

(define SCOPE 20000)

(define P
  (let ((P-v (vector)))
    (λ (n)
      (cond
        [(> n SCOPE)
         (apply + (drop-right (divisors n) 1))]
        [else
         (set! P-v (fold-divisors P-v n 0 +))
         (vector-ref P-v n)]))))

;; initialise P-v
(void (P SCOPE))

(define (aliquot-sequence-class K)
  ;; note that seq is reversed as a list, since we're consing
  (define (inr-asc seq)
    (match seq
      [(list 0 _ ...)
       (values "terminating" seq)]
      [(list (== K) (== K) _ ...)
       (values "perfect" seq)]
      [(list n n _ ...)
       (values (format "aspiring to ~a" n) seq)]
      [(list (== K) ami (== K) _ ...)
       (values (format "amicable with ~a" ami) seq)]
      [(list (== K) cycle ... (== K))
       (values (format "sociable length ~a" (add1 (length cycle))) seq)]
      [(list n cycle ... n _ ...)
       (values (format "cyclic on ~a length ~a" n (add1 (length cycle))) seq)]
      [(list X _ ...)
       #:when (> X 140737488355328)
       (values "non-terminating big number" seq)]
      [(list seq ...)
       #:when (> (length seq) 16)
       (values "non-terminating long sequence" seq)]
      [(list seq1 seq ...) (inr-asc (list* (P seq1) seq1 seq))]))
(inr-asc (list K)))

(define (report-aliquot-sequence-class n)
  (define-values (c s) (aliquot-sequence-class n))
  (printf "~a:\t~a\t~a~%" n c (reverse s)))

(for ((i (in-range 1 10)))
  (report-aliquot-sequence-class i))
(newline)

(for ((i (in-list '(11 12 28 496 220 1184 12496 1264460 790 909 562 1064 1488 15355717786080))))
  (report-aliquot-sequence-class i))
```


{{out}}

```txt
1:	terminating	(1 0)
2:	terminating	(2 1 0)
3:	terminating	(3 1 0)
4:	terminating	(4 3 1 0)
5:	terminating	(5 1 0)
6:	perfect	(6 6)
7:	terminating	(7 1 0)
8:	terminating	(8 7 1 0)
9:	terminating	(9 4 3 1 0)

11:	terminating	(11 1 0)
12:	terminating	(12 16 15 9 4 3 1 0)
28:	perfect	(28 28)
496:	perfect	(496 496)
220:	amicable with 284	(220 284 220)
1184:	amicable with 1210	(1184 1210 1184)
12496:	sociable length 5	(12496 14288 15472 14536 14264 12496)
1264460:	sociable length 4	(1264460 1547860 1727636 1305184 1264460)
790:	aspiring to 496	(790 650 652 496 496)
909:	aspiring to 6	(909 417 143 25 6 6)
562:	cyclic on 284 length 2	(562 284 220 284)
1064:	cyclic on 1184 length 2	(1064 1336 1184 1210 1184)
1488:	non-terminating long sequence	(1488 2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384 1474608)
15355717786080:	non-terminating big number	(15355717786080 44534663601120 144940087464480)

```



## REXX

Programming notes:

This REXX version uses memoization.

Two versions of   ''classifications''   of   ''non-terminating''   are used:
::*   (lowercase)   '''non-terminating'''           ───   due to more than sixteen cyclic numbers
::*   (uppercase)   '''NON-TERMINATING'''                          ───   due to a cyclic number that is larger than <big>2<sup>47</sup></big>

Both of the above limitations are imposed by this Rosetta Code task's restriction requirements:   ''For the purposes of this task, ···''.  

```rexx
/*REXX program classifies various  positive integers  for  types of  aliquot sequences. */
parse arg low high L                             /*obtain optional arguments from the CL*/
high=word(high low 10,1);    low=word(low 1,1)   /*obtain the  LOW  and  HIGH  (range). */
if L=''  then  L=11 12 28 496 220 1184 12496 1264460 790 909 562 1064 1488 15355717786080
numeric digits 100                               /*be able to compute the number:  BIG  */
big= 2**47;                  NTlimit= 16 + 1     /*limits for a non─terminating sequence*/
numeric digits max(9, 1 + length(big) )          /*be able to handle big numbers for // */
#.=.;    #.0=0;     #.1=0                        /*#.   are the proper divisor sums.    */
say center('numbers from '      low      " to "      high, 79, "═")
         do n=low  to high;    call classify  n  /*call a subroutine to classify number.*/
         end   /*n*/                             /* [↑]   process a range of integers.  */
say
say center('first numbers for each classification', 79, "═")
class.=0                                         /* [↓]  ensure one number of each class*/
         do q=1  until class.sociable\==0        /*the only one that has to be counted. */
         call classify  -q                       /*minus (-) sign indicates don't tell. */
         _=what;   upper _;  class._= class._ +1 /*bump counter for this class sequence.*/
         if class._==1  then say right(q, digits())     'is'     center(what, 15)     $
         end   /*q*/                             /* [↑]  only display the 1st occurrence*/
say                                              /* [↑]  process until all classes found*/
say center('classifications for specific numbers', 79, "═")
         do i=1  for words(L)                    /*L:   is a list of  "special numbers".*/
         call classify   word(L, i)              /*call a subroutine to classify number.*/
         end   /*i*/                             /* [↑]  process a list of integers.    */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
classify: parse arg a 1 aa;     a=abs(a)         /*obtain number that's to be classified*/
          if #.a\==.   then s=#.a                /*Was this number been  summed  before?*/
                       else s=sigma(a)           /*No, then classify number the hard way*/
          #.a=s;  $=s                            /*define sum of the  proper divisors.  */
                       what= 'terminating'       /*assume this kind of classification.  */
          c.=0;   c.s=1                          /*clear all cyclic sequences;  set 1st.*/
          if $==a then what= 'perfect'           /*check for a  "perfect"  number.      */
                  else do t=1  while  s\==0      /*loop until sum isn't  0   or   > big.*/
                       m=s                       /*obtain the last number in sequence.  */
                       if #.m==. then s=sigma(m) /*Not defined? Then sum proper divisors*/
                                 else s=#.m      /*use the previously found integer.    */
                       if m==s & m\==0  then do;   what= 'aspiring'       ;   leave;   end
                       parse var $ . word2 .     /*obtain the 2nd  number in sequence.  */
                       if word2==a      then do;   what= 'amicable'       ;   leave;   end
                       $=$  s                    /*append a sum to the integer sequence.*/
                       if s==a & t>3    then do;   what= 'sociable'       ;   leave;   end
                       if c.s  & m\==0  then do;   what= 'cyclic'         ;   leave;   end
                       c.s=1                     /*assign another possible cyclic number*/
                                                 /* [↓]  Rosetta Code task's limit: >16 */
                       if t>NTlimit     then do;   what= 'non-terminating';   leave;   end
                       if s>big         then do;   what= 'NON-TERMINATING';   leave;   end
                       end   /*t*/               /* [↑]  only permit within reason.     */
          if aa>0  then say right(a, digits() )       'is'        center(what, 15)     $
          return                                 /* [↑] only display if  AA  is positive*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
sigma: procedure expose #. !.;  parse arg x;   if x<2  then return 0;            odd=x//2
       s=1                                       /* [↓]  use EVEN or ODD integers.   ___*/
            do j=2+odd  by 1+odd  while j*j<x    /*divide by all the integers up to √ X */
            if x//j==0  then  s=s + j + x%j      /*add the two divisors to the sum.     */
            end   /*j*/                          /* [↓]  adjust for square.          ___*/
       if j*j==x  then  s=s + j                  /*Was  X  a square?    If so, add  √ X */
       #.x=s                                     /*define division sum  for argument  X.*/
       return s                                  /*return     "     "    "      "     " */
```

{{out|output|text=  when using the default input:}}

```txt

════════════════════════════numbers from  1  to  10════════════════════════════
              1 is   terminating   0 
              2 is   terminating   1 0
              3 is   terminating   1 0
              4 is   terminating   3 1 0
              5 is   terminating   1 0
              6 is     perfect     6
              7 is   terminating   1 0
              8 is   terminating   7 1 0
              9 is   terminating   4 3 1 0
             10 is   terminating   8 7 1 0

═════════════════════first numbers for each classification═════════════════════
              1 is   terminating   0 
              6 is     perfect     6
             25 is    aspiring     6
            138 is non-terminating 150 222 234 312 528 960 2088 3762 5598 6570 10746 13254 13830 19434 20886 21606 25098 26742 26754
            220 is    amicable     284 220
            562 is     cyclic      284 220 284
          12496 is    sociable     14288 15472 14536 14264 12496

═════════════════════classifications for specific numbers══════════════════════
             11 is   terminating   1 0
             12 is   terminating   16 15 9 4 3 1 0
             28 is     perfect     28
            496 is     perfect     496
            220 is    amicable     284 220
           1184 is    amicable     1210 1184
          12496 is    sociable     14288 15472 14536 14264 12496
        1264460 is     cyclic      1547860 1727636 1305184 1264460 1547860
            790 is    aspiring     650 652 496
            909 is    aspiring     417 143 25 6
            562 is     cyclic      284 220 284
           1064 is     cyclic      1336 1184 1210 1184
           1488 is non-terminating 2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384 1474608 2461648 3172912 3173904
 15355717786080 is NON-TERMINATING 44534663601120 144940087464480

```



## Ring


```ring

# Project : Aliquot sequence classnifications

see "Rosetta Code - aliquot sequence classnifications" + nl
while true
        see "enter an integer: "
        give k
        k=fabs(floor(number(k)))
        if k=0
           exit
        ok            
        printas(k)
end
see "program complete."
 
func printas(k)
       length=52
       aseq = list(length)
       n=k
       classn=0
       priorn = 0
       inc = 0
       for element=2 to length
            aseq[element]=pdtotal(n)
            see aseq[element] + " " + nl
            if aseq[element]=0
               see " terminating" + nl
               classn=1
               exit
            ok
            if aseq[element]=k and element=2
               see " perfect" + nl
               classn=2
               exit
            ok
            if aseq[element]=k and element=3
               see " amicable" + nl
               classn=3
               exit 
            ok
            if aseq[element]=k and element>3
               see " sociable" + nl
               classn=4
               exit
            ok
            if aseq[element]!=k and aseq[element-1]=aseq[element]
               see " aspiring" + nl
               classn=5
               exit
            ok
            if aseq[element]!=k and element>2 and aseq[element-2]= aseq[element] 
               see " cyclic" + nl
               classn=6
              exit 
            ok
            n=aseq[element]
            if n>priorn 
               priorn=n
               inc=inc+1
            but n<=priorn
                  inc=0
                  priorn=0
            ok
            if inc=11 or n>30000000
               exit
            ok
       next
       if classn=0
          see " non-terminating" + nl
       ok
 
func pdtotal(n)
       pdtotal = 0
       for y=2 to n
           if (n % y)=0
               pdtotal=pdtotal+(n/y)
           ok
       next
       return pdtotal

```

Output:

```txt

ROSETTA CODE - Aliquot sequence classifications
Enter an integer: 1
0  terminating
Enter an integer: 2
1 0  terminating
Enter an integer: 3
1 0  terminating
Enter an integer: 4
3 1 0  terminating
Enter an integer: 5
1 0  terminating
Enter an integer: 6
6  perfect
Enter an integer: 7
1 0  terminating
Enter an integer: 8
7 1 0  terminating
Enter an integer: 9
4 3 1 0  terminating
Enter an integer: 10
8 7 1 0  terminating
Enter an integer: 11
1 0  terminating
Enter an integer: 12
16 15 9 4 3 1 0  terminating
Enter an integer: 28
28  perfect
Enter an integer: 496
496  perfect
Enter an integer: 220
284 220  amicable
Enter an integer: 1184
1210 1184  amicable
Enter an integer: 12496
14288 15472 14536 14264 12496  sociable
Enter an integer: 1264460
1547860 1727636 1305184 1264460  sociable
Enter an integer: 790
650 652 496 496  aspiring
Enter an integer: 909
417 143 25 6 6  aspiring
Enter an integer: 562
284 220 284  cyclic
Enter an integer: 1064
1336 1184 1210 1184  cyclic
Enter an integer: 1488
2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720  non-terminating
- - - - - - - - - - - -
Enter an integer: 4004
5404 5460 13356 25956 49756 49812 83244 138964 144326 127978 67322 36250 34040 48040 60140 71572 58208 64264 60836 47692 35776 42456 69144 110376 244824 373356 594884 446170 356954 219706 118874 88720 117740 174916 174972 291844 302666 2564
38 217322 185014 92510 95626 49274 25894 17198 8602 6950 6070 4874 2440 3140  non-terminating
Enter an integer: 4344
6576 10536 15864 23856 47568 75440 112048 111152 104236 105428 79078 45842 22924 20924 15700 18586 9296 11536 14256 30756 47868 63852 94404 125900 147520 204524 153400 237200 333634 238334 121306 62438 31222 16514 9406 4706 2938 1850 1684 1
270 1034 694 350 394 200 265 59 1 0  terminating
Enter an integer: 6672
10688 10648 11312 13984 16256 16384 16383 6145 1235 445 95 25 6 6  aspiring
Enter an integer: 6420
11724 15660 34740 71184 112832 121864 106646 53326 45458 37486 18746 16198 14042 11878 5942 2974 1490 1210 1184 1210  cyclic
Enter an integer: 8128
8128  perfect
Enter an integer:
Program complete.

```



## Ruby

With [[proper_divisors#Ruby]] in place:
{{trans|Python}}


```ruby
def aliquot(n, maxlen=16, maxterm=2**47)
  return "terminating", [0] if n == 0
  s = []
  while (s << n).size <= maxlen and n < maxterm
    n = n.proper_divisors.inject(0, :+)
    if s.include?(n)
      case n
      when s[0]
        case s.size
        when 1   then   return "perfect", s
        when 2   then   return "amicable", s
        else            return "sociable of length #{s.size}", s
        end
      when s[-1] then   return "aspiring", s
      else              return "cyclic back to #{n}", s
      end
    elsif n == 0 then   return "terminating", s << 0
    end
  end
  return "non-terminating", s
end

for n in 1..10
  puts "%20s: %p" % aliquot(n)
end
puts
for n in [11, 12, 28, 496, 220, 1184,  12496, 1264460, 790, 909, 562, 1064, 1488, 15355717786080]
  puts "%20s: %p" % aliquot(n)
end
```


{{out}}

```txt

         terminating: [1, 0]
         terminating: [2, 1, 0]
         terminating: [3, 1, 0]
         terminating: [4, 3, 1, 0]
         terminating: [5, 1, 0]
             perfect: [6]
         terminating: [7, 1, 0]
         terminating: [8, 7, 1, 0]
         terminating: [9, 4, 3, 1, 0]
         terminating: [10, 8, 7, 1, 0]

         terminating: [11, 1, 0]
         terminating: [12, 16, 15, 9, 4, 3, 1, 0]
             perfect: [28]
             perfect: [496]
            amicable: [220, 284]
            amicable: [1184, 1210]
sociable of length 5: [12496, 14288, 15472, 14536, 14264]
sociable of length 4: [1264460, 1547860, 1727636, 1305184]
            aspiring: [790, 650, 652, 496]
            aspiring: [909, 417, 143, 25, 6]
  cyclic back to 284: [562, 284, 220]
 cyclic back to 1184: [1064, 1336, 1184, 1210]
     non-terminating: [1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384, 1474608]
     non-terminating: [15355717786080, 44534663601120, 144940087464480]

```



## Rust


```rust
#[derive(Debug)]
enum AliquotType { Terminating, Perfect, Amicable, Sociable, Aspiring, Cyclic, NonTerminating }

fn classify_aliquot(num: i64) -> (AliquotType, Vec<i64>) {
    let limit = 1i64 << 47; //140737488355328
    let mut terms = Some(num).into_iter().collect::<Vec<_>>();
    for i in 0..16 {
        let n = terms[i];
        let divsum = (1..(n + 1) / 2 + 1).filter(|&x| n % x == 0 && n != x).fold(0, |sum, x| sum + x);
        let classification = if divsum == 0 {
            Some(AliquotType::Terminating)
        }
        else if divsum > limit {
            Some(AliquotType::NonTerminating)
        }
        else if let Some(prev_idx) = terms.iter().position(|&x| x == divsum) {
            let cycle_len = terms.len() - prev_idx;
            Some(if prev_idx == 0 {
                match cycle_len {
                    1 => AliquotType::Perfect,
                    2 => AliquotType::Amicable,
                    _ => AliquotType::Sociable
                }
            }
            else {
                if cycle_len == 1 {AliquotType::Aspiring} else {AliquotType::Cyclic}
            })
        }
        else {
            None
        };
        terms.push(divsum);
        if let Some(result) = classification {
            return (result, terms);
        }
    }
    (AliquotType::NonTerminating, terms)
}

fn main() {
    let nums = [1i64, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488/*, 15355717786080*/];
    for num in &nums {
        println!("{} {:?}", num, classify_aliquot(*num));
    }
}
```

{{out}}

```txt

1 (Terminating, [1, 0])
2 (Terminating, [2, 1, 0])
3 (Terminating, [3, 1, 0])
4 (Terminating, [4, 3, 1, 0])
5 (Terminating, [5, 1, 0])
6 (Perfect, [6, 6])
7 (Terminating, [7, 1, 0])
8 (Terminating, [8, 7, 1, 0])
9 (Terminating, [9, 4, 3, 1, 0])
10 (Terminating, [10, 8, 7, 1, 0])
11 (Terminating, [11, 1, 0])
12 (Terminating, [12, 16, 15, 9, 4, 3, 1, 0])
28 (Perfect, [28, 28])
496 (Perfect, [496, 496])
220 (Amicable, [220, 284, 220])
1184 (Amicable, [1184, 1210, 1184])
12496 (Sociable, [12496, 14288, 15472, 14536, 14264, 12496])
1264460 (Sociable, [1264460, 1547860, 1727636, 1305184, 1264460])
790 (Aspiring, [790, 650, 652, 496, 496])
909 (Aspiring, [909, 417, 143, 25, 6, 6])
562 (Cyclic, [562, 284, 220, 284])
1064 (Cyclic, [1064, 1336, 1184, 1210, 1184])
1488 (NonTerminating, [1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384, 1474608])

```



## Scala

Put [[proper_divisors#Scala]] the full /Proper divisors for big (long) numbers/ section to the beginning:

```Scala
def createAliquotSeq(n: Long, step: Int, list: List[Long]): (String, List[Long]) = {
    val sum = properDivisors(n).sum
    if (sum == 0) ("terminate", list ::: List(sum))
    else if (step >= 16 || sum > 140737488355328L) ("non-term", list)
    else {
        list.indexOf(sum) match {
            case -1 => createAliquotSeq(sum, step + 1, list ::: List(sum))
            case 0 => if (step == 0) ("perfect", list ::: List(sum))
                else if (step == 1) ("amicable", list ::: List(sum))
                else ("sociable-" + (step + 1), list ::: List(sum))
            case index => if (step == index) ("aspiring", list ::: List(sum))
                else ("cyclic-" + (step - index + 1), list ::: List(sum))
        }
    }
}
val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 28, 496, 220, 1184,
    12496, 1264460, 790, 909, 562, 1064, 1488, 15355717786080L)
val result = numbers.map(i => createAliquotSeq(i, 0, List(i)))

result foreach { v => println(f"${v._2.head}%14d ${v._1}%10s [${v._2 mkString " "}]" ) }
```


{{out}}

```txt
             1  terminate [1 0]
             2  terminate [2 1 0]
             3  terminate [3 1 0]
             4  terminate [4 3 1 0]
             5  terminate [5 1 0]
             6    perfect [6 6]
             7  terminate [7 1 0]
             8  terminate [8 7 1 0]
             9  terminate [9 4 3 1 0]
            10  terminate [10 8 7 1 0]
            11  terminate [11 1 0]
            12  terminate [12 16 15 9 4 3 1 0]
            28    perfect [28 28]
           496    perfect [496 496]
           220   amicable [220 284 220]
          1184   amicable [1184 1210 1184]
         12496 sociable-5 [12496 14288 15472 14536 14264 12496]
       1264460 sociable-4 [1264460 1547860 1727636 1305184 1264460]
           790   aspiring [790 650 652 496 496]
           909   aspiring [909 417 143 25 6 6]
           562   cyclic-2 [562 284 220 284]
          1064   cyclic-2 [1064 1336 1184 1210 1184]
          1488   non-term [1488 2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384 1474608]
15355717786080   non-term [15355717786080 44534663601120]
```



## Tcl


This solution creates an iterator from a coroutine to generate aliquot sequences.  al_classify uses a "RESULT" exception to achieve some unusual control flow.


```Tcl
proc ProperDivisors {n} {
    if {$n == 1} {return 0}
    set divs 1
    set sum 1
    for {set i 2} {$i*$i <= $n} {incr i} {
        if {! ($n % $i)} {
            lappend divs $i
            incr sum $i
            if {$i*$i<$n} {
                lappend divs [set d [expr {$n / $i}]]
                incr sum $d
            }
        }
    }
    list $sum $divs
}

proc al_iter {n} {
    yield [info coroutine]
    while {$n} {
        yield $n
        lassign [ProperDivisors $n] n
    }
    yield 0
    return -code break
}

proc al_classify {n} {
    coroutine iter al_iter $n
    set items {}
    try {
        set type "non-terminating"
        while {[llength $items] < 16} {
            set i [iter]
            if {$i == 0} {
                set type "terminating"
            }
            set ix [lsearch -exact $items $i]
            set items [linsert $items 0 $i]
            switch $ix {
                -1 { continue }
                0 { throw RESULT "perfect" }
                1 { throw RESULT "amicable" }
                default { throw RESULT "sociable" }
            }
        }
    } trap {RESULT} {type} {
        rename iter {}
        set map {
            perfect aspiring
            amicable cyclic
            sociable cyclic
        }
        if {$ix != [llength $items]-2} {
            set type [dict get $map $type]
        }
    }
    list $type [lreverse $items]
}

for {set i 1} {$i <= 10} {incr i} {
    puts [format "%8d -> %-16s : %s" $i {*}[al_classify $i]]
}

foreach i {11 12 28 496 220 1184 12496 1264460 790 909 562 1064 1488 } {
    puts [format "%8d -> %-16s : %s" $i {*}[al_classify $i]]
}

;# stretch goal .. let's time it:
set i 15355717786080
puts [time {
    puts [format "%8d -> %-16s : %s" $i {*}[al_classify $i]]
}]
```


{{out}}

```txt

       1 -> terminating      : 1 0
       2 -> terminating      : 2 1 0
       3 -> terminating      : 3 1 0
       4 -> terminating      : 4 3 1 0
       5 -> terminating      : 5 1 0
       6 -> perfect          : 6 6
       7 -> terminating      : 7 1 0
       8 -> terminating      : 8 7 1 0
       9 -> terminating      : 9 4 3 1 0
      10 -> terminating      : 10 8 7 1 0
      11 -> terminating      : 11 1 0
      12 -> terminating      : 12 16 15 9 4 3 1 0
      28 -> perfect          : 28 28
     496 -> perfect          : 496 496
     220 -> amicable         : 220 284 220
    1184 -> amicable         : 1184 1210 1184
   12496 -> sociable         : 12496 14288 15472 14536 14264 12496
 1264460 -> sociable         : 1264460 1547860 1727636 1305184 1264460
     790 -> aspiring         : 790 650 652 496 496
     909 -> aspiring         : 909 417 143 25 6 6
     562 -> cyclic           : 562 284 220 284
    1064 -> cyclic           : 1064 1336 1184 1210 1184
    1488 -> non-terminating  : 1488 2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384

15355717786080 -> non-terminating  : 15355717786080 44534663601120 144940087464480 471714103310688 1130798979186912 2688948041357088 6050151708497568 13613157922639968 35513546724070632 74727605255142168 162658586225561832 353930992506879768 642678347124409032 1125102611548462968 1977286128289819992 3415126495450394808
556214046 microseconds per iteration

```

The large number finished (notice native bignums), but it took over 500 seconds ...


## VBA


```vb
Option Explicit

Private Type Aliquot
   Sequence() As Double
   Classification As String
End Type

Sub Main()
Dim result As Aliquot, i As Long, j As Double, temp As String
'display the classification and sequences of the numbers one to ten inclusive
   For j = 1 To 10
      result = Aliq(j)
      temp = vbNullString
      For i = 0 To UBound(result.Sequence)
         temp = temp & result.Sequence(i) & ", "
      Next i
      Debug.Print "Aliquot seq of " & j & " : " & result.Classification & "   " & Left(temp, Len(temp) - 2)
   Next j
'show the classification and sequences of the following integers, in order:
Dim a
   '15 355 717 786 080 : impossible in VBA ==> out of memory
   a = Array(11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488)
   For j = LBound(a) To UBound(a)
      result = Aliq(CDbl(a(j)))
      temp = vbNullString
      For i = 0 To UBound(result.Sequence)
         temp = temp & result.Sequence(i) & ", "
      Next i
      Debug.Print "Aliquot seq of " & a(j) & " : " & result.Classification & "   " & Left(temp, Len(temp) - 2)
   Next
End Sub

Private Function Aliq(Nb As Double) As Aliquot
Dim s() As Double, i As Long, temp, j As Long, cpt As Long
   temp = Array("non-terminating", "Terminate", "Perfect", "Amicable", "Sociable", "Aspiring", "Cyclic")
   ReDim s(0)
   s(0) = Nb
   For i = 1 To 15
      cpt = cpt + 1
      ReDim Preserve s(cpt)
      s(i) = SumPDiv(s(i - 1))
      If s(i) > 140737488355328# Then Exit For
      If s(i) = 0 Then j = 1
      If s(1) = s(0) Then j = 2
      If s(i) = s(0) And i > 1 And i <> 2 Then j = 4
      If s(i) = s(i - 1) And i > 1 Then j = 5
      If i >= 2 Then
         If s(2) = s(0) Then j = 3
         If s(i) = s(i - 2) And i <> 2 Then j = 6
      End If
      If j > 0 Then Exit For
   Next
   Aliq.Classification = temp(j)
   Aliq.Sequence = s
End Function

Private Function SumPDiv(n As Double) As Double
'returns the sum of the Proper divisors of n
Dim j As Long, t As Long
    If n > 1 Then
        For j = 1 To n \ 2
            If n Mod j = 0 Then t = t + j
        Next
    End If
    SumPDiv = t
End Function

```


{{out}}

```txt
Aliquot seq of 1 : Terminate   1, 0
Aliquot seq of 2 : Terminate   2, 1, 0
Aliquot seq of 3 : Terminate   3, 1, 0
Aliquot seq of 4 : Terminate   4, 3, 1, 0
Aliquot seq of 5 : Terminate   5, 1, 0
Aliquot seq of 6 : Perfect   6, 6
Aliquot seq of 7 : Terminate   7, 1, 0
Aliquot seq of 8 : Terminate   8, 7, 1, 0
Aliquot seq of 9 : Terminate   9, 4, 3, 1, 0
Aliquot seq of 10 : Terminate   10, 8, 7, 1, 0
Aliquot seq of 11 : Terminate   11, 1, 0
Aliquot seq of 12 : Terminate   12, 16, 15, 9, 4, 3, 1, 0
Aliquot seq of 28 : Perfect   28, 28
Aliquot seq of 496 : Perfect   496, 496
Aliquot seq of 220 : Amicable   220, 284, 220
Aliquot seq of 1184 : Amicable   1184, 1210, 1184
Aliquot seq of 12496 : Sociable   12496, 14288, 15472, 14536, 14264, 12496
Aliquot seq of 1264460 : Sociable   1264460, 1547860, 1727636, 1305184, 1264460
Aliquot seq of 790 : Aspiring   790, 650, 652, 496, 496
Aliquot seq of 909 : Aspiring   909, 417, 143, 25, 6, 6
Aliquot seq of 562 : Cyclic   562, 284, 220, 284
Aliquot seq of 1064 : Cyclic   1064, 1336, 1184, 1210, 1184
Aliquot seq of 1488 : non-terminating   1488, 2480, 3472, 4464, 8432, 9424, 10416, 21328, 22320, 55056, 95728, 96720, 236592, 459792, 881392, 882384

```



## zkl


```zkl
fcn properDivs(n){ [1.. (n + 1)/2 + 1].filter('wrap(x){ n%x==0 and n!=x }) }
fcn aliquot(k){  //-->Walker
   Walker(fcn(rk){ k:=rk.value; if(k)rk.set(properDivs(k).sum()); k }.fp(Ref(k)))
}(10).walk(15).println();
```

Or, refactoring to remove saving the intermediate divisors (and adding white space):

```zkl
fcn aliquot(k){  //-->Walker
   Walker(fcn(rk){
      k:=rk.value;
      rk.set((1).reduce((k + 1)/2, fcn(s,n,k){
	 s + (k%n==0 and k!=n and n) // s + False == s + 0
      },0,k));
      k
   }.fp(Ref(k)))
}(10).walk(15).println();
```


```zkl
fcn classify(k){
   const MAX=(2).pow(47);  // 140737488355328
   ak,aks:=aliquot(k), ak.walk(16);
   _,a2,a3:=aks;
   if(a2==k) return("perfect");
   if(a3==k) return("amicable");
   aspiring:='wrap(){
      foreach n in (aks.len()-1){ if(aks[n]==aks[n+1]) return(True) }
      False
   };
   cyclic:='wrap(){
      foreach n in (aks.len()-1){ if(aks[n+1,*].holds(aks[n])) return(aks[n]) }
      False
   };
   (if(aks.filter1('==(0))!=False) "terminating"
    else if(n:=aks[1,*].filter1n('==(k))) "sociable of length " + (n+1)
    else if(aks.filter1('>(MAX)))  "non-terminating"
    else if(aspiring())            "aspiring"
    else if((c:=cyclic())!=False)  "cyclic on " + c
    else                           "non-terminating" )
   + " " + aks.filter();
}
```


```zkl
[1..10].pump(fcn(k){ "%6d is %s".fmt(k,classify(k)).println() });
T(11,12,28,496,220,1184,12496,1264460,790,909,562,1064,1488)
   .pump(fcn(k){ "%6d is %s".fmt(k,classify(k)).println() });
```

{{out}}

```txt

L(10,8,7,1,0,0,0,0,0,0,0,0,0,0,0)
     1 is terminating L(1)
     2 is terminating L(2,1)
     3 is terminating L(3,1)
     4 is terminating L(4,3,1)
     5 is terminating L(5,1)
     6 is perfect
     7 is terminating L(7,1)
     8 is terminating L(8,7,1)
     9 is terminating L(9,4,3,1)
    10 is terminating L(10,8,7,1)
    11 is terminating L(11,1)
    12 is terminating L(12,16,15,9,4,3,1)
    28 is perfect
   496 is perfect
   220 is amicable
  1184 is amicable
 12496 is sociable of length 5 L(12496,14288,15472,14536,14264,12496,14288,15472,14536,14264,12496,14288,15472,14536,14264,12496)
1264460 is sociable of length 4 L(1264460,1547860,1727636,1305184,1264460,1547860,1727636,1305184,1264460,1547860,1727636,1305184,1264460,1547860,1727636,1305184)
   790 is aspiring L(790,650,652,496,496,496,496,496,496,496,496,496,496,496,496,496)
   909 is aspiring L(909,417,143,25,6,6,6,6,6,6,6,6,6,6,6,6)
   562 is cyclic on 284 L(562,284,220,284,220,284,220,284,220,284,220,284,220,284,220,284)
  1064 is cyclic on 1184 L(1064,1336,1184,1210,1184,1210,1184,1210,1184,1210,1184,1210,1184,1210,1184,1210)
  1488 is non-terminating L(1488,2480,3472,4464,8432,9424,10416,21328,22320,55056,95728,96720,236592,459792,881392,882384)

```

The loop to calculate 15355717786080 takes forever (literally)


## ZX Spectrum Basic

{{trans|AWK}}
This program is correct. However, a bug in the ROM of the ZX Spectrum makes the number 909 of an erroneous result. However, the same program running on Sam BASIC (a superset of Sinclair BASIC that ran on the computer Sam Coupé) provides the correct results.

```zxbasic
10 PRINT "Number classification sequence"
20 INPUT "Enter a number (0 to end): ";k: IF k>0 THEN GO SUB 2000: PRINT k;" ";s$: GO TO 20
40 STOP 
1000 REM sumprop
1010 IF oldk=1 THEN LET newk=0: RETURN 
1020 LET sum=1
1030 LET root=SQR oldk
1040 FOR i=2 TO root-0.1
1050 IF oldk/i=INT (oldk/i) THEN LET sum=sum+i+oldk/i
1060 NEXT i
1070 IF oldk/root=INT (oldk/root) THEN LET sum=sum+root
1080 LET newk=sum
1090 RETURN 
2000 REM class
2010 LET oldk=k: LET s$=" "
2020 GO SUB 1000
2030 LET oldk=newk
2040 LET s$=s$+" "+STR$ newk
2050 IF newk=0 THEN LET s$="terminating"+s$: RETURN 
2060 IF newk=k THEN LET s$="perfect"+s$: RETURN 
2070 GO SUB 1000
2080 LET oldk=newk
2090 LET s$=s$+" "+STR$ newk
2100 IF newk=0 THEN LET s$="terminating"+s$: RETURN 
2110 IF newk=k THEN LET s$="amicable"+s$: RETURN 
2120 FOR t=4 TO 16
2130 GO SUB 1000
2140 LET s$=s$+" "+STR$ newk
2150 IF newk=0 THEN LET s$="terminating"+s$: RETURN 
2160 IF newk=k THEN LET s$="sociable (period "+STR$ (t-1)+")"+s$: RETURN 
2170 IF newk=oldk THEN LET s$="aspiring"+s$: RETURN 
2180 LET b$=" "+STR$ newk+" ": LET ls=LEN s$: LET lb=LEN b$: LET ls=ls-lb
2190 FOR i=1 TO ls
2200 IF s$(i TO i+lb-1)=b$ THEN LET s$="cyclic (at "+STR$ newk+") "+s$: LET i=ls
2210 NEXT i
2220 IF LEN s$<>(ls+lb) THEN RETURN 
2300 IF newk>140737488355328 THEN LET s$="non-terminating (term > 140737488355328)"+s$: RETURN 
2310 LET oldk=newk
2320 NEXT t
2330 LET s$="non-terminating (after 16 terms)"+s$
2340 RETURN
```

{{out}}

```txt
Number classification sequence
1 terminating   0
2 terminating   1 0
3 terminating   1 0
4 terminating   3 1 0
5 terminating   1 0
6 perfect   6
7 terminating   1 0
8 terminating   7 1 0
9 terminating   4 3 1 0
10 terminating   8 7 1 0
11 terminating   1 0
12 terminating   16 15 9 4 3 1 0
28 perfect   28
496 perfect   496
220 amicable   284 220
1184 amicable   1210 1184
12496 sociable (period 5)   14288 15472 14536 14264 12496
1264460 sociable (period 4)   1547860 1727636 1305184 1264460
790 aspiring   650 652 496 496
909 aspiring   417 143 25 6 6
562 cyclic (at 284)   284 220 284
1064 cyclic (at 1184)   1336 1184 1210 1184
1488 non-terminating (after 16 terms)    2480 3472 4464 8432 9424 10416 21328 22320 55056 95728 96720 236592 459792 881392 882384
```

