+++
title = "Binary search"
description = ""
date = 2019-07-23T22:33:52Z
aliases = []
[extra]
id = 2206
[taxonomies]
categories = ["Classic CS problems and programs", "Recursion", "task"]
tags = []
languages = [
  "11l",
  "360_assembly",
  "acl2",
  "ada",
  "algol_68",
  "arm_assembly",
  "autohotkey",
  "awk",
  "axe",
  "basic",
  "batch_file",
  "bbc_basic",
  "brat",
  "c",
  "chapel",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "e",
  "easylang",
  "eiffel",
  "elixir",
  "emacs_lisp",
  "erlang",
  "euphoria",
  "factor",
  "fbsl",
  "forth",
  "fortran",
  "futhark",
  "gap",
  "go",
  "groovy",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "k",
  "kotlin",
  "lambdatalk",
  "liberty_basic",
  "logo",
  "lolcode",
  "lua",
  "m2000_interpreter",
  "m4",
  "maple",
  "matlab",
  "maxima",
  "maxscript",
  "miniscript",
  "n_t_roff",
  "nim",
  "niue",
  "objeck",
  "ocaml",
  "octave",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "pop11",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sequencel",
  "sidef",
  "simula",
  "spark",
  "standard_ml",
  "swift",
  "tcl",
  "ubasic_4th",
  "unix_shell",
  "unixpipes",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "visual_basic_.net",
  "wortel",
  "yabasic",
  "z_arch_assembler",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

A binary search divides a range of values into halves, and continues to narrow down the field of search until the unknown value is found. It is the classic example of a "divide and conquer" algorithm.

As an analogy, consider the children's game "[[Guess the number/With feedback|guess a number]]."  The scorer has a secret number, and will only tell the player if their guessed number is higher than, lower than, or equal to the secret number. The player then uses this information to guess a new number.

As the player, an optimal strategy for the general case is to start by choosing the range's midpoint as the guess, and then asking whether the guess was higher, lower, or equal to the secret number. If the guess was too high, one would select the point exactly between the range midpoint and the beginning of the range. If the original guess was too low, one would ask about the point exactly between the range midpoint and the end of the range. This process repeats until one has reached the secret number.


;Task:
Given the starting point of a range, the ending point of a range, and the "secret value", implement a binary search through a sorted integer array for a certain number. Implementations can be recursive or iterative (both if you can). Print out whether or not the number was in the array afterwards. If it was, print the index also.

There are several binary search algorithms commonly seen. They differ by how they treat multiple values equal to the given value, and whether they indicate whether the element was found or not. For completeness we will present pseudocode for all of them.

All of the following code examples use an "inclusive" upper bound (i.e. <code>high = N-1</code> initially). Any of the examples can be converted into an equivalent example using "exclusive" upper bound (i.e. <code>high = N</code> initially) by making the following simple changes (which simply increase <code>high</code> by 1):
* change <code>high = N-1</code> to <code>high = N</code>
* change <code>high = mid-1</code> to <code>high = mid</code>
* (for recursive algorithm) change <code>if (high < low)</code> to <code>if (high <= low)</code>
* (for iterative algorithm) change <code>while (low <= high)</code> to <code>while (low < high)</code>

;Traditional algorithm
The algorithms are as follows (from [[wp:Binary search|Wikipedia]]). The algorithms return the index of some element that equals the given value (if there are multiple such elements, it returns some arbitrary one). It is also possible, when the element is not found, to return the "insertion point" for it (the index that the value would have if it were inserted into the array).

'''Recursive Pseudocode''':
   // initially called with low = 0, high = N-1
   BinarySearch(A[0..N-1], value, low, high) {
       // invariants: value > A[i] for all i < low
                      value < A[i] for all i > high
       if (high < low)
           return not_found // value would be inserted at index "low"
       mid = (low + high) / 2
       if (A[mid] > value)
           return BinarySearch(A, value, low, mid-1)
       else if (A[mid] < value)
           return BinarySearch(A, value, mid+1, high)
       else
           return mid
   }

'''Iterative Pseudocode''':
   BinarySearch(A[0..N-1], value) {
       low = 0
       high = N - 1
       while (low <= high) {
           // invariants: value > A[i] for all i < low
                          value < A[i] for all i > high
           mid = (low + high) / 2
           if (A[mid] > value)
               high = mid - 1
           else if (A[mid] < value)
               low = mid + 1
           else
               return mid
       }
       return not_found // value would be inserted at index "low"
   }

;Leftmost insertion point
The following algorithms return the leftmost place where the given element can be correctly inserted (and still maintain the sorted order). This is the lower (inclusive) bound of the range of elements that are equal to the given value (if any). Equivalently, this is the lowest index where the element is greater than or equal to the given value (since if it were any lower, it would violate the ordering), or 1 past the last index if such an element does not exist. This algorithm does not determine if the element is actually found. This algorithm only requires one comparison per level.

'''Recursive Pseudocode''':
   // initially called with low = 0, high = N - 1
   BinarySearch_Left(A[0..N-1], value, low, high) {
       // invariants: value > A[i] for all i < low
                      value <= A[i] for all i > high
       if (high < low)
           return low
       mid = (low + high) / 2
       if (A[mid] >= value)
           return BinarySearch_Left(A, value, low, mid-1)
       else
           return BinarySearch_Left(A, value, mid+1, high)
   }

'''Iterative Pseudocode''':
   BinarySearch_Left(A[0..N-1], value) {
       low = 0
       high = N - 1
       while (low <= high) {
           // invariants: value > A[i] for all i < low
                          value <= A[i] for all i > high
           mid = (low + high) / 2
           if (A[mid] >= value)
               high = mid - 1
           else
               low = mid + 1
       }
       return low
   }

;Rightmost insertion point
The following algorithms return the rightmost place where the given element can be correctly inserted (and still maintain the sorted order). This is the upper (exclusive) bound of the range of elements that are equal to the given value (if any). Equivalently, this is the lowest index where the element is greater than the given value, or 1 past the last index if such an element does not exist. This algorithm does not determine if the element is actually found. This algorithm only requires one comparison per level. Note that these algorithms are almost exactly the same as the leftmost-insertion-point algorithms, except for how the inequality treats equal values.

'''Recursive Pseudocode''':
   // initially called with low = 0, high = N - 1
   BinarySearch_Right(A[0..N-1], value, low, high) {
       // invariants: value >= A[i] for all i < low
                      value < A[i] for all i > high
       if (high < low)
           return low
       mid = (low + high) / 2
       if (A[mid] > value)
           return BinarySearch_Right(A, value, low, mid-1)
       else
           return BinarySearch_Right(A, value, mid+1, high)
   }

'''Iterative Pseudocode''':
   BinarySearch_Right(A[0..N-1], value) {
       low = 0
       high = N - 1
       while (low <= high) {
           // invariants: value >= A[i] for all i < low
                          value < A[i] for all i > high
           mid = (low + high) / 2
           if (A[mid] > value)
               high = mid - 1
           else
               low = mid + 1
       }
       return low
   }

;Extra credit
Make sure it does not have overflow bugs.

The line in the pseudo-code above to calculate the mean of two integers:

```txt
mid = (low + high) / 2
```

could produce the wrong result in some programming languages when used with a bounded integer type, if the addition causes an overflow. (This can occur if the array size is greater than half the maximum integer value.) If signed integers are used, and <code>low + high</code> overflows, it becomes a negative number, and dividing by 2 will still result in a negative number. Indexing an array with a negative number could produce an out-of-bounds exception, or other undefined behavior. If unsigned integers are used, an overflow will result in losing the largest bit, which will produce the wrong result.

One way to fix it is to manually add half the range to the low number:

```txt
mid = low + (high - low) / 2
```

Even though this is mathematically equivalent to the above, it is not susceptible to overflow.

Another way for signed integers, possibly faster, is the following:

```txt
mid = (low + high) >>> 1
```

where <code> >>> </code> is the logical right shift operator. The reason why this works is that, for signed integers, even though it overflows, when viewed as an unsigned number, the value is still the correct sum. To divide an unsigned number by 2, simply do a logical right shift.


;Related task:
:* [[Guess the number/With Feedback (Player)]]


;See also:
:* [[wp:Binary search algorithm]]
:* [http://googleresearch.blogspot.com/2006/06/extra-extra-read-all-about-it-nearly.html Extra, Extra - Read All About It: Nearly All Binary Searches and Mergesorts are Broken].





## 11l


```11l
F binary_search(l, value)
   V low = 0
   V high = l.len - 1
   L low <= high
      V mid = (low + high) I/ 2
      I l[mid] > value
         high = mid - 1
      E I l[mid] < value
         low = mid + 1
      E
         R mid
   R -1
```



## 360 Assembly


```360asm
*        Binary search             05/03/2017
BINSEAR  CSECT
         USING  BINSEAR,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         MVC    LOW,=H'1'          low=1
         MVC    HIGH,=AL2((XVAL-T)/2)  high=hbound(t)
         SR     R6,R6              i=0
         MVI    F,X'00'            f=false
         LH     R4,LOW             low
       DO WHILE=(CH,R4,LE,HIGH)    do while low<=high
         LA     R6,1(R6)             i=i+1
         LH     R1,LOW               low
         AH     R1,HIGH              +high
         SRA    R1,1                 /2  {by right shift}
         STH    R1,MID               mid=(low+high)/2
         SLA    R1,1                 *2
         LH     R7,T-2(R1)           y=t(mid)
       IF CH,R7,EQ,XVAL THEN         if xval=y then
         MVI    F,X'01'                f=true
         B      EXITDO                 leave
       ENDIF    ,                    endif
       IF CH,R7,GT,XVAL THEN         if y>xval then
         LH     R2,MID                 mid
         BCTR   R2,0                   -1
         STH    R2,HIGH                high=mid-1
       ELSE     ,                    else
         LH     R2,MID                 mid
         LA     R2,1(R2)               +1
         STH    R2,LOW                low=mid+1
       ENDIF    ,                    endif
         LH     R4,LOW               low
       ENDDO    ,                  enddo
EXITDO   EQU    *                exitdo:
         XDECO  R6,XDEC            edit i
         MVC    PG(4),XDEC+8       output i
         MVC    PG+4(6),=C' loops'
         XPRNT  PG,L'PG            print buffer
         LH     R1,XVAL            xval
         XDECO  R1,XDEC            edit xval
         MVC    PG(4),XDEC+8       output xval
       IF CLI,F,EQ,X'01' THEN      if f then
         MVC    PG+4(10),=C' found at '
         LH     R1,MID               mid
         XDECO  R1,XDEC              edit mid
         MVC    PG+14(4),XDEC+8      output mid
       ELSE     ,                  else
         MVC    PG+4(20),=C' is not in the list.'
       ENDIF    ,                  endif
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
T        DC     H'3',H'7',H'13',H'19',H'23',H'31',H'43',H'47'
         DC     H'61',H'73',H'83',H'89',H'103',H'109',H'113',H'131'
         DC     H'139',H'151',H'167',H'181',H'193',H'199',H'229',H'233'
         DC     H'241',H'271',H'283',H'293',H'313',H'317',H'337',H'349'
XVAL     DC     H'229'             <= search value
LOW      DS     H
HIGH     DS     H
MID      DS     H
F        DS     X                  flag
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp
         YREGS
         END    BINSEAR
```

{{out}}

```txt

   5 loops
 229 found at   23

```



## ACL2



```Lisp
(defun defarray (name size initial-element)
   (cons name
         (compress1 name
                    (cons (list :HEADER
                                :DIMENSIONS (list size)
                                :MAXIMUM-LENGTH (1+ size)
                                :DEFAULT initial-element
                                :NAME name)
                                nil))))

(defconst *dim* 100000)

(defun array-name (array)
   (first array))

(defun set-at (array i val)
   (cons (array-name array)
         (aset1 (array-name array)
                (cdr array)
                i
                val)))

(defun populate-array-ordered (array n)
   (if (zp n)
       array
       (populate-array-ordered (set-at array
                                       (- *dim* n)
                                       (- *dim* n))
                               (1- n))))
(include-book "arithmetic-3/top" :dir :system)

(defun binary-search-r (needle haystack low high)
   (declare (xargs :measure (nfix (1+ (- high low)))))
   (let* ((mid (floor (+ low high) 2))
          (current (aref1 (array-name haystack)
                          (cdr haystack)
                          mid)))
         (cond ((not (and (natp low) (natp high))) nil)
               ((= current needle)
                mid)
               ((zp (1+ (- high low))) nil)
               ((> current needle)
                (binary-search-r needle
                                 haystack
                                 low
                                 (1- mid)))
               (t (binary-search-r needle
                                   haystack
                                   (1+ mid)
                                   high)))))

(defun binary-search (needle haystack)
   (binary-search-r needle haystack 0
                    (maximum-length (array-name haystack)
                                    (cdr haystack))))

(defun test-bsearch (needle)
   (binary-search needle
                  (populate-array-ordered
                   (defarray 'haystack *dim* 0)
                   *dim*)))
```



## Ada

Both solutions are generic. The element can be of any comparable type (such that the operation < is visible in the instantiation scope of the function Search). Note that the completion condition is different from one given in the pseudocode example above. The example assumes that the array index type does not overflow when mid is incremented or decremented beyond the corresponding array bound. This is a wrong assumption for Ada, where array bounds can start or end at the very first or last value of the index type. To deal with this, the exit condition is rather directly expressed as crossing the corresponding array bound by the coming interval middle.
;Recursive:

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Recursive_Binary_Search is
   Not_Found : exception;

   generic
      type Index is range <>;
      type Element is private;
      type Array_Of_Elements is array (Index range <>) of Element;
      with function "<" (L, R : Element) return Boolean is <>;
   function Search (Container : Array_Of_Elements; Value : Element) return Index;

   function Search (Container : Array_Of_Elements; Value : Element) return Index is
      Mid : Index;
   begin
      if Container'Length > 0 then
         Mid := (Container'First + Container'Last) / 2;
         if Value < Container (Mid) then
            if Container'First /= Mid then
               return Search (Container (Container'First..Mid - 1), Value);
            end if;
         elsif Container (Mid) < Value then
            if Container'Last /= Mid then
               return Search (Container (Mid + 1..Container'Last), Value);
            end if;
         else
            return Mid;
         end if;
      end if;
      raise Not_Found;
   end Search;

   type Integer_Array is array (Positive range <>) of Integer;
   function Find is new Search (Positive, Integer, Integer_Array);

   procedure Test (X : Integer_Array; E : Integer) is
   begin
      New_Line;
      for I in X'Range loop
         Put (Integer'Image (X (I)));
      end loop;
      Put (" contains" & Integer'Image (E) & " at" & Integer'Image (Find (X, E)));
   exception
      when Not_Found =>
         Put (" does not contain" & Integer'Image (E));
   end Test;
begin
   Test ((2, 4, 6, 8, 9), 2);
   Test ((2, 4, 6, 8, 9), 1);
   Test ((2, 4, 6, 8, 9), 8);
   Test ((2, 4, 6, 8, 9), 10);
   Test ((2, 4, 6, 8, 9), 9);
   Test ((2, 4, 6, 8, 9), 5);
end Test_Recursive_Binary_Search;
```

;Iterative:

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Binary_Search is
   Not_Found : exception;

   generic
      type Index is range <>;
      type Element is private;
      type Array_Of_Elements is array (Index range <>) of Element;
      with function "<" (L, R : Element) return Boolean is <>;
   function Search (Container : Array_Of_Elements; Value : Element) return Index;

   function Search (Container : Array_Of_Elements; Value : Element) return Index is
      Low  : Index := Container'First;
      High : Index := Container'Last;
      Mid  : Index;
   begin
      if Container'Length > 0 then
         loop
            Mid := (Low + High) / 2;
            if Value < Container (Mid) then
               exit when Low = Mid;
               High := Mid - 1;
            elsif Container (Mid) < Value then
               exit when High = Mid;
               Low := Mid + 1;
            else
               return Mid;
            end if;
         end loop;
      end if;
      raise Not_Found;
   end Search;

   type Integer_Array is array (Positive range <>) of Integer;
   function Find is new Search (Positive, Integer, Integer_Array);

   procedure Test (X : Integer_Array; E : Integer) is
   begin
      New_Line;
      for I in X'Range loop
         Put (Integer'Image (X (I)));
      end loop;
      Put (" contains" & Integer'Image (E) & " at" & Integer'Image (Find (X, E)));
   exception
      when Not_Found =>
         Put (" does not contain" & Integer'Image (E));
   end Test;
begin
   Test ((2, 4, 6, 8, 9), 2);
   Test ((2, 4, 6, 8, 9), 1);
   Test ((2, 4, 6, 8, 9), 8);
   Test ((2, 4, 6, 8, 9), 10);
   Test ((2, 4, 6, 8, 9), 9);
   Test ((2, 4, 6, 8, 9), 5);
end Test_Binary_Search;
```

Sample output:

```txt

 2 4 6 8 9 contains 2 at 1
 2 4 6 8 9 does not contain 1
 2 4 6 8 9 contains 8 at 4
 2 4 6 8 9 does not contain 10
 2 4 6 8 9 contains 9 at 5
 2 4 6 8 9 does not contain 5

```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
MODE ELEMENT = STRING;

# Iterative: #
PROC iterative binary search = ([]ELEMENT hay stack, ELEMENT needle)INT: (
    INT out,
        low := LWB hay stack,
        high := UPB hay stack;
    WHILE low < high DO
        INT mid := (low+high) OVER 2;
        IF hay stack[mid] > needle THEN high := mid-1
        ELIF hay stack[mid] < needle THEN low := mid+1
        ELSE out:= mid; stop iteration FI
    OD;
        low EXIT
    stop iteration:
        out

# Recursive: #
PROC recursive binary search = ([]ELEMENT hay stack, ELEMENT needle)INT: (
    IF LWB hay stack > UPB hay stack THEN
        LWB hay stack
    ELIF LWB hay stack = UPB hay stack THEN
        IF hay stack[LWB hay stack] = needle THEN LWB hay stack
        ELSE LWB hay stack FI
    ELSE
        INT mid := (LWB hay stack+UPB hay stack) OVER 2;
        IF hay stack[mid] > needle THEN recursive binary search(hay stack[:mid-1], needle)
        ELIF hay stack[mid] < needle THEN mid + recursive binary search(hay stack[mid+1:], needle)
        ELSE mid FI
   FI
);
# Test cases: #
test:(
  ELEMENT needle = "mister";
  []ELEMENT hay stack = ("AA","Maestro","Mario","Master","Mattress","Mister","Mistress","ZZ"),
          test cases = ("A","Master","Monk","ZZZ");

  PROC test search = (PROC([]ELEMENT, ELEMENT)INT search, []ELEMENT test cases)VOID:
    FOR case TO UPB test cases DO
        ELEMENT needle = test cases[case];
        INT index = search(hay stack, needle);
        BOOL found = ( index <= 0 | FALSE | hay stack[index]=needle);
        printf(($""""g""" "b("FOUND at","near")" index "dl$, needle, found, index))
    OD;
  test search(iterative binary search, test cases);
  test search(recursive binary search, test cases)
)
```

Output:

```txt

"A" near index 1
"Master" FOUND at index 4
"Monk" near index 8
"ZZZ" near index 8

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program binsearch.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessResult:        .ascii "Value find at index : "
sMessValeur:        .fill 11, 1, ' '            @ size => 11
szCarriageReturn:   .asciz "\n"
sMessRecursif:      .asciz "Recursive search : \n"
sMessNotFound:      .asciz "Value not found. \n"

.equ NBELEMENTS,      9
TableNumber:	     .int   4,6,7,10,11,15,22,30,35

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                           @ entry of program
    mov r0,#4                                   @ search first value
    ldr r1,iAdrTableNumber                      @ address number table
    mov r2,#NBELEMENTS                          @ number of élements
    bl bSearch
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message

    mov r0,#11                                  @ search median value
    ldr r1,iAdrTableNumber
    mov r2,#NBELEMENTS
    bl bSearch
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message

    mov r0,#12                                  @value not found
    ldr r1,iAdrTableNumber
    mov r2,#NBELEMENTS
    bl bSearch
    cmp r0,#-1
    bne 2f
    ldr r0,iAdrsMessNotFound
    bl affichageMess
    b 3f
2:
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message
3:
    mov r0,#35                                  @ search last value
    ldr r1,iAdrTableNumber
    mov r2,#NBELEMENTS
    bl bSearch
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message
/****************************************/
/*       recursive                      */
/****************************************/
    ldr r0,iAdrsMessRecursif
    bl affichageMess                            @ display message

    mov r0,#4                                   @ search first value
    ldr r1,iAdrTableNumber
    mov r2,#0                                   @ low index of elements
    mov r3,#NBELEMENTS - 1                      @ high index of elements
    bl bSearchR
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message

    mov r0,#11
    ldr r1,iAdrTableNumber
    mov r2,#0
    mov r3,#NBELEMENTS - 1
    bl bSearchR
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message

    mov r0,#12
    ldr r1,iAdrTableNumber
    mov r2,#0
    mov r3,#NBELEMENTS - 1
    bl bSearchR
    cmp r0,#-1
    bne 2f
    ldr r0,iAdrsMessNotFound
    bl affichageMess
    b 3f
2:
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message
3:
    mov r0,#35
    ldr r1,iAdrTableNumber
    mov r2,#0
    mov r3,#NBELEMENTS - 1
    bl bSearchR
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc #0                                      @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrsMessRecursif:        .int sMessRecursif
iAdrsMessNotFound:        .int sMessNotFound
iAdrTableNumber:          .int TableNumber

/******************************************************************/
/*     binary search   iterative                                  */
/******************************************************************/
/* r0 contains the value to search */
/* r1 contains the adress of table */
/* r2 contains the number of elements */
/* r0 return index or -1 if not find */
bSearch:
    push {r2-r5,lr}                                 @ save registers
    mov r3,#0                                       @ low index
    sub r4,r2,#1                                    @ high index = number of elements - 1
1:
    cmp r3,r4
    movgt r0,#-1                                    @not found
    bgt 100f
    add r2,r3,r4                                    @ compute (low + high) /2
    lsr r2,#1
    ldr r5,[r1,r2,lsl #2]                           @ load value of table at index r2
    cmp r5,r0
    moveq r0,r2                                     @ find !!!
    beq 100f
    addlt r3,r2,#1                                  @ lower -> index low = index + 1
    subgt r4,r2,#1                                  @ bigger -> index high = index - 1
    b 1b                                            @ and loop
100:
    pop {r2-r5,lr}
    bx lr                       @ return
/******************************************************************/
/*     binary search   recursif                                  */
/******************************************************************/
/* r0 contains the value to search */
/* r1 contains the adress of table */
/* r2 contains the low index of elements */
/* r3 contains the high index of elements */
/* r0 return index or -1 if not find */
bSearchR:
    push {r2-r5,lr}                                  @ save registers
    cmp r3,r2                                        @ index high < low ?
    movlt r0,#-1                                     @ yes -> not found
    blt 100f

    add r4,r2,r3                                     @ compute (low + high) /2
    lsr r4,#1
    ldr r5,[r1,r4,lsl #2]                            @ load value of table at index r4
    cmp r5,r0
    moveq r0,r4                                      @ find !!!
    beq 100f

    bgt 1f                                           @ bigger ?
    add r2,r4,#1                                     @ no new search with low = index + 1
    bl bSearchR
    b 100f
1:                                                   @ bigger
    sub r3,r4,#1                                     @ new search with high = index - 1
    bl bSearchR
100:
    pop {r2-r5,lr}
    bx lr                                            @ return
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres
    bx lr                                          @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                 @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:	                                            @ start loop
    bl divisionpar10U                               @unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b	                                    @ and loop
                                                    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
                                                     @ and move spaces in end on area
    mov r0,r4                                        @ result length
    mov r1,#' '                                      @ space
3:
    strb r1,[r3,r4]                                  @ store space in area
    add r4,#1                                        @ next position
    cmp r4,#LGZONECAL
    ble 3b                                           @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                   @ restaur registres
    bx lr                                            @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                        @ save value
    //mov r3,#0xCCCD                                 @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                              @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                             @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                               @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                             @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                             @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                            @ leave function
iMagicNumber:  	.int 0xCCCCCCCD


```




## AutoHotkey


```AutoHotkey
array := "1,2,4,6,8,9"
StringSplit, A, array, `,   ; creates associative array
MsgBox % x := BinarySearch(A, 4, 1, A0) ; Recursive
MsgBox % A%x%
MsgBox % x := BinarySearchI(A, A0, 4)  ; Iterative
MsgBox % A%x%

BinarySearch(A, value, low, high) { ; A0 contains length of array
  If (high < low)               ; A1, A2, A3...An are array elements
    Return not_found
  mid := Floor((low + high) / 2)
  If (A%mid% > value) ; A%mid% is automatically global since no such locals are present
    Return BinarySearch(A, value, low, mid - 1)
  Else If (A%mid% < value)
    Return BinarySearch(A, value, mid + 1, high)
  Else
    Return mid
}

BinarySearchI(A, lengthA, value) {
  low := 0
  high := lengthA - 1
  While (low <= high) {
    mid := Floor((low + high) / 2) ; round to lower integer
    If (A%mid% > value)
      high := mid - 1
    Else If (A%mid% < value)
      low := mid + 1
    Else
      Return mid
  }
  Return not_found
}
```



## AWK

{{works with|Gawk}}
{{works with|Mawk}}
{{works with|Nawk}}
'''Recursive'''

```awk
function binary_search(array, value, left, right,       middle) {
    if (right < left) return 0
    middle = int((right + left) / 2)
    if (value == array[middle]) return 1
    if (value <  array[middle])
        return binary_search(array, value, left, middle - 1)
    return binary_search(array, value, middle + 1, right)
}
```

'''Iterative'''

```awk
function binary_search(array, value, left, right,       middle) {
    while (left <= right) {
        middle = int((right + left) / 2)
        if (value == array[middle]) return 1
        if (value <  array[middle]) right = middle - 1
        else                        left  = middle + 1
    }
    return 0
}
```



## Axe

'''Iterative'''

BSEARCH takes 3 arguments: a pointer to the start of the data, the data to find, and the length of the array in bytes.


```axe
Lbl BSEARCH
0→L
r₃-1→H
While L≤H
 (L+H)/2→M
 If {L+M}>r₂
  M-1→H
 ElseIf {L+M}<r₂
  M+1→L
 Else
  M
  Return
 End
End
-1
Return
```



## BASIC

'''Recursive'''
{{works with|FreeBASIC}}
{{works with|RapidQ}}

```freebasic
FUNCTION binary_search ( array() AS Integer, value AS Integer, lo AS Integer, hi AS Integer) AS Integer
  DIM middle AS Integer

  IF hi < lo THEN
    binary_search = 0
  ELSE
    middle = (hi + lo) / 2
    SELECT CASE value
      CASE IS < array(middle)
	binary_search = binary_search(array(), value, lo, middle-1)
      CASE IS > array(middle)
	binary_search = binary_search(array(), value, middle+1, hi)
      CASE ELSE
	binary_search = middle
    END SELECT
  END IF
END FUNCTION
```

'''Iterative'''
{{works with|FreeBASIC}}
{{works with|RapidQ}}

```freebasic
FUNCTION binary_search ( array() AS Integer, value AS Integer, lo AS Integer, hi AS Integer) AS Integer
  DIM middle AS Integer

  WHILE lo <= hi
    middle = (hi + lo) / 2
    SELECT CASE value
      CASE IS < array(middle)
	hi = middle - 1
      CASE IS > array(middle)
	lo = middle + 1
      CASE ELSE
	binary_search = middle
	EXIT FUNCTION
    END SELECT
  WEND
  binary_search = 0
END FUNCTION
```

'''Testing the function'''

The following program can be used to test both recursive and iterative version.

```freebasic
SUB search (array() AS Integer, value AS Integer)
  DIM idx AS Integer

  idx = binary_search(array(), value, LBOUND(array), UBOUND(array))
  PRINT "Value "; value;
  IF idx < 1 THEN
    PRINT " not found"
  ELSE
    PRINT " found at index "; idx
  END IF
END SUB

DIM test(1 TO 10) AS Integer
DIM i AS Integer

DATA 2, 3, 5, 6, 8, 10, 11, 15, 19, 20
FOR i = 1 TO 10		' Fill the test array
  READ test(i)
NEXT i

search test(), 4
search test(), 8
search test(), 20
```

Output:
 Value 4 not found
 Value 8 found at index 5
 Value 20 found at index 10

=
## BBC BASIC
=

```bbcbasic
      DIM array%(9)
      array%() = 7, 14, 21, 28, 35, 42, 49, 56, 63, 70

      secret% = 42
      index% = FNwhere(array%(), secret%, 0, DIM(array%(),1))
      IF index% >= 0 THEN
        PRINT "The value "; secret% " was found at index "; index%
      ELSE
        PRINT "The value "; secret% " was not found"
      ENDIF
      END

      REM Search ordered array A%() for the value S% from index B% to T%
      DEF FNwhere(A%(), S%, B%, T%)
      LOCAL H%
      H% = 2
      WHILE H%<(T%-B%) H% *= 2:ENDWHILE
      H% /= 2
      REPEAT
        IF (B%+H%)<=T% IF S%>=A%(B%+H%) B% += H%
        H% /= 2
      UNTIL H%=0
      IF S%=A%(B%) THEN = B% ELSE = -1
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Search.bas"
110 RANDOMIZE
120 NUMERIC ARR(1 TO 20)
130 CALL FILL(ARR)
140 PRINT:INPUT PROMPT "Value: ":N
150 LET IDX=SEARCH(ARR,N)
160 IF IDX THEN
170   PRINT "The value";N;"was found the index";IDX
180 ELSE
190   PRINT "The value";N;"was not found."
200 END IF
210 DEF FILL(REF T)
220   LET T(LBOUND(T))=RND(3):PRINT T(1);
230   FOR I=LBOUND(T)+1 TO UBOUND(T)
240     LET T(I)=T(I-1)+RND(3)+1
250     PRINT T(I);
260   NEXT
270 END DEF
280 DEF SEARCH(REF T,N)
290   LET SEARCH=0:LET BO=LBOUND(T):LET UP=UBOUND(T)
300   DO
310     LET K=INT((BO+UP)/2)
320     IF T(K)<N THEN LET BO=K+1
330     IF T(K)>N THEN LET UP=K-1
340   LOOP WHILE BO<=UP AND T(K)<>N
350   IF BO<=UP THEN LET SEARCH=K
360 END DEF
```



## Batch File


```windowsnt

@echo off & setlocal enabledelayedexpansion

:: Binary Chop Algorithm - Michael Sanders 2017
::
:: example output...
::
:: binary chop algorithm vs. standard for loop
::
:: number to find 941
:: for loop required 941 iterations
:: binchop required 10 iterations

:setup

   set x=1
   set y=999
   set /a z=(%random% * (%y% - 1) / 32768 + 1)

:pseudoarray

   for /l %%q in (%x%,1,%y%) do set /a array[%%q]=%%q

:std4loop

   for /l %%q in (%x%,1,%y%) do (
      if !array[%%q]!==%z% (set f=%%q& goto :binchop)
   )

:binchop

   if !x! leq !y! (
      set /a i+=1
      set /a "p=(!x!+!y!)/2"
      call set /a t=%%array[!p!]%%
      if !t! equ !z! (set b=!i!& goto :done)
      if !t! lss !z! (set /a x=!p!+1) else (set /a y=!p!-1)
      goto :binchop
   )

:done

   cls
   echo binary chop algorithm vs. standard for loop...
   echo.
   echo . number to find !z!
   echo . for loop required !f! iterations
   echo . binchop required !b! iterations
   endlocal & exit /b 0

```



## Brat


```brat
binary_search = { search_array, value, low, high |
  true? high < low
    { null }
    {
      mid = ((low + high) / 2).to_i

      true? search_array[mid] > value
        { binary_search search_array, value, low, mid - 1 }
	{ true? search_array[mid] < value
	  { binary_search search_array, value, mid + 1, high }
	  { mid }
      }
   }
}

#Populate array
numbers = 1000.of { random 1000 }

#Sort the array
numbers.sort!

#Find a number
x = random 1000

p "Looking for #{x}"

index = binary_search numbers, x, 0, numbers.length - 1

null? index
	{ p "Not found" }
	{ p "Found at index: #{index}" }
```



## C



```c
#include <stdio.h>

int bsearch (int *a, int n, int x) {
    int i = 0, j = n - 1;
    while (i <= j) {
        int k = i + ((j - i) / 2);
        if (a[k] == x) {
            return k;
        }
        else if (a[k] < x) {
            i = k + 1;
        }
        else {
            j = k - 1;
        }
    }
    return -1;
}

int bsearch_r (int *a, int x, int i, int j) {
    if (j < i) {
        return -1;
    }
    int k = i + ((j - i) / 2);
    if (a[k] == x) {
        return k;
    }
    else if (a[k] < x) {
        return bsearch_r(a, x, k + 1, j);
    }
    else {
        return bsearch_r(a, x, i, k - 1);
    }
}

int main () {
    int a[] = {-31, 0, 1, 2, 2, 4, 65, 83, 99, 782};
    int n = sizeof a / sizeof a[0];
    int x = 2;
    int i = bsearch(a, n, x);
    printf("%d is at index %d\n", x, i);
    x = 5;
    i = bsearch_r(a, x, 0, n - 1);
    printf("%d is at index %d\n", x, i);
    return 0;
}

```

{{output}}

```txt

2 is at index 4
5 is at index -1

```



## C++

'''Recursive'''

```cpp

template <class T> int binsearch(const T array[], int low, int high, T value) {
    if (high < low) {
        return -1;
    }
    auto mid = (low + high) / 2;
    if (value < array[mid]) {
        return binsearch(array, low, mid - 1, value);
    } else if (value > array[mid]) {
        return binsearch(array, mid + 1, high, value);
    }
    return mid;
}

#include <iostream>
int main()
{
  int array[] = {2, 3, 5, 6, 8};
  int result1 = binsearch(array, 0, sizeof(array)/sizeof(int), 4),
      result2 = binsearch(array, 0, sizeof(array)/sizeof(int), 8);
  if (result1 == -1) std::cout << "4 not found!" << std::endl;
  else std::cout << "4 found at " << result1 << std::endl;
  if (result2 == -1) std::cout << "8 not found!" << std::endl;
  else std::cout << "8 found at " << result2 << std::endl;

  return 0;
}
```

'''Iterative'''

```cpp
template <class T>

int binSearch(const T arr[], int len, T what) {
  int low = 0;
  int high = len - 1;
  while (low <= high) {
    int mid = (low + high) / 2;
    if (arr[mid] > what)
      high = mid - 1;
    else if (arr[mid] < what)
      low = mid + 1;
    else
      return mid;
  }
  return -1; // indicate not found
}
```

'''Library'''
C++'s Standard Template Library has four functions for binary search, depending on what information you want to get.
They all need

```cpp
#include <algorithm>
```

The <code>lower_bound()</code> function returns an iterator to the first position where a value could be inserted without violating the order; i.e. the first element equal to the element you want, or the place where it would be inserted.

```cpp
int *ptr = std::lower_bound(array, array+len, what); // a custom comparator can be given as fourth arg
```


The <code>upper_bound()</code> function returns an iterator to the last position where a value could be inserted without violating the order; i.e. one past the last element equal to the element you want, or the place where it would be inserted.

```cpp
int *ptr = std::upper_bound(array, array+len, what); // a custom comparator can be given as fourth arg
```


The <code>equal_range()</code> function returns a pair of the results of <code>lower_bound()</code> and <code>upper_bound()</code>.

```cpp
std::pair<int *, int *> bounds = std::equal_range(array, array+len, what); // a custom comparator can be given as fourth arg
```

Note that the difference between the bounds is the number of elements equal to the element you want.

The <code>binary_search()</code> function returns true or false for whether an element equal to the one you want exists in the array. It does not give you any information as to where it is.

```cpp
bool found = std::binary_search(array, array+len, what); // a custom comparator can be given as fourth arg
```


## C#
'''Recursive'''

```c#
namespace Search {
  using System;

  public static partial class Extensions {
    /// <summary>Use Binary Search to find index of GLB for value</summary>
    /// <typeparam name="T">type of entries and value</typeparam>
    /// <param name="entries">array of entries</param>
    /// <param name="value">search value</param>
    /// <remarks>entries must be in ascending order</remarks>
    /// <returns>index into entries of GLB for value</returns>
    public static int RecursiveBinarySearchForGLB<T>(this T[] entries, T value)
      where T : IComparable {
      return entries.RecursiveBinarySearchForGLB(value, 0, entries.Length - 1);
    }

    /// <summary>Use Binary Search to find index of GLB for value</summary>
    /// <typeparam name="T">type of entries and value</typeparam>
    /// <param name="entries">array of entries</param>
    /// <param name="value">search value</param>
    /// <param name="left">leftmost index to search</param>
    /// <param name="right">rightmost index to search</param>
    /// <remarks>entries must be in ascending order</remarks>
    /// <returns>index into entries of GLB for value</returns>
    public static int RecursiveBinarySearchForGLB<T>(this T[] entries, T value, int left, int right)
      where T : IComparable {
      if (left <= right) {
        var middle = left + (right - left) / 2;
        return entries[middle].CompareTo(value) < 0 ?
          entries.RecursiveBinarySearchForGLB(value, middle + 1, right) :
          entries.RecursiveBinarySearchForGLB(value, left, middle - 1);
      }

      //[Assert]left == right + 1
      // GLB: entries[right] < value && value <= entries[right + 1]
      return right;
    }

    /// <summary>Use Binary Search to find index of LUB for value</summary>
    /// <typeparam name="T">type of entries and value</typeparam>
    /// <param name="entries">array of entries</param>
    /// <param name="value">search value</param>
    /// <remarks>entries must be in ascending order</remarks>
    /// <returns>index into entries of LUB for value</returns>
    public static int RecursiveBinarySearchForLUB<T>(this T[] entries, T value)
      where T : IComparable {
      return entries.RecursiveBinarySearchForLUB(value, 0, entries.Length - 1);
    }

    /// <summary>Use Binary Search to find index of LUB for value</summary>
    /// <typeparam name="T">type of entries and value</typeparam>
    /// <param name="entries">array of entries</param>
    /// <param name="value">search value</param>
    /// <param name="left">leftmost index to search</param>
    /// <param name="right">rightmost index to search</param>
    /// <remarks>entries must be in ascending order</remarks>
    /// <returns>index into entries of LUB for value</returns>
    public static int RecursiveBinarySearchForLUB<T>(this T[] entries, T value, int left, int right)
      where T : IComparable {
      if (left <= right) {
        var middle = left + (right - left) / 2;
        return entries[middle].CompareTo(value) <= 0 ?
          entries.RecursiveBinarySearchForLUB(value, middle + 1, right) :
          entries.RecursiveBinarySearchForLUB(value, left, middle - 1);
      }

      //[Assert]left == right + 1
      // LUB: entries[left] > value && value >= entries[left - 1]
      return left;
    }
  }
}
```

'''Iterative'''

```c#
namespace Search {
  using System;

  public static partial class Extensions {
    /// <summary>Use Binary Search to find index of GLB for value</summary>
    /// <typeparam name="T">type of entries and value</typeparam>
    /// <param name="entries">array of entries</param>
    /// <param name="value">search value</param>
    /// <remarks>entries must be in ascending order</remarks>
    /// <returns>index into entries of GLB for value</returns>
    public static int BinarySearchForGLB<T>(this T[] entries, T value)
      where T : IComparable {
      return entries.BinarySearchForGLB(value, 0, entries.Length - 1);
    }

    /// <summary>Use Binary Search to find index of GLB for value</summary>
    /// <typeparam name="T">type of entries and value</typeparam>
    /// <param name="entries">array of entries</param>
    /// <param name="value">search value</param>
    /// <param name="left">leftmost index to search</param>
    /// <param name="right">rightmost index to search</param>
    /// <remarks>entries must be in ascending order</remarks>
    /// <returns>index into entries of GLB for value</returns>
    public static int BinarySearchForGLB<T>(this T[] entries, T value, int left, int right)
      where T : IComparable {
      while (left <= right) {
        var middle = left + (right - left) / 2;
        if (entries[middle].CompareTo(value) < 0)
          left = middle + 1;
        else
          right = middle - 1;
      }

      //[Assert]left == right + 1
      // GLB: entries[right] < value && value <= entries[right + 1]
      return right;
    }

    /// <summary>Use Binary Search to find index of LUB for value</summary>
    /// <typeparam name="T">type of entries and value</typeparam>
    /// <param name="entries">array of entries</param>
    /// <param name="value">search value</param>
    /// <remarks>entries must be in ascending order</remarks>
    /// <returns>index into entries of LUB for value</returns>
    public static int BinarySearchForLUB<T>(this T[] entries, T value)
      where T : IComparable {
      return entries.BinarySearchForLUB(value, 0, entries.Length - 1);
    }

    /// <summary>Use Binary Search to find index of LUB for value</summary>
    /// <typeparam name="T">type of entries and value</typeparam>
    /// <param name="entries">array of entries</param>
    /// <param name="value">search value</param>
    /// <param name="left">leftmost index to search</param>
    /// <param name="right">rightmost index to search</param>
    /// <remarks>entries must be in ascending order</remarks>
    /// <returns>index into entries of LUB for value</returns>
    public static int BinarySearchForLUB<T>(this T[] entries, T value, int left, int right)
      where T : IComparable {
      while (left <= right) {
        var middle = left + (right - left) / 2;
        if (entries[middle].CompareTo(value) <= 0)
          left = middle + 1;
        else
          right = middle - 1;
      }

      //[Assert]left == right + 1
      // LUB: entries[left] > value && value >= entries[left - 1]
      return left;
    }
  }
}
```

'''Example'''

```c#
//#define UseRecursiveSearch

using System;
using Search;

class Program {
  static readonly int[][] tests = {
    new int[] { },
    new int[] { 2 },
    new int[] { 2, 2 },
    new int[] { 2, 2, 2, 2 },
    new int[] { 3, 3, 4, 4 },
    new int[] { 0, 1, 3, 3, 4, 4 },
    new int[] { 0, 1, 2, 2, 2, 3, 3, 4, 4},
    new int[] { 0, 1, 1, 2, 2, 2, 3, 3, 4, 4 },
    new int[] { 0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4 },
    new int[] { 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4 },
    new int[] { 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4 },
  };

  static void Main(string[] args) {
    var index = 0;
    foreach (var test in tests) {
      var join = String.Join(" ", test);
      Console.WriteLine($"test[{index}]: {join}");
#if UseRecursiveSearch
      var glb = test.RecursiveBinarySearchForGLB(2);
      var lub = test.RecursiveBinarySearchForLUB(2);
#else
      var glb = test.BinarySearchForGLB(2);
      var lub = test.BinarySearchForLUB(2);
#endif
      Console.WriteLine($"glb = {glb}");
      Console.WriteLine($"lub = {lub}");

      index++;
    }
#if DEBUG
    Console.Write("Press Enter");
    Console.ReadLine();
#endif
  }
}
```


'''Output'''

```txt
test[0]:
glb = -1
lub = 0
test[1]: 2
glb = -1
lub = 1
test[2]: 2 2
glb = -1
lub = 2
test[3]: 2 2 2 2
glb = -1
lub = 4
test[4]: 3 3 4 4
glb = -1
lub = 0
test[5]: 0 1 3 3 4 4
glb = 1
lub = 2
test[6]: 0 1 2 2 2 3 3 4 4
glb = 1
lub = 5
test[7]: 0 1 1 2 2 2 3 3 4 4
glb = 2
lub = 6
test[8]: 0 1 1 1 1 2 2 3 3 4 4
glb = 4
lub = 7
test[9]: 0 1 1 1 1 2 2 2 2 2 2 2 3 3 4 4
glb = 4
lub = 12
test[10]: 0 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 3 3 4 4
glb = 13
lub = 21
```



## Chapel


'''iterative''' -- almost a direct translation of the pseudocode

```chapel
proc binsearch(A:[], value) {
        var low = A.domain.dim(1).low;
        var high = A.domain.dim(1).high;
        while (low <= high) {
                var mid = (low + high) / 2;

                if A(mid) > value then
                        high = mid - 1;
                else if A(mid) < value then
                        low = mid + 1;
                else
                        return mid;
        }
        return 0;
}

writeln(binsearch([3, 4, 6, 9, 11], 9));
```


{{out}}
 4


## Clojure

'''Recursive'''

```clojure
(defn bsearch
  ([coll t]
    (bsearch coll 0 (dec (count coll)) t))
  ([coll l u t]
    (if (> l u) -1
      (let [m (quot (+ l u) 2) mth (nth coll m)]
        (cond
          ; the middle element is greater than t
          ; so search the lower half
          (> mth t) (recur coll l (dec m) t)
          ; the middle element is less than t
          ; so search the upper half
          (< mth t) (recur coll (inc m) u t)
          ; we've found our target
          ; so return its index
          (= mth t) m)))))
```



## COBOL

COBOL's <code>SEARCH ALL</code> statement is implemented as a binary search on most implementations.

```cobol>        >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. binary-search.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  nums-area                           VALUE "01040612184356".
    03  nums                            PIC 9(2)
                                        OCCURS 7 TIMES
                                        ASCENDING KEY nums
                                        INDEXED BY nums-idx.
PROCEDURE DIVISION.
    SEARCH ALL nums
        WHEN nums (nums-idx) = 4
            DISPLAY "Found 4 at index " nums-idx
    END-SEARCH
    .
END PROGRAM binary-search.
```



## CoffeeScript

'''Recursive'''

```coffeescript
binarySearch = (xs, x) ->
  do recurse = (low = 0, high = xs.length - 1) ->
    mid = Math.floor (low + high) / 2
    switch
      when high < low then NaN
      when xs[mid] > x then recurse low, mid - 1
      when xs[mid] < x then recurse mid + 1, high
      else mid
```

'''Iterative'''

```coffeescript
binarySearch = (xs, x) ->
  [low, high] = [0, xs.length - 1]
  while low <= high
    mid = Math.floor (low + high) / 2
    switch
      when xs[mid] > x then high = mid - 1
      when xs[mid] < x then low = mid + 1
      else return mid
  NaN
```

'''Test'''

```coffeescript
do (n = 12) ->
  odds = (it for it in [1..n] by 2)
  result = (it for it in \
    (binarySearch odds, it for it in [0..n]) \
    when not isNaN it)
  console.assert "#{result}" is "#{[0...odds.length]}"
  console.log "#{odds} are odd natural numbers"
  console.log "#{it} is ordinal of #{odds[it]}" for it in result
```

Output:

```txt
1,3,5,7,9,11 are odd natural numbers"
0 is ordinal of 1
1 is ordinal of 3
2 is ordinal of 5
3 is ordinal of 7
4 is ordinal of 9
5 is ordinal of 11
```



## Common Lisp

'''Iterative'''

```lisp
(defun binary-search (value array)
  (let ((low 0)
        (high (1- (length array))))

    (do () ((< high low) nil)
      (let ((middle (floor (+ low high) 2)))

        (cond ((> (aref array middle) value)
               (setf high (1- middle)))

              ((< (aref array middle) value)
               (setf low (1+ middle)))

              (t (return middle)))))))
```

'''Recursive'''

```lisp
(defun binary-search (value array &optional (low 0) (high (1- (length array))))
  (if (< high low)
      nil
      (let ((middle (floor (+ low high) 2)))

        (cond ((> (aref array middle) value)
               (binary-search value array low (1- middle)))

              ((< (aref array middle) value)
               (binary-search value array (1+ middle) high))

              (t middle)))))
```


## Crystal

'''Recursive'''

```ruby
class Array
  def binary_search(val, low = 0, high = (size - 1))
    return nil if high < low
    #mid = (low + high) >> 1
    mid = low + ((high - low) >> 1)
    case val <=> self[mid]
      when -1
        binary_search(val, low, mid - 1)
      when 1
        binary_search(val, mid + 1, high)
      else mid
    end
  end
end

ary = [0,1,4,5,6,7,8,9,12,26,45,67,78,90,98,123,211,234,456,769,865,2345,3215,14345,24324]

[0, 42, 45, 24324, 99999].each do |val|
  i = ary.binary_search(val)
  if i
    puts "found #{val} at index #{i}: #{ary[i]}"
  else
    puts "#{val} not found in array"
  end
end
```

'''Iterative'''

```ruby
class Array
  def binary_search_iterative(val)
    low, high = 0, size - 1
    while low <= high
      #mid = (low + high) >> 1
      mid = low + ((high - low) >> 1)
      case val <=> self[mid]
        when 1
          low = mid + 1
        when -1
          high = mid - 1
        else
          return mid
      end
    end
    nil
  end
end

ary = [0,1,4,5,6,7,8,9,12,26,45,67,78,90,98,123,211,234,456,769,865,2345,3215,14345,24324]

[0, 42, 45, 24324, 99999].each do |val|
  i = ary.binary_search_iterative(val)
  if i
    puts "found #{val} at index #{i}: #{ary[i]}"
  else
    puts "#{val} not found in array"
  end
end
```

{{out}}

```txt

found 0 at index 0: 0
42 not found in array
found 45 at index 10: 45
found 24324 at index 24: 24324
99999 not found in array

```



## D


```d
import std.stdio, std.array, std.range, std.traits;

/// Recursive.
bool binarySearch(R, T)(/*in*/ R data, in T x) pure nothrow @nogc
if (isRandomAccessRange!R && is(Unqual!T == Unqual!(ElementType!R))) {
    if (data.empty)
        return false;
    immutable i = data.length / 2;
    immutable mid = data[i];
    if (mid > x)
        return data[0 .. i].binarySearch(x);
    if (mid < x)
        return data[i + 1 .. $].binarySearch(x);
    return true;
}

/// Iterative.
bool binarySearchIt(R, T)(/*in*/ R data, in T x) pure nothrow @nogc
if (isRandomAccessRange!R && is(Unqual!T == Unqual!(ElementType!R))) {
    while (!data.empty) {
        immutable i = data.length / 2;
        immutable mid = data[i];
        if (mid > x)
            data = data[0 .. i];
        else if (mid < x)
            data = data[i + 1 .. $];
        else
            return true;
    }
    return false;
}

void main() {
    /*const*/ auto items = [2, 4, 6, 8, 9].assumeSorted;
    foreach (const x; [1, 8, 10, 9, 5, 2])
        writefln("%2d %5s %5s %5s", x,
                 items.binarySearch(x),
                 items.binarySearchIt(x),
                 // Standard Binary Search:
                 !items.equalRange(x).empty);
}
```

{{out}}

```txt
 1 false false false
 8  true  true  true
10 false false false
 9  true  true  true
 5 false false false
 2  true  true  true
```



## E


```e
/** Returns null if the value is not found. */
def binarySearch(collection, value) {
    var low := 0
    var high := collection.size() - 1
    while (low <= high) {
        def mid := (low + high) // 2
        def comparison := value.op__cmp(collection[mid])
        if      (comparison.belowZero()) { high := mid - 1 } \
        else if (comparison.aboveZero()) { low := mid + 1 }  \
        else if (comparison.isZero())    { return mid }      \
        else                             { throw("You expect me to binary search with a partial order?") }
    }
    return null
}
```


## EasyLang

<lang>func bin_search val . a[] res .
  low = 0
  high = len a[] - 1
  res = -1
  while low <= high and res = -1
    mid = (low + high) / 2
    if a[mid] > val
      high = mid - 1
    elif a[mid] < val
      low = mid + 1
    else
      res = mid
    .
  .
.
a[] = [ 2 4 6 8 9 ]
call bin_search 8 a[] r
print r
```



## Eiffel


The following solution is based on the one described in: C. A. Furia, B. Meyer, and S. Velder. ''Loop Invariants: Analysis, Classification, and Examples''. ACM Computing Surveys, 46(3), Article 34, January 2014. (Also available at http://arxiv.org/abs/1211.4470). It includes detailed loop invariants and pre- and postconditions, which make the running time linear (instead of logarithmic) when full contract checking is enabled.


```Eiffel
class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
		local
			a: ARRAY [INTEGER]
			keys: ARRAY [INTEGER]
		do
			a := <<0, 1, 4, 5, 6, 7, 8, 9,
			       12, 26, 45, 67, 78, 90,
			       98, 123, 211, 234, 456,
			       769, 865, 2345, 3215,
			       14345, 24324>>
			keys := <<0, 42, 45, 24324, 99999>>
			across keys as k loop
				if has_binary (a, k.item) then
					print ("The array has an element " + k.item.out)
				else
					print ("The array has NOT an element " + k.item.out)
				end
				print ("%N")
			end
		end

feature -- Search

	has_binary (a: ARRAY [INTEGER]; key: INTEGER): BOOLEAN
		-- Does `a[a.lower..a.upper]' include an element `key'?
		require
			is_sorted (a, a.lower, a.upper)
		local
			i: INTEGER
		do
			i := where_binary (a, key)
			if a.lower <= i and i <= a.upper then
				Result := True
			else
				Result := False
			end
		end

	where_binary (a: ARRAY [INTEGER]; key: INTEGER): INTEGER
		-- The index of an element `key' within `a[a.lower..a.upper]' if it exists.
		-- Otherwise an integer outside `[a.lower..a.upper]'
		require
			is_sorted (a, a.lower, a.upper)
		do
			Result := where_binary_range (a, key, a.lower, a.upper)
		end

	where_binary_range (a: ARRAY [INTEGER]; key: INTEGER; low, high: INTEGER): INTEGER
		-- The index of an element `key' within `a[low..high]' if it exists.
		-- Otherwise an integer outside `[low..high]'
		note
			source: "http://arxiv.org/abs/1211.4470"
		require
			is_sorted (a, low, high)
		local
			i, j, mid: INTEGER
		do
			if low > high then
				Result := low - 1
			else
				from
					i := low
					j := high
					mid := low
					Result := low - 1
				invariant
					low <= i and i <= mid + 1
					low <= mid and mid <= j and j <= high
					i <= j
					has (a, key, i, j) = has (a, key, low, high)
				until
					i >= j
				loop
					mid := i + (j - i) // 2
					if a [mid] < key then
						i := mid + 1
					else
						j := mid
					end
				variant
					j - i
				end
				if a [i] = key then
					Result := i
				end
			end
		ensure
			low <= Result and Result <= high implies a [Result] = key
			Result < low or Result > high implies not has (a, key, low, high)
		end

feature -- Implementation

	is_sorted (a: ARRAY [INTEGER]; low, high: INTEGER): BOOLEAN
		-- Is `a[low..high]' sorted in nondecreasing order?
		require
			a.lower <= low
			high <= a.upper
		do
			Result := across low |..| (high - 1) as i all a [i.item] <= a [i.item + 1] end
		end

	has (a: ARRAY [INTEGER]; key: INTEGER; low, high: INTEGER): BOOLEAN
		-- Is there an element `key' in `a[low..high]'?
		require
			a.lower <= low
			high <= a.upper
		do
			Result := across low |..| high as i some a [i.item] = key end
		end

end
```



## Elixir


```elixir
defmodule Binary do
  def search(list, value), do: search(List.to_tuple(list), value, 0, length(list)-1)

  def search(_tuple, _value, low, high) when high < low, do: :not_found
  def search(tuple, value, low, high) do
    mid = div(low + high, 2)
    midval = elem(tuple, mid)
    cond do
      value <  midval -> search(tuple, value, low, mid-1)
      value >  midval -> search(tuple, value, mid+1, high)
      value == midval -> mid
    end
  end
end

list = [0,1,4,5,6,7,8,9,12,26,45,67,78,90,98,123,211,234,456,769,865,2345,3215,14345,24324]
Enum.each([0,42,45,24324,99999], fn val ->
  case Binary.search(list, val) do
    :not_found -> IO.puts "#{val} not found in list"
    index      -> IO.puts "found #{val} at index #{index}"
  end
end)
```


{{out}}

```txt

found 0 at index 0
42 not found in list
found 45 at index 10
found 24324 at index 24
99999 not found in list

```


## Emacs Lisp


```lisp

 (defun binary-search (value array)
  (let ((low 0)
        (high (1- (length array))))

    (do () ((< high low) nil)
      (let ((middle (floor (+ low high) 2)))

        (cond ((> (aref array middle) value)
               (setf high (1- middle)))

              ((< (aref array middle) value)
               (setf low (1+ middle)))

              (t (return middle)))))))
```



## Erlang


```Erlang
%% Task: Binary Search algorithm
%% Author: Abhay Jain

-module(searching_algorithm).
-export([start/0]).

start() ->
    List = [1,2,3],
    binary_search(List, 5, 1, length(List)).


binary_search(List, Value, Low, High) ->
    if Low > High ->
        io:format("Number ~p not found~n", [Value]),
        not_found;
       true ->
        Mid = (Low + High) div 2,
        MidNum = lists:nth(Mid, List),
        if MidNum > Value ->
            binary_search(List, Value, Low, Mid-1);
           MidNum < Value ->
            binary_search(List, Value, Mid+1, High);
           true ->
            io:format("Number ~p found at index ~p", [Value, Mid]),
            Mid
        end
    end.
```



## Euphoria


### Recursive


```euphoria
function binary_search(sequence s, object val, integer low, integer high)
    integer mid, cmp
    if high < low then
        return 0 -- not found
    else
        mid = floor( (low + high) / 2 )
        cmp = compare(s[mid], val)
        if  cmp > 0 then
            return binary_search(s, val, low, mid-1)
        elsif cmp < 0 then
            return binary_search(s, val, mid+1, high)
        else
            return mid
        end if
    end if
end function
```


### Iterative


```euphoria
function binary_search(sequence s, object val)
    integer low, high, mid, cmp
    low = 1
    high = length(s)
    while low <= high do
        mid = floor( (low + high) / 2 )
        cmp = compare(s[mid], val)
        if cmp > 0 then
            high = mid - 1
        elsif cmp < 0 then
            low = mid + 1
        else
            return mid
        end if
    end while
    return 0 -- not found
end function
```


=={{header|F Sharp|F#}}==
Generic recursive version, using #light syntax:

```fsharp
let rec binarySearch (myArray:array<IComparable>, low:int, high:int, value:IComparable) =
    if (high < low) then
        null
    else
        let mid = (low + high) / 2

        if (myArray.[mid] > value) then
            binarySearch (myArray, low, mid-1, value)
        else if (myArray.[mid] < value) then
            binarySearch (myArray, mid+1, high, value)
        else
            myArray.[mid]
```



## FBSL

FBSL has built-in QuickSort() and BSearch() functions:

```qbasic
#APPTYPE CONSOLE

DIM va[], sign = {1, -1}, toggle

PRINT "Loading ... ";
DIM gtc = GetTickCount()
FOR DIM i = 0 TO 1000000
	va[] = sign[toggle] * PI * i
	toggle = NOT toggle		' randomize the array
NEXT
PRINT "done in ", GetTickCount() - gtc, " milliseconds"

PRINT "Sorting ... ";
gtc = GetTickCount()
QUICKSORT(va)				' quick sort the array
PRINT "done in ", GetTickCount() - gtc, " milliseconds"

gtc = GetTickCount()
PRINT 1000000 * PI, " found at index ", BSEARCH(va, 1000000 * PI), _	' binary search through the array
	" in ", GetTickCount() - gtc, " milliseconds"

PAUSE
```

Output:
```txt
Loading ... done in 906 milliseconds
Sorting ... done in 547 milliseconds
3141592.65358979 found at index 1000000 in 0 milliseconds

Press any key to continue...
```

User-defined implementations of the same would be considerably slower. Nonetheless, here they are in order to comply with the task requirements.

'''Iterative:'''

```qbasic
#APPTYPE CONSOLE

DIM va[]

PRINT "Loading ... ";
DIM gtc = GetTickCount()
FOR DIM i = 0 TO 1000000: va[] = i * PI: NEXT
PRINT "done in ", GetTickCount() - gtc, " milliseconds"

gtc = GetTickCount()
PRINT 1000000 * PI, " found at index ", BSearchIter(va, 1000000 * PI), _
	" in ", GetTickCount() - gtc, " milliseconds"

PAUSE

FUNCTION BSearchIter(BYVAL array, BYVAL num)
	STATIC low = LBOUND(va), high = UBOUND(va)
	WHILE low <= high
		DIM midp = (high + low) \ 2
		IF array[midp] > num THEN
			high = midp - 1
		ELSEIF array[midp] < num THEN
			low = midp + 1
		ELSE
			RETURN midp
		END IF
	WEND
	RETURN -1
END FUNCTION
```

Output:
```txt
Loading ... done in 391 milliseconds
3141592.65358979 found at index 1000000 in 62 milliseconds

Press any key to continue...
```


'''Recursive:'''

```qbasic
#APPTYPE CONSOLE

DIM va[]

PRINT "Loading ... ";
DIM gtc = GetTickCount()
FOR DIM i = 0 TO 1000000: va[] = i * PI: NEXT
PRINT "done in ", GetTickCount() - gtc, " milliseconds"

gtc = GetTickCount()
PRINT 1000000 * PI, " found at index ", BSearchRec(va, 1000000 * PI, LBOUND(va), UBOUND(va)), _
	" in ", GetTickCount() - gtc, " milliseconds"

PAUSE

FUNCTION BSearchRec(BYVAL array, BYVAL num, BYVAL low, BYVAL high)
	IF high < low THEN RETURN -1
	DIM midp = (high + low) \ 2
	IF array[midp] > num THEN
		RETURN BSearchRec(array, num, low, midp - 1)
	ELSEIF array[midp] < num THEN
		RETURN BSearchRec(array, num, midp + 1, high)
	END IF
	RETURN midp
END FUNCTION
```

Output:
```txt
Loading ... done in 390 milliseconds
3141592.65358979 found at index 1000000 in 938 milliseconds

Press any key to continue...
```



## Factor

Factor already includes a binary search in its standard library. The following code offers an interface compatible with the requirement of this task, and returns either the index of the element if it has been found or f otherwise.

```factor
USING: binary-search kernel math.order ;

: binary-search ( seq elt -- index/f )
    [ [ <=> ] curry search ] keep = [ drop f ] unless ;
```



## Forth

This version is designed for maintaining a sorted array. If the item is not found, then then location returned is the proper insertion point for the item. This could be used in an optimized [[Insertion sort]], for example.

```forth
defer (compare)
' - is (compare) \ default to numbers

: cstr-compare ( cstr1 cstr2 -- <=> ) \ counted strings
  swap count rot count compare ;

: mid ( u l -- mid ) tuck - 2/ -cell and + ;

: bsearch ( item upper lower -- where found? )
  rot >r
  begin  2dup >
  while  2dup mid
         dup @ r@ (compare)
         dup
  while  0<
         if   nip cell+   ( upper mid+1 )
         else rot drop swap ( mid lower )
         then
  repeat drop nip nip             true
  else   max ( insertion-point ) false
  then
  r> drop ;

create test 2 , 4 , 6 , 9 , 11 ,   99 ,
: probe ( n -- ) test 5 cells bounds bsearch . @ . cr ;
1 probe \ 0 2
2 probe \ -1 2
3 probe \ 0 4
10 probe \ 0 11
11 probe \ -1 11
12 probe \ 0 99
```



## Fortran

'''Recursive'''
In ISO Fortran 90 or later use a RECURSIVE function and ARRAY SECTION argument:

```fortran
recursive function binarySearch_R (a, value) result (bsresult)
    real, intent(in) :: a(:), value
    integer          :: bsresult, mid

    mid = size(a)/2 + 1
    if (size(a) == 0) then
        bsresult = 0        ! not found
    else if (a(mid) > value) then
        bsresult= binarySearch_R(a(:mid-1), value)
    else if (a(mid) < value) then
        bsresult = binarySearch_R(a(mid+1:), value)
        if (bsresult /= 0) then
            bsresult = mid + bsresult
        end if
    else
        bsresult = mid      ! SUCCESS!!
    end if
end function binarySearch_R
```

'''Iterative'''


In ISO Fortran 90 or later use an ARRAY SECTION POINTER:

```fortran
function binarySearch_I (a, value)
    integer                  :: binarySearch_I
    real, intent(in), target :: a(:)
    real, intent(in)         :: value
    real, pointer            :: p(:)
    integer                  :: mid, offset

    p => a
    binarySearch_I = 0
    offset = 0
    do while (size(p) > 0)
        mid = size(p)/2 + 1
        if (p(mid) > value) then
            p => p(:mid-1)
        else if (p(mid) < value) then
            offset = offset + mid
            p => p(mid+1:)
        else
            binarySearch_I = offset + mid    ! SUCCESS!!
            return
        end if
    end do
end function binarySearch_I
```


===Iterative, exclusive bounds, three-way test.===
This has the array indexed from 1 to N, and the "not found" return code is zero or negative. Changing the search to be for A(first:last) is trivial, but the "not-found" return protocol would require adjustment, as when starting the array indexing at zero. Aside from the "not found" report, The variables used in the search ''must'' be able to hold the values ''first - 1'' and ''last + 1'' so for example with sixteen-bit two's complement integers the maximum value for ''last'' is 3276'''6''', '''not''' 3276'''7'''.

Depending on the version of Fortran the compiler supports, the specification of the array parameter may vary, as A(1) or A(*) or A(:), and in the latter case, parameter N could be omitted because the size of an array parameter may be ascertained via the SIZE function. For the more advanced fortrans, declaring the parameters to be INTENT(IN) may help, as despite passing arrays "by reference" being the norm, the newer compilers may generate copy-in, copy-out code, vitiating the whole point of using a fast binary search instead of a slow linear search. In this case, INTENT(IN) will at least prevent the copy-back. In such a situation however, preparing in-line code may be the better move: fortunately, there is not a lot of code involved. There is no point in using an explicitly recursive version (even though the same actions may result during execution) because of the overhead of parameter passing and procedure entry/exit.

Later compilers offer features allowing the development of "generic" functions so that the same function name may be used yet the actual routine invoked will be selected according to how the parameters are integers or floating-point, and of different precisions. There would still need to be a version of the function for each type combination, each with its own name. Unfortunately, there is no three-way comparison test for character data.

The use of "exclusive" bounds simplifies the adjustment of the bounds: the appropriate bound simply receives the value of P, there is ''no'' + 1 or - 1 adjustment ''at every step''; similarly, the determination of an empty span is easy, and avoiding the risk of integer overflow via (L + R)/2 is achieved at the same time. The "inclusive" bounds version by contrast requires ''two'' manipulations of L and R ''at every step'' - once to see if the span is empty, and a second time to locate the index to test.


```Fortran
      INTEGER FUNCTION FINDI(X,A,N)	!Binary chopper. Find i such that X = A(i)
Careful: it is surprisingly difficult to make this neat, due to vexations when N = 0 or 1.
       REAL X,A(*)		!Where is X in array A(1:N)?
       INTEGER N		!The count.
       INTEGER L,R,P		!Fingers.
        L = 0			!Establish outer bounds, to search A(L+1:R-1).
        R = N + 1		!L = first - 1; R = last + 1.
    1   P = (R - L)/2		!Probe point. Beware INTEGER overflow with (L + R)/2.
        IF (P.LE.0) GO TO 5	!Aha! Nowhere!! The span is empty.
        P = P + L		!Convert an offset from L to an array index.
        IF (X - A(P)) 3,4,2	!Compare to the probe point.
    2   L = P			!A(P) < X. Shift the left bound up: X follows A(P).
        GO TO 1			!Another chop.
    3   R = P			!X < A(P). Shift the right bound down: X precedes A(P).
        GO TO 1			!Try again.
    4   FINDI = P		!A(P) = X. So, X is found, here!
       RETURN			!Done.
Curse it!
    5   FINDI = -L		!X is not found. Insert it at L + 1, i.e. at A(1 - FINDI).
      END FUNCTION FINDI	!A's values need not be all different, merely in order.
```


[[File:BinarySearch.Flowchart.png]]

### =Statistics=

Imagine a test array containing the even numbers: 2,4,6,8. A count could be kept of the number of probes required to find each of those four values, and likewise with a search for the odd numbers 1,3,5,7,9 that would probe all the places where a value might be not found. Plot the average number of probes for the two cases, plus the maximum number of probes for any case, and then repeat for another number of elements to search. With only one element in the array to be searched, all values are the same: one probe.

[[File:BinarySearchStats63.png]]

[[File:BinarySearch.Stats32767.png]]



### =An alternative version=


```Fortran
      INTEGER FUNCTION FINDI(X,A,N)	!Binary chopper. Find i such that X = A(i)
Careful: it is surprisingly difficult to make this neat, due to vexations when N = 0 or 1.
       REAL X,A(*)		!Where is X in array A(1:N)?
       INTEGER N		!The count.
       INTEGER L,R,P		!Fingers.
        L = 0			!Establish outer bounds, to search A(L+1:R-1).
        R = N + 1		!L = first - 1; R = last + 1.
        GO TO 1			!Hop to it.
    2   L = P			!A(P) < X. Shift the left bound up: X follows A(P).
    1   P = (R - L)/2		!Probe point. Beware INTEGER overflow with (L + R)/2.
        IF (P.LE.0) GO TO 5	!Aha! Nowhere!! The span is empty.
        P = P + L		!Convert an offset from L to an array index.
        IF (X - A(P)) 3,4,2	!Compare to the probe point.
    3   R = P			!X < A(P). Shift the right bound down: X precedes A(P).
        GO TO 1			!Try again.
    4   FINDI = P		!A(P) = X. So, X is found, here!
       RETURN			!Done.
Curse it!
    5   FINDI = -L		!X is not found. Insert it at L + 1, i.e. at A(1 - FINDI).
      END FUNCTION FINDI	!A's values need not be all different, merely in order.
```

The point of this is that the IF-test is going to initiate some jumps, so why not arrange that one of the bound adjustments needs no subsequent jump to the start of the next iteration - in the first version, both bound adjustments needed such a jump, the GO TO 1 statements. This was done by shifting the code for label 2 up to precede the code for label 1 - and removing its now pointless GO TO 1 (executed each time), but adding an initial GO TO 1, executed once only. This sort of change is routine when manipulating spaghetti code...

It is because the method involves such a small amount of effort per iteration that minor changes offer a significant benefit. A lot depends on the implementation of the three-way test: the hope is that after the comparison, the computer hardware has indicators set for various outcomes, so that the necessary conditional branches can be made through successive inspection of those indicators, rather than repeating the comparison. These branch tests may in turn be made in an order that notes which option (if any) involves "falling through" to the next statement, thus it may be better to swap the order of labels 3 and 4. Further, the compiler may itself choose to re-order the various code pieces. First Fortran (in 1958) had a FREQUENCY statement whereby the programmer could indicate which paths were the more likely - for the binary search, equality is the less likely discovery. An assembler version of this routine attended to all these details.

Some compilers do not produce machine code directly, but instead translate the source code into another language which is then compiled, and a common choice for that is C. This is all very well, but C is one of the many languages that do ''not'' have a three-way test option and so cannot represent Fortran's three-way IF statement directly. Before emitting asservations of faith that pseudocode such as
  if expression > 0 then optionP
   else if expression < 0 then optionN
    else optionZ;
will be recognised by the most excellent compiler producing only one comparison, note that the two expressions are ''not'' the same (one has <, the other >), and test what happens with pseudocode such as
  if X > 0 then print "Positive"
   else if X > 0 then print "Still positive";
That is, does the compiler make any remark, and does the resulting machine code contain a redundant test? However, despite all the above, the three-way IF statement has been declared deprecated in later versions of Fortran, with no alternative to repeated testing offered.

Incidentally, the exclusive-bounds version leads to a good version of the interpolation search (whereby the probe position is interpolated, not just in the middle of the span), unlike the version based on inclusive-bounds. Further, the unsourced offering in Wikipedia contains a bug - try searching an array of two equal elements for that value.


## Futhark

{{incorrect|Futhark|Futhark's syntax has changed, so this example will not compile}}

Straightforward translation of imperative iterative algorithm.


```Futhark

fun main(as: [n]int, value: int): int =
  let low = 0
  let high = n-1
  loop ((low,high)) = while low <= high do
    -- invariants: value > as[i] for all i < low
    --             value < as[i] for all i > high
    let mid = (low+high) / 2
    in if as[mid] > value
       then (low, mid - 1)
       else if as[mid] < value
       then (mid + 1, high)
       else (mid, mid-1) -- Force termination.
  in low

```



## GAP


```gap
Find := function(v, x)
  local low, high, mid;
  low := 1;
  high := Length(v);
  while low <= high do
    mid := QuoInt(low + high, 2);
    if v[mid] > x then
      high := mid - 1;
    elif v[mid] < x then
      low := mid + 1;
    else
      return mid;
    fi;
  od;
  return fail;
end;

u := [1..10]*7;
# [ 7, 14, 21, 28, 35, 42, 49, 56, 63, 70 ]
Find(u, 34);
# fail
Find(u, 35);
# 5
```



## Go

'''Recursive''':

```go
func binarySearch(a []float64, value float64, low int, high int) int {
    if high < low {
        return -1
    }
    mid := (low + high) / 2
    if a[mid] > value {
        return binarySearch(a, value, low, mid-1)
    } else if a[mid] < value {
        return binarySearch(a, value, mid+1, high)
    }
    return mid
}
```

'''Iterative''':

```go
func binarySearch(a []float64, value float64) int {
    low := 0
    high := len(a) - 1
    for low <= high {
        mid := (low + high) / 2
        if a[mid] > value {
            high = mid - 1
        } else if a[mid] < value {
            low = mid + 1
        } else {
            return mid
        }
    }
    return -1
}
```

'''Library''':

```go
import "sort"

//...

sort.SearchInts([]int{0,1,4,5,6,7,8,9}, 6) // evaluates to 4
```

Exploration of library source code shows that it uses the <tt>mid = low + (high - low) / 2</tt> technique to avoid overflow.

There are also functions <code>sort.SearchFloat64s()</code>, <code>sort.SearchStrings()</code>, and a very general <code>sort.Search()</code> function that allows you to binary search a range of numbers based on any condition (not necessarily just search for an index of an element in an array).


## Groovy

Both solutions use ''sublists'' and a tracking offset in preference to "high" and "low".

### =Recursive Solution=


```groovy

def binSearchR
//define binSearchR closure.
binSearchR = { a, key, offset=0 ->
    def m = n.intdiv(2)
    def n = a.size()
    a.empty \
        ? ["The insertion point is": offset] \
        : a[m] > key \
            ? binSearchR(a[0..<m],key, offset) \
            : a[m] < target \
                ? binSearchR(a[(m + 1)..<n],key, offset + m + 1) \
                : [index: offset + m]
}

```



### =Iterative Solution=


```groovy
def binSearchI = { aList, target ->
    def a = aList
    def offset = 0
    while (!a.empty) {
        def n = a.size()
        def m = n.intdiv(2)
        if(a[m] > target) {
            a = a[0..<m]
        } else if (a[m] < target) {
            a = a[(m + 1)..<n]
            offset += m + 1
        } else {
            return [index: offset + m]
        }
    }
    return ["insertion point": offset]
}
```

Test:

```groovy
def a = [] as Set
def random = new Random()
while (a.size() < 20) { a << random.nextInt(30) }
def source = a.sort()
source[0..-2].eachWithIndex { si, i -> assert si < source[i+1] }

println "${source}"
1.upto(5) {
    target = random.nextInt(10) + (it - 2) * 10
    print "Trial #${it}. Looking for: ${target}"
    def answers = [binSearchR, binSearchI].collect { search ->
        search(source, target)
    }
    assert answers[0] == answers[1]
    println """
    Answer: ${answers[0]}, : ${source[answers[0].values().iterator().next()]}"""
}
```

Output:

```txt
[1, 2, 5, 8, 9, 10, 11, 14, 15, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29]
Trial #1. Looking for: -9
    Answer: [insertion point:0], : 1
Trial #2. Looking for: 7
    Answer: [insertion point:3], : 8
Trial #3. Looking for: 18
    Answer: [index:9], : 18
Trial #4. Looking for: 29
    Answer: [index:19], : 29
Trial #5. Looking for: 32
    Answer: [insertion point:20], : null
```



## Haskell


### Recursive algorithm


The algorithm itself, parametrized by an "interrogation" predicate ''p'' in the spirit of the explanation above:

```haskell
import Data.Array (Array, Ix, (!), listArray, bounds)

-- BINARY SEARCH --------------------------------------------------------------
bSearch
  :: Integral a
  => (a -> Ordering) -> (a, a) -> Maybe a
bSearch p (low, high)
  | high < low = Nothing
  | otherwise =
    let mid = (low + high) `div` 2
    in case p mid of
         LT -> bSearch p (low, mid - 1)
         GT -> bSearch p (mid + 1, high)
         EQ -> Just mid

-- Application to an array:
bSearchArray
  :: (Ix i, Integral i, Ord e)
  => Array i e -> e -> Maybe i
bSearchArray a x = bSearch (compare x . (a !)) (bounds a)

-- TEST -----------------------------------------------------------------------
axs
  :: (Num i, Ix i)
  => Array i String
axs =
  listArray
    (0, 11)
    [ "alpha"
    , "beta"
    , "delta"
    , "epsilon"
    , "eta"
    , "gamma"
    , "iota"
    , "kappa"
    , "lambda"
    , "mu"
    , "theta"
    , "zeta"
    ]

main :: IO ()
main =
  let e = "mu"
      found = bSearchArray axs e
  in putStrLn $
     '\'' :
     e ++
     case found of
       Nothing -> "' Not found"
       Just x -> "' found at index " ++ show x
```

{{Out}}

```txt
'mu' found at index 9
```

The algorithm uses tail recursion, so the iterative and the recursive approach are identical in Haskell (the compiler will convert recursive calls into jumps).

A common optimisation of recursion is to delegate the main computation to a helper function with simpler type signature. For the option type of the return value, we could also use an Either as an alternative to a Maybe.


```haskell
import Data.Array (Array, Ix, (!), listArray, bounds)

-- BINARY SEARCH USING A HELPER FUNCTION WITH A SIMPLER TYPE SIGNATURE
findIndexBinary
  :: Ord a
  => (a -> Ordering) -> Array Int a -> Either String Int
findIndexBinary p axs =
  let go (lo, hi)
        | hi < lo = Left "not found"
        | otherwise =
          let mid = (lo + hi) `div` 2
          in case p (axs ! mid) of
               LT -> go (lo, pred mid)
               GT -> go (succ mid, hi)
               EQ -> Right mid
  in go (bounds axs)

-- TEST ---------------------------------------------------
haystack :: Array Int String
haystack =
  listArray
    (0, 11)
    [ "alpha"
    , "beta"
    , "delta"
    , "epsilon"
    , "eta"
    , "gamma"
    , "iota"
    , "kappa"
    , "lambda"
    , "mu"
    , "theta"
    , "zeta"
    ]

main :: IO ()
main =
  let needle = "lambda"
  in putStrLn $
     '\'' :
     needle ++
     either
       ("' " ++)
       (("' found at index " ++) . show)
       (findIndexBinary (compare needle) haystack)
```

{{Out}}

```txt
'lambda' found at index 8
```



### Iterative algorithm


The iterative algorithm could be written in terms of the '''until''' function, which takes a predicate '''p''', a function '''f''', and a seed value '''x'''.

It returns the result of applying '''f''' until '''p''' holds.


```haskell
import Data.Array (Array, Ix, (!), listArray, bounds)

-- BINARY SEARCH USING THE ITERATIVE ALGORITHM
findIndexBinary_
  :: Ord a
  => (a -> Ordering) -> Array Int a -> Either String Int
findIndexBinary_ p axs =
  let (lo, hi) =
        until
          (\(lo, hi) -> lo > hi || 0 == hi)
          (\(lo, hi) ->
              let m = quot (lo + hi) 2
              in case p (axs ! m) of
                   LT -> (lo, pred m)
                   GT -> (succ m, hi)
                   EQ -> (m, 0))
          (bounds axs) :: (Int, Int)
  in if 0 /= hi
       then Left "not found"
       else Right lo

-- TEST ---------------------------------------------------
haystack :: Array Int String
haystack =
  listArray
    (0, 11)
    [ "alpha"
    , "beta"
    , "delta"
    , "epsilon"
    , "eta"
    , "gamma"
    , "iota"
    , "kappa"
    , "lambda"
    , "mu"
    , "theta"
    , "zeta"
    ]

main :: IO ()
main =
  let needle = "kappa"
  in putStrLn $
     '\'' :
     needle ++
     either
       ("' " ++)
       (("' found at index " ++) . show)
       (findIndexBinary_ (compare needle) haystack)
```

{{Out}}

```txt
'kappa' found at index 7
```



## HicEst


```hicest
REAL :: n=10,  array(n)

   array = NINT( RAN(n) )
   SORT(Vector=array, Sorted=array)
   x = NINT( RAN(n) )

   idx = binarySearch( array, x )
   WRITE(ClipBoard) x, "has position ", idx, "in ", array
 END

FUNCTION binarySearch(A, value)
   REAL :: A(1), value

   low = 1
   high = LEN(A)
   DO i = 1, high
     IF( low > high) THEN
       binarySearch = 0
       RETURN
     ELSE
       mid = INT( (low + high) / 2 )
       IF( A(mid) > value) THEN
         high = mid - 1
       ELSEIF( A(mid) < value ) THEN
         low = mid + 1
       ELSE
         binarySearch = mid
         RETURN
       ENDIF
     ENDIF
   ENDDO
 END
```


```hicest
7 has position 9 in 0 0 1 2 3 3 4 6 7 8
5 has position 0 in 0 0 1 2 3 3 4 6 7 8
```


=={{header|Icon}} and {{header|Unicon}}==
Only a recursive solution is shown here.

```icon
procedure binsearch(A, target)
    if *A = 0 then fail
    mid := *A/2 + 1
    if target > A[mid] then {
        return mid + binsearch(A[(mid+1):0], target)
        }
    else if target < A[mid] then {
        return binsearch(A[1+:(mid-1)], target)
        }
    return mid
end
```

A program to test this is:

```icon
procedure main(args)
    target := integer(!args) | 3
    every put(A := [], 1 to 18 by 2)

    outList("Searching", A)
    write(target," is ",("at "||binsearch(A, target)) | "not found")
end

procedure outList(prefix, A)
    writes(prefix,": ")
    every writes(!A," ")
    write()
end
```

with some sample runs:

```txt

->bins 0
Searching: 1 3 5 7 9 11 13 15 17
0 is not found
->bins 1
Searching: 1 3 5 7 9 11 13 15 17
1 is at 1
->bins 2
Searching: 1 3 5 7 9 11 13 15 17
2 is not found
->bins 3
Searching: 1 3 5 7 9 11 13 15 17
3 is at 2
->bins 16
Searching: 1 3 5 7 9 11 13 15 17
16 is not found
->bins 17
Searching: 1 3 5 7 9 11 13 15 17
17 is at 9
->bins 7
Searching: 1 3 5 7 9 11 13 15 17
7 is at 4
->bins 9
Searching: 1 3 5 7 9 11 13 15 17
9 is at 5
->bins 10
Searching: 1 3 5 7 9 11 13 15 17
10 is not found
->

```



## J

J already includes a binary search primitive (<code>I.</code>). The following code offers an interface compatible with the requirement of this task, and returns either the index of the element if it has been found or 'Not Found' otherwise:

```j
bs=. i. 'Not Found'"_^:(-.@-:) I.
```

'''Examples:'''

```j
   2 3 5 6 8 10 11 15 19 20 bs 11
6
   2 3 5 6 8 10 11 15 19 20 bs 12
Not Found
```

Direct tacit iterative and recursive versions to compare to other implementations follow:

'''Iterative'''

```j
'X Y L H M'=. i.5                            NB. Setting mnemonics for boxes
f=. &({::)                                   NB. Fetching the contents of a box
o=. @:                                       NB. Composing verbs (functions)

boxes=. ; , a: $~ 3:                         NB. Appending 3 (empty) boxes to the inputs
LowHigh=. (0 ; # o (X f)) (L,H)} ]           NB. Setting the low and high bounds
midpoint=. < o (<. o (2 %~ L f + H f)) M} ]  NB. Updating the midpoint
case=.     >: o * o (Y f - M f { X f)        NB. Less=0, equal=1, or greater=2

squeeze=. (< o (_1 + M f) H} ])`(< o _: L} ])`(< o (1 + M f) L} ])@.case
return=.   (M f) o ((<@:('Not Found'"_) M} ]) ^: (_ ~: L f))

bs=. return o (squeeze o midpoint ^: (L f <: H f) ^:_) o LowHigh o boxes
```

'''Recursive'''

```j
'X Y L H M'=. i.5                            NB. Setting mnemonics for boxes
f=. &({::)                                   NB. Fetching the contents of a box
o=. @:                                       NB. Composing verbs (functions)

boxes=. a: ,~ ;                              NB. Appending 1 (empty) box to the inputs
midpoint=. < o (<. o (2 %~ L f + H f)) M} ]  NB. Updating the midpoint
case=.     >: o * o (Y f - M f { X f)        NB. Less=0, equal=1, or greater=2

recur=. (X f bs Y f ; L f ; (_1 + M f))`(M f)`(X f bs Y f ; (1 + M f) ; H f)@.case

bs=. (recur o midpoint`('Not Found'"_) @. (H f < L f) o boxes) :: ([ bs ] ; 0 ; (<: o # o [))
```



## Java

'''Iterative'''

```java
public class BinarySearchIterative {

    public static int binarySearch(int[] nums, int check) {
        int hi = nums.length - 1;
        int lo = 0;
        while (hi >= lo) {
            int guess = (lo + hi) >>> 1;  // from OpenJDK
            if (nums[guess] > check) {
                hi = guess - 1;
            } else if (nums[guess] < check) {
                lo = guess + 1;
            } else {
                return guess;
            }
        }
        return -1;
    }

    public static void main(String[] args) {
        int[] haystack = {1, 5, 6, 7, 8, 11};
        int needle = 5;
        int index = binarySearch(haystack, needle);
        if (index == -1) {
            System.out.println(needle + " is not in the array");
        } else {
            System.out.println(needle + " is at index " + index);
        }
    }
}
```


'''Recursive'''


```java
public class BinarySearchRecursive {

    public static int binarySearch(int[] haystack, int needle, int lo, int hi) {
        if (hi < lo) {
            return -1;
        }
        int guess = (hi + lo) / 2;
        if (haystack[guess] > needle) {
            return binarySearch(haystack, needle, lo, guess - 1);
        } else if (haystack[guess] < needle) {
            return binarySearch(haystack, needle, guess + 1, hi);
        }
        return guess;
    }

    public static void main(String[] args) {
        int[] haystack = {1, 5, 6, 7, 8, 11};
        int needle = 5;

        int index = binarySearch(haystack, needle, 0, haystack.length);

        if (index == -1) {
            System.out.println(needle + " is not in the array");
        } else {
            System.out.println(needle + " is at index " + index);
        }
    }
}
```

'''Library'''
When the key is not found, the following functions return <code>~insertionPoint</code> (the bitwise complement of the index where the key would be inserted, which is guaranteed to be a negative number).

For arrays:

```java
import java.util.Arrays;

int index = Arrays.binarySearch(array, thing);
int index = Arrays.binarySearch(array, startIndex, endIndex, thing);

// for objects, also optionally accepts an additional comparator argument:
int index = Arrays.binarySearch(array, thing, comparator);
int index = Arrays.binarySearch(array, startIndex, endIndex, thing, comparator);
```

For Lists:

```java
import java.util.Collections;

int index = Collections.binarySearch(list, thing);
int index = Collections.binarySearch(list, thing, comparator);
```



## JavaScript


### ES5

Recursive binary search implementation

```javascript
function binary_search_recursive(a, value, lo, hi) {
  if (hi < lo) { return null; }

  var mid = Math.floor((lo + hi) / 2);

  if (a[mid] > value) {
    return binary_search_recursive(a, value, lo, mid - 1);
  }
  if (a[mid] < value) {
    return binary_search_recursive(a, value, mid + 1, hi);
  }
  return mid;
}
```

Iterative binary search implementation

```javascript
function binary_search_iterative(a, value) {
  var mid, lo = 0,
      hi = a.length - 1;

  while (lo <= hi) {
    mid = Math.floor((lo + hi) / 2);

    if (a[mid] > value) {
      hi = mid - 1;
    } else if (a[mid] < value) {
      lo = mid + 1;
    } else {
      return mid;
    }
  }
  return null;
}
```



### ES6


Recursive and iterative, by composition of pure functions, with tests and output:


```javascript
(() => {
    'use strict';

    const main = () => {

        // findRecursive :: a -> [a] -> Either String Int
        const findRecursive = (x, xs) => {
            const go = (lo, hi) => {
                if (hi < lo) {
                    return Left('not found');
                } else {
                    const
                        mid = div(lo + hi, 2),
                        v = xs[mid];
                    return v > x ? (
                        go(lo, mid - 1)
                    ) : v < x ? (
                        go(mid + 1, hi)
                    ) : Right(mid);
                }
            };
            return go(0, xs.length);
        };


        // findRecursive :: a -> [a] -> Either String Int
        const findIter = (x, xs) => {
            const [m, l, h] = until(
                ([mid, lo, hi]) => lo > hi || lo === mid,
                ([mid, lo, hi]) => {
                    const
                        m = div(lo + hi, 2),
                        v = xs[m];
                    return v > x ? [
                        m, lo, m - 1
                    ] : v < x ? [
                        m, m + 1, hi
                    ] : [m, m, hi];
                },
                [div(xs.length / 2), 0, xs.length - 1]
            );
            return l > h ? (
                Left('not found')
            ) : Right(m);
        };

        // TESTS ------------------------------------------

        const
            // (pre-sorted AZ)
            xs = ["alpha", "beta", "delta", "epsilon", "eta", "gamma",
                "iota", "kappa", "lambda", "mu", "nu", "theta", "zeta"
            ];
        return JSON.stringify([
            'Recursive',
            map(x => either(
                    l => "'" + x + "' " + l,
                    r => "'" + x + "' found at index " + r,
                    findRecursive(x, xs)
                ),
                knuthShuffle(['cape'].concat(xs).concat('cairo'))
            ),
            '',
            'Iterative:',
            map(x => either(
                    l => "'" + x + "' " + l,
                    r => "'" + x + "' found at index " + r,
                    findIter(x, xs)
                ),
                knuthShuffle(['cape'].concat(xs).concat('cairo'))
            )
        ], null, 2);
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Left :: a -> Either a b
    const Left = x => ({
        type: 'Either',
        Left: x
    });

    // Right :: b -> Either a b
    const Right = x => ({
        type: 'Either',
        Right: x
    });

    // div :: Int -> Int -> Int
    const div = (x, y) => Math.floor(x / y);

    // either :: (a -> c) -> (b -> c) -> Either a b -> c
    const either = (fl, fr, e) =>
        'Either' === e.type ? (
            undefined !== e.Left ? (
                fl(e.Left)
            ) : fr(e.Right)
        ) : undefined;

    // Abbreviation for quick testing

    // enumFromTo :: (Int, Int) -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // FOR TESTS

    // knuthShuffle :: [a] -> [a]
    const knuthShuffle = xs => {
        const swapped = (iFrom, iTo, xs) =>
            xs.map(
                (x, i) => iFrom !== i ? (
                    iTo !== i ? x : xs[iFrom]
                ) : xs[iTo]
            );
        return enumFromTo(0, xs.length - 1)
            .reduceRight((a, i) => {
                const iRand = randomRInt(0, i)();
                return i !== iRand ? (
                    swapped(i, iRand, a)
                ) : a;
            }, xs);
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);


    // FOR TESTS

    // randomRInt :: Int -> Int -> IO () -> Int
    const randomRInt = (low, high) => () =>
        low + Math.floor(
            (Math.random() * ((high - low) + 1))
        );

    // reverse :: [a] -> [a]
    const reverse = xs =>
        'string' !== typeof xs ? (
            xs.slice(0).reverse()
        ) : xs.split('').reverse().join('');

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
[
  "Recursive",
  [
    "'delta' found at index 2",
    "'cairo' not found",
    "'cape' not found",
    "'gamma' found at index 5",
    "'eta' found at index 4",
    "'kappa' found at index 7",
    "'alpha' found at index 0",
    "'mu' found at index 9",
    "'beta' found at index 1",
    "'epsilon' found at index 3",
    "'nu' found at index 10",
    "'iota' found at index 6",
    "'theta' found at index 11",
    "'lambda' found at index 8",
    "'zeta' found at index 12"
  ],
  "",
  "Iterative:",
  [
    "'theta' found at index 11",
    "'kappa' found at index 7",
    "'zeta' found at index 12",
    "'cairo' not found",
    "'epsilon' found at index 3",
    "'beta' found at index 1",
    "'nu' found at index 10",
    "'eta' found at index 4",
    "'alpha' found at index 0",
    "'lambda' found at index 8",
    "'iota' found at index 6",
    "'mu' found at index 9",
    "'gamma' found at index 5",
    "'delta' found at index 2",
    "'cape' not found"
  ]
]
```



## jq

If the input array is sorted, then binarySearch(value) as defined here will return an index (i.e. offset) of value in the array if the array contains the value, and otherwise (-1 - ix), where ix is the insertion point, if the value cannot be found.  binarySearch will always terminate.

Recursive solution:
```jq
def binarySearch(value):
  # To avoid copying the array, simply pass in the current low and high offsets
  def binarySearch(low; high):
      if (high < low) then (-1 - low)
      else ( (low + high) / 2 | floor) as $mid
           | if (.[$mid] > value) then binarySearch(low; $mid-1)
             elif (.[$mid] < value) then binarySearch($mid+1; high)
             else $mid
             end
      end;
   binarySearch(0; length-1);
```

Example:
```jq
[-1,-1.1,1,1,null,[null]] | binarySearch(1)
```

{{Out}}
2


## Jsish


```javascript
/**
   Binary search, in Jsish, based on Javascript entry
   Tectonics: jsish -u -time true -verbose true binarySearch.jsi
*/
function binarySearchIterative(haystack, needle) {
    var mid, low = 0, high = haystack.length - 1;

    while (low <= high) {
        mid = Math.floor((low + high) / 2);
        if (haystack[mid] > needle) {
            high = mid - 1;
        } else if (haystack[mid] < needle) {
            low = mid + 1;
        } else {
            return mid;
        }
    }
    return null;
}

/* recursive */
function binarySearchRecursive(haystack, needle, low, high) {
    if (high < low) { return null; }

    var mid = Math.floor((low + high) / 2);

    if (haystack[mid] > needle) {
        return binarySearchRecursive(haystack, needle, low, mid - 1);
    }
    if (haystack[mid] < needle) {
        return binarySearchRecursive(haystack, needle, mid + 1, high);
    }
    return mid;
}

/* Testing and timing */
if (Interp.conf('unitTest') > 0) {
    var arr = [];
    for (var i = -5000; i <= 5000; i++) { arr.push(i); }

    assert(arr.length == 10001);
    assert(binarySearchIterative(arr, 0) == 5000);
    assert(binarySearchRecursive(arr, 0, 0, arr.length - 1) == 5000);

    assert(binarySearchIterative(arr, 5000) == 10000);
    assert(binarySearchRecursive(arr, -5000, 0, arr.length - 1) == 0);

    assert(binarySearchIterative(arr, -5001) == null);

    puts('--Time 100 passes--');
    puts('Iterative:', Util.times(function() { binarySearchIterative(arr, 42); }, 100), 'µs');
    puts('Recursive:', Util.times(function() { binarySearchRecursive(arr, 42, 0, arr.length - 1); }, 100), 'µs');
}
```


{{out}}

```txt
prompt$ jsish -u -time true -verbose true binarySearch.jsi
Test binarySearch.jsi
CMD: /usr/local/bin/jsish -Iasserts true -IunitTest 1 binarySearch.jsi
OUTPUT: <--Time 100 passes--
Iterative: 25969 µs
Recursive: 40863 µs
>
[PASS] binarySearch.jsi          (165 ms)
```



## Julia

{{works with|Julia|0.6}}
'''Iterative''':

```julia
function binarysearch(lst::Vector{T}, val::T) where T
    low = 1
    high = length(lst)
    while low ≤ high
        mid = (low + high) ÷ 2
        if lst[mid] > val
            high = mid - 1
        elseif lst[mid] < val
            low = mid + 1
        else
            return mid
        end
    end
    return 0
end
```


'''Recursive''':

```julia
function binarysearch(lst::Vector{T}, value::T, low=1, high=length(lst)) where T
    if isempty(lst) return 0 end
    if low ≥ high
        if low > high || lst[low] != value
            return 0
        else
            return low
        end
    end
    mid = (low + high) ÷ 2
    if lst[mid] > value
        return binarysearch(lst, value, low, mid-1)
    elseif lst[mid] < value
        return binarysearch(lst, value, mid+1, high)
    else
        return mid
    end
end
```



## K

Recursive:

```K

bs:{[a;t]
    if[0=#a; :_n];
    m:_(#a)%2;
    if[t>a@m
        tmp:_f[(m+1) _ a;t]
        :[_n~tmp; :_n; :1+m+tmp]]
    if[t<a@m
        :_f[m#a;t]]
    :m
}

  v:8 30 35 45 49 77 79 82 87 97
  {bs[v;x]}' v
0 1 2 3 4 5 6 7 8 9

```



## Kotlin


```scala>fun <T : Comparable<T>> Array<T
.iterativeBinarySearch(target: T): Int {
    var hi = size - 1
    var lo = 0
    while (hi >= lo) {
        val guess = lo + (hi - lo) / 2
        if (this[guess] > target) hi = guess - 1
        else if (this[guess] < target) lo = guess + 1
        else return guess
    }
    return -1
}

fun <T : Comparable<T>> Array<T>.recursiveBinarySearch(target: T, lo: Int, hi: Int): Int {
    if (hi < lo) return -1

    val guess = (hi + lo) / 2

    return if (this[guess] > target) recursiveBinarySearch(target, lo, guess - 1)
    else if (this[guess] < target) recursiveBinarySearch(target, guess + 1, hi)
    else guess
}

fun main(args: Array<String>) {
    val a = arrayOf(1, 3, 4, 5, 6, 7, 8, 9, 10)
    var target = 6
    var r = a.iterativeBinarySearch(target)
    println(if (r < 0) "$target not found" else "$target found at index $r")
    target = 250
    r = a.iterativeBinarySearch(target)
    println(if (r < 0) "$target not found" else "$target found at index $r")

    target = 6
    r = a.recursiveBinarySearch(target, 0, a.size)
    println(if (r < 0) "$target not found" else "$target found at index $r")
    target = 250
    r = a.recursiveBinarySearch(target, 0, a.size)
    println(if (r < 0) "$target not found" else "$target found at index $r")
}
```

{{Out}}

```txt
6 found at index 4
250 not found
6 found at index 4
250 not found
```



## Lambdatalk

Can be tested in (http://lambdaway.free.fr)[http://lambdaway.free.fr/lambdaway/?view=binary_search]

```scheme

{def BS
 {def BS.r {lambda {:a :v :i0 :i1}
  {let { {:a :a} {:v :v} {:i0 :i0} {:i1 :i1}
         {:m {floor {* {+ :i0 :i1} 0.5}}} }
  {if {<  :i1 :i0}
   then :v is not found
   else {if {> {array.item :a :m} :v}
   then {BS.r :a :v :i0 {- :m 1} }
   else {if {<  {array.item :a :m} :v}
   then {BS.r :a :v {+ :m 1} :i1 }
   else :v is at array[:m] }}}}} }
 {lambda {:a :v}
  {BS.r :a :v 0 {- {array.length :a} 1}} }}
-> BS

{def A {array 12 14 16 18 20 22 25 27 30}}
-> A = [12,14,16,18,20,22,25,27,30]

{BS {A} -1}  -> -1 is not found
{BS {A} 24}  -> 24 is not found
{BS {A} 25}  -> 25 is at array[6]
{BS {A} 123} -> 123 is not found

{def B {array {serie 1 100000 2}}}
-> B = [1,3,5,... 99997,99999]

{BS {B} 100}   -> 100 is not found
{BS {B} 12345} -> 12345 is at array[6172]

```



## Liberty BASIC


```lb

dim theArray(100)
for i = 1 to 100
  theArray(i) = i
next i

print binarySearch(80,30,90)

wait

FUNCTION binarySearch(val, lo, hi)
  IF hi < lo THEN
    binarySearch = 0
  ELSE
    middle = int((hi + lo) / 2):print middle
    if val < theArray(middle) then binarySearch = binarySearch(val, lo, middle-1)
    if val > theArray(middle) then binarySearch = binarySearch(val, middle+1, hi)
    if val = theArray(middle) then binarySearch = middle
  END IF
END FUNCTION

```



## Logo


```logo
to bsearch :value :a :lower :upper
  if :upper < :lower [output []]
  localmake "mid int (:lower + :upper) / 2
  if item :mid :a > :value [output bsearch :value :a :lower :mid-1]
  if item :mid :a < :value [output bsearch :value :a :mid+1 :upper]
  output :mid
end
```



## Lolcode

'''Iterative'''

```lolcode

HAI 1.2
  CAN HAS STDIO?

  VISIBLE "HAI WORLD!!!1!"
  VISIBLE "IMA GONNA SHOW U BINA POUNCE NAO"

  I HAS A list ITZ A BUKKIT
  list HAS A index0 ITZ 2
  list HAS A index1 ITZ 3
  list HAS A index2 ITZ 5
  list HAS A index3 ITZ 7
  list HAS A index4 ITZ 8
  list HAS A index5 ITZ 9
  list HAS A index6 ITZ 12
  list HAS A index7 ITZ 20

  BTW Method to access list by index number aka: list[index4]
  HOW IZ list access YR indexNameNumber
	FOUND YR list'Z SRS indexNameNumber
  IF U SAY SO

  BTW Method to print the array on the same line
  HOW IZ list printList
  I HAS A allList ITZ ""
	I HAS A indexNameNumber ITZ "index0"
	I HAS A index ITZ 0
	IM IN YR walkingLoop UPPIN YR index TIL BOTH SAEM index AN 8
		indexNameNumber R SMOOSH "index" index MKAY
		allList R SMOOSH allList " " list IZ access YR indexNameNumber MKAY MKAY
	IM OUTTA YR walkingLoop
	FOUND YR allList
  IF U SAY SO

  VISIBLE "WE START WIF BUKKIT LIEK DIS: " list IZ printList MKAY

  I HAS A target ITZ 12
  VISIBLE "AN TARGET LIEK DIS: " target

  VISIBLE "AN NAO 4 MAGI"

  HOW IZ I binaPounce YR list AN YR listLength AN YR target
	I HAS A left ITZ 0
	I HAS A right ITZ DIFF OF listLength AN 1
	IM IN YR whileLoop
		BTW exit while loop when left > right
		DIFFRINT left AN SMALLR OF left AN right
		O RLY?
			YA RLY
				GTFO
		OIC

		I HAS A mid ITZ QUOSHUNT OF SUM OF left AN right AN 2
		I HAS A midIndexname ITZ SMOOSH "index" mid MKAY

		BTW if target == list[mid] return mid
		BOTH SAEM target AN list IZ access YR midIndexname MKAY
		O RLY?
			YA RLY
				FOUND YR mid
		OIC

		BTW if target < list[mid] right = mid - 1
		DIFFRINT target AN BIGGR OF target AN list IZ access YR midIndexname MKAY
		O RLY?
			YA RLY
				right R DIFF OF mid AN 1
		OIC

		BTW if target > list[mid] left = mid + 1
		DIFFRINT target AN SMALLR OF target AN list IZ access YR midIndexname MKAY
		O RLY?
			YA RLY
				left R SUM OF mid AN 1
		OIC
	IM OUTTA YR whileLoop

	FOUND YR -1
  IF U SAY SO

  BTW call binary search on target here and print the index
  I HAS A targetIndex ITZ I IZ binaPounce YR list AN YR 8 AN YR target MKAY
  VISIBLE "TARGET " target " IZ IN BUKKIT " targetIndex

  VISIBLE "WE HAS TEH TARGET!!1!!"
  VISIBLE "I CAN HAS UR CHEEZBURGER NAO?"

KTHXBYE
end
```

Output

```txt

HAI WORLD!!!1!
IMA GONNA SHOW U BINA POUNCE NAO
WE START WIF BUKKIT LIEK DIS:  2 3 5 7 8 9 12 20
AN TARGET LIEK DIS: 12
AN NAO 4 MAGI
TARGET 12 IZ IN BUKKIT 6
WE HAS TEH TARGET!!1!!
I CAN HAS UR CHEEZBURGER NAO?

```



## Lua

'''Iterative'''

```lua
function binarySearch (list,value)
    local low = 1
    local high = #list
    while low <= high do
        local mid = math.floor((low+high)/2)
        if list[mid] > value then high = mid - 1
        elseif list[mid] < value then low = mid + 1
        else return mid
        end
    end
    return false
end
```

'''Recursive'''

```lua
function binarySearch (list, value)
    local function search(low, high)
        if low > high then return false end
        local mid = math.floor((low+high)/2)
        if list[mid] > value then return search(low,mid-1) end
        if list[mid] < value then return search(mid+1,high) end
        return mid
    end
    return search(1,#list)
end
```


## M2000 Interpreter


```M2000 Interpreter

\\ binary search
const N=10
Dim A(0 to N-1)
A(0):=1,2,3,4,5,6,8,9,10,11
Print Len(A())=10
Function BinarySearch(&A(), aValue) {
	def long mid, lo, hi
	def boolean ok=False
	let lo=0, hi=Len(A())-1
	While lo<=hi
		mid=(lo+hi)/2
		if A(mid)>aValue Then
			hi=mid-1
		Else.if A(mid)<aValue Then
			lo=mid+1
		Else
			=mid
			ok=True
			exit
		End if
	End While
	if not ok then =-lo-1
}
For i=0 to 12
Rem	Print "Search for value:";i
	where= BinarySearch(&A(), i)
	if where>=0 then
		Print "found i at index: ";where
	else
		where=-where-1
		if where<len(A()) then
			Print "Not found, we can insert it at index: ";where
			Dim A(len(A())+1)   ' redim
			stock A(where)	 keep len(A())-where-1, A(where+1)  'move items up
			A(where)=i  ' insert value
		Else
			Print "Not found, we can append to array at index: ";where
			Dim A(len(A())+1)   ' redim
			A(where)=i  ' insert value
		End If
	end if
next i
Print Len(A())=13
Print A()


```



## M4


```M4
define(`notfound',`-1')dnl
define(`midsearch',`ifelse(defn($1[$4]),$2,$4,
`ifelse(eval(defn($1[$4])>$2),1,`binarysearch($1,$2,$3,decr($4))',`binarysearch($1,$2,incr($4),$5)')')')dnl
define(`binarysearch',`ifelse(eval($4<$3),1,notfound,`midsearch($1,$2,$3,eval(($3+$4)/2),$4)')')dnl
dnl
define(`setrange',`ifelse(`$3',`',$2,`define($1[$2],$3)`'setrange($1,incr($2),shift(shift(shift($@))))')')dnl
define(`asize',decr(setrange(`a',1,1,3,5,7,11,13,17,19,23,29)))dnl
dnl
binarysearch(`a',5,1,asize)
binarysearch(`a',8,1,asize)
```

Output:

```txt

3
-1

```



## Maple

The calculation of "mid" cannot overflow, since Maple uses arbitrary precision integer arithmetic, and the largest list or array is far, far smaller than the effective range of integers.

'''Recursive'''

```Maple
BinarySearch := proc( A, value, low, high )
        description "recursive binary search";
        if high < low then
                FAIL
        else
                local mid := iquo( high + low, 2 );
                if A[ mid ] > value then
                        thisproc( A, value, low, mid - 1 )
                elif A[ mid ] < value then
                        thisproc( A, value, mid + 1, high )
                else
                        mid
                end if
        end if
end proc:
```


'''Iterative'''

```Maple
BinarySearch := proc( A, value )
        description "iterative binary search";
        local low, high;

        low, high := ( lowerbound, upperbound )( A );
        while low <= high do
                local mid := iquo( low + high, 2 );
                if A[ mid ] > value then
                        high := mid - 1
                elif A[ mid ] < value then
                        low := mid + 1
                else
                        return mid
                end if
        end do;
        FAIL
end proc:
```

We can use either lists or Arrays (or Vectors) for the first argument for these.

```Maple>
 N := 10:
> P := [seq]( ithprime( i ), i = 1 .. N ):
> BinarySearch( P, 12, 1, N ); # recursive version
                                  FAIL

> BinarySearch( P, 13, 1, N ); # recursive version
                                   6

> BinarySearch( Array( P ), 13, 1, N ); # make P into an array
                                   6

> PP := Array( -2 .. 7, P ): # check it works if the array is not 1-based.
> BinarySearch( PP, 13 ); # iterative version
                                   3

> PP[ 3 ];
                                   13
```



=={{header|Mathematica}} / {{header|Wolfram Language}}==
'''Recursive'''

```Mathematica
BinarySearchRecursive[x_List, val_, lo_, hi_] :=
 Module[{mid = lo + Round@((hi - lo)/2)},
  If[hi < lo, Return[-1]];
  Return[
   Which[x[[mid]] > val, BinarySearchRecursive[x, val, lo, mid - 1],
    x[[mid]] < val, BinarySearchRecursive[x, val, mid + 1, hi],
    True, mid]
   ];
  ]
```

'''Iterative'''

```Mathematica
BinarySearch[x_List, val_] := Module[{lo = 1, hi = Length@x, mid},
  While[lo <= hi,
   mid = lo + Round@((hi - lo)/2);
   Which[x[[mid]] > val, hi = mid - 1,
    x[[mid]] < val, lo = mid + 1,
    True, Return[mid]
    ];
   ];
  Return[-1];
  ]
```



## MATLAB

'''Recursive'''

```MATLAB
function mid = binarySearchRec(list,value,low,high)

    if( high < low )
        mid = [];
        return
    end

    mid = floor((low + high)/2);

    if( list(mid) > value )
        mid = binarySearchRec(list,value,low,mid-1);
        return
    elseif( list(mid) < value )
        mid = binarySearchRec(list,value,mid+1,high);
        return
    else
        return
    end

end
```

Sample Usage:

```MATLAB>>
 binarySearchRec([1 2 3 4 5 6 6.5 7 8 9 11 18],6.5,1,numel([1 2 3 4 5 6 6.5 7 8 9 11 18]))

ans =

     7
```

'''Iterative'''

```MATLAB
function mid = binarySearchIter(list,value)

    low = 1;
    high = numel(list) - 1;

    while( low <= high )
        mid = floor((low + high)/2);

        if( list(mid) > value )
            high = mid - 1;
        elseif( list(mid) < value )
        	low = mid + 1;
        else
            return
        end
    end

    mid = [];

end
```

Sample Usage:

```MATLAB>>
 binarySearchIter([1 2 3 4 5 6 6.5 7 8 9 11 18],6.5)

ans =

     7
```



## Maxima


```maxima
find(L, n) := block([i: 1, j: length(L), k, p],
    if n < L[i] or n > L[j] then 0 else (
        while j - i > 0 do (
            k: quotient(i + j, 2),
            p: L[k],
            if n < p then j: k - 1 elseif n > p then i: k + 1 else i: j: k
        ),
        if n = L[i] then i else 0
    )
)$

".."(a, b) := if a < b then makelist(i, i, a, b) else makelist(i, i, a, b, -1)$
infix("..")$

a: sublist(1 .. 1000, primep)$

find(a, 27);
0
find(a, 421);
82
```



## MAXScript

'''Iterative'''

```maxscript
fn binarySearchIterative arr value =
(
    lower = 1
    upper = arr.count
    while lower <= upper do
    (
        mid = (lower + upper) / 2
        if arr[mid] > value then
        (
            upper = mid - 1
        )
        else if arr[mid] < value then
        (
            lower = mid + 1
        )
        else
        (
            return mid
        )
    )
    -1
)

arr = #(1, 3, 4, 5, 6, 7, 8, 9, 10)
result = binarySearchIterative arr 6
```

'''Recursive'''

```maxscript
fn binarySearchRecursive arr value lower upper =
(
    if lower == upper then
    (
        if arr[lower] == value then
        (
            return lower
        )
        else
        (
            return -1
        )
    )
    mid = (lower + upper) / 2
    if arr[mid] > value then
    (
        return binarySearchRecursive arr value lower (mid-1)
    )
    else if arr[mid] < value then
    (
        return binarySearchRecursive arr value (mid+1) upper
    )
    else
    (
        return mid
    )
)

arr = #(1, 3, 4, 5, 6, 7, 8, 9, 10)
result = binarySearchRecursive arr 6 1 arr.count
```



## MiniScript

'''Recursive:'''

```MiniScript
binarySearch = function(A, value, low, high)
    if high < low then return null
    mid = floor((low + high) / 2)
    if A[mid] > value then return binarySearch(A, value, low, mid-1)
    if A[mid] < value then return binarySearch(A, value, mid+1, high)
    return mid
end function
```


'''Iterative:'''

```MiniScript
binarySearch = function(A, value)
    low = 0
    high = A.len - 1
    while true
        if high < low then return null
        mid = floor((low + high) / 2)
        if A[mid] > value then
            high = mid - 1
        else if A[mid] < value then
            low = mid + 1
        else
            return mid
        end if
    end while
end function
```




## N/t/roff


{{works with|GNU TROFF|1.22.2}}
<lang>.de end
..
.de array
.	nr \\$1.c 0 1
.	de \\$1.push end
.		nr \\$1..\\\\n+[\\$1.c] \\\\$1
.	end
.	de \\$1.pushln end
.		if \\\\n(.$>0 .\\$1.push \\\\$1
.		if \\\\n(.$>1 \{ \
.			shift
.			\\$1.pushln \\\\$@
\}
.	end
..
.
.de binarysearch
.	nr min 1
.	nr max \\n[\\$1.c]
.	nr guess \\n[min]+\\n[max]/2
.	while !\\n[\\$1..\\n[guess]]=\\$2 \{ \
.		ie \\n[\\$1..\\n[guess]]<\\$2 .nr min \\n[guess]+1
.		el .nr max \\n[guess]-1
.
.		if \\n[min]>\\n[max] \{
.			nr guess 0
.			break
.		\}
.		nr guess \\n[min]+\\n[max]/2
.	\}
\\n[guess]
..
.array a
.a.pushln 1 4 9 16 25 36 49 64 81 100 121 144
.binarysearch a 100
.br
.ie \n[guess]=0 The item \fBdoesn't exist\fP.
.el The item \fBdoes exist\fP.

```



## Nim

'''Library'''

```nim
import algorithm

let s = @[2,3,4,5,6,7,8,9,10,12,14,16,18,20,22,25,27,30]
echo binarySearch(s, 10)
```


'''Iterative''' (from the standard library)

```nim
proc binarySearch[T](a: openArray[T], key: T): int =
  var b = len(a)
  while result < b:
    var mid = (result + b) div 2
    if a[mid] < key: result = mid + 1
    else: b = mid
  if result >= len(a) or a[result] != key: result = -1
```



## Niue

'''Library'''

```ocaml
1 2 3 4 5
3 bsearch . ( => 2 )
5 bsearch . ( => 0 )
'sam 'tom 'kenny ( must be sorted before calling bsearch )
sort
.s ( => kenny sam tom )
'sam bsearch . ( => 1 )
'tom bsearch . ( => 0 )
'kenny bsearch . ( => 2 )
'tony bsearch . ( => -1)
```



## Objeck

'''Iterative'''

```objeck
use Structure;

bundle Default {
  class BinarySearch {
    function : Main(args : String[]) ~ Nil {
      values := [-1, 3, 8, 13, 22];
      DoBinarySearch(values, 13)->PrintLine();
      DoBinarySearch(values, 7)->PrintLine();
    }

    function : native : DoBinarySearch(values : Int[], value : Int) ~ Int {
      low := 0;
      high := values->Size() - 1;

      while(low <= high) {
        mid := (low + high) / 2;

        if(values[mid] > value) {
          high := mid - 1;
        }
        else if(values[mid] < value) {
          low := mid + 1;
        }
        else {
          return mid;
        };
      };

      return -1;
    }
  }
}
```


=={{header|Objective-C}}==
'''Iterative'''

```objc>#import <Foundation/Foundation.h


@interface NSArray (BinarySearch)
// Requires all elements of this array to implement a -compare: method which
// returns a NSComparisonResult for comparison.
// Returns NSNotFound when not found
- (NSInteger) binarySearch:(id)key;
@end

@implementation NSArray (BinarySearch)
- (NSInteger) binarySearch:(id)key {
  NSInteger lo = 0;
  NSInteger hi = [self count] - 1;
  while (lo <= hi) {
    NSInteger mid = lo + (hi - lo) / 2;
    id midVal = self[mid];
    switch ([midVal compare:key]) {
    case NSOrderedAscending:
      lo = mid + 1;
      break;
    case NSOrderedDescending:
      hi = mid - 1;
      break;
    case NSOrderedSame:
      return mid;
    }
  }
  return NSNotFound;
}
@end

int main()
{
  @autoreleasepool {

    NSArray *a = @[@1, @3, @4, @5, @6, @7, @8, @9, @10];
    NSLog(@"6 is at position %d", [a binarySearch:@6]); // prints 4

  }
  return 0;
}
```

'''Recursive'''

```objc>#import <Foundation/Foundation.h


@interface NSArray (BinarySearchRecursive)
// Requires all elements of this array to implement a -compare: method which
// returns a NSComparisonResult for comparison.
// Returns NSNotFound when not found
- (NSInteger) binarySearch:(id)key inRange:(NSRange)range;
@end

@implementation NSArray (BinarySearchRecursive)
- (NSInteger) binarySearch:(id)key inRange:(NSRange)range {
  if (range.length == 0)
    return NSNotFound;
  NSInteger mid = range.location + range.length / 2;
  id midVal = self[mid];
  switch ([midVal compare:key]) {
  case NSOrderedAscending:
    return [self binarySearch:key
                      inRange:NSMakeRange(mid + 1, NSMaxRange(range) - (mid + 1))];
  case NSOrderedDescending:
    return [self binarySearch:key
                      inRange:NSMakeRange(range.location, mid - range.location)];
  default:
    return mid;
  }
}
@end

int main()
{
  @autoreleasepool {

    NSArray *a = @[@1, @3, @4, @5, @6, @7, @8, @9, @10];
    NSLog(@"6 is at position %d", [a binarySearch:@6]); // prints 4

  }
  return 0;
}
```

'''Library'''
{{works with|Mac OS X|10.6+}}

```objc>#import <Foundation/Foundation.h


int main()
{
  @autoreleasepool {

    NSArray *a = @[@1, @3, @4, @5, @6, @7, @8, @9, @10];
    NSLog(@"6 is at position %lu", [a indexOfObject:@6
                                      inSortedRange:NSMakeRange(0, [a count])
                                            options:0
                                    usingComparator:^(id x, id y){ return [x compare: y]; }]); // prints 4

  }
  return 0;
}
```

Using Core Foundation (part of Cocoa, all versions):

```objc>#import <Foundation/Foundation.h


CFComparisonResult myComparator(const void *x, const void *y, void *context) {
  return [(__bridge id)x compare:(__bridge id)y];
}

int main(int argc, const char *argv[]) {
  @autoreleasepool {

    NSArray *a = @[@1, @3, @4, @5, @6, @7, @8, @9, @10];
    NSLog(@"6 is at position %ld", CFArrayBSearchValues((__bridge CFArrayRef)a,
                                                        CFRangeMake(0, [a count]),
                                                        (__bridge const void *)@6,
                                                        myComparator,
                                                        NULL)); // prints 4

  }
  return 0;
}
```



## OCaml

'''Recursive'''

```ocaml
let rec binary_search a value low high =
  if high = low then
    if a.(low) = value then
      low
    else
      raise Not_found
  else let mid = (low + high) / 2 in
    if a.(mid) > value then
      binary_search a value low (mid - 1)
    else if a.(mid) < value then
      binary_search a value (mid + 1) high
    else
      mid
```

Output:

```txt

# let arr = [|1; 3; 4; 5; 6; 7; 8; 9; 10|];;
val arr : int array = [|1; 3; 4; 5; 6; 7; 8; 9; 10|]
# binary_search arr 6 0 (Array.length arr - 1);;
- : int = 4
# binary_search arr 2 0 (Array.length arr - 1);;
Exception: Not_found.

```

OCaml supports proper tail-recursion; so this is effectively the same as iteration.


## Octave

'''Recursive'''

```octave
function i = binsearch_r(array, val, low, high)
  if ( high < low )
    i = 0;
  else
    mid = floor((low + high) / 2);
    if ( array(mid) > val )
      i = binsearch_r(array, val, low, mid-1);
    elseif ( array(mid) < val )
      i = binsearch_r(array, val, mid+1, high);
    else
      i = mid;
    endif
  endif
endfunction
```

'''Iterative'''

```octave
function i = binsearch(array, value)
  low = 1;
  high = numel(array);
  i = 0;
  while ( low <= high )
    mid = floor((low + high)/2);
    if (array(mid) > value)
      high = mid - 1;
    elseif (array(mid) < value)
      low = mid + 1;
    else
      i = mid;
      return;
    endif
  endwhile
endfunction
```

'''Example of using'''

```octave
r = sort(discrete_rnd(10, [1:10], ones(10,1)/10));
disp(r);
binsearch_r(r, 5, 1, numel(r))
binsearch(r, 5)
```



## ooRexx


```ooRexx

data = .array~of(1, 3, 5, 7, 9, 11)
-- search keys with a number of edge cases
searchkeys = .array~of(0, 1, 4, 7, 11, 12)
say "recursive binary search"
loop key over searchkeys
    pos = recursiveBinarySearch(data, key)
    if pos == 0 then say "Key" key "not found"
    else say "Key" key "found at postion" pos
end
say
say "iterative binary search"
loop key over searchkeys
    pos = iterativeBinarySearch(data, key)
    if pos == 0 then say "Key" key "not found"
    else say "Key" key "found at postion" pos
end

::routine recursiveBinarySearch
  -- NB:  Rexx arrays are 1-based
  use strict arg data, value, low = 1, high = (data~items)

  -- make sure we don't go beyond the bounds
  high = min(high, data~items)
  -- zero indicates not found
  if high < low then return 0

  mid = (low + high) % 2
  if data[mid] > value then
      return recursiveBinarySearch(data, value, low, mid - 1)
  else if data[mid] < value then
      return recursiveBinarySearch(data, value, mid + 1, high)
  -- got it!
  return mid

::routine iterativeBinarySearch
  -- NB:  Rexx arrays are 1-based
  use strict arg data, value, low = 1, high = (data~items)

  -- make sure we don't go beyond the bounds
  high = min(high, data~items)
  -- zero indicates not found
  if high < low then return 0
  loop while low <= high
      mid = (low + high) % 2
      if data[mid] > value then
          high = mid - 1
      else if data[mid] < value then
          low = mid + 1
      else
          return mid
  end
  return 0

```

Output:

```txt

recursive binary search
Key 0 not found
Key 1 found at postion 1
Key 4 not found
Key 7 found at postion 4
Key 11 found at postion 6
Key 12 not found

iterative binary search
Key 0 not found
Key 1 found at postion 1
Key 4 not found
Key 7 found at postion 4
Key 11 found at postion 6
Key 12 not found

```



## Oz

'''Recursive'''

```oz
declare
  fun {BinarySearch Arr Val}
     fun {Search Low High}
        if Low > High then nil
        else
           Mid = (Low+High) div 2
        in
           if Val < Arr.Mid then {Search Low Mid-1}
           elseif Val > Arr.Mid then {Search Mid+1 High}
           else [Mid]
           end
        end
     end
  in
     {Search {Array.low Arr} {Array.high Arr}}
  end

  A = {Tuple.toArray unit(2 3 5 6 8)}
in
  {System.printInfo "searching 4: "} {Show {BinarySearch A 4}}
  {System.printInfo "searching 8: "} {Show {BinarySearch A 8}}
```

'''Iterative'''

```oz
declare
  fun {BinarySearch Arr Val}
     Low = {NewCell {Array.low Arr}}
     High = {NewCell {Array.high Arr}}
  in
     for while:@Low =< @High  return:Return  default:nil do
        Mid = (@Low + @High) div 2
     in
        if Val < Arr.Mid then High := Mid-1
        elseif Val > Arr.Mid then Low := Mid+1
        else {Return [Mid]}
        end
     end
  end

  A = {Tuple.toArray unit(2 3 5 6 8)}
in
  {System.printInfo "searching 4: "} {Show {BinarySearch A 4}}
  {System.printInfo "searching 8: "} {Show {BinarySearch A 8}}
```



## PARI/GP

Note that, despite the name, <code>setsearch</code> works on sorted vectors as well as sets.

```parigp
setsearch(s, n)
```


The following is another implementation that takes a more manual approach.  Instead of using an intrinsic function, a general binary search algorithm is implemented using the language alone.

{{trans|N/t/roff}}


```parigp
binarysearch(v, x) = {
    local(
        minm = 1,
        maxm = length(v),
        guess = floor(maxm/2+minm/2)
    );

    while(v[guess] != x,
        if(v[guess] < x, minm = guess + 1, maxm = guess - 1);
        if(minm > maxm,
            guess = 0;
            break
        );
        guess = floor(maxm/2+minm/2)
    );

    return(guess);
}

idx = binarysearch([1,4,9,16,25,36,49,64,81,100,121,144], 121);
if(idx, \
    print("Item exists on index ", idx), \
    print("Item does not exist anywhere.") \
)
```



## Pascal

'''Iterative'''

```pascal
function binary_search(element: real; list: array of real): integer;
var
    l, m, h: integer;
begin
    l := Low(list);
    h := High(list);
    binary_search := -1;
    while l <= h do
    begin
        m := (l + h) div 2;
        if list[m] > element then
        begin
            h := m - 1;
        end
        else if list[m] < element then
        begin
            l := m + 1;
        end
        else
        begin
            binary_search := m;
            break;
        end;
    end;
end;
```

Usage:

```pascal
var
    list: array[0 .. 9] of real;
// ...
indexof := binary_search(123, list);
```



## Perl

'''Iterative'''

```perl
sub binary_search {
    my ($array_ref, $value, $left, $right) = @_;
    while ($left <= $right) {
        my $middle = int(($right + $left) >> 1);
        if ($value == $array_ref->[$middle]) {
            return $middle;
        }
        elsif ($value < $array_ref->[$middle]) {
            $right = $middle - 1;
        }
        else {
            $left = $middle + 1;
        }
    }
    return -1;
}
```

'''Recursive'''

```perl
sub binary_search {
    my ($array_ref, $value, $left, $right) = @_;
    return -1 if ($right < $left);
    my $middle = int(($right + $left) >> 1);
    if ($value == $array_ref->[$middle]) {
        return $middle;
    }
    elsif ($value < $array_ref->[$middle]) {
        binary_search($array_ref, $value, $left, $middle - 1);
    }
    else {
        binary_search($array_ref, $value, $middle + 1, $right);
    }
}
```



## Perl 6

With either of the below implementations of <code>binary_search</code>, one could write a function to search any object that does <code>Positional</code> this way:

```perl6
sub search (@a, $x --> Int) {
    binary_search { $x cmp @a[$^i] }, 0, @a.end
}
```

'''Iterative'''
{{works with|Rakudo|2015.12}}

```perl6
sub binary_search (&p, Int $lo is copy, Int $hi is copy --> Int) {
    until $lo > $hi {
        my Int $mid = ($lo + $hi) div 2;
        given p $mid {
            when -1 { $hi = $mid - 1; }
            when  1 { $lo = $mid + 1; }
            default { return $mid;    }
        }
    }
    fail;
}
```

'''Recursive'''
{{trans|Haskell}}
{{works with|Rakudo|2015.12}}

```perl6
sub binary_search (&p, Int $lo, Int $hi --> Int) {
    $lo <= $hi or fail;
    my Int $mid = ($lo + $hi) div 2;
    given p $mid {
        when -1 { binary_search &p, $lo,      $mid - 1 }
        when  1 { binary_search &p, $mid + 1, $hi      }
        default { $mid                                 }
    }
}
```



## Phix

Standard autoinclude builtin/bsearch.e, reproduced here

```Phix
global function binary_search(object needle, sequence haystack)
integer lo = 1,
        hi = length(haystack),
        mid = lo,
        c = 0

    while lo<=hi do
        mid = floor((lo+hi)/2)
        c = compare(needle, haystack[mid])
        if c<0 then
            hi = mid-1
        elsif c>0 then
            lo = mid+1
        else
            return mid  -- found!
        end if
    end while
    mid += c>0
    return -mid         -- where it would go, if inserted now
end function
```

The low + (high-low)/2 trick is not needed, since interim integer results are accurate to 53 bits (on 32 bit, 64 bits on 64 bit) on Phix.

Returns a positive index if found, otherwise the negative index where it would go if inserted now. Example use

```Phix
?binary_search(0,{1,3,5})   -- -1
?binary_search(1,{1,3,5})   --  1
?binary_search(2,{1,3,5})   -- -2
?binary_search(3,{1,3,5})   --  2
?binary_search(4,{1,3,5})   -- -3
?binary_search(5,{1,3,5})   --  3
?binary_search(6,{1,3,5})   -- -4
```



## PHP

'''Iterative'''

```php
function binary_search( $array, $secret, $start, $end )
{
        do
        {
                $guess = (int)($start + ( ( $end - $start ) / 2 ));

                if ( $array[$guess] > $secret )
                        $end = $guess;

                if ( $array[$guess] < $secret )
                        $start = $guess;

                if ( $end < $start)
                        return -1;

        } while ( $array[$guess] != $secret );

        return $guess;
}
```

'''Recursive'''

```php
function binary_search( $array, $secret, $start, $end )
{
        $guess = (int)($start + ( ( $end - $start ) / 2 ));

        if ( $end < $start)
                return -1;

        if ( $array[$guess] > $secret )
                return (binary_search( $array, $secret, $start, $guess ));

        if ( $array[$guess] < $secret )
                return (binary_search( $array, $secret, $guess, $end ) );

        return $guess;
}
```



## PicoLisp

'''Recursive'''

```PicoLisp
(de recursiveSearch (Val Lst Len)
   (unless (=0 Len)
      (let (N (inc (/ Len 2))  L (nth Lst N))
         (cond
            ((= Val (car L)) Val)
            ((> Val (car L))
               (recursiveSearch Val (cdr L) (- Len N)) )
            (T (recursiveSearch Val Lst (dec N))) ) ) ) )
```

Output:

```txt
: (recursiveSearch 5 (2 3 5 8 "abc" "klm" "xyz" (7) (a b)) 9)
-> 5
: (recursiveSearch '(a b) (2 3 5 8 "abc" "klm" "xyz" (7) (a b)) 9)
-> (a b)
: (recursiveSearch (9) (2 3 5 8 "abc" "klm" "xyz" (7) (a b)) 9)
-> NIL
```

'''Iterative'''

```PicoLisp
(de iterativeSearch (Val Lst Len)
   (use (N L)
      (loop
         (T (=0 Len))
         (setq
            N (inc (/ Len 2))
            L (nth Lst N) )
         (T (= Val (car L)) Val)
         (if (> Val (car L))
            (setq Lst (cdr L)  Len (- Len N))
            (setq Len (dec N)) ) ) ) )
```

Output:

```txt
: (iterativeSearch 5 (2 3 5 8 "abc" "klm" "xyz" (7) (a b)) 9)
-> 5
: (iterativeSearch '(a b) (2 3 5 8 "abc" "klm" "xyz" (7) (a b)) 9)
-> (a b)
: (iterativeSearch (9) (2 3 5 8 "abc" "klm" "xyz" (7) (a b)) 9)
-> NIL
```



## PL/I


```PL/I
/* A binary search of list A for element M */
search: procedure (A, M) returns (fixed binary);
   declare (A(*), M) fixed binary;
   declare (l, r, mid) fixed binary;

   l = lbound(a,1)-1; r = hbound(A,1)+1;
   do while (l <= r);
      mid = (l+r)/2;
      if A(mid) = M then return (mid);
      if A(mid) < M then
         L = mid+1;
      else
         R = mid-1;
   end;
   return (lbound(A,1)-1);
end search;
```



## Pop11

'''Iterative'''

```pop11
define BinarySearch(A, value);
    lvars low = 1, high = length(A), mid;
    while low <= high do
        (low + high) div 2 -> mid;
        if A(mid) > value then
            mid - 1 -> high;
        elseif A(mid) < value then
            mid + 1 -> low;
        else
            return(mid);
        endif;
    endwhile;
    return("not_found");
enddefine;

/* Tests */
lvars A = {2 3 5 6 8};

BinarySearch(A, 4) =>
BinarySearch(A, 5) =>
BinarySearch(A, 8) =>
```

'''Recursive'''

```pop11
define BinarySearch(A, value);
    define do_it(low, high);
        if high < low then
            return("not_found");
        endif;
        (low + high) div 2 -> mid;
        if A(mid) > value then
            do_it(low, mid-1);
        elseif A(mid) < value then
            do_it(mid+1, high);
        else
            mid;
        endif;
    enddefine;
    do_it(1, length(A));
enddefine;
```



## PowerShell


```PowerShell

function BinarySearch-Iterative ([int[]]$Array, [int]$Value)
{
    [int]$low = 0
    [int]$high = $Array.Count - 1

    while ($low -le $high)
    {
        [int]$mid = ($low + $high) / 2

        if ($Array[$mid] -gt $Value)
        {
            $high = $mid - 1
        }
        elseif ($Array[$mid] -lt $Value)
        {
            $low = $mid + 1
        }
        else
        {
            return $mid
        }
    }

    return -1
}

function BinarySearch-Recursive ([int[]]$Array, [int]$Value, [int]$Low = 0, [int]$High = $Array.Count)
{
    if ($High -lt $Low)
    {
        return -1
    }

    [int]$mid = ($Low + $High) / 2

    if ($Array[$mid] -gt $Value)
    {
        return BinarySearch $Array $Value $Low ($mid - 1)
    }
    elseif ($Array[$mid] -lt $Value)
    {
        return BinarySearch $Array $Value ($mid + 1) $High
    }
    else
    {
        return $mid
    }
}

function Show-SearchResult ([int[]]$Array, [int]$Search, [ValidateSet("Iterative", "Recursive")][string]$Function)
{
    switch ($Function)
    {
        "Iterative" {$index = BinarySearch-Iterative -Array $Array -Value $Search}
        "Recursive" {$index = BinarySearch-Recursive -Array $Array -Value $Search}
    }

    if ($index -ge 0)
    {
        Write-Host ("Using BinarySearch-{0}: {1} is at index {2}" -f $Function, $numbers[$index], $index)
    }
    else
    {
        Write-Host ("Using BinarySearch-{0}: {1} not found" -f $Function, $Search) -ForegroundColor Red
    }
}

```


```PowerShell

Show-SearchResult -Array 10, 28, 41, 46, 58, 74, 76, 86, 89, 98 -Search 41 -Function Iterative
Show-SearchResult -Array 10, 28, 41, 46, 58, 74, 76, 86, 89, 98 -Search 99 -Function Iterative
Show-SearchResult -Array 10, 28, 41, 46, 58, 74, 76, 86, 89, 98 -Search 86 -Function Recursive
Show-SearchResult -Array 10, 28, 41, 46, 58, 74, 76, 86, 89, 98 -Search 11 -Function Recursive

```

{{Out}}

```txt

Using BinarySearch-Iterative: 41 is at index 2
Using BinarySearch-Iterative: 99 not found
Using BinarySearch-Recursive: 86 is at index 7
Using BinarySearch-Recursive: 11 not found

```



## Prolog

Tested with Gnu-Prolog.

```Prolog
bin_search(Elt,List,Result):-
  length(List,N), bin_search_inner(Elt,List,1,N,Result).

bin_search_inner(Elt,List,J,J,J):-
  nth(J,List,Elt).
bin_search_inner(Elt,List,Begin,End,Mid):-
  Begin < End,
  Mid is (Begin+End) div 2,
  nth(Mid,List,Elt).
bin_search_inner(Elt,List,Begin,End,Result):-
  Begin < End,
  Mid is (Begin+End) div 2,
  nth(Mid,List,MidElt),
  MidElt < Elt,
  NewBegin is Mid+1,
  bin_search_inner(Elt,List,NewBegin,End,Result).
bin_search_inner(Elt,List,Begin,End,Result):-
  Begin < End,
  Mid is (Begin+End) div 2,
  nth(Mid,List,MidElt),
  MidElt > Elt,
  NewEnd is Mid-1,
  bin_search_inner(Elt,List,Begin,NewEnd,Result).
```


{{out|Output examples}}

```txt
 ?- bin_search(4,[1,2,4,8,16,32,64,128],Result).
Result = 3.

?- bin_search(5,[1,2,4,8],Result).
Result = -1.
```



## PureBasic

Both recursive and iterative procedures are included and called in the code below.

```PureBasic
#Recursive = 0 ;recursive binary search method
#Iterative = 1 ;iterative binary search method
#NotFound = -1 ;search result if item not found

;Recursive
Procedure  R_BinarySearch(Array a(1), value, low, high)
  Protected mid
  If high < low
    ProcedureReturn #NotFound
  EndIf

  mid = (low + high) / 2
  If a(mid) > value
    ProcedureReturn R_BinarySearch(a(), value, low, mid - 1)
  ElseIf a(mid) < value
    ProcedureReturn R_BinarySearch(a(), value, mid + 1, high)
  Else
    ProcedureReturn mid
  EndIf
EndProcedure

;Iterative
Procedure I_BinarySearch(Array a(1), value, low, high)
  Protected mid
  While low <= high
    mid = (low + high) / 2
    If a(mid) > value
      high = mid - 1
    ElseIf a(mid) < value
      low = mid + 1
    Else
      ProcedureReturn mid
    EndIf
  Wend

  ProcedureReturn #NotFound
EndProcedure

Procedure search (Array a(1), value, method)
  Protected idx

  Select method
    Case #Iterative
      idx = I_BinarySearch(a(), value, 0, ArraySize(a()))
    Default
      idx = R_BinarySearch(a(), value, 0, ArraySize(a()))
  EndSelect

  Print("  Value " + Str(Value))
  If idx < 0
    PrintN(" not found")
  Else
    PrintN(" found at index " + Str(idx))
  EndIf
EndProcedure


#NumElements = 9 ;zero based count
Dim test(#NumElements)

DataSection
  Data.i 2, 3, 5, 6, 8, 10, 11, 15, 19, 20
EndDataSection

;fill the test array
For i = 0 To #NumElements
  Read test(i)
Next


If OpenConsole()

  PrintN("Recursive search:")
  search(test(), 4, #Recursive)
  search(test(), 8, #Recursive)
  search(test(), 20, #Recursive)

  PrintN("")
  PrintN("Iterative search:")
  search(test(), 4, #Iterative)
  search(test(), 8, #Iterative)
  search(test(), 20, #Iterative)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt

Recursive search:
  Value 4 not found
  Value 8 found at index 4
  Value 20 found at index 9

Iterative search:
  Value 4 not found
  Value 8 found at index 4
  Value 20 found at index 9

```



## Python


### Python: Iterative


```python
def binary_search(l, value):
    low = 0
    high = len(l)-1
    while low <= high:
        mid = (low+high)//2
        if l[mid] > value: high = mid-1
        elif l[mid] < value: low = mid+1
        else: return mid
    return -1
```


We can also generalize this kind of binary search from direct matches to searches using a custom comparator function.
In addition to a search for a particular word in an AZ-sorted list, for example, we could also perform a binary search for a word of a given '''length''' (in a word-list sorted by rising length), or for a particular value of any other comparable property of items in a suitably sorted list:


```python
# findIndexBinary :: (a -> Ordering) -> [a] -> Maybe Int
def findIndexBinary(p):
    def isFound(bounds):
        (lo, hi) = bounds
        return lo > hi or 0 == hi

    def half(xs):
        def choice(lh):
            (lo, hi) = lh
            mid = (lo + hi) // 2
            cmpr = p(xs[mid])
            return (lo, mid - 1) if cmpr < 0 else (
                (1 + mid, hi) if cmpr > 0 else (
                    mid, 0
                )
            )
        return lambda bounds: choice(bounds)

    def go(xs):
        (lo, hi) = until(isFound)(
            half(xs)
        )((0, len(xs) - 1)) if xs else None
        return None if 0 != hi else lo

    return lambda xs: go(xs)


# COMPARISON CONSTRUCTORS ---------------------------------

# compare :: a -> a -> Ordering
def compare(a):
    '''Simple comparison of x and y -> LT|EQ|GT'''
    return lambda b: -1 if a < b else (1 if a > b else 0)


# byKV :: (a -> b) -> a -> a -> Ordering
def byKV(f):
    '''Property accessor function -> target value -> x -> LT|EQ|GT'''
    def go(v, x):
        fx = f(x)
        return -1 if v < fx else 1 if v > fx else 0
    return lambda v: lambda x: go(v, x)


# TESTS ---------------------------------------------------
def main():

    # BINARY SEARCH FOR WORD IN AZ-SORTED LIST

    mb1 = findIndexBinary(compare('iota'))(
        # Sorted AZ
        ['alpha', 'beta', 'delta', 'epsilon', 'eta', 'gamma', 'iota',
         'kappa', 'lambda', 'mu', 'theta', 'zeta']
    )

    print (
        'Not found' if None is mb1 else (
            'Word found at index ' + str(mb1)
        )
    )

    # BINARY SEARCH FOR WORD OF GIVEN LENGTH (IN WORD-LENGTH SORTED LIST)

    mb2 = findIndexBinary(byKV(len)(7))(
        # Sorted by rising length
        ['mu', 'eta', 'beta', 'iota', 'zeta', 'alpha', 'delta', 'gamma',
         'kappa', 'theta', 'lambda', 'epsilon']
    )

    print (
        'Not found' if None is mb2 else (
            'Word of given length found at index ' + str(mb2)
        )
    )


# GENERIC -------------------------------------------------

# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


if __name__ == '__main__':
    main()

```

{{Out}}

```txt
Word found at index 6
Word of given length found at index 11
```



### Python: Recursive


```python
def binary_search(l, value, low = 0, high = -1):
    if not l: return -1
    if(high == -1): high = len(l)-1
    if low >= high:
        if l[low] == value: return low
        else: return -1
    mid = (low+high)//2
    if l[mid] > value: return binary_search(l, value, low, mid-1)
    elif l[mid] < value: return binary_search(l, value, mid+1, high)
    else: return mid
```


Generalizing again with a custom comparator function (see preamble to second iterative version above).

This time using the recursive definition:


```python
# findIndexBinary_ :: (a -> Ordering) -> [a] -> Maybe Int
def findIndexBinary_(p):
    def go(xs):
        def bin(lo, hi):
            if hi < lo:
                return None
            else:
                mid = (lo + hi) // 2
                cmpr = p(xs[mid])
                return bin(lo, mid - 1) if -1 == cmpr else (
                    bin(mid + 1, hi) if 1 == cmpr else (
                        mid
                    )
                )
        n = len(xs)
        return bin(0, n - 1) if 0 < n else None
    return lambda xs: go(xs)


# COMPARISON CONSTRUCTORS ---------------------------------

# compare :: a -> a -> Ordering
def compare(a):
    '''Simple comparison of x and y -> LT|EQ|GT'''
    return lambda b: -1 if a < b else (1 if a > b else 0)


# byKV :: (a -> b) -> a -> a -> Ordering
def byKV(f):
    '''Property accessor function -> target value -> x -> LT|EQ|GT'''
    def go(v, x):
        fx = f(x)
        return -1 if v < fx else 1 if v > fx else 0
    return lambda v: lambda x: go(v, x)


# TESTS ---------------------------------------------------


if __name__ == '__main__':

    # BINARY SEARCH FOR WORD IN AZ-SORTED LIST

    mb1 = findIndexBinary_(compare('mu'))(
        # Sorted AZ
        ['alpha', 'beta', 'delta', 'epsilon', 'eta', 'gamma', 'iota',
         'kappa', 'lambda', 'mu', 'theta', 'zeta']
    )

    print (
        'Not found' if None is mb1 else (
            'Word found at index ' + str(mb1)
        )
    )

    # BINARY SEARCH FOR WORD OF GIVEN LENGTH (IN WORD-LENGTH SORTED LIST)

    mb2 = findIndexBinary_(byKV(len)(6))(
        # Sorted by rising length
        ['mu', 'eta', 'beta', 'iota', 'zeta', 'alpha', 'delta', 'gamma',
         'kappa', 'theta', 'lambda', 'epsilon']
    )

    print (
        'Not found' if None is mb2 else (
            'Word of given length found at index ' + str(mb2)
        )
    )
```

{{Out}}

```txt
Word found at index 9
Word of given length found at index 10
```



### Python: Library


Python's <code>bisect</code> module provides binary search functions

```python
index = bisect.bisect_left(list, item) # leftmost insertion point
index = bisect.bisect_right(list, item) # rightmost insertion point
index = bisect.bisect(list, item) # same as bisect_right

# same as above but actually insert the item into the list at the given place:
bisect.insort_left(list, item)
bisect.insort_right(list, item)
bisect.insort(list, item)
```



### =Python: Alternate=

Complete binary search function with python's <code>bisect</code> module:


```python
from bisect import bisect_left

def binary_search(a, x, lo=0, hi=None):   # can't use a to specify default for hi
    hi = hi if hi is not None else len(a) # hi defaults to len(a)
    pos = bisect_left(a,x,lo,hi)          # find insertion position
    return (pos if pos != hi and a[pos] == x else -1) # don't walk off the end
```



### Python: Approximate binary search

Returns the nearest item of list l to value.

```python
def binary_search(l, value):
    low = 0
    high = len(l)-1
    while low + 1 < high:
        mid = (low+high)//2
        if l[mid] > value:
            high = mid
        elif l[mid] < value:
            low = mid
        else:
            return mid
    return high if abs(l[high] - value) < abs(l[low] - value) else low
```



## R

'''Recursive'''

```R
BinSearch <- function(A, value, low, high) {
  if ( high < low ) {
    return(NULL)
  } else {
    mid <- floor((low + high) / 2)
    if ( A[mid] > value )
      BinSearch(A, value, low, mid-1)
    else if ( A[mid] < value )
      BinSearch(A, value, mid+1, high)
    else
      mid
  }
}
```

'''Iterative'''

```R
IterBinSearch <- function(A, value) {
  low = 1
  high = length(A)
  i = 0
  while ( low <= high ) {
    mid <- floor((low + high)/2)
    if ( A[mid] > value )
      high <- mid - 1
    else if ( A[mid] < value )
      low <- mid + 1
    else
      return(mid)
  }
  NULL
}
```

'''Example'''

```R
a <- 1:100
IterBinSearch(a, 50)
BinSearch(a, 50, 1, length(a)) # output 50
IterBinSearch(a, 101) # outputs NULL
```



## Racket


```racket

#lang racket
(define (binary-search x v)
  ; loop : index index -> index or #f
  ;   return i s.t. l<=i<h and v[i]=x
  (define (loop l h)
    (cond [(>= l h) #f]
          [else (define m (quotient (+ l h) 2))
                (define y (vector-ref v m))
                (cond
                  [(> y x) (loop l (- m 1))]
                  [(< y x) (loop (+ m 1) h)]
                  [else m])]))
  (loop 0 (vector-length v)))

```

Examples:

```txt

(binary-search 6 #(1 3 4 5 6 7 8 9 10))  ; gives 4
(binary-search 6 #(1 3 4 5 7 8 9 10))    ; gives #f

```



## REXX


### recursive version

Incidentally, REXX doesn't care if the values in the list are integers (or even numbers), as long as they're in order.


(includes the extra credit)

```rexx
/*REXX program finds a  value  in a  list of integers  using an iterative binary search.*/
@=  3   7  13  19  23  31  43  47  61  73  83  89 103 109 113 131 139 151 167 181,
  193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401 409 421 433,
  443 449 463 467 491 503 509 523 547 571 577 601 619 643 647 661 677 683 691 709,
  743 761 773 797 811 823 829 839 859 863 883 887 911 919 941 953 971 983 1013
                                                 /* [↑]  a list of some low weak primes.*/
parse arg ? .                                    /*get a  #  that's specified on the CL.*/
if ?==''  then do;    say;       say '***error***  no argument specified.';       say
                      exit                       /*stick a fork in it,  we're all done. */
               end
 low= 1
high= words(@)
 avg= (word(@, 1) + word(@, high)) / 2
 loc= binarySearch(low, high)

if loc==-1  then do;  say  ?  " wasn't found in the list."
                      exit                       /*stick a fork in it,  we're all done. */
                 end
            else say  ?  ' is in the list, its index is: '   loc
say
say  'arithmetic mean of the '   high   " values is: "       avg
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
binarySearch:  procedure expose @ ?;     parse arg low,high
               if high<low  then return -1       /*the item wasn't found in the @ list. */
               mid= (low + high) % 2             /*calculate the midpoint in the list.  */
               y= word(@, mid)                   /*obtain the midpoint value in the list*/
               if ?=y       then return  mid
               if y>?       then return  binarySearch(low,   mid-1)
                                 return  binarySearch(mid+1, high)
```

{{out|output|text=  when using the input of:     <tt> 499.1 </tt>}}

```txt

499.1  wasn't found in the list.

```

{{out|output|text=  when using the input of:     <tt> 499 </tt>}}

```txt

arithmetic mean of the  74  values is:  510

499  is in the list, its index is:  41

```



### iterative version

(includes the extra credit)

```rexx
/*REXX program finds a  value  in a  list of integers  using an iterative binary search.*/
@=  3   7  13  19  23  31  43  47  61  73  83  89 103 109 113 131 139 151 167 181,
  193 199 229 233 241 271 283 293 313 317 337 349 353 359 383 389 401 409 421 433,
  443 449 463 467 491 503 509 523 547 571 577 601 619 643 647 661 677 683 691 709,
  743 761 773 797 811 823 829 839 859 863 883 887 911 919 941 953 971 983 1013
                                                 /* [↑]  a list of some low weak primes.*/
parse arg ? .                                    /*get a  #  that's specified on the CL.*/
if ?==''  then do;    say;       say '***error***  no argument specified.';       say
                      exit 13
               end
 low= 1
high= words(@)
say  'arithmetic mean of the '   high    " values is: "   (word(@, 1) + word(@, high)) / 2
say
               do  while  low<=high;     mid= (low + high) % 2;            y= word(@, mid)

               if ?=y  then do;  say ?   ' is in the list, its index is: '    mid
                                 exit            /*stick a fork in it,  we're all done. */
                            end

               if y>?  then high= mid - 1        /*too high?                            */
                       else  low= mid + 1        /*too low?                             */
               end   /*while*/

say  ?   " wasn't found in the list."            /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the input of:     <tt> -314 </tt>}}

```txt

arithmetic mean of the  79  values is:  508

-314  wasn't found in the list.

```

{{out|output|text=  when using the input of:     <tt> 619 </tt>}}

```txt

arithmetic mean of the  79  values is:  508

619  is in the list, its index is:  53

```



## Ring


```ring

decimals(0)
array = [7, 14, 21, 28, 35, 42, 49, 56, 63, 70]

find= 42
index = where(array, find, 0, len(array))
if index >= 0
   see "the value " + find+ " was found at index " + index
else
   see "the value " + find + " was not found"
ok

func where a, s, b, t
     h = 2
     while h<(t-b) h *= 2 end
     h /= 2
     while h != 0
           if (b+h)<=t if s>=a[b+h] b += h ok ok
           h /= 2
     end
     if s=a[b] return b-1 else return -1 ok

```

Output:

```txt

the value 42 was found at index 6

```



## Ruby

'''Recursive'''

```ruby
class Array
  def binary_search(val, low=0, high=(length - 1))
    return nil if high < low
    mid = (low + high) >> 1
    case val <=> self[mid]
      when -1
        binary_search(val, low, mid - 1)
      when 1
        binary_search(val, mid + 1, high)
      else mid
    end
  end
end

ary = [0,1,4,5,6,7,8,9,12,26,45,67,78,90,98,123,211,234,456,769,865,2345,3215,14345,24324]

[0,42,45,24324,99999].each do |val|
  i = ary.binary_search(val)
  if i
    puts "found #{val} at index #{i}: #{ary[i]}"
  else
    puts "#{val} not found in array"
  end
end
```

'''Iterative'''

```ruby
class Array
  def binary_search_iterative(val)
    low, high = 0, length - 1
    while low <= high
      mid = (low + high) >> 1
      case val <=> self[mid]
        when 1
          low = mid + 1
        when -1
          high = mid - 1
        else
          return mid
      end
    end
    nil
  end
end

ary = [0,1,4,5,6,7,8,9,12,26,45,67,78,90,98,123,211,234,456,769,865,2345,3215,14345,24324]

[0,42,45,24324,99999].each do |val|
  i = ary.binary_search_iterative(val)
  if i
    puts "found #{val} at index #{i}: #{ary[i]}"
  else
    puts "#{val} not found in array"
  end
end
```

{{out}}

```txt

found 0 at index 0: 0
42 not found in array
found 45 at index 10: 45
found 24324 at index 24: 24324
99999 not found in array

```

'''Built in'''
Since Ruby 2.0, arrays ship with a binary search method "bsearch":

```ruby
haystack = [0,1,4,5,6,7,8,9,12,26,45,67,78,90,98,123,211,234,456,769,865,2345,3215,14345,24324]
needles = [0,42,45,24324,99999]

needles.select{|needle| haystack.bsearch{|hay| needle <=> hay} } # => [0, 45, 24324]

```
Which is 60% faster than "needles & haystack".


## Run BASIC

'''Recursive'''

```runbasic
dim theArray(100)
global theArray
for i = 1 to 100
  theArray(i) = i
next i

print binarySearch(80,30,90)

FUNCTION binarySearch(val, lo, hi)
  IF hi < lo THEN
    binarySearch = 0
  ELSE
    middle = (hi + lo) / 2
    if val < theArray(middle) then binarySearch = binarySearch(val, lo, middle-1)
    if val > theArray(middle) then binarySearch = binarySearch(val, middle+1, hi)
    if val = theArray(middle) then binarySearch = middle
  END IF
END FUNCTION
```



## Rust

'''Standard Library'''

```rust
let arr = ["a", "bc", "def", "ghij"];
arr.binary_search(&"a"); // Search lexicographically
arr.binary_search_by(|e| e.len().cmp(&1)); // Search by length
```

'''Iterative'''

```rust
use std::cmp::Ordering::*;

//
// This implementation fails the following test
//
#[test]
fn binary_search_returns_none() {
    let data = vec![1, 2, 3];
    let target = 10;
    assert_eq!(binary_search(&data, &target), None);
}

// Broken, didn't manage to debug sorry.
fn binary_search<T: Ord>(arr: &[T], elem: &T) -> Option<usize>
{
    let mut size = arr.len();
    let mut base = 0;

    while size > 0 {
        size /= 2;
        let mid = base + size;
        base = match arr[mid].cmp(elem) {
            Less    => mid,
            Greater => base,
            Equal   => return Some(mid)
        };
    }

    None
}

```


'''Iterative'''

```rust

use std::cmp::Ordering::*;

pub fn binary_search<T: Ord>(data: &[T], target: &T) -> Option<usize> {
    let mut high = data.len();
    let mut low = 0;
    let mut mid = high / 2;

    while low < high {
        match target.cmp(&data[mid]) {
            Less => high = mid - 1,
            Greater => low = mid + 1,
            Equal => return Some(mid),
        };
        mid = (high + low) / 2;
    }
    None
}

```



## Scala

'''Recursive'''

```scala
def binarySearch[A <% Ordered[A]](a: IndexedSeq[A], v: A) = {
  def recurse(low: Int, high: Int): Option[Int] = (low + high) / 2 match {
    case _ if high < low => None
    case mid if a(mid) > v => recurse(low, mid - 1)
    case mid if a(mid) < v => recurse(mid + 1, high)
    case mid => Some(mid)
  }
  recurse(0, a.size - 1)
}
```

'''Iterative'''

```scala
def binarySearch[T](xs: Seq[T], x: T)(implicit ordering: Ordering[T]): Option[Int] = {
    var low: Int = 0
    var high: Int = xs.size - 1

    while (low <= high)
      low + high >>> 1 match {
        case guess if ordering.gt(xs(guess), x) => high = guess - 1 //too high
        case guess if ordering.lt(xs(guess), x) => low = guess + 1 // too low
        case guess => return Some(guess) //found it
      }
    None //not found
  }
```

'''Test'''

```scala
def testBinarySearch(n: Int) = {
  val odds = 1 to n by 2
  val result = (0 to n).flatMap(binarySearch(odds, _))
  assert(result == (0 until odds.size))
  println(s"$odds are odd natural numbers")
  for (it <- result)
    println(s"$it is ordinal of ${odds(it)}")
}

def main() = testBinarySearch(12)
```

Output:

```txt
Range(1, 3, 5, 7, 9, 11) are odd natural numbers
0 is ordinal of 1
1 is ordinal of 3
2 is ordinal of 5
3 is ordinal of 7
4 is ordinal of 9
5 is ordinal of 11
```



## Scheme

'''Recursive'''

```scheme
(define (binary-search value vector)
  (let helper ((low 0)
               (high (- (vector-length vector) 1)))
    (if (< high low)
        #f
        (let ((middle (quotient (+ low high) 2)))
          (cond ((> (vector-ref vector middle) value)
                 (helper low (- middle 1)))
                ((< (vector-ref vector middle) value)
                 (helper (+ middle 1) high))
                (else middle))))))
```

Example:

```txt

> (binary-search 6 '#(1 3 4 5 6 7 8 9 10))
4
> (binary-search 2 '#(1 3 4 5 6 7 8 9 10))
#f

```

The calls to helper are in tail position, so since Scheme implementations
support proper tail-recursion the computation proces is iterative.


## Seed7

'''Iterative'''

```seed7
const func integer: binarySearchIterative (in array elemType: arr, in elemType: aKey) is func
  result
    var integer: result is 0;
  local
    var integer: low is 1;
    var integer: high is 0;
    var integer: middle is 0;
  begin
    high := length(arr);
    while result = 0 and low <= high do
      middle := low + (high - low) div 2;
      if aKey < arr[middle] then
        high := pred(middle);
      elsif aKey > arr[middle] then
        low := succ(middle);
      else
        result := middle;
      end if;
    end while;
  end func;
```

'''Recursive'''

```seed7
const func integer: binarySearch (in array elemType: arr, in elemType: aKey, in integer: low, in integer: high) is func
  result
    var integer: result is 0;
  begin
    if low <= high then
      result := (low + high) div 2;
      if aKey < arr[result] then
        result := binarySearch(arr, aKey, low, pred(result)); # search left
      elsif aKey > arr[result] then
        result := binarySearch(arr, aKey, succ(result), high); # search right
      end if;
    end if;
  end func;

const func integer: binarySearchRecursive (in array elemType: arr, in elemType: aKey) is
  return binarySearch(arr, aKey, 1, length(arr));
```



## SequenceL

'''Recursive'''

```sequencel
binarySearch(A(1), value(0), low(0), high(0)) :=
	let
		mid := low + (high - low) / 2;
	in
			-1 when high < low //Not Found
		else
			binarySearch(A, value, low, mid - 1) when A[mid] > value
		else
			binarySearch(A, value, mid + 1, high) when A[mid] < value
		else
			mid;
```



## Sidef

Iterative:

```ruby
func binary_search(a, i) {

    var l = 0
    var h = a.end

    while (l <= h) {
        var mid = (h+l / 2 -> int)
        a[mid] > i && (h = mid-1; next)
        a[mid] < i && (l = mid+1; next)
        return mid
    }

    return -1
}
```

Recursive:

```ruby
func binary_search(arr, value, low=0, high=arr.end) {
    high < low && return -1
    var middle = ((high+low) // 2)

    given (arr[middle]) { |item|
        case (value < item) {
            binary_search(arr, value, low, middle-1)
        }
        case (value > item) {
            binary_search(arr, value, middle+1, high)
        }
        case (value == item) {
            middle
        }
    }
}
```


Usage:

```ruby
say binary_search([34, 42, 55, 778], 55);       #=> 2
```


## Simula


```simula
BEGIN


    INTEGER PROCEDURE BINARYSEARCHREC(A, LVALUE);
        INTEGER ARRAY A;
        INTEGER LVALUE; ! VALUE IS A KEY WORD ;
    BEGIN

        INTEGER PROCEDURE SEARCH(LOW, HIGH);
            INTEGER LOW, HIGH;
        BEGIN
            INTEGER MID;
            ! INVARIANTS: VALUE > A[I] FOR ALL I < LOW
                          VALUE < A[I] FOR ALL I > HIGH ;
            MID := (LOW + HIGH) // 2;
            SEARCH := IF HIGH < LOW THEN -LOW - 1
                 ELSE IF A(MID) > LVALUE THEN SEARCH(LOW, MID-1)
                 ELSE IF A(MID) < LVALUE THEN SEARCH(MID+1, HIGH)
                 ELSE MID;
        END SEARCH;

        BINARYSEARCHREC := SEARCH(LOWERBOUND(A, 1), UPPERBOUND(A, 1));
    END BINARYSEARCHREC;


    INTEGER PROCEDURE BINARYSEARCH(A, LVALUE);
        INTEGER ARRAY A;
        INTEGER LVALUE; ! VALUE IS A KEY WORD ;
    BEGIN
        INTEGER LOW, HIGH, MID;
        BOOLEAN FOUND;

        LOW := LOWERBOUND(A, 1);
        HIGH := UPPERBOUND(A, 1);
        WHILE NOT FOUND AND LOW <= HIGH DO BEGIN
            ! INVARIANTS: LVALUE > A(I) FOR ALL I < LOW
                          LVALUE < A(I) FOR ALL I > HIGH ;
            MID := (LOW + HIGH) // 2;
            IF A(MID) > LVALUE THEN
                HIGH := MID - 1
            ELSE IF A(MID) < LVALUE THEN
                LOW := MID + 1
            ELSE
                FOUND := TRUE;
        END;
        ! LVALUE WOULD BE INSERTED AT INDEX "LOW" ;
        BINARYSEARCH := IF FOUND THEN MID ELSE -LOW - 1;
    END BINARYSEARCH;


    COMMENT ** CAUTION ** ONLY WORKS FOR ARRAY LOWER BOUND=0;
    INTEGER ARRAY HAYSTACK(0:9);
    INTEGER I, J, K, NEEDLE;

    OUTTEXT("ARRAY = (");
    I := LOWERBOUND(HAYSTACK, 1);
    FOR J := 1, 6, 17, 29, 45, 78, 79, 87, 95, 100 DO BEGIN
        HAYSTACK(I) := J;
        OUTINT(HAYSTACK(I), 0);
        IF I < UPPERBOUND(HAYSTACK, 1) THEN OUTTEXT(", ");
        I := I + 1;
    END;
    OUTTEXT(")");
    OUTIMAGE;
    OUTIMAGE;

    FOR NEEDLE:= 0, 1, 7, 17, 95, 99, 100, 101 DO BEGIN

        OUTTEXT("LOOKUP RECURSIV ");
        OUTINT(NEEDLE, 3);
        OUTTEXT(" ... INDEX = ");
        K := BINARYSEARCHREC(HAYSTACK, NEEDLE);
        OUTINT(K, 3);
        IF K < 0 THEN OUTTEXT(" NOT FOUND!");
        OUTIMAGE;

        OUTTEXT("LOOKUP ITERATIV ");
        OUTINT(NEEDLE, 3);
        OUTTEXT(" ... INDEX = ");
        K := BINARYSEARCH(HAYSTACK, NEEDLE);
        OUTINT(K, 3);
        IF K < 0 THEN OUTTEXT(" NOT FOUND!");
        OUTIMAGE;

        OUTIMAGE;
    END;

END
```

{{out}}

```txt

ARRAY = (1, 6, 17, 29, 45, 78, 79, 87, 95, 100)

LOOKUP RECURSIV   0 ... INDEX =  -1 NOT FOUND!
LOOKUP ITERATIV   0 ... INDEX =  -1 NOT FOUND!

LOOKUP RECURSIV   1 ... INDEX =   0
LOOKUP ITERATIV   1 ... INDEX =   0

LOOKUP RECURSIV   7 ... INDEX =  -3 NOT FOUND!
LOOKUP ITERATIV   7 ... INDEX =  -3 NOT FOUND!

LOOKUP RECURSIV  17 ... INDEX =   2
LOOKUP ITERATIV  17 ... INDEX =   2

LOOKUP RECURSIV  95 ... INDEX =   8
LOOKUP ITERATIV  95 ... INDEX =   8

LOOKUP RECURSIV  99 ... INDEX = -10 NOT FOUND!
LOOKUP ITERATIV  99 ... INDEX = -10 NOT FOUND!

LOOKUP RECURSIV 100 ... INDEX =   9
LOOKUP ITERATIV 100 ... INDEX =   9

LOOKUP RECURSIV 101 ... INDEX = -11 NOT FOUND!
LOOKUP ITERATIV 101 ... INDEX = -11 NOT FOUND!


```



## UNIX Shell


'''Reading values line by line'''


```bash

#!/bin/ksh
# This should work on any clone of Bourne Shell, ksh is the fastest.

value=$1; [ -z "$value" ] && exit
array=()
size=0

while IFS= read -r line; do
	size=$(($size + 1))
	array[${#array[*]}]=$line
done

```



'''Iterative'''

```bash

left=0
right=$(($size - 1))
while	[ $left -le $right ] ; do
	mid=$((($left + $right) >> 1))
#	echo "$left	$mid(${array[$mid]})	$right"
	if	[ $value -eq ${array[$mid]} ] ; then
		echo $mid
		exit
	elif	[ $value -lt ${array[$mid]} ]; then
		right=$(($mid - 1))
	else
		left=$((mid + 1))
	fi
done
echo 'ERROR 404 : NOT FOUND'

```


'''Recursive'''
<lang> No code yet
```



## SPARK

SPARK does not allow recursion, so only the iterative solution is provided. This example shows the use of a loop assertion.

All the code for this task validates with SPARK GPL 2010 and compiles and executes with GPS GPL 2010.

The Binary_Searches package is shown first. Search is a procedure, rather than a function, so that it can return a Found flag and a Position for Item, if found. This is better design than having a Position value that means 'item not found'.

There are two versions of the package provided, although the Ada code of the two versions is identical.

The first version has a postcondition that if Found is True the Position value returned is correct. This version also has a number of 'check' annotations. These are inserted to allow the Simplifier to prove all the verification conditions. See [[SPARK_Proof_Process|the SPARK Proof Process]].

```Ada
package Binary_Searches is

   subtype Item_Type is Integer; -- From specs.
   subtype Index_Type is Integer range 1 .. 100;
   type Array_Type is array (Index_Type range <>) of Item_Type;

   procedure Search (Source   : in     Array_Type;
                     Item     : in     Item_Type;
                     Found    :     out Boolean;
                     Position :     out Index_Type);
   --# derives Found,
   --#         Position from
   --#            Source,
   --#            Item;
   --# post Found -> Source (Position) = Item;
   -- If Found is False then Position is undefined.

end Binary_Searches;


package body Binary_Searches is

   procedure Search (Source   : in     Array_Type;
                     Item     : in     Item_Type;
                     Found    :     out Boolean;
                     Position :     out Index_Type)
   is
      Lower      : Index_Type; -- Lower bound of Subrange.
      Upper      : Index_Type; -- Upper bound of Subrange.
      Terminated : Boolean;
   begin
      Found := False;
      -- Default status updated on success.

      Lower      := Source'First;
      Upper      := Source'Last;
      Position   := (Lower + Upper) / 2;
      Terminated := False;

      while not Terminated loop
      --# assert Lower >= Source'First
      --#  and   Upper <= Source'Last
      --#  and   Position in Lower .. Upper
      --#  and   not Found;
         if Item < Source (Position) then
            if Position = Lower then
               -- No lower subrange.
               Terminated := True;
            else
               --# check Position > Lower;
               -- For the two following proofs.

               --# check Position - 1 >= Lower;
               --# check Lower + Position - 1 >= Lower * 2;
               --# check (Lower + Position - 1) / 2 >= Lower;
               -- For "Position >= Lower" in loop assertion.

               --# check Lower < Position;
               --# check Lower + Position - 1 <= (Position - 1) * 2;
               --# check (Lower + Position - 1) / 2 <= (Position - 1);
               -- For "Position <= Upper" in loop assertion.

               -- Switch to lower half subrange.
               Upper := Position - 1;
               Position := (Lower + Upper) / 2;
            end if;

         elsif Item > Source (Position) then
            if Position = Upper then
               -- No upper subrange.
               Terminated := True;
            else
               --# check Position < Upper;
               -- For the two following proofs.

               --# check Upper >= Position + 1;
               --# check Position + 1 + Upper >= (Position + 1) * 2;
               --# check (Position + 1 + Upper) / 2 >= (Position + 1);
               -- For "Position >= Lower" in loop assertion.

               --# check Position + 1 <= Upper;
               --# check Position + 1 + Upper <= Upper * 2;
               --# check (Position + 1 + Upper) / 2 <= Upper;
               -- For "Position <= Upper" in loop assertion.

               -- Switch to upper half subrange.
               Lower := Position + 1;
               Position := (Lower + Upper) / 2;
            end if;
         else
            Found      := True;
            Terminated := True;
         end if;
      end loop;
   end Search;

end Binary_Searches;
```

The second version of the package has a stronger postcondition on Search, which also states that if Found is False then there is no value in Source equal to Item. This postcondition cannot be proved without a precondition that Source is ordered. This version needs four user rules (see [[SPARK_Proof_Process|the SPARK Proof Process]]) to be provided to the Simplifier so that it can prove all the verification conditions.

```Ada
package Binary_Searches is

   subtype Item_Type is Integer; -- From specs.
   subtype Index_Type is Integer range 1 .. 100;
   type Array_Type is array (Index_Type range <>) of Item_Type;

   --  Ordered_Between is a 'proof function'.  It does not have a code
   --  body, but its meaning is defined by a proof rule:
   --
   --    Ordered_Between (Source, Low_Bound, High_Bound)
   --      <->
   --    for all I in Index_Type range Low_Bound .. High_Bound - 1 =>
   --             (Source(I) < Source(I + 1)) ;
   --
   --# function Ordered_Between (Source               : Array_Type;
   --#                           Range_From, Range_To : Index_Type)
   --#    return Boolean;

   procedure Search (Source   : in     Array_Type;
                     Item     : in     Item_Type;
                     Found    :     out Boolean;
                     Position :     out Index_Type);
   --# derives Found,
   --#         Position from
   --#            Source,
   --#            Item;
   --# pre  Ordered_Between (Source, Source'First, Source'Last);
   --# post (Found -> (Source (Position) = Item))
   --#  and (not Found ->
   --#         (for all I in Index_Type range Source'Range
   --#                                  => (Source(I) /= Item)));

end Binary_Searches;


package body Binary_Searches is

   procedure Search (Source   : in     Array_Type;
                     Item     : in     Item_Type;
                     Found    :     out Boolean;
                     Position :     out Index_Type)
   is
      Lower      : Index_Type; -- Lower bound of Subrange.
      Upper      : Index_Type; -- Upper bound of Subrange.
      Terminated : Boolean;
   begin
      Found := False;
      -- Default status updated on success.

      Lower      := Source'First;
      Upper      := Source'Last;
      Position   := (Lower + Upper) / 2;
      Terminated := False;

      while not Terminated loop
      --# assert not Terminated
      --#   and  not Found
      --#   and  Lower >= Source'First
      --#   and  Upper <= Source'Last
      --#   and  Position in Lower .. Upper
      --#   and  (Lower = Source'First or
      --#         (Lower > Source'First and Source(Lower - 1) < Item))
      --#   and  (Upper = Source'Last or
      --#         (Upper < Source'Last and Source(Upper + 1) > Item));
         if Item < Source (Position) then
            if Position = Lower then
               -- No lower subrange.
               Terminated := True;
            else
               -- Switch to lower half subrange.
               Upper := Position - 1;
               Position := (Lower + Upper) / 2;
            end if;
         elsif Item > Source (Position) then
            if Position = Upper then
               -- No upper subrange.
               Terminated := True;
            else
               -- Switch to upper half subrange.
               Lower := Position + 1;
               Position := (Lower + Upper) / 2;
            end if;
         else
            Found      := True;
            Terminated := True;
         end if;
      end loop;
   end Search;

end Binary_Searches;
```

The user rules for this version of the package (written in FDL, a language for modelling algorithms).

```txt
binary_search_rule(1): (X + Y) div 2 >= X
                         may_be_deduced_from
                       [ X <= Y,
                         X >= 1,
                         Y >= 1] .

binary_search_rule(2): (X + Y) div 2 <= Y
                         may_be_deduced_from
                       [ X <= Y,
                         X >= 1,
                         Y >= 1] .

binary_search_rule(3): for_all(I_ : integer, First <= I_ and I_ <= Last
                                  -> element(S, [I_]) <> X)
                         may_be_deduced_from
                       [ ordered_between(S, First, Last),
                         P >= First,
                         P <= Last,
                         element(S, [P]) > X,
                         P = First or (P > First and element(S, [P - 1]) < X) ] .

binary_search_rule(4): for_all(I_ : integer, First <= I_ and I_ <= Last
                                  -> element(S, [I_]) <> X)
                         may_be_deduced_from
                       [ ordered_between(S, First, Last),
                         P >= First,
                         P <= Last,
                         element(S, [P]) < X,
                         P = Last or (P < Last and element(S, [P + 1]) > X) ] .

```

The test program:

```Ada
with Binary_Searches;
with SPARK_IO;

--# inherit  Binary_Searches,
--#          SPARK_IO;

--# main_program;
procedure Test_Binary_Search
--# global in out SPARK_IO.Outputs;
--# derives SPARK_IO.Outputs from *;
is

   subtype Index_Type5 is Binary_Searches.Index_Type range 1 .. 5;
   subtype Index_Type7 is Binary_Searches.Index_Type range 1 .. 7;
   subtype Index_Type9 is Binary_Searches.Index_Type range 91 .. 99;
   -- Needed to define a constrained Array_Type.

   subtype Array_Type5 is Binary_Searches.Array_Type (Index_Type5);
   subtype Array_Type7 is Binary_Searches.Array_Type (Index_Type7);
   subtype Array_Type9 is Binary_Searches.Array_Type (Index_Type9);
   -- Needed to pass an array literal to Run_Search.
   -- SPARK does not allow an unconstrained type mark for that purpose.

   procedure Run_Search (Source : in     Binary_Searches.Array_Type;
                         Item   : in     Binary_Searches.Item_Type)
   --# global in out SPARK_IO.Outputs;
   --# derives SPARK_IO.Outputs from *,
   --#                               Item,
   --#                               Source;
   is
      Found    : Boolean;
      Position : Binary_Searches.Index_Type;
   begin
      SPARK_IO.Put_String (File => SPARK_IO.Standard_Output,
                           Item => "Searching for ",
                           Stop => 0);
      SPARK_IO.Put_Integer (File  => SPARK_IO.Standard_Output,
                            Item  => Item,
                            Width => 3,
                            Base  => 10);
      SPARK_IO.Put_String (File => SPARK_IO.Standard_Output,
                           Item => " in (",
                           Stop => 0);
      for Source_Index in Binary_Searches.Index_Type range Source'Range loop
         SPARK_IO.Put_Integer (File  => SPARK_IO.Standard_Output,
                               Item  => Source (Source_Index),
                               Width => 3,
                               Base  => 10);
      end loop;
      SPARK_IO.Put_String (File => SPARK_IO.Standard_Output,
                           Item => "): ",
                           Stop => 0);
      Binary_Searches.Search (Source   => Source,    -- in
                              Item     => Item,      -- in
                              Found    => Found,     -- out
                              Position => Position); -- out
      if Found then
         SPARK_IO.Put_String (File => SPARK_IO.Standard_Output,
                              Item => "found as #",
                              Stop => 0);
         SPARK_IO.Put_Integer (File  => SPARK_IO.Standard_Output,
                               Item  => Position,
                               Width => 0, -- to stick to the sibling '#' sign.
                               Base  => 10);
         SPARK_IO.Put_Line (File => SPARK_IO.Standard_Output,
                            Item => ".",
                            Stop => 0);
      else
         SPARK_IO.Put_Line (File => SPARK_IO.Standard_Output,
                            Item => "not found.",
                            Stop => 0);
      end if;
   end Run_Search;

begin
   SPARK_IO.New_Line (File => SPARK_IO.Standard_Output, Spacing => 1);
   Run_Search (Source => Array_Type5'(0, 1, 2, 3, 4), Item => 3);
   Run_Search (Source => Array_Type5'(2, 4, 6, 8, 10), Item => 3);
   Run_Search (Source => Array_Type7'(1, 2, 3, 4, 5, 6, 7), Item => 0);
   Run_Search (Source => Array_Type7'(1, 2, 3, 4, 5, 6, 7), Item => 7);
   Run_Search (Source => Array_Type9'(1, 2, 3, 4, 5, 6, 7, 8, 9), Item => 10);
   Run_Search (Source => Array_Type9'(1, 2, 3, 4, 5, 6, 7, 8, 9), Item => 1);
   Run_Search (Source => Array_Type9'(1, 2, 3, 4, 5, 6, 7, 8, 9), Item => 6);
end Test_Binary_Search;

```


Test output (for the last three tests the array is indexed from 91):

```txt

Searching for   3 in (  0  1  2  3  4): found as #4.
Searching for   3 in (  2  4  6  8 10): not found.
Searching for   0 in (  1  2  3  4  5  6  7): not found.
Searching for   7 in (  1  2  3  4  5  6  7): found as #7.
Searching for  10 in (  1  2  3  4  5  6  7  8  9): not found.
Searching for   1 in (  1  2  3  4  5  6  7  8  9): found as #91.
Searching for   6 in (  1  2  3  4  5  6  7  8  9): found as #96.

```



## Standard ML

'''Recursive'''

```sml
fun binary_search cmp (key, arr) =
  let
    fun aux slice =
      if ArraySlice.isEmpty slice then
        NONE
      else
        let
 	  val mid = ArraySlice.length slice div 2
        in
	  case cmp (ArraySlice.sub (slice, mid), key)
	  of LESS    => aux (ArraySlice.subslice (slice, mid+1, NONE))
 	   | GREATER => aux (ArraySlice.subslice (slice, 0, SOME mid))
	   | EQUAL   => SOME (#2 (ArraySlice.base slice) + mid)
        end
  in
    aux (ArraySlice.full arr)
  end
```

Usage:

```txt

- val a = Array.fromList [2, 3, 5, 6, 8];
val a = [|2,3,5,6,8|] : int array
- binary_search Int.compare (4, a);
val it = NONE : int option
- binary_search Int.compare (8, a);
val it = SOME 4 : int option

```

Standard ML supports proper tail-recursion; so this is effectively the same as iteration.

'''Library'''
{{works with|SML/NJ}}
Usage:

```txt

- structure IntArray = struct
=   open Array
=   type elem = int
=   type array = int Array.array
=   type vector = int Vector.vector
= end;
structure IntArray :
  sig
[ ... rest omitted ]
- structure IntBSearch = BSearchFn (IntArray);
structure IntBSearch :
  sig
    structure A : <sig>
    val bsearch : ('a * A.elem -> order)
                  -> 'a * A.array -> (int * A.elem) option
  end
- val a = Array.fromList [2, 3, 5, 6, 8];
val a = [|2,3,5,6,8|] : int array
- IntBSearch.bsearch Int.compare (4, a);
val it = NONE : (int * IntArray.elem) option
- IntBSearch.bsearch Int.compare (8, a);
val it = SOME (4,8) : (int * IntArray.elem) option

```



## Swift

'''Recursive'''

```swift>func binarySearch<T: Comparable
(xs: [T], x: T) -> Int? {
  var recurse: ((Int, Int) -> Int?)!
  recurse = {(low, high) in switch (low + high) / 2 {
    case _ where high < low: return nil
    case let mid where xs[mid] > x: return recurse(low, mid - 1)
    case let mid where xs[mid] < x: return recurse(mid + 1, high)
    case let mid: return mid
  }}
  return recurse(0, xs.count - 1)
}
```

'''Iterative'''

```swift>func binarySearch<T: Comparable
(xs: [T], x: T) -> Int? {
  var (low, high) = (0, xs.count - 1)
  while low <= high {
    switch (low + high) / 2 {
      case let mid where xs[mid] > x: high = mid - 1
      case let mid where xs[mid] < x: low = mid + 1
      case let mid: return mid
    }
  }
  return nil
}
```

'''Test'''

```swift
func testBinarySearch(n: Int) {
  let odds = Array(stride(from: 1, through: n, by: 2))
  let result = flatMap(0...n) {binarySearch(odds, $0)}
  assert(result == Array(0..<odds.count))
  println("\(odds) are odd natural numbers")
  for it in result {
    println("\(it) is ordinal of \(odds[it])")
  }
}

testBinarySearch(12)

func flatMap<T, U>(source: [T], transform: (T) -> U?) -> [U] {
  return source.reduce([]) {(var xs, x) in if let x = transform(x) {xs.append(x)}; return xs}
}
```

Output:

```txt
[1, 3, 5, 7, 9, 11] are odd natural numbers
0 is ordinal of 1
1 is ordinal of 3
2 is ordinal of 5
3 is ordinal of 7
4 is ordinal of 9
5 is ordinal of 11
```



## Tcl

ref: [http://wiki.tcl.tk/22796 Tcl wiki]

```tcl
proc binSrch {lst x} {
    set len [llength $lst]
    if {$len == 0} {
        return -1
    } else {
        set pivotIndex [expr {$len / 2}]
        set pivotValue [lindex $lst $pivotIndex]
        if {$pivotValue == $x} {
            return $pivotIndex
        } elseif {$pivotValue < $x} {
            set recursive [binSrch [lrange $lst $pivotIndex+1 end] $x]
            return [expr {$recursive > -1 ? $recursive + $pivotIndex + 1 : -1}]
        } elseif {$pivotValue > $x} {
            set recursive [binSrch [lrange $lst 0 $pivotIndex-1] $x]
            return [expr {$recursive > -1 ? $recursive : -1}]
        }
    }
}
proc binary_search {lst x} {
    if {[set idx [binSrch $lst $x]] == -1} {
        puts "element $x not found in list"
    } else {
        puts "element $x found at index $idx"
    }
}
```

Note also that, from Tcl 8.4 onwards, the <tt>lsearch</tt> command includes the <tt>-sorted</tt> option to enable binary searching of Tcl lists.

```tcl
proc binarySearch {lst x} {
    set idx [lsearch -sorted -exact $lst $x]
    if {$idx == -1} {
        puts "element $x not found in list"
    } else {
        puts "element $x found at index $idx"
    }
}
```


=={{header|TI-83 BASIC}}==

```ti83b
PROGRAM:BINSEARC
:Disp "INPUT A LIST:"
:Input L1
:SortA(L1)
:Disp "INPUT A NUMBER:"
:Input A
:1→L
:dim(L1)→H
:int(L+(H-L)/2)→M
:While L<H and L1(M)≠A
:If A>M
:Then
:M+1→L
:Else
:M-1→H
:End
:int(L+(H-L)/2)→M
:End
:If L1(M)=A
:Then
:Disp A
:Disp "IS AT POSITION"
:Disp M
:Else
:Disp A
:Disp "IS NOT IN"
:Disp L1
```



## uBasic/4tH

{{trans|Run BASIC}}
The overflow is fixed - which is a bit of overkill, since uBasic/4tH has only one array of 256 elements.
<lang>For i = 1 To 100                       ' Fill array with some values
  @(i-1) = i
Next

Print FUNC(_binarySearch(50,0,99))     ' Now find value '50'
End                                    ' and prints its index


_binarySearch Param(3)                 ' value, start index, end index
  Local(1)                             ' The middle of the array

If c@ < b@ Then                        ' Ok, signal we didn't find it
  Return (-1)
Else
  d@ = SHL(b@ + c@, -1)                ' Prevent overflow (LOL!)
  If a@ < @(d@) Then Return (FUNC(_binarySearch (a@, b@, d@-1)))
  If a@ > @(d@) Then Return (FUNC(_binarySearch (a@, d@+1, c@)))
  If a@ = @(d@) Then Return (d@)       ' We found it, return index!
EndIf
```



## UnixPipes

'''Parallel'''

```bash
splitter() {
   a=$1; s=$2; l=$3; r=$4;
   mid=$(expr ${#a[*]} / 2);
   echo $s ${a[*]:0:$mid} > $l
   echo $(($mid + $s)) ${a[*]:$mid} > $r
}

bsearch() {
   (to=$1; read s arr; a=($arr);
       test  ${#a[*]} -gt 1  && (splitter $a $s >(bsearch $to) >(bsearch $to)) || (test "$a" -eq "$to" && echo $a at $s)
   )
}

binsearch() {
   (read arr; echo "0 $arr" | bsearch $1)
}

echo "1 2 3 4 6 7 8 9"  | binsearch 6
```



## VBA

'''Recursive version''':

```vb
Public Function BinarySearch(a, value, low, high)
'search for "value" in ordered array a(low..high)
'return index point if found, -1 if not found

  If high < low Then
    BinarySearch = -1 'not found
    Exit Function
  End If
  midd = low + Int((high - low) / 2) ' "midd" because "Mid" is reserved in VBA
  If a(midd) > value Then
    BinarySearch = BinarySearch(a, value, low, midd - 1)
  ElseIf a(midd) < value Then
    BinarySearch = BinarySearch(a, value, midd + 1, high)
  Else
    BinarySearch = midd
  End If
End Function
```

Here are some test functions:

```vb
Public Sub testBinarySearch(n)
Dim a(1 To 100)
'create an array with values = multiples of 10
For i = 1 To 100: a(i) = i * 10: Next
Debug.Print BinarySearch(a, n, LBound(a), UBound(a))
End Sub

Public Sub stringtestBinarySearch(w)
'uses BinarySearch with a string array
Dim a
a = Array("AA", "Maestro", "Mario", "Master", "Mattress", "Mister", "Mistress", "ZZ")
Debug.Print BinarySearch(a, w, LBound(a), UBound(a))
End Sub
```

and sample output:

```txt

stringtestBinarySearch "Master"
 3
testBinarySearch "Master"
-1
testBinarySearch 170
 17
stringtestBinarySearch 170
-1
stringtestBinarySearch "Moo"
-1
stringtestBinarySearch "ZZ"
 7

```


'''Iterative version:'''

```vb
Public Function BinarySearch2(a, value)
'search for "value" in array a
'return index point if found, -1 if not found

  low = LBound(a)
  high = UBound(a)
  Do While low <= high
    midd = low + Int((high - low) / 2)
    If a(midd) = value Then
      BinarySearch2 = midd
      Exit Function
    ElseIf a(midd) > value Then
      high = midd - 1
    Else
      low = midd + 1
    End If
 Loop
 BinarySearch2 = -1 'not found
End Function
```



## Vedit macro language

'''Iterative'''

For this implementation, the numbers to be searched must be stored in current edit buffer, one number per line.
(Could be for example a csv table where the first column is used as key field.)

```vedit
// Main program for testing BINARY_SEARCH
#3 = Get_Num("Value to search: ")
EOF
#2 = Cur_Line                   // hi
#1 = 1                          // lo
Call("BINARY_SEARCH")
Message("Value ") Num_Type(#3, NOCR)
if (Return_Value < 1) {
    Message(" not found\n")
} else {
    Message(" found at index ") Num_Type(Return_Value)
}
return

:BINARY_SEARCH:
while (#1 <= #2) {
    #12 = (#1 + #2) / 2
    Goto_Line(#12)
    #11 = Num_Eval()
    if (#3 == #11) {
        return(#12)             // found
    } else {
        if (#3 < #11) {
            #2 = #12-1
        } else {
            #1 = #12+1
        }
    }
}
return(0)                       // not found
```



## Visual Basic .NET

'''Iterative'''

```vbnet
Function BinarySearch(ByVal A() As Integer, ByVal value As Integer) As Integer
    Dim low As Integer = 0
    Dim high As Integer = A.Length - 1
    Dim middle As Integer = 0

    While low <= high
        middle = (low + high) / 2
        If A(middle) > value Then
            high = middle - 1
        ElseIf A(middle) < value Then
            low = middle + 1
        Else
            Return middle
        End If
    End While

    Return Nothing
End Function
```

'''Recursive'''

```vbnet
Function BinarySearch(ByVal A() As Integer, ByVal value As Integer, ByVal low As Integer, ByVal high As Integer) As Integer
    Dim middle As Integer = 0

    If high < low Then
        Return Nothing
    End If

    middle = (low + high) / 2

    If A(middle) > value Then
        Return BinarySearch(A, value, low, middle - 1)
    ElseIf A(middle) < value Then
        Return BinarySearch(A, value, middle + 1, high)
    Else
        Return middle
    End If
End Function
```



## VBScript

{{trans|BASIC}}
'''Recursive'''

```vb
Function binary_search(arr,value,lo,hi)
		If hi < lo Then
			binary_search = 0
		Else
			middle=Int((hi+lo)/2)
			If value < arr(middle) Then
				binary_search = binary_search(arr,value,lo,middle-1)
			ElseIf value > arr(middle) Then
				binary_search = binary_search(arr,value,middle+1,hi)
			Else
				binary_search = middle
				Exit Function
			End If
		End If
End Function

'Tesing the function.
num_range = Array(2,3,5,6,8,10,11,15,19,20)
n = CInt(WScript.Arguments(0))
idx = binary_search(num_range,n,LBound(num_range),UBound(num_range))
If idx > 0 Then
	WScript.StdOut.Write n & " found at index " & idx
	WScript.StdOut.WriteLine
Else
	WScript.StdOut.Write n & " not found"
	WScript.StdOut.WriteLine
End If
```

{{out}}
'''Note:  Array index starts at 0.'''

```txt

C:\>cscript /nologo binary_search.vbs 4
4 not found

C:\>cscript /nologo binary_search.vbs 8
8 found at index 4

C:\>cscript /nologo binary_search.vbs 20
20 found at index 9

```



## Wortel

{{trans|JavaScript}}

```wortel
; Recursive
@var rec &[a v l h] [
  @if < h l @return null
  @var m @/ +h l 2
  @? {
    > `m a v @!rec[a v l -m 1]
    < `m a v @!rec[a v +1 m h]
    m
  }
]

; Iterative
@var itr &[a v] [
  @vars{l 0 h #-a}
  @while <= l h [
    @var m @/ +l h 2
    @iff {
      > `m a v :h -m 1
      < `m a v :l +m 1
      @return m
    }
  ]
  null
]
```



## Yabasic

{{trans|Lua}}

```Yabasic
sub floor(n)
    return int(n + .5)
end sub

sub binarySearch(list(), value)
    local low, high, mid

    low = 1 : high = arraysize(list(), 1)

    while(low <= high)
        mid = floor((low + high) / 2)
        if list(mid) > value then
            high = mid - 1
        elsif list(mid) < value then
            low = mid + 1
        else
            return mid
        end if
    wend
    return false
end sub

ITEMS = 10e6

dim list(ITEMS)

for n = 1 to ITEMS
    list(n) = n
next n

print binarySearch(list(), 3)
print peek("millisrunning")
```



## zkl

This algorithm is tail recursive, which means it is both recursive and iterative (since tail recursion optimizes to a jump). Overflow is not possible because Ints (64 bit) are a lot bigger than the max length of a list.

```zkl
fcn bsearch(list,value){	// list is sorted
   fcn(list,value, low,high){
      if (high < low) return(Void);	// not found
      mid:=(low + high) / 2;
      if (list[mid] > value) return(self.fcn(list,value, low,   mid-1));
      if (list[mid] < value) return(self.fcn(list,value, mid+1, high));
      return(mid);			// found
   }(list,value,0,list.len()-1);
}
```


```zkl
list:=T(1,3,5,7,9,11); println("Sorted values: ",list);
foreach i in ([0..12]){
   n:=bsearch(list,i);
   if (Void==n) println("Not found: ",i);
   else println("found ",i," at index ",n);
}
```

{{out}}

```txt

Sorted values: L(1,3,5,7,9,11)
Not found: 0
found 1 at index 0
Not found: 2
found 3 at index 1
Not found: 4
found 5 at index 2
Not found: 6
found 7 at index 3
Not found: 8
found 9 at index 4
Not found: 10
found 11 at index 5
Not found: 12

```



## ZX Spectrum Basic

{{trans|FreeBASIC}}
Iterative method:

```zxbasic
10 DATA 2,3,5,6,8,10,11,15,19,20
20 DIM t(10)
30 FOR i=1 TO 10
40 READ t(i)
50 NEXT i
60 LET value=4: GO SUB 100
70 LET value=8: GO SUB 100
80 LET value=20: GO SUB 100
90 STOP
100 REM Binary search
110 LET lo=1: LET hi=10
120 IF lo>hi THEN LET idx=0: GO TO 170
130 LET middle=INT ((hi+lo)/2)
140 IF value<t(middle) THEN LET hi=middle-1: GO TO 120
150 IF value>t(middle) THEN LET lo=middle+1: GO TO 120
160 LET idx=middle
170 PRINT "Value ";value;
180 IF idx=0 THEN PRINT " not found": RETURN
190 PRINT " found at index ";idx: RETURN

```


## z/Arch Assembler

This optimized version for z/Arch, uses six general regs and avoid branch misspredictions for high/low cases.

```z/Archasm
*        Binary search
BINSRCH  LA    R5,TABLE            Begin of table
         SR    R2,R2               low  = 0
         LA    R3,ENTRIES-1        high = N-1
LOOP     CR    R2,R3               while (low <= high)
         JH    NOTFOUND            {
         ARK   R4,R2,R3               mid = low + high
         SRL   R4,1                   mid = mid / 2
         LA    R1,1(R4)               mid + 1
         AHIK  R0,R4,-1               mid - 1
         MSFI  R4,ENTRYL              mid * length
         AR    R4,R5                  Table[mid]
         CLC   0(L'KEY,R4),SEARCH     Compare
         JE    FOUND                  Equal? => Found
         LOCRH R3,R0                  High?  => HIGH = MID-1
         LOCRL R2,R1                  Low?   => LOW  = MID+1
         J     LOOP                }
```

