+++
title = "Remove duplicate elements"
description = ""
date = 2019-10-22T03:31:43Z
aliases = []
[extra]
id = 1646
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

{{task}}Given an Array, derive a sequence of elements in which all duplicates are removed.

There are basically three approaches seen here:
* Put the elements into a hash table which does not allow duplicates. The complexity is O(''n'') on average, and O(''n''<sup>2</sup>) worst case. This approach requires a hash function for your type (which is compatible with equality), either built-in to your language, or provided by the user.
* Sort the elements and remove consecutive duplicate elements. The complexity of the best sorting algorithms is O(''n'' log ''n''). This approach requires that your type be "comparable", i.e., have an ordering. Putting the elements into a self-balancing binary search tree is a special case of sorting.
* Go through the list, and for each element, check the rest of the list to see if it appears again, and discard it if it does. The complexity is O(''n''<sup>2</sup>). The up-shot is that this always works on any type (provided that you can test for equality).




## 360 Assembly

```360asm
*        Remove duplicate elements - 18/10/2015
REMDUP   CSECT
         USING  REMDUP,R15         set base register
         SR     R6,R6              i=0
         LA     R8,1               k=1
LOOPK    C      R8,N               do k=1 to n
         BH     ELOOPK
         LR     R1,R8              k
         SLA    R1,2
         L      R9,T-4(R1)         e=t(k)
         LR     R7,R8              k
         BCTR   R7,0               j=k-1
LOOPJ    C      R7,=F'1'           do j=k-1 to 1 by -1
         BL     ELOOPJ
         LR     R1,R7              j
         SLA    R1,2
         L      R2,T-4(R1)         t(j)
         CR     R9,R2              if e=t(j) then goto iter
         BE     ITER
         BCTR   R7,0               j=j-1
         B      LOOPJ
ELOOPJ   LA     R6,1(R6)           i=i+1
         LR     R1,R6              i
         SLA    R1,2
         ST     R9,T-4(R1)         t(i)=e
ITER     LA     R8,1(R8)           k=k+1
         B      LOOPK
ELOOPK   LA     R10,PG             pgi=@pg
         LA     R8,1               k=1
LOOP     CR     R8,R6              do k=1 to i
         BH     ELOOP
         LR     R1,R8              k
         SLA    R1,2
         L      R2,T-4(R1)         t(k)
         XDECO  R2,PG+80           edit t(k)
         MVC    0(3,R10),PG+89     output t(k) on 3 char
         LA     R10,3(R10)         pgi=pgi+3
         LA     R8,1(R8)           k=k+1
         B      LOOP
ELOOP    XPRNT  PG,80              print buffer
         XR     R15,R15            set return code
         BR     R14                return to caller
T        DC     F'6',F'6',F'1',F'5',F'6',F'2',F'1',F'7',F'5',F'22'
         DC     F'4',F'19',F'1',F'1',F'6',F'8',F'9',F'10',F'11',F'12'
N        DC     A((N-T)/4)         number of T items
PG       DC     CL92' '
         YREGS
         END    REMDUP
```

```txt
  6  1  5  2  7 22  4 19  8  9 10 11 12
```



## ACL2


```lisp
(remove-duplicates xs)
```



## Ada

```ada
with Ada.Containers.Ordered_Sets, Ada.Text_IO;
use Ada.Text_IO;

procedure Duplicate is
	package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
	Nums : constant array (Natural range <>) of Integer := (1,2,3,4,5,5,6,7,1);
	Unique : Int_Sets.Set;
begin
	for n of Nums loop
		Unique.Include (n);
	end loop;
	for e of Unique loop
		Put (e'img);
	end loop;
end Duplicate;
```



## Aime

Using an index:

```aime
index x;

list(1, 2, 3, 1, 2, 3, 4, 1).ucall(i_add, 1, x, 0);
x.i_vcall(o_, 1, " ");
o_newline();
```

```txt
 1 2 3 4
```

Order preserving solution:

```aime
index x;

for (, integer a in list(8, 2, 1, 8, 2, 1, 4, 8)) {
    if ((x[a] += 1) == 1) {
        o_(" ", a);
    }
}
o_newline();
```

```txt
 8 2 1 4
```



## ALGOL 68

Using the associative array code from [[Associative_array/Iteration#ALGOL_68]]

```algol68
# use the associative array in the Associate array/iteration task    #
# this example uses strings - for other types, the associative       #
# array modes AAELEMENT and AAKEY should be modified as required     #
PR read "aArray.a68" PR

# returns the unique elements of list                                #
PROC remove duplicates = ( []STRING list )[]STRING:
     BEGIN
        REF AARRAY elements := INIT LOC AARRAY;
        INT        count    := 0;
        FOR pos FROM LWB list TO UPB list DO
            IF NOT ( elements CONTAINSKEY list[ pos ] ) THEN
                # first occurance of this element                    #
                elements // list[ pos ] := "";
                count +:= 1
            FI
        OD;
        # construct an array of the unique elements from the         #
        # associative array - the new list will not necessarily be   #
        # in the original order                                      #
        [ count ]STRING result;
        REF AAELEMENT e := FIRST elements;
        FOR pos WHILE e ISNT nil element DO
            result[ pos ] := key OF e;
            e := NEXT elements
        OD;
        result
     END; # remove duplicates #

# test the duplicate removal                                         #
print( ( remove duplicates( ( "A", "B", "D", "A", "C", "F", "F", "A" ) ), newline ) )

```


== {{header|APL}} ==
The primitive monad ∪ means "unique", so:

```apl
∪ 1 2 3 1 2 3 4 1
1 2 3 4
```


```apl
w←1 2 3 1 2 3 4 1
     ((⍳⍨w)=⍳⍴w)/w
1 2 3 4
```



## AppleScript


```applescript
unique({1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d"})

on unique(x)
    set R to {}
    repeat with i in x
        if i is not in R then set end of R to i's contents
    end repeat
    return R
end unique
```



Or, more generally, we can allow for customised definitions of equality and duplication, by following the Haskell prelude in defining a '''nub :: [a] -> [a]''' function which is a special case of '''nubBy :: (a -> a -> Bool) -> [a] -> [a]'''

In the following example, equality is defined as case-insensitive for strings. We would obtain a different list of unique strings by adjusting the '''Eq :: a -> a -> Bool''' function to make it consider case.

```AppleScript
-- CASE-INSENSITIVE UNIQUE ELEMENTS ------------------------------------------

-- nub :: [a] -> [a]
on nub(xs)
    -- Eq :: a -> a -> Bool
    script Eq
        on |λ|(x, y)
            ignoring case
                x = y
            end ignoring
        end |λ|
    end script

    nubBy(Eq, xs)
end nub


-- TEST ----------------------------------------------------------------------
on run
    {intercalate(space, ¬
        nub(splitOn(space, "4 3 2 8 0 1 9 5 1 7 6 3 9 9 4 2 1 5 3 2"))), ¬
        intercalate("", ¬
            nub(characters of "abcabc ABCABC"))}

    --> {"4 3 2 8 0 1 9 5 7 6", "abc "}
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- nubBy :: (a -> a -> Bool) -> [a] -> [a]
on nubBy(fnEq, xxs)

    set lng to length of xxs
    if lng > 1 then
        set x to item 1 of xxs
        set xs to items 2 thru -1 of xxs
        set p to mReturn(fnEq)

        -- notEq :: a -> Bool
        script notEq
            on |λ|(a)
                not (p's |λ|(a, x))
            end |λ|
        end script

        {x} & nubBy(fnEq, filter(notEq, xs))
    else
        xxs
    end if
end nubBy

-- splitOn :: Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set lstParts to text items of strMain
    set my text item delimiters to dlm
    return lstParts
end splitOn
```

```AppleScript
{"4 3 2 8 0 1 9 5 7 6", "abc "}
```



## Applesoft BASIC


```basic
100 DIM L$(15)
110 L$(0) = "NOW"
120 L$(1) = "IS"
130 L$(2) = "THE"
140 L$(3) = "TIME"
150 L$(4) = "FOR"
160 L$(5) = "ALL"
170 L$(6) = "GOOD"
180 L$(7) = "MEN"
190 L$(8) = "TO"
200 L$(9) = "COME"
210 L$(10) = "TO"
220 L$(11) = "THE"
230 L$(12) = "AID"
240 L$(13) = "OF"
250 L$(14) = "THE"
260 L$(15) = "PARTY."

300 N = 15
310 GOSUB 400
320 FOR I = 0 TO N
330     PRINT L$(I) " " ;
340 NEXT
350 PRINT
360 END

400 REMREMOVE DUPLICATES
410 FOR I = N TO 1 STEP -1
420    I$ = L$(I)
430    FOR J = 0 TO I - 1
440        EQ = I$ = L$(J)
450        IF NOT EQ THEN NEXT J
460    IF EQ THEN GOSUB 500
470 NEXT I
480 RETURN

500 REMREMOVE ELEMENT
510 L$(I) = L$(N)
520 L$(N) = ""
530 N = N - 1
540 RETURN
```



## AutoHotkey

Built in Sort has an option to remove duplicates

```AutoHotkey
a = 1,2,1,4,5,2,15,1,3,4
Sort, a, a, NUD`,
MsgBox % a  ; 1,2,3,4,5,15
```



## AWK

We produce an array a with duplicates from a string;
then index a second array b with the contents of a,
so that duplicates make only one entry;
then produce a string with the keys of b,
which is finally output.

```awk
$ awk 'BEGIN{split("a b c d c b a",a);for(i in a)b[a[i]]=1;r="";for(i in b)r=r" "i;print r}'
a b c d
```



## BBC BASIC

```bbcbasic
      DIM list$(15)
      list$() = "Now", "is", "the", "time", "for", "all", "good", "men", \
      \         "to", "come", "to", "the", "aid", "of", "the", "party."
      num% = FNremoveduplicates(list$())
      FOR i% = 0 TO num%-1
        PRINT list$(i%) " " ;
      NEXT
      PRINT
      END

      DEF FNremoveduplicates(l$())
      LOCAL i%, j%, n%, i$
      n% = 1
      FOR i% = 1 TO DIM(l$(), 1)
        i$ = l$(i%)
        FOR j% = 0 TO i%-1
          IF i$ = l$(j%) EXIT FOR
        NEXT
        IF j%>=i% l$(n%) = i$ : n% += 1
      NEXT
      = n%
```

```txt

Now is the time for all good men to come aid of party.

```



## Bracmat

Here are three solutions. The first one (A) uses a hash table, the second (B) uses a pattern for spotting the elements that have a copy further on in the list and only adds those elements to the answer that don't have copies further on. The third solution (C) utilises an mechanism that is very typical of Bracmat, namely that sums (and also products) always are transformed to a normalised form upon evaluation. Normalisation means that terms are ordered in a unique way and that terms that are equal, apart from a numerical factor, are replaced by a single term with a numerical factor that is the sum of the numerical factors of each term. The answer is obtained by replacing all numerical factors by <code>1</code> as the last step.

The list contains atoms and also a few non-atomic expressions. The hash table needs atomic keys, so we apply the <code>str</code> function when searching and inserting elements.

```bracmat
2 3 5 7 11 13 17 19 cats 222 (-100.2) "+11" (1.1) "+7" (7.) 7 5 5 3 2 0 (4.4) 2:?LIST

(A=
  ( Hashing
  =   h elm list
    .   new$hash:?h
      &   whl
        ' ( !arg:%?elm ?arg
          & ( (h..find)$str$!elm
            | (h..insert)$(str$!elm.!elm)
            )
          )
      & :?list
      &   (h..forall)
        $ (
          = .!arg:(?.?arg)&!arg !list:?list
          )
      & !list
  )
& put$("Solution A:" Hashing$!LIST \n,LIN)
);

(B=
  ( backtracking
  =   answr elm
    .     :?answr
        &   !arg
          :   ?
              (   %?`elm
                  ?
                  ( !elm ?
                  | &!answr !elm:?answr
                  )
              & ~
              )
      | !answr
  )
& put$("Solution B:" backtracking$!LIST \n,LIN)
);

(C=
  ( summing
  =   sum car LIST
    .   !arg:?LIST
      & 0:?sum
      &   whl
        ' ( !LIST:%?car ?LIST
          & (.!car)+!sum:?sum
          )
      &   whl
        ' ( !sum:#*(.?el)+?sum
          & !el !LIST:?LIST
          )
      & !LIST
  )
& put$("Solution C:" summing$!LIST \n,LIN)
);

( !A
& !B
& !C
&
)
```

Only solution B produces a list with the same order of elements as in the input.

```txt
Solution A: 19 (4.4) 17 11 13 (1.1) (7.) 222 +11 7 5 3 2 0 cats (-100.2) +7
Solution B: 11 13 17 19 cats 222 (-100.2) +11 (1.1) +7 (7.) 7 5 3 0 (4.4) 2
Solution C: (7.) (4.4) (1.1) (-100.2) cats 222 19 17 13 11 7 5 3 2 0 +7 +11
```



## Brat


```brat
some_array = [1 1 2 1 'redundant' [1 2 3] [1 2 3] 'redundant']

unique_array = some_array.unique
```



## C


===O(n^2) version, using linked lists===

Since there's no way to know ahead of time how large the new data structure will need to be, we'll return a linked list instead of an array.


```c
#include <stdio.h>
#include <stdlib.h>

struct list_node {int x; struct list_node *next;};
typedef struct list_node node;

node * uniq(int *a, unsigned alen)
 {if (alen == 0) return NULL;
  node *start = malloc(sizeof(node));
  if (start == NULL) exit(EXIT_FAILURE);
  start->x = a[0];
  start->next = NULL;

  for (int i = 1 ; i < alen ; ++i)
     {node *n = start;
      for (;; n = n->next)
         {if (a[i] == n->x) break;
          if (n->next == NULL)
             {n->next = malloc(sizeof(node));
              n = n->next;
              if (n == NULL) exit(EXIT_FAILURE);
              n->x = a[i];
              n->next = NULL;
              break;}}}

  return start;}

int main(void)
   {int a[] = {1, 2, 1, 4, 5, 2, 15, 1, 3, 4};
    for (node *n = uniq(a, 10) ; n != NULL ; n = n->next)
        printf("%d ", n->x);
    puts("");
    return 0;}
```


 1 2 4 5 15 3

===O(n^2) version, pure arrays===


```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

/* Returns `true' if element `e' is in array `a'. Otherwise, returns `false'.
 * Checks only the first `n' elements. Pure, O(n).
 */
bool elem(int *a, size_t n, int e)
{
    for (size_t i = 0; i < n; ++i)
        if (a[i] == e)
            return true;

    return false;
}

/* Removes the duplicates in array `a' of given length `n'. Returns the number
 * of unique elements. In-place, order preserving, O(n ^ 2).
 */
size_t nub(int *a, size_t n)
{
    size_t m = 0;

    for (size_t i = 0; i < n; ++i)
        if (!elem(a, m, a[i]))
            a[m++] = a[i];

    return m;
}

/* Out-place version of `nub'. Pure, order preserving, alloc < n * sizeof(int)
 * bytes, O(n ^ 2).
 */
size_t nub_new(int **b, int *a, size_t n)
{
    int *c = malloc(n * sizeof(int));
    memcpy(c, a, n * sizeof(int));
    int m = nub(c, n);
    *b = malloc(m * sizeof(int));
    memcpy(*b, c, m * sizeof(int));
    free(c);
    return m;
}

int main(void)
{
    int a[] = {1, 2, 1, 4, 5, 2, 15, 1, 3, 4};
    int *b;

    size_t n = nub_new(&b, a, sizeof(a) / sizeof(a[0]));

    for (size_t i = 0; i < n; ++i)
        printf("%d ", b[i]);
    puts("");

    free(b);
    return 0;
}
```


 1 2 4 5 15 3


### Sorting method


Using qsort and return uniques in-place:
```c
#include <stdio.h>
#include <stdlib.h>

int icmp(const void *a, const void *b)
{
#define _I(x) *(const int*)x
	return _I(a) < _I(b) ? -1 : _I(a) > _I(b);
#undef _I
}

/* filter items in place and return number of uniques.  if a separate
   list is desired, duplicate it before calling this function */
int uniq(int *a, int len)
{
	int i, j;
	qsort(a, len, sizeof(int), icmp);
	for (i = j = 0; i < len; i++)
		if (a[i] != a[j]) a[++j] = a[i];
	return j + 1;
}

int main()
{
	int x[] = {1, 2, 1, 4, 5, 2, 15, 1, 3, 4};
	int i, len = uniq(x, sizeof(x) / sizeof(x[0]));
	for (i = 0; i < len; i++) printf("%d\n", x[i]);

	return 0;
}
```


 1
 2
 3
 4
 5
 15


## C#

```c#
int[] nums = { 1, 1, 2, 3, 4, 4 };
List<int> unique = new List<int>();
foreach (int n in nums)
    if (!unique.Contains(n))
        unique.Add(n);
```


```c#
int[] nums = {1, 1, 2, 3, 4, 4};
int[] unique = nums.Distinct().ToArray();
```



## C++

This version uses <tt>std::set</tt>, which requires its element type be comparable using the < operator.

```cpp
#include <set>
#include <iostream>
using namespace std;

int main() {
    typedef set<int> TySet;
    int data[] = {1, 2, 3, 2, 3, 4};

    TySet unique_set(data, data + 6);

    cout << "Set items:" << endl;
    for (TySet::iterator iter = unique_set.begin(); iter != unique_set.end(); iter++)
          cout << *iter << " ";
    cout << endl;
}
```


This version uses <tt>hash_set</tt>, which is part of the SGI extension to the Standard Template Library. It is not part of the C++ standard library. It requires that its element type have a hash function.

```cpp
#include <ext/hash_set>
#include <iostream>
using namespace std;

int main() {
    typedef __gnu_cxx::hash_set<int> TyHash;
    int data[] = {1, 2, 3, 2, 3, 4};

    TyHash unique_set(data, data + 6);

    cout << "Set items:" << endl;
    for (TyHash::iterator iter = unique_set.begin(); iter != unique_set.end(); iter++)
          cout << *iter << " ";
    cout << endl;
}
```


This version uses <tt>unordered_set</tt>, which is part of the TR1, which is likely to be included in the next version of C++. It is not part of the C++ standard library. It requires that its element type have a hash function.

```cpp
#include <tr1/unordered_set>
#include <iostream>
using namespace std;

int main() {
    typedef tr1::unordered_set<int> TyHash;
    int data[] = {1, 2, 3, 2, 3, 4};

    TyHash unique_set(data, data + 6);

    cout << "Set items:" << endl;
    for (TyHash::iterator iter = unique_set.begin(); iter != unique_set.end(); iter++)
          cout << *iter << " ";
    cout << endl;
}
```


Alternative method working directly on the array:


```cpp
#include <iostream>
#include <iterator>
#include <algorithm>

// helper template
template<typename T> T* end(T (&array)[size]) { return array+size; }

int main()
{
  int data[] = { 1, 2, 3, 2, 3, 4 };
  std::sort(data, end(data));
  int* new_end = std::unique(data, end(data));
  std::copy(data, new_end, std::ostream_iterator<int>(std::cout, " ");
  std::cout << std::endl;
}
```


Using sort, unique, and erase on a vector.
```cpp
#include <algorithm>
#include <iostream>
#include <vector>

int main() {
  std::vector<int> data = {1, 2, 3, 2, 3, 4};

  std::sort(data.begin(), data.end());
  data.erase(std::unique(data.begin(), data.end()), data.end());

  for(int& i: data) std::cout << i << " ";
  std::cout << std::endl;
  return 0;
}
```



## CafeOBJ


```CafeOBJ

-- The parametrized module NO-DUP-LIST(ELEMENTS :: TRIV) defines the signature of simple Haskell like list structure.
-- The removal of duplicates is handled by the equational properties listed after the signature in brackets {}
-- The binary operation _,_ is associative, commutative, and idempotent.
-- This list structure does not permit duplicates, they are removed during evaluation (called reduction in CafeOBJ)
-- Actual code is contained in module called  NO-DUP-LIST.
-- The tests are performed after opening instantiated NO-DUP-LIST with various concrete types.
-- For further details see: http://www.ldl.jaist.ac.jp/cafeobj/
mod! NO-DUP-LIST(ELEMENTS :: TRIV)  {
    [ List < Elem < Elt]  -- Sorts in Ordered Sorted Algebra
    op [] : -> List { prec: 0 }  -- Empty List
    op _,_ : Elt Elt -> Elt { comm assoc idem prec: 80 l-assoc }
    op [_] : Elt -> List  { prec: 0 }
}

-- Test on lists of INT, CHARACTER, and STRING
open NO-DUP-LIST(INT)
reduce [ 1 , 2 , 1 , 1 ] .     -- Gives [ 1 , 2 ]
open NO-DUP-LIST(CHARACTER)
reduce [ 'a' , 'b' , 'a' , 'a' ] .  -- Gives [ 'a' , 'b' ]
open NO-DUP-LIST(STRING)
reduce [ "abc" , "def" , "abc" ] . -- Gives [ "def" , "abc" ]

```



## Ceylon


```ceylon
<String|Integer>[] data = [1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d"];
<String|Integer>[] unique = HashSet { *data }.sequence();
```



## Clojure



```lisp>user=
 (distinct [1 3 2 9 1 2 3 8 8 1 0 2])
(1 3 2 9 8 0)
user=>
```



## CoffeeScript

```coffeescript
data = [ 1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d" ]
set = []
set.push i for i in data when not (i in set)

console.log data
console.log set
```

```txt
[ 1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd' ]
[ 1, 2, 3, 'a', 'b', 'c', 4, 'd' ]
```



## Common Lisp


To remove duplicates non-destructively:


```lisp
(remove-duplicates '(1 3 2 9 1 2 3 8 8 1 0 2))
> (9 3 8 1 0 2)
```


Or, to remove duplicates in-place:


```lisp
(delete-duplicates '(1 3 2 9 1 2 3 8 8 1 0 2))
> (9 3 8 1 0 2)
```



## D


```d
void main() {
    import std.stdio, std.algorithm;

    [1, 3, 2, 9, 1, 2, 3, 8, 8, 1, 0, 2]
    .sort()
    .uniq
    .writeln;
}
```

```txt
[0, 1, 2, 3, 8, 9]
```

Using an associative array:

```d
void main() {
    import std.stdio;

    immutable data = [1, 3, 2, 9, 1, 2, 3, 8, 8, 1, 0, 2];

    bool[int] hash;
    foreach (el; data)
        hash[el] = true;
    hash.byKey.writeln;
}
```

```txt
[8, 0, 1, 9, 2, 3]
```

Like code D#1, but with an array returned:

```d
void main()
{
    import std.stdio, std.algorithm, std.array;

    auto a = [5,4,32,7,6,4,2,6,0,8,6,9].sort.uniq.array;
    a.writeln;
}
```

```txt
[0, 2, 4, 5, 6, 7, 8, 9, 32]
```


=={{header|Déjà Vu}}==


```dejavu
}
for item in [ 1 10 1 :hi :hello :hi :hi ]:
	@item
!. keys set{
```

```txt
[ 1 :hello 10 :hi ]
```



## Delphi

Generics were added in Delphi2009.


```Delphi
program RemoveDuplicateElements;

{$APPTYPE CONSOLE}

uses Generics.Collections;

var
  i: Integer;
  lIntegerList: TList<Integer>;
const
  INT_ARRAY: array[1..7] of Integer = (1, 2, 2, 3, 4, 5, 5);
begin
  lIntegerList := TList<Integer>.Create;
  try
  for i in INT_ARRAY do
    if not lIntegerList.Contains(i) then
      lIntegerList.Add(i);

  for i in lIntegerList do
    Writeln(i);
  finally
    lIntegerList.Free;
  end;
end.
```


```txt
1
2
3
4
5
```



## E



```e
[1,2,3,2,3,4].asSet().getElements()
```



## ECL


```ecl

inNumbers   := DATASET([{1},{2},{3},{4},{1},{1},{7},{8},{9},{9},{0},{0},{3},{3},{3},{3},{3}], {INTEGER Field1});
DEDUP(SORT(inNumbers,Field1));

```

```txt
0
1
2
3
4
7
8
9

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'collections;
import system'routines;

public program()
{
    var nums := new int[]::(1,1,2,3,4,4);
    auto unique := new Map<int, int>();

    nums.forEach:(n){ unique[n] := n };

    console.printLine(unique.MapValues.asEnumerable())
}
```

```txt

1,2,3,4

```



## Elixir

Elixir has an <code>Enum.uniq</code> built-in function.
```elixir
defmodule RC do
  # Set approach
  def uniq1(list), do: MapSet.new(list) |> MapSet.to_list

  # Sort approach
  def uniq2(list), do: Enum.sort(list) |> Enum.dedup

  # Go through the list approach
  def uniq3(list), do: uniq3(list, [])

  defp uniq3([], res), do: Enum.reverse(res)
  defp uniq3([h|t], res) do
    if h in res, do: uniq3(t, res), else: uniq3(t, [h | res])
  end
end

num = 10000
max = div(num, 10)
list = for _ <- 1..num, do: :rand.uniform(max)
funs = [&Enum.uniq/1, &RC.uniq1/1, &RC.uniq2/1, &RC.uniq3/1]
Enum.each(funs, fn fun ->
  result = fun.([1,1,2,1,'redundant',1.0,[1,2,3],[1,2,3],'redundant',1.0])
  :timer.tc(fn ->
    Enum.each(1..100, fn _ -> fun.(list) end)
  end)
  |> fn{t,_} -> IO.puts "#{inspect fun}:\t#{t/1000000}\t#{inspect result}" end.()
end)
```


```txt

&Enum.uniq/1:   0.296   [1, 2, 'redundant', 1.0, [1, 2, 3]]
&RC.uniq1/1:    0.686   [1, 2, 1.0, [1, 2, 3], 'redundant']
&RC.uniq2/1:    0.921   [1, 1.0, 2, [1, 2, 3], 'redundant']
&RC.uniq3/1:    1.497   [1, 2, 'redundant', 1.0, [1, 2, 3]]

```



## Erlang



```erlang
List = [1, 2, 3, 2, 2, 4, 5, 5, 4, 6, 6, 5].
UniqueList = gb_sets:to_list(gb_sets:from_list(List)).
% Alternatively the builtin:
Unique_list = lists:usort( List ).

```



## Euphoria


```euphoria
include sort.e

function uniq(sequence s)
    sequence out
    s = sort(s)
    out = s[1..1]
    for i = 2 to length(s) do
        if not equal(s[i],out[$]) then
            out = append(out, s[i])
        end if
    end for
    return out
end function

constant s = {1, 2, 1, 4, 5, 2, 15, 1, 3, 4}
? s
? uniq(s)
```


```txt
{1,2,1,4,5,2,15,1,3,4}
{1,2,3,4,5,15}

```


=={{header|F Sharp|F#}}==
The simplest way is to build a set from the given array (this actually works for any enumerable input sequence type, not just arrays):

```fsharp

set [|1;2;3;2;3;4|]

```

gives:

```fsharp

val it : Set<int> = seq [1; 2; 3; 4]

```



## Factor



```factor
USING: sets ;
V{ 1 2 1 3 2 4 5 } members .

V{ 1 2 3 4 5 }
```



## Forth


Forth has no built-in hashtable facility, so the easiest way to achieve this goal is to take the "uniq" program as an example.

The word uniq, if given a sorted array of cells, will remove the duplicate entries and return the new length of the array. For simplicity, uniq has been written to process cells (which are to Forth what "int" is to C), but could easily be modified to handle a variety of data types through deferred procedures, etc.

The input data is assumed to be sorted.


```forth
\ Increments a2 until it no longer points to the same value as a1
\ a3 is the address beyond the data a2 is traversing.
: skip-dups ( a1 a2 a3 -- a1 a2+n )
    dup rot ?do
      over @ i @ <> if drop i leave then
    cell +loop ;

\ Compress an array of cells by removing adjacent duplicates
\ Returns the new count
: uniq ( a n -- n2 )
   over >r             \ Original addr to return stack
   cells over + >r     \ "to" addr now on return stack, available as r@
   dup begin           ( write read )
      dup r@ <
   while
      2dup @ swap !    \ copy one cell
      cell+ r@ skip-dups
      cell 0 d+        \ increment write ptr only
   repeat  r> 2drop  r> - cell / ;
```


Here is another implementation of "uniq" that uses a popular parameters and local variables extension words. It is structurally the same as the above implementation, but uses less overt stack manipulation.


```forth
: uniqv { a n \ r e -- n }
    a n cells+ to e
    a dup to r
    \ the write address lives on the stack
    begin
      r e <
    while
      r @ over !
      r cell+ e skip-dups to r
      cell+
    repeat
    a - cell / ;
```


To test this code, you can execute:


```forth
create test 1 , 2 , 3 , 2 , 6 , 4 , 5 , 3 , 6 ,
here test - cell / constant ntest
: .test ( n -- ) 0 ?do test i cells + ? loop ;

test ntest 2dup cell-sort uniq .test
```


```txt
1 2 3 4 5 6 ok
```



## Fortran

Fortran has no built-in hash functions or sorting functions but the code below implements the compare all elements algorithm.


```fortran


program remove_dups
  implicit none
  integer :: example(12)         ! The input
  integer :: res(size(example))  ! The output
  integer :: k                   ! The number of unique elements
  integer :: i, j

  example = [1, 2, 3, 2, 2, 4, 5, 5, 4, 6, 6, 5]
  k = 1
  res(1) = example(1)
  outer: do i=2,size(example)
     do j=1,k
        if (res(j) == example(i)) then
           ! Found a match so start looking again
           cycle outer
        end if
     end do
     ! No match found so add it to the output
     k = k + 1
     res(k) = example(i)
  end do outer
  write(*,advance='no',fmt='(a,i0,a)') 'Unique list has ',k,' elements: '
  write(*,*) res(1:k)
end program remove_dups


```


Same as above but using 'ANY' to check if the input number already exists in the array of unique elements:


```fortran

program remove_dups
    implicit none
    integer :: example(12)         ! The input
    integer :: res(size(example))  ! The output
    integer :: k                   ! The number of unique elements
    integer :: i

    example = [1, 2, 3, 2, 2, 4, 5, 5, 4, 6, 6, 5]
    k = 1
    res(1) = example(1)
    do i=2,size(example)
        ! if the number already exist in res check next
        if (any( res == example(i) )) cycle
        ! No match found so add it to the output
        k = k + 1
        res(k) = example(i)
    end do

    write(*,advance='no',fmt='(a,i0,a)') 'Unique list has ',k,' elements: '
    write(*,*) res(1:k)
end program remove_dups


```


```txt
Unique list has 6 elements:            1           2           3           4           5           6
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub removeDuplicates(a() As Integer, b() As Integer)
  Dim lb As Integer = LBound(a)
  Dim ub As Integer = UBound(a)
  If ub = -1 Then Return '' empty array
  Redim b(lb To ub)
  b(lb) = a(lb)
  Dim count As Integer = 1
  Dim unique As Boolean

  For i As Integer = lb + 1 To ub
    unique = True
    For j As Integer = lb to i - 1
      If a(i) = a(j) Then
        unique = False
        Exit For
      End If
    Next j
    If unique Then
      b(lb + count) = a(i)
      count += 1
    End If
  Next i

  If count > 0 Then Redim Preserve b(lb To lb + count - 1)
End Sub

Dim a(1 To 10) As Integer = {1, 2, 1, 4, 5, 2, 15, 1, 3, 4}
Dim b() As Integer
removeDuplicates a(), b()

For i As Integer = LBound(b) To UBound(b)
  Print b(i); " ";
Next

Print
Print "Press any key to quit"
Sleep
```


```txt

 1  2  4  5  15  3

```



## Frink

The following demonstrates two of the simplest ways of removing duplicates.

```frink

b = [1, 5, 2, 6, 6, 2, 2, 1, 9, 8, 6, 5]

// One way, using OrderedList.  An OrderedList is a type of array that keeps
// its elements in order.  The items must be comparable.
a = new OrderedList
println[a.insertAllUnique[b]]

// Another way, using the "set" datatype and back to an array.
println[toArray[toSet[b]]

```


{{out}}  Note that sets are not guaranteed to be printed in any specific order.

```txt

[1, 2, 5, 6, 8, 9]
[9, 8, 6, 5, 2, 1]

```


## Gambas

'''[https://gambas-playground.proko.eu/?gist=1e2bb524d2278cd88bccdc21a1683296 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sString As String[] = Split("Now is the time for all the good men to come to the aid of the good party 1 2 1 3 3 3 2 1 1 2 3 4 33 2 5 4 333 5", " ")
Dim sFix As New String[]
Dim sTemp As String

For Each sTemp In sString
  sTemp &= " "
  If InStr(sFix.Join(" ") & " ", sTemp) Then Continue
  sFix.Add(Trim(sTemp))
Next

Print sFix.Join(" ")

End
```

Output:

```txt

Now is the time for all good men to come aid of party 1 2 3 4 33 5 333

```



## GAP


```gap
# Built-in, using sets (which are also lists)
a := [ 1, 2, 3, 1, [ 4 ], 5, 5, [4], 6 ];
# [ 1, 2, 3, 1, [ 4 ], 5, 5, [ 4 ], 6 ]
b := Set(a);
# [ 1, 2, 3, 5, 6, [ 4 ] ]
IsSet(b);
# true
IsList(b);
# true
```



## Go


### Map solution


```go
package main

import "fmt"

func uniq(list []int) []int {
	unique_set := make(map[int]bool, len(list))
	for _, x := range list {
		unique_set[x] = true
	}
	result := make([]int, 0, len(unique_set))
	for x := range unique_set {
		result = append(result, x)
	}
	return result
}

func main() {
	fmt.Println(uniq([]int{1, 2, 3, 2, 3, 4})) // prints: [3 4 1 2] (but in a semi-random order)
}
```


### Map preserving order

It takes only small changes to the above code to preserver order.  Just store the sequence in the map:

```go
package main

import "fmt"

func uniq(list []int) []int {
	unique_set := make(map[int]int, len(list))
	i := 0
	for _, x := range list {
		if _, there := unique_set[x]; !there {
			unique_set[x] = i
			i++
		}
	}
	result := make([]int, len(unique_set))
	for x, i := range unique_set {
		result[i] = x
	}
	return result
}

func main() {
	fmt.Println(uniq([]int{1, 2, 3, 2, 3, 4})) // prints: [1 2 3 4]
}
```

===Float64, removing duplicate NaNs===
In solutions above, you just replace <code>int</code> with another type to use for a list of another type.  (See [[Associative_arrays/Creation#Go]] for acceptable types.)  Except a weird thing happens with NaNs.  They (correctly) don't compare equal, so you have to special case them if you want to remove duplicate NaNs:

```go
package main

import (
	"fmt"
	"math"
)

func uniq(list []float64) []float64 {
	unique_set := map[float64]int{}
	i := 0
	nan := false
	for _, x := range list {
		if _, exists := unique_set[x]; exists {
			continue
		}
		if math.IsNaN(x) {
			if nan {
				continue
			} else {
				nan = true
			}
		}
		unique_set[x] = i
		i++
	}
	result := make([]float64, len(unique_set))
	for x, i := range unique_set {
		result[i] = x
	}
	return result
}

func main() {
	fmt.Println(uniq([]float64{1, 2, math.NaN(), 2, math.NaN(), 4})) // Prints [1 2 NaN 4]
}
```


### Any type using reflection

Go doesn't have templates or generics, but it does have reflection.
Using this it's possible to build a version that will work on almost any array or slice type.
Using the reflect package for this does make the code less readable.

Normally in Go this type of solution is somewhat rare. Instead, for very short code (such as min, max, abs) it's common to cast or make a type specific function by hand as needed. For longer code, often an interface can be used instead (see the <code>sort</code> package for an example).

Note: due to details with how Go handles map keys that contain a NaN somewhere (including within a complex or even within a sub struct field) this version simply omits any NaN containing values it comes across and returns a bool to indicate if that happened. This version is otherwise a translation of the above order preserving map implementation, it does not for example call reflect.DeepEqual so elements with pointers to distinct but equal values will be treated as non-equal.

```go
package main

import (
	"fmt"
	"math"
	"reflect"
)

func uniq(x interface{}) (interface{}, bool) {
	v := reflect.ValueOf(x)
	if !v.IsValid() {
		panic("uniq: invalid argument")
	}
	if k := v.Kind(); k != reflect.Array && k != reflect.Slice {
		panic("uniq: argument must be an array or a slice")
	}
	elemType := v.Type().Elem()
	intType := reflect.TypeOf(int(0))
	mapType := reflect.MapOf(elemType, intType)
	m := reflect.MakeMap(mapType)
	i := 0
	for j := 0; j < v.Len(); j++ {
		x := v.Index(j)
		if m.MapIndex(x).IsValid() {
			continue
		}
		m.SetMapIndex(x, reflect.ValueOf(i))
		if m.MapIndex(x).IsValid() {
			i++
		}
	}
	sliceType := reflect.SliceOf(elemType)
	result := reflect.MakeSlice(sliceType, i, i)
	hadNaN := false
	for _, key := range m.MapKeys() {
		ival := m.MapIndex(key)
		if !ival.IsValid() {
			hadNaN = true
		} else {
			result.Index(int(ival.Int())).Set(key)
		}
	}

	return result.Interface(), hadNaN
}

type MyType struct {
	name  string
	value float32
}

func main() {
	intArray := [...]int{5, 1, 2, 3, 2, 3, 4}
	intSlice := []int{5, 1, 2, 3, 2, 3, 4}
	stringSlice := []string{"five", "one", "two", "three", "two", "three", "four"}
	floats := []float64{1, 2, 2, 4,
		math.NaN(), 2, math.NaN(),
		math.Inf(1), math.Inf(1), math.Inf(-1), math.Inf(-1)}
	complexes := []complex128{1, 1i, 1 + 1i, 1 + 1i,
		complex(math.NaN(), 1), complex(1, math.NaN()),
		complex(math.Inf(+1), 1), complex(1, math.Inf(1)),
		complex(math.Inf(-1), 1), complex(1, math.Inf(1)),
	}
	structs := []MyType{
		{"foo", 42},
		{"foo", 2},
		{"foo", 42},
		{"bar", 42},
		{"bar", 2},
		{"fail", float32(math.NaN())},
	}

	fmt.Print("intArray: ", intArray, " → ")
	fmt.Println(uniq(intArray))
	fmt.Print("intSlice: ", intSlice, " → ")
	fmt.Println(uniq(intSlice))
	fmt.Print("stringSlice: ", stringSlice, " → ")
	fmt.Println(uniq(stringSlice))
	fmt.Print("floats: ", floats, " → ")
	fmt.Println(uniq(floats))
	fmt.Print("complexes: ", complexes, "\n → ")
	fmt.Println(uniq(complexes))
	fmt.Print("structs: ", structs, " → ")
	fmt.Println(uniq(structs))
	// Passing a non slice or array will compile put
	// then produce a run time panic:
	//a := 42
	//uniq(a)
	//uniq(nil)
}
```

```txt

intArray: [5 1 2 3 2 3 4] → [5 1 2 3 4] false
intSlice: [5 1 2 3 2 3 4] → [5 1 2 3 4] false
stringSlice: [five one two three two three four] → [five one two three four] false
floats: [1 2 2 4 NaN 2 NaN +Inf +Inf -Inf -Inf] → [1 2 4 +Inf -Inf] true
complexes: [(1+0i) (0+1i) (1+1i) (1+1i) (NaN+1i) (1+NaNi) (+Inf+1i) (1+Infi) (-Inf+1i) (1+Infi)]
 → [(1+0i) (0+1i) (1+1i) (+Inf+1i) (1+Infi) (-Inf+1i)] true
structs: [{foo 42} {foo 2} {foo 42} {bar 42} {bar 2} {fail NaN}] → [{foo 42} {foo 2} {bar 42} {bar 2}] true

```



## Groovy


```groovy
def list = [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
assert list.size() == 12
println "             Original List: ${list}"

// Filtering the List (non-mutating)
def list2 = list.unique(false)
assert list2.size() == 8
assert list.size() == 12
println "             Filtered List: ${list2}"

// Filtering the List (in place)
list.unique()
assert list.size() == 8
println "   Original List, filtered: ${list}"

def list3 = [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
assert list3.size() == 12

// Converting to Set
def set = list as Set
assert set.size() == 8
println "                       Set: ${set}"
```


```txt
             Original List: [1, 2, 3, a, b, c, 2, 3, 4, b, c, d]
             Filtered List: [1, 2, 3, a, b, c, 4, d]
   Original List, filtered: [1, 2, 3, a, b, c, 4, d]
                       Set: [1, 2, 3, a, b, c, 4, d]
```


=={{header|GW-BASIC}}==
```qbasic

10   ' Remove Duplicates
20   OPTION BASE 1
30   LET MAXI% = 7
40   DIM D(7), R(7): ' data, result
50   ' Set the data.
60   FOR I% = 1 TO 7
70    READ D(I%)
80   NEXT I%
90   ' Remove duplicates.
100  LET R(1) = D(1)
110  LET LRI% = 1: ' last index of result
120  LET P% = 1: ' position
130  WHILE P% < MAXI%
140   LET P% = P% + 1
150   LET ISNEW = 1: ' is a new number?
160   LET RI% = 1: ' current index of result
170   WHILE (RI% <= LRI%) AND ISNEW
180    IF D(P%) = R(RI%) THEN LET ISNEW = 0
190    LET RI% = RI% + 1
200   WEND
210   IF ISNEW THEN LET LRI% = LRI% + 1: LET R(LRI%) = D(P%)
220  WEND
230  FOR RI% = 1 TO LRI%
240   PRINT R(RI%)
250  NEXT RI%
260  END
1000 DATA 1, 2, 2, 3, 4, 5, 5

```

```txt

1
2
3
4
5

```



## Haskell



### Usage



```haskell
 print $ unique [4, 5, 4, 2, 3, 3, 4]

[4,5,2,3]
```



### Sorted result using Set


O(n ln(n)).  Requires there is a partial ordering of elements.


```haskell
import qualified Data.Set as Set

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList
```



### Unsorted result using Set


O(n ln(n)).  Retains original order.  Requires there is a partial ordering of elements.


```haskell
import Data.Set

unique :: Ord a => [a] -> [a]
unique = loop empty
  where
    loop s []                    = []
    loop s (x : xs) | member x s = loop s xs
                    | otherwise  = x : loop (insert x s) xs
```



### Using filter


O(n^2).  Retains original order.  Only requires that elements can be compared for equality.


```haskell
import Data.List

unique :: Eq a => [a] -> [a]
unique []       = []
unique (x : xs) = x : unique (filter (x /=) xs)
```



### Standard Library



```haskell
import Data.List
Data.List.nub :: Eq a => [a] -> [a]
Data.List.Unique.unique :: Ord a => [a] -> [a]
```



## HicEst


```hicest
REAL ::      nums(12)
CHARACTER :: workspace*100

nums = (1, 3, 2, 9, 1, 2, 3, 8, 8, 1, 0, 2)
WRITE(Text=workspace) nums                   ! convert to string
EDIT(Text=workspace, SortDelDbls=workspace)  ! do the job for a string
READ(Text=workspace, ItemS=individuals) nums ! convert to numeric

WRITE(ClipBoard) individuals, "individuals: ", nums ! 6 individuals: 0 1 2 3 8 9 0 0 0 0 0 0
```


=={{header|Icon}} and {{header|Unicon}}==
This solution preserves the original order of the elements.

```Icon
procedure main(args)
    every write(!noDups(args))
end

procedure noDups(L)
    every put(newL := [], notDup(set(),!L))
    return newL
end

procedure notDup(cache, a)
    if not member(cache, a) then {
         insert(cache, a)
         return a
         }
end
```

A sample run is:

```txt

->noDups a b c d c a b e
a
b
c
d
e
->

```



## IDL



```idl
non_repeated_values = array[uniq(array, sort( array))]
```



## Inform 7



```inform7
To decide which list of Ks is (L - list of values of kind K) without duplicates:
	let result be a list of Ks;
	repeat with X running through L:
		add X to result, if absent;
	decide on result.
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "RemoveDu.bas"
110 RANDOMIZE
120 NUMERIC ARR(1 TO 20),TOP
130 LET TOP=FILL(ARR)
140 CALL WRITE(ARR,TOP)
150 LET TOP=REMOVE(ARR)
160 CALL WRITE(ARR,TOP)
170 DEF WRITE(REF A,N)
180   FOR I=1 TO N
190     PRINT A(I);
200   NEXT
210   PRINT
220 END DEF
230 DEF FILL(REF A)
240   LET FILL=UBOUND(A):LET A(LBOUND(A))=1
250   FOR I=LBOUND(A)+1 TO UBOUND(A)
260     LET A(I)=A(I-1)+RND(3)
270   NEXT
280 END DEF
290 DEF REMOVE(REF A)
300   LET ST=0
310   FOR I=LBOUND(A)+1 TO UBOUND(A)
320     IF A(I-1)=A(I) THEN LET ST=ST+1
330     IF ST>0 THEN LET A(I-ST)=A(I)
340   NEXT
350   LET REMOVE=UBOUND(A)-ST
360 END DEF
```


```txt
START
 1  1  2  4  5  7  9  10  12  14  16  16  16  17  18  20  20  22  23  23
 1  2  4  5  7  9  10  12  14  16  17  18  20  22  23
ok
START
 1  2  4  5  5  5  7  8  9  9  10  10  10  12  14  15  17  17  18  20
 1  2  4  5  7  8  9  10  12  14  15  17  18  20
ok
START
 1  3  3  4  5  6  8  10  11  12  14  16  16  16  16  18  18  19  21  21
 1  3  4  5  6  8  10  11  12  14  16  18  19  21
ok
START
 1  3  3  4  5  5  7  9  11  13  13  14  16  17  17  18  19  19  20  21
 1  3  4  5  7  9  11  13  14  16  17  18  19  20  21
ok
START
 1  2  3  5  5  6  6  7  8  10  12  14  15  17  17  19  21  23  25  25
 1  2  3  5  6  7  8  10  12  14  15  17  19  21  23  25
ok
```



## J

The verb<code> ~. </code>removes duplicate items from ''any'' array (numeric, character, or other; vector, matrix, rank-n array). For example:

```j
   ~. 4 3 2 8 0 1 9 5 1 7 6 3 9 9 4 2 1 5 3 2
4 3 2 8 0 1 9 5 7 6
   ~. 'chthonic eleemosynary paronomasiac'
chtoni elmsyarp
```

Or (since J defines an item of an n dimensional array as its n-1 dimensional sub arrays):


```j
   0 1 1 2 0 */0 1 2
0 0 0
0 1 2
0 1 2
0 2 4
0 0 0
   ~. 0 1 1 2 0 */0 1 2
0 0 0
0 1 2
0 2 4
```



## Java

```java5
import java.util.*;

class Test {

    public static void main(String[] args) {

        Object[] data = {1, 1, 2, 2, 3, 3, 3, "a", "a", "b", "b", "c", "d"};
        Set<Object> uniqueSet = new HashSet<Object>(Arrays.asList(data));
        for (Object o : uniqueSet)
            System.out.printf("%s ", o);
    }
}
```


```txt
1 a 2 b 3 c d
```


```java
import java.util.*;

class Test {

    public static void main(String[] args) {

        Object[] data = {1, 1, 2, 2, 3, 3, 3, "a", "a", "b", "b", "c", "d"};
        Arrays.stream(data).distinct().forEach((o) -> System.out.printf("%s ", o));
    }
}
```


```txt
1 2 3 a b c d
```



## JavaScript

This uses the <code>===</code> "strict equality" operator, which does no type conversions (<code>4 == "4"</code> is true but <code>4 === "4"</code> is false)

```javascript
function unique(ary) {
    // concat() with no args is a way to clone an array
    var u = ary.concat().sort();
    for (var i = 1; i < u.length; ) {
        if (u[i-1] === u[i])
            u.splice(i,1);
        else
            i++;
    }
    return u;
}

var ary = [1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d", "4"];
var uniq = unique(ary);
for (var i = 0; i < uniq.length; i++)
    print(uniq[i] + "\t" + typeof(uniq[i]));
```


```txt
1 - number
2 - number
3 - number
4 - number
4 - string
a - string
b - string
c - string
d - string
```


Or, extend the prototype for Array:

```javascript
Array.prototype.unique = function() {
    var u = this.concat().sort();
    for (var i = 1; i < u.length; ) {
        if (u[i-1] === u[i])
            u.splice(i,1);
        else
            i++;
    }
    return u;
}
var uniq = [1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d"].unique();
```


With reduce and arrow functions (ES6):

```javascript
Array.prototype.unique = function() {
   return this.sort().reduce( (a,e) => e === a[a.length-1] ? a : (a.push(e), a), [] )
}
```


With sets and spread operator (ES6):

```javascript
Array.prototype.unique = function() {
    return [... new Set(this)]
}
```


If, however, the array is homogenous, or we wish to interpret it as such by using JavaScript's Abstract Equality comparison (as in '==', see http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.3) then it proves significantly faster to use a hash table.

For example, in ES 5:


```JavaScript
function uniq(lst) {
  var u = [],
    dct = {},
    i = lst.length,
    v;

  while (i--) {
    v = lst[i], dct[v] || (
      dct[v] = u.push(v)
    );
  }
  u.sort(); // optional

  return u;
}
```


Or, to allow for customised definitions of equality and duplication, we can follow the Haskell prelude in defining a '''nub :: [a] -> [a] function''' which is a special case of '''nubBy :: (a -> a -> Bool) -> [a] -> [a]'''

```JavaScript
(function () {
    'use strict';

    // nub :: [a] -> [a]
    function nub(xs) {

        // Eq :: a -> a -> Bool
        function Eq(a, b) {
            return a === b;
        }

        // nubBy :: (a -> a -> Bool) -> [a] -> [a]
        function nubBy(fnEq, xs) {
            var x = xs.length ? xs[0] : undefined;

            return x !== undefined ? [x].concat(
                nubBy(fnEq, xs.slice(1)
                    .filter(function (y) {
                        return !fnEq(x, y);
                    }))
            ) : [];
        }

        return nubBy(Eq, xs);
    }


    // TEST

    return [
        nub('4 3 2 8 0 1 9 5 1 7 6 3 9 9 4 2 1 5 3 2'.split(' '))
        .map(function (x) {
            return Number(x);
        }),
        nub('chthonic eleemosynary paronomasiac'.split(''))
        .join('')
    ]

})();
```


```txt
[[4, 3, 2, 8, 0, 1, 9, 5, 7, 6], "chtoni elmsyarp"]
```



## jq

If it is acceptable to alter the ordering of elements, then
the builtin (fast) filter, '''unique''', can be used. It can be used for arrays with elements of any JSON type and returns the distinct elements in sorted order.

```jq
[4,3,2,1,1,2,3,4] | unique
```


If all but the first occurrence of each element should be deleted, then the following function could be used.  It retains the advantage of imposing no restrictions on the types of elements in the array and for that reason is slightly more complex than would otherwise be required.

```jq
def removeAllButFirst:

  # The hash table functions all expect the hash table to be the input.

  # Is x in the hash table?
  def hashed(x):
    (x|tostring) as $value
    | .[$value] as $bucket
    | $bucket and (.[$value] | index([x]));

  # Add x to the hash table:
  def add_hash(x):
    (x|tostring) as $value
    | .[$value] as $bucket
    | if $bucket and ($bucket | index([x])) then .
      else .[$value] += [x]
      end;

  reduce .[] as $item
    ( [[], {}]; # [array, hash]
      if .[1] | hashed($item) then .
      else [ (.[0] + [$item]), (.[1] | add_hash($item)) ]
      end)
  | .[0];


```



## Julia

```julia
a = [1, 2, 3, 4, 1, 2, 3, 4]
@show unique(a) Set(a)
```


```txt
unique(a) = [1, 2, 3, 4]
Set(a) = Set([4, 2, 3, 1])
```



## K

(Inspired by the J version.)


```K
   a:4 5#20?13   / create a random 4 x 5 matrix
(12 7 12 4 3
 6 3 7 4 7
 3 8 3 1 2
 2 12 6 4 1)

   ,/a           / flatten to array
12 7 12 4 3 6 3 7 4 7 3 8 3 1 2 2 12 6 4 1

   ?,/a          / distinct elements
12 7 4 3 6 8 1 2

   ?"chthonic eleemosynary paronomasiac"
"chtoni elmsyarp"

   ?("this";"that";"was";"that";"was";"this")
("this"
 "that"
 "was")

   0 1 1 2 0 *\: 0 1 2
(0 0 0
 0 1 2
 0 1 2
 0 2 4
 0 0 0)

   ?0 1 1 2 0 *\: 0 1 2
(0 0 0
 0 1 2
 0 2 4)
```



## Kotlin

```scala
fun main(args: Array<String>) {
    val data = listOf(1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d")
    val set = data.distinct()

    println(data)
    println(set)
}
```

```txt
[1, 2, 3, a, b, c, 2, 3, 4, b, c, d]
[1, 2, 3, a, b, c, 4, d]
```



## Lang5


```lang5
: dip  swap '_ set execute _ ;

: remove-duplicates
    [] swap do unique? length 0 == if break then loop drop ;
: unique?
    0 extract swap "2dup in if drop else append then" dip ;

[1 2 6 3 6 4 5 6] remove-duplicates .
```

Built-in function:

```lang5
[1 2 6 3 6 4 5 6] 's distinct
[1 2 6 3 6 4 5 6] 's dress dup union .
```



## Lasso


```Lasso
local(
	x = array(3,4,8,1,8,1,4,5,6,8,9,6),
	y = array
)
with n in #x where #y !>> #n do => { #y->insert(#n) }
// result: array(3, 4, 8, 1, 5, 6, 9)
```



## Liberty BASIC

LB has arrays, but here the elements are stored in a space-separated string.

```lb

a$ =" 1 $23.19 2 elbow 3 2 Bork 4 3 elbow 2 $23.19 "
print "Original set of elements = ["; a$; "]"

b$ =removeDuplicates$( a$)
print "With duplicates removed  = ["; b$; "]"

end

function removeDuplicates$( in$)
    o$ =" "
    i  =1
    do
        term$    =word$( in$, i, " ")
        if instr( o$, " "; term$; " ") =0 and term$ <>" " then o$ =o$ +term$ +" "
        i        =i +1
    loop until term$ =""
    removeDuplicates$ =o$
end function

```


 Original set of elements = [ 1 $23.19 2 elbow 3 2 Bork 4 3 elbow 2 $23.19 ]
 With duplicates removed  = [ 1 $23.19 2 elbow 3 Bork 4  ]


## Logo

```logo
show remdup [1 2 3 a b c 2 3 4 b c d]   ; [1 a 2 3 4 b c d]
```



## Lua


```Lua
items = {1,2,3,4,1,2,3,4,"bird","cat","dog","dog","bird"}
flags = {}
io.write('Unique items are:')
for i=1,#items do
   if not flags[items[i]] then
      io.write(' ' .. items[i])
      flags[items[i]] = true
   end
end
io.write('\n')
```

```txt
Unique items are: 1 2 3 4 bird cat dog
```


Lua doesn't accept Not-a-Number (NaN) and nil as table key, we can handle them like this (Lua 5.3):

```Lua
local items = {1,2,3,4,1,2,3,4,0/0,nil,"bird","cat","dog","dog","bird",0/0}

function rmdup(t)
  local r,dup,c,NaN = {},{},1,{}
  for i=1,#t do
    local e = t[i]
    local k = e~=e and NaN or e
    if k~=nil and not dup[k] then
      c, r[c], dup[k]= c+1, e, true
    end
  end
  return r
end

print(table.concat(rmdup(items),' '))
```

```txt
1 2 3 4 nan bird cat dog
```



## Maple

This is simplest with a list, which is an immutable array.

```Maple>
 L := [ 1, 2, 1, 2, 3, 3, 2, 1, "a", "b", "b", "a", "c", "b" ];
      L := [1, 2, 1, 2, 3, 3, 2, 1, "a", "b", "b", "a", "c", "b"]

> [op]({op}(L));
                        [1, 2, 3, "a", "b", "c"]
```

That is idiomatic, but perhaps a bit cryptic; here is a more verbose equivalent:

```Maple>
 convert( convert( L, 'set' ), 'list' );
                        [1, 2, 3, "a", "b", "c"]
```

For an Array, which is mutable, the table solution works well in Maple.

```Maple>
 A := Array( L ):
> for u in A do T[u] := 1 end: Array( [indices]( T, 'nolist' ) );
                        [1, 2, 3, "c", "a", "b"]
```

Note that the output (due to the Array() constructor) '''is''' in fact an Array.


## Mathematica

Built-in function:

```Mathematica
DeleteDuplicates[{0, 2, 1, 4, 2, 0, 3, 1, 1, 1, 0, 3}]
```

gives back:

```Mathematica
{0, 2, 1, 4, 3}
```


Delete duplicates and return sorted elements:

```Mathematica

Union[{0, 2, 1, 4, 2, 0, 3, 1, 1, 1, 0, 3}]
```

```Mathematica
{0, 1, 2, 3, 4}
```



## MATLAB

MATLAB has a built-in function, "unique(list)", which performs this task.
<br \>Sample Usage:

```MATLAB>>
 unique([1 2 6 3 6 4 5 6])

ans =

     1     2     3     4     5     6
```


NOTE: The unique function only works for vectors and not for true arrays.


## Maxima



```maxima
unique([8, 9, 5, 2, 0, 7, 0, 0, 4, 2, 7, 3, 9, 6, 6, 2, 4, 7, 9, 8, 3, 8, 0, 3, 7, 0, 2, 7, 6, 0]);
[0, 2, 3, 4, 5, 6, 7, 8, 9]
```



## MAXScript


```maxscript
uniques = #(1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d")
for i in uniques.count to 1 by -1 do
(
    id = findItem uniques uniques[i]
    if (id != i) do deleteItem uniques i
)
```



## Microsoft Small Basic

```microsoftsmallbasic

' Set the data.
dataArray[1] = 1
dataArray[2] = 2
dataArray[3] = 2
dataArray[4] = 3
dataArray[5] = 4
dataArray[6] = 5
dataArray[7] = 5

resultArray[1] = dataArray[1]
lastResultIndex = 1
position = 1
While position < Array.GetItemCount(dataArray)
  position = position + 1
  isNewNumber = 1 ' logical 1
  resultIndex = 1
  While (resultIndex <= lastResultIndex) And isNewNumber = 1
    If dataArray[position] = resultArray[resultIndex] Then
      isNewNumber = 0
    EndIf
    resultIndex = resultIndex + 1
  EndWhile
  If isNewNumber = 1 Then
    lastResultIndex = lastResultIndex + 1
    resultArray[lastResultIndex] = dataArray[position]
  EndIf
EndWhile
For resultIndex = 1 To lastResultIndex
  TextWindow.WriteLine(resultArray[resultIndex])
EndFor

```



## ML

=
## mLite
=
A bit like option 3, except copying each element as encountered, and checking to see if it has already been encountered

```ocaml
fun mem (x, []) = false
      | (x eql a, a :: as) = true
      | (x, _ :: as) = mem (x, as)
;
fun remdup
		([], uniq) = rev uniq
	|	(h :: t, uniq) = if mem(h, uniq) then
				remdup (t, uniq)
			else
				remdup (t, h :: uniq)
	|	L =	remdup (L, [])

;
println ` implode ` remdup ` explode "the quick brown fox jumped over the lazy dog";
println ` remdup [1,2,3,4,4,3,2,1, "dog","cat","dog", 1.1, 2.2, 3.3, 1.1];

```

```txt
the quickbrownfxjmpdvlazyg
[1, 2, 3, 4, dog, cat, 1.1, 2.2, 3.3]
```



## MiniScript


```MiniScript
items = [1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d"]
d = {}
for i in items
    d.push i
end for
print d.indexes
```

```txt
["b", 1, "d", 3, "a", 4, "c", 2]
```


=={{header|Modula-2}}==
```modula2

MODULE RemoveDuplicates;

FROM STextIO IMPORT
  WriteLn;
FROM SWholeIO IMPORT
  WriteInt;

TYPE
  TArrayRange = [1 .. 7];
  TArray = ARRAY TArrayRange OF INTEGER;

VAR
  DataArray, ResultArray: TArray;
  ResultIndex, LastResultIndex, Position: CARDINAL;
  IsNewNumber: BOOLEAN;

BEGIN
  (* Set the data. *);
  DataArray[1] := 1;
  DataArray[2] := 2;
  DataArray[3] := 2;
  DataArray[4] := 3;
  DataArray[5] := 4;
  DataArray[6] := 5;
  DataArray[7] := 5;

  ResultArray[1] := DataArray[1];
  LastResultIndex := 1;
  Position := 1;
  WHILE Position < HIGH(DataArray) DO
    INC(Position);
    IsNewNumber := TRUE;
    ResultIndex := 1;
    WHILE (ResultIndex <= LastResultIndex) AND IsNewNumber DO
      IF DataArray[Position] = ResultArray[ResultIndex] THEN
        IsNewNumber := FALSE;
      END;
      INC(ResultIndex);
    END;
    IF IsNewNumber THEN
      INC(LastResultIndex);
      ResultArray[LastResultIndex] := DataArray[Position];
    END
  END;
  FOR ResultIndex := 1 TO LastResultIndex DO
    WriteInt(ResultArray[ResultIndex], 1);
    WriteLn;
  END;
END RemoveDuplicates.

```



## MUMPS

<p>We'll take advantage of the fact that an array can only have one index of any specific value. Sorting into canonical order is a side effect.
If the indices are strings containing the separator string, they'll be split apart.</p>
```MUMPS
REMDUPE(L,S)
 ;L is the input listing
 ;S is the separator between entries
 ;R is the list to be returned
 NEW Z,I,R
 FOR I=1:1:$LENGTH(L,S) SET Z($PIECE(L,S,I))=""
 ;Repack for return
 SET I="",R=""
 FOR  SET I=$O(Z(I)) QUIT:I=""  SET R=$SELECT($L(R)=0:I,1:R_S_I)
 KILL Z,I
 QUIT R
```

Example:
```txt
USER>W $$REMDUPE^ROSETTA("1,2,3,4,5,2,5,""HELLO"",42,""WORLD""",",")
1,2,3,4,5,42,"HELLO","WORLD"
```



## Neko


```ActionScript
/**
 Remove duplicate elements, in Neko
*/

var original = $array(1, 2, 1, 4, 5, 2, 15, 1, 3, 4)

/* Create a table with only unique elements from the array */
var dedup = function(a) {
    var size = $asize(a)
    var hash = $hnew(size)
    while size > 0 {
        var v = a[size - 1]
        var k = $hkey(v)
        $hset(hash, k, v, null)
        size -= 1
    }
    return hash
}

/* Show the original list and the unique values */
$print(original, "\n")
var show = function(k, v) $print(v, " ")
$hiter(dedup(original), show)
$print("\n")
```


```txt
prompt$ nekoc remove-duplicates.neko
prompt$ neko remove-duplicates.n
[1,2,1,4,5,2,15,1,3,4]
1 2 3 4 5 15
```



## Nemerle


```Nemerle
using System.Console;

module RemDups
{
    Main() : void
    {
        def nums = array[1, 4, 6, 3, 6, 2, 7, 2, 5, 2, 6, 8];
        def unique = $[n | n in nums].RemoveDuplicates();
        WriteLine(unique);
    }
}
```



## NetRexx

This sample takes advantage of the NetRexx built-in <tt>Rexx</tt> object's indexed string capability (associative arrays).  <tt>Rexx</tt> indexed strings act very like hash tables:

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

-- Note: Task requirement is to process "arrays".  The following converts arrays into simple lists of words:
--       Putting the resulting list back into an array is left as an exercise for the reader.
a1 = [2, 3, 5, 7, 11, 13, 17, 19, 'cats', 222, -100.2, +11, 1.1, +7, '7.', 7, 5, 5, 3, 2, 0, 4.4, 2]
a2 = [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
a3 = ['Now', 'is', 'the', 'time', 'for', 'all', 'good', 'men', 'to', 'come', 'to', 'the', 'aid', 'of', 'the', 'party.']
x = 0
lists = ''
x = x + 1; lists[0] = x; lists[x] = array2wordlist(a1)
x = x + 1; lists[0] = x; lists[x] = array2wordlist(a2)
x = x + 1; lists[0] = x; lists[x] = array2wordlist(a3)

loop ix = 1 to lists[0]
  nodups_list = remove_dups(lists[ix])
  say ix.right(4)':' lists[ix]
  say ''.right(4)':' nodups_list
  say
  end ix

return

--
### =======================================================================

method remove_dups(list) public static

  newlist = ''
  nodups = '0'
  loop w_ = 1 to list.words()
    ix = list.word(w_)
    nodups[ix] = nodups[ix] + 1 -- we can even collect a count of dups if we want
    end w_
  loop k_ over nodups
    newlist = newlist k_
    end k_

  return newlist.space

--
### =======================================================================

method array2wordlist(ra = Rexx[]) public static

  wordlist = ''
  loop r_ over ra
    wordlist = wordlist r_
    end r_

  return wordlist.space

```

```txt

   1: 2 3 5 7 11 13 17 19 cats 222 -100.2 11 1.1 7 7. 7 5 5 3 2 0 4.4 2
    : 13 2 3 17 19 7. 4.4 5 222 7 -100.2 1.1 cats 0 11

   2: 1 2 3 a b c 2 3 4 b c d
    : c 2 d 3 4 a b 1

   3: Now is the time for all good men to come to the aid of the party.
    : Now aid for men to the party. come time of is all good

```



## NewLISP


```NewLISP
(unique '(1 2 3 a b c 2 3 4 b c d))
```



## Nial


```nial
uniques := [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
cull uniques
=+-+-+-+-+-+-+-+-+
=|1|2|3|a|b|c|4|d|
=+-+-+-+-+-+-+-+-+
```


Using strand form

```nial
cull 1 1 2 2 3 3
=1 2 3
```



## Nim


```nim
import sequtils, algorithm, intsets

# Go through the list, and for each element, check the rest of the list to see
# if it appears again,
var items = @[1, 2, 3, 2, 3, 4, 5, 6, 7]
echo deduplicate(items) # O(n^2)

proc filterDup(xs): seq[int] =
  result = @[xs[0]]
  var last = xs[0]
  for x in xs[1..xs.high]:
    if x != last:
      result.add(x)
      last = x

#  Put the elements into a hash table which does not allow duplicates.
var s = initIntSet()
for x in items:
  s.incl(x)
echo s

# Sort the elements and remove consecutive duplicate elements.
sort(items, system.cmp[int]) # O(n log n)
echo filterDup(items) # O(n)
```



## Objeck


```objeck

use Structure;

bundle Default {
  class Unique {
    function : Main(args : String[]) ~ Nil {
      nums := [1, 1, 2, 3, 4, 4];
      unique := IntVector->New();

      each(i : nums) {
        n := nums[i];
        if(unique->Has(n) = false) {
          unique->AddBack(n);
        };
      };

      each(i : unique) {
        unique->Get(i)->PrintLine();
      };
    }
  }
}

```


=={{header|Objective-C}}==

```objc
NSArray *items = [NSArray arrayWithObjects:@"A", @"B", @"C", @"B", @"A", nil];

NSSet *unique = [NSSet setWithArray:items];
```



## OCaml


```ocaml
let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let _ =
  uniq [1;2;3;2;3;4]
```


Another solution (preserves order of first occurrence):

```ocaml
let uniq lst =
  let seen = Hashtbl.create (List.length lst) in
  List.filter (fun x -> let tmp = not (Hashtbl.mem seen x) in
                        Hashtbl.replace seen x ();
                        tmp) lst

let _ =
  uniq [1;2;3;2;3;4]
```


Solution reversing list order :

```ocaml
let uniq l =
  let rec tail_uniq a l =
    match l with
      | [] -> a
      | hd::tl -> tail_uniq (hd::a) (List.filter (fun x -> x  != hd) tl) in
  tail_uniq [] l
```


```ocaml
List.sort_uniq compare [1;2;3;2;3;4]
```



## Octave


```octave

input=[1 2 6 4 2 32 5 5 4 3 3 5 1  2 32 4 4];
output=unique(input);

```



## Oforth


The list is converted to a set to remove duplicate elements

```txt

import: set

[ 1, 2, 3, 1, 2, 4, 1, 3, 4, 5 ] asSet println
[1, 2, 3, 4, 5]

```



## ooRexx


```ooRexx
data = .array~of(1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d")
uniqueData = .set~new~union(data)~makearray~sort

say "Unique elements are"
say
do item over uniqueData
   say item
end
```

```txt
Unique elements are

1
2
3
4
a
b
c
d
```



## Oz

The following solutions only works if the value type is allowed as a key in a dictionary.


```oz
declare

  fun {Nub Xs}
     D = {Dictionary.new}
  in
     for X in Xs do D.X := unit end
     {Dictionary.keys D}
  end

in

  {Show {Nub [1 2 1 3 5 4 3 4 4]}}
```



## PARI/GP

Sort and remove duplicates. Other methods should be implemented as well.

```parigp
rd(v)={
  vecsort(v,,8)
};
```



## Pascal


```pascal
Program RemoveDuplicates;

const
  iArray: array[1..7] of integer = (1, 2, 2, 3, 4, 5, 5);

var
  rArray: array[1..7] of integer;
  i, pos, last: integer;
  newNumber: boolean;

begin
  rArray[1] := iArray[1];
  last := 1;
  pos := 1;
  while pos < high(iArray) do
  begin
    inc(pos);
    newNumber := true;
    for i := low(rArray) to last do
      if iArray[pos] = rArray[i] then
      begin
        newNumber := false;
	break;
      end;
    if newNumber then
    begin
      inc(last);
      rArray[last] := iArray[pos];
    end;
  end;
  for i := low(rArray) to last do
    writeln (rArray[i]);
end.
```

```txt
% ./RemoveDuplicates
1
2
3
4
5
```



## Perl

(this version even preserves the order of first appearance of each element)

```perl
use List::MoreUtils qw(uniq);

my @uniq = uniq qw(1 2 3 a b c 2 3 4 b c d);
```


It is implemented like this:

```perl
my %seen;
my @uniq = grep {!$seen{$_}++} qw(1 2 3 a b c 2 3 4 b c d);
```


Note: the following two solutions convert elements to strings in the result, so if you give it references they will lose the ability to be dereferenced.

Alternately:

```perl
my %hash = map { $_ => 1 } qw(1 2 3 a b c 2 3 4 b c d);
my @uniq = keys %hash;
```


Alternately:

```perl
my %seen;
@seen{qw(1 2 3 a b c 2 3 4 b c d)} = ();
my @uniq = keys %seen;
```



## Perl 6


```perl6
my @unique = [1, 2, 3, 5, 2, 4, 3, -3, 7, 5, 6].unique;
```

Or just make a set of it.

```perl6
set(1,2,3,5,2,4,3,-3,7,5,6).list
```



## Phix

Preserves order of first occurence. Applies to any data type. Should be pretty efficient, as the tagsort is O(n log n), then there are two O(n) loops.

```Phix
sequence test

function alpha(integer i, integer j)
integer res
    res = compare(test[i],test[j])
    if res=0 then
        res = compare(i,j)
    end if
    return res
end function

function unique(sequence s)
sequence at, valid = repeat(1,length(s))
object last, this
integer ai, nxt

    test = s
    at = custom_sort(routine_id("alpha"),tagset(length(test)))
    last = test[at[1]]
    for i=2 to length(at) do
        ai = at[i]
        this = test[ai]
        valid[ai] = last!=this
        last = this
    end for

    nxt = find(0,valid)
    if nxt then
        for i=nxt+1 to length(test) do
            if valid[i] then
                test[nxt] = test[i]
                nxt += 1
            end if
        end for
        test = test[1..nxt-1]
    end if
    return test
end function

?join(unique(split("Now is the time for all good men to come to the aid of the party.")))
?unique({1, 2, 1, 4, 5, 2, 15, 1, 3, 4})
?unique({1, 2, 3, "a", "b", "c", 2, 3, 4, "b", "c", "d"})
?unique({1,3,2,9,1,2,3,8,8,1,0,2})
?unique("chthonic eleemosynary paronomasiac")
```

```txt

"Now is the time for all good men to come aid of party."
{1,2,4,5,15,3}
{1,2,3,"a","b","c",4,"d"}
{1,3,2,9,8,0}
"chtoni elmsyarp"

```



## PHP


```php
$list = array(1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd');
$unique_list = array_unique($list);
```



## PicoLisp

There is a built-in function

```PicoLisp
(uniq (2 4 6 1 2 3 4 5 6 1 3 5))
```

```txt
-> (2 4 6 1 3 5)
```



## PL/I


```PL/I
*process mar(1,72);
remdup: Proc options(main);
   declare t(20) fixed initial (6, 6, 1, 5, 6, 2, 1, 7,
      5, 22, 4, 19, 1, 1, 6, 8, 9, 10, 11, 12);
   declare (i, j, k, n, e) fixed;

   put skip list ('Input:');
   put edit ((t(k) do k = 1 to hbound(t))) (skip,20(f(3)));
   n = hbound(t,1);
   i = 0;
outer:
   do k = 1 to n;
      e = t(k);
      do j = k-1 to 1 by -1;
          if e = t(j) then iterate outer;
      end;
      i = i + 1;
      t(i) = e;
   end;

   put skip list ('Unique elements are:');
   put edit ((t(k) do k = 1 to i)) (skip,20(f(3)));
end;
```

```txt
Input:
  6  6  1  5  6  2  1  7  5 22  4 19  1  1  6  8  9 10 11 12
Unique elements are:
  6  1  5  2  7 22  4 19  8  9 10 11 12
```



## Pop11



```pop11
;;; Initial array
lvars ar = {1 2 3 2 3 4};
;;; Create a hash table
lvars ht= newmapping([], 50, 0, true);
;;; Put all array as keys into the hash table
lvars i;
for i from 1 to length(ar) do
   1 -> ht(ar(i))
endfor;

;;; Collect keys into a list
lvars ls = [];
appdata(ht, procedure(x); cons(front(x), ls) -> ls; endprocedure);
```



## PostScript

```postscript

 [10 8 8 98 32 2 4 5 10 ] dup length dict begin  aload let* currentdict {pop} map end

```



## PowerShell

The common array for both approaches:

```powershell
$data = 1,2,3,1,2,3,4,1
```

Using a hash table to remove duplicates:

```powershell
$h = @{}
foreach ($x in $data) {
    $h[$x] = 1
}
$h.Keys
```

Sorting and removing duplicates along the way can be done with the <code>Sort-Object</code> cmdlet.

```powershell
$data | Sort-Object -Unique
```

Removing duplicates without sorting can be done with the <code>Select-Object</code> cmdlet.

```powershell
$data | Select-Object -Unique
```



## Prolog


```prolog
uniq(Data,Uniques) :- sort(Data,Uniques).
```


Example usage:

```prolog
?- uniq([1, 2, 3, 2, 3, 4],Xs).
Xs = [1, 2, 3, 4]
```



Because sort/2 is GNU prolog and not ISO here is an ISO compliant version:

```prolog
member1(X,[H|_]) :- X==H,!.
member1(X,[_|T]) :- member1(X,T).

distinct([],[]).
distinct([H|T],C) :- member1(H,T),!, distinct(T,C).
distinct([H|T],[H|C]) :- distinct(T,C).
```


Example usage:

```prolog
?- distinct([A, A, 1, 2, 3, 2, 3, 4],Xs).
Xs = [A, 1, 2, 3, 4]
```



## PureBasic

Task solved with the built in Hash Table which are called Maps in PureBasic

```PureBasic
NewMap MyElements.s()

For i=0 To 9              ;Mark 10 items at random, causing high risk of duplication items.
  x=Random(9)
  t$="Number "+str(x)+" is marked"
  MyElements(str(x))=t$   ; Add element 'X' to the hash list or overwrite if already included.
Next

ForEach MyElements()
  Debug MyElements()
Next
```

Output may look like this, e.g. duplicated items are automatically removed as they have the same hash value.
 Number 0 is marked
 Number 2 is marked
 Number 5 is marked
 Number 6 is marked


## Python

If all the elements are ''hashable'' (this excludes ''list'', ''dict'', ''set'', and other mutable types), we can use a <tt>set</tt>:

```python
items = [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
unique = list(set(items))
```


or if we want to keep the order of the elements


```python
items = [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
unique = []
helperset = set()
for x in items:
    if x not in helperset:
        unique.append(x)
        helperset.add(x)
```


If all the elements are comparable (i.e. <, >=, etc. operators; this works for ''list'', ''dict'', etc. but not for ''complex'' and many other types, including most user-defined types), we can sort and group:

```python
import itertools
items = [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
unique = [k for k,g in itertools.groupby(sorted(items))]
```


If both of the above fails, we have to use the brute-force method, which is inefficient:

```python
items = [1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']
unique = []
for x in items:
    if x not in unique:
        unique.append(x)
```



another way of removing duplicate elements from a list, while preserving the order would be to use OrderedDict module like so

```python

from collections import OrderedDict as od

print(list(od.fromkeys([1, 2, 3, 'a', 'b', 'c', 2, 3, 4, 'b', 'c', 'd']).keys()))

```


See also http://www.peterbe.com/plog/uniqifiers-benchmark and http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52560


We may also need to specify the particular type (or degree) of uniqueness and duplication that is at issue. Case-insensitive, with strings ? Unique with respect to a particular key in the case of dictionaries ?

One way to do this is to require an equality predicate, or perhaps a key function, in addition to a list to be pruned. For example, using itertools.groupby, at the cost of needing a sort and discarding order:

```python
from itertools import (groupby)


# nubByKey :: (a -> b) -> [a] -> [a]
def nubByKey(k, xs):
    return list(list(v)[0] for _, v in groupby(sorted(xs, key=k), key=k))


xs = [
    'apple', 'apple',
    'ampersand', 'aPPLE', 'Apple',
    'orange', 'ORANGE', 'Orange', 'orange', 'apple'
]
for k in [
    id,                      # default case sensitive uniqueness
    lambda x: x.lower(),     # case-insensitive uniqueness
    lambda x: x[0],          # unique first character (case-sensitive)
    lambda x: x[0].lower(),  # unique first character (case-insensitive)
]:
    print (
        nubByKey(k, xs)
    )
```

```txt
['apple', 'aPPLE', 'Apple', 'orange', 'ORANGE', 'Orange', 'ampersand']
['ampersand', 'apple', 'orange']
['Apple', 'ORANGE', 'apple', 'orange']
['apple', 'orange']
```


Or alternatively, using an equality predicate with a recursive function which scales less well, but does preserve order:


```python
# nubByEq :: (a -> a -> Bool) -> [a] -> [a]
def nubByEq(eq, xs):
    def go(yys, xxs):
        if yys:
            y = yys[0]
            ys = yys[1:]
            return go(ys, xxs) if (
                elemBy(eq, y, xxs)
            ) else (
                [y] + go(ys, [y] + xxs)
            )
        else:
            return []
    return go(xs, [])


# elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
def elemBy(eq, x, xs):
    if xs:
        return eq(x, xs[0]) or elemBy(eq, x, xs[1:])
    else:
        return False


xs = [
    'apple', 'apple',
    'ampersand', 'aPPLE', 'Apple',
    'orange', 'ORANGE', 'Orange', 'orange', 'apple'
]
for eq in [
    lambda a, b: a == b,                   # default case sensitive uniqueness
    lambda a, b: a.lower() == b.lower(),   # case-insensitive uniqueness
    lambda a, b: a[0] == b[0],             # unique first char (case-sensitive)
    lambda a, b: a[0].lower() == b[0].lower(),   # unique first char (any case)
]:
    print (
        nubByEq(eq, xs)
    )
```


A briefer definition of which might be in terms of ''filter'':

```python
# nubBy :: (a -> a -> Bool) -> [a] -> [a]
def nubBy(p, xs):
    def go(xs):
        if xs:
            x = xs[0]
            return [x] + go(
                list(filter(
                    lambda y: not p(x, y),
                    xs[1:]
                ))
            )
        else:
            return []
    return go(xs)
```


```txt
['apple', 'ampersand', 'aPPLE', 'Apple', 'orange', 'ORANGE', 'Orange']
['apple', 'ampersand', 'orange']
['apple', 'Apple', 'orange', 'ORANGE']
['apple', 'orange']
```



## Qi



```qi

(define remove-duplicates
  []    -> []
  [A|R] -> (remove-duplicates R) where (element? A R)
  [A|R] -> [A|(remove-duplicates R)])

(remove-duplicates [a b a a b b c d e])

```



## R



```r
items <- c(1,2,3,2,4,3,2)
unique (items)
```



## Racket


Using the built-in function

```Racket

-> (remove-duplicates '(2 1 3 2.0 a 4 5 b 4 3 a 7 1 3 x 2))
'(2 1 3 2.0 a 4 5 b 7 x)

```


Using a hash-table:

```Racket

(define (unique/hash lst)
  (hash-keys (for/hash ([x (in-list lst)]) (values x #t))))

```


Using a set:

```Racket

(define unique/set (compose1 set->list list->set))

```


A definition that works with arbitrary sequences and allows
specification of an equality predicate.


```Racket

(define (unique seq #:same-test [same? equal?])
  (for/fold ([res '()])
            ([x seq] #:unless (memf (curry same? x) res))
    (cons x res)))

```


```txt

-> (unique '(2 1 3 2.0 a 4 5 b 4 3 a 7 1 3 x 2))
'(1 2 3 a b x 4 5 7 2.0)
-> (unique '(2 1 3 2.0 4 5 4.0 3 7 1 3 2) #:same-test =)
'(7 5 4 3 1 2)
-> (unique #(2 1 3 2.0 4 5 4.0 3 7 1 3 2))
'(7 5 4 3 1 2)
-> (apply string (unique "absbabsbdbfbd"))
"fdsba"

```



## Raven



```raven
[ 1 2 3 'a' 'b' 'c' 2 3 4 'b' 'c' 'd' ] as items
items copy unique print

list (8 items)
 0 => 1
 1 => 2
 2 => 3
 3 => "a"
 4 => "b"
 5 => "c"
 6 => 4
 7 => "d"
```



## REBOL


```REBOL
print mold unique [1 $23.19 2 elbow 3 2 Bork 4 3 elbow 2 $23.19]
```


```txt
[1 $23.19 2 elbow 3 Bork 4]
```


## Red


```Red>>
 items: [1 "a" "c" 1 3 4 5 "c" 3 4 5]
>> unique items
== [1 "a" "c" 3 4 5]
```



## REXX

Note that in REXX, strings are quite literal.
:*   '''+7'''          is different from     '''7'''    (but compares numerically equal).
:*   '''00'''          is different from     '''0'''    (but compares numerically equal).
:*   '''─0'''          is different from     '''0'''    (but compares numerically equal).
:*   '''7.'''     is different from     '''7'''    (but compares numerically equal).
:*   '''Ab'''          is different from          '''AB'''   (but can compare equal if made ''case insensitive'').

Note that all the REXX examples below don't care what   ''type''   of element is used,
integer, floating point, character, binary,   ···

===version 1, using method 1===

```rexx
/*REXX program removes any duplicate elements (items) that are in a list (using a hash).*/
$= '2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 7 5 5 3 2 0 4.4 2'    /*item list.*/
say 'original list:'     $
say right( words($), 17, '─')    'words in the original list.'
z=;                              @.=             /*initialize the NEW list & index list.*/
     do j=1  for words($);       y= word($, j)   /*process the words (items) in the list*/
     if @.y==''  then z= z y;    @.y= .          /*Not duplicated? Add to Z list,@ array*/
     end   /*j*/
say
say 'modified list:'     space(z)                /*stick a fork in it,  we're all done. */
say right( words(z), 17, '─')    'words in the modified list.'
```

```txt

original list: 2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 7 5 5 3 2 0 4.4
──────────────23  words in the original list.

modified list: 2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 0 4.4
──────────────17  words in the modified list.

```


===version 2, using a modified method 3===
Instead of discard an element if it's a duplicated, it just doesn't add it to the new list.

Sorting of the list elements isn't necessary.

```rexx
/*REXX program removes any duplicate elements (items) that are in a list (using a list).*/
$= '2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 7 5 5 3 2 0 4.4 2'    /*item list.*/
say 'original list:'     $
say right( words($), 17, '─')    'words in the original list.'
#= words($)                                      /*process all the words in the list.   */
     do j=#  by -1  for #;     y= word($, j)                       /*get right-to-Left. */
     _= wordpos(y, $, j + 1);  if _\==0  then $= delword($, _, 1)  /*Dup? Then delete it*/
     end   /*j*/
say
say 'modified list:'     space($)                /*stick a fork in it,  we're all done. */
say right( words(z), 17, '─')    'words in the modified list.'
```

===version 3, using method 3===

```rexx
/*REXX program removes any duplicate elements (items) that are in a list (using 2 lists)*/
old = '2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 7 5 5 3 2 0 4.4 2'
say 'original list:'   old
say right( words(old), 17, '─')    'words in the original list.'
new=                                             /*start with a clean  (list)  slate.   */
     do j=1  for words(old);     _= word(old, j) /*process the words in the  OLD  list. */
     if wordpos(_, new)==0  then new= new _      /*Doesn't exist?  Then add word to NEW.*/
     end   /*j*/
say
say 'modified list:'     space(new)              /*stick a fork in it,  we're all done. */
say right( words(new), 17, '─')    'words in the modified list.'
```

===version 4, using method 1 (hash table) via REXX stems===

```rexx
/* REXX ************************************************************
* 26.11.2012 Walter Pachl
* added: show multiple occurrences
**********************************************************************/
old='2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 7 5 5',
    '3 2 0 4.4 2'
say 'old list='old
say 'words in the old list=' words(old)
new=''
found.=0
count.=0
Do While old<>''
  Parse Var old w old
  If found.w=0 Then Do
    new=new w
    found.w=1
    End
  count.w=count.w+1
  End
say 'new list='strip(new)
say 'words in the new list=' words(new)
Say 'Multiple occurrences:'
Say 'occ word'
Do While new<>''
  Parse Var new w new
  If count.w>1 Then
    Say right(count.w,3) w
  End
```

```txt

old list=2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 7 5 5 3 2 0 4.4 2
words in the old list= 23
new list=2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 0 4.4
words in the new list= 17
Multiple occurrences:
occ word
  3 2
  2 3
  3 5
  2 7

```



## Ring


```ring

list = ["Now", "is", "the", "time", "for", "all", "good", "men", "to", "come", "to", "the", "aid", "of", "the", "party."]
for i = 1 to len(list)
    for j = i + 1 to len(list)
        if list[i] = list[j] del(list, j) j-- ok
    next
next

for n = 1 to len(list)
    see list[n] + " "
next
see nl

```

Output:

```txt

Now is the time for all good men to come aid of party.

```



## Ruby

Ruby has an <code>Array#uniq</code> built-in method, which returns a new array by removing duplicate values in self.

```ruby
ary = [1,1,2,1,'redundant',[1,2,3],[1,2,3],'redundant']
p ary.uniq              # => [1, 2, "redundant", [1, 2, 3]]
```


You can also write your own uniq method.

```ruby
class Array
  # used Hash
  def uniq1
    each_with_object({}) {|elem, h| h[elem] = true}.keys
  end
  # sort (requires comparable)
  def uniq2
    sorted = sort
    pre = sorted.first
    sorted.each_with_object([pre]){|elem, uniq| uniq << (pre = elem) if elem != pre}
  end
  # go through the list
  def uniq3
    each_with_object([]) {|elem, uniq| uniq << elem unless uniq.include?(elem)}
  end
end

ary = [1,1,2,1,'redundant',[1,2,3],[1,2,3],'redundant']
p ary.uniq1             #=> [1, 2, "redundant", [1, 2, 3]]
p ary.uniq2 rescue nil  #   Error (not comparable)
p ary.uniq3             #=> [1, 2, "redundant", [1, 2, 3]]

ary = [1,2,3,7,6,5,2,3,4,5,6,1,1,1]
p ary.uniq1             #=> [1, 2, 3, 7, 6, 5, 4]
p ary.uniq2             #=> [1, 2, 3, 4, 5, 6, 7]
p ary.uniq3             #=> [1, 2, 3, 7, 6, 5, 4]
```


A version without implementing class declarations:

```ruby
def unique(array)
    pure = Array.new
    for i in array
        flag = false
        for j in pure
            flag = true if j==i
        end
        pure << i unless flag
    end
    return pure
end

unique ["hi","hey","hello","hi","hey","heyo"]   # => ["hi", "hey", "hello", "heyo"]
unique [1,2,3,4,1,2,3,5,1,2,3,4,5]              # => [1,2,3,4,5]
```


## Run BASIC


```runbasic
a$ = "2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 7 5 5 3 2 0 4.4 2"

for i = 1 to len(a$)
  a1$ = word$(a$,i)
  if a1$ = "" then exit for
  for i1 = 1 to len(b$)
    if a1$ = word$(b$,i1) then [nextWord]
  next i1
  b$ = b$ + a1$ + " "
[nextWord]
next i

 print "Dups:";a$
 print "No Dups:";b$
```


```txt
Dups:2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 7 5 5 3 2 0 4.4 2
No Dups:2 3 5 7 11 13 17 19 cats 222 -100.2 +11 1.1 +7 7. 0 4.4
```



## Rust


```rust
use std::collections::HashSet;
use std::hash::Hash;

fn remove_duplicate_elements_hashing<T: Hash + Eq>(elements: &mut Vec<T>) {
    let set: HashSet<_> = elements.drain(..).collect();
    elements.extend(set.into_iter());
}

fn remove_duplicate_elements_sorting<T: Ord>(elements: &mut Vec<T>) {
    elements.sort_unstable(); // order does not matter
    elements.dedup();
}

fn main() {
    let mut sample_elements = vec![0, 0, 1, 1, 2, 3, 2];
    println!("Before removal of duplicates : {:?}", sample_elements);
    remove_duplicate_elements_sorting(&mut sample_elements);
    println!("After removal of duplicates : {:?}", sample_elements);
}
```

```txt

Before removal of duplicates : [0, 0, 1, 1, 2, 3, 2]
After removal of duplicates : [1, 0, 3, 2]

```



## Scala


```scala
val list = List(1,2,3,4,2,3,4,99)
val l2 = list.distinct
// l2: scala.List[scala.Int] = List(1,2,3,4,99)

val arr = Array(1,2,3,4,2,3,4,99)
val arr2 = arr.distinct
// arr2: Array[Int] = Array(1, 2, 3, 4, 99)

```



## Scheme


```scheme
(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

(remove-duplicates '(1 2 1 3 2 4 5))
```



```scheme
(1 3 2 4 5)
```


Alternative approach:

```scheme
(define (remove-duplicates l)
  (do ((a '() (if (member (car l) a) a (cons (car l) a)))
       (l l (cdr l)))
    ((null? l) (reverse a))))

(remove-duplicates '(1 2 1 3 2 4 5))
```



```scheme
(1 2 3 4 5)
```


The function 'delete-duplicates' is also available in srfi-1.


## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const array integer: data is [] (1, 3, 2, 9, 1, 2, 3, 8, 8, 1, 0, 2);
    var set of integer: dataSet is (set of integer).value;
    var integer: number is 0;
  begin
    for number range data do
      incl(dataSet, number);
    end for;
    writeln(dataSet);
  end func;
```


```txt

{0, 1, 2, 3, 8, 9}

```



## SETL


```SETL
items := [0,7,6,6,4,9,7,1,2,3,2];
print(unique(items));
```

Output in arbitrary order (convert tuple->set then set->tuple):

```SETL
proc unique(items);
  return [item: item in {item: item in items}];
end proc;
```


Preserving source order

```SETL
proc unique(items);
  seen := {};
  return [item: item in items, nps in {#seen} | #(seen with:= item) > nps];
end proc;
```




```SETL
proc unique(items);
  seen := {};
  return [item: item in items, nps in {#seen} | #(seen with:= item) > nps];
end proc;
```


```SETL
items := [0,7,6,6,4,9,7,1,2,3,2];
print(unique(items));
```

Output in arbitrary order (convert tuple->set then set->tuple):

```SETL
proc unique(items);
  return [item: item in {item: item in items}];
end proc;
```



## SETL4



```SETL4

set = new('set')
* Add all the elements of the array to the set.
add(set,array)

```



## Sidef


```ruby
var ary = [1,1,2,1,'redundant',[1,2,3],[1,2,3],'redundant'];
say ary.uniq.dump;
say ary.last_uniq.dump;
```

```txt
[1, 2, 'redundant', [1, 2, 3]]
[2, 1, [1, 2, 3], 'redundant']
```



## Slate



```slate
[|:s| #(1 2 3 4 1 2 3 4) >> s] writingAs: Set.

"==> {"Set traitsWindow" 1. 2. 3. 4}"
```



## Smalltalk



```smalltalk
"Example of creating a collection"
|a|
a := #( 1 1 2 'hello' 'world' #symbol #another 2 'hello' #symbol ).
a asSet.
```

```txt
Set (1 2 #symbol 'world' #another 'hello' )
```


the above has the disadvantage of loosing the original order (because Sets are unordered, and the hashing shuffles elements into an arbitrary order).
When tried, I got:

```txt
Set('world' 1 #another 'hello' #symbol 2)
```

on my system. This can be avoided by using an ordered set (which has also O(n) complexity) as below:
```smalltalk
|a|
a := #( 1 1 2 'hello' 'world' #symbol #another 2 'hello' #symbol ).
a asOrderedSet.
```

```txt
OrderedSet(1 2 'hello' 'world' #symbol #another)
```



## Sparkling


```sparkling
function undupe(arr) {
	var t = {};
	foreach(arr, function(key, val) {
		t[val] = true;
	});

	var r = {};
	foreach(t, function(key) {
		r[sizeof r] = key;
	});

	return r;
}
```



## Stata


### Duplicates in a dataset

Stata can report duplicate lines, or remove them. See '''[http://www.stata.com/help.cgi?duplicates duplicates]''' in Stata help.


```stata
. clear all
. input x y
1 1
1 1
1 2
2 1
2 2
1 1
2 1
2 1
1 2
2 2
end

. duplicates drop x y, force
. list

     +-------+
     | x   y |
     |-------|
  1. | 1   1 |
  2. | 1   2 |
  3. | 2   1 |
  4. | 2   2 |
     +-------+
```



### Mata


The '''[http://www.stata.com/help.cgi?mf_uniqrows uniqrows]''' function removes duplicate rows from a matrix.


```stata
. mata
: a=1,1\1,1\1,2\2,1\2,2\1,1\2,1\2,1\1,2\2,2

: a
        1   2
     +---------+
   1 |  1   1  |
   2 |  1   1  |
   3 |  1   2  |
   4 |  2   1  |
   5 |  2   2  |
   6 |  1   1  |
   7 |  2   1  |
   8 |  2   1  |
   9 |  1   2  |
  10 |  2   2  |
     +---------+

: uniqrows(a)
       1   2
    +---------+
  1 |  1   1  |
  2 |  1   2  |
  3 |  2   1  |
  4 |  2   2  |
    +---------+
```



## Swift

Requires elements to be hashable:
```swift
println(Array(Set([3,2,1,2,3,4])))
```

```txt
[2, 3, 1, 4]
```


Another solution (preserves order of first occurrence). Also requires elements to be hashable:
```swift>func uniq<T: Hashable
(lst: [T]) -> [T] {
  var seen = Set<T>(minimumCapacity: lst.count)
  return lst.filter { x in
    let unseen = !seen.contains(x)
    seen.insert(x)
    return unseen
  }
}

println(uniq([3,2,1,2,3,4]))
```

```txt
[3, 2, 1, 4]
```


Only requires elements to be equatable, but runs in O(n^2):

```swift>func uniq<T: Equatable
(lst: [T]) -> [T] {
  var seen = [T]()
  return lst.filter { x in
    let unseen = find(seen, x) == nil
    if (unseen) {
      seen.append(x)
    }
    return unseen
  }
}

println(uniq([3,2,1,2,3,4]))
```

```txt
[3, 2, 1, 4]
```



## Tcl

The concept of an "array" in Tcl is strictly associative - and since there cannot be duplicate keys, there cannot be a redundant element in an array.
What is called "array" in many other languages is probably better represented by the "list" in Tcl (as in LISP).
With the correct option, the <code>lsort</code> command will remove duplicates.

```tcl
set result [lsort -unique $listname]
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
list_old="b'A'A'5'1'2'3'2'3'4"
list_sort=MIXED_SORT (list_old)
list_new=REDUCE (list_sort)
PRINT list_old
PRINT list_new

```

{{out}} (sorted)

```txt

b'A'A'5'1'2'3'2'3'4
1'2'3'4'5'A'b

```

or

```tuscript

$$ MODE TUSCRIPT
list_old="b'A'A'5'1'2'3'2'3'4"
DICT list CREATE
LOOP l=list_old
DICT list LOOKUP l,num
IF (num==0) DICT list ADD l
ENDLOOP
DICT list unload list
list_new=JOIN (list)
PRINT list_old
PRINT list_new

```

```txt

b'A'A'5'1'2'3'2'3'4
b'A'5'1'2'3'4

```



## UnixPipes

Assuming a sequence is represented by lines in a file.

```bash
bash$ # original list
bash$ printf '6\n2\n3\n6\n4\n2\n'
6
2
3
6
4
2
bash$ # made uniq
bash$ printf '6\n2\n3\n6\n4\n2\n'|sort -n|uniq
2
3
4
6
bash$
```


or


```bash
bash$ # made uniq
bash$ printf '6\n2\n3\n6\n4\n2\n'|sort -nu
2
3
4
6
bash$
```



## Ursala

The algorithm is to partition the list by equality and take one
representative from each class, which can be done by letting the built in partition
operator, |=, use its default comparison relation. This works
on lists of any type including character strings but the comparison
is based only on structural equivalence. It's up to the programmer
to decide whether that's a relevant criterion for equivalence or else
specify a better one.

```Ursala
#cast %s

example = |=hS& 'mississippi'
```

```txt
'mspi'
```



## VBA

Hash Table Approach
Input list (variant : Long, Double, Boolean and Strings) :
Array(1.23456789101112E+16, True, False, True, "Alpha", 1, 235, 4, 1.25, 1.25, "Beta", 1.23456789101112E+16, "Delta", "Alpha", "Charlie", 1, 2, "Foxtrot", "Foxtrot", "Alpha", 235)

```vb

Option Explicit

Sub Main()
Dim myArr() As Variant, i As Long

    myArr = Remove_Duplicate(Array(1.23456789101112E+16, True, False, True, "Alpha", 1, 235, 4, 1.25, 1.25, "Beta", 1.23456789101112E+16, "Delta", "Alpha", "Charlie", 1, 2, "Foxtrot", "Foxtrot", "Alpha", 235))
'return :
    For i = LBound(myArr) To UBound(myArr)
        Debug.Print myArr(i)
    Next
End Sub

Private Function Remove_Duplicate(Arr As Variant) As Variant()
Dim myColl As New Collection, Temp() As Variant, i As Long, cpt As Long

    ReDim Temp(UBound(Arr))
    For i = LBound(Arr) To UBound(Arr)
        On Error Resume Next
        myColl.Add CStr(Arr(i)), CStr(Arr(i))
        If Err.Number > 0 Then
            On Error GoTo 0
        Else
            Temp(cpt) = Arr(i)
            cpt = cpt + 1
        End If
    Next i
    ReDim Preserve Temp(cpt - 1)
    Remove_Duplicate = Temp
End Function
```

```txt
 1.23456789101112E+16
True
False
Alpha
 1
 235
 4
 1.25
Beta
Delta
Charlie
 2
Foxtrot
```




## VBScript

Hash Table Approach

```vb

Function remove_duplicates(list)
	arr = Split(list,",")
	Set dict = CreateObject("Scripting.Dictionary")
	For i = 0 To UBound(arr)
		If dict.Exists(arr(i)) = False Then
			dict.Add arr(i),""
		End If
	Next
	For Each key In dict.Keys
		tmp = tmp & key & ","
	Next
	remove_duplicates = Left(tmp,Len(tmp)-1)
End Function

WScript.Echo remove_duplicates("a,a,b,b,c,d,e,d,f,f,f,g,h")

```


```txt
a,b,c,d,e,f,g,h
```



## Vedit macro language


The input "array" is an edit buffer where each line is one element.

```vedit
Sort(0, File_Size)                                          // sort the data
While(Replace("^(.*)\N\1$", "\1", REGEXP+BEGIN+NOERR)){}    // remove duplicates
```



## Vim Script



```vim
call filter(list, 'count(list, v:val) == 1')
```



## Wart


```python
def (dedup l)
  let exists (table)
    collect+each x l
      unless exists.x
        yield x
      exists.x <- 1
```


```txt
dedup '(1 3 2 9 1 2 3 8 8 1 0 2)
=> (1 3 2 9 8 0)
```



## Visual FoxPro


```vfp

LOCAL i As Integer, n As Integer, lcOut As String
CLOSE DATABASES ALL
CLEAR
CREATE CURSOR nums (num I)
INDEX ON num TAG num COLLATE "Machine"
SET ORDER TO 0
n = 50
RAND(-1)
FOR i = 1 TO n
    INSERT INTO nums VALUES (RanInt(1, 10))
ENDFOR
SELECT num, COUNT(num) As cnt FROM nums ;
GROUP BY num INTO CURSOR grouped
LIST OFF TO FILE grouped.txt NOCONSOLE
lcOut = ""
SCAN
    lcOut = lcOut + TRANSFORM(num) + ","
ENDSCAN
lcOut = LEFT(lcOut, LEN(lcOut)-1)
? lcOut

FUNCTION RanInt(tnLow As Integer, tnHigh As Integer) As Integer
RETURN INT((tnHigh - tnLow + 1)*RAND() + tnLow)
ENDFUNC

```

```txt

NUM          COUNT
  1              6
  2              5
  3              6
  4              8
  5              4
  6              3
  7              8
  8              7
  9              3

Unique Values: 1,2,3,4,5,6,7,8,9

```




## Wortel


```wortel
@uniq [1 2 3 2 1 2 3] ; returns [1 2 3]
```



## XPL0


```XPL0
code Text=12;           \built-in routine to display a string of characters
string 0;               \use zero-terminated strings (not MSb terminated)

func StrLen(S);         \Return number of characters in an ASCIIZ string
char S;
int  I;
for I:= 0, -1>>1-1 do   \(limit = 2,147,483,646 if 32 bit, or 32766 if 16 bit)
        if S(I) = 0 then return I;

func Unique(S);         \Remove duplicate bytes from string
char S;
int  I, J, K, L;
[L:= StrLen(S);                         \string length
for I:= 0 to L-1 do                     \for all characters in string...
    for J:= I+1 to L-1 do               \scan rest of string for duplicates
        if S(I) = S(J) then             \if duplicate then
            [for K:= J+1 to L do        \ shift rest of string down (including
                S(K-1):= S(K);          \ terminating zero)
            L:= L-1                     \ string is now one character shorter
            ];
return S;                               \return pointer to string
];

Text(0, Unique("Pack my box with five dozen liquor jugs."))
```


```txt

Pack myboxwithfvedznlqurjgs.

```



## Yabasic


```Yabasic
data "Now", "is", "the", "time", "for", "all", "good", "men", "to", "come", "to", "the", "aid", "of", "the", "party.", ""

do
    read p$
    if p$ = "" break
    if not instr(r$, p$) r$ = r$ + p$ + " "
loop

print r$
```



## zkl

Using built ins:

```zkl
zkl: Utils.Helpers.listUnique(T(1,3,2,9,1,2,3,8,8,"8",1,0,2,"8"))
L(1,3,2,9,8,"8",0)
zkl: "1,3,2,9,1,2,3,8,8,1,0,2".unique()
,012389
```

Where listUnique is brute force:

```zkl
fcn listUnique(xs){
   xs.reduce(fcn(us,s){us.holds(s) and us or us.append(s)},L()) }
```

