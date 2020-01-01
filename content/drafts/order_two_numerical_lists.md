+++
title = "Order two numerical lists"
description = ""
date = 2019-07-09T19:43:43Z
aliases = []
[extra]
id = 10957
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting}}

Write a function that orders two lists or arrays filled with numbers.
The function should accept two lists as arguments and return <code>true</code> if the first list should be ordered before the second, and <code>false</code> otherwise.

The order is determined by [[wp:Lexicographical order#Ordering of sequences of various lengths|lexicographic order]]: Comparing the first element of each list.
If the first elements are equal, then the second elements should be compared, and so on, until one of the list has no more elements.
If the first list runs out of elements the result is <code>true</code>.
If the second list or both run out of elements the result is <code>false</code>.

<small>Note: further clarification of lexicographical ordering is expounded on the talk page [[Talk:Order_two_numerical_lists#Lexicographic_order|here]] and [[Talk:Order_two_numerical_lists#Is_the_task_statement_consistent.3F|here]].</small>


## ACL2


The built-in <tt>lexorder</tt> does this.


```txt
ACL2 !>(lexorder '(1 2 3) '(1 2 3 4))
T
ACL2 !>(lexorder '(1 2 4) '(1 2 3))
NIL
```



## Ada

This is already implemented in the built-in comparison operators for arrays of types that have a direct ordering.
This also includes arrays of user defined types, using the type definition order from smallest to largest.
Demonstrated in the program below:

```Ada

with Ada.Text_IO;  use Ada.Text_IO;
procedure Order is

   type IntArray is array (Positive range <>) of Integer;
   List1 : IntArray := (1, 2, 3, 4, 5);
   List2 : IntArray := (1, 2, 1, 5, 2, 2);
   List3 : IntArray := (1, 2, 1, 5, 2);
   List4 : IntArray := (1, 2, 1, 5, 2);

   type Animal is (Rat, Cat, Elephant);
   type AnimalArray is array (Positive range <>) of Animal;
   List5 : AnimalArray := (Cat, Elephant, Rat, Cat);
   List6 : AnimalArray := (Cat, Elephant, Rat);
   List7 : AnimalArray := (Cat, Cat, Elephant);

begin
   Put_Line (Boolean'Image (List1 > List2)); --  True
   Put_Line (Boolean'Image (List2 > List3)); --  True
   Put_Line (Boolean'Image (List3 > List4)); --  False, equal
   Put_Line (Boolean'Image (List5 > List6)); --  True
   Put_Line (Boolean'Image (List6 > List7)); --  True
end Order;

```

{{out}}

```txt

TRUE
TRUE
FALSE
TRUE
TRUE

```




## Aime


```aime
ordl(list a, b)
{
    integer i, l, o;

    l = min(~a, ~b);
    i = 0;
    while (i < l) {
        if (a[i] != b[i]) {
            o = a[i] < b[i];
            break;
        }

        i += 1;
    }

    i < l ? o : ~a <= ~b;
}

main(void)
{
    o_(ordl(list(1, 2), list(1, 2)), "\n");
    o_(ordl(list(1e2, 2), list(1e2, 2, 3)), "\n");
    o_(ordl(list(1, 2, 3), list(1, 2)), "\n");
    o_(ordl(list(.5, 4), list(.5, 2)), "\n");
    o_(ordl(list(1, 4, 2, 3), list(1, 4, 2.1, 3)), "\n");

    0;
}
```



## AppleScript

{{Trans|JavaScript}}


<= is not defined over lists in AppleScript

```AppleScript
-- <= for lists
-- compare :: [a] -> [a] -> Bool
on compare(xs, ys)
    if length of xs = 0 then
        true
    else
        if length of ys = 0 then
            false
        else
            set {hx, txs} to uncons(xs)
            set {hy, tys} to uncons(ys)

            if hx = hy then
                compare(txs, tys)
            else
                hx < hy
            end if
        end if
    end if
end compare



-- TEST
on run

    {compare([1, 2, 1, 3, 2], [1, 2, 0, 4, 4, 0, 0, 0]), ¬
        compare([1, 2, 0, 4, 4, 0, 0, 0], [1, 2, 1, 3, 2])}

end run


---------------------------------------------------------------------------

-- GENERIC FUNCTION

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    if length of xs > 0 then
        {item 1 of xs, rest of xs}
    else
        missing value
    end if
end uncons
```


{{Out}}

```AppleScript
{false, true}
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program orderlist.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessResult1:      .asciz "List1 < List2 \n"           @ message result
szMessResult2:      .asciz "List1 => List2 \n"           @ message result
szCarriageReturn:  .asciz "\n"

iTabList1:         .int  1,2,3,4,5
.equ NBELEMENTS1,   (. - iTabList1) /4
iTabList2:         .int  1,2,1,5,2,2
.equ NBELEMENTS2,   (. - iTabList2) /4
iTabList3:         .int  1,2,3,4,5
.equ NBELEMENTS3,   (. - iTabList3) /4
iTabList4:         .int  1,2,3,4,5,6
.equ NBELEMENTS4,   (. - iTabList4) /4
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                       @ entry of program
    ldr r0,iAdriTabList1
    mov r1,#NBELEMENTS1
    ldr r2,iAdriTabList2
    mov r3,#NBELEMENTS2
    bl listeOrder
    cmp r0,#0                               @ false ?
    beq 1f                                  @ yes
    ldr r0,iAdrszMessResult1                @ list 1 < list 2
    bl affichageMess                        @ display message
    b 2f
1:
    ldr r0,iAdrszMessResult2
    bl affichageMess                        @ display message

2:
    ldr r0,iAdriTabList1
    mov r1,#NBELEMENTS1
    ldr r2,iAdriTabList3
    mov r3,#NBELEMENTS3
    bl listeOrder
    cmp r0,#0                               @ false ?
    beq 3f                                  @ yes
    ldr r0,iAdrszMessResult1                @ list 1 < list 2
    bl affichageMess                        @ display message
    b 4f
3:
    ldr r0,iAdrszMessResult2
    bl affichageMess                        @ display message
4:
    ldr r0,iAdriTabList1
    mov r1,#NBELEMENTS1
    ldr r2,iAdriTabList4
    mov r3,#NBELEMENTS4
    bl listeOrder
    cmp r0,#0                               @ false ?
    beq 5f                                  @ yes
    ldr r0,iAdrszMessResult1                @ list 1 < list 2
    bl affichageMess                        @ display message
    b 6f
5:
    ldr r0,iAdrszMessResult2
    bl affichageMess                        @ display message
6:
100:                                        @ standard end of the program
    mov r0, #0                              @ return code
    mov r7, #EXIT                           @ request to exit program
    svc #0                                  @ perform the system call
iAdriTabList1:             .int iTabList1
iAdriTabList2:             .int iTabList2
iAdriTabList3:             .int iTabList3
iAdriTabList4:             .int iTabList4
iAdrszMessResult1:        .int szMessResult1
iAdrszMessResult2:        .int szMessResult2
iAdrszCarriageReturn:     .int szCarriageReturn
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of list 1 */
/* r1 contains list 1 size           */
/* r2 contains the address of list 2 */
/* r3 contains list 2 size           */
/* r0 returns 1 if list1 < list2     */
/* r0 returns 0 else                 */
listeOrder:
    push {r1-r7,lr}                   @ save  registres
    cmp r1,#0                         @ list 1 size = zero ?
    moveq r0,#-1                      @ yes -> error
    beq 100f
    cmp r3,#0                         @ list 2 size = zero ?
    moveq r0,#-2                      @ yes -> error
    beq 100f
    mov r4,#0                         @ index list 1
    mov r5,#0                         @ index list 2
1:
    ldr r6,[r0,r4,lsl #2]             @ load list 1 element
    ldr r7,[r2,r5,lsl #2]             @ load list 2 element
    cmp r6,r7                         @ compar
    movgt r0,#0                       @ list 1 > list 2 ?
    bgt 100f
    beq 2f                            @ list 1 = list 2
    add r4,#1                         @ increment index 1
    cmp r4,r1                         @ end list ?
    movge r0,#1                       @ yes -> ok list 1 < list 2
    bge 100f
    b 1b                              @ else loop
2:
    add r4,#1                         @ increment index 1
    cmp r4,r1                         @ end list ?
    bge 3f                            @ yes -> verif size
    add r5,#1                         @ else increment index 2
    cmp r5,r3                         @ end list 2 ?
    movge r0,#0                       @ yes -> list 2 < list 1
    bge 100f
    b 1b                              @ else loop
3:
   cmp r1,r3                          @ compar size
   movge r0,#0                        @ list 2 < list 1
   movlt r0,#1                        @ list 1 < list 2
100:
    pop {r1-r7,lr}                    @ restaur registers
    bx lr                             @ return
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                   @ save  registres
    mov r2,#0                               @ counter length
1:                                          @ loop length calculation
    ldrb r1,[r0,r2]                         @ read octet start position + index
    cmp r1,#0                               @ if 0 its over
    addne r2,r2,#1                          @ else add 1 in the length
    bne 1b                                  @ and loop
                                            @ so here r2 contains the length of the message
    mov r1,r0                               @ address message in r1
    mov r0,#STDOUT                          @ code to write to the standard output Linux
    mov r7, #WRITE                          @ code call system "write"
    svc #0                                  @ call systeme
    pop {r0,r1,r2,r7,lr}                    @ restaur registers */
    bx lr                                   @ return

```


## AutoHotkey

{{works with|AutoHotkey_L}}
The function is overkill as we can just compare the list's ObjMaxIndex()

```AHK
List1 := [1,2,1,3,2]
List2 := [1,2,0,4,4,0,0,0]
MsgBox % order(List1, List2)

order(L1, L2){
	return L1.MaxIndex() < L2.MaxIndex()
}
```



## AWK


```AWK

# syntax: GAWK -f ORDER_TWO_NUMERICAL_LISTS.AWK
BEGIN {
    split("1,2,1,5,2",list1,",")
    split("1,2,1,5,2,2",list2,",")
    split("1,2,3,4,5",list3,",")
    split("1,2,3,4,5",list4,",")
    x = compare_array(list1,list2) ? "<" : ">=" ; printf("list1%slist2\n",x)
    x = compare_array(list2,list3) ? "<" : ">=" ; printf("list2%slist3\n",x)
    x = compare_array(list3,list4) ? "<" : ">=" ; printf("list3%slist4\n",x)
    exit(0)
}
function compare_array(arr1,arr2,  ans,i) {
    ans = 0
    for (i=1; i<=length(arr1); i++) {
      if (arr1[i] != arr2[i]) {
        ans = 1
        break
      }
    }
    if (length(arr1) != length(arr2)) {
      ans = 1
    }
    return(ans)
}

```

{{out}}

```txt

list1<list2
list2<list3
list3>=list4

```



## BBC BASIC

'Ordered before' means 'less than' (see [[Talk:Order_two_numerical_lists|talk page]]).

```bbcbasic
      DIM list1(4) : list1() = 1, 2, 1, 5, 2
      DIM list2(5) : list2() = 1, 2, 1, 5, 2, 2
      DIM list3(4) : list3() = 1, 2, 3, 4, 5
      DIM list4(4) : list4() = 1, 2, 3, 4, 5

      IF FNorder(list1(), list2()) PRINT "list1<list2" ELSE PRINT "list1>=list2"
      IF FNorder(list2(), list3()) PRINT "list2<list3" ELSE PRINT "list2>=list3"
      IF FNorder(list3(), list4()) PRINT "list3<list4" ELSE PRINT "list3>=list4"
      END

      DEF FNorder(list1(), list2())
      LOCAL i%, l1%, l2%
      l1% = DIM(list1(),1) : l2% = DIM(list2(),1)
      WHILE list1(i%) = list2(i%) AND i% < l1% AND i% < l2%
        i% += 1
      ENDWHILE
      IF list1(i%) < list2(i%) THEN = TRUE
      IF list1(i%) > list2(i%) THEN = FALSE
      = l1% < l2%
```

{{out}}

```txt

list1<list2
list2<list3
list3>=list4

```



## Bracmat

When evaluating a sum or a product, Bracmat creates an expression with a canonical order, which happens to be compatible with the order defined in this task.
In a pattern, only a sum or product on the left hand side (lhs) of the match (<code>:</code>) operator is evaluated.
In the solution below we match a composition of the two function arguments into a sum of two terms with itself.
If the match expression succeeds, the lhs must already have been in canonical order before evaluation, which means that the first argument is smaller than the second argument.
In that case the function outputs FALSE.
Notice that if the arguments are the same, evaluation of the sum produces the product of one of the terms and a factor two. This complicates the pattern a bit.

```bracmat
(  1 2 3 4 5:?List1
& 1 2 1 5 2 2:?List2
& 1 2 1 5 2:?List3
& 1 2 1 5 2:?List4
& Cat Elephant Rat Cat:?List5
& Cat Elephant Rat:?List6
& Cat Cat Elephant:?List7
& ( gt
  =   first second
    .   !arg:(?first,?second)
      &   out
        $ (     (.!first)+(.!second)
              : ((.!first)+(.!second)|2*(.!first))
            & FALSE
          | TRUE
          )
  )
& gt$(!List1,!List2)
& gt$(!List2,!List3)
& gt$(!List3,!List4)
& gt$(!List4,!List5)
& gt$(!List5,!List6)
& gt$(!List6,!List7)
);
```

{{out}}

```txt
TRUE
TRUE
FALSE
FALSE
TRUE
TRUE
```



## C


```c
int list_cmp(int *a, int la, int *b, int lb)
{
	int i, l = la;
	if (l > lb) l = lb;
	for (i = 0; i < l; i++) {
		if (a[i] == b[i]) continue;
		return (a[i] > b[i]) ? 1 : -1;
	}
	if (la == lb) return 0;
	return la > lb ? 1 : -1;
}
```

This funciton returns one of three states, not a boolean.  One can define boolean comparisons, such as <code>list_less_or_eq</code>, based on it:
```c
#define list_less_or_eq(a,b,c,d) (list_cmp(a,b,c,d) != 1)
```


## C#

```c#
namespace RosettaCode.OrderTwoNumericalLists
{
    using System;
    using System.Collections.Generic;

    internal static class Program
    {
        private static bool IsLessThan(this IEnumerable<int> enumerable,
            IEnumerable<int> otherEnumerable)
        {
            using (
                IEnumerator<int> enumerator = enumerable.GetEnumerator(),
                    otherEnumerator = otherEnumerable.GetEnumerator())
            {
                while (true)
                {
                    if (!otherEnumerator.MoveNext())
                    {
                        return false;
                    }

                    if (!enumerator.MoveNext())
                    {
                        return true;
                    }

                    if (enumerator.Current == otherEnumerator.Current)
                    {
                        continue;
                    }

                    return enumerator.Current < otherEnumerator.Current;
                }
            }
        }

        private static void Main()
        {
            Console.WriteLine(
                new[] {1, 2, 1, 3, 2}.IsLessThan(new[] {1, 2, 0, 4, 4, 0, 0, 0}));
        }
    }
}
```

{{out}}

```txt
False
```



## C++

The built-in comparison operators already do this:

```cpp
#include <iostream>
#include <vector>

int main() {
  std::vector<int> a;
  a.push_back(1);
  a.push_back(2);
  a.push_back(1);
  a.push_back(3);
  a.push_back(2);
  std::vector<int> b;
  b.push_back(1);
  b.push_back(2);
  b.push_back(0);
  b.push_back(4);
  b.push_back(4);
  b.push_back(0);
  b.push_back(0);
  b.push_back(0);

  std::cout << std::boolalpha << (a < b) << std::endl; // prints "false"
  return 0;
}
```



## clojure



```clojure

(defn lex? [a b]
  (compare a b))

```



## Common Lisp


```Lisp
(defun list< (a b)
  (cond ((not b) nil)
        ((not a) t)
        ((= (first a) (first b))
         (list< (rest a) (rest b)))
        (t (< (first a) (first b)))))
```


Alternate version


```Lisp
(defun list< (a b)
  (let ((x (find-if-not #'zerop (mapcar #'- a b))))
    (if x (minusp x) (< (length a) (length b)))))
```



## D

The built-in comparison operators already do this:

```d
void main() {
    assert([1,2,1,3,2] >= [1,2,0,4,4,0,0,0]);
}
```



## Ela



```ela
[] <. _ = true
_ <. [] = false
(x::xs) <. (y::ys) | x == y = xs <. ys
                   | else   = x < y

[1,2,1,3,2] <. [1,2,0,4,4,0,0,0]
```



## Elixir

The built-in comparison functions already do this (not only for lists of numbers, but for any arbitrary data type).

```elixir
iex(1)> [1,2,3] < [1,2,3,4]
true
iex(2)> [1,2,3] < [1,2,4]
true
```



## Erlang

Builtin. Example use from Erlang shell:

```Erlang

5> [1,2,3] < [1,2,3,4].
true
6> [1,2,3] < [1,2,4].
true

```


=={{header|F_Sharp|F#}}==
By using the Collection.Seq Module the static method Seq.compareWith fits our needs.

```fsharp
let inline cmp x y = if x < y then -1 else if x = y then 0 else 1
let before (s1 : seq<'a>) (s2 : seq<'a>) = (Seq.compareWith cmp s1 s2) < 0

[
    ([0], []);
    ([], []);
    ([], [0]);
    ([-1], [0]);
    ([0], [0]);
    ([0], [-1]);
    ([0], [0; -1]);
    ([0], [0; 0]);
    ([0], [0; 1]);
    ([0; -1], [0]);
    ([0; 0], [0]);
    ([0; 0], [1]);
]
|> List.iter (fun (x, y) -> printf "%A %s %A\n" x (if before x y then "< " else ">=") y)
```

{{out}}

```txt
[0] >= []
[] >= []
[] <  [0]
[-1] <  [0]
[0] >= [0]
[0] >= [-1]
[0] <  [0; -1]
[0] <  [0; 0]
[0] <  [0; 1]
[0; -1] >= [0]
[0; 0] >= [0]
[0; 0] <  [1]
```



## Factor

All [[sequence]]s respond to words in the [http://docs.factorcode.org/content/vocab-math.order.html ''math.order''] vocabulary.

 IN: scratchpad '''{ 2 3 } { 2 5 } before? .'''
 t


## Go


```go
package main

import "fmt"

// If your numbers happen to be in the range of Unicode code points (0 to 0x10ffff), this function
// satisfies the task:
func lessRune(a, b []rune) bool {
    return string(a) < string(b) // see also bytes.Compare
}

// Otherwise, the following function satisfies the task for all integer
// and floating point types, by changing the type definition appropriately.
type numericType int

func lessNT(a, b []numericType) bool {
    l := len(a)
    if len(b) < l {
        l = len(b)
    }
    for i := 0; i < l; i++ {
        if a[i] != b[i] {
            return a[i] < b[i]
        }
    }
    return l < len(b)
}

var testCases = [][][]numericType{
    {{0}, {}},
    {{}, {}},
    {{}, {0}},

    {{-1}, {0}},
    {{0}, {0}},
    {{0}, {-1}},

    {{0}, {0, -1}},
    {{0}, {0, 0}},
    {{0}, {0, 1}},
    {{0, -1}, {0}},
    {{0, 0}, {0}},
    {{0, 0}, {1}},
}

func main() {
    // demonstrate the general function
    for _, tc := range testCases {
        fmt.Printf("order %6s before %6s : %t\n",
            fmt.Sprintf("%v", tc[0]),
            fmt.Sprintf("%v", tc[1]),
            lessNT(tc[0], tc[1]))
    }
    fmt.Println()

    // demonstrate that the byte specific function gives identical results
    // by offsetting test data to a printable range of characters.
    for _, tc := range testCases {
        a := toByte(tc[0])
        b := toByte(tc[1])
        fmt.Printf("order %6q before %6q : %t\n",
            string(a),
            string(b),
            lessByte(a, b))
    }
}

func toByte(a []numericType) []byte {
    b := make([]byte, len(a))
    for i, n := range a {
        b[i] = 'b' + byte(n)
    }
    return b
}
```

{{out}}

```txt

order    [0] before     [] : false
order     [] before     [] : false
order     [] before    [0] : true
order   [-1] before    [0] : true
order    [0] before    [0] : false
order    [0] before   [-1] : false
order    [0] before [0 -1] : true
order    [0] before  [0 0] : true
order    [0] before  [0 1] : true
order [0 -1] before    [0] : false
order  [0 0] before    [0] : false
order  [0 0] before    [1] : true

order    "b" before     "" : false
order     "" before     "" : false
order     "" before    "b" : true
order    "a" before    "b" : true
order    "b" before    "b" : false
order    "b" before    "a" : false
order    "b" before   "ba" : true
order    "b" before   "bb" : true
order    "b" before   "bc" : true
order   "ba" before    "b" : false
order   "bb" before    "b" : false
order   "bb" before    "c" : true

```



## Groovy

Solution:

```groovy
class CList extends ArrayList implements Comparable {
    CList() { }
    CList(Collection c) { super(c) }
    int compareTo(Object that) {
        assert that instanceof List
        def n = [this.size(), that.size()].min()
        def comp = [this[0..<n], that[0..<n]].transpose().find { it[0] != it[1] }
        comp ? comp[0] <=> comp[1] : this.size() <=> that.size()
    }
}
```


Test:

```groovy
CList a, b; (a, b) = [[], []]; assert ! (a < b)
b = [1] as CList;              assert   (a < b)
a = [1] as CList;              assert ! (a < b)
b = [2] as CList;              assert   (a < b)
a = [2, -1, 0] as CList;       assert ! (a < b)
b = [2, -1] as CList;          assert ! (a < b)
b = [2, -1, 0] as CList;       assert ! (a < b)
b = [2, -1, 0, -17] as CList;  assert   (a < b)
a = [2,  8, 0] as CList;       assert ! (a < b)
```



## Haskell

The built-in comparison operators already do this:

```haskell>Prelude
  [1,2,1,3,2] < [1,2,0,4,4,0,0,0]
False
```


=={{header|Icon}} and {{header|Unicon}}==
List_llt is written in the style of all Icon/Unicon relational operators returning its right argument if successful and signaling failure otherwise.


```Icon
procedure main()
   write( if list_llt([1,2,1,3,2],[1,2,0,4,4,0,0,0]) then "true" else "false" )
end


procedure list_llt(L1,L2)  #: returns L2 if L1 lexically lt L2 or fails
every i := 1 to min(*L1,*L2) do
   if L1[i] << L2[i] then return L2
   else if L1[i] >> L2[i] then fail
if *L1 < *L2 then return L2
end
```



## J


J's built-in comparator operates element-wise.
To compare general sequences you can either box them and use sort.
Or for numeric sequences append minus infinity and sort.
However numeric scalars sort ahead of vectors, i.e. are different from length one lists.


```j
before=: -.@(-: /:~)@,&<~
```



```j
cmp=: {.@\:@,:&(,&__)
```


Below demonstrates non-decreasing order cmp treats length one vector same as scalar


```j

   cmp&.>"{~ ('';0;(,0);1;(,1);1 1)
┌─┬─┬─┬─┬─┬─┐
│0│1│1│1│1│1│
├─┼─┼─┼─┼─┼─┤
│0│0│0│1│1│1│
├─┼─┼─┼─┼─┼─┤
│0│0│0│1│1│1│
├─┼─┼─┼─┼─┼─┤
│0│0│0│0│0│1│
├─┼─┼─┼─┼─┼─┤
│0│0│0│0│0│1│
├─┼─┼─┼─┼─┼─┤
│0│0│0│0│0│0│
└─┴─┴─┴─┴─┴─┘

before&.>"{~ (0;1;'';(,0);(,1);1 1)
┌─┬─┬─┬─┬─┬─┐
│0│1│1│1│1│1│
├─┼─┼─┼─┼─┼─┤
│0│0│1│1│1│1│
├─┼─┼─┼─┼─┼─┤
│0│0│0│1│1│1│
├─┼─┼─┼─┼─┼─┤
│0│0│0│0│1│1│
├─┼─┼─┼─┼─┼─┤
│0│0│0│0│0│1│
├─┼─┼─┼─┼─┼─┤
│0│0│0│0│0│0│
└─┴─┴─┴─┴─┴─┘
```



## Java

{{works with|Java|1.5+}}
{{trans|Common Lisp}}
There are a few methods here. The method named "ordered" which works on arrays is a translation of [[#Common Lisp|Common Lisp]]. The other two are loose translations of [[#Tcl|Tcl]] (some tweaks were needed to get the length checks to work out) and are probably better options.

```java5
import java.util.Arrays;
import java.util.List;

public class ListOrder{
	public static boolean ordered(double[] first, double[] second){
		if(first.length == 0) return true;
		if(second.length == 0) return false;
		if(first[0] == second[0])
			return ordered(Arrays.copyOfRange(first, 1, first.length),
					Arrays.copyOfRange(second, 1, second.length));
		return first[0] < second[0];
	}

	public static <T extends Comparable<? super T>> boolean ordered(List<T> first, List<T> second){
		int i = 0;
		for(; i < first.size() && i < second.size();i++){
			int cmp = first.get(i).compareTo(second.get(i));
			if(cmp == 0) continue;
			if(cmp < 0) return true;
			return false;
		}
		return i == first.size();
	}

	public static boolean ordered2(double[] first, double[] second){
		int i = 0;
		for(; i < first.length && i < second.length;i++){
			if(first[i] == second[i]) continue;
			if(first[i] < second[i]) return true;
			return false;
		}
		return i == first.length;
	}
}
```



## JavaScript



### ES6


<= is already defined for numeric lists in JavaScript


```JavaScript
(() => {
    'use strict';

    // <= is already defined for lists in JS

    // compare :: [a] -> [a] -> Bool
    const compare = (xs, ys) => xs <= ys;


    // TEST
    return [
        compare([1, 2, 1, 3, 2], [1, 2, 0, 4, 4, 0, 0, 0]),
        compare([1, 2, 0, 4, 4, 0, 0, 0], [1, 2, 1, 3, 2])
    ];

    // --> [false, true]
})()

```


{{Out}}

```JavaScript
[false, true]
```



## Joy


```Joy

DEFINE order ==
[equal] [false]
[[[[size] dip size <=] [[<=] mapr2 true [and] fold]] [i] map i and]
ifte.

```


Using it:


```txt

[1 2] [1 2 3] order. # true
[1 2] [1 3] order.   # true
[1 2] [1 2] order.   # false
[1 3] [1 2] order.   # false
[1 2 3] [1 2] order. # false

```



## jq

jq's builtin comparison operators use lexicographic ordering for arrays in general, not just arrays of integers.
```jq

[1,2,3] < [1,2,3,4]  # => true
[1,2,3] < [1,2,4]    # => true
[1,2,3] < [1,2,3]    # => false
```



## Julia

{{works with|Julia|0.6}}


```julia
function islexless(a::AbstractArray{<:Real}, b::AbstractArray{<:Real})
    for (x, y) in zip(a, b)
        if x == y continue end
        return x < y
    end
    return length(a) < length(b)
end

using Primes, Combinatorics
tests = [[1, 2, 3], primes(10), 0:2:6, [-Inf, 0.0, Inf], [π, e, φ, catalan], [2015, 5], [-sqrt(50.0), 50.0 ^ 2]]
println("List not sorted:\n - ", join(tests, "\n - "))
sort!(tests; lt=islexless)
println("List sorted:\n - ", join(tests, "\n - "))
```


{{out}}

```txt
List not sorted:
 - [1, 2, 3]
 - [2, 3, 5, 7]
 - 0:2:6
 - [-Inf, 0.0, Inf]
 - [3.14159, 2.71828, 1.61803, 0.915966]
 - [2015, 5]
 - [-7.07107, 2500.0]
List sorted:
 - [-Inf, 0.0, Inf]
 - [-7.07107, 2500.0]
 - 0:2:6
 - [1, 2, 3]
 - [2, 3, 5, 7]
 - [3.14159, 2.71828, 1.61803, 0.915966]
 - [2015, 5]
```



## Kotlin


```scala
// version 1.0.6

operator fun <T> List<T>.compareTo(other: List<T>): Int
    where T: Comparable<T>, T: Number {
    for (i in 0 until this.size) {
        if (other.size == i) return 1
        when {
            this[i] < other[i] -> return -1
            this[i] > other[i] -> return 1
        }
    }
    return if (this.size == other.size) 0 else -1
}

fun main(args: Array<String>) {
    val lists = listOf(
        listOf(1, 2, 3, 4, 5),
        listOf(1, 2, 1, 5, 2, 2),
        listOf(1, 2, 1, 5, 2),
        listOf(1, 2, 1, 5, 2),
        listOf(1, 2, 1, 3, 2),
        listOf(1, 2, 0, 4, 4, 0, 0, 0),
        listOf(1, 2, 0, 4, 4, 1, 0, 0)
    )
    for (i in 0 until lists.size) println("list${i + 1} : ${lists[i]}")
    println()
    for (i in 0 until lists.size - 1) println("list${i + 1} > list${i + 2} = ${lists[i] > lists[i + 1]}")
}
```


{{out}}

```txt

list1 : [1, 2, 3, 4, 5]
list2 : [1, 2, 1, 5, 2, 2]
list3 : [1, 2, 1, 5, 2]
list4 : [1, 2, 1, 5, 2]
list5 : [1, 2, 1, 3, 2]
list6 : [1, 2, 0, 4, 4, 0, 0, 0]
list7 : [1, 2, 0, 4, 4, 1, 0, 0]

list1 > list2 = true
list2 > list3 = true
list3 > list4 = false
list4 > list5 = true
list5 > list6 = true
list6 > list7 = false

```



## LabVIEW

{{trans|AutoHotkey}}
{{VI solution|LabVIEW_Order_two_numerical_lists.png}}


## Lasso

This is built into the Lasso comparison operators

```Lasso
local(
	first = array(1,2,1,3,2),
	second = array(1,2,0,4,4,0,0,0),
)
#first < #second

local(
	first = array(1,1,1,3,2),
	second = array(1,2,0,4,4,0,0,0),
)
#first < #second
```

{{out}}

```txt
false
true
```



## Lhogho

Uses standard '=' notation


```logo
print [1 2] = [1 2]
print [1 2] = [1 2 3]
print [1 3] = [1 2]
print [1 2 3] = [1 2]

make "list1 [1 2 3 4 5 6]
make "list2 [1 2 3 4 5 7]
print :list1 = :list2
```


{{out}}

```logo
true
false
false
false
false
```



## Lua


In Lua tables with numerical indices are used as lists or arrays and they do not support comparison out-of-the-box, so a function is needed to implement the comparison:


```lua
function arraycompare(a, b)
    for i = 1, #a do
        if b[i] == nil then
            return true
        end
        if a[i] ~= b[i] then
            return a[i] < b[1]
        end
    end
    return true
end
```


Here is some demonstration code:


```lua
function randomarray()
    local t = {}
    for i = 1, math.random(1, 10) do
        t[i] = math.random(1, 10)
    end
    return t
end

math.randomseed(os.time())

for i = 1, 10 do
    local a = randomarray()
    local b = randomarray()

    print(
        string.format("{%s} %s {%s}",
        table.concat(a, ', '),
        arraycompare(a, b) and "<=" or ">",
        table.concat(b, ', ')))
end
```


{{out}} (time used as random seed: 1413127434):

```txt

    {10, 7, 4, 9, 10, 3, 5, 5, 5, 5} > {7, 4, 6, 4, 3, 5, 10}
    {5, 7} <= {6, 3, 7, 7, 7, 1}
    {4} <= {10, 10, 3, 8, 10, 5, 2, 5, 10, 6}
    {6} <= {6, 10, 2, 1, 9, 4, 5, 6, 9}
    {9, 5, 7, 5, 5, 7, 9, 5, 6, 8} > {4, 7, 3, 5, 1, 2, 1, 2}
    {10, 8, 6, 1, 8, 5, 4} > {1, 2}
    {9, 7} > {4, 1, 5, 2, 6, 1, 9, 3, 5}
    {5, 9, 7, 6, 10, 8} <= {9, 6, 9}
    {4, 3, 4, 6, 3, 6, 7, 2, 2, 5} > {3, 10, 6, 8, 1}
    {1, 5, 1, 5, 4} > {1, 3, 5, 3, 2, 10, 1}

```



## Maple


```Maple
orderLists := proc(num1,num2)
	local len1, len2,i:
	len1,len2 := numelems(num1),numelems(num2):
	for i to min(len1,len2) do
		if num1[i] <> num2[i] then
			return evalb(num1[i]<num2[i]):
		end if:
	end do:
	return evalb(len1 < len2):
end proc:
```



## Mathematica


```Mathematica

order[List1_, List2_] := With[{
   L1 = List1[[1 ;; Min @@ Length /@ {List1, List2}]],
   L2 = List2[[1 ;; Min @@ Length /@ {List1, List2}]]
},
   If [Thread[Order[L1, L2]] == 0,
   Length[List1] < Length[List2],
   Thread[Order[L1, L2]] == 1
   ]]
```



```txt
Example use:
order[ {1, 2, 1, 3, 2}, {1, 2, 0, 4, 4, 0, 0, 0} ]
->False

order[ {1, 2}, {1, 2, 4, 4, 0, 0} ]
->True
```



## Maxima


```maxima
"<<"(a,b):=block([n:min(length(a),length(b))],
catch(for i thru n do (if a[i]#b[i] then throw(is(a[i]<b[i]))),
throw(is(length(a)<length(b)))))$
infix("<<")$

[1,2,3] << [1,2,4];
true

[1,2,3] << [1,2];
false

[1,2] << [1,2];
false
```



## Mercury


For a particular numerical type, you can get away with


```Mercury
:- pred lt(list(int)::in, list(int)::in) is semidet.
lt([], [_|_]).
lt([H1|T1], [H2|T2]) :- H1 =< H2, T1 `lt` T2.
```


For a list of any numerical type, one way would be to use a typeclass:


```Mercury
:- pred lt(list(T)::in, list(T)::in) is semidet <= comparable(T).
lt([], [_|_]).
lt([H1|T1], [H2|T2]) :- H1 =< H2, T1 `lt` T2.
```


... which you would have to create:


```Mercury
:- module comparable.
:- interface.
:- import_module int, float, integer, list.

:- typeclass comparable(T) where [
        pred '<'(T::in, T::in) is semidet,
        pred '=<'(T::in, T::in) is semidet
].
:- instance comparable(int).
:- instance comparable(float).
:- instance comparable(integer).
:- instance comparable(list(T)) <= comparable(T).

:- implementation.

:- instance comparable(int) where [
        pred('<'/2) is int.(<),
        pred('=<'/2) is int.(=<)
].
% likewise for float and integer...
:- instance comparable(list(T)) <= comparable(T) where [
        pred('<'/2) is lt,   % the 'lt' above.
        pred('=<'/2) is lte  % 'lt' with: lte([], []).
].

% pred lt
% pred lte
```


Which would be used in this way - note the typeclass and the comparison operator.


```Mercury
:- pred test(list(T), list(T), io, io) <= comparable(T).
:- mode test(in, in, di, uo) is det.
test(A, B) -->
        io.write(A), io.write_string(" < "), io.write(B),
        io.write_string(" : "), io.write_string(S), io.nl,
        { A < B -> S = "yes" ; S = "no" }.
```



## Nim


```nim
proc `<`[T](a, b: openarray[T]): bool =
  for i in 0 .. min(a.len, b.len):
    if a[i] < b[i]: return true
    if a[i] > b[i]: return false
  return a.len < b.len

echo([1,2,1,3,2] < [1,2,0,4,4,0,0,0])
```

{{out}}

```txt
false
```



## OCaml


The built-in comparison operators already do this for lists (although this is not documented):

```ocaml
# [1;2;1;3;2] < [1;2;0;4;4;0;0;0];;
- : bool = false
```


(Warning: However, the built-in comparison operators do not do this for arrays:

```ocaml
# [|1;2;1;3;2|] < [|1;2;0;4;4;0;0;0|];;
- : bool = true
```

)

But we could write it explicitly this way:


```ocaml
let rec ordered_lists = function
  | x1::tl1, x2::tl2 ->
      (match compare x1 x2 with
      | 0 -> ordered_lists (tl1, tl2)
      | 1 -> false
      | _ -> true)
  | [], _ -> true
  | _ -> false
```


Here is a small script to test this function:


```ocaml
(* copy-paste the code of ordered_lists here *)

let make_num_list p n =
  let rec aux acc =
    if Random.int p = 0 then acc
    else aux (Random.int n :: acc)
  in
  aux []

let print_num_list lst =
  List.iter (Printf.printf " %d") lst;
  print_newline()

let () =
  Random.self_init();
  let lst1 = make_num_list 8 5 in
  let lst2 = make_num_list 8 5 in
  print_num_list lst1;
  print_num_list lst2;
  Printf.printf "ordered: %B\n" (ordered_lists (lst1, lst2))
```


Sample execution:

```txt
$ ocaml ordered_lists.ml
 1 2 1 3 2
 1 2 0 4 4 0 0 0
ordered: false
```


Also notice that the function <code>ordered_lists</code> will work with anything the function <code>Pervasives.compare</code> is able to compare (most OCaml types and structures made from the base types). In the prototype of this function below <code>'a list</code> means a list of anything:


```ocaml
val ordered_lists : 'a list * 'a list -> bool
```



## Oforth


In Oforth, list comparison is already defined.

{{Out}}

```txt

[1,2,0,4,4,0,0,0] [1,2,1,3,2] <= .
1 ok

```



## Ol

This sample very similar to Scheme, but implements proper tail recursion. So can test unlimited length lists.

```scheme

(define (lexorder a b)
   (cond
      ((null? b) #false)
      ((null? a) #true)
      ((< (car a) (car b)) #true)
      ((> (car a) (car b)) #false)
      (else
         (lexorder (cdr a) (cdr b)))))

(print (lexorder '(1 2 3) '(1 2 3 4))) ; => true
(print (lexorder '(1 2 4) '(1 2 3)))   ; => false
(print (lexorder '(1 2 3) '(1 2)))     ; => false
(print (lexorder '(1 2 3) '(1 2 3)))   ; => false
(print (lexorder '(1 2 3) '(1 2 8)))   ; => true

```



## PARI/GP


```parigp
lex(u,v)<1
```



## Perl


```Perl
use strict;
use warnings;

sub orderlists {
    my ($firstlist, $secondlist) = @_;

    my ($first, $second);
    while (@{$firstlist}) {
        $first = shift @{$firstlist};
        if (@{$secondlist}) {
            $second = shift @{$secondlist};
            if ($first < $second) {
                return 1;
            }
            if ($first > $second) {
                return 0;
            }
        }
        else {
            return 0;
        }
    }

    @{$secondlist} ? 1 : 0;
}

foreach my $pair (
    [[1, 2, 4], [1, 2, 4]],
    [[1, 2, 4], [1, 2,  ]],
    [[1, 2,  ], [1, 2, 4]],
    [[55,53,1], [55,62,83]],
    [[20,40,51],[20,17,78,34]],
) {
    my $first  = $pair->[0];
    my $second = $pair->[1];
    my $before = orderlists([@$first], [@$second]) ? 'true' : 'false';
    print "(@$first) comes before (@$second) : $before\n";
}
```

{{out}}

```txt

(1 2 4) comes before (1 2 4) : false
(1 2 4) comes before (1 2) : false
(1 2) comes before (1 2 4) : true
(55 53 1) comes before (55 62 83) : true
(20 40 51) comes before (20 17 78 34) : false

```



## Perl 6

There is already a built-in comparison operator.

```perl6>my @a = <1 2 4
;
my @b = <1 2 4>;
say @a," before ",@b," = ", @a before @b;

@a = <1 2 4>;
@b = <1 2>;
say @a," before ",@b," = ", @a before @b;

@a = <1 2>;
@b = <1 2 4>;
say @a," before ",@b," = ", @a before @b;

for 1..10 {
    my @a = flat (^100).roll((2..3).pick);
    my @b = flat @a.map: { Bool.pick ?? $_ !! (^100).roll((0..2).pick) }
    say @a," before ",@b," = ", @a before @b;
}
```

{{out}}

```txt
1 2 4 before 1 2 4 = False
1 2 4 before 1 2 = False
1 2 before 1 2 4 = True
63 52 before 0 52 = False
17 75 24 before 31 75 24 = True
43 32 before 43 32 = False
73 84 before 2 84 = False
73 92 before 40 24 46 = False
16 24 before 41 24 = True
9 12 22 before 9 12 32 67 = True
81 23 before 81 23 = False
55 53 1 before 55 62 83 = True
20 40 51 before 20 17 78 34 = False
```



## Phix

Handled natively, eg ("?" is the shorthand print operator)

```Phix
?{1,2,3}<{1,2,3,4}  -- 1
?{1,2,3,4}<{1,2,3}  -- 0
?{1,2,4}<{1,2,3}    -- 0
?{1,2,3}<{1,2,3}    -- 0
?{1,2,3}<{1,2,4}    -- 1
```

Elements can be any mix of integers, floating point numbers, strings, or nested subsequences, with atoms ordered before sequences.

If you want -1/0/+1 (instead of the true(1)/false(0) shown above), use the builtin compare() function.


## PicoLisp

The built-in comparison functions already do this (not only for lists of numbers, but for any arbitrary data type).

```PicoLisp
: (> (1 2 0 4 4 0 0 0) (1 2 1 3 2))
-> NIL
```



## Pike


```Pike
int(0..1) order_array(array a, array b)
{
  if (!sizeof(a)) return true;
  if (!sizeof(b)) return false;
  if (a[0] == b[0])
    return order_array(a[1..], b[1..]);
  return a[0] < b[0];
}
```

Pikes <code>Array.sort_array()</code> function can sort an array of arrays using the <code><</code> operator, but it will sort longer arrays before shorter ones. Therefore the above function is still needed if the intent is to use the comparison for a sort operation.

If the numbers are in 32bit signed integer range, the following works too:

```Pike
(string)a < (string)b;
```



## PL/I


```PL/I
lists: procedure options (main);  /* 8 June 2014 */

   declare a(10) fixed initial (1, 2, 3, 4, 5, 8, 9, 10, 16, 17),
           b(15) fixed initial (5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 17, 18, 20, 22, 23);

   put skip list (compare(a, b));
   put skip list (compare(b, a));
   put skip list (compare(a, a));


compare: procedure (a, b) returns (bit (1));
   declare (a, b)(*) fixed;
   declare (i, m, n) fixed binary;

   m = hbound(a,1); n = hbound(b,1);
   do i = 1 to min(m, n);
      return (a(i) < b(i));
   end;
   return (m < n);
end compare;

end lists;
```

Results:

```txt

'1'B    (true)
'0'B    (false)
'0'B

```



## PowerShell


```PowerShell

function  order($as,$bs) {
    if($as -and $bs) {
        $a, $as = $as
        $b, $bs = $bs
        if($a -eq $b) {order $as $bs}
        else{$a -lt $b}
    } elseif ($bs) {$true} else {$false}
}
"$(order @(1,2,1,3,2) @(1,2,0,4,4,0,0,0))"

```

<b>Output:</b>

```txt

False

```


===Non-Recursive Version===

```PowerShell

function Test-Order ([int[]]$ReferenceArray, [int[]]$DifferenceArray)
{
    for ($i = 0; $i -lt $ReferenceArray.Count; $i++)
    {
        if ($ReferenceArray[$i] -lt $DifferenceArray[$i])
        {
            return $true
        }
        elseif ($ReferenceArray[$i] -gt $DifferenceArray[$i])
        {
            return $false
        }
    }

    return ($ReferenceArray.Count -lt $DifferenceArray.Count) -or (Compare-Object $ReferenceArray $DifferenceArray) -eq $null
}

```


```PowerShell

Test-Order -ReferenceArray 1, 2, 1, 3, 2 -DifferenceArray 1, 2, 0, 4, 4, 0, 0, 0
Test-Order -ReferenceArray 1, 2, 1, 3, 2 -DifferenceArray 1, 2, 2, 4, 4, 0, 0, 0
Test-Order -ReferenceArray 1, 2, 3       -DifferenceArray 1, 2
Test-Order -ReferenceArray 1, 2          -DifferenceArray 1, 2, 3
Test-Order -ReferenceArray 1, 2          -DifferenceArray 1, 2

```

{{Out}}

```txt

False
True
False
True
True

```



## PureBasic


```purebasic
DataSection
  Array_1:
  Data.i 5              ;element count
  Data.i 1, 2, 3, 4, 5  ;element data
  Array_2:
  Data.i 6
  Data.i 1, 2, 1, 5, 2, 2
  Array_3:
  Data.i 5
  Data.i 1, 2, 1, 5, 2
  Array_4:
  Data.i 5
  Data.i 1, 2, 1, 5, 2
  Array_5:
  Data.i 4
  Data.i 1, 2, 1, 6
  Array_6:
  Data.i 5
  Data.i 1, 2, 1, 6, 2
EndDataSection

#False = 0
#True = 1

;helper subrountine to initialize a dataset, *dataPtr points to the elementcount followed by the element data
Procedure initArrayData(Array a(1), *dataPtr)
  Protected elementCount = PeekI(*dataPtr)

  Dim a(elementCount - 1)
  For i = 0 To elementCount - 1
    *dataPtr + SizeOf(Integer)
    a(i) = PeekI(*dataPtr)
  Next
EndProcedure

;helper subroutine that returns 'True' or 'False' for a boolean input
Procedure.s booleanText(b)
  If b: ProcedureReturn "True": EndIf
  ProcedureReturn "False"
EndProcedure

Procedure order(Array a(1), Array b(1))
  Protected len_a = ArraySize(a()), len_b = ArraySize(b()), elementIndex

  While elementIndex <= len_a And elementIndex <= len_b And a(elementIndex) = b(elementIndex)
    elementIndex + 1
  Wend

  If (elementIndex > len_a  And elementIndex <= len_b) Or (elementIndex <= len_b And a(elementIndex) <= b(elementIndex))
    ProcedureReturn #True
  EndIf
EndProcedure

Dim A_1(0): initArrayData(A_1(), ?Array_1)
Dim A_2(0): initArrayData(A_2(), ?Array_2)
Dim A_3(0): initArrayData(A_3(), ?Array_3)
Dim A_4(0): initArrayData(A_4(), ?Array_4)
Dim A_5(0): initArrayData(A_5(), ?Array_5)
Dim A_6(0): initArrayData(A_6(), ?Array_6)

If OpenConsole()
  PrintN(booleanText(order(A_1(), A_2()))) ;False
  PrintN(booleanText(order(A_2(), A_3()))) ;False
  PrintN(booleanText(order(A_3(), A_4()))) ;False
  PrintN(booleanText(order(A_4(), A_5()))) ;True
  PrintN(booleanText(order(A_5(), A_6()))) ;True

  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf

```

{{out}}

```txt
False
False
False
True
True
```



## Python

The built-in comparison operators already do this:

```python>>>
 [1,2,1,3,2] < [1,2,0,4,4,0,0,0]
False
```



## Racket


```Racket
#lang racket

(define (lex<? a b)
  (cond ((null? b) #f)
        ((null? a) #t)
        ((= (car a) (car b)) (lex<? (cdr a) (cdr b)))
        (else (< (car a) (car b)))))

(lex<? '(1 2 3 4 5) '(1 2 3 4 4)) ; -> #f

```



## Rascal

The built-in comparison operator already does this:

```rascal>rascal
[2,1,3] < [5,2,1,3]
bool: true
```



## REXX

This REXX example uses the same lists as   ''BBC BASIC''.

This example will also work with non-numeric strings.

```rexx
/*REXX program determines  if  a list < previous list,   and returns   true  or  false. */
@.=;                    @.1 = 1 2 1 5 2
                        @.2 = 1 2 1 5 2 2
                        @.3 = 1 2 3 4 5
                        @.4 = 1 2 3 4 5
                                                 /* [↓]  compare a list to previous list*/
         do j=2  while  @.j\=='';      p= j - 1  /*P:  points to previous value in list.*/
         if FNorder(@.p, @.j)=='true'  then is= " < "       /*use a more familiar glyph.*/
                                       else is= " ≥ "       /* "  "   "      "      "   */
         say
         say right('['@.p"]", 40)  is  '['@.j"]"
         end   /*i*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
FNorder: procedure;  parse arg x,y
         wx= words(x);     wy= words(y)
                                         do k=1  for min(wx, wy)
                                            a= word(x, k)         /*get a value from X. */
                                            b= word(y, k)         /* "  "   "     "  Y. */
                                         if a<b  then                return 'true'
                                                 else  if a>b  then  return 'false'
                                         end   /*k*/
         if wx<wy  then return 'true'                  /*handle case of equal (so far). */
                        return 'false'                 /*   "     "   "   "     "   "   */
```

{{out|output|:}}

```txt

                             [1 2 1 5 2]  <  [1 2 1 5 2 2]

                           [1 2 1 5 2 2]  <  [1 2 3 4 5]

                             [1 2 3 4 5]  ≥  [1 2 3 4 5]

```



## Ring


```ring

list1 = "1, 2, 1, 5, 2"
list2 = "5, 2, 1, 5, 2, 2"
list3 = "1, 2, 3, 4, 5"
list4 = "1, 2, 3, 4, 5"

if order(list1, list2) = 0 see "list1=list2" + nl
but order(list1, list2) < 0 see "list1<list2" + nl
else see "list1>list2" + nl ok

if order(list2, list3) = 0 see "list2=list3" + nl
but order(list2, list3) < 0 see "list2<list3" + nl
else see "list2>list3" + nl ok

if order(list3, list4) = 0 see "list3=list4" + nl
but order(list3, list4) < 0 see "list3<list4" + nl
else see "list3>list4" + nl ok

func order alist, blist
     return strcmp(alist, blist)

```

Output:

```txt

list1<list2
list2>list3
list3=list4

```



## Ruby

The built-in <code><=></code> operator already does this:

```ruby>>
 ([1,2,1,3,2] <=> [1,2,0,4,4,0,0,0]) < 0
=> false
```



## Rust

<code>Vec<T></code> implements <code>Ord</code> when <code>T</code> does, so we can just compare them with <code><</code>. (Same with arrays and slices).

```Rust
vec![1, 2, 1, 3, 2] < vec![1, 2, 0, 4, 4, 0, 0, 0]
```



## Scala


```Scala
def lessThan1(a: List[Int], b: List[Int]): Boolean =
  if (b.isEmpty) false
  else if (a.isEmpty) true
  else if (a.head != b.head) a.head < b.head
  else lessThan1(a.tail, b.tail)
```

```Scala
def lessThan2(a: List[Int], b: List[Int]): Boolean = (a, b) match {
  case (_, Nil) => false
  case (Nil, _) => true
  case (a :: _, b :: _) if a != b => a < b
  case _ => lessThan2(a.tail, b.tail)
}
```

```Scala
def lessThan3(a: List[Int], b: List[Int]): Boolean =
  a.zipAll(b, Integer.MIN_VALUE, Integer.MIN_VALUE)
   .find{case (a, b) => a != b}
   .map{case (a, b) => a < b}
   .getOrElse(false)
```

```Scala
val tests = List(
  (List(1, 2, 3), List(1, 2, 3)) -> false,
  (List(3, 2, 1), List(3, 2, 1)) -> false,
  (List(1, 2, 3), List(3, 2, 1)) -> true,
  (List(3, 2, 1), List(1, 2, 3)) -> false,
  (List(1, 2), List(1, 2, 3)) -> true,
  (List(1, 2, 3), List(1, 2)) -> false
)

tests.foreach{case test @ ((a, b), c) =>
  assert(lessThan1(a, b) == c, test)
  assert(lessThan2(a, b) == c, test)
  assert(lessThan3(a, b) == c, test)
}
```



## Scheme


```scheme
(define (lex<? a b)
        (cond ((null? b) #f)
              ((null? a) #t)
              ((= (car a) (car b)) (lex<? (cdr a) (cdr b)))
              (else (< (car a) (car b)))))
```



## Seed7

The operator corresponding to the ordering described in this example is less than.


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln([] (1)       < [] (1, 2));    # If the first list runs out of elements the result is TRUE.
    writeln([] (1, 2)    < [] (1));       # If the second list runs out of elements the result is FALSE.
    writeln([] (1, 2)    < [] (1, 2));    # If both lists run out of elements the result is FALSE.
    writeln([] (1, 2, 3) < [] (1, 1, 3)); # The second element is greater than --> FALSE
    writeln([] (1, 2, 3) < [] (1, 3, 3)); # The second element is less than --> TRUE
    writeln(0 times 0    < [] (1));       # The empty list is less than any nonempty list --> TRUE
    writeln([] (1)       < 0 times 0);    # Any nonempty list is not less than the empty list --> FALSE
    writeln(0 times 0    < 0 times 0);    # The empty list is not less than the empty list --> FALSE
  end func;
```


{{out}}

```txt

TRUE
FALSE
FALSE
FALSE
TRUE
TRUE
FALSE
FALSE

```



## Sidef

Built-in, via the comparison operator (`<=>`):

```ruby
func ordered(a, b) {
    (a <=> b) < 0
}

for p in [
    Pair([1,2,4], [1,2,4]),
    Pair([1,2,4], [1,2]  ),
    Pair([1,2],   [1,2,4]),
] {
    var a = p.first
    var b = p.second
    var before = ordered(a, b)
    say "#{a} comes before #{b} : #{before}"
}
```

{{out}}

```txt

[1, 2, 4] comes before [1, 2, 4] : false
[1, 2, 4] comes before [1, 2] : false
[1, 2] comes before [1, 2, 4] : true

```



## Standard ML


```sml
- List.collate Int.compare ([1,2,1,3,2], [1,2,0,4,4,0,0,0]) = LESS;
val it = false : bool
```



## Swift


```swift
let a = [1,2,1,3,2]
let b = [1,2,0,4,4,0,0,0]
println(lexicographicalCompare(a, b)) // this is "less than"
```

{{out}}

```txt

false

```



## Tcl


```tcl
proc numlist< {A B} {
    foreach a $A b $B {
        if {$a<$b} {
            return 1
        } elseif {$a>$b} {
            return 0
        }
    }
    return 0
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
MODE DATA
$$ numlists=*
1'2'1'3'2
1'2'0'4'4'0'0'0
1'2'3'4'5
1'2'1'5'2'2
1'2'1'6
1'2'1'6'2
1'2'4
1'2'4
1'2
1'2'4
$$ MODE TUSCRIPT
list1="1'2'5'6'7"
LOOP n,list2=numlists
text=CONCAT (" ",list1," < ",list2)
IF (list1<list2) THEN
PRINT " true: ",text
ELSE
PRINT "false: ",text
ENDIF
list1=VALUE(list2)
ENDLOOP

```

{{out}}
<pre style='height:30ex;overflow:scroll'>
false:  1'2'5'6'7 < 1'2'1'3'2
false:  1'2'1'3'2 < 1'2'0'4'4'0'0'0
 true:  1'2'0'4'4'0'0'0 < 1'2'3'4'5
false:  1'2'3'4'5 < 1'2'1'5'2'2
 true:  1'2'1'5'2'2 < 1'2'1'6
 true:  1'2'1'6 < 1'2'1'6'2
 true:  1'2'1'6'2 < 1'2'4
false:  1'2'4 < 1'2'4
false:  1'2'4 < 1'2
 true:  1'2 < 1'2'4

```



## VBA


```vb
Private Function order(list1 As Variant, list2 As Variant) As Boolean
    i = 1
    Do While list1(i) <= list2(i)
        i = i + 1
        If i > UBound(list1) Then
            order = True
            Exit Function
        End If
        If i > UBound(list2) Then
            order = False
            Exit Function
        End If
    Loop
    order = False
End Function
Public Sub main()
    Debug.Print order([{1, 2, 3, 4}], [{1,2,0,1,2}])
    Debug.Print order([{1, 2, 3, 4}], [{1,2,3}])
    Debug.Print order([{1, 2, 3}], [{1,2,3,4}])
End Sub
```
{{out}}

```txt
Onwaar
Onwaar
Waar
```


## VBScript


```vb

Function order_list(arr1,arr2)
	order_list = "FAIL"
	n1 = UBound(arr1): n2 = UBound(arr2)
	n = 0 : p = 0
	If n1 > n2 Then
		max = n2
	Else
		max = n1
	End If
	For i = 0 To max
		If arr1(i) > arr2(i) Then
			n = n + 1
		ElseIf arr1(i) = arr2(i) Then
			p = p + 1
		End If
	Next
	If (n1 < n2 And n = 0) Or _
		 (n1 = n2 And n = 0 And p - 1 <> n1) Or _
		 (n1 > n2 And n = 0 And p = n2) Then
			order_list = "PASS"
	End If
End Function

WScript.StdOut.WriteLine order_list(Array(-1),Array(0))
WScript.StdOut.WriteLine order_list(Array(0),Array(0))
WScript.StdOut.WriteLine order_list(Array(0),Array(-1))
WScript.StdOut.WriteLine order_list(Array(0),Array(0,-1))
WScript.StdOut.WriteLine order_list(Array(0),Array(0,0))
WScript.StdOut.WriteLine order_list(Array(0),Array(0,1))
WScript.StdOut.WriteLine order_list(Array(0,-1),Array(0))
WScript.StdOut.WriteLine order_list(Array(0,0),Array(0))
WScript.StdOut.WriteLine order_list(Array(0,0),Array(1))
WScript.StdOut.WriteLine order_list(Array(1,2,1,3,2),Array(1,2,0,4,4,0,0,0))

```


{{Out}}

```txt

PASS
FAIL
FAIL
PASS
PASS
PASS
FAIL
FAIL
PASS
FAIL

```



## Wart

We'll simply overload <code>&lt;</code> for lists.

```python
def (a < b) :case (or list?.a list?.b)
  if not.b
       nil
     not.a
       b
     (car.a = car.b)
       (cdr.a < cdr.b)
     :else
       (car.a < car.b)
```


{{out}}

```txt
(< '(1 2 3) '(1 2 4))
=> 4
(< '(1 2 4) '(1 2 3))
=> nil

(< '(1 2 3) '(1 2 3 4))
=> (4)
(< '(1 2 4) '(1 2 3 4))
=> nil
```



## zkl


```zkl
fcn listLT(a,b){
   a.walker().zip(b).filter1(fcn([(a,b)]){ a<b }) :  // lazy
   if(_) return(True);;
   a.len()<b.len()
}
```

{{out}}

```txt

listLT(T(1,2,3),T(2,3,4)).println();   //-->True
listLT(T(2,3,4),T(1,2,3)).println();   //-->False
listLT(T(1,2),T(1,2,3,4)).println();   //-->True
listLT(T(1,2,3,4),T(1,2,3)).println(); //-->False
listLT(T(1,2,3),T(1,2,3)).println();   //-->False

```

