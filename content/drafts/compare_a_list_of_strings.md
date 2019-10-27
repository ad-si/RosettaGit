+++
title = "Compare a list of strings"
description = ""
date = 2019-10-17T15:05:06Z
aliases = []
[extra]
id = 17733
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Given a   [[wp:List_(abstract_data_type)|list]]   of arbitrarily many strings, show how to:

*   test if they are all lexically '''equal'''
*   test if every string is lexically '''less than''' the one after it  ''(i.e. whether the list is in strict ascending order)''



Each of those two tests should result in a single true or false value, which could be used as the condition of an   <code> if </code>   statement or similar. 

If the input list has less than two elements, the tests should always return true.

There is ''no'' need to provide a complete program and output.
 
Assume that the strings are already stored in an array/list/sequence/tuple variable (whatever is most idiomatic) with the name   <code>strings</code>,   and just show the expressions for performing those two tests on it (plus of course any includes and custom functions etc. that it needs),   with as little distractions as possible.

Try to write your solution in a way that does not modify the original list,   but if it does then please add a note to make that clear to readers.

<i>If you need further guidance/clarification,   see [[#Perl]] and [[#Python]] for solutions that use implicit short-circuiting loops,   and [[#Perl_6]] for a solution that gets away with simply using a built-in language feature. </i>


;Related tasks:
*   [[String comparison]]





## 11l

{{trans|D}}

```11l
L(strings_s) [‘AA AA AA AA’, ‘AA ACB BB CC’]
   V strings = strings_s.split(‘ ’)
   print(strings)
   print(all(zip(strings, strings[1..]).map(a -> a[0] == a[1])))
   print(all(zip(strings, strings[1..]).map(a -> a[0]  < a[1])))
   print()
```



## 360 Assembly

The program uses one ASSIST macro (XPRNT) to keep the code as short as possible. 

```360asm
*        Compare a list of strings 31/01/2017
COMPLIST CSECT
         USING  COMPLIST,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         MVC    SNAME,=C'ABC'
         LA     R1,SNAME
         LA     R2,ABC
         BAL    R14,TEST           call test('ABC',abc)
         MVC    SNAME,=C'AAA'
         LA     R1,SNAME
         LA     R2,AAA
         BAL    R14,TEST           call test('AAA',aaa)
         MVC    SNAME,=C'ACB'
         LA     R1,SNAME
         LA     R2,ACB
         BAL    R14,TEST           call test('ACB',acb)     
         L      R13,4(0,R13)       epilog 
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
*------- ----   test(name,xlist) -----------------------
TEST     MVC    NAME,0(R1)         store argument #1
         MVC    XLIST(6),0(R2)     store argument #2
         MVI    ALLEQ,X'01'        alleq=true
         MVI    INCRE,X'01'        incre=true
         LA     R6,1               i=1
LOOPI    LA     R2,NXLIST          hbound(xlist)
         BCTR   R2,0               -1
         CR     R6,R2              do i to hbound(xlist)-1
         BH     ELOOPI
         MVC    XBOOL,ALLEQ
         OC     XBOOL,INCRE        or
         CLI    XBOOL,X'01'        and while alleq or incre
         BNE    ELOOPI
         LA     R2,1(R6)           i+1
         SLA    R2,1               *2
         LA     R3,XLIST-2(R2)     @xlist(i+1)
         LR     R1,R6              i
         SLA    R1,1               *2
         LA     R4,XLIST-2(R1)     @xlist(i)
         CLC    0(2,R3),0(R4)      if xlist(i+1)=xlist(i)
         BNE    SEL1B
         MVI    INCRE,X'00'        incre=false
         B      SEL1END
SEL1B    CLC    0(2,R3),0(R4)      if xlist(i+1)<xlist(i)
         BNL    SEL1OTH
         MVI    INCRE,X'00'        incre=false
         MVI    ALLEQ,X'00'        alleq=false
         B      SEL1END
SEL1OTH  MVI    ALLEQ,X'00'        alleq=false
SEL1END  LA     R6,1(R6)           i=i+1
         B      LOOPI
ELOOPI   CLI    ALLEQ,X'01'        if alleq
         BNE    SEL2B
         MVC    TXT,=CL40'all elements are equal'
         B      SEL2END
SEL2B    CLI    INCRE,X'01'        if incre
         BNE    SEL2OTH
         MVC    TXT,=CL40'elements are in increasing order'
         B      SEL2END
SEL2OTH  MVC    TXT,=CL40'neither equal nor in increasing order'
SEL2END  MVI    PG,C' '
         MVC    PG+1(79),PG        clear buffer
         MVC    PG(3),NAME
         MVC    PG+3(3),=C' : '
         MVC    PG+6(40),TXT
         XPRNT  PG,L'PG
         BR     R14                return to caller
*        ----   ----------------------------------------
SNAME    DS     CL3
ABC      DC     CL2'AA',CL2'BB',CL2'CC'
AAA      DC     CL2'AA',CL2'AA',CL2'AA'
ACB      DC     CL2'AA',CL2'CC',CL2'BB'
NAME     DS     CL3
XLIST    DS     3CL2
NXLIST   EQU    (*-XLIST)/L'XLIST
ALLEQ    DS     X
INCRE    DS     X
TXT      DS     CL40
XBOOL    DS     X
PG       DS     CL80
         YREGS
         END    COMPLIST
```

{{out}}

```txt

ABC : elements are in increasing order
AAA : all elements are equal
ACB : neither equal nor in increasing order

```



## Ada


We will store the "list" of strings in a vector. The vector will hold "indefinite" strings, i.e., the strings can have different lengths. 

```Ada
  package String_Vec is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => String);
   
   use type String_Vec.Vector;
```


The equality test iterates from the first to the last-but one index. For index Idx,
it checks checks if Strings(Idx) and Strings(Idx+1) are different. If the answer is
yes for any Idx, the function immediately returns False. If the answer is no for all Idx,
the function finally returns True. 

```Ada
   function All_Are_The_Same(Strings: String_Vec.Vector) return Boolean is
   begin
      for Idx in Strings.First_Index .. Strings.Last_Index-1 loop
	 if Strings(Idx) /= Strings(Idx+1) then
	    return False;
	 end if;
      end loop;
      return True;
   end All_Are_The_Same;
```


Similarily, the strictly ascending test checks if Strings(Idx) is greater or equal Strings(Idx+1). 

```Ada
   function Strictly_Ascending(Strings: String_Vec.Vector) return Boolean is
   begin
      for Idx in Strings.First_Index+1 .. Strings.Last_Index loop
	 if Strings(Idx-1) >= Strings(Idx) then
	    return False;
	 end if;
      end loop;
      return True;
   end Strictly_Ascending;
```


If the variable Strings is of the type String_Vec.vector, one can call these two functions
as usual.

```Ada
Put_Line(Boolean'Image(All_Are_The_Same(Strings)) & ", " &
         Boolean'Image(Strictly_Ascending(Strings)));
```

If Strings holds two or more strings, the result will be either of TRUE, FALSE, or FALSE, TRUE, or FALSE, FALSE, indicating all strings are the same, or they are strictly ascending, or neither. 

However, if Strings only holds zero or one string, the result will be TRUE, TRUE.


## ALGOL 68


```ALGOL68
[]STRING list1 = ("AA","BB","CC");
[]STRING list2 = ("AA","AA","AA");
[]STRING list3 = ("AA","CC","BB");
[]STRING list4 = ("AA","ACB","BB","CC");
[]STRING list5 = ("single_element");

[][]STRING all lists to test = (list1, list2, list3, list4, list5);

PROC equal = ([]STRING list) BOOL:
   BEGIN
      BOOL ok := TRUE;
      FOR i TO UPB list - 1 WHILE ok DO
	 ok := list[i] = list[i+1]
      OD;
      ok
   END;

PROC less than = ([]STRING list) BOOL:
   BEGIN
      BOOL ok := TRUE;
      FOR i TO UPB list - 1 WHILE ok DO
	 ok := list[i] < list[i + 1]
      OD;
      ok
   END;

FOR i TO UPB all lists to test DO
   []STRING list = all lists to test[i];
   print (("list:", (STRING s; FOR i TO UPB list DO s +:= " " + list[i] OD; s), new line));
   IF equal (list) THEN
      print (("...is lexically equal", new line))
   ELSE
      print (("...is not lexically equal", new line))
   FI;
   IF less than (list) THEN
      print (("...is in strict ascending order", new line))
   ELSE
      print (("...is not in strict ascending order", new line))
   FI
OD
```

{{out}}

```txt
list: AA BB CC
...is not lexically equal
...is in strict ascending order
list: AA AA AA
...is lexically equal
...is not in strict ascending order
list: AA CC BB
...is not lexically equal
...is not in strict ascending order
list: AA ACB BB CC
...is not lexically equal
...is in strict ascending order
list: single_element
...is lexically equal
...is in strict ascending order

```



## ALGOL W


```algolw
    % returns true if all elements of the string array a are equal, false otherwise %
    % As Algol W procedures cannot determine the bounds of an array, the bounds     %
    % must be specified in lo and hi                                                %
    logical procedure allStringsEqual ( string(256) array a ( * )
                                      ; integer     value lo, hi
                                      ) ;
    begin
        logical same;
        integer listPos;
        same    := true;
        listPos := lo + 1;
        while same and listPos <= hi do begin
            same    := a( lo ) = a( listPos );
            listPos := listPos + 1
        end;
        same
    end allStringsEqual ;

    % returns true if the elements of the string array a are in ascending order,    %
    % false otherwise                                                               %
    % As Algol W procedures cannot determine the bounds of an array, the bounds     %
    % must be specified in lo and hi                                                %
    logical procedure ascendingOrder  ( string(256) array a ( * )
                                      ; integer     value lo, hi
                                      ) ;
    begin
        logical ordered;
        integer listPos;
        ordered := true;
        listPos := lo + 1;
        while ordered and listPos <= hi do begin
            ordered := a( listPos - 1 ) < a( listPos );
            listPos := listPos + 1
        end;
        ordered
    end ascendingOrder ;
```



## AppleScript



{{trans|JavaScript}} (ES6 Functional example)



```AppleScript
-- allEqual :: [String] -> Bool
on allEqual(xs)
    _and(zipWith(my _equal, xs, rest of xs))
end allEqual

-- azSorted :: [String] -> Bool
on azSorted(xs)
    _and(zipWith(my azBeforeOrSame, xs, rest of xs))
end azSorted

-- _equal :: a -> a -> Bool
on _equal(a, b)
    a = b
end _equal

-- azBefore :: String -> String -> Bool
on azBeforeOrSame(a, b)
    a ≥ b
end azBeforeOrSame

-- _and :: [a] -> Bool
on _and(xs)
    foldr(_equal, true, xs)
end _and


-- TEST
on run
    set lstA to ["isiZulu", "isiXhosa", "isiNdebele", "Xitsonga", "Tshivenda", ¬
        "Setswana", "Sesotho sa Leboa", "Sesotho", "English", "Afrikaans"]
    
    set lstB to ["Afrikaans", "English", "Sesotho", "Sesotho sa Leboa", "Setswana", ¬
        "Tshivenda", "Xitsonga", "isiNdebele", "isiXhosa", "isiZulu"]
    
    set lstC to ["alpha", "alpha", "alpha", "alpha", "alpha", "alpha", "alpha", ¬
        "alpha", "alpha", "alpha"]
    
    
    {allEqual:map(allEqual, [lstA, lstB, lstC]), azSorted:map(azSorted, [lstA, lstB, lstC])}
    
    -- > {allEqual:{false, false, true}, azSorted:{false, true, true}}
end run



-- GENERIC FUNCTIONS

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to lambda(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldr

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to lambda(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set nx to length of xs
    set ny to length of ys
    if nx < 1 or ny < 1 then
        {}
    else
        set lng to cond(nx < ny, nx, ny)
        set lst to {}
        tell mReturn(f)
            repeat with i from 1 to lng
                set end of lst to lambda(item i of xs, item i of ys)
            end repeat
            return lst
        end tell
    end if
end zipWith

-- cond :: Bool -> (a -> b) -> (a -> b) -> (a -> b)
on cond(bool, f, g)
    if bool then
        f
    else
        g
    end if
end cond

-- Lift 2nd class handler function into 1st class script wrapper 
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property lambda : f
        end script
    end if
end mReturn
```


{{Out}}

```txt
{allEqual:{false, false, true}, azSorted:{false, true, true}}
```



## AWK


```AWK

# syntax: GAWK -f COMPARE_A_LIST_OF_STRINGS.AWK
BEGIN {
    main("AA,BB,CC")
    main("AA,AA,AA")
    main("AA,CC,BB")
    main("AA,ACB,BB,CC")
    main("single_element")
    exit(0)
}
function main(list,  arr,i,n,test1,test2) {
    test1 = 1 # elements are identical
    test2 = 1 # elements are in ascending order
    n = split(list,arr,",")
    printf("\nlist:")
    for (i=1; i<=n; i++) {
      printf(" %s",arr[i])
      if (i > 1) {
        if (arr[i-1] != arr[i]) {
          test1 = 0 # elements are not identical
        }
        if (arr[i-1] >= arr[i]) {
          test2 = 0 # elements are not in ascending order
        }
      }
    }
    printf("\n%d\n%d\n",test1,test2)
}

```

{{out}}

```txt

list: AA BB CC
0
1

list: AA AA AA
1
0

list: AA CC BB
0
0

list: AA ACB BB CC
0
1

list: single_element
1
1

```



## C


```c>#include <stdio.h

#include <string.h>

int strings_are_equal(char * * strings, int nstrings)
{
  int result = 1;
  
  while (result && (--nstrings > 0))
  {
    result = !strcmp(*strings, *(strings+nstrings));
  }

  return result;
}

int strings_are_in_ascending_order(char * * strings, int nstrings)
{
  int result = 1;
  int k = 0;
  
  while (result && (++k < nstrings))
  {
    result = (0 >= strcmp(*(strings+k-1), *(strings+k)));
  }

  return result;
}
```



## C sharp

{{works with|C sharp|7}}

```csharp
public static (bool lexicallyEqual, bool strictlyAscending) CompareAListOfStrings(List<string> strings) =>
    strings.Count < 2 ? (true, true) :
    (
        strings.Distinct().Count() < 2,
        Enumerable.Range(1, strings.Count - 1).All(i => string.Compare(strings[i-1], strings[i]) < 0)
    );
```



## C++


Assuming that the <code>strings</code> variable is of type <code>T&lt;std::string&gt;</code> where <code>T</code> is an ordered STL container such as <code>std::vector</code>:

{{works with|C++|11}}

```cpp>#include <algorithm

#include <string>

std::all_of( ++(strings.begin()), strings.end(),
             [&](std::string a){ return a == strings.front(); } )  // All equal

std::is_sorted( strings.begin(), strings.end(),
                [](std::string a, std::string b){ return !(b < a); }) )  // Strictly ascending
```



## D


```d
void main() {
    import std.stdio, std.algorithm, std.range, std.string;

    foreach (const strings; ["AA AA AA AA", "AA ACB BB CC"].map!split) {
        strings.writeln;
        strings.zip(strings.dropOne).all!(ab => ab[0] == ab[1]).writeln;
        strings.zip(strings.dropOne).all!(ab => ab[0] < ab[1]).writeln;
        writeln;
    }
}
```

{{out}}

```txt
["AA", "AA", "AA", "AA"]
true
false

["AA", "ACB", "BB", "CC"]
false
true

```



## Dyalect



```Dyalect
func isSorted(xs) {
    var prev
    for x in xs {
        if prev && !(x > prev) {
            return false
        }
        prev = x
    }
    true
}

func isEqual(xs) {
    var prev
    for x in xs {
        if prev && x != prev {
            return false
        }
        prev = x
    }
    true
}
```



## Clojure

Used similar approach as the Python solution


```clojure


;; Checks if all items in strings list are equal (returns true if list is empty)
(every?	(fn [[a nexta]] (= a nexta)) (map vector strings (rest strings))))

;; Checks strings list is in ascending order (returns true if list is empty)
(every?	(fn [[a nexta]] (<= (compare a nexta) 0)) (map vector strings (rest strings))))


```



## Common Lisp


```Lisp

(defun strings-equal-p (strings)
  (null (remove (first strings) (rest strings) :test #'string=)))

(defun strings-ascending-p (strings)
  (loop for string1 = (first strings) then string2
        for string2 in (rest strings)
        always (string-lessp string1 string2)))

```



## Elena

ELENA 4.1 :

```elena
import system'collections;
import system'routines;
import extensions;

extension helper
{
    isEqual()
        = nil == self.seekEach(self.FirstMember, (n,m => m.equal:n.Inverted ));
        
    isAscending()
    {
        var former := self.enumerator();
        var later := self.enumerator();
        
        later.next();
        
        ^ nil == former.zipBy(later, (prev,next => next <= prev )).seekEach:(b => b)
    }
}

testCases
    = new string[][]::(
         new string[]::("AA","BB","CC"),
         new string[]::("AA","AA","AA"),
         new string[]::("AA","CC","BB"),
         new string[]::("AA","ACB","BB","CC"),
         new string[]::("single_element"));

public program()
{
    testCases.forEach:(list)
        {
            console.printLine(list.asEnumerable()," all equal - ",list.isEqual());
            console.printLine(list.asEnumerable()," ascending - ",list.isAscending())
        };
        
    console.readChar()
}
```

{{out}}

```txt

AA,BB,CC all equal - false
AA,BB,CC ascending - true
AA,AA,AA all equal - true
AA,AA,AA ascending - false
AA,CC,BB all equal - false
AA,CC,BB ascending - false
AA,ACB,BB,CC all equal - false
AA,ACB,BB,CC ascending - true
single_element all equal - true
single_element ascending - true

```



## Elixir


```elixir
defmodule RC do
  def compare_strings(strings) do
    {length(Enum.uniq(strings))<=1, strict_ascending(strings)}
  end
  
  defp strict_ascending(strings) when length(strings) <= 1, do: true
  defp strict_ascending([first, second | _]) when first >= second, do: false
  defp strict_ascending([_, second | rest]), do: strict_ascending([second | rest])
end

lists = [ ~w(AA AA AA AA), ~w(AA ACB BB CC), ~w(AA CC BB), [], ["XYZ"] ]
Enum.each(lists, fn list ->
  IO.puts "#{inspect RC.compare_strings(list)}\t<= #{inspect list} "
end)
```


{{out}}

```txt

{true, false}   <= ["AA", "AA", "AA", "AA"]
{false, true}   <= ["AA", "ACB", "BB", "CC"]
{false, false}  <= ["AA", "CC", "BB"]
{true, true}    <= []
{true, true}    <= ["XYZ"]

```



## Erlang

{{trans|Haskell}}


```erlang

-module(compare_strings).

-export([all_equal/1,all_incr/1]).

all_equal(Strings) ->
	all_fulfill(fun(S1,S2) -> S1 == S2 end,Strings).

all_incr(Strings) ->
	all_fulfill(fun(S1,S2) -> S1 < S2 end,Strings).

all_fulfill(Fun,Strings) ->
	lists:all(fun(X) -> X end,lists:zipwith(Fun, lists:droplast(Strings), tl(Strings)) ).

```


=={{header|F_Sharp|F#}}==

```fsharp
let allEqual strings = Seq.isEmpty strings || Seq.forall (fun x -> x = Seq.head strings) (Seq.tail strings)
let ascending strings = Seq.isEmpty strings || Seq.forall2 (fun x y -> x < y) strings (Seq.tail strings)
```

<p>Actually <code>allEqual</code> is a shortcut and <code>ascending</code> is a general pattern. We can make a function 
out of it which constructs a new function from a comparision function</p>

```fsharp
let (!) f s = Seq.isEmpty s || Seq.forall2 f s (Seq.tail s)
```

<p>and define the 2 task functions that way</p>

```fsharp
let allEqual = !(=)
let ascending = !(<)
```

<p>getting something similar as the builtin in Perl 6</p>


## Factor

Assuming the list is on top of the data stack, testing for lexical equality:

```factor
USE: grouping
all-equal?
```

Testing for ascending order:

```factor
USING: grouping math.order ;
[ before? ] monotonic?
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Compare_a_list_of_strings this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

note: This will work under some ANS-Forth systems. It assumes that WORD stores its string at HERE --- this isn't guaranteed by ANS-Forth.

Raw Forth is a very low level language and has no Native lists so we have to build from scratch.
Remarkably by concatenating these low level operations and using the simple Forth parser we can build the linked lists of strings and the list operators quite simply. The operators and lists that we create become extensions to the language.
<lang>\ linked list of strings creators
: ,"       ( -- )  [CHAR] " WORD  c@ 1+ ALLOT  ;             \ Parse input stream until " and write into next available memory
: [[       ( -- )  0 C, ;                                    \ begin a list. write a 0 into next memory byte (null string)
: ]]       ( -- )  [[ ;                                      \ end list with same null string

: nth      ( n list -- addr) swap 0 do count + loop ;        \ return address of the Nth item in a list

: items    ( list -- n )                                     \ return the number of items in a list
          0 >R
          BEGIN
            COUNT + DUP
            R> 1+ >R
          0= UNTIL
          DROP
          R> 1- ;

: compare$ ( $1 $2 -- -n|0|n )  count rot count compare ;    \ compare is an ANS Forth word. returns 0 if $1=$2

: compare[]   ( list n1 n2 -- flag)                          \ compare items n1 and n2 in list
            ROT dup >R nth ( -- $1)
            swap r> nth    ( -- $1 $2)
            compare$ ;

\ create our lexical operators
: LEX=     ( list -- flag)
           0                                                 \ place holder for the flag
           over items 1
           DO
              over I  I 1+ compare[] +                       \ we sum the comparison results on the stack
           LOOP
           nip 0= ;

: LEX<     ( list -- flag)
           0                                                 \ place holder for the flag
           over items 1
           DO
              over I  I 1+ compare[] 0< NOT +
           LOOP
           nip 0= ;

\ make some lists
create strings  [[ ," ENTRY 4" ," ENTRY 3" ," ENTRY 2" ," ENTRY 1" ]]
create strings2 [[ ," the same" ," the same" ," the same" ]]
create strings3 [[ ," AAA" ," BBB" ," CCC" ," DDD" ]] 
```


Test at the Forth console
(-1 is the result for TRUE)

```txt

STRINGS  lex= . 0 ok
STRINGS2 lex= . -1 ok
STRINGS3 lex= . 0 ok 
STRINGS  lex< . 0 ok
STRINGS2 lex< . 0 ok
STRINGS3 lex< . -1 ok

```



## Forth

This depends upon having the novice-package available --- the novice-package is ANS-Forth, as is this code.

I don't think it is a good idea to write "Raw Forth" as described above. 
Application code is hard to write and hard to read when low-level code is mixed in with application code.
It is better to hide low-level code in general-purpose code-libraries so that the application code can be simple.
Here is my solution using LIST.4TH from my novice-package: http://www.forth.org/novice.html

<lang>
: test-equality ( string node -- new-string bad? ) 
    over count                          \ -- string node adr cnt 
    rot .line @ count   compare ; 

: test-ascending ( string node -- new-string bad? ) 
    .line @ >r 
    count  r@ count     compare -1 <>   \ -- bad? 
    r> swap ; 

: test-seq { seq 'test -- flag }        \ 'TEST picture: string node -- new-string bad? 
     seq length 2 < if  true exit then 
     seq .line @  seq 2nd  'test  find-node 
     nip  0= ; 

```

Here is a test of the above code:
<lang>
(( c" aaa" new-seq >> c" aaa" new-seq >> c" aaa" new-seq )) drop  ok-1 
dup  ' test-equality test-seq . -1  ok-1 
kill-seq  ok 
(( c" aaa" new-seq >> c" bbb" new-seq >> c" aaa" new-seq )) drop  ok-1 
dup  ' test-equality test-seq . 0  ok-1 
kill-seq  ok 
(( c" aaa" new-seq >> c" bbb" new-seq >> c" ccc" new-seq )) drop  ok-1 
dup  ' test-ascending test-seq . -1  ok-1 
kill-seq  ok 
(( c" aaa" new-seq >> c" bbb" new-seq >> c" aaa" new-seq )) drop  ok-1 
dup  ' test-ascending test-seq . 0  ok-1 
kill-seq  ok 

```



## Fortran

Fortran does not offer a "string" item, which is to say, a sequence of items ''plus the length'' as one entity as in Pascal, among others. It does offer a CHARACTER variable, having some specified number of characters so the usual approach is to choose a length that is "long enough". In character comparisons, trailing spaces are ignored so that "xx" and "xx  " are deemed equal. Similarly, it does not offer a list-of-thingies item, so again the usual approach is to provide an array of a size "long enough". One could develop a scheme with auxiliary counters stating how many elements are in use and so forth, but for this example, the parameterisation will do. Inspection of such arrays of character entities requires explicit DO-loops and IF-statements, and functions ALLINORDER and ALLEQUAL could be devised. Earlier Fortrans (prior to 77) lack a CHARACTER type, and so one must struggle with integer arrays.

Later Fortran (90 ''et seq'') offers the special function ALL (and its associate, ANY) for testing multiple logical expressions, and also syntax allowing multiple elements of an array to be specified, as in A(3:7) to access elements 3, 4, 5, 6, 7 of array A. The ALL function has the special feature that if ''no'' logical expressions exist, then they, er, ... all ... are true and the result of ALL(nothing) is true. Well, none of them are false... Whatever the rationalisations this delivers the required result when the list has but one element and so there are no pairs to produce logical expressions, so, none of them are false, so the result is true, as specified.

On the other hand a function such as ALLINORDER would show the sound of one hand clapping. It would also reveal the order in which comparisons were made, and whether the loop would quit on the first failure or blockheadedly slog on through the lot regardless.  Alas, on these questions the documentation for ALL is suspiciously silent.


```Fortran

      INTEGER MANY,LONG
      PARAMETER (LONG = 6,MANY = 4)	!Adjust to suit.
      CHARACTER*(LONG) STRINGS(MANY)	!A list of text strings.
      STRINGS(1) = "Fee"
      STRINGS(2) = "Fie"
      STRINGS(3) = "Foe"
      STRINGS(4) = "Fum"
      IF (ALL(STRINGS(1:MANY - 1) .LT. STRINGS(2:MANY))) THEN
        WRITE (6,*) MANY," strings: strictly increasing in order."
       ELSE
        WRITE (6,*) MANY," strings: not strictly increasing in order."
      END IF
      IF (ALL(STRINGS(1:MANY - 1) .EQ. STRINGS(2:MANY))) THEN
        WRITE (6,*) MANY," strings: all equal."
       ELSE
        WRITE (6,*) MANY," strings: not all equal."
      END IF
      END

```
 

And yes, if MANY is set to one and the extra texts are commented out, the results are both true, and ungrammatical statements are made. Honest. Possibly, another special function, as in <code>COUNT(STRINGS(1:MANY - 1) .LT. STRINGS(2:MANY)))</code> would involve less one-hand-clapping when there are no comparisons to make, but the production of a report that would use it is not in the specification.

===F2003-F2008===
F2008 standard ([ISO 2010], 4.4.3)  defines the character variable of the character type as a set of values composed of character strings and a character string is a sequence of characters, numbered from left to right 1, 2, 3, ... up to the number of characters in the string. The number of characters in the string is called the length of the string. The length is a type parameter; its kind is processor dependent and its value is greater than or equal to zero. I.e in declaration 

```Fortran

 character (len=12) :: surname

```

keyword len is NOT a size of array, it is an intrinsic parameter of character type, and character type is in fortran a [[first-class type]]:  they can be assigned as objects or passed as parameters to a subroutine.

In summary, the character data type in Fortran is a real, first class data type. Fortran character strings are not hacked-up arrays!

```Fortran

program    compare_char_list
   implicit none
   character(len=6), allocatable, dimension(:) :: ss
   integer :: many
   ss = ["Fee","Fie","Foe","Fum"]
   many = size(ss)
   if (all(ss(1:many - 1) .lt. ss(2:many))) then
      write (*,*) many," strings: strictly increasing in order."
   else
      write (*,*) many," strings: not strictly increasing in order."
   end if
   if (all(ss(1:many - 1) .eq. ss(2:many))) then
      write (*,*) many," strings: all equal."
   else
      write (*,*) many," strings: not all equal."
   end if
end program compare_char_list

```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

Function AllEqual(strings() As String) As Boolean
   Dim length As Integer = UBound(strings) - LBound(strings) + 1
   If length < 2 Then Return False
   For i As Integer = LBound(strings) + 1 To UBound(strings)
     If strings(i - 1) <> strings(i) Then Return False
   Next
   Return True
End Function

Function AllAscending(strings() As String) As Boolean
   Dim length As Integer = UBound(strings) - LBound(strings) + 1
   If length < 2 Then Return False
   For i As Integer = LBound(strings) + 1 To UBound(strings)
     If strings(i - 1) >= strings(i) Then Return False
   Next
   Return True
End Function

```



## Go


```go
package cmp

func AllEqual(strings []string) bool {
	if len(strings) < 2 {
		return true
	}

	first := strings[0]
	for _, s := range strings[1:] {
		if s != first {
			return false
		}
	}
	return true
}

func AllLessThan(strings []string) bool {
	if len(strings) < 2 {
		return true
	}

	last := strings[0]
	for _, s := range strings[1:] {
		if !(last < s) {
			return false
		}
		last = s
	}
	return true
}
```

See [[Compare_a_list_of_strings/GoTests]] for validation tests.

Note also there is the function [https://golang.org/pkg/sort/#StringsAreSorted sort.StringsAreSorted] in the Go standard library.  This function tests that the list is ordered by less than or equal to, but not strictly less than.


## Gosu


```gosu
var list = {"a", "b", "c", "d"}

var isHomogeneous = list.toSet().Count < 2 
var isOrderedSet = list.toSet().order().toList() == list
```



## Haskell


```haskell>allEqual :: Eq a =
 [a] -> Bool
allEqual xs = and $ zipWith (==) xs (tail xs)
 
allIncr :: Ord a => [a] -> Bool
allIncr xs = and $ zipWith (<) xs (tail xs)
```



Alternatively, using folds:


```haskell
allEqual
  :: Eq a
  => [a] -> Bool
allEqual [] = True
allEqual (h:t) = foldl (\a x -> a && x == h) True t

allIncreasing
  :: Ord a
  => [a] -> Bool
allIncreasing [] = True
allIncreasing (h:t) = fst $ foldl (\(a, x) y -> (a && x < y, y)) (True, h) t
```


or seeking earlier exit (from longer lists) with '''until''', but in fact, perhaps due to lazy execution, the zipWith at the top performs best.


```haskell
allEq
  :: Eq a
  => [a] -> Bool
allEq [] = True
allEq (h:t) =
  null . snd $ 
  until 
    (\(x, xs) -> null xs || x /= head xs)
    (\(_, x:xs) -> (x, xs)) 
    (h, t)

allInc
  :: Ord a
  => [a] -> Bool
allInc [] = True
allInc (h:t) =
  null . snd $
  until
    (\(x, xs) -> null xs || x >= head xs)
    (\(_, x:xs) -> (x, xs))
    (h, t)
```



## J


'''Solution''' (''equality test''):
```j
   allEq =: 1 = +/@~:     NB. or 1 = #@:~. or -: 1&|. or }.-:}:
```

'''Solution''' (''order test''):
```j
   asc =: /: -: i.@#      NB. or -: (/:~) etc.
```

'''Notes''': <tt>asc</tt> indicates whether <tt>y</tt> is monotonically increasing, but not necessarily strictly monotonically increasing (in other words, it allows equal elements if they are adjacent to each other).


## Java

{{works with|Java|8}}

```java5
import java.util.Arrays;

public class CompareListOfStrings {

    public static void main(String[] args) {
        String[][] arr = {{"AA", "AA", "AA", "AA"}, {"AA", "ACB", "BB", "CC"}};
        for (String[] a : arr) {
            System.out.printf("%s%n%s%n%s%n", Arrays.toString(a),
            Arrays.stream(a).distinct().count() < a.length,
            Arrays.equals(Arrays.stream(a).distinct().sorted().toArray(), a));
        }
    }
}
```

{{out}}

```txt
[AA, AA, AA, AA]
true
false
[AA, ACB, BB, CC]
false
true
```



## JavaScript


### ES5


### =Iterative=


```JavaScript
function allEqual(a) {
  var out = true, i = 0;
  while (++i<a.length) {
    out = out && (a[i-1] === a[i]);
  } return out;
}

function azSorted(a) {
  var out = true, i = 0;
  while (++i<a.length) {
    out = out && (a[i-1] < a[i]);
  } return out;
}

var e = ['AA', 'AA', 'AA', 'AA'], s = ['AA', 'ACB', 'BB', 'CC'], empty = [], single = ['AA'];
console.log(allEqual(e)); // true
console.log(allEqual(s)); // false
console.log(allEqual(empty)); // true
console.log(allEqual(single)); // true
console.log(azSorted(e)); // false
console.log(azSorted(s)); // true
console.log(azSorted(empty)); // true
console.log(azSorted(single)); // true

```



### ES6


### =Functional=


Using a generic zipWith, and functionally composed predicates:

```JavaScript
(() => {
    'use strict';

    // allEqual :: [String] -> Bool
    let allEqual = xs => and(zipWith(equal, xs, xs.slice(1))),

        // azSorted :: [String] -> Bool
        azSorted = xs => and(zipWith(azBefore, xs, xs.slice(1))),

        // equal :: a -> a -> Bool
        equal = (a, b) => a === b,

        // azBefore :: String -> String -> Bool
        azBefore = (a, b) => a.toLowerCase() <= b.toLowerCase();


    // GENERIC

    // and :: [Bool] -> Bool
    let and = xs => xs.reduceRight((a, x) => a && x, true),

        // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
        zipWith = (f, xs, ys) => {
            let ny = ys.length;
            return (xs.length <= ny ? xs : xs.slice(0, ny))
                .map((x, i) => f(x, ys[i]));
        };


    // TEST

    let lists = [
        ['isiZulu', 'isiXhosa', 'isiNdebele', 'Xitsonga',
            'Tshivenda', 'Setswana', 'Sesotho sa Leboa', 'Sesotho',
            'English', 'Afrikaans'
        ],
        ['Afrikaans', 'English', 'isiNdebele', 'isiXhosa',
            'isiZulu', 'Sesotho', 'Sesotho sa Leboa', 'Setswana',
            'Tshivenda', 'Xitsonga',
        ],
        ['alpha', 'alpha', 'alpha', 'alpha', 'alpha', 'alpha',
            'alpha', 'alpha', 'alpha', 'alpha', 'alpha', 'alpha'
        ]
    ];

    return {
        allEqual: lists.map(allEqual),
        azSorted: lists.map(azSorted)
    };

})();
```


{{Out}}

```JavaScript
{
  "allEqual": [
    false,
    false,
    true
  ],
  "azSorted": [
    false,
    true,
    true
  ]
}
```



## jq

{{works with|jq |1.4}}

For both the following functions, the input is assumed to be a (possibly empty) array of strings.
In both cases also, the implementations are fast but could be improved at the expense of complexity.

```jq
# Are the strings all equal?
def lexically_equal:
  . as $in
  | reduce range(0;length-1) as $i
      (true; if . then $in[$i] == $in[$i + 1] else false end);

# Are the strings in strictly ascending order?
def lexically_ascending:
  . as $in
  | reduce range(0;length-1) as $i
      (true; if . then $in[$i] < $in[$i + 1] else false end);
```

'''Examples''':

```jq
[] | lexically_equal #=> true
```


```jq
["a", "ab"] | lexically_ascending #=> true
```



## Jsish

Code from Javascript, ES5.


```javascript
/* Compare list of strings, in Jsish */
function allEqual(a) {
  var out = true, i = 0;
  while (++i<a.length) {
    out = out && (a[i-1] === a[i]);
  } return out;
}

function allAscending(a) {
  var out = true, i = 0;
  while (++i<a.length) {
    out = out && (a[i-1] < a[i]);
  } return out;
}

if (allEqual(strings)) puts("strings array all equal");
else puts("strings array not all equal");

if (allAscending(strings)) puts("strings array in strict ascending order");
else puts("strings array not in strict ascending order");
```


{{out}}
None, task requirement asks for an assumed preloaded ''strings'' array, no full program, and little other distractions.


## Julia

{{works with|Julia|0.6}}


```julia
allequal(arr::AbstractArray) = isempty(arr) || all(x -> x == first(arr), arr)

test = [["RC", "RC", "RC"], ["RC", "RC", "Rc"], ["RA", "RB", "RC"],
       ["RC"], String[], ones(Int64, 4), 1:4]

for v in test
    println("\n# Testing $v:")
    println("The elements are $("not " ^ !allequal(v))all equal.")
    println("The elements are $("not " ^ !issorted(v))strictly increasing.")
end
```


{{out}}

```txt
# Testing String["RC", "RC", "RC"]:
The elements are all equal.
The elements are strictly increasing.

# Testing String["RC", "RC", "Rc"]:
The elements are not all equal.
The elements are strictly increasing.

# Testing String["RA", "RB", "RC"]:
The elements are not all equal.
The elements are strictly increasing.

# Testing String["RC"]:
The elements are all equal.
The elements are strictly increasing.

# Testing String[]:
The elements are all equal.
The elements are strictly increasing.

# Testing [1, 1, 1, 1]:
The elements are all equal.
The elements are strictly increasing.

# Testing 1:4:
The elements are not all equal.
The elements are strictly increasing.
```



## Klong


```K

    {:[2>#x;1;&/=:'x]}:(["test" "test" "test"])
1
    {:[2>#x;1;&/<:'x]}:(["bar" "baz" "foo"])
1

```



## Kotlin


```scala
// version 1.0.6

fun areEqual(strings: Array<String>): Boolean {
    if (strings.size < 2) return true
    return (1 until strings.size).none { strings[it] != strings[it - 1] }
}

fun areAscending(strings: Array<String>): Boolean {
    if (strings.size < 2) return true
    return (1 until strings.size).none { strings[it] <= strings[it - 1] }
}

// The strings are given in the command line arguments

fun main(args: Array<String>) {
    println("The strings are : ${args.joinToString()}")
    if (areEqual(args)) println("They are all equal")
    else if (areAscending(args)) println("They are in strictly ascending order")
    else println("They are neither equal nor in ascending order")
}
```

Sample input/output:
{{out}}

```txt

The strings are : first, second, third
They are in strictly ascending order

```



## Lua


```lua
function identical(t_str)
    _, fst = next(t_str)
    if fst then
        for _, i in pairs(t_str) do
            if i ~= fst then return false end
        end
    end
    return true
end

function ascending(t_str)
    prev = false
    for _, i in ipairs(t_str) do
        if prev and prev >= i then return false end
        prev = i
    end
    return true
end

function check(str)
    t_str = {}
    for i in string.gmatch(str, "[%a_]+") do
        table.insert(t_str, i)
    end
    str = str .. ": "
    if not identical(t_str) then str = str .. "not " end
    str = str .. "identical and "
    if not ascending(t_str) then str = str .. "not " end
    print(str .. "ascending.")
end

check("ayu dab dog gar panda tui yak")
check("oy oy oy oy oy oy oy oy oy oy")
check("somehow   somewhere  sometime")
check("Hoosiers")
check("AA,BB,CC")
check("AA,AA,AA")
check("AA,CC,BB")
check("AA,ACB,BB,CC")
check("single_element")
```

{{out}}

```txt
ayu dab dog gar panda tui yak: not identical and ascending.
oy oy oy oy oy oy oy oy oy oy: identical and not ascending.
somehow   somewhere  sometim: not identical and not ascending.
Hoosiers: identical and ascending.
AA,BB,CC: not identical and ascending.
AA,AA,AA: identical and not ascending.
AA,CC,BB: not identical and not ascending.
AA,ACB,BB,CC: not identical and ascending.
single_element: identical and ascending.
```



## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      Function Equal(Strings){
            k=Each(Strings, 2, -1)
            a$=Array$(Strings, 0)
            =True
            While k {
                  =False
                  if a$<>array$(k) then exit
                  =True
            }
      }
      Function LessThan(Strings){
            =True
            if len(Strings)<2 then exit
            k=Each(Strings, 2)
            a$=Array$(Strings, 0)
            While k {
                  =False
                  if a$>=array$(k) then exit
                  a$=array$(k)
                  =True
            }
      }
      
      Print Equal(("alfa","alfa","alfa", "alfa"))=True
      Print Equal(("alfa",))=True
      Dim A$(10)="alfa"
      Print Equal(A$())=True
      Print Equal(("alfa1","alfa2","alfa3", "alfa4"))=False
      
      Print LessThan(("alfa1","alfa2","alfa3", "alfa4"))=True
      Print LessThan(("alfa1",))=true
      alfa$=Lambda$ k=1 ->{=String$("*", k) : k++}
      Dim A$(20)<<alfa$()
      Print LessThan(A$())=True
      A$(5)=""
      Print LessThan(A$())=False
}
Checkit

```



## Maple


```Maple
lexEqual := proc(lst)
	local i:
	for i from 2 to numelems(lst) do
		if lst[i-1] <> lst[i] then return false: fi:
	od:
	return true:
end proc:
lexAscending := proc(lst)
	local i:
	for i from 2 to numelems(lst) do
		if StringTools:-Compare(lst[i],lst[i-1]) then return false: fi:
	od:
	return true:
end proc:
tst := ["abc","abc","abc","abc","abc"]:
lexEqual(tst):
lexAscending(tst):
```

{{Out|Examples}}

```txt
true
false
```



## Mathematica


```Mathematica
data1 = {"aaa", "aaa", "aab"};
Apply[Equal, data]
OrderedQ[data]
```

{{out}}

```txt
False
True
```




## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method isEqual(list = Rexx[]) public static binary returns boolean
  state = boolean (1 == 1) -- default to true
  loop ix = 1 while ix < list.length
    state = list[ix - 1] == list[ix]
    if \state then leave ix
    end ix
  return state

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method isAscending(list = Rexx[]) public static binary returns boolean
  state = boolean (1 == 1) -- default to true
  loop ix = 1 while ix < list.length
    state = list[ix - 1] << list[ix]
    if \state then leave ix
    end ix
  return state

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static

  samples = [ -
      ['AA', 'BB', 'CC'] -
    , ['AA', 'AA', 'AA'] -
    , ['AA', 'CC', 'BB'] -
    , ['single_element'] -
    ]

  loop ix = 0 while ix < samples.length
    sample = samples[ix]
    if isEqual(sample)     then eq  = 'elements are identical'
                           else eq  = 'elements are not identical'
    if isAscending(sample) then asc = 'elements are in ascending order'
                           else asc = 'elements are not in ascending order'
    say 'List:' Arrays.toString(sample)
    say '  'eq
    say '  'asc
    end ix
  return


```

{{out}}

```txt

List: [AA, BB, CC]
  elements are not identical
  elements are in ascending order
List: [AA, AA, AA]
  elements are identical
  elements are not in ascending order
List: [AA, CC, BB]
  elements are not identical
  elements are not in ascending order
List: [single_element]
  elements are identical
  elements are in ascending order

```



## OCaml


```Ocaml

open List;;

let analyze cmp l  =
  let rec analyze' l prevs =
    match l with
    [] -> true
    | [s] -> cmp prevs s
    | s::rest -> (cmp prevs s) && (analyze' rest s)
  in analyze' (List.tl l) (List.hd l)
;;

let isEqual     = analyze (=) ;;
let isAscending = analyze (<) ;;

let test sample =
   List.iter print_endline sample;
   if (isEqual sample)
       then (print_endline "elements are identical")
       else (print_endline "elements are not identical");
   if (isAscending sample)
	     then print_endline "elements are in ascending order"
         else print_endline "elements are not in ascending order";;


let lasc =   ["AA";"BB";"CC";"EE"];;
let leq =    ["AA";"AA";"AA";"AA"];;
let lnoasc = ["AA";"BB";"EE";"CC"];;

List.iter test [lasc;leq;lnoasc];;

```

{{out}}

```txt

AA
BB
CC
EE
elements are not identical
elements are in ascending order
AA
AA
AA
AA
elements are identical
elements are not in ascending order
AA
BB
EE
CC
elements are not identical
elements are not in ascending order

```



## Oforth



```oforth
: lexEqual   asSet size 1 <= ;
: lexCmp(l)  l l right( l size 1- ) zipWith(#<) and ;
```



## ooRexx


```oorexx
/* REXX ---------------------------------------------------------------
* 28.06.2014 Walter Pachl
*--------------------------------------------------------------------*/
Call test 'ABC',.list~of('AA','BB','CC')
Call test 'AAA',.list~of('AA','AA','AA')
Call test 'ACB',.list~of('AA','CC','BB')
Exit

test: Procedure
Use Arg name,list
all_equal=1
increasing=1
Do i=0 To list~items-2
  i1=i+1
  Select
    When list[i1]==list[i] Then increasing=0
    When list[i1]<<list[i] Then Do
                                all_equal=0
                                increasing=0
                                End
    When list[i1]>>list[i] Then all_equal=0
    End
  End
Select
  When all_equal Then
    Say 'List' name': all elements are equal'
  When increasing Then
    Say 'List' name': elements are in increasing order'
  Otherwise
    Say 'List' name': neither equal nor in increasing order'
  End
Return
```

{{out}}

```txt
List ABC: elements are in increasing order
List AAA: all elements are equal
List ACB: neither equal nor in increasing order
```



## PARI/GP

Easiest is to use <code>Set()</code>:

```parigp
allEqual(strings)=#Set(strings)<2
inOrder(strings)=Set(strings)==strings
```


More efficient:

```parigp
allEqual(strings)=for(i=2,#strings,if(strings[i]!=strings[i-1], return(0))); 1
inOrder(strings)=for(i=2,#strings,if(strings[i]>strings[i-1], return(0))); 1
```



## Perl



```perl
use List::Util 1.33 qw(all);

all { $strings[0] eq $strings[$_] } 1..$#strings  # All equal
all { $strings[$_-1] lt $strings[$_] } 1..$#strings  # Strictly ascending
```


Alternatively, if you can guarantee that the input strings don't contain null bytes, the equality test can be performed by a regex like this:


```perl
join("\0", @strings) =~ /^ ( [^\0]*+ ) (?: \0 \1 )* $/x  # All equal
```



## Perl 6


In Perl 6, putting square brackets around an [[wp:Infix_notation|infix]] operator turns it into a listop that effectively works as if the operator had been but in between all of the elements of the argument list ''(or in technical terms, it [[wp:Fold_(higher-order_function)|folds/reduces]] the list using that operator, while taking into account the operator's inherent [http://perlcabal.org/syn/S03.html#line_62 associativity] and identity value)'':


```perl6
[eq] @strings  # All equal
[lt] @strings  # Strictly ascending
```



## Phix


```Phix
function allsame(sequence s)
    for i=2 to length(s) do
        if s[i]!=s[1] then return 0 end if
    end for
    return 1
end function

function strict_order(sequence s)
    for i=2 to length(s) do
        if s[i]<=s[i-1] then return 0 end if
    end for
    return 1
end function

procedure test(sequence s)
    ?{s,allsame(s),strict_order(s)}
end procedure

test({"AA","BB","CC"})
test({"AA","AA","AA"})
test({"AA","CC","BB"})
test({"AA","ACB","BB","CC"})
test({"single_element"})
```

{{out}}

```txt

{{"AA","BB","CC"},0,1}
{{"AA","AA","AA"},1,0}
{{"AA","CC","BB"},0,0}
{{"AA","ACB","BB","CC"},0,1}
{{"single_element"},1,1}

```



## PicoLisp

PicoLisp has the native operators =, > and < these can take an infinite number of arguments and are also able to compare Transient symbols (the Strings of PicoLisp). 

```PicoLisp
(= "AA" "AA" "AA")
-> T
(= "AA" "AA" "Aa")
-> NIL
(< "AA" "AA")
-> NIL
(< "AA" "Aa")
-> T
(< "1" "A" "B" "Z" "c" )
-> T
(> "A" "B" "Z" "C")
-> NIL
```

If you want a function which takes one list here are some straight-forward implementation:

```PicoLisp

(de same (List)
  (apply = List))

(de sorted (List)
  (apply < List))

(de sorted-backwards (List)
  (apply > List))

(same '("AA" "AA" "AA"))
-> T

```

This would of course also work with <= and >= without any hassle.


## PL/I


```pli
*process source xref attributes or(!);
 /*--------------------------------------------------------------------
 * 01.07.2014 Walter Pachl
 *-------------------------------------------------------------------*/
 clist: Proc Options(main);
 Dcl (hbound) Builtin;
 Dcl sysprint Print;
 Dcl abc(3) Char(2) Init('AA','BB','CC');
 Dcl aaa(3) Char(2) Init('AA','AA','AA');
 Dcl acb(3) Char(2) Init('AA','CC','BB');
 Call test('ABC',ABC);
 Call test('AAA',AAA);
 Call test('ACB',ACB);

 test: Procedure(name,x);
 Dcl name Char(*);
 Dcl x(*) Char(*);
 Dcl (all_equal,increasing) Bit(1) Init('1'b);
 Dcl (i,i1) Bin Fixed(31);
 Dcl txt Char(50) Var;
 Do i=1 To hbound(x)-1 While(all_equal ! increasing);
  i1=i+1;
  Select;
    When(x(i1)=x(i)) increasing='0'b;
    When(x(i1)<x(i)) Do;
                     increasing='0'b;
                     all_equal='0'b;
                     End;
    Otherwise /* x(i1)>x(i) */
                     all_equal='0'b;
    End;
  End;
  Select;
    When(all_equal)  txt='all elements are equal';
    When(increasing) txt='elements are in increasing order';
    Otherwise        txt='neither equal nor in increasing order';
    End;
  Put Skip List(name!!': '!!txt);
  End;
  End;
```

{{out}}

```txt
ABC: elements are in increasing order
AAA: all elements are equal
ACB: neither equal nor in increasing order
```



## PowerShell

{{works with|PowerShell|4.0}}

```PowerShell

function IsAscending ( [string[]]$Array ) { ( 0..( $Array.Count - 2 ) ).Where{ $Array[$_] -le $Array[$_+1] }.Count -eq $Array.Count - 1 }
function IsEqual     ( [string[]]$Array ) { ( 0..( $Array.Count - 2 ) ).Where{ $Array[$_] -eq $Array[$_+1] }.Count -eq $Array.Count - 1 }
 
IsAscending 'A', 'B', 'B', 'C'
IsAscending 'A', 'C', 'B', 'C'
IsAscending 'A', 'A', 'A', 'A'
 
IsEqual     'A', 'B', 'B', 'C'
IsEqual     'A', 'C', 'B', 'C'
IsEqual     'A', 'A', 'A', 'A'

```

{{out}}

```txt

True
False
True
False
False
True

```



## PureBasic


```purebasic
EnableExplicit
DataSection
  Data.s ~"AA\tAA\tAA\nAA\tBB\tCC\nAA\tCC\tBB\nAA\tACB\tBB\tCC\nsingel_element"
EndDataSection

Macro PassFail(PF)
  If PF : PrintN("Pass") : Else : PrintN("Fail") : EndIf
EndMacro

Macro ProcRec(Proc)
  Define tf1$,tf2$ : Static chk.b : chk=#True
  tf1$=StringField(s$,c,tz$) : tf2$=StringField(s$,c+1,tz$)
  If Len(tf2$) : Proc(s$,tz$,c+1) : EndIf  
EndMacro

Procedure.b IsStringsEqual(s$,tz$=~"\t",c.i=1)
  ProcRec(IsStringsEqual)
  chk & Bool(tf1$=tf2$ Or tf2$="")
  ProcedureReturn chk
EndProcedure

Procedure.b IsStringsAscending(s$,tz$=~"\t",c.i=1)
  ProcRec(IsStringsAscending)
  chk & Bool(tf1$<tf2$ Or tf2$="")  
  ProcedureReturn chk  
EndProcedure

Define t$,sf$,c.i,i.i,PF.b
Read.s t$ : c=CountString(t$,~"\n")
OpenConsole("Compare a list of Strings")
For i=1 To c+1
  sf$=StringField(t$,i,~"\n")
  PrintN("List : "+sf$)
  Print("Lexical test   : ") : PassFail(IsStringsEqual(sf$))
  Print("Ascending test : ") : PassFail(IsStringsAscending(sf$))
  PrintN("")
Next
Input()
```

{{out}}

```txt
List : AA       AA      AA
Lexical test   : Pass
Ascending test : Fail

List : AA       BB      CC
Lexical test   : Fail
Ascending test : Pass

List : AA       CC      BB
Lexical test   : Fail
Ascending test : Fail

List : AA       ACB     BB      CC
Lexical test   : Fail
Ascending test : Pass

List : singel_element
Lexical test   : Pass
Ascending test : Pass
```



## Python

A useful pattern is that when you need some function of an item in a list with its next item over possibly all items in the list then <code>f(a, nexta) for a, nexta in zip(alist, alist[1:]))</code> works nicely. 
(Especially if an index is not needed elsewhere in the algorithm).

```python
all(a == nexta for a, nexta in zip(strings, strings[1:])) # All equal
all(a < nexta for a, nexta in zip(strings, strings[1:])) # Strictly ascending

len(set(strings)) == 1  # Concise all equal
sorted(strings, reverse=True) == strings  # Concise (but not particularly efficient) ascending

```



Equivalently, we can also use additional list arguments with '''map''' rather than zip, 

and, if we wish, pass functional forms of standard operators to either of them:


```python
from operator import (eq, lt)


xs = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta",
      "eta", "theta", "iota", "kappa", "lambda", "mu"]

ys = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta",
      "eta", "theta", "iota", "kappa", "lambda", "mu"]

az = sorted(xs)

print (
    all(map(eq, xs, ys)),

    all(map(lt, xs, xs[1:])),

    all(map(lt, az, az[1:]))
)
```

{{Out}}

```txt
True False True
```



## R


Let's start with a function that splits a vector into
sub-vectors; it starts a new vector whenever the comparison
function yields false.


```R

chunks <- function (compare, xs) {
  starts = which(c(T, !compare(head(xs, -1), xs[-1]), T))
  lapply(seq(1,length(starts)-1),
         function(i) xs[starts[i]:(starts[i+1]-1)] )
}

```


Testing:


```R

> chunks(`<`, c(0,4,8,1,3,5,7,9))
[[1]]
[1] 0 4 8

[[2]]
[1] 1 3 5 7 9

```


R displays the results in a very prolix manner, so let's simplify it.


```R

> toString(chunks(`<`, c(0,4,8,1,3,5,7,9,-2,0,88)))
[1] "c(0, 4, 8), c(1, 3, 5, 7, 9), c(-2, 0, 88)"
> toString(chunks(`==`, c(0,0,0,5,5,8)))
[1] "c(0, 0, 0), c(5, 5), 8"

```


Defining the required functions:


```R

all.eq <- function(xs) 1 == length( chunks(`==`, xs))
ascending <- function(xs) 1 == length( chunks(`<`, xs))

```


Testing:


```R

> all.eq(c('by'))
[1] TRUE
> all.eq(c('by','by','by'))
[1] TRUE
> all.eq(c('by','by','by','zoo'))
[1] FALSE
> ascending(c("at", "even", "once", "run", "zoo"))
[1] TRUE
> ascending(c("at", "even", "once", "run", "zoo", "we"))
[1] FALSE
> ascending(c("at", "even", "go", "go"))
[1] FALSE
> ascending(c("at"))
[1] TRUE

```



## Racket


Racket mostly has this... see [http://docs.racket-lang.org/reference/strings.html?q=string%3C%3F#%28def._%28%28quote._~23~25kernel%29._string~3c~3f%29%29 documentation of <code>string=?</code> and <code>string&lt;?</code>].

There are two small issues:
* Racket will not cope with comparing less than 2 strings
* also <code>string=?</code> and <code>string&lt;?</code> take variable arguments, so the list has to be <code>apply</code>ed to the functions

Hence the wrapper in the code below:

```racket
#lang racket/base
(define ((list-stringX? stringX?) strs)
  (or (null? strs) (null? (cdr strs)) (apply stringX? strs)))
(define list-string=? (list-stringX? string=?))
(define list-string<? (list-stringX? string<?))

(module+ test
  (require tests/eli-tester)
  (test
   (list-string=? '()) => #t
   (list-string=? '("a")) => #t
   (list-string=? '("a" "a")) => #t
   (list-string=? '("a" "a" "a")) => #t
   (list-string=? '("b" "b" "a")) => #f)
  
  (test
   (list-string<? '()) => #t
   (list-string<? '("a")) => #t
   (list-string<? '("a" "b")) => #t
   (list-string<? '("a" "a")) => #f
   (list-string<? '("a" "b" "a")) => #f
   (list-string<? '("a" "b" "c")) => #t))
```



## Red


```Red
Red []

list1: ["asdf" "Asdf" "asdf"]
list2: ["asdf" "bsdf" "asdf"]
list3: ["asdf" "asdf" "asdf"]

all-equal?: func [list][   1 = length? unique/case list  ]
sorted?: func [list][   list == sort/case copy list ]  ;; sort without copy would modify list !

print all-equal? list1
print sorted? list1

print all-equal? list2
print sorted? list2

print all-equal? list3
print sorted? list3

```

{{out}}

```txt
false
false
false
false
true
true

```



## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* 28.06.2014 Walter Pachl
*--------------------------------------------------------------------*/
Call mklist 'ABC','AA','BB','CC'
Call test 'ABC'
Call mklist 'AAA','AA','AA','AA'
Call mklist 'ACB','AA','CC','BB'
Call test 'AAA'
Call test 'ACB'
Exit

mklist:
  list=arg(1)
  do i=1 by 1 To arg()-1
    call value list'.'i,arg(i+1)
    End
  Call value list'.0',i-1
  Return

test:
Parse Arg list
all_equal=1
increasing=1
Do i=1 To value(list'.0')-1 While all_equal | increasing
  i1=i+1
  Select
    When value(list'.i1')==value(list'.i') Then increasing=0
    When value(list'.i1')<<value(list'.i') Then Do
                                                all_equal=0
                                                increasing=0
                                                End
    When value(list'.i1')>>value(list'.i') Then all_equal=0
    End
  End
Select
  When all_equal Then
    Say 'List' value(list)': all elements are equal'
  When increasing Then
    Say 'List' value(list)': elements are in increasing order'
  Otherwise
    Say 'List' value(list)': neither equal nor in increasing order'
  End
Return
```

{{out}}

```txt
List ABC: elements are in increasing order
List AAA: all elements are equal
List ACB: neither equal nor in increasing order
```



### version 2

Programming note:   If a caseless compare (case insensitive) is desired, the two 
:::::::* '''parse arg x'''        (on lines '''14'''   &   '''20''') 
REXX statements could be replaced with either of   (they're equivalent):
:::::::* '''parse upper arg x'''
:::::::* '''arg x'''

```rexx
/*REXX program compares a list of  (character) strings for:   equality,  all ascending. */
@.1= 'ayu dab dog gar panda tui yak'             /*seven strings: they're all ascending.*/
@.2= 'oy oy oy oy oy oy oy oy oy oy'             /*  ten strings:         all equal.    */
@.3= 'somehow   somewhere  sometime'             /*three strings:   ¬equal,  ¬ascending.*/
@.4= 'Hoosiers'                                  /*only a single string is defined.     */
@.5=                                             /*Null.   That is,  no strings here.   */
         do j=1  for 5;    say;   say            /* [↓]  traipse through all the lists. */
         say center(' '@.j, 50, "═")             /*display a centered title/header.     */
         if ifEqual( @.j)  then  say 'strings are all equal.'
         if ifAscend(@.j)  then  say 'strings are ascending.'
         end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ifEqual:  procedure; parse arg strings           /*set  STRINGS  to a string in the list*/
            do k=2  to words(strings)            /*scan the strings in the list.        */
            if word(strings,k)\==word(strings,k-1)  then return 0        /*string=prev? */
            end   /*k*/                          /* [↑]     0=false,   [↓] 1=true.      */
          return 1                               /*indicate that all strings are equal. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ifAscend: procedure; parse arg strings           /*set  STRINGS  to a string in the list*/
            do k=2  to words(strings)            /*scan the strings in the list.        */
            if word(strings,k)<<=word(strings,k-1)  then return 0        /*string>prev? */
            end   /*k*/                          /*  [↑]    0=false,     [↓]    1=true. */
          return 1                               /*indicate that strings are ascending. */
```

{{out|output|text=  when using the supplied lists:>>

```txt

══════════ ayu dab dog gar panda tui yak══════════
  The strings are ascending.


══════════ oy oy oy oy oy oy oy oy oy oy══════════
  The strings are all equal.


══════════ somehow   somewhere  sometime══════════


════════════════════ Hoosiers═════════════════════
  The strings are all equal.
  The strings are ascending.


════════════════════════ ═════════════════════════
  The strings are all equal.
  The strings are ascending.

```



### version 3

This REXX version is more idiomatic.

```rexx
/*REXX program compares a list of strings for:  equality, all ascending.                */
@.1= 'ayu dab dog gar panda tui yak'             /*seven strings: they're all ascending.*/
@.2= 'oy oy oy oy oy oy oy oy oy oy'             /*  ten strings:         all equal.    */
@.3= 'somehow   somewhere  sometime'             /*three strings:   ¬equal,  ¬ascending.*/
@.4= 'Hoosiers'                                  /*only a single string is defined.     */
@.5=                                             /*Null.   That is,  no strings here.   */
#= 5;         do j=1  for #;    say;   say       /* [↓]  traipse through all the lists. */
              say center(' '@.j, 50, "═")        /*display a centered title/header.     */
              if cStr(@.j, 'Equal'    )  then  say  "  The strings are all equal."
              if cStr(@.j, 'Ascending')  then  say  "  The strings are ascending."
              end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cStr: procedure; parse arg x;  arg , how 2       /*set X to list; get 1st char of arg #2*/
              do k=2  to words(x)                /*scan the strings in the list.        */
              if how=='E'  then if word(x,k) \== word(x,k-1)  then return 0   /*¬=prev.?*/
              if how=='A'  then if word(x,k) <<= word(x,k-1)  then return 0   /*≤ prev.?*/
              end   /*k*/                        /* [↓]   1=true.       [↑]   0=false.  */
      return 1                                   /*indicate strings have true comparison*/
```

{{out|output|text=  is identical to the above REXX version. 




## Ring


```ring

cString1 = "hello"
cString2 = "hello"
compare(cString1,cString2)
cString1 = "abc"
cString2 = "bcd"
compare(cString1,cString2)
cString1 = "bcd"
cString2 = "abc"
compare(cString1,cString2)

func compare aString, bString
     n = strcmp(aString,bString)
     if n = 0 see aString + " = " + bString + nl
     but n < 0 see aString + " < " + bString + nl
     but n > 0 see aString + " > " + bString + nl ok

```



## Ruby


```ruby
strings.uniq.one?                 # all equal?
strings == strings.uniq.sort      # ascending?
```


Short circuiting: 

```ruby
strings.all?{|str| str == strings.first} # all equal?
strings.each_cons(2).all?{|str1, str2| str1 < str2} # ascending?
```



## Rust



```rust
// Note that this solution uses the feature 'slice_patterns' which is available Rust nightly!
#![feature(slice_patterns)]

fn strings_are_equal(seq: &[&str]) -> bool {
    match seq {
        &[] | &[_] => true,
        &[x, y, ref tail..] if x == y => strings_are_equal(&[&[y], tail].concat()),
        _ => false
    }
}

fn asc_strings(seq: &[&str]) -> bool {
    match seq {
        &[] | &[_] => true,
        &[x, y, ref tail..] if x < y => asc_strings(&[&[y], tail].concat()),
        _ => false
    }
}
```


=={{header|S-lang}}==
"Simple Loop" and "Array Idiomatic" versions:
<lang S-lang>define equal_sl(sarr)
{
  variable n = length(sarr), a0, i;
  if (n < 2) return 1;  

  a0 = sarr[0];
  _for i (1, length(sarr)-1, 1)
    if (sarr[i] != a0) return 0;

  return 1;
}
define ascending_sl(sarr) {
  variable n = length(sarr), a0, i;
  if (n < 2) return 1;  

  _for i (0, length(sarr)-2, 1)
    if (sarr[i] >= sarr[i+1]) return 0;

  return 1;
}


define equal_ai(sarr) {
  if (length(sarr) < 2) return 1;
  variable s0 = sarr[0];
  return all(sarr[[1:]] == s0);
}

define ascending_ai(sarr) {
  variable la = length(sarr);
  if (la < 2) return 1;  
  return all(sarr[[0:la-2]] < sarr[[1:la-1]]);
}

define atest(a) {
  () = printf("\n");
  print(a);

  () = printf("equal_sl=%d, ascending_sl=%d\n",
              equal_sl(a), ascending_sl(a));
  () = printf("equal_ai=%d, ascending_ai=%d\n",
              equal_ai(a), ascending_ai(a));
}
              
atest(["AA","BB","CC"]);
atest(["AA","AA","AA"]);
atest(["AA","CC","BB"]);
atest(["AA","ACB","BB","CC"]);
atest(["single_element"]);
atest(NULL);

```

{{out}}

```txt
"AA"
"BB"
"CC"
equal_sl=0, ascending_sl=1
equal_ai=0, ascending_ai=1

"AA"
"AA"
"AA"
equal_sl=1, ascending_sl=0
equal_ai=1, ascending_ai=0

"AA"
"CC"
"BB"
equal_sl=0, ascending_sl=0
equal_ai=0, ascending_ai=0

"AA"
"ACB"
"BB"
"CC"
equal_sl=0, ascending_sl=1
equal_ai=0, ascending_ai=1

"single_element"
equal_sl=1, ascending_sl=1
equal_ai=1, ascending_ai=1

NULL
equal_sl=1, ascending_sl=1
equal_ai=1, ascending_ai=1

```


## Scala

Functions implemented in Scala following a functional paradigm

```Scala

def strings_are_equal(seq:List[String]):Boolean = seq match {
    case Nil => true
    case s::Nil => true
    case el1 :: el2 :: tail => el1==el2 && strings_are_equal(el2::tail)
}

def asc_strings(seq:List[String]):Boolean = seq match {
    case Nil => true
    case s::Nil => true
    case el1 :: el2 :: tail => el1.compareTo(el2) < 0
}


```

{{out}}

```txt


'''Sample tests:'''

scala> strings_are_equal(List("asdf"))
res3: Boolean = true

scala> strings_are_equal(List("asdf","asdf","sf"))
res5: Boolean = false

scala> asc_strings(List())
res10: Boolean = true

scala> asc_strings(List("asdfas","fds"))
res11: Boolean = true

scala> asc_strings(List("sdfa","asfsdf","afas","asf"))
res8: Boolean = false

```



## Scheme


For known lists that are 'short-enough', the simplest solution uses 'apply', but that relies on the list being shorter than the maximum number of arguments a function can accept.  Better is to write a simple loop:


```scheme

(define (compare-strings fn strs)
  (or (null? strs)                             ; returns #t on empty list
      (null? (cdr strs))                       ; returns #t on list of size 1
      (do ((fst strs (cdr fst))
           (snd (cdr strs) (cdr snd)))
        ((or (null? snd)
             (not (fn (car fst) (car snd))))
         (null? snd)))))                       ; returns #t if the snd list is empty, meaning all comparisons are exhausted

(compare-strings string=? strings) ; test for all equal
(compare-strings string<? strings) ; test for in ascending order

```




## Sidef

Short-circuiting:

```ruby
1..arr.end -> all{ arr[0] == arr[_] }   # all equal
1..arr.end -> all{ arr[_-1] < arr[_] }  # strictly ascending
```


Non short-circuiting:

```ruby
arr.uniq.len == 1      # all equal
arr == arr.uniq.sort   # strictly ascending
```



## Tcl

The command form of the <code>eq</code> and <code>&lt;</code> operators (introduced in Tcl 8.5) handle arbitrarily many arguments and will check if they're all equal/ordered. 
Making the operators work with a list of values is just a matter of using the expansion syntax with them.

```tcl
tcl::mathop::eq {*}$strings;		# All values string-equal
tcl::mathop::< {*}$strings;		# All values in strict order
```




## VBA


```vb

Private Function IsEqualOrAscending(myList) As String
Dim i&, boolEqual As Boolean, boolAsc As Boolean

    On Error Resume Next
    If UBound(myList) > 0 Then
        If Err.Number > 0 Then
            IsEqualOrAscending = "Error " & Err.Number & " : Empty array"
            On Error GoTo 0
            Exit Function
        Else
            For i = 1 To UBound(myList)
                If myList(i) <> myList(i - 1) Then boolEqual = True
                If myList(i) <= myList(i - 1) Then boolAsc = True
            Next
        End If
    End If
    IsEqualOrAscending = "List : " & Join(myList, ",") & ", IsEqual : " & (Not boolEqual) & ", IsAscending : " & Not boolAsc
End Function

```

Call :

```vb

Sub Main()
Dim List
    Debug.Print IsEqualOrAscending(Array("AA", "BB", "CC"))
    Debug.Print IsEqualOrAscending(Array("AA", "AA", "AA"))
    Debug.Print IsEqualOrAscending(Array("AA", "CC", "BB"))
    Debug.Print IsEqualOrAscending(Array("AA", "ACB", "BB", "CC"))
    Debug.Print IsEqualOrAscending(Array("single_element"))
    Debug.Print IsEqualOrAscending(Array("AA", "BB", "BB"))
    'test with Empty Array :
    Debug.Print IsEqualOrAscending(List)
End Sub

```

{{Out}}

```txt

List : AA,BB,CC, IsEqual : False, IsAscending : True
List : AA,AA,AA, IsEqual : True, IsAscending : False
List : AA,CC,BB, IsEqual : False, IsAscending : False
List : AA,ACB,BB,CC, IsEqual : False, IsAscending : True
List : single_element, IsEqual : True, IsAscending : True
List : AA,BB,BB, IsEqual : False, IsAscending : False
Error 13 : Empty array

```


## VBScript


```vb

Function string_compare(arr)
	lexical = "Pass"
	ascending = "Pass"
	For i = 0 To UBound(arr)
		If i+1 <= UBound(arr) Then
			If arr(i) <> arr(i+1) Then
				lexical = "Fail"
			End If
			If arr(i) >= arr(i+1) Then
				ascending = "Fail"
			End If 
		End If	
	Next
	string_compare = "List: " & Join(arr,",") & vbCrLf &_
					 "Lexical Test: " & lexical & vbCrLf &_
					 "Ascending Test: " & ascending & vbCrLf
End Function

WScript.StdOut.WriteLine string_compare(Array("AA","BB","CC"))
WScript.StdOut.WriteLine string_compare(Array("AA","AA","AA"))
WScript.StdOut.WriteLine string_compare(Array("AA","CC","BB"))
WScript.StdOut.WriteLine string_compare(Array("AA","ACB","BB","CC"))
WScript.StdOut.WriteLine string_compare(Array("FF"))

```


{{Out}}

```txt

List: AA,BB,CC
Lexical Test: Fail
Ascending Test: Pass

List: AA,AA,AA
Lexical Test: Pass
Ascending Test: Fail

List: AA,CC,BB
Lexical Test: Fail
Ascending Test: Fail

List: AA,ACB,BB,CC
Lexical Test: Fail
Ascending Test: Pass

List: FF
Lexical Test: Pass
Ascending Test: Pass

```



## zkl

These short circuit.

```zkl
fcn allEQ(strings){ (not strings.filter1('!=(strings[0]))) }
fcn monoUp(strings){
   strings.len()<2 or
   strings.reduce(fcn(a,b){ if(a>=b) return(Void.Stop,False); b }).toBool() 
}
```


```zkl
allEQ(T("AA")).println();                //True
allEQ(T("AA","AA","AA","AA")).println(); //True
allEQ(T("A", "AA","AA","AA")).println(); //False

monoUp(T("a")).println();                   //True
monoUp(T("a","aa","aaa","aaaa")).println(); //True
monoUp(T("a","aa","aaa","aaa")).println();  //False
monoUp(T("a","b","c","cc")).println();      //True
```



## zonnon


```zonnon

module CompareStrings;
type
	Vector = array * of string;
var
	v,w: Vector;
	i: integer;
	all,ascending: boolean;
begin
	v := new Vector(3);
	v[0] := "uno";
	v[1] := "uno";
	v[2] := "uno";

	all := true;
	for i := 1 to len(v) - 1 do
		all := all & (v[i - 1] = v[i]);
	end;

	w := new Vector(3);
	w[0] := "abc";
	w[1] := "bcd";
	w[2] := "cde";
	v := w;
	ascending := true;
	for i := 1 to len(v) - 1 do
		ascending := ascending & (v[i - 1] <= v[i])
	end;

	write("all equals?: ");writeln(all);
	write("ascending?: ");writeln(ascending)
end CompareStrings.

```


## ZX Spectrum Basic

{{trans|AWK}}

```zxbasic
10 FOR j=160 TO 200 STEP 10
20 RESTORE j
30 READ n
40 LET test1=1: LET test2=1
50 FOR i=1 TO n
60 READ a$
70 PRINT a$;" ";
80 IF i=1 THEN GO TO 110
90 IF p$<>a$ THEN LET test1=0
100 IF p$>=a$ THEN LET test2=0
110 LET p$=a$
120 NEXT i
130 PRINT 'test1'test2
140 NEXT j
150 STOP 
160 DATA 3,"AA","BB","CC"
170 DATA 3,"AA","AA","AA"
180 DATA 3,"AA","CC","BB"
190 DATA 4,"AA","ACB","BB","CC"
200 DATA 1,"single_element"
```

