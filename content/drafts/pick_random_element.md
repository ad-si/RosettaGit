+++
title = "Pick random element"
description = ""
date = 2019-10-18T11:50:10Z
aliases = []
[extra]
id = 10267
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}} [[Category:Randomness]]
Demonstrate how to pick a random element from a list.


## ACL2



```Lisp
:set-state-ok t

(defun pick-random-element (xs state)
   (mv-let (idx state)
           (random$ (len xs) state)
      (mv (nth idx xs) state)))
```



## Ada


The following program generates three 20-letter words.
Each vowel and each consonant is picked randomly from a list of vowels
resp. a list of consonants.


```Ada
with Ada.Text_IO, Ada.Numerics.Float_Random;

procedure Pick_Random_Element is

   package Rnd renames Ada.Numerics.Float_Random;
   Gen: Rnd.Generator; -- used globally

   type Char_Arr is array (Natural range <>) of Character;

   function Pick_Random(A: Char_Arr) return Character is
      -- Chooses one of the characters of A (uniformly distributed)
   begin
      return A(A'First + Natural(Rnd.Random(Gen) * Float(A'Last)));
   end Pick_Random;

   Vowels    : Char_Arr := ('a', 'e', 'i', 'o', 'u');
   Consonants: Char_Arr := ('t', 'n', 's', 'h', 'r', 'd', 'l');
   Specials  : Char_Arr := (',', '.', '?', '!');

begin
   Rnd.Reset(Gen);
   for J in 1 .. 3 loop
      for I in 1 .. 10 loop
         Ada.Text_IO.Put(Pick_Random(Consonants));
         Ada.Text_IO.Put(Pick_Random(Vowels));
      end loop;
      Ada.Text_IO.Put(Pick_Random(Specials) & " ");
   end loop;
   Ada.Text_IO.New_Line;
end Pick_Random_Element;
```


{{out}}

```txt
horanohesuhodinahiru. desehonirosedisinelo, losihehederidonolahe?
```



## Aime


```aime
list l;

l_append(l, 'a');
l_append(l, 'b');
l_append(l, 'c');
l_append(l, 'd');
l_append(l, 'e');
l_append(l, 'f');

o_byte(l[drand(5)]);
o_byte('\n');
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}

```algol68
# pick a random element from an array of strings #

OP PICKRANDOM = ( []STRING list )STRING:
BEGIN

    INT number of elements = ( UPB list - LWB list ) + 1;
    INT random element     =
        ENTIER ( next random * ( number of elements ) );

    list[ LWB list + random element ]
END; # PICKRANDOM #
# can define additional operators for other types of array #


main: (

    []STRING days = ( "Sunday",   "Monday", "Tuesday", "Wednesday"
                    , "Thursday", "Friday", "Saturday"
                    );

    print( ( PICKRANDOM days, newline ) )

)
```

{{out}}

```txt

Thursday

```


## App Inventor

App Inventor has the block '''pick a random item''' for selecting a random item from a list.

[https://lh4.googleusercontent.com/-FaK4IeMmyI0/Uuv4G83dpYI/AAAAAAAAJ-E/3gzO5jUMwxE/s1600/Capture.PNG CLICK HERE TO VIEW THE BLOCKS AND ANDROID APP DISPLAY]


## AppleScript


```AppleScript
get some item of [1, "two", pi, "4", 5 > 4, 5 + 1, Sunday]
```

{{out}}

```txt
"two"
```



## AutoHotkey

; True Arrays
{{works with|AutoHotkey_L}}

```AHK
list := ["abc", "def", "gh", "ijklmnop", "hello", "world"]
Random, randint, 1, % list.MaxIndex()
MsgBox % List[randint]
```

; Pseudo-Arrays
{{works with|AutoHotkey_Basic}}

```AutoHotkey
list := "abc,def,gh,ijklmnop,hello,world"
StringSplit list, list, `,
Random, randint, 1, %list0%
MsgBox % List%randint%
```


## AWK


```AWK
# syntax: GAWK -f PICK_RANDOM_ELEMENT.AWK
BEGIN {
    n = split("Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday",day_of_week,",")
    srand()
    x = int(n*rand()) + 1
    printf("%s\n",day_of_week[x])
    exit(0)
}
```

{{out}}

```txt
GAWK -f PICK_RANDOM_ELEMENT.AWK
Sunday
GAWK -f PICK_RANDOM_ELEMENT.AWK
Monday
GAWK -f PICK_RANDOM_ELEMENT.AWK
Wednesday
GAWK -f PICK_RANDOM_ELEMENT.AWK
Tuesday
```



## BaCon

This is simply an application of a ranged random number used as an array index. '''BaCon''' has no built in random element selector.


```freebasic
' Pick random element
OPTION BASE 1
DECLARE words$[6]
FOR i = 1 TO 6 : READ words$[i] : NEXT
DATA "Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta"

element = RANDOM(6) + 1
PRINT "Chose ", element, ": ", words$[element]
```


{{out}}

```txt
prompt$ ./pick-random-element
Chose 2: Beta
prompt$ ./pick-random-element
Chose 1: Alpha
prompt$ ./pick-random-element
Chose 5: Epsilon
```



## Bash


```Bash
# borrowed from github.com/search?q=bashnative

rand() {
	printf $((  $1 *  RANDOM  / 32767   ))
}
rand_element () {
    local -a th=("$@")
    unset th[0]
    printf $'%s\n' "${th[$(($(rand "${#th[*]}")+1))]}"
}

echo "You feel like a $(rand_element pig donkey unicorn eagle) today"
```



## BASIC

{{works with|QBasic}}
{{works with|PowerBASIC}}

Note the use of <code>LBOUND</code> and <code>UBOUND</code>. This is only necessary for arrays where the lower and upper limits aren't known. In this example, we know they are 0 and 10 respectively, and could have hard-coded those numbers. (For that matter, the "random selection" line could've just been entered as <code>x = INT(RND * 11)</code>.)


```qbasic
'setup
DIM foo(10) AS LONG
DIM n AS LONG, x AS LONG
FOR n = LBOUND(foo) TO UBOUND(foo)
    foo(n) = INT(RND*99999)
NEXT
RANDOMIZE TIMER

'random selection
x = INT(RND * ((UBOUND(foo) - LBOUND(foo)) + 1))

'output
PRINT x, foo(x)
```


See also: [[#Liberty BASIC|Liberty BASIC]], [[#PureBasic|PureBasic]], [[#Run BASIC|Run BASIC]]

=
## Commodore BASIC
=

```qbasic
10 DIM A$(9)
20 FOR I=0 TO 9 : READ A$(I) : NEXT
30 X = RND(-TI) : REM 'PLANT A RANDOM SEED'
40 X = INT(RND(1)*10)
50 PRINT A$(X)
60 END
100 DATA ALPHA, BRAVO, CHARLIE, DELTA, ECHO
110 DATA FOXTROT, GOLF, HOTEL, INDIA, JULIETT
```



## Batch File

Since there is no arrays in Batch File, I will use a 1-based pseudo-array.

```dos
@echo off
setlocal enabledelayedexpansion

	::Initializing the pseudo-array...
set "pseudo=Alpha Beta Gamma Delta Epsilon"
set cnt=0 & for %%P in (!pseudo!) do (
	set /a cnt+=1
	set "pseudo[!cnt!]=%%P"
)
	::Do the random thing...
set /a rndInt=%random% %% cnt +1

	::Print the element corresponding to rndint...
echo.!pseudo[%rndInt%]!
pause
exit /b
```

{{Out|Sample Outputs}}

```txt
Delta
Press any key to continue . . .

Gamma
Press any key to continue . . .

Epsilon
Press any key to continue . . .

Gamma
Press any key to continue . . .
```



## BBC BASIC


```bbcbasic
      DIM list$(5)
      list$() = "The", "five", "boxing", "wizards", "jump", "quickly"
      chosen% = RND(6)
      PRINT "Item " ; chosen% " was chosen which is '" list$(chosen%-1) "'"
```

{{out}}

```txt

Item 4 was chosen which is 'wizards'

```



## Burlesque



```burlesque

blsq ) "ABCDEFG"123456 0 6rn-]!!
'G

```


''123456'' is the random seed. In order to pick another element you have to change the random seed.


## C



```c

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(){
   char array[] = { 'a', 'b', 'c','d','e','f','g','h','i','j' };
   int i;
   time_t t;
   srand((unsigned)time(&t));

   for(i=0;i<30;i++){
		printf("%c\n", array[rand()%10]);
   }

   return 0;
}

```

Output

```txt

a
e
f
h
b
d
g
a
b
f
a
i
b
d
d
g
j
a
f
e
a
e
g
e
i
d
j
a
f
e
a

```



## C++


```cpp
#include <iostream>
#include <random>
#include <vector>

int main( ) {
   std::vector<int> numbers { 11 , 88 , -5 , 13 , 4 , 121 , 77 , 2 } ;
   std::random_device seed ;
   // generator
   std::mt19937 engine( seed( ) ) ;
   // number distribution
   std::uniform_int_distribution<int> choose( 0 , numbers.size( ) - 1 ) ;
   std::cout << "random element picked : " << numbers[ choose( engine ) ]
      << " !\n" ;
   return 0 ;
}
```



## C#


```c#
using System;
using System.Collections.Generic;

class RandomElementPicker {
  static void Main() {
    var list = new List<int>(new[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
    var rng = new Random();
    var randomElement = list[rng.Next(list.Count)];
    Console.WriteLine("I picked element {0}", randomElement);
  }
}
```



## Ceylon


```ceylon
import ceylon.random {

	DefaultRandom
}

shared void run() {
    value random = DefaultRandom();
    value element = random.nextElement([1, 2, 3, 4, 5, 6]);
    print(element);
}
```



## Clojure


```Clojure
(rand-nth coll)
```


where <code>coll</code> is some sequential collection. Equivalent to:


```Clojure
(nth coll (rand-int (count coll)))
```



## COBOL

{{works with|GNU Cobol}}

```cobol>       >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. random-element.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  nums-area                           VALUE "123456789".
    03  nums                            PIC 9 OCCURS 9 TIMES.

01  random-idx                          PIC 9 COMP.

PROCEDURE DIVISION.
    COMPUTE random-idx = FUNCTION RANDOM(FUNCTION CURRENT-DATE (9:7)) * 9 + 1
    DISPLAY nums (random-idx)
    .
END PROGRAM random-element.
```



## CoffeeScript


```coffeescript
array = [1,2,3]
console.log array[Math.floor(Math.random() * array.length)]
```



## Common Lisp


```lisp
(defvar *list* '(one two three four five))

(print (nth (random (length *list*)) *list*))
(print (nth (random (length *list*)) *list*))
(print (nth (random (length *list*)) *list*))
```


{{out}}

```txt
FIVE
THREE
ONE
```



## Crystal


```Ruby

puts [1, 2, 3, 4, 5].sample(1)

```



## D


```d
import std.stdio, std.random;

void main() {
    const items = ["foo", "bar", "baz"];
    items[uniform(0, $)].writeln;
}
```



## Delphi

See [[#Pascal / Delphi / Free Pascal]].

=={{header|Déjà Vu}}==

```dejavu
!print choose [ "one" "two" "chicken" ]
```


## EasyLang



```easyprog.online
ar$[] = [ "spring" "summer" "autumn" "winter" ]
print ar$[random len ar$[]]
```



## EchoLisp


```lisp

(define (pick-random list)
    (list-ref list (random (length list))))
(pick-random (iota 1000)) → 667
(pick-random (iota 1000)) → 179

```


## Elena

ELENA 4.1 :

```elena
import extensions;

extension listOp
{
    randomItem()
        = self[randomGenerator.eval(self.Length)];
}

public program()
{
    var item := new int[]::(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    console.printLine("I picked element ",item.randomItem())
}
```



## Elixir

{{works with|Elixir|1.2}}

```elixir
iex(1)> list = Enum.to_list(1..20)
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
iex(2)> Enum.random(list)
19
iex(3)> Enum.take_random(list,4)
[19, 20, 7, 15]
```



## Emacs Lisp


```Lisp

(defun random-choice (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(random-choice '("a" "b" "c"))
;; => "a"

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(pick_random).
-export([main/0]).

main() ->
	List =[1,2,3,4,5],
	Index = rand:uniform(length(List)),
	lists:nth(Index,List).

```



## Euphoria


```euphoria
constant s = {'a', 'b', 'c'}
puts(1,s[rand($)])
```


## Factor


```factor
( scratchpad ) { "a" "b" "c" "d" "e" "f" } random .
"a"
```



## Falcon


```falcon

lst = [1, 3, 5, 8, 10]
> randomPick(lst)

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
program pick_random
  implicit none

  integer :: i
  integer :: a(10) = (/ (i, i = 1, 10) /)
  real :: r

  call random_seed
  call random_number(r)
  write(*,*) a(int(r*size(a)) + 1)
end program
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim a(0 To 9) As String = {"Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"}

Randomize
Dim randInt As Integer

For i As Integer = 1 To 5
  randInt = Int(Rnd * 10)
  Print a(randInt)
Next
Sleep
```


Sample output :
{{out}}

```txt

Zero
Seven
Three
Nine
Three

```



## Free Pascal

See [[#Pascal / Delphi / Free Pascal]].

=={{header|F_Sharp|F#}}==

```fsharp
let list = ["a"; "b"; "c"; "d"; "e"]
let rand = new System.Random()
printfn "%s" list.[rand.Next(list.Length)]
```



## Gambas

'''[https://gambas-playground.proko.eu/ You can run this code. Copy the code, click this link, paste it in and press 'Run !']'''

```gambas
Public Sub Main()
Dim sList As String[] = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

Print sList[Rand(0, 11)]

End
```

Output:

```txt

May

```



## GAP


```gap
a := [2, 9, 4, 7, 5, 3];
Random(a);
```


This works with many GAP objects, for instance groups:


```gap
Random(SymmetricGroup(20));

(1,4,8,2)(3,12)(5,14,10,18,17,7,16)(9,13)(11,15,20,19)
```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

var list = []string{"bleen", "fuligin", "garrow", "grue", "hooloovoo"}

func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println(list[rand.Intn(len(list))])
}
```



## Groovy

Solution:

```groovy
def list = [25, 30, 1, 450, 3, 78]
def random = new Random();

(0..3).each {
    def i = random.nextInt(list.size())
    println "list[${i}] == ${list[i]}"
}
```


{{out}}

```txt
list[3] == 450
list[2] == 1
list[5] == 78
list[3] == 450
```


Alternate Solution:

```groovy

[25, 30, 1, 450, 3, 78].sort{new Random()}?.take(1)[0]

```



## Haskell

Creating a custom function:


```haskell
import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

x <- pick [1, 2, 3]
```


Using the random-fu library:


```haskell
import Data.Random
sample $ randomElement  [1, 2, 3]
```


For example:

```haskell
do
  x <- sample $ randomElement  [1, 2, 3]
  print x
```


=={{header|Icon}} and {{header|Unicon}}==
The unary operator '?' selects a random element from its argument which may be a string, list, table, or set.


```Icon
procedure main()
   L := [1,2,3]  # a list
   x := ?L       # random element
end
```



## J


```j
   ({~ ?@#) 'abcdef'
b
```



## Java


```java
import java.util.Random;
...
int[] array = {1,2,3};
return array[new Random().nextInt(array.length)]; // if done multiple times, the Random object should be re-used
```


For a List object rather than an array, substitute <code>list.get(...)</code> for <code>array[...]</code>. If preserving the order of the List isn't important, you could call <code>Collections.shuffle(list);</code> and then <code>list.get(0);</code>. You would need to shuffle each time unless you <code>remove</code>d the item from the list.


## JavaScript


```javascript
var array = [1,2,3];
return array[Math.floor(Math.random() * array.length)];
```



## Julia


```julia
array = [1,2,3]
rand(array)
```



## K


```K
  1?"abcdefg"
,"e"
```



## Kotlin


```scala
// version 1.2.10

import java.util.Random

/**
 * Extension function on any list that will return a random element from index 0
 * to the last index
 */
fun <E> List<E>.getRandomElement() = this[Random().nextInt(this.size)]

/**
 * Extension function on any list that will return a list of unique random picks
 * from the list. If the specified number of elements you want is larger than the
 * number of elements in the list it returns null
 */
fun <E> List<E>.getRandomElements(numberOfElements: Int): List<E>? {
    if (numberOfElements > this.size) {
        return null
    }
    return this.shuffled().take(numberOfElements)
}

fun main(args: Array<String>) {
    val list = listOf(1, 16, 3, 7, 17, 24, 34, 23, 11, 2)
    println("The list consists of the following numbers:\n${list}")

    // notice we can call our extension functions as if they were regular member functions of List
    println("\nA randomly selected element from the list is ${list.getRandomElement()}")
    println("\nA random sequence of 5 elements from the list is ${list.getRandomElements(5)}")
}
```


Sample output:
{{out}}

```txt

The list consists of the following numbers:
[1, 16, 3, 7, 17, 24, 34, 23, 11, 2]

A randomly selected element from the list is 11

A random sequence of 5 elements from the list is [17, 24, 23, 16, 3]

```



## LabVIEW

{{VI snippet}}<br/>[[File:LabVIEW_Pick_random_element.png]]


## Lasso


```Lasso
local(
	my array = array('one', 'two', 3)
)

#myarray -> get(integer_random(#myarray -> size, 1))
```


-> two


## Liberty BASIC

The natural way to hold an array of text is in a space- or comma-delimited string, although an array could be used.

```lb
list$ ="John Paul George Ringo Peter Paul Mary Obama Putin"
wantedTerm =int( 10 *rnd( 1))
print "Selecting term "; wantedTerm; " in the list, which was "; word$( list$, wantedTerm, " ")
```

 Selecting term 5 in the list, which was Peter


## LiveCode


```LiveCode
put "Apple,Banana,Peach,Apricot,Pear" into fruits
put item (random(the number of items of fruits)) of fruits
```



## Logo

{{works with|UCB Logo}}

```logo
pick [1 2 3]
```



## Lua



```lua
math.randomseed(os.time())
local a = {1,2,3}
print(a[math.random(1,#a)])
```



## Maple


```maple
a := [bear, giraffe, dog, rabbit, koala, lion, fox, deer, pony]:
randomNum := rand(1 ..numelems(a)):
a[randomNum()];
```



## Mathematica


```Mathematica
RandomChoice[{a, b, c}]
->c
```


=={{header|MATLAB}} / {{header|Octave}}==
In case list is a cell array:

```Matlab
        list = {'a','b','c'};
	list{ceil(rand(1)*length(list))}
```


If list is a vector:

```Matlab
        list = 1:1000;
	list(ceil(rand(1)*length(list)))
```



## Maxima


```Maxima
random_element(l):= part(l, 1+random(length(l)));
/*  (%i1) random_element(['a, 'b, 'c]);
    (%o1)                                  c
*/
```


=={{header|МК-61/52}}==
<lang>0 П0 1 П1 2 П2 3 П3 4 П4 5

^ СЧ * [x] ПE КИПE С/П
```



## NetLogo


```NetLogo
;; from list containnig only literals and literal constants
user-message one-of [ 1 3 "rooster" blue ]
;; from list containing variables and reporters
user-message one-of (list (red + 2) turtles (patch 0 0) )
```



## NetRexx


```netrexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

iArray = [ 1, 2, 3, 4, 5 ]     -- a traditional array
iList = Arrays.asList(iArray)  -- a Java Collection "List" object
iWords = '1 2 3 4 5'           -- a list as a string of space delimited words


v1 = iArray[Random().nextInt(iArray.length)]
v2 = iList.get(Random().nextInt(iList.size()))
v3 = iWords.word(Random().nextInt(iWords.words()) + 1) -- the index for word() starts at one

say v1 v2 v3

```




## NewLISP


```NewLISP

(define (pick-random-element R)
	(nth (rand (length R)) R))

```

Example:

```txt

(setq X '("alpha" "beta" "gamma" "delta" "epsilon"))
(println (pick-random-element X))
(println (pick-random-element X))
(println (pick-random-element X))
(println (pick-random-element X))

```




## Nim


```nim
randomize()

let ls = @["foo", "bar", "baz"]
echo ls.rand()
```



## Objeck


```objeck
values := [1, 2, 3];
value := values[(Float->Random() * 100.0)->As(Int) % values->Size()];
```



## OCaml

With a list:

```ocaml
let list_rand lst =
  let len = List.length lst in
  List.nth lst (Random.int len)
```



```txt

# list_rand [1;2;3;4;5] ;;
- : int = 3

```


With an array:

```ocaml
let array_rand ary =
  let len = Array.length ary in
  ary.(Random.int len)
```



```txt

# array_rand [|1;2;3;4;5|] ;;
- : int = 3

```




## Oforth



```Oforth
: pickRand(l)   l size rand l at ;
```



## Ol


```scheme

(import (otus random!))

(define x '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(print (list-ref x (rand! (length x))))

```



## PARI/GP


```parigp
pick(v)=v[random(#v)+1]
```


=={{header|Pascal}} / {{header|Delphi}} / {{header|Free Pascal}}==

```pascal
Program PickRandomElement (output);

const
  s: array [1..5] of string = ('1234', 'ABCDE', 'Charlie', 'XB56ds', 'lala');

begin
  randomize;
  writeln(s[low(s) + random(length(s))]);
end.
```



## Perl


```perl
my @array = qw(a b c);
print $array[ rand @array ];
```



## Perl 6

{{Works with|rakudo|2015-12-07}}
In a nutshell, picking an element from a list
is implemented with a method conveniently called "pick":

```perl6
say (1, 2, 3).pick;
```


There are various ways of doing something similar, though.
Perl 6 has actually two methods (with associated functional forms)
to return random elements depending on whether you are doing selection
with or without replacement.

Selection with replacement: (roll of a die)

```perl6
say (1..6).roll;          # return 1 random value in the range 1 through 6
say (1..6).roll(3);       # return a list of 3 random values in the range 1 through 6
say (1..6).roll(*)[^100]; # return first 100 values from a lazy infinite list of random values in the range 1 through 6
```


Selection without replacement: (pick a card from a deck)

```perl6
# define the deck
my @deck = <2 3 4 5 6 7 8 9 J Q K A> X~ <♠ ♣ ♥ ♦>;
say @deck.pick;    # Pick a card
say @deck.pick(5); # Draw 5
say @deck.pick(*); # Get a shuffled deck
```

Or you can always use the normal <tt>rand</tt> built-in
to generate a subscript (which automatically truncates any fractional part):

```perl6
@array[@array * rand]
```

However, the <tt>pick</tt> and <tt>roll</tt> methods (not to be confused
with the pick-and-roll method in basketball) are more general
insofar as they may be used on any enumerable type:

```perl6>say Bool.pick;  # returns either True or False</lang



## Phix


```Phix
constant s = {'a','b','c'}
puts(1,s[rand(length(s))])
```



## PHP


```php
$arr = array('foo', 'bar', 'baz');
$x = $arr[array_rand($arr)];
```



## PicoLisp


```PicoLisp
(get Lst (rand 1 (length Lst)))
```



## PL/I


```pli
   declare t(0:9) character (1) static initial
      ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j');
   put ( t(10*random()) );
```

{{out}}

```txt

e

```



## Powershell

Powershell has Get-Random Cmdlet which one of its overload is to select randomly from a given list


```Powershell

1..100 | Get-Random -Count 3

```



## Prolog

{{works with|SWI-Prolog|6}}


```prolog

?- random_member(M, [a, b, c, d, e, f, g, h, i, j]).
M = i.

```



## PureBasic


```PureBasic
Procedure.s pickRandomElement(List source.s())
  Protected x = ListSize(source())

  If x > 0
    SelectElement(source(), Random(x - 1)) ;element numbering is zero - based
    ProcedureReturn source()
  EndIf
EndProcedure

;initialize list elements
DataSection
  elements:
  Data.s "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"
EndDataSection

#elementCount = 10
NewList item.s()

Restore elements
Define i
For i = 1 To #elementCount
  AddElement(item())
  Read.s item()
Next

If OpenConsole()
  Print("Source list:  ")
  ForEach item()
    Print(item() + " ")
  Next
  PrintN(#CRLF$)

  Print("Random picks from list:  ")
  For i = 1 To 10
    Print(pickRandomElement(item()) + " ")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
Source list:  One Two Three Four Five Six Seven Eight Nine Ten

Random picks from list:  Seven Nine Two Six Four Four Nine Three Six Two
```


### Easy version


```purebasic
OpenConsole()

a$="One" +#TAB$+ "Two"  +#TAB$+ "Three" +#TAB$+ "Four" +#TAB$+ "Five" +#TAB$+
   "Six" +#TAB$+ "Seven"+#TAB$+ "Eight" +#TAB$+ "Nine" +#TAB$+ "Ten"  +#TAB$

Print("Source list: "+#TAB$+a$+#CRLF$+"Random list: "+#TAB$)

For i=1 To CountString(a$,#TAB$)
  Print(StringField(a$,Random(CountString(a$,#TAB$),1),#TAB$)+#TAB$)
Next
Input()
```

{{out}}

```txt
Source list:    One     Two     Three   Four    Five    Six     Seven   Eight   Nine    Ten
Random list:    One     Two     Seven   Nine    Ten     Seven   Three   Five    Ten     Nine
```



## Python


```python>>>
 import random
>>> random.choice(['foo', 'bar', 'baz'])
'baz'
```



## R


```rsplus
# a vector (letters are builtin)
letters
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"

# picking one element
sample(letters, 1)
# [1] "n"

# picking some elements with repetition, and concatenating to get a word
paste(sample(letters, 10, rep=T), collapse="")
# [1] "episxgcgmt"
```



## Racket



```Racket

#lang racket
(define (pick-item l)
  (list-ref l (random (length l))))

```



## REXX


### version 1

This REXX example takes the Rosetta Code task very literally.

```rexx
/*REXX program picks a   random element   from a list   (tongue in cheek, a visual pun).*/
_=  'hydrogen helium lithium beryllium boron carbon nitrogen oxygen fluorine neon sodium'
_=_ 'magnesium aluminum silicon phosphorous sulfur chlorine argon potassium calcium'
_=_ 'scandium titanium vanadium chromium manganese iron cobalt nickel copper zinc gallium'
_=_ 'germanium arsenic selenium bromine krypton rubidium strontium yttrium zirconium'
_=_ 'niobium molybdenum technetium ruthenium rhodium palladium silver cadmium indium tin'
_=_ 'antimony tellurium iodine xenon cesium barium lanthanum cerium praseodymium'
_=_ 'neodymium promethium samarium europium gadolinium terbium dysprosium holmium erbium'
_=_ 'thulium ytterbium lutetium hafnium tantalum tungsten rhenium osmium iridium platinum'
_=_ 'gold mercury thallium lead bismuth polonium astatine radon francium radium actinium'
_=_ 'thorium protactinium uranium neptunium plutonium americium curium berkelium'
_=_ 'californium einsteinium fermium mendelevium nobelium lawrencium rutherfordium dubnium'
_=_ 'seaborgium bohrium hassium meitnerium darmstadtium roentgenium copernicium nihonium'
_=_ 'flerovium moscovium livermorium tennessine oganesson ununenniym unbinvlium umbiunium'

#= words(_)                                      /*obtain the number of words in list.  */
item= subword(_, random(1, #), 1)                /*obtain random word (element) in list.*/
say 'random element: '    item                   /*stick a fork in it,  we're all done. */
```

{{out|output}}

```txt

random element:  nihoniym

```



### version 2

Slightly simplified:


Note that this version doesn't work (receives a syntax error 12) with REXXes that have a

smaller limit of the total length of a clause, in particular PC/REXX and Personal REXX

which have a limit of 1,000 characters).

```rexx

/* REXX ***************************************************************
* 18.10.2012 Walter Pachl Not only the list of elements shortened:-)
**********************************************************************/
wl='hydrogen helium lithium beryllium boron carbon nitrogen oxygen',
   'fluorine neon sodium magnesium aluminum silicon phosphorous sulfur',
   '...',
   'meitnerium darmstadtium roentgenium copernicium Ununtrium'

Say word(wl,random(1,words(wl)))

```



## Red


```Red>>
 random/only collect [repeat i 10 [keep i]]
```



## Ring


```ring

aList  = "abcdefghij"
for i = 1 to 10
    letter = random(9) + 1
    if letter > 0
       see aList[letter] + nl
    ok
next

```



## Ruby


```ruby

%w(north east south west).sample   # => "west"
(1..100).to_a.sample(2)            # => [17, 79]
```



## Run BASIC


```runbasic
list$  = "a,b,c,d,e,f,g,h,i,j"
letter = rnd(1) * 10
print "Selected letter:"; word$(list$,letter,",")
```



## Rust

{{libheader|rand}}

```rust
extern crate rand;

use rand::Rng;

fn main() {
    let array = [5,1,2,5,6,7,8,1,2,4,5];
    let mut rng = rand::thread_rng();

    println!("{}", rng.choose(&array).unwrap());
}
```



## Scala

{{libheader|Scala}}

```Scala
val a = (1 to 10).toList

println(scala.util.Random.shuffle(a).head)
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  begin
    writeln(rand([] ("foo", "bar", "baz")));
  end func;
```



## Sidef


```ruby
var arr = %w(north east south west);
say arr.rand;
say arr.rand(2).dump;
```

{{out}}

```txt
south
['west', 'south']
```



## Smalltalk


```smalltalk
x := #(1 2 3) atRandom.
```


## SuperCollider


```SuperCollider
[1, 2, 3].choose
```


## Swift


```Swift
import Darwin

let myList = [1, 2, 4, 5, 62, 234, 1, -1]
print(myList[Int(arc4random_uniform(UInt32(myList.count)))])
```



## Tcl

Random selection from a list is implemented by composing <code>lindex</code>
(for selection of an item from a list) and the pattern for generating an integral random number from the range <math>[0,n)</math>.
It's simpler to use when wrapped up as a helper procedure:

```tcl
proc randelem {list} {
    lindex $list [expr {int(rand()*[llength $list])}]
}
set x [randelem {1 2 3 4 5}]
```



## TXR

{{trans|Tcl}}


```txr
@(do (defun randelem (seq)
       [seq (random nil (length seq))]))
@(bind x @(randelem #("a" "b" "c" "d")))
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
list="John'Paul'George'Ringo'Peter'Paul'Mary'Obama'Putin"
sizeList=SIZE(list)
selectedNr=RANDOM_NUMBERS (1,sizeList,1)
selectedItem=SELECT(list,#selectednr)
PRINT "Selecting term ",selectedNr,"  in the list, which was ",selectedItem
```

{{out}}

```txt

Selecting term 3  in the list, which was George

```



## Ursa


```ursa
# generate a stream (ursa equivalent of a list)
decl string<> str
append "these" "are" "some" "values" str

decl ursa.util.random r
out str<(r.getint (size str))> endl console
```



## VBA



```vb

Option Explicit

Sub Main_Pick_Random_Element()
    Debug.Print Pick_Random_Element(Array(1, 2, 3, 4, 5, #11/24/2017#, "azerty"))
End Sub

Function Pick_Random_Element(myArray)
    Randomize Timer
    Pick_Random_Element = myArray(Int((Rnd * (UBound(myArray) - LBound(myArray) + 1) + LBound(myArray))))
End Function

```



## VBScript


```vb
Function pick_random(arr)
	Set objRandom = CreateObject("System.Random")
	pick_random = arr(objRandom.Next_2(0,UBound(arr)+1))
End Function

WScript.Echo pick_random(Array("a","b","c","d","e","f"))
```


{{Out}}

```txt
d
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Program
    Sub Main()
        Dim list As New List(Of Integer)({0, 1, 2, 3, 4, 5, 6, 7, 8, 9})
        Dim rng As New Random()
        Dim randomElement = list(rng.Next(list.Count)) ' Upper bound is exclusive.
        Console.WriteLine("I picked element {0}", randomElement)
    End Sub
End Module
```



## XPL0


```XPL0
code Ran=1, Text=12;
int  List;
[List:= ["hydrogen", "helium", "lithium", "beryllium", "boron"];  \(Thanks REXX)
Text(0, List(Ran(5)));
]
```



## zkl

{{trans|XPL0}}

```zkl
list:=T("hydrogen", "helium", "lithium", "beryllium", "boron");
list[(0).random(list.len())]
```

