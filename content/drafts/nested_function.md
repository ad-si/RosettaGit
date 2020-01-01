+++
title = "Nested function"
description = ""
date = 2019-09-09T20:17:33Z
aliases = []
[extra]
id = 21107
[taxonomies]
categories = []
tags = []
+++

{{task}}

In many languages, functions can be nested, resulting in outer functions and inner functions. The inner function can access variables from the outer function. In most languages, the inner function can also modify variables in the outer function.

'''The Task'''

Write a program consisting of two nested functions that prints the following text.

 1. first
 2. second
 3. third

The outer function (called <tt>MakeList</tt> or equivalent) is responsible for creating the list as a whole and is given the separator <tt>". "</tt> as argument. It also defines a counter variable to keep track of the item number. This demonstrates how the inner function can influence the variables in the outer function.

The inner function (called <tt>MakeItem</tt> or equivalent) is responsible for creating a list item. It accesses the separator from the outer function and modifies the counter.

'''References:'''

:* [[wp:Nested function|Nested function]]

[[Category:Scope]][[Category:Functions and subroutines]]


## Ada


```Ada
with Ada.Text_IO;

procedure Nested_Functions is -- 'Nested_Functions' is the name of 'main'

   type List is array(Natural range <>) of String(1 .. 10);

   function Make_List(Separator: String) return List is
      Counter: Natural := 0;

      function Make_Item(Item_Name: String) return String is
      begin
	 Counter := Counter + 1; -- local in Make_List, global in Make_Item
	 return(Natural'Image(Counter) & Separator & Item_Name);
      end Make_Item;

   begin
      return (Make_Item("First "), Make_Item("Second"), Make_Item("Third "));
   end Make_List;

begin -- iterate through the result of Make_List
   for Item of Make_List(". ") loop
      Ada.Text_IO.Put_Line(Item);
   end loop;
end Nested_Functions;
```

{{out}}

```txt
$ ./nested_functions
 1. First
 2. Second
 3. Third

```



## ALGOL 68


```algol68
PROC make list = ( STRING separator )STRING:
     BEGIN
        INT counter := 0;
        PROC make item = ( STRING item )STRING:
             BEGIN
                counter +:= 1;
                whole( counter, 0 ) + separator + item + REPR 10
             END; # make item #
        make item( "first" ) + make item( "second" ) + make item( "third" )
     END; # make list #

print( ( make list( ". " ) ) )

```



## ALGOL W

Algol W strings are fixed length which makes this slightly more complicated than the Algol 68 solution.

```algolw
begin
    string(30) procedure makeList ( string(2) value separator ) ;
        begin
            string(30) listValue;
            integer counter;
            string(10) procedure makeItem ( string(6) value item
                                          ; integer   value length
                                          ) ;
                begin
                    string(10) listItem;
                    counter := counter + 1;
                    listItem( 0 // 1 ) := code( decode( "0" ) + counter );
                    listItem( 1 // 2 ) := separator;
                    listItem( 3 // 6 ) := item;
                    listItem( 3 + length // 1 ) := code( 10 );
                    listItem
                end; % makeItem %
                counter   := 0;
                listValue := makeItem( "first", 5 );
                listValue(  9 // 10 ) := makeItem( "second", 6 );
                listValue( 19 // 10 ) := makeItem( "third",  5 );
                listValue
         end; % makeList %
    write( makeList( ". " ) )
end.
```



## AppleScript


```AppleScript
-- NESTED FUNCTION -----------------------------------------------------------

-- makeList :: String -> String
on makeList(separator)
    set counter to 0

    -- makeItem :: String -> String
    script makeItem
        on |λ|(x)
            set counter to counter + 1

            (counter & separator & x & linefeed) as string
        end |λ|
    end script

    map(makeItem, ["first", "second", "third"]) as string
end makeList

-- TEST ----------------------------------------------------------------------
on run

    makeList(". ")

end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

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
```

{{Out}}

```txt
1. first
2. second
3. third

```


Note, however, that mutation creates redundant complexity and loss of referential transparency. Functions which modify values outside their own scope are rarely, if ever, necessary, and always best avoided. Simpler and sounder here to derive the incrementing index either by zipping the input list with a range of integers, or by inheriting it from the higher order map function:


```AppleScript
-- makeList :: String -> String
on makeList(separator)

    -- makeItem :: String -> Int -> String
    script makeItem
        on |λ|(x, i)
            (i & separator & x & linefeed) as string
        end |λ|
    end script

    map(makeItem, ["first", "second", "third"]) as string
end makeList
```



## ATS


```ATS

(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

fun
MakeList
(
  sep: string
) : void = let
//
var count: int = 0
//
val count =
  $UNSAFE.cast{ref(int)}(addr@count)
//
fun
MakeItem
(
  item: string
) : void = let
  val () = !count := !count+1
in
  println! (!count, sep, item)
end // end of [MakeItem]
//
in
  MakeItem"first"; MakeItem"second"; MakeItem"third"
end // end of [MakeList]

(* ****** ****** *)

implement main0() = { val () = MakeList". " }

(* ****** ****** *)

```




## C

I honestly never thought this task could ever be done in C and then I was surprised, again. It turns out that nested functions although not a C standard are supported by [https://gcc.gnu.org/onlinedocs/gcc/Nested-Functions.html GCC]. I have used anonymous functions in Java and frankly, I don't see any practical benefit other than making code even harder to read. Then again, that's one of the [http://www.ioccc.org/ strengths of C]. For example, I still have no clue how come the sprintf line is working correctly. I expected the first line of the list to be '1. second', but no, [http://www.netfunny.com/rhf/jokes/90q2/ode.html C is C is C].

----

'''IMPORTANT''' This implementation will only work with GCC. Go through the link above for details.

```C

#include<stdlib.h>
#include<stdio.h>

typedef struct{
	char str[30];
}item;

item* makeList(char* separator){
	int counter = 0,i;
	item* list = (item*)malloc(3*sizeof(item));

	item makeItem(){
		item holder;

		char names[5][10] = {"first","second","third","fourth","fifth"};

		sprintf(holder.str,"%d%s%s",++counter,separator,names[counter]);

		return holder;
	}

	for(i=0;i<3;i++)
		list[i] = makeItem();

	return list;
}

int main()
{
	int i;
	item* list = makeList(". ");

	for(i=0;i<3;i++)
		printf("\n%s",list[i].str);

	return 0;
}

```

Output:

```txt

1. first
2. second
3. third

```



## C++

{{works with|C++11}}

```cpp
#include <iostream>
#include <string>
#include <vector>

std::vector<std::string> makeList(std::string separator) {
  auto counter = 0;
  auto makeItem = [&](std::string item) {
    return std::to_string(++counter) + separator + item;
  };
  return {makeItem("first"), makeItem("second"), makeItem("third")};
}

int main() {
  for (auto item : makeList(". "))
    std::cout << item << "\n";
}
```



## C#


```c#
string MakeList(string separator)
{
    int counter = 1;

    Func<string, string> makeItem = item => counter++ + separator + item + "\n";

    return makeItem("first") + makeItem("second") + makeItem("third");
}

Console.WriteLine(MakeList(". "));
```

'''Update'''<br/>
As of C#7, we can nest actual methods inside other methods instead of creating delegate instances. They can even be declared after the return statement.

```c#
string MakeList2(string separator)
{
    int counter = 1;

    return MakeItem("first") + MakeItem("second") + MakeItem("third");
    //using string interpolation
    string MakeItem(string item) => $"{counter++}{separator}{item}\n";
}
```



## Clojure



```clojure
(defn make-list [separator]
  (let [x (atom 0)]
    (letfn [(make-item [item] (swap! x inc) (println (format "%s%s%s" @x separator item)))]
      (make-item "first")
      (make-item "second")
      (make-item "third"))))

(make-list ". ")
```


{{out}}

```txt

1. first
2. second
3. third

```



## Common Lisp



```lisp
(defun my-make-list (separator)
  (let ((counter 0))
    (flet ((make-item (item)
              (format nil "~a~a~a~%" (incf counter) separator item)))
      (concatenate 'string
                   (make-item "first")
                   (make-item "second")
                   (make-item "third")))))

(format t (my-make-list ". "))
```


''PS: A function named make-list is already defined in Common Lisp, see [http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_lis.htm#make-list specification].''


## D



```d
string makeList(string seperator) {
    int counter = 1;

    string makeItem(string item) {
        import std.conv : to;
        return to!string(counter++) ~ seperator ~ item ~ "\n";
    }

    return makeItem("first") ~ makeItem("second") ~ makeItem("third");
}

void main() {
    import std.stdio : writeln;
    writeln(makeList(". "));
}
```



## Delphi

''See [[#Pascal|Pascal]]''


## Elena

ELENA 4.x :

```elena
import extensions;

MakeList(separator)
{
    var counter := 1;

    var makeItem := (item){ var retVal := counter.Printable + separator + item + (forward newLine); counter += 1; ^ retVal };

    ^ makeItem("first") + makeItem("second") + makeItem("third")
}

public program()
{
    console.printLine(MakeList(". "))
}
```

{{out}}

```txt

1. first
2. second
3. third

```



## Elixir

Elixir data are immutable. Anonymous functions are closures and as such they can access variables that are in scope when the function is defined. Keep in mind a variable assigned inside a function does not affect its surrounding environment:

```elixir
defmodule Nested do
  def makeList(separator) do
    counter = 1

    makeItem = fn {}, item ->
                      {"#{counter}#{separator}#{item}\n", counter+1}
                  {result, counter}, item ->
                      {result <> "#{counter}#{separator}#{item}\n", counter+1}
               end

    {} |> makeItem.("first") |> makeItem.("second") |> makeItem.("third") |> elem(0)
  end
end

IO.write Nested.makeList(". ")
```


{{out}}

```txt

1. first
2. second
3. third

```



## Factor

Words (named functions) cannot be defined with parsing words (such as <code>:</code> or <code>::</code>) in the definition of another word. However, quotations (anonymous functions) can be. We can easily mimic the required behavior by binding a quotation to a lexical variable named <code>make-item</code>. The only caveat is that we must explicitly <code>call</code> the quotation in order to execute it.

If we really wanted, we could also define a named word inside <code>make-list</code> at run time, using words such as <code>define</code> in the <code>words</code> vocabulary.


```factor
USING: io kernel math math.parser locals qw sequences ;
IN: rosetta-code.nested-functions

:: make-list ( separator -- str )
    1 :> counter!
    [| item |
        counter number>string separator append item append
        counter 1 + counter!
    ] :> make-item
    qw{ first second third } [ make-item call ] map "\n" join
;

". " make-list write
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Nested_function this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran


### Arithmetic statement functions

Fortran allows the user to define functions (and subroutines also) but from first Fortran (1958) on these are compiled as separate items and cannot themselves contain the definition of another function (or subroutine) - except for the special form allowing the definition of what is called an arithmetic statement function, such as follows:
```Fortran
      FUNCTION F(X)
       REAL X
       DIST(U,V,W) = X*SQRT(U**2 + V**2 + W**2)    !The contained function.
        T = EXP(X)
        F = T + DIST(T,SIN(X),ATAN(X) + 7)         !Invoked...
      END
```

This (deranged) function contains within it the definition of function DIST (which must be achieved in a single arithmetic statement), and which has access to all the variables of its containing function as well as its own parameters. The sequence <code>DIST(U,V,W) = ''etc.''</code> would normally be interpreted as an assignment of a value to an element of an array called DIST, but, no such array has been declared so this must therefore be the definition of an arithmetic statement function. Such functions are defined following any declarations of variables, and precede the normal executable statements such as <code>T = EXP(X)</code>. Since they are for arithmetic they cannot be used for character manipulations, and the CHARACTER variable only appeared with F77.


### Containerisation

With the advent of F90 comes the CONTAINS statement, whereby within a function (or subroutine) but oddly, at its ''end'' (but before its END) appears the key word CONTAINS, after which further functions (and subroutines) may be defined in the established manner. These have access to all the variables defined in the containing routine, though if the contained routine declares a name used in the containing routine then that outside name becomes inaccessible.

Such contained routines are not themselves allowed to contain routines, so that the nesting is limited to two levels - except that arithmetic statement functions are available, so that three levels could be employed. Languages such as Algol, pl/i, Pascal, etc. impose no such constraint.
```Fortran
      SUBROUTINE POOBAH(TEXT,L,SEP)	!I've got a little list!
       CHARACTER*(*) TEXT	!The supplied scratchpad.
       INTEGER L		!Its length.
       CHARACTER*(*) SEP	!The separator to be used.
       INTEGER N		!A counter.
        L = 0			!No text is in place.
        N = 0			!No items added.
        CALL ADDITEM("first")	!Here we go.
        CALL ADDITEM("second")
        CALL ADDITEM("third")
       CONTAINS		!Madly, defined after usage.
        SUBROUTINE ADDITEM(X)	!A contained routine.
         CHARACTER*(*) X	!The text of the item.
          N = N + 1			!Count another item in.
          TEXT(L + 1:L + 1) = CHAR(ICHAR("0") + N)	!Place the single-digit number.
          L = L + 1			!Rather than mess with unknown-length numbers.
          LX = LEN(SEP)			!Now for the separator.
          TEXT(L + 1:L + LX) = SEP	!Placed.
          L = L + LX			!Advance the finger.
          LX = LEN(X)			!Trailing spaces will be included.
          TEXT(L + 1:L + LX) = X	!Placed.
          L = L + LX			!Advance the finger.
          L = L + 1			!Finally,
          TEXT(L:L) = CHAR(10)		!Append an ASCII line feed. Starts a new line.
        END SUBROUTINE ADDITEM	!That was bitty.
      END SUBROUTINE POOBAH	!But only had to be written once.

      PROGRAM POKE
      CHARACTER*666 TEXT	!Surely sufficient.
      INTEGER L
      CALL POOBAH(TEXT,L,". ")
      WRITE (6,"(A)") TEXT(1:L)
      END
```


Fortran doesn't offer a "list" construction as a built-in facility so it seemed easiest to prepare the list in a CHARACTER variable. These do not have a length attribute as in a string, the LEN function reports the size of the character variable not something such as the current length of a string varying from zero to the storage limit. So, the length of the in-use portion is tracked with the aid of an auxiliary variable, and one must decide on a sufficiently large scratchpad area to hold the anticipated result. And, since the items are of varying length, the length of the whole sequence is returned, not the number of items. Subroutine POOBAH could be instead a function, but, it would have to return a fixed-size result (as in say <code>CHARACTER*66 FUNCTION POOBAH(SEP)</code>) and can't return a length as well, unless via messing with a global variable such as in COMMON or via an additional parameter as with the L above.

To achieve the required output of one item per line would mean the output of one item at a time, and all the items are packed into TEXT with unknown boundaries. A single character sequence seemed less trouble, but to achieve the one-item-per-line layout meant inserting control codes to start a new line. Oddly, the CHAR(10) is the linefeed character in ASCII but on this windows system it is treated as CRLF whereas CR returned to the start of the line with no advance. If output were to go to an old-style lineprinter, such in-line control codes would not be recognised.

Placing all the texts into one "pool" storage area saves space when items are a different length, but items can only be accessed sequentially. If item <code>i</code> were desired, it can only be found after stepping along from the start and if the collection expands beyond a few dozen items, repeated random access soon becomes slow. If this is important, rather than have the items separated by a special in-line symbol one can instead have an array of fingers to say the end of each item's text, which can thereby contain any symbol. In this case the pooled storage for the texts wastes no space on special symbols but this index array must have some predefined size (and be capable of indexing the size of the pool: 8-bits? 16-bits? 32-bits?), so once again, how long is a piece of string?


### When storage is abundant

Another way of providing a "list" is via an array as in <code>CHARACTER*28 TEXT(9)</code>) so that each item occupied one element, and the maddening question "how long is a piece of string" arises twice: how much storage to allow for each element when all must be as long as the longest text expected, and, how many elements are to be allowed for.
```Fortran
      SUBROUTINE POOBAH(TEXT,N,SEP)	!I've got a little list!
       CHARACTER*(*) TEXT(*)	!The supplied scratchpad.
       INTEGER N		!Entry count.
       CHARACTER*(*) SEP	!The separator to be used.
        N = 0			!No items added.
        CALL ADDITEM("first")	!Here we go.
        CALL ADDITEM("second")
        CALL ADDITEM("third")
       CONTAINS		!Madly, defined after usage.
        SUBROUTINE ADDITEM(X)	!A contained routine.
         CHARACTER*(*) X	!The text of the item to add.
          N = N + 1			!Count another item in.
          WRITE (TEXT(N),1) N,SEP,X	!Place the N'th text, suitably decorated..
    1     FORMAT (I1,2A)		!Allowing only a single digit.
        END SUBROUTINE ADDITEM	!That was simple.
      END SUBROUTINE POOBAH	!Still worth a subroutine.

      PROGRAM POKE
      CHARACTER*28 TEXT(9)	!Surely sufficient.
      INTEGER N
      CALL POOBAH(TEXT,N,". ")
      WRITE (6,"(A)") (TEXT(I)(1:LEN_TRIM(TEXT(I))), I = 1,N)
      END
```

The output statement could be <code>WRITE (6,"(A)") TEXT(1:N)</code> but this would write out the trailing spaces in each element. A TRIM intrinsic function may be available, but, leading spaces may be desired in the case that there are to be more than nine elements. If so, <code>FORMAT (I2,2A)</code> would be needed up to ninety-nine, or more generally, I0 format. Except that would not write out leading spaces and would spoil the neatness of a columnar layout. With file names, the lack of leading spaces (or zero digits) leads to the ideas explored in [[Natural_sorting|"Natural" sorting]]. One could define constants via the PARAMETER statement to document the linkage between the number of array elements and the correct FORMAT code, though this is messy because for NMAX elements the format code requires <Log10(NMAX) + 1> digits, and in such an attempt I've seen Log10(10) come out not as one but as 0·9999932 or somesuch, truncating to zero.

F95 introduced facilities whereby a string-style compound variable with both content and current length could be defined and manipulated, and when assigned to it would be reallocated storage so as to have exactly the size to hold the result. Later fortran standardised such a scheme. Similarly, one could define a data aggregate containing a count <code>N</code> as well as the <code>TEXT</code> array and a function could return such a compound entity as its result. It may also be possible to arrange that array TEXT becomes "ragged", that is, TEXT(i) is not always 28 characters long, but only as much as is needed to store the actual item.


## FreeBASIC


FreeBASIC does not currently support either nested procedures or lambda expressions.
The best we can do here is to create two separate procedures but pass the state of the first procedure
by reference to the second procedure so it can be modified by the latter.


```freebasic
' FB 1.05.0 Win64

Sub makeItem(sep As String, ByRef counter As Integer, text As String)
  counter += 1
  Print counter; sep; text
End Sub

Sub makeList(sep As String)
  Dim a(0 To 2) As String = {"first", "second", "third"}
  Dim counter As Integer = 0
  While counter < 3
    makeItem(sep, counter, a(counter))
  Wend
End Sub

makeList ". "
Print
Print "Press any key to quit"
Sleep

```


{{out}}

```txt

 1. first
 2. second
 3. third

```



## Free Pascal


```pascal
// In Pascal, functions always _have_ to return _some_ value,
// but the the task doesn’t specify what to return.
// Hence makeList and makeItem became procedures.
procedure makeList(const separator: string);
// The var-section for variables that ought to be accessible
// in the routine’s body as well as the /nested/ routines
// has to appear /before/ the nested routines’ definitions.
var
	counter: 1..high(integer);

	procedure makeItem;
	begin
		write(counter, separator);
		case counter of
			1:
			begin
				write('first');
			end;
			2:
			begin
				write('second');
			end;
			3:
			begin
				write('third');
			end;
		end;
		writeLn();
		counter := counter + 1;
	end;
// You can insert another var-section here, but variables declared
// in this block would _not_ be accessible in the /nested/ routine.
begin
	counter := 1;
	makeItem;
	makeItem;
	makeItem;
end;
```



## Go



```go
package main
import "fmt"

func makeList(separator string) string {
    counter := 1

    makeItem := func(item string) string {
        result := fmt.Sprintf("%d%s%s\n", counter, separator, item)
        counter += 1
        return result
    }

    return makeItem("first") + makeItem("second") + makeItem("third")
}

func main() {
    fmt.Print(makeList(". "))
}
```



## Haskell



```haskell
import Control.Monad.ST
import Data.STRef

makeList :: String -> String
makeList separator = concat $ runST $ do
  counter <- newSTRef 1
  let makeItem item = do
        x <- readSTRef counter
        let result = show x ++ separator ++ item ++ "\n"
        modifySTRef counter (+ 1)
        return result
  mapM makeItem ["first", "second", "third"]


main :: IO ()
main = putStr $ makeList ". "
```



## Io


```Io
makeList := method(separator,
    counter := 1
    makeItem := method(item,
        result := counter .. separator .. item .. "\n"
        counter = counter + 1
        result
    )
    makeItem("first") .. makeItem("second") .. makeItem("third")
)
makeList(". ") print
```



## J


J does not have nested scopes, so they must be [[Scope/Function_names_and_labels#J|emulated]]. (The design philosophy here is that nesting tends to become difficult to understand when taken too far, so the coder and designer should be mildly penalized with extra work for choosing nesting as opposed to some other problem solving approach.)

That said, emulating a single level of nesting is relatively trivial and does not reflect the complexities necessary for more elaborate (and more difficult to understand) cases:


```J
MakeList=: dyad define
  sep_MakeList_=: x
  cnt_MakeList_=: 0
  ;MakeItem each y
)

MakeItem=: verb define
  cnt_MakeList_=: cnt_MakeList_+1
  (":cnt_MakeList_),sep_MakeList_,y,LF
)
```


Example use:


```J
   '. ' MakeList 'first';'second';'third'
1. first
2. second
3. third

```



## Java

{{works with|Java|8}}

Since version 8, Java has limited support for nested functions. All variables from the outer function that are accessed by the inner function have to be _effectively final_. This means that the counter cannot be a simple <tt>int</tt> variable; the closest way to emulate it is the <tt>AtomicInteger</tt> class.


```java
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

public class NestedFunctionsDemo {

    static String makeList(String separator) {
        AtomicInteger counter = new AtomicInteger(1);

        Function<String, String> makeItem = item -> counter.getAndIncrement() + separator + item + "\n";

        return makeItem.apply("first") + makeItem.apply("second") + makeItem.apply("third");
    }

    public static void main(String[] args) {
        System.out.println(makeList(". "));
    }
}
```



## JavaScript



```javascript
function makeList(separator) {
  var counter = 1;

  function makeItem(item) {
    return counter++ + separator + item + "\n";
  }

  return makeItem("first") + makeItem("second") + makeItem("third");
}

console.log(makeList(". "));
```



## jq



```jq
def makeList(separator):
  # input: {text: _, counter: _}
  def makeItem(item):
     (.counter + 1) as $counter
     | .text += "\($counter)\(separator)\(item)\n"
     | .counter = $counter;

   {text:"", counter:0} | makeItem("first") | makeItem("second") | makeItem("third")
   | .text
;

makeList(". ")
```


With the above in a file, say program.jq, the invocation:

    $ jq -n -r -f program.jq

produces:
```txt

1. first
2. second
3. third
```



## Jsish

From Javascript entry.

```javascript
/* Nested function, in Jsish */
function makeList(separator) {
  var counter = 1;

  function makeItem(item) {
    return counter++ + separator + item + "\n";
  }

  return makeItem("first") + makeItem("second") + makeItem("third");
}

;makeList('. ');

/*
=!EXPECTSTART!=
makeList('. ') ==> 1. first
2. second
3. third

=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u nestedFunction.jsi
[PASS] nestedFunction.jsi
```



## Julia

{{works with|Julia|0.6}}


```julia
function makelist(sep::String)
    cnt = 1

    function makeitem(item::String)
        rst = string(cnt, sep, item, '\n')
        cnt += 1
        return rst
    end

    return makeitem("first") * makeitem("second") * makeitem("third")
end

print(makelist(". "))
```



## Kotlin


```scala
// version 1.0.6

fun makeList(sep: String): String {
    var count = 0
    fun makeItem(item: String): String {
        count++
        return "$count$sep$item\n"
    }
    return makeItem("first") + makeItem("second") + makeItem("third")
}

fun main(args: Array<String>) {
    print(makeList(". "))
}
```


{{out}}

```txt

1. first
2. second
3. third

```



## Lua



```lua
function makeList (separator)
    local counter = 0
    local function makeItem(item)
            counter = counter + 1
            return counter .. separator .. item .. "\n"
        end
    return makeItem("first") .. makeItem("second") .. makeItem("third")
end

print(makeList(". "))
```

{{out}}

```txt
1. first
2. second
3. third
```



## M2000 Interpreter

In M2000 functions may have functions, modules, subs, but these are black boxes. We can define globals for temporary use. Subs can use anything from module/function where we call them. First example use Subs inside a module, when call Make_list two local variables, Separator$ and Counter allocated in same space as module's. So when we call Make_item() these variables are visible. At the exit of sub Make_list local variables destroyed. In second example Letter$ pop a string from stack of values (or an error raised if no string found).


```M2000 Interpreter

Module Checkit {
      Make_List(". ")
      Sub Make_List(Separator$)
            Local Counter=0
            Make_Item("First")
            Make_Item("Second")
            Make_Item("Third")
      End Sub
      Sub Make_Item(Item_Name$)
            Counter++
            Print Str$(Counter,"")+Separator$+Item_Name$
      End Sub
}
Checkit

Module Make_List  {
      Global Counter=0, Separator$=Letter$
      Make_Item("First")
      Make_Item("Second")
      Make_Item("Third")

      Sub Make_Item(Item_Name$)
            Counter++
            Print Str$(Counter,"")+Separator$+Item_Name$
      End Sub
}

Make_List ". "

Module Make_List1  {
      Global Counter=0, Separator$=Letter$
      Module Make_Item (Item_Name$) {
            Counter++
            Print Str$(Counter,"")+Separator$+Item_Name$
      }
      Make_Item "First"
      Make_Item "Second"
      Make_Item "Third"
}

Make_List1 ". "

```



## Maple


```Maple

makelist:=proc()
	local makeitem,i;
	i:=1;
	makeitem:=proc(i)
		if i=1 then
			printf("%a\n", "1. first");
		elif i=2 then
			printf("%a\n","2. second");
		elif i=3 then
			printf("%a\n", "3. third");
		else
			return NULL;
		end if;
	end proc;
	while i<4 do
		makeitem(i);
		i:=i+1;
	end do;
end proc;


```



## Mathematica


```Mathematica
makeList[sep_String]:=Block[
  {counter=0, makeItem},
  makeItem[item_String]:=ToString[++counter]<>sep<>item;
  makeItem /@ {"first", "second", "third"}
  ]
Scan[Print, makeList[". "]]
```



## min

{{works with|min|0.19.3}}
Note the <code>@</code> sigil is the key to altering <code>counter</code> in the outer scope.

```min
(
  :separator
  1 :counter
  (
    :item
    item separator counter string ' append append "" join
    counter succ @counter
  ) :make-item
  ("first" "second" "third") 'make-item map "\n" join
) :make-list

". " make-list print
```



## MiniScript

While subfunctions in MiniScript can read variables in the enclosing scope, the can't directly assign new values to immutable types (assignment creates a local variable).  So we use a mutable type (list) here to hold our counter.

```MiniScript
makeList = function(sep)
    counter = [0]
    makeItem = function(item)
        counter[0] = counter[0] + 1
        return counter[0] + sep + item
    end function
    return [makeItem("first"), makeItem("second"), makeItem("third")]
end function

print makeList(". ")
```

Output:

```txt
["1. first", "2. second", "3. third"]
```


=={{header|Objective-C}}==


```objc
NSString *makeList(NSString *separator) {
  __block int counter = 1;

  NSString *(^makeItem)(NSString *) = ^(NSString *item) {
    return [NSString stringWithFormat:@"%d%@%@\n", counter++, separator, item];
  };

  return [NSString stringWithFormat:@"%@%@%@", makeItem(@"first"), makeItem(@"second"), makeItem(@"third")];
}

int main() {
  NSLog(@"%@", makeList(@". "));
  return 0;
}
```



## OCaml



```ocaml
let make_list separator =
  let counter = ref 1 in

  let make_item item =
    let result = string_of_int !counter ^ separator ^ item ^ "\n" in
    incr counter;
    result
  in

  make_item "first" ^ make_item "second" ^ make_item "third"

let () =
  print_string (make_list ". ")
```

Interestingly, on my computer it prints the numbers in reverse order, probably because the order of evaluation of arguments (and thus order of access of the counter) is undetermined:
{{out}}

```txt

3. first
2. second
1. third

```



## Pascal

''See [[#Free Pascal|Free Pascal]]''


## Perl



```perl
sub makeList {
    my $separator = shift;
    my $counter = 1;

    sub makeItem { $counter++ . $separator . shift . "\n" }

    makeItem("first") . makeItem("second") . makeItem("third")
}

print makeList(". ");
```



## Perl 6



```perl6
sub make-List ($separator = ') '){
    my $count = 1;

    sub make-Item ($item) { "{$count++}$separator$item" }

    join "\n", <first second third>».&make-Item;
}

put make-List('. ');
```

{{out}}

```txt
1. first
2. second
3. third
```



## Phix

{{improve|Phix|The Phix compiler needs enhancing to fully support nested functions.}}

Prior to this task, Phix had no support whatsoever for nested functions.

Instead I have taken the first baby steps and documented them.

If your distribution does not include the following demo, you need a later version (not yet uploaded at the
time of writing). The source of that demo contains far more detailed information, and any updates.


Yes, this is pig-ugly, and incomplete. But it is a good start for anyone that needs nested functions, and
shows what can be done in just a few (less than six) hours.

NB as it stands, the compiler front-end "thinks" that l_counter and sep live in the same place. They are
properly separate in the runtime/VM, but the front-end will happily emit nonsense code if you let it.

```Phix
--
-- demo\rosetta\Nested_function.exw
--
### ==========================

--
#ilASM{ jmp :fin
    --
    -- This is, of course, something the compiler should end up doing automatically,
    -- and this assembly, or something similar, should be hidden away in builtins/VM.
    --
  :%opGetnlv        -- [edi] := [esi] from frame edx
                    -- nb no reference counting (would be rqd)
    [32]
        sub esi,ebp         -- --> frame offset
      @@:
        mov ecx,[ebp+20]    -- ebp_prev
        cmp [ecx+8],edx     -- rtn
        jne @b
        mov eax,[esi+ecx]
        mov [edi],eax
    [64]
        sub rsi,rbp         -- --> frame offset
      @@:
        mov rcx,[rbp+40]    -- rbp_prev
        cmp [rcx+16],rdx    -- rtn
        jne @b
        mov rax,[rsi+rcx]
        mov [rdi],rax
    []
        ret

  :%opSetnlv        -- [edi] in frame edx := [esi] (zeroed)
    [32]
        sub edi,ebp         -- --> frame offset
      @@:
        mov ecx,[ebp+20]    -- ebp_prev
        cmp [ecx+8],edx     -- rtn
        jne @b
        mov eax,[esi]
        mov [edi+ecx],eax
--      mov [esi],ebx       -- zero src
    [64]
        sub rdi,rbp         -- --> frame offset
      @@:
        mov rcx,[rbp+40]    -- rbp_prev
        cmp [rcx+16],rdx    -- rtn
        jne @b
        mov eax,[rsi]
        mov [rdi+rcx],rax
--      mov [rsi],rbx       -- zero src
    []
        ret
    ::fin
      }

function MakeList(string sep=".  ")
integer counter = 0
    function MakeItem()
--      -- what we'd really like to see:
--      counter += 1
--      return sprintf("%d%s%s",{counter,sep,{"first","second","third"}[counter]})
        -- bar these locals, some idea of what the compiler should be doing:
        integer l_counter
        string l_sep
        #ilASM{
            [32]
                mov edx,routine_id(MakeList)
                lea esi,[counter]
                lea edi,[l_counter]
                call :%opGetnlv
                lea esi,[sep]
                lea edi,[l_sep]
                call :%opGetnlv
            [64]
                mov rdx,routine_id(MakeList)
                lea rsi,[counter]
                lea rdi,[l_counter]
                call :%opGetnlv
                lea rsi,[sep]
                lea rdi,[l_sep]
                call :%opGetnlv
            []
              }
        l_counter += 1
        #ilASM{
            [32]
                mov edx,routine_id(MakeList)
                lea esi,[l_counter]
                lea edi,[counter]
                call :%opSetnlv
            [64]
                mov rdx,routine_id(MakeList)
                lea rsi,[l_counter]
                lea rdi,[counter]
                call :%opSetnlv
            []
              }
        string res = sprintf("%d%s%s",{l_counter,l_sep,{"first","second","third"}[l_counter]})
        #ilASM{
            [32]
                mov [l_sep],ebx     -- (in lieu of proper refcounting)
            [64]
                mov [l_sep],rbx     -- (in lieu of proper refcounting)
            []
              }
        return res
    end function
    sequence res = {}
    for i=1 to 3 do
        res = append(res,MakeItem())
    end for
    return res
end function

?MakeList()
```

{{out}}

```txt

{"1.  first","2.  second","3.  third"}

```



## PHP

{{works with|PHP|5.3+}}

```php
<?
function makeList($separator) {
  $counter = 1;

  $makeItem = function ($item) use ($separator, &$counter) {
    return $counter++ . $separator . $item . "\n";
  };

  return $makeItem("first") . $makeItem("second") . $makeItem("third");
}

echo makeList(". ");
?>
```



## PicoLisp


```PicoLisp
(de makeList (Sep)
   (let (Cnt 0  makeItem '((Str) (prinl (inc 'Cnt) Sep Str)))
      (makeItem "first")
      (makeItem "second")
      (makeItem "third") ) )

(makeList ". ")
```



## Python

{{works with|Python|3+}}

```python
def makeList(separator):
    counter = 1

    def makeItem(item):
        nonlocal counter
        result = str(counter) + separator + item + "\n"
        counter += 1
        return result

    return makeItem("first") + makeItem("second") + makeItem("third")

print(makeList(". "))
```



## Racket

See also [[#Scheme]]; this demonstrates <code>map</code> a higher order function and <code>begin0</code> a form which saves us having to explicitly remember the result.


```racket
#lang racket

(define (make-list separator)
  (define counter 1)

  (define (make-item item)
    (begin0
      (format "~a~a~a~%" counter separator item)
      (set! counter (add1 counter))))

  (apply string-append (map make-item '(first second third))))

(display (make-list ". "))
```


{{out}}

```txt
1. first
2. second
3. third
```



## REXX

This REXX version is modeled after the '''FreeBASIC''' example   (and it has the
same limitations).

```rexx
/*REXX program shows that functions can be nested  (an outer and inner function).       */
ctr=0                                            /*initialize the   CTR   REXX variable.*/
call makeList '. '                               /*invoke  MakeList  with the separator.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
makeItem: parse arg sep,text;   ctr=ctr+1        /*bump the counter variable.           */
          say ctr || sep || word(string, ctr)    /*display three thingys to the terminal*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
makeList: parse arg sep;  string= 'first second third' /*get arguments; define a string.*/
                   do  while  ctr<3                    /*keep truckin'  until  finished.*/
                   call makeItem  sep, string          /*invoke the  makeItem  function.*/
                   end   /*while*/
          return
```

'''output'''

```txt

1. first
2. second
3. third

```



## Ring


```ring

# Project : Nested function

makeList(". ")
func makeitem(sep, counter, text)
       see "" + counter + sep + text + nl

func makelist(sep)
       a = ["first", "second", "third"]
       counter = 0
       while counter < 3
                counter = counter + 1
                makeitem(sep, counter, a[counter])
       end

```

Output:

```txt

1. first
2. second
3. third

```



## Ruby



```ruby
def makeList(separator)
  counter = 1

  makeItem = lambda {|item|
    result = "#{counter}#{separator}#{item}\n"
    counter += 1
    result
  }

  makeItem["first"] + makeItem["second"] + makeItem["third"]
end

print makeList(". ")
```



## Rust


```Rust
fn make_list(sep: &str) -> String {
    let mut counter = 0;
    let mut make_item = |label| {
        counter += 1;
        format!("{}{}{}", counter, sep, label)
    };
    format!(
        "{}\n{}\n{}",
        make_item("First"),
        make_item("Second"),
        make_item("Third")
    )
}

fn main() {
    println!("{}", make_list(". "))
}
```


{{out}}

```txt

1. First
2. Second
3. Third

```



## Scala


```Scala

   def main(args: Array[String]) {
      val sep: String=". "
      var c:Int=1;
      def go(s: String):Unit={
          println(c+sep+s)
          c=c+1
      }
      go("first")
      go("second")
      go("third")
   }

```


{{out}}

```txt

1. first
2. second
3. third

```



## Scheme



```scheme
(define (make-list separator)
  (define counter 1)

  (define (make-item item)
    (let ((result (string-append (number->string counter) separator item "\n")))
      (set! counter (+ counter 1))
      result))

  (string-append (make-item "first") (make-item "second") (make-item "third")))

(display (make-list ". "))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: makeList (in string: separator) is func
  result
    var string: itemList is "";
  local
    var integer: counter is 1;

    const func string: makeItem (in string: item) is func
      result
        var string: anItem is "";
      begin
        anItem := counter <& separator <& item <& "\n";
	incr(counter);
      end func

  begin
    itemList := makeItem("first") & makeItem("second") & makeItem("third");
  end func;

const proc: main is func
  begin
    write(makeList(". "));
  end func;
```


{{out}}

```txt

1. first
2. second
3. third

```



## Sidef


```ruby
func make_list(separator = ') ') {

    var count = 1
    func make_item(item) {
        [count++, separator, item].join
    }

    <first second third> «call« make_item -> join("\n")
}

say make_list('. ')
```

{{out}}

```txt

1. first
2. second
3. third

```



## Standard ML



```sml
fun make_list separator =
  let
    val counter = ref 1;
    fun make_item item =
      let
        val result = Int.toString (!counter) ^ separator ^ item ^ "\n"
      in
        counter := !counter + 1;
        result
      end
  in
    make_item "first" ^ make_item "second" ^ make_item "third"
  end;

print (make_list ". ")
```



## SuperCollider


```SuperCollider
(
f = { |separator|
	var count = 0;
	var counting = { |name|
		count = count + 1;
		count.asString ++ separator + name ++ "\n"
	};
	counting.("first") + counting.("second") + counting.("third")
};
)

f.(".")

```



## Swift



```swift
func makeList(_ separator: String) -> String {
  var counter = 1

  func makeItem(_ item: String) -> String {
    let result = String(counter) + separator + item + "\n"
    counter += 1
    return result
  }

  return makeItem("first") + makeItem("second") + makeItem("third")
}

print(makeList(". "))
```



## Tcl

The code below satisfies the specification (inspired by the Swift example). The inner function MakeItem (which gains read/write access to its caller's variables via upvar) is defined, called, and then discarded by renaming to {}. suchenwi

```Tcl
#!/usr/bin/env tclsh

proc MakeList separator {
    set counter 1
    proc MakeItem string {
	upvar 1 separator separator counter counter
	set res $counter$separator$string\n
	incr counter
	return $res
    }
    set res [MakeItem first][MakeItem second][MakeItem third]
    rename MakeItem {}
    return $res
}
puts [MakeList ". "]

```



## zkl

zkl functions don't have direct access to another functions scope, they are not nested. If a function is defined in another function, the compiler moves it out and hands you a reference to the function. So, you are unable to modify variables in the enclosing scope unless you are given a container which can be modified. Partial application can be used to bind [copies] of scope information to a function, that information is fixed at the point of application and becomes strictly local to the binding function (ie changes do not propagate). A Ref[erence] is a container that holds an object so it can be modified by other entities.

```zkl
fcn makeList(separator){
  counter:=Ref(1);  // a container holding a one. A reference.
  // 'wrap is partial application, in this case binding counter and separator
  makeItem:='wrap(item){ c:=counter.inc(); String(c,separator,item,"\n") };
  makeItem("first") + makeItem("second") + makeItem("third")
}

print(makeList(". "));
```

{{out}}

```txt

1. first
2. second
3. third

```

