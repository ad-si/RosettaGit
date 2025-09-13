+++
title = "Type detection"
description = ""
date = 2019-10-03T17:58:25Z
aliases = []
[extra]
id = 19626
[taxonomies]
categories = ["task"]
tags = []
+++

This draft task needs a purpose, a description and some way to tell whether examples satisfy or do not satisfy it.


## Task

Show a function/procedure that processes a block of text by printing it.

The function takes one parameter (ideally) that describes the text.

Demonstrate by calling the function twice, each time passing in a different type.


This can be done with pattern matching, multi-methods, dynamic type detection, structure(s) with a tag, etc.

The objective is write a [e.g. library] function that processes text from multiple sources (such as a string/char *, socket, file, etc).

If not practical, show how the caller would coerce a type that can be passed to the library function.





## AWK


```AWK

# syntax: TAWK -f TYPE_DETECTION.AWK
# uses Thompson Automation's TAWK 5.0c
BEGIN {
    arr[0] = 0
    print(typeof(arr))
    print(typeof(0.))
    print(typeof(0))
    print(typeof(/0/))
    print(typeof("0"))
    print(typeof(x))
    print(typeof(addressof("x")))
    print(typeof(fopen("x","r")))
    exit(0)
}

```

```txt

array
float
int
regular_expression
string
uninitialized
address
fileid

```


## C

The closest C comes to meeting this task, short of building it into the compiler or accessing memory segments via pointers, which is not guaranteed to be portable, is the ctype.h header file. It is part of the C Standard Library and provides 11 methods for detecting the type of a character, out of which the following 7 called in the wrapper function below can be called to be unique. The function accepts a string, but it actually checks the first character. An if ladder is used instead of if-else so that all function calls which return a non-zero value for the character are satisfied and the information is printed.

```C

#include<stdio.h>
#include<ctype.h>

void typeDetector(char* str){
	if(isalnum(str[0])!=0)
		printf("\n%c is alphanumeric",str[0]);
	if(isalpha(str[0])!=0)
		printf("\n%c is alphabetic",str[0]);
	if(iscntrl(str[0])!=0)
		printf("\n%c is a control character",str[0]);
	if(isdigit(str[0])!=0)
		printf("\n%c is a digit",str[0]);
	if(isprint(str[0])!=0)
		printf("\n%c is printable",str[0]);
	if(ispunct(str[0])!=0)
		printf("\n%c is a punctuation character",str[0]);
	if(isxdigit(str[0])!=0)
		printf("\n%c is a hexadecimal digit",str[0]);
}

int main(int argC, char* argV[])
{
	int i;

	if(argC==1)
		printf("Usage : %s <followed by ASCII characters>");
	else{
		for(i=1;i<argC;i++)
			typeDetector(argV[i]);
	}
	return 0;
}

```

Output, shown for multiple inputs, as well as single ones:

```txt

C:\rosettaCode>typeDetector.exe s 3 $ f ! as 3

s is alphanumeric
s is alphabetic
s is printable
3 is alphanumeric
3 is a digit
3 is printable
3 is a hexadecimal digit
$ is printable
$ is a punctuation character
f is alphanumeric
f is alphabetic
f is printable
f is a hexadecimal digit
! is printable
! is a punctuation character
a is alphanumeric
a is alphabetic
a is printable
a is a hexadecimal digit
3 is alphanumeric
3 is a digit
3 is printable
3 is a hexadecimal digit

C:\rosettaCode>typeDetector.exe a

a is alphanumeric
a is alphabetic
a is printable
a is a hexadecimal digit
C:\rosettaCode>typeDetector.exe $

$ is printable
$ is a punctuation character
C:\rosettaCode>typeDetector.exe 3

3 is alphanumeric
3 is a digit
3 is printable
3 is a hexadecimal digit

```



## C++

```cpp
#include <iostream>

template <typename T>
auto typeString(const T&) {
    return typeid(T).name();
}

class C {};
struct S {};

int main() {
    std::cout << typeString(1) << '\n';
    std::cout << typeString(1L) << '\n';
    std::cout << typeString(1.0f) << '\n';
    std::cout << typeString(1.0) << '\n';
    std::cout << typeString('c') << '\n';
    std::cout << typeString("string") << '\n';
    std::cout << typeString(C{}) << '\n';
    std::cout << typeString(S{}) << '\n';
    std::cout << typeString(nullptr) << '\n';
}
```

```txt
int
long
float
double
char
char [7]
class C
struct S
std::nullptr_t
```


## C#

```c#
using System;

namespace TypeDetection {
    class C { }
    struct S { }
    enum E {
        NONE,
    }

    class Program {
        static void ShowType<T>(T t) {
            Console.WriteLine("The type of '{0}' is {1}", t, t.GetType());
        }

        static void Main() {
            ShowType(5);
            ShowType(7.5);
            ShowType('d');
            ShowType(true);
            ShowType("Rosetta");
            ShowType(new C());
            ShowType(new S());
            ShowType(E.NONE);
            ShowType(new int[] { 1, 2, 3 });
        }
    }
}
```

```txt
The type of '5' is System.Int32
The type of '7.5' is System.Double
The type of 'd' is System.Char
The type of 'True' is System.Boolean
The type of 'Rosetta' is System.String
The type of 'TypeDetection.C' is TypeDetection.C
The type of 'TypeDetection.S' is TypeDetection.S
The type of 'NONE' is TypeDetection.E
The type of 'System.Int32[]' is System.Int32[]
```



## Crystal


```Ruby
def print_type(x)
  puts "Compile-time type of #{x} is #{typeof(x)}"
  puts "    Actual runtime type is #{x.class}" if x.class != typeof(x)
end

print_type 123
print_type 123.45
print_type  rand < 0.5 ? "1" : 0
print_type rand < 1.5
print_type nil
print_type 'c'
print_type "str"
print_type [1,2]
print_type({ 2, "two" })
print_type({a: 1, b: 2})
print_type ->(x : Int32){ x+2 > 0 }

```

```txt
Compile-time type of 123 is Int32
Compile-time type of 123.45 is Float64
Compile-time type of 0 is (Int32 | String)
    Actual runtime type is Int32
Compile-time type of true is Bool
Compile-time type of  is Nil
Compile-time type of c is Char
Compile-time type of str is String
Compile-time type of [1, 2] is Array(Int32)
Compile-time type of {2, "two"} is Tuple(Int32, String)
Compile-time type of {a: 1, b: 2} is NamedTuple(a: Int32, b: Int32)
Compile-time type of #<Proc(Int32, Bool):0x5645695ab9f0> is Proc(Int32, Bool)

```



## D


```D
import std.stdio;

auto typeString(T)(T _) {
    return T.stringof;
}

class C {}
struct S {}

void main() {
    writeln(typeString(1));
    writeln(typeString(1L));
    writeln(typeString(1.0f));
    writeln(typeString(1.0));
    writeln(typeString('c'));
    writeln(typeString("string"));
    writeln(typeString(new C()));
    writeln(typeString(S()));
    writeln(typeString(null));
}
```


```txt
int
long
float
double
char
string
C
S
typeof(null)
```



## Factor

Using dynamic dispatch:
<lang>USING: arrays formatting io kernel math prettyprint sequences
strings ;
IN: rosetta-code.type-detection

GENERIC: myprint ( object -- )

M: object myprint drop "I don't know how to print this." print ;
M: string myprint "I'm a string: \"%s\"\n" printf ;
M: fixnum myprint "I'm a fixnum: " write . ;
M: array  myprint "I'm an array: { " write
    [ pprint bl ] each "}" print ;

"Hello world." myprint
{ 1 2 3 4 5 }  myprint
123            myprint
3.1415         myprint
```

```txt

I'm a string: "Hello world."
I'm an array: { 1 2 3 4 5 }
I'm a fixnum: 123
I don't know how to print this.

```



## Go

Note that Go doesn't really have a character type. A single quoted character (such as 'd') is by default a ''rune'' (or 32 bit integer) literal representing its Unicode code-point.

```go
package main

import "fmt"

type any = interface{}

func showType(a any) {
    switch a.(type) {
    case rune:
        fmt.Printf("The type of '%c' is %T\n", a, a)
    default:
        fmt.Printf("The type of '%v' is %T\n", a, a)
    }
}

func main() {
    values := []any{5, 7.5, 2 + 3i, 'd', true, "Rosetta"}
    for _, value := range values {
        showType(value)
    }
}
```


```txt

The type of '5' is int
The type of '7.5' is float64
The type of '(2+3i)' is complex128
The type of 'd' is int32
The type of 'true' is bool
The type of 'Rosetta' is string

```


== {{header|J}} ==

Presumably this satisfies the task requirements...


```J
   echo 'one'
one
   echo 1
1
```


== {{header|Java}} ==
```Java
public class TypeDetection {
    private static void showType(Object a) {
        if (a instanceof Integer) {
            System.out.printf("'%s' is an integer\n", a);
        } else if (a instanceof Double) {
            System.out.printf("'%s' is a double\n", a);
        } else if (a instanceof Character) {
            System.out.printf("'%s' is a character\n", a);
        } else {
            System.out.printf("'%s' is some other type\n", a);
        }
    }

    public static void main(String[] args) {
        showType(5);
        showType(7.5);
        showType('d');
        showType(true);
    }
}
```

```txt
'5' is an integer
'7.5' is a double
'd' is a character
'true' is some other type
```


== {{header|JavaScript}} ==

[https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof?redirectlocale=en-US&redirectslug=JavaScript%2FReference%2FOperators%2Ftypeof]


```txt

console.log(typeof('foo')); // Returns string
console.log(typeof(12345)); // Returns number

```



## Kotlin


```scala
// version 1.0.6
fun showType(a: Any) = when (a) {
        is Int    -> println("'$a' is an integer")
        is Double -> println("'$a' is a double")
        is Char   -> println("'$a' is a character")
        else      -> println("'$a' is some other type")
    }

fun main(args: Array<String>) {
    showType(5)
    showType(7.5)
    showType('d')
    showType(true)
}
```


```txt

'5' is an integer
'7.5' is a double
'd' is a character
'true' is some other type

```



## Julia

In Julia, the function that returns the type of an object is the
<code>typeof</code> function, and the function <code>isa</code> tests
whether an object is of that type.

```Julia


julia> a = 1
1

julia> typeof(a)
Int32

julia> b = 1.0
1.0

julia> typeof(b)
Float64

julia> 1.0 isa Number
true

julia> 1.0 isa Int
false

julia> 1 isa Int
true

julia> typeof("hello")
String

julia> typeof(typeof("hello"))
DataType

julia> typeof(Set([1,3,4]))
Set{Int64}

julia> 1 isa String
false

julia> "1" isa Number
false

julia> "1" isa String
true

julia> isa(1.0,Float32)
false

julia> isa(1.0,Float64)
true


```


== {{header|OASYS Assembler}} ==

<lang oasys_oaa>
; The following method checks if a global variable or property is an
; object type. Does not work with locals and arguments.

[&OBJ#,^]
  ,^<,^<<    ; Remember old value
  ,^<*>      ; Create new object
  ,^<<DES    ; Destroy the object
  ,^<<EX     ; Check if variable has been cleared
  />1RF      ; It is clear
  :>0RF      ; It is not clear

```



## Perl

The function <code>ref</code> takes a reference to a variable, via '\', and returns the type. Some of the more common are shown here.
In the cases where the value in question is already a reference (<code>$regex</code> and <code>$subref</code>) the '\' is not used.

```perl
$scalar    = 1;
@array     = (1, 2);
%hash      = ('a' => 1);
$regex     = qr/foo.*bar/;
$reference = \%hash;
sub greet { print "Hello world!" };
$subref    = \&greet;

$fmt = "%-11s is type:  %s\n";
printf $fmt, '$scalar',    ref(\$scalar);
printf $fmt, '@array',     ref(\@array);
printf $fmt, '%hash',      ref(\%hash);
printf $fmt, '$regex',     ref( $regex);
printf $fmt, '$reference', ref(\$reference);
printf $fmt, '$subref',    ref( $subref);
```

```txt
$scalar     is type:  SCALAR
@array      is type:  ARRAY
%hash       is type:  HASH
$regex      is type:  Regexp
$reference  is type:  REF
$subref     is type:  CODE
```



## Perl 6

Perl 6 is a dynamic language that has gradual, duck typing. It provides introspection methods through its comprehensive MOP (Meta Object Protocol) making it easy to do type detection, subroutine signatures and multi-dispatch. Perl 6 types have two general flavors: content types and container types. Different container types have varying restrictions on what sort of content they can contain and in return provide specialized methods to operate on those contents. Content types give the compiler hints on how to best handle the information, what storage requirements it may have, what operators will work with it, etc.

This is really a very broad and kind of hand-wavey overview of Perl 6 types. For much more indepth coverage see [http://design.perl6.org/S02.html#Built-In_Data_Types| Perl 6 Synopsis S02: Bits and Pieces: Built-In Data Types]


```perl6
sub type ($t) { say $t.perl, "\tis type: ", $t.WHAT }

# some content types
.&type for 1, 2.0, 3e0, 4i, π, Inf, NaN, 'String';

# some primitive container types
.&type for $, [ ], @, { }, %, (5 .. 7), (8 ... 10), /0/, {;}, sub {}, ( );

# undefined things
.&type for Any, Nil;

# user defined types
class my-type { };

my my-type $object;

$object.&type;
```


```txt
1	is type: (Int)
2.0	is type: (Rat)
3e0	is type: (Num)
<0+4i>	is type: (Complex)
3.14159265358979e0	is type: (Num)
Inf	is type: (Num)
NaN	is type: (Num)
"String"	is type: (Str)
Any	is type: (Any)
$[]	is type: (Array)
$[]	is type: (Array)
{}	is type: (Hash)
{}	is type: (Hash)
5..7	is type: (Range)
(8, 9, 10).Seq	is type: (Seq)
/0/	is type: (Regex)
-> ;; $_? is raw { #`(Block|61385680) ... }	is type: (Block)
sub () { #`(Sub|62948936) ... }	is type: (Sub)
$()	is type: (List)
Any	is type: (Any)
Nil	is type: Nil
my-type	is type: (my-type)
```



## Phix

Phix builtin type tests are: integer(), atom(), string(), sequence(), and object() - the latter returns true unless arg is unassigned.

```Phix
procedure showtype(object o)
string t = iff(atom(o)?iff(integer(o)?"integer":"atom")
                      :iff(string(o)?"string":"sequence"))
    ?{t,o}
end procedure

showtype(5)
showtype(7.5)
showtype("string")
showtype({5,7.5,"string"})
```

```txt

{"integer",5}
{"atom",7.5}
{"string","string"}
{"sequence",{5,7.5,"string"}}

```



## PicoLisp

PicoLisp have only three base data [http://software-lab.de/doc/ref.html#data types].

```txt

: (num? 123)
-> 123
: (num? (1 2 3))
-> NIL
: (sym? 'a)
-> T
: (sym? 123)
-> NIL
: (lst? NIL)
-> T
: (lst? (1 . 2))
-> T
: (lst? (1 2 3))
-> T

```


== {{header|PHP}} ==

[http://php.net/manual/en/function.gettype.php]


```txt

echo gettype('foo'); // Returns string
echo gettype(12345); // Returns integer

```



###  Specific tester functions


* [http://php.net/manual/en/function.is-array.php is_array()]


## PowerShell

In PowerShell everything is an object and all objects have the GetType() method:

```PowerShell

[string]$str = "123"
$str.GetType()

```

```txt

IsPublic IsSerial Name                                     BaseType
-------- -------- ----                                     --------
True     True     String                                   System.Object

```


```PowerShell

[int]$int = $str -as [int]
$int.GetType()

```

```txt

IsPublic IsSerial Name                                     BaseType
-------- -------- ----                                     --------
True     True     Int32                                    System.ValueType

```


== {{header|Python}} ==
Built-in function <code>type()</code>

```txt

>>> type('foo')
<class 'str'>
>>> type(12345)
<class 'int'>
```


Testing types

```txt

>>> type('foo') is str
True
>>> type(123.0) is not int
True
>>> type([]) is list
True
>>> type({}) is dict
True
```



## Racket

Hopefully you can see how to extend the code to add all sorts of other types. If I did this, I&rsquo;d swamp the task page.
A good list of types supported/provided by Racket can be found in the Typed Racket reference:
http://docs.racket-lang.org/ts-reference/type-ref.html


```racket
#lang racket

(require racket/undefined)

(define fooer<%> (interface ()))
(define foo% (class* object% (fooer<%>)
               (super-new)))

(struct my-tree (l v r))
;; -----------------------------------------------------------------------------
(define (n.t f)
  (list f (regexp-replace #rx"\\?" (symbol->string (object-name f)) "")))

;; listed in the order (as close as) shown in
;; http://docs.racket-lang.org/guide/datatypes.html (section numbers next to
;; some entries)
(define type-tests.names
  `(,@(map n.t
           (list boolean? immutable? ; 3.1
                 ))
    ;; the famous scheme numerical tower
    ,@(map n.t ; 3.2
           (list number? complex? real? rational? integer? exact-integer?
                 exact-nonnegative-integer? exact-positive-integer?
                 inexact-real? fixnum? flonum? double-flonum? single-flonum?
                 zero? positive? negative? odd? even? exact? inexact?))
    ,@(map n.t
           (list char? ; 3.3 --- there are also char-alphabetic? etc -- but they're not
                       ;         types as such
                 string? ; 3.4
                 byte? bytes? ; 3.5
                 symbol? ; 3.6
                 keyword? ; 3.7
                 pair? null? list? ; 3.8
                 vector? ; 3.9
                 hash? hash-equal? hash-eqv? hash-eq? hash-weak? ; 3.10
                 box? ; 3.11
                 void? ; 3.12
                 ))
    ,(list (λ (v) (eq? v undefined)) "undefined") ; 3. 12
    ;; now we move to http://docs.racket-lang.org/reference/data.html
    ;; for section numbering
    ,@(map n.t
           (list
            regexp? pregexp? byte-regexp? byte-pregexp? ; 4.7
            stream? sequence? ; 4.14
            dict? ; 4.15
            set-equal? set-eqv? set-eq? set? set-mutable? set-weak? ; 4.16
            continuation? procedure? ; 4.17
            ))
    ;; class/interface testing
    ,(list (λ (v) (is-a? v object%)) "object%")
    ,(list (λ (v) (is-a? v foo%)) "foo%")
    ,(list (λ (v) (is-a? v fooer<%>)) "fooer<%>")

    ;; more types from reference (sections are top-level, mostly)
    ,@(map n.t
           (list
            syntax? ; 3.
            my-tree? ; 5.
            exn? exn:fail? exn:fail:filesystem? ; 10.2
            promise? ; 10.3
            ))

    ;; there's all sorts of other types to test!
    ))

(define (->type-names v)
  (let ((rv (for/list ((t.n (in-list type-tests.names))
                       #:when (with-handlers
                                  ((exn? (λ (x) #f)))
                                ((car t.n) v))) (cadr t.n))))
    (if (null? rv) (list "UNKNOWN") rv)))

(module+ test
  (require xml/xml)

  (define test-values
    (list 3.+4.i 3+4i (- pi) pi 0. 0 -0.5 0.5 -1/3 1/3
          -12345678909876543210123456789 12345678909876543210123456788 -132 133
          #\t #\null
          "" "monkeys" "\u03BB"
          -1 255 256
          #"" #"nibble"
          'hello '||
          '#:woo
          '() '(1 . 2) '(3) '(5 6)

          #() #(1) #("foo" 2 'bar)

          (make-hash)
          (make-hasheq)
          (make-hasheqv)
          (hash)
          (hasheq)
          (hasheqv)
          (make-weak-hash)
          (make-weak-hasheq)
          (make-weak-hasheqv)
          (make-immutable-hash)
          (make-immutable-hasheq)
          (make-immutable-hasheqv)

          (box "x")
          (void)
          undefined
          #rx".*" #px"3?" #rx#"t.m" #px#".i."
          (in-vector #(1 2 3)) (stream 1 2 3)
          #hash((a . "apple")) #("apple" "binana") '("apple" "binana")
           '((a . "apple") (b . "binana"))
           (set 1 2 3) (seteq 1 2 3) (seteqv 1 2 3)
           (mutable-set 1 2 3) (mutable-seteq 1 2 3) (mutable-seteqv 1 2 3)
           (weak-set 1 2 3) (weak-seteq 1 2 3) (weak-seteqv 1 2 3)

           + (λ (x) #t) (call/cc (λ (k) k))

           (new object%) (new foo%)

           #'(xxy zzy)
           (my-tree (my-tree #f 1 #f) 2 #f)
           (with-handlers ((exn? values)) (error 'aargh!))
           (with-handlers ((exn? values))
             (file->string "/tmp/there-is-no-way-this-file-exists---surely?"))
           (delay 3)
          ))

  ;; tempted to print a cross-reference table, but that would be too wide for RC (maybe)
  (write-xexpr #:insert-newlines? #f
   `(table (thead (tr (th "Value [~s]") (th "->type-name")))
           "\n"
           (tbody ,@(map (λ (v) `(tr "\n" (td ,(~s v))
                                     (td ,(string-join (->type-names v) ", "))))
                         test-values)))))

```


The following table is generated:

<table><thead><tr><th>Value [~s]</th><th>-&gt;type-name</th></tr></thead>
<tbody><tr>
<td>3.0+4.0i</td><td>number, complex, inexact</td></tr><tr>
<td>3+4i</td><td>number, complex, exact</td></tr><tr>
<td>-3.141592653589793</td><td>number, complex, real, rational, inexact-real, flonum, double-flonum, negative, inexact</td></tr><tr>
<td>3.141592653589793</td><td>number, complex, real, rational, inexact-real, flonum, double-flonum, positive, inexact</td></tr><tr>
<td>0.0</td><td>number, complex, real, rational, integer, inexact-real, flonum, double-flonum, zero, even, inexact</td></tr><tr>
<td>0</td><td>number, complex, real, rational, integer, exact-integer, exact-nonnegative-integer, fixnum, zero, even, exact, byte, sequence</td></tr><tr>
<td>-0.5</td><td>number, complex, real, rational, inexact-real, flonum, double-flonum, negative, inexact</td></tr><tr>
<td>0.5</td><td>number, complex, real, rational, inexact-real, flonum, double-flonum, positive, inexact</td></tr><tr>
<td>-1/3</td><td>number, complex, real, rational, negative, exact</td></tr><tr>
<td>1/3</td><td>number, complex, real, rational, positive, exact</td></tr><tr>
<td>-12345678909876543210123456789</td><td>number, complex, real, rational, integer, exact-integer, negative, odd, exact</td></tr><tr>
<td>12345678909876543210123456788</td><td>number, complex, real, rational, integer, exact-integer, exact-nonnegative-integer, exact-positive-integer, positive, even, exact, sequence</td></tr><tr>
<td>-132</td><td>number, complex, real, rational, integer, exact-integer, fixnum, negative, even, exact</td></tr><tr>
<td>133</td><td>number, complex, real, rational, integer, exact-integer, exact-nonnegative-integer, exact-positive-integer, fixnum, positive, odd, exact, byte, sequence</td></tr><tr>
<td>#\t</td><td>char</td></tr><tr>
<td>#\nul</td><td>char</td></tr><tr>
<td>""</td><td>immutable, string, sequence</td></tr><tr>
<td>"monkeys"</td><td>immutable, string, sequence</td></tr><tr>
<td>"λ"</td><td>immutable, string, sequence</td></tr><tr>
<td>-1</td><td>number, complex, real, rational, integer, exact-integer, fixnum, negative, odd, exact</td></tr><tr>
<td>255</td><td>number, complex, real, rational, integer, exact-integer, exact-nonnegative-integer, exact-positive-integer, fixnum, positive, odd, exact, byte, sequence</td></tr><tr>
<td>256</td><td>number, complex, real, rational, integer, exact-integer, exact-nonnegative-integer, exact-positive-integer, fixnum, positive, even, exact, sequence</td></tr><tr>
<td>#""</td><td>immutable, bytes, sequence</td></tr><tr>
<td>#"nibble"</td><td>immutable, bytes, sequence</td></tr><tr>
<td>hello</td><td>symbol</td></tr><tr>
<td>||</td><td>symbol</td></tr><tr>
<td>#:woo</td><td>keyword</td></tr><tr>
<td>()</td><td>null, list, stream, sequence, dict</td></tr><tr>
<td>(1 . 2)</td><td>pair</td></tr><tr>
<td>(3)</td><td>pair, list, stream, sequence</td></tr><tr>
<td>(5 6)</td><td>pair, list, stream, sequence</td></tr><tr>
<td>#()</td><td>immutable, vector, sequence, dict</td></tr><tr>
<td>#(1)</td><td>immutable, vector, sequence, dict</td></tr><tr>
<td>#("foo" 2 (quote bar))</td><td>immutable, vector, sequence, dict</td></tr><tr>
<td>#hash()</td><td>hash, hash-equal, sequence, dict</td></tr><tr>
<td>#hasheq()</td><td>hash, hash-eq, sequence, dict</td></tr><tr>
<td>#hasheqv()</td><td>hash, hash-eqv, sequence, dict</td></tr><tr>
<td>#hash()</td><td>immutable, hash, hash-equal, sequence, dict</td></tr><tr>
<td>#hasheq()</td><td>immutable, hash, hash-eq, sequence, dict</td></tr><tr>
<td>#hasheqv()</td><td>immutable, hash, hash-eqv, sequence, dict</td></tr><tr>
<td>#&lt;hash&gt;</td><td>hash, hash-equal, hash-weak, sequence, dict</td></tr><tr>
<td>#&lt;hash&gt;</td><td>hash, hash-eq, hash-weak, sequence, dict</td></tr><tr>
<td>#&lt;hash&gt;</td><td>hash, hash-eqv, hash-weak, sequence, dict</td></tr><tr>
<td>#hash()</td><td>immutable, hash, hash-equal, sequence, dict</td></tr><tr>
<td>#hasheq()</td><td>immutable, hash, hash-eq, sequence, dict</td></tr><tr>
<td>#hasheqv()</td><td>immutable, hash, hash-eqv, sequence, dict</td></tr><tr>
<td>#&amp;"x"</td><td>box</td></tr><tr>
<td>#&lt;void&gt;</td><td>void</td></tr><tr>
<td>#&lt;undefined&gt;</td><td>undefined</td></tr><tr>
<td>#rx".*"</td><td>regexp</td></tr><tr>
<td>#px"3?"</td><td>regexp, pregexp</td></tr><tr>
<td>#rx#"t.m"</td><td>byte-regexp</td></tr><tr>
<td>#px#".i."</td><td>byte-regexp, byte-pregexp</td></tr><tr>
<td>#&lt;sequence&gt;</td><td>sequence</td></tr><tr>
<td>#&lt;stream&gt;</td><td>stream, sequence</td></tr><tr>
<td>#hash((a . "apple"))</td><td>immutable, hash, hash-equal, sequence, dict</td></tr><tr>
<td>#("apple" "binana")</td><td>immutable, vector, sequence, dict</td></tr><tr>
<td>("apple" "binana")</td><td>pair, list, stream, sequence</td></tr><tr>
<td>((a . "apple") (b . "binana"))</td><td>pair, list, stream, sequence, dict</td></tr><tr>
<td>#&lt;set: 1 3 2&gt;</td><td>stream, sequence, set-equal, set</td></tr><tr>
<td>#&lt;seteq: 1 2 3&gt;</td><td>stream, sequence, set-eq, set</td></tr><tr>
<td>#&lt;seteqv: 1 2 3&gt;</td><td>stream, sequence, set-eqv, set</td></tr><tr>
<td>#&lt;mutable-set: 1 2 3&gt;</td><td>sequence, set-equal, set-mutable</td></tr><tr>
<td>#&lt;mutable-seteq: 1 2 3&gt;</td><td>sequence, set-eq, set-mutable</td></tr><tr>
<td>#&lt;mutable-seteqv: 1 2 3&gt;</td><td>sequence, set-eqv, set-mutable</td></tr><tr>
<td>#&lt;weak-set: 1 3 2&gt;</td><td>sequence, set-equal, set-weak</td></tr><tr>
<td>#&lt;weak-seteq: 1 2 3&gt;</td><td>sequence, set-eq, set-weak</td></tr><tr>
<td>#&lt;weak-seteqv: 1 2 3&gt;</td><td>sequence, set-eqv, set-weak</td></tr><tr>
<td>#&lt;procedure:+&gt;</td><td>procedure</td></tr><tr>
<td>#&lt;procedure:...pe-detection.rkt:113:13&gt;</td><td>procedure</td></tr><tr>
<td>#&lt;continuation&gt;</td><td>continuation, procedure</td></tr><tr>
<td>#(struct:object)</td><td>object%</td></tr><tr>
<td>#(struct:object:foo% ...)</td><td>object%, foo%, fooer&lt;%&gt;</td></tr><tr>
<td>#&lt;syntax:D:\Users\tim\Dropbox\hacking\rosettacode\type-detection.rkt:117:13 (xxy zzy)&gt;</td><td>syntax</td></tr><tr>
<td>#&lt;my-tree&gt;</td><td>my-tree</td></tr><tr>
<td>#(struct:exn:fail "error: aargh!" #&lt;continuation-mark-set&gt;)</td><td>exn, exn:fail</td></tr><tr>
<td>#(struct:exn:fail:filesystem "file-size: file not found\n  path: D:/tmp/there-is-no-way-this-file-exists---surely?" #&lt;continuation-mark-set&gt;)</td><td>exn, exn:fail, exn:fail:filesystem</td></tr><tr>
<td>#&lt;promise:...e/type-detection.rkt:122:11&gt;</td><td>promise</td></tr></tbody></table>


## REXX

These are some of the tests that can be performed on REXX variables (values) to determine which   ''type''   they are.

Although everything   (as far as variables are concerning)   in the REXX language is a character string,   character

strings can be classified by having certain characteristics,   or in other words, ''types''.

Characteristics of these   ''types''   can overlap.

```rexx
/*REXX program  displays  what  "type"  a variable is  (based on the variable's value). */
signal on noValue                                /*trap for undefined REXX variables.   */
y= 1938       ;           call showType y        /*╔═══════════════════════════════════╗*/
y= 77.1       ;           call showType y        /*║ All REXX variables are stored as  ║*/
y=            ;           call showType y        /*║ character strings, even numbers.  ║*/
y= '   '      ;           call showType y        /*║ If a variable string is numeric,  ║*/
y= 'abc'      ;           call showType y        /*║ all comparisons (IF statements)   ║*/
y= 'ABC'      ;           call showType y        /*║ that are made with numbers are    ║*/
y= 'aBc'      ;           call showType y        /*║ compared numerically.  If not     ║*/
y= '1515'x    ;           call showType y        /*║ numeric,  the string is compared  ║*/
y= '10 11'x   ;           call showType y        /*║ char by char after leading and    ║*/
y= '00 0001'b ;           call showType y        /*║ trailing blanks are removed, and  ║*/
y= '1'b       ;           call showType y        /*║ shorter strings are padded with   ║*/
y= ' + 1938 ' ;           call showType y        /*║ blanks to match the longer string.║*/
y= ' - 1.2e4' ;           call showType y        /*╚═══════════════════════════════════╝*/
y= '1'        ;           call showType y        /*                                     */
                          call showType yyy      /*note:  the variable YYY is undefined.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
noValue:   say '      REXX variable '   condition("D")     ' is undefined.';      exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
showType:  procedure; parse arg x 1 xu; upper xu /*get true value & an uppercase version*/
           @= '      value is';              say  @  x
                                             say  @  'of length'       length(x)
           if  x ==''                   then say  @  "null."
           if  x\==''  &  x=''          then say  @  "all blank."
           if  datatype(x, 'N')         then say  @  "numeric (decimal)."
                                        else say  @  "a character string (not numeric)."
           if  datatype(x, 'W')         then say  @  "an integer (a whole number)."
           if  datatype(x, 'N') &,
              \datatype(x, 'W')         then say  @  "not an integer."
           if  datatype(x, 'N') &,
               pos('E', xu)\==0         then say  @  "a number in exponential format."
           if  datatype(x, 'A')         then say  @  "an alphanumeric string."
           if  datatype(x, 'U')         then say  @  "all uppercase (Latin) letters."
           if  datatype(x, 'L')         then say  @  "all lowercase (Latin) letters."
           if \datatype(x, 'L') &,
              \datatype(x, 'U') &,
               datatype(x, 'M')         then say  @  "of mixed case (Latin) letters."
           if  datatype(x, 'B')         then say  @  "binary."
           if  datatype(x, 'X')         then say  @  "hexadecimal."
           if  datatype(x, 'S')         then say  @  "a REXX symbol."
           say copies('▒',  50)                  /*a fence that is used as a separator. */
           return
```

```txt

      value is 1938
      value is of length 4
      value is numeric (decimal).
      value is an integer (a whole number).
      value is an alphanumeric string.
      value is hexadecimal.
      value is a REXX symbol.
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is 77.1
      value is of length 4
      value is numeric (decimal).
      value is not an integer.
      value is a REXX symbol.
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is
      value is of length 0
      value is null.
      value is a character string (not numeric).
      value is binary.
      value is hexadecimal.
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is
      value is of length 3
      value is all blank.
      value is a character string (not numeric).
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is abc
      value is of length 3
      value is a character string (not numeric).
      value is an alphanumeric string.
      value is all lowercase (Latin) letters.
      value is hexadecimal.
      value is a REXX symbol.
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is ABC
      value is of length 3
      value is a character string (not numeric).
      value is an alphanumeric string.
      value is all uppercase (Latin) letters.
      value is hexadecimal.
      value is a REXX symbol.
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is aBc
      value is of length 3
      value is a character string (not numeric).
      value is an alphanumeric string.
      value is of mixed case (Latin) letters.
      value is hexadecimal.
      value is a REXX symbol.
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is §§
      value is of length 2
      value is a character string (not numeric).
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is ►◄
      value is of length 2
      value is a character string (not numeric).
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is ☺
      value is of length 1
      value is a character string (not numeric).
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is ☺
      value is of length 1
      value is a character string (not numeric).
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is  + 1938
      value is of length 8
      value is numeric (decimal).
      value is an integer (a whole number).
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is  - 1.2e4
      value is of length 8
      value is numeric (decimal).
      value is an integer (a whole number).
      value is a number in exponential format.
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      value is 1
      value is of length 1
      value is numeric (decimal).
      value is an integer (a whole number).
      value is an alphanumeric string.
      value is binary.
      value is hexadecimal.
      value is a REXX symbol.
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
      REXX variable  YYY  is undefined.

```



## Ring


```ring

# Project : Type detection

see "5 -> " + type(5) + nl
see "7.5 -> " + type(7.5) + nl
see "d -> " + type('d') + nl

```

Output:

```txt

5 -> NUMBER
7.5 -> NUMBER
d -> STRING

```



## Rust


While Rust compiles to machine code, it does provide multiple mechanisms for requesting that type information be preserved so that it can be queried at runtime.


### <code>enum</code>

Rust has first-class support for tagged unions via the <code>enum</code> keyword.

The compiler will require that all possible values are handled, even if it's as simple as a fallthrough <code>_ => unreachable!()</code> which causes the program to die in a memory-safe way.


```rust
enum ExpectedTypes {
    Int(i64),
    UInt(u64),
    Real(f64),
    Text(String),
    Uncertain,
}

// Avoid having to prefix each variant name with ExpectedTypes::
use ExpectedTypes::*;

fn main() {
    let enum_test = &[Int(-5), UInt(10), Real(-15.5), Text("Twenty".to_owned()),
                     Uncertain];

    for entry in enum_test {
        match entry {
            Int(x) => println!("Got an integer: {}", x),
            UInt(x) => println!("Got an unsigned integer: {}", x),
            Real(x) => println!("Got a floating-point number: {}", x),
            Text(x) => println!("Got a string of text: {}", x),
            Uncertain => println!("Value is uncertain"),
        }
    }
}
```


Output:

```txt
Got an integer: -5
Got an unsigned integer: 10
Got a floating-point number: -15.5
Got a string of text: Twenty
Value is uncertain
```


(Rust also supports untagged unions, but they're only intended to be used for calling or exposing interfaces which use the C ABI.)


### Traits


Rust traits are analogous to interfaces in other languages and support both monomorphic dispatch via generics (the compiler will generate a copy of the function for each combination of input arguments used) and polymorphic dispatch via type erasure (known as "Trait objects").

As Rust allows you to implement your own traits on other people's types, this can be used to implement type detection.


```rust
use std::error::Error;
use std::fs::File;
use std::io::{self,prelude::*};
use std::net::TcpStream;

// Declare an Identify trait and implement it for a bunch of types
pub trait Identify {
    fn id(&self) -> &'static str;
}

// Declare a macro to compact away the boilerplate
macro_rules! declare_id {
    ( $struct:ty, $id:ident ) => (
        impl Identify for $struct {
            fn id(&self) -> &'static str {
                return stringify!($id);
            }
        }
    )
}

// Use the macro to `impl` the Identify trait for a bunch of types
declare_id!(io::Empty, empty);
declare_id!(File, file_handle);
declare_id!(TcpStream, tcp_stream);
declare_id!(u8, int8);
declare_id!(&str, string);

// This uses monomorphic dispatch via generics.
// A copy of the function will be generated for each input type encountered
pub fn calc_size<R: Read + Identify>(readable: R) {
    let id = readable.id();
    let mut size = 0;

    for _byte in readable.bytes() {
        size += 1;
    }
    println!(" {}: {} bytes", id, size);
}

// This uses polymorphic dispatch via type erasure
pub fn identify(thing: &dyn Identify) {
    println!(" Got {}", thing.id());
}

fn main() -> Result<(), Box<dyn Error>> {
    println!("Monomorphic Generic Interface:");
    calc_size(File::open("/bin/sh")?);
    calc_size(io::empty());
    calc_size(TcpStream::connect("127.0.0.1:37")?);

    println!("\nPolymorphic Interface:");
    for x in &[&15u8 as &dyn Identify, &"Hello" as &dyn Identify] {
        identify(*x);
    }

    Ok(())
}
```


Output:

```txt
Monomorphic Generic Interface:
 file_handle: 154072 bytes
 empty: 0 bytes
 tcp_stream: 4 bytes

Polymorphic Interface:
 Got int8
 Got string
```



### The <code>Any</code> trait


Finally, while it's more limited than it appears, Rust does have some small degree of support for type introspection.


```rust
use std::any::Any;

pub fn is_string(thing: &dyn Any) {
    if thing.is::<&str>() {
        println!("It's a string slice!");
    } else {
        println!("Dunno");
    }
}

fn main() {
    is_string(&"Hello, World!");
    is_string(&5u16);
}
```


Output:

```txt
It's a string slice!
Dunno
```



## Scala


```scala
object TypeDetection extends App {
  def showType(a: Any) = a match {
    case a: Int => println(s"'$a' is an integer")
    case a: Double => println(s"'$a' is a double")
    case a: Char => println(s"'$a' is a character")
    case _ => println(s"'$a' is some other type")
  }

  showType(5)
  showType(7.5)
  showType('d')
  showType(true)

  println(s"\nSuccessfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")

}
```


## VBA

VBA has a built-in function TypeName (VarType returns a number), which can also recognize arrays.

```vb
Public Sub main()
    Dim c(1) As Currency
    Dim d(1) As Double
    Dim dt(1) As Date
    Dim a(1) As Integer
    Dim l(1) As Long
    Dim s(1) As Single
    Dim e As Variant
    Dim o As Object
    Set o = New Application
    Debug.Print TypeName(o)
    Debug.Print TypeName(1 = 1)
    Debug.Print TypeName(CByte(1))
    Set o = New Collection
    Debug.Print TypeName(o)
    Debug.Print TypeName(1@)
    Debug.Print TypeName(c)
    Debug.Print TypeName(CDate(1))
    Debug.Print TypeName(dt)
    Debug.Print TypeName(CDec(1))
    Debug.Print TypeName(1#)
    Debug.Print TypeName(d)
    Debug.Print TypeName(e)
    Debug.Print TypeName(CVErr(1))
    Debug.Print TypeName(1)
    Debug.Print TypeName(a)
    Debug.Print TypeName(1&)
    Debug.Print TypeName(l)
    Set o = Nothing
    Debug.Print TypeName(o)
    Debug.Print TypeName([A1])
    Debug.Print TypeName(1!)
    Debug.Print TypeName(s)
    Debug.Print TypeName(CStr(1))
    Debug.Print TypeName(Worksheets(1))
End Sub
```
```txt
Application
Boolean
Byte
Collection
Currency
Currency()
Date
Date()
Decimal
Double
Double()
Empty
Error
Integer
Integer()
Long
Long()
Nothing
Range
Single
Single()
String
Worksheet
```


## Visual Basic .NET

```vbnet
Module TypeDetection

    Sub Main()
        printTypeOf(5)
        printTypeOf("VB.Net")
        printTypeOf(7.2)
        printTypeOf(True)
    End Sub

    Private Sub printTypeOf(obj As Object)
        Console.WriteLine(obj.GetType.ToString)
    End Sub

End Module

```


```txt

System.Int32
System.String
System.Double
System.Boolean

```



## zkl


```zkl
fcn processText(data_or_fileName){ // unknown
   if (data_or_fileName.isType(String)) // == .isType("")
      data_or_fileName=File(data_or_fileName,"rb").read(); //-->Data
   text:=data_or_fileName.text;  //-->String
   doTheActualTextProcessing(text);
}
fcn doTheActualTextProcessing(text){ println(text) }
```

If an int is passed in, (123).text --> "123", other objects might throw an exception.

How to use:

```zkl
processText("foo.txt");
processText(Data(Void,"This is some text"));
// fake up a class that holds a string:
cs:=class{ var text }; cs.text="this is more text";
processText(cs);
```

```txt

this is foo.txt

This is some text
this is more text

```

