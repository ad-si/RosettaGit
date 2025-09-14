+++
title = "Test a function"
description = ""
date = 2019-10-21T00:04:08Z
aliases = []
[extra]
id = 4276
[taxonomies]
categories = ["task", "Testing"]
tags = []
languages = [
  "acl2",
  "ada",
  "autohotkey",
  "brat",
  "c",
  "clojure",
  "csharp",
  "d",
  "delphi",
  "e",
  "echolisp",
  "erlang",
  "euphoria",
  "factor",
  "fantom",
  "fortran",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "lasso",
  "lua",
  "mathematica",
  "netrexx",
  "nim",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "picolisp",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "retro",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "sql_pl",
  "swift",
  "tcl",
  "unix_shell",
  "vba",
  "zkl",
]
+++

## Task

Using a well-known testing-specific library/module/suite for your language, write some tests for your language's entry in [[Palindrome]]. If your language does not have a testing specific library well known to the language's community then state this or omit the language.


## ACL2


Using [http://www.ccs.neu.edu/home/cce/acl2/doublecheck.html DoubleCheck]:


```Lisp
(defun reverse-split-at-r (xs i ys)
  (if (zp i)
      (mv xs ys)
      (reverse-split-at-r (rest xs) (1- i)
                          (cons (first xs) ys))))

(defun reverse-split-at (xs i)
  (reverse-split-at-r xs i nil))

(defun is-palindrome (str)
  (let* ((lngth (length str))
         (idx (floor lngth 2)))
    (mv-let (xs ys)
            (reverse-split-at (coerce str 'list) idx)
            (if (= (mod lngth 2) 1)
                (equal (rest xs) ys)
                (equal xs ys)))))

(include-book "testing" :dir :teachpacks)

(check-expect (is-palindrome "abba") t)
(check-expect (is-palindrome "mom") t)
(check-expect (is-palindrome "dennis sinned") t)
(check-expect (is-palindrome "palindrome") nil)
(check-expect (is-palindrome "racecars") nil)

(include-book "doublecheck" :dir :teachpacks)

(defrandom random-palindrome ()
  (let ((chars (random-list-of (random-char))))
    (coerce (append chars (reverse chars))
            'string)))

(defproperty palindrome-test
  (p :value (random-palindrome))
  (is-palindrome p))
```



## Ada


For normal use there is pragma Assert, functioning the same as many other languages.

For larger testing frameworks, there are packages like [http://libre.adacore.com/libre/tools/aunit/ Aunit]


```Ada
with Ada.Text_IO;

procedure Test_Function is

   function Palindrome (Text : String) return Boolean is
   begin
      for Offset in 0 .. Text'Length / 2 - 1 loop
         if Text (Text'First + Offset) /= Text (Text'Last - Offset) then
            return False;
         end if;
      end loop;
      return True;
   end Palindrome;

   str1 : String := "racecar";
   str2 : String := "wombat";

begin
   begin
      pragma Assert(False); -- raises an exception if assertions are switched on
      Ada.Text_IO.Put_Line("Skipping the test! Please compile with assertions switched on!");
   exception
      when others => -- assertions are switched on -- perform the tests
         pragma Assert (Palindrome (str1) = True,  "Assertion on str1 failed");
         pragma Assert (Palindrome (str2) = False, "Assertion on str2 failed");
         Ada.Text_IO.Put_Line("Test Passed!");
   end;
end Test_Function;
```


Ada 2012 introduced a new way to specify functions and test their correctness: Pre- and Postoconditions.


```Ada
   function Palindrome (Text : String) return Boolean
     with Post => Palindrome'Result =
     (Text'Length < 2 or else
	((Text(Text'First) = Text(Text'Last)) and then
	   Palindrome(Text(Text'First+1 .. Text'Last-1))));
```



## AutoHotkey

there is no "well known" testing library, but here is a simple testing framework:

test library: assert.ahk

```AutoHotkey
; assert.ahk
;; assert(a, b, test=2)
assert(a, b="blank", test=0)
{
  if (b = "blank")
{
    if !a
      msgbox % "blank value"
      return 0
}
    if equal_list(a, b, "`n")
      return 0
    else
    msgbox % test . ":`n" . a . "`nexpected:`n" . b
}

!r::reload

;; equal_list(a, b, delimiter)
equal_list(a, b, delimiter)
{
  loop, parse, b, %delimiter%
  {
    if instr(a, A_LoopField)
      continue
    else
      return 0
  }
  loop, parse, a, %delimiter%
  {
    if instr(b, A_LoopField)
      continue
    else
      return 0
  }

  return 1
}
```

test example:

```AutoHotkey
assert(isPalindrome("in girum imus nocte et consumimur igni"), 1
, "palindrome test")
assert(broken("in girum imus nocte et consumimur igni"), "works"
, "broken test")
/*
output:
---------------------------
testPalindrome.ahk
---------------------------
broken test:
broken
expected:
works
*/

broken(x){
return "broken"
}

#Include assert.ahk
#Include palindrome.ahk
```



## Brat


```brat
include :assert

palindrome? = { str |
  str = str.downcase.sub /\s+/ ""
  str == str.reverse
}

setup name: "palindrome test" {
  test "is a palindrome" {
    assert { palindrome? "abba" }
  }

  test "is not a palindrome" {
    assert_false { palindrome? "abb" }
  }

  test "is not a string" {
    assert_fail { palindrome? 1001 }
  }

  test "this test fails" {
    assert { palindrome? "blah blah" }
  }
}
```


Output:


```txt
Loading tests...
Running palindrome test...
(4/4) this test fails
Test failure(s):

        1. 'this test fails': assert failed

4 tests, 4 assertions, 1 failures.
```



## C



```C>#include <assert.h

int IsPalindrome(char *Str);

int main()
{
    assert(IsPalindrome("racecar"));
    assert(IsPalindrome("alice"));
}

```



## Clojure


```lisp

(use 'clojure.test)

(deftest test-palindrome?
  (is (= true (palindrome? "amanaplanacanalpanama")))
  (is (= false (palindrome? "Test 1, 2, 3"))))

(run-tests)

```



## C#


First, using the VisualStudio TestTools for unit tests.  I'm testing both of the methods for palindrome detection created in the article.


```c#

using Microsoft.VisualStudio.TestTools.UnitTesting;
using PalindromeDetector.ConsoleApp;

namespace PalindromeDetector.VisualStudioTests
{
    [TestClass]
    public class VSTests
    {
        [TestMethod]
        public void PalindromeDetectorCanUnderstandPalindrome()
        {
            //Microsoft.VisualStudio.QualityTools.UnitTestFramework v4.0.30319
            bool expected = true;
            bool actual;
            actual = Program.IsPalindrome("1");
            Assert.AreEqual(expected, actual);
            actual = Program.IsPalindromeNonRecursive("1");
            Assert.AreEqual(expected, actual);
            actual = Program.IsPalindrome("ingirumimusnocteetconsumimurigni");
            Assert.AreEqual(expected, actual);
            actual = Program.IsPalindromeNonRecursive("ingirumimusnocteetconsumimurigni");
            Assert.AreEqual(expected, actual);
        }
        [TestMethod]
        public void PalindromeDetecotryCanUnderstandNonPalindrome()
        {
            bool notExpected = true;
            bool actual = Program.IsPalindrome("ThisIsNotAPalindrome");
            Assert.AreNotEqual(notExpected, actual);
            actual = Program.IsPalindromeNonRecursive("ThisIsNotAPalindrome");
            Assert.AreNotEqual(notExpected, actual);
        }
    }
}
```


Second, NUnit tests.  Couldn't test these because of namespace issues with NUnit, but I'm sure they work.

```c#

using NUnit.Framework;
using PalindromeDetector.ConsoleApp;

namespace PalindromeDetector.VisualStudioTests
{
    [TestFixture]
    public class NunitTests
    {
        [Test]
        public void PalindromeDetectorCanUnderstandPalindrome()
        {
            //nunit.framework v2.0.50727
            bool expected = true;
            bool actual;
            actual = Program.IsPalindrome("1");
            Assert.AreEqual(expected, actual);
            actual = Program.IsPalindromeNonRecursive("1");
            Assert.AreEqual(expected, actual);
            actual = Program.IsPalindrome("ingirumimusnocteetconsumimurigni");
            Assert.AreEqual(expected, actual);
            actual = Program.IsPalindromeNonRecursive("ingirumimusnocteetconsumimurigni");
            Assert.AreEqual(expected, actual);
        }
        [Test]
        public void PalindromeDetectorUnderstandsNonPalindrome()
        {
            bool notExpected = true;
            bool actual;
            actual = Program.IsPalindrome("NotAPalindrome");
            Assert.AreEqual(notExpected, actual);
            actual = Program.IsPalindromeNonRecursive("NotAPalindrome");
            Assert.AreEqual(notExpected, actual);
        }
    }
}
```



## D


```d
unittest {
  assert(isPalindrome("racecar"));
  assert(isPalindrome("bob"));
  assert(!isPalindrome("alice"));
}
```



## Delphi


Using built in assertions.


```Delphi
  Assert(IsPalindrome('salàlas'), 'salàlas is a valid palindrome');
  Assert(IsPalindrome('Ingirumimusnocteetconsumimurigni'), 'Ingirumimusnocteetconsumimurigni is a valid palindrome');
  Assert(not IsPalindrome('123'), '123 is not a valid palindrome');
```


Using [[wp:DUnit|DUnit]], an open source unit testing framework that is bundled with Delphi.


```Delphi
  Check(IsPalindrome('salàlas'), 'salàlas is a valid palindrome');
  Check(IsPalindrome('Ingirumimusnocteetconsumimurigni'), 'Ingirumimusnocteetconsumimurigni is a valid palindrome');
  Check(not IsPalindrome('123'), '123 is not a valid palindrome');
```



## E


The standard testing tool in E is Updoc, a system which takes test scripts formatted in the same style as a [[REPL]] session and verifies that executing them produces the specified result values.


```e
#!/usr/bin/env rune

? def isPalindrome(string :String) {
>   def upper := string.toUpperCase()
>   def last := upper.size() - 1
>   for i => c ? (upper[last - i] != c) in upper(0, upper.size() // 2) {
>     return false
>   }
>   return true
> }

? isPalindrome("")
# value: true

? isPalindrome("a")
# value: true

? isPalindrome("aa")
# value: true

? isPalindrome("baa")
# value: false

? isPalindrome("baab")
# value: true

? isPalindrome("ba_ab")
# value: true

? isPalindrome("ba_ ab")
# value: false

? isPalindrome("ba _ ab")
# value: true

? isPalindrome("ab"*2)
# value: false

? def x := "ab" * 2**15; null

? x.size()
# value: 65536

? def xreversed := "ba" * 2**15; null

? isPalindrome(x + xreversed)
# value: true

? (x + xreversed).size()
# value: 131072
```



## EchoLisp

EchoLisp provides (assert <true-value?> ["fail-message"]) and (check-expect <expression> <expected-result>).

```lisp

(assert (palindrome? "aba")) → #t
(assert (palindrome? "abbbca") "palindrome fail")
💥 error: palindrome fail : assertion failed : (palindrome? abbbca)

(check-expect (palindrome? "aba") #t) → #t
(check-expect (palindrome? "abcda") #f) → #t
(check-expect (palindrome? "abcda") #t)
😐 warning: #t : check failed : (palindrome? abcda) → #f
(assert (palindrome? "un roc lamina l animal cornu")) → #t

```



## Erlang

This is a unit test so I use Eunit. For system tests "Common Test" would be used. Both are built in.

```Erlang

-module( palindrome_tests ).
-compile( export_all ).
-include_lib( "eunit/include/eunit.hrl" ).

abcba_test() -> ?assert( palindrome:is_palindrome("abcba") ).

abcdef_test() -> ?assertNot( palindrome:is_palindrome("abcdef") ).

```

```txt

25> eunit:test(palindrome).
  All 2 tests passed.

```



## Euphoria



```euphoria

--unittest in standard library 4.0+
include std/unittest.e
include palendrome.e  --routines to be tested

object p = "12321"

test_equal("12321", 1, isPalindrome(p))
test_equal("r12321", 1, isPalindrome(reverse(p)))

test_report()


```



=={{header|F_Sharp|F#}}==
```fsharp
let palindrome (s : string) =
    let a = s.ToUpper().ToCharArray()
    Array.rev a = a


open NUnit.Framework

[<TestFixture>]
type TestCases() =
    [<Test>]
    member x.Test01() =
        Assert.IsTrue(palindrome "radar")

    [<Test>]
    member x.Test02() =
        Assert.IsFalse(palindrome "hello")
```




## Factor

By convention, if there is a vocabulary at ''x/x.factor'', then its tests belong in ''x/x-tests.factor''.

''palindrome/palindrome.factor''


```factor
USING: kernel sequences ;
IN: palindrome

: palindrome? ( string -- ? ) dup reverse = ;
```


''palindrome/palindrome-tests.factor''


```factor
USING: palindrome tools.test ;
IN: palindrome.tests

[ t ] [ "racecar" palindrome? ] unit-test
[ f ] [ "ferrari" palindrome? ] unit-test
```


To run these tests from the listener:


```factor
( scratchpad ) "palindrome" test
```


Factor's tutorial, [http://docs.factorcode.org/content/article-first-program.html Your first program], uses ''palindrome?'' as its example. The tutorial shows how to create tests for ''palindrome?'' and how to fix a failing test.


## Fantom


To use the built-in test library, the program must be compiled into a pod.  The layout for a simple pod and its build file is given in the [http://fantom.org/doc/docIntro/HelloWorld.html#pod documentation], and also information for adding and running the [http://fantom.org/doc/docTools/Fant.html test files].


```fantom

class TestPalindrome : Test
{
  public Void testIsPalindrome ()
  {
    verify(Palindrome.isPalindrome(""))
    verify(Palindrome.isPalindrome("a"))
    verify(Palindrome.isPalindrome("aa"))
    verify(Palindrome.isPalindrome("aba"))
    verifyFalse(Palindrome.isPalindrome("abb"))
    verify(Palindrome.isPalindrome("salàlas"))
    verify(Palindrome.isPalindrome("In girum imus nocte et consumimur igni".lower.replace(" ","")))
  }
}

```



## Fortran

There is no standard or popular facility. Compilers usually do not even check that a function or subroutine is invoked with the correct number of parameters let alone the correct types. Testing that a function returns expected values is entirely a matter for the programmer and whatever tools that may be devised, either directly for the function in question or adapted from some other project where this had been done.


## Go

Using Go's standard command, go test.

```go
package pal

import "testing"

func TestPals(t *testing.T) {
    pals := []string{
        "",
        ".",
        "11",
        "ere",
        "ingirumimusnocteetconsumimurigni",
    }
    for _, s := range pals {
        if !IsPal(s) {
            t.Error("IsPal returned false on palindrome,", s)
        }
    }
}

func TestNonPals(t *testing.T) {
    nps := []string{
        "no",
        "odd",
        "salàlas",
    }
    for _, s := range nps {
        if IsPal(s) {
            t.Error("IsPal returned true on non-palindrome,", s)
        }
    }
}
```

Output of go test:

```txt

PASS
ok      pal     0.002s

```



## Haskell


A notable testing library for Haskell is QuickCheck. It works in a way particularly supported by Haskell's type inference: you provide a function return a boolean, the test, and QuickCheck automatically generates random values for the function's parameters and checks that it returns <code>True</code> for all of them.


```haskell
import Test.QuickCheck

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

{- There is no built-in definition of how to generate random characters;
   here we just specify ASCII characters. Generating strings then automatically
   follows from the definition of String as list of Char. -}
instance Arbitrary Char where
  arbitrary = choose ('\32', '\127')

--                                            /------------------------- the randomly-generated parameters
--                                            |      /------------------ the constraint on the test values
--                                            |      |                /- the condition which should be true
--                                            v      v                v
main = do
  putStr "Even palindromes: " >> quickCheck (\s   ->                  isPalindrome (s ++ reverse s))
  putStr "Odd palindromes:  " >> quickCheck (\s   -> not (null s) ==> isPalindrome (s ++ (tail.reverse) s))
  putStr "Non-palindromes:  " >> quickCheck (\i s -> not (null s) && 0 <= i && i < length s && i*2 /= length s
                                                                  ==> not (isPalindrome (take i s ++ "•" ++ drop i s)))
```


The <code>==&gt;</code> operator is used to constrain the randomly-generated values: the second test needs a nonempty string, and the third needs an index into the string that is not the exact middle.

=={{header|Icon}} and {{header|Unicon}}==

The fact that success and failure of expression evaluation is an integral part
of both languages means that one may want to test expressions for success or
for failure.  There is no standard framework, but the following example shows
how these tests might be handled.

```unicon
procedure main()
    s := "ablewasiereisawelba"
    assert{"test1",palindrome(s)}
    assertFailure{"test2",palindrome(s)}
    s := "un"||s
    assert{"test3",palindrome(s)}
    assertFailure{"test4",palindrome(s)}
end

procedure palindrome(s)
    return s == reverse(s)
end

procedure assert(A)
    if not @A[2] then write(@A[1],": failed")
end

procedure assertFailure(A)
    if @A[2] then write(@A[1],": failed")
end
```


Which outputs:

```txt

->testf
test2: failed
test3: failed
->

```



## J

Using the [[J:Addons/general/unittest|general/unittest Addon]] to test the <code>isPalin0</code> verb from [[Palindrome#J|Palindrome]].

Tests are contained in a test script <tt>c:\mypath\palindrome_test.ijs</tt> with the following contents:

```j
NB. Contents of palindrome_test.ijs

NB. Basic testing
test_palinA=: monad define
  assert isPalin0 'abcba'
  assert isPalin0 'aa'
  assert isPalin0 ''
  assert -. isPalin0 'ab'
  assert -. isPalin0 'abcdba'
)

NB. Can test for expected failure instead
palinB_expect=: 'assertion failure'
test_palinB=: monad define
  assert isPalin0 'ab'
)
```


Example Usage:

```j
   require 'general/unittest'
   unittest 'c:\mypath\palindrome_test.ijs'
Test: c:\mypath\palindrome_test.ijs
palinA .................................. OK
palinB .................................. OK
```



## Java

```java5
import static ExampleClass.pali; // or from wherever it is defined
import static ExampleClass.rPali; // or from wherever it is defined
import org.junit.*;
public class PalindromeTest extends junit.framework.TestCase {
    @Before
    public void setUp(){
        //runs before each test
        //set up instance variables, network connections, etc. needed for all tests
    }
    @After
    public void tearDown(){
        //runs after each test
        //clean up instance variables (close files, network connections, etc.).
    }

    /**
     * Test the pali(...) method.
     */
    @Test
    public void testNonrecursivePali() throws Exception {
        assertTrue(pali("abcba"));
        assertTrue(pali("aa"));
        assertTrue(pali("a"));
        assertTrue(pali(""));
        assertFalse(pali("ab"));
        assertFalse(pali("abcdba"));
    }
    /**
     * Test the rPali(...) method.
     */
    @Test
    public void testRecursivePali() throws Exception {
        assertTrue(rPali("abcba"));
        assertTrue(rPali("aa"));
        assertTrue(rPali("a"));
        assertTrue(rPali(""));
        assertFalse(rPali("ab"));
        assertFalse(rPali("abcdba"));
    }

    /**
     * Expect a WhateverExcpetion
     */
    @Test(expected=WhateverException.class)
    public void except(){
        //some code that should throw a WhateverException
    }
}
```

Most [[IDE]]s that support Java will have JUnit built in or will have an easy-to-use plugin for it. For those that don't use these IDEs, test classes can be run from a normal main method and their results will print to standard output:

```java5
public class RunTests{
  public static main(String[] args){
    org.junit.runner.JUnitCore.runClasses(PalindromeTest.class/*, other classes here if you have more*/);
  }
}
```




## JavaScript


### ES6


```javascript
const assert = require('assert');

describe('palindrome', () => {
  const pali = require('../lib/palindrome');

  describe('.check()', () => {
    it('should return true on encountering a palindrome', () => {
      assert.ok(pali.check('racecar'));
      assert.ok(pali.check('abcba'));
      assert.ok(pali.check('aa'));
      assert.ok(pali.check('a'));
    });

    it('should return true on encountering an empty string', () => {
      assert.ok(pali.check(''));
    });

    it('should return false on encountering a non-palindrome', () => {
      assert.ok(!pali.check('alice'));
      assert.ok(!pali.check('ab'));
      assert.ok(!pali.check('abcdba'));
    });
  })
});
```


Output:

```txt
$ ls -R
.:
lib/  test/

./lib:
palindrome.js

./test:
test.js
$ mocha --harmony

  palindrome
    .check()
      ✓ should return true on encountering a palindrome
      ✓ should return true on encountering an empty string
      ✓ should return false on encountering a non-palindrome


  3 passing (18ms)

$

```



## jq


The jq command has an option (--run-tests) for running functional tests.

Each test case is presented on STDIN in the form of a triplet of adjacent lines as follows:
# a jq expression on one line (possibly including a trailing comment)
# input on one line
# expected output in "compressed" form (one or more lines)

Comment-lines (lines beginning with #) and blank lines may be inserted between triplets.

A test case can include jq function definitions, but each test case is executed in isolation.

Here is an example of a file with four test case triplets:

```sh
# Test case 1:
.
1
1

# Test case 2:
1+1
null
2

# Test case 3 (with the wrong result):
1+1
null
0

# A test case with a function definition:
def factorial: if . <= 0 then 1 else . * ((. - 1) | factorial) end; factorial
3
6
```


If the file is named, say, test.txt, then the tests can be run by executing: jq --run-tests < test.txt

jq 1.4 produces very verbose output because an execution trace is included. In this article, only the key output lines are shown.  The output that results from running the four test cases above is, in abbreviated form, as follows:

```sh
$ jq --run-tests < jq.tests

Testing '.' at line number 3
Testing '1+1' at line number 8
Testing '1+1' at line number 13
*** Expected 0, but got 2 for test at line number 15: 1+1
Testing 'def factorial: if . <= 0 then 1 else . * ((. - 1) | factorial) end; factorial' at line number 18
3 of 4 tests passed (0 malformed)

```


### Testing jq Libraries


For tests of jq libraries, the "import" command can be used if your jq supports it.
(The import command is not available in jq 1.4.)

For example, suppose the file library.jq contains the following definitions:


```jq
def factorial: if . <= 0 then 1 else . * ((. - 1) | factorial) end;

def palindrome: explode as $in | ($in|reverse) == $in;
```


and that the file test-library.txt contains the two test triplets:


```sh
import "library" as lib; lib::factorial
3
6

import "library" as lib; lib::palindrome
"salàlas"
true
```


Then the tests can be run by invoking jq in the usual way:

```sh
jq --run-tests < test-library.txt
```



## Jsish


Jsi includes unit testing in the implementation of '''jsish'''. Jsi encourages unit testing.

Given the [[Palindrome detection]] solution of


```javascript
/* Palindrome detection, in Jsish */
function isPalindrome(str:string, exact:boolean=true) {
  if (!exact) {
      str = str.toLowerCase().replace(/[^a-z0-9]/g, '');
  }
  return str === str.split('').reverse().join('');
}
```


''jsish'' allows adding ''echo mode'' lines, (which are lines with a semi-colon ; in column 1 followed by an expression, with a closing semi-colon)

For example:


```txt
;isPalindrome('BUB');
;isPalidrome('CUB');
```


These lines are echoed in a special format of '''expression ==> result''' when turned up.


```txt
prompt$ jsish --U palindrome.jsi
isPalindrome('BUB') ==> true
isPalindrome('CUB') ==> false
```


Ok, looks good so far.  ''jsish'' also has a run unit tests mode, <code>-u</code>.  Along with running basic "did the program crash", -u also handles special comment sections for expectations.


```javascript
/* Palindrome detection, in Jsish */
function isPalindrome(str:string, exact:boolean=true) {
  if (!exact) {
      str = str.toLowerCase().replace(/[^a-z0-9]/g, '');
  }
  return str === str.split('').reverse().join('');
}

;isPalindrome('BUB');
;isPalindrome('CUB');

/*
=!EXPECTSTART!=
isPalindrome('BUB') ==> true
isPalindrome('CUB') ==> false
=!EXPECTEND!=
*/
```


Giving


```txt
prompt$ jsish -u palindrome.jsi
[PASS] palindrome.jsi
```


Tests pass.  The echo lines are captured, compared, and on failure will show a diff fragment.

Changing the expectation of CUB to true, gives


```txt
prompt$ jsish palindrome.jsi
[FAIL] palindrome.jsi
at line 2 of output:
        output: <isPalindrome('CUB') ==> false>
        expect: <isPalindrome('CUB') ==> true>

### ==============
DIFFSTART
 isPalindrome('BUB') ==> true
-isPalindrome('CUB') ==> true
+isPalindrome('CUB') ==> false


### ==============
DIFFEND
```


CUB is not an exact palindrome.  Putting that back to pass again, and adding a few other tests, in particular for exercising the ''exact palindrome'' flag that ignores case and punctuation when set to false.  (A false palindrome so to speak).


```javascript
/* Palindrome detection, in Jsish */
function isPalindrome(str:string, exact:boolean=true) {
  if (!exact) {
      str = str.toLowerCase().replace(/[^a-z0-9]/g, '');
  }
  return str === str.split('').reverse().join('');
}

;isPalindrome('BUB');
;isPalindrome('CUB');
;isPalindrome('Bub');
;isPalindrome('Bub', false);
;isPalindrome('Never odd or even', false);
;isPalindrome('In girum imus nocte et consumimur igni', false);
;isPalindrome('A man, a plan, a canal; Panama!', false);
;isPalindrome('A man, a plan, a canal; Panama!', true);

/*
=!EXPECTSTART!=
isPalindrome('BUB') ==> true
isPalindrome('CUB') ==> false
=!EXPECTEND!=
*/
```


That's all good, but after looking at --U output, it'll mean an extra edit to the file to add the expectations.  ''jsish'' to the rescue, and automatically updating the expectation block:


```txt
prompt$ jsish -u -update true palindrom.jsi
Updated palindrome.jsi
```


Which now looks like


```javascript
/* Palindrome detection, in Jsish */
function isPalindrome(str:string, exact:boolean=true) {
  if (!exact) {
      str = str.toLowerCase().replace(/[^a-z0-9]/g, '');
  }
  return str === str.split('').reverse().join('');
}

;isPalindrome('BUB');
;isPalindrome('CUB');
;isPalindrome('Bub');
;isPalindrome('Bub', false);
;isPalindrome('Never odd or even', false);
;isPalindrome('In girum imus nocte et consumimur igni', false);
;isPalindrome('A man, a plan, a canal; Panama!', false);
;isPalindrome('A man, a plan, a canal; Panama!', true);

/*
=!EXPECTSTART!=
isPalindrome('BUB') ==> true
isPalindrome('CUB') ==> false
isPalindrome('Bub') ==> false
isPalindrome('Bub', false) ==> true
isPalindrome('Never odd or even', false) ==> true
isPalindrome('In girum imus nocte et consumimur igni', false) ==> true
isPalindrome('A man, a plan, a canal; Panama!', false) ==> true
isPalindrome('A man, a plan, a canal; Panama!', true) ==> false
=!EXPECTEND!=
*/
```


Easy peasy.  ''Maybe too easy'', auto update of unit tests should not be run until you know you have valid tests and known results.  If there is a bug in the expression, <code>-u -update true</code> will gladly update a script with invalid results.  The <code>-update true</code> feature should be treated as a helper, not a "turn off brain now" crutch.  Very handy on initial create, or when messaging changes in code under test, but to be treated with respect and care.

Echo lines are not the only thing captured by <code>jsish -u</code> mode.  All outputs are captured and can be compared in the EXPECT block.  It even allows for sample input testing.


```javascript
/* Interaction testing */
var trial = console.input();
puts(trial.replace(/a/g, 'b'));

/*
=!INPUTSTART!=
abccba
=!INPUTEND!=
*/

/*
=!EXPECTSTART!=
bbccbb
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u test-inputs.jsi
[PASS] test-inputs.jsi

prompt$ jsish test-inputs.jsi
abc from user
bbc from user
```


When in unit testing, no user input was required, the <code>console.input()</code> method was fed data from the INPUTSTART block.
When not in unit testing mode, code just runs as expected, and data was accepted from the keyboard.

You can ship ''jsish'' scripts with tests in and they will be ignored during normal operation. If that seems appropriate for purpose and not a max-speed production deployment.

There is more support for unit testing in Jsi. <code>assert(expression)</code> is ignored by default, and turned on during test mode for instance.  Entire directories can be evaluated in test mode, by naming a directory instead of a filename.

See https://jsish.org/doc/Testing.html for more details.


## Julia

```julia
using Base.Test
include("Palindrome_detection.jl")

# Simple test
@test palindrome("abcdcba")
@test !palindrome("abd")

# Test sets
@testset "palindromes" begin
    @test palindrome("aaaaa")
    @test palindrome("abcba")
    @test palindrome("1")
    @test palindrome("12321")
end

@testset "non-palindromes" begin
    @test !palindrome("abc")
    @test !palindrome("a11")
    @test !palindrome("012")
end
```


```txt
Test Summary: | Pass  Total
palindromes   |    4      4
Test Summary:   | Pass  Total
non-palindromes |    3      3
```



## Kotlin

Kotlin can use various JVM testing frameworks including its own kotlin-test module. However, for simple cases, it is easier to use the 'assert' function built into its standard library which will throw an AssertionError if the condition is false and assertions are enabled using java's -ea option when the application is run:

```scala
// version 1.1.3

fun isPalindrome(s: String) = (s == s.reversed())

fun main(args: Array<String>) {
    val testCases = listOf("racecar", "alice", "eertree", "david")
    for (testCase in testCases) {
        try {
            assert(isPalindrome(testCase)) { "$testCase is not a palindrome" }
        }
        catch (ae: AssertionError) {
            println(ae.message)
        }
    }
}
```


```txt

alice is not a palindrome
david is not a palindrome

```



## Lasso

The following example uses the [https://bitbucket.org/bfad/lspec/ LSpec Library]:


```lasso
// Taken from the Lasso entry in Palindrome page
define isPalindrome(text::string) => {

    local(_text = string(#text)) // need to make copy to get rid of reference issues

    #_text -> replace(regexp(`(?:$|\W)+`), -ignorecase)

    local(reversed = string(#_text))
    #reversed -> reverse

    return #_text == #reversed
}

// The tests
describe(::isPalindrome) => {
    it(`throws an error when not passed a string`) => {
        expect->error =>{
            isPalindrome(43)
        }
    }

    it(`returns true if the string is the same forward and backwords`) => {
        expect(isPalindrome('abba'))
    }

    it(`returns false if the string is different forward and backwords`) => {
        expect(not isPalindrome('aab'))
    }

    it(`ignores spaces and punctuation`) => {
        expect(isPalindrome(`Madam, I'm Adam`))
    }
}

// Run the tests and get the summary
// (This normally isn't in the code as the test suite is run via command-line.)
lspec->stop
```


```txt
....

Finished in 0.157030 seconds
4 tests, 0 failures
```



## Lua


```lua
assert( ispalindrome("ABCBA") )
assert( ispalindrome("ABCDE") )
```



## Mathematica


```Mathematica
myFun[x_] := Block[{y},y = x^2; Assert[y > 5]; Sin[y]]
On[Assert];myFun[1.0]
```


Output:


```txt
Assert::asrtf: Assertion y>5 failed.
0.841471
```



## NetRexx

```NetRExx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

import junit.framework.TestCase
import RCPalindrome

class RCTestAFunction public final extends TestCase

method setUp public
  return

method tearDown public
  return

method testIsPal public signals AssertionError

  assertTrue(RCPalindrome.isPal(Rexx 'abcba'))
  assertTrue(RCPalindrome.isPal(Rexx 'aa'))
  assertTrue(RCPalindrome.isPal(Rexx 'a'))
  assertTrue(RCPalindrome.isPal(Rexx ''))
  assertFalse(RCPalindrome.isPal(Rexx 'ab'))
  assertFalse(RCPalindrome.isPal(Rexx 'abcdba'))

  return

method except signals RuntimeException
  signal RuntimeException()

method main(args = String[]) public constant

  testResult = org.junit.runner.JUnitCore.runClasses([RCTestAFunction.class])

  secs = Rexx testResult.getRunTime / 1000.0

  if testResult.wasSuccessful then say 'Tests successful'
                              else say 'Tests failed'
  say '  failure count:' testResult.getFailureCount
  say '   ignore count:' testResult.getIgnoreCount
  say '      run count:' testResult.getRunCount
  say '       run time:' secs.format(null, 3)

  return

```

;Output

```txt

Tests successful
  failure count: 0
   ignore count: 0
      run count: 1
       run time: 0.015

```



## Nim

Using assertions (No output means all tests were correct, only works with debug builds!):

```nim
proc reverse(s): string =
  result = newString(s.len)
  for i,c in s:
    result[s.high - i] = c

proc isPalindrome(s): bool =
  s == reverse(s)

when isMainModule:
  assert(isPalindrome(""))
  assert(isPalindrome("a"))
  assert(isPalindrome("aa"))
  assert(not isPalindrome("baa"))
  assert(isPalindrome("baab"))
  assert(isPalindrome("ba_ab"))
  assert(not isPalindrome("ba_ ab"))
  assert(isPalindrome("ba _ ab"))
  assert(not isPalindrome("abab"))
```


Using the unittest module:

```nim
import unittest

proc reverse(s): string =
  result = newString(s.len)
  for i,c in s:
    result[s.high - i] = c

proc isPalindrome(s): bool =
  s == reverse(s)

when isMainModule:
  suite "palindrome":
    test "empty string":
      check isPalindrome ""

    test "string of length 1":
      check isPalindrome "a"

    test "string of length 2":
      check isPalindrome "aa"

    test "string of length 3":
      check isPalindrome "aaa"

    test "no palindrome":
      check isPalindrome("foo") == false
```

Output:

```txt
[OK] empty string

[OK] string of length 1

[OK] string of length 2

[OK] string of length 3

[OK] no palindrome
```



## OCaml


Using the library [http://www.xs4all.nl/~mmzeeman/ocaml/ OUnit].

The module '''Palindrome''' is where are compiled the two functions ''is_palindrome'' and ''rem_space'' from [[Palindrome#OCaml|this page]]. We put these two functions in a file named ''palindrome.ml'' and compile it with ''ocamlc -c palindrome.ml -o palindrome.cmo'', then with the code below in the file ''palindrome_tests.ml'' we execute the tests with this command line:
 ocaml unix.cma -I +oUnit oUnit.cma  palindrome.cmo  palindrome_tests.ml


```ocaml
open OUnit
open Palindrome

let test_palindrome_1 _ =
  assert_equal true (is_palindrome "aba")

let test_palindrome_2 _ =
  assert_equal true (is_palindrome "abba")

let test_palindrome_3 _ =
  assert_equal true (is_palindrome "abacidAdicaba")

let test_palindrome_4 _ =
  assert_equal false (is_palindrome "xREty5kgPMO")

let test_palindrome_5 _ =
  assert_equal true (is_palindrome(rem_space "in girum imus nocte et consumimur igni"))


let suite = "Test Palindrome" >::: ["test_palindrome_1" >:: test_palindrome_1;
                                    "test_palindrome_2" >:: test_palindrome_2;
                                    "test_palindrome_3" >:: test_palindrome_3;
                                    "test_palindrome_4" >:: test_palindrome_4;
                                    "test_palindrome_5" >:: test_palindrome_5]
let _ =
  run_test_tt_main suite
```





## Oforth

Unit tests are a built-in functionality. If Oforth is run using --t option, all tests are checked. Otherwise, tests are not checked :

```Oforth
test: [ "abcd" isPalindrome ]
test: ["abba" isPalindrome ]
test: [ "abcba" isPalindrome ]
```



## PARI/GP

PARI/GP comes with a testing framework for gp.  Testing the palindrome function (if converted with gp2c and added to PARI) would consist of adding lines like

```txt
? ispal("abc")
0
? ispal("aba")
1
```



## Pascal

See [[Test_a_function#Delphi | Delphi]]


## Perl

A test file, with a ''.t'' suffix, is just a Perl program that prints a test stream in [http://testanything.org/wiki/index.php/Main_Page Test Anything Protocol] (TAP). There are modules to help print things in the correct TAP format; Perl 5 bundles Test, Test::Simple and Test::More.

This example uses Test and requires the ''Palindrome.pm'' file from [[Palindrome detection#Perl]].

```perl
# ptest.t
use strict;
use warnings;

use Test;

my %tests;
BEGIN {
    # plan tests before loading Palindrome.pm
    %tests = (
        'A man, a plan, a canal: Panama.'           => 1,
        'My dog has fleas'                          => 0,
        "Madam, I'm Adam."                          => 1,
        '1 on 1'                                    => 0,
        'In girum imus nocte et consumimur igni'    => 1,
        ''                                          => 1,
    );

    # plan 4 tests per string
    plan tests => (keys(%tests) * 4);
}

use Palindrome;

for my $key (keys %tests) {
    $_ = lc $key;  # convert to lowercase
    s/[\W_]//g;    # keep only alphanumeric characters

    my $expect = $tests{$key};
    my $note = ("\"$key\" should " . ($expect ? '' : 'not ') .
                "be a palindrome.");

    ok palindrome == $expect, 1, "palindrome: $note";
    ok palindrome_c == $expect, 1, "palindrome_c: $note";
    ok palindrome_r == $expect, 1, "palindrome_r: $note";
    ok palindrome_e == $expect, 1, "palindrome_e: $note";
}
```


The program produces TAP output.


```txt
$ perl ptest.t
1..24
# Running under perl version 5.010001 for openbsd
# Current time local: Mon Jan 31 17:44:06 2011
# Current time GMT:   Mon Jan 31 22:44:06 2011
# Using Test.pm version 1.25_02
ok 1
ok 2
ok 3
...
ok 24
```


The first line '1..24' plans for 24 tests, so that one can detect if the script aborted itself before it finished all the tests. A line like 'ok 1' is a passing test, 'not ok 1' is a failing test.

To find those 'not ok' lines, one can run a TAP harness. The "prove" program is a TAP harness that comes with Perl. (By default, "prove" runs all the test files in the "t" subdirectory. I did not make "t", so I will run "prove ptest.t" instead.)


```txt
$ prove ptest.t
ptest.t .. ok
All tests successful.
Files=1, Tests=24,  0 wallclock secs ( 0.00 usr  0.02 sys +  0.01 cusr  0.00 csys =  0.03 CPU)
Result: PASS
```


All the tests passed! But suppose that palindrome_e() returned the wrong answer for the empty string "", then a test would fail.


```txt
$ perl ptest.t
...
ok 3
not ok 4
# Test 4 got: "" (ptest.t at line 36)
#   Expected: "1" (palindrome_e: "" should be a palindrome.)
#  ptest.t line 36 is:     ok palindrome_e == $expect, 1, "palindrome_e: $note";
ok 5
...

```



```txt
$ prove ptest.t
ptest.t .. 1/24 # Test 4 got: "" (ptest.t at line 36)
#   Expected: "1" (palindrome_e: "" should be a palindrome.)
#  ptest.t line 36 is:     ok palindrome_e == $expect, 1, "palindrome_e: $note";
ptest.t .. Failed 1/24 subtests

Test Summary Report
-------------------
ptest.t (Wstat: 0 Tests: 24 Failed: 1)
  Failed test:  4
Files=1, Tests=24,  0 wallclock secs ( 0.02 usr  0.01 sys +  0.02 cusr  0.00 csys =  0.05 CPU)
Result: FAIL
```



## Perl 6


```perl6
use Test;

sub palin( Str $string) { so $string.lc.comb(/\w/) eq  $string.flip.lc.comb(/\w/) }

my %tests =
    'A man, a plan, a canal: Panama.'           => True,
    'My dog has fleas'                          => False,
    "Madam, I'm Adam."                          => True,
    '1 on 1'                                    => False,
    'In girum imus nocte et consumimur igni'    => True,
    ''                                          => True,
    ;

plan %tests.elems;

for %tests.kv -> $test, $expected-result {
    is palin($test), $expected-result,
        "\"$test\" is {$expected-result??''!!'not '}a palindrome.";
}
```


Output:

```txt
1..6
ok 1 - "1 on 1" is not a palindrome.
ok 2 - "My dog has fleas" is not a palindrome.
ok 3 - "A man, a plan, a canal: Panama." is a palindrome.
ok 4 - "" is a palindrome.
ok 5 - "Madam, I'm Adam." is a palindrome.
ok 6 - "In girum imus nocte et consumimur igni" is a palindrome.
```



## PicoLisp

The '[http://software-lab.de/doc/refT.html#test test]' function is
built into PicoLisp.

```PicoLisp
(de palindrome? (S)
   (= (setq S (chop S)) (reverse S)) )

(test T (palindrome? "racecar"))
(test NIL (palindrome? "ferrari"))
```



## Prolog

SWI-Prolog has an inbuilt unit test functionality which is run automatically when building.

It can also be run by using the run_tests predicate.


```Prolog
palindrome(Word) :- name(Word,List), reverse(List,List).

:- begin_tests(palindrome).

test(valid_palindrome) :- palindrome('ingirumimusnocteetconsumimurigni').
test(invalid_palindrome, [fail]) :- palindrome('this is not a palindrome').

:- end_tests(palindrome).
```




## PureBasic

PureBasic allows for definition of Assert() and other tools & the debugger is integrated into the native editor.

```PureBasic
Macro DoubleQuote
  ; Needed for the Assert-Macro below
  "                             ; " second dlbquote to prevent Rosettas misshighlighting of following code. Remove comment before execution!
EndMacro
Macro Assert(TEST,MSG="")
  CompilerIf #PB_Compiler_Debugger
    If Not (TEST)
      If MSG<>"": Debug MSG: EndIf
      Temp$="Fail: "+DoubleQuote#TEST#DoubleQuote
      Debug Temp$+", Line="+Str(#PB_Compiler_Line)+" in "+#PB_Compiler_File
      CallDebugger
    EndIf
  CompilerEndIf
EndMacro

Procedure IsPalindrome(StringToTest.s)
  If StringToTest=ReverseString(StringToTest)
    ProcedureReturn 1
  Else
    ProcedureReturn 0
  EndIf
EndProcedure

text1$="racecar"
text2$="wisconsin"
Assert(IsPalindrome(text1$), "Catching this would be a fail")
Assert(IsPalindrome(text2$), "Catching this is correct")
```




## Python

This uses the [[wp:doctest|doctest]] module from the Python standard library. This allows copies of tests run in an interactive session to be re-used as tests.


```python
def is_palindrome(s):
    '''
        >>> is_palindrome('')
        True
        >>> is_palindrome('a')
        True
        >>> is_palindrome('aa')
        True
        >>> is_palindrome('baa')
        False
        >>> is_palindrome('baab')
        True
        >>> is_palindrome('ba_ab')
        True
        >>> is_palindrome('ba_ ab')
        False
        >>> is_palindrome('ba _ ab')
        True
        >>> is_palindrome('ab'*2)
        False
        >>> x = 'ab' *2**15
        >>> len(x)
        65536
        >>> xreversed = x[::-1]
        >>> is_palindrome(x+xreversed)
        True
        >>> len(x+xreversed)
        131072
        >>>
    '''
    return s == s[::-1]

def _test():
    import doctest
    doctest.testmod()
    #doctest.testmod(verbose=True)

if __name__ == "__main__":
    _test()
```

When run in the form as shown above there is no output as all tests pass. If the alternative doctest.testmod line is used with verbose=True, then the following output is produced:


```txt
Trying:
    is_palindrome('')
Expecting:
    True
ok
Trying:
    is_palindrome('a')
Expecting:
    True
ok
Trying:
    is_palindrome('aa')
Expecting:
    True
ok
Trying:
    is_palindrome('baa')
Expecting:
    False
ok
Trying:
    is_palindrome('baab')
Expecting:
    True
ok
Trying:
    is_palindrome('ba_ab')
Expecting:
    True
ok
Trying:
    is_palindrome('ba_ ab')
Expecting:
    False
ok
Trying:
    is_palindrome('ba _ ab')
Expecting:
    True
ok
Trying:
    is_palindrome('ab'*2)
Expecting:
    False
ok
Trying:
    x = 'ab' *2**15
Expecting nothing
ok
Trying:
    len(x)
Expecting:
    65536
ok
Trying:
    xreversed = x[::-1]
Expecting nothing
ok
Trying:
    is_palindrome(x+xreversed)
Expecting:
    True
ok
Trying:
    len(x+xreversed)
Expecting:
    131072
ok
2 items had no tests:
    __main__
    __main__._test
1 items passed all tests:
  14 tests in __main__.is_palindrome
14 tests in 3 items.
14 passed and 0 failed.
Test passed.
```



## R

See also the functions defineTestSuite and runTestSuite.

```r
checkTrue(palindroc("aba"))  # TRUE
checkTrue(!palindroc("ab"))  # TRUE
checkException(palindroc())  # TRUE
checkTrue(palindroc(""))     # Error.  Uh-oh, there's a bug in the function
```



## Racket


Racket has a built-in unit testing library. Tests can be specified next to function implementations or in a testing submodule.


```racket

#lang racket
(module+ test (require rackunit))

;; from the Palindrome entry
(define (palindromb str)
  (let* ([lst (string->list (string-downcase str))]
         [slst (remove* '(#\space) lst)])
    (string=? (list->string (reverse slst)) (list->string slst))))

;; this test module is not loaded unless it is
;; specifically requested for testing, allowing internal
;; unit test specification
(module+ test
  (check-true (palindromb "racecar"))
  (check-true (palindromb "avoova"))
  (check-false (palindromb "potato")))

```



## Retro

Retro includes a library for creating automated tests. This is used for checking the standard libraries shipped with Retro.


```Retro
needs assertion'
needs hash'

: palindrome? ( $-f ) dup ^hash'hash [ ^strings'reverse ^hash'hash ] dip = ;

with assertion'
: t0 ( - ) "hello" palindrome? 0 assert=  ; assertion
: t1 ( - ) "ingirumimusnocteetconsumimurigni" palindrome? -1 assert=  ; assertion
: test ( - ) t0 t1 ;
test
```



## REXX

There is no official suite of programs, but the writers of the various REXX interpreters each have their own.


Listed below are two such examples that are used to "stress" the interpreter.
===stress REXX keywords (used as variables)===
{This was originally written in some form of FORTRAN.}

```rexx
/* This REXX uses a lot of REXX keywords as variables. */

signal=(interpret=value);value=(interpret<parse);do upper=value to value
end;exit=upper*upper*upper*upper-value-upper;say=' ';return=say say say;
with.=signal;do then=value to exit;pull='';do otherwise=upper to then-,
value;select=otherwise-value;if.otherwise=with.otherwise+with.select;end
if.value=value;if.then=value;do otherwise=value to exit-then;pull=pull,
say''say;end;do otherwise=value to then;pull=pull center(if.otherwise,,
length(return));end;say pull;do otherwise=value to exit;with.otherwise=,
if.otherwise;end;end
```

'''output'''

```txt

                                       1
                                    1     1
                                 1     2     1
                              1     3     3     1
                           1     4     6     4     1
                        1     5    10    10     5     1
                     1     6    15    20    15     6     1
                  1     7    21    35    35    21     7     1
               1     8    28    56    70    56    28     8     1
            1     9    36    84    126   126   84    36     9     1
         1    10    45    120   210   252   210   120   45    10     1
      1    11    55    165   330   462   462   330   165   55    11     1
   1    12    66    220   495   792   924   792   495   220   66    12     1

```



### stress test some REXX BIFs

This stress tests some of the REXX built-in functions (BIFs).

```rexx
/*REXX program to show a secret message.  */
z.=' ';z=12-25-2002;y=z;w=-y
z.0=translate(right(time('c'),substr(z,4,z==y)))
z.1=left(substr(format(z,2,z==y,,z==y.1),5),z==y)
z.2=copies(right(symbol('z.'20),z==y),left(w,1))
z.3=translate(right(date('w'),z==y))
z.5=right(form(),z==y)
z.6=x2c(d2x(x2d(c2x(substr(symbol(substr(z,2)),2,z==y)))-1))
z.7=right(symbol('z.'||(z\==z)),z==y)
z.8=substr(form(),(z==y)+left(w,1),z==y)
z.9=reverse(left(form(),z==y))
z.10=left(substr(form(),6),z==y)
z.11=right(datatype(z),z==y)
z.12=substr(symbol(left(z,z=z)),left(w,1),z==y)
z.13=left(form(),z==y)
  do z=-31 to 31;z.32=z.32||z.z;end
say
say z.32
say
```

'''output'''

```txt


                                MERRY CHRISTMAS


```

Because the REXX language is interpreted, many REXX stress tests are "simple" programs like those listed above, albeit highly obfuscated.





## Ring


```ring

  assert(IsPalindrome("racecar"))
  assert(IsPalindrome("alice"))

```



## Ruby



### test/unit

Ruby comes with a unit testing package. All you have to do is to create a subclass of Test::Unit::Testcase that contains methods that begin with "test_".  The package will create a test suite and run it for you.


```ruby
def palindrome?(s)
  s == s.reverse
end

require 'test/unit'
class MyTests < Test::Unit::TestCase
  def test_palindrome_ok
    assert(palindrome? "aba")
  end

  def test_palindrome_nok
    assert_equal(false, palindrome?("ab"))
  end

  def test_object_without_reverse
    assert_raise(NoMethodError) {palindrome? 42}
  end

  def test_wrong_number_args
    assert_raise(ArgumentError) {palindrome? "a", "b"}
  end

  def test_show_failing_test
    assert(palindrome?("ab"), "this test case fails on purpose")
  end
end
```



```txt
$ ruby palindrome.rb
Loaded suite palindrome
Started
...F.
Finished in 0.018 seconds.

  1) Failure:
test_show_failing_test(MyTests) [palindrome.rb:24]:
this test case fails on purpose.
<false> is not true.

5 tests, 5 assertions, 1 failures, 0 errors
```



### minitest

Many Ruby hackers have switched from 'test/unit' to other testing libraries. Some of these libraries provide the 'describe' block, which is just a pretty way to make a test case. [http://rspec.info/ RSpec], [https://github.com/chneukirchen/bacon Bacon] and [http://bfts.rubyforge.org/minitest/ minitest] are such libraries.

This example uses Minitest, which comes with Ruby 1.9. (But if you have Ruby 1.8, then you can still install Minitest as a gem, using [[:Category:RubyGems|RubyGems]].)


```ruby
# palindrome.rb
def palindrome?(s)
  s == s.reverse
end

require 'minitest/spec'
require 'minitest/autorun'
describe "palindrome? function" do
  it "returns true if arg is a palindrome" do
    (palindrome? "aba").must_equal true
  end

  it "returns false if arg is not a palindrome" do
    palindrome?("ab").must_equal false
  end

  it "raises NoMethodError if arg is without #reverse" do
    proc { palindrome? 42 }.must_raise NoMethodError
  end

  it "raises ArgumentError if wrong number of args" do
    proc { palindrome? "a", "b" }.must_raise ArgumentError
  end

  it "passes a failing test" do
    palindrome?("ab").must_equal true, "this test case fails on purpose"
  end
end
```



```txt
$ ruby19 palindrome.rb
Loaded suite palindrome
Started
..F..
Finished in 0.000629 seconds.

  1) Failure:
test_0005_passes_a_failing_test(PalindromeFunctionSpec) [palindrome.rb:26]:
this test case fails on purpose.
Expected false, not true.

5 tests, 5 assertions, 1 failures, 0 errors, 0 skips

Test run options: --seed 40770
```



## Scala

There are three main Scala testing libraries: ScalaCheck, ScalaTest and Specs. The first is
shown here, being similar to Haskell's QuickCheck.


```scala
import org.scalacheck._
import Prop._
import Gen._

object PalindromeCheck extends Properties("Palindrome") {
  property("A string concatenated with its reverse is a palindrome") =
    forAll { s: String => isPalindrome(s + s.reverse) }

  property("A string concatenated with any character and its reverse is a palindrome") =
    forAll { (s: String, c: Char) => isPalindrome(s + c + s.reverse) }

  property("If the first half of a string is equal to the reverse of its second half, it is a palindrome") =
    forAll { (s: String) => s.take(s.length / 2) != s.drop((s.length + 1) / 2).reverse || isPalindrome(s) }

  property("If the first half of a string is different than the reverse of its second half, it isn't a palindrome") =
    forAll { (s: String) => s.take(s.length / 2) == s.drop((s.length + 1) / 2).reverse || !isPalindrome(s) }

}
```


Output:


```txt

+ Palindrome.A string concatenated with its reverse is a palindrome: OK, pa
  ssed 100 tests.
+ Palindrome.A string concatenated with any character and its reverse is a
  palindrome: OK, passed 100 tests.
+ Palindrome.If the first half of a string is equal to the reverse of its s
  econd half, it is a palindrome: OK, passed 100 tests.
+ Palindrome.If the first half of a string is different than the reverse of
   its second half, it isn't a palindrome: OK, passed 100 tests.

```



## Scheme


SRFI 64 is a popular test library.


```scheme

(import (srfi 64))
(test-begin "palindrome-tests")
(test-assert (palindrome? "ingirumimusnocteetconsumimurigni"))
(test-assert (not (palindrome? "This is not a palindrome")))
(test-equal #t (palindrome? "ingirumimusnocteetconsumimurigni")) ; another of several test functions
(test-end)

```


The library reports the number of pass/fail tests at the end; the report may be customised.
Also, a detailed log file is created, showing the results of each test.


## SQL PL

```sql pl

CREATE OR REPLACE PROCEDURE TEST_MY_TEST()
  BEGIN
    DECLARE EXPECTED INTEGER;
    DECLARE ACTUAL INTEGER;
    CALL DB2UNIT.REGISTER_MESSAGE('My first test');
    SET EXPECTED = 2;
    SET ACTUAL = 1+1;
    CALL DB2UNIT.ASSERT_INT_EQUALS('Same value', EXPECTED, ACTUAL);
  END @

```

Output:

```txt

db2 => CALL DB2UNIT.RUN_SUITE('DB2UNIT_EXAMPLE');


  Result set 1
  --------------

  TEST             FINAL_STATE MICROSECONDS MESSAGE
  ---------------- ----------- ------------ ----------------------------------------------------------------
  Before Suite     -                      - Starting execution
  TEST_MY_TEST     Passed             29585 Executing TEST_MY_TEST
  After Suite      -                      - Finishing execution
                   -                      - 1 tests were executed
                   -                      - 1 tests passed
                   -                      - 0 tests failed
                   -                      - 0 tests with errors

  7 record(s) selected.


  Result set 2
  --------------

  TIME     EXECUTION_ID STATUS                MESSAGE
  -------- ------------ --------------------- --------------------------------------------------------------
  20:43:47        52613 Initialization        db2unit is licensed under the terms of the GPL v3
  20:43:47        52613 Initialization        Execution of DB2UNIT_EXAMPLE with ID 52613
  20:43:47        52613 Prepare Report        The reports table created: DB2UNIT_EXAMPLE.REPORT_TESTS
  20:43:48        52613 Calculating time      Total execution time is: 0 seconds

  4 record(s) selected.

  Return Status = 0

```



## Swift


```Swift
import Cocoa
import XCTest

class PalindromTests: XCTestCase {

    override func setUp() {
        super.setUp()

    }

    override func tearDown() {
        super.tearDown()
    }

    func testPalindrome() {
        // This is an example of a functional test case.
        XCTAssert(isPalindrome("abcba"), "Pass")
        XCTAssert(isPalindrome("aa"), "Pass")
        XCTAssert(isPalindrome("a"), "Pass")
        XCTAssert(isPalindrome(""), "Pass")
        XCTAssert(isPalindrome("ab"), "Pass") // Fail
        XCTAssert(isPalindrome("aa"), "Pass")
        XCTAssert(isPalindrome("abcdba"), "Pass") // Fail
    }

    func testPalindromePerformance() {
        // This is an example of a performance test case.
        self.measureBlock() {
            var _is = isPalindrome("abcba")
        }
    }
}
```



## Tcl

Testing with Tcl is just about universally performed with the <tt>tcltest</tt> package, which was originally developed for testing Tcl itself, and which is a standard part of a <tt>[[tclsh]]</tt> installation.

```tcl
package require tcltest 2
source palindrome.tcl; # Assume that this is what loads the implementation of ‘palindrome’

tcltest::test palindrome-1 {check for palindromicity} -body {
    palindrome abcdedcba
} -result 1
tcltest::test palindrome-2 {check for non-palindromicity} -body {
    palindrome abcdef
} -result 0
tcltest::test palindrome-3 {check for palindrome error} -body {
    palindrome
} -returnCodes error -result "wrong # args: should be \"palindrome s\""

tcltest::cleanupTests
```

If placed in a file called <tt>palindrome.test</tt>, the following output is produced when it is executed:

```txt
palindrome.test:	Total	3	Passed	3	Skipped	0	Failed	0
```

Note that only a small fraction of the features of the testing framework are demonstrated here. In particular, it does not show off management of conditional execution, the application of setup and cleanup code, and how these things are assembled into a whole test suite for a large system.


## UNIX Shell


```sh
#!/bin/bash

is_palindrome() {
  local s1=$1
  local s2=$(echo $1 | tr -d "[ ,!:;.'\"]" | tr '[A-Z]' '[a-z]')

  if [[ $s2 = $(echo $s2 | rev) ]]
  then
     echo "[$s1] is a palindrome"
  else
     echo "[$s1] is NOT a palindrome"
  fi
}

```


is_palindrome "A man, a plan, a canal, Panama!"
is_palindrome "Madam, I'm Adam"
is_palindrome "1 on 1"</a>


## VBA

Using the StrReverse function after deleted spaces

```vb

Option Explicit

Sub Test_a_function()
Dim a, i&
    a = Array("abba", "mom", "dennis sinned", "Un roc lamina l animal cornu", "palindrome", "ba _ ab", "racecars", "racecar", "wombat", "in girum imus nocte et consumimur igni")
    For i = 0 To UBound(a)
        Debug.Print a(i) & " is a palidrome ? " & IsPalindrome(CStr(a(i)))
    Next
End Sub

Function IsPalindrome(txt As String) As Boolean
Dim tempTxt As String
    tempTxt = LCase(Replace(txt, " ", ""))
    IsPalindrome = (tempTxt = StrReverse(tempTxt))
End Function

```

```txt
abba is a palidrome ? True
mom is a palidrome ? True
dennis sinned is a palidrome ? True
Un roc lamina l animal cornu is a palidrome ? True
palindrome is a palidrome ? False
ba _ ab is a palidrome ? True
racecars is a palidrome ? False
racecar is a palidrome ? True
wombat is a palidrome ? False
in girum imus nocte et consumimur igni is a palidrome ? True
```



## zkl


```zkl
fcn pali(text){
   if (text.len()<2) return(False);
   text==text.reverse();
}
```


```zkl
tester:=TheVault.Test.UnitTester.UnitTester(__FILE__);
   // spaces make this not a palindrome
tester.testRun(pali.fp("red rum sir is murder"), Void,False,__LINE__);
```

Need to create a closure (.fp) so the unit test is what runs the test function and can catch any errors the test function throws.
```txt


### ================== Unit Test 1 ==================

Test 1 passed!

```

A test file:

```zkl
tester:=TheVault.Test.UnitTester.UnitTester(__FILE__);
fcn pali(text){
   if (text.len()<2) return(False);
   text==text.reverse();
}
tester.testRun(pali.fp("red rum sir is murder" - " "), Void,True,__LINE__);
tester.testRun(pali.fp("red rum sir is murder"), Void,True,__LINE__); //bad test
tester.testSrc("var R=(1+2)",Void,Void,3,__LINE__); // you can test source too

tester.stats();
returnClass(tester);
```


```zkl>zkl paliTest.zkl</lang

```txt


### ===== Unit Test 1 =====paliTest.zkl==6=====

Test 1 passed!

### ===== Unit Test 2 =====paliTest.zkl==7=====

Result and expected result are different: False True
Test 2 failed. I hate it when that happens (line 7).

### ===== Unit Test 3 =====paliTest.zkl==8=====

Test 3 passed!
3 tests completed.
Passed test(s): 2 (of 3)
Failed test(s): 1, tests L(2)

```

If you had a collection of files to test:

```zkl
zkl Test.testThemAll -- paliTest.zkl
```

```txt

... as above
---------.--------.----------
---------|00:00:00|----------
-----------------------------
paliTest.zkl
3 tests completed.
Passed test(s): 2 (of 3)
Failed test(s): 1, tests L(2)

Executive summary: 1 pass in 00:00:00
   3 tests completed (1 files)
   Passed test(s): 2 (of 3)
   Failed test(s):  1
   Flawed test(s):  0
   Failed files(s): 0

That used 6 simultaneous VMs, 4 of which were threads.
Number of VMs consumed: 30

```


