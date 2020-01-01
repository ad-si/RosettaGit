+++
title = "Man or boy test"
description = ""
date = 2019-10-18T11:26:38Z
aliases = []
[extra]
id = 2426
[taxonomies]
categories = []
tags = []
+++

{{wikipedia}}
{{task|Classic CS problems and programs}}
[[Category:Recursion]]



'''Background''': The '''man or boy test''' was proposed by computer scientist [[wp:Donald_Knuth|Donald Knuth]] as a means of evaluating implementations of the [[:Category:ALGOL 60|ALGOL 60]] programming language. The aim of the test was to distinguish compilers that correctly implemented "recursion and non-local references" from those that did not.

<blockquote style="font-style:italic">
  I have written the following simple routine, which may separate the 'man-compilers' from the 'boy-compilers'
 &mdash; <span style="font-style:normal">Donald Knuth</span></blockquote>

'''Task''': Imitate [[#ALGOL 60 - Knuth's example|Knuth's example in Algol 60]] in another language, as far as possible.

'''Details''': Local variables of routines are often kept in [http://c2.com/cgi/wiki?ActivationRecord ''activation records''] (also ''call frames''). In many languages, these records are kept on a [[System stack|call stack]]. In Algol (and e.g. in [[Smalltalk]]), they are allocated on a [[heap]] instead. Hence it is possible to pass references to routines that still can use and update variables from their call environment, even if the routine where those variables are declared already returned. This difference in implementations is sometimes called the [[wp:Funarg_problem|Funarg Problem]].

In Knuth's example, each call to ''A'' allocates an activation record for the variable ''A''. When ''B'' is called from ''A'', any access to ''k'' now refers to this activation record. Now ''B'' in turn calls ''A'', but passes itself as an argument. This argument remains bound to the activation record. This call to ''A'' also "shifts" the variables ''x<sub>i</sub>'' by one place, so eventually the argument ''B'' (still bound to its particular
activation record) will appear as ''x4'' or ''x5'' in a call to ''A''. If this happens when the expression ''x4 + x5'' is evaluated, then this will again call ''B'', which in turn will update ''k'' in the activation record it was originally bound to. As this activation record is shared with other instances of calls to ''A'' and ''B'', it will influence the whole computation.

So all the example does is to set up a convoluted calling structure, where updates to ''k'' can influence the behavior
in completely different parts of the call tree.

Knuth used this to test the correctness of the compiler, but one can of course also use it to test that other languages can emulate the Algol behavior correctly. If the handling of activation records is correct, the computed value will be &minus;67.

'''Performance and Memory''': Man or Boy is intense and can be pushed to challenge any machine. Memory (both stack and heap) not CPU time is the constraining resource as the recursion creates a proliferation activation records which will quickly exhaust memory and present itself through a stack error.  Each language may have ways of adjusting the amount of memory or increasing the recursion depth. Optionally, show how you would make such adjustments.

The table below shows the result, call depths, and total calls for a range of ''k'':
{| class="wikitable" style="font-size: 85%"
! ''k''
! 0
! 1
! 2
! 3
! 4
! 5
! 6
! 7
! 8
! 9
! 10
! 11
! 12
! 13
! 14
! 15
! 16
! 17
! 18
! 19
! 20
! 21
! 22
! 23
! 24
! 25
! 26
! 27
! 28
! 29
! 30
|-
! ''A''
|align="right"| 1
|align="right"| 0
|align="right"| -2
|align="right"| 0
|align="right"| 1
|align="right"| 0
|align="right"| 1
|align="right"| -1
|align="right"| -10
|align="right"| -30
|align="right"| -67
|align="right"| -138
|align="right"| -291
|align="right"| -642
|align="right"| -1,446
|align="right"| -3,250
|align="right"| -7,244
|align="right"| -16,065
|align="right"| -35,601
|align="right"| -78,985
|align="right"| -175,416
|align="right"| -389,695
|align="right"| -865,609
|align="right"| -1,922,362
|align="right"| -4,268,854
|align="right"| -9,479,595
|align="right"| -21,051,458
|align="right"| -46,750,171
|align="right"| -103,821,058
|align="right"| -230,560,902
|align="right"| -512,016,658
|-
! ''A'' called
|align="right"| 1
|align="right"| 2
|align="right"| 3
|align="right"| 4
|align="right"| 8
|align="right"| 18
|align="right"| 38
|align="right"| 80
|align="right"| 167
|align="right"| 347
|align="right"| 722
|align="right"| 1,509
|align="right"| 3,168
|align="right"| 6,673
|align="right"| 14,091
|align="right"| 29,825
|align="right"| 63,287
|align="right"| 134,652
|align="right"| 287,264
|align="right"| 614,442
|align="right"| 1,317,533
|align="right"| 2,831,900
|align="right"| 6,100,852
|align="right"| 13,172,239
|align="right"| 28,499,827
|align="right"| 61,786,266
|align="right"| 134,202,509
|align="right"| 292,011,464
|
|
|
|-
! ''A'' depth
|align="right"| 1
|align="right"| 2
|align="right"| 3
|align="right"| 4
|align="right"| 8
|align="right"| 16
|align="right"| 32
|align="right"| 64
|align="right"| 128
|align="right"| 256
|align="right"| 512
|align="right"| 1,024
|align="right"| 2,048
|align="right"| 4,096
|align="right"| 8,192
|align="right"| 16,384
|align="right"| 32,768
|align="right"| 65,536
|align="right"| 131,072
|align="right"| 262,144
|align="right"| 524,288
|align="right"| 1,048,576
|align="right"| 2,097,152
|align="right"| 4,194,304
|align="right"| 8,388,608
|
|
|
|
|
|
|-
! ''B'' called
|align="right"| 0
|align="right"| 1
|align="right"| 2
|align="right"| 3
|align="right"| 7
|align="right"| 17
|align="right"| 37
|align="right"| 79
|align="right"| 166
|align="right"| 346
|align="right"| 721
|align="right"| 1,508
|align="right"| 3,167
|align="right"| 6,672
|align="right"| 14,090
|align="right"| 29,824
|align="right"| 63,286
|align="right"| 134,651
|align="right"| 287,263
|align="right"| 614,441
|align="right"| 1,317,532
|align="right"| 2,831,899
|align="right"| 6,100,851
|align="right"| 13,172,238
|align="right"| 28,499,826
|
|
|
|
|
|
|-
! ''B'' depth
|align="right"| 0
|align="right"| 1
|align="right"| 2
|align="right"| 3
|align="right"| 7
|align="right"| 15
|align="right"| 31
|align="right"| 63
|align="right"| 127
|align="right"| 255
|align="right"| 511
|align="right"| 1,023
|align="right"| 2,047
|align="right"| 4,095
|align="right"| 8,191
|align="right"| 16,383
|align="right"| 32,767
|align="right"| 65,535
|align="right"| 131,071
|align="right"| 262,143
|align="right"| 524,287
|align="right"| 1,048,575
|align="right"| 2,097,151
|align="right"| 4,194,303
|align="right"| 8,388,607
|
|
|
|
|
|
|}


;Related tasks:
*   [[Jensen's Device]]





## Ada

Ada 2005 supports access to subprograms which is used in the implementation below:

{{works with|Ada|Ada|2005, 2012}}


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Man_Or_Boy is
   function Zero return Integer is begin return  0; end Zero;
   function One return Integer  is begin return  1; end One;
   function Neg return Integer  is begin return -1; end Neg;

   function A
            (  K : Integer;
               X1, X2, X3, X4, X5 : access function return Integer
            )  return Integer is
      M : Integer := K; -- K is read-only in Ada. Here is a mutable copy of
      function B return Integer is
      begin
         M := M - 1;
         return A (M, B'Access, X1, X2, X3, X4);
      end B;
   begin
      if M <= 0 then
         return X4.all + X5.all;
      else
         return B;
      end if;
   end A;
begin
   Put_Line
   (  Integer'Image
       (  A
          (  10,
             One'Access, -- Returns  1
             Neg'Access, -- Returns -1
             Neg'Access, -- Returns -1
             One'Access, -- Returns  1
             Zero'Access -- Returns  0
   )  )  );
end Man_Or_Boy;
```


Ada 2012 supports expression functions and conditional expressions which are used in the implementation below:

{{works with|Ada|Ada|2012}}


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Man_Or_Boy is
   function Zero return Integer is (0);
   function One return Integer  is (1);
   function Neg return Integer  is (-1);

   function A
            (K : Integer;
             X1, X2, X3, X4, X5 : access function return Integer)
             return Integer is
      M : Integer := K; -- K is read-only in Ada. Here is a mutable copy of
      function B return Integer is
      begin
         M := M - 1;
         return A (M, B'Access, X1, X2, X3, X4);
      end B;
   begin
      return (if M <= 0 then X4.all + X5.all else B);
   end A;
begin
   Put_Line
     (Integer'Image
        (A (K => 10,
            X1 => One'Access,
            X2 => Neg'Access,
            X3 => Neg'Access,
            X4 => One'Access,
            X5 => Zero'Access)));
end Man_Or_Boy;
```


This version resembles more Knuth's original version in that the result of B is thrown away.


```ada
with Ada.Text_IO;
use  Ada.Text_IO;

procedure Man_Or_Boy is

  function Zero return Integer is ( 0);
  function One  return Integer is ( 1);
  function Neg  return Integer is (-1);

  function A (K: Integer;
              X1, X2, X3, X4, X5: access function return Integer) return Integer is
    M    : Integer := K;  -- K is read-only in Ada. Here is a mutable copy of K
    Res_A: Integer;
    function B return Integer is
    begin
      M     := M - 1;
      Res_A := A (M, B'Access, X1, X2, X3, X4);  -- set result of A
      return Res_A;
    end B;
  begin
    if M <= 0 then
      return X4.all + X5.all;
    else
      declare
        Dummy: constant Integer := B;  -- throw away
      begin
        return Res_A;
      end;
    end if;
  end A;

begin

  Put_Line (Integer'Image (A (K => 10,
                              X1 => One 'Access,
                              X2 => Neg 'Access,
                              X3 => Neg 'Access,
                              X4 => One 'Access,
                              X5 => Zero'Access)));

end Man_Or_Boy;
```


Sample output:

```txt

 -67

```



## Aime


```aime
integer
F(list l)
{
    l[1];
}

integer
eval(list l)
{
    l[0](l);
}

integer A(list);

integer
B(list l)
{
    A(list(B, l[1] = l[1] - 1, l, l[-5], l[-4], l[-3], l[-2]));
}

integer
A(list l)
{
    integer x;

    if (l[1] < 1) {
        x = eval(l[-2]) + eval(l[-1]);
    } else {
        x = B(l);
    }

    x;
}

integer
main(void)
{
    list f1, f0, fn1;

    l_append(f1, F);
    l_append(f1, 1);

    l_append(f0, F);
    l_append(f0, 0);

    l_append(fn1, F);
    l_append(fn1, -1);

    o_(A(list(B, 10, f1, fn1, fn1, f1, f0)), "\n");

    0;
}
```

Output:

```txt

 -67

```


=={{header|ALGOL 60}} - Knuth's example==
 '''begin'''
   '''real''' '''procedure''' A (k, x1, x2, x3, x4, x5);
   '''value''' k; '''integer''' k;
   '''real''' x1, x2, x3, x4, x5;
   '''begin'''
     '''real''' '''procedure''' B;
     '''begin''' k:= k - 1;
           B:= A := A (k, B, x1, x2, x3, x4)
     '''end''';
     '''if''' k <= 0 '''then''' A:= x4 + x5 '''else''' B
   '''end''';
   outreal (A (10, 1, -1, -1, 1, 0))
 '''end'''

This creates a tree of ''B'' call frames that refer to each other and to the containing ''A'' call frames, each of which has its own copy of ''k'' that changes every time the associated ''B'' is called. Trying to work it through on paper is probably fruitless, but the correct answer is &minus;67, despite the fact that in the original paper Knuth postulated it to be &minus;121.

Note that Knuth's code states:

     '''if''' k <= 0 '''then''' A:= x4 + x5 '''else''' B

which actually discards the result value from the call to B.  Most of the translated examples below are equivalent to:

     A := ('''if''' k <= 0 '''then''' x4 + x5 '''else''' B)

and are therefore strictly incorrect, although in a correct 'man' compiler they do produce the expected result, because Knuth's version has already assigned to the return variable for A from within B, and it is in fact that assignment which is the true return value of the function:

           B:= A := A (k, B, x1, x2, x3, x4)

It is most likely that this was a deliberate attempt by Knuth to find yet another way to break 'boy' compilers, rather than merely being sloppy code.


## ALGOL 68

{{trans|ALGOL 60}}

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}
[[wp:Charles_H._Lindsey|Charles H. Lindsey]] implemented this [http://archive.computerhistory.org/resources/text/algol/algol_bulletin/A52/P43.HTM man boy test] in [[ALGOL 68]], and - as call by name is not necessary - the same algorithm can be implemented in many languages including [[:Category:Pascal|Pascal]] and [[:Category:PL/I|PL/I]] <!-- <ref>{{cite web|title=Block Structure and Environments | author=[[Charles H. Lindsey]] | url=http://archive.computerhistory.org/resources/text/algol/algol_bulletin/A52/P43.HTM | accessyear=2007 | accessmonthday=May 2| year=1988 | month=Dec}}</ref>. -->  <!-- <ref>{{cite web|title="Man or boy" test|url=http://groups.google.com/group/comp.lang.haskell/browse_thread/thread/eef78beaaac73b26/7a6672ceea07b34e}}</ref>,  -->.

```algol68
PROC a = (INT in k, PROC INT xl, x2, x3, x4, x5) INT:(
  INT k := in k;
  PROC b = INT: a(k-:=1, b, xl, x2, x3, x4);
  ( k<=0 | x4 + x5 | b )
);

test:(
 printf(($gl$,a(10, INT:1, INT:-1, INT:-1, INT:1, INT:0)))
)
```

Output:

```txt

        -67

```



## AppleScript

{{works with|Smile}}

AppleScript's stack limit is around 500 frames, which is too low to run this example. It runs in the compatible [http://en.wikipedia.org/wiki/Smile_%28software%29 Smile] environment, however.


```applescript
on a(k, x1, x2, x3, x4, x5)
	script b
		set k to k - 1
		return a(k, b, x1, x2, x3, x4)
	end script
	if k ≤ 0 then
		return (run x4) + (run x5)
	else
		return (run b)
	end if
end a

on int(x)
	script s
		return x
	end script
	return s
end int

a(10, int(1), int(-1), int(-1), int(1), int(0))

```

Output:

```txt

-67

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      HIMEM = PAGE + 200000000 : REM Increase recursion depth

      FOR k% = 0 TO 20
        PRINT FNA(k%, ^FN1(), ^FN_1(), ^FN_1(), ^FN1(), ^FN0())
      NEXT
      END

      DEF FNA(k%, x1%, x2%, x3%, x4%, x5%)
      IF k% <= 0 THEN = FN(x4%)(x4%) + FN(x5%)(x5%)
      LOCAL b{}
      DIM b{fn%, k%, x1%, x2%, x3%, x4%, x5%}
      b.fn% = !^FNB()
      b.k%  = k%
      b.x1% = x1%
      b.x2% = x2%
      b.x3% = x3%
      b.x4% = x4%
      b.x5% = x5%
      DEF FNB(!(^b{}+4))
      b.k% -= 1
      = FNA(b.k%, b{}, b.x1%, b.x2%, b.x3%, b.x4%)

      DEF FN0(d%) = 0
      DEF FN1(d%) = 1
      DEF FN_1(d%) = -1
```

Output:

```txt

         1
         0
        -2
         0
         1
         0
         1
        -1
       -10
       -30
       -67
      -138
      -291
      -642
     -1446
     -3250
     -7244
    -16065
    -35601
    -78985
   -175416

```



## C


Even if [[closures]] are not available in a language, their effect can be simulated. This is what happens in the following C implementation:


```c
/* man-or-boy.c */
#include <stdio.h>
#include <stdlib.h>

// --- thunks
typedef struct arg
{
  int       (*fn)(struct arg*);
  int        *k;
  struct arg *x1, *x2, *x3, *x4, *x5;
} ARG;

// --- lambdas
int f_1 (ARG* _) { return -1; }
int f0  (ARG* _) { return  0; }
int f1  (ARG* _) { return  1; }

// --- helper
int eval(ARG* a) { return a->fn(a); }
#define MAKE_ARG(...) (&(ARG){__VA_ARGS__})
#define FUN(...) MAKE_ARG(B, &k, __VA_ARGS__)

int A(ARG*);

// --- functions
int B(ARG* a)
{
  int k = *a->k -= 1;
  return A(FUN(a, a->x1, a->x2, a->x3, a->x4));
}

int A(ARG* a)
{
  return *a->k <= 0 ? eval(a->x4) + eval(a->x5) : B(a);
}

int main(int argc, char **argv)
{
  int k = argc == 2 ? strtol(argv[1], 0, 0) : 10;
  printf("%d\n", A(FUN(MAKE_ARG(f1), MAKE_ARG(f_1), MAKE_ARG(f_1),
                       MAKE_ARG(f1), MAKE_ARG(f0))));
  return 0;
}
```


Two gcc extensions to the C language, nested functions and block sub-expressions, can be combined to create this elegant version:

Version: gcc version 4.1.2 20070925 (Red Hat 4.1.2-27)

```c
#include <stdio.h>
#define INT(body) ({ int lambda(){ body; }; lambda; })
main(){
  int a(int k, int xl(), int x2(), int x3(), int x4(), int x5()){
    int b(){
      return a(--k, b, xl, x2, x3, x4);
    }
    return k<=0 ? x4() + x5() : b();
  }
  printf(" %d\n",a(10, INT(return 1), INT(return -1), INT(return -1), INT(return 1), INT(return 0)));
}
```


C without C99 or gcc extensions:


```c
#include <stdio.h>
#include <stdlib.h>

typedef struct frame
{
  int (*fn)(struct frame*);
  union { int constant; int* k; } u;
  struct frame *x1, *x2, *x3, *x4, *x5;
} FRAME;

FRAME* Frame(FRAME* f, int* k, FRAME* x1, FRAME* x2, FRAME *x3, FRAME *x4, FRAME *x5)
{
  f->u.k = k;
  f->x1 = x1;
  f->x2 = x2;
  f->x3 = x3;
  f->x4 = x4;
  f->x5 = x5;
  return f;
}

int F(FRAME* a) { return a->u.constant; }

int eval(FRAME* a) { return a->fn(a); }

int A(FRAME*);

int B(FRAME* a)
{
  int k = (*a->u.k -= 1);
  FRAME b = { B };
  return A(Frame(&b, &k, a, a->x1, a->x2, a->x3, a->x4));
}

int A(FRAME* a)
{
  return *a->u.k <= 0 ? eval(a->x4) + eval(a->x5) : B(a);
}

int main(int argc, char** argv)
{
  int k = argc == 2 ? strtol(argv[1], 0, 0) : 10;
  FRAME a = { B }, f1 = { F, { 1 } }, f0 = { F, { 0 } }, fn1 = { F, { -1 } };

  printf("%d\n", A(Frame(&a, &k, &f1, &fn1, &fn1, &f1, &f0)));
  return 0;
}
```


Output:
 -67


## C++

works with GCC

Uses "shared_ptr" smart pointers from Boost / TR1 to automatically deallocate objects. Since we have an object which needs to pass a pointer to itself to another function, we need to use "enable_shared_from_this".


```cpp
#include <iostream>
#include <tr1/memory>
using std::tr1::shared_ptr;
using std::tr1::enable_shared_from_this;

struct Arg {
  virtual int run() = 0;
  virtual ~Arg() { };
};

int A(int, shared_ptr<Arg>, shared_ptr<Arg>, shared_ptr<Arg>,
      shared_ptr<Arg>, shared_ptr<Arg>);

class B : public Arg, public enable_shared_from_this<B> {
private:
  int k;
  const shared_ptr<Arg> x1, x2, x3, x4;

public:
  B(int _k, shared_ptr<Arg> _x1, shared_ptr<Arg> _x2, shared_ptr<Arg> _x3,
    shared_ptr<Arg> _x4)
    : k(_k), x1(_x1), x2(_x2), x3(_x3), x4(_x4) { }
  int run() {
    return A(--k, shared_from_this(), x1, x2, x3, x4);
  }
};

class Const : public Arg {
private:
  const int x;
public:
  Const(int _x) : x(_x) { }
  int run () { return x; }
};

int A(int k, shared_ptr<Arg> x1, shared_ptr<Arg> x2, shared_ptr<Arg> x3,
      shared_ptr<Arg> x4, shared_ptr<Arg> x5) {
  if (k <= 0)
    return x4->run() + x5->run();
  else {
    shared_ptr<Arg> b(new B(k, x1, x2, x3, x4));
    return b->run();
  }
}

int main() {
  std::cout << A(10, shared_ptr<Arg>(new Const(1)),
                 shared_ptr<Arg>(new Const(-1)),
                 shared_ptr<Arg>(new Const(-1)),
                 shared_ptr<Arg>(new Const(1)),
                 shared_ptr<Arg>(new Const(0))) << std::endl;
  return 0;
}
```


{{works with|C++11}} uses anonymous functions. Tested with g++ version 4.5 and Visual C++ version 16 (Windows SDK 7.1):

```cpp
#include <functional>
#include <iostream>

typedef std::function<int()> F;

static int A(int k, const F &x1, const F &x2, const F &x3, const F &x4, const F &x5)
{
	F B = [=, &k, &B]
	{
		return A(--k, B, x1, x2, x3, x4);
	};

	return k <= 0 ? x4() + x5() : B();
}

static F L(int n)
{
	return [n] { return n; };
}

int main()
{
	std::cout << A(10, L(1), L(-1), L(-1), L(1), L(0)) << std::endl;
	return 0;
}
```


{{works with|TR1}} uses TR1 without C++11.

```cpp
#include <tr1/functional>
#include <iostream>

typedef std::tr1::function<int()> F;

static int A(int k, const F &x1, const F &x2, const F &x3, const F &x4, const F &x5);

struct B_class {
  int &k;
  const F x1, x2, x3, x4;
  B_class(int &_k, const F &_x1, const F &_x2, const F &_x3, const F &_x4) :
    k(_k), x1(_x1), x2(_x2), x3(_x3), x4(_x4) { }
  int operator()() const { return A(--k, *this, x1, x2, x3, x4); }
};

static int A(int k, const F &x1, const F &x2, const F &x3, const F &x4, const F &x5)
{
  F B = B_class(k, x1, x2, x3, x4);
  return k <= 0 ? x4() + x5() : B();
}

struct L {
  const int n;
  L(int _n) : n(_n) { }
  int operator()() const { return n; }
};

int main()
{
  std::cout << A(10, L(1), L(-1), L(-1), L(1), L(0)) << std::endl;
  return 0;
}
```


=={{header|C sharp|C#}}==
C# 2.0 supports anonymous methods which are used in the implementation below:

{{works with|C sharp|C#|2+}}


```csharp
using System;

delegate T Func<T>();

class ManOrBoy
{
    static void Main()
    {
        Console.WriteLine(A(10, C(1), C(-1), C(-1), C(1), C(0)));
    }

    static Func<int> C(int i)
    {
        return delegate { return i; };
    }

    static int A(int k, Func<int> x1, Func<int> x2, Func<int> x3, Func<int> x4, Func<int> x5)
    {
        Func<int> b = null;
        b = delegate { k--; return A(k, b, x1, x2, x3, x4); };
        return k <= 0 ? x4() + x5() : b();
    }
}

```


C# 3.0 supports lambda expressions which are used in the implementation below:

{{works with|C sharp|C#|3+}}


```csharp
using System;

class ManOrBoy
{
    static void Main()
    {
        Console.WriteLine(A(10, () => 1, () => -1, () => -1, () => 1, () => 0));
    }

    static int A(int k, Func<int> x1, Func<int> x2, Func<int> x3, Func<int> x4, Func<int> x5)
    {
        Func<int> b = null;
        b = () => { k--; return A(k, b, x1, x2, x3, x4); };
        return k <= 0 ? x4() + x5() : b();
    }
}
```



## Clipper



```Clipper
Procedure Main()
   Local k
   For k := 0 to 20
      ? "A(", k, ", 1, -1, -1, 1, 0) =", A(k, 1, -1, -1, 1, 0)
   Next
Return

Static Function A(k, x1, x2, x3, x4, x5)
   Local ARetVal
   Local B := {|| --k, ARetVal := A(k, B, x1, x2, x3, x4) }
   If k <= 0
      ARetVal := Evaluate(x4) + Evaluate(x5)
   Else
      B:Eval()
   Endif
Return ARetVal

Static Function Evaluate(x)
   Local xVal
   If ValType(x) == "B"
      xVal := x:Eval()
   Else
      xVal := x
   Endif
Return xVal
```



// With Clipper 5.2e compiler and standard RTLINK linker, default settings, only manages up to k=5 before a stack fault:

```txt
EVALUATE (0)  Unrecoverable error 650: Processor stack fault
```


// Using Blinker v5.1 it can get up to k=7 by increasing the stack size via BLINKER PROCEDURE DEPTH 74.  But that may be the limit for 16-bit Clipper; increasing the procedure depth further does not help, and eventually results in

```txt
A (0)  Unrecoverable error 667: Eval stack fault
```


Harbour however is definitely a man: a 32-bit WinXP executable built with Harbour v3.1 and mingw gcc 4.6.1 manages up to k=13 with the default settings.  Increasing the stack size (via the Microsoft utility "editbin /STACK:nnn", or "ulimit -s" in linux) allows it to achieve deeper levels:

```txt
A(          0 , 1, -1, -1, 1, 0) =          1
A(          1 , 1, -1, -1, 1, 0) =          0
A(          2 , 1, -1, -1, 1, 0) =         -2
A(          3 , 1, -1, -1, 1, 0) =          0
A(          4 , 1, -1, -1, 1, 0) =          1
A(          5 , 1, -1, -1, 1, 0) =          0
A(          6 , 1, -1, -1, 1, 0) =          1
A(          7 , 1, -1, -1, 1, 0) =         -1
A(          8 , 1, -1, -1, 1, 0) =        -10
A(          9 , 1, -1, -1, 1, 0) =        -30
A(         10 , 1, -1, -1, 1, 0) =        -67
A(         11 , 1, -1, -1, 1, 0) =       -138
A(         12 , 1, -1, -1, 1, 0) =       -291
A(         13 , 1, -1, -1, 1, 0) =       -642
A(         14 , 1, -1, -1, 1, 0) =      -1446
A(         15 , 1, -1, -1, 1, 0) =      -3250
A(         16 , 1, -1, -1, 1, 0) =      -7244
A(         17 , 1, -1, -1, 1, 0) =     -16065
A(         18 , 1, -1, -1, 1, 0) =     -35601
A(         19 , 1, -1, -1, 1, 0) =     -78985
A(         20 , 1, -1, -1, 1, 0) =    -175416
```



## Clojure



```lisp
(declare a)

(defn man-or-boy
  "Man or boy test for Clojure"
  [k]
  (let [k (atom k)]
    (a k
       (fn [] 1)
       (fn [] -1)
       (fn [] -1)
       (fn [] 1)
       (fn [] 0))))

(defn a
  [k x1 x2 x3 x4 x5]
  (let [k (atom @k)]
    (letfn [(b []
               (swap! k dec)
               (a k b x1 x2 x3 x4))]
      (if (<= @k 0)
        (+ (x4) (x5))
        (b)))))

(man-or-boy 10)

```



## Common Lisp



```lisp
(defun man-or-boy (x)
 (a x (lambda () 1)
      (lambda () -1)
      (lambda () -1)
      (lambda () 1)
      (lambda () 0)))

(defun a (k x1 x2 x3 x4 x5)
  (labels ((b ()
             (decf k)
             (a k #'b x1 x2 x3 x4)))
    (if (<= k 0)
        (+ (funcall x4) (funcall x5))
        (b))))

(man-or-boy 10)
```



## Crystal


```ruby
def a(k, x1, x2, x3, x4, x5)
  b = uninitialized -> typeof(k)
  b = ->() { k -= 1; a(k, b, x1, x2, x3, x4) }
  k <= 0 ? x4.call + x5.call : b.call
end

puts a(10, -> {1}, -> {-1}, -> {-1}, -> {1}, -> {0})
```

{{out}}

```txt

-67

```



## D


### Straightforward Version


```d
import core.stdc.stdio: printf;

int a(int k, const lazy int x1, const lazy int x2, const lazy int x3,
      const lazy int x4, const lazy int x5) pure {
    int b() {
        k--;
        return a(k, b(), x1, x2, x3, x4);
    }
    return k <= 0 ? x4 + x5 : b();
}

void main() {
    printf("%d\n", a(10, 1, -1, -1, 1, 0));
}
```

The DMD compiler is a man. Increasing the maximum stack space to about 1.2 GB the DMD 2.059 compiler computes the result -9479595 for k = 25 in about 6.5 seconds on a 32 bit system (-inline -O -release -L/STACK:1300000000).


### Lazy Variadic Function Version

[http://www.digitalmars.com/d/1.0/function.html Lazy Variadic Functions] version, as quoted:

:If the variadic parameter is an array of delegates with no parameters:
<tt>    void foo(int delegate()[] dgs ...);</tt>
Then each of the arguments whose type does not match that of the delegate is converted to a delegate.
<tt>    int delegate() dg;</tt>
<tt>    foo(1, 3+x, dg, cast(int delegate())null);</tt>
is the same as:
<tt>    foo( { return 1; }, { return 3+x; }, dg, null );</tt>


```d
int A(int k, int delegate() nothrow @safe[] x...) nothrow @safe {
    int b() nothrow @safe {
        k--;
        return A(k, &b, x[0], x[1], x[2], x[3]);
    }

    return (k > 0) ? b() : x[3]() + x[4]();
}

void main() {
    import std.stdio;

    A(10, 1, -1, -1, 1, 0).writeln;
}
```



### Template Version


```d
auto mb(T)(T mob) nothrow @safe { // Embeding function.
    int b() nothrow @safe @nogc {
        static if (is(T == int))
            return mob;
        else
            return mob();
    }

    return &b;
}

int A(T)(int k, T x1, T x2, T x3, T x4, T x5) nothrow @safe {
    static if (is(T == int)) {
        return A(k, mb(x1), mb(x2), mb(x3), mb(x4), mb(x5));
    } else {
        int b() nothrow @safe {
            k--;
            return A(k, &b, x1, x2, x3, x4);
        }
        return (k <= 0) ? x4() + x5() : b();
    }
}

void main() {
    import std.stdio;

    A(10, 1, -1, -1, 1, 0).writeln;
}
```



### Anonymous Class Version

Similar to Java example:

```d
import std.stdio;

interface B {
    int run();
}

int A(int k, int x1, int x2, int x3, int x4, int x5) {
    B mb(int a) {
        return new class() B {
            int run() {
                return a;
            }
        };
    }

    return A(k, mb(x1), mb(x2), mb(x3), mb(x4), mb(x5));
}

int A(int k, B x1, B x2, B x3, B x4, B x5) {
    if (k <= 0) {
        return x4.run() + x5.run();
    } else {
        return (new class() B {
            int m;

            this() {
                this.m = k;
            }

            int run() {
                m--;
                return A(m, this, x1, x2, x3, x4);
            }
        }).run();
    }
}

void main() {
    writeln(A(10, 1, -1, -1, 1, 0));
}
```



### Faster Version

This version cheats, using a different much faster algorithm.

```d
import std.bigint, std.functional;

// Adapted from C code by Goran Weinholt, adapted from Knuth code.
BigInt A(in int k, in int x1, in int x2, in int x3,
         in int x4, in int x5) {
    static struct Inner {
        static BigInt c1_(in int k) {
            if (k > 5)
                return c1(k - 1) + c2(k - 1);
            static immutable t = [0, 0, 0, 1, 2, 3];
            return t[k].BigInt;
        }
        alias c1 = memoize!c1_;

        static BigInt c2_(in int k) {
            if (k > 5)
                return c2(k - 1) + c3(k - 1);
            static immutable t = [0, 0, 1, 1, 1, 2];
            return t[k].BigInt;
        }
        alias c2 = memoize!c2_;

        static BigInt c3_(in int k) {
            if (k > 5)
                return c3(k - 1) + c4(k);
            static immutable t = [0, 1, 1, 0, 0, 1];
            return t[k].BigInt;
        }
        alias c3 = memoize!c3_;

        static BigInt c4_(in int k) {
            if (k > 5)
                return c1(k - 1) + c4(k - 1) - 1;
            static immutable t = [1, 1, 0, 0, 0, 0];
            return t[k].BigInt;
        }
        alias c4 = memoize!c4_;

        static int c5(in int k) pure nothrow {
            return !!k;
        }
    }

    with (Inner)
        return c1(k) * x1 + c2(k) * x2 + c3(k) * x3 +
               c4(k) * x4 + c5(k) * x5;
}

void main() {
    import std.stdio, std.conv, std.range;

    foreach (immutable i; 0 .. 40)
        writeln(i, " ", A(i, 1, -1, -1, 1, 0));

    writefln("...\n500 %-(%s\\\n     %)",
             A(500, 1, -1, -1, 1, 0).text.chunks(60));
}
```

{{out}}

```txt
0 1
1 0
2 -2
3 0
4 1
5 0
6 1
7 -1
8 -10
9 -30
10 -67
11 -138
12 -291
13 -642
14 -1446
15 -3250
16 -7244
17 -16065
18 -35601
19 -78985
20 -175416
21 -389695
22 -865609
23 -1922362
24 -4268854
25 -9479595
26 -21051458
27 -46750171
28 -103821058
29 -230560902
30 -512016658
31 -1137056340
32 -2525108865
33 -5607619809
34 -12453091089
35 -27655133488
36 -61414977599
37 -136386945105
38 -302880491178
39 -672620048590
...
500 -36608736847739011154197160517980804737983159473082319871442\
     971269362427356493943811133837572598465628264243340122956824\
     36642737343738734381233089412653032375404781872267320
```



## Delphi


The latest editions of Delphi support anonymous methods, providing a way to implement call by name semantics.


```delphi
type
  TFunc<T> = reference to function: T;

function C(x: Integer): TFunc<Integer>;
begin
  Result := function: Integer
  begin
    Result := x;
  end;
end;

function A(k: Integer; x1, x2, x3, x4, x5: TFunc<Integer>): Integer;
var
  b: TFunc<Integer>;
begin
  b := function: Integer
  begin
    Dec(k);
    Result := A(k, b, x1, x2, x3, x4);
  end;
  if k <= 0 then
    Result := x4 + x5
  else
    Result := b;
end;

begin
  Writeln(A(10, C(1), C(-1), C(-1), C(1), C(0))); // -67 output
end.
```


=={{header|Déjà Vu}}==

{{trans|Python}}


```dejavu
a k x1 x2 x3 x4 x5:
	local b:
		set :k -- k
		a k @b @x1 @x2 @x3 @x4
	if <= k 0:
		+ x4 x5
	else:
		b
local x i:
	labda:
		i

!. a 10 x 1 x -1 x -1 x 1 x 0
```



## Dyalect



```Dyalect
func C(i) {
    () => i
}

func A(k, x1, x2, x3, x4, x5) {
    var b
    b = () => {
        k -= 1
        A(k, b, x1, x2, x3, x4)
    }
    if k <= 0 {
        x4() + x5()
    } else {
        b()
    }
}

print(A(12, C(1), C(-1), C(-1), C(1), C(0)))
```


{{out}}


```txt
-291
```



## Dylan


```dylan
define method a
    (k :: <integer>, x1 :: <function>, x2 :: <function>, x3 :: <function>,
                     x4 :: <function>, x5 :: <function>)
 => (i :: <integer>)

  local b() => (i :: <integer>)
    k := k - 1;
    a(k, b, x1, x2, x3, x4)
  end;

  if (k <= 0) x4() + x5() else b() end if

end method a;

define method man-or-boy
    (x :: <integer>)
 => (i :: <integer>)

  a(x, method()  1 end,
       method() -1 end,
       method() -1 end,
       method()  1 end,
       method()  0 end)

end method man-or-boy;

format-out("%d\n", man-or-boy(10))
```



## E


Provided that it is marked in the caller and callee, E can perfectly emulate the requested [[wp:Call-by-name#Call_by_name|call-by-name]] behavior by passing slots instead of values:


```e
def a(var k, &x1, &x2, &x3, &x4, &x5) {
    def bS; def &b := bS
    bind bS {
        to get() {
            k -= 1
            return a(k, &b, &x1, &x2, &x3, &x4)
        }
    }
    return if (k <= 0) { x4 + x5 } else { b }
}

def p := 1
def n := -1
def z := 0
println(a(10, &p, &n, &n, &p, &z))
```


Here each of the "<code>x</code>" parameters is effectively call-by-name. <var><code>b</code></var> is bound to a custom slot definition.


## EchoLisp


```scheme

;; copied from Scheme
(define (A k x1 x2 x3 x4 x5)
  (define (B)
    (set! k (- k 1))
    (A k B x1 x2 x3 x4))
  (if (<= k 0)
      (+ (x4) (x5))
      (B)))

(A 10 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))
   → -67
(A 13 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))
   → -642
(A 14 ..)
❗ InternalError : too much recursion - JS internal error (please, report it)-
   → stack overflow using FireFox

```




## Ela


Stack overflow is not a problem in Ela (but "out of memory" is):


```ela
open monad io unsafe.cell unsafe.console

liftM2 f m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (f x1 x2)

a k x1 x2 x3 x4 x5 = do
  r <- return $ ref k
  let b = & do k <- return $ pred (valueof r)
             a k b x1 x2 x3 x4
  if k <= 0 then liftM2 (+) x4 x5 else b

_ = a 10 (!!1) (!! -1) (!! -1) (!!1) (!!0) >>= (putStr << show) ::: IO
  where (!!) f = & return f ::: IO
```



## Elena

ELENA 4.1:

```elena
import extensions;

A(k,x1,x2,x3,x4,x5)
{
    var m := new ref<int>(k);

    var b := { m -= 1; ^ A(m,this self,x1,x2,x3,x4) };

    if (m <= 0)
    {
        ^ x4() + x5()
    }
    else
    {
        ^ b()
    }
}

public program()
{
    for(int n := 0, n <= 14, n += 1)
    {
        console.printLine(A(n,{^1},{^-1},{^-1},{^1},{^0}))
    }
}
```

{{out}}

```txt

1
0
-2
0
1
0
1
-1
-10
-30
-67
-138
-291
-642
-1446

```



## Erlang

Erlang variables cannot be changed after binding, so k is decremented by sending a message to a process.

```txt

kloop(K) ->
    receive
        {decr,Pid} -> Pid ! K-1, kloop(K-1);
        _          -> ok
    end.


a(K, X1, X2, X3, X4, X5) ->
    Kproc = spawn(fun() -> kloop(K) end),
    B = fun (B) ->
                Kproc ! {decr, self()},
                receive Kdecr ->
                        a(Kdecr, fun() -> B(B) end, X1, X2, X3, X4)
                end
        end,
    if
        K =< 0  -> Kproc ! X4() + X5();
        true    -> Kproc ! B(B)
    end.


manorboy(N) ->
     a(N, fun() -> 1 end, fun() -> -1 end, fun() -> -1 end, fun() -> 1 end, fun() -> 0 end ).

```


=={{header|F Sharp|F#}}==
Straightforward version:


```fsharp

[<EntryPoint>]
let main (args : string[]) =
    let k = int(args.[0])

    let l x = fun() -> x

    let rec a k x1 x2 x3 x4 x5 =
        if k <= 0 then
            x4() + x5()
        else
            let k = ref k
            let rec b() =
                k := !k - 1
                a !k b x1 x2 x3 x4
            b()

    a k (l 1) (l -1) (l -1) (l 1) (l 0)
    |> printfn "%A"

    0

```


Using a trampoline to avoid stack overflows when k >= 20:


```fsharp

type Tramp<'t> =
    | Delay of (unit -> Tramp<'t>)
    | Bind of Tramp<'t> * ('t -> Tramp<'t>)
    | Return of 't
    | ReturnFrom of Tramp<'t>

type Tramp() =
    member this.Delay(f) = Delay f
    member this.Bind(x, f) = Bind(x, f)
    member this.Return(x) = Return x
    member this.ReturnFrom(x) = ReturnFrom x

let tramp = Tramp()

let run (tr : Tramp<'t>) =
    let rec loop tr stack =
        match tr with
        | Delay f -> loop (f()) stack
        | Bind(x, f) -> loop x (f :: stack)
        | Return x ->
            match stack with
            | [] -> x
            | f :: stack' -> loop (f x) stack'
        | ReturnFrom tr -> loop tr stack
    loop tr []

[<EntryPoint>]
let main (args : string[]) =
    let k = int(args.[0])

    let l x = fun() -> Return x

    tramp {
        let rec a k x1 x2 x3 x4 x5 =
            tramp {
                if k <= 0 then
                    let! x4' = x4()
                    let! x5' = x5()
                    return x4' + x5'
                else
                    let k = ref k
                    let rec b() =
                        tramp {
                            k := !k - 1
                            return! a !k b x1 x2 x3 x4
                        }
                    return! b()
            }

        return! a k (l 1) (l -1) (l -1) (l 1) (l 0)
    }
    |> run
    |> printfn "%A"

    0

```



## Fantom

Fantom has closures, so:


```Fantom

class ManOrBoy
{
  Void main()
  {
    echo(A(10, |->Int|{1}, |->Int|{-1}, |->Int|{-1}, |->Int|{1}, |->Int|{0}));
  }

  static Int A(Int k, |->Int| x1, |->Int| x2, |->Int| x3, |->Int| x4, |->Int| x5)
  {
    |->Int|? b
    b = |->Int| { k--; return A(k, b, x1, x2, x3, x4) }
    return k <= 0 ? x4() + x5() : b()
  }
}

```


yields

```txt

  -67

```




## Forth

{{works with|gforth|0.7.9_20180830}}

Gforth provides flat closures [{: ... :}L ... ;] that are initialized from the stack.  You have to perform flat-closure conversion and assignment conversion manually (and this has been done here).


```Forth
: A {: w^ k x1 x2 x3 xt: x4 xt: x5 | w^ B :} recursive
  k @ 0<= IF  x4 x5 f+  ELSE
    B k x1 x2 x3 action-of x4 [{: B k x1 x2 x3 x4 :}L
      -1 k +!
      k @ B @ x1 x2 x3 x4 A ;] dup B !
      execute  THEN ;
10 [: 1e ;] [: -1e ;] 2dup swap [: 0e ;] A f.
```



## Fortran

Fortran 2008 (uses an internal procedure as function argument). Tested with g95 and gfortran 4.6.

```Fortran
module man_or_boy

implicit none

contains

  recursive integer function A(k,x1,x2,x3,x4,x5) result(res)
    integer, intent(in) :: k
    interface
      recursive integer function x1()
      end function
      recursive integer function x2()
      end function
      recursive integer function x3()
      end function
      recursive integer function x4()
      end function
      recursive integer function x5()
      end function
    end interface
    integer :: m
    if ( k <= 0 ) then
      res = x4()+x5()
    else
      m = k
      res = B()
    end if

  contains

    recursive integer function B() result(res)
      m = m-1
      res = A(m,B,x1,x2,x3,x4)
    end function B

  end function A


  recursive integer function one() result(res)
    res = 1
  end function

  recursive integer function minus_one() result(res)
    res = -1
  end function

  recursive integer function zero() result(res)
    res = 0
  end function

end module man_or_boy

program test
  use man_or_boy
  write (*,*) A(10,one,minus_one,minus_one,one,zero)
end program test
```



## Go


```go
package main
import "fmt"

func a(k int, x1, x2, x3, x4, x5 func() int) int {
	var b func() int
	b = func() int {
		k--
		return a(k, b, x1, x2, x3, x4)
	}
	if k <= 0 {
		return x4() + x5()
	}
	return b()
}

func main() {
	x := func(i int) func() int { return func() int { return i } }
	fmt.Println(a(10, x(1), x(-1), x(-1), x(1), x(0)))
}
```


Another version that uses named result parameters the way the original Algol uses the function name. This includes B setting the result of its enclosing A.

```go
package main

import "fmt"

func A(k int, x1, x2, x3, x4, x5 func() int) (a int) {
	var B func() int
	B = func() (b int) {
		k--
		a = A(k, B, x1, x2, x3, x4)
		b = a
		return
	}
	if k <= 0 {
		a = x4() + x5()
	} else {
		_ = B()
	}
	return
}

func main() {
	K := func(x int) func() int { return func() int { return x } }
	fmt.Println(A(10, K(1), K(-1), K(-1), K(1), K(0)))
}

```


By exploiting interfaces, one can more closely parallel the original Algol's polymorphic parameters.

```go
package main

import "fmt"

func eval(v interface{}) int {
	switch v := v.(type) {
	case int:
		return v
	case func() int:
		return v()
	}
	panic("bad type")
	return 0
}

func A(k int, x1, x2, x3, x4, x5 interface{}) (a int) {
	var B func() int
	B = func() (b int) {
		k--
		a = A(k, B, x1, x2, x3, x4)
		b = a
		return
	}
	if k <= 0 {
		a = eval(x4) + eval(x5)
	} else {
		_ = B()
	}
	return
}

func main() {
	fmt.Println(A(10, 1, -1, -1, 1, 0))
}
```


Inspired by D's "faster" version, here's another that uses a different algorithm to compute the result.

```go
package main

import (
    "fmt"
    "math/big"
)

func A(k int) *big.Int {
    one := big.NewInt(1)
    c0 := big.NewInt(3)
    c1 := big.NewInt(2)
    c2 := big.NewInt(1)
    c3 := big.NewInt(0)
    for j := 5; j < k; j++ {
        c3.Sub(c3.Add(c3, c0), one)
        c0.Add(c0, c1)
        c1.Add(c1, c2)
        c2.Add(c2, c3)
    }
    return c0.Add(c0.Sub(c0.Sub(c0, c1), c2), c3)
}

func p(k int) {
    fmt.Printf("A(%d) = ", k)
    if s := A(k).String(); len(s) < 60 {
        fmt.Println(s)
    } else {
        fmt.Printf("%s...%s (%d digits)\n",
            s[:6], s[len(s)-5:], len(s)-1)
    }
}

func main() {
    p(10)
    p(30)
    p(500)
    p(10000)
    p(1e6)
}
```

{{out}}

```txt

A(10) = -67
A(30) = -512016658
A(500) = -36608...67320 (172 digits)
A(10000) = -19928...34899 (3464 digits)
A(1000000) = -67341...95219 (346497 digits)

```



## Gosu


Using Gosu Version 0.10.2.

This is not stictly identical with Wirth's example.


```gosu
function A(in_k: int, x1(): int, x2(): int, x3(): int, x4(): int, x5(): int): int  {
    var k = in_k
    var B(): int  // B is a function variable
    B = \ -> {
        k = k-1;
        return A(k, B, x1, x2, x3, x4)
    }
    return k<=0 ? x4()+x5() : B()
}
print(A(10, \ -> 1, \ -> -1, \ -> -1, \ -> 1, \ -> 0))
```


Output:

```txt

 -67

```



## Groovy

Solution:

```groovy
def a; a = { k, x1, x2, x3, x4, x5 ->
    def b; b = {
        a (--k, b, x1, x2, x3, x4)
    }
    k <= 0 ? x4() + x5() : b()
}

def x = { n -> { it -> n } }
```


Test 1:

```groovy
println (a(10, x(1), x(-1), x(-1), x(1), x(0)))
```


This test overflowed the stack at the default stack size. On my system I required "-Xss1409k" or larger to run successfully.

Output:

```txt
-67
```


Test 2:

```groovy
(0..20).each { k ->
    printf ("%3d: %7d\n", k, a(k, x(1), x(-1), x(-1), x(1), x(0)))
}
```


This test required "-Xss345m" to avoid overflow.

Output:

```txt
  0:       1
  1:       0
  2:      -2
  3:       0
  4:       1
  5:       0
  6:       1
  7:      -1
  8:     -10
  9:     -30
 10:     -67
 11:    -138
 12:    -291
 13:    -642
 14:   -1446
 15:   -3250
 16:   -7244
 17:  -16065
 18:  -35601
 19:  -78985
 20: -175416
```



## Haskell


Haskell is a pure language, so the impure effects of updating ''k'' must be wrapped in the IO or ST [[Monads|monad]]:


```haskell
import Data.IORef (modifyIORef, newIORef, readIORef)

a
  :: (Enum a, Num b, Num a, Ord a)
  => a -> IO b -> IO b -> IO b -> IO b -> IO b -> IO b
a k x1 x2 x3 x4 x5 = do
  r <- newIORef k
  let b = do
        k <- pred ! r
        a k b x1 x2 x3 x4
  if k <= 0
    then (+) <$> x4 <*> x5
    else b
  where
    f !r = modifyIORef r f >> readIORef r

main :: IO ()
main = a 10 # 1 # (-1) # (-1) # 1 # 0 >>= print
  where
    ( # ) f = f . return
```


On an AMD Opteron 6282 SE using GHC 7.8.2 this program can compute ''k'' = 30 in 1064 s and 156.2 GiB.

 384,694,618,688 bytes allocated in the heap
 393,966,884,256 bytes copied during GC
  73,969,319,136 bytes maximum residency (20 sample(s))
     488,551,728 bytes maximum slop
          159874 MB total memory in use (0 MB lost due to fragmentation)

                                       Tot time (elapsed)      Avg pause    Max pause
  Gen  0     711625 colls,     0 par   456.87s   10710.35s       0.0151s       3.1180s
  Gen  1         20 colls,     0 par   273.65s    9674.71s     483.7353s    5204.3968s

  INIT    time     0.00s  (    0.00s elapsed)
  MUT     time   332.81s  (14301.58s elapsed)
  GC      time   730.52s  (20385.06s elapsed)
  EXIT    time     0.43s  (   12.66s elapsed)
  Total   time  1063.76s  (34699.30s elapsed)

  %GC     time      68.7%  (58.7% elapsed)

  Alloc rate    1,155,911,179 bytes per MUT second

  Productivity  31.3% of total user, 1.0% of total elapsed

=={{header|Icon}} and {{header|Unicon}}==
There are a few challenges to implementing MoB in Icon/Unicon.
* There are no nested procedures and non-local variables that go with them
* There is no selectable call by value .vs. call by name/reference.  Knowledge of the implicit mutable/immutable types is needed.
* Procedure calls can't be deferred transparently but can be deferred through co-expressions
* Co-expressions aren't enough as they trap local copies of variables which follow Icon rules for mutability/immutability

The initial solution below involved the use of co-expressions which seemed a natural tool to solve MoB. It turns out that co-expressions aren't necessary to solve this task.  Co-expressions are very powerful and MoB really doesn't exercise their full capability.  There is a lighter weight solution and also a cheat solution which is a further simplification.  The light weight version exploits that procedures are a data type and can be passed around and assigned.  This allows us to defer calling 'B' which is just what is required. The change introduces a new record definition 'defercall' and changes only two lines of the original solution in 'eval' and 'B'. The cheat would be to have 'eval' know that it always called 'B'.

MoB is intense and can be pushed to challenge any machine.  If you run this and the program hangs up or fails with an inadequate space for static allocation error, you may need to tweak the way Icon/Unicon allocates memory.  This is controlled through the environment variables COEXPSIZE, MSTKSIZE, BLKSIZE (see [[Icon%2BUnicon/Intro#Environment_Variables|Icon and Unicon Environment Variables]]).

Notes:
* The co-expression version will require adjustment to COEXPRSIZE, and possibly BLKSIZE and MSTKSIZE.
** Mob 13 ran on a machine with 4GB RAM running Unicon Win32 using COEXPSIZE=71000; BLKSIZE=2000000; and  MSTKSIZE=1000000.
** Mob 15 ran on on a 64-bit linux box with 16GB RAM with COEXPSIZE to 200000 (and everything else defaulting).
* The non-co-expression version required adjustment to BLKSIZE and MSTKSIZE.
** Mob 21 ran on the same 4GB machine with BLKSIZE=10000000; and MSTKSIZE=70000000
** Mob 23 ran on the same 4GB machine with BLKSIZE=20000000; and MSTKSIZE=300000000

The co-expression version.

```Icon
record mutable(value)                                         # we need mutable integers
                                                              # ... be obvious when we break normal scope rules
procedure main(arglist)                                       # supply the initial k value
k := integer(arglist[1])|10                                   # .. or default to 10=default
write("Man or Boy = ", A( k, 1, -1, -1, 1, 0 ) )
end

procedure eval(ref)                                           # evaluator to distinguish between a simple value and a code reference
return if type(ref) == "co-expression" then @ref  else ref
end

procedure A(k,x1,x2,x3,x4,x5)                                 # Knuth's A
k := mutable(k)                                               # make k mutable for B
return if k.value <= 0 then                                   # -> boy compilers may recurse and die here
   eval(x4) + eval(x5)                                        # the crux of separating man .v. boy compilers
else                                                          # -> boy compilers can run into trouble at k=5+
   B(k,x1,x2,x3,x4,x5)
end

procedure B(k,x1,x2,x3,x4,x5)                                 # Knuth's B
k.value -:= 1                                                 # diddle A's copy of k
return A(k.value, create |B(k,x1,x2,x3,x4,x5),x1,x2,x3,x4)    # call A with a new k and 5 x's
end
```


Below are the code changes for the non-co-expression version.  A new record type is introduced and the two return expressions are changed slightly.


```Icon
record defercall(proc,arglist)                                # light weight alternative to co-expr for MoB

procedure eval(ref)                                           # evaluator to distinguish between a simple value and a code reference
return if type(ref) == "defercall" then ref.proc!ref.arglist else ref
end

procedure B(k,x1,x2,x3,x4,x5)                                 # Knuth's B
k.value -:= 1                                                 # diddle A's copy of k
return A(k.value, defercall(B,[k,x1,x2,x3,x4,x5]),x1,x2,x3,x4)# call A with a new k and 5 x's
end
```


=={{Header|Io}}==

Io is nothing if not aggressively manly.


```io
Range

a := method(k, xs,
    b := block(
        k = k -1
        a(k, list(b, xs slice(0,4)) flatten))
    if(k <= 0,
        (xs at(3) call) + (xs at(4) call),
        b call))

f := method(x, block(x))
1 to(500) foreach(k,
    (k .. " ") print
    a(k, list(1, -1, -1, 1, 0) map (x, f(x))) println)
```



## J


Given


```J
A=:4 :0
  L=.cocreate''  NB. L is context where names are defined.
  k__L=:x
  '`x1__L x2__L x3__L x4__L x5__L'=:y
  if.k__L<:0 do.a__L=:(x4__L + x5__L)f.'' else. L B '' end.
  (coerase L)]]]a__L
)

B=:4 :0
  L=.x
  k__L=:k__L-1
  a__L=:k__L A L&B`(x1__L f.)`(x2__L f.)`(x3__L f.)`(x4__L f.)
)
```




```J
   10 A 1:`_1:`_1:`1:`0:
_67
```



## Java


We use anonymous classes to represent closures.

Java Version 8 and up


```java
import java.util.function.DoubleSupplier;

public class ManOrBoy {

    static double A(int k, DoubleSupplier x1, DoubleSupplier x2,
                 DoubleSupplier x3, DoubleSupplier x4, DoubleSupplier x5) {

        DoubleSupplier B = new DoubleSupplier() {
            int m = k;
            public double getAsDouble() {
                return A(--m, this, x1, x2, x3, x4);
            }
        };

        return k <= 0 ? x4.getAsDouble() + x5.getAsDouble() : B.getAsDouble();
    }

    public static void main(String[] args) {
        System.out.println(A(10, () -> 1.0, () -> -1.0, () -> -1.0, () -> 1.0, () -> 0.0));
    }
}
```


Java Version 7


```java
public class ManOrBoy {
    interface Arg {
        public int run();
    }

    public static int A(final int k, final Arg x1, final Arg x2,
                          final Arg x3, final Arg x4, final Arg x5) {
        if (k <= 0)
            return x4.run() + x5.run();
        return new Arg() {
            int m = k;
            public int run() {
                m--;
                return A(m, this, x1, x2, x3, x4);
            }
        }.run();
    }
    public static Arg C(final int i) {
        return new Arg() {
            public int run() { return i; }
        };
    }

    public static void main(String[] args) {
        System.out.println(A(10, C(1), C(-1), C(-1), C(1), C(0)));
    }
}
```



## JavaScript

In Chrome we get a "Maximum call stack size exceeded" when a > 13. In Firefox we get "too much recursion" when a > 12.

```javascript
function a(k, x1, x2, x3, x4, x5) {
  function b() {
    k -= 1;
    return a(k, b, x1, x2, x3, x4);
  }
  return (k > 0) ? b() : x4() + x5();
}

// this uses lambda wrappers around the numeric arguments
function x(n) {
  return function () {
    return n;
  };
}
alert(a(10, x(1), x(-1), x(-1), x(1), x(0)));
```


Implemented using ES6 syntax

```javascript>var x = n =
 () => n;

var a = (k, x1, x2, x3, x4, x5) => {
  var b = () => return a(--k, b, x1, x2, x3, x4); //decrement k before use
  return (k > 0) ? b() : x4() + x5();
};
```



## Jsish

From Javascript entry.

```javascript
/* Knuth's Man or boy test (local references in recursion), in Jsish */
/* As noted, needs a fair sized stack depth, default is 200 in jsish v2.8.24 */
Interp.conf({maxDepth:2048});

function a(k, x1, x2, x3, x4, x5) {
  function b() {
    k -= 1;
    return a(k, b, x1, x2, x3, x4);
  }
  return (k > 0) ? b() : x4() + x5();
}

// this uses lambda wrappers around the numeric arguments
function x(n) {
  return function () {
    return n;
  };
}

puts(a(10, x(1), x(-1), x(-1), x(1), x(0)));

/*
=!EXPECTSTART!=
-67
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u manOrBoyTest.jsi
[PASS] manOrBoyTest.jsi
prompt$ jsish manOrBoyTest.jsi
-67
```



## Julia


```julia
function a(k, x1, x2, x3, x4, x5)
  b = ()-> a(k-=1, b, x1, x2, x3, x4);
  k <= 0 ? (x4() + x5()) : b();
end

println(a(10, ()->1, ()->-1,  ()->-1, ()->1, ()->0));
```



## Kotlin

Using the default JVM stack size, could only get to k = 12 before experiencing an overflow:

```scala
// version 1.1.3

typealias Func = () -> Int

fun a(k: Int, x1: Func, x2: Func, x3: Func, x4: Func, x5: Func): Int {
    var kk = k
    fun b(): Int = a(--kk, ::b, x1, x2, x3, x4)
    return if (kk <= 0) x4() + x5() else b()
}

fun main(args: Array<String>) {
    println(" k  a")
    for (k in 0..12) {
        println("${"%2d".format(k)}: ${a(k, { 1 }, { -1 }, { -1 }, { 1 }, { 0 })}")
    }
}

```


{{out}}

```txt

 k  a
 0: 1
 1: 0
 2: -2
 3: 0
 4: 1
 5: 0
 6: 1
 7: -1
 8: -10
 9: -30
10: -67
11: -138
12: -291

```



## Lox


```lox
fun A (k, xa, xb, xc, xd, xe) {
    print k;
    fun B() {
        k = k - 1;
        return A(k, B, xa, xb, xc, xd);
    }
    if (k <= 0) {
        return xd() + xe();
    } else {
        return B();
    }
}

fun I0()  { return  0; }
fun I1()  { return  1; }
fun I_1() { return -1; }

print A(10, I1, I_1, I_1, I1, I0);
```



## Lua



```lua
function a(k,x1,x2,x3,x4,x5)
  local function b()
    k = k - 1
    return a(k,b,x1,x2,x3,x4)
  end
   if k <= 0 then return x4() + x5() else return b() end
end

function K(n)
  return function()
    return n
  end
end

print(a(10, K(1), K(-1), K(-1), K(1), K(0)))
```



## Mathematica


This ''Mathematica'' code was derived from the Ruby example appearing below.

 $RecursionLimit = 1665; (* anything less fails for k0 = 10 *)

 a[k0_, x1_, x2_, x3_, x4_, x5_] := Module[{k, b },
   k = k0;
   b = (k--; a[k, b, x1, x2, x3, x4]) &;
   If[k <= 0, x4[] + x5[], b[]]]

 a[10, 1 &, -1 &, -1 &, 1 &, 0 &] (* => -67 *)

=={{header|Modula-3}}==


```modula3
MODULE Main;
IMPORT IO;

TYPE Function = PROCEDURE ():INTEGER;

PROCEDURE A(k: INTEGER; x1, x2, x3, x4, x5: Function): INTEGER =

  PROCEDURE B(): INTEGER =
  BEGIN
    DEC(k);
    RETURN A(k, B, x1, x2, x3, x4);
  END B;

BEGIN
  IF k <= 0 THEN
    RETURN x4() + x5();
  ELSE
    RETURN B();
  END;
END A;

PROCEDURE F0(): INTEGER = BEGIN RETURN 0; END F0;
PROCEDURE F1(): INTEGER = BEGIN RETURN 1; END F1;
PROCEDURE Fn1(): INTEGER = BEGIN RETURN -1; END Fn1;

BEGIN
  IO.PutInt(A(10, F1, Fn1, Fn1, F1, F0));
  IO.Put("\n");
END Main.
```



## Nim


```nim
import future

proc a(k: int; x1, x2, x3, x4, x5: proc(): int): int =
  var k = k
  proc b(): int =
    dec k
    a(k, b, x1, x2, x3, x4)
  if k <= 0: x4() + x5()
  else: b()

echo a(10, () => 1, () => -1, () => -1, () => 1, () => 0)
```


=={{header|Objective-C}}==
{{works with|Cocoa|Mac OS X 10.6+}}

```objc>#import <Foundation/Foundation.h


typedef NSInteger (^IntegerBlock)(void);

NSInteger A (NSInteger kParam, IntegerBlock x1, IntegerBlock x2, IntegerBlock x3, IntegerBlock x4, IntegerBlock x5) {
    __block NSInteger k = kParam;
    __block __weak IntegerBlock weak_B;
    IntegerBlock B;
    weak_B = B = ^ {
        return A(--k, weak_B, x1, x2, x3, x4);
    };
    return k <= 0 ? x4() + x5() : B();
}

IntegerBlock K (NSInteger n) {
    return ^{return n;};
}

int main (int argc, const char * argv[]) {
    @autoreleasepool {
        NSInteger result = A(10, K(1), K(-1), K(-1), K(1), K(0));
        NSLog(@"%d\n", result);
    }
    return 0;
}
```


Without ARC, the above should be:

```objc>#import <Foundation/Foundation.h


typedef NSInteger (^IntegerBlock)(void);

NSInteger A (NSInteger kParam, IntegerBlock x1, IntegerBlock x2, IntegerBlock x3, IntegerBlock x4, IntegerBlock x5) {
    __block NSInteger k = kParam;
    __block IntegerBlock B;
    B = ^ {
        return A(--k, B, x1, x2, x3, x4);
    };
    return k <= 0 ? x4() + x5() : B();
}

IntegerBlock K (NSInteger n) {
    return [[^{return n;} copy] autorelease];
}

int main (int argc, const char * argv[]) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSInteger result = A(10, K(1), K(-1), K(-1), K(1), K(0));
    NSLog(@"%d\n", result);
    [pool drain];
    return 0;
}
```



without Blocks or ARC:

```objc>@protocol IntegerFun <NSObject

-(NSInteger)call;
@end

NSInteger A (NSInteger kParam, id<IntegerFun> x1, id<IntegerFun> x2, id<IntegerFun> x3, id<IntegerFun> x4, id<IntegerFun> x5);

@interface B_Class : NSObject <IntegerFun> {
  NSInteger *k;
  id<IntegerFun> x1, x2, x3, x4;
}
-(id)initWithK:(NSInteger *)k x1:(id<IntegerFun>)x1 x2:(id<IntegerFun>)x2 x3:(id<IntegerFun>)x3 x4:(id<IntegerFun>)x4;
@end

@implementation B_Class
-(id)initWithK:(NSInteger *)_k x1:(id<IntegerFun>)_x1 x2:(id<IntegerFun>)_x2 x3:(id<IntegerFun>)_x3 x4:(id<IntegerFun>)_x4 {
  if ((self = [super init])) {
    k = _k;
    x1 = [_x1 retain];
    x2 = [_x2 retain];
    x3 = [_x3 retain];
    x4 = [_x4 retain];
  }
  return self;
}
-(void)dealloc {
  [x1 release];
  [x2 release];
  [x3 release];
  [x4 release];
  [super dealloc];
}
-(NSInteger)call {
  return A(--*k, self, x1, x2, x3, x4);
}
@end

NSInteger A (NSInteger k, id<IntegerFun> x1, id<IntegerFun> x2, id<IntegerFun> x3, id<IntegerFun> x4, id<IntegerFun> x5) {
  id<IntegerFun> B = [[[B_Class alloc] initWithK:&k x1:x1 x2:x2 x3:x3 x4:x4] autorelease];
  return k <= 0 ? [x4 call] + [x5 call] : [B call];
}

@interface K : NSObject <IntegerFun> {
  NSInteger n;
}
-(id)initWithN:(NSInteger)n;
@end

@implementation K
-(id)initWithN:(NSInteger)_n {
  if ((self = [super init])) {
    n = _n;
  }
  return self;
}
-(NSInteger)call {
  return n;
}
@end

int main(int argc, const char *argv[]) {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSInteger result = A(10,
                       [[[K alloc] initWithN:1] autorelease],
                       [[[K alloc] initWithN:-1] autorelease],
                       [[[K alloc] initWithN:-1] autorelease],
                       [[[K alloc] initWithN:1] autorelease],
                       [[[K alloc] initWithN:0] autorelease]);
  NSLog(@"%ld\n", result);

  [pool release];
  return 0;
}
```



## Objeck

Using anonymous classes instead of closures

```objeck
interface Arg {
  method : virtual : public : Run() ~ Int;
}

class ManOrBoy {
  New() {}

  function : A(mb : ManOrBoy, k : Int, x1 : Arg, x2 : Arg, x3 : Arg, x4 : Arg, x5 : Arg) ~ Int {
    if(k <= 0) {
            return x4->Run() + x5->Run();
    };

    return Base->New(mb, k, x1, x2, x3, x4) implements Arg {
      @mb : ManOrBoy; @k : Int; @x1 : Arg; @x2 : Arg; @x3 : Arg; @x4 : Arg; @m : Int;

      New(mb : ManOrBoy, k : Int, x1 : Arg, x2 : Arg, x3 : Arg, x4 : Arg) {
        @mb := mb; @k := k; @x1 := x1; @x2 := x2; @x3 := x3; @x4 := x4; @m := @k;
      }

      method : public : Run() ~ Int {
        @m -= 1;
        return @mb->A(@mb, @m, @self, @x1, @x2, @x3, @x4);
      }
    }->Run();
  }

  function : C(i : Int) ~ Arg {
    return Base->New(i) implements Arg {
      @i : Int;
      New(i : Int) {
        @i := i;
      }

      method : public : Run() ~ Int {
        return @i;
      }
    };
  }

  function : Main(args : String[]) ~ Nil {
    mb := ManOrBoy->New();
    mb->A(mb, 10, C(1), C(-1), C(-1), C(1), C(0))->PrintLine();
  }
}

```



## OCaml


OCaml variables are not mutable, so "k" is wrapped in a mutable object, which we access through a reference type called "ref".


```ocaml
let rec a k x1 x2 x3 x4 x5 =
  if k <= 0 then
    x4 () + x5 ()
  else
    let m = ref k in
    let rec b () =
      decr m;
      a !m b x1 x2 x3 x4
    in
    b ()

let () =
  Printf.printf "%d\n" (a 10 (fun () -> 1) (fun () -> -1) (fun () -> -1) (fun () -> 1) (fun () -> 0))
```



## Ol

Ol designed as purely functional language, so such 'tricks' with side effects are not allowed.

But! For some reasons in version 1.2 a very limited mutators (set-ref!, set-car!, set-cdr!) are added; so this task can be implemented as usual. Please, be aware that mutators receives only values (small numbers, constants) or previously (before the mutating dest) declared objects.


```scheme

; Because argument "k" is a small number, it's a value, not an object.
; So we must 'pack' it in object - 'box' it; And 'unbox' when we try to get value.

(define (box x)  (list x))
(define (unbox x) (car x))
(define (copy x) (box (unbox x)))

(define (A k x1 x2 x3 x4 x5)
   (define (B)
      (set-car! k (- (unbox k) 1))
      (A (copy k) B x1 x2 x3 x4))

   (if (<= (unbox k) 0)
      (+ (x4) (x5))
      (B)))

(define (man-or-boy N)
   (A (box N)
      (lambda ()  1)
      (lambda () -1)
      (lambda () -1)
      (lambda ()  1)
      (lambda () 0)))

(print (man-or-boy 10))
(print (man-or-boy 15))
(print (man-or-boy 20))

```

Output:

```txt

-67
-3250
-175416

```




## Oz

We emulate the ALGOL60 example as closely as possible. Like most of the examples, we use functions to emulate call-by-name.

Oz variables are immutable, so we use a mutable reference ("cell") for K. The ALGOL example uses call-by-value for K. Oz uses call-by-reference, therefore we copy K explicitly when we call A recursively.

We use explicit "return variables" to emulate the strange behaviour of the ALGOL B procedure which assigns a value to A's return value.


```oz
declare
  fun {A K X1 X2 X3 X4 X5}
     ReturnA = {NewCell undefined}
     fun {B}
        ReturnB = {NewCell undefined}
     in
        K := @K - 1
        ReturnA := {A {NewCell @K} B X1 X2 X3 X4}
        ReturnB := @ReturnA
        @ReturnB
     end
  in
     if @K =< 0 then ReturnA := {X4} + {X5} else _ = {B} end
     @ReturnA
  end

  fun {C V}
     fun {$} V end
  end
in
  {Show {A {NewCell 10} {C 1} {C ~1} {C ~1} {C 1} {C 0}}}
```



## Pascal



```pascal
program manorboy(output);

function zero: integer; begin zero := 0 end;
function one: integer; begin one := 1 end;
function negone: integer; begin negone := -1 end;

function A(
  k: integer;
  function x1: integer;
  function x2: integer;
  function x3: integer;
  function x4: integer;
  function x5: integer
): integer;

  function B: integer;
  begin k := k - 1;
        B := A(k, B, x1, x2, x3, x4)
  end;

begin if k <= 0 then A := x4 + x5 else A := B
end;

begin writeln(A(10, one, negone, negone, one, zero))
end.
```



## Perl


```perl
sub A {
    my ($k, $x1, $x2, $x3, $x4, $x5) = @_;
    my($B);
    $B = sub { A(--$k, $B, $x1, $x2, $x3, $x4) };
    $k <= 0 ? &$x4 + &$x5 : &$B;
}

print A(10, sub{1}, sub {-1}, sub{-1}, sub{1}, sub{0} ), "\n";
```

<!-- $B must be declared before it is used. Otherwise it references the global variable
     $B.
     An entertaining variation on the last line is:
     print A(10, map {my $p = \$_; sub{$$p}} 1, -1, -1, 1, 0), "\n";
-->


## Perl 6

This solution avoids creating the closure B if $k <= 0 (that is, nearly every time).

```perl6
sub A($k is copy, &x1, &x2, &x3, &x4, &x5) {
    $k <= 0
        ?? x4() + x5()
        !! (my &B = { A(--$k, &B, &x1, &x2, &x3, &x4) })();
};

say A(10, {1}, {-1}, {-1}, {1}, {0});
```

{{out}}

```txt
-67
```



## Phix

Ugh. Phix does not allow this sort of nonsense implicitly, so you have to get a bit <s>dirty</s> creative.

Explicitly allocates space (which is automatically freed) for the various "k contexts". Manages up to k=23 in about 10s, but crashes on k=24.

```Phix
forward function A(integer k, object x1, x2, x3, x4, x5)

function B(sequence s)
    object {kptr,x1,x2,x3,x4} = s
    integer k = peek4s(kptr)-1
    poke4(kptr,k)
    return A(k,{kptr,x1,x2,x3,x4},x1,x2,x3,x4)
end function

function A(integer k, object x1, x2, x3, x4, x5)
    if k<=0 then
        return iff(sequence(x4)?B(x4):x4)+
               iff(sequence(x5)?B(x5):x5)
    end if
    atom kptr = allocate(4,1)
    poke4(kptr,k)
    return B({kptr,x1,x2,x3,x4})
end function

for k=0 to 10 do
    ?{"k=",k,A(k,1,-1,-1,1,0)}
end for
```

{{out}}

```txt

{"k=",0,1}
{"k=",1,0}
{"k=",2,-2}
{"k=",3,0}
{"k=",4,1}
{"k=",5,0}
{"k=",6,1}
{"k=",7,-1}
{"k=",8,-10}
{"k=",9,-30}
{"k=",10,-67}

```



## PHP

{{works with|PHP|5.3+}}

```php
<?php
function A($k,$x1,$x2,$x3,$x4,$x5) {
    $b = function () use (&$b,&$k,$x1,$x2,$x3,$x4) {
        return A(--$k,$b,$x1,$x2,$x3,$x4);
    };
    return $k <= 0 ? $x4() + $x5() : $b();
}

echo A(10, function () { return  1; },
           function () { return -1; },
           function () { return -1; },
           function () { return  1; },
           function () { return  0; }) . "\n";
?>
```


{{works with|PHP|pre-5.3 and 5.3+}}

```php
<?php
function A($k,$x1,$x2,$x3,$x4,$x5) {
  static $i = 0;
  $b = "myfunction_$i";
  $i++;
  eval('function '.$b.'() {
    static $k = '.$k.';
    return A(--$k, '.var_export($b,true).',
             '.var_export($x1,true).',
             '.var_export($x2,true).',
             '.var_export($x3,true).',
             '.var_export($x4,true).');
  }');
  return $k <= 0 ? $x4() + $x5() : $b();
}

echo A(10, create_function('', 'return  1;'),
           create_function('', 'return -1;'),
           create_function('', 'return -1;'),
           create_function('', 'return  1;'),
           create_function('', 'return  0;')) . "\n";
?>
```



## PicoLisp

As PicoLisp uses exclusively shallow dynamic binding, stack frames have to be
explicitly constructed.

```PicoLisp
(de a (K X1 X2 X3 X4 X5)
   (let (@K (cons K)  B (cons))  # Explicit frame
      (set B
         (curry (@K B X1 X2 X3 X4) ()
            (a (dec @K) (car B) X1 X2 X3 X4) ) )
      (if (gt0 (car @K)) ((car B)) (+ (X4) (X5))) ) )

(a 10 '(() 1) '(() -1) '(() -1) '(() 1) '(() 0))
```

Output:

```txt
-> -67
```



## PL/I

 morb: proc options (main) reorder;
  dcl sysprint file;

  put skip list(a((10), lambda1, lambdam1, lambdam1, lambda0, lambda0));

  a: proc(k, x1, x2, x3, x4, x5) returns(fixed bin (31)) recursive;
    dcl k                    fixed bin (31);
    dcl (x1, x2, x3, x4, x5) entry returns(fixed bin (31));

    b: proc returns(fixed bin(31)) recursive;
      k = k - 1;
      return(a((k), b, x1, x2, x3, x4));
    end b;

    if k <= 0 then
      return(x4 + x5);
    else
      return(b);
  end a;

  lambdam1: proc returns(fixed bin (31)); return(-1); end lambdam1;
  lambda0:  proc returns(fixed bin (31)); return(1);  end lambda0;
  lambda1:  proc returns(fixed bin (31)); return(1);  end lambda1;
 end morb;

The above PL/I code has been tested on OS PL/I V2.3.0, Enterprise PL/I V3R9M0 and PL/I for Windows V8.0. The limit for OS PL/I on a z/OS machine with 4Gb seems to be A=15, the limit for Enterprise PL/I on the same machine seems to be A=23, and the limit for PL/I for Windows on a 16Gb system seems to be A=26.

The «Russian» compiler (that is based on Kildall’s compiler PL/I-86) produced the best results. However, two tricks were used there: a) hardware stack pointer was set directly to allocated memory by quasi-assembler’s instruction; b) stack of parameters was replaced by array of parameters and contexts. The result is A=27 for Win32 (Windows-XP) and A=31 for Win64 (Windows-7). Source code test for Win32 see: http://rsdn.org/article/pl1/PL1ex7/pl1ex7.xml
In source code test for Win64 FIXED(31) was replaced by FIXED(63) and pseudo-variable ?ESP by ?RSP.


## Pop11


```txt

define A(k, x1, x2, x3, x4, x5);
    define B();
        k - 1 -> k;
        A(k, B, x1, x2, x3, x4)
    enddefine;
    if k <= 0 then
        x4() + x5()
    else
        B()
    endif
enddefine;

define one(); 1 enddefine;
define minus_one(); -1 enddefine;
define zero(); 0 enddefine;
A(10, one, minus_one, minus_one, one, zero) =>

```



## Python

{{works with|Python|2.5}}

```python
#!/usr/bin/env python
import sys
sys.setrecursionlimit(1025)

def a(in_k, x1, x2, x3, x4, x5):
    k = [in_k]
    def b():
        k[0] -= 1
        return a(k[0], b, x1, x2, x3, x4)
    return x4() + x5() if k[0] <= 0 else b()

x = lambda i: lambda: i
print(a(10, x(1), x(-1), x(-1), x(1), x(0)))

```

A better-looking alternative to using lists as storage are function attributes:

```python
#!/usr/bin/env python
import sys
sys.setrecursionlimit(1025)

def a(k, x1, x2, x3, x4, x5):
    def b():
        b.k -= 1
        return a(b.k, b, x1, x2, x3, x4)
    b.k = k
    return x4() + x5() if b.k <= 0 else b()

x = lambda i: lambda: i
print(a(10, x(1), x(-1), x(-1), x(1), x(0)))

```


Output:
 -67


###  Py3k

{{works with|Python|3.0}}


```python
#!/usr/bin/env python
import sys
sys.setrecursionlimit(1025)

def A(k, x1, x2, x3, x4, x5):
    def B():
        nonlocal k
        k -= 1
        return A(k, B, x1, x2, x3, x4)
    return x4() + x5() if k <= 0 else B()

print(A(10, lambda: 1, lambda: -1, lambda: -1, lambda: 1, lambda: 0))
```



## R

Like many implementations this uses lambda wrappers around the numeric arguments and explicit function calls in the x4() + x5() step to force the order of evaluation and handle value/call duality.


```R
n <- function(x) function()x

A <- function(k, x1, x2, x3, x4, x5) {
  B <- function() A(k <<- k-1, B, x1, x2, x3, x4)
  if (k <= 0) x4() + x5() else B()
}

A(10, n(1), n(-1), n(-1), n(1), n(0))
```


That is the way any sane person would implement Man-or-Boy. However, we can be a bit more evil than that. Here <tt>call.by.name</tt> is a function that rewrites the function definition given as its input:


```r
call.by.name <- function(...) {
  cl <- as.list(match.call())
  sublist <- lapply(cl[2:(length(cl)-1)],
                    function(name) substitute(substitute(evalq(.,.caller),
                                                         list(.=substitute(name))),
                                              list(name=name)))
  names(sublist) <- enquote(cl[2:(length(cl)-1)])
  subcall <- do.call("call", c("list", lapply(sublist, enquote)))
  fndef <- cl[[length(cl)]]
  fndef[[3]] <- substitute({
    .caller <- parent.frame()
    eval(substitute(body, subcall))
  }, list(body=fndef[[3]], subcall=subcall))
  eval.parent(fndef)
}
```


allowing us to write A in a way that mirrors ALGOL60 semantics closely:


```R
A <- call.by.name(x1, x2, x3, x4, x5,
  function(k, x1, x2, x3, x4, x5) {
    Aout <- NULL
    B <- function() {
      k <<- k - 1
      Bout <- Aout <<- A(k, B(), x1, x2, x3, x4)
    }
    if (k <= 0) Aout <- x4 + x5 else B()
    Aout
  }
)
```


One has to increase the recursion limit a bit, but it gives correct answers:


```r>
 options(expressions=10000)
> mapply(A, 0:10, 1, -1, -1, 1, 0)
 [1]   1   0  -2   0   1   0   1  -1 -10 -30 -67
```


If you inspect <tt>A</tt> without the original source you will see what has happened: <tt>call.by.name</tt> rewrote A so that it looks like this:


```r>
 print(A, useSource=FALSE)
function (k, x1, x2, x3, x4, x5)
{
    .caller <- parent.frame()
    eval(substitute({
        Aout <- NULL
        B <- function() {
            k <<- k - 1
            Bout <- Aout <<- A(k, B(), x1, x2, x3, x4)
        }
        if (k <= 0) Aout <- x4 + x5 else B()
        Aout
    }, list(x1 = substitute(evalq(., .caller), list(. = substitute(x1))),
        x2 = substitute(evalq(., .caller), list(. = substitute(x2))),
        x3 = substitute(evalq(., .caller), list(. = substitute(x3))),
        x4 = substitute(evalq(., .caller), list(. = substitute(x4))),
        x5 = substitute(evalq(., .caller), list(. = substitute(x5))))))
}
```


That is, instead of evaluating its arguments normally, <tt>A</tt> captures their original expressions, and instead of evaluating its body normally, <tt>A</tt> substitutes calls to <tt>evalq</tt> the captured argument expressions in the calling frame. After a few levels of recursion this way, you end up evaluating expressions like <tt>A(k, B(), evalq(B(), .caller), evalq(evalq(B(), .caller), .caller), evalq(evalq(evalq(1, .caller), .caller), .caller), evalq(evalq(evalq(-1, .caller), .caller), .caller))</tt>, so this is not very efficient, but works.


## Racket


Copied from Scheme, works fine:


```Racket
#lang racket

(define (A k x1 x2 x3 x4 x5)
  (define (B)
    (set! k (- k 1))
    (A k B x1 x2 x3 x4))
  (if (<= k 0)
      (+ (x4) (x5))
      (B)))

(A 10 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))
```



## REXX

The REXX language only passes by value, not by name.   However, there is a way to treat passed arguments as names.

However, using the code below, it only works for   '''n'''   up to (and including)   '''3'''.

```rexx
/*REXX program performs the  "man or boy"  test as far as possible for  N.              */
     do n=0                                      /*increment  N  from  zero  forever.   */
     say 'n='n   a(N,x1,x2,x3,x4,x5)             /*display the result to the terminal.  */
     end  /*n*/                                  /* [↑]  do until something breaks.     */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
a:   procedure; parse arg k, x1, x2, x3, x4, x5
     if k<=0  then return f(x4) + f(x5)
              else return f(b)
/*──────────────────────────────────────────────────────────────────────────────────────*/
b:   k=k-1;                            return  a(k, b, x1, x2, x3, x4)
f:   interpret  'v='  arg(1)"()";      return  v
x1:  procedure;                        return  1
x2:  procedure;                        return -1
x3:  procedure;                        return -1
x4:  procedure;                        return  1
x5:  procedure;                        return  0
```

'''output'''

```txt

n=0 1
n=1 0
n=2 -2
n=3 0

```



## Ruby


Note: the lambda call can be replaced with Proc.new and still work.

```ruby
def a(k, x1, x2, x3, x4, x5)
  b = lambda { k -= 1; a(k, b, x1, x2, x3, x4) }
  k <= 0 ? x4[] + x5[] : b[]
end

puts a(10, lambda {1}, lambda {-1}, lambda {-1}, lambda {1}, lambda {0})
```




## Rust



```rust
use std::cell::Cell;

trait Arg {
    fn run(&self) -> i32;
}

impl Arg for i32 {
    fn run(&self) -> i32 { *self }
}

struct B<'a> {
    k: &'a Cell<i32>,
    x1: &'a Arg,
    x2: &'a Arg,
    x3: &'a Arg,
    x4: &'a Arg,
}

impl<'a> Arg for B<'a> {
    fn run(&self) -> i32 {
        self.k.set(self.k.get() - 1);
        a(self.k.get(), self, self.x1, self.x2, self.x3, self.x4)
    }
}

fn a(k: i32, x1: &Arg, x2: &Arg, x3: &Arg, x4: &Arg, x5: &Arg) -> i32 {
    if k <= 0 {
        x4.run() + x5.run()
    } else {
        B{
            k: &Cell::new(k),
            x1, x2, x3, x4
        }.run()
    }
}

pub fn main() {
    println!("{}", a(10, &1, &-1, &-1, &1, &0));
}
```


Another solution where we have B take itself as an argument in order to recursively call itself:


```rust
use std::cell::Cell;

fn a(k: i32, x1: &Fn() -> i32, x2: &Fn() -> i32, x3: &Fn() -> i32, x4: &Fn() -> i32, x5: &Fn() -> i32) -> i32 {
    let k1 = Cell::new(k);
    struct B<'a> { f: &'a Fn(&B) -> i32 }
    let b = B {
        f: &|b| {
            k1.set(k1.get() - 1);
            return a(k1.get(), &||(b.f)(b), x1, x2, x3, x4)
        }
    };
    let b = ||(b.f)(&b);
    return if k <= 0 {x4() + x5()} else {b()}
}

pub fn main() {
    println!("{}", a(10, &||1, &||-1, &||-1, &||1, &||0));
}
```


Another solution that gives a reference-counted function a weak reference to itself:

{{trans|Objective-C}}

```rust
use std::cell::Cell;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

fn a(k: i32, x1: &Fn() -> i32, x2: &Fn() -> i32, x3: &Fn() -> i32, x4: &Fn() -> i32, x5: &Fn() -> i32) -> i32 {
    let weak_holder: Rc<RefCell<Weak<Fn() -> i32>>> = Rc::new(RefCell::new(Weak::<fn() -> i32>::new()));
    let weak_holder2 = weak_holder.clone();
    let k_holder = Cell::new(k);
    let b: Rc<Fn() -> i32> = Rc::new(move || {
        let b = weak_holder2.borrow().upgrade().unwrap();
        k_holder.set(k_holder.get() - 1);
        return a(k_holder.get(), &*b, x1, x2, x3, x4);
    });
    weak_holder.replace(Rc::downgrade(&b));

    return if k <= 0 {x4() + x5()} else {b()}
}

pub fn main() {
    println!("{}", a(10, &||1, &||-1, &||-1, &||1, &||0));
}
```



## Scala


```scala
def A(in_k: Int, x1: =>Int, x2: =>Int, x3: =>Int, x4: =>Int, x5: =>Int): Int = {
    var k = in_k
    def B: Int = {
        k = k-1
        A(k, B, x1, x2, x3, x4)
    }
    if (k<=0) x4+x5 else B
}
println(A(10, 1, -1, -1, 1, 0))
```



## Sidef


```ruby
func a(k, x1, x2, x3, x4, x5) {
    func b { a(--k, b, x1, x2, x3, x4) };
    k <= 0 ? (x4() + x5()) : b();
}
say a(10, ->{1}, ->{-1}, ->{-1}, ->{1}, ->{0});      #=> -67
```


This solution avoids creating the closure b if k <= 0 (that is, nearly every time).

```ruby
func a(k, x1, x2, x3, x4, x5) {
    k <= 0 ? (x4() + x5())
           : func b { a(--k, b, x1, x2, x3, x4) }();
}
say a(10, ->{1}, ->{-1}, ->{-1}, ->{1}, ->{0});      #=> -67
```


Alternatively, we can implement it as a class also:

```ruby
class MOB {
    method a(k, x1, x2, x3, x4, x5) {
        func b { self.a(--k, b, x1, x2, x3, x4) };
        k <= 0 ? (x4() + x5()) : b();
    }
}

var obj = MOB();
say obj.a(10, ->{1}, ->{-1}, ->{-1}, ->{1}, ->{0});
```


=={{header|Snap!}}==

[[File:snap-man-or-boy.png]]


## Scheme



```scheme
(define (A k x1 x2 x3 x4 x5)
  (define (B)
    (set! k (- k 1))
    (A k B x1 x2 x3 x4))
  (if (<= k 0)
      (+ (x4) (x5))
      (B)))

(A 10 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))
```


=={{header|Smalltalk}} ==

 Number>>x1: x1 x2: x2 x3: x3 x4: x4 x5: x5
    | b k |
    k := self.
    b := [ k := k - 1. k x1: b x2: x1 x3: x2 x4: x3 x5: x4 ].
    ^k <= 0 ifTrue: [ x4 value + x5 value ] ifFalse: b

 10 x1: [1] x2: [-1] x3: [-1] x4: [1] x5: [0]


## Sparkling

Sparkling does not directly support modifying external local variables. To work around this limitation, we wrap the <tt>k</tt> variable in an array, which is mutable.


```sparkling
function a(k, x1, x2, x3, x4, x5) {
	let kk = { "k": k.k };
	let b = function b() {
		kk.k--;
		return a(kk, b, x1, x2, x3, x4);
	};
	return kk.k <= 0 ? x4() + x5() : b();
}

function x(n) {
	return function () {
		return n;
	};
}

print(a({ "k": 10 }, x(1), x(-1), x(-1), x(1), x(0)));
```



## Standard ML


Standard ML variables are not mutable, so "k" is wrapped in a mutable object, which we access through a reference type called "ref".


```sml
fun a (k, x1, x2, x3, x4, x5) =
  if k <= 0 then
    x4 () + x5 ()
  else let
    val m = ref k
    fun b () = (
      m := !m - 1;
      a (!m, b, x1, x2, x3, x4)
    )
  in
    b ()
  end

val () =
  print (Int.toString (a (10, fn () => 1, fn () => ~1, fn () => ~1, fn () => 1, fn () => 0)) ^ "\n")
```



## Swift

{{works with|Swift|3.x+}}

As of Swift 3.0, closure parameters are "non-escaping" by default.  The Man or Boy Test requires the closures to be escaping, and thus we must now annotate the closure parameters with the "@escaping" attribute.


```swift
func A(_ k: Int,
       _ x1: @escaping () -> Int,
       _ x2: @escaping () -> Int,
       _ x3: @escaping () -> Int,
       _ x4: @escaping () -> Int,
       _ x5: @escaping () -> Int) -> Int {
    var k1 = k

    func B() -> Int {
        k1 -= 1
        return A(k1, B, x1, x2, x3, x4)
    }

    if k1 <= 0 {
        return x4() + x5()
    } else {
        return B()
    }
}

print(A(10, {1}, {-1}, {-1}, {1}, {0}))
```

{{works with|Swift|2.x}}

```swift
func A(k: Int, _ x1: () -> Int, _ x2: () -> Int, _ x3: () -> Int, _ x4: () -> Int, _ x5: () -> Int) -> Int {
    var k1 = k
    func B() -> Int {
        k1-=1
        return A(k1, B, x1, x2, x3, x4)
    }
    if k1 <= 0 {
        return x4() + x5()
    } else {
        return B()
    }
}

print(A(10, {1}, {-1}, {-1}, {1}, {0}))
```

{{works with|Swift|1.x}}

```swift
func A(var k: Int, x1: () -> Int, x2: () -> Int, x3: () -> Int, x4: () -> Int, x5: () -> Int) -> Int {
  var B: (() -> Int)!
  B = {
    k--
    return A(k, B, x1, x2, x3, x4)
  }
  if k <= 0 {
    return x4() + x5()
  } else {
    return B()
  }
}

println(A(10, {1}, {-1}, {-1}, {1}, {0}))
```


=={{header|Tcl}} ==

There are two nontrivial features in the "man or boy" test. One is that the parameters ''x1'' though ''x5'' are in general going to be function calls that don't get evaluated until their values are needed for the addition in procedure A, which means that these in Tcl are going to be scripts, and therefore it is necessary to introduce a helper procedure C that returns a constant value. The other is that procedure B needs to refer to variables in the local context of its "parent" instance of procedure A. This is precisely what the '''upvar''' core command does, but the ''absolute'' target level needs to be embedded into the script that performs the delayed call to procedure B ('''upvar''' is more often used with relative levels).

```tcl
proc A {k x1 x2 x3 x4 x5} {
    expr {$k<=0 ? [eval $x4]+[eval $x5] : [B \#[info level]]}
}
proc B {level} {
    upvar $level k k x1 x1 x2 x2 x3 x3 x4 x4
    incr k -1
    A $k [info level 0] $x1 $x2 $x3 $x4
}
proc C {val} {return $val}
interp recursionlimit {} 1157
A 10 {C 1} {C -1} {C -1} {C 1} {C 0}
```


The <tt>[info level 0]</tt> here is a sort of "self" idiom; it returns the command (with arguments) that called the current procedure.

Since the values of ''x1'' through ''x4'' are never modified, it is also possible to embed these as parameters of B, thereby slightly purifying the program:

```tcl
proc AP {k x1 x2 x3 x4 x5} {expr {$k<=0 ? [eval $x4]+[eval $x5] : [BP \#[info level] $x1 $x2 $x3 $x4]}}
proc BP {level x1 x2 x3 x4} {AP [uplevel $level {incr k -1}] [info level 0] $x1 $x2 $x3 $x4}
proc C {val} {return $val}
interp recursionlimit {} 1157
AP 10 {C 1} {C -1} {C -1} {C 1} {C 0}
```



## TSQL


SQL is kinda limited, and TSQL is not much better. Unfortunately it fails the Man test due to Stack Level being limited to 32.

```tsql

CREATE PROCEDURE dbo.LAMBDA_WRAP_INTEGER

	@v INT

AS
	DECLARE	@name NVARCHAR(MAX) = 'LAMBDA_' + UPPER(REPLACE(NEWID(), '-', '_'))
	DECLARE @SQL NVARCHAR(MAX) = '
			CREATE PROCEDURE dbo.' + @name + '
				AS

			RETURN ' + CAST(@v AS NVARCHAR(MAX))

	EXEC(@SQL)
	RETURN OBJECT_ID(@name)
GO

CREATE PROCEDURE dbo.LAMBDA_EXEC
	@id INT

AS
	DECLARE @name SYSNAME = OBJECT_NAME(@id)
	,	@retval INT
	EXEC	@retval = @name
	RETURN	@retval
GO

-- B-procedure
CREATE PROCEDURE dbo.LAMBDA_B
	@name_out SYSNAME OUTPUT
,	@q INT
AS
BEGIN

	DECLARE	@SQL NVARCHAR(MAX)
	,	@name NVARCHAR(MAX) = 'LAMBDA_B_' + UPPER(REPLACE(NEWID(), '-', '_'))

	SELECT	@SQL = N'
		CREATE PROCEDURE dbo.' + @name + N'

			AS

		DECLARE @retval INT, @k INT, @x1 INT, @x2 INT, @x3 INT, @x4 INT

		SELECT	@k = k - 1, @x1 = x1, @x2 = x2,	@x3 = x3, @x4 = x4
		FROM	#t_args t
		WHERE	t.i = ' + CAST(@q AS NVARCHAR(MAX)) + '

		UPDATE	t
		SET	k = k -1
		FROM	#t_args t
		WHERE	t.i = ' + CAST(@q AS NVARCHAR(MAX)) + '

		EXEC	@retval = LAMBDA_A @k, @@PROCID, @x1, @x2, @x3, @x4
		RETURN @retval'
	EXEC(@SQL)

	SELECT	@name_out = @name
END

GO
-- A-procedure
CREATE PROCEDURE dbo.LAMBDA_A
(

	@k INT
,	@x1 INT
,	@x2 INT
,	@x3 INT
,	@x4 INT
,	@x5 INT
)
AS
	SET NOCOUNT ON;
	DECLARE @res1 INT
	,	@res2 INT
	,	@Name SYSNAME
	,	@q INT

	-- First add the arguments to the "stack"
	INSERT INTO #t_args (k,	x1, x2, x3, x4, x5
	)
	SELECT	@k, @x1, @x2, @x3, @x4, @x5

	SELECT	@q = SCOPE_IDENTITY()

	IF @k <= 0
	BEGIN
		EXEC	@res1 = dbo.LAMBDA_EXEC @x4
		EXEC	@res2 = dbo.LAMBDA_EXEC @x5
		RETURN	@res1 + @res2
	END
	ELSE
	BEGIN
		EXEC	dbo.LAMBDA_B @name_out = @Name OUTPUT, @q = @q
		EXEC	@res1 = @Name
		RETURN	@res1
	END

GO

-------------------------------------------------------------
-- Test script
-------------------------------------------------------------

DECLARE	@x1 INT
,	@x2 INT
,	@x0 INT
,	@x4 INT
,	@x5 INT
,	@K INT
,	@retval INT

-------------------------------------------------------------
-- Create wrapped integers to pass as arguments
-------------------------------------------------------------
EXEC	@x1 = LAMBDA_WRAP_INTEGER 1
EXEC	@x2 = LAMBDA_WRAP_INTEGER -1
EXEC	@x0 = LAMBDA_WRAP_INTEGER 0

-------------------------------------------------------------
-- Argument storage table
-------------------------------------------------------------
CREATE TABLE #t_args (
	k INT
,	x1 INT
,	x2 INT
,	x3 INT
,	x4 INT
,	x5 INT
,	i INT IDENTITY
)

SELECT	@K = 1

-- Anything above 5 blows up the stack
WHILE 	@K <= 4
BEGIN
	EXEC	@retval = dbo.LAMBDA_A @K, @x1, @x2, @x2, @x1, @x0
	PRINT	'For k=' + CAST(@K AS VARCHAR) + ', result=' + CAST(@retval AS VARCHAR)

	SELECT	@K = @K + 1
END


```


Outputs:
For k=1, result=0
For k=2, result=-2
For k=3, result=0
For k=4, result=1



## TXR


The goal in this solution is to emulate the Algol 60 solution as closely as possible, and not merely get the correct result. For that, we could just crib the Common Lisp or Scheme solution, with more succinct syntax, like this:


```txrlisp
(defun A (k x1 x2 x3 x4 x5)
  (labels ((B ()
             (dec k)
             [A k B x1 x2 x3 x4]))
    (if (<= k 0) (+ [x4] [x5]) (B))))

(prinl (A 10 (ret 1) (ret -1) (ret -1) (ret 1) (ret 0)))
```


To do a proper job, we define a call-by-name system as a set of functions and macros. With these, the function <code>A</code> can be defined as a close transliteration of the Algol, as can the call to <code>A</code> with the integer constants:


```txrlisp
(defun-cbn A (k x1 x2 x3 x4 x5)
  (let ((k k))
    (labels-cbn (B ()
                  (dec k)
                  (set B (set A (A k (B) x1 x2 x3 x4))))
      (if (<= k 0)
        (set A (+ x4 x5))
        (B))))) ;; value of (B) correctly discarded here!

(prinl (A 10 1 -1 -1 1 0))
```


We define the global function with <code>defun-cbn</code> ("cbn" stands for "call by name") and the inner function with <code>labels-cbn</code>. These functions are actually macros which call hidden call-by-value functions. The macros create all the necessary thunks out of their argument expressions, and the hidden functions use local macros to provide transparent access to their arguments from their bodies.

Even the fact that a return value is established by an assignment to the function name is simulated. Note that in <code>A</code> and <code>B</code>, we must assign to the variables <code>A</code> and <code>B</code> respectively to establish the return value. This in turn allows the faithful rendition of the detail in the original that the <code>if</code> form discards the value of the call to <code>B</code>. Establishing a return value by assignment, as in Algol, is achieved thanks to the Lisp-2 base of TXR Lisp; we can simultaneously bind a symbol to a function and variable in the same scope.

Also, <code>k</code> is treated as a call-by-name argument also, and is explicitly subject to a rebinding inside <code>A</code>, as is apparently the case in the Algol code. This detail is necessary; if we do not rebind <code>k</code>, then it is a by-name reference to the caller's <code>k</code>, which is a by-name reference to its caller's <code>k</code> and so on.

Call-by-name is achieved by representing arguments as structure objects that hold get/set lambdas, serving as access thunks, hidden behind macros. These thunks allow two-way access: the passed values can be stored, not only accessed. This creates a problem when the actual arguments are constants or function calls; that is solved.  Constants are recognized and re-bound to hidden variables, which are passed in their place. Function calls are passed as thunks configured to reject store attempts with a run-time error.

The complete code follows:


```txrlisp
(defstruct (cbn-thunk get set) nil get set)

(defmacro make-cbn-val (place)
  (with-gensyms (nv tmp)
    (cond
      ((constantp place)
        ^(let ((,tmp ,place))
           (new cbn-thunk
             get (lambda () ,tmp)
             set (lambda (,nv) (set ,tmp ,nv)))))
      ((bindable place)
        ^(new cbn-thunk
           get (lambda () ,place)
           set (lambda (,nv) (set ,place ,nv))))
      (t
        ^(new cbn-thunk
           get (lambda () ,place)
           set (lambda (ign) (error "cannot set ~s" ',place)))))))

(defun cbn-val (cbs)
  (call cbs.get))

(defun set-cbn-val (cbs nv)
  (call cbs.set nv))

(defplace (cbn-val thunk) body
  (getter setter
    (with-gensyms (thunk-tmp)
      ^(rlet ((,thunk-tmp ,thunk))
         (macrolet ((,getter () ^(cbn-val ,',thunk-tmp))
                    (,setter (val) ^(set-cbn-val ,',thunk-tmp ,val)))
       ,body)))))

(defun make-cbn-fun (sym args . body)
  (let ((gens (mapcar (ret (gensym)) args)))
    ^(,sym ,gens
       (symacrolet ,[mapcar (ret ^(,@1 (cbn-val ,@2))) args gens]
         ,*body))))

(defmacro cbn (fun . args)
  ^(call (fun ,fun) ,*[mapcar (ret ^(make-cbn-val ,@1)) args]))

(defmacro defun-cbn (name (. args) . body)
  (with-gensyms (hidden-fun)
    ^(progn
       (defun ,hidden-fun ())
       (defmacro ,name (. args) ^(cbn ,',hidden-fun ,*args))
       (set (symbol-function ',hidden-fun)
            ,(make-cbn-fun 'lambda args
                           ^(block ,name (let ((,name)) ,*body ,name)))))))

(defmacro labels-cbn ((name (. args) . lbody) . body)
  (with-gensyms (hidden-fun)
    ^(macrolet ((,name (. args) ^(cbn ,',hidden-fun ,*args)))
       (labels (,(make-cbn-fun hidden-fun args
                               ^(block ,name (let ((,name)) ,*lbody ,name))))
         ,*body))))

(defun-cbn A (k x1 x2 x3 x4 x5)
  (let ((k k))
    (labels-cbn (B ()
                  (dec k)
                  (set B (set A (A k (B) x1 x2 x3 x4))))
      (if (<= k 0)
        (set A (+ x4 x5))
        (B))))) ;; value of (B) correctly discarded here!

(prinl (A 10 1 -1 -1 1 0))
```


=={{header|Visual Prolog}} ==

Visual Prolog (like any other Prolog) does not allow variables to be changed.  But behavior can easily be mimicked by using a '''varM''' (modifiable variable), which is actually an object containing a value of the relevant type in a modifiable entity (a so called fact variable).  Secondly, anonymous function (lambda-expression) cannot be recursive, but this is mimicked by using yet a '''varM''' to hold the function.

(Token coloring of Visual Prolog in this wiki is unfortunately wrong, because styles are used across languages. A correctly colored version can be seen in [http://wiki.visual-prolog.com/index.php?title=Man_or_boy_test Man or boy test] in the Visual Prolog wiki).


```VisualProlog

implement main
    open core

clauses
    run():-
        console::init(),
        stdio::write(a(10, {() = 1}, {() = -1}, {() = -1}, {() = 1}, {() = 0})).

class predicates
    a : (integer K, function{integer} X1, function{integer} X2, function{integer} X3, function{integer} X4, function{integer} X5) -> integer Result.
clauses
    a(K, X1, X2, X3, X4, X5) = R :-
        KM = varM::new(K),
        BM = varM{function{integer}}::new({() = 0}),
        BM:value :=
            { () = BR :-
                KM:value := KM:value-1,
                BR = a(KM:value, BM:value, X1, X2, X3, X4)
            },
        R = if KM:value <= 0 then X4() + X5() else BM:value() end if.

end implement main
```


=={{header|Vorpal}} ==
Adapted from the Lua example.  In vorpal, all execution is a message to an object.  This task primarily involves functions, so we have the apply the function objects to self for them to execute.  Correctly, prints -67.


```vorpal
self.a = method(k, x1, x2, x3, x4, x5){
  b = method(){
    code.k = code.k - 1
    return( self.a(code.k, code, code.x1, code.x2, code.x3, code.x4) )
  }
  b.k = k
  b.x1 = x1
  b.x2 = x2
  b.x3 = x3
  b.x4 = x4
  b.x5 = x5

  if(k <= 0){
    return(self.apply(x4) + self.apply(x5))
  }
  else{
    return(self.apply(b))
  }
}

self.K = method(n){
  f = method(){
    return(code.n)
  }
  f.n = n
  return(f)
}

self.a(10, self.K(1), self.K(-1), self.K(-1), self.K(1), self.K(0)).print()
```


=={{header|zkl}} ==
The compiler is OK but the VM is a girlie-man VM. Due to the way closures are built, the stack blows quickly when closures recurse. So, while the code can be written per Knuth, it is unable to do anything. So, classes are used to simulate the closures. Also (5)()-->5 so no problems there.

```zkl
fcn A(k, x1, x2, x3, x4, x5){ // -->1,0,-2,0,1,0,1,-1,-10,-30,-67,-138
   B:=CB(k, x1, x2, x3, x4, x5);
   if(k <= 0) x4()+x5() else B.B();
}

foreach k in (12){
   println("k=%2d A=%d".fmt(k, A(k, 1, -1, -1, 1, 0)))
}

class CB{ var k, x1, x2, x3, x4, x5;
   fcn init{ k, x1, x2, x3, x4, x5 = vm.arglist; }
   fcn B{
      k= k - 1;
      A(k, B, x1, x2, x3, x4);
   }
}
```

{{out}}

```txt

k= 0 A=1
k= 1 A=0
k= 2 A=-2
k= 3 A=0
k= 4 A=1
k= 5 A=0
k= 6 A=1
k= 7 A=-1
k= 8 A=-10
k= 9 A=-30
k=10 A=-67
k=11 A=-138
and the stack blows

```


{{omit from|PARI/GP}}
