+++
title = "Talk:Generic swap"
description = ""
date = 2019-10-11T14:40:57Z
aliases = []
[extra]
id = 2120
[taxonomies]
categories = []
tags = []
+++

== task description too language-specific? ==

This task description seems to me a little too language-specific in its concepts.

The statement that "The generic routine is created with one or more generic formal parameters" could be read to exclude a Haskell function with an inferred, rather than explicit, type signature, or any language with an implicit universal type ("dynamically typed").

Broadly, the task description seems to me to overspecify a particular means of operation, rather than only "do this sort of thing in a way appropriate to the language".

To demonstrate the problem, I've written Haskell, E, and Perl examples. I'd like to see the intent of the task better described so that these examples are clearly correct or incorrect.

--[[User:Kevin Reid|Kevin Reid]] 18:13, 7 September 2007 (EDT)

I have adjusted the task definition. Please let me know if you have any more questions.

--[[User:Waldorf|Waldorf]] 18:11, 7 September 2007 (MDT)

Thanks; I think that's much better.

Since you ask, I was also wondering about how broadly to interpret "swap". For example, I wrote two examples which don't exchange values in mutable cells (as the Ada, Perl, and first E example do), but rather accept two values and produce them in the opposite order. (However, the Haskell version could easily be used in an imperatively-expressed program; e.g. <code>[http://haskell.org/ghc/docs/latest/html/libraries/mtl/Control-Monad-State-Class.html#v%3Amodify modify] swap</code>.)

--[[User:Kevin Reid|Kevin Reid]] 21:41, 7 September 2007 (EDT)

I do not want to limit the implementation. I am after the abstract functionality. I chose "swap" because it is conceptually simple. I am more interested in demonstrating genericity than the low level nature of swap.

--[[Waldorf|Waldorf]] 20:27, 7 September 2007 (MDT)

== C++ ==

Would the C++ example here assume that there is a copy constructor for T types? Would it be better to have "tmp = left" rather than "tmp(left)"? --[[User:mwn3d|mwn3d]] 00:08, 14 November 2007
: Well, in some circumstances (assigning a char to a CString in Windows MFC, for example), you ''have'' to use the explicit constructor, due to the way a class was defined.  Granted, it would be a poor class implementation that required explicit use of its own copy constructor, but it's not impossible.
:
: In addition, I tried a couple small programs to test the problem:

```txt
#include <iostream>

using namespace std;

int main()
{
        int a = 7;
        int b = int(a);
        cout << b << endl;
}

```


: This outputs "7".
:
: I also tried creating a class with no defined copy constructor:

```txt
#include <iostream>

using namespace std;

class CNoCopy
{
public:
        int m_value;
        CNoCopy()
        {
                cout << "Constructor Called" << endl;
        }

        ~CNoCopy()
        {
                cout << "Destructor called" << endl;
        }

        void setValue( int newVal )
        {
                m_value = newVal;
        }

        int getValue()
        {
                return m_value;
        }
};

int main()
{
        CNoCopy o1;
        o1.setValue( 7 );
        CNoCopy o2 = CNoCopy( o1 );
        cout << o2.getValue() << endl;
}

```

:This outputs

```txt
Constructor Called
7
Destructor called
Destructor called

```

: So the copy went ahead, without calling any explicitly-define constructor.  For reference, I'm using [[G++]] 4.1.3.  In all, I think the implementation is fine. --[[User:Short Circuit|Short Circuit]] 19:06, 14 November 2007 (MST)

== boolga boolga boolga ==

y does the c version use char* and not void *? can it use anything? long* int* short* byte*etc?
:Because you can't operate on (void *), only refer to it. That said, a ''real'' implementation would cast to (int *) first (the most efficient machine word size), then finish any leftovers with a few less efficient (char *) swaps at the end. --[[User:IanOsgood|IanOsgood]] 18:50, 3 February 2008 (MST)

== dc ==
reverse is only available as a GNU extension (same with n, and comments (#) etc. - probably the other version should be kept too?)
[[User:Rahul|Rahul]] 20:46, 8 December 2008 (UTC)
: OK. Could you link (or simply list; dc is tiny) the standard dc commands on the language page? --[[User:IanOsgood|IanOsgood]] 02:02, 9 December 2008 (UTC)
:: I will add the list of commands to the dc page. [[User:Rahul|Rahul]] 09:13, 9 December 2008 (UTC)

AFIK, dc only has a single data type (numeric value), so how is it '''generic''' swap? --[[User:PauliKL|PauliKL]] 12:52, 9 December 2008 (UTC)

: Would be an interesting question, because since unityped systems are a subset of typed systems. So it can be argued that if dc is unityped, then all the procedures that it implements are generic by default. Interestingly dc is not unityped - 
i.e
 $dc
 [ 1 2 3 4 ]
 [ 5 6 7 8 ]
 sa sb la lb
 f
 1 2 3 4 
 5 6 7 8
[[User:Rahul|Rahul]] 13:47, 9 December 2008 (UTC)
