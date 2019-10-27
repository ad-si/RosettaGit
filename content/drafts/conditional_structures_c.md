+++
title = "Conditional structures/C"
description = ""
date = 2011-10-27T23:01:35Z
aliases = []
[extra]
id = 5298
[taxonomies]
categories = []
tags = []
+++

{{collection|Conditional Structures}}

===if-then-else===


```c
int main (int argc, char ** argv) {
  int input = 2;

  if (3 == input) {
    /* Do something */
  }


  if (3 == input) {
    /* Do something */
  } else {
    /* Do something else */
  }
}
```


One line predicates do not require curly braces (although their usage is often preferred for readability):


```c
if (cond)
  expr;

if (cond)
  expr;
else
  expr;
```


And these may be mixed:


```c
if (cond)
  expr;
else
 {
  /* multiple expressions */
 }
```



```c
if (cond)
 {
  /* multiple expressions */
 }
else
 expr;
```


===Short-circuit conditionals===

The short-circuit evaluation of the '''&&''' and '''||''' boolean operators are also often used for control flow.

```c
if (node != NULL && node->next != NULL && guarded_test(node))
   node->next = node->next->next;
```

Neither the assignment nor guarded_test() will be called if the previous two conditions aren't met. Other languages such as [[Pascal]] don't make that guarantee.


### switch



```c
int main (int argc, char ** argv) {
  int input = 42;

  switch (input) {
    case 0:
      /* Do something, because input = 0 */
      break;
    case 1:
      /* Do something, because input = 1 */
      break;
    case 2:
      /* Do something, because input = 2 */
      /* fall through to the next statement if there is no "break" */
    default:
      /* Do something else. */
      break; /* Optional */
  }
}
```


A little known fact is that the default branch need not be the last one:


```c
int main(int argc, char* argv[])
{
  switch (argc)
  {
  default:
    /* this will be executed if argc is neither 2 nor 3 */
    break;
  case 2:
    /* this will be executed if argc is 2 */
    break;
  case 3:
    /* this will be executer if argc is 3 */
  }
}
```



### Ternary ?:


Conditionals in C can also be done with the ternary operator, ?:. The arguments are expressions, and a?b:c is an expression as well. However, since many things in C are expressions (this especially includes assignments and function calls), ?: can be used for those, too. However, the if/else construct is usually more readable and therefore preferred.


```c
int main(int argc, char ** argv)
{
  int input = 2;
  int output = (input == 2? 42 : 4711);  /* sets output to 42 */
  int output2 = (input == 3? 42 : 4711); /* sets output2 to 4711 */

  int do_something();
  int do_something_else();
  input == 1? do_something() : do_something_else(); /* only calls do_something_else() */
}
```


=== Preprocessor Techniques (compile-time)===

Conditional compile based on if a certain macro exists, 

```c
#ifdef FOO
/* compile this only if macro FOO exist */
#endif
```


Conditional compile based on if a macro doesn't exist

```c
#ifndef FOO
/* only compiled if macro FOO does not exist */
#endif
```


Conditional compile based on if a certain macro exists, with else clause

```c
#ifdef FOO
/* compile this only if macro FOO exist */
#else
/* compile this only if macro FOO does not exist */
#endif
```


Conditional compile based on expression

```c
#if defined(FOO) && FOO == 1
/* only compiled if macro FOO is defined and expands to a constant expression evaluating to 1 */
#endif
```



```c
#if FOO
/* only compiled if macro FOO is defined and expands to a constant expression of non-zero value */
#endif
```


Chain of conditionals

```c
#if defined(FOO)
/* only compiled if macro FOO is defined */
#elif defined(BAR)
/* only compiled if macro FOO is not defined, but macro BAR is */
#else
/* only compiled if neither FOO nor BAR is defined */
#endif
```

Typical usage: Include guards

```c
#ifndef FOO_H_ALREADY_INCLUDED
#define FOO_H_ALREADY_INCLUDED
/* header content */
#endif
```

If the header is included the first time, the macro FOO_H_ALREADY_INCLUDED will not be defined, thus the code between #ifndef and #endif will be compiled. The first thing this code does is define that macro, so that the next time the header is included, the code will be ignored. This effectively avoids multiple inclusion.
