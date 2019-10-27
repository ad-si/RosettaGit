+++
title = "Parse EBNF/Tests"
description = ""
date = 2011-05-22T22:43:46Z
aliases = []
[extra]
id = 9759
[taxonomies]
categories = []
tags = []
+++

The flavor of EBNF used here is the same as that defined [http://karmin.ch/ebnf/index here], except a literal can't contain the character used for quoting, and an identifier can't contain whitespace or any of the characters <code>= | ( ) { } [ ] . ; " '</code>.

==A one-liner==

```ebnf
"a" {
    a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
} "z"
```


Some valid inputs:
* <code>a1a3a4a4a5a6</code>
* <code> a1 a2a6 </code>
* <code>a1 a3 a4 a6</code> 

Some invalid inputs:
* <code>a1 a4 a5 a6</code>
* <code>a1 a2 a4 a5 a5 a6</code>
* <code>a1 a2 a4 a5 a6 a7</code>
* <code>your ad here</code>

==Arithmetic expressions==

```ebnf
{
    expr = term { plus term } .
    term = factor { times factor } .
    factor = number | '(' expr ')' .

    plus = "+" | "-" .
    times = "*" | "/" .

    number = digit { digit } .
    digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
}
```


Some valid inputs:
* <code>2</code>
* <code>2*3 + 4/23 - 7</code>
* <code>(3 + 4) * 6-2+(4*(4))</code> 

Some invalid inputs:
* <code>-2</code>
* <code>3 +</code>
* <code>(4 + 3</code>

==Some invalid EBNF==

```ebnf
a = "1";
```



```ebnf
{ a = "1" ;
```



```ebnf
{ hello world = "1"; }
```



```ebnf
{ foo = bar . }
```

