+++
title = "Compiler/Sample programs"
description = ""
date = 2017-01-16T16:33:18Z
aliases = []
[extra]
id = 21171
[taxonomies]
categories = []
tags = []
+++

Additional sample programs for the Tiny Compiler, referenced by the following tasks:

* [[Compiler/lexical_analyzer|Lexical Analyzer task]]
* [[Compiler/syntax_analyzer|Syntax Analyzer task]]
* [[Compiler/code_generator|Code Generator task]]
* [[Compiler/virtual_machine_interpreter|Virtual Machine Interpreter task]]
* [[Compiler/AST_interpreter|AST Interpreter task]]

<hr>
__TOC__


## Hello world/Text


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/*
  Hello world
 */
print("Hello, World!\n");

```



| style="vertical-align:top" |
<b>
```txt

    4      1 Keyword_print
    4      6 LeftParen
    4      7 String         "Hello, World!\n"
    4     24 RightParen
    4     25 Semicolon
    5      1 End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
;
Sequence
;
Prts
String        "Hello, World!\n"
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 0 Strings: 1
"Hello, World!\n"
   0 push  0
   5 prts
   6 halt

```
</b>
|}

;And the output is:

```txt

>lex ..\hello.t | parse | gen | vm
Hello, World!

```



## Phoenix number


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/*
  Show Ident and Integers
 */
phoenix_number = 142857;
print(phoenix_number, "\n");

```



| style="vertical-align:top" |
<b>
```txt

    4      1   Identifier      phoenix_number
    4     16   Op_assign
    4     18   Integer          142857
    4     24   Semicolon
    5      1   Keyword_print
    5      6   LeftParen
    5      7   Identifier      phoenix_number
    5     21   Comma
    5     23   String          "\n"
    5     27   RightParen
    5     28   Semicolon
    6      1   End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
;
Assign
Identifier    phoenix_number
Integer       142857
Sequence
Sequence
;
Prti
Identifier    phoenix_number
;
Prts
String        "\n"
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 1 Strings: 1
"\n"
   0 push  142857
   5 store [0]
  10 fetch [0]
  15 prti
  16 push  0
  21 prts
  22 halt

```
</b>
|}

;And the output is:

```txt

>lex ..\phoenix_number.t | parse | gen | vm
142857

```




## All symbols


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/*
  All lexical tokens - not syntactically correct, but that will
  have to wait until syntax analysis
 */
/* Print   */  print    /* Sub     */  -
/* Putc    */  putc     /* Lss     */  <
/* If      */  if       /* Gtr     */  >
/* Else    */  else     /* Leq     */  <=
/* While   */  while    /* Geq     */  >=
/* Lbrace  */  {        /* Eq      */  ==
/* Rbrace  */  }        /* Neq     */  !=
/* Lparen  */  (        /* And     */  &&
/* Rparen  */  )        /* Or      */  ||
/* Uminus  */  -        /* Semi    */  ;
/* Not     */  !        /* Comma   */  ,
/* Mul     */  *        /* Assign  */  =
/* Div     */  /        /* Integer */  42
/* Mod     */  %        /* String  */  "String literal"
/* Add     */  +        /* Ident   */  variable_name
/* character literal */  '\n'
/* character literal */  '\\'
/* character literal */  ' '

```



| style="vertical-align:top" |
<b>
```txt

    5     16   Keyword_print
    5     40   Op_subtract
    6     16   Keyword_putc
    6     40   Op_less
    7     16   Keyword_if
    7     40   Op_greater
    8     16   Keyword_else
    8     40   Op_lessequal
    9     16   Keyword_while
    9     40   Op_greaterequal
   10     16   LeftBrace
   10     40   Op_equal
   11     16   RightBrace
   11     40   Op_notequal
   12     16   LeftParen
   12     40   Op_and
   13     16   RightParen
   13     40   Op_or
   14     16   Op_subtract
   14     40   Semicolon
   15     16   Op_not
   15     40   Comma
   16     16   Op_multiply
   16     40   Op_assign
   17     16   Op_divide
   17     40   Integer             42
   18     16   Op_mod
   18     40   String          "String literal"
   19     16   Op_add
   19     40   Identifier      variable_name
   20     26   Integer             10
   21     26   Integer             92
   22     26   Integer             32
   23      1   End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

(5, 40) Print: Expecting '(', found '-'

```
</b>


| style="vertical-align:top" |
<b>
```txt


```
</b>
|}


## Test case 4


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/*** test printing, embedded \n and comments with lots of '*' ***/
print(42);
print("\nHello World\nGood Bye\nok\n");
print("Print a slash n - \\n.\n");

```



| style="vertical-align:top" |
<b>
```txt

    2      1   Keyword_print
    2      6   LeftParen
    2      7   Integer             42
    2      9   RightParen
    2     10   Semicolon
    3      1   Keyword_print
    3      6   LeftParen
    3      7   String          "\nHello World\nGood Bye\nok\n"
    3     38   RightParen
    3     39   Semicolon
    4      1   Keyword_print
    4      6   LeftParen
    4      7   String          "Print a slash n - \\n.\n"
    4     33   RightParen
    4     34   Semicolon
    5      1   End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
Sequence
;
Sequence
;
Prti
Integer       42
;
Sequence
;
Prts
String        "\nHello World\nGood Bye\nok\n"
;
Sequence
;
Prts
String        "Print a slash n - \\n.\n"
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 0 Strings: 2
"\nHello World\nGood Bye\nok\n"
"Print a slash n - \\n.\n"
   0 push  42
   5 prti
   6 push  0
  11 prts
  12 push  1
  17 prts
  18 halt

```
</b>
|}

;And the output is:

```txt

>lex ..\testcase4.t | parse | gen | vm
42
Hello World
Good Bye
ok
Print a slash n - \n.

```



## Count


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

count = 1;
while (count < 10) {
    print("count is: ", count, "\n");
    count = count + 1;
}

```



| style="vertical-align:top" |
<b>
```txt

    1      1   Identifier      count
    1      7   Op_assign
    1      9   Integer              1
    1     10   Semicolon
    2      1   Keyword_while
    2      7   LeftParen
    2      8   Identifier      count
    2     14   Op_less
    2     16   Integer             10
    2     18   RightParen
    2     20   LeftBrace
    3      5   Keyword_print
    3     10   LeftParen
    3     11   String          "count is: "
    3     23   Comma
    3     25   Identifier      count
    3     30   Comma
    3     32   String          "\n"
    3     36   RightParen
    3     37   Semicolon
    4      5   Identifier      count
    4     11   Op_assign
    4     13   Identifier      count
    4     19   Op_add
    4     21   Integer              1
    4     22   Semicolon
    5      1   RightBrace
    6      1   End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
;
Assign
Identifier    count
Integer       1
While
Less
Identifier    count
Integer       10
Sequence
Sequence
;
Sequence
Sequence
Sequence
;
Prts
String        "count is: "
;
Prti
Identifier    count
;
Prts
String        "\n"
;
Assign
Identifier    count
Add
Identifier    count
Integer       1

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 1 Strings: 2
"count is: "
"\n"
   0 push  1
   5 store [0]
  10 fetch [0]
  15 push  10
  20 lt
  21 jz     (43) 65
  26 push  0
  31 prts
  32 fetch [0]
  37 prti
  38 push  1
  43 prts
  44 fetch [0]
  49 push  1
  54 add
  55 store [0]
  60 jmp    (-51) 10
  65 halt

```
</b>
|}

;And the output is:

```txt

>lex ..\count.t | parse | gen | vm
count is: 1
count is: 2
count is: 3
count is: 4
count is: 5
count is: 6
count is: 7
count is: 8
count is: 9

```


=={{header|100_doors}}==

{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/* 100 Doors */
i = 1;
while (i * i <= 100) {
    print("door ", i * i, " is open\n");
    i = i + 1;
}

```



| style="vertical-align:top" |
<b>
```txt

lex ..\100doors.t
    2      1 Identifier      i
    2      3 Op_assign
    2      5 Integer             1
    2      6 Semicolon
    3      1 Keyword_while
    3      7 LeftParen
    3      8 Identifier      i
    3     10 Op_multiply
    3     12 Identifier      i
    3     14 Op_lessequal
    3     17 Integer           100
    3     20 RightParen
    3     22 LeftBrace
    4      5 Keyword_print
    4     10 LeftParen
    4     11 String          "door "
    4     18 Comma
    4     20 Identifier      i
    4     22 Op_multiply
    4     24 Identifier      i
    4     25 Comma
    4     27 String          " is open\n"
    4     39 RightParen
    4     40 Semicolon
    5      5 Identifier      i
    5      7 Op_assign
    5      9 Identifier      i
    5     11 Op_add
    5     13 Integer             1
    5     14 Semicolon
    6      1 RightBrace
    8      1 End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

lex ..\100doors.t | parse
Sequence
Sequence
;
Assign
Identifier     i
Integer        1
While
LessEqual
Multiply
Identifier     i
Identifier     i
Integer        100
Sequence
Sequence
;
Sequence
Sequence
Sequence
;
Prts
String         "door "
;
Prti
Multiply
Identifier     i
Identifier     i
;
Prts
String         " is open\n"
;
Assign
Identifier     i
Add
Identifier     i
Integer        1

```
</b>


| style="vertical-align:top" |
<b>
```txt

lex ..\100doors.t | parse | gen
Datasize: 1 Strings: 2
"door "
" is open\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 fetch [0]
   20 mul
   21 push  100
   26 le
   27 jz     (49) 77
   32 push  0
   37 prts
   38 fetch [0]
   43 fetch [0]
   48 mul
   49 prti
   50 push  1
   55 prts
   56 fetch [0]
   61 push  1
   66 add
   67 store [0]
   72 jmp    (-63) 10
   77 halt

```
</b>
|}

;And the output is:

```txt

lex ..\100doors.t | parse | gen | vm
door 1 is open
door 4 is open
door 9 is open
door 16 is open
door 25 is open
door 36 is open
door 49 is open
door 64 is open
door 81 is open
door 100 is open

```



## Negative tests


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

a = (-1 * ((-1 * (5 * 15)) / 10));
print(a, "\n");
b = -a;
print(b, "\n");
print(-b, "\n");
print(-(1), "\n");

```



| style="vertical-align:top" |
<b>
```txt

    1      1   Identifier      a
    1      3   Op_assign
    1      5   LeftParen
    1      6   Op_subtract
    1      7   Integer              1
    1      9   Op_multiply
    1     11   LeftParen
    1     12   LeftParen
    1     13   Op_subtract
    1     14   Integer              1
    1     16   Op_multiply
    1     18   LeftParen
    1     19   Integer              5
    1     21   Op_multiply
    1     23   Integer             15
    1     25   RightParen
    1     26   RightParen
    1     28   Op_divide
    1     30   Integer             10
    1     32   RightParen
    1     33   RightParen
    1     34   Semicolon
    2      1   Keyword_print
    2      6   LeftParen
    2      7   Identifier      a
    2      8   Comma
    2     10   String          "\n"
    2     14   RightParen
    2     15   Semicolon
    3      1   Identifier      b
    3      3   Op_assign
    3      5   Op_subtract
    3      6   Identifier      a
    3      7   Semicolon
    4      1   Keyword_print
    4      6   LeftParen
    4      7   Identifier      b
    4      8   Comma
    4     10   String          "\n"
    4     14   RightParen
    4     15   Semicolon
    5      1   Keyword_print
    5      6   LeftParen
    5      7   Op_subtract
    5      8   Identifier      b
    5      9   Comma
    5     11   String          "\n"
    5     15   RightParen
    5     16   Semicolon
    6      1   Keyword_print
    6      6   LeftParen
    6      7   Op_subtract
    6      8   LeftParen
    6      9   Integer              1
    6     10   RightParen
    6     11   Comma
    6     13   String          "\n"
    6     17   RightParen
    6     18   Semicolon
    7      1   End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    a
Multiply
Negate
Integer       1
;
Divide
Multiply
Negate
Integer       1
;
Multiply
Integer       5
Integer       15
Integer       10
Sequence
Sequence
;
Prti
Identifier    a
;
Prts
String        "\n"
;
Assign
Identifier    b
Negate
Identifier    a
;
Sequence
Sequence
;
Prti
Identifier    b
;
Prts
String        "\n"
;
Sequence
Sequence
;
Prti
Negate
Identifier    b
;
;
Prts
String        "\n"
;
Sequence
Sequence
;
Prti
Negate
Integer       1
;
;
Prts
String        "\n"
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 2 Strings: 1
"\n"
   0 push  1
   5 neg
   6 push  1
  11 neg
  12 push  5
  17 push  15
  22 mul
  23 mul
  24 push  10
  29 div
  30 mul
  31 store [0]
  36 fetch [0]
  41 prti
  42 push  0
  47 prts
  48 fetch [0]
  53 neg
  54 store [1]
  59 fetch [1]
  64 prti
  65 push  0
  70 prts
  71 fetch [1]
  76 neg
  77 prti
  78 push  0
  83 prts
  84 push  1
  89 neg
  90 prti
  91 push  0
  96 prts
  97 halt

```
</b>
|}

;And the output is:

```txt

>lex ..\negative.t | parse | gen | vm
7
-7
7
-1

```



## Deep


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

print(---------------------------------+++5, "\n");
print(((((((((3 + 2) * ((((((2))))))))))))), "\n");

if (1) { if (1) { if (1) { if (1) { if (1) { print(15, "\n"); } } } } }

```



| style="vertical-align:top" |
<b>
```txt

    1      1   Keyword_print
    1      6   LeftParen
    1      7   Op_subtract
    1      8   Op_subtract
    1      9   Op_subtract
    1     10   Op_subtract
    1     11   Op_subtract
    1     12   Op_subtract
    1     13   Op_subtract
    1     14   Op_subtract
    1     15   Op_subtract
    1     16   Op_subtract
    1     17   Op_subtract
    1     18   Op_subtract
    1     19   Op_subtract
    1     20   Op_subtract
    1     21   Op_subtract
    1     22   Op_subtract
    1     23   Op_subtract
    1     24   Op_subtract
    1     25   Op_subtract
    1     26   Op_subtract
    1     27   Op_subtract
    1     28   Op_subtract
    1     29   Op_subtract
    1     30   Op_subtract
    1     31   Op_subtract
    1     32   Op_subtract
    1     33   Op_subtract
    1     34   Op_subtract
    1     35   Op_subtract
    1     36   Op_subtract
    1     37   Op_subtract
    1     38   Op_subtract
    1     39   Op_subtract
    1     40   Op_add
    1     41   Op_add
    1     42   Op_add
    1     43   Integer              5
    1     44   Comma
    1     46   String          "\n"
    1     50   RightParen
    1     51   Semicolon
    2      1   Keyword_print
    2      6   LeftParen
    2      7   LeftParen
    2      8   LeftParen
    2      9   LeftParen
    2     10   LeftParen
    2     11   LeftParen
    2     12   LeftParen
    2     13   LeftParen
    2     14   LeftParen
    2     15   Integer              3
    2     17   Op_add
    2     19   Integer              2
    2     20   RightParen
    2     22   Op_multiply
    2     24   LeftParen
    2     25   LeftParen
    2     26   LeftParen
    2     27   LeftParen
    2     28   LeftParen
    2     29   LeftParen
    2     30   Integer              2
    2     31   RightParen
    2     32   RightParen
    2     33   RightParen
    2     34   RightParen
    2     35   RightParen
    2     36   RightParen
    2     37   RightParen
    2     38   RightParen
    2     39   RightParen
    2     40   RightParen
    2     41   RightParen
    2     42   RightParen
    2     43   RightParen
    2     44   Comma
    2     46   String          "\n"
    2     50   RightParen
    2     51   Semicolon
    4      1   Keyword_if
    4      4   LeftParen
    4      5   Integer              1
    4      6   RightParen
    4      8   LeftBrace
    4     10   Keyword_if
    4     13   LeftParen
    4     14   Integer              1
    4     15   RightParen
    4     17   LeftBrace
    4     19   Keyword_if
    4     22   LeftParen
    4     23   Integer              1
    4     24   RightParen
    4     26   LeftBrace
    4     28   Keyword_if
    4     31   LeftParen
    4     32   Integer              1
    4     33   RightParen
    4     35   LeftBrace
    4     37   Keyword_if
    4     40   LeftParen
    4     41   Integer              1
    4     42   RightParen
    4     44   LeftBrace
    4     46   Keyword_print
    4     51   LeftParen
    4     52   Integer             15
    4     54   Comma
    4     56   String          "\n"
    4     60   RightParen
    4     61   Semicolon
    4     63   RightBrace
    4     65   RightBrace
    4     67   RightBrace
    4     69   RightBrace
    4     71   RightBrace
    5      1   End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
Sequence
;
Sequence
Sequence
;
Prti
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Negate
Integer       5
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
Prts
String        "\n"
;
Sequence
Sequence
;
Prti
Multiply
Add
Integer       3
Integer       2
Integer       2
;
Prts
String        "\n"
;
If
Integer       1
If
Sequence
;
If
Integer       1
If
Sequence
;
If
Integer       1
If
Sequence
;
If
Integer       1
If
Sequence
;
If
Integer       1
If
Sequence
;
Sequence
Sequence
;
Prti
Integer       15
;
Prts
String        "\n"
;
;
;
;
;
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 0 Strings: 1
"\n"
   0 push  5
   5 neg
   6 neg
   7 neg
   8 neg
   9 neg
  10 neg
  11 neg
  12 neg
  13 neg
  14 neg
  15 neg
  16 neg
  17 neg
  18 neg
  19 neg
  20 neg
  21 neg
  22 neg
  23 neg
  24 neg
  25 neg
  26 neg
  27 neg
  28 neg
  29 neg
  30 neg
  31 neg
  32 neg
  33 neg
  34 neg
  35 neg
  36 neg
  37 neg
  38 prti
  39 push  0
  44 prts
  45 push  3
  50 push  2
  55 add
  56 push  2
  61 mul
  62 prti
  63 push  0
  68 prts
  69 push  1
  74 jz     (56) 131
  79 push  1
  84 jz     (46) 131
  89 push  1
  94 jz     (36) 131
  99 push  1
 104 jz     (26) 131
 109 push  1
 114 jz     (16) 131
 119 push  15
 124 prti
 125 push  0
 130 prts
 131 halt

```
</b>
|}

;And the output is:

```txt

>lex ..\deep.t | parse | gen | vm
-5
10
15

```




## Greatest common divisor

{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse, input to gen
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/* Compute the gcd of 1071, 1029:  21 */

a = 1071;
b = 1029;

while (b != 0) {
    new_a = b;
    b     = a % b;
    a     = new_a;
}
print(a);

```



| style="vertical-align:top" |
<b>
```txt

    3      1 Identifier      a
    3      3 Op_assign
    3      5 Integer          1071
    3      9 Semicolon
    4      1 Identifier      b
    4      3 Op_assign
    4      5 Integer          1029
    4      9 Semicolon
    6      1 Keyword_while
    6      7 LeftParen
    6      8 Identifier      b
    6     10 Op_notequal
    6     13 Integer             0
    6     14 RightParen
    6     16 LeftBrace
    7      5 Identifier      new_a
    7     11 Op_assign
    7     13 Identifier      b
    7     14 Semicolon
    8      5 Identifier      b
    8     11 Op_assign
    8     13 Identifier      a
    8     15 Op_mod
    8     17 Identifier      b
    8     18 Semicolon
    9      5 Identifier      a
    9     11 Op_assign
    9     13 Identifier      new_a
    9     18 Semicolon
   10      1 RightBrace
   11      1 Keyword_print
   11      6 LeftParen
   11      7 Identifier      a
   11      8 RightParen
   11      9 Semicolon
   12      1 End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     a
Integer        1071
Assign
Identifier     b
Integer        1029
While
NotEqual
Identifier     b
Integer        0
Sequence
Sequence
Sequence
;
Assign
Identifier     new_a
Identifier     b
Assign
Identifier     b
Mod
Identifier     a
Identifier     b
Assign
Identifier     a
Identifier     new_a
Sequence
;
Prti
Identifier     a
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 3 Strings: 0
    0 push  1071
    5 store [0]
   10 push  1029
   15 store [1]
   20 fetch [1]
   25 push  0
   30 ne
   31 jz     (45) 77
   36 fetch [1]
   41 store [2]
   46 fetch [0]
   51 fetch [1]
   56 mod
   57 store [1]
   62 fetch [2]
   67 store [0]
   72 jmp    (-53) 20
   77 fetch [0]
   82 prti
   83 halt

```
</b>

|}


## Factorial


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/* 12 factorial is 479001600 */

n = 12;
result = 1;
i = 1;
while (i <= n) {
    result = result * i;
    i = i + 1;
}
print(result);

```



| style="vertical-align:top" |
<b>
```txt

    3      1 Identifier      n
    3      3 Op_assign
    3      5 Integer            12
    3      7 Semicolon
    4      1 Identifier      result
    4      8 Op_assign
    4     10 Integer             1
    4     11 Semicolon
    5      1 Identifier      i
    5      3 Op_assign
    5      5 Integer             1
    5      6 Semicolon
    6      1 Keyword_while
    6      7 LeftParen
    6      8 Identifier      i
    6     10 Op_lessequal
    6     13 Identifier      n
    6     14 RightParen
    6     16 LeftBrace
    7      5 Identifier      result
    7     12 Op_assign
    7     14 Identifier      result
    7     21 Op_multiply
    7     23 Identifier      i
    7     24 Semicolon
    8      5 Identifier      i
    8      7 Op_assign
    8      9 Identifier      i
    8     11 Op_add
    8     13 Integer             1
    8     14 Semicolon
    9      1 RightBrace
   10      1 Keyword_print
   10      6 LeftParen
   10      7 Identifier      result
   10     13 RightParen
   10     14 Semicolon
   11      1 End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     n
Integer        12
Assign
Identifier     result
Integer        1
Assign
Identifier     i
Integer        1
While
LessEqual
Identifier     i
Identifier     n
Sequence
Sequence
;
Assign
Identifier     result
Multiply
Identifier     result
Identifier     i
Assign
Identifier     i
Add
Identifier     i
Integer        1
Sequence
;
Prti
Identifier     result
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 3 Strings: 0
    0 push  12
    5 store [0]
   10 push  1
   15 store [1]
   20 push  1
   25 store [2]
   30 fetch [2]
   35 fetch [0]
   40 le
   41 jz     (41) 83
   46 fetch [1]
   51 fetch [2]
   56 mul
   57 store [1]
   62 fetch [2]
   67 push  1
   72 add
   73 store [2]
   78 jmp    (-49) 30
   83 fetch [1]
   88 prti
   89 halt

```
</b>
|}


## Fibonacci sequence


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/* fibonacci of 44 is 701408733 */

n = 44;
i = 1;
a = 0;
b = 1;
while (i < n) {
    w = a + b;
    a = b;
    b = w;
    i = i + 1;
}
print(w, "\n");

```



| style="vertical-align:top" |
<b>
```txt

    3      1 Identifier      n
    3      3 Op_assign
    3      5 Integer            44
    3      7 Semicolon
    4      1 Identifier      i
    4      3 Op_assign
    4      5 Integer             1
    4      6 Semicolon
    5      1 Identifier      a
    5      3 Op_assign
    5      5 Integer             0
    5      6 Semicolon
    6      1 Identifier      b
    6      3 Op_assign
    6      5 Integer             1
    6      6 Semicolon
    7      1 Keyword_while
    7      7 LeftParen
    7      8 Identifier      i
    7     10 Op_less
    7     12 Identifier      n
    7     13 RightParen
    7     15 LeftBrace
    8      5 Identifier      w
    8      7 Op_assign
    8      9 Identifier      a
    8     11 Op_add
    8     13 Identifier      b
    8     14 Semicolon
    9      5 Identifier      a
    9      7 Op_assign
    9      9 Identifier      b
    9     10 Semicolon
   10      5 Identifier      b
   10      7 Op_assign
   10      9 Identifier      w
   10     10 Semicolon
   11      5 Identifier      i
   11      7 Op_assign
   11      9 Identifier      i
   11     11 Op_add
   11     13 Integer             1
   11     14 Semicolon
   12      1 RightBrace
   13      1 Keyword_print
   13      6 LeftParen
   13      7 Identifier      w
   13      8 Comma
   13     10 String          "\n"
   13     14 RightParen
   13     15 Semicolon
   15      1 End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     n
Integer        44
Assign
Identifier     i
Integer        1
Assign
Identifier     a
Integer        0
Assign
Identifier     b
Integer        1
While
Less
Identifier     i
Identifier     n
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     w
Add
Identifier     a
Identifier     b
Assign
Identifier     a
Identifier     b
Assign
Identifier     b
Identifier     w
Assign
Identifier     i
Add
Identifier     i
Integer        1
Sequence
Sequence
;
Prti
Identifier     w
;
Prts
String         "\n"
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 5 Strings: 1
"\n"
    0 push  44
    5 store [0]
   10 push  1
   15 store [1]
   20 push  0
   25 store [2]
   30 push  1
   35 store [3]
   40 fetch [1]
   45 fetch [0]
   50 lt
   51 jz     (61) 113
   56 fetch [2]
   61 fetch [3]
   66 add
   67 store [4]
   72 fetch [3]
   77 store [2]
   82 fetch [4]
   87 store [3]
   92 fetch [1]
   97 push  1
  102 add
  103 store [1]
  108 jmp    (-69) 40
  113 fetch [4]
  118 prti
  119 push  0
  124 prts
  125 halt

```
</b>
|}


## FizzBuzz

{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/* FizzBuzz */
i = 1;
while (i <= 100) {
    if (!(i % 15))
        print("FizzBuzz");
    else if (!(i % 3))
        print("Fizz");
    else if (!(i % 5))
        print("Buzz");
    else
        print(i);

    print("\n");
    i = i + 1;
}

```



| style="vertical-align:top" |
<b>
```txt

    2      1 Identifier      i
    2      3 Op_assign
    2      5 Integer             1
    2      6 Semicolon
    3      1 Keyword_while
    3      7 LeftParen
    3      8 Identifier      i
    3     10 Op_lessequal
    3     13 Integer           100
    3     16 RightParen
    3     18 LeftBrace
    4      5 Keyword_if
    4      8 LeftParen
    4      9 Op_not
    4     10 LeftParen
    4     11 Identifier      i
    4     13 Op_mod
    4     15 Integer            15
    4     17 RightParen
    4     18 RightParen
    5      9 Keyword_print
    5     14 LeftParen
    5     15 String          "FizzBuzz"
    5     25 RightParen
    5     26 Semicolon
    6      5 Keyword_else
    6     10 Keyword_if
    6     13 LeftParen
    6     14 Op_not
    6     15 LeftParen
    6     16 Identifier      i
    6     18 Op_mod
    6     20 Integer             3
    6     21 RightParen
    6     22 RightParen
    7      9 Keyword_print
    7     14 LeftParen
    7     15 String          "Fizz"
    7     21 RightParen
    7     22 Semicolon
    8      5 Keyword_else
    8     10 Keyword_if
    8     13 LeftParen
    8     14 Op_not
    8     15 LeftParen
    8     16 Identifier      i
    8     18 Op_mod
    8     20 Integer             5
    8     21 RightParen
    8     22 RightParen
    9      9 Keyword_print
    9     14 LeftParen
    9     15 String          "Buzz"
    9     21 RightParen
    9     22 Semicolon
   10      5 Keyword_else
   11      9 Keyword_print
   11     14 LeftParen
   11     15 Identifier      i
   11     16 RightParen
   11     17 Semicolon
   13      5 Keyword_print
   13     10 LeftParen
   13     11 String          "\n"
   13     15 RightParen
   13     16 Semicolon
   14      5 Identifier      i
   14      7 Op_assign
   14      9 Identifier      i
   14     11 Op_add
   14     13 Integer             1
   14     14 Semicolon
   15      1 RightBrace
   16      1 End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
;
Assign
Identifier     i
Integer        1
While
LessEqual
Identifier     i
Integer        100
Sequence
Sequence
Sequence
;
If
Not
Mod
Identifier     i
Integer        15
;
If
Sequence
;
Prts
String         "FizzBuzz"
;
If
Not
Mod
Identifier     i
Integer        3
;
If
Sequence
;
Prts
String         "Fizz"
;
If
Not
Mod
Identifier     i
Integer        5
;
If
Sequence
;
Prts
String         "Buzz"
;
Sequence
;
Prti
Identifier     i
;
Sequence
;
Prts
String         "\n"
;
Assign
Identifier     i
Add
Identifier     i
Integer        1

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 1 Strings: 4
"FizzBuzz"
"Fizz"
"Buzz"
"\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 push  100
   20 le
   21 jz     (121) 143
   26 fetch [0]
   31 push  15
   36 mod
   37 not
   38 jz     (15) 54
   43 push  0
   48 prts
   49 jmp    (66) 116
   54 fetch [0]
   59 push  3
   64 mod
   65 not
   66 jz     (15) 82
   71 push  1
   76 prts
   77 jmp    (38) 116
   82 fetch [0]
   87 push  5
   92 mod
   93 not
   94 jz     (15) 110
   99 push  2
  104 prts
  105 jmp    (10) 116
  110 fetch [0]
  115 prti
  116 push  3
  121 prts
  122 fetch [0]
  127 push  1
  132 add
  133 store [0]
  138 jmp    (-129) 10
  143 halt

```
</b>
|}


## 99 Bottles of Beer


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/* 99 bottles */
bottles = 99;
while (bottles > 0) {
    print(bottles, " bottles of beer on the wall\n");
    print(bottles, " bottles of beer\n");
    print("Take one down, pass it around\n");
    bottles = bottles - 1;
    print(bottles, " bottles of beer on the wall\n\n");
}

```



| style="vertical-align:top" |
<b>
```txt

    2      1 Identifier      bottles
    2      9 Op_assign
    2     11 Integer            99
    2     13 Semicolon
    3      1 Keyword_while
    3      7 LeftParen
    3      8 Identifier      bottles
    3     16 Op_greater
    3     18 Integer             0
    3     19 RightParen
    3     21 LeftBrace
    4      5 Keyword_print
    4     10 LeftParen
    4     11 Identifier      bottles
    4     18 Comma
    4     20 String          " bottles of beer on the wall\n"
    4     52 RightParen
    4     53 Semicolon
    5      5 Keyword_print
    5     10 LeftParen
    5     11 Identifier      bottles
    5     18 Comma
    5     20 String          " bottles of beer\n"
    5     40 RightParen
    5     41 Semicolon
    6      5 Keyword_print
    6     10 LeftParen
    6     11 String          "Take one down, pass it around\n"
    6     44 RightParen
    6     45 Semicolon
    7      5 Identifier      bottles
    7     13 Op_assign
    7     15 Identifier      bottles
    7     23 Op_subtract
    7     25 Integer             1
    7     26 Semicolon
    8      5 Keyword_print
    8     10 LeftParen
    8     11 Identifier      bottles
    8     18 Comma
    8     20 String          " bottles of beer on the wall\n\n"
    8     54 RightParen
    8     55 Semicolon
    9      1 RightBrace
   10      1 End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
;
Assign
Identifier     bottles
Integer        99
While
Greater
Identifier     bottles
Integer        0
Sequence
Sequence
Sequence
Sequence
Sequence
;
Sequence
Sequence
;
Prti
Identifier     bottles
;
Prts
String         " bottles of beer on the wall\n"
;
Sequence
Sequence
;
Prti
Identifier     bottles
;
Prts
String         " bottles of beer\n"
;
Sequence
;
Prts
String         "Take one down, pass it around\n"
;
Assign
Identifier     bottles
Subtract
Identifier     bottles
Integer        1
Sequence
Sequence
;
Prti
Identifier     bottles
;
Prts
String         " bottles of beer on the wall\n\n"
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 1 Strings: 4
" bottles of beer on the wall\n"
" bottles of beer\n"
"Take one down, pass it around\n"
" bottles of beer on the wall\n\n"
    0 push  99
    5 store [0]
   10 fetch [0]
   15 push  0
   20 gt
   21 jz     (67) 89
   26 fetch [0]
   31 prti
   32 push  0
   37 prts
   38 fetch [0]
   43 prti
   44 push  1
   49 prts
   50 push  2
   55 prts
   56 fetch [0]
   61 push  1
   66 sub
   67 store [0]
   72 fetch [0]
   77 prti
   78 push  3
   83 prts
   84 jmp    (-75) 10
   89 halt

```
</b>
|}


;And the output is:

```txt

>lex ..\bottles.t | parse | gen | vm
99 bottles of beer on the wall
99 bottles of beer
Take one down, pass it around
98 bottles of beer on the wall

98 bottles of beer on the wall
98 bottles of beer
Take one down, pass it around
97 bottles of beer on the wall

...

2 bottles of beer on the wall
2 bottles of beer
Take one down, pass it around
1 bottles of beer on the wall

1 bottles of beer on the wall
1 bottles of beer
Take one down, pass it around
0 bottles of beer on the wall


```



## Primes


{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

/*
 Simple prime number generator
 */
count = 1;
n = 1;
limit = 100;
while (n < limit) {
    k=3;
    p=1;
    n=n+2;
    while ((k*k<=n) && (p)) {
        p=n/k*k!=n;
        k=k+2;
    }
    if (p) {
        print(n, " is prime\n");
        count = count + 1;
    }
}
print("Total primes found: ", count, "\n");

```



| style="vertical-align:top" |
<b>
```txt

    4      1   Identifier      count
    4      7   Op_assign
    4      9   Integer              1
    4     10   Semicolon
    5      1   Identifier      n
    5      3   Op_assign
    5      5   Integer              1
    5      6   Semicolon
    6      1   Identifier      limit
    6      7   Op_assign
    6      9   Integer            100
    6     12   Semicolon
    7      1   Keyword_while
    7      7   LeftParen
    7      8   Identifier      n
    7     10   Op_less
    7     12   Identifier      limit
    7     17   RightParen
    7     19   LeftBrace
    8      5   Identifier      k
    8      6   Op_assign
    8      7   Integer              3
    8      8   Semicolon
    9      5   Identifier      p
    9      6   Op_assign
    9      7   Integer              1
    9      8   Semicolon
   10      5   Identifier      n
   10      6   Op_assign
   10      7   Identifier      n
   10      8   Op_add
   10      9   Integer              2
   10     10   Semicolon
   11      5   Keyword_while
   11     11   LeftParen
   11     12   LeftParen
   11     13   Identifier      k
   11     14   Op_multiply
   11     15   Identifier      k
   11     16   Op_lessequal
   11     18   Identifier      n
   11     19   RightParen
   11     21   Op_and
   11     24   LeftParen
   11     25   Identifier      p
   11     26   RightParen
   11     27   RightParen
   11     29   LeftBrace
   12      9   Identifier      p
   12     10   Op_assign
   12     11   Identifier      n
   12     12   Op_divide
   12     13   Identifier      k
   12     14   Op_multiply
   12     15   Identifier      k
   12     16   Op_notequal
   12     18   Identifier      n
   12     19   Semicolon
   13      9   Identifier      k
   13     10   Op_assign
   13     11   Identifier      k
   13     12   Op_add
   13     13   Integer              2
   13     14   Semicolon
   14      5   RightBrace
   15      5   Keyword_if
   15      8   LeftParen
   15      9   Identifier      p
   15     10   RightParen
   15     12   LeftBrace
   16      9   Keyword_print
   16     14   LeftParen
   16     15   Identifier      n
   16     16   Comma
   16     18   String          " is prime\n"
   16     31   RightParen
   16     32   Semicolon
   17      9   Identifier      count
   17     15   Op_assign
   17     17   Identifier      count
   17     23   Op_add
   17     25   Integer              1
   17     26   Semicolon
   18      5   RightBrace
   19      1   RightBrace
   20      1   Keyword_print
   20      6   LeftParen
   20      7   String          "Total primes found: "
   20     29   Comma
   20     31   Identifier      count
   20     36   Comma
   20     38   String          "\n"
   20     42   RightParen
   20     43   Semicolon
   21      1   End_of_input


```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    count
Integer       1
Assign
Identifier    n
Integer       1
Assign
Identifier    limit
Integer       100
While
Less
Identifier    n
Identifier    limit
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    k
Integer       3
Assign
Identifier    p
Integer       1
Assign
Identifier    n
Add
Identifier    n
Integer       2
While
And
LessEqual
Multiply
Identifier    k
Identifier    k
Identifier    n
Identifier    p
Sequence
Sequence
;
Assign
Identifier    p
NotEqual
Multiply
Divide
Identifier    n
Identifier    k
Identifier    k
Identifier    n
Assign
Identifier    k
Add
Identifier    k
Integer       2
If
Identifier    p
If
Sequence
Sequence
;
Sequence
Sequence
;
Prti
Identifier    n
;
Prts
String        " is prime\n"
;
Assign
Identifier    count
Add
Identifier    count
Integer       1
;
Sequence
Sequence
Sequence
;
Prts
String        "Total primes found: "
;
Prti
Identifier    count
;
Prts
String        "\n"
;

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 5 Strings: 3
" is prime\n"
"Total primes found: "
"\n"
   0 push  1
   5 store [0]
  10 push  1
  15 store [1]
  20 push  100
  25 store [2]
  30 fetch [1]
  35 fetch [2]
  40 lt
  41 jz     (160) 202
  46 push  3
  51 store [3]
  56 push  1
  61 store [4]
  66 fetch [1]
  71 push  2
  76 add
  77 store [1]
  82 fetch [3]
  87 fetch [3]
  92 mul
  93 fetch [1]
  98 le
  99 fetch [4]
 104 and
 105 jz     (53) 159
 110 fetch [1]
 115 fetch [3]
 120 div
 121 fetch [3]
 126 mul
 127 fetch [1]
 132 ne
 133 store [4]
 138 fetch [3]
 143 push  2
 148 add
 149 store [3]
 154 jmp    (-73) 82
 159 fetch [4]
 164 jz     (32) 197
 169 fetch [1]
 174 prti
 175 push  0
 180 prts
 181 fetch [0]
 186 push  1
 191 add
 192 store [0]
 197 jmp    (-168) 30
 202 push  1
 207 prts
 208 fetch [0]
 213 prti
 214 push  2
 219 prts
 220 halt


```
</b>
|}

;And the output is:

```txt

>lex ..\primes.t | parse | gen | vm
3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26

```




## Ascii Mandlebrot



{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
|-
| style="vertical-align:top" |

```c

{
/*
 This is an integer ascii Mandelbrot generator
 */
    left_edge   = -420;
    right_edge  =  300;
    top_edge    =  300;
    bottom_edge = -300;
    x_step      =    7;
    y_step      =   15;

    max_iter    =  200;

    y0 = top_edge;
    while (y0 > bottom_edge) {
        x0 = left_edge;
        while (x0 < right_edge) {
            y = 0;
            x = 0;
            the_char = ' ';
            i = 0;
            while (i < max_iter) {
                x_x = (x * x) / 200;
                y_y = (y * y) / 200;
                if (x_x + y_y > 800 ) {
                    the_char = '0' + i;
                    if (i > 9) {
                        the_char = '@';
                    }
                    i = max_iter;
                }
                y = x * y / 100 + y0;
                x = x_x - y_y + x0;
                i = i + 1;
            }
            putc(the_char);
            x0 = x0 + x_step;
        }
        putc('\n');
        y0 = y0 - y_step;
    }
}

```



| style="vertical-align:top" |
<b>
```txt

    1      1 LeftBrace
    5      5 Identifier      left_edge
    5     17 Op_assign
    5     19 Op_subtract
    5     20 Integer           420
    5     23 Semicolon
    6      5 Identifier      right_edge
    6     17 Op_assign
    6     20 Integer           300
    6     23 Semicolon
    7      5 Identifier      top_edge
    7     17 Op_assign
    7     20 Integer           300
    7     23 Semicolon
    8      5 Identifier      bottom_edge
    8     17 Op_assign
    8     19 Op_subtract
    8     20 Integer           300
    8     23 Semicolon
    9      5 Identifier      x_step
    9     17 Op_assign
    9     22 Integer             7
    9     23 Semicolon
   10      5 Identifier      y_step
   10     17 Op_assign
   10     21 Integer            15
   10     23 Semicolon
   12      5 Identifier      max_iter
   12     17 Op_assign
   12     20 Integer           200
   12     23 Semicolon
   14      5 Identifier      y0
   14      8 Op_assign
   14     10 Identifier      top_edge
   14     18 Semicolon
   15      5 Keyword_while
   15     11 LeftParen
   15     12 Identifier      y0
   15     15 Op_greater
   15     17 Identifier      bottom_edge
   15     28 RightParen
   15     30 LeftBrace
   16      9 Identifier      x0
   16     12 Op_assign
   16     14 Identifier      left_edge
   16     23 Semicolon
   17      9 Keyword_while
   17     15 LeftParen
   17     16 Identifier      x0
   17     19 Op_less
   17     21 Identifier      right_edge
   17     31 RightParen
   17     33 LeftBrace
   18     13 Identifier      y
   18     15 Op_assign
   18     17 Integer             0
   18     18 Semicolon
   19     13 Identifier      x
   19     15 Op_assign
   19     17 Integer             0
   19     18 Semicolon
   20     13 Identifier      the_char
   20     22 Op_assign
   20     24 Integer            32
   20     27 Semicolon
   21     13 Identifier      i
   21     15 Op_assign
   21     17 Integer             0
   21     18 Semicolon
   22     13 Keyword_while
   22     19 LeftParen
   22     20 Identifier      i
   22     22 Op_less
   22     24 Identifier      max_iter
   22     32 RightParen
   22     34 LeftBrace
   23     17 Identifier      x_x
   23     21 Op_assign
   23     23 LeftParen
   23     24 Identifier      x
   23     26 Op_multiply
   23     28 Identifier      x
   23     29 RightParen
   23     31 Op_divide
   23     33 Integer           200
   23     36 Semicolon
   24     17 Identifier      y_y
   24     21 Op_assign
   24     23 LeftParen
   24     24 Identifier      y
   24     26 Op_multiply
   24     28 Identifier      y
   24     29 RightParen
   24     31 Op_divide
   24     33 Integer           200
   24     36 Semicolon
   25     17 Keyword_if
   25     20 LeftParen
   25     21 Identifier      x_x
   25     25 Op_add
   25     27 Identifier      y_y
   25     31 Op_greater
   25     33 Integer           800
   25     37 RightParen
   25     39 LeftBrace
   26     21 Identifier      the_char
   26     30 Op_assign
   26     32 Integer            48
   26     36 Op_add
   26     38 Identifier      i
   26     39 Semicolon
   27     21 Keyword_if
   27     24 LeftParen
   27     25 Identifier      i
   27     27 Op_greater
   27     29 Integer             9
   27     30 RightParen
   27     32 LeftBrace
   28     25 Identifier      the_char
   28     34 Op_assign
   28     36 Integer            64
   28     39 Semicolon
   29     21 RightBrace
   30     21 Identifier      i
   30     23 Op_assign
   30     25 Identifier      max_iter
   30     33 Semicolon
   31     17 RightBrace
   32     17 Identifier      y
   32     19 Op_assign
   32     21 Identifier      x
   32     23 Op_multiply
   32     25 Identifier      y
   32     27 Op_divide
   32     29 Integer           100
   32     33 Op_add
   32     35 Identifier      y0
   32     37 Semicolon
   33     17 Identifier      x
   33     19 Op_assign
   33     21 Identifier      x_x
   33     25 Op_subtract
   33     27 Identifier      y_y
   33     31 Op_add
   33     33 Identifier      x0
   33     35 Semicolon
   34     17 Identifier      i
   34     19 Op_assign
   34     21 Identifier      i
   34     23 Op_add
   34     25 Integer             1
   34     26 Semicolon
   35     13 RightBrace
   36     13 Keyword_putc
   36     17 LeftParen
   36     18 Identifier      the_char
   36     26 RightParen
   36     27 Semicolon
   37     13 Identifier      x0
   37     16 Op_assign
   37     18 Identifier      x0
   37     21 Op_add
   37     23 Identifier      x_step
   37     29 Semicolon
   38      9 RightBrace
   39      9 Keyword_putc
   39     13 LeftParen
   39     14 Integer            10
   39     18 RightParen
   39     19 Semicolon
   40      9 Identifier      y0
   40     12 Op_assign
   40     14 Identifier      y0
   40     17 Op_subtract
   40     19 Identifier      y_step
   40     25 Semicolon
   41      5 RightBrace
   42      1 RightBrace
   43      1 End_of_input

```
</b>


| style="vertical-align:top" |
<b>
```txt

Sequence
;
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     left_edge
Negate
Integer        420
;
Assign
Identifier     right_edge
Integer        300
Assign
Identifier     top_edge
Integer        300
Assign
Identifier     bottom_edge
Negate
Integer        300
;
Assign
Identifier     x_step
Integer        7
Assign
Identifier     y_step
Integer        15
Assign
Identifier     max_iter
Integer        200
Assign
Identifier     y0
Identifier     top_edge
While
Greater
Identifier     y0
Identifier     bottom_edge
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     x0
Identifier     left_edge
While
Less
Identifier     x0
Identifier     right_edge
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     y
Integer        0
Assign
Identifier     x
Integer        0
Assign
Identifier     the_char
Integer        32
Assign
Identifier     i
Integer        0
While
Less
Identifier     i
Identifier     max_iter
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     x_x
Divide
Multiply
Identifier     x
Identifier     x
Integer        200
Assign
Identifier     y_y
Divide
Multiply
Identifier     y
Identifier     y
Integer        200
If
Greater
Add
Identifier     x_x
Identifier     y_y
Integer        800
If
Sequence
Sequence
Sequence
;
Assign
Identifier     the_char
Add
Integer        48
Identifier     i
If
Greater
Identifier     i
Integer        9
If
Sequence
;
Assign
Identifier     the_char
Integer        64
;
Assign
Identifier     i
Identifier     max_iter
;
Assign
Identifier     y
Add
Divide
Multiply
Identifier     x
Identifier     y
Integer        100
Identifier     y0
Assign
Identifier     x
Add
Subtract
Identifier     x_x
Identifier     y_y
Identifier     x0
Assign
Identifier     i
Add
Identifier     i
Integer        1
Prtc
Identifier     the_char
;
Assign
Identifier     x0
Add
Identifier     x0
Identifier     x_step
Prtc
Integer        10
;
Assign
Identifier     y0
Subtract
Identifier     y0
Identifier     y_step

```
</b>


| style="vertical-align:top" |
<b>
```txt

Datasize: 15 Strings: 0
    0 push  420
    5 neg
    6 store [0]
   11 push  300
   16 store [1]
   21 push  300
   26 store [2]
   31 push  300
   36 neg
   37 store [3]
   42 push  7
   47 store [4]
   52 push  15
   57 store [5]
   62 push  200
   67 store [6]
   72 fetch [2]
   77 store [7]
   82 fetch [7]
   87 fetch [3]
   92 gt
   93 jz     (329) 423
   98 fetch [0]
  103 store [8]
  108 fetch [8]
  113 fetch [1]
  118 lt
  119 jz     (276) 396
  124 push  0
  129 store [9]
  134 push  0
  139 store [10]
  144 push  32
  149 store [11]
  154 push  0
  159 store [12]
  164 fetch [12]
  169 fetch [6]
  174 lt
  175 jz     (193) 369
  180 fetch [10]
  185 fetch [10]
  190 mul
  191 push  200
  196 div
  197 store [13]
  202 fetch [9]
  207 fetch [9]
  212 mul
  213 push  200
  218 div
  219 store [14]
  224 fetch [13]
  229 fetch [14]
  234 add
  235 push  800
  240 gt
  241 jz     (56) 298
  246 push  48
  251 fetch [12]
  256 add
  257 store [11]
  262 fetch [12]
  267 push  9
  272 gt
  273 jz     (14) 288
  278 push  64
  283 store [11]
  288 fetch [6]
  293 store [12]
  298 fetch [10]
  303 fetch [9]
  308 mul
  309 push  100
  314 div
  315 fetch [7]
  320 add
  321 store [9]
  326 fetch [13]
  331 fetch [14]
  336 sub
  337 fetch [8]
  342 add
  343 store [10]
  348 fetch [12]
  353 push  1
  358 add
  359 store [12]
  364 jmp    (-201) 164
  369 fetch [11]
  374 prtc
  375 fetch [8]
  380 fetch [4]
  385 add
  386 store [8]
  391 jmp    (-284) 108
  396 push  10
  401 prtc
  402 fetch [7]
  407 fetch [5]
  412 sub
  413 store [7]
  418 jmp    (-337) 82
  423 halt

```
</b>
|}

;And the output is:

```txt

>lex ..\mandel.t | parse | gen | vm
1111111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222211111
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333222222222222222222222222222222222222222
1111111111111222222222233333333333333333333333344444456655544443333332222222222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
11111111111222222333333333333333333333334444444445555679@@@@7654444443333333222222222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98755544444433333332222222222222222222222222
1111111122223333333333333333333333344444444445556668@@@    @@@76555544444333333322222222222222222222222
1111111222233333333333333333333344444444455566667778@@      @987666555544433333333222222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@877779@5443333333322222222222222222222
1111112233333333333333333334444455555556679@   @@@               @@@@@@ 8544333333333222222222222222222
1111122333333333333333334445555555556666789@@@                        @86554433333333322222222222222222
1111123333333333333444456666555556666778@@ @                         @@87655443333333332222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65444333333332222222222222222
111133334444444455555668@@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
111133444444445555556778@@@         @@@@                                @855444333333333222222222222222
11124444444455555668@99@@             @                                 @655444433333333322222222222222
11134555556666677789@@                                                @86655444433333333322222222222222
111                                                                 @@876555444433333333322222222222222
11134555556666677789@@                                                @86655444433333333322222222222222
11124444444455555668@99@@             @                                 @655444433333333322222222222222
111133444444445555556778@@@         @@@@                                @855444333333333222222222222222
111133334444444455555668@@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65444333333332222222222222222
1111123333333333333444456666555556666778@@ @                         @@87655443333333332222222222222222
1111122333333333333333334445555555556666789@@@                        @86554433333333322222222222222222
1111112233333333333333333334444455555556679@   @@@               @@@@@@ 8544333333333222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@877779@5443333333322222222222222222222
1111111222233333333333333333333344444444455566667778@@      @987666555544433333333222222222222222222222
1111111122223333333333333333333333344444444445556668@@@    @@@76555544444333333322222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98755544444433333332222222222222222222222222
11111111111222222333333333333333333333334444444445555679@@@@7654444443333333222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
1111111111111222222222233333333333333333333333344444456655544443333332222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333222222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211

```

