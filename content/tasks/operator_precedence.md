+++
title = "Operator precedence"
description = ""
date = 2019-09-19T21:31:03Z
aliases = []
[extra]
id = 12237
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "8th",
  "algol_60",
  "algol_68",
  "algol_w",
  "awk",
  "bc",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "d",
  "eiffel",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "futurebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lil",
  "lua",
  "mathematica",
  "matlab",
  "nim",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "purebasic",
  "python",
  "q",
  "racket",
  "realbasic",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "scilab",
  "seed7",
  "sidef",
  "simula",
  "tcl",
  "vbscript",
  "visual_basic",
  "visual_basic_dotnet",
  "xpl0",
  "zkl",
]
+++

{{task}} [[Category:Simple]]
## Task

Provide a list of   [[wp:order of operations|precedence]]   and   [[wp:operator associativity|associativity]]   of all the operators and constructs that the language utilizes in descending order of precedence such that an operator which is listed on some row will be evaluated prior to any operator that is listed on a row further below it. 

Operators that are in the same cell (there may be several rows of operators listed in a cell) are evaluated with the same level of precedence, in the given direction. 

State whether arguments are passed by value or by reference.





## 8th

In 8th it's very simple: the currently invoked word has precedence, and items are operated upon in the order they are popped off the stack.

## ALGOL 60

{| class="wikitable"
! Priority || Description               || Operators                     || Associativity
|-
| highest  ||                           ||                               || 
|-
| 1        || power                     ||<code> 'POWER'          </code>|| left
|-
| 2        || unary operator (opposite) ||<code> + -              </code>|| left
|-
| 3        || multiplication & division ||<code> * /  '/'         </code>|| left
|-
| 4        || addition & subtraction    ||<code> + -              </code>|| left
|-
| 5        || comparison                ||<code> 'EQUAL' 'NOT EQUAL' 
 'LESS' 'NOT LESS' 
 'GREATER' 'NOT GREATER'  </code>|| left 
|-
| 6        || logical NOT               ||<code> 'NOT'            </code>|| left 
|-
| 7        || logical AND               ||<code> 'AND'            </code>|| left 
|-
| 8        || logical OR                ||<code> 'OR'             </code>|| left 
|-
| 9        || equivalence               ||<code> 'EQUIV'          </code>|| left 
|-
| 10       || implication               ||<code> 'IMPL'           </code>|| left 
|-
| lowest   ||                           ||                               || 
|}
Note: '/' is the Euclidean division


## ALGOL 68

The coder may define new '''[[wp:Operator (programming)|operators]]''' and ''both'' those and the pre-defined ones may be [[wp:overloading (programming)|overloaded]] and their priorities may be changed. 
====Array, Procedure, Dereference, Selection and Generator operations====
{|class="wikitable"
!bgcolor=#cccccc|'''prio'''rity
!bgcolor=#cccccc| Operation
!bgcolor=#cccccc|+Algol68<sup>Rev0</sup>
!bgcolor=#cccccc|+Algol68<sup>G</sup>
|-
|ALIGN=CENTER| Effectively 12
(Primary) || dereferencing, deproceduring(~,~), subscripting[~], rowing[~,] & slicing[~:~]
||
|ALIGN=CENTER|[[currying]](~,), '''diag''', '''trnsp''', '''row''', '''col'''
|-
|ALIGN=CENTER| Effectively 11
(Secondary) || '''of''' (selection), '''loc''' & '''heap''' (generators)
||→
||'''new''' (generator)
|}
These are technically not operators, rather they are considered "[http://vestein.arb-phys.uni-dortmund.de/~wb/RR/rr52.html units associated with names]"
<!-- '''diag''', '''trnsp''', '''row''', '''col''' as per S. G. van der Meulen, M. Veldhorst. TORRIX - A programming language for operations on vectors and matrices over arbitrary ﬁelds and of variable size. Rijksuniversiteit Utrecht [1977]. -->


### =Monadic operators=

{|class="wikitable"
!bgcolor=#cccccc|'''prio'''rity
(Tertiary)
!bgcolor=#cccccc|Algol68 "Worthy characters"
!bgcolor=#cccccc|+Algol68<sup>Rev0&1</sup>
!bgcolor=#cccccc|+Algol68<sup>C,G</sup>
!bgcolor=#cccccc|+Algol68<sup>Rev0</sup>
|-
|ALIGN=CENTER| 10 || '''not''', '''up''', '''down''', '''lwb''', '''upb''', 
-, '''abs''', '''arg''', '''bin''', '''entier''', '''leng''', '''level''', '''odd''', '''repr''', '''round''', '''shorten'''
| ¬, ↑, ↓, ⌊, ⌈
| ~, '''norm''', '''trace''', '''t''', '''det''', '''inv'''
| '''lws''', '''ups''', ⎩, ⎧, '''btb''', '''ctb'''
|}
<!-- '''norm''', '''trace''', '''t''', '''det''', '''inv''' as per S. G. van der Meulen, M. Veldhorst. TORRIX - A programming language for operations on vectors and matrices over arbitrary ﬁelds and of variable size. Rijksuniversiteit Utrecht [1977]. -->


### =Standard dyadic operators with associated priorities=

{|class="wikitable"
!bgcolor=#cccccc|'''prio'''rity
(Tertiary)
!bgcolor=#cccccc|Algol68 "Worthy characters"
!bgcolor=#cccccc|+Algol68<sup>Rev0&1</sup>
!bgcolor=#cccccc|+Algol68<sup>C,G</sup>
!bgcolor=#cccccc|+Algol68<sup>Rev0</sup>
|-
|ALIGN=CENTER|9||+*, '''i'''||+&times;, ⊥||  || !
|-
|ALIGN=CENTER|8|| '''shl''', '''shr''', **,  '''up''', '''down''', '''lwb''', '''upb''' || ↑, ↓, ⌊, ⌈ || || '''lws''', '''ups''', ⎩, ⎧
|-
|ALIGN=CENTER|7|| *, /,  %, '''over''',  %*, '''mod''', '''elem'''|| &times;, ÷, ÷&times;, ÷*, %&times;, □ ||  || ÷:
|-
|ALIGN=CENTER|6||-, + || ||  ||
|-
|ALIGN=CENTER|5||<, '''lt''', <=, '''le''',  >=, '''ge''', >, '''gt'''|| ≤, ≥ || ||
|-
|ALIGN=CENTER|4||=, '''eq''', /=, '''ne''' || ≠ || ~=  ||
|-
|ALIGN=CENTER|3||&, '''and'''|| ∧ || || [[wp:/\|/\]]
|-
|ALIGN=CENTER|2||'''or'''|| ∨ || || [[wp:\/|\/]]
|-
|ALIGN=CENTER|1||'''minusab''', '''plusab''', '''timesab''', '''divab''', '''overab''', '''modab''', '''plusto''', 
-:=, +:=, *:=, /:=, %:=, %*:=, +=: 
|| &times;:=, ÷:=, ÷&times;:=,  ÷*:=,  %&times;:=|| ||'''minus''', '''plus''', '''div''', '''overb''', '''modb''', ÷::=, '''prus'''
|}
Note: Tertiaries include names '''nil''' and ○.


### =Assignation and identity relations etc=

Again, these are technically not operators, rather they are considered "[http://vestein.arb-phys.uni-dortmund.de/~wb/RR/rr52.html units associated with names]"
{|class="wikitable"
!bgcolor=#cccccc|'''prio'''rity
(Quaternaries)
!bgcolor=#cccccc|Algol68 "Worthy characters"
!bgcolor=#cccccc|+Algol68<sup>Rev0&1</sup>
!bgcolor=#cccccc|+Algol68<sup>C</sup>
!bgcolor=#cccccc|+Algol68<sup>Rev0</sup>
|-
|ALIGN=CENTER| Effectively 0 || :=, =:, = , :=:, :/=:, '''is''', '''isnt''', '''at''' , @|| :≠:, : || :~=: || '''ct''', ::, '''ctab''', ::=, .., '''is not'''
|}
Note: Quaternaries include names '''skip''' and ~.

Algol 68 also includes (something like) C's ternary conditions, e.g.:
* '''case''' ~ '''in''' ~ '''ouse''' ~ '''in''' ~ '''out''' ~ '''esac''' or simply "( ~ | ~ |: ~ | ~ | ~ )", 
* '''if''' ~ '''then''' ~ '''elif''' ~ '''then''' ~ '''else''' ~ '''fi''' or simply "( ~ | ~ |: ~ | ~ | ~ )", 
And (unlike C's comma operator) the ";" can be used to indicate statements are done sequentially, where as the "," indicates that the statements can be done "collaterally", e.g. in parallel.  Or a parallel clause can be used to force statements to be executed in parallel, e.g. '''par'''( ~, ~, ... )

Key: The super scripts indicate the following:
* ALGOL 68<sup>Rev0</sup> indicates Algol 68 Final Report (Essentially Revision 0)
* ALGOL 68<sup>Rev0&1</sup> indicates Algol 68 Revised Report (Essentially Revision 1)
* ALGOL 68<sup>C</sup> indicates Cambridge University Algol 68[http://www.cantab.net/users/chris.cheney/Algol68C/].
* ALGOL 68<sup>G</sup> indicates Algol 68 Genie[http://jmvdveer.home.xs4all.nl]


## ALGOL W

{| class="wikitable"
! Priority || Operator          || Description                                 || Associativity || Arity
|-
| highest  || long short abs    ||Widen, narrow, absolute value                || left          || unary
|-
|          || shl shr **        ||left shift, right shift, raise to power      || left          || binary
|-
|          || * / div rem       ||multiply, divide, integer division, remainder|| left          || binary
|-
|          || + -               ||addition, subtraction                        || left          || unary and binary
|-
|          || < <= = ¬= >= > is ||comparison, "is" checks a reference has a particular type   || left          || binary
|-
|          || not               ||logical or bits negation                     || left          || unary
|-
|          || and               ||logical or bits "and"                        || left          || binary
|-
| lowest   || or                ||logical or bits "or"                         || left          || binary
|-
|}


## AWK

See also: [https://www.gnu.org/software/gawk/manual/html_node/Precedence.html#Precedence gawk-Reference]

```AWK

# Operators are shown in decreasing order of precedence.
# A blank line separates groups of operators with equal precedence.
# All operators are left associative except:
# . assignment operators
# . conditional operator
# . exponentiation
# which are right associative.
#
#   ( )  grouping
#
#   $    field reference
#
#   ++   increment (both prefix and postfix)
#   --   decrement (both prefix and postfix)
#
#   ^    exponentiation
#   **   exponentiation (not all awk's)
#
#   +    unary plus
#   -    unary minus
#   !    logical NOT
#
#   *    multiply
#   /    divide
#   %    modulus
#
#   +    add
#   -    subtract
#
#        string concatenation has no explicit operator
#
#   <    relational: less than
#   <=   relational: less than or equal to
#   >    relational: greater than
#   >=   relational: greater than or equal to
#   !=   relational: not equal to
#   ==   relational: equal to
#   >    redirection: output to file
#   >>   redirection: append output to file
#   |    redirection: pipe
#   |&   redirection: coprocess (not all awk's)
#
#   ~    regular expression: match
#   !~   regular expression: negated match
#
#   in   array membership
#
#   &&   logical AND
#
#   ||   logical OR
#
#   ?:   conditional expression
#
#   =    assignment
#   +=   addition assignment
#   -=   subtraction assignment
#   *=   multiplication assignment
#   /=   division assignment
#   %=   modulo assignment
#   ^=   exponentiation assignment
#   **=  exponentiation assignment (not all awk's)

```



## bc

From the [http://pubs.opengroup.org/onlinepubs/9699919799/utilities/bc.html POSIX Standard], ordered by decreasing precedence:
{| class="wikitable"
! Precedence !! Operator(s) !! Description !! Associativity
|-
| Highest || ++, -- || Prefix/Postfix Increment/Decrement || n/a
|-
| || unary - || Negation || n/a
|-
| || ^ || Exponentiation || Right to left
|-
| || *, /, % || Multiplication, Division, Remainder || Left to right
|-
| || +, binary - || Addition, Subtraction || Left to right
|-
| || =, +=, -=, *=, /=, %=, ^= || Assignment || Right to left
|-
| || ==, <=, >=, !=, <, > || Comparison || None
|-
| colspan=4 | {{Works with|GNU bc}}
|-
| || ! || Logical Not || n/a
|-
| || && || Logical And || Left to right
|-
| Lowest || <nowiki>||</nowiki> || Logical Or || Left to right
|}


## Bracmat

Bracmat has 15 binary operators and 12 unary operators, not counting the minus

The binary operators have a simple precedence order and all binary operators are right-associative.

{| class="wikitable"
! Precedence !! Operator !! Description !! Example !! Note
|-
| Highest || <code>_</code> || In pattern: matches any binary operator. Outside pattern: evaluates to last matched operator || <code>a*b:?x_?y & p_q</code> becomes <code>p*q</code> 
|-
| || <code>$</code> || Function application. Evaluates rhs and then applies function on lhs || <code>put$!x</code>
|-
| || <code>'</code> || Function application. Applies function on lhs to unevaluated rhs || <code>apply'(?a 13 ?z)</code>
|-
| || <code>\D</code> || Symbolic differentiation || <code>x\D(x^2)</code> differentiates <code>x^2</code> to <code>x</code> 
|-
| || <code>\L</code> || Logarithm || <code>e\Ly</code> is the natural logarithm of <code>y</code>
|-
| || <code>^</code> || Exponentiation || <code>x^2</code> is the square of <code>x</code>
|-
| || <code>*</code> || Multiplication || <code>a*b</code> is the product of <code>a</code> and <code>b</code> || neutral element: <code>1</code>
|-
| || <code>+</code> || Addition || <code>a+b</code> is the sum of <code>a</code> and <code>b</code> || neutral element: <code>0</code>
|-
| || white space || constructs a white space separated list || <code>a b</code>  || neutral element: empty string <code>""</code>
|-
| || <code>:</code> || Match subject on the left with pattern on the right || <code>a b c d:? c ?</code>
|-
| || <code>&amp;</code> || "and then" || <code>1:2 &amp; out$"The end of time is nearing"</code> 
|-
| || <nowiki>|</nowiki> || "or else" || <code>1:2 </code><nowiki>|</nowiki> <code>out$"Don't think so"</code> 
|-
| || <code>,</code> || constructs a comma separated list || <code>1,2,3</code>
|-
| || <code>.</code> || constructs a dot separated list or tree in general || <code>(aa.bb).cc.dd</code> 
|-
| Lowest || <code>=</code> || define || <code>swap=a b.!arg:(?a.?b)&amp;(!b.!a)</code>
|}

Precedence among unary operators is a mixed bag. Unary operators can modify something that is to the right of the unary operator, which can be another unary operator, a string or a binary operator.

Of the unary operators <code>?</code> and <code>!</code> (or <code>!!</code>), the latter have the higher priority. So <code>17:?!x</code> assigns the value <code>17</code> to the variable that happens to be the value of <code>x</code>. (Like <code>*x = 17</code> in C). (The unary operators <code>!</code> and <code>!!</code> cannot be combined and are reduced to <code>!!</code>)

The negation operator <code>~</code> negates the first of the unary operators <code>/ # < > % @</code> that is present. If none of these unary operators is present, the negation operator negates the node (string or expression with binary operator) itself, with the meaning "not equal to". Bang operators <code>!</code> or <code>!!</code> turn their operand into a variable that is supposed to have a value. The negation operator operates on the value (direct resp. indirect) of a variable, not on the variable name itself.

The combination <code>~<></code> has to be read in one piece, meaning "not unequal", which is not quite unequal to <code>~</code>. It is used for case insensitive matching.

Some combinations of unary operators have currently no meaning and are silently reinterpreted: <code>~?x</code> is the same as <code>?x</code> and <code>~?!x</code> is the same as <code>?!x</code>. But <code>~#?x</code> is not the same as <code>#?x</code>.

The <code>[</code> and <code>`</code> (grave accent) are outside any considerations of precedence.

If you are wondering what the discussed unary operators are for, see this table:

{| class="wikitable"
! Operator !! Description !! Example
|-
| <code>!</code> or <code>!!</code> || retrieves value || <code>(x=17)&amp;!x</code> becomes <code>17</code> 
|-
| <code>?</code> || (In a pattern) Assigns value. Or is a wildcard. || <code>1 2 3:?x 3 ? &amp; !x</code> becomes <code>1 2</code> 
|-
| <code>`</code> || Cuts corners, like <code>FENCE</code> in SNOBOL4 or <code>!</code> in Prolog || <code>1 2 3:? `?x 3</code> assigns <code>2</code> to <code>x</code>, because assigning <code>1 2</code> is never tried. 
|-
| <code>@</code> || Accept atomic subjects only || <code>2*a+3*b+c+6*d:?+@+?</code> succeeds, because <code>c</code> is atomic. 
|-
| <code>%</code> || Accept anything but neutral element || <code>8 2 3:%?x ?</code> assigns <code>8</code> to <code>x</code>
|-
| <code>&gt;</code> ( <code>&lt;</code> ) || Greater (less) than || <code>1 2 3 4:? >%@2 ?x</code> assigns <code>4</code> to <code>x</code>
|-
| <code>#</code> || Accept only a number || <code>a:#*?x</code> assigns <code>a</code> to <code>x</code> and matches <code>1</code> with <code>#</code>
|-
| <code>/</code> || Accept only a non-integer number || <code>1 2 5/2 3:? /?x ?</code> assigns <code>5/2</code> to <code>x</code>
|-
| <code>~</code> || Negate || <code>a:~b</code> (succeeds) <code>5:~&lt;5</code> (succeeds) 
|-
| <code>[</code> || Catches position in subject rather than part of the subject itself ||  <code>a b c d:? [?p&amp;!p</code> gives <code>4</code>, the length of the subject in number of elements. <code>a b c d:? [2 ?x</code> assigns <code>c d</code> to <code>x</code>.
|}



## C

Same as [http://rosettacode.org/wiki/Operator_precedence#C.2B.2B|See C++].


## C++

The following is a table that lists the [[wp:order of operations|precedence]] and [[wp:operator associativity|associativity]] of all the operators in the [[wp:C (programming language)|C]] and [[wp:C++|C++]] languages. An operator's precedence is unaffected by overloading.

{| class="wikitable"
|-
! style="text-align: left" | Precedence
! style="text-align: left" | Operator
! style="text-align: left" | Description
! style="text-align: left" | Associativity
|-
! 1
<small>highest</small>
| <code>::</code>
| Scope resolution (C++ only)
| style="vertical-align: top" rowspan="12" | Left-to-right
|-
! rowspan=11| 2
| style="border-bottom-style: none" | <code>++</code>
| style="border-bottom-style: none" | Suffix increment
|-
| style="border-bottom-style: none; border-top-style: none" | <code>--</code>
| style="border-bottom-style: none; border-top-style: none" | Suffix decrement
|-
| style="border-bottom-style: none; border-top-style: none" | <code>()</code>
| style="border-bottom-style: none; border-top-style: none" | Function call
|-
| style="border-bottom-style: none; border-top-style: none" | <code>[]</code>
| style="border-bottom-style: none; border-top-style: none" | Array subscripting
|-
| style="border-bottom-style: none; border-top-style: none" | <code>.</code>
| style="border-bottom-style: none; border-top-style: none" | Element selection by reference
|-
| style="border-bottom-style: none; border-top-style: none" | <code>-&gt;</code>
| style="border-bottom-style: none; border-top-style: none" | Element selection through pointer
|-
| style="border-bottom-style: none; border-top-style: none" | <code>typeid()</code>
| style="border-bottom-style: none; border-top-style: none" | [[wp:Run-time type information|Run-time type information]] (C++ only) (see [[wp:typeid|typeid]])
|-
| style="border-bottom-style: none; border-top-style: none" | <code>const_cast</code>
| style="border-bottom-style: none; border-top-style: none" | Type cast (C++ only) (see [[wp:const cast|const cast]])
|-
| style="border-bottom-style: none; border-top-style: none" | <code>dynamic_cast</code>
| style="border-bottom-style: none; border-top-style: none" | Type cast (C++ only) (see [[wp:dynamic_cast|dynamic cast]])
|-
| style="border-bottom-style: none; border-top-style: none" | <code>reinterpret_cast</code>
| style="border-bottom-style: none; border-top-style: none" | Type cast (C++ only) (see [[wp:reinterpret cast|reinterpret cast]])
|-
| style="border-top-style: none" | <code>static_cast</code>
| style="border-top-style: none" | Type cast (C++ only) (see [[wp:static cast|static cast]])
|-
! rowspan=12| 3
| style="border-bottom-style: none" | <code>++</code>
| style="border-bottom-style: none" | Prefix increment
| style="vertical-align: top" rowspan="12" | Right-to-left
|-
| style="border-bottom-style: none; border-top-style: none" | <code>--</code>
| style="border-bottom-style: none; border-top-style: none" | Prefix decrement
|-
| style="border-bottom-style: none; border-top-style: none" | <code>+</code>
| style="border-bottom-style: none; border-top-style: none" | Unary plus
|-
| style="border-bottom-style: none; border-top-style: none" | <code>-</code>
| style="border-bottom-style: none; border-top-style: none" | Unary minus
|-
| style="border-bottom-style: none; border-top-style: none" | <code>!</code>
| style="border-bottom-style: none; border-top-style: none" | Logical NOT
|-
| style="border-bottom-style: none; border-top-style: none" | <code>~</code>
| style="border-bottom-style: none; border-top-style: none" | Bitwise NOT
|-
| style="border-bottom-style: none; border-top-style: none" | <code>(''type'')</code>
| style="border-bottom-style: none; border-top-style: none" | Type cast
|-
| style="border-bottom-style: none; border-top-style: none" | <code>*</code>
| style="border-bottom-style: none; border-top-style: none" | Indirection (dereference)
|-
| style="border-bottom-style: none; border-top-style: none" | <code>&</code>
| style="border-bottom-style: none; border-top-style: none" | Address-of 
|-
| style="border-bottom-style: none; border-top-style: none" | <code>sizeof</code>
| style="border-bottom-style: none; border-top-style: none" | [[wp:sizeof|Size-of]]
|-
| style="border-bottom-style: none; border-top-style: none" | <code>new</code>, <code>new[]</code>
| style="border-bottom-style: none; border-top-style: none" | Dynamic memory allocation (C++ only)
|-
| style="border-top-style: none" | <code>delete</code>, <code>delete[]</code>
| style="border-top-style: none" | Dynamic memory deallocation (C++ only)
|-
! rowspan=2| 4
| style="border-bottom-style: none" | <code>.*</code>
| style="border-bottom-style: none" | Pointer to member (C++ only)
| style="vertical-align: top" rowspan="20" | Left-to-right
|-
| style="border-bottom-style: none; border-top-style: none" | <code>->*</code>
| style="border-bottom-style: none; border-top-style: none" | Pointer to member (C++ only)
|-
! rowspan=3| 5
| style="border-bottom-style: none" | <code>*</code>
| style="border-bottom-style: none" | Multiplication
|-
| style="border-bottom-style: none; border-top-style: none" | <code>/</code>
| style="border-bottom-style: none; border-top-style: none" | Division
|-
| style="border-bottom-style: none; border-top-style: none" | <code>%</code>
| style="border-bottom-style: none; border-top-style: none" | [[wp:Modulo operation|Modulo]] (remainder)
|-
! rowspan=2| 6
| style="border-bottom-style: none" | <code>+</code>
| style="border-bottom-style: none" | Addition
|-
| style="border-bottom-style: none; border-top-style: none" | <code>-</code>
| style="border-bottom-style: none; border-top-style: none" | Subtraction
|-
! rowspan=2| 7
| style="border-bottom-style: none" | <code>&lt;&lt;</code>
| style="border-bottom-style: none" | [[wp:Bitwise operation|Bitwise]] left shift
|-
| style="border-bottom-style: none; border-top-style: none" | <code>&gt;&gt;</code>
| style="border-bottom-style: none; border-top-style: none" | [[wp:Bitwise operation|Bitwise]] right shift
|-
! rowspan=4| 8
| style="border-bottom-style: none" | <code>&lt;</code>
| style="border-bottom-style: none" | Less than
|-
| style="border-bottom-style: none; border-top-style: none" | <code>&lt;=</code>
| style="border-bottom-style: none; border-top-style: none" | Less than or equal to
|-
| style="border-bottom-style: none; border-top-style: none" | <code>&gt;</code>
| style="border-bottom-style: none; border-top-style: none" | Greater than
|-
| style="border-bottom-style: none; border-top-style: none" | <code>&gt;=</code>
| style="border-bottom-style: none; border-top-style: none" | Greater than or equal to
|-
! rowspan=2| 9
| style="border-bottom-style: none" | <code>==</code>
| style="border-bottom-style: none" | Equal to
|-
| style="border-bottom-style: none; border-top-style: none" | <code>!=</code>
| style="border-bottom-style: none; border-top-style: none" | Not equal to
|-
! 10
| <code>&amp;</code>
| Bitwise AND
|-
! 11
| <code>^</code>
| Bitwise XOR (exclusive or)
|-
! 12
| <code><nowiki>|</nowiki></code>
| Bitwise OR (inclusive or)
|-
! 13
| <code>&amp;&amp;</code>
| Logical AND
|-
! 14
| <code><nowiki>||</nowiki></code>
| Logical OR
|-
! 15
| <code>?:</code>
| [[wp:Ternary operator|Ternary]] conditional (see [[wp:?:|?:]])
| style="vertical-align: top" rowspan="12" | Right-to-left
|-
! rowspan=11| 16
| style="border-bottom-style: none" | <code>=</code>
| style="border-bottom-style: none" | Direct assignment
|-
| style="border-bottom-style: none; border-top-style: none" | <code>+=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by sum
|-
| style="border-bottom-style: none; border-top-style: none" | <code>-=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by difference
|-
| style="border-bottom-style: none; border-top-style: none" | <code>*=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by product
|-
| style="border-bottom-style: none; border-top-style: none" | <code>/=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by quotient
|-
| style="border-bottom-style: none; border-top-style: none" | <code>%=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by remainder
|-
| style="border-bottom-style: none; border-top-style: none" | <code>&lt;&lt;=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by bitwise left shift
|-
| style="border-bottom-style: none; border-top-style: none" | <code>&gt;&gt;=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by bitwise right shift
|-
| style="border-bottom-style: none; border-top-style: none" | <code>&amp;=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by bitwise AND
|-
| style="border-bottom-style: none; border-top-style: none" | <code>^=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by bitwise XOR
|-
| style="border-bottom-style: none; border-top-style: none" | <code><nowiki>|</nowiki>=</code>
| style="border-bottom-style: none; border-top-style: none" | Assignment by bitwise OR
|-
! 17
| <code>throw</code>
| Throw operator (exceptions throwing, C++ only)
|
|-
! 18
| <code>,</code>
| [[wp:Comma operator|Comma]]
| Left-to-right
|}

For quick reference, see also [http://cpp.operator-precedence.com this] equivalent, color-coded table.


=={{header|Caché ObjectScript}}==
There is no operator precedence in COS or in MUMPS.  It is evaluated left-to-right.
Operations can be forced to evaluate in a specific order using parentheses.  Example:
```txt
SAMPLES>write 18 / 2 * 3 + 7
34
SAMPLES>write 18 / (2 * 3) + 7
10        

```



## Clojure

As is the case with LISPs in general, there is no need to worry about operator precedence.  This is one of the benefits of S-Expressions and [[wp:Polish_notation#Computer_programming|prefix notation]].  All functions evaluate left to right and inside out.  The operators in Clojure are just functions, and everything is fully parenthesized.

That being said, there is a macro expansion phase that precedes compilation, and with macros you have great power to change the rules.  A couple of the most common macros with respect to ordering are the [http://clojuredocs.org/clojure_core/clojure.core/-%3E thread-first macro] and the [http://clojuredocs.org/clojure_core/clojure.core/-%3E%3E thread-last macro].  These allow you to order expressions as a chain, which in many cases is preferable for readability.


## COBOL

The following data was derived from [http://www.cobolstandard.info/j4/files/std.zip the 2009 draft COBOL 20XX Standard].


### Arithmetic Expressions

{| class="wikitable"
! Precedence || Operator(s) || Description
|-
! '''Highest'''
| <code>+</code> <code>-</code> || Unary plus and minus
|-
!
| <code>**</code> || Exponentiation
|-
!
| <code>*</code> <code>/</code> || Multiplication and Division
|-
! '''Lowest'''
| <code>+</code> <code>-</code> || Addition and Subtraction
|}


### Boolean Expressions

{| class="wikitable"
! Precedence || Operator || Description
|-
! '''Highest'''
| <code>B-NOT</code> || Negation
|-
!
| <code>B-AND</code> || Conjunction
|-
!
| <code>B-XOR</code> || Exclusive disjunction
|-
! '''Lowest''' 
| <code>B-OR</code> || Inclusive disjunction
|}


### Concatenation Expressions

The <code>&</code> operator is the only operator used in concatenation expressions.


### Logical Expressions

{| class="wikitable"
! Precedence || Operator || Description
|-
! '''Highest'''
| <code>NOT</code> || Logical negation
|-
!
| <code>AND</code> || Logical conjunction
|-
! '''Lowest''' 
| <code>OR</code> || Logical inclusive OR
|}


## Common Lisp

There is no need to worry about operator precedence in Common Lisp and Lisp's in general. Operators (like + - * / ) are normal functions and all of the code is organized as S-expressions with a prefixed polish notation. In result all of the code is parenthesized and the code is evaluated from the innermost S-expression to the outermost S-expression.


## D


A [http://wiki.dlang.org/Operator_precedence copy from the D wiki].

{| class="wikitable"
! Priority || Description              || Operators   || Comments
|-
| 15       || Template instantiation   || !           || Top-level ',' in rhs expression treated specially. Cannot be chained
|-
| 14.5     || Lambda abstraction       || =>          || Not a real operator, occurs twice, this is binding power to the left.
|-
| 14       || Postfix operators        || . ++ -- ( [ || ( and [ treat top-level ',' in rhs expression specially and require balanced ) or ] in order to be completed
|- 
| 13       || Power operator           || ^^          || Right-associative
|-
| 12       || Unary operators          || & ++ -- * + - ! ~ || 
|-
| 11       || -                        || * / %       || 
|-
| 10       || -                        || + - ~       || Binary '~' is the concatenation operator
|-
| 9        || Bit shift operators      || << >> >>>   ||
|-
| 6a       || Comparison operators     || == != > < >= <= !> !< !>= !<= <> !<> <>= !<>= in !in is !is || Unordered with respect to bitwise operators, cannot be chained.
|-
| 8b       || Bitwise AND              || &           || Unordered with respect to comparison operators
|-
| 7b       || Bitwise XOR              || ^           || Unordered with respect to comparison operators
|-
| 6b       || Bitwise OR               || <nowiki>|</nowiki>           || Unordered with respect to comparison operators
|-
| 5        || Logical AND              || &&          || Short-circuit
|-
| 4        || Logical OR               || <nowiki>||</nowiki>        || Short-circuit
|-
| 3        || Conditional operator     || ?:          || Right-associative
|-
| 2        || Assignment operators     || /= &= |= -= += <<= >>= >>>= = *= %= ^= ^^= ~= || Right-associative
|-
| 1.5      || Lambda abstraction       || =>          || Not a real operator, occurs twice, this is binding power to the right ||
|-
| 1        || Comma operator           || ,           || Not to be confused with other uses of ',', though their precedence is the same
|-
| 0        || Range separator          || ..          || Not a real operator, hardwired into syntax at specific points
|}




## Eiffel


Official documentation: [[http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-367.pdf]], section 8.28.5
{| class="wikitable"
! Priority || Operator || Description || Associativity
|-
! rowspan=1| 13
<small>Highest</small>
| <code>.</code>
| Dot notation, in qualified and non-object calls
| 
|-
! rowspan=5| 12
| <code>old</code>
| Used in postconditions to denote the value an expression had before routine entry
| rowspan="5" | 
|-
| <code>not</code>
| Unary negation
|-
| <code>+</code>
| Unary plus
|-
| <code>-</code>
| Unary minus
|- 
| All free unary operators
| Custom unary aliases (See note below table)
|-
! 11
| All free binary operators
| Custom binary aliases (See note below table)
| 
|-
! 10
| <code>^</code>
| Power operator
| Right-to-left
|-
! rowspan=4| 9
| <code>*</code>
| Multiplication
| rowspan="4" | Left-to-right
|-
| <code>/</code>
| Division
|-
| <code>//</code>
| Integer division
|-
| <code>\\</code>
| Integer remainder (modulo)
|-
! rowspan=2| 8
| <code>+</code>
| Addition
| rowspan="2" | Left-to-right
|-
| <code>-</code>
| Subtraction
|-
! 7
| <code>..</code>
| To define an interval
|
|-
! rowspan=8| 6
| <code>=</code>
| Equality (reference)
| rowspan="8" |
|-
| <code>/=</code>
| Inequality (reference)
|-
| <code>~</code>
| Equality (object, uses x.is_equal(y) assuming x /= Void)
|-
| <code>/~</code>
| Inequality (object, uses x.is_equal(y) assuming x /= Void)
|-
| <code><</code>
| Less than
|-
| <code>></code>
| Greater than
|-
| <code><=</code>
| Less than or equal
|-
| <code>>=</code>
| Greater than or equal
|-
! rowspan=2| 5
| <code>and</code>
| Conjunctive Boolean operator (strict)
| rowspan="2" | Left-to-right
|-
| <code>and then</code>
| Conjunctive Boolean operator (semistrict — short-circuit)
|-
! rowspan=3| 4
| <code>or</code>
| Disjunctive Boolean operator (strict)
| rowspan="3" | Left-to-right
|-
| <code>or else</code>
| Disjunctive Boolean operator (semistrict — short-circuit)
|-
| <code>xor</code>
| Exclusive disjunctive Boolean operator
|-
! 3
| <code>implies</code>
| Implicative Boolean operator (( a <code>implies</code> b ) = ( <code>not</code> a <code>or else</code> b ))
| Left-to-right
|-
! 2
| <code>[ ]</code>
| Manifest tuple delimiter 
| 
|-
! 1
<small>Lowest</small>
| <code>;</code>
| Optional semicolon between an assertion clause and the next
| 
|}

Any sequence of free operators (that does not already have a defined meaning, such as <code>=</code> or <code>?</code>) can be used as an alias for unary or binary operations.

The set of free operators is:

```txt
: \ ? = ~ / ! # $ % & * + - < > @ ^ ` | , ' ;
```

Special binary aliases such as <code>()</code> and <code>[]</code> can also be used.

Refer to section 8.32.21 of the aforementioned link for more details.


## Erlang

Official documentation table: [[http://erlang.org/doc/reference_manual/expressions.html]], see "Operator Precedence" towards the end.

=={{header|F_Sharp|F#}}==
[https://msdn.microsoft.com/visualfsharpdocs/conceptual/symbol-and-operator-reference-%5bfsharp%5d#pperator-precedence MSDN documentation]


## Factor

Because Factor uses postfix notation and relies entirely on fixed-argument function composition, all operators have the same precedence. Think of using a calculator that uses reverse-polish notation. Instead of writing <tt>(3+5)*2</tt>, you'd write <tt>3 5 + 2 *</tt>. There is no need for parentheses to clarify the order of operations.


## Forth

Forth as the language of a stack machine does not require operator precedence. Since all arguments for operations are taken from the stack
the order is simply left to right, in the order of the source code. Even the brackets used for the S-expression are not needed with reverse Polish notation.


## Fortran

* Fortran77 [http://www.fortran.com/F77_std/rjcnf0001-sh-6.html#sh-6.5.1 standard].
* Fortran [http://www.starlink.rl.ac.uk/docs/sun190.htx/node115.html]
{| class="wikitable"
!Operators !! Details
|-
|| Function calls ||
|-
|| <code>**</code>    || Numeric
|-
|| <code>*</code>, <code>/</code>   || Numeric
|-
|| <code>+</code>, <code>-</code> ||Unary numeric operators
|-
|| <code>+</code>, <code>-</code> ||Binary numeric operators
|-
|| <code>//</code> || String
|-
|| <code>FROM</code>, <code>TO</code> ||
|-
|| <code>.EQ.</code>, <code>.GE.</code>, <code>.GT.</code>, <code>.LE.</code>, <code>.LT.</code>, <code>.NE.</code>,  <code>==</code>,  <code>>=</code>,  <code>></code>,  <code><=</code>, <code><</code>,  <code>/=</code> || Relational
|-
|| <code>.NOT.</code>, <code>#</code> || Logical
|-
|| <code>.AND.</code>, <code>&</code> || Logical
|-
|| <code>.OR.</code>, | || Logical
|-
|| <code>.EQV.</code>, <code>.NEQV.</code> || Logical
|-
|| <code>,</code> ||
|-
|| <code>(, )</code> ||
|-
|| Start and End of an expression ||
|}


## FutureBasic

When an expression includes more than one operator, the order in which the operations are performed can affect the result. When an operator appears to the left or right of a parenthetical expression, all of the operations within the parentheses are performed first. When several operators all appear within the same matching pair of parentheses (or outside of all parentheses), the order in which their operations are performed is determined by their order of precedence, with "higher precedence" operations being performed before "lower precedence" ones. For example, consider this expression:
4 + 7 * 5
The "*" operator has a higher precedence than the "+" operator (see the table below). So, when this expression is evaluated, first 7 is multiplied by 5 to get 35; then that result is added to 4 to get the final answer of 39.
The following table lists the operators in order of their precedence, from highest to lowest. When an expression contains several operators at the same level of precedence (and within the same depth of parentheses), their operations are always performed from left to right.
{| class="wikitable"
! Precedence
! Operators
|-
| Highest
| Unary operators: +,  -,  ^,  *,  /,  \, \\,  <,  <=,  >,  >=,  =, 
|-
|
| ==,  <>,  !=,  <<,  >>,  Not,  Mod, And,  Or,  Xor,  Nand,  Nor
|-
| 1
| unary "+",  unary "-",  Not
|-
| 2
| ^
|-
| 3
| *,  /,  \,  \\,  Mod
|-
| 4
| + (addition),  - (substraction)
|-
| 5
| <,  <=,  >,  >=,  =,  ==,  <>,  !=,  << (strings),  >> (strings)
|-
| 6
| << (shift left),  >> (shift right)
|-
| 7
| And,  Or,  Xor,  Nand,  Nor
|}



## Go


{| class="wikitable"
! Precedence
! Operators
|-
| Highest
| Unary operators: +, -, !, ^, *, &, <-
|-
| 5
| *,  /,  %,  <<,  >>,  &,  &^
|-
| 4
| +,  -,  <nowiki>|</nowiki>,  ^
|-
| 3
| ==,  !=,  <,  <=,  >,  >=
|-
| 2
| &&
|-
| 1
| <nowiki>||</nowiki>
|}
Binary operators of the same precedence associate from left to right.
Associativity has no meaning for unary operators.

Syntactic elements not in the list are not considered operators in Go; if they present ambiguity in order of evaluation, the ambiguity is resolved by other rules specific to those elements.

=={{header|Icon}} and {{header|Unicon}}==

Taken from http://www.cs.arizona.edu/icon/refernce/exprlist.htm#expressions
(blank lines separate groups of operators with equal precedence):

```txt

        (expr)                          # grouping
        {expr1;expr2;...}               # compound
        x(expr1,expr2,...)              # process argument list
        x{expr1,expr2,...}              # process co-expression list
        [expr1,expr2,...]               # list
        expr.F                          # field reference
        expr1[expr2]                    # subscript
        expr1[expr2,expr3,...]          # multiple subscript
        expr1[expr2:expr3]              # section
        expr1[expr2+:expr3]             # section
        expr1[expr2-:expr3]             # section

        not expr                        # success/failure reversal
        | expr                          # repeated alternation
        ! expr                          # element generation
        * expr                          # size
        + expr                          # numeric value
        - expr                          # negative
        . expr                          # value (dereference)
        / expr                          # null
        \ expr                          # non-null
        = expr                          # match and tab
        ? expr                          # random value
        ~ expr                          # cset complement
        @ expr                          # activation
        ^ expr                          # refresh

        expr1 \ expr2                   # limitation
        expr1 @ expr2                   # transmission
        expr1 ! expr2                   # invocation

        expr1 ^ expr2                   # power

        expr1 * expr2                   # product
        expr1 / expr2                   # quotient
        expr1 % expr2                   # remainder
        expr1 ** expr2                  # intersection

        expr1 + expr2                   # sum
        expr1 - expr2                   # numeric difference
        expr1 ++ expr2                  # union
        expr1 -- expr2                  # cset or set difference

        expr1 || expr2                  # string concatenation
        expr1 ||| expr2                 # list concatenation

        expr1 < expr2                   # numeric comparison
        expr1 <= expr2                  # numeric comparison
        expr1 = expr2                   # numeric comparison
        expr1 >= expr2                  # numeric comparison
        expr1 > expr2                   # numeric comparison
        expr1 ~= expr2                  # numeric comparison
        expr1 << expr2                  # string comparison
        expr1 <<= expr2                 # string comparison
        expr1 == expr2                  # string comparison
        expr1 >>= expr2                 # string comparison
        expr1 >> expr2                  # string comparison
        expr1 ~== expr2                 # string comparison
        expr1 === expr2                 # value comparison
        expr1 ~=== expr2                # value comparison

        expr1 | expr2                   # alternation

        expr1 to expr2 by expr3         # integer generation

        expr1 := expr2                  # assignment
        expr1 <- expr2                  # reversible assignment
        expr1 :=: expr2                 # exchange
        expr1 <-> expr2                 # reversible exchange
        expr1 op:= expr2                # (augmented assignments)

        expr1 ? expr2                   # string scanning

        expr1 & expr2                   # conjunction

Low Precedence Expressions

        break [expr]                    # break from loop
        case expr0 of {                 # case selection
           expr1:expr2
           ...
           [default:exprn]
           }
        create expr                     # co-expression creation
        every expr1 [do expr2]          # iterate over generated values
        fail                            # failure of procedure
        if expr1 then exp2 [else exp3]  # if-then-else
        next                            # go to top of loop
        repeat expr                     # loop
        return expr                     # return from procedure
        suspend expr1 [do expr2]        # suspension of procedure
        until expr1 [do expr2]          # until-loop
        while expr1 [do expr2]          # while-loop

```



## Haskell


{| class="wikitable"
|-
! style="text-align: left" | Precedence
! style="text-align: left" | Operator
! style="text-align: left" | Description
! style="text-align: left" | Associativity
|-
! 9
<small>highest</small>
| <code>.</code>
| Function composition
| Right
|-
! 8
| <code>^,^^,**</code>
| Power
| Right
|-
! 7
| <code>*,/,`quot`,`rem`,`div`,`mod`</code>
|
| Left
|-
! 6
| <code>+,-</code>
|
| Left
|-
! 5
| <code>:</code>
| Append to list
| Right
|-
! 4
| <code>==,/=,&lt;,&lt;=,&gt;=,&gt; </code>
| Compare-operators
|
|-
! 3
| <code>&amp;&amp;</code>
| Logical AND
| Right
|-
! 2
| <code><nowiki>||</nowiki></code>
| Logical OR
| Right
|-
! 1
| <code>&gt;&gt;,&gt;&gt;=</code>
|
| Left
|-
! 1
| <code>=&lt;&lt;</code>
|
| Right
|-
! 0
| <code>$,$!,`seq`</code>
|
| Right
|}


## J


{| class="wikitable"
|-
! style="text-align: left" | Precedence
! style="text-align: left" | Grammatical classification
! style="text-align: left" | Associativity
|-
! rowspan=2| <small>highest</small>
|  conjunctions
| rowspan=2| long left scope
|-
| adverbs
|-
! <small>lowest</small>
| verbs
| long right scope
|}

See http://www.jsoftware.com/help/dictionary/partsofspeech.htm for tokens in each grammatical class.

Note that other parts of speech do not have any precedence, because they are not "operators".

Note that this is an imprecise statement of the grammatical rules.  For a complete treatment, see http://www.jsoftware.com/help/dictionary/dicte.htm

Here's an informal treatment of the grammar:

Conjunctions require a left and right argument, either of which may be a noun or a verb.  If one argument is omitted the conjunction is curried with the remaining argument, forming an adverb (if the right argument is omitted, the precedence of the conjunction is treated as lower than the precedence of a verb).

Adverbs require a single left argument, which may be a noun or a verb.

Verbs require a right argument which must be a noun and may accept an optional left argument (which must also be a noun).  Unless we're working with a dangling (rightmost) conjunction, verbs have lower precedence than adverbs and conjunctions. (A conjunction on the far right without a right argument is treated as having lower precedence than verbs.) That said, note that the form <code>verb verb verb</code> is legal - this creates a derived verb whose arguments are given to the left and right verb and their results will be the argument for the middle verb. Longer trains are treated by using this mechanism on the rightmost three verbs. A shorter train is also given special treatment (the right verb gets the right argument, the left verb gets a left argument and the result of the right verb).

Nouns are not operators and accept no arguments.

The result of a verb must be a noun.

The result of an adverb or a conjunction can have any one of these grammatical classifications, and verb results are typical (and, thus, the result of an adverb or a conjunction may accept further arguments).  Adverbs and conjunctions serve a role analogous to that of macros in other languages.


## Java

This is well-documented [http://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html on the Oracle website].

## JavaScript

Mozilla Developer Network have a nice list of this at [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence JavaScript Reference:Expressions and operators:Operator Precedence]


## jq

The order of precedence of jq operators is shown in the following table, which also shows operator associativity:

# "%right" and %left" mean respectively right and left-associative;
# "%nonassoc" means it is a syntax error to find the operator twice in a row;
# "(none)" means that that no associativity is defined.

The associativity rules can be summarized:
# Operators with one of the characters "=", "<", or ">" in them are non-associative;
# "," and "//" are right-associative;
# All others are or behave as left-associative operators.


{| class="wikitable"
|-
! Precedence
! Operator
! Associativity
! Description
|-
| lowest
| &#124;
| %right
| pipe 
|-
!
| ,
| %left
| generator
|-
! 
| //
| %right
| specialized "or" for detecting empty streams
|-
!
| = &#124;= += -= *= /= %= //=
| %nonassoc
| set component
|-
!
| or 
| %left
| short-circuit "or"
|-
!
| and 
| %left
| short-circuit "and"
|-
!
| !=  ==  <  >  <=  >=
| %nonassoc
| boolean tests
|-
!
| + -
| %left
| polymorphic plus and minus
|-
!
| * / % 
| %left
| polymorphic multiply, divide; mod
|-
! highest
| ?
| (none)
| post-fix operator for suppressing errors (see note [1] below)
|}

Notes:

# If the expression immediately to the left of "?" raises an error, "?" suppresses it and emits nothings at all. E.g. 'error("bye")?' is equivalent to the expression 'empty'.
# Parentheses alter the order of evaluation in the usual way.
# Control structures such as ''if ... then ... else ... end'' and ''reduce ... as VAR (...; ...)'' determine the boundaries of subexpressions.


## Julia


```txt

Julia Operators in Order of Preference
--------------------------------------------
Syntax 				. followed by ::
Exponentiation			^
Fractions			//
Multiplication			* / % & \
Bitshifts			<< >> >>>
Addition			+ - | ⊻
Syntax				: .. followed by |>
Comparisons			> < >= <= == === != !== <:
Control flow			&& followed by || followed by ?
Assignments			= += -= *= /= //= \= ^= ÷= %= |= &= ⊻= <<= >>= >>>=

Operator precedence can be checked within Julia with the Base.operator_precedence function:

julia> Base.operator_precedence(:>=), Base.operator_precedence(:&&), Base.operator_precedence(:(=))
(6, 4, 1)

Julia Associativity of Operators
---------------------------------------------
Assignment (=, etc.), conditional (a ? b : c), -> arrows, lazy OR/AND (&& ||), 
power operators, and unary operators are right associative. All others are 
left associative.

```



## Kotlin

This is well-documented [https://kotlinlang.org/docs/reference/grammar.html on the Kotlin language website].


## LIL

From the LIL readme.txt documentation:

```txt
     expr [...]
       combines all arguments into a single string and evaluates the
       mathematical expression in that string.  The expression can use the
       following operators (in the order presented):
       
          (a)        - parentheses
       
          -a         - negative sign
          +a         - positive sign
          ~a         - bit inversion
          !a         - logical negation
       
          a * b      - multiplication
          a / b      - floating point division
          a \ b      - integer division
          a % b      - modulo
       
          a + b      - addition
          a - b      - subtraction
       
          a << b     - bit shifting
          a >> b
       
          a <= b     - comparison
          a >= b
          a < b
          a > b
       
          a == b     - equality comparison
          a != b

          a | b      - bitwise OR
          a & b      - bitwise AND
          
          a || b     - logical OR
          a && b     - logical AND
```



## Lua

Table available [http://www.lua.org/manual/5.1/manual.html#2.5.6 here]. That table does not contain all operators, however.
{| class="wikitable"
|-
! Precedence
! Operator
! Description
|-
! lowest
| '''or'''
| Boolean OR
|-
!
| '''and'''
| Boolean AND
|-
!
| '''<, <=, >, >=, ~=, =='''
| Comparisons
|-
!
| '''..'''
| Concatenation [1]
|-
!
| '''+, -'''
| Addition and subtraction
|-
!
| '''*, /, %'''
| Multiplication, division, modulo
|-
!
| '''not, -, #'''
| Boolean NOT, negation, length
|-
!
| '''^'''
| Exponentiation [1]
|-
! highest
| '''x[index], x.index, x(arguments...), x:m(arguments...)'''
| Generic index, string index, function call, method index+call [2]
|}

Notes:
# Concatenation and exponentiation are right-associative, all other binary operators are left-associative 
# Binding is done at the call site; therefore, method lookup is syntactically part of the call


## Mathematica

Here is an outline:
{| class="wikitable"
|-
! style="text-align: left" | Precedence
! style="text-align: left" | Class
! style="text-align: left" | Examples
|-
! highest
| Extensions of symbol names
| ''x''_, #2 , ''e''::''s'', etc.
|-
! 
| Function application variants
| ''e''[''e''], ''e''@@''e'', etc.
|-
! 
| Power-related operators
| √''e'', ''e''^''e'', etc.
|-
! 
| Multiplication-related operators
| ∇''e'', ''e''/''e'', ''e''⊗''e'', ''e'' ''e'', etc.
|-
! 
| Addition-related operators
| ''e''⊕''e'', ''e''+''e'', ''e''⋃''e'', etc.
|-
! 
| Relational operators
| ''e''==''e'', ''e''∼''e'', ''e''∈''e'', etc.
|-
! 
| Arrow and vector operators
| ''e''↗''e'', ''e''⇌''e'', etc.
|-
! 
| Logic operators
| ''e''&&''e'', ''e''∨''e'', ''e''⊢''e'', etc.
|-
! 
| Pattern and rule operators
| ''e''|''e'', ''e''->''e'', ''e''/.''e'', etc.
|-
! 
| Pure function operator
| ''e''&
|-
! 
| Assignment operators
| ''e''=''e'', ''e'':=''e'', etc.
|-
! lowest
| Compound expression
| ''e'';''e''
|}
There is a table of precedence of all operators on the page [http://reference.wolfram.com/language/tutorial/OperatorInputForms.html tutorial/OperatorInputForms] in Mathematica help.



## Matlab

Here is an outline:
{| class="wikitable"
|-
! style="text-align: left" | Precedence
! style="text-align: left" | Class

|-
! highest
| Parenthesis ()
|-
! 
| Transpose (.'), power (.^), complex conjugate transpose ('), matrix power (^)
|-
! 
| Unary plus (+), unary minus (-), logical negation (~)
|-
! 
| Multiplication (.*), right division (./), left division (.\), matrix multiplication (*), matrix right division (/), matrix left division (\)
|-
! 
| Addition (+), subtraction (-)
| 
|-
! 
| Colon operator (:)
|-
! 
| Less than (<), less than or equal to (<=), greater than (>), greater than or equal to (>=), equal to (==), not equal to (~=)
|-
! 
| Element-wise AND (&)
|-
! 
| Element-wise OR ( <code><nowiki>|</nowiki></code>)
|-
! 
| Short-circuit AND (&&)
|-
! Lowest
| Short-circuit OR ( <code><nowiki>||</nowiki></code>)
|
|}
There is a table of precedence of all operators on the page [http://mathworks.com/help/matlab/matlab_prog/operator-precedence.html]


## Nim

{| class="wikitable"
|-
! Precedence
! Operators
! Relevant character
|-
! 9 (highest)
|
| <code>$</code>, <code>^</code>
|-
! 8
| <code>*</code>, <code>/</code>, <code>div</code>, <code>mod</code>, <code>shl</code>, <code>shr</code>, <code>%</code>
| <code>*</code>, <code>%</code>, <code>\</code>, <code>/</code>
|-
! 7
| <code>+</code>, <code>-</code>
| <code>+</code>, <code>~</code>, <code>|</code>
|-
! 6
| <code>&</code>
| <code>&</code>
|-
! 5
| <code>..</code>
| <code>.</code>
|-
! 4
| <code>==</code>, <code><=</code>, <code><</code>, <code>>=</code>, <code>></code>, <code>!=</code>, <code>in</code>, <code>notin</code>, <code>is</code>, <code>isnot</code>, <code>not</code>, <code>of</code>
| <code>=</code>, <code><</code>, <code>></code>, <code>!</code>
|-
! 3
| <code>and</code>
|
|-
! 2
| <code>or</code>, <code>xor</code>
|
|-
! 1
|
| <code>@</code>, <code>:</code>, <code>?</code>
|-
! 0 (lowest)
| assignment operator (like <code>+=</code>, <code>*=</code>)
|
|-
|}


## OCaml

[http://caml.inria.fr/pub/docs/manual-ocaml/expr.html#@manual.kwd32 This table] contains the precedence and associativity of operators and other expression constructs in OCaml, including user-defined operators.


## Oforth


Oforth uses RPN notation. There are not different operator precedences. Everything is evaluated left to right.


## Pascal

This table contains the precedence and associativity of operators: 
{| class="wikitable"
! Priority || Description           || Operators    || Associativity
|-
| highest  ||                       ||              || 
|-
| 1        || unary operators       ||<code> not @        </code>|| left to right
|-
| 2        || mult operators        ||<code> * / div mod and shl shr as << >> </code>|| left to right 
|-
| 3        || add  operators        ||<code> + - or xor   </code>|| left to right 
|-
| 4        || relations             ||<code> = <> < > <= >= in is  </code>|| left to right 
|-
| lowest   ||                       ||              || 
|}
'''Example'''::

The expression <code> ( a>n  and  b<n ) </code> is equivalent to : 
<code> ( ( a > (n and b) ) < n ) </code> , so, it is invalid !

we must code:

<code> (a>n) and (b<n)</code>


## Perl

See [http://perldoc.perl.org/perlop.html#Operator-Precedence-and-Associativity the relevant documentation] for a table of Perl 5 operators ordered by precedence level.


## Perl 6

See [http://perlcabal.org/syn/S03.html#Operator_precedence this table] for a list of the precedence levels.  Perl 6 is an operator-rich language (and users may define more operators at will), so instead of listing all the operators in the table, representative operators are listed for some of the precedence levels; see later in the same file for a more complete list of predefined operators at each precedence level.


## Phix


{| class="wikitable"
! style="text-align: left" | Precedence
! style="text-align: left" | Operators
|-
! highest
| parenthesis/function/type calls/ternary operator
|-
! 
| subscripts/slices
|-
!
| unary-  unary+  not
|-
! 
| *  /
|-
! 
| +  -
|-
! 
| &
|-
! 
| <  >  <=  >=  =  !=
|-
! 
| and  or  xor
|-
! lowest 
| { , , , }
|}
Parenthesis is required to mix and/or/xor in an expression.


## PHP

[http://www.php.net/manual/en/language.operators.precedence.php Operator Precedence]


## PARI/GP

Unless otherwise stated, operators at a given level are applied left-to-right.
{| class="wikitable"
|-
! Precedence
! Operator
! Description
|-
! highest
| <code>:</code>
| Type information for <code>gp2c</code>. Ignored by gp.
|-
!
| <code>()</code>
| Postfix function call notation.
|-
!
| <code>++</code>, <code>--</code>
| Postfix increment or decrement operators. (Note that these work like the ''prefix'' <code>++</code> and <code>--</code> operators in C.)
|-
!
| <code>.</code>, <code>[ ]</code>
| Member operator (as in <code>m.mod</code>), selection operator (as in <code>v[1]</code>).
|-
!
| <code>'</code>, <code>~</code>, <code>!</code>
| Postfix derivative, transpose, and factorial; prefix negation.
|-
!
| <code>#</code>
| Prefix cardinality operator (like <code>length()</code>).
|-
!
| <code>^</code>
| Infix exponentiation operator, evaluated right-to-left.
|-
!
| <code>+</code>, <code>-</code> (unary)
| Prefix sign operators.
|-
!
| <code>*</code>, <code>/</code>, <code>%</code>, <code>\</code>, <code>\/</code>, <code>&lt;&lt;</code>, <code>&gt;&gt;</code>
| Infix multiplication, division, modulus, integer division, rounded quotient, left shift, and right shift.
|-
!
| <code>+</code>, <code>-</code>
| Infix addition and subtraction.
|-
!
| <code>&lt;</code>, <code>&lt;=</code>, <code>&gt;</code>, <code>&gt;=</code>, <code>!=</code> or <code>&lt;&gt;</code>, <code>==</code>, <code>===</code>
| Infix comparison operators. <code>===</code> tests whether two objects are identical component-wise and is stricter than <code>==</code>.
|-
!
| <code>&&</code>, <code><nowiki>||</nowiki></code>
| Infix shortcut logical AND and OR.
|-
!
| <code>=</code>, <code>+=</code>, <code>-=</code>, <code>*=</code>, <code>%=</code>, <code>/=</code>, <code>\=</code>, <code>\/=</code>, <code>&lt;&lt;=</code>, <code>&gt;&gt;=</code>
| Infix assignment, evaluated right-to-left.
|-
! lowest
| <code>-&gt;</code>
| Infix function definition.
|}

There are some exceptions to this standard order:
* If the [[wp:Short-circuit evaluation|short-circuit operator]] `&&` is used and the left expression is [[Boolean values#PARI/GP|falsy]] (`0`, `[]`, etc.) then the right expression is not evaluated. Similarly, with `||` is the left is truthy then the right expression is not evaluated.
* Assignment and all of the compound assignment operators need an lvalue on the left; if there is an expression where the rightmost part is an lvalue, assignment happens first. So `1 + n = 4 + 1` first adds 4 to 1, then assigns 5 to n, then adds 1 to 5.

See the User's Guide to PARI/GP in the [http://pari.math.u-bordeaux.fr/doc.html documentation], section 2.4, "GP operators".


## PicoLisp

There is no need to worry about operator precedence in PicoLisp and Lisp's in
general. All operators are normal functions and all of the code is organized as
S-expressions with a prefixed polish notation.


## PL/I

{| class="wikitable"
! Priority || Description              || Operators   || Associativity
|-
| highest  ||                          ||             || 
|-
| 1        || exponentiation           || **          || from '''right''' to left
|-
| 1        || unary operators          || -  +        || from '''right''' to left
|-
| 1        || logical NOT              || ¬           || from '''right''' to left
|-
| 2        || arithmetic * /           || *  /        || from left to right 
|-
| 3        || arithmetic + -           || +  -        || from left to right 
|-
| 4        || concatenation            || <nowiki>||</nowiki> || from left to right 
|-
| 5        || comparison               || =  ¬=  >  <  >=  <= || from left to right 
|-
| 6        || logical AND              || &           || from left to right 
|-
| 7        || logical OR               || <nowiki>|</nowiki> || from left to right 
|-
| lowest   ||                          ||             || 
|}

## PureBasic

Arguments are passed by value.

{| class="wikitable"
! Priority || Operator                  || Description                    || Associativity || Arity
|-
| ALIGN=CENTER|highest  || grouping                  ||<code> '(' ')'          </code> ||               ||
|-
| ALIGN=CENTER|8        || bitwise NOT, negation     ||<code> '~' '-'          </code> || Right to Left || 1          
|-
| ALIGN=CENTER|7        || arithmetic shift left, bitwise modulo, bitwise OR ||<code> '<<' '>>' '%' '!' </code> || Left to Right || 2
|-
| ALIGN=CENTER|6        || bitwise OR, bitwise AND   ||<code> '|' '&'          </code> || Left to Right || 2          
|-
| ALIGN=CENTER|5        || multiplication, division  ||<code> '*' '/'          </code> || Left to Right || 2          
|-
| ALIGN=CENTER|4        || addition, subtraction, string concatenation    ||<code> '+' '-' '+'         </code> || Left to Right || 2          
|-
| ALIGN=CENTER|3        || comparative               ||<code> '>' '>=' '=>' '<' 
 '<=' '=<' '=' '<>' </code> || Left to Right || 2          
|-
| ALIGN=CENTER|2        || logical                   ||<code> 'Not'            </code> || Right to Left || 1
|-
| ALIGN=CENTER|1        || logical                   ||<code> 'And' 'Or' 'XOr'  </code> || Left to Right  || 2           
|-
| ALIGN=CENTER|lowest   ||                           ||                                 ||                ||                              
|}


## Python

See [http://docs.python.org/py3k/reference/expressions.html?highlight=precedence#summary this table] and the whole page for details on Python version 3.x
An excerpt of which is this table:

{| class="wikitable"
|-
! Precedence
! Operator
! Description
|-
! lowest
| '''lambda'''
| Lambda expression
|-
!
| '''if – else'''
| Conditional expression
|-
!
| '''or'''
| <nowiki>Boolean OR</nowiki>
|-
!
| '''<nowiki>and</nowiki>'''
| Boolean AND
|-
!
| '''not x'''
| Boolean NOT
|-
!
| '''in, not in, is, is not, <, <=, >, >=, !=, =='''
| Comparisons, including membership tests and identity tests,
|-
!
| '''<nowiki>|</nowiki>'''
| Bitwise OR
|-
!
| '''^'''
| Bitwise XOR
|-
!
| '''<nowiki>&</nowiki>'''
| <nowiki>Bitwise AND</nowiki>
|-
!
| '''<nowiki><<, >></nowiki>'''
| <nowiki>Shifts</nowiki>
|-
!
| '''<nowiki>+, -</nowiki>'''
| Addition and subtraction
|-
!
| '''<nowiki>*, /, //, %</nowiki>'''
| <nowiki>Multiplication, division, remainder [1]</nowiki>
|-
!
| '''+x, -x, ~x'''
| Positive, negative, bitwise NOT
|-
!
| '''**'''
| Exponentiation [2]
|-
!
| '''x[index], x[index:index], x(arguments...), x.attribute'''
| <nowiki>Subscription, slicing, call, attribute reference</nowiki>
|-
! highest
| '''<nowiki>(expressions...), [expressions...], {key:datum...}, {expressions...}</nowiki>'''
| Binding or tuple display, list display, dictionary display, set display
|}

;Footnotes:
# The <code>%</code> operator is also used for string formatting; the same precedence applies.
# The power operator <code>**</code> binds less tightly than an arithmetic or bitwise unary operator on its right, that is, <code>2**-1</code> is <code>0.5</code>.


## Q

Operators have equal precedence and expressions are evaluated from right to left.

```q
q)3*2+1
9
q)(3*2)+1    / Brackets give the usual order of precedence
7
q)x:5
q)(x+5; x:20; x-5)
25 20 0
```



## Racket

Racket uses S-expr for its syntax, operators and functions show no precedences as all code is written as:

```Racket
(function arguments ...)
```


function being any function or operator (language or user defined) and arguments are the arguments passed to it.


## REALbasic

All operators are left-associated except exponentiation and Pair creation which are right-associated. Operators of the same scope and precedence will be evaluated from left-to-right. Precedence can be overridden by using parentheses, such that a + b - c <> a + (b - c).

By default, intrinsic types (Integer, String, Double, etc.) are passed by value and complex types (objects, arrays, etc.) are passed by reference. This can be overridden by using the ByVal or ByRef keyword before the parameter name in the method signature.

{| class="wikitable"
! Operator(s) !! Arity !! Associativity !! Description
|-
|  . (dot)|| 2 || Left || Scope resolution
|-
|  AddressOf, WeakAddressOf || 1 || Left || Delegate creation
|-
|  IsA, Is|| 2 || Left || Compare an object reference to a class or interface name; Compare the operands to determine whether they are references to the same object
|-
|  ^ || 2 || Right || Exponentiation
|-
|  - || 1 || Left || Negation and unary minus 
|-
|  Not || 1 || Left || Logical not 
|-
|  * / \ Mod || 2 || Left || Multiplication; floating-point division; integer division; modulo
|-
|  +, - || 2 || Left || Addition and string concatenation; subtraction
|-
|  =, &lt;, &gt;, &lt;&gt;, &lt;=, &gt;= || 2 || Left || Equal (comparison and assignment); less-than; greater-than; not equal; less-than or equal; greater-than or equal
|-
|  And || 2 || Left || Logical and bitwise And
|-
|  Or, Xor || 2 || Left || Logical and bitwise Or; logical and bitwise Exclusive-Or
|-
|  : (colon) || 2 || Right || Pair (e.g. linked-list) creation
|}


## REXX

Arguments are passed by value.

```txt

 ╔══════════════════════════════════════════════════════════════════════╗
 ║                                                                      ║
 ║ The following is a table that lists the precedence and associativity ║
 ║ of all the operators in the (classic) REXX language.                 ║
 ║                                                                      ║
 ║     1   is the highest precedence.                                   ║
 ║                                                                      ║
 ╠══════════╤════════╤══════════════════════════════════════════════════╣
 ║          │        │                                                  ║
 ║precedence│operator│               description                        ║
 ║          │        │                                                  ║
 ╟──────────┼────────┼──────────────────────────────────────────────────╢
 ║    1     │   -    │  unary minus                                     ║
 ║          │   +    │  unary plus                                      ║
 ║          │   \    │  logical not                                     ║
 ║          │        ├──(the following aren't supported by all REXXes)──╢
 ║          │   ¬    │  logical not                                     ║
 ║          │   ~    │  logical not                                     ║
 ║          │   ^    │  logical not                                     ║
 ╟──────────┼────────┼──────────────────────────────────────────────────╢
 ║    2     │   **   │  exponentiation    (integer power)               ║
 ╟──────────┼────────┼──────────────────────────────────────────────────╢
 ║    3     │   *    │  multiplication                                  ║
 ║          │   /    │  division                                        ║
 ║          │   %    │  integer division                                ║
 ║          │   //   │  modulus   (remainder division, sign of dividend)║
 ║          │  / /   │  modulus   (any 2 or 3 character operators may   ║
 ║          │        │             have whitespace between characters.) ║
 ╟──────────┼────────┼──────────────────────────────────────────────────╢
 ║    4     │   +    │  addition                                        ║
 ║          │   -    │  subtraction                                     ║
 ╟──────────┼────────┼──────────────────────────────────────────────────╢
 ║    5     │   ||   │  concatenation                                   ║
 ╟──────────┼────────┼──────────────────────────────────────────────────╢
 ║    6     │   &    │  logical AND                                     ║
 ║          │   |    │  logical OR      (inclusive OR)                  ║
 ║          │   &&   │  logical XOR     (exclusive OR)                  ║
 ╟──────────┼────────┼──────────────────────────────────────────────────╢
 ║    7     │[blank] │  concatenation                                   ║
 ║          │abuttal │  concatenation                                   ║
 ╟──────────┼────────┼──────────────────────────────────────────────────╢
 ║    8     │   =    │  equal to                                        ║
 ║          │   ==   │  exactly equal to   (also, strictly equal to)    ║
 ║          │   \=   │  not equal to                                    ║
 ║          │   <>   │  not equal to  (also, less than or greater than) ║
 ║          │   ><   │  not equal to  (also, greater than or less than) ║
 ║          │   >    │  greater than                                    ║
 ║          │   >=   │  greater than or equal to                        ║
 ║          │   <    │  less than                                       ║
 ║          │   <=   │  less than or equal to                           ║
 ║          │   >>   │  exactly greater than                            ║
 ║          │   <<   │  exactly less than                               ║
 ║          │  <<=   │  exactly less than or equal to                   ║
 ║          │  >>=   │  exactly greater than or equal to                ║
 ║          │        ├──(the following aren't supported by all REXXes)──╢
 ║          │   /=   │  not equal to                                    ║
 ║          │   ¬=   │  not equal to                                    ║
 ║          │   ~=   │  not equal to                                    ║
 ║          │   ^=   │  not equal to                                    ║
 ║          │  /==   │  not exactly equal to                            ║
 ║          │  \==   │  not exactly equal to                            ║
 ║          │  ¬==   │  not exactly equal to                            ║
 ║          │  ~==   │  not exactly equal to                            ║
 ║          │  ^==   │  not exactly equal to                            ║
 ║          │   /<   │  not less than                                   ║
 ║          │   ~<   │  not less than                                   ║
 ║          │   ¬<   │  not less than                                   ║
 ║          │   ^<   │  not less than                                   ║
 ║          │   />   │  not greater than                                ║
 ║          │   ¬>   │  not greater than                                ║
 ║          │   ~>   │  not greater than                                ║
 ║          │   ^>   │  not greater than                                ║
 ║          │  /<=   │  not less than or equal to                       ║
 ║          │  ¬<=   │  not less than or equal to                       ║
 ║          │  ~<=   │  not less than or equal to                       ║
 ║          │  ^<=   │  not less than or equal to                       ║
 ║          │  />=   │  not greater than or equal to                    ║
 ║          │  ¬>=   │  not greater than or equal to                    ║
 ║          │  ~>=   │  not greater than or equal to                    ║
 ║          │  ^>=   │  not greater than or equal to                    ║
 ║          │  \>>   │  not exactly greater than                        ║
 ║          │  ¬>>   │  not exactly greater than                        ║
 ║          │  ~>>   │  not exactly greater than                        ║
 ║          │  ^>>   │  not exactly greater than                        ║
 ║          │  \<<   │  not exactly less than                           ║
 ║          │  ¬<<   │  not exactly less than                           ║
 ║          │  ~<<   │  not exactly less than                           ║
 ║          │  ^<<   │  not exactly less than                           ║
 ╚══════════╧════════╧══════════════════════════════════════════════════╝

```



## Ring


```ring

The next table present operators from higher precedence (Evaluated first) to lower precedence.

Operator 
. [] () {} 
- ~ :Literal [list items] 
++ - - 
Start:End 
* / % 
+ - 
<< >> 
& 
| ^ 
< > <= >= 
= != 
not 
and or 
Assignment = += -= *= /= %=>>= <<= &= ^= |= 

Example:

See 3+5*4       # prints 23

```



## Ruby


Ruby operators, by precedence (high to low), with arity (N), associativity (A), and definability (D)

{| class="wikitable"
! Operator(s)	|| N	|| A	|| D	|| Operation
|-
| ! ~ +	        || 1	|| R	|| Y	|| Boolean NOT, bitwise complement, unary plus
|-
| **	        || 2	|| R	|| Y	|| Exponentiation
|-
| -	        || 1	|| R	|| Y	|| Unary minus (define with -@)
|-
| * / %	        || 2	|| L	|| Y	|| Multiplication, division, modulo (remainder)
|-
| + -	        || 2	|| L	|| Y	|| Addition (or concatenation), subtraction
|-
| << >>	        || 2	|| L	|| Y	|| Bitwise shift-left (or append), bitwise shift-right
|-
| &	        || 2	|| L	|| Y	|| Bitwise AND
|-
| <nowiki>|</nowiki> ^	        || 2	|| L	|| Y	|| Bitwise OR, bitwise XOR
|-
| < <= >= >	|| 2	|| L	|| Y	|| Ordering
|-
| | == === !=  =~ !~ <=> 	|| 2    || N	|| Y	|| Equality, pattern matching, comparison
|-
| &&	        || 2	|| L	|| N	|| Boolean AND
|-
| <nowiki>||</nowiki>       || 2	|| L	|| N	|| Boolean OR
|-
| .. ...	|| 2	|| N	|| N	|| Range creation and Boolean flip-flops
|-
| ? :	        || 3    || R	|| N    || Conditional
|-
| rescue	|| 2	|| L	|| N	|| Exception-handling modifier
|-
| = **= *= / = %= += -= <<= >>= &&= &= ^=   || 2	|| R	|| N	|| Assignment
|-
| defined?	|| 1	|| N	|| N	|| Test variable definition and type
|-
| not	        || 1    || R    || N	|| Boolean NOT (low precedence)
|-
| and or	|| 2	|| L	|| N	|| Boolean AND, Boolean OR (low precedence)
|-
| if unless while until 	|| 2	|| N	|| N	|| Conditional and loop modifiers
|}


## Scala

Operator precedence is well-documented on the official [https://docs.scala-lang.org/tour/operators.html Scala Documentation | Tour of Scala | Operators ] web page.


## Scheme


As with Common Lisp and Racket, Scheme uses s-expressions so there is no need for operator precedence.

e.g. an expression like "3 + 4 x 5" must be entered as either <code>(+ 3 (* 4 5))</code> or <code>(* (+ 3 4) 5)</code>


## Scilab

{| class="wikitable"
! Priority || Description               || Operators        || Associativity
|-
| highest  ||                           ||                  || 
|-
| 1        || transpose                 ||<code> '                </code>|| 
|-
| 2        || power                     ||<code> ** ^             </code>|| '''right'''
|-
| 3        || multiplication & division ||<code> * / \            </code>|| left
|-
| 4        || unary operator (opposite) ||<code> + -              </code>|| left
|-
| 5        || addition & subtraction    ||<code> + -              </code>|| left
|-
| 6        || comparison                ||<code> == ~= > < >= <=  </code>|| left 
|-
| 7        || logical NOT               ||<code> ~                </code>|| left 
|-
| 8        || logical AND               ||<code> and              </code>|| left 
|-
| 9        || logical OR                ||<code> or xor           </code>|| left 
|-
| lowest   ||                           ||                  || 
|}


## Simula

Modern Simula syntax:
{| class="wikitable"
! Priority || Description               || Operators                     || Associativity
|-
| highest  ||                           ||                               || 
|-
| 1        || power                     ||<code> **               </code>|| left
|-
| 2        || unary operator (opposite) ||<code> + -              </code>|| left
|-
| 3        || multiplication & division ||<code> * / //           </code>|| left
|-
| 4        || addition & subtraction    ||<code> + -              </code>|| left
|-
| 5        || comparison                ||<code> = <> < <= > >=   </code>|| left 
|-
| 6        || logical NOT               ||<code> NOT              </code>|| left 
|-
| 7        || logical AND               ||<code> AND              </code>|| left 
|-
| 8        || logical OR                ||<code> OR               </code>|| left 
|-
| 9        || equivalence               ||<code> EQUIV            </code>|| left 
|-
| 10       || implication               ||<code> IMPL             </code>|| left 
|-
| lowest   ||                           ||                               || 
|}
Note: // is the Euclidean division





## Seed7


Seed7 supports [http://seed7.sourceforge.net/examples/operator.htm user defined operators] with priority and associativity.
This includes user defined operator symbols. Priority and associativity are defined with
the Seed7 Structured Syntax Description ([http://seed7.sourceforge.net/manual/syntax.htm S7SSD])
A S7SSD statement like

```seed7
$ syntax expr: .(). + .()  is -> 7;
```

specifies the syntax of the <code>+</code> operator.
The right arrow <code>-&gt;</code> describes the associativity:
Binding of operands from left to right. With <code>7</code> the priority
of the <code>+</code> operator is defined. The syntax pattern

```seed7
.(). + .()
```

is introduced and delimited with dots (<code>.</code>). Without dots the pattern is

```txt
() + ()
```

The symbol <code>()</code> is a nonterminal symbol and <code>+</code> is a terminal symbol.
The S7SSD does not distinguish between different nonterminal symbols.
Instead it only knows one nonterminal symbol: <code>()</code>.

The include file [http://seed7.sourceforge.net/prg/syntax.htm syntax.s7i] contains the syntax of the predefined operators.
The table below is extracted from syntax.s7i:

{| class="wikitable"
! Priority || infix/prefix || Left associative || Right associative || Not associative
|-
|  1 || infix  ||                            || conv varConv cast value parse       ||
|-
|  1 || prefix || {                          || getfunc getobj [                    ||
|-
|  2 || infix  || . [ ^ ->                   ||                                     ||
|-
|  3 || prefix || &                          ||                                     ||
|- 
|  4 || infix  || !                          || **                                  ||
|- 
|  4 || prefix || !                          ||                                     ||
|-
|  5 || prefix ||                            || + - conj                            ||
|-
|  6 || infix  || * / div rem mdiv mod       ||                                     ||
|-
|  7 || infix  || + -                        ||                                     ||
|-
|  8 || infix  || mult find                  || times                               ||
|-
|  9 || infix  || << >>                      ||                                     ||
|-
| 10 || infix  || &                          ||                                     ||
|-
| 11 || infix  || >< |                       ||                                     ||
|-
| 12 || infix  ||                            ||                                     || = <> < > <= >= in not in
|-
| 13 || prefix || new sub                    || not subtype subrange set array hash ||
|-
| 14 || infix  || and                        ||                                     ||
|-
| 15 || infix  || or                         ||                                     ||
|-
| 16 || infix  || val radix RADIX digits sci ||                                     ||
|-
| 17 || infix  || exp lpad rpad lpad0        ||                                     ||
|-
| 18 || infix  || <&                         ||                                     ||
|-
| 20 || infix  ||                            ||                                     || := +:= -:= *:= /:= <<:= >>:= &:= @:=
|}


## Sidef

All operators in Sidef have the same precedence, which is controlled by lack of whitespace between the operands.

For example:

```ruby
1+2 * 3+4    # means: (1+2) * (3+4)
```


See also the [https://trizen.gitbooks.io/sidef-lang/content/syntax_and_semantics/operator_precedence.html documentation] on the precedence of operators.


## Tcl

Tcl only supports operators within an expression context (such as the [http://www.tcl.tk/man/tcl8.6/TclCmd/expr.htm#M6 <code>expr</code>] command, which lists the operators with more detail):

{| class="wikitable"
|-
! Precedence
! Operator
! Description
|-
! highest
| '''- + ~ !'''
| Unary minus, unary plus, bit-wise NOT, logical NOT.
|-
!
| '''**'''
| Exponentiation. Right-to-left associative.
|-
!
| '''* / %'''
| Multiply, divide, remainder.
|-
!
| '''+ -'''
| Add and subtract.
|-
!
| '''&lt;&lt; &gt;&gt;'''
| Left and right shift.
|-
!
| '''&lt; &gt; &lt;= &gt;='''
| Boolean less, greater, less than or equal, and greater than or equal.
|-
!
| '''== !='''
| Boolean equal and not equal.
|-
!
| '''eq ne'''
| Boolean string equal and string not equal.
|-
!
| '''in ni'''
| List containment and negated list containment.
|-
!
| '''&amp;'''
| Bit-wise AND.
|-
!
| '''^'''
| Bit-wise exclusive OR.
|-
!
| '''<nowiki>|</nowiki>'''
| Bit-wise OR. Valid for integer operands only.
|-
!
| '''&amp;&amp;'''
| Logical AND. Evaluates its second operand lazily.
|-
!
| '''<nowiki>||</nowiki>'''
| Logical OR. Evaluates its second operand lazily.
|-
! lowest
| ''x'' '''?''' ''y'' ''':''' ''z''
| If-then-else, as in [[C]]. Evaluates its second and third operands lazily.
|}

=={{header|TI-83 BASIC}}==
{| class="wikitable"
! Priority || Description              || Operators    || Associativity
|-
| highest  ||                          ||              || 
|-
| 1        || function                 || √( e^( 10^( sin( ...  || 
|-
| 2        || unary '''right''' operator || ²  !         || left to right
|-
| 3        || exponentiation           || ^            || left to right 
|-
| 4        || unary left operator      || (-)          || '''right''' to left
|-
| 5        || permutation combination  || nPr nCr      || left to right 
|-
| 6        || arithmetic * /           || ×  ÷         || left to right 
|-
| 7        || arithmetic + -           || +  -         || left to right 
|-
| 8        || relation                 || =  ≠  >  <  ≥  ≤ || left to right 
|-
| 9        || logical AND              || and          || left to right 
|-
| 10       || logical OR               || or xor       || left to right 
|-
| 11       || conversion               || ►Frac ►Dec ...  || left to right 
|-
| lowest   ||                          ||              || 
|}

''note'': no operator between 2 symbols means an implied multiplication, so priority 6 
and left to right associativity

''note'': NOT is not an operator, it is a function: not()


'''Examples''':
 -2^2=-(2^2)=-4
 2^3^2=(2^3)^2=64
 4/2π=(4/2)π=2π=6.283185307
 -B/2A=-((B/2)*A)=-(B/2)*A


## VBScript

{| class="wikitable"
! Priority || Description               || Operators                     || Associativity
|-
| highest  ||                           ||                               || 
|-
| 1        || unary operator (opposite) ||<code> + -              </code>|| left
|-
| 2        || power                     ||<code> ^                </code>|| left
|-
| 3        || multiplication & division ||<code> * /              </code>|| left
|-
| 4        || integer division          ||<code> \                </code>|| left
|-
| 5        || modulus                   ||<code> mod              </code>|| left
|-
| 6        || addition & subtraction    ||<code> + -              </code>|| left
|-
| 7        || concatenation             ||<code> &                </code>|| left 
|-
| 8        || comparison                ||<code> = <> < <= > >= Is </code>|| left 
|-
| 9        || logical NOT               ||<code> Not              </code>|| left 
|-
| 10       || logical AND               ||<code> And              </code>|| left 
|-
| 11       || logical OR                ||<code> Or               </code>|| left 
|-
| 12       || logical exclusion         ||<code> Xor              </code>|| left 
|-
| 13       || equivalence               ||<code> Eqv              </code>|| left 
|-
| 14       || implication               ||<code> Imp              </code>|| left 
|-
| lowest   ||                           ||                               || 
|}


Let's not the difference with Visual Basic on unary operator priority.

'''Example''':
 -2^2=(-2)^2=4
 2^3^2=(2^3)^2=64


## Visual Basic

{| class="wikitable"
! Priority || Description               || Operators                     || Associativity
|-
| highest  ||                           ||                               || 
|-
| 1        || power                     ||<code> ^                </code>|| left
|-
| 2        || unary operator (opposite) ||<code> + -              </code>|| left
|-
| 3        || multiplication & division ||<code> * /              </code>|| left
|-
| 4        || integer division          ||<code> \                </code>|| left
|-
| 5        || modulus                   ||<code> mod              </code>|| left
|-
| 6        || addition & subtraction    ||<code> + -              </code>|| left
|-
| 7        || concatenation             ||<code> &                </code>|| left 
|-
| 8        || bit shift                 ||<code> << >>            </code>|| left 
|-
| 9        || comparison                ||<code> = <> < <= > >= Is IsNot Like </code>|| left 
|-
| 10       || logical NOT               ||<code> Not              </code>|| left 
|-
| 11       || logical AND               ||<code> And AndAlso      </code>|| left 
|-
| 12       || logical OR                ||<code> Or OrElse        </code>|| left 
|-
| 13       || logical XOR               ||<code> Xor              </code>|| left 
|-
| lowest   ||                           ||                               || 
|}


## Visual Basic .NET


See [[Operator_precedence#Visual_Basic | Visual Basic]]


Note: the Imp and Eqv VB6 operators were been removed from Visual Basic .NET.


## XPL0

All operations of equal precedence are evaluated (associate) left-to-right.

{| class="wikitable"
|-
! Precedence
! Operator
! Description
|-
! highest
| '''() ,'''
| Grouping, comma separator (constructs)
|-
!
| '''- + addr @'''
| Unary minus, unary plus, address of a variable
|-
!
| '''&lt;&lt; &gt;&gt; -&gt;&gt;'''
| Left and right logical shifts, arithmetic shift right
|-
!
| '''* /'''
| Multiply and divide
|-
!
| '''+ -'''
| Add and subtract
|-
!
| '''= # &lt; &lt;= &gt; &gt;='''
| Comparisons: equal, not equal, less than, etc.
|-
!
| '''~ not'''
| Bitwise NOT
|-
!
| '''&amp; and'''
| Bitwise AND
|-
!
| '''<nowiki>!</nowiki> or <nowiki>|</nowiki> xor'''
| Bitwise OR and exclusive OR
|-
! lowest
| '''if-then-else'''
| If expression
|}


## zkl

All operations of equal precedence are evaluated (associate) left-to-right.
If shared with C it has the same precedence except for logical and/or, which have the same precedence.

{| class="wikitable"
|-
! Precedence
! Operator
! Description
|-
! highest
| '''() .(dot) [],'''
| Grouping/function call, .resolve, subscripting
|-
!
| '''- not'''
| Unary minus, logical not
|-
!
| '''* / %'''
| Multiply, divide and modulo
|-
!
| '''+ -'''
| Add and subtract
|-
!
| '''&lt; &lt;= &gt; &gt;='''
| Comparisons: equal, not equal, less than, etc.
|-
!
| '''== !='''
| Comparisons: equal, not equal
|-
! 
| '''and or'''
| logical AND and logical OR
|-
! 
| ''':'''
| compose (nest expressions)
|-
! lowest
| ''':= ='''
| Assignment
|}
