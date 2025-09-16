+++
title = "EsoLang"
description = ""
date = 2015-10-12T07:54:37Z
aliases = []
[extra]
id = 18266
[taxonomies]
categories = []
tags = []
+++

<!--
=Esotheric=
-->
{{collection|99 Bottles of Beer}}
The task [[99 Bottles of Beer]]
done in [[:Category:Esoteric Languages|esoteric languages]].

<!--
See [[99 Bottles of Beer/EsoLang]]
-->

<!-- still missing:
Burlesque
Fish
Malboge
Shakespeare
Unlambda
Whitespace
-->
__toc__


## 0815

ATTENTION: Since 0815 output is in Hexadecimals only,
the bottles count is obviously in Hexadecimals too.
So if you see something like: "1E Bottles of beer..."
don’t panic, everything is OK.


```0815

<:63:x<:20:=<:62:>=>=><:6F:x<:74:=<:6C:>=>>=><:65:x<:73:=<:20:>=>=><:6F:x<:66:=<:20:>=>=>
<:62:x<:65:=<:72:>=>>=><:20:x<:6F:=<:6E:>=>=><:20:x<:74:=<:68:>=>=><:65:x<:20:=<:77:>=>=>
<:61:x<:6C:=>=>><:54:x<:61:=<:6B:>=>=><:65:x<:20:=<:6F:>=>=><:6E:x<:65:=<:20:>=>=><:64:x
<:6F:=<:77:>=>=><:6E:x<:20:=<:61:>=>=><:6E:x<:64:=<:20:>=>=><:70:x<:61:=<:73:>=>=>><:20:x
<:69:=<:74:>=>=><:20:x<:61:=<:72:>=>=><:6F:x<:75:=<:6E:>=>=><:64:~>}:_start:{~%><:1c:~
}:_99:~{~$>=<:01:x-^:_99:<:0D:~$@:20:{~%><:10:~}:_98:~{~$>=<:01:x-^:_98:<:0D:~$@:c:<:20:~
}:_97:~{~$>=<:01:x-^:_97:<:0D:~${x<:01:x->&==<:01:-#:_322:{~%><:1c:~}:_96:~{~$>=<:01:x-
^:_96:<:d:~$$@:20:{~>&^:_start:}:_90:?<:4E:x<:6F:=<:20:>=>=><:6D:x<:6F:=<:72:>=>=><:65:x
<:20:=<:62:>=>=><:6F:x<:74:=<:6C:>=>>=><:65:x<:73:=<:20:>=>=><:6F:x<:66:=<:20:>=>=><:62:x
<:65:=<:72:>=>>=><:20:x<:6F:=<:6E:>=>=><:20:x<:74:=<:68:>=>=><:65:x<:20:=<:77:>=>=><:61:x
<:6C:=>=>><:02:~}:_70:><:23:~}:_80:~{~$>=<:01:x-^:_80:{~<:01:=-#:_60:<:0D:~$$=^:_70:}:_60:
<:0D:~$<:17:~}:_81:~{~$=<:01:x-^:_81:<:0D:~$?<:47:x<:6F:=<:20:>=>=><:74:x<:6F:=<:20:>=>=>
<:74:x<:68:=<:65:>=>=><:20:x<:73:=<:74:>=>=><:6F:x<:72:=<:65:>=>=><:20:x<:61:=<:6E:>=>=>
<:64:x<:20:=<:62:>=>=><:75:x<:79:=<:20:>=>=><:73:x<:6F:=<:6D:>=>=><:65:x<:20:=<:6D:>=>=>
<:6F:x<:72:=<:65:>=>=><:21:~}:_18:~{~$=<:01:x-^:_18:<:0D:~$<:63:x<:20:=<:62:>=>=><:6F:x
<:74:=<:6C:>=>>=><:65:x<:73:=<:20:>=>=><:6F:x<:66:=<:20:>=>=><:62:x<:65:=<:72:>=>>=><:20:
x<:6F:=<:6E:>=>=><:20:x<:74:=<:68:>=>=><:65:x<:20:=<:77:>=>=><:61:x<:6C:=>=>>{~%<:1c:~
}:_21:~{~$=<:01:x-^:_21:<:0D:~$^:end:}:_322:?<:01:x<:20:=<:62:>=>=><:6F:x<:74:=<:6C:>=>>=
><:65:x<:20:=<:6F:>=>=><:66:x<:20:=>=><:62:x<:65:=<:72:>=>>=><:20:x<:6F:=<:6E:>=>=><:20:x
<:74:=<:68:>=>=><:65:x<:20:=<:77:>=>=><:61:x<:6C:=>=>><:54:x<:61:=<:6B:>=>=><:65:x<:20:=
<:6F:>=>=><:6E:x<:65:=<:20:>=>=><:64:x<:6F:=<:77:>=>=><:6E:x<:20:=<:61:>=>=><:6E:x<:64:=
<:20:>=>=><:70:x<:61:=<:73:>=>=>><:20:x<:69:=<:74:>=>=><:20:x<:61:=<:72:>=>=><:6F:x<:75:=
<:6E:>=>=><:64:~>{~%><:1b:~}:_299:~{~$>=<:01:x-^:_299:<:0D:~$$@:20:{~%><:1b:~}:_298:~{~$>
=<:01:x-^:_298:<:0D:~$@:20:{~%<:f:~}:_297:~{~$>=<:01:x-^:_297:<:0D:~$@:c:<:20:~}:_296:~{~
$>=<:01:x-^:_296:<:0D:~${x<:01:x->&==<:01:-^:_90:

```



## Befunge


'''Readable:'''
<!-- the version at http://99-bottles-of-beer.net/language-befunge-88.html looks like a train wreck to me -->

```befunge
<v,<.g10" bottles of beer on the wall"+550   <
c>:|
  $<v,<.g10" bottles of beer"+550
    >:|
     $<v,<"take one down, pass it around"+550
       >:|
         >$01g1-:01p                     v
v,<.g10" bottles of beer on the wall"+550<
>:|
  >$55+,0`                                   |
                                             @
```


'''Concise:'''

```befunge
"d"4vv"take one down, pass it around"<>
:-2*< v "e wall"_v#\0`1%4./4::_0#%>#4^#
\4>/|>:#,_$:55+:,\4%3-!*0\>:>#,_$$:1+\1
>>>#@^>$"ht no "\>\"reeb fo selttob">>>
```


=={{header|Brainfuck}}==

```bf
>+++++++++[<+++++++++++>
-]<[>[-]>[-]<<[>+>+<<-]>>[<<+>>-]>>>
[-]<<<+++++++++<[>>>+<<[>+>[-]<<-]>[<+>-]>[<<++++++++++>>>+<
-]<<-<-]+++++++++>[<->-]>>+>[<[-]<<+>>>-]>[-]+<<[>+>-<<-]<<<
[>>+>+<<<-]>>>[<<<+>>>-]>[<+>-]<<-[>[-]<[-]]>>+<[>[-]<-]<+++
+++++[<++++++<++++++>>-]>>>[>+>+<<-]>>[<<+>>-]<[<<<<<.>>>>>-
]<<<<<<.>>[-]>[-]++++[<++++++++>-]<.>++++[<++++++++>-]<++.>+
++++[<+++++++++>-]<.><+++++..--------.-------.>>[>>+>+<<<-]>
>>[<<<+>>>-]<[<<<<++++++++++++++.>>>>-]<<<<[-]>++++[<+++++++
+>-]<.>+++++++++[<+++++++++>-]<--.---------.>+++++++[<------
---->-]<.>++++++[<+++++++++++>-]<.+++..+++++++++++++.>++++++
++[<---------->-]<--.>+++++++++[<+++++++++>-]<--.-.>++++++++
[<---------->-]<++.>++++++++[<++++++++++>-]<++++.-----------
-.---.>+++++++[<---------->-]<+.>++++++++[<+++++++++++>-]<-.
>++[<----------->-]<.+++++++++++..>+++++++++[<---------->-]<
-----.---.>>>[>+>+<<-]>>[<<+>>-]<[<<<<<.>>>>>-]<<<<<<.>>>+++
+[<++++++>-]<--.>++++[<++++++++>-]<++.>+++++[<+++++++++>-]<.
><+++++..--------.-------.>>[>>+>+<<<-]>>>[<<<+>>>-]<[<<<<++
++++++++++++.>>>>-]<<<<[-]>++++[<++++++++>-]<.>+++++++++[<++
+++++++>-]<--.---------.>+++++++[<---------->-]<.>++++++[<++
+++++++++>-]<.+++..+++++++++++++.>++++++++++[<---------->-]<
-.---.>+++++++[<++++++++++>-]<++++.+++++++++++++.++++++++++.
------.>+++++++[<---------->-]<+.>++++++++[<++++++++++>-]<-.
-.---------.>+++++++[<---------->-]<+.>+++++++[<++++++++++>-
]<--.+++++++++++.++++++++.---------.>++++++++[<---------->-]
<++.>+++++[<+++++++++++++>-]<.+++++++++++++.----------.>++++
+++[<---------->-]<++.>++++++++[<++++++++++>-]<.>+++[<----->
-]<.>+++[<++++++>-]<..>+++++++++[<--------->-]<--.>+++++++[<
++++++++++>-]<+++.+++++++++++.>++++++++[<----------->-]<++++
.>+++++[<+++++++++++++>-]<.>+++[<++++++>-]<-.---.++++++.----
---.----------.>++++++++[<----------->-]<+.---.[-]<<<->[-]>[
-]<<[>+>+<<-]>>[<<+>>-]>>>[-]<<<+++++++++<[>>>+<<[>+>[-]<<-]
>[<+>-]>[<<++++++++++>>>+<-]<<-<-]+++++++++>[<->-]>>+>[<[-]<
<+>>>-]>[-]+<<[>+>-<<-]<<<[>>+>+<<<-]>>>[<<<+>>>-]<>>[<+>-]<
<-[>[-]<[-]]>>+<[>[-]<-]<++++++++[<++++++<++++++>>-]>>>[>+>+
<<-]>>[<<+>>-]<[<<<<<.>>>>>-]<<<<<<.>>[-]>[-]++++[<++++++++>
-]<.>++++[<++++++++>-]<++.>+++++[<+++++++++>-]<.><+++++..---
-----.-------.>>[>>+>+<<<-]>>>[<<<+>>>-]<[<<<<++++++++++++++
.>>>>-]<<<<[-]>++++[<++++++++>-]<.>+++++++++[<+++++++++>-]<-
-.---------.>+++++++[<---------->-]<.>++++++[<+++++++++++>-]
<.+++..+++++++++++++.>++++++++[<---------->-]<--.>+++++++++[
<+++++++++>-]<--.-.>++++++++[<---------->-]<++.>++++++++[<++
++++++++>-]<++++.------------.---.>+++++++[<---------->-]<+.
>++++++++[<+++++++++++>-]<-.>++[<----------->-]<.+++++++++++
..>+++++++++[<---------->-]<-----.---.+++.---.[-]<<<]
```



## Chef


```chef
99 Bottles Of Beer.

Ingredients.
99 bottles

Method.
Loop the bottles.
	Put bottles into 1st mixing bowl.
	Serve with bottles of beer on the wall.
	Clean 1st mixing bowl.
	Put bottles into 1st mixing bowl.
	Serve with bottles of beer.
	Clean 1st mixing bowl.
	Serve with Take one down and pass it around.
	Clean 1st mixing bowl.
Loop the bottles until looped.
Serve with No more bottles of beer.
Clean 1st mixing bowl.
Pour contents of the 3rd mixing bowl into the 1st baking dish.

Serves 1.

bottles of beer on the wall.

Prints out "n" bottles of beer on the wall.

Ingredients.
108 g lime
97 cups asparagus
119 pinches watercress
32 tablespoons pickles
101 pinches eggplant
104 g huckleberry
116 teaspoons turnip
110 tablespoons nannyberry
111 tablespoons onion
114 tablespoons raspberry
98 g broccoli
102 g feijoa
115 teaspoons squach
10 ml new line

Method.
Put new line into 1st mixing bowl.
Put lime into 2nd mixing bowl.
Put lime into 2nd mixing bowl.
Put asparagus into 2nd mixing bowl.
Put watercress into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put huckleberry into 2nd mixing bowl.
Put turnip into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put nannyberry into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put raspberry into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put broccoli into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put feijoa into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put squach into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put lime into 2nd mixing bowl.
Put turnip into 2nd mixing bowl.
Put turnip into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put broccoli into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Liquify contents of the 2nd mixing bowl.
Pour contents of the 2nd mixing bowl into the baking dish.
Pour contents of the mixing bowl into the baking dish.
Refrigerate for 1 hour.

bottles of beer.

Prints out "n" bottles of beer.

Ingredients.
114 tablespoons raspberry
101 pinches eggplant
98 teaspoons broccoli
32 pinches pickles
102 tablespoons feijoa
111 teaspoons onion
115 cups squach
108 cups lime
116 teaspoons turnip
10 ml new line

Method.
Put new line into 1st mixing bowl.
Put raspberry into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put broccoli into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put feijoa into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put squach into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put lime into 2nd mixing bowl.
Put turnip into 2nd mixing bowl.
Put turnip into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put broccoli into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Liquify contents of the 2nd mixing bowl.
Pour contents of the 2nd mixing bowl into the baking dish.
Pour contents of the mixing bowl into the baking dish.
Refrigerate for 1 hour.

Take one down and pass it around.

Prints out "Take one down and pass it around".

Ingredients.
100 cups dandelion
110 g nannyberry
117 pinches cucumber
111 pinches onion
114 pinches raspberry
97 g asparagus
32 tablespoons pickles
116 pinches turnip
105 g chestnut
115 g squach
112 g pumpkin
119 cups watercress
101 g eggplant
107 g kale
84 cups tomatoe
10 ml new line

Method.
Put new line into 3rd mixing bowl.
Put dandelion into 2nd mixing bowl.
Put nannyberry into 2nd mixing bowl.
Put cucumber into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put raspberry into 2nd mixing bowl.
Put asparagus into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put turnip into 2nd mixing bowl.
Put chestnut into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put squach into 2nd mixing bowl.
Put squach into 2nd mixing bowl.
Put asparagus into 2nd mixing bowl.
Put pumpkin into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put dandelion into 2nd mixing bowl.
Put nannyberry into 2nd mixing bowl.
Put asparagus into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put nannyberry into 2nd mixing bowl.
Put watercress into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put dandelion into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put nannyberry into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put kale into 2nd mixing bowl.
Put asparagus into 2nd mixing bowl.
Put tomatoe into 2nd mixing bowl.
Liquify contents of the 2nd mixing bowl.
Pour contents of the 2nd mixing bowl into the baking dish.
Pour contents of the 3rd mixing bowl into the baking dish.
Refrigerate for 1 hour.

No more bottles of beer.

Prints out "No more bottles of beer".

Ingredients.
114 pinches raspberry
101 teaspoons eggplant
98 cups broccoli
32 tablespoons pickles
102 pinches feijoa
111 cups onion
115 tablespoons squach
108 tablespoons lime
116 pinches turnip
109 cups mushrooms
78 g nectarine
10 ml new line

Method.
Put new line into 3rd mixing bowl.
Put new line into 2nd mixing bowl.
Put raspberry into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put broccoli into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put feijoa into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put squach into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put lime into 2nd mixing bowl.
Put turnip into 2nd mixing bowl.
Put turnip into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put broccoli into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put eggplant into 2nd mixing bowl.
Put raspberry into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put mushrooms into 2nd mixing bowl.
Put pickles into 2nd mixing bowl.
Put onion into 2nd mixing bowl.
Put nectarine into 2nd mixing bowl.
Liquify contents of the 2nd mixing bowl.
Pour contents of the 2nd mixing bowl into the baking dish.
Pour contents of the 3rd mixing bowl into the baking dish.
Refrigerate for 1 hour.
```


=={{header|Extended Brainfuck}}==
[http://sylwester.no/ebf/ More Info About EBF]

```ebf

;; Macroes
; create 100
{init
  :tmp
  $tmp 10+(-^where 10+)
  !tmp
}

; macro that prints 99-2
{print_num
  :what:div:1s:10s
  %where(-$what+$div+)
  $div(-^where+)
  %div 10+
  $what &divmod
  $div(-)
  $10s(|"0"(-))
  $1s|"0"(-)
  $what(-)
  !10s!1s!div!what
}

; macro that prints the text between the numbers
{do_iteration
    :iter:zero:tmp
    (-$iter+$zero+)
    $zero(-^+)+
    switch $iter-
    (
      $tmp|" of beer on the wall"(-)
      $iter-
      (-$zero-|"."(-)10+..(-))
      $zero(-|", "(-))
    )
    $zero(-
            |" of beer."(-)10+.(-)
            $zero+
            $not_first((-)$zero-
                |"Go to the store and buy some more, 99 bottles of beer on the wall."(-)10+.(-))
            $zero(-|"Take one down and pass it around, "(-))
    )
  !tmp!zero!iter
}

; divmod performs divide and modulus at the same time
{divmod[->-[>+>>]>[+[-<+>]>+>>]<<<<<]*-3}

;; global variables
:not_first
:round
:number
:copy
:flag

;; main program starts here
$number &init
while $number
(
  $round++
  $not_first(-$round+)
  while $round
  (
    $number(-$copy+$flag+)
    $flag(-$number+)+
    switch $copy
    -(-
      (+
        $copy &print_num
        $flag-
        $copy(-)
        |" bottles"(-)
      ) $flag (-
          |"1 bottle"(-)
    )) $flag (-
          |"No more bottles"(-)
          $not_first+
    )

    $round &do_iteration
    $round-
  )
  $not_first+
  $number-
)


```



## FALSE


```false
[$." bottle"$1-["s"]?" of beer"]b:
99
[$][b;!" on the wall
"b;!"
Take one down and pass it around
"1-b;!" on the wall
"]#%
```




## HQ9+


```hq9plus
9>
```



## Intercal

See [[99 Bottles of Beer/Intercal]]


## LOLCODE


```LOLCODE
HAI 1.3

I HAS A bottles ITZ 99
I HAS A plural ITZ "Z"
I HAS A lyric ITZ "99 BOTTLZ OV BEER"

IM IN YR song
    VISIBLE lyric " ON TEH WALL"
    VISIBLE lyric
    VISIBLE "TAEK 1 DOWN, PAZ IT AROUN"

    bottles R DIFF OF bottles AN 1
    NOT bottles, O RLY?
        YA RLY, VISIBLE "NO MOAR BOTTLZ OV BEER ON TEH WALL", GTFO
    OIC
    BOTH SAEM bottles AN 1, O RLY?
        YA RLY, plural R ""
    OIC

    lyric R SMOOSH bottles " BOTTL" plural " OV BEER" MKAY
    VISIBLE lyric " ON TEH WALL:)"
IM OUTTA YR song

KTHXBYE
```



## Piet

[http://www.toothycat.net/~sham/piet/99bottles.png see image]


## SNUSP


```snusp
   /=!/
### =====
!/==+++++++++#   +9
   |  |  /=!/=====@/==@@@+@+++++# +48 (itoa)
   |  |  |  |  /==!/==@@@@=++++#  +32 (space)
   |  |  |  |  |   \==@@++\!+++++++++++++\!+++++\
   9  9 '9  9' space     'b'            'o'    't'
 $@/>@/>@/>@/>@/>
### =====@/>============@/>=
@/>++++++++++  \n  setup
/
### =================================loop==
>\!=>\!<<<<<<<< /
\@\@\>cr.@\< ?\<->+++++++++>->+++++++++\       |   |
  ! |     |   \===-
### ==
>=>-==BCD==!\< @\< ?/< ?/# no more beer!
  /=|=====|
### ==========================
/
  | |     \<++t.<<----a.>----k.<++++e.<_.>>++++o.-n.< e.<_.>-d.>+o.>+++w.<-n.<<_.\
  | |     /                                                                      /
  | |     \>---a.>n.<+++d.<_.>>++p.<---a.>>----s.s.<<<_.>>-------i.>+t.<<<_.\
  | |     /                                                                 /
  | |     \>a.>>--r.<++++++o.>+++u.<-n.<+++d.>>>cr.<-T<+O<--B<<<#
  | !
  \@\<<<_.>>o.-n.<<_.>>>++t.<<+++h.---e.<_.>>>+++w.<<----a.>--l.l.>>CR.<---T<+++O<+B<<<#
    |
    \9.>9.>_.>B.>O.>T.t.<---l.<+++e.>>-s.<<<_.>>+++O.<+f.<_.>----b.+++e.E.>>-R.#
```



## xEec


```xEec

h#99 >0000 o# jn000_ >0000_ p o# jn0_00 >000_0 p jn00_0 >00_00 p h#1 r ms t h#1 ms p h?
jz00100 p o# jn001_0 >01000 p jn0000 >00100 p o# jn00_10 >_0000 p o# jn0_100 >0_000 p o#
jn01110_ >1 p jn0200_0 >10_010 jz~0 >00_0 h#0 h#32 h$, h$d h$n h$u h$o h$r h$a h#32 h$t
h$i h#32 h$s h$s h$a h$p h#32 h$d h$n h$a h#32 h$n h$w h$o h$d h#32 h$e h$n h$o h#32 h$e
h$k h$a h$T >lt o$ p jnlt jz00_00 >0200_0 h#0 h#10 h$. h$l h$l h$a h$w h#32 h$e h$h h$t
h#32 h$n h$o h#32 h$r h$e h$e h$b h#32 h$f h$o h#32 h$s h$e h$l h$t h$t h$o h$b h#32 h$e
h$r h$o h$m h#32 h$o h$n h#32 h$, h$d h$n h$u h$o h$r h$a h#32 h$t h$i h#32 h$s h$s h$a
h$p h#32 h$d h$n h$a h#32 h$n h$w h$o h$d h#32 h$e h$n h$o h#32 h$e h$k h$a h$T >10_01
o$ p jn10_01 jz10_010 >0_00 h#0 h#10 h$. h$r h$e h$e h$b h#32 h$f h$o h#32 h$s h$e h$l
h$t h$t h$o h$b h#32 >2211 o$ p jn2211 jz000_0 >000_ h#0 h#32 h$, h$l h$l h$a h$w h#32
h$e h$h h$t h#32 h$n h$o h#32 h$r h$e h$e h$b h#32 h$f h$o h#32 h$s h$e h$l h$t h$t h$o
h$b h#32 >1122 o$ p jn1122 jz0000_ >001_0 h#0 h#10 h$. h$l h$l h$a h$w h#32 h$e h$h h$t
h#32 h$n h$o h#32 h$r h$e h$e h$b h#32 h$f h$o h#32 h$s h$e h$l h$t h$t h$o h$b h#32
>1111 o$ p jn1111 jz01000 >00_10 h#0 h#10 h$. h$l h$l h$a h$w h#32 h$e h$h h$t h#32 h$n
h$o h#32 h$r h$e h$e h$b h#32 h$f h$o h#32 h$e h$l h$t h$t h$o h$b h#32 >2121 o$ p jn2121
jz_0000 >0_100 h#0 h#32 h$, h$l h$l h$a h$w h#32 h$e h$h h$t h#32 h$n h$o h#32 h$r h$e
h$e h$b h#32 h$f h$o h#32 h$e h$l h$t h$t h$o h$b h#32 >1331 o$ p jn1331 jz0_000 >01110_
h#0 h#10 h$. h$r h$e h$e h$b h#32 h$f h$o h#32 h$e h$l h$t h$t h$o h$b h#32 >1551 o$ p
jn1551 jz1 >~0 h$. h$l h$l h$a h$w h#32 h$e h$h h$t h#32 h$n h$o h#32 h$r h$e h$e h$b
h#32 h$f h$o h#32 h$s h$e h$l h$t h$t h$o h$b h#32 h$9 h$9 h#32 h$, h$e h$r h$o h$m h#32
h$e h$m h$o h$s h#32 h$y h$u h$b h#32 h$d h$n h$a h#32 h$e h$r h$o h$t h$s h#32 h$e h$h
h$t h#32 h$o h$t h#32 h$o h$G h#10 h$. h$r h$e h$e h$b h#32 h$f h$o h#32 h$s h$e h$l h$t
h$t h$o h$b h#32 h$e h$r h$o h$m h#32 h$o h$n h#32 h$, h$l h$l h$a h$w h#32 h$e h$h h$t
h#32 h$n h$o h#32 h$r h$e h$e h$b h#32 h$f h$o h#32 h$s h$e h$l h$t h$t h$o h$b h#32 h$e
h$r h$o h$m h#32 h$o h$N >5115 o$ p jn5115

```



## Whenever


```whenever
1 defer (4 || N(1)<N(2) && N(2)<N(3)) print(N(1)+" bottles of beer on the wall, "+N(1)+" bottles of beer,");
2 defer (4 || N(1)==N(2)) print("Take one down and pass it around,");
3 defer (4 || N(2)==N(3)) print(N(1)+" bottles of beer on the wall.");
4 1#98,2#98,3#98;
```

