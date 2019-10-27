+++
title = "Talk:Morse code"
description = ""
date = 2016-05-07T21:21:43Z
aliases = []
[extra]
id = 8081
[taxonomies]
categories = []
tags = []
+++

== Dependence on the PC speaker ==

Some computers (including mine) don't have a configured/enabled PC speaker. Consider specifically allowing any sound output device instead. --[[User:Short Circuit|Michael Mol]] 07:48, 24 August 2010 (UTC)

== Multiple roles ==

This task covers at least three things:
* File I/O
* Encoding (characters to Morse)
** Notably limited to Latin characters.
* Sound output.

At the very least, I think these separate components should be specced in the task description as being modular components, so that there is a readable boundary in code between roles. There are some other examples of such elsewhere on the wiki. --[[User:Short Circuit|Michael Mol]] 07:55, 24 August 2010 (UTC)
: So let's simplify it to a direct string output. This omits the issue of File I/O, and the Python solution did that anyway. --[[User:Abu|Abu]] 09:46, 24 August 2010 (UTC)
:: String ''in''put? --[[User:Paddy3118|Paddy3118]] 13:39, 24 August 2010 (UTC)
::: No, I meant indeed ''out''put, in the sense of "output a given string in Morse code". Anyway, the task itself says "Send a string ..." :) --[[User:Abu|Abu]] 13:51, 24 August 2010 (UTC)
:::: Interesting. Now it simplifies to an encoding map. --[[User:Short Circuit|Michael Mol]] 14:18, 24 August 2010 (UTC)


==international Morse code characters==
(''International Morse''   was originally called   ''Continental Morse''.)

```txt

The following are the international Morse code characters:

. _                          A  letter
_ . . .                      B  letter
_ . _ .                      C  letter
_ . .                        D  letter
.                            E  letter
. . _ .                      F  letter
_ _ .                        G  letter
. . . .                      H  letter
. .                          I  letter
. _ _ _                      J  letter
_ . _                        K  letter
. _ . .                      L  letter
_ _                          M  letter
_ .                          N  letter
_ _ _                        O  letter
. _ _ .                      P  letter
_ _ . _                      Q  letter
. _ .                        R  letter
. . .                        S  letter
_                            T  letter
. . _                        U  letter
. . . _                      V  letter
. _ _                        W  letter
_ . . _                      X  letter
_ . _ _                      Y  letter
_ _ . .                      Z  letter
_ _ _ _ _                    0  digit zero
. _ _ _ _                    1  digit one
. . _ _ _                    2  digit two
. . . _ _                    3  digit three
. . . . _                    4  digit four
. . . . .                    5  digit five
_ . . . .                    6  digit six
_ _ . . .                    7  digit seven
_ _ _ . .                    8  digit eight
_ _ _ _ .                    9  digit nine
. _ _ _ _ .                  '  apostrophe
_ _ _ . . .                  :  colon
_ _ . . _ _                  ,  comma
_ . . . . _                  -  minus or hyphen
_ . _ _ . _                  (  left parenthesis
. _ . _ . _                  .  period or dot
. . _ _ . .                  ?  question mark
_ . _ . _ .                  ;  semi-colon
_ . . _ .                    /  slash or vergule or solidus
. . _ _ . _                  _  underscrore
. . . _ . . _                $  dollar sign
. _ . _ . .                  !  exclamation mark
_ _ _ . .                    )  right parenthesis
_ . . . _                    =  equal sign
. _ _ . _ .                  @  comercial at
_ _ _ _ .                    &  ampersand
. _ . . _ .                  "  double-quote
. _ . _ .                    +  plus sign

[added by Gerard Schildberger]

```


==USA's railway morse code characters==
(''railway'' and ''railroad'' are synonymous, although ''railroad'' seems to be preferred in USA.)

```txt

The following are the USA's railway Morse code characters, they're
the same as the International Morse code except for the following:

. . pause .                  C  letter,            railroad
. long_ .                    F  letter,            railroad
_ . _ .                      J  letter,            railroad
long_                        L  letter,            railroad
. pause .                    O  letter,            railroad
. . . . .                    P  letter,            railroad
. . long_ .                  Q  letter,            railroad
. pause . .                  R  letter,            railroad
. long_ . .                  X  letter,            railroad
. . pause . .                Y  letter,            railroad
. . . pause .                Z  letter,            railroad
veryLong_                    0  digit zero         railroad
. _ _ .                      1  digit one,         railroad
. . long_ . .                2  digit two,         railroad
. . . long_ .                3  digit three,       railroad
_ _ _                        5  digit five,        railroad
. . . . . .                  6  digit six,         railroad
_ _ . .                      7  digit seven,       railroad
_ . . . .                    8  digit eight,       railroad
_ . . _                      9  digit nine,        railroad
. _ . _                      ,  comma,             railroad
. . _ _ . .                  .  period or dot,     railroad
_ . . _ .                    ?  question mark,     railroad
_ _ _ .                      !  exclamation mark,  railroad
. _ . . .                    &  ampersand,         railroad

[added by Gerard Schildberger]

```

