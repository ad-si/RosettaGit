+++
title = "Talk:Factorial base numbers indexing permutations of a collection"
description = ""
date = 2019-05-30T18:34:20Z
aliases = []
[extra]
id = 22102
[taxonomies]
categories = []
tags = []
+++

== Mojibake and misspellings ==

(As the task description is fairly difficult to read as it stands, I took a shot at revising it slightly, correcting misspellings and translating the erroneous Windows-1252->utf8 mojibake. Note that this is just my interpretation as I understood it. I am not the task author. Feel free to edit / correct anything I have gotten wrong.)


### =======================================================================================================================


You need a random arrangement of a deck of cards, you are sick of lame ways of doing this.

This task is a super-cool way of doing this using factorial base numbers.

The first 25 factorial base numbers in increasing order are:

0.0.0, 0.0.1, 0.1.0, 0.1.1, 0.2.0, 0.2.1, 1.0.0, 1.0.1, 1.1.0, 1.1.1, 1.2.0,
1.2.1, 2.0.0, 2.0.1, 2.1.0, 2.1.1, 2.2.0, 2.2.1, 3.0.0, 3.0.1, 3.1.0, 3.1.1,
3.2.0, 3.2.1, 1.0.0.0

Observe that the least significant digit is base 2, the next; base 3, and so on. In general, an '''n'''-digit factorial base number will use the digits '''0..n''' in base '''n+1''' for each place '''1..n''' (from least to most significant.)

It is simple to produce a 1-to-1 mapping between an '''n''' digit factorial base number and permutations of an '''n+1''' element array:

       0.0.0 -> 0123
       0.0.1 -> 0132
       0.1.0 -> 0213
       0.1.1 -> 0231
       0.2.0 -> 0312
       0.2.1 -> 0321
       1.0.0 -> 1023
       1.0.1 -> 1032
       1.1.0 -> 1203
       1.1.1 -> 1230
       1.2.0 -> 1302
       1.2.1 -> 1320
       2.0.0 -> 2013
       2.0.1 -> 2031
       2.1.0 -> 2103
       2.1.1 -> 2130
       2.2.0 -> 2301
       2.2.1 -> 2310
       3.0.0 -> 3012
       3.0.1 -> 3021
       3.1.0 -> 3102
       3.1.1 -> 3120
       3.2.0 -> 3201
       3.2.1 -> 3210

The following pseudo-code demonstrates the procedure to generate the mapping:

Starting with '''m=0''' and '''Ω''', an array of elements to be permutated, for each digit '''g'''
starting with the '''most''' significant digit in the factorial base number.

* If '''g''' is greater than zero, rotate the elements from '''m''' to '''m+g''' in '''Ω''' (see example)
* Increment '''m''' and repeat the first step using the next most significant digit until the factorial base number is exhausted.

For example: using the factorial base number '''2.0.1''' and '''Ω''' = '''0 1 2 3''' where place 0 in both is the most significant (left-most) digit/element.

* Step 1: '''m=0 g=2'''; Rotate places 0 through 2. '''0 1 2 3''' becomes '''2 0 1 3'''
* Step 2: '''m=1 g=0'''; No action.
* Step 3: '''m=2 g=1'''; Rotate places 2 through 3. '''2 0 1 3''' becomes '''2 0 3 1'''



;Task:

* '''Part 1:''' Use your function to recreate the permutation table of 3 digit factorial base numbers as above. Show the output here.

* '''Part 2:''' Use your function to generate all permutations of 11 digits. Count them, don't display them, and compare this method with the method in RCs [[Permutations]] task.

* '''Part 3:''' Use your function to create the corresponding permutations of the following two random 51 digit factorial base numbers:

    39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0
    51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1

Use the following shoe of cards as your '''Ω''' array:

    A♠K♠Q♠J♠10♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥10♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦10♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣10♣9♣8♣7♣6♣5♣4♣3♣2♣

* '''Part 4:''' Finally, create your own 51 digit factorial base number and produce the corresponding permutation of the above shoe.


### =======================================================================================================================

::Thanks--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:21, 10 December 2018 (UTC)

== Why not using the inverse of [http://rosettacode.org/wiki/Knuth_shuffle Knuth_shuffle] ==

this has the same factorial base number, but one has to do only length-1 swaps instead of rotation.
It ist easy to create a map between source and goal arrangement.

```txt

source 1.3.2.0 with value,indices (0,3|1,0|2,1|3,1)
goal   0,1,2,3 with value,indices (0,0|1,1|2,2|3,3)
start index = 0
first swap 0 <-> 1 => index 0 <-> 3 -> update Value,Indices for swapped values ->
(Swap Index = 3 out of [0..3])
source 0.3.2.1 with value,indices (0,0|1,3|2,2|3,1)

index++ => 1
swap 3 <-> 1 => index 1 <-> 3 
(Swap Index = 3 out of [index..3])
source 0.1.2.3 with value,indices (0,0|1,1|2,2|3,3)

index++ => 2
index  2 = index of value -> no swap;
source 0.1.2.3 with value,indices (0,0|1,1|2,2|3,3)

index++ => 3
index 3 = index of value -> no swap;
source 0.1.2.3 with value,indices (0,0|1,1|2,2|3,3)

```

I see that the advantage of the rotation is the lexicographical order of generating the permutations, but what is it good for?
:see [[First perfect square in base N with N unique digits#Using Factorial base numbers indexing permutations of a collection]]--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 18:34, 30 May 2019 (UTC)
