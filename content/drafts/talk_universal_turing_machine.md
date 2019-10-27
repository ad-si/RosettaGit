+++
title = "Talk:Universal Turing machine"
description = ""
date = 2014-08-03T21:14:15Z
aliases = []
[extra]
id = 12939
[taxonomies]
categories = []
tags = []
+++

== Examples sufficient to find bugs in UTM? ==

I want to raise the question if the proposed examples (simple incrementer / busy beaver) are enough to find bugs in the code. I have to admit that I uploaded two faulty Java implementations that worked fine with both examples, before I found a (hopefully) correct version. I propose to add this example that helped me fix the bugs:

Sort a tape containing arbitrary sequences of "a" and "b" alphabetically ("*" being blank symbol, "s0" initial state, "see" terminal state):
<!-- DKF: language arbitrary, but gets right highlighting -->

```java
("s0", "a", "s0", "a", right)
("s0", "b", "s1", "B", right)
("s0", "*", "se", "*", left)
("s1", "a", "s1", "a", right)
("s1", "b", "s1", "b", right)
("s1", "*", "s2", "*", left)
("s2", "a", "s3", "b", left)
("s2", "b", "s2", "b", left)
("s2", "B", "se", "b", left)
("s3", "a", "s3", "a", left)
("s3", "b", "s3", "b", left)
("s3", "B", "s0", "a", right)
("se", "a", "se", "a", left)
("se", "*", "see", "*", right)
```


Example: <tt>abbabbabababab</tt> => <tt>aaaaaabbbbbbbb</tt>

--[[User:Coenig|Coenig]] 14:11, 17 February 2013 (UTC)

: We get: 0111111222222220. The blanks on both ends seem to be in the rules, don't you get them? Unless I'm not reading it correctly: the very last rule, for instance, writes a blank just before he goes to the halting position, so it has to be part of the output.[[User:Fwend|Fwend]] 20:36, 17 February 2013 (UTC)

: I don't think that you understood my TM definition correctly (or I'm wrong in understanding what you mean). I used a different notation than on the main site. In my TM s1, s2, ... are the states, a, b are the tape symbols, and * is the blank symbol.

: Then, ("s1", "a", "se", "b", left) means that we read a on the tape, switch from state s1 to state se, write b on the tape and move to the left. 0, 1, 2 are not tape symbols. You are right, thogh, that the tape looks something like this in the end: <tt>**aaaaaabbbbbbbb**</tt> --[[User:Coenig|Coenig]] 07:36, 19 February 2013 (UTC)
:: In the D implementation, we use different symbols, but the output is the same. [[User:Fwend|Fwend]] 16:16, 19 February 2013 (UTC)
::: Okay, now I see. That seems right to me, I get the blanks, too. (Didn't get that you were referring to the D implementation.) Should we add the example to the task definition? --[[User:Coenig|Coenig]] 19:11, 19 February 2013 (UTC)
:::: I think that's a good idea, provided that you use the same notation as the other examples. [[User:Fwend|Fwend]] 19:48, 19 February 2013 (UTC)
:::: 
<blockquote>
For instance:
    
```txt

    States: A, B, C, D, E, HALT
    Initial state: A
    Terminating states: HALT
    Permissible symbols: 0, 1, 2, 3
    Blank symbol: 0
    Rules:
    (A, 1, 1, right, A)
    (A, 2, 3, right, B)
    (A, 0, 0, left,  E)
     etc

    Input: 12212212121212
    Expected output: 0111111222222220
    
```

BTW, I think we should add expected output to all the examples. [[User:Fwend|Fwend]] 20:06, 19 February 2013 (UTC)
</blockquote>

== Universal Turing machine or just a Turing machine? ==

From reading [https://en.wikipedia.org/wiki/Universal_Turing_machine#Example_of_universal-machine_coding Wikipedia] I get the impression that a true UTM would have static pre-defined action table and would read an encoding of a specific TM from tape (along with the input for that TM) and then using the static action table effectively execute that TM producing the required output.

That's a much more difficult task that the one actually solved here which is just a general re-programable TM. (I'd also think a true UTM task wouldn't be appropriate here as it mostly involves writing a very complicated TM action table and then just inputting it to one of the general TM's specified here.)

Am I missing something or should this task be renamed/clarified.

-- [[User:Dchapes|Dchapes]] ([[User talk:Dchapes|talk]]) 21:14, 3 August 2014 (UTC)
