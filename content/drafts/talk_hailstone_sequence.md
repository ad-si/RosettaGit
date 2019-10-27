+++
title = "Talk:Hailstone sequence"
description = ""
date = 2016-04-01T05:27:16Z
aliases = []
[extra]
id = 7585
[taxonomies]
categories = []
tags = []
+++

==Merge notice==
{{alertbox|yellow|Merged '''[[Collatz conjecture]]''' &rarr; '''[[Hailstone sequence]]'''.

The old talk page was at '''[[Talk:Collatz conjecture]]'''.}}

==Brainf***==
I have restrained myself from marking the Brainf*** example incorrect as it barely scrapes through at showing the sequence, let alone the other parts to the task. Given the nature of the language I might just sit back and wonder at the masochism. (No doubt in some future war with The Aliens, BF programmers will come into their own by decoding how their spaceships work and programming them to self destruct). --[[User:Paddy3118|Paddy3118]] 08:27, 23 June 2010 (UTC)
:And if BFers can't do it, the SNUSPers can! --[[User:IanOsgood|IanOsgood]] 00:18, 25 June 2010 (UTC)

:: :-)
--[[User:Paddy3118|Paddy3118]] 04:17, 25 June 2010 (UTC)

==Fortran==
The following fortran code should be correct (at least to display the sequence) but it doesn't seem to work.  Most numbers will be processed correctly but certain numbers will cause a weird arithmetic error during which the program will multiply by 3 and SUBTRACT 1 instead of adding, resulting in an infinite loop. The smallest of these numbers appears to be 113383, might this be a specific problem with my machine or compiler(im using force 2.0 atm, ancient, I know)?  Nothing too special about that number other than that it is prime, more of these glitched numbers seem to appear more frequently as the numbers get higher.
   

       PROGRAM Hailstone
        IMPLICIT NONE
        INTEGER :: num, tnum
        outer:  DO
            PRINT *, "Type in your number (0 terminates)"
            READ *, num
            IF (num .LE. 0) EXIT
            inner:      DO
                tnum = num/2
                IF (2*tnum .EQ. num) THEN    ! num is even
                    num = tnum
                ELSE    ! num is odd
                    num = 3*num+1
                END IF
                PRINT *, num
                IF (num == 1) THEN
                    EXIT
                END IF
            END DO inner
        END DO outer
        END PROGRAM Hailstone

: Might it be integer overflow? --[[User:Paddy3118|Paddy3118]] 19:44, 13 July 2010 (UTC)
: I doubt it, since 113383 is not a power of 2 and the next few integers work fine all the way up to 134379, which is the next number that does this.
: It seems to have something to do with the DO-loop since if i remove the loop (essentially making a program that does only one iteration of the hailstone sequence) the arithmetic works fine.
:: I meant maybe the sequence starting from that number overflowed. Try printing all the variables each time through the loop, or changing the exit condition to less than or equal to 1? --[[User:Paddy3118|Paddy3118]] 04:52, 14 July 2010 (UTC)
::: I'm not sure what you mean but the exit condition isn't working anyway, I've actually seen numbers result a 1,2,1,2,1,2 oscillation, ignoring the fact that 1 was reached already.
:::: Definitely an integer overflow. Not at the number 113383, but at the number 827370449 which is reached at the 119th step in the sequence starting at 113383. By attempting to compute 3n+1 from there, the result reached is 2482111348 upon which the variable num overflows to -1812855948. You can postpone this failure by defining your variables INTEGER*8 (I'm also sure you never saw a 1,2,1,2... but a -1,-2,-1,-2; which you can convince yourself is perfectly legal if you can ever reach negative numbers.)   [[User:Sgeier|Sgeier]] 20:39, 24 August 2010 (UTC)

== Euphoria missing ==

Not a big one, but it's a pity that Euphoria is missing --[[Special:Contributions/95.97.39.162|95.97.39.162]] 14:40, 6 June 2011 (UTC)
: Add it! It's an open-edit wiki. --[[User:Short Circuit|Michael Mol]] 16:06, 6 June 2011 (UTC)

==deleted examples==

During the addition/changes for various BASICs, why were two programming examples were removed?

The examples were:
::::::* Batch File
::::::* XPL0

No mention was given to explain the reason of the deletion of those two examples. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:43, 5 November 2013 (UTC)


==Haskell==

The Haskell version is 18 lines long. This version is 12 lines long and prints all of the chain for n = 27:

        import Data.List
        import Data.Ord(comparing)
        
        main = do putStrLn $ "Collatz sequence for 27: " ++ ((show.hailstone) 27) ++ "\n"
                  putStrLn $ "The number " ++ (show longestChain)
                    ++" has the longest hailstone sequence for any number less then 100000."
        
        hailstone n
          | n == 1 = [1]
          | even n = n:(hailstone (n `div` 2))
          | otherwise = n:(hailstone (3*n+1))
        
        longestChain = fst $ maximumBy (comparing snd) $ map ((\x -> (x,(length.hailstone) x))) [1..100000]

Should this be used instead? [[User:Mathlover2|Mathlover2]] ([[User talk:Mathlover2|talk]]) 16:31, 24 February 2015 (UTC)

== Does the program need to do all this on its own? ==

Does the program need to show the sequence for 27 and find the number <100,000 with the longest sequence, or do we just need to use the program to find these?
== cache version of C ==
Running the c cache version [[Hailstone_sequence#With_caching|C]] shows:

```txt
max below              10000000 : 7532665, length 616
```

Pascal version shows:

```txt
Longest sequence under 10000000 : 8400511 with 686 elements
```

I think the problem of C-Cache is the fact that beginning with i=159487-> 5097000814 > 2^32 > unsigned long the calculation gets wrong.
I use 32 Bit.
edit: some more start values reaching highest value in the sequence:
I thought nearly quadratic for high values, but the last 319804831 -> 1414236446719942480 doesn't fit 

```txt
Longest sequence under         10 :         9 with   20 elements
Highest value          15 ->                 160
Longest sequence under        100 :        97 with  119 elements
Highest value          27 ->                9232
Longest sequence under       1000 :       871 with  179 elements
Highest value         703 ->              250504
Longest sequence under      10000 :      6171 with  262 elements
Highest value        9663 ->            27114424
Longest sequence under     100000 :     77031 with  351 elements
Highest value       77671 ->          1570824736 <= just below 1 shl 31
Longest sequence under    1000000 :    837799 with  525 elements
Highest value     1042431 ->         90239155648
Longest sequence under   10000000 :   8400511 with  686 elements
Highest value     6631675 ->      60342610919632
Longest sequence under  100000000 :  63728127 with  950 elements
Highest value   120080895 ->    3277901576118580
Longest sequence under 1000000000 : 670617279 with  987 elements
Highest value   319804831 -> 1414236446719942480 <= nearly limit of Uint64 
```


EDIT found a good web site Tomás Oliveira e Silva
[[http://sweet.ua.pt/tos/3x+1.html]] with a list of first occurences of highest value upto 2^58  
[[http://sweet.ua.pt/tos/3x+1/t1.txt.gz]]
I dont know, why his values or only * 0.5 instead of 27 ->9232 is listed with  4616, 319804831 -> 1414236446719942480 is listed with 707118223359971240.



-----



It looks like my (lucky) guesstimate for REXX of   '''20 decimal digits'''   was very close to being on the nose, with the number   '''319,804,831's'''   Collatz sequence having   '''19'''   decimal digits for its maximum value   ('''1,414,236,446,719,942,480''').   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:56, 31 March 2016 (UTC)
:: thank you. thumps up . I thought about it and conclude that they speed up calculation by the observation, that every odd number gets even by 3*n+1 so they divide by 2 in the same calculation without the need of an IF-statement and add 2 to the length of the sequence.[[User:Horsth|Horsth]]
