+++
title = "Talk:First perfect square in base N with N unique digits"
description = ""
date = 2019-07-16T18:16:27Z
aliases = []
[extra]
id = 22335
[taxonomies]
categories = []
tags = []
+++

==Leading zeros ?==

Perhaps the 'First perfect square in base 2 with 2 unique digits' is arguably '01' ? 
[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:53, 20 May 2019 (UTC)

: Ok, kind of weaselly but fair point. Reworded; First perfect square in base 2 with 2 '''significant''' unique digits. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 01:17, 21 May 2019 (UTC)
==Proof==
The task conjectures that the sum from n=0 to some n of 2n+1 has at least one value of n for every base for which the sum is a number containing all digits in the base. Can you prove this conjecture?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 21:34, 22 May 2019 (UTC)

:Prove? No. Or, at least, '''I''' can't. Though it is pretty easy to ''find'' perfect squares in bases 2 through 36 that contain 2 through 36 unique digits respectively. I have no reason to not believe that the same holds true for higher bases. Note that there is no constraint that the square in base N has '''only''' N digits; only that each digit from 0 to N-1 appears at least once. The challenge is in finding the '''smallest''' such number. 

:I ran this in the Perl 6 version for bases 17 through 36 searching arbitrarily large squares. Took less than 1/4 second to complete.   

```txt
Base 17:                       5GFF91A47EFG754GBDEC9DB68GGG² == 21G3G8551E554G6B342BGF4018G54650E0GD189C01G6740EAE1CE7F4
Base 18:                      72255AD78158400C14341DF6HFDAG² == 2EC0BEFB6B45H67C948CEGDDB57707HEGD2D01F68A1675FH9967385HDA
Base 19:                     84C2B1GB9668FI4B8641GC1167BDF9² == 3AI6D3A2DA8DIA81C46G661H0GI2DB358724FB59E9CH21AFG60E1A4D0DIH
Base 20:                    9655G27F1B50133AG5AGIB9GC5I16HC² == 46EE97G32A654CIE49IGB500000000FGCJH813CD2A000000000000000000E9
Base 21:                   A5A4GA58DB9J5K2C4C14B908214756C7² == 5063DK4HA0B78D5KAIKB40345K19JF89AGCD4JE916I8B7277215F0BC04GC8904
Base 22:                  B0D611CBEG6I3K8ED18EG4K70FH70EA4G² == 5BD691HA2A1JG0JG59G4IFH2KH98LFK557F7396BJ65KHG1JI9081J3D23E3D63BGC
Base 23:                 BCKD3DBCA7KA3E32FD26G0AB8G317DLAK6² == 5IEKJCJKBHKI1J4ED24GBLLCFCM5FC5J3115L74H4D41LLFI9EEA6AK3B605HMFI4LC8
Base 24:                BJJB1AL6I37MH6CNJ3JEK6EBK4G9L97ECAG² == 5JK4EF03B223EJ153H6NC94B2BCA7E91HI0G4G1GF2CJDHHB08EC8M412J3F0JCAM7GL81
Base 25:               BK3HB0BCN4O23H621G29O4KE2DGIM1D0FECA² == 5E9C73HH28G8I5G5CA53F1000000000002MF6NDH4KLJOG6000000000000000000009BAD9
Base 26:              BD4HKMDK973KL4HH94HJ8M0JN109MP9MMD84G² == 52AGK7MKMH0ENMCJ3A6AJD1FL8PDMCABC9LBJM3EJ0878MHE9BK544L27HIA9G27GEBA2NOPP3
Base 27:             APJNIHGH3F61KI69MHEEEHIA9DC05K7MG6195K² == 4BQ6O8IQEI6B95D1AFEE6LBLAILJBQGPEQPOC7KLN0ONJH3KD1LGGI2H0FQ5QI2GNF3I09ENE3AM
Base 28:            A55IE6QOCHG0R55GF2H20LO8MDCM143NA0Q6M14² == 3JL07Q303FAFFH0JN6GAJBHBI6D06HB1HBFI4J1JB8H2KJCE5KK1N4C5D2IR1LLME8J1JO16N9A9OP
Base 29:           97D812BS18P64KPI624O0DL0BSAC0GK8NNDOMI2E² == 2RK4HI8MRD1LDMRC80H9HA6AOOQEH7CBI82GG5EDNOMM3912SLFJ0J8FP1MRBMF8FILB1021O80SPN3S
Base 30:          86MM394P48KQMQ728TTCLLO8S0M07R1C493QTTC3E² == 27JJDP5BJ77KKA8JJHOAJ8KSG61TA0Q0NL683LE5P6TOGO5MO0L28HEREF8INATMEDGOS3O4669BMK7C7F
Base 31:         74I06DPHJJNGO4NJM3KDNMNR897TEJU2G1M9CH6QL4² == 1K2P2D4IKE0P5HM89DL52H47K2NPT3AT1Q49EJ5KU6O13IPR077EGFDDI94TI2R8R6OCUI727SLB5LHP1S58
Base 32:        62F0FKJVUFL00000000000000000000000000000000² == 14TQ8V4O1QCCRV36UCRC6QOG2DO26JPVQUO8RKSGNHCVAC5OALRGD3PMIOGM4LB1AB4KILE9700000000001FH
Base 33:       51T5JT538NMJDB8TNGSG8U93DSDSGTI9A8HEGBKW4JIJ² == PIV84U6UKBBI09WIBK966BVQI3T6TMW8MKQ5929NEAI1ENKJLGB9FPU1OSTS6KB6GDUUN7RN6F852C67JHV491G
Base 34:      4431S5BXSONE2IXPWJGXOK8U5CVEPBBJRRKIXBAKI4NAG² == GX75B752G2S64UFUJ4L5BR3X3RSOGRNREHDXN472CD4INIVLLF1NKKEI6R9AH4SPOG3LQMP0UNXS80KMOWFTV7SI8
Base 35:     3A2U0DIKHQASF0FQ29TQ5TSSVF719C5ONHMVQJVPNB8CNK² == ASDRFOOC3068TDRBUSS1A65ITSTGV6V5392DVCAJAPONRVJU13E0H76ISWH34S2OOFOXYOM06RAAS6LGRGK98NQNBLB
Base 36:    2KJVULABXHKEQZQGYBM5INOYJ5OKGMNXF53URVDRT6EF5KW² == 6LXXRR9OJDK9NUEQQDF373MXEDKOPQYYRU5VPU6F1S5QRYWCPOKCNGQBE1ERAOK4DQWWGH4XKMM028MZB3I3TSGJ64IC9
```


:Interesting in the abstract but not very useful. (I suppose the smallest such numbers aren't very useful either but... <shrug>) --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 23:36, 22 May 2019 (UTC)

::Actually, after a tiny bit of thought, I think I '''can''' prove it. 

::Base 2: Proof by demonstration. 10² == 100

::Base N > 2: Concatenate the digits 1 to N-1, two zeros, then N zeros. Find the integer ceiling of the square root and square it. The resulting number will always start with the digits 1 to N-1 and zero. It almost definitely won't be the ''smallest'' such number but it proves that at least one number exists for any N. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 14:31, 23 May 2019 (UTC)

==any ideas of optimizations ?==
The main runtime for bases 2..16 ( 99% ) is used to find the one for base 13.<BR>
First come into mind, that calculating strictly in the used base could be an advantage.<BR>
for n-> n+1 => n*n -> n*n + (2*n+1). If n*n is known than only addition to base is needed.<BR>
Squaring a number is easy by continiously doubling the number in base and add, if a bit of that number is set.<BR>
12 to base 13  : the digit 12 is 1100 in binary

```txt

sq= 00 , n2 = 0C|12dec , bit0 = 0 
sq= 00 , n2 = 1B|24dec , bit1 = 0 
sq= 00 , n2 = 39|48dec , bit2 = 1 
+   39
sq= 39 , n2 = 75|48dec , bit3 = 1 
+   75
sq= B1 -> 11*13+1 = 144 == 12*12

```

Obviously 2n+1 -> 2(n+1)+1  = (2n+1)+2<BR> 
Its a pitty that "carry := 0;if num >= base then num := num-base;Carry=1; " are not optimized to cmove in freepascal, like C or freebasic do.
But the addition has mainly halve the lenght of base conversion.<BR>
I hardcoded startvalues for base 22 ( glaciel ) that runs in <b>35.529s</b> on TryItOnline

```txt

start value first 
sqrnum above 1023456789ABCDEFGHIJKL
start  value 1023456789AF71694A3533  // 15588232346256352156349976289‬dec 
N  = sqrt(startvalue)     4F942523JK5  //124852842764017dec 
results in
n =    4F94788GJ0F  //124.853.426.667.963dec 
-> sqr ->
102369FBGDEJ48CHI7LKA5 //15588378150732414428650569369dec
    26.508s
```

It takes 583.903.946 long and short additions

: speed up adding in base < 127 < 256 Div 2 by using Uin64 to pack 8 digits.By offsetting every digit of the sum by 256-base=$FF-base  one gets an natural carry into the next digit.
:example: base = 10 89 -> $0809 offset every digit by $F6 ( the new $00 ) -> $FEFF and add 1 = $0002

```txt

    $FEFF
   +$0002
    $FF01 
```
 
:Now check which digit overflows.If one digit overflows, its highest bit is not set anymore, so the digit 0 aka $F6 must be added 

```txt

    $FF01 ( pre-result )
XOR $8080 (= Overflow-MASK )
=   $0080 ( now shr by 7 Bits )
=   $0001 ( Multiply by $F6 -> zero Offset )
*     $F6 ( = zero offset mask )
=   $00F6 
+   $FF01 (now ADD pre-result )
=   $FFF7 ( if one like convert back subtract "zero"
-   $F6F6 
=   $0901 -> 91
```

: it speeds up additions by a factor of 3, but checking the used digits takes ~ 40% of runtime - 60%/3+40% = 60%

:: think of base 10 and the square of the last 2-digits. of a number.If the last 2 digits of the square are the same you need not to test the complete number.

```txt
  0 10 12 20 30 38 40 50 60 62 70 80 88 90 
86 of 100 are left over not that impressive
```

::using more digits increases the proportion even more 4 digits -> 4660 of 10000 are left over to test.
::but 4 digits to base 37 lead to 1542240 of 1874161 need to be checked.Not that useful.

==Space compression and proof ?==

Not sure whether there is a provably reducible pattern here which might yield some more compression of the search space, but there is a literature on the necessary cyclicality of the repeated digit sums of perfect squares, and if there also turned out to be a pattern to the squared digit sum of *these* particular squares, it might be possible to divide the search space by the length of the digit sum cycle for a given base.
For professional pattern searchers, the output of this problem is adorned for each base below with: 
# the repeated digit sum for each root and square, 
# a sample of the cycle of repeated digit sums for squares represented in that base (first 20 perfect squares)

```txt
Smallest perfect squares using all digits in bases 2-16,
with repeated digit sums for each root and square, and a sample of the digit sum cycle for squares represented in each base:

Base      Root    Square
 2 ->       10 ->  1 -> 100 -> 1
['1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1', '1']

 3 ->       22 ->  2 -> 2101 -> 2
['1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2']

 4 ->       33 ->  3 -> 3201 -> 3
['1', '1', '3', '1', '1', '3', '1', '1', '3', '1', '1', '3', '1', '1', '3', '1', '1', '3', '1', '1']

 5 ->      243 ->  1 -> 132304 -> 1
['1', '4', '1', '4', '1', '4', '1', '4', '1', '4', '1', '4', '1', '4', '1', '4', '1', '4', '1', '4']

 6 ->      523 ->  5 -> 452013 -> 5
['1', '4', '4', '1', '5', '1', '4', '4', '1', '5', '1', '4', '4', '1', '5', '1', '4', '4', '1', '5']

 7 ->     1431 ->  3 -> 2450361 -> 3
['1', '4', '3', '4', '1', '6', '1', '4', '3', '4', '1', '6', '1', '4', '3', '4', '1', '6', '1', '4']

 8 ->     3344 ->  7 -> 13675420 -> 7
['1', '4', '2', '2', '4', '1', '7', '1', '4', '2', '2', '4', '1', '7', '1', '4', '2', '2', '4', '1']

 9 ->    11642 ->  6 -> 136802574 -> 4
['1', '4', '1', '8', '1', '4', '1', '8', '1', '4', '1', '8', '1', '4', '1', '8', '1', '4', '1', '8']

10 ->    32043 ->  3 -> 1026753849 -> 9
['1', '4', '9', '7', '7', '9', '4', '1', '9', '1', '4', '9', '7', '7', '9', '4', '1', '9', '1', '4']

11 ->   111453 ->  5 -> 1240a536789 -> 5
['1', '4', '9', '6', '5', '6', '9', '4', '1', 'a', '1', '4', '9', '6', '5', '6', '9', '4', '1', 'a']
12 ->   3966b9 ->  b -> 124a7b538609 -> b
['1', '4', '9', '5', '3', '3', '5', '9', '4', '1', 'b', '1', '4', '9', '5', '3', '3', '5', '9', '4']

13 ->  3828943 ->  1 -> 10254773ca86b9 -> 1
['1', '4', '9', '4', '1', 'c', '1', '4', '9', '4', '1', 'c', '1', '4', '9', '4', '1', 'c', '1', '4']

14 ->  3a9db7c ->  d -> 10269b8c57d3a4 -> d
['1', '4', '9', '3', 'c', 'a', 'a', 'c', '3', '9', '4', '1', 'd', '1', '4', '9', '3', 'c', 'a', 'a']

15 -> 1012b857 ->  7 -> 102597bace836d4 -> 7
['1', '4', '9', '2', 'b', '8', '7', '8', 'b', '2', '9', '4', '1', 'e', '1', '4', '9', '2', 'b', '8']

16 -> 404a9d9b ->  f -> 1025648cfea37bd9 -> f
['1', '4', '9', '1', 'a', '6', '4', '4', '6', 'a', '1', '9', '4', '1', 'f', '1', '4', '9', '1', 'a']
```


A Python sketch FWIW, of a function from a given base and number to a repeated digit sum as an integer.
(For bases above 10 course, you will also need a `digit` function from the sum to a digit character) 


```python
# repSum :: Int -> Int -> Int
def repSum(base):
    '''Repeated sum of the digits of a number
       n in a given base.
    '''
    def f(x):
        m = 0
        while x:
            m = m + (x % base)
            x //= base
        return m

    def baseGT(n):
        return base > n

    def go(x):
        return until(baseGT)(f)(x)
    return lambda n: go(n)


# digit :: Int -> Char
def digit(n):
    '''Digit character for given integer.'''
    return '0123456789abcdefghijklmnopqrstuvwxyz'[n]


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.
    '''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)
```


The first pattern that jumps to the eye, after the self-evidences of bases 2 and 3, is that:
# The digit sum cycles for a given base are symmetric – a series of digits is repeatedly followed by its reflection.
# Where there is a single cycle of reflection/symmetry, and only one digit that is repeatedly flanked by twin siblings, that digit will also be the repeated digit sum of the first digit-saturating perfect square (square that needs all digits in the base) that we encounter.
# The picture is subtler or more obscure in the rarer cases where there are multiple symmetries in the digit sum cycle – more than one digit which is repeatedly flanked by identical twins. Such is, perhaps not accidentally, the case of '''base 13''', which is also consuming the most processor time here. (For base 13, we repeatedly see both '1', 'c', '1', and '4', '9', '4', and the first digit-saturation that we (eventually) find occurs at a point where the digit sum is 1 ...

Any conjectures ?  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:40, 23 May 2019 (UTC)

:Excellent. This reminded me of [[Digital root]] and [[Casting out nines]]. The Digital Root of a perfect square expressed in base n is a quadratic residual in base n-1. The quadratic residuals in base 9 are 1, 4, and 7. 0 is treated as 9 so 1+2+3+4+5+6+7+8+9 -> 45 -> 9 so there may be a 10 digit perfect square using all the digits in base 10. Just as well since we've found one. So for base 13 the digital root of a perfect square must be 1, 4, 9, or 12. 1+2+3+4+5+6+7+8+9+a+b+c -> 60 -> 6. So any time spent looking for a 13 digit perfect square using all the 13 digits in base 13 has been wasted. I can also determine which digits to repeat when looking for a 14 digit perfect square. To check 1+0+2+5+4+7+7+3+c+a+8+6+b+9 -> 67 -> 10 -> 1.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 20:48, 23 May 2019 (UTC)

:: Good !  Someone with a bit of mathematical culture. I wish I had some :-)  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:07, 23 May 2019 (UTC)

:: I have tentatively identified a candidate for the smallest base 17 number. (Still have not completed an exhaustive search of 17 digit numbers.) It would be interesting to see if the below fits in with your conjecture. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 23:19, 23 May 2019 (UTC)
 
   Base 17:  423F82GA9² == 101246A89CGFB357ED
:::The residuals base 16 are 1 4 9 16
0+1+...+15+16 -> 80 -> 8 therefore no 17 digit perfect square made from digits 0..g in base 17 so searching for one is pointless.
101246A89CGFB357ED -> 81 -> 9 therefore may be a perfect square, just as well since you say it is.
smallest possible number made repeating 1 is 10123456789abcdefg so you only need to verify that there is no perfect square between 10123456789abcdefg and 101246A89CGFB357ED which contains all the digits between 0 and g to prove that 101246A89CGFB357ED is the smallest.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 10:07, 24 May 2019 (UTC)
::::Note that adding a zero to a number doesn't change it's digital root so the repeated digit can not be zero so 10123456789abcdefg must be the smallest candidate--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:45, 25 May 2019 (UTC)
::::: Many thanks for confirming that Nigel. I had in fact realized it was true when submitting my original Go entry but omitted to explain the reasoning and so, as Thundergnat rightly pointed out, it looked like I was using a 'magic number'. You're also right that it's not really acceptable to assume an extra digit for bases 13 and 17 without further explanation so in my latest Go submission I'm justifying this from first principles. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 17:08, 25 May 2019 (UTC)

:::So digital root 9, I think, drawn from the dual-symmetry base 17 cycle for perfect squares of 
::: 
```txt
           [ '1', 'g', '1', '4', '9', 'g', '9', '4', '1', 'g', '1']
```
    
::: base 13 (digital root 1 for the first match),  also has a dual-symmetry cycle.
::: Conceivable that there is some kind of congruence there ?:

```txt

         #       '1', 'g', '1', '4', '9', 'g', '9', '4', '1', 'g', '1'  base 17  (digital root 9 for that candidate)
         #                            *         *
         #            '4', '9', '4', '1', 'c', '1', '4', '9', '4'       base 13  (digital root 1 for the first match found)
```

::: [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:59, 23 May 2019 (UTC)

::: Nigel, how would you formulate cases like base 12 and 14 in terms of quadratic residues ? The repeated digit sums of the first all-digit perfect squares seen for 12 and 14 are 'b' and 'd' respectively, corresponding to x^2 mod (N-1) == 0. Am I right in thinking that the 0 case is not usually treated as a member of the set of residuals ? Presumably we need to tack it on to our working lists of residuals here ... [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:01, 24 May 2019 (UTC)
:::: Scratch that – I see that you are already including such cases above in ''residuals base 16 -> 1 4 9 16'' [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:18, 24 May 2019 (UTC)

== analytically determine minimum start value ==

As a hint to other entry authors: here is how to easily determine a minimum start value analytically. This will work correctly for any base up to 36, but is kind of pointless below base 10 as brute force is probably faster than doing the calculation.

Note: I would not have figured this out without the above analysis by Hout and Nigel Galloway. Kudos to both of them.

Here is a Perl 6 script showing each step spelled out: [https://tio.run/##fVNNb9owGL7zK56iaARGoqaTWg1UOk277LDTtNNYKwfegKfEzuxkLULpj9p1t/4xZsdJIFRaDsQ47/s@H36ck0qvDwddxljzDS9YGigpC/he/eIaK5nvpph5MdM0xn4A87iPt@4drmQWhxnL/f3s3bXvPYyrcajLLLQdftP3uOUptfVbpjQWiObHYWFu9ig4aZkPqsHA8kq40kWgf5VMEfzPooAnWiKa7TCajPCEm/fzbmf40QwwVQgCfNOkUWwJudSax4ZErVPPhsf6bIcPumDKSkpSVmAUjaYYXZof/wpheG8RX/42goShdoRaik/ON9SeyMSASd2hYDitaw2EM81AnBrtw7/vDw9/Si6M4e1//AdOOzxyHlnFosxiMu5ygdiZcC7U9d3Cj4wwi2dPbvLyp17MsO/T8x46Yj1OqPqsvnDBszIzzUlCisSKLLW9J6qgnufcSZTMIAW9or3uiVo0yWqpW/NkkmiyB5Rx4TsR4UaRYTzp6g2twK0acrniNi5db7taWMy7O1zi4qLdPD9dnpyUXzZ56xQD/r5GetsUVS30EWa8FEvxVU5hEsUsb@vQtBauM5amZLTXseNig98sLQmPPE0hiNZgAvRUKNYOa6ywj8vq96iF/nGi7Pn1V9dYgVKThzMZjiGEbMBcaqG3skzXiMlwWZHWTO3ChkB1cuztobu7UyuwgW8o1EE2ZpqO8E3vEidSIboxFwtX0fxw@Ac Try it online!]


```txt
sub digital-root ($root is copy, :$base) {
    $root = $root.comb.map({:36($_)}).sum.base($base) while $root.chars > 1;
    $root.parse-base($base);
}

sub first-square (Int $n) {
    say '*' x 79;
    say "Base $n -- Uses the possible digits:";
    say my @start = flat '1', '0', (2 ..^ $n)».base($n);

    say "\nDigital root of those digits: ",
    my $root  = digital-root( (^$n)».base($n).join, :base($n) );

    say "\nDigital roots of the first $n numbers in base $n:";
    say my @roots = (1..^$n).map(*²).map: { digital-root($_.base($n), :base($n) ) };

    say "\nMinimum difference of {$n}-digit root from one of the first $n digital roots >= $root:";
    my $offset = min(@roots.grep: * >= $root ) - $root;

    print $offset = $offset > $n ?? 0 !! $offset.base($n);

    if $offset > 0 {
        say "  ({$root+$offset} - $root = $offset)\n\nSo, at a minimum, the smallest starting value will need an extra $offset";
        @start[1+$offset] = $offset ~ @start[1+$offset];
    } else {
        say "\n\nSo no extra digits should be necessary.";
    }

    say "Minimum start value: ", @start.join;

}

.&first-square for 17 .. 21;
```


Which outputs:


```txt
*******************************************************************************
Base 17 -- Uses the possible digits:
[1 0 2 3 4 5 6 7 8 9 A B C D E F G]

Digital root of those digits: 8

Digital roots of the first 17 numbers in base 17:
[1 4 9 16 9 4 1 16 1 4 9 16 9 4 1 16]

Minimum difference of 17-digit root from one of the first 17 digital roots >= 8:
1  (9 - 8 = 1)

So, at a minimum, the smallest starting value will need an extra 1
Minimum start value: 10123456789ABCDEFG
*******************************************************************************
Base 18 -- Uses the possible digits:
[1 0 2 3 4 5 6 7 8 9 A B C D E F G H]

Digital root of those digits: 17

Digital roots of the first 18 numbers in base 18:
[1 4 9 16 8 2 15 13 13 15 2 8 16 9 4 1 17]

Minimum difference of 18-digit root from one of the first 18 digital roots >= 17:
0

So no extra digits should be necessary.
Minimum start value: 1023456789ABCDEFGH
*******************************************************************************
Base 19 -- Uses the possible digits:
[1 0 2 3 4 5 6 7 8 9 A B C D E F G H I]

Digital root of those digits: 9

Digital roots of the first 19 numbers in base 19:
[1 4 9 16 7 18 13 10 9 10 13 18 7 16 9 4 1 18]

Minimum difference of 19-digit root from one of the first 19 digital roots >= 9:
0

So no extra digits should be necessary.
Minimum start value: 1023456789ABCDEFGHI
*******************************************************************************
Base 20 -- Uses the possible digits:
[1 0 2 3 4 5 6 7 8 9 A B C D E F G H I J]

Digital root of those digits: 19

Digital roots of the first 20 numbers in base 20:
[1 4 9 16 6 17 11 7 5 5 7 11 17 6 16 9 4 1 19]

Minimum difference of 20-digit root from one of the first 20 digital roots >= 19:
0

So no extra digits should be necessary.
Minimum start value: 1023456789ABCDEFGHIJ
*******************************************************************************
Base 21 -- Uses the possible digits:
[1 0 2 3 4 5 6 7 8 9 A B C D E F G H I J K]

Digital root of those digits: 10

Digital roots of the first 21 numbers in base 21:
[1 4 9 16 5 16 9 4 1 20 1 4 9 16 5 16 9 4 1 20]

Minimum difference of 21-digit root from one of the first 21 digital roots >= 10:
6  (16 - 10 = 6)

So, at a minimum, the smallest starting value will need an extra 6
Minimum start value: 10234566789ABCDEFGHIJK
```


I've just tried to run a variation of my Go program using this approach up to base 21. However, I'm getting a lower value than your Perl 6 program for base 21 itself even though I'm starting from 10234566789ABCDEFGHIJK as you are, viz:

Base 21: 4C9HE5FE27F² == 1023457DG9HI8J6B6KCEAF
 
compared to your:

Base 21: 4C9HE8175DA² == 1023467JKAIEHB5DF9A8CG

Unless there's something wrong with Go's big.Int routines, both numbers check out as perfect squares when I convert them to decimal so I'm at a loss to explain the difference. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 00:20, 26 May 2019 (UTC)

:Well, I think the simple explanation is that I screwed something up. When I run it again, I get the same thing you got, so I'm not sure where that came from. I must have saved a result from an interim version where I was still working out bugs and never went back and reverified it; which is pretty sad since 21 actually finishes very quickly. At any rate, you are correct, I have the wrong answer for base 21 right now. Fixing. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 02:32, 26 May 2019 (UTC)

I've run it through to base 25 now and found another anomaly at that point where, although we agree on N², we disagree on N where I have 1011E145FHGHM but you have 1011E145FHGI3.

This has certainly been an interesting task which has taught me a lot so thankyou for thinking it up in the first place.

I don't know whether any substantial optimizations are still possible, though I'm continuing to think about it as it would be nice to get the run time down for the higher bases. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 15:42, 26 May 2019 (UTC)

:Argh. Made faulty assumptions about how square root of big integers are handled in Perl 6. It was only in the display code, not the calculations, so I got the right ''square'' but then displayed an incorrect square root. Will fix that. Thanks. 

:This turned out to be a more interesting task than I initially anticipated. I learned quite a bit more about number theory than I knew before. We have an interesting cross-section of talented people on here. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 16:41, 26 May 2019 (UTC)

==Calculating quadratic residues==
The valid digital roots can be calculated using the following code in F#:

```Fsharp

  let rSet g=set[for n in [0..g-1] do yield n*n%g]

```

rSet 20 -> set [0; 1; 4; 5; 9; 16]. Remember that this is base 20 and 0 corresponds to 20.
Similar code in Python is:

```Python

  rSet=lambda g: {n*n%g for n in range(1,g)}

```

rSet(20) -> {0, 1, 4, 5, 9, 16}
--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 17:02, 25 May 2019 (UTC)

==Trailing zero==
A minor optimization has occurred to me namely that N cannot end with '0', for any base greater than 2, because if it did, then N² would end with '00'.

However, this is impossible because:

* if N² has 'base' digits, it can't then be pandigital; or
* if N² has 'base+1' digits then the repeated digit would be zero and so the digital root would be unchanged. 


As checking for this is very cheap, it may knock about 4 or 5% from the run time for bases >= 20.
--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 16:27, 27 May 2019 (UTC)
==Optimization when no extra digit required==
Let me do this in base 10 to maintain sanity. As discussed (Space compression and proof) the Digital Root n Base10 for n from 1 to 20 gives 1 4 9 7 7 9 4 1 9 1 4 9 7 7 9 4 1 9 1 4. Digital Root 1023456789 Base10 is 9. So every third square is a possible solution. As discussed (analytically determine minimum start value) start from 31992**2. First find the first value whose Digital Root Base10 is 9. In this case it is 31992. It should then only be necessary to try every third value to find the solution.

```txt
 
> [31992..3..999999] |> List.map(fun n->n*n) |> List.find(fun n->n=1026753849);;  
Real: 00:00:00.068, CPU: 00:00:00.110, GC gen0: 5, gen1: 1
val it : int = 1026753849

> [31992..999999] |> List.map(fun n->n*n) |> List.find(fun n->n=1026753849);;   
Real: 00:00:00.204, CPU: 00:00:00.300, GC gen0: 14, gen1: 2
val it : int = 1026753849

```

which seems to save the expected two thirds time. This is a little trickier when an extra digit is required because you would have to check each extra digit separately (they would have different Digital Roots).
The sequence 1 4 9 7 7 9 4 1 9 1 4 9 7 7 9 4 1 9 1 4 can be produced as (n*n)%9 -> 1 4 0 7 7 0 4 1 0 1 4 0 7 7 0 4 1 0 1 4 for n = 1 to 10 with the usual 0 means 9.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 22:03, 27 May 2019 (UTC)
: //after watering the garden 

the digital root of the square of start value n ( pandigital:all digits of [0..Base-1] ) is for even numbers always Base-1.<BR>
base 10: 0123456789 -> 09 18 27 36 45 -> 0 -> 9  <BR>
For odd numbers Base % 2 ( casting out 9 aka Base-1 the Base % 2 is left over)<BR>
base 9: 012345678 -> 08 17 26 35 4 -> 4 <BR>
Now you only use such n , that there dgt-root**2 is the dgt-root of a pandigital number. Aha!

for base 9:  dgt root of the square [0..8] = 0  1  4  1  0  1  4  1  0 .. 1  4  1  0 

So you only use numbers with digit root  2 and 6


Now base 15: dgt-root of pandigital value ist 7

dgt root of the square:  0  1  4  9  2 11  8  7  8 11  2  9  4  1 .. 0

So one has only to check every 14.th value.<b>Bravo</b>!


:Simple but brilliant, Nigel :) 

:Even on my old machine, this has cut the time needed (in Go) to reach base 25 from 60 to under 4 minutes and bases 26 and 27 are now dispatched in under 16 and 30 minutes respectively.  

:I wouldn't spend too much time on optimizing for the cases where an additional digit is required as they take very little time to process anyway as a result of previous optimizations. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 10:58, 29 May 2019 (UTC)
::@PureFox your calculation of digital root is to much work [[wp:Digital_root#Congruence_formula|Digital_root]] is only one ( n MOD (Base-1) ) calculation.All numbers are >> 1 no need to check for one digit.Similary to this
```go
func sumDigits(n, base *big.Int) *big.Int {
    r := new(big.Int)
//  decrement base by one ?
    r := Rem(n,base-1)
    return r
}
```


:::Thanks for pointing out that I could use the congruence formula to simplify (and possibly quicken up) the calculation of the digital root which is used to help establish the optimum starting value for each base. Unfortunately, I haven't been able to use it as I wasn't able to obtain consistently faster times than before. Although it was marginally faster up to base 25, it then started to diverge and by the time base 28 was reached it was 48 seconds slower than before. This may be due to the vagaries of big.Int arithmetic in Go but thanks anyway for continuing to think about this task which has been a real team effort :) --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 19:50, 2 June 2019 (UTC)

==Minimum start for Pascal version==
I spotted a bug in the output of the extended Pascal version, for the value of base 17.  The values should be 

```txt
423F82GA9             101246A89CGFB357ED
```
 rather than 
```txt
4261CBG65             102369EB54FD9G7CA8

```
Looking into the code, it seems that although it correctly identifies which bases need an extra digit, the current Pascal code always adds a zero to the end.  That is, it multiplies the minimum start value by the base.  What other implementations do is to identify what digit should be added, and the location in the minimum start value where that digit should be added.  I've made a  [https://tio.run/##3Vl7b9tGEv@fn2KQK2DSoR6UXLeVYgO2Zbe6JlIuVuPeBYFBkStpbYqUuUu7TpGvXnf2RXIl2XHuUKA4wbbI3ZnZef52dp1Nr0jEG6uQRWHSmK2ih4dVns3zcAn4LeaCvtNqndE0Br4gwJZhkhDGIS2WU5JDCjyDacgITH1gGdKEOLebAk2jpIgJA6RHATGdU84gm2liCK9DWIWpHA8T5/dvhmeD0zM4e3vy2QH4/Zs348EpDE5fv/1pqAbGK06Xy08hp1kK49FnZDkdDYZnn52CEYYk7J4VnCbMx@f5ctVvtYRuaFZUJCFH1XmYc7gNkwLJnShLGUfKaBHm54RDD8I8D@8/tJvN/c5HoaiYgYOddtDp7n27/933Pxwdn6CKP/40/OfPr9@Mxm//9e588sv7i1///Z9wGsVkNl/Qq@tkmWarm5zx4vbut/tPO33H4fcrgivxUbHk2bEw/wByEmV5jKNrn5RPL@O5rU63sScVmt5z0t/OEqXc3z6D7gaUdoGrbfCSVI7xwZy/yzJ@fpM/oRnEObt8TdlX6SaZtApPkZxnyTNITlLee4pkREjMxil5k@VkIBILJWZZQsJ0k0Pa7tyGOebR0evhjyPodkSiYZCEJ9lN3jkWDzFJeKgH5bPQswplf41duFI6UlBVfl0nm7T9SdCDyQATc0KXKMbBcotIXOQExoWQ78oMFXXWq5bz@lJlAIryR1gLt2SY8r5zTOY0xeE7yheCBeIM38wowFmWC5YD6LYbOl8aARLdpVgibUUtPnc55cTdgR2vb/N9mUkX0gedwR/ox49SiE6yumTl@1mRRrKY50R66nGjezVLt5o//YL5OWFFwoUd7TW72gIjkKNZN1DbhRDmKk6/Mko7ppKonxCvXF1vjaAyXJlaxfYE4ahKC1eAYWXds6JbZpjUc1ra2GopOEU0BY2qkOMy4DJ0CKIw2BjswTKLQWqACjv1YkWrpjK3pRdmMI5jV7HgFpA6dtlqYpR@Cx05RxJGNonQ83JQEuP7sVy4r3UXMA13CxotQMCfVFzuNzdFiG5DncWb3nTuKFozRZxaki3RVBZVYTS4hcETNC7dpZ4IV83GVouRMI8WBU3nwmCWJYVITccGn0cTaH3J4cxa9aD0Q81/Kr8q4Z5RBddP0fpZVqg9N4UNXHMeRTyhUk1hrW@VilUmYqXdXhb0En1aT8NfKGZcQXvie3@vj/koIPErIMj2zILOF27F3NSF5BlfjXTpqeIqHWwXcpXkZkezMlSXbi066MKCglShcvjpr8OJnq6R5mRFQu5Yu6/WBEW8wRKpFgIxpCYGw/fWhIgl1SEsUmxDtAIbGtKn4tEZbQnGZgjkEOstV58u@d8vHM/0rtQeQePyBrPQZT7zpyq7HnOoYIiWK0UNbW/DvZverbn3XHR/70Xzd5KjSsRys8JeRCE5JaEmye5Em1u1qAZ8XgRt0RA2m02FYPACGWWbKZoGDi8DQB4ZToKvKaTkDos5X4a8qUPFbi@RVLYZtz3lCKH3dQ901Tll@LbsF54EUsFFU8pddiu9VHsXwm2iS0bwl@o5X@E/NshoywskG54pVD6EjimYau@8FsHqbAmnEL4sEhkPLdeWbyI5A/cag9XxpGOkDfgafKfezX7W3I5nFmI@d0lBF8bxNrprK8OurQy7Plzb@J6vG9UHm1eHaJhC7ecoq7JUKSyyBykNVRnV0hAff4JyGEXX5PoyDRy59ShF9rtGv6peLYzxRVers2d9SrW/NUXqikbYUefumpZm0NssvWEaoeBjOncFbqE5QR3LUr3VVEAmoMCP8Ixxvx3SpiIjhZimxp@@U0cdzWmga0ETbBwwt9vrXSGz5Chkeim5XyKD7BC0b4zEcR677PBgqn0mBbAGuA1FcTRCFj23JldSarSS/bTsl7T8CuuMm5XWSuqrr1L9r9B4Uz9EjJJcbGyv0P9lNVgzCpS3p8TWfJCKWT0xdrY5yclNQRmeIZhxi8zzes78V9nyt/b0/@7ooziuO9rHPx27mWu1xAS8Ula@FASbLheuZiA6cdmUh0siJAWHB4LcBbG@55exkBZ/ZUTsqrW7FrHI@gFt@nSESpa/NmKmHqjWoVRzPbVwv/t/q@EJ9kePHWDlTYk8MZUtjUiMK18wqeu2bedbeUcgRXalqpO2xMrsTrxs7eAkmbmjkcTFUgzpvC9vb8yDpNfoU04GlRR9ygt07alm5bALnnSzazUE@ox1KHrRjbZJ35nOaF5dlqrzbRTKKw9VSfWDpuyyMXA5Z@Z4L4dlGpXnyfptCcr1PgrYktMorOKqNJEHUmVxWhqrPtpLare3XGSxlY7qlHMmiWy3uaol9uTxaIuvzBH3ahc1qbjt0NWqvyOqv1ygtG09uGmp9JNhh1onYlOg9F3zUEff9TOy39ndoFcSvPodFxcnB4Lt/z3s@Jbcat@ptdJ47sCzPoeCkVhf0sjxslasW4crWdQiYs2nLuVqzOZx/A7cANgiqbELnLgyl1oCIzSxcdiZYZHFdmBEWW358bvTo59NaFPyGze3NirtnS9l2tageTZs4RFLqUNVIYLSqrMHDQi88qhvYkD9fwQKQlRnPwsT1dZXGSBqgdpRnwQ1uLkQgpLUdSdBg7fB2/1@f6/d7v3Q6/o7wNQFqb6pNaboV2lklRBJarBTYePEuvc1F2LplttMrcGOpNi43DcPytlu6kEVecwKpeGkZpHIHnP7Jiur863JlxLL62q7LjI2JsbyoK1MJ1gVsXaA@bfNxXA0GF@cf0ZgjtHe8l8zaHfz4eGPaJaEc/bQGHf/BA Try It Online! link ] with a kludgy fix.  It is hard-coded to stop at base 25, as one only gets 60 seconds to execute there.--[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 20:06, 6 June 2019 (UTC)
:thanks for the "fix"

```pascal
    if DgtRtSqr.drs_NeedsOneMoreDigit then
      if base <> 17 then mpz_mul_ui(sv_sqr, sv_sqr, base);
```

:I try to update the procedure StartValueCreate.
:I first had to change the TestSet .Even 64 bit-freepascal 1 shl x is done in 32 Bit, so i got no solution running 1e12 Tests for base=32 upwards.Nggggrrrrrr ( Rumpelstiltskin , i have to put me togehther )
::Looking at the revised Pascal code, it seems that the inserted digits are not being calculated correctly for some bases (such as base 17 and base 29).  Here is some commented demo code written in Pascal that demonstrates a way to generate the correct information. (and a [https://tio.run/##hVRbdxo3EH7nV8xDzgGaNQbHcVsoPif1JaF1wLWT3nz8IFYCFGuljaQF49S/3R2NdmGJc9oXexnNfPPNN5ecuZSpvVmePj3t7wMXmXEgtRPWCw5czqV34A1kUsusyGDJVCE6nU4jt2ZuWQb4/5NIfW/Q@PJidH56dg7nlyePDYAvL95PTs/g9Ozi8t0oGia5l5l8YF4aDZNxNJ6g15uL0dtxgEqHh4kyJh8ePiLe2fh0dP7YKJxw6Hq9dh@9VG7QSI12Hi0Zu@/DGOGWYqQ9DOEIaQCkC2ZdH5y3Us/R2uz2Dl4dvj76/ocf3/x8ghTfvhv98uvF@/Hk8rer6w8ff//jz7/@ZtOUi9l8IT/dqUyb/LN1vliu7tcP/zQHjSWzCDxlTtQSDhpoA0DZrPCF1SjUQkTNmAJrjAczA51A4QKRtLBWIM2AQoGzQqckBb9q6RpueydH8JyKudT0BZjLFcpDfwjdAUjExwq7IbMGcS/LgLqbBmwEtEJa2INem6LK503o1r3yi0BC8@dlskpbv2AepyVVBRex@t3R@f/SZ1Kpkf66/Aj/X7Vfk8eVyBVLRevE5OsWtT2BXkJp2gkg1geDji1NdT8zwMuaBZ9vbtvP1SPUm4NbdI6fvfBJCaNTAq/KjDXFoJwYx20fmLVsfdPtdI4wFidi21xSFX2QjvtcMLs7PVjMUQ9bhGOOUBKzcJsA4/XxCABTAqC@1cODK9oZ51U7GpWWGGS0WmMjZ2IFhnOKdpAVzsOCLUW1@TEY9z/0ducGlOsQJHo9gNVCKhENPxFh4KbWOcwnljhlMQs3uulBC0SukcNqd4oPSm2ZBW2YWrG1Q0G2swz7cBCnBDXYrMSEc3Jpx9ku3yjILWw12kgqY3cIC0o6WtWv19dVxpnEa0BaxTa5OMTGggzIvaBQyIJFYzdvZOB2G15wsyV8B7K9SYkaesmUfBCQG@fkFHWrqUCgldB4mdNC0blMYinpQqR3dZ/IBBtdFjjYsupWrJBMYIbCELlbOB6SnRDRitHH1RPZIly0bIgHHcq1kHHXXcaUEigMDQTMrWBeBFim6f0bE4msixiMBAj5VKQtxmm2tyKNZvV0@N4OOR@ENaUOz09NcCi0Fqlwjtk1IVW1lTduZaUXStNo9A8SaPahSfsUvmHvOPwq7xFm3LJxdzLfFOTi0Vt9e4YpZKRTypHAIYHQRcA/naenfwE Try It Online! link])--[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 19:36, 7 June 2019 (UTC)

```pascal
// demos inserted digits to minimum value...
program project1;
{$IFDEF FPC}
  {$MODE DELPHI}
  {$Optimization ON}
  {$CODEALIGN proc=4,loop=4}
{$ENDIF}
uses
  SysUtils;
const
  max: NativeInt = 61;
  chars: string = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz|';
var
  base: NativeInt;

    // returns the digital root of n, using current base
    function dR(n: NativeInt): NativeInt;
    begin
      result := 0; if n = 0 then exit;
      result := n MOD (base - 1); if result = 0 then result := base - 1;
    end;

    // returns a string that includes the inserted digit, using current base
    function fillIn(n: NativeInt): string;
    begin
      result := StringReplace(Copy(chars, 1, base), IntToStr(n - 1), IntToStr(n - 1) + IntToStr(n), []);
      result := chars[2] + chars[1] + Copy(result, 3, base);
    end;
 
var
  sdr: array[0..61] of NativeInt;  // sdr - square digital roots, 61 = max
  i, bdr, ad: NativeInt; // bdr - base digital root, ad - added digit
begin
  // only a few odd bases must have digits added to the minimum value
  base := 5; while base <= max do begin
    // even bases don't need added digits, digital roots of odd bases are always = (base - 1) / 2
    bdr := 0; if Odd(base) then bdr := base shr 1;
    // make a list of the digital roots of the first few squares
    for i := 1 to bdr do sdr[i - 1] := dR(i * i);
    // initialize possible added digit for minimum calculation, then check for minimums
    ad := base; for i := 0 to bdr - 1 do if sdr[i] >= bdr then if ad > sdr[i] then ad := sdr[i];
    // the result is the smallest value greater than the base digital root, minus the bdr
    Dec(ad, bdr);
    // If the result (ad) is zero, then the inserted digit is unnecessary
    if ad > 0 then writeln(base:2, ': ', ad:2, ' -> ', fillIn(ad));
    // skip the bases that won't need added digits
    Inc(base, 4);
  end;
end.
```

{{out}}

```txt
 5:  2 -> 102234
13:  3 -> 10233456789ABC
17:  1 -> 10123456789ABCDEFG
21:  6 -> 10234566789ABCDEFGHIJK
29:  2 -> 10223456789ABCDEFGHIJKLMNOPQRS
37:  7 -> 10234567789ABCDEFGHIJKLMNOPQRSTUVWXYZa
45:  3 -> 10233456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghi
49:  1 -> 10123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklm
53:  3 -> 10233456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopq
61:  6 -> 10234566789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxy
```

::: try to fix it, when the still running even numbers finished. I don't think to let 37 start.
:::: I've been trying to verify the larger results produced by the Pascal program.  I have verified the results as proper ''root,square'' pairs, except for 29 and 34.<br/><br/>On base 29, the figures verify as pandigital, but the value seems like it could be lower.  It seems to me that the square could start with 102234... or higher, and not 102345677... or higher.<br/><br/>I was unable to verify pandigital results:
```txt
BB6GLLFX5V75RA3RRL   102345679JICE8KP5LXA8L3QUPUWFPE4P
```
 for Base 34, as there are a number of duplicated digits in the square.  But by considering the ''testcount'' value, I came up with the following result: 
```txt
5SEMXRII42NG8AKSL   102345679JIESRPA8BLCVKDNMHUFTGOQWX
```
which has only one of every digit.  Or, the actual minimum square for base 34 is a lower number.  Perhaps a "copy paste" error caused this discrepancy on base 34?--[[User:Enter your username|Enter your username]] ([[User talk:Enter your username|talk]]) 23:59, 9 June 2019 (UTC)
::::: Still a work in progress.base 34 was a copy and paste failure
```txt
Testcount : 205094427126  5SEMXRII42NG8AKSL 102345679JIESRPA8BLCVKDNMHUFTGOQWX 28900.032 seconds
```
 i have changed the program and checked, if the last digit of the number squared leads to the last digit of the squared number, and it does so 
```pascal
    //check last digit sqr(num) mod base must be last digit of sqrnumber
    if (sqr(Num.ntb_dgt[0]) mod Num.ntb_bas) <> sqr2B.ntb_dgt[0] then
```
i stopped Base 36 at 
```txt
3,044,863 Mio tests about 1.1 e12 / day
```
 i will try to use multithreading. Instead of checking every 35 n-threads check every n*35, but using different start values.Tread 0 at normal , thread 1 at normal + 35 .. thread n-1 at normal + (n-1)*35
==Finding maximal distances to check if number needs extra digit==

```txt
insert 2
Base 29 test every 1
Start  : Num 5BAEFC5QHESPCLA  sqr 10223456789ABCDKM4JI4S470KCSHD
Result : Num 5BAEFC62RGS0KJF  sqr 102234586REOSIGJD9PCF7HBLKANQM
 4921.784 s Testcount : 92238034003

```

The runtime is really slow by checking every number.I try to find the distances.  
I checked all solutions for the possibly inserted digits.This makes only sense, if the inserted digit ist "small" , so that one will find a solution, before this inserted digit is reached by sqr(num) 

```txt

 Base 17 -> base-1 = 16 -> (base-1) / 2 = 8
insert 1 this is the relevant digit -> smallest startvalue.
dgt/count: digital roots of 3,5,11,13 lead to possible solution
   1  4 :  3  5 11 13 //distances = +2 = diff1 =(8-2*k) +6= (8-diff1) 
         0+3,8-3,8+3,16-3, 16+3,24-3,    n*((Base-1)/2) +/- k (= first found value ) k = 3
   8  4 :  4  8 12 16 //distances = +4 +4 +4 +4  
         0+4,8-4,8+4,16-4, 16+4,24-4,   
   9  4 :  1  7  9 15 //distances = +6 +2 +6 +2
         0+1,8-1,8+1,16-1, 16+1,24-1,   

 Base 21 -> base-1 = 20  -> 10
insert 6
   6  4 :  4  6 14 16 // +2 +8 +2 +8  
Ed:       0+4,10-4,10+4,20-4,  20+4...
 Base 29 -> base-1 = 28 -> 14
insert 2
   2  4 :  4 10 18 24 // +6 +8 +6 +8
         0+4,14-4,14+4,28-4,  28+4

```

Now one sees that only few different distances need to be calculated.In the case of base 29 only 4 out of 28  = 1/7 => runtime about 700 s.Or for a 4-core CPU test every 28 from the different start values.
For base 49 one needs 8 Core to check every 48 ;-)

```txt
 Base 49  48-> 24 ->12 -> 6 ( 5+1 = 6 ? )
insert 1
   1  8 :  5 11 13 19 29 35 37 43 //+6+2+6+10+6+2+6+10
```

          0+5,12-1(= 6-5),12+1,24-5,24+5,36-1,36+1,48-5
