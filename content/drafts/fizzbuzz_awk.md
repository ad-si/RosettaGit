+++
title = "FizzBuzz/AWK"
description = ""
date = 2015-09-20T10:43:16Z
aliases = []
[extra]
id = 18255
[taxonomies]
categories = []
tags = []
+++

{{collection|FizzBuzz}}

==regular if / else==
<!-- http://ideone.com/UrHdvd -->
This is the "traditional" approach:  
Loop, and modulo-check to see what to print. 

Minor tweak: printf with no newline, and linebreaks only after each "FizzBuzz", 
to get a more compact output.

```AWK
# usage: awk  -v n=38  -f FizzBuzz.awk
#
BEGIN {
   if(!n) n=100
   print "# FizzBuzz:"

   for (ii=1; ii<=n; ii++)
       if (ii % 15 == 0)
           {print "FizzBuzz"}
       else if (ii % 3 == 0)
           {printf "Fizz "}
       else if (ii % 5 == 0)
           {printf "Buzz "}
       else
           {printf "%3d ", ii}

    print "\n# Done."
}
```

{{out}}

```txt

# FizzBuzz:
  1   2 Fizz   4 Buzz Fizz   7   8 Fizz Buzz  11 Fizz  13  14 FizzBuzz
 16  17 Fizz  19 Buzz Fizz  22  23 Fizz Buzz  26 Fizz  28  29 FizzBuzz
 31  32 Fizz  34 Buzz Fizz  37  38 Fizz Buzz  41 Fizz  43  44 FizzBuzz
 46  47 Fizz  49 Buzz Fizz  52  53 Fizz Buzz  56 Fizz  58  59 FizzBuzz
 61  62 Fizz  64 Buzz Fizz  67  68 Fizz Buzz  71 Fizz  73  74 FizzBuzz
 76  77 Fizz  79 Buzz Fizz  82  83 Fizz Buzz  86 Fizz  88  89 FizzBuzz
 91  92 Fizz  94 Buzz Fizz  97  98 Fizz Buzz 
# Done.

```

When the output is presented like that, it is easy to see a pattern.

==bash with echo==
<!-- http://ideone.com/0VMIuO -->
Using echo from the shell to generate the numbers as input.

Advantage: we need no loop inside the script.

Disadvantage: this needs a shell where echo can do this.

```AWK
echo {1..100} | awk '
BEGIN {RS=" "} 
$1 % 15 == 0 {print "FizzBuzz"; next}
$1 %  5 == 0 {printf "Buzz ";   next}
$1 %  3 == 0 {printf "Fizz ";   next}
{printf "%3d ",$1}
'
```


==One-liner with seq==

Like version 2, using bash with seq to generate the numbers as input. 

Disadvantage: needs external command seq, i.e. this only works on unix. 
(Also, hard to read)


```AWK
seq 100 | awk '$0=NR%15?NR%5?NR%3?$0:"Fizz":"Buzz":"FizzBuzz"'
```


==No divisions, using counters==
<!-- http://ideone.com/uHmYUr -->
Division is one of the more expensive operations,
so it is nice if we can avoid it.

All processing is done inside awk, using no division & no modulo. 

Instead, a simple counter for each of the output-variants is used:

```AWK
# usage: awk -v n=38  -f fizzbuzzNoDiv.awk
#
# FizzBuzz using no division & no modulo-operations:
BEGIN {
    if(!n) n=100
    print "# FizzBuzz:"
    while (c1<n) {
	  c1++; c3++; c5++; cF++; x=sprintf("%3d ",c1)
	  if(c3>= 3) { c3=0; x="Fizz " }
          if(c5>= 5) { c5=0; x="Buzz " }
	  if(cF>=15) { cF=0; x="FizzBuzz\n" }
	  printf(x)
    }
    print "\n# Done."
}
```

Same output as version 1.

==No divisions, using pattern-string==
<!--  http://ideone.com/HJsrvl -->
Another solution that works without division / modulo.
{{works with|gawk|4.1.0}} {{works with|mawk|1.3.3}}
This is inspired by the versions "Without Modulus" of Nimrod and Python, 

using a precomputed (observed:) pattern to decide how to print each number. 

But here, the pattern is represented as chars in a string, 
instead of bits in an integer.

```AWK
# usage: awk  -v n=42  -f fizzbuzzRepeatPattern.awk
#
function prt(x,v) {
    if(v==0) {printf("%3d ",x); return}		# print number
    printf fb[v]				# else: print text
}
BEGIN {
    if(!n) n=100
    print "# FizzBuzz:"

    pattern="003053003503006"			# 0: print number, 3: print Fizz, etc.
    split("1,2, Fizz,4, Buzz, FizzBuzz\n,", fb, ",")

    while (i<n) {
	  i++; sel++;  
	  prt(i, substr(pattern,sel,1) ); 	# select variant to use from the pattern
          if( sel>=length(pattern) ) sel=0
    }
    print "\n# Done."
}
```

Same output as version 1.

==Custom FizzBuzz==

### Example program 1

generated from [[General_FizzBuzz#AWK]],
for factors 2, 3, 5 using the words A, B, C. 

The list of numbers is also generated (much like in version 2 and 3), 
and comes in as the file numbers.txt.

;Input:

```txt
31
2 A
3 B
5 C
```


<!-- http://ideone.com/yw1oEK -->

```AWK
# usage: awk  -f fizzbuzzCustom.awk  numbers.txt
#
BEGIN {print "# CustomFizzBuzz:"}

$1 %  2 == 0 {x = x "A"}
$1 %  3 == 0 {x = x "B"}
$1 %  5 == 0 {x = x "C"}

!x    {print $1; next}
      {print " ",x; x=""}

END   {print "# Done."} 
```


{{out}}

```txt
# CustomFizzBuzz:
1
  A
  B
  A
  C
  AB
7
  A
  B
  AC
11
  AB
13
  A
  BC
  A
17
  AB
19
  AC
  B
  A
23
  AB
  C
  A
  B
  A
29
  ABC
31
# Done.
```



### Example program 2

;Input: for the generator-program at [[General_FizzBuzz#AWK]]:

```txt
105
3 Fizz
5 Buzz
7 Baxx
```



```AWK
BEGIN {print "# CustomFizzBuzz:"} 

$1 %  3 == 0 {x = x "Fizz"}
$1 %  5 == 0 {x = x "Buzz"}
$1 %  7 == 0 {x = x "Baxx"}

!x  {print $1; next}
    {print " ", x; x=""}
 
END {print "# Done."}
```

