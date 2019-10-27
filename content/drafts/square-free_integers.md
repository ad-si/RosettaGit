+++
title = "Square-free integers"
description = ""
date = 2019-10-20T19:25:05Z
aliases = []
[extra]
id = 21893
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Write a function to test if a number is   ''square-free''.


A   ''square-free''   is an integer which is divisible by no perfect square other
than   '''1'''   (unity).

For this task, only positive square-free numbers will be used.



Show here (on this page) all square-free integers (in a horizontal format) that are between:
::*   '''1'''            ───►   '''145'''                        (inclusive)
::*   '''1''' trillion   ───►   '''1''' trillion + '''145'''     (inclusive)


(One trillion = 1,000,000,000,000)


Show here (on this page) the count of square-free integers from:
::*   '''1'''            ───►   one hundred                      (inclusive)
::*   '''1'''            ───►   one thousand                     (inclusive)
::*   '''1'''            ───►   ten thousand                     (inclusive)
::*   '''1'''            ───►   one hundred thousand             (inclusive)
::*   '''1'''            ───►   one million                      (inclusive)


;See also:
:*   the Wikipedia entry:   [https://wikipedia.org/wiki/Square-free_integer square-free integer]





## ALGOL 68


```algol68
BEGIN
    # count/show some square free numbers                                           #
    # a number is square free if not divisible by any square and so not divisible   #
    # by any squared prime                                                          #
    # to satisfy the task we need to know the primes up to root 1 000 000 000 145   #
    # and the square free numbers up to 1 000 000                                   #
    # sieve the primes                                                              #
    LONG INT one trillion = LENG 1 000 000 * LENG 1 000 000;
    INT prime max = ENTIER SHORTEN long sqrt( one trillion + 145 ) + 1;
    [ prime max ]BOOL prime; FOR i TO UPB prime DO prime[ i ] := TRUE OD;
    FOR s FROM 2 TO ENTIER sqrt( prime max ) DO
        IF prime[ s ] THEN
            FOR p FROM s * s BY s TO prime max DO prime[ p ] := FALSE OD
        FI
    OD;
    # sieve the square free integers                                                #
    INT sf max = 1 000 000;
    [ sf max ]BOOL square free;FOR i TO UPB square free DO square free[ i ] := TRUE OD;
    FOR s FROM 2 TO ENTIER sqrt( sf max ) DO
        IF prime[ s ] THEN
            INT q = s * s;
            FOR p FROM q BY q TO sf max DO square free[ p ] := FALSE OD
        FI
    OD;
    # returns TRUE if n is square free, FALSE otherwise                             #
    PROC is square free = ( LONG INT n )BOOL:
         IF n <= sf max THEN square free[ SHORTEN n ]
         ELSE
            # n is larger than the sieve - use trial division                       #
            INT max factor    = ENTIER SHORTEN long sqrt( n ) + 1;
            BOOL square free := TRUE;
            FOR f FROM 2 TO max factor WHILE square free DO
                IF prime[ f ] THEN
                    # have a prime                                                  #
                    square free := ( n MOD ( LENG f * LENG f ) /= 0 )
                FI
            OD;
            square free
         FI # is square free # ;
    # returns the count of square free numbers between m and n (inclusive)          #
    PROC count square free = ( INT m, n )INT:
         BEGIN
            INT count := 0;
            FOR i FROM m TO n DO IF square free[ i ] THEN count +:= 1 FI OD;
            count
         END # count square free # ;

    # task requirements                                                             #
    # show square free numbers from 1 -> 145                                        #
    print( ( "Square free numbers from 1 to 145", newline ) );
    INT    count := 0;
    FOR i TO 145 DO
        IF is square free( i ) THEN
            print( ( whole( i, -4 ) ) );
            count +:= 1;
            IF count MOD 20 = 0 THEN print( ( newline ) ) FI
        FI
    OD;
    print( ( newline ) );
    # show square free numbers from 1 trillion -> one trillion + 145                #
    print( ( "Square free numbers from 1 000 000 000 000 to 1 000 000 000 145", newline ) );
    count := 0;
    FOR i FROM 0 TO 145 DO
        IF is square free( one trillion + i ) THEN
            print( ( whole( one trillion + i, -14 ) ) );
            count +:= 1;
            IF count MOD 5 = 0 THEN print( ( newline ) ) FI
        FI
    OD;
    print( ( newline ) );
    # show counts of square free numbers                                            #
    INT sf       100 :=              count square free(       1,       100 );
    print( ( "square free numbers between 1 and       100: ", whole( sf       100, -6 ), newline ) );
    INT sf     1 000 := sf     100 + count square free(     101,     1 000 );
    print( ( "square free numbers between 1 and     1 000: ", whole( sf     1 000, -6 ), newline ) );
    INT sf    10 000 := sf   1 000 + count square free(   1 001,    10 000 );
    print( ( "square free numbers between 1 and    10 000: ", whole( sf    10 000, -6 ), newline ) );
    INT sf   100 000 := sf  10 000 + count square free(  10 001,   100 000 );
    print( ( "square free numbers between 1 and   100 000: ", whole( sf   100 000, -6 ), newline ) );
    INT sf 1 000 000 := sf 100 000 + count square free( 100 001, 1 000 000 );
    print( ( "square free numbers between 1 and 1 000 000: ", whole( sf 1 000 000, -6 ), newline ) )
END
```

{{out}}

```txt
Square free numbers from 1 to 145
   1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31
  33  34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65
  66  67  69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95
  97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
 131 133 134 137 138 139 141 142 143 145
Square free numbers from 1 000 000 000 000 to 1 000 000 000 145
 1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
 1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
 1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
 1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
 1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
 1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
 1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
 1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
 1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
 1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
 1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
 1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
 1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
 1000000000139 1000000000141 1000000000142 1000000000145
square free numbers between 1 and       100:     61
square free numbers between 1 and     1 000:    608
square free numbers between 1 and    10 000:   6083
square free numbers between 1 and   100 000:  60794
square free numbers between 1 and 1 000 000: 607926
```


## AWK


```AWK

# syntax: GAWK -f SQUARE-FREE_INTEGERS.AWK
# converted from LUA
BEGIN {
    main(1,145,1)
    main(1000000000000,1000000000145,1)
    main(1,100,0)
    main(1,1000,0)
    main(1,10000,0)
    main(1,100000,0)
    main(1,1000000,0)
    exit(0)
}
function main(lo,hi,show_values,  count,i,leng) {
    printf("%d-%d: ",lo,hi)
    leng = length(lo) + length(hi) + 3
    for (i=lo; i<=hi; i++) {
      if (square_free(i)) {
        count++
        if (show_values) {
          if (leng > 110) {
            printf("\n")
            leng = 0
          }
          printf("%d ",i)
          leng += length(i) + 1
        }
      }
    }
    printf("count=%d\n\n",count)
}
function square_free(n,  root) {
    for (root=2; root<=sqrt(n); root++) {
      if (n % (root * root) == 0) {
        return(0)
      }
    }
    return(1)
}

```

{{out}}

```txt

1-145: 1 2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30 31 33 34 35 37 38 39 41 42 43 46 47 51 53 55 57 58 59
61 62 65 66 67 69 70 71 73 74 77 78 79 82 83 85 86 87 89 91 93 94 95 97 101 102 103 105 106 107 109 110 111 113
114 115 118 119 122 123 127 129 130 131 133 134 137 138 139 141 142 143 145 count=90

1000000000000-1000000000145: 1000000000001 1000000000002 1000000000003 1000000000005 1000000000006 1000000000007
1000000000009 1000000000011 1000000000013 1000000000014 1000000000015 1000000000018 1000000000019 1000000000021
1000000000022 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031 1000000000033 1000000000037
1000000000038 1000000000039 1000000000041 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
1000000000049 1000000000051 1000000000054 1000000000055 1000000000057 1000000000058 1000000000059 1000000000061
1000000000063 1000000000065 1000000000066 1000000000067 1000000000069 1000000000070 1000000000073 1000000000074
1000000000077 1000000000078 1000000000079 1000000000081 1000000000082 1000000000085 1000000000086 1000000000087
1000000000090 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097 1000000000099 1000000000101
1000000000102 1000000000103 1000000000105 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
1000000000115 1000000000117 1000000000118 1000000000119 1000000000121 1000000000122 1000000000123 1000000000126
1000000000127 1000000000129 1000000000130 1000000000133 1000000000135 1000000000137 1000000000138 1000000000139
1000000000141 1000000000142 1000000000145 count=89

1-100: count=61

1-1000: count=608

1-10000: count=6083

1-100000: count=60794

1-1000000: count=607926

```



## C

{{trans|Go}}

```c>#include <stdio.h

#include <stdlib.h>
#include <math.h>

#define TRUE 1
#define FALSE 0
#define TRILLION 1000000000000

typedef unsigned char bool;
typedef unsigned long long uint64;

void sieve(uint64 limit, uint64 *primes, uint64 *length) {
    uint64 i, count, p, p2;
    bool *c = calloc(limit + 1, sizeof(bool));  /* composite = TRUE */
    primes[0] = 2;
    count  = 1;
    /* no need to process even numbers > 2 */
    p = 3;
    for (;;) {
        p2 = p * p;
        if (p2 > limit) break;
        for (i = p2; i <= limit; i += 2 * p) c[i] = TRUE;
        for (;;) {
            p += 2;
            if (!c[p]) break;
        }
    }
    for (i = 3; i <= limit; i += 2) {
        if (!c[i]) primes[count++] = i;
    }
    *length = count;
    free(c);
}

void squareFree(uint64 from, uint64 to, uint64 *results, uint64 *len) {    
    uint64 i, j, p, p2, np, count = 0, limit = (uint64)sqrt((double)to);
    uint64 *primes = malloc((limit + 1) * sizeof(uint64));
    bool add;
    sieve(limit, primes, &np);
    for (i = from; i <= to; ++i) {
        add = TRUE;
        for (j = 0; j < np; ++j) {
            p = primes[j];
            p2 = p * p;
            if (p2 > i) break;
            if (i % p2 == 0) {
                add = FALSE;
                break;
            }
        }
        if (add) results[count++] = i;
    }
    *len = count;
    free(primes);
}

int main() {
    uint64 i, *sf, len;
    /* allocate enough memory to deal with all examples */
    sf = malloc(1000000 * sizeof(uint64));
    printf("Square-free integers from 1 to 145:\n");
    squareFree(1, 145, sf, &len);
    for (i = 0; i < len; ++i) {
        if (i > 0 && i % 20 == 0) {
            printf("\n");
        }
        printf("%4lld", sf[i]);
    }

    printf("\n\nSquare-free integers from %ld to %ld:\n", TRILLION, TRILLION + 145);
    squareFree(TRILLION, TRILLION + 145, sf, &len);
    for (i = 0; i < len; ++i) {
        if (i > 0 && i % 5 == 0) {
            printf("\n");
        }
        printf("%14lld", sf[i]);
    }

    printf("\n\nNumber of square-free integers:\n");
    int a[5] = {100, 1000, 10000, 100000, 1000000};
    for (i = 0; i < 5; ++i) {
        squareFree(1, a[i], sf, &len);
        printf("  from %d to %d = %lld\n", 1, a[i], len);
    }
    free(sf);
    return 0;   
}
```


{{out}}

```txt

Square-free integers from 1 to 145:
   1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31
  33  34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65
  66  67  69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95
  97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
 131 133 134 137 138 139 141 142 143 145

Square-free integers from 1000000000000 to 1000000000145:
 1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
 1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
 1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
 1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
 1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
 1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
 1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
 1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
 1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
 1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
 1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
 1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
 1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
 1000000000139 1000000000141 1000000000142 1000000000145

Number of square-free integers:
  from 1 to 100 = 61
  from 1 to 1000 = 608
  from 1 to 10000 = 6083
  from 1 to 100000 = 60794
  from 1 to 1000000 = 607926

```



## Factor

The <code>sq-free?</code> word merits some explanation. Per the Wikipedia entry on square-free integers, <q cite="https://en.wikipedia.org/wiki/Square-free_integer#Equivalent_characterizations">A positive integer <i>n</i> is square-free if and only if in the prime factorization of <i>n</i>, no prime factor occurs with an exponent larger than one.</q>

For instance, the prime factorization of <tt>12</tt> is <code>2 * 2 * 3</code>, or in other words, <code>2<sup>2</sup> * 3</code>. The <tt>2</tt> repeats, so we know <tt>12</tt> isn't square-free.

```factor
USING: formatting grouping io kernel math math.functions
math.primes.factors math.ranges sequences sets ;
IN: rosetta-code.square-free

: sq-free? ( n -- ? ) factors all-unique? ;

! Word wrap for numbers.
: numbers-per-line ( m -- n ) log10 >integer 2 + 80 swap /i ;

: sq-free-show ( from to -- )
    2dup "Square-free integers from %d to %d:\n" printf
    [ [a,b] [ sq-free? ] filter ] [ numbers-per-line group ] bi
    [ [ "%3d " printf ] each nl ] each nl ;
    
: sq-free-count ( limit -- )
    dup [1,b] [ sq-free? ] count swap
    "%6d square-free integers from 1 to %d\n" printf ;
    
1 145 10 12 ^ dup 145 + [ sq-free-show ] 2bi@         ! part 1
2 6 [a,b] [ 10 swap ^ ] map [ sq-free-count ] each    ! part 2
```

{{out}}

```txt

Square-free integers from 1 to 145:
  1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31 
 33  34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65 
 66  67  69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95 
 97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130 
131 133 134 137 138 139 141 142 143 145 

Square-free integers from 1000000000000 to 1000000000145:
1000000000001 1000000000002 1000000000003 1000000000005 1000000000006 
1000000000007 1000000000009 1000000000011 1000000000013 1000000000014 
1000000000015 1000000000018 1000000000019 1000000000021 1000000000022 
1000000000023 1000000000027 1000000000029 1000000000030 1000000000031 
1000000000033 1000000000037 1000000000038 1000000000039 1000000000041 
1000000000042 1000000000043 1000000000045 1000000000046 1000000000047 
1000000000049 1000000000051 1000000000054 1000000000055 1000000000057 
1000000000058 1000000000059 1000000000061 1000000000063 1000000000065 
1000000000066 1000000000067 1000000000069 1000000000070 1000000000073 
1000000000074 1000000000077 1000000000078 1000000000079 1000000000081 
1000000000082 1000000000085 1000000000086 1000000000087 1000000000090 
1000000000091 1000000000093 1000000000094 1000000000095 1000000000097 
1000000000099 1000000000101 1000000000102 1000000000103 1000000000105 
1000000000106 1000000000109 1000000000111 1000000000113 1000000000114 
1000000000115 1000000000117 1000000000118 1000000000119 1000000000121 
1000000000122 1000000000123 1000000000126 1000000000127 1000000000129 
1000000000130 1000000000133 1000000000135 1000000000137 1000000000138 
1000000000139 1000000000141 1000000000142 1000000000145 

    61 square-free integers from 1 to 100
   608 square-free integers from 1 to 1000
  6083 square-free integers from 1 to 10000
 60794 square-free integers from 1 to 100000
607926 square-free integers from 1 to 1000000

```


## FreeBASIC


```freebasic
' version 06-07-2018
' compile with: fbc -s console

Const As ULongInt trillion = 1000000000000ull
Const As ULong max = Sqr(trillion + 145)

Dim As UByte list(), sieve()
Dim As ULong prime()
ReDim list(max), prime(max\12), sieve(max)

Dim As ULong a, b, c, i, k, stop_ = Sqr(max)

For i = 4 To max Step 2   ' prime sieve remove even numbers except 2
    sieve(i) = 1
Next
For i = 3 To stop_ Step 2 ' proces odd numbers
    If sieve(i) = 0 Then
        For a = i * i To max Step i * 2
            sieve(a) = 1
        Next
    End If
Next

For i = 2 To max          ' move primes to a list
    If sieve(i) = 0 Then
        c += 1
        prime(c) = i
    End If
Next

ReDim sieve(145): ReDim Preserve prime(c)

For i = 1 To c  ' find all square free integers between 1 and 1000000
    a = prime(i) * prime(i)
    If a > 1000000 Then Exit For
    For k = a To 1000000 Step a
        list(k) = 1
    Next
Next

k = 0
For i = 1 To 145          ' show all between 1 and 145
    If list(i) = 0 Then
        Print Using"####"; i;
        k +=1
        If k Mod 20 = 0 Then Print 
    End If
Next
Print : Print

sieve(0) = 1              ' = trillion
For i = 1 To 5            ' process primes 2, 3, 5, 7, 11
    a = prime(i) * prime(i)
    b = a - trillion Mod a
    For k = b To 145 Step a
        sieve(k) = 1
    Next
Next

For i = 6 To c            ' process the rest of the primes
    a = prime(i) * prime(i)
    k = a - trillion Mod a
    If k <= 145 Then sieve(k) = 1
Next

k = 0
For i = 0 To 145
    If sieve(i) = 0 Then
        Print Using "################"; (trillion + i);
        k += 1
        If k Mod 5 = 0 Then print
    End If
Next
Print : Print

a = 1 : b = 100 : k = 0
Do Until b > 1000000      ' count them
    For i = a To b
        If list(i) = 0 Then k += 1
    Next
    Print "There are "; k; " square free integers between 1 and "; b
    a = b : b *= 10
Loop

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
   1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31
  33  34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65
  66  67  69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95
  97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
 131 133 134 137 138 139 141 142 143 145

   1000000000001   1000000000002   1000000000003   1000000000005   1000000000006
   1000000000007   1000000000009   1000000000011   1000000000013   1000000000014
   1000000000015   1000000000018   1000000000019   1000000000021   1000000000022
   1000000000023   1000000000027   1000000000029   1000000000030   1000000000031
   1000000000033   1000000000037   1000000000038   1000000000039   1000000000041
   1000000000042   1000000000043   1000000000045   1000000000046   1000000000047
   1000000000049   1000000000051   1000000000054   1000000000055   1000000000057
   1000000000058   1000000000059   1000000000061   1000000000063   1000000000065
   1000000000066   1000000000067   1000000000069   1000000000070   1000000000073
   1000000000074   1000000000077   1000000000078   1000000000079   1000000000081
   1000000000082   1000000000085   1000000000086   1000000000087   1000000000090
   1000000000091   1000000000093   1000000000094   1000000000095   1000000000097
   1000000000099   1000000000101   1000000000102   1000000000103   1000000000105
   1000000000106   1000000000109   1000000000111   1000000000113   1000000000114
   1000000000115   1000000000117   1000000000118   1000000000119   1000000000121
   1000000000122   1000000000123   1000000000126   1000000000127   1000000000129
   1000000000130   1000000000133   1000000000135   1000000000137   1000000000138
   1000000000139   1000000000141   1000000000142   1000000000145

There are 61 square free integers between 1 and 100
There are 608 square free integers between 1 and 1000
There are 6083 square free integers between 1 and 10000
There are 60794 square free integers between 1 and 100000
There are 607926 square free integers between 1 and 1000000
```



## Go


```go
package main

import (
    "fmt"
    "math"
)

func sieve(limit uint64) []uint64 {
    primes := []uint64{2}
    c := make([]bool, limit+1) // composite = true
    // no need to process even numbers > 2
    p := uint64(3)
    for {
        p2 := p * p
        if p2 > limit {
            break
        }
        for i := p2; i <= limit; i += 2 * p {
            c[i] = true
        }
        for {
            p += 2
            if !c[p] {
                break
            }
        }
    }
    for i := uint64(3); i <= limit; i += 2 {
        if !c[i] {
            primes = append(primes, i)
        }
    }
    return primes
}

func squareFree(from, to uint64) (results []uint64) {
    limit := uint64(math.Sqrt(float64(to)))
    primes := sieve(limit)
outer:
    for i := from; i <= to; i++ {
        for _, p := range primes {
            p2 := p * p
            if p2 > i {
                break
            }
            if i%p2 == 0 {
                continue outer
            }
        }
        results = append(results, i)
    }
    return
}

const trillion uint64 = 1000000000000

func main() {
    fmt.Println("Square-free integers from 1 to 145:")
    sf := squareFree(1, 145)
    for i := 0; i < len(sf); i++ {
        if i > 0 && i%20 == 0 {
            fmt.Println()
        }
        fmt.Printf("%4d", sf[i])
    }

    fmt.Printf("\n\nSquare-free integers from %d to %d:\n", trillion, trillion+145)
    sf = squareFree(trillion, trillion+145)
    for i := 0; i < len(sf); i++ {
        if i > 0 && i%5 == 0 {
            fmt.Println()
        }
        fmt.Printf("%14d", sf[i])
    }

    fmt.Println("\n\nNumber of square-free integers:\n")
    a := [...]uint64{100, 1000, 10000, 100000, 1000000}
    for _, n := range a {
        fmt.Printf("  from %d to %d = %d\n", 1, n, len(squareFree(1, n)))
    }
}
```


{{out}}

```txt

Square-free integers from 1 to 145:
   1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31
  33  34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65
  66  67  69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95
  97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
 131 133 134 137 138 139 141 142 143 145

Square-free integers from 1000000000000 to 1000000000145:
 1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
 1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
 1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
 1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
 1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
 1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
 1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
 1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
 1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
 1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
 1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
 1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
 1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
 1000000000139 1000000000141 1000000000142 1000000000145

Number of square-free integers:

  from 1 to 100 = 61
  from 1 to 1000 = 608
  from 1 to 10000 = 6083
  from 1 to 100000 = 60794
  from 1 to 1000000 = 607926

```



## Haskell


```haskell
import Data.List.Split (chunksOf)
import Math.NumberTheory.Primes (factorise)
import Text.Printf (printf)

-- True iff the argument is a square-free number.
isSquareFree :: Integer -> Bool
isSquareFree = all ((== 1) . snd) . factorise

-- All square-free numbers in the range [lo, hi].
squareFrees :: Integer -> Integer -> [Integer]
squareFrees lo hi = filter isSquareFree [lo..hi]

-- The result of `counts limits values' is the number of values less than or
-- equal to each successive limit.  Both limits and values are assumed to be
-- in increasing order.
counts :: (Ord a, Num b) => [a] -> [a] -> [b]
counts = go 0
  where go c lims@(l:ls) (v:vs) | v > l     = c : go (c+1) ls vs
                                | otherwise = go (c+1) lims vs
        go _ [] _  = []
        go c ls [] = replicate (length ls) c

printSquareFrees :: Int -> Integer -> Integer -> IO ()
printSquareFrees cols lo hi =
  let ns = squareFrees lo hi
      title = printf "Square free numbers from %d to %d\n" lo hi
      body = unlines $ map concat $ chunksOf cols $ map (printf " %3d") ns
  in putStrLn $ title ++ body

printSquareFreeCounts :: [Integer] -> Integer -> Integer -> IO ()
printSquareFreeCounts lims lo hi =
  let cs = counts lims $ squareFrees lo hi :: [Integer]
      title = printf "Counts of square-free numbers\n"
      body = unlines $ zipWith (printf "  from 1 to %d: %d") lims cs
  in putStrLn $ title ++ body

main :: IO ()
main = do
  printSquareFrees 20 1 145
  printSquareFrees 5 1000000000000 1000000000145
  printSquareFreeCounts [100, 1000, 10000, 100000, 1000000] 1 1000000
```


{{out}}

```txt

Square free numbers from 1 to 145
   1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31
  33  34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65
  66  67  69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95
  97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
 131 133 134 137 138 139 141 142 143 145

Square free numbers from 1000000000000 to 1000000000145
 1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
 1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
 1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
 1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
 1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
 1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
 1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
 1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
 1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
 1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
 1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
 1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
 1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
 1000000000139 1000000000141 1000000000142 1000000000145

Counts of square-free numbers
  from 1 to 100: 61
  from 1 to 1000: 608
  from 1 to 10000: 6083
  from 1 to 100000: 60794
  from 1 to 1000000: 607926

```



## J

'''Solution:'''

```j
isSqrFree=: (#@~. = #)@q:   NB. are there no duplicates in the prime factors of a number?
filter=: adverb def ' #~ u' NB. filter right arg using verb to left
countSqrFree=: +/@:isSqrFree
thru=: <. + i.@(+ *)@-~     NB. helper verb
```


'''Required Examples:'''

```j
   isSqrFree filter 1 thru 145   NB. returns all results, but not all are displayed
1 2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30 31 33 34 35 37 38 39 41 42 43 46 47 51 53 55 57 58 59 61 62 65 66 67 69 70 71 73 74 77 78 79 82 83 85 86 87 89 91 93 94 95 97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130 131...
   100 list isSqrFree filter 1000000000000 thru 1000000000145  NB. ensure that all results are displayed
1000000000001 1000000000002 1000000000003 1000000000005 1000000000006 1000000000007 1000000000009
1000000000011 1000000000013 1000000000014 1000000000015 1000000000018 1000000000019 1000000000021
1000000000022 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031 1000000000033
1000000000037 1000000000038 1000000000039 1000000000041 1000000000042 1000000000043 1000000000045
1000000000046 1000000000047 1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
1000000000058 1000000000059 1000000000061 1000000000063 1000000000065 1000000000066 1000000000067
1000000000069 1000000000070 1000000000073 1000000000074 1000000000077 1000000000078 1000000000079
1000000000081 1000000000082 1000000000085 1000000000086 1000000000087 1000000000090 1000000000091
1000000000093 1000000000094 1000000000095 1000000000097 1000000000099 1000000000101 1000000000102
1000000000103 1000000000105 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
1000000000115 1000000000117 1000000000118 1000000000119 1000000000121 1000000000122 1000000000123
1000000000126 1000000000127 1000000000129 1000000000130 1000000000133 1000000000135 1000000000137
1000000000138 1000000000139 1000000000141 1000000000142 1000000000145
   countSqrFree 1 thru 100
61
   countSqrFree 1 thru 1000
608
   1 countSqrFree@thru&> 10 ^ 2 3 4 5 6  NB. count square free ints for 1 to each of 100, 1000, 10000, 10000, 100000 and 1000000
61 608 6083 60794 607926
```



## Java

{{trans|Go}}

```java
import java.util.ArrayList;
import java.util.List;

public class SquareFree
{
    private static List<Long> sieve(long limit) {
        List<Long> primes = new ArrayList<Long>();
        primes.add(2L);
        boolean[] c = new boolean[(int)limit + 1]; // composite = true
        // no need to process even numbers > 2
        long p = 3;
        for (;;) {
            long p2 = p * p;
            if (p2 > limit) break;
            for (long i = p2; i <= limit; i += 2 * p) c[(int)i] = true;
            for (;;) {
                p += 2;
                if (!c[(int)p]) break;
            }
        }
        for (long i = 3; i <= limit; i += 2) {
            if (!c[(int)i]) primes.add(i);
        }
        return primes;
    }

    private static List<Long> squareFree(long from, long to) {
        long limit = (long)Math.sqrt((double)to);
        List<Long> primes = sieve(limit);
        List<Long> results = new ArrayList<Long>();

        outer: for (long i = from; i <= to; i++) {
            for (long p : primes) {
                long p2 = p * p;
                if (p2 > i) break;
                if (i % p2 == 0) continue outer;
            }
            results.add(i);
        }
        return results;
    }

    private final static long TRILLION = 1000000000000L;

    public static void main(String[] args) {
        System.out.println("Square-free integers from 1 to 145:");
        List<Long> sf = squareFree(1, 145);
        for (int i = 0; i < sf.size(); i++) {
            if (i > 0 && i % 20 == 0) {
                System.out.println();
            }
            System.out.printf("%4d", sf.get(i));
        }

        System.out.print("\n\nSquare-free integers");
        System.out.printf(" from %d to %d:\n", TRILLION, TRILLION + 145);
        sf = squareFree(TRILLION, TRILLION + 145);
        for (int i = 0; i < sf.size(); i++) {
            if (i > 0 && i % 5 == 0) System.out.println();
            System.out.printf("%14d", sf.get(i));
        }

        System.out.println("\n\nNumber of square-free integers:\n");
        long[] tos = {100, 1000, 10000, 100000, 1000000};
        for (long to : tos) {
            System.out.printf("  from %d to %d = %d\n", 1, to, squareFree(1, to).size());
        }
    }
}
```


{{out}}

```txt

Square-free integers from 1 to 145:
   1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31
  33  34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65
  66  67  69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95
  97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
 131 133 134 137 138 139 141 142 143 145

Square-free integers from 1000000000000 to 1000000000145:
 1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
 1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
 1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
 1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
 1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
 1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
 1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
 1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
 1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
 1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
 1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
 1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
 1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
 1000000000139 1000000000141 1000000000142 1000000000145

Number of square-free integers:

  from 1 to 100 = 61
  from 1 to 1000 = 608
  from 1 to 10000 = 6083
  from 1 to 100000 = 60794
  from 1 to 1000000 = 607926

```



## jq

Requires jq 1.5 or higher

For brevity and in order to highlight some points of interest regarding jq,
this entry focuses on solving the task in a manner that reflects
the specification as closely as possible (no prime sieves or calls
to sqrt), with efficiency concerns playing second fiddle.

Once a suitable generator for squares and a test for divisibility have been written, the
test for whether a number is square-free can be written in one line:

    def is_square_free: . as $n | all( squares; divides($n) | not);

In words: to verify whether an integer, $n, is square_free, check that no admissible square divides $n.

'''squares'''

We could define the required `squares` generator using `while`:
    def squares: . as $n | 2 | while(.*. <= $n; .+1) | .*.;

(Here `.*.` calculates the square of the input number.)

However, this entails performing an unnecessary multiplication, so the question becomes whether there
is a more economical solution that closely reflects the specification of the required generator.  Since jq supports tail-recursion optimization for 0-arity filters, the answer is:


    def squares:
      . as $n
      | def s:
          (.*.) as $s
          | select($s <= $n)
          | $s, ((1+.)|s);
          2|s;

The point of interest here is the def-within-a-def.

'''divides'''

    def divides($x): ($x % .) == 0;
  
'''is_square_free'''
    `is_square_free` as defined here intentionally returns true for all numeric inputs less than 4.
    def is_square_free: . as $n | all( squares; divides($n) | not) ;


----


'''The tasks'''

The primary task is to examine square-free numbers in an inclusive range,
so we define `square_free` to emit a stream of such numbers:

    def square_free(from; including):
      range(from;including+1) | select( is_square_free ) ;

    # Compute SIGMA(s) where s is a stream
    def sigma(s): reduce s as $s (null; .+$s);

    # Group items in a stream into arrays of length at most $n.
    # For generality, this function uses `nan` as the eos marker.
    def nwise(stream; $n):
      foreach (stream, nan) as $x ([];
        if length == $n then [$x] else . + [$x] end;
        if (.[-1] | isnan) and length>1 then .[:-1]
        elif length == $n then .
        else empty
        end);

    def prettify_squares(from; including; width):
      "Square-free integers from \(from) to \(including) (inclusive):",
      (nwise( square_free(from;including); width) | map(tostring) | join(" ")),
      "";

    def prettify_count($from; $including):
      "Count from \($from) to \($including) inclusive: \(sigma( square_free($from ; $including) | 1 ))";

'''The specific tasks'''

    prettify_squares(1;145; 20),
    prettify_squares(1E12; 1E12 + 145; 5),
    ((1E2, 1E3, 1E4, 1E5, 1E6) | prettify_count(1; .))



'''Output'''

```txt
Square-free integers from 1 to 145 (inclusive):
1 2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30 31
33 34 35 37 38 39 41 42 43 46 47 51 53 55 57 58 59 61 62 65
66 67 69 70 71 73 74 77 78 79 82 83 85 86 87 89 91 93 94 95
97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
131 133 134 137 138 139 141 142 143 145

Square-free integers from 1000000000000 to 1000000000145 (inclusive):
1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
1000000000139 1000000000141 1000000000142 1000000000145

Count from 1 to 100 inclusive: 61
Count from 1 to 1000 inclusive: 608
Count from 1 to 10000 inclusive: 6083
Count from 1 to 100000 inclusive: 60794
Count from 1 to 1000000 inclusive: 607926
```



## Julia


```julia
using Primes

const maxrootprime = Int64(floor(sqrt(1000000000145)))
const sqprimes = map(x -> x * x, primes(2, maxrootprime))
possdivisorsfor(n) = vcat(filter(x -> x <= n / 2, sqprimes), n in sqprimes ? n : [])
issquarefree(n) = all(x -> floor(n / x) != n / x, possdivisorsfor(n))

function squarefreebetween(mn, mx)
    count = 1
    padsize = length(string(mx)) + 2
    println("The squarefree numbers between $mn and $mx are:")
    for n in mn:mx
        if issquarefree(n)
            print(lpad(string(n), padsize))
            count += 1
        end
        if count * padsize > 80
            println()
            count = 1
        end
    end
    println()
end

function squarefreecount(intervals, maxnum)
    count = 0
    for n in 1:maxnum
        for i in 1:length(intervals)
            if intervals[i] < n
                println("There are $count square free numbers between 1 and $(intervals[i]).")
                intervals[i] = maxnum + 1
            end
        end
        if issquarefree(n)
            count += 1
        end
    end
    println("There are $count square free numbers between 1 and $maxnum.")    
end

squarefreebetween(1, 145)
squarefreebetween(1000000000000, 1000000000145)
squarefreecount([100, 1000, 10000, 100000], 1000000)

```
 {{output}} 
```txt

 The squarefree numbers between 1 and 145 are:
    1    2    3    5    6    7   10   11   13   14   15   17   19   21   22   23
   26   29   30   31   33   34   35   37   38   39   41   42   43   46   47   51
   53   55   57   58   59   61   62   65   66   67   69   70   71   73   74   77
   78   79   82   83   85   86   87   89   91   93   94   95   97  101  102  103
  105  106  107  109  110  111  113  114  115  118  119  122  123  127  129  130
  131  133  134  137  138  139  141  142  143  145
 The squarefree numbers between 1000000000000 and 1000000000145 are:
  1000000000001  1000000000002  1000000000003  1000000000005  1000000000006
  1000000000007  1000000000009  1000000000011  1000000000013  1000000000014
  1000000000015  1000000000018  1000000000019  1000000000021  1000000000022
  1000000000023  1000000000027  1000000000029  1000000000030  1000000000031
  1000000000033  1000000000037  1000000000038  1000000000039  1000000000041
  1000000000042  1000000000043  1000000000045  1000000000046  1000000000047
  1000000000049  1000000000051  1000000000054  1000000000055  1000000000057
  1000000000058  1000000000059  1000000000061  1000000000063  1000000000065
  1000000000066  1000000000067  1000000000069  1000000000070  1000000000073
  1000000000074  1000000000077  1000000000078  1000000000079  1000000000081
  1000000000082  1000000000085  1000000000086  1000000000087  1000000000090
  1000000000091  1000000000093  1000000000094  1000000000095  1000000000097
  1000000000099  1000000000101  1000000000102  1000000000103  1000000000105
  1000000000106  1000000000109  1000000000111  1000000000113  1000000000114
  1000000000115  1000000000117  1000000000118  1000000000119  1000000000121
  1000000000122  1000000000123  1000000000126  1000000000127  1000000000129
  1000000000130  1000000000133  1000000000135  1000000000137  1000000000138
  1000000000139  1000000000141  1000000000142  1000000000145
 There are 61 square free numbers between 1 and 100.
 There are 608 square free numbers between 1 and 1000.
 There are 6083 square free numbers between 1 and 10000.
 There are 60794 square free numbers between 1 and 100000.
 There are 607926 square free numbers between 1 and 1000000.

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.2.50

import kotlin.math.sqrt

fun sieve(limit: Long): List<Long> {
    val primes = mutableListOf(2L)
    val c = BooleanArray(limit.toInt() + 1) // composite = true
    // no need to process even numbers > 2
    var p = 3
    while (true) {
        val p2 = p * p
        if (p2 > limit) break
        for (i in p2..limit step 2L * p) c[i.toInt()] = true
        do { p += 2 } while (c[p])
    }
    for (i in 3..limit step 2)
        if (!c[i.toInt()])
            primes.add(i)

    return primes
}

fun squareFree(r: LongProgression): List<Long> {
    val primes = sieve(sqrt(r.last.toDouble()).toLong())
    val results = mutableListOf<Long>()
    outer@ for (i in r) {
        for (p in primes) {
            val p2 = p * p
            if (p2 > i) break
            if (i % p2 == 0L) continue@outer
        }
        results.add(i)
    }
    return results
}

fun printResults(r: LongProgression, c: Int, f: Int) {
    println("Square-free integers from ${r.first} to ${r.last}:")
    squareFree(r).chunked(c).forEach {
        println()
        it.forEach { print("%${f}d".format(it)) }
    }
    println('\n')
}

const val TRILLION = 1000000_000000L

fun main(args: Array<String>) {
    printResults(1..145L, 20, 4)
    printResults(TRILLION..TRILLION + 145L, 5, 14)

    println("Number of square-free integers:\n")
    longArrayOf(100, 1000, 10000, 100000, 1000000).forEach {
        j -> println("  from 1 to $j = ${squareFree(1..j).size}")
    }
}
```


{{out}}

```txt

Square-free integers from 1 to 145:
   1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31
  33  34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65
  66  67  69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95
  97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
 131 133 134 137 138 139 141 142 143 145

Square-free integers from 1000000000000 to 1000000000145:
 1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
 1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
 1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
 1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
 1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
 1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
 1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
 1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
 1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
 1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
 1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
 1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
 1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
 1000000000139 1000000000141 1000000000142 1000000000145

Number of square-free integers:

  from 1 to 100 = 61
  from 1 to 1000 = 608
  from 1 to 10000 = 6083
  from 1 to 100000 = 60794
  from 1 to 1000000 = 607926

```



## Lua

This is a naive method, runs in about 1 second on LuaJIT.

```lua
function squareFree (n)
  for root = 2, math.sqrt(n) do
    if n % (root * root) == 0 then return false end
  end
  return true
end

function run (lo, hi, showValues)
  io.write("From " .. lo .. " to " .. hi)
  io.write(showValues and ":\n" or " = ")
  local count = 0
  for i = lo, hi do
    if squareFree(i) then
      if showValues then
        io.write(i, "\t")
      else
        count = count + 1
      end
    end
  end
  print(showValues and "\n" or count)
end

local testCases = {
  {1, 145, true},
  {1000000000000, 1000000000145, true},
  {1, 100},
  {1, 1000},
  {1, 10000},
  {1, 100000},
  {1, 1000000}
}
for _, example in pairs(testCases) do run(unpack(example)) end
```

{{out}}

```txt
From 1 to 145:
1       2       3       5       6       7       10      11      13      14
15      17      19      21      22      23      26      29      30      31
33      34      35      37      38      39      41      42      43      46
47      51      53      55      57      58      59      61      62      65
66      67      69      70      71      73      74      77      78      79
82      83      85      86      87      89      91      93      94      95
97      101     102     103     105     106     107     109     110     111
113     114     115     118     119     122     123     127     129     130
131     133     134     137     138     139     141     142     143     145


From 1000000000000 to 1000000000145:
1000000000001   1000000000002   1000000000003   1000000000005   1000000000006
1000000000007   1000000000009   1000000000011   1000000000013   1000000000014
1000000000015   1000000000018   1000000000019   1000000000021   1000000000022
1000000000023   1000000000027   1000000000029   1000000000030   1000000000031
1000000000033   1000000000037   1000000000038   1000000000039   1000000000041
1000000000042   1000000000043   1000000000045   1000000000046   1000000000047
1000000000049   1000000000051   1000000000054   1000000000055   1000000000057
1000000000058   1000000000059   1000000000061   1000000000063   1000000000065
1000000000066   1000000000067   1000000000069   1000000000070   1000000000073
1000000000074   1000000000077   1000000000078   1000000000079   1000000000081
1000000000082   1000000000085   1000000000086   1000000000087   1000000000090
1000000000091   1000000000093   1000000000094   1000000000095   1000000000097
1000000000099   1000000000101   1000000000102   1000000000103   1000000000105
1000000000106   1000000000109   1000000000111   1000000000113   1000000000114
1000000000115   1000000000117   1000000000118   1000000000119   1000000000121
1000000000122   1000000000123   1000000000126   1000000000127   1000000000129
1000000000130   1000000000133   1000000000135   1000000000137   1000000000138
1000000000139   1000000000141   1000000000142   1000000000145

From 1 to 100 = 61
From 1 to 1000 = 608
From 1 to 10000 = 6083
From 1 to 100000 = 60794
From 1 to 1000000 = 607926
```



## Mathematica


```Mathematica
squareFree[n_Integer] := DeleteCases[Last /@ FactorInteger[n], 1] === {};
findSquareFree[n__] := Select[Range[n], squareFree];
findSquareFree[45]
findSquareFree[10^9, 10^9 + 145]
Length[findSquareFree[10^6]]
```

{{out}}

```txt
{1, 2, 3, 5, 6, 7, 10, 11, 13, 14, 15, 17, 19, 21, 22, 23, 26, 29, 30, 31, 33, 34, 35, 37, 38, 39, 41, 42, 43}

{1000000001, 1000000002, 1000000003, 1000000005, 1000000006, 
1000000007, 1000000009, 1000000010, 1000000011, 1000000013, 
1000000014, 1000000015, 1000000018, 1000000019, 1000000021, 
1000000022, 1000000027, 1000000029, 1000000030, 1000000031, 
1000000033, 1000000034, 1000000037, 1000000038, 1000000039, 
1000000041, 1000000042, 1000000043, 1000000045, 1000000046, 
1000000047, 1000000049, 1000000051, 1000000054, 1000000055, 
1000000057, 1000000058, 1000000059, 1000000061, 1000000063, 
1000000065, 1000000066, 1000000067, 1000000069, 1000000070, 
1000000073, 1000000074, 1000000077, 1000000078, 1000000079, 
1000000081, 1000000082, 1000000083, 1000000086, 1000000087, 
1000000090, 1000000091, 1000000093, 1000000094, 1000000095, 
1000000097, 1000000099, 1000000101, 1000000102, 1000000103, 
1000000105, 1000000106, 1000000109, 1000000110, 1000000111, 
1000000113, 1000000114, 1000000115, 1000000117, 1000000118, 
1000000119, 1000000121, 1000000122, 1000000123, 1000000126, 
1000000127, 1000000129, 1000000130, 1000000131, 1000000133, 
1000000135, 1000000137, 1000000138, 1000000139, 1000000141, 1000000142}

607926

```



## Perl

{{libheader|ntheory}}

```perl
use ntheory qw/is_square_free moebius/;

sub square_free_count {
    my ($n) = @_;
    my $count = 0;
    foreach my $k (1 .. sqrt($n)) {
        $count += moebius($k) * int($n / $k**2);
    }
    return $count;
}

print "Square─free numbers between 1 and 145:\n";
print join(' ', grep { is_square_free($_) } 1 .. 145), "\n";

print "\nSquare-free numbers between 10^12 and 10^12 + 145:\n";
print join(' ', grep { is_square_free($_) } 1e12 .. 1e12 + 145), "\n";

print "\n";
foreach my $n (2 .. 6) {
    my $c = square_free_count(10**$n);
    print "The number of square-free numbers between 1 and 10^$n (inclusive) is: $c\n";
}
```

{{out}}

```txt

Square─free numbers between 1 and 145:
1 2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30 31 33 34 35 37 38 39 41 42 43 46 47 51 53 55 57 58 59 61 62 65 66 67 69 70 71 73 74 77 78 79 82 83 85 86 87 89 91 93 94 95 97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130 131 133 134 137 138 139 141 142 143 145

Square-free numbers between 10^12 and 10^12 + 145:
1000000000001 1000000000002 1000000000003 1000000000005 1000000000006 1000000000007 1000000000009 1000000000011 1000000000013 1000000000014 1000000000015 1000000000018 1000000000019 1000000000021 1000000000022 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031 1000000000033 1000000000037 1000000000038 1000000000039 1000000000041 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047 1000000000049 1000000000051 1000000000054 1000000000055 1000000000057 1000000000058 1000000000059 1000000000061 1000000000063 1000000000065 1000000000066 1000000000067 1000000000069 1000000000070 1000000000073 1000000000074 1000000000077 1000000000078 1000000000079 1000000000081 1000000000082 1000000000085 1000000000086 1000000000087 1000000000090 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097 1000000000099 1000000000101 1000000000102 1000000000103 1000000000105 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114 1000000000115 1000000000117 1000000000118 1000000000119 1000000000121 1000000000122 1000000000123 1000000000126 1000000000127 1000000000129 1000000000130 1000000000133 1000000000135 1000000000137 1000000000138 1000000000139 1000000000141 1000000000142 1000000000145

The number of square-free numbers between 1 and 10^2 (inclusive) is: 61
The number of square-free numbers between 1 and 10^3 (inclusive) is: 608
The number of square-free numbers between 1 and 10^4 (inclusive) is: 6083
The number of square-free numbers between 1 and 10^5 (inclusive) is: 60794
The number of square-free numbers between 1 and 10^6 (inclusive) is: 607926

```



## Perl 6

{{works with|Rakudo|2018.06}}
The prime factoring algorithm is not really the best option for finding long runs of sequential square-free numbers. It works, but is probably better suited for testing arbitrary numbers rather than testing every sequential number from 1 to some limit. If you know that that is going to be your use case, there are faster algorithms.


```perl6
# Prime factorization routines
sub prime-factors ( Int $n where * > 0 ) {
    return $n if $n.is-prime;
    return [] if $n == 1;
    my $factor = find-factor( $n );
    flat prime-factors( $factor ), prime-factors( $n div $factor );
}

sub find-factor ( Int $n, $constant = 1 ) {
    return 2 unless $n +& 1;
    if (my $gcd = $n gcd 6541380665835015) > 1 {
        return $gcd if $gcd != $n
    }
    my $x      = 2;
    my $rho    = 1;
    my $factor = 1;
    while $factor == 1 {
        $rho *= 2;
        my $fixed = $x;
        for ^$rho {
            $x = ( $x * $x + $constant ) % $n;
            $factor = ( $x - $fixed ) gcd $n;
            last if 1 < $factor;
        }
    }
    $factor = find-factor( $n, $constant + 1 ) if $n == $factor;
    $factor;
}

# Task routine
sub is-square-free (Int $n) { my @v = $n.&prime-factors.Bag.values; @v.sum/@v <= 1 }

# The Task
# Parts 1 & 2
for 1, 145, 1e12.Int, 145+1e12.Int -> $start, $end {
    say "\nSquare─free numbers between $start and $end:\n",
    ($start .. $end).hyper(:4batch).grep( *.&is-square-free ).list.fmt("%3d").comb(84).join("\n");
}

# Part 3
for 1e2, 1e3, 1e4, 1e5, 1e6 {
    say "\nThe number of square─free numbers between 1 and {$_} (inclusive) is: ",
    +(1 .. .Int).race.grep: *.&is-square-free;
}
```

{{out}}

```txt

Square─free numbers between 1 and 145:
  1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31  33 
 34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65  66  67 
 69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95  97 101 102 
103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130 131 133 134 137 
138 139 141 142 143 145

Square─free numbers between 1000000000000 and 1000000000145:
1000000000001 1000000000002 1000000000003 1000000000005 1000000000006 1000000000007 
1000000000009 1000000000011 1000000000013 1000000000014 1000000000015 1000000000018 
1000000000019 1000000000021 1000000000022 1000000000023 1000000000027 1000000000029 
1000000000030 1000000000031 1000000000033 1000000000037 1000000000038 1000000000039 
1000000000041 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047 
1000000000049 1000000000051 1000000000054 1000000000055 1000000000057 1000000000058 
1000000000059 1000000000061 1000000000063 1000000000065 1000000000066 1000000000067 
1000000000069 1000000000070 1000000000073 1000000000074 1000000000077 1000000000078 
1000000000079 1000000000081 1000000000082 1000000000085 1000000000086 1000000000087 
1000000000090 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097 
1000000000099 1000000000101 1000000000102 1000000000103 1000000000105 1000000000106 
1000000000109 1000000000111 1000000000113 1000000000114 1000000000115 1000000000117 
1000000000118 1000000000119 1000000000121 1000000000122 1000000000123 1000000000126 
1000000000127 1000000000129 1000000000130 1000000000133 1000000000135 1000000000137 
1000000000138 1000000000139 1000000000141 1000000000142 1000000000145

The number of square─free numbers between 1 and 100 (inclusive) is: 61

The number of square─free numbers between 1 and 1000 (inclusive) is: 608

The number of square─free numbers between 1 and 10000 (inclusive) is: 6083

The number of square─free numbers between 1 and 100000 (inclusive) is: 60794

The number of square─free numbers between 1 and 1000000 (inclusive) is: 607926
```



## Phix


```Phix
function square_free(atom start, finish)
    sequence res = {}
    if start=1 then res = {1} start = 2 end if
    while start<=finish do
        sequence pf = prime_factors(start, duplicates:=true)
        for i=2 to length(pf) do
            if pf[i]=pf[i-1] then
                pf = {}
                exit
            end if
        end for
        if pf!={} then
            res &= start
        end if
        start += 1
    end while
    return res
end function

function format_res(sequence res, string fmt)
    for i=1 to length(res) do
        res[i] = sprintf(fmt,res[i])
    end for
    return res
end function

constant ONE_TRILLION = 1_000_000_000_000

procedure main()
    sequence res = square_free(1,145)
    printf(1,"There are %d square-free integers from 1 to 145:\n",length(res))
    puts(1,join_by(format_res(res,"%4d"),1,20,""))
 
    res = square_free(ONE_TRILLION,ONE_TRILLION+145)
    printf(1,"\nThere are %d square-free integers from %,d to %,d:\n", 
             {length(res),ONE_TRILLION, ONE_TRILLION+145})
    puts(1,join_by(format_res(res,"%14d"),1,5,""))
 
    printf(1,"\nNumber of square-free integers:\n");
    for i=2 to 6 do
        integer lim = power(10,i),
                len = length(square_free(1,lim))
        printf(1,"  from %,d to %,d = %,d\n", {1,lim,len})
    end for
end procedure
main()
```

{{out}}

```txt

There are 90 square-free integers from 1 to 145:
   1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26  29  30  31
  33  34  35  37  38  39  41  42  43  46  47  51  53  55  57  58  59  61  62  65
  66  67  69  70  71  73  74  77  78  79  82  83  85  86  87  89  91  93  94  95
  97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
 131 133 134 137 138 139 141 142 143 145

There are 89 square-free integers from 1,000,000,000,000 to 1,000,000,000,145:
 1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
 1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
 1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
 1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
 1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
 1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
 1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
 1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
 1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
 1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
 1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
 1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
 1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
 1000000000139 1000000000141 1000000000142 1000000000145

Number of square-free integers:
  from 1 to 100 = 61
  from 1 to 1,000 = 608
  from 1 to 10,000 = 6,083
  from 1 to 100,000 = 60,794
  from 1 to 1,000,000 = 607,926

```



## Python


```Python

import math

def SquareFree ( _number ) :
	max = (int) (math.sqrt ( _number ))

	for root in range ( 2, max+1 ):					# Create a custom prime sieve
		if 0 == _number % ( root * root ):
			return False

	return True

def ListSquareFrees( _start, _end ):
	count = 0
	for i in range ( _start, _end+1 ):
		if True == SquareFree( i ):
			print ( "{}\t".format(i), end="" )
			count += 1

	print ( "\n\nTotal count of square-free numbers between {} and {}: {}".format(_start, _end, count))

ListSquareFrees( 1, 100 )
ListSquareFrees( 1000000000000, 1000000000145 )

```


'''Output:'''

```txt

1	2	3	5	6	7	10	11	13	
14	15	17	19	21	22	23	26	29	
30	31	33	34	35	37	38	39	41	
42	43	46	47	51	53	55	57	58	
59	61	62	65	66	67	69	70	71	
73	74	77	78	79	82	83	85	86	
87	89	91	93	94	95	97	

Total count of square-free numbers between 1 and 100: 61
1000000000001	1000000000002	1000000000003	1000000000005	1000000000006	
1000000000007	1000000000009	1000000000011	1000000000013	1000000000014	
1000000000015	1000000000018	1000000000019	1000000000021	1000000000022	
1000000000023	1000000000027	1000000000029	1000000000030	1000000000031	
1000000000033	1000000000037	1000000000038	1000000000039	1000000000041	
1000000000042	1000000000043	1000000000045	1000000000046	1000000000047	
1000000000049	1000000000051	1000000000054	1000000000055	1000000000057	
1000000000058	1000000000059	1000000000061	1000000000063	1000000000065	
1000000000066	1000000000067	1000000000069	1000000000070	1000000000073	
1000000000074	1000000000077	1000000000078	1000000000079	1000000000081	
1000000000082	1000000000085	1000000000086	1000000000087	1000000000090	
1000000000091	1000000000093	1000000000094	1000000000095	1000000000097	
1000000000099	1000000000101	1000000000102	1000000000103	1000000000105	
1000000000106	1000000000109	1000000000111	1000000000113	1000000000114	
1000000000115	1000000000117	1000000000118	1000000000119	1000000000121	
1000000000122	1000000000123	1000000000126	1000000000127	1000000000129	
1000000000130	1000000000133	1000000000135	1000000000137	1000000000138	
1000000000139	1000000000141	1000000000142	1000000000145	

Total count of square-free numbers between 1000000000000 and 1000000000145: 89

```



## Racket



```racket
#lang racket

(define (not-square-free-set-for-range range-min (range-max (add1 range-min)))
  (for*/set ((i2 (sequence-map sqr (in-range 2 (add1 (integer-sqrt range-max)))))
             (i2.x (in-range (* i2 (quotient range-min i2))
                             (* i2 (add1 (quotient range-max i2)))
                             i2))
             #:when (and (<= range-min i2.x)
                         (< i2.x range-max)))
    i2.x))

(define (square-free? n #:table (table (not-square-free-set-for-range n)))
  (not (set-member? table n)))

(define (count-square-free-numbers #:range-min (range-min 1) range-max)
  (- range-max range-min (set-count (not-square-free-set-for-range range-min range-max))))

(define ((print-list-to-width w) l)
  (let loop ((l l) (x 0))
    (if (null? l)
        (unless (zero? x) (newline))
        (let* ((str (~a (car l))) (len (string-length str)))
          (cond [(<= (+ len x) w) (display str) (write-char #\space) (loop (cdr l) (+ x len 1))]
                [(zero? x) (displayln str) (loop (cdr l) 0)]
                [else (newline) (loop l 0)])))))
                 

(define print-list-to-80 (print-list-to-width 80))

(module+ main
  (print-list-to-80 (for/list ((n (in-range 1 (add1 145))) #:when (square-free? n)) n))
  
  (print-list-to-80 (time (let ((table (not-square-free-set-for-range #e1e12 (add1 (+ #e1e12 145)))))
                            (for/list ((n (in-range #e1e12 (add1 (+ #e1e12 145))))
                                       #:when (square-free? n #:table table)) n))))
  (displayln "Compare time taken without the table (rather with table on the fly):")
  (void (time (for/list ((n (in-range #e1e12 (add1 (+ #e1e12 145)))) #:when (square-free? n)) n)))

  (count-square-free-numbers 100)
  (count-square-free-numbers 1000)
  (count-square-free-numbers 10000)
  (count-square-free-numbers 100000)
  (count-square-free-numbers 1000000))
```


{{out}}


```txt
1 2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30 31 33 34 35 37 38 39 41 42 43 
46 47 51 53 55 57 58 59 61 62 65 66 67 69 70 71 73 74 77 78 79 82 83 85 86 87 89 
91 93 94 95 97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 
127 129 130 131 133 134 137 138 139 141 142 143 145 
cpu time: 1969 real time: 1967 gc time: 876
1000000000001 1000000000002 1000000000003 1000000000005 1000000000006 
1000000000007 1000000000009 1000000000011 1000000000013 1000000000014 
1000000000015 1000000000018 1000000000019 1000000000021 1000000000022 
1000000000023 1000000000027 1000000000029 1000000000030 1000000000031 
1000000000033 1000000000037 1000000000038 1000000000039 1000000000041 
1000000000042 1000000000043 1000000000045 1000000000046 1000000000047 
1000000000049 1000000000051 1000000000054 1000000000055 1000000000057 
1000000000058 1000000000059 1000000000061 1000000000063 1000000000065 
1000000000066 1000000000067 1000000000069 1000000000070 1000000000073 
1000000000074 1000000000077 1000000000078 1000000000079 1000000000081 
1000000000082 1000000000085 1000000000086 1000000000087 1000000000090 
1000000000091 1000000000093 1000000000094 1000000000095 1000000000097 
1000000000099 1000000000101 1000000000102 1000000000103 1000000000105 
1000000000106 1000000000109 1000000000111 1000000000113 1000000000114 
1000000000115 1000000000117 1000000000118 1000000000119 1000000000121 
1000000000122 1000000000123 1000000000126 1000000000127 1000000000129 
1000000000130 1000000000133 1000000000135 1000000000137 1000000000138 
1000000000139 1000000000141 1000000000142 1000000000145 
Compare time taken without the table (rather with table on the fly):
cpu time: 283469 real time: 285225 gc time: 118039
61
608
6083
60794
607926
```



## REXX


```rexx
/*REXX program displays  square─free numbers  (integers > 1)  up to a specified limit.  */
numeric digits 20                                /*be able to handle larger numbers.    */
parse arg LO HI .                                /*obtain optional arguments from the CL*/
if LO=='' | LO==","  then LO=   1                /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI= 145                /* "      "         "   "   "     "    */
sw= linesize() - 1                               /*use one less than a full line.       */
# = 0                                            /*count of square─free numbers found.  */
$=                                               /*variable that holds a line of numbers*/
     do j=LO  to abs(HI)                         /*process all integers between LO & HI.*/
     if \isSquareFree(j)   then iterate          /*Not square─free?   Then skip this #. */
     #= # + 1                                    /*bump the count of square─free numbers*/
     if HI<0  then iterate                       /*Only counting 'em? Then look for more*/
     if length($ || j)<sw  then $= strip($ j)    /*append the number to the output list.*/
                           else do;   say $;   $=j;   end   /*display a line of numbers.*/
     end   /*j*/

if $\==''  then say $                            /*are there any residuals to display ? */
@theNum= 'The number of square─free numbers between '
if HI<0    then say @theNum  LO      " and "      abs(HI)       ' (inclusive)  is: '     #
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isSquareFree: procedure; parse arg x;  if x<1  then return 0  /*is the number too small?*/
              odd= x//2                          /*ODD=1   if X is odd,   ODD=0 if even.*/
                       do k=2+odd  to iSqrt(x)  by 1+odd /*use all numbers, or just odds*/
                       if x // k**2 == 0  then return 0  /*Is  X  divisible by a square?*/
                       end   /*k*/                       /* [↑]  Yes? Then ¬ square─free*/
              return 1                           /* [↑]       //  is REXX's ÷ remainder.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt: procedure; parse arg x;         q= 1;         do while q<=x;       q= q * 4
                                                     end   /*while q<=x*/
       r= 0
                  do while q>1;        q= q % 4;     _= x - r - q;        r= r % 2
                  if _>=0  then do;    x= _;         r= r + q;            end
                  end   /*while q>1*/
       return r                                  /*R  is the integer square root of  X. */
```

This REXX program makes use of   '''linesize'''   REXX program (or BIF)   which is used to determine the screen width (or linesize) of the terminal (console);   not all REXXes have this BIF.

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]]. 



{{out|output|text=  when using the default input:}}

(Shown at three-quarter size.)
<pre style="font-size:75%">
1 2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30 31 33 34 35 37 38 39 41 42 43 46 47 51 53 55 57 58 59 61 62 65 66 67 69 70 71 73 74 77 78 79 82 83
85 86 87 89 91 93 94 95 97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130 131 133 134 137 138 139 141 142 143 145

```

{{out|output|text=  when using the input of:     <tt> 1000000000000   1000000000145 </tt>}}

(Shown at three-quarter size.)
<pre style="font-size:75%">
1000000000001 1000000000002 1000000000003 1000000000005 1000000000006 1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
1000000000015 1000000000018 1000000000019 1000000000021 1000000000022 1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
1000000000033 1000000000037 1000000000038 1000000000039 1000000000041 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
1000000000049 1000000000051 1000000000054 1000000000055 1000000000057 1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
1000000000066 1000000000067 1000000000069 1000000000070 1000000000073 1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
1000000000082 1000000000085 1000000000086 1000000000087 1000000000090 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
1000000000099 1000000000101 1000000000102 1000000000103 1000000000105 1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
1000000000115 1000000000117 1000000000118 1000000000119 1000000000121 1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
1000000000130 1000000000133 1000000000135 1000000000137 1000000000138 1000000000139 1000000000141 1000000000142 1000000000145

```

{{out|output|text=  when using the (separate runs) inputs of:     <tt> 1   -100 </tt>   (and others)}} 

```txt

The number of square─free numbers between  1  and  100  (inclusive)  is:  61

The number of square─free numbers between  1  and  1000  (inclusive)  is:  608

The number of square─free numbers between  1  and  10000  (inclusive)  is:  6083

The number of square─free numbers between  1  and  100000  (inclusive)  is:  60794

The number of square─free numbers between  1  and  1000000  (inclusive)  is:  607926

```



## Ruby


```ruby
require "prime"

class Integer
  def square_free?
    prime_division.none?{|pr, exp| exp > 1}
  end
end

puts (1..145).select(&:square_free?).each_slice(20).map{|a| a.join(" ")}
puts

m = 10**12
puts (m..m+145).select(&:square_free?).each_slice(6).map{|a| a.join(" ")}
puts

markers = [100, 1000, 10_000, 100_000, 1_000_000]
count = 0
(1..1_000_000).each do |n|
  count += 1 if n.square_free?
  puts "#{count} square-frees upto #{n}" if markers.include?(n)
end 

```

{{out}}

```txt
1 2 3 5 6 7 10 11 13 14 15 17 19 21 22 23 26 29 30 31
33 34 35 37 38 39 41 42 43 46 47 51 53 55 57 58 59 61 62 65
66 67 69 70 71 73 74 77 78 79 82 83 85 86 87 89 91 93 94 95
97 101 102 103 105 106 107 109 110 111 113 114 115 118 119 122 123 127 129 130
131 133 134 137 138 139 141 142 143 145

1000000000001 1000000000002 1000000000003 1000000000005 1000000000006 1000000000007
1000000000009 1000000000011 1000000000013 1000000000014 1000000000015 1000000000018
1000000000019 1000000000021 1000000000022 1000000000023 1000000000027 1000000000029
1000000000030 1000000000031 1000000000033 1000000000037 1000000000038 1000000000039
1000000000041 1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
1000000000049 1000000000051 1000000000054 1000000000055 1000000000057 1000000000058
1000000000059 1000000000061 1000000000063 1000000000065 1000000000066 1000000000067
1000000000069 1000000000070 1000000000073 1000000000074 1000000000077 1000000000078
1000000000079 1000000000081 1000000000082 1000000000085 1000000000086 1000000000087
1000000000090 1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
1000000000099 1000000000101 1000000000102 1000000000103 1000000000105 1000000000106
1000000000109 1000000000111 1000000000113 1000000000114 1000000000115 1000000000117
1000000000118 1000000000119 1000000000121 1000000000122 1000000000123 1000000000126
1000000000127 1000000000129 1000000000130 1000000000133 1000000000135 1000000000137
1000000000138 1000000000139 1000000000141 1000000000142 1000000000145

61 square-frees upto 100
608 square-frees upto 1000
6083 square-frees upto 10000
60794 square-frees upto 100000
607926 square-frees upto 1000000


```



## Scala

This example uses a brute-force approach to check whether a number is square-free, and builds a lazily evaluated list of all square-free numbers with a simple filter. To get the large square-free numbers, it avoids computing the beginning of the list by starting the list at a given number.

```scala
import spire.math.SafeLong
import spire.implicits._

import scala.annotation.tailrec

object SquareFreeNums {
  def main(args: Array[String]): Unit = {
    println(
      s"""|1 - 145:
          |${formatTable(sqrFree.takeWhile(_ <= 145).toVector, 10)}
          |
          |1T - 1T+145:
          |${formatTable(sqrFreeInit(1000000000000L).takeWhile(_ <= 1000000000145L).toVector, 6)}
          |
          |Square-Free Counts...
          |100: ${sqrFree.takeWhile(_ <= 100).length}
          |1000: ${sqrFree.takeWhile(_ <= 1000).length}
          |10000: ${sqrFree.takeWhile(_ <= 10000).length}
          |100000: ${sqrFree.takeWhile(_ <= 100000).length}
          |1000000: ${sqrFree.takeWhile(_ <= 1000000).length}
          |""".stripMargin)
  }
  
  def chkSqr(num: SafeLong): Boolean = !LazyList.iterate(SafeLong(2))(_ + 1).map(_.pow(2)).takeWhile(_ <= num).exists(num%_ == 0)
  def sqrFreeInit(init: SafeLong): LazyList[SafeLong] = LazyList.iterate(init)(_ + 1).filter(chkSqr)
  def sqrFree: LazyList[SafeLong] = sqrFreeInit(1)
  
  def formatTable(lst: Vector[SafeLong], rlen: Int): String = {
    @tailrec
    def fHelper(ac: Vector[String], src: Vector[String]): String = {
      if(src.nonEmpty) fHelper(ac :+ src.take(rlen).mkString, src.drop(rlen))
      else ac.mkString("\n")
    }
    
    val maxLen = lst.map(n => f"${n.toBigInt}%,d".length).max
    val formatted = lst.map(n => s"%,${maxLen + 2}d".format(n.toBigInt))
    
    fHelper(Vector[String](), formatted)
  }
}
```


{{out}}

```txt
1 - 145:
    1    2    3    5    6    7   10   11   13   14
   15   17   19   21   22   23   26   29   30   31
   33   34   35   37   38   39   41   42   43   46
   47   51   53   55   57   58   59   61   62   65
   66   67   69   70   71   73   74   77   78   79
   82   83   85   86   87   89   91   93   94   95
   97  101  102  103  105  106  107  109  110  111
  113  114  115  118  119  122  123  127  129  130
  131  133  134  137  138  139  141  142  143  145

1T - 1T+145:
  1,000,000,000,001  1,000,000,000,002  1,000,000,000,003  1,000,000,000,005  1,000,000,000,006  1,000,000,000,007
  1,000,000,000,009  1,000,000,000,011  1,000,000,000,013  1,000,000,000,014  1,000,000,000,015  1,000,000,000,018
  1,000,000,000,019  1,000,000,000,021  1,000,000,000,022  1,000,000,000,023  1,000,000,000,027  1,000,000,000,029
  1,000,000,000,030  1,000,000,000,031  1,000,000,000,033  1,000,000,000,037  1,000,000,000,038  1,000,000,000,039
  1,000,000,000,041  1,000,000,000,042  1,000,000,000,043  1,000,000,000,045  1,000,000,000,046  1,000,000,000,047
  1,000,000,000,049  1,000,000,000,051  1,000,000,000,054  1,000,000,000,055  1,000,000,000,057  1,000,000,000,058
  1,000,000,000,059  1,000,000,000,061  1,000,000,000,063  1,000,000,000,065  1,000,000,000,066  1,000,000,000,067
  1,000,000,000,069  1,000,000,000,070  1,000,000,000,073  1,000,000,000,074  1,000,000,000,077  1,000,000,000,078
  1,000,000,000,079  1,000,000,000,081  1,000,000,000,082  1,000,000,000,085  1,000,000,000,086  1,000,000,000,087
  1,000,000,000,090  1,000,000,000,091  1,000,000,000,093  1,000,000,000,094  1,000,000,000,095  1,000,000,000,097
  1,000,000,000,099  1,000,000,000,101  1,000,000,000,102  1,000,000,000,103  1,000,000,000,105  1,000,000,000,106
  1,000,000,000,109  1,000,000,000,111  1,000,000,000,113  1,000,000,000,114  1,000,000,000,115  1,000,000,000,117
  1,000,000,000,118  1,000,000,000,119  1,000,000,000,121  1,000,000,000,122  1,000,000,000,123  1,000,000,000,126
  1,000,000,000,127  1,000,000,000,129  1,000,000,000,130  1,000,000,000,133  1,000,000,000,135  1,000,000,000,137
  1,000,000,000,138  1,000,000,000,139  1,000,000,000,141  1,000,000,000,142  1,000,000,000,145

Square-Free Counts...
100: 61
1000: 608
10000: 6083
100000: 60794
1000000: 607926
```



## Sidef

In Sidef, the functions ''is_square_free(n)'' and ''square_free_count(min, max)'' are built-in. However, we can very easily reimplement them in Sidef code, as fast integer factorization methods are also available in the language.


```ruby
func is_square_free(n) {

    n.abs!       if (n <  0)
    return false if (n == 0)

    n.factor_exp + [[1,1]] -> all { .[1] == 1 }
}

func square_free_count(n) {
    1 .. n.isqrt -> sum {|k|
        moebius(k) * idiv(n, k*k)
    }
}

func display_results(a, c, f = { _ }) {
    a.each_slice(c, {|*s|
        say s.map(f).join(' ')
    })
}

var a = range(   1,      145).grep {|n| is_square_free(n) }
var b = range(1e12, 1e12+145).grep {|n| is_square_free(n) }

say "There are #{a.len} square─free numbers between 1 and 145:"
display_results(a, 17, {|n| "%3s" % n })

say "\nThere are #{b.len} square─free numbers between 10^12 and 10^12 + 145:"
display_results(b, 5)
say ''

for (2 .. 6) { |n|
    var c = square_free_count(10**n)
    say "The number of square─free numbers between 1 and 10^#{n} (inclusive) is: #{c}"
}
```


{{out}}

```txt

There are 90 square─free numbers between 1 and 145:
  1   2   3   5   6   7  10  11  13  14  15  17  19  21  22  23  26
 29  30  31  33  34  35  37  38  39  41  42  43  46  47  51  53  55
 57  58  59  61  62  65  66  67  69  70  71  73  74  77  78  79  82
 83  85  86  87  89  91  93  94  95  97 101 102 103 105 106 107 109
110 111 113 114 115 118 119 122 123 127 129 130 131 133 134 137 138
139 141 142 143 145

There are 89 square─free numbers between 10^12 and 10^12 + 145:
1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
1000000000139 1000000000141 1000000000142 1000000000145

The number of square─free numbers between 1 and 10^2 (inclusive) is: 61
The number of square─free numbers between 1 and 10^3 (inclusive) is: 608
The number of square─free numbers between 1 and 10^4 (inclusive) is: 6083
The number of square─free numbers between 1 and 10^5 (inclusive) is: 60794
The number of square─free numbers between 1 and 10^6 (inclusive) is: 607926

```



## zkl


```zkl
const Limit=1 + (1e12 + 145).sqrt();	// 1000001 because it fits this task
var [const] 
   BI=Import.lib("zklBigNum"),    // GNU Multiple Precision Arithmetic Library
   primes=List.createLong(Limit); // one big allocate (vs lots of allocs)

// GMP provide nice way to generate primes, nextPrime is in-place
p:=BI(0); while(p<Limit){ primes.append(p.nextPrime().toInt()); } // 78,499 primes

fcn squareFree(start,end,save=False){ //-->(cnt,list|n)
   sink := Sink(if(save) List else Void);  // Sink(Void) is one item sink
   cnt, numPrimes := 0, (end - start).toFloat().sqrt().toInt() - 1;
   foreach n in ([start..end]){
      foreach j in ([0..numPrimes]){
         p,p2 := primes[j], p*p;
	 if(p2>n) break;
	 if(n%p2==0) continue(2);  // -->foreach n
      }
      sink.write(n); cnt+=1
   }
   return(cnt,sink.close());
}
```


```zkl
println("Square-free integers from 1 to 145:");
squareFree(1,145,True)[1].pump(Console.println,
   T(Void.Read,14,False),fcn{ vm.arglist.apply("%4d ".fmt).concat() });

println("\nSquare-free integers from 1000000000000 to 1000000000145:");
squareFree(1000000000000,1000000000145,True)[1].pump(Console.println,
   T(Void.Read,4,False),fcn{ vm.arglist.concat(" ") });
```

{{out}}
<pre style="height:35ex">
Square-free integers from 1 to 145:
   1    2    3    5    6    7   10   11   13   14   15   17   19   21   22 
  23   26   29   30   31   33   34   35   37   38   39   41   42   43   46 
  47   51   53   55   57   58   59   61   62   65   66   67   69   70   71 
  73   74   77   78   79   82   83   85   86   87   89   91   93   94   95 
  97  101  102  103  105  106  107  109  110  111  113  114  115  118  119 
 122  123  127  129  130  131  133  134  137  138  139  141  142  143  145 

Square-free integers from 1000000000000 to 1000000000145:
1000000000001 1000000000002 1000000000003 1000000000005 1000000000006
1000000000007 1000000000009 1000000000011 1000000000013 1000000000014
1000000000015 1000000000018 1000000000019 1000000000021 1000000000022
1000000000023 1000000000027 1000000000029 1000000000030 1000000000031
1000000000033 1000000000037 1000000000038 1000000000039 1000000000041
1000000000042 1000000000043 1000000000045 1000000000046 1000000000047
1000000000049 1000000000051 1000000000054 1000000000055 1000000000057
1000000000058 1000000000059 1000000000061 1000000000063 1000000000065
1000000000066 1000000000067 1000000000069 1000000000070 1000000000073
1000000000074 1000000000077 1000000000078 1000000000079 1000000000081
1000000000082 1000000000085 1000000000086 1000000000087 1000000000090
1000000000091 1000000000093 1000000000094 1000000000095 1000000000097
1000000000099 1000000000101 1000000000102 1000000000103 1000000000105
1000000000106 1000000000109 1000000000111 1000000000113 1000000000114
1000000000115 1000000000117 1000000000118 1000000000119 1000000000121
1000000000122 1000000000123 1000000000126 1000000000127 1000000000129
1000000000130 1000000000133 1000000000135 1000000000137 1000000000138
1000000000139 1000000000141 1000000000142 1000000000145

```



```zkl
n:=100; do(5){ 
   squareFree(1,n)[0]:
      println("%,9d square-free integers from 1 to %,d".fmt(_,n));
   n*=10;
}
```

{{out}}

```txt

       61 square-free integers from 1 to 100
      608 square-free integers from 1 to 1,000
    6,083 square-free integers from 1 to 10,000
   60,794 square-free integers from 1 to 100,000
  607,926 square-free integers from 1 to 1,000,000

```

