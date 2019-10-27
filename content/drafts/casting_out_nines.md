+++
title = "Casting out nines"
description = ""
date = 2019-06-02T12:39:12Z
aliases = []
[extra]
id = 11905
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Checksums]]

;Task   (in three parts):


;Part 1
Write a procedure (say <math>\mathit{co9}(x)</math>) which implements [http://mathforum.org/library/drmath/view/55926.html Casting Out Nines] as described by returning the checksum for <math>x</math>. Demonstrate the procedure using the examples given there, or others you may consider lucky.

;Part 2
Notwithstanding past Intel microcode errors, checking computer calculations like this would not be sensible. To find a computer use for your procedure:
: Consider the statement "318682 is 101558 + 217124 and squared is 101558217124" (see: [[Kaprekar numbers#Casting Out Nines (fast)]]).
: note that <math>318682</math> has the same checksum as (<math>101558 + 217124</math>);
: note that <math>101558217124</math> has the same checksum as (<math>101558 + 217124</math>) because for a Kaprekar they are made up of the same digits (sometimes with extra zeroes);
: note that this implies that for Kaprekar numbers the checksum of <math>k</math> equals the checksum of <math>k^2</math>.

Demonstrate that your procedure can be used to generate or filter a range of numbers with the property <math>\mathit{co9}(k) = \mathit{co9}(k^2)</math> and show that this subset is a small proportion of the range and contains all the Kaprekar in the range.

;Part 3
Considering [http://mathworld.wolfram.com/CastingOutNines.html this MathWorld page], produce a efficient algorithm based on the more mathematical treatment of Casting Out Nines, and realizing:
: <math>\mathit{co9}(x)</math> is the residual of <math>x</math> mod <math>9</math>;
: the procedure can be extended to bases other than 9.

Demonstrate your algorithm by generating or filtering a range of numbers with the property <math>k%(\mathit{Base}-1) == (k^2)%(\mathit{Base}-1)</math> and show that this subset is a small proportion of the range and contains all the Kaprekar in the range.


;related tasks
* [[First perfect square in base N with N unique digits]]
* [[Kaprekar numbers]]



## 360 Assembly

{{trans|REXX}}
The program uses two ASSIST macros (XDECO,XPRNT) to keep the code as short as possible. 

```360asm
*      Casting out nines         08/02/2017
CASTOUT CSECT
       USING  CASTOUT,R13        base register
       B      72(R15)            skip savearea
       DC     17F'0'             savearea
       STM    R14,R12,12(R13)    prolog
       ST     R13,4(R15)         " <-
       ST     R15,8(R13)         " ->
       LR     R13,R15            " addressability
       L      R1,LOW             low
       XDECO  R1,XDEC            edit low
       MVC    PGT+4(4),XDEC+8    output low
       L      R1,HIGH            high 
       XDECO  R1,XDEC            edit high
       MVC    PGT+12(4),XDEC+8   output low
       L      R1,BASE            base
       XDECO  R1,XDEC            edit base
       MVC    PGT+24(4),XDEC+8   output base
       XPRNT  PGT,L'PGT          print buffer
       L      R2,BASE            base
       BCTR   R2,0               -1
       ST     R2,RM              rm=base-1
       LA     R8,PG              ipg=0
       SR     R7,R7              j=0
       L      R6,LOW             i=low
       DO WHILE=(C,R6,LE,HIGH)   do i=low to high
         LR     R5,R6              i
         SR     R4,R4              clear for div
         D      R4,RM              /rm
         LR     R2,R4              r2=i mod rm
         LR     R5,R6              i
         MR     R4,R6              i*i
         SR     R4,R4              clear for div
         D      R4,RM              /rm
         IF CR,R2,EQ,R4 THEN       if (i//rm)=(i*i//rm) then
           LA     R7,1(R7)           j=j+1
           XDECO  R6,XDEC            edit i
           MVC    0(4,R8),XDEC+8     output i
           LA     R8,4(R8)           ipg=ipg+4
           IF C,R7,EQ,=F'20' THEN    if j=20 then
             XPRNT  PG,L'PG            print buffer
             LA     R8,PG              ipg=0
             SR     R7,R7              j=0
             MVC    PG,=CL80' '        clear buffer
           ENDIF  ,                  end if
         ENDIF  ,                  end if
         LA     R6,1(R6)           i=i+1
       ENDDO  ,                  end do i
       IF LTR,R7,NE,R7 THEN      if j<>0  then
         XPRNT  PG,L'PG            print buffer
       ENDIF  ,                  end if  
       L      R13,4(0,R13)       epilog 
       LM     R14,R12,12(R13)    " restore
       XR     R15,R15            " rc=0
       BR     R14                exit
LOW    DC     F'1'               low
HIGH   DC     F'500'             high
BASE   DC     F'10'              base
RM     DS     F                  rm
PGT    DC     CL80'for  ... to  ...    base ...'    buffer
PG     DC     CL80' '                               buffer
XDEC   DS     CL12               temp for xdeco
       YREGS
       END    CASTOUT
```

{{out}}

```txt

for    1 to  500    base  10
   1   9  10  18  19  27  28  36  37  45  46  54  55  63  64  72  73  81  82  90
  91  99 100 108 109 117 118 126 127 135 136 144 145 153 154 162 163 171 172 180
 181 189 190 198 199 207 208 216 217 225 226 234 235 243 244 252 253 261 262 270
 271 279 280 288 289 297 298 306 307 315 316 324 325 333 334 342 343 351 352 360
 361 369 370 378 379 387 388 396 397 405 406 414 415 423 424 432 433 441 442 450
 451 459 460 468 469 477 478 486 487 495 496

```



## C++


### Filter


```cpp
// Casting Out Nines
//
// Nigel Galloway. June 24th., 2012
//
#include <iostream>
int main() {
	int Base = 10;
	const int N = 2;
	int c1 = 0;
	int c2 = 0;
	for (int k=1; k<pow((double)Base,N); k++){
		c1++;
		if (k%(Base-1) == (k*k)%(Base-1)){
			c2++;
			std::cout << k << " ";
		}
	}
	std::cout << "\nTrying " << c2 << " numbers instead of " << c1 << " numbers saves " << 100 - ((double)c2/c1)*100 << "%" <<std::endl;
	return 0;
}
```

{{out|Produces}}

```txt

1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99
Trying 22 numbers instead of 99 numbers saves 77.7778%

```

The kaprekar numbers in this range 1 9 45 55 and 99 are included.

Changing: "<code>int Base = 16;</code>" Produces:

```txt

1 6 10 15 16 21 25 30 31 36 40 45 46 51 55 60 61 66 70 75 76 81 85 90 91 96 100
105 106 111 115 120 121 126 130 135 136 141 145 150 151 156 160 165 166 171 175
180 181 186 190 195 196 201 205 210 211 216 220 225 226 231 235 240 241 246 250
255
Trying 68 numbers instead of 255 numbers saves 73.3333%

```

The kaprekar numbers:
:1   is 1
:6   is 6
:a   is 10
:f   is 15
:33  is 51
:55  is 85
:5b  is 91
:78  is 120
:88  is 136
:ab  is 171
:cd  is 205
:ff  is 255

in this range are included.

Changing: "<code>int Base = 17;</code>" Produces:

```txt

1 16 17 32 33 48 49 64 65 80 81 96 97 112 113 128 129 144 145 160 161 176 177 19
2 193 208 209 224 225 240 241 256 257 272 273 288
Trying 36 numbers instead of 288 numbers saves 87.5%

```

The kaprekar numbers:
:1 is 1
:g is 16
:3d is 64
:d4 is 225
:gg is 288

in this range are included.

### C++11 For Each Generator


```cpp
// Casting Out Nines Generator - Compiles with gcc4.6, MSVC 11, and CLang3
//
// Nigel Galloway. June 24th., 2012
//
#include <iostream>
#include <vector>
struct ran {
	const int base;
	std::vector<int> rs;
	ran(const int base) : base(base) { for (int nz=0; nz<base-1; nz++) if(nz*(nz-1)%(base-1) == 0) rs.push_back(nz); }
};
class co9 {
private:
	const ran* _ran;
	const int _end;
	int _r,_x,_next;
public:
	bool operator!=(const co9& other) const {return operator*() <= _end;}
	co9 begin() const {return *this;}
        co9 end() const {return *this;}
	int operator*() const {return _next;}
	co9(const int start, const int end, const ran* r)
	:_ran(r)
	,_end(end)
	,_r(1)
	,_x(start/_ran->base)
	,_next((_ran->base-1)*_x + _ran->rs[_r])
	{
		while (operator*() < start) operator++();
	}
	const co9& operator++() {
		const int oldr = _r;
		_r = ++_r%_ran->rs.size();
		if (_r<oldr) _x++;
		_next = (_ran->base-1)*_x + _ran->rs[_r];
		return *this;
	}
};

int main() {
	ran r(10);
	for (int i : co9(1,99,&r)) { std::cout << i << ' '; }
	return 0;
}
```

{{out|Produces}}

```txt

1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 

```

An alternative implementation for struct ran using http://rosettacode.org/wiki/Sum_digits_of_an_integer#C.2B.2B which produces the same result is:

```cpp
struct ran {
	const int base;
	std::vector<int> rs;
	ran(const int base) : base(base) { for (int nz=0; nz<base-1; nz++) if(SumDigits(nz) == SumDigits(nz*nz)) rs.push_back(nz); }
};
```

Changing main:

```cpp
int main() {
	ran r(16);
	for (int i : co9(1,255,&r)) { std::cout << i << ' '; }
	return 0;
}
```

{{out|Produces}}

```txt

1 6 10 15 16 21 25 30 31 36 40 45 46 51 55 60 61 66 70 75 76 81 85 90 91 96 100 105 106 111 115 120 121 126 130 135 136 141 145 150 151 156 160 165 166 171 175 180 181 186 190 195 196 201 205 210 211 216 220 225 226 231 235 240 241 246 250 255 

```

Changing main:

```cpp
int main() {
	ran r(17);
	for (int i : co9(1,288,&r)) { std::cout << i << ' '; }
	return 0;
}
```

{{out|Produces}}

```txt

1 16 17 32 33 48 49 64 65 80 81 96 97 112 113 128 129 144 145 160 161 176 177 192 193 208 209 224 225 240 241 256 257 272 273 288 

```



## Common Lisp


```lisp
;;A macro was used to ensure that the filter is inlined.  
;;Larry Hignight.  Last updated on 7/3/2012.
(defmacro kaprekar-number-filter (n &optional (base 10))
  `(= (mod ,n (1- ,base)) (mod (* ,n ,n) (1- ,base))))

(defun test (&key (start 1) (stop 10000) (base 10) (collect t))
  (let ((count 0)
	(nums))
    (loop for i from start to stop do
	  (when (kaprekar-number-filter i base)
	    (if collect (push i nums))
	    (incf count)))
    (format t "~d potential Kaprekar numbers remain (~~~$% filtered out).~%"
	    count (* (/ (- stop count) stop) 100))
    (if collect (reverse nums))))
```

{{out}}

```txt
CL-USER> (test :stop 99)
22 potential Kaprekar numbers remain (~77.78% filtered out).
(1 9 10 18 19 27 28 36 37 45 ...)
CL-USER> (test :stop 10000 :collect nil)
2223 potential Kaprekar numbers remain (~77.77% filtered out).
NIL
CL-USER> (test :stop 1000000 :collect nil)
222223 potential Kaprekar numbers remain (~77.78% filtered out).
NIL
CL-USER> (test :stop 255 :base 16)
68 potential Kaprekar numbers remain (~73.33% filtered out).
(1 6 10 15 16 21 25 30 31 36 ...)
CL-USER> (test :stop 288 :base 17)
36 potential Kaprekar numbers remain (~87.50% filtered out).
(1 16 17 32 33 48 49 64 65 80 ...)
```



## D

{{trans|Python}}

```d
import std.stdio, std.algorithm, std.range;

uint[] castOut(in uint base=10, in uint start=1, in uint end=999999) {
    auto ran = iota(base - 1)
               .filter!(x => x % (base - 1) == (x * x) % (base - 1));
    auto x = start / (base - 1);
    immutable y = start % (base - 1);

    typeof(return) result;
    while (true) {
        foreach (immutable n; ran) {
            immutable k = (base - 1) * x + n;
            if (k < start)
                continue;
            if (k > end)
                return result;
            result ~= k;
        }
        x++;
    }
}

void main() {
    castOut(16, 1, 255).writeln;
    castOut(10, 1, 99).writeln;
    castOut(17, 1, 288).writeln;
}
```

{{out|Output (some newlines added)}}

```txt
[1, 6, 10, 15, 16, 21, 25, 30, 31, 36, 40, 45, 46, 51, 55, 60,
 61, 66, 70, 75, 76, 81, 85, 90, 91, 96, 100, 105, 106, 111, 115,
 120, 121, 126, 130, 135, 136, 141, 145, 150, 151, 156, 160, 165,
 166, 171, 175, 180, 181, 186, 190, 195, 196, 201, 205, 210, 211,
 216, 220, 225, 226, 231, 235, 240, 241, 246, 250, 255]
[1, 9, 10, 18, 19, 27, 28, 36, 37, 45, 46, 54, 55, 63, 64, 72, 73,
 81, 82, 90, 91, 99]
[1, 16, 17, 32, 33, 48, 49, 64, 65, 80, 81, 96, 97, 112, 113, 128,
 129, 144, 145, 160, 161, 176, 177, 192, 193, 208, 209, 224, 225,
 240, 241, 256, 257, 272, 273, 288]
```



## Free Pascal


```pascal
program castout9;
{$ifdef fpc}{$mode delphi}{$endif}
uses generics.collections;
type
  TIntegerList = TSortedList<integer>;
  
procedure co9(const start,base,lim:integer;kaprekars:array of integer);
var
  C1:integer = 0;
  C2:integer = 0;
  S:TIntegerlist;
  k,i:integer;
begin
  S:=TIntegerlist.Create;
  for k := start to lim do
  begin
    inc(C1);
    if k mod (base-1) = (k*k) mod (base-1) then
    begin
      inc(C2);
      S.Add(k);
    end;
  end;
  writeln('Valid subset: ');
  for i in Kaprekars do
    if not s.contains(i) then
      writeln('invalid ',i);
  
  for i in s do write(i:4);
  writeln;
  write('The Kaprekars in this range [');
  for i in kaprekars do write(i:4);
  writeln('] are included');  
  writeln('Trying ',C2, ' numbers instead of ', C1,' saves ',100-(C2 * 100 /C1):3:2,',%.');
  writeln;
  S.Free;
end;

begin 
  co9(1, 10, 99, [1,9,45,55,99]);
  co9(1, 10, 1000, [1,9,45,55,99,297,703,999]);
end.
```


```txt

Output:
Valid subset: 
   1   9  10  18  19  27  28  36  37  45  46  54  55  63  64  72  73  81  82  90  91  99
The Kaprekars in this range [   1   9  45  55  99] are included
Trying 22 numbers instead of 99 saves 77.78,%.

Valid subset: 
   1   9  10  18  19  27  28  36  37  45  46  54  55  63  64  72  73  81  82  90  91  99 100 108 109 117 118 126 127 135 136 144 145 153 154 162 163 171 172 180 181 189 190 198 199 207 208 216 217 225 226 234 235 243 244 252 253 261 262 270 271 279 280 288 289 297 298 306 307 315 316 324 325 333 334 342 343 351 352 360 361 369 370 378 379 387 388 396 397 405 406 414 415 423 424 432 433 441 442 450 451 459 460 468 469 477 478 486 487 495 496 504 505 513 514 522 523 531 532 540 541 549 550 558 559 567 568 576 577 585 586 594 595 603 604 612 613 621 622 630 631 639 640 648 649 657 658 666 667 675 676 684 685 693 694 702 703 711 712 720 721 729 730 738 739 747 748 756 757 765 766 774 775 783 784 792 793 801 802 810 811 819 820 828 829 837 838 846 847 855 856 864 865 873 874 882 883 891 892 900 901 909 910 918 919 927 928 936 937 945 946 954 955 963 964 972 973 981 982 990 991 9991000
The Kaprekars in this range [   1   9  45  55  99 297 703 999] are included
Trying 223 numbers instead of 1000 saves 77.70,%.

```


## Go


```go
package main

import (
    "fmt"
    "log"
    "strconv"
)

// A casting out nines algorithm.

// Quoting from: http://mathforum.org/library/drmath/view/55926.html
/*
First, for any number we can get a single digit, which I will call the 
"check digit," by repeatedly adding the digits. That is, we add the 
digits of the number, then if there is more than one digit in the 
result we add its digits, and so on until there is only one digit 
left.

...

You may notice that when you add the digits of 6395, if you just 
ignore the 9, and the 6+3 = 9, you still end up with 5 as your check 
digit. This is because any 9's make no difference in the result. 
That's why the process is called "casting out" nines. Also, at any 
step in the process, you can add digits, not just at the end: to do 
8051647, I can say 8 + 5 = 13, which gives 4; plus 1 is 5, plus 6 is 
11, which gives 2, plus 4 is 6, plus 7 is 13 which gives 4. I never 
have to work with numbers bigger than 18.
*/
// The twist is that co9Peterson returns a function to do casting out nines
// in any specified base from 2 to 36.
func co9Peterson(base int) (cob func(string) (byte, error), err error) {
    if base < 2 || base > 36 {
        return nil, fmt.Errorf("co9Peterson: %d invalid base", base)
    }
    // addDigits adds two digits in the specified base.
    // People perfoming casting out nines by hand would usually have their
    // addition facts memorized.  In a program, a lookup table might be
    // analogous, but we expediently use features of the programming language
    // to add digits in the specified base.
    addDigits := func(a, b byte) (string, error) {
        ai, err := strconv.ParseInt(string(a), base, 64)
        if err != nil {
            return "", err
        }
        bi, err := strconv.ParseInt(string(b), base, 64)
        if err != nil {
            return "", err
        }
        return strconv.FormatInt(ai+bi, base), nil
    }
    // a '9' in the specified base.  that is, the greatest digit.
    s9 := strconv.FormatInt(int64(base-1), base)
    b9 := s9[0]
    // define result function.  The result function may return an error
    // if n is not a valid number in the specified base.
    cob = func(n string) (r byte, err error) {
        r = '0'
        for i := 0; i < len(n); i++ { // for each digit of the number
            d := n[i]
            switch {
            case d == b9: // if the digit is '9' of the base, cast it out
                continue
            // if the result so far is 0, the digit becomes the result
            case r == '0':
                r = d
                continue
            }
            // otherwise, add the new digit to the result digit
            s, err := addDigits(r, d)
            if err != nil {
                return 0, err
            }
            switch {
            case s == s9: // if the sum is "9" of the base, cast it out
                r = '0'
                continue
            // if the sum is a single digit, it becomes the result
            case len(s) == 1:
                r = s[0]
                continue
            }
            // otherwise, reduce this two digit intermediate result before
            // continuing.
            r, err = cob(s)
            if err != nil {
                return 0, err
            }
        }
        return
    }
    return
}

// Subset code required by task.  Given a base and a range specified with
// beginning and ending number in that base, return candidate Kaprekar numbers
// based on the observation that k%(base-1) must equal (k*k)%(base-1).
// For the % operation, rather than the language built-in operator, use
// the method of casting out nines, which in fact implements %(base-1).
func subset(base int, begin, end string) (s []string, err error) {
    // convert begin, end to native integer types for easier iteration
    begin64, err := strconv.ParseInt(begin, base, 64)
    if err != nil {
        return nil, fmt.Errorf("subset begin: %v", err)
    }
    end64, err := strconv.ParseInt(end, base, 64)
    if err != nil {
        return nil, fmt.Errorf("subset end: %v", err)
    }
    // generate casting out nines function for specified base
    cob, err := co9Peterson(base)
    if err != nil {
        return
    }
    for k := begin64; k <= end64; k++ {
        ks := strconv.FormatInt(k, base)
        rk, err := cob(ks)
        if err != nil { // assertion
            panic(err) // this would indicate a bug in subset
        }
        rk2, err := cob(strconv.FormatInt(k*k, base))
        if err != nil { // assertion
            panic(err) // this would indicate a bug in subset
        }
        // test for candidate Kaprekar number
        if rk == rk2 {
            s = append(s, ks)
        }
    }
    return
}

var testCases = []struct {
    base       int
    begin, end string
    kaprekar   []string
}{
    {10, "1", "100", []string{"1", "9", "45", "55", "99"}},
    {17, "10", "gg", []string{"3d", "d4", "gg"}},
}
    
func main() {
    for _, tc := range testCases {
        fmt.Printf("\nTest case base = %d, begin = %s, end = %s:\n",
            tc.base, tc.begin, tc.end)
        s, err := subset(tc.base, tc.begin, tc.end)
        if err != nil {
            log.Fatal(err)
        }
        fmt.Println("Subset:  ", s)
        fmt.Println("Kaprekar:", tc.kaprekar)
        sx := 0
        for _, k := range tc.kaprekar {
            for {
                if sx == len(s) {
                    fmt.Printf("Fail:", k, "not in subset")
                    return
                }
                if s[sx] == k {
                    sx++
                    break
                }
                sx++
            }
        }
        fmt.Println("Valid subset.")
    }
}
```

{{out}}

```txt

Test case base = 10, begin = 1, end = 100:
Subset:   [1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 100]
Kaprekar: [1 9 45 55 99]
Valid subset.

Test case base = 17, begin = 10, end = gg:
Subset:   [10 1f 1g 2e 2f 3d 3e 4c 4d 5b 5c 6a 6b 79 7a 88 89 97 98 a6 a7 b5 b6
 c4 c5 d3 d4 e2 e3 f1 f2 g0 g1 gg]
Kaprekar: [3d d4 gg]
Valid subset.

```



## Haskell


```haskell
co9 n
  | n <= 8 = n
  | otherwise = co9 $ sum $ filter (/= 9) $ digits 10 n

task2 = filter (\n -> co9 n == co9 (n ^ 2)) [1 .. 100]

task3 k = filter (\n -> n `mod` k == n ^ 2 `mod` k) [1 .. 100]
```


Auxillary function, returning digits of a number for given base

```haskell
digits base = map (`mod` base) . takeWhile (> 0) . iterate (`div` base)
```


or using unfolding:


```haskell
digits base = Data.List.unfoldr modDiv
  where modDiv 0 = Nothing
        modDiv n = let (q, r) = (n `divMod` base) in Just (r, q)
```

 
'''Output'''


```txt
λ> co9 232345
1
λ> co9 34234234
7
λ> co9 (232345 + 34234234) == co9 232345 + co9 34234234
True
λ> co9 (232345 * 34234234) == co9 232345 * co9 34234234
True
λ> task2
[1,9,10,18,19,27,28,36,37,45,46,54,55,63,64,72,73,81,82,90,91,99,100]
λ> task2 == (task3 9)
True
λ> task3 16
[1,16,17,32,33,48,49,64,65,80,81,96,97]
```


Finally it is possible to test usefull properties of <code>co9</code> with QuickCheck:


```txt
λ> :m Test.QuickCheck
λ> quickCheck (\a -> a > 0 ==> co9 a == a `mod` 9)
+++ OK, passed 100 tests.
λ> quickCheck (\a b -> a > 0 && b > 0 ==> co9 (co9 a + co9 b) == co9 (a+b))
+++ OK, passed 100 tests.
λ> quickCheck (\a b -> a > 0 && b > 0 ==> co9 (co9 a * co9 b) == co9 (a*b))
+++ OK, passed 100 tests.
```



## J

This is an implementation of: "given two numbers which mark the beginning and end of a range of integers, and another number which denotes an integer base, return numbers from within the range where the number is equal (modulo the base minus 1) to its square".  At the time of this writing, this task is a draft task and this description does not precisely match the task description on this page.  Eventually, either the task description will change to match this implementation (which means this paragraph should be removed) or the task description will change to conflict with this implementation (so this entire section should be re-written).

```J
castout=: 1 :0
   [: (#~  ] =&((m-1)&|) *:) <. + [: i. (+*)@-~
)
```

Example use:

```J
   0 (10 castout) 100
0 1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 100
```

Alternate implementation:

```J
castout=: 1 :0
   [: (#~  0 = (m-1) | 0 _1 1&p.) <. + [: i. (+*)@-~
)
```

Note that about half of the code here is the code that implements "range of numbers".  If we factor that out, and represent the desired values directly the code becomes much simpler:

```J
   (#~ 0=9|0 _1 1&p.) i.101
0 1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 100
   (#~  ] =&(9&|) *:) i. 101
0 1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 100
```

And, of course, we can name parts of these expressions.  For example:

```J
   (#~  ] =&(co9=: 9&|) *:) i. 101
0 1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 100
```

Or, if you prefer:

```J
co9=: 9&|
   (#~  ] =&co9 *:) i. 101
0 1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 100
```



## Java

{{trans|D}}
{{works with|Java|8}}

```java
import java.util.*;
import java.util.stream.IntStream;

public class CastingOutNines {

    public static void main(String[] args) {
        System.out.println(castOut(16, 1, 255));
        System.out.println(castOut(10, 1, 99));
        System.out.println(castOut(17, 1, 288));
    }

    static List<Integer> castOut(int base, int start, int end) {
        int[] ran = IntStream
                .range(0, base - 1)
                .filter(x -> x % (base - 1) == (x * x) % (base - 1))
                .toArray();

        int x = start / (base - 1);

        List<Integer> result = new ArrayList<>();
        while (true) {
            for (int n : ran) {
                int k = (base - 1) * x + n;
                if (k < start)
                    continue;
                if (k > end)
                    return result;
                result.add(k);
            }
            x++;
        }
    }
}
```



```txt
[1, 6, 10, 15, 16, 21, 25, 30, 31, 36, 40, 45, 46, 51, 55, 60, 61, 66, 
70, 75, 76, 81, 85, 90, 91, 96, 100, 105, 106, 111, 115, 120, 121, 126, 130, 
135, 136, 141, 145, 150, 151, 156, 160, 165, 166, 171, 175, 180, 181, 186, 
190, 195, 196, 201, 205, 210, 211, 216, 220, 225, 226, 231, 235, 240, 241, 246, 250, 255]
[1, 9, 10, 18, 19, 27, 28, 36, 37, 45, 46, 54, 55, 63, 64, 72, 73, 81, 82, 90, 91, 99]
[1, 16, 17, 32, 33, 48, 49, 64, 65, 80, 81, 96, 97, 112, 113, 128, 129, 144, 145, 160,
 161, 176, 177, 192, 193, 208, 209, 224, 225, 240, 241, 256, 257, 272, 273, 288]
```



## JavaScript


### ES5

Assuming the context of a web page:

```JavaScript
function main(s, e, bs, pbs) {
    bs = bs || 10;
    pbs = pbs || 10
    document.write('start:', toString(s), ' end:', toString(e),
        ' base:', bs, ' printBase:', pbs)
    document.write('
castOutNine: ');
    castOutNine()
    document.write('
kaprekar: ');
    kaprekar()
    document.write('

')

    function castOutNine() {
        for (var n = s, k = 0, bsm1 = bs - 1; n <= e; n += 1)
            if (n % bsm1 == (n * n) % bsm1) k += 1,
                document.write(toString(n), ' ')
        document.write('
trying ', k, ' numbers instead of ', n = e - s + 1,
            ' numbers saves ', (100 - k / n * 100)
            .toFixed(3), '%')
    }

    function kaprekar() {
        for (var n = s; n <= e; n += 1)
            if (isKaprekar(n)) document.write(toString(n), ' ')

        function isKaprekar(n) {
            if (n < 1) return false
            if (n == 1) return true
            var s = (n * n)
                .toString(bs)
            for (var i = 1, e = s.length; i < e; i += 1) {
                var a = parseInt(s.substr(0, i), bs)
                var b = parseInt(s.substr(i), bs)
                if (b && a + b == n) return true
            }
            return false
        }
    }

    function toString(n) {
        return n.toString(pbs)
            .toUpperCase()
    }
}
main(1, 10 * 10 - 1)
main(1, 16 * 16 - 1, 16)
main(1, 17 * 17 - 1, 17)
main(parseInt('10', 17), parseInt('gg', 17), 17, 17)
```

{{Out}}

```txt
start:1 end:99 base:10 printBase:10
castOutNine: 1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 
trying 22 numbers instead of 99 numbers saves 77.778%
kaprekar: 1 9 45 55 99 

start:1 end:255 base:16 printBase:10
castOutNine: 1 6 10 15 16 21 25 30 31 36 40 45 46 51 55 60 61 66 70 75 76 81 85 90 91 96 100 105 106 111 115 120 121 126 130 135 136
141 145 150 151 156 160 165 166 171 175 180 181 186 190 195 196 201 205 210 211 216 220 225 226 231 235 240 241 246 250 255 
trying 68 numbers instead of 255 numbers saves 73.333%
kaprekar: 1 6 10 15 51 85 91 120 136 171 205 255 

start:1 end:288 base:17 printBase:10
castOutNine: 1 16 17 32 33 48 49 64 65 80 81 96 97 112 113 128 129 144 145 160 161 176 177 192 193 208 209 224 225 240 241 256 257 272 273 288 
trying 36 numbers instead of 288 numbers saves 87.500%
kaprekar: 1 16 64 225 288 

start:10 end:GG base:17 printBase:17
castOutNine: 10 1F 1G 2E 2F 3D 3E 4C 4D 5B 5C 6A 6B 79 7A 88 89 97 98 A6 A7 B5 B6 C4 C5 D3 D4 E2 E3 F1 F2 G0 G1 GG 
trying 34 numbers instead of 272 numbers saves 87.500%
kaprekar: 3D D4 GG 
```



### ES6

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // co9 :: Int -> Int
    const co9 = n =>
        n <= 8 ? n : co9(
            digits(10, n)
            .reduce((a, x) => x !== 9 ? a + x : a, 0)
        );

    // GENERIC FUNCTIONS

    // digits :: Int -> Int -> [Int]
    const digits = (base, n) => {
        if (n < base) return [n];
        const [q, r] = quotRem(n, base);
        return [r].concat(digits(base, q));
    };

    // quotRem :: Integral a => a -> a -> (a, a)
    const quotRem = (m, n) => [Math.floor(m / n), m % n];

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // squared :: Num a => a -> a
    const squared = n => Math.pow(n, 2);

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    // TESTS
    return show({
        test1: co9(232345), //-> 1
        test2: co9(34234234), //-> 7
        test3: co9(232345 + 34234234) === co9(232345) + co9(34234234), //-> true
        test4: co9(232345 * 34234234) === co9(232345) * co9(34234234), //-> true,
        task2: range(1, 100)
            .filter(n => co9(n) === co9(squared(n))),
        task3: (k => range(1, 100)
            .filter(n => (n % k) === (squared(n) % k)))(16)
    });
})();
```

{{Out}}

```txt
{
  "test1": 1,
  "test2": 7,
  "test3": true,
  "test4": true,
  "task2": [
    1,
    9,
    10,
    18,
    19,
    27,
    28,
    36,
    37,
    45,
    46,
    54,
    55,
    63,
    64,
    72,
    73,
    81,
    82,
    90,
    91,
    99,
    100
  ],
  "task3": [
    1,
    16,
    17,
    32,
    33,
    48,
    49,
    64,
    65,
    80,
    81,
    96,
    97
  ]
}
```



## jq

{{ works with|jq|1.4}}

In the following, the filter is_kaprekar as defined at [[Kaprekar_numbers#jq]] is used.
Since it is only defined for decimals, this section is correspondingly restricted.

'''Definition of co9''':

```jq
def co9:
  def digits: tostring | explode | map(. - 48);  # "0" is 48
  if . == 9 then 0
  elif 0 <= . and . <= 8 then .
  else digits | add | co9
  end;
```


For convenience, we also define a function to check whether co9(i) equals co9(i*i) 
for a given integer, i:

```jq
def co9_equals_co9_squared: co9 == ((.*.)|co9);
```


'''Example''':

Integers in 1 .. 100 satisfying co9(i) == co9(i*i):


```jq
[range (1;101) | select( co9_equals_co9_squared )
```

produces:
 [1,9,10,18,19,27,28,36,37,45,46,54,55,63,64,72,73,81,82,90,91,99,100]

'''Verification''':

One way to verify that the Kaprekar numbers satisfy the
co9_equals_co9_squared condition is by inspection. For the range 1..100 considered above, we have:


```jq
[ range(1;101) | select(is_kaprekar) ]
```


 [1,9,45,55,99]

To check the condition programmatically for a given range of integers, we can
define a function which will emit any exceptions, e.g.

```jq
def verify:
  range(1; .)
  | select(is_kaprekar and (co9_equals_co9_squared | not));
```


For example, running (1000 | verify) produces an empty stream.

'''Proportion of integers in 1 .. n satisfying the mod (b-1) condition''':

For a given base, "b", the following function computes the
proportion of integers, i, in 1 .. n such that i % (b-1) == (i*i) % (b-1):


```jq
def proportion(base):
  def count(stream): reduce stream as $i (0; . + 1);
  . as $n
  | (base - 1) as $b
  | count( range(1; 1+$n) | select( . % $b == (.*.) % $b) ) / $n ;
```

For example:

 (10, 100, 1000, 10000, 100000) | proportion(16)

produces:

```sh
0.3
0.27
0.267
0.2667
0.26667
```



## Julia


```Julia
co9(x) = x == 9  ? 0 : 
         1<=x<=8 ? x : 
         co9(sum(digits(x)))
```

iskaprekar is defined in the task
[[Kaprekar_numbers#Julia]].
{{Out}}

```txt
julia> show(filter(x->co9(x)==co9(x^2), 1:100))
[1,9,10,18,19,27,28,36,37,45,46,54,55,63,64,72,73,81,82,90,91,99,100]

julia> show(filter(iskaprekar, 1:100))
[1,9,45,55,99]

julia> show(filter(x->x%15 == (x^2)%15, 1:100))    # base 16
[1,6,10,15,16,21,25,30,31,36,40,45,46,51,55,60,61,66,70,75,76,81,85,90,91,96,100]
```



## Kotlin

{{trans|D}}

```scala
// version 1.1.3

fun castOut(base: Int, start: Int, end: Int): List<Int> {
    val b = base - 1
    val ran = (0 until b).filter { it % b == (it * it) % b }
    var x = start / b
    val result = mutableListOf<Int>()
    while (true) {
        for (n in ran) {
            val k = b * x + n
            if (k < start) continue
            if (k > end) return result
            result.add(k)
        }
        x++
    }
} 

fun main(args: Array<String>) {
    println(castOut(16, 1, 255))
    println()
    println(castOut(10, 1, 99))
    println()
    println(castOut(17, 1, 288))
}
```


{{out}}

```txt

[1, 6, 10, 15, 16, 21, 25, 30, 31, 36, 40, 45, 46, 51, 55, 60, 61, 66, 70, 75, 76, 81, 85, 90, 91, 96, 100, 105, 106, 111, 115, 120, 121, 126, 130, 135, 136, 141, 145, 150, 151, 156, 160, 165, 166, 171, 175, 180, 181, 186, 190, 195, 196, 201, 205, 210, 211, 216, 220, 225, 226, 231, 235, 240, 241, 246, 250, 255]

[1, 9, 10, 18, 19, 27, 28, 36, 37, 45, 46, 54, 55, 63, 64, 72, 73, 81, 82, 90, 91, 99]

[1, 16, 17, 32, 33, 48, 49, 64, 65, 80, 81, 96, 97, 112, 113, 128, 129, 144, 145, 160, 161, 176, 177, 192, 193, 208, 209, 224, 225, 240, 241, 256, 257, 272, 273, 288]

```



## Mathematica

Task 1: Simple referenced implementation that handles any base:

```mathematica
Co9[n_, b_: 10] := 
  With[{ans = FixedPoint[Total@IntegerDigits[#, b] &, n]}, 
   If[ans == b - 1, 0, ans]];
```


{{out|Task 1 output}}

```mathematica
Co9 /@ (vals = {1235, 2345, 4753})
  {2, 5, 1}

Total[Co9 /@ vals] == Co9[Total[vals]]
  True
```


Task 2:

```mathematica
task2 = Select[Range@100, Co9[#] == Co9[#^2] &] 
```


{{out|Task 2 output}}

```txt

{1, 9, 10, 18, 19, 27, 28, 36, 37, 45, 46, 54, 55, 63, 64, 72, 73, 81, 82, 90, 91, 99, 100}

```


Task 3: 
Defines the efficient co9 using Mod.

```mathematica
Co9eff[n_, b_: 10] := Mod[n, b - 1]; 
```


{{out|Task 3 output}}
Testing bases 10 and 17

```mathematica
task2 == Select[Range@100, Co9eff[#] == Co9eff[#^2] &]
   True

Select[Range@100, Co9eff[#, 17] == Co9eff[#^2, 17] &]
   {1, 16, 17, 32, 33, 48, 49, 64, 65, 80, 81, 96, 97}
```



## Nim


```nim
import sequtils

iterator castOut(base = 10, start = 1, ending = 999_999): int =
  var ran: seq[int] = @[]
  for y in 0 ..< base-1:
    if y mod (base - 1) == (y*y) mod (base - 1):
      ran.add(y)

  var x = start div (base - 1)
  var y = start mod (base - 1)

  block outer:
    while true:
      for n in ran:
        let k = (base - 1) * x + n
        if k < start:
          continue
        if k > ending:
          break outer
        yield k
      inc x

echo toSeq(castOut(base=16, start=1, ending=255))
```

{{out}}

```txt
@[1, 6, 10, 15, 16, 21, 25, 30, 31, 36, 40, 45, 46, 51, 55, 60, 61, 66, 70, 75, 76, 81, 85, 90, 91, 96, 100, 105, 106, 111, 115, 120, 121, 126, 130, 135, 136, 141, 145, 150, 151, 156, 160, 165, 166, 171, 175, 180, 181, 186, 190, 195, 196, 201, 205, 210, 211, 216, 220, 225, 226, 231, 235, 240, 241, 246, 250, 255]
```



## Objeck


```objeck
class CastingNines {
  function : Main(args : String[]) ~ Nil {
    base := 10;
    N := 2;
    c1 := 0;
    c2 := 0;

    for (k:=1; k<base->As(Float)->Power(N->As(Float)); k+=1;){
      c1+=1;
      if (k%(base-1) = (k*k)%(base-1)){
        c2+=1;
        IO.Console->Print(k)->Print(" ");
      };
    };

    IO.Console->Print("\nTrying ")->Print(c2)->Print(" numbers instead of ")
      ->Print(c1)->Print(" numbers saves ")->Print(100 - (c2->As(Float)/c1
      ->As(Float)*100))->PrintLine("%");
  }
}
```


{{out}}

```txt
1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99
Trying 22 numbers instead of 99 numbers saves 77.7777778%
```



## PARI/GP

{{trans|C++}}

```parigp
{base=10;
N=2;
c1=c2=0;
for(k=1,base^N-1,
  c1++;
  if (k%(base-1) == k^2%(base-1),
    c2++;
    print1(k" ")
  );
);
print("\nTrying "c2" numbers instead of "c1" numbers saves " 100.-(c2/c1)*100 "%")}

```

{{out|Produces}}

```txt

1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99
Trying 22 numbers instead of 99 numbers saves 77.77777777777777777777777778%

```

Changing to: "<code>base = 16;</code>" produces:

```txt

1 6 10 15 16 21 25 30 31 36 40 45 46 51 55 60 61 66 70 75 76 81 85 90 91 96 100
105 106 111 115 120 121 126 130 135 136 141 145 150 151 156 160 165 166 171 175
180 181 186 190 195 196 201 205 210 211 216 220 225 226 231 235 240 241 246 250
255
Trying 68 numbers instead of 255 numbers saves 73.33333333333333333333333333%
```



## Perl


```perl
sub co9 {  # Follows the simple procedure asked for in Part 1
  my $n = shift;
  return $n if $n < 10;
  my $sum = 0; $sum += $_ for split(//,$n);
  co9($sum);
}

sub showadd {
  my($n,$m) = @_;
  print "( $n [",co9($n),"] + $m [",co9($m),"] ) [",co9(co9($n)+co9($m)),"]", 
        "   =   ", $n+$m," [",co9($n+$m),"]\n";
}

sub co9filter {
  my $base = shift;
  die unless $base >= 2;
  my($beg, $end, $basem1) = (1, $base*$base-1, $base-1);
  my @list = grep { $_ % $basem1 == $_*$_ % $basem1 } $beg .. $end;
  ($end, scalar(@list), @list);
}

print "Part 1: Create a simple filter and demonstrate using simple example.\n";
showadd(6395, 1259);

print "\nPart 2: Use this to filter a range with co9(k) == co9(k^2).\n";
print join(" ", grep { co9($_) == co9($_*$_) } 1..99), "\n";

print "\nPart 3: Use efficient method on range.\n";
for my $base (10, 17) {
  my($N, $n, @l) = co9filter($base);
  printf "[@l]\nIn base %d, trying %d numbers instead of %d saves %.4f%%\n\n",
         $base, $n, $N, 100-($n/$N)*100;
}
```

{{out}}

```txt

Part 1: Create a simple filter and demonstrate using simple example.
( 6395 [5] + 1259 [8] ) [4]   =   7654 [4]

Part 2: Use this to filter a range with co9(k) == co9(k^2).
1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99

Part 3: Use efficient method on range.
[1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99]
In base 10, trying 22 numbers instead of 99 saves 77.7778%

[1 16 17 32 33 48 49 64 65 80 81 96 97 112 113 128 129 144 145 160 161 176 177 192 193 208 209 224 225 240 241 256 257 272 273 288]
In base 17, trying 36 numbers instead of 288 saves 87.5000%

```



## Perl 6

{{trans|Python}}
{{works with|Rakudo|2015.12}}

```perl6
sub cast-out(\BASE = 10, \MIN = 1, \MAX = BASE**2 - 1) {
  my \B9 = BASE - 1;
  my @ran = ($_ if $_ % B9 == $_**2 % B9 for ^B9);
  my $x = MIN div B9;
  gather loop {
    for @ran -> \n {
      my \k = B9 * $x + n;
      take k if k >= MIN;
    }
    $x++;
  } ...^ * > MAX;
}

say cast-out;
say cast-out 16;
say cast-out 17;
```

{{out}}

```txt
(1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99)
(1 6 10 15 16 21 25 30 31 36 40 45 46 51 55 60 61 66 70 75 76 81 85 90 91 96 100 105 106 111 115 120 121 126 130 135 136 141 145 150 151 156 160 165 166 171 175 180 181 186 190 195 196 201 205 210 211 216 220 225 226 231 235 240 241 246 250 255)
(1 16 17 32 33 48 49 64 65 80 81 96 97 112 113 128 129 144 145 160 161 176 177 192 193 208 209 224 225 240 241 256 257 272 273 288)
```



## Phix


```Phix
procedure co9(integer start, integer base, integer lim, sequence kaprekars)
integer c1=0,
        c2=0
sequence s = {}
    for k=start to lim do
        c1 += 1
        if mod(k,base-1)=mod(k*k,base-1) then
            c2 += 1
            s &= k
        end if
    end for
    string msg = "Valid subset\n"
    for i=1 to length(kaprekars) do
        if not find(kaprekars[i],s) then
            msg = "***INVALID***\n"
            exit
        end if
    end for
    if length(s)>40 then s[20..-20] = {"..."} end if
    ?s
    puts(1,"Kaprekar numbers:")
    ?kaprekars
    puts(1,msg)
    printf(1,"Trying %d numbers instead of %d saves %3.2f%%\n\n",{c2,c1,100-(c2/c1)*100})
end procedure

co9(1, 10, 99, {1,9,45,55,99})
co9(0(17)10, 17, 17*17, {0(17)3d,0(17)d4,0(17)gg})
co9(1, 10, 1000, {1,9,45,55,99,297,703,999})
```

{{out}}

```txt

{1,9,10,18,19,27,28,36,37,45,46,54,55,63,64,72,73,81,82,90,91,99}
Kaprekar numbers:{1,9,45,55,99}
Valid subset
Trying 22 numbers instead of 99 saves 77.78%

{17,32,33,48,49,64,65,80,81,96,97,112,113,128,129,144,145,160,161,176,177,192,193,208,209,224,225,240,241,256,257,272,273,288,289}
Kaprekar numbers:{64,225,288}
Valid subset
Trying 35 numbers instead of 273 saves 87.18%

{1,9,10,18,19,27,28,36,37,45,46,54,55,63,64,72,73,81,82,"...",919,927,928,936,937,945,946,954,955,963,964,972,973,981,982,990,991,999,1000}
Kaprekar numbers:{1,9,45,55,99,297,703,999}
Valid subset
Trying 223 numbers instead of 1000 saves 77.70%

```



## PicoLisp


```PicoLisp
(de kaprekar (N)
   (let L (cons 0 (chop (* N N)))
      (for ((I . R) (cdr L) R (cdr R))
         (NIL (gt0 (format R)))
         (T (= N (+ @ (format (head I L)))) N) ) ) )
         
(de co9 (N)
   (until
      (> 9
         (setq N
            (sum
               '((N) (unless (= "9" N) (format N)))
               (chop N) ) ) ) )
   N )

(println 'Part1:)      
(println
   (=
      (co9 (+ 6395 1259))
      (co9 (+ (co9 6395) (co9 1259))) ) )

(println 'Part2:)
(println
   (filter
      '((N) (= (co9 N) (co9 (* N N))))
      (range 1 100) ) )
(println   
   (filter kaprekar (range 1 100)) )
   
(println 'Part3 '- 'base17:)
(println
   (filter
      '((N) (= (% N 16) (% (* N N) 16)))
      (range 1 100) ) )
      
(bye)
```


{{out}}

```txt

Part1:
T
Part2:
(1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 100)
(1 9 45 55 99)
Part3 - base17:
(1 16 17 32 33 48 49 64 65 80 81 96 97)

```



## Python

This works slightly differently, generating the "wierd" (as defined by Counting Out Nines) numbers which may be Kaprekar, rather than filtering all numbers in a range.

```Python
# Casting out Nines
#
# Nigel Galloway: June 27th., 2012,
#
def CastOut(Base=10, Start=1, End=999999):
  ran = [y for y in range(Base-1) if y%(Base-1) == (y*y)%(Base-1)]
  x,y = divmod(Start, Base-1)
  while True:
    for n in ran:
      k = (Base-1)*x + n
      if k < Start:
        continue
      if k > End:
        return
      yield k
    x += 1

for V in CastOut(Base=16,Start=1,End=255):
  print(V, end=' ')
```

Produces:

```txt

1 6 10 15 16 21 25 30 31 36 40 45 46 51 55 60 61 66 70 75 76 81 85 90 91 96 100 105 106 111 115 120 121 126 130 135 136 141 145 150 151 156 160 165 166 171 175 180 181 186 190 195 196 201 205 210 211 216 220 225 226 231 235 240 241 246 250 255 

```

<code>CastOut(Base=10,Start=1,End=99)</code> produces:

```txt

1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 

```

<code>CastOut(Base=17,Start=1,End=288)</code> produces:

```txt

1 16 17 32 33 48 49 64 65 80 81 96 97 112 113 128 129 144 145 160 161 176 177 192 193 208 209 224 225 240 241 256 257 272 273 288 

```



## Racket


```racket
#lang racket
(require math)

(define (digits n)
  (map (compose1 string->number string)
       (string->list (number->string n))))

(define (cast-out-nines n)
  (with-modulus 9
    (for/fold ([sum 0]) ([d (digits n)])
      (mod+ sum d))))
```



## REXX


```rexx
/*REXX program demonstrates the  casting─out─nines  algorithm  (with Kaprekar numbers). */
parse arg LO HI base .                           /*obtain optional arguments from the CL*/
if LO=='' | LO==","  then do; LO=1; HI=1000; end /*Not specified?   Then use the default*/
if HI=='' | HI==","  then HI=LO                  /* "      "          "   "   "     "   */
if base=='' | base==","  then base=10            /* "      "          "   "   "     "   */
numeric digits max(9, 2*length(HI**2) )          /*insure enough decimal digits for HI².*/
numbers=castOut(LO, HI, base)                    /*generate a list of (cast out) numbers*/
@cast_out= 'cast-out-'  || (base-1)     "test"   /*construct a shortcut text for output.*/
say 'For'     LO     "through"     HI', the following passed the'       @cast_out":"
say numbers;         say                         /*display the list of cast out numbers.*/
q=HI-LO+1                                        /*Q:   is the range of numbers in list.*/
p=words(numbers)                                 /*P"    "  " number  "    "     "   "  */
pc=format(p/q * 100, , 2) / 1  ||  '%'           /*calculate the percentage (%) cast out*/
say 'For'   q   "numbers,"   p   'passed the'    @cast_out    "("pc') for base'    base"."
if base\==10  then exit                          /*if radix isn't ten, then exit program*/
Kaps=Kaprekar(LO, HI)                            /*generate a list of Kaprekar numbers. */
say;  say   'The Kaprekar numbers in the same range are:'   Kaps
say
      do i=1  for words(Kaps);    x=word(Kaps, i)                  /*verify 'em in list.*/
      if wordpos(x, numbers)\==0  then iterate                     /*it's OK so far ··· */
      say 'Kaprekar number'   x   "isn't in the numbers list."     /*oops─ay!           */
      exit 13                                                      /*go spank the coder.*/
      end   /*i*/

say 'All Kaprekar numbers are in the'     @cast_out     "numbers list."             /*OK*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
castOut:  procedure;  parse arg low,high,radix;       rm=word(radix 10, 1) - 1;         $=
                          do j=low  to  word(high low, 1)    /*test a range of numbers. */
                          if j//rm == j*j//rm  then $=$ j    /*did number pass the test?*/
                          end   /*j*/                        /* [↑]  Then add # to list.*/
          return strip($)                        /*strip and leading blanks from result.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
Kaprekar: procedure;  parse arg L,H;   $=;   if L<=1  then $=1   /*add unity if in range*/
            do j=max(2, L)  to H;        s=j*j   /*a slow way to find Kaprekar numbers. */
                do m=1  for length(s) % 2
                if j==left(s, m) + substr(s, m+1)  then do;  $=$ j;  leave;   end
                end   /*m*/                      /*     [↑]  found a Kaprekar number.   */
            end       /*j*/
          return strip($)                        /*return Kaprekar numbers to invoker.  */
```

'''output'''   when using the default inputs:

```txt

For 1 through 1000, the following passed the cast-out-9 test:
1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99 100 108 109 117 118 126 127 135 136 144 145 153 154 162 163 171 172 180 181 189 190 198 199 207 208 216 217 225 226 234 235 243 244 252
253 261 262 270 271 279 280 288 289 297 298 306 307 315 316 324 325 333 334 342 343 351 352 360 361 369 370 378 379 387 388 396 397 405 406 414 415 423 424 432 433 441 442 450 451 459 460 468 469 477
478 486 487 495 496 504 505 513 514 522 523 531 532 540 541 549 550 558 559 567 568 576 577 585 586 594 595 603 604 612 613 621 622 630 631 639 640 648 649 657 658 666 667 675 676 684 685 693 694 702
703 711 712 720 721 729 730 738 739 747 748 756 757 765 766 774 775 783 784 792 793 801 802 810 811 819 820 828 829 837 838 846 847 855 856 864 865 873 874 882 883 891 892 900 901 909 910 918 919 927
928 936 937 945 946 954 955 963 964 972 973 981 982 990 991 999 1000

For 1000 numbers, 223 passed the cast-out-9 test (22.3%) for base 10.

The Kaprekar numbers in the same range are: 1 9 45 55 99 297 703 999

All Kaprekar numbers are in the cast-out-9 test numbers list.

```

'''output'''   when using the input of:   <tt> 1   256   16 </tt>

```txt

For 1 through 256, the following passed the cast-out-15 test:
1 6 10 15 16 21 25 30 31 36 40 45 46 51 55 60 61 66 70 75 76 81 85 90 91 96 100 105 106 111 115 120 121 126 130 135 136 141 145 150 151 156 160 165 166 171 175 180 181 186 190 195 196 201 205 210 211
216 220 225 226 231 235 240 241 246 250 255 256

For 256 numbers, 69 passed the cast-out-15 test (26.95%) for base 16.

```



## Ring


```ring

# Project : Casting out nines

co9(1, 10, 99, [1,9,45,55,99])
co9(1, 10, 1000, [1,9,45,55,99,297,703,999])

func co9(start,base,lim,kaprekars)
c1=0
c2=0
s = []
for k = start to lim
     c1 = c1 + 1
      if k % (base-1) = (k*k) % (base-1) 
         c2 = c2 + 1
         add(s,k)
      ok
next
msg = "Valid subset" + nl
for i = 1 to len(kaprekars) 
     if not find(s,kaprekars[i])
       msg = "***Invalid***" + nl
       exit
     ok
next
showarray(s)
see "Kaprekar numbers:" + nl
showarray(kaprekars)
see msg
see "Trying " + c2 + " numbers instead of " + c1 + " saves " + (100-(c2/c1)*100) + "%" + nl + nl

func showarray(vect)
        see "{"
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + ", "
        next
        svect = left(svect, len(svect) - 2)
        see svect + "}" + nl

```

Output:

```txt

{1, 9, 10, 18, 19, 27, 28, 36, 37, 45, 46, 54, 55, 63, 64, 72, 73, 81, 82, 90, 91, 99}
Kaprekar numbers:
{1, 9, 45, 55, 99}
Valid subset
Trying 22 numbers instead of 99 saves 77.78%

{1, 9, 10, 18, 19, 27, 28, 36, 37, 45, 46, 54, 55, 63, 64, 72, 73, 81, 82, 90, 91, 99, 100, 108, 109, 117, 118, 126, 127, 135, 136, 144, 145, 153, 154, 162, 163, 171, 172, 180, 181, 189, 190, 198, 199, 207, 208, 216, 217, 225, 226, 234, 235, 243, 244, 252, 253, 261, 262, 270, 271, 279, 280, 288, 289, 297, 298, 306, 307, 315, 316, 324, 325, 333, 334, 342, 343, 351, 352, 360, 361, 369, 370, 378, 379, 387, 388, 396, 397, 405, 406, 414, 415, 423, 424, 432, 433, 441, 442, 450, 451, 459, 460, 468, 469, 477, 478, 486, 487, 495, 496, 504, 505, 513, 514, 522, 523, 531, 532, 540, 541, 549, 550, 558, 559, 567, 568, 576, 577, 585, 586, 594, 595, 603, 604, 612, 613, 621, 622, 630, 631, 639, 640, 648, 649, 657, 658, 666, 667, 675, 676, 684, 685, 693, 694, 702, 703, 711, 712, 720, 721, 729, 730, 738, 739, 747, 748, 756, 757, 765, 766, 774, 775, 783, 784, 792, 793, 801, 802, 810, 811, 819, 820, 828, 829, 837, 838, 846, 847, 855, 856, 864, 865, 873, 874, 882, 883, 891, 892, 900, 901, 909, 910, 918, 919, 927, 928, 936, 937, 945, 946, 954, 955, 963, 964, 972, 973, 981, 982, 990, 991, 999, 1000}
Kaprekar numbers:
{1, 9, 45, 55, 99, 297, 703, 999}
Valid subset
Trying 223 numbers instead of 1000 saves 77.70%

```



## Scala

Code written in scala follows functional paradigm of programming, finds list of candidates for Kaprekar numbers within given range.

```Scala

object kaprekar{
    // PART 1
    val co_base = ((x:Int,base:Int) => (x%(base-1) == (x*x)%(base-1)))
    //PART 2
    def get_cands(n:Int,base:Int):List[Int] = {
        if(n==1)                                List[Int]()
        else if (co_base(n,base))               n :: get_cands(n-1,base)
        else                                    get_cands(n-1,base)
    }
    def main(args:Array[String]) : Unit = {
        //PART 3
        val base = 31
        println("Candidates for Kaprekar numbers found by casting out method with base %d:".format(base))
        println(get_cands(1000,base))
    }
}

```

{{out}}

```txt

Output for base 10 within range of 100:
Candidates for Kaprekar numbers found by casting out method with base 10:
List(100, 99, 91, 90, 82, 81, 73, 72, 64, 63, 55, 54, 46, 45, 37, 36, 28, 27, 19, 18, 10, 9)

Output for base 17 with range 1000:
Candidates for Kaprekar numbers found by casting out method with base 17:
List(993, 992, 977, 976, 961, 960, 945, 944, 929, 928, 913, 912, 897, 896, 881, 880, 865, 864, 849, 848, 833, 832, 817, 816, 801, 800, 785, 784, 769, 768, 753, 752, 737, 736, 721, 720, 705, 704, 689, 688, 673, 672, 657, 656, 641, 640, 625, 624, 609, 608, 593, 592, 577, 576, 561, 560, 545, 544, 529, 528, 513, 512, 497, 496, 481, 480, 465, 464, 449, 448, 433, 432, 417, 416, 401, 400, 385, 384, 369, 368, 353, 352, 337, 336, 321, 320, 305, 304, 289, 288, 273, 272, 257, 256, 241, 240, 225, 224, 209, 208, 193, 192, 177, 176, 161, 160, 145, 144, 129, 128, 113, 112, 97, 96, 81, 80, 65, 64, 49, 48, 33, 32, 17, 16)

```




## Seed7


```seed7
$ include "seed7_05.s7i";

const func bitset: castOut (in integer: base, in integer: start, in integer: ending) is func
  result
    var bitset: casted is {};
  local
    var bitset: ran is {};
    var integer: x is 0;
    var integer: n is 0;
    var integer: k is 0;
    var boolean: finished is FALSE;
  begin
    for x range 0 to base - 2 do
      if x rem pred(base) = x ** 2 rem pred(base) then
        incl(ran, x);
      end if;
    end for;
    x := start div pred(base);
    repeat
      for n range ran until finished do
        k := pred(base) * x + n;
        if k >= start then
          if k > ending then
            finished := TRUE;
          else
            incl(casted, k);
          end if;
        end if;
      end for;
      incr(x);
    until finished;
  end func;

const proc: main is func
  begin
    writeln(castOut(16, 1, 255));
  end func;
```


{{out}}

```txt

{1, 6, 10, 15, 16, 21, 25, 30, 31, 36, 40, 45, 46, 51, 55, 60, 61, 66, 70, 75, 76, 81, 85, 90, 91, 96, 100, 105, 106, 111, 115, 120, 121, 126, 130, 135, 136, 141, 145, 150, 151, 156, 160, 165, 166, 171, 175, 180, 181, 186, 190, 195, 196, 201, 205, 210, 211, 216, 220, 225, 226, 231, 235, 240, 241, 246, 250, 255}

```



## Sidef

{{trans|Perl 6}}

```ruby
func cast_out(base = 10, min = 1, max = (base**2 - 1)) {

    var b9  = base-1
    var ran = b9.range.grep {|n| n%b9 == (n*n % b9) }

    var x = min//b9
    var r = []

    loop {
        ran.each {|n|
            var k = (b9*x + n)
            return r if (k > max)
            r << k if (k >= min)
        }
        ++x
    }

    return r
}

say cast_out().join(' ')
say cast_out(16).join(' ')
say cast_out(17).join(' ')
```

{{out}}

```txt

1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99
1 6 10 15 16 21 25 30 31 36 40 45 46 51 55 60 61 66 70 75 76 81 85 90 91 96 100 105 106 111 115 120 121 126 130 135 136 141 145 150 151 156 160 165 166 171 175 180 181 186 190 195 196 201 205 210 211 216 220 225 226 231 235 240 241 246 250 255
1 16 17 32 33 48 49 64 65 80 81 96 97 112 113 128 129 144 145 160 161 176 177 192 193 208 209 224 225 240 241 256 257 272 273 288

```



## Tcl


```tcl
proc co9 {x} {
    while {[string length $x] > 1} {
	set x [tcl::mathop::+ {*}[split $x ""]]
    }
    return $x
}
# Extended to the general case
proc coBase {x {base 10}} {
    while {$x >= $base} {
	for {set digits {}} {$x} {set x [expr {$x / $base}]} {
	    lappend digits [expr {$x % $base}]
	}
	set x [tcl::mathop::+ {*}$digits]
    }
    return $x
}

# Simple helper
proc percent {part whole} {format "%.2f%%" [expr {($whole - $part) * 100.0 / $whole}]}

puts "In base 10..."
set satisfying {}
for {set i 1} {$i < 100} {incr i} {
    if {[co9 $i] == [co9 [expr {$i*$i}]]} {
	lappend satisfying $i
    }
}
puts $satisfying
puts "Trying [llength $satisfying] numbers instead of 99 numbers saves [percent [llength $satisfying] 99]"

puts "In base 16..."
set satisfying {}
for {set i 1} {$i < 256} {incr i} {
    if {[coBase $i 16] == [coBase [expr {$i*$i}] 16]} {
	lappend satisfying $i
    }
}
puts $satisfying
puts "Trying [llength $satisfying] numbers instead of 255 numbers saves [percent [llength $satisfying] 255]"
```

{{out}}With some newlines inserted…

```txt

In base 10...
1 9 10 18 19 27 28 36 37 45 46 54 55 63 64 72 73 81 82 90 91 99
Trying 22 numbers instead of 99 numbers saves 77.78%
In base 16...
1 6 10 15 16 21 25 30 31 36 40 45 46 51 55 60 61 66 70 75 76 81 85 90 91 96 100
105 106 111 115 120 121 126 130 135 136 141 145 150 151 156 160 165 166 171 175
180 181 186 190 195 196 201 205 210 211 216 220 225 226 231 235 240 241 246 250 255
Trying 68 numbers instead of 255 numbers saves 73.33%

```



## zkl

{{trans|D}}

```zkl
fcn castOut(base=10, start=1, end=999999){
   base-=1;
   ran:=(0).filter(base,'wrap(n){ n%base == (n*n)%base });
   result:=Sink(List); 
   foreach a,b in ([start/base ..],ran){  // foreach{ foreach {} }
      k := base*a + b;
      if (k < start) continue;
      if (k > end) return(result.close());
      result.write(k);
   }
   // doesn't get here
}
```


```zkl
castOut(16, 1, 255).toString(*).println("\n-----");
castOut(10, 1,  99).toString(*).println("\n-----");
castOut(17, 1, 288).toString(*).println();
```

{{out}}

```txt

L(1,6,10,15,16,21,25,30,31,36,40,45,46,51,55,60,61,66,70,75,
  76,81,85,90,91,96,100,105,106,111,115,120,121,126,130,135,136,
  141,145,150,151,156,160,165,166,171,175,180,181,186,190,195,196,
  201,205,210,211,216,220,225,226,231,235,240,241,246,250,255)
-----
L(1,9,10,18,19,27,28,36,37,45,46,54,55,63,64,72,73,81,82,90,91,99)
-----
L(1,16,17,32,33,48,49,64,65,80,81,96,97,112,113,128,
  129,144,145,160,161,176,177,192,193,208,209,224,225,240,
  241,256,257,272,273,288)

```



## ZX Spectrum Basic

{{trans|C++}}

```zxbasic
10 LET Base=10
20 LET N=2
30 LET c1=0
40 LET c2=0
50 LET k=1
60 IF k>=(Base^N)-1 THEN GO TO 150
70 LET c1=c1+1
80 IF FN m(k,Base-1)=FN m(k*k,Base-1) THEN LET c2=c2+1: PRINT k;" ";
90 LET k=k+1
100 GO TO 60
150 PRINT '"Trying ";c2;" numbers instead of ";c1;" numbers saves ";100-(c2/c1)*100;"%"
160 STOP 
170 DEF FN m(a,b)=a-INT (a/b)*b

```


{{omit from|GUISS}}
