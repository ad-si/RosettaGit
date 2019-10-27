+++
title = "OpenWebNet Password"
description = ""
date = 2019-06-15T21:27:04Z
aliases = []
[extra]
id = 11982
[taxonomies]
categories = []
tags = []
+++

{{task}}
Calculate the password requested by ethernet gateways from the Legrand / Bticino MyHome OpenWebNet home automation system when the user's ip address is not in the gateway's whitelist

'''Note:''' Factory default password is '12345'. Changing it is highly recommended !

conversation goes as follows


```txt
← *#*1##
→ *99*0##
← *#603356072##
```


at which point a password should be sent back, calculated from the "password open" that is set in the gateway, and the nonce that was just sent


```txt
→ *#25280520##
← *#*1##
```


=={{Header|D}}==
{{trans|Python}}

```d
import std.stdio, std.string, std.conv, std.ascii, std.algorithm;

ulong ownCalcPass(in ulong password, in string nonce)
pure nothrow @safe @nogc
in {
    assert(nonce.representation.all!isDigit);
} body {
    enum ulong m_1        = 0x_FFFF_FFFF_UL;
    enum ulong m_8        = 0x_FFFF_FFF8_UL;
    enum ulong m_16       = 0x_FFFF_FFF0_UL;
    enum ulong m_128      = 0x_FFFF_FF80_UL;
    enum ulong m_16777216 = 0X_FF00_0000_UL;

    auto flag = true;
    ulong num1 = 0, num2 = 0;

    foreach (immutable char c; nonce) {
        num1 &= m_1;
        num2 &= m_1;

        switch (c) {
            case '0':
                num1 = num2;
                break;
            case '1':
                if (flag)
                    num2 = password;
                flag = false;
                num1 = num2 & m_128;
                num1 = num1 >> 7;
                num2 = num2 << 25;
                num1 = num1 + num2;
                break;
            case '2':
                if (flag)
                    num2 = password;
                flag = false;
                num1 = num2 & m_16;
                num1 = num1 >> 4;
                num2 = num2 << 28;
                num1 = num1 + num2;
                break;
            case '3':
                if (flag)
                    num2 = password;
                flag = false;
                num1 = num2 & m_8;
                num1 = num1 >> 3;
                num2 = num2 << 29;
                num1 = num1 + num2;
                break;
            case '4':
                if (flag)
                    num2 = password;
                flag = false;
                num1 = num2 << 1;
                num2 = num2 >> 31;
                num1 = num1 + num2;
                break;
            case '5':
                if (flag)
                    num2 = password;
                flag = false;
                num1 = num2 << 5;
                num2 = num2 >> 27;
                num1 = num1 + num2;
                break;
            case '6':
                if (flag)
                    num2 = password;
                flag = false;
                num1 = num2 << 12;
                num2 = num2 >> 20;
                num1 = num1 + num2;
                break;
            case '7':
                if (flag)
                    num2 = password;
                flag = false;
                num1 = num2 & 0xFF00UL;
                num1 = num1 + ((num2 & 0xFFUL) << 24);
                num1 = num1 + ((num2 & 0xFF0000UL) >> 16);
                num2 = (num2 & m_16777216) >> 8;
                num1 = num1 + num2;
                break;
            case '8':
                if (flag)
                    num2 = password;
                flag = false;
                num1 = num2 & 0xFFFFUL;
                num1 = num1 << 16;
                num1 = num1 + (num2 >> 24);
                num2 = num2 & 0xFF0000UL;
                num2 = num2 >> 8;
                num1 = num1 + num2;
                break;
            case '9':
                if (flag)
                    num2 = password;
                flag = false;
                num1 = ~num2;
                break;
            default: // Impossible if contracts are active.
                assert(0, "Non-digit in nonce");
        }

        num2 = num1;
    }

    return num1 & m_1;
}

void ownTestCalcPass(in string sPassword, in string nonce, in ulong expected)
in {
    assert(sPassword.representation.all!isDigit);
    assert(nonce.representation.all!isDigit);
} body {
    immutable password = sPassword.to!ulong;
    immutable res = ownCalcPass(password, nonce);
    immutable m = format("%d %s %d %d", password, nonce, res, expected);
    writeln((res == expected) ? "PASS " : "FAIL ", m);
}

void main() {
    ownTestCalcPass("12345", "603356072", 25280520UL);
    ownTestCalcPass("12345", "410501656", 119537670UL);
}
```

{{out}}

```txt
PASS 12345 603356072 25280520 25280520
PASS 12345 410501656 119537670 119537670
```



## Go

{{trans|Python}}

```go
package main

import (
    "fmt"
    "strconv"
)

func ownCalcPass(password, nonce string) uint32 {
    start := true
    num1 := uint32(0)
    num2 := num1
    i, _ := strconv.Atoi(password)
    pwd := uint32(i)
    for _, c := range nonce {
        if c != '0' {
            if start {
                num2 = pwd
            }
            start = false
        }
        switch c {
        case '1':
            num1 = (num2 & 0xFFFFFF80) >> 7
            num2 = num2 << 25
        case '2':
            num1 = (num2 & 0xFFFFFFF0) >> 4
            num2 = num2 << 28
        case '3':
            num1 = (num2 & 0xFFFFFFF8) >> 3
            num2 = num2 << 29
        case '4':
            num1 = num2 << 1
            num2 = num2 >> 31
        case '5':
            num1 = num2 << 5
            num2 = num2 >> 27
        case '6':
            num1 = num2 << 12
            num2 = num2 >> 20
        case '7':
            num3 := num2 & 0x0000FF00
            num4 := ((num2 & 0x000000FF) << 24) | ((num2 & 0x00FF0000) >> 16)
            num1 = num3 | num4
            num2 = (num2 & 0xFF000000) >> 8
        case '8':
            num1 = (num2&0x0000FFFF)<<16 | (num2 >> 24)
            num2 = (num2 & 0x00FF0000) >> 8
        case '9':
            num1 = ^num2
        default:
            num1 = num2
        }

        num1 &= 0xFFFFFFFF
        num2 &= 0xFFFFFFFF
        if c != '0' && c != '9' {
            num1 |= num2
        }
        num2 = num1
    }
    return num1
}

func testPasswordCalc(password, nonce string, expected uint32) {
    res := ownCalcPass(password, nonce)
    m := fmt.Sprintf("%s  %s  %-10d  %-10d", password, nonce, res, expected)
    if res == expected {
        fmt.Println("PASS", m)
    } else {
        fmt.Println("FAIL", m)
    }
}

func main() {
    testPasswordCalc("12345", "603356072", 25280520)
    testPasswordCalc("12345", "410501656", 119537670)
    testPasswordCalc("12345", "630292165", 4269684735)
}
```


{{out}}

```txt

PASS 12345  603356072  25280520    25280520  
PASS 12345  410501656  119537670   119537670 
PASS 12345  630292165  4269684735  4269684735

```


=={{Header|JavaScript}}==

```javascript

function calcPass (pass, nonce) {
	var flag = true;
	var num1 = 0x0;
	var num2 = 0x0;
	var password = parseInt(pass, 10);
	
	for (var c in nonce) {
		c = nonce[c];
		if (c!='0') {
			if (flag) num2 = password;
			flag = false;
		}
		switch (c) {
			case '1':
				num1 = num2 & 0xFFFFFF80;
				num1 = num1 >>> 7;
				num2 = num2 << 25;
				num1 = num1 + num2;
				break;
			case '2':
				num1 = num2 & 0xFFFFFFF0;
				num1 = num1 >>> 4;
				num2 = num2 << 28;
				num1 = num1 + num2;
				break;
			case '3':
				num1 = num2 & 0xFFFFFFF8;
				num1 = num1 >>> 3;
				num2 = num2 << 29;
				num1 = num1 + num2;
				break;
			case '4':
				num1 = num2 << 1;
				num2 = num2 >>> 31;
				num1 = num1 + num2;
				break;
			case '5':
				num1 = num2 << 5;
				num2 = num2 >>> 27;
				num1 = num1 + num2;
				break;
			case '6':
				num1 = num2 << 12;
				num2 = num2 >>> 20;
				num1 = num1 + num2;
				break;
			case '7':
				num1 = num2 & 0x0000FF00;
				num1 = num1 + (( num2 & 0x000000FF ) << 24 );
				num1 = num1 + (( num2 & 0x00FF0000 ) >>> 16 );
				num2 = ( num2 & 0xFF000000 ) >>> 8;
				num1 = num1 + num2;
				break;
			case '8':
				num1 = num2 & 0x0000FFFF;
				num1 = num1 << 16;
				num1 = num1 + ( num2 >>> 24 );
				num2 = num2 & 0x00FF0000;
				num2 = num2 >>> 8;
				num1 = num1 + num2;
				break;
			case '9':
				num1 = ~num2;
				break;
			case '0':
				num1 = num2;
				break;
		}
		num2 = num1;
	}
	return (num1 >>> 0).toString();
}

exports.calcPass = calcPass;

console.log ('openpass initialization');
function testCalcPass (pass, nonce, expected) {
	var res = calcPass (pass, nonce);
	var m = pass + ' ' + nonce + ' ' + res + ' ' + expected;
	if (res == parseInt(expected, 10))
		console.log ('PASS '+m);
	else
		console.log ('FAIL '+m);
}

testCalcPass ('12345', '603356072', '25280520');
testCalcPass ('12345', '410501656', '119537670');
testCalcPass ('12345', '630292165', '4269684735');
testCalcPass ('12345', '523781130', '537331200');

```



## Julia

Passes all tests.

```julia
function calcpass(passwd, nonce::String)
    startflag = true
    n1 = 0
    n2 = 0
    password = parse(Int, passwd)
    dact = Dict(
                '1' => () -> begin n1 = (n2 & 0xffffff80) >> 7; n2 <<= 25 end,
                '2' => () -> begin n1 = (n2 & 0xfffffff0) >> 4; n2 <<= 28 end,
                '3' => () -> begin n1 = (n2 & 0xfffffff8) >> 3; n2 <<= 29 end,
                '4' => () -> begin n1 = n2 << 1; n2 >>= 31 end,
                '5' => () -> begin n1 = n2 << 5; n2 >>= 27 end,
                '6' => () -> begin n1 = n2 << 12; n2 >>= 20 end,
                '7' => () -> begin n1 = (n2 & 0x0000ff00) | ((n2 & 0x000000ff) << 24) |
                             ((n2 & 0x00ff0000) >> 16); n2 = (n2 & 0xff000000) >> 8 end,
                '8' => () -> begin n1 = ((n2 & 0x0000ffff) << 16) | (n2 >> 24);
                             n2 = (n2 & 0x00ff0000) >> 8 end,
                '9' => () -> begin n1 = ~n2 end)

    for c in nonce
        if !haskey(dact, c)
            n1 = n2
        else
            if startflag
                n2 = password
            end
            startflag = false
            dact[c]()
            n1 &= 0xffffffff
            n2 &= 0xffffffff
            if c != '9'
                n1 |= n2
            end
        end
        n2 = n1
    end
    n1
end

function testcalcpass()
    tdata = [["12345", "603356072", "25280520"], ["12345", "410501656", "119537670"],
             ["12345", "630292165", "4269684735"], ["12345", "523781130", "537331200"]]
	for td in tdata
        pf = calcpass(td[1], td[2]) == parse(Int, td[3]) ? "Passes test." : "Fails test."
        println("Calculating pass for [$(td[1]), $(td[2])] = $(td[3]): $pf")
    end
end

testcalcpass()

```




## Kotlin

{{trans|Python}}

```scala
// version 1.1.51

fun ownCalcPass(password: Long, nonce: String): Long {
    val m1        = 0xFFFF_FFFFL
    val m8        = 0xFFFF_FFF8L
    val m16       = 0xFFFF_FFF0L
    val m128      = 0xFFFF_FF80L
    val m16777216 = 0xFF00_0000L

    var flag = true
    var num1 = 0L
    var num2 = 0L

    for (c in nonce) {
        num2 = num2 and m1

        when (c) {
            '1' -> {
                if (flag) num2 = password
                flag = false
                num1 = num2 and m128
                num1 = num1 ushr 7
                num2 = num2 shl 25
                num1 = num1 + num2
            }

            '2' -> {
                if (flag) num2 = password
                flag = false
                num1 = num2 and m16
                num1 = num1 ushr 4
                num2 = num2 shl 28
                num1 = num1 + num2
            }

            '3' -> {
                if (flag) num2 = password
                flag = false
                num1 = num2 and m8
                num1 = num1 ushr 3
                num2 = num2 shl 29
                num1 = num1 + num2
            }

            '4' -> {
                if (flag) num2 = password
                flag = false
                num1 = num2 shl 1
                num2 = num2 ushr 31
                num1 = num1 + num2
            }

            '5' -> {
                if (flag) num2 = password
                flag = false
                num1 = num2 shl 5
                num2 = num2 ushr 27
                num1 = num1 + num2
            }

            '6' -> {
                if (flag) num2 = password
                flag = false
                num1 = num2 shl 12
                num2 = num2 ushr 20
                num1 = num1 + num2
            }

            '7' -> {
                if (flag) num2 = password
                flag = false
                num1 = num2 and 0xFF00L
                num1 = num1 + ((num2 and 0xFFL) shl 24)
                num1 = num1 + ((num2 and 0xFF0000L) ushr 16)
                num2 = (num2 and m16777216) ushr 8
                num1 = num1 + num2
            }

            '8' -> {
                if (flag) num2 = password
                flag = false
                num1 = num2 and 0xFFFFL
                num1 = num1 shl 16
                num1 = num1 + (num2 ushr 24)
                num2 = num2 and 0xFF0000L
                num2 = num2 ushr 8
                num1 = num1 + num2
            }

            '9' -> {
                if (flag) num2 = password
                flag = false
                num1 = num2.inv()
            }

            else -> num1 = num2
        }
        num2 = num1
    }
    return num1 and m1
}

fun ownTestCalcPass(passwd: String, nonce: String, expected: Long) {
    val res = ownCalcPass(passwd.toLong(), nonce)
    val m = "$passwd  $nonce  $res  $expected"
    println(if (res == expected) "PASS  $m" else "FAIL  $m")
}

fun main(args: Array<String>) {
    ownTestCalcPass("12345", "603356072", 25280520)
    ownTestCalcPass("12345", "410501656", 119537670)
}
```


{{out}}

```txt

PASS  12345  603356072  25280520  25280520
PASS  12345  410501656  119537670  119537670

```



## Perl 6

Perl 6 doesn't really have good support for unsigned fixed bit integers yet so emulating them with regular Ints and bitmasks.

```perl6
sub own-password (Int $password, Int $nonce) {
    my int $n1 = 0;
    my int $n2 = $password;
    for $nonce.comb {
        given $_ {
            when 1 {
                $n1 = $n2 +& 0xFFFFFF80 +> 7;
                $n2 +<= 25;
            }
            when 2 {
                $n1 = $n2 +& 0xFFFFFFF0 +> 4;
                $n2 +<= 28;
            }
            when 3 {
                $n1 = $n2 +& 0xFFFFFFF8 +> 3;
                $n2 +<= 29;
            }
            when 4 {
                $n1 = $n2 +< 1;
                $n2 +>= 31;
            }
            when 5 {
                $n1 = $n2 +< 5;
                $n2 +>= 27;
            }
            when 6 {
                $n1 = $n2 +< 12;
                $n2 +>= 20;
            }
            when 7 {
                $n1 = $n2 +& 0x0000FF00 +| ($n2 +& 0x000000FF +< 24) +| ($n2 +& 0x00FF0000 +> 16);
                $n2 = $n2 +& 0xFF000000 +> 8;
            }
            when 8 {
                $n1 = $n2 +& 0x0000FFFF +< 16 +| $n2 +> 24;
                $n2 = $n2 +& 0x00FF0000 +> 8;
            }
            when 9  { $n1 = +^$n2 }
            default { $n1 = $n2 }
        }
        given $_ {
            when 0 {}
            when 9 {}
            default { $n1 = ($n1 +| $n2) +& 0xFFFFFFFF }
        }
        $n2 = $n1;
    }
    $n1
}

say own-password( 12345, 603356072 );
say own-password( 12345, 410501656 );
say own-password( 12345, 630292165 );
```


{{out}}

```txt
25280520
119537670
4269684735
```



## Phix


```Phix
function unsigned(atom n)
    atom m4 = allocate(8)
    poke4(m4,n)
    n = peek4u(m4)
    free(m4)
    return n
end function 

function ownCalcPass(atom pwd, string nonce)
    bool start = true
    atom num1 = 0,
         num2 = 0
    for i=1 to length(nonce) do
        integer c = nonce[i]
        if c!='0' and start then
            num2 = pwd
            start = false
        end if
        switch c do
            case '1':   num1 = shift_bits(num2,7)
                        num2 = shift_bits(num2,-25)
            case '2':   num1 = shift_bits(num2,4)
                        num2 = shift_bits(num2,-28)
            case '3':   num1 = shift_bits(num2,3)
                        num2 = shift_bits(num2,-29)
            case '4':   num1 = shift_bits(num2,-1)
                        num2 = shift_bits(num2,31)
            case '5':   num1 = shift_bits(num2,-5)
                        num2 = shift_bits(num2,27)
            case '6':   num1 = shift_bits(num2,-12)
                        num2 = shift_bits(num2,20)
            case '7':   num1 = or_bits(and_bits(num2,0x0000FF00),
                               or_bits(shift_bits(and_bits(num2,0x000000FF),-24),
                                       shift_bits(and_bits(num2,0x00FF0000),16)))
                        num2 = shift_bits(and_bits(num2,0xFF000000),8)
            case '8':   num1 = or_bits(shift_bits(and_bits(num2,0x0000FFFF),-16),
                                       shift_bits(num2,24))
                        num2 = shift_bits(and_bits(num2,0x00FF0000),8)
            case '9':   num1 = not_bits(num2)
            default:    num1 = num2
        end switch
        if c!='0' and c!='9' then
            num1 = or_bits(num1,num2)
        end if
        num2 = num1
    end for
    return unsigned(num1)
end function
 
procedure testPasswordCalc(atom pwd, string nonce, atom expected)
    atom res := ownCalcPass(pwd, nonce)
    string pf = iff(res=expected?"PASS":"FAIL")
    printf(1,"%s  %d  %s  %-10d  %-10d\n", {pf, pwd, nonce, res, expected})
end procedure
 
testPasswordCalc(12345, "603356072", 25280520)
testPasswordCalc(12345, "410501656", 119537670)
testPasswordCalc(12345, "630292165", 4269684735)
testPasswordCalc(12345, "523781130", 537331200)
```

{{out}}

```txt

PASS  12345  603356072  25280520    25280520
PASS  12345  410501656  119537670   119537670
PASS  12345  630292165  4269684735  4269684735
PASS  12345  523781130  537331200   537331200

```


=={{Header|Python}}==

```python

def ownCalcPass (password, nonce, test=False) :
    start = True    
    num1 = 0
    num2 = 0
    password = int(password)
    if test:
        print("password: %08x" % (password))
    for c in nonce :
        if c != "0":
            if start:
                num2 = password
            start = False
        if test:
            print("c: %s num1: %08x num2: %08x" % (c, num1, num2))
        if c == '1':
            num1 = (num2 & 0xFFFFFF80) >> 7
            num2 = num2 << 25
        elif c == '2':
            num1 = (num2 & 0xFFFFFFF0) >> 4
            num2 = num2 << 28
        elif c == '3':
            num1 = (num2 & 0xFFFFFFF8) >> 3
            num2 = num2 << 29
        elif c == '4':
            num1 = num2 << 1
            num2 = num2 >> 31
        elif c == '5':
            num1 = num2 << 5
            num2 = num2 >> 27
        elif c == '6':
            num1 = num2 << 12
            num2 = num2 >> 20
        elif c == '7':
            num1 = num2 & 0x0000FF00 | (( num2 & 0x000000FF ) << 24 ) | (( num2 & 0x00FF0000 ) >> 16 )
            num2 = ( num2 & 0xFF000000 ) >> 8
        elif c == '8':
            num1 = (num2 & 0x0000FFFF) << 16 | ( num2 >> 24 )
            num2 = (num2 & 0x00FF0000) >> 8
        elif c == '9':
            num1 = ~num2
        else :
            num1 = num2

        num1 &= 0xFFFFFFFF
        num2 &= 0xFFFFFFFF
        if (c not in "09"):
            num1 |= num2
        if test:
            print("     num1: %08x num2: %08x" % (num1, num2))
        num2 = num1
    return num1

def test_passwd_calc(passwd, nonce, expected):
    res = ownCalcPass(passwd, nonce, False)
    m = passwd+' '+nonce+' '+str(res)+' '+str(expected)
    if res == int(expected) :
        print('PASS '+m)
    else :
        print('FAIL '+m)

if __name__ == '__main__':
    test_passwd_calc('12345','603356072','25280520')
    test_passwd_calc('12345','410501656','119537670')
    test_passwd_calc('12345','630292165','4269684735')


```


=={{Header|PHP}}==

```PHP
function ownCalcPass($password, $nonce) {
    $msr = 0x7FFFFFFF;
    $m_1 = (int)0xFFFFFFFF;
    $m_8 = (int)0xFFFFFFF8;
    $m_16 = (int)0xFFFFFFF0;
    $m_128 = (int)0xFFFFFF80;
    $m_16777216 = (int)0xFF000000;
    $flag = True;
    $num1 = 0;
    $num2 = 0;

    foreach (str_split($nonce) as $c) {
        $num1 = $num1 & $m_1;
        $num2 = $num2 & $m_1;
        if ($c == '1') {
            $length = !$flag;
            if (!$length) {
                $num2 = $password;
            }
            $num1 = $num2 & $m_128;
            $num1 = $num1 >> 1;
            $num1 = $num1 & $msr;
            $num1 = $num1 >> 6;
            $num2 = $num2 << 25;
            $num1 = $num1 + $num2;
            $flag = False;
        } elseif ($c == '2') {
            $length = !$flag;
            if (!$length) {
                $num2 = $password;
            }
            $num1 = $num2 & $m_16;
            $num1 = $num1 >> 1;
            $num1 = $num1 & $msr;
            $num1 = $num1 >> 3;
            $num2 = $num2 << 28;
            $num1 = $num1 + $num2;
            $flag = False;
        } elseif ($c == '3') {
            $length = !$flag;
            if (!$length) {
                $num2 = $password;
            }
            $num1 = $num2 & $m_8;
            $num1 = $num1 >> 1;
            $num1 = $num1 & $msr;
            $num1 = $num1 >> 2;
            $num2 = $num2 << 29;
            $num1 = $num1 + $num2;
            $flag = False;
        } elseif ($c == '4') {
            $length = !$flag;
            if (!$length) {
                $num2 = $password;
            }
            $num1 = $num2 << 1;
            $num2 = $num2 >> 1;
            $num2 = $num2 & $msr;
            $num2 = $num2 >> 30;
            $num1 = $num1 + $num2;
            $flag = False;
        } elseif ($c == '5') {
            $length = !$flag;
            if (!$length) {
                $num2 = $password;
            }
            $num1 = $num2 << 5;
            $num2 = $num2 >> 1;
            $num2 = $num2 & $msr;
            $num2 = $num2 >> 26;
            $num1 = $num1 + $num2;
            $flag = False;
        } elseif ($c == '6') {
            $length = !$flag;
            if (!$length) {
                $num2 = $password;
            }
            $num1 = $num2 << 12;
            $num2 = $num2 >> 1;
            $num2 = $num2 & $msr;
            $num2 = $num2 >> 19;
            $num1 = $num1 + $num2;
            $flag = False;
        } elseif ($c == '7') {
            $length = !$flag;
            if (!$length) {
                $num2 = $password;
            }
            $num1 = $num2 & 0xFF00;
            $num1 = $num1 + (( $num2 & 0xFF ) << 24 );
            $num1 = $num1 + (( $num2 & 0xFF0000 ) >> 16 );
            $num2 = $num2 & $m_16777216;
            $num2 = $num2 >> 1;
            $num2 = $num2 & $msr;
            $num2 = $num2 >> 7;
            $num1 = $num1 + $num2;
            $flag = False;
        } elseif ($c == '8') {
            $length = !$flag;
            if (!$length) {
                $num2 = $password;
            }
            $num1 = $num2 & 0xFFFF;
            $num1 = $num1 << 16;
            $numx = $num2 >> 1;
            $numx = $numx & $msr;
            $numx = $numx >> 23;
            $num1 = $num1 + $numx;
            $num2 = $num2 & 0xFF0000;
            $num2 = $num2 >> 1;
            $num2 = $num2 & $msr;
            $num2 = $num2 >> 7;
            $num1 = $num1 + $num2;
            $flag = False;
        } elseif ($c == '9') {
            $length = !$flag;
            if (!$length) {
                $num2 = $password;
            }
            $num1 = ~(int)$num2;
            $flag = False;
        } else {
            $num1 = $num2;
        }
        $num2 = $num1;
    }
    return sprintf('%u', $num1 & $m_1);
}

```



## Racket


{{trans|Python}} (followed by some aggressive refactoring)


```racket
#lang racket/base
(define (32-bit-truncate n)
  (bitwise-and n #xFFFFFFFF))

(define (own-calc-pass password-string nonce)
  (define-values (num-1 flag)
    (for/fold ((num-1 0) (flag #t))
              ((c (in-string nonce)))
      (let* ((num-1 (32-bit-truncate num-1))
             (num-2 (if flag (string->number password-string) num-1)))
        
        (define (and-right-left-add mask right left)
          (values (+ (arithmetic-shift (bitwise-and num-2 mask) (- right))
                     (arithmetic-shift num-2 left))
                  #f))

        (define (left-right-add left right)
          (values (+ (arithmetic-shift num-2 left) (arithmetic-shift num-2 (- right))) #f))

        (define (stage-7)
          (values (+ (+ (+ (bitwise-and num-2 #xff00)
                           (arithmetic-shift (bitwise-and num-2 #xff) 24))
                        (arithmetic-shift (bitwise-and num-2 #xff0000) -16))
                     (arithmetic-shift (bitwise-and num-2 #xFF000000) -8))
                  #f))

        (define (stage-8)
          (values (+ (+ (arithmetic-shift (bitwise-and num-2 #xffff) 16)
                        (arithmetic-shift num-2 -24))
                     (arithmetic-shift (bitwise-and num-2 #xff0000) -8))
                  #f))

        (define (stage-9) (values (bitwise-not num-2) #f))

        (case c
          ([#\1] (and-right-left-add #xFFFFFF80 7 25))
          ([#\2] (and-right-left-add #xFFFFFFF0 4 28))
          ([#\3] (and-right-left-add #xFFFFFFF8 3 29))
          ([#\4] (left-right-add 1 31))
          ([#\5] (left-right-add 5 27))
          ([#\6] (left-right-add 12 20))
          ([#\7] (stage-7))
          ([#\8] (stage-8))
          ([#\9] (stage-9))
          (else (values num-1 flag))))))
  (32-bit-truncate num-1))

(module+ test
  (require rackunit)

  (define (own-test-calc-pass passwd nonce expected)
    (let* ((res (own-calc-pass passwd nonce))
           (msg (format "~a ~a ~a ~a" passwd nonce res expected)))
      (string-append (if (= res expected) "PASS" "FAIL") " " msg)))

  
  (own-test-calc-pass "12345" "603356072" 25280520)
  (own-test-calc-pass "12345" "410501656" 119537670))
```


=={{Header|Swift}}==

```swift

func openAuthenticationResponse(_password: String, operations: String) -> String? {
    var num1 = UInt32(0)
    var num2 = UInt32(0)
    var start = true
    let password = UInt32(_password)!
    for c in operations {
        if (c != "0") {
            if start {
                num2 = password
            }
            start = false
        }
        switch c {
        case "1":
            num1 = (num2 & 0xffffff80) >> 7
            num2 = num2 << 25
        case "2":
            num1 = (num2 & 0xfffffff0) >> 4
            num2 = num2 << 28
        case "3":
            num1 = (num2 & 0xfffffff8) >> 3
            num2 = num2 << 29
        case "4":
            num1 = num2 << 1
            num2 = num2 >> 31
        case "5":
            num1 = num1 << 5
            num2 = num2 >> 27
        case "6":
            num1 = num2 << 12
            num2 = num2 >> 20
        case "7":
            num1 = (num2 & 0x0000ff00) | ((num2 & 0x000000ff) << 24) | ((num2 & 0x00ff0000) >> 16)
            num2 = (num2 & 0xff000000) >> 8
        case "8":
            num1 = ((num2 & 0x0000ffff) << 16) | (num2 >> 24)
            num2 = (num2 & 0x00ff0000) >> 8
        case "9":
            num1 = ~num2
        case "0":
            num1 = num2
        default:
            print("unexpected char \(c)")
            return nil
        }
        if (c != "9") && (c != "0") {
            num1 |= num2
        }
        num2 = num1
    }
    return String(num1)
}

```

