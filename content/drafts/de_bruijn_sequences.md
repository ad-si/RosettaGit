+++
title = "De Bruijn sequences"
description = ""
date = 2019-10-21T23:58:02Z
aliases = []
[extra]
id = 22500
[taxonomies]
categories = []
tags = []
+++

{{task}}

The sequences are named after the Dutch mathematician   Nicolaas Govert de Bruijn.


A note on Dutch capitalization:   Nicolaas' last name is   '''de Bruijn''',   the   '''de'''   isn't normally capitalized
unless it's the first word in a sentence.   Rosetta Code (more or less by default or by fiat) requires the first word in the task name to be
capitalized.


In combinatorial mathematics,   a   '''de Bruijn sequence'''   of order   <big> ''n'' </big>   on
a   <big> size-''k'' </big>   alphabet (computer science)   <big> ''A'' </big>   is a cyclic sequence in which every
possible   <big> length-''n'' </big>   string (computer science, formal theory)   on   <big> ''A'' </big>   occurs
exactly once as a contiguous substring.
<!--
──────────────────────────────────────────────────────────────────────────────────────────────────────────
Yeah,  I know,  it's a pretty big mouthful,  but it's what Wikipedia uses, so I went with that definition.
──────────────────────────────────────────────────────────────────────────────────────────────────────────
!-->

Such a sequence is denoted by   <big> ''B''(''k'', ''n'') </big>   and has
length   <big>''k''<sup>''n''</sup></big>,   which is also the number of distinct substrings of
length   <big>''n''</big>   on   <big>''A''</big>;    

de Bruijn sequences are therefore optimally short.

<!--
──────────────────────────────────────────────────────────────────────────────────────────────────────────
Expressing the (below) equation with a  <math>  HTML tag causes it to "blow up" on Rosetta Code's version.
──────────────────────────────────────────────────────────────────────────────────────────────────────────
!-->
There are:
                          <big><big><big>(k!)<sup>k<sup>(n-1)</sup></sup>   <big><b>&divide;</b></big>   k<sup>n</sup></big></big></big>
distinct de Bruijn sequences   <big> ''B''(''k'', ''n''). </big>


;Task:
For this Rosetta Code task,   a   '''de Bruijn'''   sequence is to be generated that can be used to shorten a brute-force attack on
a   [https://en.wikipedia.org/wiki/Personal_Identification_Number PIN]-like   code lock that does not have an "enter"
key and accepts the last   <big> ''n'' </big>   digits entered.


Note:   [https://en.wikipedia.org/wiki/Automated_teller_machine automated tell machines (ATMs)]   used to work like
this,   but their software has been updated to not allow a brute-force attack.


;Example:
A   [https://en.wikipedia.org/wiki/digital_door_lock digital door lock]   with a 4-digit code would
have ''B''&thinsp;(10, 4) solutions,   with a length of   '''10,000'''   (digits).

Therefore, only at most     '''10,000 + 3'''     (as the solutions are cyclic or ''wrap-around'')   presses are needed to
open the lock.

Trying all 4-digit codes separately would require   '''4 &times; 10,000'''   or   '''40,000'''   presses.


;Task requirements:
:*   Generate a de Bruijn sequence for a 4-digit (decimal) PIN code.
:::*   Show the length of the generated de Bruijn sequence.
:::*   (There are many possible de Bruijn sequences that solve this task,   one solution is shown on the ''discussion'' page).
:::*   Show the first and last   '''130'''   digits of the de Bruijn sequence.
:*   Verify that all four-digit (decimal)   '''1,000'''   PIN codes are contained within the de Bruijn sequence.
:::*   0000, 0001, 0002, 0003,   ...   9996, 9997, 9998, 9999   (note the leading zeros).
:*   Reverse the de Bruijn sequence.
:*   Again, perform the (above) verification test.
:*   Replace the 4,444<sup>th</sup> digit with a period (.) in the original de Bruijn sequence.
:::*   Perform the verification test (again).   There should be several PIN codes missing.


(The last requirement is to ensure that the verification tests performs correctly.   The verification processes should list
any and all missing PIN codes.)

Show all output here, on this page.


;References:
:*   Wikipedia           entry:   [https://en.wikipedia.org/wiki/De_Bruijn_sequence de Bruijn sequence].
:*   MathWorld           entry:   [http://mathworld.wolfram.com/deBruijnSequence.html de Bruijn sequence].
:*   An  OEIS  entry:   [https://oeis.org/A166315 A166315 lexicographically earliest binary de Bruijn sequences, B(2,n)]     --- Not B(10,4),   but possibly relevant.





## C#

{{trans|Kotlin}}

```csharp
using System;
using System.Collections.Generic;
using System.Text;

namespace DeBruijn {
    class Program {
        const string digits = "0123456789";

        static string DeBruijn(int k, int n) {
            var alphabet = digits.Substring(0, k);
            var a = new byte[k * n];
            var seq = new List<byte>();
            void db(int t, int p) {
                if (t > n) {
                    if (n % p == 0) {
                        seq.AddRange(new ArraySegment<byte>(a, 1, p));
                    }
                } else {
                    a[t] = a[t - p];
                    db(t + 1, p);
                    var j = a[t - p] + 1;
                    while (j < k) {
                        a[t] = (byte)j;
                        db(t + 1, t);
                        j++;
                    }
                }
            }
            db(1, 1);
            var buf = new StringBuilder();
            foreach (var i in seq) {
                buf.Append(alphabet[i]);
            }
            var b = buf.ToString();
            return b + b.Substring(0, n - 1);
        }

        static bool AllDigits(string s) {
            foreach (var c in s) {
                if (c < '0' || '9' < c) {
                    return false;
                }
            }
            return true;
        }

        static void Validate(string db) {
            var le = db.Length;
            var found = new int[10_000];
            var errs = new List<string>();
            // Check all strings of 4 consecutive digits within 'db'
            // to see if all 10,000 combinations occur without duplication.
            for (int i = 0; i < le - 3; i++) {
                var s = db.Substring(i, 4);
                if (AllDigits(s)) {
                    int.TryParse(s, out int n);
                    found[n]++;
                }
            }
            for (int i = 0; i < 10_000; i++) {
                if (found[i] == 0) {
                    errs.Add(string.Format("    PIN number {0,4} missing", i));
                } else if (found[i] > 1) {
                    errs.Add(string.Format("    PIN number {0,4} occurs {1} times", i, found[i]));
                }
            }
            var lerr = errs.Count;
            if (lerr == 0) {
                Console.WriteLine("  No errors found");
            } else {
                var pl = lerr == 1 ? "" : "s";
                Console.WriteLine("  {0} error{1} found:", lerr, pl);
                errs.ForEach(Console.WriteLine);
            }
        }

        static string Reverse(string s) {
            char[] arr = s.ToCharArray();
            Array.Reverse(arr);
            return new string(arr);
        }

        static void Main() {
            var db = DeBruijn(10, 4);
            var le = db.Length;

            Console.WriteLine("The length of the de Bruijn sequence is {0}", le);
            Console.WriteLine("\nThe first 130 digits of the de Bruijn sequence are: {0}", db.Substring(0, 130));
            Console.WriteLine("\nThe last 130 digits of the de Bruijn sequence are: {0}", db.Substring(le - 130, 130));

            Console.WriteLine("\nValidating the deBruijn sequence:");
            Validate(db);

            Console.WriteLine("\nValidating the reversed deBruijn sequence:");
            Validate(Reverse(db));

            var bytes = db.ToCharArray();
            bytes[4443] = '.';
            db = new string(bytes);
            Console.WriteLine("\nValidating the overlaid deBruijn sequence:");
            Validate(db);
        }
    }
}
```

{{out}}

```txt
The length of the de Bruijn sequence is 10003

The first 130 digits of the de Bruijn sequence are: 0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350

The last 130 digits of the de Bruijn sequence are: 6898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999000

Validating the deBruijn sequence:
  No errors found

Validating the reversed deBruijn sequence:
  No errors found

Validating the overlaid deBruijn sequence:
  4 errors found:
    PIN number 1459 missing
    PIN number 4591 missing
    PIN number 5814 missing
    PIN number 8145 missing
```



## Go


```go
package main

import (
    "bytes"
    "fmt"
    "strconv"
    "strings"
)

const digits = "0123456789"

func deBruijn(k, n int) string {
    alphabet := digits[0:k]
    a := make([]byte, k*n)
    var seq []byte
    var db func(int, int) // recursive closure
    db = func(t, p int) {
        if t > n {
            if n%p == 0 {
                seq = append(seq, a[1:p+1]...)
            }
        } else {
            a[t] = a[t-p]
            db(t+1, p)
            for j := int(a[t-p] + 1); j < k; j++ {
                a[t] = byte(j)
                db(t+1, t)
            }
        }
    }
    db(1, 1)
    var buf bytes.Buffer
    for _, i := range seq {
        buf.WriteByte(alphabet[i])
    }
    b := buf.String()
    return b + b[0:n-1] // as cyclic append first (n-1) digits
}

func allDigits(s string) bool {
    for _, b := range s {
        if b < '0' || b > '9' {
            return false
        }
    }
    return true
}

func validate(db string) {
    le := len(db)
    found := make([]int, 10000)
    var errs []string
    // Check all strings of 4 consecutive digits within 'db'
    // to see if all 10,000 combinations occur without duplication.
    for i := 0; i < le-3; i++ {
        s := db[i : i+4]
        if allDigits(s) {
            n, _ := strconv.Atoi(s)
            found[n]++
        }
    }
    for i := 0; i < 10000; i++ {
        if found[i] == 0 {
            errs = append(errs, fmt.Sprintf("    PIN number %04d missing", i))
        } else if found[i] > 1 {
            errs = append(errs, fmt.Sprintf("    PIN number %04d occurs %d times", i, found[i]))
        }
    }
    lerr := len(errs)
    if lerr == 0 {
        fmt.Println("  No errors found")
    } else {
        pl := "s"
        if lerr == 1 {
            pl = ""
        }
        fmt.Printf("  %d error%s found:\n", lerr, pl)
        fmt.Println(strings.Join(errs, "\n"))
    }
}

func reverse(s string) string {
    bytes := []byte(s)
    for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
        bytes[i], bytes[j] = bytes[j], bytes[i]
    }
    return string(bytes)
}

func main() {
    db := deBruijn(10, 4)
    le := len(db)
    fmt.Println("The length of the de Bruijn sequence is", le)
    fmt.Println("\nThe first 130 digits of the de Bruijn sequence are:")
    fmt.Println(db[0:130])
    fmt.Println("\nThe last 130 digits of the de Bruijn sequence are:")
    fmt.Println(db[le-130:])
    fmt.Println("\nValidating the de Bruijn sequence:")
    validate(db)

    fmt.Println("\nValidating the reversed de Bruijn sequence:")
    dbr := reverse(db)
    validate(dbr)

    bytes := []byte(db)
    bytes[4443] = '.'
    db = string(bytes)
    fmt.Println("\nValidating the overlaid de Bruijn sequence:")
    validate(db)
}
```


{{out}}

```txt

The length of the de Bruijn sequence is 10003

The first 130 digits of the de Bruijn sequence are:
0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350

The last 130 digits of the de Bruijn sequence are:
6898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999000

Validating the de Bruijn sequence:
  No errors found

Validating the reversed de Bruijn sequence:
  No errors found

Validating the overlaid de Bruijn sequence:
  4 errors found:
    PIN number 1459 missing
    PIN number 4591 missing
    PIN number 5814 missing
    PIN number 8145 missing

```



## Julia


```julia
function debruijn(k::Integer, n::Integer)
    alphabet = b"0123456789abcdefghijklmnopqrstuvwxyz"[1:k]
    a = zeros(UInt8, k * n)
    seq = UInt8[]

    function db(t, p)
        if t > n
            if n % p == 0
                append!(seq, a[2:p+1])
            end
        else
            a[t + 1] = a[t - p + 1]
            db(t + 1, p)
            for j in a[t-p+1]+1:k-1
                a[t + 1] = j
                db(t + 1, t)
            end
        end
    end

    db(1, 1)
    return String([alphabet[i + 1] for i in vcat(seq, seq[1:n-1])])
end

function verifyallPIN(str, k, n, deltaposition=0)
    if deltaposition != 0
        str = str[1:deltaposition-1] * "." * str[deltaposition+1:end]
    end
    result = true
    for i in 1:k^n-1
        pin = string(i, pad=n)
        if !occursin(pin, str)
            println("PIN $pin does not occur in the sequence.")
            result = false
        end
    end
    println("The sequence does ", result ? "" : "not ", "contain all PINs.")
end

const s = debruijn(10, 4)
println("The length of the sequence is $(length(s)). The first 130 digits are:\n",
    s[1:130], "\nand the last 130 digits are:\n", s[end-130:end])
print("Testing sequence: "), verifyallPIN(s, 10, 4)
print("Testing the reversed sequence: "), verifyallPIN(reverse(s), 10, 4)
println("\nAfter replacing 4444th digit with \'.\':"), verifyallPIN(s, 10, 4, 4444)

```
{{out}}

```txt

The length of the sequence is 10003. The first 130 digits are:
0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350
and the last 130 digits are:
76898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999000
Testing sequence: The sequence does contain all PINs.
Testing the reversed sequence: The sequence does contain all PINs.

After replacing 4444th digit with '.':
PIN 1459 does not occur in the sequence.
PIN 4591 does not occur in the sequence.
PIN 5814 does not occur in the sequence.
PIN 8145 does not occur in the sequence.
The sequence does not contain all PINs.

```




## Kotlin

{{trans|Go}}

```scala
const val digits = "0123456789"

fun deBruijn(k: Int, n: Int): String {
    val alphabet = digits.substring(0, k)
    val a = ByteArray(k * n)
    val seq = mutableListOf<Byte>()
    fun db(t: Int, p: Int) {
        if (t > n) {
            if (n % p == 0) {
                seq.addAll(a.sliceArray(1..p).asList())
            }
        } else {
            a[t] = a[t - p]
            db(t + 1, p)
            var j = a[t - p] + 1
            while (j < k) {
                a[t] = j.toByte()
                db(t + 1, t)
                j++
            }
        }
    }
    db(1, 1)
    val buf = StringBuilder()
    for (i in seq) {
        buf.append(alphabet[i.toInt()])
    }
    val b = buf.toString()
    return b + b.subSequence(0, n - 1)
}

fun allDigits(s: String): Boolean {
    for (c in s) {
        if (c < '0' || '9' < c) {
            return false
        }
    }
    return true
}

fun validate(db: String) {
    val le = db.length
    val found = MutableList(10_000) { 0 }
    val errs = mutableListOf<String>()
    // Check all strings of 4 consecutive digits within 'db'
    // to see if all 10,000 combinations occur without duplication.
    for (i in 0 until le - 3) {
        val s = db.substring(i, i + 4)
        if (allDigits(s)) {
            val n = s.toInt()
            found[n]++
        }
    }
    for (i in 0 until 10_000) {
        if (found[i] == 0) {
            errs.add("    PIN number %04d missing".format(i))
        } else if (found[i] > 1) {
            errs.add("    PIN number %04d occurs %d times".format(i, found[i]))
        }
    }
    val lerr = errs.size
    if (lerr == 0) {
        println("  No errors found")
    } else {
        val pl = if (lerr == 1) {
            ""
        } else {
            "s"
        }
        println("  $lerr error$pl found:")
        println(errs.joinToString("\n"))
    }
}

fun main() {
    var db = deBruijn(10, 4)
    val le = db.length

    println("The length of the de Bruijn sequence is $le")
    println("\nThe first 130 digits of the de Bruijn sequence are: ${db.subSequence(0, 130)}")
    println("\nThe last 130 digits of the de Bruijn sequence are: ${db.subSequence(le - 130, le)}")

    println("\nValidating the deBruijn sequence:")
    validate(db)

    println("\nValidating the reversed deBruijn sequence:")
    validate(db.reversed())

    val bytes = db.toCharArray()
    bytes[4443] = '.'
    db = String(bytes)
    println("\nValidating the overlaid deBruijn sequence:")
    validate(db)
}
```

{{out}}

```txt
The length of the de Bruijn sequence is 10003

The first 130 digits of the de Bruijn sequence are: 0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350

The last 130 digits of the de Bruijn sequence are: 6898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999000

Validating the deBruijn sequence:
  No errors found

Validating the reversed deBruijn sequence:
  No errors found

Validating the overlaid deBruijn sequence:
  4 errors found:
    PIN number 1459 missing
    PIN number 4591 missing
    PIN number 5814 missing
    PIN number 8145 missing
```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';

my $seq;
for my $x (0..99) {
    my $a = sprintf '%02d', $x;
    next if substr($a,1,1) < substr($a,0,1);
    $seq .= (substr($a,0,1) == substr($a,1,1)) ? substr($a,0,1) : $a;
    for ($a+1 .. 99) {
        next if substr(sprintf('%02d', $_), 1,1) <= substr($a,0,1);
        $seq .= sprintf "%s%02d", $a, $_;
    }
}
$seq .= '000';

sub check {
    my($seq) = @_;
    my %chk;
    for (0.. -1 + length $seq) { $chk{substr($seq, $_, 4)}++ }
    say 'Missing: ' . join ' ', grep { ! $chk{ sprintf('%04d',$_) } } 0..9999;
    say 'Extra:   ' . join ' ', sort grep { $chk{$_} > 1 } keys %chk;
}

my $n = 130;
say "de Bruijn sequence length: " . length $seq;
say "\nFirst $n characters:\n" . substr($seq, 0, $n );
say "\nLast $n characters:\n"  . substr($seq, -$n, $n);
say "\nIncorrect 4 digit PINs in this sequence:";
check $seq;

say "\nIncorrect 4 digit PINs in the reversed sequence:";
check(reverse $seq);

say "\nReplacing the 4444th digit, '@{[substr($seq,4443,1)]}', with '5'";
substr $seq, 4443, 1, 5;
say "Incorrect 4 digit PINs in the revised sequence:";
check $seq;
```

{{out}}

```txt
de Bruijn sequence length: 10003

First 130 characters:
0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350

Last 130 characters:
6898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999000

Incorrect 4 digit PINs in this sequence:
Missing:
Extra:

Incorrect 4 digit PINs in the reversed sequence:
Missing:
Extra:

Replacing the 4444th digit, '4', with '5'
Incorrect 4 digit PINs in the revised sequence:
Missing: 1459 4591 5814 8145
Extra:   1559 5591 5815 8155
```



## Perl 6

{{works with|Rakudo|2019.07.1}}
Deviates very slightly from the task spec. Generates a randomized de Bruijn sequence and replaces the 4444th digit with a the digit plus 1 mod 10 rather than a '.', mostly so it can demonstrate detection of extra PINs as well as missing ones.


```perl6
# Generate the sequence
my $seq;

for ^100 {
    my $a = .fmt: '%02d';
    next if $a.substr(1,1) < $a.substr(0,1);
    $seq ~= ($a.substr(0,1) == $a.substr(1,1)) ?? $a.substr(0,1) !! $a;
    for +$a ^..^ 100 {
        next if .fmt('%02d').substr(1,1) <= $a.substr(0,1);
        $seq ~= sprintf "%s%02d", $a, $_ ;
    }
}

$seq = $seq.comb.list.rotate((^10000).pick).join;

$seq ~= $seq.substr(0,3);

sub check ($seq) {
    my %chk;
    for ^($seq.chars) { %chk{$seq.substr( $_, 4 )}++ }
    put 'Missing: ', (^9999).grep( { not %chk{ .fmt: '%04d' } } ).fmt: '%04d';
    put 'Extra:   ', %chk.grep( *.value > 1 )».key.sort.fmt: '%04d';
}

## The Task
put "de Bruijn sequence length: " ~ $seq.chars;

put "\nFirst 130 characters:\n" ~ $seq.substr( 0, 130 );

put "\nLast 130 characters:\n" ~ $seq.substr( * - 130 );

put "\nIncorrect 4 digit PINs in this sequence:";
check $seq;

put "\nIncorrect 4 digit PINs in the reversed sequence:";
check $seq.flip;

my $digit = $seq.substr(4443,1);
put "\nReplacing the 4444th digit, ($digit) with { ($digit += 1) %= 10 }";
put "Incorrect 4 digit PINs in the revised sequence:";
$seq.substr-rw(4443,1) = $digit;
check $seq;
```

{{out|Sample output}}

```txt
de Bruijn sequence length: 10003

First 130 characters:
4558455945654566456745684569457545764577457845794585458645874588458945954596459745984599464647464846494655465646574658465946654666

Last 130 characters:
5445644574458445944654466446744684469447544764477447844794485448644874488448944954496449744984499454546454745484549455545564557455

Incorrect 4 digit PINs in this sequence:
Missing: 
Extra:   

Incorrect 4 digit PINs in the reversed sequence:
Missing: 
Extra:   

Replacing the 4444th digit, (1) with 2
Incorrect 4 digit PINs in the revised sequence:
Missing: 0961 1096 6109 9610
Extra:   0962 2096 6209 9620
```



## Phix

{{trans|zkl}}
{{trans|Go}}

```Phix
string deBruijn = ""
for n=0 to 99 do
    string a = sprintf("%02d",n)
    integer {a1,a2} = a
    if a2>=a1 then
        deBruijn &= iff(a1=a2?a1:a)
        for m=n+1 to 99 do
            string ms = sprintf("%02d",m)
            if ms[2]>a1 then
                deBruijn &= a&ms
            end if
        end for
   end if
end for
deBruijn &= "000"
printf(1,"de Bruijn sequence length: %d\n\n",length(deBruijn))
printf(1,"First 130 characters:\n%s\n\n",deBruijn[1..130])
printf(1,"Last 130 characters:\n%s\n\n",deBruijn[-130..-1])
 
function check(string text)
    sequence res = {}
    sequence found = repeat(0,10000)
    integer k
    for i=1 to length(text)-3 do
        k = to_integer(text[i..i+3],-1)+1
        if k!=0 then found[k] += 1 end if
    end for
    for i=1 to 10000 do
        k = found[i]
        if k!=1 then
            string e = sprintf("Pin number %04d ",i-1)
            e &= iff(k=0?"missing":sprintf("occurs %d times",k))
            res = append(res,e)
        end if
    end for
    k = length(res)
    if k=0 then
        res = "No errors found"
    else
        string s = iff(k=1?"":"s")
        res = sprintf("%d error%s found:\n ",{k,s})&join(res,"\n ")
    end if
    return res
end function

printf(1,"Missing 4 digit PINs in this sequence: %s\n", check(deBruijn))
printf(1,"Missing 4 digit PINs in the reversed sequence: %s\n",check(reverse(deBruijn)))
printf(1,"4444th digit in the sequence: %c (setting it to .)\n", deBruijn[4444])
deBruijn[4444] = '.'
printf(1,"Re-running checks: %s\n",check(deBruijn))
```

{{out}}

```txt

de Bruijn sequence length: 10003

First 130 characters:
0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350

Last 130 characters:
6898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999000

Missing 4 digit PINs in this sequence: No errors found
Missing 4 digit PINs in the reversed sequence: No errors found
4444th digit in the sequence: 4 (setting it to .)
Re-running checks: 4 errors found:
 Pin number 1459 missing
 Pin number 4591 missing
 Pin number 5814 missing
 Pin number 8145 missing

```



## Python


```python

# from https://en.wikipedia.org/wiki/De_Bruijn_sequence

def de_bruijn(k, n):
    """
    de Bruijn sequence for alphabet k
    and subsequences of length n.
    """
    try:
        # let's see if k can be cast to an integer;
        # if so, make our alphabet a list
        _ = int(k)
        alphabet = list(map(str, range(k)))

    except (ValueError, TypeError):
        alphabet = k
        k = len(k)

    a = [0] * k * n
    sequence = []

    def db(t, p):
        if t > n:
            if n % p == 0:
                sequence.extend(a[1:p + 1])
        else:
            a[t] = a[t - p]
            db(t + 1, p)
            for j in range(a[t - p] + 1, k):
                a[t] = j
                db(t + 1, t)
    db(1, 1)
    return "".join(alphabet[i] for i in sequence)
    
def validate(db):
    """
    
    Check that all 10,000 combinations of 0-9 are present in 
    De Bruijn string db.
    
    Validating the reversed deBruijn sequence:
      No errors found
    
    Validating the overlaid deBruijn sequence:
      4 errors found:
        PIN number 1459 missing
        PIN number 4591 missing
        PIN number 5814 missing
        PIN number 8145 missing
    
    """
    
    dbwithwrap = db+db[0:3]
    
    digits = '0123456789'
    
    errorstrings = []
    
    for d1 in digits:
        for d2 in digits:
            for d3 in digits:
                for d4 in digits:
                    teststring = d1+d2+d3+d4
                    if teststring not in dbwithwrap:
                        errorstrings.append(teststring)
                        
    if len(errorstrings) > 0:
        print("  "+str(len(errorstrings))+" errors found:")
        for e in errorstrings:
            print("  PIN number "+e+"  missing")
    else:
        print("  No errors found")

db = de_bruijn(10, 4)

print(" ")
print("The length of the de Bruijn sequence is ", str(len(db)))
print(" ")
print("The first 130 digits of the de Bruijn sequence are: "+db[0:130])
print(" ")
print("The last 130 digits of the de Bruijn sequence are: "+db[-130:])
print(" ")
print("Validating the deBruijn sequence:")
validate(db)
dbreversed = db[::-1]
print(" ")
print("Validating the reversed deBruijn sequence:")
validate(dbreversed)
dboverlaid = db[0:4443]+'.'+db[4444:]
print(" ")
print("Validating the overlaid deBruijn sequence:")
validate(dboverlaid)

```

{{out}}

```txt

The length of the de Bruijn sequence is  10000
 
The first 130 digits of the de Bruijn sequence are: 0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350
 
The last 130 digits of the de Bruijn sequence are: 8976898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999
 
Validating the deBruijn sequence:
  No errors found
 
Validating the reversed deBruijn sequence:
  No errors found
 
Validating the overlaid deBruijn sequence:
  4 errors found:
  PIN number 1459  missing
  PIN number 4591  missing
  PIN number 5814  missing
  PIN number 8145  missing

```



## Racket


{{trans|Go}}


```racket
#lang racket
 
(define (de-bruijn k n)
  (define a (make-vector (* k n) 0))
  (define seq '())
  (define (db t p)
    (cond
      [(> t n) (when (= (modulo n p) 0)
                 (set! seq (cons (call-with-values
                                  (thunk (vector->values a 1 (add1 p)))
                                  list)
                                 seq)))]
      [else (vector-set! a t (vector-ref a (- t p)))
            (db (add1 t) p)
            (for ([j (in-range (add1 (vector-ref a (- t p))) k)])
              (vector-set! a t j)
              (db (add1 t) t))]))
  (db 1 1)
  (define seq* (append* (reverse seq)))
  (append seq* (take seq* (sub1 n))))

(define seq (de-bruijn 10 4))
(printf "The length of the de Bruijn sequence is ~a\n\n" (length seq))
(printf "The first 130 digits of the de Bruijn sequence are:\n~a\n\n"
        (take seq 130))
(printf "The last 130 digits of the de Bruijn sequence are:\n~a\n\n"
        (take-right seq 130))

(define (validate name seq)
  (printf "Validating the ~ade Bruijn sequence:\n" name)
  (define expected (for/set ([i (in-range 0 10000)]) i))
  (define actual (for/set ([a (in-list seq)]
                           [b (in-list (rest seq))]
                           [c (in-list (rest (rest seq)))]
                           [d (in-list (rest (rest (rest seq))))])
                   (+ (* 1000 a) (* 100 b) (* 10 c) d)))
  (define diff (set-subtract expected actual))
  (cond
    [(set-empty? diff) (printf "  No errors found\n")]
    [else (for ([n (in-set diff)])
            (printf "  ~a is missing\n" (~a n #:width 4 #:pad-string "0")))])
  (newline))

(validate "" seq)
(validate "reversed " (reverse seq))
(validate "overlaid " (list-update seq 4443 add1))
```


{{out}}


```txt

The length of the de Bruijn sequence is 10003

The first 130 digits of the de Bruijn sequence are:
(0 0 0 0 1 0 0 0 2 0 0 0 3 0 0 0 4 0 0 0 5 0 0 0 6 0 0 0 7 0 0 0 8 0 0 0 9 0 0 1 1 0 0 1 2 0 0 1 3 0 0 1 4 0 0 1 5 0 0 1 6 0 0 1 7 0 0 1 8 0 0 1 9 0 0 2 1 0 0 2 2 0 0 2 3 0 0 2 4 0 0 2 5 0 0 2 6 0 0 2 7 0 0 2 8 0 0 2 9 0 0 3 1 0 0 3 2 0 0 3 3 0 0 3 4 0 0 3 5 0)

The last 130 digits of the de Bruijn sequence are:
(6 8 9 8 6 8 9 9 6 9 6 9 7 7 6 9 7 8 6 9 7 9 6 9 8 7 6 9 8 8 6 9 8 9 6 9 9 7 6 9 9 8 6 9 9 9 7 7 7 7 8 7 7 7 9 7 7 8 8 7 7 8 9 7 7 9 8 7 7 9 9 7 8 7 8 7 9 7 8 8 8 7 8 8 9 7 8 9 8 7 8 9 9 7 9 7 9 8 8 7 9 8 9 7 9 9 8 7 9 9 9 8 8 8 8 9 8 8 9 9 8 9 8 9 9 9 9 0 0 0)

Validating the de Bruijn sequence:
  No errors found

Validating the reversed de Bruijn sequence:
  No errors found

Validating the overlaid de Bruijn sequence:
  1459 is missing
  4591 is missing
  8145 is missing
  5814 is missing

```



## REXX

The   de Bruijn   sequence generated by these REXX programs are identical to the sequence shown on the   ''discussion''   page   (1<sup>st</sup> topic). 
=== hard-coded node to be removed ===

```rexx
/*REXX pgm calculates the  de Bruijn  sequence for all pin numbers  (4 digit decimals). */
$=                                               /*initialize the  de Bruijn  sequence. */
#=10;   lastNode= (#-2)(#-2)(#-1)(#-2)           /*this number is formed when this # ···*/
                                                 /*  ··· is skipped near the cycle end. */
  do j=0  for 10;  $= $ || j;  jj= j || j        /*compose the left half of the numbers.*/
                                                 /* [↓]     "  right  "   "  "     "    */
                                do k=jj+1  to 99;      z= jj || right(k, 2, 0)
                                if z==lastNode  then iterate    /*the last node skipped.*/
                                if pos(z, $)\==0  then iterate  /*# in sequence? Skip it*/
                                $= $ || z        /* ◄─────────────────────────────────┐ */
                                end   /*k*/      /*append a number to the sequence──◄─┘ */

     do r= jj  to (j || 9);  b= right(r, 2, 0)   /*compose the left half of the numbers.*/
     if b==jj  then iterate
     $= $ || right(b, 2, 0)                      /* [↓]     "  right  "   "  "     "    */
                                do k= b+1  to 99;      z= right(b, 2, 0) || right(k, 2, 0)
                                if pos(z, $)\==0  then iterate  /*# in sequence? Skip it*/
                                $= $ || z        /* ◄─────────────────────────────────┐ */
                                end   /*k*/      /*append a number to the sequence──◄─┘ */
     end   /*r*/
  end      /*j*/
                      @deB= 'de Bruijn sequence' /*literal used in some SAY instructions*/
$= $ || left($, 3)        /*append 000*/         /*simulate "wrap-around" de Bruijn seq.*/
       say 'length of the' @deB " is " length($) /*display the length of  de Bruijn seq.*/
say;   say 'first 130 digits of the' @deB":"     /*display the title for the next line. */
       say left($, 130)                          /*display 130 left-most digits of seq. */
say;   say ' last 130 digits of the' @deB":"     /*display the title for the next line. */
       say right($, 130)                         /*display 130 right-most digits of seq.*/
say                                              /*display a blank line.                */
call val $                                       /*call the  VAL  sub for verification. */
               @deB= 'reversed'   @deB           /*next,  we'll check on a reversed seq.*/
$$= reverse($)                                   /*do what a mirror does,  reversify it.*/
call val $$                                      /*call the  VAL  sub for verification. */
$= overlay(., $, 4444)                           /*replace 4,444th digit with a period. */
               @deB= 'overlaid' subword(@deB, 2) /* [↑] this'll cause a validation barf.*/
call val $                                       /*call the  VAL  sub for verification. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
val: parse arg $$$;    e= 0;    _= copies('─',8) /*count of errors (missing PINs) so far*/
     say;      say _ 'validating the'    @deB"." /*display what's happening in the pgm. */
         do pin=0  for 1e4; pin4= right(pin,4,0) /* [↓]  maybe add leading zeros to pin.*/
         if pos(pin4, $$$)\==0  then iterate     /*Was number found?  Just as expected. */
         say 'PIN number '      pin       " wasn't found in"         @deb'.'
         e= e + 1                                /*bump the counter for number of errors*/
         end   /*pin*/                           /* [↑]  validate all 10,000 pin numbers*/
     if e==0  then e= 'No'                       /*Gooder English (sic) the error count.*/
     say _   e   'errors found.'                 /*display the number of errors found.  */
     return
```

{{out|output}}

```txt

length of the de Bruijn sequence  is  10003

first 130 digits of the de Bruijn sequence:
0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350

 last 130 digits of the de Bruijn sequence:
6898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999000


──────── validating the de Bruijn sequence.
──────── No errors found.

──────── validating the reversed de Bruijn sequence.
──────── No errors found.

──────── validating the overlaid de Bruijn sequence.
PIN number  1459  wasn't found in overlaid de Bruijn sequence.
PIN number  4591  wasn't found in overlaid de Bruijn sequence.
PIN number  5814  wasn't found in overlaid de Bruijn sequence.
PIN number  8145  wasn't found in overlaid de Bruijn sequence.
──────── 4 errors found.

```


###  programmatically removing of a node 
 
Programming note:   instead of hardcoding the   ''lastNode''   (that is elided from the sequence),   the 5<sup>th</sup> to the last node could simply be deleted.  

This method slightly bloats the program and slows execution.

```rexx
/*REXX pgm calculates the  de Bruijn  sequence for all pin numbers  (4 digit decimals). */
$=                                               /*initialize the  de Bruijn  sequence. */
   do j=0  for 10;  $= $ j;   jj= j || j          /*compose the left half of the numbers.*/
  $$= space($, 0)                                /* [↓]     "  right  "   "  "     "    */
                                do k=jj+1  to 99;      z= jj || right(k, 2, 0)
                                if pos(z, $$)\==0  then iterate /*# in sequence? Skip it*/
                                $= $ z           /* ◄─────────────────────────────────┐ */
                                end   /*k*/      /*append a number to the sequence──◄─┘ */
  $$= space($, 0)
     do r= jj  to (j || 9);  b= right(r, 2, 0)   /*compose the left half of the numbers.*/
     if b==jj  then iterate
     $= $ right(b, 2, 0)                         /* [↓]     "  right  "   "  "     "    */
     $$= space($, 0);           do k= b+1  to 99;      z= right(b, 2, 0) || right(k, 2, 0)
                                if pos(z, $$)\==0  then iterate /*# in sequence? Skip it*/
                                $= $ z           /* ◄─────────────────────────────────┐ */
                                end   /*k*/      /*append a number to the sequence──◄─┘ */
     $$= space($, 0)
     end   /*r*/
  end      /*j*/

$= delword($, words($)-4, 1)                     /*delete 5th from the last word in $.  */
$= space($, 0)
                      @deB= 'de Bruijn sequence' /*literal used in some SAY instructions*/
$= $ || left($, 3)        /*append 000*/         /*simulate "wrap-around" de Bruijn seq.*/
       say 'length of the' @deB " is " length($) /*display the length of  de Bruijn seq.*/
say;   say 'first 130 digits of the' @deB":"     /*display the title for the next line. */
       say left($, 130)                          /*display 130 left-most digits of seq. */
say;   say ' last 130 digits of the' @deB":"     /*display the title for the next line. */
       say right($, 130)                         /*display 130 right-most digits of seq.*/
call val $                                       /*call the  VAL  sub for verification. */
               @deB= 'reversed'   @deB           /*next,  we'll check on a reversed seq.*/
$r= reverse($)                                   /*do what a mirror does,  reversify it.*/
call val $r                                      /*call the  VAL  sub for verification. */
$= overlay(., $, 4444)                           /*replace 4,444th digit with a period. */
               @deB= 'overlaid' subword(@deB, 2) /* [↑] this'll cause a validation barf.*/
call val $                                       /*call the  VAL  sub for verification. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
val: parse arg $$$;    e= 0;    _= copies('─',8) /*count of errors (missing PINs) so far*/
     say;      say _ 'validating the'    @deB"." /*display what's happening in the pgm. */
         do pin=0  for 1e4; pin4= right(pin,4,0) /* [↓]  maybe add leading zeros to pin.*/
         if pos(pin4, $$$)\==0  then iterate     /*Was number found?  Just as expected. */
         say 'PIN number '      pin       " wasn't found in"         @deb'.'
         e= e + 1                                /*bump the counter for number of errors*/
         end   /*pin*/                           /* [↑]  validate all 10,000 pin numbers*/
     if e==0  then e= 'No'                       /*Gooder English (sic) the error count.*/
     say _   e   'errors found.'                 /*display the number of errors found.  */
     return
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}} 



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Text

Module Module1

    ReadOnly DIGITS As String = "0123456789"

    Function DeBruijn(k As Integer, n As Integer) As String
        Dim alphabet = DIGITS.Substring(0, k)
        Dim a(k * n) As Byte
        Dim seq As New List(Of Byte)
        Dim db As Action(Of Integer, Integer) = Sub(t As Integer, p As Integer)
                                                    If t > n Then
                                                        If n Mod p = 0 Then
                                                            Dim seg = New ArraySegment(Of Byte)(a, 1, p)
                                                            seq.AddRange(seg)
                                                        End If
                                                    Else
                                                        a(t) = a(t - p)
                                                        db(t + 1, p)
                                                        Dim j = a(t - p) + 1
                                                        While j < k
                                                            a(t) = j
                                                            db(t + 1, t)
                                                            j += 1
                                                        End While
                                                    End If
                                                End Sub
        db(1, 1)
        Dim buf As New StringBuilder
        For Each i In seq
            buf.Append(alphabet(i))
        Next
        Dim b = buf.ToString
        Return b + b.Substring(0, n - 1)
    End Function

    Function AllDigits(s As String) As Boolean
        For Each c In s
            If c < "0" OrElse "9" < c Then
                Return False
            End If
        Next
        Return True
    End Function

    Sub Validate(db As String)
        Dim le = db.Length
        Dim found(10000) As Integer
        Dim errs As New List(Of String)
        ' Check all strings of 4 consecutive digits within 'db'
        ' to see if all 10,000 combinations occur without duplication.
        For i = 1 To le - 3
            Dim s = db.Substring(i - 1, 4)
            If (AllDigits(s)) Then
                Dim n As Integer = Nothing
                Integer.TryParse(s, n)
                found(n) += 1
            End If
        Next
        For i = 1 To 10000
            If found(i - 1) = 0 Then
                errs.Add(String.Format("    PIN number {0,4} missing", i - 1))
            ElseIf found(i - 1) > 1 Then
                errs.Add(String.Format("    PIN number {0,4} occurs {1} times", i - 1, found(i - 1)))
            End If
        Next
        Dim lerr = errs.Count
        If lerr = 0 Then
            Console.WriteLine("  No errors found")
        Else
            Dim pl = If(lerr = 1, "", "s")
            Console.WriteLine("  {0} error{1} found:", lerr, pl)
            errs.ForEach(Sub(x) Console.WriteLine(x))
        End If
    End Sub

    Function Reverse(s As String) As String
        Dim arr = s.ToCharArray
        Array.Reverse(arr)
        Return New String(arr)
    End Function

    Sub Main()
        Dim db = DeBruijn(10, 4)
        Dim le = db.Length

        Console.WriteLine("The length of the de Bruijn sequence is {0}", le)
        Console.WriteLine(vbNewLine + "The first 130 digits of the de Bruijn sequence are: {0}", db.Substring(0, 130))
        Console.WriteLine(vbNewLine + "The last 130 digits of the de Bruijn sequence are: {0}", db.Substring(le - 130, 130))

        Console.WriteLine(vbNewLine + "Validating the deBruijn sequence:")
        Validate(db)

        Console.WriteLine(vbNewLine + "Validating the reversed deBruijn sequence:")
        Validate(Reverse(db))

        Dim bytes = db.ToCharArray
        bytes(4443) = "."
        db = New String(bytes)
        Console.WriteLine(vbNewLine + "Validating the overlaid deBruijn sequence:")
        Validate(db)
    End Sub

End Module
```

{{out}}

```txt
The first 130 digits of the de Bruijn sequence are: 0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350

The last 130 digits of the de Bruijn sequence are: 6898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999000

Validating the deBruijn sequence:
  No errors found

Validating the reversed deBruijn sequence:
  No errors found

Validating the overlaid deBruijn sequence:
  4 errors found:
    PIN number 1459 missing
    PIN number 4591 missing
    PIN number 5814 missing
    PIN number 8145 missing
```



## zkl

{{trans|Perl6}}

```zkl
dbSeq:=Data();	// a byte/character buffer
foreach n in (100){
   a,a01,a11 := "%02d".fmt(n), a[0,1], a[1,1];
   if(a11<a01) continue;
   dbSeq.append( if(a01==a11) a01 else a );
   foreach m in ([n+1 .. 99]){
      if("%02d".fmt(m)[1,1] <= a01) continue;
      dbSeq.append("%s%02d".fmt(a,m));
   }
}
dbSeq.append("000");
```


```zkl
seqText:=dbSeq.text;
println("de Bruijn sequence length: ",dbSeq.len());

println("\nFirst 130 characters:\n",seqText[0,130]);
println("\nLast 130 characters:\n", seqText[-130,*]);

fcn chk(seqText){
   chk:=Dictionary();
   foreach n in ([0..seqText.len()-1]){ chk[seqText[n,4]]=True }
   (9999).pump(List,"%04d".fmt,'wrap(k){ if(chk.holds(k)) Void.Skip else k })
}
println("\nMissing 4 digit PINs in this sequence: ", chk(seqText).concat(" "));
print("Missing 4 digit PINs in the reversed sequence: ",chk(seqText.reverse()).concat(" "));

println("\n4444th digit in the sequence: ", seqText[4443]);
dbSeq[4443]=".";
println("Setting the 4444th digit and reruning checks: ",chk(dbSeq.text).concat(" "));
```

{{out}}

```txt

de Bruijn sequence length: 10003

First 130 characters:
0000100020003000400050006000700080009001100120013001400150016001700180019002100220023002400250026002700280029003100320033003400350

Last 130 characters:
6898689969697769786979698769886989699769986999777787779778877897798779978787978887889789878997979887989799879998888988998989999000

Missing 4 digit PINs in this sequence: 
Missing 4 digit PINs in the reversed sequence: 
4444th digit in the sequence: 4
Setting the 4444th digit and reruning checks: 1459 4591 5814 8145

```

