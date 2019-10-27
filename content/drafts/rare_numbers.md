+++
title = "Rare numbers"
description = ""
date = 2019-10-20T18:28:15Z
aliases = []
[extra]
id = 22520
[taxonomies]
categories = []
tags = []
+++

{{task|Mathematics}}

;Definitions and restrictions:
'''Rare'''   numbers are positive integers   <big> '''n''' </big>   where:
:::*   <big> '''n'''     </big>   is expressed in base ten
:::*   <big> '''r'''     </big>   is the reverse of   <big> '''n''' </big>     (decimal digits)
:::*   <big> '''n'''     </big>   must be non-palindromic   <big>('''n''' &ne; '''r''')</big>
:::*   <big> '''(n+r)''' </big>   is the   '''sum'''
:::*   <big> '''(n-r)''' </big>   is the   '''difference'''   and must be positive
:::*   the   '''sum'''   and the   '''difference'''   must be perfect squares


;Task:
:*   find and show the first   '''5'''   ''rare''   numbers
:*   find and show the first   '''8'''   ''rare''   numbers                           (''optional'')
:*   find and show more   ''rare''   numbers                      (''stretch goal'')


Show all output here, on this page.


;References:
:*   an   OEIS   entry:   [http://oeis.org/A035519 A035519          rare numbers].
:*   an   OEIS   entry:   [http://oeis.org/A059755 A059755   odd rare numbers].
:*   planetmath entry:              [https://www.planetmath.org/RareNumbers rare numbers].       (some hints)
:*   author's  website:        [http://www.shyamsundergupta.com/rare.htm rare numbers]    by Shyam Sunder Gupta.     (lots of hints and some observations).




## C#

{{trans|Go}}
Converted to unsigned longs in order to reach 19 digits.

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using static System.Console;
using UI = System.UInt64;
using LST = System.Collections.Generic.List<System.Collections.Generic.List<sbyte>>;
using Lst = System.Collections.Generic.List<sbyte>;
using DT = System.DateTime;

class Program {

    const sbyte MxD = 19;

    public struct term { public UI coeff; public sbyte a, b;
        public term(UI c, int a_, int b_) { coeff = c; a = (sbyte)a_; b = (sbyte)b_; } }

    static int[] digs;   static List<UI> res;   static sbyte count = 0;
    static DT st; static List<List<term>> tLst; static List<LST> lists;
    static Dictionary<int, LST> fml, dmd; static Lst dl, zl, el, ol, il;
    static bool odd; static int nd, nd2; static LST ixs;
    static int[] cnd, di; static LST dis; static UI Dif;

    // converts digs array to the "difference"
    static UI ToDif() { UI r = 0; for (int i = 0; i < digs.Length; i++)
            r = r * 10 + (uint)digs[i]; return r; }
    
    // converts digs array to the "sum"
    static UI ToSum() { UI r = 0; for (int i = digs.Length - 1; i >= 0; i--)
            r = r * 10 + (uint)digs[i]; return Dif + (r << 1); }

    // determines if the nmbr is square or not
    static bool IsSquare(UI nmbr) { if ((0x202021202030213 & (1 << (int)(nmbr & 63))) != 0)
        { UI r = (UI)Math.Sqrt((double)nmbr); return r * r == nmbr; } return false; }

    // returns sequence of sbytes
    static Lst Seq(sbyte from, int to, sbyte stp = 1) { Lst res = new Lst();
        for (sbyte item = from; item <= to; item += stp) res.Add(item); return res; }

    // Recursive closure to generate (n+r) candidates from (n-r) candidates
    static void Fnpr(int lev) { if (lev == dis.Count) { digs[ixs[0][0]] = fml[cnd[0]][di[0]][0];
            digs[ixs[0][1]] = fml[cnd[0]][di[0]][1]; int le = di.Length, i = 1;
            if (odd) digs[nd >> 1] = di[--le]; foreach (sbyte d in di.Skip(1).Take(le - 1)) {
                digs[ixs[i][0]] = dmd[cnd[i]][d][0]; digs[ixs[i][1]] = dmd[cnd[i++]][d][1]; }
            if (!IsSquare(ToSum())) return; res.Add(ToDif()); WriteLine("{0,16:n0}{1,4}   ({2:n0})",
                (DT.Now - st).TotalMilliseconds, ++count, res.Last()); }
        else foreach (var n in dis[lev]) { di[lev] = n; Fnpr(lev + 1); } }

    // Recursive closure to generate (n-r) candidates with a given number of digits.
    static void Fnmr (LST list, int lev) { if (lev == list.Count) { Dif = 0; sbyte i = 0;
            foreach (var t in tLst[nd2]) { if (cnd[i] < 0) Dif -= t.coeff * (UI)(-cnd[i++]);
                else Dif += t.coeff * (UI)cnd[i++]; } if (Dif <= 0 || !IsSquare(Dif)) return;
            dis = new LST { Seq(0, fml[cnd[0]].Count - 1) };
            foreach (int ii in cnd.Skip(1)) dis.Add(Seq(0, dmd[ii].Count - 1));
            if (odd) dis.Add(il); di = new int[dis.Count]; Fnpr(0);
        } else foreach(sbyte n in list[lev]) { cnd[lev] = n; Fnmr(list, lev + 1); } }

    static void init() { UI pow = 1;
        // terms of (n-r) expression for number of digits from 2 to maxDigits
        tLst = new List<List<term>>(); foreach (int r in Seq(2, MxD)) {
            List<term> terms = new List<term>(); pow *= 10; UI p1 = pow, p2 = 1;
            for (int i1 = 0, i2 = r - 1; i1 < i2; i1++, i2--) {
                terms.Add(new term(p1 - p2, i1, i2)); p1 /= 10; p2 *= 10; }
            tLst.Add(terms); }
        //  map of first minus last digits for 'n' to pairs giving this value
        fml = new Dictionary<int, LST> {
            [0] = new LST { new Lst { 2, 2 }, new Lst { 8, 8 } },
            [1] = new LST { new Lst { 6, 5 }, new Lst { 8, 7 } },
            [4] = new LST { new Lst { 4, 0 } },
            [6] = new LST { new Lst { 6, 0 }, new Lst { 8, 2 } } };
        // map of other digit differences for 'n' to pairs giving this value
        dmd = new Dictionary<int, LST>();
        for (sbyte i = 0; i < 10; i++) for (sbyte j = 0, d = i; j < 10; j++, d--) {
                if (dmd.ContainsKey(d)) dmd[d].Add(new Lst { i, j });
                else dmd[d] = new LST { new Lst { i, j } }; }
        dl = Seq(-9, 9);    // all differences
        zl = Seq( 0, 0);    // zero differences only
        el = Seq(-8, 8, 2); // even differences only
        ol = Seq(-9, 9, 2); // odd differences only
        il = Seq( 0, 9); lists = new List<LST>();
        foreach (sbyte f in fml.Keys) lists.Add(new LST { new Lst { f } }); }

    static void Main(string[] args) { init(); res = new List<UI>(); st = DT.Now; count = 0;
        WriteLine("{0,5}{1,12}{2,4}{3,14}", "digs", "elapsed(ms)", "R/N", "Unordered Rare Numbers");
        for (nd = 2, nd2 = 0, odd = false; nd <= MxD; nd++, nd2++, odd = !odd) { digs = new int[nd];
            if (nd == 4) { lists[0].Add(zl); lists[1].Add(ol); lists[2].Add(el); lists[3].Add(ol); }
            else if (tLst[nd2].Count > lists[0].Count) foreach (LST list in lists) list.Add(dl);
            ixs = new LST(); 
            foreach (term t in tLst[nd2]) ixs.Add(new Lst { t.a, t.b });
            foreach (LST list in lists) { cnd = new int[list.Count]; Fnmr(list, 0); }
            WriteLine("  {0,2}  {1,10:n0}", nd, (DT.Now - st).TotalMilliseconds); }
        res.Sort();
        WriteLine("\nThe {0} rare numbers with up to {1} digits are:", res.Count, MxD);
        count = 0; foreach (var rare in res) WriteLine("{0,2}:{1,27:n0}", ++count, rare);
        if (System.Diagnostics.Debugger.IsAttached) ReadKey(); }
}
```

{{out}}
Results from a core i7-7700 @ 3.6Ghz.  This C# version isn't as fast as the Go version using the same hardware.  C# computes up to 17, 18 and 19 digits in under 9 minutes, 1 2/3 hours and over 2 1/2 hours respectively.  (Go is about 6 minutes, 1 1/4 hours, and under 2 hours). 

The '''''long'''-to-'''ulong''''' conversion isn't causing the reduced performance, C# has more overhead as compared to Go. This C# version can easily be converted to use BigIntegers to go beyond 19 digits, but becomes around eight times slower. (ugh!)
<pre style="height:64ex;overflow:scroll"> digs elapsed(ms) R/N  Rare Numbers
              27   1   (65)
   2          28
   3          28
   4          29
   5          29
              29   2   (621,770)
   6          29
   7          30
   8          34
              34   3   (281,089,082)
   9          36
              36   4   (2,022,652,202)
              61   5   (2,042,832,002)
  10         121
  11         176
             448   6   (872,546,974,178)
             481   7   (872,568,754,178)
             935   8   (868,591,084,757)
  12       1,232
           1,577   9   (6,979,302,951,885)
  13       2,087
           6,274  10   (20,313,693,904,202)
           6,351  11   (20,313,839,704,202)
           8,039  12   (20,331,657,922,202)
           8,292  13   (20,331,875,722,202)
           9,000  14   (20,333,875,702,202)
          21,212  15   (40,313,893,704,200)
          21,365  16   (40,351,893,720,200)
  14      23,898
          23,964  17   (200,142,385,731,002)
          24,198  18   (221,462,345,754,122)
          27,241  19   (816,984,566,129,618)
          28,834  20   (245,518,996,076,442)
          29,074  21   (204,238,494,066,002)
          29,147  22   (248,359,494,187,442)
          29,476  23   (244,062,891,224,042)
          35,481  24   (403,058,392,434,500)
          35,721  25   (441,054,594,034,340)
  15      38,231
          92,116  26   (2,133,786,945,766,212)
         113,469  27   (2,135,568,943,984,212)
         116,787  28   (8,191,154,686,620,818)
         119,647  29   (8,191,156,864,620,818)
         120,912  30   (2,135,764,587,964,212)
         122,735  31   (2,135,786,765,764,212)
         127,126  32   (8,191,376,864,400,818)
         141,793  33   (2,078,311,262,161,202)
         179,832  34   (8,052,956,026,592,517)
         184,647  35   (8,052,956,206,592,517)
         221,279  36   (8,650,327,689,541,457)
         223,721  37   (8,650,349,867,341,457)
         225,520  38   (6,157,577,986,646,405)
         273,238  39   (4,135,786,945,764,210)
         312,969  40   (6,889,765,708,183,410)
  16     316,349
         322,961  41   (86,965,750,494,756,968)
         323,958  42   (22,542,040,692,914,522)
         502,805  43   (67,725,910,561,765,640)
  17     519,583
         576,058  44   (284,684,666,566,486,482)
         707,530  45   (225,342,456,863,243,522)
         756,188  46   (225,342,458,663,243,522)
         856,346  47   (225,342,478,643,243,522)
         928,546  48   (284,684,868,364,486,482)
       1,311,170  49   (871,975,098,681,469,178)
       2,031,664  50   (865,721,270,017,296,468)
       2,048,209  51   (297,128,548,234,950,692)
       2,057,281  52   (297,128,722,852,950,692)
       2,164,878  53   (811,865,096,390,477,018)
       2,217,508  54   (297,148,324,656,930,692)
       2,242,999  55   (297,148,546,434,930,692)
       2,576,805  56   (898,907,259,301,737,498)
       3,169,675  57   (631,688,638,047,992,345)
       3,200,223  58   (619,431,353,040,136,925)
       3,482,517  59   (619,631,153,042,134,925)
       3,550,566  60   (633,288,858,025,996,145)
       3,623,653  61   (633,488,632,647,994,145)
       4,605,503  62   (653,488,856,225,994,125)
       5,198,241  63   (497,168,548,234,910,690)
  18   6,028,721
       6,130,826  64   (2,551,755,006,254,571,552)
       6,152,283  65   (2,702,373,360,882,732,072)
       6,424,945  66   (2,825,378,427,312,735,282)
       6,447,566  67   (8,066,308,349,502,036,608)
       6,677,925  68   (2,042,401,829,204,402,402)
       6,725,119  69   (2,420,424,089,100,600,242)
       6,843,016  70   (8,320,411,466,598,809,138)
       7,161,527  71   (8,197,906,905,009,010,818)
       7,198,112  72   (2,060,303,819,041,450,202)
       7,450,028  73   (8,200,756,128,308,135,597)
       7,881,502  74   (6,531,727,101,458,000,045)
       9,234,318  75   (6,988,066,446,726,832,640)
  19   9,394,513

The 75 rare numbers with up to 19 digits are:
 1:                         65
 2:                    621,770
 3:                281,089,082
 4:              2,022,652,202
 5:              2,042,832,002
 6:            868,591,084,757
 7:            872,546,974,178
 8:            872,568,754,178
 9:          6,979,302,951,885
10:         20,313,693,904,202
11:         20,313,839,704,202
12:         20,331,657,922,202
13:         20,331,875,722,202
14:         20,333,875,702,202
15:         40,313,893,704,200
16:         40,351,893,720,200
17:        200,142,385,731,002
18:        204,238,494,066,002
19:        221,462,345,754,122
20:        244,062,891,224,042
21:        245,518,996,076,442
22:        248,359,494,187,442
23:        403,058,392,434,500
24:        441,054,594,034,340
25:        816,984,566,129,618
26:      2,078,311,262,161,202
27:      2,133,786,945,766,212
28:      2,135,568,943,984,212
29:      2,135,764,587,964,212
30:      2,135,786,765,764,212
31:      4,135,786,945,764,210
32:      6,157,577,986,646,405
33:      6,889,765,708,183,410
34:      8,052,956,026,592,517
35:      8,052,956,206,592,517
36:      8,191,154,686,620,818
37:      8,191,156,864,620,818
38:      8,191,376,864,400,818
39:      8,650,327,689,541,457
40:      8,650,349,867,341,457
41:     22,542,040,692,914,522
42:     67,725,910,561,765,640
43:     86,965,750,494,756,968
44:    225,342,456,863,243,522
45:    225,342,458,663,243,522
46:    225,342,478,643,243,522
47:    284,684,666,566,486,482
48:    284,684,868,364,486,482
49:    297,128,548,234,950,692
50:    297,128,722,852,950,692
51:    297,148,324,656,930,692
52:    297,148,546,434,930,692
53:    497,168,548,234,910,690
54:    619,431,353,040,136,925
55:    619,631,153,042,134,925
56:    631,688,638,047,992,345
57:    633,288,858,025,996,145
58:    633,488,632,647,994,145
59:    653,488,856,225,994,125
60:    811,865,096,390,477,018
61:    865,721,270,017,296,468
62:    871,975,098,681,469,178
63:    898,907,259,301,737,498
64:  2,042,401,829,204,402,402
65:  2,060,303,819,041,450,202
66:  2,420,424,089,100,600,242
67:  2,551,755,006,254,571,552
68:  2,702,373,360,882,732,072
69:  2,825,378,427,312,735,282
70:  6,531,727,101,458,000,045
71:  6,988,066,446,726,832,640
72:  8,066,308,349,502,036,608
73:  8,197,906,905,009,010,818
74:  8,200,756,128,308,135,597
75:  8,320,411,466,598,809,138
```


=={{header|F_Sharp|F#}}==

### The Function

This solution demonstrates the concept described in [[Talk:Rare_numbers#30_mins_not_30_years]]. It doesn't use [[Cartesian_product_of_two_or_more_lists#Extra_Credit]]

```fsharp

// Find all Rare numbers with a digits. Nigel Galloway: September 18th., 2019. 
let rareNums a=
  let tN=set[1L;4L;5L;6L;9L]
  let izPS g=let n=(float>>sqrt>>int64)g in n*n=g
  let n=[for n in [0..a/2-1] do yield ((pown 10L (a-n-1))-(pown 10L n))]|>List.rev
  let rec fN i g e=seq{match e with 0->yield g |e->for n in i do yield! fN [-9L..9L] (n::g) (e-1)}|>Seq.filter(fun g->let g=Seq.map2(*) n g|>Seq.sum in g>0L && izPS g)
  let rec fG n i g e l=seq{
    match l with
     h::t->for l in max 0L (0L-h)..min 9L (9L-h) do if e>1L||l=0L||tN.Contains((2L*l+h)%10L) then yield! fG (n+l*e+(l+h)*g) (i+l*g+(l+h)*e) (g/10L) (e*10L) t
    |_->if n>(pown 10L (a-1)) then for l in (if a%2=0 then [0L] else [0L..9L]) do let g=l*(pown 10L (a/2)) in if izPS (n+i+2L*g) then yield (i+g,n+g)} 
  fN [0L..9L] [] (a/2) |> Seq.collect(List.rev >> fG 0L 0L (pown 10L (a-1)) 1L)

```



### 43 down


```fsharp

let test n=
  let t = System.Diagnostics.Stopwatch.StartNew()
  for n in (rareNums n) do printfn "%A" n
  t.Stop()
  printfn "Elapsed Time: %d ms for length %d" t.ElapsedMilliseconds n

[2..17] |> Seq.iter test

```

{{out}}

```txt

(56L, 65L)
Elapsed Time: 31 ms for length 2
Elapsed Time: 0 ms for length 3
Elapsed Time: 0 ms for length 4
Elapsed Time: 0 ms for length 5
(77126L, 621770L)
Elapsed Time: 6 ms for length 6
Elapsed Time: 6 ms for length 7
Elapsed Time: 113 ms for length 8
(280980182L, 281089082L)
Elapsed Time: 72 ms for length 9
(2022562202L, 2022652202L)
(2002382402L, 2042832002L)
Elapsed Time: 1525 ms for length 10
Elapsed Time: 1351 ms for length 11
(871479645278L, 872546974178L)
(871457865278L, 872568754178L)
(757480195868L, 868591084757L)
Elapsed Time: 27990 ms for length 12
(5881592039796L, 6979302951885L)
Elapsed Time: 26051 ms for length 13
(20240939631302L, 20313693904202L)
(20240793831302L, 20313839704202L)
(20222975613302L, 20331657922202L)
(20222757813302L, 20331875722202L)
(20220757833302L, 20333875702202L)
(240739831304L, 40313893704200L)
(202739815304L, 40351893720200L)
Elapsed Time: 552922 ms for length 14
(200137583241002L, 200142385731002L)
(221457543264122L, 221462345754122L)
(816921665489618L, 816984566129618L)
(244670699815542L, 245518996076442L)
(200660494832402L, 204238494066002L)
(244781494953842L, 248359494187442L)
(240422198260442L, 244062891224042L)
(5434293850304L, 403058392434500L)
(43430495450144L, 441054594034340L)
Elapsed Time: 512282 ms for length 15
(2126675496873312L, 2133786945766212L)
(2124893498655312L, 2135568943984212L)
(8180266864511918L, 8191154686620818L)
(8180264686511918L, 8191156864620818L)
(2124697854675312L, 2135764587964212L)
(2124675676875312L, 2135786765764212L)
(8180044686731918L, 8191376864400818L)
(2021612621138702L, 2078311262161202L)
(7152956206592508L, 8052956026592517L)
(7152956026592508L, 8052956206592517L)
(7541459867230568L, 8650327689541457L)
(7541437689430568L, 8650349867341457L)
(5046466897757516L, 6157577986646405L)
(124675496875314L, 4135786945764210L)
(143818075679886L, 6889765708183410L)
Elapsed Time: 11568713 ms for length 16
(86965749405756968L, 86965750494756968L)
(22541929604024522L, 22542040692914522L)
(4656716501952776L, 67725910561765640L)
Elapsed Time: 11275839 ms for length 17

```



## Go

This uses many of the hints within Shyam Sunder Gupta's webpage combined with Nigel Galloway's general approach (see Talk page) of working from (n-r) and deducing the Rare numbers with various numbers of digits from there.

As the algorithm used does not generate the Rare numbers in order, a sorted list is also printed.

```go
package main

import (
    "fmt"
    "math"
    "sort"
    "time"
)

type term struct {
    coeff    int64
    ix1, ix2 int8
}

const maxDigits = 18

func toInt64(digits []int8, reverse bool) int64 {
    sum := int64(0)
    if !reverse {
        for i := 0; i < len(digits); i++ {
            sum = sum*10 + int64(digits[i])
        }
    } else {
        for i := len(digits) - 1; i >= 0; i-- {
            sum = sum*10 + int64(digits[i])
        }
    }
    return sum
}

func isSquare(n int64) bool {
    if 0x202021202030213&(1<<(n&63)) != 0 {
        root := int64(math.Sqrt(float64(n)))
        return root*root == n
    }
    return false
}

func seq(from, to, step int8) []int8 {
    var res []int8
    for i := from; i <= to; i += step {
        res = append(res, i)
    }
    return res
}

func commatize(n int64) string {
    s := fmt.Sprintf("%d", n)
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    return s
}

func main() {
    start := time.Now()
    pow := int64(1)
    fmt.Println("Aggregate timings to process all numbers up to:")
    // terms of (n-r) expression for number of digits from 2 to maxDigits
    allTerms := make([][]term, maxDigits-1)
    for r := 2; r <= maxDigits; r++ {
        var terms []term
        pow *= 10
        pow1, pow2 := pow, int64(1)
        for i1, i2 := int8(0), int8(r-1); i1 < i2; i1, i2 = i1+1, i2-1 {
            terms = append(terms, term{pow1 - pow2, i1, i2})
            pow1 /= 10
            pow2 *= 10
        }
        allTerms[r-2] = terms
    }
    //  map of first minus last digits for 'n' to pairs giving this value
    fml := map[int8][][]int8{
        0: {{2, 2}, {8, 8}},
        1: {{6, 5}, {8, 7}},
        4: {{4, 0}},
        6: {{6, 0}, {8, 2}},
    }
    // map of other digit differences for 'n' to pairs giving this value
    dmd := make(map[int8][][]int8)
    for i := int8(0); i < 100; i++ {
        a := []int8{i / 10, i % 10}
        d := a[0] - a[1]
        dmd[d] = append(dmd[d], a)
    }
    fl := []int8{0, 1, 4, 6}
    dl := seq(-9, 9, 1) // all differences
    zl := []int8{0}     // zero differences only
    el := seq(-8, 8, 2) // even differences only
    ol := seq(-9, 9, 2) // odd differences only
    il := seq(0, 9, 1)
    var rares []int64
    lists := make([][][]int8, 4)
    for i, f := range fl {
        lists[i] = [][]int8{{f}}
    }
    var digits []int8
    count := 0

    // Recursive closure to generate (n+r) candidates from (n-r) candidates
    // and hence find Rare numbers with a given number of digits.
    var fnpr func(cand, di []int8, dis [][]int8, indices [][2]int8, nmr int64, nd, level int)
    fnpr = func(cand, di []int8, dis [][]int8, indices [][2]int8, nmr int64, nd, level int) {
        if level == len(dis) {
            digits[indices[0][0]] = fml[cand[0]][di[0]][0]
            digits[indices[0][1]] = fml[cand[0]][di[0]][1]
            le := len(di)
            if nd%2 == 1 {
                le--
                digits[nd/2] = di[le]
            }
            for i, d := range di[1:le] {
                digits[indices[i+1][0]] = dmd[cand[i+1]][d][0]
                digits[indices[i+1][1]] = dmd[cand[i+1]][d][1]
            }
            r := toInt64(digits, true)
            npr := nmr + 2*r
            if !isSquare(npr) {
                return
            }
            count++
            fmt.Printf("     R/N %2d:", count)
            fmt.Printf("  %9s ms", commatize(time.Since(start).Milliseconds()))
            n := toInt64(digits, false)
            fmt.Printf("  (%s)\n", commatize(n))
            rares = append(rares, n)
        } else {
            for _, num := range dis[level] {
                di[level] = num
                fnpr(cand, di, dis, indices, nmr, nd, level+1)
            }
        }
    }

    // Recursive closure to generate (n-r) candidates with a given number of digits.
    var fnmr func(cand []int8, list [][]int8, indices [][2]int8, nd, level int)
    fnmr = func(cand []int8, list [][]int8, indices [][2]int8, nd, level int) {
        if level == len(list) {
            nmr := int64(0)
            for i, t := range allTerms[nd-2] {
                nmr += t.coeff * int64(cand[i])
            }
            if nmr <= 0 || !isSquare(nmr) {
                return
            }
            var dis [][]int8
            dis = append(dis, seq(0, int8(len(fml[cand[0]]))-1, 1))
            for i := 1; i < len(cand); i++ {
                dis = append(dis, seq(0, int8(len(dmd[cand[i]]))-1, 1))
            }
            if nd%2 == 1 {
                dis = append(dis, il)
            }
            di := make([]int8, len(dis))
            fnpr(cand, di, dis, indices, nmr, nd, 0)
        } else {
            for _, num := range list[level] {
                cand[level] = num
                fnmr(cand, list, indices, nd, level+1)
            }
        }
    }

    for nd := 2; nd <= maxDigits; nd++ {
        digits = make([]int8, nd)
        if nd == 4 {
            lists[0] = append(lists[0], zl)
            lists[1] = append(lists[1], ol)
            lists[2] = append(lists[2], el)
            lists[3] = append(lists[3], ol)
        } else if len(allTerms[nd-2]) > len(lists[0]) {
            for i := 0; i < 4; i++ {
                lists[i] = append(lists[i], dl)
            }
        }
        var indices [][2]int8
        for _, t := range allTerms[nd-2] {
            indices = append(indices, [2]int8{t.ix1, t.ix2})
        }
        for _, list := range lists {
            cand := make([]int8, len(list))
            fnmr(cand, list, indices, nd, 0)
        }
        fmt.Printf("  %2d digits:  %9s ms\n", nd, commatize(time.Since(start).Milliseconds()))
    }

    sort.Slice(rares, func(i, j int) bool { return rares[i] < rares[j] })
    fmt.Printf("\nThe rare numbers with up to %d digits are:\n", maxDigits)
    for i, rare := range rares {
        fmt.Printf("  %2d:  %23s\n", i+1, commatize(rare))
    }
}
```


{{output}}
Timings are for an Intel Core i7-8565U machine with 32GB RAM running Go 1.13.1 on Ubuntu 18.04.

```txt

Aggregate timings to process all numbers up to:
     R/N  1:          0 ms  (65)
   2 digits:          0 ms
   3 digits:          0 ms
   4 digits:          0 ms
   5 digits:          0 ms
     R/N  2:          1 ms  (621,770)
   6 digits:          1 ms
   7 digits:          2 ms
   8 digits:         15 ms
     R/N  3:         15 ms  (281,089,082)
   9 digits:         20 ms
     R/N  4:         20 ms  (2,022,652,202)
     R/N  5:         59 ms  (2,042,832,002)
  10 digits:         99 ms
  11 digits:        137 ms
     R/N  6:        361 ms  (872,546,974,178)
     R/N  7:        389 ms  (872,568,754,178)
     R/N  8:        738 ms  (868,591,084,757)
  12 digits:        888 ms
     R/N  9:      1,130 ms  (6,979,302,951,885)
  13 digits:      1,446 ms
     R/N 10:      4,990 ms  (20,313,693,904,202)
     R/N 11:      5,058 ms  (20,313,839,704,202)
     R/N 12:      6,475 ms  (20,331,657,922,202)
     R/N 13:      6,690 ms  (20,331,875,722,202)
     R/N 14:      7,293 ms  (20,333,875,702,202)
     R/N 15:     16,685 ms  (40,313,893,704,200)
     R/N 16:     16,818 ms  (40,351,893,720,200)
  14 digits:     17,855 ms
     R/N 17:     17,871 ms  (200,142,385,731,002)
     R/N 18:     18,079 ms  (221,462,345,754,122)
     R/N 19:     20,774 ms  (816,984,566,129,618)
     R/N 20:     22,155 ms  (245,518,996,076,442)
     R/N 21:     22,350 ms  (204,238,494,066,002)
     R/N 22:     22,413 ms  (248,359,494,187,442)
     R/N 23:     22,687 ms  (244,062,891,224,042)
     R/N 24:     26,698 ms  (403,058,392,434,500)
     R/N 25:     26,905 ms  (441,054,594,034,340)
  15 digits:     27,932 ms
     R/N 26:     77,599 ms  (2,133,786,945,766,212)
     R/N 27:     96,932 ms  (2,135,568,943,984,212)
     R/N 28:     99,869 ms  (8,191,154,686,620,818)
     R/N 29:    102,401 ms  (8,191,156,864,620,818)
     R/N 30:    103,535 ms  (2,135,764,587,964,212)
     R/N 31:    105,255 ms  (2,135,786,765,764,212)
     R/N 32:    109,232 ms  (8,191,376,864,400,818)
     R/N 33:    122,372 ms  (2,078,311,262,161,202)
     R/N 34:    148,814 ms  (8,052,956,026,592,517)
     R/N 35:    153,226 ms  (8,052,956,206,592,517)
     R/N 36:    185,251 ms  (8,650,327,689,541,457)
     R/N 37:    187,467 ms  (8,650,349,867,341,457)
     R/N 38:    189,163 ms  (6,157,577,986,646,405)
     R/N 39:    217,112 ms  (4,135,786,945,764,210)
     R/N 40:    230,719 ms  (6,889,765,708,183,410)
  16 digits:    231,583 ms
     R/N 41:    236,505 ms  (86,965,750,494,756,968)
     R/N 42:    237,391 ms  (22,542,040,692,914,522)
     R/N 43:    351,728 ms  (67,725,910,561,765,640)
  17 digits:    360,678 ms
     R/N 44:    392,403 ms  (284,684,666,566,486,482)
     R/N 45:    513,738 ms  (225,342,456,863,243,522)
     R/N 46:    558,603 ms  (225,342,458,663,243,522)
     R/N 47:    653,047 ms  (225,342,478,643,243,522)
     R/N 48:    718,569 ms  (284,684,868,364,486,482)
     R/N 49:  1,087,602 ms  (871,975,098,681,469,178)
     R/N 50:  1,763,809 ms  (865,721,270,017,296,468)
     R/N 51:  1,779,059 ms  (297,128,548,234,950,692)
     R/N 52:  1,787,466 ms  (297,128,722,852,950,692)
     R/N 53:  1,888,803 ms  (811,865,096,390,477,018)
     R/N 54:  1,940,347 ms  (297,148,324,656,930,692)
     R/N 55:  1,965,331 ms  (297,148,546,434,930,692)
     R/N 56:  2,273,287 ms  (898,907,259,301,737,498)
     R/N 57:  2,657,073 ms  (631,688,638,047,992,345)
     R/N 58:  2,682,636 ms  (619,431,353,040,136,925)
     R/N 59:  2,948,725 ms  (619,631,153,042,134,925)
     R/N 60:  3,011,962 ms  (633,288,858,025,996,145)
     R/N 61:  3,077,937 ms  (633,488,632,647,994,145)
     R/N 62:  3,928,545 ms  (653,488,856,225,994,125)
     R/N 63:  4,195,016 ms  (497,168,548,234,910,690)
  18 digits:  4,445,897 ms

The rare numbers with up to 18 digits are:
   1:                       65
   2:                  621,770
   3:              281,089,082
   4:            2,022,652,202
   5:            2,042,832,002
   6:          868,591,084,757
   7:          872,546,974,178
   8:          872,568,754,178
   9:        6,979,302,951,885
  10:       20,313,693,904,202
  11:       20,313,839,704,202
  12:       20,331,657,922,202
  13:       20,331,875,722,202
  14:       20,333,875,702,202
  15:       40,313,893,704,200
  16:       40,351,893,720,200
  17:      200,142,385,731,002
  18:      204,238,494,066,002
  19:      221,462,345,754,122
  20:      244,062,891,224,042
  21:      245,518,996,076,442
  22:      248,359,494,187,442
  23:      403,058,392,434,500
  24:      441,054,594,034,340
  25:      816,984,566,129,618
  26:    2,078,311,262,161,202
  27:    2,133,786,945,766,212
  28:    2,135,568,943,984,212
  29:    2,135,764,587,964,212
  30:    2,135,786,765,764,212
  31:    4,135,786,945,764,210
  32:    6,157,577,986,646,405
  33:    6,889,765,708,183,410
  34:    8,052,956,026,592,517
  35:    8,052,956,206,592,517
  36:    8,191,154,686,620,818
  37:    8,191,156,864,620,818
  38:    8,191,376,864,400,818
  39:    8,650,327,689,541,457
  40:    8,650,349,867,341,457
  41:   22,542,040,692,914,522
  42:   67,725,910,561,765,640
  43:   86,965,750,494,756,968
  44:  225,342,456,863,243,522
  45:  225,342,458,663,243,522
  46:  225,342,478,643,243,522
  47:  284,684,666,566,486,482
  48:  284,684,868,364,486,482
  49:  297,128,548,234,950,692
  50:  297,128,722,852,950,692
  51:  297,148,324,656,930,692
  52:  297,148,546,434,930,692
  53:  497,168,548,234,910,690
  54:  619,431,353,040,136,925
  55:  619,631,153,042,134,925
  56:  631,688,638,047,992,345
  57:  633,288,858,025,996,145
  58:  633,488,632,647,994,145
  59:  653,488,856,225,994,125
  60:  811,865,096,390,477,018
  61:  865,721,270,017,296,468
  62:  871,975,098,681,469,178
  63:  898,907,259,301,737,498

```



## Julia

Pretty slow to get 8 rare numbers, even if the squares are checked via table.

```julia
fixeddigits = Dict(2 => [[0, 0, 2], [8, 8, 2]], 4 => [[0, 0, 0]],
    6 => [[2, 7, 0], [9, 8, 5]], 8 => [[6, 5, 7],[7, 7, 8]])
squares = Dict([i * i => 1 for i in 1:1500000])

i2dig(i) = (d = Int[]; while i > 0 i, r = divrem(i, 10); push!(d, r) end; d)
dig2i(d) = (n = 0; for i in d n = 10 * n + i end; n)

function simplegetrare(upto)
    ret = Int[]
    for n in 0:upto
        dig = i2dig(n)
        r = dig2i(dig)
        nrsum, nrdiff = n + r, n - r
        if nrdiff > 0 && haskey(squares, nrsum) && haskey(squares, nrdiff)
            push!(ret, n)
        end
    end
    ret
end

function getrare(N)
    ret = simplegetrare(20000)
    for i in 0:typemax(Int)
        basedigits = i2dig(i)
        for a in [2,4,6,8], (b, p, q) in fixeddigits[a]
            dig = [[q, p]; basedigits; [b, a]]
            r = dig2i(dig)
            n = dig2i(reverse(dig))
            nrsum, nrdiff = n + r, n - r
            if nrdiff > 0 && haskey(squares, nrsum) && haskey(squares, nrdiff)
                push!(ret, n)
                if length(ret) >= N
                    return ret
                end
            end
        end
    end
end

getrare(3)
@time println("The first 8 rare numbers are: ", sort(getrare(8)))

```
{{out}}

```txt

The first 8 rare numbers are: [65, 621770, 281089082, 2022652202, 2042832002, 868591084757, 872546974178, 872568754178]
1379.707737 seconds (9.36 G allocations: 545.177 GiB, 2.25% gc time)

```



## REXX

(See the ''discussion'' page for a simplistic 1<sup>st</sup> version that computes   ''rare''   numbers only using the task's basic rules).  

Most of the hints (properties of ''rare'' numbers) within Shyam Sunder Gupta's   [http://www.shyamsundergupta.com/rare.htm <u>webpage</u>]   have been incorporated in this 

REXX program and the logic is now expressed within the list of   '''AB...PQ'''   (abutted numbers within the   '''@g'''   list).

These improvements made this REXX version around   '''25%'''   faster than the previous version   (see the discussion page). 

```rexx
/*REXX program  calculates and displays  a  specified amount of   rare    numbers.      */
numeric digits 20;    w= digits() + digits() % 3 /*use enough dec. digs for calculations*/
parse arg many .                                 /*obtain optional argument from the CL.*/
if  many=='' |  many==","  then  many= 5         /*Not specified?  Then use the default.*/
@g= 2002 2112 2222 2332 2442 2552 2662 2772 2882 2992 4000 4010 4030 4050 4070 4090 4100 ,
    4110 4120 4140 4160 4180 4210 4230 4250 4270 4290 4300 4320 4340 4360 4380 4410 4430 ,
    4440 4450 4470 4490 4500 4520 4540 4560 4580 4610 4630 4650 4670 4690 4700 4720 4740 ,
    4760 4780 4810 4830 4850 4870 4890 4900 4920 4940 4960 4980 4990 6010 6015 6030 6035 ,
    6050 6055 6070 6075 6090 6095 6100 6105 6120 6125 6140 6145 6160 6165 6180 6185 6210 ,
    6215 6230 6235 6250 6255 6270 6275 6290 6295 6300 6305 6320 6325 6340 6345 6360 6365 ,
    6380 6385 6410 6415 6430 6435 6450 6455 6470 6475 6490 6495 6500 6505 6520 6525 6540 ,
    6545 6560 6565 6580 6585 6610 6615 6630 6635 6650 6655 6670 6675 6690 6695 6700 6705 ,
    6720 6725 6740 6745 6760 6765 6780 6785 6810 6815 6830 6835 6850 6855 6870 6875 6890 ,
    6895 6900 6905 6920 6925 6940 6945 6960 6965 6980 6985 8007 8008 8017 8027 8037 8047 ,
    8057 8067 8077 8087 8092 8097 8107 8117 8118 8127 8137 8147 8157 8167 8177 8182 8187 ,
    8197 8228 8272 8297 8338 8362 8387 8448 8452 8477 8542 8558 8567 8632 8657 8668 8722 ,
    8747 8778 8812 8837 8888 8902 8927 8998      /*4 digit abutted numbers for AB and PQ*/
@g#= words(@g)
         /* [↓]─────────────────boolean arrays are used for checking for digit presence.*/
@dr.=0;   @dr.2= 1; @dr.5=1 ; @dr.8= 1; @dr.9= 1 /*rare # must have these digital roots.*/
@ps.=0;   @ps.2= 1; @ps.3= 1; @ps.7= 1; @ps.8= 1 /*perfect squares    must end in these.*/
@149.=0;  @149.1=1; @149.4=1; @149.9=1           /*values for  Z  that need an even  Y. */
@odd.=0;  do i=-9  by 2  to 9;   @odd.i=1        /*   "    "   N    "    "   "   "   A. */
          end   /*i*/
@gen.=0;  do i=1  for words(@g); parse value word(@g,i) with a 2 b 3 p 4 q; @gen.a.b.p.q=1
               /*# AB···PQ  could be a good rare value*/
          end   /*i*/
div9= 9                                          /*dif must be ÷ 9 when N has even #digs*/
evenN= \ (10 // 2)                               /*initial value for evenness of  N.    */
#= 0                                             /*the number of  rare  numbers (so far)*/
    do n=10                                      /*Why 10?  All 1 dig #s are palindromic*/
    parse var   n   a  2  b  3  ''  -2  p  +1  q /*get 1st\2nd\penultimate\last digits. */
    if @odd.a  then do;  n=n+10**(length(n)-1)-1 /*bump N so next N starts with even dig*/
                         evenN=\(length(n+1)//2) /*flag when N has an even # of digits. */
                         if evenN  then div9=  9 /*when dif isn't divisible by   9  ... */
                                   else div9= 99 /*  "   "    "        "     "  99   "  */
                         iterate                 /*let REXX do its thing with  DO  loop.*/
                    end                          /* {it's allowed to modify a DO index} */
    if \@gen.a.b.p.q  then iterate               /*can  N  not be a rare AB···PQ number?*/
    r= reverse(n)                                /*obtain the reverse of the number  N. */
    if r>n   then iterate                        /*Difference will be negative?  Skip it*/
    if n==r  then iterate                        /*Palindromic?   Then it can't be rare.*/
    dif= n-r;   parse var  dif  ''  -2  y  +1  z /*obtain the last 2 digs of difference.*/
    if @ps.z  then iterate                       /*Not 0, 1, 4, 5, 6, 9? Not perfect sq.*/
       select
       when z==0   then if y\==0    then iterate /*Does Z = 0?   Then  Y  must be zero. */
       when z==5   then if y\==2    then iterate /*Does Z = 5?   Then  Y  must be two.  */
       when z==6   then if y//2==0  then iterate /*Does Z = 6?   Then  Y  must be odd.  */
       otherwise        if @149.z   then if y//2  then iterate /*Z=1,4,9? Y must be even*/
       end   /*select*/                          /* [↑]  the OTHERWISE handles Z=8 case.*/
    if dif//div9\==0  then iterate               /*Difference isn't ÷ by div9? Then skip*/
    sum= n+r;   parse var  sum  ''  -2  y  +1  z /*obtain the last two digits of the sum*/
    if @ps.z  then iterate                       /*Not 0, 2, 5, 8, or 9? Not perfect sq.*/
       select
       when z==0   then if y\==0    then iterate /*Does Z = 0?   Then  Y  must be zero. */
       when z==5   then if y\==2    then iterate /*Does Z = 5?   Then  Y  must be two.  */
       when z==6   then if y//2==0  then iterate /*Does Z = 6?   Then  Y  must be odd.  */
       otherwise        if @149.z   then if y//2  then iterate /*Z=1,4,9? Y must be even*/
       end   /*select*/                          /* [↑]  the OTHERWISE handles Z=8 case.*/
    if evenN  then if sum//11 \==0  then iterate /*N has even #digs? Sum must be ÷ by 11*/
    $= a + b                                     /*a head start on figuring digital root*/
                       do k=3  for length(n) - 2 /*now, process the rest of the digits. */
                       $= $ + substr(n, k, 1)    /*add the remainder of the digits in N.*/
                       end   /*k*/
       do while $>9                              /* [◄]  Algorithm is good for 111 digs.*/
       if $>9  then $= left($,1) + substr($,2,1) + substr($,3,1,0)     /*>9?  Reduce it.*/
       end   /*while*/
    if \@dr.$                 then iterate       /*Doesn't have good digital root?  Skip*/
    if iSqrt(sum)**2 \== sum  then iterate       /*Not a perfect square?  Then skip it. */
    if iSqrt(dif)**2 \== dif  then iterate       /* "  "    "       "       "    "   "  */
    #= # + 1;                 call tell          /*bump rare number counter;  display #.*/
    if #>=many  then leave                       /* [↑]  W:  the width of # with commas.*/
    end   /*n*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do jc=length(_)-3  to 1  by -3; _=insert(',', _, jc); end;  return _
tell:   say right(th(#),length(#)+9)  ' rare number is:'  right(commas(n),w);     return
th:     parse arg th;return th||word('th st nd rd',1+(th//10)*(th//100%10\==1)*(th//10<4))
/*──────────────────────────────────────────────────────────────────────────────────────*/
iSqrt:  parse arg x;    $= 0;  q= 1;                             do while q<=x; q=q*4; end
          do while q>1; q=q%4; _= x-$-q;  $= $%2;  if _>=0  then do;      x=_;  $=$+q; end
          end   /*while q>1*/;                     return $
```

{{out|output|text=  when using the input of:     <tt> 8 </tt>}}

```txt

       1st  rare number is:                           65
       2nd  rare number is:                      621,770
       3rd  rare number is:                  281,089,082
       4th  rare number is:                2,022,652,202
       5th  rare number is:                2,042,832,002
       6th  rare number is:              868,591,084,757
       7th  rare number is:              872,546,974,178
       8th  rare number is:              872,568,754,178

```



## Visual Basic .NET

{{trans|C#}} via {{trans|Go}} Surprisingly slow, I expected performance to be a little slower than C#, but this is quite a bit slower.  This vb.net version takes 1 2/3 minutes to do what the C# version can do in 2/3 of a minute.

```vbnet
Imports System.Console
Imports DT = System.DateTime
Imports Lsb = System.Collections.Generic.List(Of SByte)
Imports Lst = System.Collections.Generic.List(Of System.Collections.Generic.List(Of SByte))
Imports UI = System.UInt64

Module Module1
    Const MxD As SByte = 15

    Public Structure term
        Public coeff As UI : Public a, b As SByte
        Public Sub New(ByVal c As UI, ByVal a_ As Integer, ByVal b_ As Integer)
            coeff = c : a = CSByte(a_) : b = CSByte(b_)
        End Sub
    End Structure

    Dim nd, nd2, count As Integer, digs, cnd, di As Integer()
    Dim res As List(Of UI), st As DT, tLst As List(Of List(Of term))
    Dim lists As List(Of Lst), fml, dmd As Dictionary(Of Integer, Lst)
    Dim dl, zl, el, ol, il As Lsb, odd As Boolean, ixs, dis As Lst, Dif As UI

    ' converts digs array to the "difference"
    Function ToDif() As UI
        Dim r As UI = 0 : For i As Integer = 0 To digs.Length - 1 : r = r * 10 + digs(i)
        Next : Return r
    End Function

    ' converts digs array to the "sum"
    Function ToSum() As UI
        Dim r As UI = 0 : For i As Integer = digs.Length - 1 To 0 Step -1 : r = r * 10 + digs(i)
        Next : Return Dif + (r << 1)
    End Function

    '  determines if the nmbr is square or not
    Function IsSquare(nmbr As UI) As Boolean
        If (&H202021202030213 And (1UL << (nmbr And 63))) <> 0 Then _
            Dim r As UI = Math.Sqrt(nmbr) : Return r * r = nmbr Else Return False
    End Function

    '// returns sequence of SBbytes
    Function Seq(from As SByte, upto As Integer, Optional stp As SByte = 1) As Lsb
        Dim res As Lsb = New Lsb()
        For item As SByte = from To upto Step stp : res.Add(item) : Next : Return res
    End Function

    ' Recursive closure to generate (n+r) candidates from (n-r) candidates
    Sub Fnpr(ByVal lev As Integer)
        If lev = dis.Count Then
            digs(ixs(0)(0)) = fml(cnd(0))(di(0))(0) : digs(ixs(0)(1)) = fml(cnd(0))(di(0))(1)
            Dim le As Integer = di.Length, i As Integer = 1
            If odd Then le -= 1 : digs(nd >> 1) = di(le)
            For Each d As SByte In di.Skip(1).Take(le - 1)
                digs(ixs(i)(0)) = dmd(cnd(i))(d)(0)
                digs(ixs(i)(1)) = dmd(cnd(i))(d)(1) : i += 1 : Next
            If Not IsSquare(ToSum()) Then Return
            res.Add(ToDif()) : count += 1
            WriteLine("{0,16:n0}{1,4}   ({2:n0})", (DT.Now - st).TotalMilliseconds, count, res.Last())
        Else
            For Each n In dis(lev) : di(lev) = n : Fnpr(lev + 1) : Next
        End If
    End Sub

    ' Recursive closure to generate (n-r) candidates with a given number of digits.
    Sub Fnmr(ByVal list As Lst, ByVal lev As Integer)
        If lev = list.Count Then
            Dif = 0 : Dim i As SByte = 0 : For Each t In tLst(nd2)
                If cnd(i) < 0 Then Dif -= t.coeff * CULng(-cnd(i)) _
                              Else Dif += t.coeff * CULng(cnd(i))
                i += 1 : Next
            If Dif <= 0 OrElse Not IsSquare(Dif) Then Return
            dis = New Lst From {Seq(0, fml(cnd(0)).Count - 1)}
            For Each i In cnd.Skip(1) : dis.Add(Seq(0, dmd(i).Count - 1)) : Next
            If odd Then dis.Add(il)
            di = New Integer(dis.Count - 1) {} : Fnpr(0)
        Else
            For Each n As SByte In list(lev) : cnd(lev) = n : Fnmr(list, lev + 1) : Next
        End If
    End Sub

    Sub init()
        Dim pow As UI = 1
        ' terms of (n-r) expression for number of digits from 2 to maxDigits
        tLst = New List(Of List(Of term))() : For Each r As Integer In Seq(2, MxD)
            Dim terms As List(Of term) = New List(Of term)()
            pow *= 10 : Dim p1 As UI = pow, p2 As UI = 1
            Dim i1 As Integer = 0, i2 As Integer = r - 1
            While i1 < i2 : terms.Add(New term(p1 - p2, i1, i2))
                p1 = p1 / 10 : p2 = p2 * 10 : i1 += 1 : i2 -= 1 : End While
            tLst.Add(terms) : Next
        ' map of first minus last digits for 'n' to pairs giving this value
        fml = New Dictionary(Of Integer, Lst)() From {
            {0, New Lst() From {New Lsb() From {2, 2}, New Lsb() From {8, 8}}},
            {1, New Lst() From {New Lsb() From {6, 5}, New Lsb() From {8, 7}}},
            {4, New Lst() From {New Lsb() From {4, 0}}},
            {6, New Lst() From {New Lsb() From {6, 0}, New Lsb() From {8, 2}}}}
        ' map of other digit differences for 'n' to pairs giving this value
        dmd = New Dictionary(Of Integer, Lst)()
        For i As SByte = 0 To 10 - 1 : Dim j As SByte = 0, d As SByte = i
            While j < 10 : If dmd.ContainsKey(d) Then dmd(d).Add(New Lsb From {i, j}) _
                Else dmd(d) = New Lst From {New Lsb From {i, j}}
                j += 1 : d -= 1 : End While : Next
        dl = Seq(-9, 9)    ' all  differences
        zl = Seq(0, 0)     ' zero difference
        el = Seq(-8, 8, 2) ' even differences
        ol = Seq(-9, 9, 2) ' odd  differences
        il = Seq(0, 9)
        lists = New List(Of Lst)()
        For Each f As SByte In fml.Keys : lists.Add(New Lst From {New Lsb From {f}}) : Next
    End Sub

    Sub Main(ByVal args As String())
        init() : res = New List(Of UI)() : st = DT.Now : count = 0
        WriteLine("{0,5}{1,12}{2,4}{3,14}", "digs", "elapsed(ms)", "R/N", "Rare Numbers")
        nd = 2 : nd2 = 0 : odd = False : While nd <= MxD
            digs = New Integer(nd - 1) {} : If nd = 4 Then
                lists(0).Add(zl) : lists(1).Add(ol) : lists(2).Add(el) : lists(3).Add(ol)
            ElseIf tLst(nd2).Count > lists(0).Count Then
                For Each list As Lst In lists : list.Add(dl) : Next : End If
            ixs = New Lst() : For Each t As term In tLst(nd2) : ixs.Add(New Lsb From {t.a, t.b}) : Next
            For Each list As Lst In lists : cnd = New Integer(list.Count - 1) {} : Fnmr(list, 0) : Next
            WriteLine("  {0,2}  {1,10:n0}", nd, (DT.Now - st).TotalMilliseconds)
            nd += 1 : nd2 += 1 : odd = Not odd : End While
        res.Sort() : WriteLine(vbLf & "The {0} rare numbers with up to {1} digits are:", res.Count, MxD)
        count = 0 : For Each rare In res : count += 1 : WriteLine("{0,2}:{1,27:n0}", count, rare) : Next
        If System.Diagnostics.Debugger.IsAttached Then ReadKey()
    End Sub
End Module
```

{{out}}
<pre style="height:64ex;overflow:scroll"> digs elapsed(ms) R/N  Rare Numbers
              25   1   (65)
   2          26
   3          26
   4          27
   5          27
              28   2   (621,770)
   6          29
   7          30
   8          41
              42   3   (281,089,082)
   9          46
              47   4   (2,022,652,202)
             116   5   (2,042,832,002)
  10         273
  11         422
           1,363   6   (872,546,974,178)
           1,476   7   (872,568,754,178)
           2,937   8   (868,591,084,757)
  12       3,584
           4,560   9   (6,979,302,951,885)
  13       5,817
          18,234  10   (20,313,693,904,202)
          18,471  11   (20,313,839,704,202)
          23,626  12   (20,331,657,922,202)
          24,454  13   (20,331,875,722,202)
          26,599  14   (20,333,875,702,202)
          60,784  15   (40,313,893,704,200)
          61,246  16   (40,351,893,720,200)
  14      65,387
          65,465  17   (200,142,385,731,002)
          66,225  18   (221,462,345,754,122)
          76,417  19   (816,984,566,129,618)
          81,727  20   (245,518,996,076,442)
          82,461  21   (204,238,494,066,002)
          82,694  22   (248,359,494,187,442)
          83,729  23   (244,062,891,224,042)
          99,241  24   (403,058,392,434,500)
         100,009  25   (441,054,594,034,340)
  15     104,207

The 25 rare numbers with up to 15 digits are:
 1:                         65
 2:                    621,770
 3:                281,089,082
 4:              2,022,652,202
 5:              2,042,832,002
 6:            868,591,084,757
 7:            872,546,974,178
 8:            872,568,754,178
 9:          6,979,302,951,885
10:         20,313,693,904,202
11:         20,313,839,704,202
12:         20,331,657,922,202
13:         20,331,875,722,202
14:         20,333,875,702,202
15:         40,313,893,704,200
16:         40,351,893,720,200
17:        200,142,385,731,002
18:        204,238,494,066,002
19:        221,462,345,754,122
20:        244,062,891,224,042
21:        245,518,996,076,442
22:        248,359,494,187,442
23:        403,058,392,434,500
24:        441,054,594,034,340
25:        816,984,566,129,618
```

