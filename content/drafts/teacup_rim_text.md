+++
title = "Teacup rim text"
description = ""
date = 2019-10-15T03:44:39Z
aliases = []
[extra]
id = 22446
[taxonomies]
categories = []
tags = []
+++

{{draft task|Teacup rim text}}

On a set of coasters we have, there's a picture of a teacup. On the rim of the teacup the word "TEA" appears a number of times separated by bullet characters. It occurred to me that if the bullet were removed and the words run together, you could start at any letter and still end up with a meaningful three-letter word. So start at the "T" and read "TEA". Start at the "E" and read "EAT", or start at the "A" and read "ATE".

That got me thinking that maybe there are other words that could be used rather that "TEA". And that's just English. What about Italian or Greek or ... um ... Telugu. For English, we use the unixdict (now) located at http://wiki.puzzlers.org/pub/wordlists/unixdict.txt . (This maintains continuity with other RC tasks that also use it.)

So here's the task: You're in search of a set of words that could be printed around the edge of a teacup. The words in each set are to be of the same length, that length being greater than two (thus precluding AH and HA, for example.) Having listed a set, for example [ate tea eat], refrain from displaying permutations of that set, e.g. [eat tea ate] etc. The words should also be made of more than one letter (thus precluding III and OOO etc.) 

The relationship between these words is (using ATE as an example) that the first letter of the first becomes the last letter of the second. The first letter of the second becomes the last letter of the third. So ATE becomes TEA and TEA becomes EAT. All of the possible permutations, using this particular permutation technique, must be words in the list. The set you generate for ATE will never included the word ETA as that cannot be reached via the first-to-last movement method. 

Display one line for each set of teacup rim words. 


## AWK


```AWK

# syntax: GAWK -f TEACUP_RIM_TEXT.AWK UNIXDICT.TXT
#
# sorting:
#   PROCINFO["sorted_in"] is used by GAWK
#   SORTTYPE is used by Thompson Automation's TAWK
#
{   for (i=1; i<=NF; i++) {
      arr[tolower($i)] = 0
    }
}
END {
    PROCINFO["sorted_in"] = "@ind_str_asc" ; SORTTYPE = 1
    for (i in arr) {
      leng = length(i)
      if (leng > 2) {
        delete tmp_arr
        words = str = i
        tmp_arr[i] = ""
        for (j=2; j<=leng; j++) {
          str = substr(str,2) substr(str,1,1)
          if (str in arr) {
            words = words " " str
            tmp_arr[str] = ""
          }
        }
        if (length(tmp_arr) == leng) {
          count = 0
          for (j in tmp_arr) {
            (arr[j] == 0) ? arr[j]++ : count++
          }
          if (count == 0) {
            printf("%s\n",words)
            circular++
          }
        }
      }
    }
    printf("%d words, %d circular\n",length(arr),circular)
    exit(0)
}

```

{{out}}
<p>using UNIXDICT.TXT</p>

```txt

apt pta tap
arc rca car
ate tea eat
25104 words, 3 circular

```

<p>using MIT10000.TXT</p>

```txt

aim ima mai
arc rca car
asp spa pas
ate tea eat
ips psi sip
10000 words, 5 circular

```

=={{header|F_Sharp|F#}}==

```fsharp

// Teacup rim text. Nigel Galloway: August 7th., 2019
let  N=System.IO.File.ReadAllLines("dict.txt")|>Array.filter(fun n->String.length n=3 && Seq.length(Seq.distinct n)>1)|>Set.ofArray
let fG z=Set.map(fun n->System.String(Array.ofSeq (Seq.permute(fun g->(g+z)%3)n))) N
Set.intersectMany [N;fG 1;fG 2]|>Seq.distinctBy(Seq.sort>>Array.ofSeq>>System.String)|>Seq.iter(printfn "%s")

```

{{out}}

```txt

aim
arc
asp
ate
ips

```



## Factor


```factor
USING: combinators.short-circuit fry grouping hash-sets
http.client kernel math prettyprint sequences sequences.extras
sets sorting splitting ;

"https://www.mit.edu/~ecprice/wordlist.10000" http-get nip
"\n" split [ { [ length 3 < ] [ all-equal? ] } 1|| ] reject
[ [ all-rotations ] map ] [ >hash-set ] bi
'[ [ _ in? ] all? ] filter [ natural-sort ] map members .
```

{{out}}

```txt

{
    { "aim" "ima" "mai" }
    { "arc" "car" "rca" }
    { "asp" "pas" "spa" }
    { "ate" "eat" "tea" }
    { "ips" "psi" "sip" }
}

```



## Go


```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "sort"
    "strings"
)

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func readWords(fileName string) []string {
    file, err := os.Open(fileName)
    check(err)
    defer file.Close()
    var words []string
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        word := strings.ToLower(strings.TrimSpace(scanner.Text()))
        if len(word) >= 3 {
            words = append(words, word)
        }
    }
    check(scanner.Err())
    return words
}

func rotate(runes []rune) {
    first := runes[0]
    copy(runes, runes[1:])
    runes[len(runes)-1] = first
}

func main() {
    dicts := []string{"mit_10000.txt", "unixdict.txt"} // local copies
    for _, dict := range dicts {
        fmt.Printf("Using %s:\n\n", dict)
        words := readWords(dict)
        n := len(words)
        used := make(map[string]bool)
    outer:
        for _, word := range words {
            runes := []rune(word)
            variants := []string{word}
            for i := 0; i < len(runes)-1; i++ {
                rotate(runes)
                word2 := string(runes)
                if word == word2 || used[word2] {
                    continue outer
                }
                ix := sort.SearchStrings(words, word2)
                if ix == n || words[ix] != word2 {
                    continue outer
                }
                variants = append(variants, word2)
            }
            for _, variant := range variants {
                used[variant] = true
            }
            fmt.Println(variants)
        }
        fmt.Println()
    }
}
```


{{out}}

```txt

Using mit_10000.txt:

[aim ima mai]
[arc rca car]
[asp spa pas]
[ate tea eat]
[ips psi sip]

Using unixdict.txt:

[apt pta tap]
[arc rca car]
[ate tea eat]

```



## Haskell


### Using Data.Set

Circular words of more than 2 characters in a local copy of a word list.

```haskell
import Data.List (groupBy, intercalate, sort, sortBy)
import qualified Data.Set as S
import Data.Ord (comparing)
import Data.Function (on)

main :: IO ()
main =
  readFile "mitWords.txt" >>= (putStrLn . showGroups . circularWords . lines)

circularWords :: [String] -> [String]
circularWords ws =
  let lexicon = S.fromList ws
  in filter (isCircular lexicon) ws

isCircular :: S.Set String -> String -> Bool
isCircular lex w = 2 < length w && all (`S.member` lex) (rotations w)

rotations :: [a] -> [[a]]
rotations = fmap <$> rotated <*> (enumFromTo 0 . pred . length)

rotated :: [a] -> Int -> [a]
rotated [] _ = []
rotated xs n = zipWith const (drop n (cycle xs)) xs

showGroups :: [String] -> String
showGroups xs =
  unlines $
  intercalate " -> " . fmap snd <$>
  filter
    ((1 <) . length)
    (groupBy (on (==) fst) (sortBy (comparing fst) (((,) =<< sort) <$> xs)))
```

{{Out}}

```txt
arc -> car -> rca
ate -> eat -> tea
aim -> ima -> mai
asp -> pas -> spa
ips -> psi -> sip
```



### Filtering anagrams


Or taking a different approach, we can avoid the use of Data.Set by obtaining the groups of anagrams (of more than two characters) in the lexicon, and filtering out a circular subset of these:

```haskell
import Data.List (groupBy, intercalate, sort, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Bool (bool)

main :: IO ()
main =
  readFile "mitWords.txt" >>=
  (putStrLn .
   unlines . fmap (intercalate " -> ") . (circularOnly =<<) . anagrams . lines)

anagrams :: [String] -> [[String]]
anagrams ws =
  groupBy (on (==) fst) (sortBy (comparing fst) (((,) =<< sort) <$> ws)) >>=
  (bool [] . return . fmap snd) <*> ((> 2) . length)

circularOnly :: [String] -> [[String]]
circularOnly ws =
  let h = head ws
      lng = length h - 1
      rs = filter (isRotation h) (tail ws)
  in bool [h : rs] [] (lng > length rs)

isRotation :: String -> String -> Bool
isRotation xs ys = xs /= until ((||) . (ys ==) <*> (xs ==)) rotated (rotated xs)

rotated :: [a] -> [a]
rotated [] = []
rotated xs = tail xs ++ [head xs]
```

{{Out}}

```txt
arc -> rca -> car
ate -> tea -> eat
aim -> ima -> mai
asp -> spa -> pas
ips -> psi -> sip
```



## JavaScript

===Set() objects===
Reading a local dictionary with the macOS JS for Automation library:
{{Works with|JXA}}

```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () =>
        showGroups(
            circularWords(
                // Local copy of:
                // https://www.mit.edu/~ecprice/wordlist.10000
                lines(readFile('~/mitWords.txt'))
            )
        );

    // circularWords :: [String] -> [String]
    const circularWords = ws =>
        ws.filter(isCircular(new Set(ws)), ws);

    // isCircular :: Set String -> String -> Bool
    const isCircular = lexicon => w => {
        const iLast = w.length - 1;
        return 1 < iLast && until(
            ([i, bln, s]) => iLast < i || !bln,
            ([i, bln, s]) => [1 + i, lexicon.has(s), rotated(s)],
            [0, true, rotated(w)]
        )[1];
    };

    // DISPLAY --------------------------------------------

    // showGroups :: [String] -> String
    const showGroups = xs =>
        unlines(map(
            gp => map(snd, gp).join(' -> '),
            groupBy(
                (a, b) => fst(a) === fst(b),
                sortBy(
                    comparing(fst),
                    map(x => Tuple(concat(sort(chars(x))), x),
                        xs
                    )
                )
            ).filter(gp => 1 < gp.length)
        ));


    // MAC OS JS FOR AUTOMATION ---------------------------

    // readFile :: FilePath -> IO String
    const readFile = fp => {
        const
            e = $(),
            uw = ObjC.unwrap,
            s = uw(
                $.NSString.stringWithContentsOfFileEncodingError(
                    $(fp)
                    .stringByStandardizingPath,
                    $.NSUTF8StringEncoding,
                    e
                )
            );
        return undefined !== s ? (
            s
        ) : uw(e.localizedDescription);
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // chars :: String -> [Char]
    const chars = s => s.split('');

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    const groupBy = (f, xs) => {
        const tpl = xs.slice(1)
            .reduce((a, x) => {
                const h = a[1].length > 0 ? a[1][0] : undefined;
                return (undefined !== h) && f(h, x) ? (
                    Tuple(a[0], a[1].concat([x]))
                ) : Tuple(a[0].concat([a[1]]), [x]);
            }, Tuple([], 0 < xs.length ? [xs[0]] : []));
        return tpl[0].concat([tpl[1]]);
    };

    // lines :: String -> [String]
    const lines = s => s.split(/[\r\n]/);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // rotated :: String -> String
    const rotated = xs =>
        xs.slice(1) + xs[0];

    // showLog :: a -> IO ()
    const showLog = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // sort :: Ord a => [a] -> [a]
    const sort = xs => xs.slice()
        .sort((a, b) => a < b ? -1 : (a > b ? 1 : 0));

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) =>
        xs.slice()
        .sort(f);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
arc -> car -> rca
ate -> eat -> tea
aim -> ima -> mai
asp -> pas -> spa
ips -> psi -> sip
```



### Anagram filtering

Reading a local dictionary with the macOS JS for Automation library:
{{Works with|JXA}}

```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () =>
        anagrams(lines(readFile('~/mitWords.txt')))
        .flatMap(circularOnly)
        .map(xs => xs.join(' -> '))
        .join('\n')

    // anagrams :: [String] -> [[String]]
    const anagrams = ws =>
        groupBy(
            on(eq, fst),
            sortBy(
                comparing(fst),
                ws.map(w => Tuple(sort(chars(w)).join(''), w))
            )
        ).flatMap(
            gp => 2 < gp.length ? [
                gp.map(snd)
            ] : []
        )

    // circularOnly :: [String] -> [[String]]
    const circularOnly = ws => {
        const h = ws[0];
        return ws.length < h.length ? (
            []
        ) : (() => {
            const rs = rotations(h);
            return rs.every(r => ws.includes(r)) ? (
                [rs]
            ) : [];
        })();
    };

    // rotations :: String -> [String]
    const rotations = s =>
        takeIterate(s.length, rotated, s)

    // rotated :: [a] -> [a]
    const rotated = xs => xs.slice(1).concat(xs[0]);


    // GENERIC FUNCTIONS ----------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // chars :: String -> [Char]
    const chars = s => s.split('');

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // eq (==) :: Eq a => a -> a -> Bool
    const eq = (a, b) => a === b

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    const groupBy = (f, xs) => {
        const tpl = xs.slice(1)
            .reduce((a, x) => {
                const h = a[1].length > 0 ? a[1][0] : undefined;
                return (undefined !== h) && f(h, x) ? (
                    Tuple(a[0], a[1].concat([x]))
                ) : Tuple(a[0].concat([a[1]]), [x]);
            }, Tuple([], 0 < xs.length ? [xs[0]] : []));
        return tpl[0].concat([tpl[1]]);
    };

    // lines :: String -> [String]
    const lines = s => s.split(/[\r\n]/);

    // mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    const mapAccumL = (f, acc, xs) =>
        xs.reduce((a, x, i) => {
            const pair = f(a[0], x, i);
            return Tuple(pair[0], a[1].concat(pair[1]));
        }, Tuple(acc, []));

    // on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    const on = (f, g) => (a, b) => f(g(a), g(b));

    // readFile :: FilePath -> IO String
    const readFile = fp => {
        const
            e = $(),
            uw = ObjC.unwrap,
            s = uw(
                $.NSString.stringWithContentsOfFileEncodingError(
                    $(fp)
                    .stringByStandardizingPath,
                    $.NSUTF8StringEncoding,
                    e
                )
            );
        return undefined !== s ? (
            s
        ) : uw(e.localizedDescription);
    };

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // sort :: Ord a => [a] -> [a]
    const sort = xs => xs.slice()
        .sort((a, b) => a < b ? -1 : (a > b ? 1 : 0));

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) =>
        xs.slice()
        .sort(f);

    // takeIterate :: Int -> (a -> a) -> a -> [a]
    const takeIterate = (n, f, x) =>
        snd(mapAccumL((a, _, i) => {
            const v = 0 !== i ? f(a) : x;
            return [v, v];
        }, x, Array.from({
            length: n
        })));

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
arc -> rca -> car
ate -> tea -> eat
aim -> ima -> mai
asp -> spa -> pas
ips -> psi -> sip
```



## Julia

Using the MIT 10000 word list, and excluding words of less than three letters, to reduce output length.

```julia
using HTTP
 
rotate(s, n) = String(circshift(Vector{UInt8}(s), n))
 
isliketea(w, d) = (n = length(w); n > 2 && any(c -> c != w[1], w) && 
    all(i -> haskey(d, rotate(w, i)), 1:n-1))
 
function getteawords(listuri)
    req = HTTP.request("GET", listuri)
    wdict = Dict{String, Int}((lowercase(string(x)), 1) for x in split(String(req.body), r"\s+"))
    sort(unique([sort([rotate(word, i) for i in 1:length(word)]) 
        for word in collect(keys(wdict)) if isliketea(word, wdict)]))
end
 
foreach(println, getteawords("https://www.mit.edu/~ecprice/wordlist.10000"))

```
{{out}}

```txt

["aim", "ima", "mai"]
["arc", "car", "rca"]
["asp", "pas", "spa"]
["ate", "eat", "tea"]
["ips", "psi", "sip"]

```



## Lychen

Lychen is V8 JavaScript wrapped in C#, exposing C# into JavaScript.

Using https://www.mit.edu/~ecprice/wordlist.10000 as per the Julia example.


```javascript

const wc = new CS.System.Net.WebClient();
const lines = wc.DownloadString("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt");
const words = lines.split(/\n/g);
const collection = {};
words.filter(word => word.length > 2).forEach(word => {
  let allok = true;
  let newword = word;
  for (let i = 0; i < word.length - 1; i++) {
    newword = newword.substr(1) + newword.substr(0, 1);
    if (!words.includes(newword)) {
      allok = false;
      break;
    }
  }
  if (allok) {
    const key = word.split("").sort().join("");
    if (!collection[key]) {
      collection[key] = [word];
    } else {
      if (!collection[key].includes(word)) {
        collection[key].push(word);
      }
    }
  }
});
Object.keys(collection)
.filter(key => collection[key].length > 1)
.forEach(key => console.log("%s", collection[key].join(", ")));

```


```txt

apt, pta, tap
arc, car, rca
ate, eat, tea

```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature 'say';
use List::Util qw(uniqstr any);

my(%words,@teacups,%seen);

open my $fh, '<', 'ref/wordlist.10000';
while (<$fh>) {
    chomp(my $w = uc $_);
    next if length $w < 3;
    push @{$words{join '', sort split //, $w}}, $w;}

for my $these (values %words) {
    next if @$these < 3;
    MAYBE: for (@$these) {
        my $maybe = $_;
        next if $seen{$_};
        my @print;
        for my $i (0 .. length $maybe) {
            if (any { $maybe eq $_ } @$these) {
                push @print, $maybe;
                $maybe = substr($maybe,1) . substr($maybe,0,1)
            } else {
                @print = () and next MAYBE
            }
        }
        if (@print) {
            push @teacups, [@print];
            $seen{$_}++ for @print;
        }
    }
}

say join ', ', uniqstr @$_ for sort @teacups;
```

{{out}}

```txt
ARC, RCA, CAR
ATE, TEA, EAT
AIM, IMA, MAI
ASP, SPA, PAS
IPS, PSI, SIP
```



## Perl 6

{{works with|Rakudo|2019.07.1}}
There doesn't seem to be any restriction that the word needs to consist only of lowercase letters, so words of any case are included. Since the example code specifically shows the example words (TEA, EAT, ATE) in uppercase, I elected to uppercase the found words.

As the specs keep changing, this version will accept ANY text file as its dictionary and accepts parameters to configure the minimum number of characters in a word to consider and whether to allow mono-character words. 

Defaults to unixdict.txt, minimum 3 characters and mono-character 'words' disallowed. Feed a file name to use a different word list, an integer to --min-chars and/or a truthy value to --mono to allow mono-chars. 


```perl6
my %*SUB-MAIN-OPTS = :named-anywhere;

unit sub MAIN ( $dict = 'unixdict.txt', :$min-chars = 3, :$mono = False );

my %words;
$dict.IO.slurp.words.map: { .chars < $min-chars ?? (next) !! %words{.uc.comb.sort.join}.push: .uc };

my @teacups;
my %seen;

for %words.values -> @these {
    next if !$mono && @these < 2;
    MAYBE: for @these {
        my $maybe = $_;
        next if %seen{$_};
        my @print;
        for ^$maybe.chars {
            if $maybe ∈ @these {
                @print.push: $maybe;
                $maybe = $maybe.comb.list.rotate.join;
            } else {
                @print = ();
                next MAYBE
            }
        }
        if @print.elems {
            @teacups.push: @print;
            %seen{$_}++ for @print;
        }
    }
}

say .unique.join(", ") for sort @teacups;
```

{{out|Defaults}}
Command line: <tt>perl6 teacup.p6</tt>

```txt
APT, PTA, TAP
ARC, RCA, CAR
ATE, TEA, EAT
```

{{out|Allow mono-chars}}
Command line: <tt>perl6 teacup.p6 --mono=1</tt>

```txt
AAA
APT, PTA, TAP
ARC, RCA, CAR
ATE, TEA, EAT
III
```

{{out|Using a larger dictionary}}
words.txt file from https://github.com/dwyl/english-words

Command line: <tt>perl6 teacup.p6 words.txt --min-chars=4 --mono=Allow</tt>

```txt
AAAA
AAAAAA
ADAD, DADA
ADAR, DARA, ARAD, RADA
AGAG, GAGA
ALIT, LITA, ITAL, TALI
AMAN, MANA, ANAM, NAMA
AMAR, MARA, ARAM, RAMA
AMEL, MELA, ELAM, LAME
AMEN, MENA, ENAM, NAME
AMOR, MORA, ORAM, RAMO
ANAN, NANA
ANIL, NILA, ILAN, LANI
ARAR, RARA
ARAS, RASA, ASAR, SARA
ARIS, RISA, ISAR, SARI
ASEL, SELA, ELAS, LASE
ASER, SERA, ERAS, RASE
DENI, ENID, NIDE, IDEN
DOLI, OLID, LIDO, IDOL
EGOR, GORE, OREG, REGO
ENOL, NOLE, OLEN, LENO
ESOP, SOPE, OPES, PESO
ISIS, SISI
MMMM
MORO, OROM, ROMO, OMOR
OOOO
```



## Phix

Filters anagram lists

```Phix
procedure filter(sequence anagrams)
    sequence used = repeat(false,length(anagrams))
    for i=1 to length(anagrams) do
        if not used[i] then
            used[i] = true
            string word = anagrams[i]
            sequence res = {word}
            for r=2 to length(word) do
                word = word[2..$]&word[1]
                integer k = find(word,anagrams)
                if k=0 then res = {} exit end if
                if not find(word,res) then
                    res = append(res,word)
                end if
                used[k] = true
            end for
            if length(res) then ?res end if
        end if
    end for
end procedure

procedure teacup(string filename, integer minlen=3, bool allow_mono=false)
sequence words = {}, anagrams = {}, last="", letters
object word

    printf(1,"using %s",filename)
    integer fn = open(filename,"r")
    if fn=-1 then crash(filename&" not found") end if
    while 1 do
        word = lower(trim(gets(fn)))
        if atom(word) then exit end if
        if length(word)>=minlen then
            letters = sort(word)
            words = append(words, {letters, word})
        end if
    end while
    close(fn)
    printf(1,", %d words read\n",length(words))
    if length(words)!=0 then
        words = sort(words)
        for i=1 to length(words) do
            {letters,word} = words[i]
            if letters=last then
                anagrams = append(anagrams,word)
            else
                if allow_mono or length(anagrams)>=length(last) then
                    filter(anagrams) 
                end if
                last = letters
                anagrams = {word}
            end if
        end for
        if allow_mono or length(anagrams)>=length(last) then
            filter(anagrams) 
        end if
    end if
end procedure

teacup(join_path({"demo","unixdict.txt"}))
-- These match output from other entries:
--teacup(join_path({"demo","unixdict.txt"}),allow_mono:=true)
--teacup(join_path({"demo","rosetta","mit.wordlist.10000.txt"}))
--teacup(join_path({"demo","rosetta","words.txt"}),4,true)
-- Note that allow_mono is needed to display eg {"agag","gaga"}
```

{{out}}

```txt

using demo\unixdict.txt, 24948 words read
{"arc","rca","car"}
{"ate","tea","eat"}
{"apt","pta","tap"}

```



## Python


### Functional

Composing generic functions, and considering only anagram groups.

```python
'''Teacup rim text'''

from itertools import chain, groupby
from os.path import expanduser
from functools import reduce


# main :: IO ()
def main():
    '''Circular anagram groups, of more than one word,
       and containing words of length > 2, found in:
       https://www.mit.edu/~ecprice/wordlist.10000
    '''
    print('\n'.join(
        concatMap(circularGroup)(
            anagrams(3)(
                # Reading from a local copy.
                lines(readFile('~/mitWords.txt'))
            )
        )
    ))


# anagrams :: Int -> [String] -> [[String]]
def anagrams(n):
    '''Groups of anagrams, of minimum group size n,
       found in the given word list.
    '''
    def go(ws):
        return concatMap(
            lambda xs: [
                [snd(x) for x in xs]
            ] if n <= len(xs) >= len(xs[0][0]) else []
        )(
            groupBy(fst)(
                sorted(
                    [(''.join(sorted(w)), w) for w in ws],
                    key=fst
                )
            )
        )
    return lambda ws: go(ws)


# circularGroup :: [String] -> [String]
def circularGroup(ws):
    '''Either an empty list, or a list containing
       a string showing any circular subset found in ws.
    '''
    lex = set(ws)
    iLast = len(ws) - 1
    # If the set contains one word that is circular,
    # then it must contain all of them.
    (i, blnCircular) = until(
        lambda tpl: tpl[1] or (tpl[0] > iLast)
    )(
        lambda tpl: (1 + tpl[0], isCircular(lex)(ws[tpl[0]]))
    )(
        (0, False)
    )
    return [' -> '.join(allRotations(ws[i]))] if blnCircular else []


# isCircular :: Set String -> String -> Bool
def isCircular(lexicon):
    '''True if all of a word's rotations
       are found in the given lexicon.
    '''
    def go(w):
        def f(tpl):
            (i, _, x) = tpl
            return (1 + i, x in lexicon, rotated(x))

        iLast = len(w) - 1
        return until(
            lambda tpl: iLast < tpl[0] or (not tpl[1])
        )(f)(
            (0, True, rotated(w))
        )[1]

    return lambda s: go(s)


# allRotations :: String -> [String]
def allRotations(w):
    '''All rotations of the string w.'''
    return takeIterate(len(w) - 1)(
        rotated
    )(w)


# GENERIC -------------------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).
    '''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# groupBy :: (a -> b) -> [a] -> [[a]]
def groupBy(f):
    '''The elements of xs grouped,
       preserving order, by equality
       in terms of the key function f.
    '''
    return lambda xs: [
        list(x[1]) for x in groupby(xs, key=f)
    ]


# lines :: String -> [String]
def lines(s):
    '''A list of strings,
       (containing no newline characters)
       derived from a single new-line delimited string.
    '''
    return s.splitlines()


# mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
def mapAccumL(f):
    '''A tuple of an accumulation and a list derived by a
       combined map and fold,
       with accumulation from left to right.
    '''
    def go(a, x):
        tpl = f(a[0], x)
        return (tpl[0], a[1] + [tpl[1]])
    return lambda acc: lambda xs: (
        reduce(go, xs, (acc, []))
    )


# readFile :: FilePath -> IO String
def readFile(fp):
    '''The contents of any file at the path
       derived by expanding any ~ in fp.
    '''
    with open(expanduser(fp), 'r', encoding='utf-8') as f:
        return f.read()


# rotated :: String -> String
def rotated(s):
    '''A string rotated 1 character to the right.'''
    return s[1:] + s[0]


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# takeIterate :: Int -> (a -> a) -> a -> [a]
def takeIterate(n):
    '''Each value of n iterations of f
       over a start value of x.
    '''
    def go(f, x):
        def g(a, i):
            v = f(a) if i else x
            return (v, v)
        return mapAccumL(g)(x)(
            range(0, 1 + n)
        )[1]
    return lambda f: lambda x: go(f, x)


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


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
arc -> rca -> car
ate -> tea -> eat
aim -> ima -> mai
asp -> spa -> pas
ips -> psi -> sip
```



## REXX

All words that contained non─letter (Latin) characters   (periods, decimal digits, minus signs,
underbars, or embedded blanks)   weren't considered as candidates for circular words.

Duplicated words  (such as   '''sop'''   and   '''SOP''')   are ignored   (just the 2<sup>nd</sup> and subsequent duplicated words).

All words in the dictionary are treated as caseless.

The dictionary wasn't assumed to be sorted in any way.

```rexx
/*REXX pgm finds circular words (length>2),  using a dictionary,  suppress permutations.*/
parse arg iFID L .                               /*obtain optional arguments from the CL*/
if iFID==''|iFID==","  then iFID= 'wordlist.10k' /*Not specified?  Then use the default.*/
if    L==''|   L==","  then    L= 3              /* "      "         "   "   "     "    */
#= 0                                             /*number of words in dictionary, Len>L.*/
@.=                                              /*stemmed array of non─duplicated words*/
       do r=0  while lines(iFID) \== 0           /*read all lines (words) in dictionary.*/
       parse upper value  linein(iFID)  with z . /*obtain a word from the dictionary.   */
       if length(z)<L | @.z\==''  then iterate   /*length must be  L  or more,  no dups.*/
       if \datatype(z, 'U')       then iterate   /*Word contains non-letters?  Then skip*/
       @.z = z                                   /*assign a word from the dictionary.   */
       #= # + 1;     $.#= z                      /*bump word count; append word to list.*/
       end   /*r*/                               /* [↑]  dictionary need not be sorted. */
cw= 0                                            /*the number of circular words (so far)*/
say "There're "    r    ' entries in the dictionary (of all types):  '      iFID
say "There're "    #    ' words in the dictionary of at least length '      L
say
       do j=1  for #;      x= $.j;      y= x     /*obtain the  Jth  word in the list.   */
       if x==''  then iterate                    /*if a null, don't show variants.      */
       yy= y                                     /*the start of a list of the variants. */
                     do k=1  for length(x)-1     /*"circulate" the litters in the word. */
                     y= substr(y, 2)left(y, 1)   /*add the left letter to the right end.*/
                     if @.y==''  then iterate j  /*if not a word,  then skip this word. */
                     yy= yy','   y               /*append to the list of the variants.  */
                     @.y=                        /*nullify word to suppress permutations*/
                     end   /*k*/                 
       cw= cw + 1                                /*bump counter of circular words found.*/
       say 'circular word: '     yy              /*display a circular word and variants.*/
       end   /*j*/
say
say cw     ' circular words were found.'         /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default inputs:}}

```txt

There're  10000  entries in the dictionary (of all types):   wordlist.10k
There're  9578  words in the dictionary of at least length  3

circular word:  AIM, IMA, MAI
circular word:  ARC, RCA, CAR
circular word:  ASP, SPA, PAS
circular word:  ATE, TEA, EAT
circular word:  IPS, PSI, SIP

5  circular words were found.

```



## zkl


```zkl
// Limited to ASCII
// This is limited to the max items a Dictionary can hold
fcn teacut(wordFile){
   words:=File(wordFile).pump(Dictionary().add.fp1(True),"strip");
   seen :=Dictionary();
   foreach word in (words.keys){
      rots,w,sz := List(), word, word.len();
      if(sz>2 and word.unique().len()>2 and not seen.holds(word)){
	 do(sz-1){ 
	    w=String(w[-1],w[0,-1]);	// rotate one character
	    if(not words.holds(w)) continue(2);	// not a word, skip these
	    rots.append(w); 		// I'd like to see all the rotations
	 }
	 println(rots.append(word).sort().concat(" ")); 
	 rots.pump(seen.add.fp1(True));	// we've seen these rotations
      }
   }
}
```


```zkl
println("\nunixdict:");           teacut("unixdict.txt");
println("\nmit_wordlist_10000:"); teacut("mit_wordlist_10000.txt");
```

{{out}}

```txt

unixdict:
apt pta tap
ate eat tea
arc car rca

mit_wordlist_10000:
asp pas spa
ips psi sip
ate eat tea
aim ima mai
arc car rca

```

