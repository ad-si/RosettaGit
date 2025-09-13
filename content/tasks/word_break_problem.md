+++
title = "Word break problem"
description = ""
date = 2019-05-20T14:41:52Z
aliases = []
[extra]
id = 21374
[taxonomies]
categories = ["task"]
tags = []
+++

; Task:Given an input string and a dictionary of words, segment the input string into a space-separated sequence of dictionary words if possible. 


## Aime


```aime
integer
wordbreak(record dict, text phrase, integer p, list words)
{
    integer complete, n;
    text s;

    complete = 0;
    if (rsk_lower(dict, phrase, s)) {
        if (s == phrase) {
            words.append(s);
            complete = 1;
        } else {
            do {
                n = 0;
                while (phrase[n] == s[n]) {
                    n += 1;
                }
                if (!n) {
                    break;
                }
                words.append(cut(s, 0, n));
                complete = wordbreak(dict, project(phrase, n), p + 1, words);
                if (complete) {
                    break;
                }
                words.delete(-1);
            } while (rsk_less(dict, s, s));
        }
    }

    if (!p) {
        o_(phrase, ":");
        if (complete) {
            words.ucall(o_, 1, " ");
        } else {
            o_(" can't break");
        }
        o_newline();

        words.clear;
    }

    complete;
}


integer
main(void)
{
    record dict;

    dict.fit("a", 0, "bc", 0, "abc", 0, "cd", 0, "b", 0);
    list("abcd", "abbc", "abcbcd", "acdbc", "abcdd").ucall(wordbreak, 1, dict, 0, list());

    return 0;
}
```

```txt
abcd: a b cd
abbc: a b bc
abcbcd: abc b cd
acdbc: a cd bc
abcdd: can't break
```



## Go


```go
package main

import (
    "fmt"
    "strings"
)

type dict map[string]bool

func newDict(words ...string) dict {
    d := dict{}
    for _, w := range words {
        d[w] = true
    }
    return d
}

func (d dict) wordBreak(s string) (broken []string, ok bool) {
    if s == "" {
        return nil, true
    }
    type prefix struct {
        length int
        broken []string
    }
    bp := []prefix{{0, nil}}
    for end := 1; end <= len(s); end++ {
        for i := len(bp) - 1; i >= 0; i-- {
            w := s[bp[i].length:end]
            if d[w] {
                b := append(bp[i].broken, w)
                if end == len(s) {
                    return b, true
                }
                bp = append(bp, prefix{end, b})
                break
            }
        }
    }
    return nil, false
}

func main() {
    d := newDict("a", "bc", "abc", "cd", "b")
    for _, s := range []string{"abcd", "abbc", "abcbcd", "acdbc", "abcdd"} {
        if b, ok := d.wordBreak(s); ok {
            fmt.Printf("%s: %s\n", s, strings.Join(b, " "))
        } else {
            fmt.Println("can't break")
        }
    }
}
```

```txt

abcd: a b cd
abbc: a b bc
abcbcd: a bc b cd
acdbc: a cd bc
can't break

```



## Haskell

```haskell
import Data.Tree
import Data.List (isPrefixOf, intercalate)

wordBreaks :: [String] -> String -> String
wordBreaks ws s =
  let parses = go <$> tokenTrees ws s
      go t
        | null (subForest t) = [rootLabel t]
        | otherwise = subForest t >>= ((:) (rootLabel t) . go)
      report xs
        | null xs = "\tNot parseable with these words"
        | otherwise = unlines $ (('\t' :) . intercalate " -> ") <$> xs
  in s ++ (':' : '\n' : report parses)

tokenTrees :: [String] -> String -> [Tree String]
tokenTrees ws = go
  where
    go s
      | s `elem` ws = [Node s []]
      | otherwise = ws >>= next s
    next s w
      | w `isPrefixOf` s = parse w (go (drop (length w) s))
      | otherwise = []
    parse w xs
      | null xs = []
      | otherwise = [Node w xs]


-- TEST ------------------------------------------------------------
ws, texts :: [String]
ws = words "a bc abc cd b"

texts = words "abcd abbc abcbcd acdbc abcdd"

main :: IO ()
main = (putStrLn . unlines) $ wordBreaks ws <$> texts
```

```txt
abcd:
    a -> b -> cd

abbc:
    a -> b -> bc

abcbcd:
    a -> bc -> b -> cd
    abc -> b -> cd

acdbc:
    a -> cd -> bc

abcdd:
    Not parseable with these words
```



## J

With such short sentences we can find the partition sets, then check that all are words.


```J

all_partitions=: <@(<;.1)"1 _~  (1,.[:#:[:i.2^<:@:#)  NB. all_partitions 'abcd'
word_break=: ([ #~ 0 = [: #&>@:] -.L:_1 _)~ all_partitions
main=: (] , (;:inv L:_1@:word_break >))"_ 0 boxopen

```



```txt

   NB. demonstrate partitions of four integers
   all_partitions i. 4
┌─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┬─────────┐
│┌───────┐│┌─────┬─┐│┌───┬───┐│┌───┬─┬─┐│┌─┬─────┐│┌─┬───┬─┐│┌─┬─┬───┐│┌─┬─┬─┬─┐│
││0 1 2 3│││0 1 2│3│││0 1│2 3│││0 1│2│3│││0│1 2 3│││0│1 2│3│││0│1│2 3│││0│1│2│3││
│└───────┘│└─────┴─┘│└───┴───┘│└───┴─┴─┘│└─┴─────┘│└─┴───┴─┘│└─┴─┴───┘│└─┴─┴─┴─┘│
└─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┴─────────┘


   NB. demonstrate word_break
   NB.   (,L:_1]0;1 2;0 1 2;2 3;1)
   NB. ┌─┬───┬─────┬───┬─┐
   NB. │0│1 2│0 1 2│2 3│1│
   NB. └─┴───┴─────┴───┴─┘
   (,L:_1]0;1 2;0 1 2;2 3;1) word_break i. 4
┌─────────┐
│┌─┬─┬───┐│
││0│1│2 3││
│└─┴─┴───┘│
└─────────┘


   NB. save and display the dictionary
   [dictionary=: ;: 'a bc abc cd b'
┌─┬──┬───┬──┬─┐
│a│bc│abc│cd│b│
└─┴──┴───┴──┴─┘

   NB. demonstrate main
   dictionary main 'abc'
┌───┬───┬────┐
│abc│abc│a bc│
└───┴───┴────┘

   NB. solution
   dictionary main ;: 'abcd abbc abcbcd acdbc abcdd'
┌──────┬────────┬─────────┐
│abcd  │a b cd  │         │
├──────┼────────┼─────────┤
│abbc  │a b bc  │         │
├──────┼────────┼─────────┤
│abcbcd│abc b cd│a bc b cd│
├──────┼────────┼─────────┤
│acdbc │a cd bc │         │
├──────┼────────┼─────────┤
│abcdd │        │         │
└──────┴────────┴─────────┘

```



## Javascript

Composing a solution from generic functions.
```javascript
(() => {
    'use strict';

    const main = () => {
        const
            wds = words('a bc abc cd b'),
            texts = words('abcd abbc abcbcd acdbc abcdd');

        return unlines(
            map(wordBreaks(wds),
                texts
            )
        );
    };

    // WORD BREAKS ----------------------------------------

    // tokenTrees :: [String] -> String -> [Tree String]
    const tokenTrees = (wds, s) => {
        const go = s =>
            wds.includes(s) ? (
                [Node(s, [])]
            ) : bindList(wds, next(s));
        const next = s => w =>
            s.startsWith(w) ? (
                parse(w, go(s.slice(w.length)))
            ) : [];
        const parse = (w, xs) =>
            0 < xs.length ? [Node(w, xs)] : xs;
        return go(s);
    };

    // wordBreaks :: [String] -> String -> String
    const wordBreaks = wds => s => {
        const
            // go :: Tree a -> [a]
            go = t => isNull(t.nest) ? [
                t.root
            ] : bindList(
                t.nest,
                compose(cons(t.root), go),
            ),
            parses = map(go, tokenTrees(wds, s));
        return `${s}:\n` + (
            0 < parses.length ? unlines(
                map(x => '\t' + intercalateS(' -> ', x),
                    parses
                )
            ) : '\t(Not parseable with these words)'
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Node :: a -> [Tree a] -> Tree a
    const Node = (v, xs) => ({
        type: 'Node',
        root: v,
        nest: xs || []
    });

    // bindList (>>=) :: [a] -> (a -> [b]) -> [b]
    const bindList = (xs, mf) => [].concat.apply([], xs.map(mf));

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // cons :: a -> [a] -> [a]
    const cons = x => xs => [x].concat(xs);

    // intercalateS :: String -> [String] -> String
    const intercalateS = (s, xs) =>
        xs.join(s);

    // isNull :: [a] -> Bool
    // isNull :: String -> Bool
    const isNull = xs =>
        Array.isArray(xs) || ('string' === typeof xs) ? (
            1 > xs.length
        ) : undefined;

    // isPrefixOf takes two lists or strings and returns
    // true iff the first is a prefix of the second.

    // isPrefixOf :: [a] -> [a] -> Bool
    // isPrefixOf :: String -> String -> Bool
    const isPrefixOf = (xs, ys) => {
        const pfx = (xs, ys) => {
            const intX = xs.length;
            return 0 < intX ? (
                ys.length >= intX ? xs[0] === ys[0] && pfx(
                    xs.slice(1), ys.slice(1)
                ) : false
            ) : true;
        };
        return 'string' !== typeof xs ? (
            pfx(xs, ys)
        ) : ys.startsWith(xs);
    };

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // words :: String -> [String]
    const words = s => s.split(/\s+/);

    // MAIN ---
    return main();
})();
```

```txt
abcd:
    a -> b -> cd
abbc:
    a -> b -> bc
abcbcd:
    a -> bc -> b -> cd
    abc -> b -> cd
acdbc:
    a -> cd -> bc
abcdd:
    (Not parseable with these words)
```



## Julia

Some extra loops to record and print all solutions.
```Julia

words = ["a", "bc", "abc", "cd", "b"]
strings = ["abcd", "abbc", "abcbcd", "acdbc", "abcdd"]

subregex = join(words, ")|(")
regexes = ["\^\(\($subregex\)\)\{$i}\$" for i in 6:-1:1]

function wordbreak()
    for s in strings
        solutions = []
        for regex in regexes
            rmat = match(Regex(regex), s)
            if rmat != nothing
                push!(solutions, ["$w" for w in Set(rmat.captures) if w != nothing])
            end
        end
        if length(solutions) > 0
            println("$(length(solutions)) Solution(s) for $s:")
            for sol in solutions
                println("   Solution: $(sol)")
            end
        else
            println("No solutions for $s : No fitting matches found.")
        end
    end
end

wordbreak()
```

```txt

1 Solution(s) for abcd:
   Solution: SubString{String}["cd", "b", "a"]
1 Solution(s) for abbc:
   Solution: SubString{String}["b", "a", "bc"]
2 Solution(s) for abcbcd:
   Solution: SubString{String}["cd", "b", "a", "bc"]
   Solution: SubString{String}["cd", "abc", "b"]
1 Solution(s) for acdbc:
   Solution: SubString{String}["cd", "a", "bc"]
No solutions for abcdd : No fitting matches found.
```



## Kotlin

I've downloaded the free dictionary at http://www.puzzlers.org/pub/wordlists/unixdict.txt for this task. All single letters from 'a' to 'z' are considered to be words by this dictionary but 'bc' and 'cd' which I'd have expected to be present are not.

```scala
// version 1.1.3

import java.io.File

val partitions = mutableListOf<List<String>>()

fun partitionString(s: String, ml: MutableList<String>, level: Int) {
    for (i in s.length - 1 downTo 1) {
        val part1 = s.substring(0, i)
        val part2 = s.substring(i)
        ml.add(part1)
        ml.add(part2)
        partitions.add(ml.toList())
        if (part2.length > 1) {
            ml.removeAt(ml.lastIndex)
            partitionString(part2, ml, level + 1)
        }
        while (ml.size > level) ml.removeAt(ml.lastIndex)
    }
}

fun main(args: Array<String>) {
    val words = File("unixdict.txt").readLines()
    val strings = listOf("abcd", "abbc", "abcbcd", "acdbc", "abcdd")
    for (s in strings) {
        partitions.clear()
        partitions.add(listOf(s))
        val ml = mutableListOf<String>()
        partitionString(s, ml, 0)
        val solutions = mutableListOf<List<String>>()
        for (partition in partitions) {
            var allInDict = true
            for (item in partition) {
                if (words.indexOf(item) == -1) {
                    allInDict = false
                    break
                }
            }
            if (allInDict) solutions.add(partition)
        }
        val plural = if (solutions.size == 1) "" else "s"
        println("$s: ${solutions.size} solution$plural")
        for (solution in solutions) {
            println("    ${solution.joinToString(" ")}")
        }
        println() 
    }     
}
```


```txt

abcd: 2 solutions
    abc d
    a b c d

abbc: 1 solution
    a b b c

abcbcd: 3 solutions
    abc b c d
    a b cb c d
    a b c b c d

acdbc: 2 solutions
    ac d b c
    a c d b c

abcdd: 2 solutions
    abc d d
    a b c d d

```


## Lua


```lua
-- a specialized dict format is used to minimize the 
-- possible candidates for this particalur problem
function genDict(ws)
  local d,dup,head,rest = {},{}
  for w in ws:gmatch"%w+" do
    local lw = w:lower()
    if not dup[lw] then
      dup[lw], head,rest = true, lw:match"^(%w)(.-)$"
      d[head] = d[head] or {n=-1}
      local len = #rest
      d[head][len] = d[head][len] or {}
      d[head][len][rest] = true
      if len>d[head].n then 
        d[head].n = len 
      end
    end
  end
  return d
end

-- sample default dict
local defWords = "a;bc;abc;cd;b"
local defDict = genDict(defWords)

function wordbreak(w, dict)
  if type(w)~='string' or w:len()==0 then 
    return nil,'emprty or not a string'
  end
  
  dict = type(dict)=='string' and genDict(dict) or dict or defDict
  
  local r, len = {}, #w
  
  -- backtracking
  local function try(i)
    if i>len then return true end
    local head = w:sub(i,i):lower()
    local d = dict[head]
    if not d then return end
    for j=math.min(d.n, len-i),0,-1 do -- prefer longer first
      if d[j] then
        local rest = w:sub(i+1,i+j):lower()
        if d[j][rest] then
          r[1+#r] = w:sub(i,i+j)
          if try(i+j+1) then 
            return true 
          else 
            r[#r]=nil 
          end
        end            
      end
    end        
  end
  
  if try(1) then 
    return table.unpack(r) 
  else 
    return nil,'-no solution-'
  end  
end

-- test
local test = {'abcd','abbc','abcbcd','acdbc','abcdd'  }
for i=1,#test do
  print(test[i],wordbreak(test[i]))
end
```

```txt
abcd	a	b	cd
abbc	a	b	bc
abcbcd	abc	b	cd
acdbc	a	cd	bc
abcdd	nil	-no solution-
```



## Perl


```perl
use strict;
use warnings;

my @words = <a o is pi ion par per sip miss able>;
print "$_: " . word_break($_,@words) . "\n" for <a aa amiss parable opera operable inoperable permission mississippi>;

sub word_break {
    my($word,@dictionary) = @_;
    my @matches;
    my $one_of = join '|', @dictionary;
    @matches = $word =~ /^ ($one_of) ($one_of)? ($one_of)? ($one_of)? $/x; # sub-optimal: limited number of matches
    return join(' ', grep {$_} @matches) || "(not possible)";
}
```

```txt
a: a
aa: a a
ado: ad o
amiss: a miss
admission: ad miss ion
parable: par able
opera: o per a
operable: o per able
inoperable: in o per able
permission: per miss ion
permissible: Not possible
mississippi: miss is sip pi
```



## Perl 6

This implementation does not necessarily find ''every'' combination, it returns the one with the longest matching tokens.


```perl6>my @words = <a bc abc cd b
;
my $regex = @words.join('|');

put "$_: ", word-break($_) for <abcd abbc abcbcd acdbc abcdd>;

sub word-break (Str $word) { ($word ~~ / ^ (<$regex>)+ $ /)[0] // "Not possible" }
```

```txt
abcd: a b cd
abbc: a b bc
abcbcd: abc b cd
acdbc: a cd bc
abcdd: Not possible
```



## Phix

See talk page

```Phix
procedure populate_dict(sequence s)
?s
    for i=1 to length(s) do setd(s[i],0) end for
end procedure
 
--/*
function valid_word(string word)
    if length(word)=1 then return find(word,{"a","i"})!=0 end if
    if find(word,{"sis","sst","se"}) then return false end if  -- hack
    for i=1 to length(word) do
        integer ch = word[i]
        if ch<'a'
        or ch>'z' then
            return false
        end if
    end for
    return true
end function
--*/

populate_dict(split("a bc abc cd b"))
--/*
integer fn = open("demo\\unixdict.txt","r")
sequence words = get_text(fn,GT_LF_STRIPPED)
close(fn)
for i=length(words) to 1 by -1 do
    if not valid_word(words[i]) then
        words[i] = words[$]
        words = words[1..$-1]
    end if
end for
populate_dict(words)
--*/ 

function prrec(sequence wordstarts, integer idx, sequence sofar, bool show)
    if idx>length(wordstarts) then
        if show then
            ?sofar
        end if
        return 1
    end if
    integer res = 0
    for i=1 to length(wordstarts[idx]) do
        string w = wordstarts[idx][i]
        res += prrec(wordstarts,idx+length(w),append(sofar,w),show)
    end for
    return res
end function
 
function flattens(sequence s)
-- remove all nesting and empty sequences from a nested sequence of strings
sequence res = {}, si
    for i=1 to length(s) do
        si = s[i]
        if string(si) then
            res = append(res,si)
        else
            res &= flattens(si)
        end if
    end for
    return res
end function
 
procedure test(string s)
integer l = length(s)
sequence wordstarts = repeat({},l), wordends = repeat(0,l)
integer wordend = 1 -- (pretend a word just ended at start)
    for i=1 to l do
        if wordend then
            for j=i to l do
                object pkey = getd_partial_key(s[i..j])
                if string(pkey) and length(pkey)>j-i and s[i..j]=pkey[1..j-i+1] then
                    if length(pkey)=j-i+1 then
                        -- exact match
                        wordstarts[i] = append(wordstarts[i],pkey)
                        wordends[j] += 1
                    end if
                else
                    exit
                end if  
            end for
        end if
        wordend = wordends[i]
    end for
    bool worthwhile = true
    while worthwhile do
        worthwhile = false
        wordend = 1 -- (pretend a word just ended at start)
        for i=1 to l do
            if wordend then
                -- eliminate any words that end before a wordstarts of {}.
                for j=length(wordstarts[i]) to 1 by -1 do
                    integer wl = length(wordstarts[i][j])
                    if i+wl<=l and wordstarts[i+wl]={} then
                        wordends[i+wl-1] -= 1
                        wordstarts[i][j..j] = {}
                        worthwhile = true
                    end if
                end for
            else
                -- elimitate all words that start here.
                for j=1 to length(wordstarts[i]) do
                    integer wl = length(wordstarts[i][j])
                    if i+wl<=l then
                        wordends[i+wl-1] -= 1
                        worthwhile = true
                    end if
                end for
                wordstarts[i] = {}
            end if
            wordend = wordends[i]
        end for
    end while
--?{wordstarts,wordends}
    if sum(wordends)=0 then
        printf(1,"%s: not possible",{s})
    else
        integer count = prrec(wordstarts,1,{},false)
        if count=1 then
            printf(1,"%s: 1 solution: %s\n",{s,join(flattens(wordstarts))})
        elsif count>20 then
            printf(1,"%s: %d solution(s): (too many to show)\n",{s,count})
            pp({wordstarts,wordends})
        else
            printf(1,"%s: %d solution(s):\n",{s,count})
            count = prrec(wordstarts,1,{},true)
        end if
    end if
end procedure
 
constant tests = {"abcd","abbc","abcbcd","acdbc","abcdd"}
--constant tests = {"wordsisstringofspaceseparatedwords"}
for i=1 to length(tests) do test(tests[i]) end for
```

```txt

{"a","bc","abc","cd","b"}
abcd: 1 solution: a b cd
abbc: 1 solution: a b bc
abcbcd: 2 solution(s):
{"a","bc","b","cd"}
{"abc","b","cd"}
acdbc: 1 solution: a cd bc
abcdd: not possible

```



## PicoLisp


```PicoLisp
(setq *Dict (quote "a" "bc" "abc" "cd" "b"))
(setq *Dict2
   (quote
      "mobile" "samsung" "sam" "sung" "man" "mango"
      "icecream" "and" "go" "i" "like" "ice" "cream" ) )

(de word (Str D)
   (let
      (Str (chop Str)
         Len (length Str)
         DP (need (inc Len))
         Res (need (inc Len))
         B 1 )
      (set DP 0)
      (map
         '((L)
            (and
               (get DP B)
               (for N (length L)
                  (let Str (pack (head N L))
                     (when (member Str D)
                        (set (nth Res (+ B N))
                           (copy (get Res B)) )
                        (queue (nth Res (+ B N)) Str)
                        (set (nth DP (+ B N))
                           (inc (get DP B)) ) ) ) ) )
            (inc 'B) )
         Str )
      (last Res) ) )

(println (word "abcd" *Dict))
(println (word "abbc" *Dict))
(println (word "abcbcd" *Dict))
(println (word "acdbc" *Dict))
(println (word "abcdd" *Dict))
(println (word "ilikesamsung" *Dict2))
(println (word "iii" *Dict2))
(println (word "ilikelikeimangoiii" *Dict2))
(println (word "samsungandmango" *Dict2))
(println (word "samsungandmangok" *Dict2))
(println (word "ksamsungandmango" *Dict2))
```

```txt

("a" "b" "cd")
("a" "b" "bc")
("a" "bc" "b" "cd")
("a" "cd" "bc")
NIL
("i" "like" "sam" "sung")
("i" "i" "i")
("i" "like" "like" "i" "man" "go" "i" "i" "i")
("sam" "sung" "and" "man" "go")
NIL
NIL

```



## Python


### Functional


The '''tokenTrees''' function recursively builds a tree of possible token sequences, using a list monad ('''concatMap''' with a function which returns its result wrapped in a list – an empty list where a parse has failed) to discard all branches which lead to dead ends. This allows us to return more than one possible word-break parse for a given lexicon and input string. (Searches for 'monadic parsing in Python' will yield references to more sophisticated uses of this general approach).

```python
'''Parsing a string for word breaks'''

from itertools import (chain)


# stringParse :: [String] -> String -> Tree String
def stringParse(lexicon):
    '''A tree of strings representing a parse of s
       in terms of the tokens in lexicon.
    '''
    return lambda s: Node(s)(
        tokenTrees(lexicon)(s)
    )


# tokenTrees :: [String] -> String -> [Tree String]
def tokenTrees(wds):
    '''A list of possible parse trees for s,
       based on the lexicon supplied in wds.
    '''
    def go(s):
        return [Node(s)([])] if s in wds else (
            concatMap(nxt(s))(wds)
        )

    def nxt(s):
        return lambda w: parse(
            w, go(s[len(w):])
        ) if s.startswith(w) else []

    def parse(w, xs):
        return [Node(w)(xs)] if xs else xs

    return lambda s: go(s)


# showParse :: Tree String -> String
def showParse(tree):
    '''Multi line display of a string followed by any
       possible parses of it, or an explanatory
       message, if no parse was possible.
    '''
    def showTokens(x):
        xs = x['nest']
        return ' ' + x['root'] + (showTokens(xs[0]) if xs else '')
    parses = tree['nest']
    return tree['root'] + ':\n' + (
        '\n'.join(
            map(showTokens, parses)
        ) if parses else ' ( Not parseable in terms of these words )'
    )


# TEST -------------------------------------------------
# main :: IO ()
def main():
    '''Parse test and display of results.'''

    lexicon = 'a bc abc cd b'.split()
    testSamples = 'abcd abbc abcbcd acdbc abcdd'.split()

    print(unlines(
        map(
            showParse,
            map(
                stringParse(lexicon),
                testSamples
            )
        )
    ))


# GENERIC FUNCTIONS ---------------------------------------

# Node :: a -> [Tree a] -> Tree a
def Node(v):
    '''Contructor for a Tree node which connects a
       value of some kind to a list of zero or
       more child trees.'''
    return lambda xs: {'type': 'Node', 'root': v, 'nest': xs}


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
abcd:
 a b cd
abbc:
 a b bc
abcbcd:
 a bc b cd
 abc b cd
acdbc:
 a cd bc
abcdd:
 ( Not parseable in terms of these words )
```



## Racket

This returns all the possible splits (and null list if none is possible). Who's to say which is the best?


```racket
#lang racket

(define render-phrases pretty-print)

(define dict-1 (list "a" "bc" "abc" "cd" "b"))
(define dict-2 (list "mobile" "samsung" "sam" "sung" "man" "mango"
                     "icecream" "and" "go" "i" "like" "ice" "cream"))

(define (word-splits str d)
  (let ((memo (make-hash)))
    (let inr ((s str))
      (hash-ref! memo s
                 (λ () (append* (filter-map (λ (w)
                                              (and (string-prefix? s w)
                                                   (if (string=? w s)
                                                       (list s)
                                                       (map (λ (tl) (string-append w " " tl))
                                                            (inr (substring s (string-length w)))))))
                                            d)))))))

(module+ main
  (render-phrases (word-splits "abcd" dict-1))
  (render-phrases (word-splits "abbc" dict-1))
  (render-phrases (word-splits "abcbcd" dict-1))
  (render-phrases (word-splits "acdbc" dict-1))
  (render-phrases (word-splits "abcdd" dict-1))
  (render-phrases (word-splits "ilikesamsung" dict-2))
  (render-phrases (word-splits "iii" dict-2))
  (render-phrases (word-splits "ilikelikeimangoiii" dict-2))
  (render-phrases (word-splits "samsungandmango" dict-2))
  (render-phrases (word-splits "samsungandmangok" dict-2))
  (render-phrases (word-splits "ksamsungandmango" dict-2)))
```

```txt
'("a b cd")
'("a b bc")
'("a bc b cd" "abc b cd")
'("a cd bc")
'()
'("i like samsung" "i like sam sung")
'("i i i")
'("i like like i man go i i i" "i like like i mango i i i")
'("samsung and man go"
  "samsung and mango"
  "sam sung and man go"
  "sam sung and mango")
'()
'()
```



## REXX

This REXX version allows the words to be tested  (and the dictionary words) to be specified on the command line. 

```rexx
/*REXX program breaks up a  word (or string)  into a  list of words  from a dictionary. */
parse arg a '/' x;   a=space(a);     x=space(x)  /*get optional args; elide extra blanks*/
if a=='' | a==","  then a= 'abcd abbc abcbcd acdbc abcdd'    /*maybe use the defaults ? */
if x=='' | x==","  then x= 'a bc abc cd b'                   /*  "    "   "      "      */
na=words(a)                                      /*the number of words to be tested.    */
nx=words(x)                                      /* "     "    "   "    " the dictionary*/
say nx  ' dictionary words: '   x                /*display the words in the dictionary. */
say                                              /*display a blank line to the terminal.*/
aw=0;  do i=1  for na;          _=word(a, i)     /*obtain a word that will be tested.   */
       aw=max(aw, length(_) )                    /*find widest width word being tested. */
       end   /*i*/                               /* [↑]  AW  is used to align the output*/
@.=0                                             /*initialize the dictionary to "null". */
xw=0;  do i=1  for nx;          _=word(x, i)     /*obtain a word from the dictionary.   */
       xw=max(xw, length(_) );  @._=1            /*find widest width dictionary word.   */
       end   /*i*/                               /* [↑]  define a dictionary word.      */
p=0                                              /* [↓]  process a word in the  A  list.*/
       do j=1  for na;          yy=word(a, j)    /*YY:   test a word  from the  A  list.*/
         do t=(nx+1)**(xw+1)  by -1  to 1  until y=='';  y=yy    /*try word possibility.*/
         $=                                      /*nullify the (possible) result list.  */
             do try=t  while y\=''               /*keep testing until  Y  is exhausted. */
             p=(try + p)  // xw    + 1           /*use a possible width for this attempt*/
             p=fw(y, p); if p==0  then iterate t /*is this part of the word not found ? */
             $=$ ?                               /*It was found. Add partial to the list*/
             y=substr(y,  p + 1)                 /*now, use and test the rest of word.  */
             end   /*try*/
         end       /*t*/

       if t==0  then $= '(not possible)'         /*indicate that the word isn't possible*/
       say right(yy, aw)    '───►'    strip($)   /*display the result to the terminal.  */
       end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fw: parse arg z,L;  do k=L  by -1  for L; ?=left(z,k); if @.?  then leave; end;   return k
```

```txt

5  dictionary words:  a bc abc cd b

  abcd ───► a b cd
  abbc ───► a b bc
abcbcd ───► abc b cd
 acdbc ───► a cd bc
 abcdd ───► (not possible)

```



## Ring


```ring

# Project : Word break problem

load "stdlib.ring"
list = ["a", "bc", "abc", "cd", "b"]
inslist = list
for n = 1 to len(inslist) - 1
      for m = len(inslist) to 1 step -1
            insert(list,0,inslist[m])
      next
next
strings = ["abcd", "abbc", "abcbcd", "acdbc", "abcdd"]
ind = len(list)
items = newlist(pow(2,len(list))-1,ind)
powerset(list,ind)

for p = 1 to len(strings)
      showarray(items,strings[p])
next

func powerset(list,ind)
        num = 0
        num2 = 0
        items = newlist(pow(2,len(list))-1,2*ind)
        for i = 2 to (2 << len(list)) - 1 step 2
             num2 = 0
             num = num + 1
             for j = 1 to len(list) 
                  if i & (1 << j)
                      num2 = num2 + 1
                      if list[j] != 0
                        items[num][num2] = list[j]
                     ok
                  ok
             next
        next
        return items

func showarray(items,par)
        ready = []
        for n = 1 to len(items)
              for m = n + 1 to len(items) - 1
                    flag = 0
                    str = ""
                    for x = 1 to len(items[n])
                         if items[n][x] != 0  
                            str = str + items[n][x] + " "
                         ok
                    next 
                    str = left(str, len(str) - 1)
                    strsave = str
                    str = substr(str, " ", "") 
                    if str = par
                       pos = find(ready,strsave)               
                       if pos = 0              
                          add(ready,strsave)
                          flag = 1 
                          see par + " = " + strsave + nl
                      ok
                      if flag != 1 
                         del(items,m)
                      ok
                   ok
              next
        next

```

Output:

```txt

abcd = a b cd
abbc = a b bc
abcbcd = abc b cd
acdbc = a cd bc

```



## Rust


### Dynamic programming


```rust
use std::collections::HashSet;
fn create_string(s: &str, v: Vec<Option<usize>>) -> String {
    let mut idx = s.len();
    let mut slice_vec = vec![];
    while let Some(prev) = v[idx] {
        slice_vec.push(&s[prev..idx]); 
        idx = prev;
    }
    slice_vec.reverse();
    slice_vec.join(" ")


}

fn word_break(s: &str, dict: HashSet<&str>) -> Option<String> {
    let size = s.len() + 1;
    let mut possible = vec![None; size];

    let check_word = |i,j| dict.get(&s[i..j]).map(|_| i);

    for i in 1..size {
        possible[i] = possible[i].or_else(|| check_word(0,i));

        if possible[i].is_some() {
            for j in i+1..size {
                possible[j] = possible[j].or_else(|| check_word(i,j));
            }

            if possible[s.len()].is_some() {
                return Some(create_string(s, possible));
            }

        };
    }
    None
}

fn main() {
    let mut set = HashSet::new();
    set.insert("a");
    set.insert("bc");
    set.insert("abc");
    set.insert("cd");
    set.insert("b");
    println!("{:?}", word_break("abcd", set).unwrap());
}
```

```txt
"a b cd"
```



## Scala


### First solution

Finds all possible solutions recursively, using a trie representation of the dictionary:

```scala
case class TrieNode(isWord: Boolean, children: Map[Char, TrieNode]) {
  def add(s: String): TrieNode = s match {
    case "" => copy(isWord = true)
    case _ => {
      val child = children.getOrElse(s.head, TrieNode(false, Map.empty))
      copy(children = children + (s.head -> child.add(s.tail)))
    }
  }
}

def buildTrie(xs: String*): TrieNode = {
  xs.foldLeft(TrieNode(false, Map.empty))(_.add(_))
}

def wordBreakRec(s: String, root: TrieNode, currentPos: TrieNode, soFar: String): List[List[String]] = {
  val usingCurrentWord = if (currentPos.isWord) {
    if (s.isEmpty) {
      List(List(soFar))
    } else {
      wordBreakRec(s, root, root, "").map(soFar :: _)
    }
  } else {
    List.empty[List[String]]
  }
  val usingCurrentPrefix = (for {
    ch <- s.headOption
    child <- currentPos.children.get(ch)
  } yield wordBreakRec(s.tail, root, child, soFar + ch)).getOrElse(List.empty)
  usingCurrentWord ++ usingCurrentPrefix
}

def wordBreak(s: String, dict: TrieNode): List[List[String]] = {
  wordBreakRec(s, dict, dict, "")
}
```

Calling it with some example strings:

```scala
val dict = buildTrie("a", "bc", "abc", "cd", "b")
val testCases = List("abcd", "abbc", "abcbcd", "acdbc", "abcdd")
for (s <- testCases) {
  val solutions = wordBreak(s, dict)
  println(s"$s has ${solutions.size} solution(s):")
  for (words <- solutions) {
    println("\t" + words.mkString(" "))
  }
}
```

```txt
abcd has 1 solution(s):
	a b cd
abbc has 1 solution(s):
	a b bc
abcbcd has 2 solution(s):
	a bc b cd
	abc b cd
acdbc has 1 solution(s):
	a cd bc
abcdd has 0 solution(s):
```


### Combined solution

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/49YwsD5/1 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/L47OzgmjSkOnQ5wfrZ5snQ Scastie (remote JVM)].

```Scala
object WordBreak extends App {
  val dict = buildTrie("a", "bc", "abc", "cd", "b")
  lazy val empty = TrieNode(isWord = false, Map.empty) // lazy or in a companion object

  case class TrieNode(isWord: Boolean, children: Map[Char, TrieNode]) {

    def add(s: String): TrieNode = {
      def child = children.withDefaultValue(empty)(s.head)

      if (s.isEmpty) copy(isWord = true)
      else copy(children = children.updated(s.head, child.add(s.tail)))
    }
  }

  def buildTrie(xs: String*): TrieNode = xs.foldLeft(empty)(_.add(_))

  def wordBreak(s: String, dict: TrieNode): List[List[String]] = {

    def wordBreakRec(s: String,
                     root: TrieNode,
                     currentPos: TrieNode,
                     soFar: String): List[List[String]] = {

      def usingCurrentWord =
        if (currentPos.isWord)
          if (s.isEmpty) List(List(soFar))
          else wordBreakRec(s, root, root, "").map(soFar :: _)
        else Nil

      def usingCurrentPrefix =
         (for {ch <- s.headOption
              child <- currentPos.children.get(ch)
        } yield wordBreakRec(s.tail, root, child, soFar + ch))
          .getOrElse(Nil)

      usingCurrentWord ++ usingCurrentPrefix
    }

    wordBreakRec(s, dict, dict, "")
  }

  // Calling it with some example strings:
  List("abcd", "abbc", "abcbcd", "acdbc", "abcdd").foreach(s => {
    val solutions = wordBreak(s, dict)

    println(s"$s has ${solutions.size} solution(s):")
    solutions.foreach(words => println(words.mkString("\t", " ", "")))
  })

}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: wordBreak (in string: stri, in array string: words, in string: resultList) is func
  result
    var boolean: found is FALSE;
  local
    var string: word is "";
  begin
    if stri = "" then
      writeln(resultList);
      found := TRUE;
    else
      for word range words do
        if startsWith(stri, word) and
            wordBreak(stri[succ(length(word)) ..], words, resultList & " " & word) then
          found := TRUE;
        end if;
      end for;
    end if;
  end func;

const proc: main is func
  local
    const array string: words is [] ("a", "bc", "abc", "cd", "b");
    var string: stri is "";
    var string: resultList is "";
  begin
    for stri range [] ("abcd", "abbc", "abcbcd", "acdbc", "abcdd") do
      write(stri <& ": ");
      if not wordBreak(stri, words, resultList) then
        writeln("can't break");
      end if;
    end for;
  end func;
```


```txt

abcd:  a b cd
abbc:  a b bc
abcbcd:  a bc b cd
 abc b cd
acdbc:  a cd bc
abcdd: can't break

```



## Sidef

```ruby
func word_break (str, words) {

    var r = ->(str, arr=[]) {
        return true if str.is_empty
        for word in (words) {
            str.begins_with(word) || next
            if (__FUNC__(str.substr(word.len), arr)) {
                arr << word
                return arr
            }
        }
        return false
    }(str)

    r.kind_of(Array) ? r.reverse : nil
}

var words = %w(a o is pi ion par per sip miss able)
var strs = %w(a amiss parable opera operable inoperable permission mississippi)

for str in (strs) {
   printf("%11s: %s\n", str, word_break(str, words) \\ '(not possible)')
}
```

```txt

          a: ["a"]
      amiss: ["a", "miss"]
    parable: ["par", "able"]
      opera: ["o", "per", "a"]
   operable: ["o", "per", "able"]
 inoperable: (not possible)
 permission: ["per", "miss", "ion"]
mississippi: ["miss", "is", "sip", "pi"]

```



## zkl


```zkl
fcn wordBreak(str,words){	// words is string of space seperated words
   words=words.split(" ");	// to list of words
   r:=fcn(str,words,sink){	// recursive search, easy to collect answer
      foreach word in (words){
	 if(not str) return(True);  // consumed string ie matched everything
	 if(str.find(word)==0){     // word starts str, 0 so answer is ordered
	    z:=word.len();
	    if(self.fcn(str.del(0,z),words,sink)) return(sink.write(word));
	 }
      }
      False		// can't make forward progress, back out & retry
   }(str,words,List());		// run the lambda
   if(False==r) return("not possible");
   r.reverse().concat(" ")
}
```


```zkl
foreach text in (T("abcd","abbc","abcbcd","acdbc","abcdd")){
   println(text,": ",wordBreak(text,"a bc abc cd b"))
}
```

```txt

abcd: a b cd
abbc: a b bc
abcbcd: a bc b cd
acdbc: a cd bc
abcdd: not possible

```

