+++
title = "State name puzzle"
description = ""
date = 2018-04-30T16:31:51Z
aliases = []
[extra]
id = 10410
[taxonomies]
categories = ["task", "Puzzles"]
tags = []
languages = [
  "bracmat",
  "c",
  "cpp",
  "d",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "livecode",
  "mathematica",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "tcl",
  "zkl",
]
+++

## Task

'''Background'''

This task is inspired by [http://drdobbs.com/windows/198701685 Mark Nelson's DDJ Column "Wordplay"] and one of the weekly puzzle challenges from Will Shortz on NPR Weekend Edition [http://www.npr.org/templates/story/story.php?storyId=9264290] and originally attributed to David Edelheit.

The challenge was to take the names of two U.S. States, mix them all together, then rearrange the letters to form the names of two ''different'' U.S. States (so that all four state names differ from one another).

What states are these?


The problem was reissued on  [https://tapestry.tucson.az.us/twiki/bin/view/Main/StateNamesPuzzle the Unicon Discussion Web] which includes several solutions with analysis.  Several techniques may be helpful and you may wish to refer to [[wp:Goedel_numbering|Gödel numbering]], [[wp:Equivalence_relation|equivalence relations]], and [[wp:Equivalence_classes|equivalence classes]].  The basic merits of these were discussed in the Unicon Discussion Web.

A second challenge in the form of a set of fictitious new states was also presented.

'''Task:'''

Write a program to solve the challenge using both the original list of states and the fictitious list.

Caveats:
* case and spacing aren't significant - just letters (harmonize case)
* don't expect the names to be in any order - such as being sorted
* don't rely on names to be unique (eliminate duplicates - meaning if Iowa appears twice you can only use it once)

Comma separated list of state names used in the original puzzle:

```txt

    "Alabama", "Alaska", "Arizona", "Arkansas",
    "California", "Colorado", "Connecticut",
    "Delaware",
    "Florida", "Georgia", "Hawaii",
    "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana",
    "Maine", "Maryland", "Massachusetts", "Michigan",
    "Minnesota", "Mississippi", "Missouri", "Montana",
    "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Rhode Island",
    "South Carolina", "South Dakota", "Tennessee", "Texas",
    "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"

```

Comma separated list of additional fictitious state names to be added to the original (Includes a duplicate):

```txt

"New Kory", "Wen Kory", "York New", "Kory New", "New Kory"

```



## Bracmat


```bracmat
(     Alabama
      Alaska
      Arizona
      Arkansas
      California
      Colorado
      Connecticut
      Delaware
      Florida
      Georgia
      Hawaii
      Idaho
      Illinois
      Indiana
      Iowa
      Kansas
      Kentucky
      Louisiana
      Maine
      Maryland
      Massachusetts
      Michigan
      Minnesota
      Mississippi
      Missouri
      Montana
      Nebraska
      Nevada
      "New Hampshire"
      "New Jersey"
      "New Mexico"
      "New York"
      "North Carolina"
      "North Dakota"
      Ohio
      Oklahoma
      Oregon
      Pennsylvania
      "Rhode Island"
      "South Carolina"
      "South Dakota"
      Tennessee
      Texas
      Utah
      Vermont
      Virginia
      Washington
      "West Virginia"
      Wisconsin
      Wyoming
  : ?states
& "New Kory" "Wen Kory" "York New" "Kory New" "New Kory":?extrastates
& ( "State name puzzle"
  =     allStates State state statechars char
      , A Z S1 S2 S3 S4 L1 L2 L3 L4 L12
    .   0:?allStates
      &   whl
        ' ( !arg:%?State ?arg
          & low$!State:?state
          & 0:?statechars
          &   whl
            ' ( @(!state:? (%@:~" ":?char) ?state)
              & !char+!statechars:?statechars
              )
          & (!State.!statechars)+!allStates:?allStates
          )
      & (   !allStates
          :   ?
            + ?*(?S1.?L1)
            + ?A
            + ?*(?S2.?L2)
            + ( ?Z
              & !L1+!L2:?L12
              &   !A+!Z
                :   ?
                  + ?*(?S3.?L3&!L12+-1*!L3:?L4)
                  + ?
                  +   ?
                    * ( ?S4
                      .   !L4
                        & out$(!S1 "+" !S2 "=" !S3 "+" !S4)
                        & ~
                      )
                  + ?
              )
        | out$"No more solutions"
        )
  )
& "State name puzzle"$!states
& "State name puzzle"$(!states !extrastates)
);
```

Output:

```txt
North Carolina + South Dakota = North Dakota + South Carolina
No more solutions
Kory New + New Kory = New York + Wen Kory
Kory New + New Kory = New York + York New
Kory New + New Kory = Wen Kory + York New
Kory New + New York = New Kory + Wen Kory
Kory New + New York = New Kory + York New
Kory New + New York = Wen Kory + York New
Kory New + Wen Kory = New Kory + New York
Kory New + Wen Kory = New Kory + York New
Kory New + Wen Kory = New York + York New
Kory New + York New = New Kory + New York
Kory New + York New = New Kory + Wen Kory
Kory New + York New = New York + Wen Kory
New Kory + New York = Wen Kory + York New
New Kory + Wen Kory = New York + York New
New Kory + York New = New York + Wen Kory
North Carolina + South Dakota = North Dakota + South Carolina
No more solutions
```



## C

Sort by letter occurence and deal with dupes.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define USE_FAKES 1

const char *states[] = {
#if USE_FAKES
	"New Kory", "Wen Kory", "York New", "Kory New", "New Kory",
#endif
	"Alabama", "Alaska", "Arizona", "Arkansas",
	"California", "Colorado", "Connecticut",
	"Delaware",
	"Florida", "Georgia", "Hawaii",
	"Idaho", "Illinois", "Indiana", "Iowa",
	"Kansas", "Kentucky", "Louisiana",
	"Maine", "Maryland", "Massachusetts", "Michigan",
	"Minnesota", "Mississippi", "Missouri", "Montana",
	"Nebraska", "Nevada", "New Hampshire", "New Jersey",
	"New Mexico", "New York", "North Carolina", "North Dakota",
	"Ohio", "Oklahoma", "Oregon",
	"Pennsylvania", "Rhode Island",
	"South Carolina", "South Dakota", "Tennessee", "Texas",
	"Utah", "Vermont", "Virginia",
	"Washington", "West Virginia", "Wisconsin", "Wyoming"
};

int n_states = sizeof(states)/sizeof(*states);
typedef struct { unsigned char c[26]; const char *name[2]; } letters;

void count_letters(letters *l, const char *s)
{
	int c;
	if (!l->name[0]) l->name[0] = s;
	else l->name[1] = s;

	while ((c = *s++)) {
		if (c >= 'a' && c <= 'z') l->c[c - 'a']++;
		if (c >= 'A' && c <= 'Z') l->c[c - 'A']++;
	}
}

int lcmp(const void *aa, const void *bb)
{
	int i;
	const letters *a = aa, *b = bb;
	for (i = 0; i < 26; i++)
		if      (a->c[i] > b->c[i]) return  1;
		else if (a->c[i] < b->c[i]) return -1;
	return 0;
}

int scmp(const void *a, const void *b)
{
	return strcmp(*(const char *const *)a, *(const char *const *)b);
}

void no_dup()
{
	int i, j;

	qsort(states, n_states, sizeof(const char*), scmp);

	for (i = j = 0; i < n_states;) {
		while (++i < n_states && !strcmp(states[i], states[j]));
		if (i < n_states) states[++j] = states[i];
	}

	n_states = j + 1;
}

void find_mix()
{
	int i, j, n;
	letters *l, *p;

	no_dup();
	n = n_states * (n_states - 1) / 2;
	p = l = calloc(n, sizeof(letters));

	for (i = 0; i < n_states; i++)
		for (j = i + 1; j < n_states; j++, p++) {
			count_letters(p, states[i]);
			count_letters(p, states[j]);
		}

	qsort(l, n, sizeof(letters), lcmp);

	for (j = 0; j < n; j++) {
		for (i = j + 1; i < n && !lcmp(l + j, l + i); i++) {
			if (l[j].name[0] == l[i].name[0]
				|| l[j].name[1] == l[i].name[0]
				|| l[j].name[1] == l[i].name[1])
				continue;
			printf("%s + %s => %s + %s\n",
				l[j].name[0], l[j].name[1], l[i].name[0], l[i].name[1]);
		}
	}
	free(l);
}

int main(void)
{
	find_mix();
	return 0;
}
```



## C++

Ported from C solution.

```cpp
#include <algorithm>
#include <iostream>
#include <string>
#include <array>
#include <vector>

template<typename T>
T unique(T&& src)
{
    T retval(std::move(src));
    std::sort(retval.begin(), retval.end(), std::less<typename T::value_type>());
    retval.erase(std::unique(retval.begin(), retval.end()), retval.end());
    return retval;
}

#define USE_FAKES 1

auto states = unique(std::vector<std::string>({
#if USE_FAKES
    "Slender Dragon", "Abalamara",
#endif
    "Alabama", "Alaska", "Arizona", "Arkansas",
    "California", "Colorado", "Connecticut",
    "Delaware",
    "Florida", "Georgia", "Hawaii",
    "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana",
    "Maine", "Maryland", "Massachusetts", "Michigan",
    "Minnesota", "Mississippi", "Missouri", "Montana",
    "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Rhode Island",
    "South Carolina", "South Dakota", "Tennessee", "Texas",
    "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"
}));

struct counted_pair
{
    std::string name;
    std::array<int, 26> count{};

    void count_characters(const std::string& s)
    {
        for (auto&& c : s) {
            if (c >= 'a' && c <= 'z') count[c - 'a']++;
            if (c >= 'A' && c <= 'Z') count[c - 'A']++;
        }
    }

    counted_pair(const std::string& s1, const std::string& s2)
        : name(s1 + " + " + s2)
    {
        count_characters(s1);
        count_characters(s2);
    }
};

bool operator<(const counted_pair& lhs, const counted_pair& rhs)
{
    auto lhs_size = lhs.name.size();
    auto rhs_size = rhs.name.size();
    return lhs_size == rhs_size
            ? std::lexicographical_compare(lhs.count.begin(),
                                           lhs.count.end(),
                                           rhs.count.begin(),
                                           rhs.count.end())
            : lhs_size < rhs_size;
}

bool operator==(const counted_pair& lhs, const counted_pair& rhs)
{
    return lhs.name.size() == rhs.name.size() && lhs.count == rhs.count;
}

int main()
{
    const int n_states = states.size();

    std::vector<counted_pair> pairs;
    for (int i = 0; i < n_states; i++) {
        for (int j = 0; j < i; j++) {
            pairs.emplace_back(counted_pair(states[i], states[j]));
        }
    }
    std::sort(pairs.begin(), pairs.end());

    auto start = pairs.begin();
    while (true) {
        auto match = std::adjacent_find(start, pairs.end());
        if (match == pairs.end()) {
            break;
        }
        auto next = match + 1;
        std::cout << match->name << " => " << next->name << "\n";
        start = next;
    }
}
```



## D


```d
import std.stdio, std.algorithm, std.string, std.exception;

auto states = ["Alabama", "Alaska", "Arizona", "Arkansas",
"California", "Colorado", "Connecticut", "Delaware", "Florida",
"Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
"Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
"Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
"Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
"New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
"Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
"South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
"Washington", "West Virginia", "Wisconsin", "Wyoming",
// Uncomment the next line for the fake states.
// "New Kory", "Wen Kory", "York New", "Kory New", "New Kory"
];

void main() {
  states.length -= states.sort().uniq.copy(states).length;

  string[][const ubyte[]] smap;
  foreach (immutable i, s1; states[0 .. $ - 1])
    foreach (s2; states[i + 1 .. $])
      smap[(s1 ~ s2).dup.representation.sort().release.assumeUnique]
        ~= s1 ~ " + " ~ s2;

  writefln("%-(%-(%s = %)\n%)",
           smap.values.sort().filter!q{ a.length > 1 });
}
```

```txt
North Carolina + South Dakota = North Dakota + South Carolina
```



## Go


```go
package main

import (
    "fmt"
    "unicode"
)

var states = []string{"Alabama", "Alaska", "Arizona", "Arkansas",
    "California", "Colorado", "Connecticut",
    "Delaware",
    "Florida", "Georgia", "Hawaii",
    "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana",
    "Maine", "Maryland", "Massachusetts", "Michigan",
    "Minnesota", "Mississippi", "Missouri", "Montana",
    "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Rhode Island",
    "South Carolina", "South Dakota", "Tennessee", "Texas",
    "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"}

func main() {
    play(states)
    play(append(states,
        "New Kory", "Wen Kory", "York New", "Kory New", "New Kory"))
}

func play(states []string) {
    fmt.Println(len(states), "states:")
    // get list of unique state names
    set := make(map[string]bool, len(states))
    for _, s := range states {
        set[s] = true
    }
    // make parallel arrays for unique state names and letter histograms
    s := make([]string, len(set))
    h := make([][26]byte, len(set))
    var i int
    for us := range set {
        s[i] = us
        for _, c := range us {
            if u := uint(unicode.ToLower(c)) - 'a'; u < 26 {
                h[i][u]++
            }
        }
        i++
    }
    // use map to find matches.  map key is sum of histograms of
    // two different states.  map value is indexes of the two states.
    type pair struct {
        i1, i2 int
    }
    m := make(map[string][]pair)
    b := make([]byte, 26) // buffer for summing histograms
    for i1, h1 := range h {
        for i2 := i1 + 1; i2 < len(h); i2++ {
            // sum histograms
            for i := range b {
                b[i] = h1[i] + h[i2][i]
            }
            k := string(b) // make key from buffer.
            // now loop over any existing pairs with the same key,
            // printing any where both states of this pair are different
            // than the states of the existing pair
            for _, x := range m[k] {
                if i1 != x.i1 && i1 != x.i2 && i2 != x.i1 && i2 != x.i2 {
                    fmt.Printf("%s, %s = %s, %s\n", s[i1], s[i2],
                        s[x.i1], s[x.i2])
                }
            }
            // store this pair in the map whether printed or not.
            m[k] = append(m[k], pair{i1, i2})
        }
    }
}
```

Output:

```txt

50 states:
North Dakota, South Carolina = North Carolina, South Dakota
55 states:
South Dakota, North Carolina = North Dakota, South Carolina
New Kory, Kory New = Wen Kory, York New
New Kory, Kory New = Wen Kory, New York
New Kory, York New = Wen Kory, Kory New
New Kory, York New = Wen Kory, New York
New Kory, New York = Wen Kory, Kory New
New Kory, New York = Wen Kory, York New
Kory New, York New = Wen Kory, New Kory
Kory New, York New = Wen Kory, New York
Kory New, York New = New Kory, New York
Kory New, New York = Wen Kory, New Kory
Kory New, New York = Wen Kory, York New
Kory New, New York = New Kory, York New
York New, New York = Wen Kory, New Kory
York New, New York = Wen Kory, Kory New
York New, New York = New Kory, Kory New

```



## Haskell


```haskell
{-# LANGUAGE TupleSections #-}

import Data.Char (toLower, isLetter)
import Data.List (sort, sortBy, nub, groupBy)
import Data.Function (on)

stateNames :: [String]
stateNames=
    ["Alabama",
     "Alaska",
     "Arizona",
     "Arkansas",
     "California",
     "Colorado",
     "Connecticut",
     "Delaware",
     "Florida",
     "Georgia",
     "Hawaii",
     "Idaho",
     "Illinois",
     "Indiana",
     "Iowa",
     "Kansas",
     "Kentucky",
     "Louisiana",
     "Maine",
     "Maryland",
     "Massachusetts",
     "Michigan",
     "Minnesota",
     "Mississippi",
     "Missouri",
     "Montana",
     "Nebraska",
     "Nevada",
     "New Hampshire",
     "New Jersey",
     "New Mexico",
     "New York",
     "North Carolina",
     "North Dakota",
     "Ohio",
     "Oklahoma",
     "Oregon",
     "Pennsylvania",
     "Rhode Island",
     "South Carolina",
     "South Dakota",
     "Tennessee",
     "Texas",
     "Utah",
     "Vermont",
     "Virginia",
     "Washington",
     "West Virginia",
     "Wisconsin",
     "Wyoming"]

fakeStateNames :: [String]
fakeStateNames =
    ["New Kory",
     "Wen Kory",
     "York New",
     "Kory New",
     "New Kory"]

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (y:ys) = map (y,) ys ++ pairs ys

puzzle :: [String] -> [((String,String), (String, String))]
puzzle states =
    concatMap (filter isValid.pairs) $
    map (map snd) $
    filter ((>1) . length ) $
    groupBy ((==) `on` fst) $
    sortBy (compare `on` fst) [(pkey (a++b), (a,b)) | (a,b) <- pairs (nub $ sort states)] where
        pkey = sort . filter isLetter . map toLower
        isValid ((a0, a1),(b0, b1)) = (a0 /= b0) && (a0 /= b1) && (a1 /= b0) && (a1 /= b1)

main :: IO ()
main = do
    putStrLn $ "Matching pairs generated from "
               ++ show (length stateNames) ++ " state names and "
               ++ show (length fakeStateNames) ++ " fake state names:"
    mapM_ print $ puzzle $ stateNames ++ fakeStateNames
```

<pre style="font-size:80%">Matching pairs generated from 50 state names and 5 fake state names:
(("North Carolina","South Dakota"),("North Dakota","South Carolina"))
(("Kory New","New Kory"),("New York","Wen Kory"))
(("Kory New","New Kory"),("New York","York New"))
(("Kory New","New Kory"),("Wen Kory","York New"))
(("Kory New","New York"),("New Kory","Wen Kory"))
(("Kory New","New York"),("New Kory","York New"))
(("Kory New","New York"),("Wen Kory","York New"))
(("Kory New","Wen Kory"),("New Kory","New York"))
(("Kory New","Wen Kory"),("New Kory","York New"))
(("Kory New","Wen Kory"),("New York","York New"))
(("Kory New","York New"),("New Kory","New York"))
(("Kory New","York New"),("New Kory","Wen Kory"))
(("Kory New","York New"),("New York","Wen Kory"))
(("New Kory","New York"),("Wen Kory","York New"))
(("New Kory","Wen Kory"),("New York","York New"))
(("New Kory","York New"),("New York","Wen Kory"))

```



=={{header|Icon}} and {{header|Unicon}}==

###  Equivalence Class Solution


```Icon
link strings                 # for csort and deletec

procedure main(arglist)
    ECsolve(S1 := getStates())     # original state names puzzle
    ECsolve(S2 := getStates2())    # modified fictious names puzzle
    GNsolve(S1)
    GNsolve(S2)
end

procedure ECsolve(S)         # Solve challenge using equivalence classes
    local T,x,y,z,i,t,s,l,m
    st := &time              # mark runtime
    /S := getStates()        # default
    every insert(states := set(),deletec(map(!S),' \t'))  # ignore case & space

    # Build a table containing sets of state name pairs
    # keyed off of canonical form of the pair
    # Use csort(s) rather than cset(s) to preserve the numbers of each letter
    # Since we care not of X&Y .vs. Y&X keep only X&Y

    T := table()
    every (x := !states ) & ( y := !states ) do
    if z := csort(x || (x << y)) then {
        /T[z] := []
        put(T[z],set(x,y))
    }

    # For each unique key (canonical pair) find intersection of all pairs
    # Output is <current key matched> <key> <pairs>

    i := m := 0       # keys (i) and pairs (m) matched
    every z := key(T) do {
        s := &null
        every l := !T[z] do {
            /s :=  l
            s **:= l
        }
        if *s = 0 then {
            i +:= 1
            m +:= *T[z]
            every x := !T[z] do {
                #writes(i," ",z)  # uncomment for equiv class and match count
                every writes(!x," ")
                write()
            }
        }
    }
    write("... runtime ",(&time - st)/1000.,"\n",m," matches found.")
end
```


The following are common routines:
```Icon
procedure getStates()   # return list of state names
return ["Alabama", "Alaska", "Arizona", "Arkansas",
       "California", "Colorado", "Connecticut",
       "Delaware",
       "Florida", "Georgia", "Hawaii",
       "Idaho", "Illinois", "Indiana", "Iowa",
       "Kansas", "Kentucky", "Louisiana",
       "Maine", "Maryland", "Massachusetts", "Michigan",
       "Minnesota", "Mississippi", "Missouri", "Montana",
       "Nebraska", "Nevada", "New Hampshire", "New Jersey",
       "New Mexico", "New York", "North Carolina", "North Dakota",
       "Ohio", "Oklahoma", "Oregon",
       "Pennsylvania", "Rhode Island",
       "South Carolina", "South Dakota", "Tennessee", "Texas",
       "Utah", "Vermont", "Virginia",
       "Washington", "West Virginia", "Wisconsin", "Wyoming"]
end

procedure getStates2() # return list of state names + fictious states
return getStates() ||| ["New Kory", "Wen Kory", "York New", "Kory New", "New Kory"]
end
```



###  Godel Number Solution


```Icon
link factors

procedure GNsolve(S)
    local min, max
    st := &time
    equivClasses := table()
    statePairs := table()
    /S := getStates()
    every put(states := [], map(!S)) # Make case insignificant
    min := proc("min",0)             # Link "factors" loses max/min functions
    max := proc("max",0)             # ... these statements get them back

    # Build a table of equivalence classes (all state pairs in the
    #   same equivalence class have the same characters in them)
    #   Output new pair couples *before* adding each state pair to class.

    every (state1 := |get(states)) & (state2 := !states) do {
        if state1 ~== state2 then {
            statePair := min(state1, state2)||":"||max(state1,state2)
            if /statePairs[statePair] := set(state1, state2) then {
                signature := getClassSignature(state1, state2)
                /equivClasses[signature] := set()
                every *(statePairs[statePair] **   # require 4 distinct states
                statePairs[pair := !equivClasses[signature]]) == 0 do {
                    write(statePair, " and ", pair)
                }
                insert(equivClasses[signature], statePair)
            }
        }
    }

    write(&errout, "Time: ", (&time-st)/1000.0)
end

# Build a (Godel) signature identifying the equivalence class for state pair s.

procedure getClassSignature(s1, s2)
    static G
    initial G := table()
    /G[s1] := gn(s1)
    /G[s2] := gn(s2)
    return G[s1]*G[s2]
end

procedure gn(s)  # Compute the Godel number for a string (letters only)
    static xlate
    local p, i, z
    initial {
        xlate := table(1)
        p := create prime()
        every i := 1 to 26 do {
            xlate[&lcase[i]] := xlate[&ucase[i]] := @p
        }
    }
    z := 1
    every z *:= xlate[!s]
    return z
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings.icn provides deletec, csort]
[http://www.cs.arizona.edu/icon/library/src/procs/factors.icn factors.icn provides prime]

Sample Output (ECsolve):
```txt
northcarolina southdakota
northdakota southcarolina
... runtime 0.019
2 matches found.
wenkory yorknew
wenkory newyork
newyork yorknew
wenkory korynew
newyork korynew
newkory korynew
korynew yorknew
wenkory newkory
newkory newyork
newkory yorknew
northcarolina southdakota
northdakota southcarolina
... runtime 0.026
12 matches found.
```


Sample Output (GNsolve):
```txt
north dakota:south carolina and north carolina:south dakota
Time: 0.008999999999999999
north dakota:south carolina and north carolina:south dakota
new kory:wen kory and new york:york new
new kory:wen kory and kory new:new york
new kory:york new and new york:wen kory
new kory:york new and kory new:new york
kory new:new kory and new york:wen kory
kory new:new kory and new york:york new
wen kory:york new and kory new:new york
wen kory:york new and kory new:new kory
wen kory:york new and new kory:new york
kory new:wen kory and new york:york new
kory new:wen kory and new kory:york new
kory new:wen kory and new kory:new york
kory new:york new and new york:wen kory
kory new:york new and new kory:wen kory
kory new:york new and new kory:new york
Time: 0.018
```



## J


Implementation:


```j
require'strings stats'

states=:<;._2]0 :0-.LF

Alabama,Alaska,Arizona,Arkansas,California,Colorado,
Connecticut,Delaware,Florida,Georgia,Hawaii,Idaho,
Illinois,Indiana,Iowa,Kansas,Kentucky,Louisiana,
Maine,Maryland,Massachusetts,Michigan,Minnesota,
Mississippi,Missouri,Montana,Nebraska,Nevada,
New Hampshire,New Jersey,New Mexico,New York,
North Carolina,North Dakota,Ohio,Oklahoma,Oregon,
Pennsylvania,Rhode Island,South Carolina,
South Dakota,Tennessee,Texas,Utah,Vermont,Virginia,
Washington,West Virginia,Wisconsin,Wyoming,
Maine,Maine,Maine,Maine,Maine,Maine,Maine,Maine,

)

pairUp=: (#~ matchUp)@({~ 2 comb #)@~.
matchUp=: (i.~ ~: i:~)@:(<@normalize@;"1)
normalize=: /:~@tolower@-.&' '
```


In action:


```j
   pairUp states
┌──────────────┬──────────────┐
│North Carolina│South Dakota  │
├──────────────┼──────────────┤
│North Dakota  │South Carolina│
└──────────────┴──────────────┘
```


Note: this approach is sufficient to solve the original problem, but does not properly deal with the addition of fictitious states.  So:


```j
isolatePairs=: ~.@matchUp2@(#~ *./@matchUp"2)@({~ 2 comb #)
matchUp2=: /:~"2@:(/:~"1)@(#~ 4=#@~.@,"2)
```


In action:


```j
   isolatePairs pairUp 'New Kory';'Wen Kory';'York New';'Kory New';'New Kory';states
┌──────────────┬──────────────┐
│Kory New      │York New      │
├──────────────┼──────────────┤
│New Kory      │Wen Kory      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│New Kory      │Wen Kory      │
├──────────────┼──────────────┤
│New York      │York New      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │New York      │
├──────────────┼──────────────┤
│New Kory      │Wen Kory      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │Wen Kory      │
├──────────────┼──────────────┤
│New Kory      │York New      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│New Kory      │York New      │
├──────────────┼──────────────┤
│New York      │Wen Kory      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │New York      │
├──────────────┼──────────────┤
│New Kory      │York New      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │New Kory      │
├──────────────┼──────────────┤
│Wen Kory      │York New      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │New Kory      │
├──────────────┼──────────────┤
│New York      │Wen Kory      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │New Kory      │
├──────────────┼──────────────┤
│New York      │York New      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│New Kory      │New York      │
├──────────────┼──────────────┤
│Wen Kory      │York New      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │Wen Kory      │
├──────────────┼──────────────┤
│New Kory      │New York      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │York New      │
├──────────────┼──────────────┤
│New Kory      │New York      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │New York      │
├──────────────┼──────────────┤
│Wen Kory      │York New      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │Wen Kory      │
├──────────────┼──────────────┤
│New York      │York New      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│Kory New      │York New      │
├──────────────┼──────────────┤
│New York      │Wen Kory      │
└──────────────┴──────────────┘

┌──────────────┬──────────────┐
│North Carolina│South Dakota  │
├──────────────┼──────────────┤
│North Dakota  │South Carolina│
└──────────────┴──────────────┘
```



## Java

```java
import java.util.*;
import java.util.stream.*;

public class StateNamePuzzle {

    static String[] states = {"Alabama", "Alaska", "Arizona", "Arkansas",
        "California", "Colorado", "Connecticut", "Delaware", "Florida",
        "Georgia", "hawaii", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
        "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
        "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
        "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
        "New York", "North Carolina ", "North Dakota", "Ohio", "Oklahoma",
        "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
        "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
        "Washington", "West Virginia", "Wisconsin", "Wyoming",
        "New Kory", "Wen Kory", "York New", "Kory New", "New Kory",};

    public static void main(String[] args) {
        solve(Arrays.asList(states));
    }

    static void solve(List<String> input) {
        Map<String, String> orig = input.stream().collect(Collectors.toMap(
                s -> s.replaceAll("\\s", "").toLowerCase(), s -> s, (s, a) -> s));

        input = new ArrayList<>(orig.keySet());

        Map<String, List<String[]>> map = new HashMap<>();
        for (int i = 0; i < input.size() - 1; i++) {
            String pair0 = input.get(i);
            for (int j = i + 1; j < input.size(); j++) {

                String[] pair = {pair0, input.get(j)};
                String s = pair0 + pair[1];
                String key = Arrays.toString(s.chars().sorted().toArray());

                List<String[]> val = map.getOrDefault(key, new ArrayList<>());
                val.add(pair);
                map.put(key, val);
            }
        }

        map.forEach((key, list) -> {
            for (int i = 0; i < list.size() - 1; i++) {
                String[] a = list.get(i);
                for (int j = i + 1; j < list.size(); j++) {
                    String[] b = list.get(j);

                    if (Stream.of(a[0], a[1], b[0], b[1]).distinct().count() < 4)
                        continue;

                    System.out.printf("%s + %s = %s + %s %n", orig.get(a[0]),
                            orig.get(a[1]), orig.get(b[0]), orig.get(b[1]));
                }
            }
        });
    }
}
```


Output:


```txt
Wen Kory + Kory New = York New + New Kory
Wen Kory + Kory New = York New + New York
Wen Kory + Kory New = New Kory + New York
Wen Kory + York New = Kory New + New Kory
Wen Kory + York New = Kory New + New York
Wen Kory + York New = New Kory + New York
Wen Kory + New Kory = Kory New + York New
Wen Kory + New Kory = Kory New + New York
Wen Kory + New Kory = York New + New York
Wen Kory + New York = Kory New + York New
Wen Kory + New York = Kory New + New Kory
Wen Kory + New York = York New + New Kory
Kory New + York New = New Kory + New York
Kory New + New Kory = York New + New York
Kory New + New York = York New + New Kory
South Dakota + North Carolina  = North Dakota + South Carolina
```



## jq

```jq
# Input: a string
# Output: an array, being the exploded form of the normalized input
def normalize:
  explode
  | map(if . >= 97 then (. - 97) elif . >= 65 then (. - 65) else empty end);

# Input: an array of strings
# Output: a dictionary with key:value pairs: normalizedString:string
def dictionary:
  reduce .[] as $s ( {}; . + { ($s|normalize|implode): $s });

# Input: an array of strings (e.g. state names)
# Output: a stream of solutions
def solve:

  # Given a pair of normalized state names as lists of integers:
  def nletters: map(length) | add;

  # input [[s1,s2], [t2,t2]]
  def solved:
    ( .[0] | add | sort) ==  (.[1] | add | sort);

  unique
  | length as $l
  | dictionary as $dictionary
  | ($dictionary | keys | map(explode)) as $states
  | reduce ( range(0; $l) as $s1
                 | range($s1+1; $l) as $s2
                 | range($s1+1; $l) as $t1
	         | select($s2 != $t1)
	         | range($t1+1; $l) as $t2
     	         | select($s2 != $t2)
	         | [[$states[$s1], $states[$s2]], [$states[$t1], $states[$t2]]] ) as $quad
       ([];
        if ($quad[0] | nletters) == ($quad[1] | nletters)
	   and ($quad | solved)
	then . + [$quad | map( map(  $dictionary[ implode ] ))]
	else .
	end)
  | .[];
```


'''The task:'''

```jq
def States: [
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
    "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"
];

def task:
  "Real state names:",
  (States | solve),
  "",
  "States together with fictional state names:",
  (States + ["New Kory", "Wen Kory", "York New", "Kory New", "New Kory"] | solve)   ;

task
```

```sh
$ jq -c -n -r -f State_name_puzzle.jq
Real state names:
[["North Carolina","South Dakota"],["North Dakota","South Carolina"]]

States together with fictional state names:
[["Kory New","New Kory"],["New York","Wen Kory"]]
[["Kory New","New Kory"],["New York","York New"]]
[["Kory New","New Kory"],["Wen Kory","York New"]]
[["Kory New","New York"],["New Kory","Wen Kory"]]
[["Kory New","New York"],["New Kory","York New"]]
[["Kory New","New York"],["Wen Kory","York New"]]
[["Kory New","Wen Kory"],["New Kory","New York"]]
[["Kory New","Wen Kory"],["New Kory","York New"]]
[["Kory New","Wen Kory"],["New York","York New"]]
[["Kory New","York New"],["New Kory","New York"]]
[["Kory New","York New"],["New Kory","Wen Kory"]]
[["Kory New","York New"],["New York","Wen Kory"]]
[["New Kory","New York"],["Wen Kory","York New"]]
[["New Kory","Wen Kory"],["New York","York New"]]
[["New Kory","York New"],["New York","Wen Kory"]]
[["North Carolina","South Dakota"],["North Dakota","South Carolina"]]
```



## Julia

'''Module''':

```julia
module StateNamePuzzle

const realnames = ["Alabama", "Alaska", "Arizona", "Arkansas", "California",
"Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
"Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
"Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
"Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
"New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
"Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
"Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
"Wisconsin", "Wyoming"]

const fictitious = ["New Kory", "Wen Kory", "York New", "Kory New", "New Kory"]

function combine(a::AbstractString, b::AbstractString)
    chars = vcat(collect(Char, a), collect(Char, b))
    sort!(chars)
    return join(chars)
end

function solve(input::Vector{<:AbstractString})
    dict = Dict{String,String}()
    for state in input
        key = replace(state, " ", "") |> lowercase
        if !haskey(dict, key)
            dict[key] = state
        end
    end
    keyset = collect(keys(dict))
    solutions = String[]
    duplicates = String[]
    for i in eachindex(keyset), j in (i+1):endof(keyset)
        len1 = length(keyset[i]) + length(keyset[j])
        combined1 = combine(keyset[i], keyset[j])
        for k in eachindex(keyset), l in k+1:endof(keyset)
            k ∈ (i, j) && continue
            l ∈ (i, j) && continue
            len2 = length(keyset[k]) + length(keyset[l])
            len1 != len2 && continue
            combined2 = combine(keyset[k], keyset[l])
            if combined1 == combined2
                f1 = dict[keyset[i]] * " + " * dict[keyset[j]]
                f2 = dict[keyset[k]] * " + " * dict[keyset[l]]
                f3 = f1 * " = " * f2
                f3 ∈ duplicates && continue
                push!(solutions, f3)
                f4 = f2 * " = " * f1
                push!(duplicates, f4)
            end
        end
    end
    return sort!(solutions)
end

end  # module StateNamePuzzle
```


'''Main''':

```julia
println("Real states:")
foreach(println, StateNamePuzzle.solve(StateNamePuzzle.realnames))

println("\nReal + fictitious state:")
foreach(println, StateNamePuzzle.solve(vcat(StateNamePuzzle.realnames,
    StateNamePuzzle.fictitious)))
```


```txt
Real states:
South Dakota + North Carolina = South Carolina + North Dakota

Real + fictitious state:
Kory New + New Kory = New York + York New
Kory New + New Kory = Wen Kory + New York
Kory New + New Kory = Wen Kory + York New
Kory New + New York = New Kory + Wen Kory
Kory New + New York = New Kory + York New
Kory New + New York = Wen Kory + York New
Kory New + Wen Kory = New Kory + New York
Kory New + Wen Kory = New Kory + York New
Kory New + Wen Kory = New York + York New
Kory New + York New = New Kory + New York
Kory New + York New = New Kory + Wen Kory
Kory New + York New = Wen Kory + New York
New Kory + New York = Wen Kory + York New
New Kory + Wen Kory = New York + York New
New Kory + York New = Wen Kory + New York
South Dakota + North Carolina = South Carolina + North Dakota
```



## Kotlin


```scala
// version 1.2.10

fun solve(states: List<String>) {
    val dict = mutableMapOf<String, String>()
    for (state in states) {
        val key = state.toLowerCase().replace(" ", "")
        if (dict[key] == null) dict.put(key, state)
    }
    val keys = dict.keys.toList()
    val solutions = mutableListOf<String>()
    val duplicates = mutableListOf<String>()
    for (i in 0 until keys.size) {
        for (j in i + 1 until keys.size) {
            val len = keys[i].length + keys[j].length
            val chars = (keys[i] + keys[j]).toCharArray()
            chars.sort()
            val combined = String(chars)
            for (k in 0 until keys.size) {
                for (l in k + 1 until keys.size) {
                    if (k == i || k == j || l == i || l == j) continue
                    val len2 = keys[k].length + keys[l].length
                    if (len2 != len) continue
                    val chars2 = (keys[k] + keys[l]).toCharArray()
                    chars2.sort()
                    val combined2 = String(chars2)
                    if (combined == combined2) {
                        val f1 = "${dict[keys[i]]} + ${dict[keys[j]]}"
                        val f2 = "${dict[keys[k]]} + ${dict[keys[l]]}"
                        val f3 = "$f1 = $f2"
                        if (f3 in duplicates) continue
                        solutions.add(f3)
                        val f4 = "$f2 = $f1"
                        duplicates.add(f4)
                    }
                }
            }
        }
    }
    solutions.sort()
    for ((i, sol) in solutions.withIndex()) {
        println("%2d  %s".format(i + 1, sol))
    }
}

fun main(args: Array<String>) {
    val states = listOf(
        "Alabama", "Alaska", "Arizona", "Arkansas",
        "California", "Colorado", "Connecticut",
        "Delaware",
        "Florida", "Georgia", "Hawaii",
        "Idaho", "Illinois", "Indiana", "Iowa",
        "Kansas", "Kentucky", "Louisiana",
        "Maine", "Maryland", "Massachusetts", "Michigan",
        "Minnesota", "Mississippi", "Missouri", "Montana",
        "Nebraska", "Nevada", "New Hampshire", "New Jersey",
        "New Mexico", "New York", "North Carolina", "North Dakota",
        "Ohio", "Oklahoma", "Oregon",
        "Pennsylvania", "Rhode Island",
        "South Carolina", "South Dakota", "Tennessee", "Texas",
        "Utah", "Vermont", "Virginia",
        "Washington", "West Virginia", "Wisconsin", "Wyoming"
    )
    println("Real states only:")
    solve(states)
    println()
    val fictitious = listOf(
        "New Kory", "Wen Kory", "York New", "Kory New", "New Kory"
    )
    println("Real and fictitious states:")
    solve(states + fictitious)
}
```


```txt

Real states only:
 1  North Carolina + South Dakota = North Dakota + South Carolina

Real and fictitious states:
 1  New Kory + Kory New = Wen Kory + York New
 2  New Kory + Wen Kory = York New + Kory New
 3  New Kory + York New = Wen Kory + Kory New
 4  New York + Kory New = New Kory + Wen Kory
 5  New York + Kory New = New Kory + York New
 6  New York + Kory New = Wen Kory + York New
 7  New York + New Kory = Wen Kory + Kory New
 8  New York + New Kory = Wen Kory + York New
 9  New York + New Kory = York New + Kory New
10  New York + Wen Kory = New Kory + Kory New
11  New York + Wen Kory = New Kory + York New
12  New York + Wen Kory = York New + Kory New
13  New York + York New = New Kory + Kory New
14  New York + York New = New Kory + Wen Kory
15  New York + York New = Wen Kory + Kory New
16  North Carolina + South Dakota = North Dakota + South Carolina

```



## LiveCode

This is going to be O(N^2).

```livecode
function pairwiseAnagrams X
   if the optionkey is down then breakpoint
   put the long seconds into T
   put empty into itemsSoFar
   repeat for each item W in X
      put word 1 to -1 of W into W
      if D[W] = 1 then next repeat
      put 1 into D[W]
      repeat for each item W2 in itemsSoFar
         put W,W2 & cr after WPairs[sortChars(W & W2,true)]
      end repeat
      put W & comma after itemsSoFar
   end repeat
   repeat for each key K in WPairs
      put empty into pairsSoFar
      repeat for each line L in WPairs[K]
         repeat for each line L2 in pairsSoFar
            if item 1 of L is among the items of L2 or item 2 of L is among the items of L2 then next repeat
            put L && "and" && L2 & cr after R
         end repeat
         put L & cr after pairsSoFar
      end repeat
   end repeat
   put the long seconds - T
   return char 1 to -2 of R
end pairwiseAnagrams

function sortChars X,lettersOnly
   get charsToItems(X,lettersOnly)
   sort items of it
   return itemsToChars(it)
end sortChars
 
function charsToItems X,lettersOnly
   repeat for each char C in X
      if lettersOnly and C is not in "abcdefghijklmnopqrstuvwxyz" then next repeat
      put C & comma after R
   end repeat
   return char 1 to -2 of R
end charsToItems
 
function itemsToChars X
   replace comma with empty in X
   return X
end itemsToChars
```



## Mathematica


```Mathematica
letters[words_,n_] := Sort[Flatten[Characters /@ Take[words,n]]];
groupSameQ[g1_, g2_] := Sort /@ Partition[g1, 2] === Sort /@ Partition[g2, 2];
permutations[{a_, b_, c_, d_}] = Union[Permutations[{a, b, c, d}], SameTest -> groupSameQ];

Select[Flatten[
  permutations /@
   Subsets[Union[ToLowerCase/@{"Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida",
      "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
      "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
      "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
      "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
      "West Virginia", "Wisconsin", "Wyoming"}], {4}], 1],
 letters[#, 2] === letters[#, -2] &]
```



## Perl


```perl
#!/usr/bin/perl
use warnings;
use strict;
use feature qw{ say };


sub uniq {
    my %uniq;
    undef @uniq{ @_ };
    return keys %uniq
}


sub puzzle {
    my @states = uniq(@_);

    my %pairs;
    for my $state1 (@states) {
        for my $state2 (@states) {
            next if $state1 le $state2;
            my $both = join q(),
                       grep ' ' ne $_,
                       sort split //,
                       lc "$state1$state2";
            push @{ $pairs{$both} }, [ $state1, $state2 ];
        }
    }

    for my $pair (keys %pairs) {
        next if 2 > @{ $pairs{$pair} };

        for my $pair1 (@{ $pairs{$pair} }) {
            for my $pair2 (@{ $pairs{$pair} }) {
                next if 4 > uniq(@$pair1, @$pair2)
                     or $pair1->[0] lt $pair2->[0];

                say join ' = ', map { join ' + ', @$_ } $pair1, $pair2;
            }
        }
    }
}

my @states = ( 'Alabama', 'Alaska', 'Arizona', 'Arkansas',
               'California', 'Colorado', 'Connecticut', 'Delaware',
               'Florida', 'Georgia', 'Hawaii',
               'Idaho', 'Illinois', 'Indiana', 'Iowa',
               'Kansas', 'Kentucky', 'Louisiana',
               'Maine', 'Maryland', 'Massachusetts', 'Michigan',
               'Minnesota', 'Mississippi', 'Missouri', 'Montana',
               'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey',
               'New Mexico', 'New York', 'North Carolina', 'North Dakota',
               'Ohio', 'Oklahoma', 'Oregon',
               'Pennsylvania', 'Rhode Island',
               'South Carolina', 'South Dakota', 'Tennessee', 'Texas',
               'Utah', 'Vermont', 'Virginia',
               'Washington', 'West Virginia', 'Wisconsin', 'Wyoming',
             );

my @fictious = ( 'New Kory', 'Wen Kory', 'York New', 'Kory New', 'New Kory' );

say scalar @states, ' states:';
puzzle(@states);

say @states + @fictious, ' states:';
puzzle(@states, @fictious);
```



## Perl 6

```perl6
my @states = <
    Alabama Alaska Arizona Arkansas California Colorado Connecticut Delaware
    Florida Georgia Hawaii Idaho Illinois Indiana Iowa Kansas Kentucky
    Louisiana Maine Maryland Massachusetts Michigan Minnesota Mississippi
    Missouri Montana Nebraska Nevada New_Hampshire New_Jersey New_Mexico
    New_York North_Carolina North_Dakota Ohio Oklahoma Oregon Pennsylvania
    Rhode_Island South_Carolina South_Dakota Tennessee Texas Utah Vermont
    Virginia Washington West_Virginia Wisconsin Wyoming
>;

say "50 states:";
.say for anastates @states;

say "\n54 states:";
.say for sort anastates @states, < New_Kory Wen_Kory York_New Kory_New New_Kory >;

sub anastates (*@states) {
    my @s = @states.unique».subst('_', ' ');

    my @pairs = gather for ^@s -> $i {
	for $i ^..^ @s -> $j {
	    take [ @s[$i], @s[$j] ];
	}
    }

    my $equivs = hash @pairs.classify: *.lc.comb.sort.join;

    gather for $equivs.values -> @c {
	for ^@c -> $i {
	    for $i ^..^ @c -> $j {
		my $set = set @c[$i].list, @c[$j].list;
		take @c[$i].list.join(', ') ~ ' = ' ~ @c[$j].list.join(', ') if $set == 4;
	    }
	}
    }
}
```


Output:

```txt
50 states:
North Carolina, South Dakota = North Dakota, South Carolina

54 states:
New Kory, Kory New = Wen Kory, York New
New Kory, Wen Kory = York New, Kory New
New Kory, York New = Wen Kory, Kory New
New York, Kory New = New Kory, Wen Kory
New York, Kory New = New Kory, York New
New York, Kory New = Wen Kory, York New
New York, New Kory = Wen Kory, Kory New
New York, New Kory = Wen Kory, York New
New York, New Kory = York New, Kory New
New York, Wen Kory = New Kory, Kory New
New York, Wen Kory = New Kory, York New
New York, Wen Kory = York New, Kory New
New York, York New = New Kory, Kory New
New York, York New = New Kory, Wen Kory
New York, York New = Wen Kory, Kory New
North Carolina, South Dakota = North Dakota, South Carolina
```



## Phix


```Phix
constant states = {"Alabama", "Alaska", "Arizona", "Arkansas",
                   "California", "Colorado", "Connecticut", "Delaware",
                   "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                   "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
                   "Maine", "Maryland", "Massachusetts", "Michigan",
                   "Minnesota", "Mississippi", "Missouri", "Montana",
                   "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                   "New Mexico", "New York", "North Carolina", "North Dakota",
                   "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
                   "Rhode Island", "South Carolina", "South Dakota",
                   "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                   "Washington", "West Virginia", "Wisconsin", "Wyoming"},
--       extras = {"New Kory", "Wen Kory", "York New", "Kory New", "New Kory"}
         extras = {"Slender Dragon", "Abalamara"}

function no_dup(sequence s)
    s = sort(s)
    for i=length(s) to 2 by -1 do
        if s[i]=s[i-1] then
            s[i] = s[$]
            s = s[1..$-1]
        end if
    end for
    return s
end function

procedure play(sequence s)
    s = no_dup(s)
    destroy_dict(1) -- empty dict
    for i=1 to length(s)-1 do
        for j=i+1 to length(s) do
            string key = trim(sort(lower(s[i]&s[j])))
            object data = getd(key)
            if data=0 then
                putd(key,{{i,j}})
            else
                for k=1 to length(data) do
                    integer {m,n} = data[k]
                    if m!=i and m!=j and n!=i and n!=j then
                        ?{s[i],s[j],"<==>",s[m],s[n]}
                    end if
                end for
                putd(key,append(data,{i,j}))
            end if
        end for
    end for
end procedure
play(states)
?"==="
play(states&extras)
```

```txt

{"North Dakota","South Carolina","<==>","North Carolina","South Dakota"}
"==="
{"Alabama","Arkansas","<==>","Abalamara","Kansas"}
{"North Dakota","South Carolina","<==>","North Carolina","South Dakota"}
{"Oregon","Rhode Island","<==>","Ohio","Slender Dragon"}

```



## PicoLisp


```PicoLisp
(setq *States
   (group
      (mapcar '((Name) (cons (clip (sort (chop (lowc Name)))) Name))
         (quote
            "Alabama" "Alaska" "Arizona" "Arkansas"
            "California" "Colorado" "Connecticut"
            "Delaware"
            "Florida" "Georgia" "Hawaii"
            "Idaho" "Illinois" "Indiana" "Iowa"
            "Kansas" "Kentucky" "Louisiana"
            "Maine" "Maryland" "Massachusetts" "Michigan"
            "Minnesota" "Mississippi" "Missouri" "Montana"
            "Nebraska" "Nevada" "New Hampshire" "New Jersey"
            "New Mexico" "New York" "North Carolina" "North Dakota"
            "Ohio" "Oklahoma" "Oregon"
            "Pennsylvania" "Rhode Island"
            "South Carolina" "South Dakota" "Tennessee" "Texas"
            "Utah" "Vermont" "Virginia"
            "Washington" "West Virginia" "Wisconsin" "Wyoming"
            "New Kory" "Wen Kory" "York New" "Kory New" "New Kory" ) ) ) )

(extract
   '((P)
      (when (cddr P)
         (mapcar
            '((X)
               (cons
                  (cadr (assoc (car X) *States))
                  (cadr (assoc (cdr X) *States)) ) )
            (cdr P) ) ) )
   (group
      (mapcon
         '((X)
            (extract
               '((Y)
                  (cons
                     (sort (conc (copy (caar X)) (copy (car Y))))
                     (caar X)
                     (car Y) ) )
               (cdr X) ) )
         *States ) ) )
```

Output:

```txt
-> ((("North Carolina" . "South Dakota") ("North Dakota" . "South Carolina")))
```



## Prolog


Works with SWI-Prolog. Use of Goedel numbers.

```Prolog
state_name_puzzle :-
	L = ["Alabama", "Alaska", "Arizona", "Arkansas",
	     "California", "Colorado", "Connecticut",
	     "Delaware",
	     "Florida", "Georgia", "Hawaii",
	     "Idaho", "Illinois", "Indiana", "Iowa",
	     "Kansas", "Kentucky", "Louisiana",
	     "Maine", "Maryland", "Massachusetts", "Michigan",
	     "Minnesota", "Mississippi", "Missouri", "Montana",
	     "Nebraska", "Nevada", "New Hampshire", "New Jersey",
	     "New Mexico", "New York", "North Carolina", "North Dakota",
	     "Ohio", "Oklahoma", "Oregon",
	     "Pennsylvania", "Rhode Island",
	     "South Carolina", "South Dakota", "Tennessee", "Texas",
	     "Utah", "Vermont", "Virginia",
	     "Washington", "West Virginia", "Wisconsin", "Wyoming",
	     "New Kory", "Wen Kory", "York New", "Kory New", "New Kory"],

	maplist(goedel, L, R),

	% sort remove duplicates
	sort(R, RS),

	study(RS).

study([]).

study([V-Word|T]) :-
	study_1_Word(V-Word, T, T),
	study(T).


study_1_Word(_, [], _).
study_1_Word(V1-W1, [V2-W2 | T1], T) :-
	TT is V1+V2,
	study_2_Word(W1, W2, TT, T),
	study_1_Word(V1-W1, T1, T).

study_2_Word(_W1, _W2, _TT, []).

study_2_Word(W1, W2, TT, [V3-W3 | T]) :-
	(   W2 \= W3 -> study_3_Word(W1, W2, TT, V3-W3, T); true),
	study_2_Word(W1, W2, TT, T).

study_3_Word(_W1, _W2, _TT, _V3-_W3, []).

study_3_Word(W1, W2, TT, V3-W3, [V4-W4|T]) :-
	TT1 is V3 + V4,
	(   TT1 < TT -> study_3_Word(W1, W2, TT, V3-W3, T)
	;   (TT1 = TT -> ( W4 \= W2 -> format('~w & ~w  with ~w & ~w~n', [W1, W2, W3, W4])
	                               ; true),
           	         study_3_Word(W1, W2, TT, V3-W3, T))
	;   true).

% Compute a Goedel number for the word
goedel(Word, Goedel-A) :-
	name(A, Word),
	downcase_atom(A, Amin),
	atom_codes(Amin, LA),
	compute_Goedel(LA, 0, Goedel).

compute_Goedel([], G, G).

compute_Goedel([32|T], GC, GF) :-
	compute_Goedel(T, GC, GF).

compute_Goedel([H|T], GC, GF) :-
	Ind is H - 97,
	GC1 is GC + 26 ** Ind,
	compute_Goedel(T, GC1, GF).

```

Output :

```txt
  ?- time(state_name_puzzle).
North Carolina & South Dakota  with North Dakota & South Carolina
Kory New & New Kory  with New York & Wen Kory
Kory New & New Kory  with New York & York New
Kory New & New Kory  with Wen Kory & York New
Kory New & New York  with New Kory & Wen Kory
Kory New & New York  with New Kory & York New
Kory New & New York  with Wen Kory & York New
Kory New & Wen Kory  with New Kory & New York
Kory New & Wen Kory  with New Kory & York New
Kory New & Wen Kory  with New York & York New
Kory New & York New  with New Kory & New York
Kory New & York New  with New Kory & Wen Kory
Kory New & York New  with New York & Wen Kory
New Kory & New York  with Wen Kory & York New
New Kory & Wen Kory  with New York & York New
New Kory & York New  with New York & Wen Kory
% 1,076,511 inferences, 1.078 CPU in 1.141 seconds (94% CPU, 998503 Lips)
true .

```




## Python

```python
from collections import defaultdict

states = ["Alabama", "Alaska", "Arizona", "Arkansas",
"California", "Colorado", "Connecticut", "Delaware", "Florida",
"Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
"Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
"Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
"Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
"New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
"Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
"South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
"Washington", "West Virginia", "Wisconsin", "Wyoming",
# Uncomment the next line for the fake states.
# "New Kory", "Wen Kory", "York New", "Kory New", "New Kory"
]

states = sorted(set(states))

smap = defaultdict(list)
for i, s1 in enumerate(states[:-1]):
    for s2 in states[i + 1:]:
        smap["".join(sorted(s1 + s2))].append(s1 + " + " + s2)

for pairs in sorted(smap.itervalues()):
    if len(pairs) > 1:
        print " = ".join(pairs)
```



## Racket


```racket

#lang racket
(define states
  (list->set
   (map string-downcase
        '("Alabama" "Alaska" "Arizona" "Arkansas"
          "California" "Colorado" "Connecticut"
          "Delaware"
          "Florida" "Georgia" "Hawaii"
          "Idaho" "Illinois" "Indiana" "Iowa"
          "Kansas" "Kentucky" "Louisiana"
          "Maine" "Maryland" "Massachusetts" "Michigan"
          "Minnesota" "Mississippi" "Missouri" "Montana"
          "Nebraska""Nevada" "New Hampshire" "New Jersey"
          "New Mexico" "New York" "North Carolina" "North Dakota"
          "Ohio" "Oklahoma" "Oregon"
          "Pennsylvania" "Rhode Island"
          "South Carolina" "South Dakota" "Tennessee" "Texas"
          "Utah" "Vermont" "Virginia"
          "Washington" "West Virginia" "Wisconsin" "Wyoming"
          ; "New Kory" "Wen Kory" "York New" "Kory New" "New Kory"
          ))))

(define (canon s t)
  (sort (append (string->list s) (string->list t)) char<? ))

(define seen (make-hash))
(for* ([s1 states] [s2 states] #:when (string<? s1 s2))
  (define c (canon s1 s2))
  (cond [(hash-ref seen c (λ() (hash-set! seen c (list s1 s2)) #f))
         => (λ(states) (displayln (~v states (list s1 s2))))]))

```

Output:

```racket

'("north dakota" "south carolina") '("north carolina" "south dakota")

```



## REXX

Code was added to the REXX program to remove dead-end words (state names) that can't possibly be part of

a solution, in particular, words that contain a unique letter (among all the state names).

```rexx
/*REXX program  (state name puzzle)  rearranges two state's names ──► two new states.   */
!='Alabama,  Alaska, Arizona,  Arkansas, California,    Colorado, Connecticut,       Delaware, Florida, Georgia,',
  'Hawaii,   Idaho,  Illinois, Indiana,  Iowa, Kansas,  Kentucky, Louisiana,  Maine, Maryland, Massachusetts,   ',
  'Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico,',
  'New York, North Carolina,  North Dakota,  Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina,',
  'South Dakota,  Tennessee,  Texas,  Utah,  Vermont,   Virginia, Washington, West Virginia, Wisconsin,  Wyoming'
parse arg xtra;    !=! ',' xtra                     /*add optional  (fictitious)  names.*/
@abcU= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';     !=space(!) /*!: the state list, no extra blanks*/
deads=0;    dups=0;    L.=0;     !orig=!;      @@.= /*initialize some REXX variables.   */
z=0                                                 /* [↑]  elide  dend─end (DE) states.*/
    do de=0  for 2;              !=!orig            /*use original state list for each. */
    @.=
        do states=0  by 0  until !==''              /*parse until the cows come home.   */
        parse var !  x  ','  !;       x=space(x)    /*remove all blanks from state name.*/
        if @.x\==''  then do                        /*was state was already specified?  */
                          if de  then iterate       /*don't tell error if doing 2nd pass*/
                          dups=dups + 1             /*bump the duplicate counter.       */
                          say 'ignoring the 2nd naming of the state: '    x;      iterate
                          end
        @.x=x                                       /*indicate this state name exists.  */
        y=space(x,0);    upper y;    yLen=length(y) /*get upper name with no spaces; Len*/
        if de  then do                              /*Is the firstt pass?  Then process.*/
                         do j=1  for yLen           /*see if it's a dead─end state name.*/
                         _=substr(y, j, 1)          /* _:  is some state name character.*/
                         if L._ \== 1  then iterate /*Count ¬ 1?  Then state name is OK.*/
                         say 'removing dead─end state  [which has the letter '   _"]: "  x
                         deads=deads + 1            /*bump number of dead─ends states.  */
                         iterate states             /*go and process another state name.*/
                         end   /*j*/
                    z=z+1                           /*bump counter of the state names.  */
                    #.z=y;  ##.z=x                  /*assign state name;  also original.*/
                    end
               else do k=1  for yLen                /*inventorize letters of state name.*/
                    _=substr(y,k,1);   L._=L._ + 1  /*count each letter in state name.  */
                    end   /*k*/
        end   /*states*/                            /*the index STATES isn't incremented*/
    end       /*de*/
call list                                           /*list state names in order given.  */
                   say z     'state name's(z)                "are useable."
if dups \==0  then say dups  'duplicate of a state's(dups)   'ignored.'
if deads\==0  then say deads 'dead─end state's(deads)        'deleted.'
sols=0                                              /*number of solutions found (so far)*/
say                                                 /*[↑]  look for mix and match states*/
     do j=1  for z     /* ◄──────────────────────────────────────────────────────────┐  */
       do k=j+1  to z                               /* ◄─── state K,  state J  ►─────┘  */
       if #.j<<#.k  then JK=#.j || #.k              /*is the state in the proper order? */
                    else JK=#.k || #.j              /*No,  then use the new state name. */
         do m=1  for z; if m==j | m==k then iterate /*no state  overlaps  are allowed.  */
         if verify(#.m, jk) \== 0      then iterate /*is this state name even possible? */
         nJK=elider(JK, #.m)                        /*a new JK, after eliding #.m chars.*/
           do n=m+1  to z; if n==j | n==k then iterate      /*no overlaps are allowed.  */
           if verify(#.n, nJK) \== 0      then iterate      /*is it possible?           */
           if elider(nJK, #.n) \== ''     then iterate      /*any leftovers letters?    */
           if #.m<<#.n  then MN=#.m || #.n                  /*is it in the proper order?*/
                        else MN=#.n || #.m                  /*we found a new state name.*/
           if @@.JK.MN\=='' | @@.MN.JK\==""  then iterate   /*was it done before?       */
           say 'found: '      ##.j','     ##.k       "  ───►  "        ##.m','      ##.n
           @@.JK.MN=1                            /*indicate this solution as being found*/
           sols=sols+1                           /*bump the number of solutions found.  */
           end   /*n*/
         end     /*m*/
       end       /*k*/
     end         /*j*/
say                                              /*show a blank line for easier reading.*/
if sols==0  then sols= 'No'                      /*use mucher gooder (sic) Englishings. */
say sols  'solution's(sols)    "found."          /*display the number of solutions found*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
elider: parse arg hay,pins                       /*remove letters (pins) from haystack. */
                            do e=1  for length(pins);    p=pos( substr( pins, e, 1),  hay)
                            if p==0  then iterate   ;    hay=overlay(' ', hay, p)
                            end   /*e*/          /* [↑]  remove a letter from haystack. */
        return space(hay, 0)                     /*remove blanks from the haystack.     */
/*──────────────────────────────────────────────────────────────────────────────────────*/
list:   say;   do i=1  for z;   say right(i, 9)   ##.i;   end;            say;      return
s:      if arg(1)==1  then return arg(3);    return word(arg(2) 's', 1)    /*pluralizer.*/
```

<pre style="height:60ex">
removing dead─end state  [which has the letter  Z]:  Arizona
removing dead─end state  [which has the letter  J]:  New Jersey

        1 Alabama
        2 Alaska
        3 Arkansas
        4 California
        5 Colorado
        6 Connecticut
        7 Delaware
        8 Florida
        9 Georgia
       10 Hawaii
       11 Idaho
       12 Illinois
       13 Indiana
       14 Iowa
       15 Kansas
       16 Kentucky
       17 Louisiana
       18 Maine
       19 Maryland
       20 Massachusetts
       21 Michigan
       22 Minnesota
       23 Mississippi
       24 Missouri
       25 Montana
       26 Nebraska
       27 Nevada
       28 New Hampshire
       29 New Mexico
       30 New York
       31 North Carolina
       32 North Dakota
       33 Ohio
       34 Oklahoma
       35 Oregon
       36 Pennsylvania
       37 Rhode Island
       38 South Carolina
       39 South Dakota
       40 Tennessee
       41 Texas
       42 Utah
       43 Vermont
       44 Virginia
       45 Washington
       46 West Virginia
       47 Wisconsin
       48 Wyoming

48 state names are useable.
2 dead─end states deleted.

found:  North Carolina, South Dakota   ───►   North Dakota, South Carolina

1 solution found.

```

'''output''' when using the input of:   <tt> New Kory, Wen Kory, York New, Kory New, New Kory </tt>
<pre style="height:60ex">
ignoring the 2nd naming of the state:  New Kory
removing dead─end state  [which has the letter  Z]:  Arizona
removing dead─end state  [which has the letter  J]:  New Jersey

        1 Alabama
        2 Alaska
        3 Arkansas
        4 California
        5 Colorado
        6 Connecticut
        7 Delaware
        8 Florida
        9 Georgia
       10 Hawaii
       11 Idaho
       12 Illinois
       13 Indiana
       14 Iowa
       15 Kansas
       16 Kentucky
       17 Louisiana
       18 Maine
       19 Maryland
       20 Massachusetts
       21 Michigan
       22 Minnesota
       23 Mississippi
       24 Missouri
       25 Montana
       26 Nebraska
       27 Nevada
       28 New Hampshire
       29 New Mexico
       30 New York
       31 North Carolina
       32 North Dakota
       33 Ohio
       34 Oklahoma
       35 Oregon
       36 Pennsylvania
       37 Rhode Island
       38 South Carolina
       39 South Dakota
       40 Tennessee
       41 Texas
       42 Utah
       43 Vermont
       44 Virginia
       45 Washington
       46 West Virginia
       47 Wisconsin
       48 Wyoming
       49 New Kory
       50 Wen Kory
       51 York New
       52 Kory New

52 state names are useable.
1 duplicate of a state ignored.
2 dead─end states deleted.

found:  New York, New Kory   ───►   Wen Kory, York New
found:  New York, New Kory   ───►   Wen Kory, Kory New
found:  New York, New Kory   ───►   York New, Kory New
found:  New York, Wen Kory   ───►   New Kory, York New
found:  New York, Wen Kory   ───►   New Kory, Kory New
found:  New York, Wen Kory   ───►   York New, Kory New
found:  New York, York New   ───►   New Kory, Wen Kory
found:  New York, York New   ───►   New Kory, Kory New
found:  New York, York New   ───►   Wen Kory, Kory New
found:  New York, Kory New   ───►   New Kory, Wen Kory
found:  New York, Kory New   ───►   New Kory, York New
found:  New York, Kory New   ───►   Wen Kory, York New
found:  North Carolina, South Dakota   ───►   North Dakota, South Carolina
found:  New Kory, Wen Kory   ───►   York New, Kory New
found:  New Kory, York New   ───►   Wen Kory, Kory New
found:  New Kory, Kory New   ───►   Wen Kory, York New

16 solutions found.

```



## Ruby

```ruby
require 'set'

# 26 prime numbers
Primes = [ 2,  3,  5,  7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
          43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]
States = [
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
    "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"
]

def print_answer(states)
  # find goedel numbers for all pairs of states
  goedel = lambda {|str| str.chars.map {|c| Primes[c.ord - 65]}.reduce(:*)}
  pairs = Hash.new {|h,k| h[k] = Array.new}
  map = states.uniq.map {|state| [state, goedel[state.upcase.delete("^A-Z")]]}
  map.combination(2) {|(s1,g1), (s2,g2)| pairs[g1 * g2] << [s1, s2]}

  # find pairs without duplicates
  result = []
  pairs.values.select {|val| val.length > 1}.each do |list_of_pairs|
    list_of_pairs.combination(2) do |pair1, pair2|
      if Set[*pair1, *pair2].length == 4
        result << [pair1, pair2]
      end
    end
  end

  # output the results
  result.each_with_index do |(pair1, pair2), i|
    puts "%d\t%s\t%s" % [i+1, pair1.join(', '), pair2.join(', ')]
  end
end

puts "real states only"
print_answer(States)
puts ""
puts "with fictional states"
print_answer(States + ["New Kory", "Wen Kory", "York New", "Kory New", "New Kory"])
```


outputs
<pre style="height: 40ex; overflow: scroll">real states only
1       North Carolina, South Dakota    North Dakota, South Carolina

with fictional states
1       New York, New Kory      Wen Kory, York New
2       New York, New Kory      Wen Kory, Kory New
3       New York, New Kory      York New, Kory New
4       New York, Wen Kory      New Kory, York New
5       New York, Wen Kory      New Kory, Kory New
6       New York, Wen Kory      York New, Kory New
7       New York, York New      New Kory, Wen Kory
8       New York, York New      New Kory, Kory New
9       New York, York New      Wen Kory, Kory New
10      New York, Kory New      New Kory, Wen Kory
11      New York, Kory New      New Kory, York New
12      New York, Kory New      Wen Kory, York New
13      New Kory, Wen Kory      York New, Kory New
14      New Kory, York New      Wen Kory, Kory New
15      New Kory, Kory New      Wen Kory, York New
16      North Carolina, South Dakota    North Dakota, South Carolina
```


## Scala


```Scala
object StateNamePuzzle extends App {
  // Logic:
  def disjointPairs(pairs: Seq[Set[String]]) =
    for (a <- pairs; b <- pairs; if a.intersect(b).isEmpty) yield Set(a,b)

  def anagramPairs(words: Seq[String]) =
    (for (a <- words; b <- words; if a != b) yield Set(a, b)) // all pairs
    .groupBy(_.mkString.toLowerCase.replaceAll("[^a-z]", "").sorted) // grouped anagram pairs
    .values.map(disjointPairs).flatMap(_.distinct) // unique non-overlapping anagram pairs

  // Test:
  val states = List(
    "New Kory", "Wen Kory", "York New", "Kory New", "New Kory",
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
    "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
    "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"
  )

  println(anagramPairs(states).map(_.map(_ mkString " + ") mkString " = ") mkString "\n")
}
```

```txt
New Kory + Wen Kory = York New + Kory New
New Kory + Wen Kory = York New + New York
New Kory + Wen Kory = Kory New + New York
New Kory + York New = Wen Kory + Kory New
New Kory + York New = Wen Kory + New York
New Kory + York New = Kory New + New York
New Kory + Kory New = Wen Kory + York New
New Kory + Kory New = Wen Kory + New York
New Kory + Kory New = York New + New York
New Kory + New York = Wen Kory + York New
New Kory + New York = Wen Kory + Kory New
New Kory + New York = York New + Kory New
Wen Kory + York New = Kory New + New York
Wen Kory + Kory New = York New + New York
Wen Kory + New York = York New + Kory New
North Carolina + South Dakota = North Dakota + South Carolina
```



## Tcl


```tcl
package require Tcl 8.5
# Gödel number generator
proc goedel s {
    set primes {
	2 3 5 7 11 13 17 19 23 29 31 37 41
	43 47 53 59 61 67 71 73 79 83 89 97 101
    }
    set n 1
    foreach c [split [string toupper $s] ""] {
	if {![string is alpha $c]} continue
	set n [expr {$n * [lindex $primes [expr {[scan $c %c] - 65}]]}]
    }
    return $n
}
# Calculates the pairs of states
proc groupStates {stateList} {
    set stateList [lsort -unique $stateList]
    foreach state1 $stateList {
	foreach state2 $stateList {
	    if {$state1 >= $state2} continue
	    dict lappend group [goedel $state1$state2] [list $state1 $state2]
	}
    }
    foreach g [dict values $group] {
	if {[llength $g] > 1} {
	    foreach p1 $g {
		foreach p2 $g {
		    if {$p1 < $p2 && [unshared $p1 $p2]} {
			lappend result [list $p1 $p2]
		    }
		}
	    }
	}
    }
    return $result
}
proc unshared args {
    foreach p $args {
	foreach a $p {incr s($a)}
    }
    expr {[array size s] == [llength $args]*2}
}
# Pretty printer for state name pair lists
proc printPairs {title groups} {
    foreach group $groups {
	puts "$title Group #[incr count]"
	foreach statePair $group {
	    puts "\t[join $statePair {, }]"
	}
    }
}

set realStates {
    "Alabama" "Alaska" "Arizona" "Arkansas" "California" "Colorado"
    "Connecticut" "Delaware" "Florida" "Georgia" "Hawaii" "Idaho" "Illinois"
    "Indiana" "Iowa" "Kansas" "Kentucky" "Louisiana" "Maine" "Maryland"
    "Massachusetts" "Michigan" "Minnesota" "Mississippi" "Missouri" "Montana"
    "Nebraska" "Nevada" "New Hampshire" "New Jersey" "New Mexico" "New York"
    "North Carolina" "North Dakota" "Ohio" "Oklahoma" "Oregon" "Pennsylvania"
    "Rhode Island" "South Carolina" "South Dakota" "Tennessee" "Texas" "Utah"
    "Vermont" "Virginia" "Washington" "West Virginia" "Wisconsin" "Wyoming"
}
printPairs "Real States" [groupStates $realStates]
set falseStates {
    "New Kory" "Wen Kory" "York New" "Kory New" "New Kory"
}
printPairs "Real and False States" [groupStates [concat $realStates $falseStates]]
```

Output:

```txt

Real States Group #1
	North Carolina, South Dakota
	North Dakota, South Carolina
Real and False States Group #1
	Kory New, New Kory
	New York, Wen Kory
Real and False States Group #2
	Kory New, New Kory
	New York, York New
Real and False States Group #3
	Kory New, New Kory
	Wen Kory, York New
Real and False States Group #4
	Kory New, New York
	New Kory, Wen Kory
Real and False States Group #5
	Kory New, New York
	New Kory, York New
Real and False States Group #6
	Kory New, New York
	Wen Kory, York New
Real and False States Group #7
	Kory New, Wen Kory
	New Kory, New York
Real and False States Group #8
	Kory New, Wen Kory
	New Kory, York New
Real and False States Group #9
	Kory New, Wen Kory
	New York, York New
Real and False States Group #10
	Kory New, York New
	New Kory, New York
Real and False States Group #11
	Kory New, York New
	New Kory, Wen Kory
Real and False States Group #12
	Kory New, York New
	New York, Wen Kory
Real and False States Group #13
	New Kory, New York
	Wen Kory, York New
Real and False States Group #14
	New Kory, Wen Kory
	New York, York New
Real and False States Group #15
	New Kory, York New
	New York, Wen Kory
Real and False States Group #16
	North Carolina, South Dakota
	North Dakota, South Carolina

```



## zkl

```zkl
#<<<  // here doc
states:=("Alabama, Alaska, Arizona, Arkansas,
   California, Colorado, Connecticut, Delaware, Florida,
   Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas,
   Kentucky, Louisiana, Maine, Maryland, Massachusetts,
   Michigan, Minnesota, Mississippi, Missouri, Montana,
   Nebraska, Nevada, New Hampshire, New Jersey, New Mexico,
   New York, North Carolina, North Dakota, Ohio, Oklahoma,
   Oregon, Pennsylvania, Rhode Island, South Carolina,
   South Dakota, Tennessee, Texas, Utah, Vermont, Virginia,
   Washington, West Virginia, Wisconsin, Wyoming"
   /* Uncomment the next line for the fake states. */
   # ",New Kory, Wen Kory, York New, Kory New, New Kory"
#<<<
).split(",").apply("strip");

smap:=Dictionary();
Utils.Helpers.pickNFrom(2,states).apply2('wrap(ss){ // 1225 combinations
   key:=(ss.concat()).toLower().sort()-" ";
   smap[key]=smap.find(key,List()).append(ss.concat(" + "));
});

foreach pairs in (smap.values){ // 1224 keys
//    pairs=Utils.Helpers.listUnique(pairs);  // eliminate dups
    if(pairs.len()>1)
        println(pairs.concat(" = ")) }
```

```txt

North Carolina + South Dakota = North Dakota + South Carolina

```


