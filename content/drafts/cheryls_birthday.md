+++
title = "Cheryl's Birthday"
description = ""
date = 2019-10-03T00:17:15Z
aliases = []
[extra]
id = 22038
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

Albert and Bernard just became friends with Cheryl, and they want to know when her birthday is.

Cheryl gave them a list of ten possible dates:
      May 15,     May 16,     May 19
      June 17,    June 18
      July 14,    July 16
      August 14,  August 15,  August 17

Cheryl then tells Albert the   ''month''   of birth,   and Bernard the   ''day''   (of the month)   of birth.
  1)  Albert:   I don't know when Cheryl's birthday is, but I know that Bernard does not know too.
  2)  Bernard:  At first I don't know when Cheryl's birthday is, but I know now.
  3)  Albert:   Then I also know when Cheryl's birthday is.


;Task
Write a computer program to deduce, by successive elimination, Cheryl's birthday.


;Related task:
* [[Sum and Product Puzzle]]


;References
* [https://en.wikipedia.org/wiki/Cheryl%27s_Birthday Wikipedia article] of the same name.
* [https://en.wikipedia.org/wiki/Tuple_relational_calculus, Tuple Relational Calculus]





## AppleScript


```applescript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions

property M : 1 -- Month
property D : 2 -- Day

on run
    -- The MONTH with only one remaining day
    -- among the DAYs with unique months,
    -- EXCLUDING months with unique days,
    -- in Cheryl's list:

    showList(uniquePairing(M, ¬
        uniquePairing(D, ¬
            monthsWithUniqueDays(false, ¬
                map(composeList({tupleFromList, |words|, toLower}), ¬
                    splitOn(", ", ¬
                        "May 15, May 16, May 19, June 17, June 18, " & ¬
                        "July 14, July 16, Aug 14, Aug 15, Aug 17"))))))

    --> "[('july', '16')]"
end run


-- QUERY FUNCTIONS ----------------------------------------

-- monthsWithUniqueDays :: Bool -> [(Month, Day)] -> [(Month, Day)]
on monthsWithUniqueDays(blnInclude, xs)
    set _months to map(my fst, uniquePairing(D, xs))
    script uniqueDay
        on |λ|(md)
            set bln to elem(fst(md), _months)
            if blnInclude then
                bln
            else
                not bln
            end if
        end |λ|
    end script
    filter(uniqueDay, xs)
end monthsWithUniqueDays


-- uniquePairing :: DatePart -> [(M, D)] -> [(M, D)]
on uniquePairing(dp, xs)
    script go
        property f : my mReturn(item dp of {my fst, my snd})
        on |λ|(md)

            set dct to f's |λ|(md)
            script unique
                on |λ|(k)
                    set mb to lookupDict(k, dct)
                    if Nothing of mb then
                        false
                    else
                        1 = length of (Just of mb)
                    end if
                end |λ|
            end script
            set uniques to filter(unique, keys(dct))

            script found
                on |λ|(tpl)
                    elem(f's |λ|(tpl), uniques)
                end |λ|
            end script
            filter(found, xs)
        end |λ|
    end script
    bindPairs(xs, go)
end uniquePairing


-- bindPairs :: [(M, D)] -> ((Dict Text [Text], Dict Text [Text])
--                                -> [(M, D)]) -> [(M, D)]
on bindPairs(xs, f)
    tell mReturn(f)
        |λ|(Tuple(dictFromPairs(xs), ¬
            dictFromPairs(map(my swap, xs))))
    end tell
end bindPairs

-- dictFromPairs :: [(M, D)] -> Dict Text [Text]
on dictFromPairs(mds)
    set gps to groupBy(|on|(my eq, my fst), ¬
        sortBy(comparing(my fst), mds))
    script kv
        on |λ|(gp)
            Tuple(fst(item 1 of gp), map(my snd, gp))
        end |λ|
    end script
    mapFromList(map(kv, gps))
end dictFromPairs


-- LIBRARY GENERICS ---------------------------------------

-- comparing :: (a -> b) -> (a -> a -> Ordering)
on comparing(f)
    script
        on |λ|(a, b)
            tell mReturn(f)
                set fa to |λ|(a)
                set fb to |λ|(b)
                if fa < fb then
                    -1
                else if fa > fb then
                    1
                else
                    0
                end if
            end tell
        end |λ|
    end script
end comparing

-- composeList :: [(a -> a)] -> (a -> a)
on composeList(fs)
    script
        on |λ|(x)
            script
                on |λ|(f, a)
                    mReturn(f)'s |λ|(a)
                end |λ|
            end script

            foldr(result, x, fs)
        end |λ|
    end script
end composeList

-- drop :: Int -> [a] -> [a]
-- drop :: Int -> String -> String
on drop(n, xs)
    set c to class of xs
    if c is not script then
        if c is not string then
            if n < length of xs then
                items (1 + n) thru -1 of xs
            else
                {}
            end if
        else
            if n < length of xs then
                text (1 + n) thru -1 of xs
            else
                ""
            end if
        end if
    else
        take(n, xs) -- consumed
        return xs
    end if
end drop

-- dropAround :: (a -> Bool) -> [a] -> [a]
-- dropAround :: (Char -> Bool) -> String -> String
on dropAround(p, xs)
    dropWhile(p, dropWhileEnd(p, xs))
end dropAround

-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile :: (Char -> Bool) -> String -> String
on dropWhile(p, xs)
    set lng to length of xs
    set i to 1
    tell mReturn(p)
        repeat while i ≤ lng and |λ|(item i of xs)
            set i to i + 1
        end repeat
    end tell
    drop(i - 1, xs)
end dropWhile

-- dropWhileEnd :: (a -> Bool) -> [a] -> [a]
-- dropWhileEnd :: (Char -> Bool) -> String -> String
on dropWhileEnd(p, xs)
    set i to length of xs
    tell mReturn(p)
        repeat while i > 0 and |λ|(item i of xs)
            set i to i - 1
        end repeat
    end tell
    take(i, xs)
end dropWhileEnd

-- elem :: Eq a => a -> [a] -> Bool
on elem(x, xs)
    considering case
        xs contains x
    end considering
end elem

-- enumFromToInt :: Int -> Int -> [Int]
on enumFromToInt(M, n)
    if M ≤ n then
        set lst to {}
        repeat with i from M to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromToInt

-- eq (==) :: Eq a => a -> a -> Bool
on eq(a, b)
    a = b
end eq

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- foldr :: (a -> b -> b) -> b -> [a] -> b
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

-- fst :: (a, b) -> a
on fst(tpl)
    if class of tpl is record then
        |1| of tpl
    else
        item 1 of tpl
    end if
end fst

-- Typical usage: groupBy(on(eq, f), xs)
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
on groupBy(f, xs)
    set mf to mReturn(f)

    script enGroup
        on |λ|(a, x)
            if length of (active of a) > 0 then
                set h to item 1 of active of a
            else
                set h to missing value
            end if

            if h is not missing value and mf's |λ|(h, x) then
                {active:(active of a) & {x}, sofar:sofar of a}
            else
                {active:{x}, sofar:(sofar of a) & {active of a}}
            end if
        end |λ|
    end script

    if length of xs > 0 then
        set dct to foldl(enGroup, {active:{item 1 of xs}, sofar:{}}, rest of xs)
        if length of (active of dct) > 0 then
            sofar of dct & {active of dct}
        else
            sofar of dct
        end if
    else
        {}
    end if
end groupBy

-- insertMap :: Dict -> String -> a -> Dict
on insertMap(rec, k, v)
    tell (current application's NSMutableDictionary's ¬
        dictionaryWithDictionary:rec)
        its setValue:v forKey:(k as string)
        return it as record
    end tell
end insertMap

-- intercalateS :: String -> [String] -> String
on intercalateS(sep, xs)
    set {dlm, my text item delimiters} to {my text item delimiters, sep}
    set s to xs as text
    set my text item delimiters to dlm
    return s
end intercalateS

-- Just :: a -> Maybe a
on Just(x)
    {type:"Maybe", Nothing:false, Just:x}
end Just

-- keys :: Dict -> [String]
on keys(rec)
    (current application's NSDictionary's dictionaryWithDictionary:rec)'s allKeys() as list
end keys

-- lookupDict :: a -> Dict -> Maybe b
on lookupDict(k, dct)
    set ca to current application
    set v to (ca's NSDictionary's dictionaryWithDictionary:dct)'s objectForKey:k
    if v ≠ missing value then
        Just(item 1 of ((ca's NSArray's arrayWithObject:v) as list))
    else
        Nothing()
    end if
end lookupDict

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- mapFromList :: [(k, v)] -> Dict
on mapFromList(kvs)
    set tpl to unzip(kvs)
    script
        on |λ|(x)
            x as string
        end |λ|
    end script
    (current application's NSDictionary's ¬
        dictionaryWithObjects:(|2| of tpl) ¬
            forKeys:map(result, |1| of tpl)) as record
end mapFromList

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- Nothing :: Maybe a
on Nothing()
    {type:"Maybe", Nothing:true}
end Nothing

-- e.g. sortBy(|on|(compare, |length|), ["epsilon", "mu", "gamma", "beta"])
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on |on|(f, g)
    script
        on |λ|(a, b)
            tell mReturn(g) to set {va, vb} to {|λ|(a), |λ|(b)}
            tell mReturn(f) to |λ|(va, vb)
        end |λ|
    end script
end |on|

-- partition :: predicate -> List -> (Matches, nonMatches)
-- partition :: (a -> Bool) -> [a] -> ([a], [a])
on partition(f, xs)
    tell mReturn(f)
        set ys to {}
        set zs to {}
        repeat with x in xs
            set v to contents of x
            if |λ|(v) then
                set end of ys to v
            else
                set end of zs to v
            end if
        end repeat
    end tell
    Tuple(ys, zs)
end partition

-- show :: a -> String
on show(e)
    set c to class of e
    if c = list then
        showList(e)
    else if c = record then
        set mb to lookupDict("type", e)
        if Nothing of mb then
            showDict(e)
        else
            script
                on |λ|(t)
                    if "Either" = t then
                        set f to my showLR
                    else if "Maybe" = t then
                        set f to my showMaybe
                    else if "Ordering" = t then
                        set f to my showOrdering
                    else if "Ratio" = t then
                        set f to my showRatio
                    else if class of t is text and t begins with "Tuple" then
                        set f to my showTuple
                    else
                        set f to my showDict
                    end if
                    tell mReturn(f) to |λ|(e)
                end |λ|
            end script
            tell result to |λ|(Just of mb)
        end if
    else if c = date then
        "\"" & showDate(e) & "\""
    else if c = text then
        "'" & e & "'"
    else if (c = integer or c = real) then
        e as text
    else if c = class then
        "null"
    else
        try
            e as text
        on error
            ("«" & c as text) & "»"
        end try
    end if
end show

-- showList :: [a] -> String
on showList(xs)
    "[" & intercalateS(", ", map(my show, xs)) & "]"
end showList

-- showTuple :: Tuple -> String
on showTuple(tpl)
    set ca to current application
    script
        on |λ|(n)
            set v to (ca's NSDictionary's dictionaryWithDictionary:tpl)'s objectForKey:(n as string)
            if v ≠ missing value then
                unQuoted(show(item 1 of ((ca's NSArray's arrayWithObject:v) as list)))
            else
                missing value
            end if
        end |λ|
    end script
    "(" & intercalateS(", ", map(result, enumFromToInt(1, length of tpl))) & ")"
end showTuple

-- snd :: (a, b) -> b
on snd(tpl)
    if class of tpl is record then
        |2| of tpl
    else
        item 2 of tpl
    end if
end snd

-- Enough for small scale sorts.
-- Use instead sortOn :: Ord b => (a -> b) -> [a] -> [a]
-- which is equivalent to the more flexible sortBy(comparing(f), xs)
-- and uses a much faster ObjC NSArray sort method
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
on sortBy(f, xs)
    if length of xs > 1 then
        set h to item 1 of xs
        set f to mReturn(f)
        script
            on |λ|(x)
                f's |λ|(x, h) ≤ 0
            end |λ|
        end script
        set lessMore to partition(result, rest of xs)
        sortBy(f, |1| of lessMore) & {h} & ¬
            sortBy(f, |2| of lessMore)
    else
        xs
    end if
end sortBy

-- splitOn :: String -> String -> [String]
on splitOn(pat, src)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, pat}
    set xs to text items of src
    set my text item delimiters to dlm
    return xs
end splitOn

-- swap :: (a, b) -> (b, a)
on swap(ab)
    if class of ab is record then
        Tuple(|2| of ab, |1| of ab)
    else
        {item 2 of ab, item 1 of ab}
    end if
end swap

-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
        set ys to {}
        repeat with i from 1 to n
            set v to xs's |λ|()
            if missing value is v then
                return ys
            else
                set end of ys to v
            end if
        end repeat
        return ys
    else
        missing value
    end if
end take

-- toLower :: String -> String
on toLower(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        lowercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toLower

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple

-- tupleFromList :: [a] -> (a, a ...)
on tupleFromList(xs)
    set lng to length of xs
    if 1 < lng then
        if 2 < lng then
            set strSuffix to lng as string
        else
            set strSuffix to ""
        end if
        script kv
            on |λ|(a, x, i)
                insertMap(a, (i as string), x)
            end |λ|
        end script
        foldl(kv, {type:"Tuple" & strSuffix}, xs) & {length:lng}
    else
        missing value
    end if
end tupleFromList

-- unQuoted :: String -> String
on unQuoted(s)
    script p
        on |λ|(x)
            --{34, 39} contains id of x
            34 = id of x
        end |λ|
    end script
    dropAround(p, s)
end unQuoted

-- unzip :: [(a,b)] -> ([a],[b])
on unzip(xys)
    set xs to {}
    set ys to {}
    repeat with xy in xys
        set end of xs to |1| of xy
        set end of ys to |2| of xy
    end repeat
    return Tuple(xs, ys)
end unzip

-- words :: String -> [String]
on |words|(s)
    set ca to current application
    (((ca's NSString's stringWithString:(s))'s ¬
        componentsSeparatedByCharactersInSet:(ca's ¬
            NSCharacterSet's whitespaceAndNewlineCharacterSet()))'s ¬
        filteredArrayUsingPredicate:(ca's ¬
            NSPredicate's predicateWithFormat:"0 < length")) as list
end |words|
```

{{Out}}

```txt
"[('july', '16')]"
```



## C++

{{trans|Go}}

```cpp
#include <algorithm>
#include <iostream>
#include <vector>
using namespace std;

const vector<string> MONTHS = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

struct Birthday {
    int month, day;

    friend ostream &operator<<(ostream &, const Birthday &);
};

ostream &operator<<(ostream &out, const Birthday &birthday) {
    return out << MONTHS[birthday.month - 1] << ' ' << birthday.day;
}

template <typename C>
bool monthUniqueIn(const Birthday &b, const C &container) {
    auto it = cbegin(container);
    auto end = cend(container);
    int count = 0;
    while (it != end) {
        if (it->month == b.month) {
            count++;
        }
        it = next(it);
    }
    return count == 1;
}

template <typename C>
bool dayUniqueIn(const Birthday &b, const C &container) {
    auto it = cbegin(container);
    auto end = cend(container);
    int count = 0;
    while (it != end) {
        if (it->day == b.day) {
            count++;
        }
        it = next(it);
    }
    return count == 1;
}

template <typename C>
bool monthWithUniqueDayIn(const Birthday &b, const C &container) {
    auto it = cbegin(container);
    auto end = cend(container);
    while (it != end) {
        if (it->month == b.month && dayUniqueIn(*it, container)) {
            return true;
        }
        it = next(it);
    }
    return false;
}

int main() {
    vector<Birthday> choices = {
        {5, 15}, {5, 16}, {5, 19}, {6, 17}, {6, 18},
        {7, 14}, {7, 16}, {8, 14}, {8, 15}, {8, 17},
    };

    // Albert knows the month but doesn't know the day.
    // So the month can't be unique within the choices.
    vector<Birthday> filtered;
    for (auto bd : choices) {
        if (!monthUniqueIn(bd, choices)) {
            filtered.push_back(bd);
        }
    }

    // Albert also knows that Bernard doesn't know the answer.
    // So the month can't have a unique day.
    vector<Birthday> filtered2;
    for (auto bd : filtered) {
        if (!monthWithUniqueDayIn(bd, filtered)) {
            filtered2.push_back(bd);
        }
    }

    // Bernard now knows the answer.
    // So the day must be unique within the remaining choices.
    vector<Birthday> filtered3;
    for (auto bd : filtered2) {
        if (dayUniqueIn(bd, filtered2)) {
            filtered3.push_back(bd);
        }
    }

    // Albert now knows the answer too.
    // So the month must be unique within the remaining choices.
    vector<Birthday> filtered4;
    for (auto bd : filtered3) {
        if (monthUniqueIn(bd, filtered3)) {
            filtered4.push_back(bd);
        }
    }

    if (filtered4.size() == 1) {
        cout << "Cheryl's birthday is " << filtered4[0] << '\n';
    } else {
        cout << "Something went wrong!\n";
    }

    return 0;
}
```

{{out}}

```txt
Cheryl's birthday is Jul 16
```



## C#


```c#
public static class CherylsBirthday
{
    public static void Main() {
        var dates = new HashSet<(string month, int day)> {
            ("May", 15),
            ("May", 16),
            ("May", 19),
            ("June", 17),
            ("June", 18),
            ("July", 14),
            ("July", 16),
            ("August", 14),
            ("August", 15),
            ("August", 17)
        };

        Console.WriteLine(dates.Count + " remaining.");
        //The month cannot have a unique day.
        var monthsWithUniqueDays = dates.GroupBy(d => d.day).Where(g => g.Count() == 1).Select(g => g.First().month).ToHashSet();
        dates.RemoveWhere(d => monthsWithUniqueDays.Contains(d.month));
        Console.WriteLine(dates.Count + " remaining.");
        //The day must now be unique.
        dates.IntersectWith(dates.GroupBy(d => d.day).Where(g => g.Count() == 1).Select(g => g.First()));
        Console.WriteLine(dates.Count + " remaining.");
        //The month must now be unique.
        dates.IntersectWith(dates.GroupBy(d => d.month).Where(g => g.Count() == 1).Select(g => g.First()));
        Console.WriteLine(dates.Single());
    }

}
```

{{out}}

```txt

10 remaining.
5 remaining.
3 remaining.
(July, 16)

```



## Common Lisp


```lisp

;; Author: Amir Teymuri, Saturday 20.10.2018

(defparameter *possible-dates*
  '((15 . may) (16 . may) (19 . may)
    (17 . june) (18 . june)
    (14 . july) (16 . july)
    (14 . august) (15 . august) (17 . august)))

(defun unique-date-parts (possible-dates &key (alist-look-at #'car) (alist-r-assoc #'assoc))
  (let* ((date-parts (mapcar alist-look-at possible-dates))
	 (unique-date-parts (remove-if #'(lambda (part) (> (count part date-parts) 1)) date-parts)))
    (mapcar #'(lambda (part) (funcall alist-r-assoc part possible-dates))
    	    unique-date-parts)))

(defun person (person possible-dates)
  "Who's turn is it to think?"
  (case person
    ('albert (unique-date-parts possible-dates :alist-look-at #'cdr :alist-r-assoc #'rassoc))
    ('bernard (unique-date-parts possible-dates :alist-look-at #'car :alist-r-assoc #'assoc))))

(defun cheryls-birthday (possible-dates)
  (person 'albert
	  (person 'bernard
		  (set-difference
		   possible-dates
		   (person 'bernard possible-dates)
		   :key #'cdr))))

(cheryls-birthday *possible-dates*) ;; => ((16 . JULY))

```



## D


```d
import std.algorithm.iteration : filter, joiner, map;
import std.algorithm.searching : canFind;
import std.algorithm.sorting : sort;
import std.array : array;
import std.datetime.date : Date, Month;
import std.stdio : writeln;

void main() {
    auto choices = [
        // Month.jan
        Date(2019, Month.may, 15),
        Date(2019, Month.may, 16),
        Date(2019, Month.may, 19),  // unique day (1)

        Date(2019, Month.jun, 17),
        Date(2019, Month.jun, 18),  // unique day (1)

        Date(2019, Month.jul, 14),
        Date(2019, Month.jul, 16),  // final answer

        Date(2019, Month.aug, 14),
        Date(2019, Month.aug, 15),
        Date(2019, Month.aug, 17),
    ];

    // The month cannot have a unique day because Albert knows the month, and knows that Bernard does not know the answer
    auto uniqueMonths = choices.sort!"a.day < b.day".groupBy.filter!"a.array.length == 1".joiner.map!"a.month";
    // writeln(uniqueMonths.save);
    auto filter1 = choices.filter!(a => !canFind(uniqueMonths.save, a.month)).array;

    // Bernard now knows the answer, so the day must be unique within the remaining choices
    auto uniqueDays = filter1.sort!"a.day < b.day".groupBy.filter!"a.array.length == 1".joiner.map!"a.day";
    auto filter2 = filter1.filter!(a => canFind(uniqueDays.save, a.day)).array;

    // Albert knows the answer too, so the month must be unique within the remaining choices
    auto birthDay = filter2.sort!"a.month < b.month".groupBy.filter!"a.array.length == 1".joiner.front;

    // print the result
    writeln(birthDay.month, " ", birthDay.day);
}
```

{{out}}

```txt
jul 16
```


=={{header|F_Sharp|F#}}==

```fsharp

//Find Cheryl's Birthday. Nigel Galloway: October 23rd., 2018
type Month = |May |June |July |August
let fN n= n |> List.filter(fun (_,n)->(List.length n) < 2) |> List.unzip
let dates = [(May,15);(May,16);(May,19);(June,17);(June,18);(July,14);(July,16);(August,14);(August,15);(August,17)]
let _,n = dates |> List.groupBy snd |> fN
let   i = n |> List.concat |> List.map fst |> Set.ofList
let _,g = dates |> List.filter(fun (n,_)->not (Set.contains n i)) |> List.groupBy snd |> fN
let _,e = List.concat g |> List.groupBy fst |> fN
printfn "%A" e

```

{{out}}

```txt

[[(July, 16)]]

```



## Factor


```factor
USING: assocs calendar.english fry io kernel prettyprint
sequences sets.extras ;

: unique-by ( seq quot -- newseq )
    2dup map non-repeating '[ @ _ member? ] filter ; inline

ALIAS: day first
ALIAS: month second

{
    { 15 5 } { 16 5 } { 19 5 } { 17 6 } { 18 6 }
    { 14 7 } { 16 7 } { 14 8 } { 15 8 } { 17 8 }
}

! the month cannot have a unique day
dup [ day ] map non-repeating over extract-keys values
'[ month _ member? ] reject

! of the remaining dates, day must be unique
[ day ] unique-by

! of the remaining dates, month must be unique
[ month ] unique-by

! print a date that looks like { { 16 7 } }
first first2 month-name write bl .
```

{{out}}

```txt

July 16

```



## Go


```go
package main

import (
    "fmt"
    "time"
)

type birthday struct{ month, day int }

func (b birthday) String() string {
    return fmt.Sprintf("%s %d", time.Month(b.month), b.day)
}

func (b birthday) monthUniqueIn(bds []birthday) bool {
    count := 0
    for _, bd := range bds {
        if bd.month == b.month {
            count++
        }
    }
    if count == 1 {
        return true
    }
    return false
}

func (b birthday) dayUniqueIn(bds []birthday) bool {
    count := 0
    for _, bd := range bds {
        if bd.day == b.day {
            count++
        }
    }
    if count == 1 {
        return true
    }
    return false
}

func (b birthday) monthWithUniqueDayIn(bds []birthday) bool {
    for _, bd := range bds {
        if bd.month == b.month && bd.dayUniqueIn(bds) {
            return true
        }
    }
    return false
}

func main() {
    choices := []birthday{
        {5, 15}, {5, 16}, {5, 19}, {6, 17}, {6, 18},
        {7, 14}, {7, 16}, {8, 14}, {8, 15}, {8, 17},
    }

    // Albert knows the month but doesn't know the day.
    // So the month can't be unique within the choices.
    var filtered []birthday
    for _, bd := range choices {
        if !bd.monthUniqueIn(choices) {
            filtered = append(filtered, bd)
        }
    }

    // Albert also knows that Bernard doesn't know the answer.
    // So the month can't have a unique day.
    var filtered2 []birthday
    for _, bd := range filtered {
        if !bd.monthWithUniqueDayIn(filtered) {
            filtered2 = append(filtered2, bd)
        }
    }

    // Bernard now knows the answer.
    // So the day must be unique within the remaining choices.
    var filtered3 []birthday
    for _, bd := range filtered2 {
        if bd.dayUniqueIn(filtered2) {
            filtered3 = append(filtered3, bd)
        }
    }

    // Albert now knows the answer too.
    // So the month must be unique within the remaining choices.
    var filtered4 []birthday
    for _, bd := range filtered3 {
        if bd.monthUniqueIn(filtered3) {
            filtered4 = append(filtered4, bd)
        }
    }

    if len(filtered4) == 1 {
        fmt.Println("Cheryl's birthday is", filtered4[0])
    } else {
        fmt.Println("Something went wrong!")
    }
}
```


{{out}}

```txt

Cheryl's birthday is July 16

```



## Haskell


```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.List as L (filter, groupBy, head, length, sortBy)
import Data.Map.Strict as M (Map, fromList, keys, lookup)
import Data.Text as T (Text, splitOn, words)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Tuple (swap)

data DatePart
  = Month
  | Day

type M = Text

type D = Text

main :: IO ()
main =
  print $
  -- The month with only one remaining day,
  --
  -- (A's month contains only one remaining day)
  -- (3 :: A "Then I also know")
  uniquePairing Month $

  -- among the days with unique months,
  --
  -- (B's day is paired with only one remaining month)
  -- (2 :: B "I know now")
  uniquePairing Day $

  -- excluding months with unique days,
  --
  -- (A's month is not among those with unique days)
  -- (1 :: A "I know that Bernard does not know")
  monthsWithUniqueDays False $

  -- from the given month-day pairs:
  --
  -- (0 :: Cheryl's list)
  (\(x:y:_) -> (x, y)) . T.words <$>
  splitOn
    ", "
    "May 15, May 16, May 19, June 17, June 18, \
              \July 14, July 16, Aug 14, Aug 15, Aug 17"

-- QUERY FUNCTIONS ---------------------------------------------
monthsWithUniqueDays :: Bool -> [(M, D)] -> [(M, D)]
monthsWithUniqueDays bln xs =
  let months = fst <$> uniquePairing Day xs
  in L.filter
       (\(m, _) ->
           (if bln
              then id
              else not)
             (m `elem` months))
       xs

uniquePairing :: DatePart -> [(M, D)] -> [(M, D)]
uniquePairing dp xs =
  let f =
        case dp of
          Month -> fst
          _ -> snd
  in bindPairs
       xs
       (\md ->
           let dct :: M.Map Text [Text]
               dct = f md
               uniques =
                 L.filter
                   ((1 ==) . L.length . fromJust . flip M.lookup dct)
                   (keys dct)
           in L.filter ((`elem` uniques) . f) xs)

bindPairs :: [(M, D)]
          -> ((M.Map Text [Text], M.Map Text [Text]) -> [(M, D)])
          -> [(M, D)]
bindPairs xs f = f (mapFromPairs xs, mapFromPairs (swap <$> xs))

mapFromPairs :: [(M, D)] -> Map Text [Text]
mapFromPairs xs =
  M.fromList $
  ((,) . fst . L.head) <*> fmap snd <$>
  L.groupBy (on (==) fst) (L.sortBy (comparing fst) xs)
```

{{Out}}

```txt
[("July","16")]
```



## J

'''Solution:'''

```j
Dates=: <;._2 noun define
15 May
16 May
19 May
17 June
18 June
14 July
16 July
14 August
15 August
17 August
)

getDayMonth=: |:@:(' '&splitstring&>)                      NB. retrieve lists of days and months from dates
keep=: adverb def '] #~ u'                                 NB. apply mask to filter dates

monthsWithUniqueDay=: {./. #~ (1=#)/.                      NB. list months that have a unique day
isMonthWithoutUniqueDay=: (] -.@e. monthsWithUniqueDay)/@getDayMonth  NB. mask of dates with a month that doesn't have a unique day

uniqueDayInMonth=: ~.@[ #~ (1=#)/.                         NB. list of days that are unique to 1 month
isUniqueDayInMonth=: ([ e. uniqueDayInMonth)/@getDayMonth  NB. mask of dates with a day that is unique to 1 month

uniqueMonth=: ~.@] #~ (1=#)/.~                             NB. list of months with 1 unique day
isUniqueMonth=: (] e. uniqueMonth)/@getDayMonth            NB. mask of dates with a month that has 1 unique day
```

'''Usage:'''

```j
   isUniqueMonth keep isUniqueDayInMonth keep isMonthWithoutUniqueDay keep Dates
+-------+
|16 July|
+-------+
```



### Alternative Approach


The concepts here are the same, of course, it's just the presentation that's different.


```J
possible=: cut;._2 'May 15, May 16, May 19, June 17, June 18, July 14, July 16, August 14, August 15, August 17,'

Albert=: {."1 NB. Albert knows month
Bernard=: {:"1 NB. Bernard knows day

NB. Bernard's understanding of Albert's first pass
  days=: {:"1 possible
  invaliddays=: (1=#/.~ days)#~.days
  months=: {."1 possible
  validmonths=: months -. (days e. invaliddays)#months
  possibleA=. (months e. validmonths)# possible

NB. Albert's understanding of Bernard's pass
  days=: {:"1 possibleA
  invaliddays=: (1<#/.~ days)#~.days
  possibleB=. (days e. days-.invaliddays)# possibleA

NB. our understanding of Albert's second pass
  months=: {."1 possibleB
  invalidmonths=: (1<#/.~months)#~.months
  echo ;:inv (months e. months -. invalidmonths)#possibleB
```


This gives us the July 16 result we were expecting


## JavaScript


```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {
        const
            month = fst,
            day = snd;
        showLog(
            map(x => Array.from(x), (

                // The month with only one remaining day,

                // (A's month contains only one remaining day)
                // (3 :: A "Then I also know")
                uniquePairing(month)(

                    // among the days with unique months,

                    // (B's day is paired with only one remaining month)
                    // (2 :: B "I know now")
                    uniquePairing(day)(

                        // excluding months with unique days,

                        // (A's month is not among those with unique days)
                        // (1 :: A "I know that Bernard does not know")
                        monthsWithUniqueDays(false)(

                            // from the given month-day pairs:

                            // (0 :: Cheryl's list)
                            map(x => tupleFromList(words(strip(x))),
                                splitOn(/,\s+/,
                                    `May 15, May 16, May 19,
                                        June 17, June 18, July 14, July 16,
                                        Aug 14, Aug 15, Aug 17`
                                )
                            )
                        )
                    )
                )
            ))
        );
    };

    // monthsWithUniqueDays :: Bool -> [(Month, Day)] -> [(Month, Day)]
    const monthsWithUniqueDays = blnInclude => xs => {
        const months = map(fst, uniquePairing(snd)(xs));
        return filter(
            md => (blnInclude ? id : not)(
                elem(fst(md), months)
            ),
            xs
        );
    };

    // uniquePairing :: ((a, a) -> a) ->
    //      -> [(Month, Day)] -> [(Month, Day)]
    const uniquePairing = f => xs =>
        bindPairs(xs,
            md => {
                const
                    dct = f(md),
                    matches = filter(
                        k => 1 === length(dct[k]),
                        Object.keys(dct)
                    );
                return filter(tpl => elem(f(tpl), matches), xs);
            }
        );

    // bindPairs :: [(Month, Day)] -> (Dict, Dict) -> [(Month, Day)]
    const bindPairs = (xs, f) => f(
        Tuple(
            dictFromPairs(fst)(snd)(xs),
            dictFromPairs(snd)(fst)(xs)
        )
    );

    // dictFromPairs :: ((a, a) -> a) -> ((a, a) -> a) -> [(a, a)] -> Dict
    const dictFromPairs = f => g => xs =>
        foldl((a, tpl) => Object.assign(
            a, {
                [f(tpl)]: (a[f(tpl)] || []).concat(g(tpl).toString())
            }
        ), {}, xs);


    // GENERIC ABSTRACTIONS -------------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // elem :: Eq a => a -> [a] -> Bool
    const elem = (x, xs) => xs.includes(x);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // foldl :: (a -> b -> a) -> a -> [b] -> a
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // id :: a -> a
    const id = x => x;

    // intersect :: (Eq a) => [a] -> [a] -> [a]
    const intersect = (xs, ys) =>
        xs.filter(x => -1 !== ys.indexOf(x));

    // Returns Infinity over objects without finite length
    // this enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // not :: Bool -> Bool
    const not = b => !b;

    // showLog :: a -> IO ()
    const showLog = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // splitOn :: String -> String -> [String]
    const splitOn = (pat, src) =>
        src.split(pat);

    // strip :: String -> String
    const strip = s => s.trim();

    // tupleFromList :: [a] -> (a, a ...)
    const tupleFromList = xs =>
        TupleN.apply(null, xs);

    // TupleN :: a -> b ...  -> (a, b ... )
    function TupleN() {
        const
            args = Array.from(arguments),
            lng = args.length;
        return lng > 1 ? Object.assign(
            args.reduce((a, x, i) => Object.assign(a, {
                [i]: x
            }), {
                type: 'Tuple' + (2 < lng ? lng.toString() : ''),
                length: lng
            })
        ) : args[0];
    };

    // words :: String -> [String]
    const words = s => s.split(/\s+/);

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
[["July","16"]]
```




## Julia


```julia
const dates = [[15, "May"], [16, "May"], [19, "May"], [17, "June"], [18, "June"],
    [14, "July"], [16, "July"], [14, "August"], [15, "August"], [17, "August"]]

uniqueday(parr) = filter(x -> sum(y -> y[1] == x[1], parr) == 1, parr)

# At the start, they come to know that they have no unique day of month to identify.
f1 = filter(m -> !(m[2] in [d[2] for d in uniqueday(dates)]), dates)

# After eliminating all months with unique dates, they now come to know the answer has a unique date.
bday = uniqueday(f1)[1]
println("Cheryl's birthday is $(bday[2]) $(bday[1]).")


```
{{out}}

```txt

Cheryl's birthday is July 16.

```



## Kotlin

{{trans|Go}}

```scala
// Version 1.2.71

val months = listOf(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
)

class Birthday(val month: Int, val day: Int) {
    public override fun toString() = "${months[month - 1]} $day"

    public fun monthUniqueIn(bds: List<Birthday>): Boolean {
        return bds.count { this.month == it.month } == 1
    }

    public fun dayUniqueIn(bds: List<Birthday>): Boolean {
        return bds.count { this.day == it.day } == 1
    }

    public fun monthWithUniqueDayIn(bds: List<Birthday>): Boolean {
        return bds.any { (this.month == it.month) && it.dayUniqueIn(bds) }
    }
}

fun main(args: Array<String>) {
    val choices = listOf(
        Birthday(5, 15), Birthday(5, 16), Birthday(5, 19), Birthday(6, 17),
        Birthday(6, 18), Birthday(7, 14), Birthday(7, 16), Birthday(8, 14),
        Birthday(8, 15), Birthday(8, 17)
    )

    // Albert knows the month but doesn't know the day.
    // So the month can't be unique within the choices.
    var filtered = choices.filterNot { it.monthUniqueIn(choices) }

    // Albert also knows that Bernard doesn't know the answer.
    // So the month can't have a unique day.
    filtered = filtered.filterNot { it.monthWithUniqueDayIn(filtered) }

    // Bernard now knows the answer.
    // So the day must be unique within the remaining choices.
    filtered = filtered.filter { it.dayUniqueIn(filtered) }

    // Albert now knows the answer too.
    // So the month must be unique within the remaining choices.
    filtered = filtered.filter { it.monthUniqueIn(filtered) }

    if (filtered.size == 1)
        println("Cheryl's birthday is ${filtered[0]}")
    else
        println("Something went wrong!")
}
```


{{output}}

```txt

Cheryl's birthday is July 16

```



## Perl


```perl
sub filter {
    my($test,@dates) = @_;
    my(%M,%D,@filtered);

    # analysis of potential birthdays, keyed by month and by day
    for my $date (@dates) {
        my($mon,$day) = split '-', $date;
        $M{$mon}{cnt}++;
        $D{$day}{cnt}++;
        push @{$M{$mon}{day}},  $day;
        push @{$D{$day}{mon}},  $mon;
        push @{$M{$mon}{bday}}, "$mon-$day";
        push @{$D{$day}{bday}}, "$mon-$day";
    }

    # eliminates May/Jun dates based on 18th and 19th being singletons
    if ($test eq 'singleton') {
        my %skip;
        for my $day (grep { $D{$_}{cnt} == 1 } keys %D) { $skip{ @{$D{$day}{mon}}[0] }++    }
        for my $mon (grep { ! $skip{$_}      } keys %M) { push @filtered, @{$M{$mon}{bday}} }

    # eliminates Jul/Aug 14th because day count > 1 across months
    } elsif ($test eq 'duplicate') {
        for my $day (grep { $D{$_}{cnt} == 1 } keys %D) { push @filtered, @{$D{$day}{bday}} }

    # eliminates Aug 15th/17th because day count > 1, within month
    } elsif ($test eq 'multiple') {
        for my $day (grep { $M{$_}{cnt} == 1 } keys %M) { push @filtered, @{$M{$day}{bday}} }
    }
    return @filtered;
}

# doesn't matter what order singleton/duplicate tests are run, but 'multiple' must be last;
my @dates = qw<5-15 5-16 5-19 6-17 6-18 7-14 7-16 8-14 8-15 8-17>;
@dates = filter($_, @dates) for qw<singleton duplicate multiple>;

my @months = qw<_ January February March April May June July August September October November December>;

my ($m, $d) = split '-', $dates[0];
print "Cheryl's birthday is $months[$m] $d.\n";
```

{{out}}

```txt
Cheryl's birthday is July 16.
```



## Perl 6



```perl6
my @dates =
    { :15day, :5month },
    { :16day, :5month },
    { :19day, :5month },
    { :17day, :6month },
    { :18day, :6month },
    { :14day, :7month },
    { :16day, :7month },
    { :14day, :8month },
    { :15day, :8month },
    { :17day, :8month }
;

# Month can't have a unique day
my @filtered = @dates.grep(*.<month> != one(@dates.grep(*.<day> == one(@dates».<day>))».<month>));

# Day must be unique and unambiguous in remaining months
my $birthday = @filtered.grep(*.<day> == one(@filtered».<day>)).classify({.<month>})\
    .first(*.value.elems == 1).value[0];

# convenience array
my @months = <'' January February March April May June July August September October November December>;

say "Cheryl's birthday is { @months[$birthday<month>] } {$birthday<day>}.";
```

{{out}}

```txt
Cheryl's birthday is July 16.
```



## Phix


```Phix
sequence choices = {{5, 15}, {5, 16}, {5, 19}, {6, 17}, {6, 18},
                    {7, 14}, {7, 16}, {8, 14}, {8, 15}, {8, 17}}

sequence mwud = repeat(false,12)    -- months with unique days

for step=1 to 4 do
    sequence {months,days} = columnize(choices)
    bool impossible = false
    for i=length(choices) to 1 by -1 do
        integer {m,d} = choices[i]
        switch step do
            case 1:   mwud[m] += (sum(sq_eq(days,d))=1)
            case 2: impossible = mwud[m]
            case 3: impossible = (sum(sq_eq(days,d))!=1)
            case 4: impossible = (sum(sq_eq(months,m))!=1)
        end switch
        if impossible then
            choices[i..i] = {}
        end if
    end for
end for
?choices
```

Strictly speaking we only need to columnize(choices) on steps 1 and 3.

Iterating backwards down the choices array simplifies element removal.

First case [1&2, months with unique days] must "or" before removing items.

Case 3 is days with unique months, case 4 is unique months.
{{out}}

```txt

{{7,16}}

```



## Python


### Functional

{{Works with|Python|3}}

```python
'''Cheryl's Birthday'''

from itertools import (groupby)
from operator import not_
from re import (split)


# main :: IO ()
def main():
    '''Derivation of the date.'''

    (month, day) = (0, 1)
    print(
        # (3 :: A "Then I also know")
        # (A's month contains only one remaining day)
        uniquePairing(month)(
            # (2 :: B "I know now")
            # (B's day is paired with only one remaining month)
            uniquePairing(day)(
                # (1 :: A "I know that Bernard does not know")
                # (A's month is not among those with unique days)
                monthsWithUniqueDays(False)([
                    # 0 :: Cheryl's list:
                    tuple(x.split()) for x in
                    split(
                        ', ',
                        'May 15, May 16, May 19, ' +
                        'June 17, June 18, ' +
                        'July 14, July 16, ' +
                        'Aug 14, Aug 15, Aug 17'
                    )
                ])
            )
        )
    )


#  QUERY FUNCTIONS ----------------------------------------

# monthsWithUniqueDays :: Bool -> [(Month, Day)] -> [(Month, Day)]
def monthsWithUniqueDays(blnInclude):
    '''The subset of months with (or without) unique days.'''
    def go(xs):
        (month, day) = (0, 1)
        months = list(map(fst, uniquePairing(day)(xs)))
        return list(filter(
            lambda md: (
                md if blnInclude else not_
            )(md[month] in months),
            xs
        ))
    return lambda xs: go(xs)


# uniquePairing :: DatePart -> [(Month, Day)] -> [(Month, Day)]
def uniquePairing(i):
    '''Subset of months (or days) with a unique intersection.'''
    def go(xs):
        def inner(md):
            dct = md[i]
            uniques = list(filter(
                lambda k: 1 == len(dct[k]),
                dct.keys()
            ))
            return filter(lambda tpl: tpl[i] in uniques, xs)
        return inner
    return lambda xs: bindPairs(xs)(go(xs))


# bindPairs :: [(Month, Day)] -> ((Dict String [String], Dict String [String])
#                             -> [(Month, Day)]) -> [(Month, Day)]
def bindPairs(xs):
    '''List monad injection operator for lists of (Month, Day) pairs.'''
    return lambda f: list(f(
        (dictFromPairs(xs), dictFromPairs(map(swap, xs)))
    ))


# dictFromPairs :: [(Month, Day)] -> Dict Text [Text]
def dictFromPairs(xs):
    '''A dictionary derived from a list of
       month day pairs.'''
    return dict(
        (k, list(map(snd, m))) for k, m in groupby(
            sorted(xs, key=fst), key=fst
        )
    )


# GENERIC -------------------------------------------------

# fst :: (a, b) -> a
def fst(tpl):
    '''First component of a pair.'''
    return tpl[0]


# snd :: (a, b) -> b
def snd(tpl):
    '''Second component of a pair.'''
    return tpl[1]


# swap :: (a, b) -> (b, a)
def swap(tpl):
    "Swap the components of a pair."
    return (tpl[1], tpl[0])


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
[('July', '16')]
```



## Racket

{{trans|Kotlin}}

```racket
#lang racket

(define ((is x #:key [key identity]) y) (equal? (key x) (key y)))

(define albert first)
(define bernard second)

(define (((unique who) chs) date) (= 1 (count (is date #:key who) chs)))

(define (((unique-fix who-fix who) chs) date)
  (ormap (conjoin (is date #:key who-fix) ((unique who) chs)) chs))

(define-syntax-rule (solve <chs> [<act> <arg>] ...)
  (let* ([chs <chs>] [chs (<act> (<arg> chs) chs)] ...) chs))

(solve '((May 15) (May 16) (May 19) (June 17) (June 18)
         (July 14) (July 16) (August 14) (August 15) (August 17))

       ;; Albert knows the month but doesn't know the day.
       ;; So the month can't be unique within the choices.
       [filter-not (unique albert)]
       ;; Albert also knows that Bernard doesn't know the answer.
       ;; So the month can't have a unique day.
       [filter-not (unique-fix albert bernard)]
       ;; Bernard now knows the answer.
       ;; So the day must be unique within the remaining choices.
       [filter (unique bernard)]
       ;; Albert now knows the answer too.
       ;; So the month must be unique within the remaining choices
       [filter (unique albert)])
```


{{out}}

```txt

'((July 16))

```



## Scala

==={{trans|D}}===

```scala
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, Month}

object Cheryl {
  def main(args: Array[String]): Unit = {
    val choices = List(
      LocalDate.of(2019, Month.MAY, 15),
      LocalDate.of(2019, Month.MAY, 16),
      LocalDate.of(2019, Month.MAY, 19),

      LocalDate.of(2019, Month.JUNE, 17),
      LocalDate.of(2019, Month.JUNE, 18),

      LocalDate.of(2019, Month.JULY, 14),
      LocalDate.of(2019, Month.JULY, 16),

      LocalDate.of(2019, Month.AUGUST, 14),
      LocalDate.of(2019, Month.AUGUST, 15),
      LocalDate.of(2019, Month.AUGUST, 17)
    )

    // The month cannot have a unique day because Albert knows the month, and knows that Bernard does not know the answer
    val uniqueMonths = choices.groupBy(_.getDayOfMonth)
      .filter(a => a._2.length == 1)
      .flatMap(a => a._2)
      .map(a => a.getMonth)
    val filter1 = choices.filterNot(a => uniqueMonths.exists(b => a.getMonth == b))

    // Bernard now knows the answer, so the day must be unique within the remaining choices
    val uniqueDays = filter1.groupBy(_.getDayOfMonth)
      .filter(a => a._2.length == 1)
      .flatMap(a => a._2)
      .map(a => a.getDayOfMonth)
    val filter2 = filter1.filter(a => uniqueDays.exists(b => a.getDayOfMonth == b))

    // Albert knows the answer too, so the month must be unique within the remaining choices
    val birthDay = filter2.groupBy(_.getMonth)
      .filter(a => a._2.length == 1)
      .flatMap(a => a._2)
      .head

    // print the result
    printf(birthDay.format(DateTimeFormatter.ofPattern("MMMM dd")))
  }
}
```

{{out}}

```txt
July 16
```

===Scala-ish approach===
{{Out}}See it yourself by running in your browser either by [https://scalafiddle.io/sf/AiS6u7B/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/hoBvwq5fSkSRk0vnr6NKmA Scastie (remote JVM)].
{{libheader|Scala Math Puzzle}}
{{libheader|Scala Scala-ish}}
{{libheader|Scala Idiomatic}}
{{libheader|Scala Type parameters}}
{{libheader|ScalaFiddle qualified}}
{{libheader|Scastie qualified}}
{{works with|Scala|2.13}}

```Scala
object Cheryl_sBirthday extends App {

  private val possiblerDates = Set(
    Date("May", 15), Date("May", 16), Date("May", 19),
    Date("June", 17), Date("June", 18),
    Date("July", 14), Date("July", 16),
    Date("August", 14), Date("August", 15), Date("August", 17)
  )

  private def clou3: Date = {
    // Find the dates with ONE unique once and only occurrence of the day of the month.
    def onceDates[K](toBeExcluded: Set[Date], selector: Date => K): Seq[Date] =
      toBeExcluded.groupBy(selector).filter { case (_, multiSet) => multiSet.size == 1 }.values.flatten.toSeq

    // 1) Albert tells us that Bernard doesn't know the answer,
    // so we know the answer must be in months that does NOT have a same day of month.
    val uniqueMonths = onceDates(possiblerDates, (date: Date) => date.dayOfMonth).map(_.month)
    // Remove the dates with those months. The dates remain which has NOT those months.
    val clou1 = possiblerDates.filterNot(p => uniqueMonths.contains(p.month))
    // 2) Since Bernard now knows the answer, that tells us that the day MUST be unique among the remaining birthdays.
    val uniqueDays = onceDates(clou1, (date: Date) => date.dayOfMonth).map(_.dayOfMonth)

    // 3) Since Albert now knows the answer, that tells us the answer has to be unique by month.
    // First, as the first parameter, intersect clou1 (Albert) with uniqueDays (Bernard)
    onceDates(clou1.filter(date => uniqueDays.contains(date.dayOfMonth)), (date: Date) => date.month).head
  }

  case class Date(month: String, dayOfMonth: Int) {
    override def toString: String = s"${"🎂 " * 3}$dayOfMonth $month${" 🎂" * 3}"
  }

  println(clou3)
}
```



## Sidef

{{trans|Perl 6}}

```ruby
struct Date(day, month)

var dates = [
    Date(15, "May"),
    Date(16, "May"),
    Date(19, "May"),
    Date(17, "June"),
    Date(18, "June"),
    Date(14, "July"),
    Date(16, "July"),
    Date(14, "August"),
    Date(15, "August"),
    Date(17, "August")
]

var filtered = dates.grep {
    dates.grep {
        dates.map{ .day }.count(.day) == 1
    }.map{ .month }.count(.month) != 1
}

var birthday = filtered.grep {
    filtered.map{ .day }.count(.day) == 1
}.group_by{ .month }.values.first_by { .len == 1 }[0]

say "Cheryl's birthday is #{birthday.month} #{birthday.day}."
```

{{out}}

```txt

Cheryl's birthday is July 16.

```



## VBA


```vb
Private Sub exclude_unique_days(w As Collection)
    Dim number_of_dates(31) As Integer
    Dim months_to_exclude As New Collection
    For Each v In w
        number_of_dates(v(1)) = number_of_dates(v(1)) + 1
    Next v
    For i = w.Count To 1 Step -1
        If number_of_dates(w(i)(1)) = 1 Then
            months_to_exclude.Add w(i)(0)
            w.Remove i
        End If
    Next i
    For Each m In months_to_exclude
        exclude_month w, m
    Next m
End Sub
Private Sub exclude_month(x As Collection, v As Variant)
    For i = x.Count To 1 Step -1
        If x(i)(0) = v Then x.Remove i
    Next i
End Sub
Private Sub exclude_non_unique_days(w As Collection)
    Dim number_of_dates(31) As Integer
    For Each v In w
        number_of_dates(v(1)) = number_of_dates(v(1)) + 1
    Next v
    For i = w.Count To 1 Step -1
        If number_of_dates(w(i)(1)) > 1 Then
            w.Remove i
        End If
    Next i
End Sub
Private Sub exclude_non_unique_months(w As Collection)
    Dim months As New Collection
    For Each v In w
        On Error GoTo 1
        months.Add v(0), v(0)
    Next v
1:
    For i = w.Count To 1 Step -1
        If w(i)(0) = v(0) Then
            w.Remove i
        End If
    Next i
End Sub
Public Sub cherylsbirthday()
    Dim v As New Collection
    s = "May 15, May 16, May 19, June 17, June 18, July 14, July 16, August 14, August 15, August 17"
    t = Split(s, ",")
    For Each u In t
        v.Add Split(Trim(u), " ")
    Next u
    '1) Albert: I don't know when Cheryl's birthday is, but I know that Bernard does not know too.
    exclude_unique_days v
    '2) Bernard: At first I don't know when Cheryl's birthday is, but I know now.
    exclude_non_unique_days v
    '3) Albert: Then I also know when Cheryl's birthday is.
    exclude_non_unique_months v
    Debug.Print v(1)(0); " "; v(1)(1)
End Sub
```
{{out}}

```txt
July 16
```


## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Structure MonDay
        Dim month As String
        Dim day As Integer

        Sub New(m As String, d As Integer)
            month = m
            day = d
        End Sub

        Public Overrides Function ToString() As String
            Return String.Format("({0}, {1})", month, day)
        End Function
    End Structure

    Sub Main()
        Dim dates = New HashSet(Of MonDay) From {
            New MonDay("May", 15),
            New MonDay("May", 16),
            New MonDay("May", 19),
            New MonDay("June", 17),
            New MonDay("June", 18),
            New MonDay("July", 14),
            New MonDay("July", 16),
            New MonDay("August", 14),
            New MonDay("August", 15),
            New MonDay("August", 17)
        }
        Console.WriteLine("{0} remaining.", dates.Count)

        ' The month cannot have a unique day.
        Dim monthsWithUniqueDays = dates.GroupBy(Function(d) d.day).Where(Function(g) g.Count() = 1).Select(Function(g) g.First().month).ToHashSet()
        dates.RemoveWhere(Function(d) monthsWithUniqueDays.Contains(d.month))
        Console.WriteLine("{0} remaining.", dates.Count)

        ' The day must now be unique.
        dates.IntersectWith(dates.GroupBy(Function(d) d.day).Where(Function(g) g.Count() = 1).Select(Function(g) g.First()))
        Console.WriteLine("{0} remaining.", dates.Count)

        ' The month must now be unique.
        dates.IntersectWith(dates.GroupBy(Function(d) d.month).Where(Function(g) g.Count() = 1).Select(Function(g) g.First()))
        Console.WriteLine(dates.Single())
    End Sub

End Module
```

{{out}}

```txt
10 remaining.
5 remaining.
3 remaining.
(July, 16)
```



## zkl


```zkl
dates:=T(T("May",   15), T("May",   16), T("May",   19),
         T("June",  17), T("June",  18),
	 T("July",  14), T("July",  16),
	 T("August",14), T("August",15), T("August",17) );
mDs:=dates.pump(Dictionary().appendKV); // "June":(15,16,19), ...
dMs:=dates.pump(Dictionary().appendKV,"reverse"); // 15:"May", 16:"May", 19:"May", ...

// remove unique days (18,19) --> "July":(14,16),"August":(14,15,17)
dMs.values.apply2('wrap(ms){ if(ms.len()==1) mDs.del(ms[0]) });

// find intersection of above days --> (14)
fcn intersection(l1,l2){ l1.pump(List,l2.holds,'==(True),Void.Filter) }
badDs:=mDs.values.reduce(intersection);

// --> July:(16),August:(15,17) --> ( ("July",(16)) )
theDay:=mDs.filter('wrap([(m,ds)]){ ds.removeEach(badDs).len()==1 });

// print birthday such that muliples are shown, if any
println("Cheryl's birthday is ",theDay.flatten().flatten().concat(" "));
```

{{out}}

```txt

Cheryl's birthday is July 16

```

