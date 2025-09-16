+++
title = "Order disjoint list items"
description = ""
date = 2019-06-10T05:31:17Z
aliases = []
[extra]
id = 17599
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "aime",
  "applescript",
  "autohotkey",
  "bracmat",
  "common_lisp",
  "cpp",
  "d",
  "echolisp",
  "elixir",
  "factor",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "swift",
  "tcl",
  "zkl",
]
+++

Given   <code>M</code>   as a list of items and another list   <code>N</code>   of items chosen from   <code>M</code>,   create   <code>M'</code>   as a list with the ''first'' occurrences of items from   N   sorted to be in one of the set of indices of their original occurrence in   <code>M</code>   but in the order given by their order in    <code>N</code>. 

That is, items in   <code>N</code>   are taken from   <code>M</code>   without replacement, then the corresponding positions in   <code>M'</code>   are filled by successive items from   <code>N</code>.


;For example:
:if   <code>M</code>   is   <code>'the cat sat on the mat'</code>
:And   <code>N</code>   is   <code>'mat cat'</code> 
:Then the result   <code>M'</code>   is   <code>'the mat sat on the cat'</code>. 

The words not in   <code>N</code>   are left in their original positions.


If there are duplications then only the first instances in   <code>M</code>   up to as many as are mentioned in   <code>N</code>   are potentially re-ordered.


;For example:
: <code> M = 'A B C A B C A B C' </code>
: <code> N = 'C A C A'           </code>

Is ordered as:
:<code> M' = 'C B A C B A A B C' </code>



Show the output, here, for at least the following inputs:

```txt

Data M: 'the cat sat on the mat' Order N: 'mat cat'
Data M: 'the cat sat on the mat' Order N: 'cat mat'
Data M: 'A B C A B C A B C'      Order N: 'C A C A'
Data M: 'A B C A B D A B E'      Order N: 'E A D A'
Data M: 'A B'                    Order N: 'B'      
Data M: 'A B'                    Order N: 'B A'    
Data M: 'A B B A'                Order N: 'B A'

```



;Cf:
* [[Sort disjoint sublist]]





## Aime


```aime
order(list a, b)
{
    integer j;
    record r;
    text s;

    a.ucall(o_, 0, " ");

    o_("| ");

    for (, s in b) {
        r[s] += 1;
        o_(s, " ");
    }

    o_("->");

    j = -1;
    for (, s in a) {
        if ((r[s] -= 1) < 0) {
            o_(" ", s);
        } else {
            o_(" ", b[j += 1]);
        }
    }

    o_newline();
}

main(void)
{
    order(list("the", "cat", "sat", "on", "the", "mat"), list("mat", "cat"));
    order(list("the", "cat", "sat", "on", "the", "mat"), list("cat", "mat"));
    order(list("A", "B", "C", "A", "B", "C", "A", "B", "C"), list("C", "A", "C", "A"));
    order(list("A", "B", "C", "A", "B", "D", "A", "B", "E"), list("E", "A", "D", "A"));
    order(list("A", "B"), list("B"));
    order(list("A", "B"), list("B", "A"));
    order(list("A", "B", "B", "A"), list("B", "A"));

    0;
}
```

```txt
the cat sat on the mat | mat cat -> the mat sat on the cat
the cat sat on the mat | cat mat -> the cat sat on the mat
A B C A B C A B C | C A C A -> C B A C B A A B C
A B C A B D A B E | E A D A -> E B C A B D A B A
A B | B -> A B
A B | B A -> B A
A B B A | B A -> B A B A
```



## AppleScript

Accumulate a segmentation of M over a fold/reduce, and zip with N:

```AppleScript
-- DISJOINT ORDER ------------------------------------------------------------

-- disjointOrder :: String -> String -> String
on disjointOrder(m, n)
    set {ms, ns} to map(my |words|, {m, n})
    
    unwords(flatten(zip(segments(ms, ns), ns & "")))
end disjointOrder

-- segments :: [String] -> [String] -> [String]
on segments(ms, ns)
    script segmentation
        on |λ|(a, x)
            set wds to |words| of a
            
            if wds contains x then
                {parts:(parts of a) & ¬
                    [current of a], current:[], |words|:deleteFirst(x, wds)} ¬
                    
            else
                {parts:(parts of a), current:(current of a) & x, |words|:wds}
            end if
        end |λ|
    end script
    
    tell foldl(segmentation, {|words|:ns, parts:[], current:[]}, ms)
        (parts of it) & [current of it]
    end tell
end segments


-- TEST ----------------------------------------------------------------------
on run
    script order
        on |λ|(rec)
            tell rec
                [its m, its n, my disjointOrder(its m, its n)]
            end tell
        end |λ|
    end script
    
    arrowTable(map(order, [¬
        {m:"the cat sat on the mat", n:"mat cat"}, ¬
        {m:"the cat sat on the mat", n:"cat mat"}, ¬
        {m:"A B C A B C A B C", n:"C A C A"}, ¬
        {m:"A B C A B D A B E", n:"E A D A"}, ¬
        {m:"A B", n:"B"}, {m:"A B", n:"B A"}, ¬
        {m:"A B B A", n:"B A"}]))
    
    -- the cat sat on the mat  ->  mat cat  ->  the mat sat on the cat 
    -- the cat sat on the mat  ->  cat mat  ->  the cat sat on the mat 
    -- A B C A B C A B C       ->  C A C A  ->  C B A C B A A B C      
    -- A B C A B D A B E       ->  E A D A  ->  E B C A B D A B A      
    -- A B                     ->  B        ->  A B                    
    -- A B                     ->  B A      ->  B A                    
    -- A B B A                 ->  B A      ->  B A B A                
    
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- Formatting test results

-- arrowTable :: [[String]] -> String
on arrowTable(rows)
    
    script leftAligned
        script width
            on |λ|(a, b)
                (length of a) - (length of b)
            end |λ|
        end script
        
        on |λ|(col)
            set widest to length of maximumBy(width, col)
            
            script padding
                on |λ|(s)
                    justifyLeft(widest, space, s)
                end |λ|
            end script
            
            map(padding, col)
        end |λ|
    end script
    
    script arrows
        on |λ|(row)
            intercalate("  ->  ", row)
        end |λ|
    end script
    
    intercalate(linefeed, ¬
        map(arrows, ¬
            transpose(map(leftAligned, transpose(rows)))))
end arrowTable

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    script append
        on |λ|(a, b)
            a & b
        end |λ|
    end script
    
    foldl(append, {}, map(f, xs))
end concatMap

-- deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
on deleteBy(fnEq, x, xs)
    if length of xs > 0 then
        set {h, t} to uncons(xs)
        if |λ|(x, h) of mReturn(fnEq) then
            t
        else
            {h} & deleteBy(fnEq, x, t)
        end if
    else
        {}
    end if
end deleteBy

-- deleteFirst :: a -> [a] -> [a]
on deleteFirst(x, xs)
    script Eq
        on |λ|(a, b)
            a = b
        end |λ|
    end script
    
    deleteBy(Eq, x, xs)
end deleteFirst

-- flatten :: Tree a -> [a]
on flatten(t)
    if class of t is list then
        concatMap(my flatten, t)
    else
        t
    end if
end flatten

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- justifyLeft :: Int -> Char -> Text -> Text
on justifyLeft(n, cFiller, strText)
    if n > length of strText then
        text 1 thru n of (strText & replicate(n, cFiller))
    else
        strText
    end if
end justifyLeft

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

-- maximumBy :: (a -> a -> Ordering) -> [a] -> a 
on maximumBy(f, xs)
    set cmp to mReturn(f)
    script max
        on |λ|(a, b)
            if a is missing value or cmp's |λ|(a, b) < 0 then
                b
            else
                a
            end if
        end |λ|
    end script
    
    foldl(max, missing value, xs)
end maximumBy

-- minimum :: [a] -> a 
on minimum(xs)
    script min
        on |λ|(a, x)
            if x < a or a is missing value then
                x
            else
                a
            end if
        end |λ|
    end script
    
    foldl(min, missing value, xs)
end minimum

-- Lift 2nd class handler function into 1st class script wrapper 
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary 
-- assembly of a target length

-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}
    
    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- transpose :: [[a]] -> [[a]]
on transpose(xss)
    script column
        on |λ|(_, iCol)
            script row
                on |λ|(xs)
                    item iCol of xs
                end |λ|
            end script
            
            map(row, xss)
        end |λ|
    end script
    
    map(column, item 1 of xss)
end transpose

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    if length of xs > 0 then
        {item 1 of xs, rest of xs}
    else
        missing value
    end if
end uncons

-- unwords :: [String] -> String
on unwords(xs)
    intercalate(space, xs)
end unwords


-- words :: String -> [String]
on |words|(s)
    words of s
end |words|

-- zip :: [a] -> [b] -> [(a, b)]
on zip(xs, ys)
    script pair
        on |λ|(x, i)
            [x, item i of ys]
        end |λ|
    end script
    
    map(pair, items 1 thru minimum([length of xs, length of ys]) of xs)
end zip
```

```txt
the cat sat on the mat  ->  mat cat  ->  the mat sat on the cat 
the cat sat on the mat  ->  cat mat  ->  the cat sat on the mat 
A B C A B C A B C       ->  C A C A  ->  C B A C B A A B C      
A B C A B D A B E       ->  E A D A  ->  E B C A B D A B A      
A B                     ->  B        ->  A B                    
A B                     ->  B A      ->  B A                    
A B B A                 ->  B A      ->  B A B A                
```



## AutoHotkey

```AutoHotkey
Data := [ {M: "the cat sat on the mat", N: "mat cat"}
	, {M: "the cat sat on the mat", N: "cat mat"}
	, {M: "A B C A B C A B C", N: "C A C A"}
	, {M: "A B C A B D A B E", N: "E A D A"}
	, {M: "A B", N: "B"}
	, {M: "A B", N: "B A"}
	, {M: "A B B A", N: "B A"} ]

for Key, Val in Data
	Output .= Val.M " :: " Val.N " -> " OrderDisjointList(Val.M, Val.N) "`n"
MsgBox, % RTrim(Output, "`n")

OrderDisjointList(M, N) {
	ItemsN := []
	Loop, Parse, N, % A_Space
		ItemsN[A_LoopField] := ItemsN[A_LoopField] ? ItemsN[A_LoopField] + 1 : 1
	N := StrSplit(N, A_Space)
	Loop, Parse, M, % A_Space
		Result .= (ItemsN[A_LoopField]-- > 0 ? N.Remove(1) : A_LoopField) " "
	return RTrim(Result)
}
```

```txt
the cat sat on the mat :: mat cat -> the mat sat on the cat
the cat sat on the mat :: cat mat -> the cat sat on the mat
A B C A B C A B C :: C A C A -> C B A C B A A B C
A B C A B D A B E :: E A D A -> E B C A B D A B A
A B :: B -> A B
A B :: B A -> B A
A B B A :: B A -> B A B A
```



## Bracmat


```bracmat
( ( odli
  =   M N NN item A Z R
    .   !arg:(?M.?N)
      & :?NN
      &   whl
        ' ( !N:%?item ?N
          & (   !M:?A !item ?Z
              & !A (.) !Z:?M
              & !NN !item:?NN
            |
            )
          )
      & :?R
      &   whl
        ' ( !M:?A (.) ?M
          & !NN:%?item ?NN
          & !R !A !item:?R
          )
      & !R !M
  )
&     (the cat sat on the mat.mat cat)
      (the cat sat on the mat.cat mat)
      (A B C A B C A B C.C A C A)
      (A B C A B D A B E.E A D A)
      (A B.B)
      (A B.B A)
      (A B B A.B A)
  : ?tests
&   whl
  ' ( !tests:(?M.?N) ?tests
    & put$("Data M:" !M)
    & put$("\tOrder N:" !N)
    & out$(\t odli$(!M.!N))
    )
);
```

Output:

```txt
Data M: the cat sat on the mat  Order N: mat cat         the mat sat on the cat
Data M: the cat sat on the mat  Order N: cat mat         the cat sat on the mat
Data M: A B C A B C A B C       Order N: C A C A         C B A C B A A B C
Data M: A B C A B D A B E       Order N: E A D A         E B C A B D A B A
Data M: A B     Order N: B       A B
Data M: A B     Order N: B A     B A
Data M: A B B A Order N: B A     B A B A
```



## C++


```cpp

#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

template <typename T>
void print(const std::vector<T> v) {
  std::cout << "{ ";
  for (const auto& e : v) {
    std::cout << e << " ";
  }
  std::cout << "}";
}

template <typename T>
auto orderDisjointArrayItems(std::vector<T> M, std::vector<T> N) {
  std::vector<T*> M_p(std::size(M));
  for (auto i = 0; i < std::size(M_p); ++i) {
    M_p[i] = &M[i];
  }
  for (auto e : N) {
    auto i = std::find_if(std::begin(M_p), std::end(M_p), [e](auto c) -> bool {
      if (c != nullptr) {
        if (*c == e) return true;
      }
      return false;
    });
    if (i != std::end(M_p)) {
      *i = nullptr;
    }
  }
  for (auto i = 0; i < std::size(N); ++i) {
    auto j = std::find_if(std::begin(M_p), std::end(M_p), [](auto c) -> bool {
      return c == nullptr;
    });
    if (j != std::end(M_p)) {
      *j = &M[std::distance(std::begin(M_p), j)];
      **j = N[i];
    }
  }
  return M;
}

int main() {
  std::vector<std::vector<std::vector<std::string>>> l = {
    { { "the", "cat", "sat", "on", "the", "mat" }, { "mat", "cat" } },
    { { "the", "cat", "sat", "on", "the", "mat" },{ "cat", "mat" } },
    { { "A", "B", "C", "A", "B", "C", "A", "B", "C" },{ "C", "A", "C", "A" } },
    { { "A", "B", "C", "A", "B", "D", "A", "B", "E" },{ "E", "A", "D", "A" } },
    { { "A", "B" },{ "B" } },
    { { "A", "B" },{ "B", "A" } },
    { { "A", "B", "B", "A" },{ "B", "A" } }
  };
  for (const auto& e : l) {
    std::cout << "M: ";
    print(e[0]);
    std::cout << ", N: ";
    print(e[1]);
    std::cout << ", M': ";
    auto res = orderDisjointArrayItems<std::string>(e[0], e[1]);
    print(res);
    std::cout << std::endl;
  }
  std::cin.ignore();
  std::cin.get();
  return 0;
}
```


```txt
M: { the cat sat on the mat }, N: { mat cat }, M': { the mat sat on the cat }
M: { the cat sat on the mat }, N: { cat mat }, M': { the cat sat on the mat }
M: { A B C A B C A B C }, N: { C A C A }, M': { C B A C B A A B C }
M: { A B C A B D A B E }, N: { E A D A }, M': { E B C A B D A B A }
M: { A B }, N: { B }, M': { A B }
M: { A B }, N: { B A }, M': { B A }
M: { A B B A }, N: { B A }, M': { B A B A }
```



## Common Lisp


```lisp
(defun order-disjoint (data order)
  (let ((order-b (make-hash-table :test 'equal)))
    (loop :for n :in order :do (incf (gethash n order-b 0)))
    (loop :for m :in data :collect
       (cond ((< 0 (gethash m order-b 0))
              (decf (gethash m order-b))
              (pop order))
             (t m)))))
```

```txt
CL-USER> (order-disjoint '(the cat sat on the mat) '(mat cat))
(THE MAT SAT ON THE CAT)
CL-USER> (order-disjoint '(the cat sat on the mat) '(cat mat))
(THE CAT SAT ON THE MAT)
CL-USER> (order-disjoint '(a b c a b c a b c) '(c a c a))
(C B A C B A A B C)
CL-USER> (order-disjoint '(a b c a b d a b e) '(e a d a))
(E B C A B D A B A)
CL-USER> (order-disjoint '(a b) '(b))
(A B)
CL-USER> (order-disjoint '(a b) '(b a))
(B A)
CL-USER> (order-disjoint '(a b b a) '(b a))
(B A B A)
```



## D

This version is not efficient.

```d
import std.stdio, std.string, std.algorithm, std.array, std.range,
       std.conv;

T[] orderDisjointArrayItems(T)(in T[] data, in T[] items)
pure /*nothrow*/ @safe {
    int[] itemIndices;
    foreach (item; items.dup.sort().uniq) {
        immutable int itemCount = items.count(item);
        assert(data.count(item) >= itemCount,
               text("More of ", item, " than in data"));
        auto lastIndex = [-1];
        foreach (immutable _; 0 .. itemCount) {
            immutable start = lastIndex.back + 1;
            lastIndex ~= data[start .. $].countUntil(item) + start;
        }
        itemIndices ~= lastIndex.dropOne;
    }

    itemIndices.sort();
    auto result = data.dup;
    foreach (index, item; zip(itemIndices, items))
        result[index] = item;
    return result;
}

void main() {
    immutable problems =
   "the cat sat on the mat  | mat cat
    the cat sat on the mat  | cat mat
    A B C A B C A B C       | C A C A
    A B C A B D A B E       | E A D A
    A B                     | B
    A B                     | B A
    A B B A                 | B A
                            |
    A                       | A
    A B                     |
    A B B A                 | A B
    A B A B                 | A B
    A B A B                 | B A B A
    A B C C B A             | A C A C
    A B C C B A             | C A C A"
    .splitLines.map!(r => r.split("|")).array;

    foreach (immutable p; problems) {
        immutable a = p[0].split;
        immutable b = p[1].split;
        writefln("%s | %s -> %-(%s %)", p[0].strip, p[1].strip,
                 orderDisjointArrayItems(a, b));
    }
}
```

```txt
the cat sat on the mat | mat cat -> the mat sat on the cat
the cat sat on the mat | cat mat -> the cat sat on the mat
A B C A B C A B C | C A C A -> C B A C B A A B C
A B C A B D A B E | E A D A -> E B C A B D A B A
A B | B -> A B
A B | B A -> B A
A B B A | B A -> B A B A
 |  -> 
A | A -> A
A B |  -> A B
A B B A | A B -> A B B A
A B A B | A B -> A B A B
A B A B | B A B A -> B A B A
A B C C B A | A C A C -> A B C A B C
A B C C B A | C A C A -> C B A C B A
```



## EchoLisp


```scheme

(lib 'list) ;; for list-delete

(define dataM
'((the cat sat on the mat)
(the cat sat on the mat)
(A B C A B C A B C)
(A B C A B D A B E)      
(A B)                    
(A B)                    
(A B B A)))               
		
(define orderM
'((mat cat)
(cat mat)
(C A C A)
(E A D A)
(B)      
(B A)    
(B A)))

(define (order-disjoint M N)
(define R (append N null)) ;; tmp copy of N : delete w when used
	(for/list [(w M)]
	(if
		(not (member w R)) w ;; output as is
		(begin0
		(first N) ;; replacer
		(set! N (rest N))
		(set! R (list-delete R w))))))
		


```

```txt

(for [(m dataM) (n orderM)]
(writeln 'M m 'Order n '→ (order-disjoint m n)))

M     (the cat sat on the mat)     Order     (mat cat)     →     (the mat sat on the cat)    
M     (the cat sat on the mat)     Order     (cat mat)     →     (the cat sat on the mat)    
M     (A B C A B C A B C)     Order     (C A C A)     →     (C B A C B A A B C)    
M     (A B C A B D A B E)     Order     (E A D A)     →     (E B C A B D A B A)    
M     (A B)     Order     (B)     →     (A B)    
M     (A B)     Order     (B A)     →     (B A)    
M     (A B B A)     Order     (B A)     →     (B A B A)    

```



## Elixir


```elixir
defmodule Order do
  def disjoint(m,n) do
    IO.write "#{Enum.join(m," ")} | #{Enum.join(n," ")} -> "
    Enum.chunk(n,2)
    |> Enum.reduce({m,0}, fn [x,y],{m,from} ->
         md = Enum.drop(m, from)
         if x > y and x in md and y in md do
           if Enum.find_index(md,&(&1==x)) > Enum.find_index(md,&(&1==y)) do
             new_from = max(Enum.find_index(m,&(&1==x)), Enum.find_index(m,&(&1==y))) + 1
             m = swap(m,from,x,y)
             from = new_from
           end
         end
         {m,from}
       end)
    |> elem(0)
    |> Enum.join(" ")
    |> IO.puts
  end
  
  defp swap(m,from,x,y) do
    ix = Enum.find_index(m,&(&1==x)) + from
    iy = Enum.find_index(m,&(&1==y)) + from
    vx = Enum.at(m,ix)
    vy = Enum.at(m,iy)
    m |> List.replace_at(ix,vy) |> List.replace_at(iy,vx)
  end
end

[ {"the cat sat on the mat", "mat cat"},
  {"the cat sat on the mat", "cat mat"},
  {"A B C A B C A B C"     , "C A C A"},
  {"A B C A B D A B E"     , "E A D A"},
  {"A B"                   , "B"},
  {"A B"                   , "B A"},
  {"A B B A"               , "B A"}     ]
|> Enum.each(fn {m,n} ->
     Order.disjoint(String.split(m),String.split(n))
   end)
```


```txt

the cat sat on the mat | mat cat -> the mat sat on the cat
the cat sat on the mat | cat mat -> the cat sat on the mat
A B C A B C A B C | C A C A -> C B A C B A A B C
A B C A B D A B E | E A D A -> E B C A B D A B A
A B | B -> A B
A B | B A -> B A
A B B A | B A -> B A B A

```



## Factor

This solution is a tad bit whimsical (and a testament to the flexibility of the language that it allows something like this). <code>make-slots</code> replaces elements from ''M'' with <code>_</code> from the <code>fry</code> vocabulary according to the elements in ''N''. For example,

```factor
qw{ the cat sat on the mat } qw{ mat cat } make-slots
```

produces <code>{ "the" _ "sat" "on" "the" _ }</code>. Then, <code>reorder</code> fries elements from ''N'' into the sequence. This is much like a regular fried quotation.

We must directly call <code>fry</code> on the sequence we've been building, because it's not a literal/static quotation. <code>fry</code> does not call anything directly; it produces a quotation which must be called later. Since we must use <code>call</code> on this runtime-computed value, we must provide a stack effect, but there's a problem. Because there can be any number of inputs to <code>fry</code>, our stack effect must be computed at run time. Luckily for us, we can do that with the <code>effects</code> vocabulary.

Finally, <code>input<sequence</code> is a smart combinator (a combinator that infers the stack effect of one or more of its inputs) that takes a sequence and a quotation and makes it so that from inside the quotation, you can think of sequence elements as though they were data stack objects. This is precisely what we want so that we can fry them.


```factor
USING: assocs combinators combinators.smart effects formatting
fry kernel qw sequences ;
IN: rosetta-code.order-disjoint-list

: make-slot ( seq elt -- )
    dupd [ = ] curry find drop swap [ \ _ ] 2dip set-nth ;

: make-slots ( seq elts -- seq' ) dupd [ make-slot ] with each ;

: reorder ( seq elts -- seq' )
    tuck make-slots [ ] like over { "x" } <effect>
    '[ _ fry _ call-effect ] input<sequence ; inline

: show-reordering ( seq elts -- )
    2dup [ clone ] dip reorder [ " " join ] tri@
    "M: %-23s N: %-8s M': %s\n" printf ; inline

{
    { qw{ the cat sat on the mat } qw{ mat cat } }
    { qw{ the cat sat on the mat } qw{ cat mat } }
    { qw{ A B C A B C A B C      } qw{ C A C A } }
    { qw{ A B C A B D A B E      } qw{ E A D A } }
    { qw{ A B                    } qw{ B       } }
    { qw{ A B                    } qw{ B A     } }
    { qw{ A B B A                } qw{ B A     } }
}
[ show-reordering ] assoc-each
```

```txt

M: the cat sat on the mat  N: mat cat  M': the mat sat on the cat
M: the cat sat on the mat  N: cat mat  M': the cat sat on the mat
M: A B C A B C A B C       N: C A C A  M': C B A C B A A B C
M: A B C A B D A B E       N: E A D A  M': E B C A B D A B A
M: A B                     N: B        M': A B
M: A B                     N: B A      M': B A
M: A B B A                 N: B A      M': B A B A

```



## Go


```go
package main

import (
	"fmt"
	"sort"
	"strings"
)

type indexSort struct {
	val sort.Interface
	ind []int
}

func (s indexSort) Len() int           { return len(s.ind) }
func (s indexSort) Less(i, j int) bool { return s.ind[i] < s.ind[j] }
func (s indexSort) Swap(i, j int) {
	s.val.Swap(s.ind[i], s.ind[j])
	s.ind[i], s.ind[j] = s.ind[j], s.ind[i]
}

func disjointSliceSort(m, n []string) []string {
	s := indexSort{sort.StringSlice(m), make([]int, 0, len(n))}
	used := make(map[int]bool)
	for _, nw := range n {
		for i, mw := range m {
			if used[i] || mw != nw {
				continue
			}
			used[i] = true
			s.ind = append(s.ind, i)
			break
		}
	}
	sort.Sort(s)
	return s.val.(sort.StringSlice)
}

func disjointStringSort(m, n string) string {
	return strings.Join(
		disjointSliceSort(strings.Fields(m), strings.Fields(n)), " ")
}

func main() {
	for _, data := range []struct{ m, n string }{
		{"the cat sat on the mat", "mat cat"},
		{"the cat sat on the mat", "cat mat"},
		{"A B C A B C A B C", "C A C A"},
		{"A B C A B D A B E", "E A D A"},
		{"A B", "B"},
		{"A B", "B A"},
		{"A B B A", "B A"},
	} {
		mp := disjointStringSort(data.m, data.n)
		fmt.Printf("%s → %s » %s\n", data.m, data.n, mp)
	}

}
```

```txt

the cat sat on the mat → mat cat » the mat sat on the cat
the cat sat on the mat → cat mat » the cat sat on the mat
the cat sat on the mat → cat cat cat mat » the cat sat on the mat
A B C A B C A B C → C A C A » C B A C B A A B C
A B C A B D A B E → E A D A » E B C A B D A B A
A B → B » A B
A B → B A » B A
A B B A → B A » B A B A

```


## Haskell


```Haskell
import Data.List (mapAccumL, sort)

order
  :: Ord a
  => [[a]] -> [a]
order [ms, ns] = snd . mapAccumL yu ls $ ks
  where
    ks = zip ms [(0 :: Int) ..]
    ls = zip ns . sort . snd . foldl go (sort ns, []) . sort $ ks
    yu ((u, v):us) (_, y)
      | v == y = (us, u)
    yu ys (x, _) = (ys, x)
    go (u:us, ys) (x, y)
      | u == x = (us, y : ys)
    go ts _ = ts

task :: [String] -> IO ()
task ls@[ms, ns] =
  putStrLn $
  "M: " ++ ms ++ " | N: " ++ ns ++ " |> " ++ (unwords . order . map words $ ls)

main :: IO ()
main =
  mapM_
    task
    [ ["the cat sat on the mat", "mat cat"]
    , ["the cat sat on the mat", "cat mat"]
    , ["A B C A B C A B C", "C A C A"]
    , ["A B C A B D A B E", "E A D A"]
    , ["A B", "B"]
    , ["A B", "B A"]
    , ["A B B A", "B A"]
    ]
```

```txt

M: the cat sat on the mat | N: mat cat |> the mat sat on the cat
M: the cat sat on the mat | N: cat mat |> the cat sat on the mat
M: A B C A B C A B C | N: C A C A |> C B A C B A A B C
M: A B C A B D A B E | N: E A D A |> E B C A B D A B A
M: A B | N: B |> A B
M: A B | N: B A |> B A
M: A B B A | N: B A |> B A B A

```



Or, accumulating a segmentation of M over a fold, and zipping with N:
```Haskell
import Control.Arrow ((***))
import Prelude hiding (unlines, unwords, words, length)
import Data.List (delete, transpose)
import Data.Text
       hiding (concat, zipWith, foldl, transpose, maximum)

disjointOrder
  :: Eq a
  => [a] -> [a] -> [a]
disjointOrder m n = concat $ zipWith (++) ms ns
  where
    ms = segments m n
    ns = ((: []) <$> n) ++ [[]] -- as list of lists, lengthened by 1
    segments
      :: Eq a
      => [a] -> [a] -> [[a]]
    segments m n = _m ++ [_acc]
      where
        (_m, _, _acc) = foldl split ([], n, []) m
        split
          :: Eq a
          => ([[a]], [a], [a]) -> a -> ([[a]], [a], [a])
        split (ms, ns, acc) x
          | x `elem` ns = (ms ++ [acc], delete x ns, [])
          | otherwise = (ms, ns, acc ++ [x])

-- TEST -----------------------------------------------------------
tests :: [(Text, Text)]
tests =
  (pack *** pack) <$>
  [ ("the cat sat on the mat", "mat cat")
  , ("the cat sat on the mat", "cat mat")
  , ("A B C A B C A B C", "C A C A")
  , ("A B C A B D A B E", "E A D A")
  , ("A B", "B")
  , ("A B", "B A")
  , ("A B B A", "B A")
  ]

table :: Text -> [[Text]] -> Text
table delim rows =
  unlines $
  intercalate delim <$>
  transpose
    ((\col ->
         let width = (length $ maximum col)
         in justifyLeft width ' ' <$> col) <$>
     transpose rows)

main :: IO ()
main =
  putStr $
  unpack $
  table (pack "  ->  ") $
  (\(m, n) -> [m, n, unwords (disjointOrder (words m) (words n))]) <$> tests
```


```txt
the cat sat on the mat  ->  mat cat  ->  the mat sat on the cat
the cat sat on the mat  ->  cat mat  ->  the cat sat on the mat
A B C A B C A B C       ->  C A C A  ->  C B A C B A A B C     
A B C A B D A B E       ->  E A D A  ->  E B C A B D A B A     
A B                     ->  B        ->  A B                   
A B                     ->  B A      ->  B A                   
A B B A                 ->  B A      ->  B A B A  
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages.  Assumes a single blank separates items:


```unicon
procedure main(A)
    every write(" -> ",odli("the cat sat on the mat","mat cat"))
    every write(" -> ",odli("the cat sat on the mat","cat mat"))
    every write(" -> ",odli("A B C A B C A B C","C A C A"))
    every write(" -> ",odli("A B C A B D A B E","E A D A"))
    every write(" -> ",odli("A B","B"))
    every write(" -> ",odli("A B","B A"))
    every write(" -> ",odli("A B B A","B A"))
end

procedure odli(M,N)
    writes(M," :: ",N)
    Mp := "" 
    P := N ||:= " "
    (M||" ") ? while item := tab(upto(' '))||move(1) do {
            if find(item,P) then {
                P ?:= 1(tab(find(item)),move(*item))||tab(0)
                N ?:= (item := tab(upto(' '))||move(1), tab(0))
                }
            Mp ||:= item
            }
    return Mp
end
```


Output:

```txt

->odli
the cat sat on the mat :: mat cat -> the mat sat on the cat 
the cat sat on the mat :: cat mat -> the cat sat on the mat 
A B C A B C A B C :: C A C A -> C B A C B A A B C 
A B C A B D A B E :: E A D A -> E B C A B D A B A 
A B :: B -> A B 
A B :: B A -> B A 
A B B A :: B A -> B A B A 
->

```



## J


Implementation:


```J
disjorder=:3 :0&.;:
:
  clusters=. (</. i.@#) x
  order=. x i.&~. y
  need=. #/.~ y
  from=. ;need (#{.)each (/:~order){clusters
  to=. ;need {.!._ each order{clusters
  (from{x) to} x
)
```


Task examples:


```J
   'the cat sat on the mat' disjorder 'mat cat'
the mat sat on the cat
   'the cat sat on the mat' disjorder 'cat mat'
the cat sat on the mat
   'A B C A B C A B C'      disjorder 'C A C A'
C B A C B A A B C
   'A B C A B D A B E'      disjorder 'E A D A'
D B C D B E A B A
   'A B'                    disjorder 'B'      
A B
   'A B'                    disjorder 'B A'    
B A
   'A B B A'                disjorder 'B A'
B A B A
```



## Java

Doesn't handle the case when an item of N is not a member of M.

```java
import java.util.Arrays;
import java.util.BitSet;
import org.apache.commons.lang3.ArrayUtils;

public class OrderDisjointItems {

    public static void main(String[] args) {
        final String[][] MNs = {{"the cat sat on the mat", "mat cat"},
        {"the cat sat on the mat", "cat mat"},
        {"A B C A B C A B C", "C A C A"}, {"A B C A B D A B E", "E A D A"},
        {"A B", "B"}, {"A B", "B A"}, {"A B B A", "B A"}, {"X X Y", "X"}};

        for (String[] a : MNs) {
            String[] r = orderDisjointItems(a[0].split(" "), a[1].split(" "));
            System.out.printf("%s | %s -> %s%n", a[0], a[1], Arrays.toString(r));
        }
    }

    // if input items cannot be null
    static String[] orderDisjointItems(String[] m, String[] n) {
        for (String e : n) {
            int idx = ArrayUtils.indexOf(m, e);
            if (idx != -1)
                m[idx] = null;
        }
        for (int i = 0, j = 0; i < m.length; i++) {
            if (m[i] == null)
                m[i] = n[j++];
        }
        return m;
    }

    // otherwise
    static String[] orderDisjointItems2(String[] m, String[] n) {
        BitSet bitSet = new BitSet(m.length);
        for (String e : n) {
            int idx = -1;
            do {
                idx = ArrayUtils.indexOf(m, e, idx + 1);
            } while (idx != -1 && bitSet.get(idx));
            if (idx != -1)
                bitSet.set(idx);
        }
        for (int i = 0, j = 0; i < m.length; i++) {
            if (bitSet.get(i))
                m[i] = n[j++];
        }
        return m;
    }
}
```


Output:


```txt
the cat sat on the mat | mat cat -> [the, mat, sat, on, the, cat]
the cat sat on the mat | cat mat -> [the, cat, sat, on, the, mat]
A B C A B C A B C | C A C A -> [C, B, A, C, B, A, A, B, C]
A B C A B D A B E | E A D A -> [E, B, C, A, B, D, A, B, A]
A B | B -> [A, B]
A B | B A -> [B, A]
A B B A | B A -> [B, A, B, A]
X X Y | X -> [X, X, Y]
```



## JavaScript



### ES6


Accumulating a segmentation of M over a fold/reduce, and zipping with N:


```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // deleteFirst :: a -> [a] -> [a]
    const deleteFirst = (x, xs) =>
        xs.length > 0 ? (
            x === xs[0] ? (
                xs.slice(1)
            ) : [xs[0]].concat(deleteFirst(x, xs.slice(1)))
        ) : [];

    // flatten :: Tree a -> [a]
    const flatten = t => (t instanceof Array ? concatMap(flatten, t) : [t]);

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // words :: String -> [String]
    const words = s => s.split(/\s+/);

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map((x, i) => f(x, ys[i]));
    };

    //------------------------------------------------------------------------

    // ORDER DISJOINT LIST ITEMS

    // disjointOrder :: [String] -> [String] -> [String]
    const disjointOrder = (ms, ns) =>
        flatten(
            zipWith(
                (a, b) => a.concat(b),
                segments(ms, ns),
                ns.concat('')
            )
        );

    // segments :: [String] -> [String] -> [String]
    const segments = (ms, ns) => {
        const dct = ms.reduce((a, x) => {
            const wds = a.words,
                blnFound = wds.indexOf(x) !== -1;

            return {
                parts: a.parts.concat(blnFound ? [a.current] : []),
                current: blnFound ? [] : a.current.concat(x),
                words: blnFound ? deleteFirst(x, wds) : wds,
            };
        }, {
            words: ns,
            parts: [],
            current: []
        });

        return dct.parts.concat([dct.current]);
    };

    // -----------------------------------------------------------------------
    // FORMATTING TEST OUTPUT

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map((row) => row[iCol]));

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        xs.reduce((a, x) => a === undefined ? x : (
            f(x, a) > 0 ? x : a
        ), undefined);

    // 2 or more arguments
    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const intArgs = f.length,
            go = xs =>
            xs.length >= intArgs ? (
                f.apply(null, xs)
            ) : function () {
                return go(xs.concat([].slice.apply(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // justifyLeft :: Int -> Char -> Text -> Text
    const justifyLeft = (n, cFiller, strText) =>
        n > strText.length ? (
            (strText + replicateS(n, cFiller))
            .substr(0, n)
        ) : strText;

    // replicateS :: Int -> String -> String
    const replicateS = (n, s) => {
        let v = s,
            o = '';
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // -----------------------------------------------------------------------

    // TEST
    return transpose(transpose([{
                M: 'the cat sat on the mat',
                N: 'mat cat'
            }, {
                M: 'the cat sat on the mat',
                N: 'cat mat'
            }, {
                M: 'A B C A B C A B C',
                N: 'C A C A'
            }, {
                M: 'A B C A B D A B E',
                N: 'E A D A'
            }, {
                M: 'A B',
                N: 'B'
            }, {
                M: 'A B',
                N: 'B A'
            }, {
                M: 'A B B A',
                N: 'B A'
            }].map(dct => [
                dct.M, dct.N,
                unwords(disjointOrder(words(dct.M), words(dct.N)))
            ]))
            .map(col => {
                const width = maximumBy((a, b) => a.length > b.length, col)
                    .length;
                return col.map(curry(justifyLeft)(width, ' '));
            }))
        .map(
            ([a, b, c]) => a + '  ->  ' + b + '  ->  ' + c
        )
        .join('\n');
})();
```


```txt
the cat sat on the mat  ->  mat cat  ->  the mat sat on the cat 
the cat sat on the mat  ->  cat mat  ->  the cat sat on the mat 
A B C A B C A B C       ->  C A C A  ->  C B A C B A A B C      
A B C A B D A B E       ->  E A D A  ->  E B C A B D A B A      
A B                     ->  B        ->  A B                    
A B                     ->  B A      ->  B A                    
A B B A                 ->  B A      ->  B A B A                
```



## jq

Usage: <tt>M | disjoint_order(N)</tt>

```jq
def disjoint_order(N):
  # The helper function, indices, ensures that successive occurrences
  # of a particular value in N are matched by successive occurrences
  # in the input on the assumption that null is not initially in the input.
  def indices:
    . as $in
    | reduce range(0; N|length) as $i
       # state: [ array, indices ]
      ( [$in, []];
        (.[0] | index(N[$i])) as $ix | .[0][$ix] = null | .[1] += [$ix])
    | .[1];

  . as $in
  | (indices | sort) as $sorted
  | reduce range(0; N|length) as $i ($in; .[$sorted[$i]] = N[$i] ) ;
```


'''Examples''':

(scrollable)
<div style="overflow:scroll; height:400px;">

```jq
["the", "cat", "sat", "on", "the", "mat"] | indices( ["mat", "cat"] )
#=> ["the","mat","sat","on","the","cat"]
```



```jq
["the", "cat", "sat", "on", "the", "mat"] | disjoint_order( ["cat", "mat"] )
#=> ["the","cat","sat","on","the","mat"]
```



```jq
["A", "B", "C", "A", "B", "C", "A", "B", "C"] | disjoint_order( ["C", "A", "C", "A"] )
#=> ["C","B","A","C","B","A","A","B","C"]
```



```jq
["A", "B", "C", "A", "B", "D", "A", "B", "E"] | disjoint_order( ["E", "A", "D", "A"] )
#=> ["E","B","C","A","B","D","A","B","A"]
```



```jq
["A", "B"] | disjoint_order( ["B"] )
#=> ["A","B"]
```



```jq
["A", "B"] | disjoint_order( ["B", "A"] )
#=> ["B","A"]
```



```jq
["A", "B", "B", "A"] | disjoint_order( ["B", "A"] )
#=> ["B","A","B","A"]
```



```jq
["X", "X", "Y"] | disjoint_order(["X"])
#=> [X, X, Y]
```

</div>


## Julia

<tt>order_disjoint</tt> works by finding the indices of <tt>n</tt> in <tt>m</tt> and replacing the elements in <tt>m</tt> with those in <tt>n</tt> according to the sorted indices.  When <tt>n</tt> either contains elements not in <tt>m</tt> or more copies of an element than exist in <tt>m</tt>, the function throws a <tt>DomainError</tt>.

'''Function'''

```Julia

function order_disjoint{T<:AbstractArray}(m::T, n::T)
    rlen = length(n)
    rdis = zeros(Int, rlen)
    for (i, e) in enumerate(n)
        j = findfirst(m, e)
        while j in rdis && j != 0
            j = findnext(m, e, j+1)
        end
        rdis[i] = j
    end
    if 0 in rdis
        throw(DomainError())
    end
    sort!(rdis)
    p = copy(m)
    p[rdis] = n
    return p
end

```

'''Main'''

```Julia

testm = {["the", "cat", "sat", "on", "the", "mat"],
         ["the", "cat", "sat", "on", "the", "mat"],
         ["A", "B", "C", "A", "B", "C", "A", "B", "C"],
         ["A", "B", "C", "A", "B", "D", "A", "B", "E"],
         ["A", "B"],
         ["A", "B"],
         ["A", "B", "B", "A"],
         }

testn = {["mat", "cat"],
         ["cat", "mat"],
         ["C", "A", "C", "A"],
         ["E", "A", "D", "A"],
         ["B"],
         ["B", "A"],
         ["B", "A"],
         }

for i in 1:length(testm)
    m = join(testm[i], " ")
    n = join(testn[i], " ")
    p = join(order_disjoint(testm[i], testn[i]), " ")
    println("    (", m, ", ", n, ") => ", p)
end

```


```txt

    (the cat sat on the mat, mat cat) => the mat sat on the cat
    (the cat sat on the mat, cat mat) => the cat sat on the mat
    (A B C A B C A B C, C A C A) => C B A C B A A B C
    (A B C A B D A B E, E A D A) => E B C A B D A B A
    (A B, B) => A B
    (A B, B A) => B A
    (A B B A, B A) => B A B A

```



## Kotlin


```scala
// version 1.0.6

const val NULL = "\u0000"

fun orderDisjointList(m: String, n: String): String {
    val nList = n.split(' ')
    // first replace the first occurrence of items of 'n' in 'm' with the NULL character 
    // which we can safely assume won't occur in 'm' naturally
    var p = m
    for (item in nList) p = p.replaceFirst(item, NULL)
    // now successively replace the NULLs with items from nList 
    val mList = p.split(NULL)
    val sb = StringBuilder()
    for (i in 0 until nList.size) sb.append(mList[i], nList[i])       
    return sb.append(mList.last()).toString()
}

fun main(args: Array<String>) {
    val m = arrayOf(
        "the cat sat on the mat",
        "the cat sat on the mat",
        "A B C A B C A B C",
        "A B C A B D A B E",
        "A B",
        "A B", 
        "A B B A"
    ) 
    val n = arrayOf(
        "mat cat",
        "cat mat",
        "C A C A",
        "E A D A",
        "B",
        "B A",
        "B A"
    )
    for (i in 0 until m.size) 
        println("${m[i].padEnd(22)}  ->  ${n[i].padEnd(7)}  ->  ${orderDisjointList(m[i], n[i])}")
}
```


```txt

the cat sat on the mat  ->  mat cat  ->  the mat sat on the cat
the cat sat on the mat  ->  cat mat  ->  the cat sat on the mat
A B C A B C A B C       ->  C A C A  ->  C B A C B A A B C
A B C A B D A B E       ->  E A D A  ->  E B C A B D A B A
A B                     ->  B        ->  A B
A B                     ->  B A      ->  B A
A B B A                 ->  B A      ->  B A B A

```



## Lua


```Lua
-- Split str on any space characters and return as a table
function split (str)
    local t = {}
    for word in str:gmatch("%S+") do table.insert(t, word) end
    return t
end

-- Order disjoint list items
function orderList (dataStr, orderStr)
    local data, order = split(dataStr), split(orderStr)
    for orderPos, orderWord in pairs(order) do
        for dataPos, dataWord in pairs(data) do
            if dataWord == orderWord then
                data[dataPos] = false
                break
            end
        end
    end
    local orderPos = 1
    for dataPos, dataWord in pairs(data) do
        if not dataWord then
            data[dataPos] = order[orderPos]
            orderPos = orderPos + 1
            if orderPos > #order then return data end
        end
    end
    return data
end

-- Main procedure
local testCases = {
    {'the cat sat on the mat', 'mat cat'},
    {'the cat sat on the mat', 'cat mat'},
    {'A B C A B C A B C'     , 'C A C A'},
    {'A B C A B D A B E'     , 'E A D A'},
    {'A B'                   , 'B'},
    {'A B'                   , 'B A'},    
    {'A B B A'               , 'B A'}
}
for _, example in pairs(testCases) do
    print(table.concat(orderList(unpack(example)), " "))
end
```

```txt
the mat sat on the cat
the cat sat on the mat
C B A C B A A B C
E B C A B D A B A
A B
B A
B A B A
```



## Mathematica


```Mathematica
order[m_, n_] := 
  ReplacePart[m, 
   MapThread[
    Rule, {Position[m, Alternatives @@ n][[;; Length[n]]], n}]];
Print[StringRiffle[
   order[{"the", "cat", "sat", "on", "the", "mat"}, {"mat", 
     "cat"}]]];
Print[StringRiffle[
   order[{"the", "cat", "sat", "on", "the", "mat"}, {"cat", 
     "mat"}]]];
Print[StringRiffle[
   order[{"A", "B", "C", "A", "B", "C", "A", "B", "C"}, {"C", "A", 
     "C", "A"}]]];
Print[StringRiffle[
   order[{"A", "B", "C", "A", "B", "D", "A", "B", "E"}, {"E", "A", 
     "D", "A"}]]];
Print[StringRiffle[order[{"A", "B"}, {"B"}]]];
Print[StringRiffle[order[{"A", "B"}, {"B", "A"}]]];
Print[StringRiffle[order[{"A", "B", "B", "A"}, {"B", "A"}]]];
```

```txt
the mat sat on the cat
the cat sat on the mat
C B A C B A A B C
E B C A B D A B E
A B
B A
B A B A
```



## Perl


```perl
sub dsort {
        my ($m, $n) = @_;
        my %h;
        $h{$_}++ for @$n;
        map $h{$_}-- > 0 ? shift @$n : $_, @$m;
}

for (split "\n", <<"IN")
        the cat sat on the mat  | mat cat
        the cat sat on the mat  | cat mat
        A B C A B C A B C       | C A C A
        A B C A B D A B E       | E A D A
        A B                     | B
        A B                     | B A
        A B B A                 | B A
IN
{

        my ($a, $b) = map([split], split '\|');
        print "@$a | @$b -> @{[dsort($a, $b)]}\n";
}
```

```txt

the cat sat on the mat | mat cat -> the mat sat on the cat
the cat sat on the mat | mat cat -> the mat sat on the cat
the cat sat on the mat | cat mat -> the cat sat on the mat
A B C A B C A B C | C A C A -> C B A C B A A B C
A B C A B D A B E | E A D A -> E B C A B D A B A
A B | B -> A B
A B | B A -> B A
A B B A | B A -> B A B A

```



## Perl 6

```perl6
sub order-disjoint-list-items(\M, \N) {
    my \bag = N.BagHash;
    M.map: { bag{$_}-- ?? N.shift !! $_ }
}

# Testing:

for q:to/---/.comb(/ [\S+]+ % ' ' /).map({[.words]})
    the cat sat on the mat      mat cat
    the cat sat on the mat      cat mat
    A B C A B C A B C           C A C A
    A B C A B D A B E           E A D A
    A B                         B
    A B                         B A
    A B B A                     B A
    X X Y                       X
    A X                         Y A
    ---
->  $m, $n { say "\n$m ==> $n\n", order-disjoint-list-items($m, $n) }
```

```txt
the cat sat on the mat ==> mat cat
the mat sat on the cat

the cat sat on the mat ==> cat mat
the cat sat on the mat

A B C A B C A B C ==> C A C A
C B A C B A A B C

A B C A B D A B E ==> E A D A
E B C A B D A B A

A B ==> B
A B

A B ==> B A
B A

A B B A ==> B A
B A B A

X X Y ==> X
X X Y

A X ==> Y A
Y X
```



## Phix

Modified to support/skip missing elements

```Phix
function order_disjoint(sequence m, sequence n)
integer rlen = length(n)
sequence rdis = repeat(0,rlen)
    for i=1 to rlen do
        object e = n[i]
        integer j = find(e,m)
        while j!=0 and find(j,rdis) do
            j = find(e,m,j+1)
        end while
        rdis[i] = j
    end for
    rdis = sort(rdis)
    while rlen and rdis[1]=0 do
        rdis = rdis[2..$]
        rlen -= 1
    end while
    for i=1 to rlen do
        m[rdis[i]]=n[i]
    end for
    return join(m)
end function

sequence tests = {{"the cat sat on the mat","mat cat"},
                  {"the cat sat on the mat","cat mat"},
                  {"A B C A B C A B C","C A C A"},
                  {"A B C A B D A B E","E A D A"},
                  {"A B","B"},
                  {"A B","B A"},
                  {"A B B A","B A"},
                  {"",""},
                  {"A","A"},
                  {"A B",""},
                  {"A B B A","A B"},
                  {"A B A B","A B"},
                  {"A B A B","B A B A"},
                  {"A B C C B A","A C A C"},
                  {"A B C C B A","C A C A"},
                  {"A X","Y A"},
                  {"A X","Y A X"},
                  {"A X","Y X A"}}

for i=1 to length(tests) do
    string {m,n} = tests[i]
    printf(1,"\"%s\",\"%s\" => \"%s\"\n",{m,n,order_disjoint(split(m),split(n))})
end for 
```

```txt

"the cat sat on the mat","mat cat" => "the mat sat on the cat"
"the cat sat on the mat","cat mat" => "the cat sat on the mat"
"A B C A B C A B C","C A C A" => "C B A C B A A B C"
"A B C A B D A B E","E A D A" => "E B C A B D A B A"
"A B","B" => "A B"
"A B","B A" => "B A"
"A B B A","B A" => "B A B A"
"","" => ""
"A","A" => "A"
"A B","" => "A B"
"A B B A","A B" => "A B B A"
"A B A B","A B" => "A B A B"
"A B A B","B A B A" => "B A B A"
"A B C C B A","A C A C" => "A B C A B C"
"A B C C B A","C A C A" => "C B A C B A"
"A X","Y A" => "Y X"
"A X","Y A X" => "Y A"
"A X","Y X A" => "Y X"

```



## PicoLisp


```PicoLisp
(de orderDisjoint (M N)
   (for S N
      (and (memq S M) (set @ NIL)) )
   (mapcar
      '((S) (or S (pop 'N)))
      M ) )
```

Test:

```PicoLisp
: (orderDisjoint '(the cat sat on the mat) '(mat cat))
-> (the mat sat on the cat)

: (orderDisjoint '(the cat sat on the mat) '(cat mat))
-> (the cat sat on the mat)

: (orderDisjoint '(A B C A B C A B C) '(C A C A))
-> (C B A C B A A B C)

: (orderDisjoint '(A B C A B D A B E) '(E A D A))
-> (E B C A B D A B A)

: (orderDisjoint '(A B) '(B))
-> (A B)

: (orderDisjoint '(A B) '(B A))
-> (B A)

: (orderDisjoint '(A B B A) '(B A))
-> (B A B A)
```



## PowerShell


```PowerShell

function sublistsort($M, $N) {
    $arr = $M.Split(' ')
    $array = $N.Split(' ') | group
    $Count = @($array |foreach {$_.Count})
    $ip, $i = @(), 0
    $arr | foreach{ 
        $name = "$_"
        $j = $array.Name.IndexOf($name)
        if($j -gt -1){
            $k = $Count[$j] - 1
            if($k -ge 0) {
                $ip += @($i)
                $Count[$j] = $k
            }
        }
        $i++
    }
    $i = 0
    $N.Split(' ') | foreach{ $arr[$ip[$i++]] = "$_"}
    [pscustomobject]@{
        "M" = "$M "
        "N" = "$N "
        "M'" = "$($arr)"
    } 
}
$M1 = 'the cat sat on the mat'
$N1 =  'mat cat'
$M2 = 'the cat sat on the mat' 
$N2 = 'cat mat'
$M3 = 'A B C A B C A B C'      
$N3 = 'C A C A'
$M4 = 'A B C A B D A B E'      
$N4 = 'E A D A'
$M5 = 'A B'                    
$N5 = 'B'     
$M6 = 'A B'                   
$N6 = 'B A'   
$M7 = 'A B B A'                
$N7 = 'B A'
sublistsort $M1 $N1
sublistsort $M2 $N2
sublistsort $M3 $N3
sublistsort $M4 $N4
sublistsort $M5 $N5
sublistsort $M6 $N6
sublistsort $M7 $N7

```

<b>Output:</b>

```txt

M                       N        M'                    
-                       -        --                    
the cat sat on the mat  mat cat  the mat sat on the cat
the cat sat on the mat  cat mat  the cat sat on the mat
A B C A B C A B C       C A C A  C B A C B A A B C     
A B C A B D A B E       E A D A  E B C A B D A B A     
A B                     B        A B                   
A B                     B A      B A                   
A B B A                 B A      B A B A

```



## Python


```python
from __future__ import print_function

def order_disjoint_list_items(data, items):
    #Modifies data list in-place
    itemindices = []
    for item in set(items):
        itemcount = items.count(item)
        #assert data.count(item) >= itemcount, 'More of %r than in data' % item
        lastindex = [-1]
        for i in range(itemcount):
            lastindex.append(data.index(item, lastindex[-1] + 1))
        itemindices += lastindex[1:]
    itemindices.sort()
    for index, item in zip(itemindices, items):
        data[index] = item

if __name__ == '__main__':
    tostring = ' '.join
    for data, items in [ (str.split('the cat sat on the mat'), str.split('mat cat')),
                         (str.split('the cat sat on the mat'), str.split('cat mat')),
                         (list('ABCABCABC'), list('CACA')),
                         (list('ABCABDABE'), list('EADA')),
                         (list('AB'), list('B')),
                         (list('AB'), list('BA')),
                         (list('ABBA'), list('BA')),
                         (list(''), list('')),
                         (list('A'), list('A')),
                         (list('AB'), list('')),
                         (list('ABBA'), list('AB')),
                         (list('ABAB'), list('AB')),
                         (list('ABAB'), list('BABA')),
                         (list('ABCCBA'), list('ACAC')),
                         (list('ABCCBA'), list('CACA')),
                       ]:
        print('Data M: %-24r Order N: %-9r' % (tostring(data), tostring(items)), end=' ')
        order_disjoint_list_items(data, items)
        print("-> M' %r" % tostring(data))
```


```txt
Data M: 'the cat sat on the mat' Order N: 'mat cat' -> M' 'the mat sat on the cat'
Data M: 'the cat sat on the mat' Order N: 'cat mat' -> M' 'the cat sat on the mat'
Data M: 'A B C A B C A B C'      Order N: 'C A C A' -> M' 'C B A C B A A B C'
Data M: 'A B C A B D A B E'      Order N: 'E A D A' -> M' 'E B C A B D A B A'
Data M: 'A B'                    Order N: 'B'       -> M' 'A B'
Data M: 'A B'                    Order N: 'B A'     -> M' 'B A'
Data M: 'A B B A'                Order N: 'B A'     -> M' 'B A B A'
Data M: ''                       Order N: ''        -> M' ''
Data M: 'A'                      Order N: 'A'       -> M' 'A'
Data M: 'A B'                    Order N: ''        -> M' 'A B'
Data M: 'A B B A'                Order N: 'A B'     -> M' 'A B B A'
Data M: 'A B A B'                Order N: 'A B'     -> M' 'A B A B'
Data M: 'A B A B'                Order N: 'B A B A' -> M' 'B A B A'
Data M: 'A B C C B A'            Order N: 'A C A C' -> M' 'A B C A B C'
Data M: 'A B C C B A'            Order N: 'C A C A' -> M' 'C B A C B A'
```



## Racket


```racket
#lang racket
(define disjorder
  (match-lambda**
   (((list) n) '())      
   ((m (list)) m)      
   (((list h m-tail ...) (list h n-tail ...))
    (list* h (disjorder m-tail n-tail)))
   ;; the (not g/h) below stop greedy matching of the list which
   ;; would pick out orderings from the right first.
   (((list h (and (not g) m-tail-left) ... g m-tail-right ...)
     (list g (and (not h) n-tail-left) ... h n-tail-right ...))
    (disjorder `(,g ,@m-tail-left ,h ,@m-tail-right)
               `(,g ,@n-tail-left ,h ,@n-tail-right)))
   (((list h m-tail ...) n)
    (list* h (disjorder m-tail n)))))

(define (report-disjorder m n)
 (printf "Data M: ~a Order N: ~a -> ~a~%"
  (~a #:min-width 25 m) (~a #:min-width 10 n) (disjorder m n)))

;; Do the task tests
(report-disjorder '(the cat sat on the mat) '(mat cat))
(report-disjorder '(the cat sat on the mat) '(cat mat))
(report-disjorder '(A B C A B C A B C)      '(C A C A))
(report-disjorder '(A B C A B D A B E)      '(E A D A))
(report-disjorder '(A B)                    '(B))
(report-disjorder '(A B)                    '(B A))
(report-disjorder '(A B B A)                '(B A))
;; Do all of the other python tests
(report-disjorder '()            '())
(report-disjorder '(A)           '(A))
(report-disjorder '(A B)         '())
(report-disjorder '(A B B A)     '(A B))
(report-disjorder '(A B A B)     '(A B))
(report-disjorder '(A B A B)     '(B A B A))
(report-disjorder '(A B C C B A) '(A C A C))
(report-disjorder '(A B C C B A) '(C A C A))
```


```txt
Data M: (the cat sat on the mat)  Order N: (mat cat)  -> (the mat sat on the cat)
Data M: (the cat sat on the mat)  Order N: (cat mat)  -> (the cat sat on the mat)
Data M: (A B C A B C A B C)       Order N: (C A C A)  -> (C B A C B A A B C)
Data M: (A B C A B D A B E)       Order N: (E A D A)  -> (E B C A B D A B A)
Data M: (A B)                     Order N: (B)        -> (A B)
Data M: (A B)                     Order N: (B A)      -> (B A)
Data M: (A B B A)                 Order N: (B A)      -> (B A B A)
Data M: ()                        Order N: ()         -> ()
Data M: (A)                       Order N: (A)        -> (A)
Data M: (A B)                     Order N: ()         -> (A B)
Data M: (A B B A)                 Order N: (A B)      -> (A B B A)
Data M: (A B A B)                 Order N: (A B)      -> (A B A B)
Data M: (A B A B)                 Order N: (B A B A)  -> (B A B A)
Data M: (A B C C B A)             Order N: (A C A C)  -> (A B C A B C)
Data M: (A B C C B A)             Order N: (C A C A)  -> (C B A C B A)
```



## REXX

Note:   items in   <big>'''N'''</big>   needn't be in   <big>'''M'''</big>. 

```rexx
/*REXX program orders a  disjoint list  of   M   items  with a list of   N   items.     */
used = '0'x                                      /*indicates that a word has been parsed*/
 @.   =                                          /*placeholder indicates  end─of─array, */
 @.1  =   " the cat sat on the mat        |      mat cat  "                  /*a string.*/
 @.2  =   " the cat sat on the mat        |      cat mat  "                  /*"    "   */
 @.3  =   " A B C A B C A B C             |      C A C A  "                  /*"    "   */
 @.4  =   " A B C A B D A B E             |      E A D A  "                  /*"    "   */
 @.5  =   " A B                           |      B        "                  /*"    "   */
 @.6  =   " A B                           |      B A      "                  /*"    "   */
 @.7  =   " A B B A                       |      B A      "                  /*"    "   */
 @.8  =   "                               |               "                  /*"    "   */
 @.9  =   " A                             |      A        "                  /*"    "   */
 @.10 =   " A B                           |               "                  /*"    "   */
 @.11 =   " A B B A                       |      A B      "                  /*"    "   */
 @.12 =   " A B A B                       |      A B      "                  /*"    "   */
 @.13 =   " A B A B                       |      B A B A  "                  /*"    "   */
 @.14 =   " A B C C B A                   |      A C A C  "                  /*"    "   */
 @.15 =   " A B C C B A                   |      C A C A  "                  /*"    "   */
       /*  ════════════M═══════════             ════N════        */

  do j=1  while  @.j\==''                        /* [↓]  process each input string (@.).*/
  parse var  @.j    m   '|'   n                  /*parse input string into   M  and  N. */
  #= words(m)                                    /*#:   number of words in the  M  list.*/
               do i=#  for #  by -1              /*process list items in reverse order. */
               _= word(m, i);   !.i= _;   $._= i /*construct the   !.   and  $.  arrays.*/
               end   /*i*/
  r.=                                            /*nullify the replacement string  [R.] */
       do k=1  by 2  for words(n)%2              /* [↓]  process the  N  array.         */
       _= word(n, k);         v= word(n, k+1)    /*get an order word and the replacement*/
       p1= wordpos(_, m);     p2= wordpos(v, m)  /*positions of   "   "   "       "     */
       if p1==0 | p2==0  then iterate            /*if either not found, then skip them. */
       if $._>>$.v  then do;   r.p2= !.p1;    r.p1= !.p2;    end     /*switch the words.*/
                    else do;   r.p1= !.p1;    r.p2= !.p2;    end     /*don't switch.    */
       !.p1= used;    !.p2= used                                     /*mark 'em as used.*/
       m=
                         do i=1  for #;   m= m !.i;    _= word(m, i);    !.i= _;    $._= i
                         end   /*i*/
       end   /*k*/                               /* [↑]  rebuild the  !. and  $. arrays.*/
  mp=                                            /*the  MP  (aka M')  string  (so far). */
       do q=1  for #;    if !.q==used  then mp= mp  r.q              /*use the original.*/
                                       else mp= mp  !.q              /*use substitute.  */
       end   /*q*/                               /* [↑]  re─build the (output) string.  */

  say @.j   ' ────► '    space(mp)               /*display new re─ordered text ──► term.*/
  end        /*j*/                               /* [↑]  end of processing for  N  words*/
                                                 /*stick a fork in it,  we're all done. */
```

{out|output|text=  when using the internal default inputs:}}

```txt

 the cat sat on the mat      |      mat cat   ───► the mat sat on the cat
 the cat sat on the mat      |      cat mat   ───► the cat sat on the mat
 A B C A B C A B C           |      C A C A   ───► C B A C B A A B C
 A B C A B D A B E           |      E A D A   ───► E B C A B D A B A
 A B                         |      B         ───► A B
 A B                         |      B A       ───► B A
 A B B A                     |      B A       ───► B A B A
                             |                ───►
 A                           |      A         ───► A
 A B                         |                ───► A B
 A B B A                     |      A B       ───► A B B A
 A B A B                     |      A B       ───► A B A B
 A B A B                     |      B A B A   ───► B A B A
 A B C C B A                 |      A C A C   ───► A B C A B C
 A B C C B A                 |      C A C A   ───► C B A C B A

```



## Ruby


```ruby
def order_disjoint(m,n)
  print "#{m} | #{n} -> "
  m, n = m.split, n.split
  from = 0
  n.each_slice(2) do |x,y|
    next unless y
    sd = m[from..-1]
    if x > y && (sd.include? x) && (sd.include? y) && (sd.index(x) > sd.index(y))
      new_from = m.index(x)+1
      m[m.index(x)+from], m[m.index(y)+from] = m[m.index(y)+from], m[m.index(x)+from]
      from = new_from
    end
  end
  puts m.join(' ')
end

[
  ['the cat sat on the mat', 'mat cat'],
  ['the cat sat on the mat', 'cat mat'],
  ['A B C A B C A B C'     , 'C A C A'],
  ['A B C A B D A B E'     , 'E A D A'],
  ['A B'                   , 'B'      ],
  ['A B'                   , 'B A'    ],
  ['A B B A'               , 'B A'    ]
].each {|m,n| order_disjoint(m,n)}
```

```txt

the cat sat on the mat | mat cat -> the mat sat on the cat
the cat sat on the mat | cat mat -> the cat sat on the mat
A B C A B C A B C | C A C A -> C B A C B A A B C
A B C A B D A B E | E A D A -> E B C A B D A B A
A B | B -> A B
A B | B A -> B A
A B B A | B A -> B A B A

```



## Scala


```Scala
def order[T](input: Seq[T], using: Seq[T], used: Seq[T] = Seq()): Seq[T] =
  if (input.isEmpty || used.size >= using.size) input
  else if (using diff used contains input.head)
    using(used.size) +: order(input.tail, using, used :+ input.head)
  else input.head +: order(input.tail, using, used)
```

'''Test:'''

```Scala
val tests = List(
  "the cat sat on the mat" -> "mat cat",
  "the cat sat on the mat" -> "cat mat",
  "A B C A B C A B C"      -> "C A C A",
  "A B C A B D A B E"      -> "E A D A",
  "A B"                    -> "B",
  "A B"                    -> "B A",
  "A B B A"                -> "B A"
)

tests.foreach{case (input, using) =>
  val done = order(input.split(" "), using.split(" "))
  println(f"""Data M: $input%-24s Order N: $using%-9s -> Result M': ${done mkString " "}""")
}
```

```txt
Data M: the cat sat on the mat   Order N: mat cat   -> Result M': the mat sat on the cat
Data M: the cat sat on the mat   Order N: cat mat   -> Result M': the cat sat on the mat
Data M: A B C A B C A B C        Order N: C A C A   -> Result M': C B A C B A A B C
Data M: A B C A B D A B E        Order N: E A D A   -> Result M': E B C A B D A B A
Data M: A B                      Order N: B         -> Result M': A B
Data M: A B                      Order N: B A       -> Result M': B A
Data M: A B B A                  Order N: B A       -> Result M': B A B A
```



## Sidef

```ruby
func dsort(m, n) {
    var h = Hash()
    n.each {|item| h{item} := 0 ++ }
    m.map  {|item| h{item} := 0 -- > 0 ? n.shift : item}
}

<<'EOT'.lines.each { |line|
        the cat sat on the mat  | mat cat
        the cat sat on the mat  | cat mat
        A B C A B C A B C       | C A C A
        A B C A B D A B E       | E A D A
        A B                     | B
        A B                     | B A
        A B B A                 | B A
EOT
        var (a, b) = line.split('|').map{.words}...
        say "#{a.to_s} | #{b.to_s} -> #{dsort(a.clone, b.clone).to_s}"
}
```

```txt

the cat sat on the mat | mat cat -> the mat sat on the cat
the cat sat on the mat | cat mat -> the cat sat on the mat
A B C A B C A B C | C A C A -> C B A C B A A B C
A B C A B D A B E | E A D A -> E B C A B D A B A
A B | B -> A B
A B | B A -> B A
A B B A | B A -> B A B A

```



## Swift



```swift
func disjointOrder<T: Hashable>
(m: [T], n: [T]) -> [T] {
  let replaceCounts = n.reduce(into: [T: Int](), { $0[$1, default: 0] += 1 })
  let reduced = m.reduce(into: ([T](), n, replaceCounts), {cur, el in
    cur.0.append(cur.2[el, default: 0] > 0 ? cur.1.removeFirst() : el)
    cur.2[el]? -= 1
  })

  return reduced.0
}

print(disjointOrder(m: ["the", "cat", "sat", "on", "the", "mat"], n: ["mat", "cat"]))
print(disjointOrder(m: ["the", "cat", "sat", "on", "the", "mat"], n: ["cat", "mat"]))
print(disjointOrder(m: ["A", "B", "C", "A", "B", "C", "A", "B", "C"], n: ["C", "A", "C", "A"]))
print(disjointOrder(m: ["A", "B", "C", "A", "B", "D", "A", "B", "E"], n: ["E", "A", "D", "A"]))
print(disjointOrder(m: ["A", "B"], n: ["B"]))
print(disjointOrder(m: ["A", "B"], n: ["B", "A"]))
print(disjointOrder(m: ["A", "B", "B", "A"], n: ["B", "A"]))
```


```txt
["the", "mat", "sat", "on", "the", "cat"]
["the", "cat", "sat", "on", "the", "mat"]
["C", "B", "A", "C", "B", "A", "A", "B", "C"]
["E", "B", "C", "A", "B", "D", "A", "B", "A"]
["A", "B"]
["B", "A"]
["B", "A", "B", "A"]
```



## Tcl

This is a simple version that assumes that ''all'' items in the order list are present in the list to be arranged:

```tcl
proc orderDisjoint {theList theOrderList} {
    foreach item $theOrderList {incr n($item)}
    set is {}
    set i 0
    foreach item $theList {
	if {[info exist n($item)] && [incr n($item) -1] >= 0} {
	    lappend is $i
	}
	incr i
    }
    foreach item $theOrderList i $is {lset theList $i $item}
    return $theList
}
```

This is a more sophisticated version that handles items in the order list not being present in the list to be arranged:

```tcl
proc orderDisjoint {theList theOrderList} {
    foreach item $theOrderList {incr n($item)}
    set is -
    set i 0
    foreach item $theList {
	if {[info exist n($item)] && [incr n($item) -1] >= 0} {
	    lappend is $i
	}
	incr i
    }
    set i 0
    foreach item $theOrderList {
	if {[incr n($item)] <= 1} {
	    lset theList [lindex $is [incr i]] $item
	}
    }
    return $theList
}
```

Demonstration code (produces the same output from both implementations):

```tcl
foreach {items order} {
    "the cat sat on the mat" "mat cat"
    "the cat sat on the mat" "cat mat"
    "A B C A B C A B C"      "C A C A"
    "A B C A B D A B E"      "E A D A"
    "A B"                    "B"
    "A B"                    "B A"
    "A B B A"                "B A"
} {
    puts "'$items' with '$order' => '[orderDisjoint $items $order]'"
}
```

```txt

'the cat sat on the mat' with 'mat cat' => 'the mat sat on the cat'
'the cat sat on the mat' with 'cat mat' => 'the cat sat on the mat'
'A B C A B C A B C' with 'C A C A' => 'C B A C B A A B C'
'A B C A B D A B E' with 'E A D A' => 'E B C A B D A B A'
'A B' with 'B' => 'A B'
'A B' with 'B A' => 'B A'
'A B B A' with 'B A' => 'B A B A'

```



## zkl


```zkl
fcn disOrder(sm,sn){
   M:=sm.split(" ");
   N:=sn.split(" "); nc:=Walker.cycle(Utils.Helpers.listUnique(N));
   dn:=Dictionary(); N.pump(Void,'wrap(w){ dn[w] = dn.find(w,0) + 1; });
   M.pump(String,'wrap(w){ 
      if (Void==(n:=dn.find(w))) return(w); // not replaced
      if (n) { dn[w]=n-1; nc.next(); } // swaps left--
      else   { nc.next(); w }	       // exhausted
   }, String.fp(" ") )[1,*]	// remove leading blank
}
```

A dictionary is used to hold count of the words in N, which is decremented as the words are used up. 
A cycle of the words is consumed to track the replacement values.
It is assumed that there are no leading/trailing/consecutive spaces (easy to cover with a .filter()).

```zkl
sets:=T(T("the cat sat on the mat","mat cat"),
        T("the cat sat on the mat","cat mat"),
        T("A B C A B C A B C","C A C A"),
        T("A B C A B D A B E","E A D A"),
        T("A B","B"), T("A B","B A"), T("A B B A","B A") );
foreach m,n in (sets){
   m.println(" / ",n," --> ",disOrder(m,n));
}
```

```txt

the cat sat on the mat / mat cat --> the mat sat on the cat
the cat sat on the mat / cat mat --> the cat sat on the mat
A B C A B C A B C / C A C A --> C B A C B A A B C
A B C A B D A B E / E A D A --> E B C A B D A B A
A B / B --> A B
A B / B A --> B A
A B B A / B A --> B A B A

```

