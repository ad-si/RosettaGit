+++
title = "Cartesian product of two or more lists"
description = ""
date = 2019-10-17T21:23:27Z
aliases = []
[extra]
id = 21394
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "applescript",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "factor",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "maple",
  "mathematica",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "sql",
  "standard_ml",
  "stata",
  "swift",
  "tailspin",
  "tcl",
  "visual_basic_.net",
  "zkl",
]
+++

## Task
### Task:
Show one or more idiomatic ways of generating the [[wp:Cartesian_product|Cartesian product]] of two arbitrary lists in your language.

Demonstrate that your function/method correctly returns:
::{1, 2} × {3, 4} = {(1, 3), (1, 4), (2, 3), (2, 4)}

and, in contrast:
::{3, 4} × {1, 2} = {(3, 1), (3, 2), (4, 1), (4, 2)}

Also demonstrate, using your function/method, that the product of an empty list with any other list is empty.
:: {1, 2} × {} = {}
:: {} × {1, 2} = {}

For extra credit, show or write a function returning the n-ary product of an arbitrary number of lists, each of arbitrary length. Your function might, for example, accept a single argument which is itself a list of lists, and return the n-ary product of those lists.

Use your [[wp:Cartesian_product#Finite_n-ary_product|n-ary Cartesian product]] function to show the following products:
:: {1776, 1789} × {7, 12} × {4, 14, 23} × {0, 1}
:: {1, 2, 3} × {30} × {500, 100}
:: {1, 2, 3} × {} × {500, 100}





## 11l

{{trans|Go}}

```11l
F cart_prod(a, b)
   V p = [(0, 0)] * (a.len * b.len)
   V i = 0
   L(aa) a
      L(bb) b
         p[i++] = (aa, bb)
   R p

print(cart_prod([1, 2], [3, 4]))
print(cart_prod([3, 4], [1, 2]))
[Int] empty_array
print(cart_prod([1, 2], empty_array))
print(cart_prod(empty_array, [1, 2]))
```


### =Alternative version=


```11l
F cart_prod(a, b)
   R multiloop(a, b, (aa, bb) -> (aa, bb))
```

{{out}}

```txt

[(1, 3), (1, 4), (2, 3), (2, 4)]
[(3, 1), (3, 2), (4, 1), (4, 2)]
[]
[]

```



## AppleScript


```AppleScript
-- CARTESIAN PRODUCTS ---------------------------------------------------------

-- Two lists:

-- cartProd :: [a] -> [b] -> [(a, b)]
on cartProd(xs, ys)
    script
        on |λ|(x)
            script
                on |λ|(y)
                    [[x, y]]
                end |λ|
            end script
            concatMap(result, ys)
        end |λ|
    end script
    concatMap(result, xs)
end cartProd

-- N-ary – a function over a list of lists:

-- cartProdNary :: [[a]] -> [[a]]
on cartProdNary(xss)
    script
        on |λ|(accs, xs)
            script
                on |λ|(x)
                    script
                        on |λ|(a)
                            {x & a}
                        end |λ|
                    end script
                    concatMap(result, accs)
                end |λ|
            end script
            concatMap(result, xs)
        end |λ|
    end script
    foldr(result, {{}}, xss)
end cartProdNary

-- TESTS ----------------------------------------------------------------------
on run
    set baseExamples to unlines(map(show, ¬
        [cartProd({1, 2}, {3, 4}), ¬
            cartProd({3, 4}, {1, 2}), ¬
            cartProd({1, 2}, {}), ¬
            cartProd({}, {1, 2})]))

    set naryA to unlines(map(show, ¬
        cartProdNary([{1776, 1789}, {7, 12}, {4, 14, 23}, {0, 1}])))

    set naryB to show(cartProdNary([{1, 2, 3}, {30}, {500, 100}]))

    set naryC to show(cartProdNary([{1, 2, 3}, {}, {500, 100}]))

    intercalate(linefeed & linefeed, {baseExamples, naryA, naryB, naryC})
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lst to {}
    set lng to length of xs
    tell mReturn(f)
        repeat with i from 1 to lng
            set lst to (lst & |λ|(item i of xs, i, xs))
        end repeat
    end tell
    return lst
end concatMap

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldr

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- show :: a -> String
on show(e)
    set c to class of e
    if c = list then
        script serialized
            on |λ|(v)
                show(v)
            end |λ|
        end script

        "[" & intercalate(", ", map(serialized, e)) & "]"
    else if c = record then
        script showField
            on |λ|(kv)
                set {k, ev} to kv
                "\"" & k & "\":" & show(ev)
            end |λ|
        end script

        "{" & intercalate(", ", ¬
            map(showField, zip(allKeys(e), allValues(e)))) & "}"
    else if c = date then
        "\"" & iso8601Z(e) & "\""
    else if c = text then
        "\"" & e & "\""
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

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines
```

{{Out}}

```txt
[[1, 3], [1, 4], [2, 3], [2, 4]]
[[3, 1], [3, 2], [4, 1], [4, 2]]
[]
[]

[1776, 7, 4, 0]
[1776, 7, 4, 1]
[1776, 7, 14, 0]
[1776, 7, 14, 1]
[1776, 7, 23, 0]
[1776, 7, 23, 1]
[1776, 12, 4, 0]
[1776, 12, 4, 1]
[1776, 12, 14, 0]
[1776, 12, 14, 1]
[1776, 12, 23, 0]
[1776, 12, 23, 1]
[1789, 7, 4, 0]
[1789, 7, 4, 1]
[1789, 7, 14, 0]
[1789, 7, 14, 1]
[1789, 7, 23, 0]
[1789, 7, 23, 1]
[1789, 12, 4, 0]
[1789, 12, 4, 1]
[1789, 12, 14, 0]
[1789, 12, 14, 1]
[1789, 12, 23, 0]
[1789, 12, 23, 1]

[[1, 30, 500], [1, 30, 100], [2, 30, 500], [2, 30, 100], [3, 30, 500], [3, 30, 100]]

[]
```



## C

Recursive implementation for computing the Cartesian product of lists. In the pursuit of making it as interactive as possible, the parsing function ended up taking the most space. The product set expression must be supplied enclosed by double quotes. Prints out usage on incorrect invocation.

```C

#include<string.h>
#include<stdlib.h>
#include<stdio.h>

void cartesianProduct(int** sets, int* setLengths, int* currentSet, int numSets, int times){
	int i,j;

	if(times==numSets){
		printf("(");
		for(i=0;i<times;i++){
			printf("%d,",currentSet[i]);
		}
		printf("\b),");
	}
	else{
		for(j=0;j<setLengths[times];j++){
			currentSet[times] = sets[times][j];
			cartesianProduct(sets,setLengths,currentSet,numSets,times+1);
		}
	}
}

void printSets(int** sets, int* setLengths, int numSets){
	int i,j;

	printf("\nNumber of sets : %d",numSets);

	for(i=0;i<numSets+1;i++){
		printf("\nSet %d : ",i+1);
		for(j=0;j<setLengths[i];j++){
			printf(" %d ",sets[i][j]);
		}
	}
}

void processInputString(char* str){
	int **sets, *currentSet, *setLengths, setLength, numSets = 0, i,j,k,l,start,counter=0;
	char *token,*holder,*holderToken;

	for(i=0;str[i]!=00;i++)
		if(str[i]=='x')
			numSets++;

	if(numSets==0){
			printf("\n%s",str);
			return;
	}

	currentSet = (int*)calloc(sizeof(int),numSets + 1);

	setLengths = (int*)calloc(sizeof(int),numSets + 1);

	sets = (int**)malloc((numSets + 1)*sizeof(int*));

	token = strtok(str,"x");

	while(token!=NULL){
		holder = (char*)malloc(strlen(token)*sizeof(char));

		j = 0;

		for(i=0;token[i]!=00;i++){
			if(token[i]>='0' && token[i]<='9')
				holder[j++] = token[i];
			else if(token[i]==',')
				holder[j++] = ' ';
		}
		holder[j] = 00;

		setLength = 0;

		for(i=0;holder[i]!=00;i++)
			if(holder[i]==' ')
				setLength++;

		if(setLength==0 && strlen(holder)==0){
			printf("\n{}");
			return;
		}

		setLengths[counter] = setLength+1;

		sets[counter] = (int*)malloc((1+setLength)*sizeof(int));

		k = 0;

		start = 0;

		for(l=0;holder[l]!=00;l++){
			if(holder[l+1]==' '||holder[l+1]==00){
				holderToken = (char*)malloc((l+1-start)*sizeof(char));
				strncpy(holderToken,holder + start,l+1-start);
				sets[counter][k++] = atoi(holderToken);
				start = l+2;
			}
		}

		counter++;
		token = strtok(NULL,"x");
	}

	printf("\n{");
	cartesianProduct(sets,setLengths,currentSet,numSets + 1,0);
	printf("\b}");

}

int main(int argC,char* argV[])
{
	if(argC!=2)
		printf("Usage : %s <Set product expression enclosed in double quotes>",argV[0]);
	else
		processInputString(argV[1]);

	return 0;
}

```

Invocation and output :

```txt

C:\My Projects\threeJS>cartesianProduct.exe "{1,2} x {3,4}"

{(1,3),(1,4),(2,3),(2,4)}
C:\My Projects\threeJS>cartesianProduct.exe "{3,4} x {1,2}"

{(3,1),(3,2),(4,1),(4,2)}
C:\My Projects\threeJS>cartesianProduct.exe "{1,2} x {}"

{}
C:\My Projects\threeJS>cartesianProduct.exe "{} x {1,2}"

{}
C:\My Projects\threeJS>cartesianProduct.exe "{1776, 1789} x {7, 12} x {4, 14, 23} x {0, 1}"

{(1776,7,4,0),(1776,7,4,1),(1776,7,14,0),(1776,7,14,1),(1776,7,23,0),(1776,7,23,1),(1776,12,4,0),(1776,12,4,1),(1776,12,14,0),(1776,12,14,1),(1776,12,23,0),(1776,12,23,1),(1789,7,4,0),(1789,9,12,14,1),(1789,12,23,0),(1789,12,23,1)}
C:\My Projects\threeJS>cartesianProduct.exe "{1, 2, 3} x {30} x {500, 100}"

{(1,30,500),(1,30,100),(2,30,500),(2,30,100),(3,30,500),(3,30,100)}
C:\My Projects\threeJS>cartesianProduct.exe "{1, 2, 3} x {} x {500, 100}"

{}

```



## C++


```cpp

#include <iostream>
#include <vector>
#include <algorithm>

void print(const std::vector<std::vector<int>>& v) {
  std::cout << "{ ";
  for (const auto& p : v) {
    std::cout << "(";
    for (const auto& e : p) {
      std::cout << e << " ";
    }
    std::cout << ") ";
  }
  std::cout << "}" << std::endl;
}

auto product(const std::vector<std::vector<int>>& lists) {
  std::vector<std::vector<int>> result;
  if (std::find_if(std::begin(lists), std::end(lists),
    [](auto e) -> bool { return e.size() == 0; }) != std::end(lists)) {
    return result;
  }
  for (auto& e : lists[0]) {
    result.push_back({ e });
  }
  for (size_t i = 1; i < lists.size(); ++i) {
    std::vector<std::vector<int>> temp;
    for (auto& e : result) {
      for (auto f : lists[i]) {
        auto e_tmp = e;
        e_tmp.push_back(f);
        temp.push_back(e_tmp);
      }
    }
    result = temp;
  }
  return result;
}

int main() {
  std::vector<std::vector<int>> prods[] = {
    { { 1, 2 }, { 3, 4 } },
    { { 3, 4 }, { 1, 2} },
    { { 1, 2 }, { } },
    { { }, { 1, 2 } },
    { { 1776, 1789 }, { 7, 12 }, { 4, 14, 23 }, { 0, 1 } },
    { { 1, 2, 3 }, { 30 }, { 500, 100 } },
    { { 1, 2, 3 }, { }, { 500, 100 } }
  };
  for (const auto& p : prods) {
    print(product(p));
  }
  std::cin.ignore();
  std::cin.get();
  return 0;
}
```


{{out}}

```txt
{ (1 3) (1 4) (2 3) (2 4) }
{ (3 1) (3 2) (4 1) (4 2) }
{ }
{ }
{ (1776 7 4 0) (1776 7 4 1) (1776 7 14 0) (1776 7 14 1) (1776 7 23 0) (1776 7 23 1) (1776 12 4 0) (1776 12 4 1) (1776 12 14 0) (1776 12 14 1) (1776 12 23 0) (1776 12 23 1) (1789 7 4 0) (1789 7 4 1) (1789 7 14 0) (1789 7 14 1) (1789 7 23 0) (1789 7 23 1) (1789 12 4 0) (1789 12 4 1) (1789 12 14 0) (1789 12 14 1) (1789 12 23 0) (1789 12 23 1) }
{ (1 30 500) (1 30 100) (2 30 500) (2 30 100) (3 30 500) (3 30 100) }
{ }
```



## C#


```c#
using System;
public class Program
{
    public static void Main()
    {
        int[] empty = new int[0];
        int[] list1 = { 1, 2 };
        int[] list2 = { 3, 4 };
        int[] list3 = { 1776, 1789 };
        int[] list4 = { 7, 12 };
        int[] list5 = { 4, 14, 23 };
        int[] list6 = { 0, 1 };
        int[] list7 = { 1, 2, 3 };
        int[] list8 = { 30 };
        int[] list9 = { 500, 100 };

        foreach (var sequenceList in new [] {
            new [] { list1, list2 },
            new [] { list2, list1 },
            new [] { list1, empty },
            new [] { empty, list1 },
            new [] { list3, list4, list5, list6 },
            new [] { list7, list8, list9 },
            new [] { list7, empty, list9 }
        }) {
            var cart = sequenceList.CartesianProduct()
                .Select(tuple => $"({string.Join(", ", tuple)})");
            Console.WriteLine($"{{{string.Join(", ", cart)}}}");
        }
    }
}

public static class Extensions
{
    public static IEnumerable<IEnumerable<T>> CartesianProduct<T>(this IEnumerable<IEnumerable<T>> sequences) {
        IEnumerable<IEnumerable<T>> emptyProduct = new[] { Enumerable.Empty<T>() };
        return sequences.Aggregate(
            emptyProduct,
            (accumulator, sequence) =>
            from acc in accumulator
            from item in sequence
            select acc.Concat(new [] { item }));
    }
}
```

{{out}}

```txt

{(1, 3), (1, 4), (2, 3), (2, 4)}
{(3, 1), (3, 2), (4, 1), (4, 2)}
{}
{}
{(1776, 7, 4, 0), (1776, 7, 4, 1), (1776, 7, 14, 0), (1776, 7, 14, 1), (1776, 7, 23, 0), (1776, 7, 23, 1), (1776, 12, 4, 0), (1776, 12, 4, 1), (1776, 12, 14, 0), (1776, 12, 14, 1), (1776, 12, 23, 0), (1776, 12, 23, 1), (1789, 7, 4, 0), (1789, 7, 4, 1), (1789, 7, 14, 0), (1789, 7, 14, 1), (1789, 7, 23, 0), (1789, 7, 23, 1), (1789, 12, 4, 0), (1789, 12, 4, 1), (1789, 12, 14, 0), (1789, 12, 14, 1), (1789, 12, 23, 0), (1789, 12, 23, 1)}
{(1, 30, 500), (1, 30, 100), (2, 30, 500), (2, 30, 100), (3, 30, 500), (3, 30, 100)}
{}
```

If the number of lists is known, LINQ provides an easier solution:

```c#
public static void Main()
{
    ///...
    var cart1 =
        from a in list1
        from b in list2
        select (a, b); // C# 7.0 tuple
    Console.WriteLine($"{{{string.Join(", ", cart1)}}}");

    var cart2 =
        from a in list7
        from b in list8
        from c in list9
        select (a, b, c);
    Console.WriteLine($"{{{string.Join(", ", cart2)}}}");
}
```

{{out}}

```txt

{(1, 3), (1, 4), (2, 3), (2, 4)}
{(1, 30, 500), (1, 30, 100), (2, 30, 500), (2, 30, 100), (3, 30, 500), (3, 30, 100)}

```



## Common Lisp


```lisp
(defun cartesian-product (s1 s2)
  "Compute the cartesian product of two sets represented as lists"
  (loop for x in s1
	nconc (loop for y in s2 collect (list x y))))

```


'''Output'''


```lisp

CL-USER> (cartesian-product '(1 2) '(3 4))
((1 3) (1 4) (2 3) (2 4))
CL-USER> (cartesian-product '(3 4) '(1 2))
((3 1) (3 2) (4 1) (4 2))
CL-USER> (cartesian-product '(1 2) '())
NIL
CL-USER> (cartesian-product '() '(1 2))
NIL

```


'''Extra credit:'''


```lisp
(defun n-cartesian-product (l)
  "Compute the n-cartesian product of a list of sets (each of them represented as list).
   Algorithm:
     If there are no sets, then produce an empty set of tuples;
     otherwise, for all the elements x of the first set, concatenate the sets obtained by
     inserting x at the beginning of each tuple of the n-cartesian product of the remaining sets."
  (if (null l)
      (list nil)
      (loop for x in (car l)
            nconc (loop for y in (n-cartesian-product (cdr l))
                        collect (cons x y)))))
```


'''Output:'''


```lisp
CL-USER> (n-cartesian-product '((1776 1789) (7 12) (4 14 23) (0 1)))
((1776 7 4 0) (1776 7 4 1) (1776 7 14 0) (1776 7 14 1) (1776 7 23 0) (1776 7 23 1) (1776 12 4 0) (1776 12 4 1) (1776 12 14 0) (1776 12 14 1) (1776 12 23 0) (1776 12 23 1) (1789 7 4 0) (1789 7 4 1) (1789 7 14 0) (1789 7 14 1) (1789 7 23 0) (1789 7 23 1) (1789 12 4 0) (1789 12 4 1) (1789 12 14 0) (1789 12 14 1) (1789 12 23 0) (1789 12 23 1))
CL-USER> (n-cartesian-product '((1 2 3) (30) (500 100)))
((1 30 500) (1 30 100) (2 30 500) (2 30 100) (3 30 500) (3 30 100))
CL-USER> (n-cartesian-product '((1 2 3) () (500 100)))
NIL

```



## D


```D
import std.stdio;

void main() {
    auto a = listProduct([1,2], [3,4]);
    writeln(a);

    auto b = listProduct([3,4], [1,2]);
    writeln(b);

    auto c = listProduct([1,2], []);
    writeln(c);

    auto d = listProduct([], [1,2]);
    writeln(d);
}

auto listProduct(T)(T[] ta, T[] tb) {
    struct Result {
        int i, j;

        bool empty() {
            return i>=ta.length
                || j>=tb.length;
        }

        T[] front() {
            return [ta[i], tb[j]];
        }

        void popFront() {
            if (++j>=tb.length) {
                j=0;
                i++;
            }
        }
    }

    return Result();
}
```


{{out}}

```txt
[[1, 3], [1, 4], [2, 3], [2, 4]]
[[3, 1], [3, 2], [4, 1], [4, 2]]
[]
[]
```


=={{header|F Sharp|F#}}==

### The Task


```fsharp

//Nigel Galloway February 12th., 2018
let cP2 n g = List.map (fun (n,g)->[n;g]) (List.allPairs n g)

```

{{out}}

```txt

cP2 [1;2] [3;4] -> [[1; 3]; [1; 4]; [2; 3]; [2; 4]]
cP2 [3;4] [1;2] -> [[3; 1]; [3; 2]; [4; 1]; [4; 2]]
cP2 [1;2] []    -> []
cP2 [] [1;2]    -> []

```



### Extra Credit


```fsharp

//Nigel Galloway August 14th., 2018
let cP ng=Seq.foldBack(fun n g->[for n' in n do for g' in g do yield n'::g']) ng [[]]

```

{{out}}

```txt

cP [[1;2];[3;4]] -> [[1; 3]; [1; 4]; [2; 3]; [2; 4]]
cP [[3;4];[1;2]] -> [[3; 1]; [3; 2]; [4; 1]; [4; 2]]
cP [[3;4];[]] ->[]
cP [[];[1;2]] ->[]
cP [[1776;1789];[7;12];[4;14;23];[0;1]] -> [[1776; 7; 4; 0]; [1776; 7; 4; 1]; [1776; 7; 14; 0]; [1776; 7; 14; 1];
                                            [1776; 7; 23; 0]; [1776; 7; 23; 1]; [1776; 12; 4; 0]; [1776; 12; 4; 1];
                                            [1776; 12; 14; 0]; [1776; 12; 14; 1]; [1776; 12; 23; 0]; [1776; 12; 23; 1];
                                            [1789; 7; 4; 0]; [1789; 7; 4; 1]; [1789; 7; 14; 0]; [1789; 7; 14; 1];
                                            [1789; 7; 23; 0]; [1789; 7; 23; 1]; [1789; 12; 4; 0]; [1789; 12; 4; 1];
                                            [1789; 12; 14; 0]; [1789; 12; 14; 1]; [1789; 12; 23; 0]; [1789; 12; 23; 1]]
cP [[1;2;3];[30];[500;100]] -> [[1; 30; 500]; [1; 30; 100]; [2; 30; 500]; [2; 30; 100]; [3; 30; 500]; [3; 30; 100]]
cP [[1;2;3];[];[500;100]] -> []

```



## Factor


```Factor
IN: scratchpad { 1 2 } { 3 4 } cartesian-product .
{ { { 1 3 } { 1 4 } } { { 2 3 } { 2 4 } } }
IN: scratchpad { 3 4 } { 1 2 } cartesian-product .
{ { { 3 1 } { 3 2 } } { { 4 1 } { 4 2 } } }
IN: scratchpad { 1 2 } { } cartesian-product .
{ { } { } }
IN: scratchpad { } { 1 2 } cartesian-product .
{ }
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Cartesian_product_of_two_or_more_lists this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go

'''Basic Task'''

```go
package main

import "fmt"

type pair [2]int

func cart2(a, b []int) []pair {
    p := make([]pair, len(a)*len(b))
    i := 0
    for _, a := range a {
        for _, b := range b {
            p[i] = pair{a, b}
            i++
        }
    }
    return p
}

func main() {
    fmt.Println(cart2([]int{1, 2}, []int{3, 4}))
    fmt.Println(cart2([]int{3, 4}, []int{1, 2}))
    fmt.Println(cart2([]int{1, 2}, nil))
    fmt.Println(cart2(nil, []int{1, 2}))
}
```

{{out}}

```txt

[[1 3] [1 4] [2 3] [2 4]]
[[3 1] [3 2] [4 1] [4 2]]
[]
[]

```

'''Extra credit 1'''

This solution minimizes allocations and computes and fills the result sequentially.

```go
package main

import "fmt"

func cartN(a ...[]int) [][]int {
    c := 1
    for _, a := range a {
        c *= len(a)
    }
    if c == 0 {
        return nil
    }
    p := make([][]int, c)
    b := make([]int, c*len(a))
    n := make([]int, len(a))
    s := 0
    for i := range p {
        e := s + len(a)
        pi := b[s:e]
        p[i] = pi
        s = e
        for j, n := range n {
            pi[j] = a[j][n]
        }
        for j := len(n) - 1; j >= 0; j-- {
            n[j]++
            if n[j] < len(a[j]) {
                break
            }
            n[j] = 0
        }
    }
    return p
}

func main() {
    fmt.Println(cartN([]int{1, 2}, []int{3, 4}))
    fmt.Println(cartN([]int{3, 4}, []int{1, 2}))
    fmt.Println(cartN([]int{1, 2}, nil))
    fmt.Println(cartN(nil, []int{1, 2}))

    fmt.Println()
    fmt.Println("[")
    for _, p := range cartN(
        []int{1776, 1789},
        []int{7, 12},
        []int{4, 14, 23},
        []int{0, 1},
    ) {
        fmt.Println(" ", p)
    }
    fmt.Println("]")
    fmt.Println(cartN([]int{1, 2, 3}, []int{30}, []int{500, 100}))
    fmt.Println(cartN([]int{1, 2, 3}, []int{}, []int{500, 100}))

    fmt.Println()
    fmt.Println(cartN(nil))
    fmt.Println(cartN())
}
```

{{out}}

```txt

[[1 3] [1 4] [2 3] [2 4]]
[[3 1] [3 2] [4 1] [4 2]]
[]
[]

[
  [1776 7 4 0]
  [1776 7 4 1]
  [1776 7 14 0]
  [1776 7 14 1]
  [1776 7 23 0]
  [1776 7 23 1]
  [1776 12 4 0]
  [1776 12 4 1]
  [1776 12 14 0]
  [1776 12 14 1]
  [1776 12 23 0]
  [1776 12 23 1]
  [1789 7 4 0]
  [1789 7 4 1]
  [1789 7 14 0]
  [1789 7 14 1]
  [1789 7 23 0]
  [1789 7 23 1]
  [1789 12 4 0]
  [1789 12 4 1]
  [1789 12 14 0]
  [1789 12 14 1]
  [1789 12 23 0]
  [1789 12 23 1]
]
[[1 30 500] [1 30 100] [2 30 500] [2 30 100] [3 30 500] [3 30 100]]
[]

[]
[[]]

```

'''Extra credit 2'''

Code here is more compact, but with the cost of more garbage produced.  It produces the same result as cartN above.

```go
func cartN(a ...[]int) (c [][]int) {
    if len(a) == 0 {
        return [][]int{nil}
    }
    r := cartN(a[1:]...)
    for _, e := range a[0] {
        for _, p := range r {
            c = append(c, append([]int{e}, p...))
        }
    }
    return
}
```

'''Extra credit 3'''

This is a compact recursive version like Extra credit 2 but the result list is ordered differently.  This is still a correct result if you consider a cartesian product to be a set, which is an unordered collection.  Note that the set elements are still ordered lists.  A cartesian product is an unordered collection of ordered collections.  It draws attention though to the gloss of using list representations as sets.  Any of the functions here will accept duplicate elements in the input lists, and then produce duplicate elements in the result.

```go
func cartN(a ...[]int) (c [][]int) {
    if len(a) == 0 {
        return [][]int{nil}
    }
    last := len(a) - 1
    l := cartN(a[:last]...)
    for _, e := range a[last] {
        for _, p := range l {
            c = append(c, append(p, e))
        }
    }
    return
}
```



## Groovy

'''Solution:'''

The following ''CartesianCategory'' class allows for modification of regular ''Iterable'' interface behavior, overloading ''Iterable'''s ''multiply'' (*) operator to perform a Cartesian Product when the second operand is also an ''Iterable''.

```groovy
class CartesianCategory {
    static Iterable multiply(Iterable a, Iterable b) {
        assert [a,b].every { it != null }
        def (m,n) = [a.size(),b.size()]
        (0..<(m*n)).inject([]) { prod, i -> prod << [a[i.intdiv(n)], b[i%n]].flatten() }
    }
}
```

'''Test:'''

The ''mixin'' method call is necessary to make the multiply (*) operator work.

```groovy
Iterable.metaClass.mixin CartesianCategory

println "\nCore Solution:"
println "[1, 2] × [3, 4] = ${[1, 2] * [3, 4]}"
println "[3, 4] × [1, 2] = ${[3, 4] * [1, 2]}"
println "[1, 2] × [] = ${[1, 2] * []}"
println "[] × [1, 2] = ${[] * [1, 2]}"

println "\nExtra Credit:"
println "[1776, 1789] × [7, 12] × [4, 14, 23] × [0, 1] = ${[1776, 1789] * [7, 12] * [4, 14, 23] * [0, 1]}"
println "[1, 2, 3] × [30] × [500, 100] = ${[1, 2, 3] * [30] * [500, 100]}"
println "[1, 2, 3] × [] × [500, 100] = ${[1, 2, 3] * [] * [500, 100]}"

println "\nNon-Numeric Example:"
println "[John,Paul,George,Ringo] × [Emerson,Lake,Palmer] × [Simon,Garfunkle] = ["
( ["John","Paul","George","Ringo"] * ["Emerson","Lake","Palmer"] * ["Simon","Garfunkle"] ).each { println "\t${it}," }
println "]"
```

'''Output:'''

```txt

Core Solution:
[1, 2] × [3, 4] = [[1, 3], [1, 4], [2, 3], [2, 4]]
[3, 4] × [1, 2] = [[3, 1], [3, 2], [4, 1], [4, 2]]
[1, 2] × [] = []
[] × [1, 2] = []

Extra Credit:
[1776, 1789] × [7, 12] × [4, 14, 23] × [0, 1] = [[1776, 7, 4, 0], [1776, 7, 4, 1], [1776, 7, 14, 0], [1776, 7, 14, 1], [1776, 7, 23, 0], [1776, 7, 23, 1], [1776, 12, 4, 0], [1776, 12, 4, 1], [1776, 12, 14, 0], [1776, 12, 14, 1], [1776, 12, 23, 0], [1776, 12, 23, 1], [1789, 7, 4, 0], [1789, 7, 4, 1], [1789, 7, 14, 0], [1789, 7, 14, 1], [1789, 7, 23, 0], [1789, 7, 23, 1], [1789, 12, 4, 0], [1789, 12, 4, 1], [1789, 12, 14, 0], [1789, 12, 14, 1], [1789, 12, 23, 0], [1789, 12, 23, 1]]
[1, 2, 3] × [30] × [500, 100] = [[1, 30, 500], [1, 30, 100], [2, 30, 500], [2, 30, 100], [3, 30, 500], [3, 30, 100]]
[1, 2, 3] × [] × [500, 100] = []

Non-Numeric Example:
[John,Paul,George,Ringo] × [Emerson,Lake,Palmer] × [Simon,Garfunkle] = [
	[John, Emerson, Simon],
	[John, Emerson, Garfunkle],
	[John, Lake, Simon],
	[John, Lake, Garfunkle],
	[John, Palmer, Simon],
	[John, Palmer, Garfunkle],
	[Paul, Emerson, Simon],
	[Paul, Emerson, Garfunkle],
	[Paul, Lake, Simon],
	[Paul, Lake, Garfunkle],
	[Paul, Palmer, Simon],
	[Paul, Palmer, Garfunkle],
	[George, Emerson, Simon],
	[George, Emerson, Garfunkle],
	[George, Lake, Simon],
	[George, Lake, Garfunkle],
	[George, Palmer, Simon],
	[George, Palmer, Garfunkle],
	[Ringo, Emerson, Simon],
	[Ringo, Emerson, Garfunkle],
	[Ringo, Lake, Simon],
	[Ringo, Lake, Garfunkle],
	[Ringo, Palmer, Simon],
	[Ringo, Palmer, Garfunkle],
]
```



## Haskell

Various routes can be taken to Cartesian products in Haskell.
For the product of two lists we could write:

```Haskell
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys =
  [ (x, y)
  | x <- xs
  , y <- ys ]
```


more directly:

```Haskell
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = xs >>= \x -> ys >>= \y -> [(x, y)]
```


applicatively:

```Haskell
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = (,) <$> xs <*> ys
```


parsimoniously:

```Haskell
cartProd :: [a] -> [b] -> [(a, b)]
cartProd = (<*>) . fmap (,)
```


We might test any of these with:

```haskell
main :: IO ()
main =
  mapM_ print $
  uncurry cartProd <$>
  [([1, 2], [3, 4]), ([3, 4], [1, 2]), ([1, 2], []), ([], [1, 2])]
```

{{Out}}

```txt
[(1,3),(1,4),(2,3),(2,4)]
[(3,1),(3,2),(4,1),(4,2)]
[]
[]
```



For the n-ary Cartesian product of an arbitrary number of lists, we could apply the Prelude's standard '''sequence''' function to a list of lists,

```haskell
cartProdN :: [[a]] -> [[a]]
cartProdN = sequence

main :: IO ()
main = print $ cartProdN [[1, 2], [3, 4], [5, 6]]
```

{{Out}}

```txt
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
```


or we could define ourselves an equivalent function over a list of lists in terms of a fold, for example as:

```haskell
cartProdN :: [[a]] -> [[a]]
cartProdN = foldr (\xs as -> xs >>= (<$> as) . (:)) [[]]
```

or, equivalently, as:

```haskell
cartProdN :: [[a]] -> [[a]]
cartProdN = foldr
    (\xs as ->
        [ x : a
        | x <- xs
        , a <- as ])
    [[]]
```

testing any of these with something like:

```haskell
main :: IO ()
main = do
  mapM_ print $
    cartProdN [[1776, 1789], [7,12], [4, 14, 23], [0,1]]
  putStrLn ""
  print $ cartProdN [[1,2,3], [30], [500, 100]]
  putStrLn ""
  print $ cartProdN [[1,2,3], [], [500, 100]]
```

{{Out}}

```txt
[1776,7,4,0]
[1776,7,4,1]
[1776,7,14,0]
[1776,7,14,1]
[1776,7,23,0]
[1776,7,23,1]
[1776,12,4,0]
[1776,12,4,1]
[1776,12,14,0]
[1776,12,14,1]
[1776,12,23,0]
[1776,12,23,1]
[1789,7,4,0]
[1789,7,4,1]
[1789,7,14,0]
[1789,7,14,1]
[1789,7,23,0]
[1789,7,23,1]
[1789,12,4,0]
[1789,12,4,1]
[1789,12,14,0]
[1789,12,14,1]
[1789,12,23,0]
[1789,12,23,1]

[[1,30,500],[1,30,100],[2,30,500],[2,30,100],[3,30,500],[3,30,100]]

[]
```



## J

The J primitive [http://code.jsoftware.com/wiki/Vocabulary/curlylf catalogue] <code>{</code> forms the Cartesian Product of two or more boxed lists. The result is a multi-dimensional array (which can be reshaped to a simple list of lists if desired).

```j
   { 1776 1789 ; 7 12 ; 4 14 23 ; 0 1   NB. result is 4 dimensional array with shape 2 2 3 2
┌────────────┬────────────┐
│1776 7 4 0  │1776 7 4 1  │
├────────────┼────────────┤
│1776 7 14 0 │1776 7 14 1 │
├────────────┼────────────┤
│1776 7 23 0 │1776 7 23 1 │
└────────────┴────────────┘

┌────────────┬────────────┐
│1776 12 4 0 │1776 12 4 1 │
├────────────┼────────────┤
│1776 12 14 0│1776 12 14 1│
├────────────┼────────────┤
│1776 12 23 0│1776 12 23 1│
└────────────┴────────────┘


┌────────────┬────────────┐
│1789 7 4 0  │1789 7 4 1  │
├────────────┼────────────┤
│1789 7 14 0 │1789 7 14 1 │
├────────────┼────────────┤
│1789 7 23 0 │1789 7 23 1 │
└────────────┴────────────┘

┌────────────┬────────────┐
│1789 12 4 0 │1789 12 4 1 │
├────────────┼────────────┤
│1789 12 14 0│1789 12 14 1│
├────────────┼────────────┤
│1789 12 23 0│1789 12 23 1│
└────────────┴────────────┘
   { 1 2 3 ; 30 ; 50 100    NB. result is a 2-dimensional array with shape 2 3
┌───────┬────────┐
│1 30 50│1 30 100│
├───────┼────────┤
│2 30 50│2 30 100│
├───────┼────────┤
│3 30 50│3 30 100│
└───────┴────────┘
   { 1 2 3 ; '' ; 50 100    NB. result is an empty 3-dimensional array with shape 3 0 2

```


## Java

{{works with|Java Virtual Machine|1.8}}

```Java

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Optional.of;
import static java.util.stream.Collectors.toList;

import java.util.List;

public class CartesianProduct {

    public List<?> product(List<?>... a) {
        if (a.length >= 2) {
            List<?> product = a[0];
            for (int i = 1; i < a.length; i++) {
                product = product(product, a[i]);
            }
            return product;
        }

        return emptyList();
    }

    private <A, B> List<?> product(List<A> a, List<B> b) {
        return of(a.stream()
                .map(e1 -> of(b.stream().map(e2 -> asList(e1, e2)).collect(toList())).orElse(emptyList()))
                .flatMap(List::stream)
                .collect(toList())).orElse(emptyList());
    }
}

```



## JavaScript


### ES6


### =Functional=

Cartesian products fall quite naturally out of '''concatMap''' (Array.flatMap), and its argument-flipped twin '''bind'''.

For the Cartesian product of just two lists:

```JavaScript
(() => {
    // CARTESIAN PRODUCT OF TWO LISTS ---------------------

    // cartProd :: [a] -> [b] -> [[a, b]]
    const cartProd = xs => ys =>
        xs.flatMap(x => ys.map(y => [x, y]))


    // TEST -----------------------------------------------
    return [
        cartProd([1, 2])([3, 4]),
        cartProd([3, 4])([1, 2]),
        cartProd([1, 2])([]),
        cartProd([])([1, 2]),
    ].map(JSON.stringify).join('\n');
})();
```

{{Out}}

```txt
[[1,3],[1,4],[2,3],[2,4]]
[[3,1],[3,2],[4,1],[4,2]]
[]
[]
```



Abstracting a little more, we can define the cartesian product quite economically in terms of a general applicative operator:

```Javascript
(() => {

    // CARTESIAN PRODUCT OF TWO LISTS ---------------------

    // cartesianProduct :: [a] -> [b] -> [(a, b)]
    const cartesianProduct = xs =>
        ap(xs.map(Tuple));


    // GENERIC FUNCTIONS ----------------------------------

    // e.g. [(*2),(/2), sqrt] <*> [1,2,3]
    // -->  ap([dbl, hlf, root], [1, 2, 3])
    // -->  [2,4,6,0.5,1,1.5,1,1.4142135623730951,1.7320508075688772]

    // Each member of a list of functions applied to each
    // of a list of arguments, deriving a list of new values.

    // ap (<*>) :: [(a -> b)] -> [a] -> [b]
    const ap = fs => xs =>
        // The sequential application of each of a list
        // of functions to each of a list of values.
        fs.flatMap(
            f => xs.map(f)
        );

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a => b => [a, b];

    // TEST -----------------------------------------------
    return [
            cartesianProduct([1, 2])([3, 4]),
            cartesianProduct([3, 4])([1, 2]),
            cartesianProduct([1, 2])([]),
            cartesianProduct([])([1, 2]),
        ]
        .map(JSON.stringify)
        .join('\n');
})();
```

{{Out}}

```txt
[[1,3],[1,4],[2,3],[2,4]]
[[3,1],[3,2],[4,1],[4,2]]
[]
[]
```


For the n-ary Cartesian product over a list of lists:

```JavaScript
(() => {
    const main = () => {
        // n-ary Cartesian product of a list of lists.

        // cartProdN :: [[a]] -> [[a]]
        const cartProdN = foldr(
            xs => as =>
            bind(as)(
                x => bind(xs)(
                    a => [
                        [a].concat(x)
                    ]
                )
            )
        )([
            []
        ]);

        // TEST -------------------------------------------
        return intercalate('\n\n')([
            map(show)(
                cartProdN([
                    [1776, 1789],
                    [7, 12],
                    [4, 14, 23],
                    [0, 1]
                ])
            ).join('\n'),
            show(cartProdN([
                [1, 2, 3],
                [30],
                [50, 100]
            ])),
            show(cartProdN([
                [1, 2, 3],
                [],
                [50, 100]
            ]))
        ])
    };

    // GENERIC FUNCTIONS ----------------------------------

    // bind ::  [a] -> (a -> [b]) -> [b]
    const bind = xs => f => xs.flatMap(f);

    // foldr :: (a -> b -> b) -> b -> [a] -> b
    const foldr = f => a => xs =>
        xs.reduceRight((a, x) => f(x)(a), a);

    // intercalate :: String -> [a] -> String
    const intercalate = s => xs => xs.join(s);

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs => xs.map(f);

    // show :: a -> String
    const show = x => JSON.stringify(x);

    return main();
})();
```

{{Out}}

```txt
[1776,7,4,0]
[1776,7,4,1]
[1776,7,14,0]
[1776,7,14,1]
[1776,7,23,0]
[1776,7,23,1]
[1776,12,4,0]
[1776,12,4,1]
[1776,12,14,0]
[1776,12,14,1]
[1776,12,23,0]
[1776,12,23,1]
[1789,7,4,0]
[1789,7,4,1]
[1789,7,14,0]
[1789,7,14,1]
[1789,7,23,0]
[1789,7,23,1]
[1789,12,4,0]
[1789,12,4,1]
[1789,12,14,0]
[1789,12,14,1]
[1789,12,23,0]
[1789,12,23,1]

[[1,30,50],[1,30,100],[2,30,50],[2,30,100],[3,30,50],[3,30,100]]

[]
```



### =Imperative=

Imperative implementations of Cartesian products are inevitably less compact and direct, but we can certainly write an iterative translation of a fold over nested applications of '''bind''' or '''concatMap''':


```JavaScript
(() => {
    // n-ary Cartesian product of a list of lists
    // ( Imperative implementation )

    // cartProd :: [a] -> [b] -> [[a, b]]
    const cartProd = lists => {
        let ps = [],
            acc = [
                []
            ],
            i = lists.length;
        while (i--) {
            let subList = lists[i],
                j = subList.length;
            while (j--) {
                let x = subList[j],
                    k = acc.length;
                while (k--) ps.push([x].concat(acc[k]))
            };
            acc = ps;
            ps = [];
        };
        return acc.reverse();
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // TEST -------------------------------------------------------------------
    return intercalate('\n\n', [show(cartProd([
            [1, 2],
            [3, 4]
        ])),
        show(cartProd([
            [3, 4],
            [1, 2]
        ])),
        show(cartProd([
            [1, 2],
            []
        ])),
        show(cartProd([
            [],
            [1, 2]
        ])),
        unlines(map(show, cartProd([
            [1776, 1789],
            [7, 12],
            [4, 14, 23],
            [0, 1]
        ]))),
        show(cartProd([
            [1, 2, 3],
            [30],
            [50, 100]
        ])),
        show(cartProd([
            [1, 2, 3],
            [],
            [50, 100]
        ]))
    ]);
})();
```

{{Out}}

```txt
[[1,4],[1,3],[2,4],[2,3]]

[[3,2],[3,1],[4,2],[4,1]]

[]

[]

[1776,12,4,1]
[1776,12,4,0]
[1776,12,14,1]
[1776,12,14,0]
[1776,12,23,1]
[1776,12,23,0]
[1776,7,4,1]
[1776,7,4,0]
[1776,7,14,1]
[1776,7,14,0]
[1776,7,23,1]
[1776,7,23,0]
[1789,12,4,1]
[1789,12,4,0]
[1789,12,14,1]
[1789,12,14,0]
[1789,12,23,1]
[1789,12,23,0]
[1789,7,4,1]
[1789,7,4,0]
[1789,7,14,1]
[1789,7,14,0]
[1789,7,23,1]
[1789,7,23,0]

[[1,30,50],[1,30,100],[2,30,50],[2,30,100],[3,30,50],[3,30,100]]

[]
```



## jq


jq is stream-oriented and so we begin by defining a function that will emit a stream of the elements of the Cartesian product of two arrays:

```jq

def products: .[0][] as $x | .[1][] as $y | [$x,$y];

```


To generate an array of these arrays, one would in practice most likely simply write `[products]`, but to comply with the requirements of this article, we can define `product` as:

```jq

def product: [products];

```


For the sake of brevity, two illustrations should suffice:

    [ [1,2], [3,4] ] | products

produces the stream:

```txt

 [1,3]
 [1,4]
 [2,3]
 [2,4]

```


And

```jq

[[1,2], []] | product

```

produces:

```txt

[]

```


===n-way Cartesian Product===
Given an array of two or more arrays as input, `cartesians` as defined here produces a stream of the components of their Cartesian product:


```jq

def cartesians:
  if length <= 2 then products
  else .[0][] as $x
  | (.[1:] | cartesians) as $y
  | [$x] + $y
  end;

```


Again for brevity, in the following, we will just show the number of items in the Cartesian products:

    [ [1776, 1789], [7, 12], [4, 14, 23], [0, 1]] | [cartesians] | length
    # 24

    [[1, 2, 3], [30], [500, 100] ] | [cartesians] | length
    # 6

    [[1, 2, 3], [], [500, 100] ] | [cartesians] | length
    # 0


## Julia


```julia

# Product {1, 2} × {3, 4}
collect(product([1, 2], [3, 4]))
# Product {3, 4} × {1, 2}
collect(product([3, 4], [1, 2]))

# Product {1, 2} × {}
collect(product([1, 2], []))
# Product {} × {1, 2}
collect(product([], [1, 2]))

# Product {1776, 1789} × {7, 12} × {4, 14, 23} × {0, 1}
collect(product([1776, 1789], [7, 12], [4, 14, 23], [0, 1]))
# Product {1, 2, 3} × {30} × {500, 100}
collect(product([1, 2, 3], [30], [500, 100]))
# Product {1, 2, 3} × {} × {500, 100}
collect(product([1, 2, 3], [], [500, 100]))

```



## Kotlin


```scala
// version 1.1.2

fun flattenList(nestList: List<Any>): List<Any> {
    val flatList = mutableListOf<Any>()

    fun flatten(list: List<Any>) {
        for (e in list) {
            if (e !is List<*>)
                flatList.add(e)
            else
                @Suppress("UNCHECKED_CAST")
                flatten(e as List<Any>)
        }
    }

    flatten(nestList)
    return flatList
}

operator fun List<Any>.times(other: List<Any>): List<List<Any>> {
    val prod = mutableListOf<List<Any>>()
    for (e in this) {
        for (f in other) {
            prod.add(listOf(e, f))
        }
    }
    return prod
}

fun nAryCartesianProduct(lists: List<List<Any>>): List<List<Any>> {
    require(lists.size >= 2)
    return lists.drop(2).fold(lists[0] * lists[1]) { cp, ls -> cp * ls }.map { flattenList(it) }
}

fun printNAryProduct(lists: List<List<Any>>) {
    println("${lists.joinToString(" x ")} = ")
    println("[")
    println(nAryCartesianProduct(lists).joinToString("\n    ", "    "))
    println("]\n")
}

fun main(args: Array<String>) {
   println("[1, 2] x [3, 4] = ${listOf(1, 2) * listOf(3, 4)}")
   println("[3, 4] x [1, 2] = ${listOf(3, 4) * listOf(1, 2)}")
   println("[1, 2] x []     = ${listOf(1, 2) * listOf()}")
   println("[]     x [1, 2] = ${listOf<Any>() * listOf(1, 2)}")
   println("[1, a] x [2, b] = ${listOf(1, 'a') * listOf(2, 'b')}")
   println()
   printNAryProduct(listOf(listOf(1776, 1789), listOf(7, 12), listOf(4, 14, 23), listOf(0, 1)))
   printNAryProduct(listOf(listOf(1, 2, 3), listOf(30), listOf(500, 100)))
   printNAryProduct(listOf(listOf(1, 2, 3), listOf<Int>(), listOf(500, 100)))
   printNAryProduct(listOf(listOf(1, 2, 3), listOf(30), listOf('a', 'b')))
}
```


{{out}}

```txt

[1, 2] x [3, 4] = [[1, 3], [1, 4], [2, 3], [2, 4]]
[3, 4] x [1, 2] = [[3, 1], [3, 2], [4, 1], [4, 2]]
[1, 2] x []     = []
[]     x [1, 2] = []
[1, a] x [2, b] = [[1, 2], [1, b], [a, 2], [a, b]]

[1776, 1789] x [7, 12] x [4, 14, 23] x [0, 1] =
[
    [1776, 7, 4, 0]
    [1776, 7, 4, 1]
    [1776, 7, 14, 0]
    [1776, 7, 14, 1]
    [1776, 7, 23, 0]
    [1776, 7, 23, 1]
    [1776, 12, 4, 0]
    [1776, 12, 4, 1]
    [1776, 12, 14, 0]
    [1776, 12, 14, 1]
    [1776, 12, 23, 0]
    [1776, 12, 23, 1]
    [1789, 7, 4, 0]
    [1789, 7, 4, 1]
    [1789, 7, 14, 0]
    [1789, 7, 14, 1]
    [1789, 7, 23, 0]
    [1789, 7, 23, 1]
    [1789, 12, 4, 0]
    [1789, 12, 4, 1]
    [1789, 12, 14, 0]
    [1789, 12, 14, 1]
    [1789, 12, 23, 0]
    [1789, 12, 23, 1]
]

[1, 2, 3] x [30] x [500, 100] =
[
    [1, 30, 500]
    [1, 30, 100]
    [2, 30, 500]
    [2, 30, 100]
    [3, 30, 500]
    [3, 30, 100]
]

[1, 2, 3] x [] x [500, 100] =
[

]

[1, 2, 3] x [30] x [a, b] =
[
    [1, 30, a]
    [1, 30, b]
    [2, 30, a]
    [2, 30, b]
    [3, 30, a]
    [3, 30, b]
]

```



## Lua


###  Functional

An iterator is created to output the product items.

```lua
  local pk,upk = table.pack, table.unpack
  local getn = function(t)return #t end
  local const = function(k)return function(e) return k end end
  local function attachIdx(f)-- one-time-off function modifier
    local idx = 0
    return function(e)idx=idx+1 ; return f(e,idx)end
  end

  local function reduce(t,acc,f)
    for i=1,t.n or #t do acc=f(acc,t[i])end
    return acc
  end
  local function imap(t,f)
    local r = {n=t.n or #t, r=reduce, u=upk, m=imap}
    for i=1,r.n do r[i]=f(t[i])end
    return r
  end

  local function prod(...)
    local ts = pk(...)
    local limit = imap(ts,getn)
    local idx, cnt = imap(limit,const(1)),  0
    local max = reduce(limit,1,function(a,b)return a*b end)
    local function ret(t,i)return t[idx[i]] end
    return function()
      if cnt>=max then return end -- no more output
      if cnt==0 then -- skip for 1st
        cnt = cnt + 1
      else
        cnt, idx[#idx] = cnt + 1, idx[#idx] + 1
        for i=#idx,2,-1 do -- update index list
          if idx[i]<=limit[i] then
            break -- no further update need
          else -- propagate limit overflow
            idx[i],idx[i-1] = 1, idx[i-1]+1
          end
        end
      end
      return cnt,imap(ts,attachIdx(ret)):u()
    end
  end
--- test
  for i,a,b in prod({1,2},{3,4}) do
    print(i,a,b)
  end
  print()
  for i,a,b in prod({3,4},{1,2}) do
    print(i,a,b)
  end

```

{{out}}

```txt
1	1	3
2	1	4
3	2	3
4	2	4

1	3	1
2	3	2
3	4	1
4	4	2
```



###  Using coroutines

I have not benchmarked this, but I believe that this should run faster than the functional implementation and also likely the imperative implementation, it has significantly fewer function calls per iteration, and only the stack changes during iteration (no garbage collection during iteration). On the other hand due to avoiding garbage collection, result is reused between returns, so mutating the returned result is unsafe.

It is possible that specialising descend by depth may yield a further improvement in performance, but it would only be able to eliminate the lookup of ''sets[depth]'' and the if test, because the reference to ''result[depth]'' is required; I doubt the increase in complexity would be worth the (potential) improvement in performance.

```lua
local function cartesian_product(sets)
  local result = {}
  local set_count = #sets
--[[ I believe that this should make the below go very slightly faster, because it doesn't need to lookup yield in coroutine each time it
     yields, though perhaps the compiler optimises the lookup away? ]]
  local yield = coroutine.yield
  local function descend(depth)
    if depth == set_count then
      for k,v in pairs(sets[depth]) do
        result[depth] = v
        yield(result)
      end
    else
      for k,v in pairs(sets[depth]) do
        result[depth] = v
        descend(depth + 1)
      end
    end
  end
  return coroutine.wrap(function() descend(1) end)
end

--- tests
local test_cases = {
  {{1, 2}, {3, 4}},
  {{3, 4}, {1, 2}},
  {{1776, 1789}, {7, 12}, {4, 14, 23}, {0,1}},
  {{1,2,3}, {30}, {500, 100}},
  {{1,2,3}, {}, {500, 100}}
}

local function format_nested_list(list)
  if list[1] and type(list[1]) == "table" then
    local formatted_items = {}
    for i, item in ipairs(list) do
      formatted_items[i] = format_nested_list(item)
    end
    return format_nested_list(formatted_items)
  else
    return "{" .. table.concat(list, ",") .. "}"
  end
end

for _,test_case in ipairs(test_cases) do
  print(format_nested_list(test_case))
  for product in cartesian_product(test_case) do
    print("  " .. format_nested_list(product))
  end
end
```



###  Imperative iterator

The functional implementation restated as an imperative iterator, also adjusted to not allocate a new result table on each iteration; this saves time, but makes mutating the returned table unsafe.

```lua
local function cartesian_product(sets)
  local item_counts = {}
  local indices = {}
  local results = {}
  local set_count = #sets
  local combination_count = 1

  for set_index=set_count, 1, -1 do
    local set = sets[set_index]
    local item_count = #set
    item_counts[set_index] = item_count
    indices[set_index] = 1
    results[set_index] = set[1]
    combination_count = combination_count * item_count
  end

  local combination_index = 0

  return function()
    if combination_index >= combination_count then return end -- no more output

    if combination_index == 0 then goto skip_update end -- skip first index update

    indices[set_count] = indices[set_count] + 1

    for set_index=set_count, 1, -1 do -- update index list
      local set = sets[set_index]
      local index = indices[set_index]
      if index <= item_counts[set_index] then
        results[set_index] = set[index]
        break -- no further update needed
      else -- propagate item_counts overflow
        results[set_index] = set[1]
        indices[set_index] = 1
        if set_index > 1 then
          indices[set_index - 1] = indices[set_index - 1] + 1
        end
      end
    end

    ::skip_update::

    combination_index = combination_index + 1

    return combination_index, results
  end
end
--- tests
local test_cases = {
  {{1, 2}, {3, 4}},
  {{3, 4}, {1, 2}},
  {{1776, 1789}, {7, 12}, {4, 14, 23}, {0,1}},
  {{1,2,3}, {30}, {500, 100}},
  {{1,2,3}, {}, {500, 100}}
}

local function format_nested_list(list)
  if list[1] and type(list[1]) == "table" then
    local formatted_items = {}
    for i, item in ipairs(list) do
      formatted_items[i] = format_nested_list(item)
    end
    return format_nested_list(formatted_items)
  else
    return "{" .. table.concat(list, ",") .. "}"
  end
end

for _,test_case in ipairs(test_cases) do
  print(format_nested_list(test_case))
  for i, product in cartesian_product(test_case) do
    print(i, format_nested_list(product))
  end
end
```



## Maple


```Maple

cartmulti := proc ()
 local m, v;
 if [] in {args} then
 return [];
 else
m := Iterator:-CartesianProduct(args);
 for v in m do
 printf("%{}a\n", v);
 end do;
 end if;
 end proc;

```



## Mathematica


```Mathematica
cartesianProduct[args__] := Flatten[Outer[List, args], Length[{args}] - 1]
```


=={{header|Modula-2}}==

```modula2
MODULE CartesianProduct;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(a : INTEGER);
VAR buf : ARRAY[0..9] OF CHAR;
BEGIN
    FormatString("%i", buf, a);
    WriteString(buf)
END WriteInt;

PROCEDURE Cartesian(a,b : ARRAY OF INTEGER);
VAR i,j : CARDINAL;
BEGIN
    WriteString("[");
    FOR i:=0 TO HIGH(a) DO
        FOR j:=0 TO HIGH(b) DO
            IF (i>0) OR (j>0) THEN
                WriteString(",");
            END;
            WriteString("[");
            WriteInt(a[i]);
            WriteString(",");
            WriteInt(b[j]);
            WriteString("]")
        END
    END;
    WriteString("]");
    WriteLn
END Cartesian;

TYPE
    AP = ARRAY[0..1] OF INTEGER;
    E = ARRAY[0..0] OF INTEGER;
VAR
    a,b : AP;
BEGIN
    a := AP{1,2};
    b := AP{3,4};
    Cartesian(a,b);

    a := AP{3,4};
    b := AP{1,2};
    Cartesian(a,b);

    (* If there is a way to create an empty array, I do not know of it *)

    ReadChar
END CartesianProduct.
```



## OCaml

''The double semicolons are necessary only for the toplevel''

Naive but more readable version

```ocaml

let rec product l1 l2 =
    match l1, l2 with
    | [], _ | _, [] -> []
    | h1::t1, h2::t2 -> (h1,h2)::(product [h1] t2)@(product t1 l2)
;;

product [1;2] [3;4];;
(*- : (int * int) list = [(1, 3); (1, 4); (2, 3); (2, 4)]*)
product [3;4] [1;2];;
(*- : (int * int) list = [(3, 1); (3, 2); (4, 1); (4, 2)]*)
product [1;2] [];;
(*- : (int * 'a) list = []*)
product [] [1;2];;
(*- : ('a * int) list = []*)

```


Implementation with a bit more tail-call optimization, inroducing a helper function. The order of the result is changed but it should not be an issue for most uses.

```ocaml

let product' l1 l2 =
    let rec aux ~acc l1' l2' =
        match l1', l2' with
        | [], _ | _, [] -> acc
        | h1::t1, h2::t2 ->
            let acc = (h1,h2)::acc in
            let acc = aux ~acc t1 l2' in
            aux ~acc [h1] t2
    in aux [] l1 l2
;;

product' [1;2] [3;4];;
(*- : (int * int) list = [(1, 4); (2, 4); (2, 3); (1, 3)]*)
product' [3;4] [1;2];;
(*- : (int * int) list = [(3, 2); (4, 2); (4, 1); (3, 1)]*)
product' [1;2] [];;
(*- : (int * 'a) list = []*)
product' [] [1;2];;
(*- : ('a * int) list = []*)

```

Implemented using nested folds:

```ocaml

let cart_prod l1 l2 =
  List.fold_left (fun acc1 ele1 ->
    List.fold_left (fun acc2 ele2 -> (ele1,ele2)::acc2) acc1 l2) [] l1 ;;

cart_prod [1; 2; 3] ['a'; 'b'; 'c'] ;;
(*- : (int * char) list = [(3, 'c'); (3, 'b'); (3, 'a'); (2, 'c'); (2, 'b'); (2, 'a'); (1, 'c'); (1, 'b'); (1, 'a')]*)
cart_prod [1; 2; 3] [] ;;
(*- : ('a * int) list = [] *)

```


Extra credit function. Since in OCaml a function can return only one type, and because tuples of different arities are different types, this returns a list of lists rather than a list of tuples.

```ocaml

let rec product'' l =
    (* We need to do the cross product of our current list and all the others
     * so we define a helper function for that *)
    let rec aux ~acc l1 l2 = match l1, l2 with
    | [], _ | _, [] -> acc
    | h1::t1, h2::t2 ->
        let acc = (h1::h2)::acc in
        let acc = (aux ~acc t1 l2) in
        aux ~acc [h1] t2
    (* now we can do the actual computation *)
    in match l with
    | [] -> []
    | [l1] -> List.map (fun x -> [x]) l1
    | l1::tl ->
        let tail_product = product'' tl in
        aux ~acc:[] l1 tail_product


product'' [[1;2];[3;4]];;
(*- : int list list = [[1; 4]; [2; 4]; [2; 3]; [1; 3]]*)
product'' [[3;4];[1;2]];;
(*- : int list list = [[3; 2]; [4; 2]; [4; 1]; [3; 1]]*)
product'' [[1;2];[]];;
(*- : int list list = []*)
product'' [[];[1;2]];;
(*- : int list list = []*)
product'' [[1776; 1789];[7; 12];[4; 14; 23];[0; 1]];;
(*
- : int list list =

[[1776; 7; 4; 1]; [1776; 12; 4; 1]; [1776; 12; 14; 1]; [1776; 12; 23; 1];
 [1776; 12; 23; 0]; [1776; 12; 14; 0]; [1776; 12; 4; 0]; [1776; 7; 14; 1];
 [1776; 7; 23; 1]; [1776; 7; 23; 0]; [1776; 7; 14; 0]; [1789; 7; 4; 1];
 [1789; 12; 4; 1]; [1789; 12; 14; 1]; [1789; 12; 23; 1]; [1789; 12; 23; 0];
 [1789; 12; 14; 0]; [1789; 12; 4; 0]; [1789; 7; 14; 1]; [1789; 7; 23; 1];
 [1789; 7; 23; 0]; [1789; 7; 14; 0]; [1789; 7; 4; 0]; [1776; 7; 4; 0]]
*)
product'' [[1; 2; 3];[30];[500; 100]];;
(*
- : int list list =

[[1; 30; 500]; [2; 30; 500]; [3; 30; 500]; [3; 30; 100]; [2; 30; 100];
 [1; 30; 100]]
*)
product'' [[1; 2; 3];[];[500; 100]];;
(*- : int list list = []*)


```



###  Better type


In the latter example, our function has this signature:

```ocaml

val product'' : 'a list list -> 'a list list = <fun>

```

This lacks clarity as those two lists are not equivalent since one replaces a tuple. We can get a better signature by creating a tuple type:

```ocaml

type 'a tuple = 'a list

let rec product'' (l:'a list tuple) =
    (* We need to do the cross product of our current list and all the others
     * so we define a helper function for that *)
    let rec aux ~acc l1 l2 = match l1, l2 with
    | [], _ | _, [] -> acc
    | h1::t1, h2::t2 ->
        let acc = (h1::h2)::acc in
        let acc = (aux ~acc t1 l2) in
        aux ~acc [h1] t2
    (* now we can do the actual computation *)
    in match l with
    | [] -> []
    | [l1] -> List.map ~f:(fun x -> ([x]:'a tuple)) l1
    | l1::tl ->
        let tail_product = product'' tl in
        aux ~acc:[] l1 tail_product
;;

type 'a tuple = 'a list
val product'' : 'a list tuple -> 'a tuple list = <fun>

```



## Perl


### = Iterative =

Nested loops, with a short-circuit to quit early if any term is an empty set.

```perl
sub cartesian {
    my $sets = shift @_;
    for (@$sets) { return [] unless @$_ }

    my $products = [[]];
    for my $set (reverse @$sets) {
        my $partial = $products;
        $products = [];
        for my $item (@$set) {
            for my $product (@$partial) {
                push @$products, [$item, @$product];
            }
        }
    }
    $products;
}

sub product {
    my($s,$fmt) = @_;
    my $tuples;
    for $a ( @{ cartesian( \@$s ) } ) { $tuples .= sprintf "($fmt) ", @$a; }
    $tuples . "\n";
}

print
product([[1, 2],      [3, 4]                  ], '%1d %1d'        ).
product([[3, 4],      [1, 2]                  ], '%1d %1d'        ).
product([[1, 2],      []                      ], '%1d %1d'        ).
product([[],          [1, 2]                  ], '%1d %1d'        ).
product([[1,2,3],     [30],   [500,100]       ], '%1d %1d %3d'    ).
product([[1,2,3],     [],     [500,100]       ], '%1d %1d %3d'    ).
product([[1776,1789], [7,12], [4,14,23], [0,1]], '%4d %2d %2d %1d')
```

{{out}}

```txt
(1 3) (1 4) (2 3) (2 4)
(3 1) (3 2) (4 1) (4 2)


(1 30 500) (1 30 100) (2 30 500) (2 30 100) (3 30 500) (3 30 100)

(1776  7  4 0) (1776  7  4 1) (1776  7 14 0) (1776  7 14 1) (1776  7 23 0) (1776  7 23 1) (1776 12  4 0) (1776 12  4 1) (1776 12 14 0) (1776 12 14 1) (1776 12 23 0) (1776 12 23 1) (1789  7  4 0) (1789  7  4 1) (1789  7 14 0) (1789  7 14 1) (1789  7 23 0) (1789  7 23 1) (1789 12  4 0) (1789 12  4 1) (1789 12 14 0) (1789 12 14 1) (1789 12 23 0) (1789 12 23 1)
```



### = Glob =

This being Perl, there's more than one way to do it. A quick demonstration of how <code>glob</code>, more typically used for filename wildcard expansion, can solve the task.

```perl
$tuples = [ map { [split /:/] } glob '{1,2,3}:{30}:{500,100}' ];

for $a (@$tuples) { printf "(%1d %2d %3d) ", @$a; }
```

{{out}}

```txt
(1 30 500) (1 30 100) (2 30 500) (2 30 100) (3 30 500) (3 30 100)
```



### = Modules =

A variety of modules can do this correctly for an arbitrary number of lists (each of independent length).  Arguably using modules is very idiomatic Perl.

```perl
use ntheory qw/forsetproduct/;
forsetproduct { say "@_" } [1,2,3],[qw/a b c/],[qw/@ $ !/];

use Set::Product qw/product/;
product { say "@_" } [1,2,3],[qw/a b c/],[qw/@ $ !/];

use Math::Cartesian::Product;
cartesian { say "@_" } [1,2,3],[qw/a b c/],[qw/@ $ !/];

use Algorithm::Loops qw/NestedLoops/;
NestedLoops([[1,2,3],[qw/a b c/],[qw/@ $ !/]], sub { say "@_"; });
```



## Perl 6

{{works with|Rakudo|2017.06}}
The cross meta operator X will return the cartesian product of two lists. To apply the cross meta-operator to a variable number of lists, use the reduce cross meta operator [X].


```perl6
# cartesian product of two lists using the X cross meta-operator
say (1, 2) X (3, 4);
say (3, 4) X (1, 2);
say (1, 2) X ( );
say ( )    X ( 1, 2 );

# cartesian product of variable number of lists using
# the [X] reduce cross meta-operator
say [X] (1776, 1789), (7, 12), (4, 14, 23), (0, 1);
say [X] (1, 2, 3), (30), (500, 100);
say [X] (1, 2, 3), (),   (500, 100);
```

{{out}}

```txt
((1 3) (1 4) (2 3) (2 4))
((3 1) (3 2) (4 1) (4 2))
()
()
((1776 7 4 0) (1776 7 4 1) (1776 7 14 0) (1776 7 14 1) (1776 7 23 0) (1776 7 23 1) (1776 12 4 0) (1776 12 4 1) (1776 12 14 0) (1776 12 14 1) (1776 12 23 0) (1776 12 23 1) (1789 7 4 0) (1789 7 4 1) (1789 7 14 0) (1789 7 14 1) (1789 7 23 0) (1789 7 23 1) (1789 12 4 0) (1789 12 4 1) (1789 12 14 0) (1789 12 14 1) (1789 12 23 0) (1789 12 23 1))
((1 30 500) (1 30 100) (2 30 500) (2 30 100) (3 30 500) (3 30 100))
()
```



## Phix


```Phix
function cart(sequence s)
sequence res = {}
    for n=2 to length(s) do
        for i=1 to length(s[1]) do
            for j=1 to length(s[2]) do
                res = append(res,s[1][i]&s[2][j])
            end for
        end for
        if length(s)=2 then exit end if
        s[1..2] = {res}
        res = {}
    end for
    return res
end function

?cart({{1,2},{3,4}})
?cart({{3,4},{1,2}})
?cart({{1,2},{}})
?cart({{},{1,2}})
?cart({{1776, 1789},{7, 12},{4, 14, 23},{0, 1}})
?cart({{1, 2, 3},{30},{500, 100}})
?cart({{1, 2, 3},{},{500, 100}})
```

{{out}}

```txt

{{1,3},{1,4},{2,3},{2,4}}
{{3,1},{3,2},{4,1},{4,2}}
{}
{}
{{1776,7,4,0},{1776,7,4,1},{1776,7,14,0},{1776,7,14,1},{1776,7,23,0},{1776,7,23,1},
 {1776,12,4,0},{1776,12,4,1},{1776,12,14,0},{1776,12,14,1},{1776,12,23,0},{1776,12,23,1},
 {1789,7,4,0},{1789,7,4,1},{1789,7,14,0},{1789,7,14,1},{1789,7,23,0},{1789,7,23,1},
 {1789,12,4,0},{1789,12,4,1},{1789,12,14,0},{1789,12,14,1},{1789,12,23,0},{1789,12,23,1}}
{{1,30,500},{1,30,100},{2,30,500},{2,30,100},{3,30,500},{3,30,100}}
{}

```



## PicoLisp


```PicoLisp
(de 2lists (L1 L2)
   (mapcan
      '((I)
         (mapcar
            '((A) ((if (atom A) list cons) I A))
            L2 ) )
      L1 ) )
(de reduce (L . @)
   (ifn (rest) L (2lists L (apply reduce (rest)))) )
(de cartesian (L . @)
   (and L (rest) (pass reduce L)) )

(println
   (cartesian (1 2)) )
(println
   (cartesian NIL (1 2)) )
(println
   (cartesian (1 2) (3 4)) )
(println
   (cartesian (3 4) (1 2)) )
(println
   (cartesian (1776 1789) (7 12) (4 14 23) (0 1)) )
(println
   (cartesian (1 2 3) (30) (500 100)) )
(println
   (cartesian (1 2 3) NIL (500 100)) )
```


{{out}}

```txt
NIL
NIL
((1 3) (1 4) (2 3) (2 4))
((3 1) (3 2) (4 1) (4 2))
((1776 7 4 0) (1776 7 4 1) (1776 7 14 0) (1776 7 14 1) (1776 7 23 0) (1776 7 23 1) (1776 12 4 0) (1776 12 4 1) (1776 12 14 0) (1776 12 14 1) (1776 12 23 0) (1776 12 23 1) (1789 7 4 0) (1789 7 4 1) (1789 7 14 0) (1789 7 14 1) (1789 7 23 0) (1789 7 23 1) (1789 12 4 0) (1789 12 4 1) (1789 12 14 0) (1789 12 14 1) (1789 12 23 0) (1789 12 23 1))
((1 30 500) (1 30 100) (2 30 500) (2 30 100) (3 30 500) (3 30 100))
NIL
```



## Python


### Using itertools


```python
import itertools

def cp(lsts):
    return list(itertools.product(*lsts))

if __name__ == '__main__':
    from pprint import pprint as pp

    for lists in [[[1,2],[3,4]], [[3,4],[1,2]], [[], [1, 2]], [[1, 2], []],
                  ((1776, 1789),  (7, 12), (4, 14, 23), (0, 1)),
                  ((1, 2, 3), (30,), (500, 100)),
                  ((1, 2, 3), (), (500, 100))]:
        print(lists, '=>')
        pp(cp(lists), indent=2)

```

{{out}}

```txt
[[1, 2], [3, 4]] =>
[(1, 3), (1, 4), (2, 3), (2, 4)]
[[3, 4], [1, 2]] =>
[(3, 1), (3, 2), (4, 1), (4, 2)]
[[], [1, 2]] =>
[]
[[1, 2], []] =>
[]
((1776, 1789), (7, 12), (4, 14, 23), (0, 1)) =>
[ (1776, 7, 4, 0),
  (1776, 7, 4, 1),
  (1776, 7, 14, 0),
  (1776, 7, 14, 1),
  (1776, 7, 23, 0),
  (1776, 7, 23, 1),
  (1776, 12, 4, 0),
  (1776, 12, 4, 1),
  (1776, 12, 14, 0),
  (1776, 12, 14, 1),
  (1776, 12, 23, 0),
  (1776, 12, 23, 1),
  (1789, 7, 4, 0),
  (1789, 7, 4, 1),
  (1789, 7, 14, 0),
  (1789, 7, 14, 1),
  (1789, 7, 23, 0),
  (1789, 7, 23, 1),
  (1789, 12, 4, 0),
  (1789, 12, 4, 1),
  (1789, 12, 14, 0),
  (1789, 12, 14, 1),
  (1789, 12, 23, 0),
  (1789, 12, 23, 1)]
((1, 2, 3), (30,), (500, 100)) =>
[ (1, 30, 500),
  (1, 30, 100),
  (2, 30, 500),
  (2, 30, 100),
  (3, 30, 500),
  (3, 30, 100)]
((1, 2, 3), (), (500, 100)) =>
[]
```


===Using the 'Applicative' abstraction===

This task calls for alternative approaches to defining cartesian products, and one particularly compact alternative route to a native cartesian product (in a more mathematically reasoned idiom of programming) is through the Applicative abstraction (see [[wp:Applicative_functor|Applicative Functor]]), which is slightly more general than the possibly better known '''monad''' structure. Applicative functions are provided off-the-shelf by languages like Agda, Idris, Haskell and Scala, and can usefully be implemented in any language, including Python, which supports higher-order functions.

If we write ourselves a re-usable Python '''ap''' function for the case of lists (applicative functions for other 'data containers' can also be written – this one applies a list of functions to a list of values):


```python
# ap (<*>) :: [(a -> b)] -> [a] -> [b]
def ap(fs):
    return lambda xs: foldl(
        lambda a: lambda f: a + foldl(
            lambda a: lambda x: a + [f(x)])([])(xs)
    )([])(fs)
```


then one simple use of it will be to define the cartesian product of two lists (of possibly different type) as:


```python
ap(map(Tuple, xs))
```


where Tuple is a constructor, and xs is bound to the first of two lists. The returned value is a function which can be applied to a second list.

For an nAry product, we can then use a '''fold''' (catamorphism) to lift the basic function over two lists ''cartesianProduct :: [a] -> [b] -> [(a, b)]'' to a function over a list of lists:


```python
# nAryCartProd :: [[a], [b], [c] ...] -> [(a, b, c ...)]
def nAryCartProd(xxs):
    return foldl1(cartesianProduct)(
        xxs
    )
```


For example:


```python
# Two lists -> list of tuples


# cartesianProduct :: [a] -> [b] -> [(a, b)]
def cartesianProduct(xs):
    return ap(map(Tuple, xs))


# List of lists -> list of tuples

# nAryCartProd :: [[a], [b], [c] ...] -> [(a, b, c ...)]
def nAryCartProd(xxs):
    return foldl1(cartesianProduct)(
        xxs
    )


# main :: IO ()
def main():
    # Product of lists of different types
    print (
        'Product of two lists of different types:'
    )
    print(
        cartesianProduct(['a', 'b', 'c'])(
            [1, 2]
        )
    )

    # TESTS OF PRODUCTS OF TWO LISTS

    print(
        '\nSpecified tests of products of two lists:'
    )
    print(
        cartesianProduct([1, 2])([3, 4]),
        ' <--> ',
        cartesianProduct([3, 4])([1, 2])
    )
    print (
        cartesianProduct([1, 2])([]),
        ' <--> ',
        cartesianProduct([])([1, 2])
    )

    # TESTS OF N-ARY CARTESIAN PRODUCTS

    print('\nSpecified tests of nAry products:')
    for xs in nAryCartProd([[1776, 1789], [7, 12], [4, 14, 23], [0, 1]]):
        print(xs)

    for xs in (
        map_(nAryCartProd)(
            [
                [[1, 2, 3], [30], [500, 100]],
                [[1, 2, 3], [], [500, 100]]
            ]
        )
    ):
        print(
            xs
        )

# GENERIC -------------------------------------------------


# Applicative function for lists

# ap (<*>) :: [(a -> b)] -> [a] -> [b]
def ap(fs):
    return lambda xs: foldl(
        lambda a: lambda f: a + foldl(
            lambda a: lambda x: a + [f(x)])([])(xs)
    )([])(fs)


# foldl :: (a -> b -> a) -> a -> [b] -> a
def foldl(f):
    def go(v, xs):
        a = v
        for x in xs:
            a = f(a)(x)
        return a
    return lambda acc: lambda xs: go(acc, xs)


# foldl1 :: (a -> a -> a) -> [a] -> a
def foldl1(f):
    return lambda xs: foldl(f)(xs[0])(
        xs[1:]
    ) if xs else None


# map :: (a -> b) -> [a] -> [b]
def map_(f):
    return lambda xs: list(map(f, xs))


# Tuple :: a -> b -> (a, b)
def Tuple(x):
    return lambda y: (
        x + (y,)
    ) if tuple is type(x) else (x, y)


# TEST ----------------------------------------------------
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Product of two lists of different types:
[('a', 1), ('a', 2), ('b', 1), ('b', 2), ('c', 1), ('c', 2)]

Specified tests of products of two lists:
[(1, 3), (1, 4), (2, 3), (2, 4)]  <-->  [(3, 1), (3, 2), (4, 1), (4, 2)]
[]  <-->  []

Specified tests of nAry products:
(1776, 7, 4, 0)
(1776, 7, 4, 1)
(1776, 7, 14, 0)
(1776, 7, 14, 1)
(1776, 7, 23, 0)
(1776, 7, 23, 1)
(1776, 12, 4, 0)
(1776, 12, 4, 1)
(1776, 12, 14, 0)
(1776, 12, 14, 1)
(1776, 12, 23, 0)
(1776, 12, 23, 1)
(1789, 7, 4, 0)
(1789, 7, 4, 1)
(1789, 7, 14, 0)
(1789, 7, 14, 1)
(1789, 7, 23, 0)
(1789, 7, 23, 1)
(1789, 12, 4, 0)
(1789, 12, 4, 1)
(1789, 12, 14, 0)
(1789, 12, 14, 1)
(1789, 12, 23, 0)
(1789, 12, 23, 1)
[(1, 30, 500), (1, 30, 100), (2, 30, 500), (2, 30, 100), (3, 30, 500), (3, 30, 100)]
[]
```



## R



```R

one_w_many <- function(one, many) lapply(many, function(x) c(one,x))

# Let's define an infix operator to perform a cartesian product.

"%p%" <- function( a, b ) {
  p = c( sapply(a, function (x) one_w_many(x, b) ) )
  if (is.null(unlist(p))) list() else p}

display_prod <-
  function (xs) { for (x in xs) cat( paste(x, collapse=", "), "\n" ) }

fmt_vec <- function(v) sprintf("(%s)", paste(v, collapse=', '))

go <- function (...) {
  cat("\n", paste( sapply(list(...),fmt_vec), collapse=" * "), "\n")
  prod = Reduce( '%p%', list(...) )
  display_prod( prod ) }

```


Simple tests:


```R

> display_prod(  c(1, 2) %p% c(3, 4)  )
1, 3
1, 4
2, 3
2, 4
> display_prod(  c(3, 4) %p% c(1, 2)  )
3, 1
3, 2
4, 1
4, 2
> display_prod(  c(3, 4) %p% c()  )
>

```


Tougher tests:


```R

go( c(1776, 1789), c(7, 12), c(4, 14, 23), c(0, 1) )
go( c(1, 2, 3), c(30), c(500, 100) )
go( c(1, 2, 3), c(), c(500, 100) )

```


{{out}}

```txt

 (1776, 1789) * (7, 12) * (4, 14, 23) * (0, 1)
1776, 7, 4, 0
1776, 7, 4, 1
1776, 7, 14, 0
1776, 7, 14, 1
1776, 7, 23, 0
1776, 7, 23, 1
1776, 12, 4, 0
1776, 12, 4, 1
1776, 12, 14, 0
1776, 12, 14, 1
1776, 12, 23, 0
1776, 12, 23, 1
1789, 7, 4, 0
1789, 7, 4, 1
1789, 7, 14, 0
1789, 7, 14, 1
1789, 7, 23, 0
1789, 7, 23, 1
1789, 12, 4, 0
1789, 12, 4, 1
1789, 12, 14, 0
1789, 12, 14, 1
1789, 12, 23, 0
1789, 12, 23, 1

 (1, 2, 3) * (30) * (500, 100)
1, 30, 500
1, 30, 100
2, 30, 500
2, 30, 100
3, 30, 500
3, 30, 100

 (1, 2, 3) * () * (500, 100)

```



## Racket


Racket has a built-in "cartesian-product" function:

<lang>#lang racket/base
(require rackunit
         ;; usually, included in "racket", but we're using racket/base so we
         ;; show where this comes from
         (only-in racket/list cartesian-product))
;; these tests will pass silently
(check-equal? (cartesian-product '(1 2) '(3 4))
             '((1 3) (1 4) (2 3) (2 4)))
(check-equal? (cartesian-product '(3 4) '(1 2))
             '((3 1) (3 2) (4 1) (4 2)))
(check-equal? (cartesian-product '(1 2) '()) '())
(check-equal? (cartesian-product '() '(1 2)) '())

;; these will print
(cartesian-product '(1776 1789) '(7 12) '(4 14 23) '(0 1))
(cartesian-product '(1 2 3) '(30) '(500 100))
(cartesian-product '(1 2 3) '() '(500 100))
```


{{out}}


```txt
'((1776 7 4 0)
  (1776 7 4 1)
  (1776 7 14 0)
  (1776 7 14 1)
  (1776 7 23 0)
  (1776 7 23 1)
  (1776 12 4 0)
  (1776 12 4 1)
  (1776 12 14 0)
  (1776 12 14 1)
  (1776 12 23 0)
  (1776 12 23 1)
  (1789 7 4 0)
  (1789 7 4 1)
  (1789 7 14 0)
  (1789 7 14 1)
  (1789 7 23 0)
  (1789 7 23 1)
  (1789 12 4 0)
  (1789 12 4 1)
  (1789 12 14 0)
  (1789 12 14 1)
  (1789 12 23 0)
  (1789 12 23 1))
'((1 30 500) (1 30 100) (2 30 500) (2 30 100) (3 30 500) (3 30 100))
'()
```



## REXX


### version 1

This REXX version isn't limited by the number of lists or the number of sets within a list.

```rexx
/*REXX program  calculates  the   Cartesian product   of two  arbitrary-sized  lists.   */
@.=                                              /*assign the default value to  @. array*/
parse arg @.1                                    /*obtain the optional value of  @.1    */
if @.1=''  then do;  @.1= "{1,2} {3,4}"          /*Not specified?  Then use the defaults*/
                     @.2= "{3,4} {1,2}"          /* "      "         "   "   "      "   */
                     @.3= "{1,2} {}"             /* "      "         "   "   "      "   */
                     @.4= "{}    {3,4}"          /* "      "         "   "   "      "   */
                     @.5= "{1,2} {3,4,5}"        /* "      "         "   "   "      "   */
                end
                                                 /* [↓]  process each of the  @.n values*/
  do n=1  while @.n \= ''                        /*keep processing while there's a value*/
  z= translate( space( @.n, 0),  ,  ',')         /*translate the  commas  to blanks.    */
     do #=1  until z==''                         /*process each elements in first list. */
     parse var  z   '{'  x.#  '}'   z            /*parse the list  (contains elements). */
     end   /*#*/
  $=
     do       i=1   for #-1                      /*process the subsequent lists.        */
       do     a=1   for words(x.i)               /*obtain the elements of the first list*/
         do   j=i+1 for #-1                      /*   "    "  subsequent lists.         */
           do b=1   for words(x.j)               /*   "    " elements of subsequent list*/
           $=$',('word(x.i, a)","word(x.j, b)')' /*append partial Cartesian product ──►$*/
           end   /*b*/
         end     /*j*/
       end       /*a*/
     end         /*i*/
  say 'Cartesian product of '       space(@.n)       " is ───► {"substr($, 2)'}'
  end            /*n*/                           /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default lists:}}

```txt

Cartesian product of  {1,2} {3,4}  is ───► {(1,3),(1,4),(2,3),(2,4)}
Cartesian product of  {3,4} {1,2}  is ───► {(3,1),(3,2),(4,1),(4,2)}
Cartesian product of  {1,2} {}  is ───► {}
Cartesian product of  {} {3,4}  is ───► {}
Cartesian product of  {1,2} {3,4,5}  is ───► {(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)}

```



### version 2


```rexx
/* REXX computes the Cartesian Product of up to 4 sets */
Call cart '{1, 2} x {3, 4}'
Call cart '{3, 4} x {1, 2}'
Call cart '{1, 2} x {}'
Call cart '{} x {1, 2}'
Call cart '{1776, 1789} x {7, 12} x {4, 14, 23} x {0, 1}'
Call cart '{1, 2, 3} x {30} x {500, 100}'
Call cart '{1, 2, 3} x {} x {500, 100}'
Exit

cart:
  Parse Arg sl
  Say sl
  Do i=1 By 1 while pos('{',sl)>0
    Parse Var sl '{' list '}' sl
    Do j=1 By 1 While list<>''
      Parse Var list e.i.j . ',' list
      End
    n.i=j-1
    If n.i=0 Then Do /* an empty set */
      Say '{}'
      Say ''
      Return
      End
    End
  n=i-1
  ct2.=0
  Do i=1 To n.1
    Do j=1 To n.2
      z=ct2.0+1
      ct2.z=e.1.i e.2.j
      ct2.0=z
      End
    End
  If n<3 Then
    Return output(2)
  ct3.=0
  Do i=1 To ct2.0
    Do k=1 To n.3
      z=ct3.0+1
      ct3.z=ct2.i e.3.k
      ct3.0=z
      End
    End
  If n<4 Then
    Return output(3)
  ct4.=0
  Do i=1 To ct3.0
    Do l=1 To n.4
      z=ct4.0+1
      ct4.z=ct3.i e.4.l
      ct4.0=z
      End
    End
  Return output(4)

output:
  Parse Arg u
  Do v=1 To value('ct'u'.0')
    res='{'translate(value('ct'u'.'v),',',' ')'}'
    Say res
    End
  Say ' '
  Return 0
```

{{out}}

```txt
{1, 2} x {3, 4}
{1,3}
{1,4}
{2,3}
{2,4}

{3, 4} x {1, 2}
{3,1}
{3,2}
{4,1}
{4,2}

{1, 2} x {}
{}

{} x {1, 2}
{}

{1776, 1789} x {7, 12} x {4, 14, 23} x {0, 1}
{1776,7,4,0}
{1776,7,4,1}
{1776,7,14,0}
{1776,7,14,1}
{1776,7,23,0}
{1776,7,23,1}
{1776,12,4,0}
{1776,12,4,1}
{1776,12,14,0}
{1776,12,14,1}
{1776,12,23,0}
{1776,12,23,1}
{1789,7,4,0}
{1789,7,4,1}
{1789,7,14,0}
{1789,7,14,1}
{1789,7,23,0}
{1789,7,23,1}
{1789,12,4,0}
{1789,12,4,1}
{1789,12,14,0}
{1789,12,14,1}
{1789,12,23,0}
{1789,12,23,1}

{1, 2, 3} x {30} x {500, 100}
{1,30,500}
{1,30,100}
{2,30,500}
{2,30,100}
{3,30,500}
{3,30,100}

{1, 2, 3} x {} x {500, 100}
{}
```



## Ring


```ring

# Project : Cartesian product of two or more lists

list1 = [[1,2],[3,4]]
list2 = [[3,4],[1,2]]
cartesian(list1)
cartesian(list2)

func cartesian(list1)
     for n = 1 to len(list1[1])
         for m = 1 to len(list1[2])
             see "(" + list1[1][n] + ", " + list1[2][m] + ")" + nl
         next
      next
      see nl

```

Output:

```txt

(1, 3)
(1, 4)
(2, 3)
(2, 4)

(3, 1)
(3, 2)
(4, 1)
(4, 2)

```



## Ruby

"product" is a method of arrays. It takes one or more arrays as argument and results in the Cartesian product:

```ruby
p [1, 2].product([3, 4])
p [3, 4].product([1, 2])
p [1, 2].product([])
p [].product([1, 2])
p [1776, 1789].product([7, 12], [4, 14, 23], [0, 1])
p [1, 2, 3].product([30], [500, 100])
p [1, 2, 3].product([], [500, 100])

```

{{out}}
```txt
[[1, 3], [1, 4], [2, 3], [2, 4]]
[[3, 1], [3, 2], [4, 1], [4, 2]]
[]
[]
[[1776, 7, 4, 0], [1776, 7, 4, 1], [1776, 7, 14, 0], [1776, 7, 14, 1], [1776, 7, 23, 0], [1776, 7, 23, 1], [1776, 12, 4, 0], [1776, 12, 4, 1], [1776, 12, 14, 0], [1776, 12, 14, 1], [1776, 12, 23, 0], [1776, 12, 23, 1], [1789, 7, 4, 0], [1789, 7, 4, 1], [1789, 7, 14, 0], [1789, 7, 14, 1], [1789, 7, 23, 0], [1789, 7, 23, 1], [1789, 12, 4, 0], [1789, 12, 4, 1], [1789, 12, 14, 0], [1789, 12, 14, 1], [1789, 12, 23, 0], [1789, 12, 23, 1]]
[[1, 30, 500], [1, 30, 100], [2, 30, 500], [2, 30, 100], [3, 30, 500], [3, 30, 100]]
[]

```



## Rust


```rust>type List = Vec<Vec<u32>
;

fn cartesian_product(lists: &List) -> List {
    let mut res: List = vec![];
    if lists.len() < 2 || lists.iter().any(|x| x.len() == 0) {
        return res
    }

    let mut list_iter = lists.iter();
    if let Some(first_list) = list_iter.next() {
        for &i in first_list {
            res.push(vec![i]);
        }
    }
    for l in list_iter {
        let mut tmp: List = List::new();
        for r in res {
            for &el in l {
                let mut tmp_el = r.clone();
                tmp_el.push(el);
                tmp.push(tmp_el);
            }
        }
        res = tmp;
    }
    res
}

fn print_list(list: &List) {
    print!("{{ ");
    for inner_list in list {
        print!("( ");
        for el in inner_list {
            print!("{} ", el);
        }
        print!(") ");
    }
    println!("}}");
}

fn main() {
    println!("{{1, 2}} x {{3, 4}}");
    print_list(&cartesian_product(&vec![vec![1, 2], vec![3, 4]]));
    println!("\n{{3, 4}} x {{1, 2}}");
    print_list(&cartesian_product(&vec![vec![3, 4], vec![1, 2]]));
    println!("\n{{1, 2}} x {{}}");
    print_list(&cartesian_product(&vec![vec![1, 2], vec![]]));
    println!("\n{{}} x {{1, 2}}");
    print_list(&cartesian_product(&vec![vec![], vec![1, 2]]));
    println!("\n{{1776, 1789}} × {{7, 12}} × {{4, 14, 23}} × {{0, 1}}");
    print_list(&cartesian_product(&vec![vec![1776, 1789], vec![7, 12], vec![4, 14, 23], vec![0, 1]]));
    println!("\n{{1, 2, 3}} × {{30}} × {{500, 100}}");
    print_list(&cartesian_product(&vec![vec![1, 2, 3], vec![30], vec![500, 100]]));
    println!("\n{{1, 2, 3}} × {{}} × {{500, 100}}");
    print_list(&cartesian_product(&vec![vec![1, 2, 3], vec![], vec![500, 100]]));
}

```

{{out}}
```txt
{1, 2} x {3, 4}
{ ( 1 3 ) ( 1 4 ) ( 2 3 ) ( 2 4 ) }

{3, 4} x {1, 2}
{ ( 3 1 ) ( 3 2 ) ( 4 1 ) ( 4 2 ) }

{1, 2} x {}
{ }

{} x {1, 2}
{ }

{1776, 1789} × {7, 12} × {4, 14, 23} × {0, 1}
{ ( 1776 7 4 0 ) ( 1776 7 4 1 ) ( 1776 7 14 0 ) ( 1776 7 14 1 ) ( 1776 7 23 0 ) ( 1776 7 23 1 ) ( 1776 12 4 0 ) ( 1776 12 4 1 ) ( 1776 12 14 0 ) ( 1776 12 14 1 ) ( 1776 12 23 0 ) ( 1776 12 23 1 ) ( 1789 7 4 0 ) ( 1789 7 4 1 ) ( 1789 7 14 0 ) ( 1789 7 14 1 ) ( 1789 7 23 0 ) ( 1789 7 23 1 ) ( 1789 12 4 0 ) ( 1789 12 4 1 ) ( 1789 12 14 0 ) ( 1789 12 14 1 ) ( 1789 12 23 0 ) ( 1789 12 23 1 ) }

{1, 2, 3} × {30} × {500, 100}
{ ( 1 30 500 ) ( 1 30 100 ) ( 2 30 500 ) ( 2 30 100 ) ( 3 30 500 ) ( 3 30 100 ) }

{1, 2, 3} × {} × {500, 100}
{ }

```



## Scala

Function returning the n-ary product of an arbitrary number of lists, each of arbitrary length:


```scala
def cartesianProduct[T](lst: List[T]*): List[List[T]] = {

  /**
    * Prepend single element to all lists of list
    * @param e single elemetn
    * @param ll list of list
    * @param a accumulator for tail recursive implementation
    * @return list of lists with prepended element e
    */
  def pel(e: T,
          ll: List[List[T]],
          a: List[List[T]] = Nil): List[List[T]] =
    ll match {
      case Nil => a.reverse
      case x :: xs => pel(e, xs, (e :: x) :: a )
    }

  lst.toList match {
    case Nil => Nil
    case x :: Nil => List(x)
    case x :: _ =>
      x match {
        case Nil => Nil
        case _ =>
          lst.par.foldRight(List(x))( (l, a) =>
            l.flatMap(pel(_, a))
          ).map(_.dropRight(x.size))
      }
  }
}
```

and usage:

```scala
cartesianProduct(List(1, 2), List(3, 4))
  .map(_.mkString("(", ", ", ")")).mkString("{",", ","}")
```

{{out}}

```txt
{(1, 3), (1, 4), (2, 3), (2, 4)}
```



```scala
cartesianProduct(List(3, 4), List(1, 2))
  .map(_.mkString("(", ", ", ")")).mkString("{",", ","}")
```

{{out}}

```txt
{(3, 1), (3, 2), (4, 1), (4, 2)}
```



```scala
cartesianProduct(List(1, 2), List.empty)
  .map(_.mkString("(", ", ", ")")).mkString("{",", ","}")
```

{{out}}

```txt
{}
```



```scala
cartesianProduct(List.empty, List(1, 2))
  .map(_.mkString("(", ", ", ")")).mkString("{",", ","}")
```

{{out}}

```txt
{}
```



```scala
cartesianProduct(List(1776, 1789), List(7, 12), List(4, 14, 23), List(0, 1))
  .map(_.mkString("(", ", ", ")")).mkString("{",", ","}")
```

{{out}}

```txt
{(1776, 7, 4, 0), (1776, 7, 4, 1), (1776, 7, 14, 0), (1776, 7, 14, 1), (1776, 7, 23, 0), (1776, 7, 23, 1), (1776, 12, 4, 0), (1776, 12, 4, 1), (1776, 12, 14, 0), (1776, 12, 14, 1), (1776, 12, 23, 0), (1776, 12, 23, 1), (1789, 7, 4, 0), (1789, 7, 4, 1), (1789, 7, 14, 0), (1789, 7, 14, 1), (1789, 7, 23, 0), (1789, 7, 23, 1), (1789, 12, 4, 0), (1789, 12, 4, 1), (1789, 12, 14, 0), (1789, 12, 14, 1), (1789, 12, 23, 0), (1789, 12, 23, 1)}
```



```scala
cartesianProduct(List(1, 2, 3), List(30), List(500, 100))
  .map(_.mkString("(", ", ", ")")).mkString("{",", ","}")
```

{{out}}

```txt
{(1, 30, 500), (1, 30, 100), (2, 30, 500), (2, 30, 100), (3, 30, 500), (3, 30, 100)}
```



```scala
cartesianProduct(List(1, 2, 3), List.empty, List(500, 100))
  .map(_.mkString("[", ", ", "]")).mkString("\n")
```

{{out}}

```txt
{}
```



## Sidef

In Sidef, the Cartesian product of an arbitrary number of arrays is built-in as ''Array.cartesian()'':

```ruby
cartesian([[1,2], [3,4], [5,6]]).say
cartesian([[1,2], [3,4], [5,6]], {|*arr| say arr })
```


Alternatively, a simple recursive implementation:

```ruby
func cartesian_product(*arr) {

    var c = []
    var r = []

    func {
        if (c.len < arr.len) {
            for item in (arr[c.len]) {
                c.push(item)
                __FUNC__()
                c.pop
            }
        }
        else {
            r.push([c...])
        }
    }()

    return r
}
```


Completing the task:

```ruby
say cartesian_product([1,2], [3,4])
say cartesian_product([3,4], [1,2])
```

{{out}}

```txt

[[1, 3], [1, 4], [2, 3], [2, 4]]
[[3, 1], [3, 2], [4, 1], [4, 2]]

```

The product of an empty list with any other list is empty:

```ruby
say cartesian_product([1,2], [])
say cartesian_product([], [1,2])
```

{{out}}

```txt

[]
[]

```

Extra credit:

```ruby
cartesian_product([1776, 1789], [7, 12], [4, 14, 23], [0, 1]).each{ .say }
```

{{out}}

```txt

[1776, 7, 4, 0]
[1776, 7, 4, 1]
[1776, 7, 14, 0]
[1776, 7, 14, 1]
[1776, 7, 23, 0]
[1776, 7, 23, 1]
[1776, 12, 4, 0]
[1776, 12, 4, 1]
[1776, 12, 14, 0]
[1776, 12, 14, 1]
[1776, 12, 23, 0]
[1776, 12, 23, 1]
[1789, 7, 4, 0]
[1789, 7, 4, 1]
[1789, 7, 14, 0]
[1789, 7, 14, 1]
[1789, 7, 23, 0]
[1789, 7, 23, 1]
[1789, 12, 4, 0]
[1789, 12, 4, 1]
[1789, 12, 14, 0]
[1789, 12, 14, 1]
[1789, 12, 23, 0]
[1789, 12, 23, 1]

```



```ruby
say cartesian_product([1, 2, 3], [30], [500, 100])
say cartesian_product([1, 2, 3], [], [500, 100])
```

{{out}}

```txt

[[1, 30, 500], [1, 30, 100], [2, 30, 500], [2, 30, 100], [3, 30, 500], [3, 30, 100]]
[]

```



## SQL

If we create lists as tables with one column, cartesian product is easy.

```sql
-- set up list 1
create table L1 (value integer);
insert into L1 values (1);
insert into L1 values (2);
-- set up list 2
create table L2 (value integer);
insert into L2 values (3);
insert into L2 values (4);
-- get the product
select * from L1, L2;
```

{{out}}

```txt
     VALUE      VALUE
---------- ----------
         1          3
         1          4
         2          3
         2          4
```
You should be able to be more explicit should get the same result:
```sql
select * from L1 cross join L2;
```

Product with an empty list works as expected (using the tables created above):

```sql
delete from L2;
select * from L1, L2;
```

{{out}}

```txt
no rows selected
```

I don't think "extra credit" is meaningful here because cartesian product is so hard-baked into SQL, so here's just one of the extra credit examples (again using the tables created above):
```sql
insert into L1 values (3);
insert into L2 values (30);
create table L3 (value integer);
insert into L3 values (500);
insert into L3 values (100);
-- product works the same for as many "lists" as you'd like
select * from L1, L2, L3;
```

{{out}}

```txt
     VALUE      VALUE      VALUE
---------- ---------- ----------
         1         30        500
         2         30        500
         3         30        500
         1         30        100
         2         30        100
         3         30        100
```



## Standard ML


```sml
fun prodList (nil,     _) = nil
  | prodList ((x::xs), ys) = map (fn y => (x,y)) ys @ prodList (xs, ys)

fun naryProdList zs = foldl (fn (xs, ys) => map op:: (prodList (xs, ys))) [[]] (rev zs)
```


{{out}}

```txt
- prodList ([1, 2], [3, 4]);
val it = [(1,3),(1,4),(2,3),(2,4)] : (int * int) list
- prodList ([3, 4], [1, 2]);
val it = [(3,1),(3,2),(4,1),(4,2)] : (int * int) list
- prodList ([1, 2], []);
stdIn:8.1-8.22 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
val it = [] : (int * ?.X1) list
- naryProdList [[1776, 1789], [7, 12], [4, 14, 23], [0, 1]];
val it =
  [[1776,7,4,0],[1776,7,4,1],[1776,7,14,0],[1776,7,14,1],[1776,7,23,0],
   [1776,7,23,1],[1776,12,4,0],[1776,12,4,1],[1776,12,14,0],[1776,12,14,1],
   [1776,12,23,0],[1776,12,23,1],[1789,7,4,0],[1789,7,4,1],[1789,7,14,0],
   [1789,7,14,1],[1789,7,23,0],[1789,7,23,1],[1789,12,4,0],[1789,12,4,1],
   [1789,12,14,0],[1789,12,14,1],[1789,12,23,0],[1789,12,23,1]]
  : int list list
- naryProdList [[1, 2, 3], [30], [500, 100]];
val it = [[1,30,500],[1,30,100],[2,30,500],[2,30,100],[3,30,500],[3,30,100]]
  : int list list
- naryProdList [[1, 2, 3], [], [500, 100]];
val it = [] : int list list
```



## Stata


In Stata, the command '''[https://www.stata.com/help.cgi?fillin fillin]''' may be used to expand a dataset with all combinations of a number of variables. Thus it's easy to compute a cartesian product.


```stata
. list

     +-------+
     | a   b |
     |-------|
  1. | 1   3 |
  2. | 2   4 |
     +-------+

. fillin a b
. list

     +-----------------+
     | a   b   _fillin |
     |-----------------|
  1. | 1   3         0 |
  2. | 1   4         1 |
  3. | 2   3         1 |
  4. | 2   4         0 |
     +-----------------+
```


The other way around:


```stata
. list

     +-------+
     | a   b |
     |-------|
  1. | 3   1 |
  2. | 4   2 |
     +-------+

. fillin a b
. list

     +-----------------+
     | a   b   _fillin |
     |-----------------|
  1. | 3   1         0 |
  2. | 3   2         1 |
  3. | 4   1         1 |
  4. | 4   2         0 |
     +-----------------+
```


Note, however, that this is not equivalent to a cartesian product when one of the variables is "empty" (that is, only contains missing values).


```stata
. list

     +-------+
     | a   b |
     |-------|
  1. | 1   . |
  2. | 2   . |
     +-------+

. fillin a b
. list

     +-----------------+
     | a   b   _fillin |
     |-----------------|
  1. | 1   .         0 |
  2. | 2   .         0 |
     +-----------------+
```


This command works also if the varaibles have different numbers of nonmissing elements. However, this requires additional code to remove the observations with missing values.


```stata
. list

     +-----------+
     | a   b   c |
     |-----------|
  1. | 1   4   6 |
  2. | 2   5   . |
  3. | 3   .   . |
     +-----------+

. fillin a b c
. list

     +---------------------+
     | a   b   c   _fillin |
     |---------------------|
  1. | 1   4   6         0 |
  2. | 1   4   .         1 |
  3. | 1   5   6         1 |
  4. | 1   5   .         1 |
  5. | 1   .   6         1 |
     |---------------------|
  6. | 1   .   .         1 |
  7. | 2   4   6         1 |
  8. | 2   4   .         1 |
  9. | 2   5   6         1 |
 10. | 2   5   .         0 |
     |---------------------|
 11. | 2   .   6         1 |
 12. | 2   .   .         1 |
 13. | 3   4   6         1 |
 14. | 3   4   .         1 |
 15. | 3   5   6         1 |
     |---------------------|
 16. | 3   5   .         1 |
 17. | 3   .   6         1 |
 18. | 3   .   .         0 |
     +---------------------+

. foreach var of varlist _all {
          quietly drop if missing(`var')
  }

. list

     +---------------------+
     | a   b   c   _fillin |
     |---------------------|
  1. | 1   4   6         0 |
  2. | 1   5   6         1 |
  3. | 2   4   6         1 |
  4. | 2   5   6         1 |
  5. | 3   4   6         1 |
     |---------------------|
  6. | 3   5   6         1 |
     +---------------------+
```




## Swift


{{trans|Scala}}


```swift>func + <T
(el: T, arr: [T]) -> [T] {
  var ret = arr

  ret.insert(el, at: 0)

  return ret
}

func cartesianProduct<T>(_ arrays: [T]...) -> [[T]] {
  guard let head = arrays.first else {
    return []
  }

  let first = Array(head)

  func pel(
    _ el: T,
    _ ll: [[T]],
    _ a: [[T]] = []
  ) -> [[T]] {
    switch ll.count {
    case 0:
      return a.reversed()
    case _:
      let tail = Array(ll.dropFirst())
      let head = ll.first!

      return pel(el, tail, el + head + a)
    }
  }

  return arrays.reversed()
    .reduce([first], {res, el in el.flatMap({ pel($0, res) }) })
    .map({ $0.dropLast(first.count) })
}


print(cartesianProduct([1, 2], [3, 4]))
print(cartesianProduct([3, 4], [1, 2]))
print(cartesianProduct([1, 2], []))
print(cartesianProduct([1776, 1789], [7, 12], [4, 14, 23], [0, 1]))
print(cartesianProduct([1, 2, 3], [30], [500, 100]))
print(cartesianProduct([1, 2, 3], [], [500, 100])
```


{{out}}


```txt
[[1, 3], [1, 4], [2, 3], [2, 4]]
[[3, 1], [3, 2], [4, 1], [4, 2]]
[]
[[1776, 7, 4, 0], [1776, 7, 4, 1], [1776, 7, 14, 0], [1776, 7, 14, 1], [1776, 7, 23, 0], [1776, 7, 23, 1], [1776, 12, 4, 0], [1776, 12, 4, 1], [1776, 12, 14, 0], [1776, 12, 14, 1], [1776, 12, 23, 0], [1776, 12, 23, 1], [1789, 7, 4, 0], [1789, 7, 4, 1], [1789, 7, 14, 0], [1789, 7, 14, 1], [1789, 7, 23, 0], [1789, 7, 23, 1], [1789, 12, 4, 0], [1789, 12, 4, 1], [1789, 12, 14, 0], [1789, 12, 14, 1], [1789, 12, 23, 0], [1789, 12, 23, 1]]
[[1, 30, 500], [1, 30, 100], [2, 30, 500], [2, 30, 100], [3, 30, 500], [3, 30, 100]]
[]
```



## Tailspin


```tailspin

templates cartesianProduct
  { product: [$(1)... -> [$]], rest: $(2..-1) } -> #
  <{ rest: <[](0)> }> $.product !
  <> def m: $.rest(1);
     { product: [$.product... -> (def n: $; $m... -> [$n..., $] !)], rest: $.rest(2..-1) } -> #
end cartesianProduct

'{1,2}x{3,4} = $:[[1,2],[3,4]] -> cartesianProduct;
' -> !OUT::write

'{3,4}x{1,2} = $:[[3,4],[1,2]] -> cartesianProduct;
' -> !OUT::write

'{1,2}x{} = $:[[1,2],[]] -> cartesianProduct;
' -> !OUT::write

'{}x{1,2} = $:[[],[1,2]] -> cartesianProduct;
' -> !OUT::write

'{1776, 1789} × {7, 12} × {4, 14, 23} × {0, 1} = $:[[1776, 1789], [7, 12], [4, 14, 23], [0, 1]] -> cartesianProduct;
' -> !OUT::write

'{1, 2, 3} × {30} × {500, 100} = $:[[1, 2, 3], [30], [500, 100]] -> cartesianProduct;
' -> !OUT::write

'{1, 2, 3} × {} × {500, 100} = $:[[1, 2, 3], [], [500, 100]] -> cartesianProduct;
' -> !OUT::write

```

{{out}}

```txt

{1,2}x{3,4} = [[1, 3], [1, 4], [2, 3], [2, 4]]
{3,4}x{1,2} = [[3, 1], [3, 2], [4, 1], [4, 2]]
{1,2}x{} = []
{}x{1,2} = []
{1776, 1789} × {7, 12} × {4, 14, 23} × {0, 1} = [[1776, 7, 4, 0], [1776, 7, 4, 1], [1776, 7, 14, 0], [1776, 7, 14, 1], [1776, 7, 23, 0], [1776, 7, 23, 1], [1776, 12, 4, 0], [1776, 12, 4, 1], [1776, 12, 14, 0], [1776, 12, 14, 1], [1776, 12, 23, 0], [1776, 12, 23, 1], [1789, 7, 4, 0], [1789, 7, 4, 1], [1789, 7, 14, 0], [1789, 7, 14, 1], [1789, 7, 23, 0], [1789, 7, 23, 1], [1789, 12, 4, 0], [1789, 12, 4, 1], [1789, 12, 14, 0], [1789, 12, 14, 1], [1789, 12, 23, 0], [1789, 12, 23, 1]]
{1, 2, 3} × {30} × {500, 100} = [[1, 30, 500], [1, 30, 100], [2, 30, 500], [2, 30, 100], [3, 30, 500], [3, 30, 100]]
{1, 2, 3} × {} × {500, 100} = []

```



## Tcl


```tcl

proc cartesianProduct {l1 l2} {
  set result {}
  foreach el1 $l1 {
    foreach el2 $l2 {
      lappend result [list $el1 $el2]
    }
  }
  return $result
}

puts "simple"
puts "result: [cartesianProduct {1 2} {3 4}]"
puts "result: [cartesianProduct {3 4} {1 2}]"
puts "result: [cartesianProduct {1 2} {}]"
puts "result: [cartesianProduct {} {3 4}]"

proc cartesianNaryProduct {lists} {
  set result {{}}
  foreach l $lists {
    set res {}
    foreach comb $result {
      foreach el $l {
        lappend res [linsert $comb end $el]
      }
    }
    set result $res
  }
  return $result
}

puts "n-ary"
puts "result: [cartesianNaryProduct {{1776 1789} {7 12} {4 14 23} {0 1}}]"
puts "result: [cartesianNaryProduct {{1 2 3} {30} {500 100}}]"
puts "result: [cartesianNaryProduct {{1 2 3} {} {500 100}}]"


```

{{out}}

```txt

simple
result: {1 3} {1 4} {2 3} {2 4}
result: {3 1} {3 2} {4 1} {4 2}
result:
result:
n-ary
result: {1776 7 4 0} {1776 7 4 1} {1776 7 14 0} {1776 7 14 1} {1776 7 23 0} {1776 7 23 1} {1776 12 4 0} {1776 12 4 1} {1776 12 14 0} {1776 12 14 1} {1776 12 23 0} {1776 12 23 1} {1789 7 4 0} {1789 7 4 1} {1789 7 14 0} {1789 7 14 1} {1789 7 23 0} {1789 7 23 1} {1789 12 4 0} {1789 12 4 1} {1789 12 14 0} {1789 12 14 1} {1789 12 23 0} {1789 12 23 1}
result: {1 30 500} {1 30 100} {2 30 500} {2 30 100} {3 30 500} {3 30 100}
result:

```




## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Runtime.CompilerServices

Module Module1

    <Extension()>
    Function CartesianProduct(Of T)(sequences As IEnumerable(Of IEnumerable(Of T))) As IEnumerable(Of IEnumerable(Of T))
        Dim emptyProduct As IEnumerable(Of IEnumerable(Of T)) = {Enumerable.Empty(Of T)}
        Return sequences.Aggregate(emptyProduct, Function(accumulator, sequence) From acc In accumulator From item In sequence Select acc.Concat({item}))
    End Function

    Sub Main()
        Dim empty(-1) As Integer
        Dim list1 = {1, 2}
        Dim list2 = {3, 4}
        Dim list3 = {1776, 1789}
        Dim list4 = {7, 12}
        Dim list5 = {4, 14, 23}
        Dim list6 = {0, 1}
        Dim list7 = {1, 2, 3}
        Dim list8 = {30}
        Dim list9 = {500, 100}

        For Each sequnceList As Integer()() In {
            ({list1, list2}),
            ({list2, list1}),
            ({list1, empty}),
            ({empty, list1}),
            ({list3, list4, list5, list6}),
            ({list7, list8, list9}),
            ({list7, empty, list9})
        }
            Dim cart = sequnceList.CartesianProduct().Select(Function(tuple) $"({String.Join(", ", tuple)})")
            Console.WriteLine($"{{{String.Join(", ", cart)}}}")
        Next
    End Sub

End Module
```

{{out}}

```txt
{(1, 3), (1, 4), (2, 3), (2, 4)}
{(3, 1), (3, 2), (4, 1), (4, 2)}
{}
{}
{(1776, 7, 4, 0), (1776, 7, 4, 1), (1776, 7, 14, 0), (1776, 7, 14, 1), (1776, 7, 23, 0), (1776, 7, 23, 1), (1776, 12, 4, 0), (1776, 12, 4, 1), (1776, 12, 14, 0), (1776, 12, 14, 1), (1776, 12, 23, 0), (1776, 12, 23, 1), (1789, 7, 4, 0), (1789, 7, 4, 1), (1789, 7, 14, 0), (1789, 7, 14, 1), (1789, 7, 23, 0), (1789, 7, 23, 1), (1789, 12, 4, 0), (1789, 12, 4, 1), (1789, 12, 14, 0), (1789, 12, 14, 1), (1789, 12, 23, 0), (1789, 12, 23, 1)}
{(1, 30, 500), (1, 30, 100), (2, 30, 500), (2, 30, 100), (3, 30, 500), (3, 30, 100)}
{}
```



## zkl

Cartesian product is build into iterators or can be done with nested
loops.

```zkl
zkl: Walker.cproduct(List(1,2),List(3,4)).walk().println();
L(L(1,3),L(1,4),L(2,3),L(2,4))
zkl: foreach a,b in (List(1,2),List(3,4)){ print("(%d,%d) ".fmt(a,b)) }
(1,3) (1,4) (2,3) (2,4)

zkl: Walker.cproduct(List(3,4),List(1,2)).walk().println();
L(L(3,1),L(3,2),L(4,1),L(4,2))
```


The walk method will throw an error if used on an empty iterator but the pump
method doesn't.

```zkl
zkl: Walker.cproduct(List(3,4),List).walk().println();
Exception thrown: TheEnd(Ain't no more)

zkl: Walker.cproduct(List(3,4),List).pump(List).println();
L()
zkl: Walker.cproduct(List,List(3,4)).pump(List).println();
L()
```


```zkl
zkl: Walker.cproduct(L(1776,1789),L(7,12),L(4,14,23),L(0,1)).walk().println();
L(L(1776,7,4,0),L(1776,7,4,1),L(1776,7,14,0),L(1776,7,14,1),L(1776,7,23,0),L(1776,7,23,1),L(1776,12,4,0),L(1776,12,4,1),L(1776,12,14,0),L(1776,12,14,1),L(1776,12,23,0),L(1776,12,23,1),L(1789,7,4,0),L(1789,7,4,1),L(1789,7,14,0),L(1789,7,14,1),L(1789,7,23,0),L(1789,7,23,1),L(1789,12,4,0),L(1789,12,4,1),...)

zkl: Walker.cproduct(L(1,2,3),L(30),L(500,100)).walk().println();
L(L(1,30,500),L(1,30,100),L(2,30,500),L(2,30,100),L(3,30,500),L(3,30,100))

zkl: Walker.cproduct(L(1,2,3),List,L(500,100)).pump(List).println();
L()
```

