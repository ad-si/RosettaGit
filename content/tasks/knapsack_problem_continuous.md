+++
title = "Knapsack problem/Continuous"
description = ""
date = 2019-08-22T01:28:52Z
aliases = []
[extra]
id = 6122
[taxonomies]
categories = ["task", "Classic CS problems and programs"]
tags = []
languages = [
  "ada",
  "awk",
  "bbc_basic",
  "befunge",
  "bracmat",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "eiffel",
  "elixir",
  "erlang",
  "forth",
  "fortran",
  "gnu_apl",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "mathematica",
  "mathprog",
  "ocaml",
  "oforth",
  "oorexx",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "related_tasks",
  "rexx",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "scala",
  "sidef",
  "tcl",
  "ursala",
  "xpl0",
  "zkl",
]
+++

<!-- a thief  (or burglar)  steals,  a robber robs  (confronts a person while stealing).      Not exactly a perfect definition,  but close enough.  -- Gerard Schildberger. -->

A thief burgles a butcher's shop, where he can select from some items.

The thief knows the weights and prices of each items.   Because he has a knapsack with 15 kg maximal capacity, he wants to select the items such that he would have his profit maximized.   He may cut the items;   the item has a reduced price after cutting that is proportional to the original price by the ratio of masses.   That means:   half of an item has half the price of the original.


This is the item list in the butcher's shop:

{| style="text-align: left; width: 50%;" border="4" cellpadding="2" cellspacing="2"
|+ Table of potential knapsack items
|- style="background-color: rgb(255, 204, 255);"
! Item !! Weight (kg) !! Price (Value)
|-
| beef || 3.8 || 36
|-
| pork || 5.4 || 43
|-
| ham || 3.6 || 90
|-
| greaves || 2.4 || 45
|-
| flitch || 4.0 || 30
|-
| brawn || 2.5 || 56
|-
| welt || 3.7 || 67
|-
| salami || 3.0 || 95
|-
| sausage || 5.9 || 98
|- style="background-color: rgb(255, 204, 255);"
| Knapsack || &lt;=15 kg || ?
|}




## Task

Show which items the thief carries in his knapsack so that their total weight does not exceed 15 kg, and their total value is maximized.


## Related tasks

*   [[Knapsack problem/Bounded]]
*   [[Knapsack problem/Unbounded]]
*   [[Knapsack problem/0-1]]




## See also

*   Wikipedia article:   [[wp:Continuous_knapsack_problem|continuous knapsack]].





## Ada


```Ada
with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;

procedure Knapsack_Continuous is
   package US renames Ada.Strings.Unbounded;

   type Item is record
      Name   : US.Unbounded_String;
      Weight : Float;
      Value  : Positive;
      Taken  : Float;
   end record;

   function "<" (Left, Right : Item) return Boolean is
   begin
      return Float (Left.Value) / Left.Weight <
             Float (Right.Value) / Right.Weight;
   end "<";

   type Item_Array is array (Positive range <>) of Item;

   function Total_Weight (Items : Item_Array) return Float is
      Sum : Float := 0.0;
   begin
      for I in Items'Range loop
         Sum := Sum + Items (I).Taken;
      end loop;
      return Sum;
   end Total_Weight;

   function Total_Value (Items : Item_Array) return Float is
      Sum : Float := 0.0;
   begin
      for I in Items'Range loop
         Sum := Sum + Float (Items (I).Value) / Items(I).Weight * Items (I).Taken;
      end loop;
      return Sum;
   end Total_Value;

   procedure Solve_Knapsack_Continuous
     (Items        : in out Item_Array;
      Weight_Limit : Float)
   is
   begin
      -- order items by value per weight unit
      Sorting : declare
         An_Item : Item;
         J       : Natural;
      begin
         for I in Items'First + 1 .. Items'Last loop
            An_Item := Items (I);
            J       := I - 1;
            while J in Items'Range and then Items (J) < An_Item loop
               Items (J + 1) := Items (J);
               J             := J - 1;
            end loop;
            Items (J + 1) := An_Item;
         end loop;
      end Sorting;
      declare
         Rest : Float := Weight_Limit;
      begin
         for I in Items'Range loop
            if Items (I).Weight <= Rest then
               Items (I).Taken := Items (I).Weight;
            else
               Items (I).Taken := Rest;
            end if;
            Rest := Rest - Items (I).Taken;
            exit when Rest <= 0.0;
         end loop;
      end;
   end Solve_Knapsack_Continuous;
    All_Items : Item_Array :=
     ((US.To_Unbounded_String ("beef"), 3.8, 36, 0.0),
      (US.To_Unbounded_String ("pork"), 5.4, 43, 0.0),
      (US.To_Unbounded_String ("ham"), 3.6, 90, 0.0),
      (US.To_Unbounded_String ("greaves"), 2.4, 45, 0.0),
      (US.To_Unbounded_String ("flitch"), 4.0, 30, 0.0),
      (US.To_Unbounded_String ("brawn"), 2.5, 56, 0.0),
      (US.To_Unbounded_String ("welt"), 3.7, 67, 0.0),
      (US.To_Unbounded_String ("salami"), 3.0, 95, 0.0),
      (US.To_Unbounded_String ("sausage"), 5.9, 98, 0.0));

begin
   Solve_Knapsack_Continuous (All_Items, 15.0);
   Ada.Text_IO.Put ("Total Weight: ");
   Ada.Float_Text_IO.Put (Total_Weight (All_Items), 0, 2, 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Total Value:  ");
   Ada.Float_Text_IO.Put (Total_Value (All_Items), 0, 2, 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Items:");
   for I in All_Items'Range loop
      if All_Items (I).Taken > 0.0 then
         Ada.Text_IO.Put ("   ");
         Ada.Float_Text_IO.Put (All_Items (I).Taken, 0, 2, 0);
         Ada.Text_IO.Put_Line (" of " & US.To_String (All_Items (I).Name));
      end if;
   end loop;
end Knapsack_Continuous;

```

```txt

Total Weight: 15.00
Total Value:  349.38
Items:
   3.00 of salami
   3.60 of ham
   2.50 of brawn
   2.40 of greaves
   3.50 of welt

```



## GNU APL


```APL
⍝ Data
Items←'beef' 'pork' 'ham' 'greaves' 'flitch' 'brawn' 'welt' 'salami' 'sausage'
Weights←3.8 5.4 3.6 2.4 4 2.5 3.7 3 5.9
Prices←36 43 90 45 30 56 67 95 98

⍝ Solution
Order←⍒Worth←Prices÷Weights                    ⍝ 'Worth' is each item value for 1 kg.
diff←{¯1↓(⍵,0)-0,⍵}                             ⍝ 'diff' between each item and the prev item (the inverse of '+\').
Filter←×Selected←diff 15⌊+\Weights[Order]      ⍝ 'Selected' weights totaling 15kg, others 0.
Table←⊃{⍺,⍪⍵}/Items Weights Selected[⍋Order]
Take←Filter[⍋Order]/[1]Table
TotalCost←+/Prices×Selected[⍋Order]÷Weights

⍝ Output
⎕←'ITEM' 'WEIGHT AVAILABLE' 'WEIGHT SELECTED' ⍪ Take
⎕←''
⎕←'total cost:' TotalCost

```

```txt

 ITEM      WEIGHT AVAILABLE   WEIGHT SELECTED
 ham                    3.6               3.6
 greaves                2.4               2.4
 brawn                  2.5               2.5
 welt                   3.7               3.5
 salami                 3                 3

 total cost: 349.3783784

```



## AWK


```AWK
# syntax: GAWK -f KNAPSACK_PROBLEM_CONTINUOUS.AWK
BEGIN {
#   arr["item,weight,price"]
    arr["beef,3.8,36"]
    arr["pork,5.4,43"]
    arr["ham,3.6,90"]
    arr["greaves,2.4,45"]
    arr["flitch,4.0,30"]
    arr["brawn,2.5,56"]
    arr["welt,3.7,67"]
    arr["salami,3.0,95"]
    arr["sausage,5.9,98"]
    for (i in arr) {
      split(i,tmp,",")
      arr[i] = tmp[3] / tmp[2] # $/unit
    }
    sack_size = 15 # kg
    PROCINFO["sorted_in"] = "@val_num_desc"
    print("item    weight  price $/unit")
    for (i in arr) {
      if (total_weight >= sack_size) {
        break
      }
      split(i,tmp,",")
      weight = tmp[2]
      if (total_weight + weight <= sack_size) {
        price = tmp[3]
        msg = "all"
      }
      else {
        weight = sack_size - total_weight
        price = weight * arr[i]
        msg = weight " of " tmp[2]
      }
      printf("%-7s %6.2f %6.2f %6.2f take %s\n",tmp[1],weight,tmp[3],arr[i],msg)
      total_items++
      total_price += price
      total_weight += weight
    }
    printf("%7d %6.2f %6.2f total\n",total_items,total_weight,total_price)
    exit(0)
}

```

```txt

item    weight  price $/unit
salami    3.00  95.00  31.67 take all
ham       3.60  90.00  25.00 take all
brawn     2.50  56.00  22.40 take all
greaves   2.40  45.00  18.75 take all
welt      3.50  67.00  18.11 take 3.5 of 3.7
      5  15.00 349.38 total

```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"SORTSALIB"
      Sort% = FN_sortSAinit(1, 0) : REM Descending

      nItems% = 9
      maxWeight = 15.0

      DIM items{(nItems%-1) name$, weight, price, worth}
      FOR item% = 0 TO nItems%-1
        READ items{(item%)}.name$, items{(item%)}.weight, items{(item%)}.price
        items{(item%)}.worth = items{(item%)}.price / items{(item%)}.weight
      NEXT

      DATA "beef", 3.8, 36, "pork", 5.4, 43, "ham", 3.6, 90
      DATA "greaves", 2.4, 45, "flitch", 4.0, 30, "brawn", 2.5, 56
      DATA "welt", 3.7, 67, "salami", 3.0, 95, "sausage", 5.9, 98

      C% = nItems% : D% = 0
      CALL Sort%, items{()}, items{(0)}.worth

      TotalWeight = 0
      TotalPrice = 0
      FOR i% = 0 TO nItems%-1
        IF TotalWeight + items{(i%)}.weight < maxWeight THEN
          TotalWeight += items{(i%)}.weight
          TotalPrice += items{(i%)}.price
          PRINT "Take all the " items{(i%)}.name$
        ELSE
          weight = maxWeight - TotalWeight
          price = weight * items{(i%)}.worth
          TotalWeight += weight
          TotalPrice += price
          PRINT "Take "; weight " kg of " items{(i%)}.name$
          EXIT FOR
        ENDIF
      NEXT

      PRINT '"Total weight = " ; TotalWeight " kg"
      PRINT "Total price  = " ; TotalPrice
      END
```

Output:

```txt
Take all the salami
Take all the ham
Take all the brawn
Take all the greaves
Take 3.5 kg of welt

Total weight = 15 kg
Total price  = 349.378379

```



## Befunge

The table of weights and prices are stored as strings to make them easier to edit. Two characters for the weight (with the decimal point dropped), two characters for the price, and then the name of the item. The total numbers of items (9) is specified by the first value on the stack.


```befunge
9:02p>
:5+::::::0\g68*-55+*\1\g68*-+\0\pv>2gg!*::!2v
>\`!v|:-1p\3\0p\2\+-*86g\3\*+55-*86g\2<<1v*g21\*g2<
nib@_>0022p6>12p:212gg48*:**012gg/\-:0`3^+>,,55+%6v
#v0pg2231$$_^#`+5g20:+1g21$_+#!:#<0#<<p22<\v84,+*8<
*>22gg+::55*6*`\55*6*-*022gg\-:55+/68*+"."^>*"fo "v
^6*55:,+55$$_,#!1#`+#*:#82#42#:g<g22:4,,,,,,," kg"<
3836beef
5443pork
3690ham
2445greaves
4030flitch
2556brawn
3767welt
3095salami
5998sausage
```


```txt
3.0 kg of salami
3.6 kg of ham
2.5 kg of brawn
2.4 kg of greaves
3.5 kg of welt
```



## Bracmat


```bracmat
( ( fixed    {function to convert a rational number to fixed point notation.
                            The second argument is the number of decimals. }
  =   value decimals powerOf10
    .   !arg:(?value.?decimals)
      & 10^!decimals:?powerOf10
      &   str
        $ ( div$(!value.1)
            "."
              mod
            $ (div$(!value+1/2*!powerOf10^-1.!powerOf10^-1).!powerOf10)
          )
  )
&     (beef.38/10.36)
      (pork.54/10.43)
      (ham.36/10.90)
      (greaves.24/10.45)
      (flitch.40/10.30)
      (brawn.25/10.56)
      (welt.37/10.67)
      (salami.30/10.95)
      (sausage.59/10.98)
  : ?items
& 0:?sorteditems
&   whl
  ' ( !items:(?name.?mass.?price) ?items
    & (!mass*!price^-1.!mass.!name)+!sorteditems:?sorteditems
    )
& 0:?totalMass
& :?stolenItems
&   whl
  ' ( !sorteditems:(?massPerPriceunit.?mass.?name)+?sorteditems
    &   (!mass.!massPerPriceunit.!name) !stolenItems
      : ?stolenItems
    & !mass+!totalMass:?totalMass:~>15
    )
& !stolenItems:(?mass.?massPerPriceunit.?name) ?stolenItems
& 15+!mass+-1*!totalMass:?mass
& (!mass.!massPerPriceunit.!name) !stolenItems:?stolenItems
& 0:?totalPrice
& (   !stolenItems
    :   ?
        ( (?mass.?massPerPriceunit.?name)
        & out$(fixed$(!mass.1) "kg of" !name)
        & !mass*!massPerPriceunit^-1+!totalPrice:?totalPrice
        & ~
        )
        ?
  | out$(fixed$(!totalPrice.2))
  )
);
```

Output:
```txt
3.5 kg of welt
2.4 kg of greaves
2.5 kg of brawn
3.6 kg of ham
3.0 kg of salami
349.38
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

struct item { double w, v; const char *name; } items[] = {
	{ 3.8, 36, "beef" },
	{ 5.4, 43, "pork" },
	{ 3.6, 90, "ham" },
	{ 2.4, 45, "greaves" },
	{ 4.0, 30, "flitch" },
	{ 2.5, 56, "brawn" },
	{ 3.7, 67, "welt" },
	{ 3.0, 95, "salami" },
	{ 5.9, 98, "sausage" },
};

int item_cmp(const void *aa, const void *bb)
{
	const struct item *a = aa, *b = bb;
	double ua = a->v / a->w, ub = b->v / b->w;
	return ua < ub ? -1 : ua > ub;
}

int main()
{
	struct item *it;
	double space = 15;

	qsort(items, 9, sizeof(struct item), item_cmp);
	for (it = items + 9; it---items && space > 0; space -= it->w)
		if (space >= it->w)
			printf("take all %s\n", it->name);
		else
			printf("take %gkg of %g kg of %s\n",
				space, it->w, it->name);

	return 0;
}
```
output
```txt

take all salami
take all ham
take all brawn
take all greaves
take 3.5kg of 3.7 kg of welt

```



## C#


```c#
using System;  //4790@3.6
class Program
{
    static void Main()
    {
        Console.WriteLine(knapSack(15) + "\n");
        var sw = System.Diagnostics.Stopwatch.StartNew();
        for (int i = 1000; i > 0; i--) knapSack(15);
        Console.Write(sw.Elapsed); Console.Read();    // 0.60 µs
    }

    static string knapSack(double w1)
    {
        int k = w.Length; var q = new double[k];
        for (int i = 0; i < k; ) q[i] = v[i] / w[i++];
        var c = new double[k];
        Array.Copy(q, c, k); Array.Sort(c, w);
        Array.Copy(q, c, k); Array.Sort(c, v);
        Array.Sort(q, items);
        string str = "";
        for (k--; k >= 0; k--)
            if (w1 - w[k] > 0) { w1 -= w[k]; str += items[k] + "\n"; }
            else break;
        return w1 > 0 && k >= 0 ? str + items[k] : str;
    }

    static double[] w = { 3.8, 5.4, 3.6, 2.4, 4.0, 2.5, 3.7, 3.0, 5.9 },

                    v = { 36, 43, 90, 45, 30, 56, 67, 95, 98 };

    static string[] items = {"beef","pork","ham","greaves","flitch",
                             "brawn","welt","salami","sausage"};
}
```

Sorting three times is expensive,
an alternative is sorting once, with an indices array.

```c#
using System;
class Program
{
    static void Main()
    {
        Console.WriteLine(knapSack(15) + "\n");
        var sw = System.Diagnostics.Stopwatch.StartNew();
        for (int i = 1000; i > 0; i--) knapSack(15);
        Console.Write(sw.Elapsed); Console.Read();    // 0.37 µs
    }

    static string knapSack(double w1)
    {
        int i = 0, k = w.Length; var idx = new int[k];
        {
            var q = new double[k];
            while (i < k) q[i] = v[i] / w[idx[i] = i++];
            Array.Sort(q, idx);
        }
        string str = "";
        for (k--; k >= 0; k--)
            if (w1 > w[i = idx[k]]) { w1 -= w[i]; str += items[i] + "\n"; }
            else break;
        return w1 > 0 && k >= 0 ? str + items[idx[k]] : str;
    }

    static double[] w = { 3.8, 5.4, 3.6, 2.4, 4.0, 2.5, 3.7, 3.0, 5.9 },

                    v = { 36, 43, 90, 45, 30, 56, 67, 95, 98 };

    static string[] items = {"beef","pork","ham","greaves","flitch",
                             "brawn","welt","salami","sausage"};
}
```



## Clojure


```Clojure

; Solve Continuous Knapsack Problem
; Nicolas Modrzyk
; January 2015

(def maxW 15.0)
(def items
  {:beef    [3.8 36]
   :pork    [5.4 43]
   :ham     [3.6 90]
   :greaves [2.4 45]
   :flitch  [4.0 30]
   :brawn   [2.5 56]
   :welt    [3.7 67]
   :salami  [3.0 95]
   :sausage [5.9 98]})

(defn rob [items maxW]
  (let[
    val-item
        (fn[key]
          (- (/ (second (items key)) (first (items key )))))
    compare-items
        (fn[key1 key2]
          (compare (val-item key1) (val-item key2)))
    sorted (into (sorted-map-by compare-items) items)]

  (loop [current (first sorted)
         array (rest sorted)
         value 0
         weight 0]
    (let[new-weight (first (val current))
         new-value (second (val current))]
    (if (> (- maxW weight new-weight) 0)
      (do
        (println "Take all " (key current))
        (recur
         (first array)
         (rest array)
         (+ value new-value)
         (+ weight new-weight)))
      (let [t (- maxW weight)] ; else
      (println
       "Take " t " of "
       (key current) "\n"
       "Total Value is:"
       (+ value (* t (/ new-value new-weight))))))))))

(rob items maxW)

```

Output
```txt

Take all  :salami
Take all  :ham
Take all  :brawn
Take all  :greaves
Take  3.5  of  :welt
 Total Value is: 349.3783783783784

```



### Alternate Version


```Clojure

(def items
  [{:name "beef" :weight 3.8 :price 36}
   {:name "pork" :weight 5.4 :price 43}
   {:name "ham" :weight 3.6 :price 90}
   {:name "graves" :weight 2.4 :price 45}
   {:name "flitch" :weight 4.0 :price 30}
   {:name "brawn" :weight 2.5 :price 56}
   {:name "welt" :weight 3.7 :price 67}
   {:name "salami" :weight 3.0 :price 95}
   {:name "sausage" :weight 5.9 :price 98}])

(defn per-kg [item] (/ (:price item) (:weight item)))

(defn rob [items capacity]
  (let [best-items (reverse (sort-by per-kg items))]
    (loop [items best-items cap capacity total 0]
      (let [item (first items)]
        (if (< (:weight item) cap)
          (do (println (str "Take all " (:name item)))
              (recur (rest items) (- cap (:weight item)) (+ total (:price item))))
          (println (format "Take %.1f kg of %s\nTotal: %.2f monies"
                           cap (:name item) (+ total (* cap (per-kg item))))))))))

(rob items 15)

```



## C++


```cpp
#include<iostream>
#include<algorithm>
#include<string.h>

using namespace std;
double result;
double capacity = 15;
int NumberOfItems;
int number;

struct items
{
    char name[32];
    double weight;
    double price;
    double m;
} item[256];

bool cmp(items a,items b)
{
    return a.price/a.weight > b.price/b.weight; // the compare function for the sorting algorithm
}

int main()
{
NumberOfItems=9;
strcpy(item[1].name,"beef");
item[1].weight=3.8;
item[1].price=36;

strcpy(item[2].name,"pork");
item[2].weight=5.4;
item[2].price=43;

strcpy(item[3].name,"ham");
item[3].weight=3.6;
item[3].price=90;

strcpy(item[4].name,"greaves");
item[4].weight=2.4;
item[4].price=45;

strcpy(item[5].name,"flitch");
item[5].weight=4.0;
item[5].price=30;

strcpy(item[6].name,"brawn");
item[6].weight=2.5;
item[6].price=56;

strcpy(item[7].name,"welt");
item[7].weight=3.7;
item[7].price=67;

strcpy(item[8].name,"salami");
item[8].weight=3.0;
item[8].price=95;

strcpy(item[9].name,"sausage");
item[9].weight=5.9;
item[9].price=98;


sort(item+1,item+NumberOfItems+1,cmp); // We'll sort using Introsort from STL

 number = 1;
 while(capacity>0&&number<=NumberOfItems)
 {
  if(item[number].weight<=capacity)
    {
        result+=item[number].price;
        capacity-=item[number].weight;
        item[number].m=1;
    }
  else
  {
      result+=(item[number].price)*(capacity/item[number].weight);
      item[number].m=(capacity/item[number].weight);
      capacity=0;

  }
  ++number;
 }

cout<<"Total Value = "<<result<<'\n';
cout<<"Total Weight = "<<(double)15-capacity<<'\n';
cout<<"Items Used:\n";
for(int i=1;i<=NumberOfItems;++i)
    if(item[i].m)
    {
       cout<<"We took "<<item[i].m*item[i].weight<<"kg of \""<<item[i].name<<"\" and the value it brought is "<<item[i].price*item[i].m<<"\n";
    }

return 0;
}
```


```txt

Total Value = 349.378
Total Weight = 15
Items Used:
We took 3kg of "salami" and the value it brought is 95
We took 3.6kg of "ham" and the value it brought is 90
We took 2.5kg of "brawn" and the value it brought is 56
We took 2.4kg of "greaves" and the value it brought is 45
We took 3.5kg of "welt" and the value it brought is 63.3784

```



### Alternate Version


```cpp
// C++11 version
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

using namespace std;

struct item_type
{
    double  weight, value;
    string  name;
};

vector< item_type > items =
{
    { 3.8, 36, "beef"    },
    { 5.4, 43, "pork"    },
    { 3.6, 90, "ham"     },
    { 2.4, 45, "greaves" },
    { 4.0, 30, "flitch"  },
    { 2.5, 56, "brawn"   },
    { 3.7, 67, "welt"    },
    { 3.0, 95, "salami"  },
    { 5.9, 98, "sausage" }
};

int main()
{
    sort
    (
        begin( items ), end( items ),
        [] (const item_type& a, const item_type& b)
        {
            return a.value / a.weight > b.value / b.weight;
        }
    );

    double space = 15;

    for ( const auto& item : items )
    {
        if ( space >= item.weight )
            cout << "Take all " << item.name << endl;
        else
        {
            cout << "Take " << space << "kg of " << item.name << endl;
            break;
        }

        space -= item.weight;
    }
}
```


```txt

Take all salami
Take all ham
Take all brawn
Take all greaves
Take 3.5kg of welt

```



## Common Lisp


```lisp
(defstruct item
  (name nil :type string)
  (weight nil :type real)
  (price nil :type real))

(defun price-per-weight (item)
  (/ (item-price item) (item-weight item)))

(defun knapsack (items total-weight)
  (loop with sorted = (sort items #'> :key #'price-per-weight)
        while (plusp total-weight)
        for item in sorted
        for amount = (min (item-weight item) total-weight)
        collect (list (item-name item) amount)
        do (decf total-weight amount)))

(defun main ()
  (let ((items (list (make-item :name "beef"    :weight 3.8 :price 36)
                     (make-item :name "pork"    :weight 5.4 :price 43)
                     (make-item :name "ham"     :weight 3.6 :price 90)
                     (make-item :name "greaves" :weight 2.4 :price 45)
                     (make-item :name "flitch"  :weight 4.0 :price 30)
                     (make-item :name "brawn"   :weight 2.5 :price 56)
                     (make-item :name "welt"    :weight 3.7 :price 67)
                     (make-item :name "salami"  :weight 3.0 :price 95)
                     (make-item :name "sausage" :weight 5.9 :price 98))))
    (loop for (name amount) in (knapsack items 15)
          do (format t "~8A: ~,2F kg~%" name amount))))
```

```txt
salami  : 3.00 kg
ham     : 3.60 kg
brawn   : 2.50 kg
greaves : 2.40 kg
welt    : 3.50 kg
```



## D


```d
import std.stdio, std.algorithm, std.string;

struct Item {
    string name;
    real amount, value;

    @property real valuePerKG() @safe const pure nothrow @nogc {
        return value / amount;
    }

    string toString() const pure /*nothrow*/ @safe {
        return format("%10s %7.2f %7.2f %7.2f",
                      name, amount, value, valuePerKG);
    }
}

real sumBy(string field)(in Item[] items) @safe pure nothrow @nogc {
    return reduce!("a + b." ~ field)(0.0L, items);
}

void main() /*@safe*/ {
    const items = [Item("beef",    3.8, 36.0),
                   Item("pork",    5.4, 43.0),
                   Item("ham",     3.6, 90.0),
                   Item("greaves", 2.4, 45.0),
                   Item("flitch",  4.0, 30.0),
                   Item("brawn",   2.5, 56.0),
                   Item("welt",    3.7, 67.0),
                   Item("salami",  3.0, 95.0),
                   Item("sausage", 5.9, 98.0)]
                  .schwartzSort!(it => -it.valuePerKG)
                  .release;

    immutable(Item)[] chosen;
    real space = 15.0;
    foreach (const item; items)
        if (item.amount < space) {
            chosen ~= item;
            space -= item.amount;
        } else {
            chosen ~= Item(item.name, space, item.valuePerKG * space);
            break;
        }

    writefln("%10s %7s %7s %7s", "ITEM", "AMOUNT", "VALUE", "$/unit");
    writefln("%(%s\n%)", chosen);
    Item("TOTAL", chosen.sumBy!"amount", chosen.sumBy!"value").writeln;
}
```

```txt
      ITEM  AMOUNT   VALUE  $/unit
    salami    3.00   95.00   31.67
       ham    3.60   90.00   25.00
     brawn    2.50   56.00   22.40
   greaves    2.40   45.00   18.75
      welt    3.50   63.38   18.11
     TOTAL   15.00  349.38   23.29
```



### Alternative Version


```d
void main() {
    import std.stdio, std.algorithm;

    static struct T { string item; double weight, price; }

    auto items = [T("beef",    3.8, 36.0),
                  T("pork",    5.4, 43.0),
                  T("ham",     3.6, 90.0),
                  T("greaves", 2.4, 45.0),
                  T("flitch",  4.0, 30.0),
                  T("brawn",   2.5, 56.0),
                  T("welt",    3.7, 67.0),
                  T("salami",  3.0, 95.0),
                  T("sausage", 5.9, 98.0)]
                 .schwartzSort!q{ -a.price / a.weight };

    auto left = 15.0;
    foreach (it; items)
        if (it.weight <= left) {
            writeln("Take all the ", it.item);
            if (it.weight == left)
                return;
            left -= it.weight;
        } else
            return writefln("Take %.1fkg %s", left, it.item);
}
```

```txt
Take all the salami
Take all the ham
Take all the brawn
Take all the greaves
Take 3.5kg welt
```



## EchoLisp


```scheme

(lib 'struct)
(lib 'sql) ;; for table

(define T (make-table (struct meal (name poids price))))

(define meals
  '((🐂-beef 	3.8 	36)
    (🍖-pork 	5.4 	43)
    (🍗-ham 	3.6 	90)
    (🐪-greaves 	2.4 	45)
    (flitch 	4.0 	30)
    (brawn 	2.5 	56)
    (welt 	3.7 	67)
    (🐃--salami 	3.0 	95)
    (🐖-sausage 	5.9 	98)))

(list->table meals T)
;; sort table according to best price/poids ratio
(define (price/poids a b  )
	(-  (//  (* (meal-price b) (meal-poids a)) (meal-price a) (meal-poids b)) 1))
(table-sort T price/poids)

(define-syntax-rule (name i) (table-xref T i 0))
(define-syntax-rule (poids i) (table-xref T i 1))

;; shop : add items in basket, in order, until W exhausted
(define (shop W )
	(for/list ((i (table-count T)))
	#:break (<= W 0)
		(begin0
		(cons (name i) (if  (<= (poids i) W) 'all W))
		(set! W (- W (poids i))))))

;; output
(shop 15)
   → ((🐃--salami . all) (🍗-ham . all) (brawn . all) (🐪-greaves . all) (welt . 3.5))



```



## Eiffel


```Eiffel

class
	CONTINUOUS_KNAPSACK

create
	make

feature

	make
		local
			tup: TUPLE [name: STRING; weight: REAL_64; price: REAL_64]
		do
			create tup
			create items.make_filled (tup, 1, 9)
			create sorted.make
			sorted.extend (-36.0 / 3.8)
			sorted.extend (-43.0 / 5.4)
			sorted.extend (-90.0 / 3.6)
			sorted.extend (-45.0 / 2.4)
			sorted.extend (-30.0 / 4.0)
			sorted.extend (-56.0 / 2.5)
			sorted.extend (-67.0 / 3.7)
			sorted.extend (-95.0 / 3.0)
			sorted.extend (-98.0 / 5.9)
			tup := ["beef", 3.8, 36.0]
			items [sorted.index_of (- tup.price / tup.weight, 1)] := tup
			tup := ["pork", 5.4, 43.0]
			items [sorted.index_of (- tup.price / tup.weight, 1)] := tup
			tup := ["ham", 3.6, 90.0]
			items [sorted.index_of (- tup.price / tup.weight, 1)] := tup
			tup := ["greaves", 2.4, 45.0]
			items [sorted.index_of (- tup.price / tup.weight, 1)] := tup
			tup := ["flitch", 4.0, 30.0]
			items [sorted.index_of (- tup.price / tup.weight, 1)] := tup
			tup := ["brawn", 2.5, 56.0]
			items [sorted.index_of (- tup.price / tup.weight, 1)] := tup
			tup := ["welt", 3.7, 67.0]
			items [sorted.index_of (- tup.price / tup.weight, 1)] := tup
			tup := ["salami", 3.0, 95.0]
			items [sorted.index_of (- tup.price / tup.weight, 1)] := tup
			tup := ["sausage", 5.9, 98.0]
			items [sorted.index_of (- tup.price / tup.weight, 1)] := tup
			find_solution
		end

	find_solution
			-- Solution for the continuous Knapsack Problem.
		local
			maxW, value: REAL_64
		do
			maxW := 15
			across
				items as c
			loop
				if maxW - c.item.weight > 0 then
					io.put_string ("Take all: " + c.item.name + ".%N")
					value := value + c.item.price
					maxW := maxW - c.item.weight
				elseif maxW /= 0 then
					io.put_string ("Take " + maxW.truncated_to_real.out + " kg off " + c.item.name + ".%N")
					io.put_string ("The total value is " + (value + (c.item.price / c.item.weight) * maxW).truncated_to_real.out + ".")
					maxW := 0
				end
			end
		end

	items: ARRAY [TUPLE [name: STRING; weight: REAL_64; price: REAL_64]]

	sorted: SORTED_TWO_WAY_LIST [REAL_64]

end

```

```txt

Take all: salami.
Take all: ham.
Take all: brawn.
Take all: greaves.
Take 3.5kg off welt.
The total value is 349.378

```



## Elixir

```elixir
defmodule KnapsackProblem do
  def select( max_weight, items ) do
    Enum.sort_by( items, fn {_name, weight, price} -> - price / weight end )
    |> Enum.reduce( {max_weight, []}, &select_until/2 )
    |> elem(1)
    |> Enum.reverse
  end

  def task( items, max_weight ) do
    IO.puts "The robber takes the following to maximize the value"
    Enum.each( select( max_weight, items ), fn {name, weight} ->
      :io.fwrite("~.2f of ~s~n", [weight, name])
    end )
  end

  defp select_until( {name, weight, _price}, {remains, acc} ) when remains > 0 do
    selected_weight = select_until_weight( weight, remains )
    {remains - selected_weight, [{name, selected_weight} | acc]}
  end
  defp select_until( _item, acc ), do: acc

  defp select_until_weight( weight, remains ) when weight < remains, do: weight
  defp select_until_weight( _weight, remains ), do: remains
end

items = [ {"beef",    3.8, 36},
          {"pork",    5.4, 43},
          {"ham",     3.6, 90},
          {"greaves", 2.4, 45},
          {"flitch",  4.0, 30},
          {"brawn",   2.5, 56},
          {"welt",    3.7, 67},
          {"salami",  3.0, 95},
          {"sausage", 5.9, 98} ]

KnapsackProblem.task( items, 15 )
```


```txt

The robber takes the following to maximize the value
3.00 of salami
3.60 of ham
2.50 of brawn
2.40 of greaves
3.50 of welt

```



### Alternate Version

```elixir
defmodule KnapsackProblem do
  def continuous(items, max_weight) do
    Enum.sort_by(items, fn {_item, {weight, price}} -> -price / weight end)
    |> Enum.reduce_while({max_weight,0}, fn {item, {weight, price}}, {rest, value} ->
         if rest > weight do
           IO.puts "Take all #{item}"
           {:cont, {rest - weight, value + price}}
         else
           :io.format "Take ~.3fkg of ~s~n~n", [rest, item]
           :io.format "Total value of swag is ~.2f~n", [value + rest*price/weight]
           {:halt, :ok}
         end
       end)
    |> case do
         {weight, value} ->
             :io.format "Total:  weight ~.3fkg, value ~p~n", [max_weight-weight, value]
         x -> x
       end
  end
end

items = [ beef:    {3.8, 36},
          pork:    {5.4, 43},
          ham:     {3.6, 90},
          greaves: {2.4, 45},
          flitch:  {4.0, 30},
          brawn:   {2.5, 56},
          welt:    {3.7, 67},
          salami:  {3.0, 95},
          sausage: {5.9, 98} ]

KnapsackProblem.continuous( items, 15 )
```


```txt

Take all salami
Take all ham
Take all brawn
Take all greaves
Take 3.500kg of welt

Total value of swag is 349.38

```



## Erlang

Note use of lists:foldr/2, since sort is ascending.

```Erlang

-module( knapsack_problem_continuous ).

-export( [price_per_weight/1, select/2, task/0] ).

price_per_weight( Items ) -> [{Name, Weight, Price / Weight} || {Name, Weight, Price} <-Items].

select( Max_weight, Items ) ->
	{_Remains, Selected_items} = lists:foldr( fun select_until/2, {Max_weight, []}, lists:keysort(3, Items) ),
	Selected_items.

task() ->
	Items = items(),
	io:fwrite( "The robber takes the following to maximize the value~n" ),
	[io:fwrite("~.2f of ~p~n", [Weight, Name]) || {Name, Weight} <- select( 15, price_per_weight(Items) )].



items() ->
	[{"beef", 3.8, 36},
	{"pork", 5.4, 43},
	{"ham", 3.6, 90},
	{"greaves", 2.4, 45},
	{"flitch", 4.0, 30},
	{"brawn", 2.5, 56},
	{"welt", 3.7 , 67},
	{"salami", 3.0, 95},
	{"sausage",  5.9 , 98}
	].

select_until( {Name, Weight, _Price}, {Remains, Acc} ) when Remains > 0 ->
	Selected_weight = select_until_weight( Weight, Remains ),
	{Remains - Selected_weight, [{Name, Selected_weight} | Acc]};
select_until( _Item, Acc ) -> Acc.

select_until_weight( Weight, Remains ) when Weight < Remains -> Weight;
select_until_weight( _Weight, Remains ) -> Remains.

```

```txt

11> knapsack_problem_continuous:task().
The robber takes the following to maximize the value
3.50 of "welt"
2.40 of "greaves"
2.50 of "brawn"
3.60 of "ham"
3.00 of "salami"

```



=={{header|F_Sharp|F#}}==

```fsharp

//Fill a knapsack optimally - Nigel Galloway: February 1st., 2015
let items = [("beef", 3.8, 36);("pork", 5.4, 43);("ham", 3.6, 90);("greaves", 2.4, 45);("flitch" , 4.0, 30);("brawn", 2.5, 56);("welt", 3.7, 67);("salami" , 3.0, 95);("sausage", 5.9, 98)]
            |> List.sortBy(fun(_,weight,value) -> float(-value)/weight)


let knap items maxW=
  let rec take(n,g,a) =
    match g with
      | i::e -> let name, weight, value = i
                let total = n + weight
                if total <= maxW then
                  printfn "Take all %s" name
                  take(total, e, a+float(value))
                else
                  printfn "Take %0.2f kg of %s\nTotal value of swag is %0.2f" (maxW - n) name (a + (float(value)/weight)*(maxW - n))
      | []   -> printfn "Everything taken! Total value of swag is £%0.2f; Total weight of bag is %0.2fkg" a n
  take(0.0, items, 0.0)

```

```txt

> knap items 15.0;;
Take all salami
Take all ham
Take all brawn
Take all greaves
Take 3.50kg of welt
Total value of swag is £349.38

```

Should your burglar be greedy, he may bring a bigger bag.

```txt

> knap items 100.0;;
Take all salami
Take all ham
Take all brawn
Take all greaves
Take all welt
Take all sausage
Take all beef
Take all pork
Take all flitch
Everything taken! Total value of swag is £560.00; Total weight of bag is 34.30kg

```



## Forth

```forth
include lib/selcsort.4th               \ use a tiny sorting algorithm

150 value left                         \ capacity in 1/10th kilo

create items                           \ list of items
  ," beef"     38 , 3600 ,             \ description, weight, price (cents)
  ," pork"     54 , 4300 ,             \ weight in 1/10 kilo
  ," ham"      36 , 9000 ,
  ," greaves"  24 , 4500 ,
  ," flitch"   40 , 3000 ,
  ," brawn"    25 , 5600 ,
  ," welt"     37 , 6700 ,
  ," salami"   30 , 9500 ,
  ," sausage"  59 , 9800 ,
  here items - 3 / constant #items     \ total number of items

:redo items swap 3 cells * + ;         \ calculate address of record

#items array (items)                   \ array for sorting
                                       ( a -- n)
: price/weight dup 2 cells + @c swap cell+ @c / ;
: weight@ @ cell+ @c ;                 ( a -- n)
: .item @ @c count type cr ;           ( a --)
                                       \ how to sort: on price/weight
:noname >r price/weight r> price/weight > ; is precedes

: knapsack                             ( --)
  (items) dup #items dup 0 ?do i items (items) i th ! loop sort
  begin                                \ use the sorted array
     dup weight@ left <=               \ still room in the knapsack?
  while
     ." Take all of the " dup .item    \ take all of the item
     left over weight@ - to left cell+ \ adjust knapsack, increment item
  repeat left 100 * dup                \ so how much is left?
                                       \ if room, take as much as possible
  if ." Take " . ." grams of the " .item else drop drop then
;

knapsack
```



## Fortran

```fortran
program KNAPSACK_CONTINUOUS
  implicit none

  real, parameter :: maxweight = 15.0
  real :: total_weight = 0, total_value = 0, frac
  integer :: i, j

  type Item
    character(7) :: name
    real :: weight
    real :: value
  end type Item

  type(Item) :: items(9), temp

  items(1) = Item("beef",    3.8, 36.0)
  items(2) = Item("pork",    5.4, 43.0)
  items(3) = Item("ham",     3.6, 90.0)
  items(4) = Item("greaves", 2.4, 45.0)
  items(5) = Item("flitch",  4.0, 30.0)
  items(6) = Item("brawn",   2.5, 56.0)
  items(7) = Item("welt",    3.7, 67.0)
  items(8) = Item("salami",  3.0, 95.0)
  items(9) = Item("sausage", 5.9, 98.0)

  ! sort items in descending order of their value per unit weight
  do i = 2, size(items)
     j = i - 1
     temp = items(i)
     do while (j>=1 .and. items(j)%value / items(j)%weight < temp%value / temp%weight)
       items(j+1) = items(j)
       j = j - 1
     end do
    items(j+1) = temp
  end do

  i = 0
  write(*, "(a4, a13, a6)") "Item", "Weight", "Value"
  do while(i < size(items) .and. total_weight < maxweight)
    i = i + 1
    if(total_weight+items(i)%weight < maxweight) then
      total_weight = total_weight + items(i)%weight
      total_value = total_value + items(i)%value
      write(*, "(a7, 2f8.2)") items(i)
    else
      frac = (maxweight-total_weight) / items(i)%weight
      total_weight = total_weight + items(i)%weight * frac
      total_value = total_value + items(i)%value * frac
      write(*, "(a7, 2f8.2)") items(i)%name, items(i)%weight * frac, items(i)%value * frac
    end if
  end do

  write(*, "(f15.2, f8.2)") total_weight, total_value

end program KNAPSACK_CONTINUOUS
```



## Go


```go
package main

import (
    "fmt"
    "sort"
)

type item struct {
    item   string
    weight float64
    price  float64
}

type items []item

var all = items{
    {"beef", 3.8, 36},
    {"pork", 5.4, 43},
    {"ham", 3.6, 90},
    {"greaves", 2.4, 45},
    {"flitch", 4.0, 30},
    {"brawn", 2.5, 56},
    {"welt", 3.7, 67},
    {"salami", 3.0, 95},
    {"sausage", 5.9, 98},
}

// satisfy sort interface
func (z items) Len() int      { return len(z) }
func (z items) Swap(i, j int) { z[i], z[j] = z[j], z[i] }
func (z items) Less(i, j int) bool {
    return z[i].price/z[i].weight > z[j].price/z[j].weight
}

func main() {
    left := 15.
    sort.Sort(all)
    for _, i := range all {
        if i.weight <= left {
            fmt.Println("take all the", i.item)
            if i.weight == left {
                return
            }
            left -= i.weight
        } else {
            fmt.Printf("take %.1fkg %s\n", left, i.item)
            return
        }
    }
}
```

Output:

```txt

take all the salami
take all the ham
take all the brawn
take all the greaves
take 3.5kg welt

```



## Groovy

Solution: obvious greedy algorithm

```groovy
import static java.math.RoundingMode.*

def knapsackCont = { list, maxWeight = 15.0 ->
    list.sort{ it.weight / it.value }
    def remainder = maxWeight
    List sack = []
    for (item in list) {
        if (item.weight < remainder) {
            sack << [name: item.name, weight: item.weight,
                        value: (item.value as BigDecimal).setScale(2, HALF_UP)]
        } else {
            sack << [name: item.name, weight: remainder,
                        value: (item.value * remainder / item.weight).setScale(2, HALF_UP)]
            break
        }
        remainder -= item.weight
    }
    sack
}
```


Test:

```groovy
def possibleItems = [
    [name:'beef',    weight:3.8, value:36],
    [name:'pork',    weight:5.4, value:43],
    [name:'ham',     weight:3.6, value:90],
    [name:'greaves', weight:2.4, value:45],
    [name:'flitch',  weight:4.0, value:30],
    [name:'brawn',   weight:2.5, value:56],
    [name:'welt',    weight:3.7, value:67],
    [name:'salami',  weight:3.0, value:95],
    [name:'sausage', weight:5.9, value:98],
]

def contents = knapsackCont(possibleItems)
println "Total Value: ${contents*.value.sum()}"
contents.each {
    printf("    name: %-7s  weight: ${it.weight}  value: ${it.value}\n", it.name)
}
```


Output:

```txt
Total Value: 349.38
    name: salami   weight: 3.0  value: 95.00
    name: ham      weight: 3.6  value: 90.00
    name: brawn    weight: 2.5  value: 56.00
    name: greaves  weight: 2.4  value: 45.00
    name: welt     weight: 3.5  value: 63.38
```



## Haskell


We use a greedy algorithm.


```haskell
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Printf (printf)
import Control.Monad (forM_)
import Data.Ratio (numerator, denominator)

maxWgt :: Rational
maxWgt = 15

data Bounty = Bounty
  { itemName :: String
  , itemVal, itemWgt :: Rational
  }

items :: [Bounty]
items =
  [ Bounty "beef" 36 3.8
  , Bounty "pork" 43 5.4
  , Bounty "ham" 90 3.6
  , Bounty "greaves" 45 2.4
  , Bounty "flitch" 30 4.0
  , Bounty "brawn" 56 2.5
  , Bounty "welt" 67 3.7
  , Bounty "salami" 95 3.0
  , Bounty "sausage" 98 5.9
  ]

solution :: [(Rational, Bounty)]
solution = g maxWgt $ sortBy (flip $ comparing f) items
  where
    g room (b@(Bounty _ _ w):bs) =
      if w < room
        then (w, b) : g (room - w) bs
        else [(room, b)]
    f (Bounty _ v w) = v / w

main :: IO ()
main = do
  forM_ solution $ \(w, b) -> printf "%s kg of %s\n" (mixedNum w) (itemName b)
  (printf "Total value: %s\n" . mixedNum . sum) $ f <$> solution
  where
    f (w, Bounty _ v wtot) = v * (w / wtot)
    mixedNum q =
      if b == 0
        then show a
        else printf "%d %d/%d" a (numerator b) (denominator b)
      where
        a = floor q
        b = q - toEnum a
```

```txt
3 kg of salami
3 3/5 kg of ham
2 1/2 kg of brawn
2 2/5 kg of greaves
3 1/2 kg of welt
Total value: 349 14/37
```


Or similar to above (but more succinct):

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Printf (printf)

-- (name, (value, weight))
items =
  [ ("beef", (36, 3.8))
  , ("pork", (43, 5.4))
  , ("ham", (90, 3.6))
  , ("greaves", (45, 2.4))
  , ("flitch", (30, 4.0))
  , ("brawn", (56, 2.5))
  , ("welt", (67, 3.7))
  , ("salami", (95, 3.0))
  , ("sausage", (98, 5.9))
  ]

unitWeight (_, (val, weight)) = fromIntegral val / weight

solution k = loop k . sortBy (flip $ comparing unitWeight)
  where
    loop k ((name, (_, weight)):xs)
      | weight < k = putStrLn ("Take all the " ++ name) >> loop (k - weight) xs
      | otherwise = printf "Take %.2f kg of the %s\n" (k :: Float) name

main = solution 15 items
```

```txt
Take all the salami
Take all the ham
Take all the brawn
Take all the greaves
Take 3.50 kg of the welt
```


=={{header|Icon}} and {{header|Unicon}}==
This implements the greedy algorithm.  This also uses a Unicon extension to ''reverse'' which reverses a list.  In Icon, an IPL procedure is available to do the same.

```Icon
link printf

procedure main()
room := 15
every (x := !(choices := get_items())).uprice := x.price / x.weight
choices := reverse(sortf(choices,4))

every (value := 0, x := !choices) do {
   if x.weight <= room then {
      printf("Take all of the %s (%r kg) worth $%r\n",x.name,x.weight,x.price)
      value +:= x.price
      room -:= x.weight
      }
   else {
      fvalue := x.uprice * room
      printf("Take (%r kg) of the %s worth $%r\n",room,x.name,fvalue)
      value +:= fvalue
      break
   }
}
printf("Total value of a full knapsack is $%r\n",value)
end

record item(name,weight,price,uprice)

procedure get_items()
   return [  item("beef", 3.8, 36),
             item("pork", 5.4, 43),
             item("ham", 3.6, 90),
             item("greaves", 2.4, 45),
             item("flitch", 4.0, 30),
             item("brawn", 2.5, 56),
             item("welt", 3.7, 67),
             item("salami", 3.0, 95),
             item("sausage", 5.9, 98) ]
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf]

Output:
```txt
Take all of the salami (3.000000 kg) worth $95.000000
Take all of the ham (3.600000 kg) worth $90.000000
Take all of the brawn (2.500000 kg) worth $56.000000
Take all of the greaves (2.400000 kg) worth $45.000000
Take (3.500000 kg) of the welt worth $63.378378
Total value of a full knapsack is $349.378378
```




## J


We take as much as we can of the most valuable items first, and continue until we run out of space.  Only one item needs to be cut.


```J
'names numbers'=:|:;:;._2]0 :0
beef      3.8  36
pork      5.4  43
ham       3.6  90
greaves   2.4  45
flitch    4.0  30
brawn     2.5  56
welt      3.7  67
salami    3.0  95
sausage   5.9  98
)
'weights prices'=:|:".numbers
order=: \:prices%weights
take=: 15&<.&.(+/\) order{weights
result=: (*take)#(order{names),.' ',.":,.take
```


This gives the result:
 salami    3
 ham     3.6
 brawn   2.5
 greaves 2.4
 welt    3.5

For a total value of:

```J
   +/prices * (take/:order) % weights
349.378
```


See [[Knapsack_problem/Continuous/J]] for some comments on intermediate results...


## Java

Greedy solution.


```java

package hu.pj.alg.test;

import hu.pj.alg.ContinuousKnapsack;
import hu.pj.obj.Item;
import java.util.*;
import java.text.*;

public class ContinousKnapsackForRobber {
    final private double tolerance = 0.0005;

    public ContinousKnapsackForRobber() {
        ContinuousKnapsack cok = new ContinuousKnapsack(15); // 15 kg

        // making the list of items that you want to bring
        cok.add("beef",     3.8, 36); // marhahús
        cok.add("pork",     5.4, 43); // disznóhús
        cok.add("ham",      3.6, 90); // sonka
        cok.add("greaves",  2.4, 45); // tepertő
        cok.add("flitch",   4.0, 30); // oldalas
        cok.add("brawn",    2.5, 56); // disznósajt
        cok.add("welt",     3.7, 67); // hurka
        cok.add("salami",   3.0, 95); // szalámi
        cok.add("sausage",  5.9, 98); // kolbász

        // calculate the solution:
        List<Item> itemList = cok.calcSolution();

        // write out the solution in the standard output
        if (cok.isCalculated()) {
            NumberFormat nf  = NumberFormat.getInstance();

            System.out.println(
                "Maximal weight           = " +
                nf.format(cok.getMaxWeight()) + " kg"
            );
            System.out.println(
                "Total weight of solution = " +
                nf.format(cok.getSolutionWeight()) + " kg"
            );
            System.out.println(
                "Total value (profit)     = " +
                nf.format(cok.getProfit())
            );
            System.out.println();
            System.out.println(
                "You can carry the following materials " +
                "in the knapsack:"
            );
            for (Item item : itemList) {
                if (item.getInKnapsack() > tolerance) {
                    System.out.format(
                        "%1$-10s %2$-15s %3$-15s \n",
                        nf.format(item.getInKnapsack()) + " kg ",
                        item.getName(),
                        "(value = " + nf.format(item.getInKnapsack() *
                                                (item.getValue() / item.getWeight())) + ")"
                    );
                }
            }
        } else {
            System.out.println(
                "The problem is not solved. " +
                "Maybe you gave wrong data."
            );
        }

    }

    public static void main(String[] args) {
        new ContinousKnapsackForRobber();
    }

} // class
```



```java

package hu.pj.alg;

import hu.pj.obj.Item;
import java.util.*;

public class ContinuousKnapsack {

    protected List<Item> itemList   = new ArrayList<Item>();
    protected double maxWeight      = 0;
    protected double solutionWeight = 0;
    protected double profit         = 0;
    protected boolean calculated    = false;

    public ContinuousKnapsack() {}

    public ContinuousKnapsack(double _maxWeight) {
        setMaxWeight(_maxWeight);
    }

    public List<Item> calcSolution() {
        int n = itemList.size();

        setInitialStateForCalculation();
        if (n > 0  &&  maxWeight > 0) {
            Collections.sort(itemList);
            for (int i = 0; (maxWeight - solutionWeight) > 0.0  &&  i < n; i++) {
                Item item = itemList.get(i);
                if (item.getWeight() >= (maxWeight - solutionWeight)) {
                    item.setInKnapsack(maxWeight - solutionWeight);
                    solutionWeight = maxWeight;
                    profit += item.getInKnapsack() / item.getWeight() * item.getValue();
                    break;
                } else {
                    item.setInKnapsack(item.getWeight());
                    solutionWeight += item.getInKnapsack();
                    profit += item.getValue();
                }
            }
            calculated = true;
        }

        return itemList;
    }

    // add an item to the item list
    public void add(String name, double weight, double value) {
        if (name.equals(""))
            name = "" + (itemList.size() + 1);
        itemList.add(new Item(name, weight, value));
        setInitialStateForCalculation();
    }

    public double getMaxWeight() {return maxWeight;}
    public double getProfit() {return profit;}
    public double getSolutionWeight() {return solutionWeight;}
    public boolean isCalculated() {return calculated;}

    public void setMaxWeight(double _maxWeight) {
        maxWeight = Math.max(_maxWeight, 0);
    }

    // set the member with name "inKnapsack" by all items:
    private void setInKnapsackByAll(double inKnapsack) {
        for (Item item : itemList)
            item.setInKnapsack(inKnapsack);
    }

    // set the data members of class in the state of starting the calculation:
    protected void setInitialStateForCalculation() {
        setInKnapsackByAll(-0.0001);
        calculated     = false;
        profit         = 0.0;
        solutionWeight = 0.0;
    }

} // class
```



```java

package hu.pj.obj;

public class Item implements Comparable {

    protected String name       = "";
    protected double weight     = 0;
    protected double value      = 0;
    protected double inKnapsack = 0; // the weight of item in solution

    public Item() {}

    public Item(Item item) {
        setName(item.name);
        setWeight(item.weight);
        setValue(item.value);
    }

    public Item(double _weight, double _value) {
        setWeight(_weight);
        setValue(_value);
    }

    public Item(String _name, double _weight, double _value) {
        setName(_name);
        setWeight(_weight);
        setValue(_value);
    }

    public void setName(String _name) {name = _name;}
    public void setWeight(double _weight) {weight = Math.max(_weight, 0);}
    public void setValue(double _value) {value = Math.max(_value, 0);}

    public void setInKnapsack(double _inKnapsack) {
        inKnapsack = Math.max(_inKnapsack, 0);
    }

    public void checkMembers() {
        setWeight(weight);
        setValue(value);
        setInKnapsack(inKnapsack);
    }

    public String getName() {return name;}
    public double getWeight() {return weight;}
    public double getValue() {return value;}
    public double getInKnapsack() {return inKnapsack;}

    // implementing of Comparable interface:
    public int compareTo(Object item) {
        int result = 0;
        Item i2 = (Item)item;
        double rate1 = value / weight;
        double rate2 = i2.value / i2.weight;
        if (rate1 > rate2) result = -1;  // if greater, put it previously
        else if (rate1 < rate2) result = 1;
        return result;
    }

} // class
```


output:

```txt

Maximal weight           = 15 kg
Total weight of solution = 15 kg
Total value (profit)     = 349,378

You can carry the following materials in the knapsack:
3 kg       salami          (value = 95)
3,6 kg     ham             (value = 90)
2,5 kg     brawn           (value = 56)
2,4 kg     greaves         (value = 45)
3,5 kg     welt            (value = 63,378)
```



## jq

```jq
# continuous_knapsack(W) expects the input to be
# an array of objects {"name": _, "weight": _, "value": _}
# where "value" is the value of the given weight of the object.

def continuous_knapsack(W):
  map( .price = (if .weight > 0 then (.value/.weight) else 0 end) )
  | sort_by( .price )
  | reverse
  | reduce .[] as $item
      # state: [array, capacity]
      ([[], W];
       .[1] as $c
       | if $c <= 0 then .
         else ( [$item.weight, $c] | min) as $min
              | [.[0] + [ $item | (.weight = $min) | .value = (.price * $min)],
                ($c - $min) ]
         end)
  | .[1] as $remainder
  | .[0]
  | (.[] | {name, weight}),
    "Total value: \( map(.value) | add)",
    "Total weight: \(W - $remainder)" ;
```

'''The task:'''

```jq
def items: [
 { "name": "beef",    "weight":  3.8, "value": 36},
 { "name": "pork",    "weight":  5.4, "value": 43},
 { "name": "ham",     "weight":  3.6, "value": 90},
 { "name": "greaves", "weight":  2.4, "value": 45},
 { "name": "flitch",  "weight":  4.0, "value": 30},
 { "name": "brawn",   "weight":  2.5, "value": 56},
 { "name": "welt",    "weight":  3.7, "value": 67},
 { "name": "salami",  "weight":  3.0, "value": 95},
 { "name": "sausage", "weight":  5.9, "value": 98} ];

items | continuous_knapsack(15)
```

```sh
$ jq -r -c -n -f knapsack_continuous.jq
{"name":"salami","weight":3}
{"name":"ham","weight":3.6}
{"name":"brawn","weight":2.5}
{"name":"greaves","weight":2.4}
{"name":"welt","weight":3.5000000000000004}
Total value: 349.3783783783784
Total weight: 15
```



## Julia

This solution is built around the immutable type <code>KPCSupply</code>, which holds an item's data including its unit value (<code>uvalue</code>).  When the store's inventory is kept in this way, the solution to the continuous knapsack problem (provided by <code>solve</code>), is straightforward.  The thief should pack as much of the highest value items as are available until full capacity is reached, topping off with as much of the last item as the knapsack will hold.  (If the store contains less than the thief's knapsack will hold, he'll take the store's entire inventory.)

An [http://docs.julialang.org/en/release-0.3/manual/constructors/#outer-constructor-methods outer constructor method] is used to create instances of <code>KPCSupply</code> when only the <code>item</code>, <code>weight</code> and <code>value</code> are supplied.  The <code>isless</code> method is provided for <code>KPCSupply</code> objects so that items are transparently sorted by their unit value.  <code>KPCSupply</code> supports any real type for <code>weight</code>, <code>value</code> and <code>uvalue</code> (though this simple implementation does not support mixed types or promotion).  This solution uses [http://docs.julialang.org/en/release-0.3/manual/complex-and-rational-numbers/#rational-numbers Rational] numbers to avoid rounding errors until the results are printed.

'''Type and Functions''':

```julia
struct KPCSupply{T<:Real}
    item::String
    weight::T
    value::T
    uvalue::T
end
function KPCSupply(item::AbstractString, weight::Real, value::Real)
    w, v = promote(weight, value)
    KPCSupply(item, w, v, v / w)
end

Base.show(io::IO, s::KPCSupply) = print(io, s.item, @sprintf " (%.2f kg, %.2f €, %.2f €/kg)" s.weight s.value s.uvalue)
Base.isless(a::KPCSupply, b::KPCSupply) = a.uvalue < b.uvalue

function solve(store::Vector{KPCSupply{T}}, capacity::Real) where T<:Real
    sack = similar(store, 0) # vector like store, but of length 0
    kweight = zero(T)
    for s in sort(store, rev = true)
        if kweight + s.weight ≤ capacity
            kweight += s.weight
            push!(sack, s)
        else
            w = capacity - kweight
            v = w * s.uvalue
            push!(sack, KPCSupply(s.item, w, v, s.value))
            break
        end
    end
    return sack
end
```


'''Main''':

```julia
store = [KPCSupply("beef", 38//10, 36),
         KPCSupply("pork", 54//10, 43),
         KPCSupply("ham", 36//10, 90),
         KPCSupply("greaves", 24//10, 45),
         KPCSupply("flitch", 4//1, 30),
         KPCSupply("brawn", 25//10, 56),
         KPCSupply("welt", 37//10, 67),
         KPCSupply("salami", 3//1, 95),
         KPCSupply("sausage", 59//10, 98)]

sack = solve(store, 15)
println("The store contains:\n - ", join(store, "\n - "))
println("\nThe thief should take::\n - ", join(sack, "\n - "))
@printf("\nTotal value in the sack: %.2f €\n", sum(getfield.(sack, :value)))
```


```txt
The store contains:
 - beef (3.80 kg, 36.00 €, 9.47 €/kg)
 - pork (5.40 kg, 43.00 €, 7.96 €/kg)
 - ham (3.60 kg, 90.00 €, 25.00 €/kg)
 - greaves (2.40 kg, 45.00 €, 18.75 €/kg)
 - flitch (4.00 kg, 30.00 €, 7.50 €/kg)
 - brawn (2.50 kg, 56.00 €, 22.40 €/kg)
 - welt (3.70 kg, 67.00 €, 18.11 €/kg)
 - salami (3.00 kg, 95.00 €, 31.67 €/kg)
 - sausage (5.90 kg, 98.00 €, 16.61 €/kg)

The thief should take::
 - salami (3.00 kg, 95.00 €, 31.67 €/kg)
 - ham (3.60 kg, 90.00 €, 25.00 €/kg)
 - brawn (2.50 kg, 56.00 €, 22.40 €/kg)
 - greaves (2.40 kg, 45.00 €, 18.75 €/kg)
 - welt (3.50 kg, 63.38 €, 67.00 €/kg)

Total value in the sack: 349.38 €
```



## Kotlin


```scala
// version 1.1.2

data class Item(val name: String, val weight: Double, val value: Double)

val items = mutableListOf(
    Item("beef", 3.8, 36.0),
    Item("pork", 5.4, 43.0),
    Item("ham", 3.6, 90.0),
    Item("greaves", 2.4, 45.0),
    Item("flitch", 4.0, 30.0),
    Item("brawn", 2.5, 56.0),
    Item("welt", 3.7, 67.0),
    Item("salami", 3.0, 95.0),
    Item("sausage", 5.9, 98.0)
)

const val MAX_WEIGHT = 15.0

fun main(args: Array<String>) {
    // sort items by value per unit weight in descending order
    items.sortByDescending { it.value / it.weight }
    println("Item Chosen   Weight  Value  Percentage")
    println("-----------   ------ ------  ----------")
    var w = MAX_WEIGHT
    var itemCount = 0
    var sumValue = 0.0
    for (item in items) {
        itemCount++
        if (item.weight <= w) {
           sumValue += item.value
           print("${item.name.padEnd(11)}     ${"%3.1f".format(item.weight)}   ${"%5.2f".format(item.value)}")
           println("    100.00")
        }
        else {
           val value  = Math.round((w / item.weight * item.value * 100.0)) / 100.0
           val percentage = Math.round((w / item.weight * 10000.0)) / 100.0
           sumValue += value
           print("${item.name.padEnd(11)}     ${"%3.1f".format(w)}   ${"%5.2f".format(value)}")
           println("     $percentage")
           break
        }
        w -= item.weight
        if (w == 0.0) break
    }
    println("-----------   ------ ------")
    println("${itemCount} items        15.0  ${"%6.2f".format(sumValue)}")
}
```


```txt

Item Chosen   Weight  Value  Percentage
-----------   ------ ------  ----------
salami          3.0   95.00    100.00
ham             3.6   90.00    100.00
brawn           2.5   56.00    100.00
greaves         2.4   45.00    100.00
welt            3.5   63.38     94.59
-----------   ------ ------
5 items        15.0  349.38

```


Using QuickSort (a generic form, non recursive)

## M2000 Interpreter


```M2000 Interpreter

Module Knapsack {
      Form 60, 40
      Cls 5, 0
      Pen 14
      Class Quick {
      Private:
            partition=lambda-> {
                  Read &A(), p, r : i = p-1 : x=A(r)
                  For j=p to r-1 {If .LE(A(j), x) Then i++:Swap A(i),A(j)
                  } : Swap A(i+1), A(r) :  Push  i+2, i
            }
      Public:
            LE=Lambda->Number<=Number
            \\ module for strings erased here
            Function quicksort {
                 Read ref$
                 {
                         loop : If Stackitem() >= Stackitem(2) Then Drop 2 : if  empty then {Break} else continue
                         over 2,2 : call .partition(ref$) :shift 3
                 }
            }
      }
      Class Item {
            name$, weight, aValue  ' can't use Value has other meaning
            class:
            Module Item (.name$, .weight, .aValue) {}
      }
      Def Double max_weight=15, total_weight, total_value, frac
      Def long I
      Dim Items(1 to 9)
      Flush  ' empty stack
      \\ now fill stack
      Data  "beef", 3.8, 36,"pork", 5.4, 43,"ham", 3.6, 90, "greaves", 2.4, 45, "flitch", 4, 30
      Data "brawn", 2.5, 56, "welt", 3.7, 67, "salami", 3, 95, "sausage", 5.9, 98
      For i=1 to 9 : Items(i)=Item(Letter$, Number, Number): Next i
      \\ Setup QuickSort
      Quick=Quick()
      Quick.LE=lambda (b, a)-> {
            =a.avalue/a.weight<=b.avalue/b.weight
      }
      Call Quick.QuickSort(&items(), 1, 9)
      \\ So now we have a sorted array of objects
      i=0
      \\ Setup console to print
      Dim Back(-1 to 0)
      Back(-1)=#666666, #444444
      Alter=True
      \\ $("0.00", 20) Set number rounding for print, and 14 chars column width
      \\ $(2) set center justify for non proportional print
      \\ $(0) set default - strings justify left, numbers right
      Print  $("0.00", 20),$(2),"", "Knapsack"
      Pen 0 {
            Print @(pos, row,width,row+1, 7),"Item", "Weight (Kg)", "Price (value)", $(0)
      }
      While i<Len(Items()) and total_weight<max_weight {
            i++
            if total_weight+items(i).weight<max_weight Then {
                  total_weight+=items(i).weight
                  total_value+=items(i).avalue
                  WriteItem(i, 1)
            } Else {
                  frac=(max_weight-total_weight)/items(i).weight
                  total_weight+=items(i).weight*frac
                  total_value+=items(i).avalue*frac
                  WriteItem(i, frac )
            }
      }
      Print
      Pen 0 {
            Print @(pos+1, row,width,row+1, 7, 7), "Total Weight",total_weight
            Print @(pos+1, row,width,row+1, 7, 7), "Total Value", total_value
      }
      End
      Sub WriteItem(i, frac)
            For Items(i) {
                  Print @(pos+1, row,width,row+1, back(alter), 14),  .name$, .weight*frac, .avalue*frac
                  Alter~
            }
      End Sub
}
Knapsack

```

Output the same as other examples, with some color.


## Mathematica


The problem is solved by sorting the original table by price to weight ratio, finding the accumlated weight, and the index of the item which exedes the carrying capacity (overN)
The output is then all items prior to this one, along with that item corrected for it's excess weighter (overW)


```Mathematica
Knapsack[shop_, capacity_] :=  Block[{sortedTable, overN, overW, output},
 sortedTable = SortBy[{#1, #2, #3, #3/#2} & @@@ shop, -#[[4]] &];
 overN = Position[Accumulate[sortedTable[[1 ;;, 2]]], a_ /; a > capacity, 1,1][[1, 1]];
 overW = Accumulate[sortedTable[[1 ;;, 2]]][[overN]] - capacity;

 output = Reverse@sortedTable[[Ordering[sortedTable[[1 ;;, 4]], -overN]]];
 output[[-1, 2]] = output[[-1, 2]] - overW;
 output[[-1, 3]] = output[[-1, 2]] output[[-1, 4]];
 Append[output[[1 ;;, 1 ;; 3]], {"Total",Sequence @@ Total[output[[1 ;;, 2 ;; 3]]]}]]
```


A test using the specified data:

```Mathematica
weightPriceTable =
{{"beef", 3.8, 36}, {"pork", 5.4, 43}, {"ham", 3.6, 90}, {"greaves", 2.4, 45}, {"flitch", 4., 30},
{"brawn", 2.5, 56}, {"welt", 3.7, 67}, {"salami", 3., 95}, {"sausage",  5.9, 98}};
carryCapacity = 15;
Knapsack[weightPriceTable, carryCapacity] // Grid

salami	3.	95
ham	3.6	90
brawn	2.5	56
greaves	2.4	45
welt	3.5	63.3784
Total	15.	349.378

```



## Mathprog

<lang>/*Knapsack

  This model finds the optimal packing of a knapsack

  Nigel_Galloway
  January 10th., 2012
*/
set Items;
param weight{t in Items};
param value{t in Items};

var take{t in Items}, >=0, <=weight[t];

knap_weight : sum{t in Items} take[t] <= 15;

maximize knap_value: sum{t in Items} take[t] * (value[t]/weight[t]);

data;

param : Items          : weight   value :=
        beef 	          3.8     36
        pork 	          5.4	  43
        ham 		  3.6	  90
        greaves           2.4     45
        flitch 	          4.0     30
        brawn 	          2.5     56
        welt 	          3.7     67
        salami 	          3.0     95
        sausage           5.9     98
        ;
end;
```


The solution is here at [[Knapsack problem/Continuous/Mathprog]].


## OCaml


```ocaml
let items =
  [ "beef",     3.8,  36;
    "pork",     5.4,  43;
    "ham",      3.6,  90;
    "greaves",  2.4,  45;
    "flitch",   4.0,  30;
    "brawn",    2.5,  56;
    "welt",     3.7,  67;
    "salami",   3.0,  95;
    "sausage",  5.9,  98; ]

let () =
  let items = List.map (fun (name, w, p) -> (name, w, p, float p /. w)) items in
  let items = List.sort (fun (_,_,_,v1) (_,_,_,v2) -> compare v2 v1) items in
  let rec loop acc weight = function
  | ((_,w,_,_) as item) :: tl ->
      if w +. weight > 15.0
      then (weight, acc, item)
      else loop (item::acc) (w +. weight) tl
  | [] -> assert false
  in
  let weight, res, (last,w,p,v) = loop [] 0.0 items in
  print_endline "    Items  Weight Price";
  let price =
    List.fold_left (fun price (name,w,p,_) ->
      Printf.printf " %7s: %6.2f %3d\n" name w p;
      (p + price)
    ) 0 res
  in
  let rem_weight = 15.0 -. weight in
  let last_price = v *. rem_weight in
  Printf.printf " %7s: %6.2f %6.2f\n" last rem_weight last_price;
  Printf.printf " Total Price: %.3f\n" (float price +. last_price);
;;
```


    Items  Weight Price
 greaves:   2.40  45
   brawn:   2.50  56
     ham:   3.60  90
  salami:   3.00  95
    welt:   3.50  63.38
 Total Price: 349.378


## Oforth



```oforth
[
   [ "beef",    3.8, 36 ], [ "pork",    5.4, 43 ], [ "ham",     3.6, 90 ],
   [ "greaves", 2.4, 45 ], [ "flitch",  4.0, 30 ], [ "brawn",  	2.5, 56 ],
   [ "welt",  	3.7, 67 ], [ "salami",  3.0, 95 ], [ "sausage", 5.9, 98 ]
] const: Items

: rob
| item value |
  0.0 ->value
  15.0 #[ dup second swap third / ] Items sortBy forEach: item [
     dup 0.0 == ifTrue: [ return ]
     dup item second >= ifTrue: [
        "Taking" . item first . " :" . item second dup .cr -
        item third value + ->value continue
        ]
     "And part of" . item first . " :" . dup .cr
     item third * item second / value + "Total value :" . .cr break
     ] ;
```


```txt

>rob
Taking salami  : 3
Taking ham  : 3.6
Taking brawn  : 2.5
Taking greaves  : 2.4
And part of welt  : 3.5
Total value : 349.378378378378
ok

```



## ooRexx


### version 1


```oorexx
/*--------------------------------------------------------------------
* 20.09.2014 Walter Pachl translated from REXX version 2
*            utilizing ooRexx features like objects, array(s) and sort
*-------------------------------------------------------------------*/
  maxweight = 15.0
  items=.array~new
  items~append(.item~new('beef',    3.8, 36.0))
  items~append(.item~new('pork',    5.4, 43.0))
  items~append(.item~new('ham',     3.6, 90.0))
  items~append(.item~new('greaves', 2.4, 45.0))
  items~append(.item~new('flitch',  4.0, 30.0))
  items~append(.item~new('brawn',   2.5, 56.0))
  items~append(.item~new('welt',    3.7, 67.0))
  items~append(.item~new('salami',  3.0, 95.0))
  items~append(.item~new('sausage', 5.9, 98.0))

  /* show the input */
  Say '#    vpu name    weight   value'
  i=0
  Do x over items
    i+=1
    Say i format(x~vpu,2,3) left(x~name,7) format(x~weight,2,3) format(x~value,3,3)
    End

  /* sort the items by descending value per unit of weight */
  items~sortWith(.DescendingComparator~new)

  total_weight=0
  total_value =0
  Say ' '
  Say 'Item     Weight   Value'
  i=0
  Do x over items
    i+=1
    Parse Var item.i name '*' weight '*' value
    if total_weight+x~weight<maxweight then Do
      total_weight = total_weight + x~weight
      total_value  = total_value + x~value
      Say left(x~name,7) format(x~weight,3,3) format(x~value,3,3)
      End
    Else Do
      weight=maxweight-total_weight
      value=weight*x~vpu
      total_value = total_value + value
      total_weight = maxweight
      Say left(x~name,7) format(weight,3,3) format(value,3,3)
      Leave
      End
    End
  Say copies('-',23)
  Say 'total ' format(total_weight,4,3) format(total_value,3,3)
  Exit

::class item
  ::attribute vpu
  ::attribute name
  ::attribute weight
  ::attribute value

::method init
  Expose vpu
  Use Arg name, weight, value
  self~name=name
  self~weight=weight
  self~value=value
  self~vpu=value/weight

::CLASS 'DescendingComparator' MIXINCLASS Comparator
::METHOD compare
use strict arg a, b
Select
  When (a~vpu)<(b~vpu) Then res='1'
  Otherwise res='-1'
  End
Return res
```

```txt

#    vpu name    weight   value
1  9.474 beef     3.800  36.000
2  7.963 pork     5.400  43.000
3 25.000 ham      3.600  90.000
4 18.750 greaves  2.400  45.000
5  7.500 flitch   4.000  30.000
6 22.400 brawn    2.500  56.000
7 18.108 welt     3.700  67.000
8 31.667 salami   3.000  95.000
9 16.610 sausage  5.900  98.000

Item     Weight   Value
salami    3.000  95.000
ham       3.600  90.000
brawn     2.500  56.000
greaves   2.400  45.000
welt      3.500  63.378
-----------------------
total    15.000 349.378
```


### version 2


```oorexx
/*--------------------------------------------------------------------
* 20.09.2014 Walter Pachl translated from REXX version 2
*            utilizing ooRexx features like objects, array(s) and sort
* 21.09.2014 simplified (courtesy Rony Flatscher)
*            (sort uses now the method "compareTo" defined for item)
*-------------------------------------------------------------------*/
  maxweight = 15.0
  items=.array~new
  items~append(.item~new('beef',    3.8, 36.0))
  items~append(.item~new('pork',    5.4, 43.0))
  items~append(.item~new('ham',     3.6, 90.0))
  items~append(.item~new('greaves', 2.4, 45.0))
  items~append(.item~new('flitch',  4.0, 30.0))
  items~append(.item~new('brawn',   2.5, 56.0))
  items~append(.item~new('welt',    3.7, 67.0))
  items~append(.item~new('salami',  3.0, 95.0))
  items~append(.item~new('sausage', 5.9, 98.0))

  /* show the input */
  Say '#    vpu name    weight   value'
  i=0
  Do x over items
    i+=1
    Say i format(x~vpu,2,3) left(x~name,7) format(x~weight,2,3) format(x~value,3,3)
    End

  /* sort the items by descending value per unit of weight */
  items~sort          /* using the method compareTo used for item */

  total_weight=0
  total_value =0
  Say ' '
  Say 'Item     Weight   Value'
  i=0
  Do x over items
    i+=1
    Parse Var item.i name '*' weight '*' value
    if total_weight+x~weight<maxweight then Do
      total_weight = total_weight + x~weight
      total_value  = total_value + x~value
      Say left(x~name,7) format(x~weight,3,3) format(x~value,3,3)
      End
    Else Do
      weight=maxweight-total_weight
      value=weight*x~vpu
      total_value = total_value + value
      total_weight = maxweight
      Say left(x~name,7) format(weight,3,3) format(value,3,3)
      Leave
      End
    End
  Say copies('-',23)
  Say 'total ' format(total_weight,4,3) format(total_value,3,3)
  Exit

::class item
  ::attribute vpu
  ::attribute name
  ::attribute weight
  ::attribute value

::method init
  Expose vpu
  Use Arg name, weight, value
  self~name=name
  self~weight=weight
  self~value=value
  self~vpu=value/weight

::method compareTo   -- default sort order
  Expose vpu
  use Arg other
  return -sign(vpu - other~vpu)
```

Output is the same as for version 1.


## Perl


```perl
my @items = sort { $b->[2]/$b->[1] <=> $a->[2]/$a->[1] }
(
        [qw'beef    3.8 36'],
        [qw'pork    5.4 43'],
        [qw'ham     3.6 90'],
        [qw'greaves 2.4 45'],
        [qw'flitch  4.0 30'],
        [qw'brawn   2.5 56'],
        [qw'welt    3.7 67'],
        [qw'salami  3.0 95'],
        [qw'sausage 5.9 98'],
);

my ($limit, $value) = (15, 0);

print "item   fraction weight value\n";
for (@items) {
        my $ratio = $_->[1] > $limit ? $limit/$_->[1] : 1;
        print "$_->[0]\t";
        $value += $_->[2] * $ratio;
        $limit -= $_->[1];
        if ($ratio == 1) {
                print "  all\t$_->[1]\t$_->[2]\n";
        } else {
                printf "%5.3f   %s   %8.3f\n", $ratio, $_->[1] * $ratio, $_->[2] * $ratio;
                last;
        }
}

print "-" x 40, "\ntotal value: $value\n";

```

Output:
```txt
item   fraction weight value
salami    all   3.0     95
ham       all   3.6     90
brawn     all   2.5     56
greaves   all   2.4     45
welt    0.946   3.5     63.378
----------------------------------------
total value: 349.378378378378

```



## Perl 6

This Solutions sorts the item by WEIGHT/VALUE

```perl6
class KnapsackItem {
  has $.name;
  has $.weight is rw;
  has $.price is rw;
  has $.ppw;

  method new (Str $n, Rat $w, Int $p) {
    self.bless(:name($n), :weight($w), :price($p), :ppw($w/$p))
  }

  method cut-maybe ($max-weight) {
    return False if $max-weight > $.weight;
    $.price = $max-weight / $.ppw;
    $.weight = $.ppw * $.price;
    return True;
  }

  method gist () { sprintf "%8s %1.2f   %3.2f",
                            $.name,
                                $.weight,
                                        $.price }
}

my $max-w = 15;
say    "Item    Portion Value";

.say for gather
    for < beef    3.8 36
          pork    5.4 43
          ham     3.6 90
          greaves 2.4 45
          flitch  4.0 30
          brawn   2.5 56
          welt    3.7 67
          salami  3.0 95
          sausage 5.9 98 >
        ==> map({ KnapsackItem.new($^a, $^b, $^c) })
        ==> sort *.ppw
    {
        my $last-one = .cut-maybe($max-w);
        take $_;
        $max-w -= .weight;
        last if $last-one;
    }
```

'''Output:'''

```txt

%perl6 knapsack_continous.p6
Item    Portion Value
  salami 3.00   95.00
     ham 3.60   90.00
   brawn 2.50   56.00
 greaves 2.40   45.00
    welt 3.50   63.38

```



## Phix


```Phix
constant meats = {
--Item  Weight (kg)     Price (Value)
{"beef",    3.8,    36},
{"pork",    5.4,    43},
{"ham",     3.6,    90},
{"greaves", 2.4,    45},
{"flitch",  4.0,    30},
{"brawn",   2.5,    56},
{"welt",    3.7,    67},
{"salami",  3.0,    95},
{"sausage", 5.9,    98}}

function by_weighted_value(integer i, j)
    atom {?,weighti,pricei} = meats[i],
         {?,weightj,pricej} = meats[j]
    return compare(pricej/weightj,pricei/weighti)
end function

sequence tags = custom_sort(routine_id("by_weighted_value"),tagset(length(meats)))

atom w = 15, worth = 0
for i=1 to length(tags) do
    object {desc,wi,price} = meats[tags[i]]
    atom c = min(wi,w)
    printf(1,"%3.1fkg%s of %s\n",{c,iff(c=wi?" (all)":""),desc})
    worth += (c/wi)*price
    w -= c
    if w=0 then exit end if
end for
printf(1,"Total value: %f\n",{worth})
```

```txt

3.0kg (all) of salami
3.6kg (all) of ham
2.5kg (all) of brawn
2.4kg (all) of greaves
3.5kg of welt
Total value: 349.378378

```



## PicoLisp


```PicoLisp
(scl 2)

(de *Items
   ("beef" 3.8 36.0)
   ("pork" 5.4 43.0)
   ("ham" 3.6 90.0)
   ("greaves" 2.4 45.0)
   ("flitch" 4.0 30.0)
   ("brawn" 2.5 56.0)
   ("welt" 3.7 67.0)
   ("salami" 3.0 95.0)
   ("sausage" 5.9 98.0) )

(let K
   (make
      (let Weight 0
         (for I (by '((L) (*/ (caddr L) -1.0 (cadr L))) sort *Items)
            (T (= Weight 15.0))
            (inc 'Weight (cadr I))
            (T (> Weight 15.0)
               (let W (- (cadr I) Weight -15.0)
                  (link (list (car I) W (*/ W (caddr I) (cadr I)))) ) )
            (link I) ) ) )
   (for I K
      (tab (3 -9 8 8)
         NIL
         (car I)
         (format (cadr I) *Scl)
         (format (caddr I) *Scl) ) )
   (tab (12 8 8)
      NIL
      (format (sum cadr K) *Scl)
      (format (sum caddr K) *Scl) ) )
```

Output:

```txt
   salami       3.00   95.00
   ham          3.60   90.00
   brawn        2.50   56.00
   greaves      2.40   45.00
   welt         3.50   63.38
               15.00  349.38
```



## PL/I


```pli
*process source xref attributes;
 KNAPSACK_CONTINUOUS: Proc Options(main);
 /*--------------------------------------------------------------------
 * 19.09.2014 Walter Pachl translated from FORTRAN
 *-------------------------------------------------------------------*/
  Dcl (divide,float,hbound,repeat) Builtin;
  Dcl SYSPRINT Print;

  Dcl maxweight Dec Fixed(15,3);
  maxweight = 15.0;
  Dcl (total_weight,total_value) Dec Fixed(15,3) Init(0);
  Dcl vpu  Dec Float(15);
  Dcl (i,j) Bin Fixed(31);

  Dcl 1 item(9),
       2 name   Char(7),
       2 weight Dec Fixed(15,3),
       2 value  Dec Fixed(15,3);
  Dcl temp Like item;

  Call init_item(1,'beef',    3.8, 36.0);
  Call init_item(2,'pork',    5.4, 43.0);
  Call init_item(3,'ham',     3.6, 90.0);
  Call init_item(4,'greaves', 2.4, 45.0);
  Call init_item(5,'flitch',  4.0, 30.0);
  Call init_item(6,'brawn',   2.5, 56.0);
  Call init_item(7,'welt',    3.7, 67.0);
  Call init_item(8,'salami',  3.0, 95.0);
  Call init_item(9,'sausage', 5.9, 98.0);

  /* sort item in descending order of their value per unit weight */
  do i = 2 To hbound(item);
    j = i - 1;
    temp = item(i);
    do while(j>=1&item(j).value/item(j).weight<temp.value/temp.weight);
      item(j+1) = item(j);
      j = j - 1;
      end;
    item(j+1) = temp;
  end;
  Do i=1 To hbound(item);
    Put Edit(i,item(i))(Skip,f(2),x(2),a(7),2(f(8,3)));
    End;
  i = 0;
  Put Skip;
  Put Edit('Item     Weight   Value')(Skip,a);
  do i=1 By 1 while(i < hbound(item) & total_weight < maxweight);
    if total_weight+item(i).weight < maxweight then Do;
      total_weight = total_weight + item(i).weight;
      total_value  = total_value + item(i).value;
      Put Edit(item(i))(Skip,a(7),2(f(8,3)));
      End;
    Else Do;
      vpu=divide(item(i).value,item(i).weight,15,8);
      item(i).weight=maxweight-total_weight;
      item(i).value=float(item(i).weight)*vpu;
      total_value = total_value + item(i).value;
      total_weight = total_weight + item(i).weight;
      Put Edit(item(i).name, item(i).weight, item(i).value)
              (Skip,a(7),2(f(8,3)));
      Leave Loop;
      end;
    end;
  Put Edit(repeat('-',22))(Skip,a);
  Put Edit('total',total_weight, total_value)(Skip,a(6),f(9,3),f(8,3));

 init_item: Proc(i,name,weight,value);
 Dcl i Bin Fixed(31);
 Dcl name Char(*);
 Dcl (weight,value) Dec Fixed(15,3);
 item(i).name   = name;
 item(i).weight = weight;
 item(i).value  = value;
 End;
 End;
```

```txt

 1  salami    3.000  95.000
 2  ham       3.600  90.000
 3  brawn     2.500  56.000
 4  greaves   2.400  45.000
 5  welt      3.700  67.000
 6  sausage   5.900  98.000
 7  beef      3.800  36.000
 8  pork      5.400  43.000
 9  flitch    4.000  30.000

Item     Weight   Value
salami    3.000  95.000
ham       3.600  90.000
brawn     2.500  56.000
greaves   2.400  45.000
welt      3.500  63.378
-----------------------
total    15.000 349.378
```



## Prolog

Works with SWI-Prolog and <b>library(simplex)</b> written by <b>Markus Triska</b>

```Prolog
:- use_module(library(simplex)).
% tuples (name, weights, value).
knapsack :-
	L = [(   beef, 	  3.8, 	36),
	     (   pork, 	  5.4, 	43),
	     (   ham, 	  3.6, 	90),
	     (   greaves, 2.4, 	45),
	     (   flitch,  4.0, 	30),
	     (   brawn,   2.5, 	56),
	     (   welt, 	  3.7, 	67),
	     (   salami,  3.0, 	95),
	     (   sausage, 5.9, 	98)],

	 gen_state(S0),
	 length(L, N),
	 numlist(1, N, LN),
	 (   (  create_constraint_N(LN, L, S0, S1, [], LW, [], LV),
		constraint(LW =< 15.0, S1, S2),
		maximize(LV, S2, S3)
	      )),
	compute_lenword(L, 0, Len),
	sformat(A1, '~~w~~t~~~w|', [Len]),
	sformat(A2, '~~t~~2f~~~w|', [10]),
	sformat(A3, '~~t~~2f~~~w|', [10]),
	print_results(S3, A1,A2,A3, L, LN, 0, 0).


create_constraint_N([], [], S, S, LW, LW, LV, LV).

create_constraint_N([HN|TN], [(_, W, V) | TL], S1, SF, LW, LWF, LV, LVF) :-
	constraint([x(HN)] >= 0, S1, S2),
	constraint([x(HN)] =< W, S2, S3),
	X is V/W,
	create_constraint_N(TN, TL, S3, SF, [x(HN) | LW], LWF, [X * x(HN) | LV], LVF).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
compute_lenword([], N, N).
compute_lenword([(Name, _, _)|T], N, NF):-
	atom_length(Name, L),
	(   L > N -> N1 = L; N1 = N),
	compute_lenword(T, N1, NF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
print_results(_S, A1, A2, A3, [], [], WM, VM) :-
	sformat(W1, A1, [' ']),
	sformat(W2, A2, [WM]),
	sformat(W3, A3, [VM]),
	format('~w~w~w~n', [W1,W2,W3]).


print_results(S, A1, A2, A3, [(Name, W, V)|T], [N|TN], W1, V1) :-
	variable_value(S, x(N), X),
	(   X = 0 -> W1 = W2, V1 = V2
	;
	    sformat(S1, A1, [Name]),
	    sformat(S2, A2, [X]),
	    Vtemp is X * V/W,
	    sformat(S3, A3, [Vtemp]),
	    format('~w~w~w~n', [S1,S2,S3]),
	    W2 is W1 + X,
	    V2 is V1 + Vtemp ),
	print_results(S, A1, A2, A3, T, TN, W2, V2).


```

Output :

```txt
 ?- knapsack.
ham          3.60     90.00
greaves      2.40     45.00
brawn        2.50     56.00
welt         3.50     63.38
salami       3.00     95.00
            15.00    349.38
true .
```



## PureBasic

Using the greedy algorithm.

```PureBasic
Structure item
  name.s
  weight.f   ;units are kilograms (kg)
  Value.f
  vDensity.f ;the density of the value, i.e. value/weight, and yes I made up the term ;)
EndStructure

#maxWeight = 15
Global itemCount = 0 ;this will be increased as needed to match actual count
Global Dim items.item(itemCount)

Procedure addItem(name.s, weight.f, Value.f)
  If itemCount >= ArraySize(items())
    Redim items.item(itemCount + 10)
  EndIf
  With items(itemCount)
    \name = name
    \weight = weight
    \Value = Value
    If Not \weight
      \vDensity = \Value
    Else
      \vDensity = \Value / \weight
    EndIf
  EndWith
  itemCount + 1
EndProcedure

;build item list
addItem("beef", 3.8, 36)
addItem("pork", 5.4, 43)
addItem("ham", 3.6, 90)
addItem("greaves", 2.4, 45)
addItem("flitch", 4.0, 30)
addItem("brawn", 2.5, 56)
addItem("welt", 3.7, 67)
addItem("salami", 3.0, 95)
addItem("sausage", 5.9, 98)
SortStructuredArray(items(), #PB_Sort_descending, OffsetOf(item\vDensity), #PB_Sort_Float, 0, itemCount - 1)

Define TotalWeight.f, TotalValue.f, i
NewList knapsack.item()
For i = 0 To itemCount
  If TotalWeight + items(i)\weight < #maxWeight
    AddElement(knapsack())
    knapsack() = items(i)
    TotalWeight + items(i)\weight
    TotalValue + items(i)\Value
  Else
    AddElement(knapsack())
    knapsack() = items(i)
    knapsack()\weight = #maxWeight - TotalWeight
    knapsack()\Value = knapsack()\weight * knapsack()\vDensity
    TotalWeight = #maxWeight
    TotalValue + knapsack()\Value
    Break
  EndIf
Next

If OpenConsole()
  PrintN(LSet("Maximal weight", 26, " ") + "= " + Str(#maxWeight) + " kg")
  PrintN(LSet("Total weight of solution", 26, " ") + "= " + Str(#maxWeight) + " kg")
  PrintN(LSet("Total value", 26, " ") + "= " + StrF(TotalValue, 3) + " " + #CRLF$)
  PrintN("You can carry the following materials in the knapsack: ")
  ForEach knapsack()
    PrintN(RSet(StrF(knapsack()\weight, 1), 5, " ") + " kg  " + LSet(knapsack()\name, 10, " ") + "  (Value = " + StrF(knapsack()\Value, 3) + ")")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Maximal weight            = 15 kg
Total weight of solution  = 15 kg
Total value               = 349.378

You can carry the following materials in the knapsack:
  3.0 kg  salami      (Value = 95.000)
  3.6 kg  ham         (Value = 90.000)
  2.5 kg  brawn       (Value = 56.000)
  2.4 kg  greaves     (Value = 45.000)
  3.5 kg  welt        (Value = 63.378)
```



## Python

I think this greedy algorithm of taking the largest amounts of items ordered by their value per unit weight is maximal:

```python
#        NAME, WEIGHT, VALUE (for this weight)
items = [("beef",    3.8, 36.0),
         ("pork",    5.4, 43.0),
         ("ham",     3.6, 90.0),
         ("greaves", 2.4, 45.0),
         ("flitch",  4.0, 30.0),
         ("brawn",   2.5, 56.0),
         ("welt",    3.7, 67.0),
         ("salami",  3.0, 95.0),
         ("sausage", 5.9, 98.0)]

MAXWT = 15.0

sorted_items = sorted(((value/amount, amount, name)
                       for name, amount, value in items),
                      reverse = True)
wt = val = 0
bagged = []
for unit_value, amount, name in sorted_items:
    portion = min(MAXWT - wt, amount)
    wt     += portion
    addval  = portion * unit_value
    val    += addval
    bagged += [(name, portion, addval)]
    if wt >= MAXWT:
        break

print("    ITEM   PORTION VALUE")
print("\n".join("%10s %6.2f %6.2f" % item for item in bagged))
print("\nTOTAL WEIGHT: %5.2f\nTOTAL VALUE: %5.2f" % (wt, val))
```


'''Sample Output'''

```txt
    ITEM   PORTION VALUE
    salami   3.00  95.00
       ham   3.60  90.00
     brawn   2.50  56.00
   greaves   2.40  45.00
      welt   3.50  63.38

TOTAL WEIGHT: 15.00
TOTAL VALUE: 349.38
```



## R

Translated into r-script by Shana White (vandersm@mail.uc.edu) from pseudocode found in 'Algorithms: Sequential Parallel and Distributed', by Kenneth A. Berman and Jerome L. Paul

```r

knapsack<- function(Value, Weight, Objects, Capacity){
  Fraction = rep(0, length(Value))
  Cost = Value/Weight
  #print(Cost)
  W = Weight[order(Cost, decreasing = TRUE)]
  Obs = Objects[order(Cost, decreasing = TRUE)]
  Val = Value[order(Cost, decreasing = TRUE)]
  #print(W)
  RemainCap = Capacity
  i = 1
  n = length(Cost)
  if (W[1] <= Capacity){
    Fits <- TRUE
  }
  else{
    Fits <- FALSE
  }
  while (Fits && i <= n ){
    Fraction[i] <- 1
    RemainCap <- RemainCap - W[i]
    i <- i+1
    #print(RemainCap)
    if (W[i] <= RemainCap){
      Fits <- TRUE
    }
    else{
      Fits <- FALSE
    }
  }
  #print(RemainCap)
  if (i <= n){
    Fraction[i] <- RemainCap/W[i]
  }
  names(Fraction) = Obs
  Quantity_to_take = W*Fraction
  Total_Value = sum(Val*Fraction)
  print("Fraction of available quantity to take:")
  print(round(Fraction, 3))
  print("KG of each to take:")
  print(Quantity_to_take)
  print("Total value of tasty meats:")
  print(Total_Value)
}
```


'''Sample Input'''

```txt
o = c("beef", "pork", "ham", "greaves", "flitch", "brawn", "welt", "salami", "sausage")
w = c(3.8,5.4,3.6,2.4,4.0,2.5,3.7,3.0,5.9)
v = c(36,43,90,45,30,56,67,95,98)
knapsack(v, w, o, 15)
```


'''Sample Output'''

```txt

[1] "Fraction of available quantity to take:"
 salami     ham   brawn greaves    welt sausage    beef    pork  flitch
  1.000   1.000   1.000   1.000   0.946   0.000   0.000   0.000   0.000
[1] "KG of each to take:"
 salami     ham   brawn greaves    welt sausage    beef    pork  flitch
    3.0     3.6     2.5     2.4     3.5     0.0     0.0     0.0     0.0
[1] "Total value of tasty meats:"
[1] 349.3784
```



## Racket



```racket
#lang racket
(define shop-inventory
  '((beef 	3.8 	36)
    (pork 	5.4 	43)
    (ham 	3.6 	90)
    (greaves 	2.4 	45)
    (flitch 	4.0 	30)
    (brawn 	2.5 	56)
    (welt 	3.7 	67)
    (salami 	3.0 	95)
    (sausage 	5.9 	98)))


(define (continuous-knapsack shop sack sack-capacity sack-total-value)
  ;; solved by loading up on the highest value item...
  (define (value/kg item) (/ (third item) (second item)))
  (if (zero? sack-capacity)
      (values (reverse sack) sack-total-value)
      (let* ((best-value-item (argmax value/kg shop))
             (bvi-full-weight (second best-value-item))
             (amount-can-take (min sack-capacity bvi-full-weight))
             (bvi-full-value  (third best-value-item))
             (bvi-taken-value (* bvi-full-value (/ amount-can-take bvi-full-weight))))
        (continuous-knapsack (remove best-value-item shop)
                             (cons (list (first best-value-item)
                                         (if (= amount-can-take bvi-full-weight)
                                             'all-of amount-can-take) bvi-taken-value)
                                   sack)
                             (- sack-capacity amount-can-take)
                             (+ sack-total-value bvi-taken-value)))))

(define (report-knapsack sack total-value)
  (for-each (lambda (item)
              (if (eq? 'all-of (second item))
                  (printf "Take all of the ~a (for ~a)~%"
                          (first item) (third item))
                  (printf "Take ~a of the ~a (for ~a)~%"
                          (real->decimal-string (second item))
                          (first item)
                          (real->decimal-string (third item)))))
            sack)
  (printf "For a grand total of: ~a" (real->decimal-string total-value)))

(call-with-values (lambda () (continuous-knapsack shop-inventory null 15 0))
                  report-knapsack)
```


```txt
Take all of the salami (for 95.0)
Take all of the ham (for 90.0)
Take all of the brawn (for 56.0)
Take all of the greaves (for 45.0)
Take 3.50 of the welt (for 63.38)
For a grand total of: 349.38
```



## REXX


### version 1

Originally used the Fortran program as a prototype.

Some amount of code was added to format the output better.

```rexx
/*REXX pgm solves the continuous burglar's knapsack problem; items with weight and value*/
@.=               /*═══════  name    weight  value  ══════*/
                     @.1 = 'flitch     4       30  '
                     @.2 = 'beef       3.8     36  '
                     @.3 = 'pork       5.4     43  '
                     @.4 = 'greaves    2.4     45  '
                     @.5 = 'brawn      2.5     56  '
                     @.6 = 'welt       3.7     67  '
                     @.7 = 'ham        3.6     90  '
                     @.8 = 'salami     3       95  '
                     @.9 = 'sausage    5.9     98  '
parse arg maxW d .                               /*get possible arguments from the C.L. */
if maxW=='' | maxW==","  then maxW=15            /*the burglar's knapsack maximum weight*/
if    d=='' |    d==","  then    d= 3            /*# decimal digits shown with  FORMAT. */
wL=d+length('weight');   nL=d+length("total weight");    vL=d+length('value')  /*lengths*/
totW=0;  totV=0                                  /* [↓]  assign item to separate lists. */
                 do #=1  while @.#\=='';    parse var @.# n.# w.# v.# .;   end;      #=#-1
call show  'unsorted item list'                  /*display the header and the   @  list.*/
call sortD                                       /*invoke descemdomg sort for:  n. w. v.*/
call hdr "burglar's knapsack contents"
                 do j=1  for #      while totW<maxW;   f=1        /*process the items.  */
                 if totW+w.j>=maxW  then f=(maxW-totW)/w.j        /*calculate fraction. */
                 totW=totW+w.j*f;        totV=totV+v.j*f          /*add it ───► totals. */
                 call syf  left(word('{all}',1+(f\==1)),5)   n.j,  w.j*f,  v.j*f
                 end   /*j*/                     /* [↑]  display item, maybe with {all} */
call sep;  say                                   /* [↓]   $  suppresses trailing zeroes.*/
call sy  left('total weight', nL, "─"),              $(format(totW,,d))
call sy  left('total  value', nL, "─"),      ,       $(format(totV,,d))
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sortD: do s=2  to #;  a=n.s; !=w.s; u=v.s        /* [↓]  this is a descending sort.     */
         do k=s-1  by -1  to 1  while v.k/w.k<u/!;  ?=k+1; n.?=n.k; w.?=w.k; v.?=v.k;  end
       ?=k+1;   n.?=a;   w.?=!;   v.?=u
       end   /*s*/;               return         /* ↑↑↑ algorithm is OK for small arrays*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
hdr:   say;  say;  say center(arg(1),50,'─');   say;  call title;  call sep;      return
sep:   call sy  copies('═', nL),  copies("═", wL),  copies('═', vL);              return
show:  call hdr arg(1);           do j=1  for #;   call syf n.j, w.j, v.j;   end; return
sy:    say left('',9)   left(arg(1),nL)   right(arg(2),wL)   right(arg(3),vL);    return
syf:   call sy arg(1),    $(format(arg(2), , d)),    $(format(arg(3), , d));      return
title: call sy center('item',nL),  center("weight", wL),  center('value', vL);    return
$:     parse arg x;if pos(.,x)>1 then x=left(strip(strip(x,'T',0),,.),length(x)); return x
```

'''output'''   using the default inputs of:   <tt> 15   3 </tt>

```txt

────────────────unsorted item list────────────────

               item        weight    value
          ═══════════════ ═════════ ════════
          flitch              4       30
          beef                3.8     36
          pork                5.4     43
          greaves             2.4     45
          brawn               2.5     56
          welt                3.7     67
          ham                 3.6     90
          salami              3       95
          sausage             5.9     98


───────────burglar's knapsack contents────────────

               item        weight    value
          ═══════════════ ═════════ ════════
          {all} salami        3       95
          {all} ham           3.6     90
          {all} brawn         2.5     56
          {all} greaves       2.4     45
                welt          3.5     63.378
          ═══════════════ ═════════ ════════

          total weight───    15
          total  value───            349.378

```



### version 2


```rexx
 /*--------------------------------------------------------------------
 * 19.09.2014 Walter Pachl translated from FORTRAN
 * While this program works with all REXX interpreters,
 * see section ooRexx for a version that utilizes the ooRexx features
 *-------------------------------------------------------------------*/
  maxweight = 15.0
  input.0=0
  Call init_input 'beef',    3.8, 36.0
  Call init_input 'pork',    5.4, 43.0
  Call init_input 'ham',     3.6, 90.0
  Call init_input 'greaves', 2.4, 45.0
  Call init_input 'flitch',  4.0, 30.0
  Call init_input 'brawn',   2.5, 56.0
  Call init_input 'welt',    3.7, 67.0
  Call init_input 'salami',  3.0, 95.0
  Call init_input 'sausage', 5.9, 98.0

  /* sort the items by descending value per unit of weight */
  Do i = 1 to input.0
    Parse Var input.i name '*' weight '*' value
    vpu=value/weight;
    If i=1 Then Do
      item.0=1
      item.1=input.1
      vpu.1=vpu
      End
    Else Do
      Do ii=1 To item.0
        If vpu.ii<vpu Then
          Leave
        End
      Do jj=item.0 To ii By -1
        jj1=jj+1
        item.jj1=item.jj
        vpu.jj1=vpu.jj
        End
      item.ii=input.i
      vpu.ii=vpu
      item.0=item.0+1
      End
    End
  Say '#    vpu name    weight   value'
  Do i=1 To item.0
    Parse Var item.i name '*' weight '*' value
    Say i format(vpu.i,2,3) left(name,7) format(weight,2,3) format(value,3,3)
    End

  total_weight=0
  total_value =0
  Say ' '
  Say 'Item     Weight   Value'
  Do i=1 To item.0
    Parse Var item.i name '*' weight '*' value
    if total_weight+weight < maxweight then Do
      total_weight = total_weight + weight
      total_value  = total_value + value
      Say left(name,7) format(weight,3,3) format(value,3,3)
      End
    Else Do
      weight=maxweight-total_weight
      value=weight*vpu.i
      total_value = total_value + value
      total_weight = maxweight
      Say left(name,7) format(weight,3,3) format(value,3,3)
      Leave
      End
    End
  Say copies('-',23)
  Say 'total ' format(total_weight,4,3) format(total_value,3,3)
  Exit

init_input: Procedure Expose input.
  Parse Arg name,weight,value
  i=input.0+1
  input.i=name'*'weight'*'value
  input.0=i
  Return
```

```txt
#    vpu name    weight   value
1 31.667 salami   3.000  95.000
2 25.000 ham      3.600  90.000
3 22.400 brawn    2.500  56.000
4 18.750 greaves  2.400  45.000
5 18.108 welt     3.700  67.000
6 16.610 sausage  5.900  98.000
7  9.474 beef     3.800  36.000
8  7.963 pork     5.400  43.000
9  7.500 flitch   4.000  30.000

Item     Weight   Value
salami    3.000  95.000
ham       3.600  90.000
brawn     2.500  56.000
greaves   2.400  45.000
welt      3.500  63.378
-----------------------
total    15.000 349.378
```




## Ruby


```ruby
items = [ [:beef   , 3.8, 36],
          [:pork   , 5.4, 43],
          [:ham    , 3.6, 90],
          [:greaves, 2.4, 45],
          [:flitch , 4.0, 30],
          [:brawn  , 2.5, 56],
          [:welt   , 3.7, 67],
          [:salami , 3.0, 95],
          [:sausage, 5.9, 98] ].sort_by{|item, weight, price| -price / weight}
maxW, value = 15.0, 0
items.each do |item, weight, price|
  if (maxW -= weight) > 0
    puts "Take all #{item}"
    value += price
  else
    puts "Take %gkg of %s" % [t=weight+maxW, item], "",
         "Total value of swag is %g" % (value+(price/weight)*t)
    break
  end
end
```

```txt

Take all salami
Take all ham
Take all brawn
Take all greaves
Take 3.5kg of welt

Total value of swag is 349.378

```



## Run BASIC

```runbasic
dim name$(9)
dim wgt(9)
dim price(9)
dim tak$(100)

name$(1) = "beef"      : wgt(1) = 3.8 : price(1) = 36
name$(2) = "pork"      : wgt(2) = 5.4 : price(2) = 43
name$(3) = "ham"       : wgt(3) = 3.6 : price(3) = 90
name$(4) = "greaves"   : wgt(4) = 2.4 : price(4) = 45
name$(5) = "flitch"    : wgt(5) = 4.0 : price(5) = 30
name$(6) = "brawn"     : wgt(6) = 2.5 : price(6) = 56
name$(7) = "welt"      : wgt(7) = 3.7 : price(7) = 67
name$(8) = "salami"    : wgt(8) = 3.0 : price(8) = 95
name$(9) = "sausage"   : wgt(9) = 5.9 : price(9) = 98

for beef    =         0 to 15 step 3.8
 for pork    =        0 to 15 step 5.4
  for ham     =       0 to 15 step 3.6
   for greaves =      0 to 15 step 2.4
    for flitch  =     0 to 15 step 4.0
     for brawn   =    0 to 15 step 2.5
      for welt    =   0 to 15 step 3.7
       for salami  =  0 to 15 step 3.0
        for sausage = 0 to 15 step 5.9
          if beef + pork + ham + greaves + flitch + brawn + welt + salami + sausage <= 15 then
             totPrice = beef     / 3.8 * 36 + _
                        pork     / 5.4 * 43 + _
                        ham      / 3.6 * 90 + _
                        greaves  / 2.4 * 45 + _
                        flitch   / 4.0 * 30 + _
                        brawn    / 2.5 * 56 + _
                        welt     / 3.7 * 67 + _
                        salami   / 3.0 * 95 + _
                        sausage  / 5.9 * 98
            if totPrice >= maxPrice then
              maxPrice = totPrice
              theMax   = max(totPrice,maxPrice)
              t        = t + 1
              tak$(t)  = str$(maxPrice);",";beef;",";pork;",";ham;",";greaves;",";flitch;",";brawn;",";welt;",";salami;",";sausage
            end if
          end if
next:next :next :next :next :next :next :next :next

print "Best 2 Options":print
for i = t-1 to t
 totTake = val(word$(tak$(i),1,","))
 if totTake > 0 then
   totWgt  = 0
   for j   = 2 to 10
    wgt    = val(word$(tak$(i),j,","))
    totWgt = totWgt + wgt
    value  = wgt / wgt(j - 1) * price(j - 1)
    if wgt <> 0 then print name$(j-1);chr$(9);"Value: ";using("###.#",value);chr$(9);"Weight: ";using("##.#",wgt)
   next j
    print "-------- Total ";using("###.#",totTake);chr$(9);"Weight: ";totWgt
 end if
next i
```
Output:

```txt
Best 2 Options

salami	Value: 285.0	Weight:  9.0
sausage	Value:  98.0	Weight:  5.9
-------- Total 383.0	Weight: 14.9

salami	Value: 475.0	Weight: 15.0
-------- Total 475.0	Weight: 15.0
```



## Rust


```rust
fn main() {
    let items: [(&str, f32, u8); 9] = [
        ("beef", 3.8, 36),
        ("pork", 5.4, 43),
        ("ham", 3.6, 90),
        ("greaves", 2.4, 45),
        ("flitch", 4.0, 30),
        ("brawn", 2.5, 56),
        ("welt", 3.7, 67),
        ("salami", 3.0, 95),
        ("sausage", 5.9, 98),
    ];
    let mut weight: f32 = 15.0;
    let mut values: Vec<(&str, f32, f32)> = Vec::new();
    for item in &items {
        values.push((item.0, f32::from(item.2) / item.1, item.1));
    }

    values.sort_by(|a, b| (a.1).partial_cmp(&b.1).unwrap());
    values.reverse();

    for choice in values {
        if choice.2 <= weight {
            println!("Grab {:.1} kgs of {}", choice.2, choice.0);
            weight -= choice.2;
            if (choice.2 - weight).abs() < std::f32::EPSILON {
                return;
            }
        } else {
            println!("Grab {:.1} kgs of {}", weight, choice.0);
            return;
        }
    }
}
```
 Output:
```txt

Grab 3.0 kgs of salami
Grab 3.6 kgs of ham
Grab 2.5 kgs of brawn
Grab 2.4 kgs of greaves
Grab 3.5 kgs of welt

```



## SAS

Use LP solver in SAS/OR:

```sas
/* create SAS data set */
data mydata;
   input item $ weight value;
   datalines;
beef    3.8  36
pork    5.4  43
ham     3.6  90
greaves 2.4  45
flitch  4.0  30
brawn   2.5  56
welt    3.7  67
salami  3.0  95
sausage 5.9  98
;

/* call OPTMODEL procedure in SAS/OR */
proc optmodel;
   /* declare sets and parameters, and read input data */
   set <str> ITEMS;
   num weight {ITEMS};
   num value {ITEMS};
   read data mydata into ITEMS=[item] weight value;

   /* declare variables, objective, and constraints */
   var WeightSelected {i in ITEMS} >= 0 <= weight[i];
   max TotalValue = sum {i in ITEMS} (value[i]/weight[i]) * WeightSelected[i];
   con WeightCon:
      sum {i in ITEMS} WeightSelected[i] <= 15;

   /* call linear programming (LP) solver */
   solve;

   /* print optimal solution */
   print TotalValue;
   print {i in ITEMS: WeightSelected[i].sol > 1e-3} WeightSelected;
quit;
```


Output:

```txt
TotalValue
349.38

[1] WeightSelected
brawn 2.5
greaves 2.4
ham 3.6
salami 3.0
welt 3.5
```


## Scala

===Functional approach (Tail recursive)===

```Scala
import scala.annotation.tailrec

object ContinousKnapsackForRobber extends App {
  val MaxWeight = 15.0
  val items = Seq(
    Item("Beef",    3.8, 3600),
    Item("Pork",    5.4, 4300),
    Item("Ham",     3.6, 9000),
    Item("Greaves", 2.4, 4500),
    Item("Flitch",  4.0, 3000),
    Item("Brawn",   2.5, 5600),
    Item("Welt",    3.7, 6700),
    Item("Salami",  3.0, 9500),
    Item("Sausage", 5.9, 9800))

  // sort items by value per unit weight in descending order
  def sortedItems = items.sortBy(it => -it.value / it.weight)

  @tailrec
  def packer(notPacked: Seq[Item], packed: Lootsack): Lootsack = {

    if (!packed.isNotFull || notPacked.isEmpty) packed
    else {
      val try2fit = packed.copy(bagged = notPacked.head +: packed.bagged)
      if (try2fit.isNotFull) packer(notPacked.tail, try2fit)
      else {
        try2fit.copy(lastPiece = packed.weightLeft / notPacked.head.weight)
      }
    }
  }

  case class Item(name: String, weight: Double, value: Int)

  case class Lootsack(bagged: Seq[Item], lastPiece: Double = 1.0) {
    private val totWeight = if (bagged.isEmpty) 0.0
    else bagged.tail.map(_.weight).sum + bagged.head.weight * lastPiece

    def isNotFull: Boolean = weightLeft > 0

    def weightLeft: Double = MaxWeight - totWeight

    override def toString = f"${show(bagged, lastPiece)}Totals: weight: $totWeight%4.1f, value: $totValue%6.2f"

    private def totValue: BigDecimal = if (bagged.isEmpty) 0.0
    else (bagged.tail.map(_.value).sum + bagged.head.value * lastPiece) / 100

    private def show(is: Seq[Item], percentage: Double) = {
      def toStr(is: Seq[Item], percentage: Double = 1): String =
        is.map(it => f"${percentage * 100}%6.2f%% ${it.name}%-7s ${
          it.weight * percentage}%4.1f ${it.value * percentage / 100}%6.2f\n").mkString

      toStr(is.tail.reverse) + toStr(Seq(is.head), percentage)
    }
  }

  println(packer(sortedItems, Lootsack(Nil)))
}
```

```txt
100.00% Salami   3.0  95.00
100.00% Ham      3.6  90.00
100.00% Brawn    2.5  56.00
100.00% Greaves  2.4  45.00
 94.59% Welt     3.5  63.38
Totals: weight: 15.0, value: 349.38
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/RHZQ4Xj/1 ScalaFiddle (JavaScript)] <!--or by [https://scastie.scala-lang.org/mDoBS77YSG2Z7w5xdAPzcw Scastie (JVM)]-->.

## Sidef

```ruby
var items =
[
        [:beef,    3.8, 36],
        [:pork,    5.4, 43],
        [:ham,     3.6, 90],
        [:greaves, 2.4, 45],
        [:flitch,  4.0, 30],
        [:brawn,   2.5, 56],
        [:welt,    3.7, 67],
        [:salami,  3.0, 95],
        [:sausage, 5.9, 98],
].sort {|a,b| b[2]/b[1] <=> a[2]/a[1] }

var (limit, value) = (15, 0);
print "Item   Fraction Weight Value\n";

items.each { |item|
    var ratio = (item[1] > limit ? limit/item[1] : 1);
    value += item[2]*ratio;
    limit -= item[1];
    if (ratio == 1) {
        printf("%-8s %4s %7.2f %6.2f\n", item[0], 'all', item[1], item[2]);
    }
    else {
        printf("%-8s %-4.2f %7.2f %6.2f\n", item[0], ratio, item[1]*ratio, item[2]*ratio);
        break;
    }
}

say "#{'-'*28}\ntotal value: #{'%.14g' % value }"
```

```txt

Item   Fraction Weight Value
salami    all    3.00  95.00
ham       all    3.60  90.00
brawn     all    2.50  56.00
greaves   all    2.40  45.00
welt     0.95    3.50  63.38
----------------------------
total value: 349.37837837838

```



## Tcl


```tcl
package require Tcl 8.5

# Uses the trivial greedy algorithm
proc continuousKnapsack {items massLimit} {
    # Add in the unit prices
    set idx -1
    foreach item $items {
	lassign $item name mass value
	lappend item [expr {$value / $mass}]
	lset items [incr idx] $item
    }

    # Sort by unit prices
    set items [lsort -decreasing -real -index 3 $items]

    # Add items, using most valuable-per-unit first
    set result {}
    set total 0.0
    set totalValue 0
    foreach item $items {
	lassign $item name mass value unit
	if {$total + $mass < $massLimit} {
	    lappend result [list $name $mass $value]
	    set total [expr {$total + $mass}]
	    set totalValue [expr {$totalValue + $value}]
	} else {
	    set mass [expr {$massLimit - $total}]
	    set value [expr {$unit * $mass}]
	    lappend result [list $name $mass $value]
	    set totalValue [expr {$totalValue + $value}]
	    break
	}
    }

    # We return the total value too, purely for convenience
    return [list $result $totalValue]
}
```

Driver for this particular problem:

```tcl
set items {
    {beef	3.8	36}
    {pork	5.4	43}
    {ham	3.6	90}
    {greaves	2.4	45}
    {flitch	4.0	30}
    {brawn	2.5	56}
    {welt	3.7	67}
    {salami	3.0	95}
    {sausage	5.9	98}
}

lassign [continuousKnapsack $items 15.0] contents totalValue
puts [format "total value of knapsack: %.2f" $totalValue]
puts "contents:"
foreach item $contents {
    lassign $item name mass value
    puts [format "\t%.1fkg of %s, value %.2f" $mass $name $value]
}
```

Output:

```txt

total value of knapsack: 349.38
contents:
	3.0kg of salami, value 95.00
	3.6kg of ham, value 90.00
	2.5kg of brawn, value 56.00
	2.4kg of greaves, value 45.00
	3.5kg of welt, value 63.38

```



## Ursala

We might as well leave this one to the experts by setting it up as a
[[wp:Linear_programming|linear programming]] problem and handing it off to an external library (which will be either [http://sourceforge.net/projects/lpsolve lpsolve] or
[http://www.gnu.org/software/glpk/glpk.html glpk] depending on the
[http://www.basis.netii.net/avram run-time system] configuration).

```Ursala
#import flo
#import lin

items = # name: (weight,price)

<
   'beef   ': (3.8,36.0),
   'pork   ': (5.4,43.0),
   'ham    ': (3.6,90.0),
   'greaves': (2.4,45.0),
   'flitch ': (4.0,30.0),
   'brawn  ': (2.5,56.0),
   'welt   ': (3.7,67.0),
   'salami ': (3.0,95.0),
   'sausage': (5.9,98.0)>

system = # a function to transform the item list to the data structure needed by the solver

linear_system$[
   lower_bounds: *nS ~&\0.,         # all zeros because we can't steal less than zero
   upper_bounds: ~&nmlPXS,          # can't steal more than what's in the shop
   costs: * ^|/~& negative+ vid,    # prices divided by weights, negated so as to maximize
   equations: ~&iNC\15.+ 1.-*@nS]   # 1 equation constraining the total weight to 15

#cast %em

main = solution system items
```

output:

```txt
<
   'brawn  ': 2.500000e+00,
   'greaves': 2.400000e+00,
   'ham    ': 3.600000e+00,
   'salami ': 3.000000e+00,
   'welt   ': 3.500000e+00>
```



## XPL0


```XPL0
int  Name, Price, I, BestItem;
real    Weight, Best, ItemWt, TotalWt;
def     Items = 9;
real    PricePerWt(Items);
int     Taken(Items);
include c:\cxpl\codes;

[Name:=  ["beef","pork","ham","greaves","flitch","brawn","welt","salami","sausage"];
Weight:= [ 3.8,   5.4,   3.6,  2.4,      4.0,     2.5,    3.7,   3.0,     5.9];
Price:=  [ 36,    43,    90,   45,       30,      56,     67,    95,      98];

for I:= 0 to Items-1 do
        [PricePerWt(I):= float(Price(I)) / Weight(I);
        Taken(I):= false;
        ];
Format(2,1);
TotalWt:= 0.0;
repeat  Best:= 0.0;
        for I:= 0 to Items-1 do
            if not Taken(I) and PricePerWt(I) > Best then
                [Best:= PricePerWt(I);  BestItem:= I];
        Taken(BestItem):= true;                 \take item
        ItemWt:= Weight(BestItem);              \get its weight
        TotalWt:= TotalWt + ItemWt;             \add to total weight
        if TotalWt > 15.0 then                  \if total is too much, reduce
            ItemWt:= ItemWt - (TotalWt-15.0);   \item weight by amount it's over
        RlOut(0, ItemWt);  Text(0, " kg of ");  \show weight and item
        Text(0, Name(BestItem));  CrLf(0);
until   TotalWt >= 15.0;                        \all we can steal
]
```


Output:

```txt

 3.0 kg of salami
 3.6 kg of ham
 2.5 kg of brawn
 2.4 kg of greaves
 3.5 kg of welt

```



## zkl

```zkl
items:=List( T(3.8, 36.0, "beef"),  T(5.4, 43.0, "pork"),  // weight, value, name
	     T(3.6, 90.0, "ham"),   T(2.4, 45.0, "greaves"),
	     T(4.0, 30.0, "flitch"),T(2.5, 56.0, "brawn"),
	     T(3.7, 67.0, "welt"),  T(3.0, 95.0, "salami"),
	     T(5.9, 98.0, "sausage"),
);
fcn item_cmp(a,b){ a[1]/a[0] > b[1]/b[0] }

items.sort(item_cmp);
space := 15.0;
foreach it in (items){ w,_,nm:=it;
   if (space >= w){ println("take all ",nm); space-=w }
   else{ println("take %gkg of %gkg of %s".fmt(space,w,nm)); break }
}
```

```txt

take all salami
take all ham
take all brawn
take all greaves
take 3.5kg of 3.7kg of welt

```



