+++
title = "Power set"
description = ""
date = 2019-09-27T08:09:25Z
aliases = []
[extra]
id = 2880
[taxonomies]
categories = []
tags = []
+++

{{task|Discrete math}}
{{omit from|GUISS}}

A   [[set]]   is a collection (container) of certain values,
without any particular order, and no repeated values.

It corresponds with a finite set in mathematics.

A set can be implemented as an associative array (partial mapping)
in which the value of each key-value pair is ignored.

Given a set S, the [[wp:Power_set|power set]] (or powerset) of S, written P(S), or 2<sup>S</sup>, is the set of all subsets of S.


;Task:
By using a library or built-in set type, or by defining a set type with necessary operations, write a function with a set S as input that yields the power set 2<sup>S</sup> of S.


For example, the power set of     {1,2,3,4}     is
::: {{}, {1}, {2}, {1,2}, {3}, {1,3}, {2,3}, {1,2,3}, {4}, {1,4}, {2,4}, {1,2,4}, {3,4}, {1,3,4}, {2,3,4}, {1,2,3,4}}.

For a set which contains n elements, the corresponding power set has 2<sup>n</sup> elements, including the edge cases of [[wp:Empty_set|empty set]].<br />

The power set of the empty set is the set which contains itself (2<sup>0</sup> = 1):<br />
::: <math>\mathcal{P}</math>(<math>\varnothing</math>) = { <math>\varnothing</math> }<br />

And the power set of the set which contains only the empty set, has two subsets, the empty set and the set which contains the empty set (2<sup>1</sup> = 2):<br />
::: <math>\mathcal{P}</math>({<math>\varnothing</math>}) = { <math>\varnothing</math>,  { <math>\varnothing</math> } }



'''Extra credit: ''' Demonstrate that your language supports these last two powersets.





## ABAP


This works for ABAP Version 7.40 and above


```ABAP

report z_powerset.

interface set.
  methods:
    add_element
      importing
        element_to_be_added type any
      returning
        value(new_set)      type ref to set,

    remove_element
      importing
        element_to_be_removed type any
      returning
        value(new_set)        type ref to set,

    contains_element
      importing
        element_to_be_found type any
      returning
        value(contains)     type abap_bool,

    get_size
      returning
        value(size) type int4,

    is_equal
      importing
        set_to_be_compared_with type ref to set
      returning
        value(equal)            type abap_bool,

    get_elements
      exporting
        elements type any table,

    stringify
      returning
        value(stringified_set) type string.
endinterface.


class string_set definition.
  public section.
    interfaces:
      set.


    methods:
      constructor
        importing
          elements type stringtab optional,

      build_powerset
        returning
          value(powerset) type ref to string_set.


  private section.
    data elements type stringtab.
endclass.


class string_set implementation.
  method constructor.
    loop at elements into data(element).
      me->set~add_element( element ).
    endloop.
  endmethod.


  method set~add_element.
    if not line_exists( me->elements[ table_line = element_to_be_added ] ).
      append element_to_be_added to me->elements.
    endif.

    new_set = me.
  endmethod.


  method set~remove_element.
    if line_exists( me->elements[ table_line = element_to_be_removed ] ).
      delete me->elements where table_line = element_to_be_removed.
    endif.

    new_set = me.
  endmethod.


  method set~contains_element.
    contains = cond abap_bool(
      when line_exists( me->elements[ table_line = element_to_be_found ] )
      then abap_true
      else abap_false ).
  endmethod.


  method set~get_size.
    size = lines( me->elements ).
  endmethod.


  method set~is_equal.
    if set_to_be_compared_with->get_size( ) ne me->set~get_size( ).
      equal = abap_false.

      return.
    endif.

    loop at me->elements into data(element).
      if not set_to_be_compared_with->contains_element( element ).
        equal = abap_false.

        return.
      endif.
    endloop.

    equal = abap_true.
  endmethod.


  method set~get_elements.
    elements = me->elements.
  endmethod.


  method set~stringify.
    stringified_set = cond string(
      when me->elements is initial
      then `∅`
      when me->elements eq value stringtab( ( `∅` ) )
      then `{ ∅ }`
      else reduce string(
        init result = `{ `
        for element in me->elements
        next result = cond string(
          when element eq ``
          then |{ result }∅, |
          when strlen( element ) eq 1 and element ne `∅`
          then |{ result }{ element }, |
          else |{ result }\{{ element }\}, | ) ) ).

    stringified_set = replace(
      val = stringified_set
      regex = `, $`
      with = ` }`).
  endmethod.


  method build_powerset.
    data(powerset_elements) = value stringtab( ( `` ) ).

    loop at me->elements into data(element).
      do lines( powerset_elements ) times.
        if powerset_elements[ sy-index ] ne `∅`.
          append |{ powerset_elements[ sy-index ] }{ element }| to powerset_elements.
        else.
          append element to powerset_elements.
        endif.
      enddo.
    endloop.

    powerset = new string_set( powerset_elements ).
  endmethod.
endclass.


start-of-selection.
  data(set1) = new string_set( ).
  data(set2) = new string_set( ).
  data(set3) = new string_set( ).

  write: |𝑷( { set1->set~stringify( ) } ) -> { set1->build_powerset( )->set~stringify( ) }|, /.

  set2->set~add_element( `∅` ).
  write: |𝑷( { set2->set~stringify( ) } ) -> { set2->build_powerset( )->set~stringify( ) }|, /.

  set3->set~add_element( `1` )->add_element( `2` )->add_element( `3` )->add_element( `3` )->add_element( `4`
    )->add_element( `4` )->add_element( `4` ).
  write: |𝑷( { set3->set~stringify( ) } ) -> { set3->build_powerset( )->set~stringify( ) }|, /.

```


{{out}}


```txt

𝑷( ∅ ) -> { ∅ }

𝑷( { ∅ } ) -> { ∅, {∅} }

𝑷( { 1, 2, 3, 4 } ) -> { ∅, 1, 2, {12}, 3, {13}, {23}, {123}, 4, {14}, {24}, {124}, {34}, {134}, {234}, {1234} }

```



## Ada


A solution (without recursion) that prints the power set of the n arguments passed by the command line. The idea is that the i'th bit of a natural between 0 and <math>2^n-1</math> indicates whether or not we should put the i'th element of the command line inside the set.


```Ada

with Ada.Text_IO, Ada.Command_Line;
use Ada.Text_IO, Ada.Command_Line;

procedure powerset is
begin
	for set in 0..2**Argument_Count-1 loop
		Put ("{");
		declare
			k : natural := set;
			first : boolean := true;
		begin
			for i in 1..Argument_Count loop
				if k mod 2 = 1 then
					Put ((if first then "" else ",") & Argument (i));
					first := false;
			  	end if;
				k := k / 2; -- we go to the next bit of "set"
			end loop;
		end;
		Put_Line("}");
	end loop;
end powerset;

```



{{out}}


```txt
>./powerset a b c d
{}
{a}
{b}
{a,b}
{c}
{a,c}
{b,c}
{a,b,c}
{d}
{a,d}
{b,d}
{a,b,d}
{c,d}
{a,c,d}
{b,c,d}
{a,b,c,d}
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

Requires: ALGOL 68g mk14.1+

```algol68
MODE MEMBER = INT;

PROC power set = ([]MEMBER s)[][]MEMBER:(
  [2**UPB s]FLEX[1:0]MEMBER r;
  INT upb r := 0;
  r[upb r +:= 1] := []MEMBER(());
  FOR i TO UPB s DO
    MEMBER e = s[i];
    FOR j TO upb r DO
      [UPB r[j] + 1]MEMBER x;
      x[:UPB x-1] := r[j];
      x[UPB x] := e; # append to the end of x #
      r[upb r +:= 1] := x # append to end of r #
    OD
  OD;
  r[upb r] := s;
  r
);
# Example: #
test:(
  [][]MEMBER set = power set((1, 2, 4));
  FOR member TO UPB set DO
    INT upb = UPB set[member];
    FORMAT repr set = $"("f( upb=0 | $$ | $n(upb-1)(d", ")d$ )");"$;
    printf(($"set["d"] = "$,member, repr set, set[member],$l$))
  OD
)
```

{{out}}

```txt

set[1] = ();
set[2] = (1);
set[3] = (2);
set[4] = (1, 2);
set[5] = (4);
set[6] = (1, 4);
set[7] = (2, 4);
set[8] = (1, 2, 4);

```



## AppleScript

{{Trans|JavaScript}}
(functional composition examples)
{{Trans|Haskell}}

```AppleScript
-- POWER SET -----------------------------------------------------------------

-- powerset :: [a] -> [[a]]
on powerset(xs)
    script subSet
        on |λ|(acc, x)
            script cons
                on |λ|(y)
                    {x} & y
                end |λ|
            end script

            acc & map(cons, acc)
        end |λ|
    end script

    foldr(subSet, {{}}, xs)
end powerset


-- TEST ----------------------------------------------------------------------
on run
    script test
        on |λ|(x)
            set {setName, setMembers} to x
            {setName, powerset(setMembers)}
        end |λ|
    end script

    map(test, [¬
        ["Set [1,2,3]", {1, 2, 3}], ¬
        ["Empty set", {}], ¬
        ["Set containing only empty set", {{}}]])

    --> {{"Set [1,2,3]", {{}, {3}, {2}, {2, 3}, {1}, {1, 3}, {1, 2}, {1, 2, 3}}},
    -->  {"Empty set", {{}}},
    -->  {"Set containing only empty set", {{}, {{}}}}}
end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

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
```

{{Out}}

```AppleScript
{{"Set [1,2,3]", {{}, {3}, {2}, {2, 3}, {1}, {1, 3}, {1, 2}, {1, 2, 3}}},
 {"Empty set", {{}}},
 {"Set containing only empty set", {{}, {{}}}}}
```



## ATS

<lang>
(* ****** ****** *)
//
#include
"share/atspre_define.hats" // defines some names
#include
"share/atspre_staload.hats" // for targeting C
#include
"share/HATS/atspre_staload_libats_ML.hats" // for ...
//
(* ****** ****** *)
//
extern
fun
Power_set(xs: list0(int)): void
//
(* ****** ****** *)

// Helper: fast power function.
fun power(n: int, p: int): int =
	if p = 1 then n
	else if p = 0 then 1
	else if p % 2 = 0 then power(n*n, p/2)
	else n * power(n, p-1)

fun print_list(list: list0(int)): void =
  case+ list of
  | nil0() => println!(" ")
  | cons0(car, crd) =>
    let
      val () = begin print car; print ','; end
      val () = print_list(crd)
    in
    end

fun get_list_length(list: list0(int), length: int): int =
  case+ list of
  | nil0() => length
  | cons0(car, crd) => get_list_length(crd, length+1)


fun get_list_from_bit_mask(mask: int, list: list0(int), result: list0(int)): list0(int) =
  if mask = 0 then result
  else
    case+ list of
    | nil0() => result
    | cons0(car, crd) =>
      let
        val current: int = mask % 2
      in
        if current = 0 then
          get_list_from_bit_mask(mask >> 1, crd, result)
        else
          get_list_from_bit_mask(mask >> 1, crd, list0_cons(car, result))
      end


implement
Power_set(xs) = let
  val len: int = get_list_length(xs, 0)
  val pow: int = power(2, len)
  fun loop(mask: int, list: list0(int)): void =
    if mask > 0 && mask >= pow then ()
    else
      let
        val () = print_list(get_list_from_bit_mask(mask, list, list0_nil()))
      in
        loop(mask+1, list)
      end
  in
    loop(0, xs)
  end

(* ****** ****** *)

implement
main0() =
let
  val xs: list0(int) = cons0(1, list0_pair(2, 3))
in
  Power_set(xs)
end (* end of [main0] *)

(* ****** ****** *)

```



## AutoHotkey

ahk [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=147 discussion]

```autohotkey
a = 1,a,--             ; elements separated by commas
StringSplit a, a, `,   ; a0 = #elements, a1,a2,... = elements of the set

t = {
Loop % (1<<a0) {       ; generate all 0-1 sequences
   x := A_Index-1
   Loop % a0
      t .= (x>>A_Index-1) & 1 ? a%A_Index% "," : ""
   t .= "}`n{"         ; new subsets in new lines
}
MsgBox % RegExReplace(SubStr(t,1,StrLen(t)-1),",}","}")
```



## AWK


```AWK
cat power_set.awk
#!/usr/local/bin/gawk -f

# User defined function
function tochar(l,n,	r) {
 while (l) { n--; if (l%2 != 0) r = r sprintf(" %c ",49+n); l = int(l/2) }; return r
}

# For each input
{ for (i=0;i<=2^NF-1;i++) if (i == 0) printf("empty\n"); else printf("(%s)\n",tochar(i,NF)) }

```


{{out}}

```txt

$ gawk -f power_set.awk
1 2 3 4
empty
( 4 )
( 3 )
( 4  3 )
( 2 )
( 4  2 )
( 3  2 )
( 4  3  2 )
( 1 )
( 4  1 )
( 3  1 )
( 4  3  1 )
( 2  1 )
( 4  2  1 )
( 3  2  1 )
( 4  3  2  1 )

```



## BBC BASIC

The elements of a set are represented as the bits in an integer (hence the maximum size of set is 32).

```bbcbasic
      DIM list$(3) : list$() = "1", "2", "3", "4"
      PRINT FNpowerset(list$())
      END

      DEF FNpowerset(list$())
      IF DIM(list$(),1) > 31 ERROR 100, "Set too large to represent as integer"
      LOCAL i%, j%, s$
      s$ = "{"
      FOR i% = 0 TO (2 << DIM(list$(),1)) - 1
        s$ += "{"
        FOR j% = 0 TO DIM(list$(),1)
          IF i% AND (1 << j%) s$ += list$(j%) + ","
        NEXT
        IF RIGHT$(s$) = "," s$ = LEFT$(s$)
        s$ += "},"
      NEXT i%
      = LEFT$(s$) + "}"
```

{{out}}

```txt

{{},{1},{2},{1,2},{3},{1,3},{2,3},{1,2,3},{4},{1,4},{2,4},{1,2,4},{3,4},{1,3,4},{2,3,4},{1,2,3,4}}

```



## Bracmat


```bracmat
( ( powerset
  =   done todo first
    .   !arg:(?done.?todo)
      & (   !todo:%?first ?todo
          & (powerset$(!done !first.!todo),powerset$(!done.!todo))
        | !done
        )
  )
& out$(powerset$(.1 2 3 4))
);
```

{{out}}

```txt
  1 2 3 4
, 1 2 3
, 1 2 4
, 1 2
, 1 3 4
, 1 3
, 1 4
, 1
, 2 3 4
, 2 3
, 2 4
, 2
, 3 4
, 3
, 4
,
```



## Burlesque



```burlesque

blsq ) {1 2 3 4}R@
{{} {1} {2} {1 2} {3} {1 3} {2 3} {1 2 3} {4} {1 4} {2 4} {1 2 4} {3 4} {1 3 4} {2 3 4} {1 2 3 4}}

```



## C


```c
#include <stdio.h>

struct node {
	char *s;
	struct node* prev;
};

void powerset(char **v, int n, struct node *up)
{
	struct node me;

	if (!n) {
		putchar('[');
		while (up) {
			printf(" %s", up->s);
			up = up->prev;
		}
		puts(" ]");
	} else {
		me.s = *v;
		me.prev = up;
		powerset(v + 1, n - 1, up);
		powerset(v + 1, n - 1, &me);
	}
}

int main(int argc, char **argv)
{
	powerset(argv + 1, argc - 1, 0);
	return 0;
}
```

{{out}}

```txt

% ./a.out 1 2 3
[ ]
[ 3 ]
[ 2 ]
[ 3 2 ]
[ 1 ]
[ 3 1 ]
[ 2 1 ]
[ 3 2 1 ]

```



## C++


=== Non-recursive version ===


```cpp
#include <iostream>
#include <set>
#include <vector>
#include <iterator>
#include <algorithm>
typedef std::set<int> set_type;
typedef std::set<set_type> powerset_type;

powerset_type powerset(set_type const& set)
{
  typedef set_type::const_iterator set_iter;
  typedef std::vector<set_iter> vec;
  typedef vec::iterator vec_iter;

  struct local
  {
    static int dereference(set_iter v) { return *v; }
  };

  powerset_type result;

  vec elements;
  do
  {
    set_type tmp;
    std::transform(elements.begin(), elements.end(),
                   std::inserter(tmp, tmp.end()),
                   local::dereference);
    result.insert(tmp);
    if (!elements.empty() && ++elements.back() == set.end())
    {
      elements.pop_back();
    }
    else
    {
      set_iter iter;
      if (elements.empty())
      {
        iter = set.begin();
      }
      else
      {
        iter = elements.back();
        ++iter;
      }
      for (; iter != set.end(); ++iter)
      {
        elements.push_back(iter);
      }
    }
  } while (!elements.empty());

  return result;
}

int main()
{
  int values[4] = { 2, 3, 5, 7 };
  set_type test_set(values, values+4);

  powerset_type test_powerset = powerset(test_set);

  for (powerset_type::iterator iter = test_powerset.begin();
       iter != test_powerset.end();
       ++iter)
  {
    std::cout << "{ ";
    char const* prefix = "";
    for (set_type::iterator iter2 = iter->begin();
         iter2 != iter->end();
         ++iter2)
    {
      std::cout << prefix << *iter2;
      prefix = ", ";
    }
    std::cout << " }\n";
  }
}
```


{{out}}

```txt

{  }
{ 2 }
{ 2, 3 }
{ 2, 3, 5 }
{ 2, 3, 5, 7 }
{ 2, 3, 7 }
{ 2, 5 }
{ 2, 5, 7 }
{ 2, 7 }
{ 3 }
{ 3, 5 }
{ 3, 5, 7 }
{ 3, 7 }
{ 5 }
{ 5, 7 }
{ 7 }

```



### = C++14 version =


This simplified version has identical output to the previous code.


```cpp

#include <set>
#include <iostream>

template <class S>
auto powerset(const S& s)
{
    std::set<S> ret;
    ret.emplace();
    for (auto&& e: s) {
        std::set<S> rs;
        for (auto x: ret) {
            x.insert(e);
            rs.insert(x);
        }
        ret.insert(begin(rs), end(rs));
    }
    return ret;
}

int main()
{
    std::set<int> s = {2, 3, 5, 7};
    auto pset = powerset(s);

    for (auto&& subset: pset) {
        std::cout << "{ ";
        char const* prefix = "";
        for (auto&& e: subset) {
            std::cout << prefix << e;
            prefix = ", ";
        }
        std::cout << " }\n";
    }
}

```



###  Recursive version



```cpp
#include <iostream>
#include <set>

template<typename Set> std::set<Set> powerset(const Set& s, size_t n)
{
    typedef typename Set::const_iterator SetCIt;
    typedef typename std::set<Set>::const_iterator PowerSetCIt;
    std::set<Set> res;
    if(n > 0) {
        std::set<Set> ps = powerset(s, n-1);
        for(PowerSetCIt ss = ps.begin(); ss != ps.end(); ss++)
            for(SetCIt el = s.begin(); el != s.end(); el++) {
                Set subset(*ss);
                subset.insert(*el);
                res.insert(subset);
            }
        res.insert(ps.begin(), ps.end());
    } else
        res.insert(Set());
    return res;
}
template<typename Set> std::set<Set> powerset(const Set& s)
{
    return powerset(s, s.size());
}

```


=={{header|C sharp|C#}}==

```csharp

public IEnumerable<IEnumerable<T>> GetPowerSet<T>(List<T> list)
{
    return from m in Enumerable.Range(0, 1 << list.Count)
                  select
                      from i in Enumerable.Range(0, list.Count)
                      where (m & (1 << i)) != 0
                      select list[i];
}

public void PowerSetofColors()
{
    var colors = new List<KnownColor> { KnownColor.Red, KnownColor.Green,
        KnownColor.Blue, KnownColor.Yellow };

    var result = GetPowerSet(colors);

    Console.Write( string.Join( Environment.NewLine,
        result.Select(subset =>
            string.Join(",", subset.Select(clr => clr.ToString()).ToArray())).ToArray()));
}


```


{{out}}

```txt

  Red
  Green
  Red,Green
  Blue
  Red,Blue
  Green,Blue
  Red,Green,Blue
  Yellow
  Red,Yellow
  Green,Yellow
  Red,Green,Yellow
  Blue,Yellow
  Red,Blue,Yellow
  Green,Blue,Yellow
  Red,Green,Blue,Yellow

```


An alternative implementation for an arbitrary number of elements:


```csharp

  public IEnumerable<IEnumerable<T>> GetPowerSet<T>(IEnumerable<T> input) {
    var seed = new List<IEnumerable<T>>() { Enumerable.Empty<T>() }
      as IEnumerable<IEnumerable<T>>;

    return input.Aggregate(seed, (a, b) =>
      a.Concat(a.Select(x => x.Concat(new List<T>() { b }))));
  }

```



Non-recursive version


```csharp

  using System;
  class Powerset
  {
    static int count = 0, n = 4;
    static int [] buf = new int [n];

    static void Main()
    {
  	int ind = 0;
  	int n_1 = n - 1;
  	for (;;)
  	{
  	  for (int i = 0; i <= ind; ++i) Console.Write("{0, 2}", buf [i]);
  	  Console.WriteLine();
  	  count++;

  	  if (buf [ind] < n_1) { ind++; buf [ind] = buf [ind - 1] + 1; }
  	  else if (ind > 0) { ind--; buf [ind]++; }
  	  else break;
  	}
  	Console.WriteLine("n=" + n + "   count=" + count);
    }
  }

```


----------------


Recursive version

```csharp

using System;
class Powerset
{
  static int n = 4;
  static int [] buf = new int [n];

  static void Main()
  {
    rec(0, 0);
  }

  static void rec(int ind, int begin)
  {
    for (int i = begin; i < n; i++)
    {
      buf [ind] = i;
      for (int j = 0; j <= ind; j++) Console.Write("{0, 2}", buf [j]);
      Console.WriteLine();
      rec(ind + 1, buf [ind] + 1);
    }
  }
}
```



## Clojure


```Clojure
(use '[clojure.math.combinatorics :only [subsets] ])

(def S #{1 2 3 4})

user> (subsets S)
(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4) (1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 3 4))
```


'''Alternate solution''', with no dependency on third-party library:

```Clojure
(defn powerset [coll]
  (reduce (fn [a x]
            (into a (map #(conj % x)) a))
          #{#{}} coll))

(powerset #{1 2 3})
```


```Clojure
#{#{} #{1} #{2} #{1 2} #{3} #{1 3} #{2 3} #{1 2 3}}
```



## CoffeeScript


```coffeescript

print_power_set = (arr) ->
  console.log "POWER SET of #{arr}"
  for subset in power_set(arr)
    console.log subset

power_set = (arr) ->
  result = []
  binary = (false for elem in arr)
  n = arr.length
  while binary.length <= n
    result.push bin_to_arr binary, arr
    i = 0
    while true
      if binary[i]
        binary[i] = false
        i += 1
      else
        binary[i] = true
        break
    binary[i] = true
  result

bin_to_arr = (binary, arr) ->
  (arr[i] for i of binary when binary[arr.length - i  - 1])

print_power_set []
print_power_set [4, 2, 1]
print_power_set ['dog', 'c', 'b', 'a']

```

{{out}}
<lang>
> coffee power_set.coffee
POWER SET of
[]
POWER SET of 4,2,1
[]
[ 1 ]
[ 2 ]
[ 2, 1 ]
[ 4 ]
[ 4, 1 ]
[ 4, 2 ]
[ 4, 2, 1 ]
POWER SET of dog,c,b,a
[]
[ 'a' ]
[ 'b' ]
[ 'b', 'a' ]
[ 'c' ]
[ 'c', 'a' ]
[ 'c', 'b' ]
[ 'c', 'b', 'a' ]
[ 'dog' ]
[ 'dog', 'a' ]
[ 'dog', 'b' ]
[ 'dog', 'b', 'a' ]
[ 'dog', 'c' ]
[ 'dog', 'c', 'a' ]
[ 'dog', 'c', 'b' ]
[ 'dog', 'c', 'b', 'a' ]

```



## ColdFusion


Port from the [[#JavaScript|JavaScript]] version,
compatible with ColdFusion 8+ or Railo 3+

```javascript
public array function powerset(required array data)
{
  var ps = [""];
  var d = arguments.data;
  var lenData = arrayLen(d);
  var lenPS = 0;
  for (var i=1; i LTE lenData; i++)
  {
    lenPS = arrayLen(ps);
    for (var j = 1; j LTE lenPS; j++)
    {
      arrayAppend(ps, listAppend(ps[j], d[i]));
    }
  }
  return ps;
}

var res = powerset([1,2,3,4]);
```


{{out}}

```txt
["","1","2","1,2","3","1,3","2,3","1,2,3","4","1,4","2,4","1,2,4","3,4","1,3,4","2,3,4","1,2,3,4"]
```



## Common Lisp


```lisp
(defun powerset (s)
  (if s (mapcan (lambda (x) (list (cons (car s) x) x))
                (powerset (cdr s)))
      '(())))
```


{{out}}
 > (powerset '(l i s p))
 ((L I S P) (I S P) (L S P) (S P) (L I P) (I P) (L P) (P) (L I S) (I S) (L S) (S) (L I) (I) (L) NIL)


```lisp
(defun power-set (s)
  (reduce #'(lambda (item ps)
              (append (mapcar #'(lambda (e) (cons item e))
                              ps)
                      ps))
          s
          :from-end t
          :initial-value '(())))
```

{{out}}
 >(power-set '(1 2 3))
 ((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) NIL)


Alternate, more recursive (same output):

```lisp
(defun powerset (l)
  (if (null l)
      (list nil)
      (let ((prev (powerset (cdr l))))
	(append (mapcar #'(lambda (elt) (cons (car l) elt)) prev)
		prev))))
```



Imperative-style using LOOP:

```lisp
(defun powerset (xs)
  (loop for i below (expt 2 (length xs)) collect
       (loop for j below i for x in xs if (logbitp j i) collect x)))
```

{{out}}
 >(powerset '(1 2 3)
 (NIL (1) (2) (1 2) (3) (1 3) (2 3) (1 2 3))

Yet another imperative solution, this time with dolist.

```lisp
(defun power-set (list)
    (let ((pow-set (list nil)))
      (dolist (element (reverse list) pow-set)
        (dolist (set pow-set)
          (push (cons element set) pow-set)))))
```

{{out}}
 >(power-set '(1 2 3))
 ((1) (1 3) (1 2 3) (1 2) (2) (2 3) (3) NIL)


## D

This implementation defines a range which *lazily* enumerates the power set.


```d
import std.algorithm;
import std.range;

auto powerSet(R)(R r)
{
	return
		(1L<<r.length)
		.iota
		.map!(i =>
			r.enumerate
			.filter!(t => (1<<t[0]) & i)
			.map!(t => t[1])
		);
}

unittest
{
	int[] emptyArr;
	assert(emptyArr.powerSet.equal!equal([emptyArr]));
	assert(emptyArr.powerSet.powerSet.equal!(equal!equal)([[], [emptyArr]]));
}

void main(string[] args)
{
	import std.stdio;
	args[1..$].powerSet.each!writeln;
}
```


An alternative version, which implements the range construct from scratch:


```d
import std.range;

struct PowerSet(R)
	if (isRandomAccessRange!R)
{
	R r;
	size_t position;

	struct PowerSetItem
	{
		R r;
		size_t position;

		private void advance()
		{
			while (!(position & 1))
			{
				r.popFront();
				position >>= 1;
			}
		}

		@property bool empty() { return position == 0; }
		@property auto front()
		{
			advance();
			return r.front;
		}
		void popFront()
		{
			advance();
			r.popFront();
			position >>= 1;
		}
	}

	@property bool empty() { return position == (1 << r.length); }
	@property PowerSetItem front() { return PowerSetItem(r.save, position); }
	void popFront() { position++; }
}

auto powerSet(R)(R r) { return PowerSet!R(r); }
```

{{out}}

```txt
$ rdmd powerset a b c
[]
["a"]
["b"]
["a", "b"]
["c"]
["a", "c"]
["b", "c"]
["a", "b", "c"]
```




###  Alternative: using folds


An almost verbatim translation of the Haskell code in D.

Since D doesn't foldr, I've also copied Haskell's foldr implementation here.

Main difference from the Haskell:
#It isn't lazy (but it could be made so by implementing this as a generator)

Main differences from the version above:
#It isn't lazy
#It doesn't rely on integer bit fiddling, so it should work on arrays larger than size_t.


```d

// Haskell definition:
// foldr f z []     = z
// foldr f z (x:xs) = x `f` foldr f z xs
S foldr(T, S)(S function(T, S) f, S z, T[] rest) {
    return (rest.length == 0) ? z : f(rest[0], foldr(f, z, rest[1..$]));
}

// Haskell definition:
//powerSet = foldr (\x acc -> acc ++ map (x:) acc) [[]]
T[][] powerset(T)(T[] set) {
    import std.algorithm;
    import std.array;
    // Note: The types before x and acc aren't needed, so this could be made even more concise, but I think it helps
    // to make the algorithm slightly clearer.
    return foldr( (T x, T[][] acc) => acc ~ acc.map!(accx => x ~ accx).array , [[]], set );
}

```


=={{header|Déjà Vu}}==

In Déjà Vu, sets are dictionaries with all values <code>true</code> and the default set to <code>false</code>.


```dejavu
powerset s:
	local :out [ set{ } ]
	for value in keys s:
		for subset in copy out:
			local :subset+1 copy subset
			set-to subset+1 value true
			push-to out subset+1
	out

!. powerset set{ 1 2 3 4 }
```


{{out}}

```txt
[ set{ } set{ 4 } set{ 3 4 } set{ 3 } set{ 2 3 } set{ 2 3 4 } set{ 2 4 } set{ 2 } set{ 1 2 } set{ 1 2 4 } set{ 1 2 3 4 } set{ 1 2 3 } set{ 1 3 } set{ 1 3 4 } set{ 1 4 } set{ 1 } ]
```



## E



```e
pragma.enable("accumulator")

def powerset(s) {
  return accum [].asSet() for k in 0..!2**s.size() {
    _.with(accum [].asSet() for i ? ((2**i & k) > 0) => elem in s {
      _.with(elem)
    })
  }
}
```


It would also be possible to define an object which is the powerset of a provided set without actually instantiating all of its members immediately. [[Category:E examples needing attention]]


## EchoLisp


```scheme

(define (set-cons a A)
    (make-set (cons a A)))

(define (power-set e)
    (cond ((null? e)
       (make-set (list ∅)))
    (else (let [(ps (power-set (cdr e)))]
       (make-set
       (append ps (map set-cons (circular-list (car e)) ps)))))))

(define B (make-set ' ( 🍎 🍇 🎂 🎄 )))
(power-set B)
    → { ∅ { 🍇 } { 🍇 🍎 } { 🍇 🍎 🎂 } { 🍇 🍎 🎂 🎄 } { 🍇 🍎 🎄 } { 🍇 🎂 } { 🍇 🎂 🎄 }
      { 🍇 🎄 } { 🍎 } { 🍎 🎂 } { 🍎 🎂 🎄 } { 🍎 🎄 } { 🎂 } { 🎂 🎄 } { 🎄 } }

;; The Von Neumann universe

(define V0 (power-set null)) ;; null and ∅ are the same
       → { ∅ }
(define V1 (power-set V0))
       → { ∅ { ∅ } }
(define V2 (power-set V1))
       → { ∅ { ∅ } { ∅ { ∅ } } { { ∅ } } }
(define V3 (power-set V2))
       → { ∅ { ∅ } { ∅ { ∅ } } …🔃 )
(length V3) → 16
(define V4 (power-set V3))
(length V4)  → 65536
;; length V5 = 2^65536 : out of bounds



```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  use Bitwise
  def powerset1(list) do
    n = length(list)
    max = round(:math.pow(2,n))
    for i <- 0..max-1, do: (for pos <- 0..n-1, band(i, bsl(1, pos)) != 0, do: Enum.at(list, pos) )
  end

  def powerset2([]), do: [[]]
  def powerset2([h|t]) do
    pt = powerset2(t)
    (for x <- pt, do: [h|x]) ++ pt
  end

  def powerset3([]), do: [[]]
  def powerset3([h|t]) do
    pt = powerset3(t)
    powerset3(h, pt, pt)
  end

  defp powerset3(_, [], acc), do: acc
  defp powerset3(x, [h|t], acc), do: powerset3(x, t, [[x|h] | acc])
end

IO.inspect RC.powerset1([1,2,3])
IO.inspect RC.powerset2([1,2,3])
IO.inspect RC.powerset3([1,2,3])
IO.inspect RC.powerset1([])
IO.inspect RC.powerset1(["one"])
```


{{out}}

```txt

[[], [1], [2], [1, 2], [3], [1, 3], [2, 3], [1, 2, 3]]
[[1, 2, 3], [1, 2], [1, 3], [1], [2, 3], [2], [3], []]
[[1], [1, 3], [1, 2, 3], [1, 2], [2], [2, 3], [3], []]
[[]]
[[], ["one"]]

```



## Erlang


Generates all subsets of a list with the help of binary:

```txt

For [1 2 3]:
    [     ] | 0 0 0 | 0
    [    3] | 0 0 1 | 1
    [  2  ] | 0 1 0 | 2
    [  2 3] | 0 1 1 | 3
    [1    ] | 1 0 0 | 4
    [1   3] | 1 0 1 | 5
    [1 2  ] | 1 1 0 | 6
    [1 2 3] | 1 1 1 | 7
    ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯

```


```erlang
powerset(Lst) ->
    N = length(Lst),
    Max = trunc(math:pow(2,N)),
    [[lists:nth(Pos+1,Lst) || Pos <- lists:seq(0,N-1), I band (1 bsl Pos) =/= 0]
      || I <- lists:seq(0,Max-1)].
```


{{out}}
<code>[[], [1], [2], [1,2], [3], [1,3], [2,3], [1,2,3], [4], [1,4], [2,4], [1,2,4], [3,4], [1,3,4], [2,3,4], [1,2,3,4]]</code>

Alternate shorter and more efficient version:

```erlang
powerset([]) -> [[]];
powerset([H|T]) -> PT = powerset(T),
  [ [H|X] || X <- PT ] ++ PT.
```


or even more efficient version:

```erlang
powerset([]) -> [[]];
powerset([H|T]) -> PT = powerset(T),
  powerset(H, PT, PT).

powerset(_, [], Acc) -> Acc;
powerset(X, [H|T], Acc) -> powerset(X, T, [[X|H]|Acc]).
```


=={{header|F Sharp|F#}}==
almost exact copy of OCaml version

```fsharp

let subsets xs = List.foldBack (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs [[]]

```


alternatively with list comprehension


```fsharp

let rec pow =
    function
    | [] -> [[]]
    | x::xs -> [for i in pow xs do yield! [i;x::i]]

```



## Factor

We use hash sets, denoted by <code>HS{ }</code> brackets, for our sets. <code>members</code> converts from a set to a sequence, and <code><hash-set></code> converts back.

```factor
USING: kernel prettyprint sequences arrays sets hash-sets ;
IN: powerset

: add ( set elt -- newset ) 1array <hash-set> union ;
: powerset ( set -- newset ) members { HS{ } } [ dupd [ add ] curry map append ] reduce <hash-set> ;
```

Usage:

```factor
( scratchpad ) HS{ 1 2 3 4 } powerset .
HS{
    HS{ 1 2 3 4 }
    HS{ 1 2 }
    HS{ 1 3 }
    HS{ 2 3 }
    HS{ 1 2 3 }
    HS{ 1 4 }
    HS{ 2 4 }
    HS{ }
    HS{ 1 }
    HS{ 2 }
    HS{ 3 }
    HS{ 4 }
    HS{ 1 2 4 }
    HS{ 3 4 }
    HS{ 1 3 4 }
    HS{ 2 3 4 }
}
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Power_set this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

{{works with|4tH|3.61.0}}.
{{trans|C}}

```forth
: ?print dup 1 and if over args type space then ;
: .set begin dup while ?print >r 1+ r> 1 rshift repeat drop drop ;
: .powerset 0 do ." ( " 1 i .set ." )" cr loop ;
: check-none dup 2 < abort" Usage: powerset [val] .. [val]" ;
: check-size dup /cell 8 [*] >= abort" Set too large" ;
: powerset 1 argn check-none check-size 1- lshift .powerset ;

powerset
```

{{out}}

```txt

$ 4th cxq powerset.4th 1 2 3 4
( )
( 1 )
( 2 )
( 1 2 )
( 3 )
( 1 3 )
( 2 3 )
( 1 2 3 )
( 4 )
( 1 4 )
( 2 4 )
( 1 2 4 )
( 3 4 )
( 1 3 4 )
( 2 3 4 )
( 1 2 3 4 )

```



## Frink

Frink's set and array classes have built-in subsets[] methods that return all subsets.  If called with an array, the results are arrays.  If called with a set, the results are sets.

```frink

a = new set[1,2,3,4]
a.subsets[]

```



## FunL

FunL uses Scala type <code>scala.collection.immutable.Set</code> as it's set type, which has a built-in method <code>subsets</code> returning an (Scala) iterator over subsets.


```funl
def powerset( s ) = s.subsets().toSet()
```


The powerset function could be implemented in FunL directly as:


```funl
def
  powerset( {} ) = {{}}
  powerset( s ) =
    acc = powerset( s.tail() )
    acc + map( x -> {s.head()} + x, acc )
```


or, alternatively as:

```funl
import lists.foldr

def powerset( s ) = foldr( \x, acc -> acc + map( a -> {x} + a, acc), {{}}, s )

println( powerset({1, 2, 3, 4}) )
```


{{out}}


```txt

{{}, {4}, {1, 2}, {1, 3}, {2, 3, 4}, {3}, {1, 2, 3, 4}, {1, 4}, {1, 2, 3}, {2}, {1, 2, 4}, {1}, {3, 4}, {2, 3}, {2, 4}, {1, 3, 4}}

```



## GAP


```gap
# Built-in
Combinations([1, 2, 3]);
# [ [  ], [ 1 ], [ 1, 2 ], [ 1, 2, 3 ], [ 1, 3 ], [ 2 ], [ 2, 3 ], [ 3 ] ]

# Note that it handles duplicates
Combinations([1, 2, 3, 1]);
# [ [  ], [ 1 ], [ 1, 1 ], [ 1, 1, 2 ], [ 1, 1, 2, 3 ], [ 1, 1, 3 ], [ 1, 2 ], [ 1, 2, 3 ], [ 1, 3 ],
#   [ 2 ], [ 2, 3 ], [ 3 ] ]
```



## Go

No native set type in Go.  While the associative array trick mentioned in the task description works well in Go in most situations, it does not work here because we need sets of sets, and converting a general set to a hashable value for a map key is non-trivial.

Instead, this solution uses a simple (non-associative) slice as a set representation.  To ensure uniqueness, the element interface requires an equality method, which is used by the
set add method.  Adding elements with the add method ensures the uniqueness property.

While the "add" and "has" methods make a usable set type, the power set method implemented here computes a result directly without using the add method.  The algorithm ensures that the result will be a valid set as long as the input is a valid set.  This allows the more efficient append function to be used.

```go
package main

import (
    "fmt"
    "strconv"
    "strings"
)

// types needed to implement general purpose sets are element and set

// element is an interface, allowing different kinds of elements to be
// implemented and stored in sets.
type elem interface {
    // an element must be distinguishable from other elements to satisfy
    // the mathematical definition of a set.  a.eq(b) must give the same
    // result as b.eq(a).
    Eq(elem) bool
    // String result is used only for printable output.  Given a, b where
    // a.eq(b), it is not required that a.String() == b.String().
    fmt.Stringer
}

// integer type satisfying element interface
type Int int

func (i Int) Eq(e elem) bool {
    j, ok := e.(Int)
    return ok && i == j
}

func (i Int) String() string {
    return strconv.Itoa(int(i))
}

// a set is a slice of elem's.  methods are added to implement
// the element interface, to allow nesting.
type set []elem

// uniqueness of elements can be ensured by using add method
func (s *set) add(e elem) {
    if !s.has(e) {
        *s = append(*s, e)
    }
}

func (s *set) has(e elem) bool {
    for _, ex := range *s {
        if e.Eq(ex) {
            return true
        }
    }
    return false
}

func (s set) ok() bool {
    for i, e0 := range s {
        for _, e1 := range s[i+1:] {
            if e0.Eq(e1) {
                return false
            }
        }
    }
    return true
}

// elem.Eq
func (s set) Eq(e elem) bool {
    t, ok := e.(set)
    if !ok {
        return false
    }
    if len(s) != len(t) {
        return false
    }
    for _, se := range s {
        if !t.has(se) {
            return false
        }
    }
    return true
}

// elem.String
func (s set) String() string {
    if len(s) == 0 {
        return "∅"
    }
    var buf strings.Builder
    buf.WriteRune('{')
    for i, e := range s {
        if i > 0 {
            buf.WriteRune(',')
        }
        buf.WriteString(e.String())
    }
    buf.WriteRune('}')
    return buf.String()
}

// method required for task
func (s set) powerSet() set {
    r := set{set{}}
    for _, es := range s {
        var u set
        for _, er := range r {
            er := er.(set)
            u = append(u, append(er[:len(er):len(er)], es))
        }
        r = append(r, u...)
    }
    return r
}

func main() {
    var s set
    for _, i := range []Int{1, 2, 2, 3, 4, 4, 4} {
        s.add(i)
    }
    fmt.Println("      s:", s, "length:", len(s))
    ps := s.powerSet()
    fmt.Println("   𝑷(s):", ps, "length:", len(ps))

    fmt.Println("\n(extra credit)")
    var empty set
    fmt.Println("  empty:", empty, "len:", len(empty))
    ps = empty.powerSet()
    fmt.Println("   𝑷(∅):", ps, "len:", len(ps))
    ps = ps.powerSet()
    fmt.Println("𝑷(𝑷(∅)):", ps, "len:", len(ps))

    fmt.Println("\n(regression test for earlier bug)")
    s = set{Int(1), Int(2), Int(3), Int(4), Int(5)}
    fmt.Println("      s:", s, "length:", len(s), "ok:", s.ok())
    ps = s.powerSet()
    fmt.Println("   𝑷(s):", "length:", len(ps), "ok:", ps.ok())
    for _, e := range ps {
        if !e.(set).ok() {
            panic("invalid set in ps")
        }
    }
}
```

{{out}}

```txt

      s: {1,2,3,4} length: 4
   𝑷(s): {∅,{1},{2},{1,2},{3},{1,3},{2,3},{1,2,3},{4},{1,4},{2,4},{1,2,4},{3,4},{1,3,4},{2,3,4},{1,2,3,4}} length: 16

(extra credit)
  empty: ∅ len: 0
   𝑷(∅): {∅} len: 1
𝑷(𝑷(∅)): {∅,{∅}} len: 2

(regression test for earlier bug)
      s: {1,2,3,4,5} length: 5 ok: true
   𝑷(s): length: 32 ok: true

```



## Groovy

Builds on the [[Combinations#Groovy|Combinations]] solution. '''Sets''' are not a "natural" collection type in Groovy. '''Lists''' are much more richly supported. Thus, this solution is liberally sprinkled with coercion from '''Set''' to '''List''' and from '''List''' to '''Set'''.

```groovy
def comb
comb = { m, List list ->
    def n = list.size()
    m == 0 ?
        [[]] :
        (0..(n-m)).inject([]) { newlist, k ->
            def sublist = (k+1 == n) ? [] : list[(k+1)..<n]
            newlist += comb(m-1, sublist).collect { [list[k]] + it }
        }
}

def powerSet = { set ->
    (0..(set.size())).inject([]){ list, i ->  list + comb(i,set as List)}.collect { it as LinkedHashSet } as LinkedHashSet
}
```


Test program:

```groovy
def vocalists = [ "C", "S", "N", "Y" ] as LinkedHashSet
println "${vocalists}"
println powerSet(vocalists)
```


{{out}}

```txt
[C, S, N, Y]
[[], [C], [S], [N], [Y], [C, S], [C, N], [C, Y], [S, N], [S, Y], [N, Y], [C, S, N], [C, S, Y], [C, N, Y], [S, N, Y], [C, S, N, Y]]
```


Note: In this example, '''LinkedHashSet''' was used throughout for '''Set''' coercion. This is because '''LinkedHashSet''' preserves the order of input, like a '''List'''. However, if order does not matter you could replace all references to '''LinkedHashSet''' with '''Set'''.


## Haskell


```Haskell
import Data.Set
import Control.Monad

powerset :: Ord a => Set a -> Set (Set a)
powerset = fromList . fmap fromList . listPowerset . toList

listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])
```

<tt>listPowerset</tt> describes the result as all possible (using the list monad) filterings (using <tt>filterM</tt>) of the input list, regardless (using <tt>const</tt>) of each item's value. <tt>powerset</tt> simply converts the input and output from lists to sets.

'''Alternate Solution'''

```Haskell
powerset [] = [[]]
powerset (head:tail) = acc ++ map (head:) acc where acc = powerset tail
```

or

```Haskell
powerSet :: [a] -> [[a]]
powerSet = foldr (\x acc -> acc ++ map (x:) acc) [[]]
```


which could also be understood, in point-free terms, as:

```haskell
powerSet :: [a] -> [[a]]
powerSet = foldr ((mappend <*>) . fmap . (:)) (pure [])
```


Examples:

 *Main> listPowerset [1,2,3]
 [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
 *Main> powerset (Data.Set.fromList [1,2,3])
 {{},{1},{1,2},{1,2,3},{1,3},{2},{2,3},{3}}

{{works with|GHC|6.10}}
 Prelude> import Data.List
 Prelude Data.List> subsequences [1,2,3]
 [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

'''Alternate solution'''

A method using only set operations and set mapping is also possible. Ideally, <code>Set</code> would be defined as a Monad, but that's impossible given the constraint that the type of inputs to Set.map (and a few other functions) be ordered.

```Haskell
import qualified Data.Set as Set
type Set=Set.Set
unionAll :: (Ord a) => Set (Set a) -> Set a
unionAll = Set.fold Set.union Set.empty

--slift is the analogue of liftA2 for sets.
slift :: (Ord a, Ord b, Ord c) => (a->b->c) -> Set a -> Set b -> Set c
slift f s0 s1 = unionAll (Set.map (\e->Set.map (f e) s1) s0)

--a -> {{},{a}}
makeSet :: (Ord a) => a -> Set (Set a)
makeSet = (Set.insert Set.empty) . Set.singleton.Set.singleton

powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet = (Set.fold (slift Set.union) (Set.singleton Set.empty)) . Set.map makeSet
```

Usage:

```Haskell

Prelude Data.Set> powerSet fromList [1,2,3]
fromList [fromList [], fromList [1], fromList [1,2], fromList [1,2,3], fromList [1,3], fromList [2], fromList [2,3], fromList [3]]
```


=={{header|Icon}} and {{header|Unicon}}==

The two examples below show the similarities and differences between constructing an explicit representation of the solution, i.e. a set containing the powerset, and one using generators.  The basic recursive algorithm is the same in each case, but wherever the first stores part of the result away, the second uses 'suspend' to immediately pass the result back to the caller.  The caller may then decide to store the results in a set, a list, or dispose of each one as it appears.


### Set building


The following version returns a set containing the powerset:


```Icon

procedure power_set (s)
  result := set ()
  if *s = 0
    then insert (result, set ()) # empty set
    else {
      head := set(?s) # take a random element
      # and find powerset of remaining part of set
      tail_pset := power_set (x -- head)
      result ++:= tail_pset # add powerset of remainder to results
      every ps := !tail_pset do # and add head to each powerset from the remainder
        insert (result, ps ++ head)
    }
  return result
end

```


To test the above procedure:


```Icon

procedure main ()
  every s := !power_set (set(1,2,3,4)) do { # requires '!' to generate items in the result set
    writes ("[ ")
    every writes (!s || " ")
    write ("]")
  }
end

```


{{out}}

```txt

[ 3 ]
[ 4 3 ]
[ 2 4 ]
[ 2 3 ]
[ 1 3 ]
[ 4 ]
[ 2 ]
[ 2 1 3 ]
[ 2 4 1 ]
[ 4 1 3 ]
[ 2 4 1 3 ]
[ ]
[ 2 4 3 ]
[ 1 ]
[ 4 1 ]
[ 2 1 ]

```



### Generator


An alternative version, which generates each item in the power set in turn:


```Icon

procedure power_set (s)
  if *s = 0
    then suspend set ()
    else {
      head := set(?s)
      every ps := power_set (s -- head) do {
        suspend ps
        suspend ps ++ head
      }
    }
end

procedure main ()
  every s := power_set (set(1,2,3,4)) do { # power_set's values are generated by 'every'
    writes ("[ ")
    every writes (!s || " ")
    write ("]")
  }
end

```



## J


There are a [http://www.jsoftware.com/jwiki/Essays/Power_Set number of ways] to generate a power set in J.  Here's one:

```j
ps =: #~ 2 #:@i.@^ #
```

For example:

```j
   ps 'ACE'

E
C
CE
A
AE
AC
ACE
```


In the typical use, this operation makes sense on collections of unique elements.


```J
   ~.1 2 3 2 1
1 2 3
   #ps 1 2 3 2 1
32
   #ps ~.1 2 3 2 1
8
```


In other words, the power set of a 5 element set has 32 sets where the power set of a 3 element set has 8 sets.  Thus if elements of the original "set" were not unique then sets of the power "set" will also not be unique sets.


## Java

{{works with|Java|1.5+}}

### Recursion

[[Category:Recursion]]
This implementation sorts each subset, but not the whole list of subsets (which would require a custom comparator). It also destroys the original set.

```java5>public static ArrayList<String
 getpowerset(int a[],int n,ArrayList<String> ps)
    {
        if(n<0)
        {
            return null;
        }
        if(n==0)
        {
            if(ps==null)
                ps=new ArrayList<String>();
            ps.add(" ");
            return ps;
        }
        ps=getpowerset(a, n-1, ps);
        ArrayList<String> tmp=new ArrayList<String>();
        for(String s:ps)
        {
            if(s.equals(" "))
                tmp.add(""+a[n-1]);
            else
                tmp.add(s+a[n-1]);
        }
        ps.addAll(tmp);
        return ps;
    }
```



### Iterative

The iterative implementation of the above idea. Each subset is in the order that the element appears in the input list. This implementation preserves the input.

```java5

public static <T> List<List<T>> powerset(Collection<T> list) {
  List<List<T>> ps = new ArrayList<List<T>>();
  ps.add(new ArrayList<T>());   // add the empty set

  // for every item in the original list
  for (T item : list) {
    List<List<T>> newPs = new ArrayList<List<T>>();

    for (List<T> subset : ps) {
      // copy all of the current powerset's subsets
      newPs.add(subset);

      // plus the subsets appended with the current item
      List<T> newSubset = new ArrayList<T>(subset);
      newSubset.add(item);
      newPs.add(newSubset);
    }

    // powerset is now powerset of list.subList(0, list.indexOf(item)+1)
    ps = newPs;
  }
  return ps;
}

```



### Binary String

This implementation works on idea that each element in the original set can either be in the power set or not in it. With <tt>n</tt> elements in the original set, each combination can be represented by a binary string of length <tt>n</tt>. To get all possible combinations, all you need is a counter from 0 to 2<sup>n</sup> - 1. If the k<sup>th</sup> bit in the binary string is 1, the k<sup>th</sup> element of the original set is in this combination.

```java5>public static <T extends Comparable<? super T>> LinkedList<LinkedList<T>
 BinPowSet(
		LinkedList<T> A){
	LinkedList<LinkedList<T>> ans= new LinkedList<LinkedList<T>>();
	int ansSize = (int)Math.pow(2, A.size());
	for(int i= 0;i< ansSize;++i){
		String bin= Integer.toBinaryString(i); //convert to binary
		while(bin.length() < A.size()) bin = "0" + bin; //pad with 0's
		LinkedList<T> thisComb = new LinkedList<T>(); //place to put one combination
		for(int j= 0;j< A.size();++j){
			if(bin.charAt(j) == '1')thisComb.add(A.get(j));
		}
		Collections.sort(thisComb); //sort it for easy checking
		ans.add(thisComb); //put this set in the answer list
	}
	return ans;
}
```



## JavaScript



### ES5


### =Iteration=

Uses a JSON stringifier from http://www.json.org/js.html

{{works with|SpiderMonkey}}

```javascript
function powerset(ary) {
    var ps = [[]];
    for (var i=0; i < ary.length; i++) {
        for (var j = 0, len = ps.length; j < len; j++) {
            ps.push(ps[j].concat(ary[i]));
        }
    }
    return ps;
}

var res = powerset([1,2,3,4]);

load('json2.js');
print(JSON.stringify(res));
```


{{out}}

```txt
[[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3],[4],[1,4],[2,4],[1,2,4],[3,4],[1,3,4],[2,3,4],[1,2,3,4]]
```




### =Functional composition=


{{trans|Haskell}}


```JavaScript
(function () {

   // translating:  powerset = foldr (\x acc -> acc ++ map (x:) acc) [[]]

    function powerset(xs) {
        return xs.reduceRight(function (a, x) {
            return a.concat(a.map(function (y) {
                return [x].concat(y);
            }));
        }, [[]]);
    }


    // TEST
    return {
        '[1,2,3] ->': powerset([1, 2, 3]),
        'empty set ->': powerset([]),
        'set which contains only the empty set ->': powerset([[]])
    }

})();
```


{{Out}}


```JavaScript
{
 "[1,2,3] ->":[[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]],
 "empty set ->":[[]],
 "set which contains only the empty set ->":[[], [[]]]
}
```



### ES6



```JavaScript
(() => {
    'use strict';

    // powerset :: [a] -> [[a]]
    const powerset = xs =>
        xs.reduceRight((a, x) => [...a, ...a.map(y => [x, ...y])], [
            []
        ]);


    // TEST
    return {
        '[1,2,3] ->': powerset([1, 2, 3]),
        'empty set ->': powerset([]),
        'set which contains only the empty set ->': powerset([
            []
        ])
    };
})()
```


{{Out}}

```JavaScript
{"[1,2,3] ->":[[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]],
"empty set ->":[[]],
"set which contains only the empty set ->":[[], [[]]]}
```



## jq


```jq
def powerset:
  reduce .[] as $i ([[]];
     reduce .[] as $r (.; . + [$r + [$i]]));
```

Example:
 [range(0;10)]|powerset|length
 # => 1024

Extra credit:

```jq

# The power set of the empty set:
  [] | powerset
  # => [[]]

# The power set of the set which contains only the empty set:
  [ [] ] | powerset
  # => [[],[[]]]
```


### =Recursive version=


```jq
def powerset:
  if length == 0 then [[]]
  else .[0] as $first
    | (.[1:] | powerset)
    | map([$first] + . ) + .
  end;
```

Example:
 [1,2,3]|powerset
 # => [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]


## Julia


```julia

function powerset{T}(x::Vector{T})
    result = Vector{T}[[]]
    for elem in x, j in eachindex(result)
        push!(result, [result[j] ; elem])
    end
    result
end

```

{{Out}}

```txt

julia> show(powerset([1,2,3]))
[Int64[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

```



## K


```K

   ps:{x@&:'+2_vs!_2^#x}

```

Usage:

```K

   ps "ABC"
(""
 ,"C"
 ,"B"
 "BC"
 ,"A"
 "AC"
 "AB"
 "ABC")

```



## Kotlin


```scala
// version 1.1.3

class PowerSet<T>(val items: List<T>) {
    private lateinit var combination: IntArray

    init {
        println("Power set of $items comprises:")
        for (m in 0..items.size) {
            combination = IntArray(m)
            generate(0, m)
        }
    }

    private fun generate(k: Int, m: Int) {
        if (k >= m) {
            println(combination.map { items[it] })
        }
        else {
            for (j in 0 until items.size)
                if (k == 0 || j > combination[k - 1]) {
                    combination[k] = j
                    generate(k + 1, m)
                }
        }
    }
}

fun main(args: Array<String>) {
    val itemsList = listOf(
        listOf(1, 2, 3, 4),
        emptyList<Int>(),
        listOf(emptyList<Int>())
    )
    for (items in itemsList) {
        PowerSet(items)
        println()
    }
}
```


{{out}}

```txt

Power set of [1, 2, 3, 4] comprises:
[]
[1]
[2]
[3]
[4]
[1, 2]
[1, 3]
[1, 4]
[2, 3]
[2, 4]
[3, 4]
[1, 2, 3]
[1, 2, 4]
[1, 3, 4]
[2, 3, 4]
[1, 2, 3, 4]

Power set of [] comprises:
[]

Power set of [[]] comprises:
[]
[[]]

```



## Logo


```logo
to powerset :set
  if empty? :set [output [[]]]
  localmake "rest powerset butfirst :set
  output sentence  map [sentence first :set ?] :rest  :rest
end

show powerset [1 2 3]
[[1 2 3] [1 2] [1 3] [1] [2 3] [2] [3] []]
```



## Logtalk


```logtalk
:- object(set).

    :- public(powerset/2).

    powerset(Set, PowerSet) :-
        reverse(Set, RSet),
        powerset_1(RSet, [[]], PowerSet).

    powerset_1([], PowerSet, PowerSet).
    powerset_1([X| Xs], Yss0, Yss) :-
        powerset_2(Yss0, X, Yss1),
        powerset_1(Xs, Yss1, Yss).

    powerset_2([], _, []).
    powerset_2([Zs| Zss], X, [Zs, [X| Zs]| Yss]) :-
        powerset_2(Zss, X, Yss).

    reverse(List, Reversed) :-
        reverse(List, [], Reversed).

    reverse([], Reversed, Reversed).
    reverse([Head| Tail], List, Reversed) :-
        reverse(Tail, [Head| List], Reversed).

:- end_object.
```

Usage example:

```logtalk
| ?- set::powerset([1, 2, 3, 4], PowerSet).

PowerSet = [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3],[4],[1,4],[2,4],[1,2,4],[3,4],[1,3,4],[2,3,4],[1,2,3,4]]
yes
```



## Lua


```lua

--returns the powerset of s, out of order.
function powerset(s, start)
  start = start or 1
  if(start > #s) then return {{}} end
  local ret = powerset(s, start + 1)
  for i = 1, #ret do
    ret[#ret + 1] = {s[start], unpack(ret[i])}
  end
  return ret
end

--non-recurse implementation
function powerset(s)
   local t = {{}}
   for i = 1, #s do
      for j = 1, #t do
         t[#t+1] = {s[i],unpack(t[j])}
      end
   end
   return t
end

--alternative, copied from the Python implementation
function powerset2(s)
  local ret = {{}}
  for i = 1, #s do
    local k = #ret
    for j = 1, k do
      ret[k + j] = {s[i], unpack(ret[j])}
    end
  end
  return ret
end

```



## M4


```M4
define(`for',
  `ifelse($#, 0, ``$0'',
          eval($2 <= $3), 1,
          `pushdef(`$1', `$2')$4`'popdef(
             `$1')$0(`$1', incr($2), $3, `$4')')')dnl
define(`nth',
  `ifelse($1, 1, $2,
          `nth(decr($1), shift(shift($@)))')')dnl
define(`range',
  `for(`x', eval($1 + 2), eval($2 + 2),
       `nth(x, $@)`'ifelse(x, eval($2+2), `', `,')')')dnl
define(`powerpart',
  `{range(2, incr($1), $@)}`'ifelse(incr($1), $#, `',
     `for(`x', eval($1+2), $#,
        `,powerpart(incr($1), ifelse(
           eval(2 <= ($1 + 1)), 1,
           `range(2,incr($1), $@), ')`'nth(x, $@)`'ifelse(
              eval((x + 1) <= $#),1,`,range(incr(x), $#, $@)'))')')')dnl
define(`powerset',
  `{powerpart(0, substr(`$1', 1, eval(len(`$1') - 2)))}')dnl
dnl
powerset(`{a,b,c}')
```


{{out}}

```txt

{{},{a},{a,b},{a,b,c},{a,c},{b},{b,c},{c}}

```




## Maple


```Maple

combinat:-powerset({1,2,3,4});

```

{{out}}

```txt

{{}, {1}, {2}, {3}, {4}, {1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4}, {3, 4},

    {1, 2, 3}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}, {1, 2, 3, 4}}

```



## Mathematica

Built-in function that either gives all possible subsets,
subsets with at most n elements, subsets with exactly n elements
or subsets containing between n and m elements.
Example of all subsets:

```Mathematica
Subsets[{a, b, c}]
```

gives:

```Mathematica
{{}, {a}, {b}, {c}, {a, b}, {a, c}, {b, c}, {a, b, c}}
```

Subsets[list, {n, Infinity}] gives all the subsets that have n elements or more.

Subsets[list, n] gives all the subsets that have at most n elements.

Subsets[list, {n}] gives all the subsets that have exactly n elements.

Subsets[list, {m,n}] gives all the subsets that have between m and n elements.


## MATLAB


Sets are not an explicit data type in MATLAB, but cell arrays can be used for the same purpose. In fact, cell arrays have the benefit of containing any kind of data structure. So, this powerset function will work on a set of any type of data structure, without the need to overload any operators.


```MATLAB
function pset = powerset(theSet)

    pset = cell(size(theSet)); %Preallocate memory

    %Generate all numbers from 0 to 2^(num elements of the set)-1
    for i = ( 0:(2^numel(theSet))-1 )

        %Convert i into binary, convert each digit in binary to a boolean
        %and store that array of booleans
        indicies = logical(bitget( i,(1:numel(theSet)) ));

        %Use the array of booleans to extract the members of the original
        %set, and store the set containing these members in the powerset
        pset(i+1) = {theSet(indicies)};

    end

end
```


Sample Usage:
Powerset of the set of the empty set.

```MATLAB
powerset({{}})

ans =

     {}    {1x1 cell} %This is the same as { {},{{}} }
```


Powerset of { {1,2},3 }.

```MATLAB
powerset({{1,2},3})

ans =

    {1x0 cell}    {1x1 cell}    {1x1 cell}    {1x2 cell} %This is the same as { {},{{1,2}},{3},{{1,2},3} }
```



## Maxima


```maxima
powerset({1, 2, 3, 4});
/* {{}, {1}, {1, 2}, {1, 2, 3}, {1, 2, 3, 4}, {1, 2, 4}, {1, 3}, {1, 3, 4},
   {1, 4}, {2}, {2, 3}, {2, 3, 4}, {2, 4}, {3}, {3, 4}, {4}} */
```



## Nim


```nim
import sets, hashes

proc hash(x: HashSet[int]): Hash =
  var h = 0
  for i in x: h = h !& hash(i)
  result = !$h

proc powerset[T](inset: HashSet[T]): auto =
  result = toSet([initSet[T]()])

  for i in inset:
    var tmp = result
    for j in result:
      var k = j
      k.incl(i)
      tmp.incl(k)
    result = tmp

echo powerset(toSet([1,2,3,4]))
```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h


+ (NSArray *)powerSetForArray:(NSArray *)array {
	UInt32 subsetCount = 1 << array.count;
	NSMutableArray *subsets = [NSMutableArray arrayWithCapacity:subsetCount];
	for(int subsetIndex = 0; subsetIndex < subsetCount; subsetIndex++) {
		NSMutableArray *subset = [[NSMutableArray alloc] init];
		for (int itemIndex = 0; itemIndex < array.count; itemIndex++) {
			if((subsetIndex >> itemIndex) & 0x1) {
				[subset addObject:array[itemIndex]];
			}
		}
		[subsets addObject:subset];
	}
	return subsets;
}
```



## OCaml


The standard library already implements a proper ''Set'' datatype. As the base type is unspecified, the powerset must be parameterized as a module. Also, the library is lacking a ''map'' operation, which we have to implement first.


```ocaml
module PowerSet(S: Set.S) =
struct

  include Set.Make (S)

  let map f s =
    let work x r = add (f x) r in
    fold work s empty
  ;;

  let powerset s =
    let base = singleton (S.empty) in
    let work x r = union r (map (S.add x) r) in
    S.fold work s base
  ;;

end;; (* PowerSet *)
```


version for lists:

```ocaml
let subsets xs = List.fold_right (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs [[]]
```




## OPL



```OPL

{string} s={"A","B","C","D"};
range r=1.. ftoi(pow(2,card(s)));
{string} s2 [k in r] = {i | i in s: ((k div (ftoi(pow(2,(ord(s,i))))) mod 2) == 1)};

execute
{
 writeln(s2);
}

```


which gives


```result


[{} {"A"} {"B"} {"A" "B"} {"C"} {"A" "C"} {"B" "C"} {"A" "B" "C"} {"D"} {"A"
         "D"} {"B" "D"} {"A" "B" "D"} {"C" "D"} {"A" "C" "D"} {"B" "C" "D"}
         {"A" "B" "C" "D"}]

```








## Oz

Oz has a library for finite set constraints. Creating a power set is a trivial application of that:

```oz
declare
  %% Given a set as a list, returns its powerset (again as a list)
  fun {Powerset Set}
     proc {Describe Root}
        %% Describe sets by lower bound (nil) and upper bound (Set)
        Root = {FS.var.bounds nil Set}
        %% enumerate all possible sets
        {FS.distribute naive [Root]}
     end
     AllSets = {SearchAll Describe}
  in
     %% convert to list representation
     {Map AllSets FS.reflect.lowerBoundList}
  end
in
  {Inspect {Powerset [1 2 3 4]}}
```


A more convential implementation without finite set constaints:

```oz
fun {Powerset2 Set}
   case Set of nil then [nil]
   [] H|T thens
      Acc = {Powerset2 T}
   in
      {Append Acc {Map Acc fun {$ A} H|A end}}
   end
end
```



## PARI/GP


```parigp
vector(1<<#S,i,vecextract(S,i-1))
```


{{works with|PARI/GP|2.10.0+}}
The <code>forsubset</code> iterator was added in version 2.10.0 to efficiently iterate over combinations and power sets.

```parigp
S=["a","b","c"]
forsubset(#S,s,print1(vecextract(S,s)"  "))
```

{{out}}

```txt
[]  ["a"]  ["b"]  ["c"]  ["a", "b"]  ["a", "c"]  ["b", "c"]  ["a", "b", "c"]
```



## Perl


Perl does not have a built-in set data-type. However, you can...

=== Module: [https://metacpan.org/pod/Algorithm::Combinatorics Algorithm::Combinatorics] ===

This module has an iterator over the power set.  Note that it does not enforce that the input array is a set (no duplication).  If each subset is processed immediately, this has an advantage of very low memory use.

```perl
use Algorithm::Combinatorics "subsets";
my @S = ("a","b","c");
my @PS;
my $iter = subsets(\@S);
while (my $p = $iter->next) {
  push @PS, "[@$p]"
}
say join("  ",@PS);
```

{{out}}

```txt
[a b c]  [b c]  [a c]  [c]  [a b]  [b]  [a]  []
```


=== Module: [https://metacpan.org/pod/ntheory ntheory] ===
{{libheader|ntheory}}
The simplest solution is to use the one argument version of the combination iterator, which iterates over the power set.

```perl
use ntheory "forcomb";
my @S = qw/a b c/;
forcomb { print "[@S[@_]]  " } scalar(@S);
print "\n";
```

{{out}}

```txt
[]  [a]  [b]  [c]  [a b]  [a c]  [b c]  [a b c]
```


Using the two argument version of the iterator gives a solution similar to the Perl6 and Python array versions.

```perl
use ntheory "forcomb";
my @S = qw/a b c/;
for $k (0..@S) {
  # Iterate over each $#S+1,$k combination.
  forcomb { print "[@S[@_]]  " } @S,$k;
}
print "\n";
```

{{out}}

```txt
[]  [a]  [b]  [c]  [a b]  [a c]  [b c]  [a b c]
```


Similar to the Pari/GP solution, one can also use <tt>vecextract</tt> with an integer mask to select elements.  Note that it does not enforce that the input array is a set (no duplication).  This also has low memory if each subset is processed immediately and the range is applied with a loop rather than a map.  A solution using <tt>vecreduce</tt> could be done identical to the array reduce solution shown later.

```perl
use ntheory "vecextract";
my @S = qw/a b c/;
my @PS = map { "[".join(" ",vecextract(\@S,$_))."]" } 0..2**scalar(@S)-1;
say join("  ",@PS);
```

{{out}}

```txt
[]  [a]  [b]  [a b]  [c]  [a c]  [b c]  [a b c]
```


=== Module: [https://metacpan.org/pod/Set::Object Set::Object] ===

The CPAN module [https://metacpan.org/pod/Set::Object Set::Object] provides a set implementation for sets of arbitrary objects, for which a powerset function could be defined and used like so:


```perl
use Set::Object qw(set);

sub powerset {
    my $p = Set::Object->new( set() );
    foreach my $i (shift->elements) {
        $p->insert( map { set($_->elements, $i) } $p->elements );
    }
    return $p;
}

my $set = set(1, 2, 3);
my $powerset = powerset($set);

print $powerset->as_string, "\n";
```


{{out}}

```txt
Set::Object(Set::Object() Set::Object(1 2 3) Set::Object(1 2) Set::Object(1 3) Set::Object(1) Set::Object(2 3) Set::Object(2) Set::Object(3))
```


=== Simple custom hash-based set type ===

It's also easy to define a custom type for sets of strings or numbers,
using a hash as the underlying representation (like the task description suggests):


```perl
package Set {
    sub new       { bless { map {$_ => undef} @_[1..$#_] }, shift; }
    sub elements  { sort keys %{shift()} }
    sub as_string { 'Set(' . join(' ', sort keys %{shift()}) . ')' }
    # ...more set methods could be defined here...
}
```


''(Note: For a ready-to-use module that uses this approach, and comes with all the standard set methods that you would expect, see the CPAN module [https://metacpan.org/pod/Set::Tiny Set::Tiny])''

The limitation of this approach is that only primitive strings/numbers are allowed as hash keys in Perl, so a Set of Set's cannot be represented, and the return value of our powerset function will thus have to be a ''list'' of sets rather than being a Set object itself.

We could implement the function as an imperative foreach loop similar to the <code>Set::Object</code> based solution above, but using list folding (with the help of Perl's <code>List::Util</code> core module) seems a little more elegant in this case:


```perl
use List::Util qw(reduce);

sub powerset {
    @{( reduce { [@$a, map { Set->new($_->elements, $b) } @$a ] }
               [Set->new()], shift->elements )};
}

my $set = Set->new(1, 2, 3);
my @subsets = powerset($set);

print $_->as_string, "\n" for @subsets;
```


{{out}}

```txt

Set()
Set(1)
Set(2)
Set(1 2)
Set(3)
Set(1 3)
Set(2 3)
Set(1 2 3)

```



###  Arrays


If you don't actually need a proper set data-type that guarantees uniqueness of its elements, the simplest approach is to use arrays to store "sets" of items, in which case the implementation of the powerset function becomes quite short.

Recursive solution:

```perl
sub powerset {
    @_ ? map { $_, [$_[0], @$_] } powerset(@_[1..$#_]) : [];
}
```


List folding solution:


```perl
use List::Util qw(reduce);

sub powerset {
    @{( reduce { [@$a, map([@$_, $b], @$a)] } [[]], @_ )}
}
```


Usage & output:

```perl
my @set = (1, 2, 3);
my @powerset = powerset(@set);

sub set_to_string {
    "{" . join(", ", map { ref $_ ? set_to_string(@$_) : $_ } @_) . "}"
}

print set_to_string(@powerset), "\n";
```

{{out}}

```txt

{{}, {1}, {2}, {1, 2}, {3}, {1, 3}, {2, 3}, {1, 2, 3}}

```



###  Lazy evaluation

If the initial set is quite large, constructing it's powerset all at once can consume lots of memory.

If you want to iterate through all of the elements of the powerset of a set, and don't mind each element being generated immediately before you process it, and being thrown away immediately after you're done with it, you can use vastly less memory.  This is similar to the earlier solutions using the Algorithm::Combinatorics and ntheory modules.

The following algorithm uses one bit of memory for every element of the original set (technically it uses several bytes per element with current versions of Perl).  This is essentially doing a <tt>vecextract</tt> operation by hand.


```perl
use strict;
use warnings;
sub powerset(&@) {
    my $callback = shift;
    my $bitmask = '';
    my $bytes = @_/8;
    {
       my @indices = grep vec($bitmask, $_, 1), 0..$#_;
       $callback->( @_[@indices] );
       ++vec($bitmask, $_, 8) and last for 0 .. $bytes;
       redo if @indices != @_;
    }
}

print "powerset of empty set:\n";
powerset { print "[@_]\n" };
print "powerset of set {1,2,3,4}:\n";
powerset { print "[@_]\n" } 1..4;
my $i = 0;
powerset { ++$i } 1..9;
print "The powerset of a nine element set contains $i elements.\n";

```

{{out}}

```txt
powerset of empty set:
[]
powerset of set {1,2,3,4}:
[]
[1]
[2]
[1 2]
[3]
[1 3]
[2 3]
[1 2 3]
[4]
[1 4]
[2 4]
[1 2 4]
[3 4]
[1 3 4]
[2 3 4]
[1 2 3 4]
The powerset of a nine element set contains 512 elements.


```

The technique shown above will work with arbitrarily large sets, and uses a trivial amount of memory.


## Perl 6

{{works with|rakudo|2014-02-25}}

```perl6
sub powerset(Set $s) { $s.combinations.map(*.Set).Set }
say powerset set <a b c d>;
```

{{out}}

```txt
set(set(), set(a), set(b), set(c), set(d), set(a, b), set(a, c), set(a, d), set(b, c), set(b, d), set(c, d), set(a, b, c), set(a, b, d), set(a, c, d), set(b, c, d), set(a, b, c, d))
```

If you don't care about the actual <tt>Set</tt> type, the <tt>.combinations</tt> method by itself may be good enough for you:

```perl6>.say for <a b c d>.combinations</lang

{{out}}

```txt

a
b
c
d
a b
a c
a d
b c
b d
c d
a b c
a b d
a c d
b c d
a b c d
```



## Phix


```Phix
sequence powerset
integer step = 1

function pst(object key, object /*data*/, object /*user_data*/)
    integer k = 1
    while k<length(powerset) do
        k += step
        for j=1 to step do
            powerset[k] = append(powerset[k],key)
            k += 1
        end for
    end while
    step *= 2
    return 1
end function

function power_set(integer d)
    powerset = repeat({},power(2,dict_size(d)))
    step = 1
    traverse_dict(routine_id("pst"),0,d)
    return powerset
end function

integer d1234 = new_dict()
setd(1,0,d1234)
setd(2,0,d1234)
setd(3,0,d1234)
setd(4,0,d1234)
?power_set(d1234)
integer d0 = new_dict()
?power_set(d0)
setd({},0,d0)
?power_set(d0)
```

{{out}}

```txt

{{},{1},{2},{1,2},{3},{1,3},{2,3},{1,2,3},{4},{1,4},{2,4},{1,2,4},{3,4},{1,3,4},{2,3,4},{1,2,3,4}}
{{}}
{{},{{}}}

```



## PHP


```PHP

<?php
function get_subset($binary, $arr) {
  // based on true/false values in $binary array, include/exclude
  // values from $arr
  $subset = array();
  foreach (range(0, count($arr)-1) as $i) {
    if ($binary[$i]) {
      $subset[] = $arr[count($arr) - $i - 1];
    }
  }
  return $subset;
}

function print_array($arr) {
  if (count($arr) > 0) {
    echo join(" ", $arr);
  } else {
    echo "(empty)";
  }
  echo '
';
}

function print_power_sets($arr) {
  echo "POWER SET of [" . join(", ", $arr) . "]
";
  foreach (power_set($arr) as $subset) {
    print_array($subset);
  }
}

function power_set($arr) {
  $binary = array();
  foreach (range(1, count($arr)) as $i) {
    $binary[] = false;
  }
  $n = count($arr);
  $powerset = array();

  while (count($binary) <= count($arr)) {
    $powerset[] = get_subset($binary, $arr);
    $i = 0;
    while (true) {
      if ($binary[$i]) {
        $binary[$i] = false;
        $i += 1;
      } else {
        $binary[$i] = true;
        break;
      }
    }
    $binary[$i] = true;
  }

  return $powerset;
}

print_power_sets(array());
print_power_sets(array('singleton'));
print_power_sets(array('dog', 'c', 'b', 'a'));
?>

```

{{out}}
<lang>
POWER SET of []
POWER SET of [singleton]
(empty)
singleton
POWER SET of [dog, c, b, a]
(empty)
a
b
a b
c
a c
b c
a b c
dog
a dog
b dog
a b dog
c dog
a c dog
b c dog
a b c dog

```



## PicoLisp


```PicoLisp
(de powerset (Lst)
   (ifn Lst
      (cons)
      (let L (powerset (cdr Lst))
         (conc
            (mapcar '((X) (cons (car Lst) X)) L)
            L ) ) ) )
```



## PL/I

{{trans|REXX}}

```pli
*process source attributes xref or(!);
 /*--------------------------------------------------------------------
 * 06.01.2014 Walter Pachl  translated from REXX
 *-------------------------------------------------------------------*/
 powerset: Proc Options(main);
 Dcl (hbound,index,left,substr) Builtin;
 Dcl sysprint Print;
 Dcl s(4) Char(5) Var Init('one','two','three','four');
 Dcl ps   Char(1000) Var;
 Dcl (n,chunk,p) Bin Fixed(31);
 n=hbound(s);                      /* number of items in the list.   */
 ps='{} ';                         /* start with a null power set.   */
 Do chunk=1 To n;                  /* loop through the ...     .     */
   ps=ps!!combn(chunk);            /* a CHUNK at a time.             */
   End;
 Do While(ps>'');
   p=index(ps,' ');
   Put Edit(left(ps,p-1))(Skip,a);
   ps=substr(ps,p+1);
   End;

 combn: Proc(y) Returns(Char(1000) Var);
 /*--------------------------------------------------------------------
 * returns the list of subsets with y elements of set s
 *-------------------------------------------------------------------*/
 Dcl (y,base,bbase,ym,p,j,d,u) Bin Fixed(31);
 Dcl (z,l) Char(1000) Var Init('');
 Dcl a(20) Bin Fixed(31) Init((20)0);
 Dcl i Bin Fixed(31);
 base=hbound(s)+1;
 bbase=base-y;
 ym=y-1;
 Do p=1 To y;
   a(p)=p;
   End;
 Do j=1 By 1;
   l='';
   Do d=1 To y;
     u=a(d);
     l=l!!','!!s(u);
     End;
   z=z!!'{'!!substr(l,2)!!'} ';
   a(y)=a(y)+1;
   If a(y)=base Then
     If combu(ym) Then
       Leave;
   End;
 /* Put Edit('combn',y,z)(Skip,a,f(2),x(1),a); */
 Return(z);

 combu: Proc(d) Recursive Returns(Bin Fixed(31));
 Dcl (d,u) Bin Fixed(31);
 If d=0 Then
   Return(1);
 p=a(d);
 Do u=d To y;
   a(u)=p+1;
   If a(u)=bbase+u Then
     Return(combu(u-1));
   p=a(u);
   End;
 Return(0);
 End;
 End;

 End;
```

{{out}}

```txt
{}
{one}
{two}
{three}
{four}
{one,two}
{one,three}
{one,four}
{two,three}
{two,four}
{three,four}
{one,two,three}
{one,two,four}
{one,three,four}
{two,three,four}
{one,two,three,four}
```



## PowerShell


```PowerShell

function power-set ($array) {
    if($array) {
        $n = $array.Count
        function state($set, $i){
            if($i -gt -1) {
                state $set ($i-1)
                state ($set+@($array[$i])) ($i-1)
            } else {
                "$($set | sort)"
            }
        }
        $set = state @() ($n-1)
        $power = 0..($set.Count-1) | foreach{@(0)}
        $i = 0
        $set | sort | foreach{$power[$i++] = $_.Split()}
        $power | sort {$_.Count}
    } else {@()}

}
$OFS = " "
$setA = power-set  @(1,2,3,4)
"number of sets in setA: $($setA.Count)"
"sets in setA:"
$OFS = ", "
$setA | foreach{"{"+"$_"+"}"}
$setB = @()
"number of sets in setB: $($setB.Count)"
"sets in setB:"
$setB | foreach{"{"+"$_"+"}"}
$setC = @(@(), @(@()))
"number of sets in setC: $($setC.Count)"
"sets in setC:"
$setC | foreach{"{"+"$_"+"}"}
$OFS = " "

```

<b>Output:</b>

```txt

number of sets in setA: 16
sets in setA:
{}
{1}
{2}
{3}
{4}
{1, 2}
{1, 3}
{1, 4}
{2, 3}
{2, 4}
{3, 4}
{1, 2, 3}
{1, 2, 4}
{1, 3, 4}
{2, 3, 4}
{1, 2, 3, 4}
number of sets in setB: 0
sets in setB:
number of sets in setC: 2
sets in setC:
{}
{}

```



## Prolog

===Logical (cut-free) Definition===

The predicate powerset(X,Y) defined here can be read as "Y is the
powerset of X", it being understood that lists are used to represent sets.

The predicate subseq(X,Y) is true if and only if the list X is a subsequence of the list Y.

The definitions here are elementary, logical (cut-free),
and efficient (within the class of comparably generic implementations).

```Prolog
powerset(X,Y) :- bagof( S, subseq(S,X), Y).

subseq( [], []).
subseq( [], [_|_]).
subseq( [X|Xs], [X|Ys] ) :- subseq(Xs, Ys).
subseq( [X|Xs], [_|Ys] ) :- append(_, [X|Zs], Ys), subseq(Xs, Zs).

```

{{out}}

```txt
?- powerset([1,2,3], X).
X = [[], [1], [1, 2], [1, 2, 3], [1, 3], [2], [2, 3], [3]].

% Symbolic:
?- powerset( [X,Y], S).
S = [[], [X], [X, Y], [Y]].

% In reverse:
?- powerset( [X,Y], [[], [1], [1, 2], [2]] ).
X = 1,
Y = 2.
```


===Single-Functor Definition===

```Prolog
power_set( [], [[]]).
power_set( [X|Xs], PS) :-
  power_set(Xs, PS1),
  maplist( append([X]), PS1, PS2 ), % i.e. prepend X to each PS1
  append(PS1, PS2, PS).
```

{{out}}

```txt
?- power_set([1,2,3,4,5,6,7,8], X), length(X,N), writeln(N).
256

```



### Constraint Handling Rules

CHR is a programming language created by '''Professor Thom Frühwirth'''.

Works with SWI-Prolog and module chr written by '''Tom Schrijvers''' and '''Jan Wielemaker'''.

```Prolog
:- use_module(library(chr)).

:- chr_constraint chr_power_set/2, chr_power_set/1, clean/0.

clean @ clean \ chr_power_set(_) <=> true.
clean @ clean <=> true.

only_one @ chr_power_set(A) \ chr_power_set(A) <=> true.


creation @ chr_power_set([H | T], A) <=>
           append(A, [H], B),
	   chr_power_set(T, A),
           chr_power_set(T, B),
	   chr_power_set(B).


empty_element @ chr_power_set([], _) <=> chr_power_set([]).

```

{{out}}

```txt
 ?- chr_power_set([1,2,3,4], []), findall(L, find_chr_constraint(chr_power_set(L)), LL), clean.
LL = [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,4],[1,3],[1,3,4],[1,4],[2],[2,3],[2,3,4],[2,4],[3],[3,4],[4],[]] .

```



## PureBasic

This code is for console mode.

```PureBasic
If OpenConsole()
  Define argc=CountProgramParameters()
  If argc>=(SizeOf(Integer)*8) Or argc<1
    PrintN("Set out of range.")
    End 1
  Else
    Define i, j, text$
    Define.q bset=1<<argc
    Print("{")
    For i=0 To bset-1   ; check all binary combinations
      If Not i: text$=  "{"
      Else    : text$=", {"
      EndIf
      k=0
      For j=0 To argc-1  ; step through each bit
        If i&(1<<j)
          If k: text$+", ": EndIf         ; pad the output
          text$+ProgramParameter(j): k+1  ; append each matching bit
        EndIf
      Next j
      Print(text$+"}")
    Next i
    PrintN("}")
  EndIf
EndIf
```

<!-- Output modified with a line break to avoid being too long -->
{{out}}

```txt
C:\Users\PureBasic_User\Desktop>"Power Set.exe" 1 2 3 4
{{}, {1}, {2}, {1, 2}, {3}, {1, 3}, {2, 3}, {1, 2, 3}, {4}, {1, 4},
{2, 4}, {1, 2, 4}, {3, 4}, {1, 3, 4}, {2, 3, 4}, {1, 2, 3, 4}}
```



## Python


```python
def list_powerset(lst):
    # the power set of the empty set has one element, the empty set
    result = [[]]
    for x in lst:
        # for every additional element in our set
        # the power set consists of the subsets that don't
        # contain this element (just take the previous power set)
        # plus the subsets that do contain the element (use list
        # comprehension to add [x] onto everything in the
        # previous power set)
        result.extend([subset + [x] for subset in result])
    return result

# the above function in one statement
def list_powerset2(lst):
    return reduce(lambda result, x: result + [subset + [x] for subset in result],
                  lst, [[]])

def powerset(s):
    return frozenset(map(frozenset, list_powerset(list(s))))
```


<tt>list_powerset</tt> computes the power set of a list of distinct elements. <tt>powerset</tt> simply converts the input and output from lists to sets. We use the <tt>frozenset</tt> type here for immutable sets, because unlike mutable sets, it can be put into other sets.

{{out|Example}}

```txt

>>> list_powerset([1,2,3])
[[], [1], [2], [1, 2], [3], [1, 3], [2, 3], [1, 2, 3]]
>>> powerset(frozenset([1,2,3]))
frozenset([frozenset([3]), frozenset([1, 2]), frozenset([]), frozenset([2, 3]), frozenset([1]), frozenset([1, 3]), frozenset([1, 2, 3]), frozenset([2])])

```



### = Further Explanation =

If you take out the requirement to produce sets and produce list versions of each powerset element, then add a print to trace the execution, you get this simplified version of the program above where it is easier to trace the inner workings

```python
def powersetlist(s):
    r = [[]]
    for e in s:
        print "r: %-55r e: %r" % (r,e)
        r += [x+[e] for x in r]
    return r

s= [0,1,2,3]
print "\npowersetlist(%r) =\n  %r" % (s, powersetlist(s))
```


{{out}}

```txt
r: [[]]                                                    e: 0
r: [[], [0]]                                               e: 1
r: [[], [0], [1], [0, 1]]                                  e: 2
r: [[], [0], [1], [0, 1], [2], [0, 2], [1, 2], [0, 1, 2]]  e: 3

powersetlist([0, 1, 2, 3]) =
  [[], [0], [1], [0, 1], [2], [0, 2], [1, 2], [0, 1, 2], [3], [0, 3], [1, 3], [0, 1, 3], [2, 3], [0, 2, 3], [1, 2, 3], [0, 1, 2, 3]]

```



###  Binary Count method

If you list the members of the set and include them according to if the corresponding bit position of a binary count is true then you generate the powerset.
(Note that only frozensets can be members of a set in the second function)

```python
def powersequence(val):
    ''' Generate a 'powerset' for sequence types that are indexable by integers.
        Uses a binary count to enumerate the members and returns a list

        Examples:
            >>> powersequence('STR')   # String
            ['', 'S', 'T', 'ST', 'R', 'SR', 'TR', 'STR']
            >>> powersequence([0,1,2]) # List
            [[], [0], [1], [0, 1], [2], [0, 2], [1, 2], [0, 1, 2]]
            >>> powersequence((3,4,5)) # Tuple
            [(), (3,), (4,), (3, 4), (5,), (3, 5), (4, 5), (3, 4, 5)]
            >>>
    '''
    vtype = type(val); vlen = len(val); vrange = range(vlen)
    return [ reduce( lambda x,y: x+y, (val[i:i+1] for i in vrange if 2**i & n), vtype())
             for n in range(2**vlen) ]

def powerset(s):
    ''' Generate the powerset of s

        Example:
            >>> powerset(set([6,7,8]))
            set([frozenset([7]), frozenset([8, 6, 7]), frozenset([6]), frozenset([6, 7]), frozenset([]), frozenset([8]), frozenset([8, 7]), frozenset([8, 6])])
    '''
    return set( frozenset(x) for x in powersequence(list(s)) )
```



###  Recursive Alternative

This is an (inefficient) recursive version that almost reflects the recursive definition of a power set as explained in http://en.wikipedia.org/wiki/Power_set#Algorithms. It does not create a sorted output.


```python

def p(l):
    if not l: return [[]]
    return p(l[1:]) + [[l[0]] + x for x in p(l[1:])]

```



### Python: Standard documentation

Pythons [http://docs.python.org/3/library/itertools.html?highlight=powerset#itertools-recipes documentation] has a method that produces the groupings, but not as sets:


```python>>>
 from pprint import pprint as pp
>>> from itertools import chain, combinations
>>>
>>> def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))

>>> pp(set(powerset({1,2,3,4})))
{(),
 (1,),
 (1, 2),
 (1, 2, 3),
 (1, 2, 3, 4),
 (1, 2, 4),
 (1, 3),
 (1, 3, 4),
 (1, 4),
 (2,),
 (2, 3),
 (2, 3, 4),
 (2, 4),
 (3,),
 (3, 4),
 (4,)}
>>>
```



## Qi

{{trans|Scheme}}

```qi

(define powerset
  [] -> [[]]
  [A|As] -> (append (map (cons A) (powerset As))
                    (powerset As)))

```



## R

===Non-recursive version===
The conceptual basis for this algorithm is the following:
<lang>for each element in the set:
	for each subset constructed so far:
		new subset = (subset + element)

```


This method is much faster than a recursive method, though the speed is still O(2^n).


```R
powerset = function(set){
	ps = list()
	ps[[1]] = numeric()						#Start with the empty set.
	for(element in set){						#For each element in the set, take all subsets
		temp = vector(mode="list",length=length(ps))		#currently in "ps" and create new subsets (in "temp")
		for(subset in 1:length(ps)){				#by adding "element" to each of them.
			temp[[subset]] = c(ps[[subset]],element)
		}
		ps=c(ps,temp)						#Add the additional subsets ("temp") to "ps".
	}
	return(ps)
}

powerset(1:4)

```


The list "temp" is a compromise between the speed costs of doing
arithmetic and of creating new lists (since R lists are immutable,
appending to a list means actually creating a new list object).
Thus, "temp" collects new subsets that are later added to the power set.
This improves the speed by 4x compared to extending the list "ps" at every step.


### Recursive version

{{libheader|sets}}
The sets package includes a recursive method to calculate the power set.
However, this method takes ~100 times longer than the non-recursive method above.

```R
library(sets)
```

An example with a vector.

```R
v <- (1:3)^2
sv <- as.set(v)
2^sv
```

 {{}, {1}, {4}, {9}, {1, 4}, {1, 9}, {4, 9}, {1, 4, 9}}
An example with a list.

```R
l <- list(a=1, b="qwerty", c=list(d=TRUE, e=1:3))
sl <- as.set(l)
2^sl
```

 {{}, {1}, {"qwerty"}, {<<list(2)>>}, {1, <<list(2)>>}, {"qwerty",
  1}, {"qwerty", <<list(2)>>}, {"qwerty", 1, <<list(2)>>}}


## Racket


```racket

;;; Direct translation of 'functional' ruby method
(define (powerset s)
  (for/fold ([outer-set (set(set))]) ([element s])
    (set-union outer-set
               (list->set (set-map outer-set
                                   (λ(inner-set) (set-add inner-set element)))))))

```



## Rascal


```rascal

import Set;

public set[set[&T]] PowerSet(set[&T] s) = power(s);

```

{{out}}

```rascal

rascal>PowerSet({1,2,3,4})
set[set[int]]: {
  {4,3},
  {4,2,1},
  {4,3,1},
  {4,2},
  {4,3,2},
  {4,1},
  {4,3,2,1},
  {4},
  {3},
  {2,1},
  {3,1},
  {2},
  {3,2},
  {1},
  {3,2,1},
  {}
}

```



## REXX


```rexx
/*REXX program  displays a  power set;  items may be  anything  (but can't have blanks).*/
parse arg S                                      /*allow the user specify optional set. */
if S=''  then S= 'one two three four'            /*Not specified?  Then use the default.*/
@= '{}'                                          /*start process with a null power set. */
N= words(S);     do chunk=1  for N               /*traipse through the items in the set.*/
                 @=@  combN(N, chunk)            /*take  N  items, a  CHUNK  at a time. */
                 end    /*chunk*/
w= length(2**N)                                  /*the number of items in the power set.*/
                 do k=1  for words(@)            /* [↓]  show combinations, one per line*/
                 say right(k, w)     word(@, k)  /*display a single combination to term.*/
                 end    /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
combN:  procedure expose S;  parse arg x,y;    base= x + 1;            bbase= base - y
        !.= 0
                        do p=1  for y;         !.p= p
                        end   /*p*/
        $=
                        do j=1;        L=
                                               do d=1  for y;          L= L','word(S, !.d)
                                               end   /*d*/

                        $=$  '{'strip(L, "L", ',')"}";                 !.y= !.y + 1
                        if !.y==base  then  if .combU(y - 1)  then leave
                        end   /*j*/
        return strip($)                          /*return with a partial powerset chunk.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
.combU: procedure expose !. y bbase;          parse arg d;          if d==0  then return 1
        p= !.d
                        do u=d  to y;         !.u= p + 1
                        if !.u==bbase+u  then return .combU(u-1)
                        p= !.u
                        end   /*u*/
        return 0
```

{{out|output|text=  when using the default input:}}

```txt

 1 {}
 2 {one}
 3 {two}
 4 {three}
 5 {four}
 6 {one,two}
 7 {one,three}
 8 {one,four}
 9 {two,three}
10 {two,four}
11 {three,four}
12 {one,two,three}
13 {one,two,four}
14 {one,three,four}
15 {two,three,four}
16 {one,two,three,four}

```



## Ring


```ring

# Project : Power set

list = ["1", "2", "3", "4"]
see powerset(list)

func powerset(list)
        s = "{"
        for i = 1 to (2 << len(list)) - 1 step 2
             s = s + "{"
             for j = 1 to len(list)
                  if i & (1 << j)
                     s = s + list[j] + ","
                  ok
             next
             if right(s,1) = ","
                s = left(s,len(s)-1)
             ok
             s = s + "},"
        next
        return left(s,len(s)-1) + "}"

```

Output:

```txt

{{},{1},{2},{1,2},{3},{1,3},{2,3},{1,2,3},{4},{1,4},{2,4},{1,2,4},{3,4},{1,3,4},{2,3,4},{1,2,3,4}}

```



## Ruby


```ruby
# Based on http://johncarrino.net/blog/2006/08/11/powerset-in-ruby/
# See the link if you want a shorter version.
# This was intended to show the reader how the method works.
class Array
  # Adds a power_set method to every array, i.e.: [1, 2].power_set
  def power_set

    # Injects into a blank array of arrays.
    # acc is what we're injecting into
    # you is each element of the array
    inject([[]]) do |acc, you|
      ret = []             # Set up a new array to add into
      acc.each do |i|      # For each array in the injected array,
        ret << i           # Add itself into the new array
        ret << i + [you]   # Merge the array with a new array of the current element
      end
      ret       # Return the array we're looking at to inject more.
    end

  end

  # A more functional and even clearer variant.
  def func_power_set
    inject([[]]) { |ps,item|    # for each item in the Array
      ps +                      # take the powerset up to now and add
      ps.map { |e| e + [item] } # it again, with the item appended to each element
    }
  end
end

#A direct translation of the "power array" version above
require 'set'
class Set
  def powerset
    inject(Set[Set[]]) do |ps, item|
      ps.union ps.map {|e| e.union (Set.new [item])}
    end
  end
end

p [1,2,3,4].power_set
p %w(one two three).func_power_set

p Set[1,2,3].powerset
```

{{out}}

```txt

[[], [4], [3], [3, 4], [2], [2, 4], [2, 3], [2, 3, 4], [1], [1, 4], [1, 3], [1, 3, 4], [1, 2], [1, 2, 4], [1, 2, 3], [1, 2, 3, 4]]
[[], ["one"], ["two"], ["one", "two"], ["three"], ["one", "three"], ["two", "three"], ["one", "two", "three"]]
#<Set: {#<Set: {}>, #<Set: {1}>, #<Set: {2}>, #<Set: {1, 2}>, #<Set: {3}>, #<Set: {1, 3}>, #<Set: {2, 3}>, #<Set: {1, 2, 3}>}>

```



## SAS


```SAS

options mprint mlogic symbolgen source source2;

%macro SubSets (FieldCount = );
data _NULL_;
	Fields = &FieldCount;
	SubSets = 2**Fields;
	call symput ("NumSubSets", SubSets);
run;

%put &NumSubSets;

data inital;
	%do j = 1 %to &FieldCount;
		F&j. = 1;
	%end;
run;

data SubSets;
	set inital;
	RowCount =_n_;
	call symput("SetCount",RowCount);
run;

%put SetCount ;

%do %while (&SetCount < &NumSubSets);

data loop;
	%do j=1 %to &FieldCount;
		if rand('GAUSSIAN') > rand('GAUSSIAN') then F&j. = 1;
	%end;

data SubSets_  ;
set SubSets loop;
run;

proc sort data=SubSets_  nodupkey;
	by F1 - F&FieldCount.;
run;

data Subsets;
	set SubSets_;
	RowCount =_n_;
run;

proc sql noprint;
	select max(RowCount) into :SetCount
	from SubSets;
quit;
run;

%end;
%Mend SubSets;

```


You can then call the macro as:

```SAS

%SubSets(FieldCount = 5);

```


The output will be the dataset SUBSETS
and will have a 5 columns F1, F2, F3, F4, F5 and 32 columns,
one with each combination of 1 and missing values.

{{out}}

```txt

Obs	F1	F2	F3	F4	F5	RowCount
1	.	.	.	.	.	1
2	.	.	.	.	1	2
3	.	.	.	1	.	3
4	.	.	.	1	1	4
5	.	.	1	.	.	5
6	.	.	1	.	1	6
7	.	.	1	1	.	7
8	.	.	1	1	1	8
9	.	1	.	.	.	9
10	.	1	.	.	1	10
11	.	1	.	1	.	11
12	.	1	.	1	1	12
13	.	1	1	.	.	13
14	.	1	1	.	1	14
15	.	1	1	1	.	15
16	.	1	1	1	1	16
17	1	.	.	.	.	17
18	1	.	.	.	1	18
19	1	.	.	1	.	19
20	1	.	.	1	1	20
21	1	.	1	.	.	21
22	1	.	1	.	1	22
23	1	.	1	1	.	23
24	1	.	1	1	1	24
25	1	1	.	.	.	25
26	1	1	.	.	1	26
27	1	1	.	1	.	27
28	1	1	.	1	1	28
29	1	1	1	.	.	29
30	1	1	1	.	1	30
31	1	1	1	1	.	31
32	1	1	1	1	1	32

```



## Scala


```scala
import scala.compat.Platform.currentTime

object Powerset extends App {
  def powerset[A](s: Set[A]) = s.foldLeft(Set(Set.empty[A])) { case (ss, el) => ss ++ ss.map(_ + el)}

  assert(powerset(Set(1, 2, 3, 4)) == Set(Set.empty, Set(1), Set(2), Set(3), Set(4), Set(1, 2), Set(1, 3), Set(1, 4),
    Set(2, 3), Set(2, 4), Set(3, 4), Set(1, 2, 3), Set(1, 3, 4), Set(1, 2, 4), Set(2, 3, 4), Set(1, 2, 3, 4)))
  println(s"Successfully completed without errors. [total ${currentTime - executionStart} ms]")
}
```


Another option that produces lazy sequence of the sets:

```scala
def powerset[A](s: Set[A]) = (0 to s.size).map(s.toSeq.combinations(_)).reduce(_ ++ _).map(_.toSet)
```


A tail-recursive version:

```scala
def powerset[A](s: Set[A]) = {
  def powerset_rec(acc: List[Set[A]], remaining: List[A]): List[Set[A]] = remaining match {
    case Nil => acc
    case head :: tail => powerset_rec(acc ++ acc.map(_ + head), tail)
  }
  powerset_rec(List(Set.empty[A]), s.toList)
}
```



## Scheme

{{trans|Common Lisp}}

```scheme
(define (power-set set)
  (if (null? set)
      '(())
      (let ((rest (power-set (cdr set))))
        (append (map (lambda (element) (cons (car set) element))
                     rest)
                rest))))

(display (power-set (list 1 2 3)))
(newline)

(display (power-set (list "A" "C" "E")))
(newline)
```

{{out}}
 ((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ())
 ((A C E) (A C) (A E) (A) (C E) (C) (E) ())

Call/cc generation:
```lisp
(define (power-set lst)
  (define (iter yield)
    (let recur ((a '()) (b lst))
      (if (null? b) (set! yield
		      (call-with-current-continuation
			(lambda (resume)
			  (set! iter resume)
			  (yield a))))
	(begin (recur (append a (list (car b))) (cdr b))
	       (recur a (cdr b)))))

    ;; signal end of generation
    (yield 'end-of-seq))

  (lambda () (call-with-current-continuation iter)))

(define x (power-set '(1 2 3)))
(let loop ((a (x)))
  (if (eq? a 'end-of-seq) #f
    (begin
      (display a)
      (newline)
      (loop (x)))))
```

{{out}}

```txt
(1 2)
(1 3)
(1)
(2 3)
(2)
(3)
()
```


Iterative:
```scheme

(define (power_set_iter set)
  (let loop ((res '(())) (s set))
    (if (empty? s)
        res
        (loop (append (map (lambda (i) (cons (car s) i)) res) res) (cdr s)))))

```


{{out}}

```txt

'((e d c b a)
  (e d c b)
  (e d c a)
  (e d c)
  (e d b a)
  (e d b)
  (e d a)
  (e d)
  (e c b a)
  (e c b)
  (e c a)
  (e c)
  (e b a)
  (e b)
  (e a)
  (e)
  (d c b a)
  (d c b)
  (d c a)
  (d c)
  (d b a)
  (d b)
  (d a)
  (d)
  (c b a)
  (c b)
  (c a)
  (c)
  (b a)
  (b)
  (a)
  ())

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func array bitset: powerSet (in bitset: baseSet) is func
  result
    var array bitset: pwrSet is [] (bitset.value);
  local
    var integer: element is 0;
    var integer: index is 0;
    var bitset: aSet is bitset.value;
  begin
    for element range baseSet do
      for key index range pwrSet do
        aSet := pwrSet[index];
        if element not in aSet then
          incl(aSet, element);
          pwrSet &:= aSet;
        end if;
      end for;
    end for;
  end func;

const proc: main is func
  local
    var bitset: aSet is bitset.value;
  begin
    for aSet range powerSet({1, 2, 3, 4}) do
      writeln(aSet);
    end for;
  end func;
```


{{out}}

```txt

{}
{1}
{2}
{1, 2}
{3}
{1, 3}
{2, 3}
{1, 2, 3}
{4}
{1, 4}
{2, 4}
{1, 2, 4}
{3, 4}
{1, 3, 4}
{2, 3, 4}
{1, 2, 3, 4}

```



## SETL


```haskell
Pfour := pow({1, 2, 3, 4});
Pempty := pow({});
PPempty := pow(Pempty);

print(Pfour);
print(Pempty);
print(PPempty);
```


{{out}}

```txt
{{} {1} {2} {3} {4} {1 2} {1 3} {1 4} {2 3} {2 4} {3 4} {1 2 3} {1 2 4} {1 3 4} {2 3 4} {1 2 3 4}}
{{}}
{{} {{}}}
```



## Sidef


```ruby
var arr = %w(a b c)
for i in (0 .. arr.len) {
    say arr.combinations(i)
}
```


{{out}}

```txt

[[]]
[["a"], ["b"], ["c"]]
[["a", "b"], ["a", "c"], ["b", "c"]]
[["a", "b", "c"]]

```


## Simula


```simula
SIMSET
BEGIN

    LINK CLASS LOF_INT(N); INTEGER N;;

    LINK CLASS LOF_LOF_INT(H); REF(HEAD) H;;

    REF(HEAD) PROCEDURE MAP(P_LI, P_LLI);
        REF(HEAD) P_LI;
        REF(HEAD) P_LLI;
    BEGIN
        REF(HEAD) V_RESULT;
        V_RESULT :- NEW HEAD;
        IF NOT P_LLI.EMPTY THEN BEGIN
            REF(LOF_LOF_INT) V_LLI;
            V_LLI :- P_LLI.FIRST QUA LOF_LOF_INT;
            WHILE V_LLI =/= NONE DO BEGIN
                REF(HEAD) V_NEWLIST;
                V_NEWLIST :- NEW HEAD;
                ! ADD THE SAME 1ST ELEMENT TO EVERY NEWLIST ;
                NEW LOF_INT(P_LI.FIRST QUA LOF_INT.N).INTO(V_NEWLIST);
                IF NOT V_LLI.H.EMPTY THEN BEGIN
                    REF(LOF_INT) V_LI;
                    V_LI :- V_LLI.H.FIRST QUA LOF_INT;
                    WHILE V_LI =/= NONE DO BEGIN
                        NEW LOF_INT(V_LI.N).INTO(V_NEWLIST);
                        V_LI :- V_LI.SUC;
                    END;
                END;
                NEW LOF_LOF_INT(V_NEWLIST).INTO(V_RESULT);
                V_LLI :- V_LLI.SUC;
            END;
        END;
        MAP :- V_RESULT;
    END MAP;

    REF(HEAD) PROCEDURE SUBSETS(P_LI);
        REF(HEAD) P_LI;
    BEGIN
        REF(HEAD) V_RESULT;
        IF P_LI.EMPTY THEN BEGIN
            V_RESULT :- NEW HEAD;
            NEW LOF_LOF_INT(NEW HEAD).INTO(V_RESULT);
        END ELSE BEGIN
            REF(HEAD) V_SUBSET, V_MAP;
            REF(LOF_INT) V_LI;
            V_SUBSET :- NEW HEAD;
            V_LI :- P_LI.FIRST QUA LOF_INT;
            ! SKIP OVER 1ST ELEMENT ;
            IF V_LI =/= NONE THEN V_LI :- V_LI.SUC;
            WHILE V_LI =/= NONE DO BEGIN
                NEW LOF_INT(V_LI.N).INTO(V_SUBSET);
                V_LI :- V_LI.SUC;
            END;
            V_RESULT :- SUBSETS(V_SUBSET);
            V_MAP :- MAP(P_LI, V_RESULT);
            IF NOT V_MAP.EMPTY THEN BEGIN
                REF(LOF_LOF_INT) V_LLI;
                V_LLI :- V_MAP.FIRST QUA LOF_LOF_INT;
                WHILE V_LLI =/= NONE DO BEGIN
                    NEW LOF_LOF_INT(V_LLI.H).INTO(V_RESULT);
                    V_LLI :- V_LLI.SUC;
                END;
            END;
        END;
        SUBSETS :- V_RESULT;
    END SUBSETS;

    PROCEDURE PRINT_LIST(P_LI); REF(HEAD) P_LI;
    BEGIN
        OUTTEXT("[");
        IF NOT P_LI.EMPTY THEN BEGIN
            INTEGER I;
            REF(LOF_INT) V_LI;
            I := 0;
            V_LI :- P_LI.FIRST QUA LOF_INT;
            WHILE V_LI =/= NONE DO BEGIN
                IF I > 0 THEN OUTTEXT(",");
                OUTINT(V_LI.N, 0);
                V_LI :- V_LI.SUC;
                I := I+1;
            END;
        END;
        OUTTEXT("]");
    END PRINT_LIST;

    PROCEDURE PRINT_LIST_LIST(P_LLI); REF(HEAD) P_LLI;
    BEGIN
        OUTTEXT("[");
        IF NOT P_LLI.EMPTY THEN BEGIN
            INTEGER I;
            REF(LOF_LOF_INT) V_LLI;
            I := 0;
            V_LLI :- P_LLI.FIRST QUA LOF_LOF_INT;
            WHILE V_LLI =/= NONE DO BEGIN
                IF I > 0 THEN BEGIN
                    OUTTEXT(",");
                !   OUTIMAGE;
                END;
                PRINT_LIST(V_LLI.H);
                V_LLI :- V_LLI.SUC;
                I := I+1;
            END;
        END;
        OUTTEXT("]");
        OUTIMAGE;
    END PRINT_LIST_LIST;

    INTEGER N;
    REF(HEAD) V_RANGE;
    REF(HEAD) V_LISTS;

    V_RANGE :- NEW HEAD;
    V_LISTS :- SUBSETS(V_RANGE);
    PRINT_LIST_LIST(V_LISTS);
    OUTIMAGE;
    FOR N := 1 STEP 1 UNTIL 4 DO BEGIN
        NEW LOF_INT(N).INTO(V_RANGE);
        V_LISTS :- SUBSETS(V_RANGE);
        PRINT_LIST_LIST(V_LISTS);
        OUTIMAGE;
    END;
END.

```

{{out}}

```txt

[[]]

[[],[1]]

[[],[2],[1],[1,2]]

[[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

[[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4],[1],[1,4],[1,3],[1,3,4],[1,2],[1,2,4],
[1,2,3],[1,2,3,4]]

```



## Smalltalk

{{works with|GNU Smalltalk}}
Code from [http://smalltalk.gnu.org/blog/bonzinip/fun-generators Bonzini's blog]


```smalltalk
Collection extend [
    power [
        ^(0 to: (1 bitShift: self size) - 1) readStream collect: [ :each || i |
            i := 0.
            self select: [ :elem | (each bitAt: (i := i + 1)) = 1 ] ]
    ]
].
```



```smalltalk
#(1 2 4) power do: [ :each |
    each asArray printNl ].

#( 'A' 'C' 'E' ) power do: [ :each |
    each asArray printNl ].
```



## Standard ML


version for lists:

```sml
fun subsets xs = foldr (fn (x, rest) => rest @ map (fn ys => x::ys) rest) [[]] xs
```



## Swift

{{works with|Swift|Revision 4 - tested with Xcode 9.2 playground}}


```Swift>func powersetFrom<T
(_ elements: Set<T>) -> Set<Set<T>> {
  guard elements.count > 0 else {
    return [[]]
  }
  var powerset: Set<Set<T>> = [[]]
  for element in elements {
    for subset in powerset {
      powerset.insert(subset.union([element]))
    }
  }
  return powerset
}

// Example:
powersetFrom([1, 2, 4])
```

{{out}}

```txt
{
  {2, 4}
  {4, 1}
  {4},
  {2, 4, 1}
  {2, 1}
  Set([])
  {1}
  {2}
}
```



```Swift
//Example:
powersetFrom(["a", "b", "d"])
```

{{out}}

```txt
{
  {"b", "d"}
  {"b"}
  {"d"},
  {"a"}
  {"b", "d", "a"}
  Set([])
  {"d", "a"}
  {"b", "a"}
}
```



## Tcl


```tcl
proc subsets {l} {
    set res [list [list]]
    foreach e $l {
        foreach subset $res {lappend res [lappend subset $e]}
    }
    return $res
}
puts [subsets {a b c d}]
```

{{out}}

```txt
{} a b {a b} c {a c} {b c} {a b c} d {a d} {b d} {a b d} {c d} {a c d} {b c d} {a b c d}
```


### Binary Count Method


```tcl
proc powersetb set {
   set res {}
   for {set i 0} {$i < 2**[llength $set]} {incr i} {
      set pos -1
      set pset {}
      foreach el $set {
          if {$i & 1<<[incr pos]} {lappend pset $el}
      }
      lappend res $pset
   }
   return $res
}
```



## TXR


The power set function can be written concisely like this:


```txr
(defun power-set (s)
  (mappend* (op comb s) (range 0 (length s))))
```


This generates the lists of combinations of all possible lengths, from 0 to the length of <code>s</code> and catenates them.  The <code>comb</code> function generates a lazy list, so it is appropriate to use <code>mappend*</code> (the lazy version of <code>mappend</code>) to keep the behavior lazy.

A complete program which takes command line arguments and prints the power set in comma-separated brace notation:


```txr
@(do (defun power-set (s)
       (mappend* (op comb s) (range 0 (length s)))))
@(bind pset @(power-set *args*))
@(output)
@  (repeat)
{@(rep)@pset, @(last)@pset@(empty)@(end)}
@  (end)
@(end)
```

{{out}}

```txt
$ txr rosetta/power-set.txr  1 2 3
{1, 2, 3}
{1, 2}
{1, 3}
{1}
{2, 3}
{2}
{3}
{}
```


The above <code>power-set</code> function
generalizes to strings and vectors.


```txr
@(do (defun power-set (s)
       (mappend* (op comb s) (range 0 (length s))))
     (prinl (power-set "abc"))
     (prinl (power-set "b"))
     (prinl (power-set ""))
     (prinl (power-set #(1 2 3))))
```

{{out}}

```txt
$ txr power-set-generic.txr
("" "a" "b" "c" "ab" "ac" "bc" "abc")
("" "b")
("")
(#() #(1) #(2) #(3) #(1 2) #(1 3) #(2 3) #(1 2 3))
```



## UnixPipes


```ksh

| cat A
a
b
c

| cat A |\
   xargs -n 1 ksh -c 'echo \{`cat A`\}' |\
   xargs |\
   sed -e 's; ;,;g' \
       -e 's;^;echo ;g' \
       -e 's;\},;}\\ ;g' |\
   ksh |unfold `wc -l A` |\
   xargs -n1 -I{} ksh -c 'echo {} |\
        unfold 1 |sort -u |xargs' |sort -u

a
a b
a b c
a c
b
b c
c

```



## UNIX Shell

From [http://www.catonmat.net/blog/set-operations-in-unix-shell/ here]

```bash
p() { [ $# -eq 0 ] && echo || (shift; p "$@") | while read r ; do echo -e "$1 $r\n$r"; done }
```

Usage

```bash
|p `cat` | sort | uniq
A
C
E
^D
```



## Ursala


Sets are a built in type constructor in Ursala, represented as
lexically sorted lists with duplicates removed.
The powerset function is a standard library function,
but could be defined as shown below.

```Ursala
powerset = ~&NiC+ ~&i&& ~&at^?\~&aNC ~&ahPfatPRXlNrCDrT
```

test program:

```Ursala
#cast %sSS

test = powerset {'a','b','c','d'}
```

{{out}}

```txt
{
   {},
   {'a'},
   {'a','b'},
   {'a','b','c'},
   {'a','b','c','d'},
   {'a','b','d'},
   {'a','c'},
   {'a','c','d'},
   {'a','d'},
   {'b'},
   {'b','c'},
   {'b','c','d'},
   {'b','d'},
   {'c'},
   {'c','d'},
   {'d'}}
```



## V

V has a built in called powerlist

```v
[A C E] powerlist
=[[A C E] [A C] [A E] [A] [C E] [C] [E] []]
```


its implementation in std.v is (like joy)

```v
[powerlist
   [null?]
   [unitlist]
   [uncons]
   [dup swapd [cons] map popd swoncat]
    linrec].

```



## VBA


```vb
Option Base 1
Private Function power_set(ByRef st As Collection) As Collection
    Dim subset As Collection, pwset As New Collection
    For i = 0 To 2 ^ st.Count - 1
        Set subset = New Collection
        For j = 1 To st.Count
            If i And 2 ^ (j - 1) Then subset.Add st(j)
        Next j
        pwset.Add subset
    Next i
    Set power_set = pwset
End Function
Private Function print_set(ByRef st As Collection) As String
    'assume st is a collection of collections, holding integer variables
    Dim s() As String, t() As String
    ReDim s(st.Count)
    'Debug.Print "{";
    For i = 1 To st.Count
        If st(i).Count > 0 Then
            ReDim t(st(i).Count)
            For j = 1 To st(i).Count
                Select Case TypeName(st(i)(j))
                    Case "Integer": t(j) = CStr(st(i)(j))
                    Case "Collection": t(j) = "{}" 'assumes empty
                End Select
            Next j
            s(i) = "{" & Join(t, ", ") & "}"
        Else
            s(i) = "{}"
        End If
    Next i
    print_set = "{" & Join(s, ", ") & "}"
End Function
Public Sub rc()
    Dim rcset As New Collection, result As Collection
    For i = 1 To 4
        rcset.Add i
    Next i
    Debug.Print print_set(power_set(rcset))
    Set rcset = New Collection
    Debug.Print print_set(power_set(rcset))
    Dim emptyset As New Collection
    rcset.Add emptyset
    Debug.Print print_set(power_set(rcset))
    Debug.Print
End Sub
```
{{out}}

```txt
{{}, {1}, {2}, {1, 2}, {3}, {1, 3}, {2, 3}, {1, 2, 3}, {4}, {1, 4}, {2, 4}, {1, 2, 4}, {3, 4}, {1, 3, 4}, {2, 3, 4}, {1, 2, 3, 4}}
{{}}
{{}, {{}}}
```



## VBScript


```vb
Function Dec2Bin(n)
	q = n
	Dec2Bin = ""
	Do Until q = 0
		Dec2Bin = CStr(q Mod 2) & Dec2Bin
		q = Int(q / 2)
	Loop
	Dec2Bin = Right("00000" & Dec2Bin,6)
End Function

Function PowerSet(s)
	arrS = Split(s,",")
	PowerSet = "{"
	For i = 0 To 2^(UBound(arrS)+1)-1
		If i = 0 Then
			PowerSet = PowerSet & "{},"
		Else
			binS = Dec2Bin(i)
			PowerSet = PowerSet & "{"
			c = 0
			For j = Len(binS) To 1 Step -1
				If CInt(Mid(binS,j,1)) = 1 Then
					PowerSet = PowerSet & arrS(c) & ","
				End If
				c = c + 1
			Next
			PowerSet = Mid(PowerSet,1,Len(PowerSet)-1) & "},"
		End If
	Next
	PowerSet = Mid(PowerSet,1,Len(PowerSet)-1) & "}"
End Function

WScript.StdOut.Write PowerSet("1,2,3,4")
```

{{out}}

```txt
{{},{1},{2},{1,2},{3},{1,3},{2,3},{1,2,3},{4},{1,4},{2,4},{1,2,4},{3,4},{1,3,4},{2,3,4},{1,2,3,4}}
```



## zkl

Using a combinations function, build the power set from combinations of 1,2,... items.

```zkl
fcn pwerSet(list){
  (0).pump(list.len(),List, Utils.Helpers.pickNFrom.fp1(list),
     T(Void.Write,Void.Write) ) .append(list)
}
```


```zkl
foreach n in (5){
   ps:=pwerSet((1).pump(n,List)); ps.println(" Size = ",ps.len());
}
```

{{out}}

```txt

L(L()) Size = 1
L(L(),L(1)) Size = 2
L(L(),L(1),L(2),L(1,2)) Size = 4
L(L(),L(1),L(2),L(3),L(1,2),L(1,3),L(2,3),L(1,2,3)) Size = 8
L(L(),L(1),L(2),L(3),L(4),L(1,2),L(1,3),L(1,4),L(2,3),L(2,4),
   L(3,4),L(1,2,3),L(1,2,4),L(1,3,4),L(2,3,4),L(1,2,3,4)) Size = 16

```

