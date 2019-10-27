+++
title = "Flatten a list"
description = ""
date = 2019-10-10T18:44:23Z
aliases = []
[extra]
id = 4747
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Write a function to flatten the nesting in an arbitrary [[wp:List (computing)|list]] of values. 

Your program should work on the equivalent of this list:
   [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
Where the correct result would be the list:
    [1, 2, 3, 4, 5, 6, 7, 8]

;Related task:
*   [[Tree traversal]]





## 8th


```forth

\ take a list (array) and flatten it:

: (flatten)  \ a -- a
	(
		\ is it a number?
		dup >kind ns:n n:= if
			\ yes.  so add to the list
			r> swap a:push >r
		else
			\ it is not, so flatten it
			(flatten)
		then
		drop
	) a:each drop ;
	
: flatten \ a -- a
	[] >r (flatten) r> ;

[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
dup . cr
flatten 
. cr 
bye

```

{{out}}
[[1],2,[[3,4],5],[[[]]],[[[6]]],7,8,[]]

[1,2,3,4,5,6,7,8]


## ACL2


```Lisp
(defun flatten (tr)
   (cond ((null tr) nil)
         ((atom tr) (list tr))
         (t (append (flatten (first tr))
                    (flatten (rest tr))))))
```



## ActionScript


```ActionScript
function flatten(input:Array):Array {
	var output:Array = new Array();
	for (var i:uint = 0; i < input.length; i++) {
                //typeof returns "object" when applied to arrays. This line recursively evaluates nested arrays,
                // although it may break if the array contains objects that are not arrays.
		if (typeof input[i]=="object") {
			output=output.concat(flatten(input[i]));
		} else {
			output.push(input[i]);
		}
	}
	return output;
}

```



## Ada

nestable_lists.ads:

```Ada
generic
   type Element_Type is private;
   with function To_String (E : Element_Type) return String is <>;
package Nestable_Lists is

   type Node_Kind is (Data_Node, List_Node);
   
   type Node (Kind : Node_Kind);
   
   type List is access Node;
   
   type Node (Kind : Node_Kind) is record
      Next : List;
      case Kind is
         when Data_Node =>
            Data    : Element_Type;
         when List_Node =>
            Sublist : List;
      end case;
   end record;
   
   procedure Append (L : in out List; E : Element_Type);
   procedure Append (L : in out List; N : List);
   
   function Flatten (L : List) return List;

   function New_List (E : Element_Type) return List;
   function New_List (N : List) return List;
   
   function To_String (L : List) return String;
   
end Nestable_Lists;
```

nestable_lists.adb:

```Ada
with Ada.Strings.Unbounded;

package body Nestable_Lists is

   procedure Append (L : in out List; E : Element_Type) is
   begin
      if L = null then
         L := new Node (Kind => Data_Node);
         L.Data := E;
      else
         Append (L.Next, E);
      end if;
   end Append;

   procedure Append (L : in out List; N : List) is
   begin
      if L = null then
         L := new Node (Kind => List_Node);
         L.Sublist := N;
      else
         Append (L.Next, N);
      end if;
   end Append;

   function Flatten (L : List) return List is
      Result  : List;
      Current : List := L;
      Temp    : List;
   begin
      while Current /= null loop
         case Current.Kind is
            when Data_Node =>
               Append (Result, Current.Data);
            when List_Node =>
               Temp := Flatten (Current.Sublist);
               while Temp /= null loop
                  Append (Result, Temp.Data);
                  Temp := Temp.Next;
               end loop;
         end case;
         Current := Current.Next;
      end loop;
      return Result;
   end Flatten;
   
   function New_List (E : Element_Type) return List is
   begin
      return  new Node'(Kind => Data_Node, Data => E, Next => null);
   end New_List;

   function New_List (N : List) return List is
   begin
      return new Node'(Kind => List_Node, Sublist => N, Next => null);
   end New_List;

   function To_String (L : List) return String is
      Current : List := L;
      Result  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Ada.Strings.Unbounded.Append (Result, "[");
      while Current /= null loop
         case Current.Kind is
            when Data_Node =>
               Ada.Strings.Unbounded.Append
                 (Result, To_String (Current.Data));
            when List_Node =>
               Ada.Strings.Unbounded.Append
                 (Result, To_String (Current.Sublist));
         end case;
         if Current.Next /= null then
            Ada.Strings.Unbounded.Append (Result, ", ");
         end if;
         Current := Current.Next;
      end loop;
      Ada.Strings.Unbounded.Append (Result, "]");
      return Ada.Strings.Unbounded.To_String (Result);
   end To_String;

end Nestable_Lists;
```

example usage:

```Ada
with Ada.Text_IO;
with Nestable_Lists;

procedure Flatten_A_List is
   package Int_List is new Nestable_Lists
     (Element_Type => Integer,
      To_String    => Integer'Image);

   List : Int_List.List := null;
begin
   Int_List.Append (List, Int_List.New_List (1));
   Int_List.Append (List, 2);
   Int_List.Append (List, Int_List.New_List (Int_List.New_List (3)));
   Int_List.Append (List.Next.Next.Sublist.Sublist, 4);
   Int_List.Append (List.Next.Next.Sublist, 5);
   Int_List.Append (List, Int_List.New_List (Int_List.New_List (null)));
   Int_List.Append (List, Int_List.New_List (Int_List.New_List
                    (Int_List.New_List (6))));
   Int_List.Append (List, 7);
   Int_List.Append (List, 8);
   Int_List.Append (List, null);
   
   declare
      Flattened : constant Int_List.List := Int_List.Flatten (List);
   begin
      Ada.Text_IO.Put_Line (Int_List.To_String (List));
      Ada.Text_IO.Put_Line (Int_List.To_String (Flattened));
   end;
end Flatten_A_List;
```

Output:

```txt
[[ 1],  2, [[ 3,  4],  5], [[[]]], [[[ 6]]],  7,  8, []]
[ 1,  2,  3,  4,  5,  6,  7,  8]
```



## Aikido


```aikido

function flatten (list, result) {
    foreach item list {
        if (typeof(item) == "vector") {
            flatten (item, result)
        } else {
            result.append (item)
        }
    }
}

var l = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
var newl = []
flatten (l, newl)

// print out the nicely formatted result list
print ('[')
var comma = ""
foreach item newl {
    print (comma + item)
    comma = ", "
}
println("]")


```

{{out}}

```txt

 [1, 2, 3, 4, 5, 6, 7, 8]

```



## Aime


```aime
void
show_list(list l)
{
    integer i, k;

    o_text("[");

    i = 0;
    while (i < ~l) {
        o_text(i ? ", " : "");
        if (l_j_integer(k, l, i)) {
            o_integer(k);
        } else {
            show_list(l[i]);
        }
        i += 1;
    }

    o_text("]");
}

list
flatten(list c, object o)
{
    if (__id(o) == INTEGER_ID) {
        c.append(o);
    } else {
        l_ucall(o, flatten, 1, c);
    }

    c;
}

integer
main(void)
{
    list l;

    l = list(list(1), 2, list(list(3, 4), 5),
             list(list(list())), list(list(list(6))), 7, 8, list());

    show_list(l);
    o_byte('\n');

    show_list(flatten(list(), l));
    o_byte('\n');

    return 0;
}
```

{{out}}

```txt

[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
[1, 2, 3, 4, 5, 6, 7, 8]

```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

Flattening is built in to all of Algol68's ''transput'' routines.  The following example also uses ''widening'', where scalars are converted into arrays.


```algol68
main:(
  [][][]INT list = ((1), 2, ((3,4), 5), ((())), (((6))), 7, 8, ());
  print((list, new line))
)
```

{{out}}

```txt

         +1         +2         +3         +4         +5         +6         +7         +8

```



## GNU APL

Using (monadic) enlist function ε. Sometimes called 'Super Ravel'.

```APL

      ⊢list←(2 3ρι6)(2 2ρ(7 8(2 2ρ9 10 11 12)13)) 'ABCD'
┏→━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃┏→━━━━┓ ┏→━━━━━━━━━┓ "ABCD"┃
┃↓1 2 3┃ ↓      7  8┃       ┃
┃┃4 5 6┃ ┃          ┃       ┃
┃┗━━━━━┛ ┃┏→━━━━┓ 13┃       ┃
┃        ┃↓ 9 10┃   ┃       ┃
┃        ┃┃11 12┃   ┃       ┃
┃        ┃┗━━━━━┛   ┃       ┃
┃        ┗∊━━━━━━━━━┛       ┃
┗∊∊━━━━━━━━━━━━━━━━━━━━━━━━━┛
      ∊list
┏→━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃1 2 3 4 5 6 7 8 9 10 11 12 13 'A''B''C''D'┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛

```



## AppleScript


```applescript
my_flatten({{1}, 2, {{3, 4}, 5}, {{{}}}, {{{6}}}, 7, 8, {}})

on my_flatten(aList)
    if class of aList is not list then
        return {aList}
    else if length of aList is 0 then
        return aList
    else
        return my_flatten(first item of aList) & (my_flatten(rest of aList))
    end if
end my_flatten

```



Or, expressed in terms of a generic '''concatMap''':
{{trans|JavaScript}}

```AppleScript
-- flatten :: Tree a -> [a]
on flatten(t)
    if class of t is list then
        concatMap(flatten, t)
    else
        t
    end if
end flatten

-- TEST -----------------------------------------------------------------------
on run
    
    flatten([[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []])
    
    --> {1, 2, 3, 4, 5, 6, 7, 8}
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
{1, 2, 3, 4, 5, 6, 7, 8}
```



## AutoHotkey

{{works with | AutoHotkey_L}}

AutoHotkey doesn't have built in list data type.  
This examples simulates a list in a tree type object and flattens that tree.  

```AutoHotkey
list := object(1, object(1, 1), 2, 2, 3, object(1, object(1, 3, 2, 4)
, 2, 5), 4, object(1, object(1, object(1, object()))), 5
, object(1, object(1, 6)), 6, 7, 7, 8, 9, object())
msgbox % objPrint(list) ; (( 1 ) 2 (( 3  4 ) 5 )(((())))(( 6 )) 7  8 ())
msgbox % objPrint(objFlatten(list)) ; ( 1  2  3  4  5  6  7  8 )
return

!r::reload
!q::exitapp

objPrint(ast, reserved=0)
{
  if !isobject(ast)
    return " " ast " "
  
  if !reserved
    reserved := object("seen" . &ast, 1)  ; to keep track of unique objects within top object
  
  enum := ast._newenum()
  while enum[key, value]
  {
    if reserved["seen" . &value]
      continue  ; don't copy repeat objects (circular references)
;   string .= key . ": " . objPrint(value, reserved)
    string .= objPrint(value, reserved)
  }
  return "(" string ")"
}


objFlatten(ast)
{
  if !isobject(ast)
    return ast
  
  flat := object() ; flat object
  
  enum := ast._newenum()
  while enum[key, value]
  {
    if !isobject(value)
      flat._Insert(value)
    else
    {
      next := objFlatten(value)
      loop % next._MaxIndex()
      flat._Insert(next[A_Index])
      
    }
  }
  return flat
}
```



## BaCon

BaCon has the concept of delimited strings, which may contain delimited strings within delimited strings etc. Such nested delimited strings must be surrounded by (escaped) double quotes in order to avoid their delimiter messing up operations on higher level delimited strings. However, from functional point of view, a delimited string is the same as a regular list. The special function FLATTEN$ can actually flatten out lists within lists. The last SORT$ in the program below makes sure no empty items remain in the list.

```qbasic
OPTION COLLAPSE TRUE

lst$ = "\"1\",2,\"\\\"3,4\\\",5\",\"\\\"\\\\\"\\\\\"\\\"\",\"\\\"\\\\\"6\\\\\"\\\"\",7,8,\"\""

PRINT lst$

REPEAT
    lst$ = FLATTEN$(lst$)
UNTIL AMOUNT(lst$, ",") = AMOUNT(FLATTEN$(lst$), ",")

PRINT SORT$(lst$, ",")
```

{{out}}

```txt

"1",2,"\"3,4\",5","\"\\"\\"\"","\"\\"6\\"\"",7,8,""
1,2,3,4,5,6,7,8

```



## Bracmat

A list is automatically flattened during evaluation if the items are separated by either commas, plusses, asterisks or white spaces.

On top of that, lists separated with white space, plusses or asterisks have 'nil'-elements removed when evaluated. (nil-elements are empty strings, 0 and 1 respectively.)

On top of that, lists separated with plusses or asterisks have their elements sorted and, if possible, combined when evaluated.

A list that should not be flattened upon evaluation can be separated with dots.


```bracmat

( (myList = ((1), 2, ((3,4), 5), ((())), (((6))), 7, 8, ()))
& put$("Unevaluated:")
& lst$myList
& !myList:?myList          { the expression !myList evaluates myList }
& put$("Flattened:")
& lst$myList
)

```



## Brat


```brat
array.prototype.flatten = {
  true? my.empty?
    { [] }
    { true? my.first.array?
      { my.first.flatten + my.rest.flatten }
      { [my.first] + my.rest.flatten }
    }
}

list = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
p "List: #{list}"
p "Flattened: #{list.flatten}"
```



## Burlesque


Usually flattening Blocks is done with the Concat command but it only removes one level of nesting therefore it is required to chain Concat calls
until the Block does not contain Blocks anymore. 


```burlesque

blsq ) {{1} 2 {{3 4} 5} {{{}}} {{{6}}} 7 8 {}}{\[}{)to{"Block"==}ay}w!
{1 2 3 4 5 6 7 8}

```



## C


```C>#include <stdio.h

#include <stdlib.h>
#include <string.h>

typedef struct list_t list_t, *list;
struct list_t{
	int is_list, ival; /* ival is either the integer value or list length */
	list *lst;
};

list new_list()
{
	list x = malloc(sizeof(list_t));
	x->ival = 0;
	x->is_list = 1;
	x->lst = 0;
	return x;
}

void append(list parent, list child)
{
	parent->lst = realloc(parent->lst, sizeof(list) * (parent->ival + 1));
	parent->lst[parent->ival++] = child;
}

list from_string(char *s, char **e, list parent)
{
	list ret = 0;
	if (!parent) parent = new_list();

	while (*s != '\0') {
		if (*s == ']') {
			if (e) *e = s + 1;
			return parent;
		}
		if (*s == '[') {
			ret = new_list();
			ret->is_list = 1;
			ret->ival = 0;
			append(parent, ret);
			from_string(s + 1, &s, ret);
			continue;
		}
		if (*s >= '0' && *s <= '9') {
			ret = new_list();
			ret->is_list = 0;
			ret->ival = strtol(s, &s, 10);
			append(parent, ret);
			continue;
		}
		s++;
	}

	if (e) *e = s;
	return parent;
}

void show_list(list l)
{
	int i;
	if (!l) return;
	if (!l->is_list) {
		printf("%d", l->ival);
		return;
	}

	printf("[");
	for (i = 0; i < l->ival; i++) {
		show_list(l->lst[i]);
		if (i < l->ival - 1) printf(", ");
	}
	printf("]");
}

list flatten(list from, list to)
{
	int i;
	list t;

	if (!to) to = new_list();
	if (!from->is_list) {
		t = new_list();
		*t = *from;
		append(to, t);
	} else
		for (i = 0; i < from->ival; i++)
			flatten(from->lst[i], to);
	return to;
}

void delete_list(list l)
{
	int i;
	if (!l) return;
	if (l->is_list && l->ival) {
		for (i = 0; i < l->ival; i++)
			delete_list(l->lst[i]);
		free(l->lst);
	}

	free(l);
}

int main()
{
	list l = from_string("[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []", 0, 0);

	printf("Nested: ");
	show_list(l);
	printf("\n");

	list flat = flatten(l, 0);
	printf("Flattened: ");
	show_list(flat);

	/* delete_list(l); delete_list(flat); */
	return 0;
}
```

{{out}}

```txt
Nested: [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
Flattened: [1, 2, 3, 4, 5, 6, 7, 8]
```



## C++


```cpp>#include <list

#include <boost/any.hpp>

typedef std::list<boost::any> anylist;

void flatten(std::list<boost::any>& list)
{
  typedef anylist::iterator iterator;

  iterator current = list.begin();
  while (current != list.end())
  {
    if (current->type() == typeid(anylist))
    {
      iterator next = current;
      ++next;
      list.splice(next, boost::any_cast<anylist&>(*current));
      current = list.erase(current);
    }
    else
      ++current;
  }
}
```


Use example:

Since C++ currently doesn't have nice syntax for initializing lists, 
this includes a simple parser to create lists of integers and sublists. 
Also, there's no standard way to output this type of list, 
so some output code is added as well.

```cpp>#include <cctype

#include <iostream>

// *******************
// * the list parser *
// *******************

void skipwhite(char const** s)
{
  while (**s && std::isspace((unsigned char)**s))
  {
    ++*s;
  }
}

anylist create_anylist_i(char const** s)
{
  anylist result;
  skipwhite(s);
  if (**s != '[')
    throw "Not a list";
  ++*s;
  while (true)
  {
    skipwhite(s);
    if (!**s)
      throw "Error";
    else if (**s == ']')
    {
      ++*s;
      return result;
    }
    else if (**s == '[')
      result.push_back(create_anylist_i(s));
    else if (std::isdigit((unsigned char)**s))
    {
      int i = 0;
      while (std::isdigit((unsigned char)**s))
      {
        i = 10*i + (**s - '0');
        ++*s;
      }
      result.push_back(i);
    }
    else
      throw "Error";

    skipwhite(s);
    if (**s != ',' && **s != ']')
      throw "Error";
    if (**s == ',')
      ++*s;
  }
}

anylist create_anylist(char const* i)
{
  return create_anylist_i(&i);
}

// *************************
// * printing nested lists *
// *************************

void print_list(anylist const& list);

void print_item(boost::any const& a)
{
  if (a.type() == typeid(int))
    std::cout << boost::any_cast<int>(a);
  else if (a.type() == typeid(anylist))
    print_list(boost::any_cast<anylist const&>(a));
  else
    std::cout << "???";
}

void print_list(anylist const& list)
{
  std::cout << '[';
  anylist::const_iterator iter = list.begin();
  while (iter != list.end())
  {
    print_item(*iter);
    ++iter;
    if (iter != list.end())
      std::cout << ", ";
  }
  std::cout << ']';
}

// ***************************
// * The actual test program *
// ***************************

int main()
{
  anylist list =
    create_anylist("[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]");
  print_list(list);
  std::cout << "\n";
  flatten(list);
  print_list(list);
  std::cout << "\n";
}
```

{{out}}

```txt

[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
[1, 2, 3, 4, 5, 6, 7, 8]

```


=={{header|C sharp|C#}}==

{{works with|C sharp|C#|3+}}

Actual Workhorse code

```csharp

using System;
using System.Collections;
using System.Linq;

namespace RosettaCodeTasks
{
	static class FlattenList
	{
		public static ArrayList Flatten(this ArrayList List)
		{
			ArrayList NewList = new ArrayList ( );

			NewList.AddRange ( List );

			while ( NewList.OfType<ArrayList> ( ).Count ( ) > 0 )
			{
				int index = NewList.IndexOf ( NewList.OfType<ArrayList> ( ).ElementAt ( 0 ) );
				ArrayList Temp = (ArrayList)NewList[index];
				NewList.RemoveAt ( index );
				NewList.InsertRange ( index, Temp );
			}
			
			return NewList;
		}
	}
}

```


Method showing population of arraylist and usage of flatten method

```csharp

using System;
using System.Collections;

namespace RosettaCodeTasks
{
	class Program
	{
		static void Main ( string[ ] args )
		{

			ArrayList Parent = new ArrayList ( );
			Parent.Add ( new ArrayList ( ) );
			((ArrayList)Parent[0]).Add ( 1 );
			Parent.Add ( 2 );
			Parent.Add ( new ArrayList ( ) );
			( (ArrayList)Parent[2] ).Add ( new ArrayList ( ) );
			( (ArrayList)( (ArrayList)Parent[2] )[0] ).Add ( 3 );
			( (ArrayList)( (ArrayList)Parent[2] )[0] ).Add ( 4 );
			( (ArrayList)Parent[2] ).Add ( 5 );
			Parent.Add ( new ArrayList ( ) );
			( (ArrayList)Parent[3] ).Add ( new ArrayList ( ) );
			( (ArrayList)( (ArrayList)Parent[3] )[0] ).Add ( new ArrayList ( ) );
			Parent.Add ( new ArrayList ( ) );
			( (ArrayList)Parent[4] ).Add ( new ArrayList ( ) );
			( (ArrayList)( (ArrayList)Parent[4] )[0] ).Add ( new ArrayList ( ) );

			( (ArrayList)( (ArrayList)( (ArrayList)( (ArrayList)Parent[4] )[0] )[0] ) ).Add ( 6 );
			Parent.Add ( 7 );
			Parent.Add ( 8 );
			Parent.Add ( new ArrayList ( ) );


			foreach ( Object o in Parent.Flatten ( ) )
			{
				Console.WriteLine ( o.ToString ( ) );
			}
		}

	}
}


```


{{works with|C sharp|C#|4+}}


```csharp

	public static class Ex {
		public static List<object> Flatten(this List<object> list) {

			var result = new List<object>();
			foreach (var item in list) {
				if (item is List<object>) {
					result.AddRange(Flatten(item as List<object>));
				} else {
					result.Add(item);
				}
			}
			return result;
		}
		public static string Join<T>(this List<T> list, string glue) {
			return string.Join(glue, list.Select(i => i.ToString()).ToArray());
		}
	}

	class Program {

		static void Main(string[] args) {
			var list = new List<object>{new List<object>{1}, 2, new List<object>{new List<object>{3,4}, 5}, new List<object>{new List<object>{new List<object>{}}}, new List<object>{new List<object>{new List<object>{6}}}, 7, 8, new List<object>{}};

			Console.WriteLine("[" + list.Flatten().Join(", ") + "]");
			Console.ReadLine();
		}
	}

```



## Ceylon


```Ceylon
shared void run() {
    "Lazily flatten nested streams"
    {Anything*} flatten({Anything*} stream)
        =>  stream.flatMap((element)
            =>  switch (element)
                case (is {Anything*}) flatten(element)
                else [element]);

    value list = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []];
    
    print(list);
    print(flatten(list).sequence());
}
```

{{out}}

```txt

[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
[1, 2, 3, 4, 5, 6, 7, 8]

```



## Clojure

The following returns a lazy sequence of the flattened data structure.

```lisp
(defn flatten [coll]
  (lazy-seq
    (when-let [s  (seq coll)]
      (if (coll? (first s))
        (concat (flatten (first s)) (flatten (rest s)))
        (cons (first s) (flatten (rest s)))))))
```


The built-in flatten is implemented as:


```lisp
(defn flatten [x]
  (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))
```



## CoffeeScript


```coffeescript

flatten = (arr) ->
  arr.reduce ((xs, el) ->
    if Array.isArray el
      xs.concat flatten el
    else
      xs.concat [el]), []

# test
list = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
console.log flatten list

```


Ouput:
<lang>
> coffee foo.coffee 
[ 1, 2, 3, 4, 5, 6, 7, 8 ]

```



## Common Lisp



```lisp
(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))
```

or, from Paul Graham's OnLisp,

```lisp

(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

```


Note that since, in Common Lisp, the empty list, boolean false and <code>nil</code> are the same thing, a tree of <code>nil</code> values cannot be flattened; they will disappear.

A third version that is recursive, imperative, and reasonably fast.

```lisp
(defun flatten (obj)
  (let (result)
    (labels ((grep (obj)
               (cond ((null obj) nil)
                     ((atom obj) (push obj result))
                     (t (grep (rest obj))
                        (grep (first obj))))))
      (grep obj)
      result)))
```


The following version is tail recursive and functional.

```lisp
(defun flatten (x &optional stack out)
  (cond ((consp x) (flatten (rest x) (cons (first x) stack) out))
        (x         (flatten (first stack) (rest stack) (cons x out)))
        (stack     (flatten (first stack) (rest stack) out))
        (t out)))
```


The next version is imperative, iterative and does not make use of a stack. It is faster than the versions given above.

```lisp
(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))
```

The above implementations of flatten give the same output on nested proper lists.
{{Out}}

```txt
CL-USER> (flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))
(1 2 3 4 5 6 7 8)
```


It should be noted that there are several choices that can be made when implementing flatten in Common Lisp:

-- should it work on dotted pairs?

-- should it work with non-nil atoms, presumably returning the atom or a copy of the atom?

-- when it comes to nil, should it be considered as an empty list and removed, or should it be considered as an atom and preserved?

So there are in fact several slightly different functions that correspond to flatten in common lisp. They may

-- collect all atoms, including nil,

-- collect all atoms in the car of the cons cells,

-- collect all atoms which are not in the cdr of a cell,

-- collect all non-nil atoms.

Which version is suitable for a given problem depends of course on the nature of the problem.


## Crystal


```Ruby

[[1], 2, [[3, 4], 5], [[[] of Int32]], [[[6]]], 7, 8, [] of Int32].flatten()

```


```Bash

[1, 2, 3, 4, 5, 6, 7, 8]

```



## D

Instead of a Java-like class-based version, this version minimizes heap activity using a tagged union.

```d
import std.stdio, std.algorithm, std.conv, std.range;

struct TreeList(T) {
    union { // A tagged union
        TreeList[] arr; // it's a node
        T data; // It's a leaf.
    }
    bool isArray = true; // = Contains an arr on default.

    static TreeList opCall(A...)(A items) pure nothrow {
        TreeList result;

        foreach (i, el; items)
            static if (is(A[i] == T)) {
                TreeList item;
                item.isArray = false;
                item.data = el;
                result.arr ~= item;
            } else
                result.arr ~= el;

        return result;
    }

    string toString() const pure {
        return isArray ? arr.text : data.text;
    }
}

T[] flatten(T)(in TreeList!T t) pure nothrow {
    if (t.isArray)
        return t.arr.map!flatten.join;
    else
        return [t.data];
}

void main() {
    alias TreeList!int L;
    static assert(L.sizeof == 12);
    auto l = L(L(1), 2, L(L(3,4), 5), L(L(L())), L(L(L(6))),7,8,L());
    l.writeln;
    l.flatten.writeln;
}
```

{{out}}

```txt
[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
[1, 2, 3, 4, 5, 6, 7, 8]
```


### With an Algebraic Data Type

A shorter and more cryptic version.

```d
import std.stdio, std.variant, std.range, std.algorithm;

alias T = Algebraic!(int, This[]);

int[] flatten(T t) {
    return t.peek!int ? [t.get!int] : t.get!(T[])().map!flatten.join;
}

void main() {
    T([T([ T(1) ]),
       T(2),
       T([ T([ T(3), T(4) ]), T(5) ]),
       T([ T([ T( T[].init ) ]) ]),
       T([ T([ T([ T(6) ]) ]) ]),
       T(7),
       T(8),
       T( T[].init )
      ]).flatten.writeln;
}
```

{{out}}
 [1, 2, 3, 4, 5, 6, 7, 8]

=={{header|Déjà Vu}}==

```dejavu
(flatten):
	for i in copy:
		i
		if = :list type dup:
			(flatten)

flatten l:
	[ (flatten) l ]


!. flatten [ [ 1 ] 2 [ [ 3 4 ] 5 ] [ [ [] ] ] [ [ [ 6 ] ] ] 7 8 [] ]
```

{{out}}

```txt
[ 1 2 3 4 5 6 7 8 ]
```



## E



```e
def flatten(nested) {
    def flat := [].diverge()
    def recur(x) {
        switch (x) {
            match list :List { for elem in list { recur(elem) } }
            match other      { flat.push(other) }
        }
    }
    recur(nested)
    return flat.snapshot()
}
```



```e
? flatten([[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []])
# value: [1, 2, 3, 4, 5, 6, 7, 8]
```



## EchoLisp

The built-in '''(flatten list)''' is defined as follows:

```lisp

(define (fflatten l)
(cond
[(null? l) null]
[(not (list? l)) (list l)]
[else (append (fflatten (first l)) (fflatten (rest l)))]))

;;
(define L' [[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []])

(fflatten L) ;; use custom function
 → (1 2 3 4 5 6 7 8)
(flatten L) ;; use built-in
 → (1 2 3 4 5 6 7 8)

;; Remarks
;; null is the same as () - the empty list - 
(flatten '(null null null))
   → null
(flatten '[ () () () ])
  → null
(flatten null)
❗ error: flatten : expected list : null

;; The 'reverse' of flatten is group
(group '( 4 5 5 5 6 6 7 8 7 7 7 9))
    → ((4) (5 5 5) (6 6) (7) (8) (7 7 7) (9))


```



## Ela


This implementation can flattern any given list:


```Ela
xs =  [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
 
flat = flat' []
       where flat' n [] = n
             flat' n (x::xs) 
               | x is List = flat' (flat' n xs) x
               | else = x :: flat' n xs

flat xs
```


{{out}}

```txt
[1,2,3,4,5,6,7,8]
```


An alternative solution:


```Ela
flat [] = [] 
flat (x::xs) 
  | x is List = flat x ++ flat xs
  | else = x :: flat xs
```



## Elixir


```elixir

defmodule RC do
  def flatten([]), do: []
  def flatten([h|t]), do: flatten(h) ++ flatten(t)
  def flatten(h), do: [h] 
end

list = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []] 

# Our own implementation
IO.inspect RC.flatten(list)
# Library function
IO.inspect List.flatten(list)

```

{{out}}

```txt

[1, 2, 3, 4, 5, 6, 7, 8]
[1, 2, 3, 4, 5, 6, 7, 8]

```



## Elm



```haskell

import Graphics.Element exposing (show)

type Tree a
  = Leaf a
  | Node (List (Tree a))

flatten : Tree a -> List a
flatten tree =
  case tree of
    Leaf a -> [a]
    Node list -> List.concatMap flatten list

-- [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
tree : Tree Int
tree = Node
  [ Node [Leaf 1]
  , Leaf 2
  , Node [Node [Leaf 3, Leaf 4], Leaf 5]
  , Node [Node [Node []]]
  , Node [Node [Node [Leaf 6]]]
  , Leaf 7
  , Leaf 8
  , Node []
  ]

main =
  show (flatten tree)

```



## Emacs Lisp


```lisp

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist)) (flatten (cdr mylist))))))

```



## Erlang

There's a standard function (lists:flatten/1) that does it more efficiently, but this is the cleanest implementation you could have;

```Erlang
flatten([]) -> [];
flatten([H|T]) -> flatten(H) ++ flatten(T);
flatten(H) -> [H].
```



## Euphoria

{{works with|Euphoria|4.0.0}}

```Euphoria
sequence a = {{1}, 2, {{3, 4}, 5}, {{{}}}, {{{6}}}, 7, 8, {}}

function flatten( object s )
	sequence res = {}
	if sequence( s ) then
		for i = 1 to length( s ) do
			sequence c = flatten( s[ i ] )
			if length( c ) > 0 then
				res &= c 
			end if
		end for
	else
		if length( s ) > 0 then 
			res = { s }  
		end if
	end if
	return res
end function

? a
? flatten(a)
```

{{out}}

```txt
{
  {1},
  2,
  {
    {3,4},
    5
  },
  {
    {{}}
  },
  {
    {
      {6}
    }
  },
  7,
  8,
  {}
}
{1,2,3,4,5,6,7,8}
```


=={{header|F_Sharp|F#}}==
As with Haskell and OCaml we have to define our list as an algebraic data type, to be strongly typed:

```fsharp
type 'a ll =
    | I of 'a             // leaf Item
    | L of 'a ll list     // ' <- confine the syntax colouring confusion

let rec flatten = function
    | [] -> []
    | (I x)::y -> x :: (flatten y)
    | (L x)::y -> List.append (flatten x) (flatten y)

printfn "%A" (flatten [L([I(1)]); I(2); L([L([I(3);I(4)]); I(5)]); L([L([L([])])]); L([L([L([I(6)])])]); I(7); I(8); L([])])

// -> [1; 2; 3; 4; 5; 6; 7; 8]
```


An alternative approach with List.collect
and the same data type. Note that flatten operates on all deepLists (ll) and atoms (I) are "flatened" to lists.


```fsharp

let rec flatten =
    function
    | I x -> [x]
    | L x -> List.collect flatten x

printfn "%A" (flatten (L [L([I(1)]); I(2); L([L([I(3);I(4)]); I(5)]); L([L([L([])])]); L([L([L([I(6)])])]); I(7); I(8); L([])]))

// -> [1; 2; 3; 4; 5; 6; 7; 8]

```



## Factor

    USE: sequences.deep
    ( scratchpad ) { { 1 } 2 { { 3 4 } 5 } { { { } } } { { { 6 } } } 7 8 { } } flatten .
    { 1 2 3 4 5 6 7 8 }


## Fantom



```fantom

class Main
{ 
  // uses recursion to flatten a list
  static List myflatten (List items)
  {
    List result := [,]
    items.each |item|
    {
      if (item is List)
        result.addAll (myflatten(item))
      else
        result.add (item)
    }
    return result
  }
  
  public static Void main ()
  {
    List sample := [[1], 2, [[3,4], 5], [[[,]]], [[[6]]], 7, 8, [,]]
    // there is a built-in flatten method for lists
    echo ("Flattened list 1: " + sample.flatten)
    // or use the function 'myflatten'
    echo ("Flattened list 2: " + myflatten (sample))
  }
}

```



## Forth

{{works with|Forth}}
Works with any ANS Forth.
Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f
include FMS-SILib.f

: flatten {: list1 list2 --  :}
  list1 size: 0 ?do i list1 at: 
                  dup is-a object-list2
                  if list2 recurse else list2 add: then  loop ;

object-list2 list 
o{ o{ 1 } 2 o{ o{ 3 4 } 5 } o{ o{ o{ } } } o{ o{ o{ 6 } } } 7 8 o{ } } 
list flatten
list p: \ o{ 1 2 3 4 5 6 7 8 } ok
```



## Fortran



```fortran

! input   : [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
! flatten : [1, 2, 3, 4, 5, 6, 7, 8 ]

module flat
  implicit none

  type n
     integer                             :: a
     type(n), dimension(:), pointer      :: p => null()
     logical                             :: empty = .false.
  end type

contains

  recursive subroutine del(this)
  type(n), intent(inout) :: this
  integer                :: i
  if (associated(this%p)) then
    do i = 1, size(this%p)
       call del(this%p(i))
    end do
  end if
  end subroutine

  function join(xs) result (r)
  type(n), dimension(:), target :: xs
  type(n)                       :: r
  integer                       :: i
  if (size(xs)>0) then
    allocate(r%p(size(xs)), source=xs)
    do i = 1, size(xs)
      r%p(i) = xs(i)
    end do
  else
    r%empty = .true.
  end if
  end function

  recursive subroutine flatten1(x,r) 
  integer, dimension (:), allocatable, intent(inout) :: r
  type(n), intent(in)                                :: x
  integer, dimension (:), allocatable                :: tmp
  integer                                            :: i
  if (associated(x%p)) then
    do i = 1, size(x%p)
      call flatten1(x%p(i), r)
    end do
  elseif (.not. x%empty) then
    allocate(tmp(size(r)+1))
    tmp(1:size(r)) = r
    tmp(size(r)+1) = x%a
    call move_alloc(tmp, r)
  end if
  end subroutine

  function flatten(x) result (r)
  type(n), intent(in)                                :: x
  integer, dimension(:), allocatable                 :: r
  allocate(r(0))
  call flatten1(x,r)
  end function

  recursive subroutine show(x)
  type(n)   :: x
  integer   :: i
  if (x%empty) then 
    write (*, "(a)", advance="no") "[]"
  elseif (associated(x%p)) then
    write (*, "(a)", advance="no") "["
    do i = 1, size(x%p)
      call show(x%p(i))
      if (i<size(x%p)) then
        write (*, "(a)", advance="no") ", "
      end if
    end do
    write (*, "(a)", advance="no") "]"
  else
    write (*, "(g0)", advance="no") x%a
  end if
  end subroutine

  function fromString(line) result (r)
  character(len=*)                      :: line
  type (n)                              :: r
  type (n), dimension(:), allocatable   :: buffer, buffer1
  integer, dimension(:), allocatable    :: stack, stack1
  integer                               :: sp,i0,i,j, a, cur, start
  character                             :: c
 
  if (.not. allocated(buffer)) then
    allocate (buffer(5)) ! will be re-allocated if more is needed
  end if
  if (.not. allocated(stack)) then
    allocate (stack(5))
  end if

  sp = 1; cur = 1; i = 1
  do
    if ( i > len_trim(line) ) exit
    c = line(i:i)
    if (c=="[") then
      if (sp>size(stack)) then 
        allocate(stack1(2*size(stack)))
        stack1(1:size(stack)) = stack
        call move_alloc(stack1, stack)
      end if
      stack(sp) = cur;  sp = sp + 1; i = i+1
    elseif (c=="]") then
      sp = sp - 1; start = stack(sp)
      r = join(buffer(start:cur-1))
      do j = start, cur-1
        call del(buffer(j))
      end do
      buffer(start) = r; cur = start+1; i = i+1
    elseif (index(" ,",c)>0) then
      i = i + 1; continue
    elseif (index("-123456789",c)>0) then
      i0 = i
      do 
        if ((i>len_trim(line)).or. &
            index("1234567890",line(i:i))==0) then
          read(line(i0:i-1),*) a
          if (cur>size(buffer)) then
            allocate(buffer1(2*size(buffer)))
            buffer1(1:size(buffer)) = buffer
            call move_alloc(buffer1, buffer)
          end if
          buffer(cur) = n(a); cur = cur + 1; exit
        else
          i = i+1
        end if
      end do
    else
       stop "input corrupted"
    end if
  end do
  end function
end module

program main
  use flat
  type (n)  :: x
  x = fromString("[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]")
  write(*, "(a)", advance="no") "input   : "
  call show(x)
  print *
  write (*,"(a)", advance="no") "flatten : ["
  write (*, "(*(i0,:,:', '))", advance="no") flatten(x)
  print *, "]"
end program

```


===Or, older style===
Fortran does not offer strings, only CHARACTER variables of some fixed size. Functions can return such types, but, must specify a fixed size. Or, mess about with run-time allocation as above. Since in principle a list is arbitrarily long, the plan here is to crush its content in place, and thereby never have to worry about long-enough work areas. This works because the transformations in mind never replace something by something longer. A subroutine can receive an arbitrary-sized CHARACTER variable, and can change it. No attempt is made to detect improper lists.

```Fortran

      SUBROUTINE CRUSH(LIST)	!Changes LIST.
Crushes a list holding multi-level entries within [...] to a list of single-level entries. Null entries are purged.
Could escalate to recognising quoted strings as list entries (preserving spaces), not just strings of digits.
       CHARACTER*(*) LIST	!The text manifesting the list.
       INTEGER I,L		!Fingers.
       LOGICAL LIVE		!Scan state.
        L = 1		!Output finger. The starting [ is already in place.
        LIVE = .FALSE.	!A list element is not in progress.
        DO I = 2,LEN(LIST)	!Scan the characters of the list.
          SELECT CASE(LIST(I:I))	!Consider one.
           CASE("[","]",","," ")	!Punctuation or spacing?
            IF (LIVE) THEN		!Yes. If previously in an element,
              L = L + 1			!Advance the finger,
              LIST(L:L) = ","		!And place its terminating comma.
              LIVE = .FALSE.		!Thus the element is finished.
            END IF		!So much for punctuation and empty space.
           CASE DEFAULT		!Everything else is an element's content.
            LIVE = .TRUE.		!So we're in an element.
            L = L + 1			!Advance the finger.
            LIST(L:L) = LIST(I:I)	!And copy the content's character.
          END SELECT		!Either we're in an element, or, we're not.
        END DO			!On to the next character.
Completed the crush. At least one ] must have followed the last character of the last element.
        LIST(L:L) = "]"		!It had provoked a trailing comma. Now it is the ending ].
        LIST(L + 1:) = ""	!Scrub any tail end, just to be neat.
      END		!Trailing spaces are the caller's problem.

      CHARACTER*88 STUFF	!Work area.
      STUFF = "[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]"	!The example.
      WRITE (6,*) "Original: ",STUFF
      CALL CRUSH(STUFF)		!Can't be a constant, as it will be changed.
      WRITE (6,*) " Crushed: ",STUFF	!Behold!
      END
```

Output is 
 Original: [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
  Crushed: [1,2,3,4,5,6,7,8]
Note that if you insist on the rather flabby style of having spaces after commas, then there would be trouble. Instead of placing just a comma, a ", " would be required, which is ''two'' symbols going out when ''one'' symbol has come in: overwriting yet-to-be-scanned input is a bad idea. Either a more complex set of scan states would be required to squeeze in the extra or a separate work area would be needed to hold such output and the issue of "long enough" would arise.

All of this relies on the list being presented as a flat text, which text is then manipulated directly. If the list was manifested in a data structure of some kind with links and suchlike, then tree-traversal of that structure would be needed to reach the leaf entries.


## Frink


```frink

a = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
println[flatten[a]]

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=1c0157ce2b7eab99ba4e784e183ba474 Click this link to run this code]'''

```gambas
'Code 'borrowed' from Run BASIC

Public Sub Main()
Dim sComma, sString, sFlatter As String
Dim siCount As Short

sString = "[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8 []]"
For siCount = 1 To Len(sString)
 If InStr("[] ,", Mid$(sString, siCount, 1)) = 0 Then 
  sFlatter = sFlatter & sComma & Mid(sString, siCount, 1)
  sComma = ","
 End If
Next
Print "["; sFlatter; "]"

End
```

Output:

```txt

[1,2,3,4,5,6,7,8]

```



## GAP


```gap
Flat([[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]);
```



## Go


```go
package main

import "fmt"

func list(s ...interface{}) []interface{} {
    return s
}

func main() {
    s := list(list(1),
        2,
        list(list(3, 4), 5),
        list(list(list())),
        list(list(list(6))),
        7,
        8,
        list(),
    )
    fmt.Println(s)
    fmt.Println(flatten(s))
}

func flatten(s []interface{}) (r []int) {
    for _, e := range s {
        switch i := e.(type) {
        case int:
            r = append(r, i)
        case []interface{}:
            r = append(r, flatten(i)...)
        }
    }
    return
}
```

{{out}}

```txt

[[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []]
[1 2 3 4 5 6 7 8]

```

In the code above, flatten uses an easy-to-read type switch to extract ints and return an int slice.  The version below is generalized to return a flattened slice of interface{} type, which can of course refer to objects of any type, and not just int.  
Also, just to show a variation in programming style, a type assertion is used rather than a type switch.

```go
func flatten(s []interface{}) (r []interface{}) {
    for _, e := range s {
        if i, ok := e.([]interface{}); ok {
            r = append(r, flatten(i)...)
        } else {
            r = append(r, e)
        }
    }
    return
}
```



## Groovy


<code>List.flatten()</code> is a Groovy built-in that returns a flattened copy of the source list:


```groovy
assert [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []].flatten() == [1, 2, 3, 4, 5, 6, 7, 8]
```



## Haskell


In Haskell we have to interpret this structure as an algebraic data type.


```Haskell
import Data.Tree (Tree(..), flatten)

-- [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
-- implemented as multiway tree:
-- Data.Tree represents trees where nodes have values too, unlike the trees in our problem.
-- so we use a list as that value, where a node will have an empty list value,
-- and a leaf will have a one-element list value and no subtrees
list :: Tree [Int]
list =
  Node
    []
    [ Node [] [Node [1] []]
    , Node [2] []
    , Node [] [Node [] [Node [3] [], Node [4] []], Node [5] []]
    , Node [] [Node [] [Node [] []]]
    , Node [] [Node [] [Node [6] []]]
    , Node [7] []
    , Node [8] []
    , Node [] []
    ]

flattenList :: Tree [a] -> [a]
flattenList = concat . flatten

main :: IO ()
main = print $ flattenList list
```

{{Out}}

```txt
[1,2,3,4,5,6,7,8]
```


Alternately:

```haskell
data Tree a
  = Leaf a
  | Node [Tree a]
 
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node xs) = xs >>= flatten
 
main :: IO ()
main =
  (print . flatten) $
  Node
    [ Node [Leaf 1]
    , Leaf 2
    , Node [Node [Leaf 3, Leaf 4], Leaf 5]
    , Node [Node [Node []]]
    , Node [Node [Node [Leaf 6]]]
    , Leaf 7
    , Leaf 8
    , Node []
    ]
    
-- [1,2,3,4,5,6,7,8]
```


Yet another choice, custom data structure, efficient lazy flattening:

(This is unnecessary; since Haskell is lazy, the previous solution will only do just as much work as necessary for each element that is requested from the resulting list.)


```haskell
data NestedList a
  = NList [NestedList a]
  | Entry a

flatten :: NestedList a -> [a]
flatten nl = flatten_ nl []
  where
    flatten_ :: NestedList a -> [a] -> [a]
    flatten_ (Entry a) cont = a : cont
    flatten_ (NList entries) cont = foldr flatten_ cont entries

-- By passing through a list to which the results will be prepended,
-- we allow for efficient lazy evaluation
example :: NestedList Int
example =
  NList
    [ NList [Entry 1]
    , Entry 2
    , NList [NList [Entry 3, Entry 4], Entry 5]
    , NList [NList [NList []]]
    , NList [NList [NList [Entry 6]]]
    , Entry 7
    , Entry 8
    , NList []
    ]

main :: IO ()
main = print $ flatten example
-- output [1,2,3,4,5,6,7,8]
```



## Hy


```clojure
(defn flatten [lst]
  (sum (genexpr (if (isinstance x list)
                    (flatten x)
                    [x])
                [x lst])
       []))

(print (flatten [[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []]))
; [1, 2, 3, 4, 5, 6, 7, 8]
```


=={{header|Icon}} and {{header|Unicon}}==
The following procedure solves the task using a string representation of nested lists and cares not if the list is well formed or not.  

```Icon
link strings           # for compress,deletec,pretrim

procedure sflatten(s)  # uninteresting string solution
return pretrim(trim(compress(deletec(s,'[ ]'),',') ,','),',')       
end
```

{{libheader|Icon Programming Library}} 
The solution uses several procedures from [http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings in the IPL]

This procedure is more in the spirit of the task handling actual lists rather than representations.  It uses a recursive approach using some of the built-in list manipulation functions and operators.

```Icon
procedure flatten(L)   # in the spirt of the problem  a structure
local l,x

l := []
every x := !L do
   if type(x) == "list" then l |||:= flatten(x)
   else put(l,x)
return l
end
```


Finally a demo routine to drive these and a helper to show how it works.

```Icon
procedure main()
write(sflatten(" [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]"))
writelist(flatten( [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]))
end

procedure writelist(L)         
writes("[")
every writes(" ",image(!L))
write(" ]")
return
end
```



## Ioke


```ioke>iik
 [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []] flatten
[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []] flatten
+> [1, 2, 3, 4, 5, 6, 7, 8]
```



## J


'''Solution''':  

```j>flatten =: [: ; <S:0</lang


'''Example''':

```j
   NB. create and display nested noun li
   ]li =.  (<1) ; 2; ((<3; 4); 5) ; ((<a:)) ; ((<(<6))) ; 7; 8; <a:
+---+-+-----------+----+-----+-+-+--+
|+-+|2|+-------+-+|+--+|+---+|7|8|++|
||1|| ||+-----+|5|||++|||+-+|| | ||||
|+-+| |||+-+-+|| |||||||||6||| | |++|
|   | ||||3|4||| |||++|||+-+|| | |  |
|   | |||+-+-+|| ||+--+|+---+| | |  |
|   | ||+-----+| ||    |     | | |  |
|   | |+-------+-+|    |     | | |  |
+---+-+-----------+----+-----+-+-+--+

  flatten li
1 2 3 4 5 6 7 8
```


'''Notes:'''
The primitive <code>;</code> removes one level of nesting.

<code><S:0</code> takes an arbitrarily nested list and puts everything one level deep.

<code>[:</code> is glue, here.

We do not use <code>;</code> by itself because it requires that all of the contents be the same type and nested items have a different type from unnested items.

We do not use <code>]S:0</code> (which puts everything zero levels deep) because it assembles its results as items of a list, which means that short items will be padded to be equal to the largest items, and that is not what we would want here (we do not want the empty item to be padded with a fill element).

'''Alternative Solution:'''

The previous solution can be generalized to flatten the nesting and shape for a list of arbitrary values that include arrays of any rank:

```j
flatten2 =: [: ; <@,S:0
```


'''Example:'''

```j
   ]li2 =.  (<1) ; 2; ((<3;4); 5 + i.3 4) ; ((<a:)) ; ((<(<17))) ; 18; 19; <a:
+---+-+---------------------+----+------+--+--+--+
|+-+|2|+-------+-----------+|+--+|+----+|18|19|++|
||1|| ||+-----+| 5  6  7  8|||++|||+--+||  |  ||||
|+-+| |||+-+-+|| 9 10 11 12|||||||||17|||  |  |++|
|   | ||||3|4|||13 14 15 16|||++|||+--+||  |  |  |
|   | |||+-+-+||           ||+--+|+----+|  |  |  |
|   | ||+-----+|           ||    |      |  |  |  |
|   | |+-------+-----------+|    |      |  |  |  |
+---+-+---------------------+----+------+--+--+--+

   flatten2 li
1 2 3 4 5 6 7 8
   flatten2 li2
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
```


Here, we have replaced <code><S:0</code> with <code><@,S:0</code> so our leaves are flattened before the final step where their boxes are razed.


## Java

{{works with|Java|1.5+}}

The <code>flatten</code> method was overloaded for better separation of concerns. On the first one you can pass any <code>List</code> and get it flat into a <code>LinkedList</code> implementation. On the other one you can pass any <code>List</code> implementation you like for both lists.

Note that both implementations can only put the result into type <code>List<Object></code>. We cannot type-safely put the result into a generic type <code>List<T></code> because there is no way to enforce that the original list contains elements of "type T or lists of elements which are T or further lists..."; there is no generic type parameter that will express that restriction. Since we must accept lists of any elements as an argument, we can only safely put them in a <code>List<Object></code>.

Actual Workhorse code

```java5
import java.util.LinkedList;
import java.util.List;


public final class FlattenUtil {

	public static List<Object> flatten(List<?> list) {
		List<Object> retVal = new LinkedList<Object>();
		flatten(list, retVal);
		return retVal;
	}

	public static void flatten(List<?> fromTreeList, List<Object> toFlatList) {
		for (Object item : fromTreeList) {
			if (item instanceof List<?>) {
				flatten((List<?>) item, toFlatList);
			} else {
				toFlatList.add(item);
			}
		}
	}
}
```


Method showing population of the test List and usage of flatten method.

```java5
import static java.util.Arrays.asList;
import java.util.List;

public final class FlattenTestMain {

	public static void main(String[] args) {
		List<Object> treeList = a(a(1), 2, a(a(3, 4), 5), a(a(a())), a(a(a(6))), 7, 8, a());
		List<Object> flatList = FlattenUtil.flatten(treeList);
		System.out.println(treeList);
		System.out.println("flatten: " + flatList);
	}
	
	private static List<Object> a(Object... a) {
		return asList(a);
	}
}
```


{{out}}

```txt
[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
flatten: [1, 2, 3, 4, 5, 6, 7, 8]
```


;Functional version
{{works with|Java|8+}}

```java5
import java.util.List;
import java.util.stream.Stream;
import java.util.stream.Collectors;

public final class FlattenUtil {

	public static Stream<Object> flattenToStream(List<?> list) {
		return list.stream().flatMap(item ->
			item instanceof List<?> ?
			flattenToStream((List<?>)item) :
			Stream.of(item));
	}

	public static List<Object> flatten(List<?> list) {
		return flattenToStream(list).collect(Collectors.toList());
	}
}
```



## JavaScript


### ES5


```javascript
function flatten(list) {
  return list.reduce(function (acc, val) {
    return acc.concat(val.constructor === Array ? flatten(val) : val);
  }, []);
}
```



Or, expressed in terms of the more generic '''concatMap''' function:


```JavaScript
(function () {
    'use strict';

    // flatten :: Tree a -> [a]
    function flatten(t) {
        return (t instanceof Array ? concatMap(flatten, t) : t);
    }

    // concatMap :: (a -> [b]) -> [a] -> [b]
    function concatMap(f, xs) {
        return [].concat.apply([], xs.map(f));
    }

    return flatten(
        [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
    );

})();
```



From fusion of ''flatten'' with ''concatMap'' we can then derive:


```JavaScript
    // flatten :: Tree a -> [a]
    function flatten(a) {
        return a instanceof Array ? [].concat.apply([], a.map(flatten)) : a;
    }
```


For example:


```JavaScript
(function () {
    'use strict';

    // flatten :: Tree a -> [a]
    function flatten(a) {
        return a instanceof Array ? [].concat.apply([], a.map(flatten)) : a;
    }

    return flatten(
        [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
    );

})();
```


{{Out}}


```txt
[1, 2, 3, 4, 5, 6, 7, 8]
```



### ES6


====Built-in====

```javascript
// flatten :: NestedList a -> [a]
const flatten = nest => nest.flat(Infinity);
```



### =Recursive=


```javascript
// flatten :: Nested List a -> [a]
const flatten = t => {
    const go = x => Array.isArray(x) ? x.flatMap(go) : x
    return go(t);
};
```



### =Iterative=


```javascript
function flatten(list) {
  for (let i = 0; i < list.length; i++) {
    while (true) {
      if (Array.isArray(list[i])) {
      	list.splice(i, 1, ...list[i]);
      } else {
      	break;
      }
    }
  }
  return list;
}
```


Or alternatively:


```javascript
// flatten :: Nested List a -> a
const flatten = t => {
    let xs = t;
    while (xs.some(Array.isArray)) (
        xs = [].concat(...xs)
    )
    return xs;
};
```


Result is always: 

```txt
[1, 2, 3, 4, 5, 6, 7, 8]
```



## Joy


```Joy

"seqlib" libload.

[[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []] treeflatten.

(* output: [1 2 3 4 5 6 7 8] *)

```



## jq

Recent (1.4+) versions of jq include the following flatten filter:
```jq
def flatten:
   reduce .[] as $i
     ([];
     if $i | type == "array" then . + ($i | flatten)
     else . + [$i]
     end);
```
Example:
```jq

[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []] | flatten
[1,2,3,4,5,6,7,8]
```



## Jsish

From Javascript entry, with change to test for ''typeof'' equal ''"array"''.


```javascript
/* Flatten list, in Jsish */
function flatten(list) {
  return list.reduce(function (acc, val) {
    return acc.concat(typeof val === "array" ? flatten(val) : val);
  }, []);
}

if (Interp.conf('unitTest')) {
;   flatten([[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]);
}

/*
=!EXPECTSTART!=
flatten([[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]) ==> [ 1, 2, 3, 4, 5, 6, 7, 8 ]
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U flatten.jsi
flatten([[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]) ==> [ 1, 2, 3, 4, 5, 6, 7, 8 ]
```



## Julia

Note that Julia versions prior to 0.5 auto-flattened nested arrays. The following version of flatten makes use of the higher order function ''mapreduce''.

```julia
using BenchmarkTools

flat(arr) = mapreduce(x -> x == [] || x[1] === x ? x : flat(x), vcat, arr, init=[])

```

An iterative recursive version that uses less memory but is slower:

```julia
function flat1(arr)
    rst = Any[]
    grep(v) = for x in v
        if isa(x, Array) grep(x) else push!(rst, x) end
    end
    grep(arr)
    rst
end

```

Using the Julia standard Iterators library:

```julia

flat2(arr) = (while any(a -> a isa Vector, arr) arr = collect(Iterators.flatten(arr)) end; arr)

arr = [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]

@show flat(arr)
@show flat1(arr)
@show flat2(arr)

@btime flat(arr)
@btime flat1(arr)
@btime flat2(arr)

```
{{out}}

```txt

flat(arr) = Any[1, 2, 3, 4, 5, 6, 7, 8]
flat1(arr) = Any[1, 2, 3, 4, 5, 6, 7, 8]
flat2(arr) = [1, 2, 3, 4, 5, 6, 7, 8]
  17.200 μs (193 allocations: 9.44 KiB)
  504.145 ns (5 allocations: 256 bytes)
  36.699 μs (106 allocations: 3.73 KiB)

```



## K

In K, join is: <code>,</code> and reduce/fold (called "over") is: <code>/</code>. With a monadic argument (as ,/ is), over repeats application until reaching a fixed-point.

So to flatten a list of arbitrary depth, you can join-over-over, or reduce a list with a function that reduces a list with a join function:

```k
,//((1); 2; ((3;4); 5); ((())); (((6))); 7; 8; ())
```



## Kotlin


```scala
// version 1.0.6

@Suppress("UNCHECKED_CAST")

fun flattenList(nestList: List<Any>, flatList: MutableList<Int>) {
    for (e in nestList)
        if (e is Int)
            flatList.add(e)
        else
            // using unchecked cast here as can't check for instance of 'erased' generic type
            flattenList(e as List<Any>, flatList) 
}
            
fun main(args: Array<String>) {
    val nestList : List<Any> = listOf(
        listOf(1),
        2,
        listOf(listOf(3, 4), 5),
        listOf(listOf(listOf<Int>())),
        listOf(listOf(listOf(6))),
        7,
        8,
        listOf<Int>()
    )
    println("Nested    : " + nestList)
    val flatList = mutableListOf<Int>()
    flattenList(nestList, flatList)
    println("Flattened : " + flatList)    
}
```


Or, using a more functional approach:


```scala

fun flatten(list: List<*>): List<*> {
    fun flattenElement(elem: Any?): Iterable<*> {
        return if (elem is List<*>)
            if (elem.isEmpty()) elem
            else flattenElement(elem.first()) + flattenElement(elem.drop(1))
        else listOf(elem)
    }
    return list.flatMap { elem -> flattenElement(elem) }
}
```


{{out}}

```txt

Nested    : [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
Flattened : [1, 2, 3, 4, 5, 6, 7, 8]

```



## Lasso

Lasso Delve is a Lasso utility method explicitly for handling embedded arrays. With one array which contain other arrays, delve allows you to treat one array as a single series of elements, thus enabling easy access to an entire tree of values. [http://www.lassosoft.com/lassoDocs/languageReference/obj/delve www.lassosoft.com/lassoDocs/languageReference/obj/delve Lasso reference on Delve]


```Lasso
local(original = json_deserialize('[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]'))

#original
'<br />'
(with item in delve(#original)
select #item) -> asstaticarray
```


```txt
array(array(1), 2, array(array(3, 4), 5), array(array(array())), array(array(array(6))), 7, 8, array())
staticarray(1, 2, 3, 4, 5, 6, 7, 8)
```



## LFE


```lisp

> (: lists flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))
(1 2 3 4 5 6 7 8)

```



## Logtalk


```logtalk
flatten(List, Flatted) :-
    flatten(List, [], Flatted).

flatten(Var, Tail, [Var| Tail]) :-
    var(Var),
    !.
flatten([], Flatted, Flatted) :-
    !.
flatten([Head| Tail], List, Flatted) :-
    !,
    flatten(Tail, List, Aux),
    flatten(Head, Aux, Flatted).
flatten(Head, Tail, [Head| Tail]).
```



## Lua



```lua
function flatten(list)
  if type(list) ~= "table" then return {list} end
  local flat_list = {}
  for _, elem in ipairs(list) do
    for _, val in ipairs(flatten(elem)) do
      flat_list[#flat_list + 1] = val
    end
  end
  return flat_list
end

test_list = {{1}, 2, {{3,4}, 5}, {{{}}}, {{{6}}}, 7, 8, {}}

print(table.concat(flatten(test_list), ","))
```



## Logo


```logo
to flatten :l
  if not list? :l [output :l]
  if empty? :l [output []]
  output sentence flatten first :l flatten butfirst :l
end

; using a template iterator (map combining results into a sentence)
to flatten :l
  output map.se [ifelse or not list? ? empty? ? [?] [flatten ?]] :l
end

make "a [[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []]
show flatten :a
```




## Maple


This can be accomplished using the <code>Flatten</code> command from the <code>ListTools</code>, or with a custom recursive procedure.


```Maple

L := [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]:

with(ListTools):

Flatten(L);

```

{{out}}

```txt

                          [1, 2, 3, 4, 5, 6, 7, 8]

```



```Maple

flatten := proc(x)
  `if`(type(x,'list'),seq(procname(i),i = x),x);
end proc:

L := [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]:

[flatten(L)];

```

{{out}}

```txt

                          [1, 2, 3, 4, 5, 6, 7, 8]

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Flatten[{{1}, 2, {{3, 4}, 5}, {{{}}}, {{{6}}}, 7, 8, {}}]
```



## Maxima


```maxima
flatten([[[1, 2, 3], 4, [5, [6, 7]], 8], [[9, 10], 11], 12]);
/* [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12] */
```



## Mercury

As with Haskell we need to use an algebraic data type.

```mercury
:- module flatten_a_list.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- type tree(T)
    --->    leaf(T)
    ;       node(list(tree(T))).

:- func flatten(tree(T)) = list(T).

flatten(leaf(X)) = [X].
flatten(node(Xs)) = condense(map(flatten, Xs)).

main(!IO) :-
    List = node([
        node([leaf(1)]),
        leaf(2),
        node([node([leaf(3), leaf(4)]), leaf(5)]),
        node([node([node([])])]),
        node([node([node([leaf(6)])])]),
        leaf(7),
        leaf(8),
        node([])
    ]),
    io.print_line(flatten(List), !IO).

:- end_module flatten_a_list.

```

{{out}}

```txt

    [1, 2, 3, 4, 5, 6, 7, 8]

```



## Mirah


```mirah
import java.util.ArrayList
import java.util.List
import java.util.Collection

def flatten(list: Collection) 
    flatten(list, ArrayList.new)
end
def flatten(source: Collection, result: List)

    source.each do |x|
        if x.kind_of?(Collection) 
            flatten(Collection(x), result)  
        else
            result.add(x)
            result  # if branches must return same type
        end 
    end
    result
end

# creating a list-of-list-of-list fails currently, so constructor calls are needed
source = [[1], 2, [[3, 4], 5], [[ArrayList.new]], [[[6]]], 7, 8, ArrayList.new]

puts flatten(source)
```



## NewLISP


```NewLISP>
 (flat '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))
(1 2 3 4 5 6 7 8)

```



## NGS

Note that when '''kern''' method is called, the multi-dispatch tries to match '''kern''' parameters with given arguments last added '''F''' first: if '''x''' is an array, the second '''F kern''' is invoked, otherwise the first '''F kern''' is invoked.

NGS defines '''flatten''' as a shallow flatten, hence using '''flatten_r''' here.


```NGS
F flatten_r(a:Arr)
	collector {
		local kern
		F kern(x) collect(x)
		F kern(x:Arr) x.each(kern)
		kern(a)
	}

echo(flatten_r([[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]))
```


{{out}}

```txt
[1,2,3,4,5,6,7,8]
```



## Nim


```nim
type
  TreeList[T] = ref TTreeList[T]
  TTreeList[T] = object
    case isLeaf: bool
    of true:  data: T
    of false: list: seq[TreeList[T]]

proc L[T](list: varargs[TreeList[T]]): TreeList[T] =
  var s: seq[TreeList[T]] = @[]
  for x in list: s.add x
  TreeList[T](isLeaf: false, list: s)

proc N[T](data: T): TreeList[T] =
  TreeList[T](isLeaf: true, data: data)

proc `$`[T](n: TreeList[T]): string =
  if n.isLeaf: result = $n.data
  else:
    result = "["
    for i, x in n.list:
      if i > 0: result.add ", "
      result.add($x)
    result.add "]"

proc flatten[T](n: TreeList[T]): seq[T] =
  if n.isLeaf: result = @[n.data]
  else:
    result = @[]
    for x in n.list:
      result.add flatten x

var x = L(L(N 1), N 2, L(L(N 3, N 4), N 5), L(L(L[int]())), L(L(L(N 6))), N 7, N 8, L[int]())
echo x
echo flatten(x)
```

{{out}}

```txt
[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
@[1, 2, 3, 4, 5, 6, 7, 8]
```


=={{header|Objective-C}}==
{{works with|Cocoa}}

```objc2>#import <Foundation/Foundation.h


@interface NSArray (FlattenExt)
@property (nonatomic, readonly) NSArray *flattened;
@end

@implementation NSArray (FlattenExt)
-(NSArray *) flattened {
    NSMutableArray *flattened = [[NSMutableArray alloc] initWithCapacity:self.count];
    
    for (id object in self) {
        if ([object isKindOfClass:[NSArray class]])
            [flattened addObjectsFromArray:((NSArray *)object).flattened];
        else
            [flattened addObject:object];
    }
    
    return [flattened autorelease];
}
@end

int main() {
    @autoreleasepool {
        NSArray *p = @[
		         @[ @1 ],
		         @2,
		         @[ @[@3, @4], @5],
		         @[ @[ @[ ] ] ],
		         @[ @[ @[ @6 ] ] ],
		         @7,
		         @8,
		         @[ ] ];
    
        for (id object in unflattened.flattened)
            NSLog(@"%@", object);
    
    }
    
    return 0;
}
```



## OCaml


```ocaml
# let flatten = List.concat ;;
val flatten : 'a list list -> 'a list = <fun>

# let li = [[1]; 2; [[3;4]; 5]; [[[]]]; [[[6]]]; 7; 8; []] ;;
                ^^^
Error: This expression has type int but is here used with type int list

# (* use another data which can be accepted by the type system *)
  flatten [[1]; [2; 3; 4]; []; [5; 6]; [7]; [8]] ;;
- : int list = [1; 2; 3; 4; 5; 6; 7; 8]
```


Since OCaml is statically typed, it is not possible to have a value that could be both a list and a non-list. Instead, we can use an algebraic datatype:


```ocaml
# type 'a tree = Leaf of 'a | Node of 'a tree list ;;
type 'a tree = Leaf of 'a | Node of 'a tree list

# let rec flatten = function
     Leaf x -> [x]
   | Node xs -> List.concat (List.map flatten xs) ;;
val flatten : 'a tree -> 'a list = <fun>

# flatten (Node [Node [Leaf 1]; Leaf 2; Node [Node [Leaf 3; Leaf 4]; Leaf 5]; Node [Node [Node []]]; Node [Node [Node [Leaf 6]]]; Leaf 7; Leaf 8; Node []]) ;;
- : int list = [1; 2; 3; 4; 5; 6; 7; 8]
```



## Oforth



```Oforth
[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []] expand println
```


{{out}}

```txt

[1, 2, 3, 4, 5, 6, 7, 8]

```



## Oz

Oz has a standard library function "Flatten":

```oz
{Show {Flatten [[1] 2 [[3 4] 5] [[nil]] [[[6]]] 7 8 nil]}}
```

A simple, non-optimized implementation could look like this:

```oz
fun {Flatten2 Xs}
   case Xs of nil then nil
   [] X|Xr then
      {Append {Flatten2 X} {Flatten2 Xr}}
   else [Xs]
   end
end

```


## ooRexx


```ooRexx

sub1 = .array~of(1)
sub2 = .array~of(3, 4)
sub3 = .array~of(sub2, 5)
sub4 = .array~of(.array~of(.array~new))
sub5 = .array~of(.array~of(.array~of(6)))
sub6 = .array~new

-- final list construction
list = .array~of(sub1, 2, sub3, sub4, sub5, 7, 8, sub6)

-- flatten
flatlist = flattenList(list)

say "["flatlist~toString("line", ", ")"]"

::routine flattenList
  use arg list
  -- we could use a list or queue, but let's just use an array
  accumulator = .array~new

  -- now go to the recursive processing version
  call flattenSublist list, accumulator

  return accumulator

::routine flattenSublist
  use arg list, accumulator

  -- ask for the items explicitly, since this will allow
  -- us to flatten indexed collections as well
  do item over list~allItems
      -- if the object is some sort of collection, flatten this out rather
      -- than add to the accumulator
      if item~isA(.collection) then call flattenSublist item, accumulator
      else accumulator~append(item)
  end

```



## PARI/GP


```parigp
flatten(v)={
  my(u=[]);
  for(i=1,#v,
    u=concat(u,if(type(v[i])=="t_VEC",flatten(v[i]),v[i]))
  );
  u
};
```



## Perl


```perl
sub flatten {
    map { ref eq 'ARRAY' ? flatten(@$_) : $_ } @_
}

my @lst = ([1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []);
print flatten(@lst), "\n";
```



## Perl 6

{{works with|Rakudo Star|2018.03}}


```perl6
my @l = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []];

say .perl given gather @l.deepmap(*.take); # lazy recursive version

# Another way to do it is with a recursive function (here actually a Block calling itself with the &?BLOCK dynamic variable):

say { |(@$_ > 1 ?? map(&?BLOCK, @$_) !! $_) }(@l)
```



## Phix

standard builtin

```Phix
?flatten({{1},2,{{3,4},5},{{{}}},{{{6}}},7,8,{}})
```

{{out}}

```txt

{1,2,3,4,5,6,7,8}

```



## PHP

{{works with|PHP|4.x only, not 5.x}}

```php
/* Note: This code is only for PHP 4.
   It won't work on PHP 5 due to the change in behavior of array_merge(). */
while (array_filter($lst, 'is_array'))
    $lst = call_user_func_array('array_merge', $lst);
```

Explanation: while <code>$lst</code> has any elements which are themselves arrays (i.e. <code>$lst</code> is not flat), we merge the elements all together (in PHP 4, <code>array_merge()</code> treated non-array arguments as if they were 1-element arrays; PHP 5 <code>array_merge()</code> no longer allows non-array arguments.), thus flattening the top level of any embedded arrays. Repeat this process until the array is flat.


### Recursive



```php
<?php
function flatten($ary) {
    $result = array();
    foreach ($ary as $x) {
        if (is_array($x))
            // append flatten($x) onto $result
            array_splice($result, count($result), 0, flatten($x));
        else
            $result[] = $x;
    }
    return $result;
}

$lst = array(array(1), 2, array(array(3, 4), 5), array(array(array())), array(array(array(6))), 7, 8, array());
var_dump(flatten($lst));
?>
```


Alternatively:{{works with|PHP|5.3+}}


```php
<?php
function flatten($ary) {
    $result = array();
    array_walk_recursive($ary, function($x, $k) use (&$result) { $result[] = $x; });
    return $result;
}

$lst = array(array(1), 2, array(array(3, 4), 5), array(array(array())), array(array(array(6))), 7, 8, array());
var_dump(flatten($lst));
?>
```



```php
<?php
function flatten_helper($x, $k, $obj) {
    $obj->flattened[] = $x;
}

function flatten($ary) {
    $obj = (object)array('flattened' => array());
    array_walk_recursive($ary, 'flatten_helper', $obj);
    return $obj->flattened;
}

$lst = array(array(1), 2, array(array(3, 4), 5), array(array(array())), array(array(array(6))), 7, 8, array());
var_dump(flatten($lst));
?>
```


Using the standard library (warning: objects will also be flattened by this method):


```php
<?php
$lst = array(array(1), 2, array(array(3, 4), 5), array(array(array())), array(array(array(6))), 7, 8, array());
$result = iterator_to_array(new RecursiveIteratorIterator(new RecursiveArrayIterator($lst)), false);
var_dump($result);
?>
```


===Non-recursive===

Function flat is iterative and flattens the array in-place.

```php
<?php
function flat(&$ary) { // argument must be by reference or array will just be copied
    for ($i = 0; $i < count($ary); $i++) {
        while (is_array($ary[$i])) {
            array_splice($ary, $i, 1, $ary[$i]);
        }
    }
}

$lst = array(array(1), 2, array(array(3, 4), 5), array(array(array())), array(array(array(6))), 7, 8, array());
flat($lst);
var_dump($lst);
?>
```



## PicoLisp


```PicoLisp
(de flatten (X)
   (make                               # Build a list
      (recur (X)                       # recursively over 'X'
         (if (atom X)
            (link X)                   # Put atoms into the result
            (mapc recurse X) ) ) ) )   # or recurse on sub-lists
```


or a more succint way using [http://www.software-lab.de/doc/refF.html#fish fish]:


```PicoLisp
(de flatten (X)
   (fish atom X) )
```



## Pike

There's a built-in function called <code>Array.flatten()</code> which does this, but here's a custom function:

```pike
array flatten(array a) {
	array r = ({ });
	
	foreach (a, mixed n) {
		if (arrayp(n)) r += flatten(n);
		else r += ({ n });
	}
	
	return r;
}
```



## PL/I

The Translate(text,that,this) intrinsic function returns ''text'' with any character in ''text'' that is found in ''this'' (say the third) replaced by the corresponding third character in ''that''. Suppose the availability of a function Replace(text,that,this) which returns ''text'' with all occurrences of ''this'' (a single text, possibly many characters) replaced by ''that'', possibly zero characters. The Translate function does not change the length of its string, simply translate its characters in place.

```PL/I

list = translate (list, '  ', '[]' ); /*Produces "  1 , 2,   3,4 , 5 ,       ,    6   , 7, 8,     " */
list = Replace(list,'',' ');          /*Converts spaces to nothing. Same parameter order as Translate.*/
do while index(list,',,') > 0;        /*Is there a double comma anywhere?
  list = Replace(list,',',',,');      /*Yes. Convert double commas to single, nullifying empty lists*/
end;                                  /*And search afresh, in case of multiple commas in a row.*/
list = '[' || list || ']';            /*Repackage the list.*/

```

This is distinctly crude. A user-written Replace function is confronted by the requirement to specify a maximum size for its returned result, for instance <code>Replace:Procedure(text,that,this) Returns(Character 200 Varying);</code> which is troublesome for general use. The intrinsic function Translate has no such restriction.

An alternative would be to translate the commas into spaces also (thereby the null entry vanishes) then scan along the result.


## PostScript

{{libheader|initlib}}

```postscript

/flatten {
    /.f {{type /arraytype eq} {{.f} map aload pop} ift}.
    [exch .f]
}.

```

<lang>
[[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []] flatten

```



## PowerShell


```PowerShell

function flatten($a) {
    if($a.Count -gt 1) {
        $a | foreach{ $(flatten $_)}
    } else {$a}
}
$a = @(@(1), 2, @(@(3,4), 5), @(@(@())), @(@(@(6))), 7, 8, @())
"$(flatten $a)"

```

<b>Output:</b>

```txt
 
1 2 3 4 5 6 7 8

```



## Prolog


```Prolog

flatten(List, FlatList) :-
	flatten(List, [], FlatList).

flatten(Var, T, [Var|T]) :-
	var(Var), !.
flatten([], T, T) :- !.
flatten([H|T], TailList, List) :- !,
	flatten(H, FlatTail, List),
	flatten(T, TailList, FlatTail).

flatten(NonList, T, [NonList|T]).

```



## PureBasic


```PureBasic
Structure RCList
  Value.i
  List A.RCList()
EndStructure

Procedure Flatten(List A.RCList())
  ResetList(A())
  While NextElement(A())
    With A()
      If \Value
        Continue
      Else
        ResetList(\A())
        While NextElement(\A())
          If \A()\Value: A()\Value=\A()\Value: EndIf
        Wend
      EndIf
      While ListSize(\A()): DeleteElement(\A()): Wend
      If Not \Value: DeleteElement(A()): EndIf
    EndWith
  Wend
EndProcedure
```

Set up the MD-List & test the Flattening procedure.

```PureBasic
;- Set up two lists, one multi dimensional and one 1-D.
NewList A.RCList()

;- Create a deep list
With A()
  AddElement(A()):  AddElement(\A()): AddElement(\A()): \A()\Value=1
  AddElement(A()):                     A()\Value=2
  AddElement(A()):  AddElement(\A()): \A()\Value=3
  AddElement(\A()):                   \A()\Value=4
  AddElement(A()):  AddElement(\A()): \A()\Value=5
  AddElement(A()):  AddElement(\A()): AddElement(\A()): AddElement(\A())
  AddElement(A()):  AddElement(\A()): AddElement(\A()): \A()\Value=6
  AddElement(A()):                     A()\Value=7
  AddElement(A()):                     A()\Value=8
  AddElement(A()):  AddElement(\A()): AddElement(\A())
EndWith

Flatten(A())

;- Present the result
If OpenConsole()
  Print("Flatten: [")
  ForEach A()
    Print(Str(A()\Value))
    If ListIndex(A())<(ListSize(A())-1)
      Print(", ")
    Else
      PrintN("]")
    EndIf
  Next
  Print(#CRLF$+"Press ENTER to quit"): Input()
EndIf
```

```txt
Flatten: [1, 2, 4, 5, 6, 7, 8]
```



## Python



### Recursive



```python>>>
 def flatten(lst):
	return sum( ([x] if not isinstance(x, list) else flatten(x)
		     for x in lst), [] )

>>> lst = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
>>> flatten(lst)
[1, 2, 3, 4, 5, 6, 7, 8]
```



And, as [[Rosetta_Code|the idea of Rosetta Code]] is to demonstrate how languages are '''similar''' as well as different, and to thus to 'aid a person with a grounding in one approach to a problem in learning another', here it is in terms of '''concatMap''', which can be defined in any language, including mathematics, and which can be variously expressed in Python. (The fastest Python implementation of the '''concat''' component of the (concat . map) composition seems to be in terms of ''itertools.chain''). 

{{Works with|Python|3.7}}

```python
'''Flatten a nested list'''

from itertools import (chain)


# flatten :: NestedList a -> [a]
def flatten(x):
    '''A list of atomic values resulting from fully flattening
       an arbitrarily nested list.'''
    return concatMap(flatten)(x) if isinstance(x, list) else [x]


def main():
    '''Test: flatten an arbitrarily nested list.'''
    print(
        fTable(__doc__ + ':')(showList)(showList)(
            flatten
        )([
            [[[]]],
            [[1, 2, 3]],
            [[1], [[2]], [[[3, 4]]]],
            [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
        ])
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# fTable :: String -> (a -> String) ->
#                     (b -> String) ->
#        (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function ->
                 fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + (' -> ') + fxShow(f(x))
            for x in xs
        ])
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# showList :: [a] -> String
def showList(xs):
    '''Stringification of a list.'''
    return '[' + ','.join(str(x) for x in xs) + ']'


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Flatten a nested list:
                                   [[[]]] -> []
                              [[1, 2, 3]] -> [1,2,3]
                   [[1],[[2]],[[[3, 4]]]] -> [1,2,3,4]
[[1],2,[[3, 4], 5],[[[]]],[[[6]]],7,8,[]] -> [1,2,3,4,5,6,7,8]
```


===Non-recursive===

Function flat is iterative and flattens the list in-place. It follows the Python idiom of returning None when acting in-place:

```python>>>
 def flat(lst):
    i=0
    while i<len(lst):
        while True:
            try:
                lst[i:i+1] = lst[i]
            except (TypeError, IndexError):
                break
        i += 1
        
>>> lst = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
>>> flat(lst)
>>> lst
[1, 2, 3, 4, 5, 6, 7, 8]
```



And, in contexts where it may be desirable to avoid not just recursion, but also:

# mutation of the original list, and 
# dependence on error-events for evaluation control, 


we can again use the universal '''concat . map''' composition (see the second recursive example above) by embedding it in a fold / reduction, and using it with a pure, but iteratively-implemented, '''until''' function.

( Note that the generic functions in the following example are ''curried'', enabling not only more flexible composition, but also some simplifying reductions – here eliminating the need for two uses of Python's ''lambda'' keyword ):

{{Works with|Python|3.7}}

```python
'''Flatten a list'''

from functools import (reduce)
from itertools import (chain)


def flatten(xs):
    '''A flat list of atomic values derived
       from a nested list.
    '''
    return reduce(
        lambda a, x: a + list(until(every(notList))(
            concatMap(pureList)
        )([x])),
        xs, []
    )


# TEST ----------------------------------------------------
def main():
    '''From nested list to flattened list'''

    print(main.__doc__ + ':\n\n')
    xs = [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
    print(
        repr(xs) + ' -> ' + repr(flatten(xs))
    )


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


# every :: (a -> Bool) -> [a] -> Bool
def every(p):
    '''True if p(x) holds for every x in xs'''
    def go(p, xs):
        return all(map(p, xs))
    return lambda xs: go(p, xs)


# notList :: a -> Bool
def notList(x):
    '''True if the value x is not a list.'''
    return not isinstance(x, list)


# pureList :: a -> [b]
def pureList(x):
    '''x if x is a list, othewise [x]'''
    return x if isinstance(x, list) else [x]


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
From nested list to flattened list:


[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []] -> [1, 2, 3, 4, 5, 6, 7, 8]
```



### Generative

This method shows a solution using Python generators.

<code>flatten</code> is a generator that yields the non-list values of its input in order.
In this case, the generator is converted back to a list before printing.


```python>>>
 def flatten(lst):
     for x in lst:
         if isinstance(x, list):
             for x in flatten(x):
                 yield x
         else:
             yield x
 
 
>>> lst = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
>>> print list(flatten(lst)) 
[1, 2, 3, 4, 5, 6, 7, 8]
```



## Q

{{trans|K}}
We repeatedly apply <tt>raze</tt> until the return value converges to a fixed value.

```q
(raze/) ((1); 2; ((3;4); 5); ((())); (((6))); 7; 8; ())
```



## R


```R
x <- list(list(1), 2, list(list(3, 4), 5), list(list(list())), list(list(list(6))), 7, 8, list())

unlist(x)
```



## Racket

Racket has a built-in flatten function:

```Racket

#lang racket
(flatten '(1 (2 (3 4 5) (6 7)) 8 9))

```

{{out}}

```txt

'(1 2 3 4 5 6 7 8 9)

```


or, writing it explicitly with the same result:

```Racket

#lang racket
(define (flatten l)
  (cond [(empty? l)      null]
        [(not (list? l)) (list l)]
        [else            (append (flatten (first l)) (flatten (rest l)))]))
(flatten '(1 (2 (3 4 5) (6 7)) 8 9))

```



## REBOL


```rebol

flatten: func [
    "Flatten the block in place."
    block [any-block!]
][
    parse block [
        any [block: any-block! (change/part block first block 1) :block | skip]
    ]
    head block
]

```


Sample: 
```txt

>> flatten [[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []]
== [1 2 3 4 5 6 7 8]

```



## Red


```Red

flatten: function [
    "Flatten the block"
    block [any-block!]
][
    load form block
]

red>> flatten [[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []]
== [1 2 3 4 5 6 7 8]

;flatten a list to a string
>> blk: [1 2 ["test"] "a" [["bb"]] 3 4 [[[99]]]]
>> form blk
== "1 2 test a bb 3 4 99"
```



## REXX

{{trans|PL/I}}

```rexx
/*REXX program  (translated from PL/I)  flattens a list  (the data need not be numeric).*/
list= '[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]'  /*the list to be flattened.    */
say list                                                 /*display the original list.   */
 c= ','                                                  /*define a literal  (1 comma). */
cc= ',,'                                                 /*   "   "    "     (2 commas).*/
list= translate(list, , "[]")                            /*translate brackets to blanks.*/
list= space(list, 0)                                     /*Converts spaces to nulls.    */
                      do  while index(list, cc) > 0      /*any double commas ?          */
                      list= changestr(cc, list, c)       /*convert  ,,  to single comma.*/
                      end   /*while*/
list= strip(list, 'T', c)                                /*strip the last trailing comma*/
list = '['list"]"                                        /*repackage the list.          */
say list                                                 /*display the flattened list.  */
```

{{out|output|:}}

```txt

[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
[1,2,3,4,5,6,7,8]

```



## Ring


```ring

aString = "[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]"
bString = ""
cString = ""
for n=1 to len(aString)
    if ascii(aString[n]) >= 48 and  ascii(aString[n]) <= 57
       bString = bString + ", " + aString[n]
    ok
next
cString = substr(bString,3,Len(bString)-2)
cString = '"' + cString + '"'
see cString + nl

```


```txt

"1, 2, 3, 4, 5, 6, 7, 8"

```



## Ruby

<code>flatten</code> is a built-in method of Arrays

```ruby
flat = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []].flatten
p flat  # => [1, 2, 3, 4, 5, 6, 7, 8]
```

The <code>flatten</code> method takes an optional argument, which dedicates the amount of levels to be flattened.

```ruby
p flatten_once = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []].flatten(1)
# => [1, 2, [3, 4], 5, [[]], [[6]], 7, 8]

```



## Run BASIC

{{incorrect|Run BASIC| The task is not in string translation but in list translation.}}

```runbasic
n$ = "[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8 []]"
for i = 1 to len(n$)
 if instr("[] ,",mid$(n$,i,1)) = 0 then 
  flatten$ = flatten$ + c$ + mid$(n$,i,1)
  c$ = ","
 end if
next i
print "[";flatten$;"]"
```

{{out}}

```txt
[1,2,3,4,5,6,7,8]
```



## Rust

First we have to create a type that supports arbitrary nesting:

```rust
use std::{vec, mem, iter};

enum List<T> {
    Node(Vec<List<T>>),
    Leaf(T),
}

impl<T> IntoIterator for List<T> {
    type Item = List<T>;
    type IntoIter = ListIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            List::Node(vec) => ListIter::NodeIter(vec.into_iter()),
            leaf @ List::Leaf(_) => ListIter::LeafIter(iter::once(leaf)),
        }
    }
}

enum ListIter<T> {
    NodeIter(vec::IntoIter<List<T>>),
    LeafIter(iter::Once<List<T>>),
}

impl<T> ListIter<T> {
    fn flatten(self) -> Flatten<T> {
        Flatten {
            stack: Vec::new(),
            curr: self,
        }
    }
}

impl<T> Iterator for ListIter<T> {
    type Item = List<T>;
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            ListIter::NodeIter(ref mut v_iter) => v_iter.next(),
            ListIter::LeafIter(ref mut o_iter) => o_iter.next(),
        }
    }
}

struct Flatten<T> {
    stack: Vec<ListIter<T>>,
    curr: ListIter<T>,
}

// Flatten code is a little messy since we are shoehorning recursion into an Iterator
impl<T> Iterator for Flatten<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.curr.next() {
                Some(list) => {
                    match list {
                        node @ List::Node(_) => {
                            self.stack.push(node.into_iter());
                            let len = self.stack.len();
                            mem::swap(&mut self.stack[len - 1], &mut self.curr);
                        }
                        List::Leaf(item) => return Some(item),
                    }
                }
                None => {
                    if let Some(next) = self.stack.pop() {
                        self.curr = next;
                    } else {
                        return None;
                    }
                }
            }
        }
    }
}

use List::*;
fn main() {
    let list = Node(vec![Node(vec![Leaf(1)]),
                         Leaf(2),
                         Node(vec![Node(vec![Leaf(3), Leaf(4)]), Leaf(5)]),
                         Node(vec![Node(vec![Node(vec![])])]),
                         Node(vec![Node(vec![Node(vec![Leaf(6)])])]),
                         Leaf(7),
                         Leaf(8),
                         Node(vec![])]);

    for elem in list.into_iter().flatten() {
        print!("{} ", elem);
    }
    println!();

}
```

{{output}}

```txt
1 2 3 4 5 6 7 8

```


=={{header|S-lang}}==
<lang s-lang>define flatten ();

define flatten (list) {
    variable item,
        retval,
        val;
    if (typeof(list) != List_Type) {
        retval = list;
    } else {
        retval = {};
        foreach item (list) {
            foreach val (flatten(item)) {
                list_append(retval, val);
            }
        }
    }
    return retval;
}
```


Sample:


```txt

slsh> variable data = {{1}, 2, {{3,4}, 5}, {{{}}}, {{{6}}}, 7, 8, {}},
           result = flatten(data);                                    
slsh> print(result);              
{
1
2
3
4
5
6
7
8
}

```



## Scala


```scala
def flatList(l: List[_]): List[Any] = l match {
  case Nil => Nil
  case (head: List[_]) :: tail => flatList(head) ::: flatList(tail)
  case head :: tail => head :: flatList(tail)
}
```


Sample:


```txt

scala> List(List(1), 2, List(List(3, 4), 5), List(List(List())), List(List(List(6))), 7, 8, List())
res10: List[Any] = List(List(1), 2, List(List(3, 4), 5), List(List(List())), List(List(List(6))), 7, 8, List())

scala> flatList(res10)
res12: List[Any] = List(1, 2, 3, 4, 5, 6, 7, 8)

```



## Scheme


```scheme>
 (define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

> (flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))
(1 2 3 4 5 6 7 8)
```



## Shen


```Shen

(define flatten
[] -> []
[X|Y] -> (append (flatten X) (flatten Y))
X -> [X])

(flatten [[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []])

```

{{out}}

```txt

[1 2 3 4 5 6 7 8]

```



## Sidef


```ruby
func flatten(a) {
    var flat = [];
    a.each { |item|
        flat += (item.is_an(Array) ? flatten(item) : [item]);
    };
    return flat;
}

var arr = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []];
say flatten(arr).dump;      # used-defined function
say arr.flatten.dump;       # built-in method for Array obj
```



## Slate


```slate
s@(Sequence traits) flatten
[
  [| :out | s flattenOn: out] writingAs: s
].

s@(Sequence traits) flattenOn: w@(WriteStream traits)
[
  s do: [| :value |
    (value is: s)
      ifTrue: [value flattenOn: w]
      ifFalse: [w nextPut: value]].
].
```



## Smalltalk

{{works with|GNU Smalltalk}}


```smalltalk
OrderedCollection extend [
  flatten [ |f|
    f := OrderedCollection new.
    self do: [ :i |
      i isNumber
        ifTrue: [ f add: i ]
        ifFalse: [ |t|
          t := (OrderedCollection withAll: i) flatten.
          f addAll: t
        ]
    ].
    ^ f
  ]
].


|list|
list := OrderedCollection 
          withAll: { {1} . 2 . { {3 . 4} . 5 } .
                     {{{}}} . {{{6}}} . 7 . 8 . {} }.

(list flatten) printNl.
```


Here is a non-OOP (but functional) version, which uses a block-closure as function (showing higher order features of Smalltalk):


```smalltalk

flatDo := 
    [:element :action |
        element isCollection ifTrue:[
            element do:[:el | flatDo value:el value:action]
        ] ifFalse:[
            action value:element
        ].
    ].
    
collection := { 
                {1} . 2 . { {3 . 4} . 5 } .
                {{{}}} . {{{6}}} . 7 . 8 . {} 
              }.

newColl := OrderedCollection new.                         
flatDo 
    value:collection
    value:[:el | newColl add: el]
```


of course, many Smalltalk libraries already provide such functionality.
{{works with|Smalltalk/X}} {{works with|Pharo}}

```smalltalk
collection flatDo:[:el | newColl add:el]
```



## Standard ML

In Standard ML, list must be homogeneous, but nested lists can be implemented as a tree-like data structure using a <code>datatype</code> statement:

```sml
datatype 'a nestedList =
	  L of 'a			(* leaf *)
	| N of 'a nestedList list	(* node *)

```

Flattening of this structure is similar to flatten trees:

```sml
fun flatten (L  x) = [x]
  | flatten (N xs) = List.concat (map flatten xs)
```


{{out}}

```txt

- flatten (N [ L 1, N [L 2, N []], L 3]);
val it = [1,2,3] : int list

```



## Suneido


```suneido
ob = [[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8, []]
ob.Flatten()
```


{{out}}

```txt
#(1, 2, 3, 4, 5, 6, 7, 8)
```



## SuperCollider

SuperCollider has the method "flat", which completely flattens nested lists, and the method "flatten(n)" to flatten a certain number of levels.

```SuperCollider

a = [[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []];
a.flatten(1); // answers [ 1, 2, [ 3, 4 ], 5, [ [  ] ], [ [ 6 ] ], 7, 8 ]
a.flat; // answers [ 1, 2, 3, 4, 5, 6, 7, 8 ]

```


Written as a function:

```SuperCollider

(
f = { |x|
	var res = res ?? List.new;
	if(x.isSequenceableCollection) {
		x.do { |each|
			res.addAll(f.(each))
		}
	} {
		res.add(x);
	};
	res
};
f.([[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]);
)
```



## Swift


== Recursive == 


```swift
func list(s: Any...) -> [Any] {
  return s
}

func flatten<T>(s: [Any]) -> [T] {
  var r = [T]()
  for e in s {
    switch e {
    case let a as [Any]:
      r += flatten(a)
    case let x as T:
      r.append(x)
    default:
      assert(false, "value of wrong type")
    }
  }
  return r
}

let s = list(list(1),
  2,
  list(list(3, 4), 5),
  list(list(list())),
  list(list(list(6))),
  7,
  8,
  list()
)
println(s)
let result : [Int] = flatten(s)
println(result)
```

{{out}}

```txt

[[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []]
[1 2 3 4 5 6 7 8]

```


More functionally:
{{works with|Swift|1.2+}}

```swift
func list(s: Any...) -> [Any] {
  return s
}

func flatten<T>(s: [Any]) -> [T] {
  return s.flatMap {
    switch $0 {
    case let a as [Any]:
      return flatten(a)
    case let x as T:
      return [x]
    default:
      assert(false, "value of wrong type")
    }
  }
}

let s = list(list(1),
  2,
  list(list(3, 4), 5),
  list(list(list())),
  list(list(list(6))),
  7,
  8,
  list()
)
println(s)
let result : [Int] = flatten(s)
println(result)
```

{{out}}

```txt

[[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []]
[1 2 3 4 5 6 7 8]

```


== Non-recursive == 

{{works with|Swift|2.0+}}


```swift
func list(s: Any...) -> [Any]
{
    return s
}

func flatten<T>(array: [Any]) -> [T]
{
    var result: [T] = []
    var workstack: [(array: [Any], lastIndex: Int)] = [(array, 0)]
    
    workstackLoop: while !workstack.isEmpty
    {
        for element in workstack.last!.array.suffixFrom(workstack.last!.lastIndex)
        {
            workstack[workstack.endIndex - 1].lastIndex++
            
            if let element = element as? [Any]
            {
                workstack.append((element, 0))
                
                continue workstackLoop
            }
            
            result.append(element as! T)
        }
        
        workstack.removeLast()
    }
    
    return result
}

let input = list(list(1),
    2,
    list(list(3, 4), 5),
    list(list(list())),
    list(list(list(6))),
    7,
    8,
    list()
)

print(input)

let result: [Int] = flatten(input)

print(result)
```

{{out}}

```txt

[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []]
[1, 2, 3, 4, 5, 6, 7, 8]

```



## Tailspin


```tailspin

templates flatten
  [ $ -> # ] !
  <[]>
    $... -> #
  <>
    $ !
end flatten
 
[[1], 2, [[3, 4], 5], [[[]]], [[[6]]], 7, 8, []] -> flatten -> !OUT::write

```

{{out}}

```txt

[1, 2, 3, 4, 5, 6, 7, 8]

```



## Tcl


```tcl
proc flatten list {
    for {set old {}} {$old ne $list} {} {
        set old $list
        set list [join $list]
    }
    return $list
}

puts [flatten {{1} 2 {{3 4} 5} {{{}}} {{{6}}} 7 8 {}}]
# ===> 1 2 3 4 5 6 7 8
```

Note that because lists are not syntactically distinct from strings, it is probably a mistake to use this procedure with real (especially non-numeric) data. Also note that there are no parentheses around the outside of the list when printed; this is just a feature of how Tcl regards lists, and the value is a proper list (it can be indexed into with <code>lindex</code>, iterated over with <code>foreach</code>, etc.)

Another implementation that's slightly more terse:


```tcl
proc flatten {data} {
    while { $data != [set data [join $data]] } { }
    return $data
}
puts [flatten {{1} 2 {{3 4} 5} {{{}}} {{{6}}} 7 8 {}}]
# ===> 1 2 3 4 5 6 7 8
```


=={{header|TI-89 BASIC}}==

There is no nesting of lists or other data structures in TI-89 BASIC, short of using variable names as pointers.


## Trith


```trith
[[1] 2 [[3 4] 5] [[[]]] [[[6]]] 7 8 []] flatten
```


{{omit from|UNIX Shell}}


## TXR


An important builtin.

```txr
@(bind foo ((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))
@(bind bar foo)
@(flatten bar)
```


Run:

```txt
$ txr -a 5 flatten.txr  # show variable bindings in array notation to depth 5
foo[0][0]="1"
foo[1]="2"
foo[2][0][0]="3"
foo[2][0][1]="4"
foo[2][1]="5"
foo[4][0][0][0]="6"
foo[5]="7"
foo[6]="8"
bar[0]="1"
bar[1]="2"
bar[2]="3"
bar[3]="4"
bar[4]="5"
bar[5]="6"
bar[6]="7"
bar[7]="8"
```



## VBScript

Working on embedded arrays as that's about the closest we get to lists.


### ==Implementation==


```vb

class flattener
	dim separator 
	
	sub class_initialize
		separator = ","
	end sub
	
	private function makeflat( a )
		dim i
		dim res
		for i = lbound( a ) to ubound( a ) 
			if isarray( a( i ) ) then
				res = res & makeflat( a( i ) )
			else
				res = res & a( i ) & separator
			end if
		next
		makeflat = res
	end function

	public function flatten( a )
		dim res
		res = makeflat( a )
		res = left( res, len( res ) - len(separator))
		res = split( res, separator )
		flatten = res
	end function
	
	public property let itemSeparator( c )
		separator = c
	end property
end class

```



### ==Invocation==


```vb

dim flat
set flat = new flattener
flat.itemSeparator = "~"
wscript.echo join( flat.flatten( array( array( 1 ),2,array(array(3,4),5),array(array(array())),array(array(array(6))),7,8,array())), "!")

```


{{out}}

```txt

1!2!3!4!5!6!7!8

```


=====Alternative (classless) Version=====
{{works with|Windows Script Host|*}}

```VBScript

' Flatten the example array...
a = FlattenArray(Array(Array(1), 2, Array(Array(3,4), 5), Array(Array(Array())), Array(Array(Array(6))), 7, 8, Array()))

' Print the list, comma-separated...
WScript.Echo Join(a, ",")

Function FlattenArray(a)
	If IsArray(a) Then DoFlatten a, FlattenArray: FlattenArray = Split(Trim(FlattenArray))
End Function

Sub DoFlatten(a, s)
	For i = 0 To UBound(a)
		If IsArray(a(i)) Then DoFlatten a(i), s Else s = s & a(i) & " "
	Next
End Sub

```



## Wart

Here's how Wart implements <code>flatten</code>:

```python
def (flatten seq acc)
  if no.seq
       acc
     ~list?.seq
       (cons seq acc)
     :else
       (flatten car.seq (flatten cdr.seq acc))
```


{{out}}

```txt
(flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))
=> (1 2 3 4 5 6 7 8)
```



## WDTE


```WDTE>let a =
 import 'arrays';
let s => import 'stream';

let flatten array =>
  a.stream array
  -> s.flatMap (@ f v => v {
      reflect 'Array' => a.stream v -> s.flatMap f;
    })
  -> s.collect
  ;
```


'''Usage:'''

```WDTE
flatten [[1]; 2; [[3; 4]; 5]; [[[]]]; [[[6]]]; 7; 8; []] -- io.writeln io.stdout;
```


{{out}}

```txt
[1; 2; 3; 4; 5; 6; 7; 8]
```



## zkl


```zkl
fcn flatten(list){ list.pump(List,
    fcn(i){ if(List.isType(i)) return(Void.Recurse,i,self.fcn); i}) }

flatten(L(L(1), L(2), L(L(3,4), 5), L(L(L())), L(L(L(6))), 7, 8, L()))
//-->L(1,2,3,4,5,6,7,8)
```

This works by recursively writing the contents of lists to a new list. If a list is recursive or cyclic, it will blow the stack and throw an exception.


## ZX Spectrum Basic

{{incorrect|ZX Spectrum Basic| The task is not in string translation but in list translation.}}

```zxbasic
10 LET f$="["
20 LET n$="[[1], 2, [[3,4], 5], [[[]]], [[[6]]], 7, 8 []]"
30 FOR i=2 TO (LEN n$)-1
40 IF n$(i)>"/" AND n$(i)<":" THEN LET f$=f$+n$(i): GO TO 60
50 IF n$(i)="," AND f$(LEN f$)<>"," THEN LET f$=f$+","
60 NEXT i
70 LET f$=f$+"]": PRINT f$
```

