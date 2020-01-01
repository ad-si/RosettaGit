+++
title = "Topological sort"
description = ""
date = 2019-06-29T02:47:21Z
aliases = []
[extra]
id = 4469
[taxonomies]
categories = []
tags = []
+++

{{task}}
Given a mapping between items, and items they depend on, a [[wp:Topological sorting|topological sort]] orders items so that no item precedes an item it depends upon.

The compiling of a library in the [[wp:VHDL|VHDL]] language has the constraint that a library must be compiled after any library it depends on.

A tool exists that extracts library dependencies.


;Task:
Write a function that will return a valid compile order of VHDL libraries from their dependencies.

* Assume library names are single words.
* Items mentioned as only dependents, (sic), have no dependents of their own, but their order of compiling must be given.
* Any self dependencies should be ignored.
* Any un-orderable dependencies should be flagged.



Use the following data as an example:

```txt

LIBRARY          LIBRARY DEPENDENCIES

### ====          =================

des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01             ieee dw01 dware gtech
dw02             ieee dw02 dware
dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
dw04             dw04 ieee dw01 dware gtech
dw05             dw05 ieee dware
dw06             dw06 ieee dware
dw07             ieee dware
dware            ieee dware
gtech            ieee gtech
ramlib           std ieee
std_cell_lib     ieee std_cell_lib
synopsys

```




<small>Note: the above data would be un-orderable if, for example, <code>dw04</code> is added to the list of dependencies of <code>dw01</code>.</small>


;C.f.:
:*   [[Topological sort/Extracted top item]].



There are two popular algorithms for topological sorting:
:*   Kahn's 1962 topological sort <ref> [[wp: topological sorting]] </ref>
:*   depth-first search <ref> [[wp: topological sorting]] </ref> <ref> Jason Sachs
[http://www.embeddedrelated.com/showarticle/799.php "Ten little algorithms, part 4: topological sort"] </ref>





## Ada


'''Digraphs: A package for directed graphs, representing nodes as positive numbers'''

The specification:


```Ada
with Ada.Containers.Vectors; use Ada.Containers;

package Digraphs is

   type Node_Idx_With_Null is new Natural;
   subtype Node_Index is Node_Idx_With_Null range 1 .. Node_Idx_With_Null'Last;
   -- a Node_Index is a number from 1, 2, 3, ... and the representative of a node

   type Graph_Type is tagged private;

   -- make sure Node is in Graph (possibly without connections)
   procedure Add_Node
     (Graph: in out Graph_Type'Class; Node: Node_Index);

   -- insert an edge From->To into Graph; do nothing if already there
   procedure Add_Connection
     (Graph: in out Graph_Type'Class; From, To: Node_Index);

   -- get the largest Node_Index used in any Add_Node or Add_Connection op.
   -- iterate over all nodes of Graph: "for I in 1 .. Graph.Node_Count loop ..."
   function Node_Count(Graph: Graph_Type) return Node_Idx_With_Null;

   -- remove an edge From->To from Fraph; do nothing if not there
   -- Graph.Node_Count is not changed
   procedure Del_Connection
     (Graph: in out Graph_Type'Class; From, To: Node_Index);

   -- check if an edge From->to exists in Graph
   function Connected
     (Graph: Graph_Type; From, To: Node_Index) return Boolean;

   -- data structure to store a list of nodes
   package Node_Vec is new Vectors(Positive, Node_Index);

   -- get a list of all nodes From->Somewhere in Graph
   function All_Connections
     (Graph: Graph_Type; From: Node_Index) return Node_Vec.Vector;

   Graph_Is_Cyclic: exception;

   -- a depth-first search to find a topological sorting of the nodes
   -- raises Graph_Is_Cyclic if no topological sorting is possible
   function Top_Sort
     (Graph: Graph_Type) return Node_Vec.Vector;

private

   package Conn_Vec is new Vectors(Node_Index, Node_Vec.Vector, Node_Vec."=");

   type Graph_Type is new Conn_Vec.Vector with null record;

end Digraphs;
```


The implementation:


```Ada
package body Digraphs is

   function Node_Count(Graph: Graph_Type) return Node_Idx_With_Null is
   begin
      return Node_Idx_With_Null(Graph.Length);
   end Node_Count;

   procedure Add_Node(Graph: in out Graph_Type'Class; Node: Node_Index) is
   begin
      for I in Node_Index range Graph.Node_Count+1 .. Node loop
         Graph.Append(Node_Vec.Empty_Vector);
      end loop;
   end Add_Node;

   procedure Add_Connection
     (Graph: in out Graph_Type'Class; From, To: Node_Index) is
   begin
      Graph.Add_Node(Node_Index'Max(From, To));
      declare
         Connection_List: Node_Vec.Vector := Graph.Element(From);
      begin
         for I in Connection_List.First_Index .. Connection_List.Last_Index loop
            if Connection_List.Element(I) >= To then
               if Connection_List.Element(I) = To then
                  return; -- if To is already there, don't add it a second time
               else -- I is the first index with Element(I)>To, insert To here
                  Connection_List.Insert(Before => I, New_Item => To);
                  Graph.Replace_Element(From, Connection_List);
                  return;
               end if;
            end if;
         end loop;
         -- there was  no I with no Element(I) > To, so insert To at the end
         Connection_List.Append(To);
         Graph.Replace_Element(From, Connection_List);
         return;
      end;
   end Add_Connection;

   procedure Del_Connection
     (Graph: in out Graph_Type'Class; From, To: Node_Index) is
      Connection_List: Node_Vec.Vector := Graph.Element(From);
   begin
      for I in Connection_List.First_Index .. Connection_List.Last_Index loop
         if Connection_List.Element(I) = To then
            Connection_List.Delete(I);
            Graph.Replace_Element(From, Connection_List);
            return; -- we are done
         end if;
      end loop;
   end Del_Connection;

   function Connected
     (Graph: Graph_Type; From, To: Node_Index) return Boolean is
      Connection_List: Node_Vec.Vector renames Graph.Element(From);
   begin
      for I in Connection_List.First_Index .. Connection_List.Last_Index loop
         if Connection_List.Element(I) = To then
            return True;
         end if;
      end loop;
      return False;
   end Connected;

   function All_Connections
     (Graph: Graph_Type; From: Node_Index) return Node_Vec.Vector is
   begin
      return Graph.Element(From);
   end All_Connections;

   function Top_Sort
     (Graph: Graph_Type) return Node_Vec.Vector is

      Result: Node_Vec.Vector;
      Visited: array(1 .. Graph.Node_Count) of Boolean := (others => False);
      Active:  array(1 .. Graph.Node_Count) of Boolean := (others => False);

      procedure Visit(Node: Node_Index) is
      begin
         if not Visited(Node) then
            Visited(Node) := True;
            Active(Node)  := True;
            declare
               Cons: Node_Vec.Vector := All_Connections(Graph, Node);
            begin
               for Idx in Cons.First_Index .. Cons.Last_Index loop
                  Visit(Cons.Element(Idx));
               end loop;
            end;
            Active(Node) := False;
            Result.Append(Node);
         else
            if Active(Node) then
               raise Constraint_Error with "Graph is Cyclic";
            end if;
         end if;
      end Visit;

   begin
      for Some_Node in Visited'Range loop
         Visit(Some_Node);
      end loop;
      return Result;
   end Top_Sort;

end Digraphs;
```


'''Set_of_Names: Translating strings into numbers and vice versa'''

The specification:


```Ada
private with Ada.Containers.Indefinite_Vectors;

generic
   type Index_Type_With_Null is new Natural;
package Set_Of_Names is
   subtype Index_Type is Index_Type_With_Null
       range 1 .. Index_Type_With_Null'Last;
   -- manage a set of strings;
   -- each string in the set is assigned a unique index of type Index_Type

   type Set is tagged private;

   -- inserts Name into Names; do nothing if already there;
   procedure Add(Names: in out Set; Name: String);

   -- Same operation, additionally emiting Index=Names.Idx(Name)
   procedure Add(Names: in out Set; Name: String; Index: out Index_Type);

   -- remove Name from Names; do nothing if not found
   -- the removal may change the index of other strings in Names
   procedure Sub(Names: in out Set; Name: String);

   -- returns the unique index of Name in Set; or 0 if Name is not there
   function Idx(Names: Set; Name: String) return Index_Type_With_Null;

   -- returns the unique name of Index;
   function Name(Names: Set; Index: Index_Type) return String;

   -- first index, last index and total number of names in set
   -- to iterate over Names, use "for I in Names.Start .. Names.Stop loop ...
   function Start(Names: Set) return Index_Type;
   function Stop(Names: Set) return Index_Type_With_Null;
   function Size(Names: Set) return Index_Type_With_Null;

private

   package Vecs is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Index_Type, Element_Type => String);

   type Set is new Vecs.Vector with null record;

end Set_Of_Names;
```


The implementation


```Ada
package body Set_Of_Names is

   use type Ada.Containers.Count_Type, Vecs.Cursor;

   function Start(Names: Set) return Index_Type is
   begin
      if Names.Length = 0 then
         return 1;
      else
         return Names.First_Index;
      end if;
   end Start;

   function Stop(Names: Set) return Index_Type_With_Null is
   begin
      if Names.Length=0 then
         return 0;
      else
         return Names.Last_Index;
      end if;
   end Stop;

   function Size(Names: Set) return Index_Type_With_Null is
   begin
      return Index_Type_With_Null(Names.Length);
   end Size;

   procedure Add(Names: in out Set; Name: String; Index: out Index_Type) is
      I: Index_Type_With_Null := Names.Idx(Name);
   begin
      if I = 0 then -- Name is not yet in Set
         Names.Append(Name);
         Index := Names.Stop;
      else
        Index := I;
      end if;
   end Add;

   procedure Add(Names: in out Set; Name: String) is
      I: Index_Type;
   begin
      Names.Add(Name, I);
   end Add;

   procedure Sub(Names: in out Set; Name: String) is
      I: Index_Type_With_Null := Names.Idx(Name);
   begin
      if I /= 0 then -- Name is in set
         Names.Delete(I);
      end if;
   end Sub;

   function Idx(Names: Set; Name: String) return Index_Type_With_Null is
   begin
      for I in Names.First_Index .. Names.Last_Index loop
         if Names.Element(I) = Name then
            return I;
         end if;
      end loop;
      return 0;
   end Idx;

   function Name(Names: Set; Index: Index_Type) return String is
   begin
      return Names.Element(Index);
   end Name;

end Set_Of_Names;
```


'''Toposort: Putting things together for the main program'''


```Ada
with Ada.Text_IO, Digraphs, Set_Of_Names, Ada.Command_Line;

procedure Toposort is

   -- shortcuts for package names, intantiation of generic package
   package TIO renames Ada.Text_IO;
   package DG renames Digraphs;
   package SN is new Set_Of_Names(DG.Node_Idx_With_Null);

   -- reat the graph from the file with the given Filename
   procedure Read(Filename: String; G: out DG.Graph_Type; N: out SN.Set) is

      -- finds the first word in S(Start .. S'Last), delimited by spaces
      procedure Find_Token(S: String; Start: Positive;
                           First: out Positive; Last: out Natural) is

      begin
         First := Start;
         while First <= S'Last and then S(First)= ' ' loop
            First := First + 1;
         end loop;
         Last := First-1;
         while Last < S'Last and then S(Last+1) /= ' ' loop
            Last := Last + 1;
         end loop;
      end Find_Token;

      File: TIO.File_Type;
   begin
      TIO.Open(File, TIO.In_File, Filename);
      TIO.Skip_Line(File, 2);
      -- the first two lines contain header and "
### ...
"
      while not TIO.End_Of_File(File) loop
         declare
            Line: String := TIO.Get_Line(File);
            First: Positive;
            Last: Natural;
            To, From: DG.Node_Index;
         begin
            Find_Token(Line, Line'First, First, Last);
            if Last >= First then
               N.Add(Line(First .. Last), From);
               G.Add_Node(From);
               loop
                  Find_Token(Line, Last+1, First, Last);
                  exit when Last < First;
                     N.Add(Line(First .. Last), To);
                     G.Add_Connection(From, To);
                  end loop;
            end if;
         end;
      end loop;
      TIO.Close(File);
   end Read;

   Graph: DG.Graph_Type;
   Names: SN.Set;

begin
   Read(Ada.Command_Line.Argument(1), Graph, Names);

   -- eliminat self-cycles
   for Start in 1 .. Graph.Node_Count loop
      Graph.Del_Connection(Start, Start);
   end loop;

   -- perform the topological sort and output the result
   declare
      Result:  DG.Node_Vec.Vector;
   begin
      Result := Graph.Top_Sort;
      for Index in Result.First_Index .. Result.Last_Index loop
         TIO.Put(Names.Name(Result.Element(Index)));
         if Index < Result.Last_Index then
            TIO.Put(" -> ");
         end if;
      end loop;
      TIO.New_Line;
   exception
      when DG.Graph_Is_Cyclic =>
         TIO.Put_Line("There is no topological sorting -- the Graph is cyclic!");
   end;
end Toposort;
```


{{out}}
Given the name of the file with the dependencies as the parameter,
Toposort generates the following output:

```txt
std -> synopsys -> ieee -> std_cell_lib -> dware -> dw02 -> gtech -> dw01 -> ramlib ->  des_system_lib -> dw03 -> dw04 -> dw05 -> dw06 -> dw07
```


If the dependencies is circular, the the Toposort tells that:


```txt
There is no topological sorting -- the Graph is cyclic!
```



## Bracmat


```bracmat
(     ("des_system_lib".std synopsys "std_cell_lib" "des_system_lib" dw02 dw01 ramlib ieee)
      (dw01.ieee dw01 dware gtech)
      (dw02.ieee dw02 dware)
      (dw03.std synopsys dware dw03 dw02 dw01 ieee gtech)
      (dw04.dw04 ieee dw01 dware gtech)
      (dw05.dw05 ieee dware)
      (dw06.dw06 ieee dware)
      (dw07.ieee dware)
      (dware.ieee dware)
      (gtech.ieee gtech)
      (ramlib.std ieee)
      ("std_cell_lib".ieee "std_cell_lib")
      (synopsys.)
      (cycle-11.cycle-12)
      (cycle-12.cycle-11)
      (cycle-21.dw01 cycle-22 dw02 dw03)
      (cycle-22.cycle-21 dw01 dw04)
  : ?libdeps
& :?indeps
& ( toposort
  =   A Z res module dependants todo done
    .   !arg:(?todo.?done)
      & ( areDone
        =
          .   !arg:
            |     !arg
                :   ( %@
                    : [%( !module+!done+!indeps:?+(? !sjt ?)+?
                        |   ~(!libdeps:? (!sjt.?) ?)
                          & !sjt !indeps:?indeps
                        )
                    )
                    ?arg
              & areDone$!arg
        )
      & (     !todo
            :   ?A
                (?module.?dependants&areDone$!dependants)
                ( ?Z
                & toposort$(!A !Z.!done !module):?res
                )
          & !res
        | (!todo.!done)
        )
  )
& toposort$(!libdeps.):(?cycles.?res)
& out$("
compile order:" !indeps !res "\ncycles:" !cycles)
);
```

{{out}}

```txt
compile order:
  ieee
  std
  dware
  dw02
  dw05
  dw06
  dw07
  gtech
  dw01
  dw04
  ramlib
  std_cell_lib
  synopsys
  des_system_lib
  dw03

cycles:
  (cycle-11.cycle-12)
  (cycle-12.cycle-11)
  (cycle-21.dw01 cycle-22 dw02 dw03)
  (cycle-22.cycle-21 dw01 dw04)
```



## C

Parses a multiline string and show the compile order.  Note that four lines were added to the example input to form two separate cycles.  Code is a little ugly.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char input[] =
	"des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee\n"
	"dw01             ieee dw01 dware gtech\n"
	"dw02             ieee dw02 dware\n"
	"dw03             std synopsys dware dw03 dw02 dw01 ieee gtech\n"
	"dw04             dw04 ieee dw01 dware gtech\n"
	"dw05             dw05 ieee dware\n"
	"dw06             dw06 ieee dware\n"
	"dw07             ieee dware\n"
	"dware            ieee dware\n"
	"gtech            ieee gtech\n"
	"ramlib           std ieee\n"
	"std_cell_lib     ieee std_cell_lib\n"
	"synopsys\n"
	"cycle_11	  cycle_12\n"
	"cycle_12	  cycle_11\n"
	"cycle_21	  dw01 cycle_22 dw02 dw03\n"
	"cycle_22	  cycle_21 dw01 dw04";

typedef struct item_t item_t, *item;
struct item_t { const char *name; int *deps, n_deps, idx, depth; };

int get_item(item *list, int *len, const char *name)
{
	int i;
	item lst = *list;

	for (i = 0; i < *len; i++)
		if (!strcmp(lst[i].name, name)) return i;

	lst = *list = realloc(lst, ++*len * sizeof(item_t));
	i = *len - 1;
	memset(lst + i, 0, sizeof(item_t));
	lst[i].idx = i;
	lst[i].name = name;
	return i;
}

void add_dep(item it, int i)
{
	if (it->idx == i) return;
	it->deps = realloc(it->deps, (it->n_deps + 1) * sizeof(int));
	it->deps[it->n_deps++] = i;
}

int parse_input(item *ret)
{
	int n_items = 0;
	int i, parent, idx;
	item list = 0;

	char *s, *e, *word, *we;
	for (s = input; ; s = 0) {
		if (!(s = strtok_r(s, "\n", &e))) break;

		for (i = 0, word = s; ; i++, word = 0) {
			if (!(word = strtok_r(word, " \t", &we))) break;
			idx = get_item(&list, &n_items, word);

			if (!i) parent = idx;
			else    add_dep(list + parent, idx);
		}
	}

	*ret = list;
	return n_items;
}

/* recursively resolve compile order; negative means loop */
int get_depth(item list, int idx, int bad)
{
	int max, i, t;

	if (!list[idx].deps)
		return list[idx].depth = 1;

	if ((t = list[idx].depth) < 0) return t;

	list[idx].depth = bad;
	for (max = i = 0; i < list[idx].n_deps; i++) {
		if ((t = get_depth(list, list[idx].deps[i], bad)) < 0) {
			max = t;
			break;
		}
		if (max < t + 1) max = t + 1;
	}
	return list[idx].depth = max;
}

int main()
{
	int i, j, n, bad = -1, max, min;
	item items;
	n = parse_input(&items);

	for (i = 0; i < n; i++)
		if (!items[i].depth && get_depth(items, i, bad) < 0) bad--;

	for (i = 0, max = min = 0; i < n; i++) {
		if (items[i].depth > max) max = items[i].depth;
		if (items[i].depth < min) min = items[i].depth;
	}

	printf("Compile order:\n");
	for (i = min; i <= max; i++) {
		if (!i) continue;

		if (i < 0) printf("   [unorderable]");
		else	   printf("%d:", i);

		for (j = 0; j < n || !putchar('\n'); j++)
			if (items[j].depth == i)
				printf(" %s", items[j].name);
	}

	return 0;
}
```

{{out}} (items on the same row can be compiled together)<lang>Compile order:
   [unorderable] cycle_21 cycle_22
   [unorderable] cycle_11 cycle_12
1: std synopsys ieee
2: std_cell_lib ramlib dware gtech
3: dw02 dw01 dw05 dw06 dw07
4: des_system_lib dw03 dw04
```



## C++


```c

#include <map>
#include <set>

template <typename Goal>
class topological_sorter
{
protected:
	struct relations
	{
		std::size_t
			dependencies;
		std::set<Goal>
			dependents;
	};
	std::map<Goal, relations>
		map;
public:
	void
		add_goal(Goal const& goal)
	{
		map[goal];
	}
	void
		add_dependency(Goal const& goal, Goal const& dependency)
	{
		if(dependency == goal)
			return;
		auto&
			dependents = map[dependency].dependents;
		if(dependents.find(goal) == dependents.end())
		{
			dependents.insert(goal);
			++map[goal].dependencies;
		}
	}
	template <typename Container>
	void
		add_dependencies(Goal const& goal, Container const& dependencies)
	{
		for(auto const& dependency : dependencies)
			add_dependency(goal, dependency);
	}
	template <typename ResultContainer, typename CyclicContainer>
	void
		destructive_sort(ResultContainer& sorted, CyclicContainer& unsortable)
	{
		sorted.clear();
		unsortable.clear();
		for(auto const& lookup : map)
		{
			auto const&
				goal = lookup.first;
   auto const&
				relations = lookup.second;
			if(relations.dependencies == 0)
				sorted.push_back(goal);
		}
		for(std::size_t index = 0; index < sorted.size(); ++index)
			for(auto const& goal : map[sorted[index]].dependents)
				if(--map[goal].dependencies == 0)
					sorted.push_back(goal);
		for(auto const& lookup : map)
		{
			auto const&
				goal = lookup.first;
   auto const&
				relations = lookup.second;
			if(relations.dependencies != 0)
				unsortable.push_back(goal);
		}
	}
	template <typename ResultContainer, typename CyclicContainer>
	void
		sort(ResultContainer& sorted, CyclicContainer& unsortable)
	{
		topological_sorter<Goal>
			temporary = *this;
		temporary.destructive_sort(sorted, unsortable);
	}
	void
		clear()
	{
		map.clear();
	}
};

/*
	Example usage with text strings
*/

#include <fstream>
#include <sstream>
#include <iostream>
#include <string>
#include <vector>

using namespace
	std;

void
	display_heading(string const& message)
{
	cout << endl << "~ " << message << " ~" << endl;
}
void
	display_results(string const& input)
{
	topological_sorter<string>
		sorter;
	vector<string>
		sorted,
		unsortable;
	stringstream
		lines(input);
	string
		line;
	while(getline(lines, line))
	{
		stringstream
			buffer(line);
		string
			goal,
			dependency;
		buffer >> goal;
		sorter.add_goal(goal);
		while(buffer >> dependency)
			sorter.add_dependency(goal, dependency);
	}
	sorter.destructive_sort(sorted, unsortable);
	if(sorted.size() == 0)
		display_heading("Error: no independent variables found!");
	else
	{
		display_heading("Result");
		for(auto const& goal : sorted)
			cout << goal << endl;
	}
	if(unsortable.size() != 0)
	{
		display_heading("Error: cyclic dependencies detected!");
		for(auto const& goal : unsortable)
			cout << goal << endl;
	}
}
int
	main(int argc, char** argv)
{
	if(argc == 1)
	{
		string
			example =
		"des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee\n"
		"dw01             ieee dw01 dware gtech\n"
		"dw02             ieee dw02 dware\n"
		"dw03             std synopsys dware dw03 dw02 dw01 ieee gtech\n"
		"dw04             dw04 ieee dw01 dware gtech\n"
		"dw05             dw05 ieee dware\n"
		"dw06             dw06 ieee dware\n"
		"dw07             ieee dware\n"
		"dware            ieee dware\n"
		"gtech            ieee gtech\n"
		"ramlib           std ieee\n"
		"std_cell_lib     ieee std_cell_lib\n"
		"synopsys\n"
		"cycle_11	  cycle_12\n"
		"cycle_12	  cycle_11\n"
		"cycle_21	  dw01 cycle_22 dw02 dw03\n"
		"cycle_22	  cycle_21 dw01 dw04";
		display_heading("Example: each line starts with a goal followed by it's dependencies");
		cout << example << endl;
		display_results(example);
		display_heading("Enter lines of data (press enter when finished)");
		string
			line,
			data;
		while(getline(cin, line) && !line.empty())
			data += line + '\n';
		if(!data.empty())
			display_results(data);
	}
	else while(*(++argv))
	{
		ifstream
			file(*argv);
		typedef istreambuf_iterator<char>
			iterator;
		display_results(string(iterator(file), iterator()));
	}
}


```



## C++17


```c

#include <unordered_map>
#include <unordered_set>
#include <vector>

template <typename ValueType>
class topological_sorter
{
public:
    using value_type = ValueType;

protected:
    struct relations
    {
        std::size_t dependencies { 0 };
        std::unordered_set<value_type> dependents {};
    };

    std::unordered_map<value_type, relations> _map {};

public:
    void add(const value_type &object)
    {
        _map.try_emplace(object, relations {});
    }

    void add(const value_type &object, const value_type &dependency)
    {
        if (dependency == object) return;

        auto &dependents = _map[dependency].dependents;

        if (dependents.find(object) == std::end(dependents))
        {
            dependents.insert(object);

            ++_map[object].dependencies;
        }
    }

    template <typename Container>
    void add(const value_type &object, const Container &dependencies)
    {
        for (auto const &dependency : dependencies) add(object, dependency);
    }

    void add(const value_type &object, const std::initializer_list<value_type> &dependencies)
    {
        add<std::initializer_list<value_type>>(object, dependencies);
    }

    template<typename... Args>
    void add(const value_type &object, const Args&... dependencies)
    {
        (add(object, dependencies), ...);
    }

    auto sort()
    {
        std::vector<value_type> sorted, cycled;
        auto map { _map };

        for (const auto &[object, relations] : map) if (!relations.dependencies) sorted.emplace_back(object);

        for (decltype(std::size(sorted)) idx = 0; idx < std::size(sorted); ++idx)
            for (auto const& object : map[sorted[idx]].dependents)
                if (!--map[object].dependencies) sorted.emplace_back(object);

        for (const auto &[object, relations] : map) if (relations.dependencies) cycled.emplace_back(std::move(object));

        return std::pair(std::move(sorted), std::move(cycled));
    }

    void clear()
    {
        _map.clear();
    }
};

/*
	Example usage with shared_ptr to class
*/
#include <iostream>
#include <memory>

int main()
{
    using namespace std::string_literals;

    struct task
    {
        std::string message;

        task(const std::string &v) : message { v } {}
        ~task() { std::cout << message[0] << " - destroyed" << std::endl; }
    };

    using task_ptr = std::shared_ptr<task>;

    std::vector<task_ptr> tasks
    {
        // defining simple tasks
        std::make_shared<task>("A - depends on B and C"s),    //0
        std::make_shared<task>("B - depends on none"s),       //1
        std::make_shared<task>("C - depends on D and E"s),    //2
        std::make_shared<task>("D - depends on none"s),       //3
        std::make_shared<task>("E - depends on F, G and H"s), //4
        std::make_shared<task>("F - depends on I"s),          //5
        std::make_shared<task>("G - depends on none"s),       //6
        std::make_shared<task>("H - depends on none"s),       //7
        std::make_shared<task>("I - depends on none"s),       //8
    };

    topological_sorter<task_ptr> resolver;

    // now setting relations between them as described above
    resolver.add(tasks[0], { tasks[1], tasks[2] });
    //resolver.add(tasks[1]); // no need for this since the task was already mentioned as a dependency
    resolver.add(tasks[2], { tasks[3], tasks[4] });
    //resolver.add(tasks[3]); // no need for this since the task was already mentioned as a dependency
    resolver.add(tasks[4], tasks[5], tasks[6], tasks[7]); // using templated add with fold expression
    resolver.add(tasks[5], tasks[8]);
    //resolver.add(tasks[6]); // no need for this since the task was already mentioned as a dependency
    //resolver.add(tasks[7]); // no need for this since the task was already mentioned as a dependency

    //resolver.add(tasks[3], tasks[0]); // uncomment this line to test cycled dependency

    const auto &[sorted, cycled] = resolver.sort();

    if (std::empty(cycled))
    {
        for (auto const& d: sorted)
            std::cout << d->message << std::endl;
    }
    else
    {
        std::cout << "Cycled dependencies detected: ";

        for (auto const& d: cycled)
            std::cout << d->message[0] << " ";

        std::cout << std::endl;
    }

    //tasks.clear(); // uncomment this line to distroy all tasks in sorted order.

    std::cout << "exiting..." << std::endl;

    return 0;
}


```


{{out}}<lang>I - depends on none
H - depends on none
G - depends on none
D - depends on none
B - depends on none
F - depends on I
E - depends on F, G and H
C - depends on D and E
A - depends on B and C
exiting...
A - destroyed
B - destroyed
C - destroyed
D - destroyed
E - destroyed
F - destroyed
G - destroyed
H - destroyed
I - destroyed
```

{{out}}(with cycled dependency)<lang>Cycled dependencies detected: A D C
exiting...
A - destroyed
B - destroyed
C - destroyed
D - destroyed
E - destroyed
F - destroyed
G - destroyed
H - destroyed
I - destroyed
```



## C sharp


```csharp

namespace Algorithms
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public class TopologicalSorter<ValueType>
    {
        private class Relations
        {
            public int Dependencies = 0;
            public HashSet<ValueType> Dependents = new HashSet<ValueType>();
        }

        private Dictionary<ValueType, Relations> _map = new Dictionary<ValueType, Relations>();

        public void Add(ValueType obj)
        {
            if (!_map.ContainsKey(obj)) _map.Add(obj, new Relations());
        }

        public void Add(ValueType obj, ValueType dependency)
        {
            if (dependency.Equals(obj)) return;

            if (!_map.ContainsKey(dependency)) _map.Add(dependency, new Relations());

            var dependents = _map[dependency].Dependents;

            if (!dependents.Contains(obj))
            {
                dependents.Add(obj);

                if (!_map.ContainsKey(obj)) _map.Add(obj, new Relations());

                ++_map[obj].Dependencies;
            }
        }

        public void Add(ValueType obj, IEnumerable<ValueType> dependencies)
        {
            foreach (var dependency in dependencies) Add(obj, dependency);
        }

        public void Add(ValueType obj, params ValueType[] dependencies)
        {
            Add(obj, dependencies as IEnumerable<ValueType>);
        }

        public Tuple<IEnumerable<ValueType>, IEnumerable<ValueType>> Sort()
        {
            List<ValueType> sorted = new List<ValueType>(), cycled = new List<ValueType>();
            var map = _map.ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

            sorted.AddRange(map.Where(kvp => kvp.Value.Dependencies == 0).Select(kvp => kvp.Key));

            for (int idx = 0; idx < sorted.Count; ++idx) sorted.AddRange(map[sorted[idx]].Dependents.Where(k => --map[k].Dependencies == 0));

            cycled.AddRange(map.Where(kvp => kvp.Value.Dependencies != 0).Select(kvp => kvp.Key));

            return new Tuple<IEnumerable<ValueType>, IEnumerable<ValueType>>(sorted, cycled);
        }

        public void Clear()
        {
            _map.Clear();
        }
    }

}

/*
	Example usage with Task object
*/

namespace ExampleApplication
{
    using Algorithms;
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public class Task
    {
        public string Message;
    }

    class Program
    {
        static void Main(string[] args)
        {
            List<Task> tasks = new List<Task>
            {
                new Task{ Message = "A - depends on B and C" },    //0
                new Task{ Message = "B - depends on none" },       //1
                new Task{ Message = "C - depends on D and E" },    //2
                new Task{ Message = "D - depends on none" },       //3
                new Task{ Message = "E - depends on F, G and H" }, //4
                new Task{ Message = "F - depends on I" },          //5
                new Task{ Message = "G - depends on none" },       //6
                new Task{ Message = "H - depends on none" },       //7
                new Task{ Message = "I - depends on none" },       //8
            };

            TopologicalSorter<Task> resolver = new TopologicalSorter<Task>();

            // now setting relations between them as described above
            resolver.Add(tasks[0], new[] { tasks[1], tasks[2] });
            //resolver.Add(tasks[1]); // no need for this since the task was already mentioned as a dependency
            resolver.Add(tasks[2], new[] { tasks[3], tasks[4] });
            //resolver.Add(tasks[3]); // no need for this since the task was already mentioned as a dependency
            resolver.Add(tasks[4], tasks[5], tasks[6], tasks[7]);
            resolver.Add(tasks[5], tasks[8]);
            //resolver.Add(tasks[6]); // no need for this since the task was already mentioned as a dependency
            //resolver.Add(tasks[7]); // no need for this since the task was already mentioned as a dependency

            //resolver.Add(tasks[3], tasks[0]); // uncomment this line to test cycled dependency

            var result = resolver.Sort();
            var sorted = result.Item1;
            var cycled = result.Item2;

            if (!cycled.Any())
            {
                foreach (var d in sorted) Console.WriteLine(d.Message);
            }
            else
            {
                Console.Write("Cycled dependencies detected: ");

                foreach (var d in cycled) Console.Write($"{d.Message[0]} ");

                Console.WriteLine();
            }

            Console.WriteLine("exiting...");
        }
    }
}


```


{{out}}<lang>B - depends on none
D - depends on none
G - depends on none
H - depends on none
I - depends on none
F - depends on I
E - depends on F, G and H
C - depends on D and E
A - depends on B and C
exiting...
```

{{out}}(with cycled dependency)<lang>Cycled dependencies detected: A C D
exiting...
```



## Clojure

Here is a quick implementation in Clojure, developed at Java Posse Roundup 2010 in collaboration with Fred Simon, with a bit of subsequent simplification by Joel Neely.

Dependencies are represented by a map from each item to the set of items on which it depends. The first function (<code>dep</code>), builds a dependency map for a single item.

The next few functions (<code>empty-dep</code>, <code>pair-dep</code>, <code>default-deps</code>, <code>declared-deps</code>, and <code>deps</code>) are used to construct the map from a list that alternates items with lists of their dependencies.

The next three functions (<code>no-dep-items</code>, <code>remove-items</code>, and <code>topo-sort-deps</code>) are the core of the topological sort algorithm, which iteratively removes items with no remaining dependencies from the map and "stacks" them onto the result. When the map becomes empty the reversed result is returned. If no dependency-free items can be found, then any non-empty remainder of the map contains cycles.

The last function (<code>topo-sort</code>) is simply a helper which applies <code>topo-sort-deps</code> to a dependency map constructed from the item-and-list-of-dependencies input list.


### ==Implementation==


```clojure
(use 'clojure.set)
(use 'clojure.contrib.seq-utils)

(defn dep
  "Constructs a single-key dependence, represented as a map from
   item to a set of items, ensuring that item is not in the set."
  [item items]
  {item (difference (set items) (list item))})

(defn empty-dep
  "Constructs a single-key dependence from item to an empty set."
  [item]
  (dep item '()))

(defn pair-dep
  "Invokes dep after destructuring item and items from the argument."
  [[item items]]
  (dep item items))

(defn default-deps
  "Constructs a default dependence map taking every item
   in the argument to an empty set"
  [items]
  (apply merge-with union (map empty-dep (flatten items))))

(defn declared-deps
  "Constructs a dependence map from a list containaining
   alternating items and list of their predecessor items."
  [items]
  (apply merge-with union (map pair-dep (partition 2 items))))

(defn deps
  "Constructs a full dependence map containing both explicitly
   represented dependences and default empty dependences for
   items without explicit predecessors."
  [items]
  (merge (default-deps items) (declared-deps items)))

(defn no-dep-items
  "Returns all keys from the argument which have no (i.e. empty) dependences."
  [deps]
  (filter #(empty? (deps %)) (keys deps)))

(defn remove-items
  "Returns a dependence map with the specified items removed from keys
   and from all dependence sets of remaining keys."
  [deps items]
  (let [items-to-remove (set items)
        remaining-keys  (difference (set (keys deps)) items-to-remove)
        remaining-deps  (fn [x] (dep x (difference (deps x) items-to-remove)))]
    (apply merge (map remaining-deps remaining-keys))))

(defn topo-sort-deps
  "Given a dependence map, returns either a list of items in which each item
   follows all of its predecessors, or a string showing the items among which
   there is a cyclic dependence preventing a linear order."
  [deps]
  (loop [remaining-deps deps
         result         '()]
    (if (empty? remaining-deps)
        (reverse result)
        (let [ready-items (no-dep-items remaining-deps)]
          (if (empty? ready-items)
              (str "ERROR: cycles remain among " (keys remaining-deps))
              (recur (remove-items remaining-deps ready-items)
                     (concat ready-items result)))))))

(defn topo-sort
  "Given a list of alternating items and predecessor lists, constructs a
   full dependence map and then applies topo-sort-deps to that map."
  [items]
  (topo-sort-deps (deps items)))

```


Examples of sortable and non-sortable data:


```clojure
(def good-sample
  '(:des_system_lib   (:std :synopsys :std_cell_lib :des_system_lib :dw02 :dw01 :ramlib :ieee)
    :dw01             (:ieee :dw01 :dware :gtech)
    :dw02             (:ieee :dw02 :dware)
    :dw03             (:std :synopsys :dware :dw03 :dw02 :dw01 :ieee :gtech)
    :dw04             (:dw04 :ieee :dw01 :dware :gtech)
    :dw05             (:dw05 :ieee :dware)
    :dw06             (:dw06 :ieee :dware)
    :dw07             (:ieee :dware)
    :dware            (:ieee :dware)
    :gtech            (:ieee :gtech)
    :ramlib           (:std :ieee)
    :std_cell_lib     (:ieee :std_cell_lib)
    :synopsys         ()))

(def cyclic-dependence
  '(:dw01 (:dw04)))

(def bad-sample
  (concat cyclic-dependence good-sample))
```


====={{out}}=====

```clojure
Clojure 1.1.0
1:1 user=> #<Namespace topo>
1:2 topo=> (topo-sort good-sample)
(:std :synopsys :ieee :gtech :ramlib :dware :std_cell_lib :dw07 :dw06 :dw05 :dw01 :dw02 :des_system_lib :dw03 :dw04)
1:3 topo=> (topo-sort bad-sample)
"ERROR: cycles remain among (:dw01 :dw04 :dw03 :des_system_lib)"
```



## CoffeeScript


```coffeescript

toposort = (targets) ->
  # targets is hash of sets, where keys are parent nodes and
  # where values are sets that contain nodes that must precede the parent

  # Start by identifying obviously independent nodes
  independents = []
  do ->
    for k of targets
      if targets[k].cnt == 0
        delete targets[k]
        independents.push k

  # Note reverse dependencies for theoretical O(M+N) efficiency.
  reverse_deps = []
  do ->
    for k of targets
      for child of targets[k].v
        reverse_deps[child] ?= []
        reverse_deps[child].push k

  # Now be greedy--start with independent nodes, then start
  # breaking dependencies, and keep going as long as we still
  # have independent nodes left.
  result = []
  while independents.length > 0
    k = independents.pop()
    result.push k
    for parent in reverse_deps[k] or []
      set_remove targets[parent], k
      if targets[parent].cnt == 0
        independents.push parent
        delete targets[parent]

  # Show unresolvable dependencies
  for k of targets
    console.log "WARNING: node #{k} is part of cyclical dependency"
  result

parse_deps = ->
  # parse string data, remove self-deps, and fill in gaps
  #
  # e.g. this would transform {a: "a b c", d: "e"} to this:
  #   a: set(b, c)
  #   b: set()
  #   c: set()
  #   d: set(e)
  #   e: set()
  targets = {}
  deps = set()
  for k, v of data
    targets[k] = set()
    children = v.split(' ')
    for child in children
      continue if child == ''
      set_add targets[k], child unless child == k
      set_add deps, child

  # make sure even leaf nodes are in targets
  for dep of deps.v
    if dep not of targets
      targets[dep] = set()
  targets

set = ->
  cnt: 0
  v: {}

set_add = (s, e) ->
  return if s.v[e]
  s.cnt += 1
  s.v[e] = true

set_remove = (s, e) ->
  return if !s.v[e]
  s.cnt -= 1
  delete s.v[e]

data =
  des_system_lib:   "std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee"
  dw01:             "ieee dw01 dware gtech"
  dw02:             "ieee dw02 dware"
  dw03:             "std synopsys dware dw03 dw02 dw01 ieee gtech"
  dw04:             "dw04 ieee dw01 dware gtech"
  dw05:             "dw05 ieee dware"
  dw06:             "dw06 ieee dware"
  dw07:             "ieee dware"
  dware:            "ieee dware"
  gtech:            "ieee gtech"
  ramlib:           "std ieee"
  std_cell_lib:     "ieee std_cell_lib"
  synopsys:         ""


targets = parse_deps()
console.log toposort targets


```



## Common Lisp



```lisp
(defun topological-sort (graph &key (test 'eql))
  "Graph is an association list whose keys are objects and whose
values are lists of objects on which the corresponding key depends.
Test is used to compare elements, and should be a suitable test for
hash-tables.  Topological-sort returns two values.  The first is a
list of objects sorted toplogically.  The second is a boolean
indicating whether all of the objects in the input graph are present
in the topological ordering (i.e., the first value)."
  (let ((entries (make-hash-table :test test)))
    (flet ((entry (vertex)
             "Return the entry for vertex.  Each entry is a cons whose
              car is the number of outstanding dependencies of vertex
              and whose cdr is a list of dependants of vertex."
             (multiple-value-bind (entry presentp) (gethash vertex entries)
               (if presentp entry
                 (setf (gethash vertex entries) (cons 0 '()))))))
      ;; populate entries initially
      (dolist (vertex graph)
        (destructuring-bind (vertex &rest dependencies) vertex
          (let ((ventry (entry vertex)))
            (dolist (dependency dependencies)
              (let ((dentry (entry dependency)))
                (unless (funcall test dependency vertex)
                  (incf (car ventry))
                  (push vertex (cdr dentry))))))))
      ;; L is the list of sorted elements, and S the set of vertices
      ;; with no outstanding dependencies.
      (let ((L '())
            (S (loop for entry being each hash-value of entries
                     using (hash-key vertex)
                     when (zerop (car entry)) collect vertex)))
        ;; Until there are no vertices with no outstanding dependencies,
        ;; process vertices from S, adding them to L.
        (do* () ((endp S))
          (let* ((v (pop S)) (ventry (entry v)))
            (remhash v entries)
            (dolist (dependant (cdr ventry) (push v L))
              (when (zerop (decf (car (entry dependant))))
                (push dependant S)))))
        ;; return (1) the list of sorted items, (2) whether all items
        ;; were sorted, and (3) if there were unsorted vertices, the
        ;; hash table mapping these vertices to their dependants
        (let ((all-sorted-p (zerop (hash-table-count entries))))
          (values (nreverse L)
                  all-sorted-p
                  (unless all-sorted-p
                    entries)))))))
```


Provided example in which all items can be sorted:


```lisp>
 (defparameter *dependency-graph*
  '((des-system-lib   std synopsys std-cell-lib des-system-lib dw02 dw01 ramlib ieee)
    (dw01             ieee dw01 dware gtech)
    (dw02             ieee dw02 dware)
    (dw03             std synopsys dware dw03 dw02 dw01 ieee gtech)
    (dw04             dw04 ieee dw01 dware gtech)
    (dw05             dw05 ieee dware)
    (dw06             dw06 ieee dware)
    (dw07             ieee dware)
    (dware            ieee dware)
    (gtech            ieee gtech)
    (ramlib           std ieee)
    (std-cell-lib     ieee std-cell-lib)
    (synopsys)))
*DEPENDENCY-GRAPH*

> (topological-sort *dependency-graph*)
(IEEE DWARE DW02 DW05 DW06 DW07 GTECH DW01 DW04 STD-CELL-LIB SYNOPSYS STD DW03 RAMLIB DES-SYSTEM-LIB)
T
NIL
```


Provided example with <code>dw04</code> added to the dependencies of <code>dw01</code>.  Some vertices are ordered, but the second return is <code>nil</code>, indicating that not all vertices could be sorted.  The third return value is the hash table containing entries for the four vertices that couldn't be sorted.  (The variable <code>[http://www.lispworks.com/documentation/HyperSpec/Body/v_sl_sls.htm /]</code> stores the list of values produced by the last form, and <code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_descri.htm describe]</code> prints information about an object.)


```lisp>
 (defparameter *dependency-graph*
  '((des-system-lib   std synopsys std-cell-lib des-system-lib dw02 dw01 ramlib ieee)
    (dw01             ieee dw01 dw04 dware gtech)
    (dw02             ieee dw02 dware)
    (dw03             std synopsys dware dw03 dw02 dw01 ieee gtech)
    (dw04             dw04 ieee dw01 dware gtech)
    (dw05             dw05 ieee dware)
    (dw06             dw06 ieee dware)
    (dw07             ieee dware)
    (dware            ieee dware)
    (gtech            ieee gtech)
    (ramlib           std ieee)
    (std-cell-lib     ieee std-cell-lib)
    (synopsys)))
*DEPENDENCY-GRAPH*

> (topological-sort *dependency-graph*)
(IEEE DWARE DW02 DW05 DW06 DW07 GTECH STD-CELL-LIB SYNOPSYS STD RAMLIB)
NIL
#<EQL Hash Table{4} 200C9023>

> (describe (third /))

#<EQL Hash Table{4} 200C9023> is a HASH-TABLE
DW01                (1 DW04 DW03 DES-SYSTEM-LIB)
DW04                (1 DW01)
DW03                (1)
DES-SYSTEM-LIB      (1)
```



## D

{{trans|Python}}

```d
import std.stdio, std.string, std.algorithm, std.range;

final class ArgumentException : Exception {
    this(string text) pure nothrow @safe /*@nogc*/ {
        super(text);
    }
}

alias TDependencies = string[][string];

string[][] topoSort(TDependencies d) pure /*nothrow @safe*/ {
    foreach (immutable k, v; d)
        d[k] = v.sort().uniq.filter!(s => s != k).array;
    foreach (immutable s; d.byValue.join.sort().uniq)
        if (s !in d)
            d[s] = [];

    string[][] sorted;
    while (true) {
        string[] ordered;

        foreach (immutable item, const dep; d)
            if (dep.empty)
                ordered ~= item;
        if (!ordered.empty)
            sorted ~= ordered.sort().release;
        else
            break;

        TDependencies dd;
        foreach (immutable item, const dep; d)
            if (!ordered.canFind(item))
                dd[item] = dep.dup.filter!(s => !ordered.canFind(s)).array;
        d = dd;
    }

    //if (!d.empty)
    if (d.length > 0)
        throw new ArgumentException(format(
            "A cyclic dependency exists amongst:\n%s", d));

    return sorted;
}

void main() {
    immutable data =
"des_system_lib std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01           ieee dw01 dware gtech
dw02           ieee dw02 dware
dw03           std synopsys dware dw03 dw02 dw01 ieee gtech
dw04           dw04 ieee dw01 dware gtech
dw05           dw05 ieee dware
dw06           dw06 ieee dware
dw07           ieee dware
dware          ieee dware
gtech          ieee gtech
ramlib         std ieee
std_cell_lib   ieee std_cell_lib
synopsys";

    TDependencies deps;
    foreach (immutable line; data.splitLines)
        deps[line.split[0]] = line.split[1 .. $];

    auto depw = deps.dup;
    foreach (immutable idx, const subOrder; depw.topoSort)
        writefln("#%d : %s", idx + 1,  subOrder);

    writeln;
    depw = deps.dup;
    depw["dw01"] ~= "dw04";
    foreach (const subOrder; depw.topoSort) // Should throw.
        subOrder.writeln;
}
```

{{out}}

```txt
#1 : ["ieee", "std", "synopsys"]
#2 : ["dware", "gtech", "ramlib", "std_cell_lib"]
#3 : ["dw01", "dw02", "dw05", "dw06", "dw07"]
#4 : ["des_system_lib", "dw03", "dw04"]
topo.ArgumentException@topo.d(7): A cyclic dependency exists amongst:
[dw01:[dw04],des_system_lib:[dw01],dw03:[dw01],dw04:[dw01]]
----------------
...\topo.d(71): _Dmain
----------------
```



## E



```e>def makeQueue := <elib:vat.makeQueue


def topoSort(data :Map[any, Set[any]]) {
    # Tables of nodes and edges
    def forwardEdges := [].asMap().diverge()
    def reverseCount := [].asMap().diverge()

    def init(node) {
      reverseCount[node] := 0
      forwardEdges[node] := [].asSet().diverge()
    }
    for node => deps in data {
        init(node)
        for dep in deps { init(dep) }
    }

    # 'data' holds the dependencies. Compute the other direction.
    for node => deps in data {
        for dep ? (dep != node) in deps {
            forwardEdges[dep].addElement(node)
            reverseCount[node] += 1
        }
    }

    # Queue containing all elements that have no (initial or remaining) incoming edges
    def ready := makeQueue()
    for node => ==0 in reverseCount {
      ready.enqueue(node)
    }

    var result := []

    while (ready.optDequeue() =~ node :notNull) {
        result with= node
        for next in forwardEdges[node] {
            # Decrease count of incoming edges and enqueue if none
            if ((reverseCount[next] -= 1).isZero()) {
                ready.enqueue(next)
            }
        }
        forwardEdges.removeKey(node)
    }

    if (forwardEdges.size().aboveZero()) {
        throw(`Topological sort failed: $forwardEdges remains`)
    }

    return result
}
```



```e
pragma.enable("accumulator")

def dataText := "\
des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01             ieee dw01 dware gtech
dw02             ieee dw02 dware
dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
dw04             dw04 ieee dw01 dware gtech
dw05             dw05 ieee dware
dw06             dw06 ieee dware
dw07             ieee dware
dware            ieee dware
gtech            ieee gtech
ramlib           std ieee
std_cell_lib     ieee std_cell_lib
synopsys\
"

def data := accum [].asMap() for rx`(@item.{17})(@deps.*)` in dataText.split("\n") { _.with(item.trim(), deps.split(" ").asSet()) }

println(topoSort(data))
```


{{out}}
<code>["std", "synopsys", "ieee", "dware", "gtech", "ramlib", "std_cell_lib", "dw02", "dw05", "dw06", "dw07", "dw01", "des_system_lib", "dw03", "dw04"]</code>


## EchoLisp

We use the low-level primitives of the 'graph' library to build the directed graph and implement the topological sort.

''' Data


```lisp

(define dependencies
'((des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee)
(dw01             ieee dw01 dware gtech) ;; bad graph add dw04
(dw02             ieee dw02 dware )
(dw03             std synopsys dware dw03 dw02 dw01 ieee gtech)
(dw04             dw04 ieee dw01 dware gtech)
(dw05             dw05 ieee dware)
(dw06             dw06 ieee dware)
(dw07             ieee dware)
(dware            ieee dware)
(gtech            ieee gtech)
(ramlib           std ieee )
(std_cell_lib     ieee std_cell_lib)
(synopsys         )))


;; build dependency graph
;;  a depends on b
;;  add arc (arrow) a --> b
(lib 'graph.lib)
(define (a->b  g a b)
		(unless (equal? a b)
		(graph-make-arc g (graph-make-vertex g a) (graph-make-vertex g b))))

(define (add-dependencies g dep-list)
(for* ((dep dep-list)  (b (rest dep))) (a->b  g b (first dep))))

```

'''Implementation

Remove all vertices with in-degree = 0, until to one left. (in-degree = number of arrows to a vertex)

```lisp

;; topological sort
;;
;; Complexity O (# of vertices + # of edges)

(define (t-sort g)
	(stack 'Z) ; vertices of d°(0)
	(stack 'S) ; ordered result

;; mark all vertices with their in-degree = # of incoming arcs
;; push all vertices u such as d°(u) = 0
	(for ((u g)) (mark u (graph-vertex-indegree g u))
	             (when (zero? (mark? u)) (push 'Z u)))

;pop a d°(0) vertex u - add it to result
;decrement in-degree of all v vertices u->v
; if d°(v) = 0, push it

(while (not (stack-empty? 'Z))
  			(let  (( u (pop 'Z)))
 			(push 'S u)
			 (for ((v (graph-vertex-out g u)))
				  (mark v (1- (mark? v)))
				  (when (zero? (mark? v)) (push 'Z v)))))

;; finish
		 (writeln 't-sort (map vertex-label (stack->list 'S)))

;; check no one remaining
		(for ((u g))
		(unless (zero? (mark? u))
		(error " ♻️ t-sort:cyclic" (map vertex-label (graph-cycle g))))))


```

{{Out}}

```lisp

(define g (make-graph "VHDL"))
(add-dependencies g dependencies)
(graph-print g)

(t-sort g)
→ t-sort     (std synopsys ieee dware dw02 dw05 dw06 dw07 gtech dw01 dw03 dw04 ramlib
             std_cell_lib des_system_lib)

;; Error case
;; add dw01 -> dw04
(t-sort g)
t-sort     (std synopsys ieee dware dw02 dw05 dw06 dw07 gtech ramlib std_cell_lib)
⛔️ error: ♻️ t-sort:cyclic (dw04 dw01)

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Topological do
  def sort(library) do
    g = :digraph.new
    Enum.each(library, fn {l,deps} ->
      :digraph.add_vertex(g,l)           # noop if library already added
      Enum.each(deps, fn d -> add_dependency(g,l,d) end)
    end)
    if t = :digraph_utils.topsort(g) do
      print_path(t)
    else
      IO.puts "Unsortable contains circular dependencies:"
      Enum.each(:digraph.vertices(g), fn v ->
        if vs = :digraph.get_short_cycle(g,v), do: print_path(vs)
      end)
    end
  end

  defp print_path(l), do: IO.puts Enum.join(l, " -> ")

  defp add_dependency(_g,l,l), do: :ok
  defp add_dependency(g,l,d) do
    :digraph.add_vertex(g,d)   # noop if dependency already added
    :digraph.add_edge(g,d,l)   # Dependencies represented as an edge d -> l
  end
end

libraries = [
  des_system_lib:   ~w[std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee]a,
  dw01:             ~w[ieee dw01 dware gtech]a,
  dw02:             ~w[ieee dw02 dware]a,
  dw03:             ~w[std synopsys dware dw03 dw02 dw01 ieee gtech]a,
  dw04:             ~w[dw04 ieee dw01 dware gtech]a,
  dw05:             ~w[dw05 ieee dware]a,
  dw06:             ~w[dw06 ieee dware]a,
  dw07:             ~w[ieee dware]a,
  dware:            ~w[ieee dware]a,
  gtech:            ~w[ieee gtech]a,
  ramlib:           ~w[std ieee]a,
  std_cell_lib:     ~w[ieee std_cell_lib]a,
  synopsys:         []
]
Topological.sort(libraries)

IO.puts ""
bad_libraries = Keyword.update!(libraries, :dw01, &[:dw04 | &1])
Topological.sort(bad_libraries)
```


{{out}}

```txt

std -> synopsys -> ieee -> dware -> dw02 -> dw05 -> gtech -> dw01 -> dw03 -> dw04 -> ramlib -> std_cell_lib -> des_system_lib -> dw06 -> dw07

Unsortable contains circular dependencies:
dw04 -> dw01 -> dw04
dw01 -> dw04 -> dw01

```



## Erlang


```erlang

-module(topological_sort).
-compile(export_all).

-define(LIBRARIES,
        [{des_system_lib,   [std, synopsys, std_cell_lib, des_system_lib, dw02, dw01, ramlib, ieee]},
         {dw01,             [ieee, dw01, dware, gtech]},
         {dw02,             [ieee, dw02, dware]},
         {dw03,             [std, synopsys, dware, dw03, dw02, dw01, ieee, gtech]},
         {dw04,             [dw04, ieee, dw01, dware, gtech]},
         {dw05,             [dw05, ieee, dware]},
         {dw06,             [dw06, ieee, dware]},
         {dw07,             [ieee, dware]},
         {dware,            [ieee, dware]},
         {gtech,            [ieee, gtech]},
         {ramlib,           [std, ieee]},
         {std_cell_lib,     [ieee, std_cell_lib]},
         {synopsys,         []}]).

-define(BAD_LIBRARIES,
        [{des_system_lib,   [std, synopsys, std_cell_lib, des_system_lib, dw02, dw01, ramlib, ieee]},
         {dw01,             [ieee, dw01, dw04, dware, gtech]},
         {dw02,             [ieee, dw02, dware]},
         {dw03,             [std, synopsys, dware, dw03, dw02, dw01, ieee, gtech]},
         {dw04,             [dw04, ieee, dw01, dware, gtech]},
         {dw05,             [dw05, ieee, dware]},
         {dw06,             [dw06, ieee, dware]},
         {dw07,             [ieee, dware]},
         {dware,            [ieee, dware]},
         {gtech,            [ieee, gtech]},
         {ramlib,           [std, ieee]},
         {std_cell_lib,     [ieee, std_cell_lib]},
         {synopsys,         []}]).

main() ->
    top_sort(?LIBRARIES),
    top_sort(?BAD_LIBRARIES).

top_sort(Library) ->
    G = digraph:new(),
    lists:foreach(fun ({L,Deps}) ->
                          digraph:add_vertex(G,L), % noop if library already added
                          lists:foreach(fun (D) ->
                                                add_dependency(G,L,D)
                                        end, Deps)
                  end, Library),
    T = digraph_utils:topsort(G),
    case T of
        false ->
            io:format("Unsortable contains circular dependencies:~n",[]),
            lists:foreach(fun (V) ->
                                  case digraph:get_short_cycle(G,V) of
                                      false ->
                                          ok;
                                      Vs ->
                                          print_path(Vs)
                                  end
                          end, digraph:vertices(G));
        _ ->
            print_path(T)
    end.

print_path(L) ->
            lists:foreach(fun (V) -> io:format("~s -> ",[V]) end,
                          lists:sublist(L,length(L)-1)),
            io:format("~s~n",[lists:last(L)]).

add_dependency(_G,_L,_L) ->
    ok;
add_dependency(G,L,D) ->
    digraph:add_vertex(G,D), % noop if dependency already added
    digraph:add_edge(G,D,L). % Dependencies represented as an edge D -> L

```


{{out}}

```erlang

62> topological_sort:main().
synopsys -> std -> ieee -> dware -> dw02 -> dw05 -> ramlib -> std_cell_lib -> dw06 -> dw07 -> gtech -> dw01 -> des_system_lib -> dw03 -> dw04
Unsortable contains circular dependencies:
dw04 -> dw01 -> dw04
dw01 -> dw04 -> dw01
ok
```


Erlang has a built in digraph library and datastructure. ''digraph_utils'' contains the ''top_sort'' function which provides a topological sort of the vertices or returns false if it's not possible (due to circular references).
The ''digraph'' module contains ''get_short_cycle'' which returns the shortest cycle involving a vertex.


## Forth

Provides syntactical sugar for inputting the data in a way similar to the way given in the task description.

Implementation: Each node (with dependencies) goes through three
states: At the start, it contains an execution token (xt, similar to a function pointer) that calls all before-nodes.
At the start of that, the xt called by the node changes to
PROCESSING; if that is ever called, there is a cycle (or
self-reference), and if it is not a self-reference, the cycle is
printed.  When the processing of the before-nodes is complete, the
present node is printed, and the xt changes to DROP, so any further
processing of the node does nothing.

This implements depth-first search with PROCESSING being the temporary mark, and DROP being the permanent
mark.

The cool thing about this implementation is that we don't need a
single conditional branch for topologically sorting the dependencies
of a single node; there are a few for deciding what to output on a
cycle, but if we are happy with more primitive output, we can get rid
of that; we do have EXECUTE instead, so we don't get rid of branch
mispredictions, but given our representation of the dependencies, we
need the indirect branch anyway.

A Forth-specific (although unidiomatic) feature is that we can recognize self-references and print
cycles without building an extra data structure, because the chain
of nodes we are looking at is on the data stack.

Another Forth feature is that we use the dictionary as symbol table
for input processing: Each node is turned into a Forth word.  Also,
the list of dependencies is turned into an anonymous colon
definition rather than some list or array.

This code uses a number of Gforth extensions, some just as minor
conveniences, some more substantial (although nothing that could not
be replaced with a few lines of standard code).

{{works with|Gforth}}

```forth
variable nodes 0 nodes ! \ linked list of nodes

: node. ( body -- )
    body> >name name>string type ;

: nodeps ( body -- )
    \ the word referenced by body has no (more) dependencies to resolve
    ['] drop over ! node. space ;

: processing ( body1 ... bodyn body -- body1 ... bodyn )
    \ the word referenced by body is in the middle of resolving dependencies
    2dup <> if \ unless it is a self-reference (see task description)
	['] drop over !
	." (cycle: " dup node. >r 1 begin \ print the cycle
	    dup pick dup r@ <> while
		space node. 1+ repeat
	." ) " 2drop r>
	then drop ;

: >processing ( body -- body )
    ['] processing over ! ;

: node ( "name" -- )
    \ define node "name" and initialize it to having no dependences
    create
    ['] nodeps , \ on definition, a node has no dependencies
    nodes @ , lastxt nodes ! \ linked list of nodes
  does> ( -- )
    dup @ execute ; \ perform xt associated with node

: define-nodes ( "names" <newline> -- )
    \ define all the names that don't exist yet as nodes
    begin
	parse-name dup while
	    2dup find-name 0= if
		2dup nextname node then
	    2drop repeat
    2drop ;

: deps ( "name" "deps" <newline> -- )
    \ name is after deps.  Implementation: Define missing nodes, then
    \ define a colon definition for
    >in @ define-nodes >in !
    ' :noname ]] >processing [[ source >in @ /string evaluate ]] nodeps ; [[
    swap >body ! 0 parse 2drop ;

: all-nodes ( -- )
    \ call all nodes, and they then print their dependences and themselves
    nodes begin
	@ dup while
	    dup execute
	    >body cell+ repeat
    drop ;

deps des_system_lib std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
deps dw01           ieee dw01 dware gtech
deps dw02           ieee dw02 dware
deps dw03           std synopsys dware dw03 dw02 dw01 ieee gtech
deps dw04           dw04 ieee dw01 dware gtech
deps dw05           dw05 ieee dware
deps dw06           dw06 ieee dware
deps dw07           ieee dware
deps dware          ieee dware
deps gtech          ieee gtech
deps ramlib         std ieee
deps std_cell_lib   ieee std_cell_lib
deps synopsys
\ to test the cycle recognition (overwrites dependences for dw1 above)
deps dw01           ieee dw01 dware gtech dw04

all-nodes
```


{{out}}

```txt
ieee dware dw07 dw06 dw05 gtech (cycle: dw04 dw01) dw01 dw04 std synopsys dw02 dw03 ramlib std_cell_lib des_system_lib
```



## Fortran


### FORTRAN 77

Main routine for topological sort.
''Input'' : IDEP is an array ND x 2 of dependencies, with IDEP(I,1) depending on IDEP(I,2).
NL is the number of libraries to sort, ND the number of dependencies, one for each pair of ordered libraries.
Array IPOS is used internally by the routine, to maintain a list of positions of libraries in IORD.
''Output'' : IORD(1:NO) is the compile order, and IORD(NO+1:NL) contains unordered libraries.

This implementation is not optimal: for each ''level'' of dependency (for example A -> B -> C counts as three levels), there is a loop through all dependencies in IDEP.
It would be possible to optimize a bit, without changing the main idea, by first sorting IDEP according to first column, and using more temporary space, keeping track of where is located data in IDEP for each library (all dependencies of a same library being grouped).

```fortran
      SUBROUTINE TSORT(NL,ND,IDEP,IORD,IPOS,NO)
      IMPLICIT NONE
      INTEGER NL,ND,NO,IDEP(ND,2),IORD(NL),IPOS(NL),I,J,K,IL,IR,IPL,IPR
      DO 10 I=1,NL
      IORD(I)=I
   10 IPOS(I)=I
      K=1
   20 J=K
      K=NL+1
      DO 30 I=1,ND
      IL=IDEP(I,1)
      IR=IDEP(I,2)
      IPL=IPOS(IL)
      IPR=IPOS(IR)
      IF(IL.EQ.IR .OR. IPL.GE.K .OR. IPL.LT.J .OR. IPR.LT.J) GO TO 30
      K=K-1
      IPOS(IORD(K))=IPL
      IPOS(IL)=K
      IORD(IPL)=IORD(K)
      IORD(K)=IL
   30 CONTINUE
      IF(K.GT.J) GO TO 20
      NO=J-1
      END
```


An example. Dependencies are encoded to make program shorter (in array ICODE).


```fortran
      PROGRAM EX_TSORT
      IMPLICIT NONE
      INTEGER NL,ND,NC,NO,IDEP,IORD,IPOS,ICODE,I,J,IL,IR
      PARAMETER(NL=15,ND=44,NC=69)
      CHARACTER*(20) LABEL
      DIMENSION IDEP(ND,2),LABEL(NL),IORD(NL),IPOS(NL),ICODE(NC)
      DATA LABEL/'DES_SYSTEM_LIB','DW01','DW02','DW03','DW04','DW05',
     1 'DW06','DW07','DWARE','GTECH','RAMLIB','STD_CELL_LIB','SYNOPSYS',
     2 'STD','IEEE'/
      DATA ICODE/1,14,13,12,1,3,2,11,15,0,2,15,2,9,10,0,3,15,3,9,0,4,14,
     213,9,4,3,2,15,10,0,5,5,15,2,9,10,0,6,6,15,9,0,7,7,15,9,0,8,15,9,0,
     39,15,9,0,10,15,10,0,11,14,15,0,12,15,12,0,0/

C DECODE DEPENDENCIES AND BUILD IDEP ARRAY
      I=0
      J=0
   10 I=I+1
      IL=ICODE(I)
      IF(IL.EQ.0) GO TO 30
   20 I=I+1
      IR=ICODE(I)
      IF(IR.EQ.0) GO TO 10
      J=J+1
      IDEP(J,1)=IL
      IDEP(J,2)=IR
      GO TO 20
   30 CONTINUE

C SORT LIBRARIES ACCORDING TO DEPENDENCIES (TOPOLOGICAL SORT)
      CALL TSORT(NL,ND,IDEP,IORD,IPOS,NO)

      PRINT*,'COMPILE ORDER'
      DO 40 I=1,NO
   40 PRINT*,LABEL(IORD(I))
      PRINT*,'UNORDERED LIBRARIES'
      DO 50 I=NO+1,NL
   50 PRINT*,LABEL(IORD(I))
      END
```


{{out}}

```txt

 COMPILE ORDER
 IEEE
 STD
 SYNOPSYS
 STD_CELL_LIB
 RAMLIB
 GTECH
 DWARE
 DW07
 DW06
 DW05
 DW02
 DW01
 DW04
 DW03
 DES_SYSTEM_LIB
 UNORDERED LIBRARIES

```


{{out}} with alternate input (DW01 depends also on DW04):

```txt

 COMPILE ORDER
 IEEE
 STD
 SYNOPSYS
 STD_CELL_LIB
 RAMLIB
 GTECH
 DWARE
 DW07
 DW06
 DW05
 DW02
 UNORDERED LIBRARIES
 DW04
 DW03
 DW01
 DES_SYSTEM_LIB

```


### Modern Fortran

A modern Fortran (95-2008) version of the TSORT subroutine is shown here (note that the IPOS array is not an input).

```fortran
subroutine tsort(nl,nd,idep,iord,no)

  implicit none

  integer,intent(in) :: nl
  integer,intent(in) :: nd
  integer,dimension(nd,2),intent(in) :: idep
  integer,dimension(nl),intent(out) :: iord
  integer,intent(out) :: no

  integer :: i,j,k,il,ir,ipl,ipr,ipos(nl)

  do i=1,nl
    iord(i)=i
    ipos(i)=i
  end do
  k=1
  do
    j=k
    k=nl+1
    do i=1,nd
      il=idep(i,1)
      ir=idep(i,2)
      ipl=ipos(il)
      ipr=ipos(ir)
      if (il==ir .or. ipl>=k .or. ipl<j .or. ipr<j) cycle
      k=k-1
      ipos(iord(k))=ipl
      ipos(il)=k
      iord(ipl)=iord(k)
      iord(k)=il
    end do
    if (k<=j) exit
  end do
  no=j-1

end subroutine tsort

```



## FunL


```funl
def topsort( graph ) =
  val L = seq()
  val S = seq()
  val g = dict( graph )

  for (v, es) <- g
    g(v) = seq( es )

  for (v, es) <- g if es.isEmpty()
    S.append( v )

  while not S.isEmpty()
    val n = S.remove( 0 )
    L.append( n )

    for (m, es) <- g if n in es
      if (es -= n).isEmpty()
        S.append( m )

  for (v, es) <- g
    if not es.isEmpty()
      return None

  Some( L.toList() )

dependencies = '''
  des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
  dw01             ieee dw01 dware gtech
  dw02             ieee dw02 dware
  dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
  dw04             dw04 ieee dw01 dware gtech
  dw05             dw05 ieee dware
  dw06             dw06 ieee dware
  dw07             ieee dware
  dware            ieee dware
  gtech            ieee gtech
  ramlib           std ieee
  std_cell_lib     ieee std_cell_lib
  synopsys
  '''

// convert dependencies data into a directed graph
graph = dict()
deps = set()

for l <- WrappedString( dependencies ).lines() if l.trim() != ''
  case list(l.trim().split('\\s+')) of
    [a] -> graph(a) = []
    h:t ->
      d = set( t )
      d -= h                  // remove self dependencies
      graph(h) = d
      deps ++= t

// add graph vertices for dependencies not appearing in left column
for e <- deps if e not in graph
  graph(e) = []

case topsort( graph ) of
  None -> println( 'un-orderable' )
  Some( ordering ) -> println( ordering )
```


{{out}}


```txt

[synopsys, ieee, std, dware, std_cell_lib, gtech, ramlib, dw06, dw05, dw02, dw07, dw01, dw03, dw04, des_system_lib]

```



## Go


### Kahn


```go
package main

import (
    "fmt"
    "strings"
)

var data = `
LIBRARY          LIBRARY DEPENDENCIES

### ====          =================

des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01             ieee dw01 dware gtech
dw02             ieee dw02 dware
dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
dw04             dw04 ieee dw01 dware gtech
dw05             dw05 ieee dware
dw06             dw06 ieee dware
dw07             ieee dware
dware            ieee dware
gtech            ieee gtech
ramlib           std ieee
std_cell_lib     ieee std_cell_lib
synopsys         `

func main() {
    g, in, err := parseLibComp(data)
    if err != nil {
        fmt.Println(err)
        return
    }
    order, cyclic := topSortKahn(g, in)
    if cyclic != nil {
        fmt.Println("Cyclic:", cyclic)
        return
    }
    fmt.Println("Order:", order)
}

type graph map[string][]string
type inDegree map[string]int

// parseLibComp parses the text format of the task and returns a graph
// representation and a list of the in-degrees of each node.  The returned graph
// represents compile order rather than dependency order.  That is, for each map
// map key n, the map elements are libraries that depend on n being compiled
// first.
func parseLibComp(data string) (g graph, in inDegree, err error) {
    // small sanity check on input
    lines := strings.Split(data, "\n")
    if len(lines) < 3 || !strings.HasPrefix(lines[2], "=") {
        return nil, nil, fmt.Errorf("data format")
    }
    // toss header lines
    lines = lines[3:]
    // scan and interpret input, build graph
    g = graph{}
    in = inDegree{}
    for _, line := range lines {
        libs := strings.Fields(line)
        if len(libs) == 0 {
            continue // allow blank lines
        }
        lib := libs[0]
        g[lib] = g[lib]
        for _, dep := range libs[1:] {
            in[dep] = in[dep]
            if dep == lib {
                continue // ignore self dependencies
            }
            successors := g[dep]
            for i := 0; ; i++ {
                if i == len(successors) {
                    g[dep] = append(successors, lib)
                    in[lib]++
                    break
                }
                if dep == successors[i] {
                    break // ignore duplicate dependencies
                }
            }
        }
    }
    return g, in, nil
}

// General purpose topological sort, not specific to the application of
// library dependencies.  Adapted from Wikipedia pseudo code, one main
// difference here is that this function does not consume the input graph.
// WP refers to incoming edges, but does not really need them fully represented.
// A count of incoming edges, or the in-degree of each node is enough.  Also,
// WP stops at cycle detection and doesn't output information about the cycle.
// A little extra code at the end of this function recovers the cyclic nodes.
func topSortKahn(g graph, in inDegree) (order, cyclic []string) {
    var L, S []string
    // rem for "remaining edges," this function makes a local copy of the
    // in-degrees and consumes that instead of consuming an input.
    rem := inDegree{}
    for n, d := range in {
        if d == 0 {
            // accumulate "set of all nodes with no incoming edges"
            S = append(S, n)
        } else {
            // initialize rem from in-degree
            rem[n] = d
        }
    }
    for len(S) > 0 {
        last := len(S) - 1 // "remove a node n from S"
        n := S[last]
        S = S[:last]
        L = append(L, n) // "add n to tail of L"
        for _, m := range g[n] {
            // WP pseudo code reads "for each node m..." but it means for each
            // node m *remaining in the graph.*  We consume rem rather than
            // the graph, so "remaining in the graph" for us means rem[m] > 0.
            if rem[m] > 0 {
                rem[m]--         // "remove edge from the graph"
                if rem[m] == 0 { // if "m has no other incoming edges"
                    S = append(S, m) // "insert m into S"
                }
            }
        }
    }
    // "If graph has edges," for us means a value in rem is > 0.
    for c, in := range rem {
        if in > 0 {
            // recover cyclic nodes
            for _, nb := range g[c] {
                if rem[nb] > 0 {
                    cyclic = append(cyclic, c)
                    break
                }
            }
        }
    }
    if len(cyclic) > 0 {
        return nil, cyclic
    }
    return L, nil
}
```

{{out}}

```txt

Order: [std ieee std_cell_lib ramlib gtech dware dw07 dw06 dw05 dw02 dw01 dw04 synopsys dw03 des_system_lib]

```

Cycle detection demonstrated with the example in the task description:

```txt

Cyclic: [dw01 dw04]

```


### Depth First

Topological sort only, this function can replace topSortKahn in above program.  The
in-degree list is not needed.

```go
// General purpose topological sort, not specific to the application of
// library dependencies.  Also adapted from Wikipedia pseudo code.
func topSortDFS(g graph) (order, cyclic []string) {
    L := make([]string, len(g))
    i := len(L)
    temp := map[string]bool{}
    perm := map[string]bool{}
    var cycleFound bool
    var cycleStart string
    var visit func(string)
    visit = func(n string) {
        switch {
        case temp[n]:
            cycleFound = true
            cycleStart = n
            return
        case perm[n]:
            return
        }
        temp[n] = true
        for _, m := range g[n] {
            visit(m)
            if cycleFound {
                if cycleStart > "" {
                    cyclic = append(cyclic, n)
                    if n == cycleStart {
                        cycleStart = ""
                    }
                }
                return
            }
        }
        delete(temp, n)
        perm[n] = true
        i--
        L[i] = n
    }
    for n := range g {
        if perm[n] {
            continue
        }
        visit(n)
        if cycleFound {
            return nil, cyclic
        }
    }
    return L, nil
}
```

{{out}}
(when used in program of Kahn example.)

```txt

Order: [ieee gtech synopsys dware dw07 dw06 dw02 dw01 dw04 std_cell_lib dw05 std ramlib dw03 des_system_lib]

```

And with the cycle added,

```txt

Cyclic: [dw04 dw01]

```



## Haskell


```haskell
import Data.List ((\\), elemIndex, intersect, nub)
import Control.Arrow ((***), first)

combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs) = ((x :) <$> combs (k - 1) xs) ++ combs k xs

depLibs :: [(String, String)]
depLibs =
  [ ( "des_system_lib"
    , "std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee")
  , ("dw01", "ieee dw01 dware gtech")
  , ("dw02", "ieee dw02 dware")
  , ("dw03", "std synopsys dware dw03 dw02 dw01 ieee gtech")
  , ("dw04", "dw04 ieee dw01 dware gtech")
  , ("dw05", "dw05 ieee dware")
  , ("dw06", "dw06 ieee dware")
  , ("dw07", "ieee dware")
  , ("dware", "ieee dware")
  , ("gtech", "ieee gtech")
  , ("ramlib", "std ieee")
  , ("std_cell_lib", "ieee std_cell_lib")
  , ("synopsys", [])
  ]

toposort :: [(String, String)] -> [String]
toposort xs
  | (not . null) cycleDetect =
    error $ "Dependency cycle detected for libs " ++ show cycleDetect
  | otherwise = foldl makePrecede [] dB
  where
    dB = ((\(x, y) -> (x, y \\ x)) . (return *** words)) <$> xs
    makePrecede ts ([x], xs) =
      nub $
      case elemIndex x ts of
        Just i -> uncurry (++) $ first (++ xs) $ splitAt i ts
        _ -> ts ++ xs ++ [x]
    cycleDetect =
      filter ((> 1) . length) $
      (\[(a, as), (b, bs)] -> (a `intersect` bs) ++ (b `intersect` as)) <$>
      combs 2 dB

main :: IO ()
main = print $ toposort depLibs
```

{{out}}

```haskell
*Main> toposort depLibs
["std","synopsys","ieee","std_cell_lib","dware","dw02","gtech","dw01","ramlib","des_system_lib","dw03","dw04","dw05","dw06","dw07"]

*Main> toposort $ (\(xs,(k,ks):ys) -> xs++ (k,ks++" dw04"):ys) $ splitAt 1  depLibs
*** Exception: Dependency cycle detected for libs [["dw01","dw04"]]
```



## Huginn


```huginn
import Algorithms as algo;
import Text as text;

class DirectedGraph {
	_adjecentVertices = {};
	add_vertex( vertex_ ) {
		_adjecentVertices[vertex_] = [];
	}
	add_edge( from_, to_ ) {
		_adjecentVertices[from_].push( to_ );
	}
	adjecent_vertices( vertex_ ) {
		return ( vertex_ ∈ _adjecentVertices ? _adjecentVertices.get( vertex_ ) : [] );
	}
}

class DepthFirstSearch {
	_visited = set();
	_postOrder = [];
	_cycleDetector = set();
	run( graph_, start_ ) {
		_cycleDetector.insert( start_ );
		_visited.insert( start_ );
		for ( vertex : graph_.adjecent_vertices( start_ ) ) {
			if ( vertex == start_ ) {
				continue;
			}
			if ( vertex ∈ _cycleDetector ) {
				throw Exception( "A cycle involving vertices {} found!".format( _cycleDetector ) );
			}
			if ( vertex ∉ _visited ) {
				run( graph_, vertex );
			}
		}
		_postOrder.push( start_ );
		_cycleDetector.erase( start_ );
	}
	topological_sort( graph_ ) {
		for ( vertex : graph_._adjecentVertices ) {
			if ( vertex ∉ _visited ) {
				run( graph_, vertex );
			}
		}
		return ( _postOrder );
	}
}

main() {
	rawdata =
		"des_system_lib  | std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee\n"
		"dw01            | ieee dw01 dware gtech\n"
		"dw02            | ieee dw02 dware\n"
		"dw03            | std synopsys dware dw03 dw02 dw01 ieee gtech\n"
		"dw04            | dw04 ieee dw01 dware gtech\n"
		"dw05            | dw05 ieee dware\n"
		"dw06            | dw06 ieee dware\n"
		"dw07            | ieee dware\n"
		"dware           | ieee dware\n"
		"gtech           | ieee gtech\n"
		"ramlib          | std ieee\n"
		"std_cell_lib    | ieee std_cell_lib\n"
		"synopsys        |\n";
	dg = DirectedGraph();
	for ( l : algo.filter( text.split( rawdata, "\n" ), @( x ) { size( x ) > 0; } ) ) {
		def = algo.materialize( algo.map( text.split( l, "|" ), string.strip ), list );
		dg.add_vertex( def[0] );
		for ( n : algo.filter( algo.map( text.split( def[1], " " ), string.strip ), @( x ) { size( x ) > 0; } ) ) {
			dg.add_edge( def[0], n );
		}
	}
	dfs = DepthFirstSearch();
	print( "{}\n".format( dfs.topological_sort( dg ) ) );
}
```


==Icon and Unicon==

=
## Icon
=

This solution uses an efficient internal representation for a graph that
limits the number of nodes to no more than 256.

The resulting topological ordering is displayed so elements on each line are
independent and so can be built in parallel once the preceding lines of
elements have been built.


```icon
record graph(nodes,arcs)
global ex_name, in_name

procedure main()
   show(tsort(getgraph()))
end

procedure tsort(g)
   t := ""
   while (n := g.nodes -- pnodes(g)) ~== "" do {
      t ||:= "("||n||")"
      g := delete(g,n)
      }
   if g.nodes == '' then return t
   write("graph contains the cycle:")
   write("\t",genpath(fn := !g.nodes,fn,g))
end

## pnodes(g) -- return the predecessor nodes of g
#     (those that have an arc from them)
procedure pnodes(g)
   static labels, fromnodes
   initial {
           labels := &ucase
           fromnodes := 'ACEGIKMOQSUWY'
           }
   return cset(select(g.arcs,labels, fromnodes))
end

## select(s,image,object) - efficient node selection
procedure select(s,image,object)
   slen := *s
   ilen := *image
   return if slen <= ilen then map(object[1+:slen/2],image[1+:slen],s)
          else map(object,image,s[1+:ilen]) || select(s[1+ilen:0],image,object)
end

## delete(g,x) -- deletes all nodes in x from graph g
#     note that arcs must be deleted as well
procedure delete(g,x)
   t := ""
   g.arcs ? while arc := move(2) do if not upto(x,arc) then t ||:= arc
   return graph(g.nodes--x,t)
end


## getgraph() -- read and construct a graph
#      graph is described via sets of arcs, as in:
#
#           from to1 to2 to3
#
# external names are converted to single character names for efficiency
# self-referential arcs are ignored
procedure getgraph()
   static labels
   initial labels := &cset
   ex_name := table()
   in_name := table()
   count := 0
   arcstr := ""
   nodes := ''
   every line := !&input do {
       nextWord := create genWords(line)
       if nfrom := @nextWord then {
           /in_name[nfrom] := &cset[count +:= 1]
           /ex_name[in_name[nfrom]] := nfrom
           nodes ++:= in_name[nfrom]
           while nto := @nextWord do {
               if nfrom ~== nto then {
                   /in_name[nto] := &cset[count +:= 1]
                   /ex_name[in_name[nto]] := nto
                   nodes ++:= in_name[nto]
                   arcstr ||:= in_name[nfrom] || in_name[nto]
                   }
               }
           }
       }
   return graph(nodes,arcstr)
end

# generate all 'words' in string
procedure genWords(s)
    static wchars
    initial wchars := &cset -- ' \t'
    s ?  while tab(upto(wchars))\1 do suspend tab(many(wchars))\1
end

## show(t) - return the external names (in order) for the nodes in t
#  Each output line contains names that are independent of each other
procedure show(t)
   line := ""
   every n := !t do
      case n of {
         "(" : line ||:= "\n\t("
         ")" : line[-1] := ")"
         default : line ||:= ex_name[n] || " "
      }
   write(line)
end

## genpath(f,t,g) -- generate paths from f to t in g
procedure genpath(f,t,g, seen)
   /seen := ''
   seen ++:= f
   sn := nnodes(f,g)
   if t ** sn == t then return ex_name[f] || " -> " || ex_name[t]
   suspend ex_name[f] || " -> " || genpath(!(sn --seen),t,g,seen)
end

## nnodes(f,g) -- compute all nodes that could follow f in g
procedure nnodes(f,g)
   t := ''
   g.arcs ? while arc := move(2) do if arc[1] == f then t ++:= arc[2]
   return t
end
```


{{out}}

```txt
->tsort <tsort.data

	(std synopsys ieee)
	(std_cell_lib ramlib dware gtech)
	(dw02 dw01 dw05 dw06 dw07)
	(des_system_lib dw03 dw04)
->

```


When run with the cycle suggested in the problem statement:


```txt
->tsort <tsort.data
graph contains the cycle:
        dw01 -> dw04 -> dw01
->

```


=
## Unicon
=

The Icon solution also works in Unicon, but the following variant removes
the 256-node limit by using sets instead of csets with the same algorithm
that produces output so each line gives the elements that can be built
in parallel once the elements in the preceding lines have been built.


```icon
record graph(nodes,arcs)

procedure main()
   show(tsort(getgraph()))
end

procedure tsort(g)
   t := []
   while *(n := g.nodes -- pnodes(g)) > 0 do {
      every put(p := [], !n)
      put(t, p)
      g := delete(g,n)
      }
   if *g.nodes = 0 then return t
   write("graph contains the cycle:")
   write("\t",genpath(fn := !g.nodes,fn,g))
end

procedure pnodes(g)
   cp := create !g.arcs
   every insert(p := set(), |1(@cp,@cp))
   return p
end

procedure delete(g,x)
   arcs := []
   cp := create !g.arcs
   while (f := @cp, t := @cp) do {
        if !x == (f|t) then next
        every put(arcs,f|t)
        }
   return graph(g.nodes--x, arcs)
end

procedure getgraph()
   arcs := []
   nodes := set()
   every line := !&input do {
       nextWord := create genWords(line)
       if nfrom := @nextWord then {
           insert(nodes, nfrom)
           while nto := @nextWord do {
               if nfrom ~== nto then {
                   insert(nodes, nto)
                   every put(arcs, nfrom | nto)
                   }
               }
           }
       }
   return graph(nodes,arcs)
end

procedure genWords(s)
    static wchars
    initial wchars := &cset -- ' \t'
    s ?  while tab(upto(wchars))\1 do suspend tab(many(wchars))\1
end

procedure show(t)
   line := ""
   every n := !t do
      case type(n) of {
         "list" : line ||:= "\n\t("||toString(n)||")"
         default : line ||:= " "||n
         }
   write(line)
end

procedure toString(n)
   every (s := "") ||:= !n || " "
   return s[1:-1] | s
end

procedure genpath(f,t,g, seen)
   /seen := set()
   insert(seen, f)
   sn := nnodes(f,g)
   if member(sn, t) then return f || " -> " || t
   suspend f || " -> " || genpath(!(sn--seen),t,g,seen)
end

procedure nnodes(f,g)
   t := set()
   cp := create !g.arcs
   while (af := @cp, at := @cp) do if af == f then insert(t, at)
   return t
end
```



## J



```J
dependencySort=: monad define
  parsed=. <@;:;._2 y
  names=. {.&>parsed
  depends=. (> =@i.@#) names e.S:1 parsed
  depends=. (+. +./ .*.~)^:_ depends
  assert.-.1 e. (<0 1)|:depends
  (-.&names ~.;parsed),names /: +/"1 depends
)
```


With the sample data set:

 dependencies=: noun define
   des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
   dw01             ieee dw01 dware gtech
   dw02             ieee dw02 dware
   dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
   dw04             dw04 ieee dw01 dware gtech
   dw05             dw05 ieee dware
   dw06             dw06 ieee dware
   dw07             ieee dware
   dware            ieee dware
   gtech            ieee gtech
   ramlib           std ieee
   std_cell_lib     ieee std_cell_lib
   synopsys
 )

We would get:


```J>
dependencySort dependencies
 std
 ieee
 dware
 gtech
 ramlib
 std_cell_lib
 synopsys
 dw02
 dw05
 dw06
 dw07
 dw01
 dw04
 dw03
 des_system_lib
```


If we tried to also make dw01 depend on dw04, the sort would fail because of the circular dependency:


```J
   dependencySort dependencies,'dw01 dw04',LF
|assertion failure: dependencySort
|   -.1 e.(<0 1)|:depends
```


Here is an alternate implementation which uses a slightly different representation for the dependencies (instead of a boolean connection matrix to represent connections, we use a list of lists of indices to represent connections):


```J
depSort=: monad define
  parsed=. <@;:;._2 y
  names=. {.&>parsed
  depends=. (-.L:0"_1 #,.i.@#) names i.L:1 parsed
  depends=. (~.@,&.> ;@:{L:0 1~)^:_ depends
  assert.-.1 e. (i.@# e.S:0"0 ])depends
  (-.&names ~.;parsed),names /: #@> depends
)
```


It's results are identical to the first implementation, but this might be more efficient in typical cases.


## Java

{{works with|Java|7}}

```java
import java.util.*;

public class TopologicalSort {

    public static void main(String[] args) {
        String s = "std, ieee, des_system_lib, dw01, dw02, dw03, dw04, dw05,"
                + "dw06, dw07, dware, gtech, ramlib, std_cell_lib, synopsys";

        Graph g = new Graph(s, new int[][]{
            {2, 0}, {2, 14}, {2, 13}, {2, 4}, {2, 3}, {2, 12}, {2, 1},
            {3, 1}, {3, 10}, {3, 11},
            {4, 1}, {4, 10},
            {5, 0}, {5, 14}, {5, 10}, {5, 4}, {5, 3}, {5, 1}, {5, 11},
            {6, 1}, {6, 3}, {6, 10}, {6, 11},
            {7, 1}, {7, 10},
            {8, 1}, {8, 10},
            {9, 1}, {9, 10},
            {10, 1},
            {11, 1},
            {12, 0}, {12, 1},
            {13, 1}
        });

        System.out.println("Topologically sorted order: ");
        System.out.println(g.topoSort());
    }
}

class Graph {
    String[] vertices;
    boolean[][] adjacency;
    int numVertices;

    public Graph(String s, int[][] edges) {
        vertices = s.split(",");
        numVertices = vertices.length;
        adjacency = new boolean[numVertices][numVertices];

        for (int[] edge : edges)
            adjacency[edge[0]][edge[1]] = true;
    }

    List<String> topoSort() {
        List<String> result = new ArrayList<>();
        List<Integer> todo = new LinkedList<>();

        for (int i = 0; i < numVertices; i++)
            todo.add(i);

        try {
            outer:
            while (!todo.isEmpty()) {
                for (Integer r : todo) {
                    if (!hasDependency(r, todo)) {
                        todo.remove(r);
                        result.add(vertices[r]);
                         // no need to worry about concurrent modification
                        continue outer;
                    }
                }
                throw new Exception("Graph has cycles");
            }
        } catch (Exception e) {
            System.out.println(e);
            return null;
        }
        return result;
    }

    boolean hasDependency(Integer r, List<Integer> todo) {
        for (Integer c : todo) {
            if (adjacency[r][c])
                return true;
        }
        return false;
    }
}
```



```txt
[std,  ieee,  dware,  dw02,  dw05, dw06,  dw07,  gtech,  dw01,  dw04,  ramlib,  std_cell_lib,  synopsys,  des_system_lib,  dw03]
```



## JavaScript



### =ES6=



```JavaScript
const libs =
  `des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
  dw01             ieee dw01 dware gtech
  dw02             ieee dw02 dware
  dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
  dw04             dw04 ieee dw01 dware gtech
  dw05             dw05 ieee dware
  dw06             dw06 ieee dware
  dw07             ieee dware
  dware            ieee dware
  gtech            ieee gtech
  ramlib           std ieee
  std_cell_lib     ieee std_cell_lib
  synopsys`;

// A map of the input data, with the keys as the packages, and the values as
// and array of packages on which it depends.
const D = libs
  .split('\n')
  .map(e => e.split(' ').filter(e => e != ''))
  .reduce((p, c) =>
    p.set(c[0], c.filter((e, i) => i > 0 && e !== c[0] ? e : null)), new Map());
[].concat(...D.values()).forEach(e => {
  D.set(e, D.get(e) || [])
});

// The above map rotated so that it represents a DAG of the form
// Map {
//    A => [ A, B, C],
//    B => [C],
//    C => []
// }
// where each key represents a node, and the array contains the edges.
const G = [...D.keys()].reduce((p, c) =>
  p.set(
    c,
    [...D.keys()].filter(e => D.get(e).includes(c))),
  new Map()
);

// An array of leaf nodes; nodes with 0 in degrees.
const Q = [...D.keys()].filter(e => D.get(e).length == 0);

// The result array.
const S = [];
while (Q.length) {
  const u = Q.pop();
  S.push(u);
  G.get(u).forEach(v => {
    D.set(v, D.get(v).filter(e => e !== u));
    if (D.get(v).length == 0) {
      Q.push(v);
    }
  });
}

console.log('Solution:', S);

```


Output:

```JavaScript

Solution: [
  'ieee',
  'std_cell_lib',
  'gtech',
  'dware',
  'dw07',
  'dw06',
  'dw05',
  'dw02',
  'dw01',
  'dw04',
  'std',
  'ramlib',
  'synopsys',
  'dw03',
  'des_system_lib' ]

```







## jq

In the following, the graph of dependencies is represented as a JSON
object with keys being the dependent entities, and each
corresponding value being a list of its dependencies.  For example:
{"x": ["y", "z"] } means: x depends on y and z.

The tsort filter will accept a dependency graph with self-dependencies.

'''Implementation Notes''':
Notice that the main function, tsort, has an inner function which itself has an inner function.

The normalize filter eliminates self-dependencies from a dependency graph.

'''Efficiency''':
The implementation of tsort uses a tail-recursive helper function, _tsort/0, which incurs no overhead due to recursion as jq optimizes arity-0 tail-recursive functions.

Since the dependency graph is represented as a jq object, which acts like a hash, access to the dependencies of a particular dependent is fast.

To solve and print the solution to the given problem on a 1GHz machine takes about 5ms.

```jq
# independent/0 emits an array of the dependencies that have no dependencies
# Input: an object representing a normalized dependency graph
def independent:
  . as $G
  | reduce keys[] as $key
      ([];
       . +  ((reduce $G[$key][] as $node
               ([];
                if ($G[$node] == null or ($G[$node]|length)==0) then . + [$node]
                else .
                end ))))
  | unique;

# normalize/0 eliminates self-dependencies in the input dependency graph.
# Input: an object representing a dependency graph.
def normalize:
  . as $G
  | reduce keys[] as $key
      ($G;
       .[$key] as $nodes
       | if $nodes and ($nodes|index($key)) then .[$key] = $nodes - [$key] else . end);


# minus/1 removes all the items in ary from each of the values in the input object
# Input: an object representing a dependency graph
def minus(ary):
  . as $G | reduce keys[] as $key ($G; $G[$key] -= ary);

# tsort/0 emits the topologically sorted nodes of the input,
# in ">" order.
# Input is assumed to be an object representing a dependency
# graph and need not be normalized.
def tsort:
  # _sort: input: [L, Graph], where L is the tsort so far
  def _tsort:

    def done: [.[]] | all( length==0 );

    .[0] as $L | .[1] as $G
    | if ($G|done) then $L + (($G|keys) - $L)
      else
         ($G|independent) as $I
         | if (($I|length) == 0)
           then error("the dependency graph is cyclic: \($G)")
           else [ ($L + $I), ($G|minus($I))] | _tsort
           end
      end;

  normalize | [[], .] | _tsort ;

tsort
```

Data:

```json
{"des_system_lib": [ "std", "synopsys", "std_cell_lib", "des_system_lib", "dw02", "dw01", "ramlib", "ieee"],
 "dw01": [ "ieee", "dw01", "dware", "gtech"],
 "dw02": [ "ieee", "dw02", "dware"],
 "dw03": [ "std", "synopsys", "dware", "dw03", "dw02", "dw01", "ieee", "gtech"],
 "dw04": [ "dw04", "ieee", "dw01", "dware", "gtech"],
 "dw05": [ "dw05", "ieee", "dware"],
 "dw06": [ "dw06", "ieee", "dware"],
 "dw07": [ "ieee", "dware"],
 "dware": [ "ieee", "dware"],
 "gtech": [ "ieee", "gtech"],
 "ramlib": [ "std", "ieee"],
 "std_cell_lib": [ "ieee", "std_cell_lib"],
 "synopsys": []
}

```

{{Out}}

```jq

$ jq -c -f tsort.jq tsort.json
["ieee","std","synopsys","dware","gtech","ramlib","std_cell_lib","dw01","dw02","des_system_lib","dw03","dw04","dw05","dw06","dw07"]

```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
function toposort(data::Dict{T,Set{T}}) where T
    data = copy(data)
    for (k, v) in data
        delete!(v, k)
    end
    extraitems = setdiff(reduce(∪, values(data)), keys(data))
    for item in extraitems
        data[item] = Set{T}()
    end
    rst = Vector{T}()
    while true
        ordered = Set(item for (item, dep) in data if isempty(dep))
        if isempty(ordered) break end
        append!(rst, ordered)
        data = Dict{T,Set{T}}(item => setdiff(dep, ordered) for (item, dep) in data if item ∉ ordered)
    end
    @assert isempty(data) "a cyclic dependency exists amongst $(keys(data))"
    return rst
end

data = Dict{String,Set{String}}(
    "des_system_lib" => Set(split("std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee")),
    "dw01" =>           Set(split("ieee dw01 dware gtech")),
    "dw02" =>           Set(split("ieee dw02 dware")),
    "dw03" =>           Set(split("std synopsys dware dw03 dw02 dw01 ieee gtech")),
    "dw04" =>           Set(split("dw04 ieee dw01 dware gtech")),
    "dw05" =>           Set(split("dw05 ieee dware")),
    "dw06" =>           Set(split("dw06 ieee dware")),
    "dw07" =>           Set(split("ieee dware")),
    "dware" =>          Set(split("ieee dware")),
    "gtech" =>          Set(split("ieee gtech")),
    "ramlib" =>         Set(split("std ieee")),
    "std_cell_lib" =>   Set(split("ieee std_cell_lib")),
    "synopsys" =>       Set(),
    )

println("# Topologically sorted:\n - ", join(toposort(data), "\n - "))
```


{{out}}

```txt
# Topologically sorted:
 - synopsys
 - ieee
 - std
 - ramlib
 - dware
 - gtech
 - std_cell_lib
 - dw07
 - dw05
 - dw02
 - dw01
 - dw06
 - des_system_lib
 - dw03
 - dw04
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.51

val s = "std, ieee, des_system_lib, dw01, dw02, dw03, dw04, dw05, " +
        "dw06, dw07, dware, gtech, ramlib, std_cell_lib, synopsys"

val deps = mutableListOf(
     2 to 0, 2 to 14, 2 to 13, 2 to 4, 2 to 3, 2 to 12, 2 to 1,
     3 to 1, 3 to 10, 3 to 11,
     4 to 1, 4 to 10,
     5 to 0, 5 to 14, 5 to 10, 5 to 4, 5 to 3, 5 to 1, 5 to 11,
     6 to 1, 6 to 3, 6 to 10, 6 to 11,
     7 to 1, 7 to 10,
     8 to 1, 8 to 10,
     9 to 1, 9 to 10,
     10 to 1,
     11 to 1,
     12 to 0, 12 to 1,
     13 to 1
)

class Graph(s: String, edges: List<Pair<Int,Int>>) {

    val vertices = s.split(", ")
    val numVertices = vertices.size
    val adjacency = List(numVertices) { BooleanArray(numVertices) }

    init {
        for (edge in edges) adjacency[edge.first][edge.second] = true
    }

    fun hasDependency(r: Int, todo: List<Int>): Boolean {
        for (c in todo) if (adjacency[r][c]) return true
        return false
    }

    fun topoSort(): List<String>? {
        val result = mutableListOf<String>()
        val todo = MutableList<Int>(numVertices) { it }
        try {
            outer@ while(!todo.isEmpty()) {
                for ((i, r) in todo.withIndex()) {
                    if (!hasDependency(r, todo)) {
                        todo.removeAt(i)
                        result.add(vertices[r])
                        continue@outer
                     }
                }
                throw Exception("Graph has cycles")
            }
        }
        catch (e: Exception) {
            println(e)
            return null
        }
        return result
    }
}

fun main(args: Array<String>) {
    val g = Graph(s, deps)
    println("Topologically sorted order:")
    println(g.topoSort())
    println()
    // now insert 3 to 6 at index 10 of deps
    deps.add(10, 3 to 6)
    val g2 = Graph(s, deps)
    println("Following the addition of dw04 to the dependencies of dw01:")
    println(g2.topoSort())
}
```


{{out}}

```txt

Topologically sorted order:
[std, ieee, dware, dw02, dw05, dw06, dw07, gtech, dw01, dw04, ramlib, std_cell_lib, synopsys, des_system_lib, dw03]

Following the addition of dw04 to the dependencies of dw01:
java.lang.Exception: Graph has cycles
null

```


{{trans|Python}}

```txt

This version follows python implementation and returns List of Lists which is useful for parallel execution for example

```


```scala

val graph = mapOf(
    "des_system_lib" to "std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee".split(" ").toSet(),
    "dw01" to "ieee dw01 dware gtech".split(" ").toSet(),
    "dw02" to "ieee dw02 dware".split(" ").toSet(),
    "dw03" to "std synopsys dware dw03 dw02 dw01 ieee gtech".split(" ").toSet(),
    "dw04" to "dw04 ieee dw01 dware gtech".split(" ").toSet(),
    "dw05" to "dw05 ieee dware".split(" ").toSet(),
    "dw06" to "dw06 ieee dware".split(" ").toSet(),
    "dw07" to "ieee dware".split(" ").toSet(),
    "dware" to "ieee dware".split(" ").toSet(),
    "gtech" to "ieee gtech".split(" ").toSet(),
    "ramlib" to "std ieee".split(" ").toSet(),
    "std_cell_lib" to "ieee std_cell_lib".split(" ").toSet(),
    "synopsys" to setOf()
)

fun toposort( graph: Map<String,Set<String>> ): List<List<String>> {
    var data = graph.map { (k,v) -> k to v.toMutableSet() }.toMap().toMutableMap()

    // ignore self dependancies
    data = data.map { (k,v) -> v.remove(k); k to v }.toMap().toMutableMap()

    val extraItemsInDeps = data.values.reduce { a,b -> a.union( b ).toMutableSet() } - data.keys.toSet()

    data.putAll( extraItemsInDeps.map { it to mutableSetOf<String>() }.toMap() )

    val res = mutableListOf<List<String>>()
    mainloop@ while( true ) {
        innerloop@ while( true ) {
            val ordered = data.filter{ (_,v) -> v.isEmpty() }.map { (k,_) -> k }
            if( ordered.isEmpty() )
                break@innerloop

            res.add( ordered )
            data = data.filter { (k,_) -> !ordered.contains(k) }.map { (k,v) -> v.removeAll(ordered); k to v }.toMap().toMutableMap()
        }

        if( data.isNotEmpty() )
            throw Exception( "A cyclic dependency exists amongst: ${data.toList().joinToString { "," }}" )
        else
            break@mainloop
    }

    return res
}


fun main( args: Array<String> ) {
    val result = toposort( graph )
    println( "sorted dependencies:[\n${result.joinToString( ",\n")}\n]" )
}


```

{{out}}

```txt

sorted dependencies:[
[synopsys, std, ieee],
[dware, gtech, ramlib, std_cell_lib],
[dw01, dw02, dw05, dw06, dw07],
[des_system_lib, dw03, dw04]
]

```



## Mathematica

Work in Mathematica 8 or higher versions.

```mathematica
TopologicalSort[
    Graph[Flatten[# /. {l_, ld_} :>
        Map[# -> l &,
         DeleteCases[ld, l]]]]] /. {_TopologicalSort -> $Failed} &@
 {{"des_system_lib", {"std", "synopsys", "std_cell_lib",
    "des_system_lib", "dw02", "dw01", "ramlib", "ieee"}},
  {"dw01", {"ieee", "dw01", "dware", "gtech"}},
  {"dw02", {"ieee", "dw02", "dware"}},
  {"dw03", {"std", "synopsys", "dware", "dw03", "dw02", "dw01",
    "ieee", "gtech"}},
  {"dw04", {"dw04", "ieee", "dw01", "dware", "gtech"}},
  {"dw05", {"dw05", "ieee", "dware"}},
  {"dw06", {"dw06", "ieee", "dware"}},
  {"dw07", {"ieee", "dware"}},
  {"dware", {"ieee", "dware"}},
  {"gtech", {"ieee", "gtech"}},
  {"ramlib", {"std", "ieee"}},
  {"std_cell_lib", {"ieee", "std_cell_lib"}},
  {"synopsys", {}}}
```

{{Out}}

```txt
{"ieee", "std_cell_lib", "gtech", "dware", "dw07", "dw06", "dw05", \
"dw02", "dw01", "dw04", "std", "ramlib", "synopsys", "dw03", \
"des_system_lib"}
```

If the data is un-orderable, it will return $Failed.


## Mercury


```Mercury

:- module topological_sort.

:- interface.

:- import_module io.
:- pred main(io::di,io::uo) is det.


:- implementation.
:- import_module string, solutions, list, set, require.

:- pred min_element(set(T),pred(T,T),T).
:- mode min_element(in,pred(in,in) is semidet,out) is nondet.
min_element(_,_,_):-fail.
min_element(S,P,X):-
    member(X,S),
    filter((pred(Y::in) is semidet :- P(Y,X)),S,LowerThanX),
    is_empty(LowerThanX).



:- pred topological_sort(set(T),pred(T,T),list(T),list(T)).
:- mode topological_sort(in,(pred((ground >> ground), (ground >> ground)) is semidet),in,out) is nondet.
:- pred topological_sort(set(T),pred(T,T),list(T)).
:- mode topological_sort(in,(pred((ground >> ground), (ground >> ground)) is semidet),out) is nondet.

topological_sort(S,P,Ac,L) :-
    (
	is_empty(S) -> L is Ac
    ;   solutions(
	    pred(X::out) is nondet:-
		min_element(S,P,X)
	 ,  Solutions
	),
	(
	    is_empty(Solutions) -> error("No solution detected.\n")
	; delete_list(Solutions,S,Sprime),
	  append(Solutions,Ac,AcPrime),
	  topological_sort(Sprime,P,AcPrime,L)
	)
    ).

topological_sort(S,P,L) :- topological_sort(S,P,[],L).


:- pred distribute(list(T)::in,{T,list(T)}::out) is det.
distribute([],_):-error("Error in distribute").
distribute([H|T],Z) :- Z = {H,T}.

:- pred db_compare({string,list(string)}::in,{string,list(string)}::in) is semidet.
db_compare({X1,L1},{X2,_}) :- not(X1=X2),list.member(X2,L1).


main(!IO) :-
    Input  = [
"des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee",
"dw01             ieee dw01 dware gtech",
"dw02             ieee dw02 dware",
"dw03             std synopsys dware dw03 dw02 dw01 ieee gtech",
"dw04             dw04 ieee dw01 dware gtech",
"dw05             dw05 ieee dware",
"dw06             dw06 ieee dware",
"dw07             ieee dware",
"dware            ieee dware",
"gtech            ieee gtech",
"ramlib           std ieee",
"std_cell_lib     ieee std_cell_lib",
"synopsys"],
    Words=list.map(string.words,Input),
    list.map(distribute,Words,Db),
    solutions(pred(X::out) is nondet :- topological_sort(set.from_list(Db),db_compare,X),SortedWordLists),
    list.map(
	     pred({X,Y}::in,Z::out) is det:- X=Z,
	     list.det_head(SortedWordLists),
	     CompileOrder),
    print(CompileOrder,!IO).


```



## Object Pascal

Written for Free Pascal, but will probably work in Delphi if you change the required units.

```Object Pascal

program topologicalsortrosetta;

{*
Topological sorter to parse e.g. dependencies.
Written for FreePascal 2.4.x/2.5.1. Probably works in Delphi, but you'd have to
change some units.
*}
{$IFDEF FPC}
// FreePascal-specific setup
{$mode objfpc}
uses {$IFDEF UNIX}
  cwstring, {* widestring support for unix *} {$IFDEF UseCThreads}
  cthreads, {$ENDIF UseCThreads} {$ENDIF UNIX}
  Classes,
  SysUtils;
{$ENDIF}

type
  RNodeIndex = record
    NodeName: WideString; //Name of the node
    //Index: integer; //Index number used in DepGraph. For now, we can distill the index from the array index. If we want to use a TList or similar, we'd need an index property
    Order: integer;  //Order when sorted
  end;

  RDepGraph = record
    Node: integer;  //Refers to Index in NodeIndex
    DependsOn: integer; //The Node depends on this other Node.
  end;

  { TTopologicalSort }

  TTopologicalSort = class(TObject)
  private
    Nodes: array of RNodeIndex;
    DependencyGraph: array of RDepGraph;
    FCanBeSorted: boolean;
    function SearchNode(NodeName: WideString): integer;
    function SearchIndex(NodeID: integer): WideString;
    function DepFromNodeID(NodeID: integer): integer;
    function DepFromDepID(DepID: integer): integer;
    function DepFromNodeIDDepID(NodeID, DepID: integer): integer;
    procedure DelDependency(const Index: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SortOrder(var Output: TStringList);
    procedure AddNode(NodeName: WideString);
    procedure AddDependency(NodeName, DependsOn: WideString);
    procedure AddNodeDependencies(NodeAndDependencies: TStringList);
    //Each string has node, and the nodes it depends on. This allows insertion of an entire dependency graph at once
    //procedure DelNode(NodeName: Widestring);
    procedure DelDependency(NodeName, DependsOn: WideString);

    property CanBeSorted: boolean read FCanBeSorted;

  end;

const
  INVALID = -1;
  // index not found for index search functions, no sort order defined, or record invalid/deleted

  function TTopologicalSort.SearchNode(NodeName: WideString): integer;
  var
    Counter: integer;
  begin
    // Return -1 if node not found. If node found, return index in array
    Result := INVALID;
    for Counter := 0 to High(Nodes) do
    begin
      if Nodes[Counter].NodeName = NodeName then
      begin
        Result := Counter;
        break;
      end;
    end;
  end;

  function TTopologicalSort.SearchIndex(NodeID: integer): WideString;
    //Look up name for the index
  begin
    if (NodeID > 0) and (NodeID <= High(Nodes)) then
    begin
      Result := Nodes[NodeID].NodeName;
    end
    else
    begin
      Result := 'ERROR'; //something's fishy, this shouldn't happen
    end;
  end;

  function TTopologicalSort.DepFromNodeID(NodeID: integer): integer;
    // Look for Node index number in the dependency graph
    // and return the first node found. If nothing found, return -1
  var
    Counter: integer;
  begin
    Result := INVALID;
    for Counter := 0 to High(DependencyGraph) do
    begin
      if DependencyGraph[Counter].Node = NodeID then
      begin
        Result := Counter;
        break;
      end;
    end;
  end;

  function TTopologicalSort.DepFromDepID(DepID: integer): integer;
    // Look for dependency index number in the dependency graph
    // and return the index for the first one found. If nothing found, return -1
  var
    Counter: integer;
  begin
    Result := INVALID;
    for Counter := 0 to High(DependencyGraph) do
    begin
      if DependencyGraph[Counter].DependsOn = DepID then
      begin
        Result := Counter;
        break;
      end;
    end;
  end;

  function TTopologicalSort.DepFromNodeIDDepID(NodeID, DepID: integer): integer;
    // Shows index for the dependency from NodeID on DepID, or INVALID if not found
  var
    Counter: integer;
  begin
    Result := INVALID;
    for Counter := 0 to High(DependencyGraph) do
    begin
      if DependencyGraph[Counter].Node = NodeID then
        if DependencyGraph[Counter].DependsOn = DepID then
        begin
          Result := Counter;
          break;
        end;
    end;
  end;

  procedure TTopologicalSort.DelDependency(const Index: integer);
  // Removes dependency from array.
  // Is fastest when the dependency is near the top of the array
  // as we're copying the remaining elements.
  var
    Counter: integer;
    OriginalLength: integer;
  begin
    OriginalLength := Length(DependencyGraph);
    if Index = OriginalLength - 1 then
    begin
      SetLength(DependencyGraph, OriginalLength - 1);
    end;
    if Index < OriginalLength - 1 then
    begin
      for Counter := Index to OriginalLength - 2 do
      begin
        DependencyGraph[Counter] := DependencyGraph[Counter + 1];
      end;
      SetLength(DependencyGraph, OriginalLength - 1);
    end;
    if Index > OriginalLength - 1 then
    begin
      // This could happen when deleting on an empty array:
      raise Exception.Create('Tried to delete index ' + IntToStr(Index) +
        ' while the maximum index was ' + IntToStr(OriginalLength - 1));
    end;
  end;

  constructor TTopologicalSort.Create;
  begin
    inherited Create;
  end;

  destructor TTopologicalSort.Destroy;
  begin
    // Clear up data just to make sure:
    Finalize(DependencyGraph);
    Finalize(Nodes);
    inherited;
  end;

  procedure TTopologicalSort.SortOrder(var Output: TStringList);
  var
    Counter: integer;
    NodeCounter: integer;
    OutputSortOrder: integer;
    DidSomething: boolean; //used to detect cycles (circular references)
    Node: integer;
  begin
    OutputSortOrder := 0;
    DidSomething := True; // prime the loop below
    FCanBeSorted := True; //hope for the best.
    while (DidSomething = True) do
    begin
      // 1. Find all nodes (now) without dependencies, output them first and remove the dependencies:
      // 1.1 Nodes that are not present in the dependency graph at all:
      for Counter := 0 to High(Nodes) do
      begin
        if DepFromNodeID(Counter) = INVALID then
        begin
          if DepFromDepID(Counter) = INVALID then
          begin
            // Node doesn't occur in either side of the dependency graph, so it has sort order 0:
            DidSomething := True;
            if (Nodes[Counter].Order = INVALID) or
              (Nodes[Counter].Order > OutputSortOrder) then
            begin
              // Enter sort order if the node doesn't have a lower valid order already.
              Nodes[Counter].Order := OutputSortOrder;
            end;
          end; //Invalid Dep
        end; //Invalid Node
      end; //Count
      // Done with the first batch, so we can increase the sort order:
      OutputSortOrder := OutputSortOrder + 1;
      // 1.2 Nodes that are only present on the right hand side of the dep graph:
      DidSomething := False;
      // reverse order so we can delete dependencies without passing upper array
      for Counter := High(DependencyGraph) downto 0 do
      begin
        Node := DependencyGraph[Counter].DependsOn; //the depended node
        if (DepFromNodeID(Node) = INVALID) then
        begin
          DidSomething := True;
          //Delete dependency so we don't hit it again:
          DelDependency(Counter);
          if (Nodes[Node].Order = INVALID) or (Nodes[Node].Order > OutputSortOrder) then
          begin
            // Enter sort order if the node doesn't have a lower valid order already.
            Nodes[Node].Order := OutputSortOrder;
          end;
        end;
        OutputSortOrder := OutputSortOrder + 1; //next iteration
      end;
      // 2. Go back to 1 until we can't do more work, and do some bookkeeping:
      OutputSortOrder := OutputSortOrder + 1;
    end; //outer loop for 1 to 2
    OutputSortOrder := OutputSortOrder - 1; //fix unused last loop.

    // 2. If we have dependencies left, we have a cycle; exit.
    if (High(DependencyGraph) > 0) then
    begin
      FCanBeSorted := False; //indicate we have a cycle
      Output.Add('Cycle (circular dependency) detected, cannot sort further. Dependencies left:');
      for Counter := 0 to High(DependencyGraph) do
      begin
        Output.Add(SearchIndex(DependencyGraph[Counter].Node) +
          ' depends on: ' + SearchIndex(DependencyGraph[Counter].DependsOn));
      end;
    end
    else
    begin
      // No cycle:
      // Now parse results, if we have them
      for Counter := 0 to OutputSortOrder do
      begin
        for NodeCounter := 0 to High(Nodes) do
        begin
          if Nodes[NodeCounter].Order = Counter then
          begin
            Output.Add(Nodes[NodeCounter].NodeName);
          end;
        end; //output each result
      end; //order iteration
    end; //cycle detection
  end;

  procedure TTopologicalSort.AddNode(NodeName: WideString);
  var
    NodesNewLength: integer;
  begin
    // Adds node; make sure we don't add duplicate entries
    if SearchNode(NodeName) = INVALID then
    begin
      NodesNewLength := Length(Nodes) + 1;
      SetLength(Nodes, NodesNewLength);
      Nodes[NodesNewLength - 1].NodeName := NodeName; //Arrays are 0 based
      //Nodes[NodesNewLength -1].Index :=  //If we change the object to a tlist or something, we already have an index property
      Nodes[NodesNewLength - 1].Order := INVALID; //default value
    end;
  end;

  procedure TTopologicalSort.AddDependency(NodeName, DependsOn: WideString);
  begin
    // Make sure both nodes in the dependency exist as a node
    if SearchNode(NodeName) = INVALID then
    begin
      Self.AddNode(NodeName);
    end;
    if SearchNode(DependsOn) = INVALID then
    begin
      Self.AddNode(DependsOn);
    end;
    // Add the dependency, only if we don't depend on ourselves:
    if NodeName <> DependsOn then
    begin
      SetLength(DependencyGraph, Length(DependencyGraph) + 1);
      DependencyGraph[High(DependencyGraph)].Node := SearchNode(NodeName);
      DependencyGraph[High(DependencyGraph)].DependsOn := SearchNode(DependsOn);
    end;
  end;

  procedure TTopologicalSort.AddNodeDependencies(NodeAndDependencies: TStringList);
  // Takes a stringlist containing a list of strings. Each string contains node names
  // separated by spaces. The first node depends on the others. It is permissible to have
  // only one node name, which doesn't depend on anything.
  // This procedure will add the dependencies and the nodes in one go.
  var
    Deplist: TStringList;
    StringCounter: integer;
    NodeCounter: integer;
  begin
    if Assigned(NodeAndDependencies) then
    begin
      DepList := TStringList.Create;
      try
        for StringCounter := 0 to NodeAndDependencies.Count - 1 do
        begin
          // For each string in the argument: split into names, and process:
          DepList.Delimiter := ' '; //use space to separate the entries
          DepList.StrictDelimiter := False; //allows us to ignore double spaces in input.
          DepList.DelimitedText := NodeAndDependencies[StringCounter];
          for NodeCounter := 0 to DepList.Count - 1 do
          begin
            if NodeCounter = 0 then
            begin
              // Add the first node, which might be the only one.
              Self.AddNode(Deplist[0]);
            end;

            if NodeCounter > 0 then
            begin
              // Only add dependency from the second item onwards
              // The AddDependency code will automatically add Deplist[0] to the Nodes, if required
              Self.AddDependency(DepList[0], DepList[NodeCounter]);
            end;
          end;
        end;
      finally
        DepList.Free;
      end;
    end;
  end;

  procedure TTopologicalSort.DelDependency(NodeName, DependsOn: WideString);
  // Delete the record.
  var
    NodeID: integer;
    DependsID: integer;
    Dependency: integer;
  begin
    NodeID := Self.SearchNode(NodeName);
    DependsID := Self.SearchNode(DependsOn);
    if (NodeID <> INVALID) and (DependsID <> INVALID) then
    begin
      // Look up dependency and delete it.
      Dependency := Self.DepFromNodeIDDepID(NodeID, DependsID);
      if (Dependency <> INVALID) then
      begin
        Self.DelDependency(Dependency);
      end;
    end;
  end;

  // Main program:
var
  InputList: TStringList; //Lines of dependencies
  TopSort: TTopologicalSort; //Topological sort object
  OutputList: TStringList; //Sorted dependencies
  Counter: integer;
begin

  //Actual sort
  InputList := TStringList.Create;
  // Add rosetta code sample input separated by at least one space in the lines
  InputList.Add(
    'des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee');
  InputList.Add('dw01             ieee dw01 dware gtech');
  InputList.Add('dw02             ieee dw02 dware');
  InputList.Add('dw03             std synopsys dware dw03 dw02 dw01 ieee gtech');
  InputList.Add('dw04             dw04 ieee dw01 dware gtech');
  InputList.Add('dw05             dw05 ieee dware');
  InputList.Add('dw06             dw06 ieee dware');
  InputList.Add('dw07             ieee dware');
  InputList.Add('dware            ieee dware');
  InputList.Add('gtech            ieee gtech');
  InputList.Add('ramlib           std ieee');
  InputList.Add('std_cell_lib     ieee std_cell_lib');
  InputList.Add('synopsys');
  TopSort := TTopologicalSort.Create;
  OutputList := TStringList.Create;
  try
    TopSort.AddNodeDependencies(InputList); //read in nodes
    TopSort.SortOrder(OutputList); //perform the sort
    for Counter := 0 to OutputList.Count - 1 do
    begin
      writeln(OutputList[Counter]);
    end;
  except
    on E: Exception do
    begin
      Writeln(stderr, 'Error: ', DateTimeToStr(Now),
        ': Error sorting. Technical details: ',
        E.ClassName, '/', E.Message);
    end;
  end; //try
  OutputList.Free;
  TopSort.Free;
  InputList.Free;
end.

```



## OCaml



```ocaml
let dep_libs = [
  ("des_system_lib", ["std"; "synopsys"; "std_cell_lib"; "des_system_lib"; "dw02"; "dw01"; "ramlib"; "ieee"]);
  ("dw01",           (*"dw04"::*)["ieee"; "dw01"; "dware"; "gtech"]);
  ("dw02",           ["ieee"; "dw02"; "dware"]);
  ("dw03",           ["std"; "synopsys"; "dware"; "dw03"; "dw02"; "dw01"; "ieee"; "gtech"]);
  ("dw04",           ["dw04"; "ieee"; "dw01"; "dware"; "gtech"]);
  ("dw05",           ["dw05"; "ieee"; "dware"]);
  ("dw06",           ["dw06"; "ieee"; "dware"]);
  ("dw07",           ["ieee"; "dware"]);
  ("dware",          ["ieee"; "dware"]);
  ("gtech",          ["ieee"; "gtech"]);
  ("ramlib",         ["std"; "ieee"]);
  ("std_cell_lib",   ["ieee"; "std_cell_lib"]);
  ("synopsys",       []);
]

let dep_libs =
  let f (lib, deps) =  (* remove self dependency *)
    (lib,
     List.filter (fun d -> d <> lib) deps) in
  List.map f dep_libs

let rev_unique =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) []

let libs =  (* list items, each being unique *)
  rev_unique (List.flatten(List.map (fun (lib, deps) -> lib::deps) dep_libs))

let get_deps lib =
  try (List.assoc lib dep_libs)
  with Not_found -> []

let res =
  let rec aux acc later todo progress =
  match todo, later with
  | [], [] -> (List.rev acc)
  | [], _ ->
      if progress
      then aux acc [] later false
      else invalid_arg "un-orderable data"
  | x::xs, _ ->
      let deps = get_deps x in
      let ok = List.for_all (fun dep -> List.mem dep acc) deps in
      if ok
      then aux (x::acc) later xs true
      else aux acc (x::later) xs progress
  in
  let starts, todo = List.partition (fun lib -> get_deps lib = []) libs in
  aux starts [] todo false

let () =
  print_string "result: \n ";
  print_endline (String.concat ", " res);
;;
```


If dw04 is added to the set of dependencies of dw01 to make the data un-orderable (uncomment it), an exception is raised:
 Exception: Invalid_argument "un-orderable data".


## Oz

Using constraint propagation and search:

```oz
declare
  Deps = unit(
            des_system_lib: [std synopsys std_cell_lib des_system_lib
                             dw02 dw01 ramlib ieee]
            dw01: [ieee dw01 dware gtech]
            dw02: [ieee dw02 dware]
            dw03: [std synopsys dware dw03 dw02 dw01 ieee gtech]
            dw04: [dw04 ieee dw01 dware gtech]
            dw05: [dw05 ieee dware]
            dw06: [dw06 ieee dware]
            dw07: [ieee dware]
            dware: [ieee dware]
            gtech: [ieee gtech]
            ramlib: [std ieee]
            std_cell_lib: [ieee std_cell_lib]
            synopsys:nil
            )

  %% Describe possible solutions
  proc {TopologicalOrder Solution}
     FullDeps = {Complete Deps}
  in
     %% The solution is a record that maps library names
     %% to finite domain variables.
     %% The smaller the value, the earlier it must be compiled
     Solution = {FD.record sol {Arity FullDeps} 1#{Width FullDeps}}
     %% for every lib on the left side
     {Record.forAllInd FullDeps
      proc {$ LibName Dependants}
         %% ... and every dependant on the right side
         for Dependant in Dependants do
            %% propagate compilation order
            if Dependant \= LibName then
               Solution.LibName >: Solution.Dependant
            end
         end
      end
     }
     %% enumerate solutions
     {FD.distribute naive Solution}
  end

  %% adds empty list of dependencies for libs that only occur on the right side
  fun {Complete Dep}
     AllLibs = {Nub {Record.foldL Dep Append nil}}
  in
     {Adjoin
      {List.toRecord unit {Map AllLibs fun {$ L} L#nil end}}
      Dep}
  end

  %% removes duplicates
  fun {Nub Xs}
     D = {Dictionary.new}
  in
     for X in Xs do D.X := unit end
     {Dictionary.keys D}
  end

  %% print grouped by parallelizable jobs
  proc {PrintSolution Sol}
     for I in 1..{Record.foldL Sol Value.max 1} do
        for Lib in {Arity {Record.filter Sol fun {$ X} X == I end}} do
           {System.printInfo Lib#" "}
        end
        {System.printInfo "\n"}
     end
  end

  fun {GetOrderedLibs Sol}
     {Map
      {Sort {Record.toListInd Sol} CompareSecond}
      SelectFirst}
  end
  fun {CompareSecond A B} A.2 < B.2 end
  fun {SelectFirst X} X.1 end
in
  case {SearchOne TopologicalOrder}
  of nil then {System.showInfo "Un-orderable."}
  [] [Sol] then
     {System.showInfo "A possible topological ordering: "}
     {ForAll {GetOrderedLibs Sol} System.showInfo}

     {System.showInfo "\nBONUS - grouped by parallelizable compile jobs:"}
     {PrintSolution Sol}
  end
```


Output:

```txt

A possible topological ordering:
synopsys
std
ieee
std_cell_lib
ramlib
gtech
dware
dw07
dw06
dw05
dw02
dw01
dw04
dw03
des_system_lib

BONUS - grouped by parallelizable compile jobs:
ieee std synopsys
dware gtech ramlib std_cell_lib
dw01 dw02 dw05 dw06 dw07
des_system_lib dw03 dw04

```



## Pascal

See [[Topological_sort#Object Pascal | Object Pascal]]


## Perl

In July 2002, [http://perlgolf.sourceforge.net/TPR/0/4b/ Topological Sort] was the monthly [http://perlgolf.sourceforge.net/ Perl Golf] course. The [http://perlgolf.sourceforge.net/cgi-bin/PGAS/post_mortem.cgi?id=6 post-mortem] contains many solutions. This code was adapted from the solution that scored 144.39.

The algorithm used allows the output to be clustered; libraries on the same line are all independent (given the building of any previous lines of libraries), and so could be built in parallel.


```perl
sub print_topo_sort {
    my %deps = @_;

    my %ba;
    while ( my ( $before, $afters_aref ) = each %deps ) {
        for my $after ( @{ $afters_aref } ) {
            $ba{$before}{$after} = 1 if $before ne $after;
            $ba{$after} ||= {};
        }
    }

    while ( my @afters = sort grep { ! %{ $ba{$_} } } keys %ba ) {
        print "@afters\n";
        delete @ba{@afters};
        delete @{$_}{@afters} for values %ba;
    }

    print !!%ba ? "Cycle found! ". join( ' ', sort keys %ba ). "\n" : "---\n";
}

my %deps = (
    des_system_lib => [qw( std synopsys std_cell_lib des_system_lib dw02
                                                        dw01 ramlib ieee )],
    dw01           => [qw( ieee dw01 dware gtech                         )],
    dw02           => [qw( ieee dw02 dware                               )],
    dw03           => [qw( std synopsys dware dw03 dw02 dw01 ieee gtech  )],
    dw04           => [qw( dw04 ieee dw01 dware gtech                    )],
    dw05           => [qw( dw05 ieee dware                               )],
    dw06           => [qw( dw06 ieee dware                               )],
    dw07           => [qw( ieee dware                                    )],
    dware          => [qw( ieee dware                                    )],
    gtech          => [qw( ieee gtech                                    )],
    ramlib         => [qw( std ieee                                      )],
    std_cell_lib   => [qw( ieee std_cell_lib                             )],
    synopsys       => [qw(                                               )],
);
print_topo_sort(%deps);
push @{ $deps{'dw01'} }, 'dw04'; # Add unresolvable dependency
print_topo_sort(%deps);
```


Output:
```txt
ieee std synopsys
dware gtech ramlib std_cell_lib
dw01 dw02 dw05 dw06 dw07
des_system_lib dw03 dw04
---
ieee std synopsys
dware gtech ramlib std_cell_lib
dw02 dw05 dw06 dw07
Cycle found! des_system_lib dw01 dw03 dw04
```



## Perl 6

{{trans|Perl}}
{{Works with|rakudo|2016.01}}

```perl6
sub print_topo_sort ( %deps ) {
    my %ba;
    for %deps.kv -> $before, @afters {
        for @afters -> $after {
            %ba{$before}{$after} = 1 if $before ne $after;
            %ba{$after} //= {};
        }
    }

    while %ba.grep( not *.value )».key -> @afters {
        say ~@afters.sort;
        %ba{@afters}:delete;
        for %ba.values { .{@afters}:delete }
    }

    say %ba ?? "Cycle found! {%ba.keys.sort}" !! '---';
}

my %deps =
    des_system_lib => < std synopsys std_cell_lib des_system_lib dw02
                                                     dw01 ramlib ieee >,
    dw01           => < ieee dw01 dware gtech                         >,
    dw02           => < ieee dw02 dware                               >,
    dw03           => < std synopsys dware dw03 dw02 dw01 ieee gtech  >,
    dw04           => < dw04 ieee dw01 dware gtech                    >,
    dw05           => < dw05 ieee dware                               >,
    dw06           => < dw06 ieee dware                               >,
    dw07           => < ieee dware                                    >,
    dware          => < ieee dware                                    >,
    gtech          => < ieee gtech                                    >,
    ramlib         => < std ieee                                      >,
    std_cell_lib   => < ieee std_cell_lib                             >,
    synopsys       => <                                               >;

print_topo_sort(%deps);
%deps<dw01> = <ieee dw01 dware gtech dw04>; # Add unresolvable dependency
print_topo_sort(%deps);
```


Output:
```txt
ieee std synopsys
dware gtech ramlib std_cell_lib
dw01 dw02 dw05 dw06 dw07
des_system_lib dw03 dw04
---
ieee std synopsys
dware gtech ramlib std_cell_lib
dw02 dw05 dw06 dw07
Cycle found! des_system_lib dw01 dw03 dw04
```

Some differences from the Perl 5 version include use of
formal parameters; use of <tt>»</tt> as a "hyper" operator, that is, a parallelizable implicit loop; and use of normal lambda-like notation to bind loop parameters, so we can have multiple loop parameters bound on each iteration.  Also,
since <tt>=></tt> is now a real pair composer rather than a synonym for comma, the data can be represented with real pair notation that points to quoted word lists delimited by angle brackets rather than <tt>[qw(...)]</tt>.


## Phix

Implemented as a trivial normal sort.

```Phix
sequence names
enum RANK, NAME, DEP    -- content of names
-- rank is 1 for items to compile first, then 2, etc,
--      or 0 if cyclic dependencies prevent compilation.
-- name is handy, and makes the result order alphabetic!
-- dep is a list of dependencies (indexes to other names)

function add_dependency(string name)
    integer k = find(name,vslice(names,NAME))
    if k=0 then
        names = append(names,{0,name,{}})
        k = length(names)
    end if
    return k
end function

procedure topsort(string input)
    names = {}
    sequence lines = split(input,'\n')
    for i=1 to length(lines) do
        sequence line = split(lines[i],no_empty:=true),
                 dependencies = {}
        integer k = add_dependency(line[1])
        for j=2 to length(line) do
            integer l = add_dependency(line[j])
            if l!=k then -- ignore self-references
                dependencies &= l
            end if
        end for
        names[k][DEP] = dependencies
    end for

    -- Now populate names[RANK] iteratively:
    bool more = true
    integer rank = 0
    while more do
        more = false
        rank += 1
        for i=1 to length(names) do
            if names[i][RANK]=0 then
                bool ok = true
                for j=1 to length(names[i][DEP]) do
                    integer ji = names[i][DEP][j],
                            nr = names[ji][RANK]
                    if nr=0 or nr=rank then
                        -- not yet compiled, or same pass
                        ok = false
                        exit
                    end if
                end for
                if ok then
                    names[i][RANK] = rank
                    more = true
                end if
            end if
        end for
    end while

    names = sort(names) -- (ie by [RANK=1] then [NAME=2])
    integer prank = names[1][RANK]
    if prank=0 then puts(1,"** CYCLIC **:") end if
    for i=1 to length(names) do
        rank = names[i][RANK]
        if i>1 then
            puts(1,iff(rank=prank?" ":"\n"))
        end if
        puts(1,names[i][NAME])
        prank = rank
    end for
    puts(1,"\n")
end procedure

constant input = """
des_system_lib  std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01            ieee dw01 dware gtech
dw02            ieee dw02 dware
dw03            std synopsys dware dw03 dw02 dw01 ieee gtech
dw04            dw04 ieee dw01 dware gtech
dw05            dw05 ieee dware
dw06            dw06 ieee dware
dw07            ieee dware
dware           ieee dware
gtech           ieee gtech
ramlib          std ieee
std_cell_lib    ieee std_cell_lib
synopsys"""

topsort(input)
puts(1,"\nbad input:\n")
topsort(input&"\ndw01 dw04")
```

{{out}}
Items on the same line can be compiled at the same time, and each line is alphabetic.

```txt

ieee std synopsys
dware gtech ramlib std_cell_lib
dw01 dw02 dw05 dw06 dw07
des_system_lib dw03 dw04

bad input:
** CYCLIC **:des_system_lib dw01 dw03 dw04
ieee std synopsys
dware gtech ramlib std_cell_lib
dw02 dw05 dw06 dw07

```



## PicoLisp


```PicoLisp
(de sortDependencies (Lst)
   (setq Lst                              # Build a flat list
      (uniq
         (mapcan
            '((L)
               (put (car L) 'dep (cdr L)) # Store dependencies in 'dep' properties
               (copy L) )
            (mapcar uniq Lst) ) ) )       # without self-dependencies
   (make
      (while Lst
         (ifn (find '((This) (not (: dep))) Lst)   # Found non-depending lib?
            (quit "Can't resolve dependencies" Lst)
            (del (link @) 'Lst)                    # Yes: Store in result
            (for This Lst                          # and remove from 'dep's
               (=: dep (delete @ (: dep))) ) ) ) ) )
```

Output:

```txt
: (sortDependencies
   (quote
      (des-system-lib   std synopsys std-cell-lib des-system-lib dw02 dw01 ramlib ieee)
      (dw01             ieee dw01 dware gtech)
      (dw02             ieee dw02 dware)
      (dw03             std synopsys dware dw03 dw02 dw01 ieee gtech)
      (dw04             dw04 ieee dw01 dware gtech)
      (dw05             dw05 ieee dware)
      (dw06             dw06 ieee dware)
      (dw07             ieee dware)
      (dware            ieee dware)
      (gtech            ieee gtech)
      (ramlib           std ieee)
      (std-cell-lib     ieee std-cell-lib)
      (synopsys) ) )
-> (std synopsys ieee std-cell-lib ramlib dware dw02 gtech dw01 des-system-lib dw03 dw04 dw05 dw06 dw07)
```



## PowerShell


```PowerShell
#Input Data
$a=@"
des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01             ieee dw01 dware gtech
dw02             ieee dw02 dware
dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
dw04             dw04 ieee dw01 dware gtech
dw05             dw05 ieee dware
dw06             dw06 ieee dware
dw07             ieee dware
dware            ieee dware
gtech            ieee gtech
ramlib           std ieee
std_cell_lib     ieee std_cell_lib
synopsys
"@
#Convert to Object[]
$c = switch ( $a.split([char] 10) ) {
    $_ {
        $b=$_.split(' ')
        New-Object PSObject -Property @{
            Library = $b[0]
            "Library Dependencies" = @( $( $b[1..($b.length-1)] | Where-Object { $_ -match '\w' } ) )
        }
    }
}
#Add pure dependencies
$c | ForEach-Object {
    $_."Library Dependencies" | Where-Object {
        $d=$_
        $(:andl foreach($i in $c) {
            if($d -match $i.Library) {
                $false
                break andl
            }
        }) -eq $null
    } | ForEach-Object {
        $c+=New-Object PSObject -Property @{
            Library=$_
            "Library Dependencies"=@()
        }
    }
}
#Associate with a dependency value
##Initial Dependency Value
$d = $c | Sort Library | Select-Object Library,"Library Dependencies",@{
    Name="Dep Value"
    Expression={
        1
    }
}
##Modify Dependency Value, perform check for incorrect dependency
##Dep Value is determined by a parent child relationship, if a library is a parent, all libraries dependant on it are children
for( $i=0; $i -lt $d.count; $i++ ) {
    $errmsg=""
    foreach( $j in ( 0..( $d.count - 1 ) | Where-Object { $_ -ne $i } ) ) {
        #Foreach other Child Library where this is a dependency, increase the Dep Value of the Child
        if( $( :orl foreach( $k in $d[$j]."Library Dependencies" ) {
            if( $k -match $d[$i].Library ) {
                foreach( $n in $d[$i]."Library Dependencies" ) {
                    if( $n -match $d[$j].Library ) {
                        $errmsg="Error Cyclic Dependency {0}<->{1}" -f $d[$i].Library, $d[$j].Library
                        break
                    }
                }
                $true
                break orl
            }
        } ) ) {
            #If the child has already been processed, increase the Dep Value of its children
            if( $j -lt $i ) {
                foreach( $l in ( 0..( $d.count - 1 ) | Where-Object { $_ -ne $j } ) ) {
                    if( $( :orl2 foreach( $m in $d[$l]."Library Dependencies" ) {
                        if( $m -match $d[$j].Library ) {
                            $true
                            break orl2
                        }
                    } ) ) {
                        $d[$l]."Dep Value"+=$d[$i]."Dep Value"
                    }
                }
            }
            $d[$j]."Dep Value"+=$d[$i]."Dep Value"
        }
        if( $errmsg -ne "" ) {
            $errmsg
            $d=$null
            break
        }
    }
}
#Sort and Display
if( $d ) {
    $d | Sort "Dep Value",Library | ForEach-Object {
        "{0,-14} LIBRARY DEPENDENCIES`n{1,-14}
### ==============
" -f "LIBRARY", "
### =
"
    } {
        "{0,-14} $($_."Library Dependencies")" -f $_.Library
    }
}
```



## PureBasic


```PureBasic
#EndOfDataMarker$ = "::EndOfData::"
DataSection
  ;"LIBRARY: [LIBRARY_DEPENDENCY_1 LIBRARY_DEPENDENCY_2 ... LIBRARY_DEPENDENCY_N]
  Data.s "des_system_lib: [std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee]"
  Data.s "dw01: [ieee dw01 dware gtech]"
  ;Data.s "dw01: [ieee dw01 dware gtech dw04]" ;comment the previous line and uncomment this one for cyclic dependency
  Data.s "dw02: [ieee dw02 dware]"
  Data.s "dw03: [std synopsys dware dw03 dw02 dw01 ieee gtech]"
  Data.s "dw04: [dw04 ieee dw01 dware gtech]"
  Data.s "dw05: [dw05 ieee dware]"
  Data.s "dw06: [dw06 ieee dware]"
  Data.s "dw07: [ieee dware]"
  Data.s "dware: [ieee dware]"
  Data.s "gtech: [ieee gtech]"
  Data.s "ramlib: [std ieee]"
  Data.s "std_cell_lib: [ieee std_cell_lib]"
  Data.s "synopsys: nil"
  Data.s #EndOfDataMarker$
EndDataSection

Structure DAG_node
  Value.s
  forRemoval.i ;flag marks elements that should be removed the next time they are accessed
  List dependencies.s()
EndStructure

If Not OpenConsole()
  MessageRequester("Error","Unable to open console")
  End
EndIf

;// initialize Directed Acyclic Graph //
Define i, itemData.s, firstBracketPos
NewList DAG.DAG_node()
Repeat
  Read.s itemData
  itemData = Trim(itemData)
  If itemData <> #EndOfDataMarker$
    AddElement(DAG())
    ;add library
    DAG()\Value = Trim(Left(itemData, FindString(itemData, ":", 1) - 1))
    ;parse library dependencies
    firstBracketPos = FindString(itemData, "[", 1)
    If firstBracketPos
      itemData = Trim(Mid(itemData, firstBracketPos + 1, FindString(itemData, "]", 1) - firstBracketPos - 1))
      For i = (CountString(itemData, " ") + 1) To 1 Step -1
        AddElement(DAG()\dependencies())
        DAG()\dependencies() = StringField(itemData, i, " ")
      Next
    EndIf
  EndIf
Until itemData = #EndOfDataMarker$

;// process DAG //
;create DAG entry for nodes listed in dependencies but without their own entry
NewMap libraries()
ForEach DAG()
  ForEach DAG()\dependencies()
    libraries(DAG()\dependencies()) = #True
    If DAG()\dependencies() = DAG()\Value
      DeleteElement(DAG()\dependencies()) ;remove self-dependencies
    EndIf
  Next
Next

ForEach DAG()
  If FindMapElement(libraries(),DAG()\Value)
    DeleteMapElement(libraries(),DAG()\Value)
  EndIf
Next

ResetList(DAG())
ForEach libraries()
  AddElement(DAG())
  DAG()\Value = MapKey(libraries())
Next
ClearMap(libraries())

;process DAG() repeatedly until no changes occur
NewList compileOrder.s()
Repeat
  noChangesMade = #True
  ForEach DAG()
    If DAG()\forRemoval
      DeleteElement(DAG())
    Else
      ;remove dependencies that have been placed in the compileOrder
      ForEach DAG()\dependencies()
        If FindMapElement(libraries(),DAG()\dependencies())
          DeleteElement(DAG()\dependencies())
        EndIf
      Next
      ;add DAG() entry to compileOrder if it has no more dependencies
      If ListSize(DAG()\dependencies()) = 0
        AddElement(compileOrder())
        compileOrder() = DAG()\Value
        libraries(DAG()\Value) = #True ;mark the library for removal as a dependency
        DAG()\forRemoval = #True
        noChangesMade = #False
      EndIf
    EndIf
  Next
Until noChangesMade

If ListSize(DAG())
  PrintN("Cyclic dependencies detected in:" + #CRLF$)
  ForEach DAG()
    PrintN(" " + DAG()\Value)
  Next
Else
  PrintN("Compile order:" + #CRLF$)
  ForEach compileOrder()
    PrintN(" " + compileOrder())
  Next
EndIf

Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
Input()
CloseConsole()
```

Sample output for no dependencies:

```txt
Compile order:

 ieee
 std
 dware
 gtech
 ramlib
 std_cell_lib
 synopsys
 dw01
 dw02
 dw03
 dw04
 dw05
 dw06
 dw07
 des_system_lib
```

Sample output when cyclic dependencies are present:

```txt
Cyclic dependencies detected in:

 des_system_lib
 dw01
 dw03
 dw04
```



## Python



```python
try:
    from functools import reduce
except:
    pass

data = {
    'des_system_lib':   set('std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee'.split()),
    'dw01':             set('ieee dw01 dware gtech'.split()),
    'dw02':             set('ieee dw02 dware'.split()),
    'dw03':             set('std synopsys dware dw03 dw02 dw01 ieee gtech'.split()),
    'dw04':             set('dw04 ieee dw01 dware gtech'.split()),
    'dw05':             set('dw05 ieee dware'.split()),
    'dw06':             set('dw06 ieee dware'.split()),
    'dw07':             set('ieee dware'.split()),
    'dware':            set('ieee dware'.split()),
    'gtech':            set('ieee gtech'.split()),
    'ramlib':           set('std ieee'.split()),
    'std_cell_lib':     set('ieee std_cell_lib'.split()),
    'synopsys':         set(),
    }

def toposort2(data):
    for k, v in data.items():
        v.discard(k) # Ignore self dependencies
    extra_items_in_deps = reduce(set.union, data.values()) - set(data.keys())
    data.update({item:set() for item in extra_items_in_deps})
    while True:
        ordered = set(item for item,dep in data.items() if not dep)
        if not ordered:
            break
        yield ' '.join(sorted(ordered))
        data = {item: (dep - ordered) for item,dep in data.items()
                if item not in ordered}
    assert not data, "A cyclic dependency exists amongst %r" % data

print ('\n'.join( toposort2(data) ))
```


'''Ordered output'''

items on a line could be processed in any sub-order or, indeed, in parallel:

```txt
ieee std synopsys
dware gtech ramlib std_cell_lib
dw01 dw02 dw05 dw06 dw07
des_system_lib dw03 dw04
```


If dw04 is added to the set of dependencies of dw01 to make the data un-orderable, an exception is raised:

```txt
Traceback (most recent call last):
  File "C:\Documents and Settings\All Users\Documents\Paddys\topological_sort.py", line 115, in <module>
    print ('\n'.join( toposort2(data) ))
  File "C:\Documents and Settings\All Users\Documents\Paddys\topological_sort.py", line 113, in toposort2
    assert not data, "A cyclic dependency exists amongst %r" % data
AssertionError: A cyclic dependency exists amongst {'dw04': {'dw01'}, 'dw03': {'dw01'}, 'dw01': {'dw04'}, 'des_system_lib': {'dw01'}}
```



## R


First make the list

```R

deps <- list(
"des_system_lib" = c("std", "synopsys", "std_cell_lib", "des_system_lib", "dw02", "dw01", "ramlib", "ieee"),
"dw01" = c("ieee", "dw01", "dware", "gtech", "dw04"),
"dw02" = c("ieee", "dw02", "dware"),
"dw03" = c("std", "synopsys", "dware", "dw03", "dw02", "dw01", "ieee", "gtech"),
"dw04" = c("dw04", "ieee", "dw01", "dware", "gtech"),
"dw05" = c("dw05", "ieee", "dware"),
"dw06" = c("dw06", "ieee", "dware"),
"dw07" = c("ieee", "dware"),
"dware" = c("ieee", "dware"),
"gtech" = c("ieee", "gtech"),
"ramlib" = c("std", "ieee"),
"std_cell_lib" = c("ieee", "std_cell_lib"),
"synopsys" = c())

```


Topological sort function. It will throw an error if it cannot complete, printing the list of items which cannot be ordered.
If it succeeds, returns the list of items in topological order.

```R

tsort <- function(deps) {
	nm <- names(deps)
	libs <- union(as.vector(unlist(deps)), nm)

	s <- c()
	# first libs that depend on nothing
	for(x in libs) {
		if(!(x %in% nm)) {
			s <- c(s, x)
		}
	}

	k <- 1
	while(k > 0) {
		k <- 0
		for(x in setdiff(nm, s)) {
			r <- c(s, x)
			if(length(setdiff(deps[[x]], r)) == 0) {
				s <- r
				k <- 1
			}
		}
	}

	if(length(s) < length(libs)) {
		v <- setdiff(libs, s)
		stop(sprintf("Unorderable items :\n%s", paste("", v, sep="", collapse="\n")))
	}

	s
}

```


On the given example :

```R

tsort(deps)
# [1] "std"            "ieee"           "dware"          "gtech"          "ramlib"
# [6] "std_cell_lib"   "synopsys"       "dw01"           "dw02"           "dw03"
#[11] "dw04"           "dw05"           "dw06"           "dw07"           "des_system_lib"

```


If dw01 depends on dw04 as well :


```R

Unorderable items :
des_system_lib
dw01
dw04
dw03

```



## Racket


```racket

#lang racket

(define G
  (make-hash
   '((des_system_lib . (std synopsys std_cell_lib des_system_lib dw02
                            dw01 ramlib ieee))
     (dw01           . (ieee dw01 dware gtech))
     (dw02           . (ieee dw02 dware))
     (dw03           . (std synopsys dware dw03 dw02 dw01 ieee gtech))
     (dw04           . (dw04 ieee dw01 dware gtech))
     (dw05           . (dw05 ieee dware))
     (dw06           . (dw06 ieee dware))
     (dw07           . (ieee dware))
     (dware          . (ieee dware))
     (gtech          . (ieee gtech))
     (ramlib         . (std ieee))
     (std_cell_lib   . (ieee std_cell_lib))
     (synopsys       . ()))))

(define (clean G)
  (define G* (hash-copy G))
  (for ([(from tos) G])
    ; remove self dependencies
    (hash-set! G* from (remove from tos))
    ; make sure all nodes are present in the ht
    (for ([to tos]) (hash-update! G* to (λ(_)_) '())))
  G*)

(define (incoming G)
  (define in (make-hash))
  (for* ([(from tos) G] [to tos])
    (hash-update! in to (λ(fs) (cons from fs)) '()))
  in)

(define (nodes G)       (hash-keys G))
(define (out G n)       (hash-ref G n '()))
(define (remove! G n m) (hash-set! G n (remove m (out G n))))

(define (topo-sort G)
  (define n (length (nodes G)))
  (define in (incoming G))
  (define (no-incoming? n) (empty? (hash-ref in n '())))
  (let loop ([L '()] [S (list->set (filter no-incoming? (nodes G)))])
    (cond [(set-empty? S)
           (if (= (length L) n)
               L
               (error 'topo-sort (~a "cycle detected" G)))]
          [else
           (define n   (set-first S))
           (define S\n (set-rest S))
           (for ([m (out G n)])
             (remove! G n m)
             (remove! in m n)
             (when (no-incoming? m)
               (set! S\n (set-add S\n m))))
           (loop (cons n L) S\n)])))

(topo-sort (clean G))

```

Output:

```racket

'(synopsys ieee dware gtech std_cell_lib std ramlib dw07 dw06 dw05 dw01 dw04 dw02 dw03 des_system_lib)

```



## REXX

{{trans|FORTRAN 77}}

Some of the FORTRAN 77 statements were converted to   '''do'''   loops (or   '''do'''   structures),   and

some variables were   [https://en.wikipedia.org/wiki/Camel_case <u>''camel capitalized]''</u>.

```rexx
/*REXX pgm does a topological sort (orders such that no item precedes a dependent item).*/
iDep.=  0;      iPos.=  0;         iOrd.=  0     /*initialize some stemmed arrays to  0.*/
   nL= 15;         nd= 44;            nc= 69     /*     "       "  "parms"  and indices.*/
label= 'DES_SYSTEM_LIB   DW01     DW02    DW03   DW04   DW05   DW06   DW07'  ,
       'DWARE    GTECH   RAMLIB   STD_CELL_LIB   SYNOPSYS      STD    IEEE'
iCode= 1 14 13 12 1 3 2 11 15 0 2 15 2 9 10 0 3 15 3 9 0 4 14 213 9 4 3 2 15 10 0 5 5 15 ,
      2 9 10 0 6 6 15 9 0 7 7 15 9 0 8 15 9 0 39 15 9 0 10 15 10 0 11 14 15 0 12 15 12 0 0
j= 0
             do i=1
             iL= word(iCode, i);          if iL==0  then leave
                do forever;               i= i+1
                iR= word(iCode, i);       if iR==0  then leave
                j= j+1;                   iDep.j.1= iL
                                          iDep.j.2= iR
                end   /*forever*/
             end      /*i*/
call tSort
say '═══compile order═══'
@=  'libraries found.)'
#=0;                            do o=nO  by -1  for nO;   #= #+1;  say word(label, iOrd.o)
                                end   /*o*/;              if #==0  then #= 'no'
say '   ('#   @;        say
say '═══unordered libraries═══'
#=0;                            do u=nO+1  to nL;         #= #+1;  say word(label, iOrd.u)
                                end   /*u*/;              if #==0  then #= 'no'
say '   ('#   "unordered"  @
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tSort: procedure expose iDep. iOrd. iPos. nd nL nO
                                do i=1  for nL;  iOrd.i= i;  iPos.i= i
                                end   /*i*/
       k= 1
                                do  until k<=j;              j  = k;            k= nL+1
                                    do i=1  for nd;          iL = iDep.i.1;    iR= iPos.iL
                                    ipL= iPos.iL;            ipR= iPos.iR
                                    if iL==iR | ipL>.k | ipL<j | ipR<j  then iterate
                                    k= k-1
                                    _= iOrd.k;               iPos._ = ipL
                                                             iPos.iL= k
                                    iOrd.ipL= iOrd.k;        iOrd.k = iL
                                    end   /*i*/
                                end       /*until*/
       nO= j-1;     return
```

{{out|output}}

```txt

═══compile order═══
IEEE
STD
SYNOPSYS
STD_CELL_LIB
RAMLIB
GTECH
DWARE
DW07
DW06
DW05
DW04
DW03
DW02
DW01
DES_SYSTEM_LIB
   (15 libraries found.)

═══unordered libraries═══
   (no unordered libraries found.)

```



## Ruby

Uses the [http://www.ruby-doc.org/stdlib/libdoc/tsort/rdoc/classes/TSort.html TSort] module from the Ruby stdlib.

```ruby
require 'tsort'
class Hash
  include TSort
  alias tsort_each_node each_key
  def tsort_each_child(node, &block)
    fetch(node).each(&block)
  end
end

depends = {}
DATA.each do |line|
  key, *libs = line.split
  depends[key] = libs
  libs.each {|lib| depends[lib] ||= []}
end

begin
  p depends.tsort
  depends["dw01"] << "dw04"
  p depends.tsort
rescue TSort::Cyclic => e
  puts "\ncycle detected: #{e}"
end

__END__
des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01             ieee dw01 dware gtech
dw02             ieee dw02 dware
dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
dw04             dw04 ieee dw01 dware gtech
dw05             dw05 ieee dware
dw06             dw06 ieee dware
dw07             ieee dware
dware            ieee dware
gtech            ieee gtech
ramlib           std ieee
std_cell_lib     ieee std_cell_lib
synopsys
```

{{out}}

```txt

["std", "synopsys", "ieee", "std_cell_lib", "dware", "dw02", "gtech", "dw01", "ramlib", "des_system_lib", "dw03", "dw04", "dw05", "dw06", "dw07"]

cycle detected: topological sort failed: ["dw01", "dw04"]

```



## Sidef

{{trans|Perl}}

```ruby
func print_topo_sort (deps) {
    var ba = Hash.new;
    deps.each { |before, afters|
        afters.each { |after|
            if (before != after) {
                ba{before}{after} = 1;
            };
            ba{after} \\= Hash.new;
        }
    };

    loop {
        var afters = ba.keys.grep {|k| ba{k}.values.len == 0 }.sort;
        afters.len || break;
        say afters.join(" ");
        ba.delete(afters...);
        ba.values.each { |v| v.delete(afters...) };
    };

    say (ba.len ? "Cicle found! #{ba.keys.sort}" : "---");
}

var deps = Hash.new(
    des_system_lib => < std synopsys std_cell_lib des_system_lib dw02
                                                     dw01 ramlib ieee >,
    dw01           => < ieee dw01 dware gtech                         >,
    dw02           => < ieee dw02 dware                               >,
    dw03           => < std synopsys dware dw03 dw02 dw01 ieee gtech  >,
    dw04           => < dw04 ieee dw01 dware gtech                    >,
    dw05           => < dw05 ieee dware                               >,
    dw06           => < dw06 ieee dware                               >,
    dw07           => < ieee dware                                    >,
    dware          => < ieee dware                                    >,
    gtech          => < ieee gtech                                    >,
    ramlib         => < std ieee                                      >,
    std_cell_lib   => < ieee std_cell_lib                             >,
    synopsys       => <                                               >
);

print_topo_sort(deps);
deps{:dw01}.append('dw04');     # Add unresolvable dependency
print_topo_sort(deps);
```

{{out}}

```txt

ieee std synopsys
dware gtech ramlib std_cell_lib
dw01 dw02 dw05 dw06 dw07
des_system_lib dw03 dw04
---
ieee std synopsys
dware gtech ramlib std_cell_lib
dw02 dw05 dw06 dw07
Cicle found! des_system_lib dw01 dw03 dw04

```



## Tcl

{{works with|Tcl|8.5}}

```tcl
package require Tcl 8.5
proc topsort {data} {
    # Clean the data
    dict for {node depends} $data {
	if {[set i [lsearch -exact $depends $node]] >= 0} {
	    set depends [lreplace $depends $i $i]
	    dict set data $node $depends
	}
	foreach node $depends {dict lappend data $node}
    }
    # Do the sort
    set sorted {}
    while 1 {
	# Find available nodes
	set avail [dict keys [dict filter $data value {}]]
	if {![llength $avail]} {
	    if {[dict size $data]} {
		error "graph is cyclic, possibly involving nodes \"[dict keys $data]\""
	    }
	    return $sorted
	}
	# Note that the lsort is only necessary for making the results more like other langs
	lappend sorted {*}[lsort $avail]
        # Remove from working copy of graph
	dict for {node depends} $data {
	    foreach n $avail {
		if {[set i [lsearch -exact $depends $n]] >= 0} {
		    set depends [lreplace $depends $i $i]
		    dict set data $node $depends
		}
	    }
	}
	foreach node $avail {
	    dict unset data $node
	}
    }
}
```

Demonstration code (which parses it from the format that the puzzle was posed in):

```tcl
set inputData {
    des_system_lib	std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
    dw01		ieee dw01 dware gtech
    dw02		ieee dw02 dware
    dw03		std synopsys dware dw03 dw02 dw01 ieee gtech
    dw04		dw04 ieee dw01 dware gtech
    dw05		dw05 ieee dware
    dw06		dw06 ieee dware
    dw07		ieee dware
    dware		ieee dware
    gtech		ieee gtech
    ramlib		std ieee
    std_cell_lib	ieee std_cell_lib
    synopsys
}
foreach line [split $inputData \n] {
    if {[string trim $line] eq ""} continue
    dict set parsedData [lindex $line 0] [lrange $line 1 end]
}
puts [topsort $parsedData]
```

Sample output:

```txt
ieee std synopsys dware gtech ramlib std_cell_lib dw01 dw02 dw05 dw06 dw07 des_system_lib dw03 dw04
```

If the suggested extra arc is added, this is the error output:
<pre class="st0">graph is cyclic, possibly involving nodes "des_system_lib dw01 dw03 dw04"
```



## UNIX Shell

The Unix [http://www.openbsd.org/cgi-bin/man.cgi?query=tsort&apropos=0&sektion=1&manpath=OpenBSD+Current&arch=i386&format=html tsort(1)] utility does a topological sort. Each line of input must have two items in order, like 'std des_system_lib'.<ref>
[[wp: tsort]]
</ref>

{{works with|Bourne Shell}}

```bash
$ awk '{ for (i = 1; i <= NF; i++) print $i, $1 }' <<! | tsort
> des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
> dw01             ieee dw01 dware gtech
> dw02             ieee dw02 dware
> dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
> dw04             dw04 ieee dw01 dware gtech
> dw05             dw05 ieee dware
> dw06             dw06 ieee dware
> dw07             ieee dware
> dware            ieee dware
> gtech            ieee gtech
> ramlib           std ieee
> std_cell_lib     ieee std_cell_lib
> synopsys
> !
ieee
dware
dw02
dw05
dw06
dw07
gtech
dw01
dw04
std_cell_lib
synopsys
std
dw03
ramlib
des_system_lib
```


If the graph of dependencies contains a cycle, [[BSD]]'s tsort(1) will print messages to standard error, break the cycle (by deleting one of the dependencies), continue the sort, and exit 0. So if dw04 becomes a dependency of dw01, then tsort(1) finds the cycle between dw01 and dw04.


```txt
ieee
dware
dw02
dw05
dw06
dw07
gtech
std_cell_lib
synopsys
std
ramlib
tsort: cycle in data
tsort: dw01
tsort: dw04
dw01
des_system_lib
dw03
dw04
```



## Ursala

The tsort function takes a list of pairs
<(lib: <dep...>)...> and returns a pair of lists (<lib...>,<lib...>)
with the topologically sorted libraries on the left and the
unorderable libraries, if any, on the right. Self-dependences are
ignored and unlisted libraries are presumed independent.

```Ursala
tsort = ~&nmnNCjA*imSLs2nSjiNCSPT; @NiX ^=lxPrnSPX ^(~&rlPlT,~&rnPrmPljA*D@r)^|/~& ~&m!=rnSPlX
```

test program:

```Ursala
#import std

dependence_table = -[

LIBRARY          LIBRARY DEPENDENCIES

### ====          =================

des_system_lib   std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee
dw01             ieee dw01 dware gtech
dw02             ieee dw02 dware
dw03             std synopsys dware dw03 dw02 dw01 ieee gtech
dw04             dw04 ieee dw01 dware gtech
dw05             dw05 ieee dware
dw06             dw06 ieee dware
dw07             ieee dware
dware            ieee dware
gtech            ieee gtech
ramlib           std ieee
std_cell_lib     ieee std_cell_lib
synopsys         ]-

parse = ~&htA*FS+ sep` *tttt

#show+

main = <.~&l,@r ~&i&& 'unorderable: '--> mat` ~~ tsort parse dependence_table
```

With the given table, the output is

```txt

std ieee synopsys std_cell_lib ramlib gtech dware dw07 dw06 dw05 dw02 dw01 dw04 dw03 des_system_lib

```

When the suggested dependence is added, the output becomes

```txt

std ieee synopsys std_cell_lib ramlib gtech dware dw07 dw06 dw05 dw02
unorderable: des_system_lib dw01 dw03 dw04

```



## VBScript


### ==Implementation==


```vb

class topological
	dim dictDependencies
	dim dictReported
	dim depth

	sub class_initialize
		set dictDependencies = createobject("Scripting.Dictionary")
		set dictReported = createobject("Scripting.Dictionary")
		depth = 0
	end sub

	sub reset
		dictReported.removeall
	end sub

	property let dependencies( s )
		'assuming token tab token-list newline
		dim i, j ,k
		dim aList
		dim dep
		dim a1
		aList = Split( s, vbNewLine )
		'~ remove empty lines at end
		do while aList( UBound( aList ) ) = vbnullstring
			redim preserve aList( UBound( aList ) - 1 )
		loop

		for i = lbound( aList ) to ubound( aList )
			aList( i ) = Split( aList( i ), vbTab, 2 )
			a1 = Split( aList( i )( 1 ), " " )
			k = 0
			for j = lbound( a1) to ubound(a1)
				if a1(j) <> aList(i)(0) then
					a1(k) = a1(j)
					k = k + 1
				end if
			next
			redim preserve a1(k-1)
			aList(i)(1) = a1
		next
		for i = lbound( aList ) to ubound( aList )
			dep = aList(i)(0)
			if not dictDependencies.Exists( dep ) then
				dictDependencies.add dep, aList(i)(1)
			end if
		next

	end property

	sub resolve( s )
		dim i
		dim deps
		'~ wscript.echo string(depth,"!"),s
		depth = depth + 1
		if dictDependencies.Exists(s) then
			deps = dictDependencies(s)
			for i = lbound(deps) to ubound(deps)
				resolve deps(i)
			next
		end if
		if not seen(s) then
			wscript.echo s
			see s
		end if
		depth = depth - 1
	end sub

	function seen( key )
		seen = dictReported.Exists( key )
	end function

	sub see( key )
		dictReported.add key, ""
	end sub

	property get keys
		keys = dictDependencies.keys
	end property
end class

```



### ==Invocation==


```vb

dim toposort
set toposort = new topological
toposort.dependencies = "des_system_lib	std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee" & vbNewLine & _
	"dw01	ieee dw01 dware gtech" & vbNewLine & _
	"dw02	ieee dw02 dware" & vbNewLine & _
	"dw03	std synopsys dware dw03 dw02 dw01 ieee gtech" & vbNewLine & _
	"dw04	dw04 ieee dw01 dware gtech" & vbNewLine & _
	"dw05	dw05 ieee dware" & vbNewLine & _
	"dw06	dw06 ieee dware" & vbNewLine & _
	"dw07	ieee dware" & vbNewLine & _
	"dware	ieee dware" & vbNewLine & _
	"gtech	ieee gtech" & vbNewLine & _
	"ramlib	std ieee" & vbNewLine & _
	"std_cell_lib	ieee std_cell_lib" & vbNewLine & _
	"synopsys	"

dim k
for each k in toposort.keys
	wscript.echo "----- " & k
	toposort.resolve k
	wscript.echo "-----"
	toposort.reset
next

```



### ==Output==


```txt

----- des_system_lib
std
synopsys
ieee
std_cell_lib
dware
dw02
gtech
dw01
ramlib
des_system_lib
-----
----- dw01
ieee
dware
gtech
dw01
-----
----- dw02
ieee
dware
dw02
-----
----- dw03
std
synopsys
ieee
dware
dw02
gtech
dw01
dw03
-----
----- dw04
ieee
dware
gtech
dw01
dw04
-----
----- dw05
ieee
dware
dw05
-----
----- dw06
ieee
dware
dw06
-----
----- dw07
ieee
dware
dw07
-----
----- dware
ieee
dware
-----
----- gtech
ieee
gtech
-----
----- ramlib
std
ieee
ramlib
-----
----- std_cell_lib
ieee
std_cell_lib
-----
----- synopsys
synopsys
-----

```



## Visual Basic .NET

Adapted from http://tawani.blogspot.com/2009/02/topological-sorting-and-cyclic.html which was itself an adaptation of Java code. I added the Rosetta code specific format of dependencies, as well as checks for references to self.

```vbnet
' Adapted from:
' http://tawani.blogspot.com/2009/02/topological-sorting-and-cyclic.html
' added/changed:
' - conversion to VB.Net (.Net 2 framework)
' - added Rosetta Code dependency format parsing
' - check & removal of self-dependencies before sorting
Module Program
	Sub Main()
		Dim Fields As New List(Of Field)()
		' You can also add Dependson using code like:
		' .DependsOn = New String() {"ieee", "dw01", "dware"} _

		fields.Add(New Field() With { _
			.Name = "des_system_lib", _
			.DependsOn = Split("std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "dw01", _
			.DependsOn = Split("ieee dw01 dware gtech", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "dw02", _
			.DependsOn = Split("ieee dw02 dware", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "dw03", _
			.DependsOn = Split("std synopsys dware dw03 dw02 dw01 ieee gtech", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "dw04", _
			.DependsOn = Split("dw04 ieee dw01 dware gtech", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "dw05", _
			.DependsOn = Split("dw05 ieee dware", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "dw06", _
			.DependsOn = Split("dw06 ieee dware", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "dw07", _
			.DependsOn = Split("ieee dware", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "dware", _
			.DependsOn = Split("ieee dware", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "gtech", _
			.DependsOn = Split("ieee gtech", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "ramlib", _
			.DependsOn = Split("std ieee", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "std_cell_lib", _
			.DependsOn = Split("ieee std_cell_lib", " ") _
		})
		fields.Add(New Field() With { _
			.Name = "synopsys" _
		})
		Console.WriteLine("Input:")
		For Each ThisField As field In fields
			Console.WriteLine(ThisField.Name)
			If ThisField.DependsOn IsNot Nothing Then
				For Each item As String In ThisField.DependsOn
					Console.WriteLine(" -{0}", item)
				Next
			End If
		Next

		Console.WriteLine(vbLf & "...Sorting..." & vbLf)

		Dim sortOrder As Integer() = getTopologicalSortOrder(fields)

		For i As Integer = 0 To sortOrder.Length - 1
			Dim field = fields(sortOrder(i))
			Console.WriteLine(field.Name)
			' Write up dependencies, too:
			'If field.DependsOn IsNot Nothing Then
			'	For Each item As String In field.DependsOn
			'		Console.WriteLine(" -{0}", item)
			'	Next
			'End If
		Next
		Console.Write("Press any key to continue . . . ")
		Console.ReadKey(True)
	End Sub

	Private Sub CheckDependencies (ByRef Fields As List(Of Field))
		' Make sure all objects we depend on are part of the field list
		' themselves, as there may be dependencies that are not specified as fields themselves.
		' Remove dependencies on fields themselves.Y
		Dim AField As Field, ADependency As String

		For i As Integer = Fields.Count - 1 To 0 Step -1
			AField=fields(i)
			If AField.DependsOn IsNot Nothing  then
				For j As Integer = 0 To Ubound(AField.DependsOn)
					ADependency = Afield.DependsOn(j)
					' We ignore fields that depends on themselves:
					If AField.Name <> ADependency then
						If ListContainsVertex(fields, ADependency) = False Then
							' Add the dependent object to the field list, as it
							' needs to be there, without any dependencies
							Fields.Add(New Field() With { _
								.Name = ADependency _
							})
						End If
					End If
				Next j
			End If
		Next i
	End Sub

	Private Sub RemoveSelfDependencies (ByRef Fields As List(Of Field))
		' Make sure our fields don't depend on themselves.
		' If they do, remove the dependency.
		Dim InitialUbound as Integer
		For Each AField As Field In Fields
			If AField.DependsOn IsNot Nothing Then
				InitialUbound = Ubound(AField.DependsOn)
				For i As Integer = InitialUbound to 0 Step - 1
					If Afield.DependsOn(i) = Afield.Name Then
						' This field depends on itself, so remove
						For j as Integer = i To UBound(AField.DependsOn)-1
							Afield.DependsOn(j)=Afield.DependsOn(j+1)
						Next
						ReDim Preserve Afield.DependsOn(UBound(Afield.DependsOn)-1)
					End If
				Next
			End If
		Next
	End Sub

	Private Function ListContainsVertex(Fields As List(Of Field), VertexName As String) As Boolean
	' Check to see if the list of Fields already contains a vertext called VertexName
	Dim Found As Boolean = False
		For i As Integer = 0 To fields.Count - 1
			If Fields(i).Name = VertexName Then
				Found = True
				Exit For
			End If
		Next
		Return Found
	End Function

	Private Function getTopologicalSortOrder(ByRef Fields As List(Of Field)) As Integer()
		' Gets sort order. Will also add required dependencies to
		' Fields.

		' Make sure we don't have dependencies on ourselves.
		' We'll just get rid of them.
		RemoveSelfDependencies(Fields)

		'First check depencies, add them to Fields if required:
		CheckDependencies(Fields)
		' Now we have the correct Fields list, so we can proceed:
		Dim g As New TopologicalSorter(fields.Count)
		Dim _indexes As New Dictionary(Of String, Integer)(fields.count)

		'add vertex names to our lookup dictionaey
		For i As Integer = 0 To fields.Count - 1
			_indexes(fields(i).Name.ToLower()) = g.AddVertex(i)
		Next

		'add edges
		For i As Integer = 0 To fields.Count - 1
			If fields(i).DependsOn IsNot Nothing Then
				For j As Integer = 0 To fields(i).DependsOn.Length - 1
					g.AddEdge(i, _indexes(fields(i).DependsOn(j).ToLower()))
				Next
			End If
		Next

		Dim result As Integer() = g.Sort()
		Return result
	End Function

	Private Class Field
		Public Property Name() As String
			Get
				Return m_Name
			End Get
			Set
				m_Name = Value
			End Set
		End Property
		Private m_Name As String
		Public Property DependsOn() As String()
			Get
				Return m_DependsOn
			End Get
			Set
				m_DependsOn = Value
			End Set
		End Property
		Private m_DependsOn As String()
	End Class
End Module
Class TopologicalSorter
	''source adapted from:
	''http://tawani.blogspot.com/2009/02/topological-sorting-and-cyclic.html
	''which was adapted from:
	''http://www.java2s.com/Code/Java/Collections-Data-Structure/Topologicalsorting.htm
	#Region "- Private Members -"

	Private ReadOnly _vertices As Integer()
	' list of vertices
	Private ReadOnly _matrix As Integer(,)
	' adjacency matrix
	Private _numVerts As Integer
	' current number of vertices
	Private ReadOnly _sortedArray As Integer()
	' Sorted vertex labels

	#End Region

	#Region "- CTors -"

	Public Sub New(size As Integer)
		_vertices = New Integer(size - 1) {}
		_matrix = New Integer(size - 1, size - 1) {}
		_numVerts = 0
		For i As Integer = 0 To size - 1
			For j As Integer = 0 To size - 1
				_matrix(i, j) = 0
			Next
		Next
			' sorted vert labels
		_sortedArray = New Integer(size - 1) {}
	End Sub

	#End Region

	#Region "- Public Methods -"

	Public Function AddVertex(vertex As Integer) As Integer
		_vertices(System.Threading.Interlocked.Increment(_numVerts)-1) = vertex
		Return _numVerts - 1
	End Function

	Public Sub AddEdge(start As Integer, [end] As Integer)
		_matrix(start, [end]) = 1
	End Sub

	Public Function Sort() As Integer()
	' Topological sort
		While _numVerts > 0
			' while vertices remain,
			' get a vertex with no successors, or -1
			Dim currentVertex As Integer = noSuccessors()
			If currentVertex = -1 Then
				' must be a cycle
				Throw New Exception("Graph has cycles")
			End If

			' insert vertex label in sorted array (start at end)
			_sortedArray(_numVerts - 1) = _vertices(currentVertex)

				' delete vertex
			deleteVertex(currentVertex)
		End While

		' vertices all gone; return sortedArray
		Return _sortedArray
	End Function

	#End Region

	#Region "- Private Helper Methods -"

	' returns vert with no successors (or -1 if no such verts)
	Private Function noSuccessors() As Integer
		For row As Integer = 0 To _numVerts - 1
			Dim isEdge As Boolean = False
			' edge from row to column in adjMat
			For col As Integer = 0 To _numVerts - 1
				If _matrix(row, col) > 0 Then
					' if edge to another,
					isEdge = True
						' this vertex has a successor try another
					Exit For
				End If
			Next
			If Not isEdge Then
				' if no edges, has no successors
				Return row
			End If
		Next
		Return -1
		' no
	End Function

	Private Sub deleteVertex(delVert As Integer)
		' if not last vertex, delete from vertexList
		If delVert <> _numVerts - 1 Then
			For j As Integer = delVert To _numVerts - 2
				_vertices(j) = _vertices(j + 1)
			Next

			For row As Integer = delVert To _numVerts - 2
				moveRowUp(row, _numVerts)
			Next

			For col As Integer = delVert To _numVerts - 2
				moveColLeft(col, _numVerts - 1)
			Next
		End If
		_numVerts -= 1
		' one less vertex
	End Sub

	Private Sub moveRowUp(row As Integer, length As Integer)
		For col As Integer = 0 To length - 1
			_matrix(row, col) = _matrix(row + 1, col)
		Next
	End Sub

	Private Sub moveColLeft(col As Integer, length As Integer)
		For row As Integer = 0 To length - 1
			_matrix(row, col) = _matrix(row, col + 1)
		Next
	End Sub

	#End Region
End Class

```


### ==Output==


```txt

Input:
des_system_lib
 -std
 -synopsys
 -std_cell_lib
 -des_system_lib
 -dw02
 -dw01
 -ramlib
 -ieee
dw01
 -ieee
 -dw01
 -dware
 -gtech
dw02
 -ieee
 -dw02
 -dware
dw03
 -std
 -synopsys
 -dware
 -dw03
 -dw02
 -dw01
 -ieee
 -gtech
dw04
 -dw04
 -ieee
 -dw01
 -dware
 -gtech
dw05
 -dw05
 -ieee
 -dware
dw06
 -dw06
 -ieee
 -dware
dw07
 -ieee
 -dware
dware
 -ieee
 -dware
gtech
 -ieee
 -gtech
ramlib
 -std
 -ieee
std_cell_lib
 -ieee
 -std_cell_lib
synopsys

...Sorting...

des_system_lib
ramlib
dw03
std
std_cell_lib
dw04
dw01
gtech
dw07
dw06
dw05
dw02
dware
ieee
synopsys
Press any key to continue . . .

```



## zkl

{{trans|Wikipedia}}
Input data is munged

```zkl
fcn topoSort(data){ // data is L( L(root,L(leaves)),...)
   allDs:=data.pump(List,fcn(rds){ T(Void.Write,Void.Write,rds[1]) }).copy();
   roots:=Dictionary(data); // dictionary of root:leaves
   L:=List();
   S:=data.pump(List,'wrap([(r,_)]){ if(allDs.holds(r)) Void.Skip else r }).copy();
   while(S){        //while S is non-empty do
      (n:=S.pop()) : L.append(_); //remove a node n from S, add n to tail of L
      foreach m in (ds:=roots.find(n,List)){ //node m with an edge e from n to m
	 allDs.del(allDs.index(m));
	 if (Void==allDs.find(m)) S.append(m); //m has no other incoming edges
      } roots.del(n);  // remove edge e from the graph
   }
   if(roots) throw(Exception.ValueError("Cycle: "+roots.keys));
   L
}
```


```zkl
data:=T(
   "des_system_lib",   "std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee",
   "dw01",             "ieee dw01 dware gtech",
   "dw02",             "ieee dw02 dware",
   "dw03",             "std synopsys dware dw03 dw02 dw01 ieee gtech",
   "dw04",             "dw04 ieee dw01 dware gtech",
   "dw05",             "dw05 ieee dware",
   "dw06",             "dw06 ieee dware",
   "dw07",             "ieee dware",
   "dware",            "ieee dware",
   "gtech",            "ieee gtech",
   "ramlib",           "std ieee",
   "std_cell_lib",     "ieee std_cell_lib",
   "synopsys",         "",
);
data=data.pump(List,Void.Read,fcn(r,ds){
   T( r, ds.replace(r,"").strip().split().copy() ) // leaves writable 'cause they will be
});
topoSort(data).println();
```

{{out}}

```txt

L("dw07","dw06","dw05","dw04","dw03","des_system_lib","ramlib",
  "std","dw01","gtech","dw02","dware","std_cell_lib","ieee","synopsys")

```

Adding dw04 to dw01 ("dw01", "ieee dw01 dware gtech dw04") and running:
{{out}}

```txt

ValueError : Cycle: L("dw01","dw04","dware","gtech")

```

