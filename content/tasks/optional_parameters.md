+++
title = "Optional parameters"
description = ""
date = 2019-09-15T10:23:59Z
aliases = []
[extra]
id = 4236
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
+++

## Task

Define a function/method/subroutine which sorts a sequence ("table") of sequences ("rows") of strings ("cells"), by one of the strings. Besides the input to be sorted, it shall have the following optional parameters:
:{|
|
----
; ordering
: A function specifying the ordering of strings; lexicographic by default.
; column
: An integer specifying which string of each row to compare; the first by default.
; reverse
: Reverses the ordering.
----
|}

This task should be considered to include both positional and named optional parameters, as well as overloading on argument count as in Java or selector name as in Smalltalk, or, in the extreme, using different function names. Provide these variations of sorting '''in whatever way is most natural to your language'''. If the language supports both methods naturally, you are encouraged to describe both.

Do not implement a sorting algorithm; this task is about the interface. If you can't use a built-in sort routine, just omit the implementation (with a comment).

See also:
* [[Named Arguments]]





## Ada


As described in [[Named_parameters]], all parameters have to be named. You can use positional or keyed association. Optional parameters are the ones with default values.


```Ada
package Tables is

   type Table is private;

   type Ordering is (Lexicographic, Psionic, ...); -- add others

   procedure Sort (It               : in out Table;
                   Order_By         : in Ordering := Lexicographic;
                   Column           : in Positive := 1;
                   Reverse_Ordering : in Boolean  := False);

private
   ... -- implementation specific
end Tables;
```


example of use:

```Ada
with Tables;
procedure Table_Test is
   My_Table : Tables.Table;
begin
   ... -- insert stuff in table
   Sort (My_Table); -- use default sorting
   Sort (My_Table, Psionic, 5, True); -- use psionic sorting by 5th column in reverse order
   Sort (It => My_Table, Reverse_Ordering => True); -- use default sorting in reverse order
   ... -- other stuff
end Table_Test;
```



## ALGOL 68


```algol68
# as the options have distinct types (INT, BOOL and PROC( STRING, STRING )INT) the       #
# easiest way to support these optional parameters in Algol 68 would be to have an array #
# with elements of these types                                                           #
# See the Named Arguments sample for cases where the option types are not distinct       #

# default comparison function #
PROC default compare = ( STRING a, b )INT: IF a < b THEN -1 ELIF a = b THEN 0 ELSE 1 FI;

# sorting procedure #
PROC configurable sort = ( [,]STRING data, []UNION( INT, BOOL, PROC( STRING, STRING )INT ) options )VOID:
BEGIN
    # set initial values for the options #
    INT    sort column                   := 2 LWB data;
    BOOL   reverse sort                  := FALSE;
    PROC( STRING, STRING )INT comparator := default compare;
    # overide from the supplied options #
    FOR opt pos FROM LWB options TO UPB options DO
        CASE options[ opt pos ]
          IN ( PROC( STRING, STRING )INT p ): comparator   := p
           , ( INT c ):                       sort column  := c
           , ( BOOL r ):                      reverse sort := r
        ESAC
    OD
    # do the sort .... #
END # configurable sort # ;

# example calls #
[ 1 : 2, 1 : 3 ]STRING data := ( ( "a", "bb", "cde" ), ( "x", "abcdef", "Q" ) );

# sort data, default comparison, first column, reverse order #
configurable sort( data, ( TRUE ) );
# sort data, second column, ignore first chaacter when sorting, normal order #
configurable sort( data, ( 2, ( STRING a, STRING b )INT: default compare( a[ LWB a + 1 : ], b[ LWB b + 1 : ] ) ) );
# default sort #
configurable sort( data, () )
```



## AppleScript

AppleScript supports named, positional & prepositional parameters, but not default or optional parameters. Though that behavior can be simulated by passing lists or records as the parameter. Handler/functions can be passed as a parameter if they are part of a script object. AppleScript does not have built-in sorting functionality.

```AppleScript
on sortTable(x)
	set {sortOrdering, sortColumn, sortReverse} to {sort_lexicographic, 1, false}
	try
		set sortOrdering to x's ordering
	end try
	try
		set sortColumn to x's column
	end try
	try
		set sortReverse to x's reverse
	end try
	return sortOrdering's sort(x's sequence, sortColumn, sortReverse)
end sortTable

script sort_lexicographic
	on sort(table, column, reverse)
		-- Implement lexicographic Sorting process here.
		return table
	end sort
end script
```

Examples of use:

```AppleScript
-- Another sort function.
script sort_colex
	on sort(table, column, reverse)
		-- Implement colexicographic Sorting process here.
		return table
	end sort
end script

-- Populate a table (list) with data.
set table to {{1,2},{3,4}}

sortTable({sequence:table, ordering:sort_lexicographic, column:1, reverse:false})
sortTable({sequence:table, ordering:sort_colex, column:2, reverse:true})
sortTable({sequence:table, reverse:true})
sortTable({sequence:table})
```



## AutoHotkey

built in support for table sorting is available through the standard Win32 listview.

```AutoHotkey
Gosub start ; create and show the gui
sort_table("Text", column := 2, reverse := 1)  ;  lexicographic sort
Sleep, 2000
sort_table("Integer", column := 2, reverse := 1)  ;  numerical sort
Return

start:
  Gui, Add, ListView, r20 w200, 1|2|3
  data =
  (
  1,2,3
  b,q,z
  c,z,z
  )
  Loop, Parse, data, `n
  {
    StringSplit, row, A_LoopField, `,
    LV_Add(row, row1, row2, row3)
  }
  LV_ModifyCol(50)  ; Auto-size columns
  Gui, Show
Return

; The function supporting named, defaulted arguments
sort_table(ordering = "Text", column = 0, reverse = 0)
{
  If reverse
    desc = desc
  LV_ModifyCol(column, "sort" . desc . " " . ordering)
}

GuiClose:
  ExitApp
```



## BASIC

In Beta BASIC and SAM BASIC, the default values for parameters (or any variable) is given with the keyword DEFAULT.

```txt

100 DEF PROC sort_table REF t$(), ordering, col, reverse
110   DEFAULT ordering=0, col=1, reverse=0
120   REM implementation of sort not shown
190 END PROC

```


Usage example:

```txt

500 DIM a$(100,80)
510 REM fill a$ with data here...
550 sort_table a$
570 sort_table a$, 1, 5, 1

```



## BBC BASIC

BBC BASIC doesn't have optional parameters, but functions can have multiple entry points which take different numbers of parameters, avoiding the need to duplicate code or call a sub-function.  Omitted parameters can be declared as LOCAL, which initialises them to zero/false.

```bbcbasic
      DIM table$(100,100)
      PROCsort_default(table$())
      PROCsort_options(table$(), TRUE, 1, FALSE)
      END

      DEF PROCsort_options(table$(), ordering%, column%, reverse%)
      DEF PROCsort_default(table$()) : LOCAL ordering%, column%, reverse%
      REM The sort goes here, controlled by the options
      REM Zero/FALSE values for the options shall select the defaults
      ENDPROC
```



## Bracmat

Bracmat functions always have exactly one parameter, which is references by <code>!arg</code> in the function body. Positional and (optional) named 'parameters' are retrieved from this single parameter <code>!arg</code> by pattern matching. It is a good custom to separate positional parameters by commas or periods and to separate the named parameters by spaces.

```bracmat
( ( sortTable
  =   table ordering column reverse
    .     !arg
        : ( ?table
          .   ( ? (ordering.?ordering) ?
              | ?&lexicographic:?ordering
              )
            : ( ? (column.?column) ?
              | ?&1:?column
              )
            : ( ? (reverse.?reverse) ?
              | ?&no:?reverse
              )
          )
      & (...)
  )
&     (12.Claes.left)
      (11.Otto.right)
      (8.Frederikke.middle)
  : ?table
& sortTable$(!table.(column.2) (reverse.yes))
);
```



## C


```cpp
#include <iostream>
#include <stdarg.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

typedef const char * String;
typedef struct sTable {
    String * *rows;
    int      n_rows,n_cols;
} *Table;

typedef int (*CompareFctn)(String a, String b);

struct {
   CompareFctn  compare;
   int   column;
   int   reversed;
} sortSpec;

int CmprRows( const void *aa, const void *bb)
{
   String *rA = *(String *const *)aa;
   String *rB = *(String *const *)bb;
   int sortCol = sortSpec.column;

   String left = sortSpec.reversed ? rB[sortCol] : rA[sortCol];
   String right = sortSpec.reversed ? rA[sortCol] : rB[sortCol];
   return sortSpec.compare( left, right );
}

/** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * tbl parameter is a table of rows of strings
 * argSpec is a string containing zero or more of the letters o,c,r
 * if o is present - the corresponding optional argument is a function which
 *      determines the ordering of the strings.
 * if c is present - the corresponding optional argument is an integer that
 *      specifies the column to sort on.
 * if r is present - the corresponding optional argument is either
 *      true(nonzero) or false(zero) and if true, the sort will b in reverse order
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int sortTable(Table tbl, const char* argSpec,... )
{
   va_list vl;
   const char *p;
   int c;
   sortSpec.compare = &strcmp;
   sortSpec.column = 0;
   sortSpec.reversed = 0;

   va_start(vl, argSpec);
   if (argSpec)
      for (p=argSpec; *p; p++) {
         switch (*p) {
         case 'o':
            sortSpec.compare = va_arg(vl,CompareFctn);
            break;
         case 'c':
            c = va_arg(vl,int);
            if ( 0<=c && c<tbl->n_cols)
               sortSpec.column  = c;
            break;
         case 'r':
            sortSpec.reversed = (0!=va_arg(vl,int));
            break;
         }
      }
   va_end(vl);
   qsort( tbl->rows, tbl->n_rows, sizeof(String *), CmprRows);
   return 0;
}

void printTable( Table tbl, FILE *fout, const char *colFmts[])
{
   int row, col;

   for (row=0; row<tbl->n_rows; row++) {
      fprintf(fout, "   ");
      for(col=0; col<tbl->n_cols; col++) {
         fprintf(fout, colFmts[col], tbl->rows[row][col]);
      }
      fprintf(fout, "\n");
   }
   fprintf(fout, "\n");
}

int ord(char v)
{
    return v-'0';
}

/* an alternative comparison function */
int cmprStrgs(String s1, String s2)
{
    const char *p1 = s1;
    const char *p2 = s2;
    const char *mrk1, *mrk2;
    while ((tolower(*p1) == tolower(*p2)) && *p1) {
       p1++; p2++;
    }
    if (isdigit(*p1) && isdigit(*p2)) {
        long v1, v2;
        if ((*p1 == '0') ||(*p2 == '0')) {
            while (p1 > s1) {
                p1--; p2--;
                if (*p1 != '0') break;
            }
            if (!isdigit(*p1)) {
                p1++; p2++;
            }
        }
        mrk1 = p1; mrk2 = p2;
        v1 = 0;
        while(isdigit(*p1)) {
            v1 = 10*v1+ord(*p1);
            p1++;
        }
        v2 = 0;
        while(isdigit(*p2)) {
            v2 = 10*v2+ord(*p2);
            p2++;
        }
        if (v1 == v2)
           return(p2-mrk2)-(p1-mrk1);
        return v1 - v2;
    }
    if (tolower(*p1) != tolower(*p2))
       return (tolower(*p1) - tolower(*p2));
    for(p1=s1, p2=s2; (*p1 == *p2) && *p1; p1++, p2++);
    return (*p1 -*p2);
}

int main()
{
   const char *colFmts[] = {" %-5.5s"," %-5.5s"," %-9.9s"};
   String r1[] = { "a101", "red",  "Java" };
   String r2[] = { "ab40", "gren", "Smalltalk" };
   String r3[] = { "ab9",  "blue", "Fortran" };
   String r4[] = { "ab09", "ylow", "Python" };
   String r5[] = { "ab1a", "blak", "Factor" };
   String r6[] = { "ab1b", "brwn", "C Sharp" };
   String r7[] = { "Ab1b", "pink", "Ruby" };
   String r8[] = { "ab1",  "orng", "Scheme" };

   String *rows[] = { r1, r2, r3, r4, r5, r6, r7, r8 };
   struct sTable table;
   table.rows = rows;
   table.n_rows = 8;
   table.n_cols = 3;

   sortTable(&table, "");
   printf("sort on col 0, ascending\n");
   printTable(&table, stdout, colFmts);

   sortTable(&table, "ro", 1, &cmprStrgs);
   printf("sort on col 0, reverse.special\n");
   printTable(&table, stdout, colFmts);

   sortTable(&table, "c", 1);
   printf("sort on col 1, ascending\n");
   printTable(&table, stdout, colFmts);

   sortTable(&table, "cr", 2, 1);
   printf("sort on col 2, reverse\n");
   printTable(&table, stdout, colFmts);
   return 0;
}
```



## C++

This implementation only accepts function pointers for the comparators, and does not accept function objects, for simplicity.

```cpp
#include <vector>
#include <algorithm>
#include <string>

// helper comparator that is passed to std::sort()
template <class T>
struct sort_table_functor {
  typedef bool (*CompFun)(const T &, const T &);
  const CompFun ordering;
  const int column;
  const bool reverse;
  sort_table_functor(CompFun o, int c, bool r) :
    ordering(o), column(c), reverse(r) { }
  bool operator()(const std::vector<T> &x, const std::vector<T> &y) const {
    const T &a = x[column],
            &b = y[column];
    return reverse ? ordering(b, a)
                   : ordering(a, b);
  }
};

// natural-order less-than comparator
template <class T>
bool myLess(const T &x, const T &y) { return x < y; }

// this is the function we call, which takes optional parameters
template <class T>
void sort_table(std::vector<std::vector<T> > &table,
                int column = 0, bool reverse = false,
                bool (*ordering)(const T &, const T &) = myLess) {
  std::sort(table.begin(), table.end(),
            sort_table_functor<T>(ordering, column, reverse));
}

#include <iostream>

// helper function to print our 3x3 matrix
template <class T>
void print_matrix(std::vector<std::vector<T> > &data) {
  for () {
    for (int j = 0; j < 3; j++)
      std::cout << data[i][j] << "\t";
    std::cout << std::endl;
  }
}

// order in descending length
bool desc_len_comparator(const std::string &x, const std::string &y) {
  return x.length() > y.length();
}

int main() {

  std::string data_array[3][3] =
    {
      {"a", "b", "c"},
      {"", "q", "z"},
      {"zap", "zip", "Zot"}
    };

  std::vector<std::vector<std::string> > data_orig;
  for (int i = 0; i < 3; i++) {
    std::vector<std::string> row;
    for (int j = 0; j < 3; j++)
      row.push_back(data_array[i][j]);
    data_orig.push_back(row);
  }
  print_matrix(data_orig);

  std::vector<std::vector<std::string> > data = data_orig;
  sort_table(data);
  print_matrix(data);

  data = data_orig;
  sort_table(data, 2);
  print_matrix(data);

  data = data_orig;
  sort_table(data, 1);
  print_matrix(data);

  data = data_orig;
  sort_table(data, 1, true);
  print_matrix(data);

  data = data_orig;
  sort_table(data, 0, false, desc_len_comparator);
  print_matrix(data);

  return 0;
}
```



## D

```d
import std.stdio, std.algorithm, std.functional;

string[][] sortTable(string[][] table,
                     in bool function(string[],string[]) ordering=null,
                     in int column = 0,
                     in bool reverse = false) {
    if (ordering is null)
        table.schwartzSort!(row => row[column])();
    else
        table.sort!ordering();
    if (reverse)
        table.reverse();
    return table;
}

void main() {
    auto data = [["a", "b", "c"],
                 ["", "q", "z"],
                 ["zap", "zip", "Zot"]];

    alias show = curry!(writefln, "%-(%s\n%)\n");
    show(data);
    show(sortTable(data));
    show(sortTable(data, null, 2));
    show(sortTable(data, null, 1));
    show(sortTable(data, null, 1, true));
    show(sortTable(data, (a,b) => b.length > a.length));
}
```

```txt
["a", "b", "c"]
["", "q", "z"]
["zap", "zip", "Zot"]

["", "q", "z"]
["a", "b", "c"]
["zap", "zip", "Zot"]

["zap", "zip", "Zot"]
["a", "b", "c"]
["", "q", "z"]

["a", "b", "c"]
["", "q", "z"]
["zap", "zip", "Zot"]

["zap", "zip", "Zot"]
["", "q", "z"]
["a", "b", "c"]

["zap", "zip", "Zot"]
["", "q", "z"]
["a", "b", "c"]

```

Another way to emulate optional arguments is with function overloading, creating several functions with a different number of arguments.


## Clojure

There is a built-in sort routine, but rather than figure out what all these arguments are supposed to mean, I've just defined the interface.


```Clojure
(defn sort [table & {:keys [ordering column reverse?]
                     :or {ordering :lex, column 1}}]
  (println table ordering column reverse?))

(sort [1 8 3] :reverse? true)
[1 8 3] :lex 1 true
```



## Common Lisp


Common Lisp has both named and positional parameters. The following example shows optional named parameters, using the <code>&key</code> keyword. Optional positional parameters are specified using the <code>&optional</code> keyword.


```lisp
(defun sort-table (table &key (ordering #'string<)
                              (column 0)
                              reverse)
  (sort table (if reverse
                  (complement ordering)
                  ordering)
              :key (lambda (row) (elt row column))))
```


(Notes: The builtin [http://www.lispworks.com/documentation/HyperSpec/Body/f_sort_.htm sort] takes a "less than" predicate function. The [http://www.lispworks.com/documentation/HyperSpec/Body/f_comple.htm complement] function inverts a predicate.)

Example uses:

```lisp
CL-USER> (defparameter *data* '(("a" "b" "c") ("" "q" "z") ("zap" "zip" "Zot")))
*DATA*

CL-USER> (sort-table *data*)
(("" "q" "z") ("a" "b" "c") ("zap" "zip" "Zot"))

CL-USER> (sort-table *data* :column 2)
(("zap" "zip" "Zot") ("a" "b" "c") ("" "q" "z"))

CL-USER> (sort-table *data* :column 1)
(("a" "b" "c") ("" "q" "z") ("zap" "zip" "Zot"))

CL-USER> (sort-table *data* :column 1 :reverse t)
(("zap" "zip" "Zot") ("" "q" "z") ("a" "b" "c"))

CL-USER> (sort-table *data* :ordering (lambda (a b) (> (length a) (length b))))
(("zap" "zip" "Zot") ("a" "b" "c") ("" "q" "z"))
```



## E


In E, as in Java and Smalltalk, optional parameters are defined as different methods with the same base name. Methods are distinguished by name (''verb'') and number of parameters (''arity'').


```e
def defaultOrdering(a, b) { return a.op__cmp(b) }

def sort {

    to run(table) {
        return sort(table, 0, false, defaultOrdering)
    }
    to run(table, column) {
        return sort(table, column, false, defaultOrdering)
    }
    to run(table, column, reverse) {
        return sort(table, column, reverse, defaultOrdering)
    }

    to run(table :List[List[String]], column :int, reverse :boolean, ordering) {
        return table.sort(fn a, b {
            def ord := ordering(a[column], b[column])
            if (reverse) { -ord } else { ord }
        })
    }

}
```


Named parameters are not builtin, but map-patterns may be used as a substitute. (TODO: Example of this) [[Category:E examples needing attention]]


## Elixir

```elixir
defmodule Optional_parameters do
  def sort( table, options\\[] ) do
    options = options ++ [ ordering: :lexicographic, column: 0, reverse: false ]
    ordering = options[ :ordering ]
    column   = options[ :column ]
    reverse  = options[ :reverse ]
    sorted = sort( table, ordering, column )
    if reverse, do: Enum.reverse( sorted ), else: sorted
  end

  defp sort( table, :lexicographic, column ) do
    Enum.sort_by( table, &elem( &1, column ) )
  end
  defp sort( table, :numeric, column ) do
    Enum.sort_by( table, &elem( &1, column ) |> String.to_integer )
  end

  def task do
    table = [ { "123", "456", "0789" },
              { "456", "0789", "123" },
              { "0789", "123", "456" } ]
    IO.write "sort defaults "; IO.inspect sort( table )
    IO.write " & reverse    "; IO.inspect sort( table, reverse: true )
    IO.write "sort column 2 "; IO.inspect sort( table, column: 2)
    IO.write " & reverse    "; IO.inspect sort( table, column: 2, reverse: true)
    IO.write "sort numeric  "; IO.inspect sort( table, ordering: :numeric)
    IO.write " & reverse    "; IO.inspect sort( table, ordering: :numeric, reverse: true)
  end
end

Optional_parameters.task
```


```txt

sort defaults [{"0789", "123", "456"}, {"123", "456", "0789"}, {"456", "0789", "123"}]
 & reverse    [{"456", "0789", "123"}, {"123", "456", "0789"}, {"0789", "123", "456"}]
sort column 2 [{"123", "456", "0789"}, {"456", "0789", "123"}, {"0789", "123", "456"}]
 & reverse    [{"0789", "123", "456"}, {"456", "0789", "123"}, {"123", "456", "0789"}]
sort numeric  [{"123", "456", "0789"}, {"456", "0789", "123"}, {"0789", "123", "456"}]
 & reverse    [{"0789", "123", "456"}, {"456", "0789", "123"}, {"123", "456", "0789"}]

```



## Erlang


```Erlang

-module( optional_parameters ).

-export( [sort/2, task/0] ).

sort( Table, Options ) ->
	Ordering = proplists:get_value( ordering, Options, lexicographic ),
	Column = proplists:get_value( column, Options, 1 ),
	Is_reverse = proplists:get_value( reverse, Options, false ),
	Sorted = sort( Table, Ordering, Column ),
	sorted_reverse( Is_reverse, Sorted ).

task() ->
	io:fwrite( "sort defaults ~p~n", [sort( table(), [])] ),
	io:fwrite( "reverse ~p~n", [sort( table(), [reverse])] ),
	io:fwrite( "sort column 3 ~p~n", [sort( table(), [{column, 3}])] ),
	io:fwrite( "reverse ~p~n", [sort( table(), [{column, 3}, reverse])] ),
	io:fwrite( "sort numeric ~p~n", [sort( table(), [{ordering, numeric}])] ),
	io:fwrite( "reverse ~p~n", [sort( table(), [{ordering, numeric}, reverse])] ).



row_numeric( Tuple ) -> erlang:list_to_tuple( [{erlang:list_to_integer(X), X} || X <- erlang:tuple_to_list(Tuple)] ).

row_remove_numeric( Tuple ) -> erlang:list_to_tuple( [Y || {_X, Y} <- erlang:tuple_to_list(Tuple)] ).

sort( Table, lexicographic, Column ) -> lists:keysort( Column, Table );
sort( Table, numeric, Column ) ->
	Numeric_table = [row_numeric(X) || X <- Table],
	Sorted_numeric = lists:keysort( Column, Numeric_table ),
	[row_remove_numeric(X) || X <- Sorted_numeric].

sorted_reverse( true, Sorted ) -> lists:reverse( Sorted );
sorted_reverse( false, Sorted ) -> Sorted.

table() -> [table_row1(), table_row2(), table_row3()].

table_row1() -> {"123", "456", "0789"}.
table_row2() -> {"456", "0789", "123"}.
table_row3() -> {"0789", "123", "456"}.

```

```txt

23> optional_parameters:task().
sort defaults [{"0789","123","456"},{"123","456","0789"},{"456","0789","123"}]
reverse [{"456","0789","123"},{"123","456","0789"},{"0789","123","456"}]
sort column 3 [{"123","456","0789"},{"456","0789","123"},{"0789","123","456"}]
reverse [{"0789","123","456"},{"456","0789","123"},{"123","456","0789"}]
sort numeric [{"123","456","0789"},{"456","0789","123"},{"0789","123","456"}]
reverse [{"0789","123","456"},{"456","0789","123"},{"123","456","0789"}]

```



## Fortran

In Fortran, each argument has its "name". The <tt>optional</tt> attribute can be used to specify that an argument is optional, and its presence (or absence) can be tested using the <tt>present</tt> intrinsic (so that we can give a default value, or execute accordingly a totally different code).


```fortran
module ExampleOptionalParameter
  ! use any module needed for the sort function(s)
  ! and all the interfaces needed to make the code work
  implicit none
contains

  subroutine sort_table(table, ordering, column, reverse)
    type(table_type), intent(inout) :: table
    integer, optional :: column
    logical, optional :: reverse
    optional :: ordering
    interface
       integer function ordering(a, b)
         type(table_element), intent(in) :: a, b
       end function ordering
    end interface

    integer :: the_column, i
    logical :: reversing
    type(table_row) :: rowA, rowB

    if ( present(column) ) then
       if ( column > get_num_of_columns(table) ) then
          ! raise an error?
       else
          the_column = column
       end if
    else
       the_column = 1   ! a default value, de facto
    end if

    reversing = .false.  ! default value
    if ( present(reverse) ) reversing = reverse

    do
       ! loops over the rows to sort... at some point, we need
       ! comparing an element (cell) of the row, with the element
       ! in another row; ... let us suppose rowA and rowB are
       ! the two rows we are considering
       ea = get_element(rowA, the_column)
       eb = get_element(rowB, the_column)
       if ( present(ordering) ) then
          if ( .not. reversing ) then
             if ( ordering(ea, eb) > 0 ) then
                ! swap the rowA with the rowB
             end if
          else   ! < instead of >
             if ( ordering(ea, eb) < 0 ) then
                ! swap the rowA with the rowB
             end if
          end if
       else
          if ( .not. reversing ) then
             if ( lexinternal(ea, eb) > 0 ) then
                ! swap the rowA with the rowB
             end if
          else   ! < instead of >
             if ( lexinternal(ea, eb) < 0 ) then
                ! swap the rowA with the rowB
             end if
          end if
       end if
       ! ... more of the sorting algo ...
       ! ... and rows traversing ... (and an exit condition of course!)
    end do

  end subroutine sort_table

end module ExampleOptionalParameter
```



```fortran
program UsingTest
  use ExampleOptionalParameter
  implicit none

  type(table_type) :: table

  ! create the table...

  ! sorting taking from column 1, not reversed, using internal
  ! default comparator
  call sort_table(table)

  ! the same as above, but in reversed order; we MUST specify
  ! the name of the argument since it is not given in the same
  ! order of the subroutine spec
  call sort_table(table, reverse=.true.)

  ! sort the table using a custom comparator
  call sort_table(table, my_cmp)
  ! or
  call sort_table(table, ordering=my_cmp)

  ! as above, but taking from column 2
  call sort_table(table, my_cmp, 2)
  ! or (swapping the order of args for fun)
  call sort_table(table, column=2, ordering=my_cmp)

  ! with custom comparator, column 2 and reversing...
  call sort_table(table, my_cmp, 2, .true.)
  ! of course we can swap the order of optional args
  ! by prefixing them with the name of the arg

  ! sort from column 2, with internal comparator
  call sort_table(table, column=2)

end program UsingTest
```


=={{header|F_Sharp|F#}}==
F# supports optional parameters for members only, not for free-standing functions.

Optional parameters are marked by using a question mark in front of the identifier. Their values are passed as option types, i.e. as <code>Some value</code> or <code>None</code>. The helper function <code>defaultArg</code> can be used to specify default values. In the example below, we use shadowing in order to reuse the identifiers <code>ordering</code>, <code>column</code> and <code>reverse</code>.

Typically, parameters are named at the caller site when optional parameters are involved. However, this is not technically required as long as only right-most arguments are omitted.


```fsharp
type Table(rows:string[][]) =
   // in-place sorting of rows
   member x.Sort(?ordering, ?column, ?reverse) =
      let ordering = defaultArg ordering compare
      let column   = defaultArg column   0
      let reverse  = defaultArg reverse  false

      let factor = if reverse then -1 else 1
      let comparer (row1:string[]) (row2:string[]) =
         factor * ordering row1.[column] row2.[column]

      Array.sortInPlaceWith comparer rows

   member x.Print() =
      for row in rows do printfn "%A" row

// Example usage
let t = new Table([| [|"a";   "b"; "c"|]
                     [|"";    "q"; "z"|]
                     [|"can"; "z"; "a"|] |])

printfn "Unsorted"; t.Print()

t.Sort()
printfn "Default sort"; t.Print()

t.Sort(column=2)
printfn "Sorted by col. 2"; t.Print()

t.Sort(column=1)
printfn "Sorted by col. 1"; t.Print()

t.Sort(column=1, reverse=true)
printfn "Reverse sorted by col. 1"; t.Print()

t.Sort(ordering=fun s1 s2 -> compare s2.Length s1.Length)
printfn "Sorted by decreasing length"; t.Print()
```


Output:

```txt
Unsorted
[|"a"; "b"; "c"|]
[|""; "q"; "z"|]
[|"can"; "z"; "a"|]
Default sort
[|""; "q"; "z"|]
[|"a"; "b"; "c"|]
[|"can"; "z"; "a"|]
Sorted by col. 2
[|"can"; "z"; "a"|]
[|"a"; "b"; "c"|]
[|""; "q"; "z"|]
Sorted by col. 1
[|"a"; "b"; "c"|]
[|""; "q"; "z"|]
[|"can"; "z"; "a"|]
Reverse sorted by col. 1
[|"can"; "z"; "a"|]
[|""; "q"; "z"|]
[|"a"; "b"; "c"|]
Sorted by decreasing length
[|"can"; "z"; "a"|]
[|"a"; "b"; "c"|]
[|""; "q"; "z"|]
```



## Go

Go does not have optional parameters so we list the idiomatic alternatives and some less idiomatic ways of achieving an effect similar to optional parameters.

===Idiomatic (non-solutions)===

'''Zero values'''

The most idiomatic way to write this particular sorting function would be a single function that required all three parameters.  Wherever practical in Go, the zero value is used as a default, and that seems meaningful in this situation.  Given a table t with method <tt>(table) sort(less func(cell, cell) bool, column int, reverse bool)</tt>, calling <tt>t.sort(nil, 0, false)</tt> to "take the defaults" would make sense.  This approach is probably closest to "positional parameters" mentioned in the task description.  Note an idiomatic way of specifying an ordering in Go is to provide a "less" function, a function that takes two values and returns true if the the first is "less than" the second in whatever sense specifies the ordering.

'''Struct'''

As the number of optional parameters grows, at some point it can be easier to pass a struct containing all of the "parameters".  An advantage with this is that you can write a constructor function that sets defaults other than zero values.  This is also idiomatic.

Here is a partial example, partial because it doesn't really have the feel yet of "optional parameters."  Note the call to do the reverse sort takes three lines of code, one to construct the parameter struct, one to set the option, and one more to make the call.


```go
type cell string

type spec struct {
    less     func(cell, cell) bool
    column   int
    reverse  bool
}

func newSpec() (s spec) {
    // initialize any defaults
    return
}

// sort with all defaults
t.sort(newSpec())

// reverse sort
s := newSpec
s.reverse = true
t.sort(s)
```



### Struct literal with keyed elements


A solution providing more the feel of optional parameters is to pass a struct literal.  Go allows a
struct literal to be initialized with named fields but does not require all fields to be specified and does not require them to be specified in order.  Thus passing a struct literal can provide very much the feel of optional named parameters.  Given,

```go
type spec struct {
    ordering func(cell, cell) bool
    column   int
    reverse  bool
}
```

the following struct literal fills in zero values for ordering and column and assigns true to the field reverse.

```go
spec{reverse: true}
```

Structs in Go are values and are copied when passed as parameters.  The result of having a single struct parameter is that the three fields are pushed on the stack, just about like they would if they were separate parameters.  The effect is named parameters with unmentioned parameters defaulting to their zero value.

While the effect is close to that of optional parameters, Go idioms have evolved to make this technique quite non-idiomatic.  The very popular tool <tt>go vet</tt> issues a warning if a struct literal only initializes a partial set of elements.  Popular code grading services on the internet run go vet and will give your code a lower grade for using this technique.

Nevertheless, a complete program to demonstrate:

```go
package main

import (
    "fmt"
    "sort"
)

type cell string
type row []cell
type table struct {
    rows   []row
    column int
    less   func(cell, cell) bool
}

func (c cell) String() string {
    return fmt.Sprintf("%q", string(c))
}

func (t table) printRows(heading string) {
    fmt.Println("--", heading)
    for _, row := range t.rows {
        fmt.Println(row)
    }
    fmt.Println()
}

// sort.Interface
func (t table) Len() int      { return len(t.rows) }
func (t table) Swap(i, j int) { t.rows[i], t.rows[j] = t.rows[j], t.rows[i] }
func (t table) Less(i, j int) bool {
    return t.less(t.rows[i][t.column], t.rows[j][t.column])
}

// struct implements named parameter-like capability
type spec struct {
    ordering func(cell, cell) bool
    column   int
    reverse  bool
}


func (t *table) sort(s spec) {
    // set up column and comparison function for sort
    t.column = s.column
    switch {
    case s.ordering != nil:
        t.less = s.ordering
    case s.reverse:
        t.less = func(a, b cell) bool { return a > b }
    default:
        t.less = func(a, b cell) bool { return a < b }
    }

    // sort
    sort.Sort(t)

    // reverse if necessary
    if s.ordering == nil || !s.reverse {
        return
    }
    last := len(t.rows) - 1
    for i := last / 2; i >= 0; i-- {
        t.rows[i], t.rows[last-i] = t.rows[last-i], t.rows[i]
    }
}

func main() {
    t := table{rows: []row{
        {"pail", "food"},
        {"pillbox", "nurse maids"},
        {"suitcase", "airedales"},
        {"bathtub", "chocolate"},
        {"schooner", "ice cream sodas"},
    }}

    t.printRows("song")

    // no "parameters"
    t.sort(spec{})
    t.printRows("sorted on first column")

    // "named parameter" reverse.
    t.sort(spec{reverse: true})
    t.printRows("reverse sorted on first column")

    // "named parameters" column and ordering
    t.sort(spec{
        column:   1,
        ordering: func(a, b cell) bool { return len(a) > len(b) },
    })
    t.printRows("sorted by descending string length on second column")
}
```

Output:

```txt

-- song
["pail" "food"]
["pillbox" "nurse maids"]
["suitcase" "airedales"]
["bathtub" "chocolate"]
["schooner" "ice cream sodas"]

-- sorted on first column
["bathtub" "chocolate"]
["pail" "food"]
["pillbox" "nurse maids"]
["schooner" "ice cream sodas"]
["suitcase" "airedales"]

-- reverse sorted on first column
["suitcase" "airedales"]
["schooner" "ice cream sodas"]
["pillbox" "nurse maids"]
["pail" "food"]
["bathtub" "chocolate"]

-- sorted by descending string length on second column
["schooner" "ice cream sodas"]
["pillbox" "nurse maids"]
["suitcase" "airedales"]
["bathtub" "chocolate"]
["pail" "food"]

```



### Functional options

A technique that gets a nod of approval from the idiom police is sometimes termed "functional options."  This technique involves a bit of tricky machinery though and so has not really gained wide popularity.  It makes use of Go's variadic arguments and uses functions to initialize a parameter struct.  A full solution:


```go
package main

import (
    "fmt"
    "sort"
)

type cell string
type row []cell
type table struct {
    rows   []row
    column int
    less   func(cell, cell) bool
}

func (c cell) String() string {
    return fmt.Sprintf("%q", string(c))
}

func (t table) printRows(heading string) {
    fmt.Println("--", heading)
    for _, row := range t.rows {
        fmt.Println(row)
    }
    fmt.Println()
}

// sort.Interface
func (t table) Len() int      { return len(t.rows) }
func (t table) Swap(i, j int) { t.rows[i], t.rows[j] = t.rows[j], t.rows[i] }
func (t table) Less(i, j int) bool {
    return t.less(t.rows[i][t.column], t.rows[j][t.column])
}

// struct implements named parameter-like capability
type spec struct {
    ordering func(cell, cell) bool
    column   int
    reverse  bool
}

// A defined option type is not really needed by the technique, but has
// a nice advantage for documentation.  If this type is exported, then
// the the Go documentation tool go doc will organize all of the option
// functions together under the type.  (Go doc will see them as constructors
// for the type.)
type Option func(*spec)

func ordering(o func(cell, cell) bool) Option {
    return func(s *spec) { s.ordering = o }
}

func column(c int) Option {
    return func(s *spec) { s.column = c }
}

func reverse() Option {
    return func(s *spec) { s.reverse = true }
}

func (t *table) sort(options ...Option) {
    var s spec
    for _, o := range options {
        o(&s)
    }
    // set up column and comparison function for sort
    t.column = s.column
    switch {
    case s.ordering != nil:
        t.less = s.ordering
    case s.reverse:
        t.less = func(a, b cell) bool { return a > b }
    default:
        t.less = func(a, b cell) bool { return a < b }
    }

    // sort
    sort.Sort(t)

    // reverse if necessary
    if s.ordering == nil || !s.reverse {
        return
    }
    last := len(t.rows) - 1
    for i := last / 2; i >= 0; i-- {
        t.rows[i], t.rows[last-i] = t.rows[last-i], t.rows[i]
    }
}

func main() {
    t := table{rows: []row{
        {"pail", "food"},
        {"pillbox", "nurse maids"},
        {"suitcase", "airedales"},
        {"bathtub", "chocolate"},
        {"schooner", "ice cream sodas"},
    }}

    t.printRows("song")
    // no parameters
    t.sort()
    t.printRows("sorted on first column")

    // "named parameter" reverse.
    t.sort(reverse())
    t.printRows("reverse sorted on first column")

    // "named parameters" column and ordering
    byLen := func(a, b cell) bool { return len(a) > len(b) }
    t.sort(column(1), ordering(byLen))
    t.printRows("sorted by descending string length on second column")
}
```

Output same as previous solution.


## Groovy

Optional Parameters:

```groovy
def orderedSort(Collection table, column = 0, reverse = false, ordering = {x, y -> x <=> y } as Comparator) {
    table.sort(false) { x, y -> (reverse ? -1 : 1) * ordering.compare(x[column], y[column])}
}
```


Test code:

```groovy
def table = [['a', 'b', 'c'], ['', 'q', 'z'], ['zap', 'zip', 'Zot']]

assert orderedSort(table) == [['', 'q', 'z'], ['a', 'b', 'c'], ['zap', 'zip', 'Zot']]
assert orderedSort(table, 2) == [['zap', 'zip', 'Zot'], ['a', 'b', 'c'], ['', 'q', 'z']]
assert orderedSort(table, 1) == [['a', 'b', 'c'], ['', 'q', 'z'], ['zap', 'zip', 'Zot']]
assert orderedSort(table, 1, true) == [['zap', 'zip', 'Zot'],['', 'q', 'z'],['a', 'b', 'c']]
assert orderedSort(table, 0, false, {x, y -> y?.size() <=> x?.size()} as Comparator) == [['zap', 'zip', 'Zot'],['a', 'b', 'c'],['', 'q', 'z']]
```


Named Parameters:

```groovy
Collection.metaClass.orderedSort = { params ->
    def column = params?.column ?: 0
    def reverse = params?.reverse ?: false
    def ordering = params?.ordering ?: {x, y -> x <=> y } as Comparator

    table.sort(false) { x, y -> (reverse ? -1 : 1) * ordering.compare(x[column], y[column])}
}
```


Test Code:


```groovy
def table = [['a', 'b', 'c'], ['', 'q', 'z'], ['zap', 'zip', 'Zot']]

assert table.orderedSort() == [['', 'q', 'z'], ['a', 'b', 'c'], ['zap', 'zip', 'Zot']]
assert table.orderedSort(column: 2) == [['zap', 'zip', 'Zot'], ['a', 'b', 'c'], ['', 'q', 'z']]
assert table.orderedSort(column: 1) == [['a', 'b', 'c'], ['', 'q', 'z'], ['zap', 'zip', 'Zot']]
assert table.orderedSort(column: 1, reverse: true) == [['zap', 'zip', 'Zot'],['', 'q', 'z'],['a', 'b', 'c']]
assert table.orderedSort(ordering: {x, y -> y?.size() <=> x?.size()} as Comparator) == [['zap', 'zip', 'Zot'],['a', 'b', 'c'],['', 'q', 'z']]
```



## Haskell

Option 1: Using haskell's record update syntax, we can simulate named default arguments. This method has the drawback of not allowing for a parameter to be positional and named simultaneously.

```haskell

{-# LANGUAGE RecordWildCards #-}

data SorterArgs = SorterArgs { cmp :: String, col :: Int, rev :: Bool } deriving Show
defSortArgs = SorterArgs "lex" 0 False


sorter :: SorterArgs -> [[String]] -> [[String]]
sorter (SorterArgs{..}) = case cmp of
                            _ -> undefined

main = do
    sorter defSortArgs{cmp = "foo", col=1, rev=True} [[]]
    sorter defSortArgs{cmp = "foo"} [[]]
    sorter defSortArgs [[]]
    return ()

```


Option 2: This method has the drawback of being a bit verbose and requiring you to supply "Maybe a" arguments.

```haskell

import Data.Maybe (fromMaybe)
-- Use fromMaybe as an operator because its prettier
(//) = flip fromMaybe

sorter :: Maybe String -> Maybe Int -> Maybe Bool -> [[String]] -> [[String]]
sorter ((// "lex") -> cmp)
       ((// 0) -> col)
       ((// False) -> rev) = undefined

main = do
    sorter (Just "foo") (Just 1) (Just True)
    sorter Nothing Nothing Nothing

```


=={{header|Icon}} and {{header|Unicon}}==
Optional named parameters are not the norm in Icon/Unicon.  In the example below ''bubblesortf'' would be a version of [[Sorting_algorithms/Bubble_sort#Icon_and_Unicon|Bubble Sort]] modified to sort on a column number (Ordering is already supported).  It could equally be replaced by any similarly modified Rosetta sort.  The use of ''reverse'' on a list is a Unicon extension; in Icon a procedure from the IPL must be linked.

```Icon

procedure main()
   X := [ [1,2,3], [2,3,1], [3,1,2])                       # A list of lists
   Sort(X)                                                 # vanilla sort
   Sort(X,"ordering","numeric","column",2,"reverse")       # using optional parameters
end

procedure Sort(X,A[])                                      # A[] provides for a variable number of arguments
   while a := get(A) do {
      case a of {
         "ordering" :  op := case get(A) | runerr(205,a) of {
            "lexicographic"|"string":  "<<"
            "numeric": "<"
            default:  runerr(205,op)
            }
         "column"   : col := 0 < integer(col := get(A)) | runerr(205,col)
         "reverse"  : reverseorder := reverse
         default: runerr(205,a)
         }
   return (\reverseorder|1)(bubblesortf(X,\c|1,\op|"<<"))  # reverse or return the sorted list
end
```



## J



```j
srtbl=: verb define
  '' srtbl y
:
  '`ordering column reverse'=. x , (#x)}. ]`0:`0:
  |.^:reverse y /: ordering (column {"1 ])y
)
```


For simplicity, the optional arguments are all functions, and are positional (on the left -- the table, with its arbitrary number of rows and columns, is on the right).  Note also that the ordering function is expected to map its entire argument (since this offers much better efficiencies than a binary comparison).

'''Example Use'''

```j
   ]Table=: ('a';'b';'c'),('';'q';'z'),:'zip';'zap';'Zot'
┌───┬───┬───┐
│a  │b  │c  │
├───┼───┼───┤
│   │q  │z  │
├───┼───┼───┤
│zip│zap│Zot│
└───┴───┴───┘
   srtbl Table           NB. default sort
┌───┬───┬───┐
│   │q  │z  │
├───┼───┼───┤
│a  │b  │c  │
├───┼───┼───┤
│zip│zap│Zot│
└───┴───┴───┘
   ]`1: srtbl Table      NB. sort by column 1
┌───┬───┬───┐
│a  │b  │c  │
├───┼───┼───┤
│   │q  │z  │
├───┼───┼───┤
│zip│zap│Zot│
└───┴───┴───┘
   ]`2:`1: srtbl Table   NB. reverse sort by column 2
┌───┬───┬───┐
│zip│zap│Zot│
├───┼───┼───┤
│   │q  │z  │
├───┼───┼───┤
│a  │b  │c  │
└───┴───┴───┘
   #&>`0: srtbl Table    NB. sort by length
┌───┬───┬───┐
│   │q  │z  │
├───┼───┼───┤
│a  │b  │c  │
├───┼───┼───┤
│zip│zap│Zot│
└───┴───┴───┘
```



## Java


Java has no optional parameters, but methods can be overloaded on the number and types of arguments, which can be used to effectively achieve optional positional parameters.


```java
import java.util.*;

public class OptionalParams {
    // "natural ordering" comparator
    static <T extends Comparable<? super T>> Comparator<T> naturalOrdering() {
        return Collections.reverseOrder(Collections.<T>reverseOrder());
    }

    public static <T extends Comparable<? super T>> void
                             sortTable(T[][] table) {
        sortTable(table, 0);
    }
    public static <T extends Comparable<? super T>> void
                             sortTable(T[][] table,
                                       int column) {
        sortTable(table, column, false);
    }
    public static <T extends Comparable<? super T>> void
                             sortTable(T[][] table,
                                       int column, boolean reverse) {
        sortTable(table, column, reverse, OptionalParams.<T>naturalOrdering());
    }
    public static <T> void sortTable(T[][] table,
                                     final int column,
                                     final boolean reverse,
                                     final Comparator<T> ordering) {
        Comparator<T[]> myCmp = new Comparator<T[]>() {
            public int compare(T[] x, T[] y) {
                return (reverse ? -1 : 1) *
                       ordering.compare(x[column], y[column]);
            }
        };
        Arrays.sort(table, myCmp);
    }

    public static void main(String[] args) {
        String[][] data0 = {{"a", "b", "c"},
                            {"", "q", "z"},
                            {"zap", "zip", "Zot"}};
        System.out.println(Arrays.deepToString(data0));
        // prints: [[a, b, c], [, q, z], [zap, zip, Zot]]

        // we copy it so that we don't change the original copy
        String[][] data = data0.clone();
        sortTable(data);
        System.out.println(Arrays.deepToString(data));
        // prints: [[, q, z], [a, b, c], [zap, zip, Zot]]

        data = data0.clone();
        sortTable(data, 2);
        System.out.println(Arrays.deepToString(data));
        // prints: [[zap, zip, Zot], [a, b, c], [, q, z]]

        data = data0.clone();
        sortTable(data, 1);
        System.out.println(Arrays.deepToString(data));
        // prints: [[a, b, c], [, q, z], [zap, zip, Zot]]

        data = data0.clone();
        sortTable(data, 1, true);
        System.out.println(Arrays.deepToString(data));
        // prints: [[zap, zip, Zot], [, q, z], [a, b, c]]

        data = data0.clone();
        sortTable(data, 0, false, new Comparator<String>() {
                public int compare(String a, String b) {
                    return b.length() - a.length();
                }
            });
        System.out.println(Arrays.deepToString(data));
        // prints: [[zap, zip, Zot], [a, b, c], [, q, z]]
    }
}
```



## JavaScript

See [[Named parameters#JavaScript]], to pass named parameters one uses an object with properties set:

```javascript
function sorter(table, options) {
    opts = {}
    opts.ordering = options.ordering || 'lexicographic';
    opts.column   = options.column || 0;
    opts.reverse  = options.reverse || false;

    // ...
}

sorter(the_data, {reverse: true, ordering: 'numeric'});
```



## jq

In jq, there are two approaches to defining optional parameters:

* Using a JSON object to define the parameters;
* Using jq's support for multiple arities (in versions of jq higher than 1.4)

The first approach gives the most flexibility, but it is sometimes
tedious to use unless augmented by the second approach.

The second approach can be used if jq > 1.4 is available and if it
acceptable to define a hierarchy of parameters, so that the first
parameter becomes required if the second is given, and so on.  The
strictures of this approach can often be alleviated by using dynamic
typing.

In this section, we will focus on the first approach as that
is supported by all versions of jq, and because it illustrates
an important point - that in jq, the value of a field (or tag) within
an object can be specified as a 0-arity filter, even though functions
are not JSON objects.

'''An aside on jq and JSON objects'''

Since jq objects are JSON objects, it might be surprising that arity-0
filters can be specified as a value within a JSON object.  This is
possible because of the way expressions such as {"function": f} are
interpreted by jq.  The key to understanding this is that jq functions
are compiled into closures. Here is an example:

```jq
def bar: 2 *.;

def foo: {"a": bar};
```

The expression <tt>3 | foo.a</tt> evaluates to 6.

'''Preliminaries''':
In accordance with the task description, we will also suppose that
sort_table(ordering; column; reverse) is already defined. To specify the lexicographic
ordering on strings in terms of an arity-0 filter, we define less_than_or_equal/0 as follows:

```jq
def less_than_or_equal: .[0] <= .[1];
```


'''The Task''':

```jq
def sorter(options):
   sort_table( if (options|has("ordering")) then options.ordering
               else less_than_or_equal
               end;
               options.column or 0;
               options.reverse or false );

# If jq > 1.4 is being used, we may also define:
def sorter: sorter({});
```

'''Examples''':

```jq
[1,2] | sorter({ "reverse": true, "ordering": less_than_or_equal } )

[1,2] | sorter({ "reverse": true })

# If sorter/0 has also been defined:
[1,2] | sorter
```



## Julia

Julia supports both named and positional optional parameters.  We can define both versions at the same time if we want:

```julia
sorttable(T; ordering=<, column=1, reverse=false) =
  sort(T, by = t -> t[column], lt = reverse ? (a,b) -> ordering(b,a) : ordering)
sorttable(T, ordering=<, column=1, reverse=false) =
  sorttable(T, ordering=ordering, column=column, reverse=reverse)
```

where the <code>;</code> in the argument list denotes the named-parameter variant, and we have used Julia's built-in higher-order <code>sort</code> function to do the work.  Note that we simply pass a comparison function for the ordering, and the built-in <code><</code> operator is actually just a function that (on strings) compares in lexicographic order.

Example output:

```julia>julia
 data = {["a", "b", "c"], ["", "q", "z"], ["zap", "zip", "Zot"]}
3-element Array{Any,1}:
 ["a","b","c"]
 ["","q","z"]
 ["zap","zip","Zot"]

julia> sorttable(data, column=2, reverse=true) # named arguments
3-element Array{Any,1}:
 ["zap","zip","Zot"]
 ["","q","z"]
 ["a","b","c"]

julia> sorttable(data, >, 2) # the same thing, with positional arguments
3-element Array{Any,1}:
 ["zap","zip","Zot"]
 ["","q","z"]
 ["a","b","c"]
```



## Kotlin


```scala
// version 1.1.51

typealias Table = List<List<String>>

/* Note that if ordering is specified, first two parameters are ignored */
fun Table.sort(
    column: Int = 0,
    reverse: Boolean = false,
    ordering: Comparator<List<String>> =
        if (!reverse) compareBy  { it[column] }
        else compareByDescending { it[column] }
) = this.sortedWith(ordering)

fun Table.print(title: String) {
    println(title)
    for (i in 0 until this.size) {
        for (j in 0 until this[0].size) System.out.print("%-3s  ".format(this[i][j]))
        println()
    }
    println()
}

fun main(args: Array<String>) {
    val table = listOf(
        listOf("a", "b", "c"),
        listOf("", "q", "z"),
        listOf("zap", "zip", "Zot")
    )
    table.print("Original:")

    val titles = listOf(
        "Sorted by col 0:", "Sorted by col 1:", "Sorted by col 2:",
        "Reverse sorted by col 0:", "Reverse sorted by col 1:", "Reverse Sorted by col 2"
    )
    val params = listOf(
        0 to false, 1 to false, 2 to false, 0 to true, 1 to true, 2 to true
    )
    for ((i, title) in titles.withIndex()) {
        val table2 = table.sort(params[i].first, params[i].second)
        table2.print(title)
    }
    // using non-default Comparator (case insensitive by col 2, reversed)
    val comp: Comparator<List<String>> = compareByDescending { it[2].toLowerCase() }
    val table3 = table.sort(ordering = comp)
    table3.print("Reverse case insensitive sort by col 2:")
}
```


```txt

Original:
a    b    c
     q    z
zap  zip  Zot

Sorted by col 0:
     q    z
a    b    c
zap  zip  Zot

Sorted by col 1:
a    b    c
     q    z
zap  zip  Zot

Sorted by col 2:
zap  zip  Zot
a    b    c
     q    z

Reverse sorted by col 0:
zap  zip  Zot
a    b    c
     q    z

Reverse sorted by col 1:
zap  zip  Zot
     q    z
a    b    c

Reverse Sorted by col 2
     q    z
a    b    c
zap  zip  Zot

Reverse case insensitive sort by col 2:
zap  zip  Zot
     q    z
a    b    c

```



## Lasso

Lasso can handle both positional and named params. Methods support multiple dispatch where each dispatch defines it's own set of parameters.

```Lasso
define sortarray( // params are set by position
	items::array, // required param
	ordering::string	= 'lexicographic', // optional param
	column::integer		= 1,
	reverse::boolean	= false
) => {
	// sorting process
	local(sorteditems = array)
	// Lasso has no build in method to sort an array of arrays by position in the contained arrays
	// But a method could be built for it
	return #sorteditems
}

define sortarray(
	-items::array, // required param
	-ordering::string	= 'lexicographic', // optional param
	-column::integer	= 1,
	-reverse::boolean	= false
) => sortarray(#items, #ordering, #column, #reverse)

local(items = array(
	array(10, 'red', 'Volvo'),
	array(15, 'gren', 'Ford'),
	array(48, 'yellow', 'Kia'),
	array(12, 'black', 'Holden'),
	array(19, 'brown', 'Fiat'),
	array(8, 'pink', 'Batmobile'),
	array(74, 'orange', 'Bicycle')
))

sortarray(-items = #items, -reverse)

sortarray(#items)
```



## TIScript


TIScript allows to define optional parameters with default values:


```javascript
function sorter(table, ordering = "lexicographic", column = 0, reverse = false) {
    // ...
}

sorter(the_data,"numeric");
```



## Logo

```logo
to sort :table [:column 1] [:ordering "before?] [:reverse "false]
  ; ...
end
```

The function "sort" has a default arity of 1 for the required parameter. When overriding default parameters, you must wrap the call in parentheses to specify the different arity.

```logo
sort :table
(sort :table 2)
(sort :table 3 "less? "true)
```



## Lua


```Lua

function showTable(tbl)
	if type(tbl)=='table' then
		local result = {}
		for _, val in pairs(tbl) do
			table.insert(result, showTable(val))
		end
		return '{' .. table.concat(result, ', ') .. '}'
	else
		return (tostring(tbl))
	end
end

function sortTable(op)
	local tbl = op.table or {}
	local column = op.column or 1
	local reverse = op.reverse or false
	local cmp = op.cmp or (function (a, b) return a < b end)
	local compareTables = function (a, b)
		local result = cmp(a[column], b[column])
		if reverse then return not result else return result end
	end
	table.sort(tbl, compareTables)
end

A = {{"quail", "deer", "snake"},
	{"dalmation", "bear", "fox"},
	{"ant", "cougar", "coyote"}}
print('original', showTable(A))

sortTable{table=A}
print('defaults', showTable(A))

sortTable{table=A, column=2}
print('col 2    ', showTable(A))

sortTable{table=A, column=3}
print('col 3    ', showTable(A))

sortTable{table=A, column=3, reverse=true}
print('col 3 rev', showTable(A))

sortTable{table=A, cmp=(function (a, b) return #a < #b end)}
print('by length', showTable(A))

```

```txt

original        {{quail, deer, snake}, {dalmation, bear, fox}, {ant, cougar, coyote}}
defaults        {{ant, cougar, coyote}, {dalmation, bear, fox}, {quail, deer, snake}}
col 2           {{dalmation, bear, fox}, {ant, cougar, coyote}, {quail, deer, snake}}
col 3           {{ant, cougar, coyote}, {dalmation, bear, fox}, {quail, deer, snake}}
col 3 rev       {{quail, deer, snake}, {dalmation, bear, fox}, {ant, cougar, coyote}}
by length       {{ant, cougar, coyote}, {quail, deer, snake}, {dalmation, bear, fox}}

```



## Maple


```Maple

OptionalSort := proc(input, {
    ordering :: Or(procedures,identical("lexicographic")) := "lexicographic",
    column :: posint := 1,
    reverse :: truefalse := false
} )
    local compare;
    if ordering = "lexicographic" then
        compare := (x,y)->evalb(`if`(reverse,x[column]>=y[column],x[column]<=y[column]));
    else
    	compare := (x,y)->`if`(reverse,ordering(x[column],y),ordering(y,x));
    end if;
    sort( input, compare );
end proc:
```


Some examples of this procedure in action:

```Maple

> L := [[1, 2], [3, 4], [-5, 7]]:
> OptionalSort(L);
                   [[-5, 7], [1, 2], [3, 4]]
> OptionalSort(L, reverse);
                   [[3, 4], [1, 2], [-5, 7]]
> OptionalSort(L, reverse, column = 2);
                   [[-5, 7], [3, 4], [1, 2]]

```



## Mathematica


```Mathematica
Options[OptionalSort]={ordering->lexicographic,column->1,reverse-> False};
OptionalSort[x_List,OptionsPattern[]]:=If[OptionValue[reverse]==True,
SortBy[x ,#[[OptionValue[column]]]&]//Reverse,
SortBy[x,#[[OptionValue[column]]]&] ]

OptionalSort[{{"a" ,"b", "c"}, {"", "q", "z"},{"zap" ,"zip", "Zot"}} ]
->{{,q,z},{a,b,c},{zap,zip,Zot}}

OptionalSort[{{"a" ,"b", "c"}, {"", "q", "z"},{"zap" ,"zip", "Zot"}},{ordering->lexicographic,column->2,reverse-> True} ]
->{{zap,zip,Zot},{,q,z},{a,b,c}}
```



## Nemerle

It's possible to use either optional parameters or overloading on parameter number (or type). However, it's less code repetition to use optional parameters when possible (unless, of course, the implementation varies drastically with different parameters).

```Nemerle
Sorter (table : list[list[string]], ordering = "lexicographic", column = 0, reverse = false) : list[list[string]]
{
    // implementation goes here
}
```



## Nim


```nim
import algorithm, strutils, future

proc printTable(a) =
  for row in a:
    for x in row: stdout.write x, repeatChar(4 - x.len)
    echo ""
  echo ""

proc sortTable(a: seq[seq[string]], column = 0, reverse = false,
    ordering: (proc(a,b: string): int) = system.cmp) : seq[seq[string]] =
  let order = if reverse: Descending else: Ascending
  result = a
  result.sort(proc(x,y:seq[string]):int = ordering(x[column],y[column]), order)

const data = @[@["a", "b", "c"], @["", "q", "z"], @["zap", "zip", "Zot"]]

printTable data
printTable sortTable(data)
printTable sortTable(data, column = 2)
printTable sortTable(data, column = 1)
printTable sortTable(data, column = 1, reverse = true)
printTable sortTable(data, ordering = (a,b) => cmp[int](b.len,a.len))
```

Output:

```txt
a   b   c
    q   z
zap zip Zot

    q   z
a   b   c
zap zip Zot

zap zip Zot
a   b   c
    q   z

a   b   c
    q   z
zap zip Zot

zap zip Zot
    q   z
a   b   c

zap zip Zot
a   b   c
    q   z
```


=={{header|Objective-C}}==
Without getting into any detail, here is one way you might implement optional arguments. (Note that since Objective-C is a strict superset of C, any C solution can be used as well.)

```objc
typedef enum { kOrdNone, kOrdLex, kOrdByAddress, kOrdNumeric } SortOrder;

@interface MyArray : NSObject {}
// . . .
@end

@implementation MyArray

- (void)sort {
    [self sortWithOrdering:kOrdLex onColumn:0 reversed:NO];
}

- (void)sortWithOrdering:(SortOrder)ord {
    [self sortWithOrdering:ord onColumn:0 reversed:NO];
}

- (void)sortWithOrdering:(SortOrder)ord onColumn:(int)col {
    [self sortWithOrdering:ord onColumn:col reversed:NO];
}

- (void)sortWithOrdering:(SortOrder)ord onColumn:(int)col reversed:(BOOL)rev {
    // . . . Actual sort goes here . . .
}

@end
```



## OCaml


OCaml has optional named parameters. It is conventional to place a non-optional parameter after the optional parameters, because if the optional parameters were at the end, then if you don't provide them, it will just look like a partial application (because OCaml supports [[currying]]), resulting in a function which still expects the optional parameters.


```ocaml
let sort_table ?(ordering = compare) ?(column = 0) ?(reverse = false) table =
  let cmp x y = ordering (List.nth x column) (List.nth y column) * (if reverse then -1 else 1) in
    List.sort cmp table
```


Example uses:

```ocaml
# let data = [["a"; "b"; "c"]; [""; "q"; "z"]; ["zap"; "zip"; "Zot"]];;
val data : string list list =
  [["a"; "b"; "c"]; [""; "q"; "z"]; ["zap"; "zip"; "Zot"]]
# sort_table data;;
- : string list list =
[[""; "q"; "z"]; ["a"; "b"; "c"]; ["zap"; "zip"; "Zot"]]
# sort_table ~column:2 data;;
- : string list list =
[["zap"; "zip"; "Zot"]; ["a"; "b"; "c"]; [""; "q"; "z"]]
# sort_table ~column:1 data;;
- : string list list =
[["a"; "b"; "c"]; [""; "q"; "z"]; ["zap"; "zip"; "Zot"]]
# sort_table ~column:1 ~reverse:true data;;
- : string list list =
[["zap"; "zip"; "Zot"]; [""; "q"; "z"]; ["a"; "b"; "c"]]
# sort_table ~ordering:(fun a b -> compare (String.length b) (String.length a)) data;;
- : string list list =
[["zap"; "zip"; "Zot"]; ["a"; "b"; "c"]; [""; "q"; "z"]]
```


OCaml does not support optional positional parameters, because, since OCaml supports currying, it would conflict with partial applications, where you do not provide all the arguments to a function, and it results in a function which expects the remaining arguments.


## Oz

Oz supports optional parameters only for methods, not for functions.

```oz
declare
  class Table
     attr
        rows

     meth init(Rows)
        rows := Rows
     end

     meth sort(ordering:O<=Lexicographic  column:C<=1  reverse:R<=false)
        fun {Predicate Row1 Row2}
           Res = {O {Nth Row1 C} {Nth Row2 C}}
        in
           if R then {Not Res} else Res end
        end
     in
        rows := {Sort @rows Predicate}
     end
  end

  fun {Lexicographic As Bs}  %% omitted for brevity
  end

  T = {New Table init([["a" "b" "c"] ["" "q" "z"] ["zap" "zip" "Zot"]])}
in
  {T sort}
  {T sort(column:3)}
  {T sort(column:2)}
  {T sort(column:2 reverse:true)}
  {T sort(ordering:fun {$ A B} {Length B} < {Length A} end)}
```



## PARI/GP

As it happens the built-in <code>vecsort()</code> function fulfills all the requirements of this task.  In general optional arguments are handled in GP by default values:

```parigp
sort(v, ordering=0, column=0, reverse=0)
```

while in PARI it is handled by checking for NULL (assuming parser code DG, see 5.7.3 in the User's Guide to the PARI library):

```C
/*
GP;install("test_func", "vDG", "test", "path/to/test.gp.so");
*/
void
test_func(GEN x) {
  if (x == NULL)
    pari_printf("Argument omitted.\n");
  else
    pari_printf("Argument was: %Ps\n", x);
}
```



## Perl

Perl 5 has no formal parameters, so all function arguments must be processed in the function body.

This function expects its first argument to be a reference to an array of arrays. It interprets any remaining arguments as a hash of optional parameters.


```perl
sub sorttable
 {my @table = @{shift()};
  my %opt =
     (ordering => sub {$_[0] cmp $_[1]}, column => 0, reverse => 0, @_);
  my $col = $opt{column};
  my $func = $opt{ordering};
  my @result = sort
      {$func->($a->[$col], $b->[$col])}
      @table;
  return ($opt{reverse} ? [reverse @result] : \@result);}
```


An example of use:


```perl
my $a = [["a", "b", "c"], ["", "q", "z"], ["zap", "zip", "Zot"]];
foreach (@{sorttable $a, column => 1, reverse => 1})
   {foreach (@$_)
       {printf "%-5s", $_;}
    print "\n";}
```



## Perl 6

Using named parameters:

```perl6
method sorttable(:$column = 0, :$reverse, :&ordering = &infix:<cmp>) {
    my @result = self»[$column].sort: &ordering;
    return $reverse ?? @result.reverse !! @result;
}
```


Using optional positional parameters:

```perl6
method sorttable-pos($column = 0, $reverse?, &ordering = &infix:<cmp>) {
    my @result = self»[$column].sort: &ordering;
    return $reverse ?? @result.reverse !! @result;
}
```



## Phix

Optional parameters are specified simply by declaring a default value. They must however be grouped on the right.


```Phix
function increment(integer i, integer inc=1)
    return i+inc
end function

?increment(5)       -- shows 6
?increment(5,2)     -- shows 7
```


You can also use a variable length sequence to emulate optional parameters.


```Phix
printf(1,"%d records sorted in %3.2s\n",{records,time()-t0})
```


In other words printf always accepts exactly three arguments, but the third should contain the correct number of
elements to match the number of format specifications in the second argument.

The following incomplete snippet from demo\pGUI\listview.exw shows the basic idea for sorting a table by any column, up or down:


```Phix
integer sortcol = 0
integer sortdir = 1

function by_column(integer i, integer j)
    return sortdir*compare(data[i][sortcol],data[j][sortcol])
end function

sequence tags = tagset(table_size) -- {1,2,..table_size}

function click_cb(Ihandle self, integer l, integer c, atom pStatus)
string sortc
...
            sortc = sprintf("SORTSIGN%d",c)
            sortdir = iff(IupGetAttribute(self,sortc)="DOWN"?-1:1)
            IupSetAttribute(self,sortc,iff(sortdir=-1?"UP":"DOWN"))
            sortcol = c
            tags = custom_sort(routine_id("by_column"),tags)

function value_cb(Ihandle /*self*/, integer l, integer c)
    l = tags[l]
    return data[l][c]
end function
```



## PicoLisp


```PicoLisp
(de sortTable (Tbl . @)
   (let (Ordering prog  Column 1  Reverse NIL)  # Set defaults
      (bind (rest)                              # Bind optional params
         (setq Tbl
            (by '((L) (Ordering (get L Column)))
               sort
               Tbl ) )
         (if Reverse (flip Tbl) Tbl) ) ) )
```

Output:

```txt
(de *Data ("a" "bcdef" "X") (" " "qrst" "z") ("zap" "zip" "Zot"))

: (sortTable *Data)
-> ((" " "qrst" "z") ("a" "bcdef" "X") ("zap" "zip" "Zot"))

: (sortTable *Data '(Reverse . T))
-> (("zap" "zip" "Zot") ("a" "bcdef" "X") (" " "qrst" "z"))

: (sortTable *Data '(Column . 2) '(Ordering . length))
-> (("zap" "zip" "Zot") (" " "qrst" "z") ("a" "bcdef" "X"))

: (sortTable *Data '(Ordering . uppc) '(Column . 3))
-> (("a" "bcdef" "X") (" " "qrst" "z") ("zap" "zip" "Zot"))
```



## Python

{{works with|Python|2.x}} only (the "cmp" argument to sorted() is no longer accepted in Python 3)

Using a pretty-printer for the table

```python>>>
 def printtable(data):
    for row in data:
        print ' '.join('%-5s' % ('"%s"' % cell) for cell in row)


>>> import operator
>>> def sorttable(table, ordering=None, column=0, reverse=False):
    return sorted(table, cmp=ordering, key=operator.itemgetter(column), reverse=reverse)

>>> data = [["a", "b", "c"], ["", "q", "z"], ["zap", "zip", "Zot"]]
>>> printtable(data)
"a"   "b"   "c"
""    "q"   "z"
"zap" "zip" "Zot"
>>> printtable( sorttable(data) )
""    "q"   "z"
"a"   "b"   "c"
"zap" "zip" "Zot"
>>> printtable( sorttable(data, column=2) )
"zap" "zip" "Zot"
"a"   "b"   "c"
""    "q"   "z"
>>> printtable( sorttable(data, column=1) )
"a"   "b"   "c"
""    "q"   "z"
"zap" "zip" "Zot"
>>> printtable( sorttable(data, column=1, reverse=True) )
"zap" "zip" "Zot"
""    "q"   "z"
"a"   "b"   "c"
>>> printtable( sorttable(data, ordering=lambda a,b: cmp(len(b),len(a))) )
"zap" "zip" "Zot"
"a"   "b"   "c"
""    "q"   "z"
>>>
```


See the Python entry in [[Named_Arguments#Python|Named Arguments]] for a more comprehensive description of Python function parameters and call arguments.

Note that expression for a default argument of an optional parameter is evaluated only once, when the function definition is executed, and all calls of the function where that parameter is missing will be initialized to point to that same shared object. So, if the default argument value is a mutable object (e.g. list, dict, etc.), then any changes to it will affect what is seen by future calls of the function:

```txt

>>> def foo(x, lst=[]):
...   lst.append(x)
...   print lst
...
>>> foo(1)
[1]
>>> foo(2)
[1, 2]
>>> foo(3)
[1, 2, 3]

```



## R

Optional parameters are given using a name=value syntax within the function header.

```R
tablesort <- function(x, ordering="lexicographic", column=1, reverse=false)
{
   # Implementation
}

# Usage is e.g.
tablesort(mytable, column=3)
```



## Racket



```racket

#lang racket

(define (sort-table table
                    [ordering string<=?]
                    [column 0]
                    [reverse? #f])
  (sort table (if reverse?
                  (negate ordering)
                  ordering)
        #:key (λ (row) (list-ref row column))))

```



## REXX

The REXX language allows for default values for positional arguments as well as an easy method to check if a string is part of a parameter.

Also allowed are named parameters.


The REXX language doesn't have any native sorting functions, so you have to write your own sorting subroutine.

```rexx
sortStrings:  procedure expose @.      /*stemmed array is named:   @.   */
col=1;    reverse='NO';    order='LEXICOGRAPHIC'    /*set some defaults.*/
arg options
               do j=1  for words(options);     x=word(options,j)

                    select
                    when datatype(x, 'W')  then col=x/1
                    when pos('=', x)==0    then order=x
                    otherwise                   parse var x nam '=' value
                    end   /*select*/
               end        /*j*/
         /*╔═══════════════════════════════════════════════════════════╗
           ║ check for errors here:  COL isn't a positive integer ···, ║
           ║                         REVERSE value isn't  NO  or  YES, ║
           ║                         ORDER   value is recognized ···   ║
           ╚═══════════════════════════════════════════════════════════╝*/
       ... main body of string sort here ...
return                                 /*stick a fork in it, we're done.*/
```

An example use is:

```rexx
/*REXX example uses the   SortStrings   subroutine  with optional args. */
                            /*···define array (@.nnn) of strings here···*/
call sortStrings  'Reverse=no'  3
                                       /*stick a fork in it, we're done.*/

```



## Ruby

Ruby allows default values for positional arguments, but they have disadvantages. In the next example, if you want to pass ''reverse=true'', you must also give values for ''ordering'' and ''column''.


```ruby
def table_sort(table, ordering=:<=>, column=0, reverse=false)
  # ...
```


Ruby 2.0 added keyword arguments to the language. These provide the most natural solution.

```ruby
def table_sort(table, ordering: :<=>, column: 0, reverse: false)
  p = ordering.to_proc
  if reverse
    table.sort {|a, b| p.(b[column], a[column])}
  else
    table.sort {|a, b| p.(a[column], b[column])}
  end
end

# Quick example:
table = [
  ["Ottowa", "Canada"],
  ["Washington", "USA"],
  ["Mexico City", "Mexico"],
]
p table_sort(table, column: 1)
```


Older versions of Ruby can fake the effect with a Hash (as detailed in [[Named parameters#Ruby]]). The next example needs Ruby 1.8.7 only because the sort code calls <code>Symbol#to_proc</code>; the passing of parameters would yet work with Ruby older than 1.8.7.

```ruby
def table_sort(table, opts = {})
  defaults = {:ordering => :<=>, :column => 0, :reverse => false}
  opts = defaults.merge(opts)

  c = opts[:column]
  p = opts[:ordering].to_proc
  if opts[:reverse]
    table.sort {|a, b| p.call(b[c], a[c])}
  else
    table.sort {|a, b| p.call(a[c], b[c])}
  end
end
```



## Rust


Rust doesn't really have optional parameters.
One way could be to use the <code>Option</code> syntax, but then you still would have to specify the optional parameters in the function calls as <code>None</code>, which would kind of defeat the purpose.
Here we use Rust's "standard way" to have optional parameters, i.e. by using builders instead.


```rust
use std::cmp::Ordering;

struct Table {
    rows: Vec<Vec<String>>,
    ordering_function: fn(&str, &str) -> Ordering,
    ordering_column: usize,
    reverse: bool,
}

impl Table {
    fn new(rows: Vec<Vec<String>>) -> Table {
        Table {
            rows: rows,
            ordering_column: 0,
            reverse: false,
            ordering_function: |str1, str2| str1.cmp(str2),
        }
    }
}

impl Table {
    fn with_ordering_column(&mut self, ordering_column: usize) -> &mut Table {
        self.ordering_column = ordering_column;
        self
    }

    fn with_reverse(&mut self, reverse: bool) -> &mut Table {
        self.reverse = reverse;
        self
    }

    fn with_ordering_fun(&mut self, compare: fn(&str, &str) -> Ordering) -> &mut Table {
        self.ordering_function = compare;
        self
    }

    fn sort(&mut self) {
        let fun = &mut self.ordering_function;
        let idx = self.ordering_column;
        if self.reverse {
            self.rows
                .sort_unstable_by(|vec1, vec2| (fun)(&vec1[idx], &vec2[idx]).reverse());
        } else {
            self.rows
                .sort_unstable_by(|vec1, vec2| (fun)(&vec1[idx], &vec2[idx]));
        }
    }
}

#[cfg(test)]
mod test {
    use super::Table;

    fn generate_test_table() -> Table {
        Table::new(vec![
            vec!["0".to_string(), "fff".to_string()],
            vec!["2".to_string(), "aab".to_string()],
            vec!["1".to_string(), "ccc".to_string()],
        ])
    }

    #[test]
    fn test_simple_sort() {
        let mut table = generate_test_table();
        table.sort();
        assert_eq!(
            table.rows,
            vec![
                vec!["0".to_string(), "fff".to_string()],
                vec!["1".to_string(), "ccc".to_string()],
                vec!["2".to_string(), "aab".to_string()],
            ],
        )
    }

    #[test]
    fn test_ordering_column() {
        let mut table = generate_test_table();
        table.with_ordering_column(1).sort();
        assert_eq!(
            table.rows,
            vec![
                vec!["2".to_string(), "aab".to_string()],
                vec!["1".to_string(), "ccc".to_string()],
                vec!["0".to_string(), "fff".to_string()],
            ],
        )
    }

    #[test]
    fn test_with_reverse() {
        let mut table = generate_test_table();
        table.with_reverse(true).sort();
        assert_eq!(
            table.rows,
            vec![
                vec!["2".to_string(), "aab".to_string()],
                vec!["1".to_string(), "ccc".to_string()],
                vec!["0".to_string(), "fff".to_string()],
            ],
        )
    }

    #[test]
    fn test_custom_ordering_fun() {
        let mut table = generate_test_table();
        // Simple ordering function that reverses stuff.
        // Should operate like the test before.
        table.with_ordering_fun(|x, y| x.cmp(y).reverse()).sort();
        assert_eq!(
            table.rows,
            vec![
                vec!["2".to_string(), "aab".to_string()],
                vec!["1".to_string(), "ccc".to_string()],
                vec!["0".to_string(), "fff".to_string()],
            ],
        )
    }

    #[test]
    fn test_everything_together() {
        let mut table = generate_test_table();
        // Using the reversing cmp function, then reverse (= don't do anything)
        // then sort from column 1.
        table
            .with_ordering_fun(|x, y| x.cmp(y).reverse())
            .with_reverse(true)
            .with_ordering_column(1)
            .sort();
        assert_eq!(
            table.rows,
            vec![
                vec!["2".to_string(), "aab".to_string()],
                vec!["1".to_string(), "ccc".to_string()],
                vec!["0".to_string(), "fff".to_string()],
            ],
        )
    }
}

```



## Scala

With Scala 2.8 optional and named parameters are build in.

```scala
  def sortTable(data: List[List[String]],
                ordering: (String, String) => Boolean = (_ < _),
                column: Int = 0,
                reverse: Boolean = false) = {
    val result = data.sortWith((a, b) => ordering(a(column), b(column)))
    if (reverse) result.reverse else result
  }
```


```scala
val data=List(List("a","b","c"), List("","q","z"), List("zap","zip","Zot"))
println(data)
   //-> List(List(a, b, c), List(, q, z), List(zap, zip, Zot))
println(sortTable(data))
   //-> List(List(, q, z), List(a, b, c), List(zap, zip, Zot))
println(sortTable(data, reverse=true))
   //-> List(List(zap, zip, Zot), List(a, b, c), List(, q, z))
println(sortTable(data, column=2))
   //-> List(List(zap, zip, Zot), List(a, b, c), List(, q, z))
println(sortTable(data, ((a, b)=> b.size<a.size)))
   //-> List(List(zap, zip, Zot), List(a, b, c), List(, q, z))
```



## Sidef


```ruby
func table_sort(table, ordering: '<=>', column: 0, reverse: false) {
  if (reverse) {
    table.sort {|a,b| b[column].$ordering(a[column])}
  } else {
    table.sort {|a,b| a[column].$ordering(b[column])}
  }
}

# Quick example:
var table = [
  ["Ottowa", "Canada"],
  ["Washington", "USA"],
  ["Mexico City", "Mexico"],
];

say table_sort(table, column: 1);
```

```txt
[["Ottowa", "Canada"], ["Mexico City", "Mexico"], ["Washington", "USA"]]
```


Missing the point, we can also create and provide a custom method for sorting to ''ordering'':

```ruby
class String {
    method my_sort(arg) {
           (self.len <=> arg.len) ->
        || (self.lc <=> arg.lc)   ->
        || (self <=> arg)
    }
}

say table_sort(table, column: 1, ordering: 'my_sort');
```

```txt
[["Washington", "USA"], ["Ottowa", "Canada"], ["Mexico City", "Mexico"]]
```



## Slate

In Slate, named optional parameters may be specified in the method signature, but not defaults, so there is a macro <tt>defaultsTo:</tt> for specifying that within the method body at run-time.

```slate
s@(Sequence traits) tableSort &column: column &sortBy: sortBlock &reverse: reverse
[
  column `defaultsTo: 0.
  sortBlock `defaultsTo: [| :a :b | (a lexicographicallyCompare: b) isNegative].
  (reverse `defaultsTo: False)
    ifTrue: [sortBlock := [| :a :b | (sortBlock applyTo: {a. b}) not]].
  s sortBy: [| :a :b | sortBlock applyTo: {a at: column. b at: column}]
].
```



## Swift


```swift
enum SortOrder { case kOrdNone, kOrdLex, kOrdByAddress, kOrdNumeric }

func sortTable(table: [[String]], less: (String,String)->Bool = (<), column: Int = 0, reversed: Bool = false) {
  // . . . Actual sort goes here . . .
}
```



## Tcl


Tcl supports optional parameters to procedures through two mechanisms. It can either work positionally (through giving default values for arguments) or by using a special last argument called “<code>args</code>” which will collect all the remaining arguments into a list that can be processed by the procedure.

The optional positional parameter style works like this:

```tcl
proc tablesort {table {ordering ""} {column 0} {reverse 0}} {
    set direction [expr {$reverse ? "-decreasing" : "-increasing"}]
    if {$ordering ne ""} {
        lsort -command $ordering $direction -index $column $table
    } else {
        lsort $direction -index $column $table
    }
}

puts [tablesort $data]
puts [tablesort $data "" 1]
puts [tablesort $data "" 0 1]
puts [tablesort $data {
    apply {{a b} {expr {[string length $a]-[string length $b]}}}
}]
```


When using the second style, it is often common to use [[Named Arguments]] (and in fact the “<code>lsort</code>” already works very much like this). Note that it is most common to use named arguments that start with a “<code>-</code>”, but we omit them here so that we formally match the requirements of the task.


```Tcl
package require Tcl 8.5;  # Only for the list expansion syntax

proc tablesort {table args} {
    array set opt {ordering "" column 0 reverse 0}
    array set opt $args
    set pars [list -index $opt(column)]
    if {$opt(reverse)} {lappend pars -decreasing}
    if {$opt(ordering) ne ""} {lappend pars -command $opt(ordering)}
    lsort {*}$pars $table
}

puts [tablesort $data]
puts [tablesort $data column 1]
puts [tablesort $data column 0]
puts [tablesort $data column 0 reverse 1]
puts [tablesort $data ordering {
    apply {{a b} {expr {[string length $b]-[string length $a]}}}
}]
```



## Unix Shell

```bash
#!/usr/bin/env bash
# sort-args.sh

data() {
  cat <<EOF
   123  456  0789
   456 0789  123
  0789  123  456
EOF
}

# sort_table [column NUM | KIND | reverse] ... <INPUT >OUTPUT
# KIND = lexicographical | numeric | human

sort_table() {
  local opts='-b'
  local column=1
  while (( $# > 0 )) ; do
    case "$1" in
      column|col|c)          column=${2?Missing column number} ; shift ;;
      lexicographical|lex|l) opts+=' -d' ;;
      numeric|num|n)         opts+=' -g' ;;
      human|hum|h)           opts+=' -h' ;;
      reverse|rev|r)         opts+=' -r' ;;
    esac
    shift
  done
  eval "sort $opts -k $column,$column -"
}

echo sort defaults          ; data | sort_table
echo sort defaults reverse  ; data | sort_table reverse
echo sort column 2          ; data | sort_table col 2
echo sort column 2 reverse  ; data | sort_table col 2 reverse
echo sort numeric           ; data | sort_table numeric
echo sort numeric reverse   ; data | sort_table numeric reverse

```

```txt

$ ./sort-args.sh
sort defaults
  0789  123  456
   123  456  0789
   456 0789  123
sort defaults reverse
   456 0789  123
   123  456  0789
  0789  123  456
sort column 2
   456 0789  123
  0789  123  456
   123  456  0789
sort column 2 reverse
   123  456  0789
  0789  123  456
   456 0789  123
sort numeric
   123  456  0789
   456 0789  123
  0789  123  456
sort numeric reverse
  0789  123  456
   456 0789  123
   123  456  0789

```



## Ursala


Because functions in Ursala take only a single argument and the usual
programming style is point free, the most natural way of affecting
named optional function parameters is to parameterize the function by
a record with computed fields. Fields in a record instance can be
associated with descriptive identifiers, listed in any order, and
omitted if their default values are intended.

For this task, a record type <code>ss</code> (for sort specification) is defined
with three fields, <code>ordering</code>, <code>column</code>, and <code>reversed</code>. The <code>ordering</code> field
contains a binary relational predicate, with the lexicographic
relation (<code>lleq</code>) being the default. The <code>column</code> field is a natural number with
a default value of 1, and the <code>reversed</code> field is a boolean with a
default value of false.
The <code>sorter</code> is actually a second order function taking a record of
this type as an argument, and returning a function built to order
that is applicable to a list of data to be sorted.


```Ursala
#import std
#import nat

ss ::

ordering   %fZ ~ordering||lleq!
column     %n  ~column||1!
reversed   %b

sorter = +^(~reversed?/~&x! ~&!,-<+ +^/~ordering ~~+ ~&h++ //skip+ predecessor+ ~column)
```

Here is a test program using the function above to sort a table
five different ways, mentioning only the information that differs from the
defaults. The table is stored as a list of lists, with one list for each
row, hence three rows and two columns.

```Ursala
example_table =

<
   <'foo','b  '>,
   <'barr','a '>,
   <'bazzz','c'>>

#cast %sLLL

examples =

<
   (sorter ss&) example_table,                      # default sorting algorithm
   (sorter ss[ordering: leql]) example_table,       # sort by field lengths but otherwise default
   (sorter ss[column: 2]) example_table,            # etc.
   (sorter ss[reversed: true]) example_table,
   (sorter ss[reversed: true,column: 2]) example_table>
```

In practice, these five functions would have been more conveniently expressed using the
built in sort operator as <code>-<&h</code>, <code>leql-<&h</code>, <code>-<&th</code>, <code>-<x&h</code>, and <code>-<x&th</code>
respectively, but this technique is useful in more complicated applications.
Here is the output showing five different sorts of the table.

```txt
<
   <<'barr','a '>,<'bazzz','c'>,<'foo','b  '>>,
   <<'foo','b  '>,<'barr','a '>,<'bazzz','c'>>,
   <<'barr','a '>,<'foo','b  '>,<'bazzz','c'>>,
   <<'foo','b  '>,<'bazzz','c'>,<'barr','a '>>,
   <<'bazzz','c'>,<'foo','b  '>,<'barr','a '>>>
```



## VBA


```vb
Private Sub optional_parameters(theRange As String, _
        Optional ordering As Integer = 0, _
        Optional column As Integer = 1, _
        Optional reverse As Integer = 1)
    ActiveSheet.Sort.SortFields.Clear
    ActiveSheet.Sort.SortFields.Add _
        Key:=Range(theRange).Columns(column), _
        SortOn:=SortOnValues, _
        Order:=reverse, _
        DataOption:=ordering 'the optional parameter ordering and above reverse
    With ActiveSheet.Sort
        .SetRange Range(theRange)
        .Header = xlGuess
        .MatchCase = False
        .Orientation = xlTopToBottom
        .SortMethod = xlPinYin
        .Apply
    End With
End Sub
Public Sub main()
    'Sort the strings in the active sheet in Excel
    'Supply the range of cells to be sorted
    'Optionally specify ordering, default is 0,
    'which is normal sort, text and data separately;
    'ordering:=1 treats text as numeric data.
    'Optionally specify column number, default is 1
    'Optionally specify reverse, default is 1
    'which sorts in ascending order.
    'Specifying reverse:=2 will sort in descending order.
    optional_parameters theRange:="A1:C4", ordering:=1, column:=2, reverse:=1
End Sub
```


## XSLT

You can give any template parameter a default value using the optional "select" attribute.

```xml
<xsl:template name="sort">
  <xsl:param name="table" />
  <xsl:param name="ordering" select="'lexicographic'" />
  <xsl:param name="column" select="1" />
  <xsl:param name="reversed" select="false()" />
  ...
</xsl:template>
```



## Yabasic


```Yabasic
sub power(n, p)
	if numparams = 1 p = 2
	return n^p
end sub

print power(2)
print power(2, 3)
```



## zkl

zkl does left to right parameters, each with an optional default. No named parameters (ala Smalltalk). There isn't type enforcement either, a parameter can be anything (although you can set a hint to tell the compiler what you think it will be). If you do want to play named parameters, you can pass in a dictionary (ala Python). The parameters are basically a [varargs] list that you can access in traditional ways.

```zkl
const lex="L";
fcn mystrySort(table,ordering=lex,column=0,reverse=False,other){
   vm.arglist.println();
}
mystrySort.prototype.println();
mystrySort("table");
mystrySort("table",lex,1);
mystrySort("table",lex,1,True,D("row",35,"type","foobar"));


```

```txt

L("table","ordering","column","reverse","other")
L("table","L",0,False)
L("table","L",1,False)
L("table","L",1,True,D(type:foobar,row:35))

```



