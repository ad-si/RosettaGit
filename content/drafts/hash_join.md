+++
title = "Hash join"
description = ""
date = 2019-10-18T00:51:06Z
aliases = []
[extra]
id = 16752
[taxonomies]
categories = []
tags = []
+++

{{task}}

An [[wp:Join_(SQL)#Inner_join|inner join]] is an operation that combines two data tables into one table, based on matching column values. The simplest way of implementing this operation is the [[wp:Nested loop join|nested loop join]] algorithm, but a more scalable alternative is the [[wp:hash join|hash join]] algorithm.

{{task heading}}

Implement the "hash join" algorithm, and demonstrate that it passes the test-case listed below.

You should represent the tables as data structures that feel natural in your programming language.

{{task heading|Guidance}}

The "hash join" algorithm consists of two steps:

# '''Hash phase:''' Create a [[wp:Multimap|multimap]] from one of the two tables, mapping from each join column value to all the rows that contain it.

#* The multimap must support hash-based lookup which scales better than a simple linear search, because that's the whole point of this algorithm.
#* Ideally we should create the multimap for the ''smaller'' table, thus minimizing its creation time and memory size.
# '''Join phase:''' Scan the other table, and find matching rows by looking in the multimap created before.



In pseudo-code, the algorithm could be expressed as follows:

 '''let''' ''A'' = the first input table (or ideally, the larger one)
 '''let''' ''B'' = the second input table (or ideally, the smaller one)
 '''let''' ''j<sub>A</sub>'' = the join column ID of table ''A''
 '''let''' ''j<sub>B</sub>'' = the join column ID of table ''B''
 '''let''' ''M<sub>B</sub>'' = a multimap for mapping from single values to multiple rows of table ''B'' (starts out empty)
 '''let''' ''C'' = the output table (starts out empty)
 
 '''for each''' row ''b'' '''in''' table ''B''''':'''
    '''place''' ''b'' '''in''' multimap ''M<sub>B</sub>'' under key ''b''(''j<sub>B</sub>'')
 
 '''for each''' row ''a'' '''in''' table ''A''''':'''
    '''for each''' row ''b'' '''in''' multimap ''M<sub>B</sub>'' under key ''a''(''j<sub>A</sub>'')''':'''
       '''let''' ''c'' = the concatenation of row ''a'' and row ''b''
       '''place''' row ''c'' in table ''C''

{{task heading|Test-case}}

{| class="wikitable"
|-
! Input
! Output
|-
|

{| style="border:none; border-collapse:collapse;"
|-
| style="border:none" | ''A'' = 
| style="border:none" |

{| class="wikitable"
|-
! Age !! Name
|-
| 27 || Jonah
|-
| 18 || Alan
|-
| 28 || Glory
|-
| 18 || Popeye
|-
| 28 || Alan
|}

| style="border:none; padding-left:1.5em;" rowspan="2" |
| style="border:none" | ''B'' = 
| style="border:none" |

{| class="wikitable"
|-
! Character !! Nemesis
|-
| Jonah || Whales
|-
| Jonah || Spiders
|-
| Alan || Ghosts
|-
| Alan || Zombies
|-
| Glory || Buffy
|}

|-
| style="border:none" | ''j<sub>A</sub>'' =
| style="border:none" | <code>Name</code> (i.e. column 1)

| style="border:none" | ''j<sub>B</sub>'' =
| style="border:none" | <code>Character</code> (i.e. column 0)
|}

|

{| class="wikitable" style="margin-left:1em"
|-
! A.Age !! A.Name !! B.Character !! B.Nemesis
|-
| 27 || Jonah || Jonah || Whales
|-
| 27 || Jonah || Jonah || Spiders
|-
| 18 || Alan || Alan || Ghosts
|-
| 18 || Alan || Alan || Zombies
|-
| 28 || Glory || Glory || Buffy
|-
| 28 || Alan || Alan || Ghosts
|-
| 28 || Alan || Alan || Zombies
|}

|}

The order of the rows in the output table is not significant.

If you're using numerically indexed arrays to represent table rows (rather than referring to columns by name), you could represent the output rows in the form <code style="white-space:nowrap">[[27, "Jonah"], ["Jonah", "Whales"]]</code>.


<hr>


## AppleScript

{{Trans|JavaScript}}

Native AppleScript records lack introspection, but from Yosemite onwards we can read and write them a little more flexibly through the Foundation classes. 
The vertical bars distinguish AppleScript reserved words ('''name''' and '''character''' here) from field name literal strings.


```AppleScript
use framework "Foundation" -- Yosemite onwards, for record-handling functions

-- HASH JOIN -----------------------------------------------------------------

-- hashJoin :: [Record] -> [Record] -> String -> [Record]
on hashJoin(tblA, tblB, strJoin)
    set {jA, jB} to splitOn("=", strJoin)
    
    script instanceOfjB
        on |λ|(a, x)
            set strID to keyValue(x, jB)
            
            set maybeInstances to keyValue(a, strID)
            if maybeInstances is not missing value then
                updatedRecord(a, strID, maybeInstances & {x})
            else
                updatedRecord(a, strID, [x])
            end if
        end |λ|
    end script
    
    set M to foldl(instanceOfjB, {name:"multiMap"}, tblB)
    
    script joins
        on |λ|(a, x)
            set matches to keyValue(M, keyValue(x, jA))
            if matches is not missing value then
                script concat
                    on |λ|(row)
                        x & row
                    end |λ|
                end script
                
                a & map(concat, matches)
            else
                a
            end if
        end |λ|
    end script
    
    foldl(joins, {}, tblA)
end hashJoin

-- TEST ----------------------------------------------------------------------
on run
    set lstA to [¬
        {age:27, |name|:"Jonah"}, ¬
        {age:18, |name|:"Alan"}, ¬
        {age:28, |name|:"Glory"}, ¬
        {age:18, |name|:"Popeye"}, ¬
        {age:28, |name|:"Alan"}]
    
    set lstB to [¬
        {|character|:"Jonah", nemesis:"Whales"}, ¬
        {|character|:"Jonah", nemesis:"Spiders"}, ¬
        {|character|:"Alan", nemesis:"Ghosts"}, ¬
        {|character|:"Alan", nemesis:"Zombies"}, ¬
        {|character|:"Glory", nemesis:"Buffy"}, ¬
        {|character|:"Bob", nemesis:"foo"}]
    
    hashJoin(lstA, lstB, "name=character")
end run


-- RECORD FUNCTIONS ----------------------------------------------------------

-- keyValue :: String -> Record -> Maybe a
on keyValue(rec, strKey)
    set ca to current application
    set v to (ca's NSDictionary's dictionaryWithDictionary:rec)'s ¬
        objectForKey:strKey
    if v is not missing value then
        item 1 of ((ca's NSArray's arrayWithObject:v) as list)
    else
        missing value
    end if
end keyValue

-- updatedRecord :: Record -> String -> a -> Record
on updatedRecord(rec, strKey, varValue)
    set ca to current application
    set nsDct to (ca's NSMutableDictionary's dictionaryWithDictionary:rec)
    nsDct's setValue:varValue forKey:strKey
    item 1 of ((ca's NSArray's arrayWithObject:nsDct) as list)
end updatedRecord


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- splitOn :: Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set lstParts to text items of strMain
    set my text item delimiters to dlm
    return lstParts
end splitOn
```

{{Out}}

```txt
{{age:27, |name|:"Jonah", |character|:"Jonah", nemesis:"Whales"}, 
{age:27, |name|:"Jonah", |character|:"Jonah", nemesis:"Spiders"}, 
{age:18, |name|:"Alan", |character|:"Alan", nemesis:"Ghosts"}, 
{age:18, |name|:"Alan", |character|:"Alan", nemesis:"Zombies"}, 
{age:28, |name|:"Glory", |character|:"Glory", nemesis:"Buffy"}, 
{age:28, |name|:"Alan", |character|:"Alan", nemesis:"Ghosts"}, 
{age:28, |name|:"Alan", |character|:"Alan", nemesis:"Zombies"}}
```



## AWK


```AWK

# syntax: GAWK -f HASH_JOIN.AWK [-v debug={0|1}] TABLE_A TABLE_B
#
# sorting:
#   PROCINFO["sorted_in"] is used by GAWK
#   SORTTYPE is used by Thompson Automation's TAWK
#
BEGIN {
    FS = ","
    PROCINFO["sorted_in"] = "@ind_str_asc" ; SORTTYPE = 1
    if (ARGC-1 != 2) {
      print("error: incorrect number of arguments") ; errors++
      exit # go to END
    }
}
{   if (NR == FNR) { # table A
      if (FNR == 1) {
        a_head = prefix_column_names("A")
        next
      }
      a_arr[$2][$1] = $0 # [name][age]
    }
    if (NR != FNR) { # table B
      if (FNR == 1) {
        b_head = prefix_column_names("B")
        next
      }
      b_arr[$1][$2] = $0 # [character][nemesis]
    }
}
END {
    if (errors > 0) { exit(1) }
    if (debug == 1) {
      dump_table(a_arr,a_head)
      dump_table(b_arr,b_head)
    }
    printf("%s%s%s\n",a_head,FS,b_head) # table heading
    for (i in a_arr) {
      if (i in b_arr) {
        for (j in a_arr[i]) {
          for (k in b_arr[i]) {
            print(a_arr[i][j] FS b_arr[i][k]) # join table A & table B
          }
        }
      }
    }
    exit(0)
}
function dump_table(arr,heading,  i,j) {
    printf("%s\n",heading)
    for (i in arr) {
      for (j in arr[i]) {
        printf("%s\n",arr[i][j])
      }
    }
    print("")
}
function prefix_column_names(p,  tmp) {
    tmp = p "." $0
    gsub(/,/,"&" p ".",tmp)
    return(tmp)
}

```

<p>TABLE_A input:</p>

```txt

Age,Name
27,Jonah
18,Alan
28,Glory
18,Popeye
28,Alan

```

<p>TABLE_B input:</p>

```txt

Character,Nemesis
Jonah,Whales
Jonah,Spiders
Alan,Ghosts
Alan,Zombies
Glory,Buffy

```

{{out}}

```txt

A.Age,A.Name,B.Character,B.Nemesis
18,Alan,Alan,Ghosts
18,Alan,Alan,Zombies
28,Alan,Alan,Ghosts
28,Alan,Alan,Zombies
28,Glory,Glory,Buffy
27,Jonah,Jonah,Spiders
27,Jonah,Jonah,Whales

```



## Bracmat

This solution creates a hash table for the smaller relation in the function <code>join</code>. This function takes as arguments the smallest table, the biggest table and then three pieces of code: two patterns that describe each table's field order and code that generates one row of output. These pieces of code are inserted in a fixed skeleton of code using macro substitution.

```bracmat
(     (27.Jonah)
      (18.Alan)
      (28.Glory)
      (18.Popeye)
      (28.Alan)
  : ?table-A
&     (Jonah.Whales)
      (Jonah.Spiders)
      (Alan.Ghosts)
      (Alan.Zombies)
      (Glory.Buffy)
  : ?table-B
& new$hash:?H
& !table-A:? [?lenA
& !table-B:? [?lenB
& ( join
  =     smalltab bigtab smallschema bigschema joinschema
      , key val val2 keyval2
    .     !arg
        : (?smalltab.?bigtab.(=?smallschema.?bigschema.?joinschema))
      & :?rel
      & !(
         ' (   whl
             ' ( !smalltab:$smallschema ?smalltab
               & (H..insert)$(!key.!val)
               )
           &   whl
             ' ( !bigtab:$bigschema ?bigtab
               & (   (H..find)$!key:?keyval2
                   &   whl
                     ' ( !keyval2:(?key.?val2) ?keyval2
                       & $joinschema !rel:?rel
                       )
                 |
                 )
               )
           )
         )
      & !rel
  )
&   out
  $ ( join
    $ (   !lenA:~<!lenB
        & ( !table-B
          . !table-A
          . (
            = (?key.?val).(?val.?key).!val.!key.!val2
            )
          )
      | ( !table-A
        . !table-B
        . (=(?val.?key).(?key.?val).!val2.!key.!val)
        )
      )
    )
&
);
```

Output:

```txt
  (28.Alan.Ghosts)
  (28.Alan.Zombies)
  (28.Glory.Buffy)
  (18.Alan.Ghosts)
  (18.Alan.Zombies)
  (27.Jonah.Whales)
  (27.Jonah.Spiders)
```



## C++


```cpp>#include <iostream

#include <string>
#include <vector>
#include <unordered_map>

using tab_t = std::vector<std::vector<std::string>>;
tab_t tab1 {
// Age  Name
  {"27", "Jonah"}
, {"18", "Alan"}
, {"28", "Glory"}
, {"18", "Popeye"}
, {"28", "Alan"}
};

tab_t tab2 {
// Character  Nemesis
  {"Jonah", "Whales"}
, {"Jonah", "Spiders"}
, {"Alan", "Ghosts"}
, {"Alan", "Zombies"}
, {"Glory", "Buffy"}
};

std::ostream& operator<<(std::ostream& o, const tab_t& t) {
  for(size_t i = 0; i < t.size(); ++i) {
    o << i << ":";
    for(const auto& e : t[i]) 
      o << '\t' << e;
    o << std::endl;
  }
  return o;
}

tab_t Join(const tab_t& a, size_t columna, const tab_t& b, size_t columnb) {
  std::unordered_multimap<std::string, size_t> hashmap;
  // hash
  for(size_t i = 0; i < a.size(); ++i) {
    hashmap.insert(std::make_pair(a[i][columna], i));
  }
  // map
  tab_t result;
  for(size_t i = 0; i < b.size(); ++i) {
    auto range = hashmap.equal_range(b[i][columnb]);
    for(auto it = range.first; it != range.second; ++it) {
      tab_t::value_type row;
      row.insert(row.end() , a[it->second].begin() , a[it->second].end());
      row.insert(row.end() , b[i].begin()          , b[i].end());
      result.push_back(std::move(row));
    }
  }
  return result;
}

int main(int argc, char const *argv[])
{
  using namespace std;
  int ret = 0;
  cout << "Table A: "       << endl << tab1 << endl;
  cout << "Table B: "       << endl << tab2 << endl;
  auto tab3 = Join(tab1, 1, tab2, 0);
  cout << "Joined tables: " << endl << tab3 << endl;
  return ret;
}


```

{{out}}

```txt
Table A: 
0:  27  Jonah
1:  18  Alan
2:  28  Glory
3:  18  Popeye
4:  28  Alan

Table B: 
0:  Jonah Whales
1:  Jonah Spiders
2:  Alan  Ghosts
3:  Alan  Zombies
4:  Glory Buffy

Joined tables: 
0:  27  Jonah Jonah Whales
1:  27  Jonah Jonah Spiders
2:  28  Alan  Alan  Ghosts
3:  18  Alan  Alan  Ghosts
4:  28  Alan  Alan  Zombies
5:  18  Alan  Alan  Zombies
6:  28  Glory Glory Buffy

```




## C sharp

;using LINQ to Objects

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace HashJoin
{
    public class AgeName
    {
        public AgeName(byte age, string name)
        {
            Age = age;
            Name = name;
        }
        public byte Age { get; private set; }
        public string Name { get; private set; }
    }

    public class NameNemesis
    {
        public NameNemesis(string name, string nemesis)
        {
            Name = name;
            Nemesis = nemesis;
        }
        public string Name { get; private set; }
        public string Nemesis { get; private set; }
    }

    public class DataContext
    {
        public DataContext()
        {
            AgeName = new List<AgeName>();
            NameNemesis = new List<NameNemesis>();
        }
        public List<AgeName> AgeName { get; set; }
        public List<NameNemesis> NameNemesis { get; set; }
    }

    public class AgeNameNemesis
    {
        public AgeNameNemesis(byte age, string name, string nemesis)
        {
            Age = age;
            Name = name;
            Nemesis = nemesis;
        }
        public byte Age { get; private set; }
        public string Name { get; private set; }
        public string Nemesis { get; private set; }
    }

    class Program
    {
        public static void Main()
        {
            var data = GetData();
            var result = ExecuteHashJoin(data);
            WriteResultToConsole(result);
        }

        private static void WriteResultToConsole(List<AgeNameNemesis> result)
        {
            result.ForEach(ageNameNemesis => Console.WriteLine("Age: {0}, Name: {1}, Nemesis: {2}",
                ageNameNemesis.Age, ageNameNemesis.Name, ageNameNemesis.Nemesis));
        }

        private static List<AgeNameNemesis> ExecuteHashJoin(DataContext data)
        {
            return (data.AgeName.Join(data.NameNemesis, 
                ageName => ageName.Name, nameNemesis => nameNemesis.Name,
                (ageName, nameNemesis) => new AgeNameNemesis(ageName.Age, ageName.Name, nameNemesis.Nemesis)))
                .ToList();
        }

        private static DataContext GetData()
        {
            var context = new DataContext();

            context.AgeName.AddRange(new [] {
                    new AgeName(27, "Jonah"), 
                    new AgeName(18, "Alan"), 
                    new AgeName(28, "Glory"), 
                    new AgeName(18, "Popeye"), 
                    new AgeName(28, "Alan")
                });

            context.NameNemesis.AddRange(new[]
            {
                new NameNemesis("Jonah", "Whales"),
                new NameNemesis("Jonah", "Spiders"),
                new NameNemesis("Alan", "Ghosts"),
                new NameNemesis("Alan", "Zombies"),
                new NameNemesis("Glory", "Buffy")
            });

            return context;
        }
    }
}
```


{{out}}

```txt

Age: 27, Name: Jonah, Nemesis: Whales
Age: 27, Name: Jonah, Nemesis: Spiders
Age: 18, Name: Alan, Nemesis: Ghosts
Age: 18, Name: Alan, Nemesis: Zombies
Age: 28, Name: Glory, Nemesis: Buffy
Age: 28, Name: Alan, Nemesis: Ghosts
Age: 28, Name: Alan, Nemesis: Zombies

```



## Clojure


```clojure

(defn hash-join [table1 col1 table2 col2]
  (let [hashed (group-by col1 table1)]
    (flatten
      (for [r table2]
        (for [s (hashed (col2 r))]
          (merge s r))))))

(def s '({:age 27 :name "Jonah"}
         {:age 18 :name "Alan"}
         {:age 28 :name "Glory"}
         {:age 18 :name "Popeye"}
         {:age 28 :name "Alan"}))

(def r '({:nemesis "Whales" :name "Jonah"}
         {:nemesis "Spiders" :name "Jonah"}
         {:nemesis "Ghosts" :name "Alan"}
         {:nemesis "Zombies" :name "Alan"}
         {:nemesis "Buffy" :name "Glory"}))

(pprint (sort-by :name (hash-join s :name r :name)))

```


{{out}}


```txt

({:nemesis "Ghosts", :age 18, :name "Alan"}
 {:nemesis "Ghosts", :age 28, :name "Alan"}
 {:nemesis "Zombies", :age 18, :name "Alan"}
 {:nemesis "Zombies", :age 28, :name "Alan"}
 {:nemesis "Buffy", :age 28, :name "Glory"}
 {:nemesis "Whales", :age 27, :name "Jonah"}
 {:nemesis "Spiders", :age 27, :name "Jonah"})

```



## Common Lisp


```lisp
(defparameter *table-A* '((27 "Jonah") (18 "Alan") (28 "Glory") (18 "Popeye") (28 "Alan")))

(defparameter *table-B* '(("Jonah" "Whales") ("Jonah" "Spiders") ("Alan" "Ghosts") ("Alan" "Zombies") ("Glory" "Buffy")))

;; Hash phase
(defparameter *hash-table* (make-hash-table :test #'equal))

(loop for (i r) in *table-A* 
   for value = (gethash r *hash-table* (list nil))  do
   (setf (gethash r *hash-table*) value)
   (push (list i r) (first value)))

;; Join phase     
(loop for (i r) in *table-B* do
     (let ((val (car (gethash i *hash-table*))))
       (loop for (a b) in val do 
	    (format t "{~a ~a} {~a ~a}~%"  a b i r))))
```


{{out}}

```txt

{27 Jonah} {Jonah Whales}
{27 Jonah} {Jonah Spiders}
{28 Alan} {Alan Ghosts}
{18 Alan} {Alan Ghosts}
{28 Alan} {Alan Zombies}
{18 Alan} {Alan Zombies}
{28 Glory} {Glory Buffy}

```



## D

{{trans|Python}}

```d
import std.stdio, std.typecons;

auto hashJoin(size_t index1, size_t index2, T1, T2)
             (in T1[] table1, in T2[] table2) pure /*nothrow*/ @safe
if (is(typeof(T1.init[index1]) == typeof(T2.init[index2]))) {
    // Hash phase.
    T1[][typeof(T1.init[index1])] h;
    foreach (const s; table1)
        h[s[index1]] ~= s;

    // Join phase.
    Tuple!(const T1, const T2)[] result;
    foreach (const r; table2)
        foreach (const s; h.get(r[index2], null)) // Not nothrow.
            result ~= tuple(s, r);

    return result;
}

void main() {
    alias T = tuple;
    immutable table1 = [T(27, "Jonah"),
                        T(18, "Alan"),
                        T(28, "Glory"),
                        T(18, "Popeye"),
                        T(28, "Alan")];
    immutable table2 = [T("Jonah", "Whales"),
                        T("Jonah", "Spiders"),
                        T("Alan",  "Ghosts"),
                        T("Alan",  "Zombies"),
                        T("Glory", "Buffy")];

    foreach (const row; hashJoin!(1, 0)(table1, table2))
        writefln("(%s, %5s) (%5s, %7s)", row[0][], row[1][]);
}
```

{{out}}

```txt
(27, Jonah) (Jonah,  Whales)
(27, Jonah) (Jonah, Spiders)
(18,  Alan) ( Alan,  Ghosts)
(28,  Alan) ( Alan,  Ghosts)
(18,  Alan) ( Alan, Zombies)
(28,  Alan) ( Alan, Zombies)
(28, Glory) (Glory,   Buffy)
```


=={{header|Déjà Vu}}==
{{trans|Python}}

```dejavu
hashJoin table1 index1 table2 index2:
    local :h {}
    # hash phase
    for s in table1:
        local :key s! index1
        if not has h key:
            set-to h key []
        push-to h! key s
    # join phase
    []
    for r in table2:
        for s in copy h! r! index2:
            push-through swap [ s r ]

local :table1 [ [ 27 "Jonah" ] [ 18 "Alan" ] [ 28 "Glory" ] [ 18 "Popeye" ] [ 28 "Alan" ] ]
local :table2 [ [ "Jonah" "Whales" ] [ "Jonah" "Spiders" ] [ "Alan" "Ghosts" ] [ "Alan" "Zombies" ] [ "Glory" "Buffy" ] ]

for row in hashJoin table1 1 table2 0:
    !. row
```

{{out}}

```txt
[ [ 27 "Jonah" ] [ "Jonah" "Whales" ] ]
[ [ 27 "Jonah" ] [ "Jonah" "Spiders" ] ]
[ [ 28 "Alan" ] [ "Alan" "Ghosts" ] ]
[ [ 18 "Alan" ] [ "Alan" "Ghosts" ] ]
[ [ 28 "Alan" ] [ "Alan" "Zombies" ] ]
[ [ 18 "Alan" ] [ "Alan" "Zombies" ] ]
[ [ 28 "Glory" ] [ "Glory" "Buffy" ] ]

```



## EchoLisp

Since this is a real, professional application, we build the hash tables in permanent (local) storage.

```lisp

(define ages '((27 "Jonah") (18 "Alan") (28 "Glory") (18 "Popeye") (28 "Alan")))
(define nemesis '(("Jonah" "Whales") ("Jonah" "Spiders") ("Alan" "Ghosts") ("Alan" "Zombies") ("Glory" "Buffy")))

;; table: table name
;; source : input list
;; key-proc : procedure returning the join value ('name' in this task)

(define (table-hash table source key-proc )
(local-make-store table)
(for ((r source))
	(local-put-value 
		(key-proc r) 
		(append (list r) (local-get-value (key-proc r) table)) table)))

;; build the two tables
(define-syntax-rule (second record) (cadr record))
(define (key-name-age record) (second record))
(table-hash 'AGES ages key-name-age)

(define (key-nemesis-name record) (first record))
(table-hash 'NEMESIS nemesis key-nemesis-name)

;; join
(for* ((k (local-keys 'AGES)) 
	  (a (local-get-value k 'AGES))
	  (n (local-get-value k 'NEMESIS)))
	  (writeln a n))

```

{{out}}

```lisp

(28 "Alan")     ("Alan" "Zombies")    
(28 "Alan")     ("Alan" "Ghosts")    
(18 "Alan")     ("Alan" "Zombies")    
(18 "Alan")     ("Alan" "Ghosts")    
(28 "Glory")     ("Glory" "Buffy")    
(27 "Jonah")     ("Jonah" "Spiders")    
(27 "Jonah")     ("Jonah" "Whales")  

```



## ECL


```ECL

LeftRec := RECORD
  UNSIGNED1 Age;
  STRING6   Name;
END;

LeftFile := DATASET([{27,'Jonah'},{18,'Alan'},{28,'Glory'},{18,'Popeye'},{28,'Alan'}],LeftRec);

RightRec := RECORD
  STRING6   Name;
  STRING7   Nemesis;
END;
	
RightFile := DATASET([{'Jonah','Whales'},{'Jonah','Spiders'},{'Alan','Ghosts'},{'Alan','Zombies'},{'Glory','Buffy'}],
                     RightRec);
										 
HashJoin := JOIN(LeftFile,RightFile,Left.Name = RIGHT.Name,HASH);

HashJoin;


//The HASH JOIN is built-in to the ECL JOIN by using the HASH JOIN Flag

/*
OUTPUT:
Age Name  Nemesis
18  Alan  Ghosts 
18  Alan  Zombies
28  Alan  Ghosts 
28  Alan  Zombies
28  Glory Buffy  
27  Jonah Whales 
27  Jonah Spiders
*/

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Hash do
  def join(table1, index1, table2, index2) do
    h = Enum.group_by(table1, fn s -> elem(s, index1) end)
    Enum.flat_map(table2, fn r ->
      Enum.map(h[elem(r, index2)], fn s -> {s, r} end)
    end)
  end
end

table1 = [{27, "Jonah"},
          {18, "Alan"},
          {28, "Glory"},
          {18, "Popeye"},
          {28, "Alan"}]
table2 = [{"Jonah", "Whales"},
          {"Jonah", "Spiders"},
          {"Alan",  "Ghosts"},
          {"Alan",  "Zombies"},
          {"Glory", "Buffy"}]
Hash.join(table1, 1, table2, 0) |> Enum.each(&IO.inspect &1)
```


{{out}}

```txt

{{27, "Jonah"}, {"Jonah", "Whales"}}
{{27, "Jonah"}, {"Jonah", "Spiders"}}
{{28, "Alan"}, {"Alan", "Ghosts"}}
{{18, "Alan"}, {"Alan", "Ghosts"}}
{{28, "Alan"}, {"Alan", "Zombies"}}
{{18, "Alan"}, {"Alan", "Zombies"}}
{{28, "Glory"}, {"Glory", "Buffy"}}

```



## Erlang


```Erlang

-module( hash_join ).

-export( [task/0] ).

task() ->
    Table_1 = [{27, "Jonah"}, {18, "Alan"}, {28, "Glory"}, {18, "Popeye"}, {28, "Alan"}],
    Table_2 = [{"Jonah", "Whales"}, {"Jonah", "Spiders"}, {"Alan", "Ghosts"}, {"Alan", "Zombies"}, {"Glory", "Buffy"}],
    Dict = lists:foldl( fun dict_append/2, dict:new(), Table_1 ),
    lists:flatten( [dict_find( X, Dict ) || X <- Table_2] ).


dict_append( {Key, Value}, Acc ) -> dict:append( Value, {Key, Value}, Acc ).

dict_find( {Key, Value}, Dict ) -> dict_find( dict:find(Key, Dict), Key, Value ).

dict_find( error, _Key, _Value ) -> [];
dict_find( {ok,	Values}, Key, Value ) -> [{X, {Key, Value}} || X <- Values].

```

{{out}}

```txt

15> hash_join:task().
[{{27,"Jonah"},{"Jonah","Whales"}},
 {{27,"Jonah"},{"Jonah","Spiders"}},
 {{18,"Alan"},{"Alan","Ghosts"}},
 {{28,"Alan"},{"Alan","Ghosts"}},
 {{18,"Alan"},{"Alan","Zombies"}},
 {{28,"Alan"},{"Alan","Zombies"}},
 {{28,"Glory"},{"Glory","Buffy"}}]

```


=={{header|F_Sharp|F#}}==

```fsharp>[<EntryPoint
]
let main argv =
    let table1 = [27, "Jonah";
                18, "Alan";
                28, "Glory";
                18, "Popeye";
                28, "Alan"]
    let table2 = ["Jonah", "Whales";
                "Jonah", "Spiders";
                "Alan", "Ghosts";
                "Alan", "Zombies";
                "Glory", "Buffy"]
    let hash = Seq.groupBy (fun r -> snd r) table1
    table2 
    |> Seq.collect (fun r -> 
        hash
        |> Seq.collect (fun kv ->
            if (fst r) <> (fst kv) then []
            else (Seq.map (fun x -> (x, r)) (snd kv)) |> Seq.toList)
        )
    |> Seq.toList
    |> printfn "%A"
    0
```

{{out}}

```txt
[((27, "Jonah"), ("Jonah", "Whales")); ((27, "Jonah"), ("Jonah", "Spiders"));
 ((18, "Alan"), ("Alan", "Ghosts")); ((28, "Alan"), ("Alan", "Ghosts"));
 ((18, "Alan"), ("Alan", "Zombies")); ((28, "Alan"), ("Alan", "Zombies"));
 ((28, "Glory"), ("Glory", "Buffy"))]
```



## Forth

{{works with|Forth}}
Works with any ANS Forth

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth

include FMS-SI.f
include FMS-SILib.f

\ Since the same join attribute, Name, occurs more than once
\ in both tables for this problem we need a hash table that
\ will accept and retrieve multiple identical keys if we want
\ an efficient solution for large tables. We make use
\ of the hash collision handling feature of class hash-table.

\ Subclass hash-table-m allows multiple entries with the same key.
\ After a get: hit one can inspect for additional entries with
\ the same key by using next: until false is returned.

:class hash-table-m <super hash-table

\ called within insert: method in superclass
 :m (do-search): ( node hash -- idx hash false )
      swap drop idx @ swap false ;m
 :m next: ( -- val true | false )
    last-node @ dup
    if
      begin
       ( node ) next: dup
      while
        dup key@: @: key-addr @ key-len @ compare 0=
             if dup last-node ! val@: true exit then
      repeat 
    then ;m
;class

\ begin hash phase 
: obj ( addr len -- obj )
  heap> string+ dup >r !: r> ;

hash-table-m R   1 r init 
s" Whales "   obj s" Jonah" r insert:
s" Spiders "  obj s" Jonah" r insert:
s" Ghosts "   obj s" Alan"  r insert:
s" Buffy "    obj s" Glory" r insert: 
s" Zombies "  obj s" Alan"  r insert:
s" Vampires " obj s" Jonah" r insert:
\ end hash phase

\ create Age Name table S
o{ o{ 27 'Jonah' }
   o{ 18 'Alan' }
   o{ 28 'Glory' }
   o{ 18 'Popeye' }
   o{ 28 'Alan' } } value s  

\ Q is a place to store the relation
object-list2 Q

\ join phase
: join \ { obj | list -- }
  0 locals| list obj |
  1 obj at: @: r get: \ hash the join-attribute and search table r
  if \ we have a match, so concatenate and save in q
    heap> object-list2 to list list q add: \ start a new sub-list in q
    0 obj at: copy: list add: \ place age from list s in q
    1 obj at: copy: list add: \ place join-attribute (name) from list s in q
    ( str-obj ) copy: list add: \ place first nemesis in q
    begin
      r next: \ check for more nemeses
    while
       ( str-obj ) copy: list add: \ place next nemesis in q
    repeat
  then ;

: probe
  begin
    s each: \ for each tuple object in s
  while
    ( obj ) join \ pass the object to function join
  repeat ;
 
probe \ execute the probe function

q p: \ print the saved relation

\ free allocated memory
s <free 
r free2:
q free:


```

{{out}}

```txt

o{ 
o{ 27 'Jonah' Whales Spiders Vampires } 
o{ 18 'Alan' Ghosts Zombies } 
o{ 28 'Glory' Buffy } 
o{ 28 'Alan' Ghosts Zombies } } 

```



## Go


```go
package main

import "fmt"

func main() {
    tableA := []struct {
        value int
        key   string
    }{
        {27, "Jonah"}, {18, "Alan"}, {28, "Glory"}, {18, "Popeye"},
        {28, "Alan"},
    }
    tableB := []struct {
        key   string
        value string
    }{
        {"Jonah", "Whales"}, {"Jonah", "Spiders"},
        {"Alan", "Ghosts"}, {"Alan", "Zombies"}, {"Glory", "Buffy"},
    }
    // hash phase
    h := map[string][]int{}
    for i, r := range tableA {
        h[r.key] = append(h[r.key], i)
    }
    // join phase
    for _, x := range tableB {
        for _, a := range h[x.key] {
            fmt.Println(tableA[a], x)
        }
    }
}
```

{{out}}

```txt

{27 Jonah} {Jonah Whales}
{27 Jonah} {Jonah Spiders}
{18 Alan} {Alan Ghosts}
{28 Alan} {Alan Ghosts}
{18 Alan} {Alan Zombies}
{28 Alan} {Alan Zombies}
{28 Glory} {Glory Buffy}

```



## Groovy


Semi-imperative style:

```Groovy

def hashJoin(table1, col1, table2, col2) {

    def hashed = table1.groupBy { s -> s[col1] }

    def q = [] as Set

    table2.each { r ->
        def join = hashed[r[col2]]
        join.each { s ->
            q << s.plus(r)
        }
    }

    q
}

```


More functional style:

```Groovy

def hashJoin(table1, col1, table2, col2) {

    def hashed = table1.groupBy { s -> s[col1] }

    table2.collect { r ->
        hashed[r[col2]].collect { s -> s.plus(r) }
    }.flatten()
}

```


Sample run (either version as the result is the same):

```Groovy

def s = [[age: 27, name: 'Jonah'],
         [age: 18, name: 'Alan'],
         [age: 28, name: 'Glory'],
         [age: 18, name: 'Popeye'],
         [age: 28, name: 'Alan']]

def r = [[name: 'Jonah', nemesis: 'Whales'],
         [name: 'Jonah', nemesis: 'Spiders'],
         [name: 'Alan', nemesis: 'Ghosts'],
         [name: 'Alan', nemesis: 'Zombies'],
         [name: 'Glory', nemesis: 'Buffy']]

hashJoin(s, "name", r, "name").sort {it.name}.each { println it }

```


produces:


```txt

[age:18, name:Alan, nemesis:Ghosts]
[age:28, name:Alan, nemesis:Ghosts]
[age:18, name:Alan, nemesis:Zombies]
[age:28, name:Alan, nemesis:Zombies]
[age:28, name:Glory, nemesis:Buffy]
[age:27, name:Jonah, nemesis:Whales]
[age:27, name:Jonah, nemesis:Spiders]

```



## Haskell

The ST monad allows us to utilise mutable memory behind a referentially transparent interface,
allowing us to use hashtables (efficiently).

Our hashJoin function takes two lists and two selector functions.

Placing all relations with the same selector value in a list in the hashtable allows us to join many to one/many relations.

```Haskell
{-# LANGUAGE LambdaCase, TupleSections #-}
import qualified Data.HashTable.ST.Basic as H
import Data.Hashable
import Control.Monad.ST
import Control.Monad
import Data.STRef

hashJoin :: (Eq k, Hashable k) =>
            [t] -> (t -> k) -> [a] -> (a -> k) -> [(t, a)]
hashJoin xs fx ys fy = runST $ do
  l <- newSTRef []
  ht <- H.new
  forM_ ys $ \y -> H.insert ht (fy y) =<< 
    (H.lookup ht (fy y) >>= \case
      Nothing -> return [y]
      Just v -> return (y:v))
  forM_ xs $ \x -> do
    H.lookup ht (fx x) >>= \case
      Nothing -> return ()
      Just v -> modifySTRef' l ((map (x,)  v) ++) 
  readSTRef l

main = mapM_ print $ hashJoin 
    [(1, "Jonah"), (2, "Alan"), (3, "Glory"), (4, "Popeye")]
        snd
    [("Jonah", "Whales"), ("Jonah", "Spiders"), 
      ("Alan", "Ghosts"), ("Alan", "Zombies"), ("Glory", "Buffy")]
        fst

```


```txt

((3,"Glory"),("Glory","Buffy"))
((2,"Alan"),("Alan","Zombies"))
((2,"Alan"),("Alan","Ghosts"))
((1,"Jonah"),("Jonah","Spiders"))
((1,"Jonah"),("Jonah","Whales"))

```


The task require hashtables; however, a cleaner and more functional solution would be to use Data.Map (based on binary trees):

```Haskell
{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Applicative

mapJoin xs fx ys fy = joined
  where yMap = foldl' f M.empty ys
        f m y = M.insertWith (++) (fy y) [y] m
        joined = concat .
                 mapMaybe (\x -> map (x,) <$> M.lookup (fx x) yMap) $ xs

main = mapM_ print $ mapJoin
    [(1, "Jonah"), (2, "Alan"), (3, "Glory"), (4, "Popeye")]
        snd
    [("Jonah", "Whales"), ("Jonah", "Spiders"), 
     ("Alan", "Ghosts"), ("Alan", "Zombies"), ("Glory", "Buffy")]
        fst

```


```txt

((1,"Jonah"),("Jonah","Spiders"))
((1,"Jonah"),("Jonah","Whales"))
((2,"Alan"),("Alan","Zombies"))
((2,"Alan"),("Alan","Ghosts"))
((3,"Glory"),("Glory","Buffy"))

```



## J


Data:


```J
table1=: ;:;._2(0 :0)
  27 Jonah
  18 Alan
  28 Glory
  18 Popeye
  28 Alan
)

table2=: ;:;._2(0 :0)
  Jonah Whales
  Jonah Spiders
  Alan Ghosts
  Alan Zombies
  Glory Buffy
)
```


The task does not specify the hash function to use, so we'll use an identity function. But [[SHA-1]] could be used instead, with a little more work (you'd need to convert the name into the bit vector needed by the SHA-1 interface). Practically speaking, though, the only benefit of SHA-1 in this context would be to slow down the join.

Implementation:


```J
hash=: ]
dojoin=:3 :0
  c1=. {.{.y
  c2=. (1 {"1 y) -. a:
  c3=. (2 {"1 y) -. a:
  >{c1;c2;<c3
)

JOIN=: ; -.&a: ,/each(hash@{."1 <@dojoin/. ]) (1 1 0&#inv@|."1 table1), 1 0 1#inv"1 table2
```


Result:


```J
   JOIN
┌─────┬──┬───────┐
│Jonah│27│Whales │
├─────┼──┼───────┤
│Jonah│27│Spiders│
├─────┼──┼───────┤
│Alan │18│Ghosts │
├─────┼──┼───────┤
│Alan │18│Zombies│
├─────┼──┼───────┤
│Alan │28│Ghosts │
├─────┼──┼───────┤
│Alan │28│Zombies│
├─────┼──┼───────┤
│Glory│28│Buffy  │
└─────┴──┴───────┘
```



## Java

{{trans|PHP}}
{{works with|Java|8}}

```java
import java.util.*;

public class HashJoin {

    public static void main(String[] args) {
        String[][] table1 = {{"27", "Jonah"}, {"18", "Alan"}, {"28", "Glory"},
        {"18", "Popeye"}, {"28", "Alan"}};

        String[][] table2 = {{"Jonah", "Whales"}, {"Jonah", "Spiders"},
        {"Alan", "Ghosts"}, {"Alan", "Zombies"}, {"Glory", "Buffy"},
        {"Bob", "foo"}};

        hashJoin(table1, 1, table2, 0).stream()
                .forEach(r -> System.out.println(Arrays.deepToString(r)));
    }

    static List<String[][]> hashJoin(String[][] records1, int idx1,
            String[][] records2, int idx2) {

        List<String[][]> result = new ArrayList<>();
        Map<String, List<String[]>> map = new HashMap<>();

        for (String[] record : records1) {
            List<String[]> v = map.getOrDefault(record[idx1], new ArrayList<>());
            v.add(record);
            map.put(record[idx1], v);
        }

        for (String[] record : records2) {
            List<String[]> lst = map.get(record[idx2]);
            if (lst != null) {
                lst.stream().forEach(r -> {
                    result.add(new String[][]{r, record});
                });
            }
        }

        return result;
    }
}
```



```txt
[[27, Jonah], [Jonah, Whales]]
[[27, Jonah], [Jonah, Spiders]]
[[18, Alan], [Alan, Ghosts]]
[[28, Alan], [Alan, Ghosts]]
[[18, Alan], [Alan, Zombies]]
[[28, Alan], [Alan, Zombies]]
[[28, Glory], [Glory, Buffy]]
```




## JavaScript


### ES6



```JavaScript
(() => {
    'use strict';

    // hashJoin :: [Dict] -> [Dict] -> String -> [Dict]
    let hashJoin = (tblA, tblB, strJoin) => {

        let [jA, jB] = strJoin.split('='),
            M = tblB.reduce((a, x) => {
                let id = x[jB];
                return (
                    a[id] ? a[id].push(x) : a[id] = [x],
                    a
                );
            }, {});

        return tblA.reduce((a, x) => {
            let match = M[x[jA]];
            return match ? (
                a.concat(match.map(row => dictConcat(x, row)))
            ) : a;
        }, []);
    },

    // dictConcat :: Dict -> Dict -> Dict
    dictConcat = (dctA, dctB) => {
        let ok = Object.keys;
        return ok(dctB).reduce(
            (a, k) => (a['B_' + k] = dctB[k]) && a,
            ok(dctA).reduce(
                (a, k) => (a['A_' + k] = dctA[k]) && a, {}
            )
        );
    };


    // TEST
    let lstA = [
        { age: 27, name: 'Jonah' },
        { age: 18, name: 'Alan' },
        { age: 28, name: 'Glory' },
        { age: 18, name: 'Popeye' },
        { age: 28, name: 'Alan' }
    ],
    lstB = [
        { character: 'Jonah', nemesis: 'Whales' },
        { character: 'Jonah', nemesis: 'Spiders' },
        { character: 'Alan', nemesis: 'Ghosts' },
        { character:'Alan', nemesis: 'Zombies' },
        { character: 'Glory', nemesis: 'Buffy' },
        { character: 'Bob', nemesis: 'foo' }
    ];

    return hashJoin(lstA, lstB, 'name=character');
    
})();

```


{{Out}}

```txt
[{"A_age":27,"A_name":"Jonah","B_character":"Jonah","B_nemesis":"Whales"},
{"A_age":27,"A_name":"Jonah","B_character":"Jonah","B_nemesis":"Spiders"},
{"A_age":18,"A_name":"Alan","B_character":"Alan","B_nemesis":"Ghosts"},
{"A_age":18,"A_name":"Alan","B_character":"Alan","B_nemesis":"Zombies"},
{"A_age":28,"A_name":"Glory","B_character":"Glory","B_nemesis":"Buffy"},
{"A_age":28,"A_name":"Alan","B_character":"Alan","B_nemesis":"Ghosts"},
{"A_age":28,"A_name":"Alan","B_character":"Alan","B_nemesis":"Zombies"}]
```



## jq

{{ works with | jq | 1.4}}

Relational tables can be represented in several ways in JSON, and so
in this section we present two distinct "hash join" functions in jq:

* "hashJoin" can be used if the tables are represented as arrays of JSON objects, or as arrays of arrays, but the result may include the join-column twice;

* "hashJoinArrays" is intended for use if the tables are represented as arrays of arrays, and avoids the duplication mentioned above.

Both versions are relationally symmetric, and both versions allow the
join columns to contain any JSON value.  To achieve this generality,
the collision-free hash function, h, is used.


### hashJoin


```jq
# hashJoin(table1; key1; table2; key2) expects the two tables to be
# arrays, either of JSON objects, or of arrays.

# In the first case, that is, if the table's rows are represented as
# objects, then key1 should be the key of the join column of table1,
# and similarly for key2; if the join columns have different names,
# then they will both be included in the resultant objects.

# In the second case, that is, if the rows are arrays, then the
# 0-based indices of the join columns should be specified, and the
# rows are simply pasted together, resulting in duplication of the
# join columns.
#
def hashJoin(table1; key1; table2; key2):
  # collision-free hash function:
  def h:
    if type == "object" then with_entries(.value = (.value|h)) | tostring
    elif type == "array" then map(h)|tostring
    else (type[0:1]+tostring)
    end;

  # hash phase:
  reduce table1[] as $row
    ({};
     ($row[key1]|h) as $key
     | . + { ($key): (.[$key] + [$row]) } )
  | . as $hash
  # join phase
  | reduce table2[] as $row
      ([];
       ($row[key2]|h) as $key
       | if $hash|has($key) then
           reduce $hash[$key][] as $r (.; . +  [ $row + $r ] )
  	 else . end)
;
```


'''Example'''

```jq
def table1:
  [ {"age": 27, "name": "Jonah"},
    {"age": 18, "name": "Alan"},
    {"age": 28, "name": "Glory"},
    {"age": 18, "name": "Popeye"},
    {"age": 28, "name": "Alan"} ]
;

def table2:
  [ {"name": "Jonah", "nemesis": "Whales"},
    {"name": "Jonah", "nemesis": "Spiders"},
    {"name": "Alan", "nemesis": "Ghosts"},
    {"name": "Alan", "nemesis": "Zombies"},
    {"name": "Glory", "nemesis": "Buffy"} ]
;

def table1a:
  [[27, "Jonah"],
   [18, "Alan"],
   [28, "Glory"],
   [18, "Popeye"],
   [28, "Alan"] ]
;

def table2a:
  [["Jonah", "Whales"],
   ["Jonah", "Spiders"],
   ["Alan", "Ghosts"],
   ["Alan", "Zombies"],
   ["Glory", "Buffy"],
   ["Holmes", "Moriarty"] ]
;

def pp: 
  reduce .[] as $row (""; . + "\n" + ($row|tostring));

( hashJoin(table1; "name"; table2; "name"),
  hashJoin(table1a; 1; table2a; 0)
) | pp
```

{{out}}

```sh
$ jq -c -r -n -f HashJoin.jq

{"age":27,"name":"Jonah","nemesis":"Whales"}
{"age":27,"name":"Jonah","nemesis":"Spiders"}
{"age":28,"name":"Alan","nemesis":"Ghosts"}
{"age":28,"name":"Alan","nemesis":"Zombies"}
{"age":28,"name":"Glory","nemesis":"Buffy"}

[27,"Jonah","Jonah","Whales"]
[27,"Jonah","Jonah","Spiders"]
[28,"Alan","Alan","Ghosts"]
[28,"Alan","Alan","Zombies"]
[28,"Glory","Glory","Buffy"]
```



### hashJoinArrays


```jq
# The tables should be arrays of arrays;
# index1 and index2 should be the 0-based indices of the join columns.
#
def hashJoinArrays(table1; index1; table2; index2):
  # collision-free hash function:
  def h:
    if type == "object" then with_entries(.value = (.value|h)) | tostring
    elif type == "array" then map(h)|tostring
    else (type[0:1]+tostring)
    end;

  # hash phase:
  reduce table1[] as $row
    ({};
     ($row[index1]|h) as $key
     | . + (.[$key] += [ $row ])  )
  | . as $hash
  # join phase
  | reduce table2[] as $row
      ([];
       ($row[index2]|h) as $key
       | if $hash|has($key) then
           reduce $hash[$key][] as $r
	     (.; 
	      . + [ $r + $row[0:index2] + $row[index2+1:] ] )
  	 else . end)
;
```

'''Example'''

In the following example, the previously defined pretty-print function (pp) and tables (table1 and table2)
are used, so their definitions are not repeated here.

```jq
hashJoinArrays(table1; 1; table2; 0) | pp
```

{{out}}

```sh
$ jq -c -r -n -f HashJoinArrays.jq

[27,"Jonah","Whales"]
[27,"Jonah","Spiders"]
[28,"Alan","Ghosts"]
[28,"Alan","Zombies"]
[28,"Glory","Buffy"]
```



## Julia

{{works with|Julia|0.6}}
For dataframes there is a builtin function join:

```julia
using DataFrames

A = DataFrame(Age = [27, 18, 28, 18, 28], Name = ["Jonah", "Alan", "Glory", "Popeye", "Alan"])
B = DataFrame(Name = ["Jonah", "Jonah", "Alan", "Alan", "Glory"],
    Nemesis = ["Whales", "Spiders", "Ghosts", "Zombies", "Buffy"])
AB = join(A, B, on = :Name)

@show A B AB
```


{{out}}

```txt
A = 5×2 DataFrames.DataFrame
│ Row │ Age │ Name     │
├─────┼─────┼──────────┤
│ 1   │ 27  │ "Jonah"  │
│ 2   │ 18  │ "Alan"   │
│ 3   │ 28  │ "Glory"  │
│ 4   │ 18  │ "Popeye" │
│ 5   │ 28  │ "Alan"   │
B = 5×2 DataFrames.DataFrame
│ Row │ Name    │ Nemesis   │
├─────┼─────────┼───────────┤
│ 1   │ "Jonah" │ "Whales"  │
│ 2   │ "Jonah" │ "Spiders" │
│ 3   │ "Alan"  │ "Ghosts"  │
│ 4   │ "Alan"  │ "Zombies" │
│ 5   │ "Glory" │ "Buffy"   │
AB = 7×3 DataFrames.DataFrame
│ Row │ Age │ Name    │ Nemesis   │
├─────┼─────┼─────────┼───────────┤
│ 1   │ 18  │ "Alan"  │ "Ghosts"  │
│ 2   │ 18  │ "Alan"  │ "Zombies" │
│ 3   │ 28  │ "Alan"  │ "Ghosts"  │
│ 4   │ 28  │ "Alan"  │ "Zombies" │
│ 5   │ 28  │ "Glory" │ "Buffy"   │
│ 6   │ 27  │ "Jonah" │ "Whales"  │
│ 7   │ 27  │ "Jonah" │ "Spiders" │
```


Following the task hint:

```julia
function hashjoin(A::Array, ja::Int, B::Array, jb::Int)
    M = Dict(t[jb] => filter(l -> l[jb] == t[jb], B) for t in B)
    return collect([a, b] for a in A for b in get(M, a[ja], ()))
end

table1 = [(27, "Jonah"),
          (18, "Alan"),
          (28, "Glory"),
          (18, "Popeye"),
          (28, "Alan")]
table2 = [("Jonah", "Whales"),
          ("Jonah", "Spiders"),
          ("Alan", "Ghosts"),
          ("Alan", "Zombies"),
          ("Glory", "Buffy")]

for r in hashjoin(table1, 2, table2, 1)
    println(r)
end
```


{{out}}

```txt
Tuple{Any,String}[(27, "Jonah"), ("Jonah", "Whales")]
Tuple{Any,String}[(27, "Jonah"), ("Jonah", "Spiders")]
Tuple{Any,String}[(18, "Alan"), ("Alan", "Ghosts")]
Tuple{Any,String}[(18, "Alan"), ("Alan", "Zombies")]
Tuple{Any,String}[(28, "Glory"), ("Glory", "Buffy")]
Tuple{Any,String}[(28, "Alan"), ("Alan", "Ghosts")]
Tuple{Any,String}[(28, "Alan"), ("Alan", "Zombies")]
```



## Kotlin


```scala
data class A(val age: Int, val name: String)

data class B(val character: String, val nemesis: String)

data class C(val rowA: A, val rowB: B)

fun hashJoin(tableA: List<A>, tableB: List<B>): List<C> {
    val mm = tableB.groupBy { it.character }
    val tableC = mutableListOf<C>()
    for (a in tableA) {
        val value = mm[a.name] ?: continue
        for (b in value) tableC.add(C(a, b))
    }
    return tableC.toList()
}

fun main(args: Array<String>) {
    val tableA = listOf(
        A(27, "Jonah"),
        A(18, "Alan"),
        A(28, "Glory"),
        A(18, "Popeye"),
        A(28, "Alan")
    )
    val tableB = listOf(
        B("Jonah", "Whales"),
        B("Jonah", "Spiders"),
        B("Alan", "Ghosts"),
        B("Alan", "Zombies"),
        B("Glory", "Buffy")
    )
    val tableC = hashJoin(tableA, tableB)
    println("A.Age A.Name B.Character B.Nemesis")
    println("----- ------ ----------- ---------")
    for (c in tableC) {
        print("${c.rowA.age}    ${c.rowA.name.padEnd(6)} ")
        println("${c.rowB.character.padEnd(6)}      ${c.rowB.nemesis}")
    }
}
```


{{out}}

```txt

A.Age A.Name B.Character B.Nemesis
----- ------ ----------- ---------
27    Jonah  Jonah       Whales
27    Jonah  Jonah       Spiders
18    Alan   Alan        Ghosts
18    Alan   Alan        Zombies
28    Glory  Glory       Buffy
28    Alan   Alan        Ghosts
28    Alan   Alan        Zombies

```



## LFE


```lisp

(defun hash (column table)
  (lists:foldl
    (lambda (x acc)
      (orddict:append (proplists:get_value column x) x acc))
    '()
    table))

(defun get-hash (col hash-table)
  (proplists:get_value
     (proplists:get_value col r)
     hashed))

(defun merge (row-1 row-2)
  (orddict:merge
    (lambda (k v1 v2) v2)
    (lists:sort row-1)
    (lists:sort row-2)))

(defun hash-join (table-1 col-1 table-2 col-2)
  (let ((hashed (hash col-1 table-1)))
    (lc ((<- r table-2))
        (lc ((<- s (get-hash col-2 hashed)))
            (merge r s)))))

```


Table definitions in the LFE REPL:

```lisp

> (set ss '((#(age 27) #(name "Jonah"))
            (#(age 18) #(name "Alan"))
            (#(age 28) #(name "Glory"))
            (#(age 18) #(name "Popeye"))
            (#(age 28) #(name "Alan"))))

> (set rs '((#(nemesis "Whales") #(name "Jonah"))
            (#(nemesis "Spiders") #(name "Jonah"))
            (#(nemesis "Ghosts") #(name "Alan"))
            (#(nemesis "Zombies") #(name "Alan"))
            (#(nemesis "Buffy") #(name "Glory"))))

```


Output in LFE REPL:

```lisp

> (hash-join ss 'name rs 'name)
(((#(age 27) #(name "Jonah") #(nemesis "Whales")))
 ((#(age 27) #(name "Jonah") #(nemesis "Spiders")))
 ((#(age 18) #(name "Alan") #(nemesis "Ghosts"))
  (#(age 28) #(name "Alan") #(nemesis "Ghosts")))
 ((#(age 18) #(name "Alan") #(nemesis "Zombies"))
  (#(age 28) #(name "Alan") #(nemesis "Zombies")))
 ((#(age 28) #(name "Glory") #(nemesis "Buffy"))))

```




## M2000 Interpreter


```M2000 Interpreter

Module HashJoin {
      \\ normally we define variables when we put values to names
      \\ so we can remove these two lines
      Def Name$, Nemesis$
      Def Long m, mc, items_size, A
 
      \\ Lets make a container which use keys with hash function
      Inventory A      
      \\ A now is a pointer to an Inventory, with Len(A)=0
      \\ Print Type$(A)="Inventory"
 
      \\ empty stack. We use current stack to place data
      Flush
      \Input B
      data "Jonah", "Whales"
      data "Jonah", "Spiders"
      data "Alan", "Ghosts"
      data "Alan", "Zombies"
      data "Glory", "Buffy"
      \\ Keys are unique, This is the HASH PHASE
      While not empty { 
            Read Name$, Nemesis$
            If Exist(A, Name$) Then {
                  m=Eval(A)  ' get a pointer to array
                  Stack m {Data Nemesis$}
            } Else Append A, Name$:=Stack:=Nemesis$  ' a stack object with one item
      }
      \\ Input A, this is the Long Table
      data 27, "Jonah"
      data 18, "Alan"
      data 28, "Glory"
      data 18, "Popeye"
      data 28, "Alan"
 
      \\ This is the JOIN PHASE
 
      items_size=stack.size/2
      \\ using items_size we can append data (using data) to stack
      \\ $(0) is the default handler for columns.
      \\ Letters justify to left, numbers to right.
      \\ Letters can use more columns, and maybe wrap to more lines.
 
      Print $(0), "Output during join"
      Print "A.Age", "A.Name","B.Character", "B.Nemesis"
      While items_size>0 {
            Read Age, Name$
            If exist(A, Name$) Then {
                  m=Eval(A)   ' extract a pointer, this is for a stack object
                  mc=Each(m)  ' make an iterator
                  While mc {
                        \\ we use $(1) for left justify numbers too
                        \\ normal StackItem$(stackobject) return top only
                        \\ we have to use StackItem$(stackobject, 3) to get 3rd
                        \\ or StackItem(stackobject, 3) if it is numeric.
                        \\ but here mc is iterator, and place the cursor value to it
                       Print $(1), Age, Name$,Name$, StackItem$(mc)
                        \\ so now we place at the end of current stack the same output
                       Data Age, Name$,Name$, StackItem$(mc)
                  }   
            } 
            items_size--
      }
      \\ split rem line after : to use second way
      rem : goto secondway
      Print $(0), "Output after join"
      Print "A.Age", "A.Name","B.Character", "B.Nemesis"
      While not Empty {
            Print $(1), Number, Letter$, Letter$, Letter$
      }
      Exit
secondway:
      Print $(0), "Output after join using format$()"
      Print Format$("{0:5} {1:10} {2:10} {3:20}","A.Age", "A.Name","B.Character", "B.Nemesis")
      While not Empty {
            Print format$("{0::5} {1:10} {2:10} {3:20}", Number, Letter$, Letter$, Letter$)
      }
}
HashJoin

```

{{out}}
<pre >
27    Jonah      Jonah      Whales              
27    Jonah      Jonah      Spiders             
18    Alan       Alan       Ghosts              
18    Alan       Alan       Zombies             
28    Glory      Glory      Buffy               
28    Alan       Alan       Ghosts              
28    Alan       Alan       Zombies             

</pre >

=={{header|Mathematica}} / {{header|Wolfram Language}}==

{{works with|Mathematica|10}}

Updated version is now able to join wider tables by giving the index. The smaller table is hashed but this might result in different column ordering. Uses Associations introduced in Mathematica Version 10

```mathematica
hashJoin[table1_List,table1colindex_Integer,table2_List,table2colindex_Integer]:=Module[{h,f,t1,t2,tmp},
t1=If[table1colindex != 1,table1[[All,Prepend[Delete[Range@Length@table1[[1]],table1colindex],table1colindex]]],table1];
t2=If[table2colindex != 1, table2[[All,Prepend[Delete[Range@Length@table2[[1]],table2colindex],table2colindex]]],table2];

If[Length[t1]>Length[t2],tmp=t1;t1=t2;t2=tmp;];
h= GroupBy[t1,First];
f[{a_,b_List}]:={a,#}&/@b;
Partition[Flatten[Map[f,{#[[2;;]],h[#[[1]]]}&/@t2
]],Length[t1[[1]]]+Length[t2[[1]]]-1]
];
```

Sample run:

```txt

table1 = {{18, "Alan", 1}, {27, "Jonah", 2}, {28, "Alan", 3}, {28, 
    "Glory", 4}};
table2 = {{"Alan", "Ghosts"}, {"Alan", "Zombies"}, {"Glory", 
    "Buffy"}, {"Jonah", "Spiders"}, {"Jonah", "Whales"}};
table1colindex = 2;
table2colindex = 1;

hashJoin[table1, table1colindex, table2, table2colindex] // TableForm

Ghosts	Alan	18	1
Ghosts	Alan	28	3
Zombies	Alan	18	1
Zombies	Alan	28	3
Buffy	Glory	28	4
Spiders	Jonah	27	2
Whales	Jonah	27	2

table1 = {{18, "Alan", 1}, {27, "Jonah", 2}, {28, "Alan", 3}, {28, 
    "Glory", 4}};
table2 = {{33, "Alan", "Ghosts"}, {34, "Alan", "Zombies"}, {35, 
    "Glory", "Buffy"}, {36, "Jonah", "Spiders"}, {37, "Jonah", 
    "Whales"}};
table1colindex = 2;
table2colindex = 2;

hashJoin[table1, table1colindex, table2, table2colindex] // TableForm

33	Ghosts	Alan	18	1
33	Ghosts	Alan	28	3
34	Zombies	Alan	18	1
34	Zombies	Alan	28	3
35	Buffy	Glory	28	4
36	Spiders	Jonah	27	2
37	Whales	Jonah	27	2

table1 = {{19, "Zorro", 8}, {17, "Zorro", 7}, {17, "Zorro", 9}, {18, 
    "Alan", 1}, {27, "Jonah", 2}, {28, "Alan", 3}, {28, "Glory", 4}};
table2 = {{33, "Alan", "Ghosts"}, {34, "Alan", "Zombies"}, {35, 
    "Glory", "Buffy"}, {36, "Jonah", "Spiders"}, {37, "Jonah", 
    "Whales"}, {39, "Zorro", "Fox"}};
table1colindex = 2;
table2colindex = 2;

hashJoin[table1, table1colindex, table2, table2colindex] // TableForm

19	8	Zorro	39	Fox
17	7	Zorro	39	Fox
17	9	Zorro	39	Fox
18	1	Alan	33	Ghosts
18	1	Alan	34	Zombies
27	2	Jonah	36	Spiders
27	2	Jonah	37	Whales
28	3	Alan	33	Ghosts
28	3	Alan	34	Zombies
28	4	Glory	35	Buffy

```


=={{header|Oberon-2}}==
Works with oo2c version 2

```oberon2

MODULE HashJoin;
IMPORT 
  ADT:Dictionary,
  ADT:LinkedList,
  NPCT:Tools,
  Object,
  Object:Boxed,
  Out;
TYPE
  (* Some Aliases *)
  Age= Boxed.LongInt;
  Name= STRING;
  Nemesis= STRING;
  
  (* Generic Tuple *)
  Tuple(E1: Object.Object; E2: Object.Object) = POINTER TO TupleDesc(E1,E2);
  TupleDesc(E1: Object.Object; E2: Object.Object) = RECORD
    (Object.ObjectDesc)
    _1: E1;
    _2: E2;
  END;

  (* Relations *)
  RelationA = ARRAY 5 OF Tuple(Age,Name);
  RelationB = ARRAY 5 OF Tuple(Name,Nemesis);

VAR
  tableA: RelationA;
  tableB: RelationB;
  dict: Dictionary.Dictionary(Name,LinkedList.LinkedList(Age));
  ll: LinkedList.LinkedList(Age);
  
  PROCEDURE (t: Tuple(E1, E2)) INIT*(e1: E1; e2: E2);
  BEGIN
    t._1 := e1;
    t._2 := e2;
  END INIT;
  
  PROCEDURE DoHashPhase(t: RelationA;VAR dict: Dictionary.Dictionary(Name,LinkedList.LinkedList(Age)));
  VAR
    i: INTEGER;
    ll: LinkedList.LinkedList(Age);
  BEGIN
    i := 0;
    WHILE (i < LEN(t)) & (t[i] # NIL) DO
      IF (dict.HasKey(t[i]._2)) THEN
        ll := dict.Get(t[i]._2);
      ELSE
        ll := NEW(LinkedList.LinkedList(Age));
        dict.Set(t[i]._2,ll)
      END;
      ll.Append(t[i]._1);
      INC(i)
    END
  END DoHashPhase;
  
  PROCEDURE DoJoinPhase(t: RelationB; dict: Dictionary.Dictionary(Name,LinkedList.LinkedList(Age)));
  VAR
    i: INTEGER;
    age: Age;
    iterll: LinkedList.Iterator(Age);
  BEGIN
    FOR i := 0 TO LEN(t) - 1 DO
    ll := dict.Get(t[i]._1);
    iterll := ll.GetIterator(NIL);
    WHILE iterll.HasNext() DO
      age := iterll.Next();
      Out.LongInt(age.value,4);
      Out.Object(Tools.AdjustRight(t[i]._1,10));      
      Out.Object(Tools.AdjustRight(t[i]._2,10));Out.Ln
    END
  END
  END DoJoinPhase;

BEGIN
  (* tableA initialization *)
  tableA[0] := NEW(Tuple(Age,Name),NEW(Age,27),"Jonah");
  tableA[1] := NEW(Tuple(Age,Name),NEW(Age,18),"Alan");
  tableA[2] := NEW(Tuple(Age,Name),NEW(Age,28),"Glory");
  tableA[3] := NEW(Tuple(Age,Name),NEW(Age,18),"Popeye");
  tableA[4] := NEW(Tuple(Age,Name),NEW(Age,28),"Alan");
  
  (* tableB initialization *)
  tableB[0] := NEW(Tuple(Name,Nemesis),"Jonah","Whales");
  tableB[1] := NEW(Tuple(Name,Nemesis),"Jonah","Spiders");
  tableB[2] := NEW(Tuple(Name,Nemesis),"Alan","Ghost");
  tableB[3] := NEW(Tuple(Name,Nemesis),"Alan","Zombies");
  tableB[4] := NEW(Tuple(Name,Nemesis),"Glory","Buffy");  
  
  dict := NEW(Dictionary.Dictionary(Name,LinkedList.LinkedList(Age)));
  
  DoHashPhase(tableA,dict);
  DoJoinPhase(tableB,dict);
END HashJoin.

```

Output:

```txt

  27     Jonah    Whales
  27     Jonah   Spiders
  18      Alan     Ghost
  28      Alan     Ghost
  18      Alan   Zombies
  28      Alan   Zombies
  28     Glory     Buffy

```



## OCaml

This exploits the fact that Hashtbl implements a hash table that can store multiple values for a key, for an especially simple solution:

```ocaml
let hash_join table1 f1 table2 f2 =
  let h = Hashtbl.create 42 in
  (* hash phase *)
  List.iter (fun s ->
    Hashtbl.add h (f1 s) s) table1;
  (* join phase *)
  List.concat (List.map (fun r ->
    List.map (fun s -> s, r) (Hashtbl.find_all h (f2 r))) table2)
```

Sample run:

```txt

# let table1 = [27, "Jonah";
                18, "Alan";
                28, "Glory";
                18, "Popeye";
                28, "Alan"];;
# let table2 = ["Jonah", "Whales";
                "Jonah", "Spiders";
                "Alan", "Ghosts";
                "Alan", "Zombies";
                "Glory", "Buffy"];;
# hash_join table1 snd table2 fst;;
- : ((int * string) * (string * string)) list =
[((27, "Jonah"), ("Jonah", "Whales")); ((27, "Jonah"), ("Jonah", "Spiders"));
 ((28, "Alan"), ("Alan", "Ghosts")); ((18, "Alan"), ("Alan", "Ghosts"));
 ((28, "Alan"), ("Alan", "Zombies")); ((18, "Alan"), ("Alan", "Zombies"));
 ((28, "Glory"), ("Glory", "Buffy"))]

```



## Perl


```perl
use Data::Dumper qw(Dumper);

sub hashJoin {
    my ($table1, $index1, $table2, $index2) = @_;
    my %h;
    # hash phase
    foreach my $s (@$table1) {
	push @{ $h{$s->[$index1]} }, $s;
    }
    # join phase
    map { my $r = $_;
	  map [$_, $r], @{ $h{$r->[$index2]} }
    } @$table2;
}

@table1 = ([27, "Jonah"],
           [18, "Alan"],
           [28, "Glory"],
           [18, "Popeye"],
           [28, "Alan"]);
@table2 = (["Jonah", "Whales"],
           ["Jonah", "Spiders"],
           ["Alan", "Ghosts"],
           ["Alan", "Zombies"],
           ["Glory", "Buffy"]);

$Data::Dumper::Indent = 0;
foreach my $row (hashJoin(\@table1, 1, \@table2, 0)) {
    print Dumper($row), "\n";
}
```

{{out}}

```txt

$VAR1 = [[27,'Jonah'],['Jonah','Whales']];
$VAR1 = [[27,'Jonah'],['Jonah','Spiders']];
$VAR1 = [[18,'Alan'],['Alan','Ghosts']];
$VAR1 = [[28,'Alan'],['Alan','Ghosts']];
$VAR1 = [[18,'Alan'],['Alan','Zombies']];
$VAR1 = [[28,'Alan'],['Alan','Zombies']];
$VAR1 = [[28,'Glory'],['Glory','Buffy']];

```



## Perl 6


The <tt>.classify</tt> method returns a multimap represented as a <tt>Hash</tt> whose values are <tt>Array</tt>s.

{{works with|Rakudo|2016.07}}

```perl6
sub hash-join(@a, &a, @b, &b) {
    my %hash := @b.classify(&b);
    
    @a.map: -> $a {
        |(%hash{a $a} // next).map: -> $b { [$a, $b] }
    }
}

# Testing:

my @A =
    [27, "Jonah"],
    [18, "Alan"],
    [28, "Glory"],
    [18, "Popeye"],
    [28, "Alan"],
;

my @B =
    ["Jonah", "Whales"],
    ["Jonah", "Spiders"],
    ["Alan", "Ghosts"],
    ["Alan", "Zombies"],
    ["Glory", "Buffy"],
;

.say for hash-join @A, *[1], @B, *[0];
```


{{out}}

```txt
[[27 Jonah] [Jonah Whales]]
[[27 Jonah] [Jonah Spiders]]
[[18 Alan] [Alan Ghosts]]
[[18 Alan] [Alan Zombies]]
[[28 Glory] [Glory Buffy]]
[[28 Alan] [Alan Ghosts]]
[[28 Alan] [Alan Zombies]]
```



## Phix

Phix dictionary keys must be unique, but storing/extending a sequence is no trouble, and in fact simplifies the scan phase.

```Phix
constant A = {{27,"Jonah"},
              {18,"Alan"},
              {28,"Glory"},
              {18,"Popeye"},
              {28,"Alan"}},
         B = {{"Jonah","Whales"},
              {"Jonah","Spiders"},
              {"Alan", "Ghosts"},
              {"Alan", "Zombies"},
              {"Glory","Buffy"}},
        jA = 2,
        jB = 1,
        MB = new_dict()
sequence C = {}
for i=1 to length(B) do
    object key = B[i][jB]
    object data = getd(key,MB)
    if data=0 then
        data = {B[i]}
    else
        data = append(data,B[i])
    end if
    putd(key,data,MB)
end for
for i=1 to length(A) do
    object data = getd(A[i][jA],MB)
    if sequence(data) then
        for j=1 to length(data) do
            C = append(C,{A[i],data[j]})
        end for
    end if
end for
destroy_dict(MB)
pp(C,{pp_Nest,1})
```

{{out}}

```txt

{{{27, "Jonah"}, {"Jonah", "Whales"}},
 {{27, "Jonah"}, {"Jonah", "Spiders"}},
 {{18, "Alan"}, {"Alan", "Ghosts"}},
 {{18, "Alan"}, {"Alan", "Zombies"}},
 {{28, "Glory"}, {"Glory", "Buffy"}},
 {{28, "Alan"}, {"Alan", "Ghosts"}},
 {{28, "Alan"}, {"Alan", "Zombies"}}}

```



## PHP


```php
<?php
function hashJoin($table1, $index1, $table2, $index2) {
    // hash phase
    foreach ($table1 as $s)
        $h[$s[$index1]][] = $s;
    // join phase
    foreach ($table2 as $r)
    	foreach ($h[$r[$index2]] as $s)
	    $result[] = array($s, $r);
    return $result;
}

$table1 = array(array(27, "Jonah"),
           array(18, "Alan"),
           array(28, "Glory"),
           array(18, "Popeye"),
           array(28, "Alan"));
$table2 = array(array("Jonah", "Whales"),
           array("Jonah", "Spiders"),
           array("Alan", "Ghosts"),
           array("Alan", "Zombies"),
           array("Glory", "Buffy"),
           array("Bob", "foo"));

foreach (hashJoin($table1, 1, $table2, 0) as $row)
    print_r($row);
?>
```

{{out}}

```txt

Array
(
    [0] => Array
        (
            [0] => 27
            [1] => Jonah
        )

    [1] => Array
        (
            [0] => Jonah
            [1] => Whales
        )

)
Array
(
    [0] => Array
        (
            [0] => 27
            [1] => Jonah
        )

    [1] => Array
        (
            [0] => Jonah
            [1] => Spiders
        )

)
Array
(
    [0] => Array
        (
            [0] => 18
            [1] => Alan
        )

    [1] => Array
        (
            [0] => Alan
            [1] => Ghosts
        )

)
Array
(
    [0] => Array
        (
            [0] => 28
            [1] => Alan
        )

    [1] => Array
        (
            [0] => Alan
            [1] => Ghosts
        )

)
Array
(
    [0] => Array
        (
            [0] => 18
            [1] => Alan
        )

    [1] => Array
        (
            [0] => Alan
            [1] => Zombies
        )

)
Array
(
    [0] => Array
        (
            [0] => 28
            [1] => Alan
        )

    [1] => Array
        (
            [0] => Alan
            [1] => Zombies
        )

)
Array
(
    [0] => Array
        (
            [0] => 28
            [1] => Glory
        )

    [1] => Array
        (
            [0] => Glory
            [1] => Buffy
        )

)
```



## PicoLisp


```PicoLisp
(de A
   (27 . Jonah)
   (18 . Alan)
   (28 . Glory)
   (18 . Popeye)
   (28 . Alan) )

(de B
   (Jonah . Whales)
   (Jonah . Spiders)
   (Alan . Ghosts)
   (Alan . Zombies)
   (Glory . Buffy) )

(for X B
   (let K (cons (char (hash (car X))) (car X))
      (if (idx 'M K T)
         (push (caar @) (cdr X))
         (set (car K) (list (cdr X))) ) ) )

(for X A
   (let? Y (car (idx 'M (cons (char (hash (cdr X))) (cdr X))))
      (for Z (caar Y)
         (println (car X) (cdr X) (cdr Y) Z) ) ) )
```

Output:

```txt
27 Jonah Jonah Spiders
27 Jonah Jonah Whales
18 Alan Alan Zombies
18 Alan Alan Ghosts
28 Glory Glory Buffy
28 Alan Alan Zombies
28 Alan Alan Ghosts
```




## plainTeX

Works with any TeX engine.

```tex
\newtoks\tabjoin
\def\quark{\quark}
\def\tabA{27:Jonah,18:Alan,28:Glory,18:Popeye,28:Alan}
\def\tabB{Jonah:Whales,Jonah:Spiders,Alan:Ghosts,Alan:Zombies,Glory:Buffy}
\def\mergejoin{\tabjoin{}\expandafter\mergejoini\tabA,\quark:\quark,}
\def\mergejoini#1:#2,{%
	\ifx\quark#1\the\tabjoin
	\else
		\def\mergejoinii##1,#2:##2,{%
			\ifx\quark##2\else
				\tabjoin\expandafter{\the\tabjoin#1 : #2 : ##2\par}%
				\expandafter\mergejoinii\expandafter,%
			\fi
		}%
		\expandafter\mergejoinii\expandafter,\tabB,#2:\quark,%
		\expandafter\mergejoini
	\fi
}
\mergejoin
\bye
```


pdf or dvi output:

```txt
27 : Jonah : Whales
27 : Jonah : Spiders
18 : Alan : Ghosts
18 : Alan : Zombies
28 : Glory : Buffy
28 : Alan : Ghosts
28 : Alan : Zombies
```



## Prolog


```Prolog
% Name/Age	
person_age('Jonah',  27).	
person_age('Alan',   18).	
person_age('Glory',  28).	
person_age('Popeye', 18).	
person_age('Alan',   28).	

% Character/Nemesis
character_nemisis('Jonah', 'Whales').
character_nemisis('Jonah', 'Spiders').
character_nemisis('Alan',  'Ghosts').
character_nemisis('Alan',  'Zombies').
character_nemisis('Glory', 'Buffy').

join_and_print :-
	format('Age\tName\tCharacter\tNemisis\n\n'),		
	forall(
		(person_age(Person, Age), character_nemisis(Person, Nemesis)),	
		format('~w\t~w\t~w\t\t~w\n', [Age, Person, Person, Nemesis])
	).
```

{{out}}

```txt

?- join_and_print.
Age     Name    Character       Nemisis

27      Jonah   Jonah           Whales
27      Jonah   Jonah           Spiders
18      Alan    Alan            Ghosts
18      Alan    Alan            Zombies
28      Glory   Glory           Buffy
28      Alan    Alan            Ghosts
28      Alan    Alan            Zombies
true.

```



## PureBasic


```PureBasic
Structure tabA
  age.i
  name.s  
EndStructure

Structure tabB
  char_name.s
  nemesis.s
EndStructure

NewList listA.tabA()
NewList listB.tabB()

Macro SetListA(c_age, c_name)
  AddElement(listA()) : listA()\age = c_age : listA()\name = c_name  
EndMacro

Macro SetListB(c_char, c_nem)
  AddElement(listB()) : listB()\char_name = c_char : listB()\nemesis = c_nem
EndMacro

SetListA(27, "Jonah")   : SetListA(18, "Alan") : SetListA(28, "Glory")
SetListA(18, "Popeye")  : SetListA(28, "Alan")

SetListB("Jonah", "Whales") : SetListB("Jonah", "Spiders")
SetListB("Alan", "Ghosts")  : SetListB("Alan", "Zombies")
SetListB("Glory", "Buffy")

If OpenConsole("Hash_join")
  ForEach listA()
    PrintN("Input A = "+Str(listA()\age)+~"\t"+listA()\name)
  Next
  PrintN("")
  ForEach listB()
    PrintN("Input B = "+listB()\char_name+~"\t"+listB()\nemesis)
  Next
  PrintN(~"\nOutput\nA.Age\tA.Name\tB.Char.\tB.Nemesis")
  ForEach listA()
    ForEach listB()
      If listA()\name = listB()\char_name
        PrintN(Str(listA()\age)+~"\t"+listA()\name+~"\t"+
               listB()\char_name+~"\t"+listB()\nemesis)
      EndIf
    Next
  Next
  Input()
EndIf
```

{{out}}

```txt
Input A = 27    Jonah
Input A = 18    Alan
Input A = 28    Glory
Input A = 18    Popeye
Input A = 28    Alan

Input B = Jonah Whales
Input B = Jonah Spiders
Input B = Alan  Ghosts
Input B = Alan  Zombies
Input B = Glory Buffy

Output
A.Age   A.Name  B.Char. B.Nemesis
27      Jonah   Jonah   Whales
27      Jonah   Jonah   Spiders
18      Alan    Alan    Ghosts
18      Alan    Alan    Zombies
28      Glory   Glory   Buffy
28      Alan    Alan    Ghosts
28      Alan    Alan    Zombies
```



## Python


```python
from collections import defaultdict

def hashJoin(table1, index1, table2, index2):
    h = defaultdict(list)
    # hash phase
    for s in table1:
        h[s[index1]].append(s)
    # join phase
    return [(s, r) for r in table2 for s in h[r[index2]]]

table1 = [(27, "Jonah"),
          (18, "Alan"),
          (28, "Glory"),
          (18, "Popeye"),
          (28, "Alan")]
table2 = [("Jonah", "Whales"),
          ("Jonah", "Spiders"),
          ("Alan", "Ghosts"),
          ("Alan", "Zombies"),
          ("Glory", "Buffy")]

for row in hashJoin(table1, 1, table2, 0):
    print(row)
```

{{out}}

```txt

((27, 'Jonah'), ('Jonah', 'Whales'))
((27, 'Jonah'), ('Jonah', 'Spiders'))
((18, 'Alan'), ('Alan', 'Ghosts'))
((28, 'Alan'), ('Alan', 'Ghosts'))
((18, 'Alan'), ('Alan', 'Zombies'))
((28, 'Alan'), ('Alan', 'Zombies'))
((28, 'Glory'), ('Glory', 'Buffy'))

```



## Racket


```racket
#lang racket
(struct A  (age name))
(struct B  (name nemesis))
(struct AB (name age nemesis) #:transparent)

(define Ages-table
  (list (A 27 "Jonah") (A 18 "Alan")
        (A 28 "Glory") (A 18 "Popeye")
        (A 28 "Alan")))

(define Nemeses-table
  (list
   (B "Jonah" "Whales") (B "Jonah" "Spiders")
   (B "Alan" "Ghosts") (B "Alan" "Zombies")
   (B "Glory" "Buffy")))

;; Hash phase
(define name->ages#
  (for/fold ((rv (hash)))
    ((a (in-list Ages-table)))
    (match-define (A age name) a)
    (hash-update rv name (λ (ages) (append ages (list age))) null)))

;; Join phase     
(for*/list
    ((b (in-list Nemeses-table))
     (key (in-value (B-name b)))
     (age (in-list (hash-ref name->ages# key))))
  (AB key age (B-nemesis b)))
```


{{out}}

```txt
(#(struct:AB "Jonah" 27 "Whales")
 #(struct:AB "Jonah" 27 "Spiders")
 #(struct:AB "Alan" 18 "Ghosts")
 #(struct:AB "Alan" 28 "Ghosts")
 #(struct:AB "Alan" 18 "Zombies")
 #(struct:AB "Alan" 28 "Zombies")
 #(struct:AB "Glory" 28 "Buffy"))
```



## REXX


```rexx
/*REXX program demonstrates the  classic hash join algorithm  for two relations.        */
                     S.  =                   ;         R.  =
                     S.1 = 27 'Jonah'        ;         R.1 = "Jonah Whales"
                     S.2 = 18 'Alan'         ;         R.2 = "Jonah Spiders"
                     S.3 = 28 'Glory'        ;         R.3 = "Alan Ghosts"
                     S.4 = 18 'Popeye'       ;         R.4 = "Alan Zombies"
                     S.5 = 28 'Alan'         ;         R.5 = "Glory Buffy"
hash.=                                           /*initialize the  hash  table (array). */
      do #=1  while S.#\=='';   parse var  S.#  age  name          /*extract information*/
      hash.name=hash.name #                      /*build a hash table entry with its idx*/
      end   /*#*/                                /* [↑]  REXX does the heavy work here. */
#=#-1                                            /*adjust for the DO loop  (#)  overage.*/
      do j=1  while R.j\==''                     /*process a nemesis for a name element.*/
      parse var  R.j  x  nemesis                 /*extract the name  and  its nemesis.  */
      if hash.x==''  then do;   #=# + 1          /*Not in hash?  Then a new name; bump #*/
                                S.#=','  x       /*add a new name to the    S   table.  */
                                hash.x=#         /* "  "  "    "   "  "   hash    "     */
                          end                    /* [↑]  this  DO  isn't used today.    */
           do k=1  for words(hash.x);   _=word(hash.x, k)          /*obtain the pointer.*/
           S._=S._  nemesis                      /*add the nemesis ──► applicable hash. */
           end   /*k*/
      end        /*j*/
_='─'                                            /*the character used for the separator.*/
pad=left('', 4)                                  /*spacing used in header and the output*/
say  pad  center('age', 3)   pad   center("name", 20   )   pad    center('nemesis', 30   )
say  pad  center('───', 3)   pad   center(""    , 20, _)   pad    center(''       , 30, _)

      do n=1  for #;      parse  var  S.n    age  name  nems       /*obtain information.*/
      if nems==''  then iterate                                    /*No nemesis?  Skip. */
      say pad right(age,3) pad center(name,20) pad center(nems,30) /*display an  "S".   */
      end   /*n*/                                /*stick a fork in it,  we're all done. */
```

'''output'''   when using the in-code relations (data):

```txt

     age              name                         nemesis
     ───      ────────────────────      ──────────────────────────────
      27             Jonah                      Whales Spiders
      18              Alan                      Ghosts Zombies
      28             Glory                          Buffy
      28              Alan                      Ghosts Zombies

```



## Ring


```ring
Table1 = [[27, "Jonah"], [18, "Alan"], [28, "Glory"], [18, "Popeye"], [28, "Alan"]]
Table2 = [["Jonah", "Whales"], ["Jonah", "Spiders"], ["Alan", "Ghosts"], ["Alan", "Zombies"], ["Glory", "Buffy"]]
hTable = []
Qtable = []

for a in table1
	h = hashing(a[2])
	add(htable,[h , a])
next

for b in table2
	h = hashing(b[1])
	for sh in htable
		if sh[1] = h 
			 add(qtable, sh[2] + b[2])  
		ok
	next
next

print(qtable)

#
### ============End of Execution======


func print lst
see "---------------------------------------------------
Age	| Name		|| Name		| Nemesis
---------------------------------------------------
"
for l in lst
	see string(l[1]) + char(9) + "| " + l[2] + copy(char(9),2) + "|| " + l[2] + "    " + char(9) +  "| " + l[3] + nl 
next 

func Hashing str
r = 0
if len(str) > 4
	r = (ascii(str[1]) + ascii(str[len(str)]) + ascii(str[ceil(len(str) * 0.25)]) + ascii(str[ceil(len(str) * 0.75)])) 
else
	for s in str 
		r += ascii(s) 
	next
ok
return r
```

{{out}}

```txt

---------------------------------------------------
Age     | Name          || Name         | Nemesis
---------------------------------------------------
27      | Jonah         || Jonah        | Whales
27      | Jonah         || Jonah        | Whales
18      | Alan          || Alan         | Ghosts
28      | Alan          || Alan         | Ghosts
18      | Alan          || Alan         | Ghosts
28      | Alan          || Alan         | Ghosts
28      | Glory         || Glory        | Buffy

```



## Ruby


```ruby
def hashJoin(table1, index1, table2, index2)
  # hash phase
  h = table1.group_by {|s| s[index1]}
  h.default = []
  # join phase
  table2.collect {|r|
    h[r[index2]].collect {|s| [s, r]}
  }.flatten(1)
end

table1 = [[27, "Jonah"],
          [18, "Alan"],
          [28, "Glory"],
          [18, "Popeye"],
          [28, "Alan"]]
table2 = [["Jonah", "Whales"],
          ["Jonah", "Spiders"],
          ["Alan", "Ghosts"],
          ["Alan", "Zombies"],
          ["Glory", "Buffy"]]

hashJoin(table1, 1, table2, 0).each { |row| p row }
```


{{out}}

```txt

[[27, "Jonah"], ["Jonah", "Whales"]]
[[27, "Jonah"], ["Jonah", "Spiders"]]
[[18, "Alan"], ["Alan", "Ghosts"]]
[[28, "Alan"], ["Alan", "Ghosts"]]
[[18, "Alan"], ["Alan", "Zombies"]]
[[28, "Alan"], ["Alan", "Zombies"]]
[[28, "Glory"], ["Glory", "Buffy"]]

```



## Run BASIC


```Runbasic
sqliteconnect #mem, ":memory:"

#mem execute("CREATE TABLE t_age(age,name)")
#mem execute("CREATE TABLE t_name(name,nemesis)")

#mem execute("INSERT INTO t_age VALUES(27,'Jonah')")
#mem execute("INSERT INTO t_age VALUES(18,'Alan')")
#mem execute("INSERT INTO t_age VALUES(28,'Glory')")
#mem execute("INSERT INTO t_age VALUES(18,'Popeye')")
#mem execute("INSERT INTO t_age VALUES(28,'Alan')")

#mem execute("INSERT INTO t_name VALUES('Jonah','Whales')")
#mem execute("INSERT INTO t_name VALUES('Jonah','Spiders')")
#mem execute("INSERT INTO t_name VALUES('Alan','Ghosts')")
#mem execute("INSERT INTO t_name VALUES('Alan','Zombies')")
#mem execute("INSERT INTO t_name VALUES('Glory','Buffy')")

#mem execute("SELECT *,t_age.name FROM t_age LEFT JOIN t_name ON t_name.name = t_age.name")
WHILE  #mem hasanswer()
	#row		= #mem #nextrow()
	age		= #row age()
	name$		= #row name$()
	nemesis$	= #row nemesis$()
print age;" ";name$;" ";nemesis$
WEND
```
Output:

```txt
27 Jonah Spiders
27 Jonah Whales
18 Alan Ghosts
18 Alan Zombies
28 Glory Buffy
18 Popeye 
28 Alan Ghosts
28 Alan Zombies
```



## Rust


```rust
use std::collections::HashMap;
use std::hash::Hash;

// If you know one of the tables is smaller, it is best to make it the second parameter.
fn hash_join<A, B, K>(first: &[(K, A)], second: &[(K, B)]) -> Vec<(A, K, B)>
where
    K: Hash + Eq + Copy,
    A: Copy,
    B: Copy,
{
    let mut hash_map = HashMap::new();

    // hash phase
    for &(key, val_a) in second {
        // collect all values by their keys, appending new ones to each existing entry
        hash_map.entry(key).or_insert_with(Vec::new).push(val_a);
    }

    let mut result = Vec::new();
    // join phase
    for &(key, val_b) in first {
        if let Some(vals) = hash_map.get(&key) {
            let tuples = vals.iter().map(|&val_a| (val_b, key, val_a));
            result.extend(tuples);
        }
    }

    result
}

fn main() {
    let table1 = [("Jonah", 27), ("Alan", 18), ("Glory", 28), ("Popeye", 18), ("Alan", 28)];
    let table2 = [
        ("Jonah", "Whales"), ("Jonah", "Spiders"), ("Alan", "Ghosts"),
        ("Alan", "Zombies"), ("Glory", "Buffy")
    ];
    let result = hash_join(&table1, &table2);
    println!("Age | Character Name | Nemesis");
    println!("----|----------------|--------");
    for (age, name, nemesis) in result {
        println!("{:<3} | {:^14} | {}", age, name, nemesis);
    }
}
```

{{out}}

```txt
Age | Character Name | Nemesis
----|----------------|--------
27  |     Jonah      | Whales
27  |     Jonah      | Spiders
18  |      Alan      | Ghosts
18  |      Alan      | Zombies
28  |     Glory      | Buffy
28  |      Alan      | Ghosts
28  |      Alan      | Zombies
```



## Scala


```Scala
def join[Type](left: Seq[Seq[Type]], right: Seq[Seq[Type]]) = {
    val hash = right.groupBy(_.head) withDefaultValue Seq()
    left.flatMap(cols => hash(cols.last).map(cols ++ _.tail))
}

// Example:

val table1 = List(List("27", "Jonah"),
                  List("18", "Alan"),
                  List("28", "Glory"),
                  List("18", "Popeye"),
                  List("28", "Alan"))
val table2 = List(List("Jonah", "Whales"),
                  List("Jonah", "Spiders"),
                  List("Alan", "Ghosts"),
                  List("Alan", "Zombies"),
                  List("Glory", "Buffy"))

println(join(table1, table2) mkString "\n")
```

{{out}}

```txt
List(27, Jonah, Whales)
List(27, Jonah, Spiders)
List(18, Alan, Ghosts)
List(18, Alan, Zombies)
List(28, Glory, Buffy)
List(28, Alan, Ghosts)
List(28, Alan, Zombies)
```



## Scheme

{{works with|Gauche Scheme}}

```Scheme
(use srfi-42)

(define ages '((27 Jonah) (18 Alan) (28 Glory) (18 Popeye) (28 Alan)))
 
(define nemeses '((Jonah Whales) (Jonah Spiders) (Alan Ghosts)
                  (Alan Zombies) (Glory Buffy)
                  (unknown nothing)))
 
(define hash (make-hash-table 'equal?))

(dolist (item ages)
  (hash-table-push! hash (last item) (car item)))

(do-ec
  (: person nemeses)
  (:let name (car person))
  (if (hash-table-exists? hash name))
  (: age (~ hash name))
  (print (list (list age name)
               person)))

```

{{output}}

```txt

((27 Jonah) (Jonah Whales))
((27 Jonah) (Jonah Spiders))
((28 Alan) (Alan Ghosts))
((18 Alan) (Alan Ghosts))
((28 Alan) (Alan Zombies))
((18 Alan) (Alan Zombies))
((28 Glory) (Glory Buffy))

```



## Sidef


```ruby
func hashJoin(table1, index1, table2, index2) {
    var a = []
    var h = Hash()

    # hash phase
    table1.each { |s|
        h{s[index1]} := [] << s
    }

    # join phase
    table2.each { |r|
        a += h{r[index2]}.map{[_,r]}
    }

    return a
}

var t1  = [[27, "Jonah"],
           [18, "Alan"],
           [28, "Glory"],
           [18, "Popeye"],
           [28, "Alan"]]

var t2  = [["Jonah", "Whales"],
           ["Jonah", "Spiders"],
           ["Alan", "Ghosts"],
           ["Alan", "Zombies"],
           ["Glory", "Buffy"]]

hashJoin(t1, 1, t2, 0).each { .say }
```

{{out}}

```txt
[[27, 'Jonah'], ['Jonah', 'Whales']]
[[27, 'Jonah'], ['Jonah', 'Spiders']]
[[18, 'Alan'], ['Alan', 'Ghosts']]
[[28, 'Alan'], ['Alan', 'Ghosts']]
[[18, 'Alan'], ['Alan', 'Zombies']]
[[28, 'Alan'], ['Alan', 'Zombies']]
[[28, 'Glory'], ['Glory', 'Buffy']]
```



## SQL

{{works with|oracle}}

```sql
-- setting up the test data

create table people (age number(3), name varchar2(30));
insert into people (age, name)
  select 27, 'Jonah'  from dual union all
  select 18, 'Alan'   from dual union all
  select 28, 'Glory'  from dual union all
  select 18, 'Popeye' from dual union all
  select 28, 'Alan'   from dual
;

create table nemesises (name varchar2(30), nemesis varchar2(30));
insert into nemesises (name, nemesis) 
  select 'Jonah', 'Whales'  from dual union all
  select 'Jonah', 'Spiders' from dual union all
  select 'Alan' , 'Ghosts'  from dual union all
  select 'Alan' , 'Zombies' from dual union all
  select 'Glory', 'Buffy'   from dual
;
```


Doing the join is trivial. Normally we would let the optimizer select the join method. However, to force a hash join, we can use an optimizer hint, USE_HASH.

```sql
select /*+ use_hash */ * from people join nemesises using(name);
```


{{out}}

```txt
 AGE NAME             NEMESIS
---- ---------------- ----------------
  27 Jonah            Whales
  27 Jonah            Spiders
  28 Alan             Ghosts
  18 Alan             Ghosts
  28 Alan             Zombies
  18 Alan             Zombies
  28 Glory            Buffy
```



## Tcl

Tcl uses hash tables to implement both its associative arrays and its dictionaries.

```tcl
package require Tcl 8.6
# Only for lmap, which can be replaced with foreach

proc joinTables {tableA a tableB b} {
    # Optimisation: if the first table is longer, do in reverse order
    if {[llength $tableB] < [llength $tableA]} {
	return [lmap pair [joinTables $tableB $b $tableA $a] {
	    lreverse $pair
	}]
    }

    foreach value $tableA {
	lappend hashmap([lindex $value $a]) [lreplace $value $a $a]
	#dict version# dict lappend hashmap [lindex $value $a] [lreplace $value $a $a]
    }
    set result {}
    foreach value $tableB {
	set key [lindex $value $b]
	if {![info exists hashmap($key)]} continue
	#dict version# if {![dict exists $hashmap $key]} continue
	foreach first $hashmap($key) {
	    #dict version# foreach first [dict get $hashmap $key]
	    lappend result [list {*}$first $key {*}[lreplace $value $b $b]]
	}
    }
    return $result
}

set tableA {
    {27 "Jonah"} {18 "Alan"} {28 "Glory"} {18 "Popeye"} {28 "Alan"}
}
set tableB {
    {"Jonah" "Whales"} {"Jonah" "Spiders"} {"Alan" "Ghosts"} {"Alan" "Zombies"}
    {"Glory" "Buffy"}
}
set joined [joinTables $tableA 1 $tableB 0]
foreach row $joined {
    puts $row
}
```

{{out}}

```txt

27 Jonah Whales
27 Jonah Spiders
18 Alan Ghosts
28 Alan Ghosts
18 Alan Zombies
28 Alan Zombies
28 Glory Buffy

```



## TXR


Generic hash join. Arguments <code>left-key</code> and <code>right-key</code> are functions applied to the elements of the <code>left</code> and <code>right</code> sequences to retrieve the join key.


```txrlisp
(defvar age-name '((27 Jonah)
                   (18 Alan)
                   (28 Glory)
                   (18 Popeye)
                   (28 Alan)))

(defvar nemesis-name '((Jonah Whales)
                       (Jonah Spiders)
                       (Alan Ghosts)
                       (Alan Zombies)
                       (Glory Buffy)))

(defun hash-join (left left-key right right-key)
  (let ((join-hash [group-by left-key left])) ;; hash phase
    (append-each ((r-entry right))            ;; join phase
      (collect-each ((l-entry [join-hash [right-key r-entry]]))
        ^(,l-entry ,r-entry)))))

(format t "~s\n" [hash-join age-name second nemesis-name first])
```


{{out}}


```txt
$ txr hash-join.tl
(((27 Jonah) (Jonah Whales)) ((27 Jonah) (Jonah Spiders)) ((18 Alan) (Alan Ghosts)) ((28 Alan) (Alan Ghosts)) ((18 Alan) (Alan Zombies)) ((28 Alan) (Alan Zombies)) ((28 Glory) (Glory Buffy)))
```



## VBScript


```vb

Dim t_age(4,1)
t_age(0,0) = 27 : t_age(0,1) = "Jonah"
t_age(1,0) = 18 : t_age(1,1) = "Alan"
t_age(2,0) = 28 : t_age(2,1) = "Glory"
t_age(3,0) = 18 : t_age(3,1) = "Popeye"
t_age(4,0) = 28 : t_age(4,1) = "Alan"

Dim t_nemesis(4,1)
t_nemesis(0,0) = "Jonah" : t_nemesis(0,1) = "Whales"
t_nemesis(1,0) = "Jonah" : t_nemesis(1,1) = "Spiders"
t_nemesis(2,0) = "Alan" : t_nemesis(2,1) = "Ghosts"
t_nemesis(3,0) = "Alan" : t_nemesis(3,1) = "Zombies"
t_nemesis(4,0) = "Glory" : t_nemesis(4,1) = "Buffy"

Call hash_join(t_age,1,t_nemesis,0)

Sub hash_join(table_1,index_1,table_2,index_2)
	Set hash = CreateObject("Scripting.Dictionary")
	For i = 0 To UBound(table_1)
		hash.Add i,Array(table_1(i,0),table_1(i,1))
	Next
	For j = 0 To UBound(table_2)
		For Each key In hash.Keys
			If hash(key)(index_1) = table_2(j,index_2) Then
				WScript.StdOut.WriteLine hash(key)(0) & "," & hash(key)(1) &_
					" = " & table_2(j,0) & "," & table_2(j,1)
			End If
		Next
	Next
End Sub

```


{{Out}}

```txt

27,Jonah = Jonah,Whales
27,Jonah = Jonah,Spiders
18,Alan = Alan,Ghosts
28,Alan = Alan,Ghosts
18,Alan = Alan,Zombies
28,Alan = Alan,Zombies
28,Glory = Glory,Buffy

```



## Visual FoxPro

Hashing using the common key (name) gives ambiguous results as the name column is not unique in either table (a unique key could be formed by using the age and name columns) . This implementation forces a unique key on the people table.

```vfp

LOCAL i As Integer, n As Integer
CLOSE DATABASES ALL
*!* Create and populate the hash tables
CREATE CURSOR people_ids(id I, used L DEFAULT .F.)
INDEX ON id TAG id COLLATE "Machine"
INDEX ON used TAG used BINARY COLLATE "Machine"
SET ORDER TO 0
CREATE CURSOR nem_ids(id I, used L DEFAULT .F.)
INDEX ON id TAG id COLLATE "Machine"
INDEX ON used TAG used BINARY COLLATE "Machine"
SET ORDER TO 0
n = 100
FOR i = 1 TO n
    INSERT INTO people_ids (id) VALUES (i)
    INSERT INTO nem_ids (id) VALUES (i)
ENDFOR	

CREATE CURSOR people (age I, name V(16), id I)
INDEX ON id TAG id COLLATE "Machine"
INDEX ON name TAG name COLLATE "Machine"
SET ORDER TO 0
INSERT INTO people (age, name) VALUES (27, "Jonah")
INSERT INTO people (age, name) VALUES (18, "Alan")
INSERT INTO people (age, name) VALUES (28, "Glory")
INSERT INTO people (age, name) VALUES (18, "Popeye")
INSERT INTO people (age, name) VALUES (28, "Alan")
REPLACE id WITH HashMe("people_ids") ALL

*!* The plural of nemesis is nemeses
CREATE CURSOR nemeses (name V(16), nemesis V(16), p_id I, id I)
INDEX ON id TAG id COLLATE "Machine"
INDEX ON p_id TAG p_id COLLATE "Machine"
INDEX ON name TAG name COLLATE "Machine"
SET ORDER TO 0

INSERT INTO nemeses (name, nemesis) VALUES ("Jonah", "Whales")
INSERT INTO nemeses (name, nemesis) VALUES ("Jonah", "Spiders")
INSERT INTO nemeses (name, nemesis) VALUES ("Alan", "Ghosts")
INSERT INTO nemeses (name, nemesis) VALUES ("Alan", "Zombies")
INSERT INTO nemeses (name, nemesis) VALUES ("Glory", "Buffy")
REPLACE id WITH HashMe("nem_ids") ALL 
UPDATE nemeses SET p_id = people.id FROM people ;
WHERE nemeses.name = people.name

*!* Show the join
SELECT pe.age, pe.name, ne.nemesis FROM people pe ;
JOIN nemeses ne ON pe.id = ne.p_id TO FILE "hashjoin.txt"

FUNCTION HashMe(cTable As String) As Integer
LOCAL ARRAY a[1]
SELECT MIN(id) FROM (cTable) WHERE NOT used INTO ARRAY a
UPDATE (cTable) SET used = .T. WHERE id = a[1]
RETURN a[1]
ENDFUNC

```

{{out}}

```txt

AGE NAME      NEMESIS                                                
 27 Jonah     Whales                                                 
 27 Jonah     Spiders                                                
 18 Alan      Ghosts                                                 
 18 Alan      Zombies                                                
 28 Glory     Buffy                                                  

```



## zkl

Join two tables by hashing on the common key (name). The resulting join is the intersection of the two tables.

```zkl
ageName:=T(27,"Jonah", 18,"Alan", 28,"Glory", 18,"Popeye", 28,"Alan");
nameNemesis:=T("Jonah","Whales", "Jonah","Spiders", "Alan","Ghosts", 
      "Alan","Zombies", "Glory","Buffy");

fcn addAN(age,name,d){  // keys are names, values are ( (age,...),() )
   if (r:=d.find(name)) d[name] = T(r[0].append(age),r[1]);
   else d.add(name,T(T(age),T));
   d	// return d so pump will use that as result for assignment
}
fcn addNN(name,nemesis,d){ // values-->( (age,age...), (nemesis,...) )
   if (r:=d.find(name)){
      ages,nemesises := r;
      d[name] = T(ages,nemesises.append(nemesis));
   }
}
    // Void.Read --> take existing i, read next one, pass both to next function
var d=ageName.pump(Void,Void.Read,T(addAN,Dictionary()));
nameNemesis.pump(Void,Void.Read,T(addNN,d));

d.println();  // the union of the two tables
d.keys.sort().pump(Console.println,'wrap(name){  //pretty print the join
   val:=d[name]; if (not val[1])return(Void.Skip);
   String(name,":",d[name][1].concat(","));
})
```

zkl Dictionaries only have one key

```txt

D(Popeye:L(L(18),L()),Glory:L(L(28),L("Buffy")),
  Jonah:L(L(27),L("Whales","Spiders")),Alan:L(L(18,28),L("Ghosts","Zombies")))
Alan:Ghosts,Zombies
Glory:Buffy
Jonah:Whales,Spiders

```


{{omit from|UNIX Shell}}
