+++
title = "Dinesman's multiple-dwelling problem"
description = ""
date = 2019-08-13T17:24:47Z
aliases = []
[extra]
id = 9965
[taxonomies]
categories = ["task", "Dinesman's multiple-dwelling problem"]
tags = []
+++

## Task

Solve [https://web.archive.org/web/20170325033240/http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-28.html#%_sec_4.3.2 Dinesman's multiple dwelling problem] but in a way that most naturally follows the problem statement given below.

Solutions are allowed (but not required) to parse and interpret the problem text, but should remain flexible and should state what changes to the problem text are allowed. Flexibility and ease of expression are valued.

Examples may be be split into "setup", "problem statement", and "output" sections where the ease and naturalness of stating the problem and getting an answer, as well as the ease and flexibility of modifying the problem are the primary concerns.

Example output should be shown here, as well as any comments on the examples flexibility.


;The problem
Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house that contains only five floors.

* Baker does not live on the top floor.
* Cooper does not live on the bottom floor.
* Fletcher does not live on either the top or the bottom floor.
* Miller lives on a higher floor than does Cooper.
* Smith does not live on a floor adjacent to Fletcher's.
* Fletcher does not live on a floor adjacent to Cooper's.


''Where does everyone live?''






## Ada

Uses an enum type People to attempt to be naturally reading.
Problem is easily changed by altering subtype Floor, type people and the somewhat naturally reading constraints in the Constrained function.
If for example you change the floor range to 1..6 and add Superman to people, all possible solutions will be printed.

```Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure Dinesman is
   subtype Floor is Positive range 1 .. 5;
   type People is (Baker, Cooper, Fletcher, Miller, Smith);
   type Floors is array (People'Range) of Floor;
   type PtFloors is access all Floors;

   function Constrained (f : PtFloors) return Boolean is begin
      if f (Baker) /= Floor'Last and
         f (Cooper) /= Floor'First and
         Floor'First < f (Fletcher) and f (Fletcher) < Floor'Last and
         f (Miller) > f (Cooper) and
         abs (f (Smith) - f (Fletcher)) /= 1 and
         abs (f (Fletcher) - f (Cooper)) /= 1
      then return True; end if;
      return False;
   end Constrained;

   procedure Solve (list : PtFloors; n : Natural) is
      procedure Swap (I : People; J : Natural) is
         temp : constant Floor := list (People'Val (J));
      begin list (People'Val (J)) := list (I); list (I) := temp;
      end Swap;
   begin
      if n = 1 then
         if Constrained (list) then
            for p in People'Range loop
               Put_Line (p'Img & " on floor " & list (p)'Img);
            end loop;
         end if;
         return;
      end if;
      for i in People'First .. People'Val (n - 1) loop
         Solve (list, n - 1);
         if n mod 2 = 1 then Swap (People'First, n - 1);
         else Swap (i, n - 1); end if;
      end loop;
   end Solve;

   thefloors : aliased Floors;
begin
   for person in People'Range loop
      thefloors (person) := People'Pos (person) + Floor'First;
   end loop;
   Solve (thefloors'Access, Floors'Length);
end Dinesman;
```

```txt
BAKER on floor  3
COOPER on floor  2
FLETCHER on floor  4
MILLER on floor  5
SMITH on floor  1
```



## ALGOL 68

Algol 68 allows structures containing procedures to have a different procedure for each instance (similar to making each instance
a separate derived class in OO languages). This allows for easy specification of the constraints.
The constraints for each person could be changed by providing a different PROC(INT)BOOL in the initialisation of the inhabitants.
Changing the number of inhabitants would require adding or removing loops from the solution finding code.

```algol68
# attempt to solve the dinesman Multiple Dwelling problem #

# SETUP #

# special floor values #
INT    top floor    = 4;
INT    bottom floor = 0;

# mode to specify the persons floor constraint #
MODE PERSON = STRUCT( STRING name, REF INT floor, PROC( INT )BOOL ok );

# yields TRUE if the floor of the specified person is OK, FALSE otherwise #
OP OK = ( PERSON p )BOOL: ( ok OF p )( floor OF p );

# yields TRUE if floor is adjacent to other persons floor, FALSE otherwise #
PROC adjacent = ( INT floor, other persons floor )BOOL: floor >= ( other persons floor - 1 ) AND floor <= ( other persons floor + 1 );

# displays the floor of an occupant #
PROC print floor = ( PERSON occupant )VOID: print( ( whole( floor OF occupant, -1 ), " ", name OF occupant, newline ) );

# PROBLEM STATEMENT #

# the inhabitants with their floor and constraints #
PERSON baker    = ( "Baker",    LOC INT := 0, ( INT floor )BOOL: floor /= top floor );
PERSON cooper   = ( "Cooper",   LOC INT := 0, ( INT floor )BOOL: floor /= bottom floor );
PERSON fletcher = ( "Fletcher", LOC INT := 0, ( INT floor )BOOL: floor /= top floor AND floor /= bottom floor
                                                                                    AND NOT adjacent( floor, floor OF cooper ) );
PERSON miller   = ( "Miller",   LOC INT := 0, ( INT floor )BOOL: floor > floor OF cooper );
PERSON smith    = ( "Smith",    LOC INT := 0, ( INT floor )BOOL: NOT adjacent( floor, floor OF fletcher ) );

# SOLUTION #

# "brute force" solution - we run through the possible 5^5 configurations          #
# we cold optimise this by e.g. restricting f to bottom floor + 1 TO top floor - 1 #
# at the cost of reducing the flexibility of the constraints                       #
# alternatively, we could add minimum and maximum allowed floors to the PERSON     #
# STRUCT and loop through these instead of bottom floor TO top floor               #

FOR b FROM bottom floor TO top floor DO
    floor OF baker := b;
    FOR c FROM bottom floor TO top floor DO
        IF b /= c THEN
            floor OF cooper := c;
            FOR f FROM bottom floor TO top floor DO
                IF b /= f AND c /= f THEN
                    floor OF fletcher := f;
                    FOR m FROM bottom floor TO top floor DO
                        IF b /= m AND c /= m AND f /= m THEN
                            floor OF miller   := m;
                            FOR s FROM bottom floor TO top floor DO
                                IF b /= s AND c /= s AND f /= s AND m /= s THEN
                                    floor OF smith    := s;
                                    IF OK baker AND OK cooper AND OK fletcher AND OK miller AND OK smith
                                    THEN
                                        # found a solution #
                                        print floor( baker    );
                                        print floor( cooper   );
                                        print floor( fletcher );
                                        print floor( miller   );
                                        print floor( smith    )
                                    FI
                                FI
                            OD
                        FI
                    OD
                FI
            OD
        FI
    OD
OD
```

```txt

2 Baker
1 Cooper
3 Fletcher
4 Miller
0 Smith

```



## AutoHotkey

See [[Dinesman's multiple-dwelling problem/AutoHotkey]].

## AWK


```AWK

# syntax: GAWK -f DINESMANS_MULTIPLE-DWELLING_PROBLEM.AWK
BEGIN {
    for (Baker=1; Baker<=5; Baker++) {
      for (Cooper=1; Cooper<=5; Cooper++) {
        for (Fletcher=1; Fletcher<=5; Fletcher++) {
          for (Miller=1; Miller<=5; Miller++) {
            for (Smith=1; Smith<=5; Smith++) {
              if (rules() ~ /^1+$/) {
                printf("%d Baker\n",Baker)
                printf("%d Cooper\n",Cooper)
                printf("%d Fletcher\n",Fletcher)
                printf("%d Miller\n",Miller)
                printf("%d Smith\n",Smith)
              }
            }
          }
        }
      }
    }
    exit(0)
}
function rules(  stmt1,stmt2,stmt3,stmt4,stmt5,stmt6,stmt7) {
# The following problem statements may be changed:
#
# Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house
# that contains only five floors numbered 1 (ground) to 5 (top)
    stmt1 = Baker!=Cooper && Baker!=Fletcher && Baker!=Miller && Baker!=Smith &&
            Cooper!=Fletcher && Cooper!=Miller && Cooper!=Smith &&
            Fletcher!=Miller && Fletcher!=Smith &&
            Miller!=Smith
    stmt2 = Baker != 5                     # Baker does not live on the top floor
    stmt3 = Cooper != 1                    # Cooper does not live on the bottom floor
    stmt4 = Fletcher != 5 && Fletcher != 1 # Fletcher does not live on either the top or the bottom floor
    stmt5 = Miller > Cooper                # Miller lives on a higher floor than does Cooper
    stmt6 = abs(Smith-Fletcher) != 1       # Smith does not live on a floor adjacent to Fletcher's
    stmt7 = abs(Fletcher-Cooper) != 1      # Fletcher does not live on a floor adjacent to Cooper's
    return(stmt1 stmt2 stmt3 stmt4 stmt5 stmt6 stmt7)
}
function abs(x) { if (x >= 0) { return x } else { return -x } }

```

```txt

3 Baker
2 Cooper
4 Fletcher
5 Miller
1 Smith

```



## BBC BASIC

Each of the statements is represented by an equivalent conditional expression ('''stmt1$''', '''stmt2$''' etc.) as indicated in the comments, where the variables '''Baker''', '''Cooper''' etc. evaluate to the appropriate floor number.  So long as each statement can be expressed in this way, and there is a unique solution, changes to the problem text can be accommodated.


```bbcbasic
      REM Floors are numbered 0 (ground) to 4 (top)

      REM "Baker, Cooper, Fletcher, Miller, and Smith live on different floors":
      stmt1$ = "Baker<>Cooper AND Baker<>Fletcher AND Baker<>Miller AND " + \
      \        "Baker<>Smith AND Cooper<>Fletcher AND Cooper<>Miller AND " + \
      \        "Cooper<>Smith AND Fletcher<>Miller AND Fletcher<>Smith AND " + \
      \        "Miller<>Smith"

      REM "Baker does not live on the top floor":
      stmt2$ = "Baker<>4"

      REM "Cooper does not live on the bottom floor":
      stmt3$ = "Cooper<>0"

      REM "Fletcher does not live on either the top or the bottom floor":
      stmt4$ = "Fletcher<>0 AND Fletcher<>4"

      REM "Miller lives on a higher floor than does Cooper":
      stmt5$ = "Miller>Cooper"

      REM "Smith does not live on a floor adjacent to Fletcher's":
      stmt6$ = "ABS(Smith-Fletcher)<>1"

      REM "Fletcher does not live on a floor adjacent to Cooper's":
      stmt7$ = "ABS(Fletcher-Cooper)<>1"

      FOR Baker = 0 TO 4
        FOR Cooper = 0 TO 4
          FOR Fletcher = 0 TO 4
            FOR Miller = 0 TO 4
              FOR Smith = 0 TO 4
                IF EVAL(stmt2$) IF EVAL(stmt3$) IF EVAL(stmt5$) THEN
                  IF EVAL(stmt4$) IF EVAL(stmt6$) IF EVAL(stmt7$) THEN
                    IF EVAL(stmt1$) THEN
                      PRINT "Baker lives on floor " ; Baker
                      PRINT "Cooper lives on floor " ; Cooper
                      PRINT "Fletcher lives on floor " ; Fletcher
                      PRINT "Miller lives on floor " ; Miller
                      PRINT "Smith lives on floor " ; Smith
                    ENDIF
                  ENDIF
                ENDIF
              NEXT Smith
            NEXT Miller
          NEXT Fletcher
        NEXT Cooper
      NEXT Baker
      END
```

```txt

Baker lives on floor 2
Cooper lives on floor 1
Fletcher lives on floor 3
Miller lives on floor 4
Smith lives on floor 0

```



## Bracmat

The rules constitute the body of the 'constraints' function.
Each statement of the problem is translated into a pattern.
Patterns are the rhs of the ':' operator.
Constraints can be added or deleted as you like.
If the problem is underspecified, for example by deleting one or more patterns,
all solutions are output, because the line following the output statement forces Bracmat to backtrack.
Patterns are read as follows: the '~' means negation, a '?' is a wildcard that can span zero or more floors, a '|' means alternation.
If in a pattern there is no wildcard to the left of a person's name,
the pattern states that the person must live in the bottom floor.
If in a pattern there is no wildcard to the right of a person's name,
the pattern states that the person must live in the top floor.
If in a pattern name A is left of name B, the pattern states that person A is living in a lower floor than person B.
Patterns can be alternated with the '|' (OR) operator.
The match operator ':', when standing between two patterns,
functions as an AND operation, because both sides must match the subject argument 'arg'.
The names of the people can be changed to anything, except empty strings.
Bracmat supports UTF-8 encoded Unicode characters,
but falls back to ISO 8859-1 if a string cannot be parsed as UTF-8.
If a name contains characters that can be misinterpreted as operators,
such as '.' or ' ', the name must be enclosed in double quotes.
If there are no reserved characters in a name, double quotes are optional.


```Bracmat
(   Baker Cooper Fletcher Miller Smith:?people
  & ( constraints
    =
      .   !arg
        : ~(? Baker)
        : ~(Cooper ?)
        : ~(Fletcher ?|? Fletcher)
        : ? Cooper ? Miller ?
        : ~(? Smith Fletcher ?|? Fletcher Smith ?)
        : ~(? Cooper Fletcher ?|? Fletcher Cooper ?)
    )
  & ( solution
    =   floors persons A Z person
      .   !arg:(?floors.?persons)
        & (   !persons:
            & constraints$!floors
            & out$("Inhabitants, from bottom to top:" !floors)
            & ~     { The ~ always fails on evaluation. Here, failure forces Bracmat to backtrack and find all solutions, not just the first one. }
          |   !persons
            :   ?A
                %?`person
                (?Z&solution$(!floors !person.!A !Z))
          )
    )
  & solution$(.!people)
|        { After outputting all solutions, the lhs of the | operator fails. The rhs of the | operator, here an empty string, is the final result. }
);
```


```txt
Inhabitants, from bottom to top: Smith Cooper Baker Fletcher Miller
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int verbose = 0;
#define COND(a, b) int a(int *s) { return (b); }
typedef int(*condition)(int *);

/* BEGIN problem specific setup */
#define N_FLOORS 5
#define TOP (N_FLOORS - 1)
int solution[N_FLOORS] = { 0 };
int occupied[N_FLOORS] = { 0 };

enum tenants {
	baker = 0,
	cooper,
	fletcher,
	miller,
	smith,
	phantom_of_the_opera,
};

const char *names[] = {
	"baker",
	"cooper",
	"fletcher",
	"miller",
	"smith",
};

COND(c0, s[baker] != TOP);
COND(c1, s[cooper] != 0);
COND(c2, s[fletcher] != 0 && s[fletcher] != TOP);
COND(c3, s[miller] > s[cooper]);
COND(c4, abs(s[smith] - s[fletcher]) != 1);
COND(c5, abs(s[cooper] - s[fletcher]) != 1);
#define N_CONDITIONS 6

condition cond[] = { c0, c1, c2, c3, c4, c5 };

/* END of problem specific setup */


int solve(int person)
{
	int i, j;
	if (person == phantom_of_the_opera) {
		/* check condition */
		for (i = 0; i < N_CONDITIONS; i++) {
			if (cond[i](solution)) continue;

			if (verbose) {
				for (j = 0; j < N_FLOORS; j++)
					printf("%d %s\n", solution[j], names[j]);
				printf("cond %d bad\n\n", i);
			}
			return 0;
		}

		printf("Found arrangement:\n");
		for (i = 0; i < N_FLOORS; i++)
			printf("%d %s\n", solution[i], names[i]);
		return 1;
	}

	for (i = 0; i < N_FLOORS; i++) {
		if (occupied[i]) continue;
		solution[person] = i;
		occupied[i] = 1;
		if (solve(person + 1)) return 1;
		occupied[i] = 0;
	}
	return 0;
}

int main()
{
	verbose = 0;
	if (!solve(0)) printf("Nobody lives anywhere\n");
	return 0;
}
```

```txt
Found arrangement:
2 baker
1 cooper
3 fletcher
4 miller
0 smith
```


C, being its compiled self, is not terribly flexible in dynamically changing runtime code content.
Parsing some external problem specification would be one way, but for a small problem, it might as well just recompile with conditions hard coded.
For this program, to change conditions, one needs to edit content between BEGIN and END of problem specific setup.
Those could even be setup in an external file and gets <code>#include</code>d if need be.


## C#


### Constraints as functions solution

Usage of the DinesmanSolver is very simple. Just feed it a bunch of constraints in the form of functions. (It could also be one function with a bunch of 'and' clauses)<br/>
Each tenant is considered an integer from 0 to count.

For each solution, it will output an array of integers that represent the tenants ordered by floor number, from the bottom floor to the top.


```c#
public class Program
{
    public static void Main()
    {
        const int count = 5;
        const int Baker = 0, Cooper = 1, Fletcher = 2, Miller = 3, Smith = 4;
        string[] names = { nameof(Baker), nameof(Cooper), nameof(Fletcher), nameof(Miller), nameof(Smith) };

        Func<int[], bool>[] constraints = {
            floorOf => floorOf[Baker] != count-1,
            floorOf => floorOf[Cooper] != 0,
            floorOf => floorOf[Fletcher] != count-1 && floorOf[Fletcher] != 0,
            floorOf => floorOf[Miller] > floorOf[Cooper],
            floorOf => Math.Abs(floorOf[Smith] - floorOf[Fletcher]) > 1,
            floorOf => Math.Abs(floorOf[Fletcher] - floorOf[Cooper]) > 1,
        };

        var solver = new DinesmanSolver();
        foreach (var tenants in solver.Solve(count, constraints)) {
            Console.WriteLine(string.Join(" ", tenants.Select(t => names[t])));
        }
    }
}

public class DinesmanSolver
{
    public IEnumerable<int[]> Solve(int count, params Func<int[], bool>[] constraints) {
        foreach (int[] floorOf in Permutations(count)) {
            if (constraints.All(c => c(floorOf))) {
                yield return Enumerable.Range(0, count).OrderBy(i => floorOf[i]).ToArray();
            }
        }
    }

    static IEnumerable<int[]> Permutations(int length) {
        if (length == 0) {
            yield return new int[0];
            yield break;
        }
        bool forwards = false;
        foreach (var permutation in Permutations(length - 1)) {
            for (int i = 0; i < length; i++) {
                yield return permutation.InsertAt(forwards ? i : length - i - 1, length - 1).ToArray();
            }
            forwards = !forwards;
        }
    }
}

static class Extensions
{
    public static IEnumerable<T> InsertAt<T>(this IEnumerable<T> source, int position, T newElement) {
        if (source == null) throw new ArgumentNullException(nameof(source));
        if (position < 0) throw new ArgumentOutOfRangeException(nameof(position));
        return InsertAtIterator(source, position, newElement);
    }

    private static IEnumerable<T> InsertAtIterator<T>(IEnumerable<T> source, int position, T newElement) {
        int index = 0;
        foreach (T element in source) {
            if (index == position) yield return newElement;
            yield return element;
            index++;
        }
        if (index < position) throw new ArgumentOutOfRangeException(nameof(position));
        if (index == position) yield return newElement;
    }
}
```

```txt

Smith Cooper Baker Fletcher Miller

```


### Shorter Linq solution

This challenge is badly stated. It is trivial to state/add any variant as a where clause (and to the enum) in the Linq query. Need more information in order to automatically parse such statements and there is no specification of this in the challenge.
```c#
using System;
using System.Collections.Generic;
using static System.Linq.Enumerable;

static class Program
{
    enum Tenants { Baker = 0, Cooper = 1, Fletcher = 2, Miller = 3, Smith = 4 };

    static void Main()
    {
        var count = Enum.GetNames(typeof(Tenants)).Length;
        var top = count - 1;

        var solve =
            from f in Range(0, count).Permutations()
            let floors = f.ToArray()
            where floors[(int)Tenants.Baker] != top //r1
            where floors[(int)Tenants.Cooper] != 0 //r2
            where floors[(int)Tenants.Fletcher] != top && floors[(int)Tenants.Fletcher] != 0 //r3
            where floors[(int)Tenants.Miller] > floors[(int)Tenants.Cooper] //r4
            where Math.Abs(floors[(int)Tenants.Smith] - floors[(int)Tenants.Fletcher]) !=1 //r5
            where Math.Abs(floors[(int)Tenants.Fletcher] - floors[(int)Tenants.Cooper]) !=1 //r6
            select floors;
        var solved = solve.First();
        var output = Range(0,count).OrderBy(i=>solved[i]).Select(f => ((Tenants)f).ToString());
        Console.WriteLine(String.Join(" ", output));
        Console.Read();
    }

    public static IEnumerable<IEnumerable<T>> Permutations<T>(this IEnumerable<T> values)
    {
        if (values.Count() == 1)
            return values.ToSingleton();

        return values.SelectMany(v => Permutations(values.Except(v.ToSingleton())), (v, p) => p.Prepend(v));
    }

    public static IEnumerable<T> ToSingleton<T>(this T item) { yield return item; }
}
```

Output:

```txt
Smith Cooper Baker Fletcher Miller

```



## Ceylon


```ceylon
shared void run() {

	function notAdjacent(Integer a, Integer b) => (a - b).magnitude >= 2;
	function allDifferent(Integer* ints) => ints.distinct.size == ints.size;

	value solutions = [
		for (baker in 1..4)
		for (cooper in 2..5)
		for (fletcher in 2..4)
		for (miller in 2..5)
		for (smith in 1..5)
		if (miller > cooper &&
			notAdjacent(smith, fletcher) &&
			notAdjacent(fletcher, cooper) &&
			allDifferent(baker, cooper, fletcher, miller, smith))
		"baker lives on ``baker``
		 cooper lives on ``cooper``
		 fletcher lives on ``fletcher``
		 miller lives on ``miller``
		 smith lives on ``smith``"
	];

	print(solutions.first else "No solution!");
}
```

```txt
baker lives on 3
cooper lives on 2
fletcher lives on 4
miller lives on 5
smith lives on 1
```



## Clojure

This solution uses the contributed package ''clojure.core.logic'', a miniKanren-based logic solver (and contributed ''clojure.tools.macro'' as well).
The "setup" part of this code defines relational functions (or constraints) for testing "immediately above", "higher", and "on nonadjacent floors".
These are used (along with the package's "permuteo" constraint) to define a constraint ''dinesmano'' which searches for all the resident orders that satisfy the criteria.
The criteria are listed in one-to-one correspondence with the problem statement.
The problem statement could be changed to any mixture of these constraint types,
and additional constraint functions could be defined as necessary.
The final part of the code searches for all solutions and prints them out.

```clojure
(ns rosettacode.dinesman
  (:use [clojure.core.logic]
        [clojure.tools.macro :as macro]))

; whether x is immediately above (left of) y in list s; uses pattern matching on s
(defne aboveo [x y s]
       ([_ _ (x y . ?rest)])
       ([_ _ [_ . ?rest]] (aboveo x y ?rest)))

; whether x is on a higher floor than y
(defne highero [x y s]
       ([_ _ (x . ?rest)] (membero y ?rest))
       ([_ _ (_ . ?rest)] (highero x y ?rest)))

; whether x and y are on nonadjacent floors
(defn nonadjacento [x y s]
  (conda
    ((aboveo x y s) fail)
    ((aboveo y x s) fail)
    (succeed)))

(defn dinesmano [rs]
  (macro/symbol-macrolet [_ (lvar)]
    (all
      (permuteo ['Baker 'Cooper 'Fletcher 'Miller 'Smith] rs)
      (aboveo _ 'Baker rs) ;someone lives above Baker
      (aboveo 'Cooper _ rs) ;Cooper lives above someone
      (aboveo 'Fletcher _ rs)
      (aboveo _ 'Fletcher rs)
      (highero 'Miller 'Cooper rs)
      (nonadjacento 'Smith 'Fletcher rs)
      (nonadjacento 'Fletcher 'Cooper rs))))

(let [solns (run* [q] (dinesmano q))]
  (println "solution count:" (count solns))
  (println "solution(s) highest to lowest floor:")
  (doseq [soln solns] (println " " soln)))

```

```txt
solution count: 1
solution(s) highest to lowest floor:
  (Miller Fletcher Baker Cooper Smith)
```



## Crystal

This example modifies the Enumerable(T) mixin and adds a method index! that requires each index not to be nil.


```Ruby
module Enumerable(T)
    def index!(element)
        index(element).not_nil!
    end
end

residents = [:Baker, :Cooper, :Fletcher, :Miller, :Smith]

predicates = [
    ->(p : Array(Symbol)){ :Baker != p.last },
    ->(p : Array(Symbol)){ :Cooper != p.first },
    ->(p : Array(Symbol)){ :Fletcher != p.first && :Fletcher != p.last },
    ->(p : Array(Symbol)){ p.index!(:Miller) > p.index!(:Cooper) },
    ->(p : Array(Symbol)){ (p.index!(:Smith) - p.index!(:Fletcher)).abs != 1 },
    ->(p : Array(Symbol)){ (p.index!(:Cooper) - p.index!(:Fletcher)).abs != 1}
]

puts residents.permutations.find { |p| predicates.all? &.call p }
```



## D



{{incorrect|D|

 The output is incorrect:


it has Fletcher on the bottom floor,

Baker on the top,

and Cooper and Fletcher adjacent.

}}


This code uses the second lazy permutations function of '''[[Permutations#Lazy_version]]'''.

As for flexibility: the solve code works with an arbitrary number of people and predicates.

```d
import std.stdio, std.math, std.algorithm, std.traits, permutations2;

void main() {
    enum Names { Baker, Cooper, Fletcher, Miller, Smith }

    immutable(bool function(in Names[]) pure nothrow)[] predicates = [
        s => s[Names.Baker] != s.length - 1,
        s => s[Names.Cooper] != 0,
        s => s[Names.Fletcher] != 0 && s[Names.Fletcher] != s.length-1,
        s => s[Names.Miller] > s[Names.Cooper],
        s => abs(s[Names.Smith] - s[Names.Fletcher]) != 1,
        s => abs(s[Names.Cooper] - s[Names.Fletcher]) != 1];

    permutations([EnumMembers!Names])
    .filter!(solution => predicates.all!(pred => pred(solution)))
    .writeln;
}
```

```txt
[[Fletcher, Cooper, Miller, Smith, Baker]]
```



### Simpler Version


```d
void main() {
    import std.stdio, std.math, std.algorithm, permutations2;

    ["Baker", "Cooper", "Fletcher", "Miller", "Smith"]
    .permutations
    .filter!(s =>
        s.countUntil("Baker") != 4 && s.countUntil("Cooper") &&
        s.countUntil("Fletcher") && s.countUntil("Fletcher") != 4 &&
        s.countUntil("Miller") > s.countUntil("Cooper") &&
        abs(s.countUntil("Smith") - s.countUntil("Fletcher")) != 1 &&
        abs(s.countUntil("Cooper") - s.countUntil("Fletcher")) != 1)
    .writeln;
}
```

The output is the same.


## EchoLisp

The problem is solved using the '''amb''' library. The solution separates the constrainst procedure from the solver procedure. The solver does not depend on names, number of floors. This flexibility allows to easily add floors, names, constraints. See Antoinette example below, Antoinette is very close ❤️ to Cooper, and wants a prime numbered floor.
===Setup - Solver===

```scheme

(require 'hash)
(require' amb)

;;
;; Solver
;;

(define (dwelling-puzzle context names floors H)
;; each amb calls gives a floor to a name
    (for ((name names))
		(hash-set H name (amb context floors)))
;; They live on different floors.
    (amb-require (distinct? (amb-choices context)))
    (constraints floors H) ;; may fail and backtrack
;; result returned to amb-run
    (for/list  ((name names))
    	(cons name (hash-ref H name)))
;; (amb-fail) is possible here to see all solutions
)

(define (task names)
	(amb-run dwelling-puzzle
	(amb-make-context)
	 names
	(iota (length names)) ;; list of floors : 0,1, ....
	(make-hash)) ;; hash table : "name" -> floor
	)

```

=== Problem data - constraints ===

```scheme

(define names '("baker" "cooper" "fletcher" "miller" "smith" ))

(define-syntax-rule (floor name) (hash-ref H name))
(define-syntax-rule (touch a b) (= (abs (- (hash-ref H a) (hash-ref H b))) 1))

(define (constraints floors H)
(define top (1- (length floors)))
    ;; Baker does not live on the top floor.
    (amb-require (!=  (floor "baker")  top))
    ;; Cooper does not live on the bottom floor.
    (amb-require (!=  (floor "cooper") 0))
    ;; Fletcher does not live on either the top or the bottom floor.
    (amb-require (!= (floor "fletcher") top))
    (amb-require (!= (floor "fletcher") 0))
    ;; Miller lives on a higher floor than does Cooper.
    (amb-require (> (floor "miller") (floor "cooper")))
     ;; Smith does not live on a floor adjacent to Fletcher's.
    (amb-require (not (touch "smith" "fletcher")))
    ;; Fletcher does not live on a floor adjacent to Cooper's.
    (amb-require (not (touch "fletcher" "cooper")))
)

```

```scheme

(task names)
→ ((baker . 2) (cooper . 1) (fletcher . 3) (miller . 4) (smith . 0))

```

=== Changing data - constraints ===

```scheme

;; add a  name/floor
(define names '("baker" "cooper" "fletcher" "miller" "smith"  "antoinette"))

(define (constraints floors H)
;; ... same as above, add the following

     ;; Antoinette does not like 💔 Smith
     (amb-require (not (touch "smith" "antoinette")))
    ;; Antoinette is very close  ❤️ to Cooper
     (amb-require (touch "cooper" "antoinette"))
    ;; Antoinette wants a prime numbered floor
     (amb-require (prime? (floor "antoinette")))
)


```

```scheme

(task names)
   → ((baker . 0) (cooper . 1) (fletcher . 3) (miller . 4) (smith . 5) (antoinette . 2))

```




## Elixir

'''Simple solution:'''

```elixir
defmodule Dinesman do
  def problem do
    names = ~w( Baker Cooper Fletcher Miller Smith )a
    predicates = [fn(c)-> :Baker != List.last(c) end,
                  fn(c)-> :Cooper != List.first(c) end,
                  fn(c)-> :Fletcher != List.first(c) && :Fletcher != List.last(c) end,
                  fn(c)-> floor(c, :Miller) > floor(c, :Cooper) end,
                  fn(c)-> abs(floor(c, :Smith) - floor(c, :Fletcher)) != 1 end,
                  fn(c)-> abs(floor(c, :Cooper) - floor(c, :Fletcher)) != 1 end]

    permutation(names)
    |> Enum.filter(fn candidate ->
         Enum.all?(predicates, fn predicate -> predicate.(candidate) end)
       end)
    |> Enum.each(fn name_list ->
         Enum.with_index(name_list)
         |> Enum.each(fn {name,i} -> IO.puts "#{name} lives on #{i+1}" end)
       end)
  end

  defp floor(c, name), do: Enum.find_index(c, fn x -> x == name end)

  defp permutation([]), do: [[]]
  defp permutation(list), do: (for x <- list, y <- permutation(list -- [x]), do: [x|y])
end

Dinesman.problem
```


```txt

Smith lives on 1
Cooper lives on 2
Baker lives on 3
Fletcher lives on 4
Miller lives on 5

```



## Erlang

The people is an argument list.
The rules is an argument list of options.
Only rules that have a function in the program can be in the options.
The design of the rules can be argued.
Perhaps {cooper,  does_not_live_on, 0}, etc, would be better for people unfamiliar with lisp.

```Erlang

-module( dinesman_multiple_dwelling ).

-export( [solve/2, task/0] ).

solve( All_persons, Rules ) ->
    [house(Bottom_floor, B, C, D, Top_floor) || Bottom_floor <- All_persons, B <- All_persons, C <- All_persons, D <- All_persons, Top_floor <- All_persons,
	lists:all( fun (Fun) ->	Fun( house(Bottom_floor, B, C, D, Top_floor) ) end, rules( Rules ))].

task() ->
    All_persons = [baker, cooper, fletcher, miller, smith],
    Rules = [all_on_different_floors, {not_lives_on_floor, 4, baker}, {not_lives_on_floor, 0, cooper}, {not_lives_on_floor, 4, fletcher}, {not_lives_on_floor, 0, fletcher},
          {on_higher_floor, miller, cooper}, {not_adjacent, smith, fletcher}, {not_adjacent, fletcher, cooper}],
    [House] = solve( All_persons, Rules ),
    [io:fwrite("~p lives on floor ~p~n", [lists:nth(X,	House),	X - 1]) || X <- lists:seq(1,5)].



house( A, B, C, D, E ) -> [A, B, C, D, E].

is_all_on_different_floors( [A, B, C, D, E] ) ->
        A =/= B andalso A =/= C andalso A =/= D andalso A =/= E
        andalso B =/= C andalso B =/= D andalso B =/= E
        andalso C =/= D andalso C =/= E
        andalso D =/= E.

is_not_adjacent( Person1, Person2, House ) ->
        is_not_below( Person1, Person2, House ) andalso is_not_below( Person2, Person1, House ).

is_not_below( _Person1, _Person2, [_Person] ) -> true;
is_not_below( Person1, Person2, [Person1, Person2 | _T] ) -> false;
is_not_below( Person1, Person2, [_Person | T] ) -> is_not_below( Person1, Person2, T ).

is_on_higher_floor( Person1, _Person2, [Person1 | _T] ) -> false;
is_on_higher_floor( _Person1, Person2, [Person2 | _T] ) -> true;
is_on_higher_floor( Person1, Person2, [_Person | T] ) -> is_on_higher_floor( Person1, Person2, T ).

rules( Rules ) -> lists:map( fun rules_fun/1, Rules ).

rules_fun( all_on_different_floors ) -> fun is_all_on_different_floors/1;
rules_fun( {not_lives_on_floor, N, Person} ) -> fun (House) -> Person =/= lists:nth(N + 1, House) end;
rules_fun( {on_higher_floor, Person1, Person2} ) -> fun (House) -> is_on_higher_floor( Person1, Person2, House ) end;
rules_fun( {not_below, Person1, Person2} ) -> fun (House) -> is_not_below( Person1, Person2, House ) end;
rules_fun( {not_adjacent, Person1, Person2} ) -> fun (House) ->	is_not_adjacent( Person1, Person2, House ) end.

```

```txt

8> dinesman_multiple_dwelling:task().
 smith lives on floor 0
 cooper lives on floor 1
 baker lives on floor 2
 fletcher lives on floor 3
 miller lives on floor 4

```



## ERRE


```ERRE
PROGRAM DINESMAN

BEGIN
      ! Floors are numbered 0 (ground) to 4 (top)

      ! "Baker, Cooper, Fletcher, Miller, and Smith live on different floors":
      stmt1$="Baker<>Cooper AND Baker<>Fletcher AND Baker<>Miller AND "+"Baker<>Smith AND Cooper<>Fletcher AND Cooper<>Miller AND "+"Cooper<>Smith AND Fletcher<>Miller AND Fletcher<>Smith AND "+"Miller<>Smith"

      ! "Baker does not live on the top floor":
      stmt2$="Baker<>4"

      ! "Cooper does not live on the bottom floor":
      stmt3$="Cooper<>0"

      ! "Fletcher does not live on either the top or the bottom floor":
      stmt4$="Fletcher<>0 AND Fletcher<>4"

      ! "Miller lives on a higher floor than does Cooper":
      stmt5$="Miller>Cooper"

      ! "Smith does not live on a floor adjacent to Fletcher's":
      stmt6$="ABS(Smith-Fletcher)<>1"

      ! "Fletcher does not live on a floor adjacent to Cooper's":
      stmt7$="ABS(Fletcher-Cooper)<>1"

      FOR Baker=0 TO 4 DO
        FOR Cooper=0 TO 4 DO
          FOR Fletcher=0 TO 4 DO
            FOR Miller=0 TO 4 DO
              FOR Smith=0 TO 4 DO
                IF Baker<>4 AND Cooper<>0 AND Miller>Cooper THEN
                  IF Fletcher<>0 AND Fletcher<>4 AND ABS(Smith-Fletcher)<>1 AND ABS(Fletcher-Cooper)<>1 THEN
                    IF Baker<>Cooper AND Baker<>Fletcher AND Baker<>Miller AND Baker<>Smith AND Cooper<>Fletcher AND Cooper<>Miller AND Cooper<>Smith AND Fletcher<>Miller AND Fletcher<>Smith AND Miller<>Smith THEN
                      PRINT("Baker lives on floor ";Baker)
                      PRINT("Cooper lives on floor ";Cooper)
                      PRINT("Fletcher lives on floor ";Fletcher)
                      PRINT("Miller lives on floor ";Miller)
                      PRINT("Smith lives on floor ";Smith)
                    END IF
                  END IF
                END IF
              END FOR !  Smith
            END FOR !  Miller
          END FOR !  Fletcher
        END FOR !  Cooper
      END FOR !  Baker
END PROGRAM
```

```txt

Baker lives on floor  2
Cooper lives on floor  1
Fletcher lives on floor  3
Miller lives on floor  4
Smith lives on floor  0

```



## Factor

All rules are encoded in the ``meets-constraints?`` word. Any variations to the rules requires modifying ``meets-constraints?``

```factor
USING: kernel
    combinators.short-circuit
    math math.combinatorics math.ranges
    sequences
    qw prettyprint ;
IN: rosetta.dinesman

: /= ( x y -- ? ) = not ;
: fifth ( seq -- elt ) 4 swap nth ;

: meets-constraints? ( seq -- ? )
    {
        [ first 5 /= ]                          ! Baker does not live on the top floor.
        [ second 1 /= ]                         ! Cooper does not live on the bottom floor.
        [ third { 1 5 } member? not ]           ! Fletcher does not live on either the top or bottom floor.
        [ [ fourth ] [ second ] bi > ]          ! Miller lives on a higher floor than does Cooper.
        [ [ fifth ] [ third ] bi - abs 1 /= ]   ! Smith does not live on a floor adjacent to Fletcher's.
        [ [ third ] [ second ] bi - abs 1 /= ]  ! Fletcher does not live on a floor adjacent to Cooper's.
    } 1&& ;

: solutions ( -- seq )
    5 [1,b] all-permutations [ meets-constraints? ] filter ;

: >names ( seq -- seq )
    [ 1 - qw{ baker cooper fletcher miller smith } nth ] map ;

: dinesman ( -- )
    solutions [ >names . ] each ;
```

```txt
{ "fletcher" "cooper" "miller" "smith" "baker" }
```


## Forth

This solution takes advantage of several of Forth's strengths. Forth is able to picture a number in any base from 2 to 36.

This program simply iterates through all numbers between 01234 and 43210 (base 5). To see whether this is a permutation worth testing, a binary mask is generated. If all 5 bits are set (31 decimal), this is a possible candidate. Then all ASCII digits of the generated number are converted back to numbers by subtracting the value of ASCII "0". Finally each of the conditions is tested.

All conditions are confined to a single word. The algorithm "as is" will work up to 10 floors. After that, we have to take into consideration that characters A-Z are used as digits. That will work up to 36 floors.

Although this is not ANS Forth, one should have little trouble converting it.
```forth
  0 enum baker                         \ enumeration of all tenants
    enum cooper
    enum fletcher
    enum miller
constant smith

create names                           \ names of all the tenants
  ," Baker"
  ," Cooper"
  ," Fletcher"
  ," Miller"
  ," Smith"                            \ get name, type it
does> swap cells + @c count type ."  lives in " ;

        5 constant #floor              \ number of floors
#floor 1- constant top                 \ top floor
        0 constant bottom              \ we're counting the floors from 0

: num@ c@ [char] 0 - ;                 ( a -- n)
: floor chars over + num@ ;            ( a n1 -- a n2)
                                       \ is it a valid permutation?
: perm?                                ( n -- a f)
  #floor base ! 0 swap s>d <# #floor 0 ?do # loop #>
  over >r bounds do 1 i num@ lshift + loop
  31 = r> swap decimal                 \ create binary mask and check
;
                                       \ test a solution
: solution?                            ( a -- a f)
  baker floor top <>                   \ baker on top floor?
  if cooper floor bottom <>            \ cooper on the bottom floor?
     if fletcher floor dup bottom <> swap top <> and
        if cooper floor swap miller floor rot >
           if smith floor swap fletcher floor rot - abs 1 <>
              if cooper floor swap fletcher floor rot - abs 1 <>
                 if true exit then     \ we found a solution!
              then
           then
        then
     then
  then false                           \ nice try, no cigar..
;
                                       ( a --)
: .solution #floor 0 do i names i chars over + c@ 1+ emit cr loop drop ;
                                       \ main routine
: dinesman                             ( --)
  2932 194 do
    i perm? if solution? if .solution leave else drop then else drop then
  loop
;                                      \ show the solution

dinesman
```

```txt

Baker lives in 3
Cooper lives in 2
Fletcher lives in 4
Miller lives in 5
Smith lives in 1

```



## Go


```go
package main

import "fmt"

// The program here is restricted to finding assignments of tenants (or more
// generally variables with distinct names) to floors (or more generally
// integer values.)  It finds a solution assigning all tenants and assigning
// them to different floors.

// Change number and names of tenants here.  Adding or removing names is
// allowed but the names should be distinct; the code is not written to handle
// duplicate names.
var tenants = []string{"Baker", "Cooper", "Fletcher", "Miller", "Smith"}

// Change the range of floors here.  The bottom floor does not have to be 1.
// These should remain non-negative integers though.
const bottom = 1
const top = 5

// A type definition for readability.  Do not change.
type assignments map[string]int

// Change rules defining the problem here.  Change, add, or remove rules as
// desired.  Each rule should first be commented as human readable text, then
// coded as a function.  The function takes a tentative partial list of
// assignments of tenants to floors and is free to compute anything it wants
// with this information.  Other information available to the function are
// package level defintions, such as top and bottom.  A function returns false
// to say the assignments are invalid.
var rules = []func(assignments) bool{
    // Baker does not live on the top floor
    func(a assignments) bool {
        floor, assigned := a["Baker"]
        return !assigned || floor != top
    },
    // Cooper does not live on the bottom floor
    func(a assignments) bool {
        floor, assigned := a["Cooper"]
        return !assigned || floor != bottom
    },
    // Fletcher does not live on either the top or the bottom floor
    func(a assignments) bool {
        floor, assigned := a["Fletcher"]
        return !assigned || (floor != top && floor != bottom)
    },
    // Miller lives on a higher floor than does Cooper
    func(a assignments) bool {
        if m, assigned := a["Miller"]; assigned {
            c, assigned := a["Cooper"]
            return !assigned || m > c
        }
        return true
    },
    // Smith does not live on a floor adjacent to Fletcher's
    func(a assignments) bool {
        if s, assigned := a["Smith"]; assigned {
            if f, assigned := a["Fletcher"]; assigned {
                d := s - f
                return d*d > 1
            }
        }
        return true
    },
    // Fletcher does not live on a floor adjacent to Cooper's
    func(a assignments) bool {
        if f, assigned := a["Fletcher"]; assigned {
            if c, assigned := a["Cooper"]; assigned {
                d := f - c
                return d*d > 1
            }
        }
        return true
    },
}

// Assignment program, do not change.  The algorithm is a depth first search,
// tentatively assigning each tenant in order, and for each tenant trying each
// unassigned floor in order.  For each tentative assignment, it evaluates all
// rules in the rules list and backtracks as soon as any one of them fails.
//
// This algorithm ensures that the tenative assignments have only names in the
// tenants list, only floor numbers from bottom to top, and that tentants are
// assigned to different floors.  These rules are hard coded here and do not
// need to be coded in the the rules list above.
func main() {
    a := assignments{}
    var occ [top + 1]bool
    var df func([]string) bool
    df = func(u []string) bool {
        if len(u) == 0 {
            return true
        }
        tn := u[0]
        u = u[1:]
    f:
        for f := bottom; f <= top; f++ {
            if !occ[f] {
                a[tn] = f
                for _, r := range rules {
                    if !r(a) {
                        delete(a, tn)
                        continue f
                    }
                }
                occ[f] = true
                if df(u) {
                    return true
                }
                occ[f] = false
                delete(a, tn)
            }
        }
        return false
    }
    if !df(tenants) {
        fmt.Println("no solution")
        return
    }
    for t, f := range a {
        fmt.Println(t, f)
    }
}
```

```txt

Baker 3
Cooper 2
Fletcher 4
Miller 5
Smith 1

```



## Haskell

The List monad is perfect for this kind of problem. One can express the problem statements in a very natural and concise way:
```haskell
import Data.List (permutations)
import Control.Monad (guard)

dinesman :: [(Int,Int,Int,Int,Int)]
dinesman = do
  -- baker, cooper, fletcher, miller, smith are integers representing
  -- the floor that each person lives on, from 1 to 5

  -- Baker, Cooper, Fletcher, Miller, and Smith live on different floors
  -- of an apartment house that contains only five floors.
  [baker, cooper, fletcher, miller, smith] <- permutations [1..5]

  -- Baker does not live on the top floor.
  guard $ baker /= 5

  -- Cooper does not live on the bottom floor.
  guard $ cooper /= 1

  -- Fletcher does not live on either the top or the bottom floor.
  guard $ fletcher /= 5 && fletcher /= 1

  -- Miller lives on a higher floor than does Cooper.
  guard $ miller > cooper

  -- Smith does not live on a floor adjacent to Fletcher's.
  guard $ abs (smith - fletcher) /= 1

  -- Fletcher does not live on a floor adjacent to Cooper's.
  guard $ abs (fletcher - cooper) /= 1

  -- Where does everyone live?
  return (baker, cooper, fletcher, miller, smith)

main :: IO ()
main = do
  print $ head dinesman -- print first solution: (3,2,4,5,1)
  print dinesman -- print all solutions (only one): [(3,2,4,5,1)]
```


Or as a list comprehension:

```haskell
import Data.List (permutations)

main :: IO ()
main =
  print
    [ ( "Baker lives on " ++ show b
      , "Cooper lives on " ++ show c
      , "Fletcher lives on " ++ show f
      , "Miller lives on " ++ show m
      , "Smith lives on " ++ show s)
    | [b, c, f, m, s] <- permutations [1 .. 5]
    , b /= 5
    , c /= 1
    , f /= 1
    , f /= 5
    , m > c
    , abs (s - f) > 1
    , abs (c - f) > 1 ]
```

```txt
[("Baker lives on 3","Cooper lives on 2","Fletcher lives on 4","Miller lives on 5","Smith lives on 1")]
```


=={{header|Icon}} and {{header|Unicon}}==
This solution uses string invocation to call operators and the fact the Icon/Unicon procedures are first class values.  The procedure names could also be given as strings and it would be fairly simple to read the names and all the rules directly from a file. Each name and rule recurses and relies on the inherent backtracking in the language to achieve the goal.

The rules explicitly call stop() after showing the solution.  Removing the ''stop'' would cause the solver to try all possible cases and report all possible solutions (if there were multiple ones).


```Icon
invocable all
global nameL, nameT, rules

procedure main() # Dinesman

nameT := table()
nameL := ["Baker", "Cooper", "Fletcher", "Miller", "Smith"]
rules := [ [ distinct ],
           [ "~=",        "Baker",    top()      ],
           [ "~=",        "Cooper",   bottom()   ],
           [ "~=",        "Fletcher", top()      ],
           [ "~=",        "Fletcher", bottom()   ],
           [ ">",         "Miller",   "Cooper"   ],
           [ notadjacent, "Smith",    "Fletcher" ],
           [ notadjacent, "Fletcher", "Cooper"   ],
           [ showsolution ],
           [ stop ] ]

if not solve(1) then
   write("No solution found.")
end

procedure dontstop()           # use if you want to search for all solutions
end

procedure showsolution()       # show the soluton
   write("The solution is:")
   every write("   ",n := !nameL, " lives in ", nameT[n])
   return
end

procedure eval(n)              # evaluate a rule
   r := copy(rules[n-top()])
   every r[i := 2 to *r] := rv(r[i])
   if get(r)!r then suspend
end

procedure rv(x)                # return referenced value if it exists
return \nameT[x] | x
end

procedure solve(n)             # recursive solver
   if n > top() then {         # apply rules
      if n <= top() + *rules then
         ( eval(n) & solve(n+1) ) | fail
      }
   else                        # setup locations
      (( nameT[nameL[n]] := bottom() to top() ) & solve(n + 1)) | fail
   return
end

procedure distinct(a,b)        # ensure each name is distinct
   if nameT[n := !nameL] = nameT[n ~== key(nameT)] then fail
   suspend
end

procedure notadjacent(n1,n2)   # ensure n1,2 are not adjacent
   if not adjacent(n1,n2) then suspend
end

procedure adjacent(n1,n2)      # ensure n1,2 are adjacent
   if abs(n1 - n2) = 1 then suspend
end

procedure bottom()             # return bottom
   return if *nameL > 0 then 1 else 0
end

procedure top()                # return top
   return *nameL
end
```


```txt
The solution is:
   Baker lives in 3
   Cooper lives in 2
   Fletcher lives in 4
   Miller lives in 5
   Smith lives in 1
```



## J


This problem asks us to pick from one of several possibilities.
We can represent these possibilities as permutations of the residents' initials, arranged in order from lowest floor to top floor:


```j
possible=: ((i.!5) A. i.5) { 'BCFMS'
```


Additionally, we are given a variety of constraints which eliminate some possibilities:


```j
possible=: (#~ 'B' ~: {:"1) possible         NB. Baker not on top floor
possible=: (#~ 'C' ~: {."1) possible         NB. Cooper not on bottom floor
possible=: (#~ 'F' ~: {:"1) possible         NB. Fletcher not on top floor
possible=: (#~ 'F' ~: {."1) possible         NB. Fletcher not on bottom floor
possible=: (#~ </@i."1&'CM') possible        NB. Miller on higher floor than Cooper
possible=: (#~ 0 = +/@E."1~&'SF') possible   NB. Smith not immediately below Fletcher
possible=: (#~ 0 = +/@E."1~&'FS') possible   NB. Fletcher not immediately below Smith
possible=: (#~ 0 = +/@E."1~&'CF') possible   NB. Cooper not immediately below Fletcher
possible=: (#~ 0 = +/@E."1~&'FC') possible   NB. Fletcher not immediately below Cooper
```


The answer is thus:


```j
   possible
SCBFM
```


(bottom floor) Smith, Cooper, Baker, Fletcher, Miller (top floor)


## Java

'''Code:'''


```java
import java.util.*;

class DinesmanMultipleDwelling {

    private static void generatePermutations(String[] apartmentDwellers, Set<String> set, String curPermutation) {
        for (String s : apartmentDwellers) {
            if (!curPermutation.contains(s)) {
                String nextPermutation = curPermutation + s;
                if (nextPermutation.length() == apartmentDwellers.length) {
                    set.add(nextPermutation);
                } else {
                    generatePermutations(apartmentDwellers, set, nextPermutation);
                }
            }
        }
    }

    private static boolean topFloor(String permutation, String person) { //Checks to see if the person is on the top floor
        return permutation.endsWith(person);
    }

    private static boolean bottomFloor(String permutation, String person) {//Checks to see if the person is on the bottom floor
        return permutation.startsWith(person);
    }

    public static boolean livesAbove(String permutation, String upperPerson, String lowerPerson) {//Checks to see if the person lives above the other person
        return permutation.indexOf(upperPerson) > permutation.indexOf(lowerPerson);
    }

    public static boolean adjacent(String permutation, String person1, String person2) { //checks to see if person1 is adjacent to person2
        return (Math.abs(permutation.indexOf(person1) - permutation.indexOf(person2)) == 1);
    }

    private static boolean isPossible(String s) {
        /*
         What this does should be obvious...proper explaination can be given if needed
         Conditions here Switching any of these to ! or reverse will change what is given as a result

         example
         if(topFloor(s, "B"){
         }
         to
         if(!topFloor(s, "B"){
         }
         or the opposite
         if(!topFloor(s, "B"){
         }
         to
         if(topFloor(s, "B"){
         }
         */
        if (topFloor(s, "B")) {//B is on Top Floor
            return false;
        }
        if (bottomFloor(s, "C")) {//C is on Bottom Floor
            return false;
        }
        if (topFloor(s, "F") || bottomFloor(s, "F")) {// F is on top or bottom floor
            return false;
        }
        if (!livesAbove(s, "M", "C")) {// M does not live above C
            return false;
        }
        if (adjacent(s, "S", "F")) { //S lives adjacent to F
            return false;
        }
        return !adjacent(s, "F", "C"); //F does not live adjacent to C
    }

    public static void main(String[] args) {
        Set<String> set = new HashSet<String>();
        generatePermutations(new String[]{"B", "C", "F", "M", "S"}, set, ""); //Generates Permutations
        for (Iterator<String> iterator = set.iterator(); iterator.hasNext();) {//Loops through iterator
            String permutation = iterator.next();
            if (!isPossible(permutation)) {//checks to see if permutation is false if so it removes it
                iterator.remove();
            }
        }
        for (String s : set) {
            System.out.println("Possible arrangement: " + s);
            /*
            Prints out possible arranagement...changes depending on what you change in the "isPossible method"
             */
        }
    }
}

```


```txt
Possible arrangement: SCBFM
```



## JavaScript



### ES6



### =More flexibility=


(Full occupancy and no cohabitation included in the predicate)

The generality of nesting '''concatMap''', and returning values enclosed in a list (empty where the test fails, populated otherwise), is the same as that of a using a list comprehension, to which it is formally equivalent. (concatMap is the bind operator for the list monad, and '''(a -> [a])''' is the type of the 'return' function for a list monad. The effect is to define a cartesian product, and apply a predicate to each member of that product. Any empty lists returned where a predicate yields ''false'' are eliminated by the concatenation component of concatMap.

The predicates here can be varied, and the depth of concatMap nestings can be adjusted to match the number of unknowns in play, with each concatMap binding one name, and defining the list of its possible values.


```JavaScript
(() => {
    'use strict';

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // and :: [Bool] -> Bool
    const and = xs => {
        let i = xs.length;
        while (i--)
            if (!xs[i]) return false;
        return true;
    }

    // nubBy :: (a -> a -> Bool) -> [a] -> [a]
    const nubBy = (p, xs) => {
        const x = xs.length ? xs[0] : undefined;
        return x !== undefined ? [x].concat(
            nubBy(p, xs.slice(1)
                .filter(y => !p(x, y)))
        ) : [];
    }

    // PROBLEM DECLARATION

    const floors = range(1, 5);

    return  concatMap(b =>
            concatMap(c =>
            concatMap(f =>
            concatMap(m =>
            concatMap(s =>
                and([ // CONDITIONS
                    nubBy((a, b) => a === b, [b, c, f, m, s]) // all floors singly occupied
                    .length === 5,
                    b !== 5, c !== 1, f !== 1, f !== 5,
                    m > c, Math.abs(s - f) > 1, Math.abs(c - f) > 1
                ]) ? [{
                    Baker: b,
                    Cooper: c,
                    Fletcher: f,
                    Miller: m,
                    Smith: s
                }] : [],
                floors), floors), floors), floors), floors);

    // --> [{"Baker":3, "Cooper":2, "Fletcher":4, "Miller":5, "Smith":1}]
})();
```


```JavaScript
[{"Baker":3, "Cooper":2, "Fletcher":4, "Miller":5, "Smith":1}]
```



### =Less flexibility=


For a different trade-off between efficiency and generality, we can take full occupancy and no cohabitation out of the predicate, and assume them in the shape of the search space.

In the version above, with nested applications of concatMap, the requirement that all apartments are occupied by one person only is included in the test conditions.
Alternatively, we can remove any flexibility about such civic virtues from the predicate, and restrict the universe of conceivable living arrangements, by using concatMap just once, and applying it only to the various permutations of full and distinct occupancy.

ES6 splat assignment allows us to bind all five names in a single application of concatMap. We now also need a '''permutations''' function of some kind.


```JavaScript
(() => {
    'use strict';

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // and :: [Bool] -> Bool
    const and = xs => {
        let i = xs.length;
        while (i--)
            if (!xs[i]) return false;
        return true;
    }

    // permutations :: [a] -> [[a]]
    const permutations = xs =>
        xs.length ? concatMap(x => concatMap(ys => [
                [x].concat(ys)
            ],
            permutations(delete_(x, xs))), xs) : [
            []
        ];

    // delete :: a -> [a] -> [a]
    const delete_ = (x, xs) =>
        deleteBy((a, b) => a === b, x, xs);

    // deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
    const deleteBy = (f, x, xs) =>
        xs.reduce((a, y) => f(x, y) ? a : a.concat(y), []);

    // PROBLEM DECLARATION

    const floors = range(1, 5);

    return concatMap(([c, b, f, m, s]) =>
        and([ // CONDITIONS (assuming full occupancy, no cohabitation)
            b !== 5, c !== 1, f !== 1, f !== 5,
            m > c, Math.abs(s - f) > 1, Math.abs(c - f) > 1
        ]) ? [{
            Baker: b,
            Cooper: c,
            Fletcher: f,
            Miller: m,
            Smith: s
        }] : [], permutations(floors));

    // --> [{"Baker":3, "Cooper":2, "Fletcher":4, "Miller":5, "Smith":1}]
})();

```



```JavaScript
[{"Baker":3, "Cooper":2, "Fletcher":4, "Miller":5, "Smith":1}]
```



## jq


Since we are told that "Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house that contains only five floors",
we can represent the apartment house as a JSON array, the first element of which names the occupant of the 1st floor, etc.

The solution presented here does not blindly generate all permutations.  It can be characterized as a constraint-oriented approach.


```jq
# Input: an array representing the apartment house, with null at a
#    particular position signifying that the identity of the occupant
#    there has not yet been determined.
# Output: an elaboration of the input array but including person, and
#   satisfying cond, where . in cond refers to the placement of person
def resides(person; cond):
  range(0;5) as $n
  | if (.[$n] == null or .[$n] == person) and ($n|cond) then .[$n] = person
    else empty   # no elaboration is possible
    end ;

# English:
def top: 4;
def bottom: 0;
def higher(j): . > j;
def adjacent(j): (. - j) | (. == 1 or . == -1);
```

'''Solution''':

```jq
[]
| resides("Baker";  . != top)                     # Baker does not live on the top floor
| resides("Cooper"; . != bottom)                  # Cooper does not live on the bottom floor
| resides("Fletcher"; . != top and . != bottom)   # Fletcher does not live on either the top or the bottom floor.
| index("Cooper") as $Cooper
| resides("Miller"; higher( $Cooper) )            # Miller lives on a higher floor than does Cooper
| index("Fletcher") as $Fletcher
| resides("Smith"; adjacent($Fletcher) | not)     # Smith does not live on a floor adjacent to Fletcher's.
| select( $Fletcher | adjacent( $Cooper ) | not ) # Fletcher does not live on a floor adjacent to Cooper's.
```

'''Out''':

```sh

$ jq -n -f Dinesman.jq
[
  "Smith",
  "Cooper",
  "Baker",
  "Fletcher",
  "Miller"
]
```



## Julia

```julia
using Combinatorics

function solve(n::Vector{<:AbstractString}, pred::Vector{<:Function})
    rst = Vector{typeof(n)}(0)
    for candidate in permutations(n)
        if all(p(candidate) for p in predicates)
            push!(rst, candidate)
        end
    end
    return rst
end

Names = ["Baker", "Cooper", "Fletcher", "Miller", "Smith"]
predicates = [
    (s) -> last(s) != "Baker",
    (s) -> first(s) != "Cooper",
    (s) -> first(s) != "Fletcher" && last(s) != "Fletcher",
    (s) -> findfirst(s, "Miller") > findfirst(s, "Cooper"),
    (s) -> abs(findfirst(s, "Smith") - findfirst(s, "Fletcher")) != 1,
    (s) -> abs(findfirst(s, "Cooper") - findfirst(s, "Fletcher")) != 1]

solutions = solve(Names, predicates)
foreach(x -> println(join(x, ", ")), solutions)
```


```txt
Smith, Cooper, Baker, Fletcher, Miller
```



## K

Tested with Kona.


```k

perm: {x@m@&n=(#?:)'m:!n#n:#x}
filter: {y[& x'y]}
reject: {y[& ~x'y]}
adjacent: {1 = _abs (z?x) - (z?y)}

p: perm[`Baker `Cooper `Fletcher `Miller `Smith]
p: reject[{`Cooper=x[0]}; p]
p: reject[{`Baker=x[4]}; p]
p: filter[{(x ? `Miller) > (x ? `Cooper)}; p]
p: reject[{adjacent[`Smith; `Fletcher; x]}; p]
p: reject[{adjacent[`Cooper; `Fletcher; x]}; p]
p: reject[{(x ? `Fletcher)_in (0 4)}; p]

```

Output:

```txt

`Smith `Cooper `Baker `Fletcher `Miller

```



## Kotlin


```scala
// version 1.1.3

typealias Predicate = (List<String>) -> Boolean

fun <T> permute(input: List<T>): List<List<T>> {
    if (input.size == 1) return listOf(input)
    val perms = mutableListOf<List<T>>()
    val toInsert = input[0]
    for (perm in permute(input.drop(1))) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            perms.add(newPerm)
        }
    }
    return perms
}

/* looks for for all possible solutions, not just the first */
fun dinesman(occupants: List<String>, predicates: List<Predicate>) =
    permute(occupants).filter { perm -> predicates.all { pred -> pred(perm) } }

fun main(args: Array<String>) {
    val occupants = listOf("Baker", "Cooper", "Fletcher", "Miller", "Smith")

    val predicates = listOf<Predicate>(
        { it.last() != "Baker" },
        { it.first() != "Cooper" },
        { it.last() != "Fletcher" && it.first() != "Fletcher" },
        { it.indexOf("Miller") > it.indexOf("Cooper") },
        { Math.abs(it.indexOf("Smith") - it.indexOf("Fletcher")) > 1 },
        { Math.abs(it.indexOf("Fletcher") - it.indexOf("Cooper")) > 1 }
    )

    val solutions = dinesman(occupants, predicates)
    val size = solutions.size
    if (size == 0) {
        println("No solutions found")
    }
    else {
        val plural = if (size == 1) "" else "s"
        println("$size solution$plural found, namely:\n")
        for (solution in solutions) {
            for ((i, name) in solution.withIndex()) {
                println("Floor ${i + 1} -> $name")
            }
            println()
        }
    }
}
```


```txt

1 solution found, namely:

Floor 1 -> Smith
Floor 2 -> Cooper
Floor 3 -> Baker
Floor 4 -> Fletcher
Floor 5 -> Miller

```



## Lua


```lua
local wrap, yield = coroutine.wrap, coroutine.yield
local function perm(n)
    local r = {}
    for i=1,n do r[i]=i end
  return wrap(function()
    local function swap(m)
      if m==0 then
        yield(r)
      else
        for i=m,1,-1 do
          r[i],r[m]=r[m],r[i]
          swap(m-1)
          r[i],r[m]=r[m],r[i]
        end
      end
    end
    swap(n)
  end)
end

local function iden(...)return ... end
local function imap(t,f)
  local r,fn = {m=imap, c=table.concat, u=table.unpack}, f or iden
  for i=1,#t do r[i]=fn(t[i])end
  return r
end

local tenants = {'Baker', 'Cooper', 'Fletcher', 'Miller', 'Smith'}

local conds = {
  'Baker  ~= TOP',
  'Cooper ~= BOTTOM',
  'Fletcher ~= TOP and Fletcher~= BOTTOM',
  'Miller > Cooper',
  'Smith + 1 ~= Fletcher and Smith - 1 ~= Fletcher',
  'Cooper + 1 ~= Fletcher and Cooper - 1 ~= Fletcher',
}

local function makePredicate(conds, tenants)
  return load('return function('..imap(tenants):c','..
    ') return ' ..
    imap(conds,function(c)
      return string.format("(%s)",c)
    end):c"and "..
    " end ",'-',nil,{TOP=5, BOTTOM=1})()
end

local function solve (conds, tenants)
  local try, pred, upk = perm(#tenants), makePredicate(conds, tenants), table.unpack
  local answer = try()
  while answer and not pred(upk(answer)) do answer = try()end
  if answer then
    local floor = 0
    return imap(answer, function(person)
        floor=floor+1;
        return string.format(" %s lives on floor %d",tenants[floor],person)
    end):c"\n"
  else
    return nil, 'no solution'
  end
end

print(solve (conds, tenants))
```

```txt
 Baker lives on floor 3
 Cooper lives on floor 2
 Fletcher lives on floor 4
 Miller lives on floor 5
 Smith lives on floor 1
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Loads all names into memory as variables, then asserts various restrictions on them before trying to resolve them by assuming that they're integers. This works by assuming that the names are the floors the people are on. This method is slow but direct.

```Mathematica

{Baker, Cooper, Fletcher, Miller, Smith};
(Unequal @@ %) && (And @@ (0 < # < 6 & /@ %)) &&
  Baker < 5 &&
  Cooper > 1 &&
  1 < Fletcher < 5 &&
  Miller > Cooper &&
  Abs[Smith - Fletcher] > 1 &&
  Abs[Cooper - Fletcher] > 1 //
 Reduce[#, %, Integers] &

```

```Mathematica
Baker == 3 && Cooper == 2 && Fletcher == 4 && Miller == 5 && Smith == 1
```



### Alternate Version

A much quicker and traditional method. This generates all permutations of a list containing the five names as strings. The list of permutations is then filtered using the restrictions given in the problem until only one permutation is left.


```Mathematica

p = Position[#1, #2][[1, 1]] &;
Permutations[{"Baker", "Cooper", "Fletcher", "Miller", "Smith"}, {5}];
Select[%, #[[5]] != "Baker" &];
Select[%, #[[1]] != "Cooper" &];
Select[%, #[[1]] != "Fletcher" && #[[5]] != "Fletcher" &];
Select[%, #~p~"Miller" > #~p~"Cooper" &];
Select[%, Abs[#~p~"Smith" - #~p~"Fletcher"] > 1 &];
Select[%, Abs[#~p~"Cooper" - #~p~"Fletcher"] > 1 &]

```


```Mathematica
```



## Perl


A solution that parses a structured version of the problem text, translates it into a Perl expression, and uses it for a brute-force search:

'''Setup'''


```perl
use strict;
use warnings;
use feature qw(state say);
use List::Util 1.33 qw(pairmap);
use Algorithm::Permute qw(permute);

our %predicates = (
#                       | object    | sprintf format for Perl expression |
#   --------------------+-----------+------------------------------------+
    'on bottom'      => [ ''        , '$f[%s] == 1'                      ],
    'on top'         => [ ''        , '$f[%s] == @f'                     ],
    'lower than'     => [ 'person'  , '$f[%s] < $f[%s]'                  ],
    'higher than'    => [ 'person'  , '$f[%s] > $f[%s]'                  ],
    'directly below' => [ 'person'  , '$f[%s] == $f[%s] - 1'             ],
    'directly above' => [ 'person'  , '$f[%s] == $f[%s] + 1'             ],
    'adjacent to'    => [ 'person'  , 'abs($f[%s] - $f[%s]) == 1'        ],
    'on'             => [ 'ordinal' , '$f[%s] == \'%s\''                 ],
);

our %nouns = (
    'person'  => qr/[a-z]+/i,
    'ordinal' => qr/1st | 2nd | 3rd | \d+th/x,
);

sub parse_and_solve {
    my @facts = @_;

    state $parser = qr/^(?<subj>$nouns{person}) (?<not>not )?(?|@{[
                            join '|', pairmap {
                                "(?<pred>$a)" .
                                ($b->[0] ? " (?<obj>$nouns{$b->[0]})" : '')
                            } %predicates
                        ]})$/;

    my (@expressions, %ids, $i);
    my $id = sub { defined $_[0] ? $ids{$_[0]} //= $i++ : () };

    foreach (@facts) {
        /$parser/ or die "Cannot parse '$_'\n";

        my $pred = $predicates{$+{pred}};
        my $expr = '(' . sprintf($pred->[1], $id->($+{subj}),
                         $pred->[0] eq 'person' ? $id->($+{obj}) : $+{obj}). ')';
        $expr = '!' . $expr if $+{not};

        push @expressions, $expr;
    }

    my @f = 1..$i;
    eval 'no warnings "numeric";
          permute {
              say join(", ", pairmap { "$f[$b] $a" } %ids)
                  if ('.join(' && ', @expressions).');
          } @f;';
}
```


Note that it can easily be extended by modifying the <code>%predicates</code> and <code>%nouns</code> hashes at the top.

'''Problem statement'''

Since trying to extract information from free-form text feels a little too flaky, the problem statement is instead expected as structured text with one fact per line, each of them having one of these two forms:
* <code>&lt;name&gt; &lt;position&gt;</code>
* <code>&lt;name&gt; not &lt;position&gt;</code>
...where <code>&lt;position&gt;</code> can be any of:
* <code>on bottom</code>
* <code>on top</code>
* <code>lower than &lt;name&gt;</code>
* <code>higher than &lt;name&gt;</code>
* <code>directly below &lt;name&gt;</code>
* <code>directly above &lt;name&gt;</code>
* <code>adjacent to &lt;name&gt;</code>
* <code>on &lt;numeral&gt;</code>  (e.g. 1st, 2nd, etc.)

It is assumed that there are as many floors as there are different names.

Thus, the problem statement from the task description translates to:


```perl
parse_and_solve(<DATA>);

__DATA__
Baker not on top
Cooper not on bottom
Fletcher not on top
Fletcher not on bottom
Miller higher than Cooper
Smith not adjacent to Fletcher
Fletcher not adjacent to Cooper
```


```txt

2 Cooper, 5 Miller, 3 Baker, 1 Smith, 4 Fletcher

```


When there are multiple matching configurations,
it lists them all (on separate lines).


## Perl 6


### By parsing the problem


```perl6
use MONKEY-SEE-NO-EVAL;

sub parse_and_solve ($text) {
    my %ids;
    my $expr = (grammar {
        state $c = 0;
        rule TOP { <fact>+ { make join ' && ', $<fact>>>.made } }

        rule fact { <name> (not)? <position>
                    { make sprintf $<position>.made.fmt($0 ??  "!(%s)" !! "%s"),
                                   $<name>.made }
        }
        rule position {
            || on bottom             { make "\@f[%s] == 1"                            }
            || on top                { make "\@f[%s] == +\@f"                         }
            || lower than <name>     { make "\@f[%s] < \@f[{$<name>.made}]"           }
            || higher than <name>    { make "\@f[%s] > \@f[{$<name>.made}]"           }
            || directly below <name> { make "\@f[%s] == \@f[{$<name>.made}] - 1"      }
            || directly above <name> { make "\@f[%s] == \@f[{$<name>.made}] + 1"      }
            || adjacent to <name>    { make "\@f[%s] == \@f[{$<name>.made}] + (-1|1)" }
            || on <ordinal>          { make "\@f[%s] == {$<ordinal>.made}"            }
            || { note "Failed to parse line " ~ +$/.prematch.comb(/^^/); exit 1; }
        }

        token name    { :i <[a..z]>+              { make %ids{~$/} //= $c++ } }
        token ordinal { [1st | 2nd | 3rd | \d+th] { make +$/.match(/(\d+)/)[0]     } }
    }).parse($text).made;

    EVAL 'for [1..%ids.elems].permutations -> @f {
              say %ids.kv.map({ "$^a=@f[$^b]" }) if (' ~ $expr ~ ');
          }'
}

parse_and_solve Q:to/END/;
    Baker not on top
    Cooper not on bottom
    Fletcher not on top
    Fletcher not on bottom
    Miller higher than Cooper
    Smith not adjacent to Fletcher
    Fletcher not adjacent to Cooper
    END
```


Supports the same grammar for the problem statement, as the Perl solution.

```txt

Baker=3 Cooper=2 Fletcher=4 Miller=5 Smith=1

```



### Simple solution

```perl6
# Contains only five floors. 5! = 120 permutations.
for (flat (1..5).permutations) -> $b, $c, $f, $m, $s {
    say "Baker=$b Cooper=$c Fletcher=$f Miller=$m Smith=$s"
        if  $b != 5         # Baker    !live  on top floor.
        and $c != 1         # Cooper   !live  on bottom floor.
        and $f != 1|5       # Fletcher !live  on top or the bottom floor.
        and $m  > $c        # Miller    lives on a higher floor than Cooper.
        and $s != $f-1|$f+1 # Smith    !live  adjacent to Fletcher
        and $f != $c-1|$c+1 # Fletcher !live  adjacent to Cooper
    ;
}
```


Adding more people and floors requires changing the list that's being used for the permutations, adding a variable for the new person, a piece of output in the string and finally to adjust all mentions of the "top" floor.
Adjusting to different rules requires changing the multi-line if statement in the loop.

```txt
Baker=3 Cooper=2 Fletcher=4 Miller=5 Smith=1
```



## Phix

Simple static/hard-coded solution (brute force search)

```Phix
enum Baker, Cooper, Fletcher, Miller, Smith
constant names={"Baker","Cooper","Fletcher","Miller","Smith"}

procedure test(sequence flats)
    if flats[Baker]!=5
    and flats[Cooper]!=1
    and not find(flats[Fletcher],{1,5})
    and flats[Miller]>flats[Cooper]
    and abs(flats[Smith]-flats[Fletcher])!=1
    and abs(flats[Fletcher]-flats[Cooper])!=1 then
        for i=1 to 5 do
            ?{names[i],flats[i]}
        end for
    end if
end procedure

for i=1 to factorial(5) do
    test(permute(i,tagset(5)))
end for
```

```txt

{"Baker",3}
{"Cooper",2}
{"Fletcher",4}
{"Miller",5}
{"Smith",1}

```

Something more flexible. The nested rules worked just as well, and
of course the code will cope with various content in names/rules.

```Phix
sequence names = {"Baker","Cooper","Fletcher","Miller","Smith"},
         rules = {{"!=","Baker",length(names)},
                  {"!=","Cooper",1},
                  {"!=","Fletcher",1},
                  {"!=","Fletcher",length(names)},
                  {">","Miller","Cooper"},
--                {"!=",{"abs","Smith","Fletcher"},1},
                  {"nadj","Smith","Fletcher"},
--                {"!=",{"abs","Fletcher","Cooper"},1},
                  {"nadj","Fletcher","Cooper"}}

function eval(sequence rule, sequence flats)
    {string operand, object op1, object op2} = rule
    if string(op1) then
        op1 = flats[find(op1,names)]
--  elsif sequence(op1) then
--      op1 = eval(op1,flats)
    end if
    if string(op2) then
        op2 = flats[find(op2,names)]
--  elsif sequence(op2) then
--      op2 = eval(op2,flats)
    end if
    switch operand do
        case "!=": return op1!=op2
        case ">":  return op1>op2
--      case "abs": return abs(op1-op2)
        case "nadj": return abs(op1-op2)!=1
    end switch
    return 9/0
end function

procedure test(sequence flats)
    for i=1 to length(rules) do
        if not eval(rules[i],flats) then return end if
    end for
    for i=1 to length(names) do
        ?{names[i],flats[i]}
    end for
end procedure

for i=1 to factorial(length(names)) do
    test(permute(i,tagset(length(names))))
end for
```

Same output


## PicoLisp

Using Pilog (PicoLisp Prolog). The problem can be modified by changing just the 'dwelling' rule (the "Problem statement"). This might involve the names and number of dwellers (the list in the first line), and statements about who does (or does not) live on the top floor (using the 'topFloor' predicate), the bottom floor (using the 'bottomFloor' predicate), on a higher floor (using the 'higherFloor' predicate) or on an adjacent floor (using the 'adjacentFloor' predicate). The logic follows an implied AND, and statements may be arbitrarily combined using OR and NOT (using the 'or' and 'not' predicates), or any other Pilog (Prolog in picoLisp) built-in predicates. If the problem statement has several solutions, they will be all generated.

```PicoLisp
# Problem statement
(be dwelling (@Tenants)
   (permute (Baker Cooper Fletcher Miller Smith) @Tenants)
   (not (topFloor Baker @Tenants))
   (not (bottomFloor Cooper @Tenants))
   (not (or ((topFloor Fletcher @Tenants)) ((bottomFloor Fletcher @Tenants))))
   (higherFloor Miller Cooper @Tenants)
   (not (adjacentFloor Smith Fletcher @Tenants))
   (not (adjacentFloor Fletcher Cooper @Tenants)) )

# Utility rules
(be topFloor (@Tenant @Lst)
   (equal (@ @ @ @ @Tenant) @Lst) )

(be bottomFloor (@Tenant @Lst)
   (equal (@Tenant @ @ @ @) @Lst) )

(be higherFloor (@Tenant1 @Tenant2 @Lst)
   (append @ @Rest @Lst)
   (equal (@Tenant2 . @Higher) @Rest)
   (member @Tenant1 @Higher) )

(be adjacentFloor (@Tenant1 @Tenant2 @Lst)
   (append @ @Rest @Lst)
   (or
      ((equal (@Tenant1 @Tenant2 . @) @Rest))
      ((equal (@Tenant2 @Tenant1 . @) @Rest)) ) )
```

```txt
: (? (dwelling @Result))
 @Result=(Smith Cooper Baker Fletcher Miller)  # Only one solution
-> NIL
```



## PowerShell

```PowerShell

# Floors are numbered 1 (ground) to 5 (top)

# Baker, Cooper, Fletcher, Miller, and Smith live on different floors:
$statement1 = '$baker  -ne $cooper -and $baker    -ne $fletcher -and $baker    -ne $miller -and
               $baker  -ne $smith  -and $cooper   -ne $fletcher -and $cooper   -ne $miller -and
               $cooper -ne $smith  -and $fletcher -ne $miller   -and $fletcher -ne $smith  -and
               $miller -ne $smith'

# Baker does not live on the top floor:
$statement2 = '$baker -ne 5'

# Cooper does not live on the bottom floor:
$statement3 = '$cooper -ne 1'

# Fletcher does not live on either the top or the bottom floor:
$statement4 = '$fletcher -ne 1 -and $fletcher -ne 5'

# Miller lives on a higher floor than does Cooper:
$statement5 = '$miller -gt $cooper'

# Smith does not live on a floor adjacent to Fletcher's:
$statement6 = '[Math]::Abs($smith - $fletcher) -ne 1'

# Fletcher does not live on a floor adjacent to Cooper's:
$statement7 = '[Math]::Abs($fletcher - $cooper) -ne 1'

for ($baker = 1; $baker -lt 6; $baker++)
{
    for ($cooper = 1; $cooper -lt 6; $cooper++)
    {
        for ($fletcher = 1; $fletcher -lt 6; $fletcher++)
        {
            for ($miller = 1; $miller -lt 6; $miller++)
            {
                for ($smith = 1; $smith -lt 6; $smith++)
                {
                    if (Invoke-Expression $statement2)
                    {
                        if (Invoke-Expression $statement3)
                        {
                            if (Invoke-Expression $statement5)
                            {
                                if (Invoke-Expression $statement4)
                                {
                                    if (Invoke-Expression $statement6)
                                    {
                                        if (Invoke-Expression $statement7)
                                        {
                                            if (Invoke-Expression $statement1)
                                            {
                                                $multipleDwellings = @()
                                                $multipleDwellings+= [PSCustomObject]@{Name = "Baker"   ; Floor = $baker}
                                                $multipleDwellings+= [PSCustomObject]@{Name = "Cooper"  ; Floor = $cooper}
                                                $multipleDwellings+= [PSCustomObject]@{Name = "Fletcher"; Floor = $fletcher}
                                                $multipleDwellings+= [PSCustomObject]@{Name = "Miller"  ; Floor = $miller}
                                                $multipleDwellings+= [PSCustomObject]@{Name = "Smith"   ; Floor = $smith}
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

```

The solution sorted by name:

```PowerShell

$multipleDwellings

```

```txt

Name     Floor
----     -----
Baker        3
Cooper       2
Fletcher     4
Miller       5
Smith        1

```

The solution sorted by floor:

```PowerShell

$multipleDwellings | Sort-Object -Property Floor -Descending

```

```txt

Name     Floor
----     -----
Miller       5
Fletcher     4
Baker        3
Cooper       2
Smith        1

```



## Prolog



### Using CLPFD

Works with SWI-Prolog and library(clpfd) written by '''Markus Triska'''.


```Prolog
:- use_module(library(clpfd)).

:- dynamic top/1, bottom/1.

% Baker does not live on the top floor
rule1(L) :-
	member((baker, F), L),
	top(Top),
	F #\= Top.

% Cooper does not live on the bottom floor.
rule2(L) :-
	member((cooper, F), L),
	bottom(Bottom),
	F #\= Bottom.

% Fletcher does not live on either the top or the bottom floor.
rule3(L) :-
	member((fletcher, F), L),
	top(Top),
	bottom(Bottom),
	F #\= Top,
	F #\= Bottom.

% Miller lives on a higher floor than does Cooper.
rule4(L) :-
	member((miller, Fm), L),
	member((cooper, Fc), L),
	Fm #> Fc.

% Smith does not live on a floor adjacent to Fletcher's.
rule5(L) :-
	member((smith, Fs), L),
	member((fletcher, Ff), L),
	abs(Fs-Ff) #> 1.

% Fletcher does not live on a floor adjacent to Cooper's.
rule6(L) :-
	member((cooper, Fc), L),
	member((fletcher, Ff), L),
	abs(Fc-Ff) #> 1.

init(L) :-
	% we need to define top and bottom
	assert(bottom(1)),
	length(L, Top),
	assert(top(Top)),

	% we say that they are all in differents floors
	bagof(F, X^member((X, F), L), LF),
	LF ins 1..Top,
	all_different(LF),

	% Baker does not live on the top floor
	rule1(L),

	% Cooper does not live on the bottom floor.
	rule2(L),

	% Fletcher does not live on either the top or the bottom floor.
	rule3(L),

	% Miller lives on a higher floor than does Cooper.
	rule4(L),

	% Smith does not live on a floor adjacent to Fletcher's.
	rule5(L),

	% Fletcher does not live on a floor adjacent to Cooper's.
	rule6(L).


solve(L) :-
	bagof(F, X^member((X, F), L), LF),
	label(LF).

dinners :-
	retractall(top(_)), retractall(bottom(_)),
	L = [(baker, _Fb), (cooper, _Fc), (fletcher, _Ff), (miller, _Fm), (smith, _Fs)],
	init(L),
	solve(L),
	maplist(writeln, L).

```


```txt
?- dinners.
baker,3
cooper,2
fletcher,4
miller,5
smith,1
true ;
false.

```

true ==> predicate succeeded.

false ==> no other solution.


<B>About flexibility :</b> each name is associated with a floor, (contiguous floors differs from 1).
Bottom is always 1 but Top is defined from the number of names.
Each statement of the problem is translated in a Prolog rule, (a constraint on the floors), we can add so much of rules that we want, and a modification of one statement only modified one rule.
To solve the problem, library clpfd does the job.


### Plain Prolog version



```Prolog
select([A|As],S):- select(A,S,S1),select(As,S1).
select([],_).

dinesmans(X) :-
    %% Baker, Cooper, Fletcher, Miller, and Smith live on different floors
    %% of an apartment house that contains only five floors.
    select([Baker,Cooper,Fletcher,Miller,Smith],[1,2,3,4,5]),

    %% Baker does not live on the top floor.
    Baker =\= 5,

    %% Cooper does not live on the bottom floor.
    Cooper =\= 1,

    %% Fletcher does not live on either the top or the bottom floor.
    Fletcher =\= 1, Fletcher =\= 5,

    %% Miller lives on a higher floor than does Cooper.
    Miller > Cooper,

    %% Smith does not live on a floor adjacent to Fletcher's.
    1 =\= abs(Smith - Fletcher),

    %% Fletcher does not live on a floor adjacent to Cooper's.
    1 =\= abs(Fletcher - Cooper),

    %% Where does everyone live?
    X = ['Baker'(Baker), 'Cooper'(Cooper), 'Fletcher'(Fletcher),
         'Miller'(Miller), 'Smith'(Smith)].

main :-  bagof( X, dinesmans(X), L )
         -> maplist( writeln, L), nl, write('No more solutions.')
         ;  write('No solutions.').

```


Ease of change (flexibility) is arguably evident in the code. [http://ideone.com/8n9IQ The output]:

```txt

[Baker(3), Cooper(2), Fletcher(4), Miller(5), Smith(1)]

No more solutions.

```



### Testing as soon as possible


```Prolog
dinesmans(X) :-
    %% 1. Baker, Cooper, Fletcher, Miller, and Smith live on different floors
    %%    of an apartment house that contains only five floors.
    Domain = [1,2,3,4,5],

    %% 2. Baker does not live on the top floor.
    select(Baker,Domain,D1), Baker =\= 5,

    %% 3. Cooper does not live on the bottom floor.
    select(Cooper,D1,D2), Cooper =\= 1,

    %% 4. Fletcher does not live on either the top or the bottom floor.
    select(Fletcher,D2,D3), Fletcher =\= 1, Fletcher =\= 5,

    %% 5. Miller lives on a higher floor than does Cooper.
    select(Miller,D3,D4), Miller > Cooper,

    %% 6. Smith does not live on a floor adjacent to Fletcher's.
    select(Smith,D4,_), 1 =\= abs(Smith - Fletcher),

    %% 7. Fletcher does not live on a floor adjacent to Cooper's.
    1 =\= abs(Fletcher - Cooper),

    %% Where does everyone live?
    X = ['Baker'(Baker), 'Cooper'(Cooper), 'Fletcher'(Fletcher),
         'Miller'(Miller), 'Smith'(Smith)].

```


[http://ideone.com/1vYTV Running it] produces the same output, but more efficiently. Separate testing in SWI shows 1,328 inferences for the former, 379 inferences for the latter version. Moving rule 7. up below rule 4. brings it down to 295 inferences.


## PureBasic

```PureBasic
Prototype cond(Array t(1))

Enumeration #Null
  #Baker
  #Cooper
  #Fletcher
  #Miller
  #Smith
EndEnumeration

Procedure checkTenands(Array tenants(1), Array Condions.cond(1))
  Protected i, j
  Protected.cond *f
  j=ArraySize(Condions())
  For i=0 To j
    *f=Condions(i)              ; load the function pointer to the current condition
    If *f(tenants()) = #False
      ProcedureReturn  #False
    EndIf
  Next
  ProcedureReturn #True
EndProcedure

Procedure C1(Array t(1))
  If Int(Abs(t(#Fletcher)-t(#Cooper)))<>1
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C2(Array t(1))
  If t(#Baker)<>5
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C3(Array t(1))
  If t(#Cooper)<>1
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C4(Array t(1))
  If t(#Miller) >= t(#Cooper)
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C5(Array t(1))
  If t(#Fletcher)<>1 And t(#Fletcher)<>5
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C6(Array t(1))
  If Int(Abs(t(#Smith)-t(#Fletcher)))<>1
    ProcedureReturn #True
  EndIf
EndProcedure


If OpenConsole()
  Dim People(4)
  Dim Conditions(5)
  Define a, b, c, d, e, i
  ;
  ;- Load all conditions
  Conditions(i)=@C1(): i+1
  Conditions(i)=@C2(): i+1
  Conditions(i)=@C3(): i+1
  Conditions(i)=@C4(): i+1
  Conditions(i)=@C5(): i+1
  Conditions(i)=@C6()
  ;
  ; generate and the all legal combinations
  For a=1 To 5
    For b=1 To 5
      If a=b: Continue: EndIf
      For c=1 To 5
        If a=c Or b=c: Continue: EndIf
        For d=1 To 5
          If d=a Or d=b Or d=c : Continue: EndIf
          For e=1 To 5
            If e=a Or e=b Or e=c Or e=d: Continue: EndIf
            People(#Baker)=a
            People(#Cooper)=b
            People(#Fletcher)=c
            People(#Miller)=d
            People(#Smith)=e
            If checkTenands(People(), Conditions())
              PrintN("Solution found;")
              PrintN("Baker="+Str(a)+#CRLF$+"Cooper="+Str(b)+#CRLF$+"Fletcher="+Str(c))
              PrintN("Miller="+Str(d)+#CRLF$+"Smith="+Str(e)+#CRLF$)
            EndIf
          Next
        Next
      Next
    Next
  Next
  Print("Press ENTER to exit"): Input()
EndIf
```


```txt
Solution found;
Baker=3
Cooper=2
Fletcher=4
Miller=5
Smith=1
```

===Port of [http://rosettacode.org/wiki/Dinesman%27s_multiple-dwelling_problem#C C code solution]===

```PureBasic

EnableExplicit

Global verbose = #False

Macro COND ( a, b )
	Procedure a ( Array s ( 1 ) )
		ProcedureReturn Bool( b )
	EndProcedure
EndMacro

Prototype condition ( Array s ( 1 ) )

#N_FLOORS = 5
#TOP = #N_FLOORS - 1

Global Dim solutions ( #N_FLOORS - 1 )
Global Dim occupied ( #N_FLOORS - 1 )

Enumeration tenants
	#baker
	#cooper
	#fletcher
	#miller
	#smith
	#phantom_of_the_opera
EndEnumeration

Global Dim names.s ( 4 )
	names( 0 ) = "baker"
	names( 1 ) = "cooper"
	names( 2 ) = "fletcher"
	names( 3 ) = "miller"
	names( 4 ) = "smith"

COND( c0, s( #baker ) <> #TOP )
COND( c1, s( #cooper ) <> 0 )
COND( c2, s( #fletcher ) <> 0 And s( #fletcher ) <> #TOP )
COND( c3, s( #miller ) > s( #cooper ) )
COND( c4, Abs( s( #smith ) - s( #fletcher ) ) <> 1 )
COND( c5, Abs( s( #cooper ) - s( #fletcher ) ) <> 1 )

#N_CONDITIONS = 6

Global Dim conds ( #N_CONDITIONS - 1 )
	conds( 0 ) = @c0()
	conds( 1 ) = @c1()
	conds( 2 ) = @c2()
	conds( 3 ) = @c3()
	conds( 4 ) = @c4()
	conds( 5 ) = @c5()

Procedure solve ( person.i )
	Protected i.i, j.i
	If person = #phantom_of_the_opera
		For i = 0 To #N_CONDITIONS - 1
			Protected proc.condition = conds( i )
			If proc( solutions( ) )
				Continue
			EndIf
			If verbose
				For j = 0 To #N_FLOORS - 1
					PrintN( Str( solutions( j ) ) + " " + names( j ) )
				Next
				PrintN( "cond" + Str( i ) + " bad\n" )
			EndIf
			ProcedureReturn 0
		Next
		PrintN( "Found arrangement:" )
		For i = 0 To #N_FLOORS - 1
			PrintN( Str( solutions( i ) ) + " " + names( i ) )
		Next
		ProcedureReturn 1
	EndIf
	For i = 0 To #N_FLOORS - 1
		If occupied( i )
			Continue
		EndIf
		solutions( person ) = i
		occupied( i ) = #True
		If solve( person + 1 )
			ProcedureReturn #True
		EndIf
		occupied( i ) = #False
	Next
	ProcedureReturn #False
EndProcedure



OpenConsole( )

verbose = #False

If Not solve( 0 )
	PrintN( "Nobody lives anywhere" )
EndIf

Input( )
CloseConsole( )

End
```



```txt
Found arrangement:
2 baker
1 cooper
3 fletcher
4 miller
0 smith
```



## Python


### By parsing the problem statement

This example parses the statement of the problem as given and allows some variability such as the number of people, floors and constraints can be varied although the type of constraints allowed and the sentence structure is limited

;Setup

Parsing is done with the aid of the multi-line regular expression at the head of the program.


```python
import re
from itertools import product

problem_re = re.compile(r"""(?msx)(?:

# Multiple names of form n1, n2, n3, ... , and nK
(?P<namelist> [a-zA-Z]+ (?: , \s+ [a-zA-Z]+)* (?: ,? \s+ and) \s+ [a-zA-Z]+ )

# Flexible floor count (2 to 10 floors)
| (?:  .* house \s+ that \s+ contains \s+ only \s+
  (?P<floorcount> two|three|four|five|six|seven|eight|nine|ten ) \s+ floors \s* \.)

# Constraint: "does not live on the n'th floor"
|(?: (?P<not_live>  \b [a-zA-Z]+ \s+ does \s+ not \s+ live \s+ on \s+ the \s+
  (?: top|bottom|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth) \s+ floor \s* \. ))

# Constraint: "does not live on either the I'th or the J'th [ or the K'th ...] floor
|(?P<not_either> \b [a-zA-Z]+ \s+ does \s+ not \s+ live \s+ on \s+ either
  (?: \s+ (?: or \s+)? the \s+
    (?: top|bottom|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth))+ \s+ floor \s* \. )

# Constraint: "P1 lives on a higher/lower floor than P2 does"
|(?P<hi_lower> \b  [a-zA-Z]+ \s+ lives \s+ on \s+ a \s (?: higher|lower)
   \s+ floor \s+ than (?: \s+ does)  \s+  [a-zA-Z]+ \s* \. )

# Constraint: "P1 does/does not live on a floor adjacent to P2's"
|(?P<adjacency>  \b [a-zA-Z]+ \s+ does (?:\s+ not)? \s+ live \s+ on \s+ a \s+
   floor \s+ adjacent \s+ to \s+  [a-zA-Z]+ (?: 's )? \s* \. )

# Ask for the solution
|(?P<question> Where \s+ does \s+ everyone \s+ live \s* \?)

)
""")

names, lennames = None, None
floors = None
constraint_expr = 'len(set(alloc)) == lennames' # Start with all people on different floors

def do_namelist(txt):
    " E.g. 'Baker, Cooper, Fletcher, Miller, and Smith'"
    global names, lennames
    names = txt.replace(' and ', ' ').split(', ')
    lennames = len(names)

def do_floorcount(txt):
    " E.g. 'five'"
    global floors
    floors = '||two|three|four|five|six|seven|eight|nine|ten'.split('|').index(txt)

def do_not_live(txt):
    " E.g. 'Baker does not live on the top floor.'"
    global constraint_expr
    t = txt.strip().split()
    who, floor = t[0], t[-2]
    w, f = (names.index(who),
            ('|first|second|third|fourth|fifth|sixth|' +
             'seventh|eighth|ninth|tenth|top|bottom|').split('|').index(floor)
            )
    if f == 11: f = floors
    if f == 12: f = 1
    constraint_expr += ' and alloc[%i] != %i' % (w, f)

def do_not_either(txt):
    " E.g. 'Fletcher does not live on either the top or the bottom floor.'"
    global constraint_expr
    t = txt.replace(' or ', ' ').replace(' the ', ' ').strip().split()
    who, floor = t[0], t[6:-1]
    w, fl = (names.index(who),
             [('|first|second|third|fourth|fifth|sixth|' +
               'seventh|eighth|ninth|tenth|top|bottom|').split('|').index(f)
              for f in floor]
             )
    for f in fl:
        if f == 11: f = floors
        if f == 12: f = 1
        constraint_expr += ' and alloc[%i] != %i' % (w, f)


def do_hi_lower(txt):
    " E.g. 'Miller lives on a higher floor than does Cooper.'"
    global constraint_expr
    t = txt.replace('.', '').strip().split()
    name_indices = [names.index(who) for who in (t[0], t[-1])]
    if 'lower' in t:
        name_indices = name_indices[::-1]
    constraint_expr += ' and alloc[%i] > alloc[%i]' % tuple(name_indices)

def do_adjacency(txt):
    ''' E.g. "Smith does not live on a floor adjacent to Fletcher's."'''
    global constraint_expr
    t = txt.replace('.', '').replace("'s", '').strip().split()
    name_indices = [names.index(who) for who in (t[0], t[-1])]
    constraint_expr += ' and abs(alloc[%i] - alloc[%i]) > 1' % tuple(name_indices)

def do_question(txt):
    global constraint_expr, names, lennames

    exec_txt = '''
for alloc in product(range(1,floors+1), repeat=len(names)):
    if %s:
        break
else:
    alloc = None
''' % constraint_expr
    exec(exec_txt, globals(), locals())
    a = locals()['alloc']
    if a:
        output= ['Floors are numbered from 1 to %i inclusive.' % floors]
        for a2n in zip(a, names):
            output += ['  Floor %i is occupied by %s' % a2n]
        output.sort(reverse=True)
        print('\n'.join(output))
    else:
        print('No solution found.')
    print()

handler = {
    'namelist': do_namelist,
    'floorcount': do_floorcount,
    'not_live': do_not_live,
    'not_either': do_not_either,
    'hi_lower': do_hi_lower,
    'adjacency': do_adjacency,
    'question': do_question,
    }
def parse_and_solve(problem):
    p = re.sub(r'\s+', ' ', problem).strip()
    for x in problem_re.finditer(p):
        groupname, txt = [(k,v) for k,v in x.groupdict().items() if v][0]
        #print ("%r, %r" % (groupname, txt))
        handler[groupname](txt)
```


;Problem statement

This is not much more than calling a function on the text of the problem!

```python
if __name__ == '__main__':
    parse_and_solve("""
        Baker, Cooper, Fletcher, Miller, and Smith
        live on different floors of an apartment house that contains
        only five floors. Baker does not live on the top floor. Cooper
        does not live on the bottom floor. Fletcher does not live on
        either the top or the bottom floor. Miller lives on a higher
        floor than does Cooper. Smith does not live on a floor
        adjacent to Fletcher's. Fletcher does not live on a floor
        adjacent to Cooper's. Where does everyone live?""")

    print('# Add another person with more constraints and more floors:')
    parse_and_solve("""
        Baker, Cooper, Fletcher, Miller, Guinan, and Smith
        live on different floors of an apartment house that contains
        only seven floors. Guinan does not live on either the top or the third or the fourth floor.
        Baker does not live on the top floor. Cooper
        does not live on the bottom floor. Fletcher does not live on
        either the top or the bottom floor. Miller lives on a higher
        floor than does Cooper. Smith does not live on a floor
        adjacent to Fletcher's. Fletcher does not live on a floor
        adjacent to Cooper's. Where does everyone live?""")
```


;Output

This shows the output from the original problem and then for another, slightly different problem to cover some of the variability asked for in the task.

```txt
Floors are numbered from 1 to 5 inclusive.
  Floor 5 is occupied by Miller
  Floor 4 is occupied by Fletcher
  Floor 3 is occupied by Baker
  Floor 2 is occupied by Cooper
  Floor 1 is occupied by Smith

# Add another person with more constraints and more floors:
Floors are numbered from 1 to 7 inclusive.
  Floor 7 is occupied by Smith
  Floor 6 is occupied by Guinan
  Floor 4 is occupied by Fletcher
  Floor 3 is occupied by Miller
  Floor 2 is occupied by Cooper
  Floor 1 is occupied by Baker

```


===By using the [[Amb#Python|Amb]] operator===
In this example, the problem needs to be turned into valid Python code for use with the Amb operator. Setup is just to import Amb.

The second set of results corresponds to this modification to the problem statement:

```txt
Baker, Cooper, Fletcher, Miller, Guinan, and Smith
live on different floors of an apartment house that contains
only seven floors. Guinan does not live on either the top or the third or the fourth floor.
Baker does not live on the top floor. Cooper
does not live on the bottom floor. Fletcher does not live on
either the top or the bottom floor. Miller lives on a higher
floor than does Cooper. Smith does not live on a floor
adjacent to Fletcher's. Fletcher does not live on a floor
adjacent to Cooper's. Where does everyone live
```



```python
from amb import Amb

if __name__ == '__main__':
    amb = Amb()

    maxfloors = 5
    floors = range(1, maxfloors+1)
    # Possible floors for each person
    Baker, Cooper, Fletcher, Miller, Smith = (amb(floors) for i in range(5))
    for _dummy in amb( lambda Baker, Cooper, Fletcher, Miller, Smith: (
                         len(set([Baker, Cooper, Fletcher, Miller, Smith])) == 5  # each to a separate floor
                         and Baker != maxfloors
                         and Cooper != 1
                         and Fletcher not in (maxfloors, 1)
                         and Miller > Cooper
                         and (Smith - Fletcher) not in (1, -1)  # Not adjacent
                         and (Fletcher - Cooper) not in (1, -1) # Not adjacent
                         ) ):

        print 'Floors are numbered from 1 to %i inclusive.' % maxfloors
        print '\n'.join(sorted('  Floor %i is occupied by %s'
                                   % (globals()[name], name)
                               for name in 'Baker, Cooper, Fletcher, Miller, Smith'.split(', ')))
        break
    else:
        print 'No solution found.'
    print


    print '# Add another person with more constraints and more floors:'
    # The order that Guinan is added to any list of people must stay consistant

    amb = Amb()

    maxfloors = 7
    floors = range(1, maxfloors+1)
    # Possible floors for each person
    Baker, Cooper, Fletcher, Miller, Guinan, Smith = (amb(floors) for i in range(6))
    for _dummy in amb( lambda Baker, Cooper, Fletcher, Miller, Guinan, Smith: (
                         len(set([Baker, Cooper, Fletcher, Miller, Guinan, Smith])) == 6  # each to a separate floor
                         and Guinan not in (maxfloors, 3, 4)
                         and Baker != maxfloors
                         and Cooper != 1
                         and Fletcher not in (maxfloors, 1)
                         and Miller > Cooper
                         and (Smith - Fletcher) not in (1, -1)  # Not adjacent
                         and (Fletcher - Cooper) not in (1, -1) # Not adjacent
                         ) ):

        print 'Floors are numbered from 1 to %i inclusive.' % maxfloors
        print '\n'.join(sorted('  Floor %i is occupied by %s'
                                   % (globals()[name], name)
                               for name in 'Baker, Cooper, Fletcher, Miller, Guinan, Smith'.split(', ')))
        break
    else:
        print 'No solution found.'
    print

```


```txt
Floors are numbered from 1 to 5 inclusive.
  Floor 1 is occupied by Smith
  Floor 2 is occupied by Cooper
  Floor 3 is occupied by Baker
  Floor 4 is occupied by Fletcher
  Floor 5 is occupied by Miller

# Add another person with more constraints and more floors:
Floors are numbered from 1 to 7 inclusive.
  Floor 1 is occupied by Baker
  Floor 2 is occupied by Cooper
  Floor 3 is occupied by Miller
  Floor 4 is occupied by Fletcher
  Floor 5 is occupied by Guinan
  Floor 6 is occupied by Smith

```


### Simple Solutions



```python
from itertools import permutations

class Names:
    Baker, Cooper, Fletcher, Miller, Smith = range(5)
    seq = [Baker, Cooper, Fletcher, Miller, Smith]
    strings = "Baker Cooper Fletcher Miller Smith".split()

predicates = [
    lambda s: s[Names.Baker] != len(s)-1,
    lambda s: s[Names.Cooper] != 0,
    lambda s: s[Names.Fletcher] != 0 and s[Names.Fletcher] != len(s)-1,
    lambda s: s[Names.Miller] > s[Names.Cooper],
    lambda s: abs(s[Names.Smith] - s[Names.Fletcher]) != 1,
    lambda s: abs(s[Names.Cooper] - s[Names.Fletcher]) != 1];

for sol in permutations(Names.seq):
    if all(p(sol) for p in predicates):
        print(" ".join(x for x, y in sorted(zip(Names.strings, sol), key=lambda x: x[1])))
```

```txt
Smith Cooper Baker Fletcher Miller
```


Or, in a (pylinted) bare bones format:

```python
'''Dinesman's multiple-dwelling problem'''

from itertools import permutations

print([
    (
        'Baker on ' + str(b),
        'Cooper on ' + str(c),
        'Fletcher on ' + str(f),
        'Miller on ' + str(m),
        'Smith on ' + str(s)
    ) for [b, c, f, m, s] in permutations(range(1, 6))
    if all([
        5 != b,
        1 != c,
        1 != f,
        5 != f,
        c < m,
        1 < abs(s - f),
        1 < abs(c - f)
    ])
])
```

```txt
[('Baker on 3', 'Cooper on 2', 'Fletcher on 4', 'Miller on 5', 'Smith on 1')]
```



Which we could then disaggregate and comment a little more fully, replacing the list comprehension with a direct use of the list monad bind operator (concatMap):


```python
'''Dinesman's multiple-dwelling problem'''

from itertools import chain, permutations


# main :: IO ()
def main():
    '''Solution or null result.'''
    print(report(
        concatMap(dinesman)(
            permutations(range(1, 6))
        )
    ))


# dinesman :: (Int, Int, Int, Int, Int) -> [(Int, Int, Int, Int, Int)]
def dinesman(bcfms):
    '''A list containing the given permutation of five
       integers if it matches all the dinesman conditions,
       or an empty list if it does not.
    '''
    [b, c, f, m, s] = bcfms
    return [bcfms] if all([
        5 != b,
        1 != c,
        1 != f,
        5 != f,
        c < m,
        1 < abs(s - f),
        1 < abs(c - f)
    ]) else []


# report :: [(Int, Int, Int, Int, Int)] ->  String
def report(xs):
    '''A message summarizing the first (if any) solution found.
    '''
    return ', '.join(list(map(
        lambda k, n: k + ' in ' + str(n),
        ['Baker', 'Cooper', 'Fletcher', 'Miller', 'Smith'],
        xs[0]
    ))) + '.' if xs else 'No solution found.'


# GENERAL -------------------------------------------------

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


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Baker in 3, Cooper in 2, Fletcher in 4, Miller in 5, Smith in 1.
```



## R



```R

names = unlist(strsplit("baker cooper fletcher miller smith", " "))

test <- function(floors) {
  f <- function(name) which(name == floors)
  if ((f('baker') != 5) &&
      (f('cooper') != 1) &&
      (any(f('fletcher') == 2:4)) &&
      (f('miller') > f('cooper')) &&
      (abs(f('fletcher') - f('cooper')) > 1) &&
      (abs(f('smith') - f('fletcher')) > 1))
    cat("\nFrom bottom to top: --> ", floors, "\n")
}

do.perms <- function(seq, func, built = c()){
  if (0 == length(seq))  func(built)
  else  for (x in seq) do.perms( seq[!seq==x], func, c(x, built)) }

```


Testing:


```R

> do.perms(names, test)
From bottom to top: -->  smith cooper baker fletcher miller

> system.time(do.perms(names, test))
From bottom to top: -->  smith cooper baker fletcher miller
   user  system elapsed
      0       0       0

```



## Racket


This is a direct translation of the problem constraints using an <tt>amb</tt> operator to make the choices (and therefore continuations to do the search).  Since it's a direct translation, pretty much all aspects of the problem can change.  Note that a direct translation was preferred even though it could be made to run much faster.


```racket

#lang racket

;; A quick `amb' implementation
(define fails '())
(define (fail) (if (pair? fails) ((car fails)) (error "no more choices!")))
(define (amb xs)
  (let/cc k (set! fails (cons k fails)))
  (if (pair? xs) (begin0 (car xs) (set! xs (cdr xs)))
      (begin (set! fails (cdr fails)) (fail))))
(define (assert . conditions) (when (memq #f conditions) (fail)))

;; Convenient macro for definining problem items
(define-syntax-rule (with: all (name ...) #:in choices body ...)
  (let* ([cs choices] [name (amb cs)] ... [all `([,name name] ...)]) body ...))

;;
### == problem translation starts here ==


;; Baker, Cooper, Fletcher, Miller, and Smith live on different floors
;; of an apartment house that contains only five floors.
(with: residents [Baker Cooper Fletcher Miller Smith] #:in (range 1 6)
  ;; Some helpers
  (define (on-top    x) (for/and ([y residents]) (x . >= . (car y))))
  (define (on-bottom x) (for/and ([y residents]) (x . <= . (car y))))
  (define (adjacent x y) (= 1 (abs (- x y))))
  (assert
   ;; ... live on different floors ...
   (assert (= 5 (length (remove-duplicates (map car residents)))))
   ;; Baker does not live on the top floor.
   (not (on-top Baker))
   ;; Cooper does not live on the bottom floor.
   (not (on-bottom Cooper))
   ;; Fletcher does not live on either the top or the bottom floor.
   (not (on-top Fletcher))
   (not (on-bottom Fletcher))
   ;; Miller lives on a higher floor than does Cooper.
   (> Miller Cooper)
   ;; Smith does not live on a floor adjacent to Fletcher's.
   (not (adjacent Smith Fletcher))
   ;; Fletcher does not live on a floor adjacent to Cooper's.
   (assert (not (adjacent Fletcher Cooper))))
  ;; Where does everyone live?
  (printf "Solution:\n")
  (for ([x (sort residents > #:key car)]) (apply printf "  ~a. ~a\n" x)))

```


```txt

Solution:
  5. Miller
  4. Fletcher
  3. Baker
  2. Cooper
  1. Smith

```



## REXX

This REXX version tries to keep the rules as simple as possible,
with easy-to-read   '''if'''   statements.

Names of the tenants can be easily listed, and the floors are numbered according to the American system,

that is, the ground floor is the 1<sup>st</sup> floor, the next floor up is the 2<sup>nd</sup> floor, etc.

The REXX program is broken up into several parts:
::*   preamble where names and floors are defined.
::*   iterating all possibilities   (permutations would be faster, but with obtuse code).
::*   evaluation of the possibilities.
::*   elimination of cohabitation possibilities   (tenants must live on separate floors).
::*   elimination of possibilities according to the rules.
::*   displaying the possible solution(s), if any.
::*   displaying the number of solutions found.

Note that the   '''TH'''   function has extra boilerplate to handle larger numbers.

With one more REXX statement, the tenants could be listed by the order of the floors they live on;

(currently, the tenants are listed in the order they are listed in the   '''names'''   variable).

The "rules" that contain   '''=='''   could be simplified to   '''='''   for readability.

```rexx
/*REXX program solves the  Dinesman's multiple─dwelling  problem with "natural" wording.*/
names= 'Baker Cooper Fletcher Miller Smith'      /*names of multiple─dwelling tenants.  */
tenants=words(names)                             /*the number of tenants in the building*/
floors=5;   top=floors;    bottom=1;   #=floors; /*floor 1 is the ground (bottom) floor.*/
sols=0
        do !.1=1 for #;  do !.2=1 for #;  do !.3=1 for #;  do !.4=1 for #;  do !.5=1 for #
          do p=1 for tenants;     _=word(names,p);         upper _;      call value _, !.p
          end   /*p*/
                         do     j=1   for #-1    /* [↓]  people don't live on same floor*/
                             do k=j+1  to #;   if !.j==!.k  then iterate !.5    /*cohab?*/
                             end   /*k*/
                         end       /*j*/
        call Waldo                               /* ◄══ where the rubber meets the road.*/
        end;  end;  end;  end;  end              /*!.5  &  !.4  &  !.3   &  !.2  &   !.1*/

say 'found'    sols     "solution"s(sols).       /*display the number of solutions found*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Waldo: if Baker    == top                                     then return
       if Cooper   == bottom                                  then return
       if Fletcher == bottom      |  Fletcher == top          then return
       if Miller   \> Cooper                                  then return
       if Smith    == Fletcher-1  |  Smith    == Fletcher+1   then return
       if Fletcher == Cooper  -1  |  Fletcher == Cooper  +1   then return
       sols=sols+1
       say;              do p=1  for tenants;           tenant=right( word(names, p),  30)
                         say tenant      'lives on the'      !.p || th(!.p)       "floor."
                         end   /*p*/
       return                                    /* [↑]  show tenants in order in NAMES.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:     if arg(1)=1  then return '';    return "s"        /*a simple pluralizer function.*/
th:    arg x;  x=abs(x);  return word('th st nd rd', 1 +x// 10* (x//100%10\==1)*(x//10<4))
```

'''output'''

```txt

                         Baker lives on the 3rd floor.
                        Cooper lives on the 2nd floor.
                      Fletcher lives on the 4th floor.
                        Miller lives on the 5th floor.
                         Smith lives on the 1st floor.
found 1 solution.

```



## Ring


```ring

floor1 = "return baker!=cooper and baker!=fletcher and baker!=miller and
          baker!=smith and cooper!=fletcher and cooper!=miller and
          cooper!=smith and fletcher!=miller and fletcher!=smith and
          miller!=smith"
floor2 = "return baker!=4"
floor3 = "return cooper!=0"
floor4 = "return fletcher!=0 and fletcher!=4"
floor5 = "return miller>cooper"
floor6 = "return fabs(smith-fletcher)!=1"
floor7 = "return fabs(fletcher-cooper)!=1"
for baker = 0 to 4
    for cooper = 0 to 4
        for fletcher = 0 to 4
            for miller = 0 to 4
                for smith = 0 to 4
                    if eval(floor2) if eval(floor3) if eval(floor5)
                       if eval(floor4) if eval(floor6) if eval(floor7)
                          if eval(floor1)
                             see "baker lives on floor " + baker + nl
                             see "cooper lives on floor " + cooper + nl
                             see "fletcher lives on floor " + fletcher + nl
                             see "miller lives on floor " + miller + nl
                             see "smith lives on floor " + smith + nl ok ok ok ok ok ok ok
                next
            next
        next
    next
next

```

Output:

```txt

baker lives on floor 2
cooper lives on floor 1
fletcher lives on floor 3
miller lives on floor 4
smith lives on floor 0

```



## Ruby


### By parsing the problem

Inspired by the Python version.

```ruby
def solve( problem )
  lines = problem.split(".")
  names = lines.first.scan( /[A-Z]\w*/ )
  re_names = Regexp.union( names )
  # Later on, search for these keywords (the word "not" is handled separately).
  words = %w(first second third fourth fifth sixth seventh eighth ninth tenth
  bottom top higher lower adjacent)
  re_keywords = Regexp.union( words )

  predicates = lines[1..-2].flat_map do |line|  #build an array of lambda's
    keywords = line.scan( re_keywords )
    name1, name2 = line.scan( re_names )
    keywords.map do |keyword|
      l = case keyword
        when "bottom"   then ->(c){ c.first == name1 }
        when "top"      then ->(c){ c.last == name1 }
        when "higher"   then ->(c){ c.index( name1 ) > c.index( name2 ) }
        when "lower"    then ->(c){ c.index( name1 ) < c.index( name2 ) }
        when "adjacent" then ->(c){ (c.index( name1 ) - c.index( name2 )).abs == 1 }
        else                 ->(c){ c[words.index(keyword)] == name1 }
      end
      line =~ /\bnot\b/ ? ->(c){not l.call(c) } : l  # handle "not"
    end
  end

  names.permutation.detect{|candidate| predicates.all?{|predicate| predicate.(candidate)}}
end
```


The program operates under these assumptions:
*Sentences end with a ".".
*Every capitalized word in the first sentence is a name, the rest is ignored.
*There are as many floors as there are names.
*The only relevant words beside the names are: first, second, third,.., tenth, bottom, top, higher, lower, adjacent,(and) not. The rest, including the last sentence, is ignored.

Program invocation:

```ruby
#Direct positional words like top, bottom, first, second etc. can be combined; they refer to one name.
#The relative positional words higher, lower and adjacent can be combined; they need two names, not positions.

demo1 = "Abe Ben Charlie David. Abe not second top. not adjacent Ben Charlie.
David Abe adjacent. David adjacent Ben. Last line."

demo2 = "A B C D. A not adjacent D. not B adjacent higher C. C lower D. Last line"

problem1 = "Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house that
contains only five floors. Baker does not live on the top floor. Cooper does not live on the bottom floor.
Fletcher does not live on either the top or the bottom floor. Miller lives on a higher floor than does Cooper.
Smith does not live on a floor adjacent to Fletcher's. Fletcher does not live on a floor adjacent to Cooper's.
Where does everyone live?"

# from the Python version:
problem2 = "Baker, Cooper, Fletcher, Miller, Guinan, and Smith
live on different floors of an apartment house that contains
only seven floors. Guinan does not live on either the top or the third or the fourth floor.
Baker does not live on the top floor. Cooper
does not live on the bottom floor. Fletcher does not live on
either the top or the bottom floor. Miller lives on a higher
floor than does Cooper. Smith does not live on a floor
adjacent to Fletcher's. Fletcher does not live on a floor
adjacent to Cooper's. Where does everyone live?"

[demo1, demo2, problem1, problem2].each{|problem| puts solve( problem ) ;puts }
```

```txt

Ben
David
Abe
Charlie

B
A
C
D

Smith
Cooper
Baker
Fletcher
Miller

Baker
Cooper
Miller
Fletcher
Guinan
Smith

```



### Simple solution

```ruby
names = %i( Baker Cooper Fletcher Miller Smith )

predicates = [->(c){ :Baker != c.last },
              ->(c){ :Cooper != c.first },
              ->(c){ :Fletcher != c.first && :Fletcher != c.last },
              ->(c){ c.index(:Miller) > c.index(:Cooper) },
              ->(c){ (c.index(:Smith) - c.index(:Fletcher)).abs != 1 },
              ->(c){ (c.index(:Cooper) - c.index(:Fletcher)).abs != 1 }]

puts names.permutation.detect{|candidate| predicates.all?{|predicate| predicate.call(candidate)}}
```

```txt

Smith
Cooper
Baker
Fletcher
Miller

```



### Using grep


```ruby

N = %w(Baker Cooper Fletcher Miller Smith)
b,c,f,m,s = N

N.permutation.map{|a| a.join " "}.
grep(/(?=.*#{b}.)
      (?=.+#{c})
      (?=.+#{f}.)
      (?=.*#{c}.*#{m})
      (?=.*(#{f}..+#{s}|#{s}..+#{f}))
      (?=.*(#{f}..+#{c}|#{c}..+#{f}))/x).
first

```


```txt

"Smith Cooper Baker Fletcher Miller"

```



## Run BASIC

This program simply iterates by looking at each room available for each person.
It then looks to see if it meets the requirements for each person by looking at the results of the iteration.
It makes sure the room numbers add up to 15 which is the requirement of adding the floors in 1 + 2 + 3 + 4 + 5 = 15.


```runbasic
for baler          = 1 to 4                                    ' can not be in room 5
 for cooper        = 2 to 5                                    ' can not be in room 1
   for fletcher    = 2 to 4                                    ' can not be in room 1 or 5
    for miller     = 1 to 5                                    ' can be in any room
     for smith     = 1 to 5                                    ' can be in any room
     if baler <> cooper and fletcher <> miller and miller > cooper and abs(smith - fletcher) > 1 and abs(fletcher - cooper) > 1 then
      if baler + cooper + fletcher + miller + smith = 15 then  ' that is 1 + 2 + 3 + 4 + 5
        rooms$ = baler;cooper;fletcher;miller;smith
        print "baler: ";baler;" copper: ";cooper;" fletcher: ";fletcher;" miller: ";miller;" smith: ";smith
      end
      end if
     end if
    next smith
   next miller
  next fletcher
 next cooper
next baler
print "Can't assign rooms"                                     ' print this if it can not find a solution
```


```txt
baler: 3 copper: 2 fletcher: 4 miller: 5 smith: 1
```



## Scala


```Scala
import scala.math.abs

object Dinesman3 extends App {
  val tenants = List("Baker", "Cooper2", "Fletcher4", "Miller", "Smith")
  val (groundFloor, topFloor) = (1, tenants.size)

  /** Rules with related tenants and restrictions*/
  val exclusions =
    List((suggestedFloor0: Map[String, Int]) => suggestedFloor0("Baker") != topFloor,
      (suggestedFloor1: Map[String, Int]) => suggestedFloor1("Cooper2") != groundFloor,
      (suggestedFloor2: Map[String, Int]) => !List(groundFloor, topFloor).contains(suggestedFloor2("Fletcher4")),
      (suggestedFloor3: Map[String, Int]) => suggestedFloor3("Miller") > suggestedFloor3("Cooper2"),
      (suggestedFloor4: Map[String, Int]) => abs(suggestedFloor4("Smith") - suggestedFloor4("Fletcher4")) != 1,
      (suggestedFloor5: Map[String, Int]) => abs(suggestedFloor5("Fletcher4") - suggestedFloor5("Cooper2")) != 1)

  tenants.permutations.map(_ zip (groundFloor to topFloor)).
    filter(p => exclusions.forall(_(p.toMap))).toList match {
      case Nil => println("No solution")
      case xss => {
        println(s"Solutions: ${xss.size}")
        xss.foreach { l =>
          println("possible solution:")
          l.foreach(p => println(f"${p._1}%11s lives on floor number ${p._2}"))
        }
      }
    }
}
```

 Solutions: 1
  possible solution:
      Smith lives on floor number 1
    Cooper2 lives on floor number 2
      Baker lives on floor number 3
  Fletcher4 lives on floor number 4
     Miller lives on floor number 5

### Extended task

We can extend this problem by adding a tenant resp. adding conditions:

```Scala
import scala.math.abs

object Dinesman3 extends App {
  val tenants = List("Baker", "Cooper2", "Fletcher4", "Miller", "Rollo5", "Smith")
  val (groundFloor, topFloor) = (1, tenants.size)

  /** Rules with related tenants and restrictions*/
  val exclusions =
    List((suggestedFloor0: Map[String, Int]) => suggestedFloor0("Baker") != topFloor,
      (suggestedFloor1: Map[String, Int]) => suggestedFloor1("Cooper2") != groundFloor,
      (suggestedFloor2: Map[String, Int]) => !List(groundFloor, topFloor).contains(suggestedFloor2("Fletcher4")),
      (suggestedFloor3: Map[String, Int]) => suggestedFloor3("Miller") > suggestedFloor3("Cooper2"),
      (suggestedFloor4: Map[String, Int]) => abs(suggestedFloor4("Smith") - suggestedFloor4("Fletcher4")) != 1,
      (suggestedFloor5: Map[String, Int]) => abs(suggestedFloor5("Fletcher4") - suggestedFloor5("Cooper2")) != 1,

      (suggestedFloor6: Map[String, Int]) => !List(3, 4, topFloor).contains(suggestedFloor6("Rollo5")),
      (suggestedFloor7: Map[String, Int]) => suggestedFloor7("Rollo5") < suggestedFloor7("Smith"),
      (suggestedFloor8: Map[String, Int]) => suggestedFloor8("Rollo5") > suggestedFloor8("Fletcher4"))

  tenants.permutations.map(_ zip (groundFloor to topFloor)).
    filter(p => exclusions.forall(_(p.toMap))).toList match {
      case Nil => println("No solution")
      case xss => {
        println(s"Solutions: ${xss.size}")
        xss.foreach { l =>
          println("possible solution:")
          l.foreach(p => println(f"${p._1}%11s lives on floor number ${p._2}"))
        }
      }
    }
}
```

 Solutions: 1
 possible solution:
      Baker lives on floor number 1
    Cooper2 lives on floor number 2
     Miller lives on floor number 3
  Fletcher4 lives on floor number 4
     Rollo5 lives on floor number 5
      Smith lives on floor number 6

### Enhanced Solution

Combine the rules with the person names and separated the original task with an extension.

```scala
import scala.math.abs

object Dinesman2 extends App {
  val groundFloor = 1

  abstract class Rule(val person: String) { val exclusion: Map[String, Int] => Boolean }

  /** Rules with related tenants and restrictions*/
  def rulesDef(topFloor: Int) = List(
    new Rule("Baker") { val exclusion = (_: Map[String, Int])(person) != topFloor },
    new Rule("Cooper2") { val exclusion = (_: Map[String, Int])(person) != groundFloor },
    new Rule("Fletcher4") {
      val exclusion = (suggestedFloor2: Map[String, Int]) => !List(groundFloor, topFloor).contains(suggestedFloor2(person))
    }, new Rule("Miller") {
      val exclusion = (suggestedFloor3: Map[String, Int]) => suggestedFloor3(person) > suggestedFloor3("Cooper2")
    }, new Rule("Smith") {
      val exclusion = (suggestedFloor4: Map[String, Int]) => abs(suggestedFloor4(person) - suggestedFloor4("Fletcher4")) != 1
    }, new Rule("Fletcher4") {
      val exclusion = (suggestedFloor5: Map[String, Int]) => abs(suggestedFloor5(person) - suggestedFloor5("Cooper2")) != 1
    })

  def extensionDef(topFloor: Int) = List(new Rule("Rollo5") {
    val exclusion = (suggestedFloor6: Map[String, Int]) => !List(3, 4, topFloor).contains((suggestedFloor6: Map[String, Int])(person))
  }, new Rule("Rollo5") {
    val exclusion = (suggestedFloor7: Map[String, Int]) => suggestedFloor7(person) < suggestedFloor7("Smith")
  }, new Rule("Rollo5") {
    val exclusion = (suggestedFloor8: Map[String, Int]) => suggestedFloor8(person) > suggestedFloor8("Fletcher4")
  })

  def allRulesDef(topFloor: Int) = rulesDef(topFloor) ++ extensionDef(topFloor)

  val tenants = allRulesDef(0).map(_.person).distinct // Pilot balloon to get # of tenants
  val topFloor = tenants.size
  val exclusions = allRulesDef(topFloor).map(_.exclusion)

  tenants.permutations.map(_ zip (groundFloor to topFloor)).
    filter(p => exclusions.forall(_(p.toMap))).toList match {
      case Nil => println("No solution")
      case xss => {
        println(s"Solutions: ${xss.size}")
        xss.foreach { l =>
          println("possible solution:")
          l.foreach(p => println(f"${p._1}%11s lives on floor number ${p._2}"))
        }
      }
    }
}
```



## Sidef


### By parsing the problem

```ruby
func dinesman(problem) {
  var lines = problem.split('.')
  var names = lines.first.scan(/\b[A-Z]\w*/)
  var re_names = Regex(names.join('|'))
 
  # Later on, search for these keywords (the word "not" is handled separately).
  var words = %w(first second third fourth fifth sixth seventh eighth ninth tenth
                 bottom top higher lower adjacent)
  var re_keywords = Regex(words.join('|'))
 
  # Build an array of lambda's
  var predicates = lines.ft(1, lines.end-1).map{ |line|
    var keywords = line.scan(re_keywords)
    var (name1, name2) = line.scan(re_names)...
 
    keywords.map{ |keyword|
      var l = do {
        given(keyword) {
            when ("bottom")   { ->(c) { c.first == name1 } }
            when ("top")      { ->(c) { c.last == name1 } }
            when ("higher")   { ->(c) { c.index(name1) > c.index(name2) } }
            when ("lower")    { ->(c) { c.index(name1) < c.index(name2) } }
            when ("adjacent") { ->(c) { c.index(name1) - c.index(name2) -> abs == 1 } }
            default           { ->(c) { c[words.index(keyword)] == name1 } }
        }
      }
      line ~~ /\bnot\b/ ? func(c) { l(c) -> not } : l;  # handle "not"
    }
  }.flat
 
  names.permutations { |*candidate|
    predicates.all { |predicate| predicate(candidate) } && return candidate
  }
}
```


Function invocation:

```ruby
var demo1 = "Abe Ben Charlie David. Abe not second top. not adjacent Ben Charlie.
David Abe adjacent. David adjacent Ben. Last line."

var demo2 = "A B C D. A not adjacent D. not B adjacent higher C. C lower D. Last line"

var problem1 = "Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house that
contains only five floors. Baker does not live on the top floor. Cooper does not live on the bottom floor.
Fletcher does not live on either the top or the bottom floor. Miller lives on a higher floor than does Cooper.
Smith does not live on a floor adjacent to Fletcher's. Fletcher does not live on a floor adjacent to Cooper's.
Where does everyone live?"

var problem2 = "Baker, Cooper, Fletcher, Miller, Guinan, and Smith
live on different floors of an apartment house that contains
only seven floors. Guinan does not live on either the top or the third or the fourth floor.
Baker does not live on the top floor. Cooper
does not live on the bottom floor. Fletcher does not live on
either the top or the bottom floor. Miller lives on a higher
floor than does Cooper. Smith does not live on a floor
adjacent to Fletcher's. Fletcher does not live on a floor
adjacent to Cooper's. Where does everyone live?"

[demo1, demo2, problem1, problem2].each{|problem| say dinesman(problem).join("\n"); say '' }
```

```txt

Ben
David
Abe
Charlie

B
A
C
D

Smith
Cooper
Baker
Fletcher
Miller

Baker
Cooper
Miller
Fletcher
Guinan
Smith

```



### Simple solution

```ruby
var names = %w(Baker Cooper Fletcher Miller Smith)
 
var predicates = [
    ->(c){ :Baker != c.last },
    ->(c){ :Cooper != c.first },
    ->(c){ (:Fletcher != c.first) && (:Fletcher != c.last) },
    ->(c){ c.index(:Miller) > c.index(:Cooper) },
    ->(c){ (c.index(:Smith) - c.index(:Fletcher)).abs != 1 },
    ->(c){ (c.index(:Cooper) - c.index(:Fletcher)).abs != 1 },
]
 
names.permutations { |*candidate|
    if (predicates.all {|predicate| predicate(candidate) }) {
        say candidate.join("\n")
        break
    }
}
```

```txt

Smith
Cooper
Baker
Fletcher
Miller

```



## Tcl

It's trivial to extend this problem to deal with more floors and people and more constraints; the main internally-generated constraint is that the names of people should begin with an upper case character so that they are distinct from internal variables.
This code also relies on the caller encoding the conditions as expressions
that produce a value that is/can be interpreted as a boolean.
```tcl
package require Tcl 8.5
package require struct::list

proc dinesmanSolve {floors people constraints} {
    # Search for a possible assignment that satisfies the constraints
    struct::list foreachperm p $floors {
	lassign $p {*}$people
	set found 1
	foreach c $constraints {
	    if {![expr $c]} {
		set found 0
		break
	    }
	}
	if {$found} break
    }
    # Found something, or exhausted possibilities
    if {!$found} {
	error "no solution possible"
    }
    # Generate in "nice" order
    foreach f $floors {
	foreach person $people {
	    if {[set $person] == $f} {
		lappend result $f $person
		break
	    }
	}
    }
    return $result
}
```


Solve the particular problem:

```tcl
set soln [dinesmanSolve {1 2 3 4 5} {Baker Cooper Fletcher Miller Smith} {
    {$Baker != 5}
    {$Cooper != 1}
    {$Fletcher != 1 && $Fletcher != 5}
    {$Miller > $Cooper}
    {abs($Smith-$Fletcher) != 1}
    {abs($Fletcher-$Cooper) != 1}
}]
puts "Solution found:"
foreach {where who} $soln {puts "   Floor ${where}: $who"}
```


```txt

Solution found:
   Floor 1: Smith
   Floor 2: Cooper
   Floor 3: Baker
   Floor 4: Fletcher
   Floor 5: Miller

```



## uBasic/4tH

<lang>REM Floors are numbered 0 (ground) to 4 (top)

FOR B = 0 TO 4
  FOR C = 0 TO 4
    FOR F = 0 TO 4
      FOR M = 0 TO 4
        FOR S = 0 TO 4
          GOSUB 100 : IF POP() THEN
            GOSUB 110 : IF POP() THEN
              GOSUB 120 : IF POP() THEN
                GOSUB 130 : IF POP() THEN
                  GOSUB 140 : IF POP() THEN
                    GOSUB 150 : IF POP() THEN
                      GOSUB 160 : IF POP() THEN
                        PRINT "Baker lives on floor " ; B + 1
                        PRINT "Cooper lives on floor " ; C + 1
                        PRINT "Fletcher lives on floor " ; F + 1
                        PRINT "Miller lives on floor " ; M + 1
                        PRINT "Smith lives on floor " ; S + 1
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        NEXT S
      NEXT M
    NEXT F
  NEXT C
NEXT B

END

REM "Baker, Cooper, Fletcher, Miller, and Smith live on different floors"
100 PUSH (B#C)*(B#F)*(B#M)*(B#S)*(C#F)*(C#M)*(C#S)*(F#M)*(F#S)*(M#S)
    RETURN

REM "Baker does not live on the top floor"
110 PUSH B#4
    RETURN

REM "Cooper does not live on the bottom floor"
120 PUSH C#0
    RETURN

REM "Fletcher does not live on either the top or the bottom floor"
130 PUSH (F#0)*(F#4)
    RETURN

REM "Miller lives on a higher floor than does Cooper"
140 PUSH M>C
    RETURN

REM "Smith does not live on a floor adjacent to Fletcher's"
150 PUSH ABS(S-F)#1
    RETURN

REM "Fletcher does not live on a floor adjacent to Cooper's"
160 PUSH ABS(F-C)#1
    RETURN
```


Output:

```txt

Baker lives on floor 3
Cooper lives on floor 2
Fletcher lives on floor 4
Miller lives on floor 5
Smith lives on floor 1

0 OK, 0:1442

```


## UNIX Shell

```bash
#!/bin/bash

# NAMES is a list of names.  It can be changed as needed.  It can be more than five names, or less.
NAMES=(Baker Cooper Fletcher Miller Smith)

# CRITERIA are the rules imposed on who lives where.  Each criterion must be a valid bash expression
# that will be evaluated.  TOP is the top floor; BOTTOM is the bottom floor.

# The CRITERIA can be changed to create different rules.

CRITERIA=(
  'Baker    != TOP'            # Baker does not live on the top floor
  'Cooper   != BOTTOM'         # Cooper does not live on the bottom floor
  'Fletcher != TOP'            # Fletcher does not live on the top floor
  'Fletcher != BOTTOM'         # and Fletch also does not live on the bottom floor
  'Miller   >  Cooper'         # Miller lives above Cooper
  '$(abs $(( Smith    - Fletcher )) ) > 1'   # Smith and Fletcher are not on adjacent floors
  '$(abs $(( Fletcher - Cooper   )) ) > 1'   # Fletcher and Cooper are not on adjacent floors
)

# Code below here shouldn't need to change to vary parameters
let BOTTOM=0
let TOP=${#NAMES[@]}-1

# Not available as a builtin
abs() { local n=$(( 10#$1 )) ; echo $(( n < 0 ? -n : n )) ; }

# Algorithm we use to iterate over the permutations
# requires that we start with the array sorted lexically
NAMES=($(printf "%s\n" "${NAMES[@]}" | sort))
while true; do
  # set each name to its position in the array
  for (( i=BOTTOM; i<=TOP; ++i )); do
    eval "${NAMES[i]}=$i"
  done

  # check to see if we've solved the problem
  let solved=1
  for criterion in "${CRITERIA[@]}"; do
    if ! eval "(( $criterion ))"; then
      let solved=0
      break
    fi
  done
  if (( solved )); then
    echo "From bottom to top: ${NAMES[@]}"
    break
  fi

  # Bump the names list to the next permutation
  let j=TOP-1
  while (( j >= BOTTOM )) && ! [[ "${NAMES[j]}" < "${NAMES[j+1]}" ]]; do
    let j-=1
  done
  if (( j < BOTTOM )); then break; fi
  let k=TOP
  while (( k > j )) && [[ "${NAMES[k]}" < "${NAMES[j]}" ]]; do
    let k-=1
  done
  if (( k <= j )); then break; fi
  t="${NAMES[j]}"
  NAMES[j]="${NAMES[k]}"
  NAMES[k]="$t"
  for (( k=1; k<=(TOP-j); ++k )); do
    a=BOTTOM+j+k
    b=TOP-k+1
    if (( a < b )); then
      t="${NAMES[a]}"
      NAMES[a]="${NAMES[b]}"
      NAMES[b]="$t"
    fi
  done
done
```


Sample output:

```txt
From bottom to top: Smith Cooper Baker Fletcher Miller
```



## UTFool



```UTFool

···
http://rosettacode.org/wiki/Dinesman's_multiple-dwelling_problem
···
import java.util.HashSet

■ Dinesman
  § static

    houses⦂ HashSet⟨String⟩°

    ▶ main
    • args⦂ String[]
      · Baker, Cooper, Fletcher, Miller, and Smith …
      build *StringBuilder°, *StringBuilder "BCFMS"
      ∀ house ∈ houses⦂ String
        if verify house
           System.out.println house.toString°

    ▶ verify⦂ boolean
    • house⦂ String
      · Baker does not live on the top floor.
      return false if house.charAt 4 = 'B'
      · Fletcher does not live on either the top or the bottom floor.
      return false if house.charAt 0 = 'F' or house.charAt 4 = 'F'
      · Cooper does not live on the bottom floor.
      return false if house.charAt 0 = 'C'
      · Miller lives on a higher floor than does Cooper.
      return false if house.indexOf "M" ≤ house.indexOf "C"
      · Smith does not live on a floor adjacent to Fletcher's.
      return false if Math.abs (house.indexOf "S") - (house.indexOf "F") = 1
      · Fletcher does not live on a floor adjacent to Cooper's.
      return false if Math.abs (house.indexOf "F") - (house.indexOf "C") = 1
      return true

    ▶ build
    • house⦂ StringBuilder
    • people⦂ StringBuilder
      if people.length° = 0
         houses.add house.toString°
      else
         ∀ i ∈ 0…people.length°
           person⦂ char: people.charAt i
           house.append person
           people.deleteCharAt i
           build house, people
           people.insert i, person
           house.setLength house.length° - 1

```

'''Output''':

```txt
SCBFM
```



## XPL0

```XPL0
include c:\cxpl\codes;
int B, C, F, M, S;
for B:= 1 to 4 do                       \Baker does not live on top (5th) floor
  for C:= 2 to 5 do                     \Cooper does not live on bottom floor
    if C#B then                         \Cooper & Baker live on different floors
      for F:= 2 to 4 do                 \Fletcher doesn't live on top or bottom
        if F#B & F#C & F#C-1 & F#C+1 then \ and she's not adjacent to Cooper
          for M:= 1 to 5 do
            if M#F & M#B & M>C then     \Miller lives above Cooper
              for S:= 1 to 5 do         \Smith is not adjacent to Fletcher
                if S#M & S#F & S#C & S#B & S#F-1 & S#F+1 then       \show
                  [Text(0, "Baker    ");  IntOut(0, B);  CrLf(0);   \all
                   Text(0, "Cooper   ");  IntOut(0, C);  CrLf(0);   \possible
                   Text(0, "Fletcher ");  IntOut(0, F);  CrLf(0);   \solutions
                   Text(0, "Miller   ");  IntOut(0, M);  CrLf(0);
                   Text(0, "Smith    ");  IntOut(0, S);  CrLf(0);
                  ]
```


Output:

```txt

Baker    3
Cooper   2
Fletcher 4
Miller   5
Smith    1

```



## zkl

This solution uses reflection to make the problem statement more "natural". Each dinesman is a variable, whose name is the dinesman name and value is the floor the dinesman lives on. The constraints are then a straight translation of the problem statement. The "each live on a different floors" is implicit by using permutation of the floors.

A floor plan is generated, shoved into the variables and the constraints are run. If they are all true, we have a solution. The first false constraint stops the running of the rest of them (conditional and).

This could be generalized even more by putting the variables and constraint functions in a class, then reflection could be used to automagically get the variables, variable names and constraint functions.

```zkl
var Baker, Cooper, Fletcher, Miller, Smith;  // value == floor
const bottom=1,top=5;	// floors: 1..5
// All live on different floors, enforced by using permutations of floors
//fcn c0{ (Baker!=Cooper!=Fletcher) and (Fletcher!=Miller!=Smith) }
fcn c1{ Baker!=top }
fcn c2{ Cooper!=bottom }
fcn c3{ bottom!=Fletcher!=top }
fcn c4{ Miller>Cooper }
fcn c5{ (Fletcher - Smith).abs() !=1 }
fcn c6{ (Fletcher - Cooper).abs()!=1 }

filters:=T(c1,c2,c3,c4,c5,c6);
dudes:=T("Baker","Cooper","Fletcher","Miller","Smith");  // for reflection
foreach combo in (Utils.Helpers.permuteW([bottom..top].walk())){  // lazy
   dudes.zip(combo).apply2(fcn(nameValue){ setVar(nameValue.xplode()) });
   if(not filters.runNFilter(False)){  // all constraints are True
      vars.println();		       // use reflection to print solution
      break;
   }
}
```

```txt

L(L("Baker",3),L("Cooper",2),L("Fletcher",4),L("Miller",5),L("Smith",1))

```



## ZX Spectrum Basic

```zxbasic
10 REM Floors are numbered 0 (ground) to 4 (top)
20 REM "Baker, Cooper, Fletcher, Miller, and Smith live on different floors":
30 REM "Baker does not live on the top floor"
40 REM "Cooper does not live on the bottom floor"
50 REM "Fletcher does not live on either the top or the bottom floor"
60 REM "Miller lives on a higher floor than does Cooper"
70 REM "Smith does not live on a floor adjacent to Fletcher's"
80 REM "Fletcher does not live on a floor adjacent to Cooper's"
90 FOR b=0 TO 4: FOR c=0 TO 4: FOR f=0 TO 4: FOR m=0 TO 4: FOR s=0 TO 4
100 IF B<>C AND B<>F AND B<>M AND B<>S AND C<>F AND C<>M AND C<>S AND F<>M AND F<>S AND M<>S AND B<>4 AND C<>0 AND F<>0 AND F<>4 AND M>C AND ABS (S-F)<>1 AND ABS (F-C)<>1 THEN PRINT "Baker lives on floor ";b: PRINT "Cooper lives on floor ";c: PRINT "Fletcher lives on floor ";f: PRINT "Miller lives on floor ";m: PRINT "Smith lives on floor ";s: STOP
110 NEXT s: NEXT m: NEXT f: NEXT c: NEXT b
```

