+++
title = "Amb"
description = ""
date = 2019-10-02T07:26:55Z
aliases = []
[extra]
id = 2774
[taxonomies]
categories = []
tags = []
+++

{{task}}
Define and give an example of the Amb operator.

The Amb operator expresses nondeterminism. This doesn't refer to randomness (as in "nondeterministic universe") but is closely related to the term as it is used in automata theory ("non-deterministic finite automaton").

The Amb operator takes a variable number of expressions (or values if that's simpler in the language) and yields a correct one which will satisfy a constraint in some future computation, thereby avoiding failure.

Problems whose solution the Amb operator naturally expresses can be approached with other tools, such as explicit nested iterations over data sets, or with pattern matching. By contrast, the Amb operator appears integrated into the language. Invocations of Amb are not wrapped in any visible loops or other search patterns; they appear to be independent. 

Essentially Amb(x, y, z) splits the computation into three possible futures: a future in which the value x is yielded, a future in which the value y is yielded and a future in which the value z is yielded. The future which leads to a successful subsequent computation is chosen. The other "parallel universes" somehow go away.   Amb called with no arguments fails.

For simplicity, one of the domain values usable with Amb may denote failure, if that is convenient. For instance, it is convenient if a Boolean false denotes failure, so that Amb(false) fails, and thus constraints can be expressed using Boolean expressions like Amb(x * y == 8) which unless x and y add to four.

A pseudo-code program which satisfies this constraint might look like:


```txt
let x = Amb(1, 2, 3)
let y = Amb(7, 6, 4, 5)
Amb(x * y = 8)
print x, y
```


The output is <code>2 4</code> because <code>Amb(1, 2, 3)</code> correctly chooses the future in which <code>x</code> has value <code>2</code>, <code>Amb(7, 6, 4, 5)</code> chooses <code>4</code> and consequently <code>Amb(x * y = 8)</code> produces a success.

Alternatively, failure could be represented using strictly <code>Amb()</code>:


```txt
unless x * y = 8 do Amb()
```


Or else Amb could take the form of two operators or functions: one for producing values and one for enforcing constraints:


```txt
let x = Ambsel(1, 2, 3)
let y = Ambsel(4, 5, 6)
Ambassert(x * y = 8)
print x, y
```


where <code>Ambassert</code> behaves like <code>Amb()</code> if the Boolean expression is false, otherwise it allows the future computation to take place, without yielding any value.

The task is to somehow implement Amb, and demonstrate it with a program which chooses one word from each of the following four sets of character strings to generate a four-word sentence:

#<code>"the" "that" "a"</code>
#<code>"frog" "elephant" "thing"</code>
#<code>"walked" "treaded" "grows"</code>
#<code>"slowly" "quickly"</code>

The constraint to be satisfied is that the last character of each word (other than the last) is the same as the first character of its successor.

The only successful sentence is <code>"that thing grows slowly"</code>; other combinations do not satisfy the constraint and thus fail.

The goal of this task isn't to simply process the four lists of words with explicit, deterministic program flow such as nested iteration, to trivially demonstrate the correct output. The goal is to implement the Amb operator, or a facsimile thereof that is possible within the language limitations.

=={{Header|11l}}==
{{trans|Nim}}

```11l
F amb(comp, options, prev = ‘’) -> Array[String]
   I options.empty
      R []

   L(opt) options[0]
      // If this is the base call, prev is empty and we need to continue.
      I prev != ‘’ & !comp(prev, opt)
         L.continue

      // Take care of the case where we have no options left.
      I options.len == 1
         R [opt]

      // Traverse into the tree.
      V res = amb(comp, options[1..], opt)

      // If it was a failure, try the next one.
      if !res.empty
         R opt [+] res // We have a match

   R []

V sets = [[‘the’, ‘that’, ‘a’],
          [‘frog’, ‘elephant’, ‘thing’],
          [‘walked’, ‘treaded’, ‘grows’],
          [‘slowly’, ‘quickly’]]

V result = amb((s, t) -> s.last == t[0], sets)
print(result.join(‘ ’))
```

{{out}}

```txt
that thing grows slowly
```


=={{Header|Ada}}==

```ada
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;            use Ada.Text_IO;

procedure Test_Amb is
   type Alternatives is array (Positive range <>) of Unbounded_String;

   type Amb (Count : Positive) is record
      This : Positive := 1;
      Left : access Amb; 
      List : Alternatives (1..Count);
   end record;
   
   function Image (L : Amb) return String is
   begin
      return To_String (L.List (L.This));
   end Image;

   function "/" (L, R : String) return Amb is
      Result : Amb (2);
   begin
      Append (Result.List (1), L);
      Append (Result.List (2), R);
      return Result;
   end "/";
   
   function "/" (L : Amb; R : String) return Amb is
      Result : Amb (L.Count + 1);
   begin
      Result.List (1..L.Count) := L.List ;
      Append (Result.List (Result.Count), R);
      return Result;
   end "/";

   function "=" (L, R : Amb) return Boolean is
      Left : Unbounded_String renames L.List (L.This);
   begin
      return Element (Left, Length (Left)) = Element (R.List (R.This), 1);
   end "=";
   
   procedure Failure (L : in out Amb) is
   begin
      loop
         if L.This < L.Count then
            L.This := L.This + 1;
         else
            L.This := 1;
            Failure (L.Left.all);
         end if;
         exit when L.Left = null or else L.Left.all = L;
      end loop;
   end Failure;

   procedure Join (L : access Amb; R : in out Amb) is
   begin
      R.Left := L;
      while L.all /= R loop
         Failure (R);
      end loop;
   end Join;

   W_1 : aliased Amb := "the" / "that" / "a";
   W_2 : aliased Amb := "frog" / "elephant" / "thing";
   W_3 : aliased Amb := "walked" / "treaded" / "grows";
   W_4 : aliased Amb := "slowly" / "quickly";
begin
   Join (W_1'Access, W_2);
   Join (W_2'Access, W_3);
   Join (W_3'Access, W_4);
   Put_Line (Image (W_1) & ' ' & Image (W_2) & ' ' & Image (W_3) & ' ' & Image (W_4));
end Test_Amb;
```

The type Amb is implemented with the operations "/" to construct it from strings. 
Each instance keeps its state. 
The operation Failure performs back tracing. Join connects two elements into a chain. 
The implementation propagates Constraint_Error when matching fails. 
{{out}}

```txt

that thing grows slowly

```



## ALGOL 68

{{wont work with|ALGOL 68|Revision 1 - '''iterpage''' exported out of scope}}
{{wont work with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny] - '''iterpage''' exported out of scope }}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}
Note: This program violates ALGOL 68's scoping rules when a locally scoped procedure is returned to a more global scope.  [[ELLA ALGOL 68RS]] misses this violation, but [[ALGOL 68 Genie]] spots it at run time and then produces an assert.  However [[ELLA ALGOL 68RS]] does produce the desired result, but may potentially suffer from "mysterious" stack problems.

```algol68
MODE PAGE = FLEX[0]STRING;
MODE YIELDPAGE = PROC(PAGE)VOID;
MODE ITERPAGE = PROC(YIELDPAGE)VOID;

OP INITITERPAGE = (PAGE self)ITERPAGE: 
  (YIELDPAGE yield)VOID: # scope violation #
    FOR i TO UPB self DO
      yield(self[i])
    OD;
      
OP + = (ITERPAGE for strings, PAGE b)ITERPAGE:
  (YIELDPAGE yield)VOID: # scope violation #
    for strings((PAGE amb)VOID:(
      [UPB amb + 1]STRING joined; 
      joined[:UPB amb] := amb;
      STRING last string := amb[UPB amb];
      CHAR last char := last string[UPB last string];
      FOR i TO UPB b DO
        IF last char = b[i][1] THEN
          joined[UPB joined] := b[i];
          yield(joined)
        FI
      OD
    ));

OP + = (PAGE a, PAGE b)ITERPAGE: INITITERPAGE a + b;

ITERPAGE gen amb := 
   PAGE("the", "that", "a") +
   PAGE("frog", "elephant", "thing") +
   PAGE("walked", "treaded", "grows") +
   PAGE("slowly", "quickly");

PAGE sep;
#FOR PAGE amb IN # gen amb( # ) DO #
##  (PAGE amb)VOID:
    print((amb[1]+" "+amb[2]+" "+amb[3]+" "+amb[4], new line))
#OD# )
```

{{out}}

```txt

that thing grows slowly

```


=={{Header|ATS}}==

```ATS

(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#include
"share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)
//
staload "libats/ML/SATS/monad_list.sats"
staload _ = "libats/ML/DATS/monad_list.dats"
//
(* ****** ****** *)
//
datatype
words =
  | Sing of stringGt(0)
  | Comb of (words, words)
//
(* ****** ****** *)
//
extern
fun words_get_beg(words): char
extern
fun words_get_end(words): char
//
(* ****** ****** *)
//
implement
words_get_beg(w0) =
(
case+ w0 of
| Sing(cs) => cs[0]
| Comb(w1, w2) => words_get_beg(w1)
)
//
implement
words_get_end(w0) =
(
case+ w0 of
| Sing(cs) => cs[pred(length(cs))]
| Comb(w1, w2) => words_get_end(w2)
)
//
(* ****** ****** *)
//
fun
words_comb
(
  w1: words, w2: words
) : list0(words) =
  if (words_get_end(w1)=words_get_beg(w2))
    then list0_sing(Comb(w1, w2)) else list0_nil()
//
(* ****** ****** *)
//
extern
fun
fprint_words: fprint_type(words)
//
overload fprint with fprint_words
//
implement
fprint_words(out, ws) =
(
case+ ws of
| Sing(w) => fprint(out, w)
| Comb(w1, w2) => fprint!(out, w1, ' ', w2)
)
//
implement fprint_val<words> = fprint_words
//
(* ****** ****** *)
//
typedef
a = stringGt(0) and b = words
//
val ws1 =
  $list{a}("this", "that", "a")
val ws1 =
  list_map_fun<a><b>(ws1, lam(x) => Sing(x))
val ws1 = monad_list_list(list0_of_list_vt(ws1))
//
val ws2 =
  $list{a}("frog", "elephant", "thing")
val ws2 =
  list_map_fun<a><b>(ws2, lam(x) => Sing(x))
val ws2 = monad_list_list(list0_of_list_vt(ws2))
//
val ws3 =
  $list{a}("walked", "treaded", "grows")
val ws3 =
  list_map_fun<a><b>(ws3, lam(x) => Sing(x))
val ws3 = monad_list_list(list0_of_list_vt(ws3))
//
val ws4 =
  $list{a}("slowly", "quickly")
val ws4 =
  list_map_fun<a><b>(ws4, lam(x) => Sing(x))
val ws4 = monad_list_list(list0_of_list_vt(ws4))
//
(* ****** ****** *)
//
val
ws12 =
monad_bind2<b,b><b>
  (ws1, ws2, lam (w1, w2) => monad_list_list(words_comb(w1, w2)))
val
ws123 =
monad_bind2<b,b><b>
  (ws12, ws3, lam (w12, w3) => monad_list_list(words_comb(w12, w3)))
val
ws1234 =
monad_bind2<b,b><b>
  (ws123, ws4, lam (w123, w4) => monad_list_list(words_comb(w123, w4)))
//
(* ****** ****** *)

implement main0 () =
{
  val () = fprintln! (stdout_ref, "ws1234 = ", ws1234)
}

(* ****** ****** *)

```


=={{Header|AutoHotkey}}==
{{AutoHotkey case}}


Source: [http://www.autohotkey.com/forum/topic45454.html AMB - Ambiguous selector] by infogulch

```autohotkey
set1 := "the that a" 
set2 := "frog elephant thing" 
set3 := "walked treaded grows" 
set4 := "slowly quickly" 

MsgBox % amb( "", set1, set2, set3, set4 ) 
; this takes a total of 17 iterations to complete 

amb( char = "", set1 = "", set2 = "", set3 = "", set4 = "" ) 
{ ; original call to amb must leave char param blank 
  Loop, Parse, set1, %A_Space% 
    If (char = (idxchar := SubStr(A_LoopField, 1, 1)) && set2 = "" 
    || (char = idxchar || char = "") && ((retval:= amb(SubStr(A_LoopField, 0, 1), set2, set3, set4)) != "")) 
      Return A_LoopField " " retval 
  Return "" 
}
```


=={{Header|Bracmat}}==

```bracmat
( ( Amb
  =   first last list words word solution
    .   !arg:(?first.?list)
      & ( !list:
        |   !list:(.?words) ?list
          &   !words
            :   ?
                %( @(?word:!first ? @?last)
                 & Amb$(!last.!list):?solution
                 & !word !solution:?solution
                 )
                ?
          & !solution
        )
  )
&   Amb
  $ (
    .   (.the that a)
        (.frog elephant thing)
        (.walked treaded grows)
        (.slowly quickly)
    )
)
```



```txt
that thing grows slowly
```


=={{Header|C}}==
Note: This uses the continuations code from https://web.archive.org/web/20120619201518/http://homepage.mac.com:80/sigfpe/Computing/continuations.html


```c
typedef const char * amb_t;

amb_t amb(size_t argc, ...)
{
  amb_t *choices;
  va_list ap;
  int i;
  
  if(argc) {
    choices = malloc(argc*sizeof(amb_t));
    va_start(ap, argc);
    i = 0;
    do { choices[i] = va_arg(ap, amb_t); } while(++i < argc);
    va_end(ap);
    
    i = 0;
    do { TRY(choices[i]); } while(++i < argc);
    free(choices);
  }
  
  FAIL;
}

int joins(const char *left, const char *right) { return left[strlen(left)-1] == right[0]; }

int _main() {
  const char *w1,*w2,*w3,*w4;
  
  w1 = amb(3, "the", "that", "a");
  w2 = amb(3, "frog", "elephant", "thing");
  w3 = amb(3, "walked", "treaded", "grows");
  w4 = amb(2, "slowly", "quickly");
  
  if(!joins(w1, w2)) amb(0);
  if(!joins(w2, w3)) amb(0);
  if(!joins(w3, w4)) amb(0);
  
  printf("%s %s %s %s\n", w1, w2, w3, w4);
  
  return EXIT_SUCCESS;
}
```



## C sharp

The implementation of the Amb class

```csharp
using System;
using System.Collections.Generic;

public class Amb : IDisposable
{
    List<IValueSet> streams = new List<IValueSet>();
    List<IAssertOrAction> assertsOrActions = new List<IAssertOrAction>();
    volatile bool stopped = false;

    public IAmbValue<T> DefineValues<T>(params T[] values)
    {
        return DefineValueSet(values);
    }

    public IAmbValue<T> DefineValueSet<T>(IEnumerable<T> values)
    {
        ValueSet<T> stream = new ValueSet<T>();
        stream.Enumerable = values;
        streams.Add(stream);
        return stream;
    }

    public Amb Assert(Func<bool> function)
    {
        assertsOrActions.Add(new AmbAssert()
        {
            Level = streams.Count,
            IsValidFunction = function
        });
        return this;
    }

    public Amb Perform(Action action)
    {
        assertsOrActions.Add(new AmbAction()
        {
            Level = streams.Count,
            Action = action
        });
        return this;
    }

    public void Stop()
    {
        stopped = true;
    }

    public void Dispose()
    {
        RunLevel(0, 0);
        if (!stopped)
        {
            throw new AmbException();
        }
    }

    void RunLevel(int level, int actionIndex)
    {
        while (actionIndex < assertsOrActions.Count && assertsOrActions[actionIndex].Level <= level)
        {
            if (!assertsOrActions[actionIndex].Invoke() || stopped)
                return;
            actionIndex++;
        }
        if (level < streams.Count)
        {
            using (IValueSetIterator iterator = streams[level].CreateIterator())
            {
                while (iterator.MoveNext())
                {
                    RunLevel(level + 1, actionIndex);
                }
            }
        }
    }

    interface IValueSet
    {
        IValueSetIterator CreateIterator();
    }

    interface IValueSetIterator : IDisposable
    {
        bool MoveNext();
    }

    interface IAssertOrAction
    {
        int Level { get; }
        bool Invoke();
    }

    class AmbAssert : IAssertOrAction
    {
        internal int Level;
        internal Func<bool> IsValidFunction;

        int IAssertOrAction.Level { get { return Level; } }

        bool IAssertOrAction.Invoke()
        {
            return IsValidFunction();
        }
    }

    class AmbAction : IAssertOrAction
    {
        internal int Level;
        internal Action Action;

        int IAssertOrAction.Level { get { return Level; } }

        bool IAssertOrAction.Invoke()
        {
            Action(); return true;
        }
    }

    class ValueSet<T> : IValueSet, IAmbValue<T>, IValueSetIterator
    {
        internal IEnumerable<T> Enumerable;
        private IEnumerator<T> enumerator;

        public T Value { get { return enumerator.Current; } }

        public IValueSetIterator CreateIterator()
        {
            enumerator = Enumerable.GetEnumerator();
            return this;
        }

        public bool MoveNext()
        {
            return enumerator.MoveNext();
        }

        public void Dispose()
        {
            enumerator.Dispose();
        }
    }
}

public interface IAmbValue<T>
{
    T Value { get; }
}

public class AmbException : Exception
{
    public AmbException() : base("AMB is angry") { }
}
```


Usage:

```csharp
    // original problem
    using (Amb amb = new Amb())
    {
        var set1 = amb.DefineValues("the", "that", "a");
        var set2 = amb.DefineValues("frog", "elephant", "thing");
        var set3 = amb.DefineValues("walked", "treaded", "grows");
        var set4 = amb.DefineValues("slowly", "quickly");

        amb.Assert(() => IsJoinable(set1.Value, set2.Value));
        amb.Assert(() => IsJoinable(set2.Value, set3.Value));
        amb.Assert(() => IsJoinable(set3.Value, set4.Value));

        amb.Perform(() =>
            {
                System.Console.WriteLine("{0} {1} {2} {3}", set1.Value, set2.Value, set3.Value, set4.Value);
                amb.Stop();
            });
    }
    // problem from http://www.randomhacks.net/articles/2005/10/11/amb-operator
    using (Amb amb = new Amb())
    {
        IAmbValue<int> x = amb.DefineValues(1, 2, 3);
        IAmbValue<int> y = amb.DefineValues(4, 5, 6);
        amb.Assert(() => x.Value * y.Value == 8);
        amb.Perform(() =>
            {
                System.Console.WriteLine("{0} {1}", x.Value, y.Value);
                amb.Stop();
            });
    }
```

The following is a more idiomatic and not (or less) idiosyncratic C# version of Amb. The above uses a clever but unorthodox use of Dispose() to launch the backtracking (and a few other interesting quirks). Interesting but it can throw an exception in Dispose() which is strongly discouraged in C#. 

The following was written independently but borrows some ideas/help from the previous solution. There are still limitations compared to the spirit of Amb, requiring an explicit run (called Disambiguate() normally but RequireFinal() - which calls Disambiguate() internally -is used to get closer to the spirit here) but, compared to many other language solutions here, it does have the explicit Require, meaning it is  a general solution, not tied to the specific example in this task.(I suggest the task description is updated to ensure that a general amb operator is provided rather than a custom one for the single provided example). It uses a ToString override to return Value.ToString(), again to help in the spirit of things, but if the variables were used directly, one would have to be use the Value property instead. 

Also the internal algorithm allows manual external tuning minimising the verification steps required. This is shown in the ordering of the choices and requirements in the problem to be solved. This, I think, is closer to the spirit of Amb, as defined here, although really this is quite different to McCarthy's class of ambiguous functions.     
{{works with|C sharp|C#|7.1}}
<!-- By Martin Freedman, 17/01/2018 -->

```csharp
using System;
using System.Collections.Generic;

namespace Amb
{
    public interface IValue<T>
    {
        T Value { get; }
        string ToString();
    }

    public sealed class Amb
    {
        public IValue<T> Choose<T>(params T[] choices)
        {
            var array = new ChoiceArray<T> { Values = choices };
            _itemsChoices.Add(array);
            return array;
        }

        public void Require(Func<bool> predicate) =>
            _constraints.Add(new Constraint { Predicate = predicate, AppliesForItems = _itemsChoices.Count });

        public bool RequireFinal(Func<bool> predicate)
        {
            Require(predicate);
            return Disambiguate();
        }

        public bool Disambiguate()
        {
            try
            {
                Disambiguate(0, 0);
                return false;
            }
            catch (Exception ex) when (ex.Message == "Success")
            {
                return true;
            }
        }

        interface IChoices
        {
            int Length { get; }
            int Index { get; set; }
        }

        interface IConstraint
        {
            int AppliesForItems { get; }
            bool Invoke();
        }

        List<IChoices> _itemsChoices = new List<IChoices>();
        List<IConstraint> _constraints = new List<IConstraint>();

        void Disambiguate(int itemsTracked, int constraintIndex)
        {
            while (constraintIndex < _constraints.Count && _constraints[constraintIndex].AppliesForItems <= itemsTracked)
            {
                if (!_constraints[constraintIndex].Invoke())
                    return;
                constraintIndex++;
            }

            if (itemsTracked == _itemsChoices.Count)
            {
                throw new Exception("Success");
            }
                
            for (var i = 0; i < _itemsChoices[itemsTracked].Length; i++)
            {
                 _itemsChoices[itemsTracked].Index = i;
                 Disambiguate(itemsTracked + 1, constraintIndex);
            }
        }

        class Constraint : IConstraint
        {
            internal int AppliesForItems;
            int IConstraint.AppliesForItems => AppliesForItems;

            internal Func<bool> Predicate;
            public bool Invoke() => Predicate?.Invoke() ?? default;
        }

        class ChoiceArray<T> : IChoices, IValue<T>
        {
            internal T[] Values;

            public int Index { get; set; }

            public T Value { get { return Values[Index]; } }

            public int Length => Values.Length;

            public override string ToString() => Value.ToString();
        }
    }
}
```

Usage:

```csharp
using System.Linq;
using static System.Console;

namespace Amb
{
    class Program
    {
        static void Main(string[] args)
        {

            var amb = new Amb();

            var set1 = amb.Choose("the", "that", "a");
            var set2 = amb.Choose("frog", "elephant", "thing");
            amb.Require(() => set1.Value.Last() == set2.Value[0]);            
            var set3 = amb.Choose("walked", "treaded", "grows");
            amb.Require(() => set2.Value.Last() == set3.Value[0]);
            var set4 = amb.Choose("slowly", "quickly");            
            amb.RequireFinal(() => set3.Value.Last() == set4.Value[0]);

            WriteLine($"{set1} {set2} {set3} {set4}");
            Read();

            // problem from http://www.randomhacks.net/articles/2005/10/11/amb-operator
            amb = new Amb();

            var x = amb.Choose(1, 2, 3);
            var y = amb.Choose(4, 5, 6);
            amb.RequireFinal(() => x.Value* y.Value == 8);

            WriteLine($"{x} * {y} = 8");
            Read();
            Read();
        }
    }
}
```

Output:

```txt
that thing grows slowly

2 * 4 = 8
```



## Clojure


```clojure
(ns amb
  (:use clojure.contrib.monads))

(defn amb [wss]
  (let [valid-word (fn [w1 w2]
                     (if (and w1 (= (last w1) (first w2)))
                       (str w1 " " w2)))]
    (filter #(reduce valid-word %)
            (with-monad sequence-m (m-seq wss)))))

amb> (amb '(("the" "that" "a") ("frog" "elephant" "thing") ("walked" "treaded" "grows") ("slowly" "quickly")))
(("that" "thing" "grows" "slowly"))

```



## Common Lisp

Common Lisp lacks the <code>call/cc</code> present in Scheme, and so the straightforward implementation using continuations would require a full-blown code walker (and could still have some issues with dynamically bound variables).  A workable compromise uses the condition system and some convenience macros to define <code>amblet</code> a binding construct like <code>let</code> except that if a variable's init-form is of the form <code>(amb {form}*)</code> the <code>amblet</code>'s body will be evaluated with the variable bound to successive values produced by each <code>form</code> until some evaluation does not signal an <code>amb-error</code>.


```lisp
(define-condition amb-failure () ()
  (:report "No amb alternative succeeded."))

(defun invoke-ambiguously (function thunks)
  "Call function with successive values produced by successive
functions in thunks until some invocation of function does not signal
an amb-failure."
  (do ((thunks thunks (rest thunks)))
      ((endp thunks) (error 'amb-failure))
    (let ((argument (funcall (first thunks))))
      (handler-case (return (funcall function argument))
        (amb-failure ())))))

(defmacro amblet1 ((var form) &body body)
  "If form is of the form (amb {form}*) then amblet1 is a convenient
syntax for invoke-ambiguously, by which body is evaluated with var
bound the results of each form until some evaluation of body does not
signal an amb-failure. For any other form, amblet binds var the result
of form, and evaluates body."
  (if (and (listp form) (eq (first form) 'amb))
    `(invoke-ambiguously
      #'(lambda (,var) ,@body)
      (list ,@(loop for amb-form in (rest form)
                    collecting `#'(lambda () ,amb-form))))
    `(let ((,var ,form))
       ,@body)))

(defmacro amblet (bindings &body body)
  "Like let, except that if an init-form is of the form (amb {form}*),
then the corresponding var is bound with amblet1."
  (if (endp bindings)
    `(progn ,@body)
    `(amblet1 ,(first bindings)
       (amblet ,(rest bindings)
         ,@body))))
```


Example:


```txt
> (flet ((string-adjacent (s1 s2)
           (char= (char s1 (1- (length s1)))
                  (char s2 0))))
    (amblet ((w1 (amb "the" "that" "a"))
             (w2 (amb "frog" "elephant" "thing"))
             (w3 (amb "walked" "treaded" "grows"))
             (w4 (amb "slowly" "quickly")))
      (if (and (string-adjacent w1 w2)
               (string-adjacent w2 w3)
               (string-adjacent w3 w4))
        (list w1 w2 w3 w4)
        (signal 'amb-failure))))
("that" "thing" "grows" "slowly")
```


### Macro with dynamic variables


```lisp
(defparameter *amb-ops* nil)
(defparameter *amb-hist* nil)

(setf *random-state* (make-random-state t))
(defun shuffle (items)
  (loop for i from 0 with r = items with l = (length r) while (< i l) do
	(rotatef (elt r i) (elt r (+ i (random (- l i)))))
	finally (return r)))

;;; (assert '(mess in, mess out))
(defmacro amb (a &rest rest)
  (let ((f (first rest))
	(rest (rest rest)))
    (if (not f)
      `(let ((items (shuffle ,a)))
	   (let ((y (car (last *amb-hist*)))
		 (o (car (last *amb-ops*))))
	     (loop for x in items do
		   (if (or (not *amb-ops*)
			   (funcall o y x))
			   (return (append *amb-hist* (list x))))
	   (elt items (random (length items))))))

      `(let ((items (shuffle ,a)))
	   (let ((y (car (last *amb-hist*)))
		 (o (car (last *amb-ops*))))
	     (loop for x in items do
		   (if (or (not *amb-ops*)
			   (funcall o y x))
		     (let ((*amb-hist* (append *amb-hist* (list x)))
			   (*amb-ops*  (append *amb-ops* (list ,f))))
		       (let ((r ,@rest))
			 (if r (return r)))))))))))

;; test cases
(defun joins (a b)
  (char= (char a (1- (length a))) (char b 0)))

(defun w34()
  (amb '("walked" "treaded" "grows") #'joins
       (amb '("slowly" "quickly"))))

(print
  (amb '("the" "that" "a") #'joins
       (amb '("frog" "elephant" "thing") #'joins
	    (w34))))

(print (amb '(1 2 5) #'<
	    (amb '(2 3 4) #'=
		 (amb '(3 4 5))))) ; 1 4 4, 2 3 3, etc
```



## D


```d
import std.stdio, std.array;

/** This amb function takes a comparison function and
the possibilities that need to be checked.*/
//string[] amb(in bool function(in string, in string) pure comp,
const(string)[] amb(in bool function(in string, in string) pure comp,
                    in string[][] options,
                    in string prev = null) pure {
    if (options.empty)
        return null;

    foreach (immutable opt; options.front) {
        // If this is the base call, prev is null and we need to
        // continue.
        if (!prev.empty && !comp(prev, opt))
            continue;

        // Take care of the case where we have no options left.
        if (options.length == 1)
            return [opt];

        // Traverse into the tree.
        const res = amb(comp, options[1 .. $], opt);

        // If it was a failure, try the next one.
        if (!res.empty)
            return opt ~ res; // We have a match!
    }

    return null; // No matches.
}

void main() {
    immutable sets = [["the", "that", "a"],
                      ["frog", "elephant", "thing"],
                      ["walked", "treaded", "grows"],
                      ["slowly", "quickly"]];

    // Pass in the comparator and the available sets.
    // (The comparator is not nothrow because of UTF.)
    const result = amb((s, t) => s.back == t.front, sets);

    if (result.empty)
        writeln("No matches found!");
    else
        writefln("%-(%s %)", result);
}
```

{{out}}

```txt
that thing grows slowly
```



## E

{{lines too long|E}}
E does not currently have any kind of backtracking control flow (though there is a proposal in the works to backtrack upon exceptions, for the sake of consistency). However, since (Almost) Everything Is Message Passing, we can create an object which represents a set of possible values.

This is complicated, however, by the fact that any given amb must appear to produce only one result; that is, <code>def x := amb(["a", "b"]); x + x</code> produces aa or bb, not aa,bb,ab,ba as <code>amb(["a", "b"]) + amb(["a", "b"])</code> would. Therefore, each choice is associated with the ''decisions'' which produced it: a map from ''amb'' objects to which member of them was chosen; any combination of two ambs discards any combination of choices which have inconsistent decisions.

Note that the choices are not evaluated lazily; this is a breadth-first rather than depth-first search through possibilities. Also, every amb remembers all of the ambs which produced it. As such, this is probably not a practical system for large problems.


```e
pragma.enable("accumulator")

def [amb, unamb] := { # block hides internals

  def Choice := Tuple[any, Map]

  def [ambS, ambU] := <elib:sealing.makeBrand>("amb")
  var counter := 0 # Used just for printing ambs

  /** Check whether two sets of decisions are consistent */
  def consistent(decA, decB) {
    def overlap := decA.domain() & decB.domain()
    for ambObj in overlap {
      if (decA[ambObj] != decB[ambObj]) { return false }
    }
    return true
  }

  /** From an amb object, extract the possible choices */
  def getChoices(obj, decisions) :List[Choice] {
    if (decisions.maps(obj)) {
      return [[decisions[obj], decisions]]
    } else if (ambU.amplify(obj) =~ [[choices, _]]) {
      return accum [] for [chosen, dec] ? (consistent(decisions, dec)) in choices { _ + getChoices(chosen, (decisions | dec).with(obj, chosen)) }
    } else {
      return [[obj, decisions]]
    }
  }
  
  /** Construct an amb object with remembered decisions */
  def ambDec(choices :List[Choice]) {
    def serial := (counter += 1)
    def ambObj {
      to __printOn(out) {
        out.print("<amb(", serial, ")")
        for [chosen, decisions] in choices {
          out.print(" ", chosen)
          for k => v in decisions {
            out.print(";", ambU.amplify(k)[0][1], "=", v)
          }
        }
        out.print(">")
      }
      to __optSealedDispatch(brand) {
        if (brand == ambS.getBrand()) {
          return ambS.seal([choices, serial])
        }
      }
      match [verb, args] {
        var results := []
        for [rec, rdec] in getChoices(ambObj, [].asMap()) {
          def expandArgs(dec, prefix, choosing) {
            switch (choosing) {
               match [] { results with= [E.call(rec, verb, prefix), dec] }
               match [argAmb] + moreArgs {
                 for [arg, adec] in getChoices(argAmb, dec) {
                   expandArgs(adec, prefix.with(arg), moreArgs)
                 }
               }
            }
          }
          expandArgs(rdec, [], args)
        }
        ambDec(results)
      }
    }
    return ambObj
  }
  
  /** Construct an amb object with no remembered decisions. (public interface) */
  def amb(choices) {
    return ambDec(accum [] for c in choices { _.with([c, [].asMap()]) })
  }

  /** Get the possible results from an amb object, discarding decision info. (public interface) */
  def unamb(ambObj) {
    return accum [] for [c,_] in getChoices(ambObj, [].asMap()) { _.with(c) }
  }
  
  [amb, unamb]
}
```



```e
def join(a, b) {
  # This must not use the builtin if, since it coerces to boolean rather than passing messages.
  # false.pick(x, y) returns y and true.pick(x, y) returns x; we protect the amb([]) from causing
  # unconditional failure by putting both options in functions.
  # <=> is the comparison operator that happens to be message-based.
  return (a.last() <=> b[0]).pick(fn { 
    a + " " + b
  }, fn {
    amb([])
  })()
}

def w1 := amb(["the", "that", "a"           ])
def w2 := amb(["frog", "elephant", "thing"  ])
def w3 := amb(["walked", "treaded", "grows" ])
def w4 := amb(["slowly", "quickly"          ])

unamb(join(join(join(w1, w2), w3), w4))
```



;Comparison with Haskell:
This can be compared with the Haskell use of lists as a monad to represent choice.
* Haskell uses lazy evaluation; E does not. This implementation does not simulate lazy evaluation with thunks; it is eager (computes every intermediate choice before continuing) and therefore inefficient if you only need one successful result.
* Haskell does not need to track decisions. This is because when using a monad in Haskell, the points of choice are explicitly written, either by monadic operators or combinators. The analogues to the two "ab" operations given above are: <code>do x <- ["a","b"]; return (x ++ x)</code> and <code>do x <- ["a","b"]; y <- ["a","b"]; return (x ++ y)</code> — the relevant difference being the number of <code>&lt;-</code> operators. In this implementation, we instead absorb the choice into normal method calls; the Haskell analogue would be something like <code>instance Monoid a => Monoid (Amb a) where Amb ... `mconcat` Amb ... = ...</code>, which would have a similar need to track decisions.


## Egison


```egison

; We don't need 'amb' in the code since pattern-matching of Egison automatically do backtracking.
(match-all {{"the" "that" "a"} {"frog" "elephant" "thing"} {"walked" "treaded" "grows"} {"slowly" "quickly"}} (list (multiset string))
  [<cons <cons (& <snoc $c_1 _> $w_1) _>
         (loop $i [2 $n]
           <cons <cons (& <cons ,c_(- i 1) <snoc $c_i _>> $w_i) _> ...>
           <nil>)>
   (map (lambda [$i] w_i) (between 1 n))])

```

{{out}}

```egison

{{"that" "thing" "grows" "slowly"}}

```



## Ela

{{incorrect|Ela| The comparison is hard-coded into amb}}

```ela
open list core

amb xs = x where 
  (Some x) = & join xs ""
  join (x::xs) = amb' x (join xs)
  join [] = \_ -> Some ""
  eq' [] x = true
  eq' w x  = last w == head x
  amb' [] _ _ = None
  amb' (x::xs) n w 
    | eq' w x =
    match n x with
          Some v = Some (x ++ " " ++ v)
          _ = amb' xs n w
    | else = amb' xs n w
```


Usage:


```ela
amb [
       ["the","that","a"]
      ,["frog","elephant","thing"]
      ,["walked","treaded","grows"]
      ,["slowly","quickly"]
    ]
```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;
import extensions'routines;

joinable(former,later) = (former[former.Length - 1] == later[0]);

dispatcher =
{
    eval(object a, Func2 f)
    {
        ^ f(a[0],a[1])
    }

    eval(object a, Func3 f)
    {
        ^ f(a[0], a[1],a[2])
    }
    
    eval(object a, Func4 f)
    {
        ^ f(a[0],a[1],a[2],a[3])
    }
    
    eval(object a, Func5 f)
    {
        ^ f(a[0],a[1],a[2],a[3],a[4])
    }
};

class AmbValueCollection
{
    object theCombinator;
    
    constructor new(params object[] args)
    {
        theCombinator := SequentialEnumerator.new(params args)
    }

    seek(cond)
    {
        theCombinator.reset();

        theCombinator.seekEach:(v => dispatcher.eval(v,cond))
    }
    
    do(f)
    {
        var result := theCombinator.get();
        if (nil != result)
        {
            dispatcher.eval(result,f) 
        }
        else
        {
            InvalidArgumentException.raise()
        }
    }
}

singleton ambOperator
{
    for(params object[] args)
        = AmbValueCollection.new(params args);
}

public program()
{
    try
    {
        ambOperator 
            .for(new::("the","that","a"),new::("frog", "elephant", "thing"),new::("walked", "treaded", "grows"), 
                 new::("slowly", "quickly"))
            .seek:(a,b,c,d => joinable(a,b) && joinable(b,c) && joinable(c,d) )
            .do:(a,b,c,d) { console.printLine(a," ",b," ",c," ",d) }
    }
    catch(Exception e)
    {
        console.printLine:"AMB is angry"
    };
        
    console.readChar()
}
```

{{out}}

```txt

that thing grows slowly

```



## ERRE

{{incorrect|ERRE| Conditional is hard-coded into amb}}


```ERRE

PROGRAM AMB

!
! for rosettacode.org
!

!$KEY

DIM SET1$[2],SET2$[2],SET3$[2],SET4$[2]

FUNCTION WORDS_OK(STRING1$,STRING2$)
  WORDS_OK=(RIGHT$(STRING1$,1)=LEFT$(STRING2$,1))
END FUNCTION

PROCEDURE AMB(SET1$[],SET2$[],SET3$[],SET4$[]->RESULT$)
  RESULT$="" ! Empty string, e.g. fail
  FOR A=0 TO 2 DO
    FOR B=0 TO 2 DO
      FOR C=0 TO 2 DO
        FOR D=0 TO 2 DO
          IF WORDS_OK(SET1$[A],SET2$[B]) AND WORDS_OK(SET2$[B],SET3$[C]) AND WORDS_OK(SET3$[C],SET4$[D]) THEN
            RESULT$=SET1$[A]+" "+SET2$[B]+" "+SET3$[C]+" "+SET4$[D]
            EXIT PROCEDURE
          END IF
        END FOR
      END FOR
    END FOR
  END FOR
END PROCEDURE

BEGIN
  PRINT(CHR$(12);)   ! CLS
  SET1$[0]="the"     SET1$[1]="that"     SET1$[2]="a"
  SET2$[0]="frog"    SET2$[1]="elephant" SET2$[2]="thing"
  SET3$[0]="walked"  SET3$[1]="treaded"  SET3$[2]="grows"
  SET4$[0]="slowly"  SET4$[1]="quickly"  SET4$[2]=""

  AMB(SET1$[],SET2$[],SET3$[],SET4$[]->TEXT$)
  IF TEXT$<>"" THEN
    PRINT("Correct sentence would be:")
    PRINT(TEXT$)
  ELSE
    PRINT("Failed to fine a correct sentence.")
  END IF
  PRINT
  PRINT("Press any key to exit.")
  REPEAT
    GET(Z$)
  UNTIL LEN(Z$)<>0
END PROGRAM

```



## Factor


```factor
USING: backtrack continuations kernel prettyprint sequences ;
IN: amb

CONSTANT: words {
    { "the" "that" "a" }
    { "frog" "elephant" "thing" }
    { "walked" "treaded" "grows" }
    { "slowly" "quickly"  }
}

: letters-match? ( str1 str2 -- ? ) [ last ] [ first ] bi* = ;

: sentence-match? ( seq -- ? ) dup rest [ letters-match? ] 2all? ;

: select ( seq -- seq' ) [ amb-lazy ] map ;

: search ( -- )
    words select dup sentence-match? [ " " join ] [ fail ] if . ;

MAIN: search
```


Running it from the listener :

 ( scratchpad ) "amb" run
 "that thing grows slowly"

=={{header|F_Sharp|F#}}==
{{trans|Haskell}}
Important differences to the Haskell solution:
* The list monad is not predefined in F#. (But it is easy to define it.)
* F# is not lazy, so this will check all combinations even if we just want one solution.
Both problems could be addressed by using sequence expressions instead.


```fsharp
// define the List "workflow" (monad)
type ListBuilder() =
   member o.Bind( lst, f ) = List.concat( List.map (fun x -> f x) lst )
   member o.Return( x ) = [x]
   member o.Zero() = []

let list = ListBuilder()

let amb = id

// last element of a sequence
let last s = Seq.nth ((Seq.length s) - 1) s

// is the last element of left the same as the first element of right?
let joins left right = last left = Seq.head right

let example = list { let! w1 = amb ["the"; "that"; "a"]
                     let! w2 = amb ["frog"; "elephant"; "thing"]
                     let! w3 = amb ["walked"; "treaded"; "grows"]
                     let! w4 = amb ["slowly"; "quickly"]
                     if joins w1 w2 &&
                        joins w2 w3 &&
                        joins w3 w4
                     then
                        return String.concat " " [w1; w2; w3; w4]
                   }

printfn "%s" (List.head example)
```



## Go

Solution with goroutines. See description on talk page.

```go
package main

import (
    "fmt"
    "sync"
)

func ambStrings(ss []string) chan []string {
    c := make(chan []string)
    go func() {
        for _, s := range ss {
            c <- []string{s}
        }
        close(c)
    }()
    return c
}

func ambChain(ss []string, cIn chan []string) chan []string {
    cOut := make(chan []string)
    go func() {
        var w sync.WaitGroup
        for chain := range cIn {
            w.Add(1)
            go func(chain []string) {
                for s1 := range ambStrings(ss) {
                    if s1[0][len(s1[0])-1] == chain[0][0] {
                        cOut <- append(s1, chain...)
                    }
                }
                w.Done()
            }(chain)
        }
        w.Wait()
        close(cOut)
    }()
    return cOut
}

func main() {
    s1 := []string{"the", "that", "a"}
    s2 := []string{"frog", "elephant", "thing"}
    s3 := []string{"walked", "treaded", "grows"}
    s4 := []string{"slowly", "quickly"}
    c := ambChain(s1, ambChain(s2, ambChain(s3, ambStrings(s4))))
    for s := range c {
        fmt.Println(s)
    }
}
```

{{out}}

```txt

[that thing grows slowly]

```

Alternative solution:

```go
package main
import "fmt"

func amb(wordsets [][]string, res []string) bool {
	if len(wordsets) == 0 {
		return true
	}

	var s string

	l := len(res)
	if l > 0 { s = res[l - 1] }

	res = res[0:len(res) + 1]

	for _, res[l] = range(wordsets[0]) {
		if l > 0 && s[len(s) - 1] != res[l][0] { continue }

		if amb(wordsets[1:len(wordsets)], res) {
			return true
		}
	}

	return false
}

func main() {
	wordset := [][]string { { "the", "that", "a" },
				{ "frog", "elephant", "thing" },
				{ "walked", "treaded", "grows" },
				{ "slowly", "quickly" } }
	res := make([]string, len(wordset))

	if amb(wordset, res[0:0]) {
		fmt.Println(res)
	} else {
		fmt.Println("No amb found")
	}
}
```


=={{Header|Haskell}}==
Haskell's List monad returns all the possible choices. Use the "head" function on the result if you just want one.

```haskell
import Control.Monad

amb = id

joins left right = last left == head right

example = do
  w1 <- amb ["the", "that", "a"]
  w2 <- amb ["frog", "elephant", "thing"]
  w3 <- amb ["walked", "treaded", "grows"]
  w4 <- amb ["slowly", "quickly"]
  unless (joins w1 w2) (amb [])
  unless (joins w2 w3) (amb [])
  unless (joins w3 w4) (amb [])
  return (unwords [w1, w2, w3, w4])
```


Note that "amb" is defined as a no-op and is written only to help show the analogy with other implementations; ordinary style is to write e.g. <code>w1 <- ["the", "that", "a"]</code>.

It may also be illuminating to show how this desugars (dropping the do notation) if we express it directly in terms of the list monad bind function '''(>>=)''' (or '''>>=''' without brackets as in infix operator), which is possibly more familiar (or more directly intelligible) as '''concatMap''' with its arguments flipped.

The function of '''amb''' can then be seen in the return of a list of bound values, if a predicate is matched, or the return of an empty list, if the predicate fails:


```haskell
joins :: String -> String -> Bool
joins left right = last left == head right

-- First desugaring (dropping the do notation)
-- in terms of the bind operator (>>=) for the list monad

exampleBind :: String
exampleBind =
  ["the", "that", "a"] >>=
  (\w1 ->
      ["frog", "elephant", "thing"] >>=
      \w2 ->
         ["walked", "treaded", "grows"] >>=
         \w3 ->
            ["slowly", "quickly"] >>=
            (\w4 ->
                if joins w1 w2
                  then (if joins w2 w3
                          then (if joins w3 w4
                                  then unwords [w1, w2, w3, w4]
                                  else [])
                          else [])
                  else []))
                  
-- Second desugaring (still dropping the do notation)
-- in terms of the concatMap, which is >>= with its arguments flipped

exampleConcatMap :: String
exampleConcatMap =
  concatMap
    (\w1 ->
        concatMap
          (\w2 ->
              concatMap
                (\w3 ->
                    concatMap
                      (\w4 ->
                          if joins w1 w2
                            then (if joins w2 w3
                                    then (if joins w3 w4
                                            then unwords [w1, w2, w3, w4]
                                            else [])
                                    else [])
                            else [])
                      ["slowly", "quickly"])
                ["walked", "treaded", "grows"])
          ["frog", "elephant", "thing"])
    ["the", "that", "a"]

main :: IO ()
main = do
  print exampleBind
  print exampleConcatMap
```


{{Out}}

```txt
"that thing grows slowly"
"that thing grows slowly"
```


Or, immediately pairing each indeterminate value with a predicate (rather concluding with a compound predicate).

```haskell
example :: [String]
example =
  ["the", "that", "a"] >>=
  \w1 ->
     when True ["frog", "elephant", "thing"] >>=
     \w2 ->
        when (joins w1 w2) ["walked", "treaded", "grows"] >>=
        \w3 ->
           when (joins w2 w3) ["slowly", "quickly"] >>=
           \w4 -> when (joins w3 w4) [w1, w2, w3, w4]

joins :: String -> String -> Bool
joins left right = last left == head right

when :: Bool -> [a] -> [a]
when p xs =
  if p
    then xs
    else []

main :: IO ()
main = print $ unwords example
```


```txt
"that thing grows slowly"
```



And a familar '''re'''sugaring of a list monad wrapping of indeterminate values and constraints is, of course, the list comprehension notation, which has a semantics directly equivalent to that of '''amb''' tuples and contraints, and provides quite a clean and natural notation for their expression.


```haskell
joins :: String -> String -> Bool
joins left right = last left == head right

example :: [String]
example =
  [ unwords [w1, w2, w3, w4]
  | w1 <- ["the", "that", "a"] 
  , w2 <- ["frog", "elephant", "thing"] 
  , w3 <- ["walked", "treaded", "grows"] 
  , w4 <- ["slowly", "quickly"] 
  , joins w1 w2 
  , joins w2 w3 
  , joins w3 w4 ]

main :: IO ()
main = print example
```

{{Out}}

```txt
["that thing grows slowly"]
```



## Haxe


```haxe
class RosettaDemo
{
	static var setA = ['the', 'that', 'a'];
	static var setB = ['frog', 'elephant', 'thing'];
	static var setC = ['walked', 'treaded', 'grows'];
	static var setD = ['slowly', 'quickly'];

	static public function main()
	{
		Sys.print(ambParse([ setA, setB, setC, setD ]).toString());
	}

	static function ambParse(sets : Array<Array<String>>)
	{
		var ambData : Dynamic = amb(sets);

		for (data in 0...ambData.length)
		{
			var tmpData = parseIt(ambData[data]);
			var tmpArray = tmpData.split(' ');
			tmpArray.pop();
			if (tmpArray.length == sets.length)
			{
				return tmpData;
			}
		}

		return '';
	}

	static function amb(startingWith : String = '', sets : Array<Array<String>>) : Dynamic
	{
		if (sets.length == 0 || sets[0].length == 0) return;

		var match : Dynamic = [];
		for (reference in sets[0])
		{
			if (startingWith == '' || startingWith == reference.charAt(0))
			{
				var lastChar = reference.charAt(reference.length-1);
				if (Std.is(amb(lastChar, sets.slice(1)), Array))
				{
					match.push([ reference, amb(lastChar, sets.slice(1))]);
				}
				else
				{
					match.push([ reference ]);
				}
			}
		}
		return match;
	}

	static function parseIt(data : Dynamic)
	{
		var retData = '';
		if (Std.is(data, Array))
		{
			for (elements in 0...data.length)
			{
				if (Std.is(data[elements], Array))
				{
					retData = retData + parseIt(data[elements]);
				}
				else
				{
					retData = retData + data[elements] + ' ';
				}
			}
		}
		return retData;
	}
}
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main()
    s1 := ["the","that","a"]
    s2 := ["frog","elephant","thing"]
    s3 := ["walked","treaded","grows"]
    s4 := ["slowly","quickly"]

    write(amb(!s1,!s2,!s3,!s4))
end

procedure amb(exprs[])
    s := ""
    every e := !exprs do {
        if \c ~== e[1] then fail
        c := e[-1]
        s ||:= e || " "
        }
    return s
end
```



## J


```j
   amb=. ([ , ' ' , ])&>/&.>@:((({:@:[ = {.@:])&>/&> # ])@:,@:({@(,&<)))
   >@(amb&.>/) ('the';'that';'a');('frog';'elephant';'thing');('walked';'treaded';'grows');(<'slowly';'quickly')
+-----------------------+
|that thing grows slowly|
+-----------------------+
```

amb is a dyadic verb:

```j
   ('the';'that';'a') amb ('frog';'elephant';'thing') amb ('walked';'treaded';'grows') amb ('slowly';'quickly')
+-----------------------+
|that thing grows slowly|
+-----------------------+
```

A structured derivation of amb follows:

```j
   NB. Dynamic programming method...
   
   o=. @:                NB. Composing verbs
   success=. {:o[ = {.o] NB. Is the last letter of the left word equal to the first of the right?
   join=. [ , ' ' , ]    NB. Joining the left and right words
   cp=. {@(,&<)          NB. Cartesian product
   
   amb=. join&>/&.> o ((success&>/ &> # ]) o , o cp)f.
   amb NB. Showing the point-free code...
([ , ' ' , ])&>/&.>@:((({:@:[ = {.@:])&>/&> # ])@:,@:({@(,&<)))
```


Note that <code>amb</code> here is roughly equivalent to the <code>Ambassert</code> in the task description, and that the corresponding <code>Ambsel</code> is unnecessary and trivial (if needed, we could define <code>Ambsel</code> as the identity operation and make these examples slightly more verbose). However, this implementation should be refactored to extract the example logic from the implementation of <code>amb</code> (or you can do as was apparently suggested here, and use the definition of <code>amb</code> instead of the word - replacing <code>success</code> as needed).


## JavaScript


### Procedural


```javascript
function ambRun(func) {
    var choices = [];
    var index;

    function amb(values) {
        if (values.length == 0) {
            fail();
        }
        if (index == choices.length) {
            choices.push({i: 0,
                          count: values.length});
        }
        var choice = choices[index++];
        return values[choice.i];
    }

    function fail() { throw fail; }

    while (true) {
        try {
            index = 0;
            return func(amb, fail);
        } catch (e) {
            if (e != fail) {
                throw e;
            }
            var choice;
            while ((choice = choices.pop()) && ++choice.i == choice.count) {}
            if (choice == undefined) {
                return undefined;
            }
            choices.push(choice);
        }
    }
}

ambRun(function(amb, fail) {
    function linked(s1, s2) {
        return s1.slice(-1) == s2.slice(0, 1);
    }

    var w1 = amb(["the", "that", "a"]);
    var w2 = amb(["frog", "elephant", "thing"]);
    if (!linked(w1, w2)) fail();

    var w3 = amb(["walked", "treaded", "grows"]);
    if (!linked(w2, w3)) fail();

    var w4 = amb(["slowly", "quickly"]);
    if (!linked(w3, w4)) fail();

    return [w1, w2, w3, w4].join(' ');
});  // "that thing grows slowly"
```



### Functional


Defining amb as the list monad bind/inject operator:


```javascript
(() => {
    'use strict';

    // amb :: [a] -> (a -> [b]) -> [b]
    const amb = xs => f =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // when :: Bool -> [a] -> [a]
    const when = p =>
        xs => p ? (
            xs
        ) : [];


    // TEST -----------------------------------------------
    const main = () => {

        // joins :: String -> String -> Bool
        const joins = (a, b) =>
            b[0] === last(a);

        console.log(
            amb(['the', 'that', 'a'])
            (w1 => when(true)(

                amb(['frog', 'elephant', 'thing'])
                (w2 => when(joins(w1, w2))(

                    amb(['walked', 'treaded', 'grows'])
                    (w3 => when(joins(w2, w3))(

                        amb(['slowly', 'quickly'])
                        (w4 => when(joins(w3, w4))(

                            unwords([w1, w2, w3, w4])

                        ))
                    ))
                ))
            ))
        );
    };

    // GENERIC FUNCTIONS ----------------------------------  

    // last :: [a] -> a
    const last = xs =>
        0 < xs.length ? xs.slice(-1)[0] : undefined;

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
that thing grows slowly
```



## jq

{{works with|jq|1.4}}
Two solutions are given. The first follows the style of the Prolog example.  The second perhaps hews more closely to the intended specification of "amb".

'''Solution using amb/0'''

```jq
def amb: .[];
 
def joins:
  (.[0][-1:]) as $left
  | (.[1][0:1]) as $right
  | if $left == $right then true else empty end;
 
```

'''Example''':

```jq
(["the","that","a"] | amb) as $word1
  | (["frog","elephant","thing"] | amb) as $word2
  | [$word1, $word2] | joins
  | (["walked","treaded","grows"] | amb) as $word3
  | [$word2, $word3] | joins
  | (["slowly","quickly"] | amb) as $word4
  | [$word3, $word4] | joins
  | [$word1, $word2, $word3, $word4]
```

{{out}}

```sh
jq -n -f amb.jq
[
  "that",
  "thing",
  "grows",
  "slowly"
]
```

'''Solution using amb(condition)''':

```jq
def amb(condition): .[] | select(condition);
 
def joins:
  (.[0][-1:]) as $left
  | (.[1][0:1]) as $right
  | $left == $right ;
```

'''Example''':

```jq
(["the","that","a"] | amb(true)) as $word1
  | (["frog","elephant","thing"] | amb( [$word1, .] | joins)) as $word2
  | (["walked","treaded","grows"] | amb( [$word2, .] | joins)) as $word3
  | (["slowly","quickly"] | amb( [$word3, .] | joins)) as $word4
  | [$word1, $word2,$word3, $word4]
```

{{out}}
As above.


## Julia


```Julia
# This is a general purpose AMB function that takes a two-argument failure function and
# arbitrary number of iterable objects and returns the first solution found as an array
# this function is in essence an iterative backtracking solver

function amb(failure, itrs...)
    n = length(itrs)
    if n == 1 return end
    states = Vector(n)
    values = Vector(n)
    # starting point, we put down the first value from the first iterable object
    states[1] = start(itrs[1])
    values[1], states[1] = next(itrs[1], states[1])
    i = 1
    # main solver loop
    while true
        # test for failure
        if i > 1 && failure(values[i-1], values[i])
            # loop for generating a new value upon failure
            # in fact this would be way more readable using goto, but Julia doesn't seem to have that :(
            while true
                # if we failed, we must generate a new value, but first we must check whether there is any
                if done(itrs[i], states[i])
                    # backtracking step with sanity check in case we ran out of values from the current generator
                    if i == 1
                        return
                    else
                        i -= 1
                        continue
                    end
                else
                    # if there is indeed a new value, generate it
                    values[i], states[i] = next(itrs[i], states[i])
                    break
                end
            end
        else
            # no failure branch
            # if solution is ready (i.e. all generators are used) just return it
            if i == n return values end
            # else start up the next generator
            i += 1
            states[i] = start(itrs[i])
            values[i], states[i] = next(itrs[i], states[i])
        end
    end
end

# Call our generic AMB function according to the task description and
# form the solution sentence from the returned array of words
amb((s1,s2) -> s1[end] != s2[1], # failure function
    ["the", "that", "a"],
    ["frog", "elephant", "thing"],
    ["walked", "treaded", "grows"],
    ["slowly", "quickly"]) |>
    x -> join(x, " ") |>
    println

```



## Kotlin


This solves the problem using backtracking whenever amb() is executed. amb will probably have unexpected behavior if any variables are mutated. Using simple assignment for variables shouldn't be a problem.


```scala
// version 1.2.41
import kotlin.coroutines.experimental.*
import kotlin.coroutines.experimental.intrinsics.*

fun main(args: Array<String>) = amb {
    val a = amb("the", "that", "a")
    val b = amb("frog", "elephant", "thing")
    val c = amb("walked", "treaded", "grows")
    val d = amb("slowly", "quickly")
    
    if (a[a.lastIndex] != b[0]) amb()
    if (b[b.lastIndex] != c[0]) amb()
    if (c[c.lastIndex] != d[0]) amb()
    
    println(listOf(a, b, c, d))
    
    
    val x = amb(1, 2, 3)
    val y = amb(7, 6, 4, 5)
    if (x * y != 8) amb()
    println(listOf(x, y))
}


class AmbException(): Exception("Refusing to execute")
data class AmbPair<T>(val cont: Continuation<T>, val valuesLeft: MutableList<T>)

@RestrictsSuspension
class AmbEnvironment {
    val ambList = mutableListOf<AmbPair<*>>()
    
    suspend fun <T> amb(value: T, vararg rest: T): T = suspendCoroutineOrReturn { cont -> 
        if (rest.size > 0) {
            ambList.add(AmbPair(clone(cont), mutableListOf(*rest)))
        }
        
        value
    }
    
    suspend fun amb(): Nothing = suspendCoroutine<Nothing> { }
}

@Suppress("UNCHECKED_CAST")
fun <R> amb(block: suspend AmbEnvironment.() -> R): R {
    var result: R? = null
    var toThrow: Throwable? = null
    
    val dist = AmbEnvironment()
    block.startCoroutine(receiver = dist, completion = object : Continuation<R> {
        override val context: CoroutineContext get() = EmptyCoroutineContext
        override fun resume(value: R) { result = value }
        override fun resumeWithException(exception: Throwable) { toThrow = exception }
    })
    
    while (result == null && toThrow == null && !dist.ambList.isEmpty()) {
        val last = dist.ambList.run { this[lastIndex] }
        
        if (last.valuesLeft.size == 1) {
            dist.ambList.removeAt(dist.ambList.lastIndex)
            last.apply {
                (cont as Continuation<Any?>).resume(valuesLeft[0])
            }
        } else {
            val value = last.valuesLeft.removeAt(last.valuesLeft.lastIndex)
            (clone(last.cont) as Continuation<Any?>).resume(value)
        }
    }
    
    if (toThrow != null)
    {
        throw toThrow!!
    }
    else if (result != null)
    {
        return result!!
    }
    else 
    {
        throw AmbException()
    }
}

val UNSAFE = Class.forName("sun.misc.Unsafe")
    .getDeclaredField("theUnsafe")
    .apply { isAccessible = true }
    .get(null) as sun.misc.Unsafe

@Suppress("UNCHECKED_CAST")
fun <T: Any> clone(obj: T): T {
    val clazz = obj::class.java
    val copy = UNSAFE.allocateInstance(clazz) as T
    copyDeclaredFields(obj, copy, clazz)
    return copy
}

tailrec fun <T> copyDeclaredFields(obj: T, copy: T, clazz: Class<out T>) {
    for (field in clazz.declaredFields) {
        field.isAccessible = true
        val v = field.get(obj)
        field.set(copy, if (v === obj) copy else v)
    }
    val superclass = clazz.superclass
    if (superclass != null) copyDeclaredFields(obj, copy, superclass)
}
```


{{out}}

```txt

[that, thing, grows, slowly]
[2, 4]

```



## Lua


```lua
function amb (set)
    local workset = {}
    if (#set == 0) or (type(set) ~= 'table') then return end
    if #set == 1 then return set end
    if #set > 2 then
        local first = table.remove(set,1)
        set = amb(set)
        for i,v in next,first do
            for j,u in next,set do
                if v:byte(#v) == u[1]:byte(1) then table.insert(workset, {v,unpack(u)}) end
            end
        end
        return workset
    end
    for i,v in next,set[1] do
        for j,u in next,set[2] do
            if v:byte(#v) == u:byte(1) then table.insert(workset,{v,u}) end
        end
    end
    return workset
end
```

Usage example:

```lua
result = amb({{'the','that','a'},{'frog','elephant','thing'},{'walked','treaded','grows'},{'slowly','quickly'}})
for i,v in next,result do
    io.write (i,':\t')
    for j,u in next,v do
        io.write (u,' ')
    end
    io.write ('\n')
end
```


=={{Header|Mathematica}} / {{header|Wolfram Language}}==
Make all the tuples of all the lists, then filter out the good ones:

```Mathematica
 CheckValid[i_List]:=If[Length[i]<=1,True,And@@(StringTake[#[[1]],-1]==StringTake[#[[2]],1]&/@Partition[i,2,1])]
 sets={{"the","that","a"},{"frog","elephant","thing"},{"walked","treaded","grows"},{"slowly","quickly"}};
 Select[Tuples[sets],CheckValid]
```

gives back:

```Mathematica
{{"that", "thing", "grows", "slowly"}}
```

Note that it will return multiple values if multiple sentences match the requirement, that is why the returned value is a list of list (1 element, 4 elements).

Alternative algorithm (slightly faster on most data sets):

```Mathematica
CheckValid2[i_List] := StringFreeQ[StringJoin[Riffle[i, ","]], a_ ~~ "," ~~ b_ /; a =!= b]
```


=={{Header|Mercury}}==
Like Prolog, Mercury has built-in nondeterminacy; however, Mercury is explicit about it, and statically checks it.


```Mercury
:- module amb.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.
:- implementation.
:- import_module list, string, char, int.

main(!IO) :-
        ( solution(S) -> io.write_string(S, !IO), io.nl(!IO)
        ; io.write_string("No solutions found :-(\n", !IO) ).

:- pred solution(string::out) is nondet.
solution(S) :-
        member(A, ["the", "that", "a"]),
        member(N, ["frog", "elephant", "thing"]),
        member(V, ["walked", "treaded", "grows"]),
        member(E, ["slowly", "quickly"]),
        S = join_list(" ", [A, N, V, E]),
        rule1(A, N), rule1(N, V), rule1(V, E).

:- pred rule1(string::in, string::in) is semidet.
rule1(A, B) :- last_char(A) = C, first_char(B, C, _).

:- func last_char(string::in) = (char::out) is semidet.
last_char(S) = C :- index(S, length(S) - 1, C).
```


The Amb defined in the Prolog solution is similar to the use of list.member/2 above.  Predicates could be used instead:


```Mercury

:- pred noun(string).
:- mode noun(out) is multi.    % provide any one noun.
:- mode noun(in) is semidet.   % fail if given string isn't a known noun.
noun("frog").
noun("elephant").
noun("thing").
```


=={{Header|NetRexx}}==

```netrexx
 /* REXX **************************************************************
 * 25.08.2013 Walter Pachl derived from REXX version 2
 *********************************************************************/
 w=''
 l=0
 mm=0
 mkset(1,'the that a if',w,mm,l)
 mkset(2,'frog elephant thing',w,mm,l)
 mkset(3,'walked treaded grows trots',w,mm,l)
 mkset(4,'slowly quickly',w,mm,l)
 show(w,mm,l)

 Loop i=1 to 3                         /* loop over sets             */
   k=i+1                               /* the following set          */
   Loop ii=1 To 10                     /* loop over elements in set k*/
     If w[i,ii].words=i Then Do        /* a sentence part found      */
       Loop jj=1 To 10                 /* loop over following words  */
         If w[i,ii].right(1)=w[k,jj].left(1) Then Do  /* fitting     */
           ns=w[i,ii]' 'w[k,jj]        /* build new sentence (part)  */
           If ns.words=k Then          /* 'complete' part            */
             add(w,k,ns)               /* add to set k               */
           End
         End
       End
     End
   End
 Say 'Results:'
 Loop jj=1 To 10                       /* show the results           */
   If w[4,jj].words=4 Then
     Say '-->' w[4,jj]
   End

 method add(w,k,s) public static
 /*********************************************************************
 * add a fitting sentence (part) s to set w[k,*]
 *********************************************************************/
   Loop i=1 To 10 While w[k,i]>''      /* look for an empty slot     */
     End
   w[k,i]=s                            /* add the sentence (part)    */
   Return

 method mkset(n,arg,smp,mm,l) public static
 /*********************************************************************
 * create set smp[n,*] from data in arg
 * mm[0] maximum number of elements in any set
 * l[n] maximum word length in set n
 *********************************************************************/
  loop i = 1 to arg.words
    smp[n,i] = arg.word(i)
    If smp[n,i].length>l[n] Then
      l[n]=smp[n,i].length
    end
  if i-1>mm[0] Then Do
    mm[0]=i-1
    End
  return

 method show(w,mm,l) public static
 /*********************************************************************
 * show the input
 *********************************************************************/
   Say 'Input:'
   Loop j=1 To mm[0]                   /* output lines               */
     ol=''
     Loop i=1 To 4
       ol=ol w[i,j].left(l[i])
       End
     Say ol.strip
     End;
   say ''
   Return
```

{{out}}

```txt

Input:
the    frog     walked  slowly
that   elephant treaded quickly
a      thing    grows
if              trots

Results:
--> the elephant trots slowly
--> that thing grows slowly
--> if frog grows slowly

```

Note:  the output of the input is truncated (columns three and four), 
but the results are correct for the data specified, 
but not for the input as specified for this task 
(ditto for the PL/I example and the REXX version 2 example).

length corrected. thanks. extraneous input: intentional and harmless !?!

=={{Header|Nim}}==
{{trans|D}}

```nim
import future, strutils

proc amb(comp: proc(a, b: string): bool, options: seq[seq[string]],
         prev: string = nil): seq[string] =
  if options.len == 0: return @[]

  for opt in options[0]:
    # If this is the base call, prev is nil and we need to continue.
    if prev != nil and not comp(prev, opt): continue

    # Take care of the case where we have no options left.
    if options.len == 1: return @[opt]

    # Traverse into the tree.
    let res = amb(comp, options[1..options.high], opt)

    # If it was a failure, try the next one.
    if res.len > 0: return opt & res # We have a match

  return @[]

const sets = @[@["the", "that", "a"],
               @["frog", "elephant", "thing"],
               @["walked", "treaded", "grows"],
               @["slowly", "quickly"]]

let result = amb((s, t: string) => (s[s.high] == t[0]), sets)
if result.len == 0:
  echo "No matches found!"
else:
  echo result.join " "
```

{{out}}

```txt
that thing grows slowly
```


=={{Header|OCaml}}==
There is no Amb operator in OCaml. So below are two solutions to solve the same task. 
The first one is the more idiomatic for OCaml (and is similar to the Haskell solution), it builds all possible combinations and then take the good result in it.

The second solution tries to be closer to the way of solving the problem of Amb. 
It does not build and accumulate the combinations, it iterates over these with a higher order function and it stops when it finds a solution that matches the predicate.


###  Filtering possible combinations 


```ocaml
let set_1 = ["the"; "that"; "a"]
let set_2 = ["frog"; "elephant"; "thing"]
let set_3 = ["walked"; "treaded"; "grows"]
let set_4 = ["slowly"; "quickly"]

let combs ll =
  let rec aux acc = function
  | [] -> (List.map List.rev acc)
  | hd::tl ->
      let acc =
        List.fold_left
          (fun _ac l ->
            List.fold_left (fun _ac v -> (v::l)::_ac) _ac hd
          ) [] acc
      in
      aux acc tl
  in
  aux [[]] ll

let last s = s.[pred(String.length s)]
let joined a b = (last a = b.[0])

let rec test = function
  | a::b::tl -> (joined a b) && (test (b::tl))
  | _ -> true

let print_set set =
  List.iter (Printf.printf " %s") set;
  print_newline();
;;

let () =
  let sets = combs [set_1; set_2; set_3; set_4] in
  let sets = List.filter test sets in
  List.iter print_set sets;
;;
```


We can take all the good results with List.filter or just take the first one with List.find.


###  Higher order function 

Here the function comb_search replaces the function combs and uses arrays instead of lists. This function takes successively all the possible results by their indicies (with the array nx). When a result satisfies the predicate p, it is returned


```ocaml
let set_1 = [| "the"; "that"; "a" |]
let set_2 = [| "frog"; "elephant"; "thing" |]
let set_3 = [| "walked"; "treaded"; "grows" |]
let set_4 = [| "slowly"; "quickly" |]

let comb_search p aa =
  let nx = Array.make (Array.length aa) 0 in
  let lx = Array.map Array.length aa in
  let la = Array.length aa in
  let rec loop() =
    let res = Array.mapi (fun i j -> aa.(i).(j)) nx in
    if p res then (res)
    else    
    ( nx.(0) <- nx.(0) + 1;
      if nx.(0) < lx.(0)
      then loop()
      else
      ( nx.(0) <- 0;
        let rec roll n =
          if n >= la then raise Not_found
          else 
          ( nx.(n) <- nx.(n) + 1;
            if nx.(n) >= lx.(n)
            then ( nx.(n) <- 0; roll (n+1) )
            else loop()
          )
        in
        roll 1
      )
    )
  in
  loop()
  
let last s = s.[pred(String.length s)]
let joined a b = (last a = b.[0])

let rec test = function
  | a::b::tl -> (joined a b) && (test (b::tl))
  | _ -> true

let test r = test(Array.to_list r)

let print_set set =
  Array.iter (Printf.printf " %s") set;
  print_newline();
;;

let () =
  let result = comb_search test [| set_1; set_2; set_3; set_4 |] in
  print_set result;
;;
```


=={{Header|OpenEdge/Progress}}==

```OpenEdge/Progress
DEF VAR cset AS CHAR EXTENT 4 INIT [   
   "the,that,a",
   "frog,elephant,thing", 
   "walked,treaded,grows",
   "slowly,quickly"
].

FUNCTION getAmb RETURNS CHARACTER ( 
   i_cwords AS CHAR,
   i_iset   AS INT
):

   DEF VAR cresult   AS CHAR.
   DEF VAR ii        AS INT.
   DEF VAR cword     AS CHAR.

   DO ii = 1 TO NUM-ENTRIES( cset [ i_iset ] ) WHILE NUM-ENTRIES( cresult, " " ) < EXTENT( cset ):

      cword = ENTRY( ii, cset[ i_iset ] ).
      IF i_cwords = "" OR 
         SUBSTRING( i_cwords, LENGTH( i_cwords ), 1 ) = SUBSTRING( cword, 1, 1 )
      THEN DO:
         IF i_iset = EXTENT ( cset ) THEN
            cresult = i_cwords + " " + cword.
         ELSE
            cresult = getAmb( i_cwords + " " + cword, i_iset + 1 ).
      END.

   END.

   RETURN cresult.

END FUNCTION. /* getAmb */


MESSAGE getAmb( "", 1 ) VIEW-AS ALERT-BOX.
```


{{out}}

```txt

---------------------------
Message
---------------------------
 that thing grows slowly
---------------------------
OK   
---------------------------

```


=={{Header|Oz}}==
Oz is, among other things, a logic programming language and has a choice operator. 
Using recursion we can easily build an Amb operator with it.

```oz
declare

  fun {Amb Xs}
     case Xs of nil then fail
     [] [X] then X
     [] X|Xr then
        choice X
        [] {Amb Xr}
        end
     end
  end

  fun {Example}
     W1 = {Amb ["the" "that" "a"]}
     W2 = {Amb ["frog" "elephant" "thing"]}
     W3 = {Amb ["walked" "treaded" "grows"]}
     W4 = {Amb ["slowly" "quickly"]}
  in
     {List.last W1 W2.1}
     {List.last W2 W3.1}
     {List.last W3 W4.1}
     W1#" "#W2#" "#W3#" "#W4
  end

in

  {ForAll {SearchAll Example} System.showInfo}
```


In Oz, the programmer explicitly controls how a logic program is executed (search strategy, number of required solutions, laziness, which physical machines are used for the search process...). In this case we use the predefined function SearchAll to eagerly calculate all possible solution. All work is done within the current process.

=={{Header|PARI/GP}}==

```parigp
Amb(V)={
	amb(vector(#V,i,vector(#V[i],j,Vec(V[i][j]))),[])
};
amb(V,s)={
	if (#V == 0, return(concat(s)));
	my(v=V[1],U=vecextract(V,2^#V-2),t,final=if(#s,s[#s]));
	if(#s, s = concat(s,[" "]));
	for(i=1,#v,
		if ((#s == 0 | final == v[i][1]),
			t = amb(U, concat(s, v[i]));
			if (t, return(t))
		)
	);
	0
};
Amb([["the","that","a"],["frog","elephant","thing"],["walked","treaded","grows"],["slowly","quickly"]])
```


=={{Header|Perl}}==


### Using fork


This first Perl implementation of the <code>amb</code> operator provides an interface which
satisfies the terms of the task precisely. It shouldn't be used in real code
though, unless you know for a fact that the computer you are using it on has a
very lightweight fork() system call.

It is provided here simply to demonstrate that it <i>can</i> be done.


```perl
use strict;
use warnings;

use constant EXIT_FAILURE => 1;
use constant EXIT_SUCCESS => 0;

sub amb {
   exit(EXIT_FAILURE) if !@_;
   for my $word (@_) {
      my $pid = fork;
      die $! unless defined $pid;
      return $word if !$pid;
      my $wpid = waitpid $pid, 0;
      die $! unless $wpid == $pid;
      exit(EXIT_SUCCESS) if $? == EXIT_SUCCESS;
   }
   exit(EXIT_FAILURE);
}

sub joined {
   my ($join_a, $join_b) = @_;
   substr($join_a, -1) eq substr($join_b, 0, 1);
}

my $w1 = amb(qw(the that a));
my $w2 = amb(qw(frog elephant thing));
my $w3 = amb(qw(walked treaded grows));
my $w4 = amb(qw(slowly quickly));

amb() unless joined $w1, $w2;
amb() unless joined $w2, $w3;
amb() unless joined $w3, $w4;

print "$w1 $w2 $w3 $w4\n";
exit(EXIT_SUCCESS);
```



### Using the regex engine


This version also stays relatively true to the spirit of the task description. The amb routine in this case generates regex alternations, which are then dynamically interpolated into a regex and iterated/backtracked over by the regex engine. Please note that this approach only works well for <i>simple</i> search problems; for more demanding ones it scales quite badly in both speed and memory usage.


```perl
#!/usr/bin/perl

use strict;
use warnings;
use feature 'say';
use re 'eval';

sub amb ($@) {
    my $var = shift;
    join ' || ', map { "(?{ $var = '$_' })" } @_;
}

sub joins {
    substr(shift,-1,1) eq substr(shift,0,1)
}

my ($a,$b,$c,$d);
'' =~ m/
    (??{  amb '$a', qw[the that a]           })
    (??{  amb '$b', qw[frog elephant thing]  })
    (??{  amb '$c', qw[walked treaded grows] })
    (??{  amb '$d', qw[slowly quickly]       })
    (?(?{ joins($b, $c)                      })|(*FAIL))
    (?(?{ joins($a, $b)                      })|(*FAIL))
    (?(?{ joins($c, $d)                      })|(*FAIL))
    (?{   say "$a $b $c $d"                  })
/x;
```


===Using a higher-order function===

In practice, one wouldn't try to squeeze such a search problem into the
<code>amb</code> interface shown in the task description, when coding in Perl.
The main purpose of the <code>amb</code> operator is backtracking, and a more
conventional Perl idiom for that purpose is for the user to pass a subroutine
of their own into a function which acts as a backtracking engine.

The following code does just that: the first arguments for amb(...) are one or
more arrays of values, followed by a user-defined subroutine.  The amb(...)
function arbitrarily selects one value from each of the arrays, and calls the
user's supplied sub with the selected values.

If the user's supplied sub calls amb() with no arguments, the outer amb(...) will
pick the next set of values.  If the user's supplied sub returns normally, then
the return value from the sub will be the return value of amb(...).

This version uses vastly less memory, and is quite reusable.


```perl
use strict;
use warnings;

sub amb {
   if( @_ == 0 ) {
      no warnings 'exiting';
      next AMB;
   }
   my $code = pop;
   my @words = @_;
   my @index = (0) x @words;
   AMB: while( 1 ) {
      my @w = map $words[$_][$index[$_]], 0 .. $#_;
      return $code->( @w );
   } continue {
      my $i = 0;
      while( ++$index[$i] == @{$words[$i]} ) {
         $index[$i] = 0;
         return if ++$i == @index;
      }
   }
}

my @w1 = qw(the that a);
my @w2 = qw(frog elephant thing);
my @w3 = qw(walked treaded grows);
my @w4 = qw(slowly quickly);

sub joined {
   my ($join_a, $join_b) = @_;
   substr($join_a, -1) eq substr($join_b, 0, 1);
}

amb( \(@w1, @w2, @w3, @w4), sub {
   my ($w1, $w2, $w3, $w4) = @_;
   amb() unless joined($w1, $w2);
   amb() unless joined($w2, $w3);
   amb() unless joined($w3, $w4);
   print "$w1 $w2 $w3 $w4\n";
});
```


All three versions produce the same output.
{{out}}

```txt
that thing grows slowly
```



## Perl 6



### Using Junctions


Junctions are a construct that behave similarly to the wanted Amb operator. The only difference is, that they don't preserve the state that was True inside any control structure (like an if).

There is currently a trick, how you only get the "true" values from a Junction for any test: return from a subroutine. Because of DeMorgans Law, you'll have to switch and and or, since you want to return on falseness. Just look at 'all' in combination with the sub(){return unless test} as the amb operator. 


```perl6

#| an array of four words, that have more possible values. 
#| Normally we would want `any' to signify we want any of the values, but well negate later and thus we need `all'
my @a =
(all «the that a»),
(all «frog elephant thing»),
(all «walked treaded grows»),
(all «slowly quickly»);

sub test (Str $l, Str $r) {
    $l.ends-with($r.substr(0,1))
}

(sub ($w1, $w2, $w3, $w4){
  # return if the values are false
  return unless [and] test($w1, $w2), test($w2, $w3),test($w3, $w4);
  # say the results. If there is one more Container layer around them this doesn't work, this is why we need the arguments here.
  say "$w1 $w2 $w3 $w4"
})(|@a); # supply the array as argumetns


```



### Using lazy lists


{{improve|Perl 6|This doesn't really solve the task description in a meaningful way; it only works because it cheats with the way the problem statement is encoded. See the [https://gist.github.com/smls/1484e13b89490e218ddc Gist write-up here].}}

{{works with|niecza|2012-02-29}}
{{broken|Perl 6}}

```perl6>sub infix:<lf
 ($a,$b) {
    next unless try $a.substr(*-1,1) eq $b.substr(0,1);
    "$a $b";
}

multi dethunk(Callable $x) { try take $x() }
multi dethunk(     Any $x) {     take $x   }

sub amb (*@c) { gather @c».&dethunk }

say first *, do
    amb(<the that a>, { die 'oops'}) Xlf
    amb('frog',{'elephant'},'thing') Xlf
    amb(<walked treaded grows>)      Xlf
    amb { die 'poison dart' },
        {'slowly'},
        {'quickly'},
        { die 'fire' };
```

{{out}}

```txt
that thing grows slowly
```

This uses lazy lists, created by the <tt>X</tt> metaoperator applied to a user-defined function, <tt>lf</tt>, that asserts the last-first condition,
and short-circuits the match so that it does not need to generate parts of the search tree that cannot match. We use the <tt>first</tt> function to pull one element from the lazy list; a subscript of <tt>[0]</tt> would have worked just as well.

The <tt>amb</tt> operator itself uses a hyper to run the <tt>dethunk</tt> calls in parallel.  Results are returned asyncronously via <tt>gather</tt>/<tt>take</tt>.  The <tt>dethunk</tt> call traps failures after the failure has bypassed the <tt>take</tt>.


### Using the regex engine


If you consider lazy lists to be cheating on the idea of continuations, here's
some admittedly grungy code that uses the continuation engine of regexes to solve it.  At some point we'll wrap this up in nice syntax to let people write in a sublanguage of Perl 6 that looks more like a logic language.
{{broken|Perl 6}}
''Note: the compiler suggests adding <code>use MONKEY-SEE-NO-EVAL;</code> to enable regex interpolation, but that's not the only issue. The program outputs nothing.''

```perl6
sub amb($var,*@a) {
    "[{
        @a.pick(*).map: {"||\{ $var = '$_' }"}
     }]";
}

sub joins ($word1, $word2) {
    substr($word1,*-1,1) eq substr($word2,0,1)
}

'' ~~ m/
    :my ($a,$b,$c,$d);
    <{ amb '$a', <the that a> }>
    <{ amb '$b', <frog elephant thing> }>
    <?{ joins $a, $b }>
    <{ amb '$c', <walked treaded grows> }>
    <?{ joins $b, $c }>
    <{ amb '$d', <slowly quickly> }>
    <?{ joins $c, $d }>
    { say "$a $b $c $d" }
    <!>
/;
```



## Phix

Fairly simple recursive solution

```Phix
function amb1(sequence sets, object res=0, integer idx=1)
integer ch = 0
integer pass = 0
    if idx>length(sets) then
        pass = 1
    else
        if res=0 then
            res = repeat(0,length(sets))
        else
            ch = sets[idx-1][res[idx-1]][$]
        end if
        for k=1 to length(sets[idx]) do
            if ch=0 or sets[idx][k][1]=ch then
                res[idx] = k
                {pass,res} = amb1(sets,res,idx+1)
                if pass then exit end if
            end if
        end for
    end if
    return {pass,res}
end function

sequence sets = {{"the","that","a"},
                 {"frog","elephant","thing"},
                 {"walked","treaded","grows"},
                 {"slowly","quickly"}}
integer pass
sequence res
    {pass,res} = amb1(sets)
    if pass then
        puts(1,"success: ")
        for i=1 to length(sets) do
            res[i] = sets[i][res[i]]
        end for
        ?res
    else
        puts(1,"failure\n")
    end if
```

{{out}}

```txt

success: {"that","thing","grows","slowly"}

```

To make things a bit more interesting/flexible, I factored out the inner test to a routine
passed as an argument, and likewise added an optional result routine for multiple results.
And to prove it the following solves three rather different problems instead of just one.

```Phix
function amb(sequence sets, integer testrid, integer resrid=-1, object res=0, integer idx=1)
integer flag = (res==0)
integer pass = 0
    if idx>length(sets) then
        pass = 1
        if resrid!=-1 then
            call_proc(resrid,{sets,res})
        end if
    else
        if flag then
            res = repeat(0,length(sets))
        end if
        for k=1 to length(sets[idx]) do
            res[idx] = k
            if flag or call_func(testrid,{sets,res,idx}) then
                {pass,res} = amb(sets,testrid,resrid,res,idx+1)
                if pass and resrid=-1 then exit end if
            end if
        end for
    end if
    return {pass,res}
end function

function pairable(sequence sets, sequence res, integer idx)
    return sets[idx-1][res[idx-1]][$] = sets[idx][res[idx]][1]
end function
constant r_pairable = routine_id("pairable")

procedure AMB_Show(sequence sets, sequence res)
    puts(1,"success: ")
    for i=1 to length(sets) do
        res[i] = sets[i][res[i]]
    end for
    ?res
end procedure
constant r_show = routine_id("AMB_Show")

function pythagorean(sequence sets, sequence res, integer idx)
-- (note that res[idx]==sets[idx][res[idx]] in all cases)
integer x, y, z
    if sequence(sets) then end if -- (suppress warning)
    {x,y,z} = res
    return idx<3 or (x*x+y*y=z*z)
end function
constant r_pythagorean = routine_id("pythagorean")

procedure pythag_show(sequence sets, sequence res)
    if sequence(sets) then end if -- (suppress warning)
    puts(1,"success: ")
    ?res
end procedure
constant r_pythag_show = routine_id("pythag_show")

-- see http://www.randomhacks.net/articles/2005/10/11/amb-operator
function not8(sequence sets, sequence res, integer idx)
-- (note that idx==2 in all cases)
-- (at the last moment, I flipped the test, after realising that
--  someone had completely misunderstood the original article...
--  ...and proved it by showing some strange output on rosetta.)
--  return sets[1][res[1]]*sets[idx][res[idx]]!=8
    return sets[1][res[1]]*sets[idx][res[idx]]=8
end function
constant r_not8 = routine_id("not8")

procedure not8_show(sequence sets, sequence res)
    puts(1,"success: ")
    ?{sets[1][res[1]],sets[2][res[2]]}
end procedure
constant r_not8_show = routine_id("not8_show")

sequence sets = {{"the","that","a"},
                 {"frog","elephant","thing"},
                 {"walked","treaded","grows"},
                 {"slowly","quickly"}}
sequence sets2 = repeat(tagset(11),3)
sequence sets3 = {{1, 2, 3}, {4, 5, 6}}
    puts(1,"\nThe original:\n")
    {} = amb(sets,r_pairable,r_show)
    puts(1,"\nSmall Pythagorean triples problem:\n")
    {} = amb(sets2,r_pythagorean,r_pythag_show)
    puts(1,"\nSome strange not 8 problem:\n") -- (now fixed)
    {} = amb(sets3,r_not8,r_not8_show)
```

{{out}}

```txt

The original:
success: {"that","thing","grows","slowly"}

Small Pythagorean triples problem:
success: {3,4,5}
success: {4,3,5}
success: {6,8,10}
success: {8,6,10}

Some strange not 8 problem:
success: {2,4}

```



## PicoLisp

For backtracking, Pilog (PicoLisp Prolog) is the natural choice.
{{trans|Prolog}}

```PicoLisp
(be amb (@E @Lst)
   (lst @E @Lst) )

(be joins (@Left @Right)
   (^ @T (last (chop (-> @Left))))
   (^ @R (car (chop (-> @Right))))
   (or
      ((equal @T @R))
      ((amb @ NIL)) ) )  # Explicitly using amb fail as required

(be ambExample ((@Word1 @Word2 @Word3 @Word4))
  (amb @Word1 ("the" "that" "a"))
  (amb @Word2 ("frog" "elephant" "thing"))
  (amb @Word3 ("walked" "treaded" "grows"))
  (amb @Word4 ("slowly" "quickly"))
  (joins @Word1 @Word2)
  (joins @Word2 @Word3)
  (joins @Word3 @Word4) )
```

{{out}}

```txt
: (? (ambExample @Result))
 @Result=("that" "thing" "grows" "slowly")
-> NIL
```



## PL/I


```pli
*process or(!) source attributes xref;
 amb: Proc Options(main);
 /*********************************************************************
 * 25.08.2013 Walter Pachl
 *********************************************************************/
 Dcl w(4,10) Char(40) Var
     Init('the','that','a','if',(6)(1)' ',
          'frog','elephant','thing',(7)(1)' ',
          'walked','treaded','grows','trots',(6)(1)' ',
          'slowly','quickly',(8)(1)' ');
 Dcl ns Char(40) Var;
 Dcl (i,k,j,ii,jj,m,n) Bin Fixed(31);
 n=hbound(w,1);                        /* number of sets             */
 m=hbound(w,2);                        /* max number of words in set */
 Call show;                            /* show the input             */
 Do i=1 To n-1;                        /* loop over sets             */
   k=i+1;                              /* the following set          */
   Do ii=1 To m;                       /* loop over elements in set k*/
     If words(w(i,ii))=i Then Do;      /* a sentence part found      */
       Do jj=1 To m;                   /* loop over following words  */
         If right(w(i,ii),1)=left(w(k,jj),1) Then Do; /* fitting     */
           ns=w(i,ii)!!' '!!w(k,jj);   /* build new sentence (part)  */
           If words(ns)=k Then         /* 'complete' part            */
             Call add(k,ns);           /* add to set k               */
         End;
       End;
     End;
   End;
 Do jj=1 To m;                         /* show the results           */
   If words(w(4,jj))=4 Then
     put edit('--> ',w(4,jj))(Skip,a,a);
   End;

 add: Proc(ni,s);
 /*********************************************************************
 * add a sentence (part) to set ni
 *********************************************************************/
 Dcl (i,ni) Bin Fixed(31);
 Dcl s  Char(40) Var;
 Do i=1 To m While(w(ni,i)>'');        /* look for an empty slot     */
   End;
 w(ni,i)=s;                            /* add the sentence (part)    */
 End;

 words: Proc(s) Returns(Bin Fixed(31));
 /*********************************************************************
 * return the number of blank separated words in s
 *********************************************************************/
 Dcl s  Char(40) Var;
 Dcl nw Bin Fixed(31) Init(0);
 Dcl i  Bin Fixed(31) Init(1);
 If s>'' Then Do;
   nw=1;
   Do i=1 To length(s);
     If substr(s,i,1)=' ' Then
       nw+=1;
     End;
   End;
 Return(nw);
 End;

 show: Proc;
 /*********************************************************************
 * show the input sets
 *********************************************************************/
 Dcl (i,j,mm) Bin Fixed(31) Init(0);
 Dcl l(4) Bin Fixed(31) Init((4)0);
 Do i=1 To n;
   Do j=1 To m;
     If w(i,j)>'' Then Do;
       mm=max(mm,j);               /* max number of words in any set */
       l(i)=max(l(i),length(w(i,j)));  /* max word length in set i   */
       End;
     End;
   End;
 Put Edit('Input:')(Skip,a);
 Do j=1 To mm;                         /* output lines               */
   Put Skip;
   Do i=1 To n;
     Put Edit(w(i,j),' ')(a(l(i)),a);
     End;
   End;
 Put Skip;
 End;

 End;
```

{{out}}

```txt

Input: (extended by 2 words!)
the  frog     walked  slowly
that elephant treaded quickly
a    thing    grows
if            trots

--> the elephant trots slowly
--> that thing grows slowly
--> if frog grows slowly     

```


=={{Header|Prolog}}==

```prolog
amb(E, [E|_]).
amb(E, [_|ES]) :- amb(E, ES).

joins(Left, Right) :-
  append(_, [T], Left),
  append([R], _, Right),
  ( T \= R -> amb(_, [])  % (explicitly using amb fail as required)
  ; true ).

amb_example([Word1, Word2, Word3, Word4]) :-
  amb(Word1, ["the","that","a"]),
  amb(Word2, ["frog","elephant","thing"]),
  amb(Word3, ["walked","treaded","grows"]),
  amb(Word4, ["slowly","quickly"]),
  joins(Word1, Word2),
  joins(Word2, Word3),
  joins(Word3, Word4).
```



## PureBasic


```PureBasic
Procedure Words_Ok(String1.s, String2.s)
  If Mid(String1,Len(String1),1)=Mid(String2,1,1)
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

Procedure.s Amb(Array A.s(1), Array B.s(1), Array C.s(1), Array D.s(1))
  Protected a, b, c, d
  For a=0 To ArraySize(A())
    For b=0 To ArraySize(B())
      For c=0 To ArraySize(C())
        For d=0 To ArraySize(D())
          If Words_Ok(A(a),B(b)) And Words_Ok(B(b),C(c)) And Words_Ok(C(c),D(d))
            ProcedureReturn A(a)+" "+B(b)+" "+C(c)+" "+D(d)
          EndIf
        Next
      Next
    Next
  Next
  ProcedureReturn ""   ; Empty string, e.g. fail
EndProcedure

If OpenConsole()
  Define Text.s
  Dim Set1.s(2)
  Dim Set2.s(2)
  Dim Set3.s(2)
  Dim Set4.s(1)
  
  Set1(0)="the":    set1(1)="that":     set1(2)="a"
  Set2(0)="frog":   set2(1)="elephant": set2(2)="thing" 
  Set3(0)="walked": set3(1)="treaded":  set3(2)="grows" 
  Set4(0)="slowly": set4(1)="quickly"
  
  text=Amb(set1(),set2(),Set3(),set4())
  If Text<>""
    PrintN("Correct sentence would be,"+#CRLF$+Text)
  Else
    PrintN("Failed to fine a correct sentence.")
  EndIf
  PrintN(#CRLF$+#CRLF$+"Press ENTER to exit."): Input()
  CloseConsole()
EndIf
```


=={{Header|Python}}==

### Procedural

<small>(Note: The code is also imported and used as a module in the solution to [[Dinesman's_multiple-dwelling_problem#By_using_the_Amb_operator|this task]]).</small>

Python does not have the amb function, but the declarative style of programming and the use of the one "function" to do all three tasks of:
* Setting ranges
* Setting the constraint
* Iterating over all solutions
can be done in what appears to be a [[wp:Declarative programming|declarative]] manner with the following class Amb:

```python
import itertools as _itertools
 
class Amb(object):
    def __init__(self):
        self._names2values   = {}       # set of values for each global name
        self._func           = None     # Boolean constraint function
        self._valueiterator  = None     # itertools.product of names values
        self._funcargnames   = None     # Constraint parameter names
 
    def __call__(self, arg=None):
        if hasattr(arg, '__code__'):                
            ##
            ## Called with a constraint function. 
            ##
            globls = arg.__globals__ if hasattr(arg, '__globals__') else arg.func_globals
            # Names used in constraint
            argv = arg.__code__.co_varnames[:arg.__code__.co_argcount]
            for name in argv:
                if name not in self._names2values:
                    assert name in globls, \
                           "Global name %s not found in function globals" % name
                    self._names2values[name] = globls[name]
            # Gather the range of values of all names used in the constraint
            valuesets = [self._names2values[name] for name in argv]
            self._valueiterator = _itertools.product(*valuesets)
            self._func = arg
            self._funcargnames = argv
            return self
        elif arg is not None:
            ##
            ## Assume called with an iterable set of values
            ##
            arg = frozenset(arg)
            return arg
        else:
            ##
            ## blank call tries to return next solution
            ##
            return self._nextinsearch()
 
    def _nextinsearch(self):
        arg = self._func
        globls = arg.__globals__
        argv = self._funcargnames
        found = False
        for values in self._valueiterator:
            if arg(*values):
                # Set globals.
                found = True
                for n, v in zip(argv, values):
                    globls[n] = v
                break
        if not found: raise StopIteration
        return values
 
    def __iter__(self):
        return self
 
    def __next__(self):
        return self()
    next = __next__ # Python 2
 
if __name__ == '__main__':
    if True:
        amb = Amb()
 
        print("\nSmall Pythagorean triples problem:")
        x = amb(range(1,11))
        y = amb(range(1,11))
        z = amb(range(1,11))
 
        for _dummy in amb( lambda x, y, z: x*x + y*y == z*z ):
            print ('%s %s %s' % (x, y, z))
 
 
    if True:
        amb = Amb()
 
        print("\nRosetta Code Amb problem:")
        w1 = amb(["the", "that", "a"])
        w2 = amb(["frog", "elephant", "thing"])
        w3 = amb(["walked", "treaded", "grows"])
        w4 = amb(["slowly", "quickly"])
 
        for _dummy in amb( lambda w1, w2, w3, w4: \
                             w1[-1] == w2[0] and \
                             w2[-1] == w3[0] and \
                             w3[-1] == w4[0] ):
            print ('%s %s %s %s' % (w1, w2, w3, w4))
 
    if True:
        amb = Amb()
 
        print("\nAmb problem from "
            "http://www.randomhacks.net/articles/2005/10/11/amb-operator:")
        x = amb([1, 2, 3])
        y = amb([4, 5, 6])
 
        for _dummy in amb( lambda x, y: x * y != 8 ):
            print ('%s %s' % (x, y))
```


{{out}}

```txt

Small Pythagorean triples problem:
3 4 5
4 3 5
6 8 10
8 6 10

Rosetta Code Amb problem:
that thing grows slowly

Amb problem from http://www.randomhacks.net/articles/2005/10/11/amb-operator:
1 4
1 5
1 6
2 5
2 6
3 4
3 5
3 6
```



### List Comprehension

{{Trans|Haskell}}
The semantics of Python's list comprehension notation is also formally equivalent to that of the list monad structure in the Haskell versions above.

List comprehensions provide quite a clean and natural encoding of the '''amb''' relationship between sets of indeterminate values and sets of constraints:


```python
# joins :: String -> String -> Bool
def joins(a, b):
    return a[-1] == b[0]


print (
    [
        ' '.join([w1, w2, w3, w4])
        for w1 in ['the', 'that', 'a']
        for w2 in ['frog', 'elephant', 'thing']
        for w3 in ['walked', 'treaded', 'grows']
        for w4 in ['slowly', 'quickly']
        if joins(w1, w2) and joins(w2, w3) and joins(w3, w4)
    ]
)
```

{{Out}}

```txt
['that thing grows slowly']
```


Rearranging this by pairing each indeterminate value with a predicate may foreground and clarify the way in which list comprehensions encode '''amb''' pairings:


```python
def main():
    print (
        unlines([
            unwords([w1, w2, w3, w4])

            for w1 in ['the', 'that', 'a']
            if True

            for w2 in ['frog', 'elephant', 'thing']
            if joins(w1, w2)

            for w3 in ['walked', 'treaded', 'grows']
            if joins(w2, w3)

            for w4 in ['slowly', 'quickly']
            if joins(w3, w4)
        ])
    )


# joins :: String -> String -> Bool
def joins(a, b):
    return a[-1] == b[0]


# unlines :: [String] -> String
def unlines(xs):
    return '\n'.join(xs)


# unwords :: [String] -> String
def unwords(xs):
    return ' '.join(xs)


if __name__ == '__main__':
    main()
```
 
{{Out}}

```txt
that thing grows slowly
```



### List Monad


Defining '''amb''' directly as the list monad bind operator, and using it to enchain indeterminate values and predicates:


```python
from itertools import chain


# amb :: [a] -> (a -> [b]) -> [b]
def amb(xs):
    return lambda f: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# main :: IO ()
def main():

    xs = enumFromTo(1)(10)
    print ('Pythagorean triples from integers 1-10:')
    print (
        amb(xs)(
            lambda x: amb(xs)
            (lambda y: amb(xs)
                (lambda z: when(
                    x * x + y * y == z * z
                )(
                    (x, y, z)
                )
            ))
        )
    )

    # joins :: String -> String -> Bool
    def joins(a, b):
        return a[-1] == b[0]

    print ('\nRC problem given above:')
    print (
        amb(['the', 'that', 'a'])(
            lambda w1: amb(
                ['frog', 'elephant', 'thing']
            )(lambda w2: amb(
                ['walked', 'treaded', 'grows']
            )(lambda w3: amb(
                ['slowly', 'quickly']
            )(lambda w4: when(
                joins(w1, w2) and joins(w2, w3) and joins(w3, w4)
            )(
                (w1, w2, w3, w4)
            ))))
        )
    )
    print('\nAdditional problem reference in procedural version above:')
    print(
        amb([1, 2, 3])
        (
            lambda x: amb([4, 5, 6])
            (
                lambda y: when(x * y != 8)
                (
                    (x, y)
                )
            )
        )
    )

# GENERIC -------------------------------------------------


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    return lambda n: list(range(m, 1 + n))


# when :: Bool -> [a] -> [a]
def when(p):
    return lambda x: [x] if p else []

# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Pythagorean triples from integers 1-10:
[(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

RC problem given above:
[('that', 'thing', 'grows', 'slowly')]

Additional problem reference in procedural version above:
[(1, 4), (1, 5), (1, 6), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)]
```


Or, if we prefer to pair each indeterminate value with its immediate predicate, rather than using a single compound predicate at the end of the expression:


```python
from itertools import chain


# amb :: [a] -> (a -> [b]) -> [b]
def amb(xs):
    return lambda f: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# when :: Bool -> [a] -> [a]
def when(p):
    return lambda xs: xs if p else []


# TEST ----------------------------------------------------

# joins :: String -> String -> Bool
def joins(a, b):
    return a[-1] == b[0]


print (
    amb(['the', 'that', 'a'])(
        lambda w1: when(True)

        (amb(['frog', 'elephant', 'thing'])
         (lambda w2: when(joins(w1, w2))

          (amb(['walked', 'treaded', 'grows'])
           (lambda w3: when(joins(w2, w3))

            (amb(['slowly', 'quickly'])
             (lambda w4: when(joins(w3, w4))(

                 [w1, w2, w3, w4]
             ))))))
         )
    )
)
```

{{Out}}

```txt
['that', 'thing', 'grows', 'slowly']
```


=={{Header|R}}==
A brute force approach that depends on the expand.grid() function, which generates all possible paths through a list of vectors:

```r
checkSentence <- function(sentence){
# Input: character vector
# Output: whether the sentence formed by the elements of the vector is valid
  for (index in 1:(length(sentence)-1)){
    first.word  <- sentence[index]
    second.word <- sentence[index+1]
    
    last.letter  <- substr(first.word, nchar(first.word), nchar(first.word))
    first.letter <- substr(second.word, 1, 1)
    
    if (last.letter != first.letter){ return(FALSE) } 
  }
  return(TRUE)
}

amb <- function(sets){
# Input: list of character vectors containing all sets to consider
# Output: list of character vectors that are valid
  all.paths      <- apply(expand.grid(sets), 2, as.character)
  all.paths.list <- split(all.paths, 1:nrow(all.paths))
  winners        <- all.paths.list[sapply(all.paths.list, checkSentence)]
  return(winners)
}
```


{{out}}

```r
sentence1 <- c("that", "thing", "grows", "slowly")
sentence2 <- c("rosetta", "code", "is", "cool")
sentence  <- list(sentence1, sentence2)
sapply(sentence, checkSentence)
[1]  TRUE FALSE

set1 <- c("the", "that", "a")
set2 <- c("frog", "elephant", "thing")
set3 <- c("walked", "treaded", "grows")
set4 <- c("slowly", "quickly")
sets <- list(set1, set2, set3, set4)
amb(sets)
$`26`
[1] "that"   "thing"  "grows"  "slowly"
```


## Racket


```Racket

#lang racket

;; A quick `amb' implementation (same as in the Twelve Statements task)
(define failures null)

(define (fail)
  (if (pair? failures) ((first failures)) (error "no more choices!")))

(define (amb/thunks choices)
  (let/cc k (set! failures (cons k failures)))
  (if (pair? choices)
    (let ([choice (first choices)]) (set! choices (rest choices)) (choice))
    (begin (set! failures (rest failures)) (fail))))

(define-syntax-rule (amb E ...) (amb/thunks (list (lambda () E) ...)))

(define (assert condition) (unless condition (fail)))

;; Problem solution

(define (joins? left right)
  (regexp-match? #px"(.)\0\\1" (~a left "\0" right)))

(let ([result (list (amb "the" "that" "a")
                    (amb "frog" "elephant" "thing")
                    (amb "walked" "treaded" "grows")
                    (amb "slowly" "quickly"))])
  (for ([x result] [y (cdr result)]) (assert (joins? x y)))
  result)
;; -> '("that" "thing" "grows" "slowly")

```



## REXX


### version 1

An assumption was made that equivalent lowercase and uppercase (Latin) letters are considered a match.

```rexx
/*REXX program demonstrates the   Amd   operator,  choosing a word from each set.       */
              @.1 = "the    that     a"
              @.2 = "frog   elephant thing"
              @.3 = "walked treaded  grows"
              @.4 = "slowly quickly"
              @.0 = 4                            /*define the number of sets being ised.*/
call Amb 1                                       /*find all word combinations that works*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Amb: procedure expose @.;   parse arg # x;     arg . u       /*ARG uppercases U value.  */
     if #>@.0  then do;  y= word(u, 1)                       /*Y:  is a  uppercased  U. */
                                       do n=2  to words(u);                ?= word(u, n)
                                       if left(?, 1) \== right(y, 1)  then return;    y= ?
                                       end   /*n*/
                         say strip(x)                        /*¬show superfluous blanks.*/
                    end
                                                 /* [↓]  generate all combos recursively*/
            do j=1  for words(@.#);    call Amb   #+1   x   word(@.#, j)
            end   /*j*/
     return
```

{{out|output|text=  when using the default internal inputs:}}

```txt

that thing grows slowly

```



### version 2


```rexx
 /* REXX **************************************************************
 * 25.08.2013 Walter Pachl derived from PL/I
 *********************************************************************/
 mm=0
 w.=''
 l.=0
 Call mkset 1,'the that a if'
 Call mkset 2,'frog elephant thing'
 Call mkset 3,'walked treaded grows trots'
 Call mkset 4,'slowly quickly'

 Call show
 Do i=1 to 3                           /* loop over sets             */
   Call showm
   k=i+1                               /* the following set          */
   Do ii=1 To 10                       /* loop over elements in set k*/
     If words(w.i.ii)=i Then Do        /* a sentence part found      */
       Do jj=1 To 10                   /* loop over following words  */
         If right(w.i.ii,1)=left(w.k.jj,1) Then Do  /* fitting       */
           ns=w.i.ii' 'w.k.jj          /* build new sentence (part)  */
           If words(ns)=k Then         /* 'complete' part            */
             Call add k,ns             /* add to set k               */
           End
         End
       End
     End
   End

 Do jj=1 To 10                         /* show the results           */
   If words(w.4.jj)=4 Then
     Say '-->' w.4.jj
   End
 Return

 add: Procedure Expose w.
 /*********************************************************************
 * add a sentence (part) to set ni
 *********************************************************************/
   Parse Arg ni,s
   Do i=1 To 10 While w.ni.i>''        /* look for an empty slot     */
     End
   w.ni.i=s                            /* add the sentence (part)    */
   Return

 mkset: Procedure Expose w. mm l.
 /*********************************************************************
 * initialize the sets
 *********************************************************************/
   Parse Arg i,wl
   Do j=1 By 1 While wl<>''
     Parse Var wl w.i.j wl
     l.i=max(l.i,length(w.i.j))
     End
   mm=max(mm,j-1)
   Return

show: Procedure Expose w. mm l.
 /*********************************************************************
 * show the input
 *********************************************************************/
 Say 'Input:'
 Do j=1 To mm                          /* output lines               */
   ol=''
   Do i=1 To 4
     ol=ol left(w.i.j,l.i)
     End
   Say strip(ol)
   End;
 say ''
 Return

showm: Procedure Expose w.
 /*********************************************************************
 * show the sets' contents
 *********************************************************************/
  dbg=0
  If dbg Then Do
    Do i=1 To 4
      Do j=1 To 10
        If w.i.j>'' Then
          Say i j w.i.j
        End
      End
    End
  Return
```

Output: identical to PL/I's


## Ring


```ring

# Project : Amb

set1 = ["the","that","a"]
set2 = ["frog","elephant","thing"] 
set3 = ["walked","treaded","grows"] 
set4 = ["slowly","quickly"]
text = amb(set1,set2,set3,set4)
if text != ""
   see "Correct sentence would be: " + nl  + text + nl
else
   see "Failed to fine a correct sentence."
ok

func wordsok(string1, string2)
       if substr(string1,len(string1),1) = substr(string2,1,1)
          return true
       ok
       return false
 
func amb(a,b,c,d)
       for a2 = 1 to len(a)
            for b2 =1 to len(b)
                 for c2 = 1 to len(c)
                      for d2 = 1 to len(d)
                           if wordsok(a[a2],b[b2]) and wordsok(b[b2],c[c2]) and wordsok(c[c2],d[d2])
                              return a[a2]+" "+b[b2]+" "+c[c2]+" "+d[d2]
                           ok
                      next
                 next
            next
       next
       return ""  

```

Output:

```txt

Correct sentence would be: 
that thing grows slowly

```


=={{Header|Ruby}}==

```ruby
require "continuation"

class Amb
  class ExhaustedError < RuntimeError; end

  def initialize
    @fail = proc { fail ExhaustedError, "amb tree exhausted" }
  end

  def choose(*choices)
    prev_fail = @fail
    callcc { |sk|
      choices.each { |choice|
	callcc { |fk|
	  @fail = proc {
	    @fail = prev_fail
	    fk.call(:fail)
	  }
	  if choice.respond_to? :call
	    sk.call(choice.call)
	  else
	    sk.call(choice)
	  end
	}
      }
      @fail.call
    }
  end

  def failure
    choose
  end

  def assert(cond)
    failure unless cond
  end
end

A = Amb.new
w1 = A.choose("the", "that", "a")
w2 = A.choose("frog", "elephant", "thing")
w3 = A.choose("walked", "treaded", "grows")
w4 = A.choose("slowly", "quickly")

A.choose() unless w1[-1] == w2[0]
A.choose() unless w2[-1] == w3[0]
A.choose() unless w3[-1] == w4[0]

puts w1, w2, w3, w4
```



## Rust


```Rust

use std::ops::Add;
struct Amb<'a> {
    list: Vec<Vec<&'a str>>,
}
fn main() {
    let amb = Amb {
        list: vec![
            vec!["the", "that", "a"],
            vec!["frog", "elephant", "thing"],
            vec!["walked", "treaded", "grows"],
            vec!["slowly", "quickly"],
        ],
    };
    match amb.do_amb(0, 0 as char) {
        Some(text) => println!("{}", text),
        None => println!("Nothing found"),
    }
}
impl<'a> Amb<'a> {
    fn do_amb(&self, level: usize, last_char: char) -> Option<String> {
        if self.list.is_empty() {
            panic!("No word list");
        }
        if self.list.len() <= level {
            return Some(String::new());
        }
        let mut res = String::new();
        let word_list = &self.list[level];
        for word in word_list {
            if word.chars().next().unwrap() == last_char || last_char == 0 as char {
                res = res.add(word).add(" ");
                let answ = self.do_amb(level + 1, word.chars().last().unwrap());
                match answ {
                    Some(x) => {
                        res = res.add(&x);
                        return Some(res);
                    }
                    None => res.clear(),
                }
            }
        }
        None
    }
}

```


=={{Header|Scala}}==

```Scala
object Amb {

  def amb(wss: List[List[String]]): Option[String] = {
    def _amb(ws: List[String], wss: List[List[String]]): Option[String] = wss match {
      case Nil => ((Some(ws.head): Option[String]) /: ws.tail)((a, w) => a match {
        case Some(x) => if (x.last == w.head) Some(x + " " + w) else None
        case None => None
      })
      case ws1 :: wss1 => ws1.flatMap(w => _amb(w :: ws, wss1)).headOption
    }
    _amb(Nil, wss.reverse)
  }

  def main(args: Array[String]) {
    println(amb(List(List("the", "that", "a"),
                     List("frog", "elephant", "thing"),
                     List("walked", "treaded", "grows"),
                     List("slowly", "quickly"))))
  }
}
```


=={{Header|Scheme}}==

```scheme
(define fail 
  (lambda () 
    (error "Amb tree exhausted"))) 

(define-syntax amb 
  (syntax-rules () 
    ((AMB) (FAIL))                      ; Two shortcuts. 
    ((AMB expression) expression) 
 
    ((AMB expression ...) 
     (LET ((FAIL-SAVE FAIL)) 
       ((CALL-WITH-CURRENT-CONTINUATION ; Capture a continuation to 
          (LAMBDA (K-SUCCESS)           ;   which we return possibles. 
            (CALL-WITH-CURRENT-CONTINUATION 
              (LAMBDA (K-FAILURE)       ; K-FAILURE will try the next 
                (SET! FAIL K-FAILURE)   ;   possible expression. 
                (K-SUCCESS              ; Note that the expression is 
                 (LAMBDA ()             ;   evaluated in tail position 
                   expression))))       ;   with respect to AMB. 
            ... 
            (SET! FAIL FAIL-SAVE)      ; Finally, if this is reached, 
            FAIL-SAVE)))))))            ;   we restore the saved FAIL. 


(let ((w-1 (amb "the" "that" "a"))
      (w-2 (amb "frog" "elephant" "thing"))
      (w-3 (amb "walked" "treaded" "grows"))
      (w-4 (amb "slowly" "quickly")))
  (define (joins? left right)
    (equal? (string-ref left (- (string-length left) 1)) (string-ref right 0)))
  (if (joins? w-1 w-2) '() (amb))
  (if (joins? w-2 w-3) '() (amb))
  (if (joins? w-3 w-4) '() (amb))
  (list w-1 w-2 w-3 w-4))
```


=={{Header|Seed7}}==

```seed7
$ include "seed7_05.s7i";

const type: setListType is array array string;

const func array string: amb (in string: word1, in setListType: listOfSets) is func
  result
    var array string: ambResult is 0 times "";
  local
    var string: word2 is "";
  begin
    for word2 range listOfSets[1] do
      if length(ambResult) = 0 and word1[length(word1) len 1] = word2[1 len 1] then
        if length(listOfSets) = 1 then
          ambResult := [] (word1) & [] (word2);
        else
          ambResult := amb(word2, listOfSets[2 ..]);
          if length(ambResult) <> 0 then
            ambResult := [] (word1) & ambResult;
          end if;
        end if;
      end if;
    end for;
  end func;

const func array string: amb (in setListType: listOfSets) is func
  result
    var array string: ambResult is 0 times "";
  local
    var string: word1 is "";
  begin
    for word1 range listOfSets[1] do
      if length(ambResult) = 0 then
        ambResult := amb(word1, listOfSets[2 ..]);
      end if;
    end for;
  end func;

const proc: main is func
  local
    var array string: ambResult is 0 times "";
    var string: word is "";
  begin
    ambResult := amb([] ([] ("the", "that", "a"),
                         [] ("frog", "elephant", "thing"),
                         [] ("walked", "treaded", "grows"),
                         [] ("slowly", "quickly")));
    for word range ambResult do
      write(word <& " ");
    end for;
    writeln;
  end func;
```

{{out}}

```txt

that thing grows slowly 

```


=={{Header|SETL}}==

```SETL
program amb;

sets := unstr('[{the that a} {frog elephant thing} {walked treaded grows} {slowly quickly}]');

words := [amb(words): words in sets];
if exists lWord = words(i), rWord in {words(i+1)} |
          lWord(#lWord) /= rWord(1) then
  fail;
end if;

proc amb(words);
  return arb {word in words | ok};
end proc;

end program;
```

Sadly ''ok'' and ''fail'' were only ever implemented in CIMS SETL, and are not in any compiler or interpreter that is available today, so this is not very useful as it stands.

===Alternate version (avoids [http://www.setl-lang.org/wiki/index.php/Backtracking backtracking])===

```SETL
program amb;

sets := unstr('[{the that a} {frog elephant thing} {walked treaded grows} {slowly quickly}]');

print(amb(sets));

proc amb(sets);
  return amb1([], {}, sets);
end proc;

proc amb1(prev, mbLast, sets);
  if sets = [] then
    return prev;
  else
    words fromb sets;
    if exists word in words |
              (forall last in mbLast |
                      last(#last) = word(1)) and
              (exists sentence in {amb1(prev with word, {word}, sets)} |
                      true) then
      return sentence;
    end if;
  end if;
end proc;

end program;
```

We cheat a bit here - this version of ''amb'' must be given the whole list of word sets, and that list is consumed recursively.  It can't pick a word from an individual list.


## Tcl


### Brute Force

Brute force, with quick kill of failing attempts:

```Tcl
set amb {
    {the    that     a}
    {frog   elephant thing}
    {walked treaded  grows}
    {slowly quickly}
}

proc joins {a b} {
    expr {[string index $a end] eq [string index $b 0]}
}

foreach i [lindex $amb 0] {
    foreach j [lindex $amb 1] {
        if ![joins $i $j] continue
        foreach k [lindex $amb 2] {
            if ![joins $j $k] continue
            foreach l [lindex $amb 3] {
                if [joins $k $l] {
                    puts [list $i $j $k $l]
                }
            }
        }
    }
}
```


### With Coroutines

A more sophisticated using Tcl 8.6's coroutine facility that avoids the assumption of what the problem is in the code structure:

```Tcl
package require Tcl 8.6
proc cp {args} {
    coroutine cp.[incr ::cps] apply {{list args} {
	yield [info coroutine]
	foreach item $list {
	    if {[llength $args]} {
		set c [cp {*}$args]
		while 1 { yield [list $item {*}[$c]] }
	    } else { yield $item }
	}
	return -code break
    }} {*}$args
}
proc amb {name filter args} {
    coroutine $name apply {{filter args} {
	set c [cp {*}$args]
	yield [info coroutine]
	while 1 {
	    set value [$c]
	    if {[{*}$filter $value]} { yield $value }
	}
	return -code break
    }} $filter {*}$args
}

proc joins {a b} {
    expr {[string index $a end] eq [string index $b 0]}
}
proc joins* list {
    foreach a [lrange $list 0 end-1] b [lrange $list 1 end] {
	if {![joins $a $b]} {return 0}
    }
    return 1
}

amb words joins* \
    {the    that     a} \
    {frog   elephant thing} \
    {walked treaded  grows} \
    {slowly quickly}
while 1 { puts [words] }
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
set1="the'that'a"
set2="frog'elephant'thing"
set3="walked'treaded'grows"
set4="slowly'quickly"
LOOP w1=set1
 lastw1=EXTRACT (w1,-1,0)
 LOOP w2=set2
 IF (w2.sw.$lastw1) THEN
  lastw2=EXTRACT (w2,-1,0)
  LOOP w3=set3
  IF (w3.sw.$lastw2) THEN
   lastw3=EXTRACT (w3,-1,0)
   LOOP w4=set4
   IF (w4.sw.$lastw3) sentence=JOIN (w1," ",w2,w3,w4)
   ENDLOOP
  ENDIF
  ENDLOOP
 ENDIF
 ENDLOOP
ENDLOOP
PRINT sentence
```

{{out}}

```txt

that thing grows slowly

```



## TXR



### =Delimited Continuations=


Because we are using delimited continuations, we are able to confine the <code>amb</code> computation into a scope. To express this, we define an <code>amb-scope</code> operator which is just a syntactic sugar for using <code>block</code> to create a delimiting prompt whose name is <code>amb-scope</code>. Everything outside of an instance of this operator knows nothing about <code>amb</code> and is not involved in the backtracking flow at all. As far as the outside is concerned, the <code>amb-scope</code> block calculates something, terminates and returns a value, like any other ordinary Lisp form:


```txrlisp
(defmacro amb-scope (. forms)
  ^(block amb-scope ,*forms))
```


Next, we define <code>amb</code> as a function.

But first, a note about a convention: we are using the Lisp object <code>nil</code> not only to represent Boolean false, but also a failure.  Thus <code>(amb nil)</code> fails. A <code>nil</code> return out of the entire <code>amb-scope</code> denotes overall failure.

The function is very simple. It captures a single continuation and binds it to the <code>cont</code> variable, using the <code>suspend</code> macro. Then, it iterates over all of its arguments. Each argument which is <code>nil</code> is ignored. For any other value, the function effectively asks the question, "if, with this argument, I run my future computation to completion (i.e. back up to the delimiting contour defined by <code>amb-scope</code>) will the answer be a Boolean true?". It asks the question simply by invoking the continuation on the argument. If the answer is affirmative, then it breaks out of the loop and returns that argument value immediately.  Otherwise the iteration continues with the next argument, to try a different alternative future. If the loop runs through to completion, then the function returns <code>nil</code>, indicating failure.




```txrlisp
(defun amb (. args)
  (suspend amb-scope cont
    (each ((a args))
      (when (and a (call cont a))
        (return-from amb a)))))
```


And some test code:

{{out}}


```txt
$ txr -i amb.tl 
1> (amb-scope
     (let ((w1 (amb "the" "that" "a"))
           (w2 (amb "frog" "elephant" "thing"))
           (w3 (amb "walked" "treaded" "grows"))
           (w4 (amb "slowly" "quickly")))
       (amb (and (eql [w1 -1] [w2 0])
                 (eql [w2 -1] [w3 0])
                 (eql [w3 -1] [w4 0])))
       (list w1 w2 w3 w4)))
("that" "thing" "grows" "slowly")
2>
```



### =Pattern Language=


This is not exactly the implementation of an operator, but a solution worth presenting. The language has the built in pattern matching and backtracking behavior suited for this type of text mining task.

For convenience, we prepare the data in four files:


```txt
$ cat amb/set1
the
that
a
$ cat amb/set2
frog
elephant
thing
$ cat amb/set3
walked
treaded
grows
$ cat amb/set4
slowly
quickly
```


Then code is:


```txr
@(define first_last (first last whole))
@  (all)
@(skip :greedy)@{last 1}
@  (and)
@{first 1}@(skip)
@  (and)
@whole
@  (end)
@(end)
@(next "amb/set1")
@(skip)
@(first_last fi1 la1 w1)
@(next "amb/set2")
@(skip)
@(first_last la1 la2 w2)
@(next "amb/set3")
@(skip)
@(first_last la2 la3 w3)
@(next "amb/set4")
@(skip)
@(first_last la3 la4 w4)
@(output)
@w1 @w2 @w3 @w4
@(end)
```


{{out|Run}}


```txt
$ ./txr amb.txr 
that thing grows slowly
```


As you can see, this has the "nondeterministic flavor" of Amb. 
The <code>@(skip)</code> directives"magically" skip over the lines of input that do not succeed.

This example naturally handles empty strings, since the <code>first_last</code> function simply does not match such inputs.

Here is how to embed the task's specific data in the code:


```txr
@(define first_last (first last whole))
@  (all)
@(skip :greedy)@{last 1}
@  (and)
@{first 1}@(skip)
@  (and)
@whole
@  (end)
@(end)
@(next :list ("the" "that" "a"))
@(skip)
@(first_last fi1 la1 w1)
@(next :list ("frog" "elephant" "thing"))
@(skip)
@(first_last la1 la2 w2)
@(next :list ("walked" "treaded" "grows"))
@(skip)
@(first_last la2 la3 w3)
@(next :list ("slowly" "quickly"))
@(skip)
@(first_last la3 la4 w4)
@(output)
@w1 @w2 @w3 @w4
@(end)
```



## VBScript


### ==Implementation==


```vb
class ambiguous
	dim sRule

	public property let rule( x )
		sRule = x
	end property
	
	public default function amb(p1, p2)
		amb = eval(sRule)
	end function
end class
```



### ==Invocation==


```vb
dim amb
set amb = new ambiguous

amb.rule = "right(p1,1)=left(p2,1)"

dim w1, w2, w3, w4
for each w1 in split("the that a", " ")
	for each w2 in split("frog elephant thing", " ")
		for each w3 in split("walked treaded grows", " ")
			for each w4 in split("slowly quickly", " ")
				if amb(w1, w2) and amb(w2, w3) and amb(w3, w4) then
					wscript.echo w1, w2, w3, w4
				end if
			next
		next
	next
next
```


{{out}}

```txt

that thing grows slowly

```



## zkl

zkl doesn't support dynamic scoping so no variable update (without using reflection, which is not a good thing).

These solutions assume that the solution space is ordered: the possibilities in a always precede those in b, etc.
 
Some constraints on the constraint to make the task easier: it is a function of two strings rather than n items. All solutions are returned, empty list otherwise.

```zkl
fcn joins(a,b){ a[-1]==b[0] }  // the constraint
```

The do-it-in-one-wack solution:

```zkl
amb(joins,
   T("the","that","a"),
   T("frog","elephant","thing"),
   T("walked","treaded","grows"),
   T("slowly","quickly") 
).println();
```

{{out}}
```txt
L("that thing grows slowly")
```

Or, we can defer the computations (the future method starts a worker thread, the result is not forced until it is used).

```zkl
a:=amb.future(joins,T("the","that","a"),T("frog","elephant","thing"));
b:=amb.future(joins,T("walked","treaded","grows"),T("slowly","squacking"));
c:=amb.future(joins,a,b);  // a future of futures
println(a,b,c); 
c=c.noop();  // trigger the landslide, referencing c forces a result for a,b,c
println(a.noop(),b.noop(),c); // even though a has a result, it doesn't know it until we force it
```

{{out}}

```txt

DeferredDeferredDeferred
L("the elephant","that thing")L("grows slowly","grows squacking")L("that thing grows slowly","that thing grows squacking")

```

Your basic Cartesian product recursive decent tree traversal, making extensive use of varargs:

```zkl
fcn amb(f,a,b,etc){
   fcn(sink,f,a,b,etc){
      abc:=vm.arglist[2,*]; // ((the,that),(frog,elephant))
      if(abc.len()<2) return(sink.write(abc[0][0])); // back out of recursion
      foreach a,b in (abc[0],abc[1]){ // Cartesian product
	 if(f(a,b)) self.fcn(sink,f,T(String(a," ",b)),abc[2,*].xplode());
      }
   }(s:=List(),vm.pasteArgs());
   s
}
```

A more general solution, where each possible solution is a list, which is passed to the constraining function and the first solution found is returned:

```zkl
fcn amb(f,a,b,c,etc){ Walker.cproduct(vm.pasteArgs(1)).filter1(f) }
```


```zkl
   // [()] notation unpacks parameter list: f((1,2,3))-->a=1,b=2,c=3
fcn f([(a,b,c,d)]){ joins(a,b) and joins(b,c) and joins(c,d) }
amb(f, T("the","that","a"), T("frog","elephant","thing"),
    T("walked","treaded","grows"), T("slowly","quickly") 
).println();
```

{{out}}
```txt
L("that","thing","grows","slowly")
```

Here is an example using an infinite list as the first possibility space:

```zkl
amb(fcn([(x,y,z)]){ x*x + y*y == z*z },[1..],[1..10],[1..10]).println();
```

{{out}}
```txt
L(3,4,5)
```


{{omit from|GUISS}}
{{omit from|gnuplot}}
{{omit from|LaTeX}}
{{omit from|Make}}
{{omit from|PlainTeX}}
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}}
