+++
title = "Generator/Exponential"
description = ""
date = 2019-08-09T14:31:42Z
aliases = []
[extra]
id = 8822
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "e",
  "echolisp",
  "elixir",
  "emacs_lisp",
  "erlang",
  "factor",
  "fantom",
  "forth",
  "funl",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lingo",
  "lua",
  "m2000_interpreter",
  "nim",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "sidef",
  "supercollider",
  "swift",
  "tcl",
  "vba",
  "visual_basic_dotnet",
  "xpl0",
  "zkl",
]
+++

A generator is an executable entity (like a function or procedure) that contains code that yields a sequence of values, one at a time, so that each time you call the generator, the next value in the sequence is provided.

Generators are often built on top of coroutines or objects so that the internal state of the object is handled “naturally”.

Generators are often used in situations where a sequence is potentially infinite, and where it is possible to construct the next value of the sequence with only minimal state.


## Task

* Create a function that returns a generation of the m'th powers of the positive integers starting from zero, in order, and without obvious or simple upper limit. (Any upper limit to the generator should not be stated in the source but should be down to factors such as the languages natural integer size limit or computational time/size).
* Use it to create a generator of:
:::*   Squares.
:::*   Cubes.
* Create a new generator that filters all cubes from the generator of squares.
* Drop the first 20 values from this last generator of filtered results, and then show the next 10 values.


Note that this task ''requires'' the use of generators in the calculation of the result.


;Also see:
* [[wp:Generator (computer_science)|Generator]]





## Ada

To modify the internal state, the function uses an access parameter.
For a different approach, see the Random packages of the Ada compiler, which use the so-called "Rosen trick".
With the next release of Ada 2012 functions are allowed to have in-out parameters, which would solve this problem, too.
You could also use procedures instead of functions.

generator.ads:

```Ada
package Generator is

   type Generator is tagged private;
   procedure Reset (Gen : in out Generator);
   function Get_Next (Gen : access Generator) return Natural;

   type Generator_Function is access function (X : Natural) return Natural;
   procedure Set_Generator_Function (Gen  : in out Generator;
                                     Func : Generator_Function);

   procedure Skip (Gen : access Generator'Class; Count : Positive := 1);

private

   function Identity (X : Natural) return Natural;

   type Generator is tagged record
      Last_Source : Natural := 0;
      Last_Value  : Natural := 0;
      Gen_Func    : Generator_Function := Identity'Access;
   end record;

end Generator;
```


generator-filtered.ads:

```Ada
package Generator.Filtered is

   type Filtered_Generator is new Generator with private;
   procedure Reset (Gen : in out Filtered_Generator);
   function Get_Next (Gen : access Filtered_Generator) return Natural;

   procedure Set_Source (Gen    : in out Filtered_Generator;
                         Source : access Generator);
   procedure Set_Filter (Gen    : in out Filtered_Generator;
                         Filter : access Generator);

private

   type Filtered_Generator is new Generator with record
      Last_Filter : Natural := 0;
      Source, Filter : access Generator;
   end record;

end Generator.Filtered;
```


generator.adb:

```Ada
package body Generator is

   --------------
   -- Identity --
   --------------

   function Identity (X : Natural) return Natural is
   begin
      return X;
   end Identity;

   ----------
   -- Skip --
   ----------

   procedure Skip (Gen : access Generator'Class; Count : Positive := 1) is
      Val : Natural;
      pragma Unreferenced (Val);
   begin
      for I in 1 .. Count loop
         Val := Gen.Get_Next;
      end loop;
   end Skip;

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : in out Generator) is
   begin
      Gen.Last_Source := 0;
      Gen.Last_Value := 0;
   end Reset;

   --------------
   -- Get_Next --
   --------------

   function Get_Next (Gen : access Generator) return Natural is
   begin
      Gen.Last_Source := Gen.Last_Source + 1;
      Gen.Last_Value := Gen.Gen_Func (Gen.Last_Source);
      return Gen.Last_Value;
   end Get_Next;

   ----------------------------
   -- Set_Generator_Function --
   ----------------------------

   procedure Set_Generator_Function
     (Gen  : in out Generator;
      Func : Generator_Function)
   is
   begin
      if Func = null then
         Gen.Gen_Func := Identity'Access;
      else
         Gen.Gen_Func := Func;
      end if;
   end Set_Generator_Function;

end Generator;
```


generator-filtered.adb:

```Ada
package body Generator.Filtered is

   -----------
   -- Reset --
   -----------

   procedure Reset (Gen : in out Filtered_Generator) is
   begin
      Reset (Generator (Gen));
      Gen.Source.Reset;
      Gen.Filter.Reset;
      Gen.Last_Filter := 0;
   end Reset;

   --------------
   -- Get_Next --
   --------------

   function Get_Next (Gen : access Filtered_Generator) return Natural is
      Next_Source : Natural := Gen.Source.Get_Next;
      Next_Filter : Natural := Gen.Last_Filter;
   begin
      loop
         if Next_Source > Next_Filter then
            Gen.Last_Filter := Gen.Filter.Get_Next;
            Next_Filter := Gen.Last_Filter;
         elsif Next_Source = Next_Filter then
            Next_Source := Gen.Source.Get_Next;
         else
            return Next_Source;
         end if;
      end loop;
   end Get_Next;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source
     (Gen    : in out Filtered_Generator;
      Source : access Generator)
   is
   begin
      Gen.Source := Source;
   end Set_Source;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Gen    : in out Filtered_Generator;
      Filter : access Generator)
   is
   begin
      Gen.Filter := Filter;
   end Set_Filter;

end Generator.Filtered;
```


example use:

```Ada
with Ada.Text_IO;
with Generator.Filtered;

procedure Generator_Test is

   function Square (X : Natural) return Natural is
   begin
      return X * X;
   end Square;

   function Cube (X : Natural) return Natural is
   begin
      return X * X * X;
   end Cube;

   G1, G2 : aliased Generator.Generator;
   F : aliased Generator.Filtered.Filtered_Generator;

begin

   G1.Set_Generator_Function (Func => Square'Unrestricted_Access);
   G2.Set_Generator_Function (Func => Cube'Unrestricted_Access);

   F.Set_Source (G1'Unrestricted_Access);
   F.Set_Filter (G2'Unrestricted_Access);

   F.Skip (20);

   for I in 1 .. 10 loop
      Ada.Text_IO.Put ("I:" & Integer'Image (I));
      Ada.Text_IO.Put (", F:" & Integer'Image (F.Get_Next));
      Ada.Text_IO.New_Line;
   end loop;

end Generator_Test;
```


```txt
I: 1, F: 529
I: 2, F: 576
I: 3, F: 625
I: 4, F: 676
I: 5, F: 784
I: 6, F: 841
I: 7, F: 900
I: 8, F: 961
I: 9, F: 1024
I: 10, F: 1089
```



## AppleScript

Composable generators can be constructed from the methods and persistent properties of ''script'' objects:
```applescript
-- powers :: Gen [Int]
on powers(n)
    script f
        on |λ|(x)
            x ^ n as integer
        end |λ|
    end script
    fmapGen(f, enumFrom(0))
end powers


-- TEST ---------------------------------------------------
on run
    take(10, ¬
        drop(20, ¬
            differenceGen(powers(2), powers(3))))

    --> {529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089}
end run


-- GENERIC ------------------------------------------------

-- Just :: a -> Maybe a
on Just(x)
    {type:"Maybe", Nothing:false, Just:x}
end Just

-- Nothing :: Maybe a
on Nothing()
    {type:"Maybe", Nothing:true}
end Nothing

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple


-- differenceGen :: Gen [a] -> Gen [a] -> Gen [a]
on differenceGen(ga, gb)
    -- All values of ga except any
    -- already seen in gb.
    script
        property g : zipGen(ga, gb)
        property bs : {}
        property xy : missing value
        on |λ|()
            set xy to g's |λ|()
            if missing value is xy then
                xy
            else
                set x to |1| of xy
                set y to |2| of xy
                set bs to {y} & bs
                if bs contains x then
                    |λ|() -- Next in series.
                else
                    x
                end if
            end if
        end |λ|
    end script
end differenceGen


-- drop :: Int -> [a] -> [a]
-- drop :: Int -> String -> String
on drop(n, xs)
    set c to class of xs
    if script is not c then
        if string is not c then
            if n < length of xs then
                items (1 + n) thru -1 of xs
            else
                {}
            end if
        else
            if n < length of xs then
                text (1 + n) thru -1 of xs
            else
                ""
            end if
        end if
    else
        take(n, xs) -- consumed
        return xs
    end if
end drop


-- enumFrom :: Int -> [Int]
on enumFrom(x)
    script
        property v : missing value
        on |λ|()
            if missing value is not v then
                set v to 1 + v
            else
                set v to x
            end if
            return v
        end |λ|
    end script
end enumFrom


-- fmapGen <$> :: (a -> b) -> Gen [a] -> Gen [b]
on fmapGen(f, gen)
    script
        property g : mReturn(f)
        on |λ|()
            set v to gen's |λ|()
            if v is missing value then
                v
            else
                g's |λ|(v)
            end if
        end |λ|
    end script
end fmapGen


-- length :: [a] -> Int
on |length|(xs)
    set c to class of xs
    if list is c or string is c then
        length of xs
    else
        (2 ^ 29 - 1) -- (maxInt - simple proxy for non-finite)
    end if
end |length|


-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if script is class of f then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn


-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
        set ys to {}
        repeat with i from 1 to n
            set v to xs's |λ|()
            if missing value is v then
                return ys
            else
                set end of ys to v
            end if
        end repeat
        return ys
    else
        missing value
    end if
end take


-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    set lng to |length|(xs)
    if 0 = lng then
        Nothing()
    else
        if (2 ^ 29 - 1) as integer > lng then
            if class of xs is string then
                set cs to text items of xs
                Just(Tuple(item 1 of cs, rest of cs))
            else
                Just(Tuple(item 1 of xs, rest of xs))
            end if
        else
            set nxt to take(1, xs)
            if {} is nxt then
                Nothing()
            else
                Just(Tuple(item 1 of nxt, xs))
            end if
        end if
    end if
end uncons


-- zipGen :: Gen [a] -> Gen [b] -> Gen [(a, b)]
on zipGen(ga, gb)
    script
        property ma : missing value
        property mb : missing value
        on |λ|()
            if missing value is ma then
                set ma to uncons(ga)
                set mb to uncons(gb)
            end if
            if Nothing of ma or Nothing of mb then
                missing value
            else
                set ta to Just of ma
                set tb to Just of mb
                set x to Tuple(|1| of ta, |1| of tb)
                set ma to uncons(|2| of ta)
                set mb to uncons(|2| of tb)
                return x
            end if
        end |λ|
    end script
end zipGen
```

```txt
{529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089}
```



## ALGOL 68

'''File: Template.Generator.a68'''
```algol68
MODE YIELDVALUE = PROC(VALUE)VOID;
MODE GENVALUE = PROC(YIELDVALUE)VOID;

PROC gen filtered = (GENVALUE gen candidate, gen exclude, YIELDVALUE yield)VOID: (
    VALUE candidate; SEMA have next exclude = LEVEL 0;
    VALUE exclude;   SEMA get next exclude  = LEVEL 0;
    BOOL initialise exclude := TRUE;

    PAR ( # run each generator in a different thread #
# Thread 1 #
      # FOR VALUE next exclude IN # gen exclude( # ) DO #
      ##   (VALUE next exclude)VOID: (
            DOWN get next exclude; exclude := next exclude;
            IF candidate <= exclude THEN
                UP have next exclude
            ELSE
                UP get next exclude
            FI
      # OD #)),
# Thread 2 #
      # FOR VALUE next candidate IN # gen candidate( # ) DO #
      ##   (VALUE next candidate)VOID: (
            candidate := next candidate;
            IF initialise exclude ORF candidate > exclude THEN
                UP get next exclude;
                DOWN have next exclude; # wait for result #
                initialise exclude := FALSE
            FI;
            IF candidate < exclude THEN
                yield(candidate)
            FI
      # OD #))
    )
);

PROC gen slice = (GENVALUE t, VALUE start, stop, YIELDVALUE yield)VOID: (
  INT index := 0;
  # FOR VALUE i IN # t( # ) DO #
  ##   (VALUE i)VOID: (
      IF   index >= stop THEN done
      ELIF index >= start THEN yield(i) FI;
      index +:= 1
  # OD # ));
    done: SKIP
);

PROC get list = (GENVALUE gen)[]VALUE: (
    INT upb := 0;
    INT ups := 2;
    FLEX [ups]VALUE out;
  # FOR VALUE i IN # gen( # ) DO #
  ##   (VALUE i)VOID:(
        upb +:= 1;
        IF upb > ups THEN # dynamically grow the array 50% #
          [ups +:= ups OVER 2]VALUE append; append[:upb-1] := out; out := append
        FI;
        out[upb] := i
  # OD # ))
    out[:upb]
);

PROC powers = (VALUE m, YIELDVALUE yield)VOID:
    FOR n FROM 0 DO yield(n ** m) OD;
```
'''File: test.Generator.a68'''
```algol68
#!/usr/local/bin/a68g --script #

MODE VALUE = INT;
PR READ "Template.Generator.a68" PR

GENVALUE squares = powers(2,), cubes = powers(3,);
GENVALUE fil = gen filtered(squares, cubes,);

printf(($g(0)x$, get list(gen slice(fil, 20, 30, )) ))
```

```txt

529 576 625 676 784 841 900 961 1024 1089

```



## C

==={{libheader|libco}}===
libco is a tiny library that adds ''cooperative multithreading'', also known as ''coroutines'', to the C language.
Its <tt>co_switch(x)</tt> function pauses the current cothread and resumes the other cothread <tt>x</tt>.

This example provides <tt>next64()</tt> and <tt>yield64()</tt>, to generate 64-bit integers. <tt>next64()</tt> switches to a generator.
Then the generator passes some 64-bit integer to <tt>yield64()</tt>, which switches to the first cothread, where <tt>next64()</tt> returns this 64-bit integer.


```c
#include <inttypes.h> /* int64_t, PRId64 */
#include <stdlib.h>	/* exit() */
#include <stdio.h>	/* printf() */

#include <libco.h>	/* co_{active,create,delete,switch}() */



/* A generator that yields values of type int64_t. */
struct gen64 {
	cothread_t giver;	/* this cothread calls yield64() */
	cothread_t taker;	/* this cothread calls next64() */
	int64_t given;
	void (*free)(struct gen64 *);
	void *garbage;
};

/* Yields a value. */
inline void
yield64(struct gen64 *gen, int64_t value)
{
	gen->given = value;
	co_switch(gen->taker);
}

/* Returns the next value that the generator yields. */
inline int64_t
next64(struct gen64 *gen)
{
	gen->taker = co_active();
	co_switch(gen->giver);
	return gen->given;
}

static void
gen64_free(struct gen64 *gen)
{
	co_delete(gen->giver);
}

struct gen64 *entry64;

/*
 * Creates a cothread for the generator. The first call to next64(gen)
 * will enter the cothread; the entry function must copy the pointer
 * from the global variable struct gen64 *entry64.
 *
 * Use gen->free(gen) to free the cothread.
 */
inline void
gen64_init(struct gen64 *gen, void (*entry)(void))
{
	if ((gen->giver = co_create(4096, entry)) == NULL) {
		/* Perhaps malloc() failed */
		fputs("co_create: Cannot create cothread\n", stderr);
		exit(1);
	}
	gen->free = gen64_free;
	entry64 = gen;
}



/*
 * Generates the powers 0**m, 1**m, 2**m, ....
 */
void
powers(struct gen64 *gen, int64_t m)
{
	int64_t base, exponent, n, result;

	for (n = 0;; n++) {
		/*
		 * This computes result = base**exponent, where
		 * exponent is a nonnegative integer. The result
		 * is the product of repeated squares of base.
		 */
		base = n;
		exponent = m;
		for (result = 1; exponent != 0; exponent >>= 1) {
			if (exponent & 1) result *= base;
			base *= base;
		}
		yield64(gen, result);
	}
	/* NOTREACHED */
}

/* stuff for squares_without_cubes() */
#define ENTRY(name, code) static void name(void) { code; }
ENTRY(enter_squares, powers(entry64, 2))
ENTRY(enter_cubes, powers(entry64, 3))

struct swc {
	struct gen64 cubes;
	struct gen64 squares;
	void (*old_free)(struct gen64 *);
};

static void
swc_free(struct gen64 *gen)
{
	struct swc *f = gen->garbage;
	f->cubes.free(&f->cubes);
	f->squares.free(&f->squares);
	f->old_free(gen);
}

/*
 * Generates the squares 0**2, 1**2, 2**2, ..., but removes the squares
 * that equal the cubes 0**3, 1**3, 2**3, ....
 */
void
squares_without_cubes(struct gen64 *gen)
{
	struct swc f;
	int64_t c, s;

	gen64_init(&f.cubes, enter_cubes);
	c = next64(&f.cubes);

	gen64_init(&f.squares, enter_squares);
	s = next64(&f.squares);

	/* Allow other cothread to free this generator. */
	f.old_free = gen->free;
	gen->garbage = &f;
	gen->free = swc_free;

	for (;;) {
		while (c < s)
			c = next64(&f.cubes);
		if (c != s)
			yield64(gen, s);
		s = next64(&f.squares);
	}
	/* NOTREACHED */
}

ENTRY(enter_squares_without_cubes, squares_without_cubes(entry64))

/*
 * Look at the sequence of numbers that are squares but not cubes.
 * Drop the first 20 numbers, then print the next 10 numbers.
 */
int
main()
{
	struct gen64 gen;
	int i;

	gen64_init(&gen, enter_squares_without_cubes);

	for (i = 0; i < 20; i++)
		next64(&gen);
	for (i = 0; i < 9; i++)
		printf("%" PRId64 ", ", next64(&gen));
	printf("%" PRId64 "\n", next64(&gen));

	gen.free(&gen); /* Free memory. */
	return 0;
}
```


One must download [http://byuu.org/programming/ libco] and give libco.c to the compiler.


```txt
$ libco=/home/kernigh/park/libco
$ cc -I$libco -o main main.c $libco/libco.c
$ ./main
529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089
```



### Using struct to store state


```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef int (*seq_func)(void *);
#define SEQ_BASE seq_func f; int output

/* sort of polymorphing data structure */
typedef struct { SEQ_BASE; } gen_t;

int seq_next(void *state)
{
	return ((gen_t*)state)->output = (*(seq_func*)state)(state);
}

typedef struct {
	SEQ_BASE;
	int pos, n;
} power_gen_t;

int power_next(void *s)
{
	return (int)pow(++((power_gen_t*)s)->pos, ((power_gen_t*)s)->n);
}

void *power_seq(int n)
{
	power_gen_t *s = malloc(sizeof(power_gen_t));
	s->output = -1;
	s->f = power_next;
	s->n = n;
	s->pos = -1;
	return s;
}

typedef struct {
	SEQ_BASE;
	void *in, *without;
} filter_gen_t;

int filter_next(void *s)
{
	gen_t *in = ((filter_gen_t*)s)->in, *wo = ((filter_gen_t*)s)->without;

	do{
		seq_next(in);
		while (wo->output < in->output)
			seq_next(wo);
	} while(wo->output == in->output);

	return in->output;
}

void* filter_seq(gen_t *in, gen_t *without)
{
	filter_gen_t *filt = malloc(sizeof(filter_gen_t));
	filt->in = in;
	filt->without = without;
	filt->f = filter_next;
	filt->output = -1;
	return filt;
}

int main()
{
	int i;
	void *s = filter_seq(power_seq(2), power_seq(3));

	for (i = 0; i < 20; i++) seq_next(s);
	for (i = 0; i < 10; i++)
		printf("%d\n", seq_next(s));

	return 0;
}
```

```txt
529
576
625
676
784
841
900
961
1024
1089
```



## C++


A templated solution.


```cpp
#include <iostream>
using namespace std;

template<class T>
class Generator
{
public:
  virtual T operator()() = 0;
};

// Does nothing unspecialized
template<class T, T P>
class PowersGenerator: Generator<T> {};

// Specialize with other types, or provide a generic version of pow
template<int P>
class PowersGenerator<int, P>: Generator<int>
{
public:
  int i;
  PowersGenerator() { i = 1; }
  virtual int operator()()
  {
    int o = 1;
    for(int j = 0; j < P; ++j) o *= i;
    ++i;
    return o;
  }
};

// Only works with non-decreasing generators
template<class T, class G, class F>
class Filter: Generator<T>
{
public:
  G gen;
  F filter;
  T lastG, lastF;

  Filter() { lastG = gen(); lastF = filter(); }

  virtual T operator()()
  {
    while(lastG >= lastF)
    {
      if(lastG == lastF)
        lastG = gen();
      lastF = filter();
    }

    T out = lastG;
    lastG = gen();
    return out;
  }
};

int main()
{
  Filter<int, PowersGenerator<int, 2>, PowersGenerator<int, 3>> gen;

  for(int i = 0; i < 20; ++i)
    gen();

  for(int i = 20; i < 30; ++i)
    cout << i << ": " << gen() << endl;
}
```


```txt

20: 529
21: 576
22: 625
23: 676
24: 784
25: 841
26: 900
27: 961
28: 1024
29: 1089

```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

static class Program {
    static void Main() {
        Func<int, IEnumerable<int>> ms = m => Infinite().Select(i => (int)Math.Pow(i, m));
        var squares = ms(2);
        var cubes = ms(3);
        var filtered = squares.Where(square => cubes.First(cube => cube >= square) != square);
        var final = filtered.Skip(20).Take(10);
        foreach (var i in final) Console.WriteLine(i);
    }

    static IEnumerable<int> Infinite() {
        var i = 0;
        while (true) yield return i++;
    }
}
```



## Clojure


In Clojure, the role that generator functions take in some other languages
is generally filled by sequences. Most of the functions that produce sequences
produce lazy sequences, many of the standard functions deal with sequences,
and their use in Clojure is extremely idiomatic.
Thus we can define squares and cubes as lazy sequences:


```clojure
(defn powers [m] (for [n (iterate inc 1)] (reduce * (repeat m n)))))
(def squares (powers 2))
(take 5 squares) ; => (1 4 9 16 25)
```


The definition here of the squares-not-cubes lazy sequence uses the loop/recur construct,
which isn't lazy. So we use ''lazy-seq'' explicity:


```clojure
(defn squares-not-cubes
  ([] (squares-not-cubes (powers 2) (powers 3)))
  ([squares cubes]
    (loop [[p2first & p2rest :as p2s] squares, [p3first & p3rest :as p3s] cubes]
      (cond
        (= p2first p3first) (recur p2rest p3rest)
        (> p2first p3first) (recur p2s p3rest)
        :else (cons p2first (lazy-seq (squares-not-cubes p2rest p3s)))))))

(->> (squares-not-cubes) (drop 20) (take 10))
; => (529 576 625 676 784 841 900 961 1024 1089)
```


If we really need a generator function for some reason, any lazy sequence
can be turned into a stateful function. (The inverse of ''seq->fn'' is the standard
function ''repeatedly''.)


```clojure
(defn seq->fn [sequence]
  (let [state (atom (cons nil sequence))]
    (fn [] (first (swap! state rest)))

(def f (seq->fn (squares-not-cubes)))
[(f) (f) (f)] ; => [4 9 16]
```



## Common Lisp


```lisp
(defun take (seq &optional (n 1))
  (values-list (loop repeat n collect (funcall seq))))

(defun power-seq (n)
  (let ((x 0))
    (lambda () (expt (incf x) n))))

(defun filter-seq (s1 s2) ;; remove s2 from s1
  (let ((x1 (take s1)) (x2 (take s2)))
    (lambda ()
      (tagbody g
	(if (= x1 x2)
	     (progn (setf x1 (take s1) x2 (take s2)) (go g)))
	(if (> x1 x2)
	     (progn (setf x2 (take s2)) (go g))))

      (prog1 x1 (setf x1 (take s1))))))

(let ((2not3 (filter-seq (power-seq 2) (power-seq 3))))
  (take 2not3 20) ;; drop 20
  (princ (multiple-value-list (take 2not3 10))))
```



## D


### Efficient Standard Version


```d
void main() {
    import std.stdio, std.bigint, std.range, std.algorithm;

    auto squares = 0.sequence!"n".map!(i => i.BigInt ^^ 2);
    auto cubes = 0.sequence!"n".map!(i => i.BigInt ^^ 3);

    squares.setDifference(cubes).drop(20).take(10).writeln;
}
```

```txt
[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]
```


===Simple Ranges-Based Implementation===
```d
void main() {
    import std.stdio, std.bigint, std.range, std.algorithm;

    auto squares = 0.sequence!"n".map!(i => i.BigInt ^^ 2);
    auto cubes = 0.sequence!"n".map!(i => i.BigInt ^^ 3);

    squares
    .filter!(s => cubes.find!(c => c >= s).front != s)
    .drop(20)
    .take(10)
    .writeln;
}
```

The output is the same.

===More Efficient Ranges-Based Version===

```d
import std.stdio, std.bigint, std.range, std.algorithm;

struct Filtered(R1, R2) if (is(ElementType!R1 == ElementType!R2)) {
    R1 s1;
    R2 s2;
    alias ElementType!R1 T;
    T front, source, filter;

    this(R1 r1, R2 r2) {
        s1 = r1;
        s2 = r2;
        source = s1.front;
        filter = s2.front;
        popFront;
    }

    static immutable empty = false;

    void popFront() {
        while (true) {
            if (source > filter) {
                s2.popFront;
                filter = s2.front;
                continue;
            } else if (source < filter) {
                front = source;
                s1.popFront;
                source = s1.front;
                break;
            }
            s1.popFront;
            source = s1.front;
        }
    }
}

auto filtered(R1, R2)(R1 r1, R2 r2) // Helper function.
if (isInputRange!R1 && isInputRange!R2 &&
    is(ElementType!R1 == ElementType!R2)) {
    return Filtered!(R1, R2)(r1, r2);
}

void main() {
    auto squares = 0.sequence!"n".map!(i => i.BigInt ^^ 2);
    auto cubes = 0.sequence!"n".map!(i => i.BigInt ^^ 3);
    filtered(squares, cubes).drop(20).take(10).writeln;
}
```

The output is the same.

===Closures-Based Version===
```d
import std.stdio;

auto powers(in double e) pure nothrow {
    double i = 0;
    return () => i++ ^^ e;
}

auto filter2(D)(D af, D bf) {
    double a = af(), b = bf();

    return {
        double r;
        while (true) {
            if (a < b) {
                r = a;
                a = af();
                break;
            }
            if (b == a)
                a = af();
            b = bf();
        }
        return r;
    };
}

void main() {
    auto fgen = filter2(2.powers, 3.powers);
    foreach (immutable i; 0 .. 20)
        fgen();
    foreach (immutable i; 0 .. 10)
        write(fgen(), " ");
    writeln;
}
```

```txt
529 576 625 676 784 841 900 961 1024 1089
```



### Generator Range Version


```d
import std.stdio, std.range, std.algorithm, std.concurrency, std.bigint;

auto powers(in uint m) pure nothrow @safe {
    return 0.sequence!"n".map!(i => i.BigInt ^^ m);
}

auto filtered(R1, R2)(R1 r1, R2 r2) /*@safe*/
if (isForwardRange!R1 && isForwardRange!R2 &&
    is(ElementType!R1 == ElementType!R2)) {
    return new Generator!(ElementType!R1)({
        auto v = r1.front; r1.popFront;
        auto f = r2.front; r2.popFront;

        while (true) {
            if (v > f) {
                f = r2.front; r2.popFront;
                continue;
            } else if (v < f)
                yield(v);
            v = r1.front; r1.popFront;
        }
    });
}

void main() {
    auto squares = 2.powers, cubes = 3.powers;
    filtered(squares, cubes).drop(20).take(10).writeln;
}
```

```txt
[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]
```



## E


E does not provide coroutines on the principle that interleaving of execution of code should be explicit to avoid unexpected interactions. However, this problem does not especially require them. Each generator here is simply a function that returns the next value in the sequence when called.


```e
def genPowers(exponent) {
    var i := -1
    return def powerGenerator() {
        return (i += 1) ** exponent
    }
}

def filtered(source, filter) {
    var fval := filter()
    return def filterGenerator() {
        while (true) {
            def sval := source()
            while (sval > fval) {
                fval := filter()
            }
            if (sval < fval) {
                return sval
            }
        }
    }
}

def drop(n, gen) {
    for _ in 1..n { gen() }
}


def squares := genPowers(2)
def cubes := genPowers(3)
def squaresNotCubes := filtered(squares, cubes)
drop(20, squaresNotCubes)
for _ in 1..10 {
    print(`${squaresNotCubes()} `)
}
println()
```



## EchoLisp


```scheme

(lib 'tasks) ;; for make-generator

;; generator of generators
(define (gen-power power)
	(make-generator
		(lambda(n) (yield (expt n power)) (1+ n))  1))

(define powers-2 (gen-power 2))
(define powers-3 (gen-power 3))

(take powers-3 10)
    → (1 8 27 64 125 216 343 512 729 1000)

;; generators substraction
;; input : two generators ga, gb - Sequences must be increasing
;; output : new generator  = ga sequence minus gb sequence

(define (gen-substract ga gb)
	(define (substract b (a))
	(set! a (next ga))
	(while (>= a b) ; avance b until > a
		(when (= a b) (set! a (next ga)))
		(set! b (next gb)))
	(yield a)
	b ) ;; b := next state
	(make-generator substract (next gb)))

;; application
(define task    (gen-substract (gen-power 2) (gen-power 3)))

(drop task 20)
(take task 10)
    → (529 576 625 676 784 841 900 961 1024 1089)

; inspect
task → #generator:state: 1331

```



## Elixir

```elixir
defmodule Generator do
  def filter( source_pid, remove_pid ) do
    first_remove = next( remove_pid )
    spawn( fn -> filter_loop(source_pid, remove_pid, first_remove) end )
  end

  def next( pid ) do
    send(pid, {:next, self})
    receive do
      x -> x
    end
  end

  def power( m ), do: spawn( fn -> power_loop(m, 0) end )

  def task do
    squares_pid = power( 2 )
    cubes_pid = power( 3 )
    filter_pid = filter( squares_pid, cubes_pid )
    for _x <- 1..20, do: next(filter_pid)
    for _x <- 1..10, do: next(filter_pid)
  end

  defp filter_loop( pid1, pid2, n2 ) do
    receive do
      {:next, pid} ->
        {n, new_n2} = filter_loop_next( next(pid1), n2, pid1, pid2 )
        send( pid, n )
        filter_loop( pid1, pid2, new_n2 )
    end
  end

  defp filter_loop_next( n1, n2, pid1, pid2 ) when n1 > n2, do:
       filter_loop_next( n1, next(pid2), pid1, pid2 )
  defp filter_loop_next( n, n, pid1, pid2 ), do:
       filter_loop_next( next(pid1), next(pid2), pid1, pid2 )
  defp filter_loop_next( n1, n2, _pid1, _pid2 ), do: {n1, n2}

  defp power_loop( m, n ) do
    receive do
      {:next, pid} -> send( pid, round(:math.pow(n, m) ) )
    end
    power_loop( m, n + 1 )
  end
end

IO.inspect Generator.task
```


```txt

[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]

```



## Emacs Lisp


This code requires generator library which was introduced in Emacs 25.2


```lisp

(require 'generator)
(setq lexical-binding t)

(iter-defun exp-gen (pow)
  (let ((i -1))
    (while
      (setq i (1+ i))
      (iter-yield (expt i pow)))))

(iter-defun flt-gen ()
  (let* ((g (exp-gen 2))
	 (f (exp-gen 3))
	 (i (iter-next g))
	 (j (iter-next f)))
    (while
      (setq i (iter-next g))
      (while (> i j)
	(setq j (iter-next f)))
      (unless (= i j)
	(iter-yield i)))))


(let ((g (flt-gen))
      (o 'nil))
  (dotimes (i 29)
    (setq o (iter-next g))
    (when (>= i 20)
      (print o))))
```



## Erlang


```Erlang

-module( generator ).

-export( [filter/2, next/1, power/1, task/0] ).

filter(	Source_pid, Remove_pid ) ->
	First_remove = next( Remove_pid	),
        erlang:spawn( fun() -> filter_loop(Source_pid, Remove_pid, First_remove) end ).

next( Pid ) ->
    Pid ! {next, erlang:self()},
    receive X -> X end.

power( M ) -> erlang:spawn( fun() -> power_loop(M, 0) end ).

task() ->
    Squares_pid = power( 2 ),
    Cubes_pid = power( 3 ),
    Filter_pid = filter( Squares_pid, Cubes_pid ),
    [next(Filter_pid) || _X <- lists:seq(1, 20)],
    [next(Filter_pid) || _X <- lists:seq(1, 10)].


filter_loop( Pid1, Pid2, N2 ) ->
        receive
        {next, Pid} ->
	       {N, New_N2} = filter_loop_next( next(Pid1), N2, Pid1, Pid2 ),
               Pid ! N
        end,
        filter_loop( Pid1, Pid2, New_N2 ).

filter_loop_next( N1, N2, Pid1, Pid2 ) when N1 > N2 -> filter_loop_next( N1, next(Pid2), Pid1, Pid2 );
filter_loop_next( N, N, Pid1, Pid2 ) -> filter_loop_next( next(Pid1), next(Pid2), Pid1, Pid2 );
filter_loop_next( N1, N2, _Pid1, _Pid2 ) -> {N1, N2}.

power_loop( M, N ) ->
        receive	{next, Pid} -> Pid ! erlang:round(math:pow(N, M) ) end,
	power_loop( M, N + 1 ).

```

```txt

31> generator:task().
[529,576,625,676,784,841,900,961,1024,1089]

```


=={{header|F_Sharp|F#}}==
```fsharp
let m n = Seq.unfold(fun i -> Some(bigint.Pow(i, n), i + 1I)) 0I

let squares = m 2
let cubes = m 3

let (--) orig veto = Seq.where(fun n -> n <> (Seq.find(fun m -> m >= n) veto)) orig

let ``squares without cubes`` = squares -- cubes

Seq.take 10 (Seq.skip 20 (``squares without cubes``))
|> Seq.toList |> printfn "%A"
```

```txt
[529; 576; 625; 676; 784; 841; 900; 961; 1024; 1089]
```



## Factor

Using lazy lists for our generators:

```factor
USING: fry kernel lists lists.lazy math math.functions
prettyprint ;
IN: rosetta-code.generator-exponential

: mth-powers-generator ( m -- lazy-list )
    [ 0 lfrom ] dip [ ^ ] curry lmap-lazy ;

: lmember? ( elt list -- ? )
    over '[ unswons dup _ >= ] [ drop ] until nip = ;

: 2-not-3-generator ( -- lazy-list )
    2 mth-powers-generator
    [ 3 mth-powers-generator lmember? not ] <lazy-filter> ;

10 2-not-3-generator 20 [ cdr ] times ltake list>array .
```

```txt

{ 529 576 625 676 784 841 900 961 1024 1089 }

```



## Fantom


Using closures to implement generators.


```fantom

class Main
{
  // Create and return a function which generates mth powers when called
  |->Int| make_generator (Int m)
  {
    current := 0
    return |->Int|
    {
      current += 1
      return (current-1).pow (m)
    }
  }

  |->Int| squares_without_cubes ()
  {
    squares := make_generator (2)
    cubes := make_generator (3)
    c := cubes.call
    return |->Int|
    {
      while (true)
      {
        s := squares.call
        while (c < s) { c = cubes.call }
        if (c != s) return s
      }
      return 0
    }
  }

  Void main ()
  {
    swc := squares_without_cubes ()
    20.times { swc.call } // drop 20 values
    10.times // display the next 10
    {
      echo (swc.call)
    }
  }
}

```


```txt

529
576
625
676
784
841
900
961
1024
1089

```



## Forth


```Forth

\ genexp-rcode.fs   Generator/Exponential for RosettaCode.org

\ Generator/filter implementation using return stack as continuations stack
: ENTER         ( cont.addr --  ;borrowed from M.L.Gasanenko papers)
        >R
;
: |             ( f --  ;true->go ahead, false->return into generator )
        IF EXIT THEN R> DROP
;
: GEN           ( --  ;generate forever what is between 'GEN' and ';' )
        BEGIN R@ ENTER AGAIN
;
: STOP          ( f --  ;return to caller of word that contain 'GEN' )
        IF R> DROP R> DROP R> DROP THEN
;

\ Problem at hand
: square        ( n -- n^2 )    dup * ;
: cube          ( n -- n^3 )    dup square * ;

\ Faster tests using info that tested numbers are monotonic growing
        VARIABLE Sqroot         \ last square root
        VARIABLE Cbroot         \ last cubic  root
: square?       ( u -- f  ;test U for square number)
        BEGIN
                Sqroot @ square over <
        WHILE
                1 Sqroot +!
        REPEAT
        Sqroot @ square =
;
: cube?         ( u -- f  ;test U for cubic  number)
        BEGIN
                Cbroot @ cube over <
        WHILE
                1 Cbroot +!
        REPEAT
        Cbroot @ cube =
;
        VARIABLE Counter
: (go)  ( u -- u' )
        GEN 1+ Counter @ 30 >= STOP
        dup square? | dup cube? 0= | Counter @ 20 >= 1 Counter +! | dup .
;
:noname 0 Counter ! 1 Sqroot ! 1 Cbroot ! 0 (go) drop ;
execute cr bye

```

```txt

$ gforth -e "include genexp-rcode.fs"
529 576 625 676 784 841 900 961 1024 1089
$

```



## FunL

{{trans|Haskell}} (for the powers function)
{{trans|Scala}} (for the filter)

```funl
def powers( m ) = map( (^ m), 0.. )

def
  filtered( s@sh:_, ch:ct ) | sh > ch = filtered( s, ct )
  filtered( sh:st, c@ch:_ ) | sh < ch = sh # filtered( st, c )
  filtered( _:st, c ) = filtered( st, c )

println( filtered(powers(2), powers(3)).drop(20).take(10) )
```

```txt

[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]

```



## Go

Most direct and most efficient on a single core is implementing generators with closures.

```go
package main

import (
    "fmt"
    "math"
)

// note: exponent not limited to ints
func newPowGen(e float64) func() float64 {
    var i float64
    return func() (r float64) {
        r = math.Pow(i, e)
        i++
        return
    }
}

// given two functions af, bf, both monotonically increasing, return a
// new function that returns values of af not returned by bf.
func newMonoIncA_NotMonoIncB_Gen(af, bf func() float64) func() float64 {
    a, b := af(), bf()
    return func() (r float64) {
        for {
            if a < b {
                r = a
                a = af()
                break
            }
            if b == a {
                a = af()
            }
            b = bf()
        }
        return
    }
}

func main() {
    fGen := newMonoIncA_NotMonoIncB_Gen(newPowGen(2), newPowGen(3))
    for i := 0; i < 20; i++ {
        fGen()
    }
    for i := 0; i < 10; i++ {
        fmt.Print(fGen(), " ")
    }
    fmt.Println()
}
```

```txt

529 576 625 676 784 841 900 961 1024 1089

```


Alternatively, generators can be implemented in Go with goroutines and channels.
There are tradeoffs however, and often one technique is a significantly better choice.

Goroutines can run concurrently, but there is overhead associated with thread scheduling and channel communication.  Flow control is also different.
A generator implemented as a closure is a function with a single entry point fixed at the beginning.
On repeated calls, execution always starts over at the beginning and ends when a value is returned.
A generator implemented as a goroutine, on the other hand, "returns" a value by sending it on a channel, and then the goroutine continues execution from that point.
This allows more flexibility in structuring code.

```go
package main

import (
    "fmt"
    "math"
)

func newPowGen(e float64) chan float64 {
    ch := make(chan float64)
    go func() {
        for i := 0.; ; i++ {
            ch <- math.Pow(i, e)
        }
    }()
    return ch
}

// given two input channels, a and b, both known to return monotonically
// increasing values, supply on channel c values of a not returned by b.
func newMonoIncA_NotMonoIncB_Gen(a, b chan float64) chan float64 {
    ch := make(chan float64)
    go func() {
        for va, vb := <-a, <-b; ; {
            switch {
            case va < vb:
                ch <- va
                fallthrough
            case va == vb:
                va = <-a
            default:
                vb = <-b

            }
        }
    }()
    return ch
}

func main() {
    ch := newMonoIncA_NotMonoIncB_Gen(newPowGen(2), newPowGen(3))
    for i := 0; i < 20; i++ {
        <-ch
    }
    for i := 0; i < 10; i++ {
        fmt.Print(<-ch, " ")
    }
    fmt.Println()
}
```



## Haskell

Generators in most cases can be implemented using infinite lists in Haskell. Because Haskell is lazy, only as many elements as needed is computed from the infinite list:

```haskell
import Data.List.Ordered

powers :: Int -> [Int]
powers m = map (^ m) [0..]

squares :: [Int]
squares = powers 2

cubes :: [Int]
cubes = powers 3

foo :: [Int]
foo = filter (not . has cubes) squares

main :: IO ()
main = print $ take 10 $ drop 20 foo
```

```txt
[529,576,625,676,784,841,900,961,1024,1089]
```


=={{header|Icon}} and {{header|Unicon}}==
Generators are close to the heart and soul of Icon/Unicon.
Co-expressions let us circumvent the normal backtracking mechanism and get results where we need them.


```Icon
procedure main()

write("Non-cube Squares (21st to 30th):")
every (k := 0, s := noncubesquares()) do
   if(k +:= 1) > 30 then break
   else write(20 < k," : ",s)
end

procedure mthpower(m)   #: generate i^m for i = 0,1,...
while (/i := 0) | (i +:= 1) do suspend i^m
end

procedure noncubesquares()  #: filter for squares that aren't cubes
cu := create mthpower(3)    # co-expressions so that we can
sq := create mthpower(2)    # ... get our results where we need

repeat {
   if c === s then  ( c := @cu , s := @sq )
   else if s > c then c := @cu
   else {
      suspend s
      s := @sq
      }
   }
end
```


Note: The task could be written without co-expressions but would be likely be ugly.
If there is an elegant non-co-expression version please add it as an alternate example.

```txt
Non-cube Squares (21st to 30th):
21 : 529
22 : 576
23 : 625
24 : 676
25 : 784
26 : 841
27 : 900
28 : 961
29 : 1024
30 : 1089
```



## J


Generators are not very natural, in J, because they avoid the use of arrays and instead rely on sequential processing.

Here is a generator for mth powers of a number:


```j
coclass 'mthPower'
  N=: 0
  create=: 3 :0
    M=: y
  )
  next=: 3 :0
    n=. N
    N=: N+1
    n^M
  )
```


And, here are corresponding square and cube generators


```j
stateySquare=: 2 conew 'mthPower'
stateyCube=: 3 conew 'mthPower'
```


Here is a generator for squares which are not cubes:


```j
coclass 'uncubicalSquares'
  N=: 0
  next=: 3 :0"0
    while. (-: <.) 3 %: *: n=. N do. N=: N+1 end. N=: N+1
    *: n
  )
```


And here is an example of its use:


```j
   next__g i.10 [ next__g i.20 [ g=: conew 'uncubicalSquares'
529 576 625 676 784 841 900 961 1024 1089
```


That said, here is a more natural approach, for J.


```j
mthPower=: 1 :'^&m@i.'
squares=: 2 mthPower
cubes=: 3 mthPower
uncubicalSquares=: squares -. cubes
```


The downside of this approach is that it is computing independent sequences.  And for the "uncubicalSquares" verb, it is removing some elements from that sequence.  So you must estimate how many values to generate.  However, this can be made transparent to the user with a simplistic estimator:


```j
uncubicalSquares=: {. squares@<.@p.~&3 1.1 -. cubes
```


Example use:


```j
20 }. uncubicalSquares 30 NB. the 21st through 30th uncubical square
529 576 625 676 784 841 900 961 1024 1089
```



## Java

```java
import java.util.function.LongSupplier;
import static java.util.stream.LongStream.generate;

public class GeneratorExponential implements LongSupplier {
    private LongSupplier source, filter;
    private long s, f;

    public GeneratorExponential(LongSupplier source, LongSupplier filter) {
        this.source = source;
        this.filter = filter;
        f = filter.getAsLong();
    }

    @Override
    public long getAsLong() {
        s = source.getAsLong();

        while (s == f) {
            s = source.getAsLong();
            f = filter.getAsLong();
        }

        while (s > f) {
            f = filter.getAsLong();
        }

        return s;
    }

    public static void main(String[] args) {
        generate(new GeneratorExponential(new SquaresGen(), new CubesGen()))
                .skip(20).limit(10)
                .forEach(n -> System.out.printf("%d ", n));
    }
}

class SquaresGen implements LongSupplier {
    private long n;

    @Override
    public long getAsLong() {
        return n * n++;
    }
}

class CubesGen implements LongSupplier {
    private long n;

    @Override
    public long getAsLong() {
        return n * n * n++;
    }
}
```



```txt
529 576 625 676 784 841 900 961 1024 1089
```



## JavaScript


### Procedural

```JavaScript

function PowersGenerator(m) {
	var n=0;
	while(1) {
		yield Math.pow(n, m);
		n += 1;
	}
}

function FilteredGenerator(g, f){
	var value = g.next();
	var filter = f.next();

	while(1) {
		if( value < filter ) {
			yield value;
			value = g.next();
		} else if ( value > filter ) {
			filter = f.next();
		} else {
			value = g.next();
			filter = f.next();
		}
	}
}



var squares = PowersGenerator(2);
var cubes = PowersGenerator(3);

var filtered = FilteredGenerator(squares, cubes);



for( var x = 0; x < 20; x++ ) filtered.next()
for( var x = 20; x < 30; x++ ) console.logfiltered.next());


```



### =ES6=


```JavaScript
function* nPowerGen(n) {
  let e = 0;
  while (1) { e++ && (yield Math.pow(e, n)); }
}

function* filterGen(gS, gC, skip=0) {
  let s = 0; // The square value
  let c = 0; // The cube value
  let n = 0; // A skip counter

  while(1) {
    s = gS.next().value;
    s > c && (c = gC.next().value);
    s == c ?
      c = gC.next().value :
      n++ && n > skip && (yield s);
  }
}

const filtered = filterGen(nPowerGen(2), nPowerGen(3), skip=20);
```


```JavaScript
// Generate the first 10 values
for (let n = 0; n < 10; n++) {
  console.log(filtered.next().value)
}
```


```txt
529
576
625
676
784
841
900
961
1024
1089
```



### Functional


### =ES6=

Compositional derivation of custom generators:
```javascript
(() => {
    'use strict';

    // main :: IO()
    const main = () => {

        // powers :: Gen [Int]
        const powers = n =>
            fmapGen(
                x => Math.pow(x, n),
                enumFrom(0)
            );

        // xs :: [Int]
        const xs = take(10, drop(20,
            differenceGen(
                powers(2),
                powers(3)
            )
        ));

        console.log(xs);
        // -> [529,576,625,676,784,841,900,961,1024,1089]
    };


    // GENERIC FUNCTIONS ----------------------------------

    // Just :: a -> Maybe a
    const Just = x => ({
        type: 'Maybe',
        Nothing: false,
        Just: x
    });

    // Nothing :: Maybe a
    const Nothing = () => ({
        type: 'Maybe',
        Nothing: true,
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // differenceGen :: Gen [a] -> Gen [a] -> Gen [a]
    function* differenceGen(ga, gb) {
        // All values of generator stream a except any
        // already seen in generator stream b.
        const
            stream = zipGen(ga, gb),
            sb = new Set([]);
        let xy = take(1, stream);
        while (0 < xy.length) {
            const [x, y] = Array.from(xy[0]);
            sb.add(y);
            if (!sb.has(x)) yield x;
            xy = take(1, stream);
        }
    };

    // drop :: Int -> [a] -> [a]
    // drop :: Int -> Generator [a] -> Generator [a]
    // drop :: Int -> String -> String
    const drop = (n, xs) =>
        Infinity > length(xs) ? (
            xs.slice(n)
        ) : (take(n, xs), xs);

    // enumFrom :: Enum a => a -> [a]
    function* enumFrom(x) {
        let v = x;
        while (true) {
            yield v;
            v = 1 + v;
        }
    }

    // fmapGen <$> :: (a -> b) -> Gen [a] -> Gen [b]
    function* fmapGen(f, gen) {
        let v = take(1, gen);
        while (0 < v.length) {
            yield(f(v[0]))
            v = take(1, gen)
        }
    }

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // uncons :: [a] -> Maybe (a, [a])
    const uncons = xs => {
        const lng = length(xs);
        return (0 < lng) ? (
            lng < Infinity ? (
                Just(Tuple(xs[0], xs.slice(1))) // Finite list
            ) : (() => {
                const nxt = take(1, xs);
                return 0 < nxt.length ? (
                    Just(Tuple(nxt[0], xs))
                ) : Nothing();
            })() // Lazy generator
        ) : Nothing();
    };

    // zipGen :: Gen [a] -> Gen [b] -> Gen [(a, b)]
    const zipGen = (ga, gb) => {
        function* go(ma, mb) {
            let
                a = ma,
                b = mb;
            while (!a.Nothing && !b.Nothing) {
                let
                    ta = a.Just,
                    tb = b.Just
                yield(Tuple(fst(ta), fst(tb)));
                a = uncons(snd(ta));
                b = uncons(snd(tb));
            }
        }
        return go(uncons(ga), uncons(gb));
    };

    // MAIN ---
    return main();
})();
```

```txt
[529,576,625,676,784,841,900,961,1024,1089]
```



## jq

'''Part 1: i^m, 2^m and 3^m'''

jq is a purely functional language and so does not have generators
with state.  To generate a sequence of values one-by-one therefore
requires a "next-value" function, the input of which must include
relevant state information.  For convenience, a counter is usually
included.  For generating i^m, therefore, we would have:

```jq
# Compute self^m where m is a non-negative integer:
def pow(m): . as $in | reduce range(0;m) as $i (1; .*$in);

# state: [i, i^m]
def next_power(m): .[0] + 1 | [., pow(m) ];
```

To make such generators easier to use, we shall define filters to
skip and to emit a specified number of items:

```jq
# skip m states, and return the next state
def skip(m; next):
  if m <= 0 then . else next | skip(m-1; next) end;

# emit m states including the initial state
def emit(m; next):
  if m <= 0 then empty else ., (next | emit(m-1; next)) end;
```

'''Examples''':

```jq
# Generate the first 4 values in the sequence i^2:
[0,0] | emit(4; next_power(2)) | .[1]

# Generate all the values in the sequence i^3 less than 100:
[0,0] | recurse(next_power(3) | if .[1] < 100 then . else empty end) | .[1]
```

'''An aside on streams'''

Since the release of version jq 1.4, enhancements for processing
streams of values have been added, notably "foreach" and "limit".
If your version of jq has these enhancements, then it is often
preferable to use them in conjunction with functions that emit streams of values rather than the "next-value" functions that are the focus of this page.

'''Part 2: selection from 2 ^ m'''

```jq
# Infrastructure:
def last(f): reduce f as $i (null; $i);

# emit the last value that satisfies condition, or null
def while(condition; next):
  def w: if condition then ., (next|w) else empty end;
  last(w);

# Powers of m1 that are not also powers of m2.
# filtered_next_power(m1;m2) produces [[i, i^m1], [j, j^m1]] where i^m1
# is not a power of m2 and j^m2 < i^m1
#
def filtered_next_power(m1; m2):
  if . then . else [[0,0],[0,0]] end
  | (.[0] | next_power(m1)) as $next1
  | (.[1] | while( .[1] <= $next1[1]; next_power(m2))) as $next2
  | if $next1[1] == $next2[1]
    then [$next1, $next2] | filtered_next_power(m1;m2)
    else [$next1, $next2]
    end ;

# Emit ten powers of 2 that are NOT powers of 3,
# skipping the first 20 integers satisfying the condition, including 0.
filtered_next_power(2;3)
  | skip(20; filtered_next_power(2;3))
  | emit(10; filtered_next_power(2;3))
  | .[0][1]
```

```sh
$ jq -n -f generators.jq
529
576
625
676
784
841
900
961
1024
1089
```



## Julia

The task can be achieved by using closures, iterators or tasks. Here is a solution using anonymous functions and closures.


```julia
drop(gen::Function, n::Integer) = (for _ in 1:n gen() end; gen)
take(gen::Function, n::Integer) = collect(gen() for _ in 1:n)

function pgen(n::Number)
    x = 0
    return () -> (x += 1) ^ n
end

function genfilter(g1::Function, g2::Function)
    local r1
    local r2 = g2()
    return () -> begin
        r1 = g1()
        while r2 <  r1 r2 = g2() end
        while r1 == r2 r1 = g1() end
        return r1
    end
end

@show take(drop(genfilter(pgen(2), pgen(3)), 20), 10)
```


```txt
take(drop(genfilter(pgen(2), pgen(3)), 20), 10) = [529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]
```



## Kotlin

Coroutines were introduced in version 1.1 of Kotlin but, as yet, are an experimental feature:

```scala
// version 1.1.0
// compiled with flag -Xcoroutines=enable to suppress 'experimental' warning

import kotlin.coroutines.experimental.buildSequence

fun generatePowers(m: Int) =
    buildSequence {
        var n = 0
        val mm = m.toDouble()
        while (true) yield(Math.pow((n++).toDouble(), mm).toLong())
    }

fun generateNonCubicSquares(squares: Sequence<Long>, cubes: Sequence<Long>) =
    buildSequence {
        val iter2 = squares.iterator()
        val iter3 = cubes.iterator()
        var square = iter2.next()
        var cube = iter3.next()
        while (true) {
            if (square > cube) {
                cube = iter3.next()
                continue
            } else if (square < cube) {
                yield(square)
            }
            square = iter2.next()
        }
    }

fun main(args: Array<String>) {
    val squares = generatePowers(2)
    val cubes = generatePowers(3)
    val ncs = generateNonCubicSquares(squares, cubes)
    print("Non-cubic squares (21st to 30th) : ")
    ncs.drop(20).take(10).forEach { print("$it ") } // print 21st to 30th items
    println()
}
```


```txt

Non-cubic squares (21st to 30th) : 529 576 625 676 784 841 900 961 1024 1089

```



## M2000 Interpreter


```M2000 Interpreter

Module Generator {
      PowGen = Lambda (e)-> {
            =lambda i=0, e -> {
                  i++
                  =i**e
            }
      }

      Squares=lambda PowGen=PowGen(2) ->{
            =PowGen()
      }

      Cubes=Lambda PowGen=PowGen(3) -> {
            =PowGen()
      }

      Filter=Lambda  z=Squares(), Squares, m, Cubes->{
            while m<Z {m=cubes()}
            if z=m then z=Squares()
            =z
            z=Squares()
      }

      For i=1 to 20 : dropit=Filter() :Next i

      Document doc$="Non-cubic squares (21st to 30th)"
      Print doc$
      \\ a new line to doc$
      doc$={
      }
      For i=1 to  10 {
            f=Filter()
            Print Format$("I: {0::-2}, F: {1}",i+20, f)
            doc$=Format$("I: {0::-2}, F: {1}",i+20, f)+{
            }
      }
      Clipboard doc$
}
Generator

```


<pre >
Non-cubic squares (21st to 30th)
I: 21, F: 529
I: 22, F: 576
I: 23, F: 625
I: 24, F: 676
I: 25, F: 784
I: 26, F: 841
I: 27, F: 900
I: 28, F: 961
I: 29, F: 1024
I: 30, F: 1089
</pre >


## Lingo

Lingo neither supports coroutines nor first-class functions, and also misses syntactic sugar for implementing real generators. But in the context of for or while loops, simple pseudo-generator objects that store states internally and manipulate data passed by reference can be used to implement generator-like behavior and solve the given task.

```Lingo
squares = script("generator.power").new(2)
cubes   = script("generator.power").new(3)
filter  = script("generator.filter").new(squares, cubes)
filter.skip(20)
res = []
i = 0
repeat while filter.exec(res)
    i = i + 1
    if i>10 then exit repeat
    put res[1]
end repeat
```


```txt

-- 529
-- 576
-- 625
-- 676
-- 784
-- 841
-- 900
-- 961
-- 1024
-- 1089

```


Parent script "generator.power"


```Lingo
property _exp
property _index

-- @constructor
on new (me, e)
    me._exp = e
    me._index = 0
    return me
end

on exec (me, input)
    me._index = me._index+1
    input[1] = integer(power(me._index, me._exp))
    return TRUE
end

on skip (me, steps)
    me._index = me._index + steps
end

on reset (me)
    me._index = 0
end
```


Parent script "generator.filter"


```Lingo
property _genv
property _genf

-- @constructor
on new (me, genv, genf)
    me._genv = genv
    me._genf = genf
    return me
end

on exec (me, input)
    repeat while TRUE
        me._genv.exec(input)
        v = input[1]
        ok = TRUE
        me._genf.reset() -- reset filter generator
        repeat while TRUE
            me._genf.exec(input)
            f = input[1]
            if f>v then exit repeat
            if f=v then
                ok=FALSE
                exit repeat
            end if
        end repeat
        if ok then
            input[1] = v
            exit repeat
        end if
    end repeat
    return TRUE
end

on skip (me, steps)
    repeat with i = 1 to steps
        me.exec([])
    end repeat
end

on reset (me)
    me._genv.reset()
    me._genf.reset()
end
```



## Lua

Generators can be implemented both as closures and as coroutines. The following example demonstrates both.


```Lua

--could be done with a coroutine, but a simple closure works just as well.
local function powgen(m)
  local count = 0
  return function()
    count = count + 1
    return count^m
  end
end

local squares = powgen(2)
local cubes = powgen(3)

local cowrap,coyield = coroutine.wrap, coroutine.yield

local function filter(f,g)
  return cowrap(function()
    local ff,gg = f(), g()
    while true do
      if ff == gg then
        ff,gg = f(), g()
      elseif ff < gg then
        coyield(ff)
        ff = f()
      else
        gg = g()
      end
    end
  end)
end

filter = filter(squares,cubes)

for i = 1,30 do
  local result = filter()
  if i > 20 then
    print(result)
  end
end

```



## Nim


```nim
proc `^`*(base: int, exp: int): int =
  var (base, exp) = (base, exp)
  result = 1

  while exp != 0:
    if (exp and 1) != 0:
      result *= base
    exp = exp shr 1
    base *= base

proc next(s): int =
  for n in s(): return n

proc powers(m): auto =
  iterator it(): int{.closure.} =
    for n in 0 .. <int.high:
      yield n ^ m
  return it

iterator filtered(s1, s2): int =
  var v = next(s1)
  var f = next(s2)
  while true:
    if v > f:
      f = next(s2)
      continue
    elif v < f:
      yield v
    v = next(s1)

var
  squares = powers(2)
  cubes = powers(3)
  i = 1
for x in filtered(squares, cubes):
  if i > 20:
    echo x
  if i >= 30:
    break
  inc i
```

```txt
529
576
625
676
784
841
900
961
1024
1089
```



## PARI/GP

Define two generator functions genpow() and genpow2().

```parigp
g = List(1);		\\ generator stack

genpow(p) = my(a=g[1]++);listput(g,[0,p]);()->g[a][1]++^g[a][2];

genpowf(p,f) = my(a=g[1]++);listput(g,[0,p]);(s=0)->my(q);while(ispower(p=g[a][1]++^g[a][2],f)||(s&&q++<=s),);p;
```


''genpow(power)'' returns a function that returns a simple power generator.

''genpowf(power,filter)'' returns a function thats returns a filtered power generator. This generator accepts an optional skip-parameter.

Create simple power generators:

```txt
gp > squares = genpow(2);
gp > cubes = genpow(3);
gp > cubes()
1
gp > cubes()
8
gp > squares()
1
gp > squares()
4
gp > cubes()
27
gp > squares()
9
```


Create filtered power generator, skip first 20 results and print next 10 values:

```txt
gp > powf2 = genpowf(2,3);
gp > print(powf2(20)); for(i=1,9,print(powf2()))
529
576
625
676
784
841
900
961
1024
1089
```



## Perl

These generators are anonymous subroutines, which are closures.


```perl
# gen_pow($m) creates and returns an anonymous subroutine that will
# generate and return the powers 0**m, 1**m, 2**m, ...
sub gen_pow {
    my $m = shift;
    my $e = 0;
    return sub { return $e++ ** $m; };
}

# gen_filter($g1, $g2) generates everything returned from $g1 that
# is not also returned from $g2. Both $g1 and $g2 must be references
# to subroutines that generate numbers in increasing order. gen_filter
# creates and returns an anonymous subroutine.
sub gen_filter {
    my($g1, $g2) = @_;
    my $v1;
    my $v2 = $g2->();
    return sub {
        for (;;) {
            $v1 = $g1->();
            $v2 = $g2->() while $v1 > $v2;
            return $v1 unless $v1 == $v2;
        }
    };
}

# Create generators.
my $squares = gen_pow(2);
my $cubes = gen_pow(3);
my $squares_without_cubes = gen_filter($squares, $cubes);

# Drop 20 values.
$squares_without_cubes->() for (1..20);

# Print 10 values.
my @answer;
push @answer, $squares_without_cubes->() for (1..10);
print "[", join(", ", @answer), "]\n";
```


```txt
[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]
```



## Perl 6

As with Haskell, generators are disguised as lazy lists in Perl 6.

```perl6
sub powers($m) { $m XR** 0..* }

my @squares = powers(2);
my @cubes   = powers(3);

sub infix:<with-out> (@orig,@veto) {
    gather for @veto -> $veto {
        take @orig.shift while @orig[0] before $veto;
        @orig.shift if @orig[0] eqv $veto;
    }
}

say (@squares with-out @cubes)[20 ..^ 20+10].join(', ');
```


```txt
529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089
```



## Phix


```Phix
--
-- demo\rosetta\Generator_Exponential.exw
--
### ================================

--
bool terminate = false

atom res

procedure powers(integer p)
integer i=0
    while not terminate do
        res = power(i,p)
        task_suspend(task_self())
        task_yield()
        i += 1
    end while
end procedure

constant squares = task_create(routine_id("powers"),{2}),
         cubes   = task_create(routine_id("powers"),{3})

atom square, cube
task_schedule(cubes,1)
task_yield()
cube = res
for i=1 to 30 do
    while 1 do
        task_schedule(squares,1)
        task_yield()
        square = res
        while cube<square do
            task_schedule(cubes,1)
            task_yield()
            cube = res
        end while
        if square!=cube then exit end if
    end while
    if i>20 then
        ?square
    end if
end for
terminate = 1
```

```txt

529
576
625
676
784
841
900
961
1024
1089

```



## PHP

```php
<?php
function powers($m) {
    for ($n = 0; ; $n++) {
        yield pow($n, $m);
    }
}

function filtered($s1, $s2) {
    while (true) {
        list($v, $f) = [$s1->current(), $s2->current()];
        if ($v > $f) {
            $s2->next();
            continue;
        } else if ($v < $f) {
            yield $v;
        }
        $s1->next();
    }
}

list($squares, $cubes) = [powers(2), powers(3)];
$f = filtered($squares, $cubes);
foreach (range(0, 19) as $i) {
    $f->next();
}
foreach (range(20, 29) as $i) {
    echo $i, "\t", $f->current(), "\n";
    $f->next();
}
?>
```


```txt

20	529
21	576
22	625
23	676
24	784
25	841
26	900
27	961
28	1024
29	1089

```



## PicoLisp

Coroutines are available only in the 64-bit version.

```PicoLisp
(de powers (M)
   (co (intern (pack 'powers M))
      (for (I 0 (inc 'I))
         (yield (** I M)) ) ) )

(de filtered (N M)
   (co 'filtered
      (let (V (powers N)  F (powers M))
         (loop
            (if (> V F)
               (setq F (powers M))
               (and (> F V) (yield V))
               (setq V (powers N)) ) ) ) ) )

(do 20 (filtered 2 3))
(do 10 (println (filtered 2 3)))
```

```txt
529
576
625
676
784
841
900
961
1024
1089
```



## PL/I


```PL/I

Generate: procedure options (main);   /* 27 October 2013 */
        declare j fixed binary;
        declare r fixed binary;

        /* Ignore the first 20 values */
        do j = 1 to 20;
           /* put edit (filter() ) (f(6)); */
           r = filter ();
        end;
        put skip;
        do j = 1 to 10;
           put edit (filter() ) (f(6));
        end;

/* filters out cubes from the result of the square generator. */
filter: procedure returns (fixed binary);
   declare n fixed binary static initial (-0);
   declare (i, j, m) fixed binary;

   do while ('1'b);
      m = squares();
      r = 0;
      do j = 1 to m;
         if m = cubes() then go to ignore;
      end;
      return (m);
ignore:
   end;
end filter;

squares: procedure returns (fixed binary);
        declare i fixed binary static initial (-0);

        i = i + 1;
        return (i**2);
end squares;

cubes: procedure returns (fixed binary);

        r = r + 1;
        return (r**3);
end cubes;

end Generate;

```


```txt

20 dropped values:
     4     9    16    25    36    49    81   100   121   144   169   196   225
 256   289   324   361   400   441   484

Next 10 values:
   529   576   625   676   784   841   900   961  1024  1089

```



## Python

In Python, any function that contains a yield statement becomes a generator. The standard libraries itertools module provides the following functions used in the solution: [http://docs.python.org/library/itertools.html#itertools.count count], that will count up from zero; and [http://docs.python.org/library/itertools.html#itertools.islice islice], which will take a slice from an iterator/generator.

{{works with|Python|2.6+ and 3.x}} (in versions prior to 2.6, replace <code>next(something)</code> with <code>something.next()</code>)

```python
from itertools import islice, count

def powers(m):
    for n in count():
        yield n ** m

def filtered(s1, s2):
    v, f = next(s1), next(s2)
    while True:
        if v > f:
            f = next(s2)
            continue
        elif v < f:
            yield v
        v = next(s1)

squares, cubes = powers(2), powers(3)
f = filtered(squares, cubes)
print(list(islice(f, 20, 30)))
```


```txt
[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]
```



Or, deriving custom generators compositionally:

```python
'''Exponentials as generators'''

from itertools import count, islice


# powers :: Gen [Int]
def powers(n):
    '''A non-finite succession of integers,
       starting at zero,
       raised to the nth power.'''

    def f(x):
        return pow(x, n)

    return map(f, count(0))


# main :: IO ()
def main():
    '''Taking the difference between two derived generators.'''
    print(
        take(10)(
            drop(20)(
                differenceGen(powers(2))(
                    powers(3)
                )
            )
        )
    )


# GENERIC -------------------------------------------------


# differenceGen :: Gen [a] -> Gen [a] -> Gen [a]
def differenceGen(ga):
    '''All values of ga except any
       already seen in gb.'''
    def go(a, b):
        stream = zip(a, b)
        bs = set([])
        while True:
            xy = next(stream, None)
            if None is not xy:
                x, y = xy
                bs.add(y)
                if x not in bs:
                    yield x
            else:
                return
    return lambda gb: go(ga, gb)


# drop :: Int -> [a] -> [a]
# drop :: Int -> String -> String
def drop(n):
    '''The sublist of xs beginning at
       (zero-based) index n.'''
    def go(xs):
        if isinstance(xs, list):
            return xs[n:]
        else:
            take(n)(xs)
            return xs
    return lambda xs: go(xs)


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]
```



## R



```rsplus
powers = function(m)
   {n = -1
    function()
       {n <<- n + 1
        n^m}}

noncubic.squares = local(
   {squares = powers(2)
    cubes = powers(3)
    cube = cubes()
    function()
       {square = squares()
        while (1)
           {if (square > cube)
               {cube <<- cubes()
                next}
            else if (square < cube)
               {return(square)}
            else
               {square = squares()}}}})

for (i in 1:20)
    noncubic.squares()
for (i in 1:10)
    message(noncubic.squares())
```



## Racket



```racket

#lang racket

(require racket/generator)

;; this is a function that returns a powers generator, not a generator
(define (powers m)
  (generator ()
    (for ([n (in-naturals)]) (yield (expt n m)))))

(define squares (powers 2))
(define cubes   (powers 3))

;; same here
(define (filtered g1 g2)
  (generator ()
    (let loop ([n1 (g1)] [n2 (g2)])
      (cond [(< n1 n2) (yield n1) (loop (g1) n2)]
            [(> n1 n2) (loop n1 (g2))]
            [else (loop (g1) (g2))]))))

(for/list ([x (in-producer (filtered squares cubes) (lambda (_) #f))]
           [i 30] #:when (>= i 20))
  x)

```


```txt

'(529 576 625 676 784 841 900 961 1024 1089)

```



## REXX


The generators   (below, the   <big> Gxxxxx </big>   functions)   lie dormant until a request is made for a specific generator index.

```rexx
/*REXX program demonstrates how to use a  generator  (also known as an  iterator).      */
parse arg N .;   if N=='' |  N==","  then N=20   /*N  not specified?   Then use default.*/
@.=                                              /* [↓]  calculate squares,cubes,pureSq.*/
         do i=1  for N;   call Gsquare     i
                          call Gcube       i
                          call GpureSquare i     /*these are  cube─free  square numbers.*/
         end   /*i*/

     do k=1  for N;  @.pureSquare.k=;  end /*k*/ /*this is used to drop  1st  N  values.*/

w=length(N+10);               ps= 'pure square'  /*the width of the numbers;  a literal.*/

     do m=N+1  for 10;    say ps   right(m, w)":"     right(GpureSquare(m), 3*w)
     end       /*m*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Gpower:      procedure expose @.;       parse arg x,p;   q=@.pow.x.p
             if q\==''  then return q;  _=x**p;          @.pow.x.p=_
             return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
Gsquare:     procedure expose @.;       parse arg x;     q=@.square.x
             if q==''  then @.square.x=Gpower(x, 2)
             return @.square.x
/*──────────────────────────────────────────────────────────────────────────────────────*/
Gcube:       procedure expose @.;       parse arg x;     q=@.cube.x
             if q==''  then @.cube.x=Gpower(x, 3)        _=@.cube.x;     @.3pow._=1
             return @.cube.x
/*──────────────────────────────────────────────────────────────────────────────────────*/
GpureSquare: procedure expose @.;       parse arg x;     q=@.pureSquare.x
             if q\==''  then return q
             #=0
                   do j=1  until #==x;  ?=Gpower(j, 2)        /*search for pure square. */
                   if @.3pow.?==1  then iterate               /*is it a power of three? */
                   #=#+1;               @.pureSquare.#=?      /*assign next pureSquare. */
                   end   /*j*/
             return @.pureSquare.x
```

'''output'''   when using the default value:

```txt

pure square 21:    529
pure square 22:    576
pure square 23:    625
pure square 24:    676
pure square 25:    784
pure square 26:    841
pure square 27:    900
pure square 28:    961
pure square 29:   1024
pure square 30:   1089

```



## Ruby

This first solution cheats and uses only one generator! It has three iterators <tt>powers(2)</tt>, <tt>powers(3)</tt> and <tt>squares_without_cubes</tt>, but the only generator runs <tt>powers(3)</tt>.

An iterator is a Ruby method that takes a block parameter, and loops the block for each element. So <tt>powers(2) { |i| puts "Got #{i}" }</tt> would loop forever and print Got 0, Got 1, Got 4, Got 9 and so on. Starting with Ruby 1.8.7, one can use <tt>Object#enum_for</tt> to convert an iterator method to an <tt>Enumerator</tt> object. The <tt>Enumerator#next</tt> method is a generator that runs the iterator method on a separate coroutine. Here <tt>cubes.next</tt> generates the next cube number.


```ruby
# This solution cheats and uses only one generator!

def powers(m)
  return enum_for(__method__, m) unless block_given?
  0.step{|n| yield n**m}
end

def squares_without_cubes
  return enum_for(__method__) unless block_given?

  cubes = powers(3)
  c = cubes.next
  powers(2) do |s|
    c = cubes.next while c < s
    yield s unless c == s
  end
end

p squares_without_cubes.take(30).drop(20)
# p squares_without_cubes.lazy.drop(20).first(10)   # Ruby 2.0+
```

```txt

[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]

```


----

Here is the correct solution, which obeys the ''requirement'' of ''three'' generators.


```ruby
# This solution uses three generators.

def powers(m)
  return enum_for(__method__, m) unless block_given?
  0.step{|n| yield n**m}
end

def squares_without_cubes
  return enum_for(__method__) unless block_given?

  cubes = powers(3) #no block, so this is the first generator
  c = cubes.next
  squares = powers(2) # second generator
  loop do
    s = squares.next
    c = cubes.next while c < s
    yield s unless c == s
  end
end

answer = squares_without_cubes # third generator
20.times { answer.next }
p 10.times.map { answer.next }
```

```txt

[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]

```


If we change both programs to drop the first 1_000_020 values (output: <tt>[1000242014641, 1000244014884, 1000246015129, 1000248015376, 1000250015625, 1000252015876, 1000254016129, 1000256016384, 1000258016641, 1000260016900]</tt>), then the one-generator solution runs much faster than the three-generator solution on a machine with [[MRI]] 1.9.2.

----
The other way.
Powers method is the same as the above.
```ruby
def filtered(s1, s2)
  return enum_for(__method__, s1, s2) unless block_given?
  v, f = s1.next, s2.next
  loop do
    v > f and f = s2.next and next
    v < f and yield v
    v = s1.next
  end
end

squares, cubes = powers(2), powers(3)
f = filtered(squares, cubes)
p f.take(30).last(10)
# p f.lazy.drop(20).first(10)   # Ruby 2.0+
```

Output is the same as the above.


## Scala


```scala
object Generators {
   def main(args: Array[String]): Unit = {
      def squares(n:Int=0):Stream[Int]=(n*n) #:: squares(n+1)
      def cubes(n:Int=0):Stream[Int]=(n*n*n) #:: cubes(n+1)

      def filtered(s:Stream[Int], c:Stream[Int]):Stream[Int]={
         if(s.head>c.head) filtered(s, c.tail)
         else if(s.head<c.head) Stream.cons(s.head, filtered(s.tail, c))
         else filtered(s.tail, c)
      }

      filtered(squares(), cubes()) drop 20 take 10 print
   }
}
```

Here is an alternative filter implementation using pattern matching.

```scala
def filtered2(s:Stream[Int], c:Stream[Int]):Stream[Int]=(s, c) match {
   case (sh#::_, ch#::ct) if (sh>ch) => filtered2(s, ct)
   case (sh#::st, ch#::_) if (sh<ch) => sh #:: filtered2(st, c)
   case (_#::st, _) => filtered2(st, c)
}
```

```txt
529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089, empty
```



## Rust


```Rust
use std::cmp::Ordering;
use std::iter::Peekable;

fn powers(m: u32) -> impl Iterator<Item = u64> {
    (0u64..).map(move |x| x.pow(m))
}

fn noncubic_squares() -> impl Iterator<Item = u64> {
    NoncubicSquares {
        squares: powers(2).peekable(),
        cubes: powers(3).peekable(),
    }
}

struct NoncubicSquares<T: Iterator<Item = u64>, U: Iterator<Item = u64>> {
    squares: Peekable<T>,
    cubes: Peekable<U>,
}

impl<T: Iterator<Item = u64>, U: Iterator<Item = u64>> Iterator for NoncubicSquares<T, U> {
    type Item = u64;
    fn next(&mut self) -> Option<u64> {
        loop {
            match self.squares.peek()?.cmp(self.cubes.peek()?) {
                Ordering::Equal => self.squares.next(),
                Ordering::Greater => self.cubes.next(),
                Ordering::Less => return self.squares.next(),
            };
        }
    }
}

fn main() {
    noncubic_squares()
        .skip(20)
        .take(10)
        .for_each(|x| print!("{} ", x));
    println!();
}
```

```txt
529 576 625 676 784 841 900 961 1024 1089
```


## Scheme


```lisp
(define (power-seq n)
  (let ((i 0))
    (lambda ()
      (set! i (+ 1 i))
      (expt i n))))

(define (filter-seq m n)
  (let* ((s1 (power-seq m)) (s2 (power-seq n))
			    (a 0) (b 0))
    (lambda ()
      (set! a (s1))
      (let loop ()
	(if (>= a b) (begin
		       (cond ((> a b) (set! b (s2)))
			     ((= a b) (set! a (s1))))
		       (loop))))
      a)))

(let loop ((seq (filter-seq 2 3)) (i 0))
  (if (< i 30)
    (begin
      (if (> i 20)
	(begin
	  (display (seq))
	  (newline))
	(seq))
      (loop seq (+ 1 i)))))
```

```txt
576
625
676
784
841
900
961
1024
1089
```



## Sidef

```ruby
func gen_pow(m) {
    var e = 0;
    func { e++ ** m };
}

func gen_filter(g1, g2) {
    var v2 = g2.run;
    func {
        loop {
            var v1 = g1.run;
            while (v1 > v2) { v2 = g2.run };
            v1 == v2 || return v1;
        }
    }
}

# Create generators.
var squares = gen_pow(2);
var cubes = gen_pow(3);
var squares_without_cubes = gen_filter(squares, cubes);

# Drop 20 values.
20.times { squares_without_cubes() };

# Print 10 values.
var answer = [];
10.times { answer.append(squares_without_cubes()) };
say answer;
```

```txt

[529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089]

```



## Swift


```swift
func powGen(m: Int) -> GeneratorOf<Int> {
  let power = Double(m)
  var cur: Double = 0
  return GeneratorOf { Int(pow(cur++, power)) }
}

var squares = powGen(2)
var cubes = powGen(3)

var nCube = cubes.next()

var filteredSqs = GeneratorOf<Int> {
  for var nSq = squares.next() ;; nCube = cubes.next() {
    if nCube > nSq {
      return nSq
    } else if nCube == nSq {
      nSq = squares.next()
    }
  }
}

extension GeneratorOf {
  func drop(n: Int) -> GeneratorOf<T> {
    var g = self
    for _ in 0..<n {g.next()}
    return GeneratorOf{g.next()}
  }
  func take(n: Int) -> GeneratorOf<T> {
    var (i, g) = (0, self)
    return GeneratorOf{++i > n ? nil : g.next()}
  }
}

for num in filteredSqs.drop(20).take(10) {
  print(num)
}

//529
//576
//625
//676
//784
//841
//900
//961
//1024
//1089
```



## SuperCollider



```SuperCollider

f = { |m| {:x, x<-(0..) } ** m };
g = f.(2);
g.nextN(10); // answers  [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81 ]

```


patterns are stream generators:


```SuperCollider
(
f = Pseries(0, 1)
g = f ** 2;
g.asStream.nextN(10); // answers  [ 0, 1, 4, 9, 16, 25, 36, 49, 64, 81 ]
)

```


supercollider has no "without" stream function, this builds one:


```SuperCollider
(
var filter = { |a, b, func| // both streams are assumed to be ordered
	Prout {
		var astr, bstr;
		var aval, bval;
		astr = a.asStream;
		bstr = b.asStream;
		bval = bstr.next;
		while {
			aval = astr.next;
			aval.notNil
		} {
			while {
				bval.notNil and: { bval < aval }
			} {
				bval = bstr.next;
			};
			if(func.value(aval, bval)) { aval.yield };
		}
	}
};
var without = filter.(_, _, { |a, b|  a != b }); // partially apply function

f = Pseries(0, 1);

g = without.(f ** 2, f ** 3);
h = g.drop(20);
h.asStream.nextN(10);
)

answers: [ 529, 576, 625, 676, 784, 841, 900, 961, 1024, 1089 ]

```



## Tcl

Tcl implements generators in terms of coroutines.
If these generators were terminating, they would finish by doing <code>return -code break</code> so as to terminate the calling loop context that is doing the extraction of the values from the generator.

```tcl
package require Tcl 8.6

proc powers m {
    yield
    for {set n 0} true {incr n} {
	yield [expr {$n ** $m}]
    }
}
coroutine squares powers 2
coroutine cubes powers 3
coroutine filtered apply {{s1 s2} {
    yield
    set f [$s2]
    set v [$s1]
    while true {
	if {$v > $f} {
	    set f [$s2]
	    continue
	} elseif {$v < $f} {
	    yield $v
	}
	set v [$s1]
    }
}} squares cubes

# Drop 20
for {set i 0} {$i<20} {incr i} {filtered}
# Take/print 10
for {} {$i<30} {incr i} {
    puts [filtered]
}
```

```txt

529
576
625
676
784
841
900
961
1024
1089

```



## VBA


```vb
Public lastsquare As Long
Public nextsquare As Long
Public lastcube As Long
Public midcube As Long
Public nextcube As Long
Private Sub init()
    lastsquare = 1
    nextsquare = -1
    lastcube = -1
    midcube = 0
    nextcube = 1
End Sub

Private Function squares() As Long
    lastsquare = lastsquare + nextsquare
    nextsquare = nextsquare + 2
    squares = lastsquare
End Function

Private Function cubes() As Long
    lastcube = lastcube + nextcube
    nextcube = nextcube + midcube
    midcube = midcube + 6
    cubes = lastcube
End Function

Public Sub main()
    init
    cube = cubes
    For i = 1 To 30
        Do While True
            square = squares
            Do While cube < square
                cube = cubes
            Loop
            If square <> cube Then
                Exit Do
            End If
        Loop
        If i > 20 Then
            Debug.Print square;
        End If
    Next i
End Sub
```
```txt
 529  576  625  676  784  841  900  961  1024  1089
```



## Visual Basic .NET

'''Compiler:''' >= Visual Studio 2012


```vbnet
Module Program
    Iterator Function IntegerPowers(exp As Integer) As IEnumerable(Of Integer)
        Dim i As Integer = 0
        Do
            Yield CInt(Math.Pow(i, exp))
            i += 1
        Loop
    End Function

    Function Squares() As IEnumerable(Of Integer)
        Return IntegerPowers(2)
    End Function

    Function Cubes() As IEnumerable(Of Integer)
        Return IntegerPowers(3)
    End Function

    Iterator Function SquaresWithoutCubes() As IEnumerable(Of Integer)
        Dim cubeSequence = Cubes().GetEnumerator()
        Dim nextGreaterOrEqualCube As Integer = 0
        For Each curSquare In Squares()
            Do While nextGreaterOrEqualCube < curSquare
                cubeSequence.MoveNext()
                nextGreaterOrEqualCube = cubeSequence.Current
            Loop
            If nextGreaterOrEqualCube <> curSquare Then Yield curSquare
        Next
    End Function

    Sub Main()
        For Each x In From i In SquaresWithoutCubes() Skip 20 Take 10
            Console.WriteLine(x)
        Next
    End Sub
End Module
```


More concise but slower implementation that relies on LINQ-to-objects to achieve generator behavior (runs slower due to re-enumerating Cubes() for every element of Squares()).

```vbnet
    Function SquaresWithoutCubesLinq() As IEnumerable(Of Integer)
        Return Squares().Where(Function(s) s <> Cubes().First(Function(c) c >= s))
    End Function
```


```txt
529
576
625
676
784
841
900
961
1024
1089
```



## XPL0


```XPL0
code ChOut=8, IntOut=11;

func Gen(M);            \Generate Mth powers of positive integers
int  M;
int  N, R, I;
[N:= [0, 0, 0, 0];      \provides own/static variables
R:= 1;
for I:= 1 to M do R:= R*N(M);
N(M):= N(M)+1;
return R;
];

func Filter;            \Generate squares of positive integers that aren't cubes
int  S, C;
[C:= [0];               \static variable = smallest cube > current square
repeat  S:= Gen(2);
        while S > C(0) do C(0):= Gen(3);
until   S # C(0);
return S;
];

int  I;
[for I:= 1 to 20 do Filter;                             \drop first 20 values
 for I:= 1 to 10 do [IntOut(0, Filter);  ChOut(0, ^ )]; \show next 10 values
]
```


```txt

529 576 625 676 784 841 900 961 1024 1089

```



## zkl

Generators are implemented with fibers (aka VMs) and return [lazy] iterators.

```zkl
fcn powers(m){ n:=0.0; while(1){vm.yield(n.pow(m).toInt()); n+=1} }
var squared=Utils.Generator(powers,2), cubed=Utils.Generator(powers,3);

fcn filtered(sg,cg){s:=sg.next(); c:=cg.next();
   while(1){
      if(s>c){c=cg.next(); continue;}
      else if(s<c) vm.yield(s);
      s=sg.next()
   }
}
var f=Utils.Generator(filtered,squared,cubed);
f.drop(20);
f.walk(10).println();
```

```txt
L(529,576,625,676,784,841,900,961,1024,1089)
```

For this task, generators are overkill and overweight,
and lazy infinite squences can be used.
There is no real change to the algorithms (since generators are lazy sequences),
it just has been rewritten in a more functional style.
```zkl
fcn powers(m){[0.0..].tweak(fcn(n,m){a:=n; do(m-1){a*=n} a}.fp1(m))}
var squared=powers(2), cubed=powers(3);

fcn filtered(sg,cg){s:=sg.peek(); c:=cg.peek();
   if(s==c){ cg.next(); sg.next(); return(self.fcn(sg,cg)) }
   if(s>c) { cg.next(); return(self.fcn(sg,cg)); }
   sg.next(); return(s);
}
var f=[0..].tweak(filtered.fp(squared,cubed))
f.drop(20).walk(10).println();
```

