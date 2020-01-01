+++
title = "Set of real numbers"
description = ""
date = 2019-09-08T10:49:17Z
aliases = []
[extra]
id = 10590
[taxonomies]
categories = []
tags = []
+++

{{task}}
All real numbers form the uncountable set ℝ.  Among its subsets, relatively simple are the convex sets, each expressed as a range between two real numbers ''a'' and ''b'' where ''a'' ≤ ''b''.  There are actually four cases for the meaning of "between", depending on open or closed boundary:
* [''a'', ''b'']: {''x'' | ''a'' ≤ ''x'' and ''x'' ≤ ''b'' }
* (''a'', ''b''): {''x'' | ''a'' < ''x'' and ''x'' < ''b'' }
* [''a'', ''b''): {''x'' | ''a'' ≤ ''x'' and ''x'' < ''b'' }
* (''a'', ''b'']: {''x'' | ''a'' < ''x'' and ''x'' ≤ ''b'' }
Note that if ''a'' = ''b'', of the four only [''a'', ''a''] would be non-empty.

'''Task'''
* Devise a way to represent any set of real numbers, for the definition of 'any' in the implementation notes below.
* Provide methods for these common set operations (''x'' is a real number; ''A'' and ''B'' are sets):
:* ''x'' ∈ ''A'': determine if ''x'' is an element of ''A''
:: example: 1 is in [1, 2), while 2, 3, ... are not.
:* ''A'' ∪ ''B'': union of ''A'' and ''B'', i.e. {''x'' | ''x'' ∈ ''A'' or ''x'' ∈ ''B''}
:: example: [0, 2) ∪ (1, 3) = [0, 3); [0, 1) ∪ (2, 3] = well, [0, 1) ∪ (2, 3]
:* ''A'' ∩ ''B'': intersection of ''A'' and ''B'', i.e. {''x'' | ''x'' ∈ ''A'' and ''x'' ∈ ''B''}
:: example: [0, 2) ∩ (1, 3) = (1, 2); [0, 1) ∩ (2, 3] = empty set
:* ''A'' - ''B'': difference between ''A'' and ''B'', also written as ''A'' \ ''B'', i.e. {''x'' | ''x'' ∈ ''A'' and ''x'' ∉ ''B''}
:: example: [0, 2) − (1, 3) = [0, 1]
* Test your implementation by checking if numbers 0, 1, and 2 are in any of the following sets:
:* (0, 1] ∪ [0, 2)
:* [0, 2) ∩ (1, 2]
:* [0, 3) − (0, 1)
:* [0, 3) − [0, 1]

'''Implementation notes'''
* 'Any' real set means 'sets that can be expressed as the union of a finite number of convex real sets'.  Cantor's set needs not apply.
* Infinities should be handled gracefully; indeterminate numbers (NaN) can be ignored.
* You can use your machine's native real number representation, which is probably IEEE floating point, and assume it's good enough (it usually is).

'''Optional work'''
* Create a function to determine if a given set is empty (contains no element).
* Define ''[http://www.wolframalpha.com/input/?i=%7Csin%28pi+x2%29%7C%3E1%2F2%2C+0+%3C+x+%3C+10 A]'' = {''x'' | 0 < ''x'' < 10 and |sin(π ''x''²)| > 1/2 }, ''[http://www.wolframalpha.com/input/?i=%7Csin%28pi+x%29%7C%3E1%2F2%2C+0+%3C+x+%3C+10 B]'' = {''x'' | 0 < ''x'' < 10 and |sin(π ''x'')| > 1/2}, calculate the length of the real axis covered by the set ''A'' − ''B''.  Note that
|sin(π ''x'')| > 1/2 is the same as ''n'' + 1/6 < ''x'' < ''n'' + 5/6 for all integers ''n''; your program does not need to derive this by itself.


## C

Providing an implementation of lambdas would be better, but this should do for now.

```C>#include <math.h

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct RealSet {
    bool(*contains)(struct RealSet*, struct RealSet*, double);
    struct RealSet *left;
    struct RealSet *right;
    double low, high;
};

typedef enum {
    CLOSED,
    LEFT_OPEN,
    RIGHT_OPEN,
    BOTH_OPEN,
} RangeType;

double length(struct RealSet *self) {
    const double interval = 0.00001;
    double p = self->low;
    int count = 0;

    if (isinf(self->low) || isinf(self->high)) return -1.0;
    if (self->high <= self->low) return 0.0;

    do {
        if (self->contains(self, NULL, p)) count++;
        p += interval;
    } while (p < self->high);
    return count * interval;
}

bool empty(struct RealSet *self) {
    if (self->low == self->high) {
        return !self->contains(self, NULL, self->low);
    }
    return length(self) == 0.0;
}

static bool contains_closed(struct RealSet *self, struct RealSet *_, double d) {
    return self->low <= d && d <= self->high;
}

static bool contains_left_open(struct RealSet *self, struct RealSet *_, double d) {
    return self->low < d && d <= self->high;
}

static bool contains_right_open(struct RealSet *self, struct RealSet *_, double d) {
    return self->low <= d && d < self->high;
}

static bool contains_both_open(struct RealSet *self, struct RealSet *_, double d) {
    return self->low < d && d < self->high;
}

static bool contains_intersect(struct RealSet *self, struct RealSet *_, double d) {
    return self->left->contains(self->left, NULL, d) && self->right->contains(self->right, NULL, d);
}

static bool contains_union(struct RealSet *self, struct RealSet *_, double d) {
    return self->left->contains(self->left, NULL, d) || self->right->contains(self->right, NULL, d);
}

static bool contains_subtract(struct RealSet *self, struct RealSet *_, double d) {
    return self->left->contains(self->left, NULL, d) && !self->right->contains(self->right, NULL, d);
}

struct RealSet* makeSet(double low, double high, RangeType type) {
    bool(*contains)(struct RealSet*, struct RealSet*, double);
    struct RealSet *rs;

    switch (type) {
    case CLOSED:
        contains = contains_closed;
        break;
    case LEFT_OPEN:
        contains = contains_left_open;
        break;
    case RIGHT_OPEN:
        contains = contains_right_open;
        break;
    case BOTH_OPEN:
        contains = contains_both_open;
        break;
    default:
        return NULL;
    }

    rs = malloc(sizeof(struct RealSet));
    rs->contains = contains;
    rs->left = NULL;
    rs->right = NULL;
    rs->low = low;
    rs->high = high;
    return rs;
}

struct RealSet* makeIntersect(struct RealSet *left, struct RealSet *right) {
    struct RealSet *rs = malloc(sizeof(struct RealSet));
    rs->contains = contains_intersect;
    rs->left = left;
    rs->right = right;
    rs->low = fmin(left->low, right->low);
    rs->high = fmin(left->high, right->high);
    return rs;
}

struct RealSet* makeUnion(struct RealSet *left, struct RealSet *right) {
    struct RealSet *rs = malloc(sizeof(struct RealSet));
    rs->contains = contains_union;
    rs->left = left;
    rs->right = right;
    rs->low = fmin(left->low, right->low);
    rs->high = fmin(left->high, right->high);
    return rs;
}

struct RealSet* makeSubtract(struct RealSet *left, struct RealSet *right) {
    struct RealSet *rs = malloc(sizeof(struct RealSet));
    rs->contains = contains_subtract;
    rs->left = left;
    rs->right = right;
    rs->low = left->low;
    rs->high = left->high;
    return rs;
}

int main() {
    struct RealSet *a = makeSet(0.0, 1.0, LEFT_OPEN);
    struct RealSet *b = makeSet(0.0, 2.0, RIGHT_OPEN);
    struct RealSet *c = makeSet(1.0, 2.0, LEFT_OPEN);
    struct RealSet *d = makeSet(0.0, 3.0, RIGHT_OPEN);
    struct RealSet *e = makeSet(0.0, 1.0, BOTH_OPEN);
    struct RealSet *f = makeSet(0.0, 1.0, CLOSED);
    struct RealSet *g = makeSet(0.0, 0.0, CLOSED);
    int i;

    for (i = 0; i < 3; ++i) {
        struct RealSet *t;

        t = makeUnion(a, b);
        printf("(0, 1]   union   [0, 2) contains %d is %d\n", i, t->contains(t, NULL, i));
        free(t);

        t = makeIntersect(b, c);
        printf("[0, 2) intersect (1, 2] contains %d is %d\n", i, t->contains(t, NULL, i));
        free(t);

        t = makeSubtract(d, e);
        printf("[0, 3)     -     (0, 1) contains %d is %d\n", i, t->contains(t, NULL, i));
        free(t);

        t = makeSubtract(d, f);
        printf("[0, 3)     -     [0, 1] contains %d is %d\n", i, t->contains(t, NULL, i));
        free(t);

        printf("\n");
    }

    printf("[0, 0] is empty %d\n", empty(g));

    free(a);
    free(b);
    free(c);
    free(d);
    free(e);
    free(f);
    free(g);

    return 0;
}
```

{{out}}

```txt
(0, 1]   union   [0, 2) contains 0 is 1
[0, 2) intersect (1, 2] contains 0 is 0
[0, 3)     -     (0, 1) contains 0 is 1
[0, 3)     -     [0, 1] contains 0 is 0

(0, 1]   union   [0, 2) contains 1 is 1
[0, 2) intersect (1, 2] contains 1 is 0
[0, 3)     -     (0, 1) contains 1 is 1
[0, 3)     -     [0, 1] contains 1 is 0

(0, 1]   union   [0, 2) contains 2 is 0
[0, 2) intersect (1, 2] contains 2 is 0
[0, 3)     -     (0, 1) contains 2 is 1
[0, 3)     -     [0, 1] contains 2 is 1

[0, 0] is empty 0
```



## C++

{{trans|Java}}

```cpp
#include <cassert>
#include <functional>
#include <iostream>

#define _USE_MATH_DEFINES
#include <math.h>

enum RangeType {
    CLOSED,
    BOTH_OPEN,
    LEFT_OPEN,
    RIGHT_OPEN
};

class RealSet {
private:
    double low, high;
    double interval = 0.00001;
    std::function<bool(double)> predicate;

public:
    RealSet(double low, double high, const std::function<bool(double)>& predicate) {
        this->low = low;
        this->high = high;
        this->predicate = predicate;
    }

    RealSet(double start, double end, RangeType rangeType) {
        low = start;
        high = end;

        switch (rangeType) {
        case CLOSED:
            predicate = [start, end](double d) { return start <= d && d <= end; };
            break;
        case BOTH_OPEN:
            predicate = [start, end](double d) { return start < d && d < end; };
            break;
        case LEFT_OPEN:
            predicate = [start, end](double d) { return start < d && d <= end; };
            break;
        case RIGHT_OPEN:
            predicate = [start, end](double d) { return start <= d && d < end; };
            break;
        default:
            assert(!"Unexpected range type encountered.");
        }
    }

    bool contains(double d) const {
        return predicate(d);
    }

    RealSet unionSet(const RealSet& rhs) const {
        double low2 = fmin(low, rhs.low);
        double high2 = fmax(high, rhs.high);
        return RealSet(
            low2, high2,
            [this, &rhs](double d) { return predicate(d) || rhs.predicate(d); }
        );
    }

    RealSet intersect(const RealSet& rhs) const {
        double low2 = fmin(low, rhs.low);
        double high2 = fmax(high, rhs.high);
        return RealSet(
            low2, high2,
            [this, &rhs](double d) { return predicate(d) && rhs.predicate(d); }
        );
    }

    RealSet subtract(const RealSet& rhs) const {
        return RealSet(
            low, high,
            [this, &rhs](double d) { return predicate(d) && !rhs.predicate(d); }
        );
    }

    double length() const {
        if (isinf(low) || isinf(high)) return -1.0; // error value
        if (high <= low) return 0.0;

        double p = low;
        int count = 0;
        do {
            if (predicate(p)) count++;
            p += interval;
        } while (p < high);
        return count * interval;
    }

    bool empty() const {
        if (high == low) {
            return !predicate(low);
        }
        return length() == 0.0;
    }
};

int main() {
    using namespace std;

    RealSet a(0.0, 1.0, LEFT_OPEN);
    RealSet b(0.0, 2.0, RIGHT_OPEN);
    RealSet c(1.0, 2.0, LEFT_OPEN);
    RealSet d(0.0, 3.0, RIGHT_OPEN);
    RealSet e(0.0, 1.0, BOTH_OPEN);
    RealSet f(0.0, 1.0, CLOSED);
    RealSet g(0.0, 0.0, CLOSED);

    for (int i = 0; i <= 2; ++i) {
        cout << "(0, 1] ∪ [0, 2) contains " << i << " is " << boolalpha << a.unionSet(b).contains(i) << "\n";
        cout << "[0, 2) ∩ (1, 2] contains " << i << " is " << boolalpha << b.intersect(c).contains(i) << "\n";
        cout << "[0, 3) - (0, 1) contains " << i << " is " << boolalpha << d.subtract(e).contains(i) << "\n";
        cout << "[0, 3) - [0, 1] contains " << i << " is " << boolalpha << d.subtract(f).contains(i) << "\n";
        cout << endl;
    }

    cout << "[0, 0] is empty is " << boolalpha << g.empty() << "\n";
    cout << endl;

    RealSet aa(
        0.0, 10.0,
        [](double x) { return (0.0 < x && x < 10.0) && abs(sin(M_PI * x * x)) > 0.5; }
    );
    RealSet bb(
        0.0, 10.0,
        [](double x) { return (0.0 < x && x < 10.0) && abs(sin(M_PI * x)) > 0.5; }
    );
    auto cc = aa.subtract(bb);
    cout << "Approximate length of A - B is " << cc.length() << endl;

    return 0;
}
```

{{out}}

```txt
(0, 1] ? [0, 2) contains 0 is true
[0, 2) ? (1, 2] contains 0 is false
[0, 3) - (0, 1) contains 0 is true
[0, 3) - [0, 1] contains 0 is false

(0, 1] ? [0, 2) contains 1 is true
[0, 2) ? (1, 2] contains 1 is false
[0, 3) - (0, 1) contains 1 is true
[0, 3) - [0, 1] contains 1 is false

(0, 1] ? [0, 2) contains 2 is false
[0, 2) ? (1, 2] contains 2 is false
[0, 3) - (0, 1) contains 2 is true
[0, 3) - [0, 1] contains 2 is true

[0, 0] is empty is false

Approximate length of A - B is 2.07587
```



## C sharp


```csharp
using System;

namespace RosettaCode.SetOfRealNumbers
{
    public class Set<TValue>
    {
        public Set(Predicate<TValue> contains)
        {
            Contains = contains;
        }

        public Predicate<TValue> Contains
        {
            get;
            private set;
        }

        public Set<TValue> Union(Set<TValue> set)
        {
            return new Set<TValue>(value => Contains(value) || set.Contains(value));
        }

        public Set<TValue> Intersection(Set<TValue> set)
        {
            return new Set<TValue>(value => Contains(value) && set.Contains(value));
        }

        public Set<TValue> Difference(Set<TValue> set)
        {
            return new Set<TValue>(value => Contains(value) && !set.Contains(value));
        }
    }
}
```

Test:

```csharp
using Microsoft.VisualStudio.TestTools.UnitTesting;
using RosettaCode.SetOfRealNumbers;

namespace RosettaCode.SetOfRealNumbersTest
{
    [TestClass]
    public class SetTest
    {
        [TestMethod]
        public void TestUnion()
        {
            var set =
                new Set<double>(value => 0d < value && value <= 1d).Union(
                    new Set<double>(value => 0d <= value && value < 2d));
            Assert.IsTrue(set.Contains(0d));
            Assert.IsTrue(set.Contains(1d));
            Assert.IsFalse(set.Contains(2d));
        }

        [TestMethod]
        public void TestIntersection()
        {
            var set =
                new Set<double>(value => 0d <= value && value < 2d).Intersection(
                    new Set<double>(value => 1d < value && value <= 2d));
            Assert.IsFalse(set.Contains(0d));
            Assert.IsFalse(set.Contains(1d));
            Assert.IsFalse(set.Contains(2d));
        }

        [TestMethod]
        public void TestDifference()
        {
            var set =
                new Set<double>(value => 0d <= value && value < 3d).Difference(
                    new Set<double>(value => 0d < value && value < 1d));
            Assert.IsTrue(set.Contains(0d));
            Assert.IsTrue(set.Contains(1d));
            Assert.IsTrue(set.Contains(2d));

            set =
                new Set<double>(value => 0d <= value && value < 3d).Difference(
                    new Set<double>(value => 0d <= value && value <= 1d));
            Assert.IsFalse(set.Contains(0d));
            Assert.IsFalse(set.Contains(1d));
            Assert.IsTrue(set.Contains(2d));
        }
    }
}
```



## Clojure

{{trans|Racket}}

```Clojure
(ns rosettacode.real-set)

(defn >=|<= [lo hi] #(<= lo % hi))

(defn >|< [lo hi] #(< lo % hi))

(defn >=|< [lo hi] #(and (<= lo %) (< % hi)))

(defn >|<= [lo hi] #(and (< lo %) (<= % hi)))

(def ⋃ some-fn)
(def ⋂ every-pred)
(defn ∖
  ([s1] s1)
  ([s1 s2]
     #(and (s1 %) (not (s2 %))))
  ([s1 s2 s3]
     #(and (s1 %) (not (s2 %)) (not (s3 %))))
  ([s1 s2 s3 & ss]
     (fn [x] (every? #(not (% x)) (list* s1 s2 s3 ss)))))

(clojure.pprint/pprint
  (map #(map % [0 1 2])
          [(⋃ (>|<= 0 1) (>=|< 0 2))
           (⋂ (>=|< 0 2) (>|<= 1 2))
           (∖ (>=|< 0 3) (>|< 0 1))
           (∖ (>=|< 0 3) (>=|<= 0 1))])

(def ∅ (constantly false))
(def R (constantly true))
(def Z integer?)
(def Q ratio?)
(def I #(∖ R Z Q))
(def N #(∖ Z neg?))
```



## Common Lisp

Common Lisp has a standard way to represent intervals.

```lisp
(deftype set== (a b) `(real ,a ,b))
(deftype set<> (a b) `(real (,a) (,b)))
(deftype set=> (a b) `(real ,a (,b)))
(deftype set<= (a b) `(real (,a) ,b))

(deftype set-union (s1 s2) `(or ,s1 ,s2))
(deftype set-intersection (s1 s2) `(and ,s1 ,s2))
(deftype set-diff (s1 s2) `(and ,s1 (not ,s2)))

(defun in-set-p (x set)
  (typep x set))

(defun test ()
  (let ((set '(set-union (set<= 0 1) (set=> 0 2))))
    (assert (in-set-p 0 set))
    (assert (in-set-p 1 set))
    (assert (not (in-set-p 2 set))))
  (let ((set '(set-intersection (set=> 0 2) (set<= 1 2))))
    (assert (not (in-set-p 0 set)))
    (assert (not (in-set-p 1 set)))
    (assert (not (in-set-p 2 set))))
  (let ((set '(set-diff (set=> 0 3) (set<> 0 1))))
    (assert (in-set-p 0 set))
    (assert (in-set-p 1 set))
    (assert (in-set-p 2 set)))
  (let ((set '(set-diff (set<= 0 3) (set== 0 1))))
    (assert (not (in-set-p 0 set)))
    (assert (not (in-set-p 1 set)))
    (assert (in-set-p 2 set))))
```



## D

{{trans|C sharp}}

```d
struct Set(T) {
    const pure nothrow bool delegate(in T) contains;

    bool opIn_r(in T x) const pure nothrow {
        return contains(x);
    }

    Set opBinary(string op)(in Set set)
    const pure nothrow if (op == "+" || op == "-") {
        static if (op == "+")
            return Set(x => contains(x) || set.contains(x));
        else
            return Set(x => contains(x) && !set.contains(x));
    }

    Set intersection(in Set set) const pure nothrow {
        return Set(x => contains(x) && set.contains(x));
    }
}

unittest { // Test union.
    alias DSet = Set!double;
    const s = DSet(x => 0.0 < x && x <= 1.0) +
              DSet(x => 0.0 <= x && x < 2.0);
    assert(0.0 in s);
    assert(1.0 in s);
    assert(2.0 !in s);
}

unittest { // Test difference.
    alias DSet = Set!double;
    const s1 = DSet(x => 0.0 <= x && x < 3.0) -
               DSet(x => 0.0 < x && x < 1.0);
    assert(0.0 in s1);
    assert(0.5 !in s1);
    assert(1.0 in s1);
    assert(2.0 in s1);

    const s2 = DSet(x => 0.0 <= x && x < 3.0) -
               DSet(x => 0.0 <= x && x <= 1.0);
    assert(0.0 !in s2);
    assert(1.0 !in s2);
    assert(2.0 in s2);

    const s3 = DSet(x => 0 <= x && x <= double.infinity) -
               DSet(x => 1.0 <= x && x <= 2.0);
    assert(0.0 in s3);
    assert(1.5 !in s3);
    assert(3.0 in s3);
}

unittest { // Test intersection.
    alias DSet = Set!double;
    const s = DSet(x => 0.0 <= x && x < 2.0).intersection(
              DSet(x => 1.0 < x && x <= 2.0));
    assert(0.0 !in s);
    assert(1.0 !in s);
    assert(2.0 !in s);
}

void main() {}
```



## EchoLisp

Implementation of sets operations, which apply to '''any''' subsets of ℜ defined by a predicate.

### Sets operations


```scheme

(lib 'match) ;; reader-infix macros

(reader-infix '∈ )
(reader-infix '∩ )
(reader-infix '∪ )
(reader-infix '⊖ ) ;; set difference

(define-syntax-rule (∈ x a) (a x))
(define-syntax-rule (∩ a b) (lambda(x) (and ( a x) (b x))))
(define-syntax-rule (∪ a b) (lambda(x) (or ( a x) (b x))))
(define-syntax-rule (⊖ a b) (lambda(x) (and ( a x) (not (b x)))))

;; predicates to define common sets
(define (∅ x) #f) ;; the empty set predicate
(define (Z x) (integer? x))
(define (N x) (and (Z x) (>= x 0)))
(define (Q x) (rational? x))
(define (ℜ x) #t)

;; predicates to define convex sets
(define (⟦...⟧ a b)(lambda(x) (and (>= x a) (<= x b))))
(define (⟦...⟦ a b)(lambda(x) (and (>= x a) (< x b))))
(define (⟧...⟧ a b)(lambda(x) (and (> x a) (<= x b))))
(define (⟧...⟦ a b)(lambda(x) (and (> x a) (< x b))))

```

{{out}}

```txt

(3/7 ∈ ∅) → #f
(3/7 ∈ Q) → #t
(6.7 ∈ ℜ) → #t

(define A (⟦...⟧ 2 10)) ; closed interval
(define B (⟧...⟦ 5 15)) ; open interval

(8 ∈ A) → #t
(11 ∈ A)→ #f
(define AB (A ∩ B))
(8 ∈ AB) → #t
(3 ∈ AB) → #f
(5 ∈ AB) → #f ;; because B is ]5 .. 15]
(define A-B (A ⊖ B))
(5 ∈ A-B) → #t
(-666 ∈ (⟧...⟧ -Infinity 0 ))   → #t

;; task
 (0 ∈ ((⟧...⟧ 0 1)  ∪  (⟦...⟦ 0 2))) → #t
 (0 ∈ ((⟦...⟦ 0 2)  ∩  (⟧...⟧ 1 2))) → #f
 (0 ∈ ((⟦...⟦ 0 3)  ⊖  (⟧...⟦ 0 1))) → #t
 (0 ∈ ((⟦...⟦ 0 3)  ⊖  (⟦...⟧ 0 1))) → #f
 (1 ∈ ((⟧...⟧ 0 1)  ∪  (⟦...⟦ 0 2))) → #t
 (1 ∈ ((⟦...⟦ 0 2)  ∩  (⟧...⟧ 1 2))) → #f
 (1 ∈ ((⟦...⟦ 0 3)  ⊖  (⟧...⟦ 0 1))) → #t
 (1 ∈ ((⟦...⟦ 0 3)  ⊖  (⟦...⟧ 0 1))) → #f
 (2 ∈ ((⟧...⟧ 0 1)  ∪  (⟦...⟦ 0 2))) → #f
 (2 ∈ ((⟦...⟦ 0 2)  ∩  (⟧...⟧ 1 2))) → #f
 (2 ∈ ((⟦...⟦ 0 3)  ⊖  (⟧...⟦ 0 1))) → #t
 (2 ∈ ((⟦...⟦ 0 3)  ⊖  (⟦...⟧ 0 1))) → #t

```


###  Optional : measuring sets


```scheme

;; The following applies to convex sets ⟧...⟦ Cx,
;; and families F of disjoint convex sets.
;; Cx are implemented as vectors [lo, hi]

 (define-syntax-id _.lo [_ 0])
 (define-syntax-id _.hi [_ 1])

 ;; Cx-ops
 (define (Cx-new lo hi) (if (< lo hi) (vector lo hi) Cx-empty))
 (define (Cx-empty? A) (>= A.lo A.hi))
 (define  Cx-empty #(+Infinity -Infinity))
 (define (Cx-inter A B) (Cx-new (max A.lo B.lo) (min A.hi B.hi)))
 (define (Cx-measure A)  (if (< A.lo A.hi) (- A.hi A.lo) 0))

 ;; Families ops
 (define (CF-measure FA) (for/sum ((A FA)) (Cx-measure A))) ;; because disjoint
 ;; intersection of two families
 (define (CF-inter FA FB) (for*/list ((A FA)(B FB)) (Cx-inter A B)))
 ;; measure of FA/FB = m(FA) - m (FA ∩ FB)
 (define (CF-measure-FA/FB FA FB)
 	(- (CF-measure FA) (CF-measure (CF-inter FA FB))))

;; Application :
;; FA  = {x | 0 < x < 10 and |sin(π x²)| > 1/2 }
 (define FA
 	(for/list ((n 100))
          (Cx-new (sqrt (+ n (// 6))) (sqrt (+ n (// 5 6))))))

;; FB  = {x | 0 < x < 10 and |sin(π x)| > 1/2 }
 (define FB
 	(for/list ((n 10))
          (Cx-new (+ n (// 6)) (+ n (// 5 6)))))

→ (#(0.1667 0.8333) #(1.1667 1.8333) #(2.1667 2.8333)
 #(3.1667 3.8333) #(4.1667 4.8333) #(5.1667 5.8333)
 #(6.1667 6.8333) #(7.1667 7.8333) #(8.1667 8.8333) #(9.1667 9.8333))

 (CF-measure-FA/FB FA FB)
      → 2.075864841184666


```



## Elena

ELENA 4.x :

```elena
import extensions;

extension setOp
{
    union(func)
        = (val => self(val) || func(val) );

    intersection(func)
        = (val => self(val) && func(val) );

    difference(func)
        = (val => self(val) && (func(val)).Inverted );
}

public program()
{
    // union
    var set := (x => x >= 0.0r && x <= 1.0r ).union:(x => x >= 0.0r && x < 2.0r );

    set(0.0r).assertTrue();
    set(1.0r).assertTrue();
    set(2.0r).assertFalse();

    // intersection
    var set2 := (x => x >= 0.0r && x < 2.0r ).intersection:(x => x >= 1.0r && x <= 2.0r );

    set2(0.0r).assertFalse();
    set2(1.0r).assertTrue();
    set2(2.0r).assertFalse();

    // difference
    var set3 := (x => x >= 0.0r && x < 3.0r ).difference:(x => x >= 0.0r && x <= 1.0r );

    set3(0.0r).assertFalse();
    set3(1.0r).assertFalse();
    set3(2.0r).assertTrue();
}
```


=={{header|F#|F sharp}}==

```fsharp
open System

let union s1 s2 =
    fun x -> (s1 x) || (s2 x);

let difference s1 s2 =
    fun x -> (s1 x) && not (s2 x)

let intersection s1 s2 =
    fun x -> (s1 x) && (s2 x)

[<EntryPoint>]
let main _ =
    //test set union
    let u1 = union (fun x -> 0.0 < x && x <= 1.0) (fun x -> 0.0 <= x && x < 2.0)
    assert (u1 0.0)
    assert (u1 1.0)
    assert (not (u1 2.0))

    //test set difference
    let d1 = difference (fun x -> 0.0 <= x && x < 3.0) (fun x -> 0.0 < x && x < 1.0)
    assert (d1 0.0)
    assert (not (d1 0.5))
    assert (d1 1.0)
    assert (d1 2.0)

    let d2 = difference (fun x -> 0.0 <= x && x < 3.0) (fun x -> 0.0 <= x && x <= 1.0)
    assert (not (d2 0.0))
    assert (not (d2 1.0))
    assert (d2 2.0)

    let d3 = difference (fun x -> 0.0 <= x && x <= Double.PositiveInfinity) (fun x -> 1.0 <= x && x <= 2.0)
    assert (d3 0.0)
    assert (not (d3 1.5))
    assert (d3 3.0)

    //test set intersection
    let i1 = intersection (fun x -> 0.0 <= x && x < 2.0) (fun x -> 1.0 < x && x <= 2.0)
    assert (not (i1 0.0))
    assert (not (i1 1.0))
    assert (not (i1 2.0))

    0 // return an integer exit code
```



## Go

Just the non-optional part:

```go
package main

import "fmt"

type Set func(float64) bool

func Union(a, b Set) Set      { return func(x float64) bool { return a(x) || b(x) } }
func Inter(a, b Set) Set      { return func(x float64) bool { return a(x) && b(x) } }
func Diff(a, b Set) Set       { return func(x float64) bool { return a(x) && !b(x) } }
func open(a, b float64) Set   { return func(x float64) bool { return a < x && x < b } }
func closed(a, b float64) Set { return func(x float64) bool { return a <= x && x <= b } }
func opCl(a, b float64) Set   { return func(x float64) bool { return a < x && x <= b } }
func clOp(a, b float64) Set   { return func(x float64) bool { return a <= x && x < b } }

func main() {
	s := make([]Set, 4)
	s[0] = Union(opCl(0, 1), clOp(0, 2))  // (0,1] ∪ [0,2)
	s[1] = Inter(clOp(0, 2), opCl(1, 2))  // [0,2) ∩ (1,2]
	s[2] = Diff(clOp(0, 3), open(0, 1))   // [0,3) − (0,1)
	s[3] = Diff(clOp(0, 3), closed(0, 1)) // [0,3) − [0,1]

	for i := range s {
		for x := float64(0); x < 3; x++ {
			fmt.Printf("%v ∈ s%d: %t\n", x, i, s[i](x))
		}
		fmt.Println()
	}
}
```

[http://play.golang.org/p/YQ2GRBM4af Run in Go Playground].
{{out}}

```txt
0 ∈ s0: true
1 ∈ s0: true
2 ∈ s0: false

0 ∈ s1: false
1 ∈ s1: false
2 ∈ s1: false

0 ∈ s2: true
1 ∈ s2: true
2 ∈ s2: true

0 ∈ s3: false
1 ∈ s3: false
2 ∈ s3: true
```

This simple implementation doesn't support lengths so the although the A, B, and A−B sets can be defined and tested (see below), they can't be used to implement the optional part.

```Go
	A := Inter(open(0, 10), func(x float64) bool {
		return math.Abs(math.Sin(math.Pi*x*x)) > .5
	})
	B := Inter(open(0, 10), func(x float64) bool {
		return math.Abs(math.Sin(math.Pi*x)) > .5
	})
	C := Diff(A, B)
	// Can't get lengths, can only test for ∈
	for x := float64(5.98); x < 6.025; x += 0.01 {
		fmt.Printf("%.2f ∈ A−B: %t\n", x, C(x))
	}
```



## Haskell


```haskell

{- Not so functional representation of R sets (with IEEE Double), in a strange way -}

import Data.List
import Data.Maybe

data BracketType = OpenSub | ClosedSub
    deriving (Show, Enum, Eq, Ord)

data RealInterval = RealInterval {left :: BracketType, right :: BracketType,
    lowerBound :: Double, upperBound :: Double}
    deriving (Eq)

type RealSet = [RealInterval]
posInf = 1.0/0.0 :: Double -- IEEE tricks
negInf = (-1.0/0.0) :: Double
set_R = RealInterval ClosedSub ClosedSub negInf posInf :: RealInterval

emptySet = [] :: [RealInterval]

instance Show RealInterval where
    show x@(RealInterval _ _ y y')
        | y == y' && (left x == right x) && (left x == ClosedSub) = "{" ++ (show y) ++ "}"
        | otherwise = [['(', '[']!!(fromEnum $ left x)] ++ (show $ lowerBound x) ++
         "," ++ (show $ upperBound x) ++ [[')', ']']!!(fromEnum $ right x)]
    showList [x] = shows x
    showList (h:t) = shows h . (" U " ++) . showList t
    showList [] =  (++ "(/)") -- empty set

construct_interval :: Char -> Double -> Double -> Char -> RealInterval
construct_interval '(' x y ')' = RealInterval OpenSub OpenSub x y
construct_interval '(' x y ']' = RealInterval OpenSub ClosedSub x y
construct_interval '[' x y ')' = RealInterval ClosedSub OpenSub x y
construct_interval _ x y _ = RealInterval ClosedSub ClosedSub x y

set_is_empty :: RealSet -> Bool
set_is_empty rs = (rs == emptySet)

set_in :: Double -> RealSet -> Bool
set_in x [] = False
set_in x rs =
    isJust (find (\s ->
        ((lowerBound s < x) && (x < upperBound s)) ||
        (x == lowerBound s && left s == ClosedSub) ||
        (x == upperBound s && right s == ClosedSub))
        rs)

-- max, min for pairs (double, bracket)
max_p :: (Double, BracketType) -> (Double, BracketType) -> (Double, BracketType)
min_p :: (Double, BracketType) -> (Double, BracketType) -> (Double, BracketType)
max_p p1@(x, y) p2@(x', y')
    | x == x' = (x, max y y') -- closed is stronger than open
    | x < x'  = p2
    | otherwise = p1

min_p p1@(x, y) p2@(x', y')
    | x == x' = (x, min y y')
    | x < x'  = p1
    | otherwise = p2

simple_intersection :: RealInterval -> RealInterval -> [RealInterval]
simple_intersection ri1@(RealInterval l_ri1 r_ri1 x1 y1) ri2@(RealInterval l_ri2 r_ri2 x2 y2)
    | (y1 < x2) || (y2 < x1) = emptySet
    | (y1 == x2) && ((fromEnum r_ri1) + (fromEnum l_ri2) /= 2) = emptySet
    | (y2 == x1) && ((fromEnum r_ri2) + (fromEnum l_ri1) /= 2) = emptySet
    | otherwise = let lb = if x1 == x2 then (x1, min l_ri1 l_ri2) else max_p (x1, l_ri1) (x2, l_ri2) in
        let rb = min_p (y1, right ri1) (y2, right ri2) in
            [RealInterval (snd lb) (snd rb) (fst lb) (fst rb)]

simple_union :: RealInterval -> RealInterval -> [RealInterval]
simple_union ri1@(RealInterval l_ri1 r_ri1 x1 y1) ri2@(RealInterval l_ri2 r_ri2 x2 y2)
    | (y1 < x2) || (y2 < x1) = [ri2, ri1]
    | (y1 == x2) && ((fromEnum r_ri1) + (fromEnum l_ri2) /= 2) = [ri1, ri2]
    | (y2 == x1) && ((fromEnum r_ri2) + (fromEnum l_ri1) /= 2) = [ri1, ri2]
    | otherwise = let lb = if x1 == x2 then (x1, max l_ri1 l_ri2) else min_p (x1, l_ri1) (x2, l_ri2) in
        let rb = max_p (y1, right ri1) (y2, right ri2) in
            [RealInterval (snd lb) (snd rb) (fst lb) (fst rb)]

simple_complement :: RealInterval -> [RealInterval]
simple_complement ri1@(RealInterval l_ri1 r_ri1 x1 y1) =
    [(RealInterval ClosedSub (inv l_ri1) negInf x1), (RealInterval (inv r_ri1) ClosedSub y1 posInf)]
    where
        inv OpenSub = ClosedSub
        inv ClosedSub = OpenSub

set_sort :: RealSet -> RealSet
set_sort rs =
    sortBy
        (\s1 s2 ->
            let (lp, rp) = ((lowerBound s1, left s1), (lowerBound s2, left s2)) in
                if max_p lp rp == lp then GT else LT)
        rs

set_simplify :: RealSet -> RealSet
set_simplify [] = emptySet
set_simplify rs =
    concat (map make_empty (set_sort (foldl
        (\acc ri1 -> (simple_union (head acc) ri1) ++ (tail acc))
        [head sorted_rs]
        sorted_rs)))
    where
        sorted_rs = set_sort rs
        make_empty ri@(RealInterval lb rb x y)
            | x >= y && (lb /= rb || rb /= ClosedSub) = emptySet
            | otherwise = [ri]

-- set operations
set_complement :: RealSet -> RealSet
set_union :: RealSet -> RealSet -> RealSet
set_intersection :: RealSet -> RealSet -> RealSet
set_difference :: RealSet -> RealSet -> RealSet
set_measure :: RealSet -> Double

set_complement rs =
    foldl set_intersection [set_R] (map simple_complement rs)
set_union rs1 rs2 =
    set_simplify (rs1 ++ rs2)
set_intersection rs1 rs2 =
    set_simplify $ concat [simple_intersection s1 s2 | s1 <- rs1, s2 <- rs2]
set_difference rs1 rs2 =
    set_intersection (set_complement rs2) rs1
set_measure rs =
    foldl (\acc x -> acc + (upperBound x) - (lowerBound x)) 0.0 rs

-- test
test = map (\x -> [x]) [construct_interval '(' 0 1 ']', construct_interval '[' 0 2 ')',
    construct_interval '[' 0 2 ')', construct_interval '(' 1 2 ']',
    construct_interval '[' 0 3 ')', construct_interval '(' 0 1 ')',
    construct_interval '[' 0 3 ')', construct_interval '[' 0 1 ']']
restest = [set_union (test!!0) (test!!1), set_intersection (test!!2) (test!!3),
    set_difference (test!!4) (test!!5), set_difference (test!!6) (test!!7)]
isintest s =
    mapM_
        (\x -> putStrLn ((show x) ++ " is in " ++ (show s) ++ " : " ++ (show (set_in x s))))
        [0, 1, 2]

testA = [construct_interval '(' (sqrt (n + (1.0/6))) (sqrt (n + (5.0/6))) ')' | n <- [0..99]]
testB = [construct_interval '(' (n + (1.0/6)) (n + (5.0/6)) ')' | n <- [0..9]]

main =
    putStrLn ("union " ++ (show (test!!0)) ++ " " ++ (show (test!!1)) ++ " = " ++ (show (restest!!0))) >>
    putStrLn ("inter " ++ (show (test!!2)) ++ " " ++ (show (test!!3)) ++ " = " ++ (show (restest!!1))) >>
    putStrLn ("diff " ++ (show (test!!4)) ++ " " ++ (show (test!!5)) ++ " = " ++ (show (restest!!2))) >>
    putStrLn ("diff " ++ (show (test!!6)) ++ " " ++ (show (test!!7)) ++ " = " ++ (show (restest!!3))) >>
    mapM_ isintest restest >>
    putStrLn ("measure: " ++ (show (set_measure (set_difference testA testB))))

```

{{out}}

```txt

union (0.0,1.0] [0.0,2.0) = [0.0,2.0)
inter [0.0,2.0) (1.0,2.0] = (1.0,2.0)
diff [0.0,3.0) (0.0,1.0) = {0.0} U [1.0,3.0)
diff [0.0,3.0) [0.0,1.0] = (1.0,3.0)
0.0 is in [0.0,2.0) : True
1.0 is in [0.0,2.0) : True
2.0 is in [0.0,2.0) : False
0.0 is in (1.0,2.0) : False
1.0 is in (1.0,2.0) : False
2.0 is in (1.0,2.0) : False
0.0 is in {0.0} U [1.0,3.0) : True
1.0 is in {0.0} U [1.0,3.0) : True
2.0 is in {0.0} U [1.0,3.0) : True
0.0 is in (1.0,3.0) : False
1.0 is in (1.0,3.0) : False
2.0 is in (1.0,3.0) : True
measure: 2.0758648411846696

```


==Icon and {{header|Unicon}}==

The following only works in Unicon.  The code does a few crude
simplifications of some representations, but more could be done.


```unicon
procedure main(A)
    s1 := RealSet("(0,1]").union(RealSet("[0,2)"))
    s2 := RealSet("[0,2)").intersect(RealSet("(1,2)"))
    s3 := RealSet("[0,3)").difference(RealSet("(0,1)"))
    s4 := RealSet("[0,3)").difference(RealSet("[0,1]"))
    every s := s1|s2|s3|s4 do {
        every n := 0 to 2 do
            write(s.toString(),if s.contains(n) then " contains "
                                                else " doesn't contain ",n)
        write()
        }
end

class Range(a,b,lbnd,rbnd,ltest,rtest)

    method contains(x); return ((ltest(a,x),rtest(x,b)),self); end
    method toString(); return lbnd||a||","||b||rbnd; end
    method notEmpty(); return (ltest(a,b),rtest(a,b),self); end
    method makeLTest(); return proc(if lbnd == "(" then "<" else "<=",2); end
    method makeRTest(); return proc(if rbnd == "(" then "<" else "<=",2); end

    method intersect(r)
        if a < r.a then (na := r.a, nlb := r.lbnd)
        else if a > r.a then (na := a, nlb := lbnd)
        else (na := a, nlb := if "(" == (lbnd|r.lbnd) then "(" else "[")
        if b < r.b then ( nb := b, nrb := rbnd)
        else if b > r.b then (nb := r.b, nrb := r.rbnd)
        else (nb := b, nrb := if ")" == (rbnd|r.rbnd) then ")" else "]")
        range := Range(nlb||na||","||nb||nrb)
        return range
    end

    method difference(r)
        if /r then return RealSet(toString())
        r1 := lbnd||a||","||min(b,r.a)||map(r.lbnd,"([","])")
        r2 := map(r.rbnd,")]","[(")||max(a,r.b)||","||b||rbnd
        return RealSet(r1).union(RealSet(r2))
    end

initially(s)
    static lbnds, rbnds
    initial (lbnds := '([', rbnds := '])')
    if \s then {
        s ? {
            lbnd := (tab(upto(lbnds)),move(1))
            a := 1(tab(upto(',')),move(1))
            b := tab(upto(rbnds))
            rbnd := move(1)
            }
        ltest := proc(if lbnd == "(" then "<" else "<=",2)
        rtest := proc(if rbnd == ")" then "<" else "<=",2)
        }
end

class RealSet(ranges)

    method contains(x); return ((!ranges).contains(x), self); end
    method notEmpty(); return ((!ranges).notEmpty(), self); end

    method toString()
        sep := s := ""
        every r := (!ranges).toString() do s ||:= .sep || 1(r, sep := " + ")
        return s
    end

    method clone()
        newR := RealSet()
        newR.ranges := (copy(\ranges) | [])
        return newR
    end

    method union(B)
        newR := clone()
        every put(newR.ranges, (!B.ranges).notEmpty())
        return newR
    end

    method intersect(B)
        newR := clone()
        newR.ranges := []
        every (r1 := !ranges, r2 := !B.ranges) do {
            range := r1.intersect(r2)
            put(newR.ranges, range.notEmpty())
            }
        return newR
    end

    method difference(B)
        newR := clone()
        newR.ranges := []
        every (r1 := !ranges, r2 := !B.ranges) do {
            rs := r1.difference(r2)
            if rs.notEmpty() then every put(newR.ranges, !rs.ranges)
            }
        return newR
    end

initially(s)
    put(ranges := [],Range(\s).notEmpty())
end
```


Sample run:

```txt

->srn
(0,1] + [0,2) contains 0
(0,1] + [0,2) contains 1
(0,1] + [0,2) doesn't contain 2

(1,2) doesn't contain 0
(1,2) doesn't contain 1
(1,2) doesn't contain 2

[0,0] + [1,3) contains 0
[0,0] + [1,3) contains 1
[0,0] + [1,3) contains 2

(1,3) doesn't contain 0
(1,3) doesn't contain 1
(1,3) contains 2

->

```



## J


In essence, this looks like building a restricted set of statements.  So we build a specialized parser and expression builder:


```j
has=: 1 :'(interval m)`:6'
ing=: `''

interval=: 3 :0
  if.0<L.y do.y return.end.
  assert. 5=#words=. ;:y
  assert. (0 { words) e. ;:'[('
  assert. (2 { words) e. ;:','
  assert. (4 { words) e. ;:'])'
  'lo hi'=.(1 3{0".L:0 words)
  'cL cH'=.0 4{words e.;:'[]'
  (lo&(<`<:@.cL) *. hi&(>`>:@.cH))ing
)

union=: 4 :'(x has +. y has)ing'
intersect=: 4 :'(x has *. y has)ing'
without=: 4 :'(x has *. [: -. y has)ing'
```


With this in place, the required examples look like this:


```j
   ('(0,1]' union '[0,2)')has 0 1 2
1 1 0
   ('[0,2)' intersect '(1,2]')has 0 1 2
0 0 0
   ('[0,3)' without '(0,1]')has 0 1 2
1 0 1
   ('[0,3)' without '(0,1)')has 0 1 2
1 1 1
   ('[0,3)' without '[0,1]')has 0 1 2
0 0 1
```


Note that without the arguments these wind up being expressions.  For example:


```j
   ('(0,1]' union '[0,2)')has
(0&< *. 1&>:) +. 0&<: *. 2&>
```


In other words, this is a statement built up from inequality terminals (where each inequality is bound to a constant) and the terminals are combined with logical operations.


###  Optional Work



### = Empty Set Detection =


Here is an alternate formulation which allows detection of empty sets:


```j
has=: 1 :'(0 {:: interval m)`:6'
ing=: `''

edge=: 1&{::&interval
edges=: /:~@~.@,&edge
contour=: (, 2 (+/%#)\ ])@edge

interval=: 3 :0
  if.0<L.y do.y return.end.
  assert. 5=#words=. ;:y
  assert. (0 { words) e. ;:'[('
  assert. (2 { words) e. ;:','
  assert. (4 { words) e. ;:'])'
  'lo hi'=.(1 3{0".L:0 words)
  'cL cH'=.0 4{words e.;:'[]'
  (lo&(<`<:@.cL) *. hi&(>`>:@.cH))ing ; lo,hi
)

union=: 4 :'(x has +. y has)ing; x edges y'
intersect=: 4 :'(x has *. y has)ing; x edges y'
without=: 4 :'(x has *. [: -. y has)ing; x edges y'
in=: 4 :'y has x'
isEmpty=: 1 -.@e. contour in ]
```


The above examples work identically with this version, but also:


```j
   isEmpty '(0,1]' union '[0,2)'
0
   isEmpty '[0,2)' intersect '(1,2]'
0
   isEmpty '[0,2)' intersect '(2,3]'
1
   isEmpty '[0,2)' intersect '[2,3]'
1
   isEmpty '[0,2]' intersect '[2,3]'
0
```


Note that the the set operations no longer return a simple verb -- instead, they return a pair, where the first element represents the verb and the second element is a list of interval boundaries.  We can tell if two adjacent bounds, from this list, bound a valid interval by checking any point between them.


### = Length of Set Difference =


The optional work centers around expressions where the absolute value of sin pi * n is 0.5.  It would be nice if J had an arcsine which gave all values within a range, but it does not have that.  So:


```j
   1p_1 * _1 o. 0.5
0.166667
```


(Note on notation:  1 o. is sine in J, and 2 o. is cosine -- the mnemonic is that sine is an odd function and cosine is an even function, the practical value is that sine, cosine and sine/cosine pairs can all be generated from the same "real" valued function.
Similarly, _1 o. is arcsine and _2 o. is arcsine.  Also 1p_1 is the reciprocal of pi.  So the above tells us that the principal value for arc sine 0.5 is one sixth.)


```j
   (#~ 0.5 = 1 |@o. 1r6p1&*) i. 30
1 5 7 11 13 17 19 23 25 29
   2 -~/\ (#~ 0.5 = 1 |@o. 1r6p1&*) i. 30
4 2 4 2 4 2 4 2 4
```


Here we see the integers which when multiplied by pi/6 give 0.5 for the absolute value of the sine, and their first difference.  Thus:


```j
zeros0toN=: ((>: # ])[:+/\1,$&4 2@<.)&.(6&*)
```


is a function to generate the values which correspond to the boundaries of the intervals we want:


```j
zB=: zeros0toN 10
zA=: zeros0toN&.*: 10

   zA
0.408248 0.912871 1.08012 1.35401 1.47196 1.68325 1.77951 1.95789 2.04124 2.1984...
   zB
0.166667 0.833333 1.16667 1.83333 2.16667 2.83333 3.16667 3.83333 4.16667 4.8333...
   #zA
200
   #zB
20
```


And, here are the edges of the sets of intervals we need to consider.

To find the length of the the set A-B we can find the length of set A and subtract the length of the set A-B:


```j
   (+/_2 -~/\zA) - +/,0>.zA (<.&{: - >.&{.)"1/&(_2 ]\ ]) zB
2.07586
```


Here, we have paired adjacent elements from the zero bounding list (non-overlapping infixes of length 2).  For set A's length we sum the results of subtracting the smaller number of the pair from the larger.  For set A-B's length we consider each combination of pairs from A and B and subtract the larger of the beginning values from the smaller of the ending values (and ignore any negative results).

Alternatively, if we use the set implementation with empty set detection, and the following definitions:


```j
intervalSet=: interval@('[',[,',',],')'"_)&":
A=: union/_2 intervalSet/\ zA
B=: union/_2 intervalSet/\ zB
diff=: A without B
```


We can replace the above sentence to compute the length of the difference with:


```j
   +/ ((2 (+/%#)\ edge diff) in diff) * 2 -~/\ edge diff
2.07588
```


(Note that this result is not exactly the same as the previous result.  Determining why would be an interesting exercise in numerical analysis.)


## Java


```java
import java.util.Objects;
import java.util.function.Predicate;

public class RealNumberSet {
    public enum RangeType {
        CLOSED,
        BOTH_OPEN,
        LEFT_OPEN,
        RIGHT_OPEN,
    }

    public static class RealSet {
        private Double low;
        private Double high;
        private Predicate<Double> predicate;
        private double interval = 0.00001;

        public RealSet(Double low, Double high, Predicate<Double> predicate) {
            this.low = low;
            this.high = high;
            this.predicate = predicate;
        }

        public RealSet(Double start, Double end, RangeType rangeType) {
            this(start, end, d -> {
                switch (rangeType) {
                    case CLOSED:
                        return start <= d && d <= end;
                    case BOTH_OPEN:
                        return start < d && d < end;
                    case LEFT_OPEN:
                        return start < d && d <= end;
                    case RIGHT_OPEN:
                        return start <= d && d < end;
                    default:
                        throw new IllegalStateException("Unhandled range type encountered.");
                }
            });
        }

        public boolean contains(Double d) {
            return predicate.test(d);
        }

        public RealSet union(RealSet other) {
            double low2 = Math.min(low, other.low);
            double high2 = Math.max(high, other.high);
            return new RealSet(low2, high2, d -> predicate.or(other.predicate).test(d));
        }

        public RealSet intersect(RealSet other) {
            double low2 = Math.min(low, other.low);
            double high2 = Math.max(high, other.high);
            return new RealSet(low2, high2, d -> predicate.and(other.predicate).test(d));
        }

        public RealSet subtract(RealSet other) {
            return new RealSet(low, high, d -> predicate.and(other.predicate.negate()).test(d));
        }

        public double length() {
            if (low.isInfinite() || high.isInfinite()) return -1.0; // error value
            if (high <= low) return 0.0;
            Double p = low;
            int count = 0;
            do {
                if (predicate.test(p)) count++;
                p += interval;
            } while (p < high);
            return count * interval;
        }

        public boolean isEmpty() {
            if (Objects.equals(high, low)) {
                return predicate.negate().test(low);
            }
            return length() == 0.0;
        }
    }

    public static void main(String[] args) {
        RealSet a = new RealSet(0.0, 1.0, RangeType.LEFT_OPEN);
        RealSet b = new RealSet(0.0, 2.0, RangeType.RIGHT_OPEN);
        RealSet c = new RealSet(1.0, 2.0, RangeType.LEFT_OPEN);
        RealSet d = new RealSet(0.0, 3.0, RangeType.RIGHT_OPEN);
        RealSet e = new RealSet(0.0, 1.0, RangeType.BOTH_OPEN);
        RealSet f = new RealSet(0.0, 1.0, RangeType.CLOSED);
        RealSet g = new RealSet(0.0, 0.0, RangeType.CLOSED);

        for (int i = 0; i <= 2; i++) {
            Double dd = (double) i;
            System.out.printf("(0, 1] ∪ [0, 2) contains %d is %s\n", i, a.union(b).contains(dd));
            System.out.printf("[0, 2) ∩ (1, 2] contains %d is %s\n", i, b.intersect(c).contains(dd));
            System.out.printf("[0, 3) − (0, 1) contains %d is %s\n", i, d.subtract(e).contains(dd));
            System.out.printf("[0, 3) − [0, 1] contains %d is %s\n", i, d.subtract(f).contains(dd));
            System.out.println();
        }

        System.out.printf("[0, 0] is empty is %s\n", g.isEmpty());
        System.out.println();

        RealSet aa = new RealSet(
            0.0, 10.0,
            x -> (0.0 < x && x < 10.0) && Math.abs(Math.sin(Math.PI * x * x)) > 0.5
        );
        RealSet bb = new RealSet(
            0.0, 10.0,
            x -> (0.0 < x && x < 10.0) && Math.abs(Math.sin(Math.PI * x)) > 0.5
        );
        RealSet cc = aa.subtract(bb);
        System.out.printf("Approx length of A - B is %f\n", cc.length());
    }
}
```

{{out}}

```txt
(0, 1] ∪ [0, 2) contains 0 is true
[0, 2) ∩ (1, 2] contains 0 is false
[0, 3) − (0, 1) contains 0 is true
[0, 3) − [0, 1] contains 0 is false

(0, 1] ∪ [0, 2) contains 1 is true
[0, 2) ∩ (1, 2] contains 1 is false
[0, 3) − (0, 1) contains 1 is true
[0, 3) − [0, 1] contains 1 is false

(0, 1] ∪ [0, 2) contains 2 is false
[0, 2) ∩ (1, 2] contains 2 is false
[0, 3) − (0, 1) contains 2 is true
[0, 3) − [0, 1] contains 2 is true

[0, 0] is empty is false

Approx length of A - B is 2.075870
```



## Julia


```Julia

"""
    struct ConvexRealSet
Convex real set (similar to a line segment).
Parameters: lower bound, upper bound: floating point numbers
            includelower, includeupper: boolean true or false to indicate whether
            the set has a closed boundary (set to true) or open (set to false).
"""
mutable struct ConvexRealSet
    lower::Float64
    includelower::Bool
    upper::Float64
    includeupper::Bool
    function ConvexRealSet(lo, up, incllo, inclup)
       this = new()
       this.upper = Float64(up)
       this.lower = Float64(lo)
       this.includelower = incllo
       this.includeupper = inclup
       this
    end
end


function ∈(s, xelem)
    x = Float64(xelem)
    if(x == s.lower)
        if(s.includelower)
            return true
        else
            return false
        end
    elseif(x == s.upper)
        if(s.includeupper)
            return true
        else
            return false
        end
    end
    s.lower < x && x < s.upper
end


⋃(aset, bset, x) = (∈(aset, x) || ∈(bset, x))

⋂(aset, bset, x) = (∈(aset, x) && ∈(bset, x))

-(aset, bset, x) = (∈(aset, x) && !∈(bset, x))

isempty(s::ConvexRealSet) = (s.lower > s.upper) ||
                           ((s.lower == s.upper) && !s.includeupper && !s.includelower)


const s1 = ConvexRealSet(0.0, 1.0, false, true)
const s2 = ConvexRealSet(0.0, 2.0, true, false)
const s3 = ConvexRealSet(1.0, 2.0, false, true)
const s4 = ConvexRealSet(0.0, 3.0, true, false)
const s5 = ConvexRealSet(0.0, 1.0, false, false)
const s6 = ConvexRealSet(0.0, 1.0, true, true)
const sempty = ConvexRealSet(0.0, -1.0, true, true)
const testlist = [0, 1, 2]


function testconvexrealset()
    for i in testlist
        println("Testing with x = $i.\nResults:")
        println("    (0, 1] ∪ [0, 2): $(⋃(s1, s2, i))")
        println("    [0, 2) ∩ (1, 2]: $(⋂(s2, s3, i))")
        println("    [0, 3) − (0, 1): $(-(s4, s5, i))")
        println("    [0, 3) − [0, 1]: $(-(s4, s6, i))\n")
    end
    print("The set sempty is ")
    println(isempty(sempty) ? "empty." : "not empty.")
end


testconvexrealset()

```

{{output}}
```txt

Testing with x = 0.
Results:
    (0, 1] ∪ [0, 2): true
    [0, 2) ∩ (1, 2]: false
    [0, 3) − (0, 1): true
    [0, 3) − [0, 1]: false

Testing with x = 1.
Results:
    (0, 1] ∪ [0, 2): true
    [0, 2) ∩ (1, 2]: false
    [0, 3) − (0, 1): true
    [0, 3) − [0, 1]: false

Testing with x = 2.
Results:
    (0, 1] ∪ [0, 2): false
    [0, 2) ∩ (1, 2]: false
    [0, 3) − (0, 1): true
    [0, 3) − [0, 1]: true

The set sempty is empty.

```



## Kotlin

The RealSet class has two constructors - a primary one which creates an object for an arbitrary predicate and a secondary one which creates an object for a simple range by generating the appropriate predicate and then invoking the primary one.

As far as the optional work is concerned, I decided to add a length property which gives only an approximate result. Basically, it works by keeping track of the low and high values of the set and then counting points at successive small intervals between these limits which satisfy the predicate. An isEmpty() function has also been added but as this depends, to some extent, on the length property it is not 100% reliable.

Clearly, the above approach is only suitable for sets with narrow ranges (as we have here) but does have the merit of not over-complicating the basic class.

```scala
// version 1.1.4-3

typealias RealPredicate = (Double) -> Boolean

enum class RangeType { CLOSED, BOTH_OPEN, LEFT_OPEN, RIGHT_OPEN }

class RealSet(val low: Double, val high: Double, val predicate: RealPredicate) {

    constructor (start: Double, end: Double, rangeType: RangeType): this(start, end,
        when (rangeType) {
            RangeType.CLOSED     -> fun(d: Double) = d in start..end
            RangeType.BOTH_OPEN  -> fun(d: Double) = start < d && d < end
            RangeType.LEFT_OPEN  -> fun(d: Double) = start < d && d <= end
            RangeType.RIGHT_OPEN -> fun(d: Double) = start <= d && d < end
        }
    )

    fun contains(d: Double) = predicate(d)

    infix fun union(other: RealSet): RealSet {
        val low2 = minOf(low, other.low)
        val high2 = maxOf(high, other.high)
        return RealSet(low2, high2) { predicate(it) || other.predicate(it) }
    }

    infix fun intersect(other: RealSet): RealSet {
        val low2 = maxOf(low, other.low)
        val high2 = minOf(high, other.high)
        return RealSet(low2, high2) { predicate(it) && other.predicate(it) }
    }

    infix fun subtract(other: RealSet) = RealSet(low, high) { predicate(it) && !other.predicate(it) }

    var interval = 0.00001

    val length: Double get() {
        if (!low.isFinite() || !high.isFinite()) return -1.0  // error value
        if (high <= low) return 0.0
        var p = low
        var count = 0
        do {
            if (predicate(p)) count++
            p += interval
        }
        while (p < high)
        return count * interval
    }

    fun isEmpty() = if (high == low) !predicate(low) else length == 0.0
}

fun main(args: Array<String>) {
    val a = RealSet(0.0, 1.0, RangeType.LEFT_OPEN)
    val b = RealSet(0.0, 2.0, RangeType.RIGHT_OPEN)
    val c = RealSet(1.0, 2.0, RangeType.LEFT_OPEN)
    val d = RealSet(0.0, 3.0, RangeType.RIGHT_OPEN)
    val e = RealSet(0.0, 1.0, RangeType.BOTH_OPEN)
    val f = RealSet(0.0, 1.0, RangeType.CLOSED)
    val g = RealSet(0.0, 0.0, RangeType.CLOSED)

    for (i in 0..2) {
        val dd = i.toDouble()
        println("(0, 1] ∪ [0, 2) contains $i is ${(a union b).contains(dd)}")
        println("[0, 2) ∩ (1, 2] contains $i is ${(b intersect c).contains(dd)}")
        println("[0, 3) − (0, 1) contains $i is ${(d subtract e).contains(dd)}")
        println("[0, 3) − [0, 1] contains $i is ${(d subtract f).contains(dd)}\n")
    }

    println("[0, 0] is empty is ${g.isEmpty()}\n")

    val aa = RealSet(0.0, 10.0) { x -> (0.0 < x && x < 10.0) &&
                                        Math.abs(Math.sin(Math.PI * x * x)) > 0.5  }
    val bb = RealSet(0.0, 10.0) { x -> (0.0 < x && x < 10.0) &&
                                        Math.abs(Math.sin(Math.PI * x)) > 0.5  }
    val cc = aa subtract bb
    println("Approx length of A - B is ${cc.length}")
}
```


{{out}}

```txt

(0, 1] ∪ [0, 2) contains 0 is true
[0, 2) ∩ (1, 2] contains 0 is false
[0, 3) − (0, 1) contains 0 is true
[0, 3) − [0, 1] contains 0 is false

(0, 1] ∪ [0, 2) contains 1 is true
[0, 2) ∩ (1, 2] contains 1 is false
[0, 3) − (0, 1) contains 1 is true
[0, 3) − [0, 1] contains 1 is false

(0, 1] ∪ [0, 2) contains 2 is false
[0, 2) ∩ (1, 2] contains 2 is false
[0, 3) − (0, 1) contains 2 is true
[0, 3) − [0, 1] contains 2 is true

[0, 0] is empty is false

Approx length of A - B is 2.07587

```



## Lua


```lua
function createSet(low,high,rt)
    local l,h = tonumber(low), tonumber(high)
    if l and h then
        local t = {low=l, high=h}

        if type(rt) == "string" then
            if rt == "open" then
                t.contains = function(d) return low< d and d< high end
            elseif rt == "closed" then
                t.contains = function(d) return low<=d and d<=high end
            elseif rt == "left" then
                t.contains = function(d) return low< d and d<=high end
            elseif rt == "right" then
                t.contains = function(d) return low<=d and d< high end
            else
                error("Unknown range type: "..rt)
            end
        elseif type(rt) == "function" then
            t.contains = rt
        else
            error("Unable to find a range type or predicate")
        end

        t.union = function(o)
            local l2 = math.min(l, o.low)
            local h2 = math.min(h, o.high)
            local p = function(d) return t.contains(d) or o.contains(d) end
            return createSet(l2, h2, p)
        end

        t.intersect = function(o)
            local l2 = math.min(l, o.low)
            local h2 = math.min(h, o.high)
            local p = function(d) return t.contains(d) and o.contains(d) end
            return createSet(l2, h2, p)
        end

        t.subtract = function(o)
            local l2 = math.min(l, o.low)
            local h2 = math.min(h, o.high)
            local p = function(d) return t.contains(d) and not o.contains(d) end
            return createSet(l2, h2, p)
        end

        t.length = function()
            if h <= l then return 0.0 end
            local p = l
            local count = 0
            local interval = 0.00001
            repeat
                if t.contains(p) then count = count + 1 end
                p = p + interval
            until p>=high
            return count * interval
        end

        t.empty = function()
            if l == h then
                return not t.contains(low)
            end
            return t.length() == 0.0
        end

        return t
    else
        error("Either '"..low.."' or '"..high.."' is not a number")
    end
end

local a = createSet(0.0, 1.0, "left")
local b = createSet(0.0, 2.0, "right")
local c = createSet(1.0, 2.0, "left")
local d = createSet(0.0, 3.0, "right")
local e = createSet(0.0, 1.0, "open")
local f = createSet(0.0, 1.0, "closed")
local g = createSet(0.0, 0.0, "closed")

for i=0,2 do
    print("(0, 1]   union   [0, 2) contains "..i.." is "..tostring(a.union(b).contains(i)))
    print("[0, 2) intersect (1, 2] contains "..i.." is "..tostring(b.intersect(c).contains(i)))
    print("[0, 3)     -     (0, 1) contains "..i.." is "..tostring(d.subtract(e).contains(i)))
    print("[0, 3)     -     [0, 1] contains "..i.." is "..tostring(d.subtract(f).contains(i)))
    print()
end

print("[0, 0] is empty is "..tostring(g.empty()))
print()

local aa = createSet(
    0.0, 10.0,
    function(x) return (0.0<x and x<10.0) and math.abs(math.sin(math.pi * x * x)) > 0.5 end
)
local bb = createSet(
    0.0, 10.0,
    function(x) return (0.0<x and x<10.0) and math.abs(math.sin(math.pi * x)) > 0.5 end
)
local cc = aa.subtract(bb)
print("Approx length of A - B is "..cc.length())
```

{{out}}

```txt
(0, 1]   union   [0, 2) contains 0 is true
[0, 2) intersect (1, 2] contains 0 is false
[0, 3)     -     (0, 1) contains 0 is true
[0, 3)     -     [0, 1] contains 0 is false

(0, 1]   union   [0, 2) contains 1 is true
[0, 2) intersect (1, 2] contains 1 is false
[0, 3)     -     (0, 1) contains 1 is true
[0, 3)     -     [0, 1] contains 1 is false

(0, 1]   union   [0, 2) contains 2 is false
[0, 2) intersect (1, 2] contains 2 is false
[0, 3)     -     (0, 1) contains 2 is true
[0, 3)     -     [0, 1] contains 2 is true

[0, 0] is empty is false

Approx length of A - B is 2.07587
```



## Mathematica


```Mathematica
(* defining functions *)
setcc[a_, b_] := a <= x <= b
setoo[a_, b_] := a < x < b
setco[a_, b_] := a <= x < b
setoc[a_, b_] := a < x <= b
setSubtract[s1_, s2_] := s1  &&  Not[s2];  (* new function; subtraction not built in *)
inSetQ[y_, set_] := set /. x -> y
(* testing sets *)
set1 = setoc[0, 1]  || setco[0, 2]  (* union built in as || shortcut (OR) *);
Print[set1]
Print["First trial set, (0, 1] ∪ [0, 2) , testing for {0,1,2}:"]
Print[inSetQ[#, set1] & /@ {0, 1, 2}]
set2 = setco[0, 2] && setoc[1, 2];  (* intersection built in as && shortcut (AND) *)
Print[]
Print[set2]
Print["Second trial set, [0, 2) ∩ (1, 2], testing for {0,1,2}:"]
Print[inSetQ[#, set2] & /@ {0, 1, 2}]
Print[]
set3 = setSubtract[setco[0, 3], setoo[0, 1]];
Print[set3]
Print["Third trial set, [0, 3) \[Minus] (0, 1), testing for {0,1,2}"]
Print[inSetQ[#, set3] & /@ {0, 1, 2}]
Print[]
set4 = setSubtract[setco[0, 3], setcc[0, 1]];
Print[set4]
Print["Fourth trial set, [0,3)\[Minus][0,1], testing for {0,1,2}:"]
Print[inSetQ[#, set4] & /@ {0, 1, 2}]
```

{{Output}}

```txt
0<x<=1||0<=x<2
First trial set, (0, 1] ∪ [0, 2) , testing for {0,1,2}:
{True,True,False}

0<=x<2&&1<x<=2
Second trial set, [0, 2) ∩ (1, 2], testing for {0,1,2}:
{False,False,False}

0<=x<3&&!0<x<1
Third trial set, [0, 3) \[Minus] (0, 1), testing for {0,1,2}
{True,True,True}

0<=x<3&&!0<=x<=1
Fourth trial set, [0,3)\[Minus][0,1], testing for {0,1,2}:
{False,False,True}
```



## PARI/GP

Define some sets and use built-in functions:

```parigp
set11(x,a,b)=select(x -> a <= x && x <= b, x);
set01(x,a,b)=select(x -> a <  x && x <= b, x);
set10(x,a,b)=select(x -> a <= x && x <  b, x);
set00(x,a,b)=select(x -> a <  x && x <  b, x);

V = [0, 1, 2];

    setunion(set01(V, 0, 1), set10(V, 0, 2))
setintersect(set10(V, 0, 2), set01(V, 1, 2))
    setminus(set10(V, 0, 3), set00(V, 0, 1))
    setminus(set10(V, 0, 3), set11(V, 0, 1))
```


Output:
```txt

[0, 1]
[]
[0, 1, 2]
[2]
```



## Perl


```perl
use utf8;

# numbers used as boundaries to real sets.  Each has 3 components:
#	the real value x;
#	a +/-1 indicating if it's x + ϵ or x - ϵ
#	a 0/1 indicating if it's the left border or right border
# e.g. "[1.5, ..." is written "1.5, -1, 0", while "..., 2)" is "2, -1, 1"
package BNum;

use overload (
	'""'	=> \&_str,
	'<=>'	=> \&_cmp,
);

sub new {
	my $self = shift;
	bless [@_], ref $self || $self
}

sub flip {
	my @a = @{+shift};
	$a[2] = !$a[2];
	bless \@a
}

my $brackets = qw/ [ ( ) ] /;
sub _str {
	my $v = sprintf "%.2f", $_[0][0];
	$_[0][2]
		? $v . ($_[0][1] == 1 ? "]" : ")")
		: ($_[0][1] == 1 ? "(" : "[" ) . $v;
}

sub _cmp {
	my ($a, $b, $swap) = @_;

	# if one of the argument is a normal number
	if ($swap) { return -_ncmp($a, $b) }
	if (!ref $b || !$b->isa(__PACKAGE__)) { return _ncmp($a, $b) }

	$a->[0] <=> $b->[0] || $a->[1] <=> $b->[1]
}

sub _ncmp {
	# $a is a BNum, $b is something comparable to a real
	my ($a, $b) = @_;
	$a->[0] <=> $b || $a->[1] <=> 0
}

package RealSet;
use Carp;
use overload (
	'""'	=> \&_str,
	'|'	=> \&_or,
	'&'	=> \&_and,
	'~'	=> \&_neg,
	'-'	=> \&_diff,
	'bool'	=> \&not_empty, # set is true if not empty
);

my %pm = qw/ [ -1 ( 1 ) -1 ] 1 /;
sub range {
	my ($cls, $a, $b, $spec) = @_;
	$spec =~ /^( \[ | \( )( \) | \] )$/x	or croak "bad spec $spec";

	$a = BNum->new($a, $pm{$1}, 0);
	$b = BNum->new($b, $pm{$2}, 1);
	normalize($a < $b ? [$a, $b] : [])
}

sub normalize {
	my @a = @{+shift};
	# remove invalid or duplicate borders, such as "[2, 1]" or "3) [3"
	# note that "(a" == "a]" and "a)" == "[a", but "a)" < "(a" and
	# "[a" < "a]"
	for (my $i = $#a; $i > 0; $i --) {
		splice @a, $i - 1, 2
			if $a[$i] <= $a[$i - 1]
	}
	bless \@a
}

sub not_empty { scalar @{ normalize shift } }

sub _str {
	my (@a, @s) = @{+shift}		or return '()';
	join " ∪ ", map { shift(@a).", ".shift(@a) } 0 .. $#a/2
}

sub _or {
	# we may have nested ranges now; let only outmost ones survive
	my $d = 0;
	normalize [
		map {	$_->[2] ? --$d ? () : ($_)
				: $d++ ? () : ($_) }
		sort{ $a <=> $b } @{+shift}, @{+shift}
	];
}

sub _neg {
	normalize [
		BNum->new('-inf', 1, 0),
		map($_->flip, @{+shift}),
		BNum->new('inf', -1, 1),
	]
}

sub _and {
	my $d = 0;
	normalize [
		map {	$_->[2] ? --$d ? ($_) : ()
				: $d++ ? ($_) : () }
		sort{ $a <=> $b } @{+shift}, @{+shift}
	];
}

sub _diff { shift() & ~shift() }

sub has {
	my ($a, $b) = @_;
	for (my $i = 0; $i < $#$a; $i += 2) {
		return 1 if $a->[$i] <= $b && $b <= $a->[$i + 1]
	}
	return 0
}

sub len {
	my ($a, $l) = shift;
	for (my $i = 0; $i < $#$a; $i += 2) {
		$l += $a->[$i+1][0] - $a->[$i][0]
	}
	return $l
}

package main;
use List::Util 'reduce';

sub rng { RealSet->range(@_) }
my @sets = (
	rng(0, 1, '(]') | rng(0, 2, '[)'),
	rng(0, 2, '[)') & rng(0, 2, '(]'),
	rng(0, 3, '[)') - rng(0, 1, '()'),
	rng(0, 3, '[)') - rng(0, 1, '[]'),
);

for my $i (0 .. $#sets) {
	print "Set $i = ", $sets[$i], ": ";
	for (0 .. 2) {
		print "has $_; " if $sets[$i]->has($_);
	}
	print "\n";
}

# optional task
print "\n####\n";
sub brev { # show only head and tail if string too long
	my $x = shift;
	return $x if length $x < 60;
	substr($x, 0, 30)." ... ".substr($x, -30, 30)
}

# "|sin(x)| > 1/2" means (n + 1/6) pi < x < (n + 5/6) pi
my $x = reduce { $a | $b }
	map(rng(sqrt($_ + 1./6), sqrt($_ + 5./6), '()'), 0 .. 101);
$x &= rng(0, 10, '()');

print "A\t", '= {x | 0 < x < 10 and |sin(π x²)| > 1/2 }',
	"\n\t= ", brev($x), "\n";

my $y = reduce { $a | $b }
	map { rng($_ + 1./6, $_ + 5./6, '()') } 0 .. 11;
$y &= rng(0, 10, '()');

print "B\t", '= {x | 0 < x < 10 and |sin(π x)| > 1/2 }',
	"\n\t= ", brev($y), "\n";

my $z = $x - $y;
print "A - B\t= ", brev($z), "\n\tlength = ", $z->len, "\n";
print $z ? "not empty\n" : "empty\n";
```
output<lang>Set 0 = [0.00, 2.00): has 0; has 1;
Set 1 = (0.00, 2.00): has 1;
Set 2 = [0.00, 0.00] ∪ [1.00, 3.00): has 0; has 1; has 2;
Set 3 = (1.00, 3.00): has 2;

####
A       = {x | 0 < x < 10 and |sin(π x²)| > 1/2 }
        = (0.41, 0.91) ∪ (1.08, 1.35) ∪  ...  ∪ (9.91, 9.94) ∪ (9.96, 9.99)
B       = {x | 0 < x < 10 and |sin(π x)| > 1/2 }
        = (0.17, 0.83) ∪ (1.17, 1.83) ∪  ...  ∪ (8.17, 8.83) ∪ (9.17, 9.83)
A - B   = [0.83, 0.91) ∪ (1.08, 1.17] ∪  ...  ∪ (9.91, 9.94) ∪ (9.96, 9.99)
        length = 2.07586484118467
        not empty
```



## Perl 6

{{works with|Rakudo|2018.10}}

```perl6
class Iv {
    has $.range handles <min max excludes-min excludes-max minmax ACCEPTS>;
    method empty {
	$.min after $.max or $.min === $.max && ($.excludes-min || $.excludes-max)
    }
    multi method Bool() { not self.empty };
    method length() { $.max - $.min }
    method gist() {
	($.excludes-min ?? '(' !! '[') ~
	$.min ~ ',' ~ $.max ~
	($.excludes-max ?? ')' !! ']');
    }
}

class IvSet {
    has Iv @.intervals;

    sub canon (@i) {
	my @new = consolidate(|@i).grep(*.so);
	@new.sort(*.range.min);
    }

    method new(@ranges) {
	my @iv = canon @ranges.map: { Iv.new(:range($_)) }
	self.bless(:intervals(@iv));
    }

    method complement {
	my @new;
	my @old = @!intervals;
	if not @old {
	    return iv -Inf..Inf;
	}
	my $pre;
	push @old, Inf^..Inf unless @old[*-1].max === Inf;
	if @old[0].min === -Inf {
	    $pre = @old.shift;
	}
	else {
	    $pre = -Inf..^-Inf;
	}
	while @old {
	    my $old = @old.shift;
	    my $excludes-min = !$pre.excludes-max;
	    my $excludes-max = !$old.excludes-min;
	    push @new, Range.new($pre.max,$old.min,:$excludes-min,:$excludes-max);
	    $pre = $old;
	}
	IvSet.new(@new);
    }

    method ACCEPTS(IvSet:D $me: $candidate) {
	so $.intervals.any.ACCEPTS($candidate);
    }
    method empty { so $.intervals.all.empty }
    multi method Bool() { not self.empty };

    method length() { [+] $.intervals».length }
    method gist() { join ' ', $.intervals».gist }
}

sub iv(**@ranges) { IvSet.new(@ranges) }

multi infix:<∩> (Iv $a, Iv $b) {
    if $a.min ~~ $b or $a.max ~~ $b or $b.min ~~ $a or $b.max ~~ $a {
	my $min = $a.range.min max $b.range.min;
	my $max = $a.range.max min $b.range.max;
	my $excludes-min = not $min ~~ $a & $b;
	my $excludes-max = not $max ~~ $a & $b;
	Iv.new(:range(Range.new($min,$max,:$excludes-min, :$excludes-max)));
    }
}
multi infix:<∪> (Iv $a, Iv $b) {
    my $min = $a.range.min min $b.range.min;
    my $max = $a.range.max max $b.range.max;
    my $excludes-min = not $min ~~ $a | $b;
    my $excludes-max = not $max ~~ $a | $b;
    Iv.new(:range(Range.new($min,$max,:$excludes-min, :$excludes-max)));
}

multi infix:<∩> (IvSet $ars, IvSet $brs) {
    my @overlap;
    for $ars.intervals -> $a {
	for $brs.intervals -> $b {
	    if $a.min ~~ $b or $a.max ~~ $b or $b.min ~~ $a or $b.max ~~ $a {
		my $min = $a.range.min max $b.range.min;
		my $max = $a.range.max min $b.range.max;
		my $excludes-min = not $min ~~ $a & $b;
		my $excludes-max = not $max ~~ $a & $b;
		push @overlap, Range.new($min,$max,:$excludes-min, :$excludes-max);
	    }
	}
    }
    IvSet.new(@overlap)
}

multi infix:<∪> (IvSet $a, IvSet $b) {
    iv |$a.intervals».range, |$b.intervals».range;
}

multi consolidate() { () }
multi consolidate($this is copy, *@those) {
    gather {
        for consolidate |@those -> $that {
            if $this ∩ $that { $this ∪= $that }
            else             { take $that }
        }
        take $this;
    }
}

multi infix:<−> (IvSet $a, IvSet $b) { $a ∩ $b.complement }

multi prefix:<−> (IvSet $a) { $a.complement; }

constant ℝ = iv -Inf..Inf;

my $s1 = iv(0^..1) ∪ iv(0..^2);
my $s2 = iv(0..^2) ∩ iv(1^..2);
my $s3 = iv(0..^3) − iv(0^..^1);
my $s4 = iv(0..^3) − iv(0..1) ;

say "\t\t\t\t0\t1\t2";
say "(0, 1] ∪ [0, 2) -> $s1.gist()\t", 0 ~~ $s1,"\t", 1 ~~ $s1,"\t", 2 ~~ $s1;
say "[0, 2) ∩ (1, 2] -> $s2.gist()\t", 0 ~~ $s2,"\t", 1 ~~ $s2,"\t", 2 ~~ $s2;
say "[0, 3) − (0, 1) -> $s3.gist()\t", 0 ~~ $s3,"\t", 1 ~~ $s3,"\t", 2 ~~ $s3;
say "[0, 3) − [0, 1] -> $s4.gist()\t", 0 ~~ $s4,"\t", 1 ~~ $s4,"\t", 2 ~~ $s4;

say '';

say "ℝ is not empty: ", !ℝ.empty;
say "[0,3] − ℝ is empty: ", not iv(0..3) − ℝ;

my $A = iv(0..10) ∩
   iv |(0..10).map({ $_ - 1/6 .. $_ + 1/6 }).cache;

my $B = iv 0..sqrt(1/6),
      |(1..99).map({ sqrt($_-1/6) .. sqrt($_ + 1/6) }), sqrt(100-1/6)..10;

say 'A − A is empty: ', not $A − $A;

say '';

my $C = $A − $B;
say "A − B =";
say "  ",.gist for $C.intervals;
say "Length A − B = ", $C.length;
```

{{out}}

```txt
				0	1	2
(0, 1] ∪ [0, 2) -> [0,2)	True	True	False
[0, 2) ∩ (1, 2] -> (1,2)	False	False	False
[0, 3) − (0, 1) -> [0,0] [1,3)	True	True	True
[0, 3) − [0, 1] -> (1,3)	False	False	True

ℝ is not empty: True
[0,3] − ℝ is empty: True
A − A is empty: True

A − B =
  [0.833333,0.912870929175277)
  (1.08012344973464,1.166667]
  [1.833333,1.95789002074512)
  (2.04124145231932,2.166667]
  (2.85773803324704,2.97209241668783)
  (3.02765035409749,3.13581462037113)
  [3.833333,3.85140666943045)
  (3.89444048184931,3.97911212877111)
  (4.02077936060494,4.10284454169706)
  (4.14326763155202,4.166667]
  [4.833333,4.88193950529227)
  (4.91596040125088,4.98330546257535)
  (5.01663898109747,5.08265022732563)
  (5.11533641774094,5.166667]
  (5.84522597225006,5.90197706987526)
  (5.93014895821906,5.98609499868932)
  (6.01387285088957,6.06904715201104)
  (6.09644705272396,6.15088069574865)
  [6.833333,6.84348838921594)
  (6.8677992593455,6.91616464041548)
  (6.94022093788567,6.98808509774554)
  (7.01189465598754,7.05927286151579)
  (7.08284312029193,7.12974987873581)
  (7.15308791129165,7.166667]
  [7.833333,7.86341740805697)
  (7.88458411500991,7.92674796706274)
  (7.94774601171091,7.98957654280459)
  (8.01040989379861,8.05191488612077)
  (8.07258735887489,8.11377429642539)
  (8.13428956127495,8.166667]
  (8.8411914732499,8.87881373457813)
  (8.89756521002609,8.93495010245347)
  (8.95358401237553,8.99073597284078)
  (9.00925450115972,9.04617783007461)
  (9.06458309392477,9.10128196098403)
  (9.1195760135363,9.15605446321358)
  [9.833333,9.84039294608367)
  (9.85731538841416,9.89107341663853)
  (9.9079092984679,9.94149552800449)
  (9.9582461641931,9.99166319154791)
Length A − B = 2.07586484118467
```



## Phix

{{trans|Go}}

```Phix
enum ID,ARGS
function cf(sequence f, atom x) return call_func(f[ID],f[ARGS]&x) end function
function Union(sequence a, b, atom x) return cf(a,x)  or     cf(b,x) end function  constant r_Union = routine_id("Union")
function Inter(sequence a, b, atom x) return cf(a,x) and     cf(b,x) end function  constant r_Inter = routine_id("Inter")
function Diffr(sequence a, b, atom x) return cf(a,x) and not cf(b,x) end function  constant r_Diffr = routine_id("Diffr")
function OpOp(atom a, b, x)           return a <  x and x <  b       end function  constant r_OpOp  = routine_id("OpOp")
function ClCl(atom a, b, x)           return a <= x and x <= b       end function  constant r_ClCl  = routine_id("ClCl")
function OpCl(atom a, b, x)           return a <  x and x <= b       end function  constant r_OpCl  = routine_id("OpCl")
function ClOp(atom a, b, x)           return a <= x and x <  b       end function  constant r_ClOp  = routine_id("ClOp")

--                              expected
--              ---- desc ----,  0 1 2,  --------------- set method ---------------
constant s = {{"(0,1] u [0,2)", {1,1,0}, {r_Union,{{r_OpCl,{0,1}},{r_ClOp,{0,2}}}}},
              {"[0,2) n (1,2]", {0,0,0}, {r_Inter,{{r_ClOp,{0,2}},{r_OpCl,{1,2}}}}},
              {"[0,3) - (0,1)", {1,1,1}, {r_Diffr,{{r_ClOp,{0,3}},{r_OpOp,{0,1}}}}},
              {"[0,3) - [0,1]", {0,0,1}, {r_Diffr,{{r_ClOp,{0,3}},{r_ClCl,{0,1}}}}}},
         tf = {"True","False"}

for i=1 to length(s) do
    sequence {desc, expect, method} = s[i]
    for x=0 to 2 do
        bool r = cf(method,x)
        string error = iff(r!=expect[x+1]?"error":"")
        printf(1,"%d in %s : %s %s\n", {x, desc, tf[2-r],error})
    end for
    printf(1,"\n")
end for
```

{{out}}

```txt

0 in (0,1] u [0,2) : True
1 in (0,1] u [0,2) : True
2 in (0,1] u [0,2) : False

0 in [0,2) n (1,2] : False
1 in [0,2) n (1,2] : False
2 in [0,2) n (1,2] : False

0 in [0,3) - (0,1) : True
1 in [0,3) - (0,1) : True
2 in [0,3) - (0,1) : True

0 in [0,3) - [0,1] : False
1 in [0,3) - [0,1] : False
2 in [0,3) - [0,1] : True

```

Extra credit - also translated from Go, but with an extended loop and crude summation, inspired by Java/Kotlin.

```Phix
function aspxx(atom x) return abs(sin(PI*x*x))>0.5 end function constant r_aspxx = routine_id("aspxx")
function aspx(atom x) return abs(sin(PI*x))>0.5 end function constant r_aspx = routine_id("aspx")

constant A = {r_Inter,{{r_OpOp,{0,10}},{r_aspxx,{}}}},
         B = {r_Inter,{{r_OpOp,{0,10}},{r_aspx,{}}}},
         C = {r_Diffr,{A,B}}
atom x = 0, step = 0.00001, count = 0
while x<=10 do
    count += cf(C,x)
    x += step
end while
printf(1,"Approximate length of A-B: %.5f\n",{count*step})
```

{{out}}

```txt

Approximate length of A-B: 2.07587

```



## Python


```python
class Setr():
    def __init__(self, lo, hi, includelo=True, includehi=False):
        self.eqn = "(%i<%sX<%s%i)" % (lo,
                                      '=' if includelo else '',
                                      '=' if includehi else '',
                                      hi)

    def __contains__(self, X):
        return eval(self.eqn, locals())

    # union
    def __or__(self, b):
        ans = Setr(0,0)
        ans.eqn = "(%sor%s)" % (self.eqn, b.eqn)
        return ans

    # intersection
    def __and__(self, b):
        ans = Setr(0,0)
        ans.eqn = "(%sand%s)" % (self.eqn, b.eqn)
        return ans

    # difference
    def __sub__(self, b):
        ans = Setr(0,0)
        ans.eqn = "(%sand not%s)" % (self.eqn, b.eqn)
        return ans

    def __repr__(self):
        return "Setr%s" % self.eqn


sets = [
    Setr(0,1, 0,1) | Setr(0,2, 1,0),
    Setr(0,2, 1,0) & Setr(1,2, 0,1),
    Setr(0,3, 1,0) - Setr(0,1, 0,0),
    Setr(0,3, 1,0) - Setr(0,1, 1,1),
]
settexts = '(0, 1] ∪ [0, 2);[0, 2) ∩ (1, 2];[0, 3) − (0, 1);[0, 3) − [0, 1]'.split(';')

for s,t in zip(sets, settexts):
    print("Set %s %s. %s" % (t,
                             ', '.join("%scludes %i"
                                     % ('in' if v in s else 'ex', v)
                                     for v in range(3)),
                             s.eqn))
```


;Output:

```txt
Set (0, 1] ∪ [0, 2) includes 0, includes 1, excludes 2. ((0<X<=1)or(0<=X<2))
Set [0, 2) ∩ (1, 2] excludes 0, excludes 1, excludes 2. ((0<=X<2)and(1<X<=2))
Set [0, 3) − (0, 1) includes 0, includes 1, includes 2. ((0<=X<3)and not(0<X<1))
Set [0, 3) − [0, 1] excludes 0, excludes 1, includes 2. ((0<=X<3)and not(0<=X<=1))

```



## Racket

This is a simple representation of sets as functions (so obviously no good way to the the extra set length).

```Racket

#lang racket

;; Use a macro to allow infix operators
(require (only-in racket [#%app #%%app]))
(define-for-syntax infixes '())
(define-syntax (definfix stx)
  (syntax-case stx ()
    [(_ (x . xs) body ...) #'(definfix x (λ xs body ...))]
    [(_ x body) (begin (set! infixes (cons #'x infixes)) #'(define x body))]))
(define-syntax (#%app stx)
  (syntax-case stx ()
    [(_ X op Y)
     (and (identifier? #'op) (ormap (λ(o) (free-identifier=? #'op o)) infixes))
     #'(#%%app op X Y)]
    [(_ f x ...) #'(#%%app f x ...)]))


;; Ranges: (X +-+ Y) => [X,Y]; (X --- Y) => (X,Y); and same for `+--' and `--+'
;; Simple implementation as functions

;; Constructors
(definfix ((+-+ X Y) n) (<= X n Y))             ; [X,Y]
(definfix ((--- X Y) n) (< X n Y))              ; (X,Y)
(definfix ((+-- X Y) n) (and (<= X n) (< n Y))) ; [X,Y)
(definfix ((--+ X Y) n) (and (< X n) (<= n Y))) ; (X,Y]
(definfix ((== X) n) (= X n))                   ; [X,X]
;; Set operations
(definfix ((∪ . Rs) n)  (ormap  (λ(p) (p n)) Rs))
(definfix ((∩ . Rs) n)  (andmap (λ(p) (p n)) Rs))
(definfix ((∖ R1 R2) n) (and (R1 n) (not (R2 n)))) ; set-minus, not backslash
(define ((¬ R) n) (not (R n)))
;; Special sets
(define (∅ n) #f)
(define (ℜ n) #t)

(define-syntax-rule (try set)
  (apply printf "~a => ~a ~a ~a\n" (~s #:width 23 'set)
         (let ([pred set]) (for/list ([i 3]) (if (pred i) 'Y 'N)))))
(try ((0 --+ 1) ∪ (0 +-- 2)))
(try ((0 +-- 2) ∩ (1 --+ 2)))
(try ((0 +-- 3) ∖ (0 --- 1)))
(try ((0 +-- 3) ∖ (0 +-+ 1)))

```


Output:

```txt

((0 --+ 1) ∪ (0 +-- 2)) => Y Y N
((0 +-- 2) ∩ (1 --+ 2)) => N N N
((0 +-- 3) ∖ (0 --- 1)) => Y Y Y
((0 +-- 3) ∖ (0 +-+ 1)) => N N Y

```



## REXX

===no error checking, no ∞===

```rexx
/*REXX program  demonstrates  a way to  represent any set of real numbers  and  usage.  */
call quertySet 1, 3,  '[1,2)'
call quertySet ,   ,  '[0,2)   union   (1,3)'
call quertySet ,   ,  '[0,1)   union   (2,3]'
call quertySet ,   ,  '[0,2]   inter   (1,3)'
call quertySet ,   ,  '(1,2)     ∩     (2,3]'
call quertySet ,   ,  '[0,2)     \     (1,3)'
say;                                      say center(' start of required tasks ', 40, "═")
call quertySet ,   ,  '(0,1]   union   [0,2)'
call quertySet ,   ,  '[0,2)     ∩     (1,3)'
call quertySet ,   ,  '[0,3]     -     (0,1)'
call quertySet ,   ,  '[0,3]     -     [0,1]'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
emptySet:  parse arg _;               nam= valSet(_, 00);              return @.3>@.4
/*──────────────────────────────────────────────────────────────────────────────────────*/
isInSet:   parse arg #,x;             call valSet x
           if \datatype(#, 'N')       then call set_bad "number isn't not numeric:" #
           if (@.1=='(' &  #<=@.2) |,
              (@.1=='[' &  #< @.2) |,
              (@.4==')' &  #>=@.3) |,
              (@.4==']' &  #> @.3)    then return 0
           return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
quertySet: parse arg lv,hv,s1 oop s2 .;  op=oop;   upper op;      cop=
           if lv==''  then lv=0;      if hv==""  then hv= 2;      if op==''  then cop=  0
           if wordpos(op, '| or UNION')                 \==0                 then cop= "|"
           if wordpos(op, '& ∩ AND INTER INTERSECTION') \==0                 then cop= "&"
           if wordpos(op, '\ - DIF DIFF DIFFERENCE')    \==0                 then cop= "\"
           say
                   do i=lv  to hv;  b = isInSet(i, s1)
                   if cop\==0  then do
                                    b2= isInSet(i, s2)
                                    if cop=='&'  then b= b & b2
                                    if cop=='|'  then b= b | b2
                                    if cop=='\'  then b= b & \b2
                                    end
                   express = s1 center(oop, max(5, length(oop) ) )    s2
                   say right(i, 5)    ' is in set'     express": "   word('no yes', b+1)
                   end   /*i*/
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
valSet:    parse arg q;   q=space(q, 0);    L=length(q);     @.0= ',';     @.4= right(q,1)
           parse var q    @.1  2  @.2  ','  @.3  (@.4)
           if @.2>@.3  then parse var L   . @.0  @.2  @.3
           return space(@.1 @.2 @.0 @.3 @.4, 0)
```

{{out|output|text=  is the same as the next REXX version (below).}}

===has error checking, ∞ support===

```rexx
/*REXX program  demonstrates  a way to  represent any set of real numbers  and  usage.  */
call quertySet 1, 3,  '[1,2)'
call quertySet ,   ,  '[0,2)   union   (1,3)'
call quertySet ,   ,  '[0,1)   union   (2,3]'
call quertySet ,   ,  '[0,2]   inter   (1,3)'
call quertySet ,   ,  '(1,2)     ∩     (2,3]'
call quertySet ,   ,  '[0,2)     \     (1,3)'
say;                                      say center(' start of required tasks ', 40, "═")
call quertySet ,   ,  '(0,1]   union   [0,2)'
call quertySet ,   ,  '[0,2)     ∩     (1,3)'
call quertySet ,   ,  '[0,3]     -     (0,1)'
call quertySet ,   ,  '[0,3]     -     [0,1]'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
badSet:    say;    say  '***error*** bad format of SET_def:  ('arg(1)")";         exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
emptySet:  parse arg _;               nam= valSet(_, 00);                   return @.3>@.4
/*──────────────────────────────────────────────────────────────────────────────────────*/
isInSet:   parse arg #,x;             call valSet x
           if \datatype(#, 'N')       then call set_bad "number isn't not numeric:" #
           if (@.1=='(' &  #<=@.2) |,
              (@.1=='[' &  #< @.2) |,
              (@.4==')' &  #>=@.3) |,
              (@.4==']' &  #> @.3)    then return 0
           return 1
/*──────────────────────────────────────────────────────────────────────────────────────*/
quertySet: parse arg lv,hv,s1 oop s2 .;  op=oop;   upper op;      cop=
           if lv==''  then lv=0;      if hv==""  then hv= 2;      if op==''  then cop=  0
           if wordpos(op, '| or UNION')                 \==0                 then cop= "|"
           if wordpos(op, '& ∩ AND INTER INTERSECTION') \==0                 then cop= "&"
           if wordpos(op, '\ - DIF DIFF DIFFERENCE')    \==0                 then cop= "\"
           say
                   do i=lv  to hv;  b = isInSet(i, s1)
                   if cop\==0  then do
                                    b2= isInSet(i, s2)
                                    if cop=='&'  then b= b & b2
                                    if cop=='|'  then b= b | b2
                                    if cop=='\'  then b= b & \b2
                                    end
                   express = s1 center(oop, max(5, length(oop) ) )    s2
                   say right(i, 5)    ' is in set'     express": "   word('no yes', b+1)
                   end   /*i*/
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
valSet:    parse arg q;              q=space(q, 0);    L= length(q);       @.0= ','
           infinity = copies(9, digits() - 1)'e'copies(9, digits() - 1)0
           if L<2                    then call set_bad  'invalid expression'
           @.4= right(q, 1)
           parse var q  @.1  2  @.2  ','  @.3  (@.4)
           if @.1\=='(' & @.1\=="["  then call set_bad  'left boundry'
           if @.4\==')' & @.4\=="]"  then call set_bad  'right boundry'
                    do j=2  to 3;    u=@.j;               upper u
                    if right(@.j, 1)=='∞' | u="INFINITY"  then @.j= '-'infinity
                    if \datatype(@.j, 'N')  then call set_bad  "value not numeric:"    @.j
                    end  /*j*/
           if @.2>@.3  then parse var   L  .  @.0  @.2  @.3
           return space(@.1 @.2 @.0 @.3 @.4,  0)
```

{{out|output|text=  when using the (internal) default inputs:}}

```txt

    1  is in set [1,2)       :  yes
    2  is in set [1,2)       :  no
    3  is in set [1,2)       :  no

    0  is in set [0,2) union (1,3):  yes
    1  is in set [0,2) union (1,3):  yes
    2  is in set [0,2) union (1,3):  yes

    0  is in set [0,1) union (2,3]:  yes
    1  is in set [0,1) union (2,3]:  no
    2  is in set [0,1) union (2,3]:  no

    0  is in set [0,2] inter (1,3):  no
    1  is in set [0,2] inter (1,3):  no
    2  is in set [0,2] inter (1,3):  yes

    0  is in set (1,2)   ∩   (2,3]:  no
    1  is in set (1,2)   ∩   (2,3]:  no
    2  is in set (1,2)   ∩   (2,3]:  no

    0  is in set [0,2)   \   (1,3):  yes
    1  is in set [0,2)   \   (1,3):  yes
    2  is in set [0,2)   \   (1,3):  no

═══════ start of required tasks ════════

    0  is in set (0,1] union [0,2):  yes
    1  is in set (0,1] union [0,2):  yes
    2  is in set (0,1] union [0,2):  no

    0  is in set [0,2)   ∩   (1,3):  no
    1  is in set [0,2)   ∩   (1,3):  no
    2  is in set [0,2)   ∩   (1,3):  no

    0  is in set [0,3]   -   (0,1):  yes
    1  is in set [0,3]   -   (0,1):  yes
    2  is in set [0,3]   -   (0,1):  yes

    0  is in set [0,3]   -   [0,1]:  no
    1  is in set [0,3]   -   [0,1]:  no
    2  is in set [0,3]   -   [0,1]:  yes

```



## Ruby

{{works with|Ruby|1.9.3}}

```ruby
class Rset
  Set = Struct.new(:lo, :hi, :inc_lo, :inc_hi) do
    def include?(x)
      (inc_lo ? lo<=x : lo<x) and (inc_hi ? x<=hi : x<hi)
    end
    def length
      hi - lo
    end
    def to_s
      "#{inc_lo ? '[' : '('}#{lo},#{hi}#{inc_hi ? ']' : ')'}"
    end
  end

  def initialize(lo=nil, hi=nil, inc_lo=false, inc_hi=false)
    if lo.nil? and hi.nil?
      @sets = []            # empty set
    else
      raise TypeError      unless lo.is_a?(Numeric) and hi.is_a?(Numeric)
      raise ArgumentError  unless valid?(lo, hi, inc_lo, inc_hi)
      @sets = [Set[lo, hi, !!inc_lo, !!inc_hi]]         # !! -> Boolean values
    end
  end

  def self.[](lo, hi, inc_hi=true)
    self.new(lo, hi, true, inc_hi)
  end

  def self.parse(str)
    raise ArgumentError  unless str =~ /(\[|\()(.+),(.+)(\]|\))/
    b0, lo, hi, b1 = $~.captures        # $~ : Regexp.last_match
    lo = Rational(lo)
    lo = lo.numerator  if lo.denominator == 1
    hi = Rational(hi)
    hi = hi.numerator  if hi.denominator == 1
    self.new(lo, hi, b0=='[', b1==']')
  end

  def initialize_copy(obj)
    super
    @sets = @sets.map(&:dup)
  end

  def include?(x)
    @sets.any?{|set| set.include?(x)}
  end

  def empty?
    @sets.empty?
  end

  def union(other)
    sets = (@sets+other.sets).map(&:dup).sort_by{|set| [set.lo, set.hi]}
    work = []
    pre = sets.shift
    sets.each do |post|
      if valid?(pre.hi, post.lo, !pre.inc_hi, !post.inc_lo)
        work << pre
        pre = post
      else
        pre.inc_lo |= post.inc_lo  if pre.lo == post.lo
        if pre.hi < post.hi
          pre.hi = post.hi
          pre.inc_hi = post.inc_hi
        elsif pre.hi == post.hi
          pre.inc_hi |= post.inc_hi
        end
      end
    end
    work << pre  if pre
    new_Rset(work)
  end
  alias | union

  def intersection(other)
    sets = @sets.map(&:dup)
    work = []
    other.sets.each do |oset|
      sets.each do |set|
        if set.hi < oset.lo or oset.hi < set.lo
          # ignore
        elsif oset.lo < set.lo and set.hi < oset.hi
          work << set
        else
          lo = [set.lo, oset.lo].max
          if set.lo == oset.lo
            inc_lo = set.inc_lo && oset.inc_lo
          else
            inc_lo = (set.lo < oset.lo) ? oset.inc_lo : set.inc_lo
          end
          hi = [set.hi, oset.hi].min
          if set.hi == oset.hi
            inc_hi = set.inc_hi && oset.inc_hi
          else
            inc_hi = (set.hi < oset.hi) ? set.inc_hi : oset.inc_hi
          end
          work << Set[lo, hi, inc_lo, inc_hi]  if valid?(lo, hi, inc_lo, inc_hi)
        end
      end
    end
    new_Rset(work)
  end
  alias & intersection

  def difference(other)
    sets = @sets.map(&:dup)
    other.sets.each do |oset|
      work = []
      sets.each do |set|
        if set.hi < oset.lo or oset.hi < set.lo
          work << set
        elsif oset.lo < set.lo and set.hi < oset.hi
          # delete
        else
          if set.lo < oset.lo
            inc_hi = (set.hi==oset.lo and !set.inc_hi) ? false : !oset.inc_lo
            work << Set[set.lo, oset.lo, set.inc_lo, inc_hi]
          elsif valid?(set.lo, oset.lo, set.inc_lo, !oset.inc_lo)
            work << Set[set.lo, set.lo, true, true]
          end
          if oset.hi < set.hi
            inc_lo = (oset.hi==set.lo and !set.inc_lo) ? false : !oset.inc_hi
            work << Set[oset.hi, set.hi, inc_lo, set.inc_hi]
          elsif valid?(oset.hi, set.hi, !oset.inc_hi, set.inc_hi)
            work << Set[set.hi, set.hi, true, true]
          end
        end
      end
      sets = work
    end
    new_Rset(sets)
  end
  alias - difference

  # symmetric difference
  def ^(other)
    (self - other) | (other - self)
  end

  def ==(other)
    self.class == other.class and @sets == other.sets
  end

  def length
    @sets.inject(0){|len, set| len + set.length}
  end

  def to_s
    "#{self.class}#{@sets.join}"
  end
  alias inspect to_s

  protected

  attr_accessor :sets

  private

  def new_Rset(sets)
    rset = self.class.new          # empty set
    rset.sets = sets
    rset
  end

  def valid?(lo, hi, inc_lo, inc_hi)
    lo < hi or (lo==hi and inc_lo and inc_hi)
  end
end

def Rset(lo, hi, inc_hi=false)
  Rset.new(lo, hi, false, inc_hi)
end
```


Test case:

```ruby
p a = Rset[1,2,false]
[1,2,3].each{|x|puts "#{x} => #{a.include?(x)}"}
puts
a = Rset[0,2,false]             #=> Rset[0,2)
b = Rset(1,3)                   #=> Rset(1,3)
c = Rset[0,1,false]             #=> Rset[0,1)
d = Rset(2,3,true)              #=> Rset(2,3]
puts "#{a} | #{b} -> #{a | b}"
puts "#{c} | #{d} -> #{c | d}"
puts
puts "#{a} & #{b} -> #{a & b}"
puts "#{c} & #{d} -> #{c & d}"
puts "(#{c} & #{d}).empty? -> #{(c&d).empty?}"
puts
puts "#{a} - #{b} -> #{a - b}"
puts "#{a} - #{a} -> #{a - a}"
e = Rset(0,3,true)
f = Rset[1,2]
puts "#{e} - #{f} -> #{e - f}"

puts "\nTest :"
test_set = [["(0, 1]", "|", "[0, 2)"],
            ["[0, 2)", "&", "(1, 2]"],
            ["[0, 3)", "-", "(0, 1)"],
            ["[0, 3)", "-", "[0, 1]"] ]
test_set.each do |sa,ope,sb|
  str = "#{sa} #{ope} #{sb}"
  e = eval("Rset.parse(sa) #{ope} Rset.parse(sb)")
  puts "%s -> %s" % [str, e]
  (0..2).each{|i| puts "  #{i} : #{e.include?(i)}"}
end

puts
test_set = ["x = Rset[0,2] | Rset(3,7) | Rset[8,10]",
            "y = Rset(7,9) | Rset(5,6) | Rset[1,4]",
            "x | y", "x & y", "x - y", "y - x", "x ^ y",
            "y ^ x == (x | y) - (x & y)"]
x = y = nil
test_set.each {|str| puts "#{str} -> #{eval(str)}"}

puts
inf = 1.0 / 0.0             # infinity
puts "a = #{a = Rset(-inf,inf)}"
puts "b = #{b = Rset.parse('[1/3,11/7)')}"
puts "a - b -> #{a - b}"
```


{{out}}

```txt

Rset[1,2)
1 => true
2 => false
3 => false

Rset[0,2) | Rset(1,3) -> Rset[0,3)
Rset[0,1) | Rset(2,3] -> Rset[0,1)(2,3]

Rset[0,2) & Rset(1,3) -> Rset(1,2)
Rset[0,1) & Rset(2,3] -> Rset
(Rset[0,1) & Rset(2,3]).empty? -> true

Rset[0,2) - Rset(1,3) -> Rset[0,1]
Rset[0,2) - Rset[0,2) -> Rset
Rset(0,3] - Rset[1,2] -> Rset(0,1)(2,3]

Test :
(0, 1] | [0, 2) -> Rset[0,2)
  0 : true
  1 : true
  2 : false
[0, 2) & (1, 2] -> Rset(1,2)
  0 : false
  1 : false
  2 : false
[0, 3) - (0, 1) -> Rset[0,0][1,3)
  0 : true
  1 : true
  2 : true
[0, 3) - [0, 1] -> Rset(1,3)
  0 : false
  1 : false
  2 : true

x = Rset[0,2] | Rset(3,7) | Rset[8,10] -> Rset[0,2](3,7)[8,10]
y = Rset(7,9) | Rset(5,6) | Rset[1,4] -> Rset[1,4](5,6)(7,9)
x | y -> Rset[0,7)(7,10]
x & y -> Rset[1,2](3,4](5,6)[8,9)
x - y -> Rset[0,1)(4,5][6,7)[9,10]
y - x -> Rset(2,3](7,8)
x ^ y -> Rset[0,1)(2,3](4,5][6,7)(7,8)[9,10]
y ^ x == (x | y) - (x & y) -> true

a = Rset(-Infinity,Infinity)
b = Rset[1/3,11/7)
a - b -> Rset(-Infinity,1/3)[11/7,Infinity)

```


'''Optional work:'''
{{works with|Ruby|2.1+}}
(with Rational suffix.)

```ruby
str, e = "e = Rset.new", nil
puts "#{str} -> #{eval(str)}\t\t# create empty set"
str = "e.empty?"
puts "#{str} -> #{eval(str)}"
puts

include Math
lohi = Enumerator.new do |y|
  t = 1 / sqrt(6)
  0.step do |n|
    y << [sqrt(12*n+1) * t, sqrt(12*n+5) * t]
    y << [sqrt(12*n+7) * t, sqrt(12*n+11) * t]
  end
end

a = Rset.new
loop do
  lo, hi = lohi.next
  break  if 10 <= lo
  a |= Rset(lo, hi)
end
a &= Rset(0,10)

b = (0...10).inject(Rset.new){|res,i| res |= Rset(i+1/6r,i+5/6r)}

puts "a        : #{a}"
puts "a.length : #{a.length}"
puts "b        : #{b}"
puts "b.length : #{b.length}"
puts "a - b    : #{a - b}"
puts "(a-b).length : #{(a-b).length}"
```


{{out}}

```txt

e = Rset.new -> Rset		# create empty set
e.empty? -> true

a        : Rset(0.4082482904638631,0.912870929175277)(1.0801234497346435,1.3540064007726602)(1.4719601443879746,1.6832508230603467) ... (9.907909298467901,9.941495528004495)(9.958246164193106,9.991663191547909)
a.length : 6.50103079235655
b        : Rset(1/6,5/6)(7/6,11/6)(13/6,17/6)(19/6,23/6)(25/6,29/6)(31/6,35/6)(37/6,41/6)(43/6,47/6)(49/6,53/6)(55/6,59/6)
b.length : 20/3
a - b    : Rset[5/6,0.912870929175277)(1.0801234497346435,7/6][11/6,1.9578900207451218)(2.041241452319315,13/6] ... (9.907909298467901,9.941495528004495)(9.958246164193106,9.991663191547909)
(a-b).length : 2.0758648411846745

```



## Tcl

This code represents each set of real numbers as a collection of ranges, where each range is quad of the two boundary values and whether each of those boundaries is a closed boundary. (Using expressions internally would make the code much shorter, at the cost of being much less tractable when it comes to deriving information like the length of the real line “covered” by the set.) A side-effect of the representation is that the length of the list that represents the set is, after normalization, the number of discrete ranges in the set.

```tcl
package require Tcl 8.5

proc inRange {x range} {
    lassign $range a aClosed b bClosed
    expr {($aClosed ? $a<=$x : $a<$x) && ($bClosed ? $x<=$b : $x<$b)}
}
proc normalize {A} {
    set A [lsort -index 0 -real [lsort -index 1 -integer -decreasing $A]]
    for {set i 0} {$i < [llength $A]} {incr i} {
	lassign [lindex $A $i] a aClosed b bClosed
	if {$b < $a || ($a == $b && !($aClosed && $bClosed))} {
	    set A [lreplace $A $i $i]
	    incr i -1
	}
    }
    for {set i 0} {$i < [llength $A]} {incr i} {
	for {set j [expr {$i+1}]} {$j < [llength $A]} {incr j} {
	    set R [lindex $A $i]
	    lassign [lindex $A $j] a aClosed b bClosed
	    if {[inRange $a $R]} {
		if {![inRange $b $R]} {
		    lset A $i 2 $b
		    lset A $i 3 $bClosed
		}
		set A [lreplace $A $j $j]
		incr j -1
	    }
	}
    }
    return $A
}

proc realset {args} {
    set RE {^\s*([\[(])\s*([-\d.e]+|-inf)\s*,\s*([-\d.e]+|inf)\s*([\])])\s*$}
    set result {}
    foreach s $args {
	if {
	    [regexp $RE $s --> left a b right] &&
	    [string is double $a] && [string is double $b]
	} then {
	    lappend result [list \
		$a [expr {$left eq "\["}] $b [expr {$right eq "\]"}]]
	} else {
	    error "bad range descriptor"
	}
    }
    return $result
}
proc elementOf {x A} {
    foreach range $A {
	if {[inRange $x $range]} {return 1}
    }
    return 0
}
proc union {A B} {
    return [normalize [concat $A $B]]
}
proc intersection {A B} {
    set B [normalize $B]
    set C {}
    foreach RA [normalize $A] {
	lassign $RA Aa AaClosed Ab AbClosed
	foreach RB $B {
	    lassign $RB Ba BaClosed Bb BbClosed
	    if {$Aa > $Bb || $Ba > $Ab} continue
	    set RC {}
	    lappend RC [expr {max($Aa,$Ba)}]
	    if {$Aa==$Ba} {
		lappend RC [expr {min($AaClosed,$BaClosed)}]
	    } else {
		lappend RC [expr {$Aa>$Ba ? $AaClosed : $BaClosed}]
	    }
	    lappend RC [expr {min($Ab,$Bb)}]
	    if {$Ab==$Bb} {
		lappend RC [expr {min($AbClosed,$BbClosed)}]
	    } else {
		lappend RC [expr {$Ab<$Bb ? $AbClosed : $BbClosed}]
	    }
	    lappend C $RC
	}
    }
    return [normalize $C]
}
proc difference {A B} {
    set C {}
    set B [normalize $B]
    foreach arange [normalize $A] {
	if {[isEmpty [intersection [list $arange] $B]]} {
	    lappend C $arange
	    continue
	}
	lassign $arange Aa AaClosed Ab AbClosed
	foreach brange $B {
	    lassign $brange Ba BaClosed Bb BbClosed
	    if {$Bb < $Aa || ($Bb==$Aa && !($AaClosed && $BbClosed))} {
		continue
	    }
	    if {$Ab < $Ba || ($Ab==$Ba && !($BaClosed && $AbClosed))} {
		lappend C [list $Aa $AaClosed $Ab $AbClosed]
		unset arange
		break
	    }
	    if {$Aa==$Bb} {
		set AaClosed 0
		continue
	    } elseif {$Ab==$Ba} {
		set AbClosed 0
		lappend C [list $Aa $AaClosed $Ab $AbClosed]
		unset arange
		continue
	    }
	    if {$Aa<$Ba} {
		lappend C [list $Aa $AaClosed $Ba [expr {!$BaClosed}]]
		if {$Ab>$Bb} {
		    set Aa $Bb
		    set AaClosed [expr {!$BbClosed}]
		} else {
		    unset arange
		    break
		}
	    } elseif {$Aa==$Ba} {
		lappend C [list $Aa $AaClosed $Ba [expr {!$BaClosed}]]
		set Aa $Bb
		set AaClosed [expr {!$BbClosed}]
	    } else {
		set Aa $Bb
		set AaClosed [expr {!$BbClosed}]
	    }
	}
	if {[info exist arange]} {
	    lappend C [list $Aa $AaClosed $Ab $AbClosed]
	}
    }
    return [normalize $C]
}
proc isEmpty A {
    expr {![llength [normalize $A]]}
}
proc length A {
    set len 0.0
    foreach range [normalize $A] {
	lassign $range a _ b _
	set len [expr {$len + ($b-$a)}]
    }
    return $len
}
```

Basic problems:

```tcl
foreach {str Set} {
    {(0, 1] ∪ [0, 2)} {
	union [realset {(0,1]}] [realset {[0,2)}]
    }
    {[0, 2) ∩ (1, 2]} {
	intersection [realset {[0,2)}] [realset {(1,2]}]
    }
    {[0, 3) − (0, 1)} {
	difference [realset {[0,3)}] [realset {(0,1)}]
    }
    {[0, 3) − [0, 1]} {
	difference [realset {[0,3)}] [realset {[0,1]}]
    }
} {
    set Set [eval $Set]
    foreach x {0 1 2} {
	puts "$x : $str :\t[elementOf $x $Set]"
    }
}
```

Extra credit:

```tcl
proc spi2 {from to} {
    for {set i $from} {$i<=$to} {incr i} {
	lappend result [list [expr {$i+1./6}] 0 [expr {$i+5./6}] 0]
    }
    return [intersection [list [list $from 0 $to 0]] $result]
}
proc applyfunc {var func} {
    upvar 1 $var A
    for {set i 0} {$i < [llength $A]} {incr i} {
	lassign [lindex $A $i] a - b -
	lset A $i 0 [$func $a]
	lset A $i 2 [$func $b]
    }
}
set A [spi2 0 100]
applyfunc A ::tcl::mathfunc::sqrt
set B [spi2 0 10]
set AB [difference $A $B]
puts "[llength $AB] contiguous subsets, total length [length $AB]"
```

Output:

```txt

0 : (0, 1] ∪ [0, 2) :	1
1 : (0, 1] ∪ [0, 2) :	1
2 : (0, 1] ∪ [0, 2) :	0
0 : [0, 2) ∩ (1, 2] :	0
1 : [0, 2) ∩ (1, 2] :	0
2 : [0, 2) ∩ (1, 2] :	0
0 : [0, 3) − (0, 1) :	1
1 : [0, 3) − (0, 1) :	1
2 : [0, 3) − (0, 1) :	1
0 : [0, 3) − [0, 1] :	0
1 : [0, 3) − [0, 1] :	0
2 : [0, 3) − [0, 1] :	1
40 contiguous subsets, total length 2.075864841184667

```



## zkl

{{trans|D}}
No ∞

```zkl
class RealSet{
   fcn init(fx){ var [const] contains=fx; }
   fcn holds(x){ contains(x) }
   fcn __opAdd(rs){ RealSet('wrap(x){ contains(x) or rs.contains(x) }) }
   fcn __opSub(rs){ RealSet('wrap(x){ contains(x) and not rs.contains(x) }) }
   fcn intersection(rs) { RealSet('wrap(x){ contains(x) and rs.contains(x) }) }
}
```

The python method could used but the zkl compiler is slow when used in code to generate code.

The method used is a bit inefficient because it closes the contains function of the other set so you can build quite a long call chain as you create new sets.

```zkl
tester := TheVault.Test.UnitTester.UnitTester();

    // test union
s:=RealSet(fcn(x){ 0.0 <  x <= 1.0 }) +
   RealSet(fcn(x){ 0.0 <= x <  1.0 });
tester.testRun(s.holds(0.0),Void,True,__LINE__);
tester.testRun(s.holds(1.0),Void,True,__LINE__);
tester.testRun(s.holds(2.0),Void,False,__LINE__);

    // test difference
s1 := RealSet(fcn(x){ 0.0 <= x < 3.0 }) -
      RealSet(fcn(x){ 0.0 <  x < 1.0 });
tester.testRun(s1.holds(0.0),Void,True,__LINE__);
tester.testRun(s1.holds(0.5),Void,False,__LINE__);
tester.testRun(s1.holds(1.0),Void,True,__LINE__);
tester.testRun(s1.holds(2.0),Void,True,__LINE__);

s2 := RealSet(fcn(x){ 0.0 <= x <  3.0 }) -
      RealSet(fcn(x){ 0.0 <= x <= 1.0 });
tester.testRun(s2.holds(0.0),Void,False,__LINE__);
tester.testRun(s2.holds(1.0),Void,False,__LINE__);
tester.testRun(s2.holds(2.0),Void,True,__LINE__);

    // test intersection
s := RealSet(fcn(x){ 0.0 <= x <  2.0 }).intersection(
     RealSet(fcn(x){ 1.0 <  x <= 2.0 }));
tester.testRun(s.holds(0.0),Void,False,__LINE__);
tester.testRun(s.holds(1.0),Void,False,__LINE__);
tester.testRun(s.holds(2.0),Void,False,__LINE__);
```

{{out}}

```txt

$ zkl bbb

### ================== Unit Test 1 ==================

Test 1 passed!

### ================== Unit Test 2 ==================

Test 2 passed!
...

### ================== Unit Test 12 ==================

Test 12 passed!

### ================== Unit Test 13 ==================

Test 13 passed!

```


{{omit from|Delphi}}
{{omit from|Free Pascal}}
{{omit from|Lilypond}}
{{omit form|Pascal|the base type of sets has to be an ordinal type}}
{{omit from|TPP}}

[[Category:Discrete math]]
