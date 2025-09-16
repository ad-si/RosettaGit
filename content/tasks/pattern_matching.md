+++
title = "Pattern matching"
description = ""
date = 2019-08-31T13:48:48Z
aliases = []
[extra]
id = 2193
[taxonomies]
categories = ["task", "Data Structures"]
tags = []
languages = [
  "bracmat",
  "clojure",
  "common_lisp",
  "cpp",
  "echolisp",
  "elixir",
  "emacs_lisp",
  "erlang",
  "go",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "ocaml",
  "oz",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "prolog",
  "racket",
  "rascal",
  "rexx",
  "rust",
  "scala",
  "standard_ml",
  "swift",
  "tcl",
]
+++

Some languages offer direct support for [[wp:Algebraic_data_type|algebraic data types]] and pattern matching on them. While this of course can always be simulated with manual tagging and conditionals, it allows for terse code which is easy to read, and can represent the algorithm directly.


## Task

As an example, implement insertion in a [[wp:Red_Black_Tree|red-black-tree]]. 

A red-black-tree is a binary tree where each internal node has a color attribute ''red'' or ''black''. Moreover, no red node can have a red child, and every path from the root to an empty node must contain the same number of black nodes. As a consequence, the tree is balanced, and must be re-balanced after an insertion.





## Bracmat



```bracmat
( ( balance
  =   a x b y c zd
    .       !arg
          : ( B
            .   ( ( R
                  .   ((R.?a,?x,?b),?y,?c)
                    | (?a,?x,(R.?b,?y,?c))
                  )
                , ?zd
                )
              | ( ?a
                , ?x
                , ( R
                  .   ((R.?b,?y,?c),?zd)
                    | (?b,?y,(R.?c,?zd))
                  )
                )
            )
        & (R.(B.!a,!x,!b),!y,(B.!c,!zd))
      | !arg
  )
& ( ins
  =   C X tree a m z
    .     !arg:(?X.?tree)
        & !tree:(?C.?a,?m,?z)
        & (   !X:<!m
            & balance$(!C.ins$(!X.!a),!m,!z)
          |   !X:>!m
            & balance$(!C.!a,!m,ins$(!X.!z))
          | !tree
          )
      | (R.,!X,)
  )
& ( insert
  =   X tree
    .   !arg:(?X.?tree)
      & ins$(!X.!tree):(?.?X)
      & (B.!X)
  )
& ( insertMany
  =   L R tree
    .     !arg:(%?L_%?R.?tree)
        & insertMany$(!L.!tree):?tree
        & insertMany$(!R.!tree)
      | insert$!arg
  )
);
```


Test:

```bracmat
(   (   it allows for terse code which is easy to read
      , and can represent the algorithm directly
    .
    )
  : ?values
& insertMany$(!values.):?tree
& lst$tree
& done
);
```


Output:

```bracmat
(tree=
  B
.   ( B
    .   (R.(B.,,),algorithm,(B.,allows,))
      , and
      , (B.,can,)
    )
  , code
  , ( R
    .   ( B
        .   (B.(R.,directly,),easy,)
          , for
          , (B.(R.,is,),it,)
        )
      , read
      , ( B
        .   (B.,represent,)
          , terse
          , (R.(B.,the,),to,(B.,which,))
        )
    )
);
```



## C++

### Compile time

C++ templates have a robust pattern matching facility, with some warts - for example, nested templates cannot be fully specialized, so we must use a dummy template parameter. This implementation uses C++17 deduced template parameters for genericity.


```c++
enum Color { R, B };
template<Color, class, auto, class> struct T;
struct E;

template<Color col, class a, auto x, class b> struct balance {
    using type = T<col, a, x, b>;
};
template<class a, auto x, class b, auto y, class c, auto z, class d>
struct balance<B, T<R, T<R, a, x, b>, y, c>, z, d> {
    using type = T<R, T<B, a, x, b>, y, T<B, c, z, d>>;
};
template<class a, auto x, class b, auto y, class c, auto z, class d>
struct balance<B, T<R, a, x, T<R, b, y, c>>, z, d> {
    using type = T<R, T<B, a, x, b>, y, T<B, c, z, d>>;
};
template<class a, auto x, class b, auto y, class c, auto z, class d>
struct balance<B, a, x, T<R, T<R, b, y, c>, z, d>> {
    using type = T<R, T<B, a, x, b>, y, T<B, c, z, d>>;
};
template<class a, auto x, class b, auto y, class c, auto z, class d>
struct balance<B, a, x, T<R, b, y, T<R, c, z, d>>> {
    using type = T<R, T<B, a, x, b>, y, T<B, c, z, d>>;
};

template<auto x, class s> struct insert {
    template<class, class = void> struct ins;
    template<class _> struct ins<E, _> { using type = T<R, E, x, E>; };
    template<Color col, class a, auto y, class b> struct ins<T<col, a, y, b>> {
        template<int, class = void> struct cond;
        template<class _> struct cond<-1, _> : balance<col, typename ins<a>::type, y, b> {};
        template<class _> struct cond<1, _> : balance<col, a, y, typename ins<b>::type> {};
        template<class _> struct cond<0, _> { using type = T<col, a, y, b>; };
        using type = typename cond<x < y ? -1 : y < x ? 1 : 0>::type;
    };
    template<class> struct repaint;
    template<Color col, class a, auto y, class b>
    struct repaint<T<col, a, y, b>> { using type = T<B, a, y, b>; };
    using type = typename repaint<typename ins<s>::type>::type;
};
template<auto x, class s> using insert_t = typename insert<x, s>::type;

template<class> void print();
int main() {
    print<insert_t<1, insert_t<2, insert_t<0, insert_t<4, E>>>>>();
}
```



### Run time

Although C++ has structured bindings and pattern matching through function overloading, it is not yet possible to use them together so we must match the structure of the tree being rebalanced separately from decomposing it into its elements. A further issue is that function overloads are not ordered, so to avoid ambiguity we must explicitly reject any (ill-formed) trees that would match more than one case during rebalance.


```c++
#include <memory>

#include <variant>

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

enum Color { R, B };
using E = std::monostate;
template<class, Color> struct Node;
template<class T, Color C> using Ptr = std::unique_ptr<Node<T, C>>;
template<class T> using Tree = std::variant<E, Ptr<T, R>, Ptr<T, B>>;
template<class T, Color Col> struct Node {
    static constexpr auto C = Col;
    Tree<T> left;
    T value;
    Tree<T> right;
};
template<Color C, class A, class T, class B> Tree<T> tree(A&& a, T& x, B&& b) {
    return Tree<T>{Ptr<T, C>{new Node<T, C>{std::move(a), std::move(x), std::move(b)}}};
}

template<class T> Tree<T> balance(Tree<T> s) {
    auto&& ll = [](Ptr<T, R>& s, Ptr<T, R>& t, auto&, Ptr<T, B>& u, auto&, auto&, auto&) {
        auto& [a, x, b] = *s;
        auto& [s_, y, c] = *t;
        auto& [t_, z, d] = *u;
        return tree<R>(tree<B>(a, x, b), y, tree<B>(c, z, d));
    };
    auto&& lr = [](auto&, Ptr<T, R>& s, Ptr<T, R>& t, Ptr<T, B>& u, auto&, auto&, auto&) {
        auto& [a, x, t_] = *s;
        auto& [b, y, c] = *t;
        auto& [s_, z, d] = *u;
        return tree<R>(tree<B>(a, x, b), y, tree<B>(c, z, d));
    };
    auto&& rl = [](auto&, auto&, auto&, Ptr<T, B>& s, Ptr<T, R>& t, Ptr<T, R>& u, auto&) {
        auto& [a, x, u_] = *s;
        auto& [b, y, c] = *t;
        auto& [t_, z, d] = *u;
        return tree<R>(tree<B>(a, x, b), y, tree<B>(c, z, d));
    };
    auto&& rr = [](auto&, auto&, auto&, Ptr<T, B>& s, auto&, Ptr<T, R>& t, Ptr<T, R>& u) {
        auto& [a, x, t_] = *s;
        auto& [b, y, u_] = *t;
        auto& [c, z, d] = *u;
        return tree<R>(tree<B>(a, x, b), y, tree<B>(c, z, d));
    };
    auto&& l = [](auto& s) -> Tree<T>& {
        return *std::visit(overloaded{[&](E) { return &s; }, [](auto& t) { return &t->left; }}, s);
    };
    auto&& r = [](auto& s) -> Tree<T>& {
        return *std::visit(overloaded{[&](E) { return &s; }, [](auto& t) { return &t->right; }}, s);
    };
    return std::visit([&](auto&... ss) -> Tree<T> {
        if constexpr (1 <
            std::is_invocable_v<decltype(ll), decltype(ss)...> +
            std::is_invocable_v<decltype(lr), decltype(ss)...> +
            std::is_invocable_v<decltype(rl), decltype(ss)...> +
            std::is_invocable_v<decltype(rr), decltype(ss)...>)
            throw std::logic_error{""};
        else
            return overloaded{ll, lr, rl, rr, [&](auto&... ss) { return std::move(s); }}(ss...);
    }, l(l(s)), l(s), r(l(s)), s, l(r(s)), r(s), r(r(s)));
}
template<class T> Tree<T> ins(T& x, Tree<T>& s) {
    return std::visit(overloaded{
        [&](E) -> Tree<T> { return tree<R>(s, x, s); },
        [&](auto& t) {
            auto& [a, y, b] = *t;
            static constexpr auto Col = std::remove_reference_t<decltype(*t)>::C;
            return x < y ? balance(tree<Col>(ins(x, a), y, b)) :
                y < x ? balance(tree<Col>(a, y, ins(x, b))) :
                std::move(s);
        },
    }, s);
}
template<class T> Tree<T> insert(T x, Tree<T> s) {
    return std::visit(overloaded{
        [](E) -> Tree<T> { throw std::logic_error{""}; },
        [](auto&& t) {
            auto& [a, y, b] = *t;
            return tree<B>(a, y, b);
        }
    }, ins(x, s));
}

#include <iostream>
template<class T> void print(Tree<T> const& s, int i = 0) {
    std::visit(overloaded{
        [](E) {},
        [&](auto& t) {
            auto& [a, y, b] = *t;
            print(a, i + 1);
            std::cout << std::string(i, ' ') << "RB"[t->C] << " " << y << "\n";
            print(b, i + 1);
        }
    }, s);
}
int main(int argc, char* argv[]) {
    auto t = Tree<std::string>{};
    for (auto i = 1; i != argc; ++i)
        t = insert(std::string{argv[i]}, std::move(t));
    print(t);
}
```



## Clojure

Pattern matching library: [https://github.com/clojure/core.match core.match].

For code and a thorough write-up on the red-black tree implementation that uses core.match, please read: [https://github.com/clojure-cookbook/clojure-cookbook/blob/master/02_composite-data/2-27_and_2-28_custom-data-structures/2-27_red-black-trees-part-i.asciidoc Clojure Cookbook - Data Structures: Red-Black Trees].


## Common Lisp


Common Lisp doesn't come with any pattern-matching solutions on its own, but with the help of its macro facility, it can incorporate features from other languages such as pattern matching. Macros expand into efficient code during compilation time and there isn't much difference if it's included in the core language or not. As has been said, Lisp is a ball of mud and remains one no matter what one throws at it.

This is a straighforward translation of the TCL solution. I don't know red-black-trees myself but I tried mirroring the original program as closely as possible. It uses a pattern-matching library called [http://www.takeda.tk/~sthalik/stuff/toadstool-current.tar toadstool].

```lisp
(mapc #'use-package '(#:toadstool #:toadstool-system))
(defstruct (red-black-tree (:constructor tree (color left val right)))
  color left val right)
 
(defcomponent tree (operator macro-mixin))
(defexpand tree (color left val right)
  `(class red-black-tree red-black-tree-color ,color
                         red-black-tree-left ,left
                         red-black-tree-val ,val
                         red-black-tree-right ,right))
(pushnew 'tree *used-components*)
 
(defun balance (color left val right)
  (toad-ecase (color left val right)
    (('black (tree 'red (tree 'red a x b) y c) z d)
     (tree 'red (tree 'black a x b) y
           (tree 'black c z d)))
    (('black (tree 'red a x (tree 'red b y c)) z d)
     (tree 'red (tree 'black a x b) y (tree 'black c z d)))
    (('black a x (tree 'red (tree 'red b y c) z d))
     (tree 'red (tree 'black a x b) y (tree 'black c z d)))
    (('black a x (tree 'red b y (tree 'red c z d)))
     (tree 'red (tree 'black a x b) y (tree 'black c z d)))
    ((color a x b)
     (tree color a x b))))
 
(defun %insert (x s)
  (toad-ecase1 s
    (nil (tree 'red nil x nil))
    ((tree color a y b)
     (cond ((< x y)
            (balance color (%insert x a) y b))
           ((> x y)
            (balance color a y (%insert x b)))
           (t s)))))
 
(defun insert (x s)
  (toad-ecase1 (%insert x s)
    ((tree t a y b) (tree 'black a y b))))
```


=={{Header|E}}==
In E, a pattern can be used almost anywhere a variable name can. Additionally, there are two operators used for pattern matching idioms: <tt>=~</tt> (returns success as a boolean, somewhat like Perl's <tt>=~</tt>), and <tt>switch</tt> (matches multiple patterns, like Haskell's <tt>case</tt>).

Both of those operators are defined in terms of the basic bind/match operation: <tt>def <var>pattern</var> exit <var>failure_handler</var> := <var>specimen</var></tt>

 def balance(tree) {
   return if (
     tree =~ term`tree(black, tree(red, tree(red, @a, @x, @b), @y, @c), @z, @d)` ||
     tree =~ term`tree(black, tree(red, @a, @x, tree(red, @b, @y, @c)), @z, @d)` ||
     tree =~ term`tree(black, @a, @x, tree(red, tree(red, @b, @y, @c), @z, @d))` ||
     tree =~ term`tree(black, @a, @x, tree(red, @b, @y, tree(red, @c, @z, @d)))`
   ) {
     term`tree(red, tree(black, $a, $x, $b), $y, tree(black, $c, $z, $d))`
   } else { tree }
 }
 def insert(elem, tree) {
   def ins(tree) {
     return switch (tree) {
       match term`empty` { term`tree(red, empty, $elem, empty)` }
       match term`tree(@color, @a, @y, @b)` {
         if (elem < y) {
           balance(term`tree($color, ${ins(a)}, $y, $b)`)
         } else if (elem > y) {
           balance(term`tree($color, $a, $y, ${ins(b)})`)
         } else {
           tree
         }
       }
     }
   }
   def term`tree(@_, @a, @y, @b)` := ins(tree)
   return term`tree(black, $a, $y, $b)`
 }

This code was tested by filling a tree with random values; you can try this at the E REPL:

 ? var tree := term`empty`
 > for _ in 1..20 {
 >   tree := insert(entropy.nextInt(100), tree)
 > }
 > tree


## EchoLisp


```scheme

;; code adapted from Racket and Common Lisp
;; Illustrates matching on structures
(require 'match)
(require 'struct)


(define (N-tostring n) (format "%s %d"  (N-color n) (N-value n)))
(struct N (color left value right) #:tostring N-tostring)

(define (balance t)
  (match t
    [(N '⚫️ (N '🔴 (N '🔴 a x b) y c) z d) (N '🔴 (N '⚫️ a x b) y (N '⚫️ c z d))]
    [(N '⚫️ (N '🔴 a x (N '🔴 b y c)) z d) (N '🔴 (N '⚫️ a x b) y (N '⚫️ c z d))]
    [(N '⚫️ a x (N '🔴 (N '🔴 b y c) z d)) (N '🔴 (N '⚫️ a x b) y (N '⚫️ c z d))]
    [(N '⚫️ a x (N '🔴 b y (N '🔴 c z d))) (N '🔴 (N '⚫️ a x b) y (N '⚫️ c z d))]
    [else t]))
    
    (define (ins value: x  tree: t)
    (match t
      ['empty (N '🔴 'empty x 'empty)]
      [(N c l v r) (cond [(< x v) (balance (N c (ins x l) v r))]
                         [(> x v) (balance (N c l v (ins x r)))]
                         [else t])]))
                         
	(define (insert value: x tree: s)
  		(match (ins x s) [(N _ l v r) (N '⚫️ l v r)]))
  	

```

```scheme

(define (t-show n (depth 0))
	(when (!eq? 'empty n)
		(t-show (N-left n) (+ 12 depth))
		(writeln (string-pad-left (format "%s" n ) depth))
		(t-show (N-right n) (+ 12 depth))))

(define T (for/fold [t 'empty] ([i 32]) (insert (random 100) t)))
(t-show T)

```

<small>

```txt


                                                       🔴 1    
                                           ⚫️ 2    
                               ⚫️ 7    
                                           ⚫️ 8    
                                                      🔴 11    
                  🔴 17    
                                          ⚫️ 25    
                              ⚫️ 28    
                                          ⚫️ 31    
      ⚫️ 32    
                              ⚫️ 36    
                  ⚫️ 40    
                              ⚫️ 43    
⚫️ 44    
                                          🔴 45    
                              ⚫️ 53    
                  ⚫️ 71    
                                          🔴 72    
                              ⚫️ 73    
      ⚫️ 83    
                              ⚫️ 89    
                                          🔴 91    
                  ⚫️ 92    
                                          🔴 94    
                              ⚫️ 99    

```

</small>


## Elixir

But, it changed an API into the Elixir style.

```elixir
defmodule RBtree do
  def find(nil, _), do: :not_found
  def find({ key, value, _, _, _ }, key), do: { :found, { key, value } }
  def find({ key1, _, _, left, _ }, key) when key < key1, do: find(left, key)
  def find({ key1, _, _, _, right }, key) when key > key1, do: find(right, key)
  
  def new(key, value), do: ins(nil, key, value) |> make_black
  
  def insert(tree, key, value), do: ins(tree, key, value) |> make_black
  
  defp ins(nil, key, value),
    do: { key, value, :r, nil, nil }
  defp ins({ key, _, color, left, right }, key, value),
    do: { key, value, color, left, right }
  defp ins({ ky, vy, cy, ly, ry }, key, value) when key < ky,
    do: balance({ ky, vy, cy, ins(ly, key, value), ry })
  defp ins({ ky, vy, cy, ly, ry }, key, value) when key > ky,
    do: balance({ ky, vy, cy, ly, ins(ry, key, value) })
  
  defp make_black({ key, value, _, left, right }),
    do: { key, value, :b, left, right }
  
  defp balance({ kx, vx, :b, lx, { ky, vy, :r, ly, { kz, vz, :r, lz, rz } } }),
    do: { ky, vy, :r, { kx, vx, :b, lx, ly }, { kz, vz, :b, lz, rz } }
  defp balance({ kx, vx, :b, lx, { ky, vy, :r, { kz, vz, :r, lz, rz }, ry } }),
    do: { kz, vz, :r, { kx, vx, :b, lx, lz }, { ky, vy, :b, rz, ry } }
  defp balance({ kx, vx, :b, { ky, vy, :r, { kz, vz, :r, lz, rz }, ry }, rx }),
    do: { ky, vy, :r, { kz, vz, :b, lz, rz }, { kx, vx, :b, ry, rx } }
  defp balance({ kx, vx, :b, { ky, vy, :r, ly, { kz, vz, :r, lz, rz } }, rx }),
    do: { kz, vz, :r, { ky, vy, :b, ly, lz }, { kx, vx, :b, rz, rx } }
  defp balance(t),
    do: t
end

RBtree.new(0,3)        |> IO.inspect
|> RBtree.insert(1,5)  |> IO.inspect
|> RBtree.insert(2,-1) |> IO.inspect
|> RBtree.insert(3,7)  |> IO.inspect
|> RBtree.insert(4,-3) |> IO.inspect
|> RBtree.insert(5,0)  |> IO.inspect
|> RBtree.insert(6,-1) |> IO.inspect
|> RBtree.insert(7,0)  |> IO.inspect
|> RBtree.find(4)      |> IO.inspect
```


```txt

{0, 3, :b, nil, nil}
{0, 3, :b, nil, {1, 5, :r, nil, nil}}
{1, 5, :b, {0, 3, :b, nil, nil}, {2, -1, :b, nil, nil}}
{1, 5, :b, {0, 3, :b, nil, nil}, {2, -1, :b, nil, {3, 7, :r, nil, nil}}}
{1, 5, :b, {0, 3, :b, nil, nil},
 {3, 7, :r, {2, -1, :b, nil, nil}, {4, -3, :b, nil, nil}}}
{1, 5, :b, {0, 3, :b, nil, nil},
 {3, 7, :r, {2, -1, :b, nil, nil}, {4, -3, :b, nil, {5, 0, :r, nil, nil}}}}
{3, 7, :b, {1, 5, :b, {0, 3, :b, nil, nil}, {2, -1, :b, nil, nil}},
 {5, 0, :b, {4, -3, :b, nil, nil}, {6, -1, :b, nil, nil}}}
{3, 7, :b, {1, 5, :b, {0, 3, :b, nil, nil}, {2, -1, :b, nil, nil}},
 {5, 0, :b, {4, -3, :b, nil, nil}, {6, -1, :b, nil, {7, 0, :r, nil, nil}}}}
{:found, {4, -3}}

```



## Emacs Lisp


The <code>pcase</code> syntax was added in Emacs 24.


```lisp

(defun rbt-balance (tree)
  (pcase tree
    (`(B (R (R ,a ,x ,b) ,y ,c) ,z ,d) `(R (B ,a ,x ,b) ,y (B ,c ,z ,d)))
    (`(B (R ,a ,x (R ,b ,y ,c)) ,z ,d) `(R (B ,a ,x ,b) ,y (B ,c ,z ,d)))
    (`(B ,a ,x (R (R ,b ,y ,c) ,z ,d)) `(R (B ,a ,x ,b) ,y (B ,c ,z ,d)))
    (`(B ,a ,x (R ,b ,y (R ,c ,z ,d))) `(R (B ,a ,x ,b) ,y (B ,c ,z ,d)))
    (_                                 tree)))

(defun rbt-insert- (x s)
  (pcase s
    (`nil              `(R nil ,x nil))
    (`(,color ,a ,y ,b) (cond ((< x y)
                               (rbt-balance `(,color ,(rbt-insert- x a) ,y ,b)))
                              ((> x y)
                               (rbt-balance `(,color ,a ,y ,(rbt-insert- x b))))
                              (t
                               s)))
    (_                  (error "Expected tree: %S" s))))

(defun rbt-insert (x s)
  (pcase (rbt-insert- x s)
    (`(,_ ,a ,y ,b) `(B ,a ,y ,b))
    (_              (error "Internal error: %S" s))))

(let ((s nil))
  (dotimes (i 16)
    (setq s (rbt-insert (1+ i) s)))
  (pp s))

```

Output:


```txt

(B
 (B
  (B
   (B nil 1 nil)
   2
   (B nil 3 nil))
  4
  (B
   (B nil 5 nil)
   6
   (B nil 7 nil)))
 8
 (B
  (B
   (B nil 9 nil)
   10
   (B nil 11 nil))
  12
  (B
   (B nil 13 nil)
   14
   (B nil 15
      (R nil 16 nil)))))

```



## Erlang


The code used here is extracted from [https://gist.github.com/mjn/2648040 Mark Northcott's GitHubGist]. 

```erlang

-module(rbtree).
-export([insert/3, find/2]).
 
% Node structure: { Key, Value, Color, Smaller, Bigger }
 
find(_, nil) ->
  not_found;
find(Key, { Key, Value, _, _, _ }) ->
  { found, { Key, Value } };
find(Key, { Key1, _, _, Left, _ }) when Key < Key1 ->
  find(Key, Left);
find(Key, { Key1, _, _, _, Right }) when Key > Key1 ->
  find(Key, Right).
 
insert(Key, Value, Tree) ->
  make_black(ins(Key, Value, Tree)).
  
ins(Key, Value, nil) ->
  { Key, Value, r, nil, nil };
ins(Key, Value, { Key, _, Color, Left, Right }) ->
  { Key, Value, Color, Left, Right };
ins(Key, Value, { Ky, Vy, Cy, Ly, Ry }) when Key < Ky ->
  balance({ Ky, Vy, Cy, ins(Key, Value, Ly), Ry });
ins(Key, Value, { Ky, Vy, Cy, Ly, Ry }) when Key > Ky ->
  balance({ Ky, Vy, Cy, Ly, ins(Key, Value, Ry) }).
  
make_black({ Key, Value, _, Left, Right }) ->
  { Key, Value, b, Left, Right }.
  
balance({ Kx, Vx, b, Lx, { Ky, Vy, r, Ly, { Kz, Vz, r, Lz, Rz } } }) ->
  { Ky, Vy, r, { Kx, Vx, b, Lx, Ly }, { Kz, Vz, b, Lz, Rz } };
balance({ Kx, Vx, b, Lx, { Ky, Vy, r, { Kz, Vz, r, Lz, Rz }, Ry } }) ->
  { Kz, Vz, r, { Kx, Vx, b, Lx, Lz }, { Ky, Vy, b, Rz, Ry } };
balance({ Kx, Vx, b, { Ky, Vy, r, { Kz, Vz, r, Lz, Rz }, Ry }, Rx }) ->
  { Ky, Vy, r, { Kz, Vz, b, Lz, Rz }, { Kx, Vx, b, Ry, Rx } };
balance({ Kx, Vx, b, { Ky, Vy, r, Ly, { Kz, Vz, r, Lz, Rz } }, Rx }) ->
  { Kz, Vz, r, { Ky, Vy, b, Ly, Lz }, { Kx, Vx, b, Rz, Rx } };
balance(T) ->
  T.

```


Output:

```txt

> rbtree:insert(0,3,nil).
{0,3,b,nil,nil}
> T1 = rbtree:insert(0,3,nil).
{0,3,b,nil,nil}
> T2 = rbtree:insert(1,5,T1). 
{0,3,b,nil,{1,5,r,nil,nil}}
> T3 = rbtree:insert(2,-1,T2).
{1,5,b,{0,3,b,nil,nil},{2,-1,b,nil,nil}}
> T4 = rbtree:insert(3,7,T3). 
{1,5,b,{0,3,b,nil,nil},{2,-1,b,nil,{3,7,r,nil,nil}}}
> T5 = rbtree:insert(4,-3,T4).
{1,5,b,
 {0,3,b,nil,nil},
 {3,7,r,{2,-1,b,nil,nil},{4,-3,b,nil,nil}}}
> T6 = rbtree:insert(5,0,T5). 
{1,5,b,
 {0,3,b,nil,nil},
 {3,7,r,{2,-1,b,nil,nil},{4,-3,b,nil,{5,0,r,nil,nil}}}}
> T7 = rbtree:insert(6,-1,T6).
{3,7,b,
 {1,5,b,{0,3,b,nil,nil},{2,-1,b,nil,nil}},
 {5,0,b,{4,-3,b,nil,nil},{6,-1,b,nil,nil}}}
> T8 = rbtree:insert(7,0,T7). 
{3,7,b,
 {1,5,b,{0,3,b,nil,nil},{2,-1,b,nil,nil}},
 {5,0,b,{4,-3,b,nil,nil},{6,-1,b,nil,{7,0,r,nil,nil}}}}
> rbtree:find(4,T8).
{found,{4,-3}}

```



## Go

Go doesn't have algebraic data types as such though they can simulated (to a limited extent) by interfaces. 

However, pattern matching on interfaces (via the type switch statement and type assertions) is limited to matching the implementing type and so the balance() method is not very pleasant.

```go
package main

import "fmt"

type Color string

const (
    R Color = "R"
    B       = "B"
)

type Tree interface {
    ins(x int) Tree
}

type E struct{}

func (_ E) ins(x int) Tree {
    return T{R, E{}, x, E{}}
}

func (_ E) String() string {
    return "E"
}

type T struct {
    cl Color
    le Tree
    aa int
    ri Tree
}

func (t T) balance() Tree {
    if t.cl != B {
        return t
    }
    le, leIsT := t.le.(T)
    ri, riIsT := t.ri.(T)
    var lele, leri, rile, riri T
    var leleIsT, leriIsT, rileIsT, ririIsT bool
    if leIsT {
        lele, leleIsT = le.le.(T)
    }
    if leIsT {
        leri, leriIsT = le.ri.(T)
    }
    if riIsT {
        rile, rileIsT = ri.le.(T)
    }
    if riIsT {
        riri, ririIsT = ri.ri.(T)
    }
    switch {
    case leIsT && leleIsT && le.cl == R && lele.cl == R:
        _, t2, z, d := t.destruct()
        _, t3, y, c := t2.(T).destruct()
        _, a, x, b := t3.(T).destruct()
        return T{R, T{B, a, x, b}, y, T{B, c, z, d}}
    case leIsT && leriIsT && le.cl == R && leri.cl == R:
        _, t2, z, d := t.destruct()
        _, a, x, t3 := t2.(T).destruct()
        _, b, y, c := t3.(T).destruct()
        return T{R, T{B, a, x, b}, y, T{B, c, z, d}}
    case riIsT && rileIsT && ri.cl == R && rile.cl == R:
        _, a, x, t2 := t.destruct()
        _, t3, z, d := t2.(T).destruct()
        _, b, y, c := t3.(T).destruct()
        return T{R, T{B, a, x, b}, y, T{B, c, z, d}}
    case riIsT && ririIsT && ri.cl == R && riri.cl == R:
        _, a, x, t2 := t.destruct()
        _, b, y, t3 := t2.(T).destruct()
        _, c, z, d := t3.(T).destruct()
        return T{R, T{B, a, x, b}, y, T{B, c, z, d}}
    default:
        return t
    }
}

func (t T) ins(x int) Tree {
    switch {
    case x < t.aa:
        return T{t.cl, t.le.ins(x), t.aa, t.ri}.balance()
    case x > t.aa:
        return T{t.cl, t.le, t.aa, t.ri.ins(x)}.balance()
    default:
        return t
    }
}

func (t T) destruct() (Color, Tree, int, Tree) {
    return t.cl, t.le, t.aa, t.ri
}

func (t T) String() string {
    return fmt.Sprintf("T(%s, %v, %d, %v)", t.cl, t.le, t.aa, t.ri)
}

func insert(tr Tree, x int) Tree {
    t := tr.ins(x)
    switch t.(type) {
    case T:
        tt := t.(T)
        _, a, y, b := tt.destruct()
        return T{B, a, y, b}
    case E:
        return E{}
    default:
        return nil
    }
}

func main() {
    var tr Tree = E{}
    for i := 1; i <= 16; i++ {
        tr = insert(tr, i)
    }
    fmt.Println(tr)
}
```


```txt

T(B, T(B, T(B, T(B, E, 1, E), 2, T(B, E, 3, E)), 4, T(B, T(B, E, 5, E), 6, T(B, E, 7, E))), 8, T(B, T(B, T(B, E, 9, E), 10, T(B, E, 11, E)), 12, T(B, T(B, E, 13, E), 14, T(B, E, 15, T(R, E, 16, E)))))

```



## Haskell



```haskell
data Color = R | B
data Tree a = E | T Color (Tree a) a (Tree a)

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c          ) z d                               = T R (T B a x b) y (T B c z d)
balance B (T R a           x (T R b y c)) z d                               = T R (T B a x b) y (T B c z d)
balance B a                               x (T R (T R b y c) z d          ) = T R (T B a x b) y (T B c z d)
balance B a                               x (T R b           y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance col a x b = T col a x b

insert :: Ord a => a -> Tree a -> Tree a
insert x s = T B a y b where
  ins E          =  T R E x E
  ins s@(T col a y b) 
    | x < y      =  balance col (ins a) y b
    | x > y      =  balance col a y (ins b)
    | otherwise  =  s
  T _ a y b = ins s
```



## J


J incorporates a symbol data type which, in versions 6.02 and 7.01, J implements directly as a red-black tree. The [http://www.jsoftware.com/docs/help701/dictionary/dsco.htm s: entry in the J dictionary] begins
''Symbols are a data type and are created by the verb s:. Symbols provide a mechanism for searching, sorting, and comparisons more efficient than alternative mechanisms such as boxed strings. Structural, selection, and relational verbs work on symbols. Arithmetic verbs do not work on symbols.''

The following code provides dictionary functionality using a red-black tree written in J without symbols.


```J

help=: noun define
red-black tree
Store dictionary in red-black tree.  The keys can be any noun.

Reference: 
Left-leaning Red-Black Trees
Robert Sedgewick
Department of Computer Science
Princeton University

verbs:
insert key;value  Inserts item into tree
delete key        Deletes item with key from tree
                  Deletion via the Sedgewick method is fairly simple.
                  However, I elected to remove the KEY;VALUE pair
                  rather than change the tree.
find key          Returns the associated definition or EMPTY
items any_noun    Returns all the items as a rank 1 array of KEY;VALUE pairs
keys any_noun     Returns all the keys as a rank 1 array of boxes
values any_noun   Returns all the values as a rank 1 array of boxes

J stores all data as arrays.
I chose to use array indexes to implement pointers.
An "index" is a rank 0 length 1 array.

Internal data structure:

T This rank 2 array stores indexes of left and right at each branch point.
C rank 1 array of node color.
H rank 1 array of the hash value of each key.
R rank 0 array stores the root index.
D rank 1 array of boxes.  In each box is a rank 2 array of key value
  pairs associated with the hash value.  Hash collision invokes direct
  lookup by key among the keys having same hash.

Additional test idea (done):
  Changing the hash to 0: or 2&| rapidly tests
  hash collision code for integer keys.
)

bitand=: (#. 1  0 0 0 1)b.
bitxor=: (#. 1  0 1 1 0)b.
hash=: [: ((4294967295) bitand (bitxor 1201&*))/ 846661 ,~ ,@:(a.&i.)@:":
NB. hash=: ] [ 1&bitand NB. can choose simple hash functions for tests

setup=: 3 : 0
T=: i. 0 2                              NB. Tree
H=: D=: C=: i. 0                        NB. Hashes, Data, Color
R=: _                                   NB. Root
'BLACK RED'=: i. 2
EMPTY
)

setup''

flipColors=: monad def 'C=: -.@:{`[`]}&C (, {&T) y'

3 : 0 'test flipColors'
DD=.D=: ,/<@:(;3j1&":)"0 i.3
TT=.T=: _ _,0 2,:_ _
CC=.C=: 1 0 1
RR=.R=: 1
HH=.H=: i.3
flipColors R
assert C -: -. CC
assert HH -: H
assert TT -: T
assert DD -: D
assert RR -: R
)

getColor=: monad def 'C ({~ :: (BLACK"_))"_ 0 y' NB. y the node

rotateTree=: dyad define                NB. x left or right, y node
I=. x <@:(, -.)~ y
X=. I { T                               NB. x = root.otherside
J=. X <@:, x
T=: (J { T) I} T
T=: y J} T
C=: y (RED ,~ {)`(X , [)`]} C
X
)

3 : 0 'test rotateTree'
DD=.D=:,/<@:(;3j1&":)"0 i.5
TT=.T=:_ _,0 2,_ _,1 4,:_ _
CC=.C=:0 1 0 0 0
R=:3
HH=.H=:i.5
assert R = rotateTree/0 1 , R
assert DD -: D
assert CC -: C
assert HH -: H
assert TT -: T
)

setup''

insert_privately=: adverb define
:
ROOT=. m
HASH=. x
ITEM=. y
if. _ -: ROOT do.                       NB. new key
 ROOT=. # H
 H=: H , HASH
 T=: T , _ _
 D=: D , < ,: , ITEM
 C=: C , RED
elseif. HASH = ROOT { H do.       NB. change a value or hash collision
 STACK=. ROOT >@:{ D
 I=. STACK i.&:({."1) ITEM
 STACK=. ITEM <@:(I}`,@.(I = #@])) STACK
 D=: STACK ROOT } D
elseif. do.          NB. Follow tree
 NB. if both children are red then flipColors ROOT
 flipColors^:((,~ RED) -: getColor@:({&T)) ROOT
 I=. <@:(, HASH > {&H) ROOT
 TEMP=. HASH (I { T) insert_privately y
 T=:  TEMP I } T
 NB.if (isRed(h.right) && !isRed(h.left)) h = rotateLeft(h)
 ROOT=. 0&rotateTree^:((BLACK,RED) -: getColor@:({&T)) ROOT
 NB.if (isRed(h.left) && isRed(h.left.left)) h = rotateRight(h)
 if. RED -: getColor {. ROOT { T do.
  if. (RED -: (getColor@:(([: {&T <@:,&0)^:2) :: (BLACK"_))) ROOT do.
   ROOT=. 1 rotateTree ROOT
  end.
 end.
end.
ROOT
)

insert=: monad define"1
assert 'boxed' -: datatype y
R=: (R insert_privately~ hash@:(0&{::)) y
C=: BLACK R } C
y
)

find_hash_index=: monad define          NB. y is the hash
if. 0 = # T do. '' return. end.         NB. follow the tree
I=. R                                   NB. instead of
while. y ~: I { H do.                   NB. direct search
 J=. <@:(, y > {&H) I
 if. _ > II=. J { T do. I=. II else. '' return. end.
end.
)

find=: monad define
if. '' -: I=. find_hash_index hash y do. EMPTY return. end.
LIST=. I {:: D
K=. {. |: LIST
LIST {::~ ::empty 1 ,~ K i. < y
)

delete=: 3 : 0
if. '' -: I=. find_hash_index hash y do. EMPTY return. end.
LIST=. I {:: D
K=. {. |: LIST
J=. K i. < y
RESULT=. J ({::~ ,&1)~ LIST
STACK=. J <@:({. , (}.~ >:)~) LIST
D=. LIST I } D
RESULT
)

getPathsToLeaves=: a:&$: : (4 : 0) NB. PATH getPathsToLeaves ROOT  use: getPathsToLeaves R
if. 0 = # y do. getPathsToLeaves R return. end.
PATH=. x ,&.> y
if. _ -: y do. return. end.
PATH getPathsToLeaves"0 y { T
)

check=: 3 : 0
COLORS=. getColor"0&.> a: -.~ ~. , getPathsToLeaves ''
result=. EMPTY
if. 0&e.@:(= {.) +/@:(BLACK&=)@>COLORS do. result=. result,<'mismatched black count' end.
if. 1 e. 1&e.@:(*. (= 1&|.))@:(RED&=)@>COLORS do. result=. result,<'successive reds' end.
>result
)

getPath=: 3 : 0                        NB. get path to y, the key
if. 0 = # H do. EMPTY return. end.
HASH=. hash y
PATH=. , I=. R
while. HASH ~: I { H do.
 J=. <@:(, HASH > {&H) I
 PATH=. PATH , II=. J { T
 if. _ > II do. I=. II else. EMPTY return. end.
end.
PATH
)

items=: 3 :';D'
keys=: 3 :'0{"1 items y'
values=: 3 :'1{"1 items y'

```

With use:

```J

   load'rb.ijs'
   NB. populate dictionary in random order with 999 key value pairs
   insert@:(; 6j1&":)"0@:?~ 999
   find 'the' NB. 'the' has no entry.
   find 239   NB. entry 239 has the anticipated formatted string value.
 239.0
   find 823823 NB. also no such entry
   NB.
   NB. tree passes the "no consecutive red" and "same number of black"
   NB. nodes to and including NULL leaves.
   check''  

```



## Julia

Julia's multiple dispatch model is based on the types of a function's arguments, but does not look deeper into the function's array arguments for the types of their contents. Therefore we do multi-dispatch on the balance function but then use an if statement within the multiply dispatched functions to further match based on argument vector contents.

```julia
import Base.length

abstract type AbstractColoredNode end

struct RedNode <: AbstractColoredNode end; const R = RedNode()
struct BlackNode <: AbstractColoredNode end; const B = BlackNode()
struct Empty end; const E = Empty()
length(e::Empty) = 1

function balance(b::BlackNode, v::Vector, z, d)
    if v[1] == R
        if length(v[2]) == 4 && v[2][1] == R
            return [R, [B, v[2][2], v[2][3], v[2][4]], v[3], [B, v[4], z, d]]
        elseif length(v[4]) == 4 && v[4][1] == R
            return [R, [B, v[2], v[3], v[4][2]], v[4][3], [B, v[4][4], z, d]]
        end
    end
    [b, v, z, d]
end

function balance(b::BlackNode, a, x, v::Vector)
    if v[1] == R
        if length(v[2]) == 4 && v[2][1] == R
            return [R, [B, a, x, v[2][2]], v[2][3], [B, v[2][4], v[3], v[4]]]
        elseif length(v[4]) == 4 && v[4][1] == R
            return [R, [B, a, x, v[2]], v[3], [B, v[4][2], v[4][3], v[4][4]]]
        end
    end
    [b, a, x, v]
end

function balance(b::BlackNode, a::Vector, x, v::Vector)
    if v[1] == R
        if length(v[2]) == 4 && v[2][1] == R
            return [R, [B, a, x, v[2][2]], v[2][3], [B, v[2][4], v[3], v[4]]]
        elseif length(v[4]) == 4 && v[4][1] == R
            return [R, [B, a, x, v[2]], v[3], [B, v[4][2], v[4][3], v[4][4]]]
        end
    end
    [b, a, x, v]
end

balance(node, l, a, r) = [node, l, a, r]

function ins(v::Vector, x::Number)
    if length(v) == 4
        if x < v[3]
            return balance(v[1], ins(v[2], x), v[3], v[4])
        elseif x > v[3]
            return balance(v[1], v[2], v[3], ins(v[4], x))
        end
    end
    v
end

ins(t, a) = [R, E, a, E]

insert(v, a) = (t = ins(v, a); t[1] = B; t)

function testRB()
    t = E
    for i in rand(collect(1:20), 10)
        t = insert(t, i)
    end
    println(replace(string(t), r"lackNode\(\)|edNode\(\)|Any|mpty\(\)" => ""))
end

testRB()

```
```txt

[B, [R, [B, [R, E, 1, E], 2, [R, E, 3, E]], 4, [B, E, 6, E]], 14, [B, E, 18, E]]]

```



## Kotlin

Whilst Kotlin supports algebraic data types (via 'sealed classes') and destructuring of data classes, pattern matching on them (via the 'when' expression) is currently limited to matching the type. Consequently the balance() function is not very pretty!
 

```scala
// version 1.1.51

import Color.*

enum class Color { R, B }

sealed class Tree<A : Comparable<A>> {

    fun insert(x: A): Tree<A> {
        val t = ins(x)
        return when (t) {
            is T -> {
                val (_, a, y, b) = t
                T(B, a, y, b)
            }

            is E -> E()
        }
    }

    abstract fun ins(x: A): Tree<A>
}

class E<A : Comparable<A>> : Tree<A>() {

    override fun ins(x: A): Tree<A> = T(R, E(), x, E())

    override fun toString() = "E"
}

data class T<A : Comparable<A>>(
    val cl: Color,
    val le: Tree<A>,
    val aa: A,
    val ri: Tree<A>
) : Tree<A>() {

    private fun balance(): Tree<A> {
        if (cl != B) return this
        val res =
            if (le is T && le.le is T && le.cl == R && le.le.cl == R) {
               val (_, t, z, d) = this
               val (_, t2, y, c) = t as T
               val (_, a, x, b) = t2 as T
               T(R, T(B, a, x, b), y, T(B, c, z, d))
            }
            else if (le is T && le.ri is T && le.cl == R && le.ri.cl == R) {
               val (_, t, z, d) = this
               val (_, a, x, t2) = t as T
               val (_, b, y, c) = t2 as T
               T(R, T(B, a, x, b), y, T(B, c, z, d))
            }
            else if (ri is T && ri.le is T && ri.cl == R && ri.le.cl == R) {
               val (_, a, x, t) = this
               val (_, t2, z, d) = t as T
               val (_, b, y, c) = t2 as T
               T(R, T(B, a, x, b), y, T(B, c, z, d))
            }
            else if (ri is T && ri.ri is T && ri.cl == R && ri.ri.cl == R) {
               val (_, a, x, t) = this
               val (_, b, y, t2) = t as T
               val (_, c, z, d) = t2 as T
               T(R, T(B, a, x, b), y, T(B, c, z, d))
            }
            else this
        return res
    }

    override fun ins(x: A): Tree<A> = when (x.compareTo(aa)) {
        -1   -> T(cl, le.ins(x), aa, ri).balance()
        +1   -> T(cl, le, aa, ri.ins(x)).balance()
        else -> this
    }

    override fun toString() = "T($cl, $le, $aa, $ri)"
}

fun main(args: Array<String>) {
    var tree: Tree<Int> = E()
    for (i in 1..16) {
        tree = tree.insert(i)
    }
    println(tree)
}
```


```txt

T(B, T(B, T(B, T(B, E, 1, E), 2, T(B, E, 3, E)), 4, T(B, T(B, E, 5, E), 6, T(B, E, 7, E))), 8, T(B, T(B, T(B, E, 9, E), 10, T(B, E, 11, E)), 12, T(B, T(B, E, 13, E), 14, T(B, E, 15, T(R, E, 16, E)))))

```



## OCaml


```ocaml

type color = R | B
type 'a tree = E | T of color * 'a tree * 'a * 'a tree

(** val balance : color * 'a tree * 'a * 'a tree -> 'a tree *)
let balance = function
  | B, T (R, T (R,a,x,b), y, c), z, d
  | B, T (R, a, x, T (R,b,y,c)), z, d
  | B, a, x, T (R, T (R,b,y,c), z, d)
  | B, a, x, T (R, b, y, T (R,c,z,d)) -> T (R, T (B,a,x,b), y, T (B,c,z,d))
  | col, a, x, b                      -> T (col, a, x, b) 

(** val insert : 'a -> 'a tree -> 'a tree *)
let insert x s = 
  let rec ins = function
    | E                  -> T (R,E,x,E)
    | T (col,a,y,b) as s ->
	if x < y then
	  balance (col, ins a, y, b)
	else if x > y then
	  balance (col, a, y, ins b)
	else
	  s
  in let T (_,a,y,b) = ins s 
  in T (B,a,y,b)

```




## Oz

Unlike Haskell, Oz does not support multiple equations per function. So we use an explicit case-statement.
To match multiple variables at once, we create temporary tuples with "#".


```oz
fun {Balance Col A X B}
   case Col#A#X#B
   of b#t(r t(r A X B) Y C         )#Z#D                            then t(r t(b A X B) Y t(b C Z D))
   [] b#t(r A          X t(r B Y C))#Z#D                            then t(r t(b A X B) Y t(b C Z D))
   [] b#A                           #X#t(r t(r B Y C) Z D)          then t(r t(b A X B) Y t(b C Z D))
   [] b#A                           #X#t(r B          Y t(r C Z D)) then t(r t(b A X B) Y t(b C Z D))
   else t(Col A X B)
   end
end

fun {Insert X S}
   fun {Ins S}
      case S of e then t(r e X e)
      [] t(Col A Y B) then
	 if X < Y then {Balance Col {Ins A} Y B}
	 elseif X > Y then {Balance Col A Y {Ins B}}
	 else S
	 end
      end
   end
   t(_ A Y B) = {Ins S}
in
   t(b A Y B)
end
```


## Perl

Although Perl does not have algebraic data types, it does have a wonderfully flexible regular expression engine, which is powerfully enough to perform the task.

However, representing a tree as a string, and repeatedly parsing that string, is truly inefficient way to solve the problem.  Someday, someone will write a perl multi-method-dispatch module which is as amazing as perl6's, and then we can copy the perl6 solution here.

The $balanced variable matches against either some data, or the empty tree (_), or, using perl's amazing recursive regular expression feature, a non-empty tree.

Each of the single letter variables declared right after $balanced, match an instance of $balanced, and if they succeed, store the result into the %+ hash.


```perl
#!perl
use 5.010;
use strict;
use warnings qw(FATAL all);

my $balanced = qr{([^<>,]++|<(?-1),(?-1),(?-1),(?-1)>)};
my ($a, $b, $c, $d, $x, $y, $z) = map +qr((?<$_>$balanced)),
	'a'..'d', 'x'..'z';
my $col = qr{(?<col>[RB])};

sub balance {
	local $_ = shift;
	if( /^<B,<R,<R,$a,$x,$b>,$y,$c>,$z,$d>\z/ or
		/^<B,<R,$a,$x,<R,$b,$y,$c>>,$z,$d>\z/ or
		/^<B,$a,$x,<R,<R,$b,$y,$c>,$z,$d>>\z/ or
		/^<B,$a,$x,<R,$b,$y,<R,$c,$z,$d>>>\z/ )
	{
		my ($aa, $bb, $cc, $dd) = @+{'a'..'d'};
		my ($xx, $yy, $zz) = @+{'x'..'z'};
		"<R,<B,$aa,$xx,$bb>,$yy,<B,$cc,$zz,$dd>>";
	} else {
		$_;
	}
}

sub ins {
	my ($xx, $tree) = @_;
	if($tree =~ m{^<$col,$a,$y,$b>\z} ) {
		my ($color, $aa, $bb, $yy) = @+{qw(col a b y)};
		if( $xx < $yy ) {
			return balance "<$color,".ins($xx,$aa).",$yy,$bb>";
		} elsif( $xx > $yy ) {
			return balance "<$color,$aa,$yy,".ins($xx,$bb).">";
		} else {
			return $tree;
		}
	} elsif( $tree !~ /,/) {
		return "<R,_,$xx,_>";
	} else {
		print "Unexpected failure!\n";
		print "Tree parts are: \n";
		print $_, "\n" for $tree =~ /$balanced/g;
		exit;
	}
}

sub insert {
	my $tree = ins(@_);
	$tree =~ m{^<$col,$a,$y,$b>\z} or die;
	"<B,$+{a},$+{y},$+{b}>";
}

MAIN: {
	my @a = 1..10;
	for my $aa ( 1 .. $#a ) {
		my $bb = int rand( 1 + $aa );
		@a[$aa, $bb] = @a[$bb, $aa];
	}
	my $t = "!";
	for( @a ) {
		$t = insert( $_, $t );
		print "Tree: $t.\n";
	}
}
print "Done\n";

```

```txt
Tree: <B,_,9,_>.
Tree: <B,<R,_,7,_>,9,_>.
Tree: <B,<B,_,2,_>,7,<B,_,9,_>>.
Tree: <B,<B,_,2,<R,_,6,_>>,7,<B,_,9,_>>.
Tree: <B,<B,_,2,<R,_,6,_>>,7,<B,_,9,<R,_,10,_>>>.
Tree: <B,<R,<B,_,2,_>,5,<B,_,6,_>>,7,<B,_,9,<R,_,10,_>>>.
Tree: <B,<R,<B,_,2,<R,_,4,_>>,5,<B,_,6,_>>,7,<B,_,9,<R,_,10,_>>>.
Tree: <B,<R,<B,_,2,<R,_,4,_>>,5,<B,_,6,_>>,7,<B,<R,_,8,_>,9,<R,_,10,_>>>.
Tree: <B,<R,<B,<R,_,1,_>,2,<R,_,4,_>>,5,<B,_,6,_>>,7,<B,<R,_,8,_>,9,<R,_,10,_>>>.
Tree: <B,<B,<B,<R,_,1,_>,2,_>,3,<B,_,4,_>>,5,<B,<B,_,6,_>,7,<B,<R,_,8,_>,9,<R,_,10,_>>>>.
Done
```



## Perl 6

Perl 6 doesn't have algebraic data types (yet), but it does have pretty good pattern matching in multi signatures.

```perl6
enum RedBlack <R B>
;

multi balance(B,[R,[R,$a,$x,$b],$y,$c],$z,$d) { [R,[B,$a,$x,$b],$y,[B,$c,$z,$d]] }
multi balance(B,[R,$a,$x,[R,$b,$y,$c]],$z,$d) { [R,[B,$a,$x,$b],$y,[B,$c,$z,$d]] }
multi balance(B,$a,$x,[R,[R,$b,$y,$c],$z,$d]) { [R,[B,$a,$x,$b],$y,[B,$c,$z,$d]] }
multi balance(B,$a,$x,[R,$b,$y,[R,$c,$z,$d]]) { [R,[B,$a,$x,$b],$y,[B,$c,$z,$d]] }

multi balance($col, $a, $x, $b) { [$col, $a, $x, $b] }
 
multi ins( $x, @s [$col, $a, $y, $b] ) {
    when $x before $y     { balance $col, ins($x, $a), $y, $b }
    when $x after $y      { balance $col, $a, $y, ins($x, $b) }
    default               { @s }
}
multi ins( $x, Any:U ) { [R, Any, $x, Any] }

multi insert( $x, $s ) {
    [B, |ins($x,$s)[1..3]];
}

sub MAIN {
    my $t = Any;
    $t = insert($_, $t) for (1..10).pick(*);
    say $t.gist;
}
```

This code uses generic comparison operators <tt>before</tt> and <tt>after</tt>, so it should work on any ordered type.
```txt
[B [B [B (Any) 1 [R (Any) 2 (Any)]] 3 [B (Any) 4 [R (Any) 5 (Any)]]] 6 [B [B (Any) 7 (Any)] 8 [B [R (Any) 9 (Any)] 10 (Any)]]]
```



## Phix

There is no formal support for this sort of thing in Phix, but that's not to say that whipping
something up is likely to be particularly difficult, so let's give it a whirl.

Uses a slightly tweaked version of [[Visualize_a_tree#Phix|Visualize_a_tree]], for the full runnable code
see demo\rosetta\Pattern_matching.exw (shipped with 0.8.0+).

First, imagine the following is in say algebraic_data_types.e. It is not quite generic enough,
and there are too many little fudges, such as that "and not string(ki)", and the use of 0 for 
the "any value", and {} to indicate failure, for it to end up in builtins\ as-is, but not exactly 
difficult to copy/maintain on a per-project basis.

```Phix
function match_one(sequence key, object t)
    sequence res = {}
    if sequence(t)
    and length(key)==length(t) then
        for i=1 to length(key) do
            object ki = key[i], ti = t[i]
            if sequence(ki) and not string(ki) then
                sequence r2 = match_one(ki,ti)
                if r2={} then res = {} exit end if
                res &= r2
            else
                if ki=0 then
                    res = append(res,ti)
                else
                    if ki!=ti then res = {} exit end if
                end if
            end if
        end for
    end if
    return res
end function

/*global*/ function match_algebraic(sequence set, t)
    sequence s
    for i=1 to length(set) do
        s = match_one(set[i],t)
        if length(s) then exit end if
    end for
    return s
end function
```

Then we can code something like this (with include algebraic_data_types.e)

```Phix
constant B = "B", R = "R"

function balance(sequence t)
    sequence s = match_algebraic({{B,{R,{R,0,0,0},0,0},0,0},
                                  {B,{R,0,0,{R,0,0,0}},0,0},
                                  {B,0,0,{R,{R,0,0,0},0,0}},
                                  {B,0,0,{R,0,0,{R,0,0,0}}}},t)
    if length(s) then
        object {a,x,b,y,c,z,d} = s
        t = {R,{B,a,x,b},y,{B,c,z,d}}
    end if
    return t
end function

function ins(object tree, object leaf)
    if tree=NULL then
        tree = {R,NULL,leaf,NULL}
    else
        object {c,l,k,r} = tree
        if leaf!=k then
            if leaf<k then l = ins(l,leaf)
                      else r = ins(r,leaf)
            end if
            tree = balance({c,l,k,r})
        end if
    end if
    return tree
end function

function tree_insert(object tree, object leaf)
    tree = ins(tree,leaf)
    tree[1] = B
    return tree
end function

sequence stuff = shuffle(tagset(10))
object tree = NULL
for i=1 to length(stuff) do
    tree = tree_insert(tree,stuff[i])
end for
visualise_tree(tree)
```

```txt

   ┌R1
  ┌B2
 ┌B3
 │└B4
─B5
 │┌B6
 ││└R7
 └B8
  └B9
   └R10

```



## PicoLisp

```PicoLisp
(be color (R))
(be color (B))

(be tree (@ E))
(be tree (@P (T @C @L @X @R))
   (color @C)
   (tree @P @L)
   (call @P @X)
   (tree @P @R) )

(be bal (B (T R (T R @A @X @B) @Y @C) @Z @D (T R (T B @A @X @B) @Y (T B @C @Z @D))))
(be bal (B (T R @A @X (T R @B @Y @C)) @Z @D (T R (T B @A @X @B) @Y (T B @C @Z @D))))
(be bal (B @A @X (T R (T R @B @Y @C) @Z @D) (T R (T B @A @X @B) @Y (T B @C @Z @D))))
(be bal (B @A @X (T R @B @Y (T R @C @Z @D)) (T R (T B @A @X @B) @Y (T B @C @Z @D))))

(be balance (@C @A @X @B @S)
   (bal @C @A @X @B @S)
   T )
(be balance (@C @A @X @B (T @C @A @X @B)))

(be ins (@X E (T R E @X E)))
(be ins (@X (T @C @A @Y @B) @R)
   (^ @ (> (-> @Y) (-> @X)))
   (ins @X @A @Ao)
   (balance @C @Ao @Y @B @R)
   T )
(be ins (@X (T @C @A @Y @B) @R)
   (^ @ (> (-> @X) (-> @Y)))
   (ins @X @B @Bo)
   (balance @C @A @Y @Bo @R)
   T )
(be ins (@X (T @C @A @Y @B) (T @C @A @Y @B)))

(be insert (@X @S (T B @A @Y @B))
   (ins @X @S (T @ @A @Y @B)) )
```

Test:

```PicoLisp
: (? (insert 2 E @A) (insert 1 @A @B) (insert 3 @B @C))
 @A=(T B E 2 E) @B=(T B (T R E 1 E) 2 E) @C=(T B (T R E 1 E) 2 (T R E 3 E))
-> NIL
```



## Prolog


```txt

color(r).
color(b).

tree(_,e).
tree(P,t(C,L,X,R)) :- color(C), tree(P,L), call(P,X), tree(P,R).

bal(b, t(r,t(r,A,X,B),Y,C), Z, D, t(r,t(b,A,X,B),Y,t(b,C,Z,D))).
bal(b, t(r,A,X,t(r,B,Y,C)), Z, D, t(r,t(b,A,X,B),Y,t(b,C,Z,D))).
bal(b, A, X, t(r,t(r,B,Y,C),Z,D), t(r,t(b,A,X,B),Y,t(b,C,Z,D))).
bal(b, A, X, t(r,B,Y,t(r,C,Z,D)), t(r,t(b,A,X,B),Y,t(b,C,Z,D))).

balance(C,A,X,B,S) :- ( bal(C,A,X,B,T) -> S = T ; S = t(C,A,X,B) ).

ins(X,e,t(r,e,X,e)).
ins(X,t(C,A,Y,B),R) :- ( X < Y -> ins(X,A,Ao), balance(C,Ao,Y,B,R)
                       ; X > Y -> ins(X,B,Bo), balance(C,A,Y,Bo,R)
                       ; X = Y -> R = t(C,A,Y,B)
                       ).

insert(X,S,t(b,A,Y,B)) :- ins(X,S,t(_,A,Y,B)).

```



## Racket

```racket

#lang racket

;; Using short names to make the code line up nicely
(struct N (color left value right) #:prefab)

(define (balance t)
  (match t
    [(N 'B (N 'R (N 'R a x b) y c) z d) (N 'R (N 'B a x b) y (N 'B c z d))]
    [(N 'B (N 'R a x (N 'R b y c)) z d) (N 'R (N 'B a x b) y (N 'B c z d))]
    [(N 'B a x (N 'R (N 'R b y c) z d)) (N 'R (N 'B a x b) y (N 'B c z d))]
    [(N 'B a x (N 'R b y (N 'R c z d))) (N 'R (N 'B a x b) y (N 'B c z d))]
    [else t]))

(define (insert x s)
  (define (ins t)
    (match t
      ['empty (N 'R 'empty x 'empty)]
      [(N c l v r) (cond [(< x v) (balance (N c (ins l) v r))]
                         [(> x v) (balance (N c l v (ins r)))]
                         [else t])]))
  (match (ins s) [(N _ l v r) (N 'B l v r)]))

(define (visualize t0)
  (let loop ([t t0] [last? #t] [indent '()])
    (define (I mid last) (cond [(eq? t t0) ""] [last? mid] [else last]))
    (for-each display (reverse indent))
    (printf "~a~a[~a]\n" (I "\\-" "+-") (N-value t) (N-color t))
    (define subs (filter N? (list (N-left t) (N-right t))))
    (for ([s subs] [n (in-range (sub1 (length subs)) -1 -1)])
      (loop s (zero? n) (cons (I "  " "| ") indent)))))

(visualize (for/fold ([t 'empty]) ([i 16]) (insert i t)))

```



```txt

7[B]
+-3[B]
| +-1[B]
| | +-0[B]
| | \-2[B]
| \-5[B]
|   +-4[B]
|   \-6[B]
\-11[B]
  +-9[B]
  | +-8[B]
  | \-10[B]
  \-13[B]
    +-12[B]
    \-14[B]
      \-15[R]

```



## Rascal


Rascal offers many options for pattern matching. In essence, there are four sorts of patterns: Abstract, Concrete, PatternWithAction and classic Regular Expressions. These patterns can be used in several cases, for example switch or visit statements, on the right of the Match operator (:=), or in TryCatch statements for thrown exceptions. Each pattern binds variables in a conditional scope.


### Abstract


An abstract pattern is recursively defined and may contain, among others, the following elements: Literal, VariableDeclaration, MultiVariable, Variable, List, Set, Tuple, Node, Descendant, Labelled, TypedLabelled, TypeConstrained. More explanation can be found in the [http://http://tutor.rascal-mpl.org/Courses/Rascal/Rascal.html#/Courses/Rascal/Patterns/Abstract/Abstract.html Documentation]. Some examples:

```rascal

// Literal
rascal>123 := 123
bool: true

// VariableDeclaration
rascal>if(str S := "abc")
>>>>>>>   println("Match succeeds, S == \"<S>\"");
Match succeeds, S == "abc"
ok

// MultiVariable
rascal>if([10, N*, 50] := [10, 20, 30, 40, 50])
>>>>>>>   println("Match succeeds, N == <N>");
Match succeeds, N == [20,30,40]
ok

// Variable
rascal>N = 10;
int: 10
rascal>N := 10;
bool: true
rascal>N := 20;
bool: false

// Set and List
rascal>if({10, set[int] S, 50} := {50, 40, 30, 20, 10})
>>>>>>>   println("Match succeeded, S = <S>");
Match succeeded, S = {30,40,20}
ok

rascal>for([L1*, L2*] := [10, 20, 30, 40, 50]) 
>>>>>>>    println("<L1> and <L2>");
[] and [10,20,30,40,50]
[10] and [20,30,40,50]
[10,20] and [30,40,50]
[10,20,30] and [40,50]
[10,20,30,40] and [50]
[10,20,30,40,50] and []
list[void]: []

// Descendant
rascal>T = red(red(black(leaf(1), leaf(2)), black(leaf(3), leaf(4))), black(leaf(5), leaf(4)));
rascal>for(/black(_,leaf(4)) := T)
>>>>>>>    println("Match!");
Match!
Match!
list[void]: []

rascal>for(/black(_,leaf(int N)) := T)
>>>>>>>    println("Match <N>");
Match 2
Match 4
Match 4
list[void]: []

rascal>for(/int N := T)
>>>>>>>    append N;
list[int]: [1,2,3,4,5,4]

// Labelled
rascal>for(/M:black(_,leaf(4)) := T)
>>>>>>>    println("Match <M>");
Match black(leaf(3),leaf(4))
Match black(leaf(5),leaf(4))
list[void]: []
```



### Concrete


Suppose we want to manipulate text written in some hypothetical language LANG. Then first the concrete syntax of LANG has to be defined by importing a module that declares the non-terminals and syntax rules for LANG. Next LANG programs have to be parsed. LANG programs made come from text files or they may be included in the Rascal program as literals. In both cases the text is parsed according to the defined syntax and the result is a parse tree in the form of a value of type Tree. Concrete patterns operate on these trees. 

A concrete pattern is a quoted concrete syntax fragment that may contain variables. The syntax that is used to parse the concrete pattern may come from any module that has been imported in the module in which the concrete pattern occurs. Some examples of concrete patterns:

```rascal
// Quoted pattern 
` Token1 Token2 ... Tokenn `
// A typed quoted pattern 
(Symbol) ` Token1 Token2 ... TokenN `
// A typed variable pattern 
<Type Var>
// A variable pattern 
<Var>
```


A full example of concrete patterns can be found in the [http://tutor.rascal-mpl.org/Courses/Recipes/Languages/Exp/Concrete/WithLayout/WithLayout.html Rascal Recipes].


### PatternWithAction


There are two variants of the PatternsWitchAction. When the subject matches Pattern, the expression Exp is evaluated and the subject is replaced with the result. Secondly, when the subject matches Pattern, the (block of) Statement(s) is executed. See below for some ColoredTree examples:


```rascal
// Define ColoredTrees with red and black nodes and integer leaves
data ColoredTree = leaf(int N)      
                 | red(ColoredTree left, ColoredTree right) 
                 | black(ColoredTree left, ColoredTree right);
          
// Count the number of black nodes        
public int cntBlack(ColoredTree t){
   int c = 0;
   visit(t) {
     case black(_,_): c += 1;      
   };
   return c;
}

// Returns if a tree is balanced
public bool balance(ColoredTree t){
   visit(t){
     case black(a,b): if (cntBlack(a) == cntBlack(b)) true; else return false;
     case red(a,b): if (cntBlack(a) == cntBlack(b)) true; else return false;
     }
   return true;
}
// Compute the sum of all integer leaves
public int addLeaves(ColoredTree t){
   int c = 0;
   visit(t) {
     case leaf(int N): c += N;   
   };
   return c;
}

// Add green nodes to ColoredTree
data ColoredTree = green(ColoredTree left, ColoredTree right); 

// Transform red nodes into green nodes
public ColoredTree makeGreen(ColoredTree t){
   return visit(t) {
     case red(l, r) => green(l, r)   
   };
}
```



### Regular Expressions


Regular expressions are noated between two slashes. Most normal regular expressions patterns are available, such as ., \n, \d, etc. Additionally, flags can be used to create case intensiveness.


```rascal
rascal>
/XX/i := "some xx";
bool: true
rascal>/a.c/ := "abc";
bool: true
```



## REXX

The nodes used for this example are taken from the Wikipedia example at:   
[[https://en.wikipedia.org/wiki/Red%E2%80%93black_tree#/media/File:Red-black_tree_example.svg red black tree, an example]]

```rexx
/*REXX pgm builds a red/black tree (with verification & validation), balances as needed.*/
parse arg nodes '/' insert                       /*obtain optional arguments from the CL*/
if  nodes=''  then nodes =  13.8.17  8.1.11  17.15.25  1.6  25.22.27    /*default nodes.*/
if insert=''  then insert=  22.44    44.66                              /*   "   inserts*/
top=.
call Dnodes nodes                                /*define nodes, balance them as added. */
call Dnodes insert                               /*insert nodes, balance them as needed.*/
call Lnodes                                      /*list the nodes  (with indentation).  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Dnodes: arg $d;    do j=1  for words($d);   t=word($d, j)     /*color: encoded into LEV.*/
                   parse var  t   p   '.'   a   "."   b   '.'   x   1  .  .  .  xx
                   call Vnodes p a b
                   if x\==''   then call err "too many nodes specified: "   xx
                   if p\==top  then if @.p==.  then call err "node isn't defined: "  p
                   if p ==top  then do;  !.p=1;  L.1=p;  end  /*assign the top node.    */
                   @.p=a b;    n=!.p + 1                      /*assign node; bump level.*/
                   if a\==''   then do;  !.a=n;   @.a=;   maxL=max(maxL, !.a);   end
                   if b\==''   then do;  !.b=n;   @.b=;   maxL=max(maxL, !.b);   end
                   L.n=space(L.n a b)                         /*append to the level list*/
                   end   /*j*/
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
err:    say;          say '***error***: '   arg(1);           say;              exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/
Lnodes:            do L=1  for  maxL;  w=length(maxL);  rb=word('(red) (black)', 1 + L//2)
                   say "level:"   right(L, w)   left('', L+L)   " ───► "   rb    ' '   L.L
                   end   /*lev*/
        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
Vnodes: arg $v;    do v=1  for words($v);   y=word($v, v)
                   if \datatype(y, 'W')   then call err "node isn't a whole number: "    y
                   y=y/1                         /*normalize the  Y  integer: no LZ, dot*/
                   if top==.  then do;  LO=y;   top=y;  HI=y;   L.=;   @.=;   maxL=1;  end
                                        LO=min(LO, y);  HI=max(HI, y)
                   if @.y\==. & @.y\==''  then call err "node is already defined: "      y
                   end   /*v*/
        return
```
 
```txt

level: 1     ───►  (black)   13
level: 2       ───►  (red)   8 17
level: 3         ───►  (black)   1 11 15 25
level: 4           ───►  (red)   6 22 27
level: 5             ───►  (black)   44
level: 6               ───►  (red)   66

```



## Rust

This would be a horribly inefficient way to implement a Red-Black Tree in Rust as nodes are being allocated and deallocated constantly, but it does show off Rust's pattern matching.

```rust
#![feature(box_patterns, box_syntax)]
use self::Color::*;
use std::cmp::Ordering::*;

enum Color {R,B}

type Link<T> = Option<Box<N<T>>>;
struct N<T> {
    c: Color,
    l: Link<T>,
    val: T,
    r: Link<T>,
}


impl<T: Ord> N<T> {
    fn balance(col: Color, n1: Link<T>, z: T, n2: Link<T>) -> Link<T> {
        Some(box 
             match (col,n1,n2) {
                   (B, Some(box N {c: R, l: Some(box N {c: R, l: a, val: x, r: b}), val: y, r: c}), d)
                |  (B, Some(box N {c: R, l: a, val: x, r: Some (box N {c: R, l: b, val: y, r: c})}), d)
                => N {c: R, l: Some(box N {c: B, l: a, val: x, r: b}), val: y, r: Some(box N {c: B, l: c, val: z, r: d})},
                   (B, a, Some(box N {c: R, l: Some(box N {c: R, l: b, val: y, r: c}), val: v, r: d}))
                |  (B, a, Some(box N {c: R, l: b, val: y, r: Some(box N {c: R, l: c, val: v, r: d})}))
                => N {c: R, l: Some(box N {c: B, l: a, val: z, r: b}), val: y, r: Some(box N {c: B, l: c, val: v, r: d})},
                (col, a, b) => N {c: col, l: a, val: z, r: b},
        })
    }
    fn insert(x: T, n: Link<T>) -> Link<T> {
        match n {
            None => Some(box N { c: R, l: None, val: x, r: None }),
            Some(n) =>  {
                let n = *n;
                let N {c: col, l: a, val: y, r: b} = n;
                match x.cmp(&y) {
                    Greater => Self::balance(col, a,y,Self::insert(x,b)),
                    Less    => Self::balance(col, Self::insert(x,a),y,b),
                    Equal   => Some(box N {c: col, l: a, val: y, r: b})
                }
            }
        }
    }
}
```



## Scala

Algebraic data types are implemented in Scala through the combination of a number of
different features, to ensure principles of Object Oriented Programming.

The main type
is usually defined as a <code>sealed abstract class</code>, which ensures it can't be
instantiated, and guarantees that it can't be expanded outside the file it was defined
at. This last feature is used so the compiler can verify that the pattern matching is
complete, or warn when there are missing cases. It can be ommitted if preferred.

Each subtype is defined either as a <code>case object</code>, for non-paremeterized types,
or <code>case class</code>, for parameterized types, all extending the main type. The
<code>case</code> keyword is not required, but, when used, it provides a number of default
methods which ensure they can be used without any further definitions.

The most important of those default methods for the purpose of algebraic data types is the
''extractor'', a method called either <code>unapply</code> or <code>unapplySeq</code>, and
which returns an <code>Option</code> containing the deconstructed parameters, or <code>None</code>
if the passed object can't be deconstructed by this method. Scala uses the extractors to
implement pattern matching without exposing the internal representation of the data.

This specific task is made much harder than necessary because Scala doesn't have a variant
ordering class. Given that limitation, one has to either give up on a singleton object representing
the empty tree, or give up on parameterizing the tree itself.

The solution below, uses the latter approach. The algebraic data types are members of a <code>RedBlackTree</code>
class, which, itself, receives a type parameter for the keys of the tree, and an implicit
parameter for an <code>Ordering</code> for that type. To use the tree it is thus necessary
to instantiate an object of type <code>RedBlackTree</code>, and then reference the members
of that object.


```scala
class RedBlackTree[A](implicit ord: Ordering[A]) {
  sealed abstract class Color
  case object R extends Color
  case object B extends Color
  
  sealed abstract class Tree {
    def insert(x: A): Tree = ins(x) match {
      case T(_, a, y, b) => T(B, a, y, b)
      case E             => E
    }
    def ins(x: A): Tree
  }
  
  case object E extends Tree {
    override def ins(x: A): Tree = T(R, E, x, E) 
  }
  
  case class T(c: Color, left: Tree, a: A, right: Tree) extends Tree {
    private def balance: Tree = (c, left, a, right) match {
      case (B, T(R, T(R, a, x, b), y, c),             z, d                                    ) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case (B, T(R, a,             x, T(R, b, y, c)), z, d                                    ) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case (B, a,                                     x, T(R, T(R, b, y, c), z, d            )) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case (B, a,                                     x, T(R, b,             y, T(R, c, z, d))) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      case _ => this
    }
    
    override def ins(x: A): Tree = ord.compare(x, a) match {
      case -1 => T(c, left ins x, a, right      ).balance
      case  1 => T(c, left,       a, right ins x).balance
      case  0 => this
    }
  }
}
```


Usage example:


```txt

scala> val rbt = new RedBlackTree[Int]
rbt: RedBlackTree[Int] = RedBlackTree@17dfcf1

scala> import rbt._
import rbt._

scala> List.range(1, 17).foldLeft(E: Tree)(_ insert _)
res5: rbt.Tree = T(B,T(B,T(B,T(B,E,1,E),2,T(B,E,3,E)),4,T(B,T(B,E,5,E),6,T(B,E,7,E))),8,T(B,T(B,T(B,E,9,E),10,T(B,E,11,E
)),12,T(B,T(B,E,13,E),14,T(B,E,15,T(R,E,16,E)))))

```



## Standard ML


```sml

datatype color = R | B
datatype 'a tree = E | T of color * 'a tree * 'a * 'a tree

(** val balance = fn : color * 'a tree * 'a * 'a tree -> 'a tree *)
fun balance (B, T (R, T (R,a,x,b), y, c), z, d) = T (R, T (B,a,x,b), y, T (B,c,z,d))
  | balance (B, T (R, a, x, T (R,b,y,c)), z, d) = T (R, T (B,a,x,b), y, T (B,c,z,d))
  | balance (B, a, x, T (R, T (R,b,y,c), z, d)) = T (R, T (B,a,x,b), y, T (B,c,z,d))
  | balance (B, a, x, T (R, b, y, T (R,c,z,d))) = T (R, T (B,a,x,b), y, T (B,c,z,d))
  | balance (col, a, x, b)                      = T (col, a, x, b) 

(** val insert = fn : int -> int tree -> int tree *)
fun insert x s = let
  fun ins E                    = T (R,E,x,E)
    | ins (s as T (col,a,y,b)) =
	if x < y then
	  balance (col, ins a, y, b)
	else if x > y then
	  balance (col, a, y, ins b)
	else
	  s
  val T (_,a,y,b) = ins s 
in
  T (B,a,y,b)
end

```



## Swift

```swift
enum Color { case R, B }
enum Tree<A> {
  case E
  indirect case T(Color, Tree<A>, A, Tree<A>)
}

func balance<A>(input: (Color, Tree<A>, A, Tree<A>)) -> Tree<A> {
  switch input {
  case let (.B, .T(.R, .T(.R,a,x,b), y, c), z, d): return .T(.R, .T(.B,a,x,b), y, .T(.B,c,z,d))
  case let (.B, .T(.R, a, x, .T(.R,b,y,c)), z, d): return .T(.R, .T(.B,a,x,b), y, .T(.B,c,z,d))
  case let (.B, a, x, .T(.R, .T(.R,b,y,c), z, d)): return .T(.R, .T(.B,a,x,b), y, .T(.B,c,z,d))
  case let (.B, a, x, .T(.R, b, y, .T(.R,c,z,d))): return .T(.R, .T(.B,a,x,b), y, .T(.B,c,z,d))
  case let (col, a, x, b)                        : return .T(col, a, x, b)
  }
}

func insert<A : Comparable>(x: A, s: Tree<A>) -> Tree<A> {
  func ins(s: Tree<A>) -> Tree<A> {
    switch s {
    case     .E           : return .T(.R,.E,x,.E)
    case let .T(col,a,y,b):
      if x < y {
        return balance((col, ins(a), y, b))
      } else if x > y {
        return balance((col, a, y, ins(b)))
      } else {
        return s
      }
    }
  }
  switch ins(s) {
  case let .T(_,a,y,b): return .T(.B,a,y,b)
  case     .E:
    assert(false)
    return .E
  }
}
```



## Tcl

Tcl doesn't have algebraic types built-in, but they can be simulated using tagged lists, and a custom pattern matching control structure can be built:

```tcl
# From http://wiki.tcl.tk/9547
package require Tcl         8.5
package provide datatype    0.1

namespace eval ::datatype {
    namespace export define match matches
    namespace ensemble create

    # Datatype definitions
    proc define {type = args} { 
        set ns [uplevel 1 { namespace current }]
        foreach cons [split [join $args] |] {
            set name [lindex $cons 0]
            set args [lrange $cons 1 end]
            proc $ns\::$name $args [format {
                lreplace [info level 0] 0 0 %s
            } [list $name]]
        }
        return $type
    }

    # Pattern matching
    # matches pattern value envVar --
    #   Returns 1 if value matches pattern, else 0
    #   Binds match variables in envVar
    proc matches {pattern value envVar} {
        upvar 1 $envVar env
        if {[var? $pattern]} { return [bind env $pattern $value] }
        if {[llength $pattern] != [llength $value]} { return 0 }
        if {[lindex $pattern 0] ne [lindex $value 0]} { return 0 }
        foreach pat [lrange $pattern 1 end] val [lrange $value 1 end] {
            if {![matches $pat $val env]} { return 0 }
        }
        return 1
    }
    # A variable starts with lower-case letter or _. _ is a wildcard.
    proc var? term { string match {[a-z_]*} $term }
    proc bind {envVar var value} {
        upvar 1 $envVar env
        if {![info exists env]} { set env [dict create] }
        if {$var eq "_"} { return 1 }
        dict set env $var $value
        return 1
    }
    proc match args {
        #puts "MATCH: $args"
        set values [lrange $args 0 end-1]
        set choices [lindex $args end]
        append choices \n [list return -code error -level 2 "no match for $values"]
        set f [list values $choices [namespace current]]
        lassign [apply $f $values] env body
        #puts "RESULT: $env -> $body"
        dict for {k v} $env { upvar 1 $k var; set var $v }
        catch { uplevel 1 $body } msg opts
        dict incr opts -level
        return -options $opts $msg
    }
    proc case args {
        upvar 1 values values
        set patterns [lrange $args 0 end-2]
        set body [lindex $args end]
        set env [dict create]
        if {[llength $patterns] != [llength $values]} { return }
        foreach pattern $patterns value $values {
            if {![matches $pattern $value env]} { return }
        }
        return -code return [list $env $body]
    }
    proc default body { return -code return [list {} $body] }
}

```

We can then code our solution similar to Haskell:


```tcl
datatype define Color = R | B
datatype define Tree  = E | T color left val right

# balance :: Color -> Tree a -> a -> Tree a -> Tree a
proc balance {color left val right} {
    datatype match $color $left $val $right {
        case B [T R [T R a x b] y c] z d -> { T R [T B $a $x $b] $y [T B $c $z $d] }
        case B [T R a x [T R b y c]] z d -> { T R [T B $a $x $b] $y [T B $c $z $d] }
        case B a x [T R [T R b y c] z d] -> { T R [T B $a $x $b] $y [T B $c $z $d] }
        case B a x [T R b y [T R c z d]] -> { T R [T B $a $x $b] $y [T B $c $z $d] }
        case col a x b                   -> { T $col $a $x $b }
    }
}
# insert :: Ord a => a -> Tree a -> Tree a
proc insert {x s} {
    datatype match [ins $x $s] {
        case [T _ a y b]  -> { T B $a $y $b }
    }
}
# ins :: Ord a => a -> Tree a -> Tree a
proc ins {x s} {
    datatype match $s {
        case E               -> { T R E $x E }
        case [T col a y b]   -> {
            if {$x < $y} { return [balance $col [ins $x $a] $y $b] }
            if {$x > $y} { return [balance $col $a $y [ins $x $b]] }
            return $s
        }
    }
}
```


