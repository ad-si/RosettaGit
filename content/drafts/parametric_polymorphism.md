+++
title = "Parametric polymorphism"
description = ""
date = 2019-09-14T09:08:17Z
aliases = []
[extra]
id = 2209
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
[[Category:Type System]]

[[wp:Parametric Polymorphism|Parametric Polymorphism]] is a way to define types or functions that are generic over other types. The genericity can be expressed by using ''type variables'' for the parameter type, and by a mechanism to explicitly or implicitly replace the type variables with concrete types when necessary.


;Task:
Write a small example for a type declaration that is parametric over another type, together with a short bit of code (and its type signature) that uses it.


A good example is a container type, let's say a binary tree, together with some function that traverses the tree, say, a ''map''-function that operates on every element of the tree.

This language feature only applies to statically-typed languages.





## Ada


```ada
generic
   type Element_Type is private;
package Container is
   type Tree is tagged private;
   procedure Replace_All(The_Tree : in out Tree; New_Value : Element_Type);
private
   type Node;
   type Node_Access is access Node;
   type Tree tagged record
      Value : Element_type;
      Left  : Node_Access := null;
      Right : Node_Access := null;
   end record;
end Container;
```


```ada
package body Container is
   procedure Replace_All(The_Tree : in out Tree; New_Value : Element_Type) is
   begin
      The_Tree.Value := New_Value;
      If The_Tree.Left /= null then
         The_Tree.Left.all.Replace_All(New_Value);
      end if;
      if The_tree.Right /= null then
         The_Tree.Right.all.Replace_All(New_Value);
      end if;
   end Replace_All;
end Container;
```



## C

If the goal is to separate algorithms from types at compile type, C may do it by macros. Here's sample code implementing binary tree with node creation and insertion:
```c
#include <stdio.h>
#include <stdlib.h>

#define decl_tree_type(T)                                                       \
        typedef struct node_##T##_t node_##T##_t, *node_##T;                    \
        struct node_##T##_t { node_##T left, right; T value; };                 \
                                                                                \
        node_##T node_##T##_new(T v) {                                          \
                node_##T node = malloc(sizeof(node_##T##_t));                   \
                node->value = v;                                                \
                node->left = node->right = 0;                                   \
                return node;                                                    \
        }                                                                       \
        node_##T node_##T##_insert(node_##T root, T v) {                        \
                node_##T n = node_##T##_new(v);                                 \
                while (root) {                                                  \
                        if (root->value < n->value)                             \
                                if (!root->left) return root->left = n;         \
                                else root = root->left;                         \
                        else                                                    \
                                if (!root->right) return root->right = n;       \
                                else root = root->right;                        \
                }                                                               \
                return 0;                                                       \
        }

#define tree_node(T) node_##T
#define node_insert(T, r, x) node_##T##_insert(r, x)
#define node_new(T, x) node_##T##_new(x)

decl_tree_type(double);
decl_tree_type(int);

int main()
{
        int i;
        tree_node(double) root_d = node_new(double, (double)rand() / RAND_MAX);

        for (i = 0; i < 10000; i++)
                node_insert(double, root_d, (double)rand() / RAND_MAX);

        tree_node(int) root_i = node_new(int, rand());
        for (i = 0; i < 10000; i++)
                node_insert(int, root_i, rand());

        return 0;
}
```

Comments: It's ugly looking, but it gets the job done. It has the drawback that all methods need to be re-created for each tree data type used, but hey, C++ template does that, too.

Arguably more interesting is run time polymorphism, which can't be trivially done; if you are confident in your coding skill, you could keep track of data types and method dispatch at run time yourself -- but then, you are probably too confident to not realize you might be better off using some higher level languages.


## C++



```cpp
template<class T>

class tree
{
  T value;
  tree *left;
  tree *right;
public:
  void replace_all (T new_value);
};
```


For simplicity, we replace all values in the tree with a new value:


```cpp
template<class T>

void tree<T>::replace_all (T new_value)
{
  value = new_value;
  if (left != NULL)
    left->replace_all (new_value);
  if (right != NULL)
    right->replace_all (new_value);
}
```


## C#

```c#
using System;

class BinaryTree<T>
{
    public T value;
    public BinaryTree<T> left;
    public BinaryTree<T> right;

    public BinaryTree(T value)
    {
        this.value = value;
    }

    public BinaryTree<U> Map<U>(Func<T, U> f)
    {
        BinaryTree<U> tree = new BinaryTree<U>(f(this.value));
        if (this.left != null)
        {
            tree.left = this.left.Map(f);
        }
        if (this.right != null)
        {
            tree.right = this.right.Map(f);
        }
        return tree;
    }
}
```


Creating a tree of integers and using Map to generate a tree of doubles with every node half the value of the first:


```c#
class Program
{
    static void Main(string[] args)
    {
        BinaryTree<int> b = new BinaryTree<int>(6);
        b.left = new BinaryTree<int>(5);
        b.right = new BinaryTree<int>(7);

        BinaryTree<double> b2 = b.Map(x => x * 0.5);
    }
}
```


{{anchor|C# modern version}}A version using more modern language constructs:

```c#
using System;

class BinaryTree<T>
{
    public BinaryTree<T> Left { get; }
    public BinaryTree<T> Right { get; }
    public T Value { get; }

    public BinaryTree(T value, BinaryTree<T> left = null, BinaryTree<T> right = null)
    {
        this.Value = value;
        this.Left = left;
        this.Right = right;
    }

    public BinaryTree<U> Map<U>(Func<T, U> f)
    {
        return new BinaryTree<U>(f(this.Value), this.Left?.Map(f), this.Right?.Map(f));
    }

    public override string ToString()
    {
        var sb = new System.Text.StringBuilder();
        this.ToString(sb, 0);
        return sb.ToString();
    }

    private void ToString(System.Text.StringBuilder sb, int depth)
    {
        sb.Append(new string('\t', depth));
        sb.AppendLine(this.Value?.ToString());
        this.Left?.ToString(sb, depth + 1);
        this.Right?.ToString(sb, depth + 1);
    }
}

static class Program
{
    static void Main()
    {
        var b = new BinaryTree<int>(6, new BinaryTree<int>(5), new BinaryTree<int>(7));

        BinaryTree<double> b2 = b.Map(x => x * 0.5);

        Console.WriteLine(b);
        Console.WriteLine(b2);
    }
}

```


{{out}}

```txt
6
        5
        7

3
        2.5
        3.5
```



## Ceylon



```ceylon>class BinaryTree<Data
(shared Data data, shared BinaryTree<Data>? left = null, shared BinaryTree<Data>? right = null) {

	shared BinaryTree<NewData> myMap<NewData>(NewData f(Data d)) =>
			BinaryTree {
				data = f(data);
				left = left?.myMap(f);
				right = right?.myMap(f);
			};
}

shared void run() {

	value tree1 = BinaryTree {
		data = 3;
		left = BinaryTree {
			data = 4;
		};
		right = BinaryTree {
			data = 5;
			left = BinaryTree {
				data = 6;
			};
		};
	};

	tree1.myMap(print);
	print("");

	value tree2 = tree1.myMap((x) => x * 333.33);
	tree2.myMap(print);
}
```



## Clean



```clean
::Tree a = Empty | Node a (Tree a) (Tree a)

mapTree :: (a -> b) (Tree a) -> (Tree b)
mapTree f Empty        = Empty
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)
```


<blockquote><small>
A digression:

Note that for the most usefulness in practical programming, a map operation like this should not be defined with a separate name but rather as <code>fmap</code> in an ''instance'' of the <code>Functor</code> ''type class'':


```clean
instance Functor Tree where
    fmap f Empty        = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
```


<code>fmap</code> can then be used exactly where <code>mapTree</code> can, but doing this also allows the use of <code>Tree</code>s with other components which are parametric over ''any type which is a Functor''. For example, this function will add 1 to any collection of any kind of number:


```clean
add1Everywhere :: (f a) -> (f a) | Functor f & Num a
add1Everywhere nums = fmap (\x = x + 1) nums
```


If we have a tree of integers, i.e. <var>f</var> is <code>Tree</code> and <var>a</var> is <code>Integer</code>, then the type of <code>add1Everywhere</code> is <code>Tree Integer -> Tree Integer</code>.
</small></blockquote>

{{omit from|Clojure}}


## Common Lisp


Common Lisp is not statically typed, but types can be defined which are parameterized over other types.  In the following piece of code, a type <code>pair</code> is defined which accepts two (optional) type specifiers.  An object is of type <code>(pair :car car-type :cdr cdr-type)</code> if an only if it is a cons whose car is of type <code>car-type</code> and whose cdr is of type <code>cdr-type</code>.


```lisp
(deftype pair (&key (car 't) (cdr 't))
  `(cons ,car ,cdr))
```


Example

 > (typep (cons 1 2) '(pair :car number :cdr number))
 T

 > (typep (cons 1 2) '(pair :car number :cdr character))
 NIL


## D


```d
class ArrayTree(T, uint N) {
    T[N] data;
    typeof(this) left, right;

    this(T initValue) { this.data[] = initValue; }

    void tmap(const void delegate(ref typeof(data)) dg) {
        dg(this.data);
        if (left) left.tmap(dg);
        if (right) right.tmap(dg);
    }
}

void main() { // Demo code.
    import std.stdio;

    // Instantiate the template ArrayTree of three doubles.
    alias AT3 = ArrayTree!(double, 3);

    // Allocate the tree root.
    auto root = new AT3(1.00);

    // Add some nodes.
    root.left = new AT3(1.10);
    root.left.left = new AT3(1.11);
    root.left.right = new AT3(1.12);

    root.right = new AT3(1.20);
    root.right.left = new AT3(1.21);
    root.right.right = new AT3(1.22);

    // Now the tree has seven nodes.

    // Show the arrays of the whole tree.
    //root.tmap(x => writefln("%(%.2f %)", x));
    root.tmap((ref x) => writefln("%(%.2f %)", x));

    // Modify the arrays of the whole tree.
    //root.tmap((x){ x[] += 10; });
    root.tmap((ref x){ x[] += 10; });

    // Show the arrays of the whole tree again.
    writeln();
    //root.tmap(x => writefln("%(%.2f %)", x));
    root.tmap((ref x) => writefln("%(%.2f %)", x));
}
```

{{out}}

```txt
1.00 1.00 1.00
1.10 1.10 1.10
1.11 1.11 1.11
1.12 1.12 1.12
1.20 1.20 1.20
1.21 1.21 1.21
1.22 1.22 1.22

11.00 11.00 11.00
11.10 11.10 11.10
11.11 11.11 11.11
11.12 11.12 11.12
11.20 11.20 11.20
11.21 11.21 11.21
11.22 11.22 11.22
```



## Dart


```dart>class TreeNode<T
 {

  T value;
  TreeNode<T> left;
  TreeNode<T> right;

  TreeNode(this.value);

  TreeNode map(T f(T t)) {
    var node = new TreeNode(f(value));
    if(left != null) {
      node.left = left.map(f);
    }
    if(right != null) {
      node.right = right.map(f);
    }
    return node;
  }

  void forEach(void f(T t)) {
    f(value);
    if(left != null) {
      left.forEach(f);
    }
    if(right != null) {
      right.forEach(f);
    }
  }
}

void main() {
  TreeNode root = new TreeNode(1);
  root.left = new TreeNode(2);
  root.right = new TreeNode(3);
  root.left.right = new TreeNode(4);

  print('first tree');
  root.forEach(print);
  var newRoot = root.map((t) => t * 222);
  print('second tree');
  newRoot.forEach(print);
}
```

{{out}}

```txt
first tree
1
2
4
3
second tree
222
444
888
666
```



## E


While E itself does not do static (before evaluation) type checking, E does have ''guards'' which form a runtime type system, and has typed collections in the standard library. Here, we implement a typed tree, and a guard which accepts trees of a specific type.

(Note: Like some other examples here, this is an incomplete program in that the tree provides no way to insert or delete nodes.)

(Note: The guard definition is arguably messy boilerplate; future versions of E may provide a scheme where the <code>interface</code> expression can itself be used to describe parametricity, and message signatures using the type parameter, but this has not been implemented or fully designed yet. Currently, this example is more of “you can do it if you need to” than something worth doing for every data structure in your program.)


```e
interface TreeAny guards TreeStamp {}
def Tree {
    to get(Value) {
        def Tree1 {
            to coerce(specimen, ejector) {
                def tree := TreeAny.coerce(specimen, ejector)
                if (tree.valueType() != Value) {
                    throw.eject(ejector, "Tree value type mismatch")
                }
                return tree
            }
        }
        return Tree1
    }
}

def makeTree(T, var value :T, left :nullOk[Tree[T]], right :nullOk[Tree[T]]) {
    def tree implements TreeStamp {
        to valueType() { return T }
        to map(f) {
            value := f(value)  # the declaration of value causes this to be checked
            if (left != null) {
                left.map(f)
            }
            if (right != null) {
                right.map(f)
            }
        }
    }
    return tree
}
```



```e
? def t := makeTree(int, 0, null, null)
# value: <tree>

? t :Tree[String]
# problem: Tree value type mismatch

? t :Tree[Int]
# problem: Failed: Undefined variable: Int

? t :Tree[int]
# value: <tree>
```



=={{header|F_Sharp|F#}}==


```fsharp

    namespace RosettaCode

    type BinaryTree<'T> =
      | Element of 'T
      | Tree of 'T * BinaryTree<'T> * BinaryTree<'T>
      member this.Map(f) =
        match this with
        | Element(x) -> Element(f x)
        | Tree(x,left,right) -> Tree((f x), left.Map(f), right.Map(f))

```


We can test this binary tree like so:

```fsharp

    let t1 = Tree(2, Element(1), Tree(4,Element(3),Element(5)) )
    let t2 = t1.Map(fun x -> x * 10)

```




## Fortran

Fortran does not offer polymorphism by parameter type, which is to say, enables the same source code to be declared applicable for parameters of different types, so that a contained statement such as <code>X = A + B*C</code> would work for any combination of integer or floating-point or complex variables as actual parameters, since exactly that (source) code would be workable in every case. Further, there is no standardised pre-processor protocol whereby one could replicate such code to produce a separate subroutine or function specific to every combination.

However, with F90 came the MODULE protocol with facilities suitable for defining "generic" subroutines or functions, or so it appears:
```Fortran
      MODULE SORTSEARCH		!Genuflect towards Prof. D. Knuth.

       INTERFACE FIND			!Binary chop search, not indexed.
        MODULE PROCEDURE
     1   FINDI4,				!I: of integers.
     2   FINDF4,FINDF8,				!F: of numbers.
     3          FINDTTI2,FINDTTI4		!T: of texts.
       END INTERFACE FIND

      CONTAINS
      INTEGER FUNCTION FINDI4(THIS,NUMB,N)	!Binary chopper. Find i such that THIS = NUMB(i)
       USE ASSISTANCE		!Only for the trace stuff.
       INTENT(IN) THIS,NUMB,N	!Imply read-only, but definitely no need for any "copy-back".
       INTEGER*4 THIS,NUMB(1:*)	!Where is THIS in array NUMB(1:N)?
       INTEGER N		!The count. In other versions, it is supplied by the index.
       INTEGER L,R,P		!Fingers.
Chop away.
        L = 0			!Establish outer bounds.
        R = N + 1		!One before, and one after, the first and last.
    1   P = (R - L)/2		!Probe point offset. Beware integer overflow with (L + R)/2.
        IF (P.LE.0) THEN	!Aha! Nowhere! And THIS follows NUMB(L).
          FINDI4 = -L		!Having -L rather than 0 (or other code) might be of interest.
          RETURN		!Finished.
        END IF			!So much for exhaustion.
        P = P + L		!Convert from offset to probe point.
        IF (THIS - NUMB(P)) 3,4,2	!Compare to the probe point.
    2   L = P			!Shift the left bound up: THIS follows NUMB(P).
        GO TO 1			!Another chop.
    3   R = P			!Shift the right bound down: THIS precedes NUMB(P).
        GO TO 1			!Try again.
Caught it! THIS = NUMB(P)
    4   FINDI4 = P		!So, THIS is found, here!
      END FUNCTION FINDI4	!On success, THIS = NUMB(FINDI4); no fancy index here...

      END MODULE SORTSEARCH
```


There would be a function (with a unique name) for each of the contemplated variations in parameter types, and when the compiler reached an invocation of FIND(...) it would select by matching amongst the combinations that had been defined in the routines named in the INTERFACE statement. The various actual functions could have different code, and in this case, only the <code>INTEGER*4 THIS,NUMB(1:*)</code> need be changed, say to <code>REAL*4 THIS,NUMB(1:*)</code> for FINDF4, which is why both variables are named in the one statement. However, for searching CHARACTER arrays, because the character comparison operations differ from those for numbers (and, no three-way IF-test either), additional changes are required. Thus, function FIND would appear to be a polymorphic function that accepts and returns a variety of types, but it is not, and indeed, there is actually no function called FIND anywhere in the compiled code.

That said, some systems had polymorphic variables, such as the B6700 whereby integers were represented as floating-point numbers and so exactly the same function could be presented with an integer or a floating-point variable (provided the compiler didn't check for parameter type matching - but this was routine) and it would work - so long as no divisions were involved since addition, subtraction, and multiplication are the same for both, but integer division discards any remainders. More recent computers following the Intel 8087 floating-point processor and similar add novel states to the scheme for floating-point arithmetic: not just zero and "gradual underflow" but "Infinity" and "Not a Number", which last violates even more of the axia of mathematics in that ''NaN'' does not equal ''NaN''. In turn, this forces a modicum of polymorphism into the language so as to contend with the additional features, such as the special function IsNaN(x).

More generally, using the same code for different types of variable can be problematical. A scheme that works in single precision may not work in double precision (or ''vice-versa'') or may not give corresponding levels of accuracy, or not converge at all, ''etc.'' While F90 also standardised special functions that give information about the precision of variables and the like, and in principle, a method could be coded that, guided by such information, would work for different precisions, this sort of scheme is beset by all manner of difficulties in problems more complex than the simple examples of text books.

Polymorphism just exacerbates the difficulties, thus, on page 219 of ''16-Bit Modern Microcomputers'' by G. M. Corsline appears the remark "At least some of the generalized numerical solutions to common mathematical procedures have coding that is so involved and tricky in order to take care of all possible roundoff contingencies that they have been termed 'pornographic algorithms'.". And "Mathematical software is easy for the uninitiated to write but notoriously hard for the expert. This paradox exists because the beginner is satisfied if his code usually works in his own machine while the expert attempts, against overwhelming obstacles, to produce programs that always work on a large number of computers. The problem is that while standard formulas of mathematics are fairly easy to translate into FORTRAN they often are subject to instabilities due to roundoff error." - quoting John Palmer, 1980, Intel Corporation.

But sometimes it is not so troublesome, as in [[Pathological_floating_point_problems#The_Chaotic_Bank_Society]] whereby the special EPSILON(x) function that reports on the precision of a nominated variable of type ''x'' is used to determine the point beyond which further calculation (in that precision, for that formula) will make no difference.

Having flexible facilities available my lead one astray. Consider the following data aggregate, as became available with F90:
```Fortran
      TYPE STUFF
       INTEGER CODE       !A key number.
       CHARACTER*6 NAME   !Associated data.
       INTEGER THIS       !etc.
      END TYPE STUFF
      TYPE(STUFF) TABLE(600)   !An array of such entries.
```

Suppose the array was in sorted order by each entry's value of CODE so that TABLE(1).CODE <= TABLE(2).CODE, etc. and one wished to find the index of an entry with a specific value, ''x'', of CODE. It is pleasing to be able to write <code>FIND(x,TABLE.CODE,N)</code> and have it accepted by the compiler. Rather less pleasing is that it runs very slowly.

This is because consecutive elements in an array are expected to occupy consecutive locations in storage, but the CODE elements do not, being separated by the other elements of the aggregate. So, the compiler generates code to copy the required elements to a work area, presents that as the actual parameter, and copies from the work area back on return from the function, thereby vitiating the speed advantages of the binary search. This is why the <code>INTENT(IN)</code> might help in such situations, as will writing <code>FIND(x,TABLE(1:N).CODE,N)</code> should N be often less than the full size of the table. But really, in-line code for each such usage is the only answer, despite the lack of a pre-processor to generate it.

Other options are to remain with the older-style of Fortran, using separately-defined arrays having a naming convention such as TABLECODE(600), TABLENAME(600), ''etc''. thus not gaining the unity of declaring a TYPE, or, declaring the size within the type as in <code>INTEGER CODE(600)</code> except that this means that the size is a part of the type and different-sized tables would require different types, or, perhaps the compiler will handle this problem by passing a "stride" value for every array dimension so that subroutines and functions can index such parameters properly - at the cost of yet more overhead for parameter passing, and more complex indexing calculations.

In short, the available polymorphism whereby a parameter can be a normal array, or, an array-like "selection" of a component from an array of compound entities enables appealing syntax, but disasterous performance.


## Go

The parametric function in this example is the function average.  It's type parameter is the interface type intCollection, and its logic uses the polymorphic function mapElements.  In Go terminology, average is an ordinary function whose parameter happens to be of interface type.  Code inside of average is ordinary code that just happens to call the mapElements method of its parameter.  This code accesses the underlying static type only through the interface and so has no knowledge of the details of the static type or even which static type it is dealing with.

Function main creates objects t1 and t2 of two different static types, binaryTree an bTree.  Both types implement the interface intCollection.
t1 and t2 have different static types, but when they are passed to average, they are bound to parameter c, of interface type, and their static types are not visible within average.

Implementation of binaryTree and bTree is dummied, but you can see that implementation of average of binaryTree contains code specific to its representation (left, right) and that implementation of bTree contains code specific to its representation (buckets.)

```go
package main

import "fmt"

func average(c intCollection) float64 {
    var sum, count int
    c.mapElements(func(n int) {
        sum += n
        count++
    })
    return float64(sum) / float64(count)
}

func main() {
    t1 := new(binaryTree)
    t2 := new(bTree)
    a1 := average(t1)
    a2 := average(t2)
    fmt.Println("binary tree average:", a1)
    fmt.Println("b-tree average:", a2)
}

type intCollection interface {
    mapElements(func(int))
}

type binaryTree struct {
    // dummy representation details
    left, right bool
}

func (t *binaryTree) mapElements(visit func(int)) {
    // dummy implementation
    if t.left == t.right {
        visit(3)
        visit(1)
        visit(4)
    }
}

type bTree struct {
    // dummy representation details
    buckets int
}

func (t *bTree) mapElements(visit func(int)) {
    // dummy implementation
    if t.buckets >= 0 {
        visit(1)
        visit(5)
        visit(9)
    }
}
```

Output:

```txt

binary tree average: 2.6666666666666665
b-tree average: 5

```



## Go 2



```go

package rosettacode

type Tree(type T) struct {
    val T
    left *Tree(T)
    right *Tree(T)
}

func (t *Tree(T)) ReplaceAll(rep T) {
    t.val = rep
    if t.left != nil  { t.left.ReplaceAll(rep) }
    if t.right != nil { t.right.ReplaceAll(rep) }
}

```



## Groovy

{{trans|Java}} (more or less)
Solution:

```groovy>class Tree<T
 {
    T value
    Tree<T> left
    Tree<T> right

    Tree(T value = null, Tree<T> left = null, Tree<T> right = null) {
        this.value = value
        this.left = left
        this.right = right
    }

    void replaceAll(T value) {
        this.value = value
        left?.replaceAll(value)
        right?.replaceAll(value)
    }
}
```



## Haskell



```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty        = Empty
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)
```


<blockquote><small>
A digression:

Note that for the most usefulness in practical programming, a map operation like this should not be defined with a separate name but rather as <code>fmap</code> in an ''instance'' of the <code>Functor</code> ''type class'':


```haskell
instance Functor Tree where
    fmap f Empty        = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
```


<code>fmap</code> can then be used exactly where <code>mapTree</code> can, but doing this also allows the use of <code>Tree</code>s with other components which are parametric over ''any type which is a Functor''. For example, this function will add 1 to any collection of any kind of number:


```haskell
add1Everywhere :: (Functor f, Num a) => f a -> f a
add1Everywhere nums = fmap (\x -> x + 1) nums
```


If we have a tree of integers, i.e. <var>f</var> is <code>Tree</code> and <var>a</var> is <code>Integer</code>, then the type of <code>add1Everywhere</code> is <code>Tree Integer -> Tree Integer</code>.
</small></blockquote>


## Inform 7

Phrases (the equivalent of global functions) can be defined with type parameters:

```inform7
Polymorphism is a room.

To find (V - K) in (L - list of values of kind K):
	repeat with N running from 1 to the number of entries in L:
		if entry N in L is V:
			say "Found [V] at entry [N] in [L].";
			stop;
	say "Did not find [V] in [L]."

When play begins:
	find "needle" in {"parrot", "needle", "rutabaga"};
	find 6 in {2, 3, 4};
	end the story.
```


Inform 7 does not allow user-defined parametric types. Some built-in types can be parameterized, though:

```inform7
list of numbers
relation of texts to rooms
object based rulebook producing a number
description of things
activity on things
number valued property
text valued table column
phrase (text, text) -> number
```


=={{header|Icon}} and {{header|Unicon}}==

Like PicoLisp, Icon and Unicon are dynamically typed and hence inherently polymorphic.
Here's an example that can apply a function to the nodes in an <i>n</i>-tree regardless of
the type of each node.  It is up to the function to decide what to do with a given type
of node.  Note that the nodes do no even have to be of the same type.


```unicon
procedure main()
    bTree := [1, [2, [4, [7]], [5]], [3, [6, [8], [9]]]]
    mapTree(bTree, write)
    bTree := [1, ["two", ["four", [7]], [5]], [3, ["six", ["eight"], [9]]]]
    mapTree(bTree, write)
end

procedure mapTree(tree, f)
    every f(\tree[1]) | mapTree(!tree[2:0], f)
end
```



## J


In J, all functions are generic over other types.

Alternatively, J is statically typed in the sense that it supports only one data type (the array), though of course inspecting a value can reveal additional details (such as: is it an array of numbers?)

(That said, note that J also supports some types which are not, strictly speaking, data. These are the verb, adverb and conjunction types. To fit this nomenclature, data is of type "noun". Also, nouns have some additional taxonomy which is beyond the scope of this task.)


## Java

Following the C++ example:

```java>public class Tree<T
{
	private T value;
	private Tree<T> left;
	private Tree<T> right;

	public void replaceAll(T value){
		this.value = value;
		if (left != null)
			left.replaceAll(value);
		if (right != null)
			right.replaceAll(value);
	}
}
```



## Julia

{{works with|Julia|0.6}}
{{trans|C++}}


```julia
mutable struct Tree{T}
    value::T
    lchild::Nullable{Tree{T}}
    rchild::Nullable{Tree{T}}
end

function replaceall!(t::Tree{T}, v::T) where T
    t.value = v
    isnull(lchild) || replaceall(get(lchild), v)
    isnull(rchild) || replaceall(get(rchild), v)
    return t
end
```



## Kotlin

{{trans|C#}}

```scala
// version 1.0.6

class BinaryTree<T>(var value: T) {
    var left : BinaryTree<T>? = null
    var right: BinaryTree<T>? = null

    fun <U> map(f: (T) -> U): BinaryTree<U> {
        val tree = BinaryTree<U>(f(value))
        if (left  != null) tree.left  = left?.map(f)
        if (right != null) tree.right = right?.map(f)
        return tree
    }

    fun showTopThree() = "(${left?.value}, $value, ${right?.value})"
}

fun main(args: Array<String>) {
    val b   = BinaryTree(6)
    b.left  = BinaryTree(5)
    b.right = BinaryTree(7)
    println(b.showTopThree())
    val b2  = b.map { it * 10.0 }
    println(b2.showTopThree())
}
```


{{out}}

```txt

(5, 6, 7)
(50.0, 60.0, 70.0)

```



## Mercury


```mercury
:- type tree(A) ---> empty ; node(A, tree(A), tree(A)).

:- func map(func(A) = B, tree(A)) = tree(B).

map(_, empty) = empty.
map(F, node(A, Left, Right)) = node(F(A), map(F, Left), map(F, Right)).
```



## Nim


```nim
type Tree[T] = ref object
  value: T
  left, right: Tree[T]
```


=={{header|Objective-C}}==
{{trans|C++}}
{{works with|Xcode|7}}

```objc>@interface Tree<T
 : NSObject {
  T value;
  Tree<T> *left;
  Tree<T> *right;
}

- (void)replaceAll:(T)v;
@end

@implementation Tree
- (void)replaceAll:(id)v {
  value = v;
  [left replaceAll:v];
  [right replaceAll:v];
}
@end
```

Note that the generic type variable is only used in the declaration, but not in the implementation.


## OCaml



```ocaml
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

(** val map_tree : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map_tree f = function
  | Empty        -> Empty
  | Node (x,l,r) -> Node (f x, map_tree f l, map_tree f r)
```


{{omit from|Oforth|Oforth is nt  statically-typed language}}


## Perl 6


```perl6
role BinaryTree[::T] {
    has T $.value;
    has BinaryTree[T] $.left;
    has BinaryTree[T] $.right;

    method replace-all(T $value) {
        $!value = $value;
        $!left.replace-all($value) if $!left.defined;
        $!right.replace-all($value) if $!right.defined;
    }
}

class IntTree does BinaryTree[Int] { }

my IntTree $it .= new(value => 1,
                      left  => IntTree.new(value => 2),
                      right => IntTree.new(value => 3));

$it.replace-all(42);
say $it.perl;
```

{{out}}

```txt
IntTree.new(value => 42, left => IntTree.new(value => 42, left => BinaryTree[T], right => BinaryTree[T]), right => IntTree.new(value => 42, left => BinaryTree[T], right => BinaryTree[T]))
```



## Phix

Phix is naturally polymorphic, with optional static typing.

The standard builtin type hierarcy is trivial:

```txt

        <-------- object --------->
        |                |
        +-atom           +-sequence
          |                |
          +-integer        +-string

```

User defined types are subclasses of those.

If you declare a parameter as type integer then obviously it is optimised for that, and crashes when given something else (with a clear human-readable message and file name/line number).
If you declare a parameter as type object then it can handle anything you can throw at it - integers, floats, strings, or (deeply) nested sequences.

Of course many builtin routines are naturally generic, such as sort and print.

Most programming languages would throw a hissy fit if you tried to sort (or print) a mixed collection of strings and integers, but not Phix:

```Phix
?sort(shuffle({5,"oranges",6,"apples",7}))
```

{{out}}

```txt

{5,6,7,"apples","oranges"}

```

For comparison purposes (and because this entry looked a bit sparse without it) this is the D example from this page translated to Phix.

Note that tmap has to be a function rather than a procedure with a reference parameter, but this still achieves
pass-by-reference/in-situ updates, mainly because root is a local rather than global/static, and is the target of
(aka assigned to/overwritten on return from) the top-level tmap() call, and yet also manages the C#/Dart/Kotlin
thing (by which I am referring to those specific examples on this page) of creating a whole new tree, simply
because lhs assignee!=rhs reference (aka root2!=root) in "root2 = tmap(root,rid)", not that such a "deep clone"
would (barring a few dirty low-level tricks) behave any differently to "root2=root", which is "a straightforward shared reference
with cow semantics".

```Phix
enum data, left, right

function tmap(sequence tree, integer rid)
    tree[data] = call_func(rid,{tree[data]})
    if tree[left]!=null then tree[left] = tmap(tree[left],rid) end if
    if tree[right]!=null then tree[right] = tmap(tree[right],rid) end if
    return tree
end function

function newnode(object v)
    return {v,null,null}
end function

function add10(atom x) return x+10 end function

procedure main()
    object root = newnode(1.00)
    -- Add some nodes.
    root[left] = newnode(1.10)
    root[left][left] = newnode(1.11)
    root[left][right] = newnode(1.12)

    root[right] = newnode(1.20)
    root[right][left] = newnode(1.21)
    root[right][right] = newnode(1.22)

    -- Now the tree has seven nodes.

    -- Show the whole tree.
    ppOpt({pp_Nest,2})
    pp(root)

    -- Modify the whole tree.
    root = tmap(root,routine_id("add10"))

    -- Create a whole new tree.
    object root2 = tmap(root,rid)

    -- Show the whole tree again.
    pp(root)
end procedure
main()
```

{{out}}

```txt

{1,
 {1.1,
  {1.11,0,0},
  {1.12,0,0}},
 {1.2,
  {1.21,0,0},
  {1.22,0,0}}}
{11,
 {11.1,
  {11.11,0,0},
  {11.12,0,0}},
 {11.2,
  {11.21,0,0},
  {11.22,0,0}}}

```



## PicoLisp

PicoLisp is dynamically-typed, so in principle every function is polymetric over its arguments. It is up to the function to decide what to do with them. A function traversing a tree, modifying the nodes in-place (no matter what the type of the node is):

```PicoLisp
(de mapTree (Tree Fun)
   (set Tree (Fun (car Tree)))
   (and (cadr Tree) (mapTree @ Fun))
   (and (cddr Tree) (mapTree @ Fun)) )
```

Test:
<pre style="height:20em;overflow:scroll">(balance 'MyTree (range 1 7))          # Create a tree of numbers
-> NIL

: (view MyTree T)                      # Display it
      7
   6
      5
4
      3
   2
      1
-> NIL


: (mapTree MyTree inc)                 # Increment all nodes
-> NIL

: (view MyTree T)                      # Display the tree
      8
   7
      6
5
      4
   3
      2
-> NIL


: (balance 'MyTree '("a" "b" "c" "d" "e" "f" "g"))  # Create a tree of strings
-> NIL

: (view MyTree T)                      # Display it
      "g"
   "f"
      "e"
"d"
      "c"
   "b"
      "a"
-> NIL

: (mapTree MyTree uppc)                # Convert all nodes to upper case
-> NIL

: (view MyTree T)                      # Display the tree
      "G"
   "F"
      "E"
"D"
      "C"
   "B"
      "A"
-> NIL
```



## Racket


Typed Racket has parametric polymorphism:


```racket

#lang typed/racket

(define-type (Tree A) (U False (Node A)))

(struct: (A) Node
  ([val : A] [left : (Tree A)] [right : (Tree A)])
  #:transparent)

(: tree-map (All (A B) (A -> B) (Tree A) -> (Tree B)))
(define (tree-map f tree)
  (match tree
    [#f #f]
    [(Node val left right)
     (Node (f val) (tree-map f left) (tree-map f right))]))

;; unit tests
(require typed/rackunit)
(check-equal?
 (tree-map add1 (Node 5 (Node 3 #f #f) #f))
 (Node 6 (Node 4 #f #f) #f))

```



## REXX

This REXX programming example is modeled after the   '''D'''   example.

```rexx
/*REXX program  demonstrates  (with displays)  a method of  parametric polymorphism.    */
call newRoot  1.00, 3                            /*new root,  and also indicate 3 stems.*/
                                                 /* [↓]  no need to label the stems.    */
call addStem  1.10                               /*a new stem  and  its initial value.  */
call addStem  1.11                               /*"  "    "    "    "     "      "     */
call addStem  1.12                               /*"  "    "    "    "     "      "     */
call addStem  1.20                               /*"  "    "    "    "     "      "     */
call addStem  1.21                               /*"  "    "    "    "     "      "     */
call addStem  1.22                               /*"  "    "    "    "     "      "     */
                       call sayNodes             /*display some nicely formatted values.*/
call modRoot  50                                 /*modRoot will add fifty to all stems. */
                       call sayNodes             /*display some nicely formatted values.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
addStem:  nodes=nodes + 1;     do j=1  for stems;   root.nodes.j=arg(1);   end;     return
newRoot:  parse arg @,stems; nodes=-1; call addStem copies('═',9); call addStem @;  return
/*──────────────────────────────────────────────────────────────────────────────────────*/
modRoot:  arg #;   do    j=1  for nodes          /*traipse through all the defined nodes*/
                      do k=1  for stems
                      if datatype(root.j.k,'N')  then root.j.k=root.j.k + #  /*add bias.*/
                      end   /*k*/                /* [↑]  only add if numeric stem value.*/
                   end      /*j*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
sayNodes: w=9;     do    j=0  to  nodes;   _=    /*ensure each of the nodes gets shown. */
                      do k=1  for stems;   _=_ center(root.j.k, w)  /*concatenate a node*/
                      end   /*k*/
                   $=word('node='j, 1 +  (j<1) ) /*define a label for this line's output*/
                   say center($, w) substr(_, 2) /*ignore 1st (leading) blank which was */
                   end      /*j*/                /* [↑]         caused by concatenation.*/
          say                                    /*show a blank line to separate outputs*/
          return                                 /* [↑]  extreme indentation to terminal*/
```

{{out|output|text=  when using the default input:}}

```txt

          ═════════ ═════════ ═════════
 node=1     1.00      1.00      1.00
 node=2     1.10      1.10      1.10
 node=3     1.11      1.11      1.11
 node=4     1.12      1.12      1.12
 node=5     1.20      1.20      1.20
 node=6     1.21      1.21      1.21
 node=7     1.22      1.22      1.22

          ═════════ ═════════ ═════════
 node=1     51.00     51.00     51.00
 node=2     51.10     51.10     51.10
 node=3     51.11     51.11     51.11
 node=4     51.12     51.12     51.12
 node=5     51.20     51.20     51.20
 node=6     51.21     51.21     51.21
 node=7     51.22     51.22     51.22

```



## Rust


```rust>struct TreeNode<T
 {
    value: T,
    left: Option<Box<TreeNode<T>>>,
    right: Option<Box<TreeNode<T>>>,
}

impl <T> TreeNode<T> {
    fn my_map<U,F>(&self, f: &F) -> TreeNode<U> where
            F: Fn(&T) -> U {
        TreeNode {
            value: f(&self.value),
            left: match self.left {
                None => None,
                Some(ref n) => Some(Box::new(n.my_map(f))),
            },
            right: match self.right {
                None => None,
                Some(ref n) => Some(Box::new(n.my_map(f))),
            },
        }
    }
}

fn main() {
    let root = TreeNode {
        value: 3,
        left: Some(Box::new(TreeNode {
            value: 55,
            left: None,
            right: None,
        })),
        right: Some(Box::new(TreeNode {
            value: 234,
            left: Some(Box::new(TreeNode {
                value: 0,
                left: None,
                right: None,
            })),
            right: None,
        })),
    };
    root.my_map(&|x| { println!("{}" , x)});
    println!("---------------");
    let new_root = root.my_map(&|x| *x as f64 * 333.333f64);
    new_root.my_map(&|x| { println!("{}" , x) });
}
```



## Scala


There's much to be said about parametric polymorphism in Scala. Let's first see
the example in question:


```scala
case class Tree[+A](value: A, left: Option[Tree[A]], right: Option[Tree[A]]) {
  def map[B](f: A => B): Tree[B] =
    Tree(f(value), left map (_.map(f)), right map (_.map(f)))
}
```


Note that the type parameter of the class <tt>Tree</tt>, <tt>[+A]</tt>. The
plus sign indicates that <tt>Tree</tt> is ''co-variant'' on <tt>A</tt>. That means <tt>Tree[X]</tt>
will be a subtype of <tt>Tree[Y]</tt> if <tt>X</tt> is a subtype of <tt>Y</tt>. For example:


```scala
class Employee(val name: String)
class Manager(name: String) extends Employee(name)

val t = Tree(new Manager("PHB"), None, None)
val t2: Tree[Employee] = t
```


The second assignment is legal because <tt>t</tt> is of type <tt>Tree[Manager]</tt>, and since
<tt>Manager</tt> is a subclass of <tt>Employee</tt>, then <tt>Tree[Manager]</tt> is a subtype of
<tt>Tree[Employee]</tt>.

Another possible variance is the ''contra-variance''. For instance, consider the following example:


```scala
def toName(e: Employee) = e.name
val treeOfNames = t.map(toName)
```


This works, even though <tt>map</tt> is expecting a function from <tt>Manager</tt> into something,
but <tt>toName</tt> is a function of <tt>Employee</tt> into <tt>String</tt>, and <tt>Employee</tt>
is a supertype, not a subtype, of <tt>Manager</tt>. It works because functions have the following
definition in Scala:


```scala
trait Function1[-T1, +R]
```


The minus sign indicates that this trait is ''contra-variant'' in <tt>T1</tt>, which happens to be
the type of the argument of the function. In other words, it tell us that, <tt>Employee => String</tt>
is a ''subtype'' of <tt>Manager => String</tt>, because <tt>Employee</tt> is a ''supertype'' of
<tt>Manager</tt>. While the concept of contra-variance is not intuitive, it should be clear to anyone
that <tt>toName</tt> can handle arguments of type <tt>Manager</tt>, but, were not for the contra-variance,
it would ''not'' be usable with a <tt>Tree[Manager]</tt>.

Let's add another method to <tt>Tree</tt> to see another concept:


```scala
case class Tree[+A](value: A, left: Option[Tree[A]], right: Option[Tree[A]]) {
  def map[B](f: A => B): Tree[B] =
    Tree(f(value), left map (_.map(f)), right map (_.map(f)))
  def find[B >: A](what: B): Boolean =
    (value == what) || left.map(_.find(what)).getOrElse(false) || right.map(_.find(what)).getOrElse(false)
}
```


The type parameter of <tt>find</tt> is <tt>[B >: A]</tt>. That means the type is some <tt>B</tt>, as long
as that <tt>B</tt> is a supertype of <tt>A</tt>. If I tried to declare <tt>what: A</tt>, Scala would not
accept it. To understand why, let's consider the following code:


```scala
if (t2.find(new Employee("Dilbert")))
  println("Call Catbert!")
```


Here we have <tt>find</tt> receiving an argument of type <tt>Employee</tt>, even though the tree
it was defined on is of type <tt>Manager</tt>. The co-variance of <tt>Tree</tt> means a situation
such as this is possible.

There is also an operator <tt>&lt;:</tt>, with the opposite meaning of <tt>>:</tt>.

Finally, Scala also allows abstract types. Abtract types are similar to abstract methods: they have
to be defined when a class is inherited. One simple example would be:


```scala
trait DFA {
  type Element
  val map = new collection.mutable.HashMap[Element, DFA]()
}
```


A concrete class wishing to inherit from <tt>DFA</tt> would need to define <tt>Element</tt>. Abstract
types aren't all that different from type parameters. Mainly, they ensure that the type will be
selected in the definition site (the declaration of the concrete class), and not at the usage site
(instantiation of the concrete class). The difference is mainly one of style, though.


## Seed7

In Seed7 types like ''array'' and ''struct'' are not built-in, but are defined with parametric polymorphism.
In the Seed7 documentation the terms "template" and "function with type parameters and type result" are used instead of "parametric polymorphism".
E.g.: ''array'' is actually a function, which takes an element type as parameter and returns a type.
To concentrate on the essentials, the example below defines the type ''container'' as ''array''.
Note that the ''map'' function has three parameters: ''aContainer'', ''aVariable'', and ''aFunc''.
When ''map'' is called ''aVariable'' is used also in the actual parameter of ''aFunc'': map(container1, num, num + 1)


```seed7
$ include "seed7_05.s7i";

const func type: container (in type: elemType) is func
  result
    var type: container is void;
  begin
    container := array elemType;

    global

      const func container: map (in container: aContainer,
          inout elemType: aVariable, ref func elemType: aFunc) is func
        result
          var container: mapResult is container.value;
        begin
          for aVariable range aContainer do
            mapResult &:= aFunc;
          end for;
        end func;

    end global;
  end func;

const type: intContainer is container(integer);
var intContainer: container1 is [] (1, 2, 4, 6, 10, 12, 16, 18, 22);
var intContainer: container2 is 0 times 0;

const proc: main is func
  local
    var integer: num is 0;
  begin
    container2 := map(container1, num, num + 1);
    for num range container2 do
      write(num <& " ");
    end for;
    writeln;
  end func;
```


Output:

```txt

2 3 5 7 11 13 17 19 23

```



## Standard ML


```sml
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree

(** val map_tree = fn : ('a -> 'b) -> 'a tree -> 'b tree *)
fun map_tree f Empty = Empty
  | map_tree f (Node (x,l,r)) = Node (f x, map_tree f l, map_tree f r)
```



## Swift

{{trans|Java}}

```swift>class Tree<T
 {
  var value: T?
  var left: Tree<T>?
  var right: Tree<T>?

  func replaceAll(value: T?) {
    self.value = value
    left?.replaceAll(value)
    right?.replaceAll(value)
  }
}
```


Another version based on Algebraic Data Types:
{{works with|Swift|2+}}

```swift>enum Tree<T
 {
  case Empty
  indirect case Node(T, Tree<T>, Tree<T>)

  func map<U>(f : T -> U) -> Tree<U> {
    switch(self) {
    case     .Empty        : return .Empty
    case let .Node(x, l, r): return .Node(f(x), l.map(f), r.map(f))
    }
  }
}
```



## Ursala

Types are first class entities and functions to construct or operate on them may be defined
routinely. A parameterized binary tree type can be defined using a syntax for anonymous
recursion in type expressions as in this example,

```Ursala
binary_tree_of "node-type" = "node-type"%hhhhWZAZ
```

or by way of a recurrence solved using a fixed point combinator imported from a library
as shown below.

```Ursala
#import tag

#fix general_type_fixer 1

binary_tree_of "node-type" = ("node-type",(binary_tree_of "node-type")%Z)%drWZwlwAZ
```

(The <code>%Z</code> type operator constructs a "maybe" type, i.e., the free union of its operand type
with the null value. Others shown above are standard stack manipulation primitives, e.g. <code>d</code> (dup) and <code>w</code> (swap), used to build the type expression tree.) At the other extreme, one may construct an equivalent parameterized type in
point-free form.

```Ursala
binary_tree_of = %-hhhhWZAZ
```

A mapping combinator over this type can be defined with pattern matching like this

```Ursala
binary_tree_map "f" = ~&a^& ^A/"f"@an ~&amPfamPWB
```

or in point free form like this.

```Ursala
binary_tree_map = ~&a^&+ ^A\~&amPfamPWB+ @an
```

Here is a test program
defining a type of binary trees of strings, and a function that concatenates each node
with itself.

```Ursala
string_tree = binary_tree_of %s

x = 'foo': ('bar': (),'baz': ())

#cast string_tree

example = (binary_tree_map "s". "s"--"s") x
```

Type signatures are not necessarily associated with function declarations, but
have uses in the other contexts such as assertions and compiler directives
(e.g., <code>#cast</code>). Here is the output.

```txt

'foofoo': ('barbar': (),'bazbaz': ())

```



## Visual Basic .NET

{{trans|C# modern version}}

```vbnet
Class BinaryTree(Of T)
    ReadOnly Property Left As BinaryTree(Of T)
    ReadOnly Property Right As BinaryTree(Of T)
    ReadOnly Property Value As T

    Sub New(value As T, Optional left As BinaryTree(Of T) = Nothing, Optional right As BinaryTree(Of T) = Nothing)
        Me.Value = value
        Me.Left = left
        Me.Right = right
    End Sub

    Function Map(Of U)(f As Func(Of T, U)) As BinaryTree(Of U)
        Return New BinaryTree(Of U)(f(Me.Value), Me.Left?.Map(f), Me.Right?.Map(f))
    End Function

    Overrides Function ToString() As String
        Dim sb As New Text.StringBuilder()
        Me.ToString(sb, 0)
        Return sb.ToString()
    End Function

    Private Overloads Sub ToString(sb As Text.StringBuilder, depth As Integer)
        sb.Append(New String(ChrW(AscW(vbTab)), depth))
        sb.AppendLine(Me.Value?.ToString())
        Me.Left?.ToString(sb, depth + 1)
        Me.Right?.ToString(sb, depth + 1)
    End Sub
End Class

Module Program
    Sub Main()
        Dim b As New BinaryTree(Of Integer)(6, New BinaryTree(Of Integer)(5), New BinaryTree(Of Integer)(7))
        Dim b2 As BinaryTree(Of Double) = b.Map(Function(x) x * 0.5)

        Console.WriteLine(b)
        Console.WriteLine(b2)
    End Sub
End Module
```

{{out}}

```txt
6
        5
        7

3
        2.5
        3.5
```



## Visual Prolog


```prolog

domains
   tree{Type} = branch(tree{Type} Left, tree{Type} Right); leaf(Type Value).

class predicates
   treewalk : (tree{X},function{X,Y}) -> tree{Y} procedure (i,i).

clauses
   treewalk(branch(Left,Right),Func) = branch(NewLeft,NewRight) :-
        NewLeft = treewalk(Left,Func), NewRight = treewalk(Right,Func).

   treewalk(leaf(Value),Func) = leaf(X) :-
        X = Func(Value).

   run():-
       init(),
       X = branch(leaf(2), branch(leaf(3),leaf(4))),
       Y = treewalk(X,addone),
       write(Y),
       succeed().

```








{{omit from|Axe}}
{{omit from|C|no type variables in stdc}}
{{omit from|C|no type variables}}
{{omit from|Factor|not statically typed}}
{{omit from|J|not statically typed}}
{{omit from|JavaScript|not statically typed}}
{{omit from|M4|not typed}}
{{omit from|Maxima}}
{{omit from|Oz|not statically typed}}
{{omit from|Perl|not statically typed}}
{{omit from|Python|not statically typed}}
{{omit from|Ruby|not statically typed}}
{{omit from|Tcl|not statically typed}}
{{omit from|TI-83 BASIC|Does not have static or user-defined types.}}
{{omit from|TI-89 BASIC|Does not have static or user-defined types.}}
{{omit from|LaTeX}}
{{omit from|Retro|typeless}}
{{omit from|VBA|the Variant type is available.}}
{{omit from|zkl|typeless}}
