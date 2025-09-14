+++
title = "Same Fringe"
description = ""
date = 2018-04-07T21:11:08Z
aliases = []
[extra]
id = 12228
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "bracmat",
  "c",
  "clojure",
  "csharp",
  "d",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "scheme",
  "sidef",
  "tcl",
  "zkl",
]
+++

Write a routine that will compare the leaves ("fringe") of two binary trees to determine whether they are the same list of leaves when visited left-to-right.  The structure or balance of the trees does not matter; only the number, order, and value of the leaves is important.

Any solution is allowed here, but many computer scientists will consider it inelegant to collect either fringe in its entirety before starting to collect the other one.  In fact, this problem is usually proposed in various forums as a way to show off various forms of concurrency (tree-rotation algorithms have also been used to get around the need to collect one tree first).  Thinking of it a slightly different way, an elegant solution is one that can perform the minimum amount of work to falsify the equivalence of the fringes when they differ somewhere in the middle, short-circuiting the unnecessary additional traversals and comparisons.

Any representation of a binary tree is allowed, as long as the nodes are orderable, and only downward links are used (for example, you may not use parent or sibling pointers to avoid recursion).


## Ada


We first specify a "Bin_Trees" package with standard subprograms to handle binary trees.
The package is generic, which allows Data to be essentially of any type.


```Ada
generic
   type Data is private;
package Bin_Trees is

   type Tree_Type is private;

   function Empty(Tree: Tree_Type) return Boolean;
   function Left (Tree: Tree_Type) return Tree_Type;
   function Right(Tree: Tree_Type) return Tree_Type;
   function Item (Tree: Tree_Type) return Data;
   function Empty return Tree_Type;

   procedure Destroy_Tree(N: in out Tree_Type);
   function Tree(Value: Data) return Tree_Type;
   function Tree(Value: Data; Left, Right : Tree_Type) return Tree_Type;

private

   type Node;
   type Tree_Type is access Node;
   type Node is record
      Left, Right: Tree_Type := null;
      Item: Data;
   end record;

end Bin_Trees;
```


The implementation is straightforward.


```Ada
with Ada.Unchecked_Deallocation;

package body Bin_Trees is

   function Empty(Tree: Tree_Type) return Boolean is
   begin
      return Tree = null;
   end Empty;

   function Empty return Tree_Type is
   begin
      return null;
   end Empty;

   function Left (Tree: Tree_Type) return Tree_Type is
   begin
      return Tree.Left;
   end Left;

   function Right(Tree: Tree_Type) return Tree_Type is
   begin
      return Tree.Right;
   end Right;

   function Item (Tree: Tree_Type) return Data is
   begin
      return Tree.Item;
   end Item;

   procedure Destroy_Tree(N: in out Tree_Type) is
      procedure free is new Ada.Unchecked_Deallocation(Node, Tree_Type);
   begin
      if not Empty(N) then
         Destroy_Tree(N.Left);
         Destroy_Tree(N.Right);
         Free(N);
      end if;
   end Destroy_Tree;

   function Tree(Value: Data; Left, Right : Tree_Type) return Tree_Type is
      Temp : Tree_Type := new Node;
   begin
      Temp.all := (Left, Right, Value);
      return Temp;
   end Tree;

   function Tree(Value: Data) return Tree_Type is
   begin
      return Tree(Value, null, null);
   end Tree;

end Bin_Trees;
```


Next, we specify and implement package that defines a task type for tree traversal. This allows us to run any number of tree traversals in parallel, even on the same tree.


```Ada
   generic
      with procedure Process_Data(Item: Data);
      with function Stop return Boolean;
      with procedure Finish;
   package Bin_Trees.Traverse is
      task Inorder_Task is
         entry Run(Tree: Tree_Type);
         -- this will call each Item in Tree and, at the very end, it will call Finish
         -- except when Stop becomes true; in this case, the task terminates
      end Inorder_Task;
   end Bin_Trees.Traverse;
```



```Ada
   package body Bin_Trees.Traverse is
      task body Inorder_Task is
         procedure Inorder(Tree: Tree_Type) is
         begin
            if not Empty(Tree) and not Stop then
               Inorder(Tree.Left);
               if not Stop then
                  Process_Data(Item => Tree.Item);
               end if;
               if (not Stop) then
                  Inorder(Tree.Right);
               end if;
            end if;
         end Inorder;
         T: Tree_Type;
      begin
         accept Run(Tree: Tree_Type) do
            T := Tree;
         end Run;
         Inorder(T);
         Finish;
      end Inorder_Task;
   end Bin_Trees.Traverse;
```


When comparing two trees T1 and T2, we will define two tasks, a "Producer.Inorder_Task" and a "Consumer.Inorder_Task". The producer will write data items to a buffer, the consumer will read items from the buffer and compare them with its own data items. Both tasks will terminate when the consumer finds a data item different from the one written by the producer, or when either task has written its last item and the other one has items left.

A third auxiliary task just waits until the consumer has finished and the result of the fringe comparison can be read.


```Ada
with Ada.Text_IO, Bin_Trees.Traverse;

procedure Main is

   package B_Trees is new Bin_Trees(Character); use B_Trees;

   function Same_Fringe(T1, T2: Tree_Type) return Boolean is

      protected type Buffer_Type is
         entry Write(Item: Character);
         entry Write_Done;
         entry Read_And_Compare(Item: Character);
         entry Read_Done;
         entry Wait_For_The_End;
         function Early_Abort return Boolean;
         function The_Same return Boolean;
      private
         Current: Character;
         Readable: Boolean := False;
         Done: Boolean := False;
         Same: Boolean := True;
         Finished: Boolean := False;
      end Buffer_Type;

      protected body Buffer_Type is

         entry Write(Item: Character) when not Readable is
         begin
            Readable := True;
            Current  := Item;
         end Write;

         entry Write_Done when not Readable is
         begin
            Readable := True;
            Done     := True;
         end Write_Done;

         entry Read_And_Compare(Item: Character) when Readable is
         begin
            if Done then -- Producer is already out of items
               Same := False;
               Finished := True;
               -- Readable remains True, else Consumer might lock itself out
            elsif
              Item /= Current then
               Same := False;
               Finished := True;
               Readable := False;
            else
                 Readable := False;
            end if;
         end Read_And_Compare;

         entry Read_Done when Readable is
         begin
            Readable := False;
            Same     := Same and Done;
            Finished := True;
         end Read_Done;

         entry Wait_For_The_End when (Finished) or (not Same) is
         begin
            null; -- "when ..." is all we need
         end Wait_For_The_End;

         function The_Same return Boolean is
         begin
            return Same;
         end The_Same;

         function Early_Abort return Boolean is
         begin
            return not The_Same or Finished;
         end Early_Abort;

      end Buffer_Type;

      Buffer: Buffer_Type;

      -- some wrapper subprogram needed to instantiate the generics below

          procedure Prod_Write(Item: Character) is
          begin
             Buffer.Write(Item);
          end Prod_Write;

          function Stop return Boolean is
          begin
             return Buffer.Early_Abort;
          end Stop;

          procedure Prod_Stop is
          begin
             Buffer.Write_Done;
          end Prod_Stop;

          procedure Cons_Write(Item: Character) is
          begin
             Buffer.Read_And_Compare(Item);
          end Cons_Write;

          procedure Cons_Stop is
          begin
             Buffer.Read_Done;
          end Cons_Stop;

      package Producer is new B_Trees.Traverse(Prod_Write, Stop, Prod_Stop);
      package Consumer is new B_Trees.Traverse(Cons_Write, Stop, Cons_Stop);

   begin
      Producer.Inorder_Task.Run(T1);
      Consumer.Inorder_Task.Run(T2);
      Buffer.Wait_For_The_End;
      return Buffer.The_Same;
   end Same_Fringe;

   procedure Show_Preorder(Tree: Tree_Type; Prefix: String := "") is
      use Ada.Text_IO;
   begin
      if Prefix /= "" then
         Ada.Text_IO.Put(Prefix);
      end if;
      if not Empty(Tree) then
         Put("(" & Item(Tree));      Put(", ");
         Show_Preorder(Left(Tree));  Put(", ");
         Show_Preorder(Right(Tree)); Put(")");
      end if;
      if Prefix /= "" then
         New_Line;
      end if;
   end Show_Preorder;

   T_0: Tree_Type := Tree('a', Empty, Tree('b'));
   T: array(1 .. 5) of Tree_Type;

begin
   T(1) := Tree('d', Tree('c'), T_0);
   T(2)  := Tree('c', Empty, Tree('a', Tree('d'), Tree('b')));
   T(3)  := Tree('e', T(1), T(2));
   T(4)  := Tree('e', T(2), T(1));
   T(5)  := Tree('e', T_0, Tree('c', Tree('d'), T(1)));

   -- First display the trees you have (in preorder)
   for I in T'Range loop
      Show_Preorder(T(I), "Tree(" & Integer'Image(I) & " ) is ");
   end loop;
   Ada.Text_IO.New_Line;

   -- Now compare them, which have the same fringe?
   for I in T'Range loop
      for J in T'Range loop
         if Same_Fringe(T(J), T(I)) then
            Ada.Text_IO.Put("same(");
         else
            Ada.Text_IO.Put("DIFF(");
         end if;
         Ada.Text_IO.Put(Integer'Image(I) & "," & Integer'Image(J) & " ); ");
      end loop;
      Ada.Text_IO.New_Line;
   end loop;
end Main;
```


Note that we do not call Destroy_Tree to reclaim the dynamic memory. In our case, this is not needed since the memory will be reclaimed at the end of Main, anyway.

```txt
Tree( 1 ) is (d, (c, , ), (a, , (b, , )))
Tree( 2 ) is (c, , (a, (d, , ), (b, , )))
Tree( 3 ) is (e, (d, (c, , ), (a, , (b, , ))), (c, , (a, (d, , ), (b, , ))))
Tree( 4 ) is (e, (c, , (a, (d, , ), (b, , ))), (d, (c, , ), (a, , (b, , ))))
Tree( 5 ) is (e, (a, , (b, , )), (c, (d, , ), (d, (c, , ), (a, , (b, , )))))

same( 1, 1 ); same( 1, 2 ); DIFF( 1, 3 ); DIFF( 1, 4 ); DIFF( 1, 5 );
same( 2, 1 ); same( 2, 2 ); DIFF( 2, 3 ); DIFF( 2, 4 ); DIFF( 2, 5 );
DIFF( 3, 1 ); DIFF( 3, 2 ); same( 3, 3 ); same( 3, 4 ); DIFF( 3, 5 );
DIFF( 4, 1 ); DIFF( 4, 2 ); same( 4, 3 ); same( 4, 4 ); DIFF( 4, 5 );
DIFF( 5, 1 ); DIFF( 5, 2 ); DIFF( 5, 3 ); DIFF( 5, 4 ); same( 5, 5 );
```


## Bracmat


```Bracmat
( ( T
  =
    .   ( next
        =   node stack rhs
          .   !arg:%?node ?stack
            &   whl
              ' ( !node:(?node.?rhs)
                & !rhs !stack:?stack
                )
            & (!node.!stack)
        )
      & !arg:(?stackA,?stackB)
      &   whl
        ' ( !stackA:~
          & !stackB:~
          & next$!stackA:(?leafA.?stackA)
          & next$!stackB:(?leafB.?stackB)
          & !leafA:!leafB
          )
      & out$!arg
      & (   !stackA:!stackB:
          & !leafA:!leafB
          & out$equal
        | out$"not equal"
        )
  )
& T$(x,x)
& T$((x.y),(x.y))
& T$(((x.y).z),(x.y.z))
& T$((x.y.z),(x.y.q))
& T$((x.y),(x.y.q))
& T$((x.y.z),(x.y))
& T$(((x.y).z),(x.z.y))
&   T
  $ ( (a.b.c.(x.y).z)
    , (((a.b).c).x.y.z)
    )
);
```

Output:

```txt
x,x
equal
(x.y),(x.y)
equal
((x.y).z),(x.y.z)
equal
(x.y.z),(x.y.q)
not equal
(x.y),(x.y.q)
not equal
(x.y.z),(x.y)
not equal
((x.y).z),(x.z.y)
not equal
  (a.b.c.(x.y).z)
, (((a.b).c).x.y.z)
equal
```


## C

With rudimentary coroutine support based on ucontext.  I don't know if it will compile on anything other than GCC.

```c
#include <stdio.h>
#include <stdlib.h>
#include <ucontext.h>

typedef struct {
	ucontext_t caller, callee;
	char stack[8192];
	void *in, *out;
} co_t;

co_t * co_new(void(*f)(), void *data)
{
	co_t * c = malloc(sizeof(*c));
	getcontext(&c->callee);
	c->in = data;

	c->callee.uc_stack.ss_sp = c->stack;
	c->callee.uc_stack.ss_size = sizeof(c->stack);
	c->callee.uc_link = &c->caller;
	makecontext(&c->callee, f, 1, (int)c);

	return c;
}

void co_del(co_t *c)
{
	free(c);
}

inline void
co_yield(co_t *c, void *data)
{
	c->out = data;
	swapcontext(&c->callee, &c->caller);
}

inline void *
co_collect(co_t *c)
{
	c->out = 0;
	swapcontext(&c->caller, &c->callee);
	return c->out;
}

// end of coroutine stuff

typedef struct node node;
struct node {
	int v;
	node *left, *right;
};

node *newnode(int v)
{
	node *n = malloc(sizeof(node));
	n->left = n->right = 0;
	n->v = v;
	return n;
}

void tree_insert(node **root, node *n)
{
	while (*root) root = ((*root)->v > n->v)
				? &(*root)->left
				: &(*root)->right;
	*root = n;
}

void tree_trav(int x)
{
	co_t *c = (co_t *) x;

	void trav(node *root) {
		if (!root) return;
		trav(root->left);
		co_yield(c, root);
		trav(root->right);
	}

	trav(c->in);
}

int tree_eq(node *t1, node *t2)
{
	co_t *c1 = co_new(tree_trav, t1);
	co_t *c2 = co_new(tree_trav, t2);

	node *p = 0, *q = 0;
	do {
		p = co_collect(c1);
		q = co_collect(c2);
	} while (p && q && (p->v == q->v));

	co_del(c1);
	co_del(c2);
	return !p && !q;
}

int main()
{
	int x[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1 };
	int y[] = { 2, 5, 7, 1, 9, 0, 6, 4, 8, 3, -1 };
	int z[] = { 0, 1, 2, 3, 4, 5, 6, 8, 9, -1 };

	node *t1 = 0, *t2 = 0, *t3 = 0;

	void mktree(int *buf, node **root) {
		int i;
		for (i = 0; buf[i] >= 0; i++)
			tree_insert(root, newnode(buf[i]));
	}

	mktree(x, &t1); // ordered binary tree, result of traversing
	mktree(y, &t2); // should be independent of insertion, so t1 == t2
	mktree(z, &t3);

	printf("t1 == t2: %s\n", tree_eq(t1, t2) ? "yes" : "no");
	printf("t1 == t3: %s\n", tree_eq(t1, t3) ? "yes" : "no");

	return 0;
}
```

## C#
This task is almost custom designed for C# LINQ and is really trivial using that.  Most of the following is support code.  The only two routines that actually implement the task at hand are CompareTo and GetLeaves at the bottom.  GetLeaves is a really simple BinTree procedure to retreive the leaves from left to right into an IEnumerable.  That IEnumerable can be zipped with the result of GetLeaves on another tree and the results compared giving us our final answer and since everything is deferred in LINQ, this has the desirable property spoken of in the problem's description that no comparisons are done after a non-matching pair.

```c#

using System;
using System.Collections.Generic;
using System.Linq;

namespace Same_Fringe
{
	class Program
	{
		static void Main()
		{
			var rnd = new Random(110456);
			var randList = Enumerable.Range(0, 20).Select(i => rnd.Next(1000)).ToList();
			var bt1 = new BinTree<int>(randList);
			// Shuffling will create a tree with the same values but different topology
			Shuffle(randList, 428);
			var bt2 = new BinTree<int>(randList);
			Console.WriteLine(bt1.CompareTo(bt2) ? "True compare worked" : "True compare failed");
			// Insert a 0 in the first tree which should cause a failure
			bt1.Insert(0);
			Console.WriteLine(bt1.CompareTo(bt2) ? "False compare failed" : "False compare worked");
		}

		static void Shuffle<T>(List<T> values, int seed)
		{
			var rnd = new Random(seed);

			for (var i = 0; i < values.Count - 2; i++)
			{
				var iSwap = rnd.Next(values.Count - i) + i;
				var tmp = values[iSwap];
				values[iSwap] = values[i];
				values[i] = tmp;
			}
		}
	}

	// Define other methods and classes here
	class BinTree<T> where T:IComparable
	{
		private BinTree<T> _left;
		private BinTree<T> _right;
		private T _value;

		private BinTree<T> Left
		{
			get { return _left; }
		}

		private BinTree<T> Right
		{
			get { return _right; }
		}

		// On interior nodes, any value greater than or equal to Value goes in the
		// right subtree, everything else in the left.
		private T Value
		{
			get { return _value; }
		}

		public bool IsLeaf { get { return Left == null; } }

		private BinTree(BinTree<T> left, BinTree<T> right, T value)
		{
			_left = left;
			_right = right;
			_value = value;
		}

		public BinTree(T value) : this(null, null, value) { }

		public BinTree(IEnumerable<T> values)
		{
			// ReSharper disable PossibleMultipleEnumeration
			_value = values.First();
			foreach (var value in values.Skip(1))
			{
				Insert(value);
			}
			// ReSharper restore PossibleMultipleEnumeration
		}

		public void Insert(T value)
		{
			if (IsLeaf)
			{
				if (value.CompareTo(Value) < 0)
				{
					_left = new BinTree<T>(value);
					_right = new BinTree<T>(Value);
				}
				else
				{
					_left = new BinTree<T>(Value);
					_right = new BinTree<T>(value);
					_value = value;
				}
			}
			else
			{
				if (value.CompareTo(Value) < 0)
				{
					Left.Insert(value);
				}
				else
				{
					Right.Insert(value);
				}
			}
		}

		public IEnumerable<T> GetLeaves()
		{
			if (IsLeaf)
			{
				yield return Value;
				yield break;
			}
			foreach (var val in Left.GetLeaves())
			{
				yield return val;
			}
			foreach (var val in Right.GetLeaves())
			{
				yield return val;
			}
		}

		internal bool CompareTo(BinTree<T> other)
		{
			return other.GetLeaves().Zip(GetLeaves(), (t1, t2) => t1.CompareTo(t2) == 0).All(f => f);
		}
	}
}

```


Example output:

```txt

True Compare worked
False Compare worked

```



## Clojure

''fringe-seq'' produces a lazy sequence of the fringe values of a tree.
It's patterned after the standard function ''tree-seq'': aside from the ''tree'' to walk,
it takes 3 function arguments to handle a general tree structure.
''branch?'' returns true for branch nodes -- nodes which could have children, whether they
actually do or not.
''children'' returns the children of a branch node; ''content'' returns the content of a branch node.
A fringe value is either the content of a branch node without children, or a non-branch node.

```clojure
(defn fringe-seq [branch? children content tree]
  (letfn [(walk [node]
            (lazy-seq
              (if (branch? node)
                (if (empty? (children node))
                  (list (content node))
                  (mapcat walk (children node)))
                (list node))))]
    (walk tree)))
```

For this problem, binary trees are represented as vectors, whose nodes are either [''content'' ''left'' ''right''] or just ''content''.

```clojure
(defn vfringe-seq [v] (fringe-seq vector? #(remove nil? (rest %)) first v))
(println (vfringe-seq [10 1 2])) ; (1 2)
(println (vfringe-seq [10 [1 nil nil] [20 2 nil]])) ; (1 2)
```

Then we can use a general sequence-equality function:

```clojure
(defn seq= [s1 s2]
  (cond
    (and (empty? s1) (empty? s2)) true
    (not= (empty? s1) (empty? s2)) false
    (= (first s1) (first s2)) (recur (rest s1) (rest s2))
    :else false))
```



## D


### Short Version

A short recursive solution that is not lazy (and lacks the const/immutable/pure/nothrow):

```d
struct Node(T) {
    T data;
    Node* L, R;
}

bool sameFringe(T)(Node!T* t1, Node!T* t2) {
    T[] scan(Node!T* t) {
        if (!t) return [];
        return (!t.L && !t.R) ? [t.data] : scan(t.L) ~ scan(t.R);
    }
    return scan(t1) == scan(t2);
}

void main() {
    import std.stdio;
    alias N = Node!int;
    auto t1 = new N(10, new N(20, new N(30, new N(40), new N(50))));
    auto t2 = new N(1, new N(2, new N(3, new N(40), new N(50))));
    writeln(sameFringe(t1, t2));
    auto t3 = new N(1, new N(2, new N(3, new N(40), new N(51))));
    writeln(sameFringe(t1, t3));
}
```

```txt
true
false
```



### Strong Lazy Version

This version is quite long because it tries to be reliable. The code contains contracts, unit tests, annotations, and so on.

```d
import std.array: empty;
import std.algorithm: equal;


// Replace with an efficient stack when available in Phobos.
struct Stack(T) {
    private T[] data;

    public @property bool empty() const pure nothrow {
        return data.empty;
    }

    // Can't be const if T isn't a value or const.
    public @property T head() const pure nothrow
    in {
        assert(!data.empty);
    } body {
        return data[$ - 1];
    }

    public void push(T x) pure nothrow {
        data ~= x;
    }

    public void pop() pure nothrow
    in {
        assert(!data.empty);
    } body {
        data.length--;
    }
}


struct BinaryTreeNode(T) {
    T data;
    BinaryTreeNode* left, right;
}


struct Fringe(T) {
    alias const(BinaryTreeNode!T)* BT;
    private Stack!BT stack;

    pure nothrow invariant {
        assert(stack.empty || isLeaf(stack.head));
    }

    public this(BT t) pure nothrow {
        if (t != null) {
            stack.push(t);
            if (!isLeaf(t)) {
                // Here the invariant doesn't hold.
                // invariant isn't called for private methods.
                nextLeaf;
            }
        }
    }

    public @property bool empty() const pure nothrow {
        return stack.empty;
    }

    public @property T front() const pure nothrow
    in {
        assert(!stack.empty && stack.head != null);
    } body {
        return stack.head.data;
    }

    public void popFront() pure nothrow
    in {
        assert(!stack.empty);
    } body {
        stack.pop();
        if (!empty())
            nextLeaf();
    }

    private static bool isLeaf(in BT t) pure nothrow {
        return t != null && t.left == null && t.right == null;
    }

    private void nextLeaf() pure nothrow
    in {
        assert(!stack.empty);
    } body {
        auto t = stack.head;

        while (!stack.empty && !isLeaf(t)) {
            stack.pop();
            if (t.right != null)
                stack.push(t.right);
            if (t.left != null)
                stack.push(t.left);
            t = stack.head;
        }
    }
}


bool sameFringe(T)(in BinaryTreeNode!T* t1, in BinaryTreeNode!T* t2)
pure nothrow {
    return Fringe!T(t1).equal(Fringe!T(t2));
}


unittest {
    alias BinaryTreeNode!int N;

    static N* n(in int x, N* l=null, N* r=null) pure nothrow {
        return new N(x, l, r);
    }

    {
        N* t;
        assert(sameFringe(t, t));
    }

    {
        const t1 = n(10);
        const t2 = n(10);
        assert(sameFringe(t1, t2));
    }

    {
        const t1 = n(10);
        const t2 = n(20);
        assert(!sameFringe(t1, t2));
    }

    {
        const t1 = n(10, n(20));
        const t2 = n(30, n(20));
        assert(sameFringe(t1, t2));
    }

    {
        const t1 = n(10, n(20));
        const t2 = n(10, n(30));
        assert(!sameFringe(t1, t2));
    }

    {
        const t1 = n(10, n(20), n(30));
        const t2 = n(5, n(20), n(30));
        assert(sameFringe(t1, t2));
    }

    {
        const t1 = n(10, n(20), n(30));
        const t2 = n(5, n(20), n(35));
        assert(!sameFringe(t1, t2));
    }

    {
        const t1 = n(10, n(20, n(30)));
        const t2 = n(1, n(2, n(30)));
        assert(sameFringe(t1, t2));
    }

    {
        const t1 = n(10, n(20, n(30, n(40), n(50))));
        const t2 = n(1, n(2, n(3, n(40), n(50))));
        assert(sameFringe(t1, t2));
    }

    {
        const t1 = n(10, n(20, n(30, n(40), n(50))));
        const t2 = n(1, n(2, n(3, n(40), n(51))));
        assert(!sameFringe(t1, t2));
    }
}


void main() {
    import std.stdio;
    alias N = BinaryTreeNode!int;

    static N* n(in int x, N* l=null, N* r=null) pure nothrow {
        return new N(x, l, r);
    }

    const t1 = n(10, n(20, n(30, n(40), n(50))));
    writeln("fringe(t1): ", Fringe!int(t1));

    const t2 = n(1, n(2, n(3, n(40), n(50))));
    writeln("fringe(t2): ", Fringe!int(t2));

    const t3 = n(1, n(2, n(3, n(40), n(51))));
    writeln("fringe(t3): ", Fringe!int(t3));

    writeln("sameFringe(t1, t2): ", sameFringe(t1, t2));
    writeln("sameFringe(t1, t3): ", sameFringe(t1, t3));
}
```

```txt
fringe(t1): [40, 50]
fringe(t2): [40, 50]
fringe(t3): [40, 51]
sameFringe(t1, t2): true
sameFringe(t1, t3): false
```


===Range Generator Version (Lazy)===

```d
import std.stdio, std.concurrency, std.range, std.algorithm;

struct Node(T) {
    T data;
    Node* L, R;
}

Generator!T fringe(T)(Node!T* t1) {
    return new typeof(return)({
        if (t1 != null) {
            if (t1.L == null && t1.R == null) // Is a leaf.
                yield(t1.data);
            else
                foreach (data; t1.L.fringe.chain(t1.R.fringe))
                    yield(data);
        }
    });
}

bool sameFringe(T)(Node!T* t1, Node!T* t2) {
    return t1.fringe.equal(t2.fringe);
}

void main() {
    alias N = Node!int;

    auto t1 = new N(10, new N(20, new N(30, new N(40), new N(50))));
    auto t2 = new N(1, new N(2, new N(3, new N(40), new N(50))));
    sameFringe(t1, t2).writeln;

    auto t3 = new N(1, new N(2, new N(3, new N(40), new N(51))));
    sameFringe(t1, t3).writeln;

    auto t4 = new N(1, new N(2, new N(3, new N(40))));
    sameFringe(t1, t4).writeln;

    N* t5;
    sameFringe(t1, t5).writeln;
    sameFringe(t5, t5).writeln;

    auto t6 = new N(2);
    auto t7 = new N(1, new N(2));
    sameFringe(t6, t7).writeln;
}
```

```txt
true
false
false
false
true
true
```



## Go


```go
package main

import "fmt"

type node struct {
    int
    left, right *node
}

// function returns a channel that yields the leaves of the tree.
// the channel is closed after all leaves are received.
func leaves(t *node) chan int {
    ch := make(chan int)
    // recursive function to walk tree.
    var f func(*node)
    f = func(n *node) {
        if n == nil {
            return
        }
        // leaves are identified by having no children.
        if n.left == nil && n.right == nil {
            ch <- n.int
        } else {
            f(n.left)
            f(n.right)
        }
    }
    // goroutine runs concurrently with others.
    // it walks the tree then closes the channel.
    go func() {
        f(t)
        close(ch)
    }()
    return ch
}

func sameFringe(t1, t2 *node) bool {
    f1 := leaves(t1)
    f2 := leaves(t2)
    for l1 := range f1 {
        // both trees must yield a leaf, and the leaves must be equal.
        if l2, ok := <-f2; !ok || l1 != l2 {
            return false
        }
    }
    // there must be nothing left in f2 after consuming all of f1.
    _, ok := <-f2
    return !ok
}

func main() {
    // the different shapes of the trees is shown with indention.
    // the leaves are easy to spot by the int: key.
    t1 := &node{3,
        &node{1,
            &node{int: 1},
            &node{int: 2}},
        &node{8,
            &node{int: 5},
            &node{int: 13}}}
    // t2 with negative values for internal nodes that can't possibly match
    // positive values in t1, just to show that only leaves are being compared.
    t2 := &node{-8,
        &node{-3,
            &node{-1,
                &node{int: 1},
                &node{int: 2}},
            &node{int: 5}},
        &node{int: 13}}
    fmt.Println(sameFringe(t1, t2)) // prints true.
}
```



## Haskell

Since Haskell is lazy, simply getting the fringes and comparing them for equality will do. It will only do as much as work as necessary and will stop at the first difference.

To get the fringe, we can simply use the solution for [[Flatten a list#Haskell|Flatten a list]], slightly modified for a binary tree instead of a general tree:

```haskell
data Tree a
  = Leaf a
  | Node (Tree a)
         (Tree a)
  deriving (Show, Eq)

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Node n1 n2) = fringe n1 ++ fringe n2

sameFringe
  :: (Eq a)
  => Tree a -> Tree a -> Bool
sameFringe t1 t2 = fringe t1 == fringe t2

main :: IO ()
main = do
  let a = Node (Leaf 1) (Node (Leaf 2) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5))))
      b = Node (Leaf 1) (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 4) (Leaf 5)))
      c = Node (Node (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5)
      x =
        Node
          (Leaf 1)
          (Node
             (Leaf 2)
             (Node (Leaf 3) (Node (Leaf 4) (Node (Leaf 5) (Leaf 6)))))
      y = Node (Leaf 0) (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 4) (Leaf 5)))
      z = Node (Leaf 1) (Node (Leaf 2) (Node (Node (Leaf 4) (Leaf 3)) (Leaf 5)))
  mapM_ print $ sameFringe a <$> [a, b, c, x, y, z]
```

```txt
True
True
True
False
False
False
```


=={{header|Icon}} and {{header|Unicon}}==

The following solution works in both languages:


```unicon
procedure main()
    aTree := [1, [2, [4, [7]], [5]], [3, [6, [8], [9]]]]
    bTree := [1, [2, [4, [7]], [5]], [3, [6, [8], [9]]]]
    write("aTree and bTree ",(sameFringe(aTree,bTree),"have")|"don't have",
          " the same leaves.")
    cTree := [1, [2, [4, [7]], [5]], [3, [6, [8]]]]
    dTree := [1, [2, [4, [7]], [5]], [3, [6, [8], [9]]]]
    write("cTree and dTree ",(sameFringe(cTree,dTree),"have")|"don't have",
          " the same leaves.")
end

procedure sameFringe(a,b)
    return same{genLeaves(a),genLeaves(b)}
end

procedure same(L)
   while n1 := @L[1] do {
      n2 := @L[2] | fail
      if n1 ~== n2 then fail
      }
   return not @L[2]
end

procedure genLeaves(t)
    suspend (*(node := preorder(t)) == 1, node[1])
end

procedure preorder(L)
    if \L then suspend L | preorder(L[2|3])
end
```


Output:


```txt

->sf
aTree and bTree have the same leaves.
cTree and dTree don't have the same leaves.
->

```



## J



```J
sameFringe=: -:&([: ; <S:0)
```


Note that the time/space optimizations here can change with the language implementation, but current implementations make no effort to treat trees efficiently.

That said, note also that binary trees tend to be a poor data structure choice in J.  First, they shift the focus form "what needs to be done" to (in minute detail) "how to do it".  This typically means that (for example) combining operations into batches becomes difficult.  And, typically, we can find other strategies (some of which have analogies to trees) to achieve the desired efficiencies.

Anyways, here's a recursive routine to convert a flat list into a binary tree:


```J
list2tree=: (<.@-:@# ({. ,&<&list2tree}. ) ])^:(1<#)
```


And, here are two differently structured trees which represent the same underlying data:


```J
bp=: list2tree p: i.11
ubp=: p:L:0] 10;~list2tree i.10
```


And, here's our original operation in action (<code>1 {:: ubp</code> is a subtree of ubp which omits a leaf node):


```J
   ubp sameFringe bp
1
   bp sameFringe 1 {:: ubp
0
```



## Java


The code defines a Node interface, an implementation (SimpleNode), and a pair of methods to do the comparison (areLeavesSame and advanceToLeaf). The method simpleWalk() is to show what leaves are present in each tree.

'''Code:'''

```java
import java.util.*;

class SameFringe
{
  public interface Node<T extends Comparable<? super T>>
  {
    Node<T> getLeft();
    Node<T> getRight();
    boolean isLeaf();
    T getData();
  }

  public static class SimpleNode<T extends Comparable<? super T>> implements Node<T>
  {
    private final T data;
    public SimpleNode<T> left;
    public SimpleNode<T> right;

    public SimpleNode(T data)
    {  this(data, null, null);  }

    public SimpleNode(T data, SimpleNode<T> left, SimpleNode<T> right)
    {
      this.data = data;
      this.left = left;
      this.right = right;
    }

    public Node<T> getLeft()
    {  return left;  }

    public Node<T> getRight()
    {  return right;  }

    public boolean isLeaf()
    {  return ((left == null) && (right == null));  }

    public T getData()
    {  return data;  }

    public SimpleNode<T> addToTree(T data)
    {
      int cmp = data.compareTo(this.data);
      if (cmp == 0)
        throw new IllegalArgumentException("Same data!");
      if (cmp < 0)
      {
        if (left == null)
          return (left = new SimpleNode<T>(data));
        return left.addToTree(data);
      }
      if (right == null)
        return (right = new SimpleNode<T>(data));
      return right.addToTree(data);
    }
  }

  public static <T extends Comparable<? super T>> boolean areLeavesSame(Node<T> node1, Node<T> node2)
  {
    Stack<Node<T>> stack1 = new Stack<Node<T>>();
    Stack<Node<T>> stack2 = new Stack<Node<T>>();
    stack1.push(node1);
    stack2.push(node2);
    // NOT using short-circuit operator
    while (((node1 = advanceToLeaf(stack1)) != null) & ((node2 = advanceToLeaf(stack2)) != null))
      if (!node1.getData().equals(node2.getData()))
        return false;
    // Return true if finished at same time
    return (node1 == null) && (node2 == null);
  }

  private static <T extends Comparable<? super T>> Node<T> advanceToLeaf(Stack<Node<T>> stack)
  {
    while (!stack.isEmpty())
    {
      Node<T> node = stack.pop();
      if (node.isLeaf())
        return node;
      Node<T> rightNode = node.getRight();
      if (rightNode != null)
        stack.push(rightNode);
      Node<T> leftNode = node.getLeft();
      if (leftNode != null)
        stack.push(leftNode);
    }
    return null;
  }

  public static void main(String[] args)
  {
    SimpleNode<Integer> headNode1 = new SimpleNode<Integer>(35, new SimpleNode<Integer>(25, new SimpleNode<Integer>(15, new SimpleNode<Integer>(10), new SimpleNode<Integer>(20)), new SimpleNode<Integer>(30)), new SimpleNode<Integer>(45, new SimpleNode<Integer>(40), new SimpleNode<Integer>(50)));
    SimpleNode<Integer> headNode2 = new SimpleNode<Integer>(24, new SimpleNode<Integer>(14, new SimpleNode<Integer>(10), new SimpleNode<Integer>(16, null, new SimpleNode<Integer>(20))), new SimpleNode<Integer>(34, new SimpleNode<Integer>(30), new SimpleNode<Integer>(42, new SimpleNode<Integer>(40), new SimpleNode<Integer>(56, new SimpleNode<Integer>(50), null))));
    SimpleNode<Integer> headNode3 = new SimpleNode<Integer>(24, new SimpleNode<Integer>(14, new SimpleNode<Integer>(10), new SimpleNode<Integer>(16, null, new SimpleNode<Integer>(20))), new SimpleNode<Integer>(34, new SimpleNode<Integer>(30), new SimpleNode<Integer>(42, new SimpleNode<Integer>(40), new SimpleNode<Integer>(50, null, new SimpleNode<Integer>(56)))));
    System.out.print("Leaves for set 1: ");
    simpleWalk(headNode1);
    System.out.println();
    System.out.print("Leaves for set 2: ");
    simpleWalk(headNode2);
    System.out.println();
    System.out.print("Leaves for set 3: ");
    simpleWalk(headNode3);
    System.out.println();
    System.out.println("areLeavesSame(1, 2)? " + areLeavesSame(headNode1, headNode2));
    System.out.println("areLeavesSame(2, 3)? " + areLeavesSame(headNode2, headNode3));
  }

  public static void simpleWalk(Node<Integer> node)
  {
    if (node.isLeaf())
      System.out.print(node.getData() + " ");
    else
    {
      Node<Integer> left = node.getLeft();
      if (left != null)
        simpleWalk(left);
      Node<Integer> right = node.getRight();
      if (right != null)
        simpleWalk(right);
    }
  }
}
```


'''Output:'''

```txt
Leaves for set 1: 10 20 30 40 50
Leaves for set 2: 10 20 30 40 50
Leaves for set 3: 10 20 30 40 56
areLeavesSame(1, 2)? true
areLeavesSame(2, 3)? false
```



## jq

A binary tree can be conveniently represented in jq as a nested array, e.g. [1,[2,3]]. This is the data structure used by same_fringe(t;u) as defined in this section.

With this data structure, a test for whether two trees, s and t, have the same fringes could be implemented simply as:

```jq
(t|flatten) == (s|flatten)
```

but this entails generating the lists of leaves.

To accomplish the "same fringe" task efficiently in jq 1.4 without generating a list of leaves, a special-purpose function is needed.  This special-purpose function, which is here named "next", would ordinarily be defined as an inner function of "same_fringe", but for clarity, it is defined as a top-level function.

```jq
# "next" allows one to generate successive leaves, one at a time. This is accomplished
# by ensuring that the non-null output of a call to "next" can also serve as input.
#
# "next" returns null if there are no more leaves, otherwise it returns [leaf, nodes]
# where "leaf" is the next leaf, and nodes is an array of nodes still to be traversed.
# Input has the same form, but on input, "leaf" is ignored unless it is an array.
def next:
 def _next:
     .[0] as $node | .[1] as $nodes
      | if ($node|type) == "array" then
          if $node|length != 2 then
            error("improper node: \($node) should have 2 items") else . end
          | [ $node[0],  [$node[1]] + $nodes]
        elif $nodes|length > 0 then  [$nodes[0], $nodes[1:]]
        else null
        end;
  _next as $n
  | if $n == null then null
    elif ($n[0]|type) == "array" then $n|next
    else $n
    end;

# t and u must be binary trees
def same_fringe(t;u):
  # x and y must be suitable for input to "next"
  def eq(x;y):
    if x == null then y == null
    elif y == null then false
    elif x[0] != y[0] then false
    else eq( x|next;  y|next)
    end;

   eq([t,[]]|next; [u,[]]|next) ;
```

'''Example''':

```jq
  [1,[2,[3,[4,[5,[6,7]]]]]] as $a
  | [[[[[[1,2],3],4],5],6],7] as $b
  | [[[1,2],3],[4,[5,[6,7]]]] as $c
  | [[[1,2],2],4] as $d
  |  same_fringe($a;$a), same_fringe($b;$b), same_fringe($c;$c),
     same_fringe($a;$b), same_fringe($a;$c), same_fringe($b;$c),
     same_fringe($a;$d), same_fringe($d;$c), same_fringe($b;$d),

     same_fringe( ["a",["b",["c",[["x","y"],"z"]]]];
                  [[["a","b"],"c"],["x",["y","z"]]] )
```

```sh
$ jq -n -f Same_Fringe.jq
true
true
true
true
true
true
false
false
false
true

```



## Julia


```Julia

using Lazy

"""
    Input a tree for display as a fringed structure.
"""
function fringe(tree)
    fringey(node::Pair) = [fringey(i) for i in node]
    fringey(leaf::Int) = leaf
    fringey(tree)
end


"""
    equalsfringe() uses a reduction to a lazy 1D list via
    getleaflist() for its "equality" of fringes
"""
getleaflist(tree::Int) = [tree]
getleaflist(tree::Pair) = vcat(getleaflist(seq(tree[1])), getleaflist(seq(tree[2])))
getleaflist(tree::Lazy.LazyList) = vcat(getleaflist(tree[1]), getleaflist(tree[2]))
getleaflist(tree::Void) = []
equalsfringe(t1, t2) = (getleaflist(t1) == getleaflist(t2))


a = 1 => 2 => 3 => 4 => 5 => 6 => 7 => 8
b = 1 => (( 2 => 3 ) => (4 => (5 => ((6 => 7) => 8))))
c = (((1 => 2) => 3) => 4) => 5 => 6 => 7 => 8

x = 1 => 2 => 3 => 4 => 5 => 6 => 7 => 8 => 9
y = 0 => 2 => 3 => 4 => 5 => 6 => 7 => 8
z = 1 => 2 => (4 => 3) => 5 => 6 => 7 => 8

prettyprint(s) = println(replace("$s", r"\{Any,1\}|Any|Array\{T,1\}\swhere\sT|Array|", ""))
prettyprint(fringe(a))
prettyprint(fringe(b))
prettyprint(fringe(c))
prettyprint(fringe(x))
prettyprint(fringe(y))
prettyprint(fringe(z))

prettyprint(getleaflist(a))
prettyprint(getleaflist(b))
prettyprint(getleaflist(c))

println(equalsfringe(a, a))
println(equalsfringe(a, b))
println(equalsfringe(a, c))
println(equalsfringe(b, c))
println(equalsfringe(a, x) == false)
println(equalsfringe(a, y) == false)
println(equalsfringe(a, z) == false)

```

```txt

[1, [2, [3, [4, [5, [6, [7, 8]]]]]]]
[1, [[2, 3], [4, [5, [[6, 7], 8]]]]]
[[[[1, 2], 3], 4], [5, [6, [7, 8]]]]
[1, [2, [3, [4, [5, [6, [7, [8, 9]]]]]]]]
[0, [2, [3, [4, [5, [6, [7, 8]]]]]]]
[1, [2, [[4, 3], [5, [6, [7, 8]]]]]]
[1, 2, 3, 4, 5, 6, 7, 8]
[1, 2, 3, 4, 5, 6, 7, 8]
[1, 2, 3, 4, 5, 6, 7, 8]
true
true
true
true
true
true
true

```



## OCaml

While we could use a lazy datatype such as [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Stream.html Stream] for this problem, this example implements the short-circuit behavior (returning on first mismatch) by tracking the parse state.

```OCaml
type 'a btree = Leaf of 'a | BTree of ('a btree * 'a btree)

let rec next = function
  | [] -> None
  | h :: t -> match h with
    | Leaf x -> Some (x,t)
    | BTree(a,b) -> next (a::b::t)

let samefringe t1 t2 =
  let rec aux s1 s2 = match (next s1, next s2) with
    | None, None -> true
    | None, _ | _, None -> false
    | Some(a,b), Some(c,d) -> (a=c) && aux b d in
  aux [t1] [t2]

(* Test: *)
let () =
  let u = BTree(Leaf 1, BTree(Leaf 2, Leaf 3)) in
  let v = BTree(BTree(Leaf 1, Leaf 2), Leaf 3) in
  let w = BTree(BTree(Leaf 3, Leaf 2), Leaf 1) in
  let check a b =
    print_endline (if samefringe a b then "same" else "different") in
  check u v; check v u; check v w;
```

Output:

```txt
same
same
different
```



## Perl

We use a pair of tree iterators to walk through the trees.  So we pick the next leaf from each tree while the leaves are identical.  If we've picked the last leaf of both trees simulaneously, then both trees had the "same" fringe.  If we find a difference or one of the trees runs out of leaves before the other, we immediately return with a "different" fringe.

The tree iterator is pretty simple:  we use array references with index 0 as the left subtree and index 1 holding the right subtree.  So as we go down the tree towards the first leaf, we push each right subtree that we will consider later onto the rtree stack.  Eventually, we'll hit a leaf and return it.  The next time we go into the iterator, we simply pull off the last deferred subtree and continue the process.


```perl

#!/usr/bin/perl
use strict;

my @trees = (
    # 0..2 are same
    [ 'd', [ 'c', [ 'a', 'b', ], ], ],
    [ [ 'd', 'c' ], [ 'a', 'b' ] ],
    [ [ [ 'd', 'c', ], 'a', ], 'b', ],
    # and this one's different!
    [ [ [ [ [ [ 'a' ], 'b' ], 'c', ], 'd', ], 'e', ], 'f' ],
);

for my $tree_idx (1 .. $#trees) {
    print "tree[",$tree_idx-1,"] vs tree[$tree_idx]: ",
           cmp_fringe($trees[$tree_idx-1], $trees[$tree_idx]), "\n";
}

sub cmp_fringe {
    my $ti1 = get_tree_iterator(shift);
    my $ti2 = get_tree_iterator(shift);
    while (1) {
        my ($L, $R) = ($ti1->(), $ti2->());
        next if defined($L) and defined($R) and $L eq $R;
        return "Same" if !defined($L) and !defined($R);
        return "Different";
    }
}

sub get_tree_iterator {
    my @rtrees = (shift);
    my $tree;
    return sub {
        $tree = pop @rtrees;
        ($tree, $rtrees[@rtrees]) = @$tree while ref $tree;
        return $tree;
    }
}

```

```txt

tree[0] vs tree[1]: Same
tree[1] vs tree[2]: Same
tree[2] vs tree[3]: Different
```



## Perl 6

Unlike in Perl 5, where <tt>=></tt> is just a synonym for comma, in Perl 6 it creates a true Pair object.  So here we use Pair objects for our "cons" cells, just as if we were doing this in Lisp.  We use the <tt>gather/take</tt> construct to harvest the leaves lazily as the elements are visited with a standard recursive algorithm, using multiple dispatch to differentiate nodes from leaves.  The <tt>eqv</tt> value equivalence is applied to the two lists in parallel.

```perl6
sub fringe ($tree) {
    multi sub fringey (Pair $node) { fringey $_ for $node.kv; }
    multi sub fringey ( Any $leaf) { take $leaf; }

    gather fringey $tree;
}

sub samefringe ($a, $b) { fringe($a) eqv fringe($b) }

# Testing:

my $a = 1 => 2 => 3 => 4 => 5 => 6 => 7 => 8;
my $b = 1 => (( 2 => 3 ) => (4 => (5 => ((6 => 7) => 8))));
my $c = (((1 => 2) => 3) => 4) => 5 => 6 => 7 => 8;

my $x = 1 => 2 => 3 => 4 => 5 => 6 => 7 => 8 => 9;
my $y = 0 => 2 => 3 => 4 => 5 => 6 => 7 => 8;
my $z = 1 => 2 => (4 => 3) => 5 => 6 => 7 => 8;

say  so samefringe $a, $a;
say  so samefringe $a, $b;
say  so samefringe $a, $c;

say not samefringe $a, $x;
say not samefringe $a, $y;
say not samefringe $a, $z;
```

```txt
True
True
True
True
True
True
```



## Phix


```Phix
--
-- demo\rosetta\Same_Fringe.exw
--
### ======================

--
--  Requires 0.7.5 or later (implementation revealed that task_yield did not
--                           have side effects of e_all properly set.)
--
constant tests = {{0,1,{0,2,0}},
                  {{0,1,0},2,0},
                  {{0,1,0},2,{0,3,0}},
                 }

sequence tasks
integer res = 0
sequence sdata = repeat(0,2)

integer active_tasks
integer show_details = 1

procedure scan(sequence tree, integer level, integer tidx)
object {left,data,right} = tree
    if res=0 then
        if left!=0 then scan(left,level+1,tidx) end if
        sdata[tidx] = data
        if show_details then
            printf(1,"task[%d] sets sdata[%d] to ",tidx)
            ?data
        end if
        if res=0 then
            task_suspend(task_self())
            task_yield()
        end if
        if right!=0 then scan(right,level+1,tidx) end if
    end if
    if level=1 then
        if show_details then
            printf(1,"task[%d] ends\n",tidx)
        end if
        active_tasks -= 1
        tasks[tidx] = 0
        sdata[tidx] = -1 -- (or use a separate flag)
    end if
end procedure

?"started"
procedure test(integer t1, integer t2)
    tasks = {task_create(routine_id("scan"),{tests[t1],1,1}),
             task_create(routine_id("scan"),{tests[t2],1,2})}
    active_tasks = 2
    res = 0
    while active_tasks>0 do
        if tasks[1] then
            task_schedule(tasks[1],1)
            task_yield()
        end if
        if tasks[2] then
            task_schedule(tasks[2],1)
            task_yield()
        end if
        if res=0 then
            res = compare(sdata[1],sdata[2])
            if show_details then
                ?{res,sdata[1],sdata[2],active_tasks}
            end if
        end if
    end while
    printf(1,"test(%d,%d):%d\n",{t1,t2,res})
end procedure

test(1,1)
show_details = 0
test(1,2)
test(1,3)
test(2,1)
test(2,2)
test(2,3)
test(3,1)
test(3,2)
test(3,3)
```

```txt

"started"
task[1] sets sdata[1] to 1
task[2] sets sdata[2] to 1
{0,1,1,2}
task[1] sets sdata[1] to 2
task[2] sets sdata[2] to 2
{0,2,2,2}
task[1] ends
task[2] ends
{0,-1,-1,0}
test(1,1):0
test(1,2):0
test(1,3):-1
test(2,1):0
test(2,2):0
test(2,3):-1
test(3,1):1
test(3,2):1
test(3,3):0

```



## PicoLisp

This uses coroutines to traverse the trees, so it works only in the 64-bit version.

```PicoLisp
(de nextLeaf (Rt Tree)
   (co Rt
      (recur (Tree)
         (when Tree
            (recurse (cadr Tree))
            (yield (car Tree))
            (recurse (cddr Tree)) ) ) ) )

(de cmpTrees (Tree1 Tree2)
   (prog1
      (use (Node1 Node2)
         (loop
            (setq
               Node1 (nextLeaf "rt1" Tree1)
               Node2 (nextLeaf "rt2" Tree2) )
            (T (nor Node1 Node2) T)
            (NIL (= Node1 Node2)) ) )
      (co "rt1")
      (co "rt2") ) )
```

Test:

```PicoLisp
: (balance '*Tree1 (range 1 7))
-> NIL
: (for N (5 4 6 3 7 1 2) (idx '*Tree2 N T))
-> NIL

: (view *Tree1 T)
      7
   6
      5
4
      3
   2
      1
-> NIL

: (view *Tree2 T)
      7
   6
5
   4
      3
            2
         1
-> NIL

: (cmpTrees *Tree1 *Tree2)
-> T
```



## Python

This solution visits lazily the two trees in lock step like in the Perl 6 example, and stops at the first miss-match.

```python
try:
    from itertools import zip_longest as izip_longest # Python 3.x
except:
    from itertools import izip_longest                # Python 2.6+

def fringe(tree):
    """Yield tree members L-to-R depth first,
    as if stored in a binary tree"""
    for node1 in tree:
        if isinstance(node1, tuple):
            for node2 in fringe(node1):
                yield node2
        else:
            yield node1

def same_fringe(tree1, tree2):
    return all(node1 == node2 for node1, node2 in
               izip_longest(fringe(tree1), fringe(tree2)))

if __name__ == '__main__':
    a = 1, 2, 3, 4, 5, 6, 7, 8
    b = 1, (( 2, 3 ), (4, (5, ((6, 7), 8))))
    c = (((1, 2), 3), 4), 5, 6, 7, 8

    x = 1, 2, 3, 4, 5, 6, 7, 8, 9
    y = 0, 2, 3, 4, 5, 6, 7, 8
    z = 1, 2, (4, 3), 5, 6, 7, 8

    assert same_fringe(a, a)
    assert same_fringe(a, b)
    assert same_fringe(a, c)

    assert not same_fringe(a, x)
    assert not same_fringe(a, y)
    assert not same_fringe(a, z)
```

There is no output, which signifies success.


## Racket



###  Lazy Language


The same fringe problem is one of the classic cases where a lazy
language solution is extremely simple: just flatten the two trees and
compare the resulting lists.  Racket has a lazy language implementation,
but instead of using it for the whole code, the following program has
just the tree comparison part defined in the lazy language, and it gets
used outside, using the plain default Racket language --- in the test
submodule.  To verify this code, put it in some file, and run it with
“<tt>raco test <i>the-file.rkt</i></tt>.  The same exact test module can
be added to any of the following variations, it's omitted for brevity.


```racket

#lang racket

(module same-fringe lazy
  (provide same-fringe?)
  (define (same-fringe? t1 t2)
    (! (equal? (flatten t1) (flatten t2))))
  (define (flatten tree)
    (if (list? tree)
      (apply append (map flatten tree))
      (list tree))))

(require 'same-fringe)

(module+ test
  (require rackunit)
  (check-true (same-fringe? '((1 2 3) ((4 5 6) (7 8)))
                            '(((1 2 3) (4 5 6)) (7 8))))
  (check-false (same-fringe? '((1 2 3) ((4 5 6) (7 8)))
                             '(((1 2 3) (4 6)) (8)))))

```


```txt
raco test: (submod "/some/file.rkt" test)
2 tests passed
```




###  Channels and Threads


This version flattens the trees into channels, and then compares the
contents of the two channels.  Each call to <tt>fringe->channel</tt>
creates a channel for the element stream, then fires up a (green) thread
that feeds it.


```racket

#lang racket

(define (fringe->channel tree)
  (define ch (make-channel))
  (thread (λ() (let loop ([tree tree])
                 (if (list? tree) (for-each loop tree) (channel-put ch tree)))
               (channel-put ch (void)))) ; mark the end
  ch)

(define (same-fringe? tree1 tree2)
  (define ch1 (fringe->channel tree1))
  (define ch2 (fringe->channel tree2))
  (let loop ()
    (let ([x1 (channel-get ch1)] [x2 (channel-get ch2)])
      (and (equal? x1 x2) (or (void? x1) (loop))))))

```


Channels are just a one of several thread-communication devices which
could have been used, including a simple unix-like pipe-based solution.
Note the limit on the amount of allowed buffering: it can be any
(finite) value, since there are three independent threads that are
running.


```racket

#lang racket

(define (pipe-fringe tree)
  (define-values [I O] (make-pipe 100))
  (thread (λ() (let loop ([tree tree])
                 (if (list? tree) (for-each loop tree) (fprintf O "~s\n" tree)))
               (close-output-port O)))
  I)

(define (same-fringe? tree1 tree2)
  (define i1 (pipe-fringe tree1))
  (define i2 (pipe-fringe tree2))
  (let loop ()
    (let ([x1 (read i1)] [x2 (read i2)])
      (and (equal? x1 x2) (or (eof-object? x1) (loop))))))

```



###  Generators


This version is very similar, except that now we use generators:


```racket

#lang racket
(require racket/generator)

(define (fringe-generator tree)
  (generator ()
    (let loop ([tree tree])
      (if (list? tree) (for-each loop tree) (yield tree)))))

(define (same-fringe? tree1 tree2)
  (define g1 (fringe-generator tree1))
  (define g2 (fringe-generator tree2))
  (let loop ()
    (let ([x1 (g1)] [x2 (g2)])
      (and (equal? x1 x2) (or (void? x1) (loop))))))

```




###  Continuations


Finally, this is a more low-level solution, using continuation conreol
operators.  The following is a slight modification of the
<tt>same-fringe?</tt> program from Dorai Sitaram's 1993 PLDI paper
titled "Handling Control". This solution uses the <tt>fcontrol</tt>
delimited continuation operator.


```racket

#lang racket

(require racket/control)

(define (fringe-iterator tree)
  (λ() (let loop ([tree tree])
         (if (list? tree) (for-each loop tree) (fcontrol tree)))
       (fcontrol (void))))

(define (same-fringe? tree1 tree2)
  (let loop ([iter1 (fringe-iterator tree1)]
             [iter2 (fringe-iterator tree2)])
    (% (iter1)
       (λ (x1 iter1)
         (% (iter2)
            (λ (x2 iter2)
              (and (equal? x1 x2)
                   (or (void? x1) (loop iter1 iter2)))))))))

```



## REXX



### Version 1 using father node



```REXX
/* REXX ***************************************************************
* Same Fringe
*           1                A                 A
*          / \              / \               / \
*         /   \            /   \             /   \
*        /     \          /     \           /     \
*       2       3        B       C         B       C
*      / \     /        / \     /         / \     /
*     4   5   6        D   E   F         D   E   F
*    /       / \      /       / \       /       / \
*   7       8   9    G       H   I     G       *   I
*
* 23.08.2012 Walter Pachl derived from
*                            http://rosettacode.org/wiki/Tree_traversal
* Tree A: A B D G E C F H I
* Tree B: A B D G E C F * I
**********************************************************************/
debug=0
node.=0
lvl=0

Call mktree 'A'
Call mktree 'B'

done.=0
za=root.a; leafa=node.a.za.0name
zb=root.a; leafb=node.b.zb.0name
done.a.za=1
done.b.zb=1
Do i=1 To 12
  if leafa=leafb Then Do
    If leafa=0 Then Do
      Say 'Fringes are equal'
      Leave
      End
    Say leafa '=' leafb
    Do j=1 To 12 Until done.a.za=0
      za=go_next(za,'A'); leafa=node.a.za.0name
      End
    done.a.za=1
    Do j=1 To 12 Until done.b.zb=0
      zb=go_next(zb,'B'); leafb=node.b.zb.0name
      End
    done.b.zb=1
    End
  Else Do
    Select
      When leafa=0 Then
        Say leafb 'exceeds leaves in tree A'
      When leafb=0 Then
        Say leafa 'exceeds leaves in tree B'
      Otherwise
        Say 'First difference' leafa '<>' leafb
      End
    Leave
    End
  End
Exit


note:
/**********************************************************************
* add the node to the preorder list unless it's already there
* add the node to the level list
**********************************************************************/
  Parse Arg z,t
  If z<>0 &,                           /* it's a node                */
     done.z=0 Then Do                  /* not yet done               */
    wl.t=wl.t z                        /* add it to the preorder list*/
    ll.lvl=ll.lvl z                    /* add it to the level list   */
    done.z=1                           /* remember it's done         */
    leafl=leafl node.t.z.0name
    End
  Return

go_next: Procedure Expose node. lvl
/**********************************************************************
* find the next node to visit in the treewalk
**********************************************************************/
  next=0
  Parse arg z,t
  If node.t.z.0left<>0 Then Do         /* there is a left son        */
    If node.t.z.0left.done=0 Then Do   /* we have not visited it     */
      next=node.t.z.0left              /* so we go there             */
      node.t.z.0left.done=1            /* note we were here          */
      lvl=lvl+1                        /* increase the level         */
      End
    End
  If next=0 Then Do                    /* not moved yet              */
    If node.t.z.0rite<>0 Then Do       /* there is a right son       */
      If node.t.z.0rite.done=0 Then Do /* we have not visited it     */
        next=node.t.z.0rite            /* so we go there             */
        node.t.z.0rite.done=1          /* note we were here          */
        lvl=lvl+1                      /* increase the level         */
        End
      End
    End
  If next=0 Then Do                    /* not moved yet              */
    next=node.t.z.0father              /* go to the father           */
    lvl=lvl-1                          /* decrease the level         */
    End
  Return next                          /* that's the next node       */
                                       /* or zero if we are done     */

mknode: Procedure Expose node.
/**********************************************************************
* create a new node
**********************************************************************/
  Parse Arg name,t
  z=node.t.0+1
  node.t.z.0name=name
  node.t.z.0father=0
  node.t.z.0left =0
  node.t.z.0rite =0
  node.t.0=z
  Return z                        /* number of the node just created */

attleft: Procedure Expose node.
/**********************************************************************
* make son the left son of father
**********************************************************************/
  Parse Arg son,father,t
  node.t.son.0father=father
  z=node.t.father.0left
  If z<>0 Then Do
    node.t.z.0father=son
    node.t.son.0left=z
    End
  node.t.father.0left=son
  Return

attrite: Procedure Expose node.
/**********************************************************************
* make son the right son of father
**********************************************************************/
  Parse Arg son,father,t
  node.t.son.0father=father
  z=node.t.father.0rite
  If z<>0 Then Do
    node.t.z.0father=son
    node.t.son.0rite=z
    End
  node.t.father.0rite=son
  le=node.t.father.0left
  If le>0 Then
    node.t.le.0brother=node.t.father.0rite
  Return

mktree: Procedure Expose node. root.
/**********************************************************************
* build the tree according to the task
**********************************************************************/
  Parse Arg t
  If t='A' Then Do
    a=mknode('A',t); root.t=a
    b=mknode('B',t); Call attleft b,a,t
    c=mknode('C',t); Call attrite c,a,t
    d=mknode('D',t); Call attleft d,b,t
    e=mknode('E',t); Call attrite e,b,t
    f=mknode('F',t); Call attleft f,c,t
    g=mknode('G',t); Call attleft g,d,t
    h=mknode('H',t); Call attleft h,f,t
    i=mknode('I',t); Call attrite i,f,t
    End
  Else Do
    a=mknode('A',t); root.t=a
    b=mknode('B',t); Call attleft b,a,t
    c=mknode('C',t); Call attrite c,a,t
    d=mknode('D',t); Call attleft d,b,t
    e=mknode('E',t); Call attrite e,b,t
    f=mknode('F',t); Call attleft f,c,t
    g=mknode('G',t); Call attleft g,d,t
    h=mknode('*',t); Call attleft h,f,t
    i=mknode('I',t); Call attrite i,f,t
    End
  Return
```

Output:

```txt

 A = A
 B = B
 D = D
 G = G
 E = E
 C = C
 F = F
 First difference H <> *

```



### Version 2 without using father node


```rexx
/* REXX ***************************************************************
* Same Fringe
=           1                A                 A
=          / \              / \               / \
=         /   \            /   \             /   \
=        /     \          /     \           /     \
=       2       3        B       C         B       C
=      / \     /        / \     /         / \     /
=     4   5   6        D   E   F         D   E   F
=    /       / \      /       / \       /       / \
=   7       8   9    G       H   I     G       *   I
=
* 23.08.2012 Walter Pachl derived from
*                            http://rosettacode.org/wiki/Tree_traversal
* Tree A: A B D G E C F H I
* Tree B: A B D G E C F * I
**********************************************************************/
node.=0

Call mktree 'A'
Call mktree 'B'

sideboard.=0

za=root.a; leafa=node.a.za.0name
zb=root.b; leafb=node.b.zb.0name
Do i=1 To 20 Until za=0 & zb=0
  If leafa=leafb Then Do
    Say leafa '=' leafb
    Parse Value get_next(za,'A') with za leafa
    Parse Value get_next(zb,'B') with zb leafb
    End
  Else Do
    Select
      When za=0 Then Say leafb 'exceeds tree A'
      When zb=0 Then Say leafa 'exceeds tree B'
      Otherwise Say 'First difference' leafa '<>' leafb
      End
    Leave
    Exit
    End
  End
exit

get_next: Procedure Expose node. sideboard.
  Parse Arg za,t
  Select
    When node.t.za.0left<>0 Then Do
      If node.t.za.0rite<>0 Then Do
        z=sideboard.t.0+1
        sideboard.t.z=node.t.za.0rite
        sideboard.t.0=z
        End
      za=node.t.za.0left
      End
    When node.t.za.0rite<>0 Then Do
      za=node.t.za.0rite
      End
    Otherwise Do
      z=sideboard.t.0
      za=sideboard.t.z
      z=z-1
      sideboard.t.0=z
      End
    End
  Return za node.t.za.0name

mknode: Procedure Expose node.
/**********************************************************************
* create a new node
**********************************************************************/
  Parse Arg name,t
  z=node.t.0+1
  node.t.z.0name=name
  node.t.z.0father=0
  node.t.z.0left =0
  node.t.z.0rite =0
  node.t.0=z
  Return z                        /* number of the node just created */

attleft: Procedure Expose node.
/**********************************************************************
* make son the left son of father
**********************************************************************/
  Parse Arg son,father,t
  node.t.son.0father=father
  z=node.t.father.0left
  If z<>0 Then Do
    node.t.z.0father=son
    node.t.son.0left=z
    End
  node.t.father.0left=son
  Return

attrite: Procedure Expose node.
/**********************************************************************
* make son the right son of father
**********************************************************************/
  Parse Arg son,father,t
  node.t.son.0father=father
  z=node.t.father.0rite
  If z<>0 Then Do
    node.t.z.0father=son
    node.t.son.0rite=z
    End
  node.t.father.0rite=son
  le=node.t.father.0left
  If le>0 Then
    node.t.le.0brother=node.t.father.0rite
  Return

mktree: Procedure Expose node. root.
/**********************************************************************
* build the tree according to the task
**********************************************************************/
  Parse Arg t
  If t='A' Then Do
    a=mknode('A',t); root.t=a
    b=mknode('B',t); Call attleft b,a,t
    c=mknode('C',t); Call attrite c,a,t
    d=mknode('D',t); Call attleft d,b,t
    e=mknode('E',t); Call attrite e,b,t
    f=mknode('F',t); Call attleft f,c,t
    g=mknode('G',t); Call attleft g,d,t
    h=mknode('H',t); Call attleft h,f,t
    i=mknode('I',t); Call attrite i,f,t
    End
  Else Do
    a=mknode('A',t); root.t=a
    b=mknode('B',t); Call attleft b,a,t
    c=mknode('C',t); Call attrite c,a,t
    d=mknode('D',t); Call attleft d,b,t
    e=mknode('E',t); Call attrite e,b,t
    f=mknode('F',t); Call attleft f,c,t
    g=mknode('G',t); Call attleft g,d,t
    h=mknode('*',t); Call attleft h,f,t
    i=mknode('I',t); Call attrite i,f,t
    End
  Return
```

Output is the same as for Version 1


### version 1.1

This REXX example is a re─written program that mimics the first version (above).

This version has:
:::*   elided a subroutine
:::*   elided superfluous   '''do ── end'''   groups
:::*   elided some stemmed array tails
:::*   elided some REXX variables   (LVL, DEBUG, ···)
:::*   simplified some stem names
:::*   displays the tree   (as an ASCII display)
:::*   changed ''TREE'' names so as to not conflict with ''LEAF'' names
:::*   uses non─case sensitive tree names
:::*   used boolean based variables as ''logicals''
:::*   expanded message texts
:::*   combined subroutines   '''ATTLEFT'''   and   '''ATTRIGHT'''   into one
:::*   streamlined the   '''MAKE_TREE'''   subroutine

```rexx
/*REXX pgm examines the leaves of 2 binary trees (as shown below), and finds inequities.*/
_=left('', 28);           say _   "        A                                       A     "
                          say _   "       / \    ◄════1st tree                    / \    "
                          say _   "      /   \                                   /   \   "
                          say _   "     /     \                                 /     \  "
                          say _   "    B       C                               B       C "
                          say _   "   / \     /              2nd tree════►    / \     /  "
                          say _   "  D   E   F                               D   E   F   "
                          say _   " /       / \                             /       / \  "
                          say _   "G       H   I                           G       δ   I "
say
#=0;   done.=0;   @.=0                           /*initialize:   #, leaves, DONE., nodes*/
call make_tree  1                                /*define tree number 1  (the 1st tree).*/
call make_tree  2                                /*   "     "     "   2  (the 2nd   " ).*/
z1=root.1;   L1=@.1.z1;   done.1.z1=1            /*L1:    is a leaf on tree number  1.  */
z2=z1;       L2=@.2.z2;   done.2.z2=1            /*L2:     " "   "   "   "     "    2.  */

  do # % 2                                       /*loop for the number of (tree) leaves.*/
  if L1==L2 then do
                 if L1==0  then call sayX   'The trees are equal.'
                 say  '    The '     L1     " leaf is identical in both trees."
                              do until \done.1.z1;   z1=go_next(z1, 1);   L1=@.1.z1;   end
                 done.1.z1=1
                              do until \done.2.z2;   z2=go_next(z2, 2);   L2=@.2.z2;   end
                 done.2.z2=1
                 end
            else select
                 when L1==0  then call sayX L2  'exceeds leaves in 1st tree'
                 when L2==0  then call sayX L1  'exceeds leaves in 2nd tree'
                 otherwise        call sayX     'A difference is: '    L1   '¬='   L2
                 end   /*select*/
  end   /*# % 2*/
exit
/*──────────────────────────────────────────────────────────────────────────────────────*/
go_next: procedure expose @.;   arg q,t;        next=.    /*find next node in the tree. */
         if @.t.q._Lson\==0  &,                           /*there a left branch in tree?*/
            @.t.q._Lson.vis==0  then do                   /*has this node been visited? */
                                     next= @.t.q._Lson    /*point to the  ──► next node.*/
                                     @.t.q._Lson.vis= 1   /*the  leftside is completed. */
                                     end
         if next==. &,
            @.t.q._Rson\==0  &,                           /*there a right tree branch ? */
            @.t.q._Rson.vis==0  then do                   /*has this node been visited? */
                                     next= @.t.q._Rson    /*──► next node. */
                                     @.t.q._Rson.vis= 1   /*the rightside is completed. */
                                     end
         if next==.             then next= @.t.q._dad     /*process the  father  node.  */
         return next                                      /*next node  (or 0,  if done).*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
make_node: parse arg name,t;    #=# +1;       q= @.t.0 +1 /*make new node/branch on tree*/
           @.t.q= name;    @.t.q._Lson= 0;    @.t.0= q
           @.t.q._dad=0;   @.t.q._Rson= 0;    return q    /*number of node just created.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
make_tree: procedure expose @. root. #; parse arg tree    /*construct a couple of trees.*/
           if tree==1  then hhh='H'                       /*    [↓]  must be a wood duck*/
                       else hhh='δ'                       /*the odd duck in the tree.   */
                                     a=make_node('A', tree);                   root.tree=a
                                     b=make_node('B', tree);    call son 'L', b, a, tree
                                     c=make_node('C', tree);    call son 'R', c, a, tree
                                     d=make_node('D', tree);    call son 'L', d, b, tree
                                     e=make_node('E', tree);    call son 'R', e, b, tree
                                     f=make_node('F', tree);    call son 'L', f, c, tree
                                     g=make_node('G', tree);    call son 'L', g, d, tree
           /*quacks like a duck?*/   h=make_node(hhh, tree);    call son 'L', h, f, tree
                                     i=make_node('I', tree);    call son 'R', i, f, tree
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
sayX: say;      say arg(1);   say;       exit             /*display a message and exit. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
son:  procedure expose @.;    parse arg ?,son,dad,t;    LR= '_'?"SON"
      @.t.son._dad= dad;      q= @.t.dad.LR               /* [↓]  define which son.     */
      if q\==0   then do;     @.t.q._dad= son;          @.t.son.LR= q;       end
      @.t.dad.LR= son;               return
```

```txt

                                     A                                       A
                                    / \    ◄════1st tree                    / \
                                   /   \                                   /   \
                                  /     \                                 /     \
                                 B       C                               B       C
                                / \     /              2nd tree════►    / \     /
                               D   E   F                               D   E   F
                              /       / \                             /       / \
                             G       H   I                           G       δ   I

    The  A  leaf is identical in both trees.
    The  B  leaf is identical in both trees.
    The  D  leaf is identical in both trees.
    The  G  leaf is identical in both trees.
    The  E  leaf is identical in both trees.
    The  C  leaf is identical in both trees.
    The  F  leaf is identical in both trees.

A difference is:  H ¬= δ

```



## Scheme


Descend provides a list, or stack, of the leftmost ''unvisited'' nodes at each level of the tree. Two such lists are used as cursors to keep track of the remaining nodes. The main loop compares the top of each list (ie the leftmost remaining node) and breaks with ''false'' if different, or calls Ascend to update the lists. Updating may require calling Descend again if more unvisited left-nodes are found. If the end of both lists is reached simultaneously, and therefore the end of both trees, ''true'' is returned.


```Scheme
; binary tree helpers from "Structure and Interpretation of Computer Programs" 2.3.3
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; returns a list of leftmost nodes from each level of the tree
(define (descend tree ls)
  (if (null? (left-branch tree))
    (cons tree ls)
    (descend (left-branch tree) (cons tree ls))))

; updates the list to contain leftmost nodes from each remaining level
(define (ascend ls)
  (cond
    ((and (null? (cdr ls)) (null? (right-branch (car ls)))) '())
    ((null? (right-branch (car ls))) (cdr ls))
    (else
      (let ((ls (cons (right-branch (car ls))
		  (cdr ls))))
	    (if (null? (left-branch (car ls)))
	      ls
	      (descend (left-branch (car ls)) ls))))))

; loops thru each list until the end (true) or nodes are unequal (false)
(define (same-fringe? t1 t2)
  (let next ((l1 (descend t1 '()))
	     (l2 (descend t2 '())))
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1)
	   (null? l2)
	   (not (eq? (entry (car l1)) (entry (car l2))))) #f)
      (else (next (ascend l1) (ascend l2))))))
```


```txt
> (same-fringe? (list 1 '() (list 2 '() (list 3 '() '()))) (list 3 (list 2 (list 1 '() '()) '()) '()))
#t
```



## Sidef

```ruby
var trees = [
    # 0..2 are same
    [ 'd', [ 'c', [ 'a', 'b', ], ], ],
    [ [ 'd', 'c' ], [ 'a', 'b' ] ],
    [ [ [ 'd', 'c', ], 'a', ], 'b', ],
    # and this one's different!
    [ [ [ [ [ [ 'a' ], 'b' ], 'c', ], 'd', ], 'e', ], 'f' ],
];

func get_tree_iterator(*rtrees) {
    var tree;
    func {
        tree = rtrees.pop;
        while (defined(tree) && tree.is_an(Array)) {
            rtrees.append(tree[1]);
            tree = tree[0];
        }
        return tree;
    }
}

func cmp_fringe(a, b) {
    var ti1 = get_tree_iterator(a);
    var ti2 = get_tree_iterator(b);
    loop {
        var (L, R) = (ti1(), ti2());
         defined(L) &&  defined(R) && (L == R) && next;
        !defined(L) && !defined(R) && return "Same";
        return "Different";
    }
}

range(1, trees.end).each { |tree_idx|
    say ("tree[#{tree_idx-1}] vs tree[#{tree_idx}]: ",
           cmp_fringe(trees[tree_idx-1], trees[tree_idx]));
}
```


```txt

tree[0] vs tree[1]: Same
tree[1] vs tree[2]: Same
tree[2] vs tree[3]: Different

```



## Tcl

```tcl
package require Tcl 8.6
package require struct::tree

# A wrapper round a coroutine for iterating over the leaves of a tree in order
proc leafiterator {tree} {
    coroutine coro[incr ::coroutines] apply {tree {
	yield [info coroutine]
	$tree walk [$tree rootname] node {
	    if {[$tree isleaf $node]} {
		yield $node
	    }
	}
	yieldto break
    }} $tree
}

# Compare two trees for equality of their leaf node names
proc samefringe {tree1 tree2} {
    set c1 [leafiterator $tree1]
    set c2 [leafiterator $tree2]
    try {
	while 1 {
	    if {[set l1 [$c1]] ne [set l2 [$c2]]} {
		puts "$l1 != $l2";    # Just so we can see where we failed
		return 0
	    }
	}
	return 1
    } finally {
	rename $c1 {}
	rename $c2 {}
    }
}
```

Demonstrating:

```tcl
# Make some trees to compare...
struct::tree t1 deserialize {
    root {} {}
      a 0 {}
        d 3 {}
        e 3 {}
      b 0 {}
      c 0 {}
}
struct::tree t2 deserialize {
    root {} {}
      a 0 {}
        d 3 {}
        e 3 {}
      b 0 {}
      cc 0 {}
}

# Print the boolean result of doing the comparison
puts [samefringe t1 t2]
```

```txt

c != cc
0

```



## zkl

```zkl
var G=Utils.Generator;
//Tree: (node,left,right) or (leaf) or (node,left) ...
aTree := T(1, T(2, T(4, T(7)), T(5)), T(3, T(6, T(8), T(9))));
bTree := aTree;
println("aTree and bTree ",sameFringe(aTree,bTree) and "have" or "don't have",
          " the same leaves.");
cTree := T(1, T(2, T(4, T(7)), T(5)), T(3, T(6, T(8))));
dTree := T(1, T(2, T(4, T(7)), T(5)), T(3, T(6, T(8), T(9))));
println("cTree and dTree ",sameFringe(cTree,dTree) and "have"or"don't have",
          " the same leaves.");

fcn sameFringe(a,b){ same(G(genLeaves,a),G(genLeaves,b)) }

fcn same(g1,g2){ //(Generator,Generator)
   foreach n1,n2 in (g1.zip(g2)){ //-->(int,int) ...
      if(n1 != n2) return(); // == return(Void)
   }
   return(not (g2._next() or g2._next())); //-->False if g1 or g2 has leaves
}

fcn genLeaves(tree){
   switch(tree.len()){ // (), (leaf), (node,left, [right])
      case(1){ vm.yield(tree[0]) } // leaf: int
      case(2){ genLeaves(tree[1]); }
      else   { genLeaves(tree[1]); genLeaves(tree[2]); }
   }
}
```

```txt

aTree and bTree have the same leaves.
cTree and dTree don't have the same leaves.

```

