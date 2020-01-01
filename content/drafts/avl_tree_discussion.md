+++
title = "AVL Tree/Discussion"
description = ""
date = 2016-07-18T12:04:34Z
aliases = []
[extra]
id = 21012
[taxonomies]
categories = []
tags = []
+++

= Binary Search Trees (Unbalanced Trees) =

This article is oriented towards formulating the correct solution to the class Set using AVL Trees. Several examples of unbalanced trees will be given first (unbalanced trees are often referred to as Binary Search Trees). Then the full source of balanced binary trees (AVL Trees) will be displayed.

The simplest of data structures that support the set membership operator is an unbalanced tree. Set elements are stored in sorted ordered. This is achieved by a set node which is shown below.


```c#

class SetNode<T> where T : IComparable<T>
{
    public T Data;
    public SetNode<T> Left;
    public SetNode<T> Right;

    public SetNode(T t)
    {
        Data = t;
        Left = null;
        Right = null;
    }
}

```


Modern programming languages such as C# and C++ have facilities for expressing set theory. C# has generics and C++ has templates. This allows for the concept of a set of type T (where T is the element type expressed as a parameter). The generic notation for a set of T in C# is Set<T>.

Above is a node for a set of T, where T is the generic class parameter. Note that the generic parameter T inherits from IComparable of T (is constrained in other words). IComparable of T is shown below.


```c#

public interface IComparable<T>
{
 int CompareTo(T value);
}

```


This is an interface class containing a single method. Note that interface classes contain no code - they merely define the form of virtual functions. Virtual functions are functions that are called through pointers and hence it is determined at runtime which version of the function is called.

The virtual method CompareTo:


```txt

returns <0 when the current instance is less than the argument,
returns 0 when the current instance is equal to the argument,
returns >0 when the current instance is greater than the argument.

```


The built in data types such as int, float, double, string etc all support the interface IComparable (non-generic form). This means that inherantly .Net supports sets and other collections on the primary datatypes without having to define comparability for those types. The IComparable of T interface is .Net's method of defining order on datatypes.

In the above, T derives from IComparable<T>, so T has a CompareTo method. This means that instances of T can be compared.

A set node contains a reference to an object of type T (which is comparable). It also contains a left and right reference to child nodes. The first program uses this definition to place the nodes into a data structure that orders the elements. From a given node, smaller nodes are placed on the left, and greater nodes are placed on the right. A program that does this is shown below.


```c#

// Set1 - Binary Search Tree - Constraints
// Copyright Benedict McNamara 2006 - 2014, All Rights Reserved

using System;
using System.Collections.Generic;

class SetNode<T> where T : IComparable<T>
{
    public T Data;
    public SetNode<T> Left;
    public SetNode<T> Right;

    public SetNode(T t)
    {
        Data = t;
        Left = null;
        Right = null;
    }
}

class Set<T> where T : IComparable<T>
{
    SetNode<T> Root;

    public void Add(T Value)
    {
        if (Root == null)
            Root = new SetNode<T>(Value);
        else
        {
            SetNode<T> Search = Root;
            for (; ; )
            {
                int Compare = Value.CompareTo(Search.Data);

                if (Compare == 0)
                    throw new EntryAlreadyExistsException();

                else if (Compare < 0)
                {
                    if (Search.Left != null)
                        Search = Search.Left;
                    else
                    {
                        Search.Left = new SetNode<T>(Value);
                        break;
                    }
                }

                else
                {
                    if (Search.Right != null)
                        Search = Search.Right;
                    else
                    {
                        Search.Right = new SetNode<T>(Value);
                        break;
                    }
                }
            }
        }
    }

    public bool this[T Key]
    {
        get
        {
            if (Root == null)
                throw new EntryNotFoundException();
            else
            {
                SetNode<T> Search = Root;

                do
                {
                    int Result = Key.CompareTo(Search.Data);

                    if (Result < 0) Search = Search.Left;

                    else if (Result > 0) Search = Search.Right;

                    else break;

                } while (Search != null);

                if (Search == null)
                    return false;
                else
                    return true;
            }
        }
    }
}

public class EntryNotFoundException : Exception
{
    static String message = "The requested entry could not be located in the specified collection.";

    public EntryNotFoundException() : base(message) { }
}

public class EntryAlreadyExistsException : Exception
{
    static String message = "The entry already resides in the collection.";

    public EntryAlreadyExistsException() : base(message) { }
}

class Program
{
    static void Main()
    {
        Set<int> S = new Set<int>();

        for (int i = 1; i < 5; i++)
            S.Add(i);

        for (int i = 0; i < 5; i++)
            Console.WriteLine("S[{0}] = {1}", i, S[i]);
    }
}

```


The method Add moves left and right in the tree to place a new entry in the tree. New entries are placed in the tree at a leaf of the tree. Note that the method CompareTo of the IComparable of T interface is used to determine the order of new entries. An infinite loop is used to navigate the tree, with break statements used to break out of the loop when required. The Add method is iterative and this will remain the case throughout what follows.

The indexer performs a binary search on the tree. This indexer determines if an object is an element of the set.

= Enumerating Sets - Parent References =

In the previous Binary Search Tree, there is no way of progressing from one key to the next. The process of moving forward in a set is referred to as iteration (or enumeration). It is wise to consider iteration before deletion.

By ensuring that each node in a set contains a reference to its parent, it becomes possible to iterate on the set. Thus, the node for a set will be upgraded to the following.


```c#

public class SetNode<T> where T : IComparable<T>
{
    public SetNode<T> Left;
    public SetNode<T> Right;
    public SetNode<T> Parent;
    public bool IsHeader;
    public T Data;

    public SetNode()
    {
        Parent = null;
        Left = this;
        Right = this;
        IsHeader = true;
    }

    public SetNode(T t, SetNode<T> p)
    {
        Data = t;
        Left = null;
        Right = null;
        Parent = p;
        IsHeader = false;
    }
}

```


Two fields have been added to the node, a parent reference and a boolean flag indicating whether the node is a header node. Header nodes will be discussed shortly. The first constructor is used to create a header node and the second constructor is the standard constructor used to create a normal node. Note that the second constructor now assumes that the parent node is passed as a parameter when creating the node.

When attempting to progress from one key to the next in a binary tree, the problem is simple if the current node has a non-null right child reference. One merely goes right, then all the way left up to when the next left node is null. This is because the first node on the right is greater, and continuing left from there will eventually lead to the smallest node greater than the current node. This is half the algorithm required for iteration, but the real problem comes if the right node is null (the other half). The solution lies in having a parent reference. When the right node is null, one progresses up the parent chain until a node greater than the existing node is found. The logic is not found in the set class, rather it is found in the separate enumerator class. The logic is shown below, and should be carefully examined.


```c#

public struct SetEntry<T> : IEnumerator<T> where T : IComparable<T>
{
    public SetEntry(SetNode<T> N) { Node = N; }

    public T Value
    {
        get { return Node.Data; }
    }

    public bool IsEnd { get { return Node.IsHeader; } }

    public bool MoveNext()
    {
        if (Node.IsHeader)
            Node = Node.Left;
        else
        {
            if (Node.Right != null)
            {
                Node = Node.Right;
                while (Node.Left != null) Node = Node.Left;
            }
            else
            {
                SetNode<T> y = Node.Parent;

                if (y.IsHeader)
                    Node = y;
                else
                {
                    while (Node == y.Right) { Node = y; y = y.Parent; }
                    Node = y;
                }
            }
        }
        return Node.IsHeader ? false : true;
    }

    public void Reset()
    {
        while (!MoveNext()) ;
    }

    object System.Collections.IEnumerator.Current
    { get { return Node.Data; } }

    T IEnumerator<T>.Current
    { get { return Node.Data; } }

    public static bool operator ==(SetEntry<T> x, SetEntry<T> y) { return x.Node == y.Node; }
    public static bool operator !=(SetEntry<T> x, SetEntry<T> y) { return x.Node != y.Node; }

    public override string ToString()
    {
        return Value.ToString();
    }

    public void Dispose() { }

    public SetNode<T> Node;
}

```


The class SetEntry is an enumerator for the set and it is a separate class. It holds only a reference to a node in the field Node. The method MoveNext implements iteration on the set. At the top of a set is the header node. The header node points left to the beginning of the set and points right to the end of the set. The header node contains no entry, it merely acts as a sentinel in the set. The parent of the header node is the root node and the parent of the root node is the header node. The root node is at the top of the set of entries that actually hold data. In the above algorithm for iteration, when the header node is reached, iteration has proceeded beyond the end of the set (hence false is returned). MoveNext() implements the previously discussed logic for iteration. It first examines child references and failing that examines parent references to obtain the next entry in the set.

Set must now be implemented such that it supports parent references and a header node. The following code does this (Project Set2).


```c#

// Set2 - Binary Search Set
//      - Implements: Parents, Iterators

using System;
using System.Collections.Generic;

public class SetNode<T> where T : IComparable<T>
{
    public SetNode<T> Left;
    public SetNode<T> Right;
    public SetNode<T> Parent;
    public bool IsHeader;
    public T Data;

    public SetNode()
    {
        Parent = null;
        Left = this;
        Right = this;
        IsHeader = true;
    }

    public SetNode(T t, SetNode<T> p)
    {
        Data = t;
        Left = null;
        Right = null;
        Parent = p;
        IsHeader = false;
    }
}

class Set<T> : IEnumerable<T> where T : IComparable<T>
{
    SetNode<T> Header;

    SetNode<T> Root
    {
        get { return Header.Parent; }
        set { Header.Parent = value; }
    }

    public Set()
    {
        Header = new SetNode<T>();
    }

    public void Add(T Value)
        {
            if (Root == null)
            {
                Root = new SetNode<T>(Value, Header);
                Header.Left = Header.Right = Root;
            }
            else
            {
                SetNode<T> Search = Root;
                for (; ; )
                {
                    int Compare = Value.CompareTo(Search.Data);

                    if (Compare == 0)
                         throw new EntryAlreadyExistsException();

                    else if (Compare < 0)
                    {
                        if (Search.Left != null)
                            Search = Search.Left;
                        else
                        {
                            Search.Left = new SetNode<T>(Value, Search);
                            if (Header.Left == Search) Header.Left = Search.Left;
                            break;
                        }
                    }

                    else
                    {
                        if (Search.Right != null)
                            Search = Search.Right;
                        else
                        {
                            Search.Right = new SetNode<T>(Value, Search);
                            if (Header.Right == Search) Header.Right = Search.Right;
                            break;
                        }
                    }
                }
            }
        }

    public bool this[T Key]
    {
        get
        {
            if (Root == null)
                throw new EntryNotFoundException();
            else
            {
                SetNode<T> Search = Root;

                do
                {
                    int Result = Key.CompareTo(Search.Data);

                    if (Result < 0) Search = Search.Left;

                    else if (Result > 0) Search = Search.Right;

                    else break;

                } while (Search != null);

                if (Search == null)
                    return false;
                else
                    return true;
            }
        }
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    { return new SetEntry<T>(Header); }

    IEnumerator<T> IEnumerable<T>.GetEnumerator()
    { return new SetEntry<T>(Header); }
}

public struct SetEntry<T> : IEnumerator<T> where T : IComparable<T>
{
    public SetEntry(SetNode<T> N) { Node = N; }

    public T Value
    {
        get { return Node.Data; }
    }

    public bool IsEnd { get { return Node.IsHeader; } }

    public bool MoveNext()
    {
        if (Node.IsHeader)
            Node = Node.Left;
        else
        {
            if (Node.Right != null)
            {
                Node = Node.Right;
                while (Node.Left != null) Node = Node.Left;
            }
            else
            {
                SetNode<T> y = Node.Parent;

                if (y.IsHeader)
                    Node = y;
                else
                {
                    while (Node == y.Right) { Node = y; y = y.Parent; }
                    Node = y;
                }
            }
        }
        return Node.IsHeader ? false : true;
    }

    public void Reset()
    {
        while (!MoveNext()) ;
    }

    object System.Collections.IEnumerator.Current
    { get { return Node.Data; } }

    T IEnumerator<T>.Current
    { get { return Node.Data; } }

    public static bool operator ==(SetEntry<T> x, SetEntry<T> y) { return x.Node == y.Node; }
    public static bool operator !=(SetEntry<T> x, SetEntry<T> y) { return x.Node != y.Node; }

    public override string ToString()
    {
        return Value.ToString();
    }

    public void Dispose() { }

    public SetNode<T> Node;
}

public class EntryNotFoundException : Exception
{
    static String message = "The requested entry could not be located in the specified collection.";

    public EntryNotFoundException() : base(message) { }
}

public class EntryAlreadyExistsException : Exception
{
    static String message = "The entry already resides in the collection.";

    public EntryAlreadyExistsException() : base(message) { }
}

class Program
{
    static void Main()
    {
        Set<int> S = new Set<int>() {1, 2, 3, 4, 5};

        foreach (int i in S)
            Console.WriteLine("{0}", i);

        for(int i=0; i<6; i++)
            Console.WriteLine("S[{0}] = {1}", i, S[i]);

    }
}

```


The method Add has been modified to support parent references. The set membership indexer is unchanged.

The IEnumerable-IEnumerator interfaces allow for enumeration of a set. The IEnumerable of T interface has only the method GetEnumerator, which returns an enumerator for the set. In this case, it is the header node of the set. The header node points right to the first element of the set, so that when MoveNext is initially called, it positions at the first element of the set. The class SetEntry<T> is an enumerator for the set because it derives from IEnumerator<T>. It is expected to implement MoveNext, so that iteration can be achieved. When a collection conforms to the IEnumerable of T interface, the compiler is able to iterate on the collection using the foreach keyword. In the mainline, foreach is used to display the set.

Note how the set has been initialised in the mainline. Collection initialization syntax (which has a remarkable resemblance to set notation) is used. When a collection is enumerable and has the method Add, the compiler knows to call Add with the initialisers to form the collection.

= Deletion In Binary Trees =

Deleting entries from a Binary Search Tree turns out to be quite tricky. When a node has only one child it is easy, just go to the parent and link the child of the node to be deleted in place of the node itself. The main problem arises when the node to be deleted has two children. For example, the following tree will be built, and node 6 will be deleted.

[[File:TreeDelete.jpg]]

Node 6 has two children. To delete it, node 6 will be swapped with node 4, then deleted. The tree that remains is shown below.

[[File:TreeDelete2.jpg]]

Node 4 was chosen for the swap because it is the immediate predecessor of node 6. Note that a valid Binary Search Tree results. In general, when a node to be deleted has two children, its inorder predecessor is located and swapped with the node to be deleted. The node at its new position has at most one child and may easily be deleted.

An algorithm for swapping nodes in a set must be developed. Note that the data could be exchanged, but that is an inferior solution because an enumerator may be referencing the other node participating in the swap (and that node would then change). So all the parent and child references in the two nodes (and certain links in their parents and children) have to be exchanged. The nodes are unplugged from the tree, then replugged at their new positions. Of course, this is only possible when the nodes have parent references, so proper deletion in a Binary Search Tree is reliant upon parent references. We shall see later that balancing is also reliant upon parent references (i.e. if it is iterative, leaf-to-root balanced AVL - root-to-leaf, recursive balancing can be done without parents).

The source code for a node swap is shown below (Project Set3). This code was ported from C++ into C#. The code was developed in C++ entirely independently. To correctly master binary trees (in particular deletion) node swapping is required.


```c#

    static void SwapNodeReference(ref SetNode<T> First,
                                  ref SetNode<T> Second)
    { SetNode<T> Temporary = First; First = Second; Second = Temporary; }

    public static void SwapNodes(SetNode<T> A, SetNode<T> B)
    {
        if (B == A.Left)
        {
            if (B.Left != null) B.Left.Parent = A;
            if (B.Right != null) B.Right.Parent = A;

            if (A.Right != null) A.Right.Parent = B;

            if (!A.Parent.IsHeader)
            {
                if (A.Parent.Left == A)
                    A.Parent.Left = B;
                else
                    A.Parent.Right = B;
            }
            else A.Parent.Parent = B;

            B.Parent = A.Parent;
            A.Parent = B;

            A.Left = B.Left;
            B.Left = A;

            SwapNodeReference(ref A.Right, ref B.Right);
        }
        else if (B == A.Right)
        {
            if (B.Right != null) B.Right.Parent = A;
            if (B.Left != null) B.Left.Parent = A;

            if (A.Left != null) A.Left.Parent = B;

            if (!A.Parent.IsHeader)
            {
                if (A.Parent.Left == A)
                    A.Parent.Left = B;
                else
                    A.Parent.Right = B;
            }
            else A.Parent.Parent = B;

            B.Parent = A.Parent;
            A.Parent = B;

            A.Right = B.Right;
            B.Right = A;

            SwapNodeReference(ref A.Left, ref B.Left);
        }
        else if (A == B.Left)
        {
            if (A.Left != null) A.Left.Parent = B;
            if (A.Right != null) A.Right.Parent = B;

            if (B.Right != null) B.Right.Parent = A;

            if (!B.Parent.IsHeader)
            {
                if (B.Parent.Left == B)
                    B.Parent.Left = A;
                else
                    B.Parent.Right = A;
            }
            else B.Parent.Parent = A;

            A.Parent = B.Parent;
            B.Parent = A;

            B.Left = A.Left;
            A.Left = B;

            SwapNodeReference(ref A.Right, ref B.Right);
        }
        else if (A == B.Right)
        {
            if (A.Right != null) A.Right.Parent = B;
            if (A.Left != null) A.Left.Parent = B;

            if (B.Left != null) B.Left.Parent = A;

            if (!B.Parent.IsHeader)
            {
                if (B.Parent.Left == B)
                    B.Parent.Left = A;
                else
                    B.Parent.Right = A;
            }
            else B.Parent.Parent = A;

            A.Parent = B.Parent;
            B.Parent = A;

            B.Right = A.Right;
            A.Right = B;

            SwapNodeReference(ref A.Left, ref B.Left);
        }
        else
        {
            if (A.Parent == B.Parent)
                SwapNodeReference(ref A.Parent.Left, ref A.Parent.Right);
            else
            {
                if (!A.Parent.IsHeader)
                {
                    if (A.Parent.Left == A)
                        A.Parent.Left = B;
                    else
                        A.Parent.Right = B;
                }
                else A.Parent.Parent = B;

                if (!B.Parent.IsHeader)
                {
                    if (B.Parent.Left == B)
                        B.Parent.Left = A;
                    else
                        B.Parent.Right = A;
                }
                else B.Parent.Parent = A;
            }

            if (B.Left != null) B.Left.Parent = A;
            if (B.Right != null) B.Right.Parent = A;

            if (A.Left != null) A.Left.Parent = B;
            if (A.Right != null) A.Right.Parent = B;

            SwapNodeReference(ref A.Left, ref B.Left);
            SwapNodeReference(ref A.Right, ref B.Right);
            SwapNodeReference(ref A.Parent, ref B.Parent);
        }
    }

```


There are five cases, depending upon parent child relationships between the nodes being swapped. If no parent-child relationship exists (ie. when the nodes are in different parts of the set), the last case is executed. This is the general case and it should be studied before other special cases.

Once swapping has been mastered, deletion then is as follows.


```c#

    public void Remove(T Key)
    {
        SetNode<T> root = Root;

        for (; ; )
        {
            if (root == null)
                throw new EntryNotFoundException();

            int Compare = Comparer.Compare(Key, root.Data);

            if (Compare < 0)
                root = root.Left;

            else if (Compare > 0)
                root = root.Right;

            else // Item is found
            {
                if (root.Left != null && root.Right != null)
                {
                    SetNode<T> replace = root.Left;
                    while (replace.Right != null) replace = replace.Right;
                    SetUtility<T>.SwapNodes(root, replace);
                }

                SetNode<T> Parent = root.Parent;

                if (LeftMost == root)
                {
                    SetEntry<T> e = new SetEntry<T>(root); e.MoveNext();

                    if (e.Node.IsHeader)
                    { LeftMost = Header; RightMost = Header; }
                    else
                        LeftMost = e.Node;
                }
                else if (RightMost == root)
                {
                    SetEntry<T> e = new SetEntry<T>(root); e.MovePrevious();

                    if (e.Node.IsHeader)
                    { LeftMost = Header; RightMost = Header; }
                    else
                        RightMost = e.Node;
                }

                if (root.Left == null)
                {
                    if (Parent == Header)
                        Header.Parent = root.Right;
                    else if (Parent.Left == root)
                        Parent.Left = root.Right;
                    else
                        Parent.Right = root.Right;

                    if (root.Right != null) root.Right.Parent = Parent;
                }
                else
                {
                    if (Parent == Header)
                        Header.Parent = root.Left;
                    else if (Parent.Left == root)
                        Parent.Left = root.Left;
                    else
                        Parent.Right = root.Left;

                    if (root.Left != null) root.Left.Parent = Parent;
                }

                Nodes--;
                break;
            }
        }
    }

```


Swapping is performed when required. Near the line containing Nodes--, balancing routines can be plugged into the set. They also need to be plugged into the add routine. This will be done in the next section where the set will be balanced. Note that only a couple of lines of the project Set3 change to achieve the balancing.

With project Set3, a different approach has been taken with comparability. No longer are generic constraints used. Instead, the following field has been included in the set class.


```c#

    IComparer<T> Comparer;

```


The interface IComparer is documented in the .Net documentation. Roughly, an IComparer looks like the following interface class.


```c#

public interface IComparer<T>
{
 int Compare(T t1, T t2);
}

```


There is another interface called IComparable, which is shown below.


```c#

public interface IComparable<T>
{
 int CompareTo(T value);
}

```


Sets work off the IComparer interface. There are two constructors for the set class, as shown below.


```c#

class Set<T> : IEnumerable<T>
{
    IComparer<T> Comparer;
    SetNode<T> Header;
    ulong Nodes;

    public Set()
    {
        Comparer = Comparer<T>.Default;
        Header = new SetNode<T>();
        Nodes = 0;
    }

    public Set(IComparer<T> c)
    {
        Comparer = c;
        Header = new SetNode<T>();
        Nodes = 0;
    }

   ...
}

```


The first constructor (the default constructor) generates the IComparer for the set. The way this is done is through another class called Comparer (roughly shown below).


```c#

[Serializable]
public abstract class Comparer<T> : IComparer<T>
{
 public static Comparer<T> Default { get; }

 public abstract int Compare(T t1,T t2);
}

```


Notice that the property Default is static. Although the class itself (i.e. Comparer) is abstract (hence cannot be instantiated), the static property Default can be used without instantiating the class. What the property Default does is detect the IComparable interface in the key class T and use it to generate an IComparer. In summary, the class T derives from IComparable<T>, and the default constructor of the set class uses the CompareTo method of IComparable to generate the Compare method of IComparer. Hence we end up with a comparer for the key class. Alternatively, the Comparer may be passed in directly (via the other constructor). This accounts for the two constructors shown above. A production standard set class will use these techniques rather than use generic constraints on the key class.

The advantage of this system is that the class T can derive from IComparable<T> and the default constructor of the set class will detect it. If that comparer is for some reason unsuitable, a comparer may be directly supplied without altering the class. The final set class will use the same methodology.

Here is the full source code to Set3.


```c#

// Set3 - Binary Search Tree
//      - Using IComparer Generic Interface
//      - With deletion
//
// Copyright Benedict McNamara 2006 - 2014, All Rights Reserved

using System;
using System.Collections.Generic;

public class SetNode<T>
{
    public T Data;
    public SetNode<T> Left;
    public SetNode<T> Right;
    public SetNode<T> Parent;
    public bool IsHeader;

    public SetNode()
    {
        Parent = null;
        Left = this;
        Right = this;
        IsHeader = true;
    }

    public SetNode(T t, SetNode<T> p)
    {
        Data = t;
        Left = null;
        Right = null;
        Parent = p;
        IsHeader = false;
    }
}

class SetUtility<T>
{
    static void SwapNodeReference(ref SetNode<T> First,
                                  ref SetNode<T> Second)
    { SetNode<T> Temporary = First; First = Second; Second = Temporary; }

    public static void SwapNodes(SetNode<T> A, SetNode<T> B)
    {
        if (B == A.Left)
        {
            if (B.Left != null) B.Left.Parent = A;
            if (B.Right != null) B.Right.Parent = A;

            if (A.Right != null) A.Right.Parent = B;

            if (!A.Parent.IsHeader)
            {
                if (A.Parent.Left == A)
                    A.Parent.Left = B;
                else
                    A.Parent.Right = B;
            }
            else A.Parent.Parent = B;

            B.Parent = A.Parent;
            A.Parent = B;

            A.Left = B.Left;
            B.Left = A;

            SwapNodeReference(ref A.Right, ref B.Right);
        }
        else if (B == A.Right)
        {
            if (B.Right != null) B.Right.Parent = A;
            if (B.Left != null) B.Left.Parent = A;

            if (A.Left != null) A.Left.Parent = B;

            if (!A.Parent.IsHeader)
            {
                if (A.Parent.Left == A)
                    A.Parent.Left = B;
                else
                    A.Parent.Right = B;
            }
            else A.Parent.Parent = B;

            B.Parent = A.Parent;
            A.Parent = B;

            A.Right = B.Right;
            B.Right = A;

            SwapNodeReference(ref A.Left, ref B.Left);
        }
        else if (A == B.Left)
        {
            if (A.Left != null) A.Left.Parent = B;
            if (A.Right != null) A.Right.Parent = B;

            if (B.Right != null) B.Right.Parent = A;

            if (!B.Parent.IsHeader)
            {
                if (B.Parent.Left == B)
                    B.Parent.Left = A;
                else
                    B.Parent.Right = A;
            }
            else B.Parent.Parent = A;

            A.Parent = B.Parent;
            B.Parent = A;

            B.Left = A.Left;
            A.Left = B;

            SwapNodeReference(ref A.Right, ref B.Right);
        }
        else if (A == B.Right)
        {
            if (A.Right != null) A.Right.Parent = B;
            if (A.Left != null) A.Left.Parent = B;

            if (B.Left != null) B.Left.Parent = A;

            if (!B.Parent.IsHeader)
            {
                if (B.Parent.Left == B)
                    B.Parent.Left = A;
                else
                    B.Parent.Right = A;
            }
            else B.Parent.Parent = A;

            A.Parent = B.Parent;
            B.Parent = A;

            B.Right = A.Right;
            A.Right = B;

            SwapNodeReference(ref A.Left, ref B.Left);
        }
        else
        {
            if (A.Parent == B.Parent)
                SwapNodeReference(ref A.Parent.Left, ref A.Parent.Right);
            else
            {
                if (!A.Parent.IsHeader)
                {
                    if (A.Parent.Left == A)
                        A.Parent.Left = B;
                    else
                        A.Parent.Right = B;
                }
                else A.Parent.Parent = B;

                if (!B.Parent.IsHeader)
                {
                    if (B.Parent.Left == B)
                        B.Parent.Left = A;
                    else
                        B.Parent.Right = A;
                }
                else B.Parent.Parent = A;
            }

            if (B.Left != null) B.Left.Parent = A;
            if (B.Right != null) B.Right.Parent = A;

            if (A.Left != null) A.Left.Parent = B;
            if (A.Right != null) A.Right.Parent = B;

            SwapNodeReference(ref A.Left, ref B.Left);
            SwapNodeReference(ref A.Right, ref B.Right);
            SwapNodeReference(ref A.Parent, ref B.Parent);
        }
    }
}

class Set<T> : IEnumerable<T>
{
    IComparer<T> Comparer;
    SetNode<T> Header;
    ulong Nodes;

    public Set()
    {
        Comparer = Comparer<T>.Default;
        Header = new SetNode<T>();
        Nodes = 0;
    }

    public Set(IComparer<T> c)
    {
        Comparer = c;
        Header = new SetNode<T>();
        Nodes = 0;
    }

    SetNode<T> Root
    {
        get { return Header.Parent; }
        set { Header.Parent = value; }
    }

    public SetNode<T> LeftMost
    {
        get { return Header.Left; }
        set { Header.Left = value; }
    }

    public SetNode<T> RightMost
    {
        get { return Header.Right; }
        set { Header.Right = value; }
    }

    public SetEntry<T> Begin
    { get { return new SetEntry<T>(Header.Left); } }

    public SetEntry<T> End
    { get { return new SetEntry<T>(Header); } }

    public ulong Length { get { return Nodes; } }

    public void Add(T Value)
        {
            if (Root == null)
            {
                Root = new SetNode<T>(Value, Header);
                LeftMost = RightMost = Root;
            }
            else
            {
                SetNode<T> Search = Root;
                for (; ; )
                {
                    int Compare = Comparer.Compare(Value, Search.Data);

                    if (Compare == 0)
                        throw new EntryAlreadyExistsException();

                    else if (Compare < 0)
                    {
                        if (Search.Left != null)
                            Search = Search.Left;
                        else
                        {
                            Search.Left = new SetNode<T>(Value, Search);
                            if (Header.Left == Search) Header.Left = Search.Left;
                            Nodes++;
                            break;
                        }
                    }

                    else
                    {
                        if (Search.Right != null)
                            Search = Search.Right;
                        else
                        {
                            Search.Right = new SetNode<T>(Value, Search);
                            if (Header.Right == Search) Header.Right = Search.Right;
                            Nodes++;
                            break;
                        }
                    }
                }
            }
        }

    public bool this[T Key]
    {
        get
        {
            if (Root == null)
                throw new EntryNotFoundException();
            else
            {
                SetNode<T> Search = Root;

                do
                {
                    int result = Comparer.Compare(Key, Search.Data);

                    if (result < 0) Search = Search.Left;

                    else if (result > 0) Search = Search.Right;

                    else break;

                } while (Search != null);

                if (Search == null)
                    return false;
                else
                    return true;
            }
        }
    }

    public void Remove(T Key)
    {
        SetNode<T> root = Root;

        for (; ; )
        {
            if (root == null)
                throw new EntryNotFoundException();

            int Compare = Comparer.Compare(Key, root.Data);

            if (Compare < 0)
                root = root.Left;

            else if (Compare > 0)
                root = root.Right;

            else // Item is found
            {
                if (root.Left != null && root.Right != null)
                {
                    SetNode<T> replace = root.Left;
                    while (replace.Right != null) replace = replace.Right;
                    SetUtility<T>.SwapNodes(root, replace);
                }

                SetNode<T> Parent = root.Parent;

                if (LeftMost == root)
                {
                    SetEntry<T> e = new SetEntry<T>(root); e.MoveNext();

                    if (e.Node.IsHeader)
                    { LeftMost = Header; RightMost = Header; }
                    else
                        LeftMost = e.Node;
                }
                else if (RightMost == root)
                {
                    SetEntry<T> e = new SetEntry<T>(root); e.MovePrevious();

                    if (e.Node.IsHeader)
                    { LeftMost = Header; RightMost = Header; }
                    else
                        RightMost = e.Node;
                }

                if (root.Left == null)
                {
                    if (Parent == Header)
                        Header.Parent = root.Right;
                    else if (Parent.Left == root)
                        Parent.Left = root.Right;
                    else
                        Parent.Right = root.Right;

                    if (root.Right != null) root.Right.Parent = Parent;
                }
                else
                {
                    if (Parent == Header)
                        Header.Parent = root.Left;
                    else if (Parent.Left == root)
                        Parent.Left = root.Left;
                    else
                        Parent.Right = root.Left;

                    if (root.Left != null) root.Left.Parent = Parent;
                }

                Nodes--;
                break;
            }
        }
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    { return new SetEntry<T>(Header); }

    IEnumerator<T> IEnumerable<T>.GetEnumerator()
    { return new SetEntry<T>(Header); }

    public override string ToString()
    {
        string string_out = "{";

        SetEntry<T> start = Begin;
        SetEntry<T> end = End;
        SetEntry<T> last = End - 1;

        while (start != end)
        {
            string new_string_out = start.Value.ToString();
            if (start != last) new_string_out = new_string_out + ",";
            string_out = string_out + new_string_out;
            ++start;
        }

        string_out = string_out + "}";
        return string_out;
    }
}

public struct SetEntry<T> : IEnumerator<T>
{
    public SetEntry(SetNode<T> n) { Node = n; }

    public T Value
    {
        get
        {
            return Node.Data;
        }
    }

    public bool IsEnd { get { return Node.IsHeader; } }

    public bool MoveNext()
    {
        if (Node.IsHeader)
            Node = Node.Left;
        else
        {
            if (Node.Right != null)
            {
                Node = Node.Right;
                while (Node.Left != null) Node = Node.Left;
            }
            else
            {
                SetNode<T> y = Node.Parent;

                if (y.IsHeader)
                    Node = y;
                else
                {
                    while (Node == y.Right) { Node = y; y = y.Parent; }
                    Node = y;
                }
            }
        }
        return Node.IsHeader ? false : true;
    }

    public bool MovePrevious()
    {
        if (Node.IsHeader)
            Node = Node.Right;
        else
        {
            if (Node.Left != null)
            {
                Node = Node.Left;
                while (Node.Right != null) Node = Node.Right;
            }
            else
            {
                SetNode<T> y = Node.Parent;

                if (y.IsHeader)
                    Node = y;
                else
                {
                    while (Node == y.Left) { Node = y; y = y.Parent; }
                    Node = y;
                }
            }
        }
        return Node.IsHeader ? false : true;
    }

    public void Reset()
    {
        while (!MoveNext()) ;
    }

    object System.Collections.IEnumerator.Current
    { get { return Node.Data; } }

    T IEnumerator<T>.Current
    { get { return Node.Data; } }

    public static bool operator ==(SetEntry<T> x, SetEntry<T> y) { return x.Node == y.Node; }
    public static bool operator !=(SetEntry<T> x, SetEntry<T> y) { return x.Node != y.Node; }

    public static SetEntry<T> operator ++(SetEntry<T> entry)
    {
        if (entry.Node.IsHeader)
            entry.Node = entry.Node.Left;
        else
        {
            if (entry.Node.Right != null)
            {
                entry.Node = entry.Node.Right;
                while (entry.Node.Left != null) entry.Node = entry.Node.Left;
            }
            else
            {
                SetNode<T> y = entry.Node.Parent;

                if (y.IsHeader)
                    entry.Node = y;
                else
                {
                    while (entry.Node == y.Right) { entry.Node = y; y = y.Parent; }
                    entry.Node = y;
                }
            }
        }

        return entry;
    }

    public static SetEntry<T> operator --(SetEntry<T> entry)
    {
        if (entry.Node.IsHeader)
            entry.Node = entry.Node.Right;
        else
        {
            if (entry.Node.Left != null)
            {
                entry.Node = entry.Node.Left;
                while (entry.Node.Right != null) entry.Node = entry.Node.Right;
            }
            else
            {
                SetNode<T> y = entry.Node.Parent;

                if (y.IsHeader)
                    entry.Node = y;
                else
                {
                    while (entry.Node == y.Left) { entry.Node = y; y = y.Parent; }
                    entry.Node = y;
                }
            }
        }

        return entry;
    }

    public static SetEntry<T> operator +(SetEntry<T> C, ulong Increment)
    {
        SetEntry<T> Result = new SetEntry<T>(C.Node);
        for (ulong i = 0; i < Increment; i++) ++Result;
        return Result;
    }

    public static SetEntry<T> operator +(ulong Increment, SetEntry<T> C)
    {
        SetEntry<T> Result = new SetEntry<T>(C.Node);
        for (ulong i = 0; i < Increment; i++) ++Result;
        return Result;
    }

    public static SetEntry<T> operator -(SetEntry<T> C, ulong Decrement)
    {
        SetEntry<T> Result = new SetEntry<T>(C.Node);
        for (ulong i = 0; i < Decrement; i++) --Result;
        return Result;
    }

    public override string ToString()
    {
        return Value.ToString();
    }

    public void Dispose() { }

    public SetNode<T> Node;
}

public class EntryNotFoundException : Exception
{
    static String message = "The requested entry could not be located in the specified collection.";

    public EntryNotFoundException() : base(message) { }
}

public class EntryAlreadyExistsException : Exception
{
    static String message = "The entry already resides in the collection.";

    public EntryAlreadyExistsException() : base(message) { }
}

class Program
{
    static void Main()
    {
        Set<int> S = new Set<int>() {1, 2, 3, 4, 5};

        S.Remove(3);

        Console.WriteLine("{0}", S);
    }
}

```


= Balancing Sets =

The Russian mathematicians G.M.Adel'son-Vel'skii and E.M.Landis were the first to notice that certain operations can be performed on binary trees without affecting their validity. For example, please consider the following diagram.

[[File:TreeRotateLeft.jpg]]

The second tree is obtained from the first by rotating the node 7 left and upwards. It is called a left rotation. The things to notice about this transformation are:

* the resultant tree (with 7 at the root) is still in key order and
* the transformation has altered the heights of the left and right subtrees.

There is also a right rotation. Once left and right rotations have been discovered, it is possible to use them to maintain balancing when adding and removing entries from a tree. The type of tree to be considered is constrained in that left and right subtrees of any node are restricted to being at most 1 different in height (both of the trees above satisfy this condition). This type of tree is almost balanced, and is referred to as an AVL tree in honour of the Russian mathematicians mentioned above. It is possible (using rotations) to always satisfy the AVL condition when adding and removing entries from the tree.

==AVL Sets==

An AVL set is a binary tree in which the heights of the left and right subtrees of the root differ by at most 1 and in which the left and right subtrees are again AVL sets.

With each node of an AVL set is associated a balance factor that is either left high, equal or right high according to whether the left subtree has height greater than, equal to or less than the height of the right subtree (respectively).

==Set Nodes==

Another shift from the first set program presented will now be introduced. Instead of the node class being generic, a non-generic base class will be used. This is so that non-generic balancing and iteration routines can be developed. This precludes code bloat.

The above definition leads to the following base class for node and class for a set node.


```c#

public enum Direction { FromLeft, FromRight };

public enum State { Header, LeftHigh, Balanced, RightHigh };

public class Node
{
    public Node Left;
    public Node Right;
    public Node Parent;
    public State Balance;

    public Node()
    {
        Left = this;
        Right = this;
        Parent = null;
        Balance = State.Header;
    }

    public Node(Node p)
    {
        Left = null;
        Right = null;
        Parent = p;
        Balance = State.Balanced;
    }

    public bool IsHeader
    { get { return Balance == State.Header; } }
}

public class SetNode<T> : Node
{
    public T Data;

    public SetNode() { }

    public SetNode(T dataType, Node Parent) : base(Parent)
    {
        Data = dataType;
    }
}

```


There is a non-generic base class Node. This facilitates non-generic balancing. The header node is an instance of the class Node, whereas all other nodes are instances of the class SetNode<T>. Apart from the references to the left and right subtrees and parent, a node contains a balance factor. The balance factor is one of the values from the above enumeration State. With just the addition of the member (Balance), sets can be balanced (which is quite efficient in terms of node storage). The generic class SetNode<T> derives from Node.

The routine that balances a set after an insertion is shown below.


```c#

    public static void BalanceSet(Node Root, Direction From)
    {
        bool Taller = true;

        while (Taller)
        {
            Node Parent = Root.Parent;
            Direction NextFrom = (Parent.Left == Root) ? Direction.FromLeft : Direction.FromRight;

            if (From == Direction.FromLeft)
            {
                switch (Root.Balance)
                {
                    case State.LeftHigh:
                        if (Parent.IsHeader)
                            BalanceLeft(ref Parent.Parent);
                        else if (Parent.Left == Root)
                            BalanceLeft(ref Parent.Left);
                        else
                            BalanceLeft(ref Parent.Right);
                        Taller = false;
                        break;

                    case State.Balanced:
                        Root.Balance = State.LeftHigh;
                        Taller = true;
                        break;

                    case State.RightHigh:
                        Root.Balance = State.Balanced;
                        Taller = false;
                        break;
                }
            }
            else
            {
                switch (Root.Balance)
                {
                    case State.LeftHigh:
                        Root.Balance = State.Balanced;
                        Taller = false;
                        break;

                    case State.Balanced:
                        Root.Balance = State.RightHigh;
                        Taller = true;
                        break;

                    case State.RightHigh:
                        if (Parent.IsHeader)
                            BalanceRight(ref Parent.Parent);
                        else if (Parent.Left == Root)
                            BalanceRight(ref Parent.Left);
                        else
                            BalanceRight(ref Parent.Right);
                        Taller = false;
                        break;
                }
            }

            if (Taller) // skip up a level
            {
                if (Parent.IsHeader)
                    Taller = false;
                else
                {
                    Root = Parent;
                    From = NextFrom;
                }
            }
        }
    }

```


If the direction is from the left, left set balancing is instigated. If the direction is from the right, right set balancing is performed. This algorithm progresses up the parent chain of the set until the set is balanced. Certain cases can be taken care of immediately. For example, when inserting into the right subtree (FromRight), if the current node is left high, the balance factor is immediately set to State.Balanced and the job is done. Yet other cases require the set to be restructured. Ancilliary procedures BalanceLeft and BalanceRight have been created for this job. They restructure the set as required. The procedures BalanceLeft and BalanceRight are shown below.


```c#

    static void BalanceLeft(ref Node Root)
    {
        Node Left = Root.Left;

        switch (Left.Balance)
        {
            case State.LeftHigh:
                Root.Balance = State.Balanced;
                Left.Balance = State.Balanced;
                RotateRight(ref Root);
                break;

            case State.RightHigh:
                {
                    Node subRight = Left.Right;
                    switch (subRight.Balance)
                    {
                        case State.Balanced:
                            Root.Balance = State.Balanced;
                            Left.Balance = State.Balanced;
                            break;

                        case State.RightHigh:
                            Root.Balance = State.Balanced;
                            Left.Balance = State.LeftHigh;
                            break;

                        case State.LeftHigh:
                            Root.Balance = State.RightHigh;
                            Left.Balance = State.Balanced;
                            break;
                    }
                    subRight.Balance = State.Balanced;
                    RotateLeft(ref Left);
                    Root.Left = Left;
                    RotateRight(ref Root);
                }
                break;

            case State.Balanced:
                Root.Balance = State.LeftHigh;
                Left.Balance = State.RightHigh;
                RotateRight(ref Root);
                break;
        }
    }

    static void BalanceRight(ref Node Root)
    {
        Node Right = Root.Right;

        switch (Right.Balance)
        {
            case State.RightHigh:
                Root.Balance = State.Balanced;
                Right.Balance = State.Balanced;
                RotateLeft(ref Root);
                break;

            case State.LeftHigh:
                {
                    Node subLeft = Right.Left; // Left Subtree of Right
                    switch (subLeft.Balance)
                    {
                        case State.Balanced:
                            Root.Balance = State.Balanced;
                            Right.Balance = State.Balanced;
                            break;

                        case State.LeftHigh:
                            Root.Balance = State.Balanced;
                            Right.Balance = State.RightHigh;
                            break;

                        case State.RightHigh:
                            Root.Balance = State.LeftHigh;
                            Right.Balance = State.Balanced;
                            break;
                    }
                    subLeft.Balance = State.Balanced;
                    RotateRight(ref Right);
                    Root.Right = Right;
                    RotateLeft(ref Root);
                }
                break;

            case State.Balanced:
                Root.Balance = State.RightHigh;
                Right.Balance = State.LeftHigh;
                RotateLeft(ref Root);
                break;
        }
    }

```


Reference rotations are performed based upon the cases of the insertion. The procedures BalanceLeft and BalanceRight are symmetric under left/right reflection.

In the procedure BalanceSet, the case (with FromRight) where a new node has been inserted into the taller subtree of the root and its height has increased will now be considered. Under such conditions, one subtree has a height of 2 more than the other so that the set no longer satisfies the AVL condition. Part of the set must be rebuilt to restore its balance. To be specific, assume the node was inserted into the right subtree, its height has increased and originally it was right high. That is, the case where BalanceRight is called from BalanceSet will be covered. Let the root of the set be r and let x be the root of the right subtree. There are three cases to be considered depending upon the balance factor of x.

== Case1: Right High ==

The first case (in BalanceRight) is illustrated in the diagram below.

[[File:TreeRotate1.jpg]]

The required action is a left rotation. In the diagram, the node x is rotated upward and r is made the left subtree of x. The left subtree T2 of x becomes the right subtree of r. A left rotation is described in the appropriate C# function (see RotateLeft). Note that when done in the appropriate order, the steps constitute a rotation of three reference values. After the rotation, the height of the rotated set has decreased by 1 whereas it had previously increased because of the insertion, thus the height finishes where it began.

== Case2: Left High ==

The second case is when the balance factor of x is left high. This case is more complicated. It is required to move two levels to the node w that is the root of the left subtree of x to find the new root. This process is called a double rotation because the transformation can be obtained in two steps (see diagram below). First, x is rotated to the right (so that w becomes its root) and then the set with root r is rotated to the left (moving w up to become the new root).

[[File:TreeRotate2.jpg]]

The code for BalanceRight (and BalanceLeft) should now be carefully considered with the above diagrams in mind.

== Case3: Equal Height ==

It would appear that a third case must be considered. The case where the two subtrees of x have equal heights, but this case can never happen. To see why, recall that a new node has just been inserted into the subtree rooted at x and this subtree has height 2 more than the left subtree of the root. The new node was placed into either the left or right subtree of x. Its placement only increased the height of one subtree of x. If those subtrees had equal heights after the insertion then the height of the full subtree rooted at x was not changed by the insertion - contrary to the hypothesis. While the case of equal height does not occur for insertions, it does occur for deletions, hence it has been coded in the algorithms.

== The Final AVL Tree Code ==


```c#

// Set4 -  Finite Ordered Sets - 4State - Balanced

using System;
using System.Collections.Generic;

public enum Direction { FromLeft, FromRight };

public enum State { Header, LeftHigh, Balanced, RightHigh };

public enum SetOperation
{
    Union,
    Intersection,
    SymmetricDifference,
    Difference,
    Equality,
    Inequality,
    Subset,
    Superset
}

public class Node
{
    public Node Left;
    public Node Right;
    public Node Parent;
    public State Balance;

    public Node()
    {
        Left = this;
        Right = this;
        Parent = null;
        Balance = State.Header;
    }

    public Node(Node p)
    {
        Left = null;
        Right = null;
        Parent = p;
        Balance = State.Balanced;
    }

    public bool IsHeader
    { get { return Balance == State.Header; } }
}

public class SetNode<T> : Node
{
    public T Data;

    public SetNode() { }

    public SetNode(T dataType, Node Parent) : base(Parent)
    {
        Data = dataType;
    }

    public override int GetHashCode()
    {
        return Data.GetHashCode();
    }
}

class Utility // Nongeneric Tree Balancing
{
    static void RotateLeft(ref Node Root)
    {
        Node Parent = Root.Parent;
        Node x = Root.Right;

        Root.Parent = x;
        x.Parent = Parent;
        if (x.Left != null) x.Left.Parent = Root;

        Root.Right = x.Left;
        x.Left = Root;
        Root = x;
    }

    static void RotateRight(ref Node Root)
    {
        Node Parent = Root.Parent;
        Node x = Root.Left;

        Root.Parent = x;
        x.Parent = Parent;
        if (x.Right != null) x.Right.Parent = Root;

        Root.Left = x.Right;
        x.Right = Root;
        Root = x;
    }

    static void BalanceLeft(ref Node Root)
    {
        Node Left = Root.Left;

        switch (Left.Balance)
        {
            case State.LeftHigh:
                Root.Balance = State.Balanced;
                Left.Balance = State.Balanced;
                RotateRight(ref Root);
                break;

            case State.RightHigh:
                {
                    Node subRight = Left.Right;
                    switch (subRight.Balance)
                    {
                        case State.Balanced:
                            Root.Balance = State.Balanced;
                            Left.Balance = State.Balanced;
                            break;

                        case State.RightHigh:
                            Root.Balance = State.Balanced;
                            Left.Balance = State.LeftHigh;
                            break;

                        case State.LeftHigh:
                            Root.Balance = State.RightHigh;
                            Left.Balance = State.Balanced;
                            break;
                    }
                    subRight.Balance = State.Balanced;
                    RotateLeft(ref Left);
                    Root.Left = Left;
                    RotateRight(ref Root);
                }
                break;

            case State.Balanced:
                Root.Balance = State.LeftHigh;
                Left.Balance = State.RightHigh;
                RotateRight(ref Root);
                break;
        }
    }

    static void BalanceRight(ref Node Root)
    {
        Node Right = Root.Right;

        switch (Right.Balance)
        {
            case State.RightHigh:
                Root.Balance = State.Balanced;
                Right.Balance = State.Balanced;
                RotateLeft(ref Root);
                break;

            case State.LeftHigh:
                {
                    Node subLeft = Right.Left; // Left Subtree of Right
                    switch (subLeft.Balance)
                    {
                        case State.Balanced:
                            Root.Balance = State.Balanced;
                            Right.Balance = State.Balanced;
                            break;

                        case State.LeftHigh:
                            Root.Balance = State.Balanced;
                            Right.Balance = State.RightHigh;
                            break;

                        case State.RightHigh:
                            Root.Balance = State.LeftHigh;
                            Right.Balance = State.Balanced;
                            break;
                    }
                    subLeft.Balance = State.Balanced;
                    RotateRight(ref Right);
                    Root.Right = Right;
                    RotateLeft(ref Root);
                }
                break;

            case State.Balanced:
                Root.Balance = State.RightHigh;
                Right.Balance = State.LeftHigh;
                RotateLeft(ref Root);
                break;
        }
    }

    public static void BalanceSet(Node Root, Direction From)
    {
        bool Taller = true;

        while (Taller)
        {
            Node Parent = Root.Parent;
            Direction NextFrom = (Parent.Left == Root) ? Direction.FromLeft : Direction.FromRight;

            if (From == Direction.FromLeft)
            {
                switch (Root.Balance)
                {
                    case State.LeftHigh:
                        if (Parent.IsHeader)
                            BalanceLeft(ref Parent.Parent);
                        else if (Parent.Left == Root)
                            BalanceLeft(ref Parent.Left);
                        else
                            BalanceLeft(ref Parent.Right);
                        Taller = false;
                        break;

                    case State.Balanced:
                        Root.Balance = State.LeftHigh;
                        Taller = true;
                        break;

                    case State.RightHigh:
                        Root.Balance = State.Balanced;
                        Taller = false;
                        break;
                }
            }
            else
            {
                switch (Root.Balance)
                {
                    case State.LeftHigh:
                        Root.Balance = State.Balanced;
                        Taller = false;
                        break;

                    case State.Balanced:
                        Root.Balance = State.RightHigh;
                        Taller = true;
                        break;

                    case State.RightHigh:
                        if (Parent.IsHeader)
                            BalanceRight(ref Parent.Parent);
                        else if (Parent.Left == Root)
                            BalanceRight(ref Parent.Left);
                        else
                            BalanceRight(ref Parent.Right);
                        Taller = false;
                        break;
                }
            }

            if (Taller) // skip up a level
            {
                if (Parent.IsHeader)
                    Taller = false;
                else
                {
                    Root = Parent;
                    From = NextFrom;
                }
            }
        }
    }

    public static void BalanceSetRemove(Node Root, Direction From)
    {
        if (Root.IsHeader) return;

        bool Shorter = true;

        while (Shorter)
        {
            Node Parent = Root.Parent;
            Direction NextFrom = (Parent.Left == Root) ? Direction.FromLeft : Direction.FromRight;

            if (From == Direction.FromLeft)
            {
                switch (Root.Balance)
                {
                    case State.LeftHigh:
                        Root.Balance = State.Balanced;
                        Shorter = true;
                        break;

                    case State.Balanced:
                        Root.Balance = State.RightHigh;
                        Shorter = false;
                        break;

                    case State.RightHigh:
                        if (Root.Right.Balance == State.Balanced)
                            Shorter = false;
                        else
                            Shorter = true;
                        if (Parent.IsHeader)
                            BalanceRight(ref Parent.Parent);
                        else if (Parent.Left == Root)
                            BalanceRight(ref Parent.Left);
                        else
                            BalanceRight(ref Parent.Right);
                        break;
                }
            }
            else
            {
                switch (Root.Balance)
                {
                    case State.RightHigh:
                        Root.Balance = State.Balanced;
                        Shorter = true;
                        break;

                    case State.Balanced:
                        Root.Balance = State.LeftHigh;
                        Shorter = false;
                        break;

                    case State.LeftHigh:
                        if (Root.Left.Balance == State.Balanced)
                            Shorter = false;
                        else
                            Shorter = true;
                        if (Parent.IsHeader)
                            BalanceLeft(ref Parent.Parent);
                        else if (Parent.Left == Root)
                            BalanceLeft(ref Parent.Left);
                        else
                            BalanceLeft(ref Parent.Right);
                        break;
                }
            }

            if (Shorter)
            {
                if (Parent.IsHeader)
                    Shorter = false;
                else
                {
                    From = NextFrom;
                    Root = Parent;
                }
            }
        }
    }

    public static Node PreviousItem(Node Node)
    {
        if (Node.IsHeader) { return Node.Right; }

        if (Node.Left != null)
        {
            Node = Node.Left;
            while (Node.Right != null) Node = Node.Right;
        }
        else
        {
            Node y = Node.Parent;
            if (y.IsHeader) return y;
            while (Node == y.Left) { Node = y; y = y.Parent; }
            Node = y;
        }
        return Node;
    }

    public static Node NextItem(Node Node)
    {
        if (Node.IsHeader) return Node.Left;

        if (Node.Right != null)
        {
            Node = Node.Right;
            while (Node.Left != null) Node = Node.Left;
        }
        else
        {
            Node y = Node.Parent;
            if (y.IsHeader) return y;
            while (Node == y.Right) { Node = y; y = y.Parent; }
            Node = y;
        }
        return Node;
    }

    public static ulong Depth(Node Root)
    {
        if (Root != null)
        {
            ulong Left = Root.Left != null ? Depth(Root.Left) : 0;
            ulong Right = Root.Right != null ? Depth(Root.Right) : 0;
            return Left < Right ? Right + 1 : Left + 1;
        }
        else
            return 0;
    }

    static void SwapNodeReference(ref Node First,
                                  ref Node Second)
    { Node Temporary = First; First = Second; Second = Temporary; }

    public static void SwapNodes(Node A, Node B)
    {
        if (B == A.Left)
        {
            if (B.Left != null) B.Left.Parent = A;
            if (B.Right != null) B.Right.Parent = A;

            if (A.Right != null) A.Right.Parent = B;

            if (!A.Parent.IsHeader)
            {
                if (A.Parent.Left == A)
                    A.Parent.Left = B;
                else
                    A.Parent.Right = B;
            }
            else A.Parent.Parent = B;

            B.Parent = A.Parent;
            A.Parent = B;

            A.Left = B.Left;
            B.Left = A;

            SwapNodeReference(ref A.Right, ref B.Right);
        }
        else if (B == A.Right)
        {
            if (B.Right != null) B.Right.Parent = A;
            if (B.Left != null) B.Left.Parent = A;

            if (A.Left != null) A.Left.Parent = B;

            if (!A.Parent.IsHeader)
            {
                if (A.Parent.Left == A)
                    A.Parent.Left = B;
                else
                    A.Parent.Right = B;
            }
            else A.Parent.Parent = B;

            B.Parent = A.Parent;
            A.Parent = B;

            A.Right = B.Right;
            B.Right = A;

            SwapNodeReference(ref A.Left, ref B.Left);
        }
        else if (A == B.Left)
        {
            if (A.Left != null) A.Left.Parent = B;
            if (A.Right != null) A.Right.Parent = B;

            if (B.Right != null) B.Right.Parent = A;

            if (!B.Parent.IsHeader)
            {
                if (B.Parent.Left == B)
                    B.Parent.Left = A;
                else
                    B.Parent.Right = A;
            }
            else B.Parent.Parent = A;

            A.Parent = B.Parent;
            B.Parent = A;

            B.Left = A.Left;
            A.Left = B;

            SwapNodeReference(ref A.Right, ref B.Right);
        }
        else if (A == B.Right)
        {
            if (A.Right != null) A.Right.Parent = B;
            if (A.Left != null) A.Left.Parent = B;

            if (B.Left != null) B.Left.Parent = A;

            if (!B.Parent.IsHeader)
            {
                if (B.Parent.Left == B)
                    B.Parent.Left = A;
                else
                    B.Parent.Right = A;
            }
            else B.Parent.Parent = A;

            A.Parent = B.Parent;
            B.Parent = A;

            B.Right = A.Right;
            A.Right = B;

            SwapNodeReference(ref A.Left, ref B.Left);
        }
        else
        {
            if (A.Parent == B.Parent)
                SwapNodeReference(ref A.Parent.Left, ref A.Parent.Right);
            else
            {
                if (!A.Parent.IsHeader)
                {
                    if (A.Parent.Left == A)
                        A.Parent.Left = B;
                    else
                        A.Parent.Right = B;
                }
                else A.Parent.Parent = B;

                if (!B.Parent.IsHeader)
                {
                    if (B.Parent.Left == B)
                        B.Parent.Left = A;
                    else
                        B.Parent.Right = A;
                }
                else B.Parent.Parent = A;
            }

            if (B.Left != null) B.Left.Parent = A;
            if (B.Right != null) B.Right.Parent = A;

            if (A.Left != null) A.Left.Parent = B;
            if (A.Right != null) A.Right.Parent = B;

            SwapNodeReference(ref A.Left, ref B.Left);
            SwapNodeReference(ref A.Right, ref B.Right);
            SwapNodeReference(ref A.Parent, ref B.Parent);
        }

        State Balance = A.Balance;
        A.Balance = B.Balance;
        B.Balance = Balance;
    }
}

public struct SetEntry<T> : IEnumerator<T>
{
    public SetEntry(Node N) { _Node = N; }

    public T Value
    {
        get
        {
            return ((SetNode<T>)_Node).Data;
        }
    }

    public bool IsEnd { get { return _Node.IsHeader; } }

    public bool MoveNext()
    {
        _Node = Utility.NextItem(_Node);
        return _Node.IsHeader ? false : true;
    }

    public bool MovePrevious()
    {
        _Node = Utility.PreviousItem(_Node);
        return _Node.IsHeader ? false : true;
    }

    public static SetEntry<T> operator ++(SetEntry<T> entry)
    {
        entry._Node = Utility.NextItem(entry._Node);
        return entry;
    }

    public static SetEntry<T> operator --(SetEntry<T> entry)
    {
        entry._Node = Utility.PreviousItem(entry._Node);
        return entry;
    }

    public void Reset()
    {
        while (!MoveNext()) ;
    }

    object System.Collections.IEnumerator.Current
    { get { return ((SetNode<T>)_Node).Data; } }

    T IEnumerator<T>.Current
    { get { return ((SetNode<T>)_Node).Data; } }

    public static bool operator ==(SetEntry<T> x, SetEntry<T> y) { return x._Node == y._Node; }
    public static bool operator !=(SetEntry<T> x, SetEntry<T> y) { return x._Node != y._Node; }

    public override bool Equals(object o) { return _Node == ((SetEntry<T>)o)._Node; }

    public override int GetHashCode() { return _Node.GetHashCode(); }

    public static SetEntry<T> operator +(SetEntry<T> C, ulong Increment)
    {
        SetEntry<T> Result = new SetEntry<T>(C._Node);
        for (ulong i = 0; i < Increment; i++) ++Result;
        return Result;
    }

    public static SetEntry<T> operator +(ulong Increment, SetEntry<T> C)
    {
        SetEntry<T> Result = new SetEntry<T>(C._Node);
        for (ulong i = 0; i < Increment; i++) ++Result;
        return Result;
    }

    public static SetEntry<T> operator -(SetEntry<T> C, ulong Decrement)
    {
        SetEntry<T> Result = new SetEntry<T>(C._Node);
        for (ulong i = 0; i < Decrement; i++) --Result;
        return Result;
    }

    public override string ToString()
    {
        return Value.ToString();
    }

    public void Dispose() { }

    public Node _Node;
}

class Set<T> : IEnumerable<T>
{
    IComparer<T> Comparer;
    Node Header;
    ulong Nodes;

    //*** Constructors ***

    public Set()
    {
        Comparer = Comparer<T>.Default;
        Header = new Node();
        Nodes = 0;
    }

    public Set(IComparer<T> c)
    {
        Comparer = c;
        Header = new Node();
        Nodes = 0;
    }

    //*** Properties ***

    SetNode<T> Root
    {
        get { return (SetNode<T>)Header.Parent; }
        set { Header.Parent = value; }
    }

    Node LeftMost
    {
        get { return Header.Left; }
        set { Header.Left = value; }
    }

    Node RightMost
    {
        get { return Header.Right; }
        set { Header.Right = value; }
    }

    public SetEntry<T> Begin
    { get { return new SetEntry<T>(Header.Left); } }

    public SetEntry<T> End
    { get { return new SetEntry<T>(Header); } }

    public ulong Length { get { return Nodes; } }

    public ulong Depth { get { return Utility.Depth(Root); } }

    //*** Operators ***

    public bool this[T key] { get { return Search(key); } }

    public static Set<T> operator +(Set<T> set, T t)
    {
        set.Add(t); return set;
    }

    public static Set<T> operator -(Set<T> set, T t)
    {
        set.Remove(t); return set;
    }

    public static Set<T> operator |(Set<T> A, Set<T> B)
    {
        Set<T> U = new Set<T>(A.Comparer);
        CombineSets(A, B, U, SetOperation.Union);
        return U;
    }

    public static Set<T> operator &(Set<T> A, Set<T> B)
    {
        Set<T> I = new Set<T>(A.Comparer);
        CombineSets(A, B, I, SetOperation.Intersection);
        return I;
    }

    public static Set<T> operator ^(Set<T> A, Set<T> B)
    {
        Set<T> S = new Set<T>(A.Comparer);
        CombineSets(A, B, S, SetOperation.SymmetricDifference);
        return S;
    }

    public static Set<T> operator -(Set<T> A, Set<T> B)
    {
        Set<T> S = new Set<T>(A.Comparer);
        CombineSets(A, B, S, SetOperation.Difference);
        return S;
    }

    public static bool operator ==(Set<T> A, Set<T> B)
    {
        return CheckSets(A, B, SetOperation.Equality);
    }

    public static bool operator !=(Set<T> A, Set<T> B)
    {
        return CheckSets(A, B, SetOperation.Inequality);
    }

    public override bool Equals(object o)
    {
        return CheckSets(this, (Set<T>)o, SetOperation.Equality);
    }

    //*** Methods ***

    public void Add(T key)
    {
        if (Root == null)
        {
            Root = new SetNode<T>(key, Header);
            LeftMost = RightMost = Root;
        }
        else
        {
            SetNode<T> Search = Root;
            for (; ; )
            {
                int Compare = Comparer.Compare(key, Search.Data);

                if (Compare == 0) // Item Exists
                    throw new EntryAlreadyExistsException();

                else if (Compare < 0)
                {
                    if (Search.Left != null)
                        Search = (SetNode<T>)Search.Left;
                    else
                    {
                        Search.Left = new SetNode<T>(key, Search);
                        if (LeftMost == Search) LeftMost = (SetNode<T>)Search.Left;
                        Utility.BalanceSet(Search, Direction.FromLeft);
                        Nodes++;
                    }
                }

                else
                {
                    if (Search.Right != null)
                        Search = (SetNode<T>)Search.Right;
                    else
                    {
                        Search.Right = new SetNode<T>(key, Search);
                        if (RightMost == Search) RightMost = (SetNode<T>)Search.Right;
                        Utility.BalanceSet(Search, Direction.FromRight);
                        Nodes++;
                        break;
                    }
                }
            }
        }
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    { return new SetEntry<T>(Header); }

    IEnumerator<T> IEnumerable<T>.GetEnumerator()
    { return new SetEntry<T>(Header); }

    public override int GetHashCode()
    {
        return GetHashCode((SetNode<T>)Header.Parent);
    }

    int GetHashCode(SetNode<T> Root)
    {
        if (Root != null)
        {
            int HashCode = Root.GetHashCode();

            if (Root.Left != null)
                HashCode += GetHashCode((SetNode<T>)Root.Left);

            if (Root.Right != null)
                HashCode += GetHashCode((SetNode<T>)Root.Right);

            return HashCode;
        }

        return 0;
    }

    public void Remove(T key)
    {
        SetNode<T> root = Root;

        for (; ; )
        {
            if (root == null)
                throw new EntryNotFoundException();

            int Compare = Comparer.Compare(key, root.Data);

            if (Compare < 0)
                root = (SetNode<T>)root.Left;

            else if (Compare > 0)
                root = (SetNode<T>)root.Right;

            else // Item is found
            {
                if (root.Left != null && root.Right != null)
                {
                    SetNode<T> replace = (SetNode<T>)root.Left;
                    while (replace.Right != null) replace = (SetNode<T>)replace.Right;
                    Utility.SwapNodes(root, replace);
                }

                SetNode<T> Parent = (SetNode<T>)root.Parent;

                Direction From = (Parent.Left == root) ? Direction.FromLeft : Direction.FromRight;

                if (LeftMost == root)
                {
                    SetEntry<T> e = new SetEntry<T>(root); e.MoveNext();

                    if (e._Node.IsHeader)
                    { LeftMost = Header; RightMost = Header; }
                    else
                        LeftMost = e._Node;
                }
                else if (RightMost == root)
                {
                    SetEntry<T> e = new SetEntry<T>(root); e.MovePrevious();

                    if (e._Node.IsHeader)
                    { LeftMost = Header; RightMost = Header; }
                    else
                        RightMost = e._Node;
                }

                if (root.Left == null)
                {
                    if (Parent == Header)
                        Header.Parent = root.Right;
                    else if (Parent.Left == root)
                        Parent.Left = root.Right;
                    else
                        Parent.Right = root.Right;

                    if (root.Right != null) root.Right.Parent = Parent;
                }
                else
                {
                    if (Parent == Header)
                        Header.Parent = root.Left;
                    else if (Parent.Left == root)
                        Parent.Left = root.Left;
                    else
                        Parent.Right = root.Left;

                    if (root.Left != null) root.Left.Parent = Parent;
                }

                Utility.BalanceSetRemove(Parent, From);
                Nodes--;
                break;
            }
        }
    }

    public bool Search(T key)
    {
        if (Root == null)
            return false;
        else
        {
            SetNode<T> Search = Root;

            do
            {
                int Result = Comparer.Compare(key, Search.Data);

                if (Result < 0) Search = (SetNode<T>)Search.Left;

                else if (Result > 0) Search = (SetNode<T>)Search.Right;

                else break;

            } while (Search != null);

            if (Search == null)
                return false;
            else
                return true;
        }
    }

    public override string ToString()
    {
        string StringOut = "{";

        SetEntry<T> start = Begin;
        SetEntry<T> end = End;
        SetEntry<T> last = End - 1;

        while (start != end)
        {
            string new_StringOut = start.Value.ToString();
            if (start != last) new_StringOut = new_StringOut + ",";
            StringOut = StringOut + new_StringOut;
            ++start;
        }

        StringOut = StringOut + "}";
        return StringOut;
    }

    public void Validate()
    {
        if (Nodes == 0 || Root == null)
        {
            if (Nodes != 0) { throw new InvalidEmptyTreeException(); }
            if (Root != null) { throw new InvalidEmptyTreeException(); }
            if (LeftMost != Header) { throw new InvalidEndItemException(); }
            if (RightMost != Header) { throw new InvalidEndItemException(); }
        }

        Validate(Root);

        if (Root != null)
        {
            SetNode<T> x = Root;
            while (x.Left != null) x = (SetNode<T>)x.Left;

            if (LeftMost != x) throw new InvalidEndItemException();

            SetNode<T> y = Root;
            while (y.Right != null) y = (SetNode<T>)y.Right;

            if (RightMost != y) throw new InvalidEndItemException();
        }
    }

    void Validate(SetNode<T> root)
    {
        if (root == null) return;

        if (root.Left != null)
        {
            SetNode<T> Left = (SetNode<T>)root.Left;

            if (Comparer.Compare(Left.Data, root.Data) >= 0)
                throw new OutOfKeyOrderException();

            if (Left.Parent != root)
                throw new TreeInvalidParentException();

            Validate((SetNode<T>)root.Left);
        }

        if (root.Right != null)
        {
            SetNode<T> Right = (SetNode<T>)root.Right;

            if (Comparer.Compare(Right.Data, root.Data) <= 0)
                throw new OutOfKeyOrderException();

            if (Right.Parent != root)
                throw new TreeInvalidParentException();

            Validate((SetNode<T>)root.Right);
        }

        ulong depth_Left = root.Left != null ? Utility.Depth(root.Left) : 0;
        ulong depth_Right = root.Right != null ? Utility.Depth(root.Right) : 0;

        if (depth_Left > depth_Right && depth_Left - depth_Right > 2)
            throw new TreeOutOfBalanceException();

        if (depth_Left < depth_Right && depth_Right - depth_Left > 2)
            throw new TreeOutOfBalanceException();
    }

    public static void CombineSets(Set<T> A,
                                   Set<T> B,
                                   Set<T> R,
                                   SetOperation operation)
    {
        IComparer<T> TComparer = R.Comparer;
        SetEntry<T> First1 = A.Begin;
        SetEntry<T> Last1 = A.End;
        SetEntry<T> First2 = B.Begin;
        SetEntry<T> Last2 = B.End;

        switch (operation)
        {
            case SetOperation.Union:
                while (First1 != Last1 && First2 != Last2)
                {
                    int Order = TComparer.Compare(First1.Value, First2.Value);

                    if (Order < 0)
                    {
                        R.Add(First1.Value);
                        First1.MoveNext();
                    }

                    else if (Order > 0)
                    {
                        R.Add(First2.Value);
                        First2.MoveNext();
                    }

                    else
                    {
                        R.Add(First1.Value);
                        First1.MoveNext();
                        First2.MoveNext();
                    }
                }
                while (First1 != Last1)
                {
                    R.Add(First1.Value);
                    First1.MoveNext();
                }
                while (First2 != Last2)
                {
                    R.Add(First2.Value);
                    First2.MoveNext();
                }
                return;

            case SetOperation.Intersection:
                while (First1 != Last1 && First2 != Last2)
                {
                    int Order = TComparer.Compare(First1.Value, First2.Value);

                    if (Order < 0)
                        First1.MoveNext();

                    else if (Order > 0)
                        First2.MoveNext();

                    else
                    {
                        R.Add(First1.Value);
                        First1.MoveNext();
                        First2.MoveNext();
                    }
                }
                return;

            case SetOperation.SymmetricDifference:
                while (First1 != Last1 && First2 != Last2)
                {
                    int Order = TComparer.Compare(First1.Value, First2.Value);

                    if (Order < 0)
                    {
                        R.Add(First1.Value);
                        First1.MoveNext();
                    }

                    else if (Order > 0)
                    {
                        R.Add(First2.Value);
                        First2.MoveNext();
                    }

                    else
                    { First1.MoveNext(); First2.MoveNext(); }
                }

                while (First1 != Last1)
                {
                    R.Add(First1.Value);
                    First1.MoveNext();
                }

                while (First2 != Last2)
                {
                    R.Add(First2.Value);
                    First2.MoveNext();
                }
                return;

            case SetOperation.Difference:
                while (First1 != Last1 && First2 != Last2)
                {
                    int Order = TComparer.Compare(First1.Value, First2.Value);

                    if (Order < 0)
                    {
                        R.Add(First1.Value);
                        First1.MoveNext();
                    }

                    else if (Order > 0)
                    {
                        R.Add(First1.Value);
                        First1.MoveNext();
                        First2.MoveNext();
                    }

                    else
                    { First1.MoveNext(); First2.MoveNext(); }
                }

                while (First1 != Last1)
                {
                    R.Add(First1.Value);
                    First1.MoveNext();
                }
                return;
        }

        throw new InvalidSetOperationException();
    }

    public static bool CheckSets(Set<T> A,
                                 Set<T> B,
                                 SetOperation operation)
    {
        IComparer<T> TComparer = A.Comparer;
        SetEntry<T> First1 = A.Begin;
        SetEntry<T> Last1 = A.End;
        SetEntry<T> First2 = B.Begin;
        SetEntry<T> Last2 = B.End;

        switch (operation)
        {
            case SetOperation.Equality:
            case SetOperation.Inequality:
                {
                    bool Equals = true;

                    while (First1 != Last1 && First2 != Last2)
                    {
                        if (TComparer.Compare(First1.Value, First2.Value) == 0)
                        { First1.MoveNext(); First2.MoveNext(); }
                        else
                        { Equals = false; break; }
                    }

                    if (Equals)
                    {
                        if (First1 != Last1) Equals = false;
                        if (First2 != Last2) Equals = false;
                    }

                    if (operation == SetOperation.Equality)
                        return Equals;
                    else
                        return !Equals;
                }

            case SetOperation.Subset:
            case SetOperation.Superset:
                {
                    bool Subset = true;

                    while (First1 != Last1 && First2 != Last2)
                    {
                        int Order = TComparer.Compare(First1.Value, First2.Value);

                        if (Order < 0)
                        { Subset = false; break; }

                        else if (Order > 0)
                            First2.MoveNext();

                        else
                        { First1.MoveNext(); First2.MoveNext(); }
                    }

                    if (Subset)
                        if (First1 != Last1) Subset = false;

                    if (operation == SetOperation.Subset)
                        return Subset;
                    else
                        return !Subset;
                }
        }

        throw new InvalidSetOperationException();
    }
}

public class EntryNotFoundException : Exception
{
    static String message = "The requested entry could not be located in the specified collection.";

    public EntryNotFoundException() : base(message) { }
}

public class EntryAlreadyExistsException : Exception
{
    static String message = "The requested entry already resides in the collection.";

    public EntryAlreadyExistsException() : base(message) { }
}

public class InvalidEndItemException : Exception
{
    static String message = "The validation routines detected that the end item of a tree is invalid.";

    public InvalidEndItemException() : base(message) { }
}

public class InvalidEmptyTreeException : Exception
{
    static String message = "The validation routines detected that an empty tree is invalid.";

    public InvalidEmptyTreeException() : base(message) { }
}

public class OutOfKeyOrderException : Exception
{
    static String message = "A trees was found to be out of key order.";

    public OutOfKeyOrderException() : base(message) { }
}

public class TreeInvalidParentException : Exception
{
    static String message = "The validation routines detected that the Parent structure of a tree is invalid.";

    public TreeInvalidParentException() : base(message) { }
}

public class TreeOutOfBalanceException : Exception
{
    static String message = "The validation routines detected that the tree is out of State.";

    public TreeOutOfBalanceException() : base(message) { }
}

public class InvalidSetOperationException : Exception
{
    static String message = "An invalid set operation was requested.";

    public InvalidSetOperationException() : base(message) { }
}

class Program
{
    static void Main()
    {
        Set<string> s = new Set<string>() {"S0","S1","S2","S3","S4",
                                           "S5","S6","S7","S8","S9"};

        Console.WriteLine("Depth = {0}", s.Depth);

        s.Validate();

        for (int i = 0; i < 10; i += 2)
            s.Remove("S" + i.ToString());

        Console.WriteLine("Depth = {0}", s.Depth);

        s.Validate();

        Console.WriteLine("{0}", s);

        Set<int> A = new Set<int>() { 1, 3, 5, 7 };
        Set<int> B = new Set<int>() { 2, 4, 6, 8 };

        Set<int> U = A | B;

        Console.WriteLine("{0} | {1} == {2}", A, B, U);
    }
}

```

