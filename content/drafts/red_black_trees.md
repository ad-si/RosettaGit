+++
title = "Red black trees"
description = ""
date = 2016-07-11T11:32:51Z
aliases = []
[extra]
id = 21001
[taxonomies]
categories = []
tags = []
+++

Red/Black Trees in C#

== Code ==


```c#

// Set6 - Red/Black (3State) Sets

using System;
using System.Collections.Generic;

public enum Direction { FromLeft, FromRight };

public enum TriState
{
    Header,
    Red,
    Black
}

public class Node
{
    public Node Left;
    public Node Right;
    public Node Parent;
    public TriState Color;

    public bool IsHeader
    { get { return Color == TriState.Header; } }
}


public class SetNode<T> : Node
{
    public T Data;

    public SetNode()
    {
        Left = this;
        Right = this;
        Parent = null;
        Color = TriState.Header;
    }

    public SetNode(T t)
    {
        Left = null;
        Right = null;
        Color = TriState.Black;
        Data = t;
    }
}

class Utility
{
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

    public static ulong Paths(Node Root, ulong weight)
    {
        if (Root != null)
        {
            ulong Left = Root.Left != null ? Paths(Root.Left, weight + 1) : 0;
            ulong Right = Root.Right != null ? Paths(Root.Right, weight + 1) : 0;
            return Left + Right + weight;
        }
        else
            return 0;
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

    static Node Minimum(Node x)
    {
        while (x.Left != null) x = x.Left;
        return x;
    }

    static Node Maximum(Node x)
    {
        while (x.Right != null) x = x.Right;
        return x;
    }

    static void RotateLeft(Node x,
                           ref Node Root)
    {
        Node y = x.Right;

        x.Right = y.Left;
        if (y.Left != null)
            y.Left.Parent = x;
        y.Parent = x.Parent;

        if (x == Root)
            Root = y;
        else if (x == x.Parent.Left)
            x.Parent.Left = y;
        else
            x.Parent.Right = y;
        y.Left = x;
        x.Parent = y;
    }

    static void RotateRight(Node x,
                            ref Node Root)
    {
        Node y = x.Left;

        x.Left = y.Right;
        if (y.Right != null)
            y.Right.Parent = x;
        y.Parent = x.Parent;

        if (x == Root)
            Root = y;
        else if (x == x.Parent.Right)
            x.Parent.Right = y;
        else
            x.Parent.Left = y;
        y.Right = x;
        x.Parent = y;
    }

    public static void Rebalance(Node x,
                                 ref Node Root)
    {
        x.Color = TriState.Red;
        while (x != Root && x.Parent.Color == TriState.Red)
        {
            if (x.Parent == x.Parent.Parent.Left)
            {
                Node y = x.Parent.Parent.Right;
                if (y != null && y.Color == TriState.Red)
                {
                    x.Parent.Color = TriState.Black;
                    y.Color = TriState.Black;
                    x.Parent.Parent.Color = TriState.Red;
                    x = x.Parent.Parent;
                }
                else
                {
                    if (x == x.Parent.Right)
                    {
                        x = x.Parent;
                        RotateLeft(x, ref Root);
                    }
                    x.Parent.Color = TriState.Black;
                    x.Parent.Parent.Color = TriState.Red;
                    RotateRight(x.Parent.Parent, ref Root);
                }
            }
            else
            {
                Node y = x.Parent.Parent.Left;
                if (y != null && y.Color == TriState.Red)
                {
                    x.Parent.Color = TriState.Black;
                    y.Color = TriState.Black;
                    x.Parent.Parent.Color = TriState.Red;
                    x = x.Parent.Parent;
                }
                else
                {
                    if (x == x.Parent.Left)
                    {
                        x = x.Parent;
                        RotateRight(x, ref Root);
                    }
                    x.Parent.Color = TriState.Black;
                    x.Parent.Parent.Color = TriState.Red;
                    RotateLeft(x.Parent.Parent, ref Root);
                }
            }
        }
        Root.Color = TriState.Black;
    }

    static void TSwap<X>(ref X u, ref X v) { X t = u; u = v; v = t; }

    public static Node RebalanceForRemove(Node z,
                                          ref Node Root,
                                          ref Node Leftmost,
                                          ref Node Rightmost)
    {
        Node y = z;
        Node x = null;
        Node x_Parent = null;

        if (y.Left == null)
            x = y.Right;
        else
            if (y.Right == null)
                x = y.Left;
            else
            {
                y = y.Right;
                while (y.Left != null) y = y.Left;
                x = y.Right;
            }

        if (y != z)
        {
            z.Left.Parent = y;
            y.Left = z.Left;
            if (y != z.Right)
            {
                x_Parent = y.Parent;
                if (x != null) x.Parent = y.Parent;
                y.Parent.Left = x;
                y.Right = z.Right;
                z.Right.Parent = y;
            }
            else
                x_Parent = y;

            if (Root == z)
                Root = y;
            else if (z.Parent.Left == z)
                z.Parent.Left = y;
            else
                z.Parent.Right = y;
            y.Parent = z.Parent;
            TSwap(ref y.Color, ref z.Color);
            y = z;
        }
        else  // y == z
        {
            x_Parent = y.Parent;
            if (x != null) x.Parent = y.Parent;
            if (Root == z)
                Root = x;
            else
                if (z.Parent.Left == z)
                    z.Parent.Left = x;
                else
                    z.Parent.Right = x;
            if (Leftmost == z)
                if (z.Right == null)
                    Leftmost = z.Parent;
                else
                    Leftmost = Minimum(x);
            if (Rightmost == z)
                if (z.Left == null)
                    Rightmost = z.Parent;
                else
                    Rightmost = Maximum(x);
        }
        if (y.Color != TriState.Red)
        {
            while (x != Root && (x == null || x.Color == TriState.Black))
                if (x == x_Parent.Left)
                {
                    Node w = x_Parent.Right;
                    if (w.Color == TriState.Red)
                    {
                        w.Color = TriState.Black;
                        x_Parent.Color = TriState.Red;
                        RotateLeft(x_Parent, ref Root);
                        w = x_Parent.Right;
                    }
                    if ((w.Left == null || w.Left.Color == TriState.Black) &&
                        (w.Right == null || w.Right.Color == TriState.Black))
                    {
                        w.Color = TriState.Red;
                        x = x_Parent;
                        x_Parent = x_Parent.Parent;
                    }
                    else
                    {
                        if (w.Right == null || w.Right.Color == TriState.Black)
                        {
                            if (w.Left != null) w.Left.Color = TriState.Black;
                            w.Color = TriState.Red;
                            RotateRight(w, ref Root);
                            w = x_Parent.Right;
                        }
                        w.Color = x_Parent.Color;
                        x_Parent.Color = TriState.Black;
                        if (w.Right != null) w.Right.Color = TriState.Black;
                        RotateLeft(x_Parent, ref Root);
                        break;
                    }
                }
                else
                {
                    Node w = x_Parent.Left;
                    if (w.Color == TriState.Red)
                    {
                        w.Color = TriState.Black;
                        x_Parent.Color = TriState.Red;
                        RotateRight(x_Parent, ref Root);
                        w = x_Parent.Left;
                    }
                    if ((w.Right == null || w.Right.Color == TriState.Black) &&
                        (w.Left == null || w.Left.Color == TriState.Black))
                    {
                        w.Color = TriState.Red;
                        x = x_Parent;
                        x_Parent = x_Parent.Parent;
                    }
                    else
                    {
                        if (w.Left == null || w.Left.Color == TriState.Black)
                        {
                            if (w.Right != null) w.Right.Color = TriState.Black;
                            w.Color = TriState.Red;
                            RotateLeft(w, ref Root);
                            w = x_Parent.Left;
                        }
                        w.Color = x_Parent.Color;
                        x_Parent.Color = TriState.Black;
                        if (w.Left != null) w.Left.Color = TriState.Black;
                        RotateRight(x_Parent, ref Root);
                        break;
                    }
                }
            if (x != null) x.Color = TriState.Black;
        }
        return y;
    }

    public static int BlackCount(Node Node, Node Root)
    {
        if (Node == null)
            return 0;
        else
        {
            int count = Node.Color == TriState.Black ? 1 : 0;

            if (Node == Root)
                return count;
            else
                return count + BlackCount(Node.Parent, Root);
        }
    }
}

public struct SetEntry<T> : IEnumerator<T>
{
    public SetEntry(SetNode<T> n) { Node = n; }

    public T Value { get { return Node.Data; } }

    public bool IsEnd { get { return Node.IsHeader; } }

    public bool MoveNext()
    {
        Node = (SetNode<T>)Utility.NextItem(Node);
        return Node.IsHeader ? false : true;
    }

    public bool MovePrevious()
    {
        Node = (SetNode<T>)Utility.PreviousItem(Node);
        return Node.IsHeader ? false : true;
    }

    public void Reset()
    {
        while (!MoveNext()) ;
    }

    object System.Collections.IEnumerator.Current { get { return Node.Data; } }

    T IEnumerator<T>.Current { get { return Node.Data; } }

    public static bool operator ==(SetEntry<T> x, SetEntry<T> y) { return x.Node == y.Node; }
    public static bool operator !=(SetEntry<T> x, SetEntry<T> y) { return x.Node != y.Node; }


    public override string ToString() { return Value.ToString(); }

    public void Dispose() { }

    public SetNode<T> Node;
}


class Set<T> : IEnumerable<T>
{
    IComparer<T> Comparer;
    SetNode<T> Header;
    ulong Nodes;

    //*** Constructors/Destructor ***

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

    //*** Properties ***

    SetNode<T> Root
    {
        get { return (SetNode<T>)Header.Parent; }
        set { Header.Parent = value; }
    }

    SetNode<T> LeftMost
    {
        get { return (SetNode<T>)Header.Left; }
        set { Header.Left = value; }
    }

    SetNode<T> RightMost
    {
        get { return (SetNode<T>)Header.Right; }
        set { Header.Right = value; }
    }

    public SetEntry<T> Begin
    { get { return new SetEntry<T>((SetNode<T>)Header.Left); } }

    public SetEntry<T> End
    { get { return new SetEntry<T>(Header); } }

    public ulong Length { get { return Nodes; } }

    public ulong Depth { get { return Utility.Depth(Root); } }

    //*** Indexer ***

    public bool this[T Key]
    {
        get
        {
            SetNode<T> Node = Search(Key);
            if (Node == null) return false; else return true;
        }
    }

    //*** Methods ***

    SetNode<T> Add(T Key,
                   SetNode<T> y,
                   Direction From)
    {
        SetNode<T> z = new SetNode<T>(Key);
        Nodes++;

        if (y == Header)
        {
            Root = z;
            RightMost = z;
            LeftMost = z;
        }
        else if (From == Direction.FromLeft)
        {
            y.Left = z;
            if (y == LeftMost) LeftMost = z;
        }
        else
        {
            y.Right = z;
            if (y == RightMost) RightMost = z;
        }

        z.Parent = y;
        Utility.Rebalance(z, ref Header.Parent);
        return z;
    }

    public SetNode<T> Add(T Key)
    {
        SetNode<T> y = Header;
        SetNode<T> x = Root;

        int c = -1;
        while (x != null)
        {
            y = x;
            c = Comparer.Compare(Key, x.Data);
            if (c < 0)
                x = (SetNode<T>)x.Left;
            else if (c > 0)
                x = (SetNode<T>)x.Right;
            else
                throw new EntryAlreadyExistsException();
         }

        Direction From = c < 0 ? Direction.FromLeft : Direction.FromRight;
        return Add(Key, y, From);
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    { return new SetEntry<T>(Header); }

    IEnumerator<T> IEnumerable<T>.GetEnumerator()
    { return new SetEntry<T>(Header); }

    public void Remove(T Key)
    {
        SetNode<T> root = Root;

        for (; ; )
        {
            if (root == null)
                throw new EntryNotFoundException();

            int Compare = Comparer.Compare(Key, root.Data);

            if (Compare < 0)
                root = (SetNode<T>)root.Left;

            else if (Compare > 0)
                root = (SetNode<T>)root.Right;

            else // Item is found
            {
                Utility.RebalanceForRemove(root, ref Header.Parent, ref Header.Left, ref Header.Right);
                Nodes--;
                break;
            }
        }
    }

    public SetNode<T> Search(T Key)
    {
        if (Root == null)
            return null;
        else
        {
            SetNode<T> search = Root;

            do
            {
                long result = Comparer.Compare(Key, search.Data);

                if (result < 0) search = (SetNode<T>)search.Left;

                else if (result > 0) search = (SetNode<T>)search.Right;

                else break;

            } while (search != null);

            return search;
        }
    }

    public override string ToString()
    {
        string StringOut = "{";

        SetEntry<T> start = Begin;
        SetEntry<T> end = End;
        SetEntry<T> last = End; last.MovePrevious();

        while (start != end)
        {
            string NewStringOut = start.Value.ToString();
            if (start != last) NewStringOut = NewStringOut + ",";
            StringOut = StringOut + NewStringOut;
            start.MoveNext();
        }

        StringOut = StringOut + "}";
        return StringOut;
    }

    public void Validate()
    {
        if (Nodes == 0 || Root == null)
        {
            if (Nodes != 0) { throw new InvalidEmptySetException(); }
            if (Root != null) { throw new InvalidEmptySetException(); }
            if (Header.Left != Header) { throw new InvalidEndItemException(); }
            if (Header.Right != Header) { throw new InvalidEndItemException(); }
        }

        int Length = Utility.BlackCount(LeftMost, Root);

        for (SetEntry<T> Iterator = Begin; Iterator != End; Iterator.MoveNext())
        {
            SetNode<T> x = Iterator.Node;
            SetNode<T> L = (SetNode<T>)x.Left;
            SetNode<T> R = (SetNode<T>)x.Right;

            if (x.Color == TriState.Red)
                if ((L != null && L.Color == TriState.Red) ||
                    (R != null && R.Color == TriState.Red))
                    throw new InvalidNodeColorException();

            if (L != null && Comparer.Compare(x.Data, L.Data) <= 0)
                throw new OutOfKeyOrderException();

            if (R != null && Comparer.Compare(R.Data, x.Data) <= 0)
                throw new OutOfKeyOrderException();

            if (L == null && R == null &&
                Utility.BlackCount(x, Root) != Length)
                throw new InvalidBlackCountException();
        }

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
}

class Program
{
    static void Main()
    {
        Set<string> S = new Set<string>();

        for (int i = 0; i < 10; i++)
            S.Add("S" + i.ToString());

        Console.WriteLine("Depth = {0}", S.Depth);

        S.Validate();

        for (int i = 0; i < 10; i += 2)
            S.Remove("S" + i.ToString());

        Console.WriteLine("Depth = {0}", S.Depth);

        S.Validate();

        foreach (string Str in S)
            Console.WriteLine("{0}", Str);

        if (S["S" + 3.ToString()])
            Console.WriteLine("{0} is in {1}", "S" + 3.ToString(), S);
        else
            Console.WriteLine("{0} is not in {1}", "S" + 3.ToString(), S);
    }
}


public class EntryNotFoundException : Exception
{
    static String message = "The requested entry could not be located in the specified collection.";

    public EntryNotFoundException() : base(message) { }
}

public class InvalidEndItemException : Exception
{
    static String message = "The validation routines detected that the end item of a tree is invalid.";

    public InvalidEndItemException() : base(message) { }
}

public class InvalidEmptySetException : Exception
{
    static String message = "The validation routines detected that an empty tree is invalid.";

    public InvalidEmptySetException() : base(message) { }
}

public class OutOfKeyOrderException : Exception
{
    static String message = "A trees was found to be out of Key order.";

    public OutOfKeyOrderException() : base(message) { }
}

public class SetInvalidParentException : Exception
{
    static String message = "The validation routines detected that the Parent structure of a tree is invalid.";

    public SetInvalidParentException() : base(message) { }
}

public class InvalidBlackCountException : Exception
{
    static String message = "An invalid black node count was encountered.";

    public InvalidBlackCountException() : base(message) { }
}

public class InvalidNodeColorException : Exception
{
    static String message = "The color of a node is invalid.";

    public InvalidNodeColorException() : base(message) { }
}

public class EntryAlreadyExistsException : Exception
{
    static String message = "The set entry already exists.";

    public EntryAlreadyExistsException() : base(message) { }
}

```

