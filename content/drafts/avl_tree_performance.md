+++
title = "AVL Tree/Performance"
description = ""
date = 2016-07-16T12:19:36Z
aliases = []
[extra]
id = 21008
[taxonomies]
categories = []
tags = []
+++


###  Red/Black Performance versus AVL Performance


The following C# program was used to test the relative performance of Red/Black Trees versus AVL Trees.


```c#

// SetPerform - Tests Red/Black (3State) Sets against AVL Sets (4State).

using System;
using System.Collections.Generic;

//******************************************************************************
//******************************** Red/Black Sets ******************************
//******************************************************************************

public enum Direction { FromLeft, FromRight };

public enum TriState
{
    Header,
    Red,
    Black
}

public class RedBlackNode
{
    public RedBlackNode Left;
    public RedBlackNode Right;
    public RedBlackNode Parent;
    public TriState Color;

    public bool IsHeader
    { get { return Color == TriState.Header; } }
}


public class RedBlackSetNode<T> : RedBlackNode
{
    public T Data;

    public RedBlackSetNode()
    {
        Left = this;
        Right = this;
        Parent = null;
        Color = TriState.Header;
    }

    public RedBlackSetNode(T t)
    {
        Left = null;
        Right = null;
        Color = TriState.Black;
        Data = t;
    }
}

class RedBlackUtility
{
    public static ulong Depth(RedBlackNode Root)
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

    public static ulong Paths(RedBlackNode Root, ulong weight)
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

    public static RedBlackNode PreviousItem(RedBlackNode RedBlackNode)
    {
        if (RedBlackNode.IsHeader) { return RedBlackNode.Right; }

        if (RedBlackNode.Left != null)
        {
            RedBlackNode = RedBlackNode.Left;
            while (RedBlackNode.Right != null) RedBlackNode = RedBlackNode.Right;
        }
        else
        {
            RedBlackNode y = RedBlackNode.Parent;
            if (y.IsHeader) return y;
            while (RedBlackNode == y.Left) { RedBlackNode = y; y = y.Parent; }
            RedBlackNode = y;
        }
        return RedBlackNode;
    }

    public static RedBlackNode NextItem(RedBlackNode RedBlackNode)
    {
        if (RedBlackNode.IsHeader) return RedBlackNode.Left;

        if (RedBlackNode.Right != null)
        {
            RedBlackNode = RedBlackNode.Right;
            while (RedBlackNode.Left != null) RedBlackNode = RedBlackNode.Left;
        }
        else
        {
            RedBlackNode y = RedBlackNode.Parent;
            if (y.IsHeader) return y;
            while (RedBlackNode == y.Right) { RedBlackNode = y; y = y.Parent; }
            RedBlackNode = y;
        }
        return RedBlackNode;
    }

    static RedBlackNode Minimum(RedBlackNode x)
    {
        while (x.Left != null) x = x.Left;
        return x;
    }

    static RedBlackNode Maximum(RedBlackNode x)
    {
        while (x.Right != null) x = x.Right;
        return x;
    }

    static void RotateLeft(RedBlackNode x,
                           ref RedBlackNode Root)
    {
        RedBlackNode y = x.Right;

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

    static void RotateRight(RedBlackNode x,
                            ref RedBlackNode Root)
    {
        RedBlackNode y = x.Left;

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

    public static void Rebalance(RedBlackNode x,
                                 ref RedBlackNode Root)
    {
        x.Color = TriState.Red;
        while (x != Root && x.Parent.Color == TriState.Red)
        {
            if (x.Parent == x.Parent.Parent.Left)
            {
                RedBlackNode y = x.Parent.Parent.Right;
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
                RedBlackNode y = x.Parent.Parent.Left;
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

    public static RedBlackNode RebalanceForRemove(RedBlackNode z,
                                                  ref RedBlackNode Root,
                                                  ref RedBlackNode Leftmost,
                                                  ref RedBlackNode Rightmost)
    {
        RedBlackNode y = z;
        RedBlackNode x = null;
        RedBlackNode x_Parent = null;

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
                    RedBlackNode w = x_Parent.Right;
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
                    RedBlackNode w = x_Parent.Left;
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

    public static int BlackCount(RedBlackNode RedBlackNode, RedBlackNode Root)
    {
        if (RedBlackNode == null)
            return 0;
        else
        {
            int count = RedBlackNode.Color == TriState.Black ? 1 : 0;

            if (RedBlackNode == Root)
                return count;
            else
                return count + BlackCount(RedBlackNode.Parent, Root);
        }
    }
}

public struct RedBlackSetEntry<T> : IEnumerator<T>
{
    public RedBlackSetEntry(RedBlackSetNode<T> n) { RedBlackNode = n; }

    public T Value { get { return RedBlackNode.Data; } }

    public bool IsEnd { get { return RedBlackNode.IsHeader; } }

    public bool MoveNext()
    {
        RedBlackNode = (RedBlackSetNode<T>)RedBlackUtility.NextItem(RedBlackNode);
        return RedBlackNode.IsHeader ? false : true;
    }

    public bool MovePrevious()
    {
        RedBlackNode = (RedBlackSetNode<T>)RedBlackUtility.PreviousItem(RedBlackNode);
        return RedBlackNode.IsHeader ? false : true;
    }

    public void Reset()
    {
        while (!MoveNext()) ;
    }

    object System.Collections.IEnumerator.Current { get { return RedBlackNode.Data; } }

    T IEnumerator<T>.Current { get { return RedBlackNode.Data; } }

    public static bool operator ==(RedBlackSetEntry<T> x, RedBlackSetEntry<T> y) { return x.RedBlackNode == y.RedBlackNode; }
    public static bool operator !=(RedBlackSetEntry<T> x, RedBlackSetEntry<T> y) { return x.RedBlackNode != y.RedBlackNode; }


    public override string ToString() { return Value.ToString(); }

    public void Dispose() { }

    public RedBlackSetNode<T> RedBlackNode;
}


class RedBlackSet<T> : IEnumerable<T>
{
    IComparer<T> Comparer;
    RedBlackSetNode<T> Header;
    ulong RedBlackNodes;

    //*** Constructors/Destructor ***

    public RedBlackSet()
    {
        Comparer = Comparer<T>.Default;
        Header = new RedBlackSetNode<T>();
        RedBlackNodes = 0;
    }

    public RedBlackSet(IComparer<T> c)
    {
        Comparer = c;
        Header = new RedBlackSetNode<T>();
        RedBlackNodes = 0;
    }

    //*** Properties ***

    RedBlackSetNode<T> Root
    {
        get { return (RedBlackSetNode<T>)Header.Parent; }
        set { Header.Parent = value; }
    }

    RedBlackSetNode<T> LeftMost
    {
        get { return (RedBlackSetNode<T>)Header.Left; }
        set { Header.Left = value; }
    }

    RedBlackSetNode<T> RightMost
    {
        get { return (RedBlackSetNode<T>)Header.Right; }
        set { Header.Right = value; }
    }

    public RedBlackSetEntry<T> Begin
    { get { return new RedBlackSetEntry<T>((RedBlackSetNode<T>)Header.Left); } }

    public RedBlackSetEntry<T> End
    { get { return new RedBlackSetEntry<T>(Header); } }

    public ulong Length { get { return RedBlackNodes; } }

    public ulong Depth { get { return RedBlackUtility.Depth(Root); } }

    //*** Indexer ***

    public bool this[T Key]
    {
        get
        {
            RedBlackSetNode<T> RedBlackNode = Search(Key);
            if (RedBlackNode == null) return false; else return true;
        }
    }

    //*** Methods ***

    RedBlackSetNode<T> Add(T Key,
                           RedBlackSetNode<T> y,
                           Direction From)
    {
        RedBlackSetNode<T> z = new RedBlackSetNode<T>(Key);
        RedBlackNodes++;

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
        RedBlackUtility.Rebalance(z, ref Header.Parent);
        return z;
    }

    public RedBlackSetNode<T> Add(T Key)
    {
        RedBlackSetNode<T> y = Header;
        RedBlackSetNode<T> x = Root;

        int c = -1;
        while (x != null)
        {
            y = x;
            c = Comparer.Compare(Key, x.Data);
            if (c < 0)
                x = (RedBlackSetNode<T>)x.Left;
            else if (c > 0)
                x = (RedBlackSetNode<T>)x.Right;
            else
                throw new EntryAlreadyExistsException();
        }

        Direction From = c < 0 ? Direction.FromLeft : Direction.FromRight;
        return Add(Key, y, From);
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    { return new RedBlackSetEntry<T>(Header); }

    IEnumerator<T> IEnumerable<T>.GetEnumerator()
    { return new RedBlackSetEntry<T>(Header); }

    public void Remove(T Key)
    {
        RedBlackSetNode<T> root = Root;

        for (; ; )
        {
            if (root == null)
                throw new EntryNotFoundException();

            int Compare = Comparer.Compare(Key, root.Data);

            if (Compare < 0)
                root = (RedBlackSetNode<T>)root.Left;

            else if (Compare > 0)
                root = (RedBlackSetNode<T>)root.Right;

            else // Item is found
            {
                RedBlackUtility.RebalanceForRemove(root, ref Header.Parent, ref Header.Left, ref Header.Right);
                RedBlackNodes--;
                break;
            }
        }
    }

    public RedBlackSetNode<T> Search(T Key)
    {
        if (Root == null)
            return null;
        else
        {
            RedBlackSetNode<T> search = Root;

            do
            {
                long result = Comparer.Compare(Key, search.Data);

                if (result < 0) search = (RedBlackSetNode<T>)search.Left;

                else if (result > 0) search = (RedBlackSetNode<T>)search.Right;

                else break;

            } while (search != null);

            return search;
        }
    }

    public override string ToString()
    {
        string StringOut = "{";

        RedBlackSetEntry<T> start = Begin;
        RedBlackSetEntry<T> end = End;
        RedBlackSetEntry<T> last = End; last.MovePrevious();

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
        if (RedBlackNodes == 0 || Root == null)
        {
            if (RedBlackNodes != 0) { throw new InvalidEmptySetException(); }
            if (Root != null) { throw new InvalidEmptySetException(); }
            if (Header.Left != Header) { throw new InvalidEndItemException(); }
            if (Header.Right != Header) { throw new InvalidEndItemException(); }
        }

        int Length = RedBlackUtility.BlackCount(LeftMost, Root);

        for (RedBlackSetEntry<T> Iterator = Begin; Iterator != End; Iterator.MoveNext())
        {
            RedBlackSetNode<T> x = Iterator.RedBlackNode;
            RedBlackSetNode<T> L = (RedBlackSetNode<T>)x.Left;
            RedBlackSetNode<T> R = (RedBlackSetNode<T>)x.Right;

            if (x.Color == TriState.Red)
                if ((L != null && L.Color == TriState.Red) ||
                    (R != null && R.Color == TriState.Red))
                    throw new InvalidRedBlackNodeColorException();

            if (L != null && Comparer.Compare(x.Data, L.Data) <= 0)
                throw new OutOfKeyOrderException();

            if (R != null && Comparer.Compare(R.Data, x.Data) <= 0)
                throw new OutOfKeyOrderException();

            if (L == null && R == null &&
                RedBlackUtility.BlackCount(x, Root) != Length)
                throw new InvalidBlackCountException();
        }

        if (Root != null)
        {
            RedBlackSetNode<T> x = Root;
            while (x.Left != null) x = (RedBlackSetNode<T>)x.Left;

            if (LeftMost != x) throw new InvalidEndItemException();

            RedBlackSetNode<T> y = Root;
            while (y.Right != null) y = (RedBlackSetNode<T>)y.Right;

            if (RightMost != y) throw new InvalidEndItemException();
        }
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

public class InvalidRedBlackNodeColorException : Exception
{
    static String message = "The color of a node is invalid.";

    public InvalidRedBlackNodeColorException() : base(message) { }
}

public class EntryAlreadyExistsException : Exception
{
    static String message = "The set entry already exists.";

    public EntryAlreadyExistsException() : base(message) { }
}

//************************************************************************
//****************************** AVL Sets ********************************
//************************************************************************

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

    public SetNode(T dataType, Node Parent)
        : base(Parent)
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



public class InvalidEmptyTreeException : Exception
{
    static String message = "The validation routines detected that an empty tree is invalid.";

    public InvalidEmptyTreeException() : base(message) { }
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

enum Limits { Maximum = 100000 };

class Program
{
    static void Main()
    {

        //*** First Red/Black Timings ***

        RedBlackSet<int> R = new RedBlackSet<int>();

        DateTime BuildRedBlackStart = DateTime.Now;

        for (int i = 0; i < (int)Limits.Maximum; i++)
            R.Add(i);

        DateTime BuildRedBlackEnd = DateTime.Now;

        Console.WriteLine("Red/Black Build Time = {0}", BuildRedBlackEnd - BuildRedBlackStart);

        DateTime SearchRedBlackStart = DateTime.Now;

        for (int i = 0; i < (int)Limits.Maximum; i++)
        {
            bool inRedBlackSet = R[i];
        }

        DateTime SearchRedBlackEnd = DateTime.Now;

        Console.WriteLine("Red/Black Search Time = {0}", SearchRedBlackEnd - SearchRedBlackStart);

        //*** Now AVL Timings ***

        Set<int> A = new Set<int>();

        DateTime BuildAVLStart = DateTime.Now;

        for (int i = 0; i < (int)Limits.Maximum; i++)
            A.Add(i);

        DateTime BuildAVLEnd = DateTime.Now;

        Console.WriteLine("AVL Build Time = {0}", BuildAVLEnd - BuildAVLStart);

        DateTime SearchAVLStart = DateTime.Now;

        for (int i = 0; i < (int)Limits.Maximum; i++)
        {
            bool inAVLSet = A[i];
        }

        DateTime SearchAVLEnd = DateTime.Now;

        Console.WriteLine("AVL Search Time = {0}", SearchAVLEnd - SearchAVLStart);
    }
}

```


The results were as follows.


```txt

Red/Black Build Time = 00:00:00.0469217
Red/Black Search Time = 00:00:00.0156213
AVL Build Time = 00:00:00.0312380
AVL Search Time = 00:00:00.0156456

```


The times for build are a suprise - AVL is faster to build than red/black. This is contrary to popular belief. The search times being roughly equal is expected.
