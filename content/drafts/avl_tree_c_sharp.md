+++
title = "AVL tree/C sharp"
description = ""
date = 2019-03-21T23:13:41Z
aliases = []
[extra]
id = 20997
[taxonomies]
categories = []
tags = []
+++

== Code ==


```csharp

// Finite Ordered Sets - 4State - Balanced

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
                        break;
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

