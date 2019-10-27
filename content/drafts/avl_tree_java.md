+++
title = "AVL tree/Java"
description = ""
date = 2018-05-19T11:09:02Z
aliases = []
[extra]
id = 20995
[taxonomies]
categories = []
tags = []
+++

== Project ==

[http://NNcNannara.net/Html/English/Java/index.html Calculus] has many more classes included.

== Code == 


```java
import java.util.Iterator;
import java.util.Comparator;
import java.io.*;

@SuppressWarnings("unchecked")

     public enum State
     {
         Header,
         LeftHigh,
         Balanced,
         RightHigh
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
        
        public Boolean isHeader ()
        { return Balance == State.Header;  }
    }

public class Utility
{
        static int depth(Node root)
        {
            if (root != null)
            {
                int Left = root.Left != null ? depth(root.Left) : 0;
                int Right = root.Right != null ? depth(root.Right) : 0;
                return Left < Right ? Right + 1 : Left + 1;
            }
            else
                return 0;
        }

        static void swapNodes(Node A, Node B)
        {
            if (B == A.Left)
            {
                if (B.Left != null) B.Left.Parent = A;
                if (B.Right != null) B.Right.Parent = A;

                if (A.Right != null) A.Right.Parent = B;

                if (!A.Parent.isHeader())
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

                Node Temp = A.Right;
                A.Right = B.Right;
                B.Right = Temp;
            }
            else if (B == A.Right)
            {
                if (B.Right != null) B.Right.Parent = A;
                if (B.Left != null) B.Left.Parent = A;

                if (A.Left != null) A.Left.Parent = B;

                if (!A.Parent.isHeader())
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

                Node Temp=A.Left;
                A.Left = B.Left;
                B.Left = Temp;
            }
            else if (A == B.Left)
            {
                if (A.Left != null) A.Left.Parent = B;
                if (A.Right != null) A.Right.Parent = B;

                if (B.Right != null) B.Right.Parent = A;

                if (!B.Parent.isHeader())
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

                Node Temp = A.Right;
                A.Right = B.Right;
                B.Right = Temp;
            }
            else if (A == B.Right)
            {
                if (A.Right != null) A.Right.Parent = B;
                if (A.Left != null) A.Left.Parent = B;

                if (B.Left != null) B.Left.Parent = A;

                if (!B.Parent.isHeader())
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

                Node Temp = A.Left;
                A.Left = B.Left;
                B.Left = Temp;
            }
            else
            {
                if (A.Parent == B.Parent)
                {
                    Node Temp = A.Parent.Left;
                    A.Parent.Left = A.Parent.Right;
                    A.Parent.Right = Temp;
                }
                else
                {
                    if (!A.Parent.isHeader())
                    {
                        if (A.Parent.Left == A)
                            A.Parent.Left = B;
                        else
                            A.Parent.Right = B;
                    }
                    else A.Parent.Parent = B;

                    if (!B.Parent.isHeader())
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

                Node Temp1 = A.Left;
                A.Left = B.Left;
                B.Left = Temp1;
                
                Node Temp2 = A.Right;
                A.Right = B.Right;
                B.Right = Temp2;

                Node Temp3 = A.Parent;
                A.Parent = B.Parent;
                B.Parent = Temp3;
            }

            State Balance = A.Balance; A.Balance = B.Balance; B.Balance = Balance;
        }

        static Node rotateLeft(Node root)
        {
            Node Parent = root.Parent;
            Node x = root.Right;

            root.Parent = x;
            x.Parent = Parent;
            if (x.Left != null) x.Left.Parent = root;

            root.Right = x.Left;
            x.Left = root;
            return x;
        }

        static Node rotateRight(Node root)
        {
            Node Parent = root.Parent;
            Node x = root.Left;

            root.Parent = x;
            x.Parent = Parent;
            if (x.Right != null) x.Right.Parent = root;

            root.Left = x.Right;
            x.Right = root;
            return x;
        }
        
        
        static Node balanceLeft(Node root)
        {
            Node left = root.Left;

            switch (left.Balance)
            {
                case LeftHigh:
                    root.Balance = State.Balanced;
                    left.Balance = State.Balanced;
                    root = rotateRight(root);
                    break;

                case RightHigh:
                    {
                        Node subRight = left.Right;
                        switch (subRight.Balance)
                        {
                            case Balanced:
                                root.Balance = State.Balanced;
                                left.Balance = State.Balanced;
                                break;

                            case RightHigh:
                                root.Balance = State.Balanced;
                                left.Balance = State.LeftHigh;
                                break;

                            case LeftHigh:
                                root.Balance = State.RightHigh;
                                left.Balance = State.Balanced;
                                break;
                        }
                        subRight.Balance = State.Balanced;
                        left = rotateLeft(left);
                        root.Left = left;
                        root = rotateRight(root);
                    }
                    break;

                case Balanced:
                    root.Balance = State.LeftHigh;
                    left.Balance = State.RightHigh;
                    root = rotateRight(root);
                    break;
            }
            
            return root;
        }

        static Node balanceRight(Node root)
        {
            Node right = root.Right;

            switch (right.Balance)
            {
                case RightHigh:
                    root.Balance = State.Balanced;
                    right.Balance = State.Balanced;
                    root = rotateLeft(root);
                    break;

                case LeftHigh:
                    {
                        Node subLeft = right.Left; // Left Subtree of Right
                        switch (subLeft.Balance)
                        {
                            case Balanced:
                                root.Balance = State.Balanced;
                                right.Balance = State.Balanced;
                                break;

                            case LeftHigh:
                                root.Balance = State.Balanced;
                                right.Balance = State.RightHigh;
                                break;

                            case RightHigh:
                                root.Balance = State.LeftHigh;
                                right.Balance = State.Balanced;
                                break;
                        }
                        subLeft.Balance = State.Balanced;
                        right = rotateRight(right);
                        root.Right = right;
                        root = rotateLeft(root);
                    }
                    break;

                case Balanced:
                    root.Balance = State.RightHigh;
                    right.Balance = State.LeftHigh;
                    root = rotateLeft(root);
                    break;
            }
            
            return root;
        }

        static void balanceTree(Node root, Direction From)
        {
            Boolean Taller = true;

            while (Taller)
            {
                Node Parent = root.Parent;
                Direction NextFrom = (Parent.Left == root) ? Direction.FromLeft : Direction.FromRight;

                if (From == Direction.FromLeft)
                {
                    switch (root.Balance)
                    {
                        case LeftHigh:
                            if (Parent.isHeader())
                                Parent.Parent = balanceLeft(Parent.Parent);
                            else if (Parent.Left == root)
                                Parent.Left = balanceLeft(Parent.Left);
                            else
                                Parent.Right = balanceLeft(Parent.Right);
                            Taller = false;
                            break;

                        case Balanced:
                            root.Balance = State.LeftHigh;
                            Taller = true;
                            break;

                        case RightHigh:
                            root.Balance = State.Balanced;
                            Taller = false;
                            break;
                    }
                }
                else
                {
                    switch (root.Balance)
                    {
                        case LeftHigh:
                            root.Balance = State.Balanced;
                            Taller = false;
                            break;

                        case Balanced:
                            root.Balance = State.RightHigh;
                            Taller = true;
                            break;

                        case RightHigh:
                            if (Parent.isHeader())
                                Parent.Parent = balanceRight(Parent.Parent);
                            else if (Parent.Left == root)
                                Parent.Left = balanceRight(Parent.Left);
                            else
                                Parent.Right = balanceRight(Parent.Right);
                            Taller = false;
                            break;
                    }
                }

                if (Taller) // skip up a level
                {
                    if (Parent.isHeader())
                        Taller = false;
                    else
                    {
                        root = Parent;
                        From = NextFrom;
                    }
                }
            }
        }    
        
        static void balanceTreeRemove(Node root, Direction From)
        {
            if (root.isHeader()) return;

            Boolean Shorter = true;

            while (Shorter)
            {
                Node Parent = root.Parent;
                Direction NextFrom = (Parent.Left == root) ? Direction.FromLeft : Direction.FromRight;

                if (From == Direction.FromLeft)
                {
                    switch (root.Balance)
                    {
                        case LeftHigh:
                            root.Balance = State.Balanced;
                            Shorter = true;
                            break;

                        case Balanced:
                            root.Balance = State.RightHigh;
                            Shorter = false;
                            break;

                        case RightHigh:
                            if (root.Right.Balance == State.Balanced)
                                Shorter = false;
                            else
                                Shorter = true;
                            if (Parent.isHeader())
                                Parent.Parent = balanceRight(Parent.Parent);
                            else if (Parent.Left == root)
                                Parent.Left = balanceRight(Parent.Left);
                            else
                                Parent.Right = balanceRight(Parent.Right);
                            break;
                    }
                }
                else
                {
                    switch (root.Balance)
                    {
                        case RightHigh:
                            root.Balance = State.Balanced;
                            Shorter = true;
                            break;

                        case Balanced:
                            root.Balance = State.LeftHigh;
                            Shorter = false;
                            break;

                        case LeftHigh:
                            if (root.Left.Balance == State.Balanced)
                                Shorter = false;
                            else
                                Shorter = true;
                            if (Parent.isHeader())
                                Parent.Parent = balanceLeft(Parent.Parent);
                            else if (Parent.Left == root)
                                Parent.Left = balanceLeft(Parent.Left);
                            else
                                Parent.Right = balanceLeft(Parent.Right);
                            break;
                    }
                }

                if (Shorter)
                {
                    if (Parent.isHeader())
                        Shorter = false;
                    else
                    {
                        From = NextFrom;
                        root = Parent;
                    }
                }
            }
        }
        
        static Node previousNode(Node Node)
        {
            if (Node.isHeader()) { return Node.Right; }

            if (Node.Left != null)
            {
                Node = Node.Left;
                while (Node.Right != null) Node = Node.Right;
            }
            else
            {
                Node y = Node.Parent;
                if (y.isHeader()) return y;
                while (Node == y.Left) { Node = y; y = y.Parent; }
                Node = y;
            }
            return Node;
        }

        static Node nextNode(Node Node)
        {
            if (Node.isHeader()) return Node.Left;

            if (Node.Right != null)
            {
                Node = Node.Right;
                while (Node.Left != null) Node = Node.Left;
            }
            else
            {
                Node y = Node.Parent;
                if (y.isHeader()) return y;
                while (Node == y.Right) { Node = y; y = y.Parent; }
                Node = y;
            }
            return Node;
        }
        

}

    public class SetNode<T> extends Node
    {
        public T Data;

        public SetNode(T dataType, Node Parent)
        {
            super(Parent);
            Data = dataType;
        }
    }

    public interface Cloneable
    {
        T clone();
    }

    public interface Cloner<T>
    {
        T clone(T t);
    }


    public class DefaultCloner implements Cloner, Serializable
    {
        public T clone(T t)
        {
            try
            {
                Cloneable copier = (Cloneable)t;
                return copier.clone();
            }
            catch(Exception e)
            {
                return t;
            }
        }
    }

    public class DefaultComparator implements Comparator, Serializable
    {
        public int compare(T t1, T t2)
        {
            Comparable key = (Comparable)t1;
            return key.compareTo(t2);
        }

        public boolean equals(T t1, T t2)
        {
            Comparable key = (Comparable)t1;
            return key.compareTo(t2) != 0;
        }

    }

    public enum Direction
    {
            FromLeft,
            FromRight
    }

    public class SetEntry implements Iterator
    {
        public SetEntry(Node n) { _Node = n; }

        public T value()
        {
                return ((SetNode)_Node).Data;
        }

        public Boolean isHeader() { return _Node.isHeader(); }

        public boolean equals(SetEntry other)
        {
            return _Node == other._Node;
        }
        
        public String toString()
        {
            return value().toString();
        }

         public boolean hasNext()
         {
    
            Node _Next = Utility.nextNode(_Node);
            return _Next.isHeader() ? false : true;
         }

        public T next()
        {
            _Node = Utility.nextNode(_Node);
            return ((SetNode)_Node).Data;
        }

        public T previous()
        {
            _Node = Utility.previousNode(_Node);
            return ((SetNode)_Node).Data;
        }

        public Boolean moveNext()
        {
            _Node = Utility.nextNode(_Node);
            return _Node.isHeader() ? false : true;            
        }

        public Boolean movePrevious()
        {
            _Node = Utility.previousNode(_Node);
            return _Node.isHeader() ? false : true;            
        }

        public Node _Node;
    }

    public enum SetOperation
    {
        Union,
        Intersection,
        SymmetricDifference,
        Difference
    }

    public class EntryAlreadyExistsException extends Exception
    {
        public EntryAlreadyExistsException()
        {
            super("An entry already exists in the collection.");            
        }
    }

    public class EntryNotFoundException extends Exception
    {
        public EntryNotFoundException()
        {
            super("An entry could not be found.");            
        }
    }

    public class InvalidSetOperationException extends Exception
    {
        public InvalidSetOperationException()
        {
            super("An invalid set operation was specified.");            
        }
    }

    public class Set<T> implements Iterable<T>,
                                   Cloneable<Set<T>>,
                                   Serializable
    {
        public Node Header;
        public long Nodes;
        Comparator<T> Compare;
        Cloner<T> Clone;
        
        public Set()
        {
            Nodes = 0;
            Header = new Node();
            Compare = new DefaultComparator<T>();
            Clone = new DefaultCloner<T>();
        }

        public Set(Iterable<T> Iterable)
        {
            Nodes = 0;
            Header = new Node();
            Compare = new DefaultComparator<T>();
            Clone = new DefaultCloner<T>();
            
            for(T t : Iterable)
            {
                try
                {
                    add(Clone.clone(t));
                }
                catch (EntryAlreadyExistsException e) {}
            }
        }

        public Set(Comparator<T> ComparatorIn)
        {
            Nodes = 0;
            Header = new Node();
            Compare = ComparatorIn;
            Clone = new DefaultCloner<T>();
        }

        public Set(Iterable<T> Iterable,
                   Comparator<T> ComparatorIn)
        {
            Nodes = 0;
            Header = new Node();
            Compare = ComparatorIn;
            Clone = new DefaultCloner<T>();
            
            for(T t : Iterable)
            {
                try
                {
                    add(Clone.clone(t));
                }
                catch (EntryAlreadyExistsException e) {}
            }
        }

        public Set(T... params)
        {
            Nodes = 0;
            Header = new Node();
            Compare = new DefaultComparator<T>();
            Clone = new DefaultCloner<T>();
            
            for(T t : params)
            {
                try
                {
                    add(Clone.clone(t));
                }
                catch (EntryAlreadyExistsException e) {}
            }
        }
        
        public Set(Set<T> A,
                   Set<T> B,
                   SetOperation operation) throws EntryAlreadyExistsException, InvalidSetOperationException
        {
            synchronized(A)
            {
            synchronized(B)
            {
            
            Compare = A.Compare;
            Nodes = 0;
            Header = new Node();
            Clone = new DefaultCloner<T>();

            SetEntry<T> first1 = A.begin();
            SetEntry<T> last1 = A.end();
            SetEntry<T> first2 = B.begin();
            SetEntry<T> last2 = B.end();

            switch (operation)
            {
                case Union:
                    while (!first1.equals(last1) && !first2.equals(last2))
                    {
                        int order = Compare.compare(first1.value(),first2.value());

                        if (order < 0)
                        {
                            add(Clone.clone(first1.value()));
                            first1.moveNext();
                        }

                        else if (order > 0)
                        {
                            add(Clone.clone(first2.value()));
                            first2.moveNext();
                        }

                        else
                        {
                            add(first1.value());
                            first1.moveNext();
                            first2.moveNext();
                        }
                    }
                    while (!first1.equals(last1))
                    {
                        add(Clone.clone(first1.value()));
                        first1.moveNext();
                    }
                    while (!first2.equals(last2))
                    {
                        add(Clone.clone(first2.value()));
                        first2.moveNext();
                    }
                    return;

                case Intersection:
                    while (!first1.equals(last1) && !first2.equals(last2))
                    {
                        int order = Compare.compare(first1.value(),first2.value());

                        if (order < 0)
                            first1.moveNext();

                        else if (order > 0)
                            first2.moveNext();

                        else
                        {
                            add(Clone.clone(first1.value()));
                            first1.moveNext();
                            first2.moveNext();
                        }
                    }
                    return;

                case SymmetricDifference:
                    while (!first1.equals(last1) && !first2.equals(last2))
                    {
                        int order = Compare.compare(first1.value(),first2.value());

                        if (order < 0)
                        {
                            add(Clone.clone(first1.value()));
                            first1.moveNext();
                        }

                        else if (order > 0)
                        {
                            add(Clone.clone(first2.value()));
                            first2.moveNext();
                        }

                        else
                        { first1.moveNext(); first2.moveNext(); }
                    }

                    while (!first1.equals(last1))
                    {
                        add(Clone.clone(first1.value()));
                        first1.moveNext();
                    }

                    while (!first2.equals(last2))
                    {
                        add(Clone.clone(first2.value()));
                        first2.moveNext();
                    }
                    return;

                case Difference:
                    while (!first1.equals(last1) && !first2.equals(last2))
                    {
                        int order = Compare.compare(first1.value(),first2.value());

                        if (order < 0)
                        {
                            add(Clone.clone(first1.value()));
                            first1.moveNext();
                        }

                        else if (order > 0)
                        {
                            add(Clone.clone(first1.value()));
                            first1.moveNext();
                            first2.moveNext();
                        }

                        else
                        { first1.moveNext(); first2.moveNext(); }
                    }

                    while (!first1.equals(last1))
                    {
                        add(Clone.clone(first1.value()));
                        first1.moveNext();
                    }
                    return;
            }

            throw new InvalidSetOperationException();
            }
            }
        }

        public Set(Set<T> SetToCopy)
        {
            synchronized(SetToCopy)
            {
                Nodes = 0;
                Header = new Node();
                Compare = SetToCopy.Compare;
                Clone = SetToCopy.Clone;
            
                for(T t : SetToCopy)
                {
                    try
                    {
                        add(Clone.clone(t));
                    }
                    catch (EntryAlreadyExistsException e) {}
                }
            }
        }
        
        private void readObject(java.io.ObjectInputStream stream) throws IOException, ClassNotFoundException
        {
            Header = new Node();            
            Nodes = 0;
            
            int Count = stream.readInt();
            Compare = (Comparator<T>)stream.readObject();
            Clone = (Cloner<T>)stream.readObject();
            for (int i=0; i<Count; i++)
               try
               {
                   add((T)stream.readObject());
               }
               catch(EntryAlreadyExistsException e) {}
        }

        private void writeObject(java.io.ObjectOutputStream stream)  throws IOException
        {
            synchronized (this)
            {
                stream.writeInt((int)Nodes);
                stream.writeObject(Compare);
                stream.writeObject(Clone);
                for (T t : this) stream.writeObject(t);
            }
        }

        private void readObjectNoData() throws ObjectStreamException
        {
            Header = new Node();            
            Nodes = 0;
        }
        
        public long length() {return Nodes;}
        
        
        public Iterator<T> iterator()
        {
            return new SetEntry<T>(Header);
        }

        public SetEntry<T> after(T key, boolean equals)
        {
            synchronized (this)
            {
                return new SetEntry<T>(equals ? afterEquals(key) : after(key));
            }
        }
               
        public SetEntry<T> before(T key, boolean equals)
        {
            synchronized (this)
            {
                return new SetEntry<T>(equals ? beforeEquals(key) : before(key));
            }
        }
        
        public Comparator<T> getComparator() { return Compare; }
        
        public Set<T> clone()
        {
            synchronized(this)
            {
                Set<T> Out = new Set<T>(Compare);
                Out.Clone = Clone;
            
                for (T t : this)
                {
                    try
                    {
                        Out.add(Clone.clone(t));
                    }
                    catch (EntryAlreadyExistsException e) {}
                }
            
                return Out;
            }
        }
        
        public long depth()
        {
            synchronized (this)
            {
                return Utility.depth(Header.Parent);
            }
        }
        
        public Cloner<T> getCloner() {synchronized (this) {return Clone;} }
        
        public void setCloner(Cloner<T> C) {synchronized (this) {Clone = C;}}

        public SetEntry<T> insert(T data)  throws EntryAlreadyExistsException
        {
            return new SetEntry<T>(add(data));
        }
        
        public SetNode<T> add(T data) throws EntryAlreadyExistsException
        {
            synchronized(this)
            {
                if (Header.Parent == null)
                {
                    SetNode<T> newNode = new SetNode<T>(data, Header);
                    Header.Parent = newNode;
                    Nodes++;
                    Header.Left = Header.Parent;
                    Header.Right = Header.Parent;
                    return newNode;
                }
                else
                {
                    SetNode<T> newRoot = (SetNode<T>)Header.Parent;
                
                    for (; ; )
                    {
                        int compare = Compare.compare(data,newRoot.Data);

                        if (compare == 0) // Item Exists
                            throw new EntryAlreadyExistsException();

                        else if (compare < 0)
                        {
                            if (newRoot.Left != null)
                                newRoot = (SetNode<T>)newRoot.Left;
                            else
                            {
                                SetNode<T> NewNode = new SetNode<T>(data, newRoot);
                                Nodes++;
                                newRoot.Left = NewNode;
                                if (Header.Left == newRoot) Header.Left = NewNode;
                                Utility.balanceTree(newRoot, Direction.FromLeft);
                                return NewNode;
                            }
                        }

                        else
                        {
                            if (newRoot.Right != null)
                                newRoot = (SetNode<T>)newRoot.Right;
                            else
                            {
                                SetNode<T> NewNode = new SetNode<T>(data, newRoot);
                                Nodes++;
                                newRoot.Right = NewNode;
                                if (Header.Right == newRoot) Header.Right = NewNode;
                                Utility.balanceTree(newRoot, Direction.FromRight);
                                return NewNode;
                            }
                        }
                    }
                }
            }
        }

        public long remove()
        {
            synchronized (this)
            {
                long count = Nodes;
                Header.Parent = null;
                Header.Left = Header;
                Header.Right = Header;
                Nodes = 0;
                return count;
            }

        }

        public void remove(T data) throws EntryNotFoundException
        {
            synchronized (this)
            {
                SetNode<T> root = (SetNode<T>)Header.Parent;

                for (; ; )
                {
                    if (root == null)
                        throw new EntryNotFoundException();

                    int compare = Compare.compare(data,root.Data);

                    if (compare < 0)
                        root = (SetNode<T>)root.Left;

                    else if (compare > 0)
                        root = (SetNode<T>)root.Right;

                    else // Item is found
                    {
                        if (root.Left != null && root.Right != null)
                        {
                            Node replace = root.Left;
                            while (replace.Right != null) replace = replace.Right;
                            Utility.swapNodes(root, replace);
                        }

                        Node Parent = root.Parent;

                        Direction From = (Parent.Left == root) ? Direction.FromLeft : Direction.FromRight;

                        if (Header.Left == root)
                        {
                            Node n = Utility.nextNode(root);

                            if (n.isHeader())
                            { Header.Left = Header; Header.Right = Header; }
                            else
                                Header.Left = n;
                        }
                        else if (Header.Right == root)
                        {
                            Node p = Utility.previousNode(root);

                            if (p.isHeader())
                            { Header.Left = Header; Header.Right = Header; }
                            else
                                Header.Right = p;
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

                        Utility.balanceTreeRemove(Parent, From);

                        Nodes--;
                        break;
                    }
                }
            }
        }

        public boolean exists(T data)
        {
            synchronized (this)
            {
                if (Header.Parent == null)
                    return false;
                else
                {
                    Node search = Header.Parent;

                    do
                    {
                        int Result = Compare.compare(data,((SetNode<T>)search).Data);

                        if (Result < 0) search = search.Left;

                        else if (Result > 0) search = search.Right;

                        else break;

                    } while (search != null);

                    return search != null;
                }
            }
        }

        public T find(T data) throws EntryNotFoundException
        {
            synchronized (this)
            {
                if (Header.Parent == null)
                    throw new EntryNotFoundException();
                else
                {
                    Node search = Header.Parent;

                    do
                    {
                        int Result = Compare.compare(data,((SetNode<T>)search).Data);

                        if (Result < 0) search = search.Left;

                        else if (Result > 0) search = search.Right;

                        else break;

                    } while (search != null);

                    if (search != null) throw new EntryNotFoundException();
                    
                    return ((SetNode<T>)search).Data;
                }
            }
        }

        public SetEntry<T> locate(T data) throws EntryNotFoundException
        {
            synchronized (this)
            {
                if (Header.Parent == null)
                    throw new EntryNotFoundException();
                else
                {
                    Node search = Header.Parent;

                    do
                    {
                        int Result = Compare.compare(data,((SetNode<T>)search).Data);

                        if (Result < 0) search = search.Left;

                        else if (Result > 0) search = search.Right;

                        else break;

                    } while (search != null);

                    if (search != null) throw new EntryNotFoundException();
                    
                    return new SetEntry(search);
                }
            }
        }

        
        public SetEntry<T> begin() { return new SetEntry<T>(Header.Left); }
        
        public SetEntry<T> end() { return new SetEntry<T>(Header); }

        public String toString()
        {
            synchronized(this)
            {
                String StringOut = "{";

                SetEntry<T> start = begin();
                SetEntry<T> end = end();
                SetEntry<T> last = end(); last.movePrevious();

                while (!start.equals(end))
                {
                    String NewStringOut = start.value().toString();
                    if (!start.equals(last)) NewStringOut = NewStringOut + ",";
                    StringOut = StringOut + NewStringOut;
                    start.moveNext();
                }

                StringOut = StringOut + "}";
                return StringOut;
            }
        }

        Node after(T data)
        {
            Node y = Header;
            Node x = Header.Parent;

            while (x != null)
                if (Compare.compare(data,((SetNode<T>)x).Data) < 0)
                { y = x; x = x.Left; }
                else
                    x = x.Right;

            return y;
        }

        Node afterEquals(T data)
        {
            Node y = Header;
            Node x = Header.Parent;

            while (x != null)
            {
                int c = Compare.compare(data,((SetNode<T>)x).Data);
                if (c == 0)
                { y = x; break; }
                else if (c < 0)
                { y = x; x = x.Left; }
                else
                    x = x.Right;
            }

            return y;
        }
        
       Node before(T data)
        {
            Node y = Header;
            Node x = Header.Parent;

            while (x != null)
                if (Compare.compare(data,((SetNode<T>)x).Data) <= 0)
                    x = x.Left;
                else
                { y = x; x = x.Right; }

            return y;
        }

        Node beforeEquals(T data)
        {
            Node y = Header;
            Node x = Header.Parent;

            while (x != null)
            {
                int c = Compare.compare(data,((SetNode<T>)x).Data);
                if (c == 0)
                { y = x; break; }
                else if (c < 0)
                    x = x.Left;
                else
                { y = x; x = x.Right; }
            }

            return y;
        }
        
        public boolean equals(Set<T> compare)
        {
            synchronized (this)
            {
                SetEntry<T> first1 = begin();
                SetEntry<T> last1 = end();
                SetEntry<T> first2 = compare.begin();
                SetEntry<T> last2 = compare.end();

                Boolean equals = true;

                while (!first1.equals(last1) && !first2.equals(last2))
                {
                    if (Compare.compare(first1.value(),first2.value()) == 0)
                    { first1.moveNext(); first2.moveNext(); }
                    else
                    { equals = false; break; }
                }

                if (equals)
                {
                    if (!first1.equals(last1)) equals = false;
                    if (!first2.equals(last2)) equals = false;
                }

                return equals;
            }
        }
        
        public boolean isSubset(Set<T> S)
        {
            synchronized (this)
            {
            synchronized (S)
            {
                SetEntry<T> first1 = begin();
                SetEntry<T> last1 = end();
                SetEntry<T> first2 = S.begin();
                SetEntry<T> last2 = S.end();

                boolean subset = true;

                while (!first1.equals(last1) && !first2.equals(last2))
                {
                    int order = Compare.compare(first1.value(), first2.value());
                    if (order < 0)
                    { subset = false; break; }
                    else if (order > 0)
                       first2.moveNext();
                    else
                    { first1.moveNext(); first2.moveNext(); }
                }

                if (subset)
                   if (!first1.equals(last1)) subset = false;

                return subset;
            }
            }
        }
    }


```

