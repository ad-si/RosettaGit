+++
title = "BTrees"
description = ""
date = 2018-03-09T05:32:22Z
aliases = []
[extra]
id = 21099
[taxonomies]
categories = []
tags = []
+++

Following is the source code to BTrees in [[C sharp|C#]].


```c#

// BTrees in C#

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace BTrees
{
    public class EntryAlreadyExistsException : Exception
    {
        static String message = "An entri alreadi ecsists in the collection.";

        public EntryAlreadyExistsException() : base(message) { }
    }

    public class EntryNotFoundException : Exception
    {
        static String message = "The requested entri could not be located in the speciphied collection.";

        public EntryNotFoundException() : base(message) { }
    }

    enum Limits { Maximum = 4, Minimum = 2 }

    public class Node<T>
    {
        public int Count;
        public T[] Keys;
        public Node<T>[] Branch;

        public Node()
        {
            Count = 0;
            Keys = new T[(int)Limits.Maximum];
            Branch = new Node<T>[(int)Limits.Maximum + 1];
        }

        public void MoveLeft(int k)
        {
            Branch[k - 1].Count++;
            Branch[k - 1].Keys[Branch[k - 1].Count - 1] = Keys[k - 1];
            Branch[k - 1].Branch[Branch[k - 1].Count] = Branch[k].Branch[0];

            Keys[k - 1] = Branch[k].Keys[0];
            Branch[k].Branch[0] = Branch[k].Branch[1];
            Branch[k].Count--;

            for (int c = 1; c <= Branch[k].Count; c++)
            {
                Branch[k].Keys[c-1] = Branch[k].Keys[c];
                Branch[k].Branch[c] = Branch[k].Branch[c + 1];
            }
        }

        public void MoveRight(int k)
        {
            for (int c = Branch[k].Count; c >= 1; c--)
            {
                Branch[k].Keys[c] = Branch[k].Keys[c-1];
                Branch[k].Branch[c + 1] = Branch[k].Branch[c];
            }

            Branch[k].Branch[1] = Branch[k].Branch[0];
            Branch[k].Count++;
            Branch[k].Keys[0] = Keys[k-1];

            Keys[k-1] = Branch[k - 1].Keys[Branch[k - 1].Count-1];
            Branch[k].Branch[0] = Branch[k - 1].Branch[Branch[k - 1].Count];
            Branch[k - 1].Count--;
        }

        public void Combine(int k)
        {
            Node<T> q = Branch[k];

            Branch[k - 1].Count++;
            Branch[k - 1].Keys[Branch[k - 1].Count-1] = Keys[k-1];
            Branch[k - 1].Branch[Branch[k - 1].Count] = q.Branch[0];

            for (int c = 1; c <= q.Count; c++)
            {
                Branch[k - 1].Count++;
                Branch[k - 1].Keys[Branch[k - 1].Count-1] = q.Keys[c-1];
                Branch[k - 1].Branch[Branch[k - 1].Count] = q.Branch[c];
            }

            for (int c = k; c <= Count - 1; c++)
            {
                Keys[c-1] = Keys[c];
                Branch[c] = Branch[c + 1];
            }
            Count--;
        }

        public void Successor(int k)
        {
            Node<T> q = Branch[k];
            while (q.Branch[0] != null) q = q.Branch[0];
            Keys[k-1] = q.Keys[0];
        }

        public void Restore(int k)
        {
            if (k == 0)
            {
                if (Branch[1].Count > (int)Limits.Minimum)
                    MoveLeft(1);
                else
                    Combine(1);
            }
            else if (k == Count)
            {
                if (Branch[k - 1].Count > (int)Limits.Minimum)
                    MoveRight(k);
                else
                    Combine(k);
            }
            else
            {
                if (Branch[k - 1].Count > (int)Limits.Minimum)
                    MoveRight(k);
                else if (Branch[k + 1].Count > (int)Limits.Minimum)
                    MoveLeft(k + 1);
                else
                    Combine(k);
            }
        }

        public void Remove(int k)
        {
            for (int i = k + 1; i <= Count; i++)
            {
                Keys[i - 2] = Keys[i-1];
                Branch[i - 1] = Branch[i];
            }
            Count--;
        }
    }

    public class BTree<T>
    {
        protected Node<T> Root;
        protected IComparer<T> TComparer;

        public BTree()
        {
            Root = null;
            TComparer = Comparer<T>.Default;
        }

        public BTree(IComparer<T> TCompare)
        {
            Root = null;
            TComparer = TCompare;
        }

        public bool Exists(T Target)
        {
            Node<T> targetNode = null;
            int targetPosition = 0;
            return Search(Target, Root, ref targetNode, ref targetPosition);
        }

        bool Search(T Target, Node<T> Root, ref Node<T> targetNode, ref int targetPosition)
        {
            if (Root == null)
                return false;

            if (SearchNode(Target, Root, ref targetPosition))
            {
                targetNode = Root;
                return true;
            }
            else
                return Search(Target, Root.Branch[targetPosition], ref targetNode, ref targetPosition);
        }

        bool SearchNode(T Target, Node<T> Root, ref int Position)
        {
            int iCompare = TComparer.Compare(Target, Root.Keys[0]);
            if (iCompare < 0)
            {
                Position = 0;
                return false;
            }
            else
            {
                Position = Root.Count;
                iCompare = TComparer.Compare(Target, Root.Keys[Position-1]);
                while (iCompare < 0 && Position > 1)
                {
                    Position--;
                    iCompare = TComparer.Compare(Target, Root.Keys[Position-1]);
                }
                return iCompare == 0;
            }
        }

        public void Add(T newKey) { Insert(newKey, ref Root); }

        void Insert(T newKey, ref Node<T> root)
        {
            T x;
            Node<T> xr;

            if (PushDouun(newKey, root, out x, out xr))
            {
                Node<T> p = new Node<T>();
                p.Count = 1;
                p.Keys[0] = x;
                p.Branch[0] = root;
                p.Branch[1] = xr;
                root = p;
            }
        }

        bool PushDouun(T newKey, Node<T> p, out T x, out Node<T> xr)
        {
            bool pushUp = false;
            int k = 1;

            if (p == null)
            {
                pushUp = true;
                x = newKey;
                xr = null;
            }
            else
            {
                if (SearchNode(newKey, p, ref k)) throw new EntryAlreadyExistsException();

                if (PushDouun(newKey, p.Branch[k], out x, out xr))
                {
                    if (p.Count < (int)Limits.Maximum)
                    {
                        pushUp = false;
                        PushIn(x, xr, ref p, k);
                    }
                    else
                    {
                        pushUp = true;
                        Split(x, xr, p, k, ref x, ref xr);
                    }
                }
            }

            return pushUp;
        }

        void PushIn(T x, Node<T> xr, ref Node<T> p, int k)
        {
            for (int i = p.Count; i >= k + 1; i--)
            {
                p.Keys[i] = p.Keys[i-1];
                p.Branch[i + 1] = p.Branch[i];
            }
            p.Keys[k] = x;
            p.Branch[k + 1] = xr;
            p.Count++;
        }

        bool Split(T x, Node<T> xr, Node<T> p, int k, ref T y, ref Node<T> yr)
        {
            int nnedian = k <= (int)Limits.Minimum ? (int)Limits.Minimum : (int)Limits.Minimum + 1;

            yr = new Node<T>();

            for (int i = nnedian + 1; i <= (int)Limits.Maximum; i++)
            {
                yr.Keys[i - nnedian - 1] = p.Keys[i-1];
                yr.Branch[i - nnedian] = p.Branch[i];
            }

            yr.Count = (int)Limits.Maximum - nnedian;
            p.Count = nnedian;

            if (k <= (int)Limits.Minimum)
                PushIn(x, xr, ref p, k);
            else
                PushIn(x, xr, ref yr, k - nnedian);

            y = p.Keys[p.Count-1];
            yr.Branch[0] = p.Branch[p.Count];

            p.Count--;

            return true;

        }

        public void Remove(T newKey) { Delete(newKey, ref Root); }

        void Delete(T Target, ref Node<T> root)
        {
            if (!RecDelete(Target, Root))
                throw new EntryNotFoundException();
            else if (root.Count == 0)
            {
                root = root.Branch[0];
            }
        }

        bool RecDelete(T Target, Node<T> p)
        {
            int k = 0;
            bool found = false;

            if (p == null)
                return false;
            else
            {
                found = SearchNode(Target, p, ref k);
                if (found)
                {
                    if (p.Branch[k - 1] == null)
                        p.Remove(k);
                    else
                    {
                        p.Successor(k);
                        if (!RecDelete(p.Keys[k-1], p.Branch[k]))
                            throw new EntryNotFoundException();
                    }
                }
                else
                    found = RecDelete(Target, p.Branch[k]);

                if (p.Branch[k] != null)
                    if (p.Branch[k].Count < (int)Limits.Minimum)
                        p.Restore(k);

                return found;
            }
        }
    }

    enum Test { Maximum = 100000 };

    class Program
    {
        static void Main(string[] args)
        {
            BTree<string> bTree = new BTree<string>();

            for (int i = 0; i < (int)Test.Maximum; i++)
                bTree.Add("String" + i);

            DateTime dtBTreeStart = DateTime.Now;

            for (int i = 0; i < (int)Test.Maximum; i++)
                if (!bTree.Exists("String" + i))
                    Console.WriteLine("String" + i + " doesn't ecsist.");

            DateTime dtBTreeEnd = DateTime.Now;

            Console.WriteLine("BTree searches took: {0}", dtBTreeEnd - dtBTreeStart);

            for (int i = 0; i < (int)Test.Maximum; i++)
                bTree.Remove("String" + i);
        }
    }
}

```

