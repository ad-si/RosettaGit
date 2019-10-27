+++
title = "AVL tree/Managed C++"
description = ""
date = 2018-05-19T11:09:28Z
aliases = []
[extra]
id = 21003
[taxonomies]
categories = []
tags = []
+++

== Code ==

```cpp
 
// AVL in Managed C++

using namespace System;
using namespace System::Collections;
using namespace System::Collections::Generic;
using namespace System::Threading;
using namespace System::Runtime::Serialization;

namespace Calculus
{

public enum class State
{
 Header,
 LeftHigh,
 Balanced,
 RightHigh
};

public enum class Direction { FromLeft, FromRight };

public ref struct Node
{
 Node^ Left;
 Node^ Right;
 Node^ Parent;
 State Balance;

 Node()
 {
  Left = this;
  Right = this;
  Parent = nullptr;
  Balance = State::Header;
 }

 Node(Node^ p)
 {
  Left = nullptr;
  Right = nullptr;
  Parent = p;
  Balance = State::Balanced;
 }

 property Boolean IsHeader
 { Boolean get() { return Balance == State::Header; } }
};

generic <typename T>
public delegate void TypeFound(Object^ O, T type);

generic <typename T>
public delegate void TypeAdded(Object^ O, T type);

generic <typename T>
public delegate void TypeRemoved(Object^ O, T type);

generic <typename T>
public delegate void TypeUpdated(Object^ O, T before, T after);

generic<typename T>
public interface struct IHasher
{
 int GetHashCode(T t);
};

generic<typename T>
[Serializable]
public ref class Hasher abstract : IHasher<T>
{
 public:

  static property Hasher<T>^ Default { Hasher<T>^ get(); }

  virtual int GetHashCode(T t) = 0;
};

generic<typename T>
[Serializable]
public ref class DefaultHasher : Hasher<T>
{
 public:

 virtual int GetHashCode(T t) override
 {
  return t->GetHashCode();
 }
};

generic<typename T>
Hasher<T>^ Hasher<T>::Default::get() { return gcnew DefaultHasher<T>(); }


generic<typename T>
 public interface struct ICloneable
 {
  T Clone();
 };

generic<typename T>
public interface class ICloner  {  T Clone(T t); };

generic<typename T>
[Serializable]
 public ref struct Cloner abstract : ICloner<T>
{
 static property Cloner<T>^ Default { Cloner<T>^ get(); }

 static property Cloner<T>^ Invisible { Cloner<T>^ get(); }

 virtual T Clone(T t) = 0;
};

generic<typename T>
[Serializable]
public ref struct DefaultCloner1 : Cloner<T>
{
 virtual T Clone(T t) override
 {
  ICloneable<T>^ copier = (ICloneable<T>^)t;
  return copier->Clone();
 }
};

generic<typename T>
[Serializable]
public ref struct DefaultCloner2 : Cloner<T>
{
 virtual T Clone(T t) override
 {
  ICloneable<T>^ copier = (ICloneable<T>^)t;
  return (T)copier->Clone();
 }
};

generic<typename T>
[Serializable]
public ref struct DefaultNoCloner : Cloner<T>
{
 virtual T Clone(T t) override
 {
  return t;
 }
};

generic<typename T>
Cloner<T>^ Cloner<T>::Default::get()
{
   Type^ TypeT = T::typeid;
   Type^ TypeIC1 = ICloneable<T>::typeid;
   Type^ TypeIC2 = ICloneable::typeid;
   if (TypeIC1->IsAssignableFrom(TypeT))
     return gcnew DefaultCloner1<T>();
   else if (TypeIC2->IsAssignableFrom(TypeT))
     return gcnew DefaultCloner2<T>();
   else
     return gcnew DefaultNoCloner<T>();
}

 generic<typename T>
 Cloner<T>^ Cloner<T>::Invisible::get() { return gcnew DefaultNoCloner<T>(); } 

    public ref struct OutOfKeyOrderException : public Exception
    {
        static String^ message = gcnew String("A tree was found to be out of key order.");

        OutOfKeyOrderException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

    public ref struct TreeInvalidParentException : public Exception
    {
        static String^ message = gcnew String("The validation routines detected that the parent structure of a tree is invalid.");

        TreeInvalidParentException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

    public ref struct TreeOutOfBalanceException : public Exception
    {
        static String^ message = gcnew String("The validation routines detected that the tree is out of balance.");

        TreeOutOfBalanceException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

    public ref struct InvalidEmptyTreeException : public Exception
    {
        static String^ message = gcnew String("The validation routines detected that an empty tree is invalid.");

        InvalidEmptyTreeException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };


    public ref struct InvalidEndItemException : public Exception
    {
        static String^ message = gcnew String("The validation routines detected that the end item of a tree is invalid.");

        InvalidEndItemException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

     public ref struct EntryAlreadyExistsException : public Exception
    {
        static String^ message = gcnew String("An entry already exists in the collection.");

        EntryAlreadyExistsException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

    public ref struct DifferentKeysException : public Exception
    {
        static String^ message = gcnew String("The specified items have different keys.");

        DifferentKeysException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

    public ref struct AddSubTreeFailedException : public Exception
    {
        static String^ message = gcnew String("Subtree creation failed.");

        AddSubTreeFailedException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

    public ref struct IsEndItemException : public Exception
    {
        static String^ message = gcnew String("The requested action cannot be performed on an end item.");

        IsEndItemException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

    public ref struct EntryNotFoundException : public Exception
    {
        static String^ message = gcnew String("The requested entry could not be located in the specified collection.");

        EntryNotFoundException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

    public ref struct InvalidSetOperationException : public Exception
    {
        static String^ message = gcnew String("An invalid set operation was specified.");

        InvalidSetOperationException() : Exception(message)
        {
            HelpLink = gcnew String("Benedict@NNcNannara.net");
            Source = gcnew String("Calculus Subsystem");
        }
    };

Node^ PreviousItem(Node^ node)
{
 if (node->IsHeader) {return node->Right;}

 else if (node->Left != nullptr)
  {
   Node^ y = node->Left;
   while (y->Right != nullptr) y = y->Right;
   node = y;
  }
 else
  {
   Node^ y = node->Parent;
   if (y->IsHeader) return y;
   while (node == y->Left) {node = y; y = y->Parent;}
   node = y;
  }
 return node;
}

Node^ NextItem(Node^ node)
{
 if (node->IsHeader) return node->Left;

 if (node->Right != nullptr)
  {
   node = node->Right;
   while (node->Left != nullptr) node = node->Left;
  }
 else
  {
   Node^ y = node->Parent;
   if (y->IsHeader) return y;
   while (node == y->Right) {node = y; y = y->Parent;}
   node = y;
  }
 return node;
}

void SwapNodeReference(Node^% first, Node^% second)
{Node^ temporary = first; first = second; second = temporary;}

void LeftNodeSwap(Node^ Root, Node^ Replace)
{
 if (Replace->Left) Replace->Left->Parent = Root;
 if (Replace->Right) Replace->Right->Parent = Root;

 if (Root->Right) Root->Right->Parent = Replace;

 if (Replace == Root->Left)
  {
   Replace->Parent = Root->Parent;
   Root->Parent = Replace;

   Root->Left = Replace->Left;
   Replace->Left = Root;
  }
 else
  {
   Root->Left->Parent = Replace;

   if (Replace->Parent->Left == Replace)
    Replace->Parent->Left = Root;
   else
    Replace->Parent->Right = Root;

   SwapNodeReference(Root->Left,Replace->Left);
   SwapNodeReference(Root->Parent,Replace->Parent);
  }

 SwapNodeReference(Root->Right,Replace->Right);

 State Balance = Root->Balance;
 Root->Balance = Replace->Balance;
 Replace->Balance=Balance;
}

void SwapNodes(Node^ A, Node^ B)
{
 if (B == A->Left)
  {
   if (B->Left) B->Left->Parent = A;
   if (B->Right) B->Right->Parent = A;

   if (A->Right) A->Right->Parent = B;

   if (!A->Parent->IsHeader)
    {
     if (A->Parent->Left == A)
      A->Parent->Left = B;
     else
      A->Parent->Right = B;
    }
   else A->Parent->Parent = B;

   B->Parent = A->Parent;
   A->Parent = B;

   A->Left = B->Left;
   B->Left = A;

   SwapNodeReference(A->Right,B->Right);
  }
 else if (B == A->Right)
  {
   if (B->Right) B->Right->Parent = A;
   if (B->Left) B->Left->Parent = A;

   if (A->Left) A->Left->Parent = B;

   if (!A->Parent->IsHeader)
    {
     if (A->Parent->Left == A)
      A->Parent->Left = B;
     else
      A->Parent->Right = B;
    }
   else A->Parent->Parent = B;

   B->Parent = A->Parent;
   A->Parent = B;

   A->Right = B->Right;
   B->Right = A;

   SwapNodeReference(A->Left,B->Left);
  }
 else if (A == B->Left)
  {
   if (A->Left) A->Left->Parent = B;
   if (A->Right) A->Right->Parent = B;

   if (B->Right) B->Right->Parent = A;

   if (!B->Parent->IsHeader)
    {
     if (B->Parent->Left == B)
      B->Parent->Left = A;
     else
      B->Parent->Right = A;
    }
   else B->Parent->Parent = A;

   A->Parent = B->Parent;
   B->Parent = A;

   B->Left = A->Left;
   A->Left = B;

   SwapNodeReference(A->Right,B->Right);
  }
 else if (A == B->Right)
  {
   if (A->Right) A->Right->Parent = B;
   if (A->Left) A->Left->Parent = B;

   if (B->Left) B->Left->Parent = A;

   if (!B->Parent->IsHeader)
    {
     if (B->Parent->Left == B)
      B->Parent->Left = A;
     else
      B->Parent->Right = A;
    }
   else B->Parent->Parent = A;

   A->Parent = B->Parent;
   B->Parent = A;

   B->Right = A->Right;
   A->Right = B;

   SwapNodeReference(A->Left,B->Left);
  }
 else
  {
   if (A->Parent == B->Parent)
    SwapNodeReference(A->Parent->Left,A->Parent->Right);
   else
    { 
     if (!A->Parent->IsHeader)
      {
       if (A->Parent->Left == A)
        A->Parent->Left = B;
       else
        A->Parent->Right = B;
      }
     else A->Parent->Parent = B;

     if (!B->Parent->IsHeader)
      {
       if (B->Parent->Left == B)
        B->Parent->Left = A;
       else
        B->Parent->Right = A;
      }
     else B->Parent->Parent = A;
    }

   if (B->Left)  B->Left->Parent = A;
   if (B->Right) B->Right->Parent = A;

   if (A->Left)  A->Left->Parent = B;
   if (A->Right) A->Right->Parent = B;

   SwapNodeReference(A->Left,B->Left);
   SwapNodeReference(A->Right,B->Right);
   SwapNodeReference(A->Parent,B->Parent);
  }

 State Balance = A->Balance;
 A->Balance = B->Balance;
 B->Balance=Balance;
}

void RotateLeft(Node^% Root)
{
 Node^ Parent = Root->Parent;
 Node^ x = Root->Right;

 Root->Parent = x;
 x->Parent = Parent;
 if (x->Left) x->Left->Parent = Root; 

 Root->Right = x->Left;
 x->Left = Root;
 Root = x;
}    

void RotateRight(Node^% Root)
{
 Node^ Parent = Root->Parent;
 Node^ x = Root->Left;

 Root->Parent = x;
 x->Parent = Parent;
 if (x->Right) x->Right->Parent = Root; 

 Root->Left = x->Right;
 x->Right = Root;
 Root = x;
} 

void BalanceLeft(Node^% Root)
{
 Node^ Left = Root->Left; // Left Subtree of Root Node

 switch (Left->Balance)
  {
   case State::LeftHigh:
    Root->Balance = State::Balanced;
    Left->Balance = State::Balanced;
    RotateRight(Root);
    break;           
    
   case State::RightHigh:
    {
     Node^ subRight = Left->Right;  // Right subtree of Left
     switch (subRight->Balance)
      {
       case State::Balanced:
        Root->Balance = State::Balanced;
        Left->Balance = State::Balanced;
        break;

       case State::RightHigh:
        Root->Balance = State::Balanced;
        Left->Balance = State::LeftHigh;
        break;

       case State::LeftHigh:
        Root->Balance = State::RightHigh;
        Left->Balance = State::Balanced;
        break;
      }
     subRight->Balance = State::Balanced;
     RotateLeft(Left);
     Root->Left = Left;
     RotateRight(Root);
    }
    break;

   case State::Balanced:
    Root->Balance = State::LeftHigh;
    Left->Balance = State::RightHigh;
    RotateRight(Root);
    break;           
  }
} 

void BalanceRight(Node^% Root)
{
 Node^ Right = Root->Right; // Right Subtree of Root Node

 switch (Right->Balance)
  {
   case State::RightHigh:
    Root ->Balance = State::Balanced;
    Right->Balance = State::Balanced;
    RotateLeft(Root);
    break;

   case State::LeftHigh:
    {
     Node^ subLeft = Right->Left; // Left Subtree of Right
     switch (subLeft->Balance)
      {
       case State::Balanced:
        Root ->Balance = State::Balanced;
        Right->Balance = State::Balanced;
        break;

       case State::LeftHigh:
        Root ->Balance = State::Balanced;
        Right->Balance = State::RightHigh;
        break;

       case State::RightHigh:
        Root ->Balance = State::LeftHigh;
        Right->Balance = State::Balanced;
        break;
      }
     subLeft->Balance = State::Balanced;
     RotateRight(Right);
     Root->Right = Right;
     RotateLeft(Root);
    }
    break;         

   case State::Balanced:
    Root ->Balance = State::RightHigh;
    Right->Balance = State::LeftHigh;
    RotateLeft(Root);
    break;           
  }
} 

void BalanceTree(Node^ Root, Direction From)
{
  bool Taller = true;

  while (Taller)
   {
    Node^ Parent = Root->Parent;
    Direction NextFrom = (Parent->Left == Root) ? Direction::FromLeft : Direction::FromRight;

    if (From == Direction::FromLeft)
    {
     switch (Root->Balance)
      {
       case State::LeftHigh:
        if (Parent->IsHeader)
          BalanceLeft(Parent->Parent);
        else if (Parent->Left == Root)
          BalanceLeft(Parent->Left);
        else
          BalanceLeft(Parent->Right);
        Taller = false;
        break;

        case State::Balanced:
         Root->Balance = State::LeftHigh;
         Taller = true;
         break;

        case State::RightHigh:
         Root->Balance = State::Balanced;
         Taller = false;
         break;
       }
     }
    else
     {
      switch (Root->Balance)
       {
        case State::LeftHigh:
         Root->Balance = State::Balanced;
         Taller = false;
         break;

        case State::Balanced:
         Root->Balance = State::RightHigh;
         Taller = true;
         break;

        case State::RightHigh:
         if (Parent->IsHeader)
           BalanceRight(Parent->Parent);
         else if (Parent->Left == Root)
           BalanceRight(Parent->Left);
         else
           BalanceRight(Parent->Right);
         Taller = false;
         break;
        }
      }

      if (Taller) // skip up a level
      {
       if (Parent->IsHeader)
        Taller = false;
       else
       {
        Root = Parent;
        From = NextFrom;
       }
     }
   }
 }

void BalanceTreeRemove(Node^ Root, Direction From)
{
  if (Root->IsHeader) return;
  bool Shorter = true;

  while (Shorter)
   {
    Node^ Parent = Root->Parent;
    Direction NextFrom = (Parent->Left == Root) ? Direction::FromLeft : Direction::FromRight;

    if (From == Direction::FromLeft)
     {
      switch (Root->Balance)
       {
        case State::LeftHigh:
         Root->Balance = State::Balanced;
         Shorter = true;
         break;

        case State::Balanced:
         Root->Balance = State::RightHigh;
         Shorter = false;
         break;

        case State::RightHigh:
         if (Root->Right->Balance == State::Balanced)
          Shorter = false;
         else
          Shorter = true;
        if (Parent->IsHeader)
         BalanceRight(Parent->Parent);
        else if (Parent->Left == Root)
         BalanceRight(Parent->Left);
        else
         BalanceRight(Parent->Right);
        break;
       }
    }
   else
    {
     switch (Root->Balance)
      {
       case State::RightHigh:
        Root->Balance = State::Balanced;
        Shorter = true;
        break;

       case State::Balanced:
        Root->Balance = State::LeftHigh;
        Shorter = false;
        break;

       case State::LeftHigh:
        if (Root->Left->Balance == State::Balanced)
         Shorter = false;
        else
         Shorter = true;
        if (Parent->IsHeader)
          BalanceLeft(Parent->Parent);
        else if (Parent->Left == Root)
          BalanceLeft(Parent->Left);
        else
          BalanceLeft(Parent->Right);
        break;
       }
     }

     if (Shorter)
      {
       if (Parent->IsHeader)
        Shorter = false;
       else
        {
         From = NextFrom;
         Root = Parent;
        }
      }
   }
}

Node^ Minimum(Node^ node)
{
 while (node->Left) node=node->Left;
 return node;
}

Node^ Maximum(Node^ node)
{
 while (node->Right) node=node->Right;
 return node;
}

void AdjustAdd(Node^ Root)
{
 Node^ Header = Root->Parent;
 while (!Header->IsHeader) Header=Header->Parent;

 if (Root->Parent->Left == Root)
 {
  BalanceTree(Root->Parent,Direction::FromLeft);
  if (Header->Left == Root->Parent) Header->Left = Root;
 }
 else
 {
  BalanceTree(Root->Parent,Direction::FromRight);
  if (Header->Right == Root->Parent) Header->Right = Root;
 }
}

void AdjustRemove(Node^ Parent, Direction Direction)
{
 BalanceTreeRemove(Parent,Direction);
 
 Node^ Header = Parent;
 while (!Header->IsHeader) Header=Header->Parent;

 if (Header->Parent == nullptr)
 {
  Header->Left = Header;
  Header->Right = Header;
 }
 else
 {
  Header->Left = Minimum(Header->Parent);
  Header->Right = Maximum(Header->Parent);
 }
}

unsigned long long Depth(Node^ Root)
{
 if (Root)
  {
   unsigned long long Left  = Root->Left  ? Depth(Root->Left)  : 0;
   unsigned long long Right = Root->Right ? Depth(Root->Right) : 0;
   return Left < Right ? Right+1 : Left+1;
  }
 else
  return 0;
}

unsigned long long Count(Node^ Root)
{
 if (Root)
  {
   unsigned long long left  = Root->Left  ? Count(Root->Left)  : 0;
   unsigned long long right = Root->Right ? Count(Root->Right) : 0;
   return left + right + 1;
  }
 else
  return 0;
}

 public enum class SetOperation
 {
            Union,
            Intersection,
            SymmetricDifference,
            Difference,
            Equality,
            Inequality,
            Subset,
            Superset
 };

generic<typename T>
public ref class SetNode : Node
{
 public:

  T Data;

  SetNode(T dataType, Node^ Parent) : Node(Parent) {Data = dataType; }
};

generic<typename T>
public value struct SetEntry : System::Collections::Generic::IEnumerator<T>
{
 public:

  SetEntry(Node^ n) { _Node = n; }

  property T Value
  {
   T get()
    {
     if (_Node->Balance == State::Header) throw gcnew IsEndItemException();
     return ((SetNode<T>^)_Node)->Data;
    }
   void set(T Value)
    {
     if (_Node->Balance == State::Header) throw gcnew IsEndItemException();
     ((SetNode<T>^)_Node)->Data = Value;
    }
  }

 property Boolean IsHeader { Boolean get() { return _Node->IsHeader; } }

 virtual Boolean MoveNext()
 {
  _Node = NextItem(_Node);
  return _Node->IsHeader ? false : true;
 }

 Boolean MovePrevious()
 {
  _Node = PreviousItem(_Node);
  return _Node->IsHeader ? false : true;
 }

 static SetEntry<T> operator ++(SetEntry<T> entry)
 {
  entry._Node = NextItem(entry._Node);
  return entry;
 }

 static SetEntry<T> operator --(SetEntry<T> entry)
 {
  entry._Node = PreviousItem(entry._Node);
  return entry;
 }

 static SetEntry<T> operator +(SetEntry<T> C, unsigned long long Increment)
 {
  SetEntry<T> Result =  SetEntry<T>(C._Node);
  for (unsigned long long i = 0; i < Increment; i++) ++Result;
  return Result;
 }

 static SetEntry<T> operator +(unsigned long long Increment, SetEntry<T> C)
 {
  SetEntry<T> Result = SetEntry<T>(C._Node);
  for (unsigned long long i = 0; i < Increment; i++) ++Result;
  return Result;
 }

 static SetEntry<T> operator -(SetEntry<T> C, unsigned long long Decrement)
 {
  SetEntry<T> Result = SetEntry<T>(C._Node);
  for (unsigned long long i = 0; i < Decrement; i++) --Result;
  return Result;
 }

 virtual void Reset()
 { while (!_Node->IsHeader) _Node = NextItem(_Node); }

 virtual property Object^ InterfaceCurrentA
  {
   Object^ get()  = System::Collections::IEnumerator::Current::get
      {
          if (_Node->Balance == State::Header) throw gcnew IsEndItemException();
          return ((SetNode<T>^)_Node)->Data;
      }
  }

 virtual property T InterfaceCurrentB
  {
   T get()  = System::Collections::Generic::IEnumerator<T>::Current::get
      {
          if (_Node->Balance == State::Header) throw gcnew IsEndItemException();
          return ((SetNode<T>^)_Node)->Data;
      }
  }

 static Boolean operator ==(SetEntry<T> x, SetEntry<T> y) { return x._Node == y._Node; }
 static Boolean operator !=(SetEntry<T> x, SetEntry<T> y) { return x._Node != y._Node; }

 static long long operator -(SetEntry<T> This, SetEntry<T> iter)
 {
  long long Result = 0;
  while (This._Node != iter._Node) { iter.MoveNext(); Result++; }
  return Result;
 }

 virtual String^ ToString() override
 {
  if (_Node->Balance == State::Header) throw gcnew IsEndItemException();
  return Value->ToString();
 }

 Node^ _Node;
};


generic<typename T>
[Serializable]
public ref class Set : public System::Collections::Generic::ICollection<T>,
                       public System::ICloneable,
                       public ISerializable,
                       public IComparable<Set<T>^>,
                       public IEquatable<Set<T>^>
{
 public:
  event TypeAdded<T>^ Added;
  event TypeRemoved<T>^ Removed;
  event TypeUpdated<T>^ Updated;

 protected:
  Node^ Header;
  System::Collections::Generic::IComparer<T>^ TComparer;
  ICloner<T>^ TCloner;
  IHasher<T>^ THasher;
  unsigned long long Nodes;

  property Node^ Root
  {
   Node^ get() { return Header->Parent; }
   void set(Node^ Value) { Header->Parent = Value; }
  }

        //*** Constructors ***

 public:

  Set()
   {
    Nodes=0;
    Header = gcnew Node();
    TComparer = System::Collections::Generic::Comparer<T>::Default;
    TCloner = Calculus::Cloner<T>::Default;
    THasher = Calculus::Hasher<T>::Default;
   }

  Set(System::Collections::Generic::IComparer<T>^ TCompare)
  {
   Nodes=0;
   Header = gcnew Node();
   TComparer = TCompare;
   TCloner = Calculus::Cloner<T>::Default;
   THasher = Calculus::Hasher<T>::Default;
  }

  Set(Set<T>^ SetToCopy)
  {
   Nodes=0;
   Header = gcnew Node();
   TComparer = SetToCopy->TComparer;
   TCloner = SetToCopy->TCloner;
   THasher = SetToCopy->THasher;
   Copy((SetNode<T>^)SetToCopy->Root);
  }

  Set(System::Collections::Generic::IEnumerable<T>^ Collection)
  {
   Nodes=0;
   Header = gcnew Node();
   TComparer = System::Collections::Generic::Comparer<T>::Default;
   TCloner = Calculus::Cloner<T>::Default;
   THasher = Calculus::Hasher<T>::Default;

   for each (T t in Collection) Add(TCloner->Clone(t));
  }

  Set(... array<T>^ Collection)
  {
   Nodes=0;
   Header = gcnew Node();
   TComparer = System::Collections::Generic::Comparer<T>::Default;
   TCloner = Calculus::Cloner<T>::Default;
   THasher = Calculus::Hasher<T>::Default;

   for each (T t in Collection) Add(TCloner->Clone(t));
  }

  Set(System::Collections::Generic::IEnumerable<T>^ Collection,
      System::Collections::Generic::IComparer<T>^ TCompare)
  {
   Nodes=0;
   Header = gcnew Node();
   TComparer = TCompare;
   TCloner = Calculus::Cloner<T>::Default;
   THasher = Calculus::Hasher<T>::Default;
 
   for each (T t in Collection) Add(TCloner->Clone(t));
  }

  Set(Set<T>^ A,
      Set<T>^ B,
      SetOperation operation)
  {
   Nodes=0;
   Header = gcnew Node();
   TComparer = A->TComparer;
   TCloner = A->TCloner;
   THasher = A->THasher;
   CombineSets(A, B, this, operation);
  }

  Set(SerializationInfo^ si, StreamingContext sc)
  {
   Nodes=0;
   System::Collections::Generic::IComparer<T>^ TCompare = (System::Collections::Generic::IComparer<T>^)si->GetValue("TComparer", System::Collections::Generic::IComparer<T>::typeid);
   Calculus::ICloner<T>^ TClone = (Calculus::ICloner<T>^)si->GetValue("TCloner", ICloner<T>::typeid);
   Calculus::IHasher<T>^ THasher = (Calculus::IHasher<T>^)si->GetValue("THasher", IHasher<T>::typeid);

   Header = gcnew Node();
   TComparer = TCompare;
   TCloner = TClone;

   Type^ type = T::typeid;

   unsigned long long LoadCount = si->GetUInt64("Count");

   for (unsigned long long i = 0; i < LoadCount; i++)
   {
    Object^ obj = si->GetValue(i.ToString(), type);
    Add((T)obj, false);
   }
  }

        //*** Operators ***

  static Set<T>^ operator |(Set<T>^ A, Set<T>^ B)
  {
   Set<T>^ U = gcnew Set<T>(A->TComparer);
   U->TCloner = A->TCloner;
   U->THasher = A->THasher;
   CombineSets(A, B, U, SetOperation::Union);
   return U;
  }

  static Set<T>^ operator &(Set<T>^ A, Set<T>^ B)
  {
   Set<T>^ I = gcnew Set<T>(A->TComparer);
   I->TCloner = A->TCloner;
   I->THasher = A->THasher;
   CombineSets(A, B, I, SetOperation::Intersection);
   return I;
  }

  static Set<T>^ operator ^(Set<T>^ A, Set<T>^ B)
  {
   Set<T>^ S = gcnew Set<T>(A->TComparer);
   S->TCloner = A->TCloner;
   S->THasher = A->THasher;
   CombineSets(A, B, S, SetOperation::SymmetricDifference);
   return S;
  }

  static Set<T>^ operator -(Set<T>^ A, Set<T>^ B)
  {
   Set<T>^ S = gcnew Set<T>(A->TComparer);
   S->TCloner = A->TCloner;
   S->THasher = A->THasher;
   CombineSets(A, B, S, SetOperation::Difference);
   return S;
  }

  static Boolean operator ==(Set<T>^ A, Set<T>^ B)
  {
   return CheckSets(A, B, SetOperation::Equality);
  }

  static Boolean operator !=(Set<T>^ A, Set<T>^ B)
  {
   return CheckSets(A, B, SetOperation::Inequality);
  }

  static Boolean operator <(Set<T>^ A, Set<T>^ B)
  {
   return CheckSets(A, B, SetOperation::Subset);
  }

  static Boolean operator >(Set<T>^ A, Set<T>^ B)
  {
   return CheckSets(A, B, SetOperation::Superset);
  }

  property Boolean default [T]
  {
   Boolean get(T key)
   {
    if (Root == nullptr)
     return false;
    else
     {
      Node^ search = Root;

      do
       {
        int Result = TComparer->Compare(key, static_cast<SetNode<T>^>(search)->Data);

        if (Result < 0) search = search->Left;

        else if (Result > 0) search = search->Right;

        else break;

       } while (search != nullptr);

       return search != nullptr;
      }
    }
  }

 static Set<T>^ operator +(Set<T>^ set, T t)
 {
  set->Add(t, false);
  return set;
 }

 static Set<T>^ operator -(Set<T>^ set, T t)
 {
  set->Remove(t);
  return set;
 }

 //*** Properties ***

 property SetEntry<T> Begin
 { SetEntry<T> get() { return SetEntry<T>(Header->Left); } }

 property ICloner<T>^ TypeCloner
 {
  ICloner<T>^ get() { return TCloner; }
  void set(ICloner<T>^ Value) { TCloner = Value; }
 }
 property System::Collections::Generic::IComparer<T>^ Comparer
 {System::Collections::Generic::IComparer<T>^ get() { return TComparer; } }

 virtual property int Count { int get() { return (int)Length; } }

 property SetEntry<T> End
 { SetEntry<T> get() { return SetEntry<T>(Header); } }

 property T First
 { T get() { return ((SetNode<T>^)LeftMost)->Data; } }

 property int Hash { int get() { return GetHashCode(); } }

 property IHasher<T>^ Hasher
 {
  IHasher<T>^ get() { return THasher; }
  void set(IHasher<T>^ Value) { THasher = Value; }
 }

 virtual property Boolean IsReadOnly { Boolean get() { return false; } }

 virtual property Boolean IsSynchronized { Boolean get() { return true; } }

 property T Last
 { T get() { return ((SetNode<T>^)RightMost)->Data; } }

 property Node^ LeftMost
 {
  Node^ get() { return Header->Left; }
  void set(Node^ Value) { Header->Left = Value; }
 }

 property unsigned long long Length {  unsigned long long get() { return Count;} }

 property Node^ RightMost
 {
  Node^ get() { return Header->Right; }
  void set(Node^ Value) { Header->Right = Value; }
 }
 
 property Object^ SyncRoot { Object^ get() { return this; } }

        //*** Public Methods ***

 SetEntry<T> After(T Value, Boolean equals)
 {
  return SetEntry<T>(equals ? AfterEquals(Value) : After(Value));
 }

 virtual void Add(T t)
 {
  Add(t, false);
 }

 void Add(SetEntry<T> cse)
 {
  Add(TCloner->Clone(cse.Value), false);
 }

 unsigned long long Add(System::Collections::Generic::IEnumerable<T>^ copy)
 {
  unsigned long long count = 0;

  for each(T t in copy)
  {
   Add(TCloner->Clone(t), false);
   count++;
  }

  return count;
 }

 SetEntry<T> Before(T value, bool equals)
 {
  return SetEntry<T>(equals ? BeforeEquals(value) : Before(value));
 }

 virtual void Clear() { Remove(); }

 void CallRemoved(T data) { Removed(this, data); }

 virtual Object^ Clone()
 {
  Set<T>^ setOut = gcnew Set<T>(TComparer);
  setOut->TCloner = TCloner;
  setOut->THasher = THasher;
  setOut->Copy((SetNode<T>^)Root);
  return setOut;
 }

 virtual int CompareTo(Set<T>^ B)
 {
  return CompareSets(this, B);
 }

 virtual bool Contains(T t)
 {
  Node^ found = Search(t);
  return found != nullptr ? true : false;
 }

 virtual Boolean Contains(Set<T>^ ss)
 {
  for each (T t in ss)
   if (Search(t) == nullptr) return false;

  return true;
 }

 virtual void CopyTo(array<T>^ arr, int i)
 {
  SetEntry<T> begin = SetEntry<T>((SetNode<T>^)Header->Left);
  SetEntry<T> end = SetEntry<T>(Header);

  while (begin != end)
  {
   arr->SetValue(TCloner->Clone(((SetNode<T>^)begin._Node)->Data), i);
   i++; begin.MoveNext();
  }
 }

  virtual void CopyTo(System::Array^ arr, int i)
  {
   SetEntry<T> begin = SetEntry<T>((SetNode<T>^)Header->Left);
   SetEntry<T> end = SetEntry<T>(Header);

   while (begin != end)
   {
     arr->SetValue(TCloner->Clone(((SetNode<T>^)begin._Node)->Data), i);
     i++; begin.MoveNext();
   }
  }

  virtual Boolean Equals(Set<T>^ compare)
  {
   SetEntry<T> first1 = Begin;
   SetEntry<T> last1 = End;
   SetEntry<T> first2 = compare->Begin;
   SetEntry<T> last2 = compare->End;

   Boolean equals = true;

   while (first1 != last1 && first2 != last2)
    {
     if (TComparer->Compare(first1.Value, first2.Value) == 0)
      { first1.MoveNext(); first2.MoveNext(); }
     else
      { equals = false; break; }
    }

   if (equals)
    {
     if (first1 != last1) equals = false;
     if (first2 != last2) equals = false;
    }

   return equals;
  }

  T Find(T value)
  {
   Node^ _Node = Search(value);
   if (_Node == nullptr)
     throw gcnew EntryNotFoundException();
   return ((SetNode<T>^)_Node)->Data;
  }

 virtual System::Collections::IEnumerator^ InterfaceGetEnumeratorSimple() sealed = System::Collections::IEnumerable::GetEnumerator
 { return gcnew SetEntry<T>(Header); }

 virtual System::Collections::Generic::IEnumerator<T>^ InterfaceGetEnumerator() sealed = System::Collections::Generic::IEnumerable<T>::GetEnumerator
 { return gcnew SetEntry<T>(Header); }

  virtual Int32 GetHashCode() override
  {
   Int32 HashCode = 0;
   for each (T t in this) HashCode += THasher->GetHashCode(t);
   return HashCode;
  }

  virtual void GetObjectData(SerializationInfo^ si, StreamingContext sc)
  {
   si->SetType(Calculus::Set<T>::typeid);

   Type^ type = T::typeid;

   unsigned long long index = 0;
   for each (T e in *this)
   {
     si->AddValue(index.ToString(), e, type);
     index++;
   }

   si->AddValue("Count", index);
   si->AddValue("TComparer", TComparer, TComparer->GetType());
   si->AddValue("TCloner", TCloner, TCloner->GetType());
   si->AddValue("THasher", THasher, THasher->GetType());
  }

  SetEntry<T> Insert(T t) { return SetEntry<T>(Add(t, false)); }

  SetEntry<T> Locate(T value)
  {
   Node^ _Node = Search(value);
   if (_Node == nullptr)
     throw gcnew EntryNotFoundException();
   return SetEntry<T>(_Node);
  }

  void Notify()
  {
   Notify((SetNode<T>^)Root);
  }

  unsigned long long Remove()
  {
   for each (T t in this) Removed(this, t);
   unsigned long long count = Nodes;
   Root = nullptr;
   LeftMost = Header;
   RightMost = Header;
   Nodes = 0;
   return count;
  }

  virtual bool Remove(T data)
  {
   Node^ root = Root;

   for (; ; )
   {
    if (root == nullptr) throw gcnew EntryNotFoundException();

    int compare = TComparer->Compare(data, ((SetNode<T>^)root)->Data);

    if (compare < 0)
     root = root->Left;

    else if (compare > 0)
     root = root->Right;

    else // Item is found
    {
     if (root->Left != nullptr && root->Right != nullptr)
     {
      Node^ replace = root->Left;
      while (replace->Right != nullptr) replace = replace->Right;
      SwapNodes(root, replace);
     }

     Node^ Parent = root->Parent;

     Direction From = (Parent->Left == root) ? Direction::FromLeft : Direction::FromRight;

     if (LeftMost == root)
      {
       Node^ n = NextItem(root);

       if (n->IsHeader)
        { LeftMost = Header; RightMost = Header; }
       else
         LeftMost = n;
      }
      else if (RightMost == root)
       {
        Node^ p = PreviousItem(root);

        if (p->IsHeader)
         { LeftMost = Header; RightMost = Header; }
        else
          RightMost = p;
       }

     if (root->Left == nullptr)
     {
      if (Parent == Header)
        Header->Parent = root->Right;
      else if (Parent->Left == root)
        Parent->Left = root->Right;
      else
        Parent->Right = root->Right;

      if (root->Right != nullptr) root->Right->Parent = Parent;
     }
     else
      {
       if (Parent == Header)
        Header->Parent = root->Left;
       else if (Parent->Left == root)
        Parent->Left = root->Left;
       else
        Parent->Right = root->Left;

       if (root->Left != nullptr) root->Left->Parent = Parent;
      }

      AdjustRemove(Parent, From);
      Nodes--;
      Removed(this, ((SetNode<T>^)root)->Data);
      break;
     }
   }
   return true;
  }

  void Remove(SetEntry<T> i) { Remove(i._Node); }

  Node^ Search(T data)
  {
   if (Root == nullptr)
     return nullptr;
   else
    {
     Node^ search = Root;

     do
      {
       int Result = TComparer->Compare(data, ((SetNode<T>^)search)->Data);

       if (Result < 0) search = search->Left;

       else if (Result > 0) search = search->Right;

       else break;

      } while (search != nullptr);

      return search;
    }
  }

  virtual String^ ToString() override
  {
   String^ StringOut = gcnew String("{");

   SetEntry<T> start = Begin;
   SetEntry<T> end = End;
   SetEntry<T> last = End - 1;

   while (start != end)
   {
    String^ NewStringOut = start.Value->ToString();
    if (start != last) NewStringOut = NewStringOut + gcnew String(",");
    StringOut = StringOut + NewStringOut;
    ++start;
   }

   StringOut = StringOut + gcnew String("}");
   return StringOut;
  }

 void Update(T value)
 {
  if (Root == nullptr)
   throw gcnew EntryNotFoundException();
  else
   {
    Node^ search = Root;

    do
    {
     int Result = TComparer->Compare(value, ((SetNode<T>^)search)->Data);

     if (Result < 0) search = search->Left;

     else if (Result > 0) search = search->Right;

     else break;

    } while (search != nullptr);

    if (search == nullptr) throw gcnew EntryNotFoundException();

    T saved = ((SetNode<T>^)search)->Data;
    ((SetNode<T>^)search)->Data = value;
    Updated(this, saved, value);
  }
 }

 void Update(SetEntry<T>^ entry, T after) {  Update((SetNode<T>^)entry->_Node, after); }

 void Validate()
 {
  if (Nodes == 0 || Root == nullptr)
  {
   if (Nodes != 0) { throw gcnew InvalidEmptyTreeException(); }
   if (Root != nullptr) { throw gcnew InvalidEmptyTreeException(); }
   if (LeftMost != Header) { throw gcnew InvalidEndItemException(); }
   if (RightMost != Header) { throw gcnew InvalidEndItemException(); }
  }

  Validate((SetNode<T>^)Root);

  if (Root != nullptr)
  {
   Node^ x = Root;
   while (x->Left != nullptr) x = x->Left;

   if (LeftMost != x) throw gcnew InvalidEndItemException();

   Node^ y = Root;
   while (y->Right != nullptr) y = y->Right;

   if (RightMost != y) throw gcnew InvalidEndItemException();
  }
 }

        //*** Private Methods ***

 protected:

 Node^ After(T data)
 {
  Node^ y = Header;
  Node^ x = Root;

  while (x != nullptr)
   if (TComparer->Compare(data, ((SetNode<T>^)x)->Data) < 0)
    { y = x; x = x->Left; }
   else
     x = x->Right;

  return y;
 }

 Node^ AfterEquals(T data)
 {
  Node^ y = Header;
  Node^ x = Root;

  while (x != nullptr)
  {
   int c = TComparer->Compare(data, ((SetNode<T>^)x)->Data);
   if (c == 0)
    { y = x; break; }
   else if (c < 0)
    { y = x; x = x->Left; }
   else
    x = x->Right;
  }

  return y;
 }

 SetNode<T>^ Add(T data, bool exist)
 {
  Node^ root = Root;

  if (root == nullptr)
   {
    Root = gcnew SetNode<T>(data, Header);
    Nodes++;
    LeftMost = Root;
    RightMost = Root;
    Added(this, ((SetNode<T>^)Root)->Data);
    return (SetNode<T>^)Root;
   }
  else
   {
    for (; ; )
     {
      int compare = TComparer->Compare(data, static_cast<SetNode<T>^>(root)->Data);

      if (compare == 0) // Item Exists
       {
        if (exist)
         {
          T saved = ((SetNode<T>^)root)->Data;
          ((SetNode<T>^)root)->Data = data;
          Updated(this, saved, data);
          return (SetNode<T>^)root;
         }
        else
         throw gcnew EntryAlreadyExistsException();
       }

      else if (compare < 0)
       {
        if (root->Left != nullptr)
         root = root->Left;
        else
         {
          SetNode<T>^ NewNode = gcnew SetNode<T>(data, root);
          Nodes++;
          root->Left = NewNode;
          AdjustAdd(NewNode);
          Added(this, NewNode->Data);
          return NewNode;
         }
      }

    else
     {
      if (root->Right != nullptr)
       root = root->Right;
      else
       {
        SetNode<T>^ NewNode = gcnew SetNode<T>(data, root);
        Nodes++;
        root->Right = NewNode;
        AdjustAdd(NewNode);
        Added(this, NewNode->Data);
        return NewNode;
       }
     }
    }
   }
 }

 unsigned long long Add(Node^ begin, Node^ end)
 {
  bool success = true;
  unsigned long long count = 0;

  SetEntry<T> i(begin);

  while (success && i._Node != end)
  {
   if (!i._Node->IsHeader)
    {
     try
      {
       Add(TCloner->Clone(i.Value), false);
       count++;
       i.MoveNext();
      }
     catch (Exception^) { success = false; }
    }
   else i.MoveNext();
  }
  if (!success)
   {
    if (count != 0)
     {
      i.MovePrevious();
      SetEntry<T> start(begin); start.MovePrevious();

      while (i != start)
       {
        SetEntry<T> j(i._Node); j.MovePrevious();
        if (!i._Node->IsHeader) Remove(i.Value);
        i = j;
       }
     }
    throw gcnew AddSubTreeFailedException();
   }
  return Count;
 }

 Node^ Before(T data)
 {
  Node^ y = Header;
  Node^ x = Root;

  while (x != nullptr)
   if (TComparer->Compare(data, ((SetNode<T>^)x)->Data) <= 0)
    x = x->Left;
   else
    { y = x; x = x->Right; }

  return y;
 }

 Node^ BeforeEquals(T data)
 {
  Node^ y = Header;
  Node^ x = Root;

  while (x != nullptr)
  {
   int c = TComparer->Compare(data, ((SetNode<T>^)x)->Data);
   if (c == 0)
    { y = x; break; }
   else if (c < 0)
    x = x->Left;
   else
    { y = x; x = x->Right; }
  }

  return y;
 }

 void Bounds()
 {
  LeftMost = GetFirst();
  RightMost = GetLast();
 }

 void Copy(SetNode<T>^ CopyRoot)
 {
  if (Root != nullptr) Remove();
  if (CopyRoot != nullptr)
   {
     Copy(Header->Parent, CopyRoot, Header);
     LeftMost = GetFirst();
     RightMost = GetLast();
    }
 }

 void Copy(Node^% root, SetNode<T>^ CopyRoot, Node^ Parent)
 {
  root = gcnew SetNode<T>(TCloner->Clone(CopyRoot->Data), Parent);
  Nodes++;

  root->Balance = CopyRoot->Balance;

  if (CopyRoot->Left != nullptr)
   Copy(root->Left, (SetNode<T>^)CopyRoot->Left, (SetNode<T>^)root);

  if (CopyRoot->Right != nullptr)
   Copy(root->Right, (SetNode<T>^)CopyRoot->Right, (SetNode<T>^)root);

  Added(this, ((SetNode<T>^)root)->Data);
 }

 Node^ GetFirst()
 {
  if (Root == nullptr)
   return Header;

  else
   {
    Node^ search = Root;
    while (search->Left != nullptr) search = search->Left;
    return search;
   }
 }

 Node^ GetLast()
 {
  if (Root == nullptr)
   return Header;

  else
   {
    Node^ search = Root;
    while (search->Right != nullptr) search = search->Right;
    return search;
   }
 }

 void Import(SetNode<T>^ n)
 {
  if (n != nullptr) ImportTree(n);
 }

 void ImportTree(SetNode<T>^ n)
 {
  if (n->Left != nullptr) ImportTree((SetNode<T>^)n->Left);
  Add(n->Data, false);
  if (n->Right != nullptr) ImportTree((SetNode<T>^)n->Right);
 }

 void Notify(SetNode<T>^ root)
 {
  if (root != nullptr)
  {
   if (root->Left != nullptr)
    Notify((SetNode<T>^)root->Left);

   Added(this, root->Data);

   if (root->Right != nullptr)
    Notify((SetNode<T>^)root->Right);
  }
 }

 void Remove(Node^ root)
 {
  if (root->Left != nullptr && root->Right != nullptr)
  {
   Node^ replace = root->Left;
   while (replace->Right != nullptr) replace = replace->Right;
   SwapNodes(root, replace);
  }

  Node^ Parent = root->Parent;

  Direction From = (Parent->Left == root) ? Direction::FromLeft : Direction::FromRight;

  if (LeftMost == root)
  {
   Node^ n = NextItem(root);

   if (n->IsHeader)
    { LeftMost = Header; RightMost = Header; }
   else
    LeftMost = n;
  }
  else if (RightMost == root)
  {
    Node^ p = PreviousItem(root);

    if (p->IsHeader)
     { LeftMost = Header; RightMost = Header; }
    else
     RightMost = p;
  }

  if (root->Left == nullptr)
  {
   if (Parent == Header)
    Header->Parent = root->Right;
   else if (Parent->Left == root)
    Parent->Left = root->Right;
   else
    Parent->Right = root->Right;

   if (root->Right != nullptr) root->Right->Parent = Parent;
  }
  else
  {
   if (Parent == Header)
    Header->Parent = root->Left;
   else if (Parent->Left == root)
    Parent->Left = root->Left;
   else
    Parent->Right = root->Left;

   if (root->Left != nullptr) root->Left->Parent = Parent;
  }

  AdjustRemove(Parent, From);
  Nodes--;
  Removed(this, ((SetNode<T>^)root)->Data);
 }

 unsigned long long Remove(Node^ i, Node^ j)
 {
  if (i == LeftMost && j == Header)
   return Remove();
  else
   {
    unsigned long long count = 0;
    while (i != j)
    {
     SetEntry<T> iter(i); iter.MoveNext();
     if (i != Header) { Remove(i); count++; }
     i = iter._Node;
    }

    return count;
   }
 }

 void Update(SetNode<T>^ Node, T value)
 {
  if (TComparer->Compare(Node->Data, value) != 0) throw gcnew DifferentKeysException();
  T saved = Node->Data;
  Node->Data = value;
  Updated(this, saved, value);
 }

 void Validate(SetNode<T>^ root)
 {
  if (root == nullptr) return;

  if (root->Left != nullptr)
   {
    SetNode<T>^ Left = (SetNode<T>^)root->Left;

    if (TComparer->Compare(Left->Data, root->Data) >= 0)
     throw gcnew OutOfKeyOrderException();

    if (Left->Parent != root)
     throw gcnew TreeInvalidParentException();

    Validate((SetNode<T>^)root->Left);
   }

  if (root->Right != nullptr)
   {
    SetNode<T>^ Right = (SetNode<T>^)root->Right;

    if (TComparer->Compare(Right->Data, root->Data) <= 0)
     throw gcnew OutOfKeyOrderException();

    if (Right->Parent != root)
     throw gcnew TreeInvalidParentException();

    Validate((SetNode<T>^)root->Right);
   }

  unsigned long long DepthLeft = root->Left != nullptr ? Depth(root->Left) : 0;
  unsigned long long DepthRight = root->Right != nullptr ? Depth(root->Right) : 0;

  if (DepthLeft > DepthRight && DepthLeft - DepthRight > 2)
   throw gcnew TreeOutOfBalanceException();

  if (DepthLeft < DepthRight && DepthRight - DepthLeft > 2)
   throw gcnew TreeOutOfBalanceException();
 }

        //*** Static Methods

 static void CombineSets(Set<T>^ A,
                         Set<T>^ B,
                         Set<T>^ R,
                         SetOperation operation)
{
 System::Collections::Generic::IComparer<T>^ TComparer = R->TComparer;
 Calculus::ICloner<T>^ TCloner = R->TCloner;

 SetEntry<T> first1 = A->Begin;
 SetEntry<T> last1 = A->End;
 SetEntry<T> first2 = B->Begin;
 SetEntry<T> last2 = B->End;

            switch (operation)
            {
                case SetOperation::Union:
                    while (first1 != last1 && first2 != last2)
                    {
                        int order = TComparer->Compare(first1.Value, first2.Value);

                        if (order < 0)
                        {
                            R->Add(TCloner->Clone(first1.Value));
                            first1.MoveNext();
                        }

                        else if (order > 0)
                        {
                            R->Add(TCloner->Clone(first2.Value));
                            first2.MoveNext();
                        }

                        else
                        {
                            R->Add(TCloner->Clone(first1.Value));
                            first1.MoveNext();
                            first2.MoveNext();
                        }
                    }
                    while (first1 != last1)
                    {
                        R->Add(TCloner->Clone(first1.Value));
                        first1.MoveNext();
                    }
                    while (first2 != last2)
                    {
                        R->Add(TCloner->Clone(first2.Value));
                        first2.MoveNext();
                    }
                    return;

                case SetOperation::Intersection:
                    while (first1 != last1 && first2 != last2)
                    {
                        int order = TComparer->Compare(first1.Value, first2.Value);

                        if (order < 0)
                            first1.MoveNext();

                        else if (order > 0)
                            first2.MoveNext();

                        else
                        {
                            R->Add(TCloner->Clone(first1.Value));
                            first1.MoveNext();
                            first2.MoveNext();
                        }
                    }
                    return;

                case SetOperation::SymmetricDifference:
                    while (first1 != last1 && first2 != last2)
                    {
                        int order = TComparer->Compare(first1.Value, first2.Value);

                        if (order < 0)
                        {
                            R->Add(TCloner->Clone(first1.Value));
                            first1.MoveNext();
                        }

                        else if (order > 0)
                        {
                            R->Add(TCloner->Clone(first2.Value));
                            first2.MoveNext();
                        }

                        else
                        { first1.MoveNext(); first2.MoveNext(); }
                    }

                    while (first1 != last1)
                    {
                        R->Add(TCloner->Clone(first1.Value));
                        first1.MoveNext();
                    }

                    while (first2 != last2)
                    {
                        R->Add(TCloner->Clone(first2.Value));
                        first2.MoveNext();
                    }
                    return;

                case SetOperation::Difference:
                    while (first1 != last1 && first2 != last2)
                    {
                        int order = TComparer->Compare(first1.Value, first2.Value);

                        if (order < 0)
                        {
                            R->Add(TCloner->Clone(first1.Value));
                            first1.MoveNext();
                        }

                        else if (order > 0)
                        {
                            R->Add(TCloner->Clone(first1.Value));
                            first1.MoveNext();
                            first2.MoveNext();
                        }

                        else
                        { first1.MoveNext(); first2.MoveNext(); }
                    }

                    while (first1 != last1)
                    {
                        R->Add(TCloner->Clone(first1.Value));
                        first1.MoveNext();
                    }
                    return;
            }

            throw gcnew InvalidSetOperationException();
        }

        static Boolean CheckSets(Set<T>^ A,
                                 Set<T>^ B,
                                 SetOperation operation)
        {
            System::Collections::Generic::IComparer<T>^ TComparer = A->TComparer;

            SetEntry<T> first1 = A->Begin;
            SetEntry<T> last1 = A->End;
            SetEntry<T> first2 = B->Begin;
            SetEntry<T> last2 = B->End;

            switch (operation)
            {
                case SetOperation::Equality:
                case SetOperation::Inequality:
                    {
                        bool equals = true;

                        while (first1 != last1 && first2 != last2)
                        {
                            if (TComparer->Compare(first1.Value, first2.Value) == 0)
                            { first1.MoveNext(); first2.MoveNext(); }
                            else
                            { equals = false; break; }
                        }

                        if (equals)
                        {
                            if (first1 != last1) equals = false;
                            if (first2 != last2) equals = false;
                        }

                        if (operation == SetOperation::Equality)
                            return equals;
                        else
                            return !equals;
                    }

                case SetOperation::Subset:
                case SetOperation::Superset:
                    {
                        bool subset = true;

                        while (first1 != last1 && first2 != last2)
                        {
                            int order = TComparer->Compare(first1.Value, first2.Value);

                            if (order < 0)
                            { subset = false; break; }

                            else if (order > 0)
                                first2.MoveNext();

                            else
                            { first1.MoveNext(); first2.MoveNext(); }
                        }

                        if (subset)
                            if (first1 != last1) subset = false;

                        if (operation == SetOperation::Subset)
                            return subset;
                        else
                            return !subset;
                    }
            }

            throw gcnew InvalidSetOperationException();
        }

        static int CompareSets(Set<T>^ A,
                               Set<T>^ B)
        {
            System::Collections::Generic::IComparer<T>^ TComparer = A->TComparer;

            SetEntry<T> first1 = A->Begin;
            SetEntry<T> last1 = A->End;
            SetEntry<T> first2 = B->Begin;
            SetEntry<T> last2 = B->End;

            int Result = 0;

            while (first1 != last1 && first2 != last2)
            {
                Result = TComparer->Compare(first1.Value, first2.Value);
                if (Result == 0)
                { first1.MoveNext(); first2.MoveNext(); }
                else
                    return Result;
            }

            if (first1 != last1) return 1;
            if (first2 != last2) return -1;

            return 0;
        }
    };

}

using namespace Calculus;

int main(array<System::String ^> ^args)
{
    Set<int>^ S = gcnew Set<int>(1, 3, 5 , 6, 7, 9);
    Set<int>^ T = gcnew Set<int>(2, 4, 6 , 7, 8, 9);
    Set<int>^ U = S | T;
    Console::WriteLine(S + " | " + T + " == " + U);
    return 0;
}

```

