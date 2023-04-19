+++
title = "AVL tree/C++"
description = ""
date = 2018-05-19T11:09:29Z
aliases = []
[extra]
id = 20998
[taxonomies]
categories = []
tags = []
+++

== Code ==

```cpp

// The set template is the primary class of AVL Trees.
// The system is set up to add templates including tree and map.

#include<iostream>

class treeException
{
  public:

   treeException() {}
};

 class EntryAlreadyExistsException : public treeException
 {
  public:
    EntryAlreadyExistsException() {}
 };

 class EntryNotFoundException : public treeException
 {
  public:
    EntryNotFoundException()  {}
 };

 class InvalidSetOperationException : public treeException
 {
  public:
    InvalidSetOperationException() {}
 };

 class IsHeaderException : public treeException
 {
  public:
    IsHeaderException() {}
 };

struct State
 {
  enum
  {
   Header,
   Balanced,
   LeftHigh,
   RightHigh
  };
 };

struct Node  // Base Node Class for all Trees
{
 Node* Left;
 Node* Right;
 Node* Parent;
 char Balance;

 Node()
 {
  Balance = State::Header;
  Left    = this;
  Right   = this;
  Parent  = 0;
 }

 Node(Node* ParentSet)
 {
  Balance = State::Balanced;
  Left    = 0;
  Right   = 0;
  Parent  = ParentSet;
 }

 bool IsHeader() const {return !Balance;}
};

struct Direction
{
 enum {FromLeft, FromRight};
};

inline void SwapNodeReference(Node*& first, Node*& second)
{Node* temporary = first; first = second; second = temporary;}

void SwapNodes(Node* A, Node* B)
{
 if (B == A->Left)
  {
   if (B->Left) B->Left->Parent = A;
   if (B->Right) B->Right->Parent = A;

   if (A->Right) A->Right->Parent = B;

   if (!A->Parent->IsHeader())
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

   if (!A->Parent->IsHeader())
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

   if (!B->Parent->IsHeader())
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

   if (!B->Parent->IsHeader())
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
     if (!A->Parent->IsHeader())
      {
       if (A->Parent->Left == A)
        A->Parent->Left = B;
       else
        A->Parent->Right = B;
      }
     else A->Parent->Parent = B;

     if (!B->Parent->IsHeader())
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

 unsigned long Balance = A->Balance;
 A->Balance = B->Balance;
 B->Balance=(char)Balance;
}

inline void RotateLeft(Node*& Root)
{
 Node* Parent = Root->Parent;
 Node* x = Root->Right;

 Root->Parent = x;
 x->Parent = Parent;
 if (x->Left) x->Left->Parent = Root; 

 Root->Right = x->Left;
 x->Left = Root;
 Root = x;
}    

inline void RotateRight(Node*& Root)
{
 Node* Parent = Root->Parent;
 Node* x = Root->Left;

 Root->Parent = x;
 x->Parent = Parent;
 if (x->Right) x->Right->Parent = Root; 

 Root->Left = x->Right;
 x->Right = Root;
 Root = x;
} 

inline void BalanceLeft(Node*& Root)
{
 Node* Left = Root->Left; // Left Subtree of Root Node

 switch (Left->Balance)
  {
   case State::LeftHigh:
    Root->Balance = State::Balanced;
    Left->Balance = State::Balanced;
    RotateRight(Root);
    break;           
    
   case State::RightHigh:
    {
     Node* subRight = Left->Right;  // Right subtree of Left
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

inline void BalanceRight(Node*& Root)
{
 Node* Right = Root->Right; // Right Subtree of Root Node

 switch (Right->Balance)
  {
   case State::RightHigh:
    Root ->Balance = State::Balanced;
    Right->Balance = State::Balanced;
    RotateLeft(Root);
    break;

   case State::LeftHigh:
    {
     Node* subLeft = Right->Left; // Left Subtree of Right
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

inline void BalanceTree(Node* Root, unsigned long From)
{
  bool Taller = true;

  while (Taller)
   {
    Node* Parent = Root->Parent;
    unsigned long NextFrom = (Parent->Left == Root) ? Direction::FromLeft : Direction::FromRight;

    if (From == Direction::FromLeft)
    {
     switch (Root->Balance)
      {
       case State::LeftHigh:
        if (Parent->IsHeader())
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
         if (Parent->IsHeader())
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
       if (Parent->IsHeader())
        Taller = false;
       else
       {
        Root = Parent;
        From = NextFrom;
       }
     }
   }
 }


inline void BalanceTreeRemove(Node* Root, unsigned long From)
{
  if (Root->IsHeader()) return;
  bool Shorter = true;

  while (Shorter)
   {
    Node* Parent = Root->Parent;
    unsigned long NextFrom = (Parent->Left == Root) ? Direction::FromLeft : Direction::FromRight;

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
        if (Parent->IsHeader())
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
        if (Parent->IsHeader())
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
       if (Parent->IsHeader())
        Shorter = false;
       else
        {
         From = NextFrom;
         Root = Parent;
        }
      }
   }
}

Node* PreviousItem(Node* node)
{
 if (node->IsHeader()) {return node->Right;}

 else if (node->Left != 0)
  {
   Node* y = node->Left;
   while (y->Right != 0) y = y->Right;
   node = y;
  }
 else
  {
   Node* y = node->Parent;
   if (y->IsHeader()) return y;
   while (node == y->Left) {node = y; y = y->Parent;}
   node = y;
  }
 return node;
}

Node* NextItem(Node* node)
{
 if (node->IsHeader()) return node->Left;

 if (node->Right != 0)
  {
   node = node->Right;
   while (node->Left != 0) node = node->Left;
  }
 else
  {
   Node* y = node->Parent;
   if (y->IsHeader()) return y;
   while (node == y->Right) {node = y; y = y->Parent;}
   node = y;
  }
 return node;
}

inline Node* Minimum(Node* node)
{
 while (node->Left) node=node->Left;
 return node;
}

inline Node* Maximum(Node* node)
{
 while (node->Right) node=node->Right;
 return node;
}

void AdjustAdd(Node* Root)
{
 Node* Header = Root->Parent;
 while (!Header->IsHeader()) Header=Header->Parent;

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

void AdjustRemove(Node* Parent, unsigned long Direction)
{
 BalanceTreeRemove(Parent,Direction);
 
 Node* Header = Parent;
 while (!Header->IsHeader()) Header=Header->Parent;

 if (Header->Parent == 0)
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

unsigned long Depth(const Node* root)
{
 if (root)
  {
   unsigned long left  = root->Left  ? Depth(root->Left)  : 0;
   unsigned long right = root->Right ? Depth(root->Right) : 0;
   return left < right ? right+1 : left+1;
  }
 else
  return 0;
}


unsigned long Count(const Node* root)
{
 if (root)
  {
   unsigned long left  = root->Left  ? Count(root->Left)  : 0;
   unsigned long right = root->Right ? Count(root->Right) : 0;
   return left + right + 1;
  }
 else
  return 0;
}

struct setOperation
{
 enum
 {
  Union,
  Intersection,
  SymmetricDifference,
  Difference,
 };
};

template <class U, class V>
inline int compare(const U& u, const V& v)
{if (u < v) return -1; else if (v < u) return 1; else return 0;}

template<class T>
struct setNode : public Node
{
 T Element;

 setNode(const T& ElementSet,
         Node* Parent) : Node(Parent), Element(ElementSet) {}

 operator T&() {return Element;}
};

template <class T>
class setIterator
{
 public:

  Node* _node; 

  setIterator() : _node(0) {}

  setIterator(Node* in) : _node(in) {}

  setIterator(const setIterator<T>& i) : _node(i._node) {}

  T& operator*() const
  {
   return ((setNode<T>*)_node)->Element;
  }

  T* operator->() const
  {
   return &((setNode<T>*)_node)->Element;
  }

  T* operator&() const
  {
   return &((setNode<T>*)_node)->Element;
  }

  setIterator<T>& operator++()
  {_node = NextItem(_node); return *this;}

  setIterator<T> operator++(int)
  {setIterator<T> save = *this; ++*this ;return save;}

  setIterator<T>& operator+=(unsigned long increment)
  {for (unsigned long i=0; i<increment; i++) ++*this; return *this;}

  setIterator<T> operator+(unsigned long increment) const
  {
   setIterator<T> result(*this);
   for (unsigned long i=0; i<increment; i++) ++result;
   return result;
  }

  setIterator<T>& operator--()
  {_node = PreviousItem(_node); return *this;}

  setIterator<T> operator--(int)
  {setIterator<T> save = *this; --*this ;return save;}

  setIterator<T>& operator-=(unsigned long decrement)
  {for (unsigned long i=0; i<decrement; i++) --*this; return *this;}

  setIterator<T> operator-(unsigned long decrement) const
  {
   setIterator<T> result(*this);
   for (unsigned long i=0; i<decrement; i++) --result;
   return result;
  }

  bool operator==(const setIterator<T>& y) const {return _node == y._node;}

  bool operator!=(const setIterator<T>& y) const {return _node != y._node;}

  const T& operator[](long i) const {return i>=0 ? *(*this + i) : *(*this - -i);}  

  long operator-(setIterator<T> iter) const
  {
   long result=0;
   while (iter++ != *this) {result++;}
   return result;
  }

  bool IsHeader() const {return _node->IsHeader();}
};

template <class T>
class constSetIterator
{
 public:

  const Node* _node; 

  constSetIterator() : _node(0) {}

  constSetIterator(const Node* in) : _node(in) {}

  constSetIterator(const constSetIterator<T>& i) : _node(i._node) {}

  constSetIterator(const setIterator<T>& i) : _node(i._node) {}

  const T& operator*() const
  {
   return ((setNode<T>*)_node)->Element;
  }

  const T* operator->() const
  {
   return &((setNode<T>*)_node)->Element;
  }

  const T* operator&() const
  {
   return &((setNode<T>*)_node)->Element;
  }

  constSetIterator<T>& operator++()
  {_node = NextItem((Node*)_node); return *this;}

  constSetIterator<T> operator++(int)
  {constSetIterator<T> save = *this; ++*this ;return save;}

  constSetIterator<T>& operator+=(unsigned long increment)
  {for (unsigned long i=0; i<increment; i++) ++*this; return *this;}

  constSetIterator<T> operator+(unsigned long increment) const
  {
   constSetIterator<T> result(*this);
   for (unsigned long i=0; i<increment; i++) ++result;
   return result;
  }

  constSetIterator<T>& operator--()
  {_node = PreviousItem((Node*)_node); return *this;}

  constSetIterator<T> operator--(int)
  {setIterator save = *this; --*this ;return save;}

  constSetIterator<T>& operator-=(unsigned long decrement)
  {for (unsigned long i=0; i<decrement; i++) --*this; return *this;}

  constSetIterator<T> operator-(unsigned long decrement) const
  {
   constSetIterator<T> result(*this);
   for (unsigned long i=0; i<decrement; i++) --result;
   return result;
  }

  bool operator==(const constSetIterator<T>& y) const {return _node == y._node;}

  bool operator!=(const constSetIterator<T>& y) const {return _node != y._node;}

  const T& operator[](long i) const {return i>=0 ? *(*this + i) : *(*this - -i);}  

  long operator-(constSetIterator<T> iter) const
  {
   long result=0;
   while (iter++ != *this) {result++;}
   return result;
  }

  bool IsHeader() const {return _node->IsHeader;}
};

template <class T>
class set
{
 public:

  typedef int (*keyCompare)(const T&,const T&);

 protected:

  Node Header;
  keyCompare Compare;

 public:

  // *** iterators ***

  typedef setIterator<T> iterator;

  typedef constSetIterator<T> const_iterator;

  // *** constructors, destructor, operators ***

  set(keyCompare C=compare) : Compare(C) {}

  set(const set<T>& copy) : Compare(copy.Compare)
  {
   Copy((setNode<T>*)copy.Header.Parent);
  }

  set(const set& A, const set& B, unsigned long operation)
  {
   Compare = A.Compare;

   const_iterator first1 = A.begin();
   const_iterator last1  = A.end();
   const_iterator first2 = B.begin();
   const_iterator last2  = B.end();

   switch (operation)
    {
     case setOperation::Union:
      {
       while (first1 != last1 && first2 != last2)
        {
         int order = Compare(*first1,*first2);
 
         if (order < 0)
          {
           insert(*first1);
           ++first1;
          }

         else if (order > 0)
          {
           insert(*first2);
           ++first2;
          }

         else
          {
           insert(*first1);
           ++first1; ++first2;
          }
        }

       while (first1 != last1)
        {
         insert(*first1);
         first1++;
        }

       while (first2 != last2)
        {
         insert(*first2);
         first2++;
        }
      }
     break;

     case setOperation::Intersection:
      {
       while (first1 != last1 && first2 != last2)
        {
         int order = Compare(*first1,*first2);

         if (order < 0)
          ++first1;

         else if (order > 0)
          ++first2;

         else
          {
           insert(*first1);
           ++first1; ++first2;
          }
        }
      }
     break;

     case setOperation::SymmetricDifference:
      {
       while (first1 != last1 && first2 != last2)
        {
         int order = Compare(*first1,*first2);

         if (order < 0)
          {
           insert(*first1);
           ++first1;
          }
 
         else if (order > 0)
          {
           insert(*first2);
           ++first2;
          }

         else
          {++first1; ++first2;}
        }

       while (first1 != last1)
        {
         insert(*first1);
         ++first1;
        }

       while (first2 != last2)
        {
         insert(*first2);
         ++first2;
        }
      }
      break;

     case setOperation::Difference:
      {
       while (first1 != last1 && first2 != last2)
        {
         int order = Compare(*first1,*first2);

         if (order < 0)
          {
           insert(*first1);
           ++first1;
          }

         else if (order > 0)
          {
           insert(*first1);
           ++first1; ++first2;
          }

         else
          {++first1; ++first2;}
        }

       while (first1 != last1)
        {
         insert(*first1);
         ++first1;
        }
      }
      break;

     default:
      throw InvalidSetOperationException();
    }
  }

  template<class I>
  set(I first,I last,keyCompare C=compare)
  {
   Compare = C;
   while (first != last) insert(*first++);
  }

  ~set()
  {
   Destroy((setNode<T>*)Header.Parent);
  }

  set<T>& operator=(const set<T>& copy)
  {
   erase();
   Compare = copy.Compare;
   Copy((setNode<T>*)copy.Header.Parent);
   return *this;
  }

  unsigned long length() const {return Count(Header.Parent);}

  operator keyCompare() const {return Compare;}

  set<T>& operator<<(const T& Element) {insert(Element); return *this;}

  set<T>& operator>>(const T& Element) {erase(Element); return *this;}

  // *** methods ***

  iterator begin() {return Header.Left;}

  iterator end() {return &Header;}

  const_iterator begin() const {return Header.Left;}

  const_iterator end() const {return &Header;}

  iterator insert(const T& Element)
  {
   Node* RootNode = Header.Parent;

   if (RootNode == 0)
    {
     RootNode = new setNode<T>(Element,&Header);
     Header.Left = RootNode;
     Header.Right = RootNode;
     Header.Parent = RootNode;
     return RootNode;
    }

   else
    {
     for (; ; )
      {
       int Result = Compare(Element,((setNode<T>*)RootNode)->Element);

       if (Result == 0)
        throw EntryAlreadyExistsException();
 
       else if (Result < 0)
        {
         if (RootNode->Left != 0)
          RootNode = RootNode->Left;
         else
          {
           Node* newNode = new setNode<T>(Element,RootNode);
           RootNode->Left = newNode;
           AdjustAdd(newNode);
           return newNode;
          }
        }

       else
        {
         if (RootNode->Right != 0)
          RootNode = RootNode->Right;
         else
          {
           Node* newNode = new setNode<T>(Element,RootNode);
           RootNode->Right = newNode;
           AdjustAdd(newNode);
           return newNode;
          }
        }
      }
    }
  } 

  void erase(const T& Element)
  {
   Node* RootNode = Header.Parent;

   for (; ; )
    {
     if (RootNode == 0) throw EntryNotFoundException();

     int Result = Compare(Element,((setNode<T>*)RootNode)->Element);

     if (Result < 0)
      RootNode = RootNode->Left;

     else if (Result > 0)
      RootNode = RootNode->Right;

     else // Item is found
      {
       if (RootNode->Left != 0 && RootNode->Right != 0)
        {
         Node* Replace = RootNode->Left;
         while (Replace->Right != 0) Replace = Replace->Right;
         SwapNodes(RootNode, Replace);
        }

       Node* Parent = RootNode->Parent;

       unsigned long From = (Parent->Left == RootNode) ? Direction::FromLeft : Direction::FromRight;
 
       if (RootNode->Left == 0)
        {
         if (Parent == &Header)
          Header.Parent = RootNode->Right;
         else if (From == Direction::FromLeft)
          Parent->Left = RootNode->Right;
         else
          Parent->Right = RootNode->Right;

         if (RootNode->Right != 0) RootNode->Right->Parent = Parent;
        }
       else
        {
         if (Parent == &Header)
          Header.Parent = RootNode->Left;
         else if (From == Direction::FromLeft)
          Parent->Left = RootNode->Left;
         else
          Parent->Right = RootNode->Left;

         if (RootNode->Left != 0) RootNode->Left->Parent = Parent;
        }

       AdjustRemove(Parent, From);
       delete (setNode<T>*)RootNode;
       break;
      }
    }
  }

  void erase(iterator i)
  {
   Node* RootNode = i._node;

   if (RootNode->IsHeader()) throw IsHeaderException();

   if (RootNode->Left != 0 && RootNode->Right != 0)
    {
     Node* Replace = RootNode->Left;
     while (Replace->Right != 0) Replace = Replace->Right;
     SwapNodes(RootNode, Replace);
    }

   Node* Parent = RootNode->Parent;

   unsigned long From = (Parent->Left == RootNode) ? Direction::FromLeft : Direction::FromRight;

   if (RootNode->Left == 0)
    {
     if (Parent == &Header)
      Header.Parent = RootNode->Right;
     else if (From == Direction::FromLeft)
      Parent->Left = RootNode->Right;
     else
      Parent->Right = RootNode->Right;

     if (RootNode->Right != 0) RootNode->Right->Parent = Parent;
    }
   else
    {
     if (Parent == &Header)
      Header.Parent = RootNode->Left;
     else if (From == Direction::FromLeft)
      Parent->Left = RootNode->Left;
     else
      Parent->Right = RootNode->Left;

     if (RootNode->Left != 0) RootNode->Left->Parent = Parent;
    }

   AdjustRemove(Parent, From);
   delete (setNode<T>*)RootNode;
  }

  bool operator[](const T& Element) const {return exists(Element);}

  bool exists(const T& Element) const
  {
   if (!Header.Parent)
    return false;
   else
    {
     const Node* SearchNode = Header.Parent;

     do
      {
       int Result = Compare(Element,((setNode<T>*)SearchNode)->Element);

       if (Result < 0) SearchNode = SearchNode->Left;

       else if (Result > 0) SearchNode = SearchNode->Right;

       else break;

      } while (SearchNode);

     return SearchNode != 0;
    }
  }

  iterator find(const T& Element) const
  {
   if (!Header.Parent)
     throw EntryNotFoundException();
   else
    {
     const Node* SearchNode = Header.Parent;

     do
      {
       int Result = Compare(Element,((setNode<T>*)SearchNode)->Element);

       if (Result < 0) SearchNode = SearchNode->Left;

       else if (Result > 0) SearchNode = SearchNode->Right;

       else break;

      } while (SearchNode);

      if (SearchNode == 0) throw EntryNotFoundException();

     return (Node*)SearchNode;
    } 
  }      

  void erase()
  {
   Destroy((setNode<T>*)Header.Parent);
   Header.Left = &Header;
   Header.Right = &Header;
   Header.Parent = 0;
  }

  iterator after(const T& Element) const
  {
   const Node* y = &Header;
   const Node* x = Header.Parent;
   
   while (x != 0) 
    if (Compare(Element,((setNode<T>*)x)->Element)<0)
     {y=x; x=x->Left;}
    else
     x=x->Right;
   
   return (Node*)y;
  }

  iterator afterEquals(const T& Element) const
  {
   const Node* y = &Header;
   const Node* x = Header.Parent;
   
   while (x != 0) 
    {
     int c = Compare(Element,((setNode<T>*)x)->Element);
     if (c == 0)
      {y=x; break;}  
     else if (c<0)
      {y=x; x=x->Left;}
     else
      x=x->Right;
    }
   
   return (Node*)y;
  }

  iterator before(const T& Element) const
  {
   const Node* y = &Header;
   const Node* x = Header.Parent;
   
   while (x != 0) 
    if (Compare(Element,((setNode<T>*)x)->Element)<=0)
     {x=x->Left;}
    else
     {y=x; x=x->Right;}
   
   return (Node*)y;
  }

  iterator beforeEquals(const T& Element) const
  {
   const Node* y = &Header;
   const Node* x = Header.Parent;
   
   while (x != 0) 
    {
     int c = Compare(Element,((setNode<T>*)x)->Element);
     if (c == 0)
      {y = x; break;}
     else if (c<0)
      x=x->Left;
     else
      {y=x; x=x->Right;}
    }
   
   return (Node*)y;
  }

  iterator last() {return Header.Right;}

  const_iterator last() const {return Header.Right;}

  unsigned long depth() const {return Depth(Header.Parent);}

 protected:

  void Copy(setNode<T>* Clone)
  {
   if (!Header.Parent) erase();
   if (Clone)
    {
     Copy((setNode<T>*&)Header.Parent,Clone,&Header);
     Header.Left = GetFirst();
     Header.Right = GetLast();
    }
  }

  void Copy(setNode<T>*& RootNode,
            setNode<T>* n,
            const Node* Parent)
  {
   RootNode = new setNode<T>(n->Element,(Node*)Parent);
   RootNode->Balance = n->Balance;

   if (n->Left)
     Copy((setNode<T>*&)RootNode->Left,(setNode<T>*)n->Left,RootNode);
   else RootNode->Left = 0;

   if (n->Right)
     Copy((setNode<T>*&)RootNode->Right,(setNode<T>*)n->Right,RootNode);
   else RootNode->Right = 0;
  }

  Node* GetFirst()
  {
   if (!Header.Parent)
    return &Header;

   else
    {
     Node* SearchNode = Header.Parent;
     while (SearchNode->Left) SearchNode = SearchNode->Left;
     return SearchNode;
    } 
  }      

  Node* GetLast()
  {
   if (!Header.Parent)
    return &Header;

   else
    {
     Node* SearchNode = Header.Parent;
     while (SearchNode->Right) SearchNode = SearchNode->Right;
     return SearchNode;
    } 
  }      

  void Destroy(setNode<T>* RootNode)
  {
    if (RootNode)
    { 
     if (RootNode->Left)
      Destroy((setNode<T>*)RootNode->Left); 

     if (RootNode->Right)
      Destroy((setNode<T>*)RootNode->Right);

     delete RootNode;
    }
  }
};

template<class T>
inline set<T> operator|(const set<T>& a,const set<T>& b)
{set<T> r(a,b,setOperation::Union); return r;}

template<class T>
inline set<T> operator&(const set<T>& a,const set<T>& b)
{set<T> r(a,b,setOperation::Intersection); return r;}

template<class T>
inline set<T> operator^(const set<T>& a,const set<T>& b)
{set<T> r(a,b,setOperation::SymmetricDifference); return r;}

template<class T>
inline set<T> operator-(const set<T>& a,const set<T>& b)
{set<T> r(a,b,setOperation::Difference); return r;}

template<class T>
inline bool operator==(const set<T>& a,const set<T>& b)
{
 set<T>::const_iterator first1 = a.begin();
 set<T>::const_iterator last1  = a.end();
 set<T>::const_iterator first2 = b.begin();
 set<T>::const_iterator last2  = b.end();

 bool equals=true;

 set<T>::keyCompare c = a;

 while (first1 != last1 && first2 != last2)
  {
   int order = c(*first1,*first2);
   if (order < 0)
    {equals=false; break;}
   else if (order > 0)
    {equals=false; break;}
   else
    {++first1; ++first2;}
  }

 if (equals)
  {
   if (first1 != last1) equals = false;
   if (first2 != last2) equals = false;
  }

 return equals;
}

template<class T>
inline bool operator!=(const set<T>& a,const set<T>& b) {return !(a == b);}

template<class T>
inline bool operator<=(const set<T>& a,const set<T>& b)
{
 set<T>::const_iterator first1 = a.begin();
 set<T>::const_iterator last1  = a.end();
 set<T>::const_iterator first2 = b.begin();
 set<T>::const_iterator last2  = b.end();

 set<T>::keyCompare c = a;

 bool subset=true;

 while (first1 != last1 && first2 != last2)
  {
   int order = c(*first1,*first2);
 
   if (order < 0)
    {subset=false; break;}

   else if (order > 0)
    ++first2;
 
   else
    {++first1; ++first2;}
  }

 if (subset) if (first1 != last1) subset = false;

 return subset;
}

template<class T>
inline int compare(const set<T>& a,const set<T>& b)
{
 set<T>::const_iterator first1 = a.begin();
 set<T>::const_iterator last1  = a.end();
 set<T>::const_iterator first2 = b.begin();
 set<T>::const_iterator last2  = b.end();

 set<T>::keyCompare c = a;

 while (first1 != last1 && first2 != last2)
  {
   int order = c(*first1,*first2);
   if (order < 0)
    return -1;
   else if (order > 0)
    return 1;
   else
    {++first1; ++first2;}
  }

 if (first1 != last1) return 1;
 if (first2 != last2) return -1;

 return 0;
}

template<class T>
std::ostream& operator<<(std::ostream& s,const set<T>& o)
{
 s << "{";
 set<T>::const_iterator e = o.end();
 set<T>::const_iterator l = e-1;
 for (set<T>::const_iterator i = o.begin(); i!=e; ++i)
  {s << *i; if (i!=l) s << ",";}
 s << "}";
 return s;
}

void main()
{
 try
  {
   set<double> s;

   //*** Build the Set

   for (int i=0; i<10; i++) s << i+.5;

   //*** Print the set using iterators

   std::cout << "{";

   set<double>::iterator last = s.last();

   for (set<double>::iterator x = s.begin(); x != s.end(); ++x)
    {
        std::cout << *x;
        if (x != last) std::cout << ",";
    }

   std::cout << "}\n";

   //*** Print the set using stream output operator

   std::cout << s << "\n";

   //*** Print the set using for each

   std::cout << "{";

   for each (double d in s)
    {
        std::cout << d;
        if (d != *last) std::cout << ",";
    }

   std::cout << "}\n";
  }
 catch (treeException) {std::cout << "A Tree Exception Occurred.\n";}
}

```

