+++
title = "AVL tree"
description = ""
date = 2019-06-06T16:23:46Z
aliases = []
[extra]
id = 13412
[taxonomies]
categories = ["Data Structures", "task"]
tags = []
languages = [
  "agda",
  "beed",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "go",
  "haskell",
  "java",
  "kotlin",
  "lua",
  "objeck",
  "phix",
  "python",
  "rust",
  "scala",
  "sidef",
  "simula",
  "tcl",
  "typescript",
]
+++

## Task

*Reference: [Wikipedia: AVL tree](https://en.wikipedia.org/wiki/AVL_tree)*



In computer science, an '''AVL tree''' is a self-balancing binary search tree. In an AVL tree, the heights of the two child subtrees of any node differ by at most one; at no time do they differ by more than one because rebalancing is done ensure this is the case. Lookup, insertion, and deletion all take O(log ''n'') time in both the average and worst cases, where n is the number of nodes in the tree prior to the operation. Insertions and deletions may require the tree to be rebalanced by one or more tree rotations.

AVL trees are often compared with [[Red_black_trees|red-black trees]] because they support the same set of operations and because red-black trees also take O(log ''n'') time for the basic operations.  Because AVL trees are more rigidly balanced, they are faster than red-black trees for lookup-intensive applications. Similar to red-black trees, AVL trees are height-balanced, but in general not weight-balanced nor μ-balanced; that is, sibling nodes can have hugely differing numbers of descendants.


;Task:
Implement an AVL tree in the language of choice, and provide at least basic operations.





## Agda

This implementation uses the type system to enforce the height invariants, though not the BST invariants

```agda

module Avl where

-- The Peano naturals
data Nat : Set where
 z : Nat
 s : Nat -> Nat

-- An AVL tree's type is indexed by a natural.
-- Avl N is the type of AVL trees of depth N. There arj 3 different
-- node constructors:
--   Left: The left subtree is one level deeper than the right
--   Balanced: The subtrees have the same depth
--   Right: The right Subtree is one level deeper than the left
-- Since the AVL invariant is that the depths of a node's subtrees
-- always differ by at most 1, this perfectly encodes the AVL depth invariant.
data Avl : Nat -> Set where
  Empty : Avl z
  Left : {X : Nat} -> Nat -> Avl (s X) -> Avl X -> Avl (s (s X))
  Balanced : {X : Nat} -> Nat -> Avl X -> Avl X -> Avl (s X)
  Right : {X : Nat} -> Nat -> Avl X -> Avl (s X) -> Avl (s (s X))

-- A wrapper type that hides the AVL tree invariant. This is the interface
-- exposed to the user.
data Tree : Set where
  avl : {N : Nat} -> Avl N -> Tree

-- Comparison result
data Ord : Set where
  Less : Ord
  Equal : Ord
  Greater : Ord

-- Comparison function
cmp : Nat -> Nat -> Ord
cmp z (s X) = Less
cmp z z = Equal
cmp (s X) z = Greater
cmp (s X) (s Y) = cmp X Y

-- Insertions can either leave the depth the same or
-- increase it by one. Encode this in the type.
data InsertResult : Nat -> Set where
  Same : {X : Nat} -> Avl X -> InsertResult X
  Bigger : {X : Nat} -> Avl (s X) -> InsertResult X

-- If the left subtree is 2 levels deeper than the right, rotate to the right.
-- balance-left X L R means X is the root, L is the left subtree and R is the right.
balance-left : {N : Nat} -> Nat -> Avl (s (s N)) -> Avl N -> InsertResult (s (s N))
balance-left X (Right Y A (Balanced Z B C)) D = Same (Balanced Z (Balanced X A B) (Balanced Y C D))
balance-left X (Right Y A (Left Z B C)) D = Same (Balanced Z (Balanced X A B) (Right Y C D))
balance-left X (Right Y A (Right Z B C)) D = Same (Balanced Z (Left X A B) (Balanced Y C D))
balance-left X (Left Y (Balanced Z A B) C) D = Same (Balanced Z (Balanced X A B) (Balanced Y C D))
balance-left X (Left Y (Left Z A B) C) D = Same (Balanced Z (Left X A B) (Balanced Y C D))
balance-left X (Left Y (Right Z A B) C) D = Same (Balanced Z (Right X A B) (Balanced Y C D))
balance-left X (Balanced Y (Balanced Z A B) C) D = Bigger (Right Z (Balanced X A B) (Left Y C D))
balance-left X (Balanced Y (Left Z A B) C) D =  Bigger (Right Z (Left X A B) (Left Y C D))
balance-left X (Balanced Y (Right Z A B) C) D = Bigger (Right Z (Right X A B) (Left Y C D))

-- Symmetric with balance-left
balance-right : {N : Nat} -> Nat -> Avl N -> Avl (s (s N)) -> InsertResult (s (s N))
balance-right X A (Left Y (Left Z B C) D) = Same (Balanced Z (Balanced X A B) (Right Y C D))
balance-right X A (Left Y (Balanced Z B C) D) = Same(Balanced Z (Balanced  X A B) (Balanced Y C D))
balance-right X A (Left Y (Right Z B C) D) = Same(Balanced Z (Left X A B) (Balanced Y C D))
balance-right X A (Balanced Z B (Left Y C D)) = Bigger(Left Z (Right X A B) (Left Y C D))
balance-right X A (Balanced Z B (Balanced Y C D)) = Bigger (Left Z (Right X A B) (Balanced Y C D))
balance-right X A (Balanced Z B (Right Y C D)) = Bigger (Left Z (Right X A B) (Right Y C D))
balance-right X A (Right Z B (Left Y C D)) = Same (Balanced Z (Balanced X A B) (Left Y C D))
balance-right X A (Right Z B (Balanced Y C D)) = Same (Balanced Z (Balanced X A B) (Balanced Y C D))
balance-right X A (Right Z B (Right Y C D)) =  Same (Balanced Z (Balanced X A B) (Right Y C D))

-- insert' T N does all the work of inserting the element N into the tree T.
insert' : {N : Nat} -> Avl N -> Nat -> InsertResult N
insert' Empty N = Bigger (Balanced N Empty Empty)
insert' (Left Y L R) X with cmp X Y
insert' (Left Y L R) X | Less with insert' L X
insert' (Left Y L R) X | Less | Same L' = Same (Left Y L' R)
insert' (Left Y L R) X | Less | Bigger L' = balance-left Y L' R
insert' (Left Y L R) X | Equal = Same (Left Y L R)
insert' (Left Y L R) X | Greater with insert' R X
insert' (Left Y L R) X | Greater | Same R' = Same (Left Y L R')
insert' (Left Y L R) X | Greater | Bigger R' = Same (Balanced Y L R')
insert' (Balanced Y L R) X with cmp X Y
insert' (Balanced Y L R) X | Less with insert' L X
insert' (Balanced Y L R) X | Less | Same L'  = Same (Balanced Y L' R)
insert' (Balanced Y L R) X | Less | Bigger L' = Bigger (Left Y L' R)
insert' (Balanced Y L R) X | Equal = Same (Balanced Y L R)
insert' (Balanced Y L R) X | Greater with insert' R X
insert' (Balanced Y L R) X | Greater | Same R' = Same (Balanced Y L R')
insert' (Balanced Y L R) X | Greater | Bigger R' = Bigger (Right Y L R')
insert' (Right Y L R) X with cmp X Y
insert' (Right Y L R) X | Less with insert' L X
insert' (Right Y L R) X | Less | Same L' = Same (Right Y L' R)
insert' (Right Y L R) X | Less | Bigger L' = Same (Balanced Y L' R)
insert' (Right Y L R) X | Equal = Same (Right Y L R)
insert' (Right Y L R) X | Greater with insert' R X
insert' (Right Y L R) X | Greater | Same R' = Same (Right Y L R')
insert' (Right Y L R) X | Greater | Bigger R' = balance-right Y L R'

-- Wrapper around insert' to use the depth-agnostic type Tree.
insert : Tree -> Nat  -> Tree
insert (avl T) X with insert' T X
... | Same T' = avl T'
... | Bigger T' = avl T'

```



## beed



```cpp

eenioonneraashon staat
{
    heder,
    balansd,
    lepht_hi,
    riit_hi
}

eenioonneraashon direcshon
{
  phronn_lepht,
  phronn_riit
}

eenioonneraashon booleean
{
 troo,
 phals
}

clahs nohd<t>
{
    nohd<t> lepht;
    nohd<t> riit;
    nohd<t> pairent;
    staat balans;
    t daata;

    nohd()
    {
       lepht = this;
       riit = this;
       balans = heder;
    }

    nohd(nohd<t> p, t daata_in)
    {
      pairent = p;
      balans = balansd;
      daata = daata_in;
    }

    is_heder(booleean anser)
    {
        balans.eecuuols(heder,anser);
    }

    eecuuols(nohd<t> other, booleean anser)
    {
        _nohd.nohd.eecuuols(other.nohd._nohd, anser);
    }

    nnoou_necst()
    {
        booleean is_heder = phals;
        nohd.is_heder(is_heder);
        iph (is_heder)
        {
            nohd = nohd.lepht;
            reeturn;
        }

        booleean riit_is_null = phals;
        nohd.riit.is_nul(riit_is_null);
        iph (!riit_is_null)
        {
           nohd = nohd.riit;

           booleean lepht_is_nul = phals;
           nohd.lepht.is_nul(lepht_is_nul);
           uuhiil (!lepht_is_null)
           {
               nohd = nohd.lepht;
               nohd.lepht.is_nul(lepht_is_nul);
           }
        }
        els
        {
            nohd<t> uui = nohd.pairent;
            booleean uui_is_heder = phals;
            uui.is_heder(uui_is_heder);
            iph (uui_is_heder)
            {
                nohd = uui;
                reeturn;
            }
            booleean nohd_is_uui_riit = phals;
            nohd.eecuuols(uui.riit,nohd_is_uui_riit);
            uuhiil (nohd_is_uui_riit)
            {
                nohd = uui;
                uui = uui.pairent;
                nohd.eecuuols(uui.riit,nohd_is_uui_riit);
            }
            nohd = uui;
        }

    }

    rohtaat_lepht(nohd<t> root)
    {
        nohd<t> pairent = root.pairent;
        nohd<t> e = root.riit;
        root.pairent = e;
        e.pairent = pairent;
        booleean e_lepht_is_nul = phals;
        nohd<t> e_lepht = e.lepht;
        e_lepht.is_nul(e_lepht_is_nul);
        iph (!e_lepht_is_nul) {e.lepht.pairent = root;}
        root.riit = e.lepht;
        e.lepht = root;
        root = e;
    }

    rohtaat_riit(nohd<t> root)
    {
        nohd<t> pairent = root.pairent;
        nohd<t> e = root.lepht;
        root.pairent = e;
        e.pairent = pairent;
        booleean e_riit_is_nul = phals;
        nohd<t> e_riit =  e.riit;
        e_riit.is_nul(e_lepht_is_nul);
        iph (!e_lepht_is_nul) {e.riit.pairent = root;}
        root.lepht = e.riit;
        e.riit = root;
        root = e;
    }

    balans_lepht(nohd<t> root)
    {
        nohd<t> lepht = root.lepht;
        select (lepht.balans)
        {
            caas lepht_hi
            {
                root.balans = balansd;
                lepht.balans = balansd;
                rohtaat_riit(root);
            }

            caas riit_hi
            {
                nohd<t> subriit = lepht.riit;

                select(subriit.balans)
                {
                    caas balansd
                    {
                        root.balans = balansd;
                        lepht.balans = balansd;
                    }

                    caas riit_hi
                    {
                        root.balans = balansd;
                        lepht.balans = lepht_hi;
                    }

                    caas lepht_hi
                    {
                        root.balans = riit_hi;
                        lepht.balans = balansd;
                    }
                }
                subriit.balans = balansd;
                rohtaat_lepht(lepht);
                root.lepht = lepht;
                rohtaat_riit(root);
            }

            caas balansd
            {
                root.balans = lepht_hi;
                lepht.balans = riit_hi;
                rohtaat_riit(root);
            }
        }
    }

    balans_riit(nohd<t> root)
    {
        nohd<t> riit = root.riit;
        select (riit.balans)
        {
            caas riit_hi
            {
                root.balans = balansd;
                riit.balans = balansd;
                rohtaat_lepht(root);
            }

           caas lepht_hi
           {
               nohd<t> sublepht = riit.lepht;

               select(sublepht.balans)
               {
                   caas balansd
                   {
                       root.balans = balansd;
                       riit.balans = balansd;
                   }

                   caas lepht_hi
                   {
                       root.balans = balansd;
                       riit.balans = riit_hi;
                   }

                   caas riit_hi
                   {
                       root.balans = lepht_hi;
                       riit.balans = balansd;
                   }
               }
               sublepht.balans = balansd;
               rohtaat_riit(riit);
               root.riit = riit;
               rohtaat_lepht(root);
           }

           caas balansd
           {
               root.balans = riit_hi;
               riit.balans = lepht_hi;
               rohtaat_lepht(root);
           }
        }
    }

    balans_tree(nohd<t> root, direcshon phronn)
    {
        booleean torler = troo;

        uuhiil (torler)
        {
            nohd<t> pairent = root.pairent;

            direcshon necst_phronn = phronn_lepht;

            booleean is_pairent_lepht = phals;
            pairent.lepht.eecuuols(root,is_pairent_lepht);
            iph (!is_pairent_lepht)
            {
                necst_phronn = phronn_riit;
            }

            booleean is_phronn_lepht = phals;
            phronn.eecuuols(phronn_lepht, is_phronn_lepht);
            iph (is_phronn_lepht)
            {
                select (root.balans)
                {
                    caas lepht_hi
                    {
                        booleean pheder = phals;
                        pairent.is_heder(pheder);
                        iph (pheder)
                        {
                            balans_lepht(pairent.pairent);
                        }
                        els
                        {
                            booleean lepht_is_root = phals;
                            pairent.lepht.eecuuols(root,lepht_is_root);
                            iph (lepht_is_root)
                            {
                                balans_lepht(pairent.lepht);
                            }
                            els
                            {
                                balans_lepht = pairent.riit;
                                torler = phals;
                            }
                        }
                    }

                    caas balansd
                    {
                        root.balans = lepht_hi;
                        torler = troo;
                    }

                    caas riit_hi
                    {
                        root.balans = balansd;
                        torler = phals;
                    }
                }
            }
            els
            {
               select (root.balans)
               {
                   caas lepht_hi
                   {
                       root.balans = balansd;
                       torler = phals;
                   }

                   caas balansd
                   {
                       root.balans = riit_hi;
                       torler = troo;
                   }

                   caas riit_hi
                   {
                        booleean heder = phals;
                        pairent.is_heder(heder);
                        iph (heder)
                        {
                            balans_riit(pairent.pairent);
                        }
                        els
                        {
                            booleean lpairent = phals;
                            pairent.lepht.eecuuols(root,lpairent);
                            iph (lpairent)
                            {
                                balans_riit(pairent.lepht);
                            }
                            els
                            {
                                balans_riit(pairent.riit);
                                torler = phals;
                            }
                        }
                     }
                 }
             }

             iph (torler)
             {
                 booleean heder_up = phals;
                 pairent.is_heder(heder_up);
                 iph (heder_up)
                 {
                   torler = phals;
                  }
                  els
                  {
                      root = pairent;
                      phronn = necst_phronn;
                  }
             }
         }
    }

    balans_tree_reennoou(nohd<t> root, direcshon phronn)
    {
         booleean shorter = troo;

         uuhiil (shorter)
         {
            nohd<t> pairent = root.pairent;

            direcshon necst_phronn = phronn_lepht;

            booleean is_pairent_lepht = phals;
            pairent.lepht.eecuuols(root,is_pairent_lepht);
            iph (!is_pairent_lepht)
            {
                necst_phronn = phronn_riit;
            }

            booleean is_phronn_lepht = phals;
            phronn.eecuuols(phronn_lepht, is_phronn_lepht);
            iph (is_phronn_lepht)
            {
                select (root.balans)
                {
                    caas lepht_hi
                    {
                        root.balans = staat.balansd;
                        shorter = troo;
                    }


                    caas balansd
                    {
                        root.balans = staat.riit_hi;
                        shorter = phals;
                    }

                    caas riit_hi
                    {
                        booleean riit_is_balansd = phals;
                        root.riit.balans.eecuuols(balansd,riit_is_balansd);

                        iph (riit_is_balansd)
                        {
                            shorter = phals;
                        }
                        els
                        {
                            shorter = troo;
                        }

                        booleean heder = phals;
                        pairent.is_heder(heder);
                        iph (heder)
                        {
                            balans_riit(pairent.pairent);
                        }
                        els
                        {
                            booleean lpairent = phals;
                            pairent.lepht.eecuuols(root,lpairent);
                            iph (lpairent)
                            {
                                balans_riit(pairent.lepht);
                            }
                            els
                            {
                                balans_riit(pairent.riit);
                            }
                       }
                   }
                }
             }
             els
             {
                select (root.balans)
                {
                    caas riit_hi
                    {
                        root.balans = balansd;
                        shorter = troo;
                    }


                   caas balansd
                   {
                      root.balans = lepht_hi;
                      shorter = phals;
                   }


                   caas lepht_hi
                   {
                        booleean lepht_is_balansd = phals;
                        root.lepht.balans.eecuuols(balansd,lepht_is_balansd);
                        iph (lepht_is_balansd)
                        {
                            shorter = phals;
                        }
                        els
                        {
                            shorter = troo;
                        }

                        booleean is_heder = phals;
                        pairent.is_heder(is_heder);
                        iph (is_heder)
                        {
                            balans_lepht(pairent.pairent);
                        }
                        els
                        {
                            pairent.lepht.eecuuols(root,lpairent);
                            iph (lpairent)
                            {
                                balans_lepht(pairent.lepht);
                            }
                            els
                            {
                                balans_lepht(pairent.riit);
                            }
                       }
                   }
                }
            }

            iph (shorter)
            {
               booleean heder_up = phals;
               pairent.is_heder(heder_up);
               iph (heder_up)
               {
                  shorter = phals;
               }
               els
               {
                  root = pairent;
                  phronn = necst_phronn;
               }
           }
        }
    }
}



clahs entree_orlredee_ecsists_ecssepshon
{
   string naann;

    entree_orlredee_ecsists_ecssepshon(string naann_in)
    {
         naann = naann_in;
    }

    print()
    {
         string ouut(naan);
         string to_ad( orlredee ecsists);
         ouut.concat(to_ad);
         ouut.print();
    }
}


}

clahs set_entree<t>
{
    set_entree(nohd<t> n)
    {
        _nohd = n;
    }

    ualioo(t _daata)
    {
      _daata = _nohd.daata;
     }

    is_heder(booleean b) { _nohd.nohd.is_heder(b); }


    nohd<t> _nohd;
}

clahs set<t>
{
    nohd<t> heder;

    set()
    {
       heder();
    }

    ad(t daata)
    {
       string out(in ad);
       out.println();

        booleean root_is_nul = phals;
        heder.pairent.is_nul(root_is_nul);
        iph (root_is_nul)
        {
            nohd<t> n(heder,daata);
            heder.pairent = n;
            heder.lepht = heder.pairent;
            heder.riit = heder.pairent;
        }
        els
        {
            nohd<t> root = heder.pairent;

            reepeet
            {
                booleean is_les = phals;
                string outb(connpairing entrees);
                outb.println();
                daata.les(root.daata, is_les);
                iph (is_les)
                {
                    booleean root_lepht_is_nul = phals;
                    root.nohd.lepht.is_nul(root_lepht_is_nul);
                    iph (!root_lepht_is_nul)
                    {
                        root.nohd = root.lepht;
                    }
                    els
                    {
                        nohd<t> nioo_nohd(root,daata);
                        root.lepht = nioo_nohd;
                        booleean is_phurst = phals;
                        heder.lepht.eecuuols(root,is_phurst);
                        iph (is_phurst)
                        {
                            heder.lepht = nioo_nohd;
                        }
                        direcshon dir(phronn_lepht);
                        balans_tree(root.nohd, dir);
                        reeturn;
                    }
               }
               els
               {
                    booleean is_graater = phals;
                    root.daata.les(daata, is_graater);
                    iph (is_graater)
                    {
                        booleean root_riit_is_nul = phals;
                        root.nohd.riit.is_nul(root_riit_is_nul);
                        iph (!root_riit_is_nul)
                        {
                            root = root.riit;
                        }
                        els
                        {
                            nohd<t> nioo(root,daata);
                            root.riit = nioo;
                            booleean is_lahst = phals;
                            heder.riit.eecuuols(root,is_lahst);
                            iph (is_lahst)
                            {
                                heder.riit = nioo_nohd;
                            }
                            direcshon dirb(phronn_riit);
                            balans_tree(root.nohd, dirb);
                            reeturn;
                        }
                    }
                    els // iiten orlredee ecsists
                    {
                        string s(entree orlredee ecsists);
                        entree_orlredee_ecsists_ecssepshon p(s);
                        throuu p;
                    }
               }
           }
       }
   }
}

```



## C


See [[AVL_tree/C|AVL tree/C]]


## C#


See [[AVL_tree/C_sharp]].


## C++

{{trans|D}}

```cpp

#include <algorithm>
#include <iostream>

/* AVL node */
template <class T>
class AVLnode {
public:
    T key;
    int balance;
    AVLnode *left, *right, *parent;

    AVLnode(T k, AVLnode *p) : key(k), balance(0), parent(p),
                        left(NULL), right(NULL) {}

    ~AVLnode() {
        delete left;
        delete right;
    }
};

/* AVL tree */
template <class T>
class AVLtree {
public:
    AVLtree(void);
    ~AVLtree(void);
    bool insert(T key);
    void deleteKey(const T key);
    void printBalance();

private:
    AVLnode<T> *root;

    AVLnode<T>* rotateLeft          ( AVLnode<T> *a );
    AVLnode<T>* rotateRight         ( AVLnode<T> *a );
    AVLnode<T>* rotateLeftThenRight ( AVLnode<T> *n );
    AVLnode<T>* rotateRightThenLeft ( AVLnode<T> *n );
    void rebalance                  ( AVLnode<T> *n );
    int height                      ( AVLnode<T> *n );
    void setBalance                 ( AVLnode<T> *n );
    void printBalance               ( AVLnode<T> *n );
    void clearNode                  ( AVLnode<T> *n );
};

/* AVL class definition */
template <class T>
void AVLtree<T>::rebalance(AVLnode<T> *n) {
    setBalance(n);

    if (n->balance == -2) {
        if (height(n->left->left) >= height(n->left->right))
            n = rotateRight(n);
        else
            n = rotateLeftThenRight(n);
    }
    else if (n->balance == 2) {
        if (height(n->right->right) >= height(n->right->left))
            n = rotateLeft(n);
        else
            n = rotateRightThenLeft(n);
    }

    if (n->parent != NULL) {
        rebalance(n->parent);
    }
    else {
        root = n;
    }
}

template <class T>
AVLnode<T>* AVLtree<T>::rotateLeft(AVLnode<T> *a) {
    AVLnode<T> *b = a->right;
    b->parent = a->parent;
    a->right = b->left;

    if (a->right != NULL)
        a->right->parent = a;

    b->left = a;
    a->parent = b;

    if (b->parent != NULL) {
        if (b->parent->right == a) {
            b->parent->right = b;
        }
        else {
            b->parent->left = b;
        }
    }

    setBalance(a);
    setBalance(b);
    return b;
}

template <class T>
AVLnode<T>* AVLtree<T>::rotateRight(AVLnode<T> *a) {
    AVLnode<T> *b = a->left;
    b->parent = a->parent;
    a->left = b->right;

    if (a->left != NULL)
        a->left->parent = a;

    b->right = a;
    a->parent = b;

    if (b->parent != NULL) {
        if (b->parent->right == a) {
            b->parent->right = b;
        }
        else {
            b->parent->left = b;
        }
    }

    setBalance(a);
    setBalance(b);
    return b;
}

template <class T>
AVLnode<T>* AVLtree<T>::rotateLeftThenRight(AVLnode<T> *n) {
    n->left = rotateLeft(n->left);
    return rotateRight(n);
}

template <class T>
AVLnode<T>* AVLtree<T>::rotateRightThenLeft(AVLnode<T> *n) {
    n->right = rotateRight(n->right);
    return rotateLeft(n);
}

template <class T>
int AVLtree<T>::height(AVLnode<T> *n) {
    if (n == NULL)
        return -1;
    return 1 + std::max(height(n->left), height(n->right));
}

template <class T>
void AVLtree<T>::setBalance(AVLnode<T> *n) {
    n->balance = height(n->right) - height(n->left);
}

template <class T>
void AVLtree<T>::printBalance(AVLnode<T> *n) {
    if (n != NULL) {
        printBalance(n->left);
        std::cout << n->balance << " ";
        printBalance(n->right);
    }
}

template <class T>
AVLtree<T>::AVLtree(void) : root(NULL) {}

template <class T>
AVLtree<T>::~AVLtree(void) {
    delete root;
}

template <class T>
bool AVLtree<T>::insert(T key) {
    if (root == NULL) {
        root = new AVLnode<T>(key, NULL);
    }
    else {
        AVLnode<T>
            *n = root,
            *parent;

        while (true) {
            if (n->key == key)
                return false;

            parent = n;

            bool goLeft = n->key > key;
            n = goLeft ? n->left : n->right;

            if (n == NULL) {
                if (goLeft) {
                    parent->left = new AVLnode<T>(key, parent);
                }
                else {
                    parent->right = new AVLnode<T>(key, parent);
                }

                rebalance(parent);
                break;
            }
        }
    }

    return true;
}

template <class T>
void AVLtree<T>::deleteKey(const T delKey) {
    if (root == NULL)
        return;

    AVLnode<T>
        *n       = root,
        *parent  = root,
        *delNode = NULL,
        *child   = root;

    while (child != NULL) {
        parent = n;
        n = child;
        child = delKey >= n->key ? n->right : n->left;
        if (delKey == n->key)
            delNode = n;
    }

    if (delNode != NULL) {
        delNode->key = n->key;

        child = n->left != NULL ? n->left : n->right;

        if (root->key == delKey) {
            root = child;
        }
        else {
            if (parent->left == n) {
                parent->left = child;
            }
            else {
                parent->right = child;
            }

            rebalance(parent);
        }
    }
}

template <class T>
void AVLtree<T>::printBalance() {
    printBalance(root);
    std::cout << std::endl;
}

int main(void)
{
    AVLtree<int> t;

    std::cout << "Inserting integer values 1 to 10" << std::endl;
    for (int i = 1; i <= 10; ++i)
        t.insert(i);

    std::cout << "Printing balance: ";
    t.printBalance();
}

```

{{out}}

```txt

Inserting integer values 1 to 10
Printing balance: 0 0 0 1 0 0 0 0 1 0

```



###  More elaborate version

See [[AVL_tree/C++]]


###  Managed C++

See [[AVL_tree/Managed_C++]]

```



## Common Lisp

Provided is an imperative implementation of an AVL tree with a similar interface and documentation to HASH-TABLE.

```lisp
(defpackage :avl-tree
  (:use :cl)
  (:export
   :avl-tree
   :make-avl-tree
   :avl-tree-count
   :avl-tree-p
   :avl-tree-key<=
   :gettree
   :remtree
   :clrtree
   :dfs-maptree
   :bfs-maptree))

(in-package :avl-tree)

(defstruct %tree
  key
  value
  (height 0 :type fixnum)
  left
  right)

(defstruct (avl-tree (:constructor %make-avl-tree))
  key<=
  tree
  (count 0 :type fixnum))

(defun make-avl-tree (key<=)
  "Create a new AVL tree using the given comparison function KEY<=
for emplacing keys into the tree."
  (%make-avl-tree :key<= key<=))

(declaim (inline
          recalc-height
          height balance
          swap-kv
          right-right-rotate
          right-left-rotate
          left-right-rotate
          left-left-rotate
          rotate))

(defun recalc-height (tree)
  "Calculate the new height of the tree from the heights of the children."
  (when tree
    (setf (%tree-height tree)
          (1+ (the fixnum (max (height (%tree-right tree))
                               (height (%tree-left tree))))))))

(declaim (ftype (function (t) fixnum) height balance))
(defun height (tree)
  (if tree (%tree-height tree) 0))

(defun balance (tree)
  (if tree
      (- (height (%tree-right tree))
         (height (%tree-left tree)))
      0))

(defmacro swap (place-a place-b)
  "Swap the values of two places."
  (let ((tmp (gensym)))
    `(let ((,tmp ,place-a))
       (setf ,place-a ,place-b)
       (setf ,place-b ,tmp))))

(defun swap-kv (tree-a tree-b)
  "Swap the keys and values of two trees."
  (swap (%tree-value tree-a) (%tree-value tree-b))
  (swap (%tree-key tree-a) (%tree-key tree-b)))

;; We should really use gensyms for the variables in here.
(defmacro slash-rotate (tree right left)
  "Rotate nodes in a slash `/` imbalance."
  `(let* ((a ,tree)
          (b (,right a))
          (c (,right b))
          (a-left (,left a))
          (b-left (,left b)))
     (setf (,right a) c)
     (setf (,left a) b)
     (setf (,left b) a-left)
     (setf (,right b) b-left)
     (swap-kv a b)
     (recalc-height b)
     (recalc-height a)))

(defmacro angle-rotate (tree right left)
  "Rotate nodes in an angle bracket `<` imbalance."
  `(let* ((a ,tree)
          (b (,right a))
          (c (,left b))
          (a-left (,left a))
          (c-left (,left c))
          (c-right (,right c)))
     (setf (,left a) c)
     (setf (,left c) a-left)
     (setf (,right c) c-left)
     (setf (,left b) c-right)
     (swap-kv a c)
     (recalc-height c)
     (recalc-height b)
     (recalc-height a)))

(defun right-right-rotate (tree)
  (slash-rotate tree %tree-right %tree-left))

(defun left-left-rotate (tree)
  (slash-rotate tree %tree-left %tree-right))

(defun right-left-rotate (tree)
  (angle-rotate tree %tree-right %tree-left))

(defun left-right-rotate (tree)
  (angle-rotate tree %tree-left %tree-right))

(defun rotate (tree)
  (declare (type %tree tree))
  "Perform a rotation on the given TREE if it is imbalanced."
  (recalc-height tree)
  (with-slots (left right) tree
    (let ((balance (balance tree)))
      (cond ((< 1 balance) ;; Right imbalanced tree
             (if (<= 0 (balance right))
                 (right-right-rotate tree)
                 (right-left-rotate tree)))
            ((> -1 balance) ;; Left imbalanced tree
             (if (<= 0 (balance left))
                 (left-right-rotate tree)
                 (left-left-rotate tree)))))))

(defun gettree (key avl-tree &optional default)
  "Finds an entry in AVL-TREE whos key is KEY and returns the
associated value and T as multiple values, or returns DEFAULT and NIL
if there was no such entry. Entries can be added using SETF."
  (with-slots (key<= tree) avl-tree
    (labels
        ((rec (tree)
           (if tree
               (with-slots ((t-key key) left right value) tree
                 (if (funcall key<= t-key key)
                     (if (funcall key<= key t-key)
                         (values value t)
                         (rec right))
                     (rec left)))
               (values default nil))))
      (rec tree))))

(defun puttree (value key avl-tree)
  ;;(declare (optimize speed))
  (declare (type avl-tree avl-tree))
  "Emplace the the VALUE with the given KEY into the AVL-TREE, or
overwrite the value if the given key already exists."
  (let ((node (make-%tree :key key :value value)))
    (with-slots (key<= tree count) avl-tree
      (cond (tree
             (labels
                 ((rec (tree)
                    (with-slots ((t-key key) left right) tree
                      (if (funcall key<= t-key key)
                          (if (funcall key<= key t-key)
                              (setf (%tree-value tree) value)
                              (cond (right (rec right))
                                    (t (setf right node)
                                       (incf count))))
                          (cond (left (rec left))
                                (t (setf left node)
                                   (incf count))))
                      (rotate tree))))
               (rec tree)))
            (t (setf tree node)
               (incf count))))
    value))

(defun (setf gettree) (value key avl-tree &optional default)
  (declare (ignore default))
  (puttree value key avl-tree))

(defun remtree (key avl-tree)
  (declare (type avl-tree avl-tree))
  "Remove the entry in AVL-TREE associated with KEY. Return T if
there was such an entry, or NIL if not."
  (with-slots (key<= tree count) avl-tree
    (labels
        ((find-left (tree)
           (with-slots ((t-key key) left right) tree
             (if left
                 (find-left left)
                 tree)))
         (rec (tree &optional parent type)
           (when tree
             (prog1
                 (with-slots ((t-key key) left right) tree
                   (if (funcall key<= t-key key)
                       (cond
                         ((funcall key<= key t-key)
                          (cond
                            ((and left right)
                             (let ((sub-left (find-left right)))
                               (swap-kv sub-left tree)
                               (rec right tree :right)))
                            (t
                             (let ((sub (or left right)))
                               (case type
                                 (:right (setf (%tree-right parent) sub))
                                 (:left (setf (%tree-left parent) sub))
                                 (nil (setf (avl-tree-tree avl-tree) sub))))
                             (decf count)))
                          t)
                         (t (rec right tree :right)))
                       (rec left tree :left)))
               (when parent (rotate parent))))))
      (rec tree))))

(defun clrtree (avl-tree)
  "This removes all the entries from AVL-TREE and returns the tree itself."
  (setf (avl-tree-tree avl-tree) nil)
  (setf (avl-tree-count avl-tree) 0)
  avl-tree)

(defun dfs-maptree (function avl-tree)
  "For each entry in AVL-TREE call the two-argument FUNCTION on
the key and value of each entry in depth-first order from left to right.
Consequences are undefined if AVL-TREE is modified during this call."
  (with-slots (key<= tree) avl-tree
    (labels
        ((rec (tree)
           (when tree
             (with-slots ((t-key key) left right key value) tree
               (rec left)
               (funcall function key value)
               (rec right)))))
      (rec tree))))

(defun bfs-maptree (function avl-tree)
  "For each entry in AVL-TREE call the two-argument FUNCTION on
the key and value of each entry in breadth-first order from left to right.
Consequences are undefined if AVL-TREE is modified during this call."
  (with-slots (key<= tree) avl-tree
    (let* ((queue (cons nil nil))
           (end queue))
      (labels ((pushend (value)
                 (when value
                   (setf (cdr end) (cons value nil))
                   (setf end (cdr end))))
               (empty-p () (eq nil (cdr queue)))
               (popfront ()
                 (prog1 (pop (cdr queue))
                   (when (empty-p) (setf end queue)))))
        (when tree
          (pushend tree)
          (loop until (empty-p)
             do (let ((current (popfront)))
                  (with-slots (key value left right) current
                    (funcall function key value)
                    (pushend left)
                    (pushend right)))))))))

(defun test ()
  (let ((tree (make-avl-tree #'<=))
        (printer (lambda (k v) (print (list k v)))))
    (loop for key in '(0 8 6 4 2 3 7 9 1 5 5)
       for value in '(a b c d e f g h i j k)
       do (setf (gettree key tree) value))
    (loop for key in '(0 1 2 3 4 10)
       do (print (multiple-value-list (gettree key tree))))
    (terpri)
    (print tree)
    (terpri)
    (dfs-maptree printer tree)
    (terpri)
    (bfs-maptree printer tree)
    (terpri)
    (loop for key in '(0 1 2 3 10 7)
       do (print (remtree key tree)))
    (terpri)
    (print tree)
    (terpri)
    (clrtree tree)
    (print tree))
  (values))

(defun profile-test ()
  (let ((tree (make-avl-tree #'<=))
        (randoms (loop repeat 1000000 collect (random 100.0))))
    (loop for key in randoms do (setf (gettree key tree) key))))
```



## D

{{trans|Java}}

```d
import std.stdio, std.algorithm;

class AVLtree {
    private Node* root;

    private static struct Node {
        private int key, balance;
        private Node* left, right, parent;

        this(in int k, Node* p) pure nothrow @safe @nogc {
            key = k;
            parent = p;
        }
    }

    public bool insert(in int key) pure nothrow @safe {
        if (root is null)
            root = new Node(key, null);
        else {
            Node* n = root;
            Node* parent;
            while (true) {
                if (n.key == key)
                    return false;

                parent = n;

                bool goLeft = n.key > key;
                n = goLeft ? n.left : n.right;

                if (n is null) {
                    if (goLeft) {
                        parent.left = new Node(key, parent);
                    } else {
                        parent.right = new Node(key, parent);
                    }
                    rebalance(parent);
                    break;
                }
            }
        }
        return true;
    }

    public void deleteKey(in int delKey) pure nothrow @safe @nogc {
        if (root is null)
            return;
        Node* n = root;
        Node* parent = root;
        Node* delNode = null;
        Node* child = root;

        while (child !is null) {
            parent = n;
            n = child;
            child = delKey >= n.key ? n.right : n.left;
            if (delKey == n.key)
                delNode = n;
        }

        if (delNode !is null) {
            delNode.key = n.key;

            child = n.left !is null ? n.left : n.right;

            if (root.key == delKey) {
                root = child;
            } else {
                if (parent.left is n) {
                    parent.left = child;
                } else {
                    parent.right = child;
                }
                rebalance(parent);
            }
        }
    }

    private void rebalance(Node* n) pure nothrow @safe @nogc {
        setBalance(n);

        if (n.balance == -2) {
            if (height(n.left.left) >= height(n.left.right))
                n = rotateRight(n);
            else
                n = rotateLeftThenRight(n);

        } else if (n.balance == 2) {
            if (height(n.right.right) >= height(n.right.left))
                n = rotateLeft(n);
            else
                n = rotateRightThenLeft(n);
        }

        if (n.parent !is null) {
            rebalance(n.parent);
        } else {
            root = n;
        }
    }

    private Node* rotateLeft(Node* a) pure nothrow @safe @nogc {
        Node* b = a.right;
        b.parent = a.parent;

        a.right = b.left;

        if (a.right !is null)
            a.right.parent = a;

        b.left = a;
        a.parent = b;

        if (b.parent !is null) {
            if (b.parent.right is a) {
                b.parent.right = b;
            } else {
                b.parent.left = b;
            }
        }

        setBalance(a, b);

        return b;
    }

    private Node* rotateRight(Node* a) pure nothrow @safe @nogc {
        Node* b = a.left;
        b.parent = a.parent;

        a.left = b.right;

        if (a.left !is null)
            a.left.parent = a;

        b.right = a;
        a.parent = b;

        if (b.parent !is null) {
            if (b.parent.right is a) {
                b.parent.right = b;
            } else {
                b.parent.left = b;
            }
        }

        setBalance(a, b);

        return b;
    }

    private Node* rotateLeftThenRight(Node* n) pure nothrow @safe @nogc {
        n.left = rotateLeft(n.left);
        return rotateRight(n);
    }

    private Node* rotateRightThenLeft(Node* n) pure nothrow @safe @nogc {
        n.right = rotateRight(n.right);
        return rotateLeft(n);
    }

    private int height(in Node* n) const pure nothrow @safe @nogc {
        if (n is null)
            return -1;
        return 1 + max(height(n.left), height(n.right));
    }

    private void setBalance(Node*[] nodes...) pure nothrow @safe @nogc {
        foreach (n; nodes)
            n.balance = height(n.right) - height(n.left);
    }

    public void printBalance() const @safe {
        printBalance(root);
    }

    private void printBalance(in Node* n) const @safe {
        if (n !is null) {
            printBalance(n.left);
            write(n.balance, ' ');
            printBalance(n.right);
        }
    }
}

void main() @safe {
    auto tree = new AVLtree();

    writeln("Inserting values 1 to 10");
    foreach (immutable i; 1 .. 11)
        tree.insert(i);

    write("Printing balance: ");
    tree.printBalance;
}
```

{{out}}

```txt
Inserting values 1 to 10
Printing balance: 0 0 0 1 0 0 0 0 1 0
```



## Go

A package:

```go
package avl

// AVL tree adapted from Julienne Walker's presentation at
// http://eternallyconfuzzled.com/tuts/datastructures/jsw_tut_avl.aspx.
// This port uses similar indentifier names.

// The Key interface must be supported by data stored in the AVL tree.
type Key interface {
    Less(Key) bool
    Eq(Key) bool
}

// Node is a node in an AVL tree.
type Node struct {
    Data    Key      // anything comparable with Less and Eq.
    Balance int      // balance factor
    Link    [2]*Node // children, indexed by "direction", 0 or 1.
}

// A little readability function for returning the opposite of a direction,
// where a direction is 0 or 1.  Go inlines this.
// Where JW writes !dir, this code has opp(dir).
func opp(dir int) int {
    return 1 - dir
}

// single rotation
func single(root *Node, dir int) *Node {
    save := root.Link[opp(dir)]
    root.Link[opp(dir)] = save.Link[dir]
    save.Link[dir] = root
    return save
}

// double rotation
func double(root *Node, dir int) *Node {
    save := root.Link[opp(dir)].Link[dir]

    root.Link[opp(dir)].Link[dir] = save.Link[opp(dir)]
    save.Link[opp(dir)] = root.Link[opp(dir)]
    root.Link[opp(dir)] = save

    save = root.Link[opp(dir)]
    root.Link[opp(dir)] = save.Link[dir]
    save.Link[dir] = root
    return save
}

// adjust valance factors after double rotation
func adjustBalance(root *Node, dir, bal int) {
    n := root.Link[dir]
    nn := n.Link[opp(dir)]
    switch nn.Balance {
    case 0:
        root.Balance = 0
        n.Balance = 0
    case bal:
        root.Balance = -bal
        n.Balance = 0
    default:
        root.Balance = 0
        n.Balance = bal
    }
    nn.Balance = 0
}

func insertBalance(root *Node, dir int) *Node {
    n := root.Link[dir]
    bal := 2*dir - 1
    if n.Balance == bal {
        root.Balance = 0
        n.Balance = 0
        return single(root, opp(dir))
    }
    adjustBalance(root, dir, bal)
    return double(root, opp(dir))
}

func insertR(root *Node, data Key) (*Node, bool) {
    if root == nil {
        return &Node{Data: data}, false
    }
    dir := 0
    if root.Data.Less(data) {
        dir = 1
    }
    var done bool
    root.Link[dir], done = insertR(root.Link[dir], data)
    if done {
        return root, true
    }
    root.Balance += 2*dir - 1
    switch root.Balance {
    case 0:
        return root, true
    case 1, -1:
        return root, false
    }
    return insertBalance(root, dir), true
}

// Insert a node into the AVL tree.
// Data is inserted even if other data with the same key already exists.
func Insert(tree **Node, data Key) {
    *tree, _ = insertR(*tree, data)
}

func removeBalance(root *Node, dir int) (*Node, bool) {
    n := root.Link[opp(dir)]
    bal := 2*dir - 1
    switch n.Balance {
    case -bal:
        root.Balance = 0
        n.Balance = 0
        return single(root, dir), false
    case bal:
        adjustBalance(root, opp(dir), -bal)
        return double(root, dir), false
    }
    root.Balance = -bal
    n.Balance = bal
    return single(root, dir), true
}

func removeR(root *Node, data Key) (*Node, bool) {
    if root == nil {
        return nil, false
    }
    if root.Data.Eq(data) {
        switch {
        case root.Link[0] == nil:
            return root.Link[1], false
        case root.Link[1] == nil:
            return root.Link[0], false
        }
        heir := root.Link[0]
        for heir.Link[1] != nil {
            heir = heir.Link[1]
        }
        root.Data = heir.Data
        data = heir.Data
    }
    dir := 0
    if root.Data.Less(data) {
        dir = 1
    }
    var done bool
    root.Link[dir], done = removeR(root.Link[dir], data)
    if done {
        return root, true
    }
    root.Balance += 1 - 2*dir
    switch root.Balance {
    case 1, -1:
        return root, true
    case 0:
        return root, false
    }
    return removeBalance(root, dir)
}

// Remove a single item from an AVL tree.
// If key does not exist, function has no effect.
func Remove(tree **Node, data Key) {
    *tree, _ = removeR(*tree, data)
}
```

A demonstration program:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"

    "avl"
)

type intKey int

// satisfy avl.Key
func (k intKey) Less(k2 avl.Key) bool { return k < k2.(intKey) }
func (k intKey) Eq(k2 avl.Key) bool   { return k == k2.(intKey) }

// use json for cheap tree visualization
func dump(tree *avl.Node) {
    b, err := json.MarshalIndent(tree, "", "   ")
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(b))
}

func main() {
    var tree *avl.Node
    fmt.Println("Empty tree:")
    dump(tree)

    fmt.Println("\nInsert test:")
    avl.Insert(&tree, intKey(3))
    avl.Insert(&tree, intKey(1))
    avl.Insert(&tree, intKey(4))
    avl.Insert(&tree, intKey(1))
    avl.Insert(&tree, intKey(5))
    dump(tree)

    fmt.Println("\nRemove test:")
    avl.Remove(&tree, intKey(3))
    avl.Remove(&tree, intKey(1))
    dump(tree)
}
```

{{out}}

```txt

Empty tree:
null

Insert test:
{
   "Data": 3,
   "Balance": 0,
   "Link": [
      {
         "Data": 1,
         "Balance": -1,
         "Link": [
            {
               "Data": 1,
               "Balance": 0,
               "Link": [
                  null,
                  null
               ]
            },
            null
         ]
      },
      {
         "Data": 4,
         "Balance": 1,
         "Link": [
            null,
            {
               "Data": 5,
               "Balance": 0,
               "Link": [
                  null,
                  null
               ]
            }
         ]
      }
   ]
}

Remove test:
{
   "Data": 4,
   "Balance": 0,
   "Link": [
      {
         "Data": 1,
         "Balance": 0,
         "Link": [
            null,
            null
         ]
      },
      {
         "Data": 5,
         "Balance": 0,
         "Link": [
            null,
            null
         ]
      }
   ]
}

```



## Haskell

Based on solution of homework #4 from course http://www.seas.upenn.edu/~cis194/spring13/lectures.html.

```haskell
import Data.Monoid

data Tree a
  = Leaf
  | Node Int
         (Tree a)
         a
         (Tree a)
  deriving (Show, Eq)

foldTree
  :: Ord a
  => [a] -> Tree a
foldTree = foldr insert Leaf

height Leaf = -1
height (Node h _ _ _) = h

depth a b = 1 + (height a `max` height b)

insert
  :: Ord a
  => a -> Tree a -> Tree a
insert v Leaf = Node 1 Leaf v Leaf
insert v t@(Node n left v_ right)
  | v_ < v = rotate $ Node n left v_ (insert v right)
  | v_ > v = rotate $ Node n (insert v left) v_ right
  | otherwise = t

max_
  :: Ord a
  => Tree a -> Maybe a
max_ Leaf = Nothing
max_ (Node _ _ v right) =
  case right of
    Leaf -> Just v
    _ -> max_ right

delete
  :: Ord a
  => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node h left v right)
  | x == v =
    maybe left (\m -> rotate $ Node h left m (delete m right)) (max_ right)
  | x > v = rotate $ Node h left v (delete x right)
  | x < v = rotate $ Node h (delete x left) v right

rotate :: Tree a -> Tree a
rotate Leaf = Leaf
-- left left case
rotate (Node h (Node lh ll lv lr) v r)
  | lh - height r > 1 && height ll - height lr > 0 =
    Node lh ll lv (Node (depth r lr) lr v r)
-- right right case
rotate (Node h l v (Node rh rl rv rr))
  | rh - height l > 1 && height rr - height rl > 0 =
    Node rh (Node (depth l rl) l v rl) rv rr
-- left right case
rotate (Node h (Node lh ll lv (Node rh rl rv rr)) v r)
  | lh - height r > 1 =
    Node h (Node (rh + 1) (Node (lh - 1) ll lv rl) rv rr) v r
-- right left case
rotate (Node h l v (Node rh (Node lh ll lv lr) rv rr))
  | rh - height l > 1 =
    Node h l v (Node (lh + 1) ll lv (Node (rh - 1) lr rv rr))
-- re-weighting
rotate (Node h l v r) =
  let (l_, r_) = (rotate l, rotate r)
  in Node (depth l_ r_) l_ v r_

draw
  :: Show a
  => Tree a -> String
draw t = '\n' : draw_ t 0 <> "\n"
  where
    draw_ Leaf _ = []
    draw_ (Node h l v r) d = draw_ r (d + 1) <> node <> draw_ l (d + 1)
      where
        node = padding d <> show (v, h) <> "\n"
        padding n = replicate (n * 4) ' '

main :: IO ()
main = putStr $ draw $ foldTree [1 .. 15]
```

{{Out}}

```txt
            (15,0)
        (14,1)
            (13,0)
    (12,2)
            (11,0)
        (10,1)
            (9,0)
(8,3)
            (7,0)
        (6,1)
            (5,0)
    (4,2)
            (3,0)
        (2,1)
            (1,0)

```



## Java

This code has been cobbled together from various online examples. It's not easy to find a clear and complete explanation of AVL trees. Textbooks tend to concentrate on red-black trees because of their better efficiency. (AVL trees need to make 2 passes through the tree when inserting and deleting: one down to find the node to operate upon and one up to rebalance the tree.)

```java
public class AVLtree {

    private Node root;

    private static class Node {
        private int key;
        private int balance;
        private int height;
        private Node left;
        private Node right;
        private Node parent;

        Node(int key, Node parent) {
            this.key = key;
            this.parent = parent;
        }
    }

    public boolean insert(int key) {
        if (root == null) {
            root = new Node(key, null);
            return true;
        }

        Node n = root;
        while (true) {
            if (n.key == key)
                return false;

            Node parent = n;

            boolean goLeft = n.key > key;
            n = goLeft ? n.left : n.right;

            if (n == null) {
                if (goLeft) {
                    parent.left = new Node(key, parent);
                } else {
                    parent.right = new Node(key, parent);
                }
                rebalance(parent);
                break;
            }
        }
        return true;
    }

    private void delete(Node node) {
        if (node.left == null && node.right == null) {
            if (node.parent == null) {
                root = null;
            } else {
                Node parent = node.parent;
                if (parent.left == node) {
                    parent.left = null;
                } else {
                    parent.right = null;
                }
                rebalance(parent);
            }
            return;
        }

        if (node.left != null) {
            Node child = node.left;
            while (child.right != null) child = child.right;
            node.key = child.key;
            delete(child);
        } else {
            Node child = node.right;
            while (child.left != null) child = child.left;
            node.key = child.key;
            delete(child);
        }
    }

    public void delete(int delKey) {
        if (root == null)
            return;

        Node child = root;
        while (child != null) {
            Node node = child;
            child = delKey >= node.key ? node.right : node.left;
            if (delKey == node.key) {
                delete(node);
                return;
            }
        }
    }

    private void rebalance(Node n) {
        setBalance(n);

        if (n.balance == -2) {
            if (height(n.left.left) >= height(n.left.right))
                n = rotateRight(n);
            else
                n = rotateLeftThenRight(n);

        } else if (n.balance == 2) {
            if (height(n.right.right) >= height(n.right.left))
                n = rotateLeft(n);
            else
                n = rotateRightThenLeft(n);
        }

        if (n.parent != null) {
            rebalance(n.parent);
        } else {
            root = n;
        }
    }

    private Node rotateLeft(Node a) {

        Node b = a.right;
        b.parent = a.parent;

        a.right = b.left;

        if (a.right != null)
            a.right.parent = a;

        b.left = a;
        a.parent = b;

        if (b.parent != null) {
            if (b.parent.right == a) {
                b.parent.right = b;
            } else {
                b.parent.left = b;
            }
        }

        setBalance(a, b);

        return b;
    }

    private Node rotateRight(Node a) {

        Node b = a.left;
        b.parent = a.parent;

        a.left = b.right;

        if (a.left != null)
            a.left.parent = a;

        b.right = a;
        a.parent = b;

        if (b.parent != null) {
            if (b.parent.right == a) {
                b.parent.right = b;
            } else {
                b.parent.left = b;
            }
        }

        setBalance(a, b);

        return b;
    }

    private Node rotateLeftThenRight(Node n) {
        n.left = rotateLeft(n.left);
        return rotateRight(n);
    }

    private Node rotateRightThenLeft(Node n) {
        n.right = rotateRight(n.right);
        return rotateLeft(n);
    }

    private int height(Node n) {
        if (n == null)
            return -1;
        return n.height;
    }

    private void setBalance(Node... nodes) {
        for (Node n : nodes) {
            reheight(n);
            n.balance = height(n.right) - height(n.left);
        }
    }

    public void printBalance() {
        printBalance(root);
    }

    private void printBalance(Node n) {
        if (n != null) {
            printBalance(n.left);
            System.out.printf("%s ", n.balance);
            printBalance(n.right);
        }
    }

    private void reheight(Node node) {
        if (node != null) {
            node.height = 1 + Math.max(height(node.left), height(node.right));
        }
    }

    public static void main(String[] args) {
        AVLtree tree = new AVLtree();

        System.out.println("Inserting values 1 to 10");
        for (int i = 1; i < 10; i++)
            tree.insert(i);

        System.out.print("Printing balance: ");
        tree.printBalance();
    }
}
```



```txt
Inserting values 1 to 10
Printing balance: 0 0 0 1 0 1 0 0 0
```



###  More elaborate version

See [[AVL_tree/Java]]


## Kotlin

{{trans|Java}}

```kotlin
class AvlTree {
    private var root: Node? = null

    private class Node(var key: Int, var parent: Node?) {
        var balance: Int = 0
        var left : Node? = null
        var right: Node? = null
    }

    fun insert(key: Int): Boolean {
        if (root == null)
            root = Node(key, null)
        else {
            var n: Node? = root
            var parent: Node
            while (true) {
                if (n!!.key == key) return false
                parent = n
                val goLeft = n.key > key
                n = if (goLeft) n.left else n.right
                if (n == null) {
                    if (goLeft)
                        parent.left  = Node(key, parent)
                    else
                        parent.right = Node(key, parent)
                    rebalance(parent)
                    break
                }
            }
        }
        return true
    }

    fun delete(delKey: Int) {
        if (root == null) return
        var n:       Node? = root
        var parent:  Node? = root
        var delNode: Node? = null
        var child:   Node? = root
        while (child != null) {
            parent = n
            n = child
            child = if (delKey >= n.key) n.right else n.left
            if (delKey == n.key) delNode = n
        }
        if (delNode != null) {
            delNode.key = n!!.key
            child = if (n.left != null) n.left else n.right
            if (0 == root!!.key.compareTo(delKey)) {
                root = child

                if (null != root) {
                    root!!.parent = null
                }

            } else {
                if (parent!!.left == n)
                    parent.left = child
                else
                    parent.right = child

                if (null != child) {
                    child.parent = parent
                }

                rebalance(parent)
            }
    }

    private fun rebalance(n: Node) {
        setBalance(n)
        var nn = n
        if (nn.balance == -2)
            if (height(nn.left!!.left) >= height(nn.left!!.right))
                nn = rotateRight(nn)
            else
                nn = rotateLeftThenRight(nn)
        else if (nn.balance == 2)
            if (height(nn.right!!.right) >= height(nn.right!!.left))
                nn = rotateLeft(nn)
            else
                nn = rotateRightThenLeft(nn)
        if (nn.parent != null) rebalance(nn.parent!!)
        else root = nn
    }

    private fun rotateLeft(a: Node): Node {
        val b: Node? = a.right
        b!!.parent = a.parent
        a.right = b.left
        if (a.right != null) a.right!!.parent = a
        b.left = a
        a.parent = b
        if (b.parent != null) {
            if (b.parent!!.right == a)
                b.parent!!.right = b
            else
                b.parent!!.left = b
        }
        setBalance(a, b)
        return b
    }

    private fun rotateRight(a: Node): Node {
        val b: Node? = a.left
        b!!.parent = a.parent
        a.left = b.right
        if (a.left != null) a.left!!.parent = a
        b.right = a
        a.parent = b
        if (b.parent != null) {
            if (b.parent!!.right == a)
                b.parent!!.right = b
            else
                b.parent!!.left = b
        }
        setBalance(a, b)
        return b
    }

    private fun rotateLeftThenRight(n: Node): Node {
        n.left = rotateLeft(n.left!!)
        return rotateRight(n)
    }

    private fun rotateRightThenLeft(n: Node): Node {
        n.right = rotateRight(n.right!!)
        return rotateLeft(n)
    }

    private fun height(n: Node?): Int {
        if (n == null) return -1
        return 1 + Math.max(height(n.left), height(n.right))
    }

    private fun setBalance(vararg nodes: Node) {
        for (n in nodes) n.balance = height(n.right) - height(n.left)
    }

    fun printKey() {
        printKey(root)
        println()
    }

    private fun printKey(n: Node?) {
        if (n != null) {
            printKey(n.left)
            print("${n.key} ")
            printKey(n.right)
        }
    }

    fun printBalance() {
        printBalance(root)
        println()
    }

    private fun printBalance(n: Node?) {
        if (n != null) {
            printBalance(n.left)
            print("${n.balance} ")
            printBalance(n.right)
        }
    }
}

fun main(args: Array<String>) {
    val tree = AvlTree()
    println("Inserting values 1 to 10")
    for (i in 1..10) tree.insert(i)
    print("Printing key     : ")
    tree.printKey()
    print("Printing balance : ")
    tree.printBalance()
}
```


{{out}}

```txt

Inserting values 1 to 10
Printing key     : 1 2 3 4 5 6 7 8 9 10
Printing balance : 0 0 0 1 0 0 0 0 1 0

```



## Lua


```Lua
AVL={balance=0}
AVL.__mt={__index = AVL}


function AVL:new(list)
  local o={}
  setmetatable(o, AVL.__mt)
  for _,v in ipairs(list or {}) do
    o=o:insert(v)
  end
  return o
end

function AVL:rebalance()
  local rotated=false
  if self.balance>1 then
    if self.right.balance<0 then
      self.right, self.right.left.right, self.right.left = self.right.left, self.right, self.right.left.right
      self.right.right.balance=self.right.balance>-1 and 0 or 1
      self.right.balance=self.right.balance>0 and 2 or 1
    end
    self, self.right.left, self.right = self.right, self, self.right.left
    self.left.balance=1-self.balance
    self.balance=self.balance==0 and -1 or 0
    rotated=true
  elseif self.balance<-1 then
    if self.left.balance>0 then
      self.left, self.left.right.left, self.left.right = self.left.right, self.left, self.left.right.left
      self.left.left.balance=self.left.balance<1 and 0 or -1
      self.left.balance=self.left.balance<0 and -2 or -1
    end
    self, self.left.right, self.left = self.left, self, self.left.right
    self.right.balance=-1-self.balance
    self.balance=self.balance==0 and 1 or 0
    rotated=true
  end
  return self,rotated
end

function AVL:insert(v)
  if not self.value then
    self.value=v
    self.balance=0
    return self,1
  end
  local grow
  if v==self.value then
    return self,0
  elseif v<self.value then
    if not self.left then self.left=self:new() end
    self.left,grow=self.left:insert(v)
    self.balance=self.balance-grow
  else
    if not self.right then self.right=self:new() end
    self.right,grow=self.right:insert(v)
    self.balance=self.balance+grow
  end
  self,rotated=self:rebalance()
  return self, (rotated or self.balance==0) and 0 or grow
end

function AVL:delete_move(dir,other,mul)
  if self[dir] then
    local sb2,v
    self[dir], sb2, v=self[dir]:delete_move(dir,other,mul)
    self.balance=self.balance+sb2*mul
    self,sb2=self:rebalance()
    return self,(sb2 or self.balance==0) and -1 or 0,v
  else
    return self[other],-1,self.value
  end
end

function AVL:delete(v,isSubtree)
  local grow=0
  if v==self.value then
    local v
    if self.balance>0 then
      self.right,grow,v=self.right:delete_move("left","right",-1)
    elseif self.left then
      self.left,grow,v=self.left:delete_move("right","left",1)
      grow=-grow
    else
      return not isSubtree and AVL:new(),-1
    end
    self.value=v
    self.balance=self.balance+grow
  elseif v<self.value and self.left then
    self.left,grow=self.left:delete(v,true)
    self.balance=self.balance-grow
  elseif v>self.value and self.right then
    self.right,grow=self.right:delete(v,true)
    self.balance=self.balance+grow
  else
    return self,0
  end
  self,rotated=self:rebalance()
  return self, grow~=0 and (rotated or self.balance==0) and -1 or 0
end

-- output functions

function AVL:toList(list)
  if not self.value then return {} end
  list=list or {}
  if self.left then self.left:toList(list) end
  list[#list+1]=self.value
  if self.right then self.right:toList(list) end
  return list
end

function AVL:dump(depth)
  if not self.value then return end
  depth=depth or 0
  if self.right then self.right:dump(depth+1) end
  print(string.rep("    ",depth)..self.value.." ("..self.balance..")")
  if self.left then self.left:dump(depth+1) end
end

-- test

local test=AVL:new{1,10,5,15,20,3,5,14,7,13,2,8,3,4,5,10,9,8,7}

test:dump()
print("\ninsert 17:")
test=test:insert(17)
test:dump()
print("\ndelete 10:")
test=test:delete(10)
test:dump()
print("\nlist:")
print(unpack(test:toList()))

```

{{out}}

```txt
            20 (0)
        15 (1)
    14 (1)
        13 (0)
10 (-1)
            9 (0)
        8 (0)
            7 (0)
    5 (-1)
                4 (0)
            3 (1)
        2 (1)
            1 (0)

insert 17:
            20 (0)
        17 (0)
            15 (0)
    14 (1)
        13 (0)
10 (-1)
            9 (0)
        8 (0)
            7 (0)
    5 (-1)
                4 (0)
            3 (1)
        2 (1)
            1 (0)

delete 10:
            20 (0)
        17 (0)
            15 (0)
    14 (1)
        13 (0)
9 (-1)
        8 (-1)
            7 (0)
    5 (-1)
                4 (0)
            3 (1)
        2 (1)
            1 (0)

list:
1       2       3       4       5       7       8       9       13      14      15      17      20
```




## Objeck

{{trans|Java}}

```objeck
class AVLNode {
  @key : Int;
  @balance : Int;
  @height : Int;
  @left : AVLNode;
  @right : AVLNode;
  @above : AVLNode;

  New(key : Int, above : AVLNode) {
    @key := key;
    @above := above;
  }

  method : public : GetKey() ~ Int {
    return @key;
  }

  method : public : GetLeft() ~ AVLNode {
    return @left;
  }

  method : public : GetRight() ~ AVLNode {
    return @right;
  }

  method : public : GetAbove() ~ AVLNode {
    return @above;
  }

  method : public : GetBalance() ~ Int {
    return @balance;
  }

  method : public : GetHeight() ~ Int {
    return @height;
  }

  method : public : SetBalance(balance : Int) ~ Nil {
    @balance := balance;
  }

  method : public : SetHeight(height : Int) ~ Nil {
    @height := height;
  }

  method : public : SetAbove(above : AVLNode) ~ Nil {
    @above := above;
  }

  method : public : SetLeft(left : AVLNode) ~ Nil {
    @left := left;
  }

  method : public : SetRight(right : AVLNode) ~ Nil {
    @right := right;
  }

  method : public : SetKey(key : Int) ~ Nil {
    @key := key;
  }
}

class AVLTree {
  @root : AVLNode;

  New() {}

  method : public : Insert(key : Int) ~ Bool {
    if(@root = Nil) {
      @root := AVLNode->New( key, Nil);
      return true;
    };

    n := @root;
    while(true) {
      if(n->GetKey() = key) {
        return false;
      };

      parent := n;
      goLeft := n->GetKey() > key;
      n := goLeft ? n->GetLeft() : n->GetRight();

      if(n = Nil) {
        if(goLeft) {
          parent->SetLeft(AVLNode->New( key, parent));
        } else {
          parent->SetRight(AVLNode->New( key, parent));
        };
        Rebalance(parent);
        break;
      };
    };

    return true;
  }

  method : Delete(node : AVLNode) ~ Nil {
    if (node->GetLeft() = Nil & node->GetRight() = Nil) {
      if (node ->GetAbove() = Nil) {
        @root := Nil;
      } else {
        parent := node ->GetAbove();
        if (parent->GetLeft() = node) {
          parent->SetLeft(Nil);
        } else {
          parent->SetRight(Nil);
        };
        Rebalance(parent);
      };
      return;
    };

    if (node->GetLeft() <> Nil) {
      child := node->GetLeft();
      while (child->GetRight() <> Nil) {
        child := child->GetRight();
      };
      node->SetKey(child->GetKey());
      Delete(child);
    } else {
      child := node->GetRight();
      while (child->GetLeft() <> Nil) {
        child := child->GetLeft();
      };
      node->SetKey(child->GetKey());
      Delete(child);
    };
  }

  method : public : Delete(delKey : Int) ~ Nil {
    if (@root = Nil) {
      return;
    };

    child := @root;
    while (child <> Nil) {
      node := child;
      child := delKey >= node->GetKey() ? node->GetRight() : node->GetLeft();
      if (delKey = node->GetKey()) {
        Delete(node);
        return;
      };
    };
  }

  method : Rebalance(n : AVLNode) ~ Nil {
    SetBalance(n);

    if (n->GetBalance() = -2) {
      if (Height(n->GetLeft()->GetLeft()) >= Height(n->GetLeft()->GetRight())) {
        n := RotateRight(n);
      }
      else {
        n := RotateLeftThenRight(n);
      };

    } else if (n->GetBalance() = 2) {
      if(Height(n->GetRight()->GetRight()) >= Height(n->GetRight()->GetLeft())) {
        n := RotateLeft(n);
      }
      else {
        n := RotateRightThenLeft(n);
      };
    };

    if(n->GetAbove() <> Nil) {
      Rebalance(n->GetAbove());
    } else {
      @root := n;
    };
  }

  method : RotateLeft(a : AVLNode) ~ AVLNode {
    b := a->GetRight();
    b->SetAbove(a->GetAbove());

    a->SetRight(b->GetLeft());

    if(a->GetRight() <> Nil) {
      a->GetRight()->SetAbove(a);
    };

    b->SetLeft(a);
    a->SetAbove(b);

    if (b->GetAbove() <> Nil) {
      if (b->GetAbove()->GetRight() = a) {
        b->GetAbove()->SetRight(b);
      } else {
        b->GetAbove()->SetLeft(b);
      };
    };

    SetBalance(a);
    SetBalance(b);

    return b;
  }

  method : RotateRight(a : AVLNode) ~ AVLNode {
    b := a->GetLeft();
    b->SetAbove(a->GetAbove());

    a->SetLeft(b->GetRight());

    if (a->GetLeft() <> Nil) {
      a->GetLeft()->SetAbove(a);
    };

    b->SetRight(a);
    a->SetAbove(b);

    if (b->GetAbove() <> Nil) {
      if (b->GetAbove()->GetRight() = a) {
        b->GetAbove()->SetRight(b);
      } else {
        b->GetAbove()->SetLeft(b);
      };
    };

    SetBalance(a);
    SetBalance(b);

    return b;
  }

  method : RotateLeftThenRight(n : AVLNode) ~ AVLNode {
    n->SetLeft(RotateLeft(n->GetLeft()));
    return RotateRight(n);
  }

  method : RotateRightThenLeft(n : AVLNode) ~ AVLNode {
    n->SetRight(RotateRight(n->GetRight()));
    return RotateLeft(n);
  }

  method : SetBalance(n : AVLNode) ~ Nil {
    Reheight(n);
    n->SetBalance(Height(n->GetRight()) - Height(n->GetLeft()));
  }

  method : Reheight(node : AVLNode) ~ Nil {
    if(node <> Nil) {
      node->SetHeight(1 + Int->Max(Height(node->GetLeft()), Height(node->GetRight())));
    };
  }

  method : Height(n : AVLNode) ~ Int {
    if(n = Nil) {
      return -1;
    };

    return n->GetHeight();
  }

  method : public : PrintBalance() ~ Nil {
    PrintBalance(@root);
  }

  method : PrintBalance(n : AVLNode) ~ Nil {
    if (n <> Nil) {
      PrintBalance(n->GetLeft());
      balance := n->GetBalance();
      "{$balance} "->Print();
      PrintBalance(n->GetRight());
    };
  }
}

class Test {
  function : Main(args : String[]) ~ Nil {
    tree := AVLTree->New();

    "Inserting values 1 to 10"->PrintLine();
    for(i := 1; i < 10; i+=1;) {
      tree->Insert(i);
    };

    "Printing balance: "->Print();
    tree->PrintBalance();
  }
}

```


{{out}}

```txt

Inserting values 1 to 10
Printing balance: 0 0 0 1 0 1 0 0 0

```


=={{header|Objective-C}}==
{{trans|Java}}
{{incomplete|Objective-C|It is missing an <code>@interface</code> for AVLTree and also missing any <code>@interface</code> or <code>@implementation</code> for AVLTreeNode.}}
<lang Objective-C>
@implementation AVLTree

-(BOOL)insertWithKey:(NSInteger)key {

    if (self.root == nil) {
        self.root = [[AVLTreeNode alloc]initWithKey:key andParent:nil];
    } else {

        AVLTreeNode *n = self.root;
        AVLTreeNode *parent;

        while (true) {

            if (n.key == key) {
                return false;
            }

            parent = n;

            BOOL goLeft = n.key > key;
            n = goLeft ? n.left : n.right;

            if (n == nil) {

                if (goLeft) {
                    parent.left = [[AVLTreeNode alloc]initWithKey:key andParent:parent];
                } else {
                    parent.right = [[AVLTreeNode alloc]initWithKey:key andParent:parent];
                }
                [self rebalanceStartingAtNode:parent];
                break;
            }
        }
    }

    return true;
}

-(void)rebalanceStartingAtNode:(AVLTreeNode*)n {

    [self setBalance:@[n]];

    if (n.balance == -2) {
        if ([self height:(n.left.left)] >= [self height:n.left.right]) {
            n = [self rotateRight:n];
        } else {
            n = [self rotateLeftThenRight:n];
        }
    } else if (n.balance == 2) {
        if ([self height:n.right.right] >= [self height:n.right.left]) {
            n = [self rotateLeft:n];
        } else {
            n = [self rotateRightThenLeft:n];
        }
    }

    if (n.parent != nil) {
        [self rebalanceStartingAtNode:n.parent];
    } else {
        self.root = n;
    }
}


-(AVLTreeNode*)rotateRight:(AVLTreeNode*)a {

    AVLTreeNode *b = a.left;
    b.parent = a.parent;

    a.left = b.right;

    if (a.left != nil) {
        a.left.parent = a;
    }

    b.right = a;
    a.parent = b;

    if (b.parent != nil) {
        if (b.parent.right == a) {
            b.parent.right = b;
        } else {
            b.parent.left = b;
        }
    }

    [self setBalance:@[a,b]];
    return b;

}

-(AVLTreeNode*)rotateLeftThenRight:(AVLTreeNode*)n {

    n.left = [self rotateLeft:n.left];
    return [self rotateRight:n];

}

-(AVLTreeNode*)rotateRightThenLeft:(AVLTreeNode*)n {

    n.right = [self rotateRight:n.right];
    return [self rotateLeft:n];
}

-(AVLTreeNode*)rotateLeft:(AVLTreeNode*)a {

    //set a's right node as b
    AVLTreeNode* b = a.right;
    //set b's parent as a's parent (which could be nil)
    b.parent = a.parent;
    //in case b had a left child transfer it to a
    a.right = b.left;

    // after changing a's reference to the right child, make sure the parent is set too
    if (a.right != nil) {
        a.right.parent = a;
    }

    // switch a over to the left to be b's left child
    b.left = a;
    a.parent = b;

    if (b.parent != nil) {
        if (b.parent.right == a) {
            b.parent.right = b;
        } else {
            b.parent.right = b;
        }
    }

    [self setBalance:@[a,b]];

    return b;

}



-(void) setBalance:(NSArray*)nodesArray {

    for (AVLTreeNode* n in nodesArray) {

        n.balance = [self height:n.right] - [self height:n.left];
    }

}

-(int)height:(AVLTreeNode*)n {

    if (n == nil) {
        return -1;
    }

    return 1 + MAX([self height:n.left], [self height:n.right]);
}

-(void)printKey:(AVLTreeNode*)n {
    if (n != nil) {
        [self printKey:n.left];
        NSLog(@"%ld", n.key);
        [self printKey:n.right];
    }
}

-(void)printBalance:(AVLTreeNode*)n {
    if (n != nil) {
        [self printBalance:n.left];
        NSLog(@"%ld", n.balance);
        [self printBalance:n.right];
    }
}
@end
-- test

int main(int argc, const char * argv[]) {
    @autoreleasepool {

        AVLTree *tree = [AVLTree new];
        NSLog(@"inserting values 1 to 6");
        [tree insertWithKey:1];
        [tree insertWithKey:2];
        [tree insertWithKey:3];
        [tree insertWithKey:4];
        [tree insertWithKey:5];
        [tree insertWithKey:6];

        NSLog(@"printing balance: ");
        [tree printBalance:tree.root];

        NSLog(@"printing key: ");
        [tree printKey:tree.root];
    }
    return 0;
}


```


{{out}}

```txt

inserting values 1 to 6
printing balance:
0
0
0
0
1
0

printing key:
1
2
3
4
5
6

```



## Phix

Translated from the C version at http://www.geeksforgeeks.org/avl-tree-set-2-deletion

The standard distribution includes demo\rosetta\AVL_tree.exw, which contains a slightly longer but perhaps more readable version,
with a command line equivalent of https://www.cs.usfca.edu/~galles/visualization/AVLtree.html as well as a simple tree structure
display routine and additional verification code (both modelled on the C version found on this page)

```Phix
enum KEY = 0,
     LEFT,
     HEIGHT,    -- (NB +/-1 gives LEFT or RIGHT)
     RIGHT

sequence tree = {}
integer freelist = 0

function newNode(object key)
integer node
    if freelist=0 then
        node = length(tree)+1
        tree &= {key,NULL,1,NULL}
    else
        node = freelist
        freelist = tree[freelist]
        tree[node+KEY..node+RIGHT] = {key,NULL,1,NULL}
    end if
    return node
end function

function height(integer node)
    return iff(node=NULL?0:tree[node+HEIGHT])
end function

procedure setHeight(integer node)
    tree[node+HEIGHT] = max(height(tree[node+LEFT]), height(tree[node+RIGHT]))+1
end procedure

function rotate(integer node, integer direction)
integer idirection = LEFT+RIGHT-direction
integer pivot = tree[node+idirection]
    {tree[pivot+direction],tree[node+idirection]} = {node,tree[pivot+direction]}
    setHeight(node)
    setHeight(pivot)
    return pivot
end function

function getBalance(integer N)
    return iff(N==NULL ? 0 : height(tree[N+LEFT])-height(tree[N+RIGHT]))
end function

function insertNode(integer node, object key)
    if node==NULL then
        return newNode(key)
    end if
    integer c = compare(key,tree[node+KEY])
    if c!=0 then
        integer direction = HEIGHT+c    -- LEFT or RIGHT
        tree[node+direction] = insertNode(tree[node+direction], key)
        setHeight(node)
        integer balance = trunc(getBalance(node)/2) -- +/-1 (or 0)
        if balance then
            direction = HEIGHT-balance  -- LEFT or RIGHT
            c = compare(key,tree[tree[node+direction]+KEY])
            if c=balance then
                tree[node+direction] = rotate(tree[node+direction],direction)
            end if
            if c!=0 then
                node = rotate(node,LEFT+RIGHT-direction)
            end if
        end if
    end if
    return node
end function

function minValueNode(integer node)
    while 1 do
        integer next = tree[node+LEFT]
        if next=NULL then exit end if
        node = next
    end while
    return node
end function

function deleteNode(integer root, object key)
integer c
    if root=NULL then return root end if
    c = compare(key,tree[root+KEY])
    if c=-1 then
        tree[root+LEFT] = deleteNode(tree[root+LEFT], key)
    elsif c=+1 then
        tree[root+RIGHT] = deleteNode(tree[root+RIGHT], key)
    elsif tree[root+LEFT]==NULL
       or tree[root+RIGHT]==NULL then
        integer temp = iff(tree[root+LEFT] ? tree[root+LEFT] : tree[root+RIGHT])
        if temp==NULL then  -- No child case
            {temp,root} = {root,NULL}
        else                -- One child case
            tree[root+KEY..root+RIGHT] = tree[temp+KEY..temp+RIGHT]
        end if
        tree[temp+KEY] = freelist
        freelist = temp
    else                    -- Two child case
        integer temp = minValueNode(tree[root+RIGHT])
        tree[root+KEY] = tree[temp+KEY]
        tree[root+RIGHT] = deleteNode(tree[root+RIGHT], tree[temp+KEY])
    end if
    if root=NULL then return root end if
    setHeight(root)
    integer balance = trunc(getBalance(root)/2)
    if balance then
        integer direction = HEIGHT-balance
        c = compare(getBalance(tree[root+direction]),0)
        if c=-balance then
            tree[root+direction] = rotate(tree[root+direction],direction)
        end if
        root = rotate(root,LEFT+RIGHT-direction)
    end if
    return root
end function

procedure inOrder(integer node)
    if node!=NULL then
        inOrder(tree[node+LEFT])
        printf(1, "%d ", tree[node+KEY])
        inOrder(tree[node+RIGHT])
    end if
end procedure

integer root = NULL
sequence test = shuffle(tagset(50003))
    for i=1 to length(test) do
        root = insertNode(root,test[i])
    end for
    test = shuffle(tagset(50000))
    for i=1 to length(test) do
        root = deleteNode(root,test[i])
    end for
    inOrder(root)
```

{{out}}

```txt
50001 50002 50003
```



## Python


```python

"""

Python AVL tree example based on

https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-006-introduction-to-algorithms-fall-2011/lecture-videos/lec06_code.zip

Simplified for Rosetta Code example.

See also:

https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-006-introduction-to-algorithms-fall-2011/lecture-videos/MIT6_006F11_lec06_orig.pdf

https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-006-introduction-to-algorithms-fall-2011/lecture-videos/lecture-6-avl-trees-avl-sort/

"""

class AVLNode(object):
    """A node in the AVL tree."""

    def __init__(self, parent, k):
        """Creates a node.

        Args:
            parent: The node's parent.
            k: key of the node.
        """
        self.key = k
        self.parent = parent
        self.left = None
        self.right = None

    def _str(self):
        """Internal method for ASCII art."""
        label = str(self.key)
        if self.left is None:
            left_lines, left_pos, left_width = [], 0, 0
        else:
            left_lines, left_pos, left_width = self.left._str()
        if self.right is None:
            right_lines, right_pos, right_width = [], 0, 0
        else:
            right_lines, right_pos, right_width = self.right._str()
        middle = max(right_pos + left_width - left_pos + 1, len(label), 2)
        pos = left_pos + middle // 2
        width = left_pos + middle + right_width - right_pos
        while len(left_lines) < len(right_lines):
            left_lines.append(' ' * left_width)
        while len(right_lines) < len(left_lines):
            right_lines.append(' ' * right_width)
        if (middle - len(label)) % 2 == 1 and self.parent is not None and \
           self is self.parent.left and len(label) < middle:
            label += '.'
        label = label.center(middle, '.')
        if label[0] == '.': label = ' ' + label[1:]
        if label[-1] == '.': label = label[:-1] + ' '
        lines = [' ' * left_pos + label + ' ' * (right_width - right_pos),
                 ' ' * left_pos + '/' + ' ' * (middle-2) +
                 '\\' + ' ' * (right_width - right_pos)] + \
          [left_line + ' ' * (width - left_width - right_width) + right_line
           for left_line, right_line in zip(left_lines, right_lines)]
        return lines, pos, width

    def __str__(self):
        return '\n'.join(self._str()[0])

    def find(self, k):
        """Finds and returns the node with key k from the subtree rooted at this
        node.

        Args:
            k: The key of the node we want to find.

        Returns:
            The node with key k.
        """
        if k == self.key:
            return self
        elif k < self.key:
            if self.left is None:
                return None
            else:
                return self.left.find(k)
        else:
            if self.right is None:
                return None
            else:
                return self.right.find(k)

    def find_min(self):
        """Finds the node with the minimum key in the subtree rooted at this
        node.

        Returns:
            The node with the minimum key.
        """
        current = self
        while current.left is not None:
            current = current.left
        return current

    def next_larger(self):
        """Returns the node with the next larger key (the successor) in the BST.
        """
        if self.right is not None:
            return self.right.find_min()
        current = self
        while current.parent is not None and current is current.parent.right:
            current = current.parent
        return current.parent

    def insert(self, node):
        """Inserts a node into the subtree rooted at this node.

        Args:
            node: The node to be inserted.
        """
        if node is None:
            return
        if node.key < self.key:
            if self.left is None:
                node.parent = self
                self.left = node
            else:
                self.left.insert(node)
        else:
            if self.right is None:
                node.parent = self
                self.right = node
            else:
                self.right.insert(node)

    def delete(self):
        """Deletes and returns this node from the tree."""
        if self.left is None or self.right is None:
            if self is self.parent.left:
                self.parent.left = self.left or self.right
                if self.parent.left is not None:
                    self.parent.left.parent = self.parent
            else:
                self.parent.right = self.left or self.right
                if self.parent.right is not None:
                    self.parent.right.parent = self.parent
            return self
        else:
            s = self.next_larger()
            self.key, s.key = s.key, self.key
            return s.delete()

def height(node):
    if node is None:
        return -1
    else:
        return node.height

def update_height(node):
    node.height = max(height(node.left), height(node.right)) + 1

class AVL(object):
    """
    AVL binary search tree implementation.
    """

    def __init__(self):
        """ empty tree """
        self.root = None

    def __str__(self):
        if self.root is None: return '<empty tree>'
        return str(self.root)

    def find(self, k):
        """Finds and returns the node with key k from the subtree rooted at this
        node.

        Args:
            k: The key of the node we want to find.

        Returns:
            The node with key k or None if the tree is empty.
        """
        return self.root and self.root.find(k)

    def find_min(self):
        """Returns the minimum node of this BST."""

        return self.root and self.root.find_min()

    def next_larger(self, k):
        """Returns the node that contains the next larger (the successor) key in
        the BST in relation to the node with key k.

        Args:
            k: The key of the node of which the successor is to be found.

        Returns:
            The successor node.
        """
        node = self.find(k)
        return node and node.next_larger()

    def left_rotate(self, x):
        y = x.right
        y.parent = x.parent
        if y.parent is None:
            self.root = y
        else:
            if y.parent.left is x:
                y.parent.left = y
            elif y.parent.right is x:
                y.parent.right = y
        x.right = y.left
        if x.right is not None:
            x.right.parent = x
        y.left = x
        x.parent = y
        update_height(x)
        update_height(y)

    def right_rotate(self, x):
        y = x.left
        y.parent = x.parent
        if y.parent is None:
            self.root = y
        else:
            if y.parent.left is x:
                y.parent.left = y
            elif y.parent.right is x:
                y.parent.right = y
        x.left = y.right
        if x.left is not None:
            x.left.parent = x
        y.right = x
        x.parent = y
        update_height(x)
        update_height(y)

    def rebalance(self, node):
        while node is not None:
            update_height(node)
            if height(node.left) >= 2 + height(node.right):
                if height(node.left.left) >= height(node.left.right):
                    self.right_rotate(node)
                else:
                    self.left_rotate(node.left)
                    self.right_rotate(node)
            elif height(node.right) >= 2 + height(node.left):
                if height(node.right.right) >= height(node.right.left):
                    self.left_rotate(node)
                else:
                    self.right_rotate(node.right)
                    self.left_rotate(node)
            node = node.parent

    def insert(self, k):
        """Inserts a node with key k into the subtree rooted at this node.
        This AVL version guarantees the balance property: h = O(lg n).

        Args:
            k: The key of the node to be inserted.
        """
        node = AVLNode(None, k)
        if self.root is None:
            # The root's parent is None.
            self.root = node
        else:
            self.root.insert(node)
        self.rebalance(node)

    def delete(self, k):
        """Deletes and returns a node with key k if it exists from the BST.
        This AVL version guarantees the balance property: h = O(lg n).

        Args:
            k: The key of the node that we want to delete.

        Returns:
            The deleted node with key k.
        """
        node = self.find(k)
        if node is None:
            return None
        if node is self.root:
            pseudoroot = AVLNode(None, 0)
            pseudoroot.left = self.root
            self.root.parent = pseudoroot
            deleted = self.root.delete()
            self.root = pseudoroot.left
            if self.root is not None:
                self.root.parent = None
        else:
            deleted = node.delete()
        ## node.parent is actually the old parent of the node,
        ## which is the first potentially out-of-balance node.
        self.rebalance(deleted.parent)

def test(args=None):
    import random, sys
    if not args:
        args = sys.argv[1:]
    if not args:
        print('usage: %s <number-of-random-items | item item item ...>' % \
              sys.argv[0])
        sys.exit()
    elif len(args) == 1:
        items = (random.randrange(100) for i in range(int(args[0])))
    else:
        items = [int(i) for i in args]

    tree = AVL()
    print(tree)
    for item in items:
        tree.insert(item)
        print()
        print(tree)

if __name__ == '__main__': test()

```

{{out}}

```txt

python avlrc.py 1 2 3 4 5 6 7 8 9 10

... only showing last tree ...

   ..4...
  /      \
  2      .8.
 / \    /   \
1  3    6   9
/\ /\  / \  /\
      5  7   10
      /\ /\  /\


```



## Rust

See [[AVL tree/Rust]].

## Scala


```scala
import scala.collection.mutable

class AVLTree[A](implicit val ordering: Ordering[A]) extends mutable.SortedSet[A] {

  if (ordering eq null) throw new NullPointerException("ordering must not be null")

  private var _root: AVLNode = _
  private var _size = 0

  override def size: Int = _size

  override def foreach[U](f: A => U): Unit = {
    val stack = mutable.Stack[AVLNode]()
    var current = root
    var done = false

    while (!done) {
      if (current != null) {
        stack.push(current)
        current = current.left
      } else if (stack.nonEmpty) {
        current = stack.pop()
        f.apply(current.key)

        current = current.right
      } else {
        done = true
      }
    }
  }

  def root: AVLNode = _root

  override def isEmpty: Boolean = root == null

  override def min[B >: A](implicit cmp: Ordering[B]): A = minNode().key

  def minNode(): AVLNode = {
    if (root == null) throw new UnsupportedOperationException("empty tree")
    var node = root
    while (node.left != null) node = node.left
    node
  }

  override def max[B >: A](implicit cmp: Ordering[B]): A = maxNode().key

  def maxNode(): AVLNode = {
    if (root == null) throw new UnsupportedOperationException("empty tree")
    var node = root
    while (node.right != null) node = node.right
    node
  }

  def next(node: AVLNode): Option[AVLNode] = {
    var successor = node
    if (successor != null) {
      if (successor.right != null) {
        successor = successor.right
        while (successor != null && successor.left != null) {
          successor = successor.left
        }
      } else {
        successor = node.parent
        var n = node
        while (successor != null && successor.right == n) {
          n = successor
          successor = successor.parent
        }
      }
    }
    Option(successor)
  }

  def prev(node: AVLNode): Option[AVLNode] = {
    var predecessor = node
    if (predecessor != null) {
      if (predecessor.left != null) {
        predecessor = predecessor.left
        while (predecessor != null && predecessor.right != null) {
          predecessor = predecessor.right
        }
      } else {
        predecessor = node.parent
        var n = node
        while (predecessor != null && predecessor.left == n) {
          n = predecessor
          predecessor = predecessor.parent
        }
      }
    }
    Option(predecessor)
  }

  override def rangeImpl(from: Option[A], until: Option[A]): mutable.SortedSet[A] = ???

  override def +=(key: A): AVLTree.this.type = {
    insert(key)
    this
  }

  def insert(key: A): AVLNode = {
    if (root == null) {
      _root = new AVLNode(key)
      _size += 1
      return root
    }

    var node = root
    var parent: AVLNode = null
    var cmp = 0

    while (node != null) {
      parent = node
      cmp = ordering.compare(key, node.key)
      if (cmp == 0) return node // duplicate
      node = node.matchNextChild(cmp)
    }

    val newNode = new AVLNode(key, parent)
    if (cmp <= 0) parent._left = newNode
    else parent._right = newNode

    while (parent != null) {
      cmp = ordering.compare(parent.key, key)
      if (cmp < 0) parent.balanceFactor -= 1
      else parent.balanceFactor += 1

      parent = parent.balanceFactor match {
        case -1 | 1 => parent.parent
        case x if x < -1 =>
          if (parent.right.balanceFactor == 1) rotateRight(parent.right)
          val newRoot = rotateLeft(parent)
          if (parent == root) _root = newRoot
          null
        case x if x > 1 =>
          if (parent.left.balanceFactor == -1) rotateLeft(parent.left)
          val newRoot = rotateRight(parent)
          if (parent == root) _root = newRoot
          null
        case _ => null
      }
    }

    _size += 1
    newNode
  }

  override def -=(key: A): AVLTree.this.type = {
    remove(key)
    this
  }

  override def remove(key: A): Boolean = {
    var node = findNode(key).orNull
    if (node == null) return false

    if (node.left != null) {
      var max = node.left

      while (max.left != null || max.right != null) {
        while (max.right != null) max = max.right

        node._key = max.key
        if (max.left != null) {
          node = max
          max = max.left
        }
      }
      node._key = max.key
      node = max
    }

    if (node.right != null) {
      var min = node.right

      while (min.left != null || min.right != null) {
        while (min.left != null) min = min.left

        node._key = min.key
        if (min.right != null) {
          node = min
          min = min.right
        }
      }
      node._key = min.key
      node = min
    }

    var current = node
    var parent = node.parent
    while (parent != null) {
      parent.balanceFactor += (if (parent.left == current) -1 else 1)

      current = parent.balanceFactor match {
        case x if x < -1 =>
          if (parent.right.balanceFactor == 1) rotateRight(parent.right)
          val newRoot = rotateLeft(parent)
          if (parent == root) _root = newRoot
          newRoot
        case x if x > 1 =>
          if (parent.left.balanceFactor == -1) rotateLeft(parent.left)
          val newRoot = rotateRight(parent)
          if (parent == root) _root = newRoot
          newRoot
        case _ => parent
      }

      parent = current.balanceFactor match {
        case -1 | 1 => null
        case _ => current.parent
      }
    }

    if (node.parent != null) {
      if (node.parent.left == node) {
        node.parent._left = null
      } else {
        node.parent._right = null
      }
    }

    if (node == root) _root = null

    _size -= 1
    true
  }

  def findNode(key: A): Option[AVLNode] = {
    var node = root
    while (node != null) {
      val cmp = ordering.compare(key, node.key)
      if (cmp == 0) return Some(node)
      node = node.matchNextChild(cmp)
    }
    None
  }

  private def rotateLeft(node: AVLNode): AVLNode = {
    val rightNode = node.right
    node._right = rightNode.left
    if (node.right != null) node.right._parent = node

    rightNode._parent = node.parent
    if (rightNode.parent != null) {
      if (rightNode.parent.left == node) {
        rightNode.parent._left = rightNode
      } else {
        rightNode.parent._right = rightNode
      }
    }

    node._parent = rightNode
    rightNode._left = node

    node.balanceFactor += 1
    if (rightNode.balanceFactor < 0) {
      node.balanceFactor -= rightNode.balanceFactor
    }

    rightNode.balanceFactor += 1
    if (node.balanceFactor > 0) {
      rightNode.balanceFactor += node.balanceFactor
    }
    rightNode
  }

  private def rotateRight(node: AVLNode): AVLNode = {
    val leftNode = node.left
    node._left = leftNode.right
    if (node.left != null) node.left._parent = node

    leftNode._parent = node.parent
    if (leftNode.parent != null) {
      if (leftNode.parent.left == node) {
        leftNode.parent._left = leftNode
      } else {
        leftNode.parent._right = leftNode
      }
    }

    node._parent = leftNode
    leftNode._right = node

    node.balanceFactor -= 1
    if (leftNode.balanceFactor > 0) {
      node.balanceFactor -= leftNode.balanceFactor
    }

    leftNode.balanceFactor -= 1
    if (node.balanceFactor < 0) {
      leftNode.balanceFactor += node.balanceFactor
    }
    leftNode
  }

  override def contains(elem: A): Boolean = findNode(elem).isDefined

  override def iterator: Iterator[A] = ???

  override def keysIteratorFrom(start: A): Iterator[A] = ???

  class AVLNode private[AVLTree](k: A, p: AVLNode = null) {

    private[AVLTree] var _key: A = k
    private[AVLTree] var _parent: AVLNode = p
    private[AVLTree] var _left: AVLNode = _
    private[AVLTree] var _right: AVLNode = _
    private[AVLTree] var balanceFactor: Int = 0

    def parent: AVLNode = _parent

    private[AVLTree] def selectNextChild(key: A): AVLNode = matchNextChild(ordering.compare(key, this.key))

    def key: A = _key

    private[AVLTree] def matchNextChild(cmp: Int): AVLNode = cmp match {
      case x if x < 0 => left
      case x if x > 0 => right
      case _ => null
    }

    def left: AVLNode = _left

    def right: AVLNode = _right
  }

}
```


## Sidef

{{trans|D}}

```ruby
class AVLtree {

    has root = nil

    struct Node {
        Number key,
        Number balance = 0,
        Node left = nil,
        Node right = nil,
        Node parent = nil,
    }

    method insert(key) {
        if (root == nil) {
            root = Node(key)
            return true
        }

        var n = root
        var parent = nil

        loop {
            if (n.key == key) {
                return false
            }
            parent = n
            var goLeft = (n.key > key)
            n = (goLeft ? n.left : n.right)

            if (n == nil) {
                var tn = Node(key, parent: parent)
                if (goLeft) {
                    parent.left = tn
                }
                else {
                    parent.right = tn
                }
                self.rebalance(parent)
                break
            }
        }

        return true
    }

    method delete_key(delKey) {
        if (root == nil) { return nil }

        var n = root
        var parent = root
        var delNode = nil
        var child = root

        while (child != nil) {
            parent = n
            n = child
            child = (delKey >= n.key ? n.right : n.left)
            if (delKey == n.key) {
                delNode = n
            }
        }

        if (delNode != nil) {
            delNode.key = n.key
            child = (n.left != nil ? n.left : n.right)

            if (root.key == delKey) {
                root = child
            }
            else {
                if (parent.left == n) {
                    parent.left = child
                }
                else {
                    parent.right = child
                }
                self.rebalance(parent)
            }
        }
    }

    method rebalance(n) {
        if (n == nil) { return nil }
        self.setBalance(n)

        given (n.balance) {
            when (-2) {
                if (self.height(n.left.left) >= self.height(n.left.right)) {
                    n = self.rotate(n, :right)
                }
                else {
                    n = self.rotate_twice(n, :left, :right)
                }
            }
            when (2) {
                if (self.height(n.right.right) >= self.height(n.right.left)) {
                    n = self.rotate(n, :left)
                }
                else {
                    n = self.rotate_twice(n, :right, :left)
                }
            }
        }

        if (n.parent != nil) {
            self.rebalance(n.parent)
        }
        else {
            root = n
        }
    }

    method rotate(a, dir) {
        var b = (dir == :left ? a.right : a.left)
        b.parent = a.parent

        (dir == :left) ? (a.right = b.left)
                       : (a.left  = b.right)

        if (a.right != nil) {
            a.right.parent = a
        }

        b.$dir = a
        a.parent = b

        if (b.parent != nil) {
            if (b.parent.right == a) {
                b.parent.right = b
            }
            else {
                b.parent.left = b
            }
        }

        self.setBalance(a, b)
        return b
    }

    method rotate_twice(n, dir1, dir2) {
        n.left = self.rotate(n.left, dir1)
        self.rotate(n, dir2)
    }

    method height(n) {
        if (n == nil) { return -1 }
        1 + Math.max(self.height(n.left), self.height(n.right))
    }

    method setBalance(*nodes) {
        nodes.each { |n|
            n.balance = (self.height(n.right) - self.height(n.left))
        }
    }

    method printBalance {
        self.printBalance(root)
    }

    method printBalance(n) {
        if (n != nil) {
            self.printBalance(n.left)
            print(n.balance, ' ')
            self.printBalance(n.right)
        }
    }
}

var tree = AVLtree()

say "Inserting values 1 to 10"
{|i| tree.insert(i) } << 1..10
print "Printing balance: "
tree.printBalance
```

{{out}}

```txt

Inserting values 1 to 10
Printing balance: 0 0 0 1 0 0 0 0 1 0

```



## Simula


```simula
CLASS AVL;
BEGIN

    ! AVL TREE ADAPTED FROM JULIENNE WALKER'S PRESENTATION AT ;
    ! HTTP://ETERNALLYCONFUZZLED.COM/TUTS/DATASTRUCTURES/JSW_TUT_AVL.ASPX. ;
    ! THIS PORT USES SIMILAR INDENTIFIER NAMES. ;

    ! THE KEY INTERFACE MUST BE SUPPORTED BY DATA STORED IN THE AVL TREE. ;
    CLASS KEY;
    VIRTUAL:
        PROCEDURE LESS  IS BOOLEAN PROCEDURE LESS (K); REF(KEY) K;;
        PROCEDURE EQUAL IS BOOLEAN PROCEDURE EQUAL(K); REF(KEY) K;;
    BEGIN
    END KEY;

    ! NODE IS A NODE IN AN AVL TREE. ;
    CLASS NODE(DATA); REF(KEY) DATA;  ! ANYTHING COMPARABLE WITH LESS AND EQUAL. ;
    BEGIN
        INTEGER  BALANCE;             ! BALANCE FACTOR ;
        REF(NODE) ARRAY LINK(0:1);    ! CHILDREN, INDEXED BY "DIRECTION", 0 OR 1. ;
    END NODE;

    ! A LITTLE READABILITY FUNCTION FOR RETURNING THE OPPOSITE OF A DIRECTION, ;
    ! WHERE A DIRECTION IS 0 OR 1. ;
    ! WHERE JW WRITES !DIR, THIS CODE HAS OPP(DIR). ;
    INTEGER PROCEDURE OPP(DIR); INTEGER DIR;
    BEGIN
        OPP := 1 - DIR;
    END OPP;

    ! SINGLE ROTATION ;
    REF(NODE) PROCEDURE SINGLE(ROOT, DIR); REF(NODE) ROOT; INTEGER DIR;
    BEGIN
        REF(NODE) SAVE;
        SAVE :- ROOT.LINK(OPP(DIR));
        ROOT.LINK(OPP(DIR)) :- SAVE.LINK(DIR);
        SAVE.LINK(DIR) :- ROOT;
        SINGLE :- SAVE;
    END SINGLE;

    ! DOUBLE ROTATION ;
    REF(NODE) PROCEDURE DOUBLE(ROOT, DIR); REF(NODE) ROOT; INTEGER DIR;
    BEGIN
        REF(NODE) SAVE;
        SAVE :- ROOT.LINK(OPP(DIR)).LINK(DIR);

        ROOT.LINK(OPP(DIR)).LINK(DIR) :- SAVE.LINK(OPP(DIR));
        SAVE.LINK(OPP(DIR)) :- ROOT.LINK(OPP(DIR));
        ROOT.LINK(OPP(DIR)) :- SAVE;

        SAVE :- ROOT.LINK(OPP(DIR));
        ROOT.LINK(OPP(DIR)) :- SAVE.LINK(DIR);
        SAVE.LINK(DIR) :- ROOT;
        DOUBLE :- SAVE;
    END DOUBLE;

    ! ADJUST BALANCE FACTORS AFTER DOUBLE ROTATION ;
    PROCEDURE ADJUSTBALANCE(ROOT, DIR, BAL); REF(NODE) ROOT; INTEGER DIR, BAL;
    BEGIN
        REF(NODE) N, NN;
        N :- ROOT.LINK(DIR);
        NN :- N.LINK(OPP(DIR));
        IF NN.BALANCE = 0   THEN BEGIN ROOT.BALANCE := 0;    N.BALANCE := 0;   END ELSE
        IF NN.BALANCE = BAL THEN BEGIN ROOT.BALANCE := -BAL; N.BALANCE := 0;   END
                            ELSE BEGIN ROOT.BALANCE := 0;    N.BALANCE := BAL; END;
        NN.BALANCE := 0;
    END ADJUSTBALANCE;

    REF(NODE) PROCEDURE INSERTBALANCE(ROOT, DIR); REF(NODE) ROOT; INTEGER DIR;
    BEGIN REF(NODE) N;  INTEGER BAL;
        N :- ROOT.LINK(DIR);
        BAL := 2*DIR - 1;
        IF N.BALANCE = BAL THEN
        BEGIN
            ROOT.BALANCE := 0;
            N.BALANCE := 0;
            INSERTBALANCE :- SINGLE(ROOT, OPP(DIR));
        END ELSE
        BEGIN
            ADJUSTBALANCE(ROOT, DIR, BAL);
            INSERTBALANCE :- DOUBLE(ROOT, OPP(DIR));
        END;
    END INSERTBALANCE;

    CLASS TUPLE(N,B); REF(NODE) N; BOOLEAN B;;

    REF(TUPLE) PROCEDURE INSERTR(ROOT, DATA); REF(NODE) ROOT; REF(KEY) DATA;
    BEGIN
        IF ROOT == NONE THEN
            INSERTR :- NEW TUPLE(NEW NODE(DATA), FALSE)
        ELSE
        BEGIN
            REF(TUPLE) T;  BOOLEAN DONE;  INTEGER DIR;
            DIR := 0;
            IF ROOT.DATA.LESS(DATA) THEN
                DIR := 1;
            T :- INSERTR(ROOT.LINK(DIR), DATA);
            ROOT.LINK(DIR) :- T.N;
            DONE := T.B;
            IF DONE THEN INSERTR :- NEW TUPLE(ROOT, TRUE) ELSE
            BEGIN
                ROOT.BALANCE := ROOT.BALANCE + 2*DIR - 1;
                IF ROOT.BALANCE = 0 THEN
                    INSERTR :- NEW TUPLE(ROOT, TRUE) ELSE
                IF ROOT.BALANCE = 1 OR ROOT.BALANCE = -1 THEN
                    INSERTR :- NEW TUPLE(ROOT, FALSE)
                ELSE
                    INSERTR :- NEW TUPLE(INSERTBALANCE(ROOT, DIR), TRUE);
            END;
        END;
    END INSERTR;

    ! INSERT A NODE INTO THE AVL TREE. ;
    ! DATA IS INSERTED EVEN IF OTHER DATA WITH THE SAME KEY ALREADY EXISTS. ;
    PROCEDURE INSERT(TREE, DATA); NAME TREE; REF(NODE) TREE; REF(KEY) DATA;
    BEGIN
        REF(TUPLE) T;
        T :- INSERTR(TREE, DATA);
        TREE :- T.N;
    END INSERT;

    REF(TUPLE) PROCEDURE REMOVEBALANCE(ROOT, DIR); REF(NODE) ROOT; INTEGER DIR;
    BEGIN REF(NODE) N;  INTEGER BAL;
        N :- ROOT.LINK(OPP(DIR));
        BAL := 2*DIR - 1;

        IF N.BALANCE = -BAL THEN
        BEGIN ROOT.BALANCE := 0; N.BALANCE := 0;
            REMOVEBALANCE :- NEW TUPLE(SINGLE(ROOT, DIR), FALSE);
        END ELSE

        IF N.BALANCE = BAL THEN
        BEGIN ADJUSTBALANCE(ROOT, OPP(DIR), -BAL);
            REMOVEBALANCE :- NEW TUPLE(DOUBLE(ROOT, DIR), FALSE);
        END ELSE

        BEGIN ROOT.BALANCE := -BAL; N.BALANCE := BAL;
            REMOVEBALANCE :- NEW TUPLE(SINGLE(ROOT, DIR), TRUE);
        END
    END REMOVEBALANCE;

    REF(TUPLE) PROCEDURE REMOVER(ROOT, DATA); REF(NODE) ROOT; REF(KEY) DATA;
    BEGIN INTEGER DIR; BOOLEAN DONE; REF(TUPLE) T;

        IF ROOT == NONE THEN
            REMOVER :- NEW TUPLE(NONE, FALSE)
        ELSE
        IF ROOT.DATA.EQUAL(DATA) THEN
        BEGIN
            IF ROOT.LINK(0) == NONE THEN
            BEGIN
                REMOVER :- NEW TUPLE(ROOT.LINK(1), FALSE);
                GOTO L;
            END

            ELSE IF ROOT.LINK(1) == NONE THEN
            BEGIN
                REMOVER :- NEW TUPLE(ROOT.LINK(0), FALSE);
                GOTO L;
            END

            ELSE
            BEGIN REF(NODE) HEIR;
                HEIR :- ROOT.LINK(0);
                WHILE HEIR.LINK(1) =/= NONE DO
                    HEIR :- HEIR.LINK(1);
                ROOT.DATA :- HEIR.DATA;
                DATA :- HEIR.DATA;
            END;
        END;
        DIR := 0;
        IF ROOT.DATA.LESS(DATA) THEN
            DIR := 1;
        T :- REMOVER(ROOT.LINK(DIR), DATA); ROOT.LINK(DIR) :- T.N; DONE := T.B;
        IF DONE THEN
        BEGIN
            REMOVER :- NEW TUPLE(ROOT, TRUE);
            GOTO L;
        END;
        ROOT.BALANCE := ROOT.BALANCE + 1 - 2*DIR;
        IF ROOT.BALANCE = 1 OR ROOT.BALANCE = -1 THEN
            REMOVER :- NEW TUPLE(ROOT, TRUE)

        ELSE IF ROOT.BALANCE = 0 THEN
            REMOVER :- NEW TUPLE(ROOT, FALSE)

        ELSE
            REMOVER :- REMOVEBALANCE(ROOT, DIR);
    L:
    END REMOVER;

    ! REMOVE A SINGLE ITEM FROM AN AVL TREE. ;
    ! IF KEY DOES NOT EXIST, FUNCTION HAS NO EFFECT. ;
    PROCEDURE REMOVE(TREE, DATA); NAME TREE; REF(NODE) TREE; REF(KEY) DATA;
    BEGIN REF(TUPLE) T;
        T :- REMOVER(TREE, DATA);
        TREE :- T.N;
    END REMOVEM;

END.
```

A demonstration program:

```simula
EXTERNAL CLASS AVL;

AVL
BEGIN

    KEY CLASS INTEGERKEY(I); INTEGER I;
    BEGIN
        BOOLEAN PROCEDURE LESS (K); REF(KEY) K; LESS  := I < K QUA INTEGERKEY.I;
        BOOLEAN PROCEDURE EQUAL(K); REF(KEY) K; EQUAL := I = K QUA INTEGERKEY.I;
    END INTEGERKEY;

    PROCEDURE DUMP(ROOT); REF(NODE) ROOT;
    BEGIN
        IF ROOT =/= NONE THEN
        BEGIN
            DUMP(ROOT.LINK(0));
            OUTINT(ROOT.DATA QUA INTEGERKEY.I, 0); OUTTEXT(" ");
            DUMP(ROOT.LINK(1));
        END
    END DUMP;

    INTEGER I;
    REF(NODE) TREE;
    OUTTEXT("Empty tree: "); DUMP(TREE); OUTIMAGE;

    FOR I := 3, 1, 4, 1, 5 DO
    BEGIN OUTTEXT("Insert "); OUTINT(I, 0); OUTTEXT(": ");
          INSERT(TREE, NEW INTEGERKEY(I)); DUMP(TREE); OUTIMAGE;
    END;

    FOR I := 3, 1 DO
    BEGIN OUTTEXT("Remove "); OUTINT(I, 0); OUTTEXT(": ");
          REMOVE(TREE, NEW INTEGERKEY(I)); DUMP(TREE); OUTIMAGE;
    END;

END.
```

{{out}}

```txt

Empty tree:
Insert 3: 3
Insert 1: 1 3
Insert 4: 1 3 4
Insert 1: 1 1 3 4
Insert 5: 1 1 3 4 5
Remove 3: 1 1 4 5
Remove 1: 1 4 5

```



## Tcl

Note that in general, you would not normally write a tree directly in Tcl when writing code that required an <math>\alpha</math><sup>=</sup><math>\rightarrow\beta</math> map, but would rather use either an array variable or a dictionary value (which are internally implemented using a high-performance hash table engine).
{{works with|Tcl|8.6}}

```tcl
package require TclOO

namespace eval AVL {
    # Class for the overall tree; manages real public API
    oo::class create Tree {
	variable root nil class
	constructor {{nodeClass AVL::Node}} {
	    set class [oo::class create Node [list superclass $nodeClass]]

	    # Create a nil instance to act as a leaf sentinel
	    set nil [my NewNode ""]
	    set root [$nil ref]

	    # Make nil be special
	    oo::objdefine $nil {
		method height {} {return 0}
		method key {} {error "no key possible"}
		method value {} {error "no value possible"}
		method destroy {} {
		    # Do nothing (doesn't prohibit destruction entirely)
		}
		method print {indent increment} {
		    # Do nothing
		}
	    }
	}

	# How to actually manufacture a new node
	method NewNode {key} {
	    if {![info exists nil]} {set nil ""}
	    $class new $key $nil [list [namespace current]::my NewNode]
	}

	# Create a new node in the tree and return it
	method insert {key} {
	    set node [my NewNode $key]
	    if {$root eq $nil} {
		set root $node
	    } else {
		$root insert $node
	    }
	    return $node
	}

	# Find the node for a particular key
	method lookup {key} {
	    for {set node $root} {$node ne $nil} {} {
		if {[$node key] == $key} {
		    return $node
		} elseif {[$node key] > $key} {
		    set node [$node left]
		} else {
		    set node [$node right]
		}
	    }
	    error "no such node"
	}

	# Print a tree out, one node per line
	method print {{indent 0} {increment 1}} {
	    $root print $indent $increment
	    return
	}
    }

    # Class of an individual node; may be subclassed
    oo::class create Node {
	variable key value left right 0 refcount newNode
	constructor {n nil instanceFactory} {
	    set newNode $instanceFactory
	    set 0 [expr {$nil eq "" ? [self] : $nil}]
	    set key $n
	    set value {}
	    set left [set right $0]
	    set refcount 0
	}
	method ref {} {
	    incr refcount
	    return [self]
	}
	method destroy {} {
	    if {[incr refcount -1] < 1} next
	}
	method New {key value} {
	    set n [{*}$newNode $key]
	    $n setValue $value
	    return $n
	}

	# Getters
	method key {} {return $key}
	method value {} {return $value}
	method left {} {return $left}
	method right {args} {return $right}

	# Setters
	method setValue {newValue} {
	    set value $newValue
	}
	method setLeft {node} {
	    # Non-trivial because of reference management
	    $node ref
	    $left destroy
	    set left $node
	    return
	}
	method setRight {node} {
	    # Non-trivial because of reference management
	    $node ref
	    $right destroy
	    set right $node
	    return
	}

	# Print a node and its descendents
	method print {indent increment} {
	    puts [format "%s%s => %s" [string repeat " " $indent] $key $value]
	    incr indent $increment
	    $left print $indent $increment
	    $right print $indent $increment
	}

	method height {} {
	    return [expr {max([$left height], [$right height]) + 1}]
	}
	method balanceFactor {} {
	    expr {[$left height] - [$right height]}
	}

	method insert {node} {
	    # Simple insertion
	    if {$key > [$node key]} {
		if {$left eq $0} {
		    my setLeft $node
		} else {
		    $left insert $node
		}
	    } else {
		if {$right eq $0} {
		    my setRight $node
		} else {
		    $right insert $node
		}
	    }

	    # Rebalance this node
	    if {[my balanceFactor] > 1} {
		if {[$left balanceFactor] < 0} {
		    $left rotateLeft
		}
		my rotateRight
	    } elseif {[my balanceFactor] < -1} {
		if {[$right balanceFactor] > 0} {
		    $right rotateRight
		}
		my rotateLeft
	    }
	}

	# AVL Rotations
	method rotateLeft {} {
	    set new [my New $key $value]
	    set key [$right key]
	    set value [$right value]
	    $new setLeft $left
	    $new setRight [$right left]
	    my setLeft $new
	    my setRight [$right right]
	}

	method rotateRight {} {
	    set new [my New $key $value]
	    set key [$left key]
	    set value [$left value]
	    $new setLeft [$left right]
	    $new setRight $right
	    my setLeft [$left left]
	    my setRight $new
	}
    }
}
```

Demonstrating:

```tcl
# Create an AVL tree
AVL::Tree create tree

# Populate it with some semi-random data
for {set i 33} {$i < 127} {incr i} {
    [tree insert $i] setValue \
	[string repeat [format %c $i] [expr {1+int(rand()*5)}]]
}

# Print it out
tree print

# Look up a few values in the tree
for {set i 0} {$i < 10} {incr i} {
    set k [expr {33+int((127-33)*rand())}]
    puts $k=>[[tree lookup $k] value]
}

# Destroy the tree and all its nodes
tree destroy
```

{{out}}
<pre style="overflow:auto;height:400px">
64 => @@@
 48 => 000
  40 => (((((
   36 => $
    34 => """
     33 => !!
     35 => #####
    38 => &&&
     37 => %
     39 => ''''
   44 => ,
    42 => **
     41 => )))
     43 => +++++
    46 => .
     45 => --
     47 => ////
  56 => 888
   52 => 444
    50 => 22222
     49 => 1111
     51 => 333
    54 => 6
     53 => 555
     55 => 77
   60 => <<<<
    58 => ::::
     57 => 99999
     59 => ;
    62 => >>>
     61 => ===
     63 => ??
 96 => ``
  80 => PPPPP
   72 => HHHH
    68 => DDD
     66 => BBBB
      65 => A
      67 => CCC
     70 => FFF
      69 => EEEE
      71 => GGG
    76 => LL
     74 => JJ
      73 => III
      75 => KKKK
     78 => N
      77 => MMMMM
      79 => OOOOO
   88 => XXX
    84 => TTTT
     82 => R
      81 => QQQQ
      83 => SSSS
     86 => V
      85 => UUU
      87 => WWW
    92 => \\\
     90 => Z
      89 => YYYYY
      91 => [
     94 => ^^^^^
      93 => ]]]]
      95 => _____
  112 => pppp
   104 => hh
    100 => d
     98 => bb
      97 => aaa
      99 => cccc
     102 => ff
      101 => eeee
      103 => gggg
    108 => lll
     106 => j
      105 => iii
      107 => kkkkk
     110 => nn
      109 => m
      111 => o
   120 => x
    116 => ttt
     114 => rrrrr
      113 => qqqqq
      115 => s
     118 => vvv
      117 => uuuu
      119 => wwww
    124 => ||||
     122 => zzzz
      121 => y
      123 => {{{
     125 => }}}}
      126 => ~~~~
53=>555
55=>77
60=><<<<
100=>d
99=>cccc
93=>]]]]
57=>99999
56=>888
47=>////
39=>''''

```



## TypeScript

{{trans|Java}}
For use within a project, consider adding "export default" to AVLtree class declaration.

```JavaScript
/** A single node in an AVL tree */
class AVLnode <T> {
    balance: number
    left: AVLnode<T>
    right: AVLnode<T>

    constructor(public key: T, public parent: AVLnode<T> = null) {
        this.balance = 0
        this.left = null
        this.right = null
    }
}

/** The balanced AVL tree */
class AVLtree <T> {
    // public members organized here
    constructor() {
        this.root = null
    }

    insert(key: T): boolean {
        if (this.root === null) {
            this.root = new AVLnode<T>(key)
        } else {
            let n: AVLnode<T> = this.root,
                parent: AVLnode<T> = null

            while (true) {
                if(n.key === key) {
                    return false
                }

                parent = n

                let goLeft: boolean = n.key > key
                n = goLeft ? n.left : n.right

                if (n === null) {
                    if (goLeft) {
                        parent.left = new AVLnode<T>(key, parent)
                    } else {
                        parent.right = new AVLnode<T>(key, parent)
                    }

                    this.rebalance(parent)
                    break
                }
            }
        }

        return true
    }

    deleteKey(delKey: T): void {
        if (this.root === null) {
            return
        }

        let n: AVLnode<T> = this.root,
            parent: AVLnode<T> = this.root,
            delNode: AVLnode<T> = null,
            child: AVLnode<T> = this.root

        while (child !== null) {
            parent = n
            n = child
            child = delKey >= n.key ? n.right : n.left
            if (delKey === n.key) {
                delNode = n
            }
        }

        if (delNode !== null) {
            delNode.key = n.key

            child = n.left !== null ? n.left : n.right

            if (this.root.key === delKey) {
                this.root = child
            } else {
                if (parent.left === n) {
                    parent.left = child
                } else {
                    parent.right = child
                }

                this.rebalance(parent)
            }
        }
    }

    treeBalanceString(n: AVLnode<T> = this.root): string {
        if (n !== null) {
            return `${this.treeBalanceString(n.left)} ${n.balance} ${this.treeBalanceString(n.right)}`
        }
        return ""
    }

    toString(n: AVLnode<T> = this.root): string {
        if (n !== null) {
            return `${this.toString(n.left)} ${n.key} ${this.toString(n.right)}`
        }
        return ""
    }


    // private members organized here
    private root: AVLnode<T>

    private rotateLeft(a: AVLnode<T>): AVLnode<T> {
        let b: AVLnode<T> = a.right
        b.parent = a.parent
        a.right = b.left

        if (a.right !== null) {
            a.right.parent = a
        }

        b.left = a
        a.parent = b

        if (b.parent !== null) {
            if (b.parent.right === a) {
                b.parent.right = b
            } else {
                b.parent.left = b
            }
        }

        this.setBalance(a)
        this.setBalance(b)

        return b
    }

    private rotateRight(a: AVLnode<T>): AVLnode<T> {
        let b: AVLnode<T> = a.left
        b.parent = a.parent
        a.left = b.right

        if (a.left !== null) {
            a.left.parent = a
        }

        b.right = a
        a.parent = b

        if (b.parent !== null) {
            if (b.parent.right === a) {
                b.parent.right = b
            } else {
                b.parent.left = b
            }
        }

        this.setBalance(a)
        this.setBalance(b)

        return b
    }

    private rotateLeftThenRight(n: AVLnode<T>): AVLnode<T> {
        n.left = this.rotateLeft(n.left)
        return this.rotateRight(n)
    }

    private rotateRightThenLeft(n: AVLnode<T>): AVLnode<T> {
        n.right = this.rotateRight(n.right)
        return this.rotateLeft(n)
    }

    private rebalance(n: AVLnode<T>): void {
        this.setBalance(n)

        if (n.balance === -2) {
            if(this.height(n.left.left) >= this.height(n.left.right)) {
                n = this.rotateRight(n)
            } else {
                n = this.rotateLeftThenRight(n)
            }
        } else if (n.balance === 2) {
            if(this.height(n.right.right) >= this.height(n.right.left)) {
                n = this.rotateLeft(n)
            } else {
                n = this.rotateRightThenLeft(n)
            }
        }

        if (n.parent !== null) {
            this.rebalance(n.parent)
        } else {
            this.root = n
        }
    }

    private height(n: AVLnode<T>): number {
        if (n === null) {
            return -1
        }
        return 1 + Math.max(this.height(n.left), this.height(n.right))
    }

    private setBalance(n: AVLnode<T>): void {
        n.balance = this.height(n.right) - this.height(n.left)
    }

    public showNodeBalance(n: AVLnode<T>): string {
        if (n !== null) {
            return `${this.showNodeBalance(n.left)} ${n.balance} ${this.showNodeBalance(n.right)}`
        }
        return ""
    }
}

```

