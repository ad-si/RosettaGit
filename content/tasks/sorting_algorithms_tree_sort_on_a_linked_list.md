+++
title = "Sorting algorithms/Tree sort on a linked list"
description = ""
date = 2019-09-02T19:05:03Z
aliases = []
[extra]
id = 17468
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "j",
  "kotlin",
  "ol",
  "phix",
  "racket",
  "scheme",
  "zkl",
]
+++

A '''tree sort''' is a [[wp:sort algorithm|sort algorithm]] that builds a [[wp:binary search tree|binary search tree]] from the keys to be sorted, and then traverses the tree ([[wp:Tree traversal|in-order]]) so that the keys come out in sorted order. Its typical use is when sorting the elements of a stream from a file. Several other sorts would have to load the elements to a temporary data structure, whereas in a tree sort the act of loading the input into a data structure is sorting it.

The tree sort is considered by some to be the faster method to sort a linked list, followed by [[Sorting_algorithms#Quicksort|Quicksort]] and [[Sorting_algorithms#Mergesort|Mergesort]]:
* [http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.31.9981 A Comparative Study of Linked List Sorting Algorithms by Ching-Kuang Shene]
[[Sorting_algorithms#Sediment sort|Sediment sort]], [[Sorting_algorithms#bubble sort|bubble sort]], [[Sorting_algorithms#selection sort|selection sort]] perform very badly.
* http://www.martinbroadhurst.com/sorting-a-linked-list-by-turning-it-into-a-binary-tree.html


'''Task:'''

First, construct a doubly linked list (unsorted).

Then construct a tree in situ: use the prev and next of that list as left and right tree pointers.

Then traverse the tree, in order, and recreate a doubly linked list, again in situ, but of course now in sorted order.


## Go

This is based on the Kotlin entry but has been adjusted to satisfy the revised task description.

```go
package main

import (
    "container/list"
    "fmt"
)

type BinaryTree struct {
    node         int
    leftSubTree  *BinaryTree
    rightSubTree *BinaryTree
}

func (bt *BinaryTree) insert(item int) {
    if bt.node == 0 {
        bt.node = item
        bt.leftSubTree = &BinaryTree{}
        bt.rightSubTree = &BinaryTree{}
    } else if item < bt.node {
        bt.leftSubTree.insert(item)
    } else {
        bt.rightSubTree.insert(item)
    }
}

func (bt *BinaryTree) inOrder(ll *list.List) {
    if bt.node == 0 {
        return
    }
    bt.leftSubTree.inOrder(ll)
    ll.PushBack(bt.node)
    bt.rightSubTree.inOrder(ll)
}
func treeSort(ll *list.List) *list.List {
    searchTree := &BinaryTree{}
    for e := ll.Front(); e != nil; e = e.Next() {
        i := e.Value.(int)
        searchTree.insert(i)
    }
    ll2 := list.New()
    searchTree.inOrder(ll2)
    return ll2
}

func printLinkedList(ll *list.List, f string, sorted bool) {
    for e := ll.Front(); e != nil; e = e.Next() {
        i := e.Value.(int)
        fmt.Printf(f+" ", i)
    }
    if !sorted {
        fmt.Print("-> ")
    } else {
        fmt.Println()
    }
}

func main() {
    sl := []int{5, 3, 7, 9, 1}
    ll := list.New()
    for _, i := range sl {
        ll.PushBack(i)
    }
    printLinkedList(ll, "%d", false)
    lls := treeSort(ll)
    printLinkedList(lls, "%d", true)

    sl2 := []int{'d', 'c', 'e', 'b', 'a'}
    ll2 := list.New()
    for _, c := range sl2 {
        ll2.PushBack(c)
    }
    printLinkedList(ll2, "%c", false)
    lls2 := treeSort(ll2)
    printLinkedList(lls2, "%c", true)
}
```


```txt

5 3 7 9 1 -> 1 3 5 7 9 
d c e b a -> a b c d e 

```



## J


What *is* a sentence in Finnegan's Wake? Let's say that it's all the text leading up to a period, question mark or exclamation point if (and only if) the character is followed by a space or newline. (There are some practical difficulties here - this means, for example, that the first sentence of a chapter includes the chapter heading - but it's good enough for now.)

There's also the issue of how do we want to sort the sentences? Let's say we'll sort them in ascii order without normalization of the text (since that is simplest).

Let's also say that we have prepared a file which contains some sort of ascii rendition of the text. Note that the final result we get here will depend on exactly how that ascii rendition was prepared. But let's just ignore that issue so we can get something working.

Next, we need to think of what kind of tree, there are a great number of kinds of trees, and they can be categorized in many different ways. For example, a directory tree is almost never a balanced binary tree. (Note that a linked list is a kind of a tree - an extremely tall and skinny unbalanced tree, but a tree nonetheless - and a binary tree at that. Then again, note that efficiency claims in general are specious, because efficient for one purpose tends to be inefficient for many other purposes.) Since we are going for efficiency here, we will implement a short, fat tree (let's call that "efficient use of the programmer's time" or something like that...). Specifically, we'll be implementing a one level deep tree which happens to have 14961 leaves connected directly to the root node. (Edit: task description has been changed to mandate a specific binary tree. But we are going to ignore that here, since the consequence would be several orders of magnitude slowdown, and a lot of extra code to write. That kind of detail can be useful in an educational setting, and in some technology settings, but it would cause real problems here.)

Simplicity is a virtue, right?

Finally, there's the matter of counting swaps. Let's define our swap count as the minimal number of swaps which would be needed to produce our sorted result.

With these choices, the task becomes:


```J
   finn=: fread '~user/temp/wake/finneganswake.txt'
   sentences=: (<;.2~ '. '&E.@rplc&(LF,' !.?.')) finn
   #sentences
14961
      +/<:#@>C./:sentences
14945
```


We have to swap almost every sentence, but 16 of them can be sorted "for free" with the swaps of the other sentences.

For that matter, inspecting the lengths of the cycles formed by the minimal arrangements of swaps...


```J
   /:~ #@>C./:sentences
1 1 2 2 4 9 12 25 32 154 177 570 846 935 1314 10877
```


... we can see that two of the sentences were fine right where they start out. Let's see what they are:


```J
   ;:inv (#~(= /:~))sentences
 Very, all
   fourlike tellt.  What tyronte power!
```


So now you know.

(Processing time here is negligible - other than the time needed to fetch a copy of the book and render it as plain text ascii - but if we were careful to implement the efficiency recommendations of this task more in the spirit of whatever the task is presumably implying, we could probably increase the processing time by several orders of magnitude.)

So... ok... let's do this "right" (which is to say according to the current task specification, as opposed to the task specification that was present for the early drafts - though, perhaps, using Finnegan's Wake as a data set encourages a certain degree of ... informality?).

Anyways, here we go:


```J
left=: i.0
right=: i.0
data=: i.0

insert=:3 :0"0
  k=. 0
  assert. (left =&# right) * (left =&# data)
  if. 0<#data do.
    while. k<#data do.
      if. y=k{data do.return.end.
      n=. k
      if. y<k{data do.
        k=. k{".p=.'left'
      else.
        k=. k{".p=.'right'
      end.
    end.
    (p)=:(#data) n} ".p
  end.
  left=:left, _
  right=:right, _
  data=:data,y
  i.0 0
)

flatten=:3 :0
  extract 0
)

extract=:3 :0
  if. y>:#data do.'' return. end.
  (extract y{left),(y{data),extract y{right
)
```


This could be wrapped differently, but it's adequate for this task.

Example use would be something like:

```j
   insert sentences
   extract''
```


But task's the current url for Finnegan's Wake does not point at flat text and constructing such a thing would be a different task...


## Kotlin

As I can't be bothered to download Finnegan's Wake and deal with the ensuing uncertainties, I've contented myself by following a similar approach to the Racket and Scheme entries:

```scala
// version 1.1.51

import java.util.LinkedList

class BinaryTree<T : Comparable<T>> {
    var node: T? = null
    lateinit var leftSubTree: BinaryTree<T>
    lateinit var rightSubTree: BinaryTree<T>

    fun insert(item: T) {
        if (node == null) {
            node = item
            leftSubTree = BinaryTree<T>()
            rightSubTree = BinaryTree<T>()
        }
        else if (item < node as T) { 
            leftSubTree.insert(item)
        }
        else {
            rightSubTree.insert(item)
        }
    }

    fun inOrder() {
        if (node == null) return
        leftSubTree.inOrder()
        print("$node ")
        rightSubTree.inOrder()
    }
}

fun <T : Comparable<T>> LinkedList<T>.treeSort() {
    val searchTree = BinaryTree<T>()
    for (item in this) searchTree.insert(item)
    print("${this.joinToString(" ")} -> ")
    searchTree.inOrder()
    println()
}

fun main(args: Array<String>) {
    val ll = LinkedList(listOf(5, 3, 7, 9, 1))
    ll.treeSort()
    val ll2 = LinkedList(listOf('d', 'c', 'e', 'b' , 'a'))
    ll2.treeSort()
}
```


```txt

5 3 7 9 1 -> 1 3 5 7 9 
d c e b a -> a b c d e 

```



## Ol

Ol has builtin sorted key-value trees named "ff". We converting list into ff and back again as already sorted list. Only values (small integers, constants) and symbols are allowed.


```scheme

(define (tree-sort l)
   (map car (ff->list
      (fold (lambda (ff p)
               (put ff p #t))
         #empty l))))

(print (tree-sort '(5 3 7 9 1)))

```

```txt

(1 3 5 7 9)

```



## Phix

```Phix
enum KEY,LEFT,RIGHT
function tree_insert(object node, item)
    if node=NULL then
        node = {item,NULL,NULL}
    elsif item<node[KEY] then
        node[LEFT] = tree_insert(node[LEFT],item)
    else
        node[RIGHT] = tree_insert(node[RIGHT],item)
    end if
    return node
end function

function inOrder(object node)
    sequence res = {}
    if node!=NULL then
        res = inOrder(node[LEFT])
        res &= node[KEY]
        res &= inOrder(node[RIGHT])
    end if
    return res
end function
 
procedure treeSort(sequence s)
    object tree = NULL
    for i=1 to length(s) do tree = tree_insert(tree,s[i]) end for
    pp({s," => ",inOrder(tree)})
end procedure
 
treeSort({5, 3, 7, 9, 1})
treeSort("dceba")
```

```txt

{"dceba", " => ", "abcde"}

```



###  version 2 

Following my idea of a revised task description, see talk page.

```Phix
-- doubly linked list:
enum NEXT,PREV,DATA
constant empty_dll = {{1,1}}
sequence dll
 
procedure insert_after(object data, integer pos=1)
integer prv = dll[pos][PREV]
    dll = append(dll,{pos,prv,data})
    if prv!=0 then
        dll[prv][NEXT] = length(dll)
    end if
    dll[pos][PREV] = length(dll)
end procedure
 
procedure append_node(integer node)
-- (like insert_after, but in situ rebuild)
integer prev = dll[1][PREV]
    dll[node][NEXT] = 1
    dll[node][PREV] = prev
    dll[prev][NEXT] = node
    dll[1][PREV] = node
end procedure

function dll_collect()
    sequence res = ""
    integer idx = dll[1][NEXT]
    while idx!=1 do
        res = append(res,dll[idx][DATA])
        idx = dll[idx][NEXT]
    end while
    return res
end function

-- tree:
enum LEFT,RIGHT,KEY

function tree_insert(integer root, object item, integer idx)
    if root=NULL then
        return idx
    else
        integer branch = iff(item<dll[root][KEY]?LEFT:RIGHT)
        dll[root][branch] = tree_insert(dll[root][branch],item,idx)
        return root
    end if
end function

procedure traverse(integer node)
    if node!=NULL then
        traverse(dll[node][LEFT])
        integer right = dll[node][RIGHT]
        append_node(node)
        traverse(right)
    end if
end procedure

bool detailed = true
procedure treeSort()
    if detailed then
        ?{"initial dll",dll}
    end if
    object tree = NULL
    integer idx = dll[1][NEXT]
    while idx!=1 do
        integer next = dll[idx][NEXT]
        dll[idx][NEXT] = NULL
        dll[idx][PREV] = NULL
        tree = tree_insert(tree,dll[idx][DATA],idx)
        idx = next
    end while
    dll[1] = {tree,0} -- (0 is meaningless, but aligns output)
    if detailed then
        ?{"tree insitu",dll}
    end if
    dll[1] = empty_dll[1]
    traverse(tree)
    if detailed then
        ?{"rebuilt dll",dll}
    end if
end procedure
 
procedure test(sequence s)
    dll = empty_dll
    for i=1 to length(s) do insert_after(s[i]) end for
    ?{"unsorted",dll_collect()}
    treeSort()
    ?{"sorted",dll_collect()}
end procedure

test({5, 3, 7, 9, 1})
detailed = false
test("dceba")
test({"d","c","e","b","a"})
```

```txt

{"unsorted",{5,3,7,9,1}}
{"initial dll",{{2,6},{3,1,5},{4,2,3},{5,3,7},{6,4,9},{1,5,1}}}
{"tree insitu",{{2,0},{3,4,5},{6,0,3},{0,5,7},{0,0,9},{0,0,1}}}
{"rebuilt dll",{{6,5},{4,3,5},{2,6,3},{5,2,7},{1,4,9},{3,1,1}}}
{"sorted",{1,3,5,7,9}}
{"unsorted","dceba"}
{"sorted","abcde"}
{"unsorted",{"d","c","e","b","a"}}
{"sorted",{"a","b","c","d","e"}}

```



## Racket

{{trans|Scheme}} -- this implementation illustrates differences in identifiers and syntaxes of Scheme and Racket's <code>match-lambda</code> family. [http://docs.racket-lang.org/reference/match.html <code>racket/match</code> documented here].


```racket
#lang racket/base
(require racket/match)

(define insert
  ;; (insert key tree)
  (match-lambda**
   [(x '())         `(() ,x ())]
   [(x '(() () ())) `(() ,x ())]
   [(x `(,l ,k ,r)) #:when (<= x k) `(,(insert x l) ,k ,r)]
   [(x `(,l ,k ,r)) `(,l ,k ,(insert x r))]
   [(_ _) "incorrect arguments or broken tree"]))

(define in-order
  ;; (in-order tree)
  (match-lambda
    [`(() ,x ()) `(,x)]
    [`(,l ,x ())  (append (in-order l) `(,x))]
    [`(() ,x ,r)  (append `(,x) (in-order r))]
    [`(,l ,x ,r)  (append (in-order l) `(,x) (in-order r))]
    [_ "incorrect arguments or broken tree"]))

(define (tree-sort lst)
  (define tree-sort-itr
    (match-lambda**
      [(x `())        (in-order x)]
      [(x `(,a . ,b)) (tree-sort-itr (insert a x) b)] 
      [(_ _) "incorrect arguments or broken tree"]))
  (tree-sort-itr '(() () ()) lst))

(tree-sort '(5 3 7 9 1))
```


```txt
'(1 3 5 7 9)
```



## Scheme

The following implements a sorting algorithm that takes a linked list, puts each key into an unbalanced binary tree and returns an in-order traversal of the tree.
```Scheme
(use matchable)

(define insert
  ;; (insert key tree)
  (match-lambda*
   [(x ())         `(() ,x ()) ]
   [(x (() () ())) `(() ,x ()) ]
   [(x (l k r))
    (=> continue)
    (if (<= x k)
	`(,(insert x l) ,k ,r)
	(continue)) ]
   [(x (l k r)) `(,l ,k ,(insert x r)) ]
   [_ "incorrect arguments or broken tree" ]))

(define in-order
  ;; (in-order tree)
  (match-lambda
   [(() x ()) `(,x)]
   [(l x ())  (append (in-order l) `(,x))]
   [(() x r)  (append `(,x) (in-order r))]
   [(l x r)   (append (in-order l) `(,x) (in-order r))]
   [_ "incorrect arguments or broken tree" ]))

(define (tree-sort lst)
  (define tree-sort-itr
    (match-lambda*
     [(x ())      (in-order x)]
     [(x (a . b)) (tree-sort-itr (insert a x) b)] 
     [_ "incorrect arguments or broken tree" ]))
  (tree-sort-itr '(() () ()) lst))
```

Usage: 
```Scheme> #;2
 (tree-sort '(5 3 7 9 1))
(1 3 5 7 9)
```



## zkl

This code reads a file [of source code] line by line, and builds a binary tree of the first word of each line. Then prints the sorted list.

```zkl
class Node{
   var left,right,value;
   fcn init(value){ self.value=value; }
}
class Tree{
   var root;
   fcn add(value){
      if(not root){ root=Node(value); return(self); }
      fcn(node,value){
	 if(not node) return(Node(value));
	 if(value!=node.value){  // don't add duplicate values
	    if(value<node.value) node.left =self.fcn(node.left, value);
	    else                 node.right=self.fcn(node.right,value);
	 }
	 node
      }(root,value);
      return(self);
   }
   fcn walker{ Utils.Generator(walk,root); }
   fcn walk(node){	// in order traversal
      if(node){
         self.fcn(node.left);
         vm.yield(node.value);
         self.fcn(node.right);
      }
   }
}
```


```zkl
tree:=Tree();
File("bbb.zkl").pump(tree.add,fcn(line){  // 5,000 lines to 660 words
   line.split(" ")[0].strip();	// take first word
});

foreach word in (tree){ println(word) }
```

```txt

...
Atomic.sleep(0.5);
Atomic.sleep(100000);
Atomic.sleep(2);
Atomic.waitFor(fcn{
Boyz:=Boys.pump(D(),fcn([(b,gs)]){
Compiler.Compiler.compileText(code)();
...

```

