+++
title = "Fibonacci heap"
description = ""
date = 2018-12-29T19:13:37Z
aliases = []
[extra]
id = 21508
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
{{Wikipedia|Fibonacci heap}}


;Task:
* Implement queue operations for '''Fibonacci heaps'''. Where H is heap, x node with data value, k integer.
*Operations:
** MakeHeap() - create new empty Fibonacci heap
** Insert(H,x) - insert new element x into heap H 
** Union(H1, H2) - union heap H1 and heap H2
** Minimum(H) - return minimum value from heap H
** ExtractMin(H) - (or DeleteMin(H)) - return minimum value from heap H and remove it from heap
** DecreaseKey(H,x,k) - decrease value of element x in heap H to value k
** Delete(H,x) - remove element x from heap H





## C++


```cpp>template <class V
 class FibonacciHeap;

template <class V> struct node {
private:
	node<V>* prev;
	node<V>* next;
	node<V>* child;
	node<V>* parent;
	V value;
	int degree;
	bool marked;
public:
	friend class FibonacciHeap<V>;
	node<V>* getPrev() {return prev;}
	node<V>* getNext() {return next;}
	node<V>* getChild() {return child;}
	node<V>* getParent() {return parent;}
	V getValue() {return value;}
	bool isMarked() {return marked;}

	bool hasChildren() {return child;}
	bool hasParent() {return parent;}
};

template <class V> class FibonacciHeap {
protected:
	node<V>* heap;
public:

	FibonacciHeap() {
		heap=_empty();
	}
	virtual ~FibonacciHeap() {
		if(heap) {
			_deleteAll(heap);
		}
	}
	node<V>* insert(V value) {
		node<V>* ret=_singleton(value);
		heap=_merge(heap,ret);
		return ret;
	}
	void merge(FibonacciHeap& other) {
		heap=_merge(heap,other.heap);
		other.heap=_empty();
	}

	bool isEmpty() {
		return heap==NULL;
	}

	V getMinimum() {
		return heap->value;
	}

	V removeMinimum() {
		node<V>* old=heap;
		heap=_removeMinimum(heap);
		V ret=old->value;
		delete old;
		return ret;
	}

	void decreaseKey(node<V>* n,V value) {
		heap=_decreaseKey(heap,n,value);
	}

	node<V>* find(V value) {
		return _find(heap,value);
	}
private:
	node<V>* _empty() {
		return NULL;
	}

	node<V>* _singleton(V value) {
		node<V>* n=new node<V>;
		n->value=value;
		n->prev=n->next=n;
		n->degree=0;
		n->marked=false;
		n->child=NULL;
		n->parent=NULL;
		return n;
	}

	node<V>* _merge(node<V>* a,node<V>* b) {
		if(a==NULL)return b;
		if(b==NULL)return a;
		if(a->value>b->value) {
			node<V>* temp=a;
			a=b;
			b=temp;
		}
		node<V>* an=a->next;
		node<V>* bp=b->prev;
		a->next=b;
		b->prev=a;
		an->prev=bp;
		bp->next=an;
		return a;
	}

	void _deleteAll(node<V>* n) {
		if(n!=NULL) {
			node<V>* c=n;
			do {
				node<V>* d=c;
				c=c->next;
				_deleteAll(d->child);
				delete d;
			} while(c!=n);
		}
	}
	
	void _addChild(node<V>* parent,node<V>* child) {
		child->prev=child->next=child;
		child->parent=parent;
		parent->degree++;
		parent->child=_merge(parent->child,child);
	}

	void _unMarkAndUnParentAll(node<V>* n) {
		if(n==NULL)return;
		node<V>* c=n;
		do {
			c->marked=false;
			c->parent=NULL;
			c=c->next;
		}while(c!=n);
	}

	node<V>* _removeMinimum(node<V>* n) {
		_unMarkAndUnParentAll(n->child);
		if(n->next==n) {
			n=n->child;
		} else {
			n->next->prev=n->prev;
			n->prev->next=n->next;
			n=_merge(n->next,n->child);
		}
		if(n==NULL)return n;
		node<V>* trees[64]={NULL};
		
		while(true) {
			if(trees[n->degree]!=NULL) {
				node<V>* t=trees[n->degree];
				if(t==n)break;
				trees[n->degree]=NULL;
				if(n->value<t->value) {
					t->prev->next=t->next;
					t->next->prev=t->prev;
					_addChild(n,t);
				} else {
					t->prev->next=t->next;
					t->next->prev=t->prev;
					if(n->next==n) {
						t->next=t->prev=t;
						_addChild(t,n);
						n=t;
					} else {
						n->prev->next=t;
						n->next->prev=t;
						t->next=n->next;
						t->prev=n->prev;
						_addChild(t,n);
						n=t;
					}
				}
				continue;
			} else {
				trees[n->degree]=n;
			}
			n=n->next;
		}
		node<V>* min=n;
		do {
			if(n->value<min->value)min=n;
			n=n->next;
		} while(n!=n);
		return min;
	}

	node<V>* _cut(node<V>* heap,node<V>* n) {
		if(n->next==n) {
			n->parent->child=NULL;
		} else {
			n->next->prev=n->prev;
			n->prev->next=n->next;
			n->parent->child=n->next;
		}
		n->next=n->prev=n;
		n->marked=false;
		return _merge(heap,n);
	}

	node<V>* _decreaseKey(node<V>* heap,node<V>* n,V value) {
		if(n->value<value)return heap;
		n->value=value;
		if(n->value<n->parent->value) {
			heap=_cut(heap,n);
			node<V>* parent=n->parent;
			n->parent=NULL;
			while(parent!=NULL && parent->marked) {
				heap=_cut(heap,parent);
				n=parent;
				parent=n->parent;
				n->parent=NULL;
			}
			if(parent!=NULL && parent->parent!=NULL)parent->marked=true;
		}
		return heap;
	}

	node<V>* _find(node<V>* heap,V value) {
		node<V>* n=heap;
		if(n==NULL)return NULL;
		do {
			if(n->value==value)return n;
			node<V>* ret=_find(n->child,value);
			if(ret)return ret;
			n=n->next;
		}while(n!=heap);
		return NULL;
	}
};
```



## Go

A package.  Implementation follows Fredman and Tarjan's 1987 paper.

```go
package fib

import "fmt"

type Value interface {
    LT(Value) bool
}

type Node struct {
    value      Value
    parent     *Node
    child      *Node
    prev, next *Node
    rank       int
    mark       bool
}

func (n Node) Value() Value { return n.value }

type Heap struct{ *Node }

// task requirement
func MakeHeap() *Heap { return &Heap{} }

// task requirement
func (h *Heap) Insert(v Value) *Node {
    x := &Node{value: v}
    if h.Node == nil {
        x.next = x
        x.prev = x
        h.Node = x
    } else {
        meld1(h.Node, x)
        if x.value.LT(h.value) {
            h.Node = x
        }
    }
    return x
}

func meld1(list, single *Node) {
    list.prev.next = single
    single.prev = list.prev
    single.next = list
    list.prev = single
}

// task requirement
func (h *Heap) Union(h2 *Heap) {
    switch {
    case h.Node == nil:
        *h = *h2
    case h2.Node != nil:
        meld2(h.Node, h2.Node)
        if h2.value.LT(h.value) {
            *h = *h2
        }
    }
    h2.Node = nil
}

func meld2(a, b *Node) {
    a.prev.next = b
    b.prev.next = a
    a.prev, b.prev = b.prev, a.prev
}

// task requirement
func (h Heap) Minimum() (min Value, ok bool) {
    if h.Node == nil {
        return
    }
    return h.value, true
}

// task requirement
func (h *Heap) ExtractMin() (min Value, ok bool) {
    if h.Node == nil {
        return
    }
    min = h.value
    roots := map[int]*Node{}
    add := func(r *Node) {
        r.prev = r
        r.next = r
        for {
            x, ok := roots[r.rank]
            if !ok {
               break
            }
            delete(roots, r.rank)
            if x.value.LT(r.value) {
                r, x = x, r
            }
            x.parent = r
            x.mark = false
            if r.child == nil {
                x.next = x
                x.prev = x
                r.child = x
            } else {
                meld1(r.child, x)
            }
            r.rank++
        }
        roots[r.rank] = r
    }
    for r := h.next; r != h.Node; {
        n := r.next
        add(r)
        r = n
    }
    if c := h.child; c != nil {
        c.parent = nil
        r := c.next
        add(c)
        for r != c {
            n := r.next
            r.parent = nil
            add(r)
            r = n
        }
    }
    if len(roots) == 0 {
        h.Node = nil
        return min, true
    }
    var mv *Node
    var d int
    for d, mv = range roots {
        break
    }
    delete(roots, d)
    mv.next = mv
    mv.prev = mv
    for _, r := range roots {
        r.prev = mv
        r.next = mv.next
        mv.next.prev = r
        mv.next = r
        if r.value.LT(mv.value) {
            mv = r
        }
    }
    h.Node = mv
    return min, true
}

// task requirement
func (h *Heap) DecreaseKey(n *Node, v Value) error {
    if n.value.LT(v) {
        return fmt.Errorf("DecreaseKey new value greater than existing value")
    }
    n.value = v
    if n == h.Node {
        return nil
    }
    p := n.parent
    if p == nil {
        if v.LT(h.value) {
            h.Node = n
        }
        return nil
    }
    h.cutAndMeld(n)
    return nil
}

func (h Heap) cut(x *Node) {
    p := x.parent
    p.rank--
    if p.rank == 0 {
        p.child = nil
    } else {
        p.child = x.next
        x.prev.next = x.next
        x.next.prev = x.prev
    }
    if p.parent == nil {
        return
    }
    if !p.mark {
        p.mark = true
        return
    }
    h.cutAndMeld(p)
}

func (h Heap) cutAndMeld(x *Node) {
    h.cut(x)
    x.parent = nil
    meld1(h.Node, x)
}

// task requirement
func (h *Heap) Delete(n *Node) {
    p := n.parent
    if p == nil {
        if n == h.Node {
            h.ExtractMin()
            return
        }
        n.prev.next = n.next
        n.next.prev = n.prev
    } else {
        h.cut(n)
    }
    c := n.child
    if c == nil {
        return
    }
    for {
        c.parent = nil
        c = c.next
        if c == n.child {
            break
        }
    }
    meld2(h.Node, c)
}

// adapted from task "Visualize a tree"
func (h Heap) Vis() {
    if h.Node == nil {
        fmt.Println("<empty>")
        return
    }
    var f func(*Node, string)
    f = func(n *Node, pre string) {
        pc := "│ "
        for x := n; ; x = x.next {
            if x.next != n {
                fmt.Print(pre, "├─")
            } else {
                fmt.Print(pre, "└─")
                pc = "  "
            }
            if x.child == nil {
                fmt.Println("╴", x.value)
            } else {
                fmt.Println("┐", x.value)
                f(x.child, pre+pc)
            }
            if x.next == n {
                break
            }
        }
    }
    f(h.Node, "")
}
```

A demonstration:

```go
package main

import (
    "fmt"

    "fib"
)

type str string

func (s str) LT(t fib.Value) bool { return s < t.(str) }

func main() {
    fmt.Println("MakeHeap:")
    h := fib.MakeHeap()
    h.Vis()

    fmt.Println("\nInsert:")
    h.Insert(str("cat"))
    h.Vis()

    fmt.Println("\nUnion:")
    h2 := fib.MakeHeap()
    h2.Insert(str("rat"))
    h.Union(h2)
    h.Vis()

    fmt.Println("\nMinimum:")
    m, _ := h.Minimum()
    fmt.Println(m)

    fmt.Println("\nExtractMin:")
    // add a couple more items to demonstrate parent-child linking that
    // happens on delete min.
    h.Insert(str("bat"))
    x := h.Insert(str("meerkat")) // save x for decrease key and delete demos
    m, _ = h.ExtractMin()
    fmt.Printf("(extracted %v)\n", m)
    h.Vis()

    fmt.Println("\nDecreaseKey:")
    h.DecreaseKey(x, str("gnat"))
    h.Vis()
    fmt.Println("\nDelete:")
    // add yet a couple more items to show how F&T's original delete was
    // lazier than CLRS's delete.
    h.Insert(str("bobcat"))
    h.Insert(str("bat"))
    fmt.Printf("(deleting %v)\n", x.Value())
    h.Delete(x)
    h.Vis()
}
```

{{out}}

```txt

MakeHeap:
<empty>

Insert:
└─╴ cat

Union:
├─╴ cat
└─╴ rat

Minimum:
cat

ExtractMin:
(extracted bat)
├─┐ cat
│ └─╴ rat
└─╴ meerkat

DecreaseKey:
├─┐ cat
│ └─╴ rat
└─╴ gnat

Delete:
(deleting gnat)
├─╴ bat
├─╴ bobcat
└─┐ cat
  └─╴ rat

```



## Kotlin

{{trans|Go}}

```scala
// version 1.2.21

class Node<V : Comparable<V>>(var value: V) {
    var parent: Node<V>? = null
    var child:  Node<V>? = null
    var prev:   Node<V>? = null
    var next:   Node<V>? = null
    var rank = 0
    var mark = false

    fun meld1(node: Node<V>) {
        this.prev?.next = node
        node.prev = this.prev
        node.next = this
        this.prev = node
    }

    fun meld2(node: Node<V>) {
        this.prev?.next = node
        node.prev?.next = this
        val temp = this.prev
        this.prev = node.prev
        node.prev = temp
    }
}

// task requirement
fun <V: Comparable<V>> makeHeap() = FibonacciHeap<V>()

class FibonacciHeap<V: Comparable<V>>(var node: Node<V>? = null) {

    // task requirement
    fun insert(v: V): Node<V> {
        val x = Node(v)
        if (this.node == null) {
            x.next = x
            x.prev = x
            this.node = x
        }
        else {
            this.node!!.meld1(x)
            if (x.value < this.node!!.value) this.node = x
        }
        return x
    }

    // task requirement
    fun union(other: FibonacciHeap<V>) {
        if (this.node == null) {
            this.node = other.node
        }
        else if (other.node != null) {
            this.node!!.meld2(other.node!!)
            if (other.node!!.value < this.node!!.value) this.node = other.node
        }
        other.node = null
    }

    // task requirement
    fun minimum(): V? = this.node?.value

    // task requirement
    fun extractMin(): V? {
        if (this.node == null) return null
        val min = minimum()
        val roots = mutableMapOf<Int, Node<V>>()

        fun add(r: Node<V>) {
            r.prev = r
            r.next = r
            var rr = r
            while (true) {
                var x = roots[rr.rank] ?: break
                roots.remove(rr.rank)
                if (x.value < rr.value) {
                    val t = rr
                    rr = x
                    x = t
                }
                x.parent = rr
                x.mark = false
                if (rr.child == null) {
                    x.next = x
                    x.prev = x
                    rr.child = x
                }
                else {
                    rr.child!!.meld1(x)
                }
                rr.rank++
            }
            roots[rr.rank] = rr
        }

        var r = this.node!!.next
        while (r != this.node) {
            val n = r!!.next
            add(r)
            r = n
        }
        val c = this.node!!.child
        if (c != null) {
            c.parent = null
            var rr = c.next!!
            add(c)
            while (rr != c) {
                val n = rr.next!!
                rr.parent = null
                add(rr)
                rr = n
            }
        }
        if (roots.isEmpty()) {
            this.node = null
            return min
        }
        val d = roots.keys.first()
        var mv = roots[d]!!
        roots.remove(d)
        mv.next = mv
        mv.prev = mv
        for ((_, rr) in roots) {
            rr.prev = mv
            rr.next = mv.next
            mv.next!!.prev = rr
            mv.next = rr
            if (rr.value < mv.value) mv = rr
        }
        this.node = mv
        return min
    }

    // task requirement
    fun decreaseKey(n: Node<V>, v: V) {
        require (n.value > v) {
            "In 'decreaseKey' new value greater than existing value"
        }
        n.value = v
        if (n == this.node) return
        val p = n.parent
        if (p == null) {
            if (v < this.node!!.value) this.node = n
            return
        }
        cutAndMeld(n)
    }

    private fun cut(x: Node<V>) {
        val p = x.parent
        if (p == null) return
        p.rank--
        if (p.rank == 0) {
            p.child = null
        }
        else {
            p.child = x.next
            x.prev?.next = x.next
            x.next?.prev = x.prev
        }
        if (p.parent == null) return
        if (!p.mark) {
            p.mark = true
            return
        }
        cutAndMeld(p)
    }

    private fun cutAndMeld(x: Node<V>) {
        cut(x)
        x.parent = null
        this.node?.meld1(x)
    }

    // task requirement
    fun delete(n: Node<V>) {
        val p = n.parent
        if (p == null) {
            if (n == this.node) {
                extractMin()
                return
            }
            n.prev?.next = n.next
            n.next?.prev = n.prev
        }
        else {
            cut(n)
        }
        var c = n.child
        if (c == null) return
        while (true) {
            c!!.parent = null
            c = c.next
            if (c == n.child) break
        }
        this.node?.meld2(c!!)
    }

    fun visualize() {
        if (this.node == null) {
            println("<empty>")
            return
        }

        fun f(n: Node<V>, pre: String) {
            var pc = "│ "
            var x = n
            while (true) {
                if (x.next != n) {
                    print("$pre├─")
                }
                else {
                    print("$pre└─")
                    pc = "  "
                }
                if (x.child == null) {
                    println("╴ ${x.value}")
                }
                else {
                    println("┐ ${x.value}")
                    f(x.child!!, pre + pc)
                }
                if (x.next == n) break
                x = x.next!!
            }
        }
        f(this.node!!, "")
    }
}

fun main(args: Array<String>) {
    println("MakeHeap:")
    val h = makeHeap<String>()
    h.visualize()

    println("\nInsert:")
    h.insert("cat")
    h.visualize()

    println("\nUnion:")
    val h2 = makeHeap<String>()
    h2.insert("rat")
    h.union(h2)
    h.visualize()

    println("\nMinimum:")
    var m = h.minimum()
    println(m)

    println("\nExtractMin:")
    // add a couple more items to demonstrate parent-child linking that
    // happens on delete min.
    h.insert("bat")
    val x = h.insert("meerkat")  // save x for decrease key and delete demos.
    m = h.extractMin()
    println("(extracted $m)")
    h.visualize()

    println("\nDecreaseKey:")
    h.decreaseKey(x, "gnat")
    h.visualize()

    println("\nDelete:")
    // add a couple more items.
    h.insert("bobcat")
    h.insert("bat")
    println("(deleting ${x.value})")
    h.delete(x)
    h.visualize()
}
```


{{out}}

```txt

MakeHeap:
<empty>

Insert:
└─╴ cat

Union:
├─╴ cat
└─╴ rat

Minimum:
cat

ExtractMin:
(extracted bat)
├─┐ cat
│ └─╴ rat
└─╴ meerkat

DecreaseKey:
├─┐ cat
│ └─╴ rat
└─╴ gnat

Delete:
(deleting gnat)
├─╴ bat
├─╴ bobcat
└─┐ cat
  └─╴ rat

```



## Phix

{{trans|Go}}

```Phix
enum VALUE, PARENT, CHILD, PREV, NEXT, RANK, MARK, NODELEN=$

function new_node()
    return repeat(NULL,NODELEN)
end function

sequence nodes = {}
integer freelist = NULL

function new_slot()
    integer res
    if freelist!=NULL then
        res = freelist
        freelist = nodes[freelist]
        nodes[freelist] = NULL
    else
        nodes = append(nodes,NULL)
        res = length(nodes)
    end if
    return res
end function

procedure release_slot(integer n)
    nodes[n] = freelist
    freelist = n
end procedure

-- task requirement
function MakeHeap()
    return new_slot()
end function
 
procedure meld1(integer list, single)
    nodes[nodes[list][PREV]][NEXT] = single
    nodes[single][PREV] = nodes[list][PREV]
    nodes[single][NEXT] = list
    nodes[list][PREV] = single
end procedure

-- task requirement
function Insert(integer h, object v)
    integer n = 0
    sequence x = new_node()
    x[VALUE] = v
    if nodes[h] == NULL then
        x[NEXT] = h
        x[PREV] = h
        nodes[h] = x
    else
        n = new_slot()
        nodes[n] = x
        meld1(h, n)
        if nodes[n][VALUE]<nodes[h][VALUE] then
            h = n
        end if
    end if
    return {h,n}
end function
 
procedure meld2(integer a, b)
    nodes[nodes[a][PREV]][NEXT] = b
    nodes[nodes[b][PREV]][NEXT] = a
    {nodes[a][PREV], nodes[b][PREV]} = {nodes[b][PREV], nodes[a][PREV]}
end procedure

-- task requirement
function Union(integer h, h2)
    if nodes[h] == NULL then
        h = h2
    elsif nodes[h2] != NULL then
        meld2(h, h2)
        if nodes[h2][VALUE]<nodes[h][VALUE] then
            h = h2
        end if
    else
        release_slot(h2)
    end if
    return {h,NULL} -- (h2:=NULL implied)
end function
 
-- task requirement
function Minimum(integer h)
    if nodes[h] == NULL then
        return {"<none>",false}
    end if
    return {nodes[h][VALUE], true}
end function

procedure add_roots(integer r, integer roots)
    nodes[r][PREV] = r
    nodes[r][NEXT] = r
    while true do
        integer node = getd_index(nodes[r][RANK],roots)
        if node=NULL then exit end if
        integer x = getd_by_index(node,roots)
        deld(nodes[r][RANK],roots)
        if nodes[x][VALUE]<nodes[r][VALUE] then
            {r, x} = {x, r}
        end if
        nodes[x][PARENT] = r
        nodes[x][MARK] = false
        if nodes[r][CHILD] == NULL then
            nodes[x][NEXT] = x
            nodes[x][PREV] = x
            nodes[r][CHILD] = x
        else
            meld1(nodes[r][CHILD], x)
        end if
        nodes[r][RANK] += 1
    end while
    setd(nodes[r][RANK],r,roots)
end procedure
 
-- task requirement
function ExtractMin(integer h)
    if nodes[h] == NULL then
        return {h,"<none>",false}
    end if
    object minimum = nodes[h][VALUE]
    integer roots = new_dict()
    integer r = nodes[h][NEXT], n
    while r != h do
        n := nodes[r][NEXT]
        add_roots(r,roots)
        r = n
    end while
    integer c = nodes[h][CHILD]
    if c != NULL then
        nodes[c][PARENT] = NULL
        r := nodes[c][NEXT]
        add_roots(c,roots)
        while r != c do
            n := nodes[r][NEXT]
            nodes[r][PARENT] = NULL
            add_roots(r,roots)
            r = n
        end while
    end if
    if dict_size(roots) == 0 then
        destroy_dict(roots)
        return {NULL, minimum, true}
    end if
    integer d = getd_partial_key(0,roots)
    integer mv = getd(d,roots)
    deld(d,roots)
    nodes[mv][NEXT] = mv
    nodes[mv][PREV] = mv
    sequence rs = getd_all_keys(roots)
    for i=1 to length(rs) do
        r = getd(rs[i],roots)
        nodes[r][PREV] = mv
        nodes[r][NEXT] = nodes[mv][NEXT]
        nodes[nodes[mv][NEXT]][PREV] = r
        nodes[mv][NEXT] = r
        if nodes[r][VALUE]<nodes[mv][VALUE] then
            mv = r
        end if
    end for
    h = mv
    destroy_dict(roots)
    return {h, minimum, true}
end function

procedure cut_and_meld(integer h, x, bool meld)
    integer p := nodes[x][PARENT]
    nodes[p][RANK] -= 1
    if nodes[p][RANK] == 0 then
        nodes[p][CHILD] = NULL
    else
        nodes[p][CHILD] = nodes[x][NEXT]
        nodes[nodes[x][PREV]][NEXT] = nodes[x][NEXT]
        nodes[nodes[x][NEXT]][PREV] = nodes[x][PREV]
    end if
    if nodes[p][PARENT] == NULL then
        return
    end if
    if not nodes[p][MARK] then
        nodes[p][MARK] = true
        return
    end if
    cut_and_meld(h,p,true)
    if meld then
        nodes[x][PARENT] = NULL
        meld1(h, x)
    end if
end procedure

-- task requirement
function DecreaseKey(integer h, n, object v)
    if nodes[n][VALUE]<v then
        crash("DecreaseKey new value greater than existing value")
    end if
    nodes[n][VALUE] = v
    if n!=h then
        integer p := nodes[n][PARENT]
        if p == NULL then
            if v<nodes[h][VALUE] then
                h = n
            end if
        else
            cut_and_meld(h,n,true)
        end if
    end if
    return h
end function

-- task requirement
function Delete(integer h, n)
    integer p := nodes[n][PARENT]
    if p == NULL then
        if n == h then
            {h} = ExtractMin(h)
            return h
        end if
        nodes[nodes[n][PREV]][NEXT] = nodes[n][NEXT]
        nodes[nodes[n][NEXT]][PREV] = nodes[n][PREV]
    else
        cut_and_meld(h,n,false)
    end if
    integer c := nodes[n][CHILD]
    if c != NULL then
        while true do
            nodes[c][PARENT] = NULL
            c = nodes[c][NEXT]
            if c == nodes[n][CHILD] then
                exit
            end if
        end while
        meld2(h, c)
    end if
    return h
end function
 
constant W=platform()=WINDOWS,
         Horizontal = iff(W?#C4:'-'),
         Vertical   = iff(W?#B3:'|'),
         sBtmLeft   = iff(W?#C0:'+'),
         sLeftTee   = iff(W?#C3:'+'),
         sTopRight  = iff(W?#BF:'+')

procedure vis(integer n, string pre)
    string pc = Vertical&" "
    sequence x = nodes[n]
    while true do
        integer next = x[NEXT]
        if next!=n then
            printf(1,pre&sLeftTee&Horizontal)
        else
            printf(1,pre&sBtmLeft&Horizontal)
            pc = "  "
        end if
        if x[CHILD] == NULL then
            printf(1,"%c %s\n",{Horizontal,sprint(x[VALUE])})
        else
            printf(1,"%c %s\n",{sTopRight,sprint(x[VALUE])})
            vis(x[CHILD], pre&pc)
        end if
        if next=n then exit end if
        x = nodes[next]
    end while
end procedure

procedure Vis(integer h)
    if nodes[h] == NULL then
        printf(1,"<empty>\n")
        return
    end if
    vis(h,"")
end procedure

printf(1,"MakeHeap:\n")
integer h := MakeHeap()
Vis(h)
 
printf(1,"\nInsert:\n")
{h} = Insert(h,"cat")
Vis(h)
 
printf(1,"\nUnion:\n")
integer h2 := MakeHeap()
{h2} = Insert(h2,"rat")
{h,h2} = Union(h,h2)     -- (h2:=NULL)
Vis(h)
 
printf(1,"\nMinimum:\n")
{object m, {}} = Minimum(h)
?m
 
printf(1,"\nExtractMin:\n")
-- add a couple more items to demonstrate parent-child linking that
-- happens on delete min.
{h} = Insert(h,"bat")
{h,integer x} = Insert(h,"meerkat") -- save x for decrease key and delete demos
{h,m,{}} = ExtractMin(h)
printf(1,"(extracted %s)\n", {sprint(m)})
Vis(h)

printf(1,"\nDecreaseKey:\n")
h = DecreaseKey(h, x, "gnat")
Vis(h)

printf(1,"\nDelete:\n")
-- add yet a couple more items to show how F&T's original delete was
-- lazier than CLRS's delete.
{h} = Insert(h,"bobcat")
{h} = Insert(h,"bat")
printf(1,"(deleting %s)\n", {sprint(nodes[x][VALUE])})
h = Delete(h,x)
Vis(h)
```

{{out}}

```txt

MakeHeap:
<empty>

Insert:
└── "cat"

Union:
├── "cat"
└── "rat"

Minimum:
"cat"

ExtractMin:
(extracted "bat")
├─┐ "cat"
│ └── "rat"
└── "meerkat"

DecreaseKey:
├─┐ "cat"
│ └── "rat"
└── "gnat"

Delete:
(deleting "gnat")
├── "bat"
├── "bobcat"
└─┐ "cat"
  └── "rat"

```



## Python


```python
class FibonacciHeap:
    
    # internal node class 
    class Node:
        def __init__(self, data):
            self.data = data
            self.parent = self.child = self.left = self.right = None
            self.degree = 0
            self.mark = False
            
    # function to iterate through a doubly linked list
    def iterate(self, head):
        node = stop = head
        flag = False
        while True:
            if node == stop and flag is True:
                break
            elif node == stop:
                flag = True
            yield node
            node = node.right
    
    # pointer to the head and minimum node in the root list
    root_list, min_node = None, None
    
    # maintain total node count in full fibonacci heap
    total_nodes = 0
    
    # return min node in O(1) time
    def find_min(self):
        return self.min_node
         
    # extract (delete) the min node from the heap in O(log n) time
    # amortized cost analysis can be found here (http://bit.ly/1ow1Clm)
    def extract_min(self):
        z = self.min_node
        if z is not None:
            if z.child is not None:
                # attach child nodes to root list
                children = [x for x in self.iterate(z.child)]
                for i in xrange(0, len(children)):
                    self.merge_with_root_list(children[i])
                    children[i].parent = None
            self.remove_from_root_list(z)
            # set new min node in heap
            if z == z.right:
                self.min_node = self.root_list = None
            else:
                self.min_node = z.right
                self.consolidate()
            self.total_nodes -= 1
        return z
                    
    # insert new node into the unordered root list in O(1) time
    def insert(self, data):
        n = self.Node(data)
        n.left = n.right = n
        self.merge_with_root_list(n)
        if self.min_node is None or n.data < self.min_node.data:
            self.min_node = n
        self.total_nodes += 1
        
    # modify the data of some node in the heap in O(1) time
    def decrease_key(self, x, k):
        if k > x.data:
            return None
        x.data = k
        y = x.parent
        if y is not None and x.data < y.data:
            self.cut(x, y)
            self.cascading_cut(y)
        if x.data < self.min_node.data:
            self.min_node = x
            
    # merge two fibonacci heaps in O(1) time by concatenating the root lists
    # the root of the new root list becomes equal to the first list and the second
    # list is simply appended to the end (then the proper min node is determined)
    def merge(self, h2):
        H = FibonacciHeap()
        H.root_list, H.min_node = self.root_list, self.min_node
        # fix pointers when merging the two heaps
        last = h2.root_list.left
        h2.root_list.left = H.root_list.left
        H.root_list.left.right = h2.root_list
        H.root_list.left = last
        H.root_list.left.right = H.root_list
        # update min node if needed
        if h2.min_node.data < H.min_node.data:
            H.min_node = h2.min_node
        # update total nodes
        H.total_nodes = self.total_nodes + h2.total_nodes
        return H
        
    # if a child node becomes smaller than its parent node we
    # cut this child node off and bring it up to the root list
    def cut(self, x, y):
        self.remove_from_child_list(y, x)
        y.degree -= 1
        self.merge_with_root_list(x)
        x.parent = None
        x.mark = False
    
    # cascading cut of parent node to obtain good time bounds
    def cascading_cut(self, y):
        z = y.parent
        if z is not None:
            if y.mark is False:
                y.mark = True
            else:
                self.cut(y, z)
                self.cascading_cut(z)
    
    # combine root nodes of equal degree to consolidate the heap
    # by creating a list of unordered binomial trees
    def consolidate(self):
        A = [None] * self.total_nodes
        nodes = [w for w in self.iterate(self.root_list)]
        for w in xrange(0, len(nodes)):
            x = nodes[w]
            d = x.degree
            while A[d] != None:
                y = A[d] 
                if x.data > y.data:
                    temp = x
                    x, y = y, temp
                self.heap_link(y, x)
                A[d] = None
                d += 1
            A[d] = x
        # find new min node - no need to reconstruct new root list below
        # because root list was iteratively changing as we were moving 
        # nodes around in the above loop
        for i in xrange(0, len(A)):
            if A[i] is not None:
                if A[i].data < self.min_node.data:
                    self.min_node = A[i]
        
    # actual linking of one node to another in the root list
    # while also updating the child linked list
    def heap_link(self, y, x):
        self.remove_from_root_list(y)
        y.left = y.right = y
        self.merge_with_child_list(x, y)
        x.degree += 1
        y.parent = x
        y.mark = False
        
    # merge a node with the doubly linked root list   
    def merge_with_root_list(self, node):
        if self.root_list is None:
            self.root_list = node
        else:
            node.right = self.root_list.right
            node.left = self.root_list
            self.root_list.right.left = node
            self.root_list.right = node
            
    # merge a node with the doubly linked child list of a root node
    def merge_with_child_list(self, parent, node):
        if parent.child is None:
            parent.child = node
        else:
            node.right = parent.child.right
            node.left = parent.child
            parent.child.right.left = node
            parent.child.right = node
            
    # remove a node from the doubly linked root list
    def remove_from_root_list(self, node):
        if node == self.root_list:
            self.root_list = node.right
        node.left.right = node.right
        node.right.left = node.left
        
    # remove a node from the doubly linked child list
    def remove_from_child_list(self, parent, node):
        if parent.child == parent.child.right:
            parent.child = None
        elif parent.child == node:
            parent.child = node.right
            node.right.parent = parent
        node.left.right = node.right
        node.right.left = node.left
```

