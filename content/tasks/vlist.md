+++
title = "VList"
description = ""
date = 2018-10-25T17:56:21Z
aliases = []
[extra]
id = 9174
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "cpp",
  "d",
  "go",
  "kotlin",
  "oorexx",
  "phix",
  "racket",
  "rexx",
  "scala",
]
+++

[[Category:Encyclopedia]]{{draft task|Data Structures}}{{data structure}}[[Category:Classic CS problems and programs]]
In computer science, the '''VList''' is a persistent data structure that combines the fast indexing of arrays with the easy extension of cons-based (or singly-linked) linked lists.

Like arrays, VLists have constant-time lookup on average and are highly compact, requiring only O(log ''n'') storage for pointers, allowing them to take advantage of locality of reference.
Like singly-linked or cons-based lists, they are persistent, and elements can be added to or removed from the front in constant time. Length can also be found in O(log ''n'') time.

The primary operations of a VList are:
* Locate the ''k''th element (O(1) average, O(log ''n'') worst-case)
* Add an element to the front of the VList (O(1) average, with an occasional allocation)
* Obtain a new array beginning at the second element of an old array (O(1))
* Compute the length of the list (O(log ''n''))

## Task

The task is to demonstrate creation of a VList and how to perform the primary operations.


## C


```c
#include <stdio.h>
#include <stdlib.h>

typedef struct sublist{
	struct sublist* next;
	int *buf;
} sublist_t;

sublist_t* sublist_new(size_t s)
{
	sublist_t* sub = malloc(sizeof(sublist_t) + sizeof(int) * s);
	sub->buf = (int*)(sub + 1);
	sub->next = 0;
	return sub;
}

typedef struct vlist_t {
	sublist_t* head;
	size_t last_size, ofs;
} vlist_t, *vlist;

vlist v_new()
{
	vlist v = malloc(sizeof(vlist_t));
	v->head = sublist_new(1);
	v->last_size = 1;
	v->ofs = 0;
	return v;
}

void v_del(vlist v)
{
	sublist_t *s;
	while (v->head) {
		s = v->head->next;
		free(v->head);
		v->head = s;
	}
	free(v);
}

inline size_t v_size(vlist v)
{
	return v->last_size * 2 - v->ofs - 2;
}

int* v_addr(vlist v, size_t idx)
{
	sublist_t *s = v->head;
	size_t top = v->last_size, i = idx + v->ofs;

	if (i + 2 >= (top << 1)) {
		fprintf(stderr, "!: idx %d out of range\n", (int)idx);
		abort();
	}
	while (s && i >= top) {
		s = s->next, i ^= top;
		top >>= 1;
	}
	return s->buf + i;
}

inline int v_elem(vlist v, size_t idx)
{
	return *v_addr(v, idx);
}

int* v_unshift(vlist v, int x)
{
	sublist_t* s;
	int *p;

	if (!v->ofs) {
		if (!(s = sublist_new(v->last_size << 1))) {
			fprintf(stderr, "?: alloc failure\n");
			return 0;
		}
		v->ofs = (v->last_size <<= 1);
		s->next = v->head;
		v->head = s;
	}
	*(p = v->head->buf + --v->ofs) = x;
	return p;
}

int v_shift(vlist v)
{
	sublist_t* s;
	int x;

	if (v->last_size == 1 && v->ofs == 1) {
		fprintf(stderr, "!: empty list\n");
		abort();
	}
	x = v->head->buf[v->ofs++];

	if (v->ofs == v->last_size) {
		v->ofs = 0;
		if (v->last_size > 1) {
			s = v->head, v->head = s->next;
			v->last_size >>= 1;
			free(s);
		}
	}
	return x;
}

int main()
{
	int i;

	vlist v = v_new();
	for (i = 0; i < 10; i++) v_unshift(v, i);

	printf("size: %d\n", v_size(v));
	for (i = 0; i < 10; i++) printf("v[%d] = %d\n", i, v_elem(v, i));
	for (i = 0; i < 10; i++) printf("shift: %d\n", v_shift(v));

	/* v_shift(v); */ /* <- boom */

	v_del(v);
	return 0;
}
```



## C++

```c

#include <iostream>
#include <vector>
#include <forward_list>
#include <cassert>
#include <memory>

template<typename T>
class VList {
public:
    VList() : datas(), offset(0) {}
private:
    std::forward_list<std::shared_ptr<std::vector<T>>> datas;
    int offset;
public:

    // modify structure instead of returning a new one like the pure functional way does
    void cons(const T& a) {
        if(datas.empty()){
            std::shared_ptr<std::vector<T>> base = std::shared_ptr<std::vector<T>>(new std::vector<T>(1)) ;
            (*base)[0] = a;
            datas.emplace_front(base);
            offset = 0;
            return;
        }
        if(offset == 0){
            datas.front()->shrink_to_fit();
            const int new_capacity = (int) datas.front()->capacity() * 2;
            const int new_offset = new_capacity - 1;
            std::shared_ptr<std::vector<T>> base = std::shared_ptr<std::vector<T>>(new std::vector<T>(new_capacity)) ;
            (*base)[new_offset] = a;
            datas.emplace_front(base);
            offset = new_offset;
            return ;
        }
        --offset;
        (* datas.front())[offset] = a;
    }

    // lisp like cdr to keep previous version
    VList* cdr() {
        if (datas.empty()) {
            // cdr of empty list is an empty list
            return this;
        }
        VList* new_vlist = new VList();
        new_vlist->datas = this->datas;
        new_vlist->offset = offset;
        new_vlist->offset++;
        if(new_vlist->offset < new_vlist->datas.front()->capacity()){
            return new_vlist;
        }
        new_vlist->offset = 0;
        new_vlist->datas.front().reset();
        new_vlist->datas.pop_front();
        return new_vlist;
    }

    // compute the length of the list.  (It's O(1).)
    int length() {
        if (datas.empty()) {
            return 0;
        }
        return (int)datas.front()->capacity()*2 - this->offset - 1;
    }

    bool index(int i, T& out) {
        bool isValid = false;
        if (i >= 0) {
            i += this->offset;
            for(auto data : datas) {
                if (i < data->size()) {
                    out = (* data)[i];
                    isValid = true;
                    break;
                }
                i -= data->size();
            }
        }
        return isValid;
    }

    void printList() {
        if (datas.empty()) {
            std::cout << "[]" << std::endl;
            return;
        }
        std::vector<T>* first = datas.front().get();
        assert(NULL != first);
        std::cout << "[";
        for (int i=offset; i<first->size(); i++) {
            std::cout << " " << (* first)[i];
        }
        for(auto data : datas) {
            if(data.get() == datas.front().get())
                continue;
            for (int i=0; i<data->size(); i++) {
                std::cout << " " << (* data)[i];
            }
        }
        std::cout << " ]" << std::endl;
    }

    // One more method for demonstration purposes
    void printStructure() {
        std::cout << "offset:" << this->offset << std::endl;
        if (datas.empty()) {
            std::cout << "[]" << std::endl;
            return ;
        }
        std::vector<T>* first = datas.front().get();
        assert(NULL != first);
        std::cout << "[";
        for (int i=offset; i<first->size(); i++) {
            std::cout << " " << (* first)[i];
        }
        std::cout << " ]" << std::endl;
        for(auto data : datas) {
            if(data.get() == datas.front().get())
                continue;
            std::cout << "[";
            for (int i=0; i<data->size(); i++) {
                std::cout << " " << (* data)[i];
            }
            std::cout << " ]" << std::endl;
        }
        std::cout << std::endl;
    }
};

int main(int argc, const char * argv[]) {

    std::unique_ptr<VList<char>> vlist = std::unique_ptr<VList<char>>(new VList<char>());

    std::cout << "zero value for type.  empty vList:";
    vlist->printList();
    vlist->printStructure();

    std::cout << "demonstrate cons. 6 elements added:";
    for (char a = '6'; a >= '1'; a--) {
        vlist->cons(a);
    }
    vlist->printList();
    vlist->printStructure();

    std::cout << "demonstrate cdr. 1 element removed:";
    vlist = std::unique_ptr<VList<char>>(vlist->cdr());
    vlist->printList();
    vlist->printStructure();

    std::cout << "demonstrate length. length =" << vlist->length() << std::endl;

    char out;
    bool isValid = vlist->index(3, out);
    if(isValid)
        std::cout << "demonstrate element access. v[3] =" << out << std::endl;

    std::cout << "show cdr releasing segment. 2 elements removed:";
    vlist = std::unique_ptr<VList<char>>(vlist->cdr()->cdr());
    vlist->printList();
    vlist->printStructure();

    return 0;
}

```

```txt

zero value for type.  empty vList:[]
offset:0
[]
demonstrate cons. 6 elements added:[ 1 2 3 4 5 6 ]
offset:1
[ 1 2 3 ]
[ 4 5 ]
[ 6 ]

demonstrate cdr. 1 element removed:[ 2 3 4 5 6 ]
offset:2
[ 2 3 ]
[ 4 5 ]
[ 6 ]

demonstrate length. length =5
demonstrate element access. v[3] =5
show cdr releasing segment. 2 elements removed:[ 4 5 6 ]
offset:0
[ 4 5 ]
[ 6 ]


```



## D

```d
import core.stdc.stdio: fprintf, stderr;
import core.stdc.stdlib: malloc, free, abort;

/// Uses C malloc and free to manage its memory.
/// Use only VList.alloc and VList.free.
struct VList(T) {
    static struct Sublist {
        Sublist* next;
        T[0] dataArr0;

        @property data() inout pure nothrow {
            return dataArr0.ptr;
        }

        static typeof(this)* alloc(in size_t len) nothrow {
            auto ptr = cast(typeof(this)*)malloc(typeof(this).sizeof +
                                                 T.sizeof * len);
            ptr.next = null;
            return ptr;
        }
    }

    // A dynamic array of pointers to growing buffers seems
    // better than a linked list of them.
    Sublist* head;
    size_t lastSize, ofs;

    static typeof(this)* alloc() nothrow {
        auto v = cast(typeof(this)*)malloc(VList.sizeof);
        enum startLength = 1;
        v.head = Sublist.alloc(startLength);
        v.lastSize = startLength;
        v.ofs = 0;
        return v;
    }

    void free() nothrow {
        while (this.head) {
            auto s = this.head.next;
            .free(this.head);
            this.head = s;
        }
        .free(&this);
    }

    @property size_t length() const nothrow {
        return this.lastSize * 2 - this.ofs - 2;
    }

    T* addr(in size_t idx) const nothrow {
        const(Sublist)* s = this.head;
        size_t top = this.lastSize;
        size_t i = idx + this.ofs;

        if (i + 2 >= (top << 1)) {
            fprintf(stderr, "!: idx %zd out of range\n", idx);
            abort();
        }
        while (s && i >= top) {
            s = s.next;
            i ^= top;
            top >>= 1;
        }
        return s.data + i;
    }

    T elem(in size_t idx) const nothrow {
        return *this.addr(idx);
    }

    // Currently dangerous.
    //T opIndex(in size_t idx) const nothrow {
    //    return elem(idx);
    //}

    T* prepend(in T x) nothrow {
        if (!this.ofs) {
            auto s = Sublist.alloc(this.lastSize << 1);
            if (s == null) {
                fprintf(stderr, "?: alloc failure\n");
                return null;
            }
            this.lastSize <<= 1;
            this.ofs = this.lastSize;
            s.next = this.head;
            this.head = s;
        }

        this.ofs--;
        auto p = this.head.data + this.ofs;
        *p = x;
        return p;
    }

    T popHead() nothrow {
        if (this.lastSize == 1 && this.ofs == 1) {
            fprintf(stderr, "!: empty list\n");
            abort();
        }

        auto x = this.head.data[this.ofs];
        this.ofs++;

        if (this.ofs == this.lastSize) {
            this.ofs = 0;
            if (this.lastSize > 1) {
                auto s = this.head;
                this.head = s.next;
                this.lastSize >>= 1;
                .free(s);
            }
        }

        return x;
    }

    // Range protocol is missing.
}


void main() {
    import std.stdio, std.bigint;
    enum N = 10;

    auto v = VList!BigInt.alloc;
    foreach (immutable i; 0 .. N)
        v.prepend(i.BigInt);

    writefln("v.length = %d", v.length);
    foreach (immutable i; 0 .. N)
        writefln("v[%d] = %s", i, v.elem(i));
    foreach (immutable i; 0 .. N)
        writefln("popHead: %s", v.popHead);

    v.free;
}
```

```txt
v.length = 10
v[0] = 9
v[1] = 8
v[2] = 7
v[3] = 6
v[4] = 5
v[5] = 4
v[6] = 3
v[7] = 2
v[8] = 1
v[9] = 0
popHead: 9
popHead: 8
popHead: 7
popHead: 6
popHead: 5
popHead: 4
popHead: 3
popHead: 2
popHead: 1
popHead: 0
```



## Go


```go
package main

import "fmt"

type vList struct {
    base   *vSeg
    offset int
}

type vSeg struct {
    next *vSeg
    ele  []vEle
}

// element type could be anything. i pick string to demonstrate the task.
type vEle string

// primary operation 1: locate the kth element.
func (v vList) index(i int) (r vEle) {
    if i >= 0 {
        i += v.offset
        for sg := v.base; sg != nil; sg = sg.next {
            if i < len(sg.ele) {
                return sg.ele[i]
            }
            i -= len(sg.ele)
        }
    }
    // consistent with the way Go panics on slice index out of range
    panic("index out of range")
}

// primary operation 2: add an element to the front of the VList.
func (v vList) cons(a vEle) vList {
    if v.base == nil {
        return vList{base: &vSeg{ele: []vEle{a}}}
    }
    if v.offset == 0 {
        l2 := len(v.base.ele) * 2
        ele := make([]vEle, l2)
        ele[l2-1] = a
        return vList{&vSeg{v.base, ele}, l2 - 1}
    }
    v.offset--
    v.base.ele[v.offset] = a
    return v
}

// primary operation 3: obtain a new array beginning at the second element
// of an old array
func (v vList) cdr() vList {
    if v.base == nil {
        // consistent with panic above.  (not consistent with lisp)
        panic("cdr on empty vList")
    }
    v.offset++
    if v.offset < len(v.base.ele) {
        return v
    }
    return vList{v.base.next, 0}
}

// primary operation 4:  compute the length of the list.  (It's O(1).)
func (v vList) length() int {
    if v.base == nil {
        return 0
    }
    return len(v.base.ele)*2 - v.offset - 1
}

// A handy method:  satisfy stringer interface for easy output.
func (v vList) String() string {
    if v.base == nil {
        return "[]"
    }
    r := fmt.Sprintf("[%v", v.base.ele[v.offset])
    for sg, sl := v.base, v.base.ele[v.offset+1:]; ; {
        for _, e := range sl {
            r = fmt.Sprintf("%s %v", r, e)
        }
        sg = sg.next
        if sg == nil {
            break
        }
        sl = sg.ele
    }
    return r + "]"
}

// One more method for demonstration purposes
func (v vList) printStructure() {
    fmt.Println("offset:", v.offset)
    for sg := v.base; sg != nil; sg = sg.next {
        fmt.Printf("  %q\n", sg.ele) // %q illustrates the string type
    }
    fmt.Println()
}

// demonstration program using the WP example data
func main() {
    var v vList
    fmt.Println("zero value for type.  empty vList:", v)
    v.printStructure()

    for a := '6'; a >= '1'; a-- {
        v = v.cons(vEle(a))
    }
    fmt.Println("demonstrate cons. 6 elements added:", v)
    v.printStructure()

    v = v.cdr()
    fmt.Println("demonstrate cdr. 1 element removed:", v)
    v.printStructure()

    fmt.Println("demonstrate length. length =", v.length())
    fmt.Println()

    fmt.Println("demonstrate element access. v[3] =", v.index(3))
    fmt.Println()

    v = v.cdr().cdr()
    fmt.Println("show cdr releasing segment. 2 elements removed:", v)
    v.printStructure()
}
```

```txt

zero value for type.  empty vList: []
offset: 0

demonstrate cons. 6 elements added: [1 2 3 4 5 6]
offset: 1
  ["" "1" "2" "3"]
  ["4" "5"]
  ["6"]

demonstrate cdr. 1 element removed: [2 3 4 5 6]
offset: 2
  ["" "1" "2" "3"]
  ["4" "5"]
  ["6"]

demonstrate length. length = 5

demonstrate element access. v[3] = 5

show cdr releasing segment. 2 elements removed: [4 5 6]
offset: 0
  ["4" "5"]
  ["6"]

```

## Kotlin

```scala
// version 1.1.3

class VList<T : Any?> {

    private class VSeg {
        var next: VSeg? = null
        lateinit var ele: Array<Any?>
    }

    private var base: VSeg? = null
    private var offset = 0

    /* locate kth element */
    operator fun get(k: Int): T {
        var i = k
        if (i >= 0) {
            i += offset
            var sg = base
            while (sg != null) {
                @Suppress("UNCHECKED_CAST")
                if (i < sg.ele.size) return sg.ele[i] as T
                i -= sg.ele.size
                sg = sg.next
            }
        }
        throw IllegalArgumentException("Index out of range")
    }

    /* add an element to the front of VList */
    fun cons(a: T): VList<T> {
        if (this.base == null) {
            val v = VList<T>()
            val s = VSeg()
            s.ele = arrayOf<Any?>(a)
            v.base = s
            return v
        }
        if (this.offset == 0) {
            val l2 = this.base!!.ele.size * 2
            val ele = arrayOfNulls<Any>(l2)
            ele[l2 - 1] = a
            val v = VList<T>()
            val s = VSeg()
            s.next = this.base
            s.ele = ele
            v.base = s
            v.offset = l2 - 1
            return v
        }
        this.offset--
        this.base!!.ele[this.offset] = a
        return this
    }

    /* obtain a new VList beginning at the second element of an old VList */
    fun cdr(): VList<T> {
        if (base == null) throw RuntimeException("cdr invoked on empty VList")
        offset++
        if (offset < base!!.ele.size) return this
        val v = VList<T>()
        v.base = this.base!!.next
        return v
    }

    /* compute the size of the VList */
    val size: Int
        get() {
            if (base == null) return 0
            return base!!.ele.size * 2 - offset - 1
        }

    override fun toString(): String {
        if (base == null) return "[]"
        var r = "[${base!!.ele[offset]}"
        var sg = base
        var sl = base!!.ele.sliceArray(offset + 1..base!!.ele.lastIndex)
        while (true) {
            for (e in sl) r += " $e"
            sg = sg!!.next
            if (sg == null) break
            sl = sg.ele
        }
        return r + "]"
    }

    fun printStructure() {
        println("Offset: $offset")
        var sg = base
        while (sg != null) {
            println(sg.ele.contentToString())
            sg = sg.next
        }
        println()
    }
}

fun main(args: Array<String>) {
    var v = VList<Int>()
    println("Before adding any elements, empty VList: $v")
    v.printStructure()

    for (a in 6 downTo 1) v = v.cons(a)
    println("Demonstrating cons method, 6 elements added: $v")
    v.printStructure()

    v = v.cdr()
    println("Demonstrating cdr method, 1 element removed: $v")
    v.printStructure()

    println("Demonstrating size property, size = ${v.size}\n")
    println("Demonstrating element access, v[3] = ${v[3]}\n")

    v = v.cdr().cdr()
    println("Demonstrating cdr method again, 2 more elements removed: $v")
    v.printStructure()
}
```


```txt

Before adding any elements, empty VList: []
Offset: 0

Demonstrating cons method, 6 elements added: [1 2 3 4 5 6]
Offset: 1
[null, 1, 2, 3]
[4, 5]
[6]

Demonstrating cdr method, 1 element removed: [2 3 4 5 6]
Offset: 2
[null, 1, 2, 3]
[4, 5]
[6]

Demonstrating size property, size = 5

Demonstrating element access, v[3] = 5

Demonstrating cdr method again, 2 more elements removed: [4 5 6]
Offset: 0
[4, 5]
[6]

```



## ooRexx

The ooRexx queue class is a vlist implementation.
Here are some examples of usage:

```ooRexx

-- show how to use the queue class
q = .queue~of(1, 2, 3, 4)

-- show indexed access to item
say q[4]

-- update an item
q[2] = "Fred"

-- show update and that other indexes are unchanged
say q[2] q[4]

-- push an item on the front and show the change in positions
q~push("Mike")
say q[1] q[2] q[4]

-- pop an item and show the change again
q~pull
say q[1] q[2] q[4]


```

```txt

4
Fred 4
Mike 1 3
1 Fred 4

```



## Phix


```Phix
enum OFFSET,     -- (first spare slot [0=none])
     SEGMENTS

function new_vlist()
    return {0,{}} -- offset of 0, no segments
end function

function get_vlist(sequence v, integer k)
-- locate kth element
    if k>0 then
        k += v[OFFSET]
        integer sg = 1
        while sg<=length(v[SEGMENTS]) do
            sequence vsg = v[SEGMENTS][sg]
            if k<= length(vsg) then return vsg[k] end if
            k -= length(vsg)
            sg += 1
        end while
    end if
    throw("index out of range")
end function

function cons(sequence v, object a)
-- add an element to the front of v
    if length(v[SEGMENTS])=0 then
        return {0,{{a}}}
    end if
    integer offset = v[OFFSET]
    if offset=0 then
        offset = length(v[SEGMENTS][1])*2
        v[SEGMENTS] = prepend(v[SEGMENTS],repeat(0,offset))
    end if
    v[SEGMENTS][1][offset] = a
    v[OFFSET] = offset-1
    return v
end function

function cdr(sequence v)
-- remove first element of v
    if length(v[SEGMENTS])=0 then
        throw("cdr invoked on empty VList")
    end if
    integer offset = v[OFFSET]+1
    if offset>length(v[SEGMENTS][1]) then
        v[SEGMENTS] = v[SEGMENTS][2..$]
        v[OFFSET] = 1
    else
        v[OFFSET] = offset
    end if
    return v
end function

function vlist_size(sequence v)
-- compute the size of v
    if length(v[SEGMENTS])=0 then return 0 end if
    return length(v[SEGMENTS][1])*2 -v[OFFSET] -1
end function

function sprint_vlist(sequence v)
    return sprint(flatten(v[SEGMENTS])[v[OFFSET]+1..$])
end function

procedure print_vlist_structure(sequence v)
    printf(1,"Offset: %d\n",v[OFFSET])
    pp(v[SEGMENTS],{pp_Nest,1})
end procedure

procedure main()
    sequence v = new_vlist()
    printf(1,"Before adding any elements, empty VList: %s\n",{sprint_vlist(v)})
    print_vlist_structure(v)

    for a=6 to 1 by -1 do v = cons(v,a) end for
    printf(1,"Demonstrating cons method, 6 elements added: %s\n",{sprint_vlist(v)})
    print_vlist_structure(v)

    v = cdr(v)
    printf(1,"Demonstrating cdr method, 1 element removed: %s\n",{sprint_vlist(v)})
    print_vlist_structure(v)

    printf(1,"Demonstrating size property, size = %d\n",vlist_size(v))
    -- (note this is 1-based indexing)
    printf(1,"Demonstrating element access, v[3] = %d\n",get_vlist(v,3))

    v = cdr(cdr(cdr(v)))
    printf(1,"Demonstrating cdr method again, 3 more elements removed: %s, size = %d\n",
            {sprint_vlist(v),vlist_size(v)})
    print_vlist_structure(v)

    for a=7 to 9 do v = cons(v,a) end for -- (this time not by -1; {9 8 7 5 6} is expected)
    printf(1,"Demonstrating cons method, 3 more elements added: %s, size = %d\n",
            {sprint_vlist(v),vlist_size(v)})
    print_vlist_structure(v)

end procedure
main()
```

```txt

Before adding any elements, empty VList: ""
Offset: 0
{}
Demonstrating cons method, 6 elements added: {1,2,3,4,5,6}
Offset: 1
{{0,1,2,3},
 {4,5},
 {6}}
Demonstrating cdr method, 1 element removed: {2,3,4,5,6}
Offset: 2
{{0,1,2,3},
 {4,5},
 {6}}
Demonstrating size property, size = 5
Demonstrating element access, v[3] = 4
Demonstrating cdr method again, 3 more elements removed: {5,6}, size = 2
Offset: 1
{{4,5},
 {6}}
Demonstrating cons method, 3 more elements added: {9,8,7,5,6}, size = 5
Offset: 2
{{0,0,9,8},
 {7,5},
 {6}}

```



## Racket

See  https://github.com/takikawa/tr-pfds/blob/master/pfds/vlist.rkt
for an implementation of VLists.


## REXX

This classic REXX version uses (mostly) the same "input" and operations as the ooRexx version,
except that the (stack) queue isn't changed or used.

  ╔════════════════════════════════════════════════════════════════════╗
  ║                                                                    ║
  ║             ┌────────────┬───────────────────────────┐             ║
  ║             │  call  q   │                           │  (no args)  ║
  ║             └────────────┴───────────────────────────┘             ║
  ║ returns the number of items in the VList.                          ║
  ║                                                                    ║
  ║             ┌────────────┬───────────────────────────┐             ║
  ║             │  call  q   │  0,  a b c d  ∙∙∙         │             ║
  ║             └────────────┴───────────────────────────┘             ║
  ║ inserts the specified items(s) to the  front  of the VList.        ║
  ║                                                                    ║
  ║             ┌────────────┬───────────────────────────┐             ║
  ║             │  call  q   │  999999999,  a b c d ∙∙∙  │             ║
  ║             └────────────┴───────────────────────────┘             ║
  ║ appends the specified items(s) to the  end  of the VList.          ║
  ║                                                                    ║
  ║             ┌────────────┬───────────────────────────┐             ║
  ║             │  call  q   │  10,  a b c d  ∙∙∙        │             ║
  ║             └────────────┴───────────────────────────┘             ║
  ║ sets the tenth item to the items(s) specified.   If there're less  ║
  ║ than  10  items in the VList,  the specified item(s) are appended  ║
  ║ to the  end  of the VList.                                         ║
  ║                                                                    ║
  ║             ┌────────────┬───────────────────────────┐             ║
  ║             │  call  q   │  17                       │             ║
  ║             └────────────┴───────────────────────────┘             ║
  ║ returns the  seventeenth  item in the VList.                       ║
  ║                                                                    ║
  ║             ┌────────────┬───────────────────────────┐             ║
  ║             │  call  q   │  ,                        │  (a comma)  ║
  ║             └────────────┴───────────────────────────┘             ║
  ║ returns  all  the items in the VList.                              ║
  ║                                                                    ║
  ║             ┌────────────┬───────────────────────────┐             ║
  ║             │  call  q   │  -11                      │             ║
  ║             └────────────┴───────────────────────────┘             ║
  ║ deletes the  eleventh  item in the VList.                          ║
  ║                                                                    ║
  ║             ┌────────────┬───────────────────────────┐             ║
  ║             │  call  q   │  63.1,  a b c d  ∙∙∙      │             ║
  ║             └────────────┴───────────────────────────┘             ║
  ║ inserts the specified items after the  63rd  item.  If there's less║
  ║ then  63  items in the VList,  the specified items() are appended  ║
  ║ to the  end  of the VList.  Any non-zero decimal fraction may be   ║
  ║ used.       I.E.:     63.01     63.5     63.63     63.9            ║
  ╚════════════════════════════════════════════════════════════════════╝

```rexx
/*REXX program demonstrates  VList  operations:   add,  update,  delete, insert,  show. */
                                                 /*could use instead:     q  =  1 2 3 4 */
call  q  0, 1 2 3 4                              /*populate the list with values 1 ── ►4*/
say q(4)                                         /*show the indexed access to an item.  */

call q  2, 'Fred'                                /*update  (or add)  the second item.   */
say q(2) q(4)                                    /*show second and fourth items in list.*/
                                                 /*zeroth item is inserted in the front.*/
call q  0, 'Mike'                                /*insert item in front of the list.    */
say q(1) q(2) q(4)                               /*show first, second, and fourth items.*/
                                                 /*any  negative number  is deleted.    */
call q  -1                                       /*delete the first item in the list.   */
say q(1) q(2) q(4)                               /*show the  1st,  2nd,  and  4th items.*/
                                                 /*Fractional number inserts an item.   */
call q  3.5, '3½'                                /*insert the item after the third item.*/
say q ,                                          /*show all the  VList  items.          */
                                                 /*Put on a dog and pony show.          */
say 'number of items in Vlist:' q()              /*show and tell time for the  Vlist.   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
q: parse arg n 1 na 1 ni,!;   w=words(q);   $=             /*obtain arguments;  # items.*/
   if symbol('Q')=='LIT' then q=                           /*var Q may not be defined.  */
   if arg()==0        then return words(q)                 /*return the VList item count*/
   if arg(1, 'O')     then return q                        /*return the whole shebang.  */
   if arg()==1 & n>0  then return word(q,n)                /*1 positive arg?  Return it.*/
   if n==0            then do;  q=! q;  return q;  end     /*insert in front of the list*/
   if n> w            then do;  q=q !;  return q;  end     /*add it to  end.  "  "    " */
   na=abs(n)                                               /*we might need use of  ABS. */
   if \datatype(ni, 'W') & ni>0  then ni=trunc(na)         /*Is this an insert>?  TRUNC.*/
                                 else ni=0                 /*... No?  Then a plain Jane.*/
          do j=1 for w                                     /* [↓]  rebuild the  Vlist.  */
          if j==na  then do; if na==n then $=$ !;  end     /*replace the item in list.  */
                    else $=$ word(q, j)                    /*an easy-peasy (re-)build.  */
          if j==ni  then $=$ !                             /*handle the  "insert".      */
          end   /*j*/
   q=space($);           return q                          /*elide superfluous blanks.  */
```

```txt

4
Fred 4
Mike 1 3
1 Fred 4
1 Fred 3 3½ 4
number of items in Vlist: 5

```



## Scala

<blockquote cite="http://stackoverflow.com/questions/3107151/persistent-data-structures-in-scala">Two of Scala's 2.8 immutable data structures are vectors and hash tries, represented as 32-ary trees.
These were originally designed by '''Phil Bagwell''', who was working with my team at EPFL, then adopted for Clojure, and now finally adopted for Scala 2.8.
The Scala implementation shares a common root with the Clojure implementation, but is certainly not a port of it.</blockquote>
A quote of Martin Odersky, his co-worker Phil Bagwell† invented the VList.

```Scala
object VList extends App {

  val emptyVlist1 = Vector.empty[Int]
  val emptyVlist2 = Vector[Int]()
  val emptyVlist3 = Vector()

  val addedVlist1 = Vector(1, 2) :+ 6 :+ 10 :+ 12 :+ 42

  assert((addedVlist1, addedVlist1.head) == (Vector(1, 2, 6, 10, 12, 42), 1), "Header or VList not OK")

  val addedVlist2 = addedVlist1.head +: addedVlist1.tail.drop(1)
  assert((addedVlist2, addedVlist2.head) == (Vector(1, 6, 10, 12, 42), 1), "First CDR not deleted.")

  assert(addedVlist1.size == 6)

  assert(addedVlist1(3) == 10, "Wrong element accesed.")
  println("Successfully completed without errors.")
}
```

