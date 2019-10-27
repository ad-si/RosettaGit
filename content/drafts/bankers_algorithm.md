+++
title = "Banker's algorithm"
description = ""
date = 2019-08-11T09:35:29Z
aliases = []
[extra]
id = 12970
[taxonomies]
categories = []
tags = []
+++

{{draft task|Classic CS problems}}{{Wikipedia|Banker's algorithm}}
The '''Banker's algorithm''' is a [[wp:resource allocation|resource allocation]] and [[wp:deadlock|deadlock]] avoidance [[wp:algorithm|algorithm]] developed by [[wp:Edsger Dijkstra|Edsger Dijkstra]] that tests for safety by simulating the allocation of predetermined maximum possible amounts of all [[wp:resource (computer science)|resources]], and then makes a "s-state" check to test for possible deadlock conditions for all other pending activities, before deciding whether allocation should be allowed to continue.

### = Example input =

Assuming that the system distinguishes between four types of resources, (A, B, C and D), the following is an example of how those resources could be distributed.  ''Note that this example shows the system at an instant before a new request for resources arrives.  Also, the types and number of resources are abstracted.  Real systems, for example, would deal with much larger quantities of each resource.''

 Total resources in system:
 A B C D
 6 5 7 6

 Available system resources are:
 A B C D
 3 1 1 2

 Processes (currently allocated resources):
    A B C D
 P1 1 2 2 1
 P2 1 0 3 3
 P3 1 2 1 0

 Processes (maximum resources):
    A B C D
 P1 3 3 2 2
 P2 1 2 3 4
 P3 1 3 5 0

 Need= maximum resources - currently allocated resources
 Processes (need resources):
    A B C D
 P1 2 1 0 1
 P2 0 2 0 1
 P3 0 1 4 0


## C

Standard binary heap-as-priority queue affair.  Only that each node links back to its heap position for easier update.

There are two <code>main()</code> functions to choose from (look for <code>#define BIG_EXAMPLE</code>), one is for task example, the other is a much heavier duty test case.

```c>#include <stdio.h

#include <stdbool.h>

int main() {
    int curr[5][5];
    int max_claim[5][5];
    int avl[5];
    int alloc[5] = {0, 0, 0, 0, 0};
    int max_res[5];
    int running[5];

    int i, j, exec, r, p;
    int count = 0;
    bool safe = false;

    printf("\nEnter the number of resources: ");
    scanf("%d", &r);

    printf("\nEnter the number of processes: ");
    scanf("%d", &p);
    for (i = 0; i < p; i++) {
        running[i] = 1;
        count++;
    }

    printf("\nEnter Claim Vector: ");
    for (i = 0; i < r; i++)
        scanf("%d", &max_res[i]);

    printf("\nEnter Allocated Resource Table: ");
    for (i = 0; i < p; i++) {
        for (j = 0; j < r; j++)
            scanf("%d", &curr[i][j]);
    }

    printf("\nEnter Maximum Claim table: ");
    for (i = 0; i < p; i++) {
        for (j = 0; j < r; j++)
            scanf("%d", &max_claim[i][j]);
    }

    printf("\nThe Claim Vector is: ");
    for (i = 0; i < r; i++)
        printf("%d ", max_res[i]);

    printf("\nThe Allocated Resource Table:\n");
    for (i = 0; i < p; i++) {
        for (j = 0; j < r; j++)
            printf("\t%d", curr[i][j]);
        printf("\n");
    }

    printf("\nThe Maximum Claim Table:\n");
    for (i = 0; i < p; i++) {
        for (j = 0; j < r; j++)
            printf("\t%d", max_claim[i][j]);
        printf("\n");
    }

    for (i = 0; i < p; i++)
        for (j = 0; j < r; j++)
            alloc[j] += curr[i][j];

    printf("\nAllocated resources: ");
    for (i = 0; i < r; i++)
        printf("%d ", alloc[i]);
    for (i = 0; i < r; i++)
        avl[i] = max_res[i] - alloc[i];

    printf("\nAvailable resources: ");
    for (i = 0; i < r; i++)
        printf("%d ", avl[i]);
    printf("\n");

    while (count != 0) {
        safe = false;
        for (i = 0; i < p; i++) {
            if (running[i]) {
                exec = 1;
                for (j = 0; j < r; j++) {
                    if (max_claim[i][j] - curr[i][j] > avl[j]) {
                        exec = 0;
                        break;
                    }
                }

                if (exec) {
                    printf("\nProcess%d is executing.\n", i + 1);
                    running[i] = 0;
                    count--;
                    safe = true;
                    for (j = 0; j < r; j++)
                        avl[j] += curr[i][j];
                    break;
                }
            }
        }

        if (!safe) {
            printf("\nThe processes are in unsafe state.");
            break;
        }

        if (safe)
            printf("\nThe process is in safe state.");

        printf("\nAvailable vector: ");
        for (i = 0; i < r; i++)
            printf("%d ", avl[i]);
    }

    return 0;
}
```

{{out|Input and Output}}

```txt
Enter the number of resources: 4

Enter the number of processes: 5

Enter Claim Vector: 8 5 9 7

Enter Allocated Resource Table: 2 0 1 1 0 1 2 1 4 0 0 3 0 2 1 0 1 0 3 0

Enter Maximum Claim table: 3 2 1 4 0 2 5 2 5 1 0 5 1 5 3 0 3 0 3 3

The Claim Vector is: 8 5 9 7
The Allocated Resource Table:
        2       0       1       1
        0       1       2       1
        4       0       0       3
        0       2       1       0
        1       0       3       0

The Maximum Claim Table:
        3       2       1       4
        0       2       5       2
        5       1       0       5
        1       5       3       0
        3       0       3       3

Allocated resources: 7 3 7 5
Available resources: 1 2 2 2

Process3 is executing.

The process is in safe state.
Available vector: 5 2 2 5
Process1 is executing.

The process is in safe state.
Available vector: 7 2 3 6
Process2 is executing.

The process is in safe state.
Available vector: 7 3 5 7
Process4 is executing.

The process is in safe state.
Available vector: 7 5 6 7
Process5 is executing.

The process is in safe state.
Available vector: 8 5 9 7
```



## Go

WP cites EWD-108 and has EWD-623 as further reading.  In using the analogy of a money, EWD-108 considers only a single type of resource.  EWD-623 seems mostly in terms of a single resource but a couple of times says "all resources."  This subtly hints that the algorithm can be adapted for multiple resource types.  WP then gives an array-based presentation that works for multiple resources.

This solution is more inspired by EWD-623 than WP.  EWD-623, while it talks of finding a permutation of processes, notes that the "ordering effort" can be stopped as soon as the process requesting resources happens to be found satisfiable.  The solution here attempts to make this finding as soon as possible by moving the process to the front of a list of unsatisfied processes.  Also since the solved permutation of satisfied processes has no use, it is not kept which simplifies the algorithm a bit.

```go
package bank

import (
    "bytes"
    "errors"
    "fmt"
    "log"
    "sort"
    "sync"
)

type PID string
type RID string
type RMap map[RID]int

// format RIDs in order
func (m RMap) String() string {
    rs := make([]string, len(m))
    i := 0
    for r := range m {
        rs[i] = string(r)
        i++
    }
    sort.Strings(rs)
    var b bytes.Buffer
    b.WriteString("{")
    for _, r := range rs {
        fmt.Fprintf(&b, "%q: %d, ", r, m[RID(r)])
    }
    bb := b.Bytes()
    if len(bb) > 1 {
        bb[len(bb)-2] = '}'
    }
    return string(bb)
}

type Bank struct {
    available  RMap
    max        map[PID]RMap
    allocation map[PID]RMap
    sync.Mutex
}

func (b *Bank) need(p PID, r RID) int {
    return b.max[p][r] - b.allocation[p][r]
}

func New(available RMap) (b *Bank, err error) {
    for r, a := range available {
        if a < 0 {
            return nil, fmt.Errorf("negative resource %s: %d", r, a)
        }
    }
    return &Bank{
        available:  available,
        max:        map[PID]RMap{},
        allocation: map[PID]RMap{},
    }, nil
}

func (b *Bank) NewProcess(p PID, max RMap) (err error) {
    b.Lock()
    defer b.Unlock()
    if _, ok := b.max[p]; ok {
        return fmt.Errorf("process %s already registered", p)
    }
    for r, m := range max {
        switch a, ok := b.available[r]; {
        case !ok:
            return fmt.Errorf("resource %s unknown", r)
        case m > a:
            return fmt.Errorf("resource %s: process %s max %d > available %d",
                r, p, m, a)
        }
    }
    b.max[p] = max
    b.allocation[p] = RMap{}
    return
}

func (b *Bank) Request(pid PID, change RMap) (err error) {
    b.Lock()
    defer b.Unlock()
    if _, ok := b.max[pid]; !ok {
        return fmt.Errorf("process %s unknown", pid)
    }
    for r, c := range change {
        if c < 0 {
            return errors.New("decrease not allowed")
        }
        if _, ok := b.available[r]; !ok {
            return fmt.Errorf("resource %s unknown", r)
        }
        if c > b.need(pid, r) {
            return errors.New("increase exceeds declared max")
        }
    }
    // allocation is non-exported data so we can change it in place
    // then change it back if the request cannot be granted.
    for r, c := range change {
        b.allocation[pid][r] += c // change in place
    }
    defer func() {
        if err != nil { // if request not granted,
            for r, c := range change {
                b.allocation[pid][r] -= c // change it back
            }
        }
    }()
    // Collect list of process IDs, also compute cash
    // First in the list is always the requesting PID.
    cash := RMap{}
    for r, a := range b.available {
        cash[r] = a
    }
    perm := make([]PID, len(b.allocation))
    i := 1
    for pr, a := range b.allocation {
        if pr == pid {
            perm[0] = pr
        } else {
            perm[i] = pr
            i++
        }
        for r, a := range a {
            cash[r] -= a
        }
    }
    ret := RMap{}  // sum of loans
    m := len(perm) // number of processes still candidates for termination
    for {
        // find a process h that can terminate
        h := 0
    h:
        for ; ; h++ {
            if h == m {
                // no process could terminate
                return errors.New("request would make deadlock possible")
            }
            for r := range b.available {
                if b.need(perm[h], r) > cash[r]+ret[r] {
                    // h cannot terminate if any resource need cannot be met.
                    continue h
                }
            }
            // log possible terimation, consistent with WP example program.
            log.Println(" ", perm[h], "could terminate")
            // all resource needs can be met.  h can terminate.
            break
        }
        if h == 0 { // Zwanenburg condition:
            // if requesting process can terminate, pattern is safe and
            // remaining terminations do not need to be demonstrated.
            return nil
        }
        for r, a := range b.allocation[perm[h]] {
            ret[r] += a
        }
        m--
        perm[h] = perm[m]
    }
}
```


```go
package main

import (
    "fmt"
    "bank"
)

func main() {
    // Task example data:
    // create "bank" with available resources
    b, _ := bank.New(bank.RMap{"A": 6, "B": 5, "C": 7, "D": 6})

    // add processes with their maximum allocation limits
    b.NewProcess("P1", bank.RMap{"A": 3, "B": 3, "C": 2, "D": 2})
    b.NewProcess("P2", bank.RMap{"A": 1, "B": 2, "C": 3, "D": 4})
    b.NewProcess("P3", bank.RMap{"A": 1, "B": 3, "C": 5})

    // processes request resources.  Each request is checked for safety.
    // <nil> returned error value means request was safe and was granted.
    fmt.Println("P1 request:")
    fmt.Println(b.Request("P1", bank.RMap{"A": 1, "B": 2, "C": 2, "D": 1}))

    fmt.Println("\nP2 request:")
    fmt.Println(b.Request("P2", bank.RMap{"A": 1, "C": 3, "D": 3}))

    fmt.Println("\nP3 request:")
    fmt.Println(b.Request("P3", bank.RMap{"A": 1, "B": 2, "C": 1}))
}
```

{{out}}

```txt

P1 request:
2017/08/29 16:44:15   P1 could terminate
<nil>

P2 request:
2017/08/29 16:44:15   P2 could terminate
<nil>

P3 request:
2017/08/29 16:44:15   P1 could terminate
2017/08/29 16:44:15   P2 could terminate
2017/08/29 16:44:15   P3 could terminate
<nil>

```



## J


The task description currently does not define the process being run. So we follow the example set by other implementations and have each process free all resources after successfully being run. Also, since this is a demo, we give a blow-by-blow description of what's happening as it runs.


```j
bankrun=:1 :0
  'MAX ALLOC TOTAL'=. y
   todo=.(#ALLOC)#1
   whilst. (+./todo)*-. prior-:todo do.
     prior=. todo
     for_p.I.todo do.
       avail=. TOTAL-+/ALLOC
       echo 'currently available: ',":avail
       pALLOC=. p{ALLOC
       pMAX=. p{MAX
       request=. pMAX-pALLOC
       if.(0>request)+.&(+./)request>avail do.
         echo 'unsafe request ',(":request),', skipping ',":p
         continue.
       else.
         echo 'running process ',(":p),', allocating ',":request
       end.
       free=.request u pALLOC
       echo 'process ',(":p),' freeing ',":free
       assert (0<:free) *&(*/) free <: pMAX
       ALLOC=. (pALLOC-free) p} ALLOC
       todo=. 0 p} todo
    end.
  end.
  if.+./todo do.
    echo 'deadlocked processes: ',":I.todo
  end.
  echo 'DONE'
)
```


Definitions for task example:


```j
max=: 3 3 2 2,1 2 3 4,:1 3 5 0
alloc=: 1 2 2 1,1 0 3 3,:1 2 1 0
total=:6 5 7 6

NB. simulate running process
NB. left arg: newly available resources, right: previously available
NB. result: resources freed
run=: +
```


Example run:


```J
   run bankrun max;alloc;total
currently available: 3 1 1 2
running process 0, allocating 2 1 0 1
process 0 freeing 3 3 2 2
currently available: 6 4 3 4
running process 1, allocating 0 2 0 1
process 1 freeing 1 2 3 4
currently available: 7 6 6 8
running process 2, allocating 0 1 4 0
process 2 freeing 1 3 5 0
DONE
```



## Julia

{{trans|Kotlin}}

```julia
function queryprompt(query, typ)
    print(query, ": ")
    entry = uppercase(strip(readline(stdin)))
    return (typ <: Integer) ? parse(Int, entry) :
        (typ <: Vector) ? map(x -> parse(Int, x), split(entry, r"\s+")) :
        entry
end
 
function testbankers()
    r = queryprompt("Enter the number of resources", Int)
    p = queryprompt("\nEnter the number of processes", Int)
    maxres = queryprompt("\nEnter Claim Vector", Vector{Int})
    curr, maxclaim = zeros(Int, p, r), zeros(Int, p, r)
    
    for i in 1:p
        curr[i, :] .= queryprompt("\nEnter Allocated Resource Table, Row $i", Vector{Int})
    end
     for i in 1:p
        maxclaim[i, :] .= queryprompt("\nEnter Maximum Claim Table, Row $i", Vector{Int})
    end
 
    alloc = [sum(curr[:, j]) for j in 1:r]
    println("\nAllocated Resources: $alloc")
 
    avl = map(i -> maxres[i] - alloc[i], 1:r)
    println("\nAvailable Resources: $avl")
 
    running = trues(p)
    count = p
    while count != 0
        safe = false
        for i in 1:p
            if running[i]
                exec = true
                for j in 1:r
                    if maxclaim[i, j] - curr[i, j] > avl[j]
                        exec = false
                        break
                    end
                end
 
                if exec
                    println("\nProcess $i is executing.")
                    running[i] = false
                    count -= 1
                    safe = true
                    for j in 1:r
                        avl[j] += curr[i, j]
                    end
                    break
                end
            end
        end
 
        if !safe
            println("The processes are in an unsafe state.")
            break
        end
 
        println("\nThe process is in a safe state.")
        println("\nAvailable Vector: $avl")
    end
end

testbankers()

```
{{out}}

```txt

Enter the number of resources: 4

Enter the number of processes: 5

Enter Claim Vector: 8 5 9 7

Enter Allocated Resource Table, Row 1: 2 0 1 1

Enter Allocated Resource Table, Row 2: 0 1 2 1

Enter Allocated Resource Table, Row 3: 4 0 0 3

Enter Allocated Resource Table, Row 4: 0 2 1 0

Enter Allocated Resource Table, Row 5: 1 0 3 0


Enter Maximum Claim Table, Row 1: 3 2 1 4

Enter Maximum Claim Table, Row 2: 0 2 5 2

Enter Maximum Claim Table, Row 3: 5 1 0 5

Enter Maximum Claim Table, Row 4: 1 5 3 0

Enter Maximum Claim Table, Row 5: 3 0 3 3

Allocated Resources: [7, 3, 7, 5]

Available Resources: [1, 2, 2, 2]

Process 3 is executing.

The process is in a safe state.

Available Vector: [5, 2, 2, 5]

Process 1 is executing.

The process is in a safe state.

Available Vector: [7, 2, 3, 6]

Process 2 is executing.

The process is in a safe state.

Available Vector: [7, 3, 5, 7]

Process 4 is executing.

The process is in a safe state.

Available Vector: [7, 5, 6, 7]

Process 5 is executing.

The process is in a safe state.

Available Vector: [8, 5, 9, 7]

```



## Kotlin

{{trans|C}}
For simplicity, input checking is ignored:

```scala
// version 1.1.4-3

fun main(args: Array<String>) {
    print("Enter the number of resources: ")
    val r = readLine()!!.toInt()

    print("\nEnter the number of processes: ")
    val p = readLine()!!.toInt()

    print("\nEnter Claim Vector: ")
    val maxRes = readLine()!!.split(' ').map { it.toInt() } .toIntArray()

    println("\nEnter Allocated Resource Table:")
    val curr = Array(p) { IntArray(r) }
    for (i in 0 until p) {
        print("Row ${i + 1}:  ")
        curr[i] = readLine()!!.split(' ').map { it.toInt() }.toIntArray()
    }

    println("\nEnter Maximum Claim Table: ")
    val maxClaim = Array(p) { IntArray(r) }
    for (i in 0 until p) {
        print("Row ${i + 1}:  ")
        maxClaim[i] = readLine()!!.split(' ').map { it.toInt() }.toIntArray()
    }

    val alloc = IntArray(r)
    for (i in 0 until p) {
        for (j in 0 until r) alloc[j] += curr[i][j]
    }
    println("\nAllocated Resources: ${alloc.joinToString(" ")}")

    val avl = IntArray(r) { maxRes[it] - alloc[it] }
    println("\nAvailable Resources: ${avl.joinToString(" ")}")

    val running = BooleanArray(p) { true }
    var count = p
    while (count != 0) {
        var safe = false
        for (i in 0 until p) {
            if (running[i]) {
                var exec = true
                for (j in 0 until r) {
                    if (maxClaim[i][j] - curr[i][j] > avl[j]) {
                        exec = false
                        break
                    }
                }

                if (exec) {
                    print("\nProcess ${i + 1} is executing.\n")
                    running[i] = false
                    count--
                    safe = true
                    for (j in 0 until r) avl[j] += curr[i][j]
                    break
                }
            }
        }

        if (!safe) {
            print("The processes are in an unsafe state.")
            break
        }

        print("\nThe process is in a safe state.")
        println("\nAvailable Vector: ${avl.joinToString(" ")}")
    }
}
```


Sample input/output:

```txt

Enter the number of resources: 4

Enter the number of processes: 5

Enter Claim Vector: 8 5 9 7

Enter Allocated Resource Table:
Row 1:  2 0 1 1
Row 2:  0 1 2 1
Row 3:  4 0 0 3
Row 4:  0 2 1 0
Row 5:  1 0 3 0

Enter Maximum Claim Table: 
Row 1:  3 2 1 4
Row 2:  0 2 5 2
Row 3:  5 1 0 5
Row 4:  1 5 3 0
Row 5:  3 0 3 3

Allocated Resources: 7 3 7 5

Available Resources: 1 2 2 2

Process 3 is executing.

The process is in a safe state.
Available Vector: 5 2 2 5

Process 1 is executing.

The process is in a safe state.
Available Vector: 7 2 3 6

Process 2 is executing.

The process is in a safe state.
Available Vector: 7 3 5 7

Process 4 is executing.

The process is in a safe state.
Available Vector: 7 5 6 7

Process 5 is executing.

The process is in a safe state.
Available Vector: 8 5 9 7

```



## M2000 Interpreter


```M2000 Interpreter

Module BankerAlgo {
      Form 80, 44
      Cls 5
      Pen 14
      Function Request(FromWhere as Inventory, What$, Many as long)  {
            =FromWhere(What$)-FromWhere(What$+"_Request")-Many>=0
      }
      Function RequestPreset(FromWhere as Inventory, What$, Many as long)  {
            =FromWhere(What$+"_Request")-Many>=0
      }
      Function Need(FromWhere as Inventory, What$, Many) { 
            =FromWhere(What$ + "_max")-FromWhere(What$)-Many>=0
      }
      \\ code for sub can be found from parent module/function (here parent as in code, not as in call)
      Function NewProcess {
            Inventory Process
            ApplyResources(Process)   ' sub need more arguments and read from current stack
            =Process
      }
      Inventory System, Processes 
      \\ Recource, Max, Available
      ApplyResources(System, "A", 6, 3,"B", 5,1,"C", 7, 1, "D", 6, 2)
      \\ Recource, Max, Available
      Append Processes, "P1":=NewProcess("A", 3, 1, "B", 3, 2, "C", 2, 2, "D", 2,1)
      Append Processes, "P2":=NewProcess("A", 1, 1, "B", 2, 0, "C", 3, 3, "D", 4,3)
      Append Processes, "P3":=NewProcess("A", 1, 1, "B", 3, 2, "C", 5, 1, "D", 0,0)
      Status(True) ' show all process, available resource and max
      SafeState=True
      Print "Current Status"
      RequestResource() ' display Safe State
      RequestResource("P2", "D", 1) ' display Safe State
      RequestResource("P1", "A", 1, "D", 1) ' display Safe State
      RequestResource("P1", "C", 1, "D", 1) ' display Too many resources ...
      RequestResource("P2", "B", 1) ' display Unsafe State
      RequestResource("P3", "C", 1)  ' display Safe State
      Status()
      \\ Second Example
      Clear System, Processes
      ApplyResources(System, "A", 10, 3)
      Append Processes, "P1":=NewProcess("A", 9, 3)
      Append Processes, "P2":=NewProcess("A", 4, 2)
      Append Processes, "P3":=NewProcess("A", 7, 2)
      Status(True) ' show all process, available resource and max    
      Print "Current Status"
      RequestResource() ' display Safe State
      \ Third Example
      Clear System
      ApplyResources(System, "A", 10, 2)
      Return Processes, "P1":=NewProcess("A", 9,4)
      Status(True) ' show all process, available resource and max    
      Print "Current Status"
      RequestResource() ' display UnSafe State       
      Sub Respond()
            If SafeState Then {
                  Pen 15 {Print "Safe State"}
            } Else Pen 13 {Print "Unsafe State"}
      End Sub
      Sub WaitForKey()
            Pen 11 {Print "Press a key"}
            local a$=key$
      End Sub
      Sub RequestResource(ProcessName$="" )
            SafeState=True
            If ProcessName$="" Then CheckNewState(&SafeState) : Respond() : Print : WaitForKey():Exit Sub
            Local pro=Processes(ProcessName$), ResourceName$, many as long
            ClearAllRequest(pro)
            Local skip=False
            While Match("SN") {
                  Read ResourceName$, many
                  Print  Format$("Claim {1} for type {0} resource ",ResourceName$, many)
                  If skip Then Continue
                  If Request(System, ResourceName$, many) Then {
                        If Need(pro, ResourceName$, many) Then { 
                              Return pro, ResourceName$+"_Request":=many
                              Return System, ResourceName$+"_Request":=-many
                        } Else {
                              Print "Too many Recources "+ResourceName$+" for Process "+ProcessName$  : Skip=True
                        }
                  } Else Print "Too many Recources for System" : Skip=True
                  If Skip Then exit
            } 
            If skip Else  CheckNewState(&SafeState) : Respond()
            Print  ' just a new line
            WaitForKey()
      End Sub
      Sub ApplyResources(Where as Inventory, What$, MaxValue, InitialValue)
            Repeat {
                  If Not Exist(Where, What$) Then {
                        Append Where, What$:=InitialValue, What$+"_max":=MaxValue, What$+"_Request":=0
                  }
                  If not Match("SNN") Then Exit
                  Read What$, MaxValue, InitialValue
            }  Always
      End Sub
      Sub ClearAllRequest(Where  as Inventory)
            Local M=Each(Where)
            While M {
                  If Instr(Eval$(M, M^),"_")=0 Then {
                        Return Where, Eval$(M,M^)+"_Request":=0
                  }
            }
      End Sub
      Sub PrintResources(Where  as Inventory)
            Local M=Each(Where)
            While M {
                  If Instr(Eval$(M, M^),"_")=0 Then Print Eval$(M, M^)+"="+Eval$(M),
            }
            Print
      Exit Sub
      Sub PrintMax(Where  as Inventory)
            Local M=Each(Where)
            While M {
                  If Instr(Eval$(M, M^),"_max")>0 Then Print LeftPart$(Eval$(M, M^), "_")+"="+Eval$(M),
            }
            Print
      Exit Sub
      Sub Status(Ok as boolean=False)
            Print "Total System Resources"
            PrintMax(System)
            Print "Available Resources in System"
            PrintResources(System)
            If Not Ok Then WaitForKey(): Exit Sub
            Local  M=Each(Processes)
            While M {
                  Print "Process "+Eval$(M, M^)
                  PrintResources(Processes(M^!))  ' give index M^ as Key index number (using !)
                  Print "Maximum Resources for "+Eval$(M, M^)
                  PrintMax(Processes(M^!))
            }
      End Sub
      Sub CheckNewState(&Ok)
            local M=Each(Processes), M1, count=len(Processes), alive(0 to count-1)=1
            Local Z, Recource$, safe as boolean=false
            While count {
                  safe=false
                  While M {
                        If alive(M^) Then {
                              Z=Processes(M^!)
                              M1=Each(Z) 
                              safe=True 
                              While M1 {
                                    Recource$=Eval$(M1, M1^)
                                    If Instr(Recource$,"_")=0 Then {
                                         safe=System(Recource$)+System(Recource$+"_Request") >= Z(Recource$ + "_max") - Z(Recource$)-Z(Recource$ + "_Request")
		               }
                                    If not safe Then exit
                              }
                              If safe Then {
                                    print format$("Process {0} is executing", M^+1)
                                    alive(M^)=0
                                    count--
                                    M1=Each(Z) 
                                    While M1 {
                                          Recource$=Eval$(M1, M1^)
                                          If Instr(Recource$,"_")=0 Then {
                                                Return System, Recource$+"_Request":= System(Recource$+"_Request") + Z(Recource$) + Z(Recource$+"_Request")
                                                Return Z, Recource$+"_Request":=0
                                          }
                                    }
                              }
                        }
                  }
                  If safe Else exit
            }
            Ok=safe
            ClearAllRequest(System)
      End Sub
}
BankerAlgo
```



## Phix


```Phix
sequence max_res = {6, 5, 7, 6},
         curr = {{1, 2, 2, 1},
                 {1, 0, 3, 3},
                 {1, 2, 1, 0}},
        running = repeat(true,length(curr)),
        max_claim = {{3, 3, 2, 2},
                     {1, 2, 3, 4},
                     {1, 3, 5, 0}},
        alloc = repeat(0,length(max_res))

    integer count = length(curr)
    for i=1 to count do
        alloc = sq_add(alloc,curr[i])
    end for
    sequence avl = sq_sub(max_res,alloc) 

    printf(1,"Available system resources: ")    ?max_res
    printf(1,"Process allocated: ")             ?curr
    printf(1,"Maximum resources: ")             ?max_claim
    printf(1,"Allocated resources: ")           ?alloc
    printf(1,"Available resources: ")           ?avl 

    while count!=0 do
        bool safe = false
        for i=1 to length(curr) do
            if running[i] then
                bool execute = true
                for j=1 to length(max_res) do
                    if max_claim[i][j]-curr[i][j] > avl[j] then
                        execute = false
                        exit
                    end if
                end for
 
                if execute then
                    printf(1,"Process%d is executing. ", i)
                    running[i] = false
                    count -= 1
                    safe = true
                    for j=1 to length(max_res) do
                        avl[j] += curr[i][j]
                    end for
                    exit
                end if
            end if
        end for
 
        if not safe then
            printf(1,"The processes are in an unsafe state.\n");
            exit
        end if
        printf(1, "Safe state. Available resources: ")          ?avl
    end while
```

{{out}}

```txt

Available system resources: {6,5,7,6}
Process allocated: {{1,2,2,1},{1,0,3,3},{1,2,1,0}}
Maximum resources: {{3,3,2,2},{1,2,3,4},{1,3,5,0}}
Allocated resources: {3,4,6,4}
Available resources: {3,1,1,2}
Process1 is executing. Safe state. Available resources: {4,3,3,3}
Process2 is executing. Safe state. Available resources: {5,3,6,6}
Process3 is executing. Safe state. Available resources: {6,5,7,6}

```

Changing the initial curr[2] to {1, 1, 3, 3}:

```txt

Available system resources: {6,5,7,6}
Process allocated: {{1,2,2,1},{1,1,3,3},{1,2,1,0}}
Maximum resources: {{3,3,2,2},{1,2,3,4},{1,3,5,0}}
Allocated resources: {3,5,6,4}
Available resources: {3,0,1,2}
The processes are in an unsafe state.

```



## Racket



```racket
#lang racket/base
(require racket/block racket/pretty racket/port racket/vector)

(pretty-print-columns 20) ; make the matrices look a bit more matrixey

(define (bankers-algorithm p r maxres curr maxclaim)
  (define running? (make-vector p #t))
  (define alloc (for/vector #:length r ((j (in-range r)))
                  (for/sum ((cu_i (in-vector curr))) (vector-ref cu_i j))))
  (printf "Allocated resources:~%~a~%" (pretty-format alloc))
  (define avl (for/vector #:length r ((m (in-vector maxres)) (a (in-vector alloc))) (- m a)))
  (printf "Available resources:~%~a~%~%" (pretty-format avl))
  
  (define (safe-exec i mc_i cu_i)
    (define exec? (for/and ((a (in-vector avl)) (m (in-vector mc_i)) (c (in-vector cu_i)))
                    (<= (- m c) a)))
    (cond
      [exec?
       (printf "Process ~a is executing~%" (add1 i))
       (vector-set! running? i #f)
       (for ((j (in-range r)) (a (in-vector avl)) (c (in-vector cu_i))) (vector-set! avl j (+ a c)))
       #t]
      [else #f]))
  
  (let loop ()
    (unless (zero? (vector-count values running?))
      (define safe?
        (for/first ((i (in-range p))
                    (r? (in-vector running?))
                    (mc_i (in-vector maxclaim))
                    (cu_i (in-vector curr))
                    ;; the break condition for this is identical to safe?, so we have no
                    ;; separate break? flag
                    #:when r?
                    #:when (safe-exec i mc_i cu_i))
          #t))
      (cond [safe?
             (printf "The process is in a safe state~%~%Available vector: ~a~%" (pretty-format avl))
             (loop)]
            [else (displayln "The processes are in an unsafe state")]))))


(define (bankers-input)  
  (define ((n-vector? type? dims) x) ;; not the world's most efficient implementation!
    (cond [(null? dims) (type? x)]
          [(not (vector? x)) #f]
          [(not (= (car dims) (vector-length x))) #f]
          [else (for/and ((e (in-vector x))) (n-vector? type? (cdr dims)) e)]))
  
  (define-syntax-rule (prompted-input prompt valid?)
    (block
     (printf "Enter ~a:~%" prompt)
     (define rv (read))
     (pretty-print rv)
     (unless (valid? rv) (raise-argument-error 'prompted-input (format "~a" 'valid?) rv))
     rv))
  
  (define p (prompted-input "the number of processes" exact-positive-integer?))
  (define r (prompted-input "the number of resources" exact-positive-integer?))
  (define maxres (prompted-input "Claim Vector" (n-vector? exact-positive-integer? (list r))))
  (define curr (prompted-input "Allocated Resource Table"
                               (n-vector? exact-positive-integer? (list p r))))
  (define maxclaim (prompted-input "Maximum Claim Table"
                                   (n-vector? exact-positive-integer? (list p r))))
  (values p r maxres curr maxclaim))

(module+ main
  (with-input-from-string
   #<<EOS
5
4
#(8 5 9 7)
#(#(2 0 1 1)
  #(0 1 2 1)
  #(4 0 0 3)
  #(0 2 1 0)
  #(1 0 3 0))
#(#(3 2 1 4)
  #(0 2 5 2)
  #(5 1 0 5)
  #(1 5 3 0)
  #(3 0 3 3))

EOS
   (λ () (call-with-values bankers-input bankers-algorithm))))
```


{{out}}

```txt
Enter the number of processes:
5
Enter the number of resources:
4
Enter Claim Vector:
'#(8 5 9 7)
Enter Allocated Resource Table:
'#(#(2 0 1 1)
   #(0 1 2 1)
   #(4 0 0 3)
   #(0 2 1 0)
   #(1 0 3 0))
Enter Maximum Claim Table:
'#(#(3 2 1 4)
   #(0 2 5 2)
   #(5 1 0 5)
   #(1 5 3 0)
   #(3 0 3 3))
Allocated resources:
'#(7 3 7 5)
Available resources:
'#(1 2 2 2)

Process 3 is executing
The process is in a safe state

Available vector: '#(5 2 2 5)
Process 1 is executing
The process is in a safe state

Available vector: '#(7 2 3 6)
Process 2 is executing
The process is in a safe state

Available vector: '#(7 3 5 7)
Process 4 is executing
The process is in a safe state

Available vector: '#(7 5 6 7)
Process 5 is executing
The process is in a safe state

Available vector: '#(8 5 9 7)
```



## Rust


Adapted from the C language version. It crashes for invalid input.


```rust

fn read_numbers<T>() -> Vec<T>
where T: std::str::FromStr {
    use std::io::Write;
    std::io::stdout().flush().unwrap();

    let mut line = String::new();
    std::io::stdin().read_line(&mut line).unwrap();
    line.split(" ").map(|word| word.trim().parse::<T>().ok().unwrap()).collect()
}

fn main() {
    print!("Enter the number of resources: ");
    let r = read_numbers()[0];
    
    print!("Enter the number of processes: ");
    let p = read_numbers()[0];
    let mut running = vec![true; p];
    let mut count = p;
    
    print!("Enter the {}-item claim vector: ", r);
    let max_res = read_numbers::<u32>();

    println!("Enter the {}-line {}-column allocated-resource table:", p, r);
    let mut curr = vec![vec![0; 0]; p];
    for i in 0..p {
        curr[i] = read_numbers::<u32>();
    }
    
    println!("Enter the {}-line {}-column maximum-claim table:", p, r);
    let mut max_claim = vec![vec![0; 0]; p];
    for i in 0..p {
        max_claim[i] = read_numbers::<u32>();
    }
    
    print!("The claim vector is: ");
    for i in 0..r {
        print!("{} ", max_res[i]);
    }
    println!();

    println!("The allocated resources table is:");
    for i in 0..p {
        for j in 0..r {
            print!("\t{}", curr[i][j]);
        }
        println!();
    }

    println!("The maximum claims table is:");
    for i in 0..p {
        for j in 0..r {
            print!("\t{}", max_claim[i][j]);
        }
        println!();
    }
    
    let mut alloc = vec![0; r];
    for i in 0..p {
        for j in 0..r {
            alloc[j] += curr[i][j];
        }
    }
    
    print!("The allocated resources are: ");
    for i in 0..r {
        print!("{} ", alloc[i]);
    }
    println!();
    let mut avl = vec![0; r];
    for i in 0..r {
        avl[i] = max_res[i] - alloc[i];
    }

    print!("The available resources are: ");
    for i in 0..r {
        print!("{} ", avl[i]);
    }
    println!();

    while count != 0 {
        let mut safe = false;
        for i in 0..p {
            if running[i] {
                let mut exec = true;
                for j in 0..r {
                    if max_claim[i][j] - curr[i][j] > avl[j] {
                        exec = false;
                        break;
                    }
                }

                if exec {
                    println!("Process {} is executing.", i + 1);
                    running[i] = false;
                    count -= 1;
                    safe = true;
                    for j in 0..r {
                        avl[j] += curr[i][j];
                    }
                    break;
                }
            }
        }

        if safe {
            println!("The process is in safe state.");
        }
        else {
            println!("The processes are in unsafe state.");
            break;
        }

        print!("The available vector is: ");
        for i in 0..r {
            print!("{} ", avl[i]);
        }
        println!();
    }
}

```


{{out|Input and Output}}

```txt

Enter the number of resources: 4
Enter the number of processes: 5
Enter the 4-item claim vector: 8 5 9 7
Enter the 5-line 4-column allocated-resource table:
2 0 1 1
0 1 2 1
4 0 0 3
0 2 1 0
1 0 3 0
Enter the 5-line 4-column maximum-claim table:
3 2 1 4
0 2 5 2
5 1 0 5
1 5 3 0
3 0 3 3
The claim vector is: 8 5 9 7 
The allocated resources table is:
	2	0	1	1
	0	1	2	1
	4	0	0	3
	0	2	1	0
	1	0	3	0
The maximum claims table is:
	3	2	1	4
	0	2	5	2
	5	1	0	5
	1	5	3	0
	3	0	3	3
The allocated resources are: 7 3 7 5 
The available resources are: 1 2 2 2 
Process 3 is executing.
The process is in safe state.
The available vector is: 5 2 2 5 
Process 1 is executing.
The process is in safe state.
The available vector is: 7 2 3 6 
Process 2 is executing.
The process is in safe state.
The available vector is: 7 3 5 7 
Process 4 is executing.
The process is in safe state.
The available vector is: 7 5 6 7 
Process 5 is executing.
The process is in safe state.
The available vector is: 8 5 9 7 

```


{{omit from|Blast}}
{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Openscad}}

{{omit from|TPP}}
