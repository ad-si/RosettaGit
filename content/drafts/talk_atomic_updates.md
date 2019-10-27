+++
title = "Talk:Atomic updates"
description = ""
date = 2013-01-22T11:22:56Z
aliases = []
[extra]
id = 4194
[taxonomies]
categories = []
tags = []
+++

Please clarify the task. Transfer of a bucket value cannot be defined atomic operation while preserving the object's invariant. The solutions presented are incorrect in this sense because they do not have transfer '''safe'''. They rather provide an '''unsafe''' transfer with locking the object externally (to the transfer). Because transfer does not keep the object's invariant it is not an operation of. So what was the purpose  of the task? Just to show a use of mutex? Then the task looks superfluous, since there is already [[mutex]] task. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 12:44, 17 May 2009 (UTC)

: I'm sorry, but I don't understand the distinctions you're making. How is the transfer operation unsafe? If you're just concerned about the nonnegative requirement, I've revised the task to include what my E implementation actually does. The difference between this task and [[mutex]] is that this task does not explicitly require the use of a mutex; my E example does not use a mutex, and it could also be implemented using, for example, [[wp:software transactional memory]]. The purpose of this task is to demonstrate a basic task in concurrency: ensuring that a data structure preserves its invariants in the presence of concurrent access. --[[User:Kevin Reid|Kevin Reid]] 15:17, 17 May 2009 (UTC)

:: Save = preserves object's integrity (invariant). E.g. sum of the bucket's values. Since a transfer of values cannot be implemented as an independent safe operation, it is unclear why it is required. My Ada solution provides atomic updates that preserve the object's invariant (sum). Can you explain why is it an incorrect solution? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 15:36, 18 May 2009 (UTC)
:::I don't understand how it is unsafe. The transfer increases one bucket by the same amount it decreases another bucket, so it preserves the invariant. The Ada solution is incorrect because the data structure does not have a ''general'' transfer operation that could be used in other ways, just two special cases. (Of course, in the end this task is arbitrary -- feel free to argue that I should change the definition.) --[[User:Kevin Reid|Kevin Reid]] 16:52, 18 May 2009 (UTC)
::::Hmm, what is "general transfer" that preserves the invariant? To me transfer is to move something from one place to another. This is possible to do atomically, but impossible while keeping certain invariants. Ada solution implements atomic update as suggests the task name. Transfer there is only a part of update. Maybe you mean a user-defined procedure atomically applied to a pair of buckets? I.e. the operation takes two buckets and a user procedure that updates them while the object is locked. This is a possible scenario, but again, it does not preserve the invariant. So, IMO, one of the things must be dropped: 1. invariant, 2. transfer as an update, 3. specific updates, which aren't just transfers. I dropped 2 (transfer), because this the most common case it concurrent programming. You have some object with low-level operations like transfer, which are inherently unsafe (non-atomic, kill the invariant etc). You hide them in the implementation and provide higher level operations, which are safe, but of course, not so flexible as the low-level ones. (I asked about mutexes, because initially I thought you wanted to illustrate transactions: one initiates a transaction (locks the object), does some unsafe things, like transfers, and then closes the transaction (fixes the invariant and unlocks the object). Of course there could be lock-free solutions, but that is besides the point.) --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:02, 18 May 2009 (UTC)
:::::We seem to be talking past each other. I have provided a definition of an atomic transfer which preserves the invariants. Here is the E code from my example: 

```e
        to transfer(i :int, j :int, amount :int) {
            def amountLim := amount.min(values[i]).max(-(values[j]))
            values[i] -= amountLim
            values[j] += amountLim
        }
```

:::::Assume that this entire routine is executed atomically. It preserves both invariants -- the sum of buckets (by changing two buckets by equal and opposite amounts) and that each bucket is positive (by clamping the amount transferred). Why can't you do the same thing in Ada? --[[User:Kevin Reid|Kevin Reid]] 18:48, 18 May 2009 (UTC)
::::::Ah, finally I've got it. Thanks for an explanation. That is no problem. Can you move this definition of transfer into the task description? E.g. "takes two indices and then adds a specified amount to the bucket indexed by one and subtracts it from one indexed by another index, but no more than its total value." One question remains, how can this transfer be used to make pairs of buckets equal (averaging)? You need an atomic read + decide + update operation. Or this is not required by the task? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 19:24, 18 May 2009 (UTC)
:::::::I've tried to clarify the description. And yes, checking the current value is explicitly non-atomic. The idea is that in the absence of disturbance, the neatening task ''would'' achieve its goal; each step is, on average, an improvement, unless there is another task specifically perversely adjusting the buckets just in time. This isn't intended to be a sensible example of how to accomplish anything on a small data set; it's intended to be solely an exercise in preserving invariants despite sloppy/buggy/competing clients. --[[User:Kevin Reid|Kevin Reid]] 19:39, 18 May 2009 (UTC)

== And in a language that does not support multithreading ? ==
The question is in the title. While preserving an invariant still makes sense, the task needs some clarification: in that case, it doesn't illustrate anything about atomic updates, since there cannot be multiple updates at the same time anyway. [[User:Capra Hircus|Capra Hircus]] 19:53, 1 September 2012 (UTC)

== Go code ==
For people that want to actually try to run the code, I think the Go entries should explain that to compile the Go entries you need some changes:

```go
import (
    "fmt"
    "math/rand"
    "time"
    "sync"
    "runtime"
)
...
func main() {
    // Create a concrete object implementing the bucketList 
interface.
    bl := newRwList(10, originalTotal, nUpdaters)
```


And for the third:


```go
import (
    "fmt"
    "math/rand"
    "time"
    "sync"
    "runtime"
    "sync/atomic"
)
...
func main() {
    // Create a concrete object implementing the bucketList 
interface.
    bl := newLfList(10, originalTotal, nUpdaters)
```


The first Go solution compiles as it is, but I don't see any printing output.
