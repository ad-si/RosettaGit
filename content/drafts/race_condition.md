+++
title = "Race condition"
description = ""
date = 2008-08-25T22:10:49Z
aliases = []
[extra]
id = 2179
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]A '''race condition''' describes a class of programming, design or implementation bug in which the outcome of some process or function call can vary depending on the scheduling/processing of other [[thread]]s or [[process]]es. Commonly, this interference comes in the form of two processes depending on or manipulating the same piece of memory, or on multiple processes attempting access to the same file.

==Reasons==

In any situation where a resource is shared and potentially concurrently accessible there are possible race conditions. Such situations require specific contention avoidance or mitigation processes.


### File access


For example, when creating a new file on a local [[UNIX]] or [[Linux]] filesystem two processes might attempt to use the same filename at the same time. One of them will create the filesystem while the other might merely open the newly created file. Using special flags to the ''open()'' system call, it's possible to guarantee that the file will be created and exclusively opened, or that it will return an error if the file already exists. Thus the '''O_EXCL''' and the '''O_CREAT''' flags to the ''open()'' system call are required to make this guarantee. (Additionally code must check for the possible failure/error condition).

As a further example, the O_EXCL|O_CREAT combination of flags is '''not''' sufficient for providing the same race free guarantee on an NFS (networked) filesystem. The guarantee regarding ''open()'s'' semantics is only valid for local filesystems. Avoiding race conditions in creating files over NFS requires the use of unique temporary filenames, and the use of ''link()'' and possibly ''fstat()'' system calls. It's possible to write code which is guaranteed to be race free for both situations (under standards conformant implementations of UNIX and NFS). However it's very easy to mistakenly use one form of code over the other situation in ways that will only rarely fail. 


### Shared memory


Race considerations also apply to shared memory. Threads implicitly share most or all of their memory, and it's possible for processes to share memory via [[POSIX]] or SysV [[API]]s (for UNIX/Linux) or via other (operating system specific) means. On any sort of multi-processor or "hyper-threaded" hardware there can be races for memory access. However, even on single-CPU (uniprocessor) systems there can be races between fetching a value (into a CPU register) and writing a new value back out to memory. Programming languages with support for threading supply various forms of locking and synchronization primitives, and sometimes high-level APIs which must be used to coordinate all access to shared memory. Additionally, one normally uses SysV or POSIX semaphore APIs along with the corresponding shared memory features to handle contention.

The concept of "race conditions" can apply to any form of shared memory or storage resource and many networking and communications domains. Each such situation imposes specific requirements on the programmer to avoid races, and generally there is no way to definitively test for race conditions (tests can only prove that you haven't "lost" the races in some number of attempts over some specific situations). Thus the programmer must adhere carefully to domain specific standards in order to guarantee that their code is race free. Also note that the programmer is dependent on the standards compliance of the hardware, operating system, and programming tool chain; regardless of how carefully he or she as adhered to the standards.

Most popular platforms perform [[multitasking]], where multiple threads of code run on the same computer. These threads may be their own processes, or they may be part of a larger program. In addition, these threads may share the same CPU core, or there may be multiple cores (or CPUs) available.

When a thread refers to a piece of memory (i.e. an object), they may make decisions based upon its contents, and it might assume that that object will not change between the time it looks at it and the time when it carries out an action based on that glance. Take this C++ example:

 CObject* pObj = GetSharedObject();
 if ( pObj->Available() ) // Check to see if the object is available
 {
   DoSomethingWith( pObj ); // Use the object
 }

In this code, pObj points to an object (returned by GetSharedObject) that may be shared with other threads. Let's assume for the moment that another thread has also called GetSharedObject(), and received a pointer to the same object we're working with.

Because of the nature of multitasking, it's possible that the second thread might have accessed the shared object ''after'' we checked to see if the object was available, but ''before'' we did something with that object. And it's possible that what the second thread did with our shared object will make our DoSomethingWith() method fail.

==Inconsistency==

While it's possible that another thread with access to our shared resource changed it between the times when we check for its availability and when we attempt to use it, it's also possible that the second thread operated on the shared resource before or after our code, in which case our code likely wouldn't have failed. Whether or not our code fails depends on the timing of when the two threads accessed the object. Since timing is rarely consistent, the failure of our code (or any code subject to race conditions) can be inconsistent and difficult to trace.

==Solutions==

There are any number of solutions to race conditions, but most revolve around preventing access to the same data at the same time.


### Locks


[[Lock|Locks]] are mechanisms that force one thread to wait until another thread is finished with a piece of data. As an example:

 CObject* pObj = GetSharedObject();
 
 pObj->Lock();
 if ( pObj->Available() )
 {
   DoSomethingWith( pObj );
 }
 pObj->Unlock();

In the above code, we lock our object before we attempt to do anything with it. Mind you, in this example, we ''don't'' know how Lock() is implemented; Implementations vary between languages, support libraries, and operating systems. However, we're assured that no other (appropriately-behaving) thread will be able to make a change to our object while we're working with it. If another thread has a lock when we call Lock(), ''our thread will hang'' until that other thread releases the lock. Once the other thread releases the lock, our thread will continue.

Various sorts of locking primitives include [[semaphore]]s and [[mutex]]es (mutual exclusion objects).

### =Cooperative Locks=

Semaphores are commonly used to lock memory regions shared by processes. Semaphores are often called ''cooperative locks'' because processes must actively cooperate by acquiring and releasing the semaphore. Any process that ignores the semaphore can access the shared memory whether or not another process has acquired the semaphore.

### =Enforced Locks=

Synchronization locks can be designed into threading architectures by encapsulating the locking and unlocking operations in accessor methods for shared memory regions. All access to the shared memory region then enforces the locking policy for all threads. 

Languages with built-in threading syntax commonly provide very simple ways to enforce locks. [[Java]] provides the ''synchronized'' keyword to designate a code block with enforced locking. 

[[Ada]] provides ''protected objects'' with locking characteristics based upon the kind of access method described. A protected ''procedure'' enforces a write lock, allowing unconditional access to the object by only a single thread at a time. A protected ''entry'' enforces a write lock, allowing conditional access to the object by one thread at a time. A protected ''function'' provides a shared read lock, allowing multiple read access to the object at the same time. Many locking [http://home.att.net/~jimmaureenrogers/Shared_Resource_Design_Patterns.html patterns] can be defined using these three capabilities.
