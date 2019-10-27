+++
title = "Pastebin:Atomic"
description = ""
date = 2009-05-19T09:35:25Z
aliases = []
[extra]
id = 4198
[taxonomies]
categories = []
tags = []
+++

The question is, are atomic_1 and atomic_2 equivalent?  Are they atomic?  Are they thread-safe? (Continuing a discussion from #rosettacode)

They are logically equivalent under certain conditions:

# used in the same process (mutex is unnamed local in the example)
# ''var'' is not accessed otherwise than through these methods (using mutex for mutex, using interlocking functions for InterlockedExchange)

They are not atomic in the sense that when LONG I/O is not atomic at the hardware level nothing can make it atomic. Rather either method would prevent certain thread switching eliminating corrupted I/O with ''var''.

They are thread-safe for threads respecting the conditions of use. MS-documentation on InterlockedExchange is vague, but it let us suggest that it is not necessarily implemented at the hardware level (like by blocking the memory bus in order to access ''var''). So it might be unsafe to access ''var'' otherwise than through the interlocking functions. However, we can expect that any access to ''var'' is safe on a 32 or more bit machine.

Feel free to edit this to heck and back; If it needs to be linked to, link to a specific revision.


```cpp

volatile LONG var;

class statmux
{
  public:
  HANDLE hMux;
  statmux()
  {
    // Initialize the mutex
    hMux = CreateMutex(NULL, FALSE, NULL);
  }
  ~statmux()
  {
    // Free the mutex
    CloseHandle(hMux);
  }
} s;

void atomic_1(LONG newval)
{
  // Lock the mutex
  DWORD res = WaitForSingleObject(s.hMux, INFINITE);

  // WAIT_OBJECT_0 indicates that we hold the mutex.  For these purposes, we don't care about any other scenario.
  if(WAIT_OBJECT_0 != res)
    return;

  // Replace the file-static variable value with the value we were passed.
  var = newval;

  // Release the mutex
  ReleaseMutex(s.hMux);
}

void atomic_2(LONG newval)
{
  // InterlockedExchange sets up memory fences on either side of the load/store
  // Theoretically, no other thread will see a mixture of the old and new value (i.e. tearing)
  // Additionally, no other CPU will subsequently hold cached version of the old value.
  InterlockedExchange(&var, newval);
}

```

