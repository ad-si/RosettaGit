+++
title = "Semaphore"
description = ""
date = 2016-03-20T23:20:13Z
aliases = []
[extra]
id = 3033
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]

'''Semaphore''' is a synchronization object proposed by Edsger Dijkstra. A semaphore is characterized by a natural number ''k''. A [[task]] may atomically increase or decrease ''k''. When ''k'' reaches 0 the tasks attempting to decrease it are blocked. These are released in an unspecified order when other tasks increase ''k'', one per increment.

The natural number ''k'' works like a count of available slots for resources. When you (a task) want to use something (an object, a file, any resource) that can only be used by a limited number of tasks (usually one, but possibly more), you see if there are available slots (check the value of ''k''). If there are slots available (''k'' > 0), you take one (decrement ''k''). When you're done with the resource, you free your slot up (increment ''k''). If there were no slots available when you checked (''k'' = 0), you wait until one becomes available.

A semaphore is considered a low-level synchronization primitive. They are exposed to deadlocking, like in the problem of [[dining philosophers]].

See also [[mutex]], a variant of semaphore.

=Sample implementations / APIs=

==[[Ada]]==
Here is an implementation of a semaphore based on protected objects. The implementation provides operations P (seize) and V (release), these names are usually used with semaphores.

```ada

protected type Semaphore (K : Positive) is
   entry P;
   procedure V;
private
   Count : Natural := K;
end Mutex;

```

The implementation of:

```ada

protected body Semaphore is
   entry P when Count > 0 is
   begin
      Count := Count - 1;
   end P;
   procedure V is
   begin
      Count := Count + 1;
   end V;
end Semaphore;

```

Use:

```ada

declare
   S : Semaphore (5);
begin
   S.P;    -- Acquire the semaphore
   ...
   S.V;    -- Release it
   ...
   select
      S.P; -- Wait no longer than 0.5s
   or delay 0.5;
      raise Timed_Out;
   end select;
   ...
   S.V;    -- Release it
end;

```

It is also possible to implement semaphore as a monitor task.


## C

{{libheader|pthread}}

Here is an example of counting semaphores in C, using the "pthread" library. To make the code more readable, no error checks are made. A productive version of this implementation should check all the return values from the various function calls!

The example is divided into two parts: the "Interface" (usually the content of a *.h file)...

```c

//
// Interface
//
typedef struct Sema *Sema;

Sema Sema_New (int init);
void Sema_P   (Sema s);
void Sema_V   (Sema s);

```

... and the "Implementation" (the *.c file):

```c

//
// Implementation
//
#include <stdlib.h>
#include <pthread.h>

struct Sema {
    int value;
    pthread_mutex_t *mutex;
    pthread_cond_t  *cond;
};

Sema Sema_New (int init) {
    Sema s;

    s = malloc (sizeof (*s));
    s->value = init;
    s->mutex = malloc (sizeof (*(s->mutex)));
    s->cond = malloc (sizeof (*(s->cond)));
    pthread_mutex_init (s->mutex, NULL);
    pthread_cond_init (s->cond, NULL);

    return s;
}

void Sema_P (Sema s) {
    pthread_mutex_lock (s->mutex);
    while (s->value == 0) {
        pthread_cond_wait (s->cond, s->mutex);
    }
    s->value--;
    pthread_mutex_unlock (s->mutex);
}

void Sema_V (Sema s) {
    pthread_mutex_lock (s->mutex);
    s->value++;
    if (s->value == 1) {
        pthread_cond_signal (s->cond);
    }
    pthread_mutex_unlock (s->mutex);
}

```

