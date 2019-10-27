+++
title = "Talk:Synchronous concurrency"
description = ""
date = 2010-09-07T06:09:43Z
aliases = []
[extra]
id = 2177
[taxonomies]
categories = []
tags = []
+++

== Strange requirement ==

This aricle currently contains the following text: "This task should not be implemented using fork, spawn, or the linux/unix/Win32 pipe command." I think this is a very strange requirement for a task where "The goal of this task is to create two concurrent activities that share data synchronously". I think that goal actually describes one of the most common uses of fork(), spawn(), pipes and similar constructs.
I suggest we remove this requirement. [[User:Ahy1|Ahy1]] 12:43, 21 October 2007 (MDT)
:I believe the task was intended to demonstrate coordinated action between two independent processes.  Waldorf originally wrote the task; Perhaps he could chime in? --[[User:Short Circuit|Short Circuit]] 13:17, 21 October 2007 (MDT)
:I was looking specifically for concurrent tasks within a single program. Many languages call these concurrent units threads. Ada calls them tasks. There are other approaches and names for such capabilities. What I did ''not'' want was program-to-program communication as is common using fork, spawn, and pipe. Some languages, such as Java, Ada, and ML, have concurrent features built in. Some languages, such as C and C++, use external libraries to achieve similar ends. Some languages have no concurrency capabilities. I would not expect examples from languages with no capabilities. [[User:Waldorf|Waldorf]] 15:08, 21 October 2007 (MDT)
::So you're making a distinction between threads and processes? That's probably the source of confusion.  I'll go ahead and clarify. --[[User:Short Circuit|Short Circuit]] 19:42, 21 October 2007 (MDT)
:::I clarified it somewhat, and added links to [[Thread]] and [[Process]], should anyone be interested in filling in some details on those pages.  Jim, take a look at at Synchronous Concurrency, and make sure my changes didn't change alter the spirit of the task. --[[User:Short Circuit|Short Circuit]] 20:00, 21 October 2007 (MDT)
::::I have no problem with your clarification. --[[User:Waldorf|Waldorf]] 20:55, 21 October 2007 (MDT)
::::Now it is much clearer. Should there be a similar task that allows communication between processes? The [[Fork Process]] is the closest I can find, but doesn't involve communication. [[User:Ahy1|Ahy1]] 23:11, 21 October 2007 (MDT)
::::Communication between processes is always an operating system feature. Some languages such as Shell provide built-in communication through pipes, which were originally a Unix feature. Other forms of inter-process communication should include signals, semaphores, message queues, and in at least one operating system, message files. Implementations of pipes, for instance, will be fundamentally the same for all languages. The reason for that is that the API for any given Operating System is fixed for all languages. I do not think such a task will reveal very much about languages. [[User:Waldorf|Waldorf]] 19:30, 4 November 2007 (MST)
::::: I have added the Unix Pipes example, and it does reveal some thing about the language, as its usage defines the language :) perhaps It should be allowed?  Also the distinction you seem to be making is probably better put as between shared state concurrency and message passing concurrency, as the syntax and semantics of the language may not rely on the exact same operating system features every time (For e.g if I implement bash over JVM, I would not be using pipes, but that would not be visible to the user.). [[User:Rahul|Rahul]] 13:54, 15 April 2008 (MDT)

== Python ==

Alernatively the ''write()'' function here could be replaced with a '''''Writer''''' class like:

 class Writer(object):
    def __init__(self, filehandle, queue):
         self.linecount = 0
         if not hasattr(queue, 'get'):
             raise TypeError, 'filehandle must support "write()" method'
         else:
             self.file = filehandle
         if not hasattr(queue, 'get'):
             raise TypeError, 'Queue handle must support "get()" method'
         self.queue = queue
    def __call__(self):
         while True:
             line = self.queue.get()
             if line is None:
                 break
             self.file.write(line)
             self.linecount += 1

:There is not point in checking for filehandle attributes, if someone send you broken filehandle, it will raise AttribtueError anyway.

... which keep the "linecount" attribute encapsulated to allow the main code path

:The line count is already encapsulated in the write function. Why use a class when you can get the job done with a simple function?

to access it separately using something like:

 write = Writer(sys.stdout, lines)
 reader = Thread(target=read, args=(open('input.txt'),))
 writer = Thread(target=write)
 reader.start()
 writer.start()
 reader.join()
 writer.join()
 print "Line count: ", write.linecount

:Accessing the line count like this require synchronization between the reader and the writer and locking. Using a queue as in the example code make everything simpler and safer. as you explain bellow :-)

... (Though this also requires that we remove the final ''print'' statement from the ''read()'' function --- otherwise the reader() thread won't "join" because of the last item remaining it the "count" queue).

:The task description ask the reader to print.

In general it's cleaner to use the Queue objections for inter-thread communications in lieu of explicit, error prone and complicated locking.  Python Queue objects are coherent thread-safe "producer-consumer" pipelines which are suitable for any combination of single or multiple producer and consumer threads.  Since objects of any sort can be passed through a Queue it would be trivial to encapsulate each line read in an object specifying a target file object along with the data to be written.  The ''Writer'' instance could then count each of these as it called something like "line.write(line.data)" (for example).  Obviously the ''read()'' function in the original example could also be replaced with a class which could allow it to maintain any desired state or implement additional behavior.

:Obviously you can write this in many complecated ways, but here we need the simplest way that conform with the task description and is a good example of using Python for this task.

== Message passing ==

It seems to me many examples don't request the line count from the printer thread, instead maintaining state on their own. Should they be labelled incorrect? [[User talk:foobie-bletch|Foobie Bletch]] 16:21, 11 August 2009 (UTC)

== Illegal Character in ALGOL 68 Example ==

I see that there's at least several characters in the ALGOL 68 example that are “illegal”. Those characters are definitely in the data being supplied in the data from RC, so they're wrong in the database. This means that fixing is not just a matter of looking at what was supplied; it needs knowledge of how to actually write the code. Thus… ''does anyone know what they should be fixed to?'' If so, please step right in and fix it! –[[User:Dkf|Donal Fellows]] 06:09, 7 September 2010 (UTC)
