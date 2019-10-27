+++
title = "Talk:Function frequency"
description = ""
date = 2013-04-02T21:05:32Z
aliases = []
[extra]
id = 11017
[taxonomies]
categories = []
tags = []
+++

== Specification ==

It seems to me this could be better specified.  For instance this looks likes it is to be run against a source file.  However, it could equally be run against an entire library or collection.  For instance what are the statistics for a commonly used public library or Rosetta.  I know this complicates things and I'm not asking to change the task itself.  Just to clarify the intent. --[[User:Dgamey|Dgamey]] 12:23, 7 December 2011 (UTC)

: The other way to implement this (execution frequency) would also make a good task. I have used such facilities for performance profiling. --[[User:IanOsgood|IanOsgood]] 16:32, 23 April 2012 (UTC)

:: I assume by ''execution frequency'', you actually meant ''execution count'', as frequency implies how many executions per some period of time. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:15, 28 September 2012 (UTC)
:: Also, I misunderstood what the task's author meant when he specified "how often it is ''used'' by the programmer".  He probably meant ''specified'' (in the source) instead of actually ''using'' the function.  It was the word '''used''' that caused me to program the REXX version for a run-time analysis. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:15, 28 September 2012 (UTC)

=== What is a "function"? ===

Questions to ponder:  
# Is a macro a "function"?
# Is a parse-time word a "function"?
# Is a compiler directive a "function"?
# Is a lookup table a "function"?
# Is a lookup table a "function" in a language where lookup tables and procedure calls use the same syntax when mapping arguments to results?
# Are functions which are built into the language "functions"?
# Are language built-ins which are not functions (for example, the assignment operator) "functions"?
# Are constants "functions"?
# Are operations which produce constant results that take (and ignore) their arguments "functions"?
# Are constants functions in a language where language syntax allows numbers to take (and ignore) arguments "functions"?
# Are procedures with side effects "functions"?
# Are procedures which return void "functions"?
# Are OS calls "functions"?
# Are words which have not been defined "functions"?
# Are words which are not yet defined "functions" if they will later be defined to be procedures?
# Are words which are not yet defined "functions" if they will later be defined to be constants?
# Are words which are not yet defined "functions" if the language allows function definitions to be loaded lazily but does not allow constant definitions to be loaded lazily?

(All of these questions have languages where they are pertinent issues, though not all of these issues will be pertinent in the same language for any language that I know of). --[[User:Rdm|Rdm]] 20:03, 8 December 2011 (UTC)

strncmp() or memcmp() in C sometimes is compiled into "repz cmpsb" without jumping(CALL) anywhere in x86 assembly, so it's not a function!? I dont know :) --[[User:Spekkio|Spekkio]] 08:32, 9 December 2011 (UTC)

: The task obviously doesn't really care about very exact definitions, why not just take the liberty and do whatever you think fits the bill? The stated goal is to make a tool that may (or may not, even) help in analyzing a piece of source code, just keep that in mind and do something reasonable. --[[User:Ledrug|Ledrug]] 12:51, 9 December 2011 (UTC)
:: Right. I would propose to change "function" to "identifier" or "token", both in the task's title and in the description (unfortunately, I don't know how to do the former), and leave it open to the individual implementation to be more specific. --[[User:Abu|Abu]] 08:25, 10 December 2011 (UTC)
::: OK, added that, and explained the intent a bit --[[User:Abu|Abu]] 10:50, 11 December 2011 (UTC)

== C example: bugs and oddities ==

Here are some comments about the C example.  Some are bugs, others are just strange ways of doing stuff, in case someone wants to fix or improve it.  I realize that it's probably better if I actually edit the code instead of just whine about it here, but I really don't feel there's a good solution for this task in C short of writing a full compiler.  Whining is so much easier.

```c
#define _POSIX_SOURCE
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stddef.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
struct functionInfo {
   char* name;
   int timesCalled;
   char marked;
};

//strange line continuation; many more to come
//"list" is pointer to pointer, which gets confusing fast
void addToList(struct functionInfo** list, struct functionInfo toAdd, \
               size_t* numElements, size_t* allocatedSize) {
   //should not hardcode 32; e.g. "inline" is a keyword (this code itself is C99ish)
   static const char* keywords[32] = {"auto", "break", "case", "char", "const", \
                                      "continue", "default", "do", "double", \
                                      "else", "enum", "extern", "float", "for", \
                                      "goto", "if", "int", "long", "register", \
                                      "return", "short", "signed", "sizeof", \
                                      "static", "struct", "switch", "typedef", \
                                      "union", "unsigned", "void", "volatile", \
                                      "while"};
   /* If the "function" being called is actually a keyword, then ignore it */
   for (int i = 0;i < 32;i++) {
      if (!strcmp(toAdd.name, keywords[i])) {
         return;
      }
   }
   if (!*list) {
      *allocatedSize = 10;
      *list = calloc(*allocatedSize, sizeof(struct functionInfo));
      if (!*list) {
         printf("Failed to allocate %d elements of %d bytes each.\n", \
                *allocatedSize, sizeof(struct functionInfo));
         abort();
      }
      (*list)[0].name = malloc(strlen(toAdd.name)+1);
      if (!(*list)[0].name) {
         printf("Failed to allocate %d bytes.\n", strlen(toAdd.name)+1);
         abort();
      }
      strcpy((*list)[0].name, toAdd.name);
      (*list)[0].timesCalled = 1;
      (*list)[0].marked = 0;
      *numElements = 1;
   }
   else {
      char found = 0;
      for (unsigned int i = 0;i < *numElements;i++) {
         if (!strcmp((*list)[i].name, toAdd.name)) {
            found = 1;
            (*list)[i].timesCalled++;
            break;
         }
      }
      if (!found) {
         struct functionInfo* newList = calloc((*allocatedSize)+10, \
                                               sizeof(struct functionInfo));
         if (!newList) {
            printf("Failed to allocate %d elements of %d bytes each.\n", \
                   (*allocatedSize)+10, sizeof(struct functionInfo));
            abort();
         }
         memcpy(newList, *list, (*allocatedSize)*sizeof(struct functionInfo));
         free(*list);
         *allocatedSize += 10;
         *list = newList;
         (*list)[*numElements].name = malloc(strlen(toAdd.name)+1);
         if (!(*list)[*numElements].name) {
            printf("Failed to allocate %d bytes.\n", strlen(toAdd.name)+1);
            abort();
         }
         strcpy((*list)[*numElements].name, toAdd.name);
         (*list)[*numElements].timesCalled = 1;
         (*list)[*numElements].marked = 0;
         (*numElements)++;
      }
   }
}

//absolutely no reason to pass pointer to pointer here
void printList(struct functionInfo** list, size_t numElements) {
   char maxSet = 0;
   size_t maxIndex = 0;
   // a qsort would be much easier to understand
   for (unsigned int i = 0;i<10;i++) {
      maxSet = 0;
      for (size_t j = 0;j<numElements;j++) {
         if (!maxSet || (*list)[j].timesCalled > (*list)[maxIndex].timesCalled) {
            if (!(*list)[j].marked) {
               maxSet = 1;
               maxIndex = j;
            }
         }
      }
      (*list)[maxIndex].marked = 1;
      printf("%s() called %d times.\n", (*list)[maxIndex].name, \
             (*list)[maxIndex].timesCalled);
   }
}
//same pointer to pointer issue
void freeList(struct functionInfo** list, size_t numElements) {
   for (size_t i = 0;i<numElements;i++) {
      free((*list)[i].name);
   }
   free(*list);
}
char* extractFunctionName(char* readHead) {
   char* identifier = readHead;
   if (isalpha(*identifier) || *identifier == '_') {
      while (isalnum(*identifier) || *identifier == '_') {
         identifier++;
      }
   }
   /* Search forward for spaces and then an open parenthesis
    * but do not include this in the function name.
    */
   char* toParen = identifier;
   if (toParen == readHead) return NULL;
   while (isspace(*toParen)) {//except there could be a backslash line continuation
      toParen++;
   }
   if (*toParen != '(') return NULL;
   /* Copy the found function name to the output string */
   ptrdiff_t size = (ptrdiff_t)((ptrdiff_t)identifier) \
                    - ((ptrdiff_t)readHead)+1;
   char* const name = malloc(size); //why const? it's going to be written to
   if (!name) {
      printf("Failed to allocate %d bytes.\n", size);
      abort();
   }
   name[size-1] = '\0';
   memcpy(name, readHead, size-1);
   /* Function names can't be blank */
   if (strcmp(name, "")) {
      return name;
   }
   // this can never happen; if it were to, name would leak
   return NULL;
}
int main(int argc, char** argv) {
   for (int i = 1;i<argc;i++) {
      errno = 0;
      FILE* file = fopen(argv[i], "r"); // should use open instead of fopen
      // it's unusual, probably just wrong, to check errno before return value
      if (errno || !file) {
         printf("fopen() failed with error code \"%s\"\n", \
                strerror(errno));
         abort();
      }
      char comment = 0;
      #define DOUBLEQUOTE 1
      #define SINGLEQUOTE 2
      int string = 0;
      struct functionInfo* functions = NULL;
      struct functionInfo toAdd;
      size_t numElements = 0;
      size_t allocatedSize = 0;
      struct stat metaData;
      errno = 0;
      if (fstat(fileno(file), &metaData) < 0) {
         printf("fstat() returned error \"%s\"\n", strerror(errno));
         abort();
      }
      char* mmappedSource = (char*)mmap(NULL, metaData.st_size, PROT_READ, \
                                        MAP_PRIVATE, fileno(file), 0);
      if (errno) {
         printf("mmap() failed with error \"%s\"\n", strerror(errno));
         abort();
      }
      if (!mmappedSource) {
         printf("mmap() returned NULL.\n");
         abort();
      }
      char* readHead = mmappedSource;
      while (readHead < mmappedSource + metaData.st_size) {
         while (*readHead) { // nonsense check: it's mmap'ed, not null-padded
            /* Ignore comments inside strings */
            if (!string) {
	       // if '/' is the last char in source file, strncmp will read into
	       // undefined memory
               if (*readHead == '/' && !strncmp(readHead, "/*", 2)) {
                  comment = 1;
               }
               if (*readHead == '*' && !strncmp(readHead, "*/", 2)) {
                  comment = 0;
               }
            }
            /* Ignore strings inside comments */
            if (!comment) {
               if (*readHead == '"') {
                  if (!string) {
                     string = DOUBLEQUOTE;
                  }
                  else if (string == DOUBLEQUOTE) {
                     /* Only toggle string mode if the quote character
                      * is not escaped
                      */
		     // if first char of source file is a quote, strncmp will 
		     // read before valid memory position
                     if (strncmp((readHead-1), "\\\"", 2)) {
                        string = 0;
                     }
                  }
               }
               if (*readHead == '\'') {
                  if (!string) {
                     string = SINGLEQUOTE;
                  }
                  else if (string == SINGLEQUOTE) {
		     // ditto
                     if (strncmp((readHead-1), "\\\'", 2)) {
                        string = 0;
                     }
                  }
               }
            }
            /* Look for identifiers outside of any comment or string */
            if (!comment && !string) {
               char* name = extractFunctionName(readHead);
               /* Don't read part of an identifier on the next iteration */
               if (name) {
                  toAdd.name = name;
                  addToList(&functions, toAdd, &numElements, &allocatedSize);
                  readHead += strlen(name);
               }
               free(name); //might as well move this into the if block
            }
            readHead++;
         }
      }
      // ^^ some other situations are not handled; "//" line comment is one,
      // common "#if 0" and "if (0)" the other, though the latter requires
      // understand syntax instead of just string matching, so probably shouldn't
      // be faulted to the code here
      errno = 0;
      munmap(mmappedSource, metaData.st_size);
      if (errno) {
         printf("munmap() returned error \"%s\"\n", strerror(errno));
         abort();
      }
      errno = 0;
      fclose(file);
      if (errno) {
         printf("fclose() returned error \"%s\"\n", strerror(errno));
         abort();
      }
      printList(&functions, numElements);
      freeList(&functions, numElements);
   }
   return 0;
}
```


== REXX solution looks like run time counting ?! ==

Task asks for static analysis!

: Yes it does.  I was trying to address the intent of this task.  This is a best-effort solution to provide the closest thing that I could conceive of to solve the task at hand for a program or runtime environment (I choose a program environment).  It may not adhere to the definition of a static analysis, but it has a good start at the intent of the task.  If anything, I hoped it would give someone an idea how this could be accomplished by just using some simple REXX programming techniques for doing self-inspection within the REXX language. If the solution is that objectionable, it can be superceded by a better solution of course. Perhaps an object-oriented version of REXX (which has more tools for something like this) would be better suited to fulfill this task.  I feel that this REXX solution has a practical usefulness, albeit simple and straightforward. -- [[User:Gerard Schildberger|Gerard Schildberger]] 14:01, 4 September 2012 (UTC) 

In the task description 'suites' should be 'suits' ?
But I'm not a native speaker--[[User:Walterpachl|Walterpachl]] 07:44, 4 September 2012 (UTC)

== Ada ==

In the real world, you probably would not try and write a task like this in Ada because string handling is less flexible than in say Basic. (One of the main authors of Ada admits this). If it would be considered acceptable, I may try and do this in Visual Basic or Delphi. It still would not be trivial so I would not wish to start unless I was assured that It would count. [[User:Op47|Op47]] ([[User talk:Op47|talk]]) 21:05, 2 April 2013 (UTC)
