+++
title = "Function frequency"
description = ""
date = 2019-10-10T16:10:37Z
aliases = []
[extra]
id = 10768
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
+++

## Task

Display - for a program or runtime environment (whatever suits the style of your language) - the top ten most frequently occurring functions (or also identifiers or tokens, if preferred).

This is a static analysis: The question is not how often each function is
actually executed at runtime, but how often it is used by the programmer.

Besides its practical usefulness, the intent of this task is to show how to do self-inspection within the language.


## ACL2


This will, unfortunately, also catch variable names after an open paren, such as in <code>(let ...)</code> expressions.

```Lisp
(in-package "ACL2")

(set-state-ok t)

(defun read-all-objects (limit channel state)
   (mv-let (eof obj state)
           (read-object channel state)
      (if (or eof (zp limit))
          (mv nil state)
          (mv-let (so-far state)
                  (read-all-objects (- limit 1) channel state)
             (mv (cons obj so-far) state)))))

(defun list-starters (xs)
   (cond ((endp xs) nil)
         ((consp (first xs))
          (append (if (symbolp (first (first xs)))
                      (list (first (first xs)))
                      nil)
                  (list-starters (rest (first xs)))
                  (list-starters (rest xs))))
         (t (list-starters (rest xs)))))

(defun invoked-functions (filename state)
   (mv-let (channel state)
           (open-input-channel filename :object state)
      (mv-let (code state)
              (read-all-objects 1000 channel state)
         (mv (list-starters code) state))))

(defun increment-for (key alist)
   (cond ((endp alist) (list (cons key 1)))
         ((equal key (car (first alist)))
          (cons (cons key (1+ (cdr (first alist))))
                (rest alist)))
         (t (cons (first alist)
                  (increment-for key (rest alist))))))

(defun symbol-freq-table (symbols)
   (if (endp symbols)
       nil
       (increment-for (first symbols)
                      (symbol-freq-table (rest symbols)))))

(defun insert-freq-table (pair alist)
   (cond ((endp alist)
          (list pair))
         ((> (cdr pair) (cdr (first alist)))
          (cons pair alist))
         (t (cons (first alist)
                  (insert-freq-table pair (rest alist))))))

(defun isort-freq-table (alist)
   (if (endp alist)
       nil
       (insert-freq-table (first alist)
                          (isort-freq-table (rest alist)))))

(defun main (state)
   (mv-let (fns state)
           (invoked-functions "function-freq.lisp" state)
      (mv (take 10 (isort-freq-table
                    (symbol-freq-table fns))) state)))
```

Output (for itself):

```txt
(((FIRST . 10)
  (REST . 8)
  (DEFUN . 8)
  (CONS . 7)
  (MV-LET . 5)
  (LIST-STARTERS . 4)
  (IF . 4)
  (MV . 4)
  (COND . 3)
  (LIST . 3))
 <state>)
```


## AWK


```AWK

# syntax: GAWK -f FUNCTION_FREQUENCY.AWK filename(s).AWK
#
# sorting:
#   PROCINFO["sorted_in"] is used by GAWK
#   SORTTYPE is used by Thompson Automation's TAWK
#
BEGIN {
# create array of keywords to be ignored by lexer
    asplit("BEGIN:END:atan2:break:close:continue:cos:delete:" \
           "do:else:exit:exp:for:getline:gsub:if:in:index:int:"  \
           "length:log:match:next:print:printf:rand:return:sin:" \
           "split:sprintf:sqrt:srand:strftime:sub:substr:system:tolower:toupper:while",
           keywords,":")
# build the symbol-state table
    split("00:00:00:00:00:00:00:00:00:00:" \
          "20:10:10:12:12:11:07:00:00:00:" \
          "08:08:08:08:08:33:08:00:00:00:" \
          "08:44:08:36:08:08:08:00:00:00:" \
          "08:44:45:42:42:41:08",machine,":")
# parse the input
    state = 1
    for (;;) {
      symb = lex() # get next symbol
      nextstate = substr(machine[state symb],1,1)
      act = substr(machine[state symb],2,1)
      # perform required action
      if (act == "0") { # do nothing
      }
      else if (act == "1") { # found a function call
        if (!(inarray(tok,names))) {
          names[++nnames] = tok
        }
        ++xnames[tok]
      }
      else if (act == "2") { # found a variable or array
        if (tok in Local) {
          tok = tok "(" funcname ")"
          if (!(inarray(tok,names))) {
            names[++nnames] = tok
          }
          ++xnames[tok]
        }
        else {
          tok = tok "()"
          if (!(inarray(tok,names))) {
            names[++nnames] = tok
          }
          ++xnames[tok]
        }
      }
      else if (act == "3") { # found a function definition
        funcname = tok
      }
      else if (act == "4") { # found a left brace
        braces++
      }
      else if (act == "5") { # found a right brace
        braces--
        if (braces == 0) {
          delete Local
          funcname = ""
          nextstate = 1
        }
      }
      else if (act == "6") { # found a local variable declaration
        Local[tok] = 1
      }
      else if (act == "7") { # found end of file
        break
      }
      else if (act == "8") { # found an error
        printf("error: FILENAME=%s, FNR=%d\n",FILENAME,FNR)
        exit(1)
      }
      state = nextstate # finished with current token
    }
# format function names
    for (i=1; i<=nnames; i++) {
      if (index(names[i],"(") == 0) {
        tmp_arr[xnames[names[i]]][names[i]] = ""
      }
    }
# print function names
    PROCINFO["sorted_in"] = "@ind_num_desc" ; SORTTYPE = 9
    for (i in tmp_arr) {
      PROCINFO["sorted_in"] = "@ind_str_asc" ; SORTTYPE = 1
      for (j in tmp_arr[i]) {
        if (++shown <= 10) {
          printf("%d %s\n",i,j)
        }
      }
    }
    exit(0)
}
function asplit(str,arr,fs,  i,n,temp_asplit) {
    n = split(str,temp_asplit,fs)
    for (i=1; i<=n; i++) {
      arr[temp_asplit[i]]++
    }
}
function inarray(val,arr,  j) {
    for (j in arr) {
      if (arr[j] == val) {
        return(j)
      }
    }
    return("")
}
function lex() {
    for (;;) {
      if (tok == "(eof)") {
        return(7)
      }
      while (length(line) == 0) {
        if (getline line == 0) {
          tok = "(eof)"
          return(7)
        }
      }
      sub(/^[ \t]+/,"",line)           # remove white space,
      sub(/^"([^"]|\\")*"/,"",line)    # quoted strings,
      sub(/^\/([^\/]|\\\/)+\//,"",line) # regular expressions,
      sub(/^#.*/,"",line)              # and comments
      if (line ~ /^function /) {
        tok = "function"
        line = substr(line,10)
        return(1)
      }
      else if (line ~ /^{/) {
        tok = "{"
        line = substr(line,2)
        return(2)
      }
      else if (line ~ /^}/) {
        tok = "}"
        line = substr(line,2)
        return(3)
      }
      else if (match(line,/^[A-Za-z_][A-Za-z_0-9]*\[/)) {
        tok = substr(line,1,RLENGTH-1)
        line = substr(line,RLENGTH+1)
        return(5)
      }
      else if (match(line,/^[A-Za-z_][A-Za-z_0-9]*\(/)) {
        tok = substr(line,1,RLENGTH-1)
        line = substr(line,RLENGTH+1)
        if (!(tok in keywords)) { return(6) }
      }
      else if (match(line,/^[A-Za-z_][A-Za-z_0-9]*/)) {
        tok = substr(line,1,RLENGTH)
        line = substr(line,RLENGTH+1)
        if (!(tok in keywords)) { return(4) }
      }
      else {
        match(line,/^[^A-Za-z_{}]/)
        tok = substr(line,1,RLENGTH)
        line = substr(line,RLENGTH+1)
      }
    }
}

```

<p>Output of running FUNCTION_FREQUENCY.AWK on itself:</p>

```txt

3 inarray
1 asplit
1 lex

```

<p>Sample input:</p>

```txt

BEGIN {
    f1()
    f2();f2()
    f3a();f3a();f3a()
    f3b();f3b();f3b()
    f4();f4();f4();f4()
    f5();f5();f5();f5();f5()
    exit(0)
}
function f0() { }
function f1() { }
function f2() { }
function f3a() { }
function f3b() { }
function f4() { }
function f5() { }

```

<p>Sample output:</p>

```txt

5 f5
4 f4
3 f3a
3 f3b
2 f2
1 f1

```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      Sort% = FN_sortinit(1,0) : REM Descending
      
      Valid$ = "0123456789@ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijklmnopqrstuvwxyz"
      DIM func$(1000), cnt%(1000)
      nFunc% = 0
      
      file% = OPENIN("*.bbc")
      WHILE NOT EOF#file%
        ll% = BGET#file%
        no% = BGET#file% + 256*BGET#file%
        INPUT #file%, l$
        
        i% = 1
        REPEAT
          j% = INSTR(l$, CHR$&A4, i%) : REM Token for 'FN'
          k% = INSTR(l$, CHR$&F2, i%) : REM Token for 'PROC'
          IF k% IF j%=0 OR j%>k% THEN
            i% = k%
            f$ = "PROC"
          ELSE
            i% = j%
            f$ = "FN"
          ENDIF
          IF i% THEN
            REPEAT
              i% += 1
              f$ += MID$(l$, i%, 1)
            UNTIL INSTR(Valid$, MID$(l$, i%+1, 1)) = 0
            FOR j% = 0 TO nFunc%-1
              IF f$ = func$(j%) EXIT FOR
            NEXT
            IF j% >= nFunc% nFunc% += 1
            func$(j%) = f$
            cnt%(j%) += 1
          ENDIF
        UNTIL i%=0
      ENDWHILE
      CLOSE #file%
      
      C% = nFunc%
      CALL Sort%, cnt%(0), func$(0)
      
      IF C% > 10 C% = 10
      FOR i% = 0 TO C%-1
        PRINT func$(i%) " (" ; cnt%(i%) ")"
      NEXT
```

'''Output (for file LBB.BBC):'''

```txt

FNcheck (291)
FNexpr (125)
FNtoken (49)
PROCout (41)
FNhandle (31)
FNupper (30)
FNitem (30)
FNcheckns (21)
FNinstrq (17)
FNchild (17)

```



## C

This program treats doesn't differentiate between macros and functions. It works by looking for function calls which are not inside strings or comments.  If a function call has a C style comment between the opening brace and the name of the function, this program will not recognize it as a function call.

```C

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
void addToList(struct functionInfo** list, struct functionInfo toAdd, \
               size_t* numElements, size_t* allocatedSize)
{
    static const char* keywords[32] = {"auto", "break", "case", "char", "const", \
                                       "continue", "default", "do", "double", \
                                       "else", "enum", "extern", "float", "for", \
                                       "goto", "if", "int", "long", "register", \
                                       "return", "short", "signed", "sizeof", \
                                       "static", "struct", "switch", "typedef", \
                                       "union", "unsigned", "void", "volatile", \
                                       "while"
                                      };
    int i;
    /* If the "function" being called is actually a keyword, then ignore it */
    for (i = 0; i < 32; i++) {
        if (!strcmp(toAdd.name, keywords[i])) {
            return;
        }
    }
    if (!*list) {
        *allocatedSize = 10;
        *list = calloc(*allocatedSize, sizeof(struct functionInfo));
        if (!*list) {
            printf("Failed to allocate %lu elements of %lu bytes each.\n", \
                   *allocatedSize, sizeof(struct functionInfo));
            abort();
        }
        (*list)[0].name = malloc(strlen(toAdd.name)+1);
        if (!(*list)[0].name) {
            printf("Failed to allocate %lu bytes.\n", strlen(toAdd.name)+1);
            abort();
        }
        strcpy((*list)[0].name, toAdd.name);
        (*list)[0].timesCalled = 1;
        (*list)[0].marked = 0;
        *numElements = 1;
    } else {
        char found = 0;
        unsigned int i;
        for (i = 0; i < *numElements; i++) {
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
                printf("Failed to allocate %lu elements of %lu bytes each.\n", \
                       (*allocatedSize)+10, sizeof(struct functionInfo));
                abort();
            }
            memcpy(newList, *list, (*allocatedSize)*sizeof(struct functionInfo));
            free(*list);
            *allocatedSize += 10;
            *list = newList;
            (*list)[*numElements].name = malloc(strlen(toAdd.name)+1);
            if (!(*list)[*numElements].name) {
                printf("Failed to allocate %lu bytes.\n", strlen(toAdd.name)+1);
                abort();
            }
            strcpy((*list)[*numElements].name, toAdd.name);
            (*list)[*numElements].timesCalled = 1;
            (*list)[*numElements].marked = 0;
            (*numElements)++;
        }
    }
}
void printList(struct functionInfo** list, size_t numElements)
{
    char maxSet = 0;
    unsigned int i;
    size_t maxIndex = 0;
    for (i = 0; i<10; i++) {
        maxSet = 0;
        size_t j;
        for (j = 0; j<numElements; j++) {
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
void freeList(struct functionInfo** list, size_t numElements)
{
    size_t i;
    for (i = 0; i<numElements; i++) {
        free((*list)[i].name);
    }
    free(*list);
}
char* extractFunctionName(char* readHead)
{
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
    while (isspace(*toParen)) {
        toParen++;
    }
    if (*toParen != '(') return NULL;
    /* Copy the found function name to the output string */
    ptrdiff_t size = (ptrdiff_t)((ptrdiff_t)identifier) \
                     - ((ptrdiff_t)readHead)+1;
    char* const name = malloc(size);
    if (!name) {
        printf("Failed to allocate %lu bytes.\n", size);
        abort();
    }
    name[size-1] = '\0';
    memcpy(name, readHead, size-1);
    /* Function names can't be blank */
    if (strcmp(name, "")) {
        return name;
    }
    free(name);
    return NULL;
}
int main(int argc, char** argv)
{
    int i;
    for (i = 1; i<argc; i++) {
        errno = 0;
        FILE* file = fopen(argv[i], "r");
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
            while (*readHead) {
                /* Ignore comments inside strings */
                if (!string) {
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
                        } else if (string == DOUBLEQUOTE) {
                            /* Only toggle string mode if the quote character
                             * is not escaped
                             */
                            if (strncmp((readHead-1), "\\\"", 2)) {
                                string = 0;
                            }
                        }
                    }
                    if (*readHead == '\'') {
                        if (!string) {
                            string = SINGLEQUOTE;
                        } else if (string == SINGLEQUOTE) {
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
                    free(name);
                }
                readHead++;
            }
        }
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



## Common Lisp

Loading the file itself before scanning it is the quickest way to determine what function bindings would be created.

```lisp
(defun mapc-tree (fn tree)
  "Apply FN to all elements in TREE."
  (cond ((consp tree)
         (mapc-tree fn (car tree))
         (mapc-tree fn (cdr tree)))
        (t (funcall fn tree))))

(defun count-source (source)
  "Load and count all function-bound symbols in a SOURCE file."
  (load source)
  (with-open-file (s source)
    (let ((table (make-hash-table)))
      (loop for data = (read s nil nil)
         while data
         do (mapc-tree
             (lambda (x)
               (when (and (symbolp x) (fboundp x))
                 (incf (gethash x table 0))))
             data))
      table)))

(defun hash-to-alist (table)
  "Convert a hashtable to an alist."
  (let ((alist))
    (maphash (lambda (k v) (push (cons k v) alist)) table)
    alist))

(defun take (n list)
  "Take at most N elements from LIST."
  (loop repeat n for x in list collect x))

(defun top-10 (table)
  "Get the top 10 from the source counts TABLE."
  (take 10 (sort (hash-to-alist table) '> :key 'cdr)))
```

```txt
CL-USER> (top-10 (count-source "function-frequency.lisp"))
((DEFUN . 5) (MAPC-TREE . 4) (QUOTE . 2) (LIST . 2) (TAKE . 2)
 (HASH-TO-ALIST . 2) (LAMBDA . 2) (LOOP . 2) (LET . 2) (CDR . 2))
```



## Erlang

This is source code analyse. Mainly because I have never done that before.

```Erlang

-module( function_frequency ).

-export( [erlang_source/1, task/0] ).

erlang_source( File ) ->
    {ok, IO} = file:open( File, [read] ),
    Forms = parse_all( IO, io:parse_erl_form(IO, ''), [] ),
    Functions = lists:flatten( [erl_syntax_lib:fold(fun accumulate_functions/2, [], X) || X <- Forms] ),
    dict:to_list( lists:foldl(fun count/2, dict:new(), Functions) ).

task() ->
    Function_frequencies = erlang_source( "function_frequency.erl" ),
    {Top_tens, _Rest} = lists:split( 10, lists:reverse(lists:keysort(2, Function_frequencies)) ),
    [io:fwrite("Function ~p called ~p times.~n", [X, Y]) || {X, Y} <- Top_tens].



accumulate_functions( Tree, Acc ) -> accumulate_functions( erlang:element(1, Tree), Tree, Acc ).

accumulate_functions( call, Tree, Acc ) -> [accumulate_functions_name(Tree) | Acc];
accumulate_functions( _Other, _Tree, Acc ) -> Acc.

accumulate_functions_name( Tree ) -> accumulate_functions_name_scoop( erlang:element(3, Tree) ).

accumulate_functions_name_scoop( {atom, _Line, Name} ) -> Name;
accumulate_functions_name_scoop( {remote, _Line, {atom, _Line, Module}, {atom, _Line, Name}} ) -> {Module, Name}.

count( Key, Dict ) -> dict:update_counter( Key, 1, Dict ).

parse_all( _IO, {eof, _End}, Acc ) -> Acc;
parse_all( IO, {ok, Tokens, Location}, Acc ) -> parse_all( IO, io:parse_erl_form(IO, '', Location), [Tokens | Acc] ).

```

```txt

7> function_frequency:task().
Function parse_all called 2 times.
Function {erlang,element} called 2 times.
Function {io,parse_erl_form} called 2 times.
Function {lists,flatten} called 1 times.
Function {dict,update_counter} called 1 times.
Function {lists,reverse} called 1 times.
Function {lists,foldl} called 1 times.
Function {io,fwrite} called 1 times.
Function accumulate_functions_name called 1 times.
Function erlang_source called 1 times.

```



## Factor

Let's take a look at the <code>sequences</code> vocabulary/source file from Factor's standard library. This approach does not count word-defining words such as <code>:</code> and <code>M:</code>, nor does it count words like <code>{</code> and <code>[</code>.

```factor
USING: accessors kernel math.statistics prettyprint sequences
sequences.deep source-files vocabs words ;

"resource:core/sequences/sequences.factor" "sequences"
[ path>source-file top-level-form>> ]
[ vocab-words [ def>> ] [ ] map-as ] bi* compose [ word? ]
deep-filter sorted-histogram <reversed> 7 head .
```

```txt

{
    { if 54 }
    { dup 53 }
    { drop 46 }
    { dip 44 }
    { swap 42 }
    { length 39 }
    { keep 36 }
}

```



## Forth

Counts colon definitions, variables, constants, words defined by definers like CREATE..DOES>, etc.  Check for levels of top from command line, by default 4.  GForth 0.7.0 specific.

```Forth
' noop is bootmessage

\ --- LIST OF CONSTANTS
\ WORD#		maximum word size
\ RING#		size of `Rings' element
\ DEFS		definitions
\ KEYS
\
\ --- LIST OF VARIABLES
\ cmpl?		is compiling?
\ cword		current compiled word

    wordlist constant DEFS
    wordlist constant KEYS

\ --- Compiling
50 constant WORD#
: >>fPAD	( ca u -- ; u < 51 )
	PAD 80 blank s" create " PAD swap MOVE
	s"  1 , DOES> 1 swap +! ;" PAD 57 + swap MOVE
	WORD# min PAD 7 + swap MOVE ;

: funcmpl	( ca u -- )
	>>fPAD current @ DEFS current !
	PAD 80 evaluate current ! ;

: >>kPAD	( ca u -- ; )
	PAD 80 blank s" : " PAD swap MOVE
	s"  parse-name funcmpl ;" PAD 59 + swap MOVE
	WORD# min PAD 2 + swap MOVE ;

: keycmpl	( ca u -- )
	 >>kPAD current @ KEYS current !
	PAD 80 evaluate current ! ;

\ --- Interpreter
: intp	BEGIN parse-name dup
	WHILE	( ca u )
		2dup KEYS search-wordlist
		IF   execute 2drop
		ELSE DEFS search-wordlist IF execute THEN
		THEN
	REPEAT 2drop ;

: run 	BEGIN refill WHILE intp REPEAT ;

\ --- Lists&Rings
warnings OFF
: LIST	( node -- )	]] BEGIN @ dup WHILE >R [[ ; immediate
warnings ON
: LOOP-LIST	( -- )	]] R> REPEAT drop [[ ; immediate

: empty-ring?	( node -- f )	dup @ = ;
: RING	( node -- )	]] dup BEGIN @ 2dup <> WHILE 2>R [[ ; immediate
: LOOP-RING	( -- )	]] 2R> REPEAT 2drop [[ ; immediate

: new-node	( -- node )
	here dup , ;
: do-link		( node new-node  -- ; do link after current node )
	over @ over ! swap ! ;

\ --- Sorting..
: nt>freq	( nt -- n ;frequency of uses )
	name>int >BODY @ ;

: @maxfreq	( wid -- n ;maximum frequency )
	0 swap cell+
	LIST	( max )
		I nt>freq 2dup <
		IF nip ELSE drop THEN
	LOOP-LIST ;

    2 cells constant RING#
: rings-vec	( u -- a size ; create vector of rings )
	here over 1+ 0
	DO new-node drop 0 , LOOP
	swap RING# * ;

: populate-by 	( a wid -- )
	cell+
	LIST
		dup  I nt>freq RING# *   +	\ root-node
		new-node I ,			\ new-node
		do-link
	LOOP-LIST drop ;

\ --- Display TOP
: node>nt	cell+ @ ;

: .ring		( root-node -- )
	0 swap
	RING
		dup 0= IF I node>nt nt>freq . THEN
		space I node>nt name>string type
		1+
	LOOP-RING drop cr ;

: .top	( a size n -- )
	-rot BOUNDS swap
	?DO	( n )
		I empty-ring?	0= IF 1- I .ring THEN
		dup 		0= IF drop UNLOOP EXIT THEN
	[ RING# negate ] LITERAL +LOOP drop ;

: args>top#	( -- n )
	1 arg 2dup 0 0 d<>
	IF	>float
		IF 	f>d d>s dup 0= IF drop 4 THEN
		ELSE 	4 THEN
	ELSE	2drop 4 THEN ;


\ --- KEYS behaviour
    variable  cmpl?	cmpl? OFF
    2variable cword
here WORD# allot 0 cword 2!

current @ KEYS current !
: create
	cmpl? @
	IF   cword 2@   keycmpl
	ELSE parse-name funcmpl THEN ;

: constant
	cmpl? @
	IF   cword 2@   keycmpl
	ELSE parse-name funcmpl THEN ;

: variable	parse-name funcmpl ;
: value		parse-name funcmpl ;
: defer		parse-name funcmpl ;

: (	BEGIN >in @ [char] ) parse nip >in @ rot - =
	WHILE refill 0= IF exit THEN REPEAT ;
: \	10 parse 2drop ;
: \G	10 parse 2drop ;
: S"	[char] " parse 2drop ;
: ."	[char] " parse 2drop ;

: [']
	parse-name DEFS search-wordlist IF execute THEN ;
: postpone
	parse-name DEFS search-wordlist IF execute THEN ;

: ; 	cmpl? OFF ;
: :	warnings OFF
	parse-name
	cword 2@ drop WORD# rot  umin dup >R MOVE
	cword 2@ drop R> cword 2!
	cword 2@ cmpl? @
	IF	keycmpl		\ `:' inside def. = a defining word
	ELSE	funcmpl	THEN
	cmpl? ON
	warnings ON
;
current !

\ Run, ruuun!
stdin ' run execute-parsing-file  DEFS @maxfreq rings-vec  over DEFS populate-by  args>top# .top bye

```

Self test:

```txt

$ 
$ gforth funfreq.fs 10 < funfreq.fs 
7  DEFS funcmpl cmpl?
5  WORD#
4  KEYS keycmpl nt>freq RING#
3  LIST LOOP-LIST new-node node>nt
2  >>fPAD >>kPAD intp run empty-ring? RING LOOP-RING do-link @maxfreq rings-vec populate-by .ring .top args>top#
1  create constant variable value defer ( \ \G S" ." ['] postpone ; :
$ 
$ gforth funfreq.fs < funfreq.fs    
7  DEFS funcmpl cmpl?
5  WORD#
4  KEYS keycmpl nt>freq RING#
3  LIST LOOP-LIST new-node node>nt
$

```



## Go

Only crude approximation is currently easy in Go.  The following parses source code, looks for function call syntax (an expression followed by an argument list) and prints the expression.

```go
package main

import (
    "fmt"
    "go/ast"
    "go/parser"
    "go/token"
    "io/ioutil"
    "os"
    "sort"
)

func main() {
    if len(os.Args) != 2 {
        fmt.Println("usage ff <go source filename>")
        return
    }
    src, err := ioutil.ReadFile(os.Args[1])
    if err != nil {
        fmt.Println(err)
        return
    }
    fs := token.NewFileSet()
    a, err := parser.ParseFile(fs, os.Args[1], src, 0)
    if err != nil {
        fmt.Println(err)
        return
    }
    f := fs.File(a.Pos())
    m := make(map[string]int)
    ast.Inspect(a, func(n ast.Node) bool {
        if ce, ok := n.(*ast.CallExpr); ok {
            start := f.Offset(ce.Pos())
            end := f.Offset(ce.Lparen)
            m[string(src[start:end])]++
        }
        return true
    })
    cs := make(calls, 0, len(m))
    for k, v := range m {
        cs = append(cs, &call{k, v})
    }
    sort.Sort(cs)
    for i, c := range cs {
        fmt.Printf("%-20s %4d\n", c.expr, c.count)
        if i == 9 {
            break
        }
    }
}

type call struct {
    expr  string
    count int
}
type calls []*call

func (c calls) Len() int           { return len(c) }
func (c calls) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }
func (c calls) Less(i, j int) bool { return c[i].count > c[j].count }
```

Output, when run on source code above:

```txt

len                     3
fmt.Println             3
f.Offset                2
make                    2
fmt.Printf              1
ioutil.ReadFile         1
a.Pos                   1
string                  1
token.NewFileSet        1
append                  1

```



## J

The lowest approach taken here makes no effort to classify the primitives as monads nor dyads, nor as verbs, adverbs, nor conjunctions.  Did "-" mean "additive inverse" or indicate subtraction?  Does ";" raze or link?  J is a multi-instruction single data language.  Parentheses around a group of verbs form hooks or forks which affect data flow.  The simple top10 verb does not find these important constructs. So we shall ignore them for this exercise.


```j

   IGNORE=: ;:'y(0)1',CR

   Filter=: (#~`)(`:6)

   NB. extract tokens from a large body newline terminated of text
   roughparse=: ;@(<@;: ::(''"_);._2)

   NB. count frequencies and get the top x
   top=: top=: {. \:~@:((#;{.)/.~)

   NB. read all installed script (.ijs) files and concatenate them
   JSOURCE=: ;fread each 1&e.@('.ijs'&E.)@>Filter {."1 dirtree jpath '~install'

   10 top (roughparse JSOURCE)-.IGNORE
┌─────┬──┐
│49591│, │
├─────┼──┤
│40473│=:│
├─────┼──┤
│35593│; │
├─────┼──┤
│34096│=.│
├─────┼──┤
│24757│+ │
├─────┼──┤
│18726│" │
├─────┼──┤
│18564│< │
├─────┼──┤
│18446│/ │
├─────┼──┤
│16984│> │
├─────┼──┤
│14655│@ │
└─────┴──┘

```



## Julia

```julia
using Printf, DataStructures

function funcfreqs(expr::Expr)
    cnt = counter(Symbol)
    expr.head == :call &&
        push!(cnt, expr.args[1])
    for e in expr.args
        e isa Expr && merge!(cnt, funcfreqs(e))
    end
    return cnt
end

function parseall(str::AbstractString)
    exs = Any[]
    pos = start(str)
    while !done(str, pos)
        ex, pos = parse(str, pos) # returns next starting point as well as expr
        ex.head == :toplevel ? append!(exs, ex.args) : push!(exs, ex)
    end
    if isempty(exs)
        throw(ParseError("end of input"))
    elseif length(exs) == 1
        return exs[1]
    else
        return Expr(:block, exs...)
    end
end

freqs = readstring("src/Function_frequency.jl") |> parseall |> funcfreqs

for (v, f) in freqs
    @printf("%10s → %i\n", v, f)
end
```


```txt
   append! → 1
       isa → 1
     start → 1
     push! → 2
        |> → 2
 funcfreqs → 2
  parseall → 1
ParseError → 1
         ! → 1
    length → 1
     throw → 1
     parse → 1
readstring → 1
        == → 3
   counter → 1
      Expr → 1
    merge! → 1
   isempty → 1
      done → 1
```



## LiveCode

Initially based on [http://lessons.livecode.com/m/2592/l/126343-listing-all-the-handlers-in-a-script Listing all the handlers in a script]

```LiveCode
function handlerNames pScript
    put pScript into pScriptCopy
    filter pScript with regex pattern "^(on|function).*"
    -- add in the built-in commands & functions
    put the commandNames & the functionnames into cmdfunc
    repeat for each line builtin in cmdfunc
        put 0 into handlers[builtin]
    end repeat
    
    -- add user defined handlers, remove this section of you do not want your own functions included
    repeat with x = 1 to the number of lines of pScript
        put word 2 of line x of pScript into handlername
        put 0 into handlers[handlername]
    end repeat
    
    -- count handlers used
    repeat with x = 1 to the number of lines of pScriptCopy
        repeat for each key k in handlers
            if k is among the tokens of line x of pScriptCopy then
                add 1 to handlers[k]
            end if
        end repeat 
    end repeat
    
    combine handlers using cr and space
    sort lines of handlers descending by word 2 of each
    put line 1 to 10 of handlers into handlers
    return handlers
end handlerNames
```


To use
```LiveCode
put handlerNames(the script of this stack & cr & the script of this card & cr & the script of me)
```


Sample output
```LiveCode
if 8
put 8
return 8
function 7
factorialacc 4  -- user def function for other rosetta task
factorialr 3  -- user def function for other rosetta task
handlerNames 3 
factorial 2  -- user def function for other rosetta task
factorialit 2  -- user def function for other rosetta task
mouseUp 2
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
programCount[fn_] :=  Reverse[If[Length[#] > 10, Take[#, -10], #] &[SortBy[Tally[Cases[DownValues[fn], s_Symbol, \[Infinity], Heads -> True]], Last]]]
```

{{out}}The output of applying this program to itself...
```txt
programCount[programCount]

```



## Perl

We leverage the PPI::Tokenizer module.

```perl
use PPI::Tokenizer;
my $Tokenizer = PPI::Tokenizer->new( '/path/to/your/script.pl' );
my %counts;
while (my $token = $Tokenizer->get_token) {
    # We consider all Perl identifiers. The following regex is close enough.
    if ($token =~ /\A[\$\@\%*[:alpha:]]/) {
        $counts{$token}++;
    }
}
my @desc_by_occurrence =
    sort {$counts{$b} <=> $counts{$a} || $a cmp $b}
         keys(%counts);
my @top_ten_by_occurrence = @desc_by_occurrence[0 .. 9];
foreach my $token (@top_ten_by_occurrence) {
    print $counts{$token}, "\t", $token, "\n";
}
```

When run on itself:

```txt
6       $token
6       my
4       $counts
2       $Tokenizer
2       $a
2       $b
2       %counts
2       @desc_by_occurrence
2       @top_ten_by_occurrence
2       PPI::Tokenizer
```



## Perl 6

Here we just examine the ast of the Perl 6 compiler (which is written in Perl 6) to look for function calls.

```perl6
my $text = qqx[perl6 --target=ast @*ARGS[]];
my %fun;
for $text.lines {
    %fun{$0}++ if / '(call &' (.*?) ')' /
}

for %fun.invert.sort.reverse[^10] { .value.say }
```

Here we run it on the strand sort RC entry.  Note how Perl 6 considers various operators to really be function calls underneath.

```txt
$ ./morefun strand
pop
postcircumfix:<[ ]>
unshift
succeed
splice
prefix:<->
push
infix:<,>
infix:<..>
infix:<->
```



## Phix

As Phix is self hosted, we can modify the compiler (or a copy of it) directly for this task.

Add the line shown to procedure Call() in pmain.e, after the else on line 4938 (at the time of writing)

```txt

    else -- rType=FUNC|TYPE
        log_function_call(rtnNo)

```

(I may have been a bit too literal about "function" here, specifically "not procedure")

Now create our test.exw program, which wraps the entire compiler:

```Phix
constant func_log = new_dict(),
         func_freq = new_dict()

global procedure log_function_call(integer rtnNo)
    integer node = getd_index(rtnNo,func_log)
    setd(rtnNo,iff(node=NULL?1:getd_by_index(node,func_log)+1),func_log)
end procedure

include p.exw   -- the phix compiler, full source

-- invert the dictionary, then print top ten

integer count = 0
function visitor(object key, integer data, integer user_data)
    if user_data=1 then -- invert
        setd({data,key},0,func_freq)
    else
        key[2] = symtab[key[2]][S_Name]
        ?key
        count += 1
        if count>10 then return 0 end if -- cease traversal
    end if
    return 1
end function
constant r_visitor = routine_id("visitor")

rebuild_callback() -- (convert ternary tree indexes to readable names)

traverse_dict(r_visitor,1,func_log)             -- invert
traverse_dict(r_visitor,2,func_freq,rev:=true)  -- top 10
```

Invoke using "p test -norun test" (note you can omit the ".exw" part of "test.exw")
```txt

{1253,"length"}
{655,"and_bits"}
{354,"append"}
{308,"find"}
{163,"or_bits"}
{158,"repeat"}
{154,"SetField"}
{136,"sprintf"}
{119,"equal"}
{105,"sequence"}
{90,"platform"}

```

Notes:

The log_function call is passed an index into the symbol table.

For performance reasons the compiler uses integer indexes, so we need to invoke
rebuild_callback() to replace them with human-readable names in the symbol table.

For more details of S_Name and other constants/contents of the symbol table, see pglobals.e

The compiler (p.exe) interprets test.exw(+p.exw) which compiles a third copy of itself under -norun.

Notice that it is not necessary to compile the compiler (using p -c p) to test changes in it, and in 
fact weeks or months of work on the compiler often happens purely in interpreter mode, between actually
creating a new executable.

If, instead, you want to know how many times a function is called at run-time, just add "with profile" 
to the source and it will create a ex.pro listing which tells you.

Lastly, remember to remove/comment out that log_function_call() in pmain.e


## PicoLisp


```PicoLisp
(let Freq NIL
   (for "L" (filter pair (extract getd (all)))
      (for "F"
         (filter atom
            (fish '((X) (or (circ? X) (getd X)))
               "L" ) )
         (accu 'Freq "F" 1) ) )
   (for X (head 10 (flip (by cdr sort Freq)))
      (tab (-7 4) (car X) (cdr X)) ) )
```

Output, for the system in debug mode plus the above code:

```txt
quote   310
car     236
cdr     181
setq    148
let     136
if      127
and     124
cons    110
cadr     80
or       76
```

If the condition in the 5th line (getd X) is replaced with (sym? X), then all symbols are counted, and the output is

```txt
X       566
quote   310
car     236
cdr     181
C       160
N       157
L       155
Lst     152
setq    148
T       144
```

And if it is replaced with (num? X), it is

```txt
1        71
0        38
2        27
3        17
7         9
-1        9
100       8
48        6
43        6
12        6
```



## Python

This code parses a Python source file using the built-in '''ast''' module and counts simple function calls; it won't process method calls or cases when you call the result of an expression. Also, since constructors are invoked by calling the class, constructor calls are counted as well.

```python
import ast

class CallCountingVisitor(ast.NodeVisitor):

    def __init__(self):
        self.calls = {}

    def visit_Call(self, node):
        if isinstance(node.func, ast.Name):
            fun_name = node.func.id
            call_count = self.calls.get(fun_name, 0)
            self.calls[fun_name] = call_count + 1
        self.generic_visit(node)

filename = input('Enter a filename to parse: ')
with open(filename, encoding='utf-8') as f:
    contents = f.read()
root = ast.parse(contents, filename=filename) #NOTE: this will throw a SyntaxError if the file isn't valid Python code
visitor = CallCountingVisitor()
visitor.visit(root)
top10 = sorted(visitor.calls.items(), key=lambda x: x[1], reverse=True)[:10]
for name, count in top10:
    print(name,'called',count,'times')

```


The result of running the program on the ftplib module of Python 3.2:

```txt
Enter a filename to parse: c:\Python32\Lib\ftplib.py
error_reply called 10 times
print called 10 times
error_proto called 8 times
callback called 8 times
int called 7 times
repr called 7 times
isinstance called 6 times
len called 6 times
ValueError called 3 times
parse257 called 2 times

```



## Racket


```racket

#lang racket
(require math)
(define in (open-input-file "function-frequency.rkt"))
(void (read-language in))
(define s-exprs (for/list ([s (in-port read in)]) s))
(define symbols (filter symbol? (flatten s-exprs)))
(define counts  (sort (hash->list (samples->hash symbols)) >= #:key cdr))
(take counts (min 10 (length counts)))

```

Output:

```racket

'((define . 4)
  (counts . 3)
  (s-exprs . 2)
  (s . 2)
  (symbols . 2)
  (a-program . 2)
  (filter . 1)
  (hash->list . 1)
  (in-port . 1)
  (sort . 1))

```




## REXX


### version 1

This program counts statically. It lacks, however, treatment of comments and literal strings.

```rexx
fid='pgm.rex'
cnt.=0
funl=''
Do While lines(fid)>0
  l=linein(fid)
  Do Until p=0
    p=pos('(',l)
    If p>0 Then Do
      do i=p-1 To 1 By -1 While is_tc(substr(l,i,1))
        End
      fn=substr(l,i+1,p-i-1)
      If fn<>'' Then
        Call store fn
      l=substr(l,p+1)
      End
    End
  End
Do While funl<>''
  Parse Var funl fn funl
  Say right(cnt.fn,3) fn
  End
Exit
x=a(3)+bbbbb(5,c(555))
special=date('S') 'DATE'() "date"()
is_tc:
abc='abcdefghijklmnopqrstuvwxyz'
Return pos(arg(1),abc||translate(abc)'1234567890_''"')>0

store:
Parse Arg fun
cnt.fun=cnt.fun+1
If cnt.fun=1 Then
  funl=funl fun
Return
```

```txt
  1 lines
  1 linein
  2 pos
  1 '
  1 is_tc
  3 substr
  1 right
  1 a
  1 bbbbb
  1 c
  1 date
  1 'DATE'
  1 "date"
  1 arg
  1 translate
```



### version 2

This program counts statically.
Contents of comments and literal strings are not analyzed.
Neither are function invocations via CALL.

```rexx
/* REXX ****************************************** Version 11.12.2015 **
* Rexx Tokenizer to find function invocations
*-----------------------------------------------------------------------
* Tokenization remembers the following for each token
* t.i       text of token
* t.i.0t    type of token: Cx/V/K/N/O/S/L
*             comment/variable/keyword/constant/operator/string/label
* t.i.0il   line of token in the input
* t.i.0ic   col of token in the input
* t.i.0prev index of token starting previous instruction
* t.i.0ol   line of token in the output
* t.i.0oc   col of token in the output
*---------------------------------------------------------------------*/
  Call time 'R'
  Parse Upper Arg fid '(' options
  If fid='?' Then Do
    Say 'Tokenike a REXX proram and list the function invocations found'
    Say '  which are of the form symbol(... or ''string''(...'
    Say '  (the left parenthesis must immediately follow the symbol'
    Say '  or literal string.)'
    Say 'Syntax:'
    Say '  TKZ pgm < ( <Debug> <Tokens> >'
    Exit
    End
  g.=0
  Call init                         /* Initialize constants etc.      */
  g.0cont='01'x
  g.0breakc='02'x
  cnt.=0
  Call readin                       /* Read input file into l.*       */
  Call tokenize                     /* Tokenize the input             */
  tk=''
  Call process_tokens
  g.0fun_list=wordsort(g.0fun_list)
  Do While g.0fun_list>''
    Parse Var g.0fun_list fun g.0fun_list
    Say right(cnt.fun,3) fun
    End
  Say time('E') 'seconds elapsed for' t.0 'tokens in' g.0lines 'lines.'
  Exit

init:
/***********************************************************************
* Initialize constants etc.
***********************************************************************/
  g.=''
  g.0debug=0                        /* set debug off by default       */

  fid=strip(fid)
  If fid='' Then                    /* no file specified              */
    Exit exit(12 'no input file specified')
  Parse Var fid fn '.'

  os=options                        /* options specified on command   */
  g.0debug=0                        /* turn off debug output          */
  g.0tokens=0                       /* No token file                  */
  Do While os<>''                   /* process them individually      */
    Parse Upper Var os o os         /* pick one                       */
    Select
      When abbrev('DEBUG',o,1) Then /* Debug specified                */
        g.0debug=1                  /* turn on debug output           */
      When abbrev('TOKENS',o,1) Then /* Write a file with tokens      */
        g.0tokens=1
      Otherwise                     /* anything else                  */
        Say 'Unknown option:' o     /* tell the user and ignore it    */
      End
    End

  If g.0debug Then Do
    g.0dbg=fn'.dbg'; '@erase' g.0dbg
    End
  If g.0tokens Then Do
    g.0tkf=fn'.tok'; '@erase' g.0tkf
    End

/***********************************************************************
* Language specifics
***********************************************************************/
  g.0special='+-*/%''";:<>^\=|,()& '/* special characters             */
                                        /* chars that may start a var */
  g.0a='abcdefghijklmnopqrstuvwxyz'||,
       'ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$!?_'
  g.0n='1234567890'                 /* numeric characters             */
  g.0vc=g.0a||g.0n||'.'             /* var-character                  */
                                    /* multi-character operators      */
  g.0opx='&& ** // << <<= <= <> == >< >= >> >>=',
         '^< ^<< ^= ^== ^> ^>> \< \<< \= \== \> \>> ||'

  t.=''                             /* token list                     */
  Return

readin:
/***********************************************************************
* Read the file to be formatted
***********************************************************************/
  lc=''
  i=0
  g.0lines=0
  Do While lines(fid)<>0
    li=linein(fid)
    g.0lines=g.0lines+1
    If i>0 Then
      lc=strip(l.i,'T')
    If right(lc,1)=',' Then Do
      l.i=left(lc,length(lc)-1) li
      End
    Else Do
      i=i+1
      l.i=li
      End
    End
  l.0=i
  Call lineout fid
  t=l.0+1
  l.t=g.0eof                        /* add a stopper at program end   */
  l.0=t                             /* adjust number of lines         */
  g.0il=t                           /* remember end of program        */
  Return

tokenize:
/***********************************************************************
* First perform tokenization
* Input:  l.*  Program text
* Output: t.*  Token list
*         t.0t.i token type CA CB CC C comment begin/middle/end
*                           S          string
*                           O          operator (special character)
*                           V          variable symbol
*                           N          constant
*                           X          end of text
* Note: special characters are treated as separate tokens
***********************************************************************/
  li=0                              /* line index                     */
  ti=0                              /* token index                    */
  Do While li<l.0                   /* as long as there is more input */
    li=li+1                         /* index of next line             */
    l=l.li                          /* next line to be processed      */
    g.0newline=1
    g.0cc=0                         /* current column                 */
    Call dsp l.li                   /* debug output                   */
    If l='' Then                    /* empty line                     */
      Call addtoken '/*--*/','C'    /* preserve with special token    */
    Do While l<>''                  /* work through the line          */
      nbc=verify(l,' ')             /* first non-blank column         */
      g.0cc=g.0cc+nbc               /* advance to this                */
      If g.0newline='' Then Do
        If t.ti.0ic='' Then
          t.ti.0ic=0
        If g.0cc=t.ti.0ic+length(t.ti) Then Do
          tj=ti+1
          t.tj.0ad=1
          End
        End
      l=substr(l,nbc)               /* and continue with rest of line */
      Parse Var l c +1 l 1 c2 +2    /* get character(s)               */
      g.0tb=g.0cc                   /* remember where token starts    */
      Select                        /* take a decision                */
        When c2='/*' Then           /* comment starts here            */
          Call comment              /* process comment                */
        When pos(c,'''"')>0 Then    /* literal string starts here     */
          Call string c             /* process literal string         */
        Otherwise                   /* neither comment nor literal    */
          Call token                /* get other token                */
        End                         /* cmt, string, or token done     */
      End                           /* end of loop over line          */
    End                             /* end of loop over program       */
  t.0=ti                            /* store number of tokens         */
  Call dsp ti 'tokens' l.0 'lines'
  Return
comment:
/***********************************************************************
* Parse a comment
* Nested comments are supported
***********************************************************************/
  cbeg=t.ti.0il
  l=substr(l,2)                     /* continue after slash-asterisk  */
  g.0cc=g.0cc+1                     /* update current char position   */
  t='/*'                            /* token so far                   */
  incmt=1                           /* indicate "within a comment"    */
  Do Until incmt=0                  /* loop until done                */
    bc=pos('/*',l)                  /* next begin comment, if any     */
    ec=pos('*/',l)                  /* next end   comment, if any     */
    Select                          /* decide                         */
      When bc>0 &,                  /* begin-comment found            */
           (ec=0 | bc<ec) Then Do   /* and no end-comment or later    */
        t=t||left(l,bc+1)           /* add this all to token          */
        incmt=incmt+1               /* increment comment nest-depth   */
        l=substr(l,bc+2)            /* continue after slash-asterisk  */
        g.0cc=g.0cc+bc+1            /* update current char position   */
        End
      When ec>0 Then Do             /* end-comment found              */
        t=t||left(l,ec+1)           /* add all to token               */
        incmt=incmt-1               /* decrement nesting              */
        l=substr(l,ec+2)            /* continue after asterisk-slash  */
        g.0cc=g.0cc+ec+1            /* update current char position   */
        End
      Otherwise Do                  /* no further comment bracket     */
        Call addtoken t||l,ct()     /* rest of line to token          */
        li=li+1                     /* proceed to next line           */
        l=l.li                      /* contents of next line          */
        g.0newline=1
        If l=g.0eof Then Do
          Say 'Comment started in line' cbeg 'is not closed before EOF'
          Exit err(58)
          End
        g.0cc=0                     /* current char (none)            */
        g.0tb=1                     /* token (comment) starts here    */
        End
      End
    End
  Call addtoken t,ct()             /* last (or only) comment token    */
  If pos('*debug*',t)>0 Then g.0debug=1
  Return

ct:
/***********************************************************************
* Comment type
***********************************************************************/
  If incmt>0 Then Do                /* within a comment               */
    If t.ti.0t='CA' |,              /* prev. token was start or cont  */
       t.ti.0t='CB' Then Return 'CB'  /* this is continuation         */
                    Else Return 'CA'  /* this is start                */
    End
  Else Do                           /* comment is over                */
    If t.ti.0t='CA' |,              /* prev. token was start or cont  */
       t.ti.0t='CB' Then Return 'CC'  /* this is final part           */
                    Else Return 'C'   /* this is just a comment       */
    End
string:
/***********************************************************************
* Parse a string
* take care of '111'B and '123'X
***********************************************************************/
  Parse Arg delim                  /* string delimiter found          */
  t=delim                          /* star building the token         */
  instr=1                          /* note we are within a string     */
  g.0ss=li
  Do Until instr=0                 /* continue until it is over       */
    se=pos(delim,l)                /* ending delimiter                */
    If se>0 Then Do                /* found                           */
      If substr(l,se+1,1)=delim Then Do /* but it is doubled          */
        t=t||left(l,se+1)          /* so add all so far to token      */
        l=substr(l,se+2)           /* and take rest of line           */
        g.0cc=g.0cc+se+1           /* and set current character pos   */
        End
      Else Do                      /* not another one                 */
        instr=0                    /* string is done                  */
        t=t||left(l,se)            /* add the string data to token    */
        l=substr(l,se+1)           /* take the rest of the line       */
        g.0cc=g.0cc+se             /* and set current character pos   */
        If pos(translate(left(l,1)),'BX')>0 Then
          If pos(substr(l,2,1),g.0vc)=0 Then Do
            t=t||left(l,1)         /* add the char to the token       */
            l=substr(l,2)          /* take the rest of the line       */
            g.0cc=g.0cc+1          /* and set current character pos   */
            End
        End
      End
    Else Do                        /* not found                       */
      Call addtoken t||l,'S'       /* store the token                 */
      g.0lasttoken=''              /* reset this switch               */
      li=li+1                      /* go on to the next line          */
      If li>l.0 Then               /* there is no next line           */
        Exit err(60,'string starting in line' g.0ss,
                                      'does not end before end of file')
      Else
        Say 'string starting at line' g.0ss 'extended over line boundary'
      l=l.li                       /* take contents of the next line  */
      g.0cc=1                      /* current char position           */
      g.0tb=1                      /* ??                              */
      End
    End
  Call addtoken t,'S'              /* store the token                 */
  Return
token:
/***********************************************************************
* Parse a token
***********************************************************************/
  IF c=g.0comma & l='' Then Do
    t=g.0cont
    type='O'                        /* O (for operator - not quite...)*/
    End
  Else Do
    If pos(c,g.0special)>0 Then Do  /* a special character            */
      t=c                           /* take it as is                  */
      type='O'                      /* O (for operator - not quite...)*/
      End
    Else Do                         /* some other character           */
      nsp=verify(l,g.0special,'M')  /* find delimiting character      */
      If nsp>0 Then Do              /* some character found           */
        t=c||left(l,nsp-1)          /* take all up to this character  */
        l=substr(l,nsp)             /* and continue from there        */
        End
      Else Do                       /* none found                     */
        t=c||l                      /* add rest of line to token      */
        l=''                        /* and all is used up             */
        End
      g.0cc=g.0cc+length(t)-1       /* adjust current char position   */
      If pos(right(t,1),'eE')>0 &,  /* consider nxxxE+nn case         */
         pos(left(l,1),'+-')>0 Then Do
        If pos(left(t,1),'.1234567890')>0 Then /* start . or digit    */
          If pos(substr(l,2,1),'1234567890')>0 Then Do /* dig after+- */
            nsp=verify(substr(l,2),g.0special,'M')+1 /* find end      */
            If nsp>1 Then           /* delimiting character found     */
              exp=substr(l,2,nsp-2)   /* exponent (if numeric)        */
            Else
              exp=substr(l,2)
          If verify(exp,'0123456789')=0 Then Do
            t=t||left(l,1)||exp
            l=substr(l,length(exp)+2)
            g.0cc=g.0cc+length(exp)+2
            End
          End
        End
      Select
        When isvar(t) Then          /* token qualifies as variable    */
          type='V'
        When isconst(t) Then        /* token is a constant symbol     */
          type='N'
        When t=g.0eof   Then        /* token is end of file indication*/
          type='X'
        Otherwise Do                /* anything else is an error      */
          Say 'li='li
          Say l
          Say 'token error'
          Trace ?R
          Exit err(62,'token' t 'is neither variable nor constant')
          End
        End
      If left(l,1)='(' Then
        type=type||'F'
      End
    End
  Call addtoken t,type              /* store the token                */
  Return
addtoken:
/***********************************************************************
* Add a token to the token list
***********************************************************************/
  Parse Arg t,type                  /* token and its type             */
  If type='O' Then Do               /* operator (special character)   */
    If pos(t,'><=&|/*')>0 Then Do   /* char for composite operator    */
      If wordpos(t.ti||t,g.0opx)>0 Then Do  /*  composite operator    */
        t.ti=t.ti||t                /* use concatenation              */
                                    /* does not handle =/**/=         */
        t=''                        /* we are done                    */
        Return
        End
      End
    End

  If type='CC' & t='*/' Then Do     /* The special case for SPA       */
    Return
    End

  ti=ti+1                           /* increment index                */
  t.ti=t                            /* store token's value            */
  t.ti.0t=left(type,1)              /*  and its type                  */
  t.ti.0nl=g.0newline               /* token starts a new line        */
  g.0newline=''                     /* reset new line switch          */
  If t.ti.0t='C' Then Do
    t.ti.0t=type
    If left(t.ti,3)='/* ' &,
       right(t.ti,3)=' */' Then
      t.ti='/*' strip(substr(t.ti,4,length(t.ti)-6)) '*/'
    End
  t.ti.0f=substr(type,2,1)          /* 'F' if possibly a function     */
  Call setpos ti li g.0tb           /*    and its position            */
  If left(type,1)='C' Then          /* ??? */
    If left(t.ti,2)<>'/*' Then Do
      ts=strip(t.ti,'L')
      t.ti.0oc=t.ti.0oc+length(t.ti)-length(ts)
      t.ti=ts
      End
  If t.ti.0ol='' Then t.ti.0ol=li
  If t.ti.0oc='' Then t.ti.0oc=0
  t.ti.0il=t.ti.0ol                 /*    and its position            */
  t.ti.0ic=t.ti.0oc                 /*    and its position            */
  Call dsp ti t.ti t.ti.0il'/'t.ti.0ic '->' t.ti.0ol'/'t.ti.0oc
  t=''                              /* reset token variable           */
  Return

lookback:
/***********************************************************************
* Look back if...
***********************************************************************/
  Do i_=ti To 1 By -1
    Select
      When left(t.i_.0t,1)='C' Then Nop
      When t.i_.0used<>1 &,
           (t.i_=g.0comma |,
            t.i_=g.0cont)  Then Do
        t.i_.0used=1
        t.i_=g.0cont
        Return '0'
        End
      Otherwise
        Return '1'
      End
    End
  Return '1'

isvar:
/***********************************************************************
* Determine if a string qualifies as variable name
***********************************************************************/
  Parse Arg a_ +1 b_
  res=(pos(a_,g.0a)>0) &,
    (verify(b_,g.0a||g.0n||'.')=0)
  Return res

isconst:
/***********************************************************************
* Determine if a string qualifies as constant
***********************************************************************/
  Parse Arg a_
  res=(verify(a_,g.0a||g.0n||'.+-')=0) /* ??? */
  Return res

setpos:
  Parse Arg seti sol soc
  setz='setpos:' t.seti t.seti.0ol'/'t.seti.0oc '-->',
                                                    sol'/'soc '('sigl')'
  Call dsp setz
  t.seti.0ol=sol
  t.seti.0oc=soc
  Return

process_tokens:
/***********************************************************************
* Process the token list
***********************************************************************/
  Do i=1 To t.0
    If g.0tokens Then
      Call lineout g.0tkf,right(i,4) right(t.i.0il,3)'.'left(t.i.0ic,3),
                                     right(t.i.0ol,3)'.'left(t.i.0oc,3),
                                     left(t.i.0t,2) left(t.i,25)
    If t.i='(' Then Do
      j=i-1
      If t.j.0ol=t.i.0il & ,
         t.j.0oc+length(t.j)=t.i.0ic &,
         pos(t.j.0t,'VS')>0 Then
        Call store_f t.j
      End
    End
  If g.0tokens Then
    Call lineout g.0tkf
  Return

store_f:
  Parse Arg funct
  If wordpos(funct,g.0fun_list)=0 then
    g.0fun_list=g.0fun_list funct
  cnt.funct=cnt.funct+1
  Return

dsp:
/***********************************************************************
* Record (and display) a debug line
***********************************************************************/
  Parse Arg ol_.1
  If g.0debug>0 Then
    Call lineout g.0dbg,ol_.1
  If g.0debug>1 Then
    Say ol_.1
  Return

wordsort: Procedure
/**********************************************************************
* Sort the list of words supplied as argument. Return the sorted list
**********************************************************************/
  Parse Arg wl
  wa.=''
  wa.0=0
  Do While wl<>''
    Parse Var wl w wl
    Do i=1 To wa.0
      If wa.i>w Then Leave
      End
    If i<=wa.0 Then Do
      Do j=wa.0 To i By -1
        ii=j+1
        wa.ii=wa.j
        End
      End
    wa.i=w
    wa.0=wa.0+1
    End
  swl=''
  Do i=1 To wa.0
    swl=swl wa.i
    End
  Return strip(swl)

err:
/***********************************************************************
* Diagnostic error exit
***********************************************************************/
  Parse Arg errnum, errtxt
  Say 'err:' errnum  errtxt
  If t.ti.0il>g.0il Then
    Say 'Error' arg(1) 'at end of file'
  Else Do
    Say 'Error' arg(1) 'around line' t.ti.0il', column' t.ti.0ic
    _=t.ti.0il
    Say l._
    Say copies(' ',t.ti.0ic-1)'|'
    End
  If errtxt<>'' Then Say '  'errtxt
  Exit 12
```

Result for the above program.

```txt
  2 abbrev
  2 arg
  1 copies
  2 ct
  3 err
  1 exit
  1 isconst
  1 isvar
 21 left
  9 length
  1 linein
  1 lines
 15 pos
  7 right
  5 strip
 17 substr
  1 time
  1 translate
  6 verify
  2 wordpos
  1 wordsort
0.093000 seconds elapsed for 2200 tokens in 510 lines.
```



## Sidef

Sidef provides full access to its parser, allowing us to inspect all the declarations within a program.

```ruby
func foo { }
func bar { }

foo(); foo(); foo()
bar(); bar();

var data = Perl.to_sidef(Parser{:vars}{:main}).flatten

data.sort_by { |v| -v{:count} }.first(10).each { |entry|
    if (entry{:type} == :func) {
        say ("Function `#{entry{:name}}` (declared at line",
             " #{entry{:line}}) is used #{entry{:count}} times")
    }
}
```


```txt

Function `foo` (declared at line 1) is used 3 times
Function `bar` (declared at line 2) is used 2 times

```



## Tcl


```tcl
package require Tcl 8.6

proc examine {filename} {
    global cmds
    set RE "(?:^|\[\[\{\])\[\\w:.\]+"
    set f [open $filename]
    while {[gets $f line] >= 0} {
	set line [string trim $line]
	if {$line eq "" || [string match "#*" $line]} {
	    continue
	}
	foreach cmd [regexp -all -inline $RE $line] {
	    incr cmds([string trim $cmd "\{\["])
	}
    }
    close $f
}

# Parse each file on the command line
foreach filename $argv {
    examine $filename
}
# Get the command list in order of frequency
set cmdinfo [lsort -stride 2 -index 1 -integer -decreasing [array get cmds]]
# Print the top 10 (two list items per entry, so 0-19, not 0-9)
foreach {cmd count} [lrange $cmdinfo 0 19] {
    puts [format "%-20s%d" $cmd $count]
}
```

Sample run (note that the commands found are all standard Tcl commands; they're ''just'' commands so it is natural to expect them to be found):

```txt

bash$ tclsh8.6 RosettaCode/cmdfreq.tcl RosettaCode/*.tcl 
set                 2374
expr                846
if                  775
puts                558
return              553
proc                549
incr                485
foreach             432
lindex              406
lappend             351

```
<!-- note that it's certainly possible that not all commands are found; break and continue are likely underrepresented -->
