+++
title = "C"
description = ""
date = 2018-04-09T03:26:36Z
aliases = []
[extra]
id = 1702
[taxonomies]
categories = []
tags = []
+++


'''C''' is a general-purpose, [procedural](https://rosettacode.org/wiki/procedural_programming), [imperative](https://rosettacode.org/wiki/imperative_programming) computer programming language developed in 1972 by Dennis Ritchie at the [Bell Telephone Laboratories](https://rosettacode.org/wiki/Bell_Labs) for use with the [UNIX](https://rosettacode.org/wiki/UNIX) operating system. C evolved from its predecessor, [derived from::B](https://rosettacode.org/wiki/derived_from::B).

C has since spread to many other [platforms](https://rosettacode.org/wiki/:Category:Platforms), and is now one of the most widely used programming languages. C has also greatly influenced many other popular languages, such as [C++](https://rosettacode.org/wiki/C++) and [Objective-C](https://rosettacode.org/wiki/Objective-C), which were originally designed as enhancements to C. People are so familiar with its syntax that many other languages such as [AWK](https://rosettacode.org/wiki/AWK), [PHP](https://rosettacode.org/wiki/PHP), [Java](https://rosettacode.org/wiki/Java), [JavaScript](https://rosettacode.org/wiki/JavaScript), [D](https://rosettacode.org/wiki/D), and [C#](https://rosettacode.org/wiki/C_Sharp) deliberately used its "look and feel". C is the most commonly used programming language for writing system software, though it is also widely used for writing applications. [C](https://rosettacode.org/wiki/C) is the ''lingua franca'' of the [open source](https://rosettacode.org/wiki/open_source) community.


## Versions

* '''K&R C''' was the first widely-used form of C. It was originally documented in ''The C Programming Language'', published in 1978. It is named for the authors, Brian Kernighan and Dennis Ritchie (also the language's creator). Code in this style is virtually nonexistent today.
* '''C89''' (often called '''[ANSI](https://rosettacode.org/wiki/ANSI) C''') is the version of C standardized by ANSI in 1989. It is the most commonly used and supported version of the language.
* '''C90''' (often called '''[ISO](https://rosettacode.org/wiki/ISO) C''') is identical to C89, republished by ISO in 1990.
* '''C99''' is a significant improvement, adopting many features of [C++](https://rosettacode.org/wiki/C++) and standardizing common compiler extensions. It was standardized by ISO in 1999, and by ANSI in 2000. It is primarily supported by commercial C compilers, but most of its features are available in [Clang](https://rosettacode.org/wiki/Clang) [GCC](https://rosettacode.org/wiki/GCC). [http://gcc.gnu.org/c99status.html]
* '''C11''' is the current standard, published in December 2011. It is the default for [GCC](https://rosettacode.org/wiki/GCC) as of version 5.1.


## Citation

* [Wikipedia:C (programming language)](https://en.wikipedia.org/wiki/C_%28programming_language%29)


## Todo

[Reports:Tasks_not_implemented_in_C](https://rosettacode.org/wiki/Reports:Tasks_not_implemented_in_C)
## Merged content




This Brainfuck interpreter has an eight bit cell size with the tape length bounded only by available memory. The normal [http://esolangs.org/wiki/Brainfuck_bitwidth_conversions cell expansion techniques] work for Brainfuck programs that require a larger cell size (or you can just change the tape cell type).

It is a fairly slow interpreter which is probably able to run [http://esoteric.sange.fi/brainfuck/utils/mandelbrot/mandelbrot.b mandelbrot.b] in about a minute on your machine.


```c
/* This is the Neutron brainfuck interpreter.
 * It's rather small and dense, but still readable, more or less.
 *
 * Robert de Bath -- 2013.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct bfi { char cmd; struct bfi *next, *jmp; };
struct mem { char val; struct mem *next, *prev; };

int main(int argc, char **argv)
{
    FILE * ifd;
    int ch;
    struct bfi *p=0, *n=0, *j=0, *pgm = 0;
    struct mem *m = calloc(1,sizeof*m);
    setbuf(stdout, NULL);

    /* Open the file from the command line or stdin */
    if (argc < 2 || strcmp(argv[1], "-") == 0) ifd = stdin;
    else if ((ifd = fopen(argv[1], "r")) == 0) { perror(argv[1]); exit(1); }

    /*
     *  For each character, if it's a valid BF command add it onto the
     *  end of the program. If the input is stdin use the '!' character
     *  to mark the end of the program and the start of the data, but
     *  only if we have a complete program.  The 'j' variable points
     *  at the list of currently open '[' commands, one is matched off
     *  by each ']'.  A ']' without a matching '[' is not a legal BF
     *  command and so is ignored.  If there are any '[' commands left
     *  over at the end they are not valid BF commands and so are ignored.
     */
    while((ch = getc(ifd)) != EOF && (ifd!=stdin || ch != '!' || j || !pgm)) {
        if (ch == '<' || ch == '>' || ch == '+' || ch == '-' ||
            ch == ',' || ch == '.' || ch == '[' || (ch == ']' && j)) {
            if ((n = calloc(1, sizeof*n)) == 0) {
                perror(argv[0]); exit(1);
            }
            if (p) p->next = n; else pgm = n;
            n->cmd = ch; p = n;
            if (n->cmd == '[') { n->jmp=j; j = n; }
            else if (n->cmd == ']') {
                n->jmp = j; j = j->jmp; n->jmp->jmp = n;
            }
        }
    }

    /* Ignore any left over '[' commands */
    while(j) { p = j; j = j->jmp; p->jmp = 0; p->cmd = ' '; }

    if (ifd!=stdin) fclose(ifd);

    /* Execute the loaded BF program */
    for(n=pgm; n; n=n->next)
        switch(n->cmd)
        {
            case '+': m->val++; break;
            case '-': m->val--; break;
            case '.': putchar(m->val); break;
            case ',': if((ch=getchar())!=EOF) m->val=ch; break;
            case '[': if (m->val == 0) n=n->jmp; break;
            case ']': if (m->val != 0) n=n->jmp; break;
            case '<':
                if (!(m=m->prev)) {
                    fprintf(stderr, "%s: Hit start of tape\n", argv[0]);
                    exit(1);
                }
                break;
            case '>':
                if (m->next == 0) {
                    if ((m->next = calloc(1,sizeof*m)) == 0) {
                        perror(argv[0]); exit(1);
                    }
                    m->next->prev = m;
                }
                m=m->next;
                break;
        }

    return 0;
}
```


This Brainfuck interpreter is released into the Public domain or if your prefer the Creative commons CC0 license.
