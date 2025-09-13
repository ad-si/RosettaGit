+++
title = "X86 Assembly"
description = ""
date = 2017-04-03T22:57:10Z
aliases = []
[extra]
id = 6190
[taxonomies]
categories = []
tags = []
+++



[Category:Assembly](https://rosettacode.org/wiki/Category:Assembly)

Quote from Wikipedia article:

x86 assembly language is a family of backward-compatible assembly languages, which provide some level of compatibility all the way back to the Intel 8008 introduced in April 1972. x86 assembly languages are used to produce object code for the x86 class of processors. Like all assembly languages, it uses short mnemonics to represent the fundamental instructions that the CPU in a computer can understand and follow. Compilers sometimes produce assembly code as an intermediate step when translating a high level program into machine code. Regarded as a programming language, assembly coding is machine-specific and low level. Assembly languages are more typically used for detailed and time critical applications such as small real-time embedded systems or operating system kernels and device drivers.
## See also
Wikipedia Article: https://en.wikipedia.org/wiki/X86_assembly_language

Intel manuals: https://software.intel.com/en-us/articles/intel-sdm

Brief introduction: http://cs.lmu.edu/~ray/notes/x86assembly/

GNU assembler: https://sourceware.org/binutils/docs/as/

SSE: https://en.wikibooks.org/wiki/X86_Assembly/SSE

64 bit Linux assembly: http://rayseyfarth.com/asm/

Linux system calls: http://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/

YASM manual: http://www.tortall.net/projects/yasm/manual/html/index.html

NASM manual: http://www.nasm.us/xdoc/2.12.02/html/nasmdoc0.html

8086 assembly: http://www.stevemorse.org/8086/

32 bit: http://pacman128.github.io/pcasm/

64 bit: https://www.cs.cmu.edu/~fp/courses/15213-s07/misc/asm64-handout.pdf

Linux procedure calls: https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-r252.pdf


## Merged content


## x86_64 Assembly
GAS Syntax for Linux. Called it "Brainkrieg" after Teen Girl Squad. Operating principle is a little like a multi-cycle processor. Uses a jump table. Fixed data memory block.


### Implementation:


<lang>// Assembly Brainfuck interpreter
// Alternative name: "ASSembly Brainfuck interpretER"
// Compiles with `gcc -nostdlib brainkrieg.sx -o brainkrieg`
// Usage: ./brainkrieg "filename"
// Return 0 if normal exit
#define SYS_READ    $0
#define SYS_WRITE   $1
#define SYS_OPEN    $2
#define SYS_CLOSE   $3
#define SYS_FSTAT   $5
#define SYS_MMAP    $9
#define SYS_MUNMAP  $11
#define SYS_EXIT    $60

// From experiments:
#define FSIZEOFF    48
#define STATSIZE    144

// From Linux source:
#define RDONLY      $00
#define PROT_READ   $0x1
#define MAP_PRIVATE $0x02
#define STDIN       $0
#define STDOUT      $1
#define STDERR      $2

#define DEBUGMODE   0

.global _start
.text

.macro ERRCHECK code
    cmpq    $\code, %rax
    je      fs_error
.endm

/* Local stack notes:
    0: int fd
*/
#define STACKSIZE $4
_start:
    /* Entry Point: */
    // Open:
    movq    RDONLY, %rsi
    // Filename ptr is on stack currently as argv[1]:
    cmpq    $1, (%rsp)          // if argc is 1, error
    jnz     open_file
    jmp     fs_error

    open_file:
    movq    16(%rsp), %rdi      // argc(8), argv0(8) => rsp+16. filename
    movq    SYS_OPEN, %rax
    syscall
    ERRCHECK    -1
    subq    STACKSIZE, %rsp           // local stack
    movl    %eax, (%rsp)        // int fd = open(argv[1], RDONLY)

    // fstat to get filesize
    fstat:
    movq    $statstruct, %rsi
    movl    (%rsp), %edi        // fd
    movq    SYS_FSTAT, %rax
    syscall                     // fstat(fd, statstruct)
    ERRCHECK    -1

    // mmap - don't forget to munmap.
    mmap:
    movq    $0, %r9             // offset
    movq    (%rsp), %r8         // fd
    movq    MAP_PRIVATE, %r10
    movq    PROT_READ, %rdx
    movq    filesize, %rsi
    movq    (%rsp), %rdi        // vmemptr
    movq    SYS_MMAP, %rax
    syscall
    ERRCHECK    -1

    // Set up machine:
    boot:
    movq    %rax, head          // head = mmap'd file start
    movq    %rax, inst_ptr      // inst_ptr = head
    addq    filesize, %rax
    movq    %rax, end           // end = head+filesize
    movq    $tape, data_ptr     // data_ptr = tape
    xorq    %rax, %rax
    /* Magic happens here:
        - Fetch symbol
        - Decode symbol
        - Execute opcode
        - Instruction out of range = halt
        - Data out of range = halt
     */
    OS_start:
    fetch:
        movq    inst_ptr, %rbx
        cmpq    end, %rbx
        ja      shutdown            // End of code
        movzbq  (%rbx), %rax
        incq    %rbx
        movq    %rbx, inst_ptr
    decode:
        #if DEBUGMODE
            movb    %al, debugChar
            movq    $1, %rdx
            movq    $debugChar, %rsi
            movq    STDERR, %rdi
            movq    SYS_WRITE, %rax
            syscall
            ERRCHECK    -1
            movzbq  debugChar, %rax
        #endif
        mov     $branch_table, %rcx
        jmp     *(%rcx,%rax,8)
    // execute:
    dp_left:
        cmpq    $tape, data_ptr
        jz      scram
        decq    data_ptr
        jmp     fetch
    dp_right:
        cmpq    $endtape, data_ptr
        jz      scram
        incq    data_ptr
        jmp     fetch
    dec_data:
        movq    data_ptr, %rbx
        decb    (%rbx)
        jmp     fetch
    inc_data:
        movq    data_ptr, %rbx
        incb    (%rbx)
        jmp     fetch
    out_data:
        movq    $1, %rdx
        movq    data_ptr, %rsi
        movq    STDOUT, %rdi
        movq    SYS_WRITE, %rax
        syscall
        ERRCHECK    -1
        jmp     fetch
    in_data:
        movq    $1, %rdx
        movq    data_ptr, %rsi
        movq    STDIN, %rdi
        movq    SYS_READ, %rax
        syscall
        ERRCHECK    -1
        cmpb    $'\n, (%rsi)
        je      fetch
        movq    %rax, junkChar
        jmp     fetch
    brf: // branch if *dp=0
        movq    data_ptr, %rbx
        cmpb    $0, (%rbx)
        jnz     fetch
        matchfwd:
            movq    inst_ptr, %rbx
            movb    (%rbx), %al
            cmpb    $'[, %al
            jne     no_smph
            incq    brack_smph
            cmpq    end, %rbx
            je      scram
            incq    inst_ptr
            jmp     matchfwd
            no_smph:    // no semaphore needed
            cmpb    $'], %al
            je      check_smph
            cmpq    end, %rbx
            je      scram
            incq    inst_ptr
            jmp     matchfwd
            check_smph:     // check if semaphore set
            cmpq    $0, brack_smph
            jz      donematch
            decq    brack_smph
            cmpq    end, %rbx
            je      scram
            incq    inst_ptr
            jmp     matchfwd
    brb: // branch if *dp!=0
        movq    data_ptr, %rbx
        cmpb    $0, (%rbx)
        jz      fetch
        subq    $2, inst_ptr        // Branch taken, scan back.
        matchbwd:
            movq    inst_ptr, %rbx
            movb    (%rbx), %al
            cmpb    $'], %al
            jne     no_smph2
            incq    brack_smph
            cmpq    head, %rbx
            je      scram
            decq    inst_ptr
            jmp     matchbwd
            no_smph2:   // no semaphore needed
            cmpb    $'[, %al
            je      check_smph2
            cmpq    head, %rbx
            je      scram
            decq    inst_ptr
            jmp     matchbwd
            check_smph2:    // check if semaphore set
            cmpq    $0, brack_smph
            jz      donematch2
            decq    brack_smph
            cmpq    head, %rbx
            je      scram
            decq    inst_ptr
            jmp     matchbwd
        donematch:
        incq    inst_ptr
        donematch2:
        jmp     fetch

    scram:      // Memory breached
    movq    $0x7f, err_val

    shutdown:
    // Consume rest of stdin if we used it.
    cmpb    $0, junkChar
    jz      skip_flush
    flush_stdin:
    movq    SYS_READ, %rax
    movq    STDIN, %rdi
    movq    $junkChar, %rsi
    movq    $1, %rdx
    syscall
    cmpq    $0, %rax            // EOF
    jz      skip_flush
    cmpb    $'\n, junkChar
    jne     flush_stdin

    // munmap
    skip_flush:
    movq    filesize, %rsi
    movq    head, %rdi
    movq    SYS_MUNMAP, %rax
    syscall                     // munmap(vmemptr, filesize)
    cmpq    $-1, %rax
    je      fs_error
    // close
    movl    (%rsp), %edi
    movq    SYS_CLOSE, %rax
    syscall                     // close(fd)
    ERRCHECK    -1

exit:
    movq    SYS_EXIT, %rax
    movzbq  err_val, %rdi
    syscall

fs_error:
    movq    SYS_EXIT, %rax
    movq    $-1, %rdi
    syscall                         // exit(-1)

.data
branch_table:
    .rept   43
    .quad   fetch
    .endr

    .quad   inc_data
    .quad   in_data
    .quad   dec_data
    .quad   out_data

    .rept   13
    .quad   fetch
    .endr

    .quad   dp_left
    .quad   fetch
    .quad   dp_right

    .rept   28
    .quad   fetch
    .endr

    .quad   brf
    .quad   fetch
    .quad   brb

    .rept   162
    .quad   fetch
    .endr

.bss
brack_smph:    // Bracket matching semaphore
    .quad   0

junkChar:       // Also used to check if input was used.
    .byte   0

#if DEBUGMODE
debugChar:
    .byte   0
#endif

err_val:
    .byte   0

// fstat:
statstruct:     // This struct is 144 bytes. Only want size (+48)
    .zero FSIZEOFF
    filesize:  // 8 bytes.
    .quad   0
    .zero   STATSIZE-FSIZEOFF+8

// Program:
head:
    .quad   0
end:
    .quad   0
inst_ptr:
    .quad   0

// Data:
data_ptr:
    .quad   0
tape:
    .zero   (1<<20)
endtape:
    .zero   1

```

