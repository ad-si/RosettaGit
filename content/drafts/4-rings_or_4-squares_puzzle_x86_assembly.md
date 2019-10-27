+++
title = "4-rings or 4-squares puzzle/X86 Assembly"
description = ""
date = 2017-04-04T23:24:09Z
aliases = []
[extra]
id = 21343
[taxonomies]
categories = []
tags = []
+++


## X86 Assembly

{{Works with|NASM}}
{{Works with|Linux}}
64 bit

```asm

; Based on C version http://rosettacode.org/wiki/4-rings_or_4-squares_puzzle#C

%define TRUE 1
%define FALSE 0

global main,foursquares,acd,ge,bf,print_output
extern printf

segment .data

a dq 0
b dq 0
c dq 0
d dq 0
e dq 0
f dq 0
g dq 0

lo dq 0
hi dq 0
unique dq 0
show dq 0
solutions dq 0

output_fmt db `%ld %ld %ld %ld %ld %ld %ld\n`,0

segment .text

main:                            
    push rbp                     
    mov rbp,rsp                 

    mov rdi,1
    mov rsi,7
    mov rdx,TRUE
    mov rcx,TRUE
    call foursquares

    mov rdi,3
    mov rsi,9
    mov rdx,TRUE
    mov rcx,TRUE
    call foursquares

    mov rdi,0
    mov rsi,9
    mov rdx,FALSE
    mov rcx,FALSE
    call foursquares

    xor rax,rax                  
    leave                       
    ret   
    
segment .data

newlinefmt db `\n`,0
uniquefmt db `\n%ld unique solutions in %ld to %ld\n`,0
nonuniquefmt db `\n%ld non-unique solutions in %ld to %ld\n`,0

segment .text

foursquares:                             
    push rbp                     
    mov rbp,rsp
    
    mov qword [lo],rdi
    mov qword [hi],rsi
    mov qword [unique],rdx
    mov qword [show],rcx
    mov qword [solutions],0
    
    lea rdi,[newlinefmt]
    xor rax,rax
    call printf
    
    call acd
    
    mov rax,qword [unique]
    mov rbx,TRUE
    cmp rax,rbx
    je .isunique
    
    lea rdi,[nonuniquefmt]
    mov rsi,qword [solutions]
    mov rdx,qword [lo]
    mov rcx,qword [hi]
    xor rax,rax
    call printf
    jmp .done
    
.isunique:
    lea rdi,[uniquefmt]
    mov rsi,qword [solutions]
    mov rdx,qword [lo]
    mov rcx,qword [hi]
    xor rax,rax
    call printf
    
.done:
    xor rax,rax                  
    leave                        
    ret  
    
segment .text

acd:                             
    push rbp                     
    mov rbp,rsp
    
    mov rax,qword [lo] ; c = lo
    mov qword [c],rax 

.nextouterfor:
    mov rax,qword [c]  ; c <= hi
    mov rcx,qword [hi] 
    cmp rax,rcx
    jg .doneouterfor
    
    mov rax,qword [lo] ; d = lo
    mov qword [d],rax 
    
.nextinnerfor:
    mov rax,qword [d]  ; d <= hi
    mov rcx,qword [hi] 
    cmp rax,rcx
    jg .doneinnerfor
    
    mov rax,qword [unique]
    mov rcx,FALSE
    cmp rax,rcx
    je .inif
    
    mov rax,qword [c]
    mov rcx,qword [d]
    cmp rax,rcx
    jne .inif
    jmp .iffails
    
.inif: 
    mov rax,qword [c]
    mov rcx,qword [d]
    add rax,rcx
    mov qword [a],rax
    
; ((a >= lo) && 
;  (a <= hi) &&
;  ((!unique) || 
;   ((c != 0) && 
;    (d != 0)
;   )
;  )
; )
    mov rax,qword [a]  ;(a >= lo)
    mov rcx,qword [lo]
    cmp rax,rcx
    jl .iffails
    
    mov rax,qword [a]  ;(a <= hi)
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .iffails
    
    mov rax,qword [unique] ;(!unique)
    mov rcx,FALSE
    cmp rax,rcx
    je .ifsucceeds

    mov rax,qword [c]  ;(c != 0)
    mov rcx,0
    cmp rax,rcx
    je .iffails
    
    mov rax,qword [d]  ;(d != 0)
    mov rcx,0
    cmp rax,rcx
    je .iffails

.ifsucceeds:

    call ge
    
.iffails:
    mov rax,qword [d] ; d++
    inc rax
    mov qword [d],rax
    jmp .nextinnerfor

.doneinnerfor:
    mov rax,qword [c] ; c++
    inc rax
    mov qword [c],rax
    jmp .nextouterfor
   
.doneouterfor:
    xor rax,rax                  
    leave                        
    ret    
    
ge:                              
    push rbp                     
    mov rbp,rsp
    
    mov rax,qword [lo] ; e = lo
    mov qword [e],rax 

.nextfor:
    mov rax,qword [e]  ; e <= hi
    mov rcx,qword [hi] 
    cmp rax,rcx
    jg .donefor
    
    mov rax,qword [unique]
    mov rcx,FALSE
    cmp rax,rcx
    je .inif
 
; ((e != a) && (e != c) && (e != d))

    mov rax,qword [e]
    
    mov rcx,qword [a] ; (e != a)
    cmp rax,rcx
    je .skipif
    
    mov rcx,qword [c] ; (e != c)
    cmp rax,rcx
    je .skipif
    
    mov rcx,qword [d] ; (e != d)
    cmp rax,rcx
    je .skipif
    
.inif: 
    mov rax,qword [d] ; g = d + e
    mov rcx,qword [e]
    add rax,rcx
    mov qword [g],rax

; ((g >= lo) &&
;  (g <= hi) &&
;  ((!unique) || 
;   ((g != a) && 
;    (g != c) &&
;    (g != d) && 
;    (g != e)
;   )
;  )
; )    

    mov rax,qword [g]  ;(g >= lo)
    mov rcx,qword [lo]
    cmp rax,rcx
    jl .skipif
    
    mov rax,qword [g]  ;(g <= hi)
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .skipif
    
    mov rax,qword [unique] ;(!unique)
    mov rcx,FALSE
    cmp rax,rcx
    je .innerifsucceeds

    mov rax,qword [g]  ;(g != a)
    mov rcx,qword [a]
    cmp rax,rcx
    je .skipif

    mov rcx,qword [c]  ;(g != c)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [d]  ;(g != d)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [e]  ;(g != e)
    cmp rax,rcx
    je .skipif

.innerifsucceeds:
    call bf
    
.skipif:
    mov rax,qword [e] ; e++
    inc rax
    mov qword [e],rax
    jmp .nextfor
   
.donefor:
    xor rax,rax                  
    leave                        
    ret   
    
segment .text
    
bf:                              
    push rbp                     
    mov rbp,rsp
    
    mov rax,qword [lo] ; f = lo
    mov qword [f],rax 

.nextfor:
    mov rax,qword [f]  ; f <= hi
    mov rcx,qword [hi] 
    cmp rax,rcx
    jg .donefor
    
    mov rax,qword [unique]
    mov rcx,FALSE
    cmp rax,rcx
    je .inif
 
; ((f != a) && (f != c) && (f != d) && (f != g) && (f != e))

    mov rax,qword [f]
    
    mov rcx,qword [a] ; (f != a)
    cmp rax,rcx
    je .skipif
    
    mov rcx,qword [c] ; (f != c)
    cmp rax,rcx
    je .skipif
    
    mov rcx,qword [d] ; (f != d)
    cmp rax,rcx
    je .skipif
    
    mov rcx,qword [g] ; (f != g)
    cmp rax,rcx
    je .skipif
    
    mov rcx,qword [e] ; (f != e)
    cmp rax,rcx
    je .skipif
    
.inif: 
    mov rax,qword [e] ; b = e + f - c;
    mov rcx,qword [f]
    add rax,rcx
    mov rcx,qword [c]
    sub rax,rcx
    mov qword [b],rax

; ((b >= lo) &&
;  (b <= hi) &&
;  ((!unique) || 
;   ((b != a) && 
;    (b != c) &&
;    (b != d) && 
;    (b != g) && 
;    (b != e) && 
;    (b != f)
;   )
;  )
; ) 

    mov rax,qword [b]  ;(b >= lo)
    mov rcx,qword [lo]
    cmp rax,rcx
    jl .skipif
    
    mov rax,qword [b]  ;(b <= hi)
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .skipif
    
    mov rax,qword [unique] ;(!unique)
    mov rcx,FALSE
    cmp rax,rcx
    je .innerifsucceeds

    mov rax,qword [b]  ;(b != a)
    mov rcx,qword [a]
    cmp rax,rcx
    je .skipif

    mov rcx,qword [c]  ;(b != c)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [d]  ;(b != d)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [g]  ;(b != g)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [e]  ;(b != e)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [f]  ;(b != f)
    cmp rax,rcx
    je .skipif

.innerifsucceeds:
    mov rax,qword [solutions] ; solutions++
    inc rax
    mov qword [solutions],rax
    
    mov rax,qword [show]
    cmp rax,TRUE
    jne .skipif
   
    call print_output
    
.skipif:
    mov rax,qword [f] ; f++
    inc rax
    mov qword [f],rax
    jmp .nextfor
   
.donefor:
    xor rax,rax                  
    leave                        
    ret   
    
print_output:                            
    push rbp                     
    mov rbp,rsp
        

; printf("%d %d %d %d %d %d %d\n",a,b,c,d,e,f,g);
    
    lea rdi,[output_fmt]
    mov rsi,qword [a]
    mov rdx,qword [b]
    mov rcx,qword [c]
    mov r8,qword [d]
    mov r9,qword [e]
    mov rax,qword [g]
    push rax
    mov rax,qword [f]
    push rax
    xor rax,rax
    call printf
    
    xor rax,rax                  
    leave                        
    ret 

```

Output

```txt


4 7 1 3 2 6 5
6 4 1 5 2 3 7
3 7 2 1 5 4 6
5 6 2 3 1 7 4
7 3 2 5 1 4 6
4 5 3 1 6 2 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4

8 unique solutions in 1 to 7

7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7

4 unique solutions in 3 to 9


2860 non-unique solutions in 0 to 9


```

