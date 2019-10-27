+++
title = "Bitwise IO/MIPS Assembly"
description = ""
date = 2010-06-08T18:06:50Z
aliases = []
[extra]
id = 7492
[taxonomies]
categories = []
tags = []
+++


```mips
    .data

# the following are used by the subroutines
bitbuf:  .byte 0, 0
cumulus: .word 0
rbitbuf: .byte 0, 0
rfree:   .word 0

# these are instead for testing
outbuf:  .space 16
abuf:    .space 16

txt:     .asciiz  "This is a test"


    .text

main:  
    # ---- encode txt to buf -----
    la    $s0, txt
    la    $s1, outbuf
encloop:
    lbu   $a0, ($s0)
    beq   $a0, $0, txtend
    li    $a1, 7
    move  $a2, $s1
    jal   bits_write
    addu  $s0, $s0, 1
    move  $s1, $v1
    b     encloop
txtend:
    move  $a1, $s1
    jal   bits_flush

    move  $s1, $v0     # update buf, will be used as "terminator"
    la    $s0, outbuf

    # ---- write buffer
wrtchar:
    beq   $s0, $s1, wrtend # while $s0 < $s1 {
    lb    $a0, ($s0)
    li    $v0, 11
    syscall                #   print_char($a0)
    addi  $s0, $s0, 1      #   inc $s0
    b     wrtchar          # }
wrtend:

    # ---- decode from buffer in bufferb----
    # $s1 is still our end marker
    la    $s3, outbuf           # prev. output becomes input
    li    $s7, 7                # commodity
    la    $s4, abuf             # current output buf

decloop:
    bge   $s3, $s1, decend      # while $s3(input) < $s1(limit)
    move  $a0, $0               #   clean acc.
    move  $a1, $s7              #   7 bits
    move  $a2, $s3              #   from input buf
    jal   bits_read             #   read 7 bits into acc.
    move  $s3, $v0              #   update input buf
    sb    $v1, ($s4)
    addi  $s4, $s4, 1           #   write char and update outp buf ptr
    b     decloop               # end while
decend:

    # lets output abuf; $s4 points to end of abuf,
    # we need to mark it with 0 to print it as a str
    sb    $0, ($s4)
    la    $a0, abuf
    li    $v0, 4
    syscall                     # print_string(abuf)

    li    $v0, 10               # exit
    syscall


# read_bits
# $a0 acc.
# $a1 how many (max 32)
# $a2 input buffer
# > $v1 (new acc)
# > $t0 num of read bits or less (failure)
# > $v0 updated input buffer
bits_read:
    sub   $sp, $sp, 20          # local vars
    sw    $ra, 0($sp)           # save regs
    sw    $s0, 4($sp)
    sw    $s1, 8($sp)
    sw    $s2, 12($sp)
    sw    $s3, 16($sp)

    move  $s3, $a2              # $s3 input buf
    move  $s0, $a0              # $s0 acc
    move  $s1, $a1              # $s1 n
    move  $s2, $0               # rbit = 0
    li    $t0, 32
    bgt   $a1, $t0, brexit      # if n > 32 then return

brloop:
    ble   $s1, $0, brexit       # while n > 0 (if <= 0 break)
    move  $a0, $s0
    move  $a1, $s3
    jal   read_bit              #   read 1 bit into acc.
    move  $s3, $v0              #   update input buf
    li    $t1, 1
    bne   $t0, $t1, brexit      #   exit if not read
    move  $s0, $v1              #   get updated acc
    sub   $s1, $s1, 1           #   n--
    addi  $s2, $s2, 1           #   rbit++
    b     brloop
brexit:
    move  $t0, $s2              # currently read bits
    move  $v1, $s0              # new accumulator
    move  $v0, $s3              # updated input ptr
    lw    $ra, 0($sp)           # restore regs
    lw    $s0, 4($sp)
    lw    $s1, 8($sp)
    lw    $s2, 12($sp)
    lw    $s3, 16($sp)
    addu  $sp, $sp, 20          # and free stack
    jr    $ra                   # return


# get last (maybe incomplete) accumulated byte
# $a0  accumulator
# >$v0  free storage
bits_getlast:
    lw    $t0, rfree
    li    $t1, 8   
    sub   $t1, $t1, $t0          # sochar - rfree
    sllv  $a0, $a0, $t1          # *d << (8 - rfree)
    lbu   $t2, rbitbuf
    srlv  $t2, $t2, $t0          # rbitbuf >> rfree
    sb    $0, rbitbuf            # rbitbuf = 0
    sw    $0, rfree              # rfree = 0
    move  $v0, $t1               
    jr    $ra                    # return sochar - rfree


# read a single bit
# $a0 accumulator
# $a1 input buffer
# > $v1 new accumulator
# > $t0 bit read (1 or not 1; in this impl, reading
#       can't fail unless input buffer is an invalid ptr
# > $v0 updated input buffer
read_bit:
    sub    $sp, $sp, 20          # local stack,
    sw     $s0, 0($sp)           # save regs
    sw     $s1, 4($sp)
    sw     $s2, 8($sp)
    sw     $s3, 16($sp)

    move   $s0, $a0              # save args
    move   $s3, $a1              # $s0 = acc, $s3 = inbuf

    lw     $s1, rfree            # preload rfree and
    lbu    $s2, rbitbuf          # rbitbuf

    bne    $s1, $0, skipif       # if rfree == 0 then

    lbu    $s2, ($s3)            # rbitbuf = nextchar()
    addi   $s3, $s3, 1           # update it inp buf

    li     $s1, 8                #   rfree = 8
skipif:
    sll    $s0, $s0, 1           #   *d << 1
    srl    $t0, $s2, 7           #   rbitbuf >> (8-1)
    andi   $t0, $t0, 1           #     & 1
    or     $s0, $s0, $t0         #         | *d
    sll    $s2, $s2, 1           #   rbitbuf << 1
    sub    $s1, $s1, 1           #   rfree--

    sw     $s1, rfree
    sb     $s2, rbitbuf          # update rfree, rbitbuf
    li     $t0, 1                # $t0 returns 1

    move   $v1, $s0              # return the acc. in $v1
    move   $v0, $s3              # and updated inp buf in $v0
    lw     $s0, 0($sp)           # restore regs
    lw     $s1, 4($sp)  
    lw     $s2, 8($sp) 
    lw     $s3, 16($sp)
    add    $sp, $sp, 20          # .. and stack
    jr     $ra                   # return($v0, $v1)

    

# bits_write; write upto 32 bits
# $a0 holds the bits (right aligned)
# $a1 how many bits are significative
# $a2 dest buf
# > $v1 updated buf ptr
# > $v0 written bits (-1 on error)
bits_write:
    sub    $sp, $sp, 28          # some local storage
    sw     $fp, 20($sp)
    move   $fp, $sp
    sw     $s1, 4($fp)           # save regs and values
    sw     $s0, 0($fp)
    sw     $s2, 8($fp)
    sw     $s3, 12($fp)
    sw     $ra, 16($fp)
    sw     $s4, 24($fp)

    move   $s4, $a2

    move   $s1, $a1              # n is $s1
    li     $t0, 32
    bgt    $a1, $t0, error       # n > 32 => return(-1)
    
    sub    $t0, $t0, $a1         # 32 - n
    sllv   $s0, $a0, $t0         # d << (32-n)
    move   $s3, $0
loop:
    blez   $s1, exit0            # if n <= 0 then break
    sub    $s1, $s1, 1           # n--
    addu   $s3, $s3, 1           # wbit++
    move   $a0, $s0
    rol    $a0, $a0, 1           # place "last" bit in first pos.
    move   $a1, $s4
    jal    appendbit             # append it
    sll    $s0, $s0, 1           # dpad << 1
    move   $s4, $v1              # update buf ptr
    b      loop                  # loop
error:
    li     $v0, -1
    b      exit
exit0:
    move   $v0, $s3
exit:
    lw     $s1, 4($fp)           # restore regs
    lw     $s0, 0($fp)
    lw     $s2, 8($fp)
    lw     $s3, 12($fp)
    move   $v1, $s4
    lw     $s4, 24($fp)
    lw     $ra, 16($fp)
    move   $sp, $fp              # and release stack
    lw     $fp, 20($sp)
    addi   $sp, $sp, 28
    jr     $ra                   # return wbit, ptr to byte beyond last
                                 # written


# flush bits
# $a1  output buf
# >$v0 adv. ptr to buf.
bits_flush:
    li     $t0, 8
    lw     $t1, cumulus
    sub    $t0, $t0, $t1         # 8 - cumulus
    lbu    $a0, bitbuf
    sllv   $a0, $a0, $t0         # bitbuf <<= (8 - cumulus)
    sb     $a0, ($a1)            # write to buf
    addi   $v0, $a1, 1           # inc buf ptr
    sb     $0, bitbuf            # bitbuf := 0
    sw     $0, cumulus           # cumulus := 0
    jr     $ra


# add a single bit.
# $a0 holds the bit (as LSB i.e. "rightmost bit")
# $a1 output buf
# > $v0 number of written bits (always 1)
# > $v1 incremented buf ptr
appendbit:
    sub    $sp, $sp, 16        # save s-regs on stack
    sw     $s0, 0($sp)
    sw     $s1, 4($sp)
    sw     $s2, 8($sp)
    sw     $s3, 12($sp)

    move   $s3, $a1            # save buf ptr

    move   $s0, $a0            # copy argument                        
    lw     $s1, cumulus        # get cumulus
    lbu    $s2, bitbuf         # preload bitbuf
    li     $t1, 8
    bne    $s1, $t1, mroom     # if cumulus = 8 then
    sb     $s2, ($s3)          #   write bitbuf to buf
    add    $s3, $s3, 1         #   inc buf.
    move   $s2, $0             #   bitbuf := 0
    move   $s1, $0             #   cumulus := 0
mroom:                         # endif
    sll    $s2, $s2, 1         # bitbuf <<= 1
    andi   $s0, $s0, 1         # arg & 1
    or     $s2, $s2, $s0       # bitbuf |= arg
    sb     $s2, bitbuf         # store bitbuf
    addi   $s1, $s1, 1         # cumulus++
    sw     $s1, cumulus        # store cumulus

    lw     $s0, 0($sp)         # restore regs
    lw     $s1, 4($sp)
    lw     $s2, 8($sp)
    move   $v1, $s3
    lw     $s3, 12($sp)
    addi   $sp, $sp, 16        # restore stack
    li     $v0, 1
    jr     $ra                 # return(1)
```

