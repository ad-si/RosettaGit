+++
title = "360 Assembly include"
description = ""
date = 2018-07-24T15:35:14Z
aliases = []
[extra]
id = 21917
[taxonomies]
categories = []
tags = []
+++


### FORMATF

An 'include' file to format a floating-point value.

```360asm
FORMATF  CNOP   0,4                ***WRITE Y,X FORMAT(F13.n)**********
*                                  (F0,R0)->R1
         STM    R14,R12,@FMTF0F    Store registers
		 LR     R1,R0              R0=decimals
         STH    R1,@FMTFNC         Number of decimals N
         SLA    R1,2               R1=N*4
         ME     F0,@FMTFCO(R1)     F0=F0*10**N
         STE    F0,@FMTFWF         WF=X*10**N
         MVI    @FMTFTS,X'00'      Initialize the sign field
         L      R9,@FMTFWF         Load the floating-point value
         CH     R9,=H'0'           and examine the sign bit.
         BZ     @FMTFDN            The value is zero, nothing to do.
         BNL    @FMTFNN            Is the value negative?
         MVI    @FMTFTS,X'80'      Yes, it is negative.
         N      R9,=X'7FFFFFFF'    Zero out the sign bit.
@FMTFNN  LR     R8,R9              Copy the value into R8
         N      R8,=X'00FFFFFF'    Examine the fraction.  Is it 0?
         BNZ    @FMTFNZ            No, keep on working
         SR     R9,R9              Yes, the value is zero.  So set
         B      @FMTFDN            the result as 0 and exit.
@FMTFNZ  LR     R8,R9              Copy the value into R8
         N      R8,=X'FF000000'    Isolate the characteristic field
         SRL    R8,24              Shift to least significant byte
         CH     R8,=H'64'          Is exponent big enough? 16**0
         BH     @FMTFO1            Yes, number is not < 1.
         SR     R9,R9              No, set result to zero
         B      @FMTFDN            and be done with it.
@FMTFO1  CH     R8,=H'72'          Is the exponent too big? 2**32
         BH     @FMTFOV            overflow (72-64=8 16**8=2**32)
         SR     R8,R8              Set R8 to zero
         SLDL   R8,8               Shift two high-order digits into R8
         CH     R8,=H'72'          Is the exponent an 8?
         BL     @FMTFDI            Yes, we can continue
         CH     R9,=H'0'           Is the sign bit set?
         BNP    @FMTFOV            overflow, the high-order bit is 1
@FMTFDI  SH     R8,=H'72'          Produce (Characteristic - 72)
         LCR    R8,R8              Produce (72 - Characteristic)
         SLL    R8,2               Multiply by 4
         SRL    R9,0(R8)           Shift R9 by the amount in R8
@FMTFSV  SR     R8,R8              Set R8 to 0.
         IC     R8,@FMTFTS         Load the sign value
         CH     R8,=H'0'           Is the sign bit set?
         BZ     @FMTFDN            No, we are OK
         LCR    R9,R9              Negate the absolute value
@FMTFIP  B      @FMTFDN            Sign OK
@FMTFOV  MVC    @FMTFDF,=30C'*'
         B      @FMTFRT
@FMTFDN  ST     R9,@FMTFBI
         CVD    R9,@FMTFPA         to fixed(15)
         MVC    @FMTFMA,@FMTFMO
         LA     R1,@FMTFMA+10
         SH     R1,@FMTFNC
         MVI    0(R1),X'21'        10-N
         MVC    @FMTFDE,@FMTFMA
         EDMK   @FMTFDE,@FMTFPA+2  fixed(11,N)-> pic' (10-N)#(N+1)9S'
         BCTR   R1,0
         MVC    0(1,R1),@FMTFDE+12
         LA     R1,12              12-N
         SH     R1,@FMTFNC
         EX     R1,@FMTFM1         MVC @FMTFDF(0),@FMTFDE on 13-N
         LA     R2,@FMTFDF+12
         SH     R2,@FMTFNC
         MVI    0(R2),C'.'
         LA     R3,@FMTFDE+12
         SH     R3,@FMTFNC         R3=@(@FMTFDE)+12-@FMTFNC
         LA     R2,1(R2)           R2=@ after the point in @FMTFDF
         LH     R1,@FMTFNC
         BCTR   R1,0
         EX     R1,@FMTFM2         MVC 0(0,R2),0(R3) on @FMTFNC
         B      @FMTFRT
@FMTFM1  MVC    @FMTFDF(0),@FMTFDE len=13-N
@FMTFM2  MVC    0(0,R2),0(R3)      len=N
@FMTFRT  LM     R14,R12,@FMTF0F
		 LA     R1,@FMTFDF
         BR     R14
@FMTFXX  DS     E
@FMTFNC  DS     H
@FMTFCO  DC     E'1E0'             1
         DC     E'1E1'             10
         DC     E'1E2'             100
         DC     E'1E3'             1000
         DC     E'1E4'             10000
         DC     E'1E5'             100000
         DC     E'1E6'             1000000
         DC     E'1E7'             10000000
         DC     E'1E8'             100000000
         DC     E'1E9'             1000000000
@FMTFWF  DS     F
@FMTFBI  DS     F                  dcl 32-bit fixed integer
@FMTFTS  DS     X
@FMTFMO  DC     X'40',11X'20',X'60' CL13
@FMTFMA  DS     CL13
@FMTFDE  DS     CL13               pic'B###99999999S'
@FMTFDF  DS     CL13               pic'S###9V.9999999'
@FMTFPA  DS     PL8                dec fixed(15)
@FMTF0F  DS     15F                save regs
*        END    FORMATF            ------------------------------------
```

