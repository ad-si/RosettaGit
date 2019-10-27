+++
title = "M2000 Interpreter rational numbers"
description = ""
date = 2018-10-15T13:55:29Z
aliases = []
[extra]
id = 22030
[taxonomies]
categories = []
tags = []
+++


## M2000 Interpreter


```M2000 Interpreter

Module RationalNumbers {
      Class Rational {
            numerator as decimal, denominator as decimal
            gcd=lambda->0
            lcm=lambda->0
            operator "+" {
                 Read l
                 denom=.lcm(l.denominator, .denominator)
                 .numerator<=denom/l.denominator*l.numerator+denom/.denominator*.numerator
                 if .numerator==0 then denom=1
                 .denominator<=denom
            }
            Operator Unary {
                  .numerator-!
            }
            Operator "-" {
                  Read l
                  Call Operator "+", -l
            }
            Operator "*" {
                  Read l
                  g1=.gcd(l.numerator,.denominator)
                  g2=.gcd(.numerator, l.denominator)
                  Push l.numerator/g1*.numerator/g2
                  Push l.denominator/g2*.denominator/g1
                  Read .denominator, .numerator
      
            }
            Function Inverse {
                  if .numerator==0 then Error "Division by zero"
                  ret=This
                  sign=sgn(ret.numerator) : if sign<0 then ret.numerator-!
                  swap ret.numerator, ret.denominator
                  if sign<0 then ret.numerator-!
                  =ret
            }
            Operator "/" {
                  Read l
                  call operator "*", l.inverse()
            }
            Function Power {
                  Read pow as long
                  ret=This
                  ret.numerator<=.numerator^pow
                  ret.denominator<=.denominator^pow
                  =ret
            }
            Operator "=" {
                  Read l
                  Def boolean T=True, F=False
                  if Abs(sgn(l.numerator))+Abs(sgn(.numerator))=0 then Push T: exit
                  if sgn(l.numerator) <>sgn(.numerator) then Push F : exit
                  pcomp=l/this
                  PUSH pcomp.numerator=1 and pcomp.denominator=1
            }
            Operator ">" {
                  Read l
                  Def boolean F
                  if Abs(sgn(l.numerator))+Abs(sgn(.numerator))=0 then Push F: exit
                  if sgn(l.numerator)=0 then {
                        PUSH .numerator>0
                  } Else {      
                        pcomp=this/l
                        PUSH pcomp.real>1
                  }
            }
            Operator ">=" {
                  Read l
                  if sgn(l.numerator)=0 then {
                        PUSH .numerator>=0
                  } Else {      
                        pcomp=this/l
                        PUSH pcomp.real>=1
                  }
            }      
            Operator "<" {
                  Read l
                  Def boolean F
                  if Abs(sgn(l.numerator))+Abs(sgn(.numerator))=0 then Push F: exit
                  if sgn(l.numerator)=0 then {
                        PUSH .numerator<0
                  } Else {      
                        pcomp=this/l
                        PUSH pcomp.real<1
                  }
            }
            Operator "<=" {
                  Read l
                  if sgn(l.numerator)=0 then {
                        PUSH .numerator<=0
                  } Else {      
                        pcomp=this/l
                        PUSH pcomp.real<=1
                  }            
            }
            Operator "<>" {
                  Read l
                  if sgn(l.numerator)=0 then {
                        PUSH .numerator<>0
                  } Else {      
                        pcomp=this/l
                        PUSH pcomp.real<>1
                  }            
            }
            Group Real {
                  value {
                        link parent numerator, denominator to n, d
                        =n/d
                  }
            }
            Group ToString$ {
                 value {
                        link parent numerator, denominator to n, d
                        =Str$(n)+"/"+Str$(d,"")
                  }      
            }
            class:
            Module Rational (.numerator, .denominator) {
                  if .denominator<=0 then Error "Positive only denominator"
                  gcd1=lambda (a as decimal, b as decimal) -> {
                        if a<b then swap a,b
                        g=a mod b
                        while g {
                              a=b:b=g: g=a mod b
                        }
                              =abs(b)
                  }
                  .gcd<=gcd1
                  .lcm<=lambda gcd=gcd1 (a as decimal, b as decimal) -> {
                        =a/gcd(a,b)*b
                  }
            }
      }
      Print rational(-3,3)<>rational(-3,3)
      M=Rational(10, 150)
      N=Rational(2, 4)
      Z=M+N
      Print Z.numerator, Z.denominator
      Print 10/150@+2/4@
      Print Z.real
      Z=-M+N
      Print Z.numerator, Z.denominator
      Print -10/150@+2/4@
      Print Z.real
      Z=M-N
      Print Z.numerator, Z.denominator
      Print 10/150@-2/4@
      Print Z.real
      Z=M*N
      Print Z.numerator, Z.denominator
      Print (10/150@)*(2/4@)
      Print Z.real
      Z=M/N
      Print Z.numerator, Z.denominator
      Print (10/150@)/(2/4@)
      Print Z.real
      Z=Z.Power(2)
      Print Z.real
      Print Z=Z
      Print Z=N
      Print Z=-Z
      ZZ=-Z
      Print ZZ=ZZ
      Print -Z=-Z
      Print Z.numerator, Z.denominator
      Print Z.real, Z.tostring$
      \\ Array of rational numbers
      Dim K(100)=rational(1,1)
      M=K(4)+K(3)
      Print M.real
      Print K(4).toString$
}
RationalNumbers

```

