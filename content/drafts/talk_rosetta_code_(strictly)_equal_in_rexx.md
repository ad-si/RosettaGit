+++
title = "Talk:Rosetta Code/(Strictly) equal in Rexx"
description = ""
date = 2012-07-05T16:44:48Z
aliases = []
[extra]
id = 11958
[taxonomies]
categories = []
tags = []
+++

Rexx has two comparison operators (for testing equality)

```txt

strictly equal a==b tests the values byte for byte, so '1'==' 01e0 ' -> 0 (false)
         equal a=b tests the values more generously 
                       it strips leading and trailing blanks and, if both values are
           numbers compares them arithmetically (observing the Numeric Digits setting)
                                                    so '1'=' 01e0 ' -> 1 (true)

```

I performed a little performance test on TSO.

```rexx
/* REXX *************************************************************** 
* Compare the performance of equal vs. strictly equal                   
* 05.07.2012 Walter Pachl                                               
**********************************************************************/ 
Parse Version v                                                        
Say 'eqtest on Rexx Version' v                                         
Say ' Hits Misses test =      Test ==      == improvement'             
Call random 1,10,12345              /* set seed                      */
Do i=1 To 100000                                                       
  x.i=random()                                                         
  End                                                                  
Do rep=1 To 5                                                          
  cnt.=0                                                               
  Call time 'R'                                                        
  Do i=1 To 100000                                                     
    If x.i=999 Then cnt.1=cnt.1+1                                      
                Else cnt.0=cnt.0+1                                     
    End                                                                
  cnt.=0                                                               
  te=time('R')                                                         
  Do i=1 To 100000                                                     
    If x.i==999 Then cnt.1=cnt.1+1                                     
                Else cnt.0=cnt.0+1                                     
    End                                                                
  tse=time('E')                                                        
  Say right(cnt.0,5) right(cnt.1,5),                                   
      format(te,2,8) format(tse,2,8) format((100*(te-tse)/te),3,8)'%'  
  End   
```

The results:

```txt

Interpreter:
eqtest on Rexx Version REXX370 3.48 01 May 1992
 Hits Misses test =      Test ==      == improvement
99894   106  0.15227900  0.14603100   4.10299516%
99894   106  0.14801500  0.14583200   1.47485052%
99894   106  0.15465700  0.15261200   1.32228092%
99894   106  0.15660000  0.14936200   4.62196679%
99894   106  0.15060800  0.17170400 -14.00722410%

Compiled Rexx:
eqtest on Rexx Version REXXC370 3.48 23 Dec 1999
 Hits Misses test =      Test ==      == improvement
99894   106  0.01011300  0.01730900 -71.15593790%
99894   106  0.00983700  0.01328100 -35.01067400%
99894   106  0.00973800  0.01293300 -32.80961180%
99894   106  0.00984500  0.01306300 -32.68664300%
99894   106  0.01005300  0.01423700 -41.61941710%

```


As I was part of the Compiler Development Team in the former IBM Lab Vienna
I am rather proud of my friends' achievement!

You are invited to test this on your platform with your Rexx!
--[[User:Walterpachl|Walterpachl]] 07:28, 5 July 2012 (UTC)

Here the result on my pretty old desktop Windows:

```txt

eqtest on Rexx Version REXX-ooRexx_4.1.1(MT) 6.03 16 May 2012
 Hits Misses test =      Test ==      == improvement
99896   104  0.12500000  0.12500000   0.00000000%
99896   104  0.12500000  0.10900000  12.80000000%
99896   104  0.12500000  0.14100000 -12.80000000%
99896   104  0.12500000  0.10900000  12.80000000%
99896   104  0.12500000  0.12500000   0.00000000%   

```

--[[User:Walterpachl|Walterpachl]] 16:13, 5 July 2012 (UTC)


The following results are from a MacBook Pro:
* '''Processor:'''  2.66 GHz Intel Core 2 Duo
* '''Memory:'''  4 GB
* '''Software:'''  Mac OS X Lion 10.7.4
'''ooRexx:'''

```txt

eqtest on Rexx Version REXX-ooRexx_4.1.1(MT) 6.03 5 Jun 2011
 Hits Misses test =      Test ==      == improvement
99896   104  0.11199600  0.10708800   4.38229937%
99896   104  0.11749800  0.10943300   6.86394662%
99896   104  0.11566000  0.13758600 -18.95728860%
99896   104  0.11635800  0.12008500  -3.20304577%
99896   104  0.19133300  0.18279400   4.46289976%

```

'''Regina:'''

```txt

eqtest on Rexx Version REXX-Regina_3.5(MT) 5.00 31 Dec 2009
 Hits Misses test =      Test ==      == improvement
99901    99  0.07463500  0.06828200   8.51209218%
99901    99  0.05713800  0.06026000  -5.46396444%
99901    99  0.06151100  0.07270400 -18.19674530%
99901    99  0.07320100  0.08063500 -10.15559900%
99901    99  0.07326500  0.07511300  -2.52235037%

```

--[[User:Alansam|Alansam]] 16:44, 5 July 2012 (UTC)
