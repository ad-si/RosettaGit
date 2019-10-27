+++
title = "Talk:Variable-length quantity"
description = ""
date = 2010-10-18T08:48:50Z
aliases = []
[extra]
id = 8491
[taxonomies]
categories = []
tags = []
+++

==Concrete==
How about making the task more concrete by, say, changing it to a task to show the bits of 12345678901234566789 as binary then as Vlq - mabe grouping into 8 bit octets for readability? Then a function to change back and display the result . --[[User:Paddy3118|Paddy3118]] 16:00, 14 October 2010 (UTC)
:Done! [[User:Dingowolf|dingowolf]] 16:58, 14 October 2010 (UTC)

==Vlq octet order==
The wp article states:
: ''VLQ octets are arranged most significant first in a stream.''
Which doesn't make sense as the least significant octet has to be sent first so that its eighth bit can be sampled to see if there will be another octet or not. --[[User:Paddy3118|Paddy3118]] 15:14, 15 October 2010 (UTC)
:I think, in a '''mixed data''' stream for storage or transmission, the VLQ has to be like a big ending number , so that the least significant octet act as a terminal, if no other information telling the length of the VLQ. If the stream contain only VLQ, the least significant octet can sent first. [[User:Dingowolf|dingowolf]] 16:27, 15 October 2010 (UTC)
:What? I read the docs as saying that it is the LSO that has its low bit cleared, making perfect sense as a terminator. –[[User:Dkf|Donal Fellows]] 13:48, 17 October 2010 (UTC)

I interpret an 8-to-14 bit integer being sent as the first and second bytes being, in order:
{| class="wikitable" style="font-family: monospace, courier, courier new; font-size: 110%;"
|+ VLQ Octet #0
!  7
!  6
!  5
!  4
!  3
!  2
!  1
!  0
|-
! 
! 2<sup>6</sup>
! 2<sup>5</sup>
! 2<sup>4</sup>
! 2<sup>3</sup>
! 2<sup>2</sup>
! 2<sup>1</sup>
! 2<sup>0</sup>
|-
| align="center"|1
| colspan="7" align="center"|B<sub>n</sub>
|}
{| class="wikitable" style="font-family: monospace, courier, courier new; font-size: 110%;"
|+ VLQ Octet #1
!  7
!  6
!  5
!  4
!  3
!  2
!  1
!  0
|-
! 
! 2<sup>13</sup>
! 2<sup>12</sup>
! 2<sup>11</sup>
! 2<sup>10</sup>
! 2<sup>9</sup>
! 2<sup>8</sup>
! 2<sup>7</sup>
|-
| align="center"|0
| colspan="7" align="center"|B<sub>n</sub>
|}

i.e. the first byte/octet through would have its high bit set and the last octet would not. --[[User:Paddy3118|Paddy3118]] 17:17, 17 October 2010 (UTC)

I agree with Paddy.  See also: http://en.wikipedia.org/wiki/File:Uintvar_coding.svg --[[User:Rdm|Rdm]] 23:07, 17 October 2010 (UTC)
