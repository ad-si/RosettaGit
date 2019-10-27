+++
title = "Talk:Memory layout of a data structure"
description = ""
date = 2014-12-31T15:00:59Z
aliases = []
[extra]
id = 10552
[taxonomies]
categories = []
tags = []
+++

Most of the "solutions" here fail because they don't actually control the memory layout. Most of these data structures have no way to be mapped to an arbitrary address such as a hardware register, such that the fields line up with the bits there, and can be used for individual access.
:Read the task description again and if you adhere to what it says you will note that mapping a data structure to an arbitrary address is ''not'' part of the task. --[[User:Paddy3118|Paddy3118]] 18:06, 20 September 2011 (UTC)
Please note that not only the byte order changes on machines of different endianess but also the bit order that the compiler produces may change. The examples seem to assume a little endian hardware. From Ada95 onwards you could use the Bit_Order attribute to enforce a 'Low_Order_First' bit order even on a big endian machine. [Holger Rauls 13 March 2012]
Also, from Ada 2012 onwards you can use the Scalar_Storage_Order attribute to enforce a 'Low_Order_First' byte order (Intel) even on a big endian machine (Motorola). --[[User:HolgerRauls|HolgerRauls]] ([[User talk:HolgerRauls|talk]]) 15:00, 31 December 2014 (UTC)
