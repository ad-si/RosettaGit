+++
title = "Interactive Active ALGOL 68"
description = ""
date = 2011-07-17T12:21:32Z
aliases = []
[extra]
id = 2370
[taxonomies]
categories = []
tags = []
+++

{{implementation|ALGOL 68}}{{stub}}
==Sample==
With [[Interactive Active ALGOL 68]] it may be necessary to include 
appropriate "job cards" or precludes in order for the programs to 
compile successfully.  Example:
{|border="1" style="border-collapse: collapse; border: 5px double grey;"  align="center" 
|| Brief Algol68
|| Algol68 as in rosettacode
|| Actual Interactive Active ALGOL 68 code
|-
||

```algol68
print(("Hello, world!",new line))
```

||

```algol68
main:(
  print(("Hello, world!",new line))
)
```

||

```algol68
*LIB transput
print (("Hello, world!", new line))
```

|}

==See also==
* Download incremental ALGOL 68 compiler - [http://www.nunan.fsnet.co.uk/algol68/a68mk2.zip]
