+++
title = "C0H Implementation"
description = ""
date = 2011-09-01T23:44:38Z
aliases = []
[extra]
id = 10441
[taxonomies]
categories = []
tags = []
+++
c0h is the implementation for the [C0H](https://rosettacode.org/wiki/C0H) language on Unix machines; porting to Windows and other platforms should be fairly easy. 

In fact c0h is a simple shell script that efficiently uses the already available [C](https://rosettacode.org/wiki/C) compiler named cc:

```c
#!/bin/bash

FILENAME=$1
FILESIZE=$(stat -f%z "$FILENAME")
if [ $FILESIZE -eq 0 ] 
then
  cat << EOF > $FILENAME
#include <stdio.h>
int main(char args[]) {printf("Goodbye, World!\\n");}
EOF
fi

cc $FILENAME

if [ $FILESIZE -eq 0 ] 
then
  echo -n > $FILENAME
fi

```


A typical test session would look like:

```bash

$  echo -n >c0.c
$ ./c0h c0.c
$ ./a.out
Goodbye, World!

```

