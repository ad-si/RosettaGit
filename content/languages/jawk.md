+++
title = "Jawk"
description = ""
date = 2011-09-09T04:42:38Z
aliases = []
[extra]
id = 10488
[taxonomies]
categories = []
tags = []
+++
Jawk runs an Awk program inside the [runs on vm::Java Virtual Machine](https://rosettacode.org/wiki/runs_on_vm::Java_Virtual_Machine).

## Quick start
Go to http://sourceforge.net/projects/jawk/ and download the latest jar. Then run a command like


```awk
$ java -jar jawk.1_02.jar 'BEGIN { print "Hello from JVM" }'
```


## Caveat
The space after -F is not optional.


```awk
$ java -jar jawk.jar -F: '{ print $1 }'   # FAILS!!
$ java -jar jawk.jar -F : '{ print $1 }'  # ok
```

