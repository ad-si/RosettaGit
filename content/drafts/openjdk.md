+++
title = "OpenJDK"
description = ""
date = 2014-11-06T14:29:58Z
aliases = []
[extra]
id = 9548
[taxonomies]
categories = []
tags = []
+++

{{implementation|Java}}

'''[http://openjdk.java.net/ OpenJDK]''' is an [[open source|free]] implementation of [[:Category:Java|Java]] for [[Linux]], [[Solaris]] and [[Windows]].<ref>[http://hg.openjdk.java.net/jdk7/build/raw-file/tip/README-builds.html#MBE OpenJDK Build README: Minimum Build Environments]</ref> 
There is also OpenJDK for [[BSD]]<ref>[http://openjdk.java.net/projects/bsd-port/ OpenJDK: BSD Port Project]</ref>. 
Several BSD and Linux distros carry packages of OpenJDK.

OpenJDK can compile and run Java programs. 
OpenJDK 6 implements Java 6, while OpenJDK 7 is a preview of future Java 7.

== Usage ==
OpenJDK is a set of command-line tools. 
If OpenJDK is installed in <tt>/usr/local/jdk-1.7.0</tt>, then <tt>/usr/local/jdk-1.7.0/bin</tt> needs to be added to the <tt>PATH</tt> environment variable. 
The two most important tools are the [[javac]] compiler, and the java runner.

This example program would compute 12 - 4.


```java
/* TwelveMinusFour.java */
public class TwelveMinusFour {
	public static void main(String[] args) {
		System.out.println(12 - 4);
	}
}
```


One can compile this program with javac, and run it with java.


```bash
$ javac TwelveMinusFour.java                                                   
$ java TwelveMinusFour                                                         
8
```


== References ==
<references />
