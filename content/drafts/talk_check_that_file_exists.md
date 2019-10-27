+++
title = "Talk:Check that file exists"
description = ""
date = 2015-05-23T17:34:54Z
aliases = []
[extra]
id = 9709
[taxonomies]
categories = []
tags = []
+++

== Old solution for C ==


```c>#include<stdio.h

#include<dir.h>
#include<errno.h>

signed int fexist(char*s){
 FILE*f=fopen(s,"r");
 if(!f)return (errno==ENOENT)-1;
 fclose(f);
 return 1;
}

signed int direxist(char*s){
 if(chdir(s))return (errno==ENOENT)-1;
 return 1;
}

void report(char*name,signed int r){
 char*s="might";
 if(r>0)s="does";
 else if(!r)s="does not";
 printf("%s %s exist.\r\n",name,s);
}

void chkfile(char*s){
 report(s,fexist(s));
}

void chkdir(char*s){
 report(s,direxist(s));
}

int main(){
 chkfile("input.txt");
 chkfile("/input.txt");
 chkdir("docs");
 chkdir("/docs");
 return 0;
}
```


This used <code>chdir()</code> from [[POSIX]] to test a directory. I got compiler error, because [[OpenBSD]] has no <dir.h>; I must use <unistd.h>. I also got "input.txt does exist" when input.txt was a directory, because BSD can <code>fopen()</code> a directory. The current solution for C uses <code>S_ISREG()</code> and <code>S_ISDIR()</code> from POSIX. --[[User:Kernigh|Kernigh]] 00:59, 18 May 2011 (UTC)

==Rename==

I think this should be named "Check if file exists". I think we are checking, rather than ensuring. "Ensure" implies creation if file does not exist. [[User:Markhobley|Markhobley]] 20:13, 18 August 2011 (UTC)
:That seems fine. --[[User:Mwn3d|Mwn3d]] 20:35, 18 August 2011 (UTC)


==Extra credits==

Would it be possible to add a "Extra credits" for the test to make sure that a directory called input.txt is not identified as a file, and vice versa for docs? Or to just add it as a clarification. --Bengt Mon May 27 08:48:26 CEST 2013

== Old solution for GNU Awk ==


```AWK

# syntax: GAWK -f CHECK_THAT_FILE_EXISTS.AWK
BEGIN {
    check_exists("input.txt")
    check_exists("\\input.txt")
    check_exists("docs")
    check_exists("\\docs")
    exit(0)
}
function check_exists(name,  fnr,msg,rec) {
    while (getline rec <name > 0) {
      fnr++
      break
    }
    # "Permission denied" is for MS-Windows
    msg = (ERRNO == 0 || ERRNO ~ /Permission denied/ || fnr > 0) ? "exists" : "does not exist"
    printf("%s - %s\n",name,msg)
    close(name)
}

```


It reports "not exist" if input.txt is a 0-length file. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 17:34, 23 May 2015 (UTC)
