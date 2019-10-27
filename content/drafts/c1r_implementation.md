+++
title = "C1R Implementation"
description = ""
date = 2013-09-25T15:28:04Z
aliases = []
[extra]
id = 10897
[taxonomies]
categories = []
tags = []
+++

{{implementation|C1R}}
c1r is the implementation for the [[C1R]] language on Unix machines; porting to Windows and other platforms should be fairly easy. 

In fact c1r is a simple shell script that efficiently uses the already available [[C]] compiler named cc. 
The "Quine" task required special treatment.


```c
#! /bin/bash

# C1R compiler

# remove HTML tags, but replace "<br />" by newlines
function removeHTMLtags() {
 sed -e 's~<br */>~\
~g' | sed -e :a -e 's/<[^>]*>//g;/</N;//ba' 
}

# unescape HTML codes: replace "&lt;" by "<" etc
function unescapeHTML() {
 sed -e 's/&lt\;/</g;s/&gt\;/>/g;s/&nbsp\;/ /g;s/&#160\;/ /g;s/&#xA0\;/ /g;s/&quot\;/"/g;s/&#40\;/(/g;s/&#41\;/)/g;s/&#91\;/[/g;s/&#93\;/]/g;s/&#123\;/{/g;s/&#125\;/}/g'
}

FILENAME=$1

if [ -z ${FILENAME} ] 
then
  echo "Usage: $0 <fileName>"
  exit 1
fi

FILENAME1=${FILENAME}.c
ROSETTAURL=rosettacode.org/wiki
WORDCOUNT=`cat $FILENAME|wc -l`

cp ${FILENAME} ${FILENAME1}

if [ $WORDCOUNT -eq 1 ]
then
  # Note: the self-printing Quine program requires special treatment
  if [ `cat $FILENAME` = "Quine" ]
  then 
    cat << EOF > $FILENAME1
#include <stdio.h>
int main(char args[]) {printf("Quine\\n");}
EOF
  else 
    PAGEURL=$ROSETTAURL/`cat $FILENAME`
    curl $PAGEURL 2>/dev/null | grep -m 1 "<pre class=\"c highlighted_source\">" | removeHTMLtags | unescapeHTML >${FILENAME1}
  fi
fi

cc $FILENAME1

```


A typical test session would look like:

```bash

$ echo Hello_world/Text >hw.c1r
$ ./c1r hw.c1r
$ ./a.out
Goodbye, World!

```

