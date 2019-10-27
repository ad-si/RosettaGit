+++
title = "Talk:Check input device is a terminal"
description = ""
date = 2013-03-28T19:37:13Z
aliases = []
[extra]
id = 12924
[taxonomies]
categories = []
tags = []
+++

==Rename?==
I suggest we rename this to "Check if input device is a terminal". [[User:Markhobley|Markhobley]] 22:43, 12 February 2013 (UTC)

: Or ''"Determine input from terminal"''? --[[User:Paddy3118|Paddy3118]] 22:48, 12 February 2013 (UTC)

==Interactive or not?==

Actually, it might be better to know whether the program is to operate interactively or not. I think this is probably what we need to know from an application point of view. To be interactive, we need to know that both the input and device and the output device is a terminal. If either the input device or output device are not terminals, then in most cases we probably do not want to be interactive.

I suggest we return three flags:

 * Flag indicating input device is a terminal (applications using the code snippet should not obtain use input)
 * Flag indicating that the output device is a terminal (if not then the application should not attempt to display)
 * Flag indicating interactive (ie both of the above flags are set)

This could mean a potential rename to "Terminal control/Determine interactive status" (or something like that).

I suggest we drop the obtain input bit of the task, because we already have separate tasks for that.

[[User:Markhobley|Markhobley]] 15:58, 20 February 2013 (UTC)

Scrub the three flags crap. We can just return a value. Here is my proposed perl version:


```perl
sub checkinteractiverestriction {
  my $restrictions = 0;
  if (!-t STDIN) {
    $restrictions |= 1;    # No keyboard
  }
  if (!-t STDOUT) {
    $restrictions |= 2;    # No terminal
  }
  # Standard error redirection is not a restriction
  # if (!-t STDERR) {
  #  $restrictions |= 4;
  # }
  return $restrictions;
}

if (checkinteractiverestriction() == 0) {
  print "We are ok to operate interactively!";
}
```


[[User:Markhobley|Markhobley]] 10:37, 21 February 2013 (UTC)
