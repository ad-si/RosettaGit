+++
title = "Talk:Draw a clock"
description = ""
date = 2011-07-20T19:39:37Z
aliases = []
[extra]
id = 10018
[taxonomies]
categories = []
tags = []
+++

What is supposed to happen here?  I could not get the C code to compile.  --[[User:Rdm|Rdm]] 01:50, 4 July 2011 (UTC)
:Oh sorry, should have said it's C99, <code>gcc -std=c99 -Wall -lm stuff.c</code>, for example. --~~
::Ok, I built it
::
```bash
$ CFLAGS=-std=c99 make clock
```

::but when I run it my display does not look very clock like:
::
```txt


       :--::...                                :----------:: .                             ..::--- *%*
                   . c&amp;-                                   .-.c&amp;:                                  .-:.aa:
                    :-. :&amp;c.                                :-.  -&amp;c                                :-:   -#*
```

::could you maybe post a screenshot or something?  Thanks.  --[[User:Rdm|Rdm]] 13:30, 4 July 2011 (UTC)
:::I changed a code a little, it should work now (your terminal's interpretation of carriage return is different from mine).  "Screenshot":<lang>
                              :a&:      
                             *#c.       
                           :a&-         
                          -&a.          
                        .c#*            
                       -&a:             
                     .c#*               
                   .-&a:                
                 . *%*                  
               .:-- .:-:                
              :----.  .--.              
            .----:      :-:.            
          .:---:.        .:-:.          
          .:-:.            .:-:         
                             .--. 
```


Because the task is called "Draw a clock", rather than "Draw a timer" maybe we should have something about a clock in it. Perhaps: As an option show the face of an analogue clock with animated hour minute and second hands. (or alternatively rename to "Draw a timer". [[User:Markhobley|Markhobley]] 17:43, 20 July 2011 (UTC)

: Isn't the task description specific enough? --[[User:Ledrug|Ledrug]] 19:14, 20 July 2011 (UTC)
::Yeah it is. But it is not really about drawing a clock. We could rename this "Draw a timer". [[User:Markhobley|Markhobley]] 19:31, 20 July 2011 (UTC)
::FWIW, We could have a task for drawing an analogue clock, which includes showing the numbers round the face, and the animated hour, minute, and second hands. That would be better suited to "Draw a clock". [[User:Markhobley|Markhobley]] 19:35, 20 July 2011 (UTC)
:::What does a "timer" look like? Do you mean a stopwatch? "Timer" to me means a text clock (i.e. "00:00:00.000"), which is not what this task wants. I think "clock" is a nice general word to cover lots of possibilities (though not the obviously silly suggestion of a mouth counting "one on thousand"), and it's a word that people use more commonly. "Analog" would be a welcome addition to make sure people don't print numbers alone. --[[User:Mwn3d|Mwn3d]] 19:39, 20 July 2011 (UTC)
