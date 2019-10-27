+++
title = "Rosetta Code:Village Pump/"
description = ""
date = 2018-04-21T14:57:34Z
aliases = []
[extra]
id = 17102
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=i have a doubt based on c loops programming (im beginner) 
|summary= in do while loop why we assign (-1) to count in the below programming 
}}
#include <stdio.h>
#include <ctype.h>
#define EOL  '\n'
  main()
{
   char letter[80];
   int tag, count =-1;
   do ++count;
   while ((letter[count]=getchar()) != EOL);
   tag = count;
   count=0;
   do{
    putchar(toupper(letter[count]));
    ++count;
   }
    while(count<tag);

}


in do while loop why we assign (-1) to count in the below programming.... could you assist me to overcome this problem..

---------

May I first say that the purpose of this site is for people to complete programming tasks in languages which (hopefully) they know quite well, so we can compare how different languages tackle such tasks. 

It is not intended as a site for asking general programming questions so this answer should be regarded as a 'one off'.

The purpose of the first do/while loop is to get characters from the input stream and add them to the buffer until the EOL character ('\n' or 10) is reached. As array indexing starts at 0 in the C language and 'count' is incremented (using the prefix ++ operator) each time around the loop, it needs to be assigned a value of -1 initially so it will have a value of 0 by the time the while condition is first reached.

Note that the EOL character itself is added to the buffer.

The second do/while loop iterates through the characters in the buffer, converts them to upper case and then outputs them. 'count' is again used to index the characters in the buffer but this time the loop stops just before 'tag' is reached so the final EOL character will not be output.
