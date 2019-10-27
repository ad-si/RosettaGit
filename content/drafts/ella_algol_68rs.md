+++
title = "ELLA ALGOL 68RS"
description = ""
date = 2015-09-17T13:06:38Z
aliases = []
[extra]
id = 2371
[taxonomies]
categories = []
tags = []
+++

{{implementation|ALGOL 68}}
{{wikipedia|ELLA}}
{{stub}}
'''ELLA''' is a hardware design language from [[wp:Defence_Research_Agency|DRA Malvern]]. Implemented in [[ALGOL 68RS]] - "ELLA 2000: A Language for Electronic System Design", J.D. Morison and A.S. Clarke, McGraw-Hill 1993.

algol68toc is derived from a project to translate ELLA into [[C]], hence ''ELLA ALGOL 68'' is a cut down verion of [[ALGOL 68RS]] where elements not required for the ELLA translation were not ported.
==Sample==
Notes: ELLA Algol68 (in the form on Algol68toC) is missing the formatted transput (input/output), hence the following code sample uses ''print'' and not ''printf''.  It also requires ALGOL 68 RS's PROGRAM, USE and FINISH formalities.

```txt
PROGRAM helloworld CONTEXT VOID
USE standard    
BEGIN      
  print(("Hello, world!", new line))      
END
FINISH

```


==See Also==
* Ports of the ELLA A68ToC compiler:
# Download for [http://www.poenikatu.co.uk/src/algol68toc_1.19_i386.deb debian] - version 1.19
# Download for [https://sourceforge.net/project/showfiles.php?group_id=114223&package_id=301885&release_id=645539# Fedora 9] (SourceForge) - version 1.8.8d.
The SourceForge download includes the PDFs of two books:
#Programming Algol 68 Made Easy - by Sian Mountbatten - 610 pages (also in the algol68toc package).
#The RS Compiler for ALGOL 68 - Published 1978 - Defence Research Agency, Malvern, UK - 86 pages.
