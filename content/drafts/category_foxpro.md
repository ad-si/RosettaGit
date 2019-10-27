+++
title = "Category:FoxPro"
description = ""
date = 2011-04-29T09:55:57Z
aliases = []
[extra]
id = 9509
[taxonomies]
categories = []
tags = []
+++

SET DATE ansi
SET CENTURY on
for i = 2008 to 2121 
 date = CTOD( STR(i,4) + '.12.25' )
 if CDOW( date ) = 'Sunday'
  ? i
 ENDIF 
endfor
