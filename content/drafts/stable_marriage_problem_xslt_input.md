+++
title = "Stable marriage problem/XSLT input"
description = ""
date = 2012-10-28T14:54:08Z
aliases = []
[extra]
id = 12475
[taxonomies]
categories = []
tags = []
+++

The following document is the input to the XSLT 2.0 style-sheet for the [[Stable_marriage_problem#XSLT_2.0|Stable marriage problem]] problem.

<lang><t>
 <m:stable-marriage-problem xmlns:m="http://rosettacode.org/wiki/Stable_marriage_problem">
   <m:dude name="abe">
     <m:interest>abi</m:interest>
     <m:interest>eve</m:interest>
     <m:interest>cath</m:interest>
     <m:interest>ivy</m:interest>
     <m:interest>jan</m:interest>
     <m:interest>dee</m:interest>
     <m:interest>fay</m:interest>
     <m:interest>bea</m:interest>
     <m:interest>hope</m:interest>
     <m:interest>gay</m:interest>
   </m:dude>
   <m:dude name="bob">
     <m:interest>cath</m:interest>
     <m:interest>hope</m:interest>
     <m:interest>abi</m:interest>
     <m:interest>dee</m:interest>
     <m:interest>eve</m:interest>
     <m:interest>fat</m:interest>
     <m:interest>bea</m:interest>
     <m:interest>jan</m:interest>
     <m:interest>ivy</m:interest>
     <m:interest>gay</m:interest>
   </m:dude>
   <m:dude name="col">
     <m:interest>hope</m:interest>
     <m:interest>eve</m:interest>
     <m:interest>abi</m:interest>
     <m:interest>dee</m:interest>
     <m:interest>bea</m:interest>
     <m:interest>fay</m:interest>
     <m:interest>ivy</m:interest>
     <m:interest>gay</m:interest>
     <m:interest>cath</m:interest>
     <m:interest>jan</m:interest>
   </m:dude>
   <m:dude name="dan">
     <m:interest>ivy</m:interest>
     <m:interest>fay</m:interest>
     <m:interest>dee</m:interest>
     <m:interest>gay</m:interest>
     <m:interest>hope</m:interest>
     <m:interest>eve</m:interest>
     <m:interest>jan</m:interest>
     <m:interest>bea</m:interest>
     <m:interest>cath</m:interest>
     <m:interest>abi</m:interest>
   </m:dude>
   <m:dude name="ed">
     <m:interest>jan</m:interest>
     <m:interest>dee</m:interest>
     <m:interest>bea</m:interest>
     <m:interest>cath</m:interest>
     <m:interest>fay</m:interest>
     <m:interest>eve</m:interest>
     <m:interest>abi</m:interest>
     <m:interest>ivy</m:interest>
     <m:interest>hope</m:interest>
     <m:interest>gay</m:interest>
   </m:dude>
   <m:dude name="fred">
     <m:interest>bea</m:interest>
     <m:interest>abi</m:interest>
     <m:interest>dee</m:interest>
     <m:interest>gay</m:interest>
     <m:interest>eve</m:interest>
     <m:interest>ivy</m:interest>
     <m:interest>cath</m:interest>
     <m:interest>jan</m:interest>
     <m:interest>hope</m:interest>
     <m:interest>fay</m:interest>
   </m:dude>
   <m:dude name="gav">
     <m:interest>gay</m:interest>
     <m:interest>eve</m:interest>
     <m:interest>ivy</m:interest>
     <m:interest>bea</m:interest>
     <m:interest>cath</m:interest>
     <m:interest>abi</m:interest>
     <m:interest>dee</m:interest>
     <m:interest>hope</m:interest>
     <m:interest>jan</m:interest>
     <m:interest>fay</m:interest>
   </m:dude>
   <m:dude name="hal">
     <m:interest>abi</m:interest>
     <m:interest>eve</m:interest>
     <m:interest>hope</m:interest>
     <m:interest>fay</m:interest>
     <m:interest>ivy</m:interest>
     <m:interest>cath</m:interest>
     <m:interest>jan</m:interest>
     <m:interest>bea</m:interest>
     <m:interest>gay</m:interest>
     <m:interest>dee</m:interest>
   </m:dude>
   <m:dude name="ian">
     <m:interest>hope</m:interest>
     <m:interest>cath</m:interest>
     <m:interest>dee</m:interest>
     <m:interest>gay</m:interest>
     <m:interest>bea</m:interest>
     <m:interest>abi</m:interest>
     <m:interest>fay</m:interest>
     <m:interest>ivy</m:interest>
     <m:interest>jan</m:interest>
     <m:interest>eve</m:interest>
   </m:dude>
   <m:dude name="jon">
     <m:interest>abi</m:interest>
     <m:interest>fay</m:interest>
     <m:interest>jan</m:interest>
     <m:interest>gay</m:interest>
     <m:interest>eve</m:interest>
     <m:interest>bea</m:interest>
     <m:interest>dee</m:interest>
     <m:interest>cath</m:interest>
     <m:interest>ivy</m:interest>
     <m:interest>hope</m:interest>
   </m:dude>
   
   <m:maid name="abi">
     <m:interest>bob</m:interest>
     <m:interest>fred</m:interest>
     <m:interest>jon</m:interest>
     <m:interest>gav</m:interest>
     <m:interest>ian</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>dan</m:interest>
     <m:interest>ed</m:interest>
     <m:interest>col</m:interest>
     <m:interest>hal</m:interest>
   </m:maid>
   <m:maid name="bea">
     <m:interest>bob</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>col</m:interest>
     <m:interest>fred</m:interest>
     <m:interest>gav</m:interest>
     <m:interest>dan</m:interest>
     <m:interest>ian</m:interest>
     <m:interest>ed</m:interest>
     <m:interest>jon</m:interest>
     <m:interest>hal</m:interest>
   </m:maid>
   <m:maid name="cath">
     <m:interest>fred</m:interest>
     <m:interest>bob</m:interest>
     <m:interest>ed</m:interest>
     <m:interest>gav</m:interest>
     <m:interest>hal</m:interest>
     <m:interest>col</m:interest>
     <m:interest>ian</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>dan</m:interest>
     <m:interest>jon</m:interest>
   </m:maid>
   <m:maid name="dee">
     <m:interest>fred</m:interest>
     <m:interest>jon</m:interest>
     <m:interest>col</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>ian</m:interest>
     <m:interest>hal</m:interest>
     <m:interest>gav</m:interest>
     <m:interest>dan</m:interest>
     <m:interest>bob</m:interest>
     <m:interest>ed</m:interest>
   </m:maid>
   <m:maid name="eve">
     <m:interest>jon</m:interest>
     <m:interest>hal</m:interest>
     <m:interest>fred</m:interest>
     <m:interest>dan</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>gav</m:interest>
     <m:interest>col</m:interest>
     <m:interest>ed</m:interest>
     <m:interest>ian</m:interest>
     <m:interest>bob</m:interest>
   </m:maid>
   <m:maid name="fay">
     <m:interest>bob</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>ed</m:interest>
     <m:interest>ian</m:interest>
     <m:interest>jon</m:interest>
     <m:interest>dan</m:interest>
     <m:interest>fred</m:interest>
     <m:interest>gave</m:interest>
     <m:interest>col</m:interest>
     <m:interest>hal</m:interest>
   </m:maid>
   <m:maid name="gay">
     <m:interest>jon</m:interest>
     <m:interest>gav</m:interest>
     <m:interest>hal</m:interest>
     <m:interest>fred</m:interest>
     <m:interest>bob</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>col</m:interest>
     <m:interest>ed</m:interest>
     <m:interest>dan</m:interest>
     <m:interest>ian</m:interest>
   </m:maid>
   <m:maid name="hope">
     <m:interest>gav</m:interest>
     <m:interest>jon</m:interest>
     <m:interest>bob</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>ian</m:interest>
     <m:interest>dan</m:interest>
     <m:interest>hal</m:interest>
     <m:interest>ed</m:interest>
     <m:interest>col</m:interest>
     <m:interest>fred</m:interest>
   </m:maid>
   <m:maid name="ivy">
     <m:interest>ian</m:interest>
     <m:interest>col</m:interest>
     <m:interest>hal</m:interest>
     <m:interest>gav</m:interest>
     <m:interest>fred</m:interest>
     <m:interest>bob</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>ed</m:interest>
     <m:interest>jon</m:interest>
     <m:interest>dan</m:interest>
   </m:maid>
   <m:maid name="jan">
     <m:interest>ed</m:interest>
     <m:interest>hal</m:interest>
     <m:interest>gav</m:interest>
     <m:interest>abe</m:interest>
     <m:interest>bob</m:interest>
     <m:interest>jon</m:interest>
     <m:interest>col</m:interest>
     <m:interest>ian</m:interest>
     <m:interest>fred</m:interest>
     <m:interest>dan</m:interest>
   </m:maid>
 </m:stable-marriage-problem>
</t>
  
```

