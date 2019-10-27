+++
title = "Talk:100 doors/XSLT"
description = ""
date = 2010-12-03T22:50:26Z
aliases = []
[extra]
id = 8926
[taxonomies]
categories = []
tags = []
+++

== not really complete? ==

When I run this, I get a blank result.

Specifically:

I changed the prolog to:


```xslt
<?xml version='1.0'?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                version="1.0"
                result-ns="http://www.w3.org/TR/REC-html40">

  <xsl:output method="html"
              version="4.0"
              doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN"
              doctype-system="http://www.w3.org/TR/REC-html40/loose.dtd"
              encoding="ISO-8859-1"/>

  <xsl:strip-space elements="*"/>


  <xsl:template name="HundredDoors" match="/"
```


And I added a suffix:


```xslt

</xsl:stylesheet>
```


And I created an empty html document.  <code><html></html></code>

And, I used unicorn's "[http://www.unicorn-enterprises.com/products_uxt.html standard edition]" xslt processor from cygwin with the empty html document as the first argument, the modified xsl file as the second and an arbitrary name as the third, and the resulting output file had 0 characters.
