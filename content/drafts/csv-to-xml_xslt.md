+++
title = "Csv-to-xml.xslt"
description = ""
date = 2012-10-26T12:06:14Z
aliases = []
[extra]
id = 12463
[taxonomies]
categories = []
tags = []
+++

Here is a general purpose library XSLT 2.0 style-sheet for converting CSV to XML. This is referenced in task [[CSV_to_HTML_translation]]


== csv-to-xml.xslt ==


```xml
<!DOCTYPE constants[
  <!ENTITY notice "The xcsv format was developed by Sean B. Durkin&#x85;www.seanbdurkin.id.au">
  ]>  
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:fn="http://www.w3.org/2005/xpath-functions" 
  xmlns:local="http://www.seanbdurkin.id.au/xslt/csv-to-xml.xslt" 
  xmlns:xs="http://www.w3.org/2001/XMLSchema" 
  xmlns:xcsv="http://www.seanbdurkin.id.au/xslt/xcsv.xsd"
  version="2.0"
  exclude-result-prefixes="xsl xs fn local">
<xsl:output indent="yes" encoding="UTF-8" method="xml"/>
<xsl:import-schema schema-location="http://www.seanbdurkin.id.au/xslt/xcsv.xsd"
                   use-when="system-property('xsl:is-schema-aware')='yes'" />
				   
<!-- Read Me ! -->
<!-- ************************************************************************</strong> -->
<!-- A significant part of this style-sheet is derived from or copied from     --> 
<!-- Andrew Welch's solution. Refer: http://andrewjwelch.com/code/xslt/csv/csv-to-xml_v2.html -->
<!-- Modifications have been made by me (Sean B. Durkin) in order to meet the design         -->
<!-- goals as described here:                                                                -->
<!-- http://pascaliburnus.seanbdurkin.id.au/index.php?/archives/17-A-Generalised-and-Compreh -->
<!--ensive-Solution-to-CSV-to-XML-and-XML-to-CSV-Transformations.html				         -->

<!-- Stylesheet parameters -->
<!-- ************************************************************************<strong> -->
<xsl:param name="url-of-csv" as="xs:string" />
<xsl:param name="lang" as="xs:string" select="'en'" />
<xsl:param name="url-of-messages" as="xs:string" />

<!-- Configurable constants -->
<!-- ************************************************************************</strong> -->
  <xsl:variable name="quote" as="xs:string">"</xsl:variable>
  <xsl:variable name="error-messages-i18n">
   <xcsv:error error-code="1">
    <xcsv:message xml:lang="en">Uncategorised error.</xcsv:message>
   </xcsv:error>
   <xcsv:error error-code="2">
    <xcsv:message xml:lang="en">Quoted value not terminated.</xcsv:message>
   </xcsv:error>
   <xcsv:error error-code="3">
    <xcsv:message xml:lang="en">Quoted value incorrectly terminated.</xcsv:message>
   </xcsv:error>
   <xcsv:error error-code="5">
    <xcsv:message xml:lang="en">Could not open CSV resource.</xcsv:message>
   </xcsv:error>
  </xsl:variable> 
  
<!-- Non-configurable constants -->
<!-- ************************************************************************<strong> -->
 <xsl:variable name="error-messages">
   <xsl:apply-templates select="$error-messages-i18n" mode="messages" />
  </xsl:variable>
    
<xsl:template match="@*|node()" mode="messages" >
 <xsl:copy>
  <xsl:apply-templates select="@*|node()" mode="messages" />
 </xsl:copy>
</xsl:template>

<xsl:template match="xcsv:message[
      not(@xml:lang=$lang) and
     (not(@xml:lang='en') or ../xcsv:message[@xml:lang=$lang])]" mode="messages" />
	 


<xsl:function name="local:unparsed-text-lines" as="xs:string+">
 <xsl:param name="href" as="xs:string" />
 <xsl:sequence use-when="function-available('unparsed-text-lines')" select="fn:unparsed-text-lines($href)" />
 <xsl:sequence use-when="not(function-available('unparsed-text-lines'))" select="tokenize(unparsed-text($href), '\r\n|\r|\n')[not(position()=last() and .='')]" />
</xsl:function>

<xsl:function name="local:error-node" as="node()">
 <xsl:param name="error-code" as="xs:integer" />
 <xsl:param name="data" as="xs:string" />
 <xcsv:error error-code="{$error-code}">
  <xcsv:message
    xml:lang="{$error-messages/xcsv:error[@error-code=$error-code]/xcsv:message/@xml:lang}">
    <xsl:value-of select="$error-messages/xcsv:error[@error-code=$error-code]/xcsv:message"/>
  </xcsv:message>
  <xcsv:error-data><xsl:value-of select="$data"/></xcsv:error-data>
 </xcsv:error>
</xsl:function>

<xsl:function name="local:csv-to-xml" as="node()+">
 <xsl:param name="href" as="xs:string" />
 <xcsv:comma-separated-single-line-values xcsv-version="1.0">
  <xcsv:notice xml:lang="en">&notice;</xcsv:notice>
  <xsl:choose>
   <xsl:when test="fn:unparsed-text-available($href)">
    <xsl:for-each select="local:unparsed-text-lines($href)">
	 <xcsv:row>
     <xsl:analyze-string select="fn:concat(., ',')" regex='(("[^"]*")+|[^,"]*),'>
      <xsl:matching-substring>
	   <xcsv:value>
	    <xsl:choose>
         <xsl:when test="fn:starts-with( fn:regex-group(1), $quote)">
          <xsl:value-of select='fn:replace(fn:regex-group(1), "^""|""$|("")""", "$1" )' />
	     </xsl:when>
	     <xsl:otherwise>
          <xsl:value-of select='regex-group(1)' />
	     </xsl:otherwise>
	    </xsl:choose>
	   </xcsv:value>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
       <xsl:copy-of select="local:error-node(3,.)"/>
       <!-- Quoted value incorrectly terminated. -->
      </xsl:non-matching-substring>
     </xsl:analyze-string>
	 </xcsv:row>
    </xsl:for-each>
   </xsl:when>
   <xsl:otherwise>
    <xsl:copy-of select="local:error-node(5,$href)"/>
    <!-- Could not open CSV resource. -->
   </xsl:otherwise>
  </xsl:choose>
 </xcsv:comma-separated-single-line-values>
</xsl:function>

<xsl:template match="/" name="local:main">
 <xsl:copy-of select="local:csv-to-xml($url-of-csv)" />
</xsl:template>


</xsl:stylesheet>
```

