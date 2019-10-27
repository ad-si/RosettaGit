+++
title = "XML Validation"
description = ""
date = 2019-10-09T10:18:31Z
aliases = []
[extra]
id = 18190
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

Given an XML document and an XSD schema definition validate that the document follows the schema described. 

Sample XML and XSD for tests can be found on [https://www.w3schools.com/xml/schema_example.asp this W3 Schools page].


## C

{{libheader|LibXML}}
At the time of writing, the XML and XSD files at the URLs used in the other examples were inaccessible. The files from the W3 Schools page were used for tests.

```C

#include <libxml/xmlschemastypes.h>

int main(int argC, char** argV)
{
	if (argC <= 2) {
		printf("Usage: %s <XML Document Name> <XSD Document Name>\n", argV[0]);
		return 0;
	}
	
	xmlDocPtr doc;
	xmlSchemaPtr schema = NULL;
	xmlSchemaParserCtxtPtr ctxt;
	char *XMLFileName = argV[1];
	char *XSDFileName = argV[2];
	int ret;

	xmlLineNumbersDefault(1);

	ctxt = xmlSchemaNewParserCtxt(XSDFileName);

	xmlSchemaSetParserErrors(ctxt, (xmlSchemaValidityErrorFunc) fprintf, (xmlSchemaValidityWarningFunc) fprintf, stderr);
	schema = xmlSchemaParse(ctxt);
	xmlSchemaFreeParserCtxt(ctxt);


	doc = xmlReadFile(XMLFileName, NULL, 0);

	if (doc == NULL){
		fprintf(stderr, "Could not parse %s\n", XMLFileName);
	}
	else{
		xmlSchemaValidCtxtPtr ctxt;

		ctxt = xmlSchemaNewValidCtxt(schema);
		xmlSchemaSetValidErrors(ctxt, (xmlSchemaValidityErrorFunc) fprintf, (xmlSchemaValidityWarningFunc) fprintf, stderr);
		ret = xmlSchemaValidateDoc(ctxt, doc);
		
		if (ret == 0){
			printf("%s validates\n", XMLFileName);
		}
		else if (ret > 0){
			printf("%s fails to validate\n", XMLFileName);
		}
		else{
			printf("%s validation generated an internal error\n", XMLFileName);
		}
		xmlSchemaFreeValidCtxt(ctxt);
		xmlFreeDoc(doc);
	}


	if(schema != NULL)
		xmlSchemaFree(schema);

	xmlSchemaCleanupTypes();
	xmlCleanupParser();
	xmlMemoryDump();

	return 0;
}

```

Output, files used from the W3 Schools page :

```txt

C:\rosettaCode>xmlValidator.exe shiporder.xml shiporder.xsd
shiporder.xml validates

```



## C sharp


```csharp

using System;
using System.Xml;
using System.Xml.Schema;
using System.IO;

public class Test
{
	public static void Main()
	{
		// your code goes here
		XmlSchemaSet sc = new XmlSchemaSet();
		sc.Add(null, "http://venus.eas.asu.edu/WSRepository/xml/Courses.xsd");
		XmlReaderSettings settings = new XmlReaderSettings();
		settings.ValidationType = ValidationType.Schema;
		settings.Schemas = sc;
		settings.ValidationEventHandler += new ValidationEventHandler(ValidationCallBack);
		// Create the XmlReader object.
		XmlReader reader = XmlReader.Create("http://venus.eas.asu.edu/WSRepository/xml/Courses.xml", settings);
		// Parse the file.
		while (reader.Read()); 
		// will call event handler if invalid
		Console.WriteLine("The XML file is valid for the given xsd file");
	}
	
	// Display any validation errors.
	private static void ValidationCallBack(object sender, ValidationEventArgs e) {
		Console.WriteLine("Validation Error: {0}", e.Message);
	}
}

```


=={{header|F_Sharp|F#}}==
<p>Using an inline stylesheet:</p>

```fsharp
open System.Xml
open System.Xml.Schema
open System.IO

let xml = @"<root>
<!--Start of schema-->
<xs:schema id='an-element' targetNamespace='example' xmlns:mstns='example' xmlns='example' xmlns:xs='http://www.w3.org/2001/XMLSchema' attributeFormDefault='unqualified' elementFormDefault='qualified'>
  <xs:element name='an-element'>
    <xs:complexType>
      <xs:sequence minOccurs='0' maxOccurs='unbounded'>
        <xs:element name='another-element' nillable='true'>
          <xs:complexType>
            <xs:simpleContent>
              <xs:extension base='xs:string'>
                <xs:attribute name='an-attribute' form='unqualified' type='xs:boolean' />
              </xs:extension>
            </xs:simpleContent>
          </xs:complexType>
        </xs:element>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>
<!--End of schema-->
<an-element xmlns='example'>
    <another-element an-attribute='false'>...</another-element>
    <another-element an-attribute='wrong'>123</another-element>
</an-element>
</root>"

let validationData withWarnings =
    let errors = ref 0
    let warnings = ref 0
    fun input ->
        match input with
        | Some(msg, severity) ->
            if severity = XmlSeverityType.Error then
                errors := !errors + 1
                printfn "Validation error: %s" msg
            elif withWarnings then
                warnings := !warnings + 1
                printfn "Validation warning: %s" msg
            None
        | None ->
            if withWarnings then
                Some(dict[XmlSeverityType.Error, !errors; XmlSeverityType.Warning, !warnings])
            else
                Some(dict[XmlSeverityType.Error, !errors])

[<EntryPoint>]
let main argv =
    let withWarnings = argv.Length > 0 && argv.[0] = "-w"
    let vData = validationData withWarnings
    let validationEvent = new ValidationEventHandler(fun _ e ->
        vData (Some(e.Message, e.Severity)) |> ignore)
    let settings = new XmlReaderSettings()
    settings.ValidationType <- ValidationType.Schema
    settings.ValidationEventHandler.AddHandler(validationEvent)
    settings.ValidationFlags <- settings.ValidationFlags ||| XmlSchemaValidationFlags.ProcessInlineSchema ||| XmlSchemaValidationFlags.ReportValidationWarnings
    let reader = XmlReader.Create(new StringReader(xml), settings);
    while reader.Read() do ()
    printfn "%A" (Seq.toList (vData None).Value)
    0

```

{{out}}
<pre style="white-space:pre-wrap">>RosettaCode
Validation error: The 'an-attribute' element is invalid - The value 'wrong' is invalid according to its datatype 'http://www.w3.org/2001/XMLSchema:boolean' - The string 'wrong' is not a valid boolean value.
[[Error, 1]]

>RosettaCode -w
Validation warning: Could not find schema information for the element 'root'.
Validation error: The 'an-attribute' element is invalid - The value 'wrong' is invalid according to its datatype 'http://www.w3.org/2001/XMLSchema:boolean' - The string 'wrong' is not a valid boolean value.
[[Error, 1]; [Warning, 1]]


```

<p>Changing <code>wrong</code> to a boolean, e. g. <code>true</code>, The result (without -w) is 
```txt
[[Error, 0]]
```



## Go

{{libheader|libxml2(Go)}}


This uses the w3schools test data linked to above.

```go
package main

import (
    "fmt"
    "github.com/lestrrat-go/libxml2"
    "github.com/lestrrat-go/libxml2/xsd"
    "io/ioutil"
    "log"
    "os"
)

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    xsdfile := "shiporder.xsd"
    f, err := os.Open(xsdfile)
    check(err)
    defer f.Close()

    buf, err := ioutil.ReadAll(f)
    check(err)

    s, err := xsd.Parse(buf)
    check(err)
    defer s.Free()

    xmlfile := "shiporder.xml"
    f2, err := os.Open(xmlfile)
    check(err)
    defer f2.Close()

    buf2, err := ioutil.ReadAll(f2)
    check(err)

    d, err := libxml2.Parse(buf2)
    check(err)

    if err := s.Validate(d); err != nil {
        for _, e := range err.(xsd.SchemaValidationError).Errors() {
            log.Printf("error: %s", e.Error())
        }
        return
    }

    fmt.Println("Validation of", xmlfile, "against", xsdfile, "successful!")
}
```


{{out}}

```txt

Validation of shiporder.xml against shiporder.xsd successful!

```



## Groovy

{{trans|Java}}

Solution:

```groovy
import static javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import org.xml.sax.SAXParseException

def factory = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI)
def validate = { schemaURL, docURL ->
    try {
        factory.newSchema(schemaURL.toURL()).newValidator().validate(new StreamSource(docURL))
        true
    } catch (SAXParseException e) {
        false
    }
}
```


Test:

```groovy
def schemaLoc = "http://venus.eas.asu.edu/WSRepository/xml/Courses.xsd"
def docLoc = "http://venus.eas.asu.edu/WSRepository/xml/Courses.xml"
println "Document is ${validate(schemaLoc, docLoc)? 'valid' : 'invalid'}"
```



## Java


```java
import static javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import javax.xml.ws.Holder;

import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class XmlValidation {
	public static void main(String... args) throws MalformedURLException {
		URL schemaLocation = new URL("http://venus.eas.asu.edu/WSRepository/xml/Courses.xsd");
		URL documentLocation = new URL("http://venus.eas.asu.edu/WSRepository/xml/Courses.xml");
		if (validate(schemaLocation, documentLocation)) {
			System.out.println("document is valid");
		} else {
			System.out.println("document is invalid");
		}
	}

	// The least code you need for validation
	public static boolean minimalValidate(URL schemaLocation, URL documentLocation) {
		SchemaFactory factory = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI);
		try {
			Validator validator = factory.newSchema(schemaLocation).newValidator();
			validator.validate(new StreamSource(documentLocation.toString()));
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	// A more complete validator
	public static boolean validate(URL schemaLocation, URL documentLocation) {
		SchemaFactory factory = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI);
		final Holder<Boolean> valid = new Holder<>(true);
		try {
			Validator validator = factory.newSchema(schemaLocation).newValidator();
			// Get some better diagnostics out
			validator.setErrorHandler(new ErrorHandler(){
				@Override
				public void warning(SAXParseException exception) {
					System.out.println("warning: " + exception.getMessage());
				}

				@Override
				public void error(SAXParseException exception) {
					System.out.println("error: " + exception.getMessage());
					valid.value = false;
				}

				@Override
				public void fatalError(SAXParseException exception) throws SAXException {
					System.out.println("fatal error: " + exception.getMessage());
					throw exception;
				}});
			validator.validate(new StreamSource(documentLocation.toString()));
			return valid.value;
		} catch (SAXException e) {
			// Already reported above
			return false;
		} catch (Exception e) {
			// If this is the only thing that throws, it's a gross error
			System.err.println(e);
			return false;
		}
	}
}
```



## Kotlin

{{trans|C}}
{{libheader|libxml2}}
{{works with|Ubuntu 14.04}}
Assuming that libxml2 is already installed on your system in the default location(s), you first need to build libxml_schemas.klib using the following .def file and the cinterop tool:

```txt

// libxml_schemas.def
headers = /usr/include/libxml2/libxml/xmlschemastypes.h
compilerOpts = -I/usr/include/libxml2
linkerOpts = -L/usr/lib/x86_64-linux-gnu -lxml2

```

Next, you need to compile the following Kotlin program, linking against libxml_schemas.klib. 

```scala
// Kotlin Native v0.6

import kotlinx.cinterop.*
import platform.posix.*
import libxml_schemas.*

fun err(ctx: COpaquePointer?, msg: CPointer<ByteVar>?, extra: CPointer<ByteVar>?) {
    val fp = ctx?.reinterpret<FILE>()
    fprintf(fp, msg?.toKString(), extra?.toKString())
}

fun warn(ctx: COpaquePointer?, msg: CPointer<ByteVar>?, extra: CPointer<ByteVar>?) {
    err(ctx, msg, extra)
}

fun main(args: Array<String>) {
    if (args.size != 2) {
        println("You need to pass exactly 2 command line arguments, namely:")
        println("    <XML Document Name> <XSD Document Name>")
        return
    }

    val xmlFileName = args[0]
    val xsdFileName = args[1]
    xmlLineNumbersDefault(1)
    val ctxt = xmlSchemaNewParserCtxt(xsdFileName)

    xmlSchemaSetParserErrors(
        ctxt,
        staticCFunction(::err) as xmlSchemaValidityErrorFunc?,
        staticCFunction(::warn) as xmlSchemaValidityWarningFunc?,
        stderr
    )

    val schema = xmlSchemaParse(ctxt)
    xmlSchemaFreeParserCtxt(ctxt)

    val doc = xmlReadFile(xmlFileName, null, 0)
    if (doc == null) {
        println("Could not parse $xmlFileName")
    }
    else {
        val ctxt2 = xmlSchemaNewValidCtxt(schema)

        xmlSchemaSetValidErrors(
            ctxt2,
            staticCFunction(::err) as xmlSchemaValidityErrorFunc?,
            staticCFunction(::warn) as xmlSchemaValidityWarningFunc?,
            stderr
        )

        val ret = xmlSchemaValidateDoc(ctxt2, doc)
        if (ret == 0)
            println("$xmlFileName validates")
        else if (ret > 0)
            println("$xmlFileName fails to validate")
        else
            println("$xmlFileName generated an internal error")
        xmlSchemaFreeValidCtxt(ctxt2)
        xmlFreeDoc(doc)
    }

    if (schema != null) xmlSchemaFree(schema)
    xmlSchemaCleanupTypes()
    xmlCleanupParser()
    xmlMemoryDump()
}
```

Finally, the resulting .kexe file should be executed passing it similar command line arguments to the C entry to produce the following output.

```txt

$ ./xmlval.kexe shiporder.xml shiporder.xsd
shiporder.xml validates

```



## Perl



```perl
#!/usr/bin/env perl -T
use 5.018_002;
use warnings;
use Try::Tiny;
use XML::LibXML;

our $VERSION = 1.000_000;

my $parser = XML::LibXML->new();

my $good_xml         = '<a>5</a>';
my $bad_xml          = '<a>5<b>foobar</b></a>';
my $xmlschema_markup = <<'END';
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <xsd:element name="a" type="xsd:integer"/>
</xsd:schema>
END

my $xmlschema = XML::LibXML::Schema->new( string => $xmlschema_markup );

for ( $good_xml, $bad_xml ) {
    my $doc = $parser->parse_string($_);
    try {
        $xmlschema->validate($doc);
    }
    finally {
        if (@_) {
            say "Not valid: @_";
        }
        else {
            say 'Valid';
        }
    };
}
```


{{out}}

```txt
Valid
Not valid: unknown-7fe99976a9a0:0: Schemas validity error :
  Element 'a': Element content is not allowed, because the type definition is simple.
```



## Perl 6

{{trans|Perl}}

```perl6
#!/usr/bin/env perl6

# 20191009 Perl 6 programming solution

# Reference:
# https://github.com/p6-xml/LibXML-p6

use v6.d;
use LibXML;
use LibXML::Schema;

my $good_xml         = '<a>5</a>';
my $bad_xml          = '<a>5<b>foobar</b></a>';

my $xsdschema = q:to<EOF>;
   <xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">
      <xsd:element name="a" type="xsd:integer"/>
   </xsd:schema>
EOF

my LibXML $p .= new();

for ( $good_xml, $bad_xml ) {
   my $x = $p.parse: :string($_);
   try { LibXML::Schema.new( string => $xsdschema ).validate( $x ) }
   !$! ?? say "Valid." !! say $!.message() ;
}
```

{{out}}

```txt
Valid.
Schemas validity error : Element 'a': Element content is not allowed, because the type definition is simple.

```



## Python



```python
#!/bin/python
from __future__ import print_function
import lxml
from lxml import etree

if __name__=="__main__":

	parser = etree.XMLParser(dtd_validation=True)
	schema_root = etree.XML('''\
		<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">
			<xsd:element name="a" type="xsd:integer"/>
		</xsd:schema>
		''')
	schema = etree.XMLSchema(schema_root)

	#Good xml
	parser = etree.XMLParser(schema = schema)
	try:
		root = etree.fromstring("<a>5</a>", parser)
		print ("Finished validating good xml")
	except lxml.etree.XMLSyntaxError as err:
		print (err)

	#Bad xml
	parser = etree.XMLParser(schema = schema)
	try:
		root = etree.fromstring("<a>5<b>foobar</b></a>", parser)
	except lxml.etree.XMLSyntaxError as err:
		print (err)
```


{{out}}

```txt
Finished validating good xml
Element 'a': Element content is not allowed, because the type definition is simple.
```



## Scala

  
```Scala
import java.net.URL

import javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI
import javax.xml.transform.stream.StreamSource
import javax.xml.validation.SchemaFactory
import javax.xml.ws.Holder
import org.xml.sax.{ErrorHandler, SAXException, SAXParseException}

object XmlValidation extends App {
  val (schemaLocation, documentLocation) = (new URL("http://venus.eas.asu.edu/WSRepository/xml/Courses.xsd")
    , new URL("http://venus.eas.asu.edu/WSRepository/xml/Courses.xml"))

  println(s"Document is ${if (validate(schemaLocation, documentLocation)) "valid" else "invalid"}.")

  // A more complete validator
  def validate(schemaLocation: URL, documentLocation: URL): Boolean = {
    val factory = SchemaFactory.newInstance(W3C_XML_SCHEMA_NS_URI)
    val valid = new Holder[Boolean](true)
    try {
      val validator = factory.newSchema(schemaLocation).newValidator
      // Get some better diagnostics out
      validator.setErrorHandler(new ErrorHandler() {
        override def warning(exception: SAXParseException) = println("warning: " + exception.getMessage)

        override def error(exception: SAXParseException) = {
          println("error: " + exception.getMessage)
          valid.value = false
        }

        override def fatalError(exception: SAXParseException) = {
          println("fatal error: " + exception.getMessage)
          throw exception
        }
      })
      validator.validate(new StreamSource(documentLocation.toString))
      valid.value
    } catch {
      case _: SAXException =>
        // Already reported above
        false
      case e: Exception =>
        // If this is the only thing that throws, it's a gross error
        println(e)
        false
    }
  }
}
```


## Sidef

{{trans|Perl}}

```ruby
require('XML::LibXML')

func is_valid_xml(str, schema) {

    var parser    = %O<XML::LibXML>.new
    var xmlschema = %O<XML::LibXML::Schema>.new(string => schema)

    try {
        xmlschema.validate(parser.parse_string(str))
        true
    } catch {
        false
    }
}

var good_xml = '<a>5</a>'
var bad_xml  = '<a>5<b>foobar</b></a>'

var xmlschema_markup = <<'END'
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <xsd:element name="a" type="xsd:integer"/>
</xsd:schema>
END

[good_xml, bad_xml].each { |xml|
    say "is_valid_xml(#{xml.dump}) : #{is_valid_xml(xml, xmlschema_markup)}"
}
```

{{out}}

```txt

is_valid_xml("<a>5</a>") : true
is_valid_xml("<a>5<b>foobar</b></a>") : false

```



## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >= 14, e.g. with Visual Studio 2015)
{{works with|.NET Core|2.1}}


```vbnet
Option Compare Binary
Option Explicit On
Option Infer On
Option Strict On

Imports System.Xml
Imports System.Xml.Schema

Module Program
    Function GetValidationErrors(doc As XDocument, schemaSet As XmlSchemaSet) As IList(Of ValidationEventArgs)
        GetValidationErrors = New List(Of ValidationEventArgs)
        doc.Validate(schemaSet, Sub(sender, e) GetValidationErrors.Add(e))
    End Function

    Sub Main()
        ' These functions are declared in another module found below.
        Dim schema = GetSchema()
        Dim document = GetDocument()

        Dim schemaSet As New XmlSchemaSet()
        schemaSet.Add(XmlSchema.Read(schema.CreateReader(), Nothing))

        Dim errors = GetValidationErrors(document, schemaSet)
        For Each e In errors
            Console.WriteLine($"Validation {e.Severity}:{vbCrLf}{e.Message}")
        Next

        If errors.Count = 0 Then Console.WriteLine("The document is valid.")
    End Sub
End Module
```


{{out}}

```txt
Validation Error:
The 'an-attribute' attribute is invalid - The value 'wrong' is invalid according to its datatype 'http://www.w3.org/2001/XMLSchema:boolean' - The string 'wrong' is not a valid Boolean value.
```


An alternative is to use XmlReader (like the C# and F# examples [as of 2019-08-06]).


```vbnet
    Function GetValidationErrorsXmlReader(doc As XDocument, schemaSet As XmlSchemaSet, warnings As Boolean) As IList(Of ValidationEventArgs)
        GetValidationErrorsReader = New List(Of ValidationEventArgs)

        Dim settings As New XmlReaderSettings()
        With settings
            .ValidationType = ValidationType.Schema
            .Schemas = schemaSet
            If warnings Then .ValidationFlags = .ValidationFlags Or XmlSchemaValidationFlags.ReportValidationWarnings
        End With

        AddHandler settings.ValidationEventHandler, Sub(sender, e) GetValidationErrorsReader.Add(e)

        Using reader = XmlReader.Create(doc.CreateReader(), settings)
            Do While reader.Read() : Loop
        End Using
    End Function
```


Creating the documents (same as F#) from strings (does not handle syntax errors):


```vbnet
Module Constants
    Const SCHEMA As String =
"<?xml version='1.0'?>
<xs:schema id='an-element' targetNamespace='example' xmlns:mstns='example' xmlns='example' xmlns:xs='http://www.w3.org/2001/XMLSchema' attributeFormDefault='unqualified' elementFormDefault='qualified'>
    <xs:element name='an-element'>
        <xs:complexType>
            <xs:sequence minOccurs='0' maxOccurs='unbounded'>
                <xs:element name='another-element' nillable='true'>
                    <xs:complexType>
                        <xs:simpleContent>
                            <xs:extension base='xs:string'>
                                <xs:attribute name='an-attribute' form='unqualified' type='xs:boolean'/>
                            </xs:extension>
                        </xs:simpleContent>
                    </xs:complexType>
                </xs:element>
            </xs:sequence>
        </xs:complexType>
    </xs:element>
</xs:schema>"

    Const DOCUMENT As String =
"<?xml version='1.0'?>
<an-element xmlns='example'>
    <another-element an-attribute='false'>...</another-element>
    <another-element an-attribute='wrong'>123</another-element>
</an-element>"

    Function GetSchema() As XDocument
        Return XDocument.Parse(SCHEMA)
    End Function

    Function GetDocument() As XDocument
        Return XDocument.Parse(DOCUMENT)
    End Function
End Module
```


Alternatively, we can be cheeky and use VB's XML literals...


```vbnet
Module Constants
    Function GetDocument() As XDocument
        Return _
        <?xml version="1.0"?>
        <an-element xmlns="example">
            <another-element an-attribute="false">...</another-element>
            <another-element an-attribute="wrong"> 123</another-element>
        </an-element>
    End Function

    Function GetSchema() As XDocument
        Return _
        <?xml version="1.0"?>
        <xs:schema id="an-element" targetNamespace="example" xmlns:mstns="example" xmlns="example" xmlns:xs="http://www.w3.org/2001/XMLSchema" attributeFormDefault="unqualified" elementFormDefault="qualified">
            <xs:element name="an-element">
                <xs:complexType>
                    <xs:sequence minOccurs="0" maxOccurs="unbounded">
                        <xs:element name="another-element" nillable="true">
                            <xs:complexType>
                                <xs:simpleContent>
                                    <xs:extension base="xs:string">
                                        <xs:attribute name="an-attribute" form="unqualified" type="xs:boolean"/>
                                    </xs:extension>
                                </xs:simpleContent>
                            </xs:complexType>
                        </xs:element>
                    </xs:sequence>
                </xs:complexType>
            </xs:element>
        </xs:schema>
    End Function
End Module
```

