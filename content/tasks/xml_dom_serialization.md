+++
title = "XML/DOM serialization"
description = ""
date = 2019-09-07T22:28:31Z
aliases = []
[extra]
id = 1760
[taxonomies]
categories = ["task", "XML"]
tags = []
+++

## Task

Create a simple DOM and having it serialize to:

  <?xml version="1.0" ?>
  <root>
      <element>
          Some text here
      </element>
  </root>

## ABAP


```ABAP

DATA: xml_string TYPE string.

DATA(xml)  = cl_ixml=>create( ).
DATA(doc)  = xml->create_document( ).
DATA(root) = doc->create_simple_element( name   = 'root'
                                         parent = doc ).

doc->create_simple_element( name   = 'element'
                            parent = root
                            value  = 'Some text here' ).

DATA(stream_factory) = xml->create_stream_factory( ).
DATA(stream)         = stream_factory->create_ostream_cstring( string = xml_string ).
DATA(renderer)       = xml->create_renderer( document = doc
                                             ostream  = stream ).
stream->set_pretty_print( abap_true ).
renderer->render( ).

cl_demo_output=>display_text( xml_string ).

```


Output:

```txt
<?xml version="1.0" encoding="utf-16"?>
<root>
<element>Some text here</element>
</root>
```



## Ada

Uses [http://libre.adacore.com/libre/tools/xmlada/ XML/Ada] from AdaCore.


```Ada
with Ada.Text_IO.Text_Streams;
with DOM.Core.Documents;
with DOM.Core.Nodes;

procedure Serialization is
   My_Implementation : DOM.Core.DOM_Implementation;
   My_Document       : DOM.Core.Document;
   My_Root_Node      : DOM.Core.Element;
   My_Element_Node   : DOM.Core.Element;
   My_Text_Node      : DOM.Core.Text;
begin
   My_Document := DOM.Core.Create_Document (My_Implementation);
   My_Root_Node := DOM.Core.Documents.Create_Element (My_Document, "root");
   My_Root_Node := DOM.Core.Nodes.Append_Child (My_Document, My_Root_Node);
   My_Element_Node := DOM.Core.Documents.Create_Element (My_Document, "element");
   My_Element_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Element_Node);
   My_Text_Node := DOM.Core.Documents.Create_Text_Node (My_Document, "Some text here");
   My_Text_Node := DOM.Core.Nodes.Append_Child (My_Element_Node, My_Text_Node);
   DOM.Core.Nodes.Write
     (Stream => Ada.Text_IO.Text_Streams.Stream
        (Ada.Text_IO.Standard_Output),
      N => My_Document,
      Pretty_Print => True);
end Serialization;
```


It can be shortened a lot by adding "use" clauses and writing the creation functions into the declaration part.

Output:

```txt
<?xml version="1.0" encoding="utf-8"?>
<root>
 <element>Some text here</element>
</root>
```


## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program createXml.s   */
/* install package   libxml++2.6-dev    */
/* link with gcc option -lxml2    */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessEndpgm:      .asciz "Normal end of program.\n"
szFileName:        .asciz "file1.xml"
szFileMode:        .asciz "w"
szMessError:       .asciz "Error detected !!!!. \n"

szVersDoc:         .asciz "1.0"
szLibRoot:         .asciz "root"
szLibElement:      .asciz "element"
szText:            .asciz "some text here"
szCarriageReturn:  .asciz "\n"
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
.align 4

/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                     @ entry of program
    ldr r0,iAdrszVersDoc
    bl xmlNewDoc                          @ create doc
    mov r9,r0                             @ doc address
    mov r0,#0
    ldr r1,iAdrszLibRoot
    bl xmlNewNode                         @ create root node
    mov r8,r0                             @ node root address
    mov r0,r9
    mov r1,r8
    bl xmlDocSetRootElement
@TODO voir la gestion des erreurs

    mov r0,#0
    ldr r1,iAdrszLibElement
    bl xmlNewNode                         @ create element node
    mov r7,r0                             @ node element address
    ldr r0,iAdrszText
    bl xmlNewText                         @ create text
    mov r6,r0                             @ text address
    mov r0,r7                             @ node element address
    mov r1,r6                             @ text address
    bl xmlAddChild                        @ add text to element node
    mov r0,r8                             @ node root address
    mov r1,r7                             @ node element address
    bl xmlAddChild                        @ add node elemeny to root node
    ldr r0,iAdrszFileName
    ldr r1,iAdrszFileMode
    bl fopen                              @ file open
    cmp r0,#0
    blt 99f
    mov r5,r0                             @ File descriptor
    mov r1,r9                             @ doc
    mov r2,r8                             @ root
    bl xmlElemDump                        @ write xml file
    cmp r0,#0
    blt 99f
    mov r0,r5
    bl fclose                             @ file close
    mov r0,r9
    bl xmlFreeDoc
    bl xmlCleanupParser
    ldr r0,iAdrszMessEndpgm
    bl affichageMess
    b 100f
99:
    @ error
    ldr r0,iAdrszMessError
    bl affichageMess
100:                                       @ standard end of the program
    mov r0, #0                             @ return code
    mov r7, #EXIT                          @ request to exit program
    svc #0                                 @ perform the system call

iAdrszMessError:          .int szMessError
iAdrszMessEndpgm:         .int szMessEndpgm
iAdrszVersDoc:            .int szVersDoc
iAdrszLibRoot:            .int szLibRoot
iAdrszLibElement:         .int szLibElement
iAdrszText:               .int szText
iAdrszFileName:           .int szFileName
iAdrszFileMode:           .int szFileMode
iAdrszCarriageReturn:     .int szCarriageReturn

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                   @ save  registres
    mov r2,#0                               @ counter length
1:                                          @ loop length calculation
    ldrb r1,[r0,r2]                         @ read octet start position + index
    cmp r1,#0                               @ if 0 its over
    addne r2,r2,#1                          @ else add 1 in the length
    bne 1b                                  @ and loop
                                            @ so here r2 contains the length of the message
    mov r1,r0                               @ address message in r1
    mov r0,#STDOUT                          @ code to write to the standard output Linux
    mov r7, #WRITE                          @ code call system "write"
    svc #0                                  @ call systeme
    pop {r0,r1,r2,r7,lr}                    @ restaur registers */
    bx lr                                   @ return
/******************************************************************/
/*     Converting a register to a decimal                                 */
/******************************************************************/
/* r0 contains value and r1 address area   */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                         @ save registers
    mov r3,r1
    mov r2,#LGZONECAL
1:                                          @ start loop
    bl divisionpar10                        @ r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                              @ digit
    strb r1,[r3,r2]                         @ store digit on area
    cmp r0,#0                               @ stop if quotient = 0
    subne r2,#1                               @ previous position
    bne 1b                                  @ else loop
                                            @ end replaces digit in front of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]                         @ store in area begin
    add r4,#1
    add r2,#1                               @ previous position
    cmp r2,#LGZONECAL                       @ end
    ble 2b                                  @ loop
    mov r1,#' '
3:
    strb r1,[r3,r4]
    add r4,#1
    cmp r4,#LGZONECAL                       @ end
    ble 3b
100:
    pop {r1-r4,lr}                          @ restaur registres
    bx lr                                   @return
/***************************************************/
/*   division par 10   signé                       */
/* Thanks to http://thinkingeek.com/arm-assembler-raspberry-pi/*
/* and   http://www.hackersdelight.org/            */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10:
  /* r0 contains the argument to be divided by 10 */
    push {r2-r4}                           @ save registers  */
    mov r4,r0
    mov r3,#0x6667                         @ r3 <- magic_number  lower
    movt r3,#0x6666                        @ r3 <- magic_number  upper
    smull r1, r2, r3, r0                   @ r1 <- Lower32Bits(r1*r0). r2 <- Upper32Bits(r1*r0)
    mov r2, r2, ASR #2                     @ r2 <- r2 >> 2
    mov r1, r0, LSR #31                    @ r1 <- r0 >> 31
    add r0, r2, r1                         @ r0 <- r2 + r1
    add r2,r0,r0, lsl #2                   @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                   @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2-r4}
    bx lr                                  @ return


```


## AutoHotkey


```AutoHotkey
version = "1.0"
xmlheader := "<?xml version=" . version . "?>" . "<" . root . ">"

element("root", "child")
element("root", "element", "more text here")
element("root_child", "kid", "yak yak")
MsgBox % xmlheader . serialize("root")
Return

element(parent, name, text="")
{
  Global
  %parent%_children .= name . "`n"
  %parent%_%name% = %parent%_%name%
  %parent%_%name%_name := name
  %parent%_%name%_text := text
}

serialize(root){
  StringSplit, root, root, _
  xml .= "<" . root%root0% . ">"
  StringTrimRight, %root%_children, %root%_children, 1
  Loop, Parse, %root%_children, `n
  {
    If %root%_%A_LoopField%_children
      xml .= serialize(%root%_%A_LoopField%)
    Else
    {
      element := "<" . %root%_%A_LoopField%_name . ">"
      element .= %root%_%A_LoopField%_text
      element .= "</" . %root%_%A_LoopField%_name . ">"
      xml .= element
    }
  }
  Return xml .= "</" . root%root0% . ">"
}
```



## Bracmat

To produce the XML including all indentations, we can do this:

```bracmat
(     ("?"."xml version=\"1.0\" ")
      \n
      (root.,"\n    " (element.,"
        Some text here
    ") \n)
  : ?xml
& out$(toML$!xml)
);
```

If we do not care about indentation:

```bracmat
(   ("?"."xml version=\"1.0\"") (root.,(element.,"Some text here"))
  : ?xml
& out$(toML$!xml)
);
```



## C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main()
{
  const char **next;
  int a;
  FILE *outFile;

  xmlDoc *doc = xmlNewDoc("1.0");
  xmlNode *root = xmlNewNode(NULL, "root");
  xmlDocSetRootElement(doc, root);

  xmlNode *node = xmlNewNode(NULL, "element");
  xmlAddChild(node, xmlNewText("some text here"));
  xmlAddChild(root, node);

  outFile = fopen("myfile.xml", "w");
  xmlElemDump(outFile, doc, root);
  fclose(outFile);

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return EXIT_SUCCESS;
}
```



## C#

Serialization using the built-in System.Xml.Serialization library of .Net.

```c#
using System.Xml;
using System.Xml.Serialization;
[XmlRoot("root")]
public class ExampleXML
{
    [XmlElement("element")]
    public string element = "Some text here";
    static void Main(string[] args)
    {
        var xmlnamespace = new XmlSerializerNamespaces();
        xmlnamespace.Add("", ""); //used to stop default namespaces from printing
        var writer = XmlWriter.Create("output.xml");
        new XmlSerializer(typeof(ExampleXML)).Serialize(writer, new ExampleXML(), xmlnamespace);
    }
    //Output: <?xml version="1.0" encoding="utf-8"?><root><element>Some text here</element></root>
}
```


=={{header|Caché ObjectScript}}==


```txt

USER>set writer=##class(%XML.Writer).%New()
USER>set writer.Charset="UTF-8"
USER>Set writer.Indent=1
USER>Set writer.IndentChars="    "
USER>set sc=writer.OutputToString()
USER>set sc=writer.RootElement("root")
USER>set sc=writer.Element("element")
USER>set sc=writer.WriteChars("Some text here")
USER>set sc=writer.EndElement()
USER>set sc=writer.EndRootElement()
USER>Write writer.GetXMLString()

<?xml version="1.0" encoding="UTF-8"?>
<root>
    <element>Some text here</element>
</root>

```



## Clojure

```clojure

(require '[clojure.data.xml :as xml])

(def xml-example (xml/element :root {} (xml/element :element {} "Some text here")))

(with-open [out-file (java.io.OutputStreamWriter.
                        (java.io.FileOutputStream. "/tmp/output.xml") "UTF-8")]
  (xml/emit xml-example out-file))

```


```txt
<?xml version="1.0" encoding="UTF-8"?><root><element>Some text here</element></root>
```



## Common Lisp


Assuming that matching the whitespace in the problem description isn't necessary and that character encodings are permitted (see the [[Talk:DOM XML Serialization|talk page]]):


```lisp
(let* ((doc (dom:create-document 'rune-dom:implementation nil nil nil))
       (root (dom:create-element doc "root"))
       (element (dom:create-element doc "element"))
       (text (dom:create-text-node doc "Some text here")))
  (dom:append-child element text)
  (dom:append-child root element)
  (dom:append-child doc root)
  (dom:map-document (cxml:make-rod-sink) doc))
```


gives the following output:


```txt
<?xml version="1.0" encoding="UTF-8"?>
<root><element>Some text here</element></root>
```


Because <code>dom:append-child</code> returns the appended child, the same output can be produced while binding fewer variables:


```lisp
(let ((doc (dom:create-document 'rune-dom:implementation nil nil nil)))
  (dom:append-child
   (dom:append-child
    (dom:append-child doc (dom:create-element doc "root"))
    (dom:create-element doc "element"))
   (dom:create-text-node doc "Some text here"))
  (write-string (dom:map-document (cxml:make-rod-sink) doc)))
```


The prefix notation makes this hard to decipher, however, and so a final version uses an auxiliary <code>append-child*</code>:


```lisp
(defun append-child* (parent &rest children)
  (reduce 'dom:append-child children :initial-value parent))

(let* ((doc (dom:create-document 'rune-dom:implementation nil nil nil)))
  (append-child* doc
                 (dom:create-element doc "root")
                 (dom:create-element doc "element")
                 (dom:create-text-node doc "Some text here"))
  (write-string (dom:map-document (cxml:make-rod-sink) doc)))
```



## D

```d
module xmltest ;

import std.stdio ;
import std.xml ;

void main() {
  auto doc = new Document("root") ;
//doc.prolog = q"/<?xml version="1.0"?>/" ; // default
  doc ~= new Element("element", "Some text here") ;
  writefln(doc) ;
// output: <?xml version="1.0"?><root><element>Some text here</element></root>
}
```



## E

This makes use of XML libraries provided with Java.


```e>def document := <unsafe:javax.xml.parsers.makeDocumentBuilderFactory
 \
                  .newInstance() \
                  .newDocumentBuilder() \
                  .getDOMImplementation() \
                  .createDocument(null, "root", null)
def root := document.getDocumentElement()
root.appendChild(
  def element := document.createElement("element"))
element.appendChild(
  document.createTextNode("Some text here"))
println(document.saveXML(root))
```


(On the use of <tt>&lt;unsafe></tt>: The class has not yet been reviewed for E safety, so <tt>&lt;import:...makeDocumentBuilderFactory></tt> is not yet allowed. The review would probably be straightforward.)


=={{header|F_Sharp|F#}}==

```fsharp
open System.Xml

[<EntryPoint>]
let main argv =
    let xd = new XmlDocument()
    // Create the required nodes:
    xd.AppendChild (xd.CreateXmlDeclaration("1.0", null, null)) |> ignore
    let root = xd.AppendChild (xd.CreateNode("element", "root", ""))
    let element = root.AppendChild (xd.CreateElement("element", "element", ""))
    element.AppendChild (xd.CreateTextNode("Some text here")) |> ignore
    // The same can be accomplished with:
    // xd.LoadXml("""<?xml version="1.0"?><root><element>Some text here</element></root>""")

    let xw = new XmlTextWriter(System.Console.Out)
    xw.Formatting <- Formatting.Indented
    xd.WriteContentTo(xw)
    0
```

Output

```txt
<?xml version="1.0"?>
<root>
  <element>Some text here</element>
</root>
```



## Fantom



```fantom

using xml

class XmlDom
{
  public static Void main ()
  {
    doc := XDoc()
    root := XElem("root")
    doc.add (root)

    child := XElem("element")
    child.add(XText("Some text here"))
    root.add (child)

    doc.write(Env.cur.out)
  }
}

```


Output:

```txt

<?xml version='1.0' encoding='UTF-8'?>
<root>
 <element>Some text here</element>
</root>

```



## Forth

```forth
include ffl/dom.fs

\ Create a dom variable 'doc' in the dictionary

dom-create doc

\ Add the document root with its version attribute

dom.document doc dom-append-node

s" version" s" 1.0" dom.attribute doc dom-append-node

\ Add root and element

doc dom-parent 2drop

s" root"    dom.element doc dom-append-node

s" element" dom.element doc dom-append-node

\ Add the text

s" Some text here" dom.text doc dom-append-node

\ Convert the document to a string and print

doc dom-write-string [IF]
  type cr
[THEN]
```



## Go

A partial solution based on an incomplete library.  The library is missing functions needed to create a DOM piece by piece like other other solutions here.  It can however create a DOM by parsing XML.  Also, it lacks a function to access the processing instruction, so not surprisingly this is missing from the serialized output.

```go
package main

import (
    "fmt"
    dom "bitbucket.org/rj/xmldom-go"
)

func main() {
    d, err := dom.ParseStringXml(`
<?xml version="1.0" ?>
<root>
    <element>
        Some text here
    </element>
</root>`)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println(string(d.ToXml()))
}
```

```xml><root

    <element>
        Some text here
    </element>
</root>
```



## Groovy


```groovy
import groovy.xml.MarkupBuilder
def writer = new StringWriter() << '<?xml version="1.0" ?>\n'
def xml = new MarkupBuilder(writer)
xml.root() {
    element('Some text here' )
}
println writer
```



## Haskell


Using the XML.Light module from [http://hackage.haskell.org/package/xml-1.3.4 HackageDB]


```Haskell
import Data.List
import Text.XML.Light

xmlDOM :: String -> String
xmlDOM txt = showTopElement $ Element
    (unqual "root")
    []
    [ Elem $ Element
      (unqual "element")
      []
      [Text $ CData CDataText txt Nothing]
      Nothing
    ]
    Nothing
```

Output:

```txt
*Main> mapM_ (putStrLn.ppContent) $ parseXML (xmlDOM "  Some text  ")
<?xml version="1.0" ?>
<root>
  <element>  Some text  </element>
</root>
```



## J

There are probably better ways of doing this, but, here is a simple serializer for a rudimentary concept of a dom:


```j
serialize=: ('<?xml version="1.0" ?>',LF),;@serialize1&''
serialize1=:4 :0
  if.L.x do.
    start=. y,'<',(0{::x),'>',LF
    middle=. ;;(}.x) serialize1&.> <'    ',y
    end=. y,'</',(0{::x),'>',LF
    <start,middle,end
  else.
    <y,x,LF
  end.
)
```


And here is a document object that matches this task:


```j
obj=: 'root';<'element';'some text here'
```


Example use:


```xml
   serialize obj
<?xml version="1.0" ?>
<root>
    <element>
        some text here
    </element>
</root>
```



## Java

[[Java|Java's]] XML DOM tools don't really allow total control of the output format "out of the box" but the following program generates XML that is equivalent to the required output.

```java

import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class RDOMSerialization {

  private Document domDoc;

  public RDOMSerialization() {
    return;
  }

  protected void buildDOMDocument() {

    DocumentBuilderFactory factory;
    DocumentBuilder builder;
    DOMImplementation impl;
    Element elmt1;
    Element elmt2;

    try {
      factory = DocumentBuilderFactory.newInstance();
      builder = factory.newDocumentBuilder();
      impl = builder.getDOMImplementation();
      domDoc = impl.createDocument(null, null, null);
      elmt1 = domDoc.createElement("root");
      elmt2 = domDoc.createElement("element");
      elmt2.setTextContent("Some text here");

      domDoc.appendChild(elmt1);
      elmt1.appendChild(elmt2);
    }
    catch (ParserConfigurationException ex) {
      ex.printStackTrace();
    }

    return;
  }

  protected void serializeXML() {

    DOMSource domSrc;
    Transformer txformer;
    StringWriter sw;
    StreamResult sr;

    try {
      domSrc = new DOMSource(domDoc);

      txformer = TransformerFactory.newInstance().newTransformer();
      txformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
      txformer.setOutputProperty(OutputKeys.METHOD, "xml");
      txformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
      txformer.setOutputProperty(OutputKeys.INDENT, "yes");
      txformer.setOutputProperty(OutputKeys.STANDALONE, "yes");
      txformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");

      sw = new StringWriter();
      sr = new StreamResult(sw);

      txformer.transform(domSrc, sr);

      System.out.println(sw.toString());
    }
    catch (TransformerConfigurationException ex) {
      ex.printStackTrace();
    }
    catch (TransformerFactoryConfigurationError ex) {
      ex.printStackTrace();
    }
    catch (TransformerException ex) {
      ex.printStackTrace();
    }

    return;
  }

  public static void serializationDriver(String[] args) {

    RDOMSerialization lcl = new RDOMSerialization();
    lcl.buildDOMDocument();
    lcl.serializeXML();

    return;
  }

  public static void main(String[] args) {
    serializationDriver(args);
    return;
  }
}

```


'''Output:'''


```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<root>
  <element>Some text here</element>
</root>
```



## JavaScript

DOM


```javascript
var doc = document.implementation.createDocument( null, 'root', null );
var root = doc.documentElement;
var element = doc.createElement( 'element' );
root.appendChild( element );
element.appendChild( document.createTextNode('Some text here') );
var xmlString = new XMLSerializer().serializeToString( doc );
```


E4X


```javascript>var xml = <root

  <element>Some text here</element>
</root>;
var xmlString = xml.toXMLString();
```


E4X — with processing instruction


```javascript
XML.ignoreProcessingInstructions = false;
var xml = <?xml version="1.0"?>
<root>
  <element>Some text here</element>
</root>;
var xmlString = xml.toXMLString();
```



## Julia


```julia
using LightXML

# Modified from the documentation for LightXML.jl. The underlying library requires an encoding string be printed.

# create an empty XML document
xdoc = XMLDocument()

# create & attach a root node
xroot = create_root(xdoc, "root")

# create the first child
xs1 = new_child(xroot, "element")

# add the inner content
add_text(xs1, "some text here")

println(xdoc)


```
```txt

 <?xml version="1.0" encoding="utf-8"?>
 <root>
   <element>some text here</element>
 </root>

```



## Kotlin

This is the closest I could get to the required output.

There appears to be no satisfactory way to prevent the default encoding and standalone attributes from appearing in the XML declaration using the standard JDK DOM implementation. If you set the standalone attribute to true - using doc.setXmlStandalone(true) - then this removes it from the declaration but unfortunately there is then no carriage return before the <root> tag!

So I've decided to leave it as it is.

```scala
// version 1.1.3

import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.dom.DOMSource
import java.io.StringWriter
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.TransformerFactory

fun main(args: Array<String>) {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder  = dbFactory.newDocumentBuilder()
    val doc = dBuilder.newDocument()
    val root = doc.createElement("root")  // create root node
    doc.appendChild(root)
    val element = doc.createElement("element")  // create element node
    val text = doc.createTextNode("Some text here")  // create text node
    element.appendChild(text)
    root.appendChild(element)

    // serialize
    val source = DOMSource(doc)
    val sw = StringWriter()
    val result = StreamResult(sw)
    val tFactory = TransformerFactory.newInstance()
    tFactory.newTransformer().apply {
        setOutputProperty("indent", "yes")
        setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4")
        transform(source, result)
    }
    println(sw)
}
```


```txt

<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<root>
    <element>Some text here</element>
</root>

```



## Lasso


```lasso

content_type( 'text/xml' );// set MIME type if serving

xml( '<?xml version="1.0" ?><root><element>Some text here</element></root>' );


```



## Lingo

Code based on Flash Xtra (comes with Director, i.e. "built-in") that allows to use AS2/AS3 classes in Lingo (in this case "XML" is an AS2 class).

```lingo
-- create an XML document
doc = newObject("XML", "<?xml version='1.0' ?>")

root = doc.createElement("root")
doc.appendChild(root)

element = doc.createElement("element")
root.appendChild(element)

textNode = doc.createTextNode("Some text here")
element.appendChild(textNode)

put doc.toString()
-- "<?xml version='1.0' ?><root><element>Some text here</element></root>"
```



## Lua

Using the widely available 'LuaXML' module

```Lua
require("LuaXML")
local dom = xml.new("root")
local element = xml.new("element")
table.insert(element, "Some text here")
dom:append(element)
dom:save("dom.xml")
```

Resulting contents of dom.xml:

```txt
<?xml version="1.0"?>
<!-- file "dom.xml", generated by LuaXML -->

<root>
  <element>Some text here</element>
</root>
```



## Mathematica


```Mathematica
DOM = XMLObject["Document"][{XMLObject["Declaration"]["Version" -> "1.0","Encoding" -> "utf-8"]},
XMLElement["root", {}, {XMLElement["element", {}, {"Some text here"}]}], {}];

ExportString[DOM, "XML", "AttributeQuoting" -> "\""]
```

Output:

```txt
<?xml version="1.0" encoding="utf-8"?>
<root>
 <element>Some text here</element>
</root>
```



## MATLAB


```MATLAB
docNode = com.mathworks.xml.XMLUtils.createDocument('root');
docRootNode = docNode.getDocumentElement;
thisElement = docNode.createElement('element');
thisElement.appendChild(docNode.createTextNode('Some text here'));
docRootNode.appendChild(thisElement);

```

Output:

```txt
 xmlwrite(docNode)

ans =

<?xml version="1.0" encoding="utf-8"?>
<root>
   <element>Some text here</element>
</root>

```



## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.io.StringWriter
import javax.xml.
import org.w3c.dom.

class RDOMSerialization public

properties private
  domDoc = Document

method main(args = String[]) public static

  lcl = RDOMSerialization()
  lcl.buildDOMDocument()
  lcl.serializeXML()

  return

method buildDOMDocument() inheritable

  do
    factory = DocumentBuilderFactory.newInstance()
    builder = factory.newDocumentBuilder()
    impl = builder.getDOMImplementation()
    domDoc = impl.createDocument(null, null, null)
    elmt1 = domDoc.createElement("root")
    elmt2 = domDoc.createElement("element")
    elmt2.setTextContent("Some text here")

    domDoc.appendChild(elmt1)
    elmt1.appendChild(elmt2)

  catch exPC = ParserConfigurationException
    exPC.printStackTrace
  end

  return

method serializeXML() inheritable

  do
    domSrc = DOMSource(domDoc)
    txformer = TransformerFactory.newInstance().newTransformer()
    txformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no")
    txformer.setOutputProperty(OutputKeys.METHOD, "xml")
    txformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
    txformer.setOutputProperty(OutputKeys.INDENT, "yes")
    txformer.setOutputProperty(OutputKeys.STANDALONE, "yes")
    txformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2")

    sw = StringWriter()
    sr = StreamResult(sw)

    txformer.transform(domSrc, sr)

    say sw.toString

  catch exTC = TransformerConfigurationException
    exTC.printStackTrace
  catch exTF = TransformerFactoryConfigurationError
    exTF.printStackTrace
  catch exTE = TransformerException
    exTE.printStackTrace
  end

  return

```


'''Output:'''


```xml
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<root>
  <element>Some text here</element>
</root>
```



## Nim


```nim
import xmldom

var
  dom = getDOM()
  document = dom.createDocument("", "root")
  topElement = document.documentElement
  firstElement = document.createElement "element"
  textNode = document.createTextNode "Some text here"

topElement.appendChild firstElement
firstElement.appendChild textNode

echo document
```

Output:

```txt
<?xml version="1.0" encoding="UTF-8" ?>
<root>
  <element>
    Some text here
  </element>
</root>
```



## Objeck


```objeck
use XML;

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      builder := XMLBuilder->New("root", "1.0");
      root := builder->GetRoot();
      element := XMLElement->New(XMLElementType->ELEMENT, "element", "Some text here");
      root->AddChild(element);
      builder->ToString()->PrintLine();
    }
  }
}
```



## OpenEdge/Progress

The following example uses the X-DOCUMENT, for faster processing SAX can be used.

```progress

DEFINE VARIABLE hxdoc      AS HANDLE NO-UNDO.
DEFINE VARIABLE hxroot     AS HANDLE NO-UNDO.
DEFINE VARIABLE hxelement  AS HANDLE NO-UNDO.
DEFINE VARIABLE hxtext     AS HANDLE NO-UNDO.
DEFINE VARIABLE lcc        AS LONGCHAR NO-UNDO.

CREATE X-DOCUMENT hxdoc.

CREATE X-NODEREF hxroot.
hxdoc:CREATE-NODE( hxroot, 'root', 'ELEMENT' ).
hxdoc:APPEND-CHILD( hxroot ).

CREATE X-NODEREF hxelement.
hxdoc:CREATE-NODE( hxelement, 'element', 'ELEMENT' ).
hxroot:APPEND-CHILD( hxelement ).

CREATE X-NODEREF hxtext.
hxdoc:CREATE-NODE( hxtext, 'element', 'TEXT' ).
hxelement:APPEND-CHILD( hxtext ).
hxtext:NODE-VALUE = 'Some text here'.

hxdoc:SAVE( 'LONGCHAR', lcc ).
MESSAGE STRING( lcc ) VIEW-AS ALERT-BOX.

```



## Oz

With the code from [[XML Creation#Oz]], we can write:


```oz
declare
  proc {Main}
   DOM = root(element("Some text here"))
  in
     {System.showInfo {Serialize DOM}}
  end
  ...
```


Output:

```xml
<?xml version="1.0" ?>
<root>
    <element>Some text here</element>
</root>
```



## Pascal

```pascal
program CrearXML;

{$mode objfpc}{$H+}

uses
  Classes, XMLWrite, DOM;

var
  xdoc: TXMLDocument;                                  // variable objeto documento XML
  NodoRaiz, NodoPadre, NodoHijo: TDOMNode;             // variables a los nodos
begin
  //crear el documento
  xdoc := TXMLDocument.create;

  NodoRaiz := xdoc.CreateElement('root');               // crear el nodo raíz
  Xdoc.Appendchild(NodoRaiz);                           // guardar nodo raíz
  NodoPadre := xdoc.CreateElement('element');           // crear el nodo hijo
  NodoHijo := xdoc.CreateTextNode('Some text here');    // insertar el valor del nodo
  NodoPadre.Appendchild(NodoHijo);                      // guardar nodo
  NodoRaiz.AppendChild(NodoPadre);                      // insertar el nodo hijo en el correspondiente nodo padre
  writeXMLFile(xDoc,'prueba.xml');                      // escribir el XML
  Xdoc.free;
end.
```


Output:
```txt

<?xml version="1.0"?>
<root>
  <element>Some text here</element>
</root>.

```



## Perl


```perl
use XML::Simple;
print XMLout( { root => { element => "Some text here" } }, NoAttr => 1, RootName => "" );
```


'''Output''':
 <root>
   <element>Some text here</element>
 </root>

```perl
use XML::DOM::BagOfTricks qw(createDocument createTextElement);

my ($doc, $root) = createDocument('root');
$root->appendChild(
    createTextElement($doc, 'element', 'Some text here')
);
print $doc->toString;
```


'''Output''':
 <root><element>Some text here</element></root>

```perl
use XML::LibXML;

$xml = XML::LibXML::Document->new('1.0');
$node = $xml->createElement('root');
$xml->setDocumentElement($node);
$node2 = $xml->createElement('element');
$text = $xml->createTextNode('Some text here');
$node2->addChild($text);
$node->appendWellBalancedChunk('text');
$node->addChild($node2);

print $xml->toString;
```


'''Output''':
 <?xml version="1.0"?>
 <root>text<element>Some text here</element></root>


## Perl 6

```perl6
use XML;
use XML::Writer;

say my $document = XML::Document.new(
    XML::Writer.serialize( :root[ :element['Some text here', ], ] )
);
```

```xml
<?xml version="1.0"?><root><element>Some text here</element></root>
```



## Phix


```Phix
include builtins/xml.e
sequence elem = xml_new_element("element", "Some text here"),
         root = xml_new_element("root", {elem}),
         doc = xml_new_doc(root,{`<?xml version="1.0" ?>`})
puts(1,xml_sprint(doc))
```

```txt

<?xml version="1.0" ?>
<root>
  <element>Some text here</element>
</root>

```

If only one parameter is supplied to xml_new_doc(), the output includes the default `encoding="utf-8" `.


## PHP

```php
<?php
$dom = new DOMDocument();//the constructor also takes the version and char-encoding as it's two respective parameters
$dom->formatOutput = true;//format the outputted xml
$root = $dom->createElement('root');
$element = $dom->createElement('element');
$element->appendChild($dom->createTextNode('Some text here'));
$root->appendChild($element);
$dom->appendChild($root);
$xmlstring = $dom->saveXML();
```



## PicoLisp


```PicoLisp
(load "@lib/xm.l")

(xml? T)
(xml '(root NIL (element NIL "Some text here")))
```

Output:

```txt
<?xml version="1.0" encoding="utf-8"?>
<root>
   <element>Some text here</element>
</root>
```


## Pike

manually, one node at a time:

```Pike
object dom = Parser.XML.Tree.SimpleRootNode();
dom->add_child(Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_HEADER, "", ([]), ""));
object node = Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_ELEMENT, "root", ([]), "");
dom->add_child(node);

object subnode = Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_ELEMENT, "element", ([]), "");
node->add_child(subnode);

node = subnode;
subnode = Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_TEXT, "", ([]), "Some text here");
node->add_child(subnode);

dom->render_xml();
Result: "<?xml version='1.0' encoding='utf-8'?><root><element>Some text here</element></root>"
```


from an array, using a conversion function:

```Pike
object make_xml_node(array|string in, void|int level)
{
    level++;
    if (stringp(in))
        return Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_TEXT, "", ([]), in);
    else
    {
        object node = Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_ELEMENT, in[0], in[1], "");
        foreach(in[2..];; array|string child)
        {
            node->add_child(make_xml_node(child, level));
        }
        return node;
    }
}

object make_xml_tree(array input)
{
    object dom = Parser.XML.Tree.SimpleRootNode();
    dom->add_child(Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_HEADER, "", ([]), ""));
    dom->add_child(make_xml_node(input));
    return dom;
}

array input = ({ "root", ([]), ({ "element", ([]), "Some text here" }) });
make_xml_tree(input)->render_xml();
Result: "<?xml version='1.0' encoding='utf-8'?><root><element>Some text here</element></root>"
```


to render the output with indenting as specified in the task, this function adds the necessary text nodes:

```Pike
object indent_xml(object parent, void|int indent_text, void|int level)
{
    int subnodes = false;
    foreach(parent->get_children();; object child)
    {
        if (child->get_node_type() == Parser.XML.Tree.XML_ELEMENT ||
            (child->get_node_type() == Parser.XML.Tree.XML_TEXT && indent_text))
        {
            subnodes = true;
            parent->add_child_before(Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_TEXT, "", ([]), "\r\n"+"    "*level), child);
            indent_xml(child, indent_text, level+1);
        }
    }
    if (subnodes && level)
        parent->add_child(Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_TEXT, "", ([]), "\r\n"+"    "*(level-1)));
    return parent;
}


indent_xml(make_xml_tree(input))->render_xml();
Result: "<?xml version='1.0' encoding='utf-8'?>\r\n"
        "<root>\r\n"
        "    <element>Some text here</element>\r\n"
        "</root>"

indent_xml(make_xml_tree(input), 1)->render_xml();
Result: "<?xml version='1.0' encoding='utf-8'?>\r\n"
        "<root>\r\n"
        "    <element>\r\n"
        "        Some text here\r\n"
        "    </element>\r\n"
        "</root>"
```



## Python

```python
from xml.dom.minidom import getDOMImplementation

dom = getDOMImplementation()
document = dom.createDocument(None, "root", None)

topElement = document.documentElement
firstElement = document.createElement("element")
topElement.appendChild(firstElement)
textNode = document.createTextNode("Some text here")
firstElement.appendChild(textNode)

xmlString = document.toprettyxml(" " * 4)
```



```python
from xml.etree import ElementTree as et

root = et.Element("root")
et.SubElement(root, "element").text = "Some text here"
xmlString = et.tostring(root)
```



## Racket


```racket

#lang at-exp racket
(require xml)

(define xml-str
  @~a{<?xml version="1.0" ?>
       <root>
           <element>
               Some text here
           </element>
       </root>})

;; read & parse to get an xml value
(define xml (read-xml/document (open-input-string xml-str)))
;; print it out in xml form, which is identical to the input xml
(write-xml xml)
(newline)

```



## Rascal


```rascal
import lang::xml::DOM;

public void main(){
	x = document(element(none(), "root", [element(none(), "element", [charData("Some text here")])]));
	return println(xmlPretty(x));
}
```

Output example:

```rascal>rascal
main()
<?xml version="1.0" encoding="UTF-8"?>
<root>
  <element>Some text here</element>
</root>
```



## Ruby

```ruby
require("rexml/document")
include REXML
(doc = Document.new) << XMLDecl.new
root = doc.add_element('root')
element = root.add_element('element')
element.add_text('Some text here')

# save to a string
# (the first argument to write() needs an object that understands "<<")
serialized = String.new
doc.write(serialized, 4)
puts serialized
```


produces

```txt
<?xml version='1.0'?>
<root>
    <element>
        Some text here
    </element>
</root>
```



## Scala


```scala>val xml = <root><element>Some text here</element></root

scala.xml.XML.save(filename="output.xml", node=xml, enc="UTF-8", xmlDecl=true, doctype=null)
```



## Sidef

```ruby
require('XML::Simple');
print %S'XML::Simple'.XMLout(
    :(root => :( element => 'Some text here' )),
    NoAttr => 1, RootName => '',
);
```

```txt

  <root>
    <element>Some text here</element>
  </root>

```



## Tcl

```tcl
package require tdom
set d [dom createDocument root]
set root [$d documentElement]
$root appendChild [$d createElement element]
[$root firstChild] appendChild [$d createTextNode "Some text here"]
$d asXML
```


```txt
<root>
    <element>Some text here</element>
</root>
```

Using [http://tclxml.sf.net TclDOM]

```tcl
package require dom
set doc [dom::DOMImplementation create]
set root [dom::document createElement $doc root]
set elem [dom::document createElement $root element]
set text [dom::document createTextNode $elem "Some text here"]
dom::DOMImplementation serialize $doc -newline {element}
```


```txt
<?xml version='1.0'?>
<!DOCTYPE root>
<root>
<element>
Some text here
</element>
</root>
```



## XProc


```xml
<p:pipeline xmlns:p="http://www.w3.org/ns/xproc" version="1.0">
  <p:identity>
    <p:input port="source">
      <p:inline>
        <root>
          <element>
            Some text here
          </element>
        </root>
      </p:inline>
    </p:input>
  </p:identity>
</p:pipeline>
```



## XQuery

In XQuery static element construction is like normal XML:

```xquery><root

  <element>
    Some text here
  </element>
</root>
```


Dynamic element construction looks quite different:

```xquery
let $rootTagname := 'root'
let $elementTagname := 'element'
let $elementContent := 'Some text here'

return
  element {$rootTagname}
          {
            element{$elementTagname}
                   {$elementContent}
          }
```



## XSLT


```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" indent="yes" />
  <xsl:template match="/">   <!-- replace the root of the incoming document with our own model -->
    <xsl:element name="root">
      <xsl:element name="element">
        <xsl:text>Some text here</xsl:text>
      </xsl:element>
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>
```


