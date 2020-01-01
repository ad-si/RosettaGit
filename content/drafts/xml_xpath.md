+++
title = "XML/XPath"
description = ""
date = 2019-08-04T16:51:04Z
aliases = []
[extra]
id = 1643
[taxonomies]
categories = []
tags = []
+++

{{task|XML}}

Perform the following three XPath queries on the XML Document below:
* Retrieve the first "item" element
* Perform an action on each "price" element (print it out)
* Get an array of all the "name" elements

XML Document:
 <inventory title="OmniCorp Store #45x10^3">
   <section name="health">
     <item upc="123456789" stock="12">
       <name>Invisibility Cream</name>
       <price>14.50</price>
       <description>Makes you invisible</description>
     </item>
     <item upc="445322344" stock="18">
       <name>Levitation Salve</name>
       <price>23.99</price>
       <description>Levitate yourself for up to 3 hours per application</description>
     </item>
   </section>
   <section name="food">
     <item upc="485672034" stock="653">
       <name>Blork and Freen Instameal</name>
       <price>4.95</price>
       <description>A tasty meal in a tablet; just add water</description>
     </item>
     <item upc="132957764" stock="44">
       <name>Grob winglets</name>
       <price>3.56</price>
       <description>Tender winglets of Grob. Just add water</description>
     </item>
   </section>
 </inventory>

## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program xpathXml.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

.equ NBMAXELEMENTS,    100

/*******************************************/
/* Structures                               */
/********************************************/
/* structure xmlNode*/
    .struct  0
xmlNode_private:                         @ application data
    .struct  xmlNode_private + 4
xmlNode_type:                            @ type number, must be second !
    .struct  xmlNode_type + 4
xmlNode_name:                            @ the name of the node, or the entity
    .struct  xmlNode_name + 4
xmlNode_children:                        @ parent->childs link
    .struct  xmlNode_children + 4
xmlNode_last:                            @ last child link
    .struct  xmlNode_last + 4
xmlNode_parent:                          @ child->parent link
    .struct  xmlNode_parent + 4
xmlNode_next:                            @ next sibling link
    .struct  xmlNode_next + 4
xmlNode_prev:                            @ previous sibling link
    .struct  xmlNode_prev + 4
xmlNode_doc:                             @ the containing document
    .struct  xmlNode_doc + 4
xmlNode_ns:                              @ pointer to the associated namespace
    .struct  xmlNode_ns + 4
xmlNode_content:                         @ the content
    .struct  xmlNode_content + 4
xmlNode_properties:                      @ properties list
    .struct  xmlNode_properties + 4
xmlNode_nsDef:                           @ namespace definitions on this node
    .struct  xmlNode_nsDef + 4
xmlNode_psvi:                            @ for type/PSVI informations
    .struct  xmlNode_psvi + 4
xmlNode_line:                            @ line number
    .struct  xmlNode_line + 4
xmlNode_extra:                           @ extra data for XPath/XSLT
    .struct  xmlNode_extra + 4
xmlNode_fin:
/********************************************/
/* structure xmlNodeSet*/
    .struct  0
xmlNodeSet_nodeNr:                       @ number of nodes in the set
    .struct  xmlNodeSet_nodeNr + 4
xmlNodeSet_nodeMax:                      @ size of the array as allocated
    .struct  xmlNodeSet_nodeMax + 4
xmlNodeSet_nodeTab:                      @ array of nodes in no particular order
    .struct  xmlNodeSet_nodeTab + 4
xmlNodeSet_fin:
/********************************************/
/* structure xmlXPathObject*/
    .struct  0
xmlPathObj_type:                         @
    .struct  xmlPathObj_type + 4
xmlPathObj_nodesetval:                   @
    .struct  xmlPathObj_nodesetval + 4
xmlPathObj_boolval:                      @
    .struct  xmlPathObj_boolval + 4
xmlPathObj_floatval:                     @
    .struct  xmlPathObj_floatval + 4
xmlPathObj_stringval:                    @
    .struct  xmlPathObj_stringval + 4
xmlPathObj_user:                         @
    .struct  xmlPathObj_user + 4
xmlPathObj_index:                        @
    .struct  xmlPathObj_index + 4
xmlPathObj_user2:                        @
    .struct  xmlPathObj_user2 + 4
xmlPathObj_index2:                       @
    .struct  xmlPathObj_index2 + 4



/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessEndpgm:      .asciz "\nNormal end of program.\n"
szMessDisVal:      .asciz "\nDisplay set values.\n"
szMessDisArea:     .asciz "\nDisplay area values.\n"
szFileName:        .asciz "testXml.xml"
szMessError:       .asciz "Error detected !!!!. \n"


szLibName:         .asciz "name"
szLibPrice:        .asciz "//price"
szLibExtName:      .asciz "//name"
szCarriageReturn:  .asciz "\n"


/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
.align 4
tbExtract:          .skip 4 * NBMAXELEMENTS      @ result extract area
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                     @ entry of program
    ldr r0,iAdrszFileName
    bl xmlParseFile                       @ create doc
    mov r9,r0                             @ doc address
    mov r0,r9                             @ doc
    bl xmlDocGetRootElement               @ get root
    bl xmlFirstElementChild               @ get first section
    bl xmlFirstElementChild               @ get first item
    bl xmlFirstElementChild               @ get first name
    bl xmlNodeGetContent                  @ extract content
    bl affichageMess                      @ for display
    ldr r0,iAdrszCarriageReturn
    bl affichageMess

    ldr r0,iAdrszMessDisVal
    bl affichageMess
    mov r0,r9
    ldr r1,iAdrszLibPrice                  @ extract prices
    bl extractValue
    mov r0,r9
    ldr r1,iAdrszLibExtName                @ extact names
    bl extractValue
    ldr r0,iAdrszMessDisArea
    bl affichageMess
    mov r4,#0                              @ display string result area
    ldr r5,iAdrtbExtract
1:
    ldr r0,[r5,r4,lsl #2]
    cmp r0,#0
    beq 2f
    bl affichageMess
    ldr r0,iAdrszCarriageReturn
    bl affichageMess
    add r4,#1
    b 1b

2:
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
iAdrszLibName:            .int szLibName
iAdrszLibPrice:           .int szLibPrice
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrszFileName:           .int szFileName
iAdrszLibExtName:         .int szLibExtName
iAdrtbExtract:            .int tbExtract
iAdrszMessDisVal:          .int szMessDisVal
iAdrszMessDisArea:         .int szMessDisArea
/******************************************************************/
/*     extravt value of set                                       */
/******************************************************************/
/* r0 contains the doc address
/* r1 contains the address of the libel to extract */
extractValue:
    push {r1-r10,lr}                     @ save  registres
    mov r4,r1                            @ save address libel
    mov r9,r0                            @ save doc
    ldr r8,iAdrtbExtract
    bl xmlXPathNewContext                @ create context
    mov r10,r0
    mov r1,r0
    mov r0,r4
    bl xmlXPathEvalExpression
    mov r5,r0
    mov r0,r10
    bl xmlXPathFreeContext               @ free context
    cmp r5,#0
    beq 100f
    ldr r4,[r5,#xmlPathObj_nodesetval]   @ values set
    ldr r6,[r4,#xmlNodeSet_nodeNr]       @ set size
    mov r7,#0                            @ index
    ldr r4,[r4,#xmlNodeSet_nodeTab]      @ area of nods
1:                                       @ start loop
    ldr r3,[r4,r7,lsl #2]                @ load node
    mov r0,r9
    ldr r1,[r3,#xmlNode_children]        @ load string value
    mov r2,#1
    bl xmlNodeListGetString
    str r0,[r8,r7,lsl #2]                @ store string pointer in area
    bl affichageMess                     @ and display string result
    ldr r0,iAdrszCarriageReturn
    bl affichageMess
    add r7,#1
    cmp r7,r6
    blt 1b
100:
    pop {r1-r10,lr}                      @ restaur registers */
    bx lr                                @ return

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


```

{{output}}

```txt

Invisibility Cream

Display set values.
14.50
23.99
4.95
3.56
Invisibility Cream
Levitation Salve
Blork and Freen Instameal
Grob winglets

Display area values.
Invisibility Cream
Levitation Salve
Blork and Freen Instameal
Grob winglets

Normal end of program.


```


## AutoHotkey

With regular expressions

```AutoHotkey
FileRead, inventory, xmlfile.xml

RegExMatch(inventory, "<item.*?</item>", item1)
MsgBox % item1

pos = 1
While, pos := RegExMatch(inventory, "<price>(.*?)</price>", price, pos + 1)
  MsgBox % price1

While, pos := RegExMatch(inventory, "<name>.*?</name>", name, pos + 1)
  names .= name . "`n"
MsgBox % names
```

{{libheader|AHK XPath}}

```AutoHotkey
#Include xpath.ahk

xpath_load(doc, "xmlfile.xml")

; Retrieve the first "item" element
MsgBox % xpath(doc, "/inventory/section[1]/item[1]/text()")

; Perform an action on each "price" element (print it out)
prices := xpath(doc, "/inventory/section/item/price/text()")
Loop, Parse, prices,`,
  reordered .= A_LoopField "`n"
MsgBox % reordered

; Get an array of all the "name" elements
MsgBox % xpath(doc, "/inventory/section/item/name")
```




## AppleScript

'''Using System Events'''

''AppleScript has no-built in support for XPath, but it could be used via a 'do shell script' command. Here's a solution using Apple System Events.


```AppleScript
set theXMLdata to "<inventory title=\"OmniCorp Store #45x10^3\">
  <section name=\"health\">
    <item upc=\"123456789\" stock=\"12\">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc=\"445322344\" stock=\"18\">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name=\"food\">
    <item upc=\"485672034\" stock=\"653\">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc=\"132957764\" stock=\"44\">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>"

on getElementValuesByName(theXML, theNameToFind)
	set R to {}
	tell application "System Events"
		repeat with i in theXML
			set {theName, theElements} to {i's name, i's XML elements}
			if (count of theElements) > 0 then set R to R & my getElementValuesByName(theElements, theNameToFind)
			if theName = theNameToFind then set R to R & i's value
		end repeat
	end tell
	return R
end getElementValuesByName

on getBlock(theXML, theItem, theInstance)
	set text item delimiters to ""
	repeat with i from 1 to theInstance
		set {R, blockStart, blockEnd} to {{}, "<" & theItem & space, "</" & theItem & ">"}
		set x to offset of blockStart in theXML
		if x = 0 then exit repeat
		set y to offset of blockEnd in (characters x thru -1 of theXML as string)
		if y = 0 then exit repeat
		set R to characters x thru (x + y + (length of blockEnd) - 2) of theXML as string
		set theXML to characters (y + (length of blockEnd)) thru -1 of theXML as string
	end repeat
	return R
end getBlock

tell application "System Events"
	set xmlData to make new XML data with properties {name:"xmldata", text:theXMLdata}

	return my getBlock(xmlData's text, "item", 1) -- Solution to part 1 of problem.
	return my getElementValuesByName(xmlData's contents, "name") -- Solution to part 2 of problem.
	return my getElementValuesByName(xmlData's contents, "price") -- Solution to part 3 of problem.

end tell
```
Output for the three results (respectively):
```AppleScript
"<item upc=\"123456789\" stock=\"12\">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>"

{"Invisibility Cream", "Levitation Salve", "Blork and Freen Instameal", "Grob winglets"}

{"14.50", "23.99", "4.95", "3.56"}
```



## Bracmat


```bracmat
{Retrieve the first "item" element}
(   nestML$(get$("doc.xml",X,ML))
  :   ?
      ( inventory
      . ?,? (section.?,? ((item.?):?item) ?) ?
      )
      ?
& out$(toML$!item)
)

{Perform an action on each "price" element (print it out)}
(   nestML$(get$("doc.xml",X,ML))
  :   ?
      ( inventory
      .   ?
        ,   ?
            ( section
            .   ?
              ,   ?
                  ( item
                  .   ?
                    ,   ?
                        ( price
                        .   ?
                          ,   ?price
                            & out$!price
                            & ~
                        )
                        ?
                  )
                  ?
            )
            ?
      )
      ?
|
)

{Get an array of all the "name" elements}
(   :?anArray
  &   nestML$(get$("doc.xml",X,ML))
    :   ?
        ( inventory
        .   ?
          ,   ?
              ( section
              .   ?
                ,   ?
                    ( item
                    .   ?
                      ,   ?
                          ( name
                          .   ?
                            ,   ?name
                              & !anArray !name:?anArray
                              & ~
                          )
                          ?
                    )
                    ?
              )
              ?
        )
        ?
| out$!anArray {Not truly an array, but a list.}
);
```



## C

{{libheader|LibXML}}
Takes XML document and XPath expression as inputs and prints results, usage is printed if invoked incorrectly.

```C

#include <libxml/parser.h>
#include <libxml/xpath.h>

xmlDocPtr getdoc (char *docname) {
	xmlDocPtr doc;
	doc = xmlParseFile(docname);

	return doc;
}

xmlXPathObjectPtr getnodeset (xmlDocPtr doc, xmlChar *xpath){

	xmlXPathContextPtr context;
	xmlXPathObjectPtr result;

	context = xmlXPathNewContext(doc);

	result = xmlXPathEvalExpression(xpath, context);
	xmlXPathFreeContext(context);

	return result;
}

int main(int argc, char **argv) {

	if (argc <= 2) {
		printf("Usage: %s <XML Document Name> <XPath expression>\n", argv[0]);
		return 0;
	}

	char *docname;
	xmlDocPtr doc;
	xmlChar *xpath = (xmlChar*) argv[2];
	xmlNodeSetPtr nodeset;
	xmlXPathObjectPtr result;
	int i;
	xmlChar *keyword;

	docname = argv[1];
	doc = getdoc(docname);
	result = getnodeset (doc, xpath);
	if (result) {
		nodeset = result->nodesetval;
		for (i=0; i < nodeset->nodeNr; i++) {
		xmlNodePtr titleNode = nodeset->nodeTab[i];
		keyword = xmlNodeListGetString(doc, titleNode->xmlChildrenNode, 1);
		printf("Value %d: %s\n",i+1, keyword);
		xmlFree(keyword);
		}
		xmlXPathFreeObject (result);
	}
	xmlFreeDoc(doc);
	xmlCleanupParser();
	return 0;
}

```

testXML.xml contains the XML mentioned in the task description. Code must be compiled with the correct flags.

```txt

C:\rosettaCode>xPather.exe testXML.xml //price
Value 1: 14.50
Value 2: 23.99
Value 3: 4.95
Value 4: 3.56

C:\rosettaCode>xPather.exe testXML.xml //name
Value 1: Invisibility Cream
Value 2: Levitation Salve
Value 3: Blork and Freen Instameal
Value 4: Grob winglets

```



## C#


```c#
XmlReader XReader;

// Either read the xml from a string ...
XReader = XmlReader.Create(new StringReader("<inventory title=... </inventory>"));

// ... or read it from the file system.
XReader = XmlReader.Create("xmlfile.xml");

// Create a XPathDocument object (which implements the IXPathNavigable interface)
// which is optimized for XPath operation. (very fast).
IXPathNavigable XDocument = new XPathDocument(XReader);

// Create a Navigator to navigate through the document.
XPathNavigator Nav = XDocument.CreateNavigator();
Nav = Nav.SelectSingleNode("//item");

// Move to the first element of the selection. (if available).
if(Nav.MoveToFirst())
{
  Console.WriteLine(Nav.OuterXml); // The outer xml of the first item element.
}

// Get an iterator to loop over multiple selected nodes.
XPathNodeIterator Iterator = XDocument.CreateNavigator().Select("//price");

while (Iterator.MoveNext())
{
  Console.WriteLine(Iterator.Current.Value);
}

Iterator = XDocument.CreateNavigator().Select("//name");

// Use a generic list.
List<string> NodesValues = new List<string>();

while (Iterator.MoveNext())
{
  NodesValues.Add(Iterator.Current.Value);
}

// Convert the generic list to an array and output the count of items.
Console.WriteLine(NodesValues.ToArray().Length);
```



## C++

{{improve|C++|Does not use XPath}}

```cpp
#include <vector>
#include <string>
#include <iostream>
#include <boost/regex.hpp>
#include <algorithm>
#include <iterator>

int main( ) {
   const std::string xmltext(
      "<inventory title=\"OmniCorp Store #45x10^3\">"
	"<section name=\"health\">"
	  "<item upc=\"123456789\" stock=\"12\">"
	    "<name>Invisibility Cream</name>"
	    "<price>14.50</price>"
	    "<description>Makes you invisible</description>"
	  "</item>"
	  "<item upc=\"445322344\" stock=\"18\">"
	    "<name>Levitation Salve</name>"
	    "<price>23.99</price>"
	    "<description>Levitate yourself for up to 3 hours per application</description>"
	  "</item>"
	"</section>"
	"<section name=\"food\">"
	  "<item upc=\"485672034\" stock=\"653\">"
	    "<name>Blork and Freen Instameal</name>"
	    "<price>4.95</price>"
	    "<description>A tasty meal in a tablet; just add water</description>"
	  "</item>"
	  "<item upc=\"132957764\" stock=\"44\">"
	    "<name>Grob winglets</name>"
	    "<price>3.56</price>"
	    "<description>Tender winglets of Grob. Just add water</description>"
	  "</item>"
	"</section>"
      "</inventory>" ) ;
   std::string::size_type found = xmltext.find( "<item" , 0 ) ; //beginning of first item
   std::string::size_type foundnext = xmltext.find(  "</item>" , found + 5 ) ; //and its end
   std::cout << "The first item is\n" << xmltext.substr( found + 5 , foundnext - ( found + 5 ) ) << '\n' ;
   std::string::const_iterator start , end ;
   start = xmltext.begin( ) ;
   end = xmltext.end( ) ;
   boost::match_results<std::string::const_iterator> what ;
   boost::regex pricefind( "<price>(\\d+\\.?\\d+)</price>" ) ;//this regex finds the prices
   start = xmltext.begin( ) ;
   std::cout << "The prices are:\n" ;
   while ( boost::regex_search( start , end , what , pricefind ) ) {
      std::string price( what[ 1 ].first , what[ 1 ].second ) ;//find the first price
      std::cout << price << std::endl ;
      start = what[ 1 ].second ;                               //continue search after first price found
   }
   start = xmltext.begin( ) ;
   std::vector<std::string> names ;
   boost::regex namefind( "<name>(.+?)</name>" ) ;            //find characters, be greedy!
   while ( boost::regex_search ( start , end , what , namefind ) ) {
      std::string name ( what[ 1 ].first , what[ 1 ].second ) ;
      names.push_back( name ) ;
      start = what[ 1 ].second ;
   }
   std::cout << "The following name elements were found in the xml string:\n" ;
   std::copy( names.begin( ) , names.end( ) , std::ostream_iterator<std::string>( std::cout , "\n" )) ;
   return 0 ;
}
```


=={{header|Caché ObjectScript}}==


```cos
Class XML.Inventory [ Abstract ]
{

XData XMLData
{
<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>
}

ClassMethod QueryXMLDoc(Output names As %List) As %Status
{
   // get xml stream from the 'XData' block contained in this class
   Set xdata=##class(%Dictionary.CompiledXData).%OpenId($this_"||XMLData",, .sc)
   If $$$ISERR(sc) Quit sc
   Set sc=##class(%XML.XPATH.Document).CreateFromStream(xdata.Data, .xdoc)
   If $$$ISERR(sc) Quit sc

   // retrieve the first 'item' element
   Set sc=xdoc.EvaluateExpression("//section[1]", "item[1]", .res)

   // perform an action on each 'price' element (print it out)
   Set sc=xdoc.EvaluateExpression("//price", "text()", .res)
   If $$$ISERR(sc) Quit sc
   For i=1:1:res.Count() {
	   If i>1 Write ", "
	   Write res.GetAt(i).Value
   }

   // get an array of all the 'name' elements
   Set sc=xdoc.EvaluateExpression("//item", "name", .res)
   If $$$ISERR(sc) Quit sc
   Set key=""
   Do {
	   Set dom=res.GetNext(.key)
	   If '$IsObject(dom) Quit
	   While dom.Read() {
		   If dom.HasValue Set $List(names, key)=dom.Value
	   }
   } While key'=""

   // finished
   Quit $$$OK
}

}
```

{{out|Examples}}

```txt

USER>Do ##class(XML.Inventory).QueryXMLDoc(.list)
14.50, 23.99, 4.95, 3.56
USER>Write $ListToString(list, ", ")
Invisibility Cream, Levitation Salve, Blork and Freen Instameal, Grob winglets

```



## CoffeeScript

<div class='mw-collapsible mw-collapsed'>

```coffeescript

doc = new DOMParser().parseFromString '
    <inventory title="OmniCorp Store #45x10^3">
      <section name="health">
        <item upc="123456789" stock="12">
          <name>Invisibility Cream</name>
          <price>14.50</price>
          <description>Makes you invisible</description>
        </item>
        <item upc="445322344" stock="18">
          <name>Levitation Salve</name>
          <price>23.99</price>
          <description>Levitate yourself for up to 3 hours per application</description>
        </item>
      </section>
      <section name="food">
        <item upc="485672034" stock="653">
          <name>Blork and Freen Instameal</name>
          <price>4.95</price>
          <description>A tasty meal in a tablet; just add water</description>
        </item>
        <item upc="132957764" stock="44">
          <name>Grob winglets</name>
          <price>3.56</price>
          <description>Tender winglets of Grob. Just add water</description>
        </item>
      </section>
    </inventory>
', 'text/xml'

```

</div>
<nowiki>#</nowiki> "doc" is the XML as a Document object. Click expand to see parsing code ⇒


```coffeescript

# Retrieve the first "item" element
doc.evaluate('//item', doc, {}, 7, {}).snapshotItem 0

# Perform an action on each "price" element (print it out)
prices = doc.evaluate "//price", doc, {}, 7, {}
for i in [0...prices.snapshotLength] by 1
    console.log prices.snapshotItem(i).textContent

# Get an array of all the "name" elements
names = doc.evaluate "//name", doc, {}, 7, {}
names = for i in [0...names.snapshotLength] by 1
    names.snapshotItem i

```



## ColdFusion


```cfm
<cfsavecontent variable="xmlString">
<inventory
...
</inventory>
</cfsavecontent>
<cfset xml = xmlParse(xmlString)>
<!--- First Task --->
<cfset itemSearch = xmlSearch(xml, "//item")>
<!--- item = the first Item (xml element object) --->
<cfset item = itemSearch[1]>
<!--- Second Task --->
<cfset priceSearch = xmlSearch(xml, "//price")>
<!--- loop and print each price --->
<cfloop from="1" to="#arrayLen(priceSearch)#" index="i">
  #priceSearch[i].xmlText#<br/>
</cfloop>
<!--- Third Task --->
<!--- array of all the name elements --->
<cfset names = xmlSearch(xml, "//name")>
<!--- visualize the results --->
<cfdump var="#variables#">
```



## Common Lisp

{{libheader|plexippus-xpath}}
{{libheader|cxml}}
{{libheader|cxml-stp}}


```lisp
(dolist (system '(:xpath :cxml-stp :cxml))
  (asdf:oos 'asdf:load-op system))

(defparameter *doc* (cxml:parse-file "xml" (stp:make-builder)))

(xpath:first-node (xpath:evaluate "/inventory/section[1]/item[1]" *doc*))

(xpath:do-node-set (node (xpath:evaluate "/inventory/section/item/price/text()" *doc*))
  (format t "~A~%" (stp:data node)))

(defun node-array (node-set)
  (coerce (xpath:all-nodes node-set) 'vector))

(node-array
 (xpath:evaluate "/inventory/section/item/name" *doc*))
```



## D

It is important to note that the KXML library currently only supports XPath minimally.
{{libheader|KXML}}

```d
import kxml.xml;
char[]xmlinput =
"<inventory title=\"OmniCorp Store #45x10^3\">
  <section name=\"health\">
    <item upc=\"123456789\" stock=\"12\">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc=\"445322344\" stock=\"18\">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name=\"food\">
    <item upc=\"485672034\" stock=\"653\">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc=\"132957764\" stock=\"44\">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>
";
void main() {
        auto root = readDocument(xmlinput);
        auto firstitem = root.parseXPath("inventory/section/item")[0];
        foreach(price;root.parseXPath("inventory/section/item/price")) {
                std.stdio.writefln("%s",price.getCData);
        }
        auto namearray = root.parseXPath("inventory/section/item/name");
}
```



## Delphi


```Delphi
program XMLXPath;

{$APPTYPE CONSOLE}

uses ActiveX, MSXML;

const
  XML =
    '<inventory title="OmniCorp Store #45x10^3">' +
    '  <section name="health">' +
    '    <item upc="123456789" stock="12">' +
    '      <name>Invisibility Cream</name>' +
    '      <price>14.50</price>' +
    '      <description>Makes you invisible</description>' +
    '    </item>' +
    '    <item upc="445322344" stock="18">' +
    '      <name>Levitation Salve</name>' +
    '      <price>23.99</price>' +
    '      <description>Levitate yourself for up to 3 hours per application</description>' +
    '    </item>' +
    '  </section>' +
    '  <section name="food">' +
    '    <item upc="485672034" stock="653">' +
    '      <name>Blork and Freen Instameal</name>' +
    '      <price>4.95</price>' +
    '      <description>A tasty meal in a tablet; just add water</description>' +
    '    </item>' +
    '    <item upc="132957764" stock="44">' +
    '      <name>Grob winglets</name>' +
    '      <price>3.56</price>' +
    '      <description>Tender winglets of Grob. Just add water</description>' +
    '    </item>' +
    '  </section>' +
    '</inventory>';

var
  i: Integer;
  s: string;
  lXMLDoc: IXMLDOMDocument2;
  lNodeList: IXMLDOMNodeList;
  lNode: IXMLDOMNode;
  lItemNames: array of string;
begin
  CoInitialize(nil);
  lXMLDoc := CoDOMDocument.Create;
  lXMLDoc.setProperty('SelectionLanguage', 'XPath');
  lXMLDoc.loadXML(XML);

  Writeln('First item node:');
  lNode := lXMLDoc.selectNodes('//item')[0];
  Writeln(lNode.xml);
  Writeln('');

  lNodeList := lXMLDoc.selectNodes('//price');
  for i := 0 to lNodeList.length - 1 do
    Writeln('Price = ' + lNodeList[i].text);
  Writeln('');

  lNodeList := lXMLDoc.selectNodes('//item/name');
  SetLength(lItemNames, lNodeList.length);
  for i := 0 to lNodeList.length - 1 do
    lItemNames[i] := lNodeList[i].text;
  for s in lItemNames do
    Writeln('Item name = ' + s);
end.
```


Output:
<lang>First item node:
<item upc="123456789" stock="12">
	<name>Invisibility Cream</name>
	<price>14.50</price>
	<description>Makes you invisible</description>
</item>

Price = 14.50
Price = 23.99
Price = 4.95
Price = 3.56

Item name = Invisibility Cream
Item name = Levitation Salve
Item name = Blork and Freen Instameal
Item name = Grob winglets
```



## E


{{libheader|E-XML}} (currently a very early work in progress/draft library; design comments welcome)


```e
? def xml__quasiParser := <import:org.switchb.e.xml.makeXMLQuasiParser>()
> def xpath__quasiParser := xml__quasiParser.xPathQuasiParser()
> null

? def doc := xml`<inventory title="OmniCorp Store #45x10^3">
>   <section name="health">
>     <item upc="123456789" stock="12">
>       <name>Invisibility Cream</name>
>       <price>14.50</price>
>       <description>Makes you invisible</description>
>     </item>
>     <item upc="445322344" stock="18">
>       <name>Levitation Salve</name>
>       <price>23.99</price>
>       <description>Levitate yourself for up to 3 hours per application</description>n>
>     </item>
>   </section>
>   <section name="food">
>     <item upc="485672034" stock="653">
>       <name>Blork and Freen Instameal</name>
>       <price>4.95</price>
>       <description>A tasty meal in a tablet; just add water</description>
>     </item>
>     <item upc="132957764" stock="44">
>       <name>Grob winglets</name>
>       <price>3.56</price>
>       <description>Tender winglets of Grob. Just add water</description>
>     </item>
>   </section>
> </inventory>`
# value: xml`...`

? doc[xpath`inventory/section/item`][0]
# value: xml`<item stock="12" upc="123456789">
#              <name>Invisibility Cream</name>
#              <price>14.50</price>
#              <description>Makes you invisible</description>
#            </item>`

? for price in doc[xpath`inventory/section/item/price/text()`] { println(price :String) }
14.50
23.99
4.95
3.56

? doc[xpath`inventory/section/item/name`]
# value: [xml`<name>Invisibility Cream</name>`,
#         xml`<name>Levitation Salve</name>`,
#         xml`<name>Blork and Freen Instameal</name>`,
#         xml`<name>Grob winglets</name>`]

```



## Erlang


{{libheader|xmerl}}


```erlang

-module(xml_xpath).
-include_lib("xmerl/include/xmerl.hrl").

-export([main/0]).

main() ->
   XMLDocument =
      "<inventory title=\"OmniCorp Store #45x10^3\">
        <section name=\"health\">
          <item upc=\"123456789\" stock=\"12\">
            <name>Invisibility Cream</name>
            <price>14.50</price>
            <description>Makes you invisible</description>
          </item>
          <item upc=\"445322344\" stock=\"18\">
            <name>Levitation Salve</name>
            <price>23.99</price>
            <description>Levitate yourself for up to 3 hours per application</description>
          </item>
        </section>
        <section name=\"food\">
          <item upc=\"485672034\" stock=\"653\">
            <name>Blork and Freen Instameal</name>
            <price>4.95</price>
            <description>A tasty meal in a tablet; just add water</description>
          </item>
          <item upc=\"132957764\" stock=\"44\">
            <name>Grob winglets</name>
            <price>3.56</price>
            <description>Tender winglets of Grob. Just add water</description>
          </item>
        </section>
      </inventory>",
   {Document,_} = xmerl_scan:string(XMLDocument),

   io:format("First item:\n~s\n",
      [lists:flatten(
         xmerl:export_simple(
            [hd(xmerl_xpath:string("//item[1]", Document))],
            xmerl_xml, [{prolog, ""}]))]),

   io:format("Prices:\n"),
   [ io:format("~s\n",[Content#xmlText.value])
      || #xmlElement{content = [Content|_]} <- xmerl_xpath:string("//price", Document)],

   io:format("Names:\n"),
   [ Content#xmlText.value
      || #xmlElement{content = [Content|_]} <- xmerl_xpath:string("//name", Document)].

```


Output:
```txt
First item:
<item upc="123456789" stock="12">
            <name>Invisibility Cream</name>
            <price>14.50</price>
            <description>Makes you invisible</description>
          </item>
Prices:
14.50
23.99
4.95
3.56
Names:
["Invisibility Cream","Levitation Salve",
 "Blork and Freen Instameal","Grob winglets"]

```


=={{header|F_Sharp|F#}}==

```fsharp

open System.IO
open System.Xml.XPath

let xml = new StringReader("""
<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>
""")

let nav = XPathDocument(xml).CreateNavigator()

// first "item"; throws if none exists
let item = nav.SelectSingleNode(@"//item[1]")

// apply a operation (print text value) to all price elements
for price in nav.Select(@"//price") do
    printfn "%s" (price.ToString())

// array of all name elements
let names = seq { for name in nav.Select(@"//name") do yield name } |> Seq.toArray
```



## Factor

{{libheader|xml}}
{{libheader|xml.data}}
{{libheader|xml.traversal}}


```factor


! Get first item element
"""<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>""" string>xml "item" deep-tag-named

! Print out prices
"""<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>""" string>xml "price" deep-tags-named [ children>> first ] map

! Array of all name elements
"""<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>""" string>xml "name" deep-tags-named

```



## Gastona

The example uses the command XMELON which parses a XML file of any schema and stores its
contents into db tables. This parser is described in the article [http://web3.codeproject.com/Articles/680642/XMeLon-Schema XMeLon-Schema]

```gastona
#javaj#

   <frames> oSal, XML Path sample, 300, 400

#data#

   <xml>
      //<inventory title="OmniCorp Store #45x10^3">
      //  <section name="health">
      //    <item upc="123456789" stock="12">
      //      <name>Invisibility Cream</name>
      //      <price>14.50</price>
      //      <description>Makes you invisible</description>
      //    </item>
      //    <item upc="445322344" stock="18">
      //      <name>Levitation Salve</name>
      //      <price>23.99</price>
      //      <description>Levitate yourself for up to 3 hours per application</description>
      //    </item>
      //  </section>
      //  <section name="food">
      //    <item upc="485672034" stock="653">
      //      <name>Blork and Freen Instameal</name>
      //      <price>4.95</price>
      //      <description>A tasty meal in a tablet; just add water</description>
      //    </item>
      //    <item upc="132957764" stock="44">
      //      <name>Grob winglets</name>
      //      <price>3.56</price>
      //      <description>Tender winglets of Grob. Just add water</description>
      //    </item>
      //  </section>
      //</inventory>

   <DEEP_SQL_XML>
      DEEP DB, SELECT, xmelon_data
             ,, path pathStr
             ,, tag tagStr
             ,, patCnt
             ,, dataPlace
             ,, value

#listix#

   <main>
      //parsing xml data ...
      GEN, :mem datos, xml
      XMELON, FILE2DB, :mem datos
      //
      //first item ...
      //
      //
      LOOP, SQL,, //SELECT patCnt AS patITEM1 FROM (@<DEEP_SQL_XML>) WHERE path_pathStr == '/inventory/section/item' LIMIT 1
          ,HEAD, //<item
          ,, LOOP, SQL,, //SELECT * FROM (@<DEEP_SQL_XML>) WHERE patCnt == @<patITEM1> AND dataPlace == 'A'
          ,,     , LINK, ""
          ,,     ,, // @<tag_tagStr>="@<value>"
          ,, //>
          ,, //
          ,, LOOP, SQL,, //SELECT * FROM (@<DEEP_SQL_XML>) WHERE patCnt == @<patITEM1> AND dataPlace != 'A'
          ,,     ,, //    <@<tag_tagStr>>@<value></@<tag_tagStr>>
          ,TAIL, //
          ,TAIL, //</item>
      //
      //
      //report prices ...
      //
      LOOP, SQL,, //SELECT value FROM (@<DEEP_SQL_XML>) WHERE tag_tagStr == 'price'
          , LINK, ", "
          ,, @<value>
      //
      //put names into a variable
      //
      VAR=, tabnames, "name"
      LOOP, SQL,, //SELECT value FROM (@<DEEP_SQL_XML>) WHERE tag_tagStr == 'name'
          , LINK, ""
          ,, VAR+, tabnames, @<value>
      DUMP, data,, tabnames

```


{{out|Output}}

```txt

parsing xml data ...
first item ...

<item upc="123456789" stock="12">
    <name>Invisibility Cream</name>
    <price>14.50</price>
    <description>Makes you invisible</description>
</item>

report prices ...
14.50, 23.99, 4.95, 3.56
names into a variable
#data#

   <tabnames>
      //name
      //health
      //Invisibility Cream
      //Levitation Salve
      //food
      //Blork and Freen Instameal
      //Grob winglets

```


## Go

Using the standard <code>encoding/xml</code> package:

```go
package main

import (
	"encoding/xml"
	"fmt"
	"log"
	"os"
)

type Inventory struct {
	XMLName  xml.Name `xml:"inventory"`
	Title    string   `xml:"title,attr"`
	Sections []struct {
		XMLName xml.Name `xml:"section"`
		Name    string   `xml:"name,attr"`
		Items   []struct {
			XMLName     xml.Name `xml:"item"`
			Name        string   `xml:"name"`
			UPC         string   `xml:"upc,attr"`
			Stock       int      `xml:"stock,attr"`
			Price       float64  `xml:"price"`
			Description string   `xml:"description"`
		} `xml:"item"`
	} `xml:"section"`
}

// To simplify main's error handling
func printXML(s string, v interface{}) {
	fmt.Println(s)
	b, err := xml.MarshalIndent(v, "", "\t")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(b))
	fmt.Println()
}

func main() {
	fmt.Println("Reading XML from standard input...")

	var inv Inventory
	dec := xml.NewDecoder(os.Stdin)
	if err := dec.Decode(&inv); err != nil {
		log.Fatal(err)
	}

	// At this point, inv is Go struct with all the fields filled
	// in from the XML data. Well-formed XML input that doesn't
	// match the specification of the fields in the Go struct are
	// discarded without error.

	// We can reformat the parts we parsed:
	//printXML("Got:", inv)

	// 1. Retrieve first item:
	item := inv.Sections[0].Items[0]
	fmt.Println("item variable:", item)
	printXML("As XML:", item)

	// 2. Action on each price:
	fmt.Println("Prices:")
	var totalValue float64
	for _, s := range inv.Sections {
		for _, i := range s.Items {
			fmt.Println(i.Price)
			totalValue += i.Price * float64(i.Stock)
		}
	}
	fmt.Println("Total inventory value:", totalValue)
	fmt.Println()

	// 3. Slice of all the names:
	var names []string
	for _, s := range inv.Sections {
		for _, i := range s.Items {
			names = append(names, i.Name)
		}
	}
	fmt.Printf("names: %q\n", names)
}
```

{{out}}

```txt

Reading XML from standard input...
item variable: {{ item} Invisibility Cream 123456789 12 14.5 Makes you invisible}
As XML:
<item upc="123456789" stock="12">
	<name>Invisibility Cream</name>
	<price>14.5</price>
	<description>Makes you invisible</description>
</item>

Prices:
14.5
23.99
4.95
3.56
Total inventory value: 3994.81

names: ["Invisibility Cream" "Levitation Salve" "Blork and Freen Instameal" "Grob winglets"]

```


{{libheader|xmlpath}}

```go
package main

import (
    "fmt"
    "os"

    "launchpad.net/xmlpath"
)

func main() {
    f, err := os.Open("test3.xml")
    if err != nil {
        fmt.Println(err)
        return
    }
    n, err := xmlpath.Parse(f)
    f.Close()
    if err != nil {
        fmt.Println(err)
        return
    }
    q1 := xmlpath.MustCompile("//item")
    if _, ok := q1.String(n); !ok {
        fmt.Println("no item")
    }
    q2 := xmlpath.MustCompile("//price")
    for it := q2.Iter(n); it.Next(); {
        fmt.Println(it.Node())
    }
    q3 := xmlpath.MustCompile("//name")
    names := []*xmlpath.Node{}
    for it := q3.Iter(n); it.Next(); {
        names = append(names, it.Node())
    }
    if len(names) == 0 {
        fmt.Println("no names")
    }
}
```

{{out}}

```txt

14.50
23.99
4.95
3.56

```



## Groovy


```groovy
def inventory = new XmlSlurper().parseText("<inventory...")    //optionally parseText(new File("inv.xml").text)
def firstItem = inventory.section.item[0]                      //1. first item
inventory.section.item.price.each { println it }               //2. print each price
def allNamesArray = inventory.section.item.name.collect {it}   //3. collect item names into an array
```



## Haskell


```haskell
import Data.List
import Control.Arrow
import Control.Monad

takeWhileIncl           :: (a -> Bool) -> [a] -> [a]
takeWhileIncl _ []      =  []
takeWhileIncl p (x:xs)
            | p x       =  x : takeWhileIncl p xs
            | otherwise =  [x]

getmultiLineItem n = takeWhileIncl(not.isInfixOf ("</" ++ n)). dropWhile(not.isInfixOf ('<': n))
getsingleLineItems n = map (takeWhile(/='<'). drop 1. dropWhile(/='>')). filter (isInfixOf ('<': n))

main = do
  xml <- readFile "./Rosetta/xmlpath.xml"
  let xmlText = lines xml

  putStrLn "\n== First item ==\n"
  mapM_ putStrLn $ head $ unfoldr (Just. liftM2 (id &&&) (\\) (getmultiLineItem "item")) xmlText

  putStrLn "\n== Prices ==\n"
  mapM_ putStrLn $ getsingleLineItems "price" xmlText

  putStrLn "\n== Names ==\n"
  print $ getsingleLineItems "name" xmlText
```

Using the Haskell XML Toolkit (HXT):

```haskell
{-# LANGUAGE Arrows #-}
import Text.XML.HXT.Arrow
{- For HXT version >= 9.0, use instead:
import Text.XML.HXT.Core
-}

deepElem name = deep (isElem >>> hasName name)

process = proc doc -> do
  item <- single (deepElem "item") -< doc
  _ <- listA (arrIO print <<< deepElem "price") -< doc
  names <- listA (deepElem "name") -< doc
  returnA -< (item, names)

main = do
  [(item, names)] <- runX (readDocument [] "xmlpath.xml" >>> process)
  print item
  print names
```



## HicEst


```hicest
CHARACTER xml*1000, output*1000
  READ(ClipBoard) xml

  EDIT(Text=xml, Right='<item', Right=5, GetPosition=a, Right='</item>', Left, GetPosition=z)
  WRITE(Text=output) xml( a : z), $CRLF

  i = 1
1 EDIT(Text=xml, SetPosition=i, SePaRators='<>', Right='<price>', Word=1, Parse=price, GetPosition=i, ERror=99)
  IF(i > 0) THEN
      WRITE(Text=output, APPend)  'Price element = ', price, $CRLF
      GOTO 1  !  HicEst does not have a "WHILE"
  ENDIF

  EDIT(Text=xml, SPR='<>', R='<name>', W=1, WordEnd=$CR, APpendTo=output, DO=999)
  WRITE(ClipBoard) TRIM(output)
```


```hicest
 upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>

Price element = 14.50
Price element = 23.99
Price element = 4.95
Price element = 3.56
Invisibility Cream
 Levitation Salve
 Blork and Freen Instameal
 Grob winglets

```



## Java


```java
import java.io.StringReader;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

public class XMLParser {
	final static String xmlStr =
			  "<inventory title=\"OmniCorp Store #45x10^3\">"
			+ "  <section name=\"health\">"
			+ "    <item upc=\"123456789\" stock=\"12\">"
			+ "      <name>Invisibility Cream</name>"
			+ "      <price>14.50</price>"
			+ "      <description>Makes you invisible</description>"
			+ "    </item>"
			+ "    <item upc=\"445322344\" stock=\"18\">"
			+ "      <name>Levitation Salve</name>"
			+ "      <price>23.99</price>"
			+ "      <description>Levitate yourself for up to 3 hours per application</description>"
			+ "    </item>"
			+ "  </section>"
			+ "  <section name=\"food\">"
			+ "    <item upc=\"485672034\" stock=\"653\">"
			+ "      <name>Blork and Freen Instameal</name>"
			+ "      <price>4.95</price>"
			+ "      <description>A tasty meal in a tablet; just add water</description>"
			+ "    </item>"
			+ "    <item upc=\"132957764\" stock=\"44\">"
			+ "      <name>Grob winglets</name>"
			+ "      <price>3.56</price>"
			+ "      <description>Tender winglets of Grob. Just add priwater</description>"
			+ "    </item>"
			+ "  </section>"
			+ "</inventory>";

	public static void main(String[] args) {
		try {
			Document doc = DocumentBuilderFactory.newInstance()
					.newDocumentBuilder()
					.parse(new InputSource(new StringReader(xmlStr)));
			XPath xpath = XPathFactory.newInstance().newXPath();
			// 1
			System.out.println(((Node) xpath.evaluate(
					"/inventory/section/item[1]", doc, XPathConstants.NODE))
					.getAttributes().getNamedItem("upc"));
			// 2, 3
			NodeList nodes = (NodeList) xpath.evaluate(
					"/inventory/section/item/price", doc,
					XPathConstants.NODESET);
			for (int i = 0; i < nodes.getLength(); i++)
				System.out.println(nodes.item(i).getTextContent());
		} catch (Exception e) {
			System.out.println("Error ocurred while parsing XML.");
		}
	}
}
```


## JavaScript

{{works with|Firefox|2.0}}

```javascript
//create XMLDocument object from file
var xhr = new XMLHttpRequest();
xhr.open('GET', 'file.xml', false);
xhr.send(null);
var doc = xhr.responseXML;

//get first <item> element
var firstItem = doc.evaluate( '//item[1]', doc, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null ).singleNodeValue;
alert( firstItem.textContent );

//output contents of <price> elements
var prices = doc.evaluate( '//price', doc, null, XPathResult.ANY_TYPE, null );
for( var price = prices.iterateNext(); price != null; price = prices.iterateNext() ) {
  alert( price.textContent );
}

//add <name> elements to array
var names = doc.evaluate( '//name', doc, null, XPathResult.ANY_TYPE, null);
var namesArray = [];
for( var name = names.iterateNext(); name != null; name = names.iterateNext() ) {
  namesArray.push( name );
}
alert( namesArray );
```


Although some browsers support XPath, working with XML is much easier with E4X.


```javascript
//create XML object from file
var xhr = new XMLHttpRequest();
xhr.open('GET', 'file.xml', false);
xhr.send(null);
var doc = new XML(xhr.responseText);

//get first <item> element
var firstItem = doc..item[0];
alert( firstItem );

//output contents of <price> elements
for each( var price in doc..price ) {
  alert( price );
}

//add <name> elements to array
var names = [];
for each( var name in doc..name ) {
  names.push( name );
}
alert( names );
```



## Julia

Uses the LibExpat module for XML pathing. The exercise description is very
vague about output format, so this is varied in the solution below. The first
test prints the raw XML node, and the second and third are further processed.

```julia
using LibExpat

xdoc = raw"""<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>
"""

debracket(s) = replace(s, r".+\>(.+)\<.+" => s"\1")

etree = xp_parse(xdoc)
firstshow = LibExpat.find(etree, "//item")[1]
println("The first item's node XML entry is:\n", firstshow, "\n\n")

prices = LibExpat.find(etree, "//price")
println("Prices:")
for p in prices
    println("\t", debracket(string(p)))
end
println("\n")

namearray = LibExpat.find(etree, "//name")
println("Array of names of items:\n\t", map(s -> debracket(string(s)), namearray))

```
{{output}}
```txt

 The first item's node XML entry is:
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>


 Prices:
        14.50
        23.99
        4.95
        3.56


 Array of names of items:
        ["Invisibility Cream", "Levitation Salve", "Blork and Freen Instameal", "Grob winglets"]

```



## Kotlin


```scala
// version 1.1.3

import javax.xml.parsers.DocumentBuilderFactory
import org.xml.sax.InputSource
import java.io.StringReader
import javax.xml.xpath.XPathFactory
import javax.xml.xpath.XPathConstants
import org.w3c.dom.Node
import org.w3c.dom.NodeList

val xml =
"""
<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>
"""

fun main(args: Array<String>) {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder  = dbFactory.newDocumentBuilder()
    val xmlInput = InputSource(StringReader(xml))
    val doc = dBuilder.parse(xmlInput)
    val xpFactory = XPathFactory.newInstance()
    val xPath = xpFactory.newXPath()

    val qNode = xPath.evaluate("/inventory/section/item[1]", doc, XPathConstants.NODE) as Node
    val upc = qNode.attributes.getNamedItem("upc")
    val stock = qNode.attributes.getNamedItem("stock")
    println("For the first item :  upc = ${upc.textContent} and stock = ${stock.textContent}")

    val qNodes = xPath.evaluate("/inventory/section/item/price", doc, XPathConstants.NODESET) as NodeList
    print("\nThe prices of each item are : ")
    for (i in 0 until qNodes.length) print("${qNodes.item(i).textContent}  ")
    println()

    val qNodes2 = xPath.evaluate("/inventory/section/item/name", doc, XPathConstants.NODESET) as NodeList
    val names = Array<String>(qNodes2.length) { qNodes2.item(it).textContent }
    println("\nThe names of each item are as follows :")
    println("  ${names.joinToString("\n  ")}")
}
```


{{out}}

```txt

For the first item :  upc = 123456789 and stock = 12

The prices of each item are : 14.50  23.99  4.95  3.56

The names of each item are as follows :
  Invisibility Cream
  Levitation Salve
  Blork and Freen Instameal
  Grob winglets

```



## Lasso

Lasso has built in support for both XML handling and Xpaths

```Lasso
// makes extracting attribute values easier
define xml_attrmap(in::xml_namedNodeMap_attr) => {
	local(out = map)
	with attr in #in
		do #out->insert(#attr->name = #attr->value)
	return #out
}

local(
	text = '<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>
',
xml = xml(#text)
)

local(
	items		= #xml -> extract('//item'),
	firstitem	= #items -> first,
	itemattr	= xml_attrmap(#firstitem -> attributes),
	newprices	= array
)

'<strong>First item:</strong><br />
UPC: '
#itemattr -> find('upc')
' (stock: '
#itemattr -> find('stock')
')<br />'
#firstitem -> extractone('name') -> nodevalue
' ['
#firstitem -> extractone('price') -> nodevalue
'] ('
#firstitem -> extractone('description') -> nodevalue
')<br /><br />'

with item in #items
let name = #item -> extractone('name') -> nodevalue
let price = #item -> extractone('price') -> nodevalue
do {
	#newprices -> insert(#name + ': ' + (decimal(#price) * 1.10) -> asstring(-precision = 2) + ' (' + #price + ')')
}
'<strong>Adjusted prices:</strong><br />'
#newprices -> join('<br />')
'<br /><br />'
'<strong>Array with all names:</strong><br />'
#xml -> extract('//name') -> asstaticarray
```

Output:

```txt
First item:
UPC: 123456789 (stock: 12)
Invisibility Cream [14.50] (Makes you invisible)

Adjusted prices:
Invisibility Cream: 15.95 (14.50)
Levitation Salve: 26.39 (23.99)
Blork and Freen Instameal: 5.45 (4.95)
Grob winglets: 3.92 (3.56)

Array with all names:
staticarray(Invisibility Cream, Levitation Salve, Blork and Freen Instameal, Grob winglets)
```



## LiveCode

Copy the xml in this task into a text field called "FieldXML"

```LiveCode
put revXMLCreateTree(fld "FieldXML",true,true,false) into xmltree

// task 1
put revXMLEvaluateXPath(xmltree,"//item[1]") into nodepath
put revXMLText(xmltree,nodepath,true)

// task 2
put revXMLDataFromXPathQuery(xmltree,"//item/price",,comma)

// task 3
put revXMLDataFromXPathQuery(xmltree,"//name") into namenodes
filter namenodes without empty
split namenodes using cr
put namenodes is an array
```



## Lua

Requires LuaExpat

```lua
require 'lxp'
data = [[<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>]]
local first = true
local names, prices = {}, {}
p = lxp.new({StartElement = function (parser, name)
	local a, b, c = parser:pos() --line, offset, pos
	if name == 'item' and first then
		print(data:match('.-</item>', c - b + 1))
		first = false
	end
	if name == 'name' then names[#names+1] = data:match('>(.-)<', c) end
	if name == 'price' then prices[#prices+1] = data:match('>(.-)<', c) end
end})

p:parse(data)
p:close()

print('Name: ', table.concat(names, ', '))
print('Price: ', table.concat(prices, ', '))
```

Output:
```txt

    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
Name: 	Invisibility Cream, Levitation Salve, Blork and Freen Instameal, Grob winglets
Price: 	14.50, 23.99, 4.95, 3.56

```



## Mathematica


```Mathematica
example = Import["test.txt", "XML"];
Cases[example, XMLElement["item", _ , _] , Infinity] // First
Cases[example, XMLElement["price", _, List[n_]] -> n, Infinity] // Column
Cases[example, XMLElement["name", _, List[n_]] -> n, Infinity] // Column
```

Output:
```txt

XMLElement[item,{upc->123456789,stock->12},
{XMLElement[name,{},{Invisibility Cream}],XMLElement[price,{},{14.50}],XMLElement[description,{},{Makes you invisible}]}]

14.50
23.99
4.95
3.56

Invisibility Cream
Levitation Salve
Blork and Freen Instameal
Grob winglets

```



## NetRexx

{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import javax.xml.parsers.
import javax.xml.xpath.
import org.w3c.dom.
import org.xml.sax.

xmlStr = '' -
  || '<inventory title="OmniCorp Store #45x10^3">' -
  || '  <section name="health">' -
  || '    <item upc="123456789" stock="12">' -
  || '      <name>Invisibility Cream</name>' -
  || '      <price>14.50</price>' -
  || '      <description>Makes you invisible</description>' -
  || '    </item>' -
  || '    <item upc="445322344" stock="18">' -
  || '      <name>Levitation Salve</name>' -
  || '      <price>23.99</price>' -
  || '      <description>Levitate yourself for up to 3 hours per application</description>' -
  || '    </item>' -
  || '  </section>' -
  || '  <section name="food">' -
  || '    <item upc="485672034" stock="653">' -
  || '      <name>Blork and Freen Instameal</name>' -
  || '      <price>4.95</price>' -
  || '      <description>A tasty meal in a tablet; just add water</description>' -
  || '    </item>' -
  || '    <item upc="132957764" stock="44">' -
  || '      <name>Grob winglets</name>' -
  || '      <price>3.56</price>' -
  || '      <description>Tender winglets of Grob. Just add priwater</description>' -
  || '    </item>' -
  || '  </section>' -
  || '</inventory>'

expr1 = '/inventory/section/item[1]'
expr2 = '/inventory/section/item/price'
expr3 = '/inventory/section/item/name'
attr1 = 'upc'

do
  doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(InputSource(StringReader(xmlStr)))
  xpath = XPathFactory.newInstance().newXPath()

  -- Extract attribute from 1st item element
  say expr1
  say "  "(Node xpath.evaluate(expr1, doc, XPathConstants.NODE)).getAttributes().getNamedItem(attr1)
  say

  -- Extract and display all price elments
  nodes = NodeList xpath.evaluate(expr2, doc, XPathConstants.NODESET)
  say expr2
  loop i_ = 0 to nodes.getLength() - 1
    say Rexx(nodes.item(i_).getTextContent()).format(10, 2)
    end i_
  say

  -- Extract elements and store in an ArrayList
  nameList = java.util.List
  nameList = ArrayList()
  nodes = NodeList xpath.evaluate(expr3, doc, XPathConstants.NODESET)
  loop i_ = 0 to nodes.getLength() - 1
    nameList.add(nodes.item(i_).getTextContent())
    end i_

  -- display contents of ArrayList
  say expr3
  loop n_ = 0 to nameList.size() - 1
    say "  "nameList.get(n_)
    end n_
  say

catch ex = Exception
  ex.printStackTrace()
end

return

```

'''Output:'''

```txt

/inventory/section/item[1]
  upc="123456789"

/inventory/section/item/price
        14.50
        23.99
         4.95
         3.56

/inventory/section/item/name
  Invisibility Cream
  Levitation Salve
  Blork and Freen Instameal
  Grob winglets

```



## Nim


```nim
import xmldom, xmldomparser

let doc = "test3.xml".loadXMLFile.documentElement

# 1st task: retrieve the first "item" element
let i = doc.getElementsByTagName("item")[0]

# 2nd task: perform an action on each "price" element (print it out)
for j in doc.getElementsByTagName "price":
  echo j.firstChild.PText.data

# 3rd task: get an array of all the "name" elements
let namesArray = doc.getElementsByTagName "name"
```



## Objeck

XPath is used to fetch element tags.

```objeck

use XML;

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      in := String->New();
      in->Append("<inventory title=\"OmniCorp Store #45x10^3\">");
      in->Append("<section name=\"health\">");
      in->Append("<item upc=\"123456789\" stock=\"12\">");
      in->Append("<name>Invisibility Cream</name>");
      in->Append("<price>14.50</price>");
      in->Append("<description>Makes you invisible</description>");
      in->Append("</item>");
      in->Append("<item upc=\"445322344\" stock=\"18\">");
      in->Append("<name>Levitation Salve</name>");
      in->Append("<price>23.99</price>");
      in->Append("<description>Levitate yourself for up to 3 hours per application</description>");
      in->Append("</item>");
      in->Append("</section>");
      in->Append("<section name=\"food\">");
      in->Append("<item upc=\"485672034\" stock=\"653\">");
      in->Append("<name>Blork and Freen Instameal</name>");
      in->Append("<price>4.95</price>");
      in->Append("<description>A tasty meal in a tablet; just add water</description>");
      in->Append("</item>");
      in->Append("<item upc=\"132957764\" stock=\"44\">");
      in->Append("<name>Grob winglets</name>");
      in->Append("<price>3.56</price>");
      in->Append("<description>Tender winglets of Grob. Just add water</description>");
      in->Append("</item>");
      in->Append("</section>");
      in->Append("</inventory>");

      parser := XmlParser->New(in);
      if(parser->Parse()) {
        # get first item
        results := parser->FindElements("//inventory/section[1]/item[1]");
        if(results <> Nil) {
          IO.Console->Instance()->Print("items: ")->PrintLine(results->Size());
        };
        # get all prices
        results := parser->FindElements("//inventory/section/item/price");
        if(results <> Nil) {
          each(i : results) {
            element := results->Get(i)->As(XMLElement);
            element->GetContent()->PrintLine();
          };
        };
        # get names
        results := parser->FindElements("//inventory/section/item/name");
        if(results <> Nil) {
          IO.Console->Instance()->Print("names: ")->PrintLine(results->Size());
        };
      };
    }
  }
}

```



## Oz

We implement a small subset of XPath for this task:

```oz
declare
  [XMLParser] = {Module.link ['x-oz://system/xml/Parser.ozf']}

  proc {Main Data}
     Parser = {New XMLParser.parser init}
     [Doc] = {Parser parseVS(Data $)}

     FirstItem = {XPath Doc [inventory section item]}.1

     Prices = {XPath Doc [inventory section item price Text]}

     Names = {XPath Doc [inventory section item name]}
  in
     {ForAll Prices System.showInfo}
  end

  %%
  %% Emulation of some XPath functionality:
  %%

  fun {XPath Doc Path}
     P|Pr = Path
  in
     Doc.name = P %% assert
     {FoldL Pr XPathStep [Doc]}
  end

  fun {XPathStep Elements P}
     if {IsProcedure P} then
        {Map Elements P}
     else
        {FilteredChildren Elements P}
     end
  end

  %% A flat list of all Type-children of all Elements.
  fun {FilteredChildren Elements Type}
     {Flatten
      {Map Elements
       fun {$ E}
	  {Filter E.children
	   fun {$ X}
	      case X of element(name:!Type ...) then true
	      else false
	      end
	   end}
       end}}
  end

  %% PCDATA of an element as a ByteString
  fun {Text Element}
     Texts = for Child in Element.children collect:C do
		case Child of text(data:BS ...) then {C BS} end
	     end
  in
     {FoldR Texts ByteString.append {ByteString.make nil}}
  end

  Data =
   "<inventory title=\"OmniCorp Store #45x10^3\">"
  #"  <section name=\"health\">"
  #"    <item upc=\"123456789\" stock=\"12\">"
  #"      <name>Invisibility Cream</name>"
  #"      <price>14.50</price>"
  #"      <description>Makes you invisible</description>"
  #"    </item>"
  #"    <item upc=\"445322344\" stock=\"18\">"
  #"      <name>Levitation Salve</name>"
  #"      <price>23.99</price>"
  #"      <description>Levitate yourself for up to 3 hours per application</description>"
  #"    </item>"
  #"  </section>"
  #"  <section name=\"food\">"
  #"    <item upc=\"485672034\" stock=\"653\">"
  #"      <name>Blork and Freen Instameal</name>"
  #"      <price>4.95</price>"
  #"      <description>A tasty meal in a tablet; just add water</description>"
  #"    </item>"
  #"    <item upc=\"132957764\" stock=\"44\">"
  #"      <name>Grob winglets</name>"
  #"      <price>3.56</price>"
  #"      <description>Tender winglets of Grob. Just add water</description>"
  #"    </item>"
  #"  </section>"
  #"</inventory>"
in
  {Main Data}
```



## Perl

{{libheader|XML::XPath}}

```perl
use XML::XPath qw();

my $x = XML::XPath->new('<inventory ... </inventory>');

[$x->findnodes('//item[1]')->get_nodelist]->[0];
print $x->findnodes_as_string('//price');
$x->findnodes('//name')->get_nodelist;
```



## Perl 6



```perl6
use XML::XPath;

my $XML = XML::XPath.new(xml => q:to/END/);
<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>
END

put "First item:\n", $XML.find('//item[1]')[0];

put "\nPrice elements:";
.contents.put for $XML.find('//price').List;

put "\nName elements:\n", $XML.find('//name')».contents.join: ', ';
```

{{out}}

```txt
First item:
<item upc="123456789" stock="12"> <name>Invisibility Cream</name>  <price>14.50</price>  <description>Makes you invisible</description>  </item>

Price elements:
14.50
23.99
4.95
3.56

Name elements:
Invisibility Cream, Levitation Salve, Blork and Freen Instameal, Grob winglets
```



## PHP


```php
<?php
//PHP5 only example due to changes in XML extensions between version 4 and 5 (Tested on PHP5.2.0)
$doc = DOMDocument::loadXML('<inventory title="OmniCorp Store #45x10^3">...</inventory>');
//Load from file instead with $doc = DOMDocument::load('filename');
$xpath = new DOMXPath($doc);
/*
    1st Task: Retrieve the first "item" element
*/
$nodelist = $xpath->query('//item');
$result = $nodelist->item(0);
/*
    2nd task: Perform an action on each "price" element (print it out)
*/
$nodelist = $xpath->query('//price');
for($i = 0; $i < $nodelist->length; $i++)
{
  //print each price element in the DOMNodeList instance, $nodelist, as text/xml followed by a newline
  print $doc->saveXML($nodelist->item($i))."\n";
}
/*
    3rd Task: Get an array of all the "name" elements
*/
$nodelist = $xpath->query('//name');
//our array to hold all the name elements, though in practice you'd probably not need to do this and simply use the DOMNodeList
$result = array();
//a different way of iterating through the DOMNodeList
foreach($nodelist as $node)
{
  $result[] = $node;
}
```



## PicoLisp


```PicoLisp
(load "@lib/xm.l")

(let Sections (body (in "file.xml" (xml)))
   (pretty (car (body (car Sections))))
   (prinl)
   (for S Sections
      (for L (body S)
         (prinl (car (body L 'price))) ) )
   (make
      (for S Sections
         (for L (body S)
            (link (car (body L 'name))) ) ) ) )
```

Output:

```txt
(item
   ((upc . "123456789") (stock . "12"))
   (name NIL "Invisibility Cream")
   (price NIL "14.50")
   (description NIL "Makes you invisible") )
14.50
23.99
4.95
3.56
-> ("Invisibility Cream" "Levitation Salve" "Blork and Freen Instameal" "Grob winglets")
```



## PowerShell

Cast the <code>$document</code> string as <code>[xml]</code> and you have access to .NET methods affecting XML.

```PowerShell

$document = [xml]@'
<inventory title="OmniCorp Store #45x10^3">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>
'@

$query = "/inventory/section/item"
$items = $document.SelectNodes($query)

```

The first item:

```PowerShell

$items[0]

```

{{Out}}

```txt

upc         : 123456789
stock       : 12
name        : Invisibility Cream
price       : 14.50
description : Makes you invisible

```

Get some useful information:

```PowerShell

$namesAndPrices = $items | Select-Object -Property name, price
$namesAndPrices

```

{{Out}}

```txt

name                      price
----                      -----
Invisibility Cream        14.50
Levitation Salve          23.99
Blork and Freen Instameal 4.95
Grob winglets             3.56

```

Here are the prices:

```PowerShell

$items.price

```

{{Out}}

```txt

14.50
23.99
4.95
3.56

```

Here are the names:

```PowerShell

$items.name

```

{{Out}}

```txt

Invisibility Cream
Levitation Salve
Blork and Freen Instameal
Grob winglets

```



## Python


```python
# Python has basic xml parsing built in

from xml.dom import minidom

xmlfile = file("test3.xml") # load xml document from file
xmldoc = minidom.parse(xmlfile).documentElement # parse from file stream or...
xmldoc = minidom.parseString("<inventory title="OmniCorp Store #45x10^3">...</inventory>").documentElement # alternatively, parse a string

#  1st Task: Retrieve the first "item" element
i = xmldoc.getElementsByTagName("item") # get a list of all "item" tags
firstItemElement = i[0] # get the first element

# 2nd task: Perform an action on each "price" element (print it out)
for j in xmldoc.getElementsByTagName("price"): # get a list of all "price" tags
	print j.childNodes[0].data # XML Element . TextNode . data of textnode

# 3rd Task: Get an array of all the "name" elements
namesArray = xmldoc.getElementsByTagName("name")
```

In Python 2.5+ you can use ElementTree's limited XPath support

```python

import xml.etree.ElementTree as ET

xml = open('inventory.xml').read()
doc = ET.fromstring(xml)

doc = ET.parse('inventory.xml')  # or load it directly

# Note, ElementTree's root is the top level element. So you need ".//" to really start searching from top

# Return first Item
item1 = doc.find("section/item")  # or ".//item"

# Print each price
for p in doc.findall("section/item/price"):  # or ".//price"
    print "{0:0.2f}".format(float(p.text))  # could raise exception on missing text or invalid float() conversion

# list of names
names = doc.findall("section/item/name")  # or ".//name"
```

Or, you can install the <tt>lxml</tt> package and get full XPath support

```python

from lxml import etree

xml = open('inventory.xml').read()
doc = etree.fromstring(xml)

doc = etree.parse('inventory.xml')  # or load it directly

# Return first item
item1 = doc.xpath("//section[1]/item[1]")

# Print each price
for p in doc.xpath("//price"):
    print "{0:0.2f}".format(float(p.text))  # could raise exception on missing text or invalid float() conversion

names = doc.xpath("//name")  # list of names
```



## R

{{libheader|XML (R)}}

```R
## Require the XML package you can download from http://www.omegahat.org/RSXML/
library("XML")
doc <- xmlInternalTreeParse("test3.xml")
#  1st Task: Retrieve the first "item" element
(firstItemElement <- getNodeSet(doc, "//item")[[1]])
# 2nd task: Perform an action on each "price" element (print it out)
prices <- sapply(getNodeSet(doc, "//price"), xmlValue)
for(i in 1:length(prices)) print(prices[i])
# 3rd Task: Get an array of all the "name" elements
(namesArray <- sapply(getNodeSet(doc, "//name"), xmlValue))
```



## Racket


```racket

#lang at-exp racket

(define input @~a{
  <inventory title="OmniCorp Store #45x10^3">
    <section name="health">
      <item upc="123456789" stock="12">
        <name>Invisibility Cream</name>
        <price>14.50</price>
        <description>Makes you invisible</description>
      </item>
      <item upc="445322344" stock="18">
        <name>Levitation Salve</name>
        <price>23.99</price>
        <description>Levitate yourself for up to 3 hours per application</description>
      </item>
    </section>
    <section name="food">
      <item upc="485672034" stock="653">
        <name>Blork and Freen Instameal</name>
        <price>4.95</price>
        <description>A tasty meal in a tablet; just add water</description>
      </item>
      <item upc="132957764" stock="44">
        <name>Grob winglets</name>
        <price>3.56</price>
        <description>Tender winglets of Grob. Just add water</description>
      </item>
    </section>
  </inventory>})

(require xml xml/path)

(define data (xml->xexpr
              ((eliminate-whitespace '(inventory section item))
               (read-xml/element (open-input-string input)))))

;; Retrieve the first "item" element
(displayln (xexpr->string (se-path* '(item) data)))
;; => <name>Invisibility Cream</name>

;; Perform an action on each "price" element (print it out)
(printf "Prices: ~a\n" (string-join (se-path*/list '(item price) data) ", "))
;; => Prices: 14.50, 23.99, 4.95, 3.56

;; Get an array of all the "name" elements
(se-path*/list '(item name) data)
;; => '("Invisibility Cream" "Levitation Salve" "Blork and Freen Instameal" "Grob winglets")

```



## Rascal


```rascal
import lang::xml::DOM;
import Prelude;

public void get_first_item(loc a){
	D = parseXMLDOM(readFile(a));
	top-down-break visit(D){
		case E:element(_,"item",_): return println(xmlPretty(E));
	};
}

public void print_prices(loc a){
	D = parseXMLDOM(readFile(a));
	for(/element(_,"price",[charData(/str p)]) := D)
		println(p);
}

public list[str] get_names(loc a){
	D = parseXMLDOM(readFile(a));
	L = [];
	for(/element(_,"name",[charData(/str n)]) := D)
		L += n;
	return L;
}
```

Example output:

```rascal>rascal
get_first_item(|file:///Users/.../Desktop/xmlpath.xml|)
<?xml version="1.0" encoding="UTF-8"?>
<item stock="12" upc="123456789">
  <name>Invisibility Cream</name>
  <price>14.50</price>
  <description>Makes you invisible</description>
</item>


ok

rascal>print_prices(|file:///Users/.../Desktop/xmlpath.xml|)
14.50
23.99
4.95
3.56
ok

rascal>get_names(|file:///Users/.../Desktop/xmlpath.xml|)
list[str]: ["Invisibility Cream","Levitation Salve","Blork and Freen Instameal","Grob winglets"]
```



## REXX


### hard coded parsing


```rexx
/*REXX program to parse various queries on an XML document  (from a file).    */
iFID='XPATH.XML'                       /*name of the input  XML  file (doc).  */
$=                                     /*string will contain the file's text. */
     do j=1  while  lines(iFID)\==0    /*read the entire file into a string.  */
     $=$ linein(iFID)                  /*append the line to the  $  string.   */
     end   /*j*/
                                       /* [↓]  show 1st  ITEM  in the document*/
parse var $  '<item '  item  "</item>"
say center('first item:',length(space(item)),'─')     /*display a nice header.*/
say space(item)
                                       /* [↓]  show all PRICES in the document*/
prices=                                /*nullify the list and add/append to it*/
$$=$                                   /*start with a fresh copy of document. */
     do  until $$=''                   /* [↓]  keep parsing string until done.*/
     parse var $$  '<price>'   price   '</price>' $$
     prices=prices price               /*add/append the price to the list.    */
     end   /*until*/
say
say center('prices:',length(space(prices)),'─')       /*display a nice header.*/
say space(prices)
                                       /* [↓]  show all  NAMES in the document*/
names.=                                /*nullify the list and add/append to it*/
L=length(' names: ')                   /*maximum length of any one list name. */
$$=$                                   /*start with a fresh copy of document. */
     do #=1  until $$=''               /* [↓]  keep parsing string until done.*/
     parse var $$  '<name>'   names.#   '</name>'   $$
     L=max(L,length(names.#))          /*L:  is used to find the widest name. */
     end   /*#*/

names.0=#-1;                  say      /*adjust the number of names (DO loop).*/
say center('names:',L,'─')             /*display a nicely formatted header.   */
     do k=1  for names.0               /*display all the names in the list.   */
     say names.k                       /*display a name from the  NAMES  list.*/
     end   /*k*/
                                       /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

──────────────────────────────────────────────────────────first item:──────────────────────────────────────────────────────────
upc="123456789" stock="12"> <name>Invisibility Cream</name> <price>14.50</price> <description>Makes you invisible</description>

───────prices:───────
14.50 23.99 4.95 3.56

─────────names:──────────
Invisibility Cream
Levitation Salve
Blork and Freen Instameal
Grob winglets

```



### generic parsing


```rexx
/*REXX program to parse various queries on an XML document  (from a file).    */
iFID='XPATH.XML'                       /*name of the input  XML  file (doc).  */
$=                                     /*string will contain the file's text. */
     do j=1  while  lines(iFID)\==0    /*read the entire file into a string.  */
     $=$ linein(iFID)                  /*append the line to the  $  string.   */
     end   /*j*/
                                       /* [↓]  display 1st ITEM in document.  */
call parser 'item', 0                  /*go and parse the all the  ITEMs.     */
say center('first item:',@L.1,'─')     /*display a nicely formatted header.   */
say @.1;    say                        /*display the first  ITEM  found.      */

call parser 'price'                    /*go and parse all the   PRICEs.       */
say center('prices:',length(@@@),'─')  /*display a nicely formatted header.   */
say @@@;    say                        /*display a list of all the prices.    */

call parser 'name'
say center('names:',@L,'─')            /*display a nicely formatted header.   */
                        do k=1  for #  /*display all the names in the list.   */
                        say @.k        /*display a name from the  NAMES  list.*/
                        end   /*k*/
exit                                   /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
parser:  parse arg yy,tail,,@. @@. @@@;   $$=$;  @L=9;  yb='<'yy;   ye='</'yy">"
tail=word(tail 1, 1)                                /*use a tail  ">"  or not?*/
      do #=1  until  $$=''                          /*parse complete XML doc. */
      if tail  then parse  var  $$  (yb) '>' @@.# (ye) $$         /*find meat.*/
               else parse  var  $$  (yb)     @@.# (ye) $$         /*  "    "  */
      @.#=space(@@.#);   @@@=space(@@@ @.#)         /*shrink;  @@@=list of YY.*/
      @L.#=length(@.#);  @L=max(@L,@L.#)            /*length; maximum length. */
      end   /*#*/
#=#-1                                               /*adjust # of thing found.*/
return
```

'''output'''   is the same as the 1<sup>st</sup> version.


## Ruby

{{libheader|REXML}}

```ruby
#Example taken from the REXML tutorial (http://www.germane-software.com/software/rexml/docs/tutorial.html)
require "rexml/document"
include REXML
#create the REXML Document from the string (%q is Ruby's multiline string, everything between the two @-characters is the string)
doc = Document.new(
        %q@<inventory title="OmniCorp Store #45x10^3">
             ...
           </inventory>
          @
                          )
# The invisibility cream is the first <item>
invisibility = XPath.first( doc, "//item" )
# Prints out all of the prices
XPath.each( doc, "//price") { |element| puts element.text }
# Gets an array of all of the "name" elements in the document.
names = XPath.match( doc, "//name" )
```



## Scala


The code is entered in to Scala's REPL, to
better show the results.


```scala

scala> val xml: scala.xml.Elem =
     | <inventory title="OmniCorp Store #45x10^3">
     |   <section name="health">
     |     <item upc="123456789" stock="12">
     |       <name>Invisibility Cream</name>
     |       <price>14.50</price>
     |       <description>Makes you invisible</description>
     |     </item>
     |     <item upc="445322344" stock="18">
     |       <name>Levitation Salve</name>
     |       <price>23.99</price>
     |       <description>Levitate yourself for up to 3 hours per application</description>
     |     </item>
     |   </section>
     |   <section name="food">
     |     <item upc="485672034" stock="653">
     |       <name>Blork and Freen Instameal</name>
     |       <price>4.95</price>
     |       <description>A tasty meal in a tablet; just add water</description>
     |     </item>
     |     <item upc="132957764" stock="44">
     |       <name>Grob winglets</name>
     |       <price>3.56</price>
     |       <description>Tender winglets of Grob. Just add water</description>
     |     </item>
     |   </section>
     | </inventory>

scala> val firstItem = for {
     |   firstSection <- (xml \ "section").headOption
     |   firstItem <- (firstSection \ "item").headOption
     | } yield firstItem
firstItem: Option[scala.xml.Node] =
Some(<item upc="123456789" stock="12">
             <name>Invisibility Cream</name>
             <price>14.50</price>
             <description>Makes you invisible</description>
           </item>)

scala> val prices = for {
     |   section <- (xml \ "section")
     |   item <- (section \ "item")
     |   price <- (item \ "price")
     | } yield scala.math.BigDecimal(price.text)
prices: List[scala.math.BigDecimal] = List(14.50, 23.99, 4.95, 3.56)

scala> val salesTax = prices.sum * 0.05
salesTax: scala.math.BigDecimal = 2.3500

scala> println(salesTax.setScale(2, BigDecimal.RoundingMode.HALF_UP))
2.35

scala> val names = for {
     |   section <- (xml \ "section").toArray
     |   item <- (section \ "item")
     |   name <- (item \ "name")
     | } yield name.text
names: Array[String] = Array(Invisibility Cream, Levitation Salve, Blork and Freen Instameal, Grob winglets)
```



## Sidef

{{trans|Perl}}

```ruby
require('XML::XPath');

var x = %s'XML::XPath'.new(ARGF.slurp);

[x.findnodes('//item[1]')][0];
say [x.findnodes('//price')].map{x.getNodeText(_)};
[x.findnodes('//name')];
```


{{out}}

```txt

[14.5, 23.99, 4.95, 3.56]

```



## Tcl

{{libheader|tDOM}}

```tcl
# assume $xml holds the XML data
package require tdom
set doc [dom parse $xml]
set root [$doc documentElement]

set allNames [$root selectNodes //name]
puts [llength $allNames] ;# ==> 4

set firstItem [lindex [$root selectNodes //item] 0]
puts [$firstItem @upc] ;# ==> 123456789

foreach node [$root selectNodes //price] {
    puts [$node text]
}
```


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
MODE DATA
$$ XML=*
<inventory title="OmniCorp Store #45x10³">
  <section name="health">
    <item upc="123456789" stock="12">
      <name>Invisibility Cream</name>
      <price>14.50</price>
      <description>Makes you invisible</description>
    </item>
    <item upc="445322344" stock="18">
      <name>Levitation Salve</name>
      <price>23.99</price>
      <description>Levitate yourself for up to 3 hours per application</description>
    </item>
  </section>
  <section name="food">
    <item upc="485672034" stock="653">
      <name>Blork and Freen Instameal</name>
      <price>4.95</price>
      <description>A tasty meal in a tablet; just add water</description>
    </item>
    <item upc="132957764" stock="44">
      <name>Grob winglets</name>
      <price>3.56</price>
      <description>Tender winglets of Grob. Just add water</description>
    </item>
  </section>
</inventory>
$$ MODE TUSCRIPT

FILE = "test.xml"
ERROR/STOP CREATE (file,fdf-o,-std-)
FILE/ERASE/UTF8 $FILE = xml

BUILD S_TABLE beg=":<item*>:<name>:<price>:"
BUILD S_TABLE end=":</item>:</name>:</price>:"
BUILD S_TABLE modifiedbeg=":<name>:<price>:"
BUILD S_TABLE modifiedend=":</name>:</price>:"
firstitem=names="",countitem=0
ACCESS q: READ/STREAM/UTF8 $FILE s,a/beg+t+e/end
LOOP
READ/EXIT q
IF (a=="<name>")  names=APPEND(names,t)
IF (a=="<price>") PRINT t
IF (a.sw."<item") countitem=1
IF (countitem==1) THEN
firstitem=CONCAT(firstitem,a)
firstitem=CONCAT(firstitem,t)
firstitem=CONCAT(firstitem,e)
 IF (e=="</item>") THEN
 COUNTITEM=0
 MODIFY ACCESS q s_TABLE modifiedbeg,-,modifiedend
 ENDIF
ENDIF
ENDLOOP
ENDACCESS q
ERROR/STOP CLOSE (file)
firstitem=EXCHANGE (firstitem,":{2-00} ::")
firstitem=INDENT_TAGS (firstitem,-," ")
names=SPLIT(names)
TRACE *firstitem,names

```

Output:
<pre style='height:30ex;overflow:scroll'>
14.50
23.99
4.95
3.56
TRACE *    63    -*TUSTEP.EDT
firstitem    = *
           1 = <item upc="123456789" stock="12">
           2 =  <name>Invisibility Cream</name>
           3 =  <price>14.50</price>
           4 =  <description>Makes you invisible</description>
           5 = </item>
names        = *
           1 = Invisibility Cream
           2 = Levitation Salve
           3 = Blork and Freen Instameal
           4 = Grob winglets

```



## VBScript


```vb

Set objXMLDoc = CreateObject("msxml2.domdocument")

objXMLDoc.load("In.xml")

Set item_nodes = objXMLDoc.selectNodes("//item")
i = 1
For Each item In item_nodes
	If i = 1 Then
		WScript.StdOut.Write item.xml
		WScript.StdOut.WriteBlankLines(2)
		Exit For
	End If
Next

Set price_nodes = objXMLDoc.selectNodes("//price")
list_price = ""
For Each price In price_nodes
	list_price = list_price & price.text & ", "
Next
WScript.StdOut.Write list_price
WScript.StdOut.WriteBlankLines(2)

Set name_nodes = objXMLDoc.selectNodes("//name")
list_name = ""
For Each name In name_nodes
	list_name = list_name & name.text & ", "
Next
WScript.StdOut.Write list_name
WScript.StdOut.WriteBlankLines(2)

```

{{out}}

```txt

<item upc="123456789" stock="12">
	<name>Invisibility Cream</name>
	<price>14.50</price>
	<description>Makes you invisible</description>
</item>

14.50, 23.99, 4.95, 3.56,

Invisibility Cream, Levitation Salve, Blork and Freen Instameal, Grob winglets,

```



## Visual Basic .NET



```vbnet
Dim first_item = xml.XPathSelectElement("//item")
Console.WriteLine(first_item)

For Each price In xml.XPathSelectElements("//price")
    Console.WriteLine(price.Value)
Next

Dim names = (From item In xml.XPathSelectElements("//name") Select item.Value).ToArray
```




## XProc


```xml
<p:pipeline xmlns:p="http://www.w3.org/ns/xproc"
  name="one-two-three"
  version="1.0">
  <p:identity>
    <p:input port="source">
      <p:inline>
        <root>
          <first/>
          <prices/>
          <names/>
        </root>
      </p:inline>
    </p:input>
  </p:identity>
  <p:insert match="/root/first" position="first-child">
    <p:input port="insertion" select="(//item)[1]">
      <p:pipe port="source" step="one-two-three"/>
    </p:input>
  </p:insert>
  <p:insert match="/root/prices" position="first-child">
    <p:input port="insertion" select="//price">
      <p:pipe port="source" step="one-two-three"/>
    </p:input>
  </p:insert>
  <p:insert match="/root/names" position="first-child">
    <p:input port="insertion" select="//name">
      <p:pipe port="source" step="one-two-three"/>
    </p:input>
  </p:insert>
</p:pipeline>
```


## XQuery


```xquery
(:
  1. Retrieve the first "item" element
  Notice the braces around //item. This evaluates first all item elements and then retrieving the first one.
  Whithout the braces you get the first item for every section.
:)
let $firstItem := (//item)[1]

(: 2. Perform an action on each "price" element (print it out) :)
let $price := //price/data(.)

(: 3. Get an array of all the "name" elements  :)
let $names := //name

return
  <result>
    <firstItem>{$firstItem}</firstItem>
    <prices>{$price}</prices>
    <names>{$names}</names>
  </result>
```


Performing this XQuery on the given input document results in

```xquery
<?xml version="1.0" encoding="UTF-8"?>
<result>
   <firstItem>
      <item upc="123456789" stock="12">
         <name>Invisibility Cream</name>
         <price>14.50</price>
         <description>Makes you invisible</description>
      </item>
   </firstItem>
   <prices>14.50 23.99 4.95 3.56</prices>
   <names>
      <name>Invisibility Cream</name>
      <name>Levitation Salve</name>
      <name>Blork and Freen Instameal</name>
      <name>Grob winglets</name>
   </names>
</result>
```



## XSLT


```xml
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" />
  <xsl:template match="/">

    <!-- 1. first item element -->
    <xsl:text>
The first item element is</xsl:text>
    <xsl:value-of select="//item[1]" />

    <!-- 2. Print each price element -->
    <xsl:text>
The prices are: </xsl:text>
    <xsl:for-each select="//price">
      <xsl:text>
      </xsl:text>
      <xsl:copy-of select="." />
    </xsl:for-each>

    <!-- 3. Collect all the name elements -->
    <xsl:text>
The names are: </xsl:text>
    <xsl:copy-of select="//name" />
  </xsl:template>
</xsl:stylesheet>
```


{{omit from|Batch File|No way of XML parsing or processing.}}
{{omit from|PARI/GP|No real capacity for string manipulation}}
