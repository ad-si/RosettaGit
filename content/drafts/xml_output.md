+++
title = "XML/Output"
description = ""
date = 2019-10-03T10:58:53Z
aliases = []
[extra]
id = 3274
[taxonomies]
categories = []
tags = []
+++

{{task|XML}}
Create a function that takes a list of character names and a list of corresponding remarks and returns an XML document of <code><Character></code> elements each with a name attributes and each enclosing its remarks.
All <code><Character></code> elements are to be enclosed in turn, in an outer <code><CharacterRemarks></code> element.

As an example, calling the function with the three names of:

```txt

April
Tam O'Shanter
Emily
```

And three remarks of:

```txt

Bubbly: I'm > Tam and <= Emily
Burns: "When chapman billies leave the street ..."
Short & shrift
```

Should produce the XML (but not necessarily with the indentation):

```xml><CharacterRemarks

    <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
    <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
    <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```


The document may include an <tt><?xml?></tt> declaration and document type declaration, but these are optional. If attempting this task by direct string manipulation, the implementation ''must'' include code to perform entity substitution for the characters that have entities defined in the XML 1.0 specification.

Note: the example is chosen to show correct escaping of XML strings.
Note too that although the task is written to take two lists of corresponding data, a single mapping/hash/dictionary of names to remarks is also acceptable.

'''Note to editors:''' Program output with escaped characters will be viewed as the character on the page so you need to 'escape-the-escapes' to make the RC entry display what would be shown in a plain text viewer (See [[Talk:XML_Creation#Escaping_Escapes|this]]).
Alternately, output can be placed in <nowiki>
```xml>
```
</nowiki
 tags without any special treatment.


## Ada

{{works with|GNAT}}

Uses [http://libre.adacore.com/libre/tools/xmlada/ XML/Ada] from AdaCore.

character_remarks.adb:

```Ada
with Ada.Strings.Unbounded;
with Ada.Text_IO.Text_Streams;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

procedure Character_Remarks is
   package DC renames DOM.Core;
   package IO renames Ada.Text_IO;
   package US renames Ada.Strings.Unbounded;
   type Remarks is record
      Name : US.Unbounded_String;
      Text : US.Unbounded_String;
   end record;
   type Remark_List is array (Positive range <>) of Remarks;
   My_Remarks : Remark_List :=
      ((US.To_Unbounded_String ("April"),
        US.To_Unbounded_String ("Bubbly: I'm > Tam and <= Emily")),
       (US.To_Unbounded_String ("Tam O'Shanter"),
        US.To_Unbounded_String ("Burns: ""When chapman billies leave the street ...""")),
       (US.To_Unbounded_String ("Emily"),
        US.To_Unbounded_String ("Short & shrift")));
   My_Implementation : DC.DOM_Implementation;
   My_Document       : DC.Document := DC.Create_Document (My_Implementation);
   My_Root_Node      : DC.Element  := DC.Nodes.Append_Child (My_Document,
                                         DC.Documents.Create_Element (My_Document, "CharacterRemarks"));
   My_Element_Node   : DC.Element;
   My_Text_Node      : DC.Text;
begin
   for I in My_Remarks'Range loop
      My_Element_Node := DC.Nodes.Append_Child (My_Root_Node,
                            DC.Documents.Create_Element (My_Document, "Character"));
      DC.Elements.Set_Attribute (My_Element_Node, "Name", US.To_String (My_Remarks (I).Name));
      My_Text_Node    := DC.Nodes.Append_Child (My_Element_Node,
                            DC.Documents.Create_Text_Node (My_Document, US.To_String (My_Remarks (I).Text)));
   end loop;
   DC.Nodes.Write (IO.Text_Streams.Stream (IO.Standard_Output),
                   N => My_Document,
                   Pretty_Print => True);
end Character_Remarks;
```



###  Alternative version using Matreshka


Uses [http://forge.ada-ru.org/matreshka Matreshka SAX API for XML].


```Ada
with Ada.Wide_Wide_Text_IO;

with League.Strings;
with XML.SAX.Attributes;
with XML.SAX.Pretty_Writers;

procedure Main is

   function "+"
    (Item : Wide_Wide_String) return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   type Remarks is record
      Name   : League.Strings.Universal_String;
      Remark : League.Strings.Universal_String;
   end record;

   type Remarks_Array is array (Positive range <>) of Remarks;

   ------------
   -- Output --
   ------------

   procedure Output (Remarks : Remarks_Array) is
      Writer     : XML.SAX.Pretty_Writers.SAX_Pretty_Writer;
      Attributes : XML.SAX.Attributes.SAX_Attributes;

   begin
      Writer.Set_Offset (2);
      Writer.Start_Document;
      Writer.Start_Element (Qualified_Name => +"CharacterRemarks");

      for J in Remarks'Range loop
         Attributes.Clear;
         Attributes.Set_Value (+"name", Remarks (J).Name);
         Writer.Start_Element
           (Qualified_Name => +"Character", Attributes => Attributes);
         Writer.Characters (Remarks (J).Remark);
         Writer.End_Element (Qualified_Name => +"Character");
      end loop;

      Writer.End_Element (Qualified_Name => +"CharacterRemarks");
      Writer.End_Document;

      Ada.Wide_Wide_Text_IO.Put_Line (Writer.Text.To_Wide_Wide_String);
   end Output;

begin
   Output
    (((+"April",         +"Bubbly: I'm > Tam and <= Emily"),
      (+"Tam O'Shanter", +"Burns: ""When chapman billies leave the street ..."""),
      (+"Emily",         +"Short & shrift")));
end Main;
```



## ALGOL 68


```algol68
# returns a translation of str suitable for attribute values and content in an XML document #
OP  TOXMLSTRING = ( STRING str )STRING:
    BEGIN
        STRING result := "";
        FOR pos FROM LWB str TO UPB str DO
            CHAR c = str[ pos ];
            result +:= IF   c = "<"   THEN "&lt;"
                       ELIF c = ">"   THEN "&gt;"
                       ELIF c = "&"   THEN "&amp;"
                       ELIF c = "'"   THEN "&apos;"
                       ELIF c = """"  THEN "&quot;"
                       ELSE c
                       FI
        OD;
        result
    END; # TOXMLSTRING #

# generate a CharacterRemarks XML document from the characters and remarks #
# the number of elements in characters and remrks must be equal - this is not checked #
# the <?xml?> element is not generated #
PROC generate character remarks document = ( []STRING characters, remarks )STRING:
     BEGIN
         STRING result     := "<CharacterRemarks>";
         INT    remark pos := LWB remarks;
         FOR char pos FROM LWB characters TO UPB characters DO
             result +:= "<Character name=""" + TOXMLSTRING characters[ char pos ] + """>"
                      + TOXMLSTRING remarks[ remark pos ]
                      + "</Character>"
                      + REPR 10
                      ;
             remark pos +:= 1
         OD;
         result +:= "</CharacterRemarks>";
         result
     END; # generate character remarks document #

# test the generation #
print( ( generate character remarks document( ( "April", "Tam O'Shanter", "Emily" )
                                            , ( "Bubbly: I'm > Tam and <= Emily"
                                              , "Burns: ""When chapman billies leave the street ..."
                                              , "Short & shrift"
                                              )
                                            )
        , newline
        )
      )

```

{{out}}

```xml><CharacterRemarks
<Character name="April">Bubbly: I&apos;m &gt; Tam and &lt;= Emily</Character>
<Character name="Tam O&apos;Shanter">Burns: &quot;When chapman billies leave the street ...</Character>
<Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program outputXml.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall


/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessEndpgm:      .asciz "Normal end of program.\n"
szFileName:        .asciz "file2.xml"
szFileMode:        .asciz "w"
szMessError:       .asciz "Error detected !!!!. \n"
szName1:           .asciz "April"
szName2:           .asciz "Tam O'Shanter"
szName3:           .asciz "Emily"
szRemark1:         .asciz "Bubbly: I'm > Tam and <= Emily"
szRemark2:         .asciz "Burns: \"When chapman billies leave the street ...\""
szRemark3:         .asciz "Short & shrift"
szVersDoc:         .asciz "1.0"
szLibCharRem:      .asciz "CharacterRemarks"
szLibChar:         .asciz "Character"

//szLibExtract:    .asciz "Student"
szLibName:         .asciz "Name"
szCarriageReturn:  .asciz "\n"

tbNames:           .int szName1              @ area of pointer string name
                   .int szName2
                   .int szName3
                   .int 0                    @ area end
tbRemarks:         .int szRemark1            @ area of pointer string remark
                   .int szRemark2
                   .int szRemark3
                   .int 0                    @ area end
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
    ldr r1,iAdrszLibCharRem
    bl xmlNewNode                         @ create root node
    mov r8,r0                             @ node characterisation address
    mov r0,r9                             @ doc
    mov r1,r8                             @ node root
    bl xmlDocSetRootElement
    ldr r4,iAdrtbNames
    ldr r5,iAdrtbRemarks
    mov r6,#0                             @ loop counter
1:                                        @ start loop
    mov r0,#0
    ldr r1,iAdrszLibChar                  @ create node
    bl xmlNewNode
    mov r7,r0                             @ node character
                                          @ r0 = node address
    ldr r1,iAdrszLibName
    ldr r2,[r4,r6,lsl #2]                 @ load name string address
    bl xmlNewProp
    ldr r0,[r5,r6,lsl #2]                 @ load remark string address
    bl xmlNewText
    mov r1,r0
    mov r0,r7
    bl xmlAddChild
    mov r0,r8
    mov r1,r7
    bl xmlAddChild
    add r6,#1
    ldr r2,[r4,r6,lsl #2]                  @ load name string address
    cmp r2,#0                              @ = zero ?
    bne 1b                                 @ no -> loop

    ldr r0,iAdrszFileName
    ldr r1,iAdrszFileMode
    bl fopen                               @ file open
    cmp r0,#0
    blt 99f
    mov r6,r0                              @FD
    mov r1,r9
    mov r2,r8
    bl xmlDocDump                          @ write doc on the file

    mov r0,r6
    bl fclose
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
iAdrszLibCharRem:         .int szLibCharRem
iAdrszLibChar:            .int szLibChar
iAdrszLibName:            .int szLibName
iAdrtbNames:              .int tbNames
iAdrtbRemarks:            .int tbRemarks
iAdrszCarriageReturn:     .int szCarriageReturn
iStdout:                  .int STDOUT
iAdrszFileName:           .int szFileName
iAdrszFileMode:           .int szFileMode
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

{{output}}

```txt

more file2.xml =
<?xml version="1.0"?>
<CharacterRemarks><Character Name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</
Character><Character Name="Tam O'Shanter">Burns: "When chapman billies leave the
 street ..."</Character><Character Name="Emily">Short &amp; shrift</Character></
CharacterRemarks>

```


## AutoHotkey


```AutoHotkey
gosub constants
names := xmlescape(names)
remarks := xmlescape(remarks)

stringsplit, remarks, remarks, `n
xml = "<CharacterRemarks>"

loop, parse, names, `n
  xml .= "<Character name=" . A_LoopField . ">" . remarks%A_Index%
  . "</Character>`n"

xml .= "</CharacterRemarks>"

msgbox % xml
return

xmlescape(string)
{
  static
  punc = ",>,<,<=,>=,',&  ; "
  xmlpunc = &quot;,&gt;,&lt;,&lt;=,&gt;=,&apos;,&amp;
  if !punc1
  {
	StringSplit, punc, punc, `,
	StringSplit, xmlpunc, xmlpunc, `,
  }
  escaped := string
  loop, parse, punc, `,
  {
  StringReplace, escaped, escaped, % A_LoopField, % xmlpunc%A_Index%, All
  }
  Return escaped
}

constants:
#LTrim
names =
(
  April
  Tam O'Shanter
  Emily
)

remarks =
(
  Bubbly: I'm > Tam and <= Emily
  Burns: "When chapman billies leave the street ..."
  Short & shrift
)
return
```



## BASIC

{{works with|FreeBASIC}}

```basic
Data "April", "Bubbly: I'm > Tam and <= Emily", _
    "Tam O'Shanter", "Burns: ""When chapman billies leave the street ...""", _
    "Emily", "Short & shrift"

Declare Function xmlquote(ByRef s As String) As String
Dim n As Integer, dev As String, remark As String

Print "<CharacterRemarks>"
For n = 0 to 2
    Read dev, remark
    Print "  <Character name="""; xmlquote(dev); """>"; _
        xmlquote(remark); "</Character>"
Next
Print "</CharacterRemarks>"

End

Function xmlquote(ByRef s As String) As String
    Dim n As Integer
    Dim r As String
    For n = 0 To Len(s)
        Dim c As String
        c = Mid(s,n,1)
        Select Case As Const Asc(c)
        Case Asc("<")
             r = r + "&lt;"
        Case Asc(">")
             r = r + "&gt;"
        Case Asc("&")
             r = r + "&amp;"
        Case Asc("""")
             r = r + "&quot;"
        Case Asc("'")
             r = r + "&apos;"
        Case Else
             r = r + c
        End Select
    Next
    Function = r
End Function
```

=
## Applesoft BASIC
=

```ApplesoftBasic
100 Q$ = CHR$(34)
110 DE$(0) = "April"
120 RE$(0) = "Bubbly: I'm > Tam and <= Emily"
130 DE$(1) = "Tam O'Shanter"
140 RE$(1) = "Burns: " + Q$ + "When chapman billies leave the street ..." + Q$
150 DE$(2) = "Emily"
160 RE$(2) = "Short & shrift"

200 Print "<CharacterRemarks>"
210 For I = 0 to 2
220    Print "    <Character name="Q$;
230    X$=DE$(I) : GOSUB 300xmlquote
240    PRINT Q$">";
250    X$=RE$(I) : GOSUB 300xmlquote
260    PRINT "</Character>"
270 Next
280 Print "</CharacterRemarks>"
290 End

300 For n = 1 To Len(X$)
310    c$ = Mid$(X$,n,1)
320    IF C$ = "<" THEN C$ = "&lt;"
330    IF C$ = ">" THEN C$ = "&gt;"
340    IF C$ = "&" THEN C$ = "&amp;"
350    IF C$ = Q$ THEN C$ = "&quot;"
360    IF C$ = "'" THEN C$ = "&apos;"
370    PRINT C$;
380 NEXT N
390 RETURN
```



## Bracmat


```bracmat
( ( 2XML
  =     PCDATAentities attributeValueEntities doAll doAttributes
      , xml
    .   ( attributeValueEntities
        =   a c
          .     @( !arg
                 :   ?a
                     (("<"|"&"|\"):?c)
                     ?arg
                 )
              &   !a
                  "&"
                  ( !c:"<"&lt
                  | !c:"&"&amp
                  | quot
                  )
                  ";"
                  attributeValueEntities$!arg
            | !arg
        )
      & ( PCDATAentities
        =   a c
          .     @( !arg
                 :   ?a
                     (("<"|"&"|">"):?c)
                     ?arg
                 )
              &   !a
                  "&"
                  ( !c:"<"&lt
                  | !c:"&"&amp
                  | gt
                  )
                  ";"
                  PCDATAentities$!arg
            | !arg
        )
      & ( doAttributes
        =   a v
          .     !arg:(?a.?v) ?arg
              &   " "
                  PCDATAentities$!a
                  "=\""
                  attributeValueEntities$!v
                  \"
                  doAttributes$!arg
            |
        )
      & ( doAll
        =   xml first A B C att XML
          .   !arg:?xml
            & :?XML
            &   whl
              ' ( !xml:%?first ?xml
                & (   !first:(?A.?B)
                    & (   !B:(?att,?C)
                        &     !XML
                              (   !C:
                                & "<" !A doAttributes$!att " />\n"
                              |   "<"
                                  !A
                                  doAttributes$!att
                                  ">"
                                  doAll$!C
                                  "</"
                                  !A
                                  ">\n"
                              )
                          : ?XML
                      |   !A
                        : ( "!"&!XML "<!" !B ">":?XML
                          |   "!--"
                            & !XML "<!--" !B "-->":?XML
                          | "?"&!XML "<?" !B "?>\n":?XML
                          |   "![CDATA["
                            & !XML "<![CDATA[" !B "]]>":?XML
                          |   "!DOCTYPE"
                            & !XML "<!DOCTYPE" !B ">":?XML
                          |   ?
                            & !XML "<" !A doAttributes$!B ">":?XML
                          )
                      )
                  | !XML PCDATAentities$!first:?XML
                  )
                )
            & str$!XML
        )
      & doAll$!arg
  )
& ( makeList
  =   characters name names remark remarks
    .   !arg:(?names.?remarks)
      & :?characters
      &   whl
        ' (   (!names.!remarks)
            : (%?name ?names.%?remark ?remarks)
          &   !characters (Character.(name.!name),!remark)
            : ?characters
          )
      & ("?".xml) (CharacterRemarks.,!characters)
  )
&   put
  $ ( 2XML
    $ ( makeList
      $ ( April "Tam O'Shanter" Emily
        .   "Bubbly: I'm > Tam and <= Emily"
            "Burns: \"When chapman billies leave the street ...\""
            "Short & shrift"
        )
      )
    )
)
```

{{out}}

```txt
<?xml?>
<CharacterRemarks><Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
<Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
<Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```



## C

{{libheader|libXML}}

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

const char *names[] = {
  "April", "Tam O'Shanter", "Emily", NULL
};
const char *remarks[] = {
  "Bubbly: I'm > Tam and <= Emily",
  "Burns: \"When chapman billies leave the street ...\"",
  "Short & shrift", NULL
};

int main()
{
  xmlDoc *doc = NULL;
  xmlNode *root = NULL, *node;
  const char **next;
  int a;

  doc = xmlNewDoc("1.0");
  root = xmlNewNode(NULL, "CharacterRemarks");
  xmlDocSetRootElement(doc, root);

  for(next = names, a = 0; *next != NULL; next++, a++) {
    node = xmlNewNode(NULL, "Character");
    (void)xmlNewProp(node, "name", *next);
    xmlAddChild(node, xmlNewText(remarks[a]));
    xmlAddChild(root, node);
  }

  xmlElemDump(stdout, doc, root);

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return EXIT_SUCCESS;
}
```



## C++

Library: Boost

```cpp
#include <vector>
#include <utility>
#include <iostream>
#include <boost/algorithm/string.hpp>

std::string create_xml( std::vector<std::string> & ,std::vector<std::string> & ) ;

int main( ) {
   std::vector<std::string> names , remarks ;
   names.push_back( "April" ) ;
   names.push_back( "Tam O'Shantor" ) ;
   names.push_back ( "Emily" ) ;
   remarks.push_back( "Bubbly, I'm > Tam and <= Emily" ) ;
   remarks.push_back( "Burns: \"When chapman billies leave the street ...\"" ) ;
   remarks.push_back( "Short & shrift" ) ;
   std::cout << "This is in XML:\n" ;
   std::cout << create_xml( names , remarks ) << std::endl ;
   return 0 ;
}

std::string create_xml( std::vector<std::string> & names ,
      std::vector<std::string> & remarks ) {
   std::vector<std::pair<std::string , std::string> > entities ;
   entities.push_back( std::make_pair( "&" , "&amp;" ) ) ;
   entities.push_back( std::make_pair( "<" , "&lt;" ) ) ;
   entities.push_back( std::make_pair( ">" , "&gt;" ) ) ;
   std::string xmlstring ( "<CharacterRemarks>\n" ) ;
   std::vector<std::string>::iterator vsi = names.begin( ) ;
   typedef std::vector<std::pair<std::string , std::string> >::iterator Vpss ;
   for ( ; vsi != names.end( ) ; vsi++ ) {
      for ( Vpss vs = entities.begin( ) ; vs != entities.end( ) ; vs++ ) {
	 boost::replace_all ( *vsi , vs->first , vs->second ) ;
      }
   }
   for ( vsi = remarks.begin( ) ; vsi != remarks.end( ) ; vsi++ ) {
      for ( Vpss vs = entities.begin( ) ; vs != entities.end( ) ; vs++ ) {
	 boost::replace_all ( *vsi , vs->first , vs->second ) ;
      }
   }
   for ( int i = 0 ; i < names.size( ) ; i++ ) {
      xmlstring.append( "\t<Character name=\"").append( names[ i ] ).append( "\">")
	 .append( remarks[ i ] ).append( "</Character>\n" ) ;
   }
   xmlstring.append( "</CharacterRemarks>" ) ;
   return xmlstring ;
}
```



## C sharp

{{works with|C sharp|C#|3.0+}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Linq;

class Program
{
    static string CreateXML(Dictionary<string, string> characterRemarks)
    {
        var remarks = characterRemarks.Select(r => new XElement("Character", r.Value, new XAttribute("Name", r.Key)));
        var xml = new XElement("CharacterRemarks", remarks);
        return xml.ToString();
    }

    static void Main(string[] args)
    {
        var characterRemarks = new Dictionary<string, string>
        {
            { "April", "Bubbly: I'm > Tam and <= Emily" },
            { "Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\"" },
            { "Emily", "Short & shrift" }
        };

        string xml = CreateXML(characterRemarks);
        Console.WriteLine(xml);
    }
}
```



## Clojure


```lisp
(use 'clojure.xml)
(defn character-remarks-xml [characters remarks]
  (with-out-str (emit-element
                  {:tag :CharacterRemarks,
                   :attrs nil,
                   :content (vec (for [item (map vector characters remarks)]
                                   {:tag :Character,
                                    :attrs {:name (item 0)},
                                    :content [(item 1)]}) )})))
```



## Common Lisp


{{libheader|Closure XML}}


```lisp
(defun write-xml (characters lines &optional (out *standard-output*))
  (let* ((doc (dom:create-document 'rune-dom:implementation nil nil nil))
         (chars (dom:append-child doc (dom:create-element doc "Characters"))))
    (map nil (lambda (character line)
               (let ((c (dom:create-element doc "Character")))
                 (dom:set-attribute c "name" character)
                 (dom:append-child c (dom:create-text-node doc line))
                 (dom:append-child chars c)))
         characters lines)
    (write-string (dom:map-document (cxml:make-rod-sink) doc) out)))
```


Example of use:


```lisp
(write-xml '("April" "Tam O'Shanter" "Emily")
           '("Bubbly: I'm > Tam and <= Emily"
             "Burns: \"When chapman billies leave the street ...\""
             "Short & shrift"))
```


{{out}}
<div style="width:full;overflow:scroll">

```txt
<?xml version="1.0" encoding="UTF-8"?>
<Characters><Character name="April">Bubbly: I'm &amp;gt; Tam and &amp;lt;= Emily</Character><Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character><Character name="Emily">Short &amp;amp; shrift</Character></Characters>
```
</div>


## D

{{libheader|KXML}}

```d
import kxml.xml;
char[][][]characters =
        [["April","Bubbly: I'm > Tam and <= Emily"],
        ["Tam O'Shanter","Burns: \"When chapman billies leave the street ...\""],
        ["Emily","Short & shrift"]];
void addChars(XmlNode root,char[][][]info) {
        auto remarks = new XmlNode("CharacterRemarks");
        root.addChild(remarks);
        foreach(set;info) {
                remarks.addChild((new XmlNode("Character")).setAttribute("name",set[0]).addCData(set[1]));
        }
}
void main() {
        auto root = new XmlNode("");
        root.addChild(new XmlPI("xml"));
        addChars(root,characters);
        std.stdio.writefln("%s",root.write);
}
```


```txt

<?xml?>
<CharacterRemarks>
    <Character name="April">
        Bubbly: I'm &amp;gt; Tam and &amp;lt;= Emily
    </Character>
    <Character name="Tam O'Shanter">
        Burns: "When chapman billies leave the street ..."
    </Character>
    <Character name="Emily">
        Short &amp;amp; shrift
    </Character>
</CharacterRemarks>

```



## Delphi


```Delphi

//You need to use these units
uses
  Classes,
  Dialogs,
  XMLIntf,
  XMLDoc;

//..............................................

//This function creates the XML
function CreateXML(aNames, aRemarks: TStringList): string;
var
  XMLDoc: IXMLDocument;
  Root: IXMLNode;
  i: Integer;
begin
  //Input check
  if (aNames   = nil) or
     (aRemarks = nil) then
  begin
     Result:= '<CharacterRemarks />';
     Exit;
  end;

  //Creating the TXMLDocument instance
  XMLDoc:= TXMLDocument.Create(nil);

  //Activating the document
  XMLDoc.Active:= True;

  //Creating the Root element
  Root:= XMLDoc.AddChild('CharacterRemarks');

  //Creating the inner nodes
  for i:=0 to Min(aNames.Count, aRemarks.Count) - 1 do
  with Root.AddChild('Character') do
  begin
    Attributes['name']:= aNames[i];
    Text:= aRemarks[i];
  end;

  //Outputting the XML as a string
  Result:= XMLDoc.XML.Text;
end;

//..............................................

//Consuming code example (fragment)
var
  Names,
  Remarks: TStringList;
begin
  //Creating the lists objects
  Names:= TStringList.Create;
  Remarks:= TStringList.Create;
  try
    //Filling the list with names
    Names.Add('April');
    Names.Add('Tam O''Shanter');
    Names.Add('Emily');

    //Filling the list with remarks
    Remarks.Add('Bubbly: I''m > Tam and <= Emily');
    Remarks.Add('Burns: "When chapman billies leave the street ..."');
    Remarks.Add('Short & shrift');

    //Constructing and showing the XML
    Showmessage(CreateXML(Names, Remarks));

  finally
    //Freeing the list objects
    Names.Free;
    Remarks.Free;
  end;
end;


```



## Erlang


```Erlang

-module( xml_output ).

-export( [task/0] ).

-include_lib("xmerl/include/xmerl.hrl").

task() ->
    Data = {'CharacterRemarks', [], [{'Character', [{name, X}], [[Y]]} || {X, Y} <- contents()]},
    lists:flatten( xmerl:export_simple([Data], xmerl_xml) ).


contents() -> [{"April", "Bubbly: I'm > Tam and <= Emily"}, {"Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\""}, {"Emily", "Short & shrift"}].

```

{{out}}

```txt

4> xml_output:task().
"<?xml version=\"1.0\"?><CharacterRemarks><Character name=\"April\">Bubbly: I'm &gt; Tam and &lt;= Emily</Character><Character name=\"Tam O'Shanter\">Burns: \"When chapman billies leave the street ...\"</Character><Character name=\"Emily\">Short &amp; shrift</Character></CharacterRemarks>"

```



## Euphoria

{{trans|BASIC}}

```euphoria
function xmlquote(sequence s)
    sequence r
    r = ""
    for i = 1 to length(s) do
        if s[i] = '<' then
            r &= "&lt;"
        elsif s[i] = '>' then
            r &= "&gt;"
        elsif s[i] = '&' then
            r &= "&amp;"
        elsif s[i] = '"' then
            r &= "&quot;"
        elsif s[i] = '\'' then
            r &= "&apos;"
        else
            r &= s[i]
        end if
    end for
    return r
end function

constant CharacterRemarks = {
    {"April", "Bubbly: I'm > Tam and <= Emily"},
    {"Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\""},
    {"Emily", "Short & shrift"}
}

puts(1,"<CharacterRemarks>\n")
for i = 1 to length(CharacterRemarks) do
    printf(1,"  <CharacterName=\"%s\">",{xmlquote(CharacterRemarks[i][1])})
    puts(1,xmlquote(CharacterRemarks[i][2]))
    puts(1,"</Character>\n")
end for
puts(1,"</CharacterRemarks>\n")
```


{{out}}

```txt
<CharacterRemarks>
  <CharacterName="April">Bubbly: I&apos;m &gt; Tam and &lt;= Emily</Character>
  <CharacterName="Tam O&apos;Shanter">Burns: &quot;When chapman billies leave the street ...&quot;</Character>
  <CharacterName="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```


=={{header|F_Sharp|F#}}==

```fsharp
#light

open System.Xml
type Character = {name : string; comment : string }

let data = [
    { name = "April"; comment = "Bubbly: I'm > Tam and <= Emily"}
    { name = "Tam O'Shanter"; comment = "Burns: \"When chapman billies leave the street ...\""}
    { name = "Emily"; comment = "Short & shrift"} ]

let doxml (characters : Character list) =
    let doc = new XmlDocument()
    let root = doc.CreateElement("CharacterRemarks")
    doc.AppendChild root |> ignore
    Seq.iter (fun who ->
             let node = doc.CreateElement("Character")
             node.SetAttribute("name", who.name)
             doc.CreateTextNode(who.comment)
             |> node.AppendChild |> ignore
             root.AppendChild node |> ignore
             ) characters
    doc.OuterXml
```


```txt
<CharacterRemarks>
 <Character name="April">Bubbly: I'm &amp;gt; Tam and &amp;lt;= Emily</Character>
 <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
 <Character name="Emily">Short &amp;amp; shrift</Character>
</CharacterRemarks>
```



## Factor


```factor
USING: sequences xml.syntax xml.writer ;

: print-character-remarks ( names remarks -- )
    [ [XML <Character name=<-> ><-></Character> XML] ] 2map
    [XML <CharacterRemarks><-></CharacterRemarks> XML] pprint-xml ;
```

Example of usage:

```factor
{ "April" "Tam O'Shanter" "Emily" } {
    "Bubbly: I'm > Tam and <= Emily"
    "Burns: \"When chapman billies leave the street ...\""
    "Short & shrift"
} print-remarks
```



## Fantom


```fantom

using xml

class XmlOutput
{
  public static Void main ()
  {
    Str[] names := ["April", "Tam O'Shanter", "Emily"]
      Str[] remarks := ["Bubbly: I'm > Tam and <= Emily",
        "Burns: \"When chapman billies leave the street ...\"",
        "Short & shrift"]

    doc := XDoc()
    root := XElem("CharacterRemarks")
    doc.add (root)

    names.each |Str name, Int i|
    {
      child := XElem("Character")
      child.addAttr("Name", name)
      child.add(XText(remarks[i]))
      root.add (child)
    }

    doc.write(Env.cur.out)
  }
}

```


{{out}} (not exactly conforming):

```txt

<?xml version='1.0' encoding='UTF-8'?>
<CharacterRemarks>
 <Character Name='April'>Bubbly: I'm > Tam and &amp;lt;= Emily</Character>
 <Character Name='Tam O&amp;apos;Shanter'>Burns: "When chapman billies leave the street ..."</Character>
 <Character Name='Emily'>Short &amp; shrift</Character>
</CharacterRemarks>

```



## Forth


{{libheader|Forth Foundation Library}}


```forth
include ffl/est.fs
include ffl/xos.fs

\ Input lists
0 value names
here ," Emily"
here ," Tam O'Shanter"
here ," April"
here to names
, , ,

0 value remarks
here ,"  Short & shrift"
here ,\" Burns: \"When chapman billies leave the street ...\""
here ,"  Bubbly: I'm > Tam and <= Emily"
here to remarks
, , ,

: s++ ( c-addr1 -- c-addr2 c-addr3 u3 )
  dup cell+ swap @ count
;

\ Create xml writer
tos-create xml

: create-xml ( c-addr1 c-addr2 -- )
  0 s" CharacterRemarks" xml xos-write-start-tag
  3 0 DO
    swap s++ s" name" 2swap 1
    s" Character" xml xos-write-start-tag
    swap s++      xml xos-write-text
    s" Character" xml xos-write-end-tag
  LOOP
  drop drop
  s" CharacterRemarks" xml xos-write-end-tag
;

names remarks create-xml

\ Output xml string
xml str-get type cr
```


## Go

'''Using package xml to marshal from a data structure:'''

```go
package main

import (
    "encoding/xml"
    "fmt"
)

// Function required by task description.
func xRemarks(r CharacterRemarks) (string, error) {
    b, err := xml.MarshalIndent(r, "", "    ")
    return string(b), err
}

// Task description allows the function to take "a single mapping..."
// This data structure represents a mapping.
type CharacterRemarks struct {
    Character []crm
}

type crm struct {
    Name   string `xml:"name,attr"`
    Remark string `xml:",chardata"`
}

func main() {
    x, err := xRemarks(CharacterRemarks{[]crm{
        {`April`, `Bubbly: I'm > Tam and <= Emily`},
        {`Tam O'Shanter`, `Burns: "When chapman billies leave the street ..."`},
        {`Emily`, `Short & shrift`},
    }})
    if err != nil {
        x = err.Error()
    }
    fmt.Println(x)
}
```

{{out}}

```xml><CharacterRemarks

    <Character name="April">Bubbly: I&#39;m &gt; Tam and &lt;= Emily</Character>
    <Character name="Tam O&#39;Shanter">Burns: &#34;When chapman billies leave the street ...&#34;</Character>
    <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```


Using the text/template package to generate text:
(but still leaning on the xml package for escaping.)

```go
package main

import (
    "encoding/xml"
    "fmt"
    "strings"
    "text/template"
)

type crm struct {
    Char, Rem string
}

var tmpl = `<CharacterRemarks>
{{range .}}    <Character name="{{xml .Char}}">{{xml .Rem}}</Character>
{{end}}</CharacterRemarks>
`

func xmlEscapeString(s string) string {
    var b strings.Builder
    xml.Escape(&b, []byte(s))
    return b.String()
}

func main() {
    xt := template.New("").Funcs(template.FuncMap{"xml": xmlEscapeString})
    template.Must(xt.Parse(tmpl))

    // Define function required by task description.
    xRemarks := func(crms []crm) (string, error) {
        var b strings.Builder
        err := xt.Execute(&b, crms)
        return b.String(), err
    }

    // Call the function with example data.  The data is represented as
    // a "single mapping" as allowed by the task, rather than two lists.
    x, err := xRemarks([]crm{
        {`April`, `Bubbly: I'm > Tam and <= Emily`},
        {`Tam O'Shanter`, `Burns: "When chapman billies leave the street ..."`},
        {`Emily`, `Short & shrift`}})
    if err != nil {
        x = err.Error()
    }
    fmt.Println(x)
}
```

Output is same as marshalled version.


## Groovy


```groovy
def writer = new StringWriter()
def builder = new groovy.xml.MarkupBuilder(writer)
def names = ["April", "Tam O'Shanter", "Emily"]
def remarks = ["Bubbly: I'm > Tam and <= Emily", 'Burns: "When chapman billies leave the street ..."', "Short & shrift"]

builder.CharacterRemarks() {
    names.eachWithIndex() { n, i -> Character(name:n, remarks[i]) };
}

println writer.toString()
```



## Haskell

This implementation uses the [http://hackage.haskell.org/package/xml <code>xml</code> package].


```haskell
import Text.XML.Light

characterRemarks :: [String] -> [String] -> String
characterRemarks names remarks = showElement $ Element
    (unqual "CharacterRemarks")
    []
    (zipWith character names remarks)
    Nothing
  where character name remark = Elem $ Element
            (unqual "Character")
            [Attr (unqual "name") name]
            [Text $ CData CDataText remark Nothing]
            Nothing
```



## HicEst


```HicEst
CHARACTER names="April~Tam O'Shanter~Emily~"
CHARACTER remarks*200/%Bubbly: I'm > Tam and <= Emily~Burns: "When chapman billies leave the street ..."~Short & shrift~%/
CHARACTER XML*1000

EDIT(Text=remarks, Right='&', RePLaceby='&amp;', DO)
EDIT(Text=remarks, Right='>', RePLaceby='&gt;', DO)
EDIT(Text=remarks, Right='<', RePLaceby='&lt;', DO)

XML = "<CharacterRemarks>" // $CRLF
DO i = 1, 3
  EDIT(Text=names, SePaRators='~', ITeM=i, Parse=name)
  EDIT(Text=remarks, SePaRators='~', ITeM=i, Parse=remark)
  XML = TRIM(XML) // '<Character name="' // name // '">' // remark // '</Character>' // $CRLF
ENDDO
XML = TRIM(XML) // "</CharacterRemarks>"
```



## J


First create the table of substitutions and the verb which properly escapes the input string:


```j
tbl=: ('&quote;'; '&amp;'; '&lt;'; '&gt;') (a.i.'"&<>')} <"0 a.
esc=: [:; {&tbl@:i.~&a.
```


Then create the verb which combines name with remark:

```J
cmb=: [:; dyad define &.>
  '<Character name="', (esc x), '">', (esc y), '</Character>', LF
)
```


Finally, create the verb which creates the final XML:

```j
xmlify=:  '<CharacterRemarks>', LF, cmb, '</CharacterRemarks>'"_
```


Example:


```j
names=: 'April'; 'Tam O''Shanter'; 'Emily'

remarks=: <;._2]0 :0
 I'm > Tam and <= Emily
 Burns: "When chapman billies leave the street ..."
 Short & shrift
)
```


<lang>   names xmlify remarks
<CharacterRemarks>
<Character name="April"> I'm &gt; Tam and &lt;= Emily</Character>
<Character name="Tam O'Shanter"> Burns: &quote;When chapman billies leave the street ...&quote;</Character>
<Character name="Emily"> Short &amp; shrift</Character>
</CharacterRemarks>
```



## Java


### Using DOM


```java
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class XmlCreation {

  private static final String[] names = {"April", "Tam O'Shanter", "Emily"};
  private static final String[] remarks = {"Bubbly: I'm > Tam and <= Emily",
    "Burns: \"When chapman billies leave the street ...\"",
      "Short & shrift"};

  public static void main(String[] args) {
    try {
      // Create a new XML document
      final Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();

      // Append the root element
      final Element root = doc.createElement("CharacterRemarks");
      doc.appendChild(root);

      // Read input data and create a new <Character> element for each name.
      for(int i = 0; i < names.length; i++) {
        final Element character = doc.createElement("Character");
        root.appendChild(character);
        character.setAttribute("name", names[i]);
        character.appendChild(doc.createTextNode(remarks[i]));
      }

      // Serializing XML in Java is unnecessary complicated
      // Create a Source from the document.
      final Source source = new DOMSource(doc);

      // This StringWriter acts as a buffer
      final StringWriter buffer = new StringWriter();

      // Create a Result as a transformer target.
      final Result result = new StreamResult(buffer);

      // The Transformer is used to copy the Source to the Result object.
      final Transformer transformer = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty("indent", "yes");
      transformer.transform(source, result);

      // Now the buffer is filled with the serialized XML and we can print it
      // to the console.
      System.out.println(buffer.toString());
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

}
```


Result:

```xml
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<CharacterRemarks>
<Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
<Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
<Character name="Emily">Short &amp; shrift</Character>
```


===Using the Streaming API for XML (StAX)===

```java
import java.io.StringWriter;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;

public class XmlCreationStax {

  private static final String[] names = {"April", "Tam O'Shanter", "Emily"};
  private static final String[] remarks = {"Bubbly: I'm > Tam and <= Emily",
    "Burns: \"When chapman billies leave the street ...\"",
      "Short & shrift"};

  public static void main(String[] args) {
    try {
      final StringWriter buffer = new StringWriter();

      final XMLStreamWriter out = XMLOutputFactory.newInstance()
          .createXMLStreamWriter(buffer);

      out.writeStartDocument("UTF-8", "1.0");
      out.writeStartElement("CharacterRemarks");

      for(int i = 0; i < names.length; i++) {
        out.writeStartElement("Character");
        out.writeAttribute("name", names[i]);
        out.writeCharacters(remarks[i]);
        out.writeEndElement();
      }

      out.writeEndElement();
      out.writeEndDocument();

      System.out.println(buffer);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```


This produces:

```xml
<?xml version="1.0" encoding="UTF-8"?><CharacterRemarks><Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character><Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character><Character name="Emily">Short &amp; shrift</Character></CharacterRemarks>
```



## Joy


```Joy

DEFINE subst ==
[[['< "&lt;"  putchars]
  ['> "&gt;"  putchars]
  ['& "&amp;" putchars]
  [putch]] case] step;

XMLOutput ==
"<CharacterRemarks>\n" putchars
[ "<Character name=\"" putchars uncons swap putchars "\">" putchars first subst  "</Character>\n" putchars] step
"</CharacterRemarks>\n" putchars.

[ [ "April" "Bubbly: I'm > Tam and <= Emily" ]
  [ "Tam O'Shanter" "Burns: \"When chapman billies leave the street ...\"" ]
  [ "Emily" "Short & shrift" ]
] XMLOutput.

```




## Julia


```julia
using LightXML

dialog = [("April", "Bubbly: I'm > Tam and <= Emily"),
          ("Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\""),
          ("Emily", "Short & shrift")]

const xdoc = XMLDocument()
const xroot = create_root(xdoc, "CharacterRemarks")

for (name, remarks) in dialog
    xs1 = new_child(xroot, "Character")
    set_attribute(xs1, "name", name)
    add_text(xs1, remarks)
end

println(xdoc)

```
{{output}}
```txt

 <?xml version="1.0" encoding="utf-8"?>
 <CharacterRemarks>
   <Character name="April">Bubbly: I'm &amp;gt; Tam and &lt;= Emily</Character>
   <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
   <Character name="Emily">Short &amp;amp; shrift</Character>
 </CharacterRemarks>

```



## Kotlin


```scala
// version 1.1.3

import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.dom.DOMSource
import java.io.StringWriter
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.TransformerFactory

fun main(args: Array<String>) {
    val names = listOf("April", "Tam O'Shanter", "Emily")

    val remarks = listOf(
        "Bubbly: I'm > Tam and <= Emily",
        "Burns: \"When chapman billies leave the street ...\"",
        "Short & shrift"
    )

    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder  = dbFactory.newDocumentBuilder()
    val doc = dBuilder.newDocument()
    val root = doc.createElement("CharacterRemarks") // create root node
    doc.appendChild(root)

    // now create Character elements
    for (i in 0 until names.size) {
        val character = doc.createElement("Character")
        character.setAttribute("name", names[i])
        val remark = doc.createTextNode(remarks[i])
        character.appendChild(remark)
        root.appendChild(character)
    }

    val source = DOMSource(doc)
    val sw = StringWriter()
    val result = StreamResult(sw)
    val tFactory = TransformerFactory.newInstance()
    tFactory.newTransformer().apply {
        setOutputProperty("omit-xml-declaration", "yes")
        setOutputProperty("indent", "yes")
        setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4")
        transform(source, result)
    }
    println(sw)
}

```


{{out}}

```xml><CharacterRemarks

    <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
    <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
    <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```



## Lasso

Lasso has built in support for both creating and parsing xml.

```Lasso
define character2xml(names::array, remarks::array) => {

	fail_if(#names -> size != #remarks -> size, -1, 'Input arrays not of same size')

	local(
		domimpl = xml_domimplementation,
		doctype = #domimpl -> createdocumenttype(
			'svg:svg',
			'-//W3C//DTD SVG 1.1//EN',
			'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'
		),
		character_xml = #domimpl -> createdocument(
			'http://www.w3.org/2000/svg',
			'svg:svg',
			#docType
		),
		csnode = #character_xml -> createelement('CharacterRemarks'),
		charnode
	)

	#character_xml -> appendChild(#csnode)

	loop(#names -> size) => {
		#charnode = #character_xml -> createelement('Character')
		#charnode -> setAttribute('name', #names  -> get(loop_count))
		#charnode -> nodeValue = encode_xml(#remarks -> get(loop_count))
		#csnode -> appendChild(#charnode)
	}
	return #character_xml

}

character2xml(
	array(`April`, `Tam O'Shanter`, `Emily`),
	array(`Bubbly: I'm > Tam and <= Emily`, `Burns: "When chapman billies leave the street ..."`, `Short & shrift`)
)
```

{{out}}

```xml

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE svg:svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg xmlns:svg="http://www.w3.org/2000/svg"/>
<CharacterRemarks>
  <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```



## Lua

Using the LuaXML library as available via luarocks.  Note that strings in Lua can be enclosed in either single or double quotes to help reduce the need for escape characters.

```Lua
require("LuaXML")

function addNode(parent, nodeName, key, value, content)
    local node = xml.new(nodeName)
    table.insert(node, content)
    parent:append(node)[key] = value
end

root = xml.new("CharacterRemarks")
addNode(root, "Character", "name", "April", "Bubbly: I'm > Tam and <= Emily")
addNode(root, "Character", "name", "Tam O'Shanter", 'Burns: "When chapman billies leave the street ..."')
addNode(root, "Character", "name", "Emily", "Short & shrift")
print(root)
```

{{out}}

```xml><CharacterRemarks

  <Character name="April">Bubbly: I&apos;m &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O&apos;Shanter">Burns: &quot;When chapman billies leave the street ...&quot;</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```

Note also that LuaXML escapes quote marks and apostrophes, which makes the output slightly different to the task requirement.  This can be 'fixed' if necessary using Lua's in-built string.gsub function:

```Lua
xmlStr = xml.str(root):gsub("&apos;", "'"):gsub("&quot;", '"')
print(xmlStr)
```

{{out}}

```xml><CharacterRemarks

  <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```



## Mathematica

Some tricky input with the second remark

```Mathematica
c = {"April", "Tam O'Shanter","Emily"};
r = {"Bubbly:I'm > Tam and <= Emily" ,
StringReplace["Burns:\"When chapman billies leave the street ...\"", "\"" -> ""], "Short & shrift"};

ExportString[ XMLElement[  "CharacterRemarks", {},
{XMLElement["Character", {"name" -> c[[1]]}, {r[[1]]}],
 XMLElement["Character", {"name" -> c[[2]]}, {r[[2]]}],
 XMLElement["Character", {"name" -> c[[3]]}, {r[[3]]}]
}], "XML", "AttributeQuoting" -> "\""]
```



```txt
<CharacterRemarks>
 <Character name=\"April\">Bubbly:I'm &gt; Tam and &lt;= Emily</Character>
 <Character name=\"Tam O'Shanter\">Burns:When chapman billies leave the street ...</Character>
 <Character name=\"Emily\">Short &amp; shrift</Character>
</CharacterRemarks>
```



## Matlab


```Matlab

RootXML = com.mathworks.xml.XMLUtils.createDocument('CharacterRemarks');
docRootNode = RootXML.getDocumentElement;
thisElement = RootXML.createElement('Character');
thisElement.setAttribute('Name','April')
thisElement.setTextContent('Bubbly: I''m > Tam and <= Emily');
docRootNode.appendChild(thisElement);
thisElement = RootXML.createElement('Character');
thisElement.setAttribute('Name','Tam O''Shanter')
thisElement.setTextContent('Burns: "When chapman billies leave the street ..."');
docRootNode.appendChild(thisElement);
thisElement = RootXML.createElement('Character');
thisElement.setAttribute('Name','Emily')
thisElement.setTextContent('Short & shrift');
docRootNode.appendChild(thisElement);

```

Output;

```txt

xmlwrite(RootXML)

ans =

<?xml version="1.0" encoding="utf-8"?>
<CharacterRemarks>
   <Character Name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
   <Character Name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
   <Character Name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```



## NetRexx


### Using DOM

{{trans|Java}}

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

import java.io.StringWriter

import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.Result
import javax.xml.transform.Source
import javax.xml.transform.Transformer
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult

import org.w3c.dom.Document
import org.w3c.dom.Element

names = [String -
  "April", "Tam O'Shanter", "Emily" -
]

remarks = [ String -
     "Bubbly: I'm > Tam and <= Emily" -
  ,  'Burns: "When chapman billies leave the street ..."' -
  ,  'Short & shrift' -
]

do
  -- Create a new XML document
  doc = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument()

  -- Append the root element
  root = doc.createElement("CharacterRemarks")
  doc.appendChild(root)

  -- Read input data and create a new <Character> element for each name.
  loop i_ = 0 to names.length - 1
    character = doc.createElement("Character")
    root.appendChild(character)
    character.setAttribute("name", names[i_])
    character.appendChild(doc.createTextNode(remarks[i_]))
    end i_

  -- Serializing XML in Java is unnecessary complicated
  -- Create a Source from the document.
  source = DOMSource(doc)

  -- This StringWriter acts as a buffer
  buffer = StringWriter()

  -- Create a Result as a transformer target.
  result = StreamResult(buffer)

  -- The Transformer is used to copy the Source to the Result object.
  transformer = TransformerFactory.newInstance().newTransformer()
  transformer.setOutputProperty("indent", "yes")
  transformer.transform(source, result)

  -- Now the buffer is filled with the serialized XML and we can print it to the console.
  say buffer.toString
catch ex = Exception
  ex.printStackTrace
end

return

```

;Output

```xml
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<CharacterRemarks>
<Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
<Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
<Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```


===Using the Streaming API for XML (StAX)===
{{trans|Java}}

```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

import java.io.StringWriter

import javax.xml.stream.XMLOutputFactory
import javax.xml.stream.XMLStreamWriter

names = [String -
  "April", "Tam O'Shanter", "Emily" -
]

remarks = [ String -
     "Bubbly: I'm > Tam and <= Emily" -
  ,  'Burns: "When chapman billies leave the street ..."' -
  ,  'Short & shrift' -
]

do
  buffer = StringWriter()

  out = XMLOutputFactory.newInstance().createXMLStreamWriter(buffer)

  out.writeStartDocument("UTF-8", "1.0")
  out.writeCharacters('\n')

  out.writeStartElement("CharacterRemarks")
  out.writeCharacters('\n')

  loop i_ = 0 to names.length - 1
    out.writeStartElement("Character")
    out.writeAttribute("name", names[i_])
    out.writeCharacters(remarks[i_])
    out.writeEndElement()
    out.writeCharacters('\n')
    end i_

  out.writeEndElement()
  out.writeEndDocument()
  out.writeCharacters('\n')

  say buffer.toString
catch ex = Exception
  ex.printStackTrace
end

return

```

;Output

```xml
<?xml version="1.0" encoding="UTF-8"?>
<CharacterRemarks>
<Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
<Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
<Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```



## Nim


```nim
import xmltree, strtabs, sequtils

proc charsToXML(names, remarks): XmlNode =
  result = <>CharacterRemarks()
  for name, remark in items zip(names, remarks):
    result.add(<>Character(name=name, remark.newText))

echo charsToXML(@["April", "Tam O'Shanter", "Emily"],
  @["Bubbly: I'm > Tam and <= Emily",
    "Burns: \"When chapman billies leave the street ...\"",
    "Short & shrift"])
```

Output:

```txt
<CharacterRemarks>
  <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O'Shanter">Burns: &quot;When chapman billies leave the street ...&quot;</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```



## Objeck


```objeck

use XML;
use Structure;

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      # list of name
      names := Vector->New();
      names->AddBack("April"->As(Base));
      names->AddBack("Tam O'Shanter"->As(Base));
      names->AddBack("Emily"->As(Base));
      # list of comments
      comments := Vector->New();
      comments->AddBack("Bubbly: I'm > Tam and <= Emily"->As(Base));
      comments->AddBack("Burns: \"When chapman billies leave the street ...\""->As(Base));
      comments->AddBack("Short & shrift"->As(Base));
      # build XML document
      builder := XMLBuilder->New("CharacterRemarks");
      root := builder->GetRoot();
      if(names->Size() = comments->Size()) {
        each(i : names) {
          element := XMLElement->New(XMLElementType->ELEMENT,
            names->Get(i)->As(String),
            comments->Get(i)->As(String));
          root->AddChild(element);
        };
      };
      XMLElement->DecodeString(builder->ToString())->PrintLine();
    }
  }
}

```



## OCaml


from the toplevel using the library [http://tech.motion-twin.com/xmllight.html xml-light]:


```ocaml
# #directory "+xml-light" (* or maybe "+site-lib/xml-light" *) ;;

# #load "xml-light.cma" ;;

# let data = [
    ("April", "Bubbly: I'm > Tam and <= Emily");
    ("Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\"");
    ("Emily", "Short & shrift");
  ] in
  let tags =
    List.map (fun (name, comment) ->
      Xml.Element ("Character", [("name", name)], [(Xml.PCData comment)])
    ) data
  in
  print_endline (
    Xml.to_string_fmt (Xml.Element ("CharacterRemarks", [], tags)))
  ;;
<CharacterRemarks>
  <Character name="April">Bubbly: I&apos;m &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O'Shanter">Burns: &quot;When chapman billies leave the street ...&quot;</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
- : unit = ()
```


Another solution using the library [http://erratique.ch/software/xmlm xmlm]:


```ocaml
#directory "+xmlm"
#load "xmlm.cmo"
open Xmlm

let datas = [
    ("April", "Bubbly: I'm > Tam and <= Emily");
    ("Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\"");
    ("Emily", "Short & shrift");
  ]

let xo = make_output (`Channel stdout)

let () =
  output xo (`Dtd None);
  output xo (`El_start (("", "CharacterRemarks"), []));
  List.iter (fun (name, content) ->
      output xo (`El_start (("", "Character"), [(("", "name"), name)]));
      output xo (`Data content);
      output xo (`El_end);
  ) datas;
  output xo (`El_end);
  print_newline()
```



## Oz

It is natural to represent XML document as nested Oz records. Writing a function that serializes records to textual XML is not too difficult.


```oz
declare
  proc {Main}
     Names = ["April"
	      "Tam O'Shanter"
	      "Emily"]

     Remarks = ["Bubbly: I'm > Tam and <= Emily"
		"Burns: \"When chapman billies leave the street ...\""
		"Short & shrift"]

     Characters = {List.zip Names Remarks
		   fun {$ N R}
		      'Character'(name:N R)
		   end}

     DOM = {List.toTuple 'CharacterRemarks' Characters}
  in
     {System.showInfo {Serialize DOM}}
  end

  fun {Serialize DOM}
     "<?xml version=\"1.0\" ?>\n"#
     {SerializeElement DOM 0}
  end

  fun {SerializeElement El Indent}
     Name = {Label El}
     Attributes ChildElements Contents
     {DestructureElement El ?Attributes ?ChildElements ?Contents}
     EscContents = {Map Contents Escape}
     Spaces = {List.make Indent} for S in Spaces do S = &  end
  in
     Spaces#"<"#Name#
     {VSConcatMap Attributes SerializeAttribute}#">"#
     {VSConcat EscContents}#{When ChildElements\=nil "\n"}#
     {VSConcatMap ChildElements fun {$ E} {SerializeElement E Indent+4} end}#
     {When ChildElements\=nil Spaces}#"</"#Name#">\n"
  end

  proc {DestructureElement El ?Attrs ?ChildElements ?Contents}
     SubelementRec AttributeRec
     {Record.partitionInd El fun {$ I _} {Int.is I} end
      ?SubelementRec ?AttributeRec}
     Subelements = {Record.toList SubelementRec}
  in
     {List.partition Subelements VirtualString.is ?Contents ?ChildElements}
     Attrs = {Record.toListInd AttributeRec}
  end

  fun {SerializeAttribute Name#Value}
     " "#Name#"=\""#{EscapeAttribute Value}#"\""
  end

  fun {Escape VS}
     {Flatten {Map {VirtualString.toString VS} EscapeChar}}
  end

  fun {EscapeAttribute VS}
     {Flatten {Map {VirtualString.toString VS} EscapeAttributeChar}}
  end

  fun {EscapeChar X}
     case X of 60 then "&lt;"
     [] 62 then "&gt;"
     [] 38 then "&amp;"
     else X
     end
  end

  fun {EscapeAttributeChar X}
     case X of 34 then "&quot;"
     else {EscapeChar X}
     end
  end

  %% concatenates a list to a virtual string
  fun {VSConcat Xs}
     {List.toTuple '#' Xs}
  end

  fun {VSConcatMap Xs F}
     {VSConcat {Map Xs F}}
  end

  fun {When Cond X}
     if Cond then X else nil end
  end
in
  {Main}
```


Output:

```xml
<?xml version="1.0" ?>
<CharacterRemarks>
    <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
    <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
    <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```



## Perl

{{libheader|XML::Mini}}

```perl
#! /usr/bin/perl
use strict;
use XML::Mini::Document;

my @students = ( [ "April", "Bubbly: I'm > Tam and <= Emily" ],
                 [ "Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\"" ],
		 [ "Emily", "Short & shrift" ]
                );

my $doc = XML::Mini::Document->new();
my $root = $doc->getRoot();
my $studs = $root->createChild("CharacterRemarks");
foreach my $s (@students)
{
    my $stud = $studs->createChild("Character");
    $stud->attribute("name", $s->[0]);
    $stud->text($s->[1]);
}
print $doc->toString();
```



## Perl 6

{{works with|Rakudo|2018.05}}


```perl6
use XML::Writer;

my @students =
    [ Q[April],         Q[Bubbly: I'm > Tam and <= Emily] ],
    [ Q[Tam O'Shanter], Q[Burns: "When chapman billies leave the street ..."] ],
    [ Q[Emily],         Q[Short & shrift] ]
;

my @lines = map { :Character[:name(.[0]), .[1]] }, @students;

say XML::Writer.serialize( CharacterRemarks => @lines );
```

{{out}}

```XML><CharacterRemarks
<Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
<Character name="Tam O'Shanter">Burns: &quot;When chapman billies leave the street ...&quot;</Character>
<Character name="Emily">Short &amp; shrift</Character></CharacterRemarks>
```



## Phix


```Phix
constant {hchars,hsubs} = columnize({{"<","&lt;"},
                                     {">","&gt;"},
                                     {"&","&amp;"},
                                     {"\"","&quot;"},
                                     {"\'","&apos;"}})
function xmlquote_all(sequence s)
    for i=1 to length(s) do
        s[i] = substitute_all(s[i],hchars,hsubs)
    end for
    return s
end function

function xml_CharacterRemarks(sequence data)
string res = "<CharacterRemarks>\n"
    for i=1 to length(data) do
        res &= sprintf("  <CharacterName=\"%s\">%s</Character>\n",xmlquote_all(data[i]))
    end for
    return res & "</CharacterRemarks>\n"
end function

constant testset = {
    {"April", "Bubbly: I'm > Tam and <= Emily"},
    {"Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\""},
    {"Emily", "Short & shrift"}
}
printf(1,xml_CharacterRemarks(testset))

-- Sample output:
--  <CharacterRemarks>
--    <CharacterName="April">Bubbly: I&apos;m &amp;gt; Tam and &amp;lt;= Emily</Character>
--    <CharacterName="Tam O&apos;Shanter">Burns: &quot;When chapman billies leave the street ...&quot;</Character>
--    <CharacterName="Emily">Short &amp; shrift</Character>
--  </CharacterRemarks>
```



## PicoLisp


```PicoLisp
(load "@lib/xml.l")

(de characterRemarks (Names Remarks)
   (xml
      (cons
         'CharacterRemarks
         NIL
         (mapcar
            '((Name Remark)
               (list 'Character (list (cons 'name Name)) Remark) )
            Names
            Remarks ) ) ) )

(characterRemarks
   '("April" "Tam O'Shanter" "Emily")
   (quote
      "I'm > Tam and <= Emily"
      "Burns: \"When chapman billies leave the street ..."
      "Short & shrift" ) )
```

Output:

```txt
<CharacterRemarks>
   <Character name="April">I'm > Tam and &amp;#60;= Emily</Character>
   <Character name="Tam O'Shanter">Burns: &amp;#34;When chapman billies leave the street ...</Character>
   <Character name="Emily">Short &amp;#38; shrift</Character>
</CharacterRemarks>
```



## PureBasic


```Purebasic
DataSection
  dataItemCount:
  Data.i 3

  names:
  Data.s "April", "Tam O'Shanter", "Emily"

  remarks:
  Data.s "Bubbly: I'm > Tam and <= Emily",
         ~"Burns: \"When chapman billies leave the street ...\"",
         "Short & shrift"
EndDataSection

Structure characteristic
  name.s
  remark.s
EndStructure

NewList didel.characteristic()
Define item.s, numberOfItems, i

Restore dataItemCount
Read.i numberOfItems

;add names
Restore names
For i = 1 To numberOfItems
  AddElement(didel())
  Read.s item
  didel()\name = item
Next

;add remarks
ResetList(didel())
FirstElement(didel())
Restore remarks:
For i = 1 To numberOfItems
  Read.s item
  didel()\remark = item
  NextElement(didel())
Next

Define xml, mainNode, itemNode
ResetList(didel())
FirstElement(didel())
xml = CreateXML(#PB_Any)
mainNode = CreateXMLNode(RootXMLNode(xml), "CharacterRemarks")
ForEach didel()
  itemNode = CreateXMLNode(mainNode, "Character")
  SetXMLAttribute(itemNode, "name", didel()\name)
  SetXMLNodeText(itemNode, didel()\remark)
Next
FormatXML(xml, #PB_XML_ReFormat |  #PB_XML_WindowsNewline | #PB_XML_ReIndent)

If OpenConsole()
  PrintN(ComposeXML(xml, #PB_XML_NoDeclaration))
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
<CharacterRemarks>
  <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O&apos;Shanter">Burns: "When chapman billies leave the st
reet ..."</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```



## Python

Normal output is all one line of XML, the .replace(...) makes it more readable.

```python>>>
 from xml.etree import ElementTree as ET
>>> from itertools import izip
>>> def characterstoxml(names, remarks):
	root = ET.Element("CharacterRemarks")
	for name, remark in izip(names, remarks):
		c = ET.SubElement(root, "Character", {'name': name})
		c.text = remark
	return ET.tostring(root)

>>> print characterstoxml(
	names = ["April", "Tam O'Shanter", "Emily"],
	remarks = [ "Bubbly: I'm > Tam and <= Emily",
		    'Burns: "When chapman billies leave the street ..."',
		    'Short & shrift' ] ).replace('><','>\n<')
```

Gives the output:

```xml><CharacterRemarks

<Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
<Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
<Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```



## R

{{libheader|XML}}

```R
library(XML)
char2xml <- function(names, remarks){
	tt <- xmlHashTree()
	head <- addNode(xmlNode("CharacterRemarks"), character(), tt)
	node <- list()
	for (i in 1:length(names)){
		node[[i]] <- addNode(xmlNode("Character", attrs=c(name=names[i])), head, tt)
		addNode(xmlTextNode(remarks[i]), node[[i]], tt)
	}
	return(tt)
}
output <- char2xml( names=c("April","Tam O'Shanter","Emily"),
remarks=c("Bubbly: I'm > Tam and <= Emily", 'Burns: "When chapman billies leave the street ..."', "Short & shrift") )
```


Gives the output: <!-- manually inserted "&amp;" where required to protect actual R output from the browser -->

```txt
<CharacterRemarks>
 <Character name="April">Bubbly: I&apos;m &amp;gt; Tam and &amp;lt;= Emily</Character>
 <Character name="Tam O'Shanter">Burns: &amp;quot;When chapman billies leave the street ...&amp;quot;</Character>
 <Character name="Emily">Short &amp;amp; shrift</Character>
</CharacterRemarks>
```



## Racket



```racket

#lang racket
(require xml)

(define (make-character-xexpr characters remarks)
  `(CharacterRemarks
    ,@(for/list ([character characters]
                [remark remarks])
       `(Character ((name ,character)) ,remark))))

(display-xml/content
 (xexpr->xml
  (make-character-xexpr
   '("April" "Tam O'Shanter" "Emily")
   '("Bubbly: I'm > Tam and <= Emily"
     "Burns: \"When chapman billies leave the street ...\""
     "Short & shrift"))))

```


Output:


```xml

<CharacterRemarks>
  <Character name="April">
    Bubbly: I'm &gt; Tam and &lt;= Emily
  </Character>
  <Character name="Tam O'Shanter">
    Burns: "When chapman billies leave the street ..."
  </Character>
  <Character name="Emily">
    Short &amp; shrift
  </Character>
</CharacterRemarks>

```



## Rascal


```rascal
import Prelude;
import lang::xml::DOM;

list[str] charnames = ["April", "Tam O\'Shanter", "Emily"];
list[str] remarks = ["Bubbly: I\'m \> Tam and \<= Emily", "Burns: \"When chapman billies leave the street ...\"", "Short & shrift"];

public void xmloutput(list[str] n,list[str] r){
	if(size(n) != size(r)){
		throw "n and r should be of the same size";
                }
	else{
		characters = [element(none(),"Character",[attribute(none(),"name",n[i]), charData(r[i])]),charData("\n")| i <- [0..size(n)-1]];
		x = document(element(none(),"CharacterRemarks",characters));
		return println(xmlPretty(x));
                }
}
```

This gives an output:

```rascal>rascal
xmloutput(charnames, remarks)
<?xml version="1.0" encoding="UTF-8"?>
<CharacterRemarks>
  <Character name="April">Bubbly: I'\m &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O'\Shanter">Burns: "When chapman billies leave the street ..."</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>


ok
```



## REXX

REXX doesn't have any functions to handle XML entities, an abbreviated version is included here.

```rexx
/*REXX program creates an HTML (XML) list of character names and corresponding remarks. */
charname.  =
charname.1 = "April"
charname.2 = "Tam O'Shanter"
charname.3 = "Emily"
                                     do i=1  while  charname.i\==''
                                     say 'charname'   i   '='   charname.i
                                     end   /*i*/;     say
remark.  =
remark.1 = "I'm > Tam and <= Emily"
remark.2 = "When chapman billies leave the street ..."
remark.3 = "Short & shift"
                                     do k=1  while  remark.k\==''
                                     say '  remark'   k   '='   remark.k
                                     end   /*k*/;     say
items  = 0
header = 'CharacterRemarks'
header = header'>'

    do j=1  while  charname.j\==''
    _=charname.j
    if j==1  then call create '<'header
    call create '    <Character name="'  ||,
                char2xml(_)'">"'         ||,
                char2xml(remark.j)'"</Character>'
    end   /*j*/

if create.0\==0  then call create '</'header

        do m=1  for create.0
        say create.m                             /*display the  Mth  entry to terminal. */
        end   /*m*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
char2xml: procedure;  parse arg $
          amper = pos('&', $)\==0                /*  &   has to be treated special.     */
          semi  = pos(';', $)\==0                /*  ;    "   "  "    "       "         */
          #=0                                    /* [↓]  find a free/unused character···*/
          if amper  then do                      /*          ··· and translate freely.  */
                           do j=0  for 255;    ?=d2c(j);    if pos(?, $)==0   then leave
                           end   /*j*/
                         $=translate($, ?, "&");            #= j + 1
                         end
                                                 /* [↓]  find a free/unused character···*/
          if semi   then do                      /*          ··· and translate freely.  */
                           do k=#  for 255;    ?=d2c(k);    if pos(?, $)==0  then leave
                           end   /*k*/
                         $=translate($, ?, ";")
                         end

/*───── Following are most of the characters in the  DOS  (or DOS Windows)  codepage  437 ──────────*/

$=XML_('â',"ETH")    ; $=XML_('ƒ',"fnof")  ; $=XML_('═',"boxH")      ; $=XML_('♥',"hearts")
$=XML_('â','#x00e2') ; $=XML_('á',"aacute"); $=XML_('╬',"boxVH")     ; $=XML_('♦',"diams")
$=XML_('â','#x00e9') ; $=XML_('á','#x00e1'); $=XML_('╧',"boxHu")     ; $=XML_('♣',"clubs")
$=XML_('ä',"auml")   ; $=XML_('í',"iacute"); $=XML_('╨',"boxhU")     ; $=XML_('♠',"spades")
$=XML_('ä','#x00e4') ; $=XML_('í','#x00ed'); $=XML_('╤',"boxHd")     ; $=XML_('♂',"male")
$=XML_('à',"agrave") ; $=XML_('ó',"oacute"); $=XML_('╥',"boxhD")     ; $=XML_('♀',"female")
$=XML_('à','#x00e0') ; $=XML_('ó','#x00f3'); $=XML_('╙',"boxUr")     ; $=XML_('☼',"#x263c")
$=XML_('å',"aring")  ; $=XML_('ú',"uacute"); $=XML_('╘',"boxuR")     ; $=XML_('↕',"UpDownArrow")
$=XML_('å','#x00e5') ; $=XML_('ú','#x00fa'); $=XML_('╒',"boxdR")     ; $=XML_('¶',"para")
$=XML_('ç',"ccedil") ; $=XML_('ñ',"ntilde"); $=XML_('╓',"boxDr")     ; $=XML_('§',"sect")
$=XML_('ç','#x00e7') ; $=XML_('ñ','#x00f1'); $=XML_('╫',"boxVh")     ; $=XML_('↑',"uarr")
$=XML_('ê',"ecirc")  ; $=XML_('Ñ',"Ntilde"); $=XML_('╪',"boxvH")     ; $=XML_('↑',"uparrow")
$=XML_('ê','#x00ea') ; $=XML_('Ñ','#x00d1'); $=XML_('┘',"boxul")     ; $=XML_('↑',"ShortUpArrow")
$=XML_('ë',"euml")   ; $=XML_('¿',"iquest"); $=XML_('┌',"boxdr")     ; $=XML_('↓',"darr")
$=XML_('ë','#x00eb') ; $=XML_('⌐',"bnot")  ; $=XML_('█',"block")     ; $=XML_('↓',"downarrow")
$=XML_('è',"egrave") ; $=XML_('¬',"not")   ; $=XML_('▄',"lhblk")     ; $=XML_('↓',"ShortDownArrow")
$=XML_('è','#x00e8') ; $=XML_('½',"frac12"); $=XML_('▀',"uhblk")     ; $=XML_('←',"larr")
$=XML_('ï',"iuml")   ; $=XML_('½',"half")  ; $=XML_('α',"alpha")     ; $=XML_('←',"leftarrow")
$=XML_('ï','#x00ef') ; $=XML_('¼',"frac14"); $=XML_('ß',"beta")      ; $=XML_('←',"ShortLeftArrow")
$=XML_('î',"icirc")  ; $=XML_('¡',"iexcl") ; $=XML_('ß',"szlig")     ; $=XML_('1c'x,"rarr")
$=XML_('î','#x00ee') ; $=XML_('«',"laqru") ; $=XML_('ß','#x00df')    ; $=XML_('1c'x,"rightarrow")
$=XML_('ì',"igrave") ; $=XML_('»',"raqru") ; $=XML_('Γ',"Gamma")     ; $=XML_('1c'x,"ShortRightArrow")
$=XML_('ì','#x00ec') ; $=XML_('░',"blk12") ; $=XML_('π',"pi")        ; $=XML_('!',"excl")
$=XML_('Ä',"Auml")   ; $=XML_('▒',"blk14") ; $=XML_('Σ',"Sigma")     ; $=XML_('"',"apos")
$=XML_('Ä','#x00c4') ; $=XML_('▓',"blk34") ; $=XML_('σ',"sigma")     ; $=XML_('$',"dollar")
$=XML_('Å',"Aring")  ; $=XML_('│',"boxv")  ; $=XML_('µ',"mu")        ; $=XML_("'","quot")
$=XML_('Å','#x00c5') ; $=XML_('┤',"boxvl") ; $=XML_('τ',"tau")       ; $=XML_('*',"ast")
$=XML_('É',"Eacute") ; $=XML_('╡',"boxvL") ; $=XML_('Φ',"phi")       ; $=XML_('/',"sol")
$=XML_('É','#x00c9') ; $=XML_('╢',"boxVl") ; $=XML_('Θ',"Theta")     ; $=XML_(':',"colon")
$=XML_('æ',"aelig")  ; $=XML_('╖',"boxDl") ; $=XML_('δ',"delta")     ; $=XML_('<',"lt")
$=XML_('æ','#x00e6') ; $=XML_('╕',"boxdL") ; $=XML_('∞',"infin")     ; $=XML_('=',"equals")
$=XML_('Æ',"AElig")  ; $=XML_('╣',"boxVL") ; $=XML_('φ',"Phi")       ; $=XML_('>',"gt")
$=XML_('Æ','#x00c6') ; $=XML_('║',"boxV")  ; $=XML_('ε',"epsilon")   ; $=XML_('?',"quest")
$=XML_('ô',"ocirc")  ; $=XML_('╗',"boxDL") ; $=XML_('∩',"cap")       ; $=XML_('_',"commat")
$=XML_('ô','#x00f4') ; $=XML_('╝',"boxUL") ; $=XML_('≡',"equiv")     ; $=XML_('[',"lbrack")
$=XML_('ö',"ouml")   ; $=XML_('╜',"boxUl") ; $=XML_('±',"plusmn")    ; $=XML_('\',"bsol")
$=XML_('ö','#x00f6') ; $=XML_('╛',"boxuL") ; $=XML_('±',"pm")        ; $=XML_(']',"rbrack")
$=XML_('ò',"ograve") ; $=XML_('┐',"boxdl") ; $=XML_('±',"PlusMinus") ; $=XML_('^',"Hat")
$=XML_('ò','#x00f2') ; $=XML_('└',"boxur") ; $=XML_('≥',"ge")        ; $=XML_('`',"grave")
$=XML_('û',"ucirc")  ; $=XML_('┴',"bottom"); $=XML_('≤',"le")        ; $=XML_('{',"lbrace")
$=XML_('û','#x00fb') ; $=XML_('┴',"boxhu") ; $=XML_('÷',"div")       ; $=XML_('{',"lcub")
$=XML_('ù',"ugrave") ; $=XML_('┬',"boxhd") ; $=XML_('÷',"divide")    ; $=XML_('|',"vert")
$=XML_('ù','#x00f9') ; $=XML_('├',"boxvr") ; $=XML_('≈',"approx")    ; $=XML_('|',"verbar")
$=XML_('ÿ',"yuml")   ; $=XML_('─',"boxh")  ; $=XML_('∙',"bull")      ; $=XML_('}',"rbrace")
$=XML_('ÿ','#x00ff') ; $=XML_('┼',"boxvh") ; $=XML_('°',"deg")       ; $=XML_('}',"rcub")
$=XML_('Ö',"Ouml")   ; $=XML_('╞',"boxvR") ; $=XML_('·',"middot")    ; $=XML_('Ç',"Ccedil")
$=XML_('Ö','#x00d6') ; $=XML_('╟',"boxVr") ; $=XML_('·',"middledot") ; $=XML_('Ç','#x00c7')
$=XML_('Ü',"Uuml")   ; $=XML_('╚',"boxUR") ; $=XML_('·',"centerdot") ; $=XML_('ü',"uuml")
$=XML_('Ü','#x00dc') ; $=XML_('╔',"boxDR") ; $=XML_('·',"CenterDot") ; $=XML_('ü','#x00fc')
$=XML_('¢',"cent")   ; $=XML_('╩',"boxHU") ; $=XML_('√',"radic")     ; $=XML_('é',"eacute")
$=XML_('£',"pound")  ; $=XML_('╦',"boxHD") ; $=XML_('²',"sup2")      ; $=XML_('é','#x00e9')
$=XML_('¥',"yen")    ; $=XML_('╠',"boxVR") ; $=XML_('■',"square ")   ; $=XML_('â',"acirc")

          if amper  then $=xml_(?,  "amp")       /*Was there an ampersand?  Translate it*/
          if semi   then $=xml_(??, "semi")      /* "   "     " semicolon?      "      "*/
          return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
create:          items= items + 1                /*bump the count of items in the list. */
          create.items= arg(1)                   /*add item to the  CREATE  list.       */
          create.0    = items                    /*indicate how many items in the list. */
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
xml_:     parse arg _                            /*make an XML entity    (&xxxx;)       */
          if pos(_, $)\==0  then return changestr(_, $, "&"arg(2)";")
                                 return $
```

Some older REXXes don't have a '''changestr''' bif, so one is included here   ──►   [[CHANGESTR.REX]].

{{out|output|text=  (unrendered):

```txt

charname 1 = April
charname 2 = Tam O'Shanter
charname 3 = Emily

  remark 1 = I'm > Tam and <= Emily
  remark 2 = When chapman billies leave the street ...
  remark 3 = Short & shift

<CharacterRemarks>
    <Character name="April">"I&quot;m &gt; Tam and &lt;&equals; Emily"</Character>
    <Character name="Tam O&quot;Shanter">"When chapman billies leave the street ..."</Character>
    <Character name="Emily">"Short &amp; shift"</Character>
</CharacterRemarks>

```



## Ruby

The {{libheader|REXML}}library handles character mapping when adding attributes and text.

```ruby
require 'rexml/document'
include REXML

remarks = {
  %q(April)         => %q(Bubbly: I'm > Tam and <= Emily),
  %q(Tam O'Shanter) => %q(Burns: "When chapman billies leave the street ..."),
  %q(Emily)         => %q(Short & shrift),
}

doc = Document.new
root = doc.add_element("CharacterRemarks")

remarks.each do |name, remark|
  root.add_element("Character", {'Name' => name}).add_text(remark)
end

# output with indentation
doc.write($stdout, 2)
```


produces <!-- manually inserted "&amp;" where required to protect actual Ruby output from the browser -->

```txt
<CharacterRemarks>
  <Character Name='Emily'>
    Short &amp;amp; shrift
  </Character>
  <Character Name='Tam O&amp;apos;Shanter'>
    Burns: &amp;quot;When chapman billies leave the street ...&amp;quot;
  </Character>
  <Character Name='April'>
    Bubbly: I&amp;apos;m &amp;gt; Tam and &amp;lt;= Emily
  </Character>
</CharacterRemarks>
```



## Rust


```rust
extern crate xml;

use std::collections::HashMap;
use std::str;

use xml::writer::{EmitterConfig, XmlEvent};

fn characters_to_xml(characters: HashMap<String, String>) -> String {
    let mut output: Vec<u8> = Vec::new();
    let mut writer = EmitterConfig::new()
        .perform_indent(true)
        .create_writer(&mut output);

    writer
        .write(XmlEvent::start_element("CharacterRemarks"))
        .unwrap();

    for (character, line) in &characters {
        let element = XmlEvent::start_element("Character").attr("name", character);
        writer.write(element).unwrap();
        writer.write(XmlEvent::characters(line)).unwrap();
        writer.write(XmlEvent::end_element()).unwrap();
    }

    writer.write(XmlEvent::end_element()).unwrap();
    str::from_utf8(&output).unwrap().to_string()
}

#[cfg(test)]
mod tests {
    use super::characters_to_xml;
    use std::collections::HashMap;

    #[test]
    fn test_xml_output() {
        let mut input = HashMap::new();
        input.insert(
            "April".to_string(),
            "Bubbly: I'm > Tam and <= Emily".to_string(),
        );
        input.insert(
            "Tam O'Shanter".to_string(),
            "Burns: \"When chapman billies leave the street ...\"".to_string(),
        );
        input.insert("Emily".to_string(), "Short & shrift".to_string());

        let output = characters_to_xml(input);

        println!("{}", output);
        assert!(output.contains(
            "<Character name=\"Tam O&apos;Shanter\">Burns: \"When chapman \
             billies leave the street ...\"</Character>"
        ));
        assert!(output
            .contains("<Character name=\"April\">Bubbly: I'm > Tam and &lt;= Emily</Character>"));
        assert!(output.contains("<Character name=\"Emily\">Short &amp; shrift</Character>"));
    }
}
```



## Scala


```scala
val names = List("April", "Tam O'Shanter", "Emily")

val remarks = List("Bubbly: I'm > Tam and <= Emily", """Burns: "When chapman billies leave the street ..."""", "Short & shrift")

def characterRemarks(names: List[String], remarks: List[String]) = <CharacterRemarks>
  { names zip remarks map { case (name, remark) => <Character name={name}>{remark}</Character> } }
</CharacterRemarks>

characterRemarks(names, remarks)

```


Result:

```xml><CharacterRemarks

  <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character><Character name="Tam O'Shanter">Burns:
&quot;When chapman billies leave the street ...&quot;</Character><Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>
```



## Sidef

{{trans|Perl}}

```ruby
require('XML::Mini::Document');

var students = [
                ["April",         "Bubbly: I'm > Tam and <= Emily"],
                ["Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\""],
                ["Emily",         "Short & shrift"]
               ];

var doc   = %s'XML::Mini::Document'.new;
var root  = doc.getRoot;
var studs = root.createChild("CharacterRemarks");

students.each { |s|
    var stud = studs.createChild("Character");
    stud.attribute("name", s[0]);
    stud.text(s[1]);
};

print doc.toString;
```



## Slate



```slate
lobby define: #remarks -> (
{'April' -> 'Bubbly: I\'m > Tam and <= Emily'.
'Tam O\'Shanter' -> 'Burns: "When chapman billies leave the street ..."'.
'Emily' -> 'Short & shrift'.
} as: Dictionary).

define: #writer -> (Xml Writer newOn: '' new writer).
writer inTag: 'CharacterRemarks' do:
  [| :w |
   lobby remarks keysAndValuesDo:
     [| :name :remark | w inTag: 'Character' do: [| :w | w ; remark] &attributes: {'name' -> name}].
   ].

inform: writer contents
```

Produces:
<div style="width:full;overflow:scroll">

```txt

<CharacterRemarks><Character name="Emily">Short &amp; shrift</Character><Character name="Tam O&apos;Shanter">Burns: "When chapman billies leave the street ..."</Character><Character name="April">Bubbly: I&apos;m &gt; Tam and &lt;= Emily</Character></CharacterRemarks>

```
</div>


## Tcl


### Using only Tcl string manipulation


```Tcl
proc xquote string {
    list [string map "' &apos; \\\" &quot; < &gt; > &lt; & &amp;" $string]
}
proc < {name attl args} {
    set res <$name
    foreach {att val} $attl {
        append res " $att='$val'"
    }
    if {[llength $args]} {
        append res >
        set sep ""
        foreach a $args {
            append res $sep $a
            set sep \n
        }
        append res </$name>
    } else {append res />}
    return $res
}
set cmd {< CharacterRemarks {}}
foreach {name comment} {
    April "Bubbly: I'm > Tam and <= Emily"
    "Tam O'Shanter" "Burns: \"When chapman billies leave the street ...\""
    Emily "Short & shrift"
} {
    append cmd " \[< Character {Name [xquote $name]} [xquote $comment]\]"
}
puts [eval $cmd]
```

produces
```txt
<CharacterRemarks><Character Name='April'>Bubbly: I&amp;apos;m &amp;lt; Tam and &amp;gt;= Emily</Character>
<Character Name='Tam O&amp;apos;Shanter'>Burns: &amp;quot;When chapman billies leave the street ...&amp;quot;</Character>
<Character Name='Emily'>Short &amp;amp; shrift</Character></CharacterRemarks>
```



### Working with DOM trees

Using {{libheader|tDOM}}

```tcl
package require tdom
set xml [dom createDocument CharacterRemarks]
foreach {name comment} {
    April "Bubbly: I'm > Tam and <= Emily"
    "Tam O'Shanter" "Burns: \"When chapman billies leave the street ...\""
    Emily "Short & shrift"
} {
    set elem [$xml createElement Character]
    $elem setAttribute name $name
    $elem appendChild [$xml createTextNode $comment]
    [$xml documentElement] appendChild $elem
}
$xml asXML
```


```txt
<CharacterRemarks>
    <Character name="April">Bubbly: I'm &amp;amp;gt; Tam and &amp;amp;lt;= Emily</Character>
    <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
    <Character name="Emily">Short &amp;amp; shrift</Character>
</CharacterRemarks>
```

Using {{libheader|TclXML}}

```tcl
package require dom
set xml [dom::DOMImplementation create]
set root [dom::document createElement $xml CharacterRemarks]
foreach {name comment} {
    April "Bubbly: I'm > Tam and <= Emily"
    "Tam O'Shanter" "Burns: \"When chapman billies leave the street ...\""
    Emily "Short & shrift"
} {
    set element [dom::document createElement $root Character]
    dom::element setAttribute $element name $name
    dom::document createTextNode $element $comment
}
dom::DOMImplementation serialize $xml -indent 1
```

produces (with line breaks added for clarity:

```txt
<?xml version="1.0"?>
<CharacterRemarks>
  <Character name="April">Bubbly: I'm &amp;gt; Tam and &amp;lt;= Emily</Character>
  <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
  <Character name="Emily">Short &amp;amp; shrift</Character>
</CharacterRemarks>
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
STRUCTURE xmloutput
DATA '<CharacterRemarks>'
DATA * '  <Character name="' names +'">' remarks +'</Character>'
DATA = '</CharacterRemarks>'
ENDSTRUCTURE
BUILD X_TABLE entitysubst=" >> &gt; << &lt; & &amp; "
ERROR/STOP CREATE ("dest",seq-o,-std-)
ACCESS d: WRITE/ERASE/STRUCTURE  "dest" num,str
str="xmloutput"
  names=*
  DATA April
  DATA Tam O'Shanter
  DATA Emily
  remarks=*
  DATA Bubbly: I'm > Tam and <= Emily
  DATA Burns: "When chapman billies leave the street ..."
  DATA Short & shrift
  remarks=EXCHANGE(remarks,entitysubst)
WRITE/NEXT d
ENDACCESS d

```

Output in file "dest":

```txt

<CharacterRemarks>
  <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```



## VBScript

Saves in a file.
<lang VBScript}}>
Set objXMLDoc = CreateObject("msxml2.domdocument")

Set objRoot = objXMLDoc.createElement("CharacterRemarks")
objXMLDoc.appendChild objRoot

Call CreateNode("April","Bubbly: I'm > Tam and <= Emily")
Call CreateNode("Tam O'Shanter","Burns: ""When chapman billies leave the street ...""")
Call CreateNode("Emily","Short & shrift")

objXMLDoc.save("C:\Temp\Test.xml")

Function CreateNode(attrib_value,node_value)
	Set objNode = objXMLDoc.createElement("Character")
	objNode.setAttribute "name", attrib_value
	objNode.text = node_value
	objRoot.appendChild objNode
End Function

```



## Vedit macro language

The input data is given in an edit buffer, one name+remark pair on each line line, separated with TAB character.

```vedit
// Replace special characters with entities:
Replace("&", "&amp;", BEGIN+ALL+NOERR)          // this must be the first replace!
Replace("<", "&lt;", BEGIN+ALL+NOERR)
Replace(">", "&gt;", BEGIN+ALL+NOERR)
Replace("'", "&apos;", BEGIN+ALL+NOERR)
Replace('"', "&quot;", BEGIN+ALL+NOERR)

// Insert XML marking
BOF
IT("<CharacterRemarks>") IN
Repeat(ALL) {
    Search("^.", REGEXP+ERRBREAK)
    IT('  <Character name="')
    Replace('|T', '">')
    EOL IT('</Character>')
}
EOF
IT("</CharacterRemarks>") IN
```


Example input:

```txt

April	Bubbly: I'm > Tam and <= Emily
Tam O'Shanter	Burns: "When chapman billies leave the street ..."
Emily	Short & shrift

```


Produces this output:

```txt

<CharacterRemarks>
  <Character name="April">Bubbly: I&amp;apos;m &amp;gt; Tam and &amp;lt;= Emily</Character>
  <Character name="Tam O&amp;apos;Shanter">Burns: &amp;quot;When chapman billies leave the street ...&amp;quot; </Character>
  <Character name="Emily">Short &amp;amp; shrift</Character>
</CharacterRemarks>

```



## Visual Basic .NET


```vbnet
Module XMLOutput
    Sub Main()
        Dim charRemarks As New Dictionary(Of String, String)
        charRemarks.Add("April", "Bubbly: I'm > Tam and <= Emily")
        charRemarks.Add("Tam O'Shanter", "Burns: ""When chapman billies leave the street ...""")
        charRemarks.Add("Emily", "Short & shrift")

        Dim xml = <CharacterRemarks>
                      <%= From cr In charRemarks Select <Character name=<%= cr.Key %>><%= cr.Value %></Character> %>
                  </CharacterRemarks>

        Console.WriteLine(xml)
    End Sub
End Module
```


Output:

```txt

<CharacterRemarks>
  <Character name="April">Bubbly: I'm &amp;gt; Tam and &amp;lt;= Emily</Character>
  <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street..."</Character>
  <Character name="Emily">Short &amp;amp; shrift</Character>
</CharacterRemarks>

```



## XQuery


First, we create two string sequences (ordered lists), $names and $remarks, through which we then will loop, using a counter, $count, to keep the current iteration index, which we then will apply as XPath predicate to select an item at this index from the second sequence.

In the second variant we use a function 'fn:for-each-pair#3', instead of the FLOWR, to accomplish the same task. XML construction is being encapsulated within a callback function here.

Please note a single derivation from the requested task at the third item of the second list: I had to replace the ampersand (&) character with its XML entity, otherwise the input would have not been valid for the XQuery processor.

Variant #1-1: using a FLOWR expression and element constructors


```xquery

let $names := ("April","Tam O'Shanter","Emily")
let $remarks := ("Bubbly: I'm > Tam and <= Emily", 'Burns: "When chapman billies leave the street ..."',"Short &amp; shrift")
return element CharacterRemarks {
                                  for $name at $count in $names
                                  return element Character {
                                                             attribute name { $name }
                                                            ,text { $remarks[$count] }
                                                            }
                                }

```


Variant #1-2: using a FLOWR expression and literal XML


```xquery

let $names := ("April","Tam O'Shanter","Emily")
let $remarks := ("Bubbly: I'm > Tam and <= Emily", 'Burns: "When chapman billies leave the street ..."',"Short &amp; shrift")
return <CharacterRemarks>
       {
         for $name at $count in $names
         return <Character name='{$name}'> {$remarks[$count]} </Character>
       }
       </CharacterRemarks>

```


Variant #2-1: using a Higher Order Function and element constructors


```xquery

xquery version "3.1";

let $names   := ("April","Tam O'Shanter","Emily")
let $remarks := ("Bubbly: I'm > Tam and <= Emily", 'Burns: "When chapman billies leave the street ..."',"Short &amp; shrift")
return element CharacterRemarks {
                                  for-each-pair($names, $remarks, function($name, $remark) {
                                                                                              element Character {
                                                                                                attribute name { $name }
                                                                                               ,text { $remark }
                                                                                            }
                                                                  })
                                }

```


Variant #2-2: using a Higher Order Function and literal XML expressions :)


```xquery

xquery version "3.1";

let $names := ("April","Tam O'Shanter","Emily")
let $remarks := ("Bubbly: I'm > Tam and <= Emily", 'Burns: "When chapman billies leave the street ..."',"Short &amp; shrift")
return <CharacterRemarks>
       {
         for-each-pair($names, $remarks, function($name, $remark) {
                                                                    <Character name='{$name}'> {$remark} </Character>
                                                                  })
       }
       </CharacterRemarks>

```


Result for all variants:


```xml

<CharacterRemarks>
  <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O'Shanter">Burns: "When chapman billies leave the street ..."</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```



## XPL0


```XPL0
code ChOut=8, CrLf=9, Text=12;
string 0;               \use zero-terminated strings

proc XmlOut(S);         \Output string in XML format
char S;
repeat  case S(0) of    \character entity substitutions
          ^<: Text(0, "&lt;");
          ^>: Text(0, "&gt;");
          ^&: Text(0, "&amp;");
          ^": Text(0, "&quot;");
          ^': Text(0, "&apos;")
        other ChOut(0, S(0));
        S:= S+1;
until S(0) = 0;

int Name, Remark, I;
[Name:= ["April",  "Tam O'Shanter",  "Emily"];
Remark:= ["Bubbly: I'm > Tam and <= Emily",
          "Burns: ^"When chapman billies leave the street ...^"",
          "Short & shrift"];
Text(0, "<CharacterRemarks>");  CrLf(0);
for I:= 0 to 3-1 do
        [Text(0, " <Character name=^"");
        XmlOut(Name(I));
        Text(0, "^">");
        XmlOut(Remark(I));
        Text(0, "</Character>");  CrLf(0);
        ];
Text(0, "</CharacterRemarks>");  CrLf(0);
]
```


{{out}}

```txt

<CharacterRemarks>
 <Character name="April">Bubbly: I&apos;m &gt; Tam and &lt;= Emily</Character>
 <Character name="Tam O&apos;Shanter">Burns: &quot;When chapman billies leave the street ...&quot;</Character>
 <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```



## Yabasic


```Yabasic
sign$ = "<,&lt;,>,&gt;,&,&amp;"
dim sign$(1)
long = token(sign$, sign$(), ",")

sub substitute_all$(s$)
    local i

    for i = 1 to long step 2
        if s$ = sign$(i) return sign$(i + 1)
    next i
    return s$
end sub

sub xmlquote_all$(s$)
    local i, res$

    for i = 1 to len(s$)
        res$ = res$ + substitute_all$(mid$(s$, i, 1))
    next i
    return res$
end sub

sub xml_CharacterRemarks$(datos$())
    local res$, i, long

    long = arraysize(datos$(), 1)

    res$ = "<CharacterRemarks>\n"

    for i = 1 to long
        res$ = res$ + "  <CharacterName=\"" + xmlquote_all$(datos$(i, 1)) + "\">" + xmlquote_all$(datos$(i, 2)) + "</Character>\n"
    next i
    return res$ + "</CharacterRemarks>\n"
end sub

data "April", "Bubbly: I'm > Tam and <= Emily"
data "Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\""
data "Emily", "Short & shrift"

dim testset$(3, 2)

for i = 1 to 3
    read testset$(i, 1), testset$(i, 2)
next i

print xml_CharacterRemarks$(testset$())
```



## zkl


```zkl
fcn xmlEscape(text){
   text.replace(" &"," &amp;")  .replace(" \""," &quot;")
       .replace(" '"," &apos;") .replace(" <"," &lt;") .replace(" >"," &gt;")
}
fcn toXML(as,bs){
   xml:=Sink("<CharacterRemarks>\n");
   as.zipWith('wrap(a,b){
      xml.write("  <Character name=\"",xmlEscape(a),"\">",
		xmlEscape(b),"</Character>\n");
   },bs);
   xml.write("</CharacterRemarks>\n").close();
}

toXML(T("April", "Tam O'Shanter", "Emily"),
       T("Bubbly: I'm > Tam and <= Emily",
         0'|Burns: "When chapman billies leave the street ..."|,
	 "Short & shrift"))
.print();
```

{{out}}

```txt

<CharacterRemarks>
  <Character name="April">Bubbly: I'm &gt; Tam and &lt;= Emily</Character>
  <Character name="Tam O'Shanter">Burns: &quot;When chapman billies leave the street ..."</Character>
  <Character name="Emily">Short &amp; shrift</Character>
</CharacterRemarks>

```




{{omit from|Batch File|No way of XML parsing or processing.}}
{{omit from|PARI/GP|No real capacity for string manipulation}}
