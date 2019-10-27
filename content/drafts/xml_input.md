+++
title = "XML/Input"
description = ""
date = 2019-09-15T16:04:38Z
aliases = []
[extra]
id = 3275
[taxonomies]
categories = []
tags = []
+++

{{task|XML}}

Given the following XML fragment, extract the list of ''student names'' using whatever means desired. If the only viable method is to use XPath, refer the reader to the task [[XML and XPath]].


```xml><Students

  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
```


Expected Output

 April
 Bob
 Chad
 Dave
 Émily


## 8th


```forth

 
\ Load the XML text into the var 'x':
quote *
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
* xml:parse var, x
 
\ print only xml nodes which have a tag of 'Student' and whose attributes are not empty
: .xml \ xml --
	xml:tag@ "Student" s:cmp if drop ;; then
	xml:attrs null? if drop ;; then 
 
	"Name" m:@ . cr drop ;
 
\ Iterate over the XML document in the var 'x'
x @ ' .xml xml:each bye

```

{{out}}
April

Bob

Chad

Dave

Émily


## ActionScript


```actionscript
package 
{
    import flash.display.Sprite;

    public class XMLReading extends Sprite
    {
        public function XMLReading()
        {
            var xml:XML = <Students>
                            <Student Name="April" />
                            <Student Name="Bob" />
                            <Student Name="Chad" />
                            <Student Name="Dave" />
                            <Student Name="Emily" />
                          </Students>;
            for each(var node:XML in xml..Student)
            {
                trace(node.@Name);
            }
        }
    }
}
```



## Ada

{{works with|GNAT}}

Uses [http://libre.adacore.com/libre/tools/xmlada/ XML/Ada] from AdaCore.

extract_students.adb:

```Ada
with Sax.Readers;
with Input_Sources.Strings;
with Unicode.CES.Utf8;
with My_Reader;

procedure Extract_Students is
   Sample_String : String :=
"<Students>" &
   "<Student Name=""April"" Gender=""F"" DateOfBirth=""1989-01-02"" />" &
   "<Student Name=""Bob"" Gender=""M"" DateOfBirth=""1990-03-04"" />" &
   "<Student Name=""Chad"" Gender=""M"" DateOfBirth=""1991-05-06"" />" &
   "<Student Name=""Dave"" Gender=""M"" DateOfBirth=""1992-07-08"">" &
      "<Pet Type=""dog"" Name=""Rover"" />" &
   "</Student>" &
   "<Student DateOfBirth=""1993-09-10"" Gender=""F"" Name=""&#x00C9;mily"" />" &
"</Students>";
   Reader : My_Reader.Reader;
   Input : Input_Sources.Strings.String_Input;
begin
   Input_Sources.Strings.Open (Sample_String, Unicode.CES.Utf8.Utf8_Encoding, Input);
   My_Reader.Parse (Reader, Input);
   Input_Sources.Strings.Close (Input);
end Extract_Students;
```


my_reader.ads:

```Ada
with Sax.Attributes;
with Sax.Readers;
with Unicode.CES;
package My_Reader is
   type Reader is new Sax.Readers.Reader with null record;
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);
end My_Reader;
```


my_reader.adb:

```Ada
with Ada.Text_IO;
package body My_Reader is
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class) is
   begin
      if Local_Name = "Student" then
         Ada.Text_IO.Put_Line (Sax.Attributes.Get_Value (Atts, "Name"));
      end if;
   end Start_Element;
end My_Reader;
```


Output:

```txt
April
Bob
Chad
Dave
Émily
```



### Alternative using a DOM document


```Ada
with Ada.Text_IO;
with Sax.Readers;
with Input_Sources.Strings;
with Unicode.CES.Utf8;
with DOM.Readers;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Attrs;

procedure Extract_Students is
   Sample_String : String :=
"<Students>" &
   "<Student Name=""April"" Gender=""F"" DateOfBirth=""1989-01-02"" />" &
   "<Student Name=""Bob"" Gender=""M"" DateOfBirth=""1990-03-04"" />" &
   "<Student Name=""Chad"" Gender=""M"" DateOfBirth=""1991-05-06"" />" &
   "<Student Name=""Dave"" Gender=""M"" DateOfBirth=""1992-07-08"">" &
      "<Pet Type=""dog"" Name=""Rover"" />" &
   "</Student>" &
   "<Student DateOfBirth=""1993-09-10"" Gender=""F"" Name=""&#x00C9;mily"" />" &
"</Students>";
   Input : Input_Sources.Strings.String_Input;
   Reader : DOM.Readers.Tree_Reader;
   Document : DOM.Core.Document;
   List : DOM.Core.Node_List;
begin
   Input_Sources.Strings.Open (Sample_String, Unicode.CES.Utf8.Utf8_Encoding, Input);
   DOM.Readers.Parse (Reader, Input);
   Input_Sources.Strings.Close (Input);
   Document := DOM.Readers.Get_Tree (Reader);
   List := DOM.Core.Documents.Get_Elements_By_Tag_Name (Document, "Student");
   for I in 0 .. DOM.Core.Nodes.Length (List) - 1 loop
      Ada.Text_IO.Put_Line
        (DOM.Core.Attrs.Value
           (DOM.Core.Nodes.Get_Named_Item
              (DOM.Core.Nodes.Attributes
                 (DOM.Core.Nodes.Item (List, I)), "Name")
           )
       );
   end loop;
   DOM.Readers.Free (Reader);
end Extract_Students;
```


output is the same.


###  Alternative version using Matreshka 


Uses [http://forge.ada-ru.org/matreshka Matreshka's SAX API for XML].

main.adb:


```Ada
with League.Application;
with XML.SAX.Input_Sources.Streams.Files;
with XML.SAX.Simple_Readers;

with Handlers;

procedure Main is
   Handler : aliased Handlers.Handler;
   Input   : aliased XML.SAX.Input_Sources.Streams.Files.File_Input_Source;
   Reader  : aliased XML.SAX.Simple_Readers.SAX_Simple_Reader;

begin
   Input.Open_By_File_Name (League.Application.Arguments.Element (1));
   Reader.Set_Content_Handler (Handler'Unchecked_Access);
   Reader.Parse (Input'Unchecked_Access);
end Main;
```


handlers.ads:


```Ada
with League.Strings;
with XML.SAX.Attributes;
with XML.SAX.Content_Handlers;

package Handlers is

   type Handler is
     limited new XML.SAX.Content_Handlers.SAX_Content_Handler with null record;

   overriding procedure Start_Element
    (Self           : in out Handler;
     Namespace_URI  : League.Strings.Universal_String;
     Local_Name     : League.Strings.Universal_String;
     Qualified_Name : League.Strings.Universal_String;
     Attributes     : XML.SAX.Attributes.SAX_Attributes;
     Success        : in out Boolean);

   overriding function Error_String
    (Self : Handler) return League.Strings.Universal_String;

end Handlers;
```


handlers.adb:


```Ada
with Ada.Wide_Wide_Text_IO;

package body Handlers is

   use type League.Strings.Universal_String;

   function "+"
    (Item : Wide_Wide_String) return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   ------------------
   -- Error_String --
   ------------------

   overriding function Error_String
    (Self : Handler) return League.Strings.Universal_String is
   begin
      return League.Strings.Empty_Universal_String;
   end Error_String;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
    (Self           : in out Handler;
     Namespace_URI  : League.Strings.Universal_String;
     Local_Name     : League.Strings.Universal_String;
     Qualified_Name : League.Strings.Universal_String;
     Attributes     : XML.SAX.Attributes.SAX_Attributes;
     Success        : in out Boolean) is
   begin
      if Qualified_Name = +"Student" then
         Ada.Wide_Wide_Text_IO.Put_Line
          (Attributes.Value (+"Name").To_Wide_Wide_String);
      end if;
   end Start_Element;

end Handlers;
```



## Aikido

Put the XML in the file called t.xml

```aikido

import xml

var s = openin ("t.xml")
var tree = XML.parseStream (s)

foreach node tree {
    if (node.name == "Students") {
        foreach studentnode node {
            if (studentnode.name == "Student") {
                println (studentnode.getAttribute ("Name"))
            }
        }
    }
}


```

The output is (Aikido doesn't support unicode rendering):
 April
 Bob
 Chad
 Dave
 &#x00C9;mily


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program inputXml.s   */

/* Constantes    */
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
.equ XML_ELEMENT_NODE,          1
.equ    XML_ATTRIBUTE_NODE,     2
 .equ   XML_TEXT_NODE,          3
.equ    XML_CDATA_SECTION_NODE, 4
.equ    XML_ENTITY_REF_NODE,    5
.equ    XML_ENTITY_NODE,        6
.equ    XML_PI_NODE,            7
.equ    XML_COMMENT_NODE,       8
.equ    XML_DOCUMENT_NODE,      9
.equ    XML_DOCUMENT_TYPE_NODE, 10
.equ   XML_DOCUMENT_FRAG_NODE,  11
.equ    XML_NOTATION_NODE,      12
.equ    XML_HTML_DOCUMENT_NODE, 13
.equ    XML_DTD_NODE,           14
.equ    XML_ELEMENT_DECL,       15
.equ    XML_ATTRIBUTE_DECL,     16
.equ    XML_ENTITY_DECL,        17
.equ    XML_NAMESPACE_DECL,     18
.equ    XML_XINCLUDE_START,     19
.equ    XML_XINCLUDE_END,       20
.equ    XML_DOCB_DOCUMENT_NODE  21

/*******************************************/
/* Structures                               */
/********************************************/
/* structure linkedlist*/
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


/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessEndpgm:      .asciz "Normal end of program.\n" 
szMessError:       .asciz "Error detected !!!!. \n"
szText:            .ascii "<Students>\n"
                   .ascii "<Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"
                   .ascii "<Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"
                   .ascii "<Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"
                   .ascii "<Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"
                   .ascii  "<Pet Type=\"dog\" Name=\"Rover\" />\n"
                   .ascii "</Student>\n"
                   .ascii "<Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"
                   .asciz "</Students>"
.equ LGSZTEXT,   . - szText            @ compute text size (. is current address)

szLibExtract:      .asciz "Student"
szLibName:         .asciz "Name"
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
    ldr r0,iAdrszText                     @ text buffer
    mov r1,#LGSZTEXT                      @ text size
    mov r2,#0                             @ param 3
    mov r3,#0                             @ param 4
    mov r4,#0                             @ param 5
    sub sp,#4                             @ stack assignement
    push {r4}                             @ param 5 on stack
    bl xmlReadMemory                      @ read text in document
    add sp,#8                             @ stack assignement for 1 push and align stack
    cmp r0,#0                             @ error ?
    beq 1f
    mov r9,r0                             @ doc address
    mov r0,r9
    bl xmlDocGetRootElement               @ search root return in r0
    bl affElement                         @ display elements

    mov r0,r9
    bl xmlFreeDoc
1:
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
iAdrszText:               .int szText
iAdrszCarriageReturn:     .int szCarriageReturn

/******************************************************************/
/*     display name of student                             */ 
/******************************************************************/
/* r0 contains the address of node */
affElement:
    push {r1-r4,lr}                   @ save  registres
    mov r4,r0                         @ save node
1:
    ldr r2,[r4,#xmlNode_type]         @ type ?
    cmp r2,#XML_ELEMENT_NODE
    bne 2f
    ldr r0,[r4,#xmlNode_name]         @ name = "Student" ?
    ldr r1,iAdrszLibExtract
    bl comparString
    cmp r0,#0
    bne 2f                            @ no
    mov r0,r4
    ldr r1,iAdrszLibName              @ load property of "Name"
    bl xmlHasProp
    cmp r0,#0
    beq 2f
    ldr r1,[r0,#xmlNode_children]     @ children node of property name
    ldr r0,[r1,#xmlNode_content]      @ and address of content
    bl affichageMess                  @ for display
    ldr r0,iAdrszCarriageReturn
    bl affichageMess
2:
    ldr r0,[r4,#xmlNode_children]     @ node have children ?
    cmp r0,#0
    blne affElement                   @ yes -> call procedure

    ldr r1,[r4,#xmlNode_next]         @ other element ?
    cmp r1,#0
    beq 100f                          @ no -> end procedure
 
    mov r4,r1                         @ else loop with next element
    b 1b

100:
    pop {r1-r4,lr}                    @ restaur registers */ 
    bx lr                             @ return  
iAdrszLibName:              .int szLibName
iAdrszLibExtract:           .int szLibExtract
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
/************************************/       
/* comparaison de chaines           */
/************************************/      
/* r0 et r1 contiennent les adresses des chaines */
/* retour 0 dans r0 si egalite */
/* retour -1 si chaine r0 < chaine r1 */
/* retour 1  si chaine r0> chaine r1 */
comparString:
    push {r1-r4}                @ save des registres
    mov r2,#0                   @ indice
1:    
    ldrb r3,[r0,r2]             @ octet chaine 1
    ldrb r4,[r1,r2]             @ octet chaine 2
    cmp r3,r4
    movlt r0,#-1                @ plus petite
    movgt r0,#1                 @ plus grande
    bne 100f                    @ pas egaux
    cmp r3,#0                   @ 0 final
    moveq r0,#0                 @ egalite
    beq 100f                    @ c'est la fin
    add r2,r2,#1                @ sinon plus 1 dans indice
    b 1b                        @ et boucle
100:
    pop {r1-r4}
    bx lr   

```

{{output}}

```txt

April
Bob
Chad
Dave
Émily
Normal end of program.

```


## AutoHotkey

simply using regular expressions

```AutoHotkey
students = 
(
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
)

quote = " ; "
pos = 1
while, pos := RegExMatch(students, "Name=.(\w+)" . quote . "\sGender"
, name, pos + 1)
names .= name1 . "`n"

msgbox % names
```


## AWK

The following code extracts the value of the property "Name" from every Student tag. It does not handle the <tt>&amp;#CODE;</tt>; this can be left to others: a way to cope with it fastly, is to output a very simple HTML structure, so that the interpretation is left to an HTML reader/browser.


```awk
function parse_buf()
{
    if ( match(buffer, /<Student[ \t]+[^>]*Name[ \t]*=[ \t]*"([^"]*)"/, mt) != 0 ) {
      students[mt[1]] = 1
    }
    buffer = ""
}

BEGIN {
  FS=""
  mode = 0
  buffer = ""
  li = 1
}

mode==1 {
  for(i=1; i <= NF; i++) {
    buffer = buffer $i
    if ( $i == ">" ) {
      mode = 0;
      break;
    }
  }
  if ( mode == 0 ) {
    li = i
  } else {
    li = 1
  }
  # let us process the buffer if "complete"
  if ( mode == 0 ) {
    parse_buf()
  }
}

mode==0 {
  for(i=li; i <= NF; i++) {
    if ( $i == "<" ) {
      mode = 1
      break;
    }
  }
  for(j=i; i <= NF; i++) {
    buffer = buffer $i
    if ( $i == ">" ) {
      mode = 0
      parse_buf()
    }
  }
  li = 1
}

END {
  for(k in students) {
    print k
  }
}
```

Using [http://awk.info/?getxml getXML.awk] written by Jan Weber, one could do this:

{{works with|gawk}} or {{works with|nawk}}

```awk
awk -f getXML.awk sample.xml | awk '
    $1 == "TAG"                 {tag = $2}
    tag == "Student" && /Name=/ {print substr($0, index($0, "=") + 1)}
'
```

Using [http://home.vrweb.de/~juergen.kahrs/gawk/XML/xmlgawk.html#Steve-Coile_0027s-xmlparse_002eawk-script xmlparser.awk] by Steve Coile, one can do this:

{{works with|gawk}}

```awk
gawk -f xmlparser.awk sample.xml | awk '
    $1 == "begin"                                         {tag = $2}
    $1 == "attrib"                                        {attrib = $2}
    $1 == "value" && tag == "STUDENT" && attrib == "name" {print $2}
'
```


Both of these produce this output

```txt
April
Bob
Chad
Dave
&amp;#x00C9;mily
```


{{works with|XMLgawk}}

Scripts in AWK are often [[wp:One-liner_program|one-liners]]. This one-liner implementation searches for ''Student'' tags and then displays the contents of their ''Name'' attribute. The following line is meant to be typed in on the command line of a [[Unix]] shell or an [[MS-DOS]] command window.
 gawk -lxml 'XMLSTARTELEM == "Student" {print XMLATTR["Name"]}' rosetta.xml
Output:
 April
 Bob
 Chad
 Dave
 Émily

## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"XMLLIB"
      xmlfile$ = "C:\students.xml"
      PROC_initXML(xmlobj{}, xmlfile$)
      
      level% = FN_skipTo(xmlobj{}, "Students", 0)
      IF level%=0 ERROR 100, "Students not found"
      
      REPEAT
        IF FN_isTag(xmlobj{}) THEN
          tag$ = FN_nextToken(xmlobj{})
          IF LEFT$(tag$, 8) = "<Student" THEN
            np% = INSTR(tag$, "Name")
            IF np% THEN PRINT FN_repEnt(EVAL(MID$(tag$, np%+5)))
          ENDIF
        ELSE
          dummy$ = FN_nextToken(xmlobj{})
        ENDIF
      UNTIL FN_getLevel(xmlobj{}) < level%
      
      PROC_exitXML(xmlobj{})
```

Output:

```txt

April
Bob
Chad
Dave
Émily

```


## Bracmat

The <code>get</code> function can read markup into a native datastructure, which can be analysed using pattern matching.
The read datastructure is a flat list of tags and text fragments. For proper nesting of elements extra code would have to be written, but in this simple task that is not necessary. On the downside, the pattern must both handle empty tags (the <code>(? (Name.?name) ?,</code> pattern) and open tags (the <code>? (Name.?name) ?</code> pattern).
Reading input from a file:

```bracmat
( :?names
& (   get$("students.xml",X,ML)
    :   ?
        ( ( Student
          .   (? (Name.?name) ?,)
            | ? (Name.?name) ?
          )
        & !names !name:?names
        & ~
        )
        ?
  | !names
  )
)
```


Alternative solution, reading input from memory:

```bracmat
( :?names
& (     get
      $ ( "<Students>
  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />
  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />
  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />
  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">
    <Pet Type=\"dog\" Name=\"Rover\" />
  </Student>
  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />
</Students>"
        , MEM
        , X
        , ML
        )
    :   ?
        ( ( Student
          .   (? (Name.?name) ?,)
            | ? (Name.?name) ?
          )
        & !names !name:?names
        & ~
        )
        ?
  | !names
  )
)
```

Output:

```txt
April Bob Chad Dave Émily
```



## C


{{libheader|LibXML}}
{{uses from|Library|libxml|component1=xmlDoc|component2=xmlNode|component3=xmlReadMemory|component4=xmlDocGetRootElement|component5=xmlFreeDoc|component6=xmlCleanupParser|component7=xmlNode|component8=XML_ELEMENT_NODE|component9=xmlAttr|component10=xmlHasProp}}
{{uses from|Library|C Runtime|component1=printf}}

```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

static void print_names(xmlNode *node)
{
  xmlNode *cur_node = NULL;
  for (cur_node = node; cur_node; cur_node = cur_node->next) {
    if (cur_node->type == XML_ELEMENT_NODE) {
      if ( strcmp(cur_node->name, "Student") == 0 ) {
	xmlAttr *prop = NULL;
	if ( (prop = xmlHasProp(cur_node, "Name")) != NULL ) {
	  printf("%s\n", prop->children->content);
	  
	}
      }
    }
    print_names(cur_node->children);
  }
}

const char *buffer =
  "<Students>\n"
  "  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"
  "  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"
  "  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"
  "  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"
  "    <Pet Type=\"dog\" Name=\"Rover\" />\n"
  "  </Student>\n"
  "  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"
  "</Students>\n";

int main()
{
  xmlDoc *doc = NULL;
  xmlNode *root = NULL;

  doc = xmlReadMemory(buffer, strlen(buffer), NULL, NULL, 0);
  if ( doc != NULL ) {
    root = xmlDocGetRootElement(doc);
    print_names(root);
    xmlFreeDoc(doc);
  }
  xmlCleanupParser();
  return 0;
}
```



## C++


{{libheader|Qt}}
{{uses from|Library|Qt|component1=QDomDocument|component2=QObject|component3=QDomElement}}


```cpp
/*
Using the Qt library's XML parser.
*/
#include <iostream>

#include <QDomDocument>
#include <QObject>

int main() {
    QDomDocument doc;

    doc.setContent(
       QObject::tr(
          "<Students>\n"
          "<Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"
          "<Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"
          "<Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"
          "<Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"
          "<Pet Type=\"dog\" Name=\"Rover\" />\n"
          "</Student>\n"
          "<Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"
          "</Students>"));
    
    QDomElement n = doc.documentElement().firstChildElement("Student");
    while(!n.isNull()) {
        std::cout << qPrintable(n.attribute("Name")) << std::endl;
        n = n.nextSiblingElement();
    }
    return 0;
}
```



## C sharp


```csharp

class Program
{
    static void Main(string[] args)
    {   
        XDocument xmlDoc = XDocument.Load("XMLFile1.xml");
        var query = from p in xmlDoc.Descendants("Student")
                    select p.Attribute("Name");

        foreach (var item in query)
        {
            Console.WriteLine(item.Value);
        }
        Console.ReadLine();
    }  
}

```


=={{header|Caché ObjectScript}}==


```cos
Class XML.Students [ Abstract ]
{

XData XMLData
{
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
}

ClassMethod Output() As %Status
{
   // get xml stream from the 'XData' block contained in this class and parse
   Set xdata=##class(%Dictionary.CompiledXData).%OpenId($this_"||XMLData",, .sc)
   If $$$ISERR(sc) Quit sc
   Set sc=##class(%XML.TextReader).ParseStream(xdata.Data, .hdlr)
   If $$$ISERR(sc) Quit sc
   
   // iterate through document, node by node
   While hdlr.Read() {
	   If hdlr.Path="/Students/Student", hdlr.MoveToAttributeName("Name") {
		   Write hdlr.Value, !
	   }
   }
   
   // finished
   Quit $$$OK
}

}
```

{{out|Examples}}

```txt

USER>Do ##class(XML.Students).Output()
April
Bob
Chad
Dave
Émily

```



## Clojure

{{uses from|Library|java.io|component1=ByteArrayInputStream}}
{{uses from|Library|clojure.xml|component1=parse}}
This version uses the standard Clojure function ''xml-seq'

```lisp

(import '(java.io ByteArrayInputStream))
(use 'clojure.xml) ; defines 'parse

(def xml-text "<Students>
  <Student Name='April' Gender='F' DateOfBirth='1989-01-02' />
  <Student Name='Bob' Gender='M'  DateOfBirth='1990-03-04' />
  <Student Name='Chad' Gender='M'  DateOfBirth='1991-05-06' />
  <Student Name='Dave' Gender='M'  DateOfBirth='1992-07-08'>
    <Pet Type='dog' Name='Rover' />
  </Student>
  <Student DateOfBirth='1993-09-10' Gender='F' Name='&#x00C9;mily' />
</Students>")

(def students (parse (-> xml-text .getBytes ByteArrayInputStream.)))

```


The parse produces a data structure where each element is represented as a map with '':tag'', '':attrs'', and '':content'' keys.
Thus the "April" element becomes ''{:tag :Student, :attrs {:Name "April", :Gender "F", :DateOfBirth "1989-01-02"}, :content nil}''.
''xml-seq'' produces a sequence of such nodes by walking the resulting tree.


```lisp

(doseq [{:keys [tag attrs]} (xml-seq students)]
  (if (= :Student tag)
    (println (:Name attrs))))

```



## Common Lisp


{{libheader|Closure XML}}


```lisp
(defparameter *xml-blob*
"<Students>
  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />
  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />
  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />
  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">
    <Pet Type=\"dog\" Name=\"Rover\" />
  </Student>
  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />
</Students>")

(let* ((document (cxml:parse *xml-blob* (cxml-dom:make-dom-builder)))
       (students (dom:item (dom:get-elements-by-tag-name document "Students") 0))
       (student-names '()))
  (dom:do-node-list (child (dom:child-nodes students) (nreverse student-names))
    (when (dom:element-p child)
      (push (dom:get-attribute child "Name") student-names))))
```


produces
```lisp
("April" "Bob" "Chad" "Dave" "Émily")
```



## D

{{libheader|KXML}}

```d
import kxml.xml;
char[]xmlinput =
"<Students>
  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />
  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />
  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />
  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">
    <Pet Type=\"dog\" Name=\"Rover\" />
  </Student>
  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />
</Students>";

void main() {
        auto root = readDocument(xmlinput);
        foreach(students;root.getChildren) if (!students.isCData && students.getName == "Students") {
                // now look for student subnodes
                foreach(student;students.getChildren) if (!student.isCData && student.getName == "Student") {
                        // we found a student!
                        std.stdio.writefln("%s",student.getAttribute("Name"));
                }
                // we only want one, so break out of the loop once we find a match
                break;
        }
}
```



## Delphi


```Delphi

//You need to use these units
uses
  SysUtils,
  Dialogs,
  XMLIntf,
  XMLDoc;

//..............................................

//This function process the XML
function GetStudents(aXMLInput: string): string;
var
  XMLDoc: IXMLDocument;
  i: Integer;
begin
  //Creating the TXMLDocument instance
  XMLDoc:= TXMLDocument.Create(nil);

  //Loading8 the XML string
  XMLDoc.LoadFromXML(aXMLInput);

  //Parsing the xml document
  for i:=0 to XMLDoc.DocumentElement.ChildNodes.Count - 1 do
    Result:= Result + XMLDoc.DocumentElement.ChildNodes.Get(i).GetAttributeNS('Name', '') + #13#10;

  //Removing the trailing #13#10 characters
  Result:= Trim(Result);
end;

//..............................................

//Consuming code example (fragment)
var
  XMLInput: string;
begin
  XMLInput:= '<Students>' +
                '<Student Name="April" Gender="F" DateOfBirth="1989-01-02" />' +
                '<Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />' +
                '<Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />' +
                '<Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">' +
                  '<Pet Type="dog" Name="Rover" />' +
                '</Student>' +
                '<Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />'+
              '</Students>';
  Showmessage(GetStudents(XMLInput));
end;

```




## Erlang


```Erlang

-module( xml_input ).

-export( [task/0] ).

-include_lib("xmerl/include/xmerl.hrl").

task() ->
    {XML, []} = xmerl_scan:string( xml(), [{encoding, "iso-10646-utf-1"}] ),
    Attributes = lists:flatten( [X || #xmlElement{name='Student', attributes=X} <- XML#xmlElement.content] ),
    [io:fwrite("~s~n", [X]) || #xmlAttribute{name='Name', value=X} <- Attributes].



xml() -> "<Students>
  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />
  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />
  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />
  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">
    <Pet Type=\"dog\" Name=\"Rover\" />
  </Student>
  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />
</Students>".

```

{{out}}

```txt

38> xml_input:task().
April
Bob
Chad
Dave
Émily

```


=={{header|F_Sharp|F#}}==

```fsharp
open System.IO
open System.Xml
open System.Xml.Linq

let xn s = XName.Get(s)

let xd = XDocument.Load(new StringReader("""
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
"""))  // "



[<EntryPoint>]
let main argv =
    let students = xd.Descendants <| xn "Student"
    let names = students.Attributes <| xn "Name"
    Seq.iter ((fun (a : XAttribute) ->  a.Value) >> printfn "%s") names
    0
```



## Factor


```factor
USING: io multiline sequences xml xml.data xml.traversal ;

: print-student-names ( string -- )
    string>xml "Student" tags-named [ "Name" attr print ] each ;

[[ <Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>]] print-student-names
```



## Fantom


```fantom

using xml

class XmlInput
{
  public static Void main () 
  {
    // create the XML parser
    parser := XParser(File("sample-xml.xml".toUri).in)
    // parse the document, creating an XML document
    XDoc doc := parser.parseDoc
    // walk through each child element from the root of the document
    doc.root.elems.each |elem|
    {
      // printing the Name attribute of all Students
      if (elem.name == "Student") { echo (elem.get("Name")) }
    }
  }
}

```



## Forth


{{libheader|Forth Foundation Library}}


```forth
include ffl/est.fs
include ffl/str.fs
include ffl/xis.fs

\ Build input string
str-create xmlstr   
: x+ xmlstr str-append-string ;

s\" <Students>\n" x+
s\" <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n" x+
s\" <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"  x+
s\" <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n" x+
s\" <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"   x+
s\" <Pet Type=\"dog\" Name=\"Rover\" />\n" x+
s\" </Student>\n" x+
s\" <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n" x+
s\" </Students>\n" x+

\ Setup xml parser
xis-create xmlparser
xmlstr str-get xmlparser xis-set-string

\ Parse the xml
: xmlparse
  BEGIN
    xmlparser xis-read  dup xis.error <> over xis.done <> AND
  WHILE
    dup xis.start-tag = over xis.empty-element = OR IF
      drop
      s" Student" compare 0= IF
        0 ?DO
          2swap s" Name" compare 0= IF
            type cr
          ELSE
            2drop
          THEN
        LOOP
      ELSE
        xis+remove-attribute-parameters
      THEN
    ELSE
      xis+remove-read-parameters
    THEN
  REPEAT
  drop
;

xmlparse
```




## Fortran



###  tixi library 


Uses [https://github.com/DLR-SC/tixi tixi library]  (+ LibXML, curl as dependencies)


```fortran

program tixi_rosetta
  use tixi
  implicit none
  integer :: i
  character (len=100) :: xml_file_name 
  integer :: handle
  integer :: error	
  character(len=100) :: name, xml_attr
  xml_file_name = 'rosetta.xml'

  call tixi_open_document( xml_file_name, handle, error )
  i = 1
  do 
      xml_attr = '/Students/Student['//int2char(i)//']'
      call tixi_get_text_attribute( handle, xml_attr,'Name', name, error )      
      if(error /= 0) exit
      write(*,*) name
      i = i + 1
  enddo

  call tixi_close_document( handle, error )

  contains

  function int2char(i) result(res)
    character(:),allocatable :: res
    integer,intent(in) :: i
    character(range(i)+2) :: tmp
    write(tmp,'(i0)') i
    res = trim(tmp)
  end function int2char

end program tixi_rosetta

```


Compile

```txt

gfortran tixi_rosetta.f90 tixi.f90 -L../tixi/tixi-master/build/lib/ -lTIXI -lTIXIf  -lxml2 -lcurl -o tixi_rosetta.x

```


Output

```txt

./tixi_rosetta.x 
 April                                                                                               
 Bob                                                                                                 
 Chad                                                                                                
 Dave                                                                                                
 Émily

```


=== Fortran XML library (FoX) ===
Uses [https://github.com/andreww/fox FoX]


```fortran

program fox_rosetta
   use FoX_dom
   use FoX_sax
   implicit none
   integer :: i
   type(Node), pointer :: doc => null()
   type(Node), pointer :: p1 => null()
   type(Node), pointer :: p2 => null()
   type(NodeList), pointer :: pointList => null()
   character(len=100) :: name

   doc => parseFile("rosetta.xml")
   if(.not. associated(doc)) stop "error doc"

   p1 => item(getElementsByTagName(doc, "Students"), 0)
   if(.not. associated(p1)) stop "error p1"
   ! write(*,*) getNodeName(p1)

   pointList => getElementsByTagname(p1, "Student")
   ! write(*,*) getLength(pointList), "Student elements"

   do i = 0, getLength(pointList) - 1
      p2 => item(pointList, i)
      call extractDataAttribute(p2, "Name", name)
      write(*,*) name
   enddo

   call destroy(doc)
end program fox_rosetta

```


Output

```txt

./fox_rosetta.x 
 April                                                                                               
 Bob                                                                                                 
 Chad                                                                                                
 Dave                                                                                                
 &mily

```



## Go

Go's <tt>xml.Unmarshal</tt> uses reflection to fill in data-structures recursively.

```go
package main

import (
    "encoding/xml"
    "fmt"
)

const XML_Data = `
<Students>
   <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
   <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
   <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
   <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
      <Pet Type="dog" Name="Rover" />
   </Student>
   <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
`

type Students struct {
    Student []Student
}

type Student struct {
    Name string `xml:",attr"`
    //  Gender      string `xml:",attr"`
    //  DateOfBirth string `xml:",attr"`
    //  Pets        []Pet  `xml:"Pet"`
}

type Pet struct {
    Type string `xml:",attr"`
    Name string `xml:",attr"`
}

// xml.Unmarshal quietly skips well formed input with no corresponding
// member in the output data structure.  With Gender, DateOfBirth, and
// Pets commented out of the Student struct, as above, Student contains
// only Name, and this is the only value extracted from the input XML_Data.
func main() {
    var data Students
    err := xml.Unmarshal([]byte(XML_Data), &data)
    if err != nil {
        fmt.Println(err)
        return
    }
    for _, s := range data.Student {
        fmt.Println(s.Name)
    }
}
```

{{out}}

```txt

April
Bob
Chad
Dave
Émily

```



## Groovy


```groovy
def input = """<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>"""

def students = new XmlParser().parseText(input)
students.each { println it.'@Name' }
```



## Haskell


```haskell
import Data.Maybe
import Text.XML.Light

students="<Students>"++
        " <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />"++
        " <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />"++
        " <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\"/>"++
        " <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">"++
        "   <Pet Type=\"dog\" Name=\"Rover\" />  </Student>"++
        " <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />"++
        "</Students>"

xmlRead elm name  = mapM_ putStrLn 
      . concatMap (map (fromJust.findAttr (unqual name)).filterElementsName (== unqual elm))
      . onlyElems.  parseXML
```

Show names:

```haskell
*Main> xmlRead "Student" "Name" students
April
Bob
Chad
Dave
Émily
```



## HicEst


```HicEst
CHARACTER in*1000, out*100

READ(ClipBoard) in
EDIT(Text=in, SPR='"', Right='<Student', Right='Name=', Word=1, WordEnd, APpendTo=out, DO)
```


```txt
out is returned as:
April  Bob  Chad  Dave  &#x00C9;mily

```



## J

J's system includes several XML processing libraries.  This task is probably best addressed using XPath (this is the type of problem XPath was designed to solve), but the task description implicitly discourages that method.  So we can use the SAX library instead:


```j
load'xml/sax'
 
saxclass 'Students'
startElement =: ([: smoutput 'Name' getAttribute~ [)^:('Student'-:])
cocurrent'base'
 
process_Students_ XML
```

 April
 Bob
 Chad
 Dave
 Émily

and the definition of the variable <code>XML</code>:

```j
XML=: noun define
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
)
```



## Java

{{uses from|Library|java.io|component1=IOException|component2=StringReader}}
{{uses from|Library|org.xml.sax|component1=Attributes|component2=InputSource|component3=SAXException|component4=XMLReader|component5=helpers.DefaultHandler|component6=helpers.XMLReaderFactory}}

```java
import java.io.IOException;
import java.io.StringReader;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public class StudentHandler extends DefaultHandler {
  public static void main(String[] args)throws Exception{
    String xml = "<Students>\n"+
    "<Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"+
    "<Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"+
    "<Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"+
    "<Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"+
    "  <Pet Type=\"dog\" Name=\"Rover\" />\n"+
    "</Student>\n"+
    "<Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"+
    "</Students>";
    StudentHandler handler = new StudentHandler();
    handler.parse(new InputSource(new StringReader(xml)));
  }

  public void parse(InputSource src) throws SAXException, IOException {
		XMLReader parser = XMLReaderFactory.createXMLReader();
    parser.setContentHandler(this);
    parser.parse(src);
  }

  @Override
  public void characters(char[] ch, int start, int length) throws SAXException {
    //if there were text as part of the elements, we would deal with it here
    //by adding it to a StringBuffer, but we don't have to for this task
    super.characters(ch, start, length);
  }

  @Override
  public void endElement(String uri, String localName, String qName) throws SAXException {
    //this is where we would get the info from the StringBuffer if we had to,
    //but all we need is attributes
    super.endElement(uri, localName, qName);
  }

  @Override
  public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
    if(qName.equals("Student")){
      System.out.println(attributes.getValue("Name"));
    }
  }
}
```



## JavaScript



###  Browser version 

This version tested against Chrome 37, Firefox 32, and IE 11:

```JavaScript

var xmlstr = '<Students>' + 
  '<Student Name="April" Gender="F" DateOfBirth="1989-01-02" />' +
  '<Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />' +
  '<Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />' +
  '<Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">' +
    '<Pet Type="dog" Name="Rover" />' +
  '</Student>' +
  '<Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />' +
'</Students>';

if (window.DOMParser)
  {
  parser=new DOMParser();
  xmlDoc=parser.parseFromString(xmlstr,"text/xml");
  }
else // Internet Explorer
  {
  xmlDoc=new ActiveXObject("Microsoft.XMLDOM");
  xmlDoc.async=false;
  xmlDoc.loadXML(xmlstr); 
  }

var students=xmlDoc.getElementsByTagName('Student');
for(var e=0; e<=students.length-1; e++) {
  console.log(students[e].attributes.Name.value);
}

```

{{works with|Mozilla Firefox|32}}


###  Node.js version 


```JavaScript

var parseString = require('xml2js').parseString;
var xmlstr = '<Students>' +
  '<Student Name="April" Gender="F" DateOfBirth="1989-01-02" />' +
  '<Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />' +
  '<Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />' +
  '<Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">' +
    '<Pet Type="dog" Name="Rover" />' +
  '</Student>' +
  '<Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />' +
'</Students>';

parseString(xmlstr, function (err, result) {
   if (!err) {
      result.Students.Student.forEach( function(student) {
         console.log(student.$.Name);
      } );
   }
});

```



###  E4X version 

Alternatively, use the E4X featureset (currently only in Firefox):

```JavaScript

var xmlstr = '<Students>' + 
  '<Student Name="April" Gender="F" DateOfBirth="1989-01-02" />' +
  '<Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />' +
  '<Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />' +
  '<Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">' +
    '<Pet Type="dog" Name="Rover" />' +
  '</Student>' +
  '<Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />' +
'</Students>';
var xml = XML(xmlstr);
var list = xml.Student.@Name;
var output = '';
for (var i = 0; i < list.length(); i++) {
  if (i > 0) {
    output += ', ';
  }
  output += list[i];
}

alert(output);

```



## Julia

{{works with|Julia|0.6}}


```julia
using LightXML

let docstr = """<Students>
      <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
      <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
      <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
      <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
        <Pet Type="dog" Name="Rover" />
      </Student>
      <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
    </Students>"""

    doc = parse_string(docstr)
    xroot = root(doc)
    for elem in xroot["Student"]
        println(attribute(elem, "Name"))
    end
end
```


{{out}}

```txt
April
Bob
Chad
Dave
Émily
```



## Kotlin

As this is just a small XML document, the DOM parser has been used rather than the SAX parser: 

```scala
// version 1.1.3

import javax.xml.parsers.DocumentBuilderFactory
import org.xml.sax.InputSource
import java.io.StringReader
import org.w3c.dom.Node
import org.w3c.dom.Element

val xml = 
"""
<Students>
    <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
    <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
    <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
    <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
        <Pet Type="dog" Name="Rover" />
    </Student>
    <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
"""

fun main(args: Array<String>) {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder  = dbFactory.newDocumentBuilder()
    val xmlInput = InputSource(StringReader(xml))
    val doc = dBuilder.parse(xmlInput)
    val nList = doc.getElementsByTagName("Student")
    for (i in 0 until nList.length) {
        val node = nList.item(i)
        if (node.nodeType == Node.ELEMENT_NODE) {
            val element = node as Element
            val name = element.getAttribute("Name")
            println(name)
        }
    } 
}
```


{{out}}

```txt

April
Bob
Chad
Dave
Émily

```



## Lasso

Task calls for a result not using Xpaths. Thus two examples shown. First uses Xpath, second uses regular expression.

```Lasso
// makes extracting attribute values easier
define xml_attrmap(in::xml_namedNodeMap_attr) => {
	local(out = map)
	with attr in #in
		do #out->insert(#attr->name = #attr->value)
	return #out
}

local(
	text = '<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
',
xml = xml(#text)
)

local(
	students	= #xml -> extract('//Student'),
	names		= array
)
with student in #students do {
	#names -> insert(xml_attrmap(#student -> attributes) -> find('Name'))
}
#names -> join('<br />')

```


```Lasso
// not using XML or Xpath
'<hr />'
local(
	regexp	= regexp(-find = `<Student.*?Name="(.*?)"`, -input = #text, -ignoreCase),
	names	= array
)

while( #regexp -> find) => {
	#names -> insert(#regexp -> matchstring(1))
}
#names -> join('<br />')
```

Output:

```txt
April
Bob
Chad
Dave
Émily
-----------
April
Bob
Chad
Dave
Émily
```



## Lingo


```lingo
q = QUOTE
r = RETURN
xml = "<Students>"&r&\
"  <Student Name="&q&"April"&q&" Gender="&q&"F"&q&" DateOfBirth="&q&"1989-01-02"&q&" />"&r&\
"  <Student Name="&q&"Bob"&q&" Gender="&q&"M"&q&" DateOfBirth="&q&"1990-03-04"&q&" />"&r&\
"  <Student Name="&q&"Chad"&q&" Gender="&q&"M"&q&" DateOfBirth="&q&"1991-05-06"&q&" />"&r&\
"  <Student Name="&q&"Dave"&q&" Gender="&q&"M"&q&" DateOfBirth="&q&"1992-07-08"&q&">"&r&\
"    <Pet Type="&q&"dog"&q&" Name="&q&"Rover"&q&" />"&r&\
"  </Student>"&r&\
"  <Student DateOfBirth="&q&"1993-09-10"&q&" Gender="&q&"F"&q&" Name="&q&"&#x00C9;mily"&q&" />"&r&\
"</Students>"

parser = xtra("xmlparser").new()
parser.parseString(xml)
res = parser.makePropList()	
repeat with c in res.child
  put c.attributes.name
end repeat
```


{{out}}

```txt

-- "April"
-- "Bob"
-- "Chad"
-- "Dave"
-- "Émily"

```



## LiveCode

Put the XML text in a text field called FieldXML for this exercise.

```LiveCode
put revXMLCreateTree(fld "FieldXML",true,true,false) into currTree
put revXMLAttributeValues(currTree,"Students","Student","Name",return,-1)
```

    

## Lua

Requires LuaExpat

```lua

require 'lxp'
data = [[<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>]]

p = lxp.new({StartElement = function (parser, name, attr)
	if name == 'Student' and attr.Name then
		print(attr.Name)
	end
end})

p:parse(data)
p:close()

```

Output:

```txt

April 
Bob 
Chad 
Dave 
Émily
```



## M2000 Interpreter

Works with Wine too (Linux).
Declare Object Nothing is optional. COM objects deleted when module exit by default.


```M2000 Interpreter

Module CheckIt {
      Const Enumerator=-4&
      xml$={<Students>
              <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
              <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
              <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
              <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
                <Pet Type="dog" Name="Rover" />
              </Student>
              <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
            </Students>
            }
      Declare Dom "Msxml2.DOMDocument"
      Method Dom, "LoadXML", xml$
      Method Dom, "getElementsByTagName", "Student" as Students
      
      With Students, Enumerator as Student
      While Student {
            Method Student, "getAttribute", "Name" as Student.Name$
            Print Student.Name$
      }
      Declare Student Nothing
      Declare Students Nothing
      Declare DOM Nothing
}
CheckIt

```

{{out}}

```txt

April 
Bob 
Chad 
Dave 
Émily
</pre >


## Mathematica


```Mathematica
Column[Cases[Import["test.xml","XML"],Rule["Name", n_ ] -> n,Infinity]]
```

Output:

```txt

April 
Bob 
Chad 
Dave 
Émily
```



## Matlab


```Matlab

RootXML = com.mathworks.xml.XMLUtils.createDocument('Students');
docRootNode = RootXML.getDocumentElement;
thisElement = RootXML.createElement('Student');
thisElement.setAttribute('Name','April')
thisElement.setAttribute('Gender','F')
thisElement.setAttribute('DateOfBirth','1989-01-02')
docRootNode.appendChild(thisElement);
thisElement = RootXML.createElement('Student');
thisElement.setAttribute('Name','Bob')
thisElement.setAttribute('Gender','M')
thisElement.setAttribute('DateOfBirth','1990-03-04')
docRootNode.appendChild(thisElement);
thisElement = RootXML.createElement('Student');
thisElement.setAttribute('Name','Chad')
thisElement.setAttribute('Gender','M')
thisElement.setAttribute('DateOfBirth','1991-05-06')
docRootNode.appendChild(thisElement);
thisElement = RootXML.createElement('Student');
thisElement.setAttribute('Name','Dave')
thisElement.setAttribute('Gender','M')
thisElement.setAttribute('DateOfBirth','1992-07-08')
node = RootXML.createElement('Pet');
node.setAttribute('Type','dog')
node.setAttribute('name','Rover')
thisElement.appendChild(node);
docRootNode.appendChild(thisElement);
thisElement = RootXML.createElement('Student');
thisElement.setAttribute('Name','Émily')
thisElement.setAttribute('Gender','F')
thisElement.setAttribute('DateOfBirth','1993-09-10')
docRootNode.appendChild(thisElement);
clearvars -except RootXML

for I=0:1:RootXML.getElementsByTagName('Student').item(0).getAttributes.getLength-1
    if strcmp(RootXML.getElementsByTagName('Student').item(0).getAttributes.item(I).getName,'Name')
        tag=I;
        break
    end
end

for I=0:1:RootXML.getElementsByTagName('Student').getLength-1
    disp(RootXML.getElementsByTagName('Student').item(I).getAttributes.item(tag).getValue)
end

```

Output:

```txt

April 
Bob 
Chad 
Dave 
Émily
```



## Neko


```ActionScript
/**
 XML/Input in Neko
 Tectonics:
   nekoc xml-input.neko
   neko xml-input | recode html
*/

/* Get the Neko XML parser function */
var parse_xml = $loader.loadprim("std@parse_xml", 2);

/* Load the student.xml file as string */
var file_contents = $loader.loadprim("std@file_contents", 1);
var xmlString = file_contents("students.xml");

/* Build up a (very specific) XML event processor object */
/* Needs functions for xml, done, pcdata, cdata and comment */
var events = $new(null);
events.xml = function(name, attributes) {
  if name == "Student" {
    $print(attributes.Name, "\n");
  }
}
events.done = function() { }
events.pcdata = function(x) { }
events.cdata = function(x) { }
events.comment = function(x) { }

parse_xml(xmlString, events);

/* Entities are not converted, use external recode program for that */
```


{{out}}

```txt

prompt$ nekoc xml-input.neko
prompt$ neko xml-input.n
```

```xml
April
Bob
Chad
Dave
&#x00C9;mily
```


```txt

prompt$ neko xml-input.n | recode html
April
Bob
Chad
Dave
Émily
```



## newLISP


```newlisp

(set 'xml-input "<Students>
  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />
  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />
  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />
  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">
    <Pet Type=\"dog\" Name=\"Rover\" />
  </Student>
  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />
</Students>")

(set 'sexp (xml-parse xml-input))

(dolist (x (ref-all "Name" sexp))	
	(if (= (length x) 6)
		(println (last (sexp (chop x))))))

```


Output:

```txt

April 
Bob 
Chad 
Dave 
Émily
```



## Nim


```nim
import xmlparser, xmltree, streams

let doc = newStringStream """<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>"""

for i in doc.parseXml.findAll "Student":
  echo i.attr "Name"
```

Output:

```txt
April
Bob
Chad
Dave
Émily
```



## Objeck


```objeck

use XML;

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      in := String->New();
      in->Append("<Students>");
      in->Append("<Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />");
      in->Append("<Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />");
      in->Append("<Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />");
      in->Append("<Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">");
      in->Append("<Pet Type=\"dog\" Name=\"Rover\" />");
      in->Append("</Student>");
      in->Append("<Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" /></Students>");
    
      parser := XmlParser->New(in);
      if(parser->Parse()) {
        root := parser->GetRoot();
        children := root->GetChildren("Student");
        each(i : children) {
          child : XMLElement := children->Get(i)->As(XMLElement);
          XMLElement->DecodeString(child->GetAttribute("Name"))->PrintLine();
        };
      };
    }
  }
}

```



## OCaml

from the toplevel using the library [http://tech.motion-twin.com/xmllight.html xml-light]:

```ocaml
# #directory "+xml-light" (* or maybe "+site-lib/xml-light" *) ;;
# #load "xml-light.cma" ;;

# let x = Xml.parse_string "
  <Students>
    <Student Name='April' Gender='F' DateOfBirth='1989-01-02' />
    <Student Name='Bob' Gender='M'  DateOfBirth='1990-03-04' />
    <Student Name='Chad' Gender='M'  DateOfBirth='1991-05-06' />
    <Student Name='Dave' Gender='M'  DateOfBirth='1992-07-08'>
      <Pet Type='dog' Name='Rover' />
    </Student>
    <Student DateOfBirth='1993-09-10' Gender='F' Name='&#x00C9;mily' />
  </Students>"
  in
  Xml.iter (function
    Xml.Element ("Student", attrs, _) ->
       List.iter (function ("Name", name) -> print_endline name | _ -> ()) attrs
  | _ -> ()) x
  ;;
April
Bob
Chad
Dave
&#x00C9;mily
- : unit = ()
```


Another solution using the library [http://erratique.ch/software/xmlm xmlm]:

```ocaml
#directory "+xmlm"
#load "xmlm.cmo"
open Xmlm

let str = "
  <Students>
    <Student Name='April' Gender='F' DateOfBirth='1989-01-02' />
    <Student Name='Bob'   Gender='M' DateOfBirth='1990-03-04' />
    <Student Name='Chad'  Gender='M' DateOfBirth='1991-05-06' />
    <Student Name='Dave'  Gender='M' DateOfBirth='1992-07-08'>
      <Pet Type='dog' Name='Rover' />
    </Student>
    <Student DateOfBirth='1993-09-10' Gender='F' Name='&#x00C9;mily' />
  </Students>"


let xi = make_input(`String(0, str))

let () =
  while not(eoi xi) do
    match Xmlm.input xi with
    | `El_start ((_, "Student"), attrs) ->
        List.iter (function ((_, "Name"), name) -> print_endline name | _ -> ()) attrs
    | _ -> ()
  done
```


using the [http://www.xs4all.nl/~mmzeeman/ocaml/ ocaml expat wrapper]:


```ocaml
open Expat

let xml_str = "
  <Students>
    <Student Name='April' Gender='F' DateOfBirth='1989-01-02' />
    <Student Name='Bob'   Gender='M' DateOfBirth='1990-03-04' />
    <Student Name='Chad'  Gender='M' DateOfBirth='1991-05-06' />
    <Student Name='Dave'  Gender='M' DateOfBirth='1992-07-08'>
      <Pet Type='dog' Name='Rover' />
    </Student>
    <Student DateOfBirth='1993-09-10' Gender='F' Name='&#x00C9;mily' />
  </Students>"

let () =
  let p = parser_create None in
  set_start_element_handler p
    (fun tag attrs ->
       if tag = "Student" then
         List.iter (function ("Name", name) -> print_endline name | _ -> ()) attrs
    );
  parse p xml_str;
  final p;
```



## OpenEdge/Progress

The following example uses the X-DOCUMENT DOM parser. For larger documents the SAX parser is recommended.


```progress

DEF VAR lcc          AS LONGCHAR.
DEF VAR hxdoc        AS HANDLE.
DEF VAR hxstudents   AS HANDLE.
DEF VAR hxstudent    AS HANDLE.
DEF VAR hxname       AS HANDLE.
DEF VAR ii           AS INTEGER EXTENT 2.
DEF VAR cstudents    AS CHARACTER.

lcc   =  '<Students>'
      +  '<Student Name="April" Gender="F" DateOfBirth="1989-01-02" />'
      +  '<Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />'
      +  '<Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />'
      +  '<Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">'
      +  '<Pet Type="dog" Name="Rover" />'
      +  '</Student>'
      +  '<Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />'
      +  '</Students>'.

CREATE X-DOCUMENT hxdoc.
hxdoc:LOAD( 'LONGCHAR', lcc, FALSE ).

DO ii[1] = 1 TO hxdoc:NUM-CHILDREN:
   CREATE X-NODEREF hxstudents.
   hxdoc:GET-CHILD( hxstudents, ii[1] ).
   IF hxstudents:NAME = 'Students' THEN DO ii[2] = 1 TO hxstudents:NUM-CHILDREN:
      CREATE X-NODEREF hxstudent.
      hxstudents:GET-CHILD( hxstudent, ii[2] ).
      IF hxstudent:NAME = 'Student' THEN 
         cstudents = cstudents + hxstudent:GET-ATTRIBUTE( 'Name' ) + '~n'.
      DELETE OBJECT hxstudent.
   END.
   DELETE OBJECT hxstudents.
END.
DELETE OBJECT hxdoc.

MESSAGE cstudents VIEW-AS ALERT-BOX.
```


'''Output:'''


```txt
---------------------------
Message
---------------------------
April
Bob
Chad
Dave
Émily
---------------------------
OK   
---------------------------

```



## Oz


```oz
declare
  [XMLParser] = {Module.link ['x-oz://system/xml/Parser.ozf']}
  Parser = {New XMLParser.parser init}

  Data =
   "<Students>"
  #"  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />"
  #"  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />"
  #"  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />"
  #"  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">"
  #"    <Pet Type=\"dog\" Name=\"Rover\" />"
  #"  </Student>"
  #"  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />"
  #"</Students>"

  fun {IsStudentElement X}
     case X of element(name:'Student' ...) then true
     else false
     end
  end

  fun {GetStudentName element(attributes:As ...)}
     [NameAttr] = {Filter As fun {$ attribute(name:N ...)} N == 'Name' end}
  in
     NameAttr.value
  end

  [StudentsDoc] = {Parser parseVS(Data $)}
  Students = {Filter StudentsDoc.children IsStudentElement}
  StudentNames = {Map Students GetStudentName}
in
  {ForAll StudentNames System.showInfo}
```



## Perl

{{libheader|XML::Simple}}

```perl
use utf8;
use XML::Simple;

my $ref = XMLin('<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>');

print join( "\n", map { $_->{'Name'} } @{$ref->{'Student'}});
```

{{out}}

```txt
April
Bob
Chad
Dave
Émily
```



## Perl 6

{{works with|Rakudo|2018.02}}

{{libheader|XML}}

```perl6
use XML;

my $xml = from-xml '<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>';

say .<Name> for $xml.nodes.grep(/Student/)
```



## Phix


```Phix
include builtins/xml.e
constant xml = """
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
"""
sequence x = xml_parse(xml)

procedure traverse(sequence x)
    if x[XML_TAGNAME]="Student" then
        ?xml_get_attribute(x,"Name")
    else
        x = x[XML_CONTENTS]
        if not string(x) then
            for i=1 to length(x) do
                traverse(x[i])
            end for
        end if
    end if
end procedure
traverse(x[XML_CONTENTS])
```

{{out}}
(note the last line (&#x00C9;mily) looks better on this page than it does on a windows console!)

```txt

"April"
"Bob"
"Chad"
"Dave"
"&#x00C9;mily"

```



## PHP


```PHP
<?php
$data = '<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>';
$xml = new XMLReader();
$xml->xml( $data );
while ( $xml->read() )
	if ( XMLREADER::ELEMENT == $xml->nodeType && $xml->localName == 'Student' )
		echo $xml->getAttribute('Name') . "\n";
?>
```



## PicoLisp


```PicoLisp
(load "@lib/xm.l")

(mapcar
   '((L) (attr L 'Name))
   (body (in "file.xml" (xml))) )
```

Output:

```txt
-> ("April" "Bob" "Chad" "Dave" "Émily")
```


## Pike


```Pike

string in = "<Students>\n"
            "  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"
            "  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"
            "  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"
            "  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"
            "    <Pet Type=\"dog\" Name=\"Rover\" />\n"
            "  </Student>\n"
            "  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"
            "</Students>\n";

object s = Parser.XML.Tree.simple_parse_input(in);

array collect = ({});
s->walk_inorder(lambda(object node)
                { 
                    if (node->get_tag_name() == "Student") 
                        collect += ({ node->get_attributes()->Name }); 
                });
write("%{%s\n%}", collect);
```


Output:
 April
 Bob
 Chad
 Dave
 Émily


## PowerShell


```Powershell

[xml]$xml = @'
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
'@

foreach ($node in $xml.DocumentElement.ChildNodes) {$node.Name}




```



## OpenEdge ABL/Progress 4GL



```OpenEdgeABL

/** 
### = Definitions ==
 **/
DEFINE VARIABLE chXMLString AS LONGCHAR NO-UNDO.
DEFINE TEMP-TABLE ttStudent NO-UNDO XML-NODE-NAME 'Student'
    FIELD StudentName AS CHARACTER XML-NODE-TYPE 'attribute' XML-NODE-NAME 'Name'        LABEL 'Name'
    FIELD Gender      AS CHARACTER XML-NODE-TYPE 'attribute' XML-NODE-NAME 'Gender'      LABEL 'Gender'
    FIELD DateOfBirth AS CHARACTER XML-NODE-TYPE 'attribute' XML-NODE-NAME 'DateOfBirth' LABEL 'Date Of Birth'.
DEFINE DATASET dsStudents XML-NODE-NAME 'Students' FOR ttStudent.

/** 
### = Main block =
**/

/** ASSIGN the XML string with the XML data.. **/ 
chXMLString = '<Students>~
                <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />~
                <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />~
                <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />~
                <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">~
                    <Pet Type="dog" Name="Rover" />~
                </Student>~
                <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />~
               </Students>'.

/** Read the string into the dataset...**/
DATASET dsStudents:READ-XML('LONGCHAR', chXMLString, 'EMPTY',?,?).

/** Loop thought each temp-table record to produce the results...**/
FOR EACH ttStudent:
    DISPLAY ttStudent.StudentName.
END.


```



## PureBasic

Uses a PureBasic XML library (which is linked automatically) that is based on the library [http://expat.sourceforge.net/ expat XML parser] licensed under the MIT license.

```PureBasic
Define studentNames.String, src$

src$ = "<Students>"
src$ + "<Student Name='April' Gender='F' DateOfBirth='1989-01-02' />"
src$ + "<Student Name='Bob' Gender='M'  DateOfBirth='1990-03-04' />"
src$ + "<Student Name='Chad' Gender='M'  DateOfBirth='1991-05-06' />"
src$ + "<Student Name='Dave' Gender='M'  DateOfBirth='1992-07-08'>"
src$ + "<Pet Type='dog' Name='Rover' />"
src$ + "</Student>"
src$ + "<Student DateOfBirth='1993-09-10' Gender='F' Name='&#x00C9;mily' />"
src$ + "</Students>"

;This procedure is generalized to match any attribute of any normal element's node name
;i.e. get_values(MainXMLNode(0),"Pet","Type",@petName.String) and displaying petName\s
;would display "dog".
Procedure get_values(*cur_node, nodeName$, attribute$, *valueResults.String)
  ;If nodeName$ and attribute$ are matched then the value
  ;will be added to the string structure pointed to by *valueResults .
  Protected result$

  While *cur_node
    If XMLNodeType(*cur_node) = #PB_XML_Normal
      
      result$ = GetXMLNodeName(*cur_node)
      If result$ = nodeName$
        If ExamineXMLAttributes(*cur_node)
          While NextXMLAttribute(*cur_node)
            If XMLAttributeName(*cur_node) = attribute$
              If *valueResults <> #Null
                *valueResults\s + XMLAttributeValue(*cur_node) + Chr(13) ;value + carriage-return
              EndIf 
            EndIf
          Wend
        EndIf
      EndIf 
      
    EndIf 
    
    get_values(ChildXMLNode(*cur_node), nodeName$, attribute$, *valueResults)
    *cur_node = NextXMLNode(*cur_node)
  Wend 
EndProcedure

CatchXML(0,@src$,Len(src$))

If IsXML(0)
  get_values(MainXMLNode(0), "Student", "Name",@studentNames)
  MessageRequester("Student Names", studentNames\s)
  FreeXML(0)
EndIf 
```

Sample output:

```txt

April
Bob
Chad
Dave
Émily

```



## Python


```python
import xml.dom.minidom

doc = """<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>"""

doc = xml.dom.minidom.parseString(doc)

for i in doc.getElementsByTagName("Student"):
    print i.getAttribute("Name")
```



## R

{{libheader|XML}}

```R
library(XML)
#Read in XML string
str <- readLines(tc <- textConnection('<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>'))
close(tc)
str
```

 [1] "<Students>"                                                                 
 [2] "  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />"       
 [3] "  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />"        
 [4] "  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />"       
 [5] "  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">"         
 [6] "    <Pet Type=\"dog\" Name=\"Rover\" />"                                    
 [7] "  </Student>"                                                               
 [8] "  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />"
 [9] "</Students>"

```R
#Convert to an XML tree
xmltree <- xmlTreeParse(str)

#Retrieve the students, and how many there are
students <- xmltree$doc$children$Students
nstudents <- length(students)

#Get each of their names
studentsnames <- character(nstudents)
for(i in 1:nstudents)
{
   this.student <- students$children[i]$Student
   studentsnames[i] <- this.student$attributes["Name"]   
}

#Change the encoding so that Emily displays correctly
Encoding(studentsnames) <- "UTF-8"
studentsnames
```

 [1] "April" "Bob"   "Chad"  "Dave"  "Émily"i


## Racket



```racket

#lang racket
(require xml xml/path)

(define input
#<<END
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
END
  )

(define students
  (xml->xexpr 
   (document-element
    (read-xml (open-input-string input)))))
 
(se-path*/list '(Student #:Name) students)

```



## Rascal


```rascal
import lang::xml::DOM;

public void getNames(loc a){
	D = parseXMLDOM(readFile(a));
	visit(D){
		case element(_,"Student",[_*,attribute(_,"Name", x),_*]): println(x);
	};
}
```

Output:

```rascal>rascal
getNames(|file:///Users/.../Desktop/xmlinput.xml|)
April
Bob
Chad
Dave
Émily
ok
```



## REBOL


```REBOL
REBOL [
	Title: "XML Reading"
	URL: http://rosettacode.org/wiki/XML_Reading
]

xml: {
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
}

; REBOL has a simple built-in XML parser. It's not terribly fancy, but
; it's easy to use. It converts the XML into a nested list of blocks
; which can be accessed using standard REBOL path operators. The only
; annoying part (in this case) is that it does try to preserve
; whitespace, so some of the parsed elements are just things like line
; endings and whatnot, which I need to ignore.

; Once I have drilled down to the individual student records, I can
; just use the standard REBOL 'select' to locate the requested
; property.

data: parse-xml xml
students: data/3/1/3 ; Drill down to student records.
foreach student students [
	if block! = type? student [ ; Ignore whitespace elements.
		print select student/2 "Name"
	]
]
```


Output:


```txt
April
Bob
Chad
Dave
&#x00C9;mily
```



## REXX


### version 1


```rexx
/*REXX program  extracts  student names  from an  XML  string(s).                       */
g.=
g.1 = '<Students>                                                             '
g.2 = '  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />         '
g.3 = '  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />          '
g.4 = '  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />         '
g.5 = '  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">           '
g.6 = '    <Pet Type="dog" Name="Rover" />                                    '
g.7 = '  </Student>                                                           '
g.8 = '  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />  '
g.9 = '</Students>                                                            '

  do j=1  while g.j\==''
  g.j=space(g.j)
  parse var   g.j   'Name="'   studname   '"'
  if studname\==''  then say studname
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default (internal) input:}}

```txt

April
Bob
Chad
Dave
Rover
&#x00C9;mily

```



### version 2

This REXX version handles more HTML tags for output.

```rexx
/*REXX program  extracts  student names  from an  XML  string(s).                       */
g.=
g.1 = '<Students>                                                             '
g.2 = '  <Student Name="April" Gender="F"  DateOfBirth="1989-01-02" />        '
g.3 = '  <Student Name="Bob" Gender="M"   DateOfBirth="1990-03-04" />         '
g.4 = '  <Student Name="Chad" Gender="M"   DateOfBirth="1991-05-06" />        '
g.5 = '  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">           '
g.6 = '    <Pet Type="dog" Name="Rover" / >                                   '
g.7 = '  </Student>                                                           '
g.8 = '  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00c9;mily" />  '
g.9 = '</Students>                                                            '

        do j=1  while g.j\==''
        g.j=space(g.j)
        parse  var   g.j   'Name="'   studname   '"'
        if studname==''  then iterate
        if pos('&', studname)\==0  then studname=xmlTranE(studname)
        say studname
        end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
xml_:   parse arg ,_                             /*transkate an  XML  entity   (&xxxx;) */
        xmlEntity! = '&'_";"
        if pos(xmlEntity!, $)\==0  then $=changestr(xmlEntity!, $, arg(1) )
        if left(_, 2)=='#x'  then do
                                  xmlEntity!='&'left(_, 3)translate( substr(_, 4) )";"
                                  $=changestr(xmlEntity!, $, arg(1) )
                                  end
        return $
/*──────────────────────────────────────────────────────────────────────────────────────*/
xmlTranE: procedure; parse arg $                 /*Following are most of the chars in   */
                                                 /*the  DOS  (under Windows)  codepage. */
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
return $
```

Some older REXXes don't have a   '''changestr'''   BIF,   so one is included here   ──►   [[CHANGESTR.REX]].

{{out|output|text=  when using the default (internal) input:}}

```txt

April
Bob
Chad
Dave
Rover
Émily

```



## Ruby

{{libheader|REXML}}

```ruby
require 'rexml/document'
include REXML

doc = Document.new(File.new("sample.xml"))
# or
# doc = Document.new(xml_string)

# without using xpath
doc.each_recursive do |node|
  puts node.attributes["Name"] if node.name == "Student"
end

# using xpath
doc.each_element("*/Student") {|node| puts node.attributes["Name"]}
```



## Run BASIC


```runbasic
' ------------------------------------------------------------------------
'XMLPARSER methods

'#handle ELEMENTCOUNT() - Return the number of child XML elements
'#handle KEY$() 	- Return the key as a string from an XML expression like <key>value</key>
'#handle VALUE$() 	- Return the value as a string from an XML expression like <key>value</key>
'#handle VALUEFORKEY$(keyExpr$) - Return the value for the specified tag key in keyExpr$
'#handle #ELEMENT(n) 	- Return the nth child-element XML element
'#handle #ELEMENT(nameExpr$) - Return the child-element XML element named by nameExpr$
'#handle ATTRIBCOUNT() 	- Return a count of attribute pairs; <a attrA="abc" attrB="def"> has two pairs
'#handle ATTRIBKEY$(n) 	- Return the key string of the nth attribute
'#handle ATTRIBVALUE$(n) - Return the value string of the nth attribute
'#handle ATTRIBVALUE$(n$) - Return the value string of the attribute with the key n$, or an empty string if it doesn't exist.
'#handle ISNULL() 	- Returns zero (or false)
'#handle DEBUG$() 	- Returns the string "Xmlparser"
' ------------------------------------------------------------------------

' The xml string
xml$ = "
<Students>
  <Student Name=""April"" Gender=""F"" DateOfBirth=""1989-01-02"" />
  <Student Name=""Bob"" Gender=""M""  DateOfBirth=""1990-03-04"" />
  <Student Name=""Chad"" Gender=""M""  DateOfBirth=""1991-05-06"" />
  <Student Name=""Dave"" Gender=""M""  DateOfBirth=""1992-07-08"">
    <Pet Type=""dog"" Name=""Rover"" />
  </Student>
  <Student DateOfBirth=""1993-09-10"" Gender=""F"" Name=""&#x00C9;mily"" />
</Students>"

 
' Creates the xml handler, using the string
xmlparser #spies, xml$
 
' Uses elementCount() to know how many elements are in betweeb <spies>...</spies>
for count = 1 to #spies elementCount()
 
  ' Uses "count" to work through the elements, and assigns the element to the
  ' handle "#spy"
  #spy = #spies #element(count)
 
  ' Prints the value, or inner text, of "#spy": Sam, Clover, & Alex
  print count;" ";#spy value$();" ->";#spy ATTRIBVALUE$(1)
 
next count
```



## Scala


Scala has native XML support, with query constructs similar to XPath and XQuery.


```scala
val students =
  <Students>
    <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
    <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
    <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
    <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
      <Pet Type="dog" Name="Rover" />
    </Student>
    <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
  </Students>

students \ "Student" \\ "@Name" foreach println
```



## Sidef

{{trans|Perl}}

```ruby
require('XML::Simple');

var ref = %S'XML::Simple'.XMLin('<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>');

ref{:Student}.each { say _{:Name} };
```

{{out}}

```txt

April
Bob
Chad
Dave
Émily

```



## Slate

{{lines too long|Slate}}
Slate's XML Reader is still being developed at the time of this writing.
 

```slate
slate[1]> [ |tree|

  tree: (Xml SimpleParser newOn: '<Students>
    <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
    <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
    <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
    <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
      <Pet Type="dog" Name="Rover" />
    </Student>
    <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
  </Students>') parse.
  tree name = 'Students' ifTrue: [(tree children select: #is: `er <- Xml Element)
                                         do: [|:child| child name = 'Student' ifTrue: [inform: (child attributes at: 'Name' ifAbsent: ['Noname'])]]].

] do.
April
Bob
Chad
Dave
&#x00C9;mily
Nil
```



## Tcl

Using {{libheader|tDOM}}

```tcl
package require tdom
set tree [dom parse $xml]
set studentNodes [$tree getElementsByTagName Student] ;# or: set studentNodes [[$tree documentElement] childNodes]

foreach node $studentNodes {
    puts [$node getAttribute Name]
}
```

Using {{libheader|TclXML}}

```tcl
package require xml
set parser [xml::parser -elementstartcommand elem]
proc elem {name attlist args} {
    if {$name eq "Student"} {
        puts [dict get $attlist Name]
    }
}
$parser parse $xml
```


Using just pure-Tcl (originally on http://wiki.tcl.tk/3919):

```Tcl
proc xml2list xml {
    regsub -all {>\s*<} [string trim $xml " \n\t<>"] "\} \{" xml
    set xml [string map {> "\} \{#text \{" < "\}\} \{"}  $xml]
    set res ""   ;# string to collect the result
    set stack {} ;# track open tags
    set rest {}
    foreach item "{$xml}" {
        switch -regexp -- $item {
	    ^# {append res "{[lrange $item 0 end]} " ; #text item}
	    ^/ {
		regexp {/(.+)} $item -> tagname ;# end tag
		set expected [lindex $stack end]
		set stack [lrange $stack 0 end-1]
		append res "\}\} "
            }
	    /$ { # singleton - start and end in one <> group
                regexp {([^ ]+)( (.+))?/$} $item -> tagname - rest
                set rest [lrange [string map {= " "} $rest] 0 end]
                append res "{$tagname [list $rest] {}} "
	    }
	    default {
                set tagname [lindex $item 0] ;# start tag
                set rest [lrange [string map {= " "} $item] 1 end]
                lappend stack $tagname
                append res "\{$tagname [list $rest] \{"
	    }
        }
    }
    string map {"\} \}" "\}\}"} [lindex $res 0]   ;#"
}
proc deent str {
    regsub -all {&\#x(.+?);} $str {\\u\1} str
    subst -nocommands -novar $str
}
#----------------------- Testing the whole thing:
set xml {<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" /></Students>
}
foreach i [lindex  [xml2list $xml] 2] {
    if {[lindex $i 0] eq "Student"} {
        foreach {att val} [lindex $i 1] {
            if {$att eq "Name"} {puts [deent $val]}
        }
    }
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
MODE DATA
$$ SET xmldata =*
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="Emily" />
</Students>
$$ MODE TUSCRIPT
COMPILE
LOOP x = xmldata
SET name=GET_TAG_NAME (x)
IF (name!="student") CYCLE
 studentname=GET_ATTRIBUTE (x,"Name")
 IF (studentname!="") PRINT studentname
ENDLOOP
ENDCOMPILE

```

Output:

```txt

April
Bob
Chad
Dave
Emily

```



## TXR

This program shows how most of the information in the XML can be extracted
with very little code, which doesn't actually understand XML.
The name Émily is properly converted from the HTML/XML escape syntax.


```txr><Students

@(collect :vars (NAME GENDER YEAR MONTH DAY (PET_TYPE "none") (PET_NAME "")))
@  (cases)
  <Student Name="@NAME" Gender="@GENDER" DateOfBirth="@YEAR-@MONTH-@DAY"@(skip)
@  (or)
  <Student DateOfBirth="@YEAR-@MONTH-@DAY" Gender="@GENDER" Name="@NAME"@(skip)
@  (end)
@  (maybe)
    <Pet Type="@PET_TYPE" Name="@PET_NAME" />
@  (end)
@(until)
</Students>
@(end)
@(output :filter :from_html)
NAME         G DOB        PET
@  (repeat)
@{NAME 12} @GENDER @YEAR-@MONTH-@DAY @PET_TYPE @PET_NAME
@  (end)
@(end)
```


Sample run:

```txt
$ txr students.txr students.xml
NAME         G DOB        PET
April        F 1989-01-02 none 
Bob          M 1990-03-04 none 
Chad         M 1991-05-06 none 
Dave         M 1992-07-08 dog Rover
Émily        F 1993-09-10 none
```


To obtain the output specified in this task, we can simply reduce the @(output) block to this:


```txr
@(output :filter :from_html)
@NAME
@(end)
```



```txt

April
Bob
Chad
Dave
Émily
```



## Vedit macro language

This implementation finds all ''Student'' tags and then displays the contents of their ''Name'' parameter.

```vedit
Repeat(ALL) {
    Search("<Student|X", ERRBREAK)
    #1 = Cur_Pos
    Match_Paren()
    if (Search_Block(/Name=|{",'}/, #1, Cur_Pos, BEGIN+ADVANCE+NOERR+NORESTORE)==0) { Continue }
    #2 = Cur_Pos
    Search(/|{",'}/)
    Type_Block(#2, Cur_Pos)
    Type_Newline
}
```


Output:
 April
 Bob
 Chad
 Dave
 &#x00C9;mily



## VBA


```vb
Option Explicit

Const strXml As String = "" & _
"<Students>" & _
    "<Student Name=""April"" Gender=""F"" DateOfBirth=""1989-01-02"" />" & _
    "<Student Name=""Bob"" Gender=""M""  DateOfBirth=""1990-03-04"" />" & _
    "<Student Name=""Chad"" Gender=""M""  DateOfBirth=""1991-05-06"" />" & _
    "<Student Name=""Dave"" Gender=""M""  DateOfBirth=""1992-07-08"">" & _
        "<Pet Type=""dog"" Name=""Rover"" />" & _
    "</Student>" & _
    "<Student DateOfBirth=""1993-09-10"" Gender=""F"" Name=""&#x00C9;mily"" />" & _
"</Students>"

Sub Main_Xml()
Dim MyXml As Object
Dim myNodes, myNode

    With CreateObject("MSXML2.DOMDocument")
        .LoadXML strXml
        Set myNodes = .getElementsByTagName("Student")
    End With
    If Not myNodes Is Nothing Then
        For Each myNode In myNodes
            Debug.Print myNode.getAttribute("Name")
        Next
    End If
    Set myNodes = Nothing
End Sub
```

{{out}}

```txt
April
Bob
Chad
Dave
Émily
```



## Visual Basic .NET


```vbnet>Dim xml = <Students

              <Student Name="April"/>
              <Student Name="Bob"/>
              <Student Name="Chad"/>
              <Student Name="Dave"/>
              <Student Name="Emily"/>
           </Students>
 
Dim names = (From node In xml...<Student> Select node.@Name).ToArray
 
For Each name In names
     Console.WriteLine(name)
Next
```



## XPL0


```XPL0
code ChOut=8, CrLf=9;   \intrinsic routines
string 0;               \use zero-terminated strings

func StrLen(A);         \Return number of characters in an ASCIIZ string
char A;
int  I;
for I:= 0 to -1>>1-1 do
    if A(I) = 0 then return I;

func StrFind(A, B);     \Search for ASCIIZ string A in string B
\Returns address of first occurrence of string A in B, or zero if A is not found
char A, B;              \strings to be compared
int  LA, LB, I, J;
[LA:= StrLen(A);
LB:= StrLen(B);
for I:= 0 to LB-LA do
    [for J:= 0 to LA-1 do
        if A(J) # B(J+I) then J:= LA+1;
    if J = LA then return B+I;  \found
    ];
return 0;
];

char XML, P;
[XML:= "<Students>
        <Student Name=^"April^" Gender=^"F^" DateOfBirth=^"1989-01-02^" />
        <Student Name=^"Bob^" Gender=^"M^" DateOfBirth=^"1990-03-04^" />
        <Student Name=^"Chad^" Gender=^"M^" DateOfBirth=^"1991-05-06^" />
        <Student Name=^"Dave^" Gender=^"M^" DateOfBirth=^"1992-07-08^">
        <Pet Type=^"dog^" Name=^"Rover^" />
        </Student>
        <Student DateOfBirth=^"1993-09-10^" Gender=^"F^" Name=^"&#x00C9;mily^" />
        </Students>";
P:= XML;
loop    [P:= StrFind("<Student ", P);
        if P=0 then quit;
        P:= StrFind("Name=", P);
        if P=0 then quit;
        P:= P + StrLen("Name=x");
        repeat  ChOut(0, P(0));
                P:= P+1;
        until   P(0) = ^";
        CrLf(0);
        ];
]
```


{{out}}

```txt

April
Bob
Chad
Dave
&#x00C9;mily

```



## Yabasic


```Yabasic
// 
### ======= routine for set code conversion =============


data 32, 173, 189, 156, 207, 190, 221, 245, 249, 184, 166, 174, 170, 32, 169, 238
data 248, 241, 253, 252, 239, 230, 244, 250, 247, 251, 167, 175, 172, 171, 243, 168
data 183, 181, 182, 199, 142, 143, 146, 128, 212, 144, 210, 211, 222, 214, 215, 216
data 209, 165, 227, 224, 226, 229, 153, 158, 157, 235, 233, 234, 154, 237, 232, 225
data 133, 160, 131, 198, 132, 134, 145, 135, 138, 130, 136, 137, 141, 161, 140, 139
data 208, 164, 149, 162, 147, 228, 148, 246, 155, 151, 163, 150, 129, 236, 231, 152

initCode = 160 : TOASCII = 0 : TOUNICODE = 1 : numCodes = 255 - initCode + 1

dim codes(numCodes)

for i = 0 to numCodes - 1 : read codes(i) : next

sub codeConversion(charcode, tocode)
    local i

    if tocode then
        for i = 0 to numCodes - 1
            if codes(i) = charcode return i + initCode
        next
    else
        return codes(charcode - initCode)
    end if
end sub

// 
### ======= end routine for set code conversion =========


xml$ =  "<Students>\n"
xml$ = xml$ + "  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />\n"
xml$ = xml$ + "  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />\n"
xml$ = xml$ + "  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />\n"
xml$ = xml$ + "  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">\n"
xml$ = xml$ + "    <Pet Type=\"dog\" Name=\"Rover\" />\n"
xml$ = xml$ + "  </Student>\n"
xml$ = xml$ + "  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />\n"
xml$ = xml$ + "</Students>\n"

tag1$ = "<Student"
tag2$ = "Name=\""
ltag = len(tag2$)

sub convASCII$(name$, mark$)
    local p, c, lm
    
    lm = len(mark$)
    
    do
        p = instr(name$, mark$, p)
        if not p break
        c = dec(mid$(name$, p + lm, 4))
        c = codeConversion(c)
        name$ = left$(name$, p-1) + chr$(c) + right$(name$, len(name$) - (p + lm + 4))
        p = p + 1
    loop
    return name$
end sub

do
    p = instr(xml$, tag1$, p)
    if not p break
    p = instr(xml$, tag2$, p)
    p = p + ltag
    p2 = instr(xml$, "\"", p)
    name$ = convASCII$(mid$(xml$, p, p2 - p), "&#x")
    print name$
loop
```



## zkl

Uses regular expressions and the data is in a file identical to task description.<br/>
Assumes: a name attribute is complete in a line and only one name per line.

```zkl
student:=RegExp(0'|.*<Student\s*.+Name\s*=\s*"([^"]+)"|);
unicode:=RegExp(0'|.*(&#x[0-9a-fA-F]+;)|);
xml:=File("foo.xml").read();

students:=xml.pump(List,'wrap(line){
   if(student.search(line)){
      s:=student.matched[1];    // ( (match start,len),group text )
      while(unicode.search(s)){ // convert "&#x00C9;" to 0xc9 to UTF-8
	 c:=unicode.matched[1];
	 uc:=c[3,-1].toInt(16).toString(-8);
      	 s=s.replace(c,uc);
      }
      s
   }
   else Void.Skip; // line doesn't contain <Student ... Name ...
});

students.println();
```

{{out}}
```txt
L("April","Bob","Chad","Dave","Émily")
```


{{omit from|GUISS}}
{{omit from|PARI/GP|No real capacity for string manipulation}}
