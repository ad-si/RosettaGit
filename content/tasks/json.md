+++
title = "JSON"
description = ""
date = 2019-10-03T13:11:31Z
aliases = []
[extra]
id = 2305
[taxonomies]
categories = ["task"]
tags = []
+++

Load a [[wp:JSON|JSON]] string into a data structure.
Also, create a new data structure and serialize it into JSON.

Use objects and arrays (as appropriate for your language)
and make sure your JSON is valid (https://jsonformatter.org).



## 8th

8th uses JSON as its data description language, so:

```txt

[1,2,3] . cr

```

Prints an array of [1,2,3].

Converting a string to an object:

```txt

"{ \"a\": 123 }"  json>

```

Takes the string and converts to the JSON object (which is an 8th object).  Convert back to a string:

```txt

{"a": 123} >s

```

That takes the object and converts to the JSON string given above.

## Ada


###  Alternative using GNATCOLL


```ada

with Ada.Text_IO;
with GNATCOLL.JSON;

procedure JSON_Test is
   use Ada.Text_IO;
   use GNATCOLL.JSON;

   JSON_String : constant String := "{""name"":""Pingu"",""born"":1986}";

   Penguin : JSON_Value := Create_Object;
   Parents : JSON_Array;
begin
   Penguin.Set_Field (Field_Name => "name",
                      Field      => "Linux");

   Penguin.Set_Field (Field_Name => "born",
                      Field      => 1992);

   Append (Parents, Create ("Linus Torvalds"));
   Append (Parents, Create ("Alan Cox"));
   Append (Parents, Create ("Greg Kroah-Hartman"));

   Penguin.Set_Field (Field_Name => "parents",
                      Field      => Parents);

   Put_Line (Penguin.Write);

   Penguin := Read (JSON_String, "json.errors");

   Penguin.Set_Field (Field_Name => "born",
                      Field      => 1986);

   Parents := Empty_Array;
   Append (Parents, Create ("Otmar Gutmann"));
   Append (Parents, Create ("Silvio Mazzola"));

   Penguin.Set_Field (Field_Name => "parents",
                      Field      => Parents);

   Put_Line (Penguin.Write);
end JSON_Test;

```


```txt

{"parents":["Linus Torvalds", "Alan Cox", "Greg Kroah-Hartman"], "name":"Linux", "born":1992}
{"parents":["Otmar Gutmann", "Silvio Mazzola"], "name":"Pingu", "born":1986}

```



###  Alternative using Matreshka



```ada

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with League.JSON.Arrays;    use League.JSON.Arrays;
with League.JSON.Documents; use League.JSON.Documents;
with League.JSON.Objects;   use League.JSON.Objects;
with League.JSON.Values;    use League.JSON.Values;
with League.Strings;        use League.Strings;

procedure Main is

   function "+" (Item : Wide_Wide_String) return Universal_String
     renames To_Universal_String;

   JSON_String : constant Universal_String
     := +"{""name"":""Pingu"",""born"":1986}";

   Penguin : JSON_Object;
   Parents : JSON_Array;

begin
   Penguin.Insert (+"name", To_JSON_Value (+"Linux"));
   Penguin.Insert (+"born", To_JSON_Value (1992));

   Parents.Append (To_JSON_Value (+"Linus Torvalds"));
   Parents.Append (To_JSON_Value (+"Alan Cox"));
   Parents.Append (To_JSON_Value (+"Greg Kroah-Hartman"));

   Penguin.Insert (+"parents", To_JSON_Value (Parents));

   Put_Line (To_JSON_Document (Penguin).To_JSON.To_Wide_Wide_String);

   Penguin := From_JSON (JSON_String).To_Object;

   Parents := Empty_JSON_Array;

   Parents.Append (To_JSON_Value (+"Otmar Gutmann"));
   Parents.Append (To_JSON_Value (+"Silvio Mazzola"));

   Penguin.Insert (+"parents", To_JSON_Value (Parents));

   Put_Line (To_JSON_Document (Penguin).To_JSON.To_Wide_Wide_String);
end Main;

```


```txt

{"parents":["Linus Torvalds","Alan Cox","Greg Kroah-Hartman"],"name":"Linux","born":1992}
{"parents":["Otmar Gutmann","Silvio Mazzola"],"name":"Pingu","born":1986}

```



## AntLang

JSON parser (maybe failes with "invalid JSON" error)

```AntLang

json:{[data]catch[eval[,|{[y]catch[{":" = "="; "[" = "<"; "]" = ">"; "," = ";"}[y];{x};{[]y}]}'("""("(\\.|[^\\"])*"|\-?[0-9]+(\.[0-9]+)?|\{|\}|\[|\]|\:|\,)"""~data)["strings"]];{x};{error["Invalid JSON"]}]}

```



## ANTLR

[[File:ANTLRObject.png|left|ANTLR]]
[[File:ANTLRPair.png|left|ANTLR]]
[[File:ANTLRString.png|left|ANTLR]]
[[File:ANTLRValue.png|left|ANTLR]]
[[File:ANTLRNumber.png|left|ANTLR]]
[[File:ANTLRKeyword.png|left|ANTLR]]
[[File:ANTLRArray.png|left|ANTLR]]
<br clear=both>


### Java


```java

//  Parse JSON
//
//  Nigel Galloway - April 27th., 2012
//
grammar JSON ;
@members {
String Indent = "";
}
Number	:	(('0')|('-'? ('1'..'9') ('0'..'9')*)) ('.' ('0'..'9')+)? (('e'|'E') ('+'|'-')? ('0'..'9')+)?;
WS	:	(' ' | '\t' | '\r' |'\n') {skip();};
Tz	:	' ' .. '!' | '#' .. '[' | ']' .. '~';
Control	:	'\\' ('"'|'\\'|'/'|'b'|'f'|'n'|'r'|'t'|UCode);
UCode	:	'u' ('0'..'9'|'a'..'f'|'A'..'F') ('0'..'9'|'a'..'f'|'A'..'F') ('0'..'9'|'a'..'f'|'A'..'F') ('0'..'9'|'a'..'f'|'A'..'F');
Keyword	:	'true' | 'false' | 'null';
String	:	'"' (Control? Tz)* '"';
object	:       '{' {System.out.println(Indent + "{Object}"); Indent += "    ";} (pair (',' pair*)*)? '}' {Indent = Indent.substring(4);};
pair	:	e = String {System.out.println(Indent + "{Property}\t" + $e.text);} ':' value;
value	:	Number             {System.out.println(Indent + "{Number}  \t" + $Number.text);}
	|	object
	|	String             {System.out.println(Indent + "{String}  \t" + $String.text);}
	|	Keyword            {System.out.println(Indent + "{Keyword} \t" + $Keyword.text);}
	|	array;
array	:	'[' {System.out.println(Indent + "Array"); Indent += "    ";} (value (',' value)*)? ']' {Indent = Indent.substring(4);};

```

Produces:

```txt

>java Test
{
  "Nigel"
  :
  -110.2e-13
  ,
  "Fred"
  :
  {
    "Joe"
    :
    [3,true,"Nigel"]
  }
  "Harry"
  :
  [23,"Hello"]
}
^Z
{Object}
    {Property}  "Nigel"
    {Number}    -110.2e-13
    {Property}  "Fred"
    {Object}
        {Property}      "Joe"
        Array
            {Number}    3
            {Keyword}   true
            {String}    "Nigel"
    {Property}  "Harry"
    Array
        {Number}        23
        {String}        "Hello"

```



## Apex

JSON serialization and deserialization is built in

```apex
class TestClass{
    String foo {get;set;}
    Integer bar {get;set;}
}

TestClass testObj = new TestClass();
testObj.foo = 'ABC';
testObj.bar = 123;

String serializedString = JSON.serialize(testObj);
TestClass deserializedObject = (TestClass)JSON.deserialize(serializedString, TestClass.class);

//"testObj.foo == deserializedObject.foo" is true
//"testObj.bar == deserializedObject.bar" is true

```



## Bracmat


Bracmat has built-in functionality for reading and writing JSON data.
A full roundtrip from JSON file over a Bracmat internal representation back to a JSON file looks like this:


```bracmat
put$(jsn$(get$("input.json",JSN)),"output.JSN,NEW)
```


Let us split this into separate steps.

To read a JSON file "myfile.json", use


```bracmat
get$("myfile.json",JSN)
```


If the JSON data, e.g, an array, has to be read from a string value, use the <code>MEM</code> option on the <code>get</code> function, like this:


```bracmat
get$("[1,2,3]",JSN,MEM)
```


To convert the corresponding Bracmat data structure <code>(,1 2 3)</code> back to a JSON string, use


```bracmat
jsn$(,1 2 3)
```


To write a JSON string <code>"[1,2,3]"</code> to a file "array.json", use


```bracmat
put$("[1,2,3]","array.json",NEW)
```


Bracmat and JSON/Javascript do far from represent data in similar ways.
Bracmat has arbitrary-precision arithmetic. Floating point numbers are not a native datatype in Bracmat.
Bracmat has no Boolean values <code>true</code> and <code>false</code> and no <code>null</code> value.
Bracmat has arrays and objects, but they are second class citizens. Most data is best represented as binary tree structures, with binary operators like the plus, comma, dot or white space sitting in the nodes and the atomic parts of the data sitting in the leaves.
The only data type that is somewhat the same in JSON/JavaScript and Bracmat is the string, but there is a slight difference in the representation of strings in code. Whereas strings in JSON always are enclosed in quotation marks, this is only necessary in Bracmat if a string contains operator characters or starts with flag characters.

Here are the mapping rules:

{| class="wikitable"
|-
! Bracmat representation
! JSON representation
! Comment
|-
| <code> null             </code>
| <code> null             </code>
|
|-
| <code> true             </code>
| <code> true             </code>
|-
| <code> false            </code>
| <code> false            </code>
|-
| <code> 12345            </code>
| <code> 12345            </code>
|-
| <code> 2/3              </code>
| <code> 6.66666666666666666667E-1</code>
| Most rational numbers cannot be represented as floating point numbers, see note (1)
|-
| <code> (.string)        </code>
| <code> "string"         </code>
|-
| <code> (,1 2)           </code>
| <code> [1,2]            </code>
|-
| <code> ((a.1)+(b.2),)   </code>
| <code> {"a":1,"b":2}    </code>
| The <code>+</code> operator sorts its arguments. See note (2)
|}

Note (1) All floating point numbers can be represented as rational numbers. Therefore, a round trip of a Bracmat number to a JSON number and back to a Bracmat number can give a different number: <code>2/3</code> becomes <code>666666666666666666667/1000000000000000000000</code>.

Note (2) The objects <code>{"a":1,"b":2}</code> and <code>{"b":2,"a":1}</code> are equivalent in Bracmat, as they are both internally represented as <code>((a.1)+(b.2),)</code>

Here is a full round trip of the following JSON data, which is assumed to be stored in a file "rosetta.json".
The employed code is:

```bracmat

( get$("rosetta.json",JSN):?json
& lst$(json,"json.bra",NEW)
& put$(jsn$!json,"rosetta-roundtrip.json",NEW)
)
```


rosetta.json:

```txt
[
    {
        "ape": "\"King Kong\"",
        "C:\\projects": "23",
        "OS\/2": {
            "White\b\f\n\r\tspace": {}
        },
        "Cyrillic": [
            "Ya \u042F",
            "ya \u044f"
        ]
    },
    "TAB	",
    [
        "elem1",
        "elem2"
    ],
    "Bernhard",
    [],
    [
        "x",
        "y",
        "z"
    ],
    [
        "true",
        true,
        false,
        null
    ],
    {
        "fixed point": [
            3.4,
            0.00987654321,
            -10.01,
            56.78,
            56.780
        ]
    },
    {
        "floating point": [
            0e0,
            0.0000006e-007,
            17E123,
            -17.87E123,
            0.87E123,
            286e400
        ]
    },
    {
        "integer": [
            -0,
            0,
            -5,
            1234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567890
        ]
    }
]
```


Content of json.bra:


```txt
json=

,   (   ("C:\\projects"..23)
      + (Cyrillic.,(."Ya Я") (."ya я"))
      + (OS/2.("White\b\f
\r	space".0,),)
      + (ape.."\"King Kong\"")
    ,
    )
    (.TAB\t)
    (,(.elem1) (.elem2))
    (.Bernhard)
    (,)
    (,(.x) (.y) (.z))
    (,(.true) true false null)
    ( ("fixed point".,34/10 987654321/100000000000 -1001/100 5678/100 56780/1000)
    ,
    )
    ( ( "floating point"
      .
        ,   0
            6/100000000000000
            17000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
            -17870000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
            870000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
            2860000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 ...
      )
    ,
    )
    ( ( integer
      .
        ,   -0
            0
            -5
            1234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891 ...
      )
    ,
    );
```


The file "rosetta-roundtrip.json" will contain a single line. There is no beautify option when constructing a JSON string using the <code>jsn</code> function. The http://jsonlint.com service can be used to view the JSON string, but notice that some of the numbers are to big to be handled by this service and are turned into null values.

Content of rosetta-roundtrip.json (1402 characters):


```txt
[{"C:\\projects":"23","Cyrillic":["Ya Я","ya я"],"OS/2":{"White\b\f\n\r\tspace":{}},"ape":"\"King Kong\""},"TAB\t",["elem1","elem2"],"Bernhard",[],["x","y","z"],["true",true,false,null],{"fixed point":[3.4,0.00987654321,-10.01,56.78,56.78]},{"floating point":[0,0.00000000000006,17000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,-17870000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,870000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,2860000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000]},{"integer":[-0,0,-5,1234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567890]}]
```


After manual reformatting, again shortened where lines run off the screen:


```txt
[
    {
        "C:\\projects": "23",
        "Cyrillic": [
            "Ya Я",
            "ya я"
        ],
        "OS/2": {
            "White\b\f\n\r\tspace": {}
        },
        "ape": "\"King Kong\""
    },
    "TAB\t",
    [
        "elem1",
        "elem2"
    ],
    "Bernhard",
    [],
    [
        "x",
        "y",
        "z"
    ],
    [
        "true",
        true,
        false,
        null
    ],
    {
        "fixed point": [
            3.4,
            0.00987654321,
            -10.01,
            56.78,
            56.78
        ]
    },
    {
        "floating point": [
            0,
            0.00000000000006,
            17000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,
            -17870000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,
            870000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,
            2860000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 ...
        ]
    },
    {
        "integer": [
            -0,
            0,
            -5,
            1234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891234567891 ...
        ]
    }
]
```



## C


Reads a snippet of JSON into [https://github.com/lloyd/yajl YAJL's] tree format, then walks the tree to print it back out again.  The tree contains numbers both in an unparsed, string form, and also converted to long long or double when possible.  The example below demonstrates both ways of dealing with numbers.


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <yajl/yajl_tree.h>
#include <yajl/yajl_gen.h>

static void print_callback (void *ctx, const char *str, size_t len)
{
  FILE *f = (FILE *) ctx;
  fwrite (str, 1, len, f);
}

static void check_status (yajl_gen_status status)
{
  if (status != yajl_gen_status_ok)
    {
      fprintf (stderr, "yajl_gen_status was %d\n", (int) status);
      exit (EXIT_FAILURE);
    }
}

static void serialize_value (yajl_gen gen, yajl_val val, int parse_numbers)
{
  size_t i;

  switch (val->type)
    {
    case yajl_t_string:
      check_status (yajl_gen_string (gen,
                                     (const unsigned char *) val->u.string,
                                     strlen (val->u.string)));
      break;
    case yajl_t_number:
      if (parse_numbers  &&  YAJL_IS_INTEGER (val))
        check_status (yajl_gen_integer (gen, YAJL_GET_INTEGER (val)));
      else if (parse_numbers  &&  YAJL_IS_DOUBLE (val))
        check_status (yajl_gen_double (gen, YAJL_GET_DOUBLE (val)));
      else
        check_status (yajl_gen_number (gen, YAJL_GET_NUMBER (val),
                                       strlen (YAJL_GET_NUMBER (val))));
      break;
    case yajl_t_object:
      check_status (yajl_gen_map_open (gen));
      for (i = 0  ;  i < val->u.object.len  ;  i++)
        {
          check_status (yajl_gen_string (gen,
                                         (const unsigned char *) val->u.object.keys[i],
                                         strlen (val->u.object.keys[i])));
          serialize_value (gen, val->u.object.values[i], parse_numbers);
        }
      check_status (yajl_gen_map_close (gen));
      break;
    case yajl_t_array:
      check_status (yajl_gen_array_open (gen));
      for (i = 0  ;  i < val->u.array.len  ;  i++)
        serialize_value (gen, val->u.array.values[i], parse_numbers);
      check_status (yajl_gen_array_close (gen));
      break;
    case yajl_t_true:
      check_status (yajl_gen_bool (gen, 1));
      break;
    case yajl_t_false:
      check_status (yajl_gen_bool (gen, 0));
      break;
    case yajl_t_null:
      check_status (yajl_gen_null (gen));
      break;
    default:
      fprintf (stderr, "unexpectedly got type %d\n", (int) val->type);
      exit (EXIT_FAILURE);
    }
}

static void print_tree (FILE *f, yajl_val tree, int parse_numbers)
{
  yajl_gen gen;

  gen = yajl_gen_alloc (NULL);
  if (! gen)
    {
      fprintf (stderr, "yajl_gen_alloc failed\n");
      exit (EXIT_FAILURE);
    }

  if (0 == yajl_gen_config (gen, yajl_gen_beautify, 1)  ||
      0 == yajl_gen_config (gen, yajl_gen_validate_utf8, 1)  ||
      0 == yajl_gen_config (gen, yajl_gen_print_callback, print_callback, f))
    {
      fprintf (stderr, "yajl_gen_config failed\n");
      exit (EXIT_FAILURE);
    }

  serialize_value (gen, tree, parse_numbers);
  yajl_gen_free (gen);
}

int main (int argc, char **argv)
{
  char err_buf[200];
  const char *json =
    "{\"pi\": 3.14, \"large number\": 123456789123456789123456789, "
    "\"an array\": [-1, true, false, null, \"foo\"]}";
  yajl_val tree;

  tree = yajl_tree_parse (json, err_buf, sizeof (err_buf));
  if (! tree)
    {
      fprintf (stderr, "parsing failed because: %s\n", err_buf);
      return EXIT_FAILURE;
    }

  printf ("Treating numbers as strings...\n");
  print_tree (stdout, tree, 0);
  printf ("Parsing numbers to long long or double...\n");
  print_tree (stdout, tree, 1);

  yajl_tree_free (tree);

  return EXIT_SUCCESS;
}
```


```txt
Treating numbers as strings...
{
    "pi": 3.14,
    "large number": 123456789123456789123456789,
    "an array": [
        -1,
        true,
        false,
        null,
        "foo"
    ]
}
Parsing numbers to long long or double...
{
    "pi": 3.1400000000000001243,
    "large number": 1.2345678912345679134e+26,
    "an array": [
        -1,
        true,
        false,
        null,
        "foo"
    ]
}
```



## C#

This uses the [http://msdn.microsoft.com/en-us/library/system.web.script.serialization.javascriptserializer(v=VS.90).aspx JavaScriptSerializer] class which was shipped with .NET 3.5.


```c#
using System;
using System.Collections.Generic;
using System.Web.Script.Serialization;

class Program
{
    static void Main()
    {
        var people = new Dictionary<string, object> {{"1", "John"}, {"2", "Susan"}};
        var serializer = new JavaScriptSerializer();

        var json = serializer.Serialize(people);
        Console.WriteLine(json);

        var deserialized = serializer.Deserialize<Dictionary<string, object>>(json);
        Console.WriteLine(deserialized["2"]);

        var jsonObject = serializer.DeserializeObject(@"{ ""foo"": 1, ""bar"": [10, ""apples""] }");
        var data = jsonObject as Dictionary<string, object>;
        var array = data["bar"] as object[];
        Console.WriteLine(array[1]);
    }
}
```



## C++


```cpp
#include "Core/Core.h"

using namespace Upp;

CONSOLE_APP_MAIN
{
	JsonArray a;
	a << Json("name", "John")("phone", "1234567") << Json("name", "Susan")("phone", "654321");
	String txt = ~a;
	Cout() << txt << '\n';
	Value v = ParseJSON(txt);
	for(int i = 0; i < v.GetCount(); i++)
		Cout() << v[i]["name"] << ' ' << v[i]["phone"] << '\n';
}

```


C++11 {{libheader|nlohmann&#58;&#58;json}}

```cpp
#include <iostream>
#include <iomanip> // std::setw
#include <sstream>
#include <cassert>

#include "json.hpp"

using json = nlohmann::json;

int main( int argc, char* argv[] )
{
        std::string const expected =
R"delim123({
    "answer": {
        "everything": 42
    },
    "happy": true,
    "list": [
        1,
        0,
        2
    ],
    "name": "Niels",
    "nothing": null,
    "object": {
        "currency": "USD",
        "value": 42.99
    },
    "pi": 3.141
})delim123";

    json const jexpected = json::parse( expected );

    assert( jexpected["list"][1].get<int>() == 0 );
    assert( jexpected["object"]["currency"] == "USD" );

    json jhandmade = {
        {"pi", 3.141},
        {"happy", true},
        {"name", "Niels"},
        {"nothing", nullptr},
        {"answer", {
             {"everything", 42}
         }
        },
        {"list", {1, 0, 2}},
        {"object", {
             {"currency", "USD"},
             {"value", 42.99}
         }
        }
    };

    assert( jexpected == jhandmade );

    std::stringstream jhandmade_stream;
    jhandmade_stream << std::setw(4) << jhandmade;

    std::string jhandmade_string = jhandmade.dump(4);

    assert( jhandmade_string == expected );
    assert( jhandmade_stream.str() == expected );

    return 0;
}

```


=={{header|Caché ObjectScript}}==


```cos

Class Sample.JSON [ Abstract ]
{

ClassMethod GetPerson(ByRef pParms, Output pObject As %RegisteredObject) As %Status
{
	Set pObject=##class(Sample.Person).%OpenId(pParms("oid"))
	Quit $$$OK
}

}

```

```txt

SAMPLES>Set pParms("oid")=5
SAMPLES>Do ##class(%ZEN.Auxiliary.jsonProvider).%WriteJSONFromObject("", "Sample.JSON", "GetPerson", .pParms)

{
"_class":"Sample.Person",
"_id":5,
"Age":80,
"DOB":33603,
"FavoriteColors":["White","Purple"],
"Home":
   {
"_class":"Sample.Address",
   "City":"Denver",
   "State":"SC",
   "Street":"6932 Second Court",
   "Zip":51309
   },
"Name":"Tillem,Will D.",
"Office":
   {
"_class":"Sample.Address",
   "City":"Queensbury",
   "State":"NV",
   "Street":"1169 Main Drive",
   "Zip":25310
   },
"SSN":"729-56-4619",
"Spouse":null
}

SAMPLES>Read json
{"_class":"Sample.Person","_id":5,"Age":80,"DOB":33603,"FavoriteColors":["White"
,"Purple"],"Home":{"_class":"Sample.Address","City":"Denver","State":"S C","Stre
et":"6932 Second Court","Zip":51309},"Name":"Tillem,Will D.","O ffice":{"_class"
:"Sample.Address","City":"Queensbury","State":"NV"," Street":"1169 Main Drive","
Zip":25310},"SSN":"729-56-4619","Spouse":null}

SAMPLES>Do ##class(%ZEN.Auxiliary.jsonProvider).%ConvertJSONToObject(json, "", .person)

SAMPLES>Write person.Name
Tillem,Will D.
SAMPLES>Write person.FavoriteColors.Count()
2
SAMPLES>Write person.Home.Street
6932 Second Court

```



## Clojure

Library: [https://github.com/clojure/data.json data.json]

```clojure
(use 'clojure.data.json)

 ; Load as Clojure data structures and bind the resulting structure to 'json-map'.
(def json-map (read-json "{ \"foo\": 1, \"bar\": [10, \"apples\"] }"))

; Use pr-str to print out the Clojure representation of the JSON created by read-json.
(pr-str json-map)

; Pretty-print the Clojure representation of JSON. We've come full circle.
(pprint-json json-map)
```



## CoffeeScript


```coffeescript

sample =
  blue: [1, 2]
  ocean: 'water'

json_string = JSON.stringify sample
json_obj = JSON.parse json_string

console.log json_string
console.log json_obj

```



## Common Lisp

Library: [https://github.com/hankhero/cl-json cl-json]

```lisp

(ql:quickload '("cl-json"))

(json:encode-json
 '#( ((foo . (1 2 3)) (bar . t) (baz . #\!))
    "quux" 4/17 4.25))

(print (with-input-from-string
	   (s "{\"foo\": [1, 2, 3], \"bar\": true, \"baz\": \"!\"}")
	 (json:decode-json s)))


```



```txt
To load "cl-json":
  Load 1 ASDF system:
    cl-json
; Loading "cl-json"

[{"foo":[1,2,3],"bar":true,"baz":"!"},"quux",0.23529412,4.25]
((:FOO 1 2 3) (:BAR . T) (:BAZ . "!"))

```



## Crystal


```Ruby

require "json"

class Foo
  JSON.mapping(
    num: Int64,
    array: Array(String),
  )
end

def json
  foo = Foo.from_json(%({"num": 1, "array": ["a", "b"]}))
  puts("#{foo.num} #{foo.array}")
  puts(foo.to_json)
end

```



```Bash

1 ["a", "b"]
{"num":1,"array":["a","b"]}

```



## D


```d
import std.stdio, std.json;

void main() {
    auto j = parseJSON(`{ "foo": 1, "bar": [10, "apples"] }`);
    writeln(toJSON(&j));
}
```


```txt
{"foo":1,"bar":[10,"apples"]}
```



## EchoLisp

The '''json''' library allows to import/export basic JSON types (string ,numbers, arrays) and to translate EchoLisp objects (lists, dates, ..) from/to JSON objects and types. See reference documentation [[http://www.echolalie.org/echolisp/help.html#export-json]].

```lisp

;; JSON standard types : strings, numbers, and arrays (vectors)
(export-json #(6 7 8 9))  →   "[6,7,8,9]"
(export-json #("alpha" "beta" "gamma"))  →   "["alpha","beta","gamma"]"

(json-import "[6,7,8,9]")  →   #( 6 7 8 9)
(json-import #<< ["alpha","beta","gamma"] >>#)  →   #( "alpha" "beta" "gamma")

;; EchoLisp types : dates, rational, complex, big int
(export-json 3/4)  →   "{"_instanceof":"Rational","a":3,"b":4}"
(json-import #<< {"_instanceof":"Rational","a":666,"b":42} >>#)  →   111/7

;; Symbols
(export-json 'Simon-Gallubert)  →   "{"_instanceof":"Symbol","name":"Simon-Gallubert"}"
(json-import #<< {"_instanceof":"Symbol","name":"Antoinette-de-Gabolde"} >>#)
    →   Antoinette-de-Gabolde

;; Lists
(define my-list
    (export-json '( 43 4 5 ( 6 7 ( 8 9 )))))
    →    "{"_instanceof":"List" ,"array":[43,4,5,{"_instanceof":"List",
    "array":[6,7,{"_instanceof":"List",
    "array":[8,9],"circular":false}],"circular":false}],"circular":false}"

(json-import my-list)  →   (43 4 5 (6 7 (8 9)))

;; Structures
(struct Person (name pict))  →   #struct:Person [name pict]
(define antoinette (Person "antoinette" "👰"))   →   # (antoinette 👰)

(export-json antoinette)  →
    "{"_instanceof":"Struct", "struct":"Person","id":17,"fields":["antoinette","👰"]}"
(json-import
    #<< {"_instanceof":"Struct","struct":"Person","id":18,"fields":["simon","🎩"]} >>#)
	→   # (simon 🎩)


```



## EGL

Structures used both to construct and to parse JSON strings:

```EGL
record familyMember
	person person;
	relationships relationship[]?;
end

record person
	firstName string;
	lastName string;
	age int;
end

record relationship
	relationshipType string;
	id int;
end
```


Construct JSON string:

```EGL
people Person[]; // Array of people

people.appendElement(new Person { firstName = "Frederick", lastName = "Flintstone", age = 35} );
people.appendElement(new Person { firstName = "Wilma", lastName = "Flintstone", age = 34} );
people.appendElement(new Person { firstName = "Pebbles", lastName = "Flintstone", age = 2} );
people.appendElement(new Person { firstName = "Bernard", lastName = "Rubble", age = 32} );
people.appendElement(new Person { firstName = "Elizabeth", lastName = "Rubble", age = 29} );
people.appendElement(new Person { firstName = "Bam Bam", lastName = "Rubble", age = 2} );

family Dictionary; // A dictionary of family members using a uid as key
family["1"] = new FamilyMember{ person = people[1], relationships = [new Relationship{ relationshipType="spouse", id = 2 }, new Relationship{ relationshipType="child", id = 3}] };
family["2"] = new FamilyMember{ person = people[2], relationships = [new Relationship{ relationshipType="spouse", id = 1 }, new Relationship{ relationshipType="child", id = 3}] };
family["3"] = new FamilyMember{ person = people[3], relationships = [new Relationship{ relationshipType="mother", id = 2 }, new Relationship{ relationshipType="father", id = 1}] };
family["4"] = new FamilyMember{ person = people[4], relationships = [new Relationship{ relationshipType="spouse", id = 5 }, new Relationship{ relationshipType="child", id = 6}] };
family["5"] = new FamilyMember{ person = people[5], relationships = [new Relationship{ relationshipType="spouse", id = 4 }, new Relationship{ relationshipType="child", id = 6}] };
family["6"] = new FamilyMember{ person = people[6], relationships = [new Relationship{ relationshipType="mother", id = 5 }, new Relationship{ relationshipType="father", id = 4}] };

// Convert dictionary of family members to JSON string
jsonString string = jsonLib.convertToJSON(family);

// Show JSON string
SysLib.writeStdout(jsonString);
```


```txt
{
"1":{"person":{"firstName":"Frederick","lastName":"Flintstone","age":35},"relationships":[{"relationshipType":"spouse","id":2},{"relationshipType":"child","id":3}]},
"2":{"person":{"firstName":"Wilma","lastName":"Flintstone","age":34},"relationships":[{"relationshipType":"spouse","id":1},{"relationshipType":"child","id":3}]},
"3":{"person":{"firstName":"Pebbles","lastName":"Flintstone","age":2},"relationships":[{"relationshipType":"mother","id":2},{"relationshipType":"father","id":1}]},
"4":{"person":{"firstName":"Bernard","lastName":"Rubble","age":32},"relationships":[{"relationshipType":"spouse","id":5},{"relationshipType":"child","id":6}]},
"5":{"person":{"firstName":"Elizabeth","lastName":"Rubble","age":29},"relationships":[{"relationshipType":"spouse","id":4},{"relationshipType":"child","id":6}]},
"6":{"person":{"firstName":"Bam Bam","lastName":"Rubble","age":2},"relationships":[{"relationshipType":"mother","id":5},{"relationshipType":"father","id":4}]}
}
```


```txt
{
    "1": {
        "person": {
            "firstName": "Frederick",
            "lastName": "Flintstone",
            "age": 35
        },
        "relationships": [
            {
                "relationshipType": "spouse",
                "id": 2
            },
            {
                "relationshipType": "child",
                "id": 3
            }
        ]
    },
...
}
```


Parse JSON:

```EGL
// Convert JSON string into dictionary of family members
family Dictionary;
jsonLib.convertFromJSON(jsonString, family);

// List family members and their relationships
familyMember FamilyMember;
relation FamilyMember;

keys string[] = family.getKeys();
for(i int from 1 to keys.getSize())

	SysLib.writeStdout("----------------------------------------------------");

	familyMember = family[keys[i]];

	SysLib.writeStdout(familyMember.person.lastName + ", " + familyMember.person.firstName + " - " + familyMember.person.age);

	for(j int from 1 to familyMember.relationships.getSize())
		id string = familyMember.relationships[j].id;
		relation = family[id];
		SysLib.writeStdout(familyMember.relationships[j].relationshipType + ":  " +
		relation.person.lastName + ", " + relation.person.firstName);
	end

end
```


```txt
Flintstone, Frederick - 35
spouse: Flintstone, Wilma
child: Flintstone, Pebbles
----------------------------------------------------
Flintstone, Wilma - 34
spouse: Flintstone, Frederick
child: Flintstone, Pebbles
----------------------------------------------------
Flintstone, Pebbles - 2
mother: Flintstone, Wilma
father: Flintstone, Frederick
----------------------------------------------------
Rubble, Bernard - 32
spouse: Rubble, Elizabeth
child: Rubble, Bam Bam
----------------------------------------------------
Rubble, Elizabeth - 29
spouse: Rubble, Bernard
child: Rubble, Bam Bam
----------------------------------------------------
Rubble, Bam Bam - 2
mother: Rubble, Elizabeth
father: Rubble, Bernard
```


The examples above illustrate that it is possible to perform manual conversions to and from a JSON format but in EGL it is much more common for the programming language to handle these conversion automatically as a natural part of service invocations. Below is an example of a function definition designed to consume the Google Maps Geocoding service. The results are returned in a JSON format and parsed by EGL into records that mirror the structure of the reply.


```EGL
// Service function definition
function geocode(address String) returns (GoogleGeocoding) {
    @Resource{uri = "binding:GoogleGeocodingBinding"},
    @Rest{method = _GET, uriTemplate = "/json?address={address}&sensor=false",
    requestFormat = None, responseFormat = JSON}
}

// Invoke service function
call geocode("111 Maple Street, Somewhere, CO") returning to callback;

function callBack(result GoogleGeocoding in)
    SysLib.writeStdout(result.status);
    SysLib.writeStdout(result.results[1].geometry.location.lat);
    SysLib.writeStdout(result.results[1].geometry.location.lng);
end
```




## Elena

ELENA 4.x

```elena
import extensions;
import extensions'dynamic;

public program()
{
    var json := "{ ""foo"": 1, ""bar"": [10, ""apples""] }";

    var o := json.fromJson();

    console.printLine("json.foo=",o.foo);
    console.printLine("json.bar=",o.bar)
}
```

```txt

json.foo=1
json.bar=10,apples

```



## Erlang

Use the JSON library for Erlang (mochijson) from [https://github.com/mochi/mochiweb/blob/master/src/mochijson.erl mochiweb]. The JSON code is extracted from [http://en.wikipedia.org/wiki/JSON#JSON_example wikipedia]

```Erlang

-module(json).
-export([main/0]).

main() ->
	JSON =
		"{
		    \"firstName\": \"John\",
		    \"lastName\": \"Smith\",
		    \"age\": 25,
		    \"address\": {
		        \"streetAddress\": \"21 2nd Street\",
		        \"city\": \"New York\",
		        \"state\": \"NY\",
		        \"postalCode\": \"10021\"
		    },
		    \"phoneNumber\": [
		        {
		            \"type\": \"home\",
		            \"number\": \"212 555-1234\"
		        },
		        {
		            \"type\": \"fax\",
		            \"number\": \"646 555-4567\"
		        }
		    ]
		}",
	Erlang =
		{struct,
			[{"firstName","John"},
	         {"lastName","Smith"},
	         {"age",25},
	         {"address",
	          {struct,[{"streetAddress","21 2nd Street"},
	                   {"city","New York"},
	                   {"state","NY"},
	                   {"postalCode","10021"}]}},
	         {"phoneNumber",
	          {array,[{struct,[{"type","home"},{"number","212 555-1234"}]},
	                  {struct,[{"type","fax"},{"number","646 555-4567"}]}]}}]},
	io:format("JSON -> Erlang\n~p\n",[mochijson:decode(JSON)]),
	io:format("Erlang -> JSON\n~s\n",[mochijson:encode(Erlang)]).

```


```txt
JSON -> Erlang
{struct,[{"firstName","John"},
         {"lastName","Smith"},
         {"age",25},
         {"address",
          {struct,[{"streetAddress","21 2nd Street"},
                   {"city","New York"},
                   {"state","NY"},
                   {"postalCode","10021"}]}},
         {"phoneNumber",
          {array,[{struct,[{"type","home"},{"number","212 555-1234"}]},
                  {struct,[{"type","fax"},{"number","646 555-4567"}]}]}}]}
Erlang -> JSON
{"firstName":"John","lastName":"Smith","age":25,"address":{"streetAddress":"21 2nd Street","city":"New York","state":"NY","postalCode":"10021"},"phoneNumber":[{"type":"home","number":"212 555-1234"},{"type":"fax","number":"646 555-4567"}]}
```



## Factor


```Factor

USING: json.writer json.reader ;

SYMBOL: foo

! Load a JSON string into a data structure
"[[\"foo\",1],[\"bar\",[10,\"apples\"]]]" json> foo set


! Create a new data structure and serialize into JSON
{ { "blue" { "ocean" "water" } } >json

```



## Fantom



```fantom

using util

class Json
{
  public static Void main ()
  {
    Str input := """{"blue": [1, 2], "ocean": "water"}"""
    Map jsonObj := JsonInStream(input.in).readJson

    echo ("Value for 'blue' is: " + jsonObj["blue"])
    jsonObj["ocean"] = ["water":["cold", "blue"]]
    Map ocean := jsonObj["ocean"]
    echo ("Value for 'ocean/water' is: " + ocean["water"])
    output := JsonOutStream(Env.cur.out)
    output.writeJson(jsonObj)
    echo ()
  }
}

```


```txt

Value for 'blue' is: [1, 2]
Value for 'ocean/water' is: [cold, blue]
{"blue":[1,2],
"ocean":{"water":["cold","blue"]}}

```



## Fortran

Using [https://github.com/jacobwilliams/json-fortran json-fortran] library. Creating the json example file / reading a JSON string.

```txt

{
  "PhoneBook": [
    {
      "name": "Adam",
      "phone": "0000001"
    },
    {
      "name": "Eve",
      "phone": "0000002"
    },
    {
      "name": "Julia",
      "phone": "6666666"
    }
  ]
}

```



```fortran

program json_fortran
   use json_module
   implicit none

   type phonebook_type
      character(len=:),allocatable :: name
      character(len=:),allocatable :: phone
   end type phonebook_type

   type(phonebook_type), dimension(3) :: PhoneBook
   integer :: i
   type(json_value),pointer :: json_phonebook,p,e
   type(json_file) :: json

   PhoneBook(1) % name = 'Adam'
   PhoneBook(2) % name = 'Eve'
   PhoneBook(3) % name = 'Julia'
   PhoneBook(1) % phone = '0000001'
   PhoneBook(2) % phone = '0000002'
   PhoneBook(3) % phone = '6666666'

   call json_initialize()

   !create the root structure:
   call json_create_object(json_phonebook,'')

   !create and populate the phonebook array:
   call json_create_array(p,'PhoneBook')
   do i=1,3
      call json_create_object(e,'')
      call json_add(e,'name',PhoneBook(i)%name)
      call json_add(e,'phone',PhoneBook(i)%phone)
      call json_add(p,e) !add this element to array
      nullify(e) !cleanup for next loop
   end do
   call json_add(json_phonebook,p) !add p to json_phonebook
   nullify(p) !no longer need this

   !write it to a file:
   call json_print(json_phonebook,'phonebook.json')

   ! read directly from a character string
   call json%load_from_string('{ "PhoneBook": [ { "name": "Adam", "phone": "0000001" },&
   { "name": "Eve", "phone": "0000002" }, { "name": "Julia", "phone": "6666666" } ]}')
   ! print it to the console
   call json%print_file()

end program json_fortran

```



## FunL

Since FunL map syntax is conforms to JSON, the built-in function
<code>eval()</code> can be used to parse a JSON string.
Built-in <code>println()</code> also produces JSON conformant output.
This method only uses built-in functions but is comparatively slow.

```funl
println( eval('{ "foo": 1, "bar": [10, "apples"] }') )
```


Using module <code>json</code> gives better performance and also pretty prints the JSON output.

```funl
import json.*

DefaultJSONWriter.write( JSONReader({'ints', 'bigInts'}).fromString('{ "foo": 1, "bar": [10, "apples"] }') )
```


```txt

{"foo": 1, "bar": [10, "apples"]}
{
  "foo": 1,
  "bar": [
    10,
    "apples"
  ]
}

```


=={{header|F_Sharp|F#}}==
There are several ways:

1. Using Json.Net

```fsharp

open Newtonsoft.Json
type Person = {ID: int; Name:string}
let xs = [{ID = 1; Name = "First"} ; { ID = 2; Name = "Second"}]

let json = JsonConvert.SerializeObject(xs)
json |> printfn "%s"

let xs1 = JsonConvert.DeserializeObject<Person list>(json)
xs1 |> List.iter(fun x -> printfn "%i  %s" x.ID x.Name)

```


Print:

```fsharp
[{"ID":1,"Name":"First"},{"ID":2,"Name":"Second"}]
1  First
2  Second

```

2. Using FSharp.Data

```fsharp
open FSharp.Data
open FSharp.Data.JsonExtensions

type Person = {ID: int; Name:string}
let xs = [{ID = 1; Name = "First"} ; { ID = 2; Name = "Second"}]

let infos = xs |> List.map(fun x -> JsonValue.Record([| "ID", JsonValue.Number(decimal x.ID); "Name", JsonValue.String(x.Name) |]))
            |> Array.ofList |> JsonValue.Array

infos |> printfn "%A"
match JsonValue.Parse(infos.ToString()) with
| JsonValue.Array(x) -> x |> Array.map(fun x -> {ID = System.Int32.Parse(string x?ID); Name = (string x?Name)})
| _ -> failwith "fail json"
|> Array.iter(fun x -> printfn "%i  %s" x.ID x.Name)
```

Print:

```fsharp
[
  {
    "ID": 1,
    "Name": "First"
  },
  {
    "ID": 2,
    "Name": "Second"
  }
]
1  "First"
2  "Second"

```

3. Alternative way of parsing: JsonProvider

```fsharp
open FSharp.Data
type Person = {ID: int; Name:string}
type People = JsonProvider<""" [{"ID":1,"Name":"First"},{"ID":2,"Name":"Second"}] """>

People.GetSamples()
|> Array.map(fun x -> {ID = x.Id; Name = x.Name} )
|> Array.iter(fun x -> printfn "%i  %s" x.ID x.Name)
```

Print:
```fsharp

1  First
2  Second

```



## Go

Example below shows simple correspondence between JSON objects and Go maps, and shows that you don't have to know anything about the structure of the JSON data to read it in.

```go
package main

import "encoding/json"
import "fmt"

func main() {
    var data interface{}
    err := json.Unmarshal([]byte(`{"foo":1, "bar":[10, "apples"]}`), &data)
    if err == nil {
        fmt.Println(data)
    } else {
        fmt.Println(err)
    }

    sample := map[string]interface{}{
        "blue":  []interface{}{1, 2},
        "ocean": "water",
    }
    json_string, err := json.Marshal(sample)
    if err == nil {
        fmt.Println(string(json_string))
    } else {
        fmt.Println(err)
    }
}
```

```txt

map[bar:[10 apples] foo:1]
{"blue":[1,2],"ocean":"water"}

```

Example below demonstrates more typical case where you have an expected correspondence between JSON data and some composite data types in your program, and shows how the correspondence doesn't have to be exact.

```go
package main

import "encoding/json"
import "fmt"

type Person struct {
    Name string   `json:"name"`
    Age  int      `json:"age,omitempty"`
    Addr *Address `json:"address,omitempty"`
    Ph   []string `json:"phone,omitempty"`
}

type Address struct {
    Street string `json:"street"`
    City   string `json:"city"`
    State  string `json:"state"`
    Zip    string `json:"zip"`
}

func main() {
    // compare with output, note apt field ignored, missing fields
    // have zero values.
    jData := []byte(`{
        "name": "Smith",
        "address": {
            "street": "21 2nd Street",
            "apt": "507",
            "city": "New York",
            "state": "NY",
            "zip": "10021"
        }
    }`)
    var p Person
    err := json.Unmarshal(jData, &p)
    if err != nil {
        fmt.Println(err)
    } else {
        fmt.Printf("%+v\n  %+v\n\n", p, p.Addr)
    }

    // compare with output, note empty fields omitted.
    pList := []Person{
        {
            Name: "Jones",
            Age:  21,
        },
        {
            Name: "Smith",
            Addr: &Address{"21 2nd Street", "New York", "NY", "10021"},
            Ph:   []string{"212 555-1234", "646 555-4567"},
        },
    }
    jData, err = json.MarshalIndent(pList, "", "    ")
    if err != nil {
        fmt.Println(err)
    } else {
        fmt.Println(string(jData))
    }
}
```

```txt

{Name:Smith Age:0 Addr:0xf840026080 Ph:[]}
  &{Street:21 2nd Street City:New York State:NY Zip:10021}

[
    {
        "name": "Jones",
        "age": 21
    },
    {
        "name": "Smith",
        "address": {
            "street": "21 2nd Street",
            "city": "New York",
            "state": "NY",
            "zip": "10021"
        },
        "phone": [
            "212 555-1234",
            "646 555-4567"
        ]
    }
]

```



## Gosu

Gosu consumes JSON as a Dynamic type via this core API:

```javascript

gw.lang.reflect.json.Json#fromJson( String json ) : javax.script.Bindings

```

As the signature of the method suggests, you pass in a JSON string and receive standard script Bindings in return. Bindings is basically a map mirroring the tree structure of the JSON object. Internally Gosu supports any Bindings instance as a Dynamic Expando object. Essentially this means you can directly cast any Bindings instance to Dynamic and treat it as an Expando.

The following JSON example illustrates this:

Sample Person JSON (from http://gosu-lang.github.io/data/person.json):

```javascript
{
  "Name": "Dickson Yamada",
  "Age": 39,
  "Address": {
    "Number": 9604,
    "Street": "Donald Court",
    "City": "Golden Shores",
    "State": "FL"
  },
  "Hobby": [
    {
      "Category": "Sport",
      "Name": "Baseball"
    },
    {
      "Category": "Recreation",
      "Name": "Hiking"
    }
  ]
}

```

And the dynamic Gosu code to access it:

```javascript

var personUrl = new URL( "http://gosu-lang.github.io/data/person.json" )
var person: Dynamic = personUrl.JsonContent
print( person.Name )

```

Notice the JsonContent property on URL:

```javascript

personUrl.JsonContent

```

This is a convenient enhancement property Gosu provides for Java’s URL class. It does all the work to get the JSON text and calls the new Json#fromJson() method for you. It also declares the Dynamic type for you as its return type, so the declared Dynamic type on the person var is unnecessary; it’s there to clearly demonstrate that the person var is indeed Dynamic.

As you can see we can access the Name property from the JSON object from the person var. This is all well and good, but falls short of our desired level of JSON support. Gosu being a static language, we really want that Name reference to be statically verified as well as code-completed in the IDE.

Here’s how we make the previous example work statically:

```javascript

print( person.toStructure( "Person", false ) )

```

Gosu enhances Bindings with the method, toStructure( name: String, mutable: boolean ). Note the resulting structure is optionally mutable via the mutable argument. This method generates the complete nesting of types plus convenient factory methods:

```javascript

structure Person {
  static function fromJson( jsonText: String ): Person {
    return gw.lang.reflect.json.Json.fromJson( jsonText ) as Person
  }
  static function fromJsonUrl( url: String ): Person {
    return new java.net.URL( url ).JsonContent
  }
  static function fromJsonUrl( url: java.net.URL ): Person {
    return url.JsonContent
  }
  static function fromJsonFile( file: java.io.File ) : Person {
    return fromJsonUrl( file.toURI().toURL() )
  }
  property get Address(): Address
  property get Hobby(): List<Hobby>
  property get Age(): Integer
  property get Name(): String
  structure Address {
    property get Number(): Integer
    property get State(): String
    property get Street(): String
    property get City(): String
  }
  structure Hobby {
    property get Category(): String
    property get Name(): String
  }
}

```

The Person structure reflects the JSON object’s implied type nesting. You can do whatever you like with this type. You can embed it as an inner structure in an existing class or make a top-level type. In any case all the types in the JSON object are uniquely preserved in one structure. Use it like this:

```javascript

var person = Person.fromJsonUrl( personUrl )
print( person.Name )
print( person.Address.City )
print( person.Hobby[0].Name )

```

All statically verified and fully code completion friendly!

Other features:

```javascript

print( person.toJson() ) // toJson() generates the Expando bindings to a JSON string
print( person.toGosu() ) // toGosu() generates any Bindings instance to a Gosu Expando initializer string
print( person.toXml() ) // toXml() generates any Bindings instance to standard XML

```

And similar to JavaScript, you can directly evaluate a Gosu Expando initializer string:

```javascript

var clone = eval( person.toGosu() )

```



## Groovy

Solution requires Groovy 1.8 or later.

Note that JsonSlurper accepts an extra comma such as [1,2,3,].  This is an extension to the [[http://www.json.org/fatfree.html JSON grammar]].


```groovy
def slurper = new groovy.json.JsonSlurper()
def result = slurper.parseText('''
{
    "people":[
        {"name":{"family":"Flintstone","given":"Frederick"},"age":35,"relationships":{"wife":"people[1]","child":"people[4]"}},
        {"name":{"family":"Flintstone","given":"Wilma"},"age":32,"relationships":{"husband":"people[0]","child":"people[4]"}},
        {"name":{"family":"Rubble","given":"Barnard"},"age":30,"relationships":{"wife":"people[3]","child":"people[5]"}},
        {"name":{"family":"Rubble","given":"Elisabeth"},"age":32,"relationships":{"husband":"people[2]","child":"people[5]"}},
        {"name":{"family":"Flintstone","given":"Pebbles"},"age":1,"relationships":{"mother":"people[1]","father":"people[0]"}},
        {"name":{"family":"Rubble","given":"Bam-Bam"},"age":1,"relationships":{"mother":"people[3]","father":"people[2]"}},
    ]
}
''')
```


Test:

```groovy
result.each { println it.key; it.value.each {person -> println person} }

assert result.people[0].name == [family:'Flintstone', given:'Frederick']
assert result.people[4].age == 1
assert result.people[2].relationships.wife == 'people[3]'
assert result.people[3].name == [family:'Rubble', given:'Elisabeth']
assert Eval.x(result, 'x.' + result.people[2].relationships.wife + '.name') == [family:'Rubble', given:'Elisabeth']
assert Eval.x(result, 'x.' + result.people[1].relationships.husband + '.name') == [family:'Flintstone', given:'Frederick']
```


```txt
people
[age:35, name:[given:Frederick, family:Flintstone], relationships:[child:people[4], wife:people[1]]]
[age:32, name:[given:Wilma, family:Flintstone], relationships:[child:people[4], husband:people[0]]]
[age:30, name:[given:Barnard, family:Rubble], relationships:[child:people[5], wife:people[3]]]
[age:32, name:[given:Elisabeth, family:Rubble], relationships:[child:people[5], husband:people[2]]]
[age:1, name:[given:Pebbles, family:Flintstone], relationships:[mother:people[1], father:people[0]]]
[age:1, name:[given:Bam-Bam, family:Rubble], relationships:[mother:people[3], father:people[2]]]
```



## Halon


```halon
$data = json_decode(''{ "foo": 1, "bar": [10, "apples"] }'');

$sample = ["blue" => [1, 2], "ocean" => "water"];
$jsonstring = json_encode($sample, ["pretty_print" => true]);
```



## Harbour

Parse JSON string into the ''arr'' variable:

```visualfoxpro
LOCAL arr
hb_jsonDecode( '[101,[26,"Test1"],18,false]', @arr )
```

{{out}} the JSON representation of an array ''arr'':

```visualfoxpro
LOCAL arr := { 101, { 18, "Test1" }, 18, .F. }
? hb_jsonEncode( arr )
// The output is:
// [101,[26,"Test1"],18,false]
```



## Haskell


Uses the Aeson library from hackage (http://hackage.haskell.org/package/aeson).


```Haskell

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Attoparsec (parseOnly)
import Data.Text
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as S

testdoc = object [
    "foo"   .= (1 :: Int),
    "bar"   .= ([1.3, 1.6, 1.9] :: [Double]),
    "baz"   .= ("some string" :: Text),
    "other" .= object [
        "yes" .= ("sir" :: Text)
        ]
    ]

main = do
    let out = encode testdoc
    B.putStrLn out
    case parseOnly json (S.concat $ B.toChunks out) of
        Left e -> error $ "strange error re-parsing json: " ++ (show e)
        Right v | v /= testdoc -> error "documents not equal!"
        Right v | otherwise    -> print v


```


An example using Aeson and TemplateHaskell. Note that it can handle the absence of keys.

```haskell

{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
import Data.Aeson
import Data.Aeson.TH

data Person = Person { firstName :: String
                     , lastName  :: String
                     , age :: Maybe Int
                     } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Person)

main = do
    let test1 = "{\"firstName\":\"Bob\", \"lastName\":\"Smith\"}"
        test2 = "{\"firstName\":\"Miles\", \"lastName\":\"Davis\", \"age\": 45}"
    print (decode test1 :: Maybe Person)
    print (decode test2 :: Maybe Person)

```


An example using Aeson and GHC.Generics. Note that it can handle the absence of keys.

```haskell

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import Data.Aeson
import GHC.Generics

data Person = Person { firstName :: String
                     , lastName  :: String
                     , age :: Maybe Int
                     } deriving (Show, Eq, Generic)

instance FromJSON Person
instance ToJSON Person

main = do
    let test1 = "{\"firstName\":\"Bob\", \"lastName\":\"Smith\"}"
        test2 = "{\"firstName\":\"Miles\", \"lastName\":\"Davis\", \"age\": 45}"
    print (decode test1 :: Maybe Person)
    print (decode test2 :: Maybe Person)

```



## Hoon


```Hoon
:-  %say
|=  [^ [in=@tas ~] ~]
:-  %noun
  =+  obj=(need (poja in))                              :: try parse to json
  =+  typ=$:(name=@tas age=@ud)                         :: datastructure
  =+  spec=(ot name/so age/ni ~):jo                     :: parsing spec
  ?.  ?=([%o *] obj)                                    :: isnt an object?
    ~
  =+  ^=  o
    %.  %.  (spec obj)                                  :: parse with spec
      need                                              :: panic if failed
    ,typ                                                :: cast to type
  =.  age.o  +(age.o)                                   :: increment its age...
  %:  crip  %:  pojo                                    :: pretty-print result
    (jobe [%name s/name.o] [%age n/(crip <age.o>)] ~)   :: convert back to json
```


Usage: Put code in gen/json.hoon

```txt

> +json '{"name":"pojo", "age":4}'
'{"age":5,"name":"pojo"}'

```



## J


Here is a minimal implementation based on [http://www.jsoftware.com/pipermail/chat/2007-April/000462.html an old email message].


```j
NB.               character classes:
NB. 0: whitespace
NB. 1: "
NB. 2: \
NB. 3: [ ] , { } :
NB. 4: ordinary
classes=.3<. '"\[],{}:' (#@[ |&>: i.) a.
classes=.0 (I.a.e.' ',CRLF,TAB)} (]+4*0=])classes

words=:(0;(0 10#:10*".;._2]0 :0);classes)&;: NB. states:
  0.0  1.1  2.1  3.1  4.1  NB. 0 whitespace
  1.0  5.0  6.0  1.0  1.0  NB. 1 "
  4.0  4.0  4.0  4.0  4.0  NB. 2 \
  0.3  1.2  2.2  3.2  4.2  NB. 3 { : , } [ ]
  0.3  1.2  2.0  3.2  4.0  NB. 4 ordinary
  0.3  1.2  2.2  3.2  4.2  NB. 5 ""
  1.0  1.0  1.0  1.0  1.0  NB. 6 "\
)

tokens=. ;:'[ ] , { } :'
actions=: lBra`rBracket`comma`lBra`rBrace`colon`value

NB. action verbs argument conventions:
NB.   x -- boxed json word
NB.   y -- boxed json state stack
NB.   result -- new boxed json state stack
NB.
NB. json state stack is an list of boxes of incomplete lists
NB. (a single box for complete, syntactically valid json)
jsonParse=: 0 {:: (,a:) ,&.> [: actions@.(tokens&i.@[)/ [:|.a:,words

lBra=: a: ,~ ]
rBracket=: _2&}.@], [:< _2&{::@], _1&{@]
comma=: ]
rBrace=: _2&}.@], [:< _2&{::@](, <)  [:|: (2,~ [: -:@$ _1&{::@]) $ _1&{::@]
colon=: ]
value=: _1&}.@], [:< _1&{::@], jsonValue&.>@[

NB. hypothetically, jsonValue should strip double quotes
NB. interpret back slashes
NB. and recognize numbers
jsonValue=:]


require'strings'
jsonSer2=: jsonSer1@(<"_1^:(0>.#@$-1:))
jsonSer1=: ']' ,~ '[' }:@;@; (',' ,~ jsonSerialize)&.>
jsonSer0=: '"', jsonEsc@:":, '"'"_
jsonEsc=: rplc&(<;._1' \ \\ " \"')
jsonSerialize=:jsonSer0`jsonSer2@.(*@L.)
```


Example use:

<lang>   jsonParse'{ "blue": [1,2], "ocean": "water" }'
┌────────────────┐
│┌──────┬───────┐│
││"blue"│"ocean"││
│├──────┼───────┤│
││┌─┬─┐ │"water"││
│││1│2│ │       ││
││└─┴─┘ │       ││
│└──────┴───────┘│
└────────────────┘
└──────────────────────────────┘
   jsonSerialize jsonParse'{ "blue": [1,2], "ocean": "water" }'
[[["\"blue\"","\"ocean\""],[["1","2"],"\"water\""]]]
```


Note that these are not strict inverses of each other.  These routines allow data to be extracted from json and packed into json format, but only in a minimalistic sense.  No attempts are made to preserve the subtleties of type and structure which json can carry.  This should be good enough for most applications which are required to deal with json but will not be adequate for ill behaved applications which exploit the typing mechanism to carry significant information.

Also, a different serializer will probably be necessary, if you are delivering json to legacy javascript.  Nevertheless, these simplifications are probably appropriate for practical cases.


## Java

This uses [http://code.google.com/p/google-gson/ Gson], a library to convert JSON to Java objects and vice-versa.

```Java
import com.google.gson.Gson;

public class JsonExample {

	public static void main(String[] args) {
		Gson gson = new Gson();
		String json = "{ \"foo\": 1, \"bar\": [ \"10\", \"apples\"] }";

		MyJsonObject obj = gson.fromJson(json, MyJsonObject.class);

		System.out.println(obj.getFoo());

		for(String bar : obj.getBar()) {
			System.out.println(bar);
		}

		obj = new MyJsonObject(2, new String[] { "20", "oranges" });
		json = gson.toJson(obj);

		System.out.println(json);
	}

}

class MyJsonObject {

	private int foo;
	private String[] bar;

	public MyJsonObject(int foo, String[] bar) {
		this.foo = foo;
		this.bar = bar;
	}

	public int getFoo() {
		return foo;
	}

	public String[] getBar() {
		return bar;
	}

}
```



## JavaScript

Requires JSON library, now present in all major browsers.

```JavaScript
var data = JSON.parse('{ "foo": 1, "bar": [10, "apples"] }');

var sample = { "blue": [1,2], "ocean": "water" };
var json_string = JSON.stringify(sample);
```


JSON is called ''JavaScript'' Object Notation, but JSON differs form JavaScript object literal. cf. [https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/JSON MDN/JSON]


## jq


JSON is jq's native data format, so nothing need be done to parse JSON input. For example, to "pretty print" a stream of JSON entities (including scalars), it would be sufficient to use the jq program:
```jq> . </lang


Here are the jq equivalents of the examples given in the section on Julia, assuming the file data.json holds the following JSON text:

{ "blue": [1,2], "ocean": "water" }


```jq
jq -c . data.json
```

produces:

{"blue":[1,2],"ocean":"water"}


```jq>jq tostring data.json</lang

produces:
"{\"blue\":[1,2],\"ocean\":\"water\"}"


## Jsish


```javascript
prompt$ jsish
Jsish interactive: see 'help [cmd]'
# var data = JSON.parse('{ "foo": 1, "bar": [10, "apples"] }');
variable
# data
{ bar:[ 10, "apples" ], foo:1 }

# var sample = { blue: [1,2], ocean: "water" };
variable
# sample
{ blue:[ 1, 2 ], ocean:"water" }

# puts(JSON.stringify(sample))
{ "blue":[ 1, 2 ], "ocean":"water" }
```



## Julia

```julia
# Pkg.add("JSON") ... an external library http://docs.julialang.org/en/latest/packages/packagelist/
using JSON

sample = Dict()
sample["blue"] = [1, 2]
sample["ocean"] = "water"

@show sample jsonstring = json(sample)
@show jsonobj = JSON.parse(jsonstring)

@assert jsonstring == "{\"ocean\":\"water\",\"blue\":[1,2]}"
@assert jsonobj == Dict("ocean" => "water", "blue" => [1, 2])
@assert typeof(jsonobj) == Dict{String, Any}
```



## Kotlin

We use Kotlin JS here to obtain access to the JavaScript JSON object:

```scala
// version 1.2.21

data class JsonObject(val foo: Int, val bar: Array<String>)

data class JsonObject2(val ocean: String, val blue: Array<Int>)

fun main(args: Array<String>) {
    // JSON to object
    val data: JsonObject = JSON.parse("""{ "foo": 1, "bar": ["10", "apples"] }""")
    println(JSON.stringify(data))

    // object to JSON
    val data2 = JsonObject2("water", arrayOf(1, 2))
    println(JSON.stringify(data2))
}
```


```txt

{"foo":1,"bar":["10","apples"]}
{"ocean":"water","blue":[1,2]}

```



## Lasso


```Lasso
// Javascript objects are represented by maps in Lasso
local(mymap = map(
	'success'	= true,
	'numeric'	= 11,
	'string'	= 'Eleven'
))

json_serialize(#mymap) // {"numeric": 11,"string": "Eleven","success": true}
'<br />'

// Javascript arrays are represented by arrays
local(opendays = array(
	'Monday',
	'Tuesday'
))

local(closeddays = array(
	'Wednesday',
	'Thursday',
	'Friday'
))

json_serialize(#opendays) // ["Monday", "Tuesday"]
'<br />'
json_serialize(#closeddays) // ["Wednesday", "Thursday", "Friday"]
'<br />'

#mymap -> insert('Open' = #opendays)
#mymap -> insert('Closed' = #closeddays)

local(myjson = json_serialize(#mymap))
#myjson // {"Closed": ["Wednesday", "Thursday", "Friday"],"numeric": 11,"Open": ["Monday", "Tuesday"],"string": "Eleven","success": true}
'<br />'

json_deserialize(#myjson) // map(Closed = array(Wednesday, Thursday, Friday), numeric = 11, Open = array(Monday, Tuesday), string = Eleven, success = true)
```



## LFE


This example uses the third-party library "Jiffy".


### Encoding


```lisp

(: jiffy encode (list 1 2 3 '"apple" 'true 3.14))

```


The result from that can be made a little more legible with the following:

```lisp

(: erlang binary_to_list
  (: jiffy encode (list 1 2 3 '"apple" 'true 3.14)))

```



### Decoding

We can run the encoding example in reverse, and get back what we put in above with the following:

```lisp

(: jiffy decode '"[1,2,3,[97,112,112,108,101],true,3.14]")

```


Here's a key-value example:

```lisp

(: jiffy decode '"{\"foo\": [1, 2, 3]}")

```



### Decoding to Patterns

We can also extract the key and value using Erlang patterns:

```lisp

(let (((tuple (list (tuple key value)))
       (: jiffy decode '"{\"foo\": [1, 2, 3]}")))
  (: io format '"~p: ~p~n" (list key value)))

```



## Lingo

Lingo has no native JSON support. A JSON library could of course be written in pure Lingo or as binary plugin ("Xtra").

But since Director - the only implementation of Lingo - also includes a SpiderMonkey JS engine (rather outdated and without native JSON object), we can implement a simple (unsafe) JSON encoder/decoder with the following two movie scripts:

JavaScript movie script "JSON":

```javascript
//--------------------------------------
// Simple (unsafe) JSON decoder based on eval()
// @param {string} json
// @return {any}
//--------------------------------------
function json_decode (json){
  var o;
  eval('o='+json);
  return _json_decode_val(o);
}

function _json_decode_val (o){
  if (o==null) return undefined;
  switch(typeof(o)){
    case "object":
      if (o instanceof Array){
        var v = list();
        var cnt = o.length;
        for (i=0;i<cnt;i++){
          v.add(_json_decode_val(o[i]));
        }
      }else{
        var v = propList();
        for (var i in o){
          var p = i;
          v.setAProp(_json_decode_val(p), _json_decode_val(o[i]));
        }
      }
      return v;
    case "string":
      // optional support of special Lingo data type 'symbol' unknown to JavaScript
      if (o.substr(0,7)=='__sym__') return symbol(o.substr(7));
      return o;
    default:
      return o;
  }
}

function _json_escape_string (str){
  var hash={"\\":"\\\\", "/":"\\/", "\n":"\\n", "\t":"\\t", "\r":"\\r", "\b":"\\b", "\f":"\\f", "\"":"\\\""};
  var patt = "["; for (i in hash) patt+=i;patt+="]";
  return str.replace(RegExp(patt, "g"), function(c){
    return hash[c]
  });
}
```


Lingo movie script "JSON":
```lingo
----------------------------------------
-- JSON encoder
-- Supported Lingo data types: VOID, integer, float, string, symbol, list, propList
-- @param {any} o
-- @return {string}
----------------------------------------
on json_encode (o)
  case ilk(o) of
    #void:
      return "null"
    #integer, #float:
      return string(o)
    #string:
      return QUOTE & _json_escape_string(o) & QUOTE
    #list:
      res = []
      repeat with v in o
        res.add(json_encode(v))
      end repeat
      return "[" & _cimplode(res) & "]"
    #propList:
      res = []
      cnt = count(o)
      repeat with i = 1 to cnt
        p = o.getPropAt(i)
        v = o[i]
        res.add( json_encode(p)&":"&json_encode(v) )
      end repeat
      return "{" & _cimplode(res) & "}"
    #symbol:
      -- optional support of special Lingo data type 'symbol' unknown to JavaScript
      return QUOTE &"__sym__"&_json_escape_string(string(o)) & QUOTE
    otherwise:
      put "ERROR: unsupported data type"
  end case
end

----------------------------------------
-- Implodes list into comma-separated string
-- @param {list} l
-- @return {string}
----------------------------------------
on _cimplode (l)
  str=""
  repeat with i=1 to l.count
    put l[i]&"," after str
  end repeat
  delete the last char of str
  return str
end
```


Usage:

```lingo
data_org = [\
  42,\
  3.14159,\
  [2, 4, #fooBar, "apples", "bananas", "cherries" ],\
  ["foo": 1, #bar: VOID, "Hello": "world!"],\
  VOID\
]

json_str = json_encode(data_org) -- valid according to JSONLint

data_decoded = json_decode(json_str)

put data_org
-- [42, 3.1416, [2, 4, #fooBar, "apples", "bananas", "cherries"], ["foo": 1, #bar: <Void>, "Hello": "world!"], <Void>]
put data_decoded
-- [42, 3.1416, [2, 4, #fooBar, "apples", "bananas", "cherries"], ["foo": 1, #bar: <Void>, "Hello": "world!"], <Void>]
```



## Lua


Using the [http://www.eharning.us/wiki/luajson/ luajson] library:


```lua
local json = require("json")

local json_data = [=[[
    42,
    3.14159,
    [ 2, 4, 8, 16, 32, 64, "apples", "bananas", "cherries" ],
    { "H": 1, "He": 2, "X": null, "Li": 3 },
    null,
    true,
    false
]]=]

print("Original JSON: " .. json_data)
local data = json.decode(json_data)
json.util.printValue(data, 'Lua')
print("JSON re-encoded: " .. json.encode(data))

local data = {
    42,
    3.14159,
    {
        2, 4, 8, 16, 32, 64,
        "apples",
        "bananas",
        "cherries"
    },
    {
        H = 1,
        He = 2,
        X = json.util.null(),
        Li = 3
    },
    json.util.null(),
    true,
    false
}

print("JSON from new Lua data: " .. json.encode(data))
```


Since in Lua <code>nil</code> signifies an ''undefined'' value, i.e. a variable or table entry with a <code>nil</code> value is undistinguishable from an undefined variable or non-existing table entry, a <code>null</code> value in JSON notation is decoded to a special function value, which ensures that it can be re-encoded properly to <code>null</code> again.
To manually insert a <code>null</code> value in the JSON output,
use the <code>json.util.null</code> function.

    Original JSON: [
        42,
        3.14159,
        [ 2, 4, 8, 16, 32, 64, "apples", "bananas", "cherries" ],
        { "H": 1, "He": 2, "X": null, "Li": 3 },
        null,
        true,
        false
    ]
    Lua= {
     1=42
     2=3.14159
     3= {
      1=2
      2=4
      3=8
      4=16
      5=32
      6=64
      7=[[apples]]
      8=[[bananas]]
      9=[[cherries]]
     4= {
      Li=3
      He=2
      H=1
      X=function: 0x8f6f00
     5=function: 0x8f6f00
     6=true
     7=false
    JSON re-encoded: [42,3.14159,[2,4,8,16,32,64,"apples","bananas","cherries"],{"Li":3,"He":2,"H":1,"X":null},null,true,false]
    JSON from new Lua data: [42,3.14159,[2,4,8,16,32,64,"apples","bananas","cherries"],{"Li":3,"He":2,"H":1,"X":null},null,true,false]


## M2000 Interpreter

We use a class written in M2000 for Json [[M2000 Interpreter Json Class]] in a module LIB1

```M2000 Interpreter

MODULE A {
      \\ Process data in json format

      \\ We can load from external file with Inline "libName"
      \\ or multiple files Inline "file1" && "file2"
      \\ but here we have the library in a module
      Inline Code Lib1
      \\ So now we make a Parser object (a group type in M2000)
      Parser=ParserClass()
      \\ We can display any function, module that is public and known list
      Modules ?
      \\ And this are all known variables (or and objects)
      List !
      Document json$
      \\ We can load from file
      \\ Load.Doc json$, "alfa.json"
      json$={{
            "alfa":-0.11221e+12,
            "array" : [
                  -0.67,
                  "alfa1",
                  [
                        10,
                        20
                  ],
                  "beta1",
                  1.21e12,
                  21.12145,
                  "ok"
            ],
            "delta": false, "epsilon" : true, "Null Value" : null
      }}
      Save.Doc json$, "json2.json"    \\ by default in Utf-8 with BOM
      \\ just show multiline text
      \\ Report display lines and stop after 3/4 of console height lines
      \\ just press a key or click mouse button
      Report json$
      \\ so now we get text to a new object
      alfa=Parser.Eval(json$)
      \\ check it
      Print Type$(alfa) ' it is a group
      Print "alfa.type$=";alfa.type$ \\ this is a read only property

      Report "as one line"
      Report Parser.Ser$(alfa, 0)

      Report "as multiline"
      Report Parser.Ser$(alfa, 1)

      Print "Using Print"
      Print Parser.ReadAnyString$(alfa)

      Print "Value for alfa, id alfa"
      Print Parser.ReadAnyString$(alfa,"alfa")
      Report "as multiline"
      Report Parser.Ser$(Parser.Eval(Parser.ReadAnyString$(alfa,"array", 2)), 1)
      \\ We get a copy of an array as a Group (a group which return an array)
      Alfa3=Parser.Eval(Parser.ReadAnyString$(alfa,"array", 2))
      \\ First value is for actual object, second value is a readonly property of this object
      Print type$(Alfa3), Alfa3.type$
      Dim B()
      \\ Now Alfa3 run Value part and pass a pointer of array
      \\  B() is an array and here take a pointer to Alfa3 array (as value of Alfa3)
      B()=Alfa3
      \\ each() make an iterator for B()
      N=each(B())
      While N {
            \\ Using B() we get values always. but if we have "object" or "array" then Print prints items **
            Print B(N^)
      }
      \\ Print show here nothing because if value is object then "print" just leave a column and continue to next one
      Print B()
      \\ we have to use Group() to get group not value of group (if any).
      \\ Group() works for "named" group, not for stored in an array or an inventory or a stack
      Print Parser.StringValue$(Group(Alfa3), 0)
      Print Parser.StringValue$(Group(Alfa3), 1)
      \\ Now we want to pass a new value
      \\ Interpreter want to match type of expression from left side to right side
      \\ Because Parser.StringValue$ is actual a Group (As property),
      \\ we have a second linked name:  Parser.StringValue
      \\ we have to use Parser.StringValue()
      \\ and all values must be groups, as those provided by Parser
      Parser.StringValue(Group(Alfa3), 1)=Parser.Numeric(1234)
      Print Parser.StringValue$(Group(Alfa3), 1)
      Print Parser.StringValue$(Group(Alfa), "array", 2, 0)
      \\ we have to use Parser.StringValue$()
      Parser.StringValue$(Group(Alfa), "array", 2, 0)=Parser.JString$("Changed to String")
      Print Parser.StringValue$(Group(Alfa), "array", 2,0)
      Try ok {
            Print Parser.StringValue$(Group(Alfa), "array", 2)
      }
      If Error or not ok Then Print Error$
      Parser.StringValue.Add = True
      Parser.StringValue$(Group(Alfa), "array", 2, 10)=Parser.JString$("Changed to String 2")
      Parser.StringValue(Group(Alfa), "Last value")=Parser.Boolean(true)
      Report "as multiline"
      Report Parser.Ser$(alfa3, 1)
      Report Parser.Ser$(alfa, 1)
      Parser.StringValue.Add = False
      Parser.StringValue.Del = True
      Parser.StringValue(Group(Alfa), "array", 0)=Parser.Null()
      Parser.StringValue(Group(Alfa), "delta")=Parser.Null()
      Parser.StringValue.Del = False
      For Parser {
            .StringValue(Group(Alfa), "array", 1,5)=.Arr((.Numeric(10), .Jstring$("ok 20"), .Boolean(true)))
      }
      Report Parser.Ser$(alfa, 1)

}
// call A
A

```



## Maple


```Maple>
 JSON:-ParseString("[{\"tree\": \"maple\", \"count\": 21}]");
       [table(["tree" = "maple", "count" = 21])]
> JSON:-ToString( [table(["tree" = "maple", "count" = 21])] );
       "[{\"count\": 21, \"tree\": \"maple\"}]"
```



## Mathematica


```Mathematica

data = ImportString["{ \"foo\": 1, \"bar\": [10, \"apples\"] }","JSON"]
ExportString[data, "JSON"]

```


=={{header|MATLAB}} / {{header|Octave}}==

```matlab>>
 jsondecode('{ "foo": 1, "bar": [10, "apples"] }')
ans =
  struct with fields:

    foo: 1
    bar: {2×1 cell}
>> jsonencode(ans)
ans =
{"foo":1,"bar":[10,"apples"]}
```

The toolbox [http://iso2mesh.sourceforge.net/cgi-bin/index.cgi?jsonlab/Download JSONlab] is doing a nice job to read (loadjson.m) and write (savejson.m) data in JSON format.


## NetRexx


### json.org Library

This uses a library provided by [http://www.json.org/java/index.html json.org] to serialize/deserialize JSON objects.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.util.List
import org.json.JSONObject
import org.json.JSONArray
import org.json.JSONTokener
import org.json.JSONException

/**
 * Using library from json.org
 *
 * @see http://www.json.org/java/index.html
 */
class RJson01 public

  properties private constant
    JSON_DWARFS = '' -
      '{\n' -
      '  "F1937_1" : {\n' -
      '    "title"  : "Snow White and the Seven Dwarfs",\n' -
      '    "year"   : 1937,\n' -
      '    "medium" : "film",\n' -
      '    "dwarfs" : [ "Grumpy", "Happy", "Sleepy", "Bashful", "Sneezy", "Dopey", "Doc" ]\n' -
      '  },\n' -
      '  "F2012_1" : {\n' -
      '    "title"  : "Mirror, Mirror",\n' -
      '    "year"   : 2012,\n' -
      '    "medium" : "film",\n' -
      '    "dwarfs" : [ "Grimm", "Butcher", "Wolf", "Napoleon", "Half Pint", "Grub", "Chuckles" ]\n' -
      '  },\n' -
      '}'

      /**
       * A bean that looks like the following JSON
       *
```txt

       * {
       *   "F2012_2" : {
       *     "title"  : "Snow White & the Huntsman",
       *     "year"   : 2012,
       *     "medium" : "film",
       *     "dwarfs" : [ "Beith", "Quert", "Muir", "Coll", "Duir", "Gus", "Gort", "Nion" ]
       *   }
       * }
       *
```

       */
      SAMPLE_BEAN = DwarfBean( -
        "F2012_2", -
        "Snow White & the Huntsman", -
        Long(2012), -
        "film", -
        Arrays.asList([String "Beith", "Quert", "Muir", "Coll", "Duir", "Gus", "Gort", "Nion"]) -
        )

  method main(args = String[]) public static
    say json2bean(JSON_DWARFS)
    say
    say bean2json(SAMPLE_BEAN)
    say
    return

method json2bean(dwarfs) public static returns List
    say "Make beans from this JSON string:"
    say dwarfs
    jsonBeans = ArrayList()
    do
      jd = JSONObject(JSONTokener(dwarfs))
      ns = JSONObject.getNames(jd)
      name = String
      loop name over ns
        dwarves = ArrayList()
        jn      = jd.getJSONObject(name)
        title   = jn.getString('title')
        year    = Long(jn.getLong('year'))
        medium  = jn.getString('medium')
        dwa     = jn.getJSONArray('dwarfs')
        loop di = 0 to dwa.length() - 1
          dwarves.add(dwa.getString(di))
          end di
        jb = DwarfBean(name, title, year, medium, dwarves)
        jsonBeans.add(jb.toString())
        end name
    catch ex = JSONException
      ex.printStackTrace()
    end
  return jsonBeans

method bean2json(sb = DwarfBean) public static returns String
  say "Make JSONObject from this bean:"
  say sb
  jsonString = String
  do
    jd = JSONObject(sb)
    jo = JSONObject()
    jo = jo.put(sb.keyGet(), jd)
    jsonString = jo.toString(2)
  catch ex = JSONException
    ex.printStackTrace()
  end
  return jsonString

--
### =======================================================================

class RJson01.DwarfBean public binary
  properties private
    key    = String -- not part of bean
  properties indirect
    title  = String
    year   = Long
    medium = String
    dwarfs = List

  method DwarfBean(key_ = String null, title_ = String null, year_ = Long null, medium_ = String null, dwarfs_ = List null) public
    keyPut(key_)
    setTitle(title_)
    setYear(year_)
    setMedium(medium_)
    setDwarfs(dwarfs_)
    return

  method keyPut(key_ = String) public
    key = key_
    return

  method keyGet() returns String
    return key

  method toString public returns String
    ts = StringBuilder()
    ts.append(String.format("%s@%08x ", [Object this.getClass().getSimpleName(), Integer(hashCode())]))
    ts.append('[')
    ts.append('key='String.valueOf(keyGet())', ')
    ts.append('title='String.valueOf(getTitle())', ')
    ts.append('year='String.valueOf(getYear())', ')
    ts.append('medium='String.valueOf(getMedium())', ')
    ts.append('dwarfs='String.valueOf(getDwarfs()))
    ts.append(']')
    return ts.toString()

```

```txt

Make beans from this JSON string:
 {
   "F1937_1" : {
     "title"  : "Snow White and the Seven Dwarfs",
     "year"   : 1937,
     "medium" : "film",
     "dwarfs" : [ "Grumpy", "Happy", "Sleepy", "Bashful", "Sneezy", "Dopey", "Doc" ]
   },
   "F2012_1" : {
     "title"  : "Mirror, Mirror",
     "year"   : 2012,
     "medium" : "film",
     "dwarfs" : [ "Grimm", "Butcher", "Wolf", "Napoleon", "Half Pint", "Grub", "Chuckles" ]
   },
 }
[DwarfBean@07377711 [key=F2012_1, title=Mirror, Mirror, year=2012, medium=film, dwarfs=[Grimm, Butcher, Wolf, Napoleon, Half Pint, Grub, Chuckles]], DwarfBean@19f16e6e [key=F1937_1, title=Snow White and the Seven Dwarfs, year=1937, medium=film, dwarfs=[Grumpy, Happy, Sleepy, Bashful, Sneezy, Dopey, Doc]]]

Make JSONObject from this bean:
DwarfBean@39890510 [key=F2012_2, title=Snow White & the Huntsman, year=2012, medium=film, dwarfs=[Beith, Quert, Muir, Coll, Duir, Gus, Gort, Nion]]
{"F2012_2": {
  "title": "Snow White & the Huntsman",
  "dwarfs": [
    "Beith",
    "Quert",
    "Muir",
    "Coll",
    "Duir",
    "Gus",
    "Gort",
    "Nion"
  ],
  "year": 2012,
  "medium": "film"
}}

```



### Google gson Library

This uses [http://code.google.com/p/google-gson/ Gson], a library to convert JSON to Java objects and vice-versa.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import com.google.gson.
import java.util.List

/**
 * Using google-gson library
 *
 * @see https://code.google.com/p/google-gson/
 */
class RJson02 public

  properties private constant
    JSON_DWARFS = '' -
      '{\n' -
      '  "title" : "Snow White and the Seven Dwarfs",\n' -
      '  "year"  : 1937,\n' -
      '  "medium": "film",\n' -
      '  "dwarfs": [ "Grumpy", "Happy", "Sleepy", "Bashful", "Sneezy", "Dopey", "Doc" ]\n' -
      '}'

      /**
       * A bean that looks like the following JSON
       *
```txt

       * {
       *   "title"  : "Snow White & the Huntsman",
       *   "year"   : 2012,
       *   "medium" : "film",
       *   "dwarfs" : [ "Beith", "Quert", "Muir", "Coll", "Duir", "Gus", "Gort", "Nion" ]
       * }
       *
```

       */
      SAMPLE_BEAN = RJSON02.DwarfBean( -
        /*"F2012_2",*/ -
        "Snow White and the Huntsman", -
        Long(2012), -
        "film", -
        Arrays.asList([String "Beith", "Quert", "Muir", "Coll", "Duir", "Gus", "Gort", "Nion"]) -
        )

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static
    gsonObj = GsonBuilder().setPrettyPrinting().create()
    jsonBean = RJson02.DwarfBean gsonObj.fromJson(JSON_DWARFS, RJson02.DwarfBean.class)
    say JSON_DWARFS
    say jsonBean.toString()
    say

    json = gsonObj.toJson(SAMPLE_BEAN);
    say json
    say SAMPLE_BEAN.toString()
    say

    return

--
### =======================================================================

class RJson02.DwarfBean public binary
  properties indirect
    title  = String
    year   = Long
    medium = String
    dwarfs = List

  method DwarfBean(title_ = String null, year_ = Long null, medium_ = String null, dwarfs_ = List null) public
    setTitle(title_)
    setYear(year_)
    setMedium(medium_)
    setDwarfs(dwarfs_)
    return

  method toString public returns String
    ts = StringBuilder()
    ts.append(String.format("%s@%08x ", [Object this.getClass().getSimpleName(), Integer(hashCode())]))
    ts.append('[')
    ts.append('title='String.valueOf(getTitle())', ')
    ts.append('year='String.valueOf(getYear())', ')
    ts.append('medium='String.valueOf(getMedium())', ')
    ts.append('dwarfs='String.valueOf(getDwarfs()))
    ts.append(']')
    return ts.toString()

```

```txt

 {
   "title" : "Snow White and the Seven Dwarfs",
   "year"  : 1937,
   "medium": "film",
   "dwarfs": [ "Grumpy", "Happy", "Sleepy", "Bashful", "Sneezy", "Dopey", "Doc" ]
 }
DwarfBean@63f5e4b6 [title=Snow White and the Seven Dwarfs, year=1937, medium=film, dwarfs=[Grumpy, Happy, Sleepy, Bashful, Sneezy, Dopey, Doc]]

{
  "title": "Snow White and the Huntsman",
  "year": 2012,
  "medium": "film",
  "dwarfs": [
    "Beith",
    "Quert",
    "Muir",
    "Coll",
    "Duir",
    "Gus",
    "Gort",
    "Nion"
  ]
}
DwarfBean@6d63de20 [title=Snow White and the Huntsman, year=2012, medium=film, dwarfs=[Beith, Quert, Muir, Coll, Duir, Gus, Gort, Nion]]

```



## Nim


```nim
import json

var data = parseJson("""{ "foo": 1, "bar": [10, "apples"] }""")
echo data["foo"]
echo data["bar"]

var js = %* [{"name": "John", "age": 30}, {"name": "Susan", "age": 31}]
echo js
```

```txt
1
[ 10,  "apples"]
[ { "name": "John",  "age": 30 },  { "name": "Susan",  "age": 31 }]
```



## Objeck


```objeck

use Struct;
use JSON;

bundle Default {
  class Json {
    function : Main(args : String[]) ~ Nil {
      parser := JSONParser->New("{ \"foo\": 1, \"bar\": [10, \"apples\"] }");
      root := parser->Parse();
      if(root <> Nil) {
        root->ToString()->PrintLine();
      };
    }
  }
}
```


=={{header|Objective-C}}==
```objc
NSString *jsonString = @"{ \"foo\": 1, \"bar\": [10, \"apples\"] }";
id obj = [NSJSONSerialization
    JSONObjectWithData: [jsonString dataUsingEncoding: NSUTF8StringEncoding]
               options: 0
                 error: NULL];
NSLog(@"%@", obj);

NSDictionary *dict = @{ @"blue": @[@1, @2], @"ocean": @"water"};
NSData *jsonData = [NSJSONSerialization dataWithJSONObject: dict
                                                   options: 0
                                                     error: NULL];
NSString *jsonString2 = [[NSString alloc] initWithData: jsonData
                                              encoding: NSUTF8StringEncoding];
NSLog(@"%@", jsonString2);

```



## OCaml


=== Using json-wheel/json-static libs ===

```ocaml
type json item =
  < name    "Name": string;
    kingdom "Kingdom": string;
    phylum  "Phylum": string;
    class_  "Class": string;
    order   "Order": string;
    family  "Family": string;
    tribe   "Tribe": string
  >

let str = "
  {
    \"Name\":    \"camel\",
    \"Kingdom\": \"Animalia\",
    \"Phylum\":  \"Chordata\",
    \"Class\":   \"Mammalia\",
    \"Order\":   \"Artiodactyla\",
    \"Family\":  \"Camelidae\",
    \"Tribe\":   \"Camelini\"
  }"

let () =
  let j = Json_io.json_of_string str in
  print_endline (Json_io.string_of_json j);
```


compile with:
 ocamlfind opt -o j.opt j.ml -linkpkg -package json-static -syntax camlp4o


###  Using yojson lib


```ocaml
open Yojson.Basic.Util

let s = "
{ \"name\": \"John Doe\",
  \"pages\": [
    { \"id\": 1,
      \"title\": \"The Art of Flipping Coins\",
      \"url\": \"http://example.com/398eb027/1\"
    },
    { \"id\": 2, \"deleted\": true },
    { \"id\": 3,
      \"title\": \"Artichoke Salad\",
      \"url\": \"http://example.com/398eb027/3\"
    },
    { \"id\": 4,
      \"title\": \"Flying Bananas\",
      \"url\": \"http://example.com/398eb027/4\"
    }
  ]
}"

let extract_titles json =
  [json]
    |> filter_member "pages"
    |> flatten
    |> filter_member "title"
    |> filter_string

let () =
  let json = Yojson.Basic.from_string s in
  List.iter print_endline (extract_titles json)
```


Compile and run:


```txt

$ ocamlfind ocamlopt -o filtering filtering.ml -package yojson -linkpkg
$ ./filtering
The Art of Flipping Coins
Artichoke Salad
Flying Bananas

```



## Oforth


In oforth, Json objects are builtin and can be interpreted natively


A String can be converted to Json object using #perform

A Json object can be converted to a string using #asString


```Oforth>
{"parents":["Otmar Gutmann", "Silvio Mazzola"], "name":"Pingu", "born":1986} .s
[1] (Json) {"parents" : ["Otmar Gutmann", "Silvio Mazzola"], "name" : "Pingu", "born" : 1986 }
ok
>asString .s
[1] (String) {"parents" : ["Otmar Gutmann", "Silvio Mazzola"], "name" : "Pingu", "born" :1986 }
ok
>perform .s
[1] (Json) {"parents" : ["Otmar Gutmann", "Silvio Mazzola"], "name" : "Pingu", "born" : 1986 }
ok
>
```



## OpenEdge/Progress

The WRITE-JSON and READ-JSON methods were introduced in Progress OpenEdge 10.2B.

```progress
/* using a longchar to read and write to, can also be file, memptr, stream */
DEFINE VARIABLE lcjson AS LONGCHAR NO-UNDO.

/* temp-table defines object, can also be dataset */
DEFINE TEMP-TABLE example
   FIELD blue  AS INTEGER EXTENT 2
   FIELD ocean AS CHARACTER
   .
CREATE example.
ASSIGN
   example.blue [1]  =  1
   example.blue [2]  =  2
   example.ocean     =  "water"
   .
/* write-json to put result in lcjson, true indicates formatted */
TEMP-TABLE example:DEFAULT-BUFFER-HANDLE:WRITE-JSON( "LONGCHAR", lcjson, TRUE ).

/* display result */
MESSAGE
   STRING( lcjson )
VIEW-AS ALERT-BOX.

/* empty results */
EMPTY TEMP-TABLE example.

/* read-json to get result from lcjson */
TEMP-TABLE example:DEFAULT-BUFFER-HANDLE:READ-JSON( "LONGCHAR", lcjson ).

FIND example.
/* display results */
MESSAGE
   example.blue [1] example.blue [2] SKIP
   example.ocean
VIEW-AS ALERT-BOX.
```


{{out}} write-json:


```txt

---------------------------
Message
---------------------------
{"example": [
  {
    "blue": [
      1,
      2
    ],
    "ocean": "water"
  }
]}
---------------------------
OK
---------------------------

```


{{out}} read-json:


```txt

---------------------------
Message
---------------------------
1 2
water
---------------------------
OK
---------------------------

```



## Oz

Using the [http://code.google.com/p/oz-code/downloads/list google.com/oz-code] JSON library:

```oz
declare
  [JSON] = {Module.link ['JSON.ozf']}

  {System.show {JSON.decode "{ \"foo\": 1, \"bar\": [10, \"apples\"] }"}}

  Sample = object(blue:array(1 2) ocean:"water")
  {System.showInfo {JSON.encode Sample}}
```


```txt
object(bar:array(10 [97 112 112 108 101 115]) foo:1)
{"blue":[1,2],"ocean":"water"}
```



## Perl

```perl
use JSON;

my $data = decode_json('{ "foo": 1, "bar": [10, "apples"] }');

my $sample = { blue => [1,2], ocean => "water" };
my $json_string = encode_json($sample);
```



## Perl 6


Using [http://github.com/moritz/json/ JSON::Tiny]


```perl6
use JSON::Tiny;

my $data = from-json('{ "foo": 1, "bar": [10, "apples"] }');

my $sample = { blue => [1,2], ocean => "water" };
my $json_string = to-json($sample);
```



## Phix

The distribution now contains a simple json module

```Phix
--
-- demo\rosetta\JSON.exw
--
### ===============

--
include builtins/json.e

puts(1,"roundtrip (10 examples):\n")
sequence json_strings = {"{\"this\":\"that\",\"age\":{\"this\":\"that\",\"age\":29}}",
                         "1",
                         "\"hello\"",
                         "null",
                         "[12]",
                         "[null,12]",
                         "[]",
                         "{\"this\":\"that\",\"age\":29}",
                         "{}",
                         "[null,[null,12]]"}

for i=1 to length(json_strings) do
    string s = json_strings[i]
    puts(1,s&"\n")
    object json_object = parse_json(s)
    puts(1,print_json("",json_object,true)&"\n")
    if not equal(print_json("",json_object,true),s) then ?9/0 end if
end for
```

```txt

roundtrip (10 examples):
{"this":"that","age":{"this":"that","age":29}}
{"this":"that","age":{"this":"that","age":29}}
1
1
"hello"
"hello"
null
null
[12]
[12]
[null,12]
[null,12]
[]
[]
{"this":"that","age":29}
{"this":"that","age":29}
{}
{}
[null,[null,12]]
[null,[null,12]]

```



## PHP


```php
<?php
$data = json_decode('{ "foo": 1, "bar": [10, "apples"] }'); // dictionaries will be returned as objects
$data2 = json_decode('{ "foo": 1, "bar": [10, "apples"] }', true); // dictionaries will be returned as arrays

$sample = array( "blue" => array(1,2), "ocean" => "water" );
$json_string = json_encode($sample);
?>
```



## PicoLisp

PicoLisp has no JSON library, but it is easy enough to write one. The following supports only fixpoint numbers (no floating point, as it doesn't exist in PicoLisp). Arrays and objects are both mapped to lists.

```PicoLisp
(de checkJson (X Item)
   (unless (= X Item)
      (quit "Bad JSON" Item) ) )

(de readJson ()
   (case (read "_")
      ("{"
         (make
            (for (X (readJson)  (not (= "}" X))  (readJson))
               (checkJson ":" (readJson))
               (link (cons X (readJson)))
               (T (= "}" (setq X (readJson))))
               (checkJson "," X) ) ) )
      ("["
         (make
            (link T)  # Array marker
            (for (X (readJson)  (not (= "]" X))  (readJson))
               (link X)
               (T (= "]" (setq X (readJson))))
               (checkJson "," X) ) ) )
      (T
         (let X @
            (cond
               ((pair X) (pack X))
               ((and (= "-" X) (format (peek)))
                  (- (read)) )
               (T X) ) ) ) ) )

(de printJson (Item)  # For simplicity, without indentation
   (cond
      ((atom Item) (if Item (print @) (prin "{}")))
      ((=T (car Item))
         (prin "[")
         (map
            '((X)
               (printJson (car X))
               (and (cdr X) (prin ", ")) )
            (cdr Item) )
         (prin "]") )
      (T
         (prin "{")
         (map
            '((X)
               (print (caar X))
               (prin ": ")
               (printJson (cdar X))
               (and (cdr X) (prin ", ")) )
            Item )
         (prin "}") ) ) )
```

This reads/prints JSON from/to files, pipes, sockets etc. To read from a string, a pipe can be used:

```txt
: (pipe (prinl "{ \"foo\": 1, \"bar\": [10, \"apples\"] }")
   (readJson) )
-> (("foo" . 1) ("bar" T 10 "apples"))

: (printJson
   (quote
      ("name" . "Smith")
      ("age" . 25)
      ("address"
         ("street" . "21 2nd Street")
         ("city" . "New York")
         ("state" . "NY")
         ("zip" . "10021") )
      ("phone" T "212 555-1234" "646 555-4567") ) )
{"name": "Smith", "age": 25, ... {"street": ... "phone": ["212 555-1234", ...
```



## Pike


```pike
int main() {
	// Decoding
	string json = "{\"cake\":[\"desu\",1,2.3],\"foo\":1}";
	write("%O\n", Standards.JSON.decode(json));

	// Encoding
	mapping m = ([
		"foo": ({ 1, 2, 3 }),
		"bar": "hello"
	]);

	write("%s\n", Standards.JSON.encode(m));
}
```



```txt
([ /* 2 elements */
  "cake": ({ /* 3 elements */
        "desu",
        1,
        2.3
    }),
  "foo": 1
])
{"foo":[1,2,3],"bar":"hello"}

```



## PowerShell


```PowerShell

# JSON input is being stored in ordered hashtable.
# Ordered hashtable is available in PowerShell v3 and higher.
[ordered]@{ "foo"= 1; "bar"= 10, "apples" } | ConvertTo-Json

# ConvertFrom-Json converts a JSON-formatted string to a custom object.
# If you use the Invoke-RestMethod cmdlet there is not need for the ConvertFrom-Json cmdlet
Invoke-WebRequest -Uri "http://date.jsontest.com" | ConvertFrom-Json

```

```PowerShell

{
    "foo":  1,
    "bar":  [
                10,
                "apples"
            ]
}

time                                                                  milliseconds_since_epoch date
----                                                                  ------------------------ ----
12:25:25 PM                                                                      1414326325923 10-26-2014

```



## Prolog


Using SWI-Prolog 7's library(http/json), and the new dict datatype, there is nearly transparent handling of JSON objects. All of the serialization and parsing in the following code is accomplished with two predicates. The rest of the code is for the sake of example.


```Prolog
:- use_module([ library(http/json),
                library(func) ]).

test_json('{"widget": { "debug": "on", "window": { "title": "Sample Konfabulator Widget", "name": "main_window", "width": 500, "height": 500 }, "image": { "src": "Images/Sun.png", "name": "sun1", "hOffset": 250, "vOffset": 250, "alignment": "center" }, "text": { "data": "Click Here", "size": 36, "style": "bold", "name": "text1", "hOffset": 250, "vOffset": 100, "alignment": "center", "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;" }}}').

reading_JSON_term :-
    atom_json_dict(test_json(~), Dict, []), %% This accomplishes reading in the JSON data
    writeln( 'JSON as Prolog dict: ~w~n'
           $ Dict),
    writeln( 'Access field "widget.text.data": ~s~n'
           $ Dict.widget.text.data),
    writeln( 'Alter field "widget": ~w~n'
           $ Dict.put(widget, "Altered")).

searalize_a_JSON_term :-
    Dict = _{book:_{title:"To Mock a Mocking Bird",
                    author:_{first_name:"Ramond",
                             last_name:"Smullyan"},
                    publisher:"Alfred A. Knopf",
                    year:1985
                   }},
    json_write(current_output, Dict). %% This accomplishes serializing the JSON object.
```


{{out}} from these two example predicates:

```Prolog
?- reading_JSON_term.
JSON as Prolog dict: _G5217{widget:_G5207{debug:on,image:_G5123{alignment:center,hOffset:250,name:sun1,src:Images/Sun.png,vOffset:250},text:_G5189{alignment:center,data:Click Here,hOffset:250,name:text1,onMouseUp:sun1.opacity = (sun1.opacity / 100) * 90;,size:36,style:bold,vOffset:100},window:_G5077{height:500,name:main_window,title:Sample Konfabulator Widget,width:500}}}

Access field "widget.text.data": Click Here

Alter field "widget": _G5217{widget:Altered}

true.

?- searalize_a_JSON_term.
{
  "book": {
    "author": {"first_name":"Ramond", "last_name":"Smullyan"},
    "publisher":"Alfred A. Knopf",
    "title":"To Mock a Mocking Bird",
    "year":1985
  }
}
true.
```



## PureBasic


```purebasic
OpenConsole()
If CreateJSON(1)
  PB_Team_Members=SetJSONObject(JSONValue(1))
  SetJSONString(AddJSONMember(PB_Team_Members,"PB_Team_Member_1"),"Frederic Laboureur")
  SetJSONString(AddJSONMember(PB_Team_Members,"PB_Team_Member_2"),"Andre Beer")
  SetJSONString(AddJSONMember(PB_Team_Members,"PB_Team_Member_3"),"Timo Harter")
EndIf

If CreateJSON(2)
  Former_Team_Members=SetJSONArray(JSONValue(2))
  SetJSONString(AddJSONElement(Former_Team_Members),"Richard Andersson")
  SetJSONString(AddJSONElement(Former_Team_Members),"Benny Sels")
  SetJSONString(AddJSONElement(Former_Team_Members),"Danilo Krahn")
EndIf

PrintN("PureBasic - Team Members:")
PrintN(ComposeJSON(1,#PB_JSON_PrettyPrint)+#CRLF$)
PrintN("PureBasic - Former Team Members:")
PrintN(ComposeJSON(2,#PB_JSON_PrettyPrint)+#CRLF$)

#DL=Chr(34)
PB_Special_thanks$="[ " +#DL+"Gary Willoughby"+#DL+", " +#DL+"Mark James"+#DL+", " +#DL+"Neil Hodgson"+#DL+" ]"
NewList otherpersons.s()

If ParseJSON(3,PB_Special_thanks$)
  ExtractJSONList(JSONValue(3),otherpersons())
  PrintN("Pure Basic - and others:")
  ForEach otherpersons() : PrintN(otherpersons()) : Next
Else
  PrintN(JSONErrorMessage() + " : " + Str(JSONErrorPosition()))
  PrintN(Left(PB_Special_thanks$,JSONErrorPosition()))
  PrintN(Mid(PB_Special_thanks$,JSONErrorPosition()+1))
EndIf
Input()
```

```txt
PureBasic - Team Members:
{
  "PB_Team_Member_1": "Frederic Laboureur",
  "PB_Team_Member_2": "Andre Beer",
  "PB_Team_Member_3": "Timo Harter"
}

PureBasic - Former Team Members:
[
  "Richard Andersson",
  "Benny Sels",
  "Danilo Krahn"
]

Pure Basic - and others:
Gary Willoughby
Mark James
Neil Hodgson
```



## Python

```Python>>>
 import json
>>> data = json.loads('{ "foo": 1, "bar": [10, "apples"] }')
>>> sample = { "blue": [1,2], "ocean": "water" }
>>> json_string = json.dumps(sample)
>>> json_string
'{"blue": [1, 2], "ocean": "water"}'
>>> sample
{'blue': [1, 2], 'ocean': 'water'}
>>> data
{'foo': 1, 'bar': [10, 'apples']}
```


Because most of JSON is valid Python syntax (except "true", "false", and "null", and a few obscure escape sequences), it is also possible (but not recommended) to parse JSON using eval():

```python>>>
 true = True; false = False; null = None
>>> data = eval('{ "foo": 1, "bar": [10, "apples"] }')
>>> data
{'foo': 1, 'bar': [10, 'apples']}
```



## R


```R
library(rjson)
data <- fromJSON('{ "foo": 1, "bar": [10, "apples"] }')
data
```


```txt
data
$foo
[1] 1

$bar
$bar[[1]]
[1] 10

$bar[[2]]
[1] "apples"

```


```R
cat(toJSON(data))
```


```txt
{"foo":1,"bar":[10,"apples"]}
```



## Racket


```Racket

#lang racket

(require json)

(string->jsexpr
 "{\"foo\":[1,2,3],\"bar\":null,\"baz\":\"blah\"}")

(write-json '(1 2 "three" #hash((x . 1) (y . 2) (z . 3))))

```



## REBOL

Using [http://www.json.org/json.r json.org/json.r]


```rebol
json-str: {{"menu": {
    "id": "file",
    "string": "File:",
    "number": -3,
    "boolean": true,
    "boolean2": false,
    "null": null,
    "array": [1, 0.13, null, true, false, "\t\r\n"],
    "empty-string": ""
  }
}}

res: json-to-rebol json-str
js: rebol-to-json res

```


json-to-rebol Result:


```txt

make object! [
    menu: make object! [
        id: "file"
        string: "File:"
        number: -3
        boolean: true
        boolean2: false
        null: none
        array: [1 0.13 none true false "^-^M^/"]
        empty-string: ""
    ]
]

```



rebol-to-json Result:


```txt

{
    "menu": {
        "id": "file",
        "string": "File:",
        "number": -3,
        "boolean": true,
        "boolean2": false,
        "null": null,
        "array": [1, 0.13, null, true, false, "\t\r\n"],
        "empty-string": ""
    }
}

```



## Ruby


```ruby
require 'json'

ruby_obj = JSON.parse('{"blue": [1, 2], "ocean": "water"}')
puts ruby_obj

ruby_obj["ocean"] = { "water" => ["fishy", "salty"] }
puts JSON.generate(ruby_obj)
puts JSON.pretty_generate(ruby_obj)
```


```txt

{"blue"=>[1, 2], "ocean"=>"water"}
{"blue":[1,2],"ocean":{"water":["fishy","salty"]}}
{
  "blue": [
    1,
    2
  ],
  "ocean": {
    "water": [
      "fishy",
      "salty"
    ]
  }
}

```



## Rust


```rust
extern crate rustc_serialize;

use rustc_serialize::json;

#[derive(RustcDecodable, RustcEncodable)]
struct Penguin {
    name : String,
    born : i16
}
fn main() {
    let pengu = Penguin { name : "pengu".to_string(), born : 1999 };
    println!("{}", json::encode(&pengu).unwrap());
    let pingu : Penguin = json::decode(r##"{"name":"pingu","born":2001}"##).unwrap();
    assert_eq!(&pingu.name, "pingu");
    assert_eq!(pingu.born, 2001);
}
```

```txt
{"name":"pengu","born":1999}
```



## Scala

Using the builtin parsing lib (debatably slower than third-party libs such as lift-json from Liftweb).


```scala>scala
 import scala.util.parsing.json.{JSON, JSONObject}
import scala.util.parsing.json.{JSON, JSONObject}

scala> JSON.parseFull("""{"foo": "bar"}""")
res0: Option[Any] = Some(Map(foo -> bar))

scala> JSONObject(Map("foo" -> "bar")).toString()
res1: String = {"foo" : "bar"}

```



## Scheme

Using the [http://api.call-cc.org/doc/json json] egg:
```scheme

(use json)
(define object-example
  (with-input-from-string "{\"foo\": \"bar\", \"baz\": [1, 2, 3]}"
                          json-read))
(pp object-example)
; this prints #(("foo" . "bar") ("baz" 1 2 3))

(json-write #([foo . bar]
              [baz   1 2 3]
              [qux . #((rosetta . code))]))
; this writes the following:
; {"foo": "bar", "baz": [1, 2, 3], "qux": {"foo": "bar"}}

```



## Sidef


```ruby
var json = require('JSON').new;
var data = json.decode('{"blue": [1, 2], "ocean": "water"}');
say data;
data{:ocean} = Hash.new(water => %w[fishy salty]);
say json.encode(data);
```

```txt
Hash.new(
    'blue' => [1, 2],
    'ocean' => 'water'
    )
{"blue":[1,2],"ocean":{"water":["fishy","salty"]}}
```



## Smalltalk

Use the NeoJSON library: [http://smalltalkhub.com/#!/~SvenVanCaekenberghe/Neo NeoJSON]

```smalltalk

NeoJSONReader fromString: '{ "foo": 1, "bar": [10, "apples"] }'.

```


```txt

a Dictionary('bar'->#(10 'apples') 'foo'->1 )

```


## Swift


```Swift
import Foundation

let jsonString = "{ \"foo\": 1, \"bar\": [10, \"apples\"] }"
if let jsonData = jsonString.data(using: .utf8) {
	if let jsonObject: Any = try? JSONSerialization.jsonObject(with: jsonData, options: .allowFragments) {
		print("Dictionary: \(jsonObject)")
	}
}

let obj = [
	"foo": [1, "Orange"],
	"bar": 1
] as [String : Any]

if let objData = try? JSONSerialization.data(withJSONObject: obj, options: .prettyPrinted) {
	if let jsonString2 = String(data: objData, encoding: .utf8) {
		print("JSON: \(jsonString2)")
	}
}
```

```txt

Dictionary: {
    bar =     (
        10,
        apples
    );
    foo = 1;
}
JSON: {
  "foo" : [
    1,
    "Orange"
  ],
  "bar" : 1
}

```



## Tailspin

A JSON parser and printer can fairly easily be created

```tailspin

// Not all JSON object member keys can be represented, a fallback would need to be implemented
// Currently Tailspin only supports integers so for now we leave numbers as strings, as we do for true, false and null

templates hexToInt
  templates hexDigit
    <'0'> 0! <'1'> 1! <'2'> 2! <'3'> 3! <'4'> 4! <'5'> 5! <'6'> 6! <'7'> 7! <'8'> 8! <'9'> 9!
    <'[Aa]'> 10! <'[Bb]'> 11! <'[Cc]'> 12! <'[Dd]'> 13! <'[Ee]'> 14! <'[Ff]'> 15!
  end hexDigit
  @: 0;
  $... -> hexDigit -> @: $@ * 16 + $;
  $@ !
end hexToInt

composer jsonParser
  <value>
  rule value: (<WS>?) <string|number|object|array|true|false|null> (<WS>?)

  rule string: (<'"'>) <chars>  (<'"'>)
  rule chars: [ <quote|backslash|slash|backspace|formfeed|linefeed|return|tab|unicode|'[^"]'>* ] -> '$...;'
  rule quote: <'\\"'> -> '"'
  rule backslash: <'\\\\'> -> '\'
  rule slash: <'\\/'> -> '/'
  rule backspace: <'\\b'> -> '$#8;'
  rule formfeed: <'\\f'> -> '$#12;'
  rule linefeed: <'\\n'> -> '$#10;'
  rule return: <'\\r'> -> '$#13;'
  rule tab: <'\\t'> -> '$#9;'
  rule unicode: (<'\\u'>) <'[0-9A-Fa-f]{4}'> -> hexToInt -> '$#$;'

  rule number: <'-?(0|[1-9][0-9]*)(\.[0-9]+)?((e|E)(\+|-)?[0-9]+)?'> // TODO: represent this other than string

  rule object: (<'\{'> <WS>?) { <keyValues>? } (<'}'>)
  rule keyValues: <keyValue> <followingKeyValue>*
  rule keyValue: <string>: (<WS>? <':'>) <value>
  rule followingKeyValue: (<','> <WS>?) <keyValue>

  rule array: (<'\['>) [ <arrayValues>? ] (<']'>)
  rule arrayValues: <value> <followingArrayValue>*
  rule followingArrayValue: (<','>) <value>

  rule true: <'true'> // TODO: represent this other than string
  rule false: <'false'> // TODO: represent this other than string
  rule null: <'null'> // TODO: represent this other than string
end jsonParser

templates printJson
  templates printKeyValue
    '$::key -> printJson;: $::value -> printJson;' !
  end printKeyValue
  templates encodeChars
    <'"'> '\"' !
    <'\\'> '\\' !
    <'/'> '\/' !
    <'$#8;'> '\b' !
    <'$#12;'> '\f' !
    <'$#10;'> '\n' !
    <'$#13;'> '\r' !
    <'$#9;'> '\t' !
    <> $ !
  end encodeChars
  $ -> #
  <[]>
    '[$(1) -> printJson;$(2..-1)... -> ', $ -> printJson;';]' !
  <{}>
    [ $... ] -> '{$(1) -> printKeyValue;$(2..-1)... -> ', $ -> printKeyValue;';}' !
  <..0|0..>
    '$;'!
  <'.*'>
    [ $... -> encodeChars ] -> '"$...;"' !
  <> 'WTF!' !
  // Other types do not yet exist in Tailspin
end printJson

```

Then, called as below:

```tailspin

'{ "foo": 1, "bar": [10, "apples"] }' -> jsonParser -> '$.bar(2);
' -> !OUT::write

{ blue: [1,2], ocean: 'water' } -> printJson -> '$;
' -> !OUT::write

```

```txt

apples
{"blue": [1, 2], "ocean": "water"}

```



## Tcl

For parsing JSON, {{tcllib|json}} provides the capability (see [http://wiki.tcl.tk/13419 the Tcler's Wiki page on it] for more discussion):

```tcl
package require json
set sample {{ "foo": 1, "bar": [10, "apples"] }}

set parsed [json::json2dict $sample]
puts $parsed
```

```txt
foo 1 bar {10 apples}
```

However, that package is very weak in its generation of JSON because Tcl's official type system is substantially different to that envisaged by JSON. It's possible to work around this though the use of Tcl 8.6, as this next example shows:


```tcl
package require Tcl 8.6
package require json::write

proc tcl2json value {
    # Guess the type of the value; deep *UNSUPPORTED* magic!
    regexp {^value is a (.*?) with a refcount} \
	[::tcl::unsupported::representation $value] -> type

    switch $type {
	string {
	    return [json::write string $value]
	}
	dict {
	    return [json::write object {*}[
		dict map {k v} $value {tcl2json $v}]]
	}
	list {
	    return [json::write array {*}[lmap v $value {tcl2json $v}]]
	}
	int - double {
	    return [expr {$value}]
	}
	booleanString {
	    return [expr {$value ? "true" : "false"}]
	}
	default {
	    # Some other type; do some guessing...
	    if {$value eq "null"} {
		# Tcl has *no* null value at all; empty strings are semantically
		# different and absent variables aren't values. So cheat!
		return $value
	    } elseif {[string is integer -strict $value]} {
		return [expr {$value}]
	    } elseif {[string is double -strict $value]} {
		return [expr {$value}]
	    } elseif {[string is boolean -strict $value]} {
		return [expr {$value ? "true" : "false"}]
	    }
	    return [json::write string $value]
	}
    }
}
```

Sample code (note that the value is built with <code>dict create</code> and <code>list</code> so that there is some auxiliary type hints available, which the above procedure can read):

```tcl
set d [dict create blue [list 1 2] ocean water]
puts [tcl2json $d]
```

```txt

{
    "blue"  : [1,2],
    "ocean" : "water"
}

```

Note that this is capable of correctly handling the round-trip of values parsed from the <code>json</code> package described above.


## TXR



### Parsing


The following implements the parsing half of the task. It is a parser closely based on the JSON grammar [[http://www.json.org/fatfree.html]].

It is implemented with recursive horizontal pattern matching functions, and so basically the definition resembles a grammar. Horizontal functions are a new feature in TXR, and basically allow the language to easily specify LL grammars with indefinite lookahead, not restricted to regular languages (thanks to TXR's backtracking).  The numerous occurences of @\ in the code are line continuations. Horizontal functions must be written on one logical line. @\ eats the whitespace at the start of the next physical line, to allow indentation.

The parser translates to a nested list structure in which the types are labeled with the strings "O", "A", "N", "S" and "K". (Object, array, number, string, and keyword).

The largest grammar rule handles JSON string literals. The strategy is to generate a HTML string and then filter from HTML using the <code>:from_html</code> filter in TXR. For instance \uABCD is translated to <code>&amp;#xABCD;</code> and then the filter will produce the proper Unicode character. Similarly \" is translated to <code>&amp;quot;</code> and \n is translated to &#10; etc.

A little liberty is taken: the useless commas in JSON are treated as optional.

Superfluous terminating commas (not generated by the JSON grammar but accepted by some other parsers) are not allowed by this parser.


```txr
@(define value (v))@\
  @(cases)@\
    @(string v)@(or)@(num v)@(or)@(object v)@(or)@\
    @(keyword v)@(or)@(array v)@\
  @(end)@\
@(end)
@(define ws)@/[\n\t ]*/@(end)
@(define string (g))@\
  @(local s hex)@\
  @(ws)@\
  "@(coll :gap 0 :vars (s))@\
     @(cases)@\
       \"@(bind s "&quot;")@(or)@\
       \\@(bind s "\\\\")@(or)@\
       \/@(bind s "\\/")@(or)@\
       \b@(bind s "&#8;")@(or)@\
       \f@(bind s "&#12;")@(or)@\
       \n@(bind s "&#10;")@(or)@\
       \r@(bind s "&#13;")@(or)@\
       \t@(bind s "&#9;")@(or)@\
       \u@{hex /[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]/}@\
         @(bind s `&#x@hex;`)@(or)@\
       @{s /[^"\\]*/}@(filter :to_html s)@\
     @(end)@\
     @(until)"@\
   @(end)"@\
  @(ws)@\
  @(cat s "")@\
  @(filter :from_html s)@\
  @(bind g ("S" s))@\
@(end)
@(define num (v))@\
  @(local n)@\
  @(ws)@{n /-?[0-9]+((\.[0-9]+)?([Ee][+\-]?[0-9]+)?)?/}@(ws)@\
  @(bind v ("N" n))@\
@(end)
@(define keyword (v))@\
  @(local k)@\
  @(all)@(ws)@{k /true|false|null/}@(trailer)@/[^A-Za-z0-9_]/@(end)@(ws)@\
  @(bind v ("K" k))@\
@(end)
@(define object (v))@\
  @(local p e pair)@\
  @(ws){@(ws)@(coll :gap 0 :vars (pair))@\
                @(string p):@(value e)@/,?/@\
                @(bind pair (p e))@\
                @(until)}@\
             @(end)}@(ws)@\
  @(bind v ("O" pair))@\
@(end)
@(define array (v))@\
  @(local e)@\
  @(ws)[@(ws)@(coll :gap 0 :var (e))@(value e)@/,?/@(until)]@(end)]@(ws)@\
  @(bind v ("A" e))@\
@(end)
@(freeform)
@(maybe)@(value v)@(end)@badsyntax
```


A few tests. Note, the <code>badsyntax</code> variable is bound to any trailing portion of the input that does not match the syntax. The call to the parser <code>@(value v)</code> extracts the longest prefix of the input which is consistent with the syntax, leaving the remainder to be matched into <code>badsyntax</code>.


```bash
$ echo  -n '{ "a" : { "b" : 3, "c" : [1,2,3] }  }[' | ./txr -l json.txr -
(v "O" ((("S" "a") ("O" ((("S" "b") ("N" "3")) (("S" "c") ("A" (("N" "1") ("N" "2") ("N" "3")))))))))
(badsyntax . "[\n")

$ echo  -n '"\u1234"' | ./txr -l json.txr -
(v "S" "\11064")
(badsyntax . "")
```



## XQuery


The XPath 3.1 standard specifies an XML format to store JSON information. Different XQuery processors implement their own JSON parsers in addition to the XPath functions. One such function has been added, to show, how to map JSON into an XPath map using the BaseX processor. As XQuery is a superset of XPath, the following code is valid XQuery 3.1. Except for 'null', which does not exist in the XPath data model, all JSON datatypes have their XPath equivalent. 'Null' is being represented by the empty sequence. This gets shown at the last function invocation, which creates an XPath map. It may be interesting to note, that the different options for the json serializers and parsers have not been used here.

```xquery

let $json := '
{
  "Astring" : "string-value",
  "Anumber" : 5.7,
  "Anull"   : null,
  "Aarray"  : ["One","Two", 3],
  "Aobject" : {
               "key1": "value1",
               "key2": "value2"
             },
  "Atrue"   : true,
  "Afalse"  : false
}
'
let $xml := json-to-xml($local:json)
return (
 "XPath fn:json-to-xml#1 function:"
 ,""
 ,$xml
 ,""
 ,"Round trip, using fn:xml-to-json#1:"
 ,""
 ,xml-to-json($xml)
 ,""
 ,"Using BaseX json:parse#2 function to create an XPath 3.1 map:"
 ,""
 ,json:parse($local:json, map{"format":"xquery"})
)


```


Result:


```txt

XPath fn:json-to-xml#1 function:

<map xmlns="http://www.w3.org/2005/xpath-functions">
  <string key="Astring">string-value</string>
  <number key="Anumber">5.7</number>
  <null key="Anull"/>
  <array key="Aarray">
    <string>One</string>
    <string>Two</string>
    <number>3</number>
  </array>
  <map key="Aobject">
    <string key="key1">value1</string>
    <string key="key2">value2</string>
  </map>
  <boolean key="Atrue">true</boolean>
  <boolean key="Afalse">false</boolean>
</map>

Round trip, using fn:xml-to-json#1:

{
  "Astring":"string-value",
  "Anumber":5.7,
  "Anull":null,
  "Aarray":[
    "One",
    "Two",
    3
  ],
  "Aobject":{
    "key1":"value1",
    "key2":"value2"
  },
  "Atrue":true,
  "Afalse":false
}

Using BaseX json:parse#2 function to create an XPath 3.1 map:

map {
  "Aobject": map {
    "key1": "value1",
    "key2": "value2"
  },
  "Afalse": false(),
  "Anull": (),
  "Anumber": 5.7e0,
  "Atrue": true(),
  "Astring": "string-value",
  "Aarray": ["One", "Two", 3.0e0]
}

```



## zkl

zkl has a JSON codec based on yajl.

To convert from JSON to zkl:

```zkl
a,b:=Import.lib("zklYAJL");
var [const] YAJL=a, toJSON=b;
src:=
#<<<
0'|{
    "pi": 3.14,
    "large number": 123456789123456791,
    "an array": [
        -1,
        true,
        false,
        null,
        "foo"
    ]
}|;
#<<<
obj:=YAJL().write(src).close();
// or obj:=src.pump(YAJL()).close(); // for example, from file or socket
obj.println();
```

```txt

D(pi:3.14,an array:L(-1,True,False,Void,"foo"),large number:123456789123456791)

```

From zkl to JSON:

```zkl
// using above code plus:
toJSON(obj).println();
```

```txt
{"pi":3.1400000000,"an array":[-1,true,false,null,"foo"],"large number":123456789123456791}

```


