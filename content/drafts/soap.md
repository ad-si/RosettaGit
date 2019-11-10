+++
title = "SOAP"
description = ""
date = 2019-02-08T20:03:36Z
aliases = []
[extra]
id = 1678
[taxonomies]
categories = []
tags = []
+++

{{task|Networking and Web Interaction}}[[Category:Less Than 10 Examples]]
In this task, the goal is to create a SOAP client which accesses functions defined at '''http://example.com/soap/wsdl''', and calls the functions '''soapFunc( )''' and '''anotherSoapFunc( )'''.
{{Clarify_task}}


## ActionScript

{{works with|ActionScript|3.0}}

```actionscript
import mx.rpc.soap.WebService;
import mx.rpc.events.ResultEvent;
var ws:WebService = new WebService();
ws.wsdl = 'http://example.com/soap/wsdl';
ws.soapFunc.addEventListener("result",soapFunc_Result);
ws.anotherSoapFunc.addEventListener("result",anotherSoapFunc_Result);
ws.loadWSDL();
ws.soapFunc();
ws.anotherSoapFunc();
//  method invocation callback handlers
private function soapFunc_Result(event:ResultEvent):void {
  // do something
}
private function anotherSoapFunc_Result(event:ResultEvent):void {
  // do another something
}
```


## AutoHotkey

using embedded vb scripting.
{{libheader|ws4ahk}}

```AutoHotkey
WS_Initialize()
    WS_Exec("Set client = CreateObject(""MSSOAP.SoapClient"")")
    WS_Exec("client.MSSoapInit ""http://example.com/soap/wsdl""")
    callhello = client.soapFunc("hello")
    callanother = client.anotherSoapFunc(34234)

    WS_Eval(result, callhello)
    WS_Eval(result2, callanother)
    Msgbox % result . "`n" . result2
    WS_Uninitialize()
#Include ws4ahk.ahk  ; http://www.autohotkey.net/~easycom/ws4ahk_public_api.html
```



## C

Although this is a generic task to show that calling SOAP functions are possible, the following implementation is geared for the real world. In order to execute it, just choose an actual WSDL URL and construct the input XML files for the functions properly, this can also be done in C but requires libraries like xerces unless you want to really construct the XML from scratch.
{{libheader|libcurl}}

```C

#include <curl/curl.h>
#include <string.h>
#include <stdio.h>

size_t write_data(void *ptr, size_t size, size_t nmeb, void *stream){
    return fwrite(ptr,size,nmeb,stream);
}

size_t read_data(void *ptr, size_t size, size_t nmeb, void *stream){
    return fread(ptr,size,nmeb,stream);
}

void callSOAP(char* URL, char * inFile, char * outFile) {

    FILE * rfp = fopen(inFile, "r");
    if(!rfp)
        perror("Read File Open:");

    FILE * wfp = fopen(outFile, "w+");
    if(!wfp)
        perror("Write File Open:");

    struct curl_slist *header = NULL;
		header = curl_slist_append (header, "Content-Type:text/xml");
		header = curl_slist_append (header, "SOAPAction: rsc");
		header = curl_slist_append (header, "Transfer-Encoding: chunked");
		header = curl_slist_append (header, "Expect:");
    CURL *curl;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, URL);
        curl_easy_setopt(curl, CURLOPT_POST, 1L);
        curl_easy_setopt(curl, CURLOPT_READFUNCTION, read_data);
        curl_easy_setopt(curl, CURLOPT_READDATA, rfp);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, wfp);
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, header);
        curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE_LARGE, (curl_off_t)-1);
        curl_easy_setopt(curl, CURLOPT_VERBOSE,1L);
        curl_easy_perform(curl);

        curl_easy_cleanup(curl);
    }
}

int main(int argC,char* argV[])
{
	if(argC!=4)
		printf("Usage : %s <URL of WSDL> <Input file path> <Output File Path>",argV[0]);
	else
		callSOAP(argV[1],argV[2],argV[3]);
	return 0;
}

```

Input XML for soapFunc()

```XML

<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xmlns:xsd="http://www.w3.org/2001/XMLSchema"
xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
xmlns:dom="http://example.com/soap/wsdl">
   <soapenv:Header/>
   <soapenv:Body>
      <dom:soapFunc soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"/>
   </soapenv:Body>
</soapenv:Envelope>

```

Input XML for anotherSoapFunc()

```XML

<?xml version="1.0" encoding="UTF-8"?>
<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xmlns:xsd="http://www.w3.org/2001/XMLSchema"
xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
xmlns:dom="http://example.com/soap/wsdl">
   <soapenv:Header/>
   <soapenv:Body>
      <dom:anotherSoapFunc soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"/>
   </soapenv:Body>
</soapenv:Envelope>

```



## Clojure


```clojure
(require '[clj-soap.core :as soap])

(let [client (soap/client-fn "http://example.com/soap/wsdl")]
  (client :soapFunc)
  (client :anotherSoapFunc))
```



## ColdFusion


```cfm
<cfset client = createObject("webservice","http://example.com/soap/wsdl")>
<cfset result = client.soapFunc("hello")>
<cfset result = client.anotherSoapFunc(34234)>
```


=={{header|F Sharp|F#}}==
The availability of functions and the type of parameters is checked at compile time. The development environment supports auto-completion and parameter information just like for regular types.


```fsharp
open Microsoft.FSharp.Data.TypeProviders

type Wsdl = WsdlService<"http://example.com/soap/wsdl">
let result = Wsdl.soapFunc("hello")
let result2 = Wsdl.anotherSoapFunc(34234)
```



## Go

{{libheader|Go Soap}}


To make this example a bit more interesting we test against a publicly available working SOAP server at the date of posting.

```go
package main

import (
    "fmt"
    "github.com/tiaguinho/gosoap"
    "log"
)

type CheckVatResponse struct {
    CountryCode string `xml:"countryCode"`
    VatNumber   string `xml:"vatNumber"`
    RequestDate string `xml:"requestDate"`
    Valid       string `xml:"valid"`
    Name        string `xml:"name"`
    Address     string `xml:"address"`
}

var (
    rv CheckVatResponse
)

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    // create SOAP client
    soap, err := gosoap.SoapClient("http://ec.europa.eu/taxation_customs/vies/checkVatService.wsdl")

    // map parameter names to values
    params := gosoap.Params{
        "vatNumber":   "6388047V",
        "countryCode": "IE",
    }

    // call 'checkVat' function
    err = soap.Call("checkVat", params)
    check(err)

    // unmarshal response to 'rv'
    err = soap.Unmarshal(&rv)
    check(err)

    // print response
    fmt.Println("Country Code  : ", rv.CountryCode)
    fmt.Println("Vat Number    : ", rv.VatNumber)
    fmt.Println("Request Date  : ", rv.RequestDate)
    fmt.Println("Valid         : ", rv.Valid)
    fmt.Println("Name          : ", rv.Name)
    fmt.Println("Address       : ", rv.Address)
}
```


{{out}}

```txt

Country Code  :  IE
Vat Number    :  6388047V
Request Date  :  2019-02-08+01:00
Valid         :  true
Name          :  GOOGLE IRELAND LIMITED
Address       :  3RD FLOOR, GORDON HOUSE, BARROW STREET, DUBLIN 4

```


==Icon and {{header|Unicon}}==
{{libheader|Unicon Code Library}} provides the [http://tapestry.tucson.az.us/unilib/pack_soap.html Soap] package.

This code uses Unicon features not available in Icon.

```unicon
import soap

procedure main(A)
    soap := SoapClient(A[1] | "http://example.com/soap/wsdl")  # Allow override of default
    write("soapFunc: ",soap.call("soapFunc"))
    write("anotherSoapFunc: ",soap.call("anotherSoapFunc"))
end
```


A matching SOAP server can be implemented as:


```unicon
import soap

procedure main()
    server := SoapServer("http://example.com/soap/wsdl")
    server.addService("soapFunc",   soapFunc)
    server.addService("anotherSoapFunc", anotherSoapFunc)
    msg := server.handleRequest()
    write(msg)
    exit(0)
end

procedure soapFunc(A[])
    every (s := " ") ||:= (!A || " ")
    return "Hello" || s[1:-1]
end

procedure anotherSoapFunc(A[])
    every (s := " ") ||:= (!A || " ")
    return "Goodbye" || s[1:-1]
end
```



## Kotlin

{{trans|C}}
{{libheader|libcurl}}
{{works with|Ubuntu 14.04}}
Assuming that libcurl is already installed on your system in the default location(s), you first need to build libcurl.klib using the following .def file and the cinterop tool:

```txt

// libcurl.def
headers = /usr/include/curl/curl.h
linkerOpts.linux = -L/usr/lib/x86_64-linux-gnu -lcurl

```

Next, you need to compile the following Kotlin program, linking against libcurl.klib.

```scala
// Kotlin Native v0.6

import kotlinx.cinterop.*
import platform.posix.*
import libcurl.*

fun writeData(ptr: COpaquePointer?, size: size_t, nmeb: size_t, stream: COpaquePointer?)
    = fwrite(ptr, size, nmeb, stream?.reinterpret<FILE>())

fun readData(ptr: COpaquePointer?, size: size_t, nmeb: size_t, stream: COpaquePointer?)
    = fread(ptr, size, nmeb, stream?.reinterpret<FILE>())

fun callSOAP(url: String, inFile: String, outFile: String) {
    val rfp = fopen(inFile, "r")
    if (rfp == null) {
        perror("Read File Open: ")
        exit(1)
    }
    val wfp = fopen(outFile, "w+")
    if (wfp == null) {
        perror("Write File Open: ")
        fclose(rfp)
        exit(1)
    }

    var header: CPointer<curl_slist>? = null
    header = curl_slist_append (header, "Content-Type:text/xml")
    header = curl_slist_append (header, "SOAPAction: rsc")
    header = curl_slist_append (header, "Transfer-Encoding: chunked")
    header = curl_slist_append (header, "Expect:")

    val curl = curl_easy_init()
    if (curl != null) {
        curl_easy_setopt(curl, CURLOPT_URL, url)
        curl_easy_setopt(curl, CURLOPT_POST, 1L)
        curl_easy_setopt(curl, CURLOPT_READFUNCTION, staticCFunction(::readData))
        curl_easy_setopt(curl, CURLOPT_READDATA, rfp)
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, staticCFunction(::writeData))
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, wfp)
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, header)
        curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE_LARGE, -1L)
        curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L)
        curl_easy_perform(curl)
        curl_easy_cleanup(curl)
    }

    curl_slist_free_all(header)
    fclose(rfp)
    fclose(wfp)
}

fun main(args: Array<String>) {
    if (args.size != 3) {
        println("You need to pass exactly 3 command line arguments, namely :-")
        println("    <URL of WSDL> <Input file path> <Output File Path>")
        return
    }
    callSOAP(args[0], args[1], args[2])
}
```

Finally, the resulting .kexe file should be executed passing it similar command line arguments to the C entry.


## Mathematica


```Mathematica
InstallService["http://example.com/soap/wsdl"];
soapFunc["Hello"];
anotherSoapFunc[12345];

```




## Perl

{{libheader|SOAP::Lite}}

```perl
use SOAP::Lite;

print SOAP::Lite
  -> service('http://example.com/soap/wsdl')
  -> soapFunc("hello");
print SOAP::Lite
  -> service('http://example.com/soap/wsdl')
  -> anotherSoapFunc(34234);
```




## Perl 6


```perl6
#!/usr/bin/env perl6

# Reference:
# https://github.com/retupmoca/P6-SOAP
# http://wiki.dreamfactory.com/DreamFactory/Tutorials/Temp_Conversion_SOAP_API

use v6;
use SOAP::Client;

my $request = SOAP::Client.new('http://www.w3schools.com/xml/tempconvert.asmx?WSDL') or die;

say $request.call('CelsiusToFahrenheit', Celsius => 100 ) or die;

say $request.call('FahrenheitToCelsius', Fahrenheit => 212 ) or die;
```

{{out}}

```txt
{CelsiusToFahrenheitResult => [212]}
{FahrenheitToCelsiusResult => [100]}

```



## PHP

{{works with|PHP|5.0.0+}}

```php
<?php
//load the wsdl file
$client = new SoapClient("http://example.com/soap/definition.wsdl");
//functions are now available to be called
$result = $client->soapFunc("hello");
$result = $client->anotherSoapFunc(34234);

//SOAP Information
$client = new SoapClient("http://example.com/soap/definition.wsdl");
//list of SOAP types
print_r($client->__getTypes());
//list if SOAP Functions
print_r($client->__getFunctions());
?>
```



## PureBasic


```Purebasic

XIncludeFile "COMatePLUS.pbi"
Define.COMateObject soapObject = COMate_CreateObject("MSSOAP.SoapClient")
soapObject\Invoke("MSSoapInit('http://example.com/soap/wsdl')")
result = soapObject\Invoke("soapFunc('hello')")
result2 = soapObject\Invoke("anotherSoapFunc(34234)")

```



## Python

{{works with|Python|2.4 and 2.5}}

```python
from SOAPpy import WSDL
proxy = WSDL.Proxy("http://example.com/soap/wsdl")
result = proxy.soapFunc("hello")
result = proxy.anotherSoapFunc(34234)
```


'''Note:''' SOAPpy is a third-party module and can be found at [http://pywebsvcs.sourceforge.net/ Python Web Services]


## Ruby

{{works with|Ruby|1.8}}

```ruby
require 'soap/wsdlDriver'

wsdl = SOAP::WSDLDriverFactory.new("http://example.com/soap/wsdl")
soap = wsdl.create_rpc_driver

response1 = soap.soapFunc(:elementName => "value")
puts response1.soapFuncReturn

response2 = soap.anotherSoapFunc(:aNumber => 42)
puts response2.anotherSoapFuncReturn
```



## Smalltalk

{{works with|Smalltalk/X}} {{works with|Dolphin Smalltalk}} (not sure about Pharo and VW)
(assuming that the open source [http://www.mars.dti.ne.jp/~umejava/smalltalk/soapOpera/index.html SOAPSpray] package has been loaded.)

```smalltalk
| service client response1 response2 |

service := SprayWSDLService onUrl: 'http://example.com/soap/wsdl'.

client := service createClient.
response1 := client send: 'soapFunc' withArguments:{ 'hello' }.
response2 := client send: 'anotherSoapFunc' withArguments:{ 34234 }.
```



## Tcl

{{works with|Tcl|8.5+}}
Uses the <code>[http://code.google.com/p/tclws/ tclws]</code> package.

```Tcl
package require WS::Client

# Grok the service, and generate stubs
::WS::Client::GetAndParseWsdl http://example.com/soap/wsdl
::WS::Client::CreateStubs ExampleService   ;# Assume that's the service name...

# Do the calls
set result1 [ExampleService::soapFunc "hello"]
set result2 [ExampleService::anotherSoapFunc 34234]
```




## Uniface

Assuming http://example.com/soap/wsdl has been imported into repository and, as result, exists a new component called "webservice"


```Uniface

variables
   string result1, result2
endvariables

activate "webservice".soapFunc("hello", result1)
activate "webservice".anotherSoapFunc(34234, result2)

```



## VBScript



```vbscript
Dim client
Dim result
Set client = CreateObject("MSSOAP.SoapClient")
client.MSSoapInit "http://example.com/soap/wsdl"
result = client.soapFunc("hello")
result = client.anotherSoapFunc(34234)
```



## Visual Objects



```visobj
LOCAL oSoapClient 	AS OBJECT //OLEAUTOOBJECT
LOCAL cUrl 		AS STRING
LOCAL uResult        AS USUAL
oSoapClient := OLEAutoObject{"MSSOAP.SoapClient30"}
cUrl        := "http://example.com/soap/wsdl"
IF oSoapClient:fInit
   oSoapClient:mssoapinit(cUrl,"", "", "" )
   uResult := oSoapClient:soapFunc("hello")
   uResult := oSoapClient:anotherSoapFunc(34234)
ENDIF
```


{{omit from|Batch File|Does not have network access.}}
{{omit from|Brainfuck}}
{{omit from|GUISS}}
{{omit from|M4}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|PostScript}}
{{omit from|Retro|Does not have network access.}}
{{omit from|TI-83 BASIC|Does not have network access.}}
{{omit from|TI-89 BASIC|Does not have network access.}}
{{omit from|Unlambda|Does not have network access.}}
{{omit from|Yorick|Does not have network access.}}
{{omit from|ZX Spectrum Basic|Does not have network access.}}
