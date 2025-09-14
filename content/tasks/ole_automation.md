+++
title = "OLE Automation"
description = ""
date = 2019-01-10T21:15:31Z
aliases = []
[extra]
id = 10147
[taxonomies]
categories = ["task", "OLE Automation Client and Server"]
tags = []
languages = [
  "autohotkey",
  "go",
  "m2000_interpreter",
  "python",
]
+++

## Task

[http://en.wikipedia.org/wiki/OLE_Automation OLE Automation] is an inter-process communication mechanism based on [http://en.wikipedia.org/wiki/Component_Object_Model Component Object Model] (COM) on Microsoft Windows. 

Provide an automation server implementing objects that can be accessed by a client running in a separate process.  The client gets a proxy-object that can call methods on the object.   
The communication should be able to handle conversions of [http://en.wikipedia.org/wiki/Variant_type variants] to and from the native value types. 


## AutoHotkey

{{libheader|ComDispatch}} by [http://www.autohotkey.com/forum/author-fincs.html fincs]:[http://www.autohotkey.com/forum/topic66559.html discussion]

client: using the ahk ole server as well as the python ole server implemented below

```autohotkey
ahk := comobjactive("ahkdemo.ahk")
ahk.hello("hello world")
py := ComObjActive("python.server")
py.write("hello")
return
```

server: 

```autohotkey
#Persistent
CLSID_ThisScript := "{38A3EB13-D0C4-478b-9720-4D0B2D361DB9}"
APPID_ThisScript := "ahkdemo.ahk"
funcs := ["aRegisterIDs", "aGetObject", "aCallFunc", "hello"]
server := ahkComServer(CLSID_ThisScript, APPID_ThisScript, funcs)   
return

aRegisterIDs(this, CLSID, APPID){
RegisterIDs(CLSID, APPID)
}

hello(this, message){
msgbox % message
}
aGetObject(this, name){
global
return %name%
}

aCallFunc(this, func, args){
return %func%(args)
}

;; ahkcomserver()
ahkComServer(CLSID_ThisScript, APPID_ThisScript, funcs)
{
global serverReady
server := object()
 ; CLSID_ThisScript := "{38A3EB13-D0C4-478b-9720-4D0B2D361DB9}"
 ; APPID_ThisScript := "Garglet.QueryServer"
  
  RegisterIDs(CLSID_ThisScript, APPID_ThisScript)
for i, func in funcs
{
str .= func . ", "
}
str := SubStr(str, 1, strlen(str) - 2)

  myObj := ComDispatch("", str)
; Expose it
  if !(hRemote := ComRemote(myObj, CLSID_ThisScript))
  {
    MsgBox, 16, %A_ScriptName%, Can't remote the object!
    ExitApp
  }
server.CLSID := CLSID_ThisScript
server.APPID := APPID_ThisScript
server.hRemote := hRemote
serverReady := 1
  return server
}

#Include ComRemote.ahk
#include lib\ComDispTable.ahk
#include lib\ComDispatch.ahk
#include lib\ComVar.ahk
```



## Go

This uses OLE automation to create a new Microsoft Word document, write some text to it and after 10 seconds close the document without saving and quit Word.

```go
package main

import (
    "time"
    ole "github.com/go-ole/go-ole"
    "github.com/go-ole/go-ole/oleutil"
)

func main() {
    ole.CoInitialize(0)
    unknown, _ := oleutil.CreateObject("Word.Application")
    word, _ := unknown.QueryInterface(ole.IID_IDispatch)
    oleutil.PutProperty(word, "Visible", true)
    documents := oleutil.MustGetProperty(word, "Documents").ToIDispatch()
    document := oleutil.MustCallMethod(documents, "Add").ToIDispatch()
    content := oleutil.MustGetProperty(document, "Content").ToIDispatch()
    paragraphs := oleutil.MustGetProperty(content, "Paragraphs").ToIDispatch()
    paragraph := oleutil.MustCallMethod(paragraphs, "Add").ToIDispatch()
    rnge := oleutil.MustGetProperty(paragraph, "Range").ToIDispatch()
    oleutil.PutProperty(rnge, "Text", "This is a Rosetta Code test document.")

    time.Sleep(10 * time.Second)

    oleutil.PutProperty(document, "Saved", true)
    oleutil.CallMethod(document, "Close", false)
    oleutil.CallMethod(word, "Quit")
    word.Release()

    ole.CoUninitialize()
}
```



## M2000 Interpreter

M2000 Interpreter is an ActiveX dll (a COM object) so can be start from any language which can handle com objects. Also M2000 can handle other Com Objects, like MS Word. We have to Declare the object (in a Windows OS), form ready made objects in registry, which we have permissions to handle.

We can use Events, from declared objects and from objects that we get as result from methods, using WithEvents.

```M2000 Interpreter

Module CheckAutomation {
      ExitNow=false
      Declare WithEvents Alfa "WORD.APPLICATION"
      \\ minimize console
      Title "Minimized- Waiting", 0
      Wait 300
      Print "ok"
      With Alfa, "Visible", True
      Function ALFA_QUIT {
                  Print "Why you close Word?"
                  ExitNow=True
      }
      M=0
      Every 20 {
            If ExitNow then exit
            M++
            If M>500 then exit
      }
      Try {
            Method Alfa, "QUIT"
      }
      Declare Alfa Nothing
      if ExitNow then {
            Print format$("Finish  {0:2} sec", M/1000)
      } Else {
            Print "Close Word manually"
      }
      \\ show again console
      Title "ok"
}
CheckAutomation

```



## Python

Server uses a client of the ahk server above to register the clsid in the windows registry. 
Translated from <tt>win32com/test/testDynamic.py</tt>

```python
#!/usr/bin/env python
# -*- coding: utf-8 -*-
import win32com.client
from win32com.server.util import wrap, unwrap
from win32com.server.dispatcher import DefaultDebugDispatcher
from ctypes import *
import commands
import pythoncom
import winerror
from win32com.server.exception import Exception

clsid = "{55C2F76F-5136-4614-A397-12214CC011E5}"
iid = pythoncom.MakeIID(clsid)
appid = "python.server"

class VeryPermissive:
    def __init__(self):
        self.data = []
        self.handle = 0
        self.dobjects = {}        
    def __del__(self):
        pythoncom.RevokeActiveObject(self.handle)
    def _dynamic_(self, name, lcid, wFlags, args):
        if wFlags & pythoncom.DISPATCH_METHOD:
            return getattr(self,name)(*args)
        if wFlags & pythoncom.DISPATCH_PROPERTYGET:
            try:
                # to avoid problems with byref param handling, tuple results are converted to lists.
                ret = self.__dict__[name]
                if type(ret)==type(()):
                    ret = list(ret)
                return ret
            except KeyError: # Probably a method request.
                raise Exception(scode=winerror.DISP_E_MEMBERNOTFOUND)
        if wFlags & (pythoncom.DISPATCH_PROPERTYPUT | pythoncom.DISPATCH_PROPERTYPUTREF):
            setattr(self, name, args[0])
            return
        raise Exception(scode=winerror.E_INVALIDARG, desc="invalid wFlags")
    def write(self, x):
        print x
        return 0
import win32com.server.util, win32com.server.policy
child = VeryPermissive()
ob = win32com.server.util.wrap(child, usePolicy=win32com.server.policy.DynamicPolicy)
try:
    handle = pythoncom.RegisterActiveObject(ob, iid, 0)
except pythoncom.com_error, details:
    print "Warning - could not register the object in the ROT:", details
    handle = None    
child.handle = handle  
          
ahk = win32com.client.Dispatch("ahkdemo.ahk")
ahk.aRegisterIDs(clsid, appid)
# autohotkey.exe ahkside.ahk
# python /c/Python26/Scripts/ipython.py -wthread -i pythonside.py
# must use -wthread otherwise calling com client hangs
```

client

```Python
import win32com.client
client = win32com.client.Dispatch("python.server")
client.write("hello world")
```


{{omit from|UNIX Shell}}<!--Wrong OS! -->
