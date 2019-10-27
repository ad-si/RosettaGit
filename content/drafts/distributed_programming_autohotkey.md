+++
title = "Distributed programming/AutoHotkey"
description = ""
date = 2010-02-06T17:24:45Z
aliases = []
[extra]
id = 4829
[taxonomies]
categories = []
tags = []
+++

{{libheader|WinSock2.ahk}}

WinSock2 library created by derRaphael. Basic code structure created by Trikster. Everything else created by B R (skylord5816 on IRC).
<div style="clear:both;width:full;overflow:scroll">
```autohotkey
#Include WinSock2.ahk
#Persistent
#SingleInstance, force
#Include, WinSock2.ahk
SetBatchLines, -1
SetWorkingDir, %A_ScriptDir%
Gui, Add, Button, gB1, Button number One!
Gui, Add, Edit, vE1, Type in me!
Gui, Add, Button, gB2, Press me after you're done typing!
Gui, Add, DropDownList, vDDL1 gDDL1, Select something!||One|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten
Gui, Add, Button, gB3, Click me!
 
Server  := "irc.freenode.net"
Port    := "6667"             
 
Channel := "#aokjf" ; Choose something long and random - a channel sure to be deserted.
 
Nick := "aokjfONE"  ; Register this! You can drop it later if you want, but register it!
Pass := "aokjfONE"  ; It can be any nick and any pass.
 
Name := "distributed program"
 
BotCmd := "!"
OtherBotCmd := "~"
 
OnExit, CleanUp
WS2_CleanUp()
If (!Socket := WS2_Connect(Connection := Server . ":" . Port))
{
   MsgBox, 16, Error!, An error occured wilist connecting to %Connection%
}
 
   ; Set OnMessage function
   WS2_AsyncSelect(Socket, "DataProcess")
 
   ; Send AUTH info to the server
   ; USER A-trivial-nickname a-domain-like-google.com some-trivial-stuff :your-real-name
   WS2_SendData(Socket, "USER " . Nick . " google.com AHKBOT :" . Name . "`n") ; All data send to the server must be
                                                                               ; followed by a newline.
 
   ; PASS A-trivial-pass
   WS2_SendData(Socket, "PASS " . Pass . "`n")
 
   ; NICK A-trivial-nick
   WS2_SendData(Socket, "NICK " . Nick . "`n")
 
   ; Join channel
   ; JOIN A-trivial-channel
   WS2_SendData(Socket, "JOIN " . Channel . "`n")
 
   Gui, Show
Return
 
DataProcess(Socket, Data) ; OnMessage function
{
   global Server,Port,Channel,Nick,Pass,Name,BotCMD
   StringSplit, Param, Data, %A_Space%
   Name := SubStr(Data, 2, InStr(Data, "!")-2)
   StringReplace, Command, Param5, % Chr(10),, All
   StringReplace, Command, Command, % Chr(13),, All
 
   If (Param1 == "PING")
      WS2_SendData(Socket, "PONG " . Param2 . "`n")
   Else If (RegExMatch(Data, ":\" . BotCMD . " "))
   {
      If (Command == "B1")
         MsgBox, The other person pressed Button Number One
      Else If (Command == "E1B2")
      {
         out :=
         Loop
         {
            If (A_Index < 6)
               continue
            If Param%A_Index%
               out := out . " " . Param%A_Index%
            Else
               break
         }
         StringTrimRight, out, out, 1
         MsgBox, The other person typed:`n%out%
      }
      Else If (Command == "DDL1")
         MsgBox, The other person selected %Param6% %Param7% in the drop-down-list.
      Else If (Command == "B3")
         MsgBox, The other person clicked Button Three.
   }
   return
}
B1:
WS2_SendData(Socket, "PRIVMSG " . Channel . " :" . OtherBotCmd . " B1`n")
return
B2:
Gui, Submit, NoHide
WS2_SendData(Socket, "PRIVMSG " . Channel . " :" . OtherBotCmd . " E1B2 " . E1 . "`n")
return
DDL1:
Gui, Submit, NoHide
WS2_SendData(Socket, "PRIVMSG " . Channel . " :" . OtherBotCmd . " DDL1 " . DDL1 . "`n")
return
B3:
WS2_SendData(Socket, "PRIVMSG " . Channel . " :" . OtherBotCmd . " B3" . "`n")
return
CleanUp:
GuiClose:
WS2_CleanUp()
ExitApp
```
</div>

Put the above on one computer, and run it. Then put the below on a different computer (no need for a network, just add Internet!) and run it.
<div style="width:full;overflow:scroll">
```autohotkey
#Include WinSock2.ahk
#Persistent
#SingleInstance, force
#Include WinSock2.ahk
SetBatchLines, -1
SetWorkingDir, %A_ScriptDir%
Gui, Add, Button, gB1, Button number One!
Gui, Add, Edit, vE1, Type in me!
Gui, Add, Button, gB2, Press me after you're done typing!
Gui, Add, DropDownList, vDDL1 gDDL1, Select something!||One|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten
Gui, Add, Button, gB3, Click me!
 
Server  := "irc.freenode.net"
Port    := "6667"             
 
Channel := "#aokjf" ; Choose something long and random - a channel sure to be deserted.
 
Nick := "aokjfTWO"  ; Register this! You can drop it later if you want, but register it!
Pass := "aokjfTWO"  ; It can be any nick and any pass.
 
Name := "distributed program"
 
BotCmd := "~"
OtherBotCmd := "!"
 
OnExit, CleanUp
WS2_CleanUp()
If (!Socket := WS2_Connect(Connection := Server . ":" . Port))
{
   MsgBox, 16, Error!, An error occured wilist connecting to %Connection%
}
 
   ; Set OnMessage function
   WS2_AsyncSelect(Socket, "DataProcess")
 
   ; Send AUTH info to the server
   ; USER A-trivial-nickname a-domain-like-google.com some-trivial-stuff :your-real-name
   WS2_SendData(Socket, "USER " . Nick . " google.com AHKBOT :" . Name . "`n") ; All data send to the server must be
                                                                               ; followed by a newline.
 
   ; PASS A-trivial-pass
   WS2_SendData(Socket, "PASS " . Pass . "`n")
 
   ; NICK A-trivial-nick
   WS2_SendData(Socket, "NICK " . Nick . "`n")
 
   ; Join channel
   ; JOIN A-trivial-channel
   WS2_SendData(Socket, "JOIN " . Channel . "`n")
 
   Gui, Show
Return
 
DataProcess(Socket, Data) ; OnMessage function
{
   global Server,Port,Channel,Nick,Pass,Name,BotCMD
   StringSplit, Param, Data, %A_Space%
   Name := SubStr(Data, 2, InStr(Data, "!")-2)
   StringReplace, Command, Param5, % Chr(10),, All
   StringReplace, Command, Command, % Chr(13),, All
 
   If (Param1 == "PING")
      WS2_SendData(Socket, "PONG " . Param2 . "`n")
   Else If (RegExMatch(Data, ":\" . BotCMD . " "))
   {
      If (Command == "B1")
         MsgBox, The other person pressed Button Number One
      Else If (Command == "E1B2")
      {
         out :=
         Loop
         {
            If (A_Index < 6)
               continue
            If Param%A_Index%
               out := out . " " . Param%A_Index%
            Else
               break
         }
         StringTrimRight, out, out, 1
         MsgBox, The other person typed:`n%out%
      }
      Else If (Command == "DDL1")
         MsgBox, The other person selected %Param6% %Param7% in the drop-down-list.
      Else If (Command == "B3")
         MsgBox, The other person clicked Button Three.
   }
   return
}
B1:
WS2_SendData(Socket, "PRIVMSG " . Channel . " :" . OtherBotCmd . " B1`n")
return
B2:
Gui, Submit, NoHide
WS2_SendData(Socket, "PRIVMSG " . Channel . " :" . OtherBotCmd . " E1B2 " . E1 . "`n")
return
DDL1:
Gui, Submit, NoHide
WS2_SendData(Socket, "PRIVMSG " . Channel . " :" . OtherBotCmd . " DDL1 " . DDL1 . "`n")
return
B3:
WS2_SendData(Socket, "PRIVMSG " . Channel . " :" . OtherBotCmd . " B3" . "`n")
return
CleanUp:
GuiClose:
WS2_CleanUp()
ExitApp
```
</div>
