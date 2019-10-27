+++
title = "HTTP/MIRC Scripting Language"
description = ""
date = 2010-05-03T16:10:03Z
aliases = []
[extra]
id = 7174
[taxonomies]
categories = []
tags = []
+++

{{collection|HTTP}}
A simple implementation of the [[HTTP|HTTP protocol]] in [[MIRC Scripting Language]]. The 'on SOCKREAD' event is structured that way so that it can be re-entered when more data arrives, without losing any information.
<br clear=right>

```mirc
; /wget http://www.example.com/somefile.txt
alias wget {
  var %url = $1-
  var %id = wget. $+ $ctime

  ; A simple regular expression to parse the url:
  var %re = ^http://([^\s/]+)(.*)
  if ($regex(wget,%url,%re) !isnum 1-) {
    echo -ti2a * wget: Invalid url: %url
    return
  }

  var %hostport = $regml(wget,1)
  var %path = $regml(wget,2)

  ; Split on ":"
  var %host = $gettok(%hostport,1,58)
  var %port = $gettok(%hostport,2,58)

  ; Default values
  if (%port == $null) {
    %port = 80
  }
  if (%path == $null) {
    %path = /
  }

  ; Open a socket to the server, and mark it with some
  ; information we will need later
  sockopen %id %host %port
  sockmark %id %hostport %path
}

on *:SOCKOPEN:wget.*:{
  var %id = $sockname
  if ($sockerr) {
    echo -ti2a * wget: Error while connecting: $sock($sockname).wserr $&
      $sock($sockname).wsmsg
    sockclose %id
    halt
  }

  ; Take out the hostname and path which the socket has been marked with
  var %mark = $sock(%id).mark
  var %hostport = $gettok(%mark,1,32)
  var %path = $gettok(%mark,2-,32)

  ; Update the mark with a state name. (See below)
  sockmark %id statusline

  ; Send the request
  var %w = sockwrite -tn %id
  %w GET %path HTTP/1.0
  %w Host: %hostport
  %w Connection: Close
  %w $crlf
}

on *:SOCKREAD:wget.*:{
  var %id = $sockname
  if ($sockerr) {
    echo -ti2a * wget: Error while receiving: $sock($sockname).wserr $sock($sockname).wsmsg
    sockclose %id
    halt
  }

  ; Look at the current state
  var %state = $sock(%id).mark

  ; Read the first line
  var %line
  sockread %line

  ; Loop while more data is coming in
  while ($sockbr) {
    if (%state == statusline) {
      ; STATUSLINE: This state is the initial one.
      ; Examine the status line and take an appropriate action
      var %version = $gettok(%line,1,32)
      var %code = $gettok(%line,2,32)
      var %reason = $gettok(%line,3-,32)
      if (2?? iswm %code) {
        ; 2xx codes indicate it went ok
        %state = headers
        sockmark %id %state
      }
      elseif (3?? iswm %code) {
        ; 3xx codes indicate redirection
        %state = redirect
        sockmark %id %state
      }
      else {
        ; but anything else is treated as an error
        echo -ti2a * wget: Server error: %line
        sockclose %id
        halt
      }
    }
    elseif (%state == redirect) {
      ; REDIRECT: This state is entered when a 3xx status code is received.
      ; Find the 'Location' header, and issue a new request.
      if ($len(%line) == 0) {
        ; An empty line indicate the end of the headers
        echo -ti2a * wget: Server error: Got 3xx status code, but nowhere to redirect.
        sockclose %id
        halt
      }
      else {
        var %header = $gettok(%line,1,58)
        var %value = $gettok(%line,2-,58)
        if (%header == Location) {
          ; 'Location' header found. Issue a new request.
          wget %value
          sockclose %id
          halt
        }
      }
    }
    elseif (%state == headers) {
      ; HEADERS: This state is entered after the status line has been examined.
      ; Examine the headers, and look for a blank line.
      if ($len(%line) == 0) {
        ; An empty line indicate the end of the headers
        %state = data
        sockmark %id %state
      }
      else {
        var %header = $gettok(%line,1,58)
        var %value = $gettok(%line,2-,58)
        ; Do something with the headers
      }
    }
    elseif (%state == data) {
      ; DATA: This state is the final one.
      ; Read and display everything received
      echo -a %line
    }
    sockread %line
  }
}
```

