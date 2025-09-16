+++
title = "Active Directory/Connect"
description = ""
date = 2019-09-09T05:54:14Z
aliases = []
[extra]
id = 2983
[taxonomies]
categories = ["Programming environment operations", "task"]
tags = []
languages = [
  "autohotkey",
  "autoit",
  "c",
  "coldfusion",
  "csharp",
  "d",
  "erlang",
  "go",
  "haskell",
  "java",
  "kotlin",
  "netrexx",
  "perl",
  "perl_6",
  "php",
  "picolisp",
  "python",
  "racket",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "smart_basic",
  "tcl",
  "vbscript",
]
+++

## Task

The task is to establish a connection to an Active Directory or Lightweight Directory Access Protocol server.


## AutoIt

{{works with|AutoIt}}

```AutoIt
 #include <AD.au3>

_AD_Open()
```



## AutoHotkey

{{works with|AutoHotkey_L}}
{{trans|VBScript}}

```AutoHotkey
objConn := CreateObject("ADODB.Connection")
objCmd := CreateObject("ADODB.Command")
objConn.Provider := "ADsDSOObject"
objConn.Open()
```


## C

With OpenLDAP:

```c
#include <ldap.h

...
char *name, *password;
...
LDAP *ld = ldap_init("ldap.somewhere.com", 389);
ldap_simple_bind_s(ld, name, password);
... after done with it...
ldap_unbind(ld);
```


## C#

```c#

// Requires adding a reference to System.DirectoryServices
var objDE = new System.DirectoryServices.DirectoryEntry("LDAP://DC=onecity,DC=corp,DC=fabrikam,DC=com");

```




## ColdFusion


```cfm

<cfldap
server = "#someip#"
action="query"
start="somestart#"
username = "#someusername#"
password = "#somepassowrd#"
name = "results"
scope="subtree"
attributes = "#attributeslist#"
>

```




## D

Based on dopenldap.

```d

import openldap;
import std.stdio;

void main() {
  auto ldap = LDAP("ldap://localhost");
  auto r = ldap.search_s("dc=example,dc=com", LDAP_SCOPE_SUBTREE, "(uid=%s)".format("test"));
  int b = ldap.bind_s(r[0].dn, "password");
  scope(exit) ldap.unbind;
  if (b)
  {
    writeln("error on binding");
    return;
  }

  // do something
  ...

}

```



## Erlang

This needs a test case. Is there a LDAP server available?

```Erlang

-module(ldap_example).
-export( [main/1] ).

main( [Host, DN, Password] ) ->
 {ok, Handle} = eldap:open( [Host] ),
 ok = eldap:simple_bind( Handle, DN, Password ),
 eldap:close( Handle ).

```


=={{header|F_Sharp|F#}}==
{{trans|C_sharp}}
<p>For Active Directory we use the library System.DirectoryServices</p>

```fsharp
let adObject = new System.DirectoryServices.DirectoryEntry("LDAP://DC=onecity,DC=corp,DC=fabrikam,DC=com")
```

<p>For your average LDAP server we use System.DirectoryServices.Protocol</p>
<p>For a minimal example we make an anonymous connect to the local machine on the well-known LDAP port 389

```fsharp
let ldapServer = new System.DirectoryServices.Protocols.LdapDirectoryIdentifier("127.0.0.1")
let connect = new System.DirectoryServices.Protocols.LdapConnection(ldapServer)
connect.Bind()
```



## Go

{{libheader|go-ldap-client}}


There are a large number of third-party LDAP libraries for Go. This uses one of the simpler ones and the code below is largely taken from the example on its main page.

```go
package main

import (
    "log"
    "github.com/jtblin/go-ldap-client"
)

func main() {
    client := &ldap.LDAPClient{
        Base:         "dc=example,dc=com",
        Host:         "ldap.example.com",
        Port:         389,
        UseSSL:       false,
        BindDN:       "uid=readonlyuser,ou=People,dc=example,dc=com",
        BindPassword: "readonlypassword",
        UserFilter:   "(uid=%s)",
        GroupFilter:  "(memberUid=%s)",
        Attributes:   []string{"givenName", "sn", "mail", "uid"},
    }
    defer client.Close()
    err := client.Connect()
    if err != nil {
        log.Fatalf("Failed to connect : %+v", err)
    }
    // Do something
}
```



## Haskell


Example uses the [https://hackage.haskell.org/package/ldap-client <tt>ldap-client</tt>] package:


```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Foldable (for_)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import           Ldap.Client (Attr(..), Filter(..))
import qualified Ldap.Client as Ldap (Dn(..), Host(..), search, with, typesOnly)

main :: IO ()
main = do
    entries <- Ldap.with (Ldap.Plain "localhost") 389 $ \ldap ->
        Ldap.search ldap (Ldap.Dn "o=example.com") (Ldap.typesOnly True) (Attr "uid" := Text.encodeUtf8 "user") []
    for_ entries $ \entry ->
        print entry
```



## Java


This code uses the Apache Directory third-party library.


```java
import java.io.IOException;
import org.apache.directory.api.ldap.model.exception.LdapException;
import org.apache.directory.ldap.client.api.LdapConnection;
import org.apache.directory.ldap.client.api.LdapNetworkConnection;

public class LdapConnectionDemo {

    public static void main(String[] args) throws LdapException, IOException {
        try (LdapConnection connection = new LdapNetworkConnection("localhost", 10389)) {
            connection.bind();
            connection.unBind();
        }
    }
}
```



## Kotlin


```scala

import org.apache.directory.api.ldap.model.exception.LdapException
import org.apache.directory.ldap.client.api.LdapNetworkConnection
import java.io.IOException
import java.util.logging.Level
import java.util.logging.Logger

class LDAP(map: Map<String, String>) {
    fun run() {
        var connection: LdapNetworkConnection? = null
        try {
            if (info) log.info("LDAP Connection to $hostname on port $port")
            connection = LdapNetworkConnection(hostname, port.toInt())

            try {
                if (info) log.info("LDAP bind")
                connection.bind()
            } catch (e: LdapException) {
                log.severe(e.toString())
            }

            try {
                if (info) log.info("LDAP unbind")
                connection.unBind()
            } catch (e: LdapException) {
                log.severe(e.toString())
            }
        } finally {
            try {
                if (info) log.info("LDAP close connection")
                connection!!.close()
            } catch (e: IOException) {
                log.severe(e.toString())
            }
        }
    }

    private val log = Logger.getLogger(LDAP::class.java.name)
    private val info = log.isLoggable(Level.INFO)
    private val hostname: String by map
    private val port: String by map
}

fun main(args: Array<String>) = LDAP(mapOf("hostname" to "localhost", "port"  to "10389")).run()

```



## NetRexx

Uses the [http://directory.apache.org/api/ Apache LDAP API], connecting to a local [http://directory.apache.org/apacheds/1.5/ ApacheDS] LDAP directory server.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import org.apache.directory.ldap.client.api.LdapConnection
import org.apache.directory.ldap.client.api.LdapNetworkConnection
import org.apache.directory.shared.ldap.model.exception.LdapException
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class RDirectoryLDAP public

  properties constant
    log_ = LoggerFactory.getLogger(RDirectoryLDAP.class)

  properties private static
    connection = LdapConnection null

  method main(args = String[]) public static
    ldapHostName = String "localhost"
    ldapPort = int 10389

    if log_.isInfoEnabled() then log_.info("LDAP Connection to" ldapHostName "on port" ldapPort)
    connection = LdapNetworkConnection(ldapHostName, ldapPort)

    do
      if log_.isTraceEnabled() then log_.trace("LDAP bind")
      connection.bind()

      if log_.isTraceEnabled() then log_.trace("LDAP unbind")
      connection.unBind()
    catch lex = LdapException
      log_.error("LDAP Error", Throwable lex)
    catch iox = IOException
      log_.error("I/O Error", Throwable iox)
    finally
      do
      if connection \= null then connection.close()
      catch iox = IOException
        log_.error("I/O Error on connection.close()", Throwable iox)
      end
    end

    return

```


'''Sample <tt>log4j.xml</tt> configuration file:'''

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE log4j:configuration SYSTEM "log4j.dtd">
<log4j:configuration xmlns:log4j='http://jakarta.apache.org/log4j/'>
  <appender name="stdout" class="org.apache.log4j.ConsoleAppender">
    <param name="Target" value="System.out" />
    <layout class="org.apache.log4j.PatternLayout">
      <param name="ConversionPattern" value="[%d{HH:mm:ss}] %-5p [%c] - %m%n" />
    </layout>
  </appender>

  <!-- with these we'll not get innundated when switching to DEBUG -->
  <logger name="org.apache.directory.shared.ldap.name">
    <level value="warn" />
  </logger>
  <logger name="org.apache.directory.shared.codec">
    <level value="warn" />
  </logger>
  <logger name="org.apache.directory.shared.asn1">
    <level value="warn" />
  </logger>

  <root>
    <level value="info" />
    <appender-ref ref="stdout" />
  </root>
</log4j:configuration>

```


'''Output:'''

```txt

[08:40:05] INFO  [RDirectoryLDAP] - LDAP Connection to localhost on port 10389

```



## Perl

[http://search.cpan.org/dist/perl-ldap/|Perl LDAP Modules]

```perl

use Net::LDAP;

my $ldap = Net::LDAP->new('ldap://ldap.example.com') or die $@;
my $mesg = $ldap->bind( $bind_dn, password => $bind_pass );

```



## Perl 6

Using module LMDB - bindings to the openLDAP library. Requires an LDAP instance.


```perl6
use LMDB;

my %DB := LMDB::DB.open(:path<some-dir>, %connection-parameters);

```


%DB may be accessed, read from and written to like a native hash.


## PHP

[http://php.net/ldap PHP LDAP Reference]

```php
<?php
$ldap = ldap_connect($hostname, $port);
$success = ldap_bind($ldap, $username, $password);
```



## PicoLisp


```PicoLisp
(unless (=0 (setq Ldap (native "libldap.so" "ldap_open" 'N "example.com" 389)))
   (quit "Can't open LDAP") )

(native "libldap.so" "ldap_simple_bind_s" 'I Ldap "user" "password")
```



## Python

{{works with|Python|2.6}}
{{libheader|python-ldap}}

[http://www.python-ldap.org/doc/html/index.html python-ldap Documentation]


```python
import ldap

l = ldap.initialize("ldap://ldap.example.com")
try:
    l.protocol_version = ldap.VERSION3
    l.set_option(ldap.OPT_REFERRALS, 0)

    bind = l.simple_bind_s("me@example.com", "password")
finally:
    l.unbind()

```



## Racket

This version uses the ldap package, and was tested against OpenLDAP (with real values):

```racket
#lang racket
(require net/ldap)
(ldap-authenticate "ldap.somewhere.com" 389 "uid=username,ou=people,dc=somewhere,dc=com" password)
```


{{trans|C}}

This is a direct translation of the C code -- I have no idea how to try it out since I don't have a working ldap server...  So take it as a stub that waits for someone who can try it to do so.  (And it's a low level thing anyway, there's an ldap package for Racket which I can't try for a similar reason.)


```racket
#lang racket

(require ffi/unsafe ffi/unsafe/define)

(define-ffi-definer defldap (ffi-lib "libldap"))
(defldap ldap_init (_fun _string _int -> _pointer))
(defldap ldap_unbind (_fun _pointer -> _void))
(defldap ldap_simple_bind_s (_fun _pointer _string _string -> _int))
(defldap ldap_err2string (_fun _int -> _string))

(define name ...)
(define password ...)
(define ld (ldap_init "ldap.somewhere.com" 389))
(ldap_simple_bind_s ld name password)

(ldap_unbind ld)
```



## Ruby

Similar to Tcl, assume the AD server talks LDAP.

There are many Ruby LDAP packages ([http://rubyforge.org/search/?type_of_search=soft&words=ldap&Search=Search]) -- this solution uses [http://net-ldap.rubyforge.org/rdoc/ Net::LDAP] ("Pure Ruby LDAP Tools" on RubyForge, gem name "ruby-net-ldap")

{{libheader|RubyGems}}

```ruby
require 'rubygems'
require 'net/ldap'
ldap = Net::LDAP.new(:host => 'ldap.example.com', :base => 'o=companyname')
ldap.authenticate('bind_dn', 'bind_pass')
```



## Run BASIC

{{incorrect|Run BASIC|Active Directory has nothing to do with the local file system}}

```runbasic
print shell$("dir") ' shell out to the os and print it
```



## Rust

This solution uses the popular [https://crates.io/crates/ldap3 ldap3] crate.

```rust

let conn = ldap3::LdapConn::new("ldap://ldap.example.com")?;
conn.simple_bind("bind_dn", "bind_pass")?.success()?;

```



## Scala


```scala
import java.io.IOException

import org.apache.directory.api.ldap.model.exception.LdapException
import org.apache.directory.ldap.client.api.{LdapConnection, LdapNetworkConnection}

object LdapConnectionDemo {
  @throws[LdapException]
  @throws[IOException]
  def main(args: Array[String]): Unit = {
    try {
      val connection: LdapConnection = new LdapNetworkConnection("localhost", 10389)
      try {
        connection.bind()
        connection.unBind()
      } finally if (connection != null) connection.close()
    }
  }
}
```


## smart BASIC

{{incorrect|smart BASIC|Active Directory has nothing to do with the local file system}}

smart BASIC uses three separate commands to list the current directory, folder and files respectively.

```qbasic
PRINT "Current directory: ";CURRENT_DIR$()
PRINT
PRINT "Folders:"
PRINT
DIR "/" LIST DIRS a$,c
FOR n = 0 TO c-1
PRINT ,a$(n)
NEXT n
PRINT
PRINT "Files:"
PRINT
DIR "/" LIST FILES a$,c
FOR n = 0 TO c-1
PRINT ,a$(n)
NEXT n
```



## Tcl

This does not use SSPI/Kerberos yet, so your AD would need to allow simple ldap access.

```tcl
package require ldap
set conn [ldap::connect $host $port]
ldap::bind $conn $user $password
```



## VBScript

Creating the normal connection to AD

```vbscript
Set objConn = CreateObject("ADODB.Connection")
Set objCmd = CreateObject("ADODB.Command")
objConn.Provider = "ADsDSOObject"
objConn.Open
```


{{omit from|Active Directory}}
{{omit from|AWK}}
{{omit from|Clojure}}
{{omit from|GUISS}}
{{omit from|Inform 7|Does not have network access.}}
{{omit from|Lilypond}}
{{omit from|Lingo}}
{{omit from|TI-83 BASIC}}
{{omit from|TI-89 BASIC}} <!-- Does not have network access. -->
{{omit from|Mathematica}}
{{omit from|MIPS Assembly|None of the commonly used implementations can access AD functions}}
{{omit from|ML/I}}
{{omit from|PARI/GP}}
{{omit from|PostScript}}
{{omit from|Retro}}
{{omit from|SNOBOL4|Does not have network access.}}
{{omit from|Yorick|Does not have network access.}}
{{omit from|ZX Spectrum Basic|Does not have network access.}}
{{omit from|Maxima}}

[[Category:Active Directory]]
