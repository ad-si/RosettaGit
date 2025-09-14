+++
title = "Active Directory/Search for a user"
description = ""
date = 2019-07-18T10:43:17Z
aliases = []
[extra]
id = 2973
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
languages = [
  "c",
  "d",
  "eiffel",
  "go",
  "haskell",
  "java",
  "netrexx",
  "oorexx",
  "perl_6",
  "php",
  "picolisp",
  "python",
  "rexx",
  "ruby",
  "run_basic",
  "scala",
  "tcl",
  "unix_shell",
  "vbscript",
]
+++

## Task

Make sure you [[Connect to Active Directory]]


## C


```C>#include <ldap.h


char *name, *password;
...

LDAP *ld = ldap_init("ldap.somewhere.com", 389);
ldap_simple_bind_s(ld, name, password);

LDAPMessage **result;
ldap_search_s(ld, "dc=somewhere,dc=com", LDAP_SCOPE_SUBTREE,
	/* search for all persons whose names start with joe or shmoe */
	"(&(objectclass=person)(|(cn=joe*)(cn=shmoe*)))",
	NULL, /* return all attributes */
	0,  /* want both types and values of attrs */
	result); /* ldap will allocate room for return messages */

/* arduously do stuff here to result, with ldap_first_message(),
	ldap_parse_result(), etc. */

ldap_msgfree(*result);	/* free messages */
ldap_unbind(ld);	/* disconnect */
```




## D

Based on dopenldap.

```d

import openldap;
import std.stdio;

void main() {
  // connect to server
  auto ldap = LDAP("ldap://localhost");

  // search for uid
  auto r = ldap.search_s("dc=example,dc=com", LDAP_SCOPE_SUBTREE, "(uid=%s)".format("test"));

  // show properties
  writeln("Found dn: %s", r[0].dn);
  foreach(k, v; r[0].entry)
    writeln("%s = %s", k, v);

  // bind on found entry
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



## Eiffel

Eiffel does not have the notion of "return", but "Result". A consequence of this is that Eiffel routines are Single-entry-Single-exit, which means less bugs. In the example (below), the Result is of type BOOLEAN.

Moreover, strings in Eiffel are objects and cannot be directly passed to the Windows OS. As such, they need to undergo a format change through the facilities of a WEL_STRING, which makes the appropriate structure conversion.

```Eiffel

feature -- Validation

	is_user_credential_valid (a_domain, a_username, a_password: READABLE_STRING_GENERAL): BOOLEAN
			-- Is the pair `a_username'/`a_password' a valid credential in `a_domain'?
		local
			l_domain, l_username, l_password: WEL_STRING
		do
			create l_domain.make (a_domain)
			create l_username.make (a_username)
			create l_password.make (a_password)
			Result := cwel_is_credential_valid (l_domain.item, l_username.item, l_password.item)
		end

```


Because Active Directory is a Windows OS facility, in Eiffel we must use the WEL (Windows Eiffel Library) components. Thus, the code above is not cross-platform. Moreover, the call to `cwel_is_credential_valid' is shown below:


```Eiffel

	cwel_is_credential_valid (a_domain, a_username, a_password: POINTER): BOOLEAN
		external
			"C inline use %"wel_user_validation.h%""
		alias
			"return cwel_is_credential_valid ((LPTSTR) $a_domain, (LPTSTR) $a_username, (LPTSTR) $a_password);"
		end

```



## Go

There are a large number of third-party LDAP libraries for Go. This uses one of the simpler ones and the code below is largely taken from the example on its main page.

```go
package main

import (
    "log"
    "github.com/jtblin/go-ldap-client"
)

func main() {
    client := &ldap.LDAPClient{
        Base:        "dc=example,dc=com",
        Host:        "ldap.example.com",
        Port:        389,
        GroupFilter: "(memberUid=%s)",
    }
    defer client.Close()
    err := client.Connect()
    if err != nil { 
        log.Fatalf("Failed to connect : %+v", err)
    }
    groups, err := client.GetGroupsOfUser("username")
    if err != nil {
        log.Fatalf("Error getting groups for user %s: %+v", "username", err)
    }
    log.Printf("Groups: %+v", groups) 
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


The following code uses the Apache Directory project, version 1.0.0.


```java
import java.io.IOException;
import org.apache.directory.api.ldap.model.cursor.CursorException;
import org.apache.directory.api.ldap.model.cursor.EntryCursor;
import org.apache.directory.api.ldap.model.entry.Entry;
import org.apache.directory.api.ldap.model.exception.LdapException;
import org.apache.directory.api.ldap.model.message.SearchScope;
import org.apache.directory.ldap.client.api.LdapConnection;
import org.apache.directory.ldap.client.api.LdapNetworkConnection;

public class LdapSearchDemo {

    public static void main(String[] args) throws IOException, LdapException, CursorException {
        new LdapSearchDemo().demonstrateSearch();
    }

    private void demonstrateSearch() throws IOException, LdapException, CursorException {
        try (LdapConnection conn = new LdapNetworkConnection("localhost", 11389)) {
            conn.bind("uid=admin,ou=system", "********");
            search(conn, "*mil*");
            conn.unBind();
        }
    }

    private void search(LdapConnection connection, String uid) throws LdapException, CursorException {
        String baseDn = "ou=users,o=mojo";
        String filter = "(&(objectClass=person)(&(uid=" + uid + ")))";
        SearchScope scope = SearchScope.SUBTREE;
        String[] attributes = {"dn", "cn", "sn", "uid"};
        int ksearch = 0;

        EntryCursor cursor = connection.search(baseDn, filter, scope, attributes);
        while (cursor.next()) {
            ksearch++;
            Entry entry = cursor.get();
            System.out.printf("Search entry %d = %s%n", ksearch, entry);
        }
    }
}
```



## ooRexx

Using LDAP connecting to a local [http://directory.apache.org/apacheds/1.5/ ApacheDS] LDAP directory server.

This program drives the <tt>ldapsearch</tt> command and captures the output into an external data queue via ooRexx <tt>rxqueue</tt> facility.  The contents of the queue are then read into program variables for further processing.


```ooRexx
/* Rexx */
do
  LDAP_URL        = 'ldap://localhost:11389'
  LDAP_DN_STR     = 'uid=admin,ou=system'
  LDAP_CREDS      = '********'
  LDAP_BASE_DN    = 'ou=users,o=mojo'
  LDAP_SCOPE      = 'sub'
  LDAP_FILTER     = '"(&(objectClass=person)(&(uid=*mil*)))"'
  LDAP_ATTRIBUTES = '"dn" "cn" "sn" "uid"'

  ldapCommand =               ,
    'ldapsearch'              ,
    '-s base'                 ,
    '-H' LDAP_URL             ,
    '-LLL'                    ,
    '-x'                      ,
    '-v'                      ,
    '-s' LDAP_SCOPE           ,
    '-D' LDAP_DN_STR          ,
    '-w' LDAP_CREDS           ,
    '-b' LDAP_BASE_DN         ,
    LDAP_FILTER               ,
    LDAP_ATTRIBUTES           ,
    '2>&1'                    ,
    '|'                       ,
    'rxqueue'                 ,
    ''

  address command,
    ldapCommand

  ldapResult. = ''
  loop ln = 1 to queued()
    parse pull line
    ldapResult.0  = ln
    ldapResult.ln = line
    end ln

  loop ln = 1 to ldapResult.0
    parse var ldapResult.ln 'dn:'  dn_   ,
      0                     'uid:' uid_  ,
      0                     'sn:'  sn_   ,
      0                     'cn:'  cn_
    select
      when length(strip(dn_,  'b')) > 0 then dn  = dn_
      when length(strip(uid_, 'b')) > 0 then uid = uid_
      when length(strip(sn_,  'b')) > 0 then sn  = sn_
      when length(strip(cn_,  'b')) > 0 then cn  = cn_
      otherwise nop
      end
    end ln

  say 'Distiguished Name:' dn
  say '      Common Name:' cn
  say '          Surname:' sn
  say '           userID:' uid

  return
end
exit

```

'''Output:'''

```txt

Distiguished Name:  cn=John Milton,ou=users,o=mojo
      Common Name:  John Milton
          Surname:  Milton
           userID:  jmilton

```



## Perl 6


```perl6
#!/usr/bin/env perl6

# 20190718 Perl 6 programming solution
# https://github.com/perl6/doc/issues/2898
# https://www.facebook.com/groups/perl6/permalink/2379873082279037/

# Reference:
# https://github.com/Altai-man/cro-ldap
# https://www.forumsys.com/tutorials/integration-how-to/ldap/online-ldap-test-server/

use v6.d;
use Cro::LDAP::Client;

my $client = await Cro::LDAP::Client.connect('ldap://ldap.forumsys.com');

my $bind = await $client.bind(
   name=>'cn=read-only-admin,dc=example,dc=com',password=>'password'
);
die $bind.error-message if $bind.result-code;

my $resp = $client.search(
   :dn<dc=example,dc=com>, base=>"ou=mathematicians", filter=>'(&(uid=gauss))'
);

react {
   whenever $resp -> $entry {
      for $entry.attributes.kv -> $k, $v {
         my $value-str = $v ~~ Blob ?? $v.decode !! $v.map(*.decode);
         note "$k -> $value-str";
      }
   }
}
```

```txt
objectClass -> inetOrgPerson organizationalPerson top person
mail -> gauss@ldap.forumsys.com
uid -> gauss
cn -> Carl Friedrich Gauss
sn -> Gauss

```



## PicoLisp


```PicoLisp
(de ldapsearch (Sn)
   (in
      (list "ldapsearch" "-xH" "ldap://db.debian.org"
         "-b" "dc=debian,dc=org"
         (pack "sn=" Sn) )
      (list
         (cons 'cn (prog (from "cn: ") (line T)))
         (cons 'uid (prog (from "uid: ") (line T))) ) ) )
```

Test:

```txt
: (ldapsearch "Fischer")
-> ((cn . "Mika") (uid . "mf"))
```



## NetRexx

Uses the [http://directory.apache.org/api/ Apache LDAP API], connecting to a local [http://directory.apache.org/apacheds/1.5/ ApacheDS] LDAP directory server.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols binary

import org.apache.directory.ldap.client.api.LdapConnection
import org.apache.directory.ldap.client.api.LdapNetworkConnection
import org.apache.directory.shared.ldap.model.cursor.EntryCursor
import org.apache.directory.shared.ldap.model.entry.Entry
import org.apache.directory.shared.ldap.model.exception.LdapException
import org.apache.directory.shared.ldap.model.message.SearchScope
import org.slf4j.Logger
import org.slf4j.LoggerFactory

class RDirectorySearchLDAP public

  properties constant
    log_ = LoggerFactory.getLogger(RDirectorySearchLDAP.class)

  properties private constant
    ldapHostName = String 'localhost'
    ldapPort = int 11389
    ldapDnStr = String 'uid=admin,ou=system'
    ldapCreds = String '********'

    isTrue = boolean (1 == 1)
    isFalse = boolean (1 \== 1)

  properties private static
    connection = LdapConnection

  method main(args = String[]) public static

    connected = isFalse
    do
      connected = setUp()
      if connected then do
        search('*mil*')
        end

    finally
      if connected then do
        tearDown()
        end
    end

    return

  method search(uid = String '*') static returns boolean

    state      = isTrue
    cursor     = EntryCursor
    baseDn     = 'ou=users,o=mojo'
    filter     = '(&(objectClass=person)(&(uid=' || uid || ')))'
    scope      = SearchScope.SUBTREE
    attributes = String[] [String 'dn', 'cn', 'sn', 'uid']
    do
      if log_.isTraceEnabled() then log_.trace('LDAP search')
      if log_.isInfoEnabled() then do
        log_.info('Begin search')
        log_.info('  search base distinguished name:' baseDn)
        log_.info('  search filter:' filter)
        log_.info('  search attributes:' Arrays.asList(attributes))
        end
      cursor = connection.search(baseDn, filter, scope, attributes)
      loop ksearch = 1 while cursor.next()
        ev = cursor.get()
        if log_.isInfoEnabled() then log_.info('Search cursor entry count:' ksearch)
        if log_.isInfoEnabled() then log_.info(ev.toString())
        end ksearch
    catch lex = LdapException
      state = isFalse
      log_.error('LDAP Error in cursor loop: Iteration' ksearch, Throwable lex)
    catch ex = Exception
      state = isFalse
      log_.error('I/O Error in cursor loop: Iteration' ksearch, Throwable ex)
    end

    return state

  method setUp() static returns boolean

    state = isFalse
    do
      if log_.isInfoEnabled() then log_.info('LDAP Connection to' ldapHostName 'on port' ldapPort)
      connection = LdapNetworkConnection(ldapHostName, ldapPort)

      if log_.isTraceEnabled() then log_.trace('LDAP bind')
      connection.bind(ldapDnStr, ldapCreds)

      state = isTrue
    catch lex = LdapException
      state = isFalse
      log_.error('LDAP Error', Throwable lex)
    catch iox = IOException
      state = isFalse
      log_.error('I/O Error', Throwable iox)
    end

    return state

  method tearDown() static returns boolean

    state = isFalse
    do
      if log_.isTraceEnabled() then log_.trace('LDAP unbind')
      connection.unBind()
      state = isTrue
    catch lex = LdapException
      state = isFalse
      log_.error('LDAP Error', Throwable lex)
    finally
      do
        connection.close()
      catch iox = IOException
        state = isFalse
        log_.error('I/O Error on connection.close()', Throwable iox)
      end
    end

    return state

```

'''Output:'''

```txt

[16:51:37] INFO  [RDirectorySearchLDAP] - LDAP Connection to localhost on port 11389 
[16:51:39] INFO  [RDirectorySearchLDAP] - Begin search 
[16:51:39] INFO  [RDirectorySearchLDAP] -   search base distinguished name: ou=users,o=mojo 
[16:51:39] INFO  [RDirectorySearchLDAP] -   search filter: (&(objectClass=person)(&(uid=*mil*))) 
[16:51:39] INFO  [RDirectorySearchLDAP] -   search attributes: [dn, cn, sn, uid] 
[16:51:39] INFO  [RDirectorySearchLDAP] - Search cursor entry count: 1 
[16:51:39] INFO  [RDirectorySearchLDAP] - Entry 
    dn: cn=John Milton,ou=users,o=mojo 
    uid: jmilton 
    sn: Milton 
    cn: John Milton

```




## PHP

```php
<?php

$l = ldap_connect('ldap.example.com');
ldap_set_option($l, LDAP_OPT_PROTOCOL_VERSION, 3);
ldap_set_option($l, LDAP_OPT_REFERRALS, false);

$bind = ldap_bind($l, 'me@example.com', 'password');

$base = 'dc=example, dc=com';
$criteria = '(&(objectClass=user)(sAMAccountName=username))';
$attributes = array('displayName', 'company');

$search = ldap_search($l, $base, $criteria, $attributes);
$entries = ldap_get_entries($l, $search);

var_dump($entries);
```



## Python

[http://www.python-ldap.org/doc/html/index.html python-ldap Documentation]


```python
import ldap

l = ldap.initialize("ldap://ldap.example.com")
try:
    l.protocol_version = ldap.VERSION3
    l.set_option(ldap.OPT_REFERRALS, 0)

    bind = l.simple_bind_s("me@example.com", "password")
    
    base = "dc=example, dc=com"
    criteria = "(&(objectClass=user)(sAMAccountName=username))"
    attributes = ['displayName', 'company']
    result = l.search_s(base, ldap.SCOPE_SUBTREE, criteria, attributes)

    results = [entry for dn, entry in result if isinstance(entry, dict)]
    print results
finally:
    l.unbind()

```



## REXX

Using LDAP connecting to a local [http://directory.apache.org/apacheds/1.5/ ApacheDS] LDAP directory server.

A little contrived; this [[REXX]] program drives the <tt>ldapsearch</tt> command.

```REXX
/* Rexx */
do
  LDAP_URL        = 'ldap://localhost:11389'
  LDAP_DN_STR     = 'uid=admin,ou=system'
  LDAP_CREDS      = '********'
  LDAP_BASE_DN    = 'ou=users,o=mojo'
  LDAP_SCOPE      = 'sub'
  LDAP_FILTER     = '"(&(objectClass=person)(&(uid=*mil*)))"'
  LDAP_ATTRIBUTES = '"dn" "cn" "sn" "uid"'

  ldapCommand =               ,
    'ldapsearch'              ,
    '-s base'                 ,
    '-H' LDAP_URL             ,
    '-LLL'                    ,
    '-x'                      ,
    '-v'                      ,
    '-s' LDAP_SCOPE           ,
    '-D' LDAP_DN_STR          ,
    '-w' LDAP_CREDS           ,
    '-b' LDAP_BASE_DN         ,
    LDAP_FILTER               ,
    LDAP_ATTRIBUTES           ,
    ''

  say ldapCommand
  address command,
    ldapCommand

  return
end
exit

```

'''Output:'''

```txt

ldapsearch -s base -H ldap://localhost:11389 -LLL -x -v -s sub -D uid=admin,ou=system -w ******** -b ou=users,o=mojo "(&(objectClass=person)(&(uid=*mil*)))" "dn" "cn" "sn" "uid" 
ldap_initialize( ldap://localhost:11389/??base )
filter: (&(objectClass=person)(&(uid=*mil*)))
requesting: dn cn sn uid 
dn: cn=John Milton,ou=users,o=mojo
uid: jmilton
sn: Milton
cn: John Milton


```



## Ruby

Assume AD server talks LDAP.

```ruby
require 'rubygems'
require 'net/ldap'

ldap = Net::LDAP.new(:host => 'hostname', :base => 'base')
ldap.authenticate('bind_dn', 'bind_pass')

filter = Net::LDAP::Filter.pres('objectclass')
filter &= Net::LDAP::Filter.eq('sn','Jackman')
# or
filter = Net::LDAP::Filter.construct('(&(objectclass=*)(sn=Jackman))')

results = ldap.search(:filter => filter)  # returns an array of Net::LDAP::Entry objects

puts results[0][:sn]  # ==> "Jackman"
```



## Run BASIC


```txt
This allows the client on the web to see their directory.
The user can click on any file or directory and this will give them the following options:
 [upload] data from their computer to the server
 [delete] data from their directory 
 [rename] files
 [view]   image files
```


```runbasic
' ---------------------------------------------
' Directory maintenance
' ---------------------------------------------
cr$	= chr$(13)
dirOf$	= "c:\*.*"		' get directory of

' -------------------------------------------
' Shell out directory
' -------------------------------------------

[dirShell]
cls
html "<table bgcolor=lightsteelblue><TR><TD id=wk></TD></TABLE>"
loc$	= strRep$(dirOf$,"*.*","")
x$	= shell$("dir ";dirOf$)

i 	= 1
while word$(x$,i,cr$) <> ""
	a$	= word$(x$,i,cr$)
	if trim$(a$)   = ""	then goto [next]
	if left$(a$,1) = " "	then goto [next]
	if left$(a$,1) = cr$	then goto [next]
	type$	= mid$(a$,26,3)
	size$	= mid$(a$,30,9)
	size$	= strRep$(size$,",","")
	size	= val(size$)
	if type$ <> "DIR" and size = 0 then goto [next]
	name$	= mid$(a$,40)
	a$	= strRep$(a$,"<","[")
	a$	= strRep$(a$,">","]")
	html left$(a$,39)
	link #ddir,name$, [doDir]
	     #ddir setkey(type$;"|";loc$;name$)
	html "<BR>"
	goto [next1]
     [next]
	print a$
     [next1]
	i = i + 1
wend
wait
[doDir]
type$	= word$(EventKey$,1,"|")
name$	= word$(EventKey$,2,"|")

if type$ = "DIR" then 
	dirOf$ = name$;"\*.*"
	goto [dirShell]
end if

html "<script> document.getElementById('wk').innerHTML = '"
nname$	= strRep$(name$,"\","\\")
html "What do you want to do with ";nname$;"<BR>"
button #dofile,	"Upload",[upload]
button #dofile,	"Delete",[delete]
button #rename, "Rename",[rename]
button #view, 	"View",	 [view]
html "';</script>"
wait

[delete] 
 kill name$
 goto [dirShell]

[view]
nname$ = strRep$(name$,"\","/")
print "File:";nname$
nname$ = mid$(nname$,3)
html "<EMBED SRC=""..";nname$;""">"
print "<EMBED SRC=""..";nname$;""">"

wait

[upload]
print "Upload File:";name$
files  #f, name$
if #f HASANSWER() = 0 then
	print "File: ";name$;" not found"
end if

' -------------------------------------
' load data to directory
' -------------------------------------
OPEN name$ FOR binary AS #f
filedata$  = input$(#f, LOF(#f))
CLOSE #f
print filedata$
wait

f$	= photoDir$;uploadId$
OPEN f$ FOR binary AS #f
PRINT  	#f, filedata$
CLOSE  	#f
wait

' --------------------------------
' string replace rep str with
' --------------------------------
FUNCTION strRep$(strRep$,rep$,with$)
ln	= len(rep$)
k	= instr(strRep$,rep$)
while k
	strRep$	= left$(strRep$,k - 1) + with$ + mid$(strRep$,k + ln)
	k	= instr(strRep$,rep$)
WEND
END FUNCTION
end
```

Output as seen by the client on the web
```txt

Volume in drive C has no label.
Volume Serial Number is F42C-D87A

 Directory of c:\
06/10/2009  02:42 PM                24 autoexec.bat
06/10/2009  02:42 PM                10 config.sys
01/30/2012  02:26 PM               206 csb.log
03/09/2012  10:00 AM    [DIR]          data
02/07/2012  07:48 AM       748,990,464 precise-desktop-i386.iso
03/20/2012  04:07 PM    [DIR]          Program Files
02/05/2012  05:09 PM    [DIR]          Python
03/19/2012  04:55 PM    [DIR]          rbp101
01/30/2012  02:26 PM             3,081 RHDSetup.log
01/30/2012  10:14 PM    [DIR]          Users
01/30/2012  02:35 PM    [DIR]          wamp
03/06/2012  04:00 AM    [DIR]          Windows
               5 File(s)    748,993,785 bytes
               7 Dir(s) 952,183,820,288 bytes free
```



## Scala


```Scala
import org.apache.directory.api.ldap.model.message.SearchScope
import org.apache.directory.ldap.client.api.{LdapConnection, LdapNetworkConnection}

object LdapSearchDemo extends App {

  class LdapSearch {

    def demonstrateSearch(): Unit = {

      val conn = new LdapNetworkConnection("localhost", 11389)
      try {
        conn.bind("uid=admin,ou=system", "********")
        search(conn, "*mil*")
        conn.unBind()
      } finally if (conn != null) conn.close()

    }

    private def search(connection: LdapConnection, uid: String): Unit = {
      val baseDn = "ou=users,o=mojo"
      val filter = "(&(objectClass=person)(&(uid=" + uid + ")))"
      val scope = SearchScope.SUBTREE
      val attributes = List("dn", "cn", "sn", "uid")
      var ksearch = 0
      val cursor = connection.search(baseDn, filter, scope, attributes: _*)
      while (cursor.next) {
        ksearch += 1
        val entry = cursor.get
        printf("Search entry %d = %s%n", ksearch, entry)
      }
    }
  }

  new LdapSearch().demonstrateSearch()

}
```


## Tcl

One can do it with the low level [[Connect to Active Directory]] based handle with this code:

This is just the basic setup.

```tcl
set Username "TestUser"
set Filter "((&objectClass=*)(sAMAccountName=$Username))"
set Base "dc=skycityauckland,dc=sceg,dc=com"
set Attrs distinguishedName
```


Now do the actual search.

```tcl
set result [ldap::search $conn $Base $Filter $Attrs -scope subtree]
```

 
If we have only a single result its easy:

```tcl
if {[llength $result] == 1} {
    puts [dict get [lindex $result 0 1] distinguishedName]
}
```


Looping over the result set to output some values.

```tcl
foreach pair $result {
    lassign $pair cn attributes
    puts [dict get $attributes distinguishedName]
}
```


If you're bored you can also use this instead:

```tcl
package require ldapx
set conn [ldapx::connect $BindDN $Password]
$conn traverse $Base $Filter $Attrs e {
    puts [$e get distinguishedName]
}
```



## UNIX Shell

Using LDAP connecting to a local [http://directory.apache.org/apacheds/1.5/ ApacheDS] LDAP directory server.

A shell script to drive the <tt>ldapsearch</tt> command.

```bash
#!/bin/sh

LDAP_HOST="localhost"
LDAP_PORT=11389
LDAP_DN_STR="uid=admin,ou=system"
LDAP_CREDS="********"
LDAP_BASE_DN="ou=users,o=mojo"
LDAP_SCOPE="sub"
LDAP_FILTER="(&(objectClass=person)(&(uid=*mil*)))"
LDAP_ATTRIBUTES="dn cn sn uid"

ldapsearch \
  -s base \
  -h $LDAP_HOST \
  -p $LDAP_PORT \
  -LLL \
  -x \
  -v \
  -s $LDAP_SCOPE \
  -D $LDAP_DN_STR \
  -w $LDAP_CREDS \
  -b $LDAP_BASE_DN \
  $LDAP_FILTER \
  $LDAP_ATTRIBUTES

```

'''Output:'''

```txt

ldap_initialize( ldap://localhost:11389 )
filter: (&(objectClass=person)(&(uid=*mil*)))
requesting: dn cn sn uid 
dn: cn=John Milton,ou=users,o=mojo
uid: jmilton
sn: Milton
cn: John Milton


```



## VBScript

The search string and execution of the string

```vbscript
strUsername = "TestUser"
strQuery = "<LDAP://dc=skycityauckland,dc=sceg,dc=com>;"_
 & "(&(objectclass=*)(samaccountname=" & strUsername & "));distinguishedname;subtree"
objCmd.ActiveConnection = objConn
objCmd.Properties("Page Size")=100
objCmd.CommandText = strQuery
Set objRS = objCmd.Execute
```


Doing something with a single result (this will output the returned users full DN)

```vbscript
If objRS.RecordCount = 1 Then
  WScript.Echo objRS.Fields("DistinguishedName")
End If
```


Doing something with multiple results (this will output each returned users full DN)

```vbscript>If objRS.RecordCount 
 0 Then
  For Each objUser in ObjRS
    WScript.Echo objRS.Fields("DistinguishedName")
  Next
End If
```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have network access. -->
