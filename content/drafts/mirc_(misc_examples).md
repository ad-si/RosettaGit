+++
title = "MIRC (misc examples)"
description = ""
date = 2010-01-29T22:37:50Z
aliases = []
[extra]
id = 1572
[taxonomies]
categories = []
tags = []
+++

These [[mIRC]] code snippets are not associated with any programming task.

=MArray Snippet=
{{Lines_too_long}}
'''mIRC Array Snippet'''
----

  /*_____________________________________________________________________________
  |/
  /  Array snippet by Haso "sm0kie_" Keric <Osah@comcast.net>
  |  Version 0.1, released 11/06 -- support on #script/irc.gamesurge.net
  |  Use/modify however you want, but please keep my name in it. Thank you!
  |
  |  Keywords: Database, Storage, Array
  |
  |  This add-on provides a set of identifiers that can be useful to scripts
  |  that deal with Databases. And methods to store things. 
  |
  |  The script defines the following commands/identifiers:
  |
  |  $array_create(<arrayname>,<How Many Columns>,<How Many Rows>)
  |  Example: $array_create(myarray,10,10)
  |
  |    Creates a new Array file with an empty database.
  |
  |  $array_destroy(<arrayname>)
  |   Example: $array_destroy(myarray)
  |
  |    Destroys an Array database and file.
  |
  |  $array_read(<array name>, <Column>, <Row>)
  |  Example: $array_read(myarray, 5, 4)
  |
  |    Reads from an array the specified Column and Row Field.
  |
  |  $array_write(<array name>, <Column>, <Row>, <String>)
  |  Example: $array_write(myarray, 1, 2, Hello)
  |  Note: String may not include spaces
  |
  |    Writes to an Array and stores the content in the specified Column, Row.
  |
  |  Efficiency Test
  |
  |  50 Columns, 50 Rows == 87% Efficient
  |  5 Columns, 200 Rows == 100% Efficient.
  |  The less Columns, the more efficient will the readings be.
  |
  \
  _\_____________________________________________________________________________
  */
  alias -l array_version return 0.1 Osah-Framework.
  alias -l array_extension return .ray
  alias -l array_url return www.hasokeric.com
  alias -l array_dir return $mircdir
  alias -l array.f return $array_dir $+ $1 $+ $array_extension
  alias -l array.rows return $lines($array.f($1))
  alias -l array.new .timer_array $3 0 write $array_dir $+ $1 $str(NUL $+ $chr(32),$2)
  alias -l array.read return $read($array.f($1),$calc($3 +1))
  alias array_write write -l $+ $calc($3 +1) $array.f($1) $puttok($array.read($1,$2,$3),$4,$2,32) | return * Array changed
  alias array_create {
  if ($2 == $null) || ($3 == $null)  { return * Error, Not enough parameters. }
  if ($2 > 100)  { return * Error, Too many columns. > 70 | halt } | if ($file($1 $+ $array_extension) != $null)  { return * Error, Array file exists. | halt }
  write $1 $+ $array_extension $ctime $+ @ $+ $2  $+ : $+ $3 $+ @OSAHFRAMEWORK | array.new $1 $+ $array_extension $2 $3 | return * Created Array $1
  }
  alias array_destroy if ($file($array.f($1)))  { remove $array.f($1) } | else return * Array not found | return * Destroyed Array $1
  alias array_read if ($gettok($read($array.f($1),$calc($3 +1)),$2,32)) { return $gettok($read($array.f($1),$calc($3 +1)),$2,32) } | else return * Nothing stored at specified Column and Row
