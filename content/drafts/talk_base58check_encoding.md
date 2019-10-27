+++
title = "Talk:Base58Check encoding"
description = ""
date = 2018-12-11T14:17:41Z
aliases = []
[extra]
id = 21663
[taxonomies]
categories = []
tags = []
+++

==prepend leading zeros when the hash digest number has leading ones==
Am I reading the   ''reference algorithm''   (the Base58Check encoding at the bitcoin wiki webpage) correctly in regarding to leading ones (1's) in the (decimal version) hash digest?

In particular, if there are (say) three leading ones in the hash digest, the output string should have three leading zeros. 

All of the programming entries so far (except for the REXX version 3 entry) do not address this part of the   ''reference algorithm''   (as shown in the pseudo-code)   at   [https://en.bitcoin.it/wiki/Base58Check_encoding#Base58_symbol_chart the Bitcoin's Base58Check page].   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:23, 17 November 2017 (UTC)

-----

== Code examples are NOT Base58Check. ==

They are more normal Base58 examples. Base58Check INCLUDES setting a version byte BEFORE anything else, double-SHA256-ing the concatenated hex string and putting the first 4 bytes of the result behind the hex string, then convert the whole thing to Base58.<br /><br />
For example:<br />
Let the input be following hex: 000102030405060708090a0b0c0d0e0f<br />
Put a version byte before it, for example 80. You get '''80'''000102030405060708090a0b0c0d0e0f<br />
Now do a SHA256. You get 4d00ec9820c958f2d198ccc75f6682f7876228eea3ca3bd68c851302438151cb<br />
Now do another SHA256 of the previous SHA256 result. You get 5ffc3b4fca2a220cc9c19c41dfd7248af608f50b188efcbbfa2ae3df96643ad0<br />
Now take the first 4 bytes from this and append them to the hex with the version byte as following: 80000102030405060708090a0b0c0d0e0f'''5ffc3b4f'''<br />
This will now be converted to Base58: 8sWmykwPRCRjGtuBczJSJTysdzuaW<br /><br />
This is the whole Base58Check process. It actually works with any hex of any length.<br />
In case of Bitcoin, those hex strings are 32 bytes for private keys and 20 bytes for addresses (not public keys). Also, version bytes are 80 for private keys and 00 for P2PKH addresses (05 for P2SH and SegWit nested in P2SH). Every leading 00 must be denoted with an 1 in the Base58 string.<br />There can also be more then one version byte. Extended keys in Bitcoin (HD wallets) use 4 version bytes. -- [[User:Stl1988|Stl1988]] ([[User talk:Stl1988|talk]]) 07:28, 11 December 2018 (UTC)
:As I see it there are three tasks on rosettacode: this one, [[Bitcoin/address_validation]], and [[Bitcoin/public_point_to_address]] with the last one doing (almost) what you suggest. This one corresponds to the third option on (say) [[http://lenschulwitz.com/base58]] and should ''not'' be using sha256, or inserting a version byte. Either that or we should not have three quite-so-similar tasks. I agree that leading zeroes/"1"s are not properly handled by most entries though. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 11:05, 11 December 2018 (UTC)
::If this should only show the Base58 encoding, it should be called Base58, not Base58Check, because there's a difference between these two. Base58 is just the encoding while Base58Check is the process I described above.<br />
::It would be awesome to have a code that enables you to manually enter the version byte and doing the whole Base58Check for any hex string. We already have something doing this on [[https://brainwalletx.github.io/#converter]], but it doesn't allow you to manually enter a version byte. -- [[User:Stl1988|Stl1988]] ([[User talk:Stl1988|talk]]) 14:16, 11 December 2018 (UTC)
