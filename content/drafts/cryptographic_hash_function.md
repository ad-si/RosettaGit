+++
title = "Cryptographic hash function"
description = ""
date = 2017-01-22T14:20:58Z
aliases = []
[extra]
id = 11617
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]
A [[wp:cryptographic hash function|cryptographic hash function]], also known as a ''collision resistant hash function'', maps a message (of any length) to a digest (of a fixed length), and has the following three properties:<ref>Dobbertin, Bosselaers, Preneel. [http://homes.esat.kuleuven.be/~bosselae/ripemd160.html "RIPEMD-160, a strengthened version of RIPEMD"]</ref><ref>
[https://en.wikibooks.org/wiki/Cryptography/Hashes Wikibooks: Cryptography: Hashes]
</ref>

* ''Preimage resistance.'' It is too difficult to invert the function and find an original message from its digest.
* ''Second preimage resistance.'' It is too difficult to change the message without also changing its digest.
* ''Collision resistance.'' It is too difficult to find any two messages with the same digest.

With many algorithms, the message is a string of [[octet]]s, and the digest has a fixed number of bits. For example, SHA-256 always computes a 256-bit digest. One can write this digest in hexadecimal.

Though algorithms like [[MD4]] define digest for message of any bit length, many actual implementations assume that the bit length is a multiple of 8.

== Algorithms ==
[[SHA-256]] is the recommended stronger alternative to [[SHA-1]] and [[RIPEMD-160]], which in turn are stronger than [[MD5]], which in turn is stronger than [[MD4]].

== References ==
<references />
