+++
title = "Talk:Vigenère cipher"
description = ""
date = 2012-03-03T03:35:55Z
aliases = []
[extra]
id = 9847
[taxonomies]
categories = []
tags = []
+++

There's four examples here, and no one seems to be having any trouble with the task description, so I'm going to remove the draft status from this one. Feel free to put it back if there are any objections. [[User:MagiMaster|MagiMaster]] 03:25, 2 June 2011 (UTC)

== A cool trick ==

It occurred to me that decoding is encoding with a different key. Specifically, the key can be modified by subtracting its letters' indices from 26. So a key of ABC will become ZYX. Then, encode the ciphertext with that new key to decode it. Here's some pseudo-code:
<code>
 text = 'Hello'
 key = 'ABC'
 ciphertext = VigenereCipher(text, key)
 comment ^ the ciphertext will be 'Hfnlp'
 
 comment Now to decode:
 For each character in key:
    alpha = ASCII_value_of_char(character)-65
    newAlpha = 26 - alpha
    newCharacter = Char_from_ASCII_value(newAlpha)
    append newCharacter to newKey
 
 decoded = VigenereCipher(ciphertext, newKey)</code>
This can shorten the AutoHotkey decipher function to 3 lines, and I'm pretty sure Python could pull off a one-liner using List Comprehensions.
