+++
title = "Talk:Bitmap/Write a PPM file"
description = ""
date = 2013-12-16T17:37:36Z
aliases = []
[extra]
id = 12667
[taxonomies]
categories = []
tags = []
+++

'''Haskell code wrong?'''
Binary PPM files should have the header "P6", and PPM files in plain ASCII should have the header "P3". However, the Haskell code reads a file in ASCII but checks if its header is "P6", and complains if the file data is raw bytes. That should be corrected.

Code in C# is wrong. using Environment.NewLine will cause the data section to be shifted in a way that the first few RGBRGBRGB... will be lost, and what will be written actually starts with "G", so the output data section will be GBR instead of RGB. this is because Environment.NewLine will write CR+LF (Windows) which is not a single whitespace as the PPM specification says, but in fact two whitespaces.
if Environment.NewLine is substituted with " " (Space), it works.
