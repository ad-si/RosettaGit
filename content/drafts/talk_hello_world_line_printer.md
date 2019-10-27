+++
title = "Talk:Hello world/Line printer"
description = ""
date = 2011-04-26T13:44:23Z
aliases = []
[extra]
id = 8397
[taxonomies]
categories = []
tags = []
+++

== Task definition ==

Based on the given solutions, I've done a quick definition of what the task is about. Feel free to revise if you believe I've not captured it correctly. –[[User:Dkf|Donal Fellows]] 07:22, 3 October 2010 (UTC)

== Is a line printer separate from some other way to print? ==

Java

public class Print
{
   public static void main(String[] args)
   {
       System.out.println("Hello World");
   }
}
:A [[wp:Line_printer|Line Printer]] is a 'older' printer that uses a continuous paper-feed instead of single sheets and normally a stance (types/needles/chain) that physically strikes the paper when printing. Printing a single text line to it (and then closing the device) only prints that line and does not add an LF/FF etc. if not specifically added to the stream, e.g. the next print that is made would then end up on the next line on the same paper. 
:This is a several very desirable aspects for line printers; 
::(A) for ex. alarm-printer in the industries as the alarm list after any incident can be checked and verified line by line on one continuous paper and thereby reducing the risk of anyone removing/tampering with the printouts. 
::(B) they are often much more economical than lasers etc. and therefore used for large printing volumes where the finish is less of a concern.
:If trying to print as to a 'modern' device the output would risk to come out as a single line per sheet. :--[[User:Jofur|&lt;Jofur&gt;]] 12:14, 26 April 2011 (UTC)

Line printers utilize ASCII (or occasionally EBCDIC on some mainframe variants.), Sending the characters to the word "Hello" down to a line printer will produce the word "Hello" on the page (following an appropriate line end or page throw sequence).

This is different to page printers, which require a driver to convert the text into a printer specific control language (such as PCL, Postscript, or other proprietary printer specific control code sequence.)
[[User:Markhobley|Markhobley]] 13:44, 26 April 2011 (UTC)
