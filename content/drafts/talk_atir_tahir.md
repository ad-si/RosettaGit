+++
title = "Talk:Atir Tahir"
description = ""
date = 2019-09-19T13:54:30Z
aliases = []
[extra]
id = 22550
[taxonomies]
categories = []
tags = []
+++

== Too much like an ad, not enough like a task ==


Rosetta Code is not an advertising platform for a commercial service. I didn't delete this outright since it ''could'' be turned into a task, but as it stands, it reads too much like an ad.  --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 13:53, 19 September 2019 (UTC)


== Secure your documents programmatically ==

Things are going '''digital'''. There are outstanding products/tools to pull you into the paperless era. We share information across the companies or anywhere in the form of different file formats. It could be a Spreadsheet, PDF or any other MS Office formats. But the biggest threat is security. Let's understand this with a use-case.

If you have a Word file or PDF and you want to secure it with a password before sharing it to the team, what will you do? Of course, you will protect it using MS Word (using a built-in feature). '''But what if you don't have MS Office or Adobe PDF installed on your machine?'''

There are few APIs/tools that provide facility to secure a lot of file formats. Have a look at the GroupDocs.Merger APIs for Java and .NET platform:

'''.NET Code Snippet'''

```c#
string sourceFile = (@"D:/Data/sample.pdf");
string password = "iamironman";
Stream openFile = new FileStream(sourceFile, FileMode.Open);
DocumentResult result = new DocumentHandler().AddPassword(openFile, password);
Stream documentStream = result.Stream;
var fileStream = File.Create(@"D:/Data/Output.pdf");
documentStream.CopyTo(fileStream);
documentStream.Close();
```


'''Java Code Snippet'''

```java
String sourceFile = "source file";
String password = "spidy";
AddPasswordOptions options = new AddPasswordOptions(FileFormat.Docx, password);
InputStream documentExample = new FileInputStream(sourceFile);
DocumentResult result = new DocumentHandler().addPassword(documentExample, options);
OutputStream documentStream = result.getStream();
ByteArrayOutputStream byteArrayStream = (ByteArrayOutputStream) documentStream;
byte[] bytes = byteArrayStream.toByteArray();
FileOutputStream fos = new FileOutputStream(CommonUtilities.outputPath + fileName);
fos.write(bytes, 0, bytes.length);
fos.close();
```

