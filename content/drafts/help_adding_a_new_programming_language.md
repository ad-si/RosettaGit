+++
title = "Help:Adding a new programming language"
description = ""
date = 2009-09-03T14:09:34Z
aliases = []
[extra]
id = 1566
[taxonomies]
categories = []
tags = []
+++

If you're familiar with the language, adding a new programming language is simple enough.

## Programming Example

You should probably start by examining the existing [[:Category:Programming Tasks|programming tasks]], and finding one that you can complete with your language of choice. Good starter examples are [[Empty Program]], [[Comments]], [[User Output]], and those in [[:Category:Control Structures]] and [[:Category:Basic Data Operations]].
See [[:Help:Adding a new programming example]] for details.


## Language Page

Rosetta code uses the Category namespace for describing programming languages, with a normal page redirecting to it for more convenient links.

If a programming example already exists for the language, creating a language page is simple. Just click on the red link to the language, and click "Create this article" on the resulting page and add at least this boilerplate:


```txt
<nowiki>
{{language}}
Short description of your language.
</nowiki>
```


Your language description should briefly point out unique features of the language and provide links to external sites for more information or downloads. You can also provide wiki links to other languages or other wiki pages for cross-reference. If you have little to say, please prepend the text <code><nowiki>{{stub}}</nowiki></code> to let us know more information needs to be added. There are also other options that can be added to the language template to specify features of the language. See [[Template:Language]] for usage instructions. You may also specify programming paradigms used by this language by adding <nowiki>{{language programming paradigm|paradigm name}}</nowiki>. See [[:Category:Programming paradigm]] for paradigm options. Now click the "Save" button.

Now hit "Save" again. Go back to the example page and reload. Check that clicking the header link for your language takes you to the category page you just created, and that the example shows up in a section titled '''Articles in category "My Language"'''.


## Conclusion

Thanks for showing an interest in adding information to Rosetta Code.
With the help of people like you, Rosetta Code will become a true programmer's resource!
