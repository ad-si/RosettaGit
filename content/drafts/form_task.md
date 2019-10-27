+++
title = "Form:Task"
description = ""
date = 2010-10-21T21:42:25Z
aliases = []
[extra]
id = 8550
[taxonomies]
categories = []
tags = []
+++

<noinclude>
This is the "Task" form.
To create a page with this form, enter the page name below;
if a page with that name already exists, you will be sent to a form to edit that page.


{{#forminput:form=Task}}

</noinclude><includeonly>
<div id="wikiPreview" style="display: none; padding-bottom: 25px; margin-bottom: 25px; border-bottom: 1px solid #AAAAAA;"></div>
{{{for template|Task/realazthat}}}
{| class="formtable"
! Description:
| {{{field|description|input type=textarea|mandatory}}}
|-
| {{{field|solutions|hidden|input type=textarea}}}
|}
{{{end template}}}

'''Free text:'''

{{{standard input|free text|rows=10}}}


{{{standard input|summary}}}

{{{standard input|minor edit}}} {{{standard input|watch}}}

{{{standard input|save}}} {{{standard input|preview}}} {{{standard input|changes}}} {{{standard input|cancel}}}
</includeonly>
