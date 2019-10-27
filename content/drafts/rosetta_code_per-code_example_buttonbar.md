+++
title = "Rosetta Code:Per-Code Example Buttonbar"
description = ""
date = 2010-07-16T20:06:30Z
aliases = []
[extra]
id = 7660
[taxonomies]
categories = []
tags = []
+++

//
```javascript

/*
### ==================================

This comparison script was written by
Tyrok1, and has been tested on IE 7,
IE 8, Fx 3.6.3, and Fe 5.0.380.

### ==================================
*/

//define the relationship between the CSS classes and the language names
var buttonBarCodepadLanguages = [
					{ cssClass: "c", padName: "C" },
					{ cssClass: "cpp", padName: "C++" },
					{ cssClass: "d", padName: "D" },
					{ cssClass: "haskell", padName: "Haskell" },
					{ cssClass: "lua", padName: "Lua" },
					{ cssClass: "ocaml", padName: "OCaml" },
					{ cssClass: "php", padName: "PHP" },
					{ cssClass: "perl", padName: "Perl" },
					{ cssClass: "python", padName: "Python" },
					{ cssClass: "ruby", padName: "Ruby" },
					{ cssClass: "scheme", padName: "Scheme" },
					{ cssClass: "tcl", padName: "Tcl" }
				];

function AddHandler(el, handlerType, func)
{
	//add an event handler in a more cross-browser way
	if(el.addEventListener)
	{
		el.addEventListener(handlerType, func, false);
	}
	else if(el.attachEvent)
	{
		el.attachEvent("on" + handlerType, func);
	}
	else
	{
		eval("el.on" + handlerType + " = func;");
	}
}

function IsChild(parent, child)
{
	//check to see if an element is a child of another element
	while(child && child != parent)
	{
		child = child.parentNode;
	}
	return (child == parent);
}

function ButtonBarSelectAll(preEl)
{
	//select all code within a given 
```txt
 element
	var range = null;
	var menuEl = preEl.getElementsByTagName("ul")[0];
	if(((document.createRange && (range = document.createRange())) || (document.selection.createRange && (range = document.selection.createRange()))) && range.selectNodeContents)
	{
		//for Fx, Fe, and other more standards-compliant browsers
		range.selectNodeContents(preEl);
		range.setEndBefore(menuEl);
		var sel = window.getSelection();
		if(sel.setBaseAndExtent)
		{
			//odd variant of a few of the WebKit browsers
			sel.setBaseAndExtent(preEl, 0, menuEl, 0);
		}
		else
		{
			sel.removeAllRanges();
			sel.addRange(range);
		}
	}
	else
	{
		//for IE
		range = document.body.createTextRange();
		range.moveToElementText(preEl);
		var menuRange = document.body.createTextRange();
		menuRange.moveToElementText(menuEl);
		
		//deselect the popup menu
		while(range.compareEndPoints("EndToStart", menuRange) > 0)
		{
			range.moveEnd("word", -1);
		}
		range.select();
	}
	
	//return the range so other functions can use it
	return range;
}

function ButtonBarCreateSelectAll(preEl)
{
	return function() {
				//select all of the text in the pre element
				ButtonBarSelectAll(preEl);
			};
}

function ButtonBarCreateCopy(preEl)
{
	return function() {
				//copy all of the text in the pre element
				//IE-only at this point
				var range = ButtonBarSelectAll(preEl);
				range.execCommand("Copy");
			};
}

function ButtonBarGetText(preEl)
{
	//get the code from inside of a 
```txt
 element
	var outText = "";
	for(var c = 0; c < preEl.childNodes.length; ++c)
	{
		switch(preEl.childNodes[c].tagName ? preEl.childNodes[c].tagName.toLowerCase() : "")
		{
			case "br":
			{
				outText += "\n";
			} break;
			case "ul":
			{
				//get rid of  s
				outText = outText.replace(/\xA0/g, " ");
				
				//return the cleaned text
				return outText;
			} break;
			case "":
			{
				//no tag name
				//probably just text
				outText += (preEl.childNodes[c].textContent ? preEl.childNodes[c].textContent : (preEl.childNodes[c].innerText ? preEl.childNodes[c].innerText : preEl.childNodes[c].data));
			} break;
			default:
			{
				outText += ButtonBarGetText(preEl.childNodes[c]);
			}
		}
	}
	
	//get rid of  s
	outText = outText.replace(/\xA0/g, " ");
	
	//return the cleaned text
	return outText;
}

function ButtonBarAddSpacer(ulEl)
{
	//add a spacer bullet item to the menu list
	var spacerEl = ulEl.appendChild(document.createElement("li"));
	spacerEl.appendChild(document.createTextNode("\u2022"));
	spacerEl.style.display = "inline";
	spacerEl.style.marginLeft = "0.5em";
	spacerEl.style.marginRight = "0.5em";
}

function ButtonBarCreateMouseOver(preEl)
{
	//the mouseover event handler for 
```txt
 elements
	return function() {
				//if there's already a menu, don't add another
				if(preEl.getElementsByTagName("ul").length > 0)
				{
					return false;
				}
				
				//style the 
```txt
 and add the menu
				preEl.style.position = "relative";
				var ulEl = preEl.appendChild(document.createElement("ul"));
				ulEl.className = "ButtonBar";
				ulEl.style.position = "absolute";
				ulEl.style.right = 0;
				ulEl.style.top = 0;
				ulEl.style.whitespace = "nowrap";
				ulEl.style.marginRight = "0.5em";
				
				//define actions we want to add
				var listItems = [
								{title:"Select All",code:ButtonBarCreateSelectAll(preEl)}
							];
				if(window.clipboardData)
				{
					listItems[listItems.length] = {title:"Copy to Clipboard",code:ButtonBarCreateCopy(preEl)};
				}
				
				//add the standard menu items
				for(var i = 0; i < listItems.length; ++i)
				{
					//if it's not the first one, add a spacer
					if(i > 0)
					{
						ButtonBarAddSpacer(ulEl);
					}
					var liEl = ulEl.appendChild(document.createElement("li"));
					liEl.appendChild(document.createTextNode(listItems[i].title));
					liEl.style.cursor = "pointer";
					liEl.style.display = "inline";
					liEl.style.fontFamily = "sans-serif";
					liEl.style.textDecoration = "underline";
					AddHandler(liEl, "click", listItems[i].code);
				}
				
				//check for a codepad-supported language
				var langClass = preEl.className.split(" ")[0];
				for(var l = 0; l < buttonBarCodepadLanguages.length; ++l)
				{
					if(langClass == buttonBarCodepadLanguages[l].cssClass)
					{
						//found the language
						//add <li>s
						ButtonBarAddSpacer(ulEl);
						var liEl = ulEl.appendChild(document.createElement("li"));
						liEl.style.display = "inline";
						
						//build a form to submit to codepad
						var formEl = liEl.appendChild(document.createElement("form"));
						formEl.target = "_blank";
						formEl.method = "post";
						formEl.action = "http://codepad.org";
						formEl.style.display = "inline";
						var inputValues = [
									{ name: "lang", value: buttonBarCodepadLanguages[l].padName },
									{ name: "code", value: ButtonBarGetText(preEl) },
									{ name: "run", value: "True" },
									{ name: "private", value: "True" }
								];
						for(var i = 0; i < inputValues.length; ++i)
						{
							var inputEl = document.createElement("input");
							inputEl.setAttribute("type", "hidden");
							inputEl.setAttribute("name", inputValues[i].name);
							inputEl.setAttribute("value", inputValues[i].value);
							formEl.appendChild(inputEl);
						}
						
						//add and style the submit button to look like the other items
						var inputEl = document.createElement("input");
						inputEl.setAttribute("type", "submit");
						inputEl.setAttribute("name", "submit");
						inputEl.setAttribute("value", "Try on Codepad");
						inputEl.style.border = "none";
						inputEl.style.background = "transparent";
						inputEl.style.cursor = "pointer";
						inputEl.style.textDecoration = "underline";
						inputEl.style.display = "inline-block";
						inputEl.style.padding = 0;
						formEl.appendChild(inputEl);
						break;
					}
				}
				return false;
			};
}

function ButtonBarCreateMouseOut(preEl)
{
	//mouseout handler
	return function(e) {
			//if we're moving to another element inside of the same 
```txt
, add a mouseout handler and return
			if(!e) e = window.event;
			var target = e.relatedTarget || e.toElement;
			if(IsChild(preEl, target))
			{
				AddHandler(target, "mouseout", ButtonBarCreateMouseOut);
				return false;
			}
			
			//we're moving away from the container 
```txt
, so we need to remove all menu lists
			var ulEls = preEl.getElementsByTagName("ul");
			for(var u = 0; u < ulEls.length; ++u)
			{
				ulEls[u].parentNode.removeChild(ulEls[u]);
			}
			return false;
		};
}

function ButtonBarActivate()
{
	//check to see if we're looking at a task page
	var catLinksEl = document.getElementById("catlinks");
	var isTask = false;
	if(catLinksEl)
	{
		var aEls = catLinksEl.getElementsByTagName("a");
		for(var a = 0; a < aEls.length; ++a)
		{
			if(aEls[a].getAttribute("title") == "Category:Programming Tasks")
			{
				//it's a task
				isTask = true;
				break;
			}
		}
	}
	if(!isTask)
	{
		return;
	}
	
	//look for language examples
	var allPres = document.getElementsByTagName("pre");
	for(var p = 0; p < allPres.length; ++p)
	{
		if(allPres[p].className && allPres[p].className.indexOf("highlighted_source") >= 0)
		{
			//set up the onmouseover, onmousleave events to show/hide the button bar
			AddHandler(allPres[p], "mouseover", ButtonBarCreateMouseOver(allPres[p]));
			AddHandler(allPres[p], "mouseout", ButtonBarCreateMouseOut(allPres[p]));
		}
	}
}

//register the comparison script with the window's load event
AddHandler(window, "load", ButtonBarActivate);

//
```

