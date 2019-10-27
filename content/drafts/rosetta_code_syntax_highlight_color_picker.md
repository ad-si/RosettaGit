+++
title = "Rosetta Code:Syntax Highlight Color Picker"
description = ""
date = 2011-07-03T19:30:38Z
aliases = []
[extra]
id = 10016
[taxonomies]
categories = []
tags = []
+++

//
```javascript

/*
### ==================================

This comparison script was written by
Tyrok1 and has been tested in Cr 12 and
Fx 5.

### ==================================
*/

//define the relationship between the CSS classes and what they do
var highlightClasses = [
					{ cssBase: "co", description: "Comments" },
					{ cssBase: "nu", description: "Numbers" },
					{ cssBase: "kw", description: "Keywords" },
					{ cssBase: "sy", description: "Operators" },
					{ cssBase: "st", description: "Strings" },
					{ cssBase: "re", description:
						"Variables/Erlang Funs" },
					{ cssBase: "br", description: "Brackets/Parens" },
					{ cssBase: "co", description:
						"Comments/Compiler directives" },
					{ cssBase: "me", description: "Class methods" }
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

function HighlightCode(codeEl)
{
	//find all instances of elements with the classes in a given
	//code block and update their colors
	var allSpans = codeEl.getElementsByTagName("span");
	for(var s = 0; s < allSpans.length; ++s)
	{
		if(allSpans[s].className)
		{
			for(var c in highlightClasses)
			{
				var cssBase = allSpans[s].className
					.substring(0, highlightClasses[c].cssBase.length);
				if(cssBase == highlightClasses[c].cssBase)
				{
					var colorEl = document.getElementById(
						"HighlightColor" + c);
					var dropdownVal = colorEl.options[
						colorEl.selectedIndex].value;
					allSpans[s].style.color = 
						(dropdownVal == "auto" ? null : dropdownVal);
				}				
			}
		}
	}
}

function HighlightUpdate()
{
	//look for language examples
	var allPres = document.getElementsByTagName("pre");
	for(var p = 0; p < allPres.length; ++p)
	{
		if(allPres[p].className && 
			allPres[p].className.indexOf("highlighted_source") >= 0)
		{
			HighlightCode(allPres[p]);
		}
	}
	
	//save defaults to cookie
	var defaults = "";
	for(var c in highlightClasses)
	{
		var colorEl = document.getElementById("HighlightColor" + c);
		defaults += (defaults == "" ? "" : ",") + 
			highlightClasses[c].cssBase + "-" + 
			colorEl.options[colorEl.selectedIndex].value;
	}
	document.cookie = ("highlightDefaults=" + defaults + 
		"; expires=" + (new Date((new Date()).getTime() + 
		1000 * 60 * 60 * 24 * 365)).toGMTString() + "; path=/");
}

function HighlightAddToolbox()
{
	//check the cookie for default colors from last time
	var cookies = document.cookie.split(";");
	for(var c in cookies)
	{
		var nameVal = cookies[c].split("=", 2);
		if(nameVal[0].replace(/^ */, "") == "highlightDefaults")
		{
			var defaults = nameVal[1].split(",");
			for(var d in defaults)
			{
				var defaultNameVal = defaults[d].split("-", 2);
				for(var h in highlightClasses)
				{
					if(highlightClasses[h].cssBase == defaultNameVal[0])
					{
						highlightClasses[h].defaultColor = 
							defaultNameVal[1];
					}
				}
			}
		}
	}
	
	//add the toolbox
	var hexDigits = ["0", "3", "6", "9", "c", "f"];
	var sideColumn = document.getElementById("column-one");
	var portletEl = sideColumn.appendChild(document.createElement("div"));
	portletEl.className = "portlet";
	portletEl.id = "HighlightToolbox";
	portletEl.style.position = "relative";
	portletEl.appendChild(document.createElement("h5"))
		.appendChild(document.createTextNode("Highlight Colors"));
	var pBodyEl = portletEl.appendChild(document.createElement("div"));
	pBodyEl.className = "pBody";
	
	//add a definition list of color types
	var dlEl = pBodyEl.appendChild(document.createElement("dl"));
	for(var c in highlightClasses)
	{
		//add the text label
		var labelEl = dlEl.appendChild(document.createElement("dt"))
			.appendChild(document.createElement("label"));
		labelEl.setAttribute("for", "HighlightColor" + c);
		labelEl.appendChild(
			document.createTextNode(highlightClasses[c].description));
		
		//add the dropdown
		var inputEl = document.createElement("select");
		inputEl.id = "HighlightColor" + c;
		inputEl = dlEl.appendChild(document.createElement("dd"))
			.appendChild(inputEl);
		inputEl.onclick = HighlightUpdate;
		inputEl.onchange = HighlightUpdate;
		inputEl.onkeyup = HighlightUpdate;
		
		//add an option for (Default)
		var optionEl = inputEl.appendChild(
			document.createElement("option"));
		optionEl.value = "auto";
		optionEl.appendChild(document.createTextNode("(Default)"));
		
		//add each of the color options
		var optionNum = 1;
		for(h1 in hexDigits)
		{
			for(h2 in hexDigits)
			{
				for(h3 in hexDigits)
				{
					var optionEl = inputEl.appendChild(
						document.createElement("option"));
					var color = "#" + hexDigits[h1] + 
						hexDigits[h2] + hexDigits[h3];
					optionEl.value = color;
					optionEl.style.color = color;
					optionEl.appendChild(document.createTextNode(color));
					if(highlightClasses[c].defaultColor && 
						color.toLowerCase() == 
						highlightClasses[c].defaultColor.toLowerCase())
					{
						inputEl.selectedIndex = optionNum;
					}
					++optionNum;
				}
			}
		}
	}
}

function HighlightScroll(e)
{
	//event handler for scrolling to stick the highlight color
	//toolbox to the top of the screen when scrolled down
	var toolboxEl = document.getElementById("HighlightToolbox");
	if(!toolboxEl.originalTop)
	{
		toolboxEl.originalTop = toolboxEl.offsetTop;
	}
	var pageScroll = (document.body.scrollTop ?
			document.body.scrollTop : window.pageYOffset);
	var newTop = (pageScroll - toolboxEl.originalTop);
	toolboxEl.style.top = (newTop >= 0 ? newTop : 0) + "px";
}

function HighlightActivate()
{
	//check to see if we're looking at a task page
	var catLinksEl = document.getElementById("catlinks");
	var isTask = false;
	if(catLinksEl)
	{
		var aEls = catLinksEl.getElementsByTagName("a");
		for(var a = 0; a < aEls.length; ++a)
		{
			if(aEls[a].getAttribute("title") ==
				"Category:Programming Tasks")
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
	
	//add the toolbox
	HighlightAddToolbox();
	
	//add the event handler for scrolling
	AddHandler(window, "scroll", HighlightScroll);
	
	//update the syntax highlight colors to bring in settings
	//from the cookie
	HighlightUpdate();
}

//register the script with the window's load event
AddHandler(window, "load", HighlightActivate);

//
```

