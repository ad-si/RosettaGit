+++
title = "Rosetta Code:My Scripts Menu"
description = ""
date = 2011-07-03T19:35:41Z
aliases = []
[extra]
id = 7778
[taxonomies]
categories = []
tags = []
+++

//
```javascript

/*
### ==================================

This menu script was written by
Tyrok1, and has been tested on Fx 3.6.6,
Cr 5.0.375.99, Epiphany 2.30.2,
Midori 0.2.2

### ==================================
*/

var gadgetsAvailable = [
					{
						id: "LanguageComparison",
						name: "Language comparison",
						url: "http://rosettacode.org/mw/index.php" +
							"?title=Rosetta_Code:Language_comparison_script" + 
							"&action=raw&ctype=text/javascript",
						prefix: "Compare"
					},
					{
						id: "Highlight",
						name: "Syntax Highlight Color Picker",
						url: "http://rosettacode.org/mw/index.php" +
							"?title=Rosetta_Code:Syntax_Highlight_Color_Picker" +
							"&action=raw&ctype=text/javascript",
						prefix: "Highlight"
					},
					{
						id: "UtilityButtonBar",
						name: "Utility button bar",
						url: "http://rosettacode.org/mw/index.php" +
							"?title=Rosetta_Code:Per-Code_Example_Buttonbar" +
							"&action=raw&ctype=text/javascript",
						prefix: "ButtonBar"
					}
				];

function GadgetsAddHandler(el, handlerType, func)
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

function GadgetsScriptLoad(g)
{
	//see if we need to load it
	//if we don't, there's not much point in loading it again
	if(!document.getElementById("GadgetScript" + g))
	{
		var scriptEl = document.createElement("script");
		scriptEl.setAttribute("id", "GadgetScript" + g);
		scriptEl.setAttribute("type", "text/javascript");
		scriptEl.setAttribute("src", gadgetsAvailable[g].url);
		document.documentElement.appendChild(scriptEl);
	}
}

function GadgetsScriptActivate(g)
{
	//check to see if the script's finished loading
	if(eval("typeof " + gadgetsAvailable[g].prefix + "Activate") == "undefined")
	{
		//not finished yet - try reactivating in another 0.5s
		setTimeout(function() { GadgetsScriptActivate(g); }, 500);
	}
	else
	{
		//run the activation function
		eval(gadgetsAvailable[g].prefix + "Activate();");
	}
}

function GadgetsScriptDeactivate(g)
{
	//check to see if the specified script has a deactivation function
	if(eval("typeof " + gadgetsAvailable[g].prefix + "Deactivate") != "undefined")
	{
		//it does - run it
		eval(gadgetsAvailable[g].prefix + "Deactivate();");
	}
	else
	{
		//pop up a warning to the user to let them know
		//the plugin will be disabled when they reload
		alert("This script will be disabled on next page load");
	}
}

function GadgetsCreateCheckboxClosure(el)
{
	//create an onclick handler for the script checkboxes
	return function() {
				//save the checkboxes to the cookie for next page load
				GadgetsSaveCheckboxes();
				
				//check to see if we need to activate or deactivate
				if(el.checked)
				{
					//load and activate the script
					GadgetsScriptLoad(el.value);
					GadgetsScriptActivate(el.value);
				}
				else
				{
					//deactivate the script
					GadgetsScriptDeactivate(el.value);
				}
			};
}

function GadgetsMouseOver()
{
	//show the scripts dropdown
	document.getElementById("pt-jsgadgets-list").style.display = "block";
}

function GadgetsMouseOut()
{
	//hide the scripts dropdown
	document.getElementById("pt-jsgadgets-list").style.display = "none";
}

function GadgetsSaveCheckboxes()
{
	var checkedBoxes = "", checkboxEl = null;
	
	//build a string representation of all of the checked boxes to save to the cookie
	for(var c = 0; checkboxEl = document.getElementById("pt-jsgadgets-gadget" + c); ++c)
	{
		if(checkboxEl.checked)
		{
			if(checkedBoxes != "")
			{
				checkedBoxes += ",";
			}
			checkedBoxes += gadgetsAvailable[c].id;
		}
	}
	
	//set an expiry date 1000 years into the future
	var expireDate = new Date();
	expireDate.setFullYear(expireDate.getFullYear() + 1000);
	
	//store the cookie
	document.cookie = "jsGadgets=" + escape(checkedBoxes) +
			"; expires=" + expireDate.toUTCString();
}

function GadgetsGetCheckboxes()
{
	//pull in the cookie and look for the jsGadgets key
	var fullCookie = document.cookie;
	var gadgetsPos = fullCookie.indexOf("jsGadgets=");
	if(gadgetsPos < 0)
	{
		//no gadgets in the cookie
		return new Array();
	}
	var endPos = fullCookie.indexOf(";", gadgetsPos);
	
	//split the gadget IDs by comma
	var jsGadgets = fullCookie.substring(gadgetsPos + ("jsGadgets=").length,
					(endPos > 0 ? endPos : fullCookie.length));
	var checkIDs = unescape(jsGadgets).split(",");
	
	//translate the list of IDs to a list of array indices,
	//in keeping with the way the rest of this module works
	var checkIndices = new Array();
	for(var g = 0; g < gadgetsAvailable.length; ++g)
	{
		for(var c = 0; c < checkIDs.length; ++c)
		{
			if(gadgetsAvailable[g].id == checkIDs[c])
			{
				checkIndices[checkIndices.length] = g;
				break;
			}
		}
	}
	return checkIndices;
}

function GadgetsActivate()
{
	//find the user preferences button and add a new list item to the right of it
	var prefsEl = document.getElementById("pt-preferences");
	if(!prefsEl)
	{
		var personalEl = document.getElementById("p-personal");
		if(!personalEl)
		{
			return;
		}
		var liEls = personalEl.getElementsByTagName("li");
		if(!liEls || liEls.length < 2)
		{
			return;
		}
		prefsEl = liEls[liEls.length - 2];
	}
	var jsMenuEl = prefsEl.parentNode.insertBefore(
				document.createElement("li"), prefsEl.nextSibling);
	jsMenuEl.setAttribute("id", "pt-jsgadgets");
	
	//add a new link named "My scripts" in the new list item
	//from a semantics point of view, this is not great,
	//but this way the styling kicks in to keep it consistent with its siblings
	var jsLinkEl = jsMenuEl.appendChild(document.createElement("a"));
	jsLinkEl.appendChild(document.createTextNode("My scripts"));
	jsLinkEl.setAttribute("href", "javascript: void(0);");
	jsLinkEl.style.position = "relative";
	
	//add handlers for showing on mouseover and hiding on mouseout
	GadgetsAddHandler(jsLinkEl, "mouseover", GadgetsMouseOver);
	GadgetsAddHandler(jsLinkEl, "mouseout", GadgetsMouseOut);
	
	//build a list for showing available JS gadgets
	var modulesListEl = jsLinkEl.appendChild(document.createElement("ol"));
	modulesListEl.setAttribute("id", "pt-jsgadgets-list");
	modulesListEl.style.listStyle = "none";
	modulesListEl.style.margin = 0;
	modulesListEl.style.padding = 0;
	modulesListEl.style.position = "absolute";
	modulesListEl.style.top = "100%";
	modulesListEl.style.left = 0;
	modulesListEl.style.backgroundColor = "#fff";
	modulesListEl.style.border = "1px solid #69c";
	modulesListEl.style.padding = "0.5em";
	modulesListEl.style.display = "none";
	
	//raise the zIndex of the new button (as well as
	//a few parents) so the menu overlaps the page
	var zEl = modulesListEl;
	for(var z = 0; z < 6 && zEl && zEl.style; ++z, zEl = zEl.parentNode)
	{
		zEl.style.zIndex = 100;
	}
	
	//add each of the gadgets to the list
	for(var m = 0; m < gadgetsAvailable.length; ++m)
	{
		//add the list item element
		var liEl = modulesListEl.appendChild(document.createElement("li"));
		liEl.style.display = "block";
		liEl.style.cssFloat = "none";
		liEl.style.right = "auto";
		liEl.style.left = 0;
		liEl.style.textAlign = "left";
		liEl.style.margin = 0;
		liEl.style.padding = 0;
		
		//add the checkbox
		var inputEl = document.createElement("input");
		var checkboxId = "pt-jsgadgets-gadget" + m;
		inputEl.setAttribute("type", "checkbox");
		inputEl.setAttribute("id", checkboxId);
		inputEl.setAttribute("value", m);
		GadgetsAddHandler(inputEl, "click", GadgetsCreateCheckboxClosure(inputEl));
		liEl.appendChild(inputEl);
		
		//add the label
		var labelEl = document.createElement("label");
		labelEl.setAttribute("for", checkboxId);
		labelEl.appendChild(document.createTextNode(gadgetsAvailable[m].name));
		labelEl.style.paddingLeft = "1em";
		liEl.appendChild(labelEl);
	}
	
	//fetch the saved checkboxes from the cookie
	var checks = GadgetsGetCheckboxes();
	for(var c = 0; c < checks.length; ++c)
	{
		//for each one that should be checked on page load,
		//check the box, load, and activate it
		document.getElementById("pt-jsgadgets-gadget" + checks[c]).checked = true;
		GadgetsScriptLoad(checks[c]);
		GadgetsScriptActivate(checks[c]);
	}
}

//register the comparison script with the window's load event
GadgetsAddHandler(window, "load", GadgetsActivate);
//
```

