+++
title = "Rosetta Code:Language comparison script"
description = ""
date = 2011-07-02T21:34:04Z
aliases = []
[extra]
id = 7396
[taxonomies]
categories = []
tags = []
+++

//
```javascript

/*
### ==================================

This comparison script was written by
Tyrok1.  Tested in:
- Fe 7.0.520.0
- Fx 3.6.11
- IE 6, 7, 8

### ==================================
*/
function GetElWidth(el)
{
	return (el.clientWidth ? el.clientWidth : el.offsetWidth);
}

function ToCMulticolumn()
{
	var gutter = 10;
	
	//split the ToC into multiple columns
	var tocEl = document.getElementById("toc");
	var ulElsNodeList = tocEl.getElementsByTagName("ul");
	if(tocEl && ulElsNodeList.length > 0)
	{
		//limit what we're looking at to only what's directly in
		//the main list, and move from NodeList objects to Arrays
		var ulEls = [ulElsNodeList[0]];
		for(var u = 1; u < ulElsNodeList.length; ++u)
		{
			if(ulElsNodeList[u].parentNode == ulElsNodeList[0].parentNode)
			{
				ulEls[ulEls.length] = ulElsNodeList[u];
			}
		}
		var liElsNodeList = tocEl.getElementsByTagName("li");
		var liEls = new Array();
		for(var l = 0; l < liElsNodeList.length; ++l)
		{
			for(u = 0; u < ulEls.length; ++u)
			{
				if(liElsNodeList[l].parentNode == ulEls[u])
				{
					liEls[liEls.length] = liElsNodeList[l];
					break;
				}
			}
		}
		
		//show all of the <ul> tags so we can get their items' ideal widths
		if(ulEls[0].style.display && ulEls[0].style.display == "none")
		{
			ulEls[0].oldDisplay = "none";
		}
		else
		{
			ulEls[0].oldDisplay = "inline-block";
		}
		for(u = 0; u < ulEls.length; ++u)
		{
			ulEls[u].style.display = "inline-block";
		}
		
		//find the widest item and use that to set up an ideal number of columns
		var maxWidth = 0;
		for(l = 0; l < liEls.length; ++l)
		{
			if(!liEls[l].initialWidth)
			{
				liEls[l].initialWidth = GetElWidth(liEls[l]);
			}
			maxWidth = Math.max(liEls[l].initialWidth, maxWidth);
			if(navigator.appVersion.indexOf("MSIE 5.") >= 0 ||
				navigator.appVersion.indexOf("MSIE 6.") >= 0 ||
				navigator.appVersion.indexOf("MSIE 7.") >= 0)
			{
				liEls[l].style.display = "inline";
			}
		}
		
		//add in the gutter
		maxWidth += gutter;
		
		//figure out the ideal number of columns
		tocEl.style.width = "100%";
		var idealColumns = Math.min(
			Math.floor(GetElWidth(tocEl) / maxWidth),
			Math.floor(liEls.length / 3) + 1);
		if(navigator.appVersion.indexOf("MSIE 5.") >= 0 ||
			navigator.appVersion.indexOf("MSIE 6.") >= 0 ||
			navigator.appVersion.indexOf("MSIE 7.") >= 0)
		{
			idealColumns = 1;
		}
		
		//see if we've already got the ideal number of columns
		//if so, we don't need to change anything
		if(ulEls.length != idealColumns)
		{
			//split the list into multiple columns of width (95 / idealColumns)%
			//first, make sure we have enough lists to split into
			while(ulEls.length < idealColumns)
			{
				ulEls[ulEls.length] =
					ulEls[ulEls.length - 1].parentNode.insertBefore(
						document.createElement("ul"),
						ulEls[ulEls.length - 1].nextSibling);
			}
			
			//style the lists
			for(var c = 0; c < ulEls.length; ++c)
			{
				ulEls[c].style.display = "inline-block";
				ulEls[c].style.cssFloat = "left";
				ulEls[c].style.verticalAlign = "top";
				ulEls[c].style.width = (95 / idealColumns) + "%";
			}
			
			//sort the items into the lists
			for(l = 0; l < liEls.length; ++l)
			{
				liEls[l] = ulEls[
						Math.floor(l / liEls.length * idealColumns)
					].appendChild(liEls[l].parentNode.removeChild(liEls[l]));
			}
			
			//get rid of any unnecessary lists
			for(l = idealColumns; l < ulEls.length; ++l)
			{
				ulEls[l].parentNode.removeChild(ulEls[l]);
			}
		}
		
		//hide the instructions/lists if they're supposed to be hidden
		var insEl = document.getElementById("tocinstructions");
		if(insEl)
		{
			insEl.style.display =
				(ulEls[0].oldDisplay.toLowerCase() == "none" ? "none" : "block");
		}
		for(u = 0; u < ulEls.length; ++u)
		{
			ulEls[u].style.display = ulEls[0].oldDisplay;
		}
		
		//if we're setting this up for the first time...
		if(!window.toggleTocOld)
		{
			//change out the toggling function so it toggles all of the lists
			window.toggleTocOld = window.toggleToc;
			window.toggleUpdateVisibility = function() {
							var tocEl = document.getElementById("toc");
							var ulEls = tocEl.getElementsByTagName("ul");
							for(var u = 0; u < ulEls.length; ++u)
							{
								if(ulEls[0].style.display &&
									ulEls[0].style.display == "none")
								{
									ulEls[u].style.display = "none"
								}
								else
								{
									ulEls[u].style.display = "inline-block"
								}
							}
							var el = document.getElementById("tocinstructions");
							if(el)
							{
								if(ulEls[0].style.display &&
									ulEls[0].style.display.toLowerCase() == "none")
								{
									el.style.display = "none";
								}
								else
								{
									el.style.display = "block";
								}
							}
						};
			window.toggleToc = function() {
							toggleTocOld();
							toggleUpdateVisibility();
						};
			
			//add an event handler so if the window's
			//resized the number of columns can change
			if(window.addEventListener)
			{
				window.addEventListener("resize", ToCMulticolumn, false);
			}
			else if(document.addEventListener)
			{
				document.addEventListener("resize", ToCMulticolumn, false);
			}
			else if(window.attachEvent)
			{
				window.attachEvent("onresize", ToCMulticolumn);
			}
		}
	}
}

function CompareActivate()
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
	
	//check to make sure we have a table of contents
	var tocEl = document.getElementById("toc");
	if(tocEl)
	{
		//remove the extra show/hide button from the Contents area
		var tocTitleEl = document.getElementById("toctitle");
		if(tocTitleEl)
		{
			var spanEls = tocTitleEl.getElementsByTagName("span");
			var foundToggle = false;
			for(var s = 0; s < spanEls.length; ++s)
			{
				if(spanEls[s].className &&
					spanEls[s].className.toLowerCase() == "toctoggle")
				{
					if(foundToggle)
					{
						spanEls[s].parentNode.removeChild(spanEls[s]);
					}
					foundToggle = true;
				}
			}
		}
		
		//find all of the links to language sections and add checkboxes
		var liEls = tocEl.getElementsByTagName("li");
		for(var l  = 0; l < liEls.length; ++l)
		{
			//add a checkbox with the language section's anchor name attribute
			//as its value for easier lookup in the refresh function
			var checkboxEl = document.createElement("input");
			checkboxEl.setAttribute("type", "checkbox");
			checkboxEl.setAttribute("name", "CompareLanguage" + l);
			checkboxEl.setAttribute("id", "CompareLanguage" + l);
			var href = liEls[l].getElementsByTagName("a")[0].getAttribute("href");
			checkboxEl.setAttribute("value",
				href.substring(href.indexOf("#") + 1, href.length));
			checkboxEl.onclick = CompareRefresh;
			liEls[l].insertBefore(checkboxEl, liEls[l].firstChild);
		}
		
		//add some instructions
		var toctitleEl = document.getElementById("toctitle");
		var instructionsEl = toctitleEl.appendChild(document.createElement("p"));
		instructionsEl.setAttribute("id", "tocinstructions");
		instructionsEl.appendChild(
			document.createTextNode("Check the boxes next to languages to compare them."));
		instructionsEl.appendChild(document.createElement("br"));
		instructionsEl.appendChild(
			document.createTextNode("Uncheck all boxes to show all languages."));
		
		//switch the ToC to multi-column mode
		ToCMulticolumn();
		
		//put all language sections inside of a <div>
		var spanEls = document.getElementsByTagName("span");
		var languages = CompareGetLanguages();
		var divEl = null;
		for(var s = 0; s < spanEls.length; ++s)
		{
			if(typeof spanEls[s].id != "undefined" &&
				typeof languages[spanEls[s].id] != "undefined")
			{
				//we can assume that this is a language section
				//make a list of all of the elements between this <span>
				//tag and the next one that has a name attribute
				divEl = spanEls[s].parentNode.parentNode.insertBefore(
					document.createElement("div"), spanEls[s].parentNode);
				var curEl = spanEls[s].parentNode.nextSibling;
				divEl.appendChild(spanEls[s].parentNode.parentNode.removeChild(
					spanEls[s].parentNode));
				while(curEl && (!curEl.tagName || curEl.tagName.toLowerCase() != "h2"))
				{
					var newCurEl = curEl.nextSibling;
					divEl.appendChild(curEl.parentNode.removeChild(curEl));
					curEl = newCurEl;
				};
			}
		}
		if(divEl)
		{
			//add a float clearing div
			var clearEl = divEl.parentNode.insertBefore(
				document.createElement("div"), divEl.nextSibling);
			clearEl.style.clear = "both";
		}
		
		//and refresh the display for good measure (sometimes
		//browsers like to save <input> states between reloads)
		CompareRefresh();
		
		//add an event listener for window resizing
		if(window.addEventListener)
		{
			window.addEventListener("resize", CompareRefresh, false);
		}
		else if(document.addEventListener)
		{
			document.addEventListener("resize", CompareRefresh, false);
		}
		else if(window.attachEvent)
		{
			window.attachEvent("onresize", CompareRefresh);
		}
	}
}

function CompareGetLanguages()
{
	var tocEl = document.getElementById("toc");
	var languages = new Array();
	if(tocEl)
	{
		//find all of the checkboxes within the ToC
		var inputEls = tocEl.getElementsByTagName("input");
		for(var i = 0; i < inputEls.length; ++i)
		{
			//if it's one of the ones we added earlier, log the
			//checked status to the languages array for later use
			if(inputEls[i].getAttribute("name").substring(0,
				("CompareLanguage").length) == "CompareLanguage")
			{
				languages[inputEls[i].value] = inputEls[i].checked;
			}
		}
	}
	return languages;
}

function CompareRefresh()
{
	//refresh the display of the language sections displayed under the ToC
	//first, check to see if we're looking at a task page
	var codeMargin = 20;
	var tocEl = document.getElementById("toc");
	var languages = new Array();
	var numChecked = 0;
	if(tocEl)
	{
		//count how many languages are checked
		languages = CompareGetLanguages();
		for(var l in languages)
		{
			if(languages[l])
			{
				++numChecked;
			}
		}
		
		//now, find all <a> tags with name attributes
		var spanEls = document.getElementsByTagName("span");
		for(var s = 0; s < spanEls.length; ++s)
		{
			if(typeof spanEls[s].className != "undefined" &&
				spanEls[s].className.indexOf("mw-headline") >= 0 &&
				typeof spanEls[s].id != "undefined" &&
				typeof languages[spanEls[s].id] != "undefined")
			{
				var anchorName = spanEls[s].id;
				var parentDiv = spanEls[s].parentNode.parentNode;
				
				//we can assume that this is a language section
				//check to see if we're displaying in side-by-side mode,
				//regular mode, or not at all
				if(numChecked < 1 || (languages[anchorName] &&
					(navigator.appVersion.indexOf("MSIE 5.") >= 0 ||
					navigator.appVersion.indexOf("MSIE 6.") >= 0 ||
					navigator.appVersion.indexOf("MSIE 7.") >= 0)))
				{
					//enable full-width mode
					parentDiv.style.display = "block";
					parentDiv.style.cssFloat = "none";
					parentDiv.style.width = "auto";
					parentDiv.style.marginRight = 0;
				}
				else if(languages[anchorName])
				{
					//enable side-by-side mode
					parentDiv.style.display = "inline-block";
					parentDiv.style.verticalAlign = "top";
					parentDiv.style.marginRight = "20px";
					
					//find the width for this language
					var preEls = parentDiv.getElementsByTagName("pre");
					var maxWidth = 0;
					for(var p = 0; p < preEls.length; ++p)
					{
						var oldWidth = GetElWidth(preEls[p]);
						preEls[p].style.cssFloat = "left";
						if(GetElWidth(preEls[p]) == oldWidth)
						{
							//assume it's either wider than the
							//window or IE where they don't shrink
							//arbitrarily set to 45% width
							maxWidth = Math.max(
								GetElWidth(parentDiv.parentNode) * 0.45,
								maxWidth);
						}
						else
						{
							maxWidth = Math.max(GetElWidth(preEls[p]),
								maxWidth);
						}
						preEls[p].style.cssFloat = "none";
					}
					
					//if there are no code blocks, base it off of the <h2>
					if(!maxWidth)
					{
						var h2Els = parentDiv.getElementsByTagName("h2");
						if(h2Els.length > 0)
						{
							//we have at least one h2
							//use its width
							var oldWidth = GetElWidth(h2Els[p]);
							h2Els[0].style.cssFloat = "left";
							if(GetElWidth(h2Els[p]) == oldWidth)
							{
								//assume it's either wider than the
								//window or IE where they don't shrink
								//arbitrarily set to 45% width
								maxWidth = Math.max(
									GetElWidth(parentDiv.parentNode) * 0.45,
									maxWidth);
							}
							else
							{
								maxWidth = GetElWidth(h2Els[0]);
							}
							h2Els[0].style.cssFloat = "none";
						}
						else
						{
							//no clue what it should be
							//arbitrarily set it to 200
							maxWidth = 200;
						}
					}
					
					//set the container <div> to a size just a little bit larger
					parentDiv.style.width = (maxWidth + codeMargin) + "px";
				}
				else
				{
					//hide it entirely
					parentDiv.style.display = "none";
				}
			}
		}
	}
}

//register the comparison script with the window's load event
if(window.addEventListener)
{
	window.addEventListener("load", CompareActivate, false);
}
else if(window.attachEvent)
{
	window.attachEvent("onload", CompareActivate);
}
else if(document.addEventListener)
{
	document.addEventListener("load", CompareActivate, false);
}
else
{
	window.onload = CompareActivate;
}
//
```

