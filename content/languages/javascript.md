+++
title = "JavaScript"
description = ""
date = 2016-11-04T18:42:07Z
aliases = []
[extra]
id = 1699
[taxonomies]
categories = []
tags = []
+++

'''JavaScript''' is the name of Netscape Communications Corporation's implementation of the [ECMAScript standard](https://rosettacode.org/wiki/ECMAScript_standard), a scripting language based on the concept of prototype-based programming. The language is best known for its use in websites (as client-side JavaScript), but is also used to enable scripting access to objects embedded in other applications.

Despite the name, JavaScript is only distantly related to the [Java](https://rosettacode.org/wiki/Java) programming language, the main similarity being their common debt to the [C](https://rosettacode.org/wiki/C) syntax. Semantically, JavaScript syntax has far more in common with the [Self](https://rosettacode.org/wiki/Self) [programming language](https://rosettacode.org/wiki/programming_language).

JavaScript is a registered trademark of [Sun Microsystems](https://rosettacode.org/wiki/Sun_Microsystems), Inc. It was used under license for technology invented and implemented by Netscape Communications and current entities such as the Mozilla Foundation.

Major browsers have generally implemented the features of ECMASScript 5 (ES5), and have started to adopt elements of ES6.
Updates to implementation of ES6 features are tabulated here: [https://kangax.github.io/compat-table/es6/ https://kangax.github.io/compat-table/es6/]

Once largely confined to browser environments, and typically isolated from access to system resources, JavaScript (and particularly the cross-platform [https://nodejs.org/en/ Node.js runtime environment]) is now very widely used in server-side and application scripting environments, with full access to local file systems and other OS resources.

At the same time, mainly because of JavaScript's role in the web, there is a growing number of other languages which [https://github.com/jashkenas/coffeescript/wiki/list-of-languages-that-compile-to-js compile to JavaScript].

The inclusion of '''tail-call optimisation''' in the ES6 standard reflects increased interest in functional approaches to the composition of JavaScript code, expressed for example, in significant adoption of libraries like Underscore and Lodash. If ES6 tail-call optimisation is widely implemented by JavaScript engines (so far this has mainly been achieved only by Apple's Safari engine) it will make JavaScript a more efficient and more natural environment for coding in a functional idiom.

## Citations
* [Wikipedia:Javascript](https://en.wikipedia.org/wiki/Javascript)
* [https://nodejs.org/en/ Node.js] Event-driven I/O server-side JavaScript environment based on V8
* [https://www.npmjs.com npm – Node.js Package Manager] Claims to be the largest ecosystem of open source libraries in the world
* [https://developer.apple.com/library/mac/releasenotes/InterapplicationCommunication/RN-JavaScriptForAutomation/Articles/Introduction.html OS X JavaScript for Applications] JavaScript as an OS X scripting language – supported by the Safari debugger
* [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Shells Other JavaScript shells] List maintained by Mozilla
* [https://github.com/jashkenas/coffeescript/wiki/list-of-languages-that-compile-to-js List of languages that compile to JS] maintained on Github by Jeremy Ashenas – author of CoffeeScript, Underscore and Backbone
* [http://shop.oreilly.com/product/0636920028857.do Functional JavaScript] – Michael Fogus, O'Reilly 2013


## Merged content



Presented here is the core of a Brainfuck interpreter/debugger written in [JavaScript](https://rosettacode.org/wiki/JavaScript) that is designed to run within a web page. The full source can be found at http://www.quirkster.com/iano/js/bf.html by viewing the page source.


```javascript
var code;           // formatted code
var ip = 0;         // current instruction within code
var nest = 0;       // current bracket nesting (for Out button)
var ahead = [];     // locations of matching brackets

var data = [0];     // data array (mod by +, -)
var dp = 0;         // index into data (mod by <, >)

var inp = 0;        // current input character (fetch with ,)
var quit = 0;       // termination flag

var commands = {
	'>':function() { if (++dp >= data.length) data[dp]=0 },
 	'<':function() { if (--dp < 0) quit++ },
 	'+':function() { ++data[dp] },
	'-':function() { --data[dp] },
 	'[':function() { if (!data[dp]) ip = ahead[ip]; else ++nest },
 	']':function() { if ( data[dp]) ip = ahead[ip]; else --nest },
 	',':function() {
		var c = document.getElementById("input").value.charCodeAt(inp++);
		data[dp] = isNaN(c) ? 0 : c;  // EOF: other options are -1 or no change
 	},
	'.':function() {
		var s = document.getElementById("output").innerHTML
		      + String.fromCharCode(data[dp]);
		s = s.replace(/\n/g,"
").replace(/ /g,"&amp;nbsp;");
		document.getElementById("output").innerHTML = s;
	},
};

function format(id) {
	var el = document.getElementById(id);
	code = el.value || el.textContent || el.innerText;
	// scan for matching braces
	var st = [], back, error = -1;
	ahead = [];
	for (ip=0; ip<code.length; ip++) {
		switch(code[ip]) {
		case '[':
			st.push(ip);
			break;
		case ']':
			if (st.length == 0) error = ip;
			back = st.pop();
			ahead[ip] = back;
			ahead[back] = ip;
			break;
		}
	}
	if (st.length > 0) error = st[0];
	init();
	if (error >= 0) {
		ip = error;	// highlight error
		alert("Unmatched '"+code[error]+"' at "+error+"!");
	}
	// show the  code in a PRE block (id=code)
	dump();
	window.scroll(0,document.getElementById("code").offsetTop);
}

function encode(s) {
	var e = s.replace(/&/g, "&amp;amp;");
	    e = e.replace(/</g, "&amp;lt;");
		e = e.replace(/>/g, "&amp;gt;");
		e = e.replace(/ /g, "&amp;nbsp;");
	return  e.replace(/\n/g, "
");
}
function dump() {
	document.getElementById("code").innerHTML
		= encode(code.substring(0,ip))
		+ '<span style="background: pink">'
		+ encode(code.charAt(ip))
		+ "</span>"
		+ encode(code.substring(ip+1));
	document.getElementById("mem").value = data.join();
}

function init() {
	inp = 0; quit = 0; dp = 0;
	data = [0]; ip = 0;
	document.getElementById("output").innerHTML = "";
}

function done() {
	return quit || ip >= code.length;
}

function body() {
        var fn;
        do {
          fn = commands[code.charAt(ip)];
	  if (fn) fn();
	  ++ip;
	} while (fn == null && !done());
}

// button handlers

var tid = 0;
function step() {
	var n = document.getElementById("repeat").value - 0;

	if (done()) init();
	while (--n>=0 && !done())
		body();

	if (done() && tid) slow();	// stop timer
	dump();
}

function out() {
	var d = nest;
	while (nest>=d && !done())
		body();
	dump();
}

function slow() {
	if (tid) {
		clearInterval(tid);
		tid = 0;
		document.getElementById("slow").value = "Slow";
	} else {
		var n = document.getElementById("speed").value - 0;
		tid = setInterval(step, Math.max(n,10));
		document.getElementById("slow").value = "Stop";
	}
}

function run() {
	while (!done())
		body();
	dump();
}
```

