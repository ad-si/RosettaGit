+++
title = "Recursive descent parser generator"
description = ""
date = 2014-04-30T07:07:59Z
aliases = []
[extra]
id = 17472
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "cpp",
  "j",
]
+++

Write a recursive descent parser generator that takes a description of a grammar as input and outputs the source code for a parser in the same language as the generator. (So a generator written in C++ would output C++ source code for the parser.) You can assume that all of the rules have been preprocessed into a form suitable for the construction of a recursive descent parser. Check the following links for more details.
* http://www.cs.engr.uky.edu/~lewis/essays/compilers/rec-des.html
* http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm

Use the parser generator and a grammar file to build a parser that takes an arithmetic expression and turns it in to three address code. The resulting parser should take this (or something similar) as input:

```txt

(one + two) * three - four * five

```


And generate this (or something similar) as output:

```txt

_0001 = one + two
_0002 = _0001 * three
_0003 = four * five
_0004 = _0002 - _0003

```



## C++

This program translates an annotated LL(1) grammar into a C++ lexer plus parser. Each rule is required to return a string of some kind and the return values of the already matched nonterminals (and matched text of terminals) can be accessed with $1, $2, etc. which are replaced by the appropriate string variable.

It can't handle newlines as part of the grammar, the error checking is fairly limited and the error reporting is basically non-existent, but the parser it generates (not shown below) is human readable.


```cpp

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <map>
#include <set>
#include <regex>
using namespace std;

map<string, string> terminals;
map<string, vector<vector<string>>> nonterminalRules;
map<string, set<string>> nonterminalFirst;
map<string, vector<string>> nonterminalCode;

int main(int argc, char **argv) {
	if (argc < 3) {
		cout << "Usage: <input file> <output file>" << endl;
		return 1;
	}

	ifstream inFile(argv[1]);
	ofstream outFile(argv[2]);

	regex blankLine(R"(^\s*$)");
	regex terminalPattern(R"((\w+)\s+(.+))");
	regex rulePattern(R"(^!!\s*(\w+)\s*->\s*((?:\w+\s*)*)$)");
	regex argPattern(R"(\$(\d+))");
	smatch results;

	// Read terminal patterns
	string line;
	while (true) {
		getline(inFile, line);

		// Terminals section ends with a blank line
		if (regex_match(line, blankLine))
			break;

		regex_match(line, results, terminalPattern);
		terminals[results[1]] = results[2];
	}

	outFile << "#include <iostream>" << endl;
	outFile << "#include <fstream>" << endl;
	outFile << "#include <string>" << endl;
	outFile << "#include <regex>" << endl;
	outFile << "using namespace std;" << endl << endl;

	// Generate the token processing functions
	outFile << "string input, nextToken, nextTokenValue;" << endl;
	outFile << "string prevToken, prevTokenValue;" << endl << endl;

	outFile << "void advanceToken() {" << endl;
	outFile << "	static smatch results;" << endl << endl;

	outFile << "	prevToken = nextToken;" << endl;
	outFile << "	prevTokenValue = nextTokenValue;" << endl << endl;

	for (auto i = terminals.begin(); i != terminals.end(); ++i) {
		string name = i->first + "_pattern";
		string pattern = i->second;

		outFile << "	static regex " << name << "(R\"(^\\s*(" << pattern << "))\");" << endl;
		outFile << "	if (regex_search(input, results, " << name << ", regex_constants::match_continuous)) {" << endl;
		outFile << "		nextToken = \"" << i->first << "\";" << endl;
		outFile << "		nextTokenValue = results[1];" << endl;
		outFile << "		input = regex_replace(input, " << name << ", \"\");" << endl;
		outFile << "		return;" << endl;
		outFile << "	}" << endl << endl;
	}

	outFile << "	static regex eof(R\"(\\s*)\");" << endl;
	outFile << "	if (regex_match(input, results, eof, regex_constants::match_continuous)) {" << endl;
	outFile << "		nextToken = \"\";" << endl;
	outFile << "		nextTokenValue = \"\";" << endl;
	outFile << "		return;" << endl;
	outFile << "	}" << endl << endl;

	outFile << "	throw \"Unknown token\";" << endl;
	outFile << "}" << endl << endl;

	outFile << "bool same(string symbol) {" << endl;
	outFile << "	if (symbol == nextToken) {" << endl;
	outFile << "		advanceToken();" << endl;
	outFile << "		return true;" << endl;
	outFile << "	}" << endl;
	outFile << "	return false;" << endl;
	outFile << "}" << endl << endl;

	// Copy the header code to the output
	while (true) {
		getline(inFile, line);
		
		// Copy lines until we reach the first rule
		if (regex_match(line, results, rulePattern))
			break;

		outFile << line << endl;
	}

	// Build the nonterminal table
	while (true) {
		// results already contains the last matched rule
		string name = results[1];
		stringstream ss(results[2]);

		string tempString;
		vector<string> tempVector;
		while (ss >> tempString)
			tempVector.push_back(tempString);
		nonterminalRules[name].push_back(tempVector);

		// Read code until another rule is found
		string code = "";
		while (true) {
			getline(inFile, line);

			if (!inFile || regex_match(line, results, rulePattern))
				break;

			// Replace $1 with results[1], etc.
			line = regex_replace(line, argPattern, "results[$1]");

			code += line + "\n";
		}
		nonterminalCode[name].push_back(code);

		// Stop when we reach the end of the file
		if (!inFile)
			break;
	}

	// Generate the first sets, inefficiently
	bool done = false;
	while (!done)
		for (auto i = nonterminalRules.begin(); i != nonterminalRules.end(); ++i) {
			string name = i->first;
			done = true; 

			if (nonterminalFirst.find(i->first) == nonterminalFirst.end())
				nonterminalFirst[i->first] = set<string>();

			for (int j = 0; j < i->second.size(); ++j) {
				if (i->second[j].size() == 0)
					nonterminalFirst[i->first].insert("");
				else {
					string first = i->second[j][0];
					if (nonterminalFirst.find(first) != nonterminalFirst.end()) {
						for (auto k = nonterminalFirst[first].begin(); k != nonterminalFirst[first].end(); ++k) {
							if (nonterminalFirst[name].find(*k) == nonterminalFirst[name].end()) {
								nonterminalFirst[name].insert(*k);
								done = false;
							}
						}
					} else if (nonterminalFirst[name].find(first) == nonterminalFirst[name].end()) {
						nonterminalFirst[name].insert(first);
						done = false;
					}
				}
			}
		}

	// Generate function signatures for the nonterminals
	for (auto i = nonterminalRules.begin(); i != nonterminalRules.end(); ++i) {
		string name = i->first + "_rule";
		outFile << "string " << name << "();" << endl;
	}
	outFile << endl;

	// Generate the nonterminal functions
	for (auto i = nonterminalRules.begin(); i != nonterminalRules.end(); ++i) {
		string name = i->first + "_rule";
		outFile << "string " << name << "() {" << endl;
		outFile << "	vector<string> results;" << endl;
		outFile << "	results.push_back(\"\");" << endl << endl;
		
		// Check if this rule can match an empty string
		int epsilon = -1;
		for (int j = 0; epsilon == -1 && j < i->second.size(); ++j)
			if (i->second[j].size() == 0)
				epsilon = j;

		// Generate each production
		for (int j = 0; j < i->second.size(); ++j) {
			// Nothing to generate for an empty rule
			if (j == epsilon)
				continue;

			string token = i->second[j][0];
			if (terminals.find(token) != terminals.end())
				outFile << "	if (nextToken == \"" << i->second[j][0] << "\") {" << endl;
			else {
				outFile << "	if (";
				bool first = true;
				for (auto k = nonterminalFirst[token].begin(); k != nonterminalFirst[token].end(); ++k, first = false) {
					if (!first)
						outFile << " || ";
					outFile << "nextToken == \"" << (*k) << "\"";
				}
				outFile << ") {" << endl;
			}
			
			for (int k = 0; k < i->second[j].size(); ++k) {
				if (terminals.find(i->second[j][k]) != terminals.end()) {
					outFile << "		if(same(\"" << i->second[j][k] << "\"))" << endl;
					outFile << "			results.push_back(prevTokenValue);" << endl;
					outFile << "		else" << endl;
					outFile << "			throw \"Syntax error - mismatched token\";" << endl;
				} else
					outFile << "		results.push_back(" << i->second[j][k] << "_rule());" << endl;
			}

			// Copy rule code to output
			outFile << nonterminalCode[i->first][j];

			outFile << "	}" << endl << endl;
		}

		if (epsilon == -1)
			outFile << "	throw \"Syntax error - unmatched token\";" << endl;
		else
			outFile << nonterminalCode[i->first][epsilon];

		outFile << "}" << endl << endl;
	}

	// Generate the main function
	outFile << "int main(int argc, char **argv) {" << endl;
	outFile << "	if(argc < 2) {" << endl;
	outFile << "		cout << \"Usage: <input file>\" << endl;" << endl;
	outFile << "		return 1;" << endl;
	outFile << "	}" << endl << endl;

	outFile << "	ifstream file(argv[1]);" << endl;
	outFile << "	string line;" << endl;
	outFile << "	input = \"\";" << endl << endl;

	outFile << "	while(true) {" << endl;
	outFile << "		getline(file, line);" << endl;
	outFile << "		if(!file) break;" << endl;
	outFile << "		input += line + \"\\n\";" << endl;
	outFile << "	}" << endl << endl;

	outFile << "	advanceToken();" << endl << endl;

	outFile << "	start_rule();" << endl;
	outFile << "}" << endl;
}

```


Example grammar:

```txt

plus	\+
minus	-
times	\*
div	/
open	\(
close	\)
num	[0-9]+
var	[a-z]+

string nextLabel() {
	static string label = "0000";
	for(int i = label.length() - 1; i >= 0; --i) {
		if(label[i] == '9')
			label[i] = '0';
		else {
			++label[i];
			break;
		}
	}
	return "_" + label;
}

!! start -> expr start2
if($2 == "")
	return $1;
else {
	string label = nextLabel();
	cout << label << " = " << $1 << " " << $2 << endl;
	return label;
}

!! start2 -> plus start
return "+ " + $2;

!! start2 -> minus start
return "- " + $2;

!! start2 -> 
return "";

!! expr -> term expr2
if($2 == "")
	return $1;
else {
	string label = nextLabel();
	cout << label << " = " << $1 << " " << $2 << endl;
	return label;
}

!! expr2 -> times expr
return "* " + $2;

!! expr2 -> div expr
return "/ " + $2;

!! expr2 ->
return "";

!! term -> var
return $1;

!! term -> num
return $1;

!! term -> open start close
return $2;

```


Example input to parser (filename passed through command line):

```txt

(one + two) * three + four * five

```


Output (to standard out):

```txt

_0001 = one + two
_0002 = _0001 * three
_0003 = four * five
_0004 = _0002 + _0003

```



## J


J's native recursive descent parser is adequate for this task, if we map names appropriately.

Implementation:


```J
cocurrent 'base'

inlocale=: 4 :0 L:0
  x,'_',y,'_'
)

parse=: 3 :0
  sentence=. ;:y
  opinds=. (;:'+*-')i.sentence
  opfuns=. (;:'plus times minus') inlocale 'base'
  scratch=. cocreate''
  coinsert__scratch 'base'
  names=. ~.sentence#~_1<:nc sentence
  (names inlocale scratch)=: names
  r=. do__scratch ;:inv opinds}((#sentence)#"0 opfuns),sentence
  codestroy__scratch''
  r
)

term=: 1 :0
  2 :('m''',m,'''expr n')
)

expr=:1 :0
:
  r=. genname''
  emit r,'=:',x,m,y
  r
)

plus=: '+' expr
times=: '*' term
minus=: '-' expr

N=: 10000
genname=: 3 :0
  'z',}.":N=: N+1
)

emit=: smoutput

```


Task example:


```J
   parse '(one + two) * three - four * five'
z0001=:four*five
z0002=:one+two
z0003=:z0002*three
z0004=:z0003-z0001
z0004
```


See also http://www.jsoftware.com/svn/base/trunk/packages/misc/trace.ijs for a model of the underlying parser being employed here.
