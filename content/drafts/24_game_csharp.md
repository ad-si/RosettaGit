+++
title = "24 game/CSharp"
description = ""
date = 2014-04-15T17:26:50Z
aliases = []
[extra]
id = 7148
[taxonomies]
categories = []
tags = []
+++

The C# language does not directly contain an Eval function for evaluating a string as a math expression, though there are still a number of ways to go about it.

You could, for example, use the CodeDOM to dynamically compile an object that contains the expression string.

Or, while not necessarily a good coding practice, but certainly a short and simple route, you could use System.Xml.XPath.XPathNavigator.Evaluate(string xpath) as shown here:

```csharp

public class XPathEval : I24MathParser {
	public float Evaluate(string expression) {
		System.Xml.XPath.XPathNavigator navigator = 
			new System.Xml.XPath.XPathDocument(new System.IO.StringReader("<r/>")).CreateNavigator();

		//expath evaluator needs 
		//	'/' expressed as "div" 
		//	'%' expressed as "mod"
		string xpathExpression = expression.Replace("/", " div ").Replace("%", " mod ");
		float answer = Convert.ToSingle(navigator.Evaluate(String.Format("number({0})", xpathExpression)));

		return answer;
	}  	
}

```



The XPathEval class is implementing this interface to facilitate swapping out Evaluate providers:

```csharp

interface I24MathParser {
	float Evaluate(string expression);
}

```



Here is a more verbose, native solution - a lightweight math expression parser for evaluating 24 Game user input:

```csharp

/// <summary>
/// Lightweight math parser - C# does not have an Evaluate function
/// </summary>
public class MathParser : I24MathParser {
	//used to translate brackets to implied multiplication - i.e. "3(4)5(6)" will be interpreted as "3*(4)*5*(6)"
	private const string bracketsPattern = @"(?<=[0-9)])(?<rightSide>\()|(?<=\))(?<rightSide>[0-9])";
		
	//finds multiplication or division sub expression - i.e. "4*8-4*2)" yields {"4*8", "4*2"}
	private const string multiplyDividePattern = @"[0-9]+[/*][0-9]+";
		
	//finds bracketed expressions - i.e. "(4+30)(10-1)" yields {"4+30", "10-1"}
	private const string subExpressionPattern = @"\(([0-9/*\-+]*)\)";
		
	//splits expression into it elements - i.e. "4+-30-4.123" yields {"4", "+", "-30" ,"-", "4.123"}
	private const string tokenPattern = @"(?:(?<=[/*\-+]|^)[+-]?)?(?:[0-9]+(?:\.[0-9]*)?)|[/*\-+]";

	Regex brackets;
	Regex multiplyDivide;
	Regex subExpression;
	Regex token;


	public MathParser() {
		//initialize reusable regular expressions
		brackets = new Regex(bracketsPattern, RegexOptions.Compiled);
		subExpression = new Regex(subExpressionPattern, RegexOptions.Compiled);
		token = new Regex(tokenPattern, RegexOptions.Compiled);
		multiplyDivide = new Regex(multiplyDividePattern, RegexOptions.Compiled);
	}


	public float Evaluate(string input) {
		//brackets with no operator implies multiplication
		string equation = brackets.Replace(input, "*${rightSide}");
		float answer = Solve(equation);
			
		return answer;
	}


	float Solve(string equation) {
		//carry out order of operations
		//	bracketed subexpressions - for any operator
		equation = SolveSubExpressions(subExpression, equation);

		//	multiplication and division
		equation = SolveSubExpressions(multiplyDivide, equation);

		//	addition and subtraction
		float answer = ParseEquation(equation);

		return answer;
	}
		

	string SolveSubExpressions(Regex subExpression, string equation) {
		float subResult;
		Match match = subExpression.Match(equation);

		while (match.Success) {
			if (match.Groups[1].Length > 0) {
				//recursively solve for subexpressions -- match group 1 excludes outer brackets
				subResult = Solve(match.Groups[1].Value);
			}
			else {
				//no more nested expressions - get final result for this subExpression
				subResult = ParseEquation(match.Value);
			}


			//replace subexpression with resolved answer
			equation = equation.Replace(match.Value, subResult.ToString());

			//retest updated equation string
			match = subExpression.Match(equation);
		}

		return equation;
	}


	float ParseEquation(string equation) {
		Match match = token.Match(equation);
		float leftSide = leftSide = float.Parse(match.Value);
		string symbol;
		float rightSide;
		match = match.NextMatch();

		while (match.Success) {
			symbol = match.Value;
			match = match.NextMatch();

			if (match.Success)
			{
				rightSide = float.Parse(match.Value);
				leftSide = Calculate(leftSide, symbol, rightSide);
				match = match.NextMatch();
			}
		} 

		return leftSide;
	}


	float Calculate(float leftSide, string symbol, float rightSide) {
		float answer;

		switch (symbol) {
			case "/":
				answer = leftSide / rightSide;
				break;

			case "*":
				answer = leftSide * rightSide;
				break;

			case "-":
				answer = leftSide - rightSide;
				break;

			case "+":
				answer = leftSide + rightSide;
				break;

			default:
				throw new ArgumentException();
		}

		return answer;
	}
}

```



This is the main class that handles puzzle generation and user interaction

```csharp

/// <summary>
/// The Game.  Handles user interaction and puzzle generation.
/// </summary>
class TwentyFourGame {
	//puzzle parameters
	private const int listSize = 4;
	private const int minValue = 1;
	private const int maxValue = 9;

	//signals end of game
	private const string quitToken = "Q";

	//the only valid puzzle solution
	private const float targetValue = 24;

	//Regular Expressions for evaluating math input
	private const string dictionaryBlacklistPattern = @"[^1-9/*\-+()]";
	private const string inputDigitsPattern = @"(?:(?<=[+-]|^)[+-]?)?(?:[0-9]+(?:\.[0-9]*)?)";
	Regex dictionaryBlackList;
	Regex inputDigits;
	I24MathParser mathParser;

	public TwentyFourGame() {
		//initialize reusable regular expressions
		dictionaryBlackList = new Regex(dictionaryBlacklistPattern, RegexOptions.Compiled);
		inputDigits = new Regex(inputDigitsPattern, RegexOptions.Compiled);

		//define instance of math evaluator provider
		//custom parser
		//mathParser = new MathParser();

		//xpath parser
		mathParser = new XPathEval();
	}


	static void Main(string[] args)	{
		TwentyFourGame game = new TwentyFourGame();
		game.PrintInstructions();
		game.PlayGame();
	}


	void PlayGame()	{
		string input;
		bool endGame = false;
			

		//repeat play cycle until user signals the end
		do {
			string puzzle = GetPuzzle();
			bool isValid = false;

			//continue prompting user until valid input is received
			do {
				float answer;
				string message = String.Empty;

				try {
					//show user puzzle and get read their solution
					input = GetInput(puzzle);

					if (input.Length == 0) {
						//skip current puzzle - perhaps there is no solution
						isValid = true;
						message = "Skipping this puzzle";
					}
					else if (String.Compare(input, quitToken, true) == 0) {
						//user wishes to quit
						isValid = true;
						message = "End Game";
					}
					else if (ValidateInput(input, puzzle)) {
						//interpret user input and calculate answer
						answer = mathParser.Evaluate(input);

						if (answer == targetValue) {
							isValid = true;
							message = String.Format("Good work.  {0} = {1}.", input, answer);
						}
						else {
							isValid = false;
							message = String.Format("Incorrect.  {0} = {1}.  Try again.", input, answer);
						}
					}
					else {
						isValid = false;
						message = "Invalid input.  Try again.";
					}
				}
				catch {
					message = "An error occurred.  Check your input and try again.";
					isValid = false;
				}
				finally {
					PrintMessage(message);
					PrintMessage(String.Empty);
					PrintMessage(String.Empty);
				}
			} while (!isValid);
		} while (!endGame);

		//pause
		GetInput(String.Empty);
	}

	
	bool ValidateInput(string input, string puzzle) {
		bool isValid;

		if (dictionaryBlackList.IsMatch(input)) {
			//illegal characters used
			isValid = false;
		}
		else {
			//get inputted digits and compare to those in puzzle
			string inputNumbers = String.Join(" ", from Match m in inputDigits.Matches(input) orderby float.Parse(m.Value) select m.Value);

			isValid = inputNumbers.CompareTo(puzzle) == 0;
		}

		return isValid;
	}


	string GetPuzzle() {
		int[] digits = new int[listSize];

		//randomly choose 4 digits (from 1 to 9)
		Random rand = new Random();

		for (int i = 0; i < digits.Length; i++) {
			digits[i] = rand.Next(minValue, maxValue);
		}

		//format for display
		Array.Sort(digits);
		string puzzle = String.Join(" ", digits);
		return puzzle;
	}


	string GetInput(string prompt) {
		Console.Write(String.Concat(prompt, ":  "));
		return Console.ReadLine();
	}


	void PrintMessage(string message) {
		Console.WriteLine(message);
	}


	void PrintInstructions() {
		PrintMessage("--------------------------------- 24 Game ---------------------------------");
		PrintMessage(String.Empty);
		PrintMessage("------------------------------- Instructions ------------------------------");
		PrintMessage("Four digits will be displayed.");
		PrintMessage("Enter an equation using all of those four digits that evaluates to 24");
		PrintMessage("Only * / + - operators and () are allowed");
		PrintMessage("Digits can only be used once, but in any order you need.");
		PrintMessage("Digits cannot be combined - i.e.: 12 + 12 when given 1,2,2,1 is not allowed");
		PrintMessage("Submit a blank line to skip the current puzzle.");
		PrintMessage("Type 'Q' to quit");
		PrintMessage(String.Empty);
		PrintMessage("Example: given 2 3 8 2, answer should resemble 8*3-(2-2)");
		PrintMessage("---------------------------------------------------------------------------");
		PrintMessage(String.Empty);
		PrintMessage(String.Empty);
	}
}

```



Example output:

```txt

--------------------------------- 24 Game ---------------------------------

------------------------------- Instructions ------------------------------
Four digits will be displayed.
Enter an equation using all of those four digits that evaluates to 24
Only * / + - operators and () are allowed
Digits can only be used once, but in any order you need.
Digits cannot be combined - i.e.: 12 + 12 when given 1,2,2,1 is not allowed
Submit a blank line to skip the current puzzle.
Type 'Q' to quit

Example: given 2 3 8 2, answer should resemble 8*3-(2-2)
---------------------------------------------------------------------------


1 3 3 4:  4*(3-1)*3
Good work.  4*(3-1)*3 = 24.


1 3 5 6:

```

