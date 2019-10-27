+++
title = "Parsing/RPN/Ruby"
description = ""
date = 2011-12-06T20:10:08Z
aliases = []
[extra]
id = 11012
[taxonomies]
categories = []
tags = []
+++

[[Category:Ruby]]
Code for parsing and evaluating RPN expressions. See 
* [[Parsing/RPN calculator algorithm]]
* [[Parsing/RPN to infix conversion]]
* [[Parsing/Shunting-yard algorithm]]


```ruby
class RPNExpression

  # Set up the table of known operators
  Operator = Struct.new(:precedence, :associativity, :english, :ruby_operator)
  class Operator
    def left_associative?; associativity == :left; end
    def <(other)
      if left_associative? 
        precedence <= other.precedence
      else 
        precedence < other.precedence
      end
    end
  end

  Operators = {
    "+" => Operator.new(2, :left, "ADD", "+"),
    "-" => Operator.new(2, :left, "SUB", "-"),
    "*" => Operator.new(3, :left, "MUL", "*"),
    "/" => Operator.new(3, :left, "DIV", "/"),
    "^" => Operator.new(4, :right, "EXP", "**"),
  }

  # create a new object
  def initialize(str)
    @expression = str
    @infix_tree = nil
    @value = nil
  end
  attr_reader :expression
  
  # convert an infix expression into RPN
  def self.from_infix(expression)
    debug "\nfor Infix expression: #{expression}\nTerm\tAction\tOutput\tStack"
    rpn_expr = []
    op_stack = []
    tokens = expression.split
    until tokens.empty?
      term = tokens.shift

      if Operators.has_key?(term)
        op2 = op_stack.last
        if Operators.has_key?(op2) and Operators[term] < Operators[op2]
          rpn_expr << op_stack.pop
          debug "#{term}\t#{Operators[op2].english}\t#{rpn_expr}\t#{op_stack}\t#{op2} has higher precedence than #{term}"
        end
        op_stack << term
        debug "#{term}\tPUSH OP\t#{rpn_expr}\t#{op_stack}"

      elsif term == "("
        op_stack << term
        debug "#{term}\tOPEN_P\t#{rpn_expr}\t#{op_stack}"

      elsif term == ")"
        until op_stack.last == "("
          rpn_expr << op_stack.pop
          debug "#{term}\t#{Operators[rpn_expr.last].english}\t#{rpn_expr}\t#{op_stack}\tunwinding parenthesis"
        end
        op_stack.pop
        debug "#{term}\tCLOSE_P\t#{rpn_expr}\t#{op_stack}"

      else
        rpn_expr << term
        debug "#{term}\tPUSH V\t#{rpn_expr}\t#{op_stack}"
      end
    end
    until op_stack.empty?
      rpn_expr << op_stack.pop
    end
    obj = self.new(rpn_expr.join(" "))
    debug "RPN = #{obj.to_s}"
    obj
  end

  # calculate the value of an RPN expression
  def eval
    return @value unless @value.nil?

    debug "\nfor RPN expression: #{expression}\nTerm\tAction\tStack"
    stack = []
    expression.split.each do |term|
      if Operators.has_key?(term)
        a, b = stack.pop(2)
        raise ArgumentError, "not enough operands on the stack" if b.nil?
        a = a.to_f if term == "/"
        op = (term == "^" ? "**" : term)
        stack.push(a.method(op).call(b))
        debug "#{term}\t#{Operators[term].english}\t#{stack}"
      else
        begin
          number = Integer(term) rescue Float(term)
        rescue ArgumentError
          raise ArgumentError, "cannot handle term: #{term}"
        end
        stack.push(number)
        debug "#{number}\tPUSH\t#{stack}"
      end
    end
    @value = stack.pop
    debug "Value = #@value"
    @value
  end

  private
  # convert an RPN expression into an AST
  def to_infix_tree
    return @infix_tree unless @infix_tree.nil?

    debug "\nfor RPN expression: #{expression}\nTerm\tAction\tStack"
    stack = []
    expression.split.each do |term|
      if Operators.has_key?(term)
        a, b = stack.pop(2)
        raise ArgumentError, "not enough operands on the stack" if b.nil?
        op = InfixNode.new(term)
        op.left = a
        op.right = b
        stack.push(op)
        debug "#{term}\t#{Operators[term].english}\t#{stack.inspect}"
      else
        begin
          Integer(term) rescue Float(term)
        rescue ArgumentError
          raise ArgumentError, "cannot handle term: #{term}"
        end
        stack.push(InfixNode.new(term))
        debug "#{term}\tPUSH\t#{stack.inspect}"
      end
    end
    @infix_tree = stack.pop
  end

  public
  # express the AST as a string
  def to_infix
    expr = to_infix_tree.to_s
    debug "Infix = #{expr}"
    expr
  end

  # express the AST as a string, but in a form that allows Ruby to evaluate it
  def to_ruby
    expr = to_infix_tree.to_ruby
    debug "Ruby = #{expr}"
    expr
  end

  def to_s
    expression
  end


  private
  class InfixNode
    def initialize(value)
      @value = value
      @left = nil
      @right = nil
    end
    attr_reader :value
    attr_accessor :left, :right

    def leaf?
      left.nil? and right.nil?
    end

    def to_s;    to_string(false); end
    def to_ruby; to_string(true);  end 

    def to_string(to_ruby)
      result = []
      result << display_child(left, to_ruby, (to_ruby and value == "/"))
      result << (to_ruby ? Operators[value].ruby_operator : value)
      result << display_child(right, to_ruby)
      result.join(" ")
    end

    def display_child(child, to_ruby, need_float = false)
      result = if child.leaf?
                 child.value
               elsif Operators[child.value].precedence < Operators[value].precedence
                 "( #{child.to_string(to_ruby)} )"
               else
                 child.to_string(to_ruby)
               end
      result += ".to_f" if need_float
      result
    end

    def inspect
      str = "node[#{value}]"
      str << "<left=#{left.inspect}, right=#{right.inspect}>" unless leaf?
      str
    end
  end
end

def debug(msg)
  puts msg if $DEBUG
end


require 'test/unit'
class TestRPNExpression < Test::Unit::TestCase
  def setup
    @rpn_expr = "3 4 2 * 1 5 - 2 3 ^ ^ / +"
    @infix_expr = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
    @ruby_expr = "3 + 4 * 2.to_f / ( 1 - 5 ) ** 2 ** 3"
    @value = 3.0001220703125 
  end

  def test_eval
    rpn = RPNExpression.new @rpn_expr
    value = rpn.eval
    assert_equal @value, value
  end

  def test_rpn_to_infix
    rpn = RPNExpression.new @rpn_expr
    infix = rpn.to_infix
    assert_equal @infix_expr, infix
    assert_equal @value, eval(rpn.to_ruby)
  end

  def test_infix_to_rpn
    rpn = RPNExpression.from_infix @infix_expr
    assert_equal @rpn_expr, rpn.to_s
  end

  def test_other_expressions
    old_debug = $DEBUG
    $DEBUG = false

    rpn =   ["56 34 213.7 + * 678 -",     "1 56 35 + 16 9 - / +"]
    infix = ["56 * ( 34 + 213.7 ) - 678", "1 + ( 56 + 35 ) / ( 16 - 9 )"]
    value = ["13193.2",                   "14.0"]

    [0, 1].each do |idx|
      obj = RPNExpression.new rpn[idx]
      assert_equal value[idx], "%.1f" % obj.eval
      assert_equal infix[idx], obj.to_infix
      assert_equal value[idx], "%.1f" % eval(obj.to_ruby)
      obj = RPNExpression.from_infix infix[idx]
      assert_equal rpn[idx], obj.to_s
    end
    $DEBUG = old_debug 
  end
end
```


Running with $DEBUG on (<code>ruby -d rpn.rb</code>) gives:

```txt
Run options: 

# Running tests:


for RPN expression: 3 4 2 * 1 5 - 2 3 ^ ^ / +
Term	Action	Stack
3	PUSH	[3]
4	PUSH	[3, 4]
2	PUSH	[3, 4, 2]
*	MUL	[3, 8]
1	PUSH	[3, 8, 1]
5	PUSH	[3, 8, 1, 5]
-	SUB	[3, 8, -4]
2	PUSH	[3, 8, -4, 2]
3	PUSH	[3, 8, -4, 2, 3]
^	EXP	[3, 8, -4, 8]
^	EXP	[3, 8, 65536]
/	DIV	[3, 0.0001220703125]
+	ADD	[3.0001220703125]
Value = 3.0001220703125
.
for Infix expression: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Term	Action	Output	Stack
3	PUSH V	["3"]	[]
+	PUSH OP	["3"]	["+"]
4	PUSH V	["3", "4"]	["+"]
*	PUSH OP	["3", "4"]	["+", "*"]
2	PUSH V	["3", "4", "2"]	["+", "*"]
/	MUL	["3", "4", "2", "*"]	["+"]	* has higher precedence than /
/	PUSH OP	["3", "4", "2", "*"]	["+", "/"]
(	OPEN_P	["3", "4", "2", "*"]	["+", "/", "("]
1	PUSH V	["3", "4", "2", "*", "1"]	["+", "/", "("]
-	PUSH OP	["3", "4", "2", "*", "1"]	["+", "/", "(", "-"]
5	PUSH V	["3", "4", "2", "*", "1", "5"]	["+", "/", "(", "-"]
)	SUB	["3", "4", "2", "*", "1", "5", "-"]	["+", "/", "("]	unwinding parenthesis
)	CLOSE_P	["3", "4", "2", "*", "1", "5", "-"]	["+", "/"]
^	PUSH OP	["3", "4", "2", "*", "1", "5", "-"]	["+", "/", "^"]
2	PUSH V	["3", "4", "2", "*", "1", "5", "-", "2"]	["+", "/", "^"]
^	PUSH OP	["3", "4", "2", "*", "1", "5", "-", "2"]	["+", "/", "^", "^"]
3	PUSH V	["3", "4", "2", "*", "1", "5", "-", "2", "3"]	["+", "/", "^", "^"]
RPN = 3 4 2 * 1 5 - 2 3 ^ ^ / +
..
for RPN expression: 3 4 2 * 1 5 - 2 3 ^ ^ / +
Term	Action	Stack
3	PUSH	[node[3]]
4	PUSH	[node[3], node[4]]
2	PUSH	[node[3], node[4], node[2]]
*	MUL	[node[3], node[*]<left=node[4], right=node[2]>]
1	PUSH	[node[3], node[*]<left=node[4], right=node[2]>, node[1]]
5	PUSH	[node[3], node[*]<left=node[4], right=node[2]>, node[1], node[5]]
-	SUB	[node[3], node[*]<left=node[4], right=node[2]>, node[-]<left=node[1], right=node[5]>]
2	PUSH	[node[3], node[*]<left=node[4], right=node[2]>, node[-]<left=node[1], right=node[5]>, node[2]]
3	PUSH	[node[3], node[*]<left=node[4], right=node[2]>, node[-]<left=node[1], right=node[5]>, node[2], node[3]]
^	EXP	[node[3], node[*]<left=node[4], right=node[2]>, node[-]<left=node[1], right=node[5]>, node[^]<left=node[2], right=node[3]>]
^	EXP	[node[3], node[*]<left=node[4], right=node[2]>, node[^]<left=node[-]<left=node[1], right=node[5]>, right=node[^]<left=node[2], right=node[3]>>]
/	DIV	[node[3], node[/]<left=node[*]<left=node[4], right=node[2]>, right=node[^]<left=node[-]<left=node[1], right=node[5]>, right=node[^]<left=node[2], right=node[3]>>>]
+	ADD	[node[+]<left=node[3], right=node[/]<left=node[*]<left=node[4], right=node[2]>, right=node[^]<left=node[-]<left=node[1], right=node[5]>, right=node[^]<left=node[2], right=node[3]>>>>]
Infix = 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Ruby = 3 + 4 * 2.to_f / ( 1 - 5 ) ** 2 ** 3
.

Finished tests in 0.012002s, 333.2831 tests/s, 999.8494 assertions/s.

4 tests, 12 assertions, 0 failures, 0 errors, 0 skips
```

