#################################################
### Assignment 4 --- Object-Oriented Programs ###
###                                           ###
###   Organization of Programming Languages   ###
###                 Fall 2025                 ###
###                Paul Downen                ###
#################################################

# For each of the exercises below, follow the instructions to fill in
# the missing pieces of code, marked by the comment looking like
#
#     ### FILL IN HERE ###
# 
# to complete the program.

# The end of each exercise comes with a list of examples that will
# print out a result partially based on the code you give for your
# answer.  As a quick check to see if you completed each exercise
# correctly, see if the given examples print out the correct result.

# To run this program and check your answers, you can open a
# terminal/command prompt in the same directory that you store this
# file and use the command
#
#     ruby assignment4.rb
#
# which will display any printed messages before ending.
# Alternatively, you can start an interactive ruby interpreter in a
# terminal/command prompt (again, inside the same directory as this
# file) using the command
#
#     irb
#
# Then you can (re)load this file into irb by the Ruby expression
#
#     load "assignment4.rb"
#
# which will print out any messages written by `puts` in this program,
# and then bring into scope function names (like
# `positive_negative_zero`) and class names (like `Bird`) written
# below for you to play with in the interpreter.  For example, after
# loading "assignment4.rb" into your interpreter, try running the code
#
#     Dog.new("Spot").fetch("a ball")



################
## Exercise 1 ##
################

# Complete the following definition of the function
# `positive_negative_zero` that takes a sequence of numbers, and for
# each one, prints whether that number is positive, negative, or
# exactly equal to zero.
def positive_negative_zero(numbers)
  for x in numbers
    ### FILL IN HERE ###
    # Depending on the value of x, use `puts` to do one of the following:
    #   * If x is greater than 0, put "x is positive"
    #   * If x is less than 0, put "x is negative"
    #   * If x is exactly 0, put "x is zero"
    # In each case, be sure to fill in the actual value of x into the string.
  end
end

# HINT: You might find the comparison operators >, <, and == helpful
# to complete this code. For example:
#
#      10 > 0 returns true
#       0 > 0 returns false
#     -10 > 0 returns false
#
#      10 < 0 returns false
#       0 > 0 returns false
#     -10 < 0 returns true
#
#      10 == 0 returns false
#       0 == 0 returns true
#     -10 == 0 returns false

puts ""
puts "Exercise 1: positive_negative_zero(-5..5)"
puts "==========="
positive_negative_zero(-5..5)
# This should print several lines looking like:
# -5 is negative
# ...
# 0 is zero
# ...
# 5 is positive


################
## Exercise 2 ##
################

# Complete the following definition of the Bird class by filling in
# the missing code in its `speak` and `fly` methods.  You may use the
# included definition of the Fish class as an example to help.

class Animal
  def speak
    puts "..."
  end
end

class Fish < Animal
  def speak
    puts '"blub..."'
  end

  def swim
    puts "The fish swims"
  end
end

class Bird < Animal
  def speak
    ### FILL IN HERE ###
    # Print out the string '"chirp"' when this method is called
  end

  def fly
    ### FILL IN HERE ###
    # Print out the string 'The bird takes flight!' when this method is called
  end
end

puts ""
puts "Exercise 2: examples"
puts "==========="

nemo = Fish.new
nemo.speak  # Prints: "blub..."
nemo.swim   # Prints: The fish swims

jeremy = Bird.new
jeremy.speak  # Should print: "chirp!"
jeremy.fly    # Should print: The bird takes flight!


################
## Exercise 3 ##
################

# Complete the following definition of the Human class by filling in
# the missing code in its `speak` and `think` methods.  You may use
# the included definition of the Dog class as an example.

class Named < Animal
  attr_accessor :name
  
  def initialize(name)
    @name = name
  end
  
  def speak(phrase = "...")
    puts "#{self.name} says '#{phrase}'"
  end
end

class Dog < Named
  def speak
    super("woof!")
  end

  def fetch(thing)
    puts "#{self.name} fetches #{thing}!"
  end
end

class Human < Named
  def speak
    ### FILL IN HERE ###
    # Use the `super` keyword to call the `speak` method from the
    # superclass Named (given above) with the parameter "hello"
  end

  def think(subject)
    ### FILL IN HERE ###
    # Print the message "name thinks really hard about subject",
    # making sure to plug in the given string subject in place of the
    # word "subject" and plug in this Human's name in place of "name"
  end
end

puts ""
puts "Exercise 3: examples"
puts "==========="

lassie = Dog.new("Lassie")
lassie.speak                  # Prints: Lassie says 'woof!'
lassie.fetch("a stick")       # Prints: Lassie fetches a stick!
lassie.fetch("Little Timmy")  # Prints: Lassie fetches Little Timmy!

socrates = Human.new("Socrates")
socrates.speak                # Should print: Socrates says 'hello'
socrates.think("philosophy")  # Should print: Socrates thinks really hard about philosophy
socrates.think("lunch")       # Should print: Socrates thinks really hard about lunch

################
## Exercise 4 ##
################

# Complete the Times and Divide classes below by filling in the
# missing code for their `eval` and `to_s` methods.  You may use the
# included definitions of the Num, Plus, and Minus classes as
# reference examples to help.

class ArithExpr
  def type
    "Arithmetic Expression"
  end
end

class Num < ArithExpr
  attr_accessor :value
  
  def initialize(n)
    @value = n
  end

  def eval
    self.value
  end

  def to_s
    "#{self.value}"
  end
end

class Plus < ArithExpr
  attr_accessor :left, :right

  def initialize(l, r)
    @left = l
    @right = r
  end

  def eval
    self.left.eval + self.right.eval
  end

  def to_s
    "(#{self.left}) + (#{self.right})"
  end
end

class Minus < ArithExpr
  attr_accessor :left, :right

  def initialize(l, r)
    @left = l
    @right = r
  end

  def eval
    self.left.eval - self.right.eval
  end

  def to_s
    "(#{self.left}) - (#{self.right})"
  end
end

class Times < ArithExpr
  attr_accessor :left, :right

  def initialize(l, r)
    @left = l
    @right = r
  end

  def eval
    ### FILL IN HERE ###
    # Return result of evaluating this object's left and right
    # sub-expressions, and then multiplying those two numbers
    # together with the * operator.
  end

  def to_s
    ### FILL IN HERE ###
    # Return a string that has the form "(left) * (right)", where the
    # words "left" and "right" are replaced by strings taken from this
    # object's left and right sub-expressions.
  end
end

class Divide < ArithExpr
  attr_accessor :left, :right

  def initialize(l, r)
    @left = l
    @right = r
  end

  def eval
    ### FILL IN HERE ###
    # Return result of evaluating this object's left and right
    # sub-expressions, and then dividing those two numbers with the /
    # operator.
  end

  def to_s
    ### FILL IN HERE ###
    # Return a string that has the form "(left) / (right)", where the
    # words "left" and "right" are replaced by strings taken from this
    # object's left and right sub-expressions.
  end
end

# Here are some shorthand names for referring to just simple Number
# expressions.
Zero = Num.new(0)
One = Num.new(1)
Two = Num.new(2)
Three = Num.new(3)
Four = Num.new(4)
Five = Num.new(5)
Six = Num.new(6)

puts ""
puts "Exercise 4: examples"
puts "==========="

ex4_1 = Times.new(Plus.new(Five, Four), Three)
ex4_2 = Divide.new(Minus.new(Five, One), Two)
ex4_3 = Minus.new(Plus.new(Five, One), Times.new(Two, Two))

for example in [ex4_1, ex4_2, ex4_3]
  puts "#{example} = #{example.eval}  (#{example.type})"
end
# This should print:
# ((5) + (4)) * (3) = 27    (Arithmetic Expression)
# ((5) - (1)) / (2) = 2    (Arithmetic Expression)
# ((5) + (1)) - ((2) * (2)) = 2    (Arithmetic Expression)


################
## Exercise 5 ##
################

# Complete the And, IsZero, and If classes below by filling in the
# missing code for:
#
#   * And's `initialize`, `eval`, and `to_s` methods,
#   * IsZero's `eval` and `to_s` methods, and
#   * If's `eval` method.
#
# You may use the included definitions of the Bool, Or, and Geq
# classes as reference examples to help.

class CondExpr
  def type
    "Conditional Expression"
  end
end

class Bool < CondExpr
  attr_accessor :value
  
  def initialize(n)
    @value = n
  end

  def eval
    self.value
  end

  def to_s
    "#{self.value}"
  end
end

class Or < CondExpr
  attr_accessor :left, :right

  def initialize(l, r)
    @left = l
    @right = r
  end

  def eval
    if self.left.eval
    then
      true
    else
      self.right.eval
    end
  end

  def to_s
    "(#{self.left}) or (#{self.right})"
  end
end

class And < CondExpr
  attr_accessor :left, :right

  def initialize(l, r)
    ### FILL IN HERE ###
    # Initialize a new And object by setting its @left instance
    # variable equal to the value of parameter l, and setting its
    # @right instance variable equal to the value of parameter r
  end

  def eval
    ### FILL IN HERE ###
    # Return the result of evaluating the boolean expression.  Make
    # sure to implement left-to-right short-circuiting:
    #
    #   * if this object's left sub-expression evaluates to false,
    #     then return false immediately without checking the right
    #     sub-expression;
    #
    #   * but if this object's left sub-expression instead evaluates
    #     to true, you must use the right sub-expression to figure out
    #     the answer
  end

  def to_s
    ### FILL IN HERE ###
    # Return a string that has the form "(left) and (right)", where the
    # words "left" and "right" are replaced by strings taken from this
    # object's left and right sub-expressions.
  end
end

class IsZero < CondExpr
  attr_accessor :subexpr

  def initialize(e)
    @subexpr = e
  end

  def eval
    ### FILL IN HERE ###
    # evaluate this object's only subexpression (which should return a
    # number), and return true if that number is equal (==) to zero,
    # or false otherwise.
  end

  def to_s
    ### FILL IN HERE ###
    # Return a string that has the form "zero?(subexpr)" where the
    # word "subexpr" is replaced by the string given for this objects
    # only sub-expression.
  end
end

class Geq < CondExpr
  attr_accessor :left, :right

  def initialize(l, r)
    @left = l
    @right = r
  end

  def eval
    self.left.eval >= self.right.eval
  end

  def to_s
    "(#{self.left}) >= (#{self.right})"
  end
end

class If < ArithExpr
  attr_accessor :check, :then, :else

  def initialize(check, if_true, if_false)
    @check = check
    @then = if_true
    @else = if_false
  end

  def eval
    ### FILL IN HERE ###
    # Decide which of the two arithmetic sub-expressions (self.then or
    # self.else) to evaluate depending on if the boolean
    # sub-expression (self.check) evaluates to true or false.
    #
    #   * If self.check.eval returns true, then return the result of
    #     evaluating self.then
    #
    #   * Otherwise, return the result of evaluating self.else
  end

  def to_s
    "if #{self.check} then #{self.then} else #{self.else}"
  end
end

True = Bool.new(true)
False = Bool.new(false)

puts ""
puts "Exercise 5: examples"
puts "==========="

ex5_1 = IsZero.new(Minus.new(Plus.new(Two,Three), Five))
ex5_2 = And.new(False, Divide.new(One, Zero))
ex5_3 = If.new(IsZero.new(Minus.new(Plus.new(One,One),Two)),
               Divide.new(Four,Two),
               Divide.new(Four,Minus.new(Plus.new(One,One),Two)))
ex5_4 = If.new(IsZero.new(Times.new(Plus.new(Three,Four), Minus.new(One,One))),
               Minus.new(Five,Three),
               Divide.new(Six,Two))

for example in [ex5_1, ex5_2, ex5_3, ex5_4]
  puts "#{example} = #{example.eval}    (#{example.type})"
end
# This should print:
# zero?(((2) + (3)) - (5)) = true    (Conditional Expression)
# (false) and ((1) / (0)) = false    (Conditional Expression)
# if zero?(((1) + (1)) - (2)) then (4) / (2) else (4) / (((1) + (1)) - (2)) = 2    (Arithmetic Expression)
# if zero?(((3) + (4)) * ((1) - (1))) then (5) - (3) else (6) / (2) = 2    (Arithmetic Expression)
