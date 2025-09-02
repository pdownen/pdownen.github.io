# coding: utf-8
#############################################
###       Compositional Interpreter       ###
###                                       ###
### Organization of Programming Languages ###
###              Spring 2023              ###
###              Paul Downen              ###
#############################################

# Remember our small language of (Conditional) Arithmetic.


## Purely Arithmetic Language ##

# Syntax of arithmetic values (n) and expressions (A):
#
#     n ::= 0 | 1 | 2 | 3 | ...
#     A ::= n | Plus(A, A) | Minus(A, A) | If(B, A, A)

class ArithExpr
  def type
    "Arithmetic Expression"
  end

  # Subclasses of ArithExpr should define a more specific method:

  # eval: returns a number (the result of evaluating the expression)
end

# Each option in the syntax of A will get its own subclass defining an
# `eval` method that matches the rules of the big-step operational
# semantics.

# Big-step operational semantics of arithmetic expressions:

# —————
# n ⇓ n

class Num < ArithExpr
  attr_accessor :value

  def initialize(n)
    @value = n
  end

  def eval
    self.value
  end
end
	
# A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ + n₂ = n
# ——————————————————————————————
# Plus(A₁, A₂) ⇓ n

class Plus < ArithExpr
  attr_accessor :left, :right

  def initialize(l, r)
    @left = l
    @right = r
  end

  def eval
    self.left.eval + self.right.eval
  end
end

# A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ - n₂ = n  n₁ ≥ n₂
# ——————————————————————————————————————
# Minus(A₁, A₂) ⇓ n

class Minus < ArithExpr
  attr_accessor :left, :right

  def initialize(l, r)
    @left = l
    @right = r
  end

  def eval
    n1 = self.left.eval
    n2 = self.right.eval
    n1 - n2 if n1 >= n2
  end
end

# ex1 = Minus(Plus(5, 4), 6)

ex1 = Minus.new(Plus.new(Num.new(5), Num.new(4)), Num.new(6))

p ex1.type
p ex1
p ex1.eval

# ex2 = Plus(5, Minus(4, 6))
ex2 = Plus.new(Num.new(5), Minus.new(Num.new(4), Num.new(6)))

p ex2.type
p ex2
# p ex2.eval  # Error evaluating at run-time


## But what about looking at expressions?

# In addition to `eval`, every sublcass of `ArithExpr` should
# implement an appropriate `to_s` method for generating a string that
# corresponds to the syntax tree of the expression.  Let's just add
# them to the classes above...

class Num
  def to_s
    "#{self.value}"
  end
end

class Plus
  def to_s
    "(#{self.left}) + (#{self.right})"
  end
end

class Minus
  def to_s
    "(#{self.left}) - (#{self.right})"
  end
end

p ex1.to_s
p ex2.to_s


## Boolean Expression Language ##

# Syntax of boolean values (b) and expressions (B)
#
#     b ::= true | false
#     B ::= b | Or(B, B) | Geq(A, A)

# Big-step operational semantics of conditional expressions

class CondExpr
  def type
    "Conditional Expression"
  end

  # Similar to ArithExpr, subclasses of CondExpr should define two
  # more specific methods:

  # eval: returns a boolean (the result of evaluating the expression)

  # to_s: returns a string (written text representing the expression)
end

# —————
# b ⇓ b

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

# A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ ≥ n₂
# —————————————————————————
# Geq(A₁, A₂) ⇓ true

# A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ < n₂
# —————————————————————————
# Geq(A₁, A₂) ⇓ false

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

# B₁ ⇓ true
# —————————————————
# Or(B₁, B₂) ⇓ true

# B₁ ⇓ false  B₂ ⇓ b₂
# ———————————————————
# Or(B₁, B₂) ⇓ b₂

class Or < CondExpr
  attr_accessor :left, :right

  def initialize(l, r)
    @left = l
    @right = r
  end

  def eval
    b = self.left.eval
    
    if true == b
    then
      true
    elsif false == b
      self.right.eval
    end
  end

  def to_s
    "(#{self.left}) or (#{self.right})"
  end
end

ex3 = Or.new(Bool.new(false), Bool.new(false))

p ex3
p ex3.to_s
p ex3.eval

subexp = Geq.new(Num.new(0), Minus.new(Num.new(0), Num.new(1)))
ex4 = Or.new(Bool.new(true), subexp)
ex5 = Or.new(subexp, Bool.new(true))

p ex4.to_s
p ex4.eval
p ex5.to_s
# p ex5.eval

## Adding Conditional Arithmetic Expressions ##

# B ⇓ true  A₁ ⇓ n₁
# —————————————————————————
# if B then A₁ else A₂ ⇓ n₁

# B ⇓ false  A₂ ⇓ n₂
# —————————————————————————
# if B then A₁ else A₂ ⇓ n₂

class If < ArithExpr
  attr_accessor :check, :then, :else

  def initialize(check, if_true, if_false)
    @check = check
    @then = if_true
    @else = if_false
  end

  def to_s
    "if #{self.check} then #{self.then} else #{self.else}"
  end

  def eval
    # Decide which of the two arithmetic sub-expressions (self.then or
    # self.else) to evaluate depending on if the boolean
    # sub-expression (self.check) evaluates to true or false.
    #
    #   * If self.check.eval returns true, then return the result of
    #     evaluating self.then
    #
    #   * Otherwise, return the result of evaluating self.else
  end
end
