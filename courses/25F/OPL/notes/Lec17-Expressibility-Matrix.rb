# coding: utf-8
#############################################
###       The Expressibility Matrix       ###
###                                       ###
### Organization of Programming Languages ###
###              Spring 2023              ###
###              Paul Downen              ###
#############################################

# Arithmetic and conditional syntax trees can be represented
# concretely in Ruby as nested symbolic hash tables. The root of the
# syntax tree corresponds to a hash table with a single key-value
# pair: the key is a symbol which describes which type of syntax node
# it is, and the value is a (list of) subtree(s) within it.

#     n ::= 0 | 1 | 2 | 3 | ...
#     A ::= n | plus(A, A) | minus(A, A)
#         | if B then A else A
#
#     b ::= true | false
#     B ::= b | or(B, B) | geq?(A, A)

# You can map the grammatical rules of A and B to symbolic hash tables
# like so:
#
#     n                    ==>  {num: n}    (for some number n)
#     plus(left, right)    ==>  {plus: [left, right]}
#     minus(left, right)   ==>  {minus: [left, right]}
#     if(chk, thn, els)    ==>  {if: [chk, thn, els]}
#
#     b                    ==>  {bool: b}    (for b = true or b = false)
#     or(left, right)      ==>  {or: [left, right]}
#     geq?(left,right)     ==>  {geq?: [left, right]}

# Here are some examples of arithmetic and conditional expressions
# represented as nested hash-tables using the rules above:

# ex1 = (5 + 4) - 3
# ex1 = minus(plus(5,4),3)
ex1   = {minus: [{plus: [{num: 5}, {num: 4}]},
                 {num: 3}]}

# ex2 = 5 + 1 - (2 + 2)
# ex2 = minus(plus(5,1),plus(2,2))
ex2   = {minus: [{plus: [{num: 5}, {num: 1}]},
                 {plus: [{num: 2}, {num: 2}]}]}

# ex3 = if 1 + 1 >= 2 then (1 + 1) - 2 else 0
# ex3 = if(geq?(plus(1,1),2), minus(plus(1,1), 2), 0)
ex3   = {if: [{geq?: [{plus: [{num: 1}, {num: 1}]},
                      {num: 2}]},
              {minus: [{plus: [{num: 1}, {num: 1}]},
                       {num: 2}]},
              {num: 0}]}

# ex4 = true or 0 - 2 >= 0
# ex4 = or(true,geq?(minus(0,2), 0))
ex4   = {or: [{bool: true},
              {geq?: [{minus: [{num: 0}, {num: 2}]},
                      {num: 0}]}]}

# ex5 = 3 >= 4 or 4 >= 5
# ex5 = or(geq?(3, 4), geq?(4, 5))
ex5   = {or: [{geq?: [{num: 3}, {num: 4}]},
              {geq?: [{num: 4}, {num: 5}]}]}

arith_examples = [ex1, ex2, ex3]
cond_examples = [ex4, ex5]
examples = arith_examples + cond_examples

examples.each { |ex| puts ex }


def eval_arith(expr)
  case expr
  # —————
  # n ⇓ n

  in {num: n}
    # To evaluate a numeric expression, just return that number.
    n

  # A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ + n₂ = n
  # ——————————————————————————————
  # plus(A₁, A₂) ⇓ n

  in {plus: [left, right]}
    # To evaluate a `:plus` operation, evaluate the two
    # sub-expressions and then add the numbers you get back.
    eval_arith(left) + eval_arith(right)

  # A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ - n₂ = n  n₁ ≥ n₂
  # ——————————————————————————————————————
  # minus(A₁, A₂) ⇓ n

  in {minus: [left, right]}
    # To evaluate a `:minus` operation, evaluate the two
    # sub-expressions, and then check the side condition to make sure
    # you don't get a negative number before returning their
    # difference.
    n1 = eval_arith(left)
    n2 = eval_arith(right)
    return n1 - n2 if n1 >= n2

  # B ⇓ true  A₁ ⇓ n₁
  # —————————————————————————
  # if B then A₁ else A₂ ⇓ n₁

  # B ⇓ false  A₂ ⇓ n₂
  # —————————————————————————
  # if B then A₁ else A₂ ⇓ n₂

  in {if: [check, thn, els]}
    # To evaluate an if-then-else expression, first evaluate the
    # conditional check: if it returns `true` (only) evaluate the
    # `thn` branch and return its result, otherwise if it returns
    # `false` (only) evaluate the `els` branch and return its result.
    # Notice that it never happens that you evaluate *both* the `thn`
    # and `els` branches.
    if eval_cond(check)
    then eval_arith(thn)
    else eval_arith(els)
    end
  end
end

def eval_cond(expr)
  case expr
  # —————
  # b ⇓ b

  in {bool: b}
    # To evaluate a literal boolean expression, just return its value.
    b

  # B₁ ⇓ true
  # —————————————————
  # or(B₁, B₂) ⇓ true

  # B₁ ⇓ false  B₂ ⇓ b₂
  # ———————————————————
  # or(B₁, B₂) ⇓ b₂

  in {or: [left, right]}
    # To evaluate an `:or` operation, first check the result of
    # evaluating the left sub-expression: if it's true then return
    # true right away, but if it's false you need to evaluate the
    # right sub-expression to see if the combination is true or false.
    if eval_cond(left)
    then true
    else eval_cond(right)
    end

  # A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ ≥ n₂
  # —————————————————————————
  # geq?(A₁, A₂) ⇓ true

  # A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ < n₂
  # —————————————————————————
  # geq?(A₁, A₂) ⇓ false

  in {geq?: [left, right]}
    # To evaluate a `:geq?` operation, evaluate the matching left and
    # right sub-expressions (which are arithmetic expressions) and
    # compare the numbers you get back to check if the first is
    # greater than or equal to the second.
    eval_arith(left) >= eval_arith(right)
  end
end

arith_examples.each { |ex| puts eval_arith(ex) }
cond_examples.each { |ex| puts eval_cond(ex) }


def ugly_print_arith(expr)
  case expr
  in {num: n}
    "#{n}"

  in {plus: [left, right]}
    "(#{ugly_print_arith(left)}) + (#{ugly_print_arith(right)})"

  in {minus: [left, right]}
    "(#{ugly_print_arith(left)}) - (#{ugly_print_arith(right)})"

  in {if: [chk, thn, els]}
    "if #{ugly_print_cond(chk)} then #{ugly_print_arith(thn)} else #{ugly_print_arith(els)}"
  end
end

def ugly_print_cond(expr)
  case expr

  in {bool: b}
    "#{b}"

  in {or: [left, right]}
    "(#{ugly_print_cond(left)}) or (#{ugly_print_cond(right)})"

  in {geq?: [left, right]}
    "(#{ugly_print_arith(left)}) >= (#{ugly_print_arith(right)})"
  end
end

arith_examples.each { |ex| puts ugly_print_arith(ex) }
cond_examples.each { |ex| puts ugly_print_cond(ex) }


def flatten_sum(expr)
  case expr
  in {plus: [left, right]}
    flatten_sum(left) + [:plus] + flatten_sum(right)

  in {minus: [left, right]}
    flatten_sum(left) + [:minus] + flatten_sum(right)

  else
    [expr]
  end
end

def pretty_print_summand(summ)
  case summ
  in :plus
    # The `:plus` operator is printed as "+"
    "+"

  in :minus
    # The `:minus` operator is printed as "-"
    "-"

  in {num: _}
    # A sub-expression which is a number (matching {num:...}) does not need to
    # be surrounded by parentheses when inside of a chain of additions and
    # subtractions...
    pretty_print_arith(summ)

  else
    # ... but any other arithmetic sub-expression *does* need to be
    # wrapped in parenthesis if it appears inside a chain of additions
    # and subtractions.
    "(#{pretty_print_arith(summ)})"
  end
end

def pretty_print_arith(expr)
  case expr
  in {num: n}
    # To pretty-print a numeric expression, just print that number.
    "#{n}"

  in {plus: _} | {minus: _}
    # To pretty-print an addition or subtraction expression (matching
    # {plus:...} or {minus:...}) do the following:
    #
    #   1. Flatten the expression into a list containing each piece of
    #      the full chain of `:plus` and `:minus` operations using
    #      `flatten_sum` from Exercise 4.
    #
    #   2. Transform each "summand" (the elements in the list from
    #      step 1) by printing each part individually with
    #      `pretty_print_summand` from Exercise 5.
    #
    #   3. Finally, join all the individual strings in the list
    #      together with a space " " in between them.
    #
    # This sequence of steps can be completed by the following
    # sequence of method calls: `flatten_sum(expr)` returns the list
    # of summands, invoking the `map` method on that list applies
    # `pretty_print_summand` to each individual summand `s` inside it
    # and returns a new list containing those strings, and then
    # finally the `join(" ")` method on the list of strings puts them
    # all together with a space in between each one.
    flatten_sum(expr)
      .map {|s| pretty_print_summand(s)}
      .join(" ")

  in {if: [chk, thn, els]}
    "if #{pretty_print_cond(chk)} then #{pretty_print_arith(thn)} else #{pretty_print_arith(els)}"
  end
end

def pretty_print_cond(expr)
  case expr
  in {bool: b}
    "#{b}"

  in {or: [left, right]}
    "(#{pretty_print_cond(left)}) and (#{pretty_print_cond(right)})"
    # We could do better here...

  in {geq?: [left, right]}
    "#{pretty_print_arith(left)} >= #{pretty_print_arith(right)}"
  end
end

arith_examples.each { |ex| puts pretty_print_arith(ex) }
cond_examples.each { |ex| puts pretty_print_cond(ex) }



# Now, compare the syntactic interpreter above (written in a
# functional style) with the compositional interpreter below (written
# in an object-oriented style) that we saw previously in class

class ArithExpr
  def type
    "Arithmetic Expression"
  end

  # Subclasses of ArithExpr should define a more specific method:

  # eval: returns a number (the result of evaluating the expression)
  
  # to_s: returns a string (written text representing the expression)
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
    n1 = self.left.eval
    n2 = self.right.eval
    n1 - n2 if n1 >= n2
  end

  def to_s
    "(#{self.left}) - (#{self.right})"
  end
end

class CondExpr
  def type
    "Conditional Expression"
  end

  # Similar to ArithExpr, subclasses of CondExpr should define two
  # more specific methods:

  # eval: returns a boolean (the result of evaluating the expression)

  # to_s: returns a string (written text representing the expression)
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
