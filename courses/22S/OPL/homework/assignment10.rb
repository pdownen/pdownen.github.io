# coding: utf-8
################################################
### Assignment 10 --- Functional Interpreter ###
###                                          ###
###  Organization of Programming Languages   ###
###               Spring 2022                ###
###               Paul Downen                ###
################################################

# For each of the exercises below, follow the instructions to fill in
# the missing pieces of code, marked by the comment looking like
#
#     ### FILL IN HERE ###
# 
# to complete the program.  Each section marked as "Exercise" is a
# mandatory problem, and must be completed to get full credit for this
# assignment.  Each section marked as "Extra Credit" is optional;
# completing an Extra Credit problem is not required for full points,
# but will add to your overall grade.  If you complete some Extra
# Credit in addition to all required Exercises, you can earn over 100%
# for this assignment, which can help make up for any lost points from
# other assignments.

# The end of this file comes with a list of examples that will print
# out a result partially based on the code you give for your answer.
# As a quick check to see if you completed each exercise correctly,
# see if the given examples print out the correct result.

# To run this program and check your answers, you can open a
# terminal/command prompt in the same directory that you store this
# file and use the command
#
#     ruby assignment10.rb
#
# which will display any printed messages before ending.
# Alternatively, you can start an interactive ruby interpreter in a
# terminal/command prompt (again, inside the same directory as this
# file) using the command
#
#     irb
#
# Then you can (re)load this file into irb via the Ruby command
#
#     load "assignment10.rb"


##############
## Overview ##
##############

# For this assignment, you will be writing an interpreter for the
# simply-typed lambda calculus with numbers and booleans.  The grammar
# of this language's expression syntax is:

#     n ::= 0 | 1 | 2 | 3 | ...
#     b ::= true | false
#     x ::= any symbolic name
#     M ::= n | b | x | M M | λx:T.M | if M then M else M

# The grammar of values that might be returned by the interpreter is:

#     p#2 ::= any primitive operation expecting n arguments
#     V ::= n | b | [Γ]λx.M | p#n V₁...Vᵢ    (where i < n)
#     Γ ::= ε | Γ, x=>V

# Notice that in addition to the values for constants (n and b) and
# closures ([Γ]λx.M), the syntax of values also includes the partial
# application of primitive operations.  This looks like
#
#     p#2 V₁, V₂, V₃, ..., Vᵢ    (if i < n)
#
# which is the primitive operation `p` which expects `n` arguments,
# but so far has only been applied to `i` values V₁,...,Vᵢ which is
# less than `n`.  For example, we might have the `plus` primitive
# operation for adding two numbers.  The partial application of `plus`
# to only the number 10 would look like the value:
#
#     plus#2 10
#
# The annotation `#2` says that `plus` is expecting 2 arguments, so we
# know that `plus#2 10` is not a full application of `plus` yet
# because it has only been applied to one argument (the number `10`).
# If this partial application gets called later, then it can add the
# two numbers, for example:
#
#     (λf. f 25) (plus#2 10)
#
# will eventually call `plus` with the arguments 10 and 25 (in that
# order) to return the result 35.

# Just like in assignments 6 and 8, you will be representing syntax
# trees of expressions concretely as nested symbolic hash tables.  As
# a reminder, the root of the syntax tree corresponds to a hash table
# with a single key-value pair: the key is a symbol which describes
# which case of syntax node it is, and the value is a (list of)
# subtree(s) within it.

# The grammatical rules of expressions (M) correspond to the exact
# same symbolic hash tables from assignment 8, which are:

#                         n  ==>  {num: n}
#                         b  ==>  {bool: b}
#                         x  ==>  {var: :x}
#                     M₁ M₂  ==>  {app: [M₁, M₂]}
#                    λx:T.M  ==>  {fun: [{var: :x, type: T}, M]}
#     if M₁ then M₂ else M₃  ==>  {if: [M₁, M₂, M₃]}

# For example, remember this recursive function for printing out the
# hash-table representations of expressions as more readable strings.
def print_expr(expr)
  case expr
  in {num: n}
    "#{n}"
  in {bool: b}
    "#{b}"
  in {var: x}
    "#{x}"
  in {fun: [{var: x, type: t}, body]}
    "(λ#{x}:#{print_type(t)}. #{print_expr(body)})"
  in {fun: [{var: x}, body]}
    "(λ#{x}. #{print_expr(body)})"
  in {app: [function, argument]}
    "(#{print_expr(function)} #{print_expr(argument)})"
  in {if: [check, if_true, if_false]}
    "if #{print_expr(check)} then #{print_expr(if_true)} else #{print_expr(if_false)}"
  end
end
# Note that this time, there is an extra case that says what to do if
# a function parameter is not annotated with its type.

def print_type(ty)
  case ty
  in :number
    "number"
  in :boolean
    "boolean"
  in {arrow: [a, b]}
    "(#{print_type(a)}->#{print_type(b)})"
  end
end

# In contrast, you will be representing values abstract as objects
# that implement certain methods, rather than as concrete trees
# encoded as hash tables.  The more abstract choice of representation
# for values will make it easier in cases where different forms of
# values can appear in the same context.  For example, both closures
# `[Γ]λx.M` and primitive operations `p#n` can be called like
# functions, so they will both be represented as objects that respond
# to a `call` method in their own specific way.


# Here is the base class of values; by default Values are generic
# objects that don't look like anything in particular, raising an
# exception when you try to use them in some way.
class Value
  # `describe` returns a string that says what sort of Value this
  # looks like (a number, a boolean, or a function).  By default, we
  # don't know anything more specific to say here, so it is just a
  # "value".
  def describe
    "value"
  end

  # Some Values look like numbers, so that their `to_i` method will
  # return an actual number.  By default, non-numeric Values will
  # raise an exception when you try to ask for their number.
  def to_i
    raise TypeError.new("Expected a number value, got a #{self.describe} (#{self.to_s})")
  end

  # Some Values look like booleans, so that their `to_b` method will
  # return true or false.  By default, non-boolean Values will raise
  # an exception when you try to ask for their boolean value.
  def to_b
    raise TypeError.new("Expected a boolean value, got a #{self.describe} (#{self.to_s})")
  end

  # Some Values look like functions, meaning that they can be `call`ed
  # with an argument, which will return some result.  By default,
  # non-function Values will raise an exception when you try to `call`
  # them with an argument.
  def call(arg)
    raise TypeError.new("Cannot call #{self.describe} (#{self.to_s}) with argument (#{arg})")
  end
end


################
## Exercise 1 ##
################

# There are two different classes of literal Values: numeric and
# boolean values.  In each case, a Literal class of value just holds
# some constant (like 3 or true) that you can fetch later if you know
# what type of answer (like numeric or boolean) you expect to get.
class Literal < Value
  # Internally, a Literal object will hold onto its @value, which is
  # passed to its initializer...
  def initialize(v)
    @value = v
  end

  # ... this @value can be accessed from the outside via the `value`
  # method of a Literal object.
  attr_accessor :value

  # Render a Literal object as a string can be done by just converting
  # the value it holds to a string.
  def to_s
    self.value.to_s
  end
end

# Here is the class of Numeric Literal Values (NumValue). Objects of
# this class are described as a "number", and they respond to the
# `to_i` method by returning their internal value (which will be a
# number).
class NumValue < Literal
  # To prevent constructing nonsense Numeric Literal Values, the
  # NumValue initialize method checks that it is given an actual
  # number before storing it.
  def initialize(v)
    unless v.is_a? Integer
      raise TypeError.new("Can't construct numeric value; #{v} is not a number")
    end
    super(v)
  end
  
  def describe
    "number"
  end

  def to_i
    self.value
  end
end
# Notice that `NumValue` objects will raise a TypeError if you ask them
# to be a boolean via `to_b` (following the default implementation
# from the base Value class); that's because they don't hold a boolean
# value, so they have nothing to return.  `NumValue` objects will also
# raise a TypeError if you try to `call` them.

# Write the code for implementing the class of Boolean Literal Values
# (BoolValue), similar to the `NumValue` class above.
class BoolValue < Literal
  def describe
    ### FILL IN HERE ###

    # BoolValue objects are described as a "boolean"
  end

  def to_b
    ### FILL IN HERE ###

    # If you ask a BoolValue object to be a boolean via `to_b`, it
    # should just return the `value` it holds (which will be either
    # true or false).
  end
end
# Notice that `BoolValue` objects will raise a TypeError if you ask
# them to be a number via `to_i` (following the default implementation
# from the base Value class); that's because they don't hold a numeric
# value, so they have nothing to return.  `BoolValue` objects will
# also raise a TypeError if you try to `call` them.

####################
## Extra Credit 1 ##
####################

# Add an initialize method for BoolValue with error-checking code
# making sure the constructor BoolValue.new(v) is passed a value v
# that is either `true` or `false`.  When passed `true` or `false`,
# BoolValue's initialize method should just set the objects instance
# variable @value to be equal to its parameter according it's
# superclass method in Literal (see the similar definition of
# `initialize` in NumValue).  If initialize is provided any other
# argument that is not `true` or `false`, it should raise a TypeError
# exception.


################
## Exercise 2 ##
################

# A Closure Value combines a function (consisting of a parameter that
# needs to be plugged in to run its body of code) along with an
# environment.
class Closure < Value
  # Closures are initialized by providing:
  #
  #   1. the name of their function parameter,
  #
  #   2. the body of the function that will be executed when they are
  #      called, *and*
  #
  #   3. the environment of static variable bindings that should be
  #      used to evaluate the function body whenever it is called.
  def initialize(name, code, env)
    @parameter = name
    @body = code
    @environment = env
  end

  def describe
    ### FILL IN HERE ###

    # Closures are described as a "function"
  end

  def call(arg)
    ### FILL IN HERE ###

    # Program how to `call` a Closure object.  To `call` a Closure,
    # you will need to invoke the `evaluate` function that you will
    # complete in Exercise 5 below. `evaluate` takes an expression to
    # evaluate along with the environment of variable-to-value
    # bindings, and then computes the answer of that expression
    # (possibly using the environment to look up values of variables).

    # To properly use `evaluate` to compute the answer to a Closure
    # `call`, you need to pass it:
    #
    #   1. The body of the function as the expression to evaluate, and
    #
    #   2. The same environment that was used to initialize this
    #      object, *extended* so that the parameter of the function is
    #      associated to the `arg` passed to `call`.

    # HINT: Remember that when you extend an environment, you should
    # *never* modify the hash-table representing it.  Instead, make
    # sure that you create a *new* hash-table that is identical to the
    # old one, and add the new variable-value association to the brand
    # new copy, leaving the old hash-table to be the same on future
    # use.  The `merge` method of hash tables does this, by returning
    # a brand new table instead of modifying the old one in-place.
    # You may wish to review Assignment 8 on implementing the type
    # checker to remember how to use hash tables for environments.
  end

  def to_s
    ### FILL IN HERE ###
    
    # Rendering a closure as a string should do the same thing as
    # printing the corresponding function expression:
    #
    #     {fun: [{var: @parameter}, @body]}
  end
end

####################
## Extra Credit 2 ##
####################

# For extra credit, you may extend the `to_s` method for Closure
# objects above to *also* print the captured environment along with
# the function itself, to show the programmer what variables are in
# scope when this closure was created.

# For extra extra credit , avoid printing any primitive operations
# that were loaded into the environment at the start of evaluation.
# In other words, if the value associated with a variable name is the
# same in the closure's environment as it is in the `$primops` table
# below, then *do not* include it in the output of `to_s`.


################
## Exercise 3 ##
################

# A Primitive Operation (PrimOp) Value represents one of the basic
# instructions of the underlying machine, such as addition,
# multiplication, comparison, etc.  PrimOps may be *partially
# applied*, which means that they have been applied to some values,
# but not enough to actually run the operation yet.  Partially applied
# PrimOps are also Values.
class PrimOp < Value
  # A PrimOp is initialized with:
  #
  #   1. the name of the operation (op),
  #
  #   2. the *arity* of the operation (n), which is the number of
  #      arguments it needs to have before the operation can be run,
  #      and
  #
  #   3. the list of arguments (arg) that the operation has been
  #      partially applied to so far, which needs to be fewer
  #      arguments than the operations arity (otherwise, the operation
  #      would no longer be a partially-applied value, but an action
  #      to perform now)
  #
  # By default, the argument list is empty, which represents the basic
  # Primitive Operation itself before being partially applied to any
  # arguments.
  def initialize(op, n, args = [])
    @operation = op
    @arity = n
    @arguments = args
  end

  # Primitive Operations are also described as a "function", the same
  # as Closures
  def describe
    "function"
  end

  # To call a Primitive Operation, the computer needs to:
  #
  #   1. add the new argument (arg), to the list of arguments it
  #      already has, in case the PrimOp has been previously applied
  #      to some arguments,
  #
  #   2. if the addition of the new argument (arg) is enough to call
  #      the operation with all the necessary arguments, then invoke
  #      `call_primop` to compute the answer to this fully-applied
  #      operation,
  #
  #   3. otherwise, the operation still does not have enough arguments
  #      to run yet; in this case, return a new partially-applied
  #      PrimOp Value which has the new list of arguments, and
  #      everything else is the same.
  def call(arg)
    new_args = @arguments + [arg]
    if new_args.length == @arity
    then
      call_primop(@operation, new_args)
    else
      PrimOp.new(@operation, @arity, new_args)
    end
  end

  # To render a (partially applied) PrimOp as a string, put together
  # the operation with any arguments it may be applied to already, and
  # print them all out together in the application form:
  #
  #     operation arg1 arg2 arg3 ... argn
  def to_s
    application = [@operation] + @arguments
    "#{application.join(' ')}"
  end
end

# `call_primop` is the procedure which *decodes* the name of a
# primitive operation, and *decides* which real instruction should be
# used to compute the result.
def call_primop(name, args)
  case [name, args]
  # For example, calling the primop named `:plus` with *exactly* two
  # arguments [x, y] is implemented by reading the numeric values
  # stored in arguments `x` and `y`, and then adding them with Ruby's
  # + operation to return their sum wrapped up in a new Numeric Value
  # object
  in :plus, [x, y]
    NumValue.new(x.to_i + y.to_i)

  ### FILL IN HERE ###

  # Add additional cases for the numeric operations:

  #   1. :minus, implemented by Ruby's - operation
  #   2. :times, implemented by Ruby's * operation
  #   3. :divide, implemented by Ruby's / operation

  # As another example, calling the primop named `:equal?` with
  # *exactly* two arguments [x, y] is implemented by reading the
  # numeric values stored in `x` and `y`, and then comparing them for
  # equality with the == operation to return either true or false
  # wrapped up in a new Boolean Value object
  in :equal?, [x, y]
    BoolValue.new(x.to_i == y.to_i)

  ### FILL IN HERE ###

  # Add additional cases for the comparison operations:

  #   1. :inequal?, implemented by Ruby's != operation
  #   2. :greater?, implemented by Ruby's > operation
  end
end

#################################
## Exercise 4 + Extra Credit 3 ##
#################################

$primops = {}
# The $primops hash table $primops is used to associate the symbolic
# name of each primitive operation with a new PrimOp object storing
# its name and arity (the exact number of arguments it needs to run).

# For example, the following assignment adds a new PrimOp value to the
# $primops table corresponding to :plus.
$primops[:plus] = PrimOp.new(:plus, 2)

### FILL IN HERE ###

# Fill in the rest of the $primops table for the remaining primitive
# operations from Assignment 8.  Here is a list of all the primitive
# operations and their arity:

#   * :plus is a PrimOp with arity 2
#   * :minus is a PrimOp with arity 2
#   * :times is a PrimOp with arity 2
#   * :divide is a PrimOp with arity 2
#   * :equal? is a PrimOp with arity 2
#   * :inequal? is a PrimOp with arity 2
#   * :greater? is a PrimOp with arity 2


# FOR EXTRA CREDIT: rather than creating a new PrimOp object by hand
# for every individual operation, you can use the type of the
# operation from Assignment 8 to calculate the arity
# automatically. First, fill in the code for the following
# `type_arity` operation
def type_arity(ty)
  ### EXTRA CREDIT ###

  # Count the number of rightward-leaning arrows in a type; that
  # number is the arity of the type.  The two main cases are:
  #
  #   * The arity of `a -> b` is 1 + the arity of b
  #
  #   * Otherwise, the arity of any other non-arrow type is 0
end

# Now, copy your $primtypes table from Assignment 8 Exercise 2 to
# here.  You can automatically fill out the $primops table by looping
# over each name-type association in $primtypes, calculating the arity
# of the type, and then creating a new PrimOp object with that name
# and number associated with the operation's name in the $primops
# table.


################
## Exercise 5 ##
################

# You are now ready to complete the main `evaluate` function which
# calculates the answer (which will be some sort of Value object) to
# any expression (given in parameter `expr`) within a specific
# environment (given in parameter `env`).  By default, if no
# environment is given, `evaluate` uses the $primops table.
def evaluate(expr, env = $primops)
  case expr
  in {num: n}
    NumValue.new(n)

  # Evaluating a plain number just returns a new NumValue object
  # storing that number.  This case corresponds to the big-step
  # evaluation rule:
  
  #     ------------
  #     env ⊢ n ⇓ n

  in {bool: b}
    ### FILL IN HERE ###

  # Evaluating a plain boolean (true or false) just returns a new
  # BoolValue object storing that boolean, similarly to the above
  # case.  This case corresponds to the big-step evaluation rule:

  #     ------------
  #     env ⊢ b ⇓ b

  in {var: x}
    ### FILL IN HERE ###

  # To evaluate a variable named `x`, just look up the value
  # associated with the name `x` inside the environment `env` and
  # return that.  This case corresponds to the big-step evaluation
  # rule:

  #     (x => v) ∈ env
  #     --------------
  #     env ⊢ x ⇓ b

  in {fun: [{var: x}, body]}
    ### FILL IN HERE ###

  # To evaluate a function abstraction (λx.body), create a new Closure
  # object which stores the parameter to the function (x), the body
  # that is used to calculate the return value of the function (body),
  # *and* the current environment where the function occurred (env).
  # Return that closure object.

  # This case corresponds to the big-step evaluation rule:

  #     -------------------------------
  #     env ⊢ λx:t.body ⇓ [env]λx.body

  in {app: [function, argument]}
    ### FILL IN HERE ###

  # To evaluation the application (function argument), you need to:
  #
  #   1. evaluate the `function` in the current environment to compute
  #      its Value, then
  #
  #   2. evaluate the `argument` in the current environment to compute
  #      its Value, then
  #
  #   3. the Value for the `function` you got from step 1 should be
  #      something you can `call`; invoke the `call` method of the
  #      `function`s value from step 1 by passing the `argument`s
  #      value from step 2 as the parameter.

  # Written this way in terms of the `call` method of the function
  # value, this case corresponds to the big-step evaluation rule:

  #     env ⊢ function ⇓ V    env ⊢ argument ⇓ Vₐ    V₁.call(V₂) ⇓ V'
  #     --------------------------------------------------------------
  #     env ⊢ function argument ⇓ V'

  # The above big-step rule summarizes over several more specific
  # cases, which implement application slightly different depending on
  # if the function turns out to be a closure or a primitive
  # operation, and if it is a primitive operation, is the result a
  # partial application or an actual instruction?  These more specific
  # rules do not need to be implemented in your code, because the
  # `call` method of the function's Value will know what to do,
  # automatically deciding for itself which of the following
  # evaluation rules to implement:

  #     env ⊢ function ⇓ [env']λx.body    env ⊢ argument ⇓ Vₐ    env', x=>Vₐ ⊢ body ⇓ V'
  #     --------------------------------------------------------------------------------
  #     env ⊢ function argument ⇓ V'

  #     env ⊢ function ⇓ p#n V₁...Vᵢ     env ⊢ argument ⇓ Vₐ    i+1=n     prim(p#n, [V₁, ..., Vᵢ, Vₐ]) = V'
  #     --------------------------------------------------------------------------------------------------
  #     env ⊢ function argument ⇓ V'

  #     env ⊢ function ⇓ p#n V₁...Vᵢ     env ⊢ argument ⇓ Vₐ    i+1<n
  #     -------------------------------------------------------------
  #     env ⊢ function argument ⇓ p#n V₁...Vᵢ Vₐ

  in {if: [check, if_true, if_false]}
    ### FILL IN HERE ###
    
  # To evaluate the branch (if check then if_true else if_false), you
  # need to:
  #
  #   1. Evaluate the `check` of the if-then-else in the current
  #      environment to get its Value first, then
  #
  #   2. extract the boolean `true` or `false` from the Value of
  #      `check` from the previous step by invoking its `to_b` method,
  #      then depending on that answer:
  #
  #      a. if `to_b` returns `true`, evaluate `if_true` in the
  #         current environment and return its value
  #      a. if `to_b` returns `false`, evaluate `if_false` in the
  #         current environment and return its value

  # This case corresponds to these two big-step evaluation rules:
  
  #     env ⊢ check ⇓ true    env ⊢ if_true ⇓ V
  #     ---------------------------------------------
  #     env ⊢ if check then if_true else if_true ⇓ V

  #     env ⊢ check ⇓ false    env ⊢ if_false ⇓ V
  #     ---------------------------------------------
  #     env ⊢ if check then if_true else if_true ⇓ V
  end

end

####################
## Extra Credit 4 ##
####################

# For extra credit points, you may add a "recursive function
# definition" expression to the language as found in Standard ML,
# similar to Assignment 8 Extra Credit Exercise 3.  This appears as
# another case in the grammar of syntactically well-formed expressions
# looking like:
#
#     M ::= ... | fun f(x) = M in M

# The idea behind the recursive function `fun f(x) = M in M'` is that
# it introduces a local function named f which is visible in BOTH the
# body of the block of code `M'` as well as the body `M` of the
# function `f` being defined.  In other words, calculating the result
# of the function may recursively refer to itself via the name `f`.

# The evaluation rule for a recursive function is:
# 
#     env2 ⊢ M' : V'   env2 = (env, f=>[env2]λx.M)
#     ----------------------------------------------
#     env ⊢ fun f(x) = M in M' ⇓ V

# Notice how the environment `env2` used to evaluate the block of code
# M' is also captured in the Closure bound to `f`, so that `env2`
# recursively points to itself inside of `f`s closure.  This recursive
# cycle in the environment can be done by doing an in-place update
# (the ONLY place in this entire assignment that you should MUTATE an
# environment):

#   1. First, clone the original environment `env`, call it `env2`.
#
#   2. Second, create a new Closure object with the parameter name
#      `x`, the expression body `M`, and the environment `env2`.
#
#   3. Third, *mutate* the environment `env2` by associating the name
#      `f` with the Closure object you just created in step 2.  You
#      should be sure that you change the hash table `env2` *in
#      place*, which will mean that `f` will now be associated with
#      the Closure *inside of the closure's environment*

# To finish implementing this extension to the language, add cases for
# handling a recursive let to the following places:
#
#   1. Represent a recursive function (fun f(x)= M in M') as
#
#          {function: [{name: f}, {param: x}, M, M']}
#
#   2. In `print_expr`, add a case for printing an expression matching
#      the pattern  `{function: [{name: f}, {param: x}, M, M']}`.
#
#   3. In `evaluate`, add a case for recursively binding a function
#      Closure to a name in order to calculate the value of an
#      expression matching the big-step evaluation rule above.  Be
#      sure that the Closure can find itself through the environment
#      it stores internally via the bound name, such as using the
#      above 3-step process that mutates the new environment to update
#      the Closure after its already created.


################
## Test Cases ##
################

# In the following expected answers, primitive operations originally
# included in the initial $primops table will not be shown in the
# environment of Closures

# ex01 = +(5)
# ex01 = 5 + _
ex01 = {app: [{var: :plus}, {num: 5}]}
# Expected answer = plus 5

# ex02 = ex1(4) = +(5)(4)
# ex02 = 5 + 4
ex02 = {app: [ex01, {num: 4}]}
# Expected answer = 9

# ex03 = -(ex02)
# ex03 = ex02 - _ = (5 + 4) - _
ex03 = {app: [{var: :minus}, ex02]}
# Expected answer = minus 9

# ex04 = ex03(3)
# ex04 = (5 + 4) - 3
ex04 = {app: [ex03, {num: 3}]}
# Expected answer = 6

# ex05 = λx:boolean. λy:boolean. if x then true else y
ex05 = {fun: [{var: :x, type: :boolean},
              {fun: [{var: :y, type: :boolean},
                     {if: [{var: :x},
                           {bool: true},
                           {var: :y}]}]}]}
# Expected answer = [](λx. (λy:boolean. if x then true else y))

# ex06 = ex05(false)
ex06 = {app: [ex05, {bool: false}]}
# Expected answer = [x=false](λy. if x then true else y)

# ex07 = ex06(false) = ex05(false)(false)
ex07 = {app: [ex06, {bool: false}]}
# Expected answer = false

# ex08 = λf.λx. f(f(x))
ex08 = {fun: [{var: :f, type: {arrow: [:number, :number]}},
              {fun: [{var: :x, type: :number},
                     {app: [{var: :f},
                            {app: [{var: :f},
                                   {var: :x}]}]}]}]}
# Expected answer = [](λf. (λx:number. (f (f x))))

# ex09 = ex08(ex01)
ex09 = {app: [ex08, ex01]}
# Expected answer = [f=plus 5](λx. (f (f x)))

# ex10 = ex09(9) = ex08(ex01)(9)
ex10 = {app: [ex09, {num: 9}]}
# Expected answer = 19

# The local binding
#
#     let x:T = m1 in m2
#
# can be written using function application and abstraction as
#
#     (λx:T. m2) m1
def let(name, ty, m1, m2)
  {app: [{fun: [{var: name, type: ty}, m2]},
         m1]}
end

# ex11 = let x:number = 11 in x + x
ex11 = let(:x, :number, {num: 11},
           {app: [{app: [{var: :plus}, {var: :x}]},
                  {var: :x}]})
# Expected answer = 22

# ex12 = let x:number = 12 in
#        let f:boolean->boolean = (λx:boolean. x) in
#        x + x
ex12 = let(:x, :number, {num: 12},
           let(:f, {arrow: [:boolean, :boolean]},
               {fun: [{var: :x, type: :boolean}, {var: :x}]},
               {app: [{app: [{var: :plus}, {var: :x}]},
                      {var: :x}]}))
# Expected answer = 24

# ex13 = let x:number = 13 in
#        (λy:number. λx:boolean. y) x false
ex13 = let(:x, :number, {num: 13},
           {app: [{app: [{fun: [{var: :y, type: :number},
                                {fun: [{var: :x, type: :boolean},
                                       {var: :y}]}]},
                         {var: :x}]},
                  {bool: false}]})
# Expected answer = 13

# ex14 = let x : number         = 14 in
#        let f : number->number = (λy. x+y) in
#        let x : boolean        = false in
#        f(1)
ex14 = let(:x, :number,
           {num: 14},
           let(:f, {arrow: [:number, :number]},
               {fun: [{var: :y, type: :number},
                      {app: [{app: [{var: :plus}, {var: :x}]},
                             {var: :y}]}]},
               let(:x, :boolean,
                   {bool: false},
                   {app: [{var: :f}, {num: 1}]})))
# Expected answer = 15

# ex15 = let f : (number -> number) -> number -> number = (λh.λx. h(x)) in
#        let x : boolean = false in
#        let g : number -> number = f(λx. x-1) in
#        g(15)
ex15 = let(:f, {arrow: [{arrow: [:number, :number]}, {arrow: [:number, :number]}]},
           {fun: [{var: :h, type: {arrow: [:number, :number]}},
                  {fun: [{var: :x, type: :number},
                         {app: [{var: :h}, {var: :x}]}]}]},
           let(:x, :boolean, {bool: false},
               let(:g, {arrow: [:number, :number]},
                   {app: [{var: :f},
                          {fun: [{var: :x, type: :number},
                                 {app: [{app: [{var: :minus}, {var: :x}]},
                                        {num: 1}]}]}]},
                   {app: [{var: :g}, {num: 15}]})))
# Expected answer = 14

# ex16 = (λx:boolean.x) 16
ex16 = {app: [{fun: [{var: :x, type: :boolean}, {var: :x}]},
              {num: 16}]}
# Expected answer = 16

# ex17 = false(17)
ex17 = {app: [{bool: false}, {num: 17}]}
# Expected answer = Type Error: Cannot call boolean (false) with argument (17)

# ex18 = let x : number = 1 in
#        let y : boolean = true in
#        if false then x else y
ex18 = let(:x, :number, {num: 1},
           let(:y, :boolean, {bool: true},
               {if: [{bool: false}, {var: :x}, {var: :y}]}))
# Expected answer = true

# ex19 = true / false
ex19 = {app: [{app: [{var: :divide}, {bool: true}]}, {bool: false}]}
# Expected answer = Type Error: Expected a number value, got a boolean (true)

# ex20 = if (λx:number. x) then 1 else 0
ex20 = {if: [{fun: [{var: :x, type: :number}, {var: :x}]},
             {num: 1},
             {num: 0}]}
# Expected answer = Type Error: Expected a boolean value, got a function ([](λx. x))

examples = [ ex01, ex02, ex03, ex04, ex05,
             ex06, ex07, ex08, ex09, ex10,
             ex11, ex12, ex13, ex14, ex15,
             ex16, ex17, ex18, ex19, ex20 ]

require 'pp'

examples.map.with_index do |ex, i|
  puts "", "Example #{i+1}:"
  pp ex
  puts "========================================"
  begin
    puts "#{print_expr(ex)}", "=", evaluate(ex).to_s
  rescue TypeError => err
    puts err
  end
end
