##############################################
### Assignment 6 --- Syntactic Interpreter ###
###                                        ###
### Organization of Programming Languages  ###
###               Fall 2025                ###
###              Paul Downen               ###
##############################################

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
#     ruby assignment6.rb
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
#     load "assignment6.rb"



# You already saw a compositional interpreter for this language of
# conditional arithmetic expressions:

#     n ::= 0 | 1 | 2 | 3 | ...
#     A ::= n | plus(A, A) | minus(A, A) | times(A, A) | divide(A, A)
#         | if B then A else A
#
#     b ::= true | false
#     B ::= b | or(B, B) | and(B, B) | geq?(A, A) | zero?(A)

# The interpreter was written in an object-oriented style using
# classes, inheritance, and subtyping to implement an `eval` method
# for evaluating expressions to compute their (numeric or boolean)
# answer along with a `to_s` method which renders an expression as a
# (overly-parenthesized) string.

# In assignment 4, you added classes implementing multiplication,
# division, logical and, zero testing, and an if-then-else expression.

# In this assignment, you will be writing variations on an interpreter
# and expression-processing functions for this same language using:
#
#   1. syntactic representations of expressions as concrete data
#      structures, (for example, symbolic hash tables and arrays),
#
#   2. pattern-matching on the structure of data, and
#
#   3. functional programming style, involving maps and recursion over
#      data structures


################
## Exercise 1 ##
################

# Arithmetic and conditional syntax trees can be represented
# concretely in Ruby as nested symbolic hash tables. The root of the
# syntax tree corresponds to a hash table with a single key-value
# pair: the key is a symbol which describes which type of syntax node
# it is, and the value is a (list of) subtree(s) within it.

# You can map the grammatical rules of A and B to symbolic hash tables
# like so:
#
#     n                    ==>  {num: n}    (for some number n)
#     plus(left, right)    ==>  {plus: [left, right]}
#     minus(left, right)   ==>  {minus: [left, right]}
#     times(left, right)   ==>  {times: [left, right]}
#     divide(left, right)  ==>  {divide: [left, right]}
#     if(chk, thn, els)    ==>  {if: [chk, thn, els]}
#
#     b                    ==>  {bool: b}    (for b = true or b = false)
#     or(left, right)      ==>  {or: [left, right]}
#     and(left, right)     ==>  {and: [left, right]}
#     geq?(left,right)     ==>  {geq?: [left, right]}
#     zero?(subexpr)       ==>  {zero?: subexpr}

# Here are some examples of arithmetic and conditional expressions
# represented as nested hash-tables using the rules above:

# ex1 = (5 + 4) * 3
# ex1 = times(plus(5,4),3)
ex1   = {times: [{plus: [{num: 5}, {num: 4}]}, {num: 3}]}

# ex2 = (4 - 1) / 2
# ex2 = divide(minus(4,1),2)
ex2   = {divide: [{minus: [{num: 4}, {num: 1}]}, {num: 2}]}

# ex3 = 5 + 1 - 2 * 2
# ex3 = minus(plus(5,1),times(2,2))
ex3   = {minus: [{plus: [{num: 5}, {num: 1}]}, {times: [{num: 2}, {num: 2}]}]}

# ex4 = if zero?(1 + 1 - 2) then 4 / 2 else 4 / (1 + 1 - 2)
# ex4 = if(zero?(minus(plus(1,1),2)), divide(4,2), divide(4,minus(plus(1,1),2)))
ex4   = {if: [{zero?: {minus: [{plus: [{num: 1}, {num: 1}]},
                               {num: 2}]}},
              {divide: [{num: 4}, {num: 2}]},
              {divide: [{num: 4},
                        {minus: [{plus: [{num: 1}, {num: 1}]},
                                 {num: 2}]}]}]}

# ex5 = if zero?((3 + 4) * (1 - 1)) then 5 - 2 else 6 / 2
# ex5 = if(zero?(times(plus(3,4), minus(1,1))), minus(5,2), divide(6,2))
ex5   = {if: [{zero?: {times: [{plus: [{num: 3}, {num: 4}]},
                               {minus: [{num: 1}, {num: 1}]}]}},
              {minus: [{num: 5}, {num: 2}]},
              {divide: [{num: 6}, {num: 2}]}]}

# ex6 = zero?(2 + 3 - 5)
# ex6 = zero?(minus(plus(2,3),5))
ex6   = {zero?: {minus: [{plus: [{num: 2}, {num: 3}]},
                         {num: 5}]}}

# ex7 = false and zero?(1 / 0)
# ex7 = and(false,zero?(divide(1,0)))
ex7   = {and: [{bool: false},
               {zero?: {divide: [{num: 1}, {num: 0}]}}]}

# ex8 = 4 >= 3 and 5 >= 4 or zero?(4)
# ex8 = or(and(geq?(4, 3), geq?(5, 4)), zero?(4))
ex8   = {or: [{and: [{geq?: [{num: 4}, {num: 3}]},
                     {geq?: [{num: 5}, {num: 4}]}]},
              {zero?: {num: 4}}]}


# Write down the following A syntax tree as a symbolic hash-table
# using the rules above:

# exercise1 = 5 - 8 / 2
# exercise1 = minus(5, divide(8, 2))
exercise1 = nil  ### FILL IN HERE: replace nil with your syntax tree ###


################
## Exercise 2 ##
################

# Recall from assignment 4 that we defined how to render
# representations of syntax trees as string via a `to_s` method that
# produced an over abundance of parentheses.

# Complete this pair of "ugly printing" functions which converts a
# syntax tree represented as a symbolic hash table into a string
# rendered with the correct binary operation corresponding to each
# node in the syntax tree.
#
#   * ugly_print_arith converts an arithmetic syntax tree (represented
#     by a symbolic hash table following the rules in Exercise 1) to a
#     string
#
#   * ugly_print_cond converts a conditional syntax tree (represented
#     by a symbolic hash table following the rules in Exercise 1) to a
#     string
#
# You may use the completed branches of the `case` statement as
# examples to help fill in your answer.

# Hint: When trying to print a sub-expression, be careful that you use
# the correct function (ugly_print_arith or ugly_print_cond) that
# matches the type of sub-expression you are expecting for each case
# of syntax!

def ugly_print_arith(expr)
  case expr
  in {num: n}
    "#{n}"

  in {plus: [left, right]}
    "(#{ugly_print_arith(left)}) + (#{ugly_print_arith(right)})"

  in {minus: [left, right]}
    "(#{ugly_print_arith(left)}) - (#{ugly_print_arith(right)})"

  in {times: [left, right]}
    ## FILL IN HERE ###
    # Return a string that has the form "(left) * (right)", where the
    # words "left" and "right" are replaced by the strings generated
    # by ugly-printing the matching left and right sub-expressions.

  in {divide: [left, right]}
    ## FILL IN HERE ###
    # Return a string that has the form "(left) / (right)", where the
    # words "left" and "right" are replaced by the strings generated
    # by ugly-printing the matching left and right sub-expressions.

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

  in {and: [left, right]}
    ### FILL IN HERE ###
    # Return a string that has the form "(left) and (right)", where
    # the words "left" and "right" are replaced by strings generated
    # by ugly-printing the matching left and right sub-expressions.

  in {zero?: subexpr}
    ### FILL IN HERE ###
    # Return a string that has the form "zero?(subexpr)" where the
    # word "subexpr" is replaced by the string generated by
    # ugly-printing the (only) matching sub-expression.

  in {geq?: [left, right]}
    ### FILL IN HERE ###
    # Return a string that has the form "(left) >= (right)", where
    # the words "left" and "right" are replaced by strings generated
    # by ugly-printing the matching left and right sub-expressions.
  end
end


################
## Exercise 3 ##
################

# In assignment 4, we also defined an `eval` method that was able to
# evaluate each of the different types of syntax trees to get a final
# answer.

# Complete this pair of evaluation functions which converts a syntax
# tree represented as a symbolic hash table into its final answer.
#
#   * eval_arith converts an arithmetic syntax tree to a number
#
#   * eval_cond converts a conditional syntax tree to a boolean value
#     true or false
#
# You may use the completed branches of the `case` statement as
# examples to help fill in your answer.

def eval_arith(expr)
  case expr
  in {num: n}
    # To evaluate a numeric expression, just return that number.
    n

  in {plus: [left, right]}
    # To evaluate a `:plus` operation, evaluate the two
    # sub-expressions and then add the numbers you get back.
    eval_arith(left) + eval_arith(right)

  in {minus: [left, right]}
    # To evaluate a `:minus` operation, evaluate the two
    # sub-expressions, and then check the side condition to make sure
    # you don't get a negative number before returning their
    # difference.
    n1 = eval_arith(left)
    n2 = eval_arith(right)
    return n1 - n2 if n1 >= n2

  in {times: [left, right]}
    ### FILL IN HERE ###
    # Return result of evaluating the matching left and right
    # sub-expressions, and then multiplying those two numbers together
    # with the * operator.

  in {divide: [left, right]}
    ### FILL IN HERE ###
    # Return result of evaluating the matching left and right
    # sub-expressions, and then dividing those two numbers with the /
    # operator.

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
  in {bool: b}
    # To evaluate a literal boolean expression, just return its value.
    b

  in {or: [left, right]}
    # To evaluate an `:or` operation, first check the result of
    # evaluating the left sub-expression: if it's true then return
    # true right away, but if it's false you need to evaluate the
    # right sub-expression to see if the combination is true or false.
    if eval_cond(left)
    then true
    else eval_cond(right)
    end

  in {and: [left, right]}
    ### FILL IN HERE ###
    # Evaluate the two sub-expressions in such a way that you return
    # false if one of them is false and true only if they are both
    # true .  Make sure to implement left-to-right short-circuiting:
    #
    #   * if the matching left sub-expression evaluates to false, then
    #     return false immediately without evaluating the right
    #     sub-expression;
    #
    #   * but if the matching left sub-expression instead evaluates to
    #     true, you must evaluate the right sub-expression to figure
    #     out the answer

  in {geq?: [left, right]}
    # To evaluate a `:geq?` operation, evaluate the matching left and
    # right sub-expressions (which are arithmetic expressions) and
    # compare the numbers you get back to check if the first is
    # greater than or equal to the second.
    eval_arith(left) >= eval_arith(right)

  in {zero?: subexpr}
    ### FILL IN HERE ###
    # evaluate the only subexpression (which should return a number),
    # and return true if that number is equal (==) to zero, or false
    # otherwise.
  end
end


################
## Exercise 4 ##
################

# Because we are now representing syntax trees concretely as (nested)
# hash-tables, it is easy to define new functions over them by just
# matching on the patterns in the given hash-table.

# Complete the following two functions:
#
#   * `flatten_product` takes any arithmetic syntax tree and returns a
#     list representing the outer-most "product", containing a series
#     multiplicative operation `:times` or `:divide` in between
#     "factors" (arithmetic sub-expressions which are not a `:times`
#     or `:divide` operation)
#
#   * `flatten_logic` takes any conditional syntax tree and returns a
#     list representing the outer-most boolean "logic", containing a
#     series of boolean operations `:and` or `:or` in between "terms"
#     (conditional sub-expressions which are not an `:and` or `:or`
#     operation)
#
# You may use the completed definition of the similar `flatten_sum`
# function below as an example to help you fill in your answer.

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

def flatten_product(expr)
  case expr
  in {times: [left, right]}
    ### FILL IN HERE ###
    # Return a list starting with the factors generated by flattening
    # the outer-most product in the left sub-expression, then followed
    # by a `:times` symbol, and finally ending with the factors
    # generated by flattening the outer-most product in the right
    # sub-expression.

  in {divide: [left, right]}
    ### FILL IN HERE ###
    # Return a list starting with the factors generated by flattening
    # the outer-most product in the left sub-expression, then followed
    # by a `:divide` symbol, and finally ending with the factors
    # generated by flattening the outer-most product in the right
    # sub-expression.

  else
    ### FILL IN HERE ###
    # In all other cases, just return a one-element list containing
    # the original `expr` as-is.
  end
end

def flatten_logic(expr)
  ### FILL IN HERE ###
  # Write a case-analysis on the conditional syntax tree `expr` that
  # handles the following possible cases:
  #
  #   * If `expr` matches the pattern {and: [left, right]}, then
  #     return a list starting with the terms generated by flattening
  #     the outer-most logic in the left sub-expression, then followed
  #     by a `:and` symbol, and finally ending with the terms
  #     generated by flattening the outer-most logic in the right
  #     sub-expression.
  #
  #   * If `expr` matches the pattern {or: [left, right]}, then
  #     return a list starting with the terms generated by flattening
  #     the outer-most logic in the left sub-expression, then followed
  #     by a `:or` symbol, and finally ending with the terms
  #     generated by flattening the outer-most logic in the right
  #     sub-expression.
  #
  #   * In all other cases, just return a one-element list containing
  #     the original `expr` as-is.
end


################
## Exercise 5 ##
################

# By flattening expressions first, we can render the individual parts
# in a chain of similar operations in a more "pretty" way, that avoids
# adding unneeded parenthesis by considering the order of operations.
# This exercise is setting up for the full "pretty-print" functions
# that you will complete next in Exercise 6.

# Complete the following two functions:
#
#   * `pretty_print_term` takes any single "term" that may appear in
#     the list generated by `flatten_logic` and renders it as the
#     appropriate string.
#
#   * `pretty_print_factor` takes a single "factor" that may appear in
#     the list generated by `flatten_product` and renders it as the
#     appropriate string.
#
# In both cases, we will take the order of operations in account, and
# avoid surrounding sub-expressions in parenthesis when they have a
# higher-precedence than the chain of operations they appear in.  For
# example, a sub-expression "2*3" (represented by the hash-table
# {times: [{num: 2}, {num: 3}]}) would not be surrounded with
# parenthesis if it appears inside of a chain of `:plus` and `:minus`
# operations.

# You may use the completed definition of the similar
# `pretty_print_summand` function below as an example to help you fill
# in your answer.

def pretty_print_summand(summ)
  case summ
  in :plus
    # The `:plus` operator is printed as "+"
    "+"

  in :minus
    # The `:minus` operator is printed as "-"
    "-"

  in {num: _} | {times: _} | {divide: _}
    # A sub-expression which is a number (matching {num:...}) a
    # multiplication (matching {times:...}) or a division (matching
    # {divide:...}) does not need to be surrounded by parentheses when
    # inside of a chain of additions and subtractions...
    pretty_print_arith(summ)

  else
    # ... but any other arithmetic sub-expression *does* need to be
    # wrapped in parenthesis if it appears inside a chain of additions
    # and subtractions.
    "(#{pretty_print_arith(summ)})"
  end
end

def pretty_print_factor(fact)
  ### FILL IN HERE ###
  # Write a case-analysis on the multiplicative factor `fact` that
  # handles the following possible cases:
  #
  #   * If `fact` is the `:times` symbol, then return the string "*"
  #     standing for the multiplication operator.
  #
  #   * If `fact` is the `:divide` symbol, then return the string "/"
  #     standing for the division operator.
  #
  #   * If `fact` is just a numeric expression, matching the pattern
  #     {num: n}, then convert the integer `n` to a string and return
  #     it as-is, with no surrounding parenthesis.
  #
  #   * In all other cases where `fact` is some other arithmetic
  #     sub-expression, it needs to be surrounded in parentheses.
  #
  #     In order to complete this case, you may assume that the
  #     function `pretty_print_arith` already exists, and know how to
  #     convert the arithmetic sub-expression `fact` into a
  #     prettily-rendered string; return that string surrounded in
  #     parenthesis "(...)".
end

def pretty_print_term(term)
  case term
  in :and
    ### FILL IN HERE ###
    # The operation `:and` should be rendered as the string "and"

  in :or
    ### FILL IN HERE ###
    # The operation `:or` should be rendered as the string "or"

  in {bool: _} | {geq?: _} | {zero?: _}
    ### FILL IN HERE ###
    # A sub-expression which is either a boolean literal (matching
    # {bool:...}), a greater-than-or-equal-to operation (matching
    # {geq?:...}), or an equal-to-zero operation (matching
    # {zero?:...}) does not need parenthesis.
    #
    # In order to complete this case, you may assume that the function
    # `pretty_print_cond` already exists, and knows how to convert the
    # conditional expression `term` into a prettily-rendered string;
    # return that string as-is.

  else
    ### FILL IN HERE ###
    # All other cases of logical terms which are conditional
    # sub-expressions need to be surrounded by parenthesis.
    #
    # As in the previous case, you may assume that the function
    # `pretty_print_cond` already exists and knows how to convert
    # `term` into a prettily-rendered string; return that string
    # wrapped in parenthesis "(...)".
  end
end


################
## Exercise 6 ##
################

# Now it's time to finally complete pretty-printing! In this exercise,
# you will finish the two functions you used previously in Exercise 5:
#
#   * `pretty_print_arith` takes any arithmetic syntax tree as an
#     argument and returns a string showing the expression it
#     represents, avoiding any unnecessary parenthesis that are clear
#     from the precedence of operations.
#
#   * `pretty_print_cond` does the analogous pretty-printing for
#   * conditional expressions, taking any conditional syntax tree as
#   * an argument and returning a nicely-printed string it represents
#   * without redundant parenthesis.

# While filling in your answers, you may use the already-completed
# cases as examples to help finish the following code.

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

  in {times: _} | {divide: _}
    ### FILL IN HERE###
    # To pretty-print a multiplication or division expression (matching
    # {times:...} or {divide:...}) do the following:
    #
    #   1. Flatten the expression into a list containing each piece of
    #      the full chain of `:times` and `:divide` operations using
    #      `flatten_product` from Exercise 4.
    #
    #   2. Transform each "factor" (the elements in the list from step
    #      1) by printing each part individually with
    #      `pretty_print_factor` from Exercise 5.
    #
    #   3. Finally, join all the individual strings in the list
    #      together with a space " " in between them.
    #
    # With help on how to translate these steps to Ruby code, look at
    # the previous case for {plus: _} and {minus: _} expressions to
    # see how it can be implemented by chaining together the `map` and
    # `join` methods of lists.

  in {if: [chk, thn, els]}
    "if #{pretty_print_cond(chk)} then #{pretty_print_arith(thn)} else #{pretty_print_arith(els)}"
  end
end

def pretty_print_cond(expr)
  case expr
  in {bool: b}
    "#{b}"

  in {and: _} | {or: _}
    ### FILL IN HERE ###
    # To pretty-print a logical "and" or "or" expression (matching
    # {and:...} or {or:...}) do the following:
    #
    #   1. Flatten the expression into a list containing each piece of
    #      the full chain of `:and` and `:or` operations using
    #      `flatten_logic` from Exercise 4.
    #
    #   2. Transform each "term" (the elements in the list from step
    #      1) by printing each part individually with
    #      `pretty_print_term` from Exercise 5.
    #
    #   3. Finally, join all the individual strings in the list
    #      together with a space " " in between them.
    #
    # With help on how to translate these steps to Ruby code, look at
    # the previous case for {plus: _} and {minus: _} expressions to
    # see how it can be implemented by chaining together the `map` and
    # `join` methods of lists.

  in {geq?: [left, right]}
    "#{pretty_print_arith(left)} >= #{pretty_print_arith(right)}"

  in {zero?: subexpr}
    ### FILL IN HERE ###
    # Return a string that has the form "zero?(subexpr)", where the
    # word "subexpr" is replaced by the pretty-printed string generated
    # for the arithmetic syntax tree `subexpr`
  end
end


################
## Test Cases ##
################

arith_examples = [ex1, ex2, ex3, ex4, ex5]
cond_examples  = [ex6, ex7, ex8]

puts ""
puts "Exercises 1 & 2: Syntactic Expressions as Symbolic Hash Tables"
puts "=============================================================="

for ex in arith_examples
  puts "#{ugly_print_arith(ex)}", "=", "ugly_print_arith(#{ex})", ""
end

for ex in cond_examples
  puts "#{ugly_print_cond(ex)}", "=", "ugly_print_arith(#{ex})", ""
end

puts ""
puts "Exercise 3: Evaluation"
puts "======================"

arith_examples.each { |ex| puts "#{eval_arith(ex)} = #{ugly_print_arith(ex)}" }
cond_examples.each { |ex| puts "#{eval_cond(ex)} = #{ugly_print_cond(ex)}" }

puts ""
puts "Exercise 4: Flattening"
puts "======================"

for ex in arith_examples
  puts "#{flatten_sum(ex)}", "=", "flatten_sum(#{ex})", ""
  puts "#{flatten_product(ex)}", "=", "flatten_product(#{ex})", ""
end

for ex in cond_examples
  puts "#{flatten_logic(ex)}", "=", "flatten_logic(#{ex})", ""
end

puts ""
puts "Exercises 5 & 6: Pretty Printing"
puts "================================"


for ex in arith_examples
  puts "#{pretty_print_arith(ex)}", "=", "#{ugly_print_arith(ex)}", ""
end

for ex in cond_examples
  puts "#{pretty_print_cond(ex)}", "=", "#{ugly_print_cond(ex)}", ""
end
