# coding: utf-8
#############################################
###     Assignment 8 --- Type Checking    ###
###                                       ###
### Organization of Programming Languages ###
###              Spring 2022              ###
###              Paul Downen              ###
#############################################

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
#     ruby assignment8.rb
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
#     load "assignment8.rb"


##############
## Overview ##
##############

# For this assignment, you will be writing a type checker for the
# simply-typed lambda calculus with numbers and booleans.  The
# grammar of this language's expression syntax is:

#     n ::= 0 | 1 | 2 | 3 | ...
#     b ::= true | false
#     x ::= any symbolic name
#     M ::= n | b | x | M M | λx:T.M | if M then M else M

# The grammar of this language's type syntax is:

#     T ::= number | boolean | T -> T

# Notice that the syntax of expressions (M) does not include primitive
# operations like `plus`, `minus`, `or`, and `geq?`.  Instead, these
# primitives can be stored in variables, which are initialized at the
# start of type checking or interpreting the expression.

# Also notice that in the definition of functions (λx:T.M), the
# parameter x comes annotated with the type of value it is expected to
# hold.  This extra annotation will make it *vastly* easier for you to
# check the type of the function without having to guess what will go
# into the parameter.

# Just like in assignment 6, you will be representing syntax trees
# concretely as nested symbolic hash tables.  As a reminder, the root
# of the syntax tree corresponds to a hash table with a single
# key-value pair: the key is a symbol which describes which case of
# syntax node it is, and the value is a (list of) subtree(s) within
# it.

# The grammatical rules of types (T) correspond to:

#      number  ==>  :number
#     boolean  ==>  :boolean
#     T -> T'  ==>  {arrow: [T, T']}

# The grammatical rules of expressions (M) correspond to these
# symbolic hash tables:

#                         n  ==>  {num: n}
#                         b  ==>  {bool: b}
#                         x  ==>  {var: :x}
#                     M₁ M₂  ==>  {app: [M₁, M₂]}
#                    λx:T.M  ==>  {fun: [{var: :x, type: T}, M]}
#     if M₁ then M₂ else M₃  ==>  {if: [M₁, M₂, M₃]}


################
## Exercise 1 ##
################

# To show the result of type checking, we need to be able to print out
# types.  Here is a function that prints types in an ugly way, by
# cautiously surrounding every single arrow in parenthesis.
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

# To make looking at types more palatable, we can remove excess
# parenthesis around chains of arrows, by saying that arrows associate
# to the right.  For example, the following two types are the same:
#
#     number -> (number -> number)  =  number -> number -> number
#
# However, take care that these two types are DIFFERENT:
#
#     (number -> number) -> number  !=  number -> number -> number

# In this exercise, you will write a function to print types in a
# prettier way by avoiding parenthesis when possible in
# (right-leaning) chains of arrows.


# First, fill in the code in the `flatten_arrows` function which
# converts a type into a list of types found in the outermost chain of
# right-leaning arrows.  For example, flattening the arrows in type
#
#     a -> ((b -> c) -> (d -> e))
#
# should return the list
#
#     [a, (b -> c), d, e]
#
# Notice how the arrow (b -> c) to the left of another arrow DOES NOT
# get flattened into the list.
def flatten_arrows(ty)
  case ty
  in {arrow: [a, b]}
    ### FILL IN HERE ###

    # The interesting case is when the type `ty` is an arrow.  To
    # flatten an arrow type `a -> b` represented as `{arrow: [a, b]}`,
    # return a list consisting of:
    #
    #   1. First, the type `a` to the left of the arrow AS IS. In
    #      other words, the parameter type `a` SHOULD NOT be flattened.
    #
    #   2. Following the parameter type `a`, the rest of the list
    #      should contained the result of flattening the arrows in the
    #      return type `b`.  In other words, the type `b` to the right
    #      of the arrow SHOULD be flattened into the same level as
    #      this arrow.
  else
    ### FILL IN HERE ###

    # In the default case, every other type (such as atomic types
    # `number` and `boolean`) are not arrows and should not be
    # flattened. Remember that `flatten_arrows` must always return a
    # LIST of types, so return a list containing just one element: the
    # original type you were given, as-is.
  end
end

# Now you are ready to fill in the code for this `pretty_print_type`
# function, which writes only the parenthesis around arrows which are
# absolutely necessary to correctly read the type.
def pretty_print_type(ty)
  case ty
  in :number
    ### FILL IN HERE ###

    # Return the string representing the :number type.
    
  in :boolean
    ### FILL IN HERE ###

    # Return the string representing the :boolean type.

  in {arrow: _}
    ### FILL IN HERE ###

    # Pretty-print an arrow type following this procedure:
    #
    #   1. Flatten the biggest right-leaning chain of arrows in the
    #      type using the `flatten_arrows` function to get the list of
    #      types found in between those arrows.
    #
    #   2. For each individual type in the list from step 1, correctly
    #      pretty-print that part as follows:
    #
    #      a) if the individual type found in the list is another
    #         arrow, make sure to pretty-print it SURROUNDED in
    #         parentheses.
    #
    #      b) if the individual type found in the list is anything
    #         else (not an arrow), then just pretty-print it WITHOUT
    #         parentheses.
    #
    #   3. Take each of the strings produced in step 2 and join them
    #      together into a single string with an ASCII arrow "->" in
    #      between each element.

    # Hint: Review the pretty-printing code from assignment 6 for help
    # with how to loop over the flattened type.
  end
end


####################
## Extra Credit 1 ##
####################


# Printing expressions in a more readable format is also useful for
# debugging.  Here is a basic recursive function which prints out an
# expression (represented as nested hash tables) as a string.
def print_expr(expr)
  case expr
  in {num: n}
    "#{n}"
  in {bool: b}
    "#{b}"
  in {var: x}
    "#{x}"
  in {fun: [{var: x, type: t}, body]}
    "(λ#{x}:#{pretty_print_type(t)}. #{print_expr(body)})"
  in {app: [function, argument]}
    "(#{print_expr(function)} #{print_expr(argument)})"
  in {if: [check, if_true, if_false]}
    "if #{print_expr(check)} then #{print_expr(if_true)} else #{print_expr(if_false)}"
  end
end

# The above `print_expr` function cautiously surrounds EVERY function
# abstraction (`fun`) and application (`app`) in parentheses, just in
# case leaving them out would be ambiguous.

# For extra credit, define a prettier version of `print_expr` named
# `pretty_print_expr` which avoids writing parentheses that aren't
# needed to read the expression correctly.  In particular, use the
# precedence of the lambda calculus (previously discussed in class and
# in the homework) to drop unnecessary parentheses.  For example, here
# are some expressions with unnecessary parentheses that should not be
# written by `pretty_print_expr` as follows:
#
#     print_expr    versus    pretty_print_expr
#     -----------------------------------------
#     (λx:a. (λy:b. (λz:c. x)))  =  λx:a.λy:b.λz:c. x
#                 (((f x) y) z)  =  f x y z
#             (λx:a. ((f y) z))  =  λx:a. f y z
#                 (f (λx:a. y))  =  f λx:a. y
#
# After you have implemented `pretty_print_expr`, use it to replace
# the calls to the more ugly `print_expr` in the following code.


################
## Exercise 2 ##
################

# As alluded to above, you do not have to handle each primitive
# operation separately.  Instead, primitive operations are made
# available inside regular variables that are loaded into the initial
# environment providing the default context to analyze expressions.

# For example, all binary arithmetic operations like `plus` will have
# the type `number -> number -> number`, as represented by the
# `bin_arith_type` definition:

# bin_arith_type = number -> (number -> number)
bin_arith_type = {arrow: [:number, {arrow: [:number, :number]}]}

# Similarly, all binary comparison operations like `greater?` will
# have the type `number -> number -> boolean`, as represented by the
# `bin_compare_type` definition below:

# bin_compare_type = number -> (number -> boolean)
bin_compare_type = {arrow: [:number, {arrow: [:number, :boolean]}]}


# Now, fill in the rest of the operations below to populate the table
# describing all of the primitive operations and their types, used for
# type checking programs.
$primtypes = {plus:     bin_arith_type,
              ### FILL IN HERE ###
              # Add additional primitive operations (and their types)
              # to this table.  Make sure to include the following:
              #
              #   * Other binary arithmetic operations: `minus`,
              #     `times`, and `divide`. These should all have the
              #     same type as the `plus` operator above.
              #
              #   * Other comparison operations: `equal?` and
              #     `inequal?`.  These should all have the same type
              #     as the `greater?` operator below.
              greater?: bin_compare_type}


################
## Exercise 3 ##
################

# You are now ready to complete the type inference algorithm below,
# which takes two arguments:
#
#   * an expression `expr` (represented as a nested symbolic hash
#     table) and
#
#   * a typing environment `env` (represented as a hash table with the
#     names of variables as keys associated to symbolic
#     representations of types)
#
# By default, if no typing environment is provided, then the initial
# environment `$primtypes` containing all the primitive operations and
# their types will be used.
#
# Fill in the code below, where function call `infer_type(expr, env)
# should either
#
#   * Return a representation of `expr`'s type, assuming that each
#     free variable in `expr` has the associated type defined by `env`, or
#
#   * Raise a TypeError exception if `expr` does not have any type
#     according to the typing rules of the lambda calculus listed below.

def infer_type(expr, env = $primtypes)
  case expr
  in {num: n}
    :number

    # Without any pre-conditions, a numeric `n` always has the type
    # `number`.  This case implements the typing rule:
    #
    #     ----------
    #     env ⊢ n : number

  in {bool: b}
    ### FILL IN HERE ###

    # Return the type of an expression fitting the pattern `{bool: b}`
    # following the typing rule:
    #
    #     -----------
    #     env ⊢ b : boolean

  in {var: x}
    ### FILL IN HERE ###

    # Return the type of a variable `x` with the given `name` by
    # consulting the given typing environment `env` according to the
    # typing rule:
    #
    #     (x : t) ∈ env
    #     -------------
    #     env ⊢ x : t

    # Hint: Remember you can read the value associated with key `k` in
    # the hash table `h` via `h[k]`.  If `k` has no associated value
    # in `h`, then `h[k]` will return `nil`.  It can happen that `x`
    # does not type check in the given environment `env` in the case
    # when there is NO (x : t) ∈ env.  In code, this corresponds to
    # the case when `env` has no type associated with the key named
    # `x`.

  in {fun: [{var: x, type: t}, body]}
    ### FILL IN HERE ###

    # Return the type of a function abstraction `λx:t. body`, which is
    # represented in code as a nested hash table matching the pattern
    #
    #     {fun: [{var: x, type: t}, body]}

    # The type you should return is `t -> t'` following this rule:
    #
    #     env, x : t ⊢ body : t'
    #     --------------------------
    #     env ⊢ λx:t. body : t -> t'

    # In other words, to infer the type of an expression matching the
    # pattern `{fun: [{var: x, type: t}, body]}` in the current
    # environment `env`, first infer the type of `body` inside of the
    # extension of `env` where the variable name `x` is associated with
    # the given type `t`.  If you find that `body` has the type `t'` in
    # the extended environment, then the original function expression
    # has the type representing the arrow `t -> t'`.

    # REMEMBER: When you are extending the environment `env` so that
    # `env[x] = t`, make sure that the `env` hash table does not get
    # modified!  In other words, you should generate a NEW hash table
    # which is exactly like `env` except that `env[x] = t`, and the OLD
    # hash table `env` should still remain the same as it was before.
    # One way to do this is with the `merge` method of hash tables.
    #
    # If you accidentally change the original hash table `env` itself,
    # you will "leak" out this local parameter that should only be
    # visible inside the function to its surrounding context, which is
    # incorrect.
    
  in {app: [function, argument]}
    ### FILL IN HERE ###

    # Return the type of a function application `(function argument)`,
    # which is represented in code as a nested hash table matching
    #
    #     {app: [function, argument]}
    #
    # The type you should return is `t` following this rule:
    #
    #     env ⊢ function : t' -> t    env ⊢ argument : t'
    #     -----------------------------------------------
    #     env ⊢ (function argument) : t

    # In other words, to infer the type of an application matching the
    # pattern `{app: [function, argument]}` in the current environment
    # `env`, you must
    #
    #   1. first, infer the type of the expression `function` in the
    #      environment `env`, and make sure it is an arrow type
    #      matching the pattern `{arrow: [t', t]}, then
    #
    #   2. second, infer the type of the expression `argument` in the
    #      same environment `env`, and make sure that it is EXACTLY
    #      the same as the type `t'` to the left of the arrow found
    #      previously.
    #
    # If both of the above checks pass, then the type of value
    # returned by the whole application will be the type `t` found to
    # the right of the arrow in step 1.  If EITHER of these steps fail
    # (if `function` has a non-arrow type, or if `argument` has a type
    # which is DIFFERENT than the type to the left of the arrow for
    # `function`), then raise a TypeError signaling that the
    # expression does not type check.

  in {if: [check, if_true, if_false]}
    ### FILL IN HERE ###

    # Return the type of an if-then-else expression which is
    # represented in code as a hash table matching the pattern
    #
    #     {if: [check, if_true, if_false]}
    #
    # The type you should return is `t` following this rule:
    #
    #     env ⊢ check : boolean    env ⊢ if_true : t    env ⊢ if_false : t
    #     ----------------------------------------------------------------
    #     env ⊢ if check then if_true else if_false : t

    # In other words, to infer the type of an expression matching the
    # pattern `{if: [check, if_true, if_false]}` in the current
    # environment `env`, you must
    #
    #   1. confirm that the inferred type of the expression `check` in
    #      the environment `env` matches `:boolean`,
    #
    #   2. infer the types of both the expressions `if_true` and
    #      `if_false` in the current environment `env, and
    #
    #   3. confirm that both the types inferred in the previous step
    #      (2) for `if_true` and `if_false` are the SAME TYPE `t`.
    #
    #  If all of the above checks pass, then the type returned for the
    #  entire if-then-else expression is the shared type `t` which was
    #  inferred for both `if_true` and `if_false`.  If EITHER of these
    #  confirmations fail (you infer some type for `check` other than
    #  `:boolean`, or the types inferred for `if_true` and `if_false`
    #  are not equal to one another), then raise a TypeError
    #  signaling that the expression does not type check.
  end
end

####################
## Extra Credit 2 ##
####################

# If you have completed Exercise 3, there are four different places
# that `infer_type` might raise a TypeError exception detecting these
# scenarios:
#
#   1. A non-function (an expression whose type is not an arrow) is
#      applied to some argument.
#
#   2. The argument to an application does not have the same type as
#      expected by the function applied to it.
#
#   3. The expression checked by an if-then-else is not a boolean.
#
#   4. The two branches returned by an if-then-else do not have the
#      same type as one another.

# For extra credit, attach an informative error message to each one
# of these different TypeError exceptions which captures the relevant
# information which lead to the error.  In each type error message, be
# sure to include:
#
#   1. The reason for the type error, that is to say, which of the
#      four possible scenarios caused you to raise the TypeError.
#
#   2. A description of what specifically when wrong.  For example, a
#      comparison between what expected versus what you got, like
#      "expected a boolean but got 1 : number".  Or a list of the
#      types that should have matched but didn't, like "got 'then'
#      branch 1 : number and 'else' branch false : boolean".
#
#   3. A printed version of the expression in which the type error was
#      found.
#
#   4. The typing environment at the point of the error, which shows
#      the types of free variables in scope which were introduced by
#      the context surrounding the expression.

# To print out the typing environment as a string, consider defining a
# function `print_environment` which takes an environment hash table
# (associating variable names to types) as an argument, and generates
# a string which pretty-prints each variable name and its associated
# type, such as "x : number, f : number->number".


####################
## Extra Credit 3 ##
####################

# For extra credit points, you may add a "recursive let" expression to
# the language.  This appears as another case in the grammar of
# syntactically well-formed expressions looking like:
#
#     M ::= ... | rec x : T = M in M

# The idea behind the recursive let `rec x:T = M in M'` is that it
# introduces a local variable name (x) which is visible in BOTH the
# body of the binding `M'` as well as the expression `M` which is
# named `x`.  In other words, the bound expression `M` may recursively
# refer to itself via the name `x` that it will be bound to.

# The typing rule for a recursive let is:
# 
#     Γ, x : T ⊢ M : T    Γ, x : T ⊢ M' : T'
#     --------------------------------------
#     Γ ⊢ rec x : T = M in M'

# Notice how the local variable name `x:T` is added to the environment
# for checking the term `M : T` that will be locally bound to `x`, and
# also in the environment for the type of the result `M' : T'`.

# To implement this extension to the language, add cases for handling
# a recursive let to the following places:
#
#   1. Represent a recursive let (rec x : T = M in M') as
#
#          {rec: [{var: x, type: T}, M, M']
#
#   2. In `print_expr`, add a case for printing an expression matching
#      the pattern  `{rec: [{var: x, type: T}, M, M']}`.
#
#   3. If you implemented `pretty_print_expr` for Extra Credit 1, add
#      a similar case for pretty-printing a rec expression.
#
#   4. In `infer_type`, add a case for inferring and checking the type
#      of an expression matching the typing rule above.  Note that to
#      correctly infer the type of a rec expression, you will need to
#      extend the environment used to check BOTH sub-expressions with
#      the local variable it introduces.


################
## Test Cases ##
################

# ex01 = plus(5)
# ex01 = 5 + _
ex01 = {app: [{var: :plus}, {num: 5}]}
# (plus 5)
# :
# (number->number)
# =
# number->number


# ex02 = ex1(4) = plus(5)(4)
# ex02 = 5 + 4
ex02 = {app: [ex01, {num: 4}]}
# ((plus 5) 4)
# :
# number
# =
# number


# ex03 = minus(ex02) = minus(plus(5)(4))
# ex03 = ex02 - _ = (5 + 4) - _
ex03 = {app: [{var: :minus}, ex02]}
# (minus ((plus 5) 4))
# :
# (number->number)
# =
# number->number



# ex04 = ex03(3) = minus(plus(5)(4))(3)
# ex04 = (5 + 4) - 3
ex04 = {app: [ex03, {num: 3}]}
# ((minus ((plus 5) 4)) 3)
# :
# number
# =
# number


# ex05 = λx:boolean. λy:boolean. if x then true else y
ex05 = {fun: [{var: :x, type: :boolean},
              {fun: [{var: :y, type: :boolean},
                     {if: [{var: :x},
                           {bool: true},
                           {var: :y}]}]}]}
# (λx:boolean. (λy:boolean. if x then true else y))
# :
# (boolean->(boolean->boolean))
# =
# boolean->boolean->boolean


# ex06 = ex05(false)
ex06 = {app: [ex05, {bool: false}]}
# ((λx:boolean. (λy:boolean. if x then true else y)) false)
# :
# (boolean->boolean)
# =
# boolean->boolean


# ex07 = ex06(false) = ex05(false)(false)
ex07 = {app: [ex06, {bool: false}]}
# (((λx:boolean. (λy:boolean. if x then true else y)) false) false)
# :
# boolean
# =
# boolean


# ex08 = λf.λx. f(f(x))
ex08 = {fun: [{var: :f, type: {arrow: [:number, :number]}},
              {fun: [{var: :x, type: :number},
                     {app: [{var: :f},
                            {app: [{var: :f},
                                   {var: :x}]}]}]}]}
# (λf:number->number. (λx:number. (f (f x))))
# :
# ((number->number)->(number->number))
# =
# (number->number)->number->number


# ex09 = ex08(ex01)
ex09 = {app: [ex08, ex01]}
# ((λf:number->number. (λx:number. (f (f x)))) (plus 5))
# :
# (number->number)
# =
# number->number


# ex10 = ex09(9) = ex08(ex01)(9)
ex10 = {app: [ex09, {num: 9}]}
# (((λf:number->number. (λx:number. (f (f x)))) (plus 5)) 9)
# :
# number
# =
# number


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
# ((λx:number. ((plus x) x)) 11)
# :
# number
# =
# number


# ex12 = let x:number = 12 in
#        let f:boolean->boolean = (λx:boolean. x) in
#        x + x
ex12 = let(:x, :number, {num: 12},
           let(:f, {arrow: [:boolean, :boolean]},
               {fun: [{var: :x, type: :boolean}, {var: :x}]},
               {app: [{app: [{var: :plus}, {var: :x}]},
                      {var: :x}]}))
# ((λx:number. ((λf:boolean->boolean. ((plus x) x)) (λx:boolean. x))) 12)
# :
# number
# =
# number


# ex13 = let x:number = 13 in
#        (λy:number. λx:boolean. y) x false
ex13 = let(:x, :number, {num: 13},
           {app: [{app: [{fun: [{var: :y, type: :number},
                                {fun: [{var: :x, type: :boolean},
                                       {var: :y}]}]},
                         {var: :x}]},
                  {bool: false}]})
# ((λx:number. (((λy:number. (λx:boolean. y)) x) false)) 13)
# :
# number
# =
# number


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
# ((λx:number. ((λf:number->number. ((λx:boolean. (f 1)) false)) (λy:number. ((plus x) y)))) 14)
# :
# number
# =
# number


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
# ((λf:(number->number)->number->number. ((λx:boolean. ((λg:number->number. (g 15)) (f (λx:number. ((minus x) 1))))) false)) (λh:number->number. (λx:number. (h x))))
# :
# number
# =
# number


# ex16 = (λx:boolean.x) 16
ex16 = {app: [{fun: [{var: :x, type: :boolean}, {var: :x}]},
              {num: 16}]}
# Incompatible types in function application:
#
#   expected parameter of type boolean,
#   got argument 16 : number,
#
# in application: ((λx:boolean. x) 16);
#
# context: plus : number->number->number, minus : number->number->number, times : number->number->number, divide : number->number->number, equal? : number->number->boolean, greater? : number->number->boolean

# ex17 = false(17)
ex17 = {app: [{bool: false}, {num: 17}]}
# Non-function operation in application:
#
#   got operation false : boolean,
#
# in application: (false 17);
#
# context: plus : number->number->number, minus : number->number->number, times : number->number->number, divide : number->number->number, equal? : number->number->boolean, greater? : number->number->boolean


# ex18 = let x : number = 1 in
#        let y : boolean = true in
#        if false then x else y
ex18 = let(:x, :number, {num: 1},
           let(:y, :boolean, {bool: true},
               {if: [{bool: false}, {var: :x}, {var: :y}]}))
# Incompatible types in branches of conditional statement:
#
#   got 'then' branch x : number
#   got 'else' branch y : boolean
#
# in expression: if false then x else y
#
#context: plus : number->number->number, minus : number->number->number, times : number->number->number, divide : number->number->number, equal? : number->number->boolean, greater? : number->number->boolean, x : number, y : boolean


# ex19 = true / false
ex19 = {app: [{app: [{var: :divide}, {bool: true}]}, {bool: false}]}
# Incompatible types in function application:
#
#   expected parameter of type number,
#   got argument true : boolean,
#
# in application: (divide true);
#
# context: plus : number->number->number, minus : number->number->number, times : number->number->number, divide : number->number->number, equal? : number->number->boolean, greater? : number->number->boolean


# ex20 = if (λx:number. x) then 1 else 0
ex20 = {if: [{fun: [{var: :x, type: :number}, {var: :x}]},
             {num: 1},
             {num: 0}]}
# Non-boolean guard condition of 'if':
#
#   got guard (λx:number. x) : number->number
#
# in expression: if (λx:number. x) then 1 else 0;
#
# context: plus : number->number->number, minus : number->number->number, times : number->number->number, divide : number->number->number, equal? : number->number->boolean, greater? : number->number->boolean


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
    ty = infer_type(ex)
    puts "#{print_expr(ex)}",
         ":",
         "#{print_type(ty)}",
         "=",
         "#{pretty_print_type(ty)}"
  rescue TypeError => err
    puts err
  end
end
