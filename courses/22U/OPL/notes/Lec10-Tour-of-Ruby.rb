#############################################
###             Tour of Ruby              ###
###                                       ###
### Organization of Programming Languages ###
###              Spring 2022              ###
###              Paul Downen              ###
#############################################


# These notes are a Ruby script! You can run it with the command
#
#     ruby Lec10-Intro-to-Ruby.rb
#
# or you can start your own interactive Ruby session with the command
#
#     irb
#
# And follow along by typing these expressions into the interpreter.

# (Anything following a '#' is a comment, and is ignored by Ruby)


# Motto:
#
#     There's more than one way to do it!


#########################
### Atomic Data Types ###
#########################

## Numbers ##

# You can print out any value, like the number `1`, using `puts`

puts 1

# You can write numeric expressions using all of the basic arithmetic
# operations that you are used to

puts 5 + 6

# Giving a name to an expression lets you use its value again later

two = 1 + 1

puts two * two

# Be careful about division: there is a difference between "floating
# point" division (which can give you a fractional number when the two
# don't divide evenly) versus "integer division" (which always rounds
# down and drops the remainder entirely).

puts 3 / 2

puts 3.0 / 2

puts 3 / 2.0

# Ruby is *dynamically typed*, and the / operator decides which
# version of division to use based on the type of numbers it is given.
# Many operations and functions in Ruby will automatically adapt to
# the actual arguments they receive to do something sensible.  Because
# the 3 and 2 in `3 / 2` are both integers, the result is calculated
# to 1 using integer division.  In the other two cases, 3.0 or 2.0` is
# a floating-point number, so the result of both `3.0 / 2` and `3 /
# 2.0` is calculated to 1.5 using floating-point division.


## Booleans ##

# The two boolean values are `true` and `false`
puts true
puts false


puts (true and false)
puts (true or  false)

puts (true or 1/0)


## Strings ##

# Strings are enclosed by either double quotation marks ("") or single
# quotation marks ('')

puts "Hello, World!"
puts 'Goodbye, World!'

# You can combine two strings with +

puts 'a' + "b"

# But you cannot + strings with other things like numbers. Both of the
# following expressions cause errors if you try to evaluate them:

# 'a' + 1
# 1 + 'a'

# Instead, if you want to insert another value into a string, you
# splice it in using the special syntax:

puts "This is four: #{2 * 2}"

# You can splice *any* expression into a string (as long as Ruby knows
# how to convert it to a string, like the number 4). You can also
# splice previously-named expressions into a string.

puts "This is two: #{two}"


###########################
### Compound Data Types ###
###########################

## Arrays ##

# Arrays values can be made directly via the array syntax, which
# encloses a comma-separated sequence of values in square brackets []

puts [1, 2, 3]

# As you may have noticed, `puts` prints each element of an array on a
# separate line. If you want to just print the whole array on the same
# line, you can use `p`

p [1, 2, 3]

# Any combination of values can be combined together in the same
# array, regardless of what types of things they are

p ["apples", "bananas"]
p [1, 2.0, 'c']

# You can access a particular element of an array by indexing it; the
# index 0 refers to the first element, 1 refers to the second, and so
# on.

abc = ['a', 'b', 'c']

puts abc[1]
puts abc[2]

# You can add more elements to an array with +

abc = abc + ['d', 'e', 'f']

p abc

# And you can change the element at a particular index by writing the
# chosen index to the left of the =

abc[5] = 'z'

p abc


## Ranges ##

# Ranges let you quickly count from a starting point to an end
# point.  These could be numbers or letters.

puts (1..10)
puts ('a'..'z')

# The `to_a` method converts a range to a full array.  To use a method
# like `to_a`, you write it *after* the main object with a `.`

p (1..10).to_a
p ('a'..'z').to_a

# In case you want slight variations on the end-point, three dots lets
# you count from the start to *one less than* the end point.  And
# leaving out an end point lets you count forever.

p (1..10).to_a
p (1...10).to_a
p (1..)  # An endless range can't be converted to an array, because
         # your computer memory can't fit it all


## Hashes / Dictionaries ##

# Hashes (also known as "dictionaries") let you associate any indexes
# of your choice with some values.  They are like a generalization of
# arrays where indexes don't have to be the sequence of numeric
# positions starting from 0.

puts({"Paul" => "Downen", "River" => "Hawks"})

# The indexes can be any type of things (strings, numbers, etc), as
# can the values.

puts({"a" => 1, "b" => 2, true => "Great!"})

# Reading and writing a hash at an index uses the same syntax as
# arrays. Writing to an index that isn't already in the hash will add
# it as a new index-value pair.

alpha_num = {'a' => 1, 'b' => 2, 'c' => 3}

puts alpha_num['b']

alpha_num['b'] = "banana"
alpha_num['z'] = 0

puts alpha_num

# In the special case where you always want to associate names to
# values, you can use the shorthand colon notation

puts({a: 1, b: 2, c: 3})

# This colon-hash uses a lightweight form of *symbols* for the names
# of indexes, which is more efficient than looking up a particular
# string to read/write inside of a hash


#######################
### Loops & Control ###
#######################

## Function definitions ##

# Functions are defined using this 'def' syntax:

def double(num)
  return num + num
end

# The final expression of a function is always implicitly returned, so
# the `return` keyword is optional for simple functions.

def square(num)
  num * num
end


## Repetition and iteration ##

# You can repeat an action several times with the `times` methods.

5.times { puts "Hooray!" }

# You can iterate through each individual element of any group ---
# like an array, hash, or range --- using the `each` method.

lunch = ['coffee', 'cheese', 'bread']

lunch.each { |food| puts food.capitalize }

alpha_num.each do |index, value|
  puts "#{index} => #{value}"
end

def print_squares(numbers)
  numbers.each do |x|
    puts square(x)
  end
end

puts print_squares(1..10)


## Conditionals and branching ##

# The if-then-else expression lets you do something different
# depending on some conditional.

mil = 10 ** 6

if mil > 999
then
  puts "#{mil} is big!"
else
  puts "#{mil} is small..."
end

# Sometimes you just want a single line to happen or not depending on
# some condition.  You can do this quickly by adding a side-condition
# starting with 'if' at the end of that line.

puts "Never happens" if mil < 10
puts "Does happen" unless mil < 10

def abs(num)
  return -num if num <  0
  return  num if num >= 0
end
