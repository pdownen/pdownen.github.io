#############################################
###           Classes & Objects           ###
###                                       ###
### Organization of Programming Languages ###
###               Fall 2025               ###
###              Paul Downen              ###
#############################################


# Remember the function

def double(x)
  x + x
end

# Function calls: verb(subject1, subject2, ...)

p double(5)

p double([1,2,3])

# We already know where to find the code to run (up above) associated with the
# function `double`, regardless of the arguments


# Method calls: object.verb(subject1, subject2, ...)

p 5.to_s

p [1,2,3].to_s

# We don't know what `.to_s` means in isolation. The `object` in `object.to_s`
# decides what the method does.

# In Ruby, everything is an object!  So even numbers have (lots of) methods

p 5.methods


# Five essential principles of object-oriented programming (four on page 278 of
# Concepts in Programming Languages, plus one more):
#
#   * Dynamic dispatch: Doing your own thing
#   * Subtyping: If it walks like a duck, and talks like a duck...
#   * Inheritance: Copying your friends
#   * Abstraction: Hiding your secrets
#   * Introspection: Know thyself

## Dynamic Dispatch: Doing your own thing ##

# Ducks are a class of objects that have something in common (they all know how
# to walk and talk) even though they might have some individual differences

class Duck
  def walk
    puts "waddle waddle"
  end

  def talk
    puts "quack!"
  end
end

# Penguins are a different class of objects with something in common (they also
# know how to walk and talk, but a little differently).
class Penguin
  def walk
    puts "waddle waddle"
  end

  def talk
    puts "noot!"
  end
end

# The action of doing the `walk` and `talk` methods depends on the object doing
# them.  Deciding what to do based on the object that appears at run-time,
# rather than by looking up a fixed place in your program at compile-time, is
# *dynamic dispatch*.

donald = Duck.new
donald.walk
donald.talk

pingu = Penguin.new
pingu.walk
pingu.talk


## Subtyping: If it walks like a Duck and talks like a Duck... ##

# The outside world only interacts with objects through their methods.  So it is
# okay to use any object (regardless of its specific class) that implements the
# right sorts of methods for a particular use-case.

def walk_and_talk(duck)
  duck.walk
  duck.talk
end

# Even though the `walk_and_talk` function is "expecting" to get a Duck, there
# is no problem passing in a Penguin instead, since Ducks and Penguins both know
# how to walk and talk.

walk_and_talk(donald)
walk_and_talk(pingu)


## Inheritance: Copying your friends ##

# Sometimes different classes objects might share *exactly* the same behavior
# for some methods (like `walk` of `Duck`s and `Penguin`s) even though they
# differ for other methods (like `talk` of `Duck`s and `Penguin`s).  You can
# copy&paste shared code into another class using *inheritance*.

# Although there are all sorts of different `Animal`s, they all can talk.

class Animal
  def talk
    puts "..."
  end
end

Animal.new.talk

# A `Fish` is a subtype of `Animal` (it is a more specific class of `Animal`) so
# anything an `Animal` can do a `Fish` can do.  In addition, `Fish` know how to
# `swim`.

class Fish < Animal
  def swim
    puts "The fish swims"
  end
end

# The `Fish` class of objects inherits the same behavior for `talk` from the
# more general `Animal` class of objects.

nemo = Fish.new
nemo.swim
nemo.talk

# A `Frog` is also a more specific type of `Animal`.  However, `Frog`s talk
# differently.

class Frog < Animal
  def talk
    puts "Ribbit"
  end

  def jump
    puts "The frog hops"
  end
end

# A method like `talk` that would be inherited from the superclass can be
# selectively overridden with custom behavior.

kermit = Frog.new
kermit.talk
kermit.jump


## Abstraction: Hiding your secrets ##

# Even if objects from the same class share many similarities, they can still be
# unique individuals.  *Instance variables* are pieces of information that are
# associated with just one individual object.  Different objects of the same
# class will have their own instance variables.

# Instance variables are like normal variables, but they always start with '@'.

class Named < Animal
  # `initialize` is a special method of a class that says what to do when you
  # make a `new` object.  By default, `initialize` takes no parameters and does
  # nothing.  But you can override `initialize` to take parameters and do
  # something with them.  Creating a `new` `Named` object takes a string which
  # it stores in the instance variable `@name`.
  def initialize(string)
    @name = string
  end

  # When a `Named` animal talks, it can say *who* is doing the talking.
  def talk(phrase = "...")
    puts "#{@name} says '#{phrase}'"
  end
  # The `phrase` parameter is given a default value "value".  This means that if
  # you call object.talk("some text") with a parameter, then `phrase` will be
  # bound to "some text".  But if you call object.talk() with no parameters,
  # then `phrase` will be the default "...".
end

# Different named objects can have different behavior, because what they do
# depends on their personal `@name`.

me = Named.new("Paul")
me.talk
me.talk("Wow!")

Named.new("Joe").talk("Hello")


# Sometimes you want to only modify an inherited method, instead of overriding
# it from scratch.
class Dog < Named
  def talk
    # The `super` keyword says to call the method of the same name (in this
    # case, `talk` because it appears inside the body of a `Dog`'s `talk`
    # method) as found in the superclass (in this case the one from `Named`,
    # because `Dog < Named`).
    super("woof!")
  end

  def fetch(thing)
    puts "#{@name} fetches #{thing}!"
  end
end

lassie = Dog.new("Lassie")
lassie.talk
lassie.fetch("a stack")

# Instance variables (which always start with an '@') are totally private; the
# only one who can read/write directly to an '@' variable is the object that it
# belongs to.  A class of objects can choose to reveal an instance variable to
# the outside world by providing methods that give access to it.
class NameChange < Named
  # A "getter" is a method that just returns the value of an instance variable
  # and does nothing else.  Good Ruby style is to have the getter use the same
  # name as the instance variable, but without the '@'
  def name
    @name
  end

  # A "setter" is a method that updates an instance variable with a new value
  # (provided as a parameter) and does nothing else.  Ruby looks methods with
  # the special naming convention `name=` (where `name` could be any variable
  # name).  These are used as setters associated with the normal `=` syntax
  # object.name = value.
  def name=(new_name)
    @name = new_name
  end
end

joe = NameChange.new("Joe")
joe.talk("Hello")

puts joe.name
joe.name = "Bob"
joe.talk("Do you like my new name?")

# It can be tedious to write getters and setters by hand, especially for objects
# with many instance variables.  Thankfully, Ruby provides a shorthand that will
# generate them automatically.
class ExpressNameChange < Named
  # The command `attr_accessor :name` will generate getter and setter methods
  # for the instance variable `@name`. This generates exactly the same code as
  # found in `NameChange` above.
  attr_accessor :name
end


## Introspection: Know thyself ##

# All objects are conscious: they are aware of themselves.  An object can refer
# to itself through the special `self` keyword to complete its own methods.

# `Cat`s are another class of `Named` `Animal`
class Cat < Named
  # Each `Cat` has public getters/setters for their personal `@name`
  attr_accessor :name

  # `Cat`s love to `sleep`.  Good object-oriented programming style will use the
  # public getter (when available) rather than reading the raw instance variable
  # itself.  To access its own getter, the `sleep` method of a `Cat` can use the
  # `self` keyword to refer to the object doing the method.
  def sleep
    puts "#{self.name} takes a nap"
  end
  # In other words, if you call `specific_cat.sleep`, then the `self.name`
  # inside of `sleep` will be resolved to `specific_cat.name`.

  def eat(food)
    puts "#{self.name} enjoys a nice meal of #{food}"
  end
  # Using `self.name` instead of `@name` in a method makes your code more
  # "future proof:" if some sub-class redefines how to store and access its
  # name, then that new code will be automatically used by this definition of
  # `sleep` and `eat`.

  # Besides just getters and setters, methods of an object can use `self` to
  # refer to and invoke any other methods of that object.  Here, the
  # daily_schedule of a `Cat` is pretty lazy, and revolves around a lot of
  # napping.
  def daily_schedule(food)
    self.sleep
    self.eat(food)
    self.sleep
  end
end

garfield = Cat.new("Garfield")
puts garfield.name

garfield.talk
garfield.daily_schedule("cat food")

# `garfield` is a `Cat`, indeed, but `garfield` is also a very special `Cat`
# that hates Mondays.  Just like an individual object can have its own personal
# data not shared by anyone else, Ruby also lets individual objects have their
# own personal methods not shared by anyone else (even others of the same
# class).  `garfield` can have a special `daily_schedule` different from other
# `Cats` personalized to his most favorite food (lasagna) and his least favorite
# day (Monday).
class << garfield
  def daily_schedule(day = 'Sunday')
    if 'Monday' == day
      self.talk("I hate Mondays...")
    else
      super("lasagna")
    end
  end
end

garfield.daily_schedule
garfield.daily_schedule("Monday")

# `garfield`s personal `daily_schedule` doesn't affect any other `Cat`s that
# might come along.

Cat.new("Cheshire cat").daily_schedule("biscuits and tea")


## Fine-grained access control ##

# Here's another example of more fine-grained access control to private instance
# variables in shapes. `SecretCircle`s and `SecretRectangle`s have internal
# dimensions (a radius or a height and width) that no one else knows, but can
# sill be used to calculate area and circumference.

class SecretCircle
  def initialize(r)
    @radius = r
  end

  def area
    (Math::PI * @radius) ** 2
  end

  def circumference
    2 * Math::PI * @radius
  end
end

class SecretRectangle
  def initialize(h, w)
    @height = h
    @width = w
  end

  def area
    @height * @width
  end

  def circumference
    2 * (@height + @width)
  end
end

def check_shape(shape)
  puts shape.area
  puts shape.circumference
end

check_shape(SecretCircle.new(1))
check_shape(SecretRectangle.new(2,3))

# We can selectively choose to give read-only or write-only access to an
# instance variable like `@radius` by providing only a getter or only a setter
# method, respectively.
class PermanentCircle < SecretCircle
  def radius
    @radius
  end
end

class AdjustableCircle < SecretCircle
  def radius=(r)
    @radius = r
  end
end

puts PermanentCircle.new(1).radius

c = AdjustableCircle.new(1)
puts c.area
c.radius = 2
puts c.area

# Ruby provides other shorthands for choosing how much public access is given to
# an instance variable:
#
#   * attr_reader provides only getter methods for each listed variable name
#
#   * attr_writer provides only setter methods for each listed variable name
#
#   * attr_acccessor provides both getter and setters
#
# A single class definition can mix-and-match as many `attr_reader`,
# `attr_writer`, and `attr_accessor` commands as you want.

class PermanentRectangle < SecretRectangle
  attr_reader :height, :width
end

class AdjustableRectangle < SecretRectangle
  attr_writer :height, :width
end

class Rectangle < SecretRectangle
  attr_accessor :height, :width
end
