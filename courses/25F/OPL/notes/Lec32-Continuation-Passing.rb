#############################################
###      Continuation-Passing Style       ###
###                                       ###
### Organization of Programming Languages ###
###              Spring 2023              ###
###              Paul Downen              ###
#############################################


def increment(x)
  return x + 1
end

def double(x)
  return x + x
end

def square(x)
  return x * x
end

puts increment(10)
puts double(10)
puts square(10)


inc = lambda {|x| x + 1}
dub = lambda {|x| x + x}
sqr = lambda {|x| x * x}

$ops = [inc, dub, sqr]

for f in $ops do
  puts f.call(10)
end

def apply(f, x)
  return f.call(x)
end

def apply_cont(x)
  return yield(x)
end

def apply_block(x, &block)
  return block.call(x)
end

def apply_twice(f, x)
  return f.call(f.call(x))
end

def apply_cont_twice(x)
  return yield(yield(x))
end

def apply_block_twice(x, &block)
  return block.call(block.call(x))
end

def curry(f)
  lambda {|x| lambda {|y| f.call(x,y) }}
end

add = lambda {|x, y| x + y}

def uncurry(f)
  lambda {|x, y| f.call(x).call(y) }
end

def curry_cont
  lambda {|x| lambda {|y| yield(x,y) } }
end

def uncurry_cont
  lambda {|x, y| yield(x).call(y)}
end


class Control
  attr_accessor :code

  def initialize(f)
    self.code = f
  end

  def run
    self.code.call { |x| x }
  end

  def then
    self.code.call { |x| yield x }
  end

  def then(&cont)
    self.code.call(&cont)
  end

  def self.just(x)
    Control.new(lambda { |&cont| cont.call(x) })
  end

  def self.label
    Control.new(lambda { |&cont| yield(cont) })
  end

  def self.label(&subroutine)
    Control.new(lambda { |&cont| subroutine.call(cont) })
  end
end

Control.just(4).then do |x|
  Control.just(2).then do |y|
    Control.just(x*y).then do |z|
      Control.just(puts z).then do |_|
        Control.just(z)
      end
    end
  end
end

=begin
λk₀.(λk₁. k₁(4))
     λx.(λk₂. k₂(2))
        λy.(λk₃. k₃(x*y))
            λz.(λk₄. puts z; k₄(nil))
                λ_. k₀(z)
=end

result = Control.just(4).then do |x|
  Control.label do |future|
    future.call(2).then do |a|
      future.call(20).then do |b|
        future.call(200).then do |c|
          Control.just(a+b+c)
        end
      end
    end
  end.then do |y|
    Control.just(x*y).then do |z|
      Control.just(puts z).then do |_|
        Control.just(z)
      end
    end
  end
end
puts result.run

=begin
λk₀.(λk₁. k₁(4))
     λx.(λfuture.
         λk'. future 2
              λa. future 20
                  λb. future 200
                      λc. k'(a+b+c))
         λy.(λk₃. k₃(x*y))
             λz.(λk₄. puts z; k₄(nil)))
                 λ_. k₀(z)
=end


class Throw < Control
  attr_accessor :raised
  
  def initialize(x)
    self.raised = x
  end

  def handle
    yield(self.raised)
  end

  def then
    self
  end

  def run
    raise Exception.new(self.raised)
  end
end

class Control
  def handle
    self
  end
end

def divide_throw(x, y)
  if
    y == 0
  then
    Throw.new(x)
  else
    Control.just(y)
  end.then do |nonzero|
    Control.just(x / nonzero)
  end
end


def test_throw(over, under)
  div = divide_throw(over, under).handle do |numerator|
    Control.just("Could not divide #{numerator} by zero")
  end
  result = div.run
  puts "#{over}/#{under} = #{result}"
  return result
end

test_throw(10, 5)
test_throw(10, 0)

class Pause < Throw
  attr_accessor :resume

  def initialize(x, k = lambda{|x| Control.just(x)})
    super(x)
    self.resume = k
  end

  def then
    more = lambda{ |x| self.resume.call(x).then{ |y| yield y } }
    Pause.new(self.raised, more)
  end

  def then(&cont)
    more = lambda{ |x| self.resume.call(x).then(&cont) }
    Pause.new(self.raised, more)
  end

  def handle
    yield(self.raised, self.resume)
  end
end

def divide_pause(x, y)
  if
    y == 0
  then
    Pause.new(x)
  else
    Control.just(y)
  end.then do |z|
    Control.just(x / z)
  end
end

$default_denominator = lambda {|numerator| 2}

def test_pause(over, under)
  div = divide_pause(over, under).handle do |numerator, resume_division|
    nonzero = $default_denominator.call(numerator)
    puts (
"Tried to divide #{numerator} by zero; \
resume using #{nonzero} instead")
    resume_division.call(nonzero)
  end
  result = div.run
  puts "#{over}/#{under} = #{result}"
  return result
end

test_pause(10, 5)
test_pause(10, 0)
