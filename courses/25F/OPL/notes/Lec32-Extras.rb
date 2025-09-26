#############################################
###  Continuation-Passing Style (Extras)  ###
###                                       ###
### Organization of Programming Languages ###
###               Fall 2025               ###
###              Paul Downen              ###
#############################################


class Choose
  attr_accessor :options
  
  def initialize(list)
    self.options = list
  end

  def self.just(value)
    return self.new([value])
  end

  def self.nothing
    return self.new([])
  end

  def then
    new_options =
      self.options
      .flat_map { |x| yield(x).options }
    return Choose.new(new_options)
  end
end

def factors(n)
  Choose.new(1..n).then do |d|
    if n % d == 0
    then
      Choose.just(d)
    else
      Choose.nothing
    end
  end
end

def splits(n)
  Choose.new(1..n).then do |x|
    Choose.new(1..n).then do |y|
      if x * y == n
      then Choose.just([x,y])
      else Choose.nothing
      end
    end
  end
end


class Prob
  attr_accessor :dist
  
  def initialize(table)
    self.dist = table
  end

  def self.just(value)
    return self.new({value => 1.0})
  end

  def self.nothing
    return self.new({})
  end

  def self.uniform(list)
    uniform_table = {}
    list.each {|x| uniform_table[x] = 1}
    return self.new(uniform_table)
  end

  def then
    new_table = {}
    self.dist.each do |value, prob|
      result = yield(value)
      new_table.merge!(result.dist) do |val, prob1, prob2|
        prob1 + prob2
      end
    end
    return Prob.new(new_table)
  end

  def normalize
    total = 0.0
    self.dist.each do |value, prob|
      total = total+prob
    end
    self.dist.each do |value, prob|
      self.dist[value] = prob / total
    end
  end
end


coin = Prob.uniform(["heads", "tails"])

def dice(number=1, sides=6)
  return Prob.just(0) if number <= 0
  
  Prob.uniform(1..sides).then do |roll|
    dice(number-1, sides).then do |others|
      Prob.just(roll + others)
    end
  end
end
