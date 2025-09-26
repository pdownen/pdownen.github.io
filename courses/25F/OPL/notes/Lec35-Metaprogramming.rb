def select_true
  return lambda {|if_true, if_false| if_true}
end

def select_false
  return lambda {|if_true, if_false| if_false}
end

def select_and(b1, b2)
  b1.call(b2, b1)
end

def zero
  return lambda {|z, &s| z}
end

def succ(n)
  return lambda do |z, &s|
    s.call(n.call(z, &s))
  end
end

def number(n)
  n.call(0) {|x| x+1}
end

def dots(n)
  n.call("") {|ddd| "#{ddd}."}
end

def plus(n, m)
  return lambda do
    |z, &s| n.call(m.call(z, &s), &s)
  end
end

def times(n, m)
  return lambda do |z, &s|
    n.call(z) {|x| m.call(x, &s)}
  end
end

def true.if
  branches = yield()
  branches.then()
end

def false.if
  yield.else
end

class CheckTrueFalse
  def then
    puts "it's true"
  end

  def else
    puts "it's false"
  end
end

true.if do
  CheckTrueFalse.new
end

false.if do
  Class.new do
    def then
      puts "now it's true"
    end
    def else
      puts "now it's false"
    end
  end.new
end

class Integer
  def iterate(z)
    x = z
    self.times { x = yield(x) }
    return x
  end

  def church
    return lambda do |z, &s|
      self.iterate(z, &s)
    end
  end
end
