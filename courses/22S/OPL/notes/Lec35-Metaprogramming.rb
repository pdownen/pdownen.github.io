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

class Integer
  def iterate
    return lambda do |z, &s|
      x = z
      self.times { x = s.call(x) }
      return x
    end
  end
end

def true.if
  yield().then()
end

def false.if
  yield.else
end

true.if do
  Class.new do
    def then
      puts "it's true"
    end
    def else
      puts "it's false"
    end
  end.new
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
