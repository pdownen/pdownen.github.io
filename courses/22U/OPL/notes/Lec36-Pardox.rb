
def fact_loop(n)
  total = 1
  for i in 1..n do
    total = i * total
  end
  return total
end

def fact_rec(n)
  if n == 0 then
    1
  else
    n * fact(n-1)
  end
end

def test(range = (0..5))
  range.map {|i| [i, yield(i)]}
end

def y(&code)
  selfless = lambda do |&itself|
    code.call { itself.call(&itself) }
  end
  selfless.call(&selfless)
end

def fact_y
  y do |&recurse|
    lambda do |n|
      if n == 0 then
        1
      else
        n * recurse.call.call(n-1)
      end
    end
  end
end

def sum(n)
  y do |&recurse|
    lambda do |i, total|
      if i > n then
        total
      else
        recurse.call.call(i+1, i+total)
      end
    end
  end.call(0, 0)
end


def fun(&code)
  selfless = lambda do |*args, &itself|
    code.call(*args) {|*new_args| itself.call(*new_args, &itself) }
  end
  lambda {|*args| selfless.call(*args, &selfless)}
end

def fact_fun
  fun do |n, &recurse|
    if n == 0 then
      1
    else
      n * recurse.call(n-1)
    end
  end
end

def count_down(start = 9)
  fun do |i, &recurse|
    if i == 0 then
      "blast off!"
    else
      "#{i}, #{recurse.call(i-1)}"
    end
  end.call(start)
end
