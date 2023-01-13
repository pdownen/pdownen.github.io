#############################################
###     Programming With Continuations    ###
###                                       ###
### Organization of Programming Languages ###
###              Spring 2023              ###
###              Paul Downen              ###
#############################################

3.times { puts "Hello!" }

(1..5).each do |i|
  puts i
end

for i in 1..5 do
  puts i
end

def do_it_thrice()
  yield
  yield
  yield
  return
end

do_it_thrice { puts "Hello!" }

x = 2

puts x
do_it_thrice do
  x = x+x
end
puts x

def one_two_three()
  yield 1
  yield 2
  yield 3
  return
end

one_two_three {|i| puts i}

x = 0
puts x
one_two_three do |i|
  x = x + i
end
puts x

def repeat_times(n)
  while n > 0 do
    yield
    n = n-1
  end
end

repeat_times(2) { puts "Hello!" }

repeat_times(3) do
  puts "Farewell"
end

repeat_times(0) do
  puts "Silent..."
end

def count(start, stop, step=1)
  while start <= stop
    yield start
    start = start + step
  end
  return start
end

count(1,3) {|i| puts i}

count(1,10,2) {|i| puts i}

def list_three
  x = yield
  y = yield
  z = yield
  return [x, y, z]
end

require 'pp'

pp list_three { "same" }

x = 0
vals = list_three do
  x + 1
  x
end
pp vals

def iterate(x)
  while true
    x = yield(x)
  end
  return x
end

iterate(2) do |x|
  puts x
  break if x >= 10000
  x * x
end

def forward(array)
  n = 0
  while n < array.length do
    yield array[n]
    n = n+1
  end
  return array
end

fruits = ["apple", "banana", "mango", "orange", "pear"]

forward(fruits) do |snack|
  puts "Mmm, a #{snack}!"
end

def backward(array)
  n = array.length - 1
  while n >= 0 do
    yield array[n]
    n = n-1
  end
  return array
end

backward(fruits) do |snack|
  puts "Mmm, a #{snack}!"
end

$four_by_four = [[ 1, 2, 3, 4],
                 [ 5, 6, 7, 8],
                 [ 9,10,11,12],
                 [13,14,15,16]]

def by_rows(matrix)
  for row in 0...(matrix.length) do
    for col in 0...(matrix[row].length) do
      yield matrix[row][col]
    end
  end
  return matrix
end

by_rows($four_by_four) {|n| puts n}

def by_columns(matrix)
  col = 0
  finished = false
  while not finished do
    finished = true
    for row in 0...matrix.length do
      if col < matrix[row].length
        finished = false
        yield matrix[row][col]
      end
    end
    col = col + 1
  end
  return matrix
end

by_columns($four_by_four) {|n| puts n}

def diagonally(matrix)
  row = 0

  while row < matrix.length do

    for col in 0..row do
      yield matrix[row-col][col]
    end

    row = row+1
  end
end

diagonally($four_by_four) {|n| puts n}
