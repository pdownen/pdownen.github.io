module Lecture11 where

-- We will be re-defining some of the standard functions which are
-- normally automatically imported from the Prelude, so here we must
-- explicitly hide those definitions so we can give our own.
import Prelude hiding (take, repeat, cycle, zip, zipWith)

-- Haskell uses "lazy evaluation." This means that the arguments to
-- functions and constructors are only ever evaluated (meaning,
-- simplified down to a basic value like a specific number for
-- Integers or True or False for Bools) when they are "needed." Lazy
-- evaluation can be seen as an optimization (since there are no side
-- effects, there is no point in evaluating a function call if the
-- result is never used). But one extra consequence of lazy evaluation
-- is data structures like lists can be *infinite*, and go on
-- forever. For example, the range [1..10] includes just the numbers
-- from 1 to 10
--
--     [1..10] = [1,2,3,4,5,6,7,8,9,10]
--
-- but the range [1..] which does not specify an upper bounds includes
-- ALL the numbers equal to or greater than 1
--
--     [1..] = [1,2,3,4,5,6,7,8,9,10,11,12,...

-- By default, the 'take' is automatically imported from the
-- Prelude. It's definition is:
take :: Int -> [a] -> [a]
take _ []     = []
take 0 _      = []
take n (x:xs) = x : (take (n-1) xs)
-- 'take' will cut off a list to a given length. For example,
--
--     take 5 [1,2,3,4,5,6,7,... = [1,2,3,4,5]
--
-- This works on both finite *and* infinite lists.
--
--     myTake 10 [1..] = [1..10]

-- The practical use-case is that you can modify infinite data
-- structures like lists first *before* you cut them down into the
-- part you actually need.  For example, you can filter an infinite
-- list to throw out elements you don't need before deciding how many
-- you want: since the filter applies first, you still get the desired
-- number of elements at the end. Compare the difference with the
-- following to combinations of taking and filtering:
--
--    take 5 (filter even [1..]) = [2,4,6,8,10]
--    filter even (take 5 [1..]) = [2,4]

-- Here are some basic functions for generating infinite lists. Note
-- that they are just recursive functions; nothing special needs to be
-- done to activate laziness. When you call these functions, you don't
-- see an infinite loop. Instead, you only see the amount of their
-- returned list that you actually use.

-- The 'repeat' function from Prelude is defined as:
repeat :: a -> [a]
repeat x = x : repeat x
-- For example,
--
--     repeat 1 = [1, 1, 1, 1, 1, 1, ...

-- The 'cycle' function from Prelude is defined as:
cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs
-- For example,
--
--     myCycle [1,2,3] = [1,2,3,1,2,3,1,2,3,...

-- Another definition, using the local looping 'go' function
cycle' xs = go xs
  where go [] = go xs
        go (x:xs') = x : go xs'

-- The example expression from the very first assignment:
--
--     let motor sounds = sounds "Vroom! " in motor cycle

motor :: (String -> a) -> a               -- The most general type of motor
-- motor :: (String -> String) -> String  -- A more specific type
motor sounds = sounds "Vroom! "

rumble :: String
rumble = motor cycle

{-
You can use equational reasoning to simplify 'rumble' and figure out
exactly what it is doing.

rumble
=
motor cycle
=
cycle "Vroom! "
=
cycle ['V','r','o','o','m','!',' ']
=
['V','r','o','o','m','!',' ','V','r','o','o','m','!',' ',...
-}

-- Laziness can be very good for efficiency, too.  Consider the 'fib'
-- function which computes the n^th Fibonacci number:
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- The performance of 'fib n' scales poorly as 'n' gets bigger,
-- because the third line defining 'fib n' in general causes TWO
-- recursive calls to 'fib'.  That means that the cost of 'fib n' is
-- exponential in the size of 'n'; very bad.

-- Before optimizing 'fib', let's look at two useful helper functions
-- when working with lists.

-- The 'zip' function from Prelude is defined as:
zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : (zip xs ys)
zip _      _      = []
-- 'zip' combines two lists together, pairing together elements from
-- the two lists in sequence.  If either list runs out of elements,
-- then their 'zip' is done.  So the length of 'zip xs ys' with be the
-- shortest length between 'xs' and 'ys'.  But what happens if the two
-- lists both go on forever (like 'zip (repeat 0) (repeat 1)')?  Given
-- two infinite lists, 'zip' will produce an infinite list combining
-- their elements pairwise:
--
--     zip (x0:x1:x2:...) (y0:y1:y2:...) = (x0,y0):(y1,x1):(x2,y2):...

-- The 'zipWith' function from Prelude is defined as:
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = (f x y) : (zipWith f xs ys)
zipWith _ _      _      = []
-- 'zipWith' generalizes 'zip' by taking a function 'f' which combines
-- the individual elements x and y from the two lists, rather than
-- always just pairing those elements together.

-- The difference between 'zip' and 'zipWith' is largely convenience,
-- since the two can be defined in terms of one another.
--
--     zip       xs ys = ZipWith (\x y -> (x,y)) xs ys
--     zipWith f xs ys = map (\(x,y) -> f x y) (zip xs ys)

-- Now, rather than computing the Fibonacci numbers one-at-a-time, we
-- can instead collect a sequential list that contains all the
-- Fibonacci numbers in order.  This list of Fibonacci numbers is
--
--     fibs = [0, 1, 3, 5, 8, 13, ...
--
-- which can be calculated from this infinite list:
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Notice how much more efficient looking up elements in the list
-- 'fibs' is compared with calling the function 'fib'!  Lazy
-- evaluation means that the list 'fibs' is only computed up to the
-- point that is needed to answer questions from the user, which is
-- how we can safely deal with the infinite list.  In addition, once
-- an element of 'fibs' is calculated the first time, its value is
-- automatically remembered so that it can be quickly looked up in the
-- future.  This is called "memoization", and is a great source of
-- efficiency, since it avoids recomputing the same answers again and
-- again.

-- In your algorithms classes, you may have encountered a style of
-- algorithm called "dynamic programming", where you store
-- intermediate results in a lookup table so that they don't need to
-- be recomputed on other steps.  Dynamic programming is an
-- application of memoization, which automatically manages that lookup
-- table in the background, so you don't have to program it yourself.

-- We can also collect a list of all the (infinitely many) prime
-- numbers.  First, 'sieve' will repeatedly filter an infinite list,
-- to make sure that each element of the output does not evenly divide
-- any of the following numbers.
sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

-- The prime numbers are then calculated by sieving the list of all
-- numbers beginning with 2.
primes :: [Integer]
primes = sieve [2..]


-- Laziness is not something special about lists. EVERY data type in Haskell is
-- lazily evaluated, including custom data types you define for yourself.

-- For example, binary trees are lazily evaluated.
data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving Show

-- We can grow a tree without bound. 
grow :: (a -> a) -> (a -> a) -> a -> Tree a
grow f g x = Branch (grow f g (f x)) x (grow f g (g x))

-- 'prune' cuts off a given tree to a specified height, throwing away all
-- sub-trees below that depth. 'prune' is the analogous function as 'take' but
-- for binary trees instead of sequential lists.
prune :: Integer -> Tree a -> Tree a
prune 0 _              = Leaf
prune _ Leaf           = Leaf
prune n (Branch t x u) = Branch (prune (n-1) t) x (prune (n-1) u)

-- Now, recall the 'growTree' function from lecture 7, that is like 'grow', but
-- it stops after a given depth is reached.
growTree :: (a -> a) -> (a -> a) -> a -> Int -> Tree a
growTree f g x 0 = Leaf
growTree f g x n = Branch
                   (growTree f g (f x) (n-1))
                   x
                   (growTree f g (g x) (n-1))
-- growTree is equal to a composition of the above grow and prune functions.
--
--     growTree f g x n = prune n (grow f g x)


-- Laziness makes it possible to "tie the knot" so that a result can
-- depend on itself in unplanned ways.  For example, here is a
-- function that turns a list of questions into a list of answers.
introspect :: [[a] -> a] -> [a]
introspect questions = let answers = [ q answers | q <- questions ]
                       in  answers
-- The input list contains functions; each function in the list
-- expects to see all the answers before producing its individual
-- result. 'introspect' works by creating a self-referrential list of
-- 'answers'. Each individual answer in the list is obtained by asking
-- one of the 'q's in the list of 'questions' what its answer is,
-- assuming that the 'answers' are already known.  Since the list of
-- 'answers' will be figured out lazily---each individual answer will
-- only be computed at the last moment when its needed for something
-- else---the 'introspect' operation will succeed as long as the given
-- list of questions does not have a cycle in it.

-- For example, here is a list of questions (functions [Int] -> Int
-- that take a list of all Int answers and produce one Int answer),
-- where the first question depends on the third answer (at index 2),
-- the second question depends on nothing, and the third question
-- depends on the second answer (at index 1).
noncyclic = [ (\xs -> xs!!2 * 3),
              (\xs -> 5),
              (\xs -> xs!!1 + 2) ]
-- Note that the (!!) operator takes a list 'xs' (as the left
-- argument) and an index 'i' (as the right argument) and looks up the
-- i^th element of xs (beginning with 0 as the first index).  Check
-- what you get for
--
--     introspect noncyclic

-- In contrast, here is a cyclic list of questions, where the first
-- one depends on the answer to the second, and the second one depends
-- on the answer to the first.
cyclic = [ (\xs -> xs!!1 + 1),
           (\xs -> xs!!0 * 2) ]
-- What happens when you try to introspect to tie this knot?
--
--     introspect cyclic

-- Rewriting the introspect operation slightly, we can replace the
-- list comprehension used to compute the answers with the 'map'
-- function. introspect' below does the same thing as solve up above.
introspect' :: [[a] -> a] -> [a]
introspect' questions = let answers = map (\q -> q answers) questions
                        in  answers

-- Recall the 'fmap' function, which generalizes 'map'ping an
-- operation over a list to instead apply an operation everywhere
-- throughout any container-like data structure (known as a Functor).
-- A quick swap of 'fmap' for 'map' generalizes the knot-tying magic
-- to apply to all sorts of data structures; not just lists.
solve :: Functor f => f (f b -> b) -> f b
solve questions = let answers = fmap (\q -> q answers) questions
                  in  answers

-- For example, consider spreadsheets, which are 2-dimensional grids
-- rather than 1-dimensional lists.  The data in a spreadsheet can be
-- modeled as a combination of rows and columns: a single sheet
-- contains a list of Rows, and each Row contains a list of values for
-- each Column in the sheet.
type Columns a = [a]
type Rows a = [a]

data Spreadsheet a = Sheet (Rows (Columns a))

-- We can extract all the cells from a Spreadsheet grid by returning
-- the entire contents of the Sheet.
cells :: Spreadsheet a -> Rows (Columns a)
cells (Sheet s) = s

-- We can index into a specific cell of a Spreasheet by looking it up
-- its row and column.
(!) :: Spreadsheet a -> (Int, Int) -> a
Sheet s ! (i,j) = s !! i !! j

-- A nice way to Show Spreadsheets is to display them as a grid of
-- row/column data.
instance (Show a) => Show (Spreadsheet a) where
  show (Sheet s) = unlines [ unwords [ show x | x <- row ] | row <- s]

-- Spreadsheets are Functors just like regular 1-dimensional lists,
-- which means that they have their own version of 'fmap' that applies
-- an operation uniformly to every cell in the grid similar to 'map'
-- that applies an operation to every value in a list.
instance Functor Spreadsheet where
  fmap f (Sheet s) = Sheet [ [ f x | x <- row ] | row <- s ]

-- This is all we need to say in order to be able to 'solve' a
-- Spreadsheet, by giving each cell the ability to access to the
-- answers in the entire Sheet.  As long as we are careful to not have
-- a cycle of dependencies, then 'solve' will automatically figure out
-- the answer to each formula in the Sheet.  For example, here is a
-- Spreadsheet containing a couple columns of data, and computes a
-- total value for each row and average for each column.
calc = Sheet
       [ [ const 10, const 20, \s -> s!(0,0) + s!(0,1) ],
         [ const 33, const 12, \s -> s!(1,0) + s!(1,1) ],
         [ const 29, const 13, \s -> s!(2,0) + s!(2,1) ],
         [ const 16, const 19, \s -> s!(3,0) + s!(3,1) ],
         [ \s -> (s!(0,0) + s!(1,0) + s!(2,0) + s!(3,0)) / 4,
           \s -> (s!(0,1) + s!(1,1) + s!(2,1) + s!(3,1)) / 4,
           \s -> (s!(0,2) + s!(1,2) + s!(2,2) + s!(3,2)) / 4 ] ]
-- Note that the function
--
--     const :: a -> (b -> a)
--     const x = \y -> x
--
-- always returns its first argument.  So 'const 10' is the function
-- that always returns 10, regardless of its next argument.  

