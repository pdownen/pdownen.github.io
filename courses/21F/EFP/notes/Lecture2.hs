module Lecture2 where

--------------------
--- Polymorphism ---
--------------------

-- Basic polymorphic functions
identity :: a -> a
identity x = x

first :: (a, b) -> a
first (x, y) = x

second :: (a, b) -> b
second (x, y) = y

-- Polymorphism and recursion

-- Polymorphic map function: mapList f xs transforms every element of xs by f.
mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs
-- named 'map' in Prelude

-- Alternative definition of map using a list comprehension
mapList' f xs = [ f x | x <- xs ]

-- Polymorphic filter function: filterList f xs keeps only those elements of xs
-- for which f returns true.
filterList :: (a -> Bool) -> [a] -> [a]
filterList f [] = []
filterList f (x:xs)
  | f x = x : filterList f xs
  | otherwise = filterList f xs
-- named 'filter' in Prelude

-- Alternative definition of filter using a list comprehension with a
-- side-condition f x
filterList' f xs = [ x | x <- xs, f x ]

-- The cartesian product of two lists
--
--     cartesianProduct [1,2,3] ['a', 'b']
--     =
--     [(1,'a'), (1, 'b'), (2, 'a'), (2, 'b'), (3, 'a'), (3, 'b')]
--
-- can be defined with a list comprehension by drawing from both lists at the
-- same time.
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [ (x, y) | x <- xs, y <- ys ]
-- Note that the first list being drawn from (xs) is walked down first, and then
-- the second list being drawn from (ys) is secondary.


------------------
--- Data Types ---
------------------

-- An enumeration data type
data ABC = A | B | C
  deriving (Ord, Enum, Bounded)
-- A data type can have an associated 'deriving' clause to specify that
-- instances for some type classes should be automatically generated.  Here, an
-- instance of Ord ABC, Enum ABC, and Bounded ABC are automatically generated.

-- Type names (like ABC) and constructor names (like A, B, and C) *must* start
-- with a capital letter. Non-constructor names (like the function isA or a
-- constant) *must not* start with a capital letter. Likewise type variables,
-- like the variables 'a' and 'b' in the types above, *must not* start with a
-- capital letter.

-- Once a data type has been defined, we can pattern match on its constructors.
isA :: ABC -> Bool
isA A = True
isA _ = False

-- An enumeration with something more

-- A Shape is either a Circle (along with a floating-point radius) or a
-- Rectangle (along with floating-point length and width).x
data Shape = Circle    Float       -- radius
           | Rectangle Float Float -- length and width
  deriving (Show, Read, Eq)

-- Some operations on Shapes.

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle l w) = 2 * (l + w)

area :: Shape -> Float
area (Circle r) = pi * (r^2)
area (Rectangle l w) = l * w

-- Data types can be polymorphic, taking a type variable as a parameter. One
-- example of a polymorphic data type is the list type, which can be used to have
-- lists of Strings, lists of Integers, etc.

-- An Annotated a value combines an 'a' value with some string annotation.
data Annotated a = Note String a
-- The constructor Note has the type String -> a -> Annotated a

-- Given an Annotated a, you can pull out both the String annotation as well as
-- the 'a' value.
getNote :: Annotated a -> String
getNote (Note s a) = s

getValue :: Annotated a -> a
getValue (Note s a) = a

-- Data types can be recursive as well.

-- An equivalent definition of the list type, without the special syntactic
-- sugar.
data List a = Nil              -- []
            | Cons a (List a)  -- x : xs
  deriving (Show, Eq, Ord)

-- You can convert back and forth between [a] and List a.
toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

-- More general trees, like binary trees, can also be defined as recursive data
-- types.
data Tree a = Leaf | Branch (Tree a) a (Tree a)
  deriving (Show, Eq, Ord)

-- A left-to-right depth-first search converts a Tree a into a list of the
-- elements in its branches.
depthFirstSearch :: Tree a -> [a]
depthFirstSearch Leaf = []
depthFirstSearch (Branch left x right) =
  depthFirstSearch left -- [a]
  ++
  [x]
  ++
  depthFirstSearch right -- [a]

-- Type synonyms

-- String is a type synonym
--     type String = [Char]

-- Unlike data types, type synonyms do not define new types, they define aliases
-- that are indistinguishable with the original type.

-- Like data types, type synonyms can be parameterized by type variables.

-- One simple way of representing a mapping from Ints to values a is with a list
-- of pairs (Int, a) describing the key-value relationship.
type IntMap a = [(Int, a)]

-- Because an IntMap a *is* just a list, we can define functions on IntMaps by
-- defining treating them like lists, using both constructors and pattern
-- matching.
insertIntMap :: Int -> a -> IntMap a -> IntMap a
insertIntMap i x m = (i, x) : m

findIntMap :: Int -> IntMap a -> a
findIntMap i []  = error "findIntMap: key not found"
findIntMap i ((j,x) : m)
  | i == j       = x
  | otherwise    = findIntMap i m


--------------------
--- Type Classes ---
--------------------

-- double requires the Num a constraint because of the use of the addition (+)
-- operator on x of type a.
double :: Num a => a -> a
double x = x + x

-- prod requires the Num a constraint because of the use of the multiplication
-- (*) operator on the elements of the list [a].
prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs

-- easySort requires an Ord a constraint because it compares elements of the
-- list [a] with the ordering operators (<) and (>=).
easySort :: Ord a => [a] -> [a]
easySort [] = []
easySort (x:xs) = easySort [ y | y <- xs, y < x ]
                  ++
                  [x]
                  ++
                  easySort [ z | z <- xs, z >= x ]

-- Maps can be generalized to any key type i like so
type Map i a = [(i, a)]
-- Note that IntMap a is the same type as Map Int a which in turn is the same
-- type as [(Int, a)].

-- The generalized insert function is exactly the same as the insertion function
-- for IntMaps, but with a more general type.
insert :: i -> a -> Map i a -> Map i a
insert i x m = (i, x) : m

-- Note that the generalized find function requires the Eq i constraint, because
-- it needs to test equality of the keys of type i.
find :: Eq i => i -> Map i a -> a
find i []     = error "find: key not found"
find i ((j,x) : m)
  | i == j    = x
  | otherwise = find i m

-- New instances of type classes can be defined by hand. For instance, the Show
-- type class can be given an instance for the ABC type like so
instance Show ABC where
  show A = "A" -- This does not have to be "A", it can be "Anything you want"
  show B = "B"
  show C = "C"
-- Since the 'show' function is used to print values in the ghci interpeter,
-- this instance allows for values of ABC to be printed like Integer, Boolean,
-- etc. values.

-- An Eq ABC instance, which explains how to test equality of ABC values, can
-- also be defined with pattern matching.
instance Eq ABC where
  A == A = True
  B == B = True
  C == C = True
  _ == _ = False

-- Some other type class instances can be very tedious to write by hand.
{-
instance Ord ABC where
  compare A A = Eq
  compare A B = LT
  ...
-}
-- Instead, you can automatically derive a sensible instance using the
-- 'deriving' keyword as done above. Some type classes supported by 'deriving'
-- are: Show, Read, Eq, Ord, Enum, Bounded. Show a and Read a converts between
-- values of 'a' and Strings. Eq and Ord provide equality and ordering
-- comparison functions. Enum is used in list ranges like [1..10]. Bounded
-- defines the minimum and maximum bound on finite types (those types with a set
-- number of possible values) like Int.

-- Type class instances can assume other instances.
instance Eq a => Eq (Annotated a) where
  (Note s x) == (Note t y) = x == y

instance Ord a => Ord (Annotated a) where
  compare (Note _ x) (Note _ y) = compare x y

-- Type classes allow for "polymorphic values"
everything :: (Enum a, Bounded a) => [a]
everything = [ minBound .. maxBound ]
-- The polymorphic values
--
--     minBound :: Bounded a => a
--     maxBound :: Bounded a => a
--
-- give the smallest and largest 'a' values, which depends on the type 'a'.
-- That means that 'everything' is the list of all values of a type 'a', as long
-- as that type is enumerable (Enum a) and bounded (Bounded a).

-- You can define your own type class.

-- Combination describes a type 'a' with values that can be 'combine'd together,
-- along with an 'empty' element with respect to combination.
class Combination a where
  empty   :: a
  combine :: a -> a -> a

  -- In addition to the two main methods above of Combination a, it also makes
  -- sense to take the total of a list by combining all the elements of the list
  -- together.
  total   :: [a] -> a
  -- Since it's always safe to define a generic implementation of 'total' in
  -- terms of 'empty' and 'combine', that implementation can be given inside the
  -- definition of the type class. This default definition of total will be used
  -- in any instance of Combination that does not give its own definition of
  -- 'total'.
  total []     = empty
  total (x:xs) = x & total xs
-- This type class is known as 'Monoid' in the Prelude

-- You can define your own infix operators by coming up with any combination of
-- symbol characters (like &, *, +, etc).  For example, the 'combine' method of
-- Combination can be given the infix shorthand & like so
(&) :: Combination a => a -> a -> a
x & y = combine x y

-- A Combination instance should obey the following equalities:
--
--     empty & x   = x
--     x & empty   = empty
--     x & (y & z) = (x & y) & z
--
-- With this in mind, there are many possible instances of Combination on the
-- types we've seen so far.
instance Combination Bool where
  empty   = False
  combine = (||)

instance Combination Integer where
  empty   = 0
  combine = (+)

instance Combination [a] where
  empty   = []
  combine = (++)

-- The combination of a pair (a, b) can be defined for any types 'a' and 'b'
-- that already have Combination instances.
instance (Combination a, Combination b) => Combination (a, b) where
  empty                 = (empty, empty)
  combine (a, x) (b, y) = (combine a b, combine x y)

-- You can give your own definition to a defaulted method.  This can be more
-- efficient than the default definition. For example, the empty tuple is ()
-- which belongs to the type (). In the instance of Combination (), there is
-- only one option for each of the methods, since there is only one value of
-- type (). That means that 'total' can return a result without even looking at
-- the list it is given.
instance Combination () where
  empty         = ()
  combine () () = ()
  total   _     = ()


--------------------------
--- A Note About Magic ---
--------------------------

-- The equality operator (==) can only be used on two arguments of *exactly* the
-- same type. And yet, somehow the following is True even though 4 is an Integer
-- and 4.0 is a Float.
thisIsTrue = 4 == 4.0

-- What is happening? Are numbers automatically coerced at run-time, so that
-- Integers are converted to Floats as needed? Not quite. Haskell will never
-- automatically coerce different number types at run-time.  Once the type of
-- something is set, then that is it.  For example, if we give names to both
-- fours
four :: Integer
four = 4

fourPoint0 :: Float
fourPoint0 = 4.0
-- Then the equality comparison no longer compiles
-- thisIsBogus = four == fourPoint0    -- Gives a type error

-- What's happening is that numeric literals in Haskell have a little
-- context-sensitive magic to make using them a little easier. When Haskell sees
-- the number literal '4', it interprets it as 'fromInteger 4', where the
-- 'fromInteger' function comes from the Num type class:
--
--     fromInteger :: Num a => Integer -> a
--
-- Notice that fromInteger is a type class function with a fixed input type
-- (Integer) but a generic *output* type 'a'. The result is that when you see
-- the expression 'fromInteger 4', then its type will depend on its context,
-- providing any type of number that is needed.

-- Similar, numeric literals with decimal points use the same sort of magic to
-- make them context-sensitive. The number literal '4.0' is interpreted as
-- 'fromRational 4.0', where the 'fromRational function somes from the
-- Fractional type class:
--
--     fromRational :: Fractional a => Rational -> a
--
-- The Rational type represents exact, arbitrary precision, fractions. They are
-- slower than using Float or Double floating point numbers, but do not have the
-- same issues with rounding and approximation.
