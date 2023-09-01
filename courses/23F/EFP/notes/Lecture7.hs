module Lecture7 where

-- Since we will be defining some things that are already given to us from the
-- Prelude, I'm hiding those definitions by explicitly importing Prelude and
-- saying what definitions I want to hide. That way, there is no confusion when
-- the following code refers to (for example) Maybe: it refers to the definition
-- of Maybe given in this file.
import Prelude hiding
  (id, (.), (<$>), zip3, Maybe(..),
   Monoid(..), Functor(..), Applicative(..))


------------
-- Monoid --
------------

-- Monoid is a type class describing types that can be summed up in some way.

class Monoid a where
  -- Note that here, 'a' is a type, written a :: *

  -- mempty is a neutral "empty" element, with respect to mappend.
  mempty  :: a

  -- mappend combines two elements into one.
  mappend :: a -> a -> a

  -- mconcat combines all the values in the list using mappend and mempty.
  mconcat :: [a] -> a
  mconcat []     = mempty
  mconcat (x:xs) = x `mappend` mconcat xs

-- P.S.: The above definition of Monoid is simplified from the one in the
-- standard library by removing the dependency on Semigroup.

instance Monoid Integer where
  -- Integers can be seen as a Monoid where mempty is 0 and mappend is addition.

  -- mempty :: Integer
  mempty  = 0

  -- mappend :: Integer -> Integer -> Integer
  mappend = (+)

instance Monoid [a] where
  -- Lists can be seen as a Monoid where mempty is the empty list and mappend is
  -- list append.

  -- mempty :: [a]
  mempty  = []

  -- mappend :: [a] -> [a] -> [a]
  mappend = (++)

-------------
-- Functor --
-------------

-- The Functor type class abstracts over the mapping function 'map'. You can
-- think of an instance of Functor as a polymorphic container, where we can
-- transform all values in that container.

class Functor m where
  -- 'm' is a type *function*, written m :: * -> *
  fmap :: (a -> b) -> m a -> m b

-- Note that [] is also the name of the function turning a type into a list
-- containing values of that type. For example, the type [Bool] is the same
-- thing as [] Bool.

instance Functor [] where
  -- fmap :: (a -> b) -> [] a -> [] b
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap f []     = []
  fmap f (x:xs) = f x : fmap f xs
  -- fmap for lists is exactly the usual map function for lists.
  --
  --     fmap = map

double :: Int -> Int
double x = x + x

doubleList :: [Int] -> [Int]
doubleList xs = map double xs

doubleFunctor :: Functor m => m Int -> m Int
doubleFunctor fa = fmap double fa

-- Recall the Maybe type, which is usually imported by default from Prelude.
data Maybe a = Nothing | Just a
  deriving (Show)

-- Unsafe function that tries to return the head of a list, and stops the whole
-- program if the list is empty.
head1 :: [a] -> a
head1 (x:xs) = x

-- Safe function that tries to return the head of a list, and returns Nothing if
-- the list is empty.
head2 :: [a] -> Maybe a
head2 []     = Nothing
head2 (x:xs) = Just x

-- Maybe is a type function, since given a type like Int, Maybe Int is a type.
instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)
  -- fmap for Maybe will modify the contents in case there is Just a value, and
  -- otherwise do nothing if there is Nothing.

doubleMaybe :: Maybe Int -> Maybe Int
doubleMaybe mx = doubleFunctor mx

divide :: Int -> Int -> Maybe Int
divide x 0 = Nothing
divide x y = Just (x `div` y)

-- A value of type 'Writer log a' is a computation/container delivering a value
-- of type 'a', while also writing out some write-only "logging" information of
-- type 'log'.
data Writer log a = Write a log
                  deriving Show

-- Running a 'Writer log a' computation and extracting its result and log is
-- just extracting the contents of the constructor Write.
runWriter :: Writer log a -> (a, log)
runWriter (Write x w) = (x, w)

instance Functor (Writer log) where
  -- fmap :: (a -> b) -> Writer log a -> Writer log b
  fmap f (Write x w) = Write (f x) w
  -- Note that, f :: a -> b, x :: a, w :: log
  --
  -- fmap for Writer applies the given function to the "result" part of the
  -- pair, leaving the "logging" part alone.

noisyAdd :: Int -> Int -> Writer String Int
noisyAdd x y = Write (x+y) ("Added " ++ show x ++ " + " ++ show y ++ ".")

-- A value of type 'Reader env a' is a computation/container that delivers a
-- value of type a, which might depend on reading some information from an
-- environment of type 'env'
data Reader env a = Read (env -> a)

-- Running a 'Reader env a' computation requires that we are given an initial
-- environment to use, which we can pass to the reader to generate the result.
runReader :: Reader env a -> env -> a
runReader (Read f) r = f r

instance Functor (Reader env) where
  -- fmap :: (a -> b) -> Reader env a -> Reader env b
  fmap f (Read g) = Read (\r -> f (g r))
  -- Note that, f :: a -> b, g :: env -> a, r :: env

  -- fmap for Reader composes the given function with the "reader" function,
  -- which takes the environment to generate an 'a' value, which is then
  -- transformed into the final 'b' result.

-----------------
-- Applicative --
-----------------

-- Functors are a useful, but a very simple interface. The Functor type class
-- captures one of the primary operations on "container"-like things, but not
-- everything.  For example, fmap does not let us combine two containers (values
-- of type m a for some Functor m) with a binary operation. Trying to multiply
-- two lists together as
--
--     map (*) [2,3] [4,5]
--
-- doesn't make sense because mapping (*) over the list [2,3] gives a list of
-- functions,
--
--     map (*) [2,3] = [(2*), (3*)]
--
-- which then doesn't "apply" to the following list [4,5].

-- The Applicative type class adds additional functionality on top of
-- Functor. In addition to mapping, every Applicative m has some way of
-- converting any plain 'pure' value into a fancy m-value, but in the "least
-- fancy" way. Furthermore, Applicative m provides an operator (<*>) that lets
-- you apply fancy m-functions to fancy m-values, which lets you chain together
-- several fancy applications in a row.

class Functor m => Applicative m where
  -- pure turns a normal 'a' into a fancy 'm a' in the "least fancy" way
  -- possible.
  pure :: a -> m a

  -- <*> (read as "app") does fancy function application.
  (<*>) :: m (a -> b) -> m a -> m b

-- Note that the standard library gives an operator name to regular function
-- application.
($) :: (a -> b) -> a -> b
f $ x = f x

infixr 0 $
-- The right-associative infix declration, 'infixr 0 $', means that the $
-- operator has the *lowest* possible precedence, and groups to the right.  That
-- way, you can write
--
--     f $ g $ h $ x * x
--
-- and the parser will automatically insert parenthesis to see
--
--     f (g (h (x * x)))

-- By analogy, we can also define an operator (<$>) that is a synonym for the
-- generic Functor fmap.
(<$>) :: Functor m => (a -> b) -> m a -> m b
f <$> x = fmap f x

infixl 4 <*>
infixl 4 <$>

-- Note that for well-behaved instances of Applicative,
--
--     f <$> x  =  pure f <*> x

-- This is the way that you can see Applicative as generalizing Functor. Instead
-- of just fmapping a single unary function, you can fmappily apply functions of
-- any number of arguments. For example,
fmap3 :: Applicative m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
fmap3 f x y z = f <$> x <*> y <*> z

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure x = Just x
  -- A "pure" x (in the sense of Maybe) is Just x.
  
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Just f <*> Just x = Just (f x)
  _      <*> _      = Nothing
  -- Note that f :: (a -> b) and x :: a.

  -- Applying a possible function to a possible argument does normal function
  -- application when both exist. If either (or both) aren't there, then Nothing
  -- can be done.

instance Monoid log => Applicative (Writer log) where
  -- The Applicative instance for Writer requires that log is a Monoid type,
  -- meaning that we have some representation of an "empty" log, and know how to
  -- combine two logs.

  -- pure for Writer contains the result and an empty log (it is the least fancy
  -- 'Writer log a' corresponding to the given value).
  --
  -- pure :: a -> Writer log a
  pure x = Write x mempty

  --  <*> for Writer applies the contained function and input value, and
  -- combines their associated logs.
  --
  -- (<*>) :: Writer log (a -> b) -> Writer log a -> Writer log b
  Write f w1 <*> Write x w2 = Write (f x) (w1 `mappend` w2)
  -- f :: a -> b, x :: a, and w1, w2 :: log

instance Applicative (Reader env) where
  -- The Applicative instance for Reader will thread the same environment across
  -- multiple applications.

  -- pure for Reader will completely ignore the environment and always return
  -- some given constant value (it is the least fancy 'Reader env a'
  -- corresponding to the given value).
  --
  -- pure :: a -> Reader env a
  pure x = Read (\_ -> x)

  -- <*> for Reader applies the contained function (which might depend on the
  -- environment) to the contained argument (which also might depend on the same
  -- environment).
  --
  -- (<*>) :: Reader env (a -> b) -> Reader env a -> Reader env b
  Read f <*> Read g = Read (\r -> f r (g r))

-- A "cartesian product" style of application: all combinations of functions and
-- arguments are combined via application.
cap :: [a -> b] -> [a] -> [b]
cap fs xs = [ f x | f <- fs, x <- xs ]

-- A "zip" style of application: functions and arguments are combined pairwise
-- in order from two lists, truncating the longer of the two in case they are
-- not of the same length.
zap :: [a -> b] -> [a] -> [b]
zap fs xs = [ f x | (f, x) <- zip fs xs ]

instance Applicative [] where
  -- The "normal" Applicative instance for lists is the cartesian-product-like
  -- "all possible combinations" behavior for lists.

  -- pure for [] returns the singleton list containing the given value. This is
  -- the least fancy list corresponding to the value for this notion of
  -- Applicative, since all combinations of one thing is just that one thing.
  --
  -- pure :: a -> [a]
  pure x = [x]

  -- <*> for [] returns all combinations of the given function*s* applied to the
  -- given value*s*.
  --
  -- (<*>) :: [a -> b] -> [a] -> [b]
  (<*>) = cap

-- But there is another way to handle lists! Instead of trying "all possible
-- combinations", you can instead combine lists pointwise, like 'zip' does. To
-- get around the fact that a type class can have at most one instance for any
-- particular type, we can define a *new* type that is exactly the same as
-- lists, but will be treated zippily by the Applicative type class.
data ZipList a = Zip [a]
  deriving Show

getZip :: ZipList a -> [a]
getZip (Zip xs) = xs

instance Functor ZipList where
  -- The Functor instance of ZipList is exactly the same as the normal one.

  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap f (Zip xs) = Zip (map f xs)

instance Applicative ZipList where
  -- But the Applicative instance of ZipList is very different. Instead of a
  -- cartesian product of all possible combinations, elements are only combined
  -- pointwise.

  -- pure for ZipList is an infinite repetition of the given value. This is the
  -- least fancy list corresponding to that value since zipping pointwise will
  -- drop excess elements, truncating down to a number of elements matching the
  -- shortest list. By repeating infinitely, we can be sure that a pure ZipList
  -- will never cause accidental truncation.
  --
  -- pure :: a -> ZipList a
  pure x = Zip (repeat x)

  -- <*> for ZipList applies each function to each value *pointwise* from the
  -- two lists, a la zip. If either of the function list or value list has fewer
  -- elements than the other, then the longer list will be truncated.
  --
  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  Zip fs <*> Zip xs = Zip (zap fs xs)

-- Now, the Applicative instance for ZipLists gives us a flexible and composable
-- interface for zipping together lists in all sorts of ways! Usually, you would
-- have to manually write a different zip function for every number of lists you
-- want to combine. For example:

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 [] _ _ = []
zip3 _ [] _ = []
zip3 _ _ [] = []
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs

-- This is boring and tedious, as you continue to write out the zip functions
-- for combining 4, 5, 6, ... lists together.

-- Instead, we can use the Applicative operations from ZipList to zip as many
-- lists ans we want together. For example, the zip3 function can be written as
-- an application of fmap3 above.
zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' xs ys zs = getZip (fmap3 triple (Zip xs) (Zip ys) (Zip zs))
  where triple x y z = (x, y, z)

-- zipping together four lists is not much different, just requiring another
-- argument.
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 ws xs ys zs = getZip (quad <$> Zip ws <*> Zip xs <*> Zip ys <*> Zip zs)
  where quad w x y z = (w, x, y, z)

-----------------
-- Alternative --
-----------------

-- The Alternative type class is an extension of Applicative.  It adds the
-- functionality to have a neutral "empty" fancy value, as well as a way to
-- combine alternative fancy options of the same type. Notice that this sounds a
-- lot like Monoid, just fancy!  In contrast to the way Monoid works with whole
-- types, like "Int" or "Maybe String" or "[Bool]", Applicative works with type
-- functions, like "Maybe" or the list type constructor "[]", do describe fancy
-- modifiers of other types.

class Applicative m => Alternative m where
  -- The neutral element (with respect to the <|> operator).
  empty :: m a
  -- The binary operator that combines two alternatives.
  (<|>) :: m a -> m a -> m a


instance Alternative [] where
  -- empty :: [a]
  empty = []

  -- (<|>) :: [a] -> [a] -> [a]
  (<|>) = (++)

-- 'Monoid' versus 'Alternative' for lists: notice how the code and types are
-- effectively the same thing for the two instances, but the underlying generic
-- operations have a different type

instance Monoid (Maybe a) where
  -- 'Maybe a' can be seen as a Monoid where mempty is Nothing and mappend will
  -- select the (left-most) non-Nothing value.

  -- mempty :: Maybe a
  mempty = Nothing
  
  -- mappend :: Maybe a -> Maybe a -> Maybe a
  mappend Nothing  y = y
  mappend (Just x) y = Just x

instance Alternative Maybe where
  -- empty :: Maybe a
  empty = Nothing

  -- (<|>) :: Maybe a -> Maybe a -> Maybe a
  Nothing <|> y = y
  Just x  <|> _ = Just x


----------
-- Laws --
----------

-- An instance of a type class has the ability to do anything at all that passes
-- the type checker, but many (if not most) generic type classes expect
-- instances to follow some additional laws. These laws contrain what an
-- instance of the type class might do, which in turn gives you some foothold
-- for reasoning about type class generic code. For example, we intuitively
-- expect that the equality (==) and difference (/=) checks be opposite to one
-- another (two values are equal by == exactly when they are not different by
-- /=) and will use that intuition to reason about code when using those two
-- operators, but it's possible for an instance of Eq to break this
-- assumption. That's why the informal contract of the Eq type class asks that
-- implementors only provide instances Eq a satisfying the laws that, for all
-- values x, y :: a,
--
--     x == x  =  True            (reflexivity)
--     x /= y  =  not (x == y)    (negation)
--
-- A well-behaved instance of Eq satisfies (at least) these two laws, so code
-- which uses the (==) and (/=) operators can assume that these equalities hold.

-- Another generic type class that comes with some informal laws associated with
-- it is the Monoid type class above, which asks that instances follow the
-- corresponding monoid laws for mathematics. A well-behaved instance Monoid a
-- should satisfy the following equalities for all values x, y, z :: a
--
--     mempty `mappend` x  =  x  =  x `mappend` mempty           (identity)
--     (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) (associativity)

-- For example, the Monoid (Maybe a) instance follows the above laws. For the
-- left identity law, you can just simplify the definitions directly.
--
--     mempty `mappend` x
--     = {- definition of mempty -}
--     Nothing `mappend` x
--     = {- definition of mappend -}
--     x
--
-- To show the right identity law is a little more involved, since mappend
-- pattern matches on its left argument, which is some unknown x. You need to
-- consider the cases on what the left argument of mappend might be. For x ::
-- Maybe a, x must either be Nothing or Just y for some y :: a. These two cases
-- of the law are
--
--     x `mappend` empty
--     = {- assumption that x = Nothing -}
--     Nothing `mappend` mempty
--     = {- definition of mappend (for Maybe) -}
--     mempty
--     = {- definition of mempty (for Maybe) -}
--     Nothing
--     = {- assumption that x = Nothing -}
--     x
--
--     x `mappend` empty
--     = {- assumption that x = Just y -}
--     (Just y) `mappend` mempty
--     = {- definition of mappend (for Maybe) -}
--     Just y
--     = {- assumption that x = Just y -}
--     x
--
-- Proving the associativity law for Monoid Maybe follows similar reasoning to
-- the above, by considering the cases when x is Nothing and x is Just
-- something.

-- The same is true for Functor and Applicative. An instance of Functor m is
-- well-behaved if it follows the *functor laws*: for all functions f :: a -> b
-- and g :: b -> c, the following equations hold
--
--     fmap id = id                      (identity)
--     fmap g . fmap f = fmap (g . f)    (composition)
--
-- Inlining the definitions of the identity function (id) and function
-- composition (.), these above laws say that for all values x :: m a
--
--     fmap (\y -> y) x  = x
--     fmap g (fmap f x) = fmap (\y -> g (f y)) x

id :: a -> a
id x = x

(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g (f x)

infixr 9 .

-- For example, why should we use the definition for Functor Maybe that we do
-- instead of the "simpler" one:
--
--     fmap _ _ = Nothing
--
-- This typechecks, and just always returns Nothing no matter the input. Let's
-- check the functor laws. The always-nothing definition of fmap breaks the
-- identity law:
--
--     fmap id (Just x) = Nothing /= Just x
--
-- But the above instance of Functor Maybe, which applies the given function to
-- the contexts of a Just, does obey both laws. When fmap is applied to Nothing,
-- then we have
--
--     fmap id Nothing = Nothing
--     (fmap g . fmap f) Nothing = Nothing = fmap (g . f) Nothing
--
-- And the more interesting case when fmap is applied to Just x gives
--
--     fmap id (Just x) = Just (id x) = Just x
--
--     (fmap g . fmap f) (Just x)
--     =
--     fmap g (fmap f (Just x))
--     =
--     fmap g (Just (f x))
--     =
--     Just (g (f x))
--     =
--     fmap (\x -> g (f x)) (Just x)
--     =
--     fmap (g . f) (Just x)

-- The informal contract of the Applicative type class also expects instances to
-- follow certain laws to be well behaved. An instance of Applicative m is
-- well-behaved when, for all f :: a -> b, x :: a, v :: m a, u :: m (a -> b),
-- and w :: m (b -> c), the following equalities hold:
--
--     pure id <*> v = v                               (identity)
--     pure f <*> pure x = pure (f x)                  (homomorphism)
--     u <*> pure x = pure ($ x) <*> u                 (interchange)
--     pure (.) <*> w <*> u <*> v = w <*> (u <*> v)    (composition)
