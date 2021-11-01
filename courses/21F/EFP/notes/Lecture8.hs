{-# LANGUAGE RebindableSyntax #-}
-- The above comment is called a "language pragma", and signals that we're using
-- an extension to the base Haskell programming language. The RebindableSyntax
-- lets us use our own definitions like Monad used for desugaring syntactic
-- sugar, and replace the one that's given in the Prelude. The definitions we
-- use here are all the same as the ones given in the Prelude, so this is just
-- for illustrative purposes, but it is possible to use rebindable syntax for
-- more exotic things.
module Lecture8 where

import Lecture7
import Prelude hiding
  (id, (.), (<$>),
   sequence, forM,
   Maybe(..), Monoid(..),
   Functor(..), Applicative(..), Monad(..))
import qualified Prelude

-----------------------
-- Fancy Application --
-----------------------

-- A loose approximation of a deck of cards, from the first two programming
-- assignments.
type Card = Int
type Deck = [Card]
type Hand = [Card]

-- A full deck of cards, approximately.
fullDeck :: Deck
fullDeck = [1..52]

-- Drawing a single card (forget about what to do when the deck is empty, for
-- now).
draw :: Deck -> (Card, Deck)
draw (c:cs) = (c, cs)

-- Hitting a hand by moving a card from the deck to the hand.
hit :: Hand -> Deck -> (Hand, Deck)
hit h (c:cs) = (c:h, cs)

-- Deal a hand of two cards.
{-
deal2 d = hit (hit [] d) d
-}
-- This doesn't work, because hit returns the remaining deck in addition to the
-- drawn card. More importantly, the current state of the deck is *duplicated*,
-- which it should only be used once (on the first call to hit). The second call
-- to hit should use the updated deck with one card removed.

deal2 :: Deck -> (Hand, Deck)
deal2 d = let (c1, d1) = draw d
              (c2, d2) = draw d1
          in  ([c1, c2], d2)
-- This works, but it can get pretty tedious and doesn't scale very well.

deal3 :: Deck -> (Hand, Deck)
deal3 d = let (c1, d1) = draw d
              (c2, d2) = draw d1
              (c3, d3) = draw d2
          in  ([c1,c2,c3], d3)
-- A correct by tedious definition. Threading the deck around manually can be
-- error prone, since the requirement that the deck is used *only once* can be
-- easily broken by confusing two variable names. Nothing in the programming
-- language is helping you keep track of which deck should be used on each line.

-- Plain old function application passes some input to the function without
-- doing anything fancy.
apply :: (a -> b) -> a -> b
apply f x = f x

-- A type of *actions* that depend on the state of the "ambient" deck which
-- changes over time. Each DeckState a action needs to be given the current
-- state of the deck, and then returns some value 'a' along with an updated
-- state of the deck.
type DeckState a = Deck -> (a, Deck)

-- Both the draw and hit functions above can be seen in terms of DeckState.

-- draw is a DeckState action that returns a Card (while manipulating the value
-- of the ambient Deck).
draw' :: DeckState Card
draw' = draw

-- hit is a function from a Hand to a DeckState Hand, which returns a larger
-- Hand by taking one card off the top of the ambient Deck.
hit' :: Hand -> DeckState Hand
hit' = hit

-- A definition of "stateful" function application, when both the argument and
-- the function might depend on (and modify) the ambient state of the deck.
applyState
  :: (a -> DeckState b) -- deliver a 'b' given 'a' and a "stateful" deck
  -> DeckState a        -- deliver an 'a' given a "stateful" deck
  -> DeckState b        -- deliver a 'b' given a "state" 'd'
applyState f x = \d -> let (y, d')  = x d
                           (z, d'') = f y d'
                       in  (z, d'')
-- f :: a -> Deck -> (b, Deck)
-- x :: Deck -> (a, Deck)
-- d, d', d'' :: Deck
-- y :: a
-- z :: b

-- Again, swapping the order of the arguments to applyState makes for a useful
-- "glue" for chaining together stateful operations.
bindState :: DeckState a        -- deliver an 'a' given a "stateful" deck
          -> (a -> DeckState b) -- deliver a 'b' given 'a' and a "stateful" deck
          -> DeckState b        -- deliver a 'b' given a "state" 'd'
bindState x f = applyState f x

-- Convert a plain 'a' into a 'DeckState a' computation which does not depend on
-- or change the given deck.
stateless :: a -> DeckState a
stateless x = \d -> (x, d)

-- Dealing 2 and 3 cards, written as a chain of stateful operations.
deal2' :: DeckState Hand
deal2' = draw' `bindState` \c1 ->
         draw' `bindState` \c2 ->
         stateless [c1, c2]

deal3' :: DeckState Hand
deal3' = draw' `bindState` \c1 ->
         draw' `bindState` \c2 ->
         draw' `bindState` \c3 ->
         stateless [c1, c2, c3]

------------
-- Monads --
------------

-- The Monad type class: a general interface for combining operations with
-- side-effects.
class Applicative m => Monad m where
  -- Turn a regular 'a' into a fancy 'm a'.
  return :: a -> m a

  -- Chain together two m-computations, running the first one for its effect
  -- only and ignoring its result, then running the second one and returning its
  -- result.
  (>>)   :: m a -> m b -> m b

  -- Run the 'm a' computation, bind the returned result 'a' to the input of the
  -- function, and then run and return the function's result.
  (>>=)  :: m a -> (a -> m b) -> m b

  -- (>>) can be defined in terms of (>>=) with a function that ignores its
  -- input. 'return' is a synonym for 'pure' from the Applicative type class.
  m1 >> m2 = m1 >>= \ _ -> m2

  -- 'return' is a synonym for 'pure'.
  return   = pure

-- ALL do expressions can be (and in fact, are) desugared (i.e., rewritten) into
-- the above operations from the Monad type class. The basic rewritings are
--
--     do e                  =  e
--     do { x <- e; e' }     =  e >>= \x -> e'
--     do { e; e' }          =  e >> e'
--     do { let x = e; e' }  =  let x = e in e'
--     do { st; st'... }     =  do { st; do { st'... } }
--
-- where "e" and "e'" are generic expressions, "st" is a generic 'do' statement
-- (an expression, a "<-" bind, or a "let"), and "st'..." is one ore more 'do'
-- statements.

-- For example, the prompt and greet actions from Lecture 3 can be rewritten
-- using only Monad operations as follows:
prompt :: String -> IO String
{-
prompt question = do
  putStrLn question
  getLine
-}
prompt question = putStrLn question >> getLine

greet :: IO ()
{-
greet = do
  name <- prompt "Who is this"
  let greeting = "Hello, " ++ name ++ "!"
  putStrLn greeting
-}
greet = prompt "Who is this?" >>= \name ->
        let greeting = "Hello, " ++ name ++ "!"
        in putStrLn greeting
-- The do expressions in the comment are translated into the shown uses of the
-- Monad operations.

-- This use of >>=, >>, and return for IO requires that we provide Functor,
-- Applicative, and Monad instances of IO (which are defined in the Prelude).
instance Functor IO where
  fmap = Prelude.fmap

instance Applicative IO where
  pure  = Prelude.pure
  (<*>) = (Prelude.<*>)

instance Monad IO where
  (>>=)  = (Prelude.>>=)


-- You can write generic functions over arbitrary Monads, like we can write
-- generic functions over arbitrary Functors and Applicatives.

-- sequence takes a list of m-actions and runs each one in sequence (in order),
-- collecting their results and returning each of them in a list.
sequence :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (m:ms) = m >>= \x ->
                  sequence ms >>= \xs ->
                  return (x:xs)
-- Note that the above is exactly the same as the following do expression
{-
sequence (m:ms) = do x  <- m
                     xs <- sequence ms
                     return (x:xs)
-}

-- forM takes a list of values and a function that returns an m-action for each
-- value, then calls the given function on each value in order and runs the
-- action, returning a list of the returned result for each.
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM xs f = sequence [ f x | x <- xs ]

-- The "fish" operators (>=> and <=<) is the fancy version of plain old function
-- composition (.) that works for "monad operations" returning a result of the
-- type m a for some Monad m.
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = g >=> f

infixr 1 >=>
infixr 1 <=<


-- We can now explain the behavior of 'do' expressions for failable (Maybe) and
-- non-deterministic (list) computations with the corresponding instances of
-- Maybe. At the end of the day there is no magic: everything is spelled out by
-- the return and bind (>>=) operations as plain old functional code. These
-- definitions are implicitly imported from the Prelude.
instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Just x  >>= f = f x
  Nothing >>= f = Nothing

instance Monad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  []     >>= f = []
  (x:xs) >>= f = f x ++ (xs >>= f)
  -- xs >>= f = concat (map f xs)

-----------
-- State --
-----------

-- A stateful operation, with a single readable/writable state value of type
-- 's', can be simulated by a function from s (the initial state) to a return
-- value (of some type 'a') and a new s (the updated state).
newtype State s a = State (s -> (a, s))

-- To run a stateful operation, we need to know what initial state to use, and
-- we get out the final state (after the operation has run) in addition to the
-- value returned by the operation.
runState :: State s a -> s -> (a, s)
runState (State g) s = g s

-- We can define some simple, primitive operations like get, put, and modify, in
-- terms of the simulation of State as a pure function.

-- get returns the current state, and doesn't change it.
get :: State s s
get = State (\s -> (s, s))

-- put overwrites the current state, and returns a boring value ().
put :: s -> State s ()
put s = State (\_ -> ((), s))

-- modify applies a function to the current state to update, and returns the new
-- state as its result.
modify :: (s -> s) -> State s s
modify f = State (\s -> let s' = f s in (s', s'))
-- Note that
--
--     modify f = do s <- get
--                   let s' = f s
--                   put s'
--                   return s'

-- In order to put together stateful operations, we can use the Functor,
-- Applicative, and especially Monad interfaces. So we need to define how those
-- composition operations work for State.
instance Functor (State s) where
  -- fmap for State modifies the return value, but does not change the stateful
  -- value.
  fmap f (State g) = State (\s -> let (x, s') = g s
                                  in  (f x, s'))

instance Applicative (State s) where
  -- pure returns the given value without touching the state (either reading or
  -- writing).

  -- pure :: a -> State s a
  pure x = State (\s -> (x, s))

  -- <*> runs its left side first and the right side second, threading the
  -- initial state between them, to get a function and an argument for that
  -- function. The return value is the function applied to the argument, and the
  -- final state is the state after the right side is finished running.

  -- (<*>) :: State s (a -> b) -> State s a -> State s b
  State h <*> State g = State (\s -> let (f, s')  = h s
                                         (x, s'') = g s'
                                     in  (f x, s''))
  -- Note that h :: s -> (a -> b), g :: s -> a, f :: a -> b, and x :: a.

instance Monad (State s) where
  -- >>= is more general than <*>, but still threads the state from left to
  -- right. First the given stateful computation is run to get a value and an
  -- updated state. Then the given operation is passed both the value from the
  -- left side of >>= to get another stateful operation which is run with the
  -- updated state, giving both a return value and the next updated state, which
  -- is the final outcome.
  State h >>= f = State (\s -> let (x, s')  = h s
                                   State g  = f x
                                   (y, s'') = g s'
                               in  (y, s''))

-- An example use of State, using the Monad interface for State to stitch
-- together the basic get and put actions. Note how the type of the returned
-- result (the triple (Integer, Integer, Integer)) is different from the type of
-- the stateful value (a single Integer).
example :: State Integer (Integer, Integer, Integer)
example = do
  put 1
  x <- get
  put 2
  y <- get
  put 3
  z <- get
  return (x,y,z)

-- You don't need to use do notation. You can call the Monad operations by hand
-- yourself. The above example is exactly the same as the following one written
-- in terms of >>= and >>.
example' :: State Integer (Integer, Integer, Integer)
example' =
  put 1 >>
  get >>= \x ->
  put 2 >>
  get >>= \y ->
  put 3 >>
  get >>= \z ->
  return (x,y,z)

----------
-- Laws --
----------

-- Just like Monoid, Functor, and Applicative, Monad expects instances to follow
-- some laws as well. These laws ensure that, for instance, 'do' expressions
-- will always follow some predictable patterns, even if the underlying
-- implementation and effect may be very different. Written in terms of 'do'
-- notation, the monad laws are:
--
--     do { y <- return x; f y }  =  do { f x }
--
--     do { x <- m; return x }    =  do { m }
--
--     do { y <- do { x <- m;
--                    f x
--                  };
--          g y
--        }
--     =
--     do { x <- m;
--          y <- f x;
--          g y }
--
-- Written in terms of the Monad type class operations (return and >>=), these
-- same laws are (in the same order):
--
--     return x >>= f   =  f x
--     m >>= return     =  m
--     (m >>= f) >>= g  =  m >>= (\x -> f x >>= g)
--
-- Changing perspective again, these monad laws can also be written in terms of
-- the composition operation (>=>) instead of the binding operation (>>=), which
-- gives a much more symmetric view of the same three laws (in the same order):
--
--     return >=> f     =  f                  (left identity)
--     g >=> return     =  g                  (right identity)
--     (f >=> g) >=> h  =  f >=> (g >=> h)    (associativity)
--
-- In other words, these laws follow the same pattern of the Monoid laws (two
-- identity laws and one associativity law), except that the types of return, f,
-- g, and h are more advanced.

-- Now, let's prove that one of the Monad instances above follow these laws. For
-- example, take the instance for Maybe. For left identity, it is just a matter
-- of inlining the definitions of return and >>= from the Monad Maybe instance:
--
--     return x >>= f
--     = {- inlining definition of return/pure (for Maybe) -}
--     Just x >>= f
--     = {- inlining definition of >>= (for Maybe) -}
--     f x
--
-- The right identity law is a little tricker, since we have to work with some
-- unknown m :: Maybe a, and the first step is to pattern-match on m by the >>=
-- operator. But we know how to handle this: consider the cases of what m might
-- be (that is, "induction" for Maybe)! If m is Nothing, then
--
--     m >>= return
--     = {- assumption that m = Nothing -}
--     Nothing >>= return
--     = {- inlining definition of >>= (for Maybe) -}
--     Nothing
--     = {- assumption that m = Nothing -}
--     m
--
-- Otherwise, if m is Just x for some x, then
--
--     m >>= return
--     = {- assumption that m = Just x -}
--     Just x >>= return
--     = {- definition of >>= (for Maybe) -}
--     return x
--     = {- definition of return (for Maybe) -}
--     Just x
--     = {- assumption that m = Just x -}
--
-- The associativity law follows from similar reasoning by cases on what m might
-- be (either Nothing or Just something).
--
--     m >>= (\x -> f x >>= g)
--     = {- assumption that m = Nothing -}
--     Nothing >>= (\x -> f x >>= g)
--     = {- definition of >>= (for Maybe) -}
--     Nothing
--     = {- definition of >>= (for Maybe) -}
--     Nothing >>= g
--     = {- definition of >>= (for Maybe) -}
--     (Nothing >>= f) >>= g
--     = {- assumption that m = Nothing -}
--     (m >>= f) >>= g
--
--     m >>= (\x -> f x >>= g)
--     = {- assumption that m = Just y -}
--     Just y >>= (\x -> f x >>= g)
--     = {- definition of >>= (for Maybe) -}
--     (\x -> f x >>= g) y
--     = {- "beta" reducing the application of a lambda function -}
--     f y >>= g
--     = {- definition of >>= (for Maybe) -}
--     (Just y >>= f) >>= g
--     = {- assumption that m = Just y -}
--     (m >>= f) >>= g
