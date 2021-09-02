-- Module declaration
-- We are writing the 'Lecture1' module 
module Lecture1 where

-- Module import.  We will need the following functions
--
--     ord     :: Char -> Int
--     chr     :: Int  -> Char
--     isUpper :: Char -> Bool
--     isLower :: Char -> Bool
--     isAlpha :: Char -> Bool
--
-- from the Data.Char module.
import Data.Char


---------------
--- Numbers ---
---------------

-- Constant values can be written with an equal sign, with the name to the left
-- and the expression to the right.
three :: Integer
three = 1 + 2

-- Integers are arbitrary sized (they can be very big, only limited by the
-- memory available).  In contrast, Ints are range-limited machine integers,
-- either 32 or 64 bits.
big :: Integer
big = 12 ^ 300

-- Functions can be defined by listing the parameters to the left of the equal
-- sign.
increment :: Integer -> Integer
increment x = let y = x+1
              in y
-- Local variables can be bound by a 'let' expression.  A 'let' binding is
-- recursive, where the bound variable on the left-hand side of the equal sign
-- is seen on the right-hand side as well, so be careful!


----------------
--- Booleans ---
----------------

-- The two Bool values are True and False.

-- not :: Bool -> Bool is the negation function on Bools.  && and || are the
-- 'and' and 'or' operators.

-- The contradiction function always returns False.
--
--     contradiction x = False
contradiction :: Bool -> Bool
contradiction x = x && not x

-- The excludedMiddle function always returns True.
--
--     excludedMiddle x = True
excludedMiddle x = x || not x

-- Haskell has type inference.  We did not need to write down the type of
-- excludedMiddle because GHC figured it out for us.


-------------
--- Lists ---
-------------

-- A list can be constructed by listing out the elements between square
-- brackets.
oneTwoThree :: [Integer]
oneTwoThree = [1, 2, 3]

everyBool :: [Bool]
everyBool = [True, False]

-- The empty list contains no elements.
empty :: [a]
empty = []

-- Another element can be added onto the front of the list with the cons (:)
-- operator
alsoZero = 0 : oneTwoThree

-- Two lists can be appended together with the (++) operator
upToFiveAsWell = alsoZero ++ [4,5]

-------------------
--- List Ranges ---
-------------------

-- 'upto n' generates the list of all numbers from 0 to n.
upto n = [0..n]

-- 'uptoby n inc' generates the list of all number from 0 to n skipping by inc
-- increments.
uptoby n inc = [0, inc .. n]

-----------------
--- Recursion ---
-----------------

-- All looping in Haskell is done by recursion.  A recursive definition refers
-- to the name of the thing being defined (to the left of =) on the right of the
-- equals sign.

-- The nth factorial is the product of numbers from 1 to n.  The function for
-- returning the nth factorial can be defined by recursion on the given number.
fact :: Integer -> Integer
fact 0 = 1
fact n = n * (fact (n-1))
-- The definition of fact uses pattern matching to test if its input is 0 or
-- some other number.  'fact 0' matches the first line, and is equal to 1.
-- Otherwise, 'fact n' of any other n is equal to the second line.

-- Factorial can alternatively be defined as the product of all numbers from 1
-- to n.  The numbers can be generated with a list range, and the product
-- function is provided by the default Haskell Prelude module.
factorial n = product [1..n]

-- The definition of the product of a list can be defined recursively by pattern
-- matching on the given list.  The two cases for a list is an empty list
-- (written []) or a list with the first element x and remaining elements xs
-- (written x:xs).
_product :: [Integer] -> Integer
_product []     = 0
_product (x:xs) = x * _product xs
-- Here, I use the name _product to avoid clashing with the similar function
-- named product that is implicitly imported from the Prelude.


----------------------
--- Simple ciphers ---
----------------------

-- A very simple form of cipher is the Caesar cipher.  The idea behind the
-- Caesar cipher is to replace every letter with the letter n steps later in the
-- alphabet, where n is the key to the cipher.  If the end of the alphabet is
-- reached, then the cipher wraps back around to the beginning again.  For
-- example
--
--     caesar 3 "c" = "f"
--     caesar 1 "z" = "a"

-- Rotate a letter c n positions down the alphabet, which is all characters
-- between 'start' and 'end'. The 'ord' function converts a Char to an Int, and
-- 'chr' converts an Int back to a Char.
rotateRange :: Char -> Char -> Int -> Char -> Char
rotateRange start end n c = c'
  where range    = ord end - ord start
        position = ord c - ord start
        rotated  = (position + n) `mod` range
        c'       = chr (rotated + ord start)

-- Rotate a lowercase letter found in the alphabet between 'a' and 'z'
rotateLower n c = rotateRange 'a' 'z' n c

-- Rotate an uppercase letter found in the alphabet between 'A' and 'Z'
rotateUpper n c = rotateRange 'A' 'Z' n c

-- Decisions can be made with guards written with vertical bars (|).  Like
-- pattern matching, guards are tried top-down until a successful case is found.
-- The expression between the vertical bar and equal sign (the "guard
-- expression") should return a Bool; the right-hand side of the equal sign is
-- returned only when the guard expression returns True.

-- rotateLetter tests whether the given character is uppercase or lowercase, and
-- calls the appropriate function.  If c is not a letter, then it is returned
-- without modification.
rotateLetter :: Int -> Char -> Char
rotateLetter n c
  | isLower c = rotateLower n c
  | isUpper c = rotateUpper n c
  | otherwise = c
-- 'otherwise' is just an ordinary constant bound to True.  So an 'otherwise'
-- guard always succeeds.

-- In Haskell, a String is just a list of characters.  The empty string "" is
-- exactly the same as the empty list [], and the string "abc" is the same as
-- the list ['a', 'b', 'c'].

-- The full Caesar cipher applies the rotation to every letter in a String.
-- This can be defined by recursion on the given String (that is to say, the
-- given [Char]).
caesar :: Int -> String -> String
caesar n ""   = ""
caesar n (c:str)
  | isAlpha c = rotateLetter n c : caesar n str
  | otherwise = caesar n str
-- Note that in the above, a guard is used to toss out non-letter characters,
-- since we did not define how to rotate them. For example
--
--     caesar 2 "so1me37thi0ng8" = "uqogvjkpi"
--
-- where the numbers in the string have been discarded.

-- Instead of recursion, we could also define the cipher by a list
-- comprehension. A list comprehension builds a list by drawing elements from
-- another list, and then evaluating an expression depending on each element in
-- turn.

-- The reverse of the Caesar cipher rotates in the other direction
--
--     uncaesar n str = caesar (-n) str
--     uncaesar n (caesar n str) = str
uncaesar :: Int -> String -> String
uncaesar n str = [ rotateLetter (-n) c | c <- str, isAlpha c ]
-- In the above, the 'c <- str' draws each character out of the String str one
-- at a time, in order. For each character c, the expression
--
--     rotateLetter (-n) c
--
-- becomes the element in the new list. Additionally, the condition 'isAlpha c'
-- is checked; if it evaluates to False, then that element is skipped entirely.

-- A famous use of the Caesar cipher is the rot13 program, which uses 13 as a
-- key. The reason that 13 is chosen is because it is its own reverse (it is
-- halfway through the alphabet of 26 characters, so rot13 is the same forwards
-- and backwards.
--
--     rot13 (rot13 str) = str
rot13 :: String -> String
rot13 = caesar 13
-- Note that the above function definition uses partial application. By
-- supplying the first element to caesar (the key) but not the second (the
-- message to encode), we get a specialized function that always rotates all
-- letters in the string by 13.
