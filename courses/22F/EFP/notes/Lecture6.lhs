Equational Reasoning and Inductive Proofs for Functional Programs
=================================================================

Note that these notes are a runnable Haskell file. You can test out and measure
the timing difference of the example functions given below.  The lines beginning
with a '>' are interpreted as code, whereas *everything* else is a comment.  For
example, this is a comment, but the following line defines the module we are in:

> module Lecture6 where

This style of putting comments first as the default, and having code be
secondary is known as "literate programming," hence the ".lhs" file extension
signifying a "Literate HaSkell" program.


Exponentials
============

Here's an easy definition of the exponential function.

> easyExp :: Integer -> Integer -> Integer
> x `easyExp` 0 = 1
> x `easyExp` n = x * (x `easyExp` (n-1))

Although this example code is rather straightforward, we can use some equational
reasoning to prove that easyExp calculates the "true" mathematical exponent
operation (^).  Since easyExp is defined by induction on the second argument
(which is some number), we will prove by induction on the second argument. In
the base case of 0 is quick:

~~~~~
x `easyExp` 0
= {- inlining the matching definition of easyExp: first case -}
1
= {- algebraic law of the ^ operation -}
x^0
~~~~~

For the inductive case where the second argument is some n+1, we have:

~~~~~
x `easyExp` n+1
= {- inlining the matching definition of easyExp: second case -}
x * (x `easyExp` (n+1)-1)
= {- algebraic laws of the + operation -}
x * (x `easyExp` n)
= {- inductive hypothesis for n -}
x * (x^n)
= {- algebraic law of the ^ operation -}
x^1 * x^n
= {- algebraic law of the ^ operation -}
x^(1+n)
= {- commutativity of + -}
x^(n+1)
~~~~~

This easy exponential operation is correct, but it's not especially efficient;
it's linear in the size of the second argument. Applying a little algebra, we
can calculate a much more efficient exponential function by applying a binary
division. In the inductive case, we can do something different depending on
whether or not the exponent is even. If the exponent is even, then it has the
form 2n for some n. In this case, the value of exponential is

~~~~~
x^(2n) = (x^2)^n = (x*x)^n
~~~~~

Otherwise, the exponent is odd of the form 2n+1 for some n. In this case, the
value of the exponential is

~~~~~
x^(2n+1) = (x^1) * (x^2n) = x * (x*x)^n
~~~~~

To perform this split between even and odd numbers, we can use the quotRem
function which performs division and returns *both* the quotient and the
remainder. The main property of quotRem is (assuming y is not 0)

~~~~~
x `quotRem` y = (q, r)
IF AND ONLY IF
x = y*q + r
~~~~~

Which can also be stated as the single equality

~~~~~
x = y * (fst (x `quotRem` y)) + (snd (x `quotRem` y))
~~~~~

Writing down both separate inductive cases with quotRem in Haskell gives

> binExp :: Integer -> Integer -> Integer
> x `binExp` 0 = 1
> x `binExp` n = case n `quotRem` 2 of
>                  (q,0) -> (x*x) `binExp` q
>                  (q,1) -> x * ((x*x) `binExp` q)

This function now is only logarithmic in the size of the second argument. How
much faster is it? On my machine, 1 `easyExp` 10000000 takes a couple of
seconds, and 1 `easyExp` 100000000 causes a stack overflow. But for the binary
binExp, even 1 `binExp` 100000000000000000000000000 runs in less than 10
milliseconds.

To show that this function is correct, we can apply equational reasoning in
Haskell proving it behaves the same as easyExp using *strong* induction on the
second argument, so we assume the fact holds not just for the next smaller
inputs, but for *all* smaller inputs. The base case follows just by the
definitions

~~~~~
x `binExp` 0 = 1 = x `easyExp` 0
~~~~~

The inductive step for n > 0 has two separate cases, depending on the result
of quotRem. In the first case, n `quotRem` 2 = (q, 0), so we have

~~~~~
x `binExp` n
= {- inlining matching definition of binExp: second case, first sub-case -}
x*x `binExp` q
= {- since n = 2*q and n /= 0, it must be that n > q,
     so the inductive hypothesis applies -}
x*x `easyExp` q
= {- easyExp is equal to the exponential operation -}
(x*x)^q
= {- by the above algebraic equality derived from exponential laws -}
x^(2*q)
= {- the main property of quotRem -}
x^n
= {- easyExp is equal to the exponential operation -}
x `easyExp` n
~~~~~

In the second case, n `quotRem` 2 = (q, 1), so we have

~~~~~
x `binExp` n
= {- inlining matching definition of binExp: second case, second sub-case -}
x * ((x*x) `binExp` q)
= {- since n = 2*q+1 it must be that n > q, so the inductive hypothesis applies -}
x * ((x*x) `easyExp` q)
= {- since easyExp is the exponential operation, and the calculation above -}
x `easyExp` (2*q + 1)
= {- the main property of quotRem -}
x `easyExp` n
~~~~~


Reversing
=========

Here is a direct way of defining the reverse function.

> easyRev :: [a] -> [a]
> easyRev []     = []
> easyRev (x:xs) = easyRev xs ++ [x]

where the list append function (++) is defined in the standard library as:
~~~~~
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
~~~~~

We can prove that this function makes sense by reasoning about how it reverses
an in-order numeric list [n .. m]

~~~~~
easyRev [n .. m] = easyRev [m, m-1 .. n]
~~~~~

for every n <= m, by induction on m - n. In the base case with m - n = 0, i.e.,
when n = m, we have

~~~~~
easyRev [n .. m]
= {- definition of [n .. m] when n = m -}
easyRev [n]
= {- desugaring list syntax -}
easyRev (n:[])
= {- definition of easyRev: second case -}
easyRev [] ++ [n]
= {- definition of easyRev: first case -}
[] ++ [n]
= {- definition of (++) -}
[n]
= {- definition of [m, m-1 .. n] when m = n -}
[n]
~~~~~

In the inductive case with m - n = k + 1, we have

~~~~~
easyRev [n .. m]
= {- definition of [n .. m] when n < m -}
easyRev (n : [n+1 .. m])
= {- definition of easyRev: second case -}
easyRev [n+1 .. m] ++ [n]
= {- inductive hypothesis, since m-(n+1) = (m-n)-1 = (k+1)-1 = k -}
[m, m-1 .. n+1] ++ [n]
= {- property of [m, m-1 .. n] -}
[m, m-1 .. n]
~~~~~

But this definition of reverse is also not very efficient: it is quadratic in
the length of the input list! That is because the append operator (++) is linear
in the length of the first list (its left argument).

This is a classic case where we need to "generalize the recursion"
(corresponding to "generalize the inductive hypothesis" in mathematics). Instead
of just reversing the list, we can efficiently do two operations at once:
reverse one list and append it on top of another list, simultaneously.  This
reversing append function can be written as

> revApp :: [a] -> [a] -> [a]
> []     `revApp` ys = ys
> (x:xs) `revApp` ys = xs `revApp` (x:ys)

A faster reverse function can now be derived from revApp by forcing the second
argument the empty list, thereby making the "append" part of revApp boring.

> fastRev :: [a] -> [a]
> fastRev xs = xs `revApp` []

How much faster is fastRev? On my machine, length (easyRev [1..10000]) takes
about a couple seconds to run and length (easyRev [1..100000]) takes much, much
longer (gave up and interrupted the calculation after about a minute of no
response). In contrast length (fastRev [1..10000]) returns immediately and
length (fastRev [1..100000]) still takes less than 10 milliseconds. Even going
up to length (fastRev [1..10000000]) takes a couple of seconds.

But is fastRev actually correct? It's not as easy to see as with the easyRev
function. Thankfully, since we already trust that easyRev does the "right"
thing, we can prove that fastRev is correct by proving that it is equal to
easyRev. In other words, we should prove that fastRev has the same output as
easyRev for every possible input. We can show this by induction on lists, with
the two basic cases (the empty list [], and a "cons" x:xs). The base case is
direct enough to show:

~~~~~
fastRev []
= {- inlining the definition of fastRev -}
[] `revApp` []
= {- definition of revApp: first case -}
[]
= {- definition of easyRev: first case -}
easyRev []
~~~~~

Now, let's try the inductive step, where we have the non-empty list argument
x:xs.

~~~~~
fastRev (x:xs)
= {- inlining the definition of fastRev -}
x:xs `revApp` []
= {- definition of revApp: second case -}
xs `revApp` [x]
???
~~~~~

But now we are stuck. The problem is that the inductive hypothesis that we have
(that fastRev xs = xs `revApp` [] = easyRev xs) is too specific, and does not
apply when using revApp with a non-empty second argument. So, just like we
generalized the recursive function to take more inputs, we need to also
generalize our inductive proof to consider more variability. The basic property
of revApp, which we stated informally above in the description, can be stated
formally as the following equation

~~~~~
xs `revApp` ys = easyRev xs ++ ys
~~~~~

Now, let's try to prove this more general fact about revApp by induction on the
first argument xs. The base case just follows the definitions of revApp,
easyRev, and list append (++).

~~~~~
[] `revApp` ys
= {- definition of revApp: first case -}
ys
= {- definition of (++) -}
[] ++ ys
= {- definition of easyRev: first case -}
easyRev [] ++ ys
~~~~~

But now, the inductive step has a more general inductive hypothesis (where the
second argument to revApp could be *any* list), which lets us continue on from
where we got stuck before.

~~~~~
x:xs `revApp` ys
= {- definition of revApp: second case -}
xs `revApp` x:ys
= {- inductive hypothesis -}
easyRev xs ++ x:ys
= {- definition of (++) -}
easyRev xs ++ ([x] ++ ys)
= {- (++) is associative (exercise left to reader): (xs++ys)++zs = xs++(ys++zs) -}
(easyRev xs ++ [x]) ++ ys
= {- definition of easyRev: second case -}
easyRev (x:xs) ++ ys
~~~~~

So the fact that fastRev is a correct reverse function (because it is equal to
the easyRev, which we trust is a correct reverse function) is derived as a
special case of the above property about revApp where the second argument
happens to be empty.

~~~~~
fastRev xs
= {- inlining the definition of fastRev -}
xs `revApp` []
= {- the above property of revApp -}
easyRev xs ++ []
= {- (++) has [] as a neutral element (see below): ys++[] = ys -}
easyRev xs
~~~~~

In the above equational reasoning, we needed to use some facts about list append
(++).

~~~~~
[] ++ ys = ys
xs ++ [] = xs
(xs ++ ys) ++ zs = xs ++ (ys ++ zs)
~~~~~

These are some of the main properties that describe how list append works, and
helps us to reason about what our list-processing programs do.  The first pair
of properties says that appending the empty list [] to any other list doesn't
change it, in either direction:


The first equation, [] ++ ys = ys, is true by definition: it is exactly the
first line which defines how (++) is computed.  The second one, xs ++ [] = xs,
is a little harder, because the definition of (++) doesn't say directly that
this equation is true.  Instead, we have to work to prove the second equation
above by induction on the first argument 'xs' to (++).

The base case, when xs = [], follows by directly applying the definition of
(++).

~~~~~
[] ++ []
= {- definition of (++): first case -}
[]
~~~~~

The inductive step, when xs = x:xs', requires that we use this *inductive
hypothesis*

~~~~~
xs' ++ [] == xs'    (inductive hypothesis)
~~~~~

in the following equational reasoning:

~~~~~
(x:xs') ++ []
= {- definition of (++): second case -}
x : (xs' ++ [])
= {- inductive hypothesis -}
x : xs'
~~~~~

The final equation of associativity, (xs++ys)++zs = xs++(ys++zs), also does not
follow directly by applying the definition of (++).  As with xs++[]=xs, proving
associativity by induction on xs.  Since the definition of (++) matches on its
left argument, which is 'xs' here, we need to break down 'xs' into its possible
shapes to let us compute what (++) will do in any possible case.
