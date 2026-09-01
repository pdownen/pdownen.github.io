#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functional Programming ;;;
;;;          with          ;;;
;;; Higher-Order Functions ;;;
;;;           in           ;;;
;;;      Scheme/Racket     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lists ;;;

;; LISP = LISt Processing

;; Lists written as (x y ... z)

;; A list can be interpreted as either CODE (a way to write a program
;; expression) or as DATA (a list of information)

;; The expression (define name expr) evaluates the expression `expr`
;; and whatever value it returns gets bound to the variable `name`
(define two 2)

;; Calling the function f with arguments x, y, z, looks like the list
;; (f x y z).  This syntax is used for ALL function calls, even to
;; arithmetic operations like multiplication *
(define four (* two two))

;; The `list` function takes any number of arguments, and constructs a
;; list containing them
(define one-two-three (list 1 2 3))

;; You can also "quote" with the single-quote character ' any list to
;; say "please interpret this list as a literal data value, NOT as
;; code to be executed"
(define abc '(a b c))

abc

;; The `car` function extracts the first element of list and the `cdr`
;; function (pronounced like "cudder") extracts the rest of a list
;; that follows the first element
(car abc)
(cdr abc)

;; The `cons` constructor adds a new element in front of a list, so
;; that (cons w (list x y z)) returns (list w x y z).  If the second
;; argument y to (cons x y) is some value OTHER than a list, then
;; `cons` returns a "pair" of the two elements written as (a . b)
(cons 0 abc)
(cons 'a 'b)

;; Each call to `cons` generates a brand new object in computer
;; memory, even if it is given the same arguments a second time
(define ab-twice
  (cons (cons 'a 'b)
        (cons 'a 'b)))

ab-twice

;; The cons cells contained in `ab-twice` contain the same data (as
;; checked by the `equal?` function for testing value equality) but
;; refer to different objects in memory (as checked by the `eq?`
;; function for testing pointer equality)
(equal? (car ab-twice) (cdr ab-twice))
(eq? (car ab-twice) (cdr ab-twice))

;; Instead of generating duplicate objects of the same information,
;; you can remember and re-use a value by binding it to a variable. A
;; `let` expression lets us define a local variable name by setting it
;; equal to a value that we can use several times.
(define ab-shared
  (let ([ab (cons 'a 'b)])
    (cons ab ab)))

;; Even though `ab-shared` looks like a similar value to as
;; `ab-twice`, it has a different pointer structure because it re-uses
;; the same heap object storing '(a . b) twice.
ab-shared
(equal? ab-shared ab-twice)
(eq? ab-shared ab-twice)
(equal? (car ab-shared) (cdr ab-shared))
(eq? (car ab-shared) (cdr ab-shared))


;; To process lists, we can write functions taking lists as inputs
;; and/or returning lists as outputs.  If you take a list as an input,
;; you may want to check whether or not that list is empty.

;; The `null?` function checks whether its argument is the empty list
;; `null`.
null
(null? null)
(null? abc)

;; The `cons?` function checks whether its argument was built by
;; `cons`; all lists are built by `cons`.
(cons? null)
(cons? abc)

;; We can use the tests `null?` and `cons?` together with the
;; extraction functions `car` and `cdr` to write functions which
;; process any list. The general template for a recursive function `f`
;; taking a single list `xs` as an argument will usually look like
;;
;;    (define (f xs)
;;      (cond [(null? xs) ...]
;;            [(cons? xs) ... (car xs) ... (f (cdr xs)) ...]))

;; (append xs ys) puts the list `xs` in front of the list `ys`
(define (append xs ys)
  (cond [(null? xs) ys]
        [(cons? xs)
         (cons (car xs)
               (append (cdr xs) ys))]))

(append abc one-two-three)

;; (reverse xs) rearranges the list `xs` in reverse order
(define (reverse xs)
  (cond [(null? xs) xs]
        [(cons? xs)
         (append (reverse (cdr xs))
                 (list (car xs)))]))

(reverse abc)

;;; Higher-Order Functions ;;;

;; As we've seen with `append` and `reverse`, you can define a Scheme
;; function by giving parameters alongside a `define`d name.  So the
;; expression (define (f x y z) expr) defines a new function named `f`
;; taking three parameters (x, y, and z), and returns the result of
;; `expr` when called with three arguments to plug into its
;; parameters.

;; Here's a function which does a little arithmetic on numbers
(define (f1 x y)
  (* 2 (+ x y)))

;; Alternatively, you can define the *same* function as `f1` by
;; binding a lambda expression to a variable name.
(define f2
  (lambda (x y) (* 2 (+ x y))))

;; What's that "lambda"? It an ASCII spelling of the λ from the
;; λ-calculus! The λ-calculus is a core part of Scheme, all λ-calculus
;; expressions can be translated to Scheme (and sometimes vice
;; versa). Here are the translations between Scheme and λ-calculus
;; functions:

;; Scheme's "lambda" versus λ
;;
;;     (lambda (x y z) ...) in Scheme
;;     = λx. λy. λz. ...    in λ-calculus
;;     = λx.(λy.(λz. ...))  in λ-calculus

;; Scheme's application vs λ-calculus application
;;
;;     (f x y z)       in Scheme
;;     = f x y z       in λ-calculus
;;     = (((f x) y) z) in λ-calculus

;; Scheme's function definitions vs lambda value definitions
;;
;;     (define (f x y z) ...)   in Scheme
;;     = (define f
;;         (lambda (x y z) ...) in Scheme

;; lambda-expressions are first-class values, meaning they can be
;; bound to names, passed to functions, and returned from function
;; calls.

;; For example, `apply0` takes any function as its first parameter
;; `f`, and calls it with the argument 0.
(define (apply0 f)
  (f 0))

;; `twice` takes any function `f` and a possible argument `x`, and
;; then applies f twice to x, meaning first apply f to x, and then
;; apply f to the result of (f x).
(define (twice f x)
  (f (f x)))

;; Function composition does two things:
;;
;;   1. It takes two functions, `f`, and `g`, as arguments
;;
;;   2. It returns a new function, which takes an argument `x`, and
;;   applies the two functions in a row as (f (g x))
(define (compose f g)
  (lambda (x) (f (g x))))

;; The definition of `compose` as a Scheme function can be rewritten
;; as giving a name to a lambda value
(define compose*
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; The Scheme definition of `compose*` can now be translated to the
;; λ-calculus like so
;;
;;     compose* = λf. λg. λx. f (g x)

;; The `twice` function can be rewritten to use `compose` by passing
;; it the same function argument twice
(define (twice* f x)
  ((compose f f) x))

;;; Higher-Order List-Processing functions ;;;

;; (map f xs) changes every element in the list xs by applying the
;; given function f
;;
;;     (map f (list x y z))
;;     = (list (f x) (f y) (f z))
(define (map change-car xs)
  (cond
    [(null? xs) xs]
    [(cons? xs)  ; xs = (cons (car xs) (cdr xs))
     (cons (change-car (car xs))
           (map change-car (cdr xs)))]))

(map (lambda (x) (* x x)) (list 1 2 3 4))
(map even? (list 1 2 3 4))

;; (reduce f z xs) condenses the list xs into a summary value by
;; replacing the final empty `null` with the value z, and then
;; replacing every `cons` cell with an application of `f`
;;
;;     (reduce f z
;;      (cons 1 (cons 2 (cons 3 null))))
;;     = (f 1 (f 2 (f 3 z)))
(define (reduce change-cons
                change-nil
                xs)
  (cond [(null? xs) change-nil]
        [(cons? xs)  ;; xs = (cons (car xs) (cdr xs))
         (change-cons
          (car xs)
          (reduce change-cons change-nil (cdr xs)))]))

(reduce * 1 (list 1 2 3 4))
(reduce (lambda (a b) (and a b)) #t (list #t #t #t #t))
(reduce (lambda (a b) (and a b)) #t (list #t #f #t #t))

;; You can combine map and reduce together to
;;
;;   1. Do something to process each individual element of a list
;;      independently of the others, and then
;;
;;   2. Calculate a final result based on those newly-generated values


;; Check if all elements of a list are even
(reduce (lambda (a b) (and a b)) #t
        (map even? (list 2 4 6 8)))
(reduce (lambda (a b) (and a b)) #t
        (map even? (list 2 4 5 6 8)))

;; This can be really useful for organizing complex programs, like
;; distributed systems that operate over data stored on many computers
;; on a network!

;; (filter f xs) doesn't change any elements of xs, but instead only
;; keeps the elements of xs that pass a test calculated by f (in other
;; words, it returns a list containing elements where f returns #t)
(define (filter check-car xs)
  (cond [(null? xs) xs]
        [(and (cons? xs)  ; xs = (cons (car xs) (cdr xs))
              (check-car (car xs)))
         (cons (car xs)
               (filter check-car (cdr xs)))]
        [else (filter check-car (cdr xs))]))

(filter even? (list 1 2 3 4))

;;; Lists as Trees ;;;

(define list-of-lists
  (list one-two-three
        abc
        '("hello" "goodbye")))

(define tree '((1 2) 3 (4 (5) 6)))

(define (map-tree f t)
  (cond [(null? t) t]
        [(cons? t)
         (cons (map-tree f (car t))
               (map-tree f (cdr t)))]
        [else (f t)]))

tree
(map-tree (lambda (x) (* x x)) tree)
