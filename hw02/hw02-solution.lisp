#|

CS 2800 Homework 2 - Spring 2015

This homework is done in groups. The rules are:

 * ALL group members must submit the homework file (this file)
 * the file submitted must be THE SAME for all group members (we use this
   to confirm that alleged group members agree to be members of that group)
 * you must list the names of ALL group members below.

Names of ALL group members: ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw02.lisp

- make sure you are in BEGINNER mode. This is essential! Note that you can
  only change the mode when the session is not running, so set the correct
  mode before starting the session.

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish all problems, comment
  the unfinished ones out. Comments should also be used for any English
  text that you may add. This file already contains many comments, so you
  can see what the syntax is.

- when done, save your file and submit it as hw02.lisp

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution. Only submit the lisp
  file.

Instructions for programming problems:

For each function definition, you must provide both contracts and a body.

You must also ALWAYS supply your own tests. This is in addition to the
tests sometimes provided. Make sure you produce sufficiently many new test
cases. This means: cover at least the possible scenarios according to the
data definitions of the involved types. For example, a function taking two
lists should have at least 4 tests: all combinations of each list being
empty and non-empty.

Beyond that, the number of tests should reflect the difficulty of the
function. For very simple ones, the above coverage of the data definition
cases may be sufficient. For complex functions with numerical output, you
want to test whether it produces the correct output on a reasonable
number if inputs.

Use good judgment. For unreasonably few test cases we will deduct points.

We will use ACL2s' check= function for tests. This is a two-argument
function that rejects two inputs that do not evaluate equal. You can think
of check= roughly as defined like this:

(defunc check= (x y)
  :input-contract (equal x y)
  :output-contract (equal (check= x y) t)
  t)

That is, check= only accepts two inputs with equal value. For such inputs, t
(or "pass") is returned. For other inputs, you get an error. If any check=
test in your file does not pass, your file will be rejected.

|#

#|

Since this is our first programming exercise, we will simplify the
interaction with ACL2s somewhat: instead of asking it to formally *prove*
the various conditions for admitting a function, we will just require that
they be *tested* on a reasonable number of inputs. This is achieved using
the following directive (do not remove it!):

|#

:program

#|

Notes:

1. Testing is cheaper but less powerful than proving. So, by turning off
proving and doing only testing, it is possible that the functions we are
defining cause runtime errors even if called on valid inputs. In the future
we will require functions complete with admission proofs, i.e. without the
above directive. For this first homework, the functions are simple enough
that there is a good chance ACL2s's testing will catch any contract or
termination errors you may have.

2. The tests ACL2s runs test only the conditions for admitting the
function. They do not test for "functional correctness", i.e. does the
function do what it is supposed to do? ACL2s has no way of telling what
your function is supposed to do. That is what your own tests are for!

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; has-dups: List -> Boolean

; (has-dups l) returns t if l contains any element (at least) twice; nil otherwise.

(defunc has-dups (l)
  :input-contract (listp l)
  :output-contract (booleanp (has-dups l))
  (if (endp l)
    nil
    (or (in (first l) (rest l))
        (has-dups (rest l)))))

(check= (has-dups '(1))   nil)
(check= (has-dups '(1 1)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; how-many: All x List -> Nat

; (how-many e l) returns the number of occurrences of e in l.

(defunc how-many (e l)
  :input-contract (listp l)
  :output-contract (natp (how-many e l))
  (if (endp l)
    0
    (+ (if (equal e (first l)) 1 0)
       (how-many e (rest l)))))

(check= (how-many  1 ())     0)
(check= (how-many  1 '(1 1)) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; same-multiplicity: List x List x List -> Boolean

; (same-multiplicity l l1 l2) returns t iff every element of l occurs in l1
; and l2 the same number of times.

(defunc same-multiplicity (l l1 l2)
  :input-contract (and (listp l) (and (listp l1) (listp l2)))
  :output-contract (booleanp (same-multiplicity l l1 l2))
  (if (endp l)
    t
    (and (equal (how-many (first l) l1)
                (how-many (first l) l2))
         (same-multiplicity (rest l) l1 l2))))

(check= (same-multiplicity '(1)   '(2 1 3) '(1 2 2)) t)
(check= (same-multiplicity '(1 2) '(2 1 3) '(1 2 2)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; is-permutation: List x List -> Boolean

; (is-permutation l1 l2) returns t iff l1 is a permutation of l2.
; l1 is a permutation of l2 exactly if l1 and l2 differ only in the order
; of their elements (in particular, they must have the same length).

; Hint: this function is easy and non-recursive if you consider how
; function same-multiplicity can help you. To do that, reformulate, in
; terms of multiplicity, the condition that l1 and l2 are permutations of
; each other.

(defunc is-permutation (l1 l2)
  :input-contract (and (listp l1) (listp l2))
  :output-contract (booleanp (is-permutation l1 l2))
  (and (same-multiplicity l1 l1 l2)
       (same-multiplicity l2 l1 l2)))

(check= (is-permutation '(1 2) '(2 1))   t)
(check= (is-permutation '(1 3) '(3 1 1)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following data definitions define the types, "list of rational
; numbers" and "list of natural numbers". They automatically give rise to
; recognizers for these types, called rationallistp : All -> Boolean and
; natlistp : All -> Boolean, which you are free to use.

(defdata rationallist (listof rational))
(defdata natlist (listof nat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; insert Nat x Natlist -> Natlist

; (insert n l) inserts a natural number n in the list of numbers l
; before the first element in l which is >= (greater than or
; equal to) n , or at the end of l if no such element exists.

(defunc insert (n l)
  :input-contract (and (natlistp l) (natp n))
  :output-contract (natlistp (insert n l))
  (cond ((endp l)        (cons n l))
        ((< n (first l)) (cons n l))
        (t               (cons (first l) (insert n (rest l))))))

(check= (insert 1 '(2 3 4)) '(1 2 3 4))
(check= (insert 1 nil) '(1))
(check= (insert 4 '(3 2 5)) '(3 2 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; minimum: Rationallist-{0} -> Rational

; (minimum l) returns the minimum among the numbers in l. Note that l is
; required to be non-empty.

; Hint: first write a binary function min : Rational x Rational -> Rational
; with the obvious meaning. Then define minimum recursively.

(defunc min (x y)
  :input-contract (and (rationalp x) (rationalp y))
  :output-contract (rationalp (min x y))
  (if (< x y) x y))

(defunc minimum (l)
  :input-contract (and (rationallistp l) (not (endp l)))
  :output-contract (rationalp (minimum l))
  (let ((f (first l))
        (r (rest  l)))
    (if (endp r)
      f
      (min f (minimum r)))))

(check= (minimum '(4 2 5 -2/3)) -2/3)
(check= (minimum '(1)) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; rem-rep-list : Natlist -> Natlist

; (rem-rep-list l) replaces double consecutive occurrences of a number in
; the Natlist l by a single occurrence.

(defunc rem-rep-list (l)
  :input-contract (natlistp l)
  :output-contract (natlistp (rem-rep-list l))
  (cond ((endp l)                     l)
        ((endp (rest l))              l)
        ((equal (first l) (second l)) (rem-rep-list (rest l)))
        (t                            (cons (first l) (rem-rep-list (rest l))))))

(check= (rem-rep-list '(1 2 3 4)) '(1 2 3 4))
(check= (rem-rep-list '(1 1 3 4)) '(1 3 4))
(check= (rem-rep-list '(1 3 1 4)) '(1 3 1 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; every-other : List -> List

; (every-other l) returns every other element of l, beginning with the
; first, i.e. it returns the elements at positions 0,2,4,...

(defunc every-other (l)
  :input-contract (listp l)
  :output-contract (listp (every-other l))
  (cond ((endp l)        ())
	((endp (rest l)) l)
        (t               (cons (first l) (every-other (rest (rest l)))))))

(check= (every-other  ())       ())
(check= (every-other '(1 2))   '(1))
(check= (every-other '(1 2 3)) '(1 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; rem: Nat x Nat-{0} -> Nat

; (rem x y) returns the remainder of the integral division of x by y.

; Hint: write a recursive function.

(defunc rem (x y)
  :input-contract (and (natp x) (natp y) (not (equal y 0)))
  :output-contract (natp (rem x y))
  (if (integerp (/ x y))
    0
    (+ 1 (rem (- x 1) y))))

(check= (rem 2 4) 2)
(check= (rem 4 2) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; nat/: Nat x Nat-{0} -> Nat

; (nat/ x y) returns the result of integer division of x by y.
; That is, it returns the integral part of x/y,
; which is a natural number. See the examples below.

; Hint: this is a non-recursive one-liner.

(defunc nat/ (x y)
  :input-contract (and (natp x) (natp y) (not (equal y 0)))
  :output-contract (natp (nat/ x y))
  (/ (- x (rem x y)) y))

(check= (nat/ 10 2) 5)
(check= (nat/ 11 2) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define
; add-digits: Nat -> Nat

; (add-digits x) returns the sum of the decimal digits in x,
; which is a natural number.

; Hint: Write a recursive definition that sums up the digits in x from the
; least significant to the most significant. The function rem will be
; helpful. Also recall what we learned about let and let* -- avoid using
; the same expression twice in your function body.

(defunc add-digits (x)
  :input-contract (natp x)
  :output-contract (natp (add-digits x))
  (if (< x 10)
    x
    (let* ((rem (rem x 10))
           (y   (/ (- x rem) 10)))
      (+ rem (add-digits y)))))

(check= (add-digits 000) 0)
(check= (add-digits 123) 6)
