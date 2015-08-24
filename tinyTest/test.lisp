; **************** BEGIN INITIALIZATION FOR ACL2s B MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);

#|
Pete Manolios
Fri Jan 27 09:39:00 EST 2012
----------------------------

Made changes for spring 2012.


Pete Manolios
Thu Jan 27 18:53:33 EST 2011
----------------------------

The Beginner level is the next level after Bare Bones level.

|#

; Put CCG book first in order, since it seems this results in faster loading of this mode.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "ccg/ccg" :uncertified-okp nil :dir :acl2s-modes :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

;Common base theory for all modes.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s base theory book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "base-theory" :dir :acl2s-modes)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "custom" :dir :acl2s-modes :uncertified-okp nil :ttags :all)

;Settings common to all ACL2s modes
(acl2s-common-settings)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading trace-star and evalable-ld-printing books.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "trace-star" :uncertified-okp nil :dir :acl2s-modes :ttags ((:acl2s-interaction)) :load-compiled-file nil)
(include-book "hacking/evalable-ld-printing" :uncertified-okp nil :dir :system :ttags ((:evalable-ld-printing)) :load-compiled-file nil)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s Beginner mode.") (value :invisible))
;Settings specific to ACL2s Beginner mode.
(acl2s-beginner-settings)

; why why why why 
(acl2::xdoc acl2s::defunc) ; almost 3 seconds

(cw "~@0Beginner mode loaded.~%~@1"
    #+acl2s-startup "${NoMoReSnIp}$~%" #-acl2s-startup ""
    #+acl2s-startup "${SnIpMeHeRe}$~%" #-acl2s-startup "")


(acl2::in-package "ACL2S B")

; ***************** END INITIALIZATION FOR ACL2s B MODE ******************* ;
;$ACL2s-SMode$;Beginner
;(defun successor (x) (1+ x))

(defunc >5 (x)
  :input-contract (and (integerp x) (>= x -5))
  :output-contract (booleanp (>5 x))
  (> x 5))

; Compute the nth fibonacci number (maybe)
(defunc fib (n)
  :input-contract (natp n)
  :output-contract (natp (fib n))
  (if (< 2 n)
    (+ (fib (- n 1)) (fib (- n 2)))
    1))


(defunc even-natp (x)
  :input-contract (natp x)
  :output-contract (booleanp (even-natp x))
  (if (equal x 0)
    t
    (not (even-natp (- x 1)))))

(defunc snoc (l e)
  ; Returns a list like l, but with e at the end
  :input-contract (and (listp l))
  :output-contract (listp (snoc l e))
  (if (endp l)
    (cons e l)
    (cons (first l)
          (snoc (rest l) e))))

(defunc app2 (l1 l2)
  ; Returns a list that append l1 and l2
  :input-contract (and (listp l1) (listp l2))
  :output-contract (listp (app2 l1 l2))
  (if (endp l2)
    l1
    (app2 (snoc l1 (first l2)) (rest l2))))

(defunc even-integerp (n)
  :input-contract (integerp n)
  :output-contract (booleanp (even-integerp n))
  (cond
   ((equal n 0) t)
   ((< n 0) (not (even-integerp (+ n 1))))
   (t (not (even-integerp (- n 1))))))

(defunc even-integerp2 (n)
  :input-contract (integerp n)
  :output-contract (booleanp (even-integerp2 n))
  (cond
   ((equal n 0) t)
   ((< n 0) (even-natp (* n -1)))
   (t (even-natp n))))

(test? (implies (< 20/3 n)
                (equal (even-integerp n)
                       (even-natp n))))#|ACL2s-ToDo-Line|#



(test? (implies (natp n)
                (equal (even-integerp n)
                       (even-natp n))))



(defunc rev# (l)
  ; Returns a reversion of the given list
  :input-contract (listp l)
  :output-contract (listp (rev# l))
  (if (endp l)
    l
    (app (rev# (rest l)) (list (first l)))))

(defunc app1 (l1 l2)
  ; Returns a list that append l1 and l2
  :input-contract (and (listp l1) (listp l2))
  :output-contract (consp (app l1 l2))
  (if (endp l1)
    l2
    (cons (first l1) (app (rest l1) l2))))


(defunc len1 (l)
  ; Returns the length of the list l.    
  :input-contract (listp l)
  :output-contract (natp (len l))
  (if (endp l) 
    0
    (+ 1 (len (rest l)))))

(defunc atom (x)
  :input-contract t
  :output-contract (boolean (atom x))
  (not (consp l)))

(defunc endp (l)
  :input-contract (listp l)
  :output-contract (boolean (endp l))
  (not (consp l)))

(defunc even-integerp (x)
  :input-contract (integerp x)
  :output-contract (booleanp (even-integerp x))
  (cond
   ((equal x 0) t)
   ((< x 0) (not (even-integerp (+ x 1))))
   (t (not (even-integerp (- x 1)))))) 


(check= (even-integerp 0) t)
(check= (even-integerp 1) nil)
(check= (even-integerp -22) t)