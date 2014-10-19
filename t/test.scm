(define-library (eros test)
  (import (picrin base)
          (scheme base)
          (picrin test))

  (load "/home/stibear/src/EROS/src/eros.scm")
  (import (eros))

  (test-begin "EROS")

  (test-begin "define-class")

  (define-record-type pare
    (kons x y)
    pare?
    (x kar)
    (y kdr))

  (define-class <pare> pare?)

  (test #t (record? <pare>))
  (test #t (class? <pare>))

  (test-end)

  (test-begin "define-generic & define-method")

  (define foo (kons 1 2))

  (define-generic bar)
  (define-method (bar obj)
    obj)

  (test foo (bar foo))

  (test-end)

  (test-begin "define-relation & call-next-method")
  
  (define-class <integer> integer?)

  (define-relation <integer> <number>)

  (define-generic hoge)

  (define-method (hoge (n <number>))
    'number)
  (test 'number (hoge 10))
  
  (define-method (hoge (n <integer>))
    (call-next-method n))
  (test 'number (hoge 10))

  (test-end)

  (test-begin "redefinition of method")

  (define-generic hoge)

  (define-method (hoge n)
    n)

  (define-method (hoge n)
    (+ n 1))

  (test 11 (hoge 10))

  (test-end)

  (test-end))
