(define-library (eros test)
  (import (picrin base)
          (scheme base)
          (picrin test))

  (load "src/eros.scm")
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

  (define foo (kons 1 2))

  (test-begin "define-generic & define-method")

  (define-generic bar)
  (define-method (bar obj)
    obj)

  (test foo (bar foo))

  (define-method (bar (pr <pare>))
    (list (kar pr) (kdr pr)))

  (test '(1 2) (bar foo))

  (test-end)

  (test-end))
