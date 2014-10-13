(define-library (eros)
  (import (scheme base)
          (scheme write)
          (srfi 1)
          (srfi 8)
          (srfi 95)
          (picrin base)
          (picrin dictionary)
          (picrin control))

  (define-record-type class
    (make-class membership relation-list)
    class?
    (membership class-membership)
    (relation-list class-relation-list set-class-relation-list!))

  (define (add-relation! subclass superclass)
    (set-class-relation-list!
     subclass
     (cons superclass (class-relation-list subclass))))

  (define (instance-of? obj class)
    ((class-membership class) obj))

  (define (collect-superclasses class)
    (delete-duplicates
     (cons class
           (append-map collect-superclasses
                       (class-relation-list class)))))

  (define (class-inclusive? class1 class2)
    (if (member class1 (cdr (collect-superclasses class2))) #t #f))

  (define (preceding-sort method-lst)
    (reverse
     (merge-sort
      method-lst
      (lambda (x y)
        (any class-inclusive? (car x) (car y))))))

  (define (find-method args method-lst)
    (let ((applicable-methods
           (filter
            (lambda (x)
              (every values
                     (map instance-of? args (car x))))
            method-lst)))
      (if (null? applicable-methods)
          (error "No applicable methods found")
          (map cdr (preceding-sort applicable-methods)))))

  (define (make-generic)
    (define (generic . args)
      (let ((method-alst
             (dictionary-ref (attribute generic) 'methods)))
        (if method-alst
            (reset
             (lambda ()
               (let loop ((methods (find-method args method-alst)) (args args))
                 (if (null? methods) (error "No next methods fonud")
                     (loop (cdr methods) (apply (car methods) args))))))
            (error "No methods found"))))
    generic)

  (define (add-method generic-fn arg-type-list closure)
    (dictionary-set!
     (attribute generic-fn) 'methods
     `((,arg-type-list . ,closure) .
       ,(let ((x (dictionary-ref (attribute generic-fn) 'methods)))
          (if x x '())))))

  (define-syntax define-class
    (syntax-rules ()
      ((_ class-name membership)
       (define class-name
         (make-class membership '())))))

  (define-syntax define-relation
    (syntax-rules ()
      ((_ subclass superclass)
       (add-relation! subclass superclass))))

  (define-syntax define-generic
    (syntax-rules ()
      ((_ generic-name)
       (define generic-name
         (make-generic)))))

  (define-syntax extract?
    (syntax-rules ()
      ((_ symb body _cont-t _cont-f)
       (letrec-syntax
           ((tr
             (syntax-rules (symb)
               ((_ x symb tail (cont-head symb-l . cont-args) cont-false)
                (cont-head (x . symb-l) . cont-args))
               ((_ d (x . y) tail . reset)
                (tr x x (y . tail) . reset))
               ((_ d1 d2 () cont-t (cont-head symb-l . cont-args))
                (cont-head (symb . symb-l) . cont-args))
               ((_ d1 d2 (x . y) . reset)
                (tr x x y . reset)))))
         (tr body body () _cont-t _cont-f)))))

  (define-syntax extract
    (syntax-rules ()
      ((_ symb body cont)
       (extract? symb body cont cont))))

  (define-syntax inject-call-next-method
    (syntax-rules ()
      ((_ method-name names types bodies)
       (let-syntax
           ((cont
             (syntax-rules ()
               ((_ (symb) bodies)
                (add-method method-name
                            (list . types)
                            (lambda names
                              (shift
                               (lambda (symb)
                                 . bodies))))))))
         (extract call-next-method bodies
                  (cont () bodies))))))

  (define-syntax separate-method-args
    (syntax-rules ()
      ((_ ((n t) arg ...) (name ...) (type ...)  k)
       (separate-method-args (arg ...) (name ... n) (type ... t) k))
      ((_ (n arg ...) (name ...) (type ...) k)
       (separate-method-args (arg ...) (name ... n) (type ... <value>) k))
      ((_ () (name ...) (type ...) (method-name body ...))
       (inject-call-next-method method-name (name ...) (type ...) (body ...)))))

  (define-syntax define-method
    (syntax-rules ()
      ((_ (method-name arg ...) body ...)
       (separate-method-args (arg ...) () () (method-name body ...)))))

  (define-class <value> (lambda (obj) #t))
  (define-class <class> class?)
  (define-class <number> number?)
  (define-class <string> string?)
  (define-class <procedure> procedure?)
  (define-class <boolean> boolean?)
  (define-class <char> char?)
  (define-class <null> null?)
  (define-class <pair> pair?)
  (define-class <symbol> symbol?)
  (define-class <bytevector> bytevector?)
  (define-class <eof-object> eof-object?)
  (define-class <port> port?)

  (define-generic class-of)
  (define-method (class-of obj)
    <value>)
  (define-method (class-of (num <number>))
    <number>)
  (define-method (class-of (str <string>))
    <string>)
  (define-method (class-of (proc <procedure>))
    <procedure>)
  (define-method (class-of (bool <boolean>))
    <boolean>)
  (define-method (class-of (chr <char>))
    <char>)
  (define-method (class-of (nil <null>))
    <null>)
  (define-method (class-of (pare <pair>))
    <pair>)
  (define-method (class-of (symb <symbol>))
    <symbol>)
  (define-method (class-of (bytvect <bytevector>))
    <bytevector>)
  (define-method (class-of (eofobj <eof-object>))
    <eof-object>)
  (define-method (class-of (port <port>))
    <port>)

  (export class? define-class instance-of?
          define-generic define-method class-of define-relation
          <class> <value> <number> <string> <procedure> <boolean>
          <null> <pair> <symbol> <bytevector> <eof-object> <port>))
