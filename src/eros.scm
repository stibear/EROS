(define-library (eros)
  (import (scheme base)
          (scheme write)
          (srfi 1)
          (srfi 8)
          (picrin macro)
          (picrin attribute)
          (picrin dictionary))

  (define-record-type class
    (make-class membership)
    class?
    (membership class-membership))

  (define-syntax define-class
    (syntax-rules ()
      ((_ class-name membership)
       (define class-name
         (make-class membership)))))

  (define-class <value> (lambda (obj) #t))
  (define-class <class> class?)

  (define (instance-of? obj class)
    ((class-membership class) obj))

  (define method-sym 'methods)

  (define-syntax define-generic
    (ir-macro-transformer
     (lambda (form inject compare)
       (let ((generic-name (cadr form)))
         `(define (,generic-name . args)
            (let ((method-alst
                   (dictionary-ref (attribute ,generic-name) method-sym)))
              (if method-alst
                  (apply (find-method args method-alst) args)
                  (error "No methods found"))))))))
  
  (define (add-method generic-fn arg-type-list closure)
    (dictionary-set!
     (attribute generic-fn) 'methods
     `((,arg-type-list . ,closure) .
       ,(let ((x (dictionary-ref (attribute generic-fn)
                                 'methods)))
          (if x x '())))))

  (define (method-args-types lst)
    (map
     (lambda (x)
       (if (pair? x) (cadr x) <value>))
     lst))

  (define (method-args-params lst)
    (map
     (lambda (x)
       (if (pair? x) (car x) x))
     lst))

  (define-syntax define-method
    (ir-macro-transformer
     (lambda (form rename compare)
       (let ((method-name (caadr form))
             (args (cdadr form))
             (body (cddr form)))
         `(add-method ,method-name
                      (list ,@(method-args-types args))
                      (lambda ,(method-args-params args)
                        ,@body))))))
  
  (define (find-method args method-lst)
    (let ((method
           (member
            args method-lst
            (lambda (x y)
              (every values
                     (map instance-of? x (car y)))))))
      (if method
          (cdar method)
          (error "No methods found"))))

  (define-class <number> number?)
  (define-class <string> string?)
  (define-class <procedure> procedure?)
  (define-class <boolean> boolean?)
  (define-class <char> char?)
  (define-class <null> null?) ; needless?
  (define-class <pair> pair?) ; should be replaced by list??
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
          define-generic define-method class-of
          <class> <value> <number> <string> <procedure> <boolean>
          <null> <pair> <symbol> <bytevector> <eof-object> <port>))
