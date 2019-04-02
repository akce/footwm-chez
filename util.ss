(library (util)
  (export
   bitmap
   enum
   fmem
   list-find-index
   list-insert
   list-replace)
  (import
   (chezscheme))

  (define-syntax bitmap
    (syntax-rules ()
      [(_ name (symbol bit) ...)
       (begin (define symbol (fxsll 1 bit)) ...)]))

  (define-syntax enum
    (syntax-rules ()
      [(_ name (symbol value) ...)
       (begin (define symbol value) ...)]))

  ;; [syntax] (fmem ((var varptr type)) ...)
  (define-syntax fmem
    (syntax-rules ()
      [(_ ((var varptr type) ...) first rest ...)
       (let ([var (foreign-alloc (ftype-sizeof type))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (let ((r first))
             ;; TODO should be wrapped via exceptions.
             rest ...
             (foreign-free var) ...
             r)))]
      [(_ ((var varptr type num) ...) first rest ...)
       ;; Ensure num is at least 1, that's a requirement of foreign-alloc.
       (let ([var (foreign-alloc (* (if (= num 0) 1 num) (ftype-sizeof type)))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (let ((r first))
             ;; TODO should be wrapped via exceptions.
             rest ...
             (foreign-free var) ...
             r)))]))

  ;; Like 'find' but returns the 0-based index of the first match for predicate, #f otherwise.
  (define list-find-index
    (lambda (pred lst)
      (let ([index 0])
        (if (find
             (lambda (x)
               (cond
                [(pred x) #t]
                [else (set! index (add1 index)) #f]))
             lst)
            index
            #f))))

  (define list-insert
    (lambda (lst item pos)
      (let loop ((i 0) (rem lst) (ret '()))
        (if (null? rem)
            (reverse (if (>= pos i) (cons item ret) ret))
            (let ([tail (cons (car rem) (if (= pos i) (cons item ret) ret))])
              (loop (fx+ i 1) (cdr rem) tail))))))

  (define list-replace
    (lambda (lst pos item)
      (let loop ((i 0) (rem lst) (ret '()))
        (if (null? rem)
            (reverse ret)
            (loop (add1 i) (cdr rem) (cons (if (= i pos) item (car rem)) ret)))))))
