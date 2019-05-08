(library (util)
  (export
   bitmap
   case-equal?
   enum
   list-combinations
   list-combinations*
   list-find-index
   list-insert
   list-replace
   remove*
   vector-enumerate)
  (import
   (rnrs)
   (only (chezscheme) add1 foreign-alloc foreign-free ftype-sizeof fxsll iota make-ftype-pointer sub1))

  (define-syntax bitmap
    (syntax-rules ()
      [(_ name (symbol bit) ...)
       (begin (define symbol (fxsll 1 bit)) ...)]))

  ;; [syntax] case-equal?: like case but compares using equal?.
  (define-syntax case-equal?
    (syntax-rules (else)
      [(_ var (val body ...) ... (else bodye ...))
       (let ([v var])
         (cond
          ((equal? v val) body ...) ...
          (else bodye ...)))]
      [(_ var (val body ...) ...)
       (let ([v var])
         (cond
          ((equal? v val) body ...) ...))]))

  (define-syntax enum
    (syntax-rules ()
      [(_ name (symbol value) ...)
       (begin (define symbol value) ...)]))

  (define list-combinations
    (lambda (lst size)
      (let loop ((part lst) (sz size))
        (cond
         [(= sz 0) '(())]
         [(null? part) '()]
         [else
          (append
           (map
            (lambda (x)
              (cons (car part) x))
            (loop (cdr part) (sub1 sz)))
           (loop (cdr part) sz))]))))

  (define list-combinations*
    (lambda (lst)
      (apply
       append
       (map
        (lambda (size)
          (list-combinations lst size))
        (iota (add1 (length lst)))))))

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
            (loop (add1 i) (cdr rem) (cons (if (= i pos) item (car rem)) ret))))))

  ;; [proc] remove* like remove but can remove mulitple items.
  (define remove*
    (lambda (lst . items)
      (let loop ((ret lst) (i items))
        (cond
         [(null? i) ret]
         [else
          (loop (remove (car i) ret) (cdr i))]))))

  (define vector-enumerate
    (lambda (v)
      (let* ([len (vector-length v)]
             [res (make-vector len)])
        (let loop ((i 0))
          (cond
           [(fx=? i len) res]
           [else
             (vector-set! res i i)
             (loop (fx+ i 1))]))))))
