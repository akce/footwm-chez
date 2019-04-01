(library (util)
  (export
   bitmap
   enum
   fmem)
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
             r)))])))
