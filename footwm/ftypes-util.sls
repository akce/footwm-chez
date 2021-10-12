;; ftypes (chez-ffi) utility functions.
;;
;; Written by Jerry 2019-2021.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm ftypes-util)
  (export
   c-function
   c-default-function
   fill-memory/ulongs
   fmem
   free/u8**
   ptr->string
   ptr->ulongs
   strdup
   str*->u8**
   ptr->utf8s
   void*-cast)
  (import
   (chezscheme)
   (footwm util))

  (meta define symbol/kebab-case->pascal-case
        (lambda (stx)
          (datum->syntax stx
            (kebab-case->pascal-case (symbol->string (syntax->datum stx))))))

  ;; [syntax] c-function: converts scheme-like function names to c-like function names before passing to foreign-procedure.
  ;; ie, word separating hyphens are converted to underscores for c.
  ;; eg,
  ;; (c-function (str-length (string) int) ....)
  ;; is converted to:
  ;; (begin
  ;;   (define str-length (foreign-procedure "str_length" (string) int))
  ;;   ...)
  (define-syntax c-function
    (lambda (stx)
      (syntax-case stx ()
        [(_ (name args return) ...)
         (with-syntax ([(function-string ...)
                        (map symbol/kebab-case->pascal-case #'(name ...))])
            #'(begin
                (define name
                  (foreign-procedure function-string args return)) ...))])))

  ;; [syntax] c-default-function: define c functions that take a default argument.
  ;; This behaves like c-function, except it first takes a (type, instance) pair.
  ;; c-default-function is useful for those c modules that define a bunch of functions that take
  ;; the same struct as the first argument.
  ;;
  ;; The expansion of this definition:
  ;; (c-default-function (type (current-parameter))
  ;;   (func-name1 (arg1) int)
  ;;   ...)
  ;; will look like:
  ;; (begin
  ;;   (define func-name1
  ;;     (let ([ffi-func (foreign-procedure "func_name1" (type arg1) int)])
  ;;       (lambda args (apply ffi-func (current-parameter) args))))
  ;;   ...)
  (define-syntax c-default-function
    (lambda (stx)
      (syntax-case stx ()
        [(_ (type instance) (name (arg ...) return) ...)
         (with-syntax ([(function-string ...)
                        (map symbol/kebab-case->pascal-case #'(name ...))])
            #'(begin
                (define name
                  (let ([ffi-func (foreign-procedure function-string (type arg ...) return)])
                    (lambda args
                      (apply ffi-func instance args)))) ...))])))

  (define fill-memory/ulongs
    (lambda (memory ulongs)
      (for-each
       (lambda (val offset)
         (foreign-set! 'unsigned-long memory offset val))
       ulongs (map (lambda (i)
                     (* i (ftype-sizeof unsigned-long)))
                   (iota (length ulongs))))))

  ;; [syntax] (fmem ((var type)) ...)
  ;; [syntax] (fmem ((var varptr type [count])) ...)
  (define-syntax fmem
    (syntax-rules ()
      [(_ ((var type) ...) first rest ...)
       (let ([var (foreign-alloc (ftype-sizeof type))] ...)
         (dynamic-wind
           (lambda () #t)
           (lambda ()
             first rest ...)
           (lambda ()
             (foreign-free var) ...)))]
      [(_ ((var varptr type . count) ...) first rest ...)
       (let ([var (foreign-alloc (* (parse-alloc-count count) (ftype-sizeof type)))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (dynamic-wind
             (lambda () #t)
             (lambda ()
               first rest ...)
             (lambda ()
               (foreign-free var) ...))))]))

  ;; parse, or rather destructure, the optional alloc count parameter.
  (define-syntax parse-alloc-count
    (syntax-rules ()
      [(_ ())
       1]
      [(_ (count))
       (and (identifier? #'count) (integer? #'count) (> #'count 0))
       count]
      [(_ (count))
       ;; Hope that the 'count' identifier is a var or function call referring to a positive int.
       count]
      ))

  (define free/u8**
    (lambda (u8** len)
      ;; free individual string pointers.
      (for-each
       (lambda (i)
         (foreign-free (foreign-ref 'void* u8** (* i (ftype-sizeof void*)))))
       (iota len))
      ;; free containing block.
      (foreign-free u8**)))

  (define (ptr->string fptr)
      (utf8->string
       (let f ([i 0])
         (let ([c (foreign-ref 'unsigned-8 fptr i)])
           (if (fx= c 0)
               (make-bytevector i)
               (let ([bv (f (fx+ i 1))])
                 (bytevector-u8-set! bv i c)
                 bv))))))

  ;; internal: extract numbers from pointer location.
  (define ptr->ulongs
    (lambda (ptr len)
      (do ([i 0 (+ i 1)]
           [v (make-vector len) (begin
                                  (vector-set! v i (foreign-ref 'unsigned-long ptr (* i (ftype-sizeof unsigned-long))))
                                  v)])
          ((= i len) (vector->list v)))))

  (define strdup
    (lambda (str)
      ;; foreign-alloc every string and copy in the bytes.
      (let* ([bv (string->utf8 str)]
             [len (bytevector-length bv)])
        (let ([ret
               (do ([i 0 (fx+ i 1)]
                    [fv (foreign-alloc (fx+ 1 len))
                        (begin
                          (foreign-set! 'unsigned-8 fv i (bytevector-u8-ref bv i))
                          fv)])
                   ((= i len) fv))])
          (foreign-set! 'unsigned-8 ret len 0)	;; null terminate.
          ret))))

  (define str*->u8**
    (lambda (str*)
      (let ([len (length str*)])
        (do ([i 0 (+ i 1)]
             [v (foreign-alloc (* len (ftype-sizeof void*)))
                (let ([fstr (strdup (list-ref str* i))])
                  (foreign-set! 'void* v (* i (ftype-sizeof void*)) fstr)
                  v)])
            ((= i len) v)))))

  (define ptr->utf8s
    (lambda (text-list nitems)
      (let ([n (foreign-ref 'int nitems 0)]
            [strvect (foreign-ref 'void* text-list 0)]	;; strvect = vector of strings (utf8**)
            [sz (ftype-sizeof void*)])
        (do ([i 0 (+ i 1)]
             [v (make-vector n)
                (let ([saddr (foreign-ref 'void* strvect (* i sz))])
                  (vector-set! v i (ptr->string saddr))
                  v)])
            ;; TODO consider limiting to n-1 since the last string always seems to be "".
            ((= i n) (vector->list v))))))

  ;; return an ftype-pointer of void* type for ftype-pointer fptr.
  (define void*-cast
    (lambda (fptr)
      (make-ftype-pointer void* (ftype-pointer-address fptr)))))
