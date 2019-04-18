;; ftypes (chez-ffi) utility functions.
(library (ftypes-util)
  (export
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
   (chezscheme))

  (define fill-memory/ulongs
    (lambda (memory ulongs)
      (for-each
       (lambda (val offset)
         (foreign-set! 'unsigned-long memory offset val))
       ulongs (map (lambda (i)
                     (* i (ftype-sizeof unsigned-long)))
                   (iota (length ulongs))))))

  ;; [syntax] (fmem ((var varptr type)) ...)
  (define-syntax fmem
    (syntax-rules ()
      [(_ ((var varptr type) ...) first rest ...)
       (let ([var (foreign-alloc (ftype-sizeof type))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (let ([r (begin first rest ...)])
             ;; make-ftype-pointer implicitly locks var, so manually unlock before free.
             (unlock-object var) ...
             (foreign-free var) ...
             r)))]
      [(_ ((var varptr type num) ...) first rest ...)
       ;; Ensure num is at least 1, that's a requirement of foreign-alloc.
       (let ([var (foreign-alloc (* (if (= num 0) 1 num) (ftype-sizeof type)))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (let ([r (begin first rest ...)])
             ;; make-ftype-pointer implicitly locks var, so manually unlock before free.
             (unlock-object var) ...
             (foreign-free var) ...
             r)))]))

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
