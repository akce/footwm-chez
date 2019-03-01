(library (xutil)
  (export
   atom-name
   open
   property->string
   property->string*
   property->u32*

   make-atoms
   init-atoms
   make-atom-ref
   )
  (import (chezscheme)
          (xlib))

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
             r)))]))

  (define atom-name
    (lambda (d a)
      (let* ([ptr (XGetAtomName d a)]
             [str (ptr->string ptr)])
        (XFree ptr)
        str)))

  ;; wraps XOpenDisplay so the connection string is optional.
  (define open
    (case-lambda
     [() (open #f)]
     [(s) (XOpenDisplay s)]))

   ;;;; basic atom manager.
  ;; Just a very thin wrapper around hash tables.
  (define make-atoms make-eq-hashtable)

  ;; initialise atoms.
  ;; For those atoms that require display so can only be initialised after a connection to X is made.
  (define init-atoms
    (lambda (d atoms atom-list)
      (for-each
       (lambda (a)
         (hashtable-set! atoms a (XInternAtom d (symbol->string a) #f)))
       atom-list)))

  (define make-atom-ref
    (lambda (atoms)
      (lambda (a)
        (hashtable-ref atoms a #f))))

  (define ptr->utf8s
    (lambda (text-list nitems)
      (let ([n (foreign-ref 'integer-32 nitems 0)]
            [strvect (foreign-ref 'void* text-list 0)]	;; strvect = vector of strings (utf8**)
            [sz (ftype-sizeof void*)])
        (do ([i 0 (+ i 1)]
             [v (make-vector n)
                (let ([saddr (foreign-ref 'void* strvect (* i sz))])
                  (vector-set! v i (ptr->string saddr))
                  v)])
            ;; TODO consider limiting to n-1 since the last string always seems to be "".
            ((= i n) v)))))

  (define property->string
    (lambda (d wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (XGetTextProperty d wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (let* (#;[enc (ftype-ref XTextProperty (encoding) &tp)]
                         #;[num (ftype-ref XTextProperty (nitems) &tp)]
                         [addr (ftype-pointer-address (ftype-ref XTextProperty (value) &tp))]
                         [str (ptr->string addr)])
                    #;(display (format "encoding ~d:~s nitems ~d ~n" enc (atom-name d enc) num))
                    (XFree addr)
                    str)
                  #f)))))

  (define property->string*
    (lambda (d wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (XGetTextProperty d wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (fmem ([nitems &nitems integer-32]
                         [text-list &text-list u8**])
                        (let* ([stat (Xutf8TextPropertyToTextList d &tp &text-list &nitems)]
                               [res (ptr->utf8s text-list nitems)])
                          ;; TODO text-list foreign-ref is also calc'd in text-list->utf8s. Need to re-org.
                          (XFreeStringList (foreign-ref 'void* text-list 0))
                          res))
                  #f)))))

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
  (define ptr->u32*
    (lambda (ptr len)
      (do ([i 0 (+ i 1)]
           [v (make-vector len) (begin
                                  (vector-set! v i (foreign-ref 'unsigned-32 ptr (* i (ftype-sizeof unsigned-32))))
                                  v)])
          ((= i len) v))))

  ;; window property to vector of u32's.
  (define property->u32*
    (lambda (d wid propatom atomtype)
      (fmem ([atr &atr atom]		;; atr = actual type return
             [afr &afr integer-32]	;; afr = actual format return
             [nir &nir unsigned-long]	;; nir = number of items return
             [bar &bar unsigned-long]	;; bar = bytes after return
             [pr  &pr u8*])		;; pr  = property return
            (let ([rc (XGetWindowProperty d wid propatom 0 2048 #f atomtype &atr &afr &nir &bar &pr)])
              (if (= rc 0)
                  ;; success: extract window ids from pr.
                  (let* ([pr* (foreign-ref 'void* pr 0)]
                         [nums (ptr->u32* pr* (foreign-ref 'unsigned-long nir 0))])
                    (XFree pr*)
                    nums)
                  ;; failure: return empty list.
                  (list))))))
)
