(library (xlib)
  (export open-display
          close-display
          default-root
          set-atom!
          atom-name
          free
          void*->string
          window-property-u32
          window-property

          get-text-property
          text-property-utf8

          XA-WINDOW
          )
  (import (chezscheme))

  (define library-init
    (load-shared-object "libX11.so"))

  (define-syntax proc
    (syntax-rules ()
      [(_ libsym exportsym args return)
       (define exportsym (foreign-procedure (symbol->string 'libsym) args return))]))

  ;; type aliases.
  (define-ftype dpy* void*)
  (define-ftype window unsigned-32)
  (define-ftype atom unsigned-32)
  (define-ftype status unsigned-32)

  (define-ftype u8 unsigned-8)
  (define-ftype u8* (* u8))

  ;; X atoms from Xatom.h
  (define XA-WINDOW 33)

  (define (void*->string fptr)
    (utf8->string
     (let f ([i 0])
       (let ([c (foreign-ref 'unsigned-8 fptr i)])
         (if (fx= c 0)
             (make-bytevector i)
             (let ([bv (f (fx+ i 1))])
               (bytevector-u8-set! bv i c)
               bv))))))
  (define atom-name
    (lambda (d a)
      (let* ([ptr ((foreign-procedure "XGetAtomName" (dpy* atom) void*) d a)]
             [str (void*->string ptr)])
        (free ptr)
        str)))
  (proc XInternAtom set-atom! (dpy* string boolean) atom)
  (proc XFree free (void*) void)

  ;; [syntax] (fmem ((var varptr type)) ...)
  ;; a let that handles common foreign memory pointer alloc/free.
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

  (define void*->u32
    (lambda (ptr len)
      (do ([i 0 (+ i 1)]
           [v (make-vector len) (begin
                                  (vector-set! v i (foreign-ref 'unsigned-32 ptr (* i (ftype-sizeof unsigned-32))))
                                  v)])
          ((= i len) v))))

  (define window-property-u32
    (lambda (d wid propatom atomtype)
      (fmem ([atr &atr atom]		;; atr = actual type return
             [afr &afr integer-32]	;; afr = actual format return
             [nir &nir unsigned-long]	;; nir = number of items return
             [bar &bar unsigned-long]	;; bar = bytes after return
             [pr  &pr u8*])		;; pr  = property return
            (let ([rc (window-property d wid propatom 0 2048 #f atomtype &atr &afr &nir &bar &pr)])
              (if (= rc 0)
                  ;; success: extract window ids from pr.
                  (let* ([pr* (foreign-ref 'void* pr 0)]
                         [nums (void*->u32 pr* (foreign-ref 'unsigned-long nir 0))])
                    (free pr*)
                    nums)
                  ;; failure: return empty list.
                  (list))))))

  (define text-property-utf8
    (lambda (d wid propatom)
      (fmem ([tp &tp text-property-type])
            (let ([rc (get-text-property d wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (let* (#;[enc (ftype-ref text-property-type (encoding) &tp)]
                         #;[num (ftype-ref text-property-type (nitems) &tp)]
                         [addr (ftype-pointer-address (ftype-ref text-property-type (value) &tp))]
                         [str (void*->string addr)])
                    #;(display (format "encoding ~d:~s nitems ~d ~n" enc (atom-name d enc) num))
                    (free addr)
                    str)
                  #f)
              ))))

  (define-ftype text-property-type
    (struct
     [value	u8*]
     [encoding	atom]
     [format	integer-32]
     [nitems	unsigned-long]))

  (proc XGetTextProperty get-text-property (dpy* window (* text-property-type) atom) status)

  (proc XGetWindowProperty window-property (dpy* window atom long long boolean atom (* atom) (* integer-32) (* unsigned-long) (* unsigned-long) (* u8*)) int)
  (proc XOpenDisplay	open-display	(string)	dpy*)
  (proc XCloseDisplay	close-display	(dpy*)		int)
  (proc XDefaultRootWindow default-root (dpy*)		window))
