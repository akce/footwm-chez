(library (xlib)
  (export open-display
          close-display
          default-root
          set-atom!
          atom-name
          free
          void*->string
          window-ids
          window-property)
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

  (define-ftype u8 unsigned-8)
  (define-ftype u8* (* u8))

  ;; X atoms from Xatom.h
  (define XA/WINDOW 33)

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

  (define window-ids
    (lambda (d wid a)
      (fmem ([atr &atr atom]		;; atr = actual type return
             [afr &afr integer-32]	;; afr = actual format return
             [nir &nir unsigned-long]	;; nir = number of items return
             [bar &bar unsigned-long]	;; bar = bytes after return
             [pr  &pr u8*])		;; pr  = property return
            (let ([rc (window-property d wid a 0 2048 #f XA/WINDOW &atr &afr &nir &bar &pr)])
              (if (= rc 0)
                  ;; success: extract window ids from pr.
                  (let* ([pr* (foreign-ref 'void* pr 0)]
                         [vlen (foreign-ref 'unsigned-long nir 0)]
                         [sz-u32 (ftype-sizeof unsigned-32)]
                         [ids (do ([i 0 (+ i 1)]
                                   [v (make-vector vlen) (begin
                                                           (vector-set! v i (foreign-ref 'unsigned-32 pr* (* i sz-u32)))
                                                           v)])
                                  ((= i vlen) v))])
                    (free pr*)
                    ids)
                  ;; failure: return empty list.
                  (list))))))

  (proc XGetWindowProperty window-property (dpy* window atom long long boolean atom (* atom) (* integer-32) (* unsigned-long) (* unsigned-long) (* u8*)) int)
  (proc XOpenDisplay	open-display	(string)	dpy*)
  (proc XCloseDisplay	close-display	(dpy*)		int)
  (proc XDefaultRootWindow default-root (dpy*)		window))
