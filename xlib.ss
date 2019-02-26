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

  (define window-ids
    (lambda (d wid a)
      (let ([atr (foreign-alloc (ftype-sizeof atom))]		;; atr = actual type return
            [afr (foreign-alloc (ftype-sizeof integer-32))]	;; afr = actual format return
            [nir (foreign-alloc (ftype-sizeof unsigned-long))]	;; nir = number of items return
            [bar (foreign-alloc (ftype-sizeof unsigned-long))]	;; bar = bytes after return
            [pr  (foreign-alloc (ftype-sizeof u8*))])		;; pr  = property return
        (let ([&atr (make-ftype-pointer atom atr)]	;; ftype-pointers for foreign-alloc'd items
              [&afr (make-ftype-pointer integer-32 afr)]
              [&nir (make-ftype-pointer unsigned-long nir)]
              [&bar (make-ftype-pointer unsigned-long bar)]
              [&pr  (make-ftype-pointer u8* pr)])
          (let ([ret (window-property d wid a 0 2048 #f XA/WINDOW &atr &afr &nir &bar &pr)])
            (when (= ret 0)
                ;; success: extract window ids from pr.
              (let* ([pr* (foreign-ref 'void* pr 0)]
                     [vlen (foreign-ref 'unsigned-long nir 0)]
                     [sz-u32 (ftype-sizeof unsigned-32)]
                     [ids (do ([i 0 (+ i 1)]
                               [v (make-vector vlen) (begin
                                                       (vector-set! v i (foreign-ref 'unsigned-32 pr* (* i sz-u32)))
                                                       v)])
                              ((= i vlen) v)
                            #;(display (format "win: ~d~n" (foreign-ref 'unsigned-32 pr* (* i sz-u32)))))])
                (free pr*)
                (foreign-free atr)
                (foreign-free afr)
                (foreign-free nir)
                (foreign-free bar)
                (foreign-free pr)
                ids)))))))


  (proc XGetWindowProperty window-property (dpy* window atom long long boolean atom (* atom) (* integer-32) (* unsigned-long) (* unsigned-long) (* u8*)) int)
  (proc XOpenDisplay	open-display	(string)	dpy*)
  (proc XCloseDisplay	close-display	(dpy*)		int)
  (proc XDefaultRootWindow default-root (dpy*)		window))
