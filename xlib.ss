(library (xlib)
  (export open-display
          close-display
          set-atom!
          atom-name
          free
          void*->string)
  (import (chezscheme))

  (define library-init
    (load-shared-object "libX11.so"))

  (define-syntax proc
    (syntax-rules ()
      [(_ libsym exportsym args return)
       (define exportsym (foreign-procedure (symbol->string 'libsym) args return))]))

  (define-ftype dpy* void*)
  (define-ftype window unsigned-32)
  (define-ftype atom unsigned-32)

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
  (proc XOpenDisplay	open-display	(string)	dpy*)
  (proc XCloseDisplay	close-display	(dpy*)		int))
