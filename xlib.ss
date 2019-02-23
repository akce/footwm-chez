(library (xlib)
  (export open-display close-display)
  (import (chezscheme))

  (define library-init
    (load-shared-object "libX11.so"))

  (define-syntax proc
    (syntax-rules ()
      [(_ libsym exportsym args return)
       (define exportsym (foreign-procedure (symbol->string 'libsym) args return))]))

  (proc XOpenDisplay	open-display	(string)	void*)
  (proc XCloseDisplay	close-display	(void*)		int))
