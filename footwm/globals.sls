(library (footwm globals)
  (export
   config-path)
  (import
   (rnrs base)
   (only (chezscheme) getenv make-parameter))

  (define config-path
    (make-parameter
      (string-append (getenv "HOME") "/" ".foot"))))
