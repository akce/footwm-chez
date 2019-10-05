(library (footwm globals)
  (export
   current-display
   root
   config-path)
  (import
   (rnrs base)
   (only (chezscheme) getenv make-parameter))

  (define current-display
    (make-parameter #f))
  (define root
    (make-parameter #f))

  (define config-path
    (make-parameter
      (string-append (getenv "HOME") "/" ".foot"))))
