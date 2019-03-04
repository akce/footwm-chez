(library (globals)
  (export
   current-display
   root)
  (import
   (rnrs base)
   (only (chezscheme) make-parameter))

  (define current-display
    (make-parameter #f))
  (define root
    (make-parameter #f))
)
