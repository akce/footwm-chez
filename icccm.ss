(library (icccm)
  (export
   init-atoms
   atoms
   atom-ref
   )
  (import
   (prefix (xuser) x.)
   (rnrs base))

  (define atom-list
    '(WM_CLASS
      WM_COMMAND
      WM_NAME
      ))
  (define atoms (x.make-atoms))
  (define init-atoms
    (lambda (d)
      (x.init-atoms d atoms atom-list)))
  (define atom-ref (x.make-atom-ref atoms))
  )
