(library (icccm)
  (export
   init-atoms
   atoms
   atom-ref
   )
  (import
   (prefix (xutil) xutil.)
   (rnrs base))

  (define atom-list
    '(WM_CLASS
      WM_COMMAND
      WM_NAME
      ))
  (define atoms (xutil.make-atoms))
  (define init-atoms
    (lambda (d)
      (xutil.init-atoms d atoms atom-list)))
  (define atom-ref (xutil.make-atom-ref atoms))
  )
