(library (icccm)
  (export
   init-atoms
   atoms
   atom-ref

   class
   class-hint
   resource

   client-machine
   command
   name
   )
  (import
   (prefix (xutil) xutil.)
   (rnrs base))

  (define atom-list
    '(WM_CLASS
      WM_CLIENT_MACHINE
      WM_COMMAND
      WM_NAME
      ))
  (define atoms (xutil.make-atoms))
  (define init-atoms
    (lambda (d)
      (xutil.init-atoms d atoms atom-list)))
  (define atom-ref (xutil.make-atom-ref atoms))

  (define class-hint
    (lambda (d wid)
      ;; This should use XGetClassHint but class hints are just two strings so
      ;; I'll save myself the hassle for now and use the text property stuff I've already got.
      (xutil.text-property->utf8s d wid (atom-ref 'WM_CLASS))))

  (define class
    (lambda (d wid)
      (vector-ref (class-hint d wid) 1)))

  (define resource
    (lambda (d wid)
      (vector-ref (class-hint d wid) 0)))

  (define client-machine
    (lambda (d wid)
      (xutil.text-property->utf8 d wid (atom-ref 'WM_CLIENT_MACHINE))))

  (define command
    (lambda (d wid)
      (xutil.text-property->utf8s d wid (atom-ref 'WM_COMMAND))))

  (define name
    (lambda (d wid)
      (xutil.text-property->utf8 d wid (atom-ref 'WM_NAME))))
  )
