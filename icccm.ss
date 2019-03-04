(library (icccm)
  (export
   init-atoms
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
   (rnrs base)
   (only (chezscheme) define-values))

  (define atom-list
    '(WM_CLASS
      WM_CLIENT_MACHINE
      WM_COMMAND
      WM_NAME
      ))
  (define-values
      (init-atoms atom-ref) (xutil.make-atom-manager atom-list))

  (define class-hint
    (lambda (wid)
      ;; This should use XGetClassHint but class hints are just two strings so
      ;; I'll save myself the hassle for now and use the text property stuff I've already got.
      (xutil.property->string* wid (atom-ref 'WM_CLASS))))

  (define class
    (lambda (wid)
      (vector-ref (class-hint wid) 1)))

  (define resource
    (lambda (wid)
      (vector-ref (class-hint wid) 0)))

  (define client-machine
    (lambda (wid)
      (xutil.property->string wid (atom-ref 'WM_CLIENT_MACHINE))))

  (define command
    (lambda (wid)
      (xutil.property->string* wid (atom-ref 'WM_COMMAND))))

  (define name
    (lambda (wid)
      (xutil.property->string wid (atom-ref 'WM_NAME))))
  )
