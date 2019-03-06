(library (wm)
  (export
   desktop-add
   desktop-delete
   desktop-rename
   atom-ref
   init-atoms
   install)
  (import
   (globals)
   (xlib)
   (prefix (xutil) xutil.)
   (chezscheme))

  (define-values
      (init-atoms atom-ref) (xutil.make-atom-manager '(FOOT_COMMANDV)))

  (define desktop-add
    (lambda (name index)
      (xutil.text-property-set! (root) `("desktop" "insert" ,name ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

  (define desktop-delete
    (lambda (index)
      (xutil.text-property-set! (root) `("desktop" "delete" ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

  (define desktop-rename
    (lambda (index new-name)
      (xutil.text-property-set! (root) `("desktop" "rename" ,(number->string index) ,new-name) (atom-ref 'FOOT_COMMANDV))))

  (define main
    (lambda ()
      (setup)
      (run)
      (cleanup)))

  (define setup
    (lambda ()
      (install)
      #;(install-error-handler)
      #;(import-existing-windows)))

  (define install
    (lambda ()
      (let* ([installed #t]
             [check-bad-access
              (foreign-callable
               (lambda (d xerrorev)
                 (when (fx=? (ftype-ref XErrorEvent (error-code) xerrorev) BadAccess)
                   (set! installed #f))
                 0) (dpy* (* XErrorEvent)) int)])
        ;; Set as window manager.
        (lock-object check-bad-access)
        (let ([orig (XSetErrorHandler (foreign-callable-entry-point check-bad-access))])
          (XSelectInput (current-display) (root) (logor PropertyChange StructureNotify SubstructureRedirect SubstructureNotify))
          (XSync (current-display) #f)
          (XSetErrorHandler orig)
          (unlock-object check-bad-access)
          (unless installed
            (raise (condition (make-error) (make-message-condition "Failed to install. Another WM is running."))))))))

  (define run
    (lambda ()
      (let loop ()
        (loop))))

  (define cleanup
    (lambda ()
      #f)))
