(library (wm)
  (export
   desktop-add
   desktop-delete
   desktop-rename
   atom-ref
   init-atoms
   init-desktops
   init-windows)
  (import
   (globals)
   (xlib)
   (prefix (ewmh) ewmh.)
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
      #;(load-config)
      (install-as-wm)
      #;(install-error-handler)
      (init-desktops)
      (init-windows)))

  ;; Install ourselves as *the* window manager.
  ;; raises an error condition on failure.
  (define install-as-wm
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

  (define init-desktops
    (lambda ()
      ;; footwm needs _NET_DESKTOP_NAMES, _NET_NUMBER_OF_DESKTOPS, and _NET_CURRENT_DESKTOP.
      ;; Make sure they exist or create if necessary.
      (if (not (ewmh.current-desktop))
          (ewmh.current-desktop-set! 0))
      (if (not (ewmh.desktop-names))
          (ewmh.desktop-names-set! '("Unassigned")))
      (if (not (ewmh.desktop-count))
          (ewmh.desktop-count-set! 1))
      (XSync (current-display) #f)))

  (define init-windows
    (lambda ()
      ;; Import pre-existing windows that need to be managed and then arranges as per initial desktop layout.
      #f))

  (define run
    (lambda ()
      (let loop ()
        (loop))))

  (define cleanup
    (lambda ()
      #f)))
