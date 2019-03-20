(library (wm)
  (export
   desktop-add
   desktop-delete
   desktop-rename
   atom-ref
   init-atoms
   init-desktops
   init-windows
   main)
  (import
   (globals)
   (util)
   (xlib)
   (prefix (ewmh) ewmh.)
   (prefix (icccm) icccm.)
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
      (unless (ewmh.current-desktop)
          (ewmh.current-desktop-set! 0))
      (unless (ewmh.desktop-names)
          (ewmh.desktop-names-set! '("Unassigned")))
      (unless (ewmh.desktop-count)
          (ewmh.desktop-count-set! 1))
      (XSync (current-display) #f)))

  (define init-windows
    ;; Import pre-existing windows that need to be managed and then arranges as per initial desktop layout.
    (lambda ()
      (let ([mws (list->vector (filter icccm.manage-window? (vector->list (xutil.get-child-windows (root)))))]) ; managed wid list
        ;; set WM_STATE & _NET_WM_DESKTOP for each window.
        (vector-for-each
         (lambda (wid)
           (icccm.init-window wid)
           (unless (ewmh.window-desktop wid)
             (ewmh.window-desktop-set! wid 0)))
         mws)
        ;; set client-list/stacking ewmh hints.
        (unless (ewmh.client-list)
          (ewmh.client-list-set! mws))
        (unless (ewmh.client-list-stacking)
          (ewmh.client-list-stacking-set! mws))
        (XSync (current-display) #f))))

  (define run
    (lambda ()
      (let loop ()
        (let ([ev (xutil.get-next-event)])
          (cond
           ((xclientmessageevent? ev)		(on-client-message ev))
           ((xconfigureevent? ev)		(on-configure ev))
           ((xconfigurerequestevent? ev)	(on-configure-request ev))
           ((xcreatewindowevent? ev)		(on-create-window ev))
           ((xdestroywindowevent? ev)		(on-destroy-window ev))
           ((xmapevent? ev)			(on-map ev))
           ((xmaprequestevent? ev)		(on-map-request ev))
           ((xpropertyevent? ev)		(on-property ev))
           ((xunmapevent? ev)			(on-unmap ev))
           (else
	    (display (format "Unknown event ~d~n" (xanyevent-type ev))))))
        (loop))))

  (define on-client-message
    (lambda (ev)
      (display "XClientMessageEvent\n")))

  (define on-configure
    (lambda (ev)
      (display "XConfigureEvent\n")))

  (define on-configure-request
    (lambda (ev)
      (icccm.on-configure-request ev)))

  (define on-create-window
    (lambda (ev)
      (display "XCreateWindowEvent\n")
      (icccm.on-create-window ev)))

  (define on-destroy-window
    (lambda (ev)
      (display "XDestroyWindowEvent\n")))

  (define on-map
    (lambda (ev)
      (display "XMapEvent\n")))

  (define on-map-request
    (lambda (ev)
      (display "XMapRequestEvent\n")))

  (define on-property
    (lambda (ev)
      (display (format "XPropertyEvent ~a~n" (xutil.atom-name (xpropertyevent-propatom ev))))))

  (define on-unmap
    (lambda (ev)
      (display "XUnmapEvent\n")))

  (define cleanup
    (lambda ()
      #f)))
