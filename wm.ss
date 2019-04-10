(library (wm)
  (export
   desktop-add-set!
   desktop-delete-set!
   desktop-rename-set!
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
   (prefix (op) op.)
   (prefix (xutil) xutil.)
   (chezscheme))

  (define-values
      (init-atoms atom-ref) (xutil.make-atom-manager '(FOOT_COMMANDV)))

  (define main
    (lambda ()
      (setup)
      (run)
      (cleanup)))

  (define setup
    (lambda ()
      #;(load-config)
      (install-as-wm)
      (install-error-handler)
      (init-desktops)
      (init-windows)
      (op.arrange-windows)))

  ;; Install as *the* window manager.
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
        ;; Set as the window manager client.
        ;; Structure = geometry, border, stacking info for a window.
        ;; Sub = child.
        ;; Redirect = server routes changes as events to the wm client.
        ;; The window manager client (wm) is the only client that can select SubstructureRedirect on the root window.
        ;; Any config change requested by child windows will result in CirculateRequest, ConfigureRequest, or MapRequest
        ;; events being sent to the wm. The wm can then honour, disregard, or modify those requests.
        (lock-object check-bad-access)
        (let ([orig (XSetErrorHandler (foreign-callable-entry-point check-bad-access))]
              [mask
               (bitwise-ior
                PropertyChange
                StructureNotify
                SubstructureRedirect	; Redirect child window change requests to this wm-client.
                SubstructureNotify)])
          (XSelectInput (current-display) (root) mask)
          (XSync (current-display) #f)
          (XSetErrorHandler orig)
          (unlock-object check-bad-access)
          (unless installed
            (raise (condition (make-error) (make-message-condition "Failed to install. Another WM is running."))))))))

  ;; Replace the default error handler with our own. This one ensures that the wm continues to function
  ;; even after the unmap notify handler tries to get wm-state from non-existant window (as happens when a
  ;; window is destroyed).
  (define install-error-handler
    (let ([efunc (foreign-callable
                  (lambda (d xerrorev)
                    (let ([type (ftype-ref XErrorEvent (type) xerrorev)]
                          [wid (ftype-ref XErrorEvent (resourceid) xerrorev)]
                          [error-code (ftype-ref XErrorEvent (error-code) xerrorev)])
                      (display (format "XError: type=~a wid=#x~x error=~d~n" type wid error-code))
                      0)) (dpy* (* XErrorEvent)) int)])
      (lambda ()
        (lock-object efunc)
        (XSetErrorHandler (foreign-callable-entry-point efunc)))))

  (define init-desktops
    (lambda ()
      ;; footwm needs _NET_DESKTOP_NAMES, _NET_NUMBER_OF_DESKTOPS, and _NET_CURRENT_DESKTOP.
      ;; Make sure they exist or create if necessary.
      (unless (ewmh.current-desktop)
          (ewmh.current-desktop-set! 0))
      (unless (ewmh.desktop-names)
          (ewmh.desktop-names-set! '("Unassigned")))
      (unless (ewmh.desktop-count)
          (ewmh.desktop-count-set! 1))))

  (define init-windows
    ;; Import pre-existing windows that need to be managed and then arranges as per initial desktop layout.
    (lambda ()
      (let ([ws (filter icccm.manage-window? (vector->list (xutil.get-child-windows (root))))]
            [defgroup (op.get-unassigned (ewmh.desktop-names))])
        (define wid-exists?
          (lambda (wid)
            (memq wid ws)))
        (define set-join
          (lambda (l1 l2)
            (append l1 (filter (lambda (x) (not (memq x l1))) l2))))
        ;; set WM_STATE & _NET_WM_DESKTOP for each window.
        (for-each
         (lambda (wid)
           (icccm.init-window wid)
           (unless (ewmh.window-desktop wid)
             (ewmh.window-desktop-set! wid defgroup)))
         ws)
        ;; set client-list/stacking ewmh hints.
        (let ([clients (ewmh.client-list)])
          (if (= (length clients) 0)
              (ewmh.client-list-set! ws)
              ;; client list already exists, need to sanitise it with ws.
              (ewmh.client-list-set! (filter wid-exists? (set-join clients ws)))))
        (let ([clients (ewmh.client-list-stacking)])
          (if (= (length clients) 0)
              (ewmh.client-list-stacking-set! ws)
              ;; client list stacking already exists, need to sanitise it with ws.
              (ewmh.client-list-stacking-set! (filter wid-exists? (set-join clients ws))))))))

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
      (let ([wid (xanyevent-wid (xclientmessageevent-xany ev))]
            [type (xclientmessageevent-message-type ev)])
        (display (format "#x~x ClientMessage ~a ~a~n" wid type (xutil.atom-name type)))
        (cond
         [(eq? type (ewmh.atom-ref '_NET_ACTIVE_WINDOW))
          (op.activate-window wid)]
         [(eq? type (ewmh.atom-ref '_NET_CLOSE_WINDOW))
          (icccm.delete-window wid)]
         [(eq? type (ewmh.atom-ref '_NET_CURRENT_DESKTOP))
          (op.desktop-activate (list-ref (xclientmessageevent-data ev) 0))]
         [(eq? type (ewmh.atom-ref '_NET_WM_DESKTOP))
          (op.move-window-to-desktop wid (list-ref (xclientmessageevent-data ev) 0))]
         [(eq? type (icccm.atom-ref 'WM_CHANGE_STATE))
          (if (eq? (list-ref (xclientmessageevent-data ev) 0) icccm.IconicState)
              (op.banish-window wid))]
         [else
          (display (format "#x~x Unknown ClientMessage message type~n" wid))]))))

  (define on-configure
    (lambda (ev)
      ;; For Notify events, always ignore those generated by SubstructureRedirects.
      (if (= (xconfigureevent-wid ev) (xanyevent-wid (xconfigureevent-xany ev)))
          (display (format "#x~x ConfigureNotify\n" (xconfigureevent-wid ev))))))

  (define on-configure-request
    (lambda (ev)
      (icccm.on-configure-request ev)))

  (define on-create-window
    (lambda (ev)
      (display (format "#x~x CreateNotify~n" (xcreatewindowevent-wid ev)))
      (icccm.on-create-window ev)))

  (define on-destroy-window
    (lambda (ev)
      ;; For Notify events, always ignore those generated by SubstructureRedirects.
      (if (= (xdestroywindowevent-wid ev) (xanyevent-wid (xdestroywindowevent-xany ev)))
          (begin
            (display (format "#x~x DestroyNotify~n" (xdestroywindowevent-wid ev)))
            (ewmh.remove-window (xdestroywindowevent-wid ev))
            (op.arrange-windows)))))

  (define on-map
    (lambda (ev)
      ;; For Notify events, always ignore those generated by SubstructureRedirects.
      (if (= (xmapevent-wid ev) (xanyevent-wid (xmapevent-xany ev)))
          (display (format "#x~x MapNotify~n" (xmapevent-wid ev))))))

  (define on-map-request
    (lambda (ev)
      (let ([wid (xmaprequestevent-wid ev)])
        (display (format "#x~x MapRequest ~a~n" wid (op.window-name wid))))
      (icccm.on-map-request ev)
      (ewmh.on-map-request ev)
      (op.arrange-windows)))

  (define on-property
    (lambda (ev)
      (let ([wid (xanyevent-wid (xpropertyevent-xany ev))]
            [atom (xpropertyevent-propatom ev)])
        (display (format "#x~x PropertyNotify ~a~n" wid (xutil.atom-name atom)))
        (cond
         [(eq? atom (atom-ref 'FOOT_COMMANDV))
          (let ([cmd (xutil.property->string* wid (atom-ref 'FOOT_COMMANDV))])
            (if (string=? (vector-ref cmd 0) "desktop")
              (cond
               [(string=? (vector-ref cmd 1) "insert")
                (op.desktop-insert (vector-ref cmd 2) (string->number (vector-ref cmd 3)))]
               [(string=? (vector-ref cmd 1) "delete")
                (op.desktop-delete (string->number (vector-ref cmd 2)))]
               [(string=? (vector-ref cmd 1) "rename")
                (op.desktop-rename (string->number (vector-ref cmd 2)) (vector-ref cmd 3))])))]))))

  (define on-unmap
    (lambda (ev)
      ;; Only action non-redirect events as we subscribe to both root-substructure & child structure notify
      ;; (in order to get child window destroy events).
      (if (= (xunmapevent-wid ev) (xanyevent-wid (xunmapevent-xany ev)))
          ;; Window has been unmapped/hidden.
          ;; This could be a prelude to deletion, or it could be the window is just going iconic.
          ;; Check wm-state:
          ;; - ICONIC (the window is hidden, do nothing unless it was visible)
          ;; - WITHDRAWN (the window is being removed, remove it from EWMH hints).
          #;(icccm.on-unmap ev)	;; transitions WM_STATE::NORMAL -> WITHDRAWN
          (let* ([wid (xunmapevent-wid ev)]
                 [state (icccm.get-wm-state wid)])
            (display (format "#x~x UnmapNotify state=~a\n" wid state))
            (if (or (not state) (not (eq? state 'ICONIC)))
                (begin
                  (display (format "#x~x removing window from EWMH client lists~n" wid))
                  (ewmh.remove-window wid)))
            (op.arrange-windows)))))

  (define desktop-add-set!
    (lambda (name index)
      (xutil.text-property-set! (root) `("desktop" "insert" ,name ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

  (define desktop-delete-set!
    (lambda (index)
      (xutil.text-property-set! (root) `("desktop" "delete" ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

  (define desktop-rename-set!
    (lambda (index new-name)
      (xutil.text-property-set! (root) `("desktop" "rename" ,(number->string index) ,new-name) (atom-ref 'FOOT_COMMANDV))))

  (define cleanup
    (lambda ()
      #f)))
