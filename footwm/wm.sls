(library (footwm wm)
  (export
   init-desktops
   init-windows
   main)
  (import
   (rnrs)
   (only (chezscheme) format)
   (footwm globals)
   (only (footwm util) case-equal?)
   (footwm xlib)
   (prefix (footwm ewmh) ewmh.)
   (prefix (footwm hints) hints.)
   (prefix (footwm icccm) icccm.)
   (prefix (footwm op) op.)
   (prefix (footwm xutil) xutil.))

  (define main
    (lambda ()
      (install-as-wm)
      ;; Replace the default error handler with our own. This ensures that the wm continues to function even
      ;; after an error. eg, when trying to access a resource from a destroyed window.
      (xutil.install-default-error-handler)
      (init-desktops)
      (init-windows)
      (op.arrange-windows)
      (run)))

  ;; Install as *the* window manager.
  ;; raises an error condition on failure.
  (define install-as-wm
    (lambda ()
      ;; The window manager client (wm) is the only client that can select SubstructureRedirect on the root window.
      ;; Any config change requested by directo child windows will result in CirculateRequest, ConfigureRequest,
      ;; or MapRequest events being sent to the wm. The wm can then honour, disregard, or modify those requests.
      (let* ([installed #t]
             [orig (xutil.install-error-handler
                    (lambda (d ev)
                      (if (eq? (xerrorevent-error-code ev) BadAccess)
                        (set! installed #f))))]
             [mask
              (bitwise-ior
               PropertyChange
               StructureNotify		; Structure = geometry, border, stacking info for a window.
               SubstructureNotify	; Sub = structure notify for child windows.
               SubstructureRedirect)])	; Redirect child window change requests to this client.
          (xutil.select-input (root) mask)
          (xutil.sync)
          (xutil.install-error-handler orig)
          (unless installed
            (raise (condition (make-error) (make-message-condition "Failed to install. Another WM is running.")))))))

  (define init-desktops
    (lambda ()
      ;; footwm needs _NET_DESKTOP_NAMES, _NET_NUMBER_OF_DESKTOPS, and _NET_CURRENT_DESKTOP.
      ;; Make sure they exist or create if necessary.
      (unless (ewmh.current-desktop)
          (ewmh.current-desktop-set! 0))
      (if (null? (ewmh.desktop-names))
          (ewmh.desktop-names-set! '("Unassigned")))
      (unless (ewmh.desktop-count)
          (ewmh.desktop-count-set! 1))))

  (define init-windows
    ;; Import pre-existing windows that need to be managed and then arranges as per initial desktop layout.
    (lambda ()
      (let* ([allwids (xutil.get-child-windows)]
             [ws (filter op.manage-window? allwids)]
             [defgroup (op.get-unassigned (ewmh.desktop-names))])
        (define wid-exists?
          (lambda (wid)
            (memq wid ws)))
        (define set-join
          (lambda (l1 l2)
            (append l1 (filter (lambda (x) (not (memq x l1))) l2))))
        (ewmh.calculate-workarea allwids)
        ;; set WM_STATE & _NET_WM_DESKTOP for each window.
        (for-each
         (lambda (wid)
           (icccm.init-window wid)
           (unless (ewmh.window-desktop wid)
             (ewmh.window-desktop-set! wid defgroup)))
         ws)
        ;; set client-list ewmh hints.
        (let ([clients (ewmh.client-list)])
          (if (null? clients)
              (ewmh.client-list-set! ws)
              ;; client list already exists, need to sanitise it with ws.
              (ewmh.client-list-set! (filter wid-exists? (set-join clients ws))))))))

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
        (case-equal? type
         [(ewmh.atom-ref '_NET_ACTIVE_WINDOW)
          (op.activate-window wid)]
         [(ewmh.atom-ref '_NET_CLOSE_WINDOW)
          (icccm.delete-window wid)]
         [(ewmh.atom-ref '_NET_CURRENT_DESKTOP)
          (op.desktop-activate (list-ref (xclientmessageevent-data ev) 0))]
         [(ewmh.atom-ref '_NET_WM_STATE)
          (ewmh.on-client-state wid ev)]
         [(ewmh.atom-ref '_NET_WM_DESKTOP)
          (op.move-window-to-desktop wid (list-ref (xclientmessageevent-data ev) 0))]
         [(icccm.atom-ref 'WM_CHANGE_STATE)
          (if (eq? (list-ref (xclientmessageevent-data ev) 0) icccm.IconicState)
              (op.banish-window wid))]
         [(icccm.atom-ref 'WM_PROTOCOLS)
          (let ([a (list-ref (xclientmessageevent-data ev) 0)])
            (display (format "#x~x WM_PROTOCOLS -> ~a ~a~n" wid a (xutil.atom-name a))))]
         [else
          (display (format "#x~x Unknown ClientMessage message type~n" wid))]))))

  (define on-configure
    (lambda (ev)
      (let ([wid (xconfigureevent-wid ev)])
        ;; For Notify events, always ignore those generated by SubstructureRedirects.
        (when (eq? wid (xanyevent-wid (xconfigureevent-xany ev)))
          (display (format "#x~x ConfigureNotify" wid))
          (when (eq? wid (ewmh.active-window))
            ;; Some windows ignore the initial geom we set during map-request so we check
            ;; and force the resize if necessary.
            (let ([ideal (ewmh.workarea-geometry)])
              (if (xutil.geometry=? ideal (xutil.xconfigureevent-geometry ev))
                  (display " ideal")
                  (begin
                    (display (format " non-ideal requesting ~a" ideal))
                    (xutil.resize-window wid ideal)))))
          (newline)))))

  (define on-configure-request
    (lambda (ev)
      (let ([wid (xconfigurerequestevent-wid ev)]
            [g (icccm.on-configure-request ev)])
        (display (format "#x~x ConfigureRequest ~a~n" wid g))
        (when (ewmh.dock-window? wid)
          ;; ConfigureRequest is for a dock window, recalculate work area.
          ;; I'd prefer to handle this in the ConfigureNotify handler, but i'm not getting
          ;; those events for windows that already exist at wm-startup time.
          (ewmh.calculate-workarea (xutil.get-child-windows))
          (op.draw-active-window (ewmh.active-window))))))

  (define on-create-window
    (lambda (ev)
      (when (icccm.on-create-window ev)
        (display (format "#x~x CreateNotify~n" (xcreatewindowevent-wid ev))))))

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
      (let ([wid (xmapevent-wid ev)]
            [aid (ewmh.active-window)])
        (when (eq? wid (xanyevent-wid (xmapevent-xany ev)))
          (if (eq? wid aid)
            (icccm.focus-window wid))
          (display (format "#x~x MapNotify ~a~n" wid (op.window-name wid)))))))

  (define on-map-request
    (lambda (ev)
      (let ([wid (xmaprequestevent-wid ev)])
        (display (format "#x~x MapRequest ~a~n" wid (op.window-name wid)))
        (icccm.on-map-request ev)
        (unless (ewmh.dock-window? wid)
          (ewmh.on-map-request ev)
          (op.arrange-windows)))))

  (define on-property
    (lambda (ev)
      (let ([wid (xanyevent-wid (xpropertyevent-xany ev))]
            [atom (xpropertyevent-propatom ev)])
        (display (format "#x~x PropertyNotify ~a~n" wid (xutil.atom-name atom)))
        (when (hints.command? atom)
         (hints.on-command wid)))))

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
          (let ([wid (xunmapevent-wid ev)])
            (if (memq wid (ewmh.client-list))
              (let ([state (icccm.get-wm-state wid)])
                (display (format "#x~x UnmapNotify state=~a\n" wid state))
                ;; State could be #f if window is already deleted.
                (when (or (not state) (not (eq? state icccm.IconicState)))
                  (display (format "#x~x removing window from EWMH client lists~n" wid))
                  (ewmh.remove-window wid)
                  (when (eq? wid (ewmh.active-window))
                    (ewmh.active-window-set! None))
                  (op.arrange-windows)))
              ;; We don't store dockapps in client-list, but watch them so we can update struts.
              ;; Traversing all child windows is a bit brute force. Need to consider a FOOT_DOCKAPP_LIST.
              (begin
                (ewmh.calculate-workarea (remove wid (xutil.get-child-windows)))
                (op.draw-active-window (ewmh.active-window)))))))))