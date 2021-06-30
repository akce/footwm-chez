;; Footwm main module.
;;
;; This contains the footwm init, end and event loop functions.
;;
;; Written by Akce 2019-2020.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm footwm)
  (export
   init-desktops
   init-windows
   main)
  (import
   (rnrs)
   (only (chezscheme) format)
   (only (footwm util) case-equal?)
   (footwm xlib)
   (prefix (footwm ewmh) ewmh.)
   (prefix (footwm hints) hints.)
   (prefix (footwm icccm) icccm.)
   (prefix (footwm wm) wm.))

  (define main
    (lambda (desktops assignments)
      (install-as-wm)
      ;; Replace the default error handler with our own. This ensures that the wm continues to function even
      ;; after an error. eg, when trying to access a resource from a destroyed window.
      (x-set-error-handler)
      (ewmh.net-supported-init!)
      (ewmh.desktop-geometry-sync!)
      (ewmh.desktop-viewport-init!)
      (init-desktops desktops)
      (init-windows assignments)
      (wm.arrange-windows)
      (run assignments)))

  ;; Install as *the* window manager.
  ;; raises an error condition on failure.
  (define install-as-wm
    (lambda ()
      ;; The window manager client (wm) is the only client that can select SubstructureRedirect on the root window.
      ;; Any config change requested by directo child windows will result in CirculateRequest, ConfigureRequest,
      ;; or MapRequest events being sent to the wm. The wm can then honour, disregard, or modify those requests.
      (let* ([installed #t]
             [orig (x-set-error-handler
                    (lambda (d ev)
                      (if (eq? (xerrorevent-error-code ev) BadAccess)
                        (set! installed #f))))]
             [mask
              (bitwise-ior
               PropertyChange
               StructureNotify		; Structure = geometry, border, stacking info for a window.
               SubstructureNotify	; Sub = structure notify for child windows.
               SubstructureRedirect)])	; Redirect child window change requests to this client.
          (x-select-input (root) mask)
          (x-sync)
          (x-set-error-handler orig)
          (unless installed
            (raise (condition (make-error) (make-message-condition "Failed to install. Another WM is running.")))))))

  (define init-desktops
    (lambda (desktops)
      ;; footwm needs _NET_DESKTOP_NAMES, _NET_NUMBER_OF_DESKTOPS, and _NET_CURRENT_DESKTOP.
      ;; Make sure they exist or create if necessary.
      (unless ewmh.current-desktop
        (set! ewmh.current-desktop 0))
      (when (null? ewmh.desktop-names)
        (set! ewmh.desktop-names desktops))
      (unless ewmh.desktop-count
        (set! ewmh.desktop-count (length desktops)))))

  (define init-windows
    ;; Import pre-existing windows that need to be managed and then arranges as per initial desktop layout.
    (lambda (assignments)
      (let* ([allwids (x-query-tree)]
             [ws (filter wm.manage-window? allwids)])
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
             (ewmh.window-desktop-set! wid (wm.assign-desktop assignments wid))))
         ws)
        ;; set client-list ewmh hints.
        (let ([clients ewmh.client-list])
          (set! ewmh.client-list
            (if (null? clients)
              ws
              ;; client list already exists, need to sanitise it with ws.
              (filter wid-exists? (set-join clients ws))))))))

  (define run
    (lambda (assignments)
      (let loop ()
        (let ([ev (x-next-event)])
          (cond
           ((xclientmessageevent? ev)		(on-client-message ev))
           ((xconfigureevent? ev)		(on-configure ev))
           ((xconfigurerequestevent? ev)	(on-configure-request ev))
           ((xcreatewindowevent? ev)		(on-create-window ev))
           ((xdestroywindowevent? ev)		(on-destroy-window ev))
           ((xmapevent? ev)			(on-map ev))
           ((xmaprequestevent? ev)		(on-map-request ev assignments))
           ((xpropertyevent? ev)		(on-property ev))
           ((xunmapevent? ev)			(on-unmap ev))
           (else
	    (format #t "Unknown event ~d~n" (xanyevent-type ev)))))
        (loop))))

  (define on-client-message
    (lambda (ev)
      (let ([wid (xanyevent-wid (xclientmessageevent-xany ev))]
            [type (xclientmessageevent-message-type ev)])
        (format #t "#x~x ClientMessage ~a ~a~n" wid type (x-get-atom-name type))
        (case-equal? type
         [(ewmh.atom 'ref '_NET_ACTIVE_WINDOW)
          (wm.activate-window wid)]
         [(ewmh.atom 'ref '_NET_CLOSE_WINDOW)
          (icccm.delete-window wid)]
         [(ewmh.atom 'ref '_NET_CURRENT_DESKTOP)
          (wm.desktop-activate (list-ref (xclientmessageevent-data ev) 0))]
         [(ewmh.atom 'ref '_NET_REQUEST_FRAME_EXTENTS)
          (ewmh.frame-extents-set! wid (wm.ideal-window-geometry wid))]
         [(ewmh.atom 'ref '_NET_WM_STATE)
          (ewmh.on-client-state wid ev)
          (when (eqv? ewmh.active-window wid)
            ;; Only redraw if active window was affected. eg, for fullscreen state change.
            (wm.draw-active-window wid))]
         [(ewmh.atom 'ref '_NET_WM_DESKTOP)
          (wm.move-window-to-desktop wid (list-ref (xclientmessageevent-data ev) 0))]
         [(icccm.atom 'ref 'WM_CHANGE_STATE)
          (if (eq? (list-ref (xclientmessageevent-data ev) 0) icccm.IconicState)
              (wm.banish-window wid))]
         [(icccm.atom 'ref 'WM_PROTOCOLS)
          (let ([a (list-ref (xclientmessageevent-data ev) 0)])
            (format #t "#x~x WM_PROTOCOLS -> ~a ~a~n" wid a (x-get-atom-name a)))]
         [else
          (format #t "#x~x Unknown ClientMessage message type~n" wid)]))))

  (define on-configure
    (lambda (ev)
      (let ([wid (xconfigureevent-wid ev)])
        ;; For Notify events, always ignore those generated by SubstructureRedirects.
        (when (eq? wid (xanyevent-wid (xconfigureevent-xany ev)))
          (format #t "#x~x ConfigureNotify" wid)
          (when (eq? wid ewmh.active-window)
            ;; Some windows ignore the initial geom we set during map-request so we check
            ;; and force the resize if necessary.
            (let ([ideal (wm.ideal-window-geometry wid)])
              (cond
                [(geometry=? ideal (xconfigureevent-geometry ev))
                  (display " ideal")]
                [else
                  (format #t " non-ideal requesting ~a" ideal)
                  (x-configure-window wid ideal)])))
          (newline)))))

  (define on-configure-request
    (lambda (ev)
      (let ([wid (xconfigurerequestevent-wid ev)]
            [g (icccm.on-configure-request ev)])
        (format #t "#x~x ConfigureRequest ~a~n" wid g)
        (when (ewmh.dock-window? wid)
          ;; ConfigureRequest is for a dock window, recalculate work area.
          ;; I'd prefer to handle this in the ConfigureNotify handler, but i'm not getting
          ;; those events for windows that already exist at wm-startup time.
          (ewmh.calculate-workarea (x-query-tree))
          (wm.draw-active-window ewmh.active-window)))))

  (define on-create-window
    (lambda (ev)
      (when (icccm.on-create-window ev)
        (format #t "#x~x CreateNotify~n" (xcreatewindowevent-wid ev)))))

  (define on-destroy-window
    (lambda (ev)
      ;; For Notify events, always ignore those generated by SubstructureRedirects.
      (when (= (xdestroywindowevent-wid ev) (xanyevent-wid (xdestroywindowevent-xany ev)))
        (format #t "#x~x DestroyNotify~n" (xdestroywindowevent-wid ev))
        (wm.remove-window (xdestroywindowevent-wid ev)))))

  ;; MapNotify: A window has become visible.
  ;; see XMapWindow(3).
  (define on-map
    (lambda (ev)
      ;; For Notify events, always ignore those generated by SubstructureRedirects.
      (let ([wid (xmapevent-wid ev)])
        (when (eq? wid (xanyevent-wid (xmapevent-xany ev)))
          (if (eq? wid ewmh.active-window)
              ;; Don't use the fancy icccm version as it doesn't always work. eg, Some SDL windowed app
              ;; regaining focus don't regain keyboard..
              ;; This simple version seems to work better, at least so far..
            (x-set-input-focus wid RevertToNone CurrentTime)
            #;(icccm.focus-window wid))
          (format #t "#x~x MapNotify ~a~n" wid (wm.window-name wid))))))

  ;; MapRequest: A window has requested that it become viewable.
  ;; see XMapWindow(3).
  (define on-map-request
    (lambda (ev assignments)
      (let ([wid (xmaprequestevent-wid ev)])
        (format #t "#x~x MapRequest ~a~n" wid (wm.window-name wid))
        (icccm.on-map-request ev)
        (unless (ewmh.dock-window? wid)
          (wm.add-window ev assignments)
          (wm.arrange-windows)))))

  (define on-property
    (lambda (ev)
      (let ([wid (xanyevent-wid (xpropertyevent-xany ev))]
            [atom (xpropertyevent-propatom ev)])
        (format #t "#x~x PropertyNotify ~a~n" wid (x-get-atom-name atom))
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
            (cond
              [(memq wid ewmh.client-list)
               (let ([state (icccm.get-wm-state wid)])
                 (cond
                   ;; State could be #f if window is already deleted.
                   [(or (not state) (eq? state icccm.WithdrawnState))
                    (format #t "#x~x removing window from EWMH client lists~n" wid)
                    (wm.remove-window wid)]
                   [else
                     (format #t "#x~x UnmapNotify ignore state=~a\n" wid state)]))]
              [else
                ;; We don't store dockapps in client-list, but watch them so we can update struts.
                ;; Traversing all child windows is a bit brute force. Need to consider a FOOT_DOCKAPP_LIST.
                (ewmh.calculate-workarea (remove wid (x-query-tree)))
                (wm.draw-active-window ewmh.active-window)])))))
  )
