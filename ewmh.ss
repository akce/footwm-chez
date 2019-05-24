;; Extended Window Manager Hints (EWMH)
;; As at version 1.5.
;; See:
;;    https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html
(library (ewmh)
  (export
   init-atoms
   atom-ref
   client-list
   client-list-set!
   desktop-count
   desktop-count-set!
   current-desktop
   current-desktop-set!
   current-desktop-request!
   desktop-names
   desktop-names-set!
   active-window
   active-window-set!
   window-active-request!
   workarea-geometry
   calculate-workarea
   window-close-request!
   name
   window-desktop
   window-desktop-set!
   window-desktop-request!
   get-wm-window-type
   dock-window?
   show-in-taskbar?
   on-client-state
   show-window
   iconify-window
   demands-attention?
   get-wm-strut
   get-wm-strut-partial
   pid
   on-map-request
   remove-window)
  (import
   (rnrs)
   (only (chezscheme) define-values format)
   (prefix (xutil) xutil.)
   (globals)
   (xlib))

  (define atom-list
    '(_NET_ACTIVE_WINDOW
      _NET_CLIENT_LIST
      _NET_CLOSE_WINDOW
      _NET_CURRENT_DESKTOP
      _NET_DESKTOP_NAMES
      _NET_NUMBER_OF_DESKTOPS
      _NET_WORKAREA
      _NET_WM_DESKTOP
      _NET_WM_NAME
      _NET_WM_PID
      _NET_WM_STATE
      _NET_WM_STATE_DEMANDS_ATTENTION
      _NET_WM_STATE_HIDDEN
      _NET_WM_STRUT
      _NET_WM_STRUT_PARTIAL
      _NET_WM_WINDOW_TYPE
      _NET_WM_WINDOW_TYPE_DOCK
      _NET_WM_WINDOW_TYPE_NORMAL))

  (define-values
      (init-atoms atom-ref) (xutil.make-atom-manager atom-list))

  ;; Return first (and likely only) item in list or false if list is empty.
  (define first-or-false
    (lambda (lst)
      (if (null? lst)
          #f
          (list-ref lst 0))))

  ;;;;;; Root window properties (and messages)

  ;;;; _NET_SUPPORTED ATOM[]/32
  ;; TODO

  ;;;; _NET_CLIENT_LIST WINDOW[]/32

  (define client-list
    (lambda ()
      (xutil.property->ulongs (root) (atom-ref '_NET_CLIENT_LIST) XA-WINDOW)))

  (define client-list-set!
    (lambda (wids)
      ;; return as list as lists have more builtin operations.
      (xutil.ulongs-property-set! (root) (atom-ref '_NET_CLIENT_LIST) wids XA-WINDOW)))

  ;;;; _NET_CLIENT_LIST_STACKING WINDOW[]/32
  ;; N/A (Does not seem to be used by any taskbar.)

  ;;;; _NET_NUMBER_OF_DESKTOPS CARDINAL/32

  (define desktop-count
    (lambda ()
      (first-or-false (xutil.property->ulongs (root) (atom-ref '_NET_NUMBER_OF_DESKTOPS) XA-CARDINAL))))

  (define desktop-count-set!
    (lambda (number)
      (xutil.cardinal-set! (root) (atom-ref '_NET_NUMBER_OF_DESKTOPS) number)))

  ;;;; _NET_DESKTOP_GEOMETRY width, height, CARDINAL[2]/32
  ;; TODO

  ;;;; _NET_DESKTOP_VIEWPORT x, y, CARDINAL[][2]/32
  ;; TODO

  ;;;; _NET_CURRENT_DESKTOP desktop, CARDINAL/32

  (define current-desktop
    (lambda ()
      (first-or-false (xutil.property->ulongs (root) (atom-ref '_NET_CURRENT_DESKTOP) XA-CARDINAL))))

  (define current-desktop-set!
    (lambda (number)
      (xutil.cardinal-set! (root) (atom-ref '_NET_CURRENT_DESKTOP) number)))

  ;; client: request current desktop change.
  (define current-desktop-request!
    (lambda (desktop-number)
      (xutil.send-message-cardinal (root) 0 (atom-ref '_NET_CURRENT_DESKTOP) desktop-number)))

  ;;;; _NET_DESKTOP_NAMES UTF8_STRING[]

  (define desktop-names
    (lambda ()
      ;; return as list as lists have more builtin operations.
      (xutil.property->string* (root) (atom-ref '_NET_DESKTOP_NAMES))))

  (define desktop-names-set!
    (lambda (names)
      (xutil.text-property-set! (root) names (atom-ref '_NET_DESKTOP_NAMES))))

  ;;;; _NET_ACTIVE_WINDOW WINDOW/32

  (define active-window
    (lambda ()
      (first-or-false (xutil.property->ulongs (root) (atom-ref '_NET_ACTIVE_WINDOW) XA-WINDOW))))

  (define active-window-set!
    (lambda (wid)
      (xutil.ulongs-property-set! (root) (atom-ref '_NET_ACTIVE_WINDOW) (list wid) XA-WINDOW)))

  ;; client: Request WM activate window.
  (define window-active-request!
    (lambda (wid)
      (xutil.send-message-cardinal (root) wid (atom-ref '_NET_ACTIVE_WINDOW) 0)))

  ;;;; _NET_WORKAREA x, y, width, height CARDINAL[][4]/32
  ;; TODO
  (define workarea-geometry
    (lambda ()
      (let ([gl (xutil.property->ulongs (root) (atom-ref '_NET_WORKAREA) XA-CARDINAL)])
        (xutil.make-geometry (list-ref gl 0) (list-ref gl 1) (list-ref gl 2) (list-ref gl 3)))))

  (define workarea-set!
    (lambda (x y w h)
      (xutil.ulongs-property-set! (root) (atom-ref '_NET_WORKAREA) (list x y w h) XA-CARDINAL)))

  (define calculate-workarea
    (lambda (wids)
      (let ([rg (xutil.window-attributes-geom (xutil.get-window-attributes (root)))])
        ;; find the maximal strut.
        (let loop ([struts (map get-strut (filter dock-window? wids))] [left 0] [right 0] [top 0] [bottom 0])
          (cond
           [(null? struts)
            (let ([x left]
                  [y top]
                  [w (- (xutil.geometry-width rg) (+ left right))]
                  [h (- (xutil.geometry-height rg) (+ top bottom))])
              (workarea-set! x y w h))]
           [else
            (let ([strut (car struts)])
              (loop (cdr struts)
                    (max left (list-ref strut 0))
                    (max right (list-ref strut 1))
                    (max top (list-ref strut 2))
                    (max bottom (list-ref strut 3))))])))))

  ;;;; _NET_SUPPORTING_WM_CHECK WINDOW/32
  ;; TODO

  ;;;; _NET_VIRTUAL_ROOTS WINDOW[]/32
  ;; N/A

  ;;;; _NET_DESKTOP_LAYOUT orientation, columns, rows, starting_corner CARDINAL[4]/32
  ;; N/A

  ;;;; _NET_SHOWING_DESKTOP desktop, CARDINAL/32
  ;; TODO

  ;;;;;; Other Root window messages.

  ;;;; _NET_CLOSE_WINDOW

  ;; client: Request WM to close the window.
  (define window-close-request!
    (lambda (wid)
      (xutil.send-message-cardinal (root) wid (atom-ref '_NET_CLOSE_WINDOW) 0)))

  ;;;; _NET_MOVERESIZE_WINDOW
  ;; TODO

  ;;;; _NET_WM_MOVERESIZE
  ;; TODO

  ;;;; _NET_RESTACK_WINDOW
  ;; TODO

  ;;;; _NET_REQUEST_FRAME_EXTENTS
  ;; TODO

  ;;;;;; Application Window properties.

  ;;;; _NET_WM_NAME UTF8_STRING

  ;; Get the name for the window.
  (define name
    (lambda (wid)
      (xutil.property->string wid (atom-ref '_NET_WM_NAME))))

  ;;;; _NET_WM_VISIBLE_NAME UTF8_STRING
  ;; N/A

  ;;;; _NET_WM_ICON_NAME UTF8_STRING
  ;; N/A

  ;;;; _NET_WM_VISIBLE_ICON_NAME UTF8_STRING
  ;; N/A

  ;;;; _NET_WM_DESKTOP desktop, CARDINAL/32

  ;; Get the desktop number for the window.
  (define window-desktop
    (lambda (wid)
      (first-or-false (xutil.property->ulongs wid (atom-ref '_NET_WM_DESKTOP) XA-CARDINAL))))

  ;; Used by the WM to set the desktop for a window. Clients must use 'window-desktop-request!'.
  (define window-desktop-set!
    (lambda (wid number)
      (xutil.cardinal-set! wid (atom-ref '_NET_WM_DESKTOP) number)))

  ;; Send a message to the WM requesting window wid be moved to desktop-number.
  (define window-desktop-request!
    (lambda (wid desktop-number)
      (xutil.send-message-cardinal (root) wid (atom-ref '_NET_WM_DESKTOP) desktop-number)))

  ;;;; _NET_WM_WINDOW_TYPE ATOM[]/32
  (define get-wm-window-type
    (lambda (wid)
      (xutil.property->ulongs wid (atom-ref '_NET_WM_WINDOW_TYPE) XA-ATOM)))

  (define dock-window?
    (lambda (wid)
      (let ([types (get-wm-window-type wid)])
        (if (memq (atom-ref '_NET_WM_WINDOW_TYPE_DOCK) types)
            #t
            #f))))

  (define show-in-taskbar?
    (lambda (wid)
      (let ([types (get-wm-window-type wid)])
        (cond
         [(null? types) #t]
         [(memq (atom-ref '_NET_WM_WINDOW_TYPE_NORMAL) types) #t]
         [else #f]))))

  ;;;; _NET_WM_STATE ATOM[]/32

  (define get-net-wm-state
    (lambda (wid)
      (xutil.property->ulongs wid (atom-ref '_NET_WM_STATE) XA-ATOM)))

  (define net-wm-state-set!
    (lambda (wid values)
      (xutil.ulongs-property-set! wid (atom-ref '_NET_WM_STATE) values XA-ATOM)))

  (define on-client-state
    (lambda (wid ev)
      (let ([action (list-ref (xclientmessageevent-data ev) 0)]
            [prop1 (list-ref (xclientmessageevent-data ev) 1)]
            [prop2 (list-ref (xclientmessageevent-data ev) 2)]
            #;[source (list-ref (xclientmessageevent-data ev) 3)])
        (display (format "#x~x _NET_WM_STATE action ~a ~a ~a ~a ~a ~n" wid action prop1 (xutil.atom-name prop1) prop2 (xutil.atom-name prop2))))))

  (define add-net-wm-states-state!
    (lambda (wid a)
      (let ([states (get-net-wm-state wid)])
        (unless (memq a states)
          (net-wm-state-set! wid (cons a states))))))

  (define del-net-wm-states-state!
    (lambda (wid a)
      (let ([states (get-net-wm-state wid)])
        (when (memq a states)
          (net-wm-state-set! wid (remove a states))))))

  (define show-window
    (lambda (wid)
      (del-net-wm-states-state! wid (atom-ref '_NET_WM_STATE_DEMANDS_ATTENTION))
      (del-net-wm-states-state! wid (atom-ref '_NET_WM_STATE_HIDDEN))))

  (define iconify-window
    (lambda (wid)
      (add-net-wm-states-state! wid (atom-ref '_NET_WM_STATE_HIDDEN))))

  ;; _NET_WM_STATE_DEMANDS_ATTENTION
  ;; Like icccm.UrgencyHint but can be set by both wm & clients. It is usually cleared by the wm on activation.
  (define demands-attention?
    (lambda (wid)
      (let ([a (atom-ref '_NET_WM_STATE_DEMANDS_ATTENTION)]
            [states (get-net-wm-state wid)])
        (if (memq a states)
            #t
            #f))))

  ;;;; _NET_WM_ALLOWED_ACTIONS ATOM[]/32
  ;; TODO

  ;;;; _NET_WM_STRUT left, right, top, bottom, CARDINAL[4]/32
  (define get-wm-strut
    (lambda (wid)
      (xutil.property->ulongs wid (atom-ref '_NET_WM_STRUT) XA-CARDINAL)))

  ;;;; _NET_WM_STRUT_PARTIAL left, right, top, bottom, left_start_y, left_end_y,right_start_y, right_end_y, top_start_x, top_end_x, bottom_start_x,bottom_end_x,CARDINAL[12]/32
  (define get-wm-strut-partial
    (lambda (wid)
      (xutil.property->ulongs wid (atom-ref '_NET_WM_STRUT_PARTIAL) XA-CARDINAL)))

  (define get-strut
    (lambda (wid)
      (let ([partial (get-wm-strut-partial wid)])
        (if (null? partial)
            (get-wm-strut wid)
            partial))))

  ;;;; _NET_WM_ICON_GEOMETRY x, y, width, height, CARDINAL[4]/32
  ;; N/A

  ;;;; _NET_WM_ICON CARDINAL[][2+n]/32
  ;; N/A

  ;;;; _NET_WM_PID CARDINAL/32

  (define pid
    (lambda (wid)
      (first-or-false (xutil.property->ulongs wid (atom-ref '_NET_WM_PID) XA-CARDINAL))))

  ;;;; _NET_WM_HANDLED_ICONS
  ;; N/A

  ;;;; _NET_WM_USER_TIME CARDINAL/32
  ;; TODO

  ;;;; _NET_WM_USER_TIME_WINDOW WINDOW/32
  ;; TODO

  ;;;; _NET_FRAME_EXTENTS left, right, top, bottom, CARDINAL[4]/32
  ;; TODO

  ;;;; _NET_WM_OPAQUE_REGION x, y, width, height, CARDINAL[][4]/32
  ;; N/A

  ;;;; _NET_WM_BYPASS_COMPOSITOR CARDINAL/32
  ;; N/A

  ;;;;;; Window Manager Protocols

  ;;;; _NET_WM_PING
  ;; TODO

  ;;;; _NET_WM_SYNC_REQUEST
  ;; TODO

  ;;;; _NET_WM_FULLSCREEN_MONITORS CARDINAL[4]/32
  ;; TODO

  ;;;;;; Other Properties

  ;;;; _NET_FULL_PLACEMENT
  ;; TODO

  ;;;; Compositing Managers.
  ;; N/A

  ;;;; WM_TRANSIENT_FOR for override-redirect windows
  ;; TODO

  ;;;;; Other/misc functions.

  (define on-map-request
    (lambda (ev)
      ;; EWMH house keeping.
      ;; Add/move window to top of client window list etc.
      ;; Set window desktop and set active window.
      ;; Adjust workarea with newly mapped dock apps.
      (let ([wid (xmaprequestevent-wid ev)])
        (when (show-in-taskbar? wid)
          (let ([desk (current-desktop)]
                [clients (client-list)])
            (window-desktop-set! wid desk)
            (client-list-set!
             (append
              (list wid)
              (if (memq wid clients)
                  (remove wid clients)
                  clients))))))))

  (define remove-window
    (lambda (wid)
      ;; Remove window from client lists *only*.
      ;; We don't touch active-window here because we're not changing window state. That's up to the wm proper.
      (let ([clients (client-list)])
        (if (memq wid clients)
            (client-list-set! (remove wid clients)))))))
