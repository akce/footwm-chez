;; Extended Window Manager Hints (EWMH)
;; As at version 1.5.
;; See:
;;    https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html
;;
;; Written by Akce 2019-2020.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm ewmh)
  (export
   atom
   net-supported-init!
   client-list
   desktop-count
   desktop-geometry
   desktop-geometry-sync!
   desktop-viewport
   desktop-viewport-init!
   current-desktop
   current-desktop-request!
   desktop-names
   active-window
   window-active-request!
   workarea-geometry
   calculate-workarea
   showing-desktop
   window-close-request!
   name
   window-desktop
   window-desktop-set!
   window-desktop-request!
   get-wm-window-type
   dock-window?
   show-in-taskbar?
   get-net-wm-state
   net-wm-state-set!
   on-client-state
   show-window
   iconify-window
   demands-attention?
   fullscreen-window?
   get-wm-strut
   get-wm-strut-partial
   pid
   #;frame-extents
   frame-extents-set!
   remove-window)
  (import
   (rnrs)
   (only (chezscheme) format)
   (footwm xlib))

  (define atom
    (make-atom-manager
      '(_NET_ACTIVE_WINDOW
         _NET_CLIENT_LIST
         _NET_CLOSE_WINDOW
         _NET_CURRENT_DESKTOP
         _NET_DESKTOP_GEOMETRY
         _NET_DESKTOP_NAMES
         _NET_DESKTOP_VIEWPORT
         _NET_FRAME_EXTENTS
         _NET_NUMBER_OF_DESKTOPS
         _NET_REQUEST_FRAME_EXTENTS
         _NET_SHOWING_DESKTOP
         _NET_SUPPORTED
         _NET_WORKAREA
         _NET_WM_DESKTOP
         _NET_WM_NAME
         _NET_WM_PID
         _NET_WM_STATE
         _NET_WM_STATE_DEMANDS_ATTENTION
         _NET_WM_STATE_HIDDEN
         _NET_WM_STATE_FULLSCREEN
         _NET_WM_STRUT
         _NET_WM_STRUT_PARTIAL
         _NET_WM_WINDOW_TYPE
         _NET_WM_WINDOW_TYPE_DIALOG
         _NET_WM_WINDOW_TYPE_DOCK
         _NET_WM_WINDOW_TYPE_NORMAL)))

  ;; Return first (and likely only) item in list or false if list is empty.
  (define first-or-false
    (lambda (lst)
      (if (null? lst)
          #f
          (list-ref lst 0))))

  ;;;;;; Root window properties (and messages)

  ;;;; _NET_SUPPORTED ATOM[]/32
  (define net-supported-init!
    (lambda ()
      (ulongs-property-set! (root) (atom 'ref '_NET_SUPPORTED) (atom 'values) (x-atom 'ref 'ATOM))))

  ;;;; _NET_CLIENT_LIST WINDOW[]/32

  (define-root-property client-list
    ulong/list
    (atom 'ref '_NET_CLIENT_LIST)
    (x-atom 'ref 'WINDOW))

  ;;;; _NET_CLIENT_LIST_STACKING WINDOW[]/32
  ;; N/A (Does not seem to be used by any taskbar.)

  ;;;; _NET_NUMBER_OF_DESKTOPS CARDINAL/32

  (define-root-property desktop-count
    ulong
    (atom 'ref '_NET_NUMBER_OF_DESKTOPS)
    (x-atom 'ref 'CARDINAL))

  ;;;; _NET_DESKTOP_GEOMETRY width, height, CARDINAL[2]/32
  (define desktop-geometry
    (lambda ()
      (let ([g (property->ulongs (root) (atom 'ref '_NET_DESKTOP_GEOMETRY) (x-atom 'ref 'CARDINAL))])
        (make-geometry 0 0 (list-ref g 0) (list-ref g 1)))))

  (define desktop-geometry-sync!
    (lambda ()
      (let ([g (window-attributes-geom (x-get-window-attributes (root)))])
        (ulongs-property-set! (root) (atom 'ref '_NET_DESKTOP_GEOMETRY) `(,(geometry-width g) ,(geometry-height g)) (x-atom 'ref 'CARDINAL)))))

  ;;;; _NET_DESKTOP_VIEWPORT x, y, CARDINAL[][2]/32
  ;; Footwm doesn't support large desktops, so it will always be 0, 0.
  (define desktop-viewport
    (lambda ()
      (property->ulongs (root) (atom 'ref '_NET_DESKTOP_VIEWPORT) (x-atom 'ref 'CARDINAL))))

  (define desktop-viewport-init!
    (lambda ()
      (ulongs-property-set! (root) (atom 'ref '_NET_DESKTOP_VIEWPORT) '(0 0) (x-atom 'ref 'CARDINAL))))

  ;;;; _NET_CURRENT_DESKTOP desktop, CARDINAL/32

  (define-root-property current-desktop
    ulong
    (atom 'ref '_NET_CURRENT_DESKTOP)
    (x-atom 'ref 'CARDINAL))

  ;; client: request current desktop change.
  (define current-desktop-request!
    (lambda (desktop-number)
      (send-message-cardinal (root) 0 (atom 'ref '_NET_CURRENT_DESKTOP) desktop-number)))

  ;;;; _NET_DESKTOP_NAMES UTF8_STRING[]

  (define-root-property desktop-names
    string/list
    (atom 'ref '_NET_DESKTOP_NAMES))

  ;;;; _NET_ACTIVE_WINDOW WINDOW/32

  (define-root-property active-window
    ulong
    (atom 'ref '_NET_ACTIVE_WINDOW)
    (x-atom 'ref 'WINDOW))

  ;; client: Request WM activate window.
  (define window-active-request!
    (lambda (wid)
      (send-message-cardinal (root) wid (atom 'ref '_NET_ACTIVE_WINDOW) 0)))

  ;;;; _NET_WORKAREA x, y, width, height CARDINAL[][4]/32
  (define workarea-geometry
    (lambda ()
      (let ([gl (property->ulongs (root) (atom 'ref '_NET_WORKAREA) (x-atom 'ref 'CARDINAL))])
        (make-geometry (list-ref gl 0) (list-ref gl 1) (list-ref gl 2) (list-ref gl 3)))))

  (define workarea-set!
    (lambda (x y w h)
      (ulongs-property-set! (root) (atom 'ref '_NET_WORKAREA) (list x y w h) (x-atom 'ref 'CARDINAL))))

  (define calculate-workarea
    (lambda (wids)
      (let ([dg (desktop-geometry)])
        ;; find the maximal strut.
        (let loop ([struts (map get-strut (filter dock-window? wids))] [left 0] [right 0] [top 0] [bottom 0])
          (cond
           [(null? struts)
            (let ([x left]
                  [y top]
                  [w (- (geometry-width dg) (+ left right))]
                  [h (- (geometry-height dg) (+ top bottom))])
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
  (define showing-desktop
    (lambda (bool)
      (ulongs-property-set! (root) (atom 'ref '_NET_SHOWING_DESKTOP) (list (if bool 1 0)) (x-atom 'ref 'CARDINAL))))

  ;;;;;; Other Root window messages.

  ;;;; _NET_CLOSE_WINDOW

  ;; client: Request WM to close the window.
  (define window-close-request!
    (lambda (wid)
      (send-message-cardinal (root) wid (atom 'ref '_NET_CLOSE_WINDOW) 0)))

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
      (property->string wid (atom 'ref '_NET_WM_NAME))))

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
      (first-or-false (property->ulongs wid (atom 'ref '_NET_WM_DESKTOP) (x-atom 'ref 'CARDINAL)))))

  ;; Used by the WM to set the desktop for a window. Clients must use 'window-desktop-request!'.
  (define window-desktop-set!
    (lambda (wid number)
      (cardinal-set! wid (atom 'ref '_NET_WM_DESKTOP) number)))

  ;; Send a message to the WM requesting window wid be moved to desktop-number.
  (define window-desktop-request!
    (lambda (wid desktop-number)
      (send-message-cardinal (root) wid (atom 'ref '_NET_WM_DESKTOP) desktop-number)))

  ;;;; _NET_WM_WINDOW_TYPE ATOM[]/32
  (define get-wm-window-type
    (lambda (wid)
      (property->ulongs wid (atom 'ref '_NET_WM_WINDOW_TYPE) (x-atom 'ref 'ATOM))))

  (define dock-window?
    (lambda (wid)
      (let ([types (get-wm-window-type wid)])
        (if (memq (atom 'ref '_NET_WM_WINDOW_TYPE_DOCK) types)
            #t
            #f))))

  (define show-in-taskbar?
    (lambda (wid)
      (let ([types (get-wm-window-type wid)])
        (cond
         [(null? types) #t]
         [(memq (atom 'ref '_NET_WM_WINDOW_TYPE_NORMAL) types) #t]
         [(memq (atom 'ref '_NET_WM_WINDOW_TYPE_DIALOG) types) #t]
         [else #f]))))

  ;;;; _NET_WM_STATE ATOM[]/32

  (define get-net-wm-state
    (lambda (wid)
      (property->ulongs wid (atom 'ref '_NET_WM_STATE) (x-atom 'ref 'ATOM))))

  (define net-wm-state-set!
    (lambda (wid values)
      (ulongs-property-set! wid (atom 'ref '_NET_WM_STATE) values (x-atom 'ref 'ATOM))))

  ;; Defines for on-client-state: action.
  (define _NET_WM_STATE_REMOVE	0)	; remove/unset property
  (define _NET_WM_STATE_ADD	1)	; add/set property
  (define _NET_WM_STATE_TOGGLE	2)	; toggle property

  (define on-client-state
    (lambda (wid ev)
      (let ([action (list-ref (xclientmessageevent-data ev) 0)]
            [prop1 (list-ref (xclientmessageevent-data ev) 1)]
            [prop2 (list-ref (xclientmessageevent-data ev) 2)]
            #;[source (list-ref (xclientmessageevent-data ev) 3)]
            [wm-state (get-net-wm-state wid)])
        (display
          (format
            "#x~x _NET_WM_STATE action ~a ~a ~a ~a ~a ~n" wid action prop1 (x-get-atom-name prop1) prop2 (x-get-atom-name prop2)))
        (net-wm-state-set!
          wid
          (cond
            [(= action _NET_WM_STATE_REMOVE)
             (remv prop1 wm-state)]
            [(= action _NET_WM_STATE_ADD)
             (if (memv prop1 wm-state)
                 wm-state
                 (cons prop1 wm-state))]
            [(= action _NET_WM_STATE_TOGGLE)
             (if (memv prop1 wm-state)
                 (remv prop1 wm-state)
                 (cons prop1 wm-state))]
            [else
              (error #f (format "#x~x Bad _NET_WM_STATE action!" wid) action)]))
        )))

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
      (del-net-wm-states-state! wid (atom 'ref '_NET_WM_STATE_DEMANDS_ATTENTION))
      (del-net-wm-states-state! wid (atom 'ref '_NET_WM_STATE_HIDDEN))))

  (define iconify-window
    (lambda (wid)
      (add-net-wm-states-state! wid (atom 'ref '_NET_WM_STATE_HIDDEN))))

  ;; _NET_WM_STATE_DEMANDS_ATTENTION
  ;; Like icccm.UrgencyHint but can be set by both wm & clients. It is usually cleared by the wm on activation.
  (define demands-attention?
    (lambda (wid)
      (let ([a (atom 'ref '_NET_WM_STATE_DEMANDS_ATTENTION)]
            [states (get-net-wm-state wid)])
        (if (memq a states)
            #t
            #f))))

  ;; _NET_WM_STATE_FULLSCREEN
  (define fullscreen-window?
    (lambda (wid)
      (let ([a (atom 'ref '_NET_WM_STATE_FULLSCREEN)]
            [states (get-net-wm-state wid)])
        (if (memq a states)
            #t
            #f))))

  ;;;; _NET_WM_ALLOWED_ACTIONS ATOM[]/32
  ;; TODO

  ;;;; _NET_WM_STRUT left, right, top, bottom, CARDINAL[4]/32
  (define get-wm-strut
    (lambda (wid)
      (property->ulongs wid (atom 'ref '_NET_WM_STRUT) (x-atom 'ref 'CARDINAL))))

  ;;;; _NET_WM_STRUT_PARTIAL left, right, top, bottom, left_start_y, left_end_y,right_start_y, right_end_y, top_start_x, top_end_x, bottom_start_x,bottom_end_x,CARDINAL[12]/32
  (define get-wm-strut-partial
    (lambda (wid)
      (property->ulongs wid (atom 'ref '_NET_WM_STRUT_PARTIAL) (x-atom 'ref 'CARDINAL))))

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
      (first-or-false (property->ulongs wid (atom 'ref '_NET_WM_PID) (x-atom 'ref 'CARDINAL)))))

  ;;;; _NET_WM_HANDLED_ICONS
  ;; N/A

  ;;;; _NET_WM_USER_TIME CARDINAL/32
  ;; TODO

  ;;;; _NET_WM_USER_TIME_WINDOW WINDOW/32
  ;; TODO

  ;;;; _NET_FRAME_EXTENTS left, right, top, bottom, CARDINAL[4]/32
  ;; TODO
  #;(define frame-extents
    (lambda (wid)
      (let ([gl (property->ulongs wid (atom 'ref '_NET_FRAME_EXTENTS) (x-atom 'ref 'CARDINAL))])
        (cond
          [(null? gl)
           #f]
          [else
            ;; FIXME: left right top bottom != x y width height
            ;; FIXME: This is unused by the WM, but there should be an extents->geometry function.
            (make-geometry (list-ref gl 0) (list-ref gl 1) (list-ref gl 2) (list-ref gl 3))]))))

  (define frame-extents-set!
    (lambda (wid g)
      (ulongs-property-set! wid (atom 'ref '_NET_FRAME_EXTENTS)
                            `(,(geometry-x g)		                ; left
                               ,(- (geometry-width g) (geometry-x g))	; right
                               ,(geometry-y g)				; top
                               ,(- (geometry-height g) (geometry-y g)))	; bottom
                            (x-atom 'ref 'CARDINAL))))

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

  (define remove-window
    (lambda (wid)
      ;; Remove window from client lists *only*.
      ;; We don't touch active-window here because we're not changing window state. That's up to the wm proper.
      (let ([clients client-list])
        (if (memq wid clients)
            (set! client-list (remove wid clients)))))))
