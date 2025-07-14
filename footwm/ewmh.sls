;; Extended Window Manager Hints (EWMH)
;; As at version 1.5.
;; See:
;;    https://specifications.freedesktop.org/wm-spec/latest
;;
;; Written by Jerry 2019-2025.
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
   current-desktop
   current-desktop-request!
   desktop-names
   active-window
   window-active-request!
   workarea-geometry
   calculate-workarea
   net-supporting-wm-check-init!
   showing-desktop
   window-close-request!
   window-name
   window-name-set!
   window-desktop
   window-desktop-set!
   window-desktop-request!
   window-net-wm-type
   dock-window?
   user-selectable?
   window-net-wm-state
   window-net-wm-state?
   make-wm-state-change
   wm-state-change-action wm-state-change-prop1 wm-state-change-prop2
   add-net-wm-states-state!
   del-net-wm-states-state!
   show-window
   iconify-window
   demands-attention?
   fullscreen-window?
   window-allowed-actions
   window-allowed-actions-set!
   window-net-wm-strut
   window-net-wm-strut-partial
   window-strut
   pid
   window-frame-extents
   window-frame-extents-set!
   remove-window)
  (import
   (rnrs)
   (only (chezscheme) errorf format iota)
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
         _NET_SUPPORTING_WM_CHECK

         _NET_WM_ALLOWED_ACTIONS
         _NET_WM_ACTION_MINIMIZE
         _NET_WM_ACTION_FULLSCREEN
         _NET_WM_ACTION_CHANGE_DESKTOP
         _NET_WM_ACTION_CLOSE

         _NET_WM_DESKTOP
         _NET_WM_FULL_PLACEMENT
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
         _NET_WM_WINDOW_TYPE_NORMAL

         _NET_WORKAREA)))

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
  (define-syntax desktop-geometry
    (identifier-syntax
      (let ([g (property->ulongs (root) (atom 'ref '_NET_DESKTOP_GEOMETRY) (x-atom 'ref 'CARDINAL))])
        (make-geometry 0 0 (list-ref g 0) (list-ref g 1)))))

  (define desktop-geometry-sync!
    (lambda ()
      (let ([g (window-attributes-geom (x-get-window-attributes (root)))])
        (ulongs-property-set! (root) (atom 'ref '_NET_DESKTOP_GEOMETRY) `(,(geometry-width g) ,(geometry-height g)) (x-atom 'ref 'CARDINAL)))))

  ;;;; _NET_DESKTOP_VIEWPORT x, y, CARDINAL[][2]/32
  (define-root-property desktop-viewport
    ulong/list
    (atom 'ref '_NET_DESKTOP_VIEWPORT)
    (x-atom 'ref 'CARDINAL))

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
  (define-root-property net-workarea
    ulong/list
    (atom 'ref '_NET_WORKAREA)
    (x-atom 'ref 'CARDINAL))

  (define-syntax workarea-geometry
    (identifier-syntax
      (let ([nw net-workarea])
        (make-geometry (list-ref nw 0) (list-ref nw 1) (list-ref nw 2) (list-ref nw 3)))))

  (define filter-struts
    (lambda (wids)
      (fold-left
        (lambda (acc w)
          (cond
            [(window-strut w)
             => (lambda (strut)
                  (cons strut acc))]
            [else
              acc]))
        '()
        wids)))

  (define calculate-workarea
    (lambda (wids)
      (let ([dg desktop-geometry])
        ;; Adjust usable client workarea by removing reserved strut (dock) areas.
        ;; Note that partial struts are not supported. ie, the strut will take the enter border area.
        (let loop ([struts (filter-struts wids)] [left 0] [right 0] [top 0] [bottom 0])
          (cond
           [(null? struts)
            (let ([x left]
                  [y top]
                  [w (- (geometry-width dg) (+ left right))]
                  [h (- (geometry-height dg) (+ top bottom))])
              (format #t "calculate-workarea ~a, ~a ~ax~a ~a~n" x y w h wids)
              (set! net-workarea
                ;; Copy one workarea set for each desktop.
                (apply append (map (lambda args (list x y w h)) (iota desktop-count)))))]
           [else
            (let ([strut (car struts)])
              (loop (cdr struts)
                    (max left (list-ref strut 0))
                    (max right (list-ref strut 1))
                    (max top (list-ref strut 2))
                    (max bottom (list-ref strut 3))))])))))

  ;;;; _NET_SUPPORTING_WM_CHECK WINDOW/32

  (define net-supporting-wm-check-init!
    (lambda ()
      (let ([wid (x-create-simple-window (root) 0 0 10 10 0 0 0)])
        ;; *both* the root window and child window must have this set as SDL apps and GTK+3 apps only look
        ;; to the child window for compliance.
        (for-each
          (lambda (w)
            (ulongs-property-set! w (atom 'ref '_NET_SUPPORTING_WM_CHECK) (list wid) (x-atom 'ref 'WINDOW)))
          (list (root) wid))
        (window-name-set! wid "footwm")
        wid)))

  ;;;; _NET_VIRTUAL_ROOTS WINDOW[]/32
  ;; N/A

  ;;;; _NET_DESKTOP_LAYOUT orientation, columns, rows, starting_corner CARDINAL[4]/32
  ;; N/A

  ;;;; _NET_SHOWING_DESKTOP desktop, CARDINAL/32
  (define-syntax showing-desktop
    (identifier-syntax
      [id
        (cond
          [(first-or-false (property->ulongs (root) (atom 'ref '_NET_SHOWING_DESKTOP) (x-atom 'ref 'CARDINAL)))
           => (lambda (num)
                (eqv? num 1))]
           [else
             #f])]
      [(set! id bool)
       (ulongs-property-set! (root) (atom 'ref '_NET_SHOWING_DESKTOP) (list (if bool 1 0)) (x-atom 'ref 'CARDINAL))]))

  ;;;;;; Other Root window messages.

  ;;;; _NET_CLOSE_WINDOW

  ;; client: Request WM to close the window.
  (define window-close-request!
    (lambda (wid)
      (send-message-cardinal (root) wid (atom 'ref '_NET_CLOSE_WINDOW) 0)))

  ;;;; _NET_MOVERESIZE_WINDOW
  ;; N/A

  ;;;; _NET_WM_MOVERESIZE
  ;; N/A

  ;;;; _NET_RESTACK_WINDOW
  ;; TODO This could be a way to give fine grained control of stacking list ordering.

  ;;;; _NET_REQUEST_FRAME_EXTENTS

  ;; Called in response to a ClientMessage _NET_WM_REQUEST_FRAME_EXTENTS.
  ;; wid is the window that requests the information.
  (define window-frame-extents-set!
    (lambda (wid)
      (ulongs-property-set! wid (atom 'ref '_NET_FRAME_EXTENTS)
                            '(0 0 0 0)		; footwm adds no borders.
                            (x-atom 'ref 'CARDINAL))))

  ;;;;;; Application Window properties.

  ;;;; _NET_WM_NAME UTF8_STRING

  ;; Get the name for the window.
  (define window-name
    (lambda (wid)
      (property->string wid (atom 'ref '_NET_WM_NAME))))

  (define window-name-set!
    (lambda (wid name)
      (text-property-set! wid (list name) (atom 'ref '_NET_WM_NAME))))

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
      (cond
        [(first-or-false (property->ulongs wid (atom 'ref '_NET_WM_DESKTOP) (x-atom 'ref 'CARDINAL)))
         => (lambda (long)
              ;; Masking required for sticky desktop value #xffffffff.
              (bitwise-and long #xffffffff))]
        [else
          #f])))

  ;; Used by the WM to set the desktop for a window. Clients must use 'window-desktop-request!'.
  (define window-desktop-set!
    (lambda (wid number)
      (cardinal-set! wid (atom 'ref '_NET_WM_DESKTOP) number)))

  ;; Send a message to the WM requesting window wid be moved to desktop-number.
  (define window-desktop-request!
    (lambda (wid desktop-number)
      (send-message-cardinal (root) wid (atom 'ref '_NET_WM_DESKTOP) desktop-number)))

  ;;;; _NET_WM_WINDOW_TYPE ATOM[]/32
  (define window-net-wm-type
    (lambda (wid)
      (property->ulongs wid (atom 'ref '_NET_WM_WINDOW_TYPE) (x-atom 'ref 'ATOM))))

  (define dock-window?
    (lambda (wid)
      (and (memq (atom 'ref '_NET_WM_WINDOW_TYPE_DOCK) (window-net-wm-type wid)) #t)))

  ;; This has scope for improvement. Refer to discussion in _NET_WM_WINDOW_TYPE.
  (define user-selectable?
    (lambda (wid)
      (let ([types (window-net-wm-type wid)])
        (cond
         [(null? types) #t]
         [(memq (atom 'ref '_NET_WM_WINDOW_TYPE_NORMAL) types) #t]
         [(memq (atom 'ref '_NET_WM_WINDOW_TYPE_DIALOG) types) #t]
         [else #f]))))

  ;;;; _NET_WM_STATE ATOM[]/32

  (define window-net-wm-state
    (lambda (wid)
      (property->ulongs wid (atom 'ref '_NET_WM_STATE) (x-atom 'ref 'ATOM))))

  (define net-wm-state-set!
    (lambda (wid value)
      (ulongs-property-set! wid (atom 'ref '_NET_WM_STATE) value (x-atom 'ref 'ATOM))))

  (define window-net-wm-state?
    (lambda (wid prop)
      (and
        (memq (if (symbol? prop)
                (atom 'ref prop)
                prop)
              (window-net-wm-state wid))
        #t)))

  ;; Defines for on-client-state: action.
  (define _NET_WM_STATE_REMOVE	0)	; remove/unset property
  (define _NET_WM_STATE_ADD	1)	; add/set property
  (define _NET_WM_STATE_TOGGLE	2)	; toggle property

  (define action->symbol
    (lambda (actid)
      (case actid
        [(0)
         'REMOVE]
        [(1)
         'ADD]
        [(2)
         'TOGGLE]
        [else
          'UNKNOWN])))

  (define-record-type wm-state-change
    (fields action prop1 prop2 source)
    (protocol
      (lambda (new)
        (lambda (ev)
          (new
            (action->symbol (list-ref (xclientmessageevent-data ev) 0))
            (list-ref (xclientmessageevent-data ev) 1)
            (let ([p2 (list-ref (xclientmessageevent-data ev) 2)])
              (if (eqv? p2 0)
                #f
                p2))
            (list-ref (xclientmessageevent-data ev) 3))))))

  (define add-net-wm-states-state!
    (lambda (wid a)
      (let ([states (window-net-wm-state wid)])
        (unless (memq a states)
          (net-wm-state-set! wid (cons a states))))))

  (define del-net-wm-states-state!
    (lambda (wid a)
      (let ([states (window-net-wm-state wid)])
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
      (window-net-wm-state? wid '_NET_WM_STATE_DEMANDS_ATTENTION)))

  ;; _NET_WM_STATE_FULLSCREEN
  (define fullscreen-window?
    (lambda (wid)
       (window-net-wm-state? wid '_NET_WM_STATE_FULLSCREEN)))

  ;;;; _NET_WM_ALLOWED_ACTIONS ATOM[]/32

  (define window-allowed-actions
    (lambda (wid)
      (map
        (lambda (x)
          (string->symbol (x-get-atom-name x)))
        (property->ulongs wid (atom 'ref '_NET_WM_ALLOWED_ACTIONS) (x-atom 'ref 'ATOM)))))

  (define window-allowed-actions-set!
    (lambda (wid)
      (ulongs-property-set! wid (atom 'ref '_NET_WM_ALLOWED_ACTIONS)
                            (map
                              (lambda (x)
                                (atom 'ref x))
                              '(_NET_WM_ACTION_MINIMIZE
                                 _NET_WM_ACTION_FULLSCREEN
                                 _NET_WM_ACTION_CHANGE_DESKTOP
                                 _NET_WM_ACTION_CLOSE))
                            (x-atom 'ref 'ATOM))))

  ;;;; _NET_WM_STRUT left, right, top, bottom, CARDINAL[4]/32
  (define window-net-wm-strut
    (lambda (wid)
      (property->ulongs wid (atom 'ref '_NET_WM_STRUT) (x-atom 'ref 'CARDINAL))))

  ;;;; _NET_WM_STRUT_PARTIAL left, right, top, bottom, left_start_y, left_end_y,right_start_y, right_end_y, top_start_x, top_end_x, bottom_start_x,bottom_end_x,CARDINAL[12]/32
  (define window-net-wm-strut-partial
    (lambda (wid)
      (property->ulongs wid (atom 'ref '_NET_WM_STRUT_PARTIAL) (x-atom 'ref 'CARDINAL))))

  ;; window-strut tries to retrieve the newer _NET_WM_STRUT_PARTIAL property, falling back to _NET_WM_STRUT, or #f if neither are defined.
  (define window-strut
    (lambda (wid)
      (let ([partial (window-net-wm-strut-partial wid)])
        (cond
          [partial
            partial]
          [(window-net-wm-strut wid)
           => values]
          [else
            ;; No struts defined for window. ie, window won't be an ewmh dock window.
            #f]))))

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

  (define window-frame-extents
    (lambda (wid)
      (let ([exts (property->ulongs wid (atom 'ref '_NET_FRAME_EXTENTS) (x-atom 'ref 'CARDINAL))])
        (cond
          [(null? exts)
           #f]
          [else
            ;; TODO Convert to geometry?
            exts]))))

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

  ;;;; _NET_WM_FULL_PLACEMENT
  ;; Setting in _NET_SUPPORTED indicates to Clients that the WM wants complete charge of window placement.

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
