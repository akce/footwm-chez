;; Extended Window Manager Hints (EWMH)
;; As at version 1.5.
;; See:
;;    https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html
(library (ewmh)
  (export
   active-window
   client-list
   client-list-set!
   client-list-stacking
   client-list-stacking-set!
   current-desktop
   current-desktop-set!
   desktop-count
   desktop-count-set!
   desktop-names
   desktop-names-set!
   window-desktop
   window-desktop-set!
   name
   pid
   current-desktop-request!
   window-active-request!
   window-close-request!
   window-desktop-request!

   init-atoms
   atom-ref
   )
  (import (chezscheme)
          (globals)
          (xlib)
          (prefix (xutil) xutil.))

  (define atom-list
    '(_NET_ACTIVE_WINDOW
      _NET_CLIENT_LIST
      _NET_CLIENT_LIST_STACKING
      _NET_CLOSE_WINDOW
      _NET_CURRENT_DESKTOP
      _NET_DESKTOP_NAMES
      _NET_NUMBER_OF_DESKTOPS
      _NET_WM_DESKTOP
      _NET_WM_NAME
      _NET_WM_PID

      ;UTF8_STRING
      ))
  (define-values
      (init-atoms atom-ref) (xutil.make-atom-manager atom-list))

  ;; Return first (and likely only) item in vector or false if vector is empty.
  (define first-or-false
    (lambda (vect)
      (if (fx=? 0 (vector-length vect))
          #f
          (vector-ref vect 0))))

  ;; Return the property vector, or #f if empty.
  (define property-or-false
    (lambda (vect)
      (if (fx=? 0 (vector-length vect))
          #f
          vect)))

  (define active-window
    (lambda ()
      (first-or-false (xutil.property->ulongs (root) (atom-ref '_NET_ACTIVE_WINDOW) XA-WINDOW))))

  (define client-list
    (lambda ()
      (property-or-false (xutil.property->ulongs (root) (atom-ref '_NET_CLIENT_LIST) XA-WINDOW))))

  (define client-list-set!
    (lambda (wids)
      (xutil.ulongs-property-set! (root) (atom-ref '_NET_CLIENT_LIST) wids XA-WINDOW)))

  (define client-list-stacking
    (lambda ()
      (property-or-false (xutil.property->ulongs (root) (atom-ref '_NET_CLIENT_LIST_STACKING) XA-WINDOW))))

  (define client-list-stacking-set!
    (lambda (wids)
      (xutil.ulongs-property-set! (root) (atom-ref '_NET_CLIENT_LIST_STACKING) wids XA-WINDOW)))

  ;; wm: the current active desktop number.
  (define current-desktop
    (lambda ()
      (first-or-false (xutil.property->ulongs (root) (atom-ref '_NET_CURRENT_DESKTOP) XA-CARDINAL))))

  (define current-desktop-set!
    (lambda (number)
      (xutil.cardinal-set! (root) (atom-ref '_NET_CURRENT_DESKTOP) number)))

  ;; Get the desktop number for the window.
  (define window-desktop
    (lambda (wid)
      (first-or-false (xutil.property->ulongs wid (atom-ref '_NET_WM_DESKTOP) XA-CARDINAL))))

  ;; Used by the WM to set the desktop for a window. Clients must use 'window-desktop-request!'.
  (define window-desktop-set!
    (lambda (wid number)
      (xutil.cardinal-set! wid (atom-ref '_NET_WM_DESKTOP) number)))

  (define desktop-count
    (lambda ()
      (first-or-false (xutil.property->ulongs (root) (atom-ref '_NET_NUMBER_OF_DESKTOPS) XA-CARDINAL))))

  (define desktop-count-set!
    (lambda (number)
      (xutil.cardinal-set! (root) (atom-ref '_NET_NUMBER_OF_DESKTOPS) number)))

  (define desktop-names
    (lambda ()
      (xutil.property->string* (root) (atom-ref '_NET_DESKTOP_NAMES))))

  (define desktop-names-set!
    (lambda (names)
      (xutil.text-property-set! (root) names (atom-ref '_NET_DESKTOP_NAMES))))

  ;; Get the name for the window.
  (define name
    (lambda (wid)
      (xutil.property->string wid (atom-ref '_NET_WM_NAME))))

  (define pid
    (lambda (wid)
      (first-or-false (xutil.property->ulongs wid (atom-ref '_NET_WM_PID) XA-CARDINAL))))

  ;; Request WM activate window.
  (define window-active-request!
    (lambda (wid)
      (xutil.send-message-cardinal (root) wid (atom-ref '_NET_ACTIVE_WINDOW) 0)))

  ;; Request WM to close the window.
  (define window-close-request!
    (lambda (wid)
      (xutil.send-message-cardinal (root) wid (atom-ref '_NET_CLOSE_WINDOW) 0)))

  ;; Send a message to the WM requesting window wid be moved to desktop-number.
  (define window-desktop-request!
    (lambda (wid desktop-number)
      (xutil.send-message-cardinal (root) wid (atom-ref '_NET_WM_DESKTOP) desktop-number)))

  (define current-desktop-request!
    (lambda (desktop-number)
      (xutil.send-message-cardinal (root) 0 (atom-ref '_NET_CURRENT_DESKTOP) desktop-number)))
  )
