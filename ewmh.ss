(library (ewmh)
  (export
   active-window
   client-list
   client-list-stacking
   current-desktop
   desktop-names
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
      _NET_WM_DESKTOP
      _NET_WM_NAME
      _NET_WM_PID

      ;UTF8_STRING
      ))
  (define-values
      (init-atoms atom-ref) (xutil.make-atom-manager atom-list))

  (define active-window
    (lambda ()
      (vector-ref (xutil.property->u32* (root) (atom-ref '_NET_ACTIVE_WINDOW) XA-WINDOW) 0)))

  (define client-list
    (lambda ()
      (xutil.property->u32* (root) (atom-ref '_NET_CLIENT_LIST) XA-WINDOW)))

  (define client-list-stacking
    (lambda ()
      (xutil.property->u32* (root) (atom-ref '_NET_CLIENT_LIST_STACKING) XA-WINDOW)))

  ;; wm: the current active desktop number.
  (define current-desktop
    (lambda ()
      (vector-ref (xutil.property->u32* (root) (atom-ref '_NET_CURRENT_DESKTOP) XA-CARDINAL) 0)))

  ;; Get the desktop number for the window.
  (define window-desktop
    (lambda (wid)
      (vector-ref (xutil.property->u32* wid (atom-ref '_NET_WM_DESKTOP) XA-CARDINAL) 0)))

  ;; Used by the WM to set the desktop for a window. Clients must use 'window-desktop-request!'.
  (define window-desktop-set!
    (lambda (wid number)
      (xutil.cardinal-set! wid (atom-ref '_NET_WM_DESKTOP) number)))

  (define desktop-names
    (lambda ()
      (xutil.property->string* (root) (atom-ref '_NET_DESKTOP_NAMES))))

  ;; Get the name for the window.
  (define name
    (lambda (wid)
      (xutil.property->string wid (atom-ref '_NET_WM_NAME))))

  (define pid
    (lambda (wid)
      (vector-ref (xutil.property->u32* wid (atom-ref '_NET_WM_PID) XA-CARDINAL) 0)))

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
