(library (ewmh)
  (export
   active-window
   client-list
   client-list-stacking
   current-desktop
   desktop
   desktop-names
   desktop-set!
   name
   pid
   current-desktop-request!
   window-active-request!
   window-close-request!
   window-desktop-request!

   init-atoms
   atoms
   atom-ref
   )
  (import (chezscheme)
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
  (define atoms (xutil.make-atoms))
  (define init-atoms
    (lambda (d)
      (xutil.init-atoms d atoms atom-list)))
  (define atom-ref (xutil.make-atom-ref atoms))

  (define active-window
    (lambda (d wid)
      (vector-ref (xutil.property->u32* d wid (atom-ref '_NET_ACTIVE_WINDOW) XA-WINDOW) 0)))

  (define client-list
    (lambda (d)
      (xutil.property->u32* d (XDefaultRootWindow d) (atom-ref '_NET_CLIENT_LIST) XA-WINDOW)))

  (define client-list-stacking
    (lambda (d)
      (xutil.property->u32* d (XDefaultRootWindow d) (atom-ref '_NET_CLIENT_LIST_STACKING) XA-WINDOW)))

  ;; wm: the current active desktop number.
  (define current-desktop
    (lambda (d wid)
      (vector-ref (xutil.property->u32* d wid (atom-ref '_NET_CURRENT_DESKTOP) XA-CARDINAL) 0)))

  ;; Get the desktop number for the window.
  (define desktop
    (lambda (d wid)
      (vector-ref (xutil.property->u32* d wid (atom-ref '_NET_WM_DESKTOP) XA-CARDINAL) 0)))

  ;; Used by the WM to set the desktop for a window. Clients must use 'request-desktop'.
  (define desktop-set!
    (lambda (d wid number)
      (xutil.cardinal-set! d wid (atom-ref '_NET_WM_DESKTOP) number)))

  (define desktop-names
    (lambda (d wid)
      (xutil.property->string* d wid (atom-ref '_NET_DESKTOP_NAMES))))

  ;; Get the name for the window.
  (define name
    (lambda (d wid)
      (xutil.property->string d wid (atom-ref '_NET_WM_NAME))))

  (define pid
    (lambda (d wid)
      (vector-ref (xutil.property->u32* d wid (atom-ref '_NET_WM_PID) XA-CARDINAL) 0)))

  ;; Request WM activate window.
  (define window-active-request!
    (lambda (d r wid)
      (xutil.send-message-cardinal d r wid (atom-ref '_NET_ACTIVE_WINDOW) 0)))

  ;; Request WM to close the window.
  (define window-close-request!
    (lambda (d r wid)
      (xutil.send-message-cardinal d r wid (atom-ref '_NET_CLOSE_WINDOW) 0)))

  ;; Send a message to the WM requesting window wid be moved to desktop-number.
  (define window-desktop-request!
    (lambda (d r wid desktop-number)
      (xutil.send-message-cardinal d r wid (atom-ref '_NET_WM_DESKTOP) desktop-number)))

  (define current-desktop-request!
    (lambda (d r desktop-number)
      (xutil.send-message-cardinal d r 0 (atom-ref '_NET_CURRENT_DESKTOP) desktop-number)))
  )
