(library (ewmh)
  (export client-list
          client-list-stacking
          init-atoms
          atoms
          atom-ref
          )
  (import (chezscheme)
          (xlib)
          (prefix (xutil) xutil.))

  (define atom-list
    '(_NET_CLIENT_LIST
      _NET_CLIENT_LIST_STACKING
      _NET_WM_NAME
      ))
  (define atoms (xutil.make-atoms))
  (define init-atoms
    (lambda (d)
      (xutil.init-atoms d atoms atom-list)))
  (define atom-ref (xutil.make-atom-ref atoms))

  (define client-list
    (lambda (d)
      (xutil.window-property-u32 d (XDefaultRootWindow d) (atom-ref '_NET_CLIENT_LIST) XA-WINDOW)))

  (define client-list-stacking
    (lambda (d)
      (xutil.window-property-u32 d (XDefaultRootWindow d) (atom-ref '_NET_CLIENT_LIST_STACKING) XA-WINDOW)))
  )
