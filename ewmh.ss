(library (ewmh)
  (export client-list
          client-list-stacking
          init-atoms
          atoms
          atom-ref
          )
  (import (chezscheme)
          (xlib)
          (prefix (xuser) x.))

  (define atom-list
    '(_NET_CLIENT_LIST
      _NET_CLIENT_LIST_STACKING
      _NET_WM_NAME
      ))
  (define atoms (x.make-atoms))
  (define init-atoms
    (lambda (d)
      (x.init-atoms d atoms atom-list)))
  (define atom-ref (x.make-atom-ref atoms))

  (define client-list
    (lambda (d)
      (x.window-property-u32 d (XDefaultRootWindow d) (atom-ref '_NET_CLIENT_LIST) XA-WINDOW)))

  (define client-list-stacking
    (lambda (d)
      (x.window-property-u32 d (XDefaultRootWindow d) (atom-ref '_NET_CLIENT_LIST_STACKING) XA-WINDOW)))
  )
