(library (ewmh)
  (export client-list
          client-list-stacking
          )
  (import (rnrs base)
          (xlib)
          (prefix (xuser) x.))

  (define _NET_CLIENT_LIST #f)
  (define _NET_CLIENT_LIST_STACKING #f)

  (define atom-list
     '("_NET_CLIENT_LIST" "_NET_CLIENT_LIST_STACKING"))

  (define client-list
    (lambda (d)
      (x.window-property-u32 d (XDefaultRootWindow d) (XInternAtom d "_NET_CLIENT_LIST" #f) XA-WINDOW)))

  (define client-list-stacking
    (lambda (d)
      (x.window-property-u32 d (XDefaultRootWindow d) (XInternAtom d "_NET_CLIENT_LIST_STACKING" #f) XA-WINDOW)))
  )
