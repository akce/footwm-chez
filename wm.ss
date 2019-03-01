(import (prefix (ewmh) ewmh.)
        (prefix (icccm) icccm.)
        (xlib)
        (prefix (xutil) xutil.)
        (rnrs base))

(define d (xutil.open))
(define r (XDefaultRootWindow d))

(icccm.init-atoms d)
(ewmh.init-atoms d)

;; XFree86 extension.
(define UTF8_STRING (XInternAtom d "UTF8_STRING" #f))

