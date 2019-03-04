(import (prefix (ewmh) ewmh.)
        (prefix (wm) wm.)
        (prefix (icccm) icccm.)
        (prefix (shell) sh.)
        (xlib)
        (prefix (xutil) xutil.)
        (rnrs base))

(define d (xutil.open))
(define r (XDefaultRootWindow d))

(icccm.init-atoms d)
(ewmh.init-atoms d)
(wm.init-atoms d)
