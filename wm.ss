(import (prefix (ewmh) e.)
        (xlib)
        (prefix (xuser) x.)
        (rnrs base))

#;(define dpy (make-parameter))

(define d (XOpenDisplay #f))
(define r (XDefaultRootWindow d))

;; XFree86 extension.
(define UTF8_STRING (XInternAtom d "UTF8_STRING" #f))
;; ICCCM atoms.
(define WM_NAME (XInternAtom d "WM_NAME" #f))
;; EWMH atoms.
(define _NET_WM_NAME (XInternAtom d "_NET_WM_NAME" #f))
(define WM_CLASS (XInternAtom d "WM_CLASS" #f))
(define WM_COMMAND (XInternAtom d "WM_COMMAND" #f))

