(import (prefix (ewmh) e.)
        (prefix (xlib) x.)
        (rnrs base))

#;(define dpy (make-parameter))

(define d (x.XOpenDisplay #f))
(define r (x.XDefaultRootWindow d))

;; XFree86 extension.
(define UTF8_STRING (x.XInternAtom d "UTF8_STRING" #f))
;; ICCCM atoms.
(define WM_NAME (x.XInternAtom d "WM_NAME" #f))
;; EWMH atoms.
(define _NET_WM_NAME (x.XInternAtom d "_NET_WM_NAME" #f))
(define WM_CLASS (x.XInternAtom d "WM_CLASS" #f))
(define WM_COMMAND (x.XInternAtom d "WM_COMMAND" #f))

