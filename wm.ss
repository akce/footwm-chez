(import (prefix (ewmh) e.)
        (prefix (xlib) x.)
        (rnrs base))

#;(define dpy (make-parameter))

(define d (x.open-display #f))
(define r (x.default-root d))

;; XFree86 extension.
(define UTF8_STRING (x.set-atom! d "UTF8_STRING" #f))
;; ICCCM atoms.
(define WM_NAME (x.set-atom! d "WM_NAME" #f))
;; EWMH atoms.
(define _NET_WM_NAME (x.set-atom! d "_NET_WM_NAME" #f))
(define WM_CLASS (x.set-atom! d "WM_CLASS" #f))
(define WM_COMMAND (x.set-atom! d "WM_COMMAND" #f))

