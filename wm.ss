(import (prefix (ewmh) e.)
        (prefix (xlib) x.)
        (rnrs base))

#;(define dpy (make-parameter))

(define d (x.open-display #f))
(define r (x.default-root d))
