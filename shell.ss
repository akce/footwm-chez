(library (shell)
  (export
   desktops
   windows
   )
  (import (prefix (ewmh) e.)
          (prefix (icccm) i.)
          (prefix (xutil) x.)
          (xlib)
          (rnrs)
          (only (chezscheme) format))

  (define vector-enumerate
    (lambda (v)
      (let* ([len (vector-length v)]
             [res (make-vector len)])
        (let loop ((i 0))
          (cond
           [(fx=? i len) res]
           [else
             (vector-set! res i i)
             (loop (fx+ i 1))])))))

  (define desktops
    (lambda (d r)
      (let ([names (e.desktop-names d r)])
        (vector-for-each
         (lambda (desk)
           (display (desktop-display-string desk))
           (newline))
         (vector-map cons (vector-enumerate names) names)))))

  ;; prints out the windows list in most-recently-used order.
  (define windows
    (lambda (d r)
      (vector-for-each
       (lambda (wid)
         (display (window-display-string d r wid))
         (newline))
       (e.client-list-stacking d r))))

  (define desktop-display-string
    (lambda (desk)
      (format "~d ~a" (car desk) (cdr desk))))

  (define window-display-string
    (lambda (d r wid)
      (let ([c (i.class-hint d wid)])
          ;; window id desktop resource class title
          (format
           "#x~x ~a ~a ~a ~a"
           wid
           (e.window-desktop d wid)
       	   (vector-ref c 0)
           (vector-ref c 1)
           (e.name d wid)))))
  )
