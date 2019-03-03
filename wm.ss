(import (prefix (ewmh) ewmh.)
        (prefix (icccm) icccm.)
        (xlib)
        (prefix (xutil) xutil.)
        (rnrs base))

(define d (xutil.open))
(define r (XDefaultRootWindow d))

;; XFree86 extension.
(define UTF8_STRING (XInternAtom d "UTF8_STRING" #f))

(define atom-list
  '(FOOT_COMMANDV))

(define atoms (xutil.make-atoms))
(define init-atoms
  (lambda (d)
    (xutil.init-atoms d atoms atom-list)))
(define atom-ref (xutil.make-atom-ref atoms))

(icccm.init-atoms d)
(ewmh.init-atoms d)
(init-atoms d)

(define desktop-add
  (lambda (d r name index)
    (xutil.text-property-set! d r `("desktop" "insert" ,name ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

(define desktop-delete
  (lambda (d r index)
    (xutil.text-property-set! d r `("desktop" "delete" ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

(define desktop-rename
  (lambda (d r index new-name)
    (xutil.text-property-set! d r `("desktop" "rename" ,(number->string index) ,new-name) (atom-ref 'FOOT_COMMANDV))))
