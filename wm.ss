(library (wm)
  (export
   desktop-add
   desktop-delete
   desktop-rename
   atom-list
   atom-ref
   init-atoms)
(import
 (prefix (xutil) xutil.)
 (rnrs base))

(define atom-list
  '(FOOT_COMMANDV))

(define atoms (xutil.make-atoms))
(define init-atoms
  (lambda ()
    (xutil.init-atoms atoms atom-list)))
(define atom-ref (xutil.make-atom-ref atoms))

(define desktop-add
  (lambda (r name index)
    (xutil.text-property-set! r `("desktop" "insert" ,name ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

(define desktop-delete
  (lambda (r index)
    (xutil.text-property-set! r `("desktop" "delete" ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

(define desktop-rename
  (lambda (r index new-name)
    (xutil.text-property-set! r `("desktop" "rename" ,(number->string index) ,new-name) (atom-ref 'FOOT_COMMANDV))))
)
