(library (wm)
  (export
   desktop-add
   desktop-delete
   desktop-rename
   atom-ref
   init-atoms)
(import
 (globals)
 (prefix (xutil) xutil.)
 (rnrs base)
 (only (chezscheme) define-values))

(define-values
    (init-atoms atom-ref) (xutil.make-atom-manager '(FOOT_COMMANDV)))

(define desktop-add
  (lambda (name index)
    (xutil.text-property-set! (root) `("desktop" "insert" ,name ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

(define desktop-delete
  (lambda (index)
    (xutil.text-property-set! (root) `("desktop" "delete" ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

(define desktop-rename
  (lambda (index new-name)
    (xutil.text-property-set! (root) `("desktop" "rename" ,(number->string index) ,new-name) (atom-ref 'FOOT_COMMANDV))))
)
