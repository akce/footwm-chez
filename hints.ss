;; Foot specific wm hints and properties support.
(library (hints)
  (export
   desktop-add-set!
   desktop-delete-set!
   desktop-rename-set!

   command?
   on-command

   init-atoms)
  (import
   (rnrs base)
   (only (chezscheme) define-values)
   (globals)
   (prefix (op) op.)
   (prefix (xutil) xutil.))

  (define-values
      (init-atoms atom-ref) (xutil.make-atom-manager '(FOOT_COMMANDV)))

  (define desktop-add-set!
    (lambda (name index)
      (xutil.text-property-set! (root) `("desktop" "insert" ,name ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

  (define desktop-delete-set!
    (lambda (index)
      (xutil.text-property-set! (root) `("desktop" "delete" ,(number->string index)) (atom-ref 'FOOT_COMMANDV))))

  (define desktop-rename-set!
    (lambda (index new-name)
      (xutil.text-property-set! (root) `("desktop" "rename" ,(number->string index) ,new-name) (atom-ref 'FOOT_COMMANDV))))

  (define command?
    (lambda (atom)
      (eq? atom (atom-ref 'FOOT_COMMANDV))))

  (define on-command
    (lambda (wid)
      (let ([cmd (xutil.property->string* wid (atom-ref 'FOOT_COMMANDV))])
        (if (string=? (list-ref cmd 0) "desktop")
            (cond
             [(string=? (list-ref cmd 1) "insert")
              (op.desktop-insert (list-ref cmd 2) (string->number (list-ref cmd 3)))]
             [(string=? (list-ref cmd 1) "delete")
              (op.desktop-delete (string->number (list-ref cmd 2)))]
             [(string=? (list-ref cmd 1) "rename")
              (op.desktop-rename (string->number (list-ref cmd 2)) (list-ref cmd 3))]))))))
