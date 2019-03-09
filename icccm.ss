(library (icccm)
  (export
   init-atoms
   atom-ref

   class
   class-hint
   resource

   client-machine
   command
   get-wm-state
   manage-window?
   name
   )
  (import
   (prefix (xutil) xutil.)
   (rnrs base)
   (only (chezscheme) define-values))

  ;; Only define current ICCCM values. See Xutil.h
  ;; MapState
  (define IsUnmapped	0)
  (define IsUnviewable	1)
  (define IsViewable	2)

  ;; WM_STATE window state. See Xutil.h & ICCCM 4.1.3.1
  #;(define Withdrawn	0)
  #;(define Normal	1)
  #;(define Iconic	3)

  #;(define-ftype wm-state
    (struct
     [state integer-32]
     [icon window]))

  (define atom-list
    '(WM_CLASS
      WM_CLIENT_MACHINE
      WM_COMMAND
      WM_NAME
      WM_STATE
      ))
  (define-values
      (init-atoms atom-ref) (xutil.make-atom-manager atom-list))

  (define class-hint
    (lambda (wid)
      ;; This should use XGetClassHint but class hints are just two strings so
      ;; I'll save myself the hassle for now and use the text property stuff I've already got.
      (xutil.property->string* wid (atom-ref 'WM_CLASS))))

  (define class
    (lambda (wid)
      (vector-ref (class-hint wid) 1)))

  (define resource
    (lambda (wid)
      (vector-ref (class-hint wid) 0)))

  (define client-machine
    (lambda (wid)
      (xutil.property->string wid (atom-ref 'WM_CLIENT_MACHINE))))

  (define command
    (lambda (wid)
      (xutil.property->string* wid (atom-ref 'WM_COMMAND))))

  (define get-wm-state
    (lambda (wid)
      (let* ([at (atom-ref 'WM_STATE)]
             [res (xutil.property->u32* wid at at)])
        (if (> (vector-length res) 0)
            (case (vector-ref res 0)
              [(0) 'WITHDRAWN]
              [(1) 'NORMAL]
              [(3) 'ICONIC])
            #f))))

  ;; Return #t if the window should be managed
  (define manage-window?
    (lambda (wid)
      ;; Never manage cases where override_redirect=True.
      ;; We have two extra restrictions when importing windows at program startup.
      ;; Allow import of windows that have MapState=IsViewable, or WM_STATE exists.
      ;; This is because X has an extra restriction about windows to manage. ie, X apps can create children of the
      ;; root window, with their override_redirect=False but never Map them and the window manager then has to ignore them.
      ;; With these checks we're assuming that IsViewable means the window will want to Map itself or that a prior window
      ;; manager decided to manage the window and added a WM_STATE attribute so we'll manage it too.
      (let ([wa (xutil.get-window-attributes wid)])
        (cond
         [(= (xutil.window-attributes-map-state wa) IsViewable)
          #t]
         [(get-wm-state wid)
          #t]
         [(eq? #f (xutil.window-attributes-override-redirect wa))
          #f]
         [else #f]))))

  (define name
    (lambda (wid)
      (xutil.property->string wid (atom-ref 'WM_NAME))))
  )
