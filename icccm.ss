;;;; X - Inter Client Communication Conventions (Manual) support.
;;;; Reference:
;;;;	https://www.x.org/docs/ICCCM/icccm.pdf
(library (icccm)
  (export
   init-atoms
   atom-ref

   ;; WM_NAME
   name

   ;; WM_NORMAL_HINTS
   USPosition
   USSize
   PPosition
   PSize
   PMinSize
   PMaxSize
   PResizeInc
   PAspect
   PBaseSize
   PWinGravity

   size-hints?
   size-hints-flags
   size-hints-min-w
   size-hints-min-h
   size-hints-max-w
   size-hints-max-h
   size-hints-w-inc
   size-hints-h-inc
   size-hints-min-aspect-x
   size-hints-min-aspect-y
   size-hints-max-aspect-x
   size-hints-max-aspect-y
   size-hints-base-w
   size-hints-base-h
   size-hints-win-gravity

   get-normal-hints

   InputHint
   StateHint
   WindowGroupHint
   UrgencyHint

   wm-hints?
   wm-hints-flags
   wm-hints-input
   wm-hints-initial-state
   wm-hints-window-group

   ;; WM_HINTS
   get-wm-hints

   ;; WM_CLASS
   class-hint
   class
   instance

   ;; WM_CLIENT_MACHINE
   client-machine

   ;; WM_STATE
   WithdrawnState
   NormalState
   IconicState

   get-wm-state
   get-state
   wm-state-set!

   ;; Changing window state.
   on-create-window
   on-configure-request

   ;; WM_COMMAND
   command

   ;; Misc util functions.
   manage-window?
   init-window)
  (import
   (globals)
   (util)
   (xlib)
   (prefix (xutil) xutil.)
   (chezscheme))

  (define atom-list
    '(WM_CLASS
      WM_CLIENT_MACHINE
      WM_COMMAND
      WM_NAME
      WM_NORMAL_HINTS
      WM_SIZE_HINTS
      WM_STATE
      ))
  (define-values
      (init-atoms atom-ref) (xutil.make-atom-manager atom-list))

  ;;;;;; ICCCM 4.1.2 Client properties.

  ;;;; ICCCM 4.1.2.1 WM_NAME
  (define name
    (lambda (wid)
      (xutil.property->string wid (atom-ref 'WM_NAME))))

  ;;;; ICCCM 4.1.2.2 WM_ICON_NAME
  ;; N/A

  ;;;; ICCCM 4.1.2.3 WM_NORMAL_HINTS
  ;; Constraints on window geometry.
  (bitmap size-hints-flags
        (USPosition	0)
        (USSize		1)
        (PPosition	2)
        (PSize		3)
        (PMinSize	4)
        (PMaxSize	5)
        (PResizeInc	6)
        (PAspect	7)
        (PBaseSize	8)
        (PWinGravity	9))

  (define-record-type size-hints
    (fields flags min-w min-h max-w max-h w-inc h-inc min-aspect-x min-aspect-y max-aspect-x max-aspect-y base-w base-h win-gravity))

  (define get-normal-hints
    (lambda (wid)
      ;; WM_NORMAL_HINTS is of type WM_SIZE_HINTS.
      ;; But, WM_SIZE_HINTS is just a list of 32bit integers, so save defining a C struct by grabbing the vector
      ;; and creating the record from it.
      (let ([buf (xutil.property->u32* wid (atom-ref 'WM_NORMAL_HINTS) (atom-ref 'WM_SIZE_HINTS))])
        (if (> (vector-length buf) 0)
            ;; Maybe not defining the c-struct wasn't such a good idea......
            (make-size-hints
             (vector-ref buf 0)		; flags
             (vector-ref buf 5) (vector-ref buf 6) (vector-ref buf 7) (vector-ref buf 8) ; min/max w/h
             (vector-ref buf 9) (vector-ref buf 10)	; width/height increment
             (vector-ref buf 11) (vector-ref buf 12) (vector-ref buf 13) (vector-ref buf 14)	; aspect min/max (x,y)
             (vector-ref buf 15) (vector-ref buf 16)	; base width/height
             (vector-ref buf 17))	; win-gravity
            #f))))

  ;;;; ICCCM 4.1.2.4 WM_HINTS.
  ;; Other hints that don't fit anywhere else.
  (bitmap wm-hints-flags
        (InputHint		0)
        (StateHint		1)	; State to transition to from Withdrawn. ie, Normal or Iconic.
        ;IconPixmapHint
        ;IconWindowHint
        ;IconPositionHint
        ;IconMaskHint
        (WindowGroupHint	6)
        ;; MessageHint is obsolete.
        (UrgencyHint		8))

  (define-record-type wm-hints
    (fields flags input initial-state window-group))

  (define get-wm-hints
    (lambda (wid)
      ;; WM_HINTS has type WM_HINTS.
      (let ([buf (xutil.property->u32* wid (atom-ref 'WM_HINTS) (atom-ref 'WM_HINTS))])
        (if (> (vector-length buf) 0)
            ;; Make the record.
            (make-wm-hints
             (vector-ref buf 0)		; flags
             (vector-ref buf 1)		; client input model
             (vector-ref buf 2)		; initial state
             ;; skip the icon stuff since this wm doesn't support icons.
             (vector-ref buf 8))	; window group
            #f))))

  ;;;; ICCCM 4.1.2.5 WM_CLASS
  (define class-hint
    (lambda (wid)
      ;; This should use XGetClassHint but class hints are just two strings (see ICCCM 4.1.2.5 WM_CLASS).
      ;; Save some hassle and use the text property stuff.
      (xutil.property->string* wid (atom-ref 'WM_CLASS))))

  (define class
    (lambda (wid)
      (vector-ref (class-hint wid) 1)))

  (define instance
    (lambda (wid)
      ;; ICCCM 4.1.2.5 WM_CLASS refers to this first string as the "instance" even though XClassHint has it as res_name.
      ;; Probably because there are a number of "name" ways to populate this value.
      ;; eg, -name, RESOURCE_NAME, bin name.
      (vector-ref (class-hint wid) 0)))

  ;;;; ICCCM 4.1.2.6 WM_TRANSIENT_FOR
  ;; TODO

  ;;;; ICCCM 4.1.2.7 WM_PROTOCOLS
  ;; TODO

  ;;;; ICCCM 4.1.2.8 WM_COLORMAP_WINDOWS
  ;; TODO

  ;;;; ICCCM 4.1.2.9 WM_CLIENT_MACHINE
  (define client-machine
    (lambda (wid)
      (xutil.property->string wid (atom-ref 'WM_CLIENT_MACHINE))))

  ;;;;; ICCCM 4.1.3 Window manager properties.

  ;;;; ICCCM 4.1.3.1 WM_STATE
  ;; Window managers place WM_STATE on all non-Withdrawn top-level windows.

  (enum wm-state-state
        (WithdrawnState	0)	;; Hidden.
        (NormalState	1)	;; Client should animate its window.
        (IconicState	3))	;; Client should animate its icon window.

  (define get-wm-state
    (lambda (wid)
      (let* ([at (atom-ref 'WM_STATE)]
             [res (xutil.property->u32* wid at at)])
        (if (> (vector-length res) 0)
            res
            #f))))

  (define get-state
    (lambda (wid)
      (let ([res (get-wm-state wid)])
        ;; The second WM_STATE field is the icon window. Ignore as we don't support icons.
        (if res
            (case (vector-ref res 0)
              [(0)	'WITHDRAWN]
              [(1)	'NORMAL]
              [(3)	'ICONIC]
              [else	#f])
            #f))))

  ;; WM *must* set in top-level windows.
  (define wm-state-set!
    (lambda (wid state)
      (let ([at (atom-ref 'WM_STATE)])
        (xutil.u32*-property-set! wid at (vector state 0) at))))

  ;;;; ICCCM 4.1.3.2 WM_ICON_SIZE
  ;; N/A

  ;;;;;; ICCCM 4.1.4 Changing window state.

  ;; From ICCCM (emphasis mine): Newly created *top-level* windows are in the Withdrawn state.
  (define on-create-window
    (lambda (ev)
      ;; Always ignore self-managed override-redirect windows.
      (unless (xcreatewindowevent-override-redirect ev)
        ;; is it a top-level window?
        (if (= (xanyevent-wid (xcreatewindowevent-xany ev)) (xcreatewindowevent-wid ev))
            ;; yes: add WM_STATE set to WithdrawnState.
            (wm-state-set! (xcreatewindowevent-wid ev) WithdrawnState)))))

  ;;;;;; ICCCM 4.1.5 Configuring the Window.
  (define-syntax bit-case
    (syntax-rules ()
      [(_ var ((bit expr ...) ...))
       (let ([v var])
         (if (bitwise-bit-set? v bit)
           (begin
             expr ...)) ...)]))

  (define on-configure-request
    (lambda (ev)
      ;; Honour all configure requests as not all clients behave nicely otherwise.
      ;; We'll resize (if necessary) in the configure notify handler.
      (let ([change-mask 0])
        (fmem ([changes &changes XWindowChanges])
          (bit-case (xconfigurerequestevent-value-mask ev)
            ((CWX (ftype-set! XWindowChanges (x) changes (xconfigurerequestevent-x ev))
                  (set! change-mask (bitwise-copy-bit change-mask CWX 1)))
             (CWY (ftype-set! XWindowChanges (y) changes (xconfigurerequestevent-y ev))
                  (set! change-mask (bitwise-copy-bit change-mask CWY 1)))
             (CWWidth (ftype-set! XWindowChanges (width) changes (xconfigurerequestevent-width ev))
                      (set! change-mask (bitwise-copy-bit change-mask CWWidth 1)))
             (CWHeight (ftype-set! XWindowChanges (height) changes (xconfigurerequestevent-height ev))
                       (set! change-mask (bitwise-copy-bit change-mask CWHeight 1)))))
          (XConfigureWindow (current-display) (xconfigurerequestevent-wid ev) change-mask &changes)
          (display (format "#x~x XConfigureRequestEvent change-mask #b~b~n" (xconfigurerequestevent-wid ev) change-mask))))))

  ;;;;;; ICCCM Appendix C Obsolete Session management conventions.

  ;;;; ICCCM C.1.1 WM_COMMAND
  (define command
    (lambda (wid)
      (xutil.property->string* wid (atom-ref 'WM_COMMAND))))

  ;;;;;; Other/Misc functions.

  ;; Return #t if the window should be managed. Use this to import windows at wm startup.
  (define manage-window?
    (lambda (wid)
      ;; wid is assumed to be a child of the root window and this function does not check.
      ;; Rules for importing/managing a window:
      ;; - never manage windows with override-redirect #t
      ;; - windows with WM_STATE will have been managed by a previous WM so:
      ;;   - import those that were in NORMAL or ICONIC state
      ;;   - ignore those in WITHDRAWN state
      ;; - windows without WM_STATE, manage those that could be viewable.
      ;;   ie, map-state = IsViewable. Ignore unmapped and unviewable windows.
      (let ([wa (xutil.get-window-attributes wid)])
        (if wa
            (if (xutil.window-attributes-override-redirect wa)
                #f	;; always ignore override-redirect == true
                (let ([state (get-state wid)])
                  (if state
                      (or (eq? state 'NORMAL) (eq? state 'ICONIC))
                      (= (xutil.window-attributes-map-state wa) IsViewable))))
            #f #| window without attributes, probably one we won't want to manage |#))))

  ;; Initialise a window according to ICCCM requirements.
  (define init-window
    (lambda (wid)
      ;; Really only involves making sure the window has WM_STATE property.
      ;; Always set new state to NORMAL, do nothing if there's already one.
      ;; Assumes that the layout/arrange function will update to relevant values.
      (unless (get-wm-state wid)
        (wm-state-set! wid NormalState)))))
