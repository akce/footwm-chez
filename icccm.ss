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

   wm-hints?
   wm-hints-input
   wm-hints-initial-state
   wm-hints-window-group
   wm-hints-urgency

   ;; WM_HINTS
   get-wm-hints

   ;; WM_CLASS
   class-hint
   class
   instance

   ;; WM_PROTOCOLS
   get-wm-protocols
   has-wm-protocol?

   ;; WM_CLIENT_MACHINE
   client-machine

   ;; WM_STATE
   WithdrawnState
   NormalState
   IconicState

   get-wm-state
   wm-state-set!

   ;; Changing window state.
   on-create-window
   iconify-window
   deiconify-window
   show-window
   on-map-request
   on-unmap
   client-iconify-message
   on-configure-request

   focus-window
   focus-root
   delete-window

   ;; WM_COMMAND
   command

   ;; Misc util functions.
   manage-window?
   init-window
   send-client-message)
  (import
   (rnrs)
   (only (chezscheme)
     define-values define-ftype foreign-free ftype-ref make-ftype-pointer foreign-ref ftype-set! ftype-sizeof unlock-object)
   (only (ftypes-util) fmem)
   (globals)
   (prefix (util) util.)
   (xlib)
   (prefix (xutil) xutil.))

  (define atom-list
    '(WM_CHANGE_STATE
      WM_CLASS
      WM_CLIENT_MACHINE
      WM_COMMAND
      WM_DELETE_WINDOW
      WM_HINTS
      WM_NAME
      WM_NORMAL_HINTS
      WM_PROTOCOLS
      WM_SIZE_HINTS
      WM_STATE
      WM_TAKE_FOCUS))
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
  ;; See also the XGetWMNormalHints manpage for extra info.
  ;; Constraints on window geometry.
  (util.enum size-hints-flags
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

  ;; NOTE: ICCCM defines this struct in terms of 32bit cardinals etc. However, client Xlib uses local machine types.
  (define-ftype c-size-hints
    (struct
     [flags		long]
     [pad1		int]	; obsolete: x
     [pad2		int]	; obsolete: y
     [pad3		int]	; obsolete: width
     [pad4		int]	; obsolete: height
     [min-width		int]
     [min-height	int]
     [max-width		int]
     [max-height	int]
     [width-inc		int]
     [height-inc	int]
     [min-aspect-x	int]
     [min-aspect-y	int]
     [max-aspect-x	int]
     [max-aspect-y	int]
     [base-width	int]
     [base-height	int]
     [win-gravity	int]))

  (define-record-type size-hints
    (fields flags min-w min-h max-w max-h w-inc h-inc min-aspect-x min-aspect-y max-aspect-x max-aspect-y base-w base-h win-gravity))

  (define get-normal-hints
    (lambda (wid)
      ;; WM_NORMAL_HINTS is of type WM_SIZE_HINTS.
      (let ([ptrlen (xutil.get-property-ptr wid (atom-ref 'WM_NORMAL_HINTS) (atom-ref 'WM_SIZE_HINTS))])
        (if ptrlen
            (let* ([ptr (car ptrlen)]
                   [*ptr (foreign-ref 'void* ptr 0)]
                   [wp (make-ftype-pointer c-size-hints *ptr)]
                   ;; Make the record.
                   [ret (make-size-hints
                         (ftype-ref c-size-hints (flags) wp)
                         (ftype-ref c-size-hints (min-width) wp)
                         (ftype-ref c-size-hints (min-height) wp)
                         (ftype-ref c-size-hints (max-width) wp)
                         (ftype-ref c-size-hints (max-height) wp)
                         (ftype-ref c-size-hints (width-inc) wp)
                         (ftype-ref c-size-hints (height-inc) wp)
                         (ftype-ref c-size-hints (min-aspect-x) wp)
                         (ftype-ref c-size-hints (min-aspect-y) wp)
                         (ftype-ref c-size-hints (max-aspect-x) wp)
                         (ftype-ref c-size-hints (max-aspect-y) wp)
                         (ftype-ref c-size-hints (base-width) wp)
                         (ftype-ref c-size-hints (base-height) wp)
                         (ftype-ref c-size-hints (win-gravity) wp))])
              (XFree *ptr)
              (unlock-object *ptr)
              (foreign-free ptr)
              ret)
            #f))))

  ;;;; ICCCM 4.1.2.4 WM_HINTS.
  ;; Other hints that don't fit anywhere else.
  ;; Only defining the parts potentially useful to this wm.
  (util.enum wm-hints-flags
        (InputHint		0)
        (StateHint		1)	; State to transition to from Withdrawn. ie, Normal or Iconic.
        (WindowGroupHint	6)
        (UrgencyHint		8))

  ;; NOTE: ICCCM defines this struct in terms of 32bit cardinals etc. However, client Xlib uses local machine types.
  (define-ftype c-wm-hints
    (struct
     [flags		long]
     [input		boolean]	; client input model
     [initial-state	int]
     [icon-pixmap	pixmap]
     [icon-window	window]
     [icon-x		int]
     [icon-y		int]
     [icon-mask		pixmap]
     [window-group	window]))

  (define-record-type wm-hints
    (fields flags input initial-state window-group))

  ;; Add a pseudo record accessor for the urgency hint.
  (define wm-hints-urgency
    (lambda (wh)
      (bitwise-bit-set? (wm-hints-flags wh) UrgencyHint)))

  (define get-wm-hints
    (lambda (wid)
      ;; WM_HINTS has type WM_HINTS.
      (let* ([at (atom-ref 'WM_HINTS)]
             [ptrlen (xutil.get-property-ptr wid at at)])
        (if ptrlen
            (let* ([ptr (car ptrlen)]
                   [*ptr (foreign-ref 'void* ptr 0)]
                   [wp (make-ftype-pointer c-wm-hints *ptr)]
                   [fl (ftype-ref c-wm-hints (flags) wp)]
                   ;; Make the record.
                   [ret (make-wm-hints
                         fl
                         (if (bitwise-bit-set? fl InputHint) (ftype-ref c-wm-hints (input) wp) #f)
                         (if (bitwise-bit-set? fl StateHint) (ftype-ref c-wm-hints (initial-state) wp) #f)
                         (if (bitwise-bit-set? fl WindowGroupHint) (ftype-ref c-wm-hints (window-group) wp) #f))])
              (XFree *ptr)
              (unlock-object *ptr)
              (foreign-free ptr)
              ret)
            #f))))

  ;;;; ICCCM 4.1.2.5 WM_CLASS
  (define class-hint
    (lambda (wid)
      ;; This should use XGetClassHint but class hints are just two strings (see ICCCM 4.1.2.5 WM_CLASS).
      ;; Save some hassle and use the text property stuff.
      (xutil.property->string* wid (atom-ref 'WM_CLASS))))

  (define class
    (lambda (wid)
      (list-ref (class-hint wid) 1)))

  (define instance
    (lambda (wid)
      ;; ICCCM 4.1.2.5 WM_CLASS refers to this first string as the "instance" even though XClassHint has it as res_name.
      ;; Probably because there are a number of "name" ways to populate this value.
      ;; eg, -name, RESOURCE_NAME, bin name.
      (list-ref (class-hint wid) 0)))

  ;;;; ICCCM 4.1.2.6 WM_TRANSIENT_FOR
  ;; TODO

  ;;;; ICCCM 4.1.2.7 WM_PROTOCOLS
  (define get-wm-protocols
    (lambda (wid)
      (xutil.property->ulongs wid (atom-ref 'WM_PROTOCOLS) XA-ATOM)))

  (define has-wm-protocol?
    (lambda (wid proto-atom)
      (if (memq proto-atom (get-wm-protocols wid))
          #t
          #f)))

  ;;;; ICCCM 4.1.2.8 WM_COLORMAP_WINDOWS
  ;; TODO

  ;;;; ICCCM 4.1.2.9 WM_CLIENT_MACHINE
  (define client-machine
    (lambda (wid)
      (xutil.property->string wid (atom-ref 'WM_CLIENT_MACHINE))))

  ;;;;; ICCCM 4.1.3 Window manager properties.

  ;;;; ICCCM 4.1.3.1 WM_STATE
  ;; Window managers place WM_STATE on all non-Withdrawn top-level windows.

  ;; NOTE: ICCCM defines type WM_STATE as (struct [state card32] [icon window]) but it's not defined in any X header.
  ;; xprop treats it as 2 longs; long is also consistent with size=32 values as described in XChangeProperty.
  ;; We'll treat it the same way.

  (util.enum wm-state-state
        (WithdrawnState	0)	;; Hidden.
        (NormalState	1)	;; Client should animate its window.
        (IconicState	3))	;; Client should animate its icon window.

  (define get-wm-state
    (lambda (wid)
      (let ([at (atom-ref 'WM_STATE)])
        (let ([data (xutil.property->ulongs wid at at)])
          (if (null? data)
              #f
              (car data))))))

  ;; WM *must* set in top-level windows.
  (define wm-state-set!
    (lambda (wid state)
      (let ([at (atom-ref 'WM_STATE)])
        (xutil.ulongs-property-set! wid at (list state 0) at))))

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

  ;;;;;; ICCCM 4.1.4 Changing Window State.
  (define iconify-window
    (lambda (wid)
      (if (eq? (get-wm-state wid) NormalState)
          (begin
            (wm-state-set! wid IconicState)
            (XUnmapWindow (current-display) wid)
            #t)
          #f)))

  (define deiconify-window
    (lambda (wid)
      (if (eq? (get-wm-state wid) IconicState)
          (begin
            (wm-state-set! wid NormalState)
            (XMapWindow (current-display) wid)
            #t)
          #f)))

  (define show-window
    (lambda (wid)
      (unless (eq? (get-wm-state wid) NormalState)
        (wm-state-set! wid NormalState)
        (XMapWindow (current-display) wid))))

  (define on-map-request
    (lambda (ev)
      ;; MapRequest event.
      ;; Received when SubstructureRedirect set on a window, and a child of that window wants to Map,
      ;; (and only where child has override-redirect=false).
      ;; Map the window and set the wm-state.
      (let ([wid (xmaprequestevent-wid ev)])
        (wm-state-set! wid NormalState)
        (watch-window wid)
        (XMapWindow (current-display) wid))))

  (define on-unmap
    (lambda (ev)
      (let ([wid (xunmapevent-wid ev)])
        ;; Need to still manage Unmapped windows that are going ICONIC.
        ;; This wm will have set the state to ICONIC if we're hiding the window so no need to do anything.
        ;; However, a NORMAL window being unmapped could mean that the client is shutting down. We'll set
        ;; to WITHDRAWN and see what happens.
        (unless (eq? (get-wm-state wid) IconicState)
            (wm-state-set! wid WithdrawnState)))))

  ;; Client -> WM message: Iconify window request.
  ;; Client wishes WM to transition window from NORMAL to ICONIC state.
  (define client-iconify-message
    (lambda (wid)
      ;; Sends client message to the WM as the WM is the only client watching SubstructureRedirect on the root window.
      (send-client-message (root) wid (atom-ref 'WM_CHANGE_STATE) IconicState SubstructureRedirect)))

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
      ;; We'll resize (if necessary) when arranging the windows.
      (let ([x #f] [y #f] [w #f] [h #f])
          (bit-case (xconfigurerequestevent-value-mask ev)
            ((CWX (set! x (xconfigurerequestevent-x ev)))
             (CWY (set! y (xconfigurerequestevent-y ev)))
             (CWWidth (set! w (xconfigurerequestevent-width ev)))
             (CWHeight (set! h (xconfigurerequestevent-height ev)))))
          (xutil.resize-window (xconfigurerequestevent-wid ev) x y w h))))

  ;;;;;; ICCCM 4.1.7 Input Focus.
  ;;;; The XGetWMHints manpage also has useful info in its input section.
  (define focus-window
    (lambda (wid)
      (let* ([h (get-wm-hints wid)]
             [input-hint
              (if h
                  (wm-hints-input h)
                  #f)]
             [take-focus (has-wm-protocol? wid (atom-ref 'WM_TAKE_FOCUS))])
        (if input-hint
            (if take-focus
                ;; Locally active (input-hint #t) or Globally active (#f): always send WM_TAKE_FOCUS client message.
                (send-client-message wid wid (atom-ref 'WM_PROTOCOLS) (atom-ref 'WM_TAKE_FOCUS) StructureNotify)
                ;; Passive: manually set input focus.
                (XSetInputFocus (current-display) wid RevertToNone CurrentTime))
            ;; PointerRoot mode.
            (focus-root)))))

  (define focus-root
    (lambda ()
      (XSetInputFocus (current-display) PointerRoot None CurrentTime)))

  ;;;;;; ICCCM 4.2.1.8 Window Deletion.
  (define delete-window
    (lambda (wid)
      (if (has-wm-protocol? wid (atom-ref 'WM_DELETE_WINDOW))
          (send-client-message wid wid (atom-ref 'WM_PROTOCOLS) (atom-ref 'WM_DELETE_WINDOW) NoEvent)
          (XDestroyWindow (current-display) wid))))

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
                (let ([state (get-wm-state wid)])
                  (if state
                      (or (eq? state NormalState) (eq? state IconicState))
                      (= (xutil.window-attributes-map-state wa) IsViewable))))
            #f #| window without attributes, probably one we won't want to manage |#))))

  ;; Subscribe to window notification events.
  ;; - StructureNotify: for client window destruction notifications.
  (define watch-window
    (lambda (wid)
      (xutil.select-input wid StructureNotify)))

  ;; Initialise a window according to ICCCM requirements.
  (define init-window
    (lambda (wid)
      ;; Subscribe to window event changes.
      (watch-window wid)
      ;; Set the WM_STATE property.
      ;; Always set new state to NORMAL, do nothing if there's already one.
      ;; Assumes that the layout/arrange function will update to relevant values.
      (unless (get-wm-state wid)
        (wm-state-set! wid NormalState))))

  (define send-client-message
    (lambda (wid msgwid type sub-type event-mask)
      (fmem ([ev &ev XEvent])
        (ftype-set! XEvent (client-message xany type) &ev ClientMessage)
        (ftype-set! XEvent (client-message xany wid) &ev msgwid)
        (ftype-set! XEvent (client-message xany send-event) &ev #t)
        (ftype-set! XEvent (client-message message-type) &ev type)
        (ftype-set! XEvent (client-message format) &ev 32)
        (ftype-set! XEvent (client-message data l 0) &ev sub-type)
        (ftype-set! XEvent (client-message data l 1) &ev CurrentTime)
        (XSendEvent (current-display) wid #f event-mask &ev)))))
