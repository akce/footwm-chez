;; Footwm Inter Client Communication Conventions (Manual) support.
;;
;; Reference:
;;	https://www.x.org/docs/ICCCM/icccm.pdf
;;
;; Written by Jerry 2019-2021.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm icccm)
  (export
   atom

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
   normal-hints-flags->string
   apply-normal-hints

   wm-hints?
   wm-hints-input
   wm-hints-initial-state
   wm-hints-window-group
   wm-hints-urgency

   ;; WM_HINTS
   get-wm-hints

   ;; WM_CLASS
   class-hint
   class-hint-instance
   class-hint-class

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
   log-create-window?
   window-iconic?
   window-normal?
   show-window!
   iconify-window
   deiconify-window
   show-window
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
   watch-window
   init-window
   send-client-message)
  (import
   (rnrs)
   (only (chezscheme)
     inexact->exact
     define-ftype foreign-free ftype-pointer-address ftype-ref make-ftype-pointer foreign-ref ftype-set! ftype-sizeof)
   (only (footwm ftypes-util) fmem)
   (prefix (footwm util) util.)
   (footwm xlib))

  (define atom
    (make-atom-manager
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
         WM_TAKE_FOCUS)))

  ;;;;;; ICCCM 4.1.2 Client properties.

  ;;;; ICCCM 4.1.2.1 WM_NAME
  (define name
    (lambda (wid)
      (property->string wid (atom 'ref 'WM_NAME))))

  ;;;; ICCCM 4.1.2.2 WM_ICON_NAME
  ;; N/A

  ;;;; ICCCM 4.1.2.3 WM_NORMAL_HINTS
  ;; See also the XGetWMNormalHints manpage for extra info.
  ;; Constraints on window geometry.
  (util.enum size-hints-flags
        (USPosition	0)	; obsolete
        (USSize		1)	; obsolete
        (PPosition	2)
        (PSize		3)
        (PMinSize	4)
        (PMaxSize	5)
        (PResizeInc	6)
        (PAspect	7)
        (PBaseSize	8)
        (PWinGravity	9))

  ;; NOTE: ICCCM defines this struct in terms of 32bit cardinals etc. However, client Xlib uses local machine types.
  ;; NOTE: Accessing the property directly (rather than through XGetWMNormalHints) returns as a list of longs.
  (define-ftype c-size-hints
    (struct
     [flags		long]
     [pad1		long]	; obsolete: x
     [pad2		long]	; obsolete: y
     [pad3		long]	; obsolete: width
     [pad4		long]	; obsolete: height
     [min-width		long]
     [min-height	long]
     [max-width		long]
     [max-height	long]
     [width-inc		long]
     [height-inc	long]
     [min-aspect-x	long]
     [min-aspect-y	long]
     [max-aspect-x	long]
     [max-aspect-y	long]
     [base-width	long]
     [base-height	long]
     [win-gravity	long]))

  (define-record-type size-hints
    (fields
      flags
      min-w min-h
      max-w max-h
      w-inc h-inc
      min-aspect-x min-aspect-y
      max-aspect-x max-aspect-y
      base-w base-h
      win-gravity)
    (protocol
      (lambda (new)
        (lambda (fptr)
          (let ([fl (ftype-ref c-size-hints (flags) fptr)])
            (let-syntax ([flag-ref
                           (syntax-rules ()
                             [(_ flag field)
                              (if (bitwise-bit-set? fl flag)
                                (ftype-ref c-size-hints (field) fptr)
                                #f)])])
              (new
                fl
                (flag-ref PMinSize min-width)
                (flag-ref PMinSize min-height)
                (flag-ref PMaxSize max-width)
                (flag-ref PMaxSize max-height)
                (flag-ref PResizeInc width-inc)
                (flag-ref PResizeInc height-inc)
                (flag-ref PAspect min-aspect-x)
                (flag-ref PAspect min-aspect-y)
                (flag-ref PAspect max-aspect-x)
                (flag-ref PAspect max-aspect-y)
                (flag-ref PBaseSize base-width)
                (flag-ref PBaseSize base-height)
                (flag-ref PWinGravity win-gravity))))))))

  (define get-normal-hints
    (lambda (wid)
      ;; WM_NORMAL_HINTS is of type WM_SIZE_HINTS.
      (let-values ([(ptr len) (get-property-ptr wid (atom 'ref 'WM_NORMAL_HINTS) (atom 'ref 'WM_SIZE_HINTS))])
        (cond
          [ptr
            (let* ([wp (make-ftype-pointer c-size-hints (foreign-ref 'void* ptr 0))]
                   [ret (make-size-hints wp)])
              (x-free (ftype-pointer-address wp))
              (foreign-free ptr)
              ret)]
          [else
            #f]))))

  (define normal-hints-flags->string
    (lambda (nh)
      (let-values ([(op g) (open-string-output-port)])
        (put-string op "(flags")
        (cond
          [nh
            (bit-case (size-hints-flags nh)
              ;; user position.
              ([USPosition	(put-string op " USPosition")]
               [USSize	(put-string op " USSize")]
               [PPosition	(put-string op " PPosition")]
               [PSize	(put-string op " PSize")]
               [PMinSize	(put-string op " PMinSize")]
               [PMaxSize	(put-string op " PMaxSize")]
               [PResizeInc	(put-string op " PResizeInc")]
               [PAspect	(put-string op " PAspect")]
               [PBaseSize	(put-string op " PBaseSize")]
               [PWinGravity	(put-string op " PWinGravity")]))]
          [else
            #f])
        (put-string op ")")
        (g))))

  (define nh-get-base
    (lambda (nh max-geom)
      (let ([flags (size-hints-flags nh)])
        (if (bitwise-bit-set? flags PBaseSize)
            (values (size-hints-base-w nh) (size-hints-base-h nh))
            (if (bitwise-bit-set? flags PMinSize)
                (values (size-hints-min-w nh) (size-hints-min-h nh))
                (values (geometry-width max-geom) (geometry-height max-geom)))))))

  (define nh-get-max
    (lambda (nh max-geom)
      (if (bitwise-bit-set? (size-hints-flags nh) PMaxSize)
          (values (min (size-hints-max-w nh) (geometry-width max-geom))
                  (min (size-hints-max-h nh) (geometry-height max-geom)))
          (values (geometry-width max-geom)
                  (geometry-height max-geom)))))

  (define max-inc-count
    (lambda (inc base top)
      (inexact->exact (floor (/ (- top base) inc)))))

  (define calc-max
    (lambda (inc base top)
      (if (> inc 0)
          (let ([i (max-inc-count inc base top)])
            (+ (* i inc) base))
          top)))

  (define apply-normal-hints
    (lambda (nh max-geom)
      (cond
        [nh
          (let-values ([(flags) (size-hints-flags nh)]
                       [(mw mh) (nh-get-max nh max-geom)])
            (cond
              [(bitwise-bit-set? flags PResizeInc)
               (let-values ([(bw bh) (nh-get-base nh max-geom)])
                 (make-geometry (geometry-x max-geom) (geometry-y max-geom)
                                (calc-max (size-hints-w-inc nh) bw mw)
                                (calc-max (size-hints-h-inc nh) bh mh)))]
              [(bitwise-bit-set? flags PMaxSize)
               (make-geometry (geometry-x max-geom) (geometry-y max-geom) mw mh)]
              [else
                max-geom]))]
        [else
          max-geom])))

  ;;;; ICCCM 4.1.2.4 WM_HINTS.
  ;; Other hints that don't fit anywhere else.
  ;; Only defining the parts potentially useful to this wm.
  (util.enum wm-hints-flags
        (InputHint		0)
        (StateHint		1)	; State to transition to from Withdrawn. ie, Normal or Iconic.
        (WindowGroupHint	6)
        (UrgencyHint		8))

  ;; Xlib returns these structs with all fields as longs so it's simpler to retrieve as a list of longs
  ;; and build a record from that. Especially as we're only interested in a few fields.
  #;(define-ftype c-wm-hints
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
    (fields flags input initial-state window-group)
    (protocol
      (lambda (new)
        (lambda (longs)
          (let ([flags (list-ref longs 0)])
            (let-syntax ([flag-ref
                           (syntax-rules ()
                             [(_ flag field)
                              (if (bitwise-bit-set? flags flag)
                                (list-ref longs field)
                                #f)])])
              (new
                flags
                (eqv? (flag-ref InputHint 1) 1)		; Convert to boolean.
                (flag-ref StateHint 2)
                (flag-ref WindowGroupHint 8))))))))

  ;; Add a pseudo record accessor for the urgency hint.
  (define wm-hints-urgency
    (lambda (wh)
      (and wh (bitwise-bit-set? (wm-hints-flags wh) UrgencyHint))))

  (define get-wm-hints
    (lambda (wid)
      ;; WM_HINTS has type WM_HINTS.
      (let ([longs (property->ulongs wid (atom 'ref 'WM_HINTS) (atom 'ref 'WM_HINTS))])
        (cond
          [(null? longs)
           #f]
          [else
            (make-wm-hints longs)]))))

  ;;;; ICCCM 4.1.2.5 WM_CLASS
  (define class-hint
    (lambda (wid)
      ;; This should use XGetClassHint but class hints are just two strings (see ICCCM 4.1.2.5 WM_CLASS).
      ;; Save some hassle and use the text property stuff.
      (property->string* wid (atom 'ref 'WM_CLASS))))

  ;; ICCCM 4.1.2.5 WM_CLASS refers to this first string as the "instance" even though XClassHint has it as res_name.
  ;; Probably because there are a number of "name" ways to populate this value.
  ;; eg, -name, RESOURCE_NAME, bin name.
  (define class-hint-instance
    (lambda (ch)
      (if (null? ch)
          ""
          (list-ref ch 0))))

  (define class-hint-class
    (lambda (ch)
      (if (null? ch)
          ""
          (list-ref ch 1))))

  ;;;; ICCCM 4.1.2.6 WM_TRANSIENT_FOR
  ;; TODO

  ;;;; ICCCM 4.1.2.7 WM_PROTOCOLS
  (define get-wm-protocols
    (lambda (wid)
      (property->ulongs wid (atom 'ref 'WM_PROTOCOLS) (x-atom 'ref 'ATOM))))

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
      (property->string wid (atom 'ref 'WM_CLIENT_MACHINE))))

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
      (let ([at (atom 'ref 'WM_STATE)])
        (let ([data (property->ulongs wid at at)])
          (if (null? data)
              #f
              (car data))))))

  ;; WM *must* set in top-level windows.
  (define wm-state-set!
    (lambda (wid state)
      (let ([at (atom 'ref 'WM_STATE)])
        (ulongs-property-set! wid at (list state 0) at))))

  ;;;; ICCCM 4.1.3.2 WM_ICON_SIZE
  ;; N/A

  ;;;;;; ICCCM 4.1.4 Changing window state.

  ;; This function now serves as a way to log some created windows of interest.
  ;; It can't do more than that because we only know if we want to manage a window if it maps itself.
  ;; There's a surprising amount of windows created as children of (root) but never mapped!
  ;; Also note that this is the only time we'll see override-redirect windows.
  (define log-create-window?
    (lambda (ev)
      (and
        ;; Always ignore self-managed override-redirect windows.
        (not (xcreatewindowevent-override-redirect ev))
        ;; is it a top-level window? (ie, parent window is root)
        (eqv? (xanyevent-wid (xcreatewindowevent-xany ev)) (root))
        #t)))

  ;;;;;; ICCCM 4.1.4 Changing Window State.

  (define window-iconic?
    (lambda (wid)
      (eq? (get-wm-state wid) IconicState)))

  (define window-normal?
    (lambda (wid)
      (eq? (get-wm-state wid) NormalState)))

  (define show-window!
    (lambda (wid)
      (wm-state-set! wid NormalState)
      (x-map-window wid)))

  (define iconify-window!
    (lambda (wid)
      (wm-state-set! wid IconicState)
      (x-unmap-window wid)))

  (define iconify-window
    (lambda (wid)
      (if (window-normal? wid)
        (iconify-window! wid))))

  (define deiconify-window
    (lambda (wid)
      (if (window-iconic? wid)
        (show-window! wid))))

  (define show-window
    (lambda (wid)
      (unless (window-normal? wid)
        (show-window! wid))))

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
      (send-client-message (root) wid (atom 'ref 'WM_CHANGE_STATE) IconicState SubstructureRedirect)))

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
      (let ([x #f] [y #f] [w #f] [h #f])
        (bit-case (xconfigurerequestevent-value-mask ev)
          ((CWX (set! x (xconfigurerequestevent-x ev)))
           (CWY (set! y (xconfigurerequestevent-y ev)))
           (CWWidth (set! w (xconfigurerequestevent-width ev)))
           (CWHeight (set! h (xconfigurerequestevent-height ev)))))
        ;; Request may just be for stack order update, so resize only if there's a geom change.
        (when (or x y w h)
          (let ([g (make-geometry x y w h)])
            (x-configure-window (xconfigurerequestevent-wid ev) g)
            g)))))

  ;;;;;; ICCCM 4.1.7 Input Focus.
  ;;;; The XGetWMHints manpage also has useful info in its input section.

  (define focus-window
    (lambda (wid)
      (x-set-input-focus wid RevertToNone CurrentTime)
      ;; Don't use the fancy icccm version below as it doesn't always work. eg, Some SDL windowed apps
      ;; won't regain focus and disallow calls to footkeys.
      ;; The simple version above seems to work better, at least so far..
      #;(let* ([h (get-wm-hints wid)]
             [input-hint
              (if h
                  (wm-hints-input h)
                  #f)]
             [take-focus (has-wm-protocol? wid (atom 'ref 'WM_TAKE_FOCUS))])
        (if input-hint
            (if take-focus
                ;; Locally active (input-hint #t) or Globally active (#f): always send WM_TAKE_FOCUS client message.
                (send-client-message wid wid (atom 'ref 'WM_PROTOCOLS) (atom 'ref 'WM_TAKE_FOCUS) StructureNotify)
                ;; Passive: manually set input focus.
                (x-set-input-focus wid RevertToNone CurrentTime))
            ;; PointerRoot mode.
            (focus-root)))))

  (define focus-root
    (lambda ()
      (x-set-input-focus PointerRoot None CurrentTime)))

  ;;;;;; ICCCM 4.2.1.8 Window Deletion.
  (define delete-window
    (lambda (wid)
      (if (has-wm-protocol? wid (atom 'ref 'WM_DELETE_WINDOW))
          (send-client-message wid wid (atom 'ref 'WM_PROTOCOLS) (atom 'ref 'WM_DELETE_WINDOW) NoEvent)
          (x-destroy-window wid))))

  ;;;;;; ICCCM Appendix C Obsolete Session management conventions.

  ;;;; ICCCM C.1.1 WM_COMMAND
  (define command
    (lambda (wid)
      (property->string* wid (atom 'ref 'WM_COMMAND))))

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
      (let ([wa (x-get-window-attributes wid)])
        (if wa
            (if (window-attributes-override-redirect wa)
                #f	;; always ignore override-redirect == true
                (let ([state (get-wm-state wid)])
                  (if state
                      (or (eq? state NormalState) (eq? state IconicState))
                      (= (window-attributes-map-state wa) IsViewable))))
            #f #| window without attributes, probably one we won't want to manage |#))))

  ;; Subscribe to window notification events.
  ;; - StructureNotify: for client window destruction notifications.
  (define watch-window
    (lambda (wid)
      (x-select-input wid StructureNotify)))

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
        (x-send-event wid #f event-mask &ev)))))
