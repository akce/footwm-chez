;; Footwm Chez scheme XLib layer.
;;
;; Written by Jerry 2019-2021.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm xlib)
  (export
   current-display
   root

   make-atom-manager

   ;; Custom geometry record.
   make-geometry
   geometry-x
   geometry-y
   geometry-width
   geometry-height
   geometry=?
   xconfigureevent-geometry
   xconfigurerequestevent-geometry

   ;; XWindowAttributes.
   window-attributes
   window-attributes-geom
   window-attributes-override-redirect
   window-attributes-map-state

   XAnyEvent
   XEvent

   XErrorEvent
   make-xerrorevent
   xerrorevent?
   xerrorevent-type
   xerrorevent-d
   xerrorevent-resourceid
   xerrorevent-serial
   xerrorevent-error-code
   xerrorevent-request-code
   xerrorevent-minor-code

   make-xanyevent
   xanyevent?
   xanyevent-type
   xanyevent-serial
   xanyevent-send-event
   xanyevent-d
   xanyevent-wid

   XClientMessageEvent
   ClientMessage
   make-xclientmessageevent
   xclientmessageevent?
   xclientmessageevent-xany
   xclientmessageevent-message-type
   xclientmessageevent-format
   xclientmessageevent-data

   XConfigureEvent
   ConfigureNotify
   make-xconfigureevent
   xconfigureevent?
   xconfigureevent-xany
   xconfigureevent-wid
   xconfigureevent-x
   xconfigureevent-y
   xconfigureevent-width
   xconfigureevent-height
   xconfigureevent-border-width
   xconfigureevent-above
   xconfigureevent-override-redirect

   XConfigureRequestEvent
   ConfigureRequest
   make-xconfigurerequestevent
   xconfigurerequestevent?
   xconfigurerequestevent-xany
   xconfigurerequestevent-wid
   xconfigurerequestevent-x
   xconfigurerequestevent-y
   xconfigurerequestevent-width
   xconfigurerequestevent-height
   xconfigurerequestevent-border-width
   xconfigurerequestevent-above
   xconfigurerequestevent-detail
   xconfigurerequestevent-value-mask

   XCreateWindowEvent
   CreateNotify
   make-xcreatewindowevent
   xcreatewindowevent?
   xcreatewindowevent-xany
   xcreatewindowevent-wid
   xcreatewindowevent-x
   xcreatewindowevent-y
   xcreatewindowevent-width
   xcreatewindowevent-height
   xcreatewindowevent-border-width
   xcreatewindowevent-override-redirect

   XDestroyWindowEvent
   DestroyNotify
   make-xdestroywindowevent
   xdestroywindowevent?
   xdestroywindowevent-xany
   xdestroywindowevent-wid

   KeyReleaseEvent
   XKeyEvent
   KeyPressEvent	;; TODO this should be KeyPress but there's already one defined. Need to scope these.
   make-xkeyevent
   xkeyevent?
   xkeyevent-xany
   xkeyevent-root
   xkeyevent-subwindow
   xkeyevent-time
   xkeyevent-x
   xkeyevent-y
   xkeyevent-x-root
   xkeyevent-y-root
   xkeyevent-state
   xkeyevent-keycode
   xkeyevent-same-screen

   XMapEvent
   MapNotify
   make-xmapevent
   xmapevent?
   xmapevent-xany
   xmapevent-wid
   xmapevent-override-redirect

   XMapRequestEvent
   MapRequest
   make-xmaprequestevent
   xmaprequestevent?
   xmaprequestevent-xany
   xmaprequestevent-wid

   XPropertyEvent
   PropertyNotify
   make-xpropertyevent
   xpropertyevent?
   xpropertyevent-xany
   xpropertyevent-propatom
   xpropertyevent-time
   xpropertyevent-state

   XUnmapEvent
   UnmapNotify
   make-xunmapevent
   xunmapevent?
   xunmapevent-xany
   xunmapevent-wid
   xunmapevent-from-configure

   XTextProperty
   XWindowAttributes
   XWindowChanges

   x-atom

   Success
   BadAccess

   ;; XWindowAttributes map-state
   IsUnmapped
   IsUnviewable
   IsViewable

   NoEvent
   KeyPress
   KeyRelease
   ButtonPress
   ButtonRelease
   EnterWindow
   LeaveWindow
   PointerMotion
   PointerMotionHint
   Button1Motion
   Button2Motion
   Button3Motion
   Button4Motion
   Button5Motion
   ButtonMotion
   KeymapState
   Exposure
   VisibilityChange
   StructureNotify
   ResizeRedirect
   SubstructureNotify
   SubstructureRedirect
   FocusChange
   PropertyChange
   ColormapChange
   OwnerGrabButton

   ;; configure-window-request
   CWX
   CWY
   CWWidth
   CWHeight
   CWBorderWidth
   CWSibling
   CWStackMode

   ;; stacking method
   Above
   Below
   TopIf
   BottomIf

   ;; input focus
   RevertToNone
   RevertToPointerRoot
   RevertToParent

   Shift
   Lock
   Control
   Mod1
   Mod2
   Mod3
   Mod4
   Mod5

   GrabModeSync
   GrabModeAsync

   AnyModifier

   None
   AnyKey
   CurrentTime
   NoSymbol
   PointerRoot

   UTF8String

   x-create-simple-window
   x-change-property
   x-delete-property
   x-close-display
   x-configure-window
   x-default-root-window
   x-destroy-window
   x-flush
   x-free
   x-free-string-list
   x-get-atom-name
   x-get-text-property
   x-get-window-attributes
   x-get-window-property
   x-grab-key
   x-grab-server
   x-intern-atom
   x-keycode-to-keysym
   x-keysym-to-keycode
   x-keysym-to-string
   x-lower-window
   x-map-window
   x-move-resize-window
   x-next-event
   x-with-next-event
   x-open-display
   x-query-tree
   x-raise-window
   x-select-input
   x-send-event
   x-set-error-handler
   x-set-input-focus
   x-set-text-property
   x-string-to-keysym
   x-sync
   x-ungrab-key
   x-ungrab-server
   x-unmap-window
   xutf8-text-list-to-text-property
   xutf8-text-property-to-text-list

   card32
   dpy*
   pixmap
   status
   u8*
   u8**
   window
   window*

   ;; Property helper functions.
   cardinal-set!
   property->string
   property->string*
   property->ulongs
   ulongs-property-set!
   text-property-set!

   define-root-property
   first-or-false

   send-message-cardinal)
  (import
   (chezscheme)
   (footwm ftypes-util)
   (footwm util))

  (define library-init
    (load-shared-object "libX11.so.6"))

  (define current-display
    (make-parameter #f))

  (define root
    (make-parameter #f))

  ;;;; basic atom manager.
  ;; Just a very thin wrapper around hash tables.
  (define make-atom-manager
    (lambda (atom-list)
      (define atoms (make-eq-hashtable))
      (define init-atoms
        ;; initialise atoms.
        ;; For those atoms that require display so can only be initialised after a connection to X is made.
        (lambda ()
          (for-each
           (lambda (a)
             (hashtable-set! atoms a (x-intern-atom (symbol->string a) #f)))
           atom-list)))
      (define atom-ref
        (lambda (a)
          (hashtable-ref atoms a #f)))
      (lambda (cmd . args)
        (case cmd
          [(intern)	(init-atoms)]
          [(ref)	(atom-ref (car args))]
          [(list)	atom-list]
          [(values)	(vector->list (hashtable-values atoms))]
          ))))

  ;; type aliases.
  (define-ftype card32 integer-32)
  (define-ftype dpy* void*)
  (define-ftype xid unsigned-long)
  (define-ftype window xid)
  (define-ftype atom unsigned-long)
  (define-ftype status int)
  (define-ftype Colormap xid)
  (define-ftype pixmap xid)
  (define-ftype Time unsigned-long)
  (define-ftype keycode unsigned-8)
  (define-ftype keysym xid)

  (define-ftype u8 unsigned-8)
  (define-ftype u8* (* u8))
  (define-ftype u8** (* u8*))
  (define-ftype window* (* window))

  (define-ftype XAnyEvent
    (struct
     [type		int]		;; message type id
     [serial		unsigned-long]	;; number of last request handled by server
     [send-event	boolean]	;; #t if this came from a SendEvent
     [d			dpy*]		;; display this was read from
     [wid		window]))
  (define-record-type xanyevent
    (fields type serial send-event d wid))

  (define-syntax define-xevent
    (syntax-rules ()
      [(_ name (record idname idval)
          ((field type) (field* type*) ...))
       (begin
         (define-ftype name
          (struct
           [field type]
           [field* type*] ...))
         (define idname idval)
         (define-record-type record (fields field field* ...)))]))

  (define-xevent XClientMessageEvent
    (xclientmessageevent	ClientMessage	33)
    ([xany		XAnyEvent]
     [message-type	atom]		;; message type
     [format		int]
     [data		(union
                         [b (array 20 char)]
                         [s (array 10 short)]
                         [l (array 5 long)])]))

  (define-xevent XConfigureEvent
    (xconfigureevent	ConfigureNotify	22)
    ([xany		XAnyEvent]	; (xany wid) is the parent of the window configured.
     [wid		window]		; window id of the window configured.
     [x			int]
     [y			int]
     [width		int]
     [height		int]
     [border-width	int]
     [above		window]
     [override-redirect	boolean]))

  (define-xevent XConfigureRequestEvent
    (xconfigurerequestevent	ConfigureRequest	23)
    ([xany		XAnyEvent]	; (xany wid) is the parent of the window to be reconfigured.
     [wid		window]		; window id of the window to be reconfigured.
     [x			int]
     [y			int]
     [width		int]
     [height		int]
     [border-width	int]
     [above		window]
     [detail		int]
     [value-mask	unsigned-long]))	; the components specified in the ConfigureWindow request.

  (define-xevent XCreateWindowEvent
    (xcreatewindowevent	CreateNotify 	16)
    ([xany		XAnyEvent]	; (xany wid) is the parent of the window created.
     [wid		window]		; window id of the window created.
     [x			int]
     [y			int]
     [width		int]
     [height		int]
     [border-width	int]
     [override-redirect	boolean]))

  (define-xevent XDestroyWindowEvent
    (xdestroywindowevent	DestroyNotify	17)
    ([xany		XAnyEvent]
     [wid		window]))	; wid is the window that was destroyed.

  (define-record-type xerrorevent
    (fields type d resourceid serial error-code request-code minor-code))

  (define-ftype XErrorEvent
    (struct
     [type		int]
     [d			dpy*]
     [resourceid	xid]
     [serial		unsigned-long]
     [error-code	u8]
     [request-code	u8]
     [minor-code	u8]))

  (define KeyReleaseEvent	3)
  (define-xevent XKeyEvent
    (xkeyevent	KeyPressEvent	2)	;; TODO xkeyevent can also be KeyRelease..
    ([xany		XAnyEvent]
     [root		window]
     [subwindow		window]
     [time		Time]
     [x			int]
     [y			int]
     [x-root		int]
     [y-root		int]
     [state		unsigned]
     [keycode		unsigned]
     [same-screen	boolean]))

  (define-xevent XMapEvent
    (xmapevent	MapNotify		19)
    ([xany		XAnyEvent]
     [wid		window]		; wid is the window that was mapped.
     [override-redirect	boolean]))	; usually true for things like pop-up windows.

  (define-xevent XMapRequestEvent
    (xmaprequestevent	MapRequest		20)
    ([xany		XAnyEvent]
     [wid		window]))	; wid is the window to be mapped.

  (define-xevent XPropertyEvent
    (xpropertyevent	PropertyNotify		28)
    ([xany		XAnyEvent]
     [propatom		atom]
     [time		Time]
     [state		int]))

  (define-xevent XUnmapEvent
    (xunmapevent	UnmapNotify	18)
    ([xany		XAnyEvent]
     [wid		window]		; wid is the window that was unmapped.
     [from-configure	boolean]))	; man XUnmapEvent for a description of this param.

  (define-ftype xevent-size  (array 24 long))
  (define-ftype XEvent
    (union
     [type		int]
     [xany		XAnyEvent]
     [client-message	XClientMessageEvent]
     [xconfigure	XConfigureEvent]
     [xconfigurerequest	XConfigureRequestEvent]
     [xcreatewindow	XCreateWindowEvent]
     [xdestroywindow	XDestroyWindowEvent]
     [xerror		XErrorEvent]
     [xkey		XKeyEvent]
     [xmap		XMapEvent]
     [xmaprequest	XMapRequestEvent]
     [xproperty		XPropertyEvent]
     [xunmap		XUnmapEvent]
     [pad		xevent-size]))

  (define-ftype XTextProperty
    (struct
     [value	u8*]
     [encoding	atom]
     [format	int]
     [nitems	unsigned-long]))

  (define-ftype XWindowAttributes
    (struct
     [x			int]
     [y			int]
     [width		int]
     [height		int]
     [border-width	int]
     [depth		int]
     [visual		void*]
     [root		window]
     [class		int]
     [bit-gravity	int]
     [win-gravity	int]
     [backing-store	int]
     [backing-planes	unsigned-long]
     [backing-pixel	unsigned-long]
     [save-under	boolean]
     [colormap		Colormap]
     [map-installed	boolean]
     [map-state		int]
     [all-event-masks	long]
     [your-event-mask	long]
     [do-not-propagate-mask	long]
     [override-redirect	boolean]
     [screen		void*]))

  (define-ftype XWindowChanges
    (struct
     [x			int]
     [y			int]
     [width		int]
     [height		int]
     [border-width	int]
     [sibling		window]
     [stack-mode	int]))

  ;; X atoms from Xatom.h
  (define x-atom
    (make-atom-manager
      '(ATOM CARDINAL WINDOW)))

  ;; Error codes. From X.h
  (enum error-codes
        (Success		0)
        (BadAccess		10))

  (enum map-state
        (IsUnmapped	0)
        (IsUnviewable	1)
        (IsViewable	2))

  (define NoEvent              0)
  (bitmap input-event-mask
   (KeyPress             0)
   (KeyRelease           1)
   (ButtonPress          2)
   (ButtonRelease        3)
   (EnterWindow          4)
   (LeaveWindow          5)
   (PointerMotion        6)
   (PointerMotionHint    7)
   (Button1Motion        8)
   (Button2Motion        9)
   (Button3Motion        10)
   (Button4Motion        11)
   (Button5Motion        12)
   (ButtonMotion         13)
   (KeymapState          14)
   (Exposure             15)
   (VisibilityChange     16)
   (StructureNotify      17)
   (ResizeRedirect       18)
   (SubstructureNotify   19)
   (SubstructureRedirect 20)
   (FocusChange          21)
   (PropertyChange       22)
   (ColormapChange       23)
   (OwnerGrabButton      24))

  ;; for value-mask in XConfigureRequestEvent
  (enum configure-window-mask
   (CWX             0)
   (CWY             1)
   (CWWidth         2)
   (CWHeight        3)
   (CWBorderWidth   4)
   (CWSibling       5)
   (CWStackMode     6))

  ;; window stacking method for configuring windows.
  (enum stacking-method
   (Above     0)
   (Below     1)
   (TopIf     2)
   (BottomIf  3))

  (enum input-focus
    (RevertToNone		 0)
    (RevertToPointerRoot	 1)
    (RevertToParent		 2))

  (bitmap key-modifier-mask
    (Shift	0)
    (Lock	1)
    (Control	2)
    (Mod1	3)
    (Mod2	4)
    (Mod3	5)
    (Mod4	6)
    (Mod5	7))

  (enum grab-mode
    (GrabModeSync	0)
    (GrabModeAsync	1))

  (enum button-masks
    ;; skipping unused buttons.
    (AnyModifier	15))

  ;; X.h reserved resources/constants.
  (define None 0)
  (define AnyKey 1)
  (define CurrentTime 0)
  (define NoSymbol 0)
  (define PointerRoot 1)

  ;; Xutil.h  XICCEncodingStyle
  (define UTF8String 4)

  (c-function
    (x-free (void*) void)
    ;; The arg to XFreeStringList should be char** but foreign-ref doesn't support that.
    ;; void* points to anything so use that for now.
    (x-free-string-list (void*) void)
    (x-keysym-to-string (keysym) string)
    (XOpenDisplay (string) dpy*)
    ;; XSetErrorHandler prototype returns an int, but it's actually a pointer to the previous error handler.
    ;; So deviating and marking it as void* instead.
    (XSetErrorHandler (void*) void*)
    (x-string-to-keysym (string) keysym))

  (c-default-function
    (dpy* (current-display))
    ;; Window XCreateSimpleWindow(parent x y width height border_width border background);
    (x-create-simple-window (window int int unsigned unsigned unsigned unsigned-long unsigned-long) window)
    (x-change-property (window atom atom int int void* int) int)
    (x-delete-property (window atom) int)
    (x-close-display () int)
    ;; data should be a u8* but using a void* instead.
    (XConfigureWindow (window unsigned (* XWindowChanges)) int)
    (x-default-root-window () window)
    (x-destroy-window (window) int)
    (x-flush () int)
    (XGetAtomName (atom) void*)
    (x-get-text-property (window (* XTextProperty) atom) status)
    (XGetWindowAttributes (window (* XWindowAttributes)) status)
    (x-get-window-property (window atom long long boolean atom (* atom) (* int) (* unsigned-long) (* unsigned-long) (* void*)) int)
    (x-grab-key (int unsigned window boolean int int) int)
    (x-grab-server () int)
    (x-intern-atom (string boolean) atom)
    (x-keycode-to-keysym (keycode int) keysym)
    (x-keysym-to-keycode (keysym) keycode)
    (x-lower-window (window) int)
    (x-map-window (window) int)
    (x-move-resize-window (window int int unsigned unsigned) int)
    (XNextEvent ((* XEvent)) int)
    (XQueryTree (window (* window) (* window) (* window*) (* unsigned)) status)
    (x-raise-window (window) int)
    (x-select-input (window long) int)
    (x-send-event (window boolean long (* XEvent)) status)
    (x-set-input-focus (window int Time) int)
    (x-set-text-property (window (* XTextProperty) atom) void)
    (XSync (boolean) int)
    (x-ungrab-key (int unsigned window) int)
    (x-ungrab-server () int)
    (x-unmap-window (window) int)
    ;; TODO void* in Xutf8TextListToTextProperty should be (* u8*).
    ;; I had troubles with foreign-set! and 'u8* so revisit when I understand ftypes better.
    (xutf8-text-list-to-text-property (void* int unsigned (* XTextProperty)) int)
    (xutf8-text-property-to-text-list ((* XTextProperty) (* u8**) (* int)) int))

  (define x-configure-window
    (lambda (wid geo)
      (let ([change-mask 0])
        (fmem ([changes &changes XWindowChanges])
          (when (geometry-x geo)
            (ftype-set! XWindowChanges (x) &changes (geometry-x geo))
            (set! change-mask (bitwise-copy-bit change-mask CWX 1)))
          (when (geometry-y geo)
            (ftype-set! XWindowChanges (y) &changes (geometry-y geo))
            (set! change-mask (bitwise-copy-bit change-mask CWY 1)))
          (when (geometry-width geo)
            (ftype-set! XWindowChanges (width) &changes (geometry-width geo))
            (set! change-mask (bitwise-copy-bit change-mask CWWidth 1)))
          (when (geometry-height geo)
            (ftype-set! XWindowChanges (height) &changes (geometry-height geo))
            (set! change-mask (bitwise-copy-bit change-mask CWHeight 1)))
          (XConfigureWindow wid change-mask &changes)
          #;(display (format "#x~x x-configure-window change-mask #b~b~n" wid change-mask))))))

  (define x-get-atom-name
    (lambda (a)
      ;; Unset atom fields from messages will be zero, so account for that here as a convenience to print functions.
      (if (> a 0)
        (let* ([ptr (XGetAtomName a)]
               [str (ptr->string ptr)])
          (x-free ptr)
          str)
        "")))

  (define-record-type geometry
    (fields x y width height))

  (define geometry=?
    (lambda (lhs rhs)
      (and (eq? (geometry-x lhs) (geometry-x rhs))
           (eq? (geometry-y lhs) (geometry-y rhs))
           (eq? (geometry-width lhs) (geometry-width rhs))
           (eq? (geometry-height lhs) (geometry-height rhs)))))

  (define xconfigureevent-geometry
    (lambda (ev)
      (make-geometry (xconfigureevent-x ev) (xconfigureevent-y ev) (xconfigureevent-width ev) (xconfigureevent-height ev))))

  (define xconfigurerequestevent-geometry
    (lambda (ev)
      (make-geometry (xconfigurerequestevent-x ev) (xconfigurerequestevent-y ev) (xconfigurerequestevent-width ev) (xconfigurerequestevent-height ev))))

  (define-record-type window-attributes
    (fields geom override-redirect map-state))

  (define x-get-window-attributes
    (lambda (wid)
      (fmem ([wa &wa XWindowAttributes])
            (let ([rc (XGetWindowAttributes wid &wa)])
              (if (= rc 0)
                  ;; failure.
                  #f
                  (let ([x (ftype-ref XWindowAttributes (x) &wa)]
                        [y (ftype-ref XWindowAttributes (y) &wa)]
                        [w (ftype-ref XWindowAttributes (width) &wa)]
                        [h (ftype-ref XWindowAttributes (height) &wa)]
                        [map-state (ftype-ref XWindowAttributes (map-state) &wa)]
                        [override (ftype-ref XWindowAttributes (override-redirect) &wa)])
                    (make-window-attributes (make-geometry x y w h) override map-state)))))))

  ;; wraps XOpenDisplay so the connection string is optional.
  (define x-open-display
    (case-lambda
     [()
      ;; XOpenDisplay will crash unless it has a valid DISPLAY.
      (unless (getenv "DISPLAY")
        (raise (condition (make-error) (make-message-condition "DISPLAY not given. Exiting.."))))
      (XOpenDisplay #f)]
     [(s) (XOpenDisplay s)]))

  (define x-query-tree
    (lambda ()
      (fmem ([root-return &rr window]
             [parent-return &pr window]
             [children-return &cr window*]
             [num-children &nc unsigned])
            (let ([rc (XQueryTree (root) &rr &pr &cr &nc)])
              (if (and (> rc 0) (> num-children 0))
                  (let ([ptr (foreign-ref 'void* children-return 0)]
                        [len (foreign-ref 'unsigned num-children 0)])
                    (let ([wids (ptr->ulongs ptr len)])
                      (x-free ptr)
                      wids))
                  '())))))

  ;; wraps XSync so discard boolean is optional.
  (define x-sync
    (case-lambda
     [() (XSync #f)]
     [(s) (XSync s)]))

  ;; wraps XSetErrorHandler so that the default error handler is installed with no args.
  ;; Previous error handlers are stored and dealloc when no longer needed.
  (define x-set-error-handler
    ;; Store the previous locked lambda so that it can be unlocked if replaced.
    (let ([previous-lambda #f])
      (case-lambda
        [()
         ;; Installs a simple one-line printing error handler.
         (x-set-error-handler
           (lambda (dpy ev)
             (display (format "XError: type=~a wid=#x~x error=~d~n" (xerrorevent-type ev) (xerrorevent-resourceid ev) (xerrorevent-error-code ev)))))]
        [(handler)
         (when previous-lambda
           (unlock-object previous-lambda)
           (set! previous-lambda #f))
         (XSetErrorHandler
           (if (procedure? handler)
               ;; Wrap the lambda for convenience:
               ;; - converts c event struct to scheme record
               ;; - returns int 0 so that connection to X server remains.
               (let ([f/proc
                       (foreign-callable
                         (lambda (d c/xerrorevent)
                           (let ([ev
                                   (make-xerrorevent
                                     (ftype-ref XErrorEvent (type) c/xerrorevent)
                                     (ftype-ref XErrorEvent (d) c/xerrorevent)
                                     (ftype-ref XErrorEvent (resourceid) c/xerrorevent)
                                     (ftype-ref XErrorEvent (serial) c/xerrorevent)
                                     (ftype-ref XErrorEvent (error-code) c/xerrorevent)
                                     (ftype-ref XErrorEvent (request-code) c/xerrorevent)
                                     (ftype-ref XErrorEvent (minor-code) c/xerrorevent))])
                             (handler d ev)
                             0))
                         (dpy* (* XErrorEvent)) int)])
                 (lock-object f/proc)
                 (set! previous-lambda f/proc)
                 (foreign-callable-entry-point f/proc))
               ;; Else assume handler is a mem addr, eg as would be returned by the first call to XSetErrorHandler.
               handler))])))

  (define x-next-event
    (lambda ()
      (fmem ([ev &ev XEvent])
        (XNextEvent &ev)
        (make-event &ev))))

  (define x-with-next-event
    (lambda (func)
      (fmem ([ev &ev XEvent])
        (dynamic-wind
          (lambda () #t)
          (lambda ()
            (XNextEvent &ev)
            #;(x-grab-server)
            (func (make-event &ev)))
          (lambda ()
            #f
            #;(x-ungrab-server))))))

  ;; Convert the cevent struct to a scheme record.
  ;; TODO this c-struct->scheme-record conversion to be done in xlib as part of define-xevent??
  (define make-event
    (lambda (cevent)
      (let ([evid (ftype-ref XEvent (type) cevent)])
        (case-equal? evid
          (ClientMessage
            (make-xclientmessageevent
             (make-xany cevent)
             (ftype-ref XEvent (client-message message-type) cevent)
             (ftype-ref XEvent (client-message format) cevent)
             (ftype-fields client-message cevent
                           ((data l 0)
                            (data l 1)
                            (data l 2)
                            (data l 3)
                            (data l 4)))))
           (ConfigureNotify
            (apply make-xconfigureevent
             (make-xany cevent)
             (ftype-fields xconfigure cevent (wid x y width height border-width above override-redirect))))
           (ConfigureRequest
            (apply make-xconfigurerequestevent
             (make-xany cevent)
             (ftype-fields xconfigurerequest cevent (wid x y width height border-width above detail value-mask))))
           (CreateNotify
            (apply make-xcreatewindowevent
             (make-xany cevent)
             (ftype-fields xcreatewindow cevent (wid x y width height border-width override-redirect))))
           (DestroyNotify
            (apply make-xdestroywindowevent
             (make-xany cevent)
             (ftype-fields xdestroywindow cevent (wid))))
           (KeyPressEvent
            (apply make-xkeyevent
             (make-xany cevent)
             (ftype-fields xkey cevent (root subwindow time x y x-root y-root state keycode same-screen))))
           (KeyReleaseEvent
            (apply make-xkeyevent
             (make-xany cevent)
             (ftype-fields xkey cevent (root subwindow time x y x-root y-root state keycode same-screen))))
           (MapNotify
            (apply make-xmapevent
             (make-xany cevent)
             (ftype-fields xmap cevent (wid override-redirect))))
           (MapRequest
            (apply make-xmaprequestevent
             (make-xany cevent)
             (ftype-fields xmaprequest cevent (wid))))
           (PropertyNotify
            (apply make-xpropertyevent
             (make-xany cevent)
             (ftype-fields xproperty cevent (propatom time state))))
           (UnmapNotify
            (apply make-xunmapevent
             (make-xany cevent)
             (ftype-fields xunmap cevent (wid from-configure))))
           (else
            (make-xany cevent))))))

  (define-syntax ftype-fields
    (syntax-rules ()
      [(_ type obj ((field name offset) ...))
       (list (ftype-ref XEvent (type field name offset) obj) ...)]
      [(_ type obj (field ...))
       (list (ftype-ref XEvent (type field) obj) ...)]))

  (define make-xany
    (lambda (cevent)
      (apply make-xanyevent
       (ftype-fields xany cevent (type serial send-event d wid)))))

  (define cardinal-set!
    (lambda (wid atomprop value)
      (fmem ([num &num unsigned-32])
            (foreign-set! 'unsigned-32 num 0 value)
            (x-change-property wid atomprop (x-atom 'ref 'CARDINAL) 32 0 num 1))))

  (define ulongs-property-set!
    (lambda (wid atomprop ulongs typeatom)
      (let ([len (length ulongs)])
        (cond
          [(fx>? len 0)
           (fmem ([mem &mem unsigned-long len])
             (fill-memory/ulongs mem ulongs)
             (x-change-property wid atomprop typeatom 32 0 mem len))]
          [else
            (format (current-error-port) "#x~x deleting property ~a(~a)~n" wid (x-get-atom-name atomprop) atomprop)
            (x-delete-property wid atomprop)]))))

  (define property->string
    (lambda (wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (x-get-text-property wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (let* (#;[enc (ftype-ref XTextProperty (encoding) &tp)]
                         #;[num (ftype-ref XTextProperty (nitems) &tp)]
                         [addr (ftype-pointer-address (ftype-ref XTextProperty (value) &tp))]
                         [str (ptr->string addr)])
                    #;(display (format "encoding ~d:~s nitems ~d ~n" enc (x-get-atom-name enc) num))
                    (x-free addr)
                    str)
                  #f)))))

  (define property->string*
    (lambda (wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (x-get-text-property wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (fmem ([nitems &nitems int]
                         [text-list &text-list u8**])
                        (let* ([stat (xutf8-text-property-to-text-list &tp &text-list &nitems)]
                               [res (ptr->utf8s text-list nitems)])
                          ;; TODO text-list foreign-ref is also calc'd in text-list->utf8s. Need to re-org.
                          (x-free-string-list (foreign-ref 'void* text-list 0))
                          res))
                  (list))))))

  ;; Return list/pair (address length) of property memory data or #f on failure.
  ;; Caller *Must* foreign-free returned address.
  (define get-property-ptr
    (lambda (wid propatom atomtype)
      (fmem ([atr &atr atom]		;; atr = actual type return
             [afr &afr int]		;; afr = actual format return
             [nir &nir unsigned-long]	;; nir = number of items return
             [bar &bar unsigned-long])	;; bar = bytes after return
        ;; pr  = property return
        ;; declared outside fmem as ownership is transferred to caller.
        (let* ([pr (foreign-alloc (ftype-sizeof void*))]
               [&pr (make-ftype-pointer void* pr)]
               [rc (x-get-window-property wid propatom 0 2048 #f atomtype &atr &afr &nir &bar &pr)])
          (cond
            [(= rc 0)
             ;; maybe success: make sure there was something returned.
             (let ([len (foreign-ref 'unsigned-long nir 0)])
               (cond
                 [(> len 0)
                  ;; more success: return data ptr and length.
                  (values pr len)]
                 [else
                   ;; nothing back, free memory and return.
                   (x-free (foreign-ref 'void* pr 0))
                   (foreign-free pr)
                   (values #f #f)]))]
             [else
               ;; failure: return false.
               (values #f #f)])))))

  ;; window property to list of unsigned longs.
  (define property->ulongs
    (lambda (wid propatom atomtype)
      (let-values ([(ptr len) (get-property-ptr wid propatom atomtype)])
        (cond
          [ptr
            (let* ([*ptr (foreign-ref 'void* ptr 0)]
                   [nums (ptr->ulongs *ptr len)])
              (x-free *ptr)
              (foreign-free ptr)
              nums)]
          [else
            ;; failure: return empty.
            (list)]))))

  (define text-property-set!
    (lambda (wid string-list propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([u8mem (str*->u8** string-list)])
              (let ([rc (xutf8-text-list-to-text-property u8mem (length string-list) UTF8String &tp)])
                (if (fx= rc 0)
                    (x-set-text-property wid &tp propatom))
                (free/u8** u8mem (length string-list))
                (x-free (ftype-pointer-address (void*-cast (ftype-ref XTextProperty (value) &tp)))))))))

  ;; Return first (and likely only) item in list or false if list is empty.
  (define-syntax first-or-false
    (syntax-rules ()
      [(_ ls)
       (let ([als ls])
         (and (pair? als)
              (car als)))]))

  (define-syntax define-root-property
    (lambda (x)
      (syntax-case x ()
        [(_ name prop-sym atom-id atom-type)
         (case (syntax->datum #'prop-sym)
           [(ulong/list)
            #'(define-syntax name
                (identifier-syntax
                  [id
                    (property->ulongs (root) atom-id atom-type)]
                  [(set! id value)
                   (ulongs-property-set! (root) atom-id value atom-type)]))]
           [(ulong)
            #'(define-syntax name
                (identifier-syntax
                  [id
                    (first-or-false (property->ulongs (root) atom-id atom-type))]
                  [(set! id value)
                   (ulongs-property-set! (root) atom-id (list value) atom-type)]))])]
        [(_ name prop-sym atom-id)
         (case (syntax->datum #'prop-sym)
           [(string/list)
            #'(define-syntax name
                (identifier-syntax
                  [id
                    (property->string* (root) atom-id)]
                  [(set! id value)
                   (text-property-set! (root) value atom-id)]))]
           [(string)
            #'(define-syntax name
                (identifier-syntax
                  [id
                    (first-or-false (property->string* (root) atom-id))]
                  [(set! id value)
                   (text-property-set! (root) (list value) atom-id)]))])])))

  (define send-message-cardinal
    (lambda (root wid atom value)
      (fmem ([ev &ev XEvent])
            (let ([event-mask (fxlogor SubstructureNotify SubstructureRedirect)])
              (ftype-set! XEvent (client-message xany type) &ev ClientMessage)
              (ftype-set! XEvent (client-message xany wid) &ev wid)
              (ftype-set! XEvent (client-message xany send-event) &ev #t)
              (ftype-set! XEvent (client-message message-type) &ev atom)
              (ftype-set! XEvent (client-message format) &ev 32)
              (ftype-set! XEvent (client-message data l 0) &ev value)
              (ftype-set! XEvent (client-message data l 1) &ev 0)	;; zero out.
              (x-send-event root #f event-mask &ev)))))
  )
