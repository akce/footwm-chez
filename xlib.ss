(library (xlib)
  (export
   XAnyEvent
   XEvent
   XErrorEvent

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

   XA-ATOM
   XA-CARDINAL
   XA-WINDOW

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

   CurrentTime

   ;; input focus
   RevertToNone
   RevertToPointerRoot
   RevertToParent

   UTF8String

   XChangeProperty
   XCloseDisplay
   XConfigureWindow
   XDefaultRootWindow
   XDestroyWindow
   XFlush
   XFree
   XFreeStringList
   XGetAtomName
   XGetTextProperty
   XGetWindowAttributes
   XGetWindowProperty
   XInternAtom
   XMapWindow
   XMoveResizeWindow
   XNextEvent
   XOpenDisplay
   XQueryTree
   XSelectInput
   XSendEvent
   XSetErrorHandler
   XSetInputFocus
   XSetTextProperty
   XSync
   XUnmapWindow
   Xutf8TextListToTextProperty
   Xutf8TextPropertyToTextList

   atom
   card32
   dpy*
   pixmap
   status
   u8*
   u8**
   window
   window*)
  (import
   (chezscheme)
   (util))

  (define library-init
    (load-shared-object "libX11.so.6"))

  (define-syntax define-x
    (syntax-rules ()
      [(_ (libsym args return) ...)
       (begin
         (define libsym (foreign-procedure (symbol->string 'libsym) args return)) ...)]))

  (define-syntax define-xevent
    (syntax-rules ()
      [(_ name (record idname idval)
          ((field type) (fieldn typen) ...))
       (begin
         (define-ftype name
          (struct
           [field type]
           [fieldn typen] ...))
         (define idname idval)
         (define-record-type record (fields field fieldn ...)))]))

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

  (define-ftype b20 (array 20 char))
  (define-ftype s10 (array 10 short))
  (define-ftype l5  (array 5 long))

  (define-xevent XClientMessageEvent
    (xclientmessageevent	ClientMessage	33)
    ([xany		XAnyEvent]
     [message-type	atom]		;; message type
     [format		int]
     [data		(union
                         [b b20]
                         [s s10]
                         [l l5])]))

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

  (define-ftype XErrorEvent
    (struct
     [type		int]
     [d			dpy*]
     [resourceid	xid]
     [serial		unsigned-long]
     [error-code	u8]
     [request-code	u8]
     [minor-code	u8]))

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
  (enum XA
        (XA-ATOM 4)
        (XA-CARDINAL 6)
        (XA-WINDOW 33))

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

  (define CurrentTime 0)

  (enum input-focus
    (RevertToNone		 0)
    (RevertToPointerRoot	 1)
    (RevertToParent		 2))

  ;; Xutil.h  XICCEncodingStyle
  (define UTF8String 4)

  (define-x
   ;; data should be a u8* but using a void* instead.
   (XChangeProperty (dpy* window atom atom int int void* int) int)
   (XCloseDisplay (dpy*) int)
   (XConfigureWindow (dpy* window unsigned (* XWindowChanges)) int)
   (XDefaultRootWindow (dpy*) window)
   (XDestroyWindow (dpy* window) int)
   (XFlush (dpy*) int)
   (XFree (void*) void)
   (XFreeStringList (void*) void)
   (XGetAtomName (dpy* atom) void*)
   (XGetTextProperty (dpy* window (* XTextProperty) atom) status)
   (XGetWindowAttributes (dpy* window (* XWindowAttributes)) status)
   ;; The arg to XFreeStringList should be char** but foreign-ref doesn't support that.
   ;; void* points to anything so use that for now.
   (XGetWindowProperty (dpy* window atom long long boolean atom (* atom) (* int) (* unsigned-long) (* unsigned-long) (* void*)) int)
   (XInternAtom (dpy* string boolean) atom)
   (XMapWindow (dpy* window) int)
   (XMoveResizeWindow (dpy* window int int unsigned unsigned) int)
   (XNextEvent (dpy* (* XEvent)) int)
   (XOpenDisplay (string) dpy*)
   (XQueryTree (dpy* window (* window) (* window) (* window*) (* unsigned)) status)
   (XSelectInput (dpy* window long) int)
   (XSendEvent (dpy* window boolean long (* XEvent)) status)
   ;; XSetErrorHandler prototype returns an int, but it's actually a pointer to the previous error handler.
   ;; So deviating and marking it as void* instead.
   (XSetErrorHandler (void*) void*)
   (XSetInputFocus (dpy* window int Time) int)
   (XSetTextProperty (dpy* window (* XTextProperty) atom) void)
   (XSync (dpy* boolean) int)
   (XUnmapWindow (dpy* window) int)
   ;; TODO void* in Xutf8TextListToTextProperty should be (* u8*).
   ;; I had troubles with foreign-set! and 'u8* so revisit when I understand ftypes better.
   (Xutf8TextListToTextProperty (dpy* void* int unsigned (* XTextProperty)) int)
   (Xutf8TextPropertyToTextList (dpy* (* XTextProperty) (* u8**) (* int)) int)))
