(library (xlib)
  (export
   XEvent
   XClientMessageEvent
   XCreateWindowEvent
   XErrorEvent
   XTextProperty
   XWindowAttributes

   XA-CARDINAL
   XA-WINDOW

   Success
   BadAccess

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

   ClientMessage
   CreateNotify

   UTF8String

   XChangeProperty
   XCloseDisplay
   XDefaultRootWindow
   XFlush
   XFree
   XFreeStringList
   XGetAtomName
   XGetTextProperty
   XGetWindowAttributes
   XGetWindowProperty
   XInternAtom
   XOpenDisplay
   XQueryTree
   XSelectInput
   XSendEvent
   XSetErrorHandler
   XSetTextProperty
   XSync
   Xutf8TextListToTextProperty
   Xutf8TextPropertyToTextList

   dpy*
   window
   atom
   status
   u8*
   u8**
   window*
   )
  (import (chezscheme))

  (define library-init
    (load-shared-object "libX11.so"))

  (define-syntax proc
    (syntax-rules ()
      [(_ libsym args return)
       (define libsym (foreign-procedure (symbol->string 'libsym) args return))]))

  ;; type aliases.
  (define-ftype dpy* void*)
  (define-ftype window unsigned-32)
  (define-ftype atom unsigned-32)
  (define-ftype status unsigned-32)
  (define-ftype xid unsigned-32)
  (define-ftype Colormap xid)

  (define-ftype u8 unsigned-8)
  (define-ftype u8* (* u8))
  (define-ftype u8** (* u8*))
  (define-ftype window* (* window))

  (define-ftype XAnyEvent
    (struct
     [type		integer-32]	;; message type id
     [serial		unsigned-long]	;; number of last request handled by server
     [send-event	boolean]	;; #t if this came from a SendEvent
     [d			dpy*]		;; display this was read from
     [wid		window]))

  (define-ftype b20 (array 20 char))
  (define-ftype s10 (array 10 short))
  (define-ftype l5  (array 5 long))

  (define-ftype XClientMessageEvent
    (struct
     [xany		XAnyEvent]
     [message-type	atom]		;; message type
     [format		integer-32]
     [data		(union
                         [b b20]
                         [s s10]
                         [l l5])]))

  (define-ftype XCreateWindowEvent
    (struct
     [xany		XAnyEvent]	; (xany wid) is the parent of the window created.
     [wid		window]		; window id of the window created.
     [x			integer-32]
     [y			integer-32]
     [width		integer-32]
     [height		integer-32]
     [border-width	integer-32]
     [override-redirect	boolean]))

  (define-ftype XErrorEvent
    (struct
     [type		integer-32]
     [d			dpy*]
     [resourceid	xid]
     [serial		unsigned-long]
     [error-code	u8]
     [request-code	u8]
     [minor-code	u8]))

  (define-ftype XEvent
    (union
     [xany		XAnyEvent]
     [client-message	XClientMessageEvent]
     [xcreatewindow	XCreateWindowEvent]
     [xerror		XErrorEvent]))

  (define-ftype XTextProperty
    (struct
     [value	u8*]
     [encoding	atom]
     [format	integer-32]
     [nitems	unsigned-long]))

  (define-ftype XWindowAttributes
    (struct
     [x			integer-32]
     [y			integer-32]
     [width		integer-32]
     [height		integer-32]
     [border-width	integer-32]
     [depth		integer-32]
     [visual		void*]
     [root		window]
     [class		integer-32]
     [bit-gravity	integer-32]
     [win-gravity	integer-32]
     [backing-store	integer-32]
     [backing-planes	unsigned-32]
     [backing-pixel	unsigned-32]
     [save-under	boolean]
     [colormap		Colormap]
     [map-installed	boolean]
     [map-state		integer-32]
     [all-event-masks	unsigned-32]
     [your-event-mask	unsigned-32]
     [do-not-propagate-mask	unsigned-32]
     [override-redirect	boolean]
     [screen		void*]))

  ;; X atoms from Xatom.h
  (define XA-CARDINAL 6)
  (define XA-WINDOW 33)

  ;; Error codes. From X.h
  (define Success		0)
  (define BadAccess		10)

  ;; Input event mask.
  (define NoEvent              0)
  (define KeyPress             (fxsll 1 0))
  (define KeyRelease           (fxsll 1 1))
  (define ButtonPress          (fxsll 1 2))
  (define ButtonRelease        (fxsll 1 3))
  (define EnterWindow          (fxsll 1 4))
  (define LeaveWindow          (fxsll 1 5))
  (define PointerMotion        (fxsll 1 6))
  (define PointerMotionHint    (fxsll 1 7))
  (define Button1Motion        (fxsll 1 8))
  (define Button2Motion        (fxsll 1 9))
  (define Button3Motion        (fxsll 1 10))
  (define Button4Motion        (fxsll 1 11))
  (define Button5Motion        (fxsll 1 12))
  (define ButtonMotion         (fxsll 1 13))
  (define KeymapState          (fxsll 1 14))
  (define Exposure             (fxsll 1 15))
  (define VisibilityChange     (fxsll 1 16))
  (define StructureNotify      (fxsll 1 17))
  (define ResizeRedirect       (fxsll 1 18))
  (define SubstructureNotify   (fxsll 1 19))
  (define SubstructureRedirect (fxsll 1 20))
  (define FocusChange          (fxsll 1 21))
  (define PropertyChange       (fxsll 1 22))
  (define ColormapChange       (fxsll 1 23))
  (define OwnerGrabButton      (fxsll 1 24))

  ;; EventName
  (define CreateNotify  16)	; XCreateWindowEvent
  (define ClientMessage 33)

  ;; Xutil.h  XICCEncodingStyle
  (define UTF8String 4)

  ;; data should be a u8* but using a void* instead.
  (proc XChangeProperty (dpy* window atom atom integer-32 integer-32 (* unsigned-32) integer-32) integer-32)
  (proc XCloseDisplay (dpy*) int)
  (proc XDefaultRootWindow (dpy*) window)
  (proc XFlush (dpy*) integer-32)
  (proc XFree (void*) void)
  (proc XFreeStringList (void*) void)
  (proc XGetAtomName (dpy* atom) void*)
  (proc XGetTextProperty (dpy* window (* XTextProperty) atom) status)
  (proc XGetWindowAttributes (dpy* window (* XWindowAttributes)) status)
  ;; The arg to XFreeStringList should be char** but foreign-ref doesn't support that.
  ;; void* points to anything so use that for now.
  (proc XGetWindowProperty (dpy* window atom long long boolean atom (* atom) (* integer-32) (* unsigned-long) (* unsigned-long) (* u8*)) int)
  (proc XInternAtom (dpy* string boolean) atom)
  (proc XOpenDisplay (string) dpy*)
  (proc XQueryTree (dpy* window (* window) (* window) (* window*) (* unsigned-32)) status)
  (proc XSelectInput (dpy* window long) integer-32)
  (proc XSendEvent (dpy* window boolean long (* XEvent)) status)
  ;; XSetErrorHandler prototype returns an int, but it's actually a pointer to the previous error handler.
  ;; So deviating and marking it as void* instead.
  (proc XSetErrorHandler (void*) void*)
  (proc XSetTextProperty (dpy* window (* XTextProperty) atom) void)
  (proc XSync (dpy* boolean) integer-32)
  ;; TODO void* in Xutf8TextListToTextProperty should be (* u8*).
  ;; I had troubles with foreign-set! and 'u8* so revisit when I understand ftypes better.
  (proc Xutf8TextListToTextProperty (dpy* void* int unsigned-32 (* XTextProperty)) int)
  (proc Xutf8TextPropertyToTextList (dpy* (* XTextProperty) (* u8**) (* integer-32)) integer-32)
  )
