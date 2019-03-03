(library (xlib)
  (export
   XEvent
   XClientMessageEvent
   XTextProperty

   XA-CARDINAL
   XA-WINDOW

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

   UTF8String

   XChangeProperty
   XCloseDisplay
   XDefaultRootWindow
   XFlush
   XFree
   XFreeStringList
   XGetAtomName
   XGetTextProperty
   XGetWindowProperty
   XInternAtom
   XOpenDisplay
   XSendEvent
   XSetTextProperty
   Xutf8TextListToTextProperty
   Xutf8TextPropertyToTextList

   dpy*
   window
   atom
   status
   u8*
   u8**
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

  (define-ftype u8 unsigned-8)
  (define-ftype u8* (* u8))
  (define-ftype u8** (* u8*))

  (define-ftype b20 (array 20 char))
  (define-ftype s10 (array 10 short))
  (define-ftype l5  (array 5 long))

  (define-ftype XClientMessageEvent
    (struct
     [type		integer-32]	;; ClientMessage
     [serial		unsigned-long]	;; number of last request handled by server
     [send-event	boolean]	;; #t if this came from a SendEvent
     [d			dpy*]		;; display this was read from
     [wid		window]
     [message-type	atom]		;; message type
     [format		integer-32]
     [data		(union
                         [b b20]
                         [s s10]
                         [l l5])]))

  (define-ftype XEvent
    (union
     [client-message XClientMessageEvent]))

  (define-ftype XTextProperty
    (struct
     [value	u8*]
     [encoding	atom]
     [format	integer-32]
     [nitems	unsigned-long]))

  ;; X atoms from Xatom.h
  (define XA-CARDINAL 6)
  (define XA-WINDOW 33)

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
  ;; The arg to XFreeStringList should be char** but foreign-ref doesn't support that.
  ;; void* points to anything so use that for now.
  (proc XGetWindowProperty (dpy* window atom long long boolean atom (* atom) (* integer-32) (* unsigned-long) (* unsigned-long) (* u8*)) int)
  (proc XInternAtom (dpy* string boolean) atom)
  (proc XOpenDisplay (string) dpy*)
  (proc XSendEvent (dpy* window boolean long (* XEvent)) status)
  (proc XSetTextProperty (dpy* window (* XTextProperty) atom) void)
  ;; TODO void* in Xutf8TextListToTextProperty should be (* u8*).
  ;; I had troubles with foreign-set! and 'u8* so revisit when I understand ftypes better.
  (proc Xutf8TextListToTextProperty (dpy* void* int unsigned-32 (* XTextProperty)) int)
  (proc Xutf8TextPropertyToTextList (dpy* (* XTextProperty) (* u8**) (* integer-32)) integer-32)
  )
