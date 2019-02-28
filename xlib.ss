(library (xlib)
  (export
   XTextProperty

   XA-WINDOW

   XCloseDisplay
   XDefaultRootWindow
   XFree
   XFreeStringList
   XGetAtomName
   XGetTextProperty
   XGetWindowProperty
   XInternAtom
   XOpenDisplay
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

  (define-ftype XTextProperty
    (struct
     [value	u8*]
     [encoding	atom]
     [format	integer-32]
     [nitems	unsigned-long]))

  ;; X atoms from Xatom.h
  (define XA-WINDOW 33)

  (proc XCloseDisplay (dpy*) int)
  (proc XDefaultRootWindow (dpy*) window)
  (proc XFree (void*) void)
  (proc XFreeStringList (void*) void)
  (proc XGetAtomName (dpy* atom) void*)
  (proc XGetTextProperty (dpy* window (* XTextProperty) atom) status)
  ;; The arg to XFreeStringList should be char** but foreign-ref doesn't support that.
  ;; void* points to anything so use that for now.
  (proc XGetWindowProperty (dpy* window atom long long boolean atom (* atom) (* integer-32) (* unsigned-long) (* unsigned-long) (* u8*)) int)
  (proc XInternAtom (dpy* string boolean) atom)
  (proc XOpenDisplay (string) dpy*)
  (proc Xutf8TextPropertyToTextList (dpy* (* XTextProperty) (* u8**) (* integer-32)) integer-32)
  )
