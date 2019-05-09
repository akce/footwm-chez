#! /usr/bin/scheme --script

(suppress-greeting #t)
(compile-imported-libraries #t)
(debug-on-exception #t)

(import
 (rnrs base)
 (prefix (ewmh) ewmh.)
 (globals)
 (prefix (hints) hints.)
 (prefix (icccm) icccm.)
 (prefix (wm) wm.)
 (xlib)
 (prefix (xutil) xutil.))

(current-display (xutil.open))
(root (XDefaultRootWindow (current-display)))

(icccm.init-atoms)
(ewmh.init-atoms)
(hints.init-atoms)

(wm.main)
