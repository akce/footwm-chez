#! /usr/bin/scheme --script

(suppress-greeting #t)
(compile-imported-libraries #t)

(import
 (rnrs base)
 (only (chezscheme) new-cafe)
 (globals)
 (xlib)
 (prefix (xutil) xutil.)
 ;; Import all local modules so they're accessable from the command line.
 (prefix (ewmh) ewmh.)
 (prefix (hints) hints.)
 (prefix (icccm) icccm.)
 (prefix (op) op.)
 (prefix (shell) shell.)
 (prefix (wm) wm.))

(current-display (xutil.open))
(root (XDefaultRootWindow (current-display)))

(icccm.init-atoms)
(ewmh.init-atoms)
(hints.init-atoms)

(let ([argv (command-line)])
  (cond
   [(fx=? (length argv) 1)
    (new-cafe)]
   [else
    (shell.main argv)]))
