#! /usr/bin/scheme --script

(suppress-greeting #t)
(compile-imported-libraries #t)

(import
 (rnrs base)
 (only (chezscheme) new-cafe)
 (prefix (ewmh) ewmh.)
 (globals)
 (prefix (hints) hints.)
 (prefix (icccm) icccm.)
 (prefix (shell) sh.)
 (xlib)
 (prefix (xutil) xutil.))

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
    (sh.main argv)]))
