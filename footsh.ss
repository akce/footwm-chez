#! /usr/bin/scheme --script

(suppress-greeting #t)
(compile-imported-libraries #t)

(import (prefix (ewmh) ewmh.)
        (globals)
        (prefix (wm) wm.)
        (prefix (icccm) icccm.)
        (prefix (shell) sh.)
        (xlib)
        (prefix (xutil) xutil.)
        (rnrs base)
        (only (chezscheme) new-cafe))

(current-display (xutil.open))
(root (XDefaultRootWindow (current-display)))

(icccm.init-atoms)
(ewmh.init-atoms)
(wm.init-atoms)

(let ([argv (command-line)])
  (cond
   [(fx=? (length argv) 0)
    (new-cafe)]
   [else
    (sh.main argv)]))
