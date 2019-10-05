#! /usr/bin/scheme --script

(suppress-greeting #t)
(compile-imported-libraries #t)

(import
 (rnrs base)
 (only (chezscheme) new-cafe)
 (footwm globals)
 (footwm xlib)
 (prefix (footwm xutil) xutil.)
 ;; Import all local modules so they're accessable from the command line.
 (prefix (footwm ewmh) ewmh.)
 (prefix (footwm hints) hints.)
 (prefix (footwm icccm) icccm.)
 (prefix (footwm op) op.)
 (prefix (footwm shell) shell.)
 (prefix (footwm wm) wm.))

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
