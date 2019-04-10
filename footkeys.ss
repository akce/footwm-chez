#! /usr/bin/scheme --script

(suppress-greeting #t)
(compile-imported-libraries #t)

(import
 (rnrs base)
 (only (chezscheme) new-cafe)
 (prefix (ewmh) ewmh.)
 (globals)
 (prefix (icccm) icccm.)
 (prefix (keys) keys.)
 (prefix (wm) wm.)
 (xlib)
 (prefix (xutil) xutil.))

(current-display (xutil.open))
(root (XDefaultRootWindow (current-display)))

(icccm.init-atoms)
(ewmh.init-atoms)
(wm.init-atoms)

(define default-config
  (lambda ()
    (string-append (config-path) "/" ".footkeyconfig.ss")))

(let ([bin (car (command-line))]
      [argv (command-line-arguments)])
  (cond
   [(null? argv)
    (keys.main (default-config))]
   [(string=? (car argv) "shell")
    (keys.shell)]
   [else
    (keys.main (car argv))]))
