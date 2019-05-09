#! /usr/bin/scheme --script

(suppress-greeting #t)
(compile-imported-libraries #t)
(debug-on-exception #t)

(import
 (rnrs base)
 (only (chezscheme) new-cafe)
 (prefix (ewmh) ewmh.)
 (globals)
 (prefix (hints) hints.)
 (prefix (icccm) icccm.)
 (prefix (keys) keys.)
 (xlib)
 (prefix (xutil) xutil.))

(current-display (xutil.open))
(root (XDefaultRootWindow (current-display)))

(icccm.init-atoms)
(ewmh.init-atoms)
(hints.init-atoms)

(define default-config
  (lambda ()
    (string-append (config-path) "/" "footkeysconfig.ss")))

(let ([bin (car (command-line))]
      [argv (command-line-arguments)])
  (cond
   [(null? argv)
    (keys.main (default-config))]
   [(string=? (car argv) "shell")
    (keys.shell)]
   [else
    (keys.main (car argv))]))
