#! /usr/bin/scheme --script

;; Footwm key handler app.
;;
;; Written by Akce 2019-2020.
;;
;; SPDX-License-Identifier: Unlicense

(suppress-greeting #t)
(debug-on-exception #t)

(import
 (rnrs base)
 (only (chezscheme) new-cafe)
 (prefix (footwm ewmh) ewmh.)
 (prefix (footwm hints) hints.)
 (prefix (footwm icccm) icccm.)
 (prefix (footwm keys) keys.)
 (footwm xlib))

(current-display (x-open-display))
(root (x-default-root-window))

(x-init-atoms)
(icccm.init-atoms)
(ewmh.init-atoms)
(hints.init-atoms)

(define default-config
  (lambda ()
    (string-append (getenv "HOME") "/" ".foot" "/" "footkeysconfig.sls")))

(let ([bin (car (command-line))]
      [argv (command-line-arguments)])
  (cond
   [(null? argv)
    (keys.main (default-config))]
   [(string=? (car argv) "shell")
    (keys.shell)]
   [else
    (keys.main (car argv))]))
