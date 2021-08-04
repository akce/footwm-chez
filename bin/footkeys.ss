#! /usr/bin/env -S chez-scheme --quiet --debug-on-exception --program

;; Footwm key handler app.
;;
;; Written by Jerry 2019-2021.
;;
;; SPDX-License-Identifier: Unlicense

(import
 (rnrs)
 (only (chezscheme) command-line-arguments getenv new-cafe)
 (prefix (footwm ewmh) ewmh.)
 (prefix (footwm hints) hints.)
 (prefix (footwm icccm) icccm.)
 (prefix (footwm keys) keys.)
 (footwm xlib))

(current-display (x-open-display))
(root (x-default-root-window))

(x-atom 'intern)
(icccm.atom 'intern)
(ewmh.atom 'intern)
(hints.atom 'intern)

(define default-config
  (lambda ()
    (string-append (getenv "HOME") "/" ".foot" "/" "footkeysconfig.sls")))

(let ([bin (car (command-line))]
      [argv (command-line-arguments)])
  (cond
   [(null? argv)
    (keys.main (default-config))]
   [else
    (keys.main (car argv))]))
