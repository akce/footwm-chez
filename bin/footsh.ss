#! /usr/bin/scheme --script

;; Footwm shell (command line) interface.
;;
;; Written by Akce 2019-2020.
;;
;; SPDX-License-Identifier: Unlicense

(suppress-greeting #t)

(import
 (rnrs base)
 (only (chezscheme) new-cafe)
 (footwm xlib)
 ;; Import all local modules so they're accessable from the command line.
 (prefix (footwm ewmh) ewmh.)
 (prefix (footwm hints) hints.)
 (prefix (footwm icccm) icccm.)
 (prefix (footwm wm) wm.)
 (prefix (footwm shell) shell.)
 (prefix (footwm footwm) footwm.))

(current-display (x-open-display))
(root (x-default-root-window))

(x-init-atoms)
(icccm.init-atoms)
(ewmh.init-atoms)
(hints.init-atoms)

(let ([argv (command-line)])
  (cond
   [(fx=? (length argv) 1)
    (new-cafe)]
   [else
    (shell.main argv)]))
