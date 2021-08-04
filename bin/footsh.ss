#! /usr/bin/env -S chez-scheme --debug-on-exception --eehistory ~/.foot/footsh_history --script

;; Footwm shell (command line) interface.
;;
;; Written by Jerry 2019-2021.
;;
;; SPDX-License-Identifier: Unlicense

(import
 (chezscheme)
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

(x-atom 'intern)
(icccm.atom 'intern)
(ewmh.atom 'intern)
(hints.atom 'intern)

(let ([argv (command-line)])
  (cond
   [(fx=? (length argv) 1)
    ;; NOTE: new-cafe does *not* include any footwm imports in --program mode. ie, compile-whole-program.
    ;; Manually include them till i work something out.
    (new-cafe)]
   [else
    (shell.main argv)]))
