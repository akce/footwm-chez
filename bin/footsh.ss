#! /usr/bin/env -S chez-scheme --debug-on-exception --eehistory ~/.foot/footsh_history --script

;; Footwm shell (command line) interface.
;;
;; Written by Jerry 2019-2025.
;;
;; SPDX-License-Identifier: Unlicense

(import
 (chezscheme)
 (footwm xlib)
 (prefix (footwm ewmh) ewmh.)
 (prefix (footwm hints) hints.)
 (prefix (footwm icccm) icccm.)
 (prefix (footwm wm) wm.)
 (prefix (footwm shell) shell.)
 (prefix (footwm footwm) footwm.)
 ;; Any import additions here need to be added to foot-env below.
 )

(current-display (x-open-display))
(root (x-default-root-window))

(x-atom 'intern)
(icccm.atom 'intern)
(ewmh.atom 'intern)
(hints.atom 'intern)

;; Refer to any library that needs to be compiled into the program executable
;; at least once at the top level so that compile-whole-program embeds it.
;; Referencing any public symbol seems to be enough.
(define xxx footwm.main)
(define yyy wm.window-name)

(let ([argv (command-line)])
  (cond
   [(fx=? (length argv) 1)
    ;; NOTE: new-cafe does *not* access any of the above footwm imports in --program mode. ie, compile-whole-program.
    ;; It seems to have a different value for interaction-environment so we'll change it to use an environment
    ;; with footwm symbols available to it.
    (let ([foot-env
            (copy-environment
              (environment
                '(chezscheme)
                '(footwm xlib)
                ;; Import all local modules so they're immediately accessable from the cafe.
                '(prefix (footwm ewmh) ewmh.)
                '(prefix (footwm hints) hints.)
                '(prefix (footwm icccm) icccm.)
                '(prefix (footwm wm) wm.)
                '(prefix (footwm shell) shell.)
                '(prefix (footwm footwm) footwm.)))])
      ;; Setting our own interaction-environment (rather than own eval only) includes our symbols in TAB completion.
      (parameterize ([interaction-environment foot-env]
                     [print-radix 16])
        (new-cafe)))]
   [else
    (shell.main argv)]))
