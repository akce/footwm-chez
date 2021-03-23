#! /usr/bin/scheme --script

;; Foot window manager.
;;
;; Written by Akce 2019-2020.
;;
;; SPDX-License-Identifier: Unlicense

(suppress-greeting #t)
(debug-on-exception #t)

(import
 (rnrs base)
 (footwm config)
 (prefix (footwm ewmh) ewmh.)
 (prefix (footwm hints) hints.)
 (prefix (footwm icccm) icccm.)
 (prefix (footwm footwm) footwm.)
 (footwm xlib))

(define conf
  (let ([argv (cdr (command-line))])
    (cond
      [(null? argv)
       (let ([default-file "~/.foot/footwmconfig.sls"])
         (if (file-exists? default-file)
             (config-load default-file)
             '((desktops "Default"))))]
      [else
        (config-load (car argv))])))

(current-display (x-open-display))
(root (x-default-root-window))

(x-atom 'intern)
(icccm.atom 'intern)
(ewmh.atom 'intern)
(hints.atom 'intern)

(footwm.main (config-section conf desktops) (config-section conf assignments))
