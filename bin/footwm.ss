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
 (prefix (footwm ewmh) ewmh.)
 (prefix (footwm hints) hints.)
 (prefix (footwm icccm) icccm.)
 (prefix (footwm footwm) footwm.)
 (footwm xlib))

(current-display (x-open-display))
(root (x-default-root-window))

(x-init-atoms)
(icccm.init-atoms)
(ewmh.init-atoms)
(hints.init-atoms)

(footwm.main)
