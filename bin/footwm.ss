#! /usr/bin/scheme --script

(suppress-greeting #t)
(debug-on-exception #t)

(import
 (rnrs base)
 (prefix (footwm ewmh) ewmh.)
 (prefix (footwm hints) hints.)
 (prefix (footwm icccm) icccm.)
 (prefix (footwm wm) wm.)
 (footwm xlib))

(current-display (x-open-display))
(root (x-default-root-window))

(x-init-atoms)
(icccm.init-atoms)
(ewmh.init-atoms)
(hints.init-atoms)

(wm.main)
