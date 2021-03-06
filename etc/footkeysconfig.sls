;; SPDX-License-Identifier: Unlicense
;;
;; Sample footkeys configuration.
;;
;; Copy to ~/.foot/footkeysconfig.sls and edit to suit.
;;
;; The Makefile provides a target to perform the copy:
;;
;;   $ make install-config

(import
 (rnrs base)
 (only (chezscheme) system)
 (footwm keys)
 (prefix (footwm ewmh) ewmh.)
 (prefix (footwm hints) hints.)
 (prefix (footwm wm) wm.))

(define terminal
  (let ([t "urxvtc"])
    (case-lambda
     [()	(system t)]
     [(cmdstr)
      (system (string-append t " -e " cmdstr))])))

(key-config
 ;; Window selection.
 ('F1 '()
   (wm.activate-window/index 1))
 ('F2 '()
   (wm.activate-window/index 2))
 ('F3 '()
   (wm.activate-window/index 3))
 ;; Window menu.
 ('F4 '()
   (system "footwin"))
 ;; Desktop selection.
 ('F5 '()
   (ewmh.current-desktop-request! 1))
 ('F6 '()
   (ewmh.current-desktop-request! 2))
 ('F7 '()
   (ewmh.current-desktop-request! 3))
 ;; Desktop menu.
 ('F8 '()
   (system "footdesk"))
 ;; Close current window/desktop.
 ('F9 `(,(alt))
   (wm.close-window/index 0))
 ('F10 `(,(alt))
   (hints.desktop-delete-set! 0))
 ;; Terminal shortcut.
 ('Return `(,(alt))
   (terminal)))
