;; Footwm shell (command line) interface functions.
;;
;; Written by Akce 2019-2020.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm shell)
  (export
   main
   desktops
   windows)
  (import
   (rnrs)
   (only (chezscheme) enumerate format)
   (prefix (footwm ewmh) ewmh.)
   (prefix (footwm hints) hints.)
   (prefix (footwm icccm) icccm.)
   (prefix (footwm wm) wm.)
   (only (footwm util) list-find-index)
   (footwm xlib))

(define main
  (lambda (argv)
    (let ([binary (car argv)]
          [cmd (list-ref argv 1)]
          [args (cdr (cdr argv))])
      (cond
       [(or (string=? "h" cmd) (string=? "help" cmd))
        (help binary)]
       [(string=? "da" cmd)
        (hints.desktop-add-set! (list-ref args 0) (string->number (list-ref args 1)))]
       [(string=? "dc" cmd)
        (hints.desktop-delete-set! (get-desktop-id (list-ref args 0)))]
       [(string=? "dl" cmd)
        (desktops)]
       [(string=? "dr" cmd)
        (hints.desktop-rename-set! (get-desktop-id (list-ref args 0)) (list-ref args 1))]
       [(string=? "ds" cmd)
        (ewmh.current-desktop-request! (get-desktop-id (list-ref args 0)))]
       [(string=? "wb" cmd)
        (icccm.client-iconify-message (string->number (list-ref args 0)))]
       [(string=? "wc" cmd)
        (ewmh.window-close-request! (string->number (list-ref args 0)))]
       [(string=? "wd" cmd)
        (ewmh.window-desktop-request! (string->number (list-ref args 0)) (string->number (list-ref args 1)))]
       [(string=? "wl" cmd)
        (windows)]
       [(string=? "ws" cmd)
        (wm.activate-window/index (string->number (list-ref args 0)))]
       [else
        (display "command not understood. showing help.")
        (newline)
        (help binary)])
      (x-sync))))

(define help
  (lambda (binary)
    (display
     (format "\
~a [[command] [args...]]

where [command] is one of:
  da <name> <desktop-index>
    Add a new desktop named <name> at position <desktop-index>
  dc <desktop-index-or-name>
    Close the desktop referenced by <desktop-index-or-name>
  dl
    List desktops
  dr <desktop-index-or-name> <new-name>
    Rename desktop referenced by <desktop-index-or-name> with <new-name>
  ds <desktop-index-or-name>
    Select desktop referenced by <desktop-index-or-name>
  wc <window-id>
    Close the window given by <window-id>
  wd <window-id> <desktop-index>
    Move window given by <window-id> to desktop with index <desktop-index>
  ws <window-id>
    Select (bring to front) window with id <window-id>
  wl
    List all windows, regardless desktop

Enter shell mode if no [command] given.
"
      binary))))

  (define desktops
    (lambda ()
      (let ([names ewmh.desktop-names])
        (for-each
         (lambda (desk)
           (display (desktop-display-string desk))
           (newline))
         (map cons (enumerate names) names)))))

  ;; prints out the windows list in most-recently-used order.
  (define windows
    (lambda ()
      (let ([wids ewmh.client-list])
        (for-each
         (lambda (wid i)
           (display (window-display-string wid i))
           (newline))
         (wm.window-sort wids) (enumerate wids)))))

  (define desktop-display-string
    (lambda (desk)
      (format "~d ~a" (car desk) (cdr desk))))

  (define window-display-string
    (lambda (wid i)
      (let ([c (icccm.class-hint wid)])
        ;; window id desktop resource class title
        (format
         "#x~x ~a ~a ~a ~a ~a"
         wid
         i
         (ewmh.window-desktop wid)
         (icccm.class-hint-instance c)
         (icccm.class-hint-class c)
         (wm.window-name wid)))))

  (define get-desktop-id
    (lambda (number-or-name)
      (cond
        [(string->number number-or-name)
         => values]
        [else
          (list-find-index
            (lambda (d)
              (string-ci=? d number-or-name))
            ewmh.desktop-names)])))
  )
