#! /usr/bin/scheme-script

(import
 (chezscheme)
 (prefix (ewmh) ewmh.)
 (globals)
 (gobject)
 (prefix (icccm) icccm.)
 (menugtk)
 (prefix (op) op.)
 (xlib)
 (prefix (xutil) xutil.))

(current-display (xutil.open))
(root (XDefaultRootWindow (current-display)))

(ewmh.init-atoms)
(icccm.init-atoms)

(define hex
  (lambda (num)
    (format "#x~x" num)))

(define remove-active-window
  (lambda (wids)
    (cond
     [(null? wids) wids]
     ;; Comparing active-window handles the case where the current desktop is empty.
     [(eq? (car wids) (ewmh.active-window)) (cdr wids)]
     [else wids])))

;; The current window is not included in this list as there's no point selecting the already selected window.
(define make-window-rows
  (lambda ()
    (let ([desks (ewmh.desktop-names)]
          [wids (remove-active-window (reverse (op.window-sort (ewmh.client-list-stacking))))])
      (map
       (lambda (i wid)
         (let ([c (icccm.class-hint wid)]
               [dname (list-ref desks (ewmh.window-desktop wid))])
           (list i (hex wid) dname (list-ref c 0) (list-ref c 1) (op.window-name wid) wid)))
       (enumerate wids) wids))))

(define make-window-data
  (lambda ()
    (make-table
     '("Index" "Id" "Desktop" "Instance" "Class" "Title")
     (list g-type-int g-type-string g-type-string g-type-string g-type-string g-type-string g-type-int)
     (make-window-rows))))

(define parse-args
  (lambda (argv)
    (let ([argc (length argv)])
      (void))))

(define main
  (lambda ()
    (parse-args (command-line-arguments))
    (menu "Footdesk" (make-window-data)
      (lambda (row)
        (ewmh.window-active-request! (list-ref row 6))
        (xutil.sync)))))

(main)
