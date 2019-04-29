#! /usr/bin/scheme-script

(import
 (chezscheme)
 (prefix (ewmh) ewmh.)
 (globals)
 (gobject)
 (menugtk)
 (xlib)
 (prefix (xutil) xutil.))

(current-display (xutil.open))
(root (XDefaultRootWindow (current-display)))

(ewmh.init-atoms)

(define make-desktop-data
  (lambda ()
    (make-table
     '("Id" "Desktop")
     (list g-type-int g-type-string)
     (let ([ds (ewmh.desktop-names)])
       (map list (enumerate ds) ds)))))

(define parse-args
  (lambda (argv)
    (let ([argc (length argv)])
      (void))))

(define main
  (lambda ()
    (parse-args (command-line-arguments))
    (menu "Footdesk" (make-desktop-data)
      (lambda (row)
        (ewmh.current-desktop-request! (car row))
        (xutil.sync)))))

(main)
