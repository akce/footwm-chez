#! /usr/bin/scheme --script

(suppress-greeting #t)
(compile-imported-libraries #t)
(debug-on-exception #t)

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
     (list g-type-int g-type-string g-type-int)
     ;; The current desktop is not included in this list as there's no point selecting the already selected desktop.
     (let* ([ds (cdr (ewmh.desktop-names))]
            [idxs (enumerate ds)])
       (map list idxs ds (map add1 idxs))))))

(define parse-args
  (lambda (argv)
    (let ([argc (length argv)])
      (void))))

(define main
  (lambda ()
    (parse-args (command-line-arguments))
    (menu "Footdesk" (make-desktop-data)
      (lambda (row)
        (ewmh.current-desktop-request! (list-ref row 2))
        (xutil.sync)))))

(main)
