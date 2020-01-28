#! /usr/bin/scheme --script

(suppress-greeting #t)
(debug-on-exception #t)

(import
 (chezscheme)
 (prefix (footwm ewmh) ewmh.)
 (footwm gobject)
 (prefix (footwm hints) hints.)
 (footwm menugtk)
 (footwm xlib))

(current-display (x-open-display))
(root (x-default-root-window))

(ewmh.init-atoms)
(hints.init-atoms)

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
      ;; activation command
      (lambda (row)
        (ewmh.current-desktop-request! (list-ref row 2))
        (x-sync))
      ;; creation command
      (lambda (name)
        ;; Create a new desktop.
        (hints.desktop-add-set! name 0)
        (x-sync))
      ;; delete command
      (lambda (rows)
        (for-each
         (lambda (row)
           (hints.desktop-delete-set! (list-ref row 2)))
         rows)
        (x-sync)))))

(main)
