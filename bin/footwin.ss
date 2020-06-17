#! /usr/bin/scheme --script

(suppress-greeting #t)
(debug-on-exception #t)

(import
 (chezscheme)
 (prefix (footwm ewmh) ewmh.)
 (footwm gobject)
 (prefix (footwm icccm) icccm.)
 (footwm menugtk)
 (prefix (footwm wm) wm.)
 (footwm xlib))

(current-display (x-open-display))
(root (x-default-root-window))

(x-init-atoms)
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

(define urgency-flag
  (lambda (wid)
    (let ([e (ewmh.demands-attention? wid)]
          [i (icccm.wm-hints-urgency (icccm.get-wm-hints wid))])
      (cond
       [(and e i) "*"]
       [e "-"]
       [i "+"]
       [else ""]))))

;; The current window is not included in this list as there's no point selecting the already selected window.
(define make-window-rows
  (lambda ()
    (let ([desks (ewmh.desktop-names)]
          [wids (remove-active-window (wm.window-sort (ewmh.client-list)))])
      (map
       (lambda (i wid)
         (let ([c (icccm.class-hint wid)]
               [dname (list-ref desks (ewmh.window-desktop wid))])
           (list i (hex wid) dname (icccm.class-hint-instance c) (icccm.class-hint-class c) (urgency-flag wid) (wm.window-name wid) wid)))
       (enumerate wids) wids))))

(define make-window-data
  (lambda ()
    (make-table
     '("Index" "Id" "Desktop" "Instance" "Class" "U" "Title")
     (list g-type-int g-type-string g-type-string g-type-string g-type-string g-type-string g-type-string g-type-int)
     (make-window-rows))))

(define parse-args
  (lambda (argv)
    (let ([argc (length argv)])
      (void))))

(define main
  (lambda ()
    (parse-args (command-line-arguments))
    (menu "Footwin" (make-window-data)
      (lambda (row)
        (ewmh.window-active-request! (list-ref row 7))
        (x-sync))
      ;; Null creation func. Hmmm.. could this be an app launch function?
      (lambda (text)
        #t)
      ;; Delete command.
      (lambda (rows)
        (for-each
         (lambda (row)
           (ewmh.window-close-request! (list-ref row 7)))
         rows)
        (x-sync)))))

(main)
