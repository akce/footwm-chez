#! /usr/bin/env scheme-script

;; Script to dump keysym values from X header files.
;;
;; Requires irregex.

(import
 (chezscheme)
 (irregex))

(define parse-x-key
  (lambda (line)
    (let ([m
           (irregex-search
            '(: bol "#define" (+ space) "XK_" ($ (+ (or (/ "azAZ09") "_"))) (+ space) "0x" ($ (+ (/ "09af"))))
            line)])
      (when m
        (display (irregex-match-substring m 1))
        (display "\t0x")
        (display (irregex-match-substring m 2))
        (newline)))))

(let ([f
       (open-file-input-port
        (if (null? (command-line-arguments))
            "/usr/include/X11/keysymdef.h"
            (car (command-line-arguments)))
        (file-options no-create)
        (buffer-mode line)
        (make-transcoder (utf-8-codec)))])
  (let loop ((line (get-line f)))
    (cond
     [(eof-object? line)
      (close-port f)]
     [else
      (parse-x-key line)
      (loop (get-line f))])))
