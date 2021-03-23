(library
  (footwm config)
  (export
    config-load
    config-section)
  (import
    (rnrs))

  (define config-load
    (lambda (filename)
      (let ([f (open-input-file filename)])
        (let loop ([i (read f)] [acc '()])
          (cond
            [(eof-object? i)
             (reverse acc)]
            [else
              (loop (read f) (cons i acc))])))))

  (define-syntax config-section
    (syntax-rules ()
      [(_ config name)
       (cond
         [(find
            (lambda (x)
              (eq? 'name (car x)))
            config)
          => cdr]
         [else
           '()])]))

  )
