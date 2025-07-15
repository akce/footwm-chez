(library (footwm fnmatch)
  (export
    fnmatch
    fnsearch
    )
  (import
    (chezscheme))

  (define lib-load
    (load-shared-object #f))

  (define %fnmatch
    (foreign-procedure "fnmatch" (string string int) int))

  (define FNM_CASEFOLD (bitwise-arithmetic-shift-left 1 4))
  (define FNM_NOMATCH 1)

  (define fnmatch
    (lambda (needle haystack)
      (fx=? (%fnmatch needle haystack FNM_CASEFOLD) 0)))

  (define any-upper?
    (lambda (str)
      (let loop ([cs (string->list str)])
        (cond
          [(null? cs)
           #f]
          [(char-upper-case? (car cs))
           #t]
          [else
            (loop (cdr cs))]))))

  ;; Converts a needle such that it matches anywhere in a string, or as some regexp systems call it: a string search.
  (define search-needle
    (lambda (needle)
      (let loop ([n needle])
        (cond
          [(fx=? (string-length n) 0)
           "*"]
          [(not (char=? (string-ref n 0) #\*))
           (loop (string-append "*" n))]
          [(not (char=? (string-ref n (fx- (string-length n) 1)) #\*))
           (loop (string-append n "*"))]
          [else
            n]))))

  (define fnsearch
    (lambda (needle haystack)
      ;; Any upper-case characters in needle mark the search as case-sensitive.
      (fx=?
        (%fnmatch
          (search-needle needle)
          haystack
          (if (any-upper? needle)
            0
            FNM_CASEFOLD)) 0)))

  )
