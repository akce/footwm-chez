(library (xutil)
  (export
   atom-name
   cardinal-set!
   open
   property->string
   property->string*
   property->u32*
   send-message-cardinal
   text-property-set!

   make-atoms
   init-atoms
   make-atom-ref
   )
  (import (chezscheme)
          (xlib))

  ;; [syntax] (fmem ((var varptr type)) ...)
  (define-syntax fmem
    (syntax-rules ()
      [(_ ((var varptr type) ...) first rest ...)
       (let ([var (foreign-alloc (ftype-sizeof type))] ...)
         (let ([varptr (make-ftype-pointer type var)] ...)
           (let ((r first))
             ;; TODO should be wrapped via exceptions.
             rest ...
             (foreign-free var) ...
             r)))]))

  (define atom-name
    (lambda (d a)
      (let* ([ptr (XGetAtomName d a)]
             [str (ptr->string ptr)])
        (XFree ptr)
        str)))

  ;; wraps XOpenDisplay so the connection string is optional.
  (define open
    (case-lambda
     [() (open #f)]
     [(s) (XOpenDisplay s)]))

   ;;;; basic atom manager.
  ;; Just a very thin wrapper around hash tables.
  (define make-atoms make-eq-hashtable)

  ;; initialise atoms.
  ;; For those atoms that require display so can only be initialised after a connection to X is made.
  (define init-atoms
    (lambda (d atoms atom-list)
      (for-each
       (lambda (a)
         (hashtable-set! atoms a (XInternAtom d (symbol->string a) #f)))
       atom-list)))

  (define make-atom-ref
    (lambda (atoms)
      (lambda (a)
        (hashtable-ref atoms a #f))))

  (define ptr->utf8s
    (lambda (text-list nitems)
      (let ([n (foreign-ref 'integer-32 nitems 0)]
            [strvect (foreign-ref 'void* text-list 0)]	;; strvect = vector of strings (utf8**)
            [sz (ftype-sizeof void*)])
        (do ([i 0 (+ i 1)]
             [v (make-vector n)
                (let ([saddr (foreign-ref 'void* strvect (* i sz))])
                  (vector-set! v i (ptr->string saddr))
                  v)])
            ;; TODO consider limiting to n-1 since the last string always seems to be "".
            ((= i n) v)))))

  (define cardinal-set!
    (lambda (d wid atomprop value)
      (fmem ([num &num unsigned-32])
            (foreign-set! 'unsigned-32 num 0 value)
            (XChangeProperty d wid atomprop XA-CARDINAL 32 0 &num 1))))

  (define property->string
    (lambda (d wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (XGetTextProperty d wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (let* (#;[enc (ftype-ref XTextProperty (encoding) &tp)]
                         #;[num (ftype-ref XTextProperty (nitems) &tp)]
                         [addr (ftype-pointer-address (ftype-ref XTextProperty (value) &tp))]
                         [str (ptr->string addr)])
                    #;(display (format "encoding ~d:~s nitems ~d ~n" enc (atom-name d enc) num))
                    (XFree addr)
                    str)
                  #f)))))

  (define property->string*
    (lambda (d wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (XGetTextProperty d wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (fmem ([nitems &nitems integer-32]
                         [text-list &text-list u8**])
                        (let* ([stat (Xutf8TextPropertyToTextList d &tp &text-list &nitems)]
                               [res (ptr->utf8s text-list nitems)])
                          ;; TODO text-list foreign-ref is also calc'd in text-list->utf8s. Need to re-org.
                          (XFreeStringList (foreign-ref 'void* text-list 0))
                          res))
                  #f)))))

  (define (ptr->string fptr)
      (utf8->string
       (let f ([i 0])
         (let ([c (foreign-ref 'unsigned-8 fptr i)])
           (if (fx= c 0)
               (make-bytevector i)
               (let ([bv (f (fx+ i 1))])
                 (bytevector-u8-set! bv i c)
                 bv))))))

  ;; internal: extract numbers from pointer location.
  (define ptr->u32*
    (lambda (ptr len)
      (do ([i 0 (+ i 1)]
           [v (make-vector len) (begin
                                  (vector-set! v i (foreign-ref 'unsigned-32 ptr (* i (ftype-sizeof unsigned-32))))
                                  v)])
          ((= i len) v))))

  ;; window property to vector of u32's.
  (define property->u32*
    (lambda (d wid propatom atomtype)
      (fmem ([atr &atr atom]		;; atr = actual type return
             [afr &afr integer-32]	;; afr = actual format return
             [nir &nir unsigned-long]	;; nir = number of items return
             [bar &bar unsigned-long]	;; bar = bytes after return
             [pr  &pr u8*])		;; pr  = property return
            (let ([rc (XGetWindowProperty d wid propatom 0 2048 #f atomtype &atr &afr &nir &bar &pr)])
              (if (= rc 0)
                  ;; success: extract window ids from pr.
                  (let* ([pr* (foreign-ref 'void* pr 0)]
                         [nums (ptr->u32* pr* (foreign-ref 'unsigned-long nir 0))])
                    (XFree pr*)
                    nums)
                  ;; failure: return empty list.
                  (list))))))

  (define send-message-cardinal
    (lambda (d root wid atom value)
      (fmem ([ev &ev XEvent])
            (let ([event-mask (fxlogor SubstructureNotify SubstructureRedirect)])
              (ftype-set! XEvent (client-message type) &ev ClientMessage)
              (ftype-set! XEvent (client-message wid) &ev wid)
              (ftype-set! XEvent (client-message message-type) &ev atom)
              (ftype-set! XEvent (client-message send-event) &ev #t)
              (ftype-set! XEvent (client-message format) &ev 32)
              (ftype-set! XEvent (client-message data l 0) &ev value)
              (ftype-set! XEvent (client-message data l 1) &ev 0)	;; zero out.
              (XSendEvent d root #f event-mask &ev)))))

  (define strdup
    (lambda (str)
      ;; foreign-alloc every string and copy in the bytes.
      (let* ([bv (string->utf8 str)]
             [len (bytevector-length bv)])
        (let ([ret
               (do ([i 0 (fx+ i 1)]
                    [fv (foreign-alloc (fx+ 1 len))
                        (begin
                          (foreign-set! 'unsigned-8 fv i (bytevector-u8-ref bv i))
                          fv)])
                   ((= i len) fv))])
          (foreign-set! 'unsigned-8 ret len 0)	;; null terminate.
          ret))))

  (define str*->u8**
    (lambda (str*)
      (let ([len (length str*)])
        (do ([i 0 (+ i 1)]
             [v (foreign-alloc (* len (ftype-sizeof u8*)))
                (let ([fstr (strdup (list-ref str* i))])
                  (foreign-set! 'void* v (* i (ftype-sizeof u8*)) fstr)
                  v)])
            ((= i len) v)))))

  (define free/u8**
    (lambda (u8** len)
      ;; free individual string pointers.
      (for-each
       (lambda (i)
         (foreign-free (foreign-ref 'void* u8** (* i (ftype-sizeof u8*)))))
       (iota len))
      ;; free containing block.
      (foreign-free u8**)))

  (define text-property-set!
    (lambda (d wid str* propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([u8mem (str*->u8** str*)])
              (let ([rc (Xutf8TextListToTextProperty d u8mem (length str*) UTF8String &tp)])
                (if (fx= rc 0)
                    (XSetTextProperty d wid &tp propatom))
                (free/u8** u8mem (length str*))
                ;; the steps below cast tp->value to void*.
                (XFree (ftype-pointer-address (make-ftype-pointer void* (ftype-pointer-address (ftype-ref XTextProperty (value) &tp))))))))))
)
