(library (footwm xutil)
  (export
   make-geometry
   geometry-x
   geometry-y
   geometry-width
   geometry-height
   geometry=?
   xconfigureevent-geometry
   window-attributes
   window-attributes-geom
   window-attributes-override-redirect
   window-attributes-map-state

   cardinal-set!
   get-window-attributes
   property->string
   property->string*
   get-property-ptr
   property->ulongs
   ulongs-property-set!
   send-message-cardinal
   text-property-set!

   resize-window)
  (import
   (rnrs)
   (only (chezscheme)
         format fxlogor fx= iota values
         getenv
         lock-object unlock-object foreign-callable foreign-callable-entry-point
         foreign-alloc foreign-free foreign-ref foreign-set! ftype-pointer-address ftype-ref ftype-set! ftype-sizeof make-ftype-pointer)
   (footwm ftypes-util)
   (footwm util)
   (footwm xlib))

  (define-record-type geometry
    (fields x y width height))

  (define geometry=?
    (lambda (lhs rhs)
      (and (eq? (geometry-x lhs) (geometry-x rhs))
           (eq? (geometry-y lhs) (geometry-y rhs))
           (eq? (geometry-width lhs) (geometry-width rhs))
           (eq? (geometry-height lhs) (geometry-height rhs)))))

  (define xconfigureevent-geometry
    (lambda (ev)
      (make-geometry (xconfigureevent-x ev) (xconfigureevent-y ev) (xconfigureevent-width ev) (xconfigureevent-height ev))))

  (define-record-type window-attributes
    (fields geom override-redirect map-state))

  (define cardinal-set!
    (lambda (wid atomprop value)
      (fmem ([num &num unsigned-32])
            (foreign-set! 'unsigned-32 num 0 value)
            (x-change-property wid atomprop (x-atom-ref 'CARDINAL) 32 0 num 1))))

  (define ulongs-property-set!
    (lambda (wid atomprop ulongs typeatom)
      (let ([len (length ulongs)])
        (fmem ([mem &mem unsigned-long len])
          (fill-memory/ulongs mem ulongs)
          (x-change-property wid atomprop typeatom 32 0 mem len)))))

  (define property->string
    (lambda (wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (x-get-text-property wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (let* (#;[enc (ftype-ref XTextProperty (encoding) &tp)]
                         #;[num (ftype-ref XTextProperty (nitems) &tp)]
                         [addr (ftype-pointer-address (ftype-ref XTextProperty (value) &tp))]
                         [str (ptr->string addr)])
                    #;(display (format "encoding ~d:~s nitems ~d ~n" enc (x-get-atom-name enc) num))
                    (x-free addr)
                    str)
                  #f)))))

  (define property->string*
    (lambda (wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (x-get-text-property wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (fmem ([nitems &nitems int]
                         [text-list &text-list u8**])
                        (let* ([stat (xutf8-text-property-to-text-list &tp &text-list &nitems)]
                               [res (ptr->utf8s text-list nitems)])
                          ;; TODO text-list foreign-ref is also calc'd in text-list->utf8s. Need to re-org.
                          (x-free-string-list (foreign-ref 'void* text-list 0))
                          res))
                  (list))))))

  ;; Return list/pair (address length) of property memory data or #f on failure.
  ;; Caller *Must* foreign-free returned address.
  ;; (Using list as the fmem macro won't allow a values return.)
  (define get-property-ptr
    (lambda (wid propatom atomtype)
      (fmem ([atr &atr atom]		;; atr = actual type return
             [afr &afr int]		;; afr = actual format return
             [nir &nir unsigned-long]	;; nir = number of items return
             [bar &bar unsigned-long])	;; bar = bytes after return
        ;; pr  = property return
        ;; declared outside fmem as ownership is transferred to caller.
        (let* ([pr (foreign-alloc (ftype-sizeof void*))]
               [&pr (make-ftype-pointer void* pr)]
               [rc (x-get-window-property wid propatom 0 2048 #f atomtype &atr &afr &nir &bar &pr)])
          (if (= rc 0)
              ;; maybe success: make sure there was something returned.
              (let ([len (foreign-ref 'unsigned-long nir 0)])
                (if (> len 0)
                    ;; more success: return data ptr and length.
                    (list pr len)
                    (begin
                      ;; nothing back, free memory and return.
                      (x-free (foreign-ref 'void* pr 0))
                      (unlock-object pr)
                      (foreign-free pr)
                      #f)))
              ;; failure: return false.
              #f)))))

  ;; window property to list of unsigned longs.
  (define property->ulongs
    (lambda (wid propatom atomtype)
      (let ([ptrlen (get-property-ptr wid propatom atomtype)])
        (if ptrlen
            ;; success: extract window ids from pr.
            (let* ([ptr (list-ref ptrlen 0)]
                   [*ptr (foreign-ref 'void* ptr 0)]
                   [len (list-ref ptrlen 1)]
                   [nums (ptr->ulongs *ptr len)])
              (x-free *ptr)
              (foreign-free ptr)
              nums)
            ;; failure: return empty.
            (list)))))

  (define send-message-cardinal
    (lambda (root wid atom value)
      (fmem ([ev &ev XEvent])
            (let ([event-mask (fxlogor SubstructureNotify SubstructureRedirect)])
              (ftype-set! XEvent (client-message xany type) &ev ClientMessage)
              (ftype-set! XEvent (client-message xany wid) &ev wid)
              (ftype-set! XEvent (client-message xany send-event) &ev #t)
              (ftype-set! XEvent (client-message message-type) &ev atom)
              (ftype-set! XEvent (client-message format) &ev 32)
              (ftype-set! XEvent (client-message data l 0) &ev value)
              (ftype-set! XEvent (client-message data l 1) &ev 0)	;; zero out.
              (x-send-event root #f event-mask &ev)))))

  (define text-property-set!
    (lambda (wid str* propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([u8mem (str*->u8** str*)])
              (let ([rc (xutf8-text-list-to-text-property u8mem (length str*) UTF8String &tp)])
                (if (fx= rc 0)
                    (x-set-text-property wid &tp propatom))
                (free/u8** u8mem (length str*))
                (x-free (ftype-pointer-address (void*-cast (ftype-ref XTextProperty (value) &tp)))))))))

  (define get-window-attributes
    (lambda (wid)
      (fmem ([wa &wa XWindowAttributes])
            (let ([rc (x-get-window-attributes wid &wa)])
              (if (= rc 0)
                  ;; failure.
                  #f
                  (let ([x (ftype-ref XWindowAttributes (x) &wa)]
                        [y (ftype-ref XWindowAttributes (y) &wa)]
                        [w (ftype-ref XWindowAttributes (width) &wa)]
                        [h (ftype-ref XWindowAttributes (height) &wa)]
                        [map-state (ftype-ref XWindowAttributes (map-state) &wa)]
                        [override (ftype-ref XWindowAttributes (override-redirect) &wa)])
			(make-window-attributes (make-geometry x y w h) override map-state)))))))

  (define resize-window
    (lambda (wid geo)
      (let ([change-mask 0])
        (fmem ([changes &changes XWindowChanges])
          (when (geometry-x geo)
            (ftype-set! XWindowChanges (x) &changes (geometry-x geo))
            (set! change-mask (bitwise-copy-bit change-mask CWX 1)))
          (when (geometry-y geo)
            (ftype-set! XWindowChanges (y) &changes (geometry-y geo))
            (set! change-mask (bitwise-copy-bit change-mask CWY 1)))
          (when (geometry-width geo)
            (ftype-set! XWindowChanges (width) &changes (geometry-width geo))
            (set! change-mask (bitwise-copy-bit change-mask CWWidth 1)))
          (when (geometry-height geo)
            (ftype-set! XWindowChanges (height) &changes (geometry-height geo))
            (set! change-mask (bitwise-copy-bit change-mask CWHeight 1)))
          (x-configure-window wid change-mask &changes)
          #;(display (format "#x~x x-configure-window change-mask #b~b~n" wid change-mask))))))
)
