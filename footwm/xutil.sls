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

   atom-name
   cardinal-set!
   get-child-windows
   get-next-event
   get-window-attributes
   open
   sync
   select-input
   install-default-error-handler
   install-error-handler
   property->string
   property->string*
   get-property-ptr
   property->ulongs
   ulongs-property-set!
   send-message-cardinal
   text-property-set!

   make-atom-manager

   resize-window)
  (import
   (rnrs)
   (only (chezscheme)
         format fxlogor fx= iota values
         getenv
         lock-object unlock-object foreign-callable foreign-callable-entry-point
         foreign-alloc foreign-free foreign-ref foreign-set! ftype-pointer-address ftype-ref ftype-set! ftype-sizeof make-ftype-pointer)
   (footwm ftypes-util)
   (footwm globals)
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

  (define atom-name
    (lambda (a)
      ;; Unset atom fields from messages will be zero, so account for that here as a convenience to print functions.
      (if (> a 0)
        (let* ([ptr (XGetAtomName (current-display) a)]
               [str (ptr->string ptr)])
          (XFree ptr)
          str)
        "")))

  ;; wraps XOpenDisplay so the connection string is optional.
  (define open
    (case-lambda
     [()
      ;; XOpenDisplay will crash unless it has a valid DISPLAY.
      (unless (getenv "DISPLAY")
        (raise (condition (make-error) (make-message-condition "DISPLAY not given. Exiting.."))))
      (open #f)]
     [(s) (XOpenDisplay s)]))

  ;; wraps XSync so discard boolean is optional.
  (define sync
    (case-lambda
     [() (sync #f)]
     [(s) (XSync (current-display) s)]))

  (define select-input
    (lambda (wid mask)
      (XSelectInput (current-display) wid mask)))

  ;; Installs a simple one-line printing error handler.
  (define install-default-error-handler
    (lambda ()
      (install-error-handler
       (lambda (dpy ev)
         (display (format "XError: type=~a wid=#x~x error=~d~n" (xerrorevent-type ev) (xerrorevent-resourceid ev) (xerrorevent-error-code ev)))))))

  (define install-error-handler
    ;; Store the previous locked lambda so that it can be unlocked if replaced.
    (let ([previous-lambda #f])
      (lambda (handler)
        (when previous-lambda
          (unlock-object previous-lambda)
          (set! previous-lambda #f))
        (XSetErrorHandler
         (if (procedure? handler)
             ;; Wrap the lambda for convenience:
             ;; - converts c event struct to scheme record
             ;; - returns int 0 so that connection to X server remains.
             (let ([f/proc
                    (foreign-callable
                     (lambda (d c/xerrorevent)
                       (let ([ev
                              (make-xerrorevent
                               (ftype-ref XErrorEvent (type) c/xerrorevent)
                               (ftype-ref XErrorEvent (d) c/xerrorevent)
                               (ftype-ref XErrorEvent (resourceid) c/xerrorevent)
                               (ftype-ref XErrorEvent (serial) c/xerrorevent)
                               (ftype-ref XErrorEvent (error-code) c/xerrorevent)
                               (ftype-ref XErrorEvent (request-code) c/xerrorevent)
                               (ftype-ref XErrorEvent (minor-code) c/xerrorevent))])
                         (handler d ev)
                         0))
                     (dpy* (* XErrorEvent)) int)])
               (lock-object f/proc)
               (set! previous-lambda f/proc)
               (foreign-callable-entry-point f/proc))
             ;; Else assume handler is a mem addr, eg as would be returned by the first call to XSetErrorHandler.
             handler)))))

  ;;;; basic atom manager.
  ;; Just a very thin wrapper around hash tables.
  (define make-atom-manager
    (lambda (atom-list)
      (define atoms (make-eq-hashtable))
      (define init-atoms
        ;; initialise atoms.
        ;; For those atoms that require display so can only be initialised after a connection to X is made.
        (lambda ()
          (for-each
           (lambda (a)
             (hashtable-set! atoms a (XInternAtom (current-display) (symbol->string a) #f)))
           atom-list)))
      (define atom-ref
        (lambda (a)
          (hashtable-ref atoms a #f)))
      (values init-atoms atom-ref)))

  (define cardinal-set!
    (lambda (wid atomprop value)
      (fmem ([num &num unsigned-32])
            (foreign-set! 'unsigned-32 num 0 value)
            (XChangeProperty (current-display) wid atomprop XA-CARDINAL 32 0 num 1))))

  (define ulongs-property-set!
    (lambda (wid atomprop ulongs typeatom)
      (let ([len (length ulongs)])
        (fmem ([mem &mem unsigned-long len])
          (fill-memory/ulongs mem ulongs)
          (XChangeProperty (current-display) wid atomprop typeatom 32 0 mem len)))))

  (define property->string
    (lambda (wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (XGetTextProperty (current-display) wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (let* (#;[enc (ftype-ref XTextProperty (encoding) &tp)]
                         #;[num (ftype-ref XTextProperty (nitems) &tp)]
                         [addr (ftype-pointer-address (ftype-ref XTextProperty (value) &tp))]
                         [str (ptr->string addr)])
                    #;(display (format "encoding ~d:~s nitems ~d ~n" enc (atom-name (current-display) enc) num))
                    (XFree addr)
                    str)
                  #f)))))

  (define property->string*
    (lambda (wid propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([rc (XGetTextProperty (current-display) wid &tp propatom)])
              (if (> rc 0)
                  ;; success
                  (fmem ([nitems &nitems int]
                         [text-list &text-list u8**])
                        (let* ([stat (Xutf8TextPropertyToTextList (current-display) &tp &text-list &nitems)]
                               [res (ptr->utf8s text-list nitems)])
                          ;; TODO text-list foreign-ref is also calc'd in text-list->utf8s. Need to re-org.
                          (XFreeStringList (foreign-ref 'void* text-list 0))
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
             [bar &bar unsigned-long]	;; bar = bytes after return
             [pr  &pr void*])		;; pr  = property return
        (let* ([pr (foreign-alloc (ftype-sizeof void*))]
               [&pr (make-ftype-pointer void* pr)]
               [rc (XGetWindowProperty (current-display) wid propatom 0 2048 #f atomtype &atr &afr &nir &bar &pr)])
          (if (= rc 0)
              ;; maybe success: make sure there was something returned.
              (let ([len (foreign-ref 'unsigned-long nir 0)])
                (if (> len 0)
                    ;; more success: return data ptr and length.
                    (list pr len)
                    (begin
                      ;; nothing back, free memory and return.
                      (XFree (foreign-ref 'void* pr 0))
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
              (XFree *ptr)
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
              (XSendEvent (current-display) root #f event-mask &ev)))))

  (define text-property-set!
    (lambda (wid str* propatom)
      (fmem ([tp &tp XTextProperty])
            (let ([u8mem (str*->u8** str*)])
              (let ([rc (Xutf8TextListToTextProperty (current-display) u8mem (length str*) UTF8String &tp)])
                (if (fx= rc 0)
                    (XSetTextProperty (current-display) wid &tp propatom))
                (free/u8** u8mem (length str*))
                (XFree (ftype-pointer-address (void*-cast (ftype-ref XTextProperty (value) &tp)))))))))

  (define get-child-windows
    (lambda ()
      (fmem ([root-return &rr window]
             [parent-return &pr window]
             [children-return &cr window*]
             [num-children &nc unsigned])
            (let ([rc (XQueryTree (current-display) (root) &rr &pr &cr &nc)])
              (if (and (> rc 0) (> num-children 0))
                  (let ([ptr (foreign-ref 'void* children-return 0)]
                        [len (foreign-ref 'unsigned num-children 0)])
                    (let ([wids (ptr->ulongs ptr len)])
                      (XFree ptr)
                      wids))
                  '())))))

  (define get-window-attributes
    (lambda (wid)
      (fmem ([wa &wa XWindowAttributes])
            (let ([rc (XGetWindowAttributes (current-display) wid &wa)])
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

  (define get-next-event
    (lambda ()
      (fmem ([ev &ev XEvent])
        (XNextEvent (current-display) &ev)
        (make-event &ev))))

  (define-syntax ftype-fields
    (syntax-rules ()
      [(_ type obj ((field name offset) ...))
       (list (ftype-ref XEvent (type field name offset) obj) ...)]
      [(_ type obj (field ...))
       (list (ftype-ref XEvent (type field) obj) ...)]))

  ;; Convert the cevent struct to a scheme record.
  ;; TODO this c-struct->scheme-record conversion to be done in xlib as part of define-xevent??
  (define make-event
    (lambda (cevent)
      (let ([evid (ftype-ref XEvent (type) cevent)])
        (case-equal? evid
          (ClientMessage
            (make-xclientmessageevent
             (make-xany cevent)
             (ftype-ref XEvent (client-message message-type) cevent)
             (ftype-ref XEvent (client-message format) cevent)
             (ftype-fields client-message cevent
                           ((data l 0)
                            (data l 1)
                            (data l 2)
                            (data l 3)
                            (data l 4)))))
           (ConfigureNotify
            (apply make-xconfigureevent
             (make-xany cevent)
             (ftype-fields xconfigure cevent (wid x y width height border-width above override-redirect))))
           (ConfigureRequest
            (apply make-xconfigurerequestevent
             (make-xany cevent)
             (ftype-fields xconfigurerequest cevent (wid x y width height border-width above detail value-mask))))
           (CreateNotify
            (apply make-xcreatewindowevent
             (make-xany cevent)
             (ftype-fields xcreatewindow cevent (wid x y width height border-width override-redirect))))
           (DestroyNotify
            (apply make-xdestroywindowevent
             (make-xany cevent)
             (ftype-fields xdestroywindow cevent (wid))))
           (KeyPressEvent
            (apply make-xkeyevent
             (make-xany cevent)
             (ftype-fields xkey cevent (root subwindow time x y x-root y-root state keycode same-screen))))
           (MapNotify
            (apply make-xmapevent
             (make-xany cevent)
             (ftype-fields xmap cevent (wid override-redirect))))
           (MapRequest
            (apply make-xmaprequestevent
             (make-xany cevent)
             (ftype-fields xmaprequest cevent (wid))))
           (PropertyNotify
            (apply make-xpropertyevent
             (make-xany cevent)
             (ftype-fields xproperty cevent (propatom time state))))
           (UnmapNotify
            (apply make-xunmapevent
             (make-xany cevent)
             (ftype-fields xunmap cevent (wid from-configure))))
           (else
            (make-xany cevent))))))

  (define make-xany
    (lambda (cevent)
      (apply make-xanyevent
       (ftype-fields xany cevent (type serial send-event d wid)))))

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
          (XConfigureWindow (current-display) wid change-mask &changes)
          #;(display (format "#x~x XConfigureWindow change-mask #b~b~n" wid change-mask))))))
)