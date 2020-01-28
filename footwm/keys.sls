(library (footwm keys)
  (export
   alt
   caps-lock
   ctrl
   num-lock
   scroll-lock

   key-actions
   key-config
   main)
  (import
   (rnrs)
   (only (chezscheme)
         copy-environment eval format load logior make-parameter scheme-environment set-top-level-value!)
   (only (footwm util) case-equal? list-combinations* remove*)
   (footwm xlib))

  ;;; Set default modifier keys.
  (define alt
    (make-parameter Mod1))

  (define caps-lock
    (make-parameter Lock))

  (define ctrl
    (make-parameter Control))

  (define num-lock
    (make-parameter Mod2))

  (define scroll-lock
    (make-parameter Mod3))

  (define global-required
    (make-parameter (list)))

  (define global-ignored
    (make-parameter (list (caps-lock) (scroll-lock))))

  (define key-actions
    (make-parameter (list)))

  (define global-modifiers-set!
    (lambda (required ignored)
      ;; TODO validate before setting globals.
      (global-required required)
      (global-ignored ignored)))

  (define grab-key
    (lambda (keycode modifier-mask)
      (x-grab-key keycode modifier-mask (root) #f GrabModeAsync GrabModeAsync)))

  (define make-mod-combos
    (lambda (key-required-mods)
      ;; Join the global-required list with the key-required-mods, and join with the list of ignored mod combos.
      (let ([req (append key-required-mods (global-required))])
        (map
         (lambda (is)
           (append req is))
         (list-combinations* (global-ignored))))))

  (define ungrab-keys
    (lambda ()
      (x-ungrab-key AnyKey AnyModifier (root))))

  (define keystrsym->keycode
    (lambda (keystrsym)
      (let ([keysym (x-string-to-keysym (symbol->string keystrsym))])
        (if (eq? keysym NoSymbol)
            #f
            (let ([keycode (x-keysym-to-keycode keysym)])
              (if (eq? keycode 0)
                  #f
                  keycode))))))

  (define list->bitmap
    (lambda (lst)
      (fold-left logior 0 lst)))

  (define watch-key
    (lambda (keystrsym modifiers action)
      (let ([key-code (keystrsym->keycode keystrsym)])
        (if key-code
          (map
           (lambda (mods)
             (let ([modmap (list->bitmap mods)])
               (display (format "grabbing: ~a ~a~n" key-code mods))
               (grab-key key-code modmap)
               (cons (cons key-code modmap) action)))
           (make-mod-combos modifiers))
          '()))))

  (define-syntax key-config
    (syntax-rules ()
      [(_ (key-code modifiers action actionn ...) ...)
       (key-actions (append (watch-key key-code modifiers (lambda () action actionn ...)) ...))]))

  (define main
    (lambda (config-file)
      (x-set-error-handler)
      (x-select-input (root) KeyPress)
      (load-config config-file)
      (run)))

  (define load-config
    (lambda (config-file)
      (let ([env (copy-environment (scheme-environment))])
        (set-top-level-value! 'alt alt env)
        (set-top-level-value! 'caps-lock caps-lock env)
        (set-top-level-value! 'num-lock num-lock env)
        (set-top-level-value! 'scroll-lock scroll-lock env)
        (set-top-level-value! 'global-modifiers-set! global-modifiers-set! env)
        (set-top-level-value! 'watch-key watch-key env)
        ;; TODO try load-compiled-from-port/compile-to-port to return the last expression from compiled file.
        ;; TODO could replace the load/key-actions combo.
        (load config-file
              (lambda (x)
                (eval x env))))))

  (define run
    (lambda ()
      (let loop ()
        (let ([ev (x-next-event)])
          (cond
           ((xkeyevent? ev)		(on-key ev))
           (else
            (let ([eid (xanyevent-type ev)])
              (case-equal? eid
               [KeyPressEvent (display "Ignore KeyPressEvent\n")]
               [KeyReleaseEvent (display "Ignore KeyReleaseEvent\n")]
               [else
                (display (format "Unsupported event ~d~n" eid))])))))
        (loop))))

  (define on-key
    (lambda (ev)
      (display (format "#x~x Key event state #b~b~n" (xanyevent-wid (xkeyevent-xany ev)) (xkeyevent-state ev)))
      (let* ([key (cons (xkeyevent-keycode ev) (xkeyevent-state ev))]
             [action (assoc key (key-actions))])
        (when action
            ((cdr action)))))))
