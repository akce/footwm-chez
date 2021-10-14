;; Common Footwm wm operations. eg, Window, desktop, and layout etc
;;
;; Written by Jerry 2019-2021.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm wm)
  (export
   on-client-state
   add-window
   ;; Window operations.
   activate-window
   banish-window
   move-window-to-desktop
   remove-window
   window-name
   window-sort
   activate-window/index
   banish-window/index
   close-window/index
   activate-urgent
   run-or-raise-em
   ;; Desktop operations.
   desktop-activate
   desktop-insert
   desktop-delete
   desktop-rename
   get-desktop-id
   ;; Layout operations.
   calculate-workarea
   ideal-window-geometry
   draw-active-window
   arrange-windows
   ;; Misc
   assign-desktop
   )
  (import
   (rnrs)
   (only (chezscheme) add1 enumerate format sub1 system)
   (prefix (footwm ewmh) ewmh.)
   (prefix (footwm icccm) icccm.)
   (prefix (footwm util) util.)
   (footwm xlib))

  ;;;;;; Window operations.

  (define top-level-window? ewmh.user-selectable?)

  (define change-client-state-prop!
    (lambda (wid action prop)
      (define make-change!
        (lambda (a window-has-prop?)
          (cond
            [(and (eq? a 'REMOVE) window-has-prop?)
             ;; Special per/property processing can go here.
             (ewmh.del-net-wm-states-state! wid prop)]
            [(and (eq? a 'ADD) (not window-has-prop?))
             ;; Special per/property processing can go here.
             (ewmh.add-net-wm-states-state! wid prop)]
            [(eq? a 'TOGGLE)
             (make-change! (if window-has-prop? 'REMOVE 'ADD) prop)]
            [else
              (format #t "#x~x Bad _NET_WM_STATE action! ~a has-prop? ~a~n" wid a window-has-prop?)])))
      (format #t "#x~x _NET_WM_STATE ~a ~a(~a)~n" wid action (x-get-atom-name prop) prop)
      (make-change! action (ewmh.window-wm-state? wid prop))))

  ;; Make the _NET_WM_STATE property change to the client and have arrange-windows react to it.
  ;; eg, add/remove fullscreen property and have arrange-windows handle docks, workarea and visible window etc.
  (define on-client-state
    (lambda (wid ev)
      (let ([wso (ewmh.make-wm-state-change ev)])
        (change-client-state-prop! wid (ewmh.wm-state-change-action wso) (ewmh.wm-state-change-prop1 wso))
        (when (ewmh.wm-state-change-prop2 wso)
          (change-client-state-prop! wid (ewmh.wm-state-change-action wso) (ewmh.wm-state-change-prop2 wso)))
        (arrange-windows))))

  (define add-window
    (lambda (ev assignments)
      ;; Mainly EWMH house keeping.
      ;; Add/move window to top of client window list etc.
      ;; Set window desktop and set active window.
      ;; Adjust workarea with newly mapped dock apps.
      (let ([wid (xmaprequestevent-wid ev)])
        (let ([clients ewmh.client-list]
              [deskid (assign-desktop assignments wid)])
          (set! ewmh.client-list
            (cons
              wid
              (if (memq wid clients)
                (remove wid clients)
                clients)))
          (cond
            [(ewmh.dock-window? wid)
             (arrange-windows)
             ]
            [(eqv? deskid ewmh.current-desktop)
             (ewmh.window-desktop-set! wid deskid)
             (activate-window wid)]
            [else
              (ewmh.window-desktop-set! wid deskid)
              (desktop-activate deskid)])))))

  (define activate-window
    (lambda (wid)
      ;; promote window to top of ewmh.client-list, set as ewmh.active-window.
      (if (top-level-window? wid)
          (unless (eqv? wid ewmh.active-window)
            (set! ewmh.client-list (cons wid (remove wid ewmh.client-list)))
            (if (eqv? (ewmh.window-desktop wid) ewmh.current-desktop)
                (arrange-windows)
                (desktop-activate (ewmh.window-desktop wid)))))))

  (define banish-window
    (lambda (wid)
      ;; This wm banishes a window by iconifying and moving to the bottom of ewmh.client-list.
      (when (top-level-window? wid)
        (set! ewmh.client-list (append (remove wid ewmh.client-list) (list wid)))
        (when (eqv? wid ewmh.active-window)
          (arrange-windows)))))

  (define move-window-to-desktop
    (lambda (wid index)
      (when (and
              (top-level-window? wid)
              (< index ewmh.desktop-count)
              (not (= index (ewmh.window-desktop wid))))
        (ewmh.window-desktop-set! wid index)
        (when (eqv? (icccm.get-wm-state wid) icccm.NormalState)
          (arrange-windows)))))

  (define remove-window
    (lambda (wid)
      (ewmh.remove-window wid)
      (cond
        [(or (eqv? wid ewmh.active-window) (ewmh.dock-window? wid))
         (arrange-windows)]
        [else
          ;; XXX only do this if the window was visible.
          (calculate-workarea)
          (draw-active-window ewmh.active-window)])))

  ;; Retrieve EWMH _NET_WM_NAME or fallback to ICCCM WM_NAME. #f if neither exist.
  (define window-name
    (lambda (wid)
      (let ([ename (ewmh.window-name wid)])
        (if ename
            ename
            (icccm.name wid)))))

  ;;;; Window sorting.

  (define-record-type winfo
    (fields wid desktop stack))

  (define make-winfo-list
    (lambda (wids)
      ;; Filter winfo-desktop removes stale windows. It's an edge case, but it can happen that a
      ;; window is closed just as we've been called. Such windows will have desktop = #f.
      (filter
        winfo-desktop
        (map
         (lambda (wid i)
           (make-winfo wid (ewmh.window-desktop wid) i))
         wids (enumerate wids)))))

  ;; Order by desktop, followed by order in stack.
  (define window>?
    (lambda (win-l win-r)
      (cond
       [(eqv? (winfo-desktop win-l) (winfo-desktop win-r))
        (< (winfo-stack win-l) (winfo-stack win-r))]
       [else
        (< (winfo-desktop win-l) (winfo-desktop win-r))])))

  (define sorted-winfo
    (lambda (wids)
      (list-sort window>? (make-winfo-list wids))))

  (define window-sort
    (lambda (wids)
      ;; return only a list of wids. Less efficient than returning records but it keeps the interface clean.
      (map winfo-wid (sorted-winfo wids))))

  ;;;; Client window operations.

  (define window-op/index
    (lambda (op index)
      (let* ([wlist (sorted-winfo (filter ewmh.user-selectable? ewmh.client-list))]
             [d ewmh.current-desktop]
             [dlist
              (filter (lambda (w)
                        (eqv? d (winfo-desktop w))) wlist)])
        (when (< index (length dlist))
          (op (winfo-wid (list-ref dlist index)))))))

  ;; Select window to activate by position in list.
  (define activate-window/index
    (lambda (index)
      (window-op/index ewmh.window-active-request! index)))

  (define banish-window/index
    (lambda (index)
      (window-op/index banish-window index)))

  (define close-window/index
    (lambda (index)
      (window-op/index ewmh.window-close-request! index)))

  (define activate-urgent
    (lambda ()
      (let ([wids
              (filter
                (lambda (wid)
                  (or
                    (icccm.wm-hints-urgency (icccm.get-wm-hints wid))
                    (ewmh.demands-attention? wid)))
                ewmh.client-list)])
        (unless (null? wids)
          (activate-window (car wids))
           (x-sync)))))

  (define run-or-raise-em
    (lambda (instance command)
      (let loop ([wids ewmh.client-list])
        (cond
          [(null? wids)
           (system (string-append "exec " command))]
          [(window-instance? (car wids) instance)
            (activate-window (car wids))
            (x-sync)]
          [else
            (loop (cdr wids))]))))

  (define window-instance?
    (lambda (wid instance)
      (string-ci=? instance (icccm.class-hint-instance (icccm.class-hint wid)))))

  ;;;;;; Desktop operations.

  (define adjust-windows-desktop
    (lambda (pos action)
      (for-each
       (lambda (wid)
         (let ([d (ewmh.window-desktop wid)])
           (if (and d (>= d pos))
               (ewmh.window-desktop-set! wid (action d)))))
       (filter ewmh.user-selectable? ewmh.client-list))))

  (define desktop-activate
    (lambda (index)
      (when (and index (< index ewmh.desktop-count))
        (let ([c ewmh.current-desktop])
          (unless (= index c)
            (for-each
             (lambda (wid)
               (let ([wd (ewmh.window-desktop wid)])
                 (if wd
                   (cond
                    [(= wd index)
                     (ewmh.window-desktop-set! wid c)]
                    [(< wd index)
                     (ewmh.window-desktop-set! wid (add1 wd))]))
                  #| else ignore, only windows at or below index need adjustment.|#))
             (filter ewmh.user-selectable? ewmh.client-list))
            (let* ([names ewmh.desktop-names]
                   [name (list-ref names index)])
              (set! ewmh.desktop-names (util.list-insert (remove name names) name c)))
            (arrange-windows))))))

  (define desktop-insert
    (lambda (name index)
      (let ([names ewmh.desktop-names])
        ;; desktop names must be unique.
        (unless (member name names)
          (set! ewmh.desktop-names (util.list-insert names name index))
          (adjust-windows-desktop index add1)
          (set! ewmh.desktop-count (add1 (length names)))
          (if (= index ewmh.current-desktop)
              (arrange-windows))))))

  (define merge-desktop-to
    (lambda (old-desktop-id new-desktop-id)
      ;; Move orphaned windows to the destination desktop.
      (for-each
        (lambda (wid)
          (when (= old-desktop-id (ewmh.window-desktop wid))
            (ewmh.window-desktop-set! wid new-desktop-id)))
        (filter ewmh.user-selectable? ewmh.client-list))
      ;; Adjust window desktops at old-desktop-id and higher downwards.
      (adjust-windows-desktop old-desktop-id sub1)
      (delete-desktop-hints old-desktop-id)
      ;; Redraw if one of the affected desktops was visible.
      (when (or (= ewmh.current-desktop old-desktop-id)
                (= ewmh.current-desktop new-desktop-id))
        (arrange-windows))))

  (define delete-desktop-hints
    (lambda (delete-index)
      ;; Update desktop ewmh hints.
      (let ([names ewmh.desktop-names])
        (set! ewmh.desktop-names (remove (list-ref names delete-index) names))
        (set! ewmh.desktop-count (sub1 (length names))))))

  (define desktop-delete
    (lambda (delete-index)
      (let ([c ewmh.desktop-count])
        (cond
          [(or (>= delete-index c)		; sanity check for a valid index.
               (< delete-index 0))
           (if #f #f)]
          [(= c 1)
           ;; Special case. There's only one desktop.
           ;; Rather than delete, just rename it to Default.
           (desktop-rename 0 "Default")]
          [(= (+ delete-index 1) c)
           ;; Deleting the last desktop results in merging to previous desktop in line.
           (merge-desktop-to delete-index (- delete-index 1))]
          [else
            ;; Otherwise merge to following desktop.
            (merge-desktop-to delete-index (+ delete-index 1))]))))

  (define desktop-rename
    (lambda (index name)
      (when (< index ewmh.desktop-count)
        (set! ewmh.desktop-names (util.list-replace ewmh.desktop-names index name)))))

  (define get-desktop-id
    (lambda (number-or-name)
      (cond
        [(string->number number-or-name)
         => values]
        [else
          (util.list-find-index
            (lambda (d)
              (string-ci=? d number-or-name))
            ewmh.desktop-names)])))

 ;;;;;; Layout operations.

  (define calculate-workarea
    (lambda ()
      (let ([wids (filter (lambda (w)
                            (and (ewmh.dock-window? w) (icccm.window-normal? w))) ewmh.client-list)])
        (ewmh.calculate-workarea wids))))

  (define ideal-window-geometry
    (lambda (wid)
      (cond
        [(ewmh.fullscreen-window? wid)
         (ewmh.desktop-geometry)]
        [else
          (ewmh.workarea-geometry)])))

  (define show-window
    (lambda (wid)
      (ewmh.showing-desktop #f)
      (set! ewmh.active-window wid)
      (show-dock-window wid)))

  (define show-dock-window
    (lambda (d)
      (ewmh.show-window d)
      (icccm.show-window d)))

  (define iconify-window
    (lambda (wid)
      (ewmh.iconify-window wid)
      (icccm.iconify-window wid)))

  (define draw-active-window
    (lambda (wid)
      (let ([normal-hints (icccm.get-normal-hints wid)])
        (format #t "#x~x WMHints ~a ~a~n" wid (icccm.normal-hints-flags->string normal-hints) normal-hints)
        (x-configure-window wid (icccm.apply-normal-hints normal-hints (ideal-window-geometry wid)))
        ;; Footwm won't arrange any overlapping windows so lower the active window in the
        ;; stack list so that any override-redirect (eg, tooltips etc) from strut windows
        ;; will be visible.
        ;; (As is the case with tint2. My guess is it's reusing its tooltip window so
        ;; newer footwm windows would obscure tooltips unless we lower our window.)
        (x-lower-window wid)
        (show-window wid))))

  (define show-desktop
    (lambda ()
      ;; set input focus to root (so keygrabbers still function) and clear ewmh.
      (icccm.focus-root)
      (ewmh.showing-desktop #t)
      (set! ewmh.active-window None)))

  (define arrange-windows
    (lambda ()
      (let*-values
        ([(d) ewmh.current-desktop]
         ;; split client windows into dock windows and user windows.
         [(docks uws) (partition ewmh.dock-window? ewmh.client-list)]
         ;; split user windows into those on the current desktop, and those on other desktops.
         [(cdws odws) (partition (lambda (w) (eq? d (ewmh.window-desktop w))) uws)])
        (for-each iconify-window odws)	; should do this only on wm-init and desktop change..
        (cond
          [(null? cdws)	; no window to show in current desktop.
           (for-each show-dock-window docks)
           (show-desktop)]
          [else
            (let ([wid (car cdws)])		; visible window
               ;; iconify hidden windows
               (for-each iconify-window (cdr cdws))
               (cond
                 [(ewmh.fullscreen-window? wid)
                  (for-each iconify-window docks)]
                 [else
                   (for-each show-dock-window docks)])
               (calculate-workarea)
               (draw-active-window wid))]))))

  ;;; Misc

  (define rule-compare car)
  (define rule-column cadr)
  (define rule-value caddr)
  (define rule-desktop cadddr)

  (define match-assign?
    (lambda (a wid)
      (case (rule-column a)
        [(instance)
         (window-instance? wid (rule-value a))]
        [else
          #f])))

  ;; assign a desktop for the window.
  (define assign-desktop
    (case-lambda
      [(assignments wid)
       (assign-desktop assignments wid ewmh.current-desktop)]
      [(assignments wid default-desktop)
       ;; Ultra paranoid check. default-desktop must be a number within desktop count!
       (unless (and (fixnum? default-desktop) (fx>=? default-desktop 0) (fx<? default-desktop ewmh.desktop-count))
         (set! default-desktop 0))
       (cond
         [(find
            (lambda (a)
              (match-assign? a wid))
            assignments)
          => (lambda (a)
               ;; Fallback to the default desktop if the desired desktop doesn't exist.
               ;; Perhaps we should create the desktop instead of the fallback?
               (or (get-desktop-id (rule-desktop a))
                   default-desktop))]
         [else
           default-desktop])]))
  )
