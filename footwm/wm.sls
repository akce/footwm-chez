;; Common Footwm wm operations. eg, Window, desktop, and layout etc
;;
;; Written by Akce 2019-2020.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm wm)
  (export
   manage-window?
   ;; Window operations.
   activate-window
   banish-window
   move-window-to-desktop
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
   get-unassigned
   desktop-delete
   desktop-rename
   ;; Layout operations.
   ideal-window-geometry
   draw-active-window
   arrange-windows)
  (import
   (rnrs)
   (only (chezscheme) add1 enumerate format sub1 system)
   (prefix (footwm ewmh) ewmh.)
   (prefix (footwm icccm) icccm.)
   (prefix (footwm util) util.)
   (footwm xlib))

  ;;;;;; Window operations.

  (define top-level-window?
    (lambda (wid)
      (memq wid ewmh.client-list)))

  (define manage-window?
    (lambda (wid)
      (and (icccm.manage-window? wid)
          (ewmh.show-in-taskbar? wid))))

  (define activate-window
    (lambda (wid)
      ;; promote window to top of ewmh.client-list, set as ewmh.active-window.
      (if (top-level-window? wid)
          (unless (= wid ewmh.active-window)
            (set! ewmh.client-list (cons wid (remove wid ewmh.client-list)))
            (if (= (ewmh.window-desktop wid) ewmh.current-desktop)
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
      (when (top-level-window? wid)
        (when (< index ewmh.desktop-count)
          (unless (= index (ewmh.window-desktop wid))
            (ewmh.window-desktop-set! wid index)
            (if (eqv? (icccm.get-wm-state wid) icccm.NormalState)
                (arrange-windows)))))))

  ;; Retrieve EWMH _NET_WM_NAME or fallback to ICCCM WM_NAME. #f if neither exist.
  (define window-name
    (lambda (wid)
      (let ([ename (ewmh.name wid)])
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
      (let* ([wlist (sorted-winfo ewmh.client-list)]
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
          [(string-ci=? instance (icccm.class-hint-instance (icccm.class-hint (car wids))))
            (activate-window (car wids))
            (x-sync)]
          [else
            (loop (cdr wids))]))))

  ;;;;;; Desktop operations.

  (define adjust-windows-desktop
    (lambda (pos action)
      (for-each
       (lambda (wid)
         (let ([d (ewmh.window-desktop wid)])
           (if (and d (>= d pos))
               (ewmh.window-desktop-set! wid (action d)))))
       ewmh.client-list)))

  (define desktop-activate
    (lambda (index)
      (when (< index ewmh.desktop-count)
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
             ewmh.client-list)
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

  (define get-unassigned
    (lambda (names)
      (util.list-find-index
       (lambda (x)
         (string=? "Unassigned" x))
       names)))

  (define desktop-delete
    (lambda (index)
      (let ([c ewmh.desktop-count])
        (if (< index c)
          (let* ([names ewmh.desktop-names]
                 [unassigned (get-unassigned names)])
            (when unassigned
              ;; Move orphaned windows to the unassigned desktop.
              (for-each
               (lambda (wid)
                 (if (= index (ewmh.window-desktop wid))
                   (ewmh.window-desktop-set! wid unassigned)))
               ewmh.client-list)
              ;; Adjust window desktops at index and higher downwards.
              (adjust-windows-desktop index sub1)
              ;; Update desktop ewmh hints.
              (set! ewmh.desktop-names (remove (list-ref names index) names))
              (set! ewmh.desktop-count (sub1 (length names)))
              ;; Redraw if deleted desktop was the displayed desktop.
              (if (= index ewmh.current-desktop)
                (arrange-windows))))))))

  (define desktop-rename
    (lambda (index name)
      (let ([c ewmh.desktop-count])
        (when (< index c)
          (let ([names ewmh.desktop-names])
            (unless (string=? "Unassigned" (list-ref names index))
              (set! ewmh.desktop-names (util.list-replace names index name))))))))

 ;;;;;; Layout operations.

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
      (ewmh.show-window wid)
      (icccm.show-window wid)))

  (define iconify-window
    (lambda (wid)
      (ewmh.iconify-window wid)
      (icccm.iconify-window wid)))

  (define draw-active-window
    (lambda (wid)
      (x-configure-window wid (icccm.apply-normal-hints (icccm.get-normal-hints wid) (ideal-window-geometry wid)))
      ;; Footwm won't arrange any overlapping windows so lower the active window in the
      ;; stack list so that any override-redirect (eg, tooltips etc) from strut windows
      ;; will be visible.
      ;; (As is the case with tint2. My guess is it's reusing its tooltip window so
      ;; newer footwm windows would obscure tooltips unless we lower our window.)
      (cond
        [(ewmh.fullscreen-window? wid)
         ;; This is a work-around to stop fullscreen windows being obstructed by strut windows.
         ;; TODO: redo the way strut windows are (not) managed by this wm.
         (x-raise-window wid)]
        [else
          (x-lower-window wid)])
      (show-window wid)))

  (define show-desktop
    (lambda ()
      ;; set input focus to root (so keygrabbers still function) and clear ewmh.
      (icccm.focus-root)
      (ewmh.showing-desktop #t)
      (set! ewmh.active-window None)))

  (define arrange-windows
    (lambda ()
      (let ([d ewmh.current-desktop])
        (let-values ([(ws ows) (partition (lambda (w) (eq? d (ewmh.window-desktop w))) ewmh.client-list)])
          (for-each iconify-window ows)	; should do this only on wm-init and desktop change..
          (if (null? ws)
            (show-desktop)	; no window to show:
            (let ([vis (car ws)]		; visible window
                  [hs (cdr ws)])		; hidden windows
              (for-each iconify-window hs)
              (unless (eq? vis ewmh.active-window)
                (draw-active-window vis)))))))))
