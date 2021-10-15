;; Common Footwm wm operations. eg, Window, desktop, and layout etc
;;
;; Written by Jerry 2019-2021.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm wm)
  (export
   on-client-state
   on-activate-window
   on-configure-request
   on-map-request
   ;; Window operations.
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
        ;; TODO arrange-windows is brute force, try and refine so minimal changes to X server are made.
        (arrange-windows))))

  ;; Called in response to a _NET_ACTIVE_WINDOW
  ;; This function does a sanity check on 'wid' before calling activate-window.
  (define on-activate-window
    (lambda (wid)
      (let-values ([(docks cdws odws) (categorise-client-windows)])
        ;; Allow remote activation of current desktop windows or other desktop windows only.
        (cond
          [(or (memq wid cdws) (memq wid odws))
           (activate-window wid)]
          [else
            (format #t "#x~x on-activate-window ignoring request~n" wid)]))))

  (define on-configure-request
    (lambda (wid ev)
      ;; TODO check that the ConfigureRequest isn't a stack-order-only change.
      (cond
        [(ewmh.dock-window? wid)
         ;; Blindly honour dock window changes as they could be for auto-hide panels etc.
         (icccm.on-configure-request ev)
         (arrange-windows)]
        [else
          ;; ideal also accounts for fullscreen apps.
          (let ([ideal (ideal-window-geometry wid)])
            (unless (geometry=? ideal (xconfigurerequestevent-geometry ev))
              (x-configure-window wid ideal)))])))

  ;; Mainly EWMH house keeping.
  ;; Add/move window to top of client window list etc.
  ;; Set window desktop and set active window.
  ;; Adjust workarea with newly mapped dock apps.
  (define on-map-request
    (lambda (wid assignments)
      (cond
        [(ewmh.dock-window? wid)
         ;; TODO moving wid to top of client-list is done in two places: on-map-request & activate-window.
         (set! ewmh.client-list (cons wid (remove wid ewmh.client-list)))
         (arrange-windows)]
        [else
          (ewmh.window-desktop-set! wid (assign-desktop assignments wid))
          (ewmh.window-frame-extents-set! wid)
          (ewmh.window-allowed-actions-set! wid)
          (activate-window wid)])))

  ;; Show a window, activating it's desktop if necessary.
  ;; Notes:
  ;; - wid is assumed to be a valid client window value, eg, not a dock window.
  ;; - wid will be added to ewmh.client-list if it is not already there.
  ;; - wid that is already the ewmh.active-window is ignored.
  (define activate-window
    (lambda (wid)
      ;; promote window to top of ewmh.client-list, set as ewmh.active-window.
      (unless (eqv? wid ewmh.active-window)
        ;; TODO moving wid to top of client-list is done in two places: on-map-request & activate-window.
        (set! ewmh.client-list (cons wid (remove wid ewmh.client-list)))
        (cond
          [(eqv? (ewmh.window-desktop wid) ewmh.current-desktop)
           (arrange-windows)]
          [else
            (desktop-activate (ewmh.window-desktop wid))]))))

  ;; This wm banishes a window by iconifying and moving to the bottom of ewmh.client-list.
  ;; Banishment is only allowed for regular client windows in the current desktop and only when
  ;; there's another window that can take its place.
  ;; ie, do not allow an empty desktop view as banishing a window on a desktop with only one window
  ;; would break the activate window-by-index-1-or-greater model.
  (define banish-window
    (lambda (wid)
      (let-values ([(docks cdws odws) (categorise-client-windows)])
        (cond
          [(and (fx>? (length cdws) 1) (memq wid cdws))
           ;; move to bottom of client-list.
           (set! ewmh.client-list (append (remove wid ewmh.client-list) (list wid)))
           ;; activate new top client window.
           (activate-window (car cdws))]
          [(memq wid odws)
           ;; banishing a window that's a member of another desktop merely puts it at the bottom of the client list.
           (set! ewmh.client-list (append (remove wid ewmh.client-list) (list wid)))]
          [else
            (format #t "#x~x banish-window ignoring request '~a'~n" wid (window-name wid))]))))

  (define move-window-to-desktop
    (lambda (wid deskid)
      (when (and
              (ewmh.user-selectable? wid)
              (fx<? deskid ewmh.desktop-count)
              (fx>=? deskid 0)
              (not (fx=? deskid (ewmh.window-desktop wid))))
        (ewmh.window-desktop-set! wid deskid)
        (cond
          [(fx=? deskid ewmh.current-desktop)
           ;; Window has been moved to the current desktop.
           ;; Activate the window if it happens to be the new top in the desktop stack.
           (let-values ([(docks cdws odws) (categorise-client-windows)])
             ;; Note: car is safe to call as cdws must have at least one entry in it: wid.
             (when (eqv? wid (car cdws))
               (activate-window wid)))]
          [(fx=? wid ewmh.active-window)
           ;; wid was active but moved to a desktop that's not visible; find the new top window and show it.
           (select-active-window)]))))

  ;; Selects a new active-window from the current desktop or show the (empty) desktop if there's none available.
  ;; This is a lighter-weight version of arrange-windows and usually only needed when ewmh.active-window has changed.
  ;; ie, call arrange-windows if there's a change to a desktop or in a dock window (either added, deleted, or resized)
  ;; requiring a workarea recalculation.
  (define select-active-window
    (lambda ()
      (let-values ([(docks cdws odws) (categorise-client-windows)])
        (cond
          [(null? cdws)
           (show-desktop)]
          [else
            (activate-window (car cdws))]))))

  (define remove-window
    (lambda (wid)
      (ewmh.remove-window wid)
      (cond
        [(ewmh.user-selectable? wid)
         ;; Find the next window ond show it, or desktop if there's none left.
         (select-active-window)]
        [else
          ;; other windows like dock windows, will need a full recalculation of the desktop.
          (arrange-windows)])))

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

  ;; NOTE: by using a most-recently-used (MRU) model, ewmh.current-desktop MUST always equal 0.
  ;; desktop-activate sets all windows belonging to deskid to desktop 0 and shuffles up ewmh.window-desktop
  ;; values for windows that previously belonged to desktops 0 => deskid.
  (define desktop-activate
    (lambda (deskid)
      (when (and deskid (< deskid ewmh.desktop-count))
        (let ([c ewmh.current-desktop])
          (unless (= deskid c)
            (for-each
             (lambda (wid)
               (let ([wd (ewmh.window-desktop wid)])
                 (if wd
                   (cond
                    [(= wd deskid)
                     (ewmh.window-desktop-set! wid c)]
                    [(< wd deskid)
                     (ewmh.window-desktop-set! wid (add1 wd))]))
                  #| else ignore, only windows at or below deskid need adjustment.|#))
             (filter ewmh.user-selectable? ewmh.client-list))
            (let* ([names ewmh.desktop-names]
                   [name (list-ref names deskid)])
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
          (when (= index ewmh.current-desktop)
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
      (let ([normal-hints (icccm.get-normal-hints wid)]
            [wa (x-get-window-attributes wid)]
            [ideal (ideal-window-geometry wid)])
        #;(format #t "#x~x WMHints ~a ~a~n" wid (icccm.normal-hints-flags->string normal-hints) normal-hints)
        ;; Make sure to try and resize here because not all client apps try to size themselves at creation time.
        ;; ie, not all initial MapRequests include a ConfigureRequest.
        (cond
          ;; Some SDL apps get here without window attributes (wa), so make sure we have one before trying to use.
          ;; An alternate solution seems to be to enable x-grab-server in xlib:x-with-next-event but Xlib
          ;; docs *highly* recommend not to use grabs.
          [wa
            ;; Only resize if window has a different geometry than what we want.
            ;; TODO write a fuzzy-geom=? as ev-geom and win-geom might not be exact because of hard resize increments.
            (unless (geometry=? (window-attributes-geom wa) ideal)
              (format #t "#x~x resizing non-ideal window geom ~a -> ~a~n" wid (window-attributes-geom wa) ideal)
              (x-configure-window wid (icccm.apply-normal-hints normal-hints ideal)))]
          [else
            (x-configure-window wid (icccm.apply-normal-hints normal-hints ideal))])
        ;; Footwm won't arrange any overlapping windows so move the active window to the bottom of the
        ;; stack list so that any override-redirect windows (eg, menu popups, tooltips, etc) will be visible.
        (x-lower-window wid)
        (ewmh.showing-desktop #f)
        (set! ewmh.active-window wid)
        (show-dock-window wid))))

  (define show-desktop
    (lambda ()
      ;; set input focus to root (so keygrabbers still function) and clear ewmh.
      (icccm.focus-root)
      (ewmh.showing-desktop #t)
      (set! ewmh.active-window None)))

  ;; divides ewmh.client-list into 3 categories (returned as 'values'):
  ;; - dock windows
  ;; - windows belonging to the current desktop (ewmh.current-desktop)
  ;; - windows belonging to all other desktops.
  (define categorise-client-windows
    (lambda ()
      (let*-values
        ([(d) ewmh.current-desktop]
         ;; split client windows into dock windows and user windows.
         [(docks uws) (partition ewmh.dock-window? ewmh.client-list)]
         ;; split user windows into those on the current desktop, and those on other desktops.
         [(cdws odws) (partition (lambda (w) (eq? d (ewmh.window-desktop w))) uws)])
        (values docks cdws odws))))

  ;; arrange-windows is a full recalc and redraw for the current desktop.
  ;; It will minimise/show user selectable windows based on current desktop.
  ;; It will minimise/show dock windows depending on state of ewmh.active-window.
  ;; It will set ewmh.active-window (via draw-active-window) or show-desktop if there's no window available.
  (define arrange-windows
    (lambda ()
      ;; split user windows into those on the current desktop, and those on other desktops.
      (let-values ([(docks cdws odws) (categorise-client-windows)])
        (for-each iconify-window odws)	; should do this only on wm-init and desktop change..
        (cond
          [(null? cdws)	; no window to show in current desktop.
           (for-each show-dock-window docks)
           (calculate-workarea)
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
