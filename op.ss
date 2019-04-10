;; Window, desktop, and layout operations.
(library (op)
  (export
   ;; Window operations.
   activate-window
   banish-window
   move-window-to-desktop
   window-name
   window-sort
   ;; Desktop operations.
   desktop-activate
   desktop-insert
   get-unassigned
   desktop-delete
   desktop-rename
   ;; Layout operations.
   arrange-windows)
  (import
   (rnrs)
   (only (chezscheme) add1 enumerate format sub1)
   (globals)
   (prefix (ewmh) ewmh.)
   (prefix (icccm) icccm.)
   (prefix (util) util.)
   (xlib)
   (prefix (xutil) xutil.))

  ;;;;;; Window operations.

  (define top-level-window?
    (lambda (wid)
      (memq wid (ewmh.client-list))))

  (define activate-window
    (lambda (wid)
      ;; promote window to top of ewmh.client-list-stacking, set as ewmh.active-window.
      (if (top-level-window? wid)
          (let ([rstack (reverse (ewmh.client-list-stacking))])
            (unless (= wid (car rstack))
              (ewmh.client-list-stacking-set! (reverse (cons wid (remove wid rstack))))
              (arrange-windows))))))

  (define banish-window
    (lambda (wid)
      ;; This wm banishes a window by iconifying and moving to the bottom of ewmh.client-list-stacking.
      (if (top-level-window? wid)
          (let ([state (icccm.get-wm-state wid)])
            (ewmh.client-list-stacking-set! (append (list wid) (remove wid (ewmh.client-list-stacking))))
            (if (eq? state 'NORMAL)
                ;; Normal means the window is visible, hide and re-arrange desktop.
                (begin
                  (icccm.iconify-window wid)
                  (arrange-windows)))))))

  (define move-window-to-desktop
    (lambda (wid index)
      (when (< index (ewmh.desktop-count))
        (unless (= index (ewmh.window-desktop wid))
          (ewmh.window-desktop-set! wid index)
          (if (eq? (icccm.get-wm-state wid) 'NORMAL)
              (arrange-windows))))))

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
      (map
       (lambda (wid i)
         (make-winfo wid (ewmh.window-desktop wid) i))
       wids (enumerate wids))))

  ;; Order by desktop, followed by order in stack.
  (define window>?
    (lambda (win-l win-r)
      (cond
       [(eq? (winfo-desktop win-l) (winfo-desktop win-r))
        (< (winfo-stack win-l) (winfo-stack win-r))]
       [else
        (> (winfo-desktop win-l) (winfo-desktop win-r))])))

  (define window-sort
    (lambda (wids)
      (map winfo-wid (list-sort window>? (make-winfo-list wids)))))

  ;;;;;; Desktop operations.

  (define adjust-windows-desktop
    (lambda (pos action)
      (for-each
       (lambda (wid)
         (let ([d (ewmh.window-desktop wid)])
           (if (>= d pos)
               (ewmh.window-desktop-set! wid (action d)))))
       (ewmh.client-list-stacking))))

  (define desktop-activate
    (lambda (index)
      (when (< index (ewmh.desktop-count))
        (let ([c (ewmh.current-desktop)])
          (unless (= index c)
            (for-each
             (lambda (wid)
               (let ([wd (ewmh.window-desktop wid)])
                 (cond
                  [(= wd index)
                   (ewmh.window-desktop-set! wid c)]
                  [(< wd index)
                   (ewmh.window-desktop-set! wid (add1 wd))])
                  #| else ignore, only windows at or below index need adjustment.|#))
             (ewmh.client-list-stacking))
            (let* ([names (ewmh.desktop-names)]
                   [name (list-ref names index)])
              (ewmh.desktop-names-set! (util.list-insert (remove name names) name c)))
            (arrange-windows))))))

  (define desktop-insert
    (lambda (name index)
      (let ([names (ewmh.desktop-names)])
        ;; desktop names must be unique.
        (unless (member name names)
          (ewmh.desktop-names-set! (util.list-insert names name index))
          (adjust-windows-desktop index add1)
          (ewmh.desktop-count-set! (add1 (length names)))
          (if (= index (ewmh.current-desktop))
              (arrange-windows))))))

  (define get-unassigned
    (lambda (names)
      (util.list-find-index
       (lambda (x)
         (string=? "Unassigned" x))
       names)))

  (define desktop-delete
    (lambda (index)
      (let ([c (ewmh.desktop-count)])
        (if (< index c)
          (let* ([names (ewmh.desktop-names)]
                 [unassigned (get-unassigned names)])
            (when unassigned
              ;; Move orphaned windows to the unassigned desktop.
              (for-each
               (lambda (wid)
                 (if (= index (ewmh.window-desktop wid))
                   (ewmh.window-desktop-set! wid unassigned)))
               (ewmh.client-list-stacking))
              ;; Adjust window desktops at index and higher downwards.
              (adjust-windows-desktop index sub1)
              ;; Update desktop ewmh hints.
              (ewmh.desktop-names-set! (remove (list-ref names index) names))
              (ewmh.desktop-count-set! (sub1 (length names)))
              ;; Redraw if deleted desktop was the displayed desktop.
              (if (= index (ewmh.current-desktop))
                (arrange-windows))))))))

  (define desktop-rename
    (lambda (index name)
      (let ([c (ewmh.desktop-count)])
        (when (< index c)
          (let ([names (ewmh.desktop-names)])
            (unless (string=? "Unassigned" (list-ref names index))
              (ewmh.desktop-names-set! (util.list-replace names index name))))))))

 ;;;;;; Layout operations.

  (define arrange-window
    (lambda (wid)
      ;; give it the same dimensions as the root window.
      (let ([rgeom (xutil.window-attributes-geom (xutil.get-window-attributes (root)))])
        (display (format "#x~x arrange active window ~a~n" wid rgeom))
        (xutil.resize-window wid (xutil.geometry-x rgeom) (xutil.geometry-y rgeom) (xutil.geometry-width rgeom) (xutil.geometry-height rgeom)))))

  (define arrange-windows
    (lambda ()
      ;; client-list-stacking is bottom to top, reverse so we can easily get to the first window later.
      (let ([aws (reverse (ewmh.client-list-stacking))]
            [d (ewmh.current-desktop)])
        (let-values ([(ws ows) (partition (lambda (w) (eq? d (ewmh.window-desktop w))) aws)])
          (for-each icccm.iconify-window ows)	; should do this only on wm-init and desktop change..
          (unless (null? ws)
            (let ([vis (car ws)]		; visible window
                  [hs (cdr ws)])		; hidden windows
              (for-each icccm.iconify-window hs)
              (icccm.show-window vis)
              (arrange-window vis)
              (icccm.focus-window vis)
              (ewmh.active-window-set! vis))))))))