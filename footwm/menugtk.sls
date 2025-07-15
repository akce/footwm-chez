;; Footwm GTK menu module.
;;
;; Written by Jerry 2019-2025.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm menugtk)
  (export
   make-table
   table?
   table-headers
   table-types
   table-rows
   menu)
  (import
   (rnrs)
   (only (chezscheme) enumerate)
   (footwm fnmatch)
   (footwm ftypes-util)
   (footwm gobject)
   (footwm gtk)
   (footwm util)
   )

  (define-record-type table
    (fields headers types rows))

  (define init-window
    (lambda (w title table activate-callback create-callback delete-callback)
      (gtk-window-set-title w title)
      (let* ([store (apply gtk-list-store-new (table-types table))]
             [fstore (gtk-tree-model-filter-new store 0)])
        (fill-model! store table)
        (let ([grid (gtk-grid-new)]
              [text (gtk-entry-new)]
              [tree (gtk-tree-view-new-with-model/list fstore)])
          (gtk-widget-set-hexpand text #t)
          (gtk-widget-set-hexpand tree #t)
          (gtk-widget-set-vexpand tree #t)
          (gtk-tree-view-set-enable-search tree #f)
          (add-table-headers tree table)
          (gtk-container-add w grid)
          (gtk-grid-attach grid text 0 0 1 1)
          (gtk-grid-attach grid tree 0 1 1 1)
          (let ([current-filter? (make-cell-filter "")]
                [filtered-rows (table-rows table)])
            (define visible-rows
              (lambda ()
                (filter (lambda (row) (row-visible? row current-filter?)) filtered-rows)))
            (g-signal-connect w "key-press-event"
              (keyevent-callback
               (lambda (widget keyval)
                 (case-equal? keyval
                   [GDK_KEY_Delete (delete-callback (visible-rows)) (gtk-main-quit)]
                   [GDK_KEY_Escape (gtk-main-quit)])
                 #t)) 0)
            (g-signal-connect text "changed"
              (callback-widget-data
               (lambda (editable user-data)
                 (set! current-filter? (make-cell-filter (gtk-entry-get-text text)))
                 (gtk-tree-model-filter-refilter fstore))) 0)
            (gtk-tree-model-filter-set-visible-func fstore
              (callback-row-visible-filter?
               (lambda (model iter user-data)
                 (let* ([row-num (gtk-tree-model-get/int model iter 0)]
                        [row (list-ref (table-rows table) row-num)])
                   (if (memq row filtered-rows)
                       (row-visible? row current-filter?)
                       #f)))) 0 0)
            ;; Watch for ENTER key.
            (g-signal-connect text "activate"
              (callback-widget-data
               (lambda (widget user-data)
                 ;; Get the rows visible in the list view.
                 (let ([vs (visible-rows)])
                   (cond
                    [(null? vs)
                     ;; No matches, assume creation if there's some text.
                     (when create-callback
                       (let ([name (gtk-entry-get-text text)])
                         (when (> (string-length name) 0)
                           (create-callback name)
                           (gtk-main-quit))))]
                    [(= (length vs) 1)
                     ;; Activate if there's only one.
                     (activate-callback (car vs))
                     (gtk-main-quit)]
                    [else
                     ;; Save off the current visible rows and reset the current text filter.
                     (set! filtered-rows vs)
                     (set! current-filter? (make-cell-filter ""))
                     (gtk-entry-buffer-delete-text (gtk-entry-get-buffer text) 0 -1)])))) 0))
          (gtk-tree-selection-set-mode (gtk-tree-view-get-selection tree) GTK_SELECTION_SINGLE)
          (g-signal-connect tree "row-activated"
            (callback-row-activated
             (lambda (treeview path column user-data)
               ;; Assumes column 0 is the key/table-index.
               (let ([key (gtk-tree-view-get/int treeview 0)])
                 (activate-callback (list-ref (table-rows table) key))
                 (gtk-main-quit)))) 0))
        (g-object-unref fstore)
        (g-object-unref store))))

  (define add-table-headers
    (lambda (tree table)
      (for-each
       (lambda (i header)
         (add-column tree header i))
       (enumerate (table-headers table)) (table-headers table))))

  (define make-cell-filter
    (lambda (needle)
      (if (= (string-length needle) 0)
          (lambda (x) #t)	; show-all
          (lambda (haystack)
            (and (fnsearch needle haystack) #t)))))

  (define row-visible?
    (lambda (row filter?)
      ;; match filter? against any column containing text.
      (let* ([strcells (filter string? row)])
        (not (null? (filter filter? strcells))))))

  (define add-column
    (lambda (tree header field-id)
      (let ([column (gtk-tree-view-column-new)]
            [renderer (gtk-cell-renderer-text-new)])
        (gtk-tree-view-append-column tree column)
        (gtk-tree-view-column-set-title column header)
        (gtk-tree-view-column-pack-start column renderer #t)
        (gtk-tree-view-column-add-attribute column renderer "text" field-id))))

  (define fill-model!
    (lambda (store table)
      (fmem ([iter &iter gtktreeiter]
             [gvi &gvi gvalue]
             [gvs &gvs gvalue])
        (g-value-zero! &gvi)
        (g-value-init &gvi g-type-int)
        (g-value-zero! &gvs)
        (g-value-init &gvs g-type-string)
        (let ([rows (table-rows table)]
              [types (table-types table)])
          (for-each
           (lambda (row)
             ;; Add new row.
             (gtk-list-store-append store &iter)
             (for-each
              (lambda (i cell)
                (case-equal? (list-ref types i)
                  [g-type-int
                   (g-value-set-int &gvi cell)
                   (gtk-list-store-set-value store &iter i &gvi)]
                  [g-type-string
                   (g-value-set-string &gvs cell)
                   (gtk-list-store-set-value store &iter i &gvs)]))
              (enumerate row) row))
           (table-rows table))))))

  (define menu
    (lambda (title table activate-callback create-callback delete-callback)
      (gtk-init 0 0)
      (let ([w (gtk-window-new GTK_WINDOW_TOPLEVEL)])
        (init-window w title table activate-callback create-callback delete-callback)
        (g-signal-connect w "destroy" gtk-main-quit-addr 0)
        (gtk-widget-show-all w))
      (gtk-main))))
