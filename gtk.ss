(library (gtk)
  (export
   gdkeventkey*

   gtkwidget*
   gtktreepath*
   gtktreeiter
   gtktreeiter*
   gpointer

   GDK_KEY_PRESS_MASK
   GDK_KEY_RELEASE_MASK

   keyevent-callback

   gtk-application-new
   gtk-application-window-new
   activate-callback

   gtk-widget-show-all
   gtk-widget-set-hexpand
   gtk-widget-set-vexpand
   gtk-widget-add-events
   gtk-widget-destroy

   GTK_WINDOW_TOPLEVEL
   GTK_WINDOW_POPUP
   gtk-window-new
   gtk-window-set-title

   gtk-container-add
   gtk-grid-new
   gtk-grid-attach

   gtk-entry-new
   gtk-entry-get-text
   gtk-entry-get-buffer
   gtk-entry-buffer-delete-text
   callback-widget-data

   gtk-list-store-new
   gtk-list-store-append
   gtk-list-store-set-value

   gtk-tree-view-get/int
   gtk-tree-model-get/int
   gtk-tree-model-filter-set-visible-func
   gtk-tree-model-filter-refilter

   gtk-tree-view-new-with-model/list
   gtk-tree-model-filter-new
   gtk-cell-renderer-text-new
   gtk-tree-view-append-column
   gtk-tree-view-column-new
   gtk-tree-view-column-set-title
   gtk-tree-view-column-pack-start
   gtk-tree-view-column-add-attribute
   gtk-tree-view-set-enable-search

   GTK_SELECTION_NONE
   GTK_SELECTION_SINGLE
   GTK_SELECTION_BROWSE
   GTK_SELECTION_MULTIPLE

   gtk-tree-view-get-selection
   gtk-tree-selection-set-mode
   gtk-tree-selection-get-selected
   gtk-tree-selection-selected-foreach

   callback-row-visible-filter?
   callback-row-activated

   gtk-init
   gtk-main
   gtk-main-quit
   gtk-main-quit-addr)
  (import
   (chezscheme)
   (ftypes-util)
   (gobject)
   (only (util) bitmap enum))

  (define lib-load
    (load-shared-object "libgtk-3.so"))

  ;; Gdk is an interface between underlying system and gtk+.
  (define-ftype gdkeventkey* void*)

  (define-ftype gtkapplication* void*)
  (define-ftype gtkwidget* void*)
  (define-ftype gtkentrybuffer* void*)
  (define-ftype gtkliststore* void*)
  (define-ftype gtktreepath* void*)
  (define-ftype gtkcellrenderer* void*)
  (define-ftype gtktreeviewcolumn* void*)
  (define-ftype gtktreeselection* void*)
  ;; should be (gtktreeselectionforeachfunc (gtkliststore* gtktreepath* gtktreeiter* gpointer) void)
  (define-ftype gtktreeselectionforeachfunc void*)
  ;; from gtk/gtktreemodel.h
  (define-ftype gtktreeiter
    (struct
     [stamp		gint]
     [user-data		gpointer]
     [user-data2	gpointer]
     [user-data3	gpointer]))
  (define-ftype gtktreeiter* (* gtktreeiter))

  ;; Gdk.
  (bitmap GdkEventMask
    (GDK_KEY_PRESS_MASK		10)
    (GDK_KEY_RELEASE_MASK	11))

  (define keyevent-callback
    (lambda (callback)
      (let ([fp (foreign-callable callback (gtkwidget* gdkeventkey* gpointer) boolean)])
        (lock-object fp)
        (foreign-callable-entry-point fp))))

  ;;;; Application.
  (define gtk-application-new
    (foreign-procedure "gtk_application_new" (string int) gtkapplication*))
  (define gtk-application-window-new
    (foreign-procedure "gtk_application_window_new" (gtkapplication*) gtkwidget*))

  (define activate-callback
    (lambda (callback)
      (let ([fp (foreign-callable callback (gtkapplication* gpointer) void)])
        (lock-object fp)
        (foreign-callable-entry-point fp))))

  ;;;; Widgets.
  (define gtk-widget-show-all
    (foreign-procedure "gtk_widget_show_all" (gtkwidget*) void))
  (define gtk-widget-set-hexpand
    (foreign-procedure "gtk_widget_set_hexpand" (gtkwidget* boolean) void))
  (define gtk-widget-set-vexpand
    (foreign-procedure "gtk_widget_set_vexpand" (gtkwidget* boolean) void))
  (define gtk-widget-add-events
    (foreign-procedure "gtk_widget_add_events" (gtkwidget* gint) void))
  (define gtk-widget-destroy
    (foreign-entry "gtk_widget_destroy"))

  ;; General widget user-data callback.
  (define callback-widget-data
    (lambda (callback)
      (let ([fp (foreign-callable callback (void* gpointer) void)])
        (lock-object fp)
        (foreign-callable-entry-point fp))))

  ;;;; Window.
  (enum GtkWindowType
    (GTK_WINDOW_TOPLEVEL 0)
    (GTK_WINDOW_POPUP 1))
  (define gtk-window-new
    (foreign-procedure "gtk_window_new" (int) gtkwidget*))
  (define gtk-window-set-title
    (foreign-procedure "gtk_window_set_title" (gtkwidget* string) void))

  ;;;; Containers.
  (define gtk-container-add
    ;; first param should be a GtkContainer*, but gtk_grid_new returns a GtkWidget* so use that instead.
    (foreign-procedure "gtk_container_add" (gtkwidget* gtkwidget*) void))

  (define gtk-grid-new
    (foreign-procedure "gtk_grid_new" () gtkwidget*))
  (define gtk-grid-attach
    ;; Args: grid* widget* left right width height
    (foreign-procedure "gtk_grid_attach" (gtkwidget* gtkwidget* int int int int) void))

  ;;;; Text entry.
  (define gtk-entry-new
    (foreign-procedure "gtk_entry_new" () gtkwidget*))
  (define gtk-entry-get-text
    (foreign-procedure "gtk_entry_get_text" (void*) string))
  (define gtk-entry-get-buffer
    (foreign-procedure "gtk_entry_get_buffer" (gtkwidget*) gtkentrybuffer*))
  (define gtk-entry-buffer-delete-text
    (foreign-procedure "gtk_entry_buffer_delete_text" (gtkentrybuffer* guint gint) guint))

  ;;;; List store.
  (define gtk-list-store-newv
    (foreign-procedure "gtk_list_store_newv" (int void*) gtkliststore*))

  (define gtk-list-store-new
    (lambda gtypes
      (fmem ([mem &mem gulong (length gtypes)])
        (fill-memory/ulongs mem gtypes)
        (gtk-list-store-newv (length gtypes) mem))))

  (define gtk-list-store-append
    (foreign-procedure "gtk_list_store_append" (void* (* gtktreeiter)) void))

  (define gtk-list-store-set-value
    (foreign-procedure "gtk_list_store_set_value" (void* (* gtktreeiter) int (* gvalue)) void))

  ;;;; Tree model.
  (define gtk-tree-view-get/int
    (lambda (treeview column-id)
      (let ([sel (gtk-tree-view-get-selection treeview)])
        (fmem ([model &model void*]
               [iter &iter gtktreeiter])
          (gtk-tree-selection-get-selected sel &model &iter)
          (gtk-tree-model-get/int (foreign-ref 'void* model 0) &iter column-id)))))

  (define gtk-tree-model-get/int
    (lambda (model iter column-id)
      (fmem ([num &num int])
        (gtk-tree-model-get model iter column-id num -1)
        (foreign-ref 'int num 0))))

  (define gtk-tree-model-get
    (foreign-procedure "gtk_tree_model_get" (void* (* gtktreeiter) int void* int) void))
  (define gtk-tree-model-filter-set-visible-func
    (foreign-procedure "gtk_tree_model_filter_set_visible_func" (void* void* void* void*) void))
  (define gtk-tree-model-filter-refilter
    (foreign-procedure "gtk_tree_model_filter_refilter" (void*) void))

  ;;;; Tree view.
  ;; New tree with list store.
  (define gtk-tree-view-new-with-model/list
    (foreign-procedure "gtk_tree_view_new_with_model" (gtkliststore*) gtkwidget*))
  (define gtk-tree-model-filter-new
    (foreign-procedure "gtk_tree_model_filter_new" (gtkliststore* gtktreepath*) void*))
  (define gtk-cell-renderer-text-new
    (foreign-procedure "gtk_cell_renderer_text_new" () gtkcellrenderer*))
  (define gtk-tree-view-append-column
    (foreign-procedure "gtk_tree_view_append_column" (gtkwidget* gtktreeviewcolumn*) gint))
  ;; Tree columns.
  (define gtk-tree-view-column-new
    (foreign-procedure "gtk_tree_view_column_new" () gtktreeviewcolumn*))
  (define gtk-tree-view-column-set-title
    (foreign-procedure "gtk_tree_view_column_set_title" (gtktreeviewcolumn* string) void))
  (define gtk-tree-view-column-pack-start
    (foreign-procedure "gtk_tree_view_column_pack_start" (gtktreeviewcolumn* gtkcellrenderer* boolean) void))
  (define gtk-tree-view-column-add-attribute
    (foreign-procedure "gtk_tree_view_column_add_attribute" (gtktreeviewcolumn* gtkcellrenderer* string gint) void))
  (define gtk-tree-view-set-enable-search
    (foreign-procedure "gtk_tree_view_set_enable_search" (gtkwidget* boolean) void))
  ;; Tree selections.
  (enum GtkSelectionMode
    (GTK_SELECTION_NONE 0)
    (GTK_SELECTION_SINGLE 1)
    (GTK_SELECTION_BROWSE 2)
    (GTK_SELECTION_MULTIPLE 3))
  (define gtk-tree-view-get-selection
    (foreign-procedure "gtk_tree_view_get_selection" (gtkwidget*) gtktreeselection*))
  (define gtk-tree-selection-set-mode
    (foreign-procedure "gtk_tree_selection_set_mode" (gtktreeselection* int) void))
  (define gtk-tree-selection-get-selected
    (foreign-procedure "gtk_tree_selection_get_selected" (gtktreeselection* (* gtkliststore*) (* gtktreeiter)) boolean))
  (define gtk-tree-selection-selected-foreach
    (foreign-procedure "gtk_tree_selection_selected_foreach" (gtktreeselection* gtktreeselectionforeachfunc gpointer) void))

  (define tree-selection-changed-callback
    (lambda (callback)
      (let ([fp (foreign-callable callback (gtktreeselection* gpointer) void)])
        (lock-object fp)
        (foreign-callable-entry-point fp))))

  (define callback-row-visible-filter?
    (lambda (callback)
      (let ([fp (foreign-callable callback (void* (* gtktreeiter) gpointer) boolean)])
        (lock-object fp)
        (foreign-callable-entry-point fp))))

  (define callback-row-activated
    (lambda (callback)
      (let ([fp (foreign-callable callback (gtkwidget* gtktreepath* void* gpointer) void)])
        (lock-object fp)
        (foreign-callable-entry-point fp))))

  (define gtk-init
    (foreign-procedure "gtk_init" (void* void*) void))
  (define gtk-main
    (foreign-procedure "gtk_main" () void))
  (define gtk-main-quit
    (foreign-procedure "gtk_main_quit" () void))
  (define gtk-main-quit-addr
    (foreign-entry "gtk_main_quit")))
