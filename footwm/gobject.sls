;; Glib/GObject defines.
;;
;; Written by Akce 2019-2020.
;;
;; SPDX-License-Identifier: Unlicense

(library (footwm gobject)
  (export
   gcallback
   gchar
   gchar*
   gpointer
   gtype
   gint
   guint
   gulong
   gvalue

   g-value-zero!
   g-value-init
   g-value-set-int
   g-value-set-string

   g-type-int
   g-type-string

   g-application-run

   g-object-unref

   g-signal-connect
   g-signal-connect-swapped
   g-signal-connect-data
   )
  (import
   (chezscheme)
   (only (footwm util) bitmap))

  (define lib-load
    (begin
      (load-shared-object "libgio-2.0.so.0")
      (load-shared-object "libgobject-2.0.so.0")))

  (define-syntax define-f
    (syntax-rules ()
      [(_ (libsym args return) ...)
       (begin
         (define libsym (foreign-procedure (symbol->string 'libsym) args return)) ...)]))

  (define-ftype gulong unsigned-long)
  (define-ftype gpointer void*)
  ;; from glib/gtypes.h
  (define-ftype gchar char)
  (define-ftype gchar* (* gchar))
  ;; from gobject/gtype.h: warning may not be a long if sizeof(size_t) != sizeof(long)
  (define-ftype gtype long)
  (define-ftype gint int)
  (define-ftype guint unsigned)
  (define-ftype gvalue
    (struct
     [g-type	gtype]
     ;; this is not how struct _GValue is defined, but i don't need details, only the correct struct sizeof().
     [data1	unsigned-64]
     [data2	unsigned-64]))

  (define g-type-fundamental-shift 2)
  (define g-type-int	(bitwise-arithmetic-shift-left 6 g-type-fundamental-shift))
  (define g-type-string	(bitwise-arithmetic-shift-left 16 g-type-fundamental-shift))

  ;; Equivalent to: GValue value = G_VALUE_INIT;
  (define g-value-zero!
    (lambda (&gv)
      (ftype-set! gvalue (g-type) &gv 0)
      (ftype-set! gvalue (data1) &gv 0)
      (ftype-set! gvalue (data2) &gv 0)))

  (define g-value-init
    (foreign-procedure "g_value_init" ((* gvalue) gtype) (* gvalue)))
  (define g-value-set-int
    (foreign-procedure "g_value_set_int" ((* gvalue) int) void))
  (define g-value-set-string
    (foreign-procedure "g_value_set_string" ((* gvalue) string) void))

  ;;; From gclosure.h
  ;; should be: void (*gclosurenotify)(gpointer, gclosure*);
  (define-ftype gclosurenotify void*)
  ;; should be: void (*gcallback)();
  ;; Although header mentions that the signature changes depending on the signal so void* is probably the best option.
  (define-ftype gcallback void*)

  ;;;; From gio/gapplication.h
  (define g-application-run
    (foreign-procedure "g_application_run" (void* int void*) int))

  ;;;; From gobject/gobject.h
  (define g-object-unref
    (foreign-procedure "g_object_unref" (gpointer) void))

  ;;;; From gsignal.h
  (bitmap GConnectFlags
    (G_CONNECT_AFTER 0)
    (G_CONNECT_SWAPPED 1))

  ;; g-signal-connect is a C convenience macro defined in gsignal.h
  (define g-signal-connect
    (lambda (instance detailed-signal c-handler data)
      (g-signal-connect-data instance detailed-signal c-handler data 0 G_CONNECT_AFTER)))

  ;; g-signal-connect-swapped is a C convenience macro defined in gsignal.h
  (define g-signal-connect-swapped
    (lambda (instance detailed-signal c-handler data)
      (g-signal-connect-data instance detailed-signal c-handler data 0 G_CONNECT_SWAPPED)))

  (define g-signal-connect-data
    ;; args: instance detailed-signal c-handler data destroy-data GConnectFlags
    (foreign-procedure "g_signal_connect_data" (gpointer string gcallback gpointer gclosurenotify int) gulong))

  )
