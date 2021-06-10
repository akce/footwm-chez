;;;; Footwm example config.

;; List of initial desktops.
(desktops
  "communications"
  "system"
  "entertainment"
  "development")

;; New window desktop assignments.
;; These string compare against the ICCCM WM_CLASS window instance/res_name property.
;; Currently, this is all that footwm supports.
;; For the case where the desktop does not exist, footwm will assign the new window
;; to the current desktop.
(assignments
  (= instance "xmutt"	"communications")
  (= instance "peace"	"entertainment")
  (= instance "wm"	"system")
  (= instance "xmpp"	"communications")
  )
