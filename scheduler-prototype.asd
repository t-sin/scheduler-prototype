(defsystem :scheduler-prototype
  :depends-on ("cl-pulseaudio"
               "bordeaux-threads"
               "ieee-floats")
  :serial t
  :components ((:file "event")
               (:file "sound")
               (:file "main")))
