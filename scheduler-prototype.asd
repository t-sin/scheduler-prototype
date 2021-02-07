(defsystem :scheduler-prototype
  :depends-on ("also-alsa"
               "bordeaux-threads"
               "ieee-floats")
  :serial t
  :components ((:file "event")
               (:file "sound")
               (:file "main")))
