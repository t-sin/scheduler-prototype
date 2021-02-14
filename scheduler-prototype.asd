(defsystem :scheduler-prototype
  :depends-on ("cl-portaudio"
               "bordeaux-threads"
               "ieee-floats")
  :serial t
  :components ((:file "event")
               (:file "scheduler")
               (:file "sound")
               (:file "main")))
