(defsystem :scheduler-prototype
  :depends-on ("cl-portaudio"
               "bordeaux-threads"
               "ieee-floats"
               "queues")
  :serial t
  :components ((:file "event")
               (:file "scheduler")
               (:file "sound")
               (:file "main")))
