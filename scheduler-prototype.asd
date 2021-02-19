(defsystem :scheduler-prototype
  :depends-on ("cl-portaudio"
               "bordeaux-threads"
               "ieee-floats"
               "queues"
               "queues.simple-queue")
  :serial t
  :components ((:file "event")
               (:file "scheduler")
               (:file "sound")
               (:file "main")))
