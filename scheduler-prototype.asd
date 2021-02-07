(defsystem :scheduler-prototype
  :depends-on ("cl-portaudio"
               "bordeaux-threads")
  :serial t
  :components ((:file "event")
               (:file "sound")
               (:file "main")))
