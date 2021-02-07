(defpackage #:scheduler-prototype
  (:use #:cl)
  (:nicknames #:sp)
  (:import-from #:scheduler-prototype.event
                #:load-events)
  (:export #:start
           #:stop))
(in-package #:scheduler-prototype)

(defstruct transport
  (bpm 120) )

(defun start (&optional (pathname "./sequence-data.lisp"))
  (load-events pathname)
  (sp-sound:init)
  (sp-sound:start #'sp-sound:process-signal))

(defun stop ()
  (sp-sound:stop))
