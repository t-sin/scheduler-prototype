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
  (let ((events (load-events pathname))
        (queue (queues:make-queue :simple-queue)))
    (sp-sound:init queue)
    (sp-sound:start #'sp-sound:process-signal)
    (sp-scheduler:start events queue)))

(defun stop ()
  (sp-scheduler:stop)
  (sp-sound:stop))
