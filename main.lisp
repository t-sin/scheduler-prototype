(defpackage #:scheduler-prototype
  (:use #:cl)
  (:nicknames #:sp)
  (:export #:start))
(in-package #:scheduler-prototype)

(defun start (pathname)
  (scheduler-prototype.event:load-events pathname))
