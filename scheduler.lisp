(defpackage #:scheduler-prototype.scheduler
  (:use #:cl
        #:scheduler-prototype.event)
  (:nicknames #:sp-scheduler)
  (:export #:start
           #:stop))
(in-package #:scheduler-prototype.scheduler)

(defstruct scheduler
  (running-p nil :type (or t nil))
  (now (make-timepos :bar 0 :tick 0) :type timepos)
  (events nil)
  (event-queue nil))

(defparameter *scheduler-thread* nil)
(defparameter *scheduler-state* nil)

(defun make-scheduler-fn (state)
  (lambda ()
    (setf *scheduler-state* nil
          *scheduler-thread* nil)))

(defun start (events event-queue)
  (unless *scheduler-thread*
    (let ((state (make-scheduler :events events :event-queue event-queue))
          (specials `((*standard-output* . ,*standard-output*))))
      (setf *scheduler-state* state
            *scheduler-thread* (bt:make-thread (make-scheduler-fn state)
                                               :name "scheduler thread"
                                               :initial-bindings specials)))))

(defun stop ()
  (when *scheduler-thread*
    (when (bt:thread-alive-p *scheduler-thread*)
      (setf (scheduler-running-p *scheduler-state*) nil))
    (setf *scheduler-thread* nil)))
