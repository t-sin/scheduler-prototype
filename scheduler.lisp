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
  (queue nil))

(defparameter *scheduler-thread* nil)
(defparameter *scheduler-state* nil)

(defun make-scheduler-fn (state)
  (lambda ()
    (print state)
    (setf *scheduler-state* nil
          *scheduler-thread* nil)))

(defun start (events queue)
  (unless *scheduler-thread*
    (let ((state (make-scheduler :events events :queue queue))
          (specials `((*standard-output* . ,*standard-output*))))
      (setf *scheduler-state* state
            *scheduler-thread* (bt:make-thread (make-scheduler-fn state)
                                               :name "sp: scheduler thread"
                                               :initial-bindings specials)))))

(defun stop ()
  (when *scheduler-state*
    (setf (scheduler-running-p *scheduler-state*) nil))
  (when *scheduler-thread*
    (when (bt:thread-alive-p *scheduler-thread*)
      (bt:destroy-thread *scheduler-thread*))
    (setf *scheduler-thread* nil)))
