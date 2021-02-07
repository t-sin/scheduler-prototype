(defpackage #:scheduler-prototype.sound
  (:use #:cl)
  (:nicknames #:sp-sound)
  (:import-from #:scheduler-prototype.event
                #:event/note-on
                #:event/note-on-freq
                #:event/note-off)
  (:export #:*sound-thread*
           #:init
           #:start
           #:stop
           #:process-signal))
(in-package #:scheduler-prototype.sound)

(defstruct audio-state
  sample-rate elapsed-samples event-queue)

(defun start-portaudio (state signal-fn)
  (lambda ()
    (let ((frames-per-buffer 1024)
          (sample-rate (audio-state-sample-rate state)))
      (pa:with-audio
        (pa:with-default-audio-stream (stream 0 2
                                              :frames-per-buffer frames-per-buffer
                                              :sample-format :float
                                              :sample-rate sample-rate)
          (let ((buffer (make-array (* 2 frames-per-buffer)
                                    :initial-element 0.0
                                    :element-type 'single-float)))
            (loop
              (loop
                :for n :from 0 :below frames-per-buffer
                :do (multiple-value-bind (l r)
                        (funcall signal-fn state)
                      (setf (aref buffer (* 2 n)) (coerce l 'single-float)
                            (aref buffer (1+ (* 2 n))) (coerce r 'single-float))))
              (pa:write-stream stream buffer))))))))

(defparameter *sound-thread* nil)
(defparameter *audio-state* nil)

(defun init ()
  (let ((state (make-audio-state :sample-rate 44100D0
                                 :elapsed-samples 0
                                 :event-queue ())))
    (setf *audio-state* state)))

(defun start (signal-fn)
  (unless *sound-thread*
    (let ((th (bt:make-thread (start-portaudio *audio-state* signal-fn)
                               :name "pukunui-sound-thread"
                               :initial-bindings `((*standard-output* . ,*standard-output*)))))
      (setf *sound-thread* th))))

(defun stop ()
  (when *sound-thread*
    (bt:destroy-thread *sound-thread*)
    (setf *sound-thread* nil)))

(defun pulse (x &optional (duty (/ 1 2)))
  (let* ((x (mod x 1)))
    (cond ((< x duty) 1)
          (t -1))))

(defstruct pulse-osc
  (phase 0)
  (freq 440)
  (duty (/ 1 2)))

(defun process-pulse (pulse-osc audio-state)
  (let ((v (pulse (pulse-osc-phase pulse-osc)
                  (pulse-osc-duty pulse-osc)))
        (freq (pulse-osc-freq pulse-osc))
        (sample-rate (audio-state-sample-rate audio-state)))
    (incf (pulse-osc-phase pulse-osc) (/ freq sample-rate 2))
    v))

(defun adsr (a d s r state elapsed)
  (cond ((minusp elapsed) (values nil 0))
        ((and (eq state :a) (< elapsed a))
         (values :a (/ elapsed a)))
        ((and (member state '(:a :d)) (< elapsed (+ a d)))
         (values :d (- 1 (* (- 1 s) (/ (- elapsed a) d)))))
        ((and (member state '(:d :s)) (>= elapsed (+ a d)))
         (values :s s))
        ((and (eq state :r) (< elapsed r)) ;; TODO
         (values :r (- s (* elapsed (/ s r)))))
        (t (values nil 0))))

(defstruct envelope
  (state nil)
  (start 0))

(let ((osc (make-pulse-osc))
      (env (make-envelope)))
  (declare (ignorable env))
  (defun process-signal (state)
    (handler-case
        (let ((oscval (* 0.3 (process-pulse osc state))))
          (values oscval oscval))
      (condition (c) (print c)))))
