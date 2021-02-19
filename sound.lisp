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
  (sample-rate 44100d0)
  (playing-p t)
  (elapsed-samples 0)
  (event-queue))

(defparameter *sound-thread* nil)
(defparameter *audio-state* nil)

(defun set-float32 (vec offset val)
  (let ((v (ieee-floats:encode-float32 val)))
    (loop
      :for n :from 0 :below 4
      :for shift := (* n 8)
      :for byte := (ash (logand v (ash #xff shift)) (- shift))
      :do (setf (aref vec (+ n (* 4 offset))) byte))))

(defun make-sound-fn (state signal-fn)
  (lambda ()
    (let* ((number-of-frames 1024)
           (sample-rate (audio-state-sample-rate state)))
      (pa:with-audio
          (pa:with-default-audio-stream (stream 0 2
                                                  :frames-per-buffer number-of-frames
                                                  :sample-format :float
                                                  :sample-rate sample-rate)
          (loop
            :with buffer := (make-array (* 2 number-of-frames) :element-type 'single-float)
            :while (audio-state-playing-p state)
            :do (loop
                  :for n :from 0 :below number-of-frames
                  :do (multiple-value-bind (l r)
                          (funcall signal-fn state)
                        (setf (aref buffer (* 2 n)) l
                              (aref buffer (1+ (* 2 n))) r)))
                (pa:write-stream stream buffer)))))
    (setf *audio-state* nil
          *sound-thread* nil)))

(defun init (queue)
  (let ((state (make-audio-state :sample-rate 44100d0
                                 :playing-p t
                                 :elapsed-samples 0
                                 :event-queue queue)))
    (setf *audio-state* state)))

(defun start (signal-fn)
  (unless *sound-thread*
    (let ((th (bt:make-thread (make-sound-fn *audio-state* signal-fn)
                               :name "sp: audio thread"
                               :initial-bindings `((*standard-output* . ,*standard-output*)))))
      (setf *sound-thread* th))))

(defun stop ()
  (when *audio-state*
    (setf (audio-state-playing-p *audio-state*) nil))
  (when *sound-thread*
    (when (bt:thread-alive-p *sound-thread*)
      (bt:destroy-thread *sound-thread*))
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
    ;; TODO: process events in queue
    (let ((v (* 0.3 (process-pulse osc state))))
      (incf (audio-state-elapsed-samples state))
      (values v v))))
