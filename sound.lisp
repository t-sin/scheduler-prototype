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
  sample-rate playing-p elapsed-samples event-queue)

(defun set-float32 (vec offset val)
  (let ((v (ieee-floats:encode-float32 val)))
    (loop
      :for n :from 0 :below 4
      :for shift := (* n 8)
      :for byte := (ash (logand v (ash #xff shift)) (- shift))
      :do (setf (aref vec (+ n (* 4 offset))) byte))))

(defun start-sound (state signal-fn)
  (lambda ()
    (let* ((number-of-frames 1024)
           (frame-size 4)
           (channels 2)
           (buffer-size (* frame-size channels number-of-frames))
           (sample-rate (audio-state-sample-rate state))
           (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
      (handler-bind ((condition (lambda (c)
                                  (print c *standard-output*))))
        (pulseaudio:with-audio-stream (stream :direction :output
                                              :sample-format :float32le
                                              :channels channels
                                              :rate sample-rate
                                              :buffer-size buffer-size)
          (loop
            :while (audio-state-playing-p state)
            :do (loop
                  :for n :from 0 :below number-of-frames
                  :do (multiple-value-bind (l r)
                          (funcall signal-fn state)
                        (let ((pos (* n channels frame-size)))
                          (set-float32 buffer n l)
                          (set-float32 buffer (1+ n) r))))
                (pulseaudio:write-stream stream buffer)
                (pulseaudio:drain-stream stream)))))))

(defparameter *sound-thread* nil)
(defparameter *audio-state* nil)

(defun init ()
  (let ((state (make-audio-state :sample-rate 44100
                                 :playing-p t
                                 :elapsed-samples 0
                                 :event-queue ())))
    (setf *audio-state* state)))

(defun start (signal-fn)
  (unless *sound-thread*
    (let ((th (bt:make-thread (start-sound *audio-state* signal-fn)
                               :name "pukunui-sound-thread"
                               :initial-bindings `((*standard-output* . ,*standard-output*)))))
      (setf *sound-thread* th))))

(defun stop ()
  (when *sound-thread*
    (when (bt:thread-alive-p *sound-thread*)
      (setf (audio-state-playing-p *audio-state*) nil))
      ;;(bt:destroy-thread *sound-thread*))
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
      (values v v))))
