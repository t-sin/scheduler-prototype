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

(defun set-float32 (vec offset val)
  (let ((v (ieee-floats:encode-float32 val)))
    (loop
      :for n :from 0 :below 4
      :for shift := (* n 8)
      :for byte := (ash (logand v (ash #xff shift)) (- shift))
      :do (setf (aref vec (+ n (* 4 offset))) byte))))

(defparameter *device-name* "sysdefault:CARD=USB")

(defun start-sound (state signal-fn)
  (lambda ()
    (let ((frames-per-buffer 1024)
          (sample-rate (audio-state-sample-rate state)))
      (also-alsa:with-alsa-device (pcm *device-name* frames-per-buffer 'single-float
                                       :direction :output
                                       :channels-count 1
                                       :sample-rate sample-rate)
        (also-alsa:alsa-start pcm)
        (loop
          (loop
            :with buffer := (also-alsa:buffer pcm)
            :for n :from 0 :below (also-alsa:buffer-size pcm)
            :do (set-float32 buffer n (funcall signal-fn state)))
          ;; (multiple-value-bind (avail delay)
          ;;     (also-alsa:get-avail-delay pcm)
          ;;   (declare (ignore avail delay))
          (also-alsa:alsa-write pcm)
          (also-alsa:alsa-wait pcm 10))))))

(defparameter *sound-thread* nil)
(defparameter *audio-state* nil)

(defun init ()
  (let ((state (make-audio-state :sample-rate 44100
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
    (* 0.3 (process-pulse osc state))))
