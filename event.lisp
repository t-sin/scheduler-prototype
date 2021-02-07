(defpackage #:scheduler-prototype.event
  (:use #:cl)
  (:export #:measure
           #:measure-beats
           #:measure-nots
           #:timepos
           #:timepos-bar
           #:timepos-tick
           #:event
           #:event-timepos
           #:event-type
           #:event-body
           #:load-events))
(in-package #:scheduler-prototype.event)

(defconstant +ticks-per-bar+ 1200)

(defstruct measure
  beats notes)

(defstruct timepos
  bar tick)

(defun timepos+ (a b)
  a)

(defun timepos-from-length (timepos)
  0)

(defstruct event/note-on freq)
(defstruct event/note-off)
(defstruct event/set-bpm bpm)
(defstruct event/set-measure beats notes)

(defun key-num (name)
  (let* ((key-name (symbol-name name))
         ;; キーの位置を数字で得る
         (num (position (char key-name 0) "A BC D EF G")))
    (if (null num)
        nil  ; 上にない文字は休符扱い
        (+ num
           (if (= (length key-name) 1)
               0
               ;; シャープ・フラットによって1加減算する
               (ecase (char key-name 1)
                 (#\+ 1)
                 (#\- -1)))))))

(defun key->freq (n o)
  (let ((num (key-num n)))
    (unless (null num)  ; 休符のときは周波数をnilとする
      (let* ((pitch-ratio (/ num 12))  ; 音位置を12等分した比率にする
             (relative-octave (- o 4)))  ; オクターブを4基準になおす
        (when (find (char (symbol-name n) 0) "AB")
          ;; key-numの返す音位置はA基準のため、C基準では1オクターブ下になる
          (incf relative-octave))
        (* 440  ; 基本周波数
           (expt 2 (+ pitch-ratio relative-octave)))))))

(defstruct event
  (timepos nil :type timepos)
  (body nil :type (member event/note-on
                          event/note-off
                          event/set-bpm
                          event/set-measure)))

(defun load-event (pathname)
  (if (probe-file pathname)
      (let ((sexp (with-open-file (in pathname :direction :input)
                    (read in)))
            (timepos (make-timepos :bar 0 :tick 0))
            (events ()))
        (let* ((bpm (getf sexp :bpm))
               (set-bpm (make-event/set-bpm :bpm (or bpm 120)))
               (event (make-event :timepos (copy-timepos timepos) :body set-bpm))) 
          (push event events))

        (let* ((measure (getf sexp :measure))
               (set-measure (make-event/set-measure :beats (or (getf measure :beats) 4)
                                                    :notes (or (getf measure :notes) 4)))
               (event (make-event :timepos (copy-timepos timepos) :body set-measure)))
          (push event events))

        (loop
          :for note :in (getf sexp :notes)
          :do (let ((note-name (first note))
                    (note-octave (second note))
                    (note-length (third note)))
                (let* ((freq (key->freq note-name note-octave))
                       (note-on (make-event/note-on :freq freq))
                       (event (make-event :timepos (copy-timepos timepos) :body note-on)))
                  (push event events))

                (let* ((new-timepos (timepos+ timepos (timepos-from-length note-length)))
                       (note-off (make-event/note-off))
                       (event (make-event :timepos (copy-timepos new-timepos) :body note-off)))
                  (setf timepos new-timepos)
                  (push event events))))

        (coerce (nreverse events) 'vector))
      (error "file ~s does not exist." pathname)))
