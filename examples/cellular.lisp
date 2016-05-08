#!/bin/sh
":" ; exec cl-launch -Q -s cl-mpi -E main -X -- "$0" "$@" || exit

;;; A one dimensional cellular automaton.

(in-package :cl-user)

(defpackage :cl-mpi/examples/cellular
  (:use :cl :alexandria :cl-mpi :static-vectors))
(in-package :cl-mpi/examples/cellular)

(defun printf (fmt &rest args)
  (format t "rank ~d: " (mpi-comm-rank))
  (apply #'format t fmt args)
  (finish-output))

(defun print-usage ()
  (printf "usage: cellular CELLS~%"))

(defun update (src dst)
  (declare (type simple-bit-vector src dst *update-rule*))
  (loop for i from 1 below (- (length src) 1) do
    (let ((x__ (aref src (- i 1)))
          (_x_ (aref src i))
          (__x (aref src (+ i 1))))
      (setf (aref dst i)
            (aref
             ;; #*01111000 ; rule-30
             ;; #*01011010 ; rule-90
             ;; #*00011101 ; rule-184
             #*01110110 ; rule-30
             (+ (* 4 x__) (* 2 _x_) __x))))))

(defun synchronize (domain left-neighbor right-neighbor buffer)
  (declare (type (simple-bit-vector 32) buffer))
  (replace buffer domain :end1 8 :start2 8)
  (replace buffer domain :start1 8 :end1 16
                         :start2 (- (length domain) 16))
  (mpi-waitall
   (mpi-isend buffer left-neighbor  :start  0 :end  8)
   (mpi-isend buffer right-neighbor :start  8 :end 16)
   (mpi-irecv buffer left-neighbor  :start 16 :end 24)
   (mpi-irecv buffer right-neighbor :start 24 :end 32))
  (replace domain buffer :end1 8 :start2 16)
  (replace domain buffer :start1 (- (length domain) 8)
                         :start2 24))

(defun print-domain (domain)
  (loop for active-rank below (mpi-comm-size) do
    (when (= active-rank (mpi-comm-rank))
      (loop for i from 8 below (- (length domain) 8) do
        (if (zerop (aref domain i))
            (write-char #\SPACE)
            (write-char #\X)))
      (finish-output))
    (mpi-barrier))
  (if (= 0 (mpi-comm-rank))
      (write-char #\NEWLINE)
      (finish-output)))

(defun partition-domain (N rank size)
  (let ((chunk-size (max (floor N size) 8)))
    (multiple-value-bind (active-ranks remainder)
        (floor N chunk-size)
      (when (zerop active-ranks)
        (error "Need a domain size of at least ~D cells." chunk-size))
      ;; split the remainder amongst all active ranks
      (multiple-value-bind (bonus big-chunks)
          (floor remainder active-ranks)
        (values
         (+ (* 2 8) ;; the surrounding ghost layers
            (cond
              ((< rank big-chunks) (+ chunk-size bonus 1))
              ((< rank active-ranks) (+ chunk-size bonus))
              (t 0)))
         (mod (- rank 1) active-ranks)
         (mod (+ rank 1) active-ranks))))))

(defun cl-user::main (&optional args)
  (mpi-init)
  (let ((N (or (parse-integer (or (car args) "") :junk-allowed t)
               (print-usage)
               (mpi-finalize)
               (uiop:quit)))
        (iterations 40))
    (multiple-value-bind (n-local left right)
        (partition-domain N (mpi-comm-rank) (mpi-comm-size))
      (when (/= n-local 0)
        (with-static-vectors ((v1 n-local :element-type 'bit)
                              (v2 n-local :element-type 'bit)
                              (sync-buffer 32 :element-type 'bit))
          ;; initialize the domain
          (let ((random-state (make-random-state t)))
            (loop for i below n-local do
              (setf (aref v1 i) (random 2 random-state))))
          ;; now the timesteps
          (loop repeat (floor iterations 8) do
            (loop repeat 4 do
              (update v1 v2)
              (print-domain v2)
              (update v2 v1)
              (print-domain v1))
            ;; sync is only required every 8th iteration
            (synchronize v1 left right sync-buffer)))))
    (mpi-finalize)
    (uiop:quit)))
