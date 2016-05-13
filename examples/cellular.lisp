#!/bin/sh
":" ; exec cl-launch -Q -s cl-mpi -E main -X -- "$0" "$@" || exit || echo " -*- mode: Lisp -*- "

;;; A one dimensional cellular automaton.
;;;
;;; This example is particularly interesting as it works with bit
;;; vectors. MPI supports only messages that are aligned to byte
;;; boundaries, so special care must be taken to have each message properly
;;; aligned.

(in-package :cl-user)

(defpackage :cl-mpi/examples/cellular
  (:use :cl :alexandria :cl-mpi :static-vectors))
(in-package :cl-mpi/examples/cellular)

(defun printf (fmt &rest args)
  (format t "rank ~d: " (mpi-comm-rank +mpi-comm-world+))
  (apply #'format t fmt args)
  (finish-output))

(defun print-usage ()
  (printf "usage: cellular CELLS~%"))

(defun update (src dst)
  (declare (type simple-bit-vector src dst)
           (optimize (speed 3) (debug 0) (safety 0)))
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

(defun synchronize (domain lsend lrecv rsend rrecv)
  (declare (type simple-bit-vector domain)
           (type (simple-bit-vector 8) lsend lrecv rsend rrecv))
  (replace lsend domain :start2 8)
  (replace rsend domain :start2 (- (length domain) 16))
  (let ((left (mod (- (mpi-comm-rank) 1) (mpi-comm-size)))
        (right (mod (+ (mpi-comm-rank) 1) (mpi-comm-size))))
    (mpi-waitall
     (mpi-isend lsend left)
     (mpi-isend rsend right)
     (mpi-irecv lrecv left)
     (mpi-irecv rrecv right)))
  (replace domain lrecv :start1 0)
  (replace domain rrecv :start1 (- (length domain) 8)))

(defun byte-align (n)
  ;; round up to a power of eight, see the book "Hacker's Delight"
  (logand (+ 7 n) -8))

(defun print-domain (domain)
  (declare (type simple-bit-vector domain))
  (let ((count (- (length domain) 16)) ; strip the ghost layer
        (size (mpi-comm-size))
        (rank (mpi-comm-rank))
        (root 0) (meta-tag 0) (data-tag 1))
    (with-static-vector (metadata 1 :element-type '(unsigned-byte 64)
                                    :initial-element count)
      ;; have each rank (including ROOT) send his data to ROOT
      (let ((meta-request (mpi-isend metadata root :tag meta-tag))
            (data-request (mpi-isend domain root
                                     :tag data-tag
                                     :start 8
                                     :end (byte-align (+ 8 count)))))
        (when (= rank root)
          ;; process all ranks sequentially
          (with-static-vectors ((meta-recv 1 :element-type '(unsigned-byte 64))
                                ;; note that that ROOT has the longest domain!
                                (data (length domain) :element-type 'bit))
            (loop for source below size do
              (mpi-recv meta-recv source :tag meta-tag)
              (let ((N (aref meta-recv 0)))
                (mpi-recv data source :start 0 :end (byte-align N) :tag data-tag)
                  ;; print the data received from SOURCE
                (loop for i below N do
                  (if (zerop (aref data i))
                      (write-char #\SPACE)
                      (write-char #\X))))))
          (write-char #\NEWLINE)
          (finish-output))
        ;; wait for the send requests to complete
        (mpi-waitall meta-request data-request)))))

(defun partition-domain (N)
  (let* ((size (mpi-comm-size))
         (rank (mpi-comm-rank))
         (chunk-size (max (floor N size) 8)))
    (multiple-value-bind (active-ranks remainder)
        (floor N chunk-size)
      (when (zerop active-ranks)
        (error "Need a domain size of at least ~D cells." chunk-size))
      ;; split the remainder amongst all active ranks
      (multiple-value-bind (bonus big-chunks)
          (floor remainder active-ranks)
        (values
         ;; the size of the rank local domain
         (cond
           ((< rank big-chunks) (+ chunk-size bonus 1))
           ((< rank active-ranks) (+ chunk-size bonus))
           (t 0))
         ;; the communicator containing only active ranks
         (mpi-comm-create
          (mpi-group-incl (mpi-comm-group) `(0 ,(- active-ranks 1)))))))))

(defmacro with-partitioning ((N-local N) &body body)
  ;; binding *standard-communicator* so that consecutive calls
  ;; to (MPI-COMM-SIZE) and (MPI-COMM-RANK) consider only
  ;; the active ranks determined by PARTITION-DOMAIN
  (assert (symbolp N-local))
  `(multiple-value-bind (,N-local *standard-communicator*)
       (partition-domain ,N)
     ,@body))

(defun cellular (N iterations)
  (with-partitioning (N-local N)
    (printf "simulating ~D cells~%" N-local)
    (unless (zerop N-local)
      (let ((domain-size (+ (* 2 8) ; include ghost layers
                            N-local))
            (random-state (make-random-state t)))
        (with-static-vectors ((v1 domain-size :element-type 'bit)
                              (v2 domain-size :element-type 'bit)
                              (b1 8 :element-type 'bit)
                              (b2 8 :element-type 'bit)
                              (b3 8 :element-type 'bit)
                              (b4 8 :element-type 'bit))
          ;; initialize the domain
          (loop for i below domain-size do
            (setf (aref v1 i) (random 2 random-state)))
          (print-domain v1)
          (synchronize v1 b1 b2 b3 b4)
          ;; now the iterations
          (loop repeat (floor iterations 8) do
            (loop repeat 4 do
              (print-domain v1)
              (update v1 v2)
              (print-domain v2)
              (update v2 v1))
            ;; sync is only required every 8th iteration
            (synchronize v1 b1 b2 b3 b4)))))))

(defun cl-user::main (&optional args)
  (mpi-init)
  (let ((N (or (parse-integer (or (car args) "") :junk-allowed t)
               (print-usage)
               (mpi-finalize)
               (uiop:quit)))
        (iterations 80))
    (cellular N iterations)
    ;(printf "done~%")
    (mpi-finalize)
    (uiop:quit)))
