;;; all participating processes pass a given message in a circle until it
;;; reaches again the original sender (here rank 0)

(in-package :cl-user)

(defpackage :cl-mpi/examples/ring
  (:use :cl :alexandria :cl-mpi :static-vectors)
  (:export #:main))
(in-package :cl-mpi/examples/ring)

(defun printf (fmt &rest args)
  (format t "rank ~d: " (mpi-comm-rank))
  (apply #'format t fmt args)
  (finish-output))

(defun main (&optional arg)
  (declare (ignorable arg))
  (mpi-init)
  (let* ((message "foobar")
         (rank (mpi-comm-rank))
         (size (mpi-comm-size))
         (left-neighbor  (mod (- rank 1) size))
         (right-neighbor (mod (+ rank 1) size)))
    (with-static-vector (buffer (length message)
                                :element-type 'character)
      (when (= 0 rank) (printf "sending ~S~%" message))
      (mpi-barrier)
      (cond ((= 0 rank)
             ;; rank null must use the nonblocking versions MPI-ISEND and
             ;; MPI-IRECV, otherwise in the trivial case of only a single
             ;; process (= (mpi-comm-size) 1), both right-neighbor and
             ;; left-neighbor will be zero and we have a potential
             ;; deadlock. Alternatively one could use MPI-SENDRECV.
             (mpi-waitall
              (mpi-isend message right-neighbor)
              (mpi-irecv buffer left-neighbor))
             (printf "received ~S~%" buffer))
            (t
             (mpi-recv buffer left-neighbor)
             (printf "received ~S~%" buffer)
             (mpi-send buffer right-neighbor)))))
  (mpi-finalize)
  (uiop:quit))
