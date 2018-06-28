;; A simple benchmark, where a message is sent back and forth between pairs
;; of processors

(in-package :cl-user)

(defpackage :cl-mpi/examples/pingpong
  (:use :cl :alexandria :cl-mpi :static-vectors)
  (:export #:main))
(in-package :cl-mpi/examples/pingpong)

(defun printf (fmt &rest args)
  (format t "rank ~d: " (mpi-comm-rank))
  (apply #'format t fmt args)
  (finish-output))

(defun die (fmt &rest args)
  (apply #'printf fmt args)
  (mpi-finalize)
  (uiop:quit))

(defun target-rank ()
  (when (oddp (mpi-comm-size))
    (die "pingpong requires an even number of processors~%")
    (mpi-finalize)
    (uiop:quit))
  (let ((rank (mpi-comm-rank)))
    (cond ((evenp rank) (1+ rank))
          ((oddp rank) (1- rank)))))

(defun pingpong (&rest message-sizes)
  (with-static-vector (buffer (apply #'max message-sizes))
    (loop for message-size in message-sizes
          with target = (target-rank) do
            (let ((iterations (ceiling 100000000 (+ message-size 1000)))
                  (tbegin (mpi-wtime)))
              (loop repeat iterations do
                (cond ((evenp target)
                       (mpi-send buffer target :end message-size)
                       (mpi-recv buffer target :end message-size))
                      ((oddp target)
                       (mpi-recv buffer target :end message-size)
                       (mpi-send buffer target :end message-size)))
                ;; in case you want to compare the performance of cl-mpi
                ;; with low level CFFI calls, here is what the latter would
                ;; look like. (Spoiler: 100 nanoseconds, so dont bother)
                ;;
                ;; (cond ((evenp target)
                ;;        (mpi::%mpi-send ptr count +mpi-byte+ target 0 comm)
                ;;        (mpi::%mpi-recv ptr count +mpi-byte+ target 0 comm +mpi-status-ignore+))
                ;;       ((oddp target)
                ;;        (mpi::%mpi-recv ptr count +mpi-byte+ target 0 comm +mpi-status-ignore+)
                ;;        (mpi::%mpi-send ptr count +mpi-byte+ target 0 comm)))
                    )
              (let ((usec (* 1000000.0d0 (- (mpi-wtime) tbegin))))
                (when (= (mpi-comm-rank) 0)
                  (printf "~9D bytes ~12,4F usec/msg ~8,2F MB/sec~%"
                          message-size
                          (/ usec iterations 2)
                          (/ (* message-size iterations 2) usec))))))))

(defun main (&optional args)
  (mpi-init)
  (let ((parsed-args (mapcar
                      (lambda (arg)
                        (or (parse-integer arg :junk-allowed t)
                            (die "pingpong [MSG-SIZE]*~%")))
                      args))
        (default-args (loop for i below 30 collect (expt 2 i))))
    (apply #'pingpong (or parsed-args default-args))
    (mpi-finalize)
    (uiop:quit)))
