
(in-package :mpi-benchmarks)

(defun fast-pingpong (msg-size iterations comm)
  (declare (type (signed-byte 32) msg-size iterations))
  (let ((sbuf (make-array msg-size :element-type '(signed-byte 32)))
        (rbuf (make-array msg-size :element-type '(signed-byte 32)))
        (rank (mpi-comm-rank comm)))
    (loop for i from 0 below msg-size do
      (progn (setf (aref sbuf i) 1)
             (setf (aref rbuf i) 0)))
    (loop repeat 100 do (mpi-barrier comm))
    (let ((t-begin (mpi-wtime)))
      (locally (declare (optimize speed))
        (loop repeat iterations do
             (if
              (evenp rank)
              (let ((target-rank (+ rank 1)))
                (mpi-send sbuf target-rank)
                (mpi-receive rbuf target-rank))
              (let ((target-rank (- rank 1)))
                (declare (fixnum target-rank))
                (mpi-receive rbuf target-rank)
                (mpi-send sbuf target-rank)))))
      (let* ((seconds (- (mpi-wtime) t-begin))
             (usec (* seconds 1000000.0))
             (usec/iter (/ usec iterations)))
        (if (= 0 rank)
            (format t "pingpong(~A bytes) ~A usec/iteration~%" msg-size usec/iter))))))
