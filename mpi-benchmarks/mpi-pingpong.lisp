
(in-package :mpi-benchmarks)

(defun pingpong (msg-size buf iterations)
  (declare (type (signed-byte 32) msg-size iterations)
           (type (simple-array (unsigned-byte 8) (*)) buf))
  (let ((rank (mpi-comm-rank)))
    (loop repeat 100 do (mpi-barrier))
    (let ((t-begin (mpi-wtime)))
      (locally (declare (optimize speed))
        (loop repeat iterations do
             (if
              (evenp rank)
              (let ((target-rank (+ rank 1)))
                (mpi-send buf target-rank :end msg-size)
                (mpi-recv buf target-rank :end msg-size))
              (let ((target-rank (- rank 1)))
                (declare (fixnum target-rank))
                (mpi-recv buf target-rank :end msg-size)
                (mpi-send buf target-rank :end msg-size)))))
      (let* ((seconds (- (mpi-wtime) t-begin))
             (usec (* seconds 1000000.0))
             (usec/iter (/ usec iterations))
             (mb/sec (/ (* msg-size iterations 2) seconds 1000000)))
        (if (= 0 rank)
            (format t "pingpong(~9D bytes) ~10,4F usec/iteration ~8,2F MBytes/sec~%"
                    msg-size usec/iter mb/sec))))))
