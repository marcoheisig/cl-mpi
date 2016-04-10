(in-package :cl-mpi-test-suite)

(in-suite mpi-stress-tests)

(test (serial-mpi-request-stress-test)
  "Create and use a lot of MPI-REQUEST objects."
  (mpi-init)
  (let ((self (mpi-comm-rank))
        (errors? nil))
    (with-static-vectors ((v1 4 :element-type 'single-float
                                :initial-element 1.0)
                          (v2 4 :element-type 'single-float
                                :initial-element 2.0))
      (loop repeat 1000000 do
        (progn
          (mpi-waitall ;; swap the contents of v1 and v2
           (mpi-isend v1 self)
           (mpi-isend v2 self)
           (mpi-irecv v2 self)
           (mpi-irecv v1 self))
          (rotatef v1 v2)
          (unless (and (every (lambda (x) (= x 1.0)) v1)
                       (every (lambda (x) (= x 2.0)) v2))
            (setf errors? t)))))
    (is (not errors?))))
