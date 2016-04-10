(in-package :cl-mpi-testsuite)

(in-suite mpi-serial-tests)

;;; environment related functions

(test (mpi-wtime)
  (is (<= 0 (mpi-wtime)))
  (is (<= 0 (mpi-wtick))))

(test (mpi-init)
  "MPI Initialization."
  (mpi-init)
  (is (mpi-initialized))
  (is (not (mpi-finalized))))

(test (processor-name :depends-on mpi-init)
  "The function mpi-get-processor-name should return a string describing the
  current processor in use."
  (let ((processor-name (mpi-get-processor-name)))
    (is (stringp processor-name))
    (is (plusp (length processor-name)))))

(test (serial-groups :depends-on mpi-init)
  "MPI group management functions."
  (let* ((size (mpi-comm-size))
         (all-procs (mpi-comm-group +mpi-comm-world+))
         (first (mpi-group-incl all-procs 0))
         (all-but-first (mpi-group-excl all-procs 0))
         (evens (mpi-group-incl all-procs `(0 ,(- size 1) 2)))
         (odds  (if (> size 1)
                    (mpi-group-excl all-procs `(1 ,(- size 1) 2))
                    (mpi-group-incl all-procs))))
    (is (= size (mpi-group-size all-procs)))
    (is (= 1 (mpi-group-size first)))
    (is (= (- size 1) (mpi-group-size all-but-first)))
    (is (= (ceiling size 2) (mpi-group-size evens)))
    (is (= (floor size 2) (mpi-group-size odds)))
    (mpi-group-free all-procs first all-but-first odds evens)))

(test (mpi-context :depends-on mpi-init)
  (is (mpi-comm-free (mpi-comm-dup)))
  (let ((c1 *standard-communicator*)
        (c2 (mpi-comm-dup *standard-communicator*))
        (c3 (mpi-comm-create (mpi-comm-group *standard-communicator*)
                             :comm *standard-communicator*)))
    (unwind-protect
         (is (= (mpi-comm-size c1)
                (mpi-comm-size c2)
                (mpi-comm-size c3)))
      (mpi-comm-free c2)
      (mpi-comm-free c3))))

;;; point to point communication

(test (serial-mpi-sendrecv :depends-on mpi-context)
  (with-fresh-mpi-context
    (let ((self (mpi-comm-rank)))
      ;; send a array containing 10 zeros
      (with-static-vectors ((src 10 :element-type 'double-float
                                    :initial-element 0.0d0)
                            (dst 10 :element-type 'double-float
                                    :initial-element 1.0d0))
        (mpi-sendrecv src self dst self :send-tag 42 :recv-tag 42)
        (is (every #'zerop dst)))
      ;; swap the latter 10 elements of two buffers
      (with-static-vectors ((ones 20 :element-type 'double-float
                                     :initial-element 1.0d0)
                            (temp 10 :element-type 'double-float
                                     :initial-element 0.0d0)
                            (twos 20 :element-type 'double-float
                                     :initial-element 2.0d0))
        (mpi-sendrecv ones self temp self :send-start 10 :send-end 20)
        (mpi-sendrecv twos self ones self :send-start 10 :send-end 20
                                    :recv-start 10 :recv-end 20)
        (mpi-sendrecv temp self twos self :recv-start 10 :recv-end 20)
        (is (and (every (lambda (x) (= x 1.0d0)) (subseq ones 0 10))
                 (every (lambda (x) (= x 2.0d0)) (subseq ones 10 20))))
        (is (and (every (lambda (x) (= x 2.0d0)) (subseq twos 0 10))
                 (every (lambda (x) (= x 1.0d0)) (subseq twos 10 20))))))))

(test (serial-mpi-isend :depends-on mpi-context)
  (with-fresh-mpi-context
    (let ((self (mpi-comm-rank)))
      (loop
        for (mode size)
          in '((:basic 1)
               (:basic 100)
               (:basic 1000000)
               (:buffered 10)
               (:synchronous 100)
               (:ready 1)
               (:ready 1000000))
        do
           (with-static-vectors ((src size :element-type 'double-float
                                           :initial-element 0.0d0)
                                 (dst size :element-type 'double-float
                                           :initial-element 1.0d0))
             (mpi-waitall
              (mpi-irecv dst self)
              (mpi-isend src self :mode mode))
             (is (every #'zerop dst)
                 (format nil "Error during ~s MPI-ISEND of ~d bytes."
                         mode (* 8 size))))))))

(test (serial-mpi-probe :depends-on mpi-context)
  (with-fresh-mpi-context
    (let ((self (mpi-comm-rank)))
      (with-static-vectors ((src 3 :element-type 'double-float)
                            (dst 3 :element-type 'double-float))
        (let ((request (mpi-isend src self :tag 10)))
          (is (= (* 3/8 (cl-mpi::bits-per-element src))
                 (mpi-probe self :tag 10)))
          (mpi-waitall
           request
           (mpi-irecv dst self :tag 10)))))))
