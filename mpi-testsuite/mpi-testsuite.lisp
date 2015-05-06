(defpackage :mpi-testsuite
  (:use :cl :mpi :5am :uiop :cffi :static-vectors))

(in-package :mpi-testsuite)

(def-suite mpi-testsuite :description "All MPI related tests.")

(in-suite mpi-testsuite)

(test (mpi-init)
  "MPI Initialization."
  (is (mpi-initialized) "failed to initialize MPI (or cl-mpi is broken)"))

(test (size-and-rank :depends-on mpi-init)
  "Check whether it is possible to determine size and rank."
  (let ((size (mpi-comm-size))
        (rank (mpi-comm-rank)))
    (is (> size 0) "Invalid size of +mpi-comm-world+")
    (is (> size rank -1) "Invalid MPI rank")))

(test (processor-name :depends-on mpi-init)
  "The function mpi-get-processor-name should return a string describing the
  current processor in use."
  (let ((processor-name (mpi-get-processor-name)))
    (is (stringp processor-name))
    (is (plusp (length processor-name)))))

(test (mpi-barrier :depends-on mpi-init)
  "synchronize all processes with multiple MPI barriers."
  (loop for i from 0 below 10 do (mpi-barrier)))

(test (serial-groups :depends-on size-and-rank)
  "MPI group tests that can be run on a single process."
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

(test (parallel :depends-on size-and-rank)
  "Check whether there is more than one MPI process."
  (let ((size (mpi-comm-size))
        (rank (mpi-comm-rank)))
    (is (> size 1) "More than one MPI process is required for most MPI tests")
    ;; discard the output of all but one MPI process
    (unless (zerop rank)
      (setf *test-dribble* (make-broadcast-stream)))))

(test (mpi-ring :depends-on parallel)
  "Send a Common Lisp datastructure through all nodes."
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size))
        (buffer (make-static-vector 11 :element-type 'character
                                       :initial-element #\SPACE))
        (message (make-static-vector 9 :element-type 'character
                                       :initial-contents "+foobar!+")))
    (let ((left-neighbor  (mod (- rank 1) size))
          (right-neighbor (mod (+ rank 1) size)))
      (unwind-protect
           (cond ((= 0 rank)
                  (mpi-send message right-neighbor :start 1 :end 8)
                  (mpi-receive buffer left-neighbor :start 2 :end 9)
                  (is (string= "  foobar!  " buffer)))
                 (t
                  (mpi-receive buffer left-neighbor :start 2 :end 9)
                  (mpi-send buffer right-neighbor :start 2 :end 9)))
        (free-static-vector buffer)
        (free-static-vector message)))))

(test (mpi-sendreceive :depends-on parallel)
  "Send a Common Lisp datastructure through all nodes using mpi-sendreceive."
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size)))
    (let ((left-neighbor  (mod (- rank 1) size))
          (right-neighbor (mod (+ rank 1) size))
          (left-buffer  (make-static-vector 1 :element-type '(unsigned-byte 64)
                                              :initial-element 0))
          (right-buffer (make-static-vector 1 :element-type '(unsigned-byte 64)
                                              :initial-element 0))
          (my-buffer    (make-static-vector 1 :element-type '(unsigned-byte 64)
                                              :initial-element rank)))
      (unwind-protect
           (progn
             (mpi-sendreceive my-buffer right-neighbor left-buffer left-neighbor)
             (mpi-sendreceive my-buffer left-neighbor right-buffer right-neighbor))
        (is (= (aref left-buffer 0) left-neighbor))
        (is (= (aref right-buffer 0) right-neighbor))
        (free-static-vector left-buffer)
        (free-static-vector right-buffer)
        (free-static-vector my-buffer)))))

(test (mpi-broadcast :depends-on parallel)
  "Use mpi-broadcast to broadcast a single number."
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size)))
    (let ((buffer (make-static-vector 1 :element-type 'double-float))
          (root (- size 1))
          (message (coerce pi 'double-float)))
      (if (= rank root)
          (setf (aref buffer 0) message))
      (unwind-protect (mpi-broadcast buffer root)
        (is (= (aref buffer 0) message))
        (free-static-vector buffer)))))

(test (mpi-allgather :depends-on parallel)
  "Use mpi-allgather to generate a vector of all ranks."
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size)))
    (let ((recv-array (make-static-vector size :element-type '(signed-byte 32)
                                               :initial-element 0))
          (send-array (make-static-vector 1 :element-type '(signed-byte 32)
                                            :initial-element rank)))
      (unwind-protect (mpi-allgather send-array recv-array)
        (is (loop for i below size
                  when (/= (aref recv-array i) i) do
                       (return nil)
                  finally
                     (return t)))
        (free-static-vector recv-array)
        (free-static-vector send-array)))))
