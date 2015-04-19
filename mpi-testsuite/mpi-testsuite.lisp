(defpackage :mpi-testsuite
  (:use :cl :mpi :5am :uiop :cffi :static-vectors))

(in-package :mpi-testsuite)

(def-suite mpi-testsuite :description "All MPI related tests.")

(in-suite mpi-testsuite)

(test (mpi-init)
      "MPI Initialization"
      (is (mpi-initialized) "failed to initialize MPI (or cl-mpi is broken)"))

(test (error-handling :depends-on mpi-init)
      "Test whether MPI errors are properly handled"
      (is (stringp (mpi-error-string 0))))

(test (size-and-rank :depends-on mpi-init)
      "Checking whether it is possible to determine size and rank"
      (let ((size (mpi-comm-size))
            (rank (mpi-comm-rank)))
        (is (> size 0) "Invalid size of +mpi-comm-world+")
        (is (> size rank -1) "Invalid MPI rank")))

(test (processor-name :depends-on mpi-init)
      "The function mpi-get-processor-name should return a string describing the
  current processor in use"
      (let ((processor-name (mpi-get-processor-name)))
        (is (stringp processor-name))
        (is (plusp (length processor-name)))))

(test (datatypes :depends-on mpi-init)
      "Check whether the CFFI types and MPI types actually correspond"
      (let ((mpi-types
              (list
               +mpi-char+ +mpi-unsigned-char+
               +mpi-short+ +mpi-unsigned-short+
               +mpi-int+ +mpi-unsigned+
               +mpi-long+ +mpi-unsigned-long+
               +mpi-long-long-int+ +mpi-unsigned-long-long+
               +mpi-float+ +mpi-double+)))
        (is (every #'=
                   (mapcar #'mpi-type-size mpi-types)
                   (mapcar #'(lambda (mpi-type)
                               (foreign-type-size
                                (mpi-type-to-cffi-type mpi-type)))
                           mpi-types)))))

(test (mpi-barrier :depends-on mpi-init)
      "synchronize all processes with multiple MPI barriers"
      (loop for i from 0 below 10 do (mpi-barrier)))

(test (serial-groups :depends-on size-and-rank)
      "MPI group tests that can be run on a single process"
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
      "Is there more than one MPI process"
      (let ((size (mpi-comm-size))
            (rank (mpi-comm-rank)))
        (is (> size 1) "More than one MPI process is required for most MPI tests")
        ;; discard the output of all but one MPI process
        (unless (zerop rank)
          (setf *test-dribble* (make-broadcast-stream)))))

(test (mpi-ring :depends-on parallel)
      "Send a Common Lisp datastructure through all nodes"
      (let ((rank (mpi-comm-rank))
            (size (mpi-comm-size))
            (buffer (make-static-vector 7 :element-type 'character
                                          :initial-element #\!))
            (message (make-static-vector 6 :element-type 'character
                                           :initial-contents "foobar")))
        (let ((left-neighbor  (mod (- rank 1) size))
              (right-neighbor (mod (+ rank 1) size)))
          (cond ((= 0 rank)
                 (mpi-send message right-neighbor)
                 (mpi-receive buffer left-neighbor)
                 (is (string= "foobar!" buffer)))
                (t
                 (mpi-receive buffer left-neighbor)
                 (mpi-send buffer right-neighbor))))
        (free-static-vector buffer)
        (free-static-vector message)))

(test (mpi-sendreceive :depends-on parallel)
      "Send a Common Lisp datastructure through all nodes"
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
          (mpi-sendreceive my-buffer right-neighbor left-buffer left-neighbor)
          (mpi-sendreceive my-buffer left-neighbor right-buffer right-neighbor)
          (is (= (aref left-buffer 0) left-neighbor))
          (is (= (aref right-buffer 0) right-neighbor))
          (free-static-vector left-buffer)
          (free-static-vector right-buffer)
          (free-static-vector my-buffer))))
