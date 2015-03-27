(defpackage :mpi-testsuite
  (:use :cl :mpi :5am :uiop :cffi))

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
    (is (> size 0) "Invalid size of *mpi-comm-world*")
    (is (> size rank -1) "Invalid MPI rank")))

(test (processor-name :depends-on mpi-init)
  "The function mpi-get-processor-name should return a string describing the
  current processor in use"
  (let ((processor-name (mpi-get-processor-name)))
    (is (stringp processor-name))
    (is (plusp (length processor-name)))))

(test (mpi-barrier :depends-on mpi-init)
  "synchronize all processes with multiple MPI barriers"
  (loop for i from 0 below 10 do (mpi-barrier)))

(test (serial-groups :depends-on mpi-init)
  "MPI group tests that can be run on a single process"
  (let* ((all-procs (mpi-comm-group *mpi-comm-world*))
         (first-proc (mpi-group-select-from all-procs 0)))
    (is (< 0 (mpi-group-size all-procs)))
    (is (= 1 (mpi-group-size first-proc)))))

(test (parallel :depends-on size-and-rank)
  "Is there more than one MPI process"
  (let ((size (mpi-comm-size))
        (rank (mpi-comm-rank)))
    (is (> size 1) "More than one MPI process is required for most MPI tests")
    (unless (zerop rank) ;; discard the output of all but one MPI process
      (setf *test-dribble* (make-broadcast-stream)))))

(test (mpi-ring-foreign :depends-on parallel)
  "Send a foreign array through all nodes"
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size))
        (message-length 42))
    (let ((left-neighbor  (mod (- rank 1) size))
          (right-neighbor (mod (+ rank 1) size))
          (tag 42))
      (with-foreign-objects
          ((send-buffer :int message-length)
           (recv-buffer :int message-length))
        ;; initialize send buffer with ascending integers
        (loop for i from 0 below message-length do
             (setf (mem-aref send-buffer :int i) i))
        (cond ((= 0 rank)
               (mpi:mpi-send-foreign right-neighbor send-buffer message-length :int :tag tag)
               (multiple-value-bind (source tag)
                   (mpi:mpi-receive-foreign left-neighbor recv-buffer message-length :int :tag tag)
                 (is (= source left-neighbor))
                 (is (= tag tag))
                 (is (loop for i from 0 below message-length
                        unless (= (mem-aref recv-buffer :int i) i)
                        do (return nil)
                        finally (return t)))))
              (t
               (mpi:mpi-receive-foreign left-neighbor recv-buffer message-length :int :tag tag)
               (mpi:mpi-send-foreign right-neighbor send-buffer message-length :int :tag tag)))))))

(test (mpi-ring :depends-on parallel)
  "Send a Common Lisp datastructure through all nodes"
  (force-output)
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size)))
    (let ((left-neighbor  (mod (- rank 1) size))
          (right-neighbor (mod (+ rank 1) size)))
      (cond ((= 0 rank)
             (mpi:mpi-send right-neighbor '(3 different "elements"))
             (is (equalp '(3 different "elements") (mpi:mpi-receive left-neighbor))))
            (t
             (mpi:mpi-send right-neighbor
                           (mpi:mpi-receive left-neighbor)))))))

(test (mpi-sendreceive-foreign :depends-on parallel)
  "Send a foreign array through all nodes via mpi-sendreceive-foreign"
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size))
        (message-length 42))
    (let ((left-neighbor  (mod (- rank 1) size))
          (right-neighbor (mod (+ rank 1) size))
          (tag 42))
      (with-foreign-objects
          ((send-buffer :int message-length)
           (recv-buffer :int message-length))
        ;; initialize send buffer with ascending integers
        (loop for i from 0 below message-length do
          (setf (mem-aref send-buffer :int i) i))
        (mpi-sendreceive-foreign right-neighbor send-buffer message-length :int
                                 left-neighbor  recv-buffer message-length :int
                                 :send-tag tag :recv-tag tag)
        (is (loop for i from 0 below message-length
                        unless (= (mem-aref recv-buffer :int i) i)
                        do (return nil)
                        finally (return t)))))))
