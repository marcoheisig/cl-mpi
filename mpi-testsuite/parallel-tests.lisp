(in-package :mpi-testsuite)

(in-suite mpi-parallel-tests)

(defun team-partner ()
  "Group all processes in teams of two. Return the rank of the partner."
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size)))
    (cond
      ((and (oddp size)
            (>= rank (- size 1)))
       +mpi-proc-null+)
      ((evenp rank)
       (+ rank 1))
      ((oddp rank)
       (- rank 1)))))

;;; The parallel tests must form a serial dependency chain so that their order
;;; is deterministic. Otherwise the parallel testsuite can deadlock.

(test (mpi-barrier)
  "synchronize all processes with multiple MPI barriers."
  (loop for i from 0 below 10 do (mpi-barrier)))

(test (mpi-ring :depends-on mpi-barrier)
  "Send a Common Lisp datastructure through all nodes."
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size))
        (buffer (make-static-vector 7 :element-type 'character
                                      :initial-element #\SPACE))
        (message (make-static-vector 7 :element-type 'character
                                       :initial-contents "foobar!")))
    (let ((left-neighbor  (mod (- rank 1) size))
          (right-neighbor (mod (+ rank 1) size)))
      (unwind-protect
           (cond ((= 0 rank)
                  (mpi-send message right-neighbor)
                  (mpi-recv buffer left-neighbor)
                  (is (string= "foobar!" buffer)))
                 (t
                  (mpi-recv buffer left-neighbor)
                  (mpi-send buffer right-neighbor)))
        (free-static-vector buffer)
        (free-static-vector message)))))

(test (mpi-sendrecv :depends-on mpi-ring)
  "Send a Common Lisp datastructure through all nodes using mpi-sendrecv."
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
             (mpi-sendrecv my-buffer right-neighbor left-buffer left-neighbor)
             (mpi-sendrecv my-buffer left-neighbor right-buffer right-neighbor))
        (is (= (aref left-buffer 0) left-neighbor))
        (is (= (aref right-buffer 0) right-neighbor))
        (free-static-vector left-buffer)
        (free-static-vector right-buffer)
        (free-static-vector my-buffer)))))

(test (send-subsequence :depends-on mpi-sendrecv)
  "Send only a subsequence of an array"
  (let* ((partner (team-partner))
         (recvbuf (make-static-vector 11 :element-type 'character
                                         :initial-element #\SPACE))
         (sendbuf (make-static-vector 9 :element-type 'character
                                        :initial-contents "+foobar!+")))
    (unwind-protect
         (mpi-sendrecv sendbuf partner
                          recvbuf partner
                          :send-start 1 :send-end 8
                          :recv-start 2 :recv-end 9)
      (is (string= "  foobar!  " recvbuf))
      (free-static-vector recvbuf)
      (free-static-vector sendbuf))))

(test (mpi-broadcast :depends-on send-subsequence)
  "Use mpi-broadcast to broadcast a single number."
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size)))
    (with-static-vector (buffer 1 :element-type 'double-float)
      (let ((root (- size 1))
            (message (coerce pi 'double-float)))
        (if (= rank root)
            (setf (aref buffer 0) message))
        (mpi-broadcast buffer root)
        (is (= (aref buffer 0) message))))))

(test (mpi-allgather :depends-on mpi-broadcast)
  "Use mpi-allgather to generate a vector of all ranks."
  (let ((rank (mpi-comm-rank))
        (size (mpi-comm-size)))
    (with-static-vector (recv-array size :element-type '(signed-byte 32)
                                         :initial-element 0)
      (with-static-vector (send-array 1 :element-type '(signed-byte 32)
                                        :initial-element rank)
        (mpi-allgather send-array recv-array)
        (is (loop for i below size
                  when (/= (aref recv-array i) i) do
                    (return nil)
                  finally
                     (return t)))))))

(test (nonblocking :depends-on mpi-allgather)
  "Nonblocking communication with MPI-I[SEND,RECV], MPI-WAIT and
MPI-WAITALL"
  (let ((partner (team-partner)))
    (with-static-vector (recvbuf 3 :element-type '(unsigned-byte 32)
                                   :initial-element 0)
      (with-static-vector (sendbuf 3 :element-type '(unsigned-byte 32)
                                     :initial-element 1)
        (mpi-waitall (mpi-isend sendbuf partner)
                     (mpi-irecv recvbuf partner))
        (is (equalp recvbuf #(1 1 1))))
      (with-static-vector (sendbuf 3 :element-type '(unsigned-byte 32)
                                     :initial-element 2)
        (mapc #'mpi-wait
              (list (mpi-isend sendbuf partner)
                    (mpi-irecv recvbuf partner)))
        (is (equalp recvbuf #(2 2 2))))
      (with-static-vector (sendbuf 3 :element-type '(unsigned-byte 32)
                                     :initial-element 3)
        (let ((requests (list (mpi-isend sendbuf partner)
                              (mpi-irecv recvbuf partner))))
          (is (not (some #'mpi-null-p requests)))
          (mapc #'mpi-wait requests)
          (is (every #'mpi-null-p requests))
          (is (equalp recvbuf #(3 3 3))))))))

(test (mpi-allreduce :depends-on nonblocking)
      "Perform different reductions with MPI-Allreduce."
      (let ((size (mpi-comm-size)))
        (with-static-vector (source 3 :element-type '(unsigned-byte 64)
                                      :initial-element 1)
          (with-static-vector (dest 3 :element-type '(unsigned-byte 64)
                                      :initial-element 0)
            (mpi-allreduce source dest +mpi-prod+)
            (is (every (lambda (x) (= x 1)) dest))
            (mpi-allreduce source dest +mpi-sum+)
            (is (every (lambda (x) (= x size)) dest))))))

(test (mpi-reduce :depends-on mpi-allreduce)
  "Perform different reductions with MPI-Reduce"
  (let* ((size (mpi-comm-size))
         (rank (mpi-comm-rank))
         (root (- size 1)))
    (with-static-vector (source 3 :element-type 'double-float
                                  :initial-element 1.0d0)
      (with-static-vector (allgood 1 :element-type '(unsigned-byte 8)
                                     :initial-element 0)
        (if (= rank root)
            (with-static-vector (dest 3 :element-type 'double-float
                                        :initial-element 0d0)
              (mpi-reduce source dest +mpi-max+ root)
              (when (every (lambda (x) (= x 1.0d0)) dest)
                (setf (aref allgood 0) 1)))
            (mpi-reduce source nil +mpi-max+ root))
        (mpi-broadcast allgood root)
        (is (plusp (aref allgood 0)))))))
