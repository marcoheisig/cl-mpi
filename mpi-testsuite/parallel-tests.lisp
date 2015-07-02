(in-package :mpi-testsuite)
(in-suite mpi-testsuite)

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

(test (mpi-ring :depends-on parallel)
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

(test (mpi-sendrecv :depends-on parallel)
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

(test (nonblocking :depends-on parallel)
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
