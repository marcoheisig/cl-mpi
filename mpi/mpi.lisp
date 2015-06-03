#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

Convenient wrappers for most MPI functions

Copyright (c) 2008,2009  Alex Fukunaga
Copyright (C) 2014,2015  Marco Heisig <marco.heisig@fau.de>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :mpi)

(defun static-vector-mpi-data (vector start end)
  "Return a pointer to the raw memory of the given array, as well as the
corresponding mpi-type and length.

WARNING: If ARRAY is somehow moved in memory (e.g. by the garbage collector),
your code is broken. So better have a look at the STATIC-VECTORS package."
  (declare (type (simple-array * (*)) vector)
           (type (or (integer 0 #.array-total-size-limit) null) start end)
           (values foreign-pointer
                   mpi-datatype
                   (integer 0 #.array-total-size-limit)))
  (let* ((element-size
           #.(let ((cases
                     (loop for type in
                           '(single-float     double-float
                             (signed-byte 1)  (unsigned-byte 1)
                             (signed-byte 2)  (unsigned-byte 2)
                             (signed-byte 4)  (unsigned-byte 4)
                             (signed-byte 8)  (unsigned-byte 8)
                             (signed-byte 16) (unsigned-byte 16)
                             (signed-byte 32) (unsigned-byte 32)
                             (signed-byte 64) (unsigned-byte 64)
                             base-char character)
                           collect
                           `((simple-array ,type (*))
                             ,(%array-element-size type)))))
               `(etypecase vector
                  ,@cases)))
         (len (length vector))
         (start (if start start len))
         (end (if end end len)))
    (assert (<= 0 start end len))
    (if (integerp element-size)
        (let* ((offset (ceiling (* start element-size)))
               (count (- end start))
               (ptr (static-vector-pointer vector :offset offset)))
          (ecase element-size
            (1 (values ptr +mpi-uint8-t+ count))
            (2 (values ptr +mpi-uint16-t+ count))
            (4 (values ptr +mpi-uint32-t+ count))
            (8 (values ptr +mpi-uint64-t+ count))))
        (let* ((offset (ceiling (* start element-size)))
               (count (- end start))
               (ptr (static-vector-pointer vector :offset offset)))
          (ecase element-size
            (1/8 (values ptr +mpi-uint8-t+ (ceiling count 8)))
            (1/4 (values ptr +mpi-uint8-t+ (ceiling count 4)))
            (1/2 (values ptr +mpi-uint8-t+ (ceiling count 2))))))))

(defmacro with-foreign-results (bindings &body body)
  "Evaluate body as with WITH-FOREIGN-OBJECTS, but afterwards convert them to
  lisp objects and return them via VALUES."
  `(with-foreign-objects ,bindings
     ,@body
     (values
      ,@(loop for binding in bindings
              collect `(mem-ref ,@binding)))))

(defun mpi-init ()
  "In CL-MPI, MPI-INIT is automatically called called at load-time and
subsequent calls have no effect."
  (unless (mpi-initialized)
    (%mpi-init (null-pointer) (null-pointer))
    ;; by default MPI reacts to each failure by crashing the process. This is
    ;; not the Lisp way of doing things. The following call makes error
    ;; non-fatal in most cases.
    (%mpi-comm-set-errhandler +mpi-comm-world+ +mpi-errors-return+)))

(defun mpi-initialized ()
  "Returns true if MPI_INIT has been called and nil otherwise.
   This routine may be used to determine whether MPI-INIT has been called. It
   is the only routine that may be called before MPI-INIT is called."
  (with-foreign-results ((flag :boolean))
    (%mpi-initialized flag)))

(defun mpi-abort(&key (comm *standard-communicator*) (errcode -1))
  "This routine makes a 'best attempt' to abort all tasks in the group of
comm. This function does not require that the invoking environment take any
action with the error code. However, a Unix or POSIX environment should handle
this as a return errorcode from the main program or an abort(errorcode)."
  (%mpi-abort comm errcode))

(defun mpi-get-processor-name ()
  "This routine returns the name of the processor on which it was called at
the moment of the call.  The name is a character string for maximum
flexibility. From this value it must be possible to identify a specific piece
of hardware; possible values include 'processor 9 in rack 4 of mpp.cs.org' and
'231' (where 231 is the actual processor number in the running homogeneous
system)."
  (with-foreign-object (namelen :int)
    (with-foreign-pointer (processor-name +mpi-max-processor-name+)
      (%mpi-get-processor-name processor-name namelen)
      (values (foreign-string-to-lisp
               processor-name
               :count (mem-aref namelen :int))))))

(defun mpi-error-string (errorcode)
  "Convert the given errorcode to a human readable error message"
  (with-foreign-object (strlen :int)
    (with-foreign-pointer (error-string +mpi-max-error-string+)
      (%mpi-error-string errorcode error-string strlen)
      (values (foreign-string-to-lisp
               error-string
               :count (mem-aref strlen :int))))))

(defun mpi-barrier (&optional (comm *standard-communicator*))
  "MPI-BARRIER blocks the caller until all group members have called it. The
  call returns at any process only after all group members have entered the
  call."
  (%mpi-barrier comm))

(defun mpi-sendreceive (out dest in source
                        &key
                          (send-tag 0)
                          (recv-tag +mpi-any-tag+)
                          (comm *standard-communicator*))
  (declare (type simple-array out in)
           (type (signed-byte 32)
                 dest send-tag source  recv-tag))
  (multiple-value-bind (send-buf send-type send-count)
      (static-vector-mpi-data out 0 nil)
    (multiple-value-bind (recv-buf recv-type recv-count)
        (static-vector-mpi-data in 0 nil)
      (%mpi-sendrecv
       send-buf send-count send-type dest send-tag
       recv-buf recv-count recv-type source recv-tag
       comm +mpi-status-ignore+))))

(defun mpi-send (array dest &key
                              (start 0)
                              (end nil)
                              (tag 0)
                              (comm *standard-communicator*)
                              (mode :basic))
  "Send a given ARRAY to a corresponding MPI-RECEIVE. The arrays passed to
MPI-SEND and MPI-RECEIVE must be of type SIMPLE-ARRAY and have the same
element-type and dimensions. Undefined behaviour occurs if the arrays at
sender and receiver side do not match."
  (declare (type simple-array array)
           (type (signed-byte 32) dest tag)
           (type mpi-comm comm)
           (type (member :basic :buffered :synchronous :ready) mode))
  (let ((send-function
          (ecase mode
            (:basic #'%mpi-send)
            (:buffered #'%mpi-bsend)
            (:synchronous #'%mpi-ssend)
            (:ready #'%mpi-rsend))))
    (multiple-value-bind (ptr type count)
        (static-vector-mpi-data array start end)
      (funcall send-function ptr count type dest tag comm))))

(defun mpi-isend (array dest &key
                               (start 0)
                               (end nil)
                               (tag 0)
                               (comm *standard-communicator*)
                               (mode :basic))
  "A non-blocking variant of MPI-SEND. It returns immediately. To check
  whether the send operation is complete, use MPI-WAIT or MPI-TEST.

WARNING: The caller of MPI-ISEND is responsible that the given array is not
relocated or garbage-collected until the send operation is complete. This can
be achieved by using STATIC-VECTORS or some implementation dependent
mechanism such as sb-sys:with-pinned-objects."
  (declare (type simple-array array)
           (type (signed-byte 32) dest tag)
           (type mpi-comm comm)
           (type (member :basic :buffered :synchronous :ready) mode))
  (let ((send-function
          (ecase mode
            (:basic #'%mpi-isend)
            (:buffered #'%mpi-ibsend)
            (:synchronous #'%mpi-issend)
            (:ready #'%mpi-irsend))))
    (multiple-value-bind (ptr type count)
        (static-vector-mpi-data array start end)
      (with-foreign-results ((request 'mpi-request))
        (funcall send-function ptr count type dest tag comm request)))))

(defun mpi-receive (array source &key
                                   (start 0)
                                   (end nil)
                                   (tag +mpi-any-tag+)
                                   (comm *standard-communicator*))
  (declare (type simple-array array)
           (type (signed-byte 32) source tag)
           (type mpi-comm comm))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end) ;; TODO check the mpi-status
    (%mpi-recv ptr count type source tag comm +mpi-status-ignore+)))

(defun mpi-broadcast (array root &key
                                   (start 0)
                                   (end nil)
                                   (comm *standard-communicator*))
  (declare (type simple-array array)
           (type (signed-byte 32) root)
           (type mpi-comm comm))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end)
    (%mpi-bcast ptr count type root comm)))

(defun mpi-allgather (send-array recv-array &key
                                              (send-start 0)
                                              (send-end nil)
                                              (recv-start 0)
                                              (recv-end nil)
                                              (comm *standard-communicator*))
  (declare (type simple-array send-array recv-array))
  (multiple-value-bind (sendbuf sendtype sendcount)
      (static-vector-mpi-data send-array send-start send-end)
    (multiple-value-bind (recvbuf recvtype recvcount)
        (static-vector-mpi-data recv-array recv-start recv-end)
      (declare (ignore recvcount))
      (%mpi-allgather sendbuf sendcount sendtype
                      recvbuf sendcount recvtype comm))))

(defun mpi-probe (source &key
                           (tag +mpi-any-tag+)
                           (comm *standard-communicator*))
  (with-foreign-object (status '(:struct mpi-status))
    (%mpi-probe source tag comm status)
    (with-foreign-slots ((mpi-tag mpi-error) status (:struct mpi-status))
      (values
       (with-foreign-results ((count :int))
         (%mpi-get-count status +mpi-byte+ count))
       mpi-tag))))

(defun mpi-comm-group (&optional (comm *standard-communicator*))
  (make-instance
   'mpi-group
   :foreign-object
   (with-foreign-results ((newgroup 'mpi-group))
     (%mpi-comm-group comm newgroup))))

(defun mpi-group-size (group)
  (with-foreign-results ((size :int))
    (%mpi-group-size group size)))

(defun mpi-group-rank (group)
  (with-foreign-results ((rank :int))
    (%mpi-group-rank group rank)))

(defun mpi-group-union (group1 group2)
  (with-foreign-results ((newgroup 'mpi-group))
    (%mpi-group-union group1 group2 newgroup)))

(defun mpi-group-intersection (group1 group2)
  (with-foreign-results ((newgroup 'mpi-group))
    (%mpi-group-intersection group1 group2 newgroup)))

(defun mpi-group-difference (group1 group2)
  (with-foreign-results ((newgroup 'mpi-group))
    (%mpi-group-difference group1 group2 newgroup)))

(defun to-mpi-rank-spec (rank-spec)
  (let* ((count (length rank-spec))
         (buffer (foreign-alloc :int :count (* 3 count))))
    (loop for spec in rank-spec and i from 0 by 3
          with step-size = 1 and last-rank and first-rank do
            (etypecase spec
              (integer
               (setf first-rank spec)
               (setf last-rank spec))
              ((cons integer (cons integer null))
               (setf first-rank (car spec))
               (setf last-rank (cadr spec)))
              ((cons integer (cons integer (cons integer null)))
               (setf first-rank (car spec))
               (setf last-rank (cadr spec))
               (setf step-size (caddr spec))))
            (setf (mem-aref buffer :int (+ i 0)) first-rank)
            (setf (mem-aref buffer :int (+ i 1)) last-rank)
            (setf (mem-aref buffer :int (+ i 2)) step-size))
    buffer))

(defmacro with-mpi-rank-spec ((spec-name count-name)
                              (rank-spec) &body body)
  (check-type spec-name symbol)
  (check-type count-name symbol)
  (once-only (rank-spec)
    `(let ((,count-name (length ,rank-spec))
           (,spec-name (to-mpi-rank-spec ,rank-spec)))
       (unwind-protect
            (progn ,@body)
         (foreign-free ,spec-name)))))

(defun mpi-group-incl (group &rest rank-spec)
  "Create a new MPI group consisting of a subset of the ranks of the original
 group. A valid range can be
  - an integer
  - a list of the form (first-rank last-rank &optional step-size)"
  (make-instance
   'mpi-group
   :foreign-object
   (with-foreign-results ((newgroup 'mpi-group))
     (with-mpi-rank-spec (spec count) (rank-spec)
       (%mpi-group-range-incl group count spec newgroup)))))

(defun mpi-group-excl (group &rest rank-spec)
  "Create a new MPI group consisting of a subset of the ranks of the original
 group. A valid range can be
  - an integer
  - a list of the form (first-rank last-rank &optional step-size)"
  (make-instance
   'mpi-group
   :foreign-object
   (with-foreign-results ((newgroup 'mpi-group))
     (with-mpi-rank-spec (spec count) (rank-spec)
       (%mpi-group-range-excl group count spec newgroup)))))

(defun mpi-group-free (&rest groups)
  (loop for group in groups do
    (let ((ptr (foreign-alloc 'mpi-group
                              :initial-element (slot-value group 'foreign-object))))
      (%mpi-group-free ptr)
      (setf (slot-value group 'foreign-object)
            (mem-ref ptr 'mpi-group)))))

(defun mpi-comm-size (&optional (comm *standard-communicator*))
  "Indicates the number of processes involved in a communicator. For
+mpi-comm-world+, it indicates the total number of processes available."
  (with-foreign-results ((size :int))
    (%mpi-comm-size comm size)))

(defun mpi-comm-rank (&optional (comm *standard-communicator*))
  "Returns the rank of the process in a given communicator."
  (with-foreign-results ((rank :int))
    (%mpi-comm-rank comm rank)))

(defun mpi-comm-create (group &key (comm *standard-communicator*))
  (make-instance
   'mpi-comm
   :foreign-object
   (with-foreign-results ((newcomm 'mpi-comm))
     (%mpi-comm-create comm group newcomm))))

(defun mpi-comm-dup (&optional (comm *standard-communicator*))
  (make-instance
   'mpi-comm
   :foreign-object
   (with-foreign-results ((newcomm 'mpi-comm))
     (%mpi-comm-dup comm newcomm))))

(defun mpi-type-size (datatype)
  (with-foreign-results ((size :int))
    (%mpi-type-size datatype size)))
