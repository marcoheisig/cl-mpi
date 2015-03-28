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

;; TODO make this work for pointers to arrays
(defmacro pointer-ref ((name cffi-type) &body body)
  "Allocate a foreign value of type CFFI-TYPE and bind it to NAME. Then
evaluate BODY and return the value of the foreign value."
  (check-type name symbol)
  `(with-foreign-object (,name ,cffi-type)
     ,@body
     (mem-ref ,name ,cffi-type)))

(defun mpi-init ()
  "This routine must be called before any other MPI routine. It must be called
at most once; subsequent calls are erroneous (see MPI-INITIALIZED).

All MPI programs must contain a call to MPI-INIT; this routine must be called
before any other MPI routine (apart from MPI-INITIALIZED) is called."
  (unless (mpi-initialized)
    (%mpi-init (null-pointer) (null-pointer))
    ;; by default MPI reacts to each failure by crashing the process. This is
    ;; not the Lisp way of doing things. The following call makes error
    ;; non-fatal in most cases.
    (%mpi-comm-set-errhandler *mpi-comm-world* *mpi-errors-return*)))

(defun mpi-initialized ()
  "Returns true if MPI_INIT has been called and nil otherwise.
   This routine may be used to determine whether MPI-INIT has been called. It
   is the only routine that may be called before MPI-INIT is called."
  (not (zerop (pointer-ref (flag :int)
                (%mpi-initialized flag)))))

(defun mpi-abort(&key (comm *mpi-comm-world*) (errcode -1))
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

(defun mpi-barrier (&optional (comm *mpi-comm-world*))
  "MPI-BARRIER blocks the caller until all group members have called it. The
  call returns at any process only after all group members have entered the
  call."
  (%mpi-barrier comm))

(defun mpi-send-foreign (dest foreign-pointer count cffi-type
                         &key (tag 0) (comm *mpi-comm-world*))
  "Send data to the MPI process specified by the integers DEST and
  TAG. FOREIGN-POINTER must point to a foreign-array with SIZE elements of
  type CFFI-TYPE."
  (declare (type (signed-byte 32) dest count tag)
           (type keyword cffi-type))
  (%mpi-send foreign-pointer count (cffi-type-to-mpi-type cffi-type) dest tag comm))

(defun mpi-receive-foreign (source foreign-pointer count cffi-type
                            &key (tag +mpi-any-tag+) (comm *mpi-comm-world*))
  "Receive data from the MPI process specified by the integers SOURCE and
  TAG. FOREIGN-POINTER must point to a foreign-array with COUNT elements of
  type CFFI-TYPE."
  (declare (type (signed-byte 32) source count tag)
           (type keyword cffi-type))
  (with-foreign-object (mpi-status '(:struct mpi-status))
    (%mpi-recv foreign-pointer count (cffi-type-to-mpi-type cffi-type) source tag comm mpi-status)
    (with-foreign-slots ((mpi-source mpi-tag mpi-error) mpi-status (:struct mpi-status))
      (unless (zerop mpi-error) (signal-mpi-error mpi-error))
      (values mpi-source mpi-tag))))

(defun mpi-sendreceive-foreign
    (dest   send-buf send-count send-type
     source recv-buf recv-count recv-type
     &key (send-tag 0) (recv-tag +mpi-any-tag+) (comm *mpi-comm-world*))
  (declare (type (signed-byte 32)
                 dest send-count send-tag
                 source recv-count recv-tag))
  (%mpi-sendrecv
   send-buf send-count (cffi-type-to-mpi-type send-type) dest send-tag
   recv-buf recv-count (cffi-type-to-mpi-type recv-type) source recv-tag
   comm *mpi-status-ignore*))

(defun mpi-broadcast-foreign (inout-buffer count cffi-type root &key (comm *mpi-comm-world*))
  (declare (type (signed-byte 32) root count))
  (%mpi-bcast inout-buffer count (cffi-type-to-mpi-type cffi-type) root comm))

(defun mpi-broadcast (object root &key (comm *mpi-comm-world*))
  "Return the value of OBJECT given by the process with the rank ROOT. The value of
OBJECT is ignored in all other processes."
  (declare (type (signed-byte 32) root))
  (with-foreign-object (count-mem :int)
    (let ((data #())
          (is-root (= root (mpi-comm-rank comm))))
      (when is-root
        (setf data (conspack:encode object))
        (setf (mem-ref count-mem :int) (length data)))
      ;; step 1: broadcast size of message
      (mpi-broadcast-foreign count-mem 1 :int root :comm comm)
      (let* ((count (mem-ref count-mem :int))
             (data (if is-root
                       data
                       (make-array count :element-type '(unsigned-byte 8)))))
        (with-foreign-object (buffer :uchar count)
          (when is-root
            (loop for i from 0 below count do
              (setf (mem-ref buffer :uchar i) (aref data i))))
          ;; step 2: broadcast message
          (mpi-broadcast-foreign buffer count :uchar root :comm comm)
          (unless is-root
            (loop for i from 0 below count do
              (setf (aref data i) (mem-ref buffer :uchar i)))))
        (mpi-barrier)
        (conspack:decode data)))))

(defun mpi-send (dest object &key (tag 0) (comm *mpi-comm-world*))
  (declare (type (signed-byte 32) dest tag))
  (let* ((data (conspack:encode object :stream :static))
         (count (length data)))
    (with-foreign-object (sendbuf :uchar count)
      (loop for i from 0 below count do
        (setf (mem-ref sendbuf :uchar i) (aref data i)))
      (mpi-send-foreign dest sendbuf count :uchar :tag tag :comm comm))))

(defun mpi-receive (source &key (tag +mpi-any-tag+) (comm *mpi-comm-world*))
  (declare (type (signed-byte 32) source tag))
  (with-foreign-objects ((status '(:struct mpi-status))
                         (count-mem :int))
    (%mpi-probe source tag comm status)
    (%mpi-get-count status *mpi-unsigned-char* count-mem)
    (let* ((count (mem-ref count-mem :int))
           (data (make-array count :element-type '(unsigned-byte 8))))
      (with-foreign-object (buf :uchar count)
        (mpi-receive-foreign source buf count :uchar :tag tag :comm comm)
        (loop for i from 0 below count do
          (setf (aref data i) (mem-ref buf :uchar i)))
        (values (conspack:decode data) ; TODO
                )))))

(defun mpi-comm-group (comm)
  (make-instance
   'mpi-group
   :mpi-object-handle
   (pointer-ref (newgroup 'mpi-group)
     (%mpi-comm-group comm newgroup))))

(defun mpi-group-size (group)
  (pointer-ref (size :int)
    (%mpi-group-size group size)))

(defun mpi-group-rank (group)
  (pointer-ref (rank :int)
    (%mpi-group-rank group rank)))

(defun mpi-group-union (group1 group2)
    (pointer-ref (newgroup 'mpi-group)
      (%mpi-group-union group1 group2 newgroup)))

(defun mpi-group-intersection (group1 group2)
  (pointer-ref (newgroup 'mpi-group)
    (%mpi-group-intersection group1 group2 newgroup)))

(defun mpi-group-difference (group1 group2)
  (pointer-ref (newgroup 'mpi-group)
    (%mpi-group-difference group1 group2 newgroup)))

;; (defun mpi-group-select-from (group &rest ranges)
;;   "Create a new MPI group consisting of a subset of the ranks of the original
;;  group. A valid range can be
;;   - an integer
;;   - a list of the form (first-rank last-rank &optional step-size)"
;;   (let ((n (length ranges)))
;;     (with-foreign-objects ((mem :int (* 3 n)) (newgroup 'mpi-group))
;;       (loop for range-spec in ranges and i from 0 by 3
;;          with step-size = 1 and last-rank and first-rank do
;;            (cond
;;              ((integerp range-spec)
;;               (setf first-rank range-spec)
;;               (setf last-rank range-spec))
;;              ((listp range-spec)
;;               (setf first-rank (car range-spec))
;;               (setf last-rank (cadr range-spec))
;;               (setf step-size (if (cddr range-spec) (caddr range-spec) 1)))
;;              (t (error "invalid range spec")))
;;            (setf (mem-aref mem :int (+ i 0)) first-rank)
;;            (setf (mem-aref mem :int (+ i 1)) last-rank)
;;            (setf (mem-aref mem :int (+ i 2)) step-size))
;;       (MPI-Group-range-incl group n mem newgroup)
;;       (mem-ref newgroup 'MPI-Group))))

;; (defmpifun "MPI-Group-excl" (group :pointer) (n :int) (ranks :pointer) (newgroup :pointer))
;; (defmpifun "MPI-Group-free" (group mpi-group))
;;(defun mpi-group-free (group) (MPI-Group-free group))

(defun mpi-comm-size (&optional (comm *mpi-comm-world*))
  "Indicates the number of processes involved in a communicator. For
*mpi-comm-world*, it indicates the total number of processes available."
  (pointer-ref (size :int)
    (%mpi-comm-size comm size)))

(defun mpi-comm-rank (&optional (comm *mpi-comm-world*))
  "Returns the rank of the process in a given communicator."
  (pointer-ref (rank :int)
    (%mpi-comm-rank comm rank)))

(defun mpi-comm-create (group &key (comm *mpi-comm-world*))
  (make-instance
   'mpi-comm
   :mpi-object-handle
   (pointer-ref (newcomm 'mpi-comm)
    (%mpi-comm-create comm group newcomm))))


;;; after careful consideration I decided it is the right thing to call
;;; MPI-INIT as soon as cl-mpi is loaded. Otherwise all MPI calls exhibit
;;; unspecified behaviour.
(eval-when (:load-toplevel :execute) (mpi-init))
