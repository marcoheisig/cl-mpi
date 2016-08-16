#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

Lispy extensions to MPI

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

(in-package :cl-mpi-extensions)

(defvar *standard-encode-function*
  #'(lambda (x) (conspack:encode x :stream :static))
  "A function that serializes a given object.")

(defvar *standard-cleanup-function* #'free-static-vector
  "A function that cleans up the buffer returned by the corresponding
  encode function.")

(defvar *standard-decode-function* #'conspack:decode
  "A function that can deserialize the buffer created by the
  corresponding encode function.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; nonblocking communication

(defun mpi-send-anything (object dest &key (comm *standard-communicator*)
                                        (tag 0)
                                        (encode *standard-encode-function*)
                                        (cleanup *standard-cleanup-function*))
  "MPI-SEND-ANYTHING is a slower but more general variant of MPI-SEND. It can
  transmit any object to a matching MPI-RECV-ANYTHING."
  (mpi-waitall-anything
   (mpi-isend-anything object dest :comm comm :tag tag :encode encode :cleanup cleanup)))

(defun mpi-recv-anything (source &key (comm *standard-communicator*)
                                   (tag +mpi-any-tag+)
                                   (decode *standard-decode-function*))
  (mpi-waitall-anything
   (mpi-irecv-anything source :comm comm :tag tag :decode decode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; all to all communication

(defun mpi-broadcast-anything (root &key (comm *standard-communicator*)
                                      object
                                      (encode *standard-encode-function*)
                                      (decode *standard-decode-function*)
                                      (cleanup *standard-cleanup-function*))
  "The node with rank ROOT sends the given object to every other rank in the
  communicator COMM."
  (declare (type (signed-byte  32) root)
           (type mpi-comm comm))
  (cond
    ((= root (mpi-comm-rank))
     (let* ((sendbuf (funcall encode object :stream :static))
            (size (make-static-vector 1 :element-type '(signed-byte 64)
                                        :initial-element (length sendbuf))))
       (mpi-bcast size root :comm comm)
       (mpi-bcast sendbuf root :comm comm)
       (prog1 object
         (funcall cleanup sendbuf)
         (free-static-vector size))))
    (t
     (let ((size (make-static-vector 1 :element-type '(signed-byte 64))))
       (mpi-bcast size root :comm comm)
       (let ((recvbuf (make-static-vector (aref size 0))))
         (mpi-bcast recvbuf root :comm comm)
         (prog1 (funcall decode recvbuf)
           (free-static-vector recvbuf)
           (free-static-vector size)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; nonblocking communication

(defclass mpi-request-anything (mpi-request)
  ((%hook :initarg :hook :reader hook)))

(defun run-hook (request)
  (funcall (hook request)))

(defun mpi-waitall-anything (&rest metadata-requests)
  "Return a list of message descriptions. Each message description is of
  the form (SOURCE TAG OBJECTS)."
  (let ((data-requests (mapcar #'run-hook (apply #'mpi-waitall metadata-requests))))
    (delete-if #'not (mapcar #'run-hook (apply #'mpi-waitall data-requests)))))

(defun mpi-isend-anything (object dest &key (comm *standard-communicator*)
                                         (tag 0)
                                         (encode *standard-encode-function*)
                                         (cleanup *standard-cleanup-function*))
  "MPI-SEND-ANYTHING is a slower but more general variant of MPI-SEND. It can
  transmit any object to a matching MPI-RECEIVE-ANYTHING."
  (declare (type (signed-byte 32) dest tag)
           (type mpi-comm comm))
  (let* ((data-buffer
           (funcall encode object))
         (metadata-buffer
           (make-static-vector 1 :element-type '(unsigned-byte 64)
                                 :initial-element (length data-buffer))))
    (change-class
     (mpi-isend metadata-buffer dest :tag tag :comm comm :mode :synchronous)
     'mpi-request-anything
     :hook
     (lambda ()
       (free-static-vector metadata-buffer)
       (change-class
        (mpi-isend data-buffer dest :tag tag :comm comm :mode :synchronous)
        'mpi-request-anything
        :hook (lambda ()
                (funcall cleanup data-buffer)
                nil))))))

(defun mpi-irecv-anything (source &key (comm *standard-communicator*)
                                    (tag +mpi-any-tag+)
                                    (decode *standard-decode-function*))
  (declare (type (signed-byte 32) source tag)
           (type mpi-comm comm))
  (let ((metadata-buffer
          (make-static-vector 1 :element-type '(unsigned-byte 64)))
        data-buffer)
    (change-class
     (mpi-irecv metadata-buffer source :comm comm :tag tag)
     'mpi-request-anything
     :hook
     (lambda ()
       (setf data-buffer
             (make-static-vector (aref metadata-buffer 0)))
       (free-static-vector metadata-buffer)
       (change-class
        (mpi-irecv data-buffer source :comm comm :tag tag)
        'mpi-request-anything
        :hook
        (lambda ()
          (prog1
              (list source tag (funcall decode data-buffer))
            (free-static-vector data-buffer))))))))
