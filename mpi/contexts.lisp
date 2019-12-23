#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI context handling - groups, communicators, caching

Copyright (C) 2015  Marco Heisig <marco.heisig@fau.de>

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

(in-package :cl-mpi)

(defun mpi-comm-group (&optional (comm *standard-communicator*))
  (declare (type mpi-comm comm))
  (with-foreign-results ((newgroup 'mpi-group))
    (%mpi-comm-group comm newgroup)))

(defun mpi-group-size (group)
  (declare (type mpi-group group))
  (with-foreign-results ((size :int))
    (%mpi-group-size group size)))

(defun mpi-group-rank (group)
  (declare (type mpi-group group))
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
  (with-foreign-results ((newgroup 'mpi-group))
    (with-mpi-rank-spec (spec count) (rank-spec)
      (%mpi-group-range-incl group count spec newgroup))))

(defun mpi-group-excl (group &rest rank-spec)
  "Create a new MPI group consisting of a subset of the ranks of the original
 group. A valid range can be
  - an integer
  - a list of the form (first-rank last-rank &optional step-size)"
  (with-foreign-results ((newgroup 'mpi-group))
    (with-mpi-rank-spec (spec count) (rank-spec)
      (%mpi-group-range-excl group count spec newgroup))))

(defun mpi-group-free (&rest groups)
  (let ((handle (foreign-alloc 'mpi-group)))
    (loop for group in groups do
      (setf (mem-ref handle 'mpi-group) group)
      (%mpi-group-free handle)
      (setf (mpi-object-handle group)
            (mem-ref handle #.foreign-mpi-object-type)))))

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
  (declare (type mpi-group group)
           (type mpi-comm comm))
  (with-foreign-results ((newcomm 'mpi-comm))
    (%mpi-comm-create comm group newcomm)))

(defun mpi-comm-dup (&optional (comm *standard-communicator*))
  (with-foreign-results ((newcomm 'mpi-comm))
    (%mpi-comm-dup comm newcomm)))

(defun mpi-comm-free (comm)
  (let ((handle (foreign-alloc 'mpi-comm)))
    (setf (mem-ref handle 'mpi-comm) comm)
    (%mpi-comm-free handle)
    (setf (mpi-object-handle comm)
          (mem-ref handle #.foreign-mpi-object-type)))
  comm)

(defun mpi-comm-split (color key &key (comm *standard-communicator*))
  "Returns new communicator by partitioning a communicator according
to color and key."
  (declare (type (or (and int (integer 0)) (eql #.+mpi-undefined+)) color)
           (type int key)
           (type mpi-comm comm))
  (with-foreign-results ((newcomm 'mpi-comm))
    (%mpi-comm-split comm color key newcomm)))
