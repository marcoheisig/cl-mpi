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

(in-package :mpi)

;;; A.2.4 Groups, Contexts, Communicators, and Caching C Bindings

;; (defmpifun "MPI_COMM_DUP_FN")
;; (defmpifun "MPI_COMM_NULL_COPY_FN")
;; (defmpifun "MPI_COMM_NULL_DELETE_FN")
(defmpifun "MPI_Comm_compare" (comm1 comm2 *result) :introduced "1.0")
(defmpifun "MPI_Comm_create" (comm group *newcomm) :introduced "1.0")
(defmpifun "MPI_Comm_create_group" (comm group tag *newcomm) :introduced "3.0")
;; (defmpifun "MPI_Comm_create_keyval")
;; (defmpifun "MPI_Comm_delete_attr")
(defmpifun "MPI_Comm_dup" (comm *newcomm) :introduced "1.0")
;; (defmpifun "MPI_Comm_dup_with_info")
(defmpifun "MPI_Comm_free" (comm) :introduced "1.0")
;; (defmpifun "MPI_Comm_free_keyval")
;; (defmpifun "MPI_Comm_get_attr")
;; (defmpifun "MPI_Comm_get_info")
;; (defmpifun "MPI_Comm_get_name")
(defmpifun "MPI_Comm_group" (comm *group) :introduced "1.0")
(defmpifun "MPI_Comm_idup" (comm *newcomm *request) :introduced "3.0")
(defmpifun "MPI_Comm_rank" (comm *rank) :introduced "1.0")
(defmpifun "MPI_Comm_remote_group" (comm *group) :introduced "1.0")
(defmpifun "MPI_Comm_remote_size" (comm *size) :introduced "1.0")
;; (defmpifun "MPI_Comm_set_attr")
;; (defmpifun "MPI_Comm_set_info")
;; (defmpifun "MPI_Comm_set_name")
(defmpifun "MPI_Comm_size" (comm *size) :introduced "1.0")
;; (defmpifun "MPI_Comm_split" :introduced "1.0")
;; (defmpifun "MPI_Comm_split_type")
(defmpifun "MPI_Comm_test_inter" (comm *flag) :introduced "1.0")
(defmpifun "MPI_Group_compare" (group1 group2 *result) :introduced "1.0")
(defmpifun "MPI_Group_difference" (group1 group2 *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_excl" (group count ranges *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_free" (*group) :introduced "1.0")
(defmpifun "MPI_Group_incl" (group count ranges *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_intersection" (group1 group2 *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_range_excl" (group count ranges *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_range_incl" (group count ranges *newgroup) :introduced "1.0")
(defmpifun "MPI_Group_rank" (group *rank) :introduced "1.0")
(defmpifun "MPI_Group_size" (group *size) :introduced "1.0")
(defmpifun "MPI_Group_translate_ranks" (group1 count ranks1 group2 ranks2) :introduced "1.0")
(defmpifun "MPI_Group_union" (group1 group2 *newgroup) :introduced "1.0")
;; (defmpifun "MPI_Intercomm_create" :introduced "1.0")
;; (defmpifun "MPI_Intercomm_merge" :introduced "1.0")
;; (defmpifun "MPI_TYPE_DUP_FN")
;; (defmpifun "MPI_TYPE_NULL_COPY_FN")
;; (defmpifun "MPI_TYPE_NULL_DELETE_FN")
;; (defmpifun "MPI_Type_create_keyval")
;; (defmpifun "MPI_Type_free_keyval")
;; (defmpifun "MPI_Type_get_attr")
;; (defmpifun "MPI_Type_get_name")
;; (defmpifun "MPI_Type_set_attr")
;; (defmpifun "MPI_Type_set_name")
;; (defmpifun "MPI_WIN_DUP_FN")
;; (defmpifun "MPI_WIN_NULL_COPY_FN")
;; (defmpifun "MPI_WIN_NULL_DELETE_FN")
;; (defmpifun "MPI_Win_create_keyval")
;; (defmpifun "MPI_Win_delete_attr")
;; (defmpifun "MPI_Win_free_attr")
;; (defmpifun "MPI_Win_get_attr")
;; (defmpifun "MPI_Win_get_name")
;; (defmpifun "MPI_Win_set_attr")
;; (defmpifun "MPI_Win_set_name")

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
