;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;; Copyright (C) 2014  Marco Heisig <marco.heisig@fau.de>

(in-package :mpi)

(defun detect-mpi-implementation ()
  (cond
    ((boundp 'mpi-header::OPEN_MPI) :openmpi)
    ((boundp 'mpi-header::MPICH) :mpich)
    ((boundp 'mpi-header::MPICH2) :mpich2)
    (t :unknown)))

(pushnew (detect-mpi-implementation) *features*)

(defconstant +mpi-version+
  (if (boundp '+mpi-version+)
      +mpi-version+
      (format nil "~D.~D"
              mpi-header::MPI_VERSION
              mpi-header::MPI_SUBVERSION)))

(defconstant +mpi-max-error-string+ mpi-header::MPI_MAX_ERROR_STRING)
(defconstant +mpi-max-processor-name+ mpi-header::MPI_MAX_PROCESSOR_NAME)
(defconstant +mpi-any-tag+ mpi-header::MPI_ANY_TAG)
(defconstant +mpi-any-source+ mpi-header::MPI_ANY_SOURCE)
(defconstant +mpi-proc-null+ mpi-header::MPI_PROC_NULL)
(defconstant +mpi-root+ mpi-header::MPI_ROOT)
(defconstant +mpi-undefined+ mpi-header::MPI_UNDEFINED)

(defconstant +mpi-status-ignore+
  (if (boundp '+mpi-status-ignore+)
      (symbol-value '+mpi-status-ignore+)
      (make-pointer mpi-header::MPI_STATUS_IGNORE)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *mpi-naming-conventions*
    (flet ((genpairs (args)
             (destructuring-bind (type &rest symbols) args
               (loop for symbol in symbols
                     collect (list symbol type)))))
      (apply
       #'append
       (mapcar
        #'genpairs
        '((:pointer
           *buf *sendbuf *recvbuf *inbuf *outbuf *inoutbuf fun argc argv)
          ((:pointer :int)
           *result *count *position *size *rank *index *outcount *commute *keyval)
          (:string string)
          ((:pointer :boolean) *flag)
          (:int
           count incount outcount insize outsize sendcount recvcount source dest
           tag sendtag recvtag size root commute errorcode)
          (:boolean flag)
          (mpi-errhandler errhandler)
          (mpi-comm comm oldcomm comm1 comm2)
          (mpi-group group group1 group2)
          (mpi-datatype datatype sendtype recvtype oldtype)
          (mpi-op op)
          (mpi-request request)
          ((:pointer (:struct mpi-status)) *status)
          ((:pointer mpi-op) *op)
          ((:pointer mpi-message) *message)
          ((:pointer mpi-request) *request)
          ((:pointer mpi-comm) *newcomm)
          ((:pointer mpi-group) *newgroup *group)
          ((:pointer mpi-datatype) *newtype)
          ((:array (:struct mpi-status) *) statuses)
          ((:array mpi-datatype *) sendtypes recvtypes)
          ((:array mpi-request *) requests)
          ((:array :int *) indices)
          ((:pointer (:array :int *)) ranges)
          ((:array :int *)
           ranks ranks1 ranks2 sendcounts recvcounts displs sdispls rdispls)))))))

(defmacro defmpifun (foreign-name (&rest mpi-identifiers)
                     &key
                       documentation
                       introduced
                       deprecated
                       removed)
  (check-type foreign-name string)
  (let ((lisp-name
          (intern
           (concatenate 'string "%" (substitute #\- #\_ (string-upcase foreign-name)))
           :mpi))
        (args
          (loop for i in mpi-identifiers
                collect (find i *mpi-naming-conventions* :test #'eq :key #'car)))
        (introducedp (version<= introduced +mpi-version+))
        (removedp (and removed (version<= removed +mpi-version+)))
        (deprecatedp (version<= deprecated +mpi-version+)))
    (declare (ignorable deprecatedp)) ;; currently I do not handle deprecation
    (if documentation (push documentation args))
    (if (and introducedp (not removedp))
        `(progn
           (defcfun (,foreign-name ,lisp-name) mpi-error-code ,@args))
        `(progn
           (defun ,lisp-name ,mpi-identifiers
             (declare (ignore ,@mpi-identifiers))
             (error "This function was ~:[introduced~;removed~] in MPI version
                 ~:[~A~*~;~*~A~], while your MPI library is version ~A.~%"
                    ,removedp ,removedp ,introduced ,removed +mpi-version+))))))
