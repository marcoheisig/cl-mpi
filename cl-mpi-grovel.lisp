(cl:defpackage #:cl-mpi-grovel
  (:use :cl)
  (:export #:mpi-grovel-file #:mpi-wrapper-file))

(cl:in-package #:cl-mpi-grovel)

(defclass mpi-mixin ()
  ())

(defclass mpi-grovel-file (cffi-grovel:grovel-file mpi-mixin)
  ())

(defclass mpi-wrapper-file (cffi-grovel:wrapper-file mpi-mixin)
  ())

(defmethod asdf:perform :around ((op asdf:operation) (c mpi-mixin))
  (let ((cffi-toolchain:*cc* "mpicc")
        (cffi-toolchain:*ld* "mpicc"))
    (call-next-method)))
