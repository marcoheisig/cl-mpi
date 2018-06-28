(cl:defpackage #:cl-mpi-asdf-integration
  (:use :cl)
  (:export #:mpi-grovel-file #:mpi-wrapper-file #:mpi-program))

(cl:in-package #:cl-mpi-asdf-integration)

(defclass mpi-mixin ()
  ())

(defclass mpi-grovel-file (cffi-grovel:grovel-file mpi-mixin)
  ())

(defclass mpi-wrapper-file (cffi-grovel:wrapper-file mpi-mixin)
  ())

(defclass mpi-program (asdf:system mpi-mixin)
  ()
  (:documentation
   "Class of ASDF systems that represent standalone MPI programs."))

(defmethod asdf:perform :around ((op asdf:operation) (c mpi-mixin))
  (let ((cffi-toolchain:*cc* "mpicc")
        (cffi-toolchain:*ld* "mpicc"))
    (call-next-method)))

#+sbcl
(defmethod asdf:perform :before ((op asdf:program-op) (c mpi-program))
  (loop for object in sb-alien::*shared-objects* do
        (setf (sb-alien::shared-object-dont-save object) t)))

;; Allow for keywords in ASDF definitions.

(setf (find-class 'asdf::mpi-grovel-file)
      (find-class 'mpi-grovel-file))

(setf (find-class 'asdf::mpi-wrapper-file)
      (find-class 'mpi-wrapper-file))

(setf (find-class 'asdf::mpi-program)
      (find-class 'mpi-program))
