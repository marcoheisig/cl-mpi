(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system 'cffi-grovel))

(defpackage :cl-mpi-system
  (:use :asdf :cl)
  (:export #:mpi-library))

(in-package #:cl-mpi-system)

;; use "mpicc" as compiler for all mpi related cffi-grovel files
(defmethod perform :around ((op cffi-grovel::process-op)
                            (c cffi-grovel:grovel-file))
  (let ((cffi-grovel::*cc* "mpicc"))
    (call-next-method)))

(defclass mpi-library (c-source-file) ())

(defmethod output-files ((o compile-op) (c mpi-library))
  (declare (ignorable o))
  (list (make-pathname :defaults (component-pathname c)
                       :type "so")))

(defmethod perform ((o compile-op) (c mpi-library))
  (let ((target (output-file o c))
        (source (component-pathname c)))
    (let ((possible-commands
            (list
             (format nil "mpicc -shared -fPIC -o ~A ~A" target source)
             ;; more commands can be added here if there is a system where the
             ;; above command fails
             )))
      (block compile-stub-library
        (dolist (cmd possible-commands)
          (if (multiple-value-bind (stdout stderr exit-code)
                  (uiop:run-program cmd :ignore-error-status t)
                (declare (ignore stdout stderr))
                (zerop exit-code))
              (progn
                (format *standard-output* "; ~A~%" cmd)
                (return-from compile-stub-library))))
        (error 'operation-error :component c :operation o)))))

(defmethod perform ((o load-op) (c mpi-library))
  (cffi:load-foreign-library (output-file 'compile-op c)))

(in-package :asdf-user)

(asdf:defsystem :cl-mpi
  :description "Common Lisp bindings for the Message Passing Interface (MPI)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.5"
  :license "MIT"
  :depends-on (:alexandria :cffi :static-vectors :cl-conspack)
  :in-order-to ((test-op (test-op "cl-mpi-testsuite")))
  :components
  ((:module "mpi"
    :serial t
    :components
    ((:file "packages")
     ("cffi-grovel:grovel-file" "mpi-grovel")
     ("cl-mpi-system:mpi-library" "cl-mpi-stub")
     (:file "mpi-configure")
     (:file "mpi-types")
     (:file "mpi-variables")
     (:file "mpi-bindings")
     (:file "mpi")
     (:file "mpi-extensions")))))
