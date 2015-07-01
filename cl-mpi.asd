(in-package #:asdf-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system 'cffi-grovel))

(defpackage #:cl-mpi-system
  (:use #:asdf #:cl)
  (:export #:mpi-library))

(in-package #:cl-mpi-system)
;;; Extend ASDF and the CFFI groveller to be MPI aware

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

(in-package #:asdf-user)

(defsystem #:cl-mpi
  :description "Common Lisp bindings for the Message Passing Interface (MPI)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.5"
  :license "MIT"
  :depends-on (#:alexandria #:cffi #:static-vectors #:cl-conspack)
  :in-order-to ((test-op (test-op "cl-mpi-testsuite")))
  :components
  ((:module "mpi"
    :serial t
    ;; Let me explain this long chain of serial dependencies: After the
    ;; package declaration, "grovel.lisp" extracts all constants from mpi.h
    ;; and the system MPI library is loaded via "cl-mpi-stub.c". The constants
    ;; are then used in "configure.lisp" to set up MPI implementation
    ;; dependent reader conditionals. "wrapper-types.lisp" integrates the MPI
    ;; handles and error codes into Lisp objects with their CFFI
    ;; counterparts. After the object definition, MPI related constants and
    ;; variables can be declared in "variables.lisp". Those variables are used
    ;; to define several helper functions in "utilities.lisp". All remaining
    ;; files are independent of each other and correspond to the individual
    ;; chapters of the MPI specification.
    :components
    ((:file "packages")
     (cffi-grovel:grovel-file "grovel") ; extract all constants from "mpi.h"
     (cl-mpi-system:mpi-library "cl-mpi-stub") ; load system MPI implementation
     (:file "configure") ; MPI implementation dependent *features*
     (:file "wrapper-types") ; CLOS wrappers for MPI handles
     (:file "variables") ; Lisp-accessible variables from mpi.h
     (:file "utilities")
     (:file "datatypes")
     (:file "collective")
     (:file "contexts")
     (:file "environment")
     (:file "point-to-point")
     (:file "one-sided")
     (:file "extensions")))))
