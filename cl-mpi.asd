(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system 'cffi-grovel))

(defpackage :cl-mpi-system (:use :asdf :cl :uiop :cffi))
(in-package :cl-mpi-system)

;; use "mpicc" as compiler for all mpi related cffi-grovel files
(defmethod perform :around ((op cffi-grovel::process-op)
                            (c cffi-grovel:grovel-file))
  (let ((cffi-grovel::*cc* "mpicc"))
    (call-next-method)))

(defclass mpi-library (c-source-file) ())

(defmethod perform ((o compile-op) (c mpi-library))
  (let ((target
          (namestring
           (make-pathname :defaults (component-pathname c)
                          :type "so")))
        (source (component-pathname c)))
    (let ((possible-commands
            (list
             (format nil "mpicc -shared -o ~A ~A" target source)
             ;; more commands can be added here if there is a system where the
             ;; above command fails
             )))
      (flet ((execution-successful-p (cmd)
               (multiple-value-bind (stdout stderr exit-code)
                   (run-program cmd :ignore-error-status t)
                 (zerop exit-code))))
        (unless (some #'execution-successful-p possible-commands)
          (error 'operation-error :component c :operation o))))))

(defmethod perform ((o load-op) (c mpi-library))
  (load-foreign-library (namestring
                         (make-pathname :defaults (component-pathname c)
                                        :type "so"))))

(defmethod operation-done-p ((o compile-op) (c mpi-library))
  (let ((lib (make-pathname :defaults (component-pathname c)
                            :type "so")))
    (and
     (probe-file lib)
     (> (file-write-date lib) (file-write-date (component-pathname c)))
     t)))

(asdf:defsystem :cl-mpi
  :description "Common Lisp bindings for the Message Passing Interface (MPI)"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :version "0.5"
  :license "MIT"
  :depends-on (:alexandria :cffi :cl-ppcre :static-vectors :cl-conspack :uiop)
  :in-order-to ((test-op (test-op "cl-mpi-testsuite")))
  :components
  ((:module "mpi"
    :serial t
    :components
    ((:file "packages")
     (cffi-grovel:grovel-file "mpi-grovel")
     (:mpi-library "mpi-stub")
     (:file "mpi-configure")
     (:file "mpi-types")
     (:file "mpi-variables")
     (:file "mpi-bindings")
     (:file "mpi")
     (:file "mpi-extensions")))))
