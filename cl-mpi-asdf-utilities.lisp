;;; Extend ASDF and the CFFI groveller to be MPI aware
(defpackage #:cl-mpi-asdf-utilities
  (:use #:asdf #:cl)
  (:export #:mpi-stub))

(in-package #:cl-mpi-asdf-utilities)

;;; use "mpicc" as compiler for all mpi related cffi-grovel files
(defmethod perform :around ((op cffi-grovel::process-op)
                            (c cffi-grovel:grovel-file))
  (let ((cffi-grovel::*cc* "mpicc"))
    (call-next-method)))

(defclass mpi-stub (c-source-file)
  ((%mpi-info :initform ""
              :accessor mpi-info)))


(defun compute-mpi-info ()
  "Produce some value that is EQUALP unless the MPI implementation changes."
  (multiple-value-bind (output stderr exit-code)
      (uiop:run-program "mpicc -show~%" :output :string
                                        :ignore-error-status t)
    (declare (ignore stderr))
    (if (zerop exit-code)
        output
        nil)))

(defmethod output-files ((o compile-op) (c mpi-stub))
  (declare (ignorable o))
  (list (make-pathname :defaults (component-pathname c)
                       :type "so")))

;; Convert a single c source file into a shared library that can be loaded
;; with ASDF:LOAD-OP.
(defmethod perform ((o compile-op) (c mpi-stub))
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
        (error 'operation-error :component c :operation o))
      (setf (mpi-info c) (compute-mpi-info)))))

(defmethod perform ((o load-op) (c mpi-stub))
  (cffi:load-foreign-library (output-file 'compile-op c)))

(defmethod operation-done-p ((o compile-op) (c mpi-stub))
  (format t "old: ~A~%new: ~A~%" (mpi-info c) (compute-mpi-info))
  (if (equalp (mpi-info c)
              (compute-mpi-info))
      (call-next-method)
      nil))
