;;; Wrapper library specification for querying information that is not
;;; necessarily static, such as the definition of MPI_COMM_WORLD.

(include "mpi.h")

(in-package :cl-mpi)

#.
`(progn
   ,@(loop for (object c-name reader-name) in *mpi-constant-table*
           collect
           `(defwrapper*
                ,reader-name
                ,(cffi::canonicalize-foreign-type
                  (class-name
                   (class-of object))) ()
              ,(format nil "return ~A;" c-name))))
