;;;; extract all MPI symbols from mpi.h

(include "mpi.h")

(in-package :cl-mpi)

;;; optional and MPI implementation specific constants
(constant (|MPICH| "MPICH") :optional t)
(constant (|MPICH_VERSION| "MPICH_VERSION") :optional t)

(constant (|MPICH2| "MPICH2") :optional t)
(constant (|MPICH2_VERSION| "MPICH2_VERSION") :optional t)

(constant (|OPEN_MPI| "OPEN_MPI") :optional t)
(constant (|OPEN_MPI_MAJOR_VERSION| "OMPI_MAJOR_VERSION") :optional t)
(constant (|OPEN_MPI_MINOR_VERSION| "OMPI_MINOR_VERSION") :optional t)
(constant (|OPEN_MPI_RELEASE_VERSION| "OMPI_RELEASE_VERSION") :optional t)

;;; standardized MPI constants

(constant (|MPI_VERSION| "MPI_VERSION"))
(constant (|MPI_SUBVERSION| "MPI_SUBVERSION"))
(constant (+mpi-any-source+ "MPI_ANY_SOURCE"))
(constant (+mpi-proc-null+ "MPI_PROC_NULL"))
(constant (+mpi-root+ "MPI_ROOT"))
(constant (+mpi-any-tag+ "MPI_ANY_TAG"))
(constant (+mpi-undefined+ "MPI_UNDEFINED"))

(constant (+mpi-max-processor-name+ "MPI_MAX_PROCESSOR_NAME"))
(constant (+mpi-max-error-string+ "MPI_MAX_ERROR_STRING"))

(cenum (mpi-thread-options :base-type :int)
       ((:mpi-thread-single "MPI_THREAD_SINGLE"))
       ((:mpi-thread-funneled "MPI_THREAD_FUNNELED"))
       ((:mpi-thread-serialized "MPI_THREAD_SERIALIZED"))
       ((:mpi-thread-multiple "MPI_THREAD_MULTIPLE")))

(cstruct mpi-status "MPI_Status"
         (mpi-source "MPI_SOURCE" :type :int)
         (mpi-tag "MPI_TAG" :type :int)
         (mpi-error "MPI_ERROR" :type :int))
