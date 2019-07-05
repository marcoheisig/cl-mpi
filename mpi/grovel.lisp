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
(constant (+mpi-max-processor-name+ "MPI_MAX_PROCESSOR_NAME"))
(constant (+mpi-max-error-string+ "MPI_MAX_ERROR_STRING"))
(constant (+mpi-max-object-name+ "MPI_MAX_OBJECT_NAME"))
(constant (+mpi-max-library-version-string+ "MPI_MAX_LIBRARY_VERSION_STRING"))
(constant (+mpi-undefined+ "MPI_UNDEFINED"))
(constant (+mpi-dist-graph+ "MPI_DIST_GRAPH"))
(constant (+mpi-cart+ "MPI_CART"))
(constant (+mpi-graph+ "MPI_GRAPH"))
(constant (+mpi-keyval-invalid+ "MPI_KEYVAL_INVALID"))
(constant (+mpi-unweighted+ "MPI_UNWEIGHTED"))
(constant (+mpi-weights-empty+ "MPI_WEIGHTS_EMPTY"))
(constant (+mpi-bottom+ "MPI_BOTTOM"))
(constant (+mpi-in-place+ "MPI_IN_PLACE"))
(constant (+mpi-bsend-overhead+ "MPI_BSEND_OVERHEAD"))
(constant (+mpi-max-info-key+ "MPI_MAX_INFO_KEY"))
(constant (+mpi-max-info-val+ "MPI_MAX_INFO_VAL"))
(constant (+mpi-argv-null+ "MPI_ARGV_NULL"))
(constant (+mpi-argvs-null+ "MPI_ARGVS_NULL"))
(constant (+mpi-errcodes-ignore+ "MPI_ERRCODES_IGNORE"))
(constant (+mpi-max-port-name+ "MPI_MAX_PORT_NAME"))
(constant (+mpi-order-c+ "MPI_ORDER_C"))
(constant (+mpi-order-fortran+ "MPI_ORDER_FORTRAN"))
(constant (+mpi-distribute-block+ "MPI_DISTRIBUTE_BLOCK"))
(constant (+mpi-distribute-cyclic+ "MPI_DISTRIBUTE_CYCLIC"))
(constant (+mpi-distribute-none+ "MPI_DISTRIBUTE_NONE"))
(constant (+mpi-distribute-dflt-darg+ "MPI_DISTRIBUTE_DFLT_DARG"))
(constant (+mpi-mode-create+ "MPI_MODE_CREATE"))
(constant (+mpi-mode-rdonly+ "MPI_MODE_RDONLY"))
(constant (+mpi-mode-wronly+ "MPI_MODE_WRONLY"))
(constant (+mpi-mode-rdwr+ "MPI_MODE_RDWR"))
(constant (+mpi-mode-delete-on-close+ "MPI_MODE_DELETE_ON_CLOSE"))
(constant (+mpi-mode-unique-open+ "MPI_MODE_UNIQUE_OPEN"))
(constant (+mpi-mode-excl+ "MPI_MODE_EXCL"))
(constant (+mpi-mode-append+ "MPI_MODE_APPEND"))
(constant (+mpi-mode-sequential+ "MPI_MODE_SEQUENTIAL"))
(constant (+mpi-displacement-current+ "MPI_DISPLACEMENT_CURRENT"))
(constant (+mpi-seek-set+ "MPI_SEEK_SET"))
(constant (+mpi-seek-cur+ "MPI_SEEK_CUR"))
(constant (+mpi-seek-end+ "MPI_SEEK_END"))
(constant (+mpi-max-datarep-string+ "MPI_MAX_DATAREP_STRING"))
(constant (+mpi-mode-nocheck+ "MPI_MODE_NOCHECK"))
(constant (+mpi-mode-noprecede+ "MPI_MODE_NOPRECEDE"))
(constant (+mpi-mode-noput+ "MPI_MODE_NOPUT"))
(constant (+mpi-mode-nostore+ "MPI_MODE_NOSTORE"))
(constant (+mpi-mode-nosucceed+ "MPI_MODE_NOSUCCEED"))
(constant (+mpi-lock-exclusive+ "MPI_LOCK_EXCLUSIVE"))
(constant (+mpi-lock-shared+ "MPI_LOCK_SHARED"))
(constant (+mpi-win-flavor-create+ "MPI_WIN_FLAVOR_CREATE"))
(constant (+mpi-win-flavor-allocate+ "MPI_WIN_FLAVOR_ALLOCATE"))
(constant (+mpi-win-flavor-dynamic+ "MPI_WIN_FLAVOR_DYNAMIC"))
(constant (+mpi-win-flavor-shared+ "MPI_WIN_FLAVOR_SHARED"))
(constant (+mpi-win-unified+ "MPI_WIN_UNIFIED"))
(constant (+mpi-win-separate+ "MPI_WIN_SEPARATE"))
(constant (+mpi-success+ "MPI_SUCCESS"))
(constant (+mpi-err-buffer+ "MPI_ERR_BUFFER"))
(constant (+mpi-err-count+ "MPI_ERR_COUNT"))
(constant (+mpi-err-type+ "MPI_ERR_TYPE"))
(constant (+mpi-err-tag+ "MPI_ERR_TAG"))
(constant (+mpi-err-comm+ "MPI_ERR_COMM"))
(constant (+mpi-err-rank+ "MPI_ERR_RANK"))
(constant (+mpi-err-request+ "MPI_ERR_REQUEST"))
(constant (+mpi-err-root+ "MPI_ERR_ROOT"))
(constant (+mpi-err-group+ "MPI_ERR_GROUP"))
(constant (+mpi-err-op+ "MPI_ERR_OP"))
(constant (+mpi-err-topology+ "MPI_ERR_TOPOLOGY"))
(constant (+mpi-err-dims+ "MPI_ERR_DIMS"))
(constant (+mpi-err-arg+ "MPI_ERR_ARG"))
(constant (+mpi-err-unknown+ "MPI_ERR_UNKNOWN"))
(constant (+mpi-err-truncate+ "MPI_ERR_TRUNCATE"))
(constant (+mpi-err-other+ "MPI_ERR_OTHER"))
(constant (+mpi-err-intern+ "MPI_ERR_INTERN"))
(constant (+mpi-err-in-status+ "MPI_ERR_IN_STATUS"))
(constant (+mpi-err-pending+ "MPI_ERR_PENDING"))
(constant (+mpi-err-access+ "MPI_ERR_ACCESS"))
(constant (+mpi-err-amode+ "MPI_ERR_AMODE"))
(constant (+mpi-err-assert+ "MPI_ERR_ASSERT"))
(constant (+mpi-err-bad-file+ "MPI_ERR_BAD_FILE"))
(constant (+mpi-err-base+ "MPI_ERR_BASE"))
(constant (+mpi-err-conversion+ "MPI_ERR_CONVERSION"))
(constant (+mpi-err-disp+ "MPI_ERR_DISP"))
(constant (+mpi-err-dup-datarep+ "MPI_ERR_DUP_DATAREP"))
(constant (+mpi-err-file-exists+ "MPI_ERR_FILE_EXISTS"))
(constant (+mpi-err-file-in-use+ "MPI_ERR_FILE_IN_USE"))
(constant (+mpi-err-file+ "MPI_ERR_FILE"))
(constant (+mpi-err-info-key+ "MPI_ERR_INFO_KEY"))
(constant (+mpi-err-info-nokey+ "MPI_ERR_INFO_NOKEY"))
(constant (+mpi-err-info-value+ "MPI_ERR_INFO_VALUE"))
(constant (+mpi-err-info+ "MPI_ERR_INFO"))
(constant (+mpi-err-io+ "MPI_ERR_IO"))
(constant (+mpi-err-keyval+ "MPI_ERR_KEYVAL"))
(constant (+mpi-err-locktype+ "MPI_ERR_LOCKTYPE"))
(constant (+mpi-err-name+ "MPI_ERR_NAME"))
(constant (+mpi-err-no-mem+ "MPI_ERR_NO_MEM"))
(constant (+mpi-err-not-same+ "MPI_ERR_NOT_SAME"))
(constant (+mpi-err-no-space+ "MPI_ERR_NO_SPACE"))
(constant (+mpi-err-no-such-file+ "MPI_ERR_NO_SUCH_FILE"))
(constant (+mpi-err-port+ "MPI_ERR_PORT"))
(constant (+mpi-err-quota+ "MPI_ERR_QUOTA"))
(constant (+mpi-err-read-only+ "MPI_ERR_READ_ONLY"))
(constant (+mpi-err-rma-conflict+ "MPI_ERR_RMA_CONFLICT"))
(constant (+mpi-err-rma-sync+ "MPI_ERR_RMA_SYNC"))
(constant (+mpi-err-service+ "MPI_ERR_SERVICE"))
(constant (+mpi-err-size+ "MPI_ERR_SIZE"))
(constant (+mpi-err-spawn+ "MPI_ERR_SPAWN"))
(constant (+mpi-err-unsupported-datarep+ "MPI_ERR_UNSUPPORTED_DATAREP"))
(constant (+mpi-err-unsupported-operation+ "MPI_ERR_UNSUPPORTED_OPERATION"))
(constant (+mpi-err-win+ "MPI_ERR_WIN"))
(constant (+mpi-t-err-memory+ "MPI_T_ERR_MEMORY"))
(constant (+mpi-t-err-not-initialized+ "MPI_T_ERR_NOT_INITIALIZED"))
(constant (+mpi-t-err-cannot-init+ "MPI_T_ERR_CANNOT_INIT"))
(constant (+mpi-t-err-invalid-index+ "MPI_T_ERR_INVALID_INDEX"))
(constant (+mpi-t-err-invalid-item+ "MPI_T_ERR_INVALID_ITEM"))
(constant (+mpi-t-err-invalid-handle+ "MPI_T_ERR_INVALID_HANDLE"))
(constant (+mpi-t-err-out-of-handles+ "MPI_T_ERR_OUT_OF_HANDLES"))
(constant (+mpi-t-err-out-of-sessions+ "MPI_T_ERR_OUT_OF_SESSIONS"))
(constant (+mpi-t-err-invalid-session+ "MPI_T_ERR_INVALID_SESSION"))
(constant (+mpi-t-err-cvar-set-not-now+ "MPI_T_ERR_CVAR_SET_NOT_NOW"))
(constant (+mpi-t-err-cvar-set-never+ "MPI_T_ERR_CVAR_SET_NEVER"))
(constant (+mpi-t-err-pvar-no-startstop+ "MPI_T_ERR_PVAR_NO_STARTSTOP"))
(constant (+mpi-t-err-pvar-no-write+ "MPI_T_ERR_PVAR_NO_WRITE"))
(constant (+mpi-t-err-pvar-no-atomic+ "MPI_T_ERR_PVAR_NO_ATOMIC"))
(constant (+mpi-err-rma-range+ "MPI_ERR_RMA_RANGE"))
(constant (+mpi-err-rma-attach+ "MPI_ERR_RMA_ATTACH"))
(constant (+mpi-err-rma-flavor+ "MPI_ERR_RMA_FLAVOR"))
(constant (+mpi-err-rma-shared+ "MPI_ERR_RMA_SHARED"))
(constant (+mpi-t-err-invalid+ "MPI_T_ERR_INVALID"))
(constant (+mpi-t-err-invalid-name+ "MPI_T_ERR_INVALID_NAME"))
(constant (+mpi-err-lastcode+ "MPI_ERR_LASTCODE"))
(constant (+mpi-typeclass-integer+ "MPI_TYPECLASS_INTEGER"))
(constant (+mpi-typeclass-real+ "MPI_TYPECLASS_REAL"))
(constant (+mpi-typeclass-complex+ "MPI_TYPECLASS_COMPLEX"))

(cenum mpi-predefined-attribute-keyvals
       ((:mpi-tag-ub "MPI_TAG_UB"))
       ((:mpi-host "MPI_HOST"))
       ((:mpi-io "MPI_IO"))
       ((:mpi-wtime-is-global "MPI_WTIME_IS_GLOBAL"))
       ((:mpi-appnum "MPI_APPNUM"))
       ((:mpi-lastusedcode "MPI_LASTUSEDCODE"))
       ((:mpi-universe-size "MPI_UNIVERSE_SIZE"))
       ((:mpi-win-base "MPI_WIN_BASE"))
       ((:mpi-win-size "MPI_WIN_SIZE"))
       ((:mpi-win-disp-unit "MPI_WIN_DISP_UNIT"))
       ((:mpi-win-create-flavor "MPI_WIN_CREATE_FLAVOR"))
       ((:mpi-win-model "MPI_WIN_MODEL")))

(cenum mpi-comparison-results
       ((:mpi-ident "MPI_IDENT"))
       ((:mpi-congruent "MPI_CONGRUENT"))
       ((:mpi-similar "MPI_SIMILAR"))
       ((:mpi-unequal "MPI_UNEQUAL")))

(cenum mpi-thread-options
       ((:mpi-thread-single "MPI_THREAD_SINGLE"))
       ((:mpi-thread-funneled "MPI_THREAD_FUNNELED"))
       ((:mpi-thread-serialized "MPI_THREAD_SERIALIZED"))
       ((:mpi-thread-multiple "MPI_THREAD_MULTIPLE")))

(cenum mpi-datatype-combiners
       ((:mpi-combiner-named "MPI_COMBINER_NAMED"))
       ((:mpi-combiner-dup "MPI_COMBINER_DUP"))
       ((:mpi-combiner-contiguous "MPI_COMBINER_CONTIGUOUS"))
       ((:mpi-combiner-vector "MPI_COMBINER_VECTOR"))
       ((:mpi-combiner-hvector-integer "MPI_COMBINER_HVECTOR_INTEGER"))
       ((:mpi-combiner-hvector "MPI_COMBINER_HVECTOR"))
       ((:mpi-combiner-indexed "MPI_COMBINER_INDEXED"))
       ((:mpi-combiner-hindexed-integer "MPI_COMBINER_HINDEXED_INTEGER"))
       ((:mpi-combiner-hindexed "MPI_COMBINER_HINDEXED"))
       ((:mpi-combiner-indexed-block "MPI_COMBINER_INDEXED_BLOCK"))
       ((:mpi-combiner-struct-integer "MPI_COMBINER_STRUCT_INTEGER"))
       ((:mpi-combiner-struct "MPI_COMBINER_STRUCT"))
       ((:mpi-combiner-subarray "MPI_COMBINER_SUBARRAY"))
       ((:mpi-combiner-darray "MPI_COMBINER_DARRAY"))
       ((:mpi-combiner-f90-real "MPI_COMBINER_F90_REAL"))
       ((:mpi-combiner-f90-complex "MPI_COMBINER_F90_COMPLEX"))
       ((:mpi-combiner-f90-integer "MPI_COMBINER_F90_INTEGER"))
       ((:mpi-combiner-resized "MPI_COMBINER_RESIZED"))
       ((:mpi-combiner-hindexed-block "MPI_COMBINER_HINDEXED_BLOCK")))

(cenum mpi-communicator-split-type-constants
       ((:mpi-comm-type-shared "MPI_COMM_TYPE_SHARED"))
       ;;
       ;; Defined for Open MPI ------------------------------------
       ;; ((:ompi-comm-type-hwthread "OMPI_COMM_TYPE_HWTHREAD"))
       ;; ((:ompi-comm-type-core "OMPI_COMM_TYPE_CORE"))
       ;; ((:ompi-comm-type-l1cache "OMPI_COMM_TYPE_L1CACHE"))
       ;; ((:ompi-comm-type-l2cache "OMPI_COMM_TYPE_L2CACHE"))
       ;; ((:ompi-comm-type-l3cache "OMPI_COMM_TYPE_L3CACHE"))
       ;; ((:ompi-comm-type-socket "OMPI_COMM_TYPE_SOCKET"))
       ;; ((:ompi-comm-type-numa "OMPI_COMM_TYPE_NUMA"))
       ;; ((:ompi-comm-type-board "OMPI_COMM_TYPE_BOARD"))
       ;; ((:ompi-comm-type-host "OMPI_COMM_TYPE_HOST"))
       ;; ((:ompi-comm-type-cu "OMPI_COMM_TYPE_CU"))
       ;; ((:ompi-comm-type-cluster "OMPI_COMM_TYPE_CLUSTER"))
       ;;
       ;; Defined for MPICH variants ------------------------------
       ;; ((:mpix-comm-type-neighborhood "MPIX_COMM_TYPE_NEIGHBORHOOD"))
       )

(cenum mpi-mpit-verbosity-levels
       ((:mpi-t-verbosity-user-basic "MPI_T_VERBOSITY_USER_BASIC"))
       ((:mpi-t-verbosity-user-detail "MPI_T_VERBOSITY_USER_DETAIL"))
       ((:mpi-t-verbosity-user-all "MPI_T_VERBOSITY_USER_ALL"))
       ((:mpi-t-verbosity-tuner-basic "MPI_T_VERBOSITY_TUNER_BASIC"))
       ((:mpi-t-verbosity-tuner-detail "MPI_T_VERBOSITY_TUNER_DETAIL"))
       ((:mpi-t-verbosity-tuner-all "MPI_T_VERBOSITY_TUNER_ALL"))
       ((:mpi-t-verbosity-mpidev-basic "MPI_T_VERBOSITY_MPIDEV_BASIC"))
       ((:mpi-t-verbosity-mpidev-detail "MPI_T_VERBOSITY_MPIDEV_DETAIL"))
       ((:mpi-t-verbosity-mpidev-all "MPI_T_VERBOSITY_MPIDEV_ALL")))

(cenum mpi-mpit-scopes
       ((:mpi-t-scope-constant "MPI_T_SCOPE_CONSTANT"))
       ((:mpi-t-scope-readonly "MPI_T_SCOPE_READONLY"))
       ((:mpi-t-scope-local "MPI_T_SCOPE_LOCAL"))
       ((:mpi-t-scope-group "MPI_T_SCOPE_GROUP"))
       ((:mpi-t-scope-group-eq "MPI_T_SCOPE_GROUP_EQ"))
       ((:mpi-t-scope-all "MPI_T_SCOPE_ALL"))
       ((:mpi-t-scope-all-eq "MPI_T_SCOPE_ALL_EQ")))

(cenum mpi-mpit-object-binding
       ((:mpi-t-bind-no-object "MPI_T_BIND_NO_OBJECT"))
       ((:mpi-t-bind-mpi-comm "MPI_T_BIND_MPI_COMM"))
       ((:mpi-t-bind-mpi-datatype "MPI_T_BIND_MPI_DATATYPE"))
       ((:mpi-t-bind-mpi-errhandler "MPI_T_BIND_MPI_ERRHANDLER"))
       ((:mpi-t-bind-mpi-file "MPI_T_BIND_MPI_FILE"))
       ((:mpi-t-bind-mpi-group "MPI_T_BIND_MPI_GROUP"))
       ((:mpi-t-bind-mpi-op "MPI_T_BIND_MPI_OP"))
       ((:mpi-t-bind-mpi-request "MPI_T_BIND_MPI_REQUEST"))
       ((:mpi-t-bind-mpi-win "MPI_T_BIND_MPI_WIN"))
       ((:mpi-t-bind-mpi-message "MPI_T_BIND_MPI_MESSAGE"))
       ((:mpi-t-bind-mpi-info "MPI_T_BIND_MPI_INFO")))

(cenum mpi-mpit-pvar-classes
       ((:mpi-t-pvar-class-state "MPI_T_PVAR_CLASS_STATE"))
       ((:mpi-t-pvar-class-level "MPI_T_PVAR_CLASS_LEVEL"))
       ((:mpi-t-pvar-class-size "MPI_T_PVAR_CLASS_SIZE"))
       ((:mpi-t-pvar-class-percentage "MPI_T_PVAR_CLASS_PERCENTAGE"))
       ((:mpi-t-pvar-class-highwatermark "MPI_T_PVAR_CLASS_HIGHWATERMARK"))
       ((:mpi-t-pvar-class-lowwatermark "MPI_T_PVAR_CLASS_LOWWATERMARK"))
       ((:mpi-t-pvar-class-counter "MPI_T_PVAR_CLASS_COUNTER"))
       ((:mpi-t-pvar-class-aggregate "MPI_T_PVAR_CLASS_AGGREGATE"))
       ((:mpi-t-pvar-class-timer "MPI_T_PVAR_CLASS_TIMER"))
       ((:mpi-t-pvar-class-generic "MPI_T_PVAR_CLASS_GENERIC")))

(cstruct mpi-status "MPI_Status"
         (mpi-source "MPI_SOURCE" :type :int)
         (mpi-tag "MPI_TAG" :type :int)
         (mpi-error "MPI_ERROR" :type :int))

(ctype mpi-aint "MPI_Aint")
(ctype mpi-offset "MPI_Offset")
(ctype mpi-count "MPI_Count")
