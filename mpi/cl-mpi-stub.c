
#include <mpi.h>

/*
  Unfortunately MPI makes no guarantees on the data type of its fundamental
  constants (MPI_COMM_WORLD and the like). In practice, MPICH and its
  derivatives represent all constants by integers, while OpenMPI uses
  pointers. The treatment of integers and pointers is unfortunately quite
  different from the Lisp perspective. The integers, or even worse, C
  preprocessor constants are best extracted with CFFI-GROVEL, while the
  pointers must be extracted at run time with FOREIGN-SYMBOL-POINTER. In
  order to simplify things, here are getter functions for all constants
  that work regardless of the type of the relevant constant. */

#define defgetter(type,name)                    \
    type cl_mpi_get_ ## name () {               \
        return name;                            \
    }

defgetter( MPI_Errhandler, MPI_ERRORS_RETURN )
defgetter( MPI_Errhandler, MPI_ERRORS_ARE_FATAL )
defgetter( MPI_Errhandler, MPI_ERRHANDLER_NULL )
defgetter( MPI_Group, MPI_GROUP_EMPTY )
defgetter( MPI_Group, MPI_GROUP_NULL )
defgetter( MPI_Comm, MPI_COMM_WORLD )
defgetter( MPI_Comm, MPI_COMM_SELF )
defgetter( MPI_Comm, MPI_COMM_NULL )
defgetter( MPI_Datatype, MPI_LB )
defgetter( MPI_Datatype, MPI_UB )
defgetter( MPI_Datatype, MPI_CHAR )
defgetter( MPI_Datatype, MPI_SIGNED_CHAR )
defgetter( MPI_Datatype, MPI_UNSIGNED_CHAR )
defgetter( MPI_Datatype, MPI_BYTE )
defgetter( MPI_Datatype, MPI_SHORT )
defgetter( MPI_Datatype, MPI_UNSIGNED_SHORT )
defgetter( MPI_Datatype, MPI_INT )
defgetter( MPI_Datatype, MPI_UNSIGNED )
defgetter( MPI_Datatype, MPI_LONG )
defgetter( MPI_Datatype, MPI_UNSIGNED_LONG )
defgetter( MPI_Datatype, MPI_LONG_LONG_INT )
defgetter( MPI_Datatype, MPI_UNSIGNED_LONG_LONG )
defgetter( MPI_Datatype, MPI_FLOAT )
defgetter( MPI_Datatype, MPI_DOUBLE )
defgetter( MPI_Datatype, MPI_LONG_DOUBLE )
defgetter( MPI_Datatype, MPI_WCHAR )
defgetter( MPI_Datatype, MPI_C_BOOL )
defgetter( MPI_Datatype, MPI_INT8_T )
defgetter( MPI_Datatype, MPI_INT16_T )
defgetter( MPI_Datatype, MPI_INT32_T )
defgetter( MPI_Datatype, MPI_INT64_T )
defgetter( MPI_Datatype, MPI_UINT8_T )
defgetter( MPI_Datatype, MPI_UINT16_T )
defgetter( MPI_Datatype, MPI_UINT32_T )
defgetter( MPI_Datatype, MPI_UINT64_T )
defgetter( MPI_Datatype, MPI_PACKED )
defgetter( MPI_Datatype, MPI_DATATYPE_NULL )
defgetter( MPI_Op, MPI_MIN )
defgetter( MPI_Op, MPI_MAX )
defgetter( MPI_Op, MPI_SUM )
defgetter( MPI_Op, MPI_PROD )
defgetter( MPI_Op, MPI_LAND )
defgetter( MPI_Op, MPI_BAND )
defgetter( MPI_Op, MPI_LOR )
defgetter( MPI_Op, MPI_BOR )
defgetter( MPI_Op, MPI_LXOR )
defgetter( MPI_Op, MPI_BXOR )
defgetter( MPI_Op, MPI_MAXLOC )
defgetter( MPI_Op, MPI_MINLOC )
defgetter( MPI_Op, MPI_REPLACE )
defgetter( MPI_Op, MPI_OP_NULL )
defgetter( MPI_Request, MPI_REQUEST_NULL )
defgetter( MPI_Status*, MPI_STATUS_IGNORE )

int cl_mpi_some_dummy_function() {
    int flag;
    MPI_Initialized(&flag);
    return flag;
}
