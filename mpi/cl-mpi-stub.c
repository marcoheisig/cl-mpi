
#include <mpi.h>

int mpi_stub_dummy_function() {
    int flag;
    MPI_Initialized(&flag);
    return flag;
}
