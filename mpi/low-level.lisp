#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI low-level function interface

Copyright (C) 2019  Juan M. Bello-Rivas <jbellorivas@rigetti.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

(in-package :cl-mpi)


(defcfun ("MPI_Abort" %mpi-abort)
    mpi-error-code
  (comm mpi-comm)
  (errorcode :int))


(defcfun ("MPI_Accumulate" %mpi-accumulate)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (op mpi-op)
  (win mpi-win))


(defcfun ("MPI_Add_error_class" %mpi-add-error-class)
    mpi-error-code
  (errorclass (:pointer :int)))


(defcfun ("MPI_Add_error_code" %mpi-add-error-code)
    mpi-error-code
  (errorclass :int)
  (errorcode (:pointer :int)))


(defcfun ("MPI_Add_error_string" %mpi-add-error-string)
    mpi-error-code
  (errorcode :int)
  (string (:pointer :char)))


(defcfun ("MPI_Allgather" %mpi-allgather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("MPI_Iallgather" %mpi-iallgather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Allgatherv" %mpi-allgatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("MPI_Iallgatherv" %mpi-iallgatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Alloc_mem" %mpi-alloc-mem)
    mpi-error-code
  (size mpi-aint)
  (info mpi-info)
  (baseptr :pointer))


(defcfun ("MPI_Allreduce" %mpi-allreduce)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("MPI_Iallreduce" %mpi-iallreduce)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Alltoall" %mpi-alltoall)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("MPI_Ialltoall" %mpi-ialltoall)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Alltoallv" %mpi-alltoallv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("MPI_Ialltoallv" %mpi-ialltoallv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Alltoallw" %mpi-alltoallw)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtypes (:pointer mpi-datatype))
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtypes (:pointer mpi-datatype))
  (comm mpi-comm))


(defcfun ("MPI_Ialltoallw" %mpi-ialltoallw)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtypes (:pointer mpi-datatype))
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtypes (:pointer mpi-datatype))
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Barrier" %mpi-barrier)
    mpi-error-code
  (comm mpi-comm))


(defcfun ("MPI_Ibarrier" %mpi-ibarrier)
    mpi-error-code
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Bcast" %mpi-bcast)
    mpi-error-code
  (buffer :pointer)
  (count :int)
  (datatype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("MPI_Bsend" %mpi-bsend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm))


(defcfun ("MPI_Ibcast" %mpi-ibcast)
    mpi-error-code
  (buffer :pointer)
  (count :int)
  (datatype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Bsend_init" %mpi-bsend-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Buffer_attach" %mpi-buffer-attach)
    mpi-error-code
  (buffer :pointer)
  (size :int))


(defcfun ("MPI_Buffer_detach" %mpi-buffer-detach)
    mpi-error-code
  (buffer :pointer)
  (size (:pointer :int)))


(defcfun ("MPI_Cancel" %mpi-cancel)
    mpi-error-code
  (request (:pointer mpi-request)))


(defcfun ("MPI_Cart_coords" %mpi-cart-coords)
    mpi-error-code
  (comm mpi-comm)
  (rank :int)
  (maxdims :int)
  (coords (:pointer :int)))


(defcfun ("MPI_Cart_create" %mpi-cart-create)
    mpi-error-code
  (old-comm mpi-comm)
  (ndims :int)
  (dims (:pointer :int))
  (periods (:pointer :int))
  (reorder :int)
  (comm-cart (:pointer mpi-comm)))


(defcfun ("MPI_Cart_get" %mpi-cart-get)
    mpi-error-code
  (comm mpi-comm)
  (maxdims :int)
  (dims (:pointer :int))
  (periods (:pointer :int))
  (coords (:pointer :int)))


(defcfun ("MPI_Cart_map" %mpi-cart-map)
    mpi-error-code
  (comm mpi-comm)
  (ndims :int)
  (dims (:pointer :int))
  (periods (:pointer :int))
  (newrank (:pointer :int)))


(defcfun ("MPI_Cart_rank" %mpi-cart-rank)
    mpi-error-code
  (comm mpi-comm)
  (coords (:pointer :int))
  (rank (:pointer :int)))


(defcfun ("MPI_Cart_shift" %mpi-cart-shift)
    mpi-error-code
  (comm mpi-comm)
  (direction :int)
  (disp :int)
  (rank-source (:pointer :int))
  (rank-dest (:pointer :int)))


(defcfun ("MPI_Cart_sub" %mpi-cart-sub)
    mpi-error-code
  (comm mpi-comm)
  (remain-dims (:pointer :int))
  (new-comm (:pointer mpi-comm)))


(defcfun ("MPI_Cartdim_get" %mpi-cartdim-get)
    mpi-error-code
  (comm mpi-comm)
  (ndims (:pointer :int)))


(defcfun ("MPI_Close_port" %mpi-close-port)
    mpi-error-code
  (port-name (:pointer :char)))


(defcfun ("MPI_Comm_accept" %mpi-comm-accept)
    mpi-error-code
  (port-name (:pointer :char))
  (info mpi-info)
  (root :int)
  (comm mpi-comm)
  (newcomm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_call_errhandler" %mpi-comm-call-errhandler)
    mpi-error-code
  (comm mpi-comm)
  (errorcode :int))


(defcfun ("MPI_Comm_compare" %mpi-comm-compare)
    mpi-error-code
  (comm1 mpi-comm)
  (comm2 mpi-comm)
  (result (:pointer :int)))


(defcfun ("MPI_Comm_connect" %mpi-comm-connect)
    mpi-error-code
  (port-name (:pointer :char))
  (info mpi-info)
  (root :int)
  (comm mpi-comm)
  (newcomm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_create_errhandler" %mpi-comm-create-errhandler)
    mpi-error-code
  #'(:pointer mpi-comm-errhandler-function)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("MPI_Comm_create_keyval" %mpi-comm-create-keyval)
    mpi-error-code
  (comm-copy-attr-fn (:pointer mpi-comm-copy-attr-function))
  (comm-delete-attr-fn (:pointer mpi-comm-delete-attr-function))
  (comm-keyval (:pointer :int))
  (extra-state :pointer))


(defcfun ("MPI_Comm_create_group" %mpi-comm-create-group)
    mpi-error-code
  (comm mpi-comm)
  (group mpi-group)
  (tag :int)
  (newcomm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_create" %mpi-comm-create)
    mpi-error-code
  (comm mpi-comm)
  (group mpi-group)
  (newcomm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_delete_attr" %mpi-comm-delete-attr)
    mpi-error-code
  (comm mpi-comm)
  (comm-keyval :int))


(defcfun ("MPI_Comm_disconnect" %mpi-comm-disconnect)
    mpi-error-code
  (comm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_dup" %mpi-comm-dup)
    mpi-error-code
  (comm mpi-comm)
  (newcomm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_idup" %mpi-comm-idup)
    mpi-error-code
  (comm mpi-comm)
  (newcomm (:pointer mpi-comm))
  (request (:pointer mpi-request)))


(defcfun ("MPI_Comm_dup_with_info" %mpi-comm-dup-with-info)
    mpi-error-code
  (comm mpi-comm)
  (info mpi-info)
  (newcomm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_free_keyval" %mpi-comm-free-keyval)
    mpi-error-code
  (comm-keyval (:pointer :int)))


(defcfun ("MPI_Comm_free" %mpi-comm-free)
    mpi-error-code
  (comm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_get_attr" %mpi-comm-get-attr)
    mpi-error-code
  (comm mpi-comm)
  (comm-keyval :int)
  (attribute-val :pointer)
  (flag (:pointer :int)))


(defcfun ("MPI_Dist_graph_create" %mpi-dist-graph-create)
    mpi-error-code
  (comm-old mpi-comm)
  (n :int)
  (nodes (:pointer :int))
  (degrees (:pointer :int))
  (targets (:pointer :int))
  (weights (:pointer :int))
  (info mpi-info)
  (reorder :int)
  (newcomm (:pointer mpi-comm)))


(defcfun ("MPI_Dist_graph_create_adjacent" %mpi-dist-graph-create-adjacent)
    mpi-error-code
  (comm-old mpi-comm)
  (indegree :int)
  (sources (:pointer :int))
  (sourceweights (:pointer :int))
  (outdegree :int)
  (destinations (:pointer :int))
  (destweights (:pointer :int))
  (info mpi-info)
  (reorder :int)
  (comm-dist-graph (:pointer mpi-comm)))


(defcfun ("MPI_Dist_graph_neighbors" %mpi-dist-graph-neighbors)
    mpi-error-code
  (comm mpi-comm)
  (maxindegree :int)
  (sources (:pointer :int))
  (sourceweights (:pointer :int))
  (maxoutdegree :int)
  (destinations (:pointer :int))
  (destweights (:pointer :int)))


(defcfun ("MPI_Dist_graph_neighbors_count" %mpi-dist-graph-neighbors-count)
    mpi-error-code
  (comm mpi-comm)
  (inneighbors (:pointer :int))
  (outneighbors (:pointer :int))
  (weighted (:pointer :int)))


(defcfun ("MPI_Comm_get_errhandler" %mpi-comm-get-errhandler)
    mpi-error-code
  (comm mpi-comm)
  (erhandler (:pointer mpi-errhandler)))


(defcfun ("MPI_Comm_get_info" %mpi-comm-get-info)
    mpi-error-code
  (comm mpi-comm)
  (info-used (:pointer mpi-info)))


(defcfun ("MPI_Comm_get_name" %mpi-comm-get-name)
    mpi-error-code
  (comm mpi-comm)
  (comm-name (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("MPI_Comm_get_parent" %mpi-comm-get-parent)
    mpi-error-code
  (parent (:pointer mpi-comm)))


(defcfun ("MPI_Comm_group" %mpi-comm-group)
    mpi-error-code
  (comm mpi-comm)
  (group (:pointer mpi-group)))


(defcfun ("MPI_Comm_join" %mpi-comm-join)
    mpi-error-code
  (fd :int)
  (intercomm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_rank" %mpi-comm-rank)
    mpi-error-code
  (comm mpi-comm)
  (rank (:pointer :int)))


(defcfun ("MPI_Comm_remote_group" %mpi-comm-remote-group)
    mpi-error-code
  (comm mpi-comm)
  (group (:pointer mpi-group)))


(defcfun ("MPI_Comm_remote_size" %mpi-comm-remote-size)
    mpi-error-code
  (comm mpi-comm)
  (size (:pointer :int)))


(defcfun ("MPI_Comm_set_attr" %mpi-comm-set-attr)
    mpi-error-code
  (comm mpi-comm)
  (comm-keyval :int)
  (attribute-val :pointer))


(defcfun ("MPI_Comm_set_errhandler" %mpi-comm-set-errhandler)
    mpi-error-code
  (comm mpi-comm)
  (errhandler mpi-errhandler))


(defcfun ("MPI_Comm_set_info" %mpi-comm-set-info)
    mpi-error-code
  (comm mpi-comm)
  (info mpi-info))


(defcfun ("MPI_Comm_set_name" %mpi-comm-set-name)
    mpi-error-code
  (comm mpi-comm)
  (comm-name (:pointer :char)))


(defcfun ("MPI_Comm_size" %mpi-comm-size)
    mpi-error-code
  (comm mpi-comm)
  (size (:pointer :int)))


(defcfun ("MPI_Comm_spawn" %mpi-comm-spawn)
    mpi-error-code
  (command (:pointer :char))
  (argv :pointer)
  (maxprocs :int)
  (info mpi-info)
  (root :int)
  (comm mpi-comm)
  (intercomm (:pointer mpi-comm))
  (array-of-errcodes (:pointer :int)))


(defcfun ("MPI_Comm_spawn_multiple" %mpi-comm-spawn-multiple)
    mpi-error-code
  (count :int)
  (array-of-commands :pointer)
  (array-of-argv :pointer)
  (array-of-maxprocs (:pointer :int))
  (array-of-info (:pointer mpi-info))
  (root :int)
  (comm mpi-comm)
  (intercomm (:pointer mpi-comm))
  (array-of-errcodes (:pointer :int)))


(defcfun ("MPI_Comm_split" %mpi-comm-split)
    mpi-error-code
  (comm mpi-comm)
  (color :int)
  (key :int)
  (newcomm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_split_type" %mpi-comm-split-type)
    mpi-error-code
  (comm mpi-comm)
  (split-type :int)
  (key :int)
  (info mpi-info)
  (newcomm (:pointer mpi-comm)))


(defcfun ("MPI_Comm_test_inter" %mpi-comm-test-inter)
    mpi-error-code
  (comm mpi-comm)
  (flag (:pointer :int)))


(defcfun ("MPI_Compare_and_swap" %mpi-compare-and-swap)
    mpi-error-code
  (origin-addr :pointer)
  (compare-addr :pointer)
  (result-addr :pointer)
  (datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (win mpi-win))


(defcfun ("MPI_Dims_create" %mpi-dims-create)
    mpi-error-code
  (nnodes :int)
  (ndims :int)
  (dims (:pointer :int)))


(defcfun ("MPI_Errhandler_free" %mpi-errhandler-free)
    mpi-error-code
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("MPI_Error_class" %mpi-error-class)
    mpi-error-code
  (errorcode :int)
  (errorclass (:pointer :int)))


(defcfun ("MPI_Error_string" %mpi-error-string)
    mpi-error-code
  (errorcode :int)
  (string (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("MPI_Exscan" %mpi-exscan)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("MPI_Fetch_and_op" %mpi-fetch-and-op)
    mpi-error-code
  (origin-addr :pointer)
  (result-addr :pointer)
  (datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (op mpi-op)
  (win mpi-win))


(defcfun ("MPI_Iexscan" %mpi-iexscan)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_call_errhandler" %mpi-file-call-errhandler)
    mpi-error-code
  (fh mpi-file)
  (errorcode :int))


(defcfun ("MPI_File_create_errhandler" %mpi-file-create-errhandler)
    mpi-error-code
  #'(:pointer mpi-file-errhandler-function)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("MPI_File_set_errhandler" %mpi-file-set-errhandler)
    mpi-error-code
  (file mpi-file)
  (errhandler mpi-errhandler))


(defcfun ("MPI_File_get_errhandler" %mpi-file-get-errhandler)
    mpi-error-code
  (file mpi-file)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("MPI_File_open" %mpi-file-open)
    mpi-error-code
  (comm mpi-comm)
  (filename (:pointer :char))
  (amode :int)
  (info mpi-info)
  (fh (:pointer mpi-file)))


(defcfun ("MPI_File_close" %mpi-file-close)
    mpi-error-code
  (fh (:pointer mpi-file)))


(defcfun ("MPI_File_delete" %mpi-file-delete)
    mpi-error-code
  (filename (:pointer :char))
  (info mpi-info))


(defcfun ("MPI_File_set_size" %mpi-file-set-size)
    mpi-error-code
  (fh mpi-file)
  (size mpi-offset))


(defcfun ("MPI_File_preallocate" %mpi-file-preallocate)
    mpi-error-code
  (fh mpi-file)
  (size mpi-offset))


(defcfun ("MPI_File_get_size" %mpi-file-get-size)
    mpi-error-code
  (fh mpi-file)
  (size (:pointer mpi-offset)))


(defcfun ("MPI_File_get_group" %mpi-file-get-group)
    mpi-error-code
  (fh mpi-file)
  (group (:pointer mpi-group)))


(defcfun ("MPI_File_get_amode" %mpi-file-get-amode)
    mpi-error-code
  (fh mpi-file)
  (amode (:pointer :int)))


(defcfun ("MPI_File_set_info" %mpi-file-set-info)
    mpi-error-code
  (fh mpi-file)
  (info mpi-info))


(defcfun ("MPI_File_get_info" %mpi-file-get-info)
    mpi-error-code
  (fh mpi-file)
  (info-used (:pointer mpi-info)))


(defcfun ("MPI_File_set_view" %mpi-file-set-view)
    mpi-error-code
  (fh mpi-file)
  (disp mpi-offset)
  (etype mpi-datatype)
  (filetype mpi-datatype)
  (datarep (:pointer :char))
  (info mpi-info))


(defcfun ("MPI_File_get_view" %mpi-file-get-view)
    mpi-error-code
  (fh mpi-file)
  (disp (:pointer mpi-offset))
  (etype (:pointer mpi-datatype))
  (filetype (:pointer mpi-datatype))
  (datarep (:pointer :char)))


(defcfun ("MPI_File_read_at" %mpi-file-read-at)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_read_at_all" %mpi-file-read-at-all)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_write_at" %mpi-file-write-at)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_write_at_all" %mpi-file-write-at-all)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_iread_at" %mpi-file-iread-at)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_iwrite_at" %mpi-file-iwrite-at)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_iread_at_all" %mpi-file-iread-at-all)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_iwrite_at_all" %mpi-file-iwrite-at-all)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_read" %mpi-file-read)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_read_all" %mpi-file-read-all)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_write" %mpi-file-write)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_write_all" %mpi-file-write-all)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_iread" %mpi-file-iread)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_iwrite" %mpi-file-iwrite)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_iread_all" %mpi-file-iread-all)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_iwrite_all" %mpi-file-iwrite-all)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_seek" %mpi-file-seek)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (whence :int))


(defcfun ("MPI_File_get_position" %mpi-file-get-position)
    mpi-error-code
  (fh mpi-file)
  (offset (:pointer mpi-offset)))


(defcfun ("MPI_File_get_byte_offset" %mpi-file-get-byte-offset)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (disp (:pointer mpi-offset)))


(defcfun ("MPI_File_read_shared" %mpi-file-read-shared)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_write_shared" %mpi-file-write-shared)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_iread_shared" %mpi-file-iread-shared)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_iwrite_shared" %mpi-file-iwrite-shared)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("MPI_File_read_ordered" %mpi-file-read-ordered)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_write_ordered" %mpi-file-write-ordered)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_seek_shared" %mpi-file-seek-shared)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (whence :int))


(defcfun ("MPI_File_get_position_shared" %mpi-file-get-position-shared)
    mpi-error-code
  (fh mpi-file)
  (offset (:pointer mpi-offset)))


(defcfun ("MPI_File_read_at_all_begin" %mpi-file-read-at-all-begin)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("MPI_File_read_at_all_end" %mpi-file-read-at-all-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_write_at_all_begin" %mpi-file-write-at-all-begin)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("MPI_File_write_at_all_end" %mpi-file-write-at-all-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_read_all_begin" %mpi-file-read-all-begin)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("MPI_File_read_all_end" %mpi-file-read-all-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_write_all_begin" %mpi-file-write-all-begin)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("MPI_File_write_all_end" %mpi-file-write-all-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_read_ordered_begin" %mpi-file-read-ordered-begin)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("MPI_File_read_ordered_end" %mpi-file-read-ordered-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_write_ordered_begin" %mpi-file-write-ordered-begin)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("MPI_File_write_ordered_end" %mpi-file-write-ordered-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_File_get_type_extent" %mpi-file-get-type-extent)
    mpi-error-code
  (fh mpi-file)
  (datatype mpi-datatype)
  (extent (:pointer mpi-aint)))


(defcfun ("MPI_File_set_atomicity" %mpi-file-set-atomicity)
    mpi-error-code
  (fh mpi-file)
  (flag :int))


(defcfun ("MPI_File_get_atomicity" %mpi-file-get-atomicity)
    mpi-error-code
  (fh mpi-file)
  (flag (:pointer :int)))


(defcfun ("MPI_File_sync" %mpi-file-sync)
    mpi-error-code
  (fh mpi-file))


(defcfun ("MPI_Finalize" %mpi-finalize)
    mpi-error-code)


(defcfun ("MPI_Finalized" %mpi-finalized)
    mpi-error-code
  (flag (:pointer :int)))


(defcfun ("MPI_Free_mem" %mpi-free-mem)
    mpi-error-code
  (base :pointer))


(defcfun ("MPI_Gather" %mpi-gather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("MPI_Igather" %mpi-igather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Gatherv" %mpi-gatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("MPI_Igatherv" %mpi-igatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Get_address" %mpi-get-address)
    mpi-error-code
  (location :pointer)
  (address (:pointer mpi-aint)))


(defcfun ("MPI_Get_count" %mpi-get-count)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count (:pointer :int)))


(defcfun ("MPI_Get_elements" %mpi-get-elements)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count (:pointer :int)))


(defcfun ("MPI_Get_elements_x" %mpi-get-elements-x)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count (:pointer mpi-count)))


(defcfun ("MPI_Get" %mpi-get)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (win mpi-win))


(defcfun ("MPI_Get_accumulate" %mpi-get-accumulate)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (result-addr :pointer)
  (result-count :int)
  (result-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (op mpi-op)
  (win mpi-win))


(defcfun ("MPI_Get_library_version" %mpi-get-library-version)
    mpi-error-code
  (version (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("MPI_Get_processor_name" %mpi-get-processor-name)
    mpi-error-code
  (name (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("MPI_Get_version" %mpi-get-version)
    mpi-error-code
  (version (:pointer :int))
  (subversion (:pointer :int)))


(defcfun ("MPI_Graph_create" %mpi-graph-create)
    mpi-error-code
  (comm-old mpi-comm)
  (nnodes :int)
  (index (:pointer :int))
  (edges (:pointer :int))
  (reorder :int)
  (comm-graph (:pointer mpi-comm)))


(defcfun ("MPI_Graph_get" %mpi-graph-get)
    mpi-error-code
  (comm mpi-comm)
  (maxindex :int)
  (maxedges :int)
  (index (:pointer :int))
  (edges (:pointer :int)))


(defcfun ("MPI_Graph_map" %mpi-graph-map)
    mpi-error-code
  (comm mpi-comm)
  (nnodes :int)
  (index (:pointer :int))
  (edges (:pointer :int))
  (newrank (:pointer :int)))


(defcfun ("MPI_Graph_neighbors_count" %mpi-graph-neighbors-count)
    mpi-error-code
  (comm mpi-comm)
  (rank :int)
  (nneighbors (:pointer :int)))


(defcfun ("MPI_Graph_neighbors" %mpi-graph-neighbors)
    mpi-error-code
  (comm mpi-comm)
  (rank :int)
  (maxneighbors :int)
  (neighbors (:pointer :int)))


(defcfun ("MPI_Graphdims_get" %mpi-graphdims-get)
    mpi-error-code
  (comm mpi-comm)
  (nnodes (:pointer :int))
  (nedges (:pointer :int)))


(defcfun ("MPI_Grequest_complete" %mpi-grequest-complete)
    mpi-error-code
  (request mpi-request))


(defcfun ("MPI_Grequest_start" %mpi-grequest-start)
    mpi-error-code
  (query-fn (:pointer mpi-grequest-query-function))
  (free-fn (:pointer mpi-grequest-free-function))
  (cancel-fn (:pointer mpi-grequest-cancel-function))
  (extra-state :pointer)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Group_compare" %mpi-group-compare)
    mpi-error-code
  (group1 mpi-group)
  (group2 mpi-group)
  (result (:pointer :int)))


(defcfun ("MPI_Group_difference" %mpi-group-difference)
    mpi-error-code
  (group1 mpi-group)
  (group2 mpi-group)
  (newgroup (:pointer mpi-group)))


(defcfun ("MPI_Group_excl" %mpi-group-excl)
    mpi-error-code
  (group mpi-group)
  (n :int)
  (ranks (:pointer :int))
  (newgroup (:pointer mpi-group)))


(defcfun ("MPI_Group_free" %mpi-group-free)
    mpi-error-code
  (group (:pointer mpi-group)))


(defcfun ("MPI_Group_incl" %mpi-group-incl)
    mpi-error-code
  (group mpi-group)
  (n :int)
  (ranks (:pointer :int))
  (newgroup (:pointer mpi-group)))


(defcfun ("MPI_Group_intersection" %mpi-group-intersection)
    mpi-error-code
  (group1 mpi-group)
  (group2 mpi-group)
  (newgroup (:pointer mpi-group)))


(defcfun ("MPI_Group_range_excl" %mpi-group-range-excl)
    mpi-error-code
  (group mpi-group)
  (n :int)
  (ranges :pointer)
  (newgroup (:pointer mpi-group)))


(defcfun ("MPI_Group_range_incl" %mpi-group-range-incl)
    mpi-error-code
  (group mpi-group)
  (n :int)
  (ranges :pointer)
  (newgroup (:pointer mpi-group)))


(defcfun ("MPI_Group_rank" %mpi-group-rank)
    mpi-error-code
  (group mpi-group)
  (rank (:pointer :int)))


(defcfun ("MPI_Group_size" %mpi-group-size)
    mpi-error-code
  (group mpi-group)
  (size (:pointer :int)))


(defcfun ("MPI_Group_translate_ranks" %mpi-group-translate-ranks)
    mpi-error-code
  (group1 mpi-group)
  (n :int)
  (ranks1 (:pointer :int))
  (group2 mpi-group)
  (ranks2 (:pointer :int)))


(defcfun ("MPI_Group_union" %mpi-group-union)
    mpi-error-code
  (group1 mpi-group)
  (group2 mpi-group)
  (newgroup (:pointer mpi-group)))


(defcfun ("MPI_Ibsend" %mpi-ibsend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Improbe" %mpi-improbe)
    mpi-error-code
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (flag (:pointer :int))
  (message (:pointer mpi-message))
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Imrecv" %mpi-imrecv)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (type mpi-datatype)
  (message (:pointer mpi-message))
  (request (:pointer mpi-request)))


(defcfun ("MPI_Info_create" %mpi-info-create)
    mpi-error-code
  (info (:pointer mpi-info)))


(defcfun ("MPI_Info_delete" %mpi-info-delete)
    mpi-error-code
  (info mpi-info)
  (key (:pointer :char)))


(defcfun ("MPI_Info_dup" %mpi-info-dup)
    mpi-error-code
  (info mpi-info)
  (newinfo (:pointer mpi-info)))


(defcfun ("MPI_Info_free" %mpi-info-free)
    mpi-error-code
  (info (:pointer mpi-info)))


(defcfun ("MPI_Info_get" %mpi-info-get)
    mpi-error-code
  (info mpi-info)
  (key (:pointer :char))
  (valuelen :int)
  (value (:pointer :char))
  (flag (:pointer :int)))


(defcfun ("MPI_Info_get_nkeys" %mpi-info-get-nkeys)
    mpi-error-code
  (info mpi-info)
  (nkeys (:pointer :int)))


(defcfun ("MPI_Info_get_nthkey" %mpi-info-get-nthkey)
    mpi-error-code
  (info mpi-info)
  (n :int)
  (key (:pointer :char)))


(defcfun ("MPI_Info_get_valuelen" %mpi-info-get-valuelen)
    mpi-error-code
  (info mpi-info)
  (key (:pointer :char))
  (valuelen (:pointer :int))
  (flag (:pointer :int)))


(defcfun ("MPI_Info_set" %mpi-info-set)
    mpi-error-code
  (info mpi-info)
  (key (:pointer :char))
  (value (:pointer :char)))


(defcfun ("MPI_Init" %mpi-init)
    mpi-error-code
  (argc (:pointer :int))
  (argv :pointer))


(defcfun ("MPI_Initialized" %mpi-initialized)
    mpi-error-code
  (flag (:pointer :int)))


(defcfun ("MPI_Init_thread" %mpi-init-thread)
    mpi-error-code
  (argc (:pointer :int))
  (argv :pointer)
  (required :int)
  (provided (:pointer :int)))


(defcfun ("MPI_Intercomm_create" %mpi-intercomm-create)
    mpi-error-code
  (local-comm mpi-comm)
  (local-leader :int)
  (bridge-comm mpi-comm)
  (remote-leader :int)
  (tag :int)
  (newintercomm (:pointer mpi-comm)))


(defcfun ("MPI_Intercomm_merge" %mpi-intercomm-merge)
    mpi-error-code
  (intercomm mpi-comm)
  (high :int)
  (newintercomm (:pointer mpi-comm)))


(defcfun ("MPI_Iprobe" %mpi-iprobe)
    mpi-error-code
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (flag (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Irecv" %mpi-irecv)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Irsend" %mpi-irsend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Isend" %mpi-isend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Issend" %mpi-issend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Is_thread_main" %mpi-is-thread-main)
    mpi-error-code
  (flag (:pointer :int)))


(defcfun ("MPI_Lookup_name" %mpi-lookup-name)
    mpi-error-code
  (service-name (:pointer :char))
  (info mpi-info)
  (port-name (:pointer :char)))


(defcfun ("MPI_Mprobe" %mpi-mprobe)
    mpi-error-code
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (message (:pointer mpi-message))
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Mrecv" %mpi-mrecv)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (type mpi-datatype)
  (message (:pointer mpi-message))
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Neighbor_allgather" %mpi-neighbor-allgather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("MPI_Ineighbor_allgather" %mpi-ineighbor-allgather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Neighbor_allgatherv" %mpi-neighbor-allgatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("MPI_Ineighbor_allgatherv" %mpi-ineighbor-allgatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Neighbor_alltoall" %mpi-neighbor-alltoall)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("MPI_Ineighbor_alltoall" %mpi-ineighbor-alltoall)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Neighbor_alltoallv" %mpi-neighbor-alltoallv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("MPI_Ineighbor_alltoallv" %mpi-ineighbor-alltoallv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Neighbor_alltoallw" %mpi-neighbor-alltoallw)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer mpi-aint))
  (sendtypes (:pointer mpi-datatype))
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer mpi-aint))
  (recvtypes (:pointer mpi-datatype))
  (comm mpi-comm))


(defcfun ("MPI_Ineighbor_alltoallw" %mpi-ineighbor-alltoallw)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer mpi-aint))
  (sendtypes (:pointer mpi-datatype))
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer mpi-aint))
  (recvtypes (:pointer mpi-datatype))
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Op_commutative" %mpi-op-commutative)
    mpi-error-code
  (op mpi-op)
  (commute (:pointer :int)))


(defcfun ("MPI_Op_create" %mpi-op-create)
    mpi-error-code
  #'(:pointer mpi-user-function)
  (commute :int)
  (op (:pointer mpi-op)))


(defcfun ("MPI_Open_port" %mpi-open-port)
    mpi-error-code
  (info mpi-info)
  (port-name (:pointer :char)))


(defcfun ("MPI_Op_free" %mpi-op-free)
    mpi-error-code
  (op (:pointer mpi-op)))


(defcfun ("MPI_Pack_external" %mpi-pack-external)
    mpi-error-code
  (datarep (:pointer :char))
  (inbuf :pointer)
  (incount :int)
  (datatype mpi-datatype)
  (outbuf :pointer)
  (outsize mpi-aint)
  (position (:pointer mpi-aint)))


(defcfun ("MPI_Pack_external_size" %mpi-pack-external-size)
    mpi-error-code
  (datarep (:pointer :char))
  (incount :int)
  (datatype mpi-datatype)
  (size (:pointer mpi-aint)))


(defcfun ("MPI_Pack" %mpi-pack)
    mpi-error-code
  (inbuf :pointer)
  (incount :int)
  (datatype mpi-datatype)
  (outbuf :pointer)
  (outsize :int)
  (position (:pointer :int))
  (comm mpi-comm))


(defcfun ("MPI_Pack_size" %mpi-pack-size)
    mpi-error-code
  (incount :int)
  (datatype mpi-datatype)
  (comm mpi-comm)
  (size (:pointer :int)))


(defcfun ("MPI_Pcontrol" %mpi-pcontrol)
    mpi-error-code
  (level :int))


(defcfun ("MPI_Probe" %mpi-probe)
    mpi-error-code
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Publish_name" %mpi-publish-name)
    mpi-error-code
  (service-name (:pointer :char))
  (info mpi-info)
  (port-name (:pointer :char)))


(defcfun ("MPI_Put" %mpi-put)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (win mpi-win))


(defcfun ("MPI_Query_thread" %mpi-query-thread)
    mpi-error-code
  (provided (:pointer :int)))


(defcfun ("MPI_Raccumulate" %mpi-raccumulate)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (op mpi-op)
  (win mpi-win)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Recv_init" %mpi-recv-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Recv" %mpi-recv)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Reduce" %mpi-reduce)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (root :int)
  (comm mpi-comm))


(defcfun ("MPI_Ireduce" %mpi-ireduce)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Reduce_local" %mpi-reduce-local)
    mpi-error-code
  (inbuf :pointer)
  (inoutbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op))


(defcfun ("MPI_Reduce_scatter" %mpi-reduce-scatter)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("MPI_Ireduce_scatter" %mpi-ireduce-scatter)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Reduce_scatter_block" %mpi-reduce-scatter-block)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (recvcount :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("MPI_Ireduce_scatter_block" %mpi-ireduce-scatter-block)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (recvcount :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Register_datarep" %mpi-register-datarep)
    mpi-error-code
  (datarep (:pointer :char))
  (read-conversion-fn (:pointer mpi-datarep-conversion-function))
  (write-conversion-fn (:pointer mpi-datarep-conversion-function))
  (dtype-file-extent-fn (:pointer mpi-datarep-extent-function))
  (extra-state :pointer))


(defcfun ("MPI_Request_free" %mpi-request-free)
    mpi-error-code
  (request (:pointer mpi-request)))


(defcfun ("MPI_Request_get_status" %mpi-request-get-status)
    mpi-error-code
  (request mpi-request)
  (flag (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Rget" %mpi-rget)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (win mpi-win)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Rget_accumulate" %mpi-rget-accumulate)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (result-addr :pointer)
  (result-count :int)
  (result-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (op mpi-op)
  (win mpi-win)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Rput" %mpi-rput)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-cout :int)
  (target-datatype mpi-datatype)
  (win mpi-win)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Rsend" %mpi-rsend)
    mpi-error-code
  (ibuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm))


(defcfun ("MPI_Rsend_init" %mpi-rsend-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Scan" %mpi-scan)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("MPI_Iscan" %mpi-iscan)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Scatter" %mpi-scatter)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("MPI_Iscatter" %mpi-iscatter)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Scatterv" %mpi-scatterv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (displs (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("MPI_Iscatterv" %mpi-iscatterv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (displs (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Send_init" %mpi-send-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Send" %mpi-send)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm))


(defcfun ("MPI_Sendrecv" %mpi-sendrecv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (dest :int)
  (sendtag :int)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (source :int)
  (recvtag :int)
  (comm mpi-comm)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Sendrecv_replace" %mpi-sendrecv-replace)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (sendtag :int)
  (source :int)
  (recvtag :int)
  (comm mpi-comm)
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Ssend_init" %mpi-ssend-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("MPI_Ssend" %mpi-ssend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm))


(defcfun ("MPI_Start" %mpi-start)
    mpi-error-code
  (request (:pointer mpi-request)))


(defcfun ("MPI_Startall" %mpi-startall)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request)))


(defcfun ("MPI_Status_set_cancelled" %mpi-status-set-cancelled)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (flag :int))


(defcfun ("MPI_Status_set_elements" %mpi-status-set-elements)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count :int))


(defcfun ("MPI_Status_set_elements_x" %mpi-status-set-elements-x)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count mpi-count))


(defcfun ("MPI_Testall" %mpi-testall)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request))
  (flag (:pointer :int))
  (array-of-statuses (:pointer (:struct mpi-status))))


(defcfun ("MPI_Testany" %mpi-testany)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request))
  (index (:pointer :int))
  (flag (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Test" %mpi-test)
    mpi-error-code
  (request (:pointer mpi-request))
  (flag (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Test_cancelled" %mpi-test-cancelled)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (flag (:pointer :int)))


(defcfun ("MPI_Testsome" %mpi-testsome)
    mpi-error-code
  (incount :int)
  (array-of-requests (:pointer mpi-request))
  (outcount (:pointer :int))
  (array-of-indices (:pointer :int))
  (array-of-statuses (:pointer (:struct mpi-status))))


(defcfun ("MPI_Topo_test" %mpi-topo-test)
    mpi-error-code
  (comm mpi-comm)
  (status (:pointer :int)))


(defcfun ("MPI_Type_commit" %mpi-type-commit)
    mpi-error-code
  (type (:pointer mpi-datatype)))


(defcfun ("MPI_Type_contiguous" %mpi-type-contiguous)
    mpi-error-code
  (count :int)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_darray" %mpi-type-create-darray)
    mpi-error-code
  (size :int)
  (rank :int)
  (ndims :int)
  (gsize-array (:pointer :int))
  (distrib-array (:pointer :int))
  (darg-array (:pointer :int))
  (psize-array (:pointer :int))
  (order :int)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_f90_complex" %mpi-type-create-f90-complex)
    mpi-error-code
  (p :int)
  (r :int)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_f90_integer" %mpi-type-create-f90-integer)
    mpi-error-code
  (r :int)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_f90_real" %mpi-type-create-f90-real)
    mpi-error-code
  (p :int)
  (r :int)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_hindexed_block" %mpi-type-create-hindexed-block)
    mpi-error-code
  (count :int)
  (blocklength :int)
  (array-of-displacements (:pointer mpi-aint))
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_hindexed" %mpi-type-create-hindexed)
    mpi-error-code
  (count :int)
  (array-of-blocklengths (:pointer :int))
  (array-of-displacements (:pointer mpi-aint))
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_hvector" %mpi-type-create-hvector)
    mpi-error-code
  (count :int)
  (blocklength :int)
  (stride mpi-aint)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_keyval" %mpi-type-create-keyval)
    mpi-error-code
  (type-copy-attr-fn (:pointer mpi-type-copy-attr-function))
  (type-delete-attr-fn (:pointer mpi-type-delete-attr-function))
  (type-keyval (:pointer :int))
  (extra-state :pointer))


(defcfun ("MPI_Type_create_indexed_block" %mpi-type-create-indexed-block)
    mpi-error-code
  (count :int)
  (blocklength :int)
  (array-of-displacements (:pointer :int))
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_struct" %mpi-type-create-struct)
    mpi-error-code
  (count :int)
  (array-of-block-lengths (:pointer :int))
  (array-of-displacements (:pointer mpi-aint))
  (array-of-types (:pointer mpi-datatype))
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_subarray" %mpi-type-create-subarray)
    mpi-error-code
  (ndims :int)
  (size-array (:pointer :int))
  (subsize-array (:pointer :int))
  (start-array (:pointer :int))
  (order :int)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_create_resized" %mpi-type-create-resized)
    mpi-error-code
  (oldtype mpi-datatype)
  (lb mpi-aint)
  (extent mpi-aint)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_delete_attr" %mpi-type-delete-attr)
    mpi-error-code
  (type mpi-datatype)
  (type-keyval :int))


(defcfun ("MPI_Type_dup" %mpi-type-dup)
    mpi-error-code
  (type mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_free" %mpi-type-free)
    mpi-error-code
  (type (:pointer mpi-datatype)))


(defcfun ("MPI_Type_free_keyval" %mpi-type-free-keyval)
    mpi-error-code
  (type-keyval (:pointer :int)))


(defcfun ("MPI_Type_get_attr" %mpi-type-get-attr)
    mpi-error-code
  (type mpi-datatype)
  (type-keyval :int)
  (attribute-val :pointer)
  (flag (:pointer :int)))


(defcfun ("MPI_Type_get_contents" %mpi-type-get-contents)
    mpi-error-code
  (mtype mpi-datatype)
  (max-integers :int)
  (max-addresses :int)
  (max-datatypes :int)
  (array-of-integers (:pointer :int))
  (array-of-addresses (:pointer mpi-aint))
  (array-of-datatypes (:pointer mpi-datatype)))


(defcfun ("MPI_Type_get_envelope" %mpi-type-get-envelope)
    mpi-error-code
  (type mpi-datatype)
  (num-integers (:pointer :int))
  (num-addresses (:pointer :int))
  (num-datatypes (:pointer :int))
  (combiner (:pointer :int)))


(defcfun ("MPI_Type_get_extent" %mpi-type-get-extent)
    mpi-error-code
  (type mpi-datatype)
  (lb (:pointer mpi-aint))
  (extent (:pointer mpi-aint)))


(defcfun ("MPI_Type_get_extent_x" %mpi-type-get-extent-x)
    mpi-error-code
  (type mpi-datatype)
  (lb (:pointer mpi-count))
  (extent (:pointer mpi-count)))


(defcfun ("MPI_Type_get_name" %mpi-type-get-name)
    mpi-error-code
  (type mpi-datatype)
  (type-name (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("MPI_Type_get_true_extent" %mpi-type-get-true-extent)
    mpi-error-code
  (datatype mpi-datatype)
  (true-lb (:pointer mpi-aint))
  (true-extent (:pointer mpi-aint)))


(defcfun ("MPI_Type_get_true_extent_x" %mpi-type-get-true-extent-x)
    mpi-error-code
  (datatype mpi-datatype)
  (true-lb (:pointer mpi-count))
  (true-extent (:pointer mpi-count)))


(defcfun ("MPI_Type_indexed" %mpi-type-indexed)
    mpi-error-code
  (count :int)
  (array-of-blocklengths (:pointer :int))
  (array-of-displacements (:pointer :int))
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Type_match_size" %mpi-type-match-size)
    mpi-error-code
  (typeclass :int)
  (size :int)
  (type (:pointer mpi-datatype)))


(defcfun ("MPI_Type_set_attr" %mpi-type-set-attr)
    mpi-error-code
  (type mpi-datatype)
  (type-keyval :int)
  (attr-val :pointer))


(defcfun ("MPI_Type_set_name" %mpi-type-set-name)
    mpi-error-code
  (type mpi-datatype)
  (type-name (:pointer :char)))


(defcfun ("MPI_Type_size" %mpi-type-size)
    mpi-error-code
  (type mpi-datatype)
  (size (:pointer :int)))


(defcfun ("MPI_Type_size_x" %mpi-type-size-x)
    mpi-error-code
  (type mpi-datatype)
  (size (:pointer mpi-count)))


(defcfun ("MPI_Type_vector" %mpi-type-vector)
    mpi-error-code
  (count :int)
  (blocklength :int)
  (stride :int)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("MPI_Unpack" %mpi-unpack)
    mpi-error-code
  (inbuf :pointer)
  (insize :int)
  (position (:pointer :int))
  (outbuf :pointer)
  (outcount :int)
  (datatype mpi-datatype)
  (comm mpi-comm))


(defcfun ("MPI_Unpublish_name" %mpi-unpublish-name)
    mpi-error-code
  (service-name (:pointer :char))
  (info mpi-info)
  (port-name (:pointer :char)))


(defcfun ("MPI_Unpack_external" %mpi-unpack-external)
    mpi-error-code
  (datarep (:pointer :char))
  (inbuf :pointer)
  (insize mpi-aint)
  (position (:pointer mpi-aint))
  (outbuf :pointer)
  (outcount :int)
  (datatype mpi-datatype))


(defcfun ("MPI_Waitall" %mpi-waitall)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request))
  (array-of-statuses (:pointer (:struct mpi-status))))


(defcfun ("MPI_Waitany" %mpi-waitany)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request))
  (index (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Wait" %mpi-wait)
    mpi-error-code
  (request (:pointer mpi-request))
  (status (:pointer (:struct mpi-status))))


(defcfun ("MPI_Waitsome" %mpi-waitsome)
    mpi-error-code
  (incount :int)
  (array-of-requests (:pointer mpi-request))
  (outcount (:pointer :int))
  (array-of-indices (:pointer :int))
  (array-of-statuses (:pointer (:struct mpi-status))))


(defcfun ("MPI_Win_allocate" %mpi-win-allocate)
    mpi-error-code
  (size mpi-aint)
  (disp-unit :int)
  (info mpi-info)
  (comm mpi-comm)
  (baseptr :pointer)
  (win (:pointer mpi-win)))


(defcfun ("MPI_Win_allocate_shared" %mpi-win-allocate-shared)
    mpi-error-code
  (size mpi-aint)
  (disp-unit :int)
  (info mpi-info)
  (comm mpi-comm)
  (baseptr :pointer)
  (win (:pointer mpi-win)))


(defcfun ("MPI_Win_attach" %mpi-win-attach)
    mpi-error-code
  (win mpi-win)
  (base :pointer)
  (size mpi-aint))


(defcfun ("MPI_Win_call_errhandler" %mpi-win-call-errhandler)
    mpi-error-code
  (win mpi-win)
  (errorcode :int))


(defcfun ("MPI_Win_complete" %mpi-win-complete)
    mpi-error-code
  (win mpi-win))


(defcfun ("MPI_Win_create" %mpi-win-create)
    mpi-error-code
  (base :pointer)
  (size mpi-aint)
  (disp-unit :int)
  (info mpi-info)
  (comm mpi-comm)
  (win (:pointer mpi-win)))


(defcfun ("MPI_Win_create_dynamic" %mpi-win-create-dynamic)
    mpi-error-code
  (info mpi-info)
  (comm mpi-comm)
  (win (:pointer mpi-win)))


(defcfun ("MPI_Win_create_errhandler" %mpi-win-create-errhandler)
    mpi-error-code
  #'(:pointer mpi-win-errhandler-function)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("MPI_Win_create_keyval" %mpi-win-create-keyval)
    mpi-error-code
  (win-copy-attr-fn (:pointer mpi-win-copy-attr-function))
  (win-delete-attr-fn (:pointer mpi-win-delete-attr-function))
  (win-keyval (:pointer :int))
  (extra-state :pointer))


(defcfun ("MPI_Win_delete_attr" %mpi-win-delete-attr)
    mpi-error-code
  (win mpi-win)
  (win-keyval :int))


(defcfun ("MPI_Win_detach" %mpi-win-detach)
    mpi-error-code
  (win mpi-win)
  (base :pointer))


(defcfun ("MPI_Win_fence" %mpi-win-fence)
    mpi-error-code
  (assert :int)
  (win mpi-win))


(defcfun ("MPI_Win_flush" %mpi-win-flush)
    mpi-error-code
  (rank :int)
  (win mpi-win))


(defcfun ("MPI_Win_flush_all" %mpi-win-flush-all)
    mpi-error-code
  (win mpi-win))


(defcfun ("MPI_Win_flush_local" %mpi-win-flush-local)
    mpi-error-code
  (rank :int)
  (win mpi-win))


(defcfun ("MPI_Win_flush_local_all" %mpi-win-flush-local-all)
    mpi-error-code
  (win mpi-win))


(defcfun ("MPI_Win_free" %mpi-win-free)
    mpi-error-code
  (win (:pointer mpi-win)))


(defcfun ("MPI_Win_free_keyval" %mpi-win-free-keyval)
    mpi-error-code
  (win-keyval (:pointer :int)))


(defcfun ("MPI_Win_get_attr" %mpi-win-get-attr)
    mpi-error-code
  (win mpi-win)
  (win-keyval :int)
  (attribute-val :pointer)
  (flag (:pointer :int)))


(defcfun ("MPI_Win_get_errhandler" %mpi-win-get-errhandler)
    mpi-error-code
  (win mpi-win)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("MPI_Win_get_group" %mpi-win-get-group)
    mpi-error-code
  (win mpi-win)
  (group (:pointer mpi-group)))


(defcfun ("MPI_Win_get_info" %mpi-win-get-info)
    mpi-error-code
  (win mpi-win)
  (info-used (:pointer mpi-info)))


(defcfun ("MPI_Win_get_name" %mpi-win-get-name)
    mpi-error-code
  (win mpi-win)
  (win-name (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("MPI_Win_lock" %mpi-win-lock)
    mpi-error-code
  (lock-type :int)
  (rank :int)
  (assert :int)
  (win mpi-win))


(defcfun ("MPI_Win_lock_all" %mpi-win-lock-all)
    mpi-error-code
  (assert :int)
  (win mpi-win))


(defcfun ("MPI_Win_post" %mpi-win-post)
    mpi-error-code
  (group mpi-group)
  (assert :int)
  (win mpi-win))


(defcfun ("MPI_Win_set_attr" %mpi-win-set-attr)
    mpi-error-code
  (win mpi-win)
  (win-keyval :int)
  (attribute-val :pointer))


(defcfun ("MPI_Win_set_errhandler" %mpi-win-set-errhandler)
    mpi-error-code
  (win mpi-win)
  (errhandler mpi-errhandler))


(defcfun ("MPI_Win_set_info" %mpi-win-set-info)
    mpi-error-code
  (win mpi-win)
  (info mpi-info))


(defcfun ("MPI_Win_set_name" %mpi-win-set-name)
    mpi-error-code
  (win mpi-win)
  (win-name (:pointer :char)))


(defcfun ("MPI_Win_shared_query" %mpi-win-shared-query)
    mpi-error-code
  (win mpi-win)
  (rank :int)
  (size (:pointer mpi-aint))
  (disp-unit (:pointer :int))
  (baseptr :pointer))


(defcfun ("MPI_Win_start" %mpi-win-start)
    mpi-error-code
  (group mpi-group)
  (assert :int)
  (win mpi-win))


(defcfun ("MPI_Win_sync" %mpi-win-sync)
    mpi-error-code
  (win mpi-win))


(defcfun ("MPI_Win_test" %mpi-win-test)
    mpi-error-code
  (win mpi-win)
  (flag (:pointer :int)))


(defcfun ("MPI_Win_unlock" %mpi-win-unlock)
    mpi-error-code
  (rank :int)
  (win mpi-win))


(defcfun ("MPI_Win_unlock_all" %mpi-win-unlock-all)
    mpi-error-code
  (win mpi-win))


(defcfun ("MPI_Win_wait" %mpi-win-wait)
    mpi-error-code
  (win mpi-win))


(defcfun ("MPI_Wtick" %mpi-wtick)
    :double)


(defcfun ("MPI_Wtime" %mpi-wtime)
    :double)


(defcfun ("PMPI_Abort" %pmpi-abort)
    mpi-error-code
  (comm mpi-comm)
  (errorcode :int))


(defcfun ("PMPI_Accumulate" %pmpi-accumulate)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (op mpi-op)
  (win mpi-win))


(defcfun ("PMPI_Add_error_class" %pmpi-add-error-class)
    mpi-error-code
  (errorclass (:pointer :int)))


(defcfun ("PMPI_Add_error_code" %pmpi-add-error-code)
    mpi-error-code
  (errorclass :int)
  (errorcode (:pointer :int)))


(defcfun ("PMPI_Add_error_string" %pmpi-add-error-string)
    mpi-error-code
  (errorcode :int)
  (string (:pointer :char)))


(defcfun ("PMPI_Allgather" %pmpi-allgather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("PMPI_Iallgather" %pmpi-iallgather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Allgatherv" %pmpi-allgatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("PMPI_Iallgatherv" %pmpi-iallgatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Alloc_mem" %pmpi-alloc-mem)
    mpi-error-code
  (size mpi-aint)
  (info mpi-info)
  (baseptr :pointer))


(defcfun ("PMPI_Allreduce" %pmpi-allreduce)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("PMPI_Iallreduce" %pmpi-iallreduce)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Alltoall" %pmpi-alltoall)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("PMPI_Ialltoall" %pmpi-ialltoall)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Alltoallv" %pmpi-alltoallv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("PMPI_Ialltoallv" %pmpi-ialltoallv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Alltoallw" %pmpi-alltoallw)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtypes (:pointer mpi-datatype))
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtypes (:pointer mpi-datatype))
  (comm mpi-comm))


(defcfun ("PMPI_Ialltoallw" %pmpi-ialltoallw)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtypes (:pointer mpi-datatype))
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtypes (:pointer mpi-datatype))
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Dist_graph_create" %pmpi-dist-graph-create)
    mpi-error-code
  (comm-old mpi-comm)
  (n :int)
  (nodes (:pointer :int))
  (degrees (:pointer :int))
  (targets (:pointer :int))
  (weights (:pointer :int))
  (info mpi-info)
  (reorder :int)
  (newcomm (:pointer mpi-comm)))


(defcfun ("PMPI_Dist_graph_create_adjacent" %pmpi-dist-graph-create-adjacent)
    mpi-error-code
  (comm-old mpi-comm)
  (indegree :int)
  (sources (:pointer :int))
  (sourceweights (:pointer :int))
  (outdegree :int)
  (destinations (:pointer :int))
  (destweights (:pointer :int))
  (info mpi-info)
  (reorder :int)
  (comm-dist-graph (:pointer mpi-comm)))


(defcfun ("PMPI_Dist_graph_neighbors" %pmpi-dist-graph-neighbors)
    mpi-error-code
  (comm mpi-comm)
  (maxindegree :int)
  (sources (:pointer :int))
  (sourceweights (:pointer :int))
  (maxoutdegree :int)
  (destinations (:pointer :int))
  (destweights (:pointer :int)))


(defcfun ("PMPI_Dist_graph_neighbors_count" %pmpi-dist-graph-neighbors-count)
    mpi-error-code
  (comm mpi-comm)
  (inneighbors (:pointer :int))
  (outneighbors (:pointer :int))
  (weighted (:pointer :int)))


(defcfun ("PMPI_Barrier" %pmpi-barrier)
    mpi-error-code
  (comm mpi-comm))


(defcfun ("PMPI_Ibarrier" %pmpi-ibarrier)
    mpi-error-code
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Bcast" %pmpi-bcast)
    mpi-error-code
  (buffer :pointer)
  (count :int)
  (datatype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("PMPI_Ibcast" %pmpi-ibcast)
    mpi-error-code
  (buffer :pointer)
  (count :int)
  (datatype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Bsend" %pmpi-bsend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm))


(defcfun ("PMPI_Bsend_init" %pmpi-bsend-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Buffer_attach" %pmpi-buffer-attach)
    mpi-error-code
  (buffer :pointer)
  (size :int))


(defcfun ("PMPI_Buffer_detach" %pmpi-buffer-detach)
    mpi-error-code
  (buffer :pointer)
  (size (:pointer :int)))


(defcfun ("PMPI_Cancel" %pmpi-cancel)
    mpi-error-code
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Cart_coords" %pmpi-cart-coords)
    mpi-error-code
  (comm mpi-comm)
  (rank :int)
  (maxdims :int)
  (coords (:pointer :int)))


(defcfun ("PMPI_Cart_create" %pmpi-cart-create)
    mpi-error-code
  (old-comm mpi-comm)
  (ndims :int)
  (dims (:pointer :int))
  (periods (:pointer :int))
  (reorder :int)
  (comm-cart (:pointer mpi-comm)))


(defcfun ("PMPI_Cart_get" %pmpi-cart-get)
    mpi-error-code
  (comm mpi-comm)
  (maxdims :int)
  (dims (:pointer :int))
  (periods (:pointer :int))
  (coords (:pointer :int)))


(defcfun ("PMPI_Cart_map" %pmpi-cart-map)
    mpi-error-code
  (comm mpi-comm)
  (ndims :int)
  (dims (:pointer :int))
  (periods (:pointer :int))
  (newrank (:pointer :int)))


(defcfun ("PMPI_Cart_rank" %pmpi-cart-rank)
    mpi-error-code
  (comm mpi-comm)
  (coords (:pointer :int))
  (rank (:pointer :int)))


(defcfun ("PMPI_Cart_shift" %pmpi-cart-shift)
    mpi-error-code
  (comm mpi-comm)
  (direction :int)
  (disp :int)
  (rank-source (:pointer :int))
  (rank-dest (:pointer :int)))


(defcfun ("PMPI_Cart_sub" %pmpi-cart-sub)
    mpi-error-code
  (comm mpi-comm)
  (remain-dims (:pointer :int))
  (new-comm (:pointer mpi-comm)))


(defcfun ("PMPI_Cartdim_get" %pmpi-cartdim-get)
    mpi-error-code
  (comm mpi-comm)
  (ndims (:pointer :int)))


(defcfun ("PMPI_Close_port" %pmpi-close-port)
    mpi-error-code
  (port-name (:pointer :char)))


(defcfun ("PMPI_Comm_accept" %pmpi-comm-accept)
    mpi-error-code
  (port-name (:pointer :char))
  (info mpi-info)
  (root :int)
  (comm mpi-comm)
  (newcomm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_call_errhandler" %pmpi-comm-call-errhandler)
    mpi-error-code
  (comm mpi-comm)
  (errorcode :int))


(defcfun ("PMPI_Comm_compare" %pmpi-comm-compare)
    mpi-error-code
  (comm1 mpi-comm)
  (comm2 mpi-comm)
  (result (:pointer :int)))


(defcfun ("PMPI_Comm_connect" %pmpi-comm-connect)
    mpi-error-code
  (port-name (:pointer :char))
  (info mpi-info)
  (root :int)
  (comm mpi-comm)
  (newcomm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_create_errhandler" %pmpi-comm-create-errhandler)
    mpi-error-code
  #'(:pointer mpi-comm-errhandler-function)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("PMPI_Comm_create_keyval" %pmpi-comm-create-keyval)
    mpi-error-code
  (comm-copy-attr-fn (:pointer mpi-comm-copy-attr-function))
  (comm-delete-attr-fn (:pointer mpi-comm-delete-attr-function))
  (comm-keyval (:pointer :int))
  (extra-state :pointer))


(defcfun ("PMPI_Comm_create_group" %pmpi-comm-create-group)
    mpi-error-code
  (comm mpi-comm)
  (group mpi-group)
  (tag :int)
  (newcomm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_create" %pmpi-comm-create)
    mpi-error-code
  (comm mpi-comm)
  (group mpi-group)
  (newcomm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_delete_attr" %pmpi-comm-delete-attr)
    mpi-error-code
  (comm mpi-comm)
  (comm-keyval :int))


(defcfun ("PMPI_Comm_disconnect" %pmpi-comm-disconnect)
    mpi-error-code
  (comm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_dup" %pmpi-comm-dup)
    mpi-error-code
  (comm mpi-comm)
  (newcomm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_idup" %pmpi-comm-idup)
    mpi-error-code
  (comm mpi-comm)
  (newcomm (:pointer mpi-comm))
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Comm_dup_with_info" %pmpi-comm-dup-with-info)
    mpi-error-code
  (comm mpi-comm)
  (info mpi-info)
  (newcomm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_free_keyval" %pmpi-comm-free-keyval)
    mpi-error-code
  (comm-keyval (:pointer :int)))


(defcfun ("PMPI_Comm_free" %pmpi-comm-free)
    mpi-error-code
  (comm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_get_attr" %pmpi-comm-get-attr)
    mpi-error-code
  (comm mpi-comm)
  (comm-keyval :int)
  (attribute-val :pointer)
  (flag (:pointer :int)))


(defcfun ("PMPI_Comm_get_errhandler" %pmpi-comm-get-errhandler)
    mpi-error-code
  (comm mpi-comm)
  (erhandler (:pointer mpi-errhandler)))


(defcfun ("PMPI_Comm_get_info" %pmpi-comm-get-info)
    mpi-error-code
  (comm mpi-comm)
  (info-used (:pointer mpi-info)))


(defcfun ("PMPI_Comm_get_name" %pmpi-comm-get-name)
    mpi-error-code
  (comm mpi-comm)
  (comm-name (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("PMPI_Comm_get_parent" %pmpi-comm-get-parent)
    mpi-error-code
  (parent (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_group" %pmpi-comm-group)
    mpi-error-code
  (comm mpi-comm)
  (group (:pointer mpi-group)))


(defcfun ("PMPI_Comm_join" %pmpi-comm-join)
    mpi-error-code
  (fd :int)
  (intercomm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_rank" %pmpi-comm-rank)
    mpi-error-code
  (comm mpi-comm)
  (rank (:pointer :int)))


(defcfun ("PMPI_Comm_remote_group" %pmpi-comm-remote-group)
    mpi-error-code
  (comm mpi-comm)
  (group (:pointer mpi-group)))


(defcfun ("PMPI_Comm_remote_size" %pmpi-comm-remote-size)
    mpi-error-code
  (comm mpi-comm)
  (size (:pointer :int)))


(defcfun ("PMPI_Comm_set_attr" %pmpi-comm-set-attr)
    mpi-error-code
  (comm mpi-comm)
  (comm-keyval :int)
  (attribute-val :pointer))


(defcfun ("PMPI_Comm_set_errhandler" %pmpi-comm-set-errhandler)
    mpi-error-code
  (comm mpi-comm)
  (errhandler mpi-errhandler))


(defcfun ("PMPI_Comm_set_info" %pmpi-comm-set-info)
    mpi-error-code
  (comm mpi-comm)
  (info mpi-info))


(defcfun ("PMPI_Comm_set_name" %pmpi-comm-set-name)
    mpi-error-code
  (comm mpi-comm)
  (comm-name (:pointer :char)))


(defcfun ("PMPI_Comm_size" %pmpi-comm-size)
    mpi-error-code
  (comm mpi-comm)
  (size (:pointer :int)))


(defcfun ("PMPI_Comm_spawn" %pmpi-comm-spawn)
    mpi-error-code
  (command (:pointer :char))
  (argv :pointer)
  (maxprocs :int)
  (info mpi-info)
  (root :int)
  (comm mpi-comm)
  (intercomm (:pointer mpi-comm))
  (array-of-errcodes (:pointer :int)))


(defcfun ("PMPI_Comm_spawn_multiple" %pmpi-comm-spawn-multiple)
    mpi-error-code
  (count :int)
  (array-of-commands :pointer)
  (array-of-argv :pointer)
  (array-of-maxprocs (:pointer :int))
  (array-of-info (:pointer mpi-info))
  (root :int)
  (comm mpi-comm)
  (intercomm (:pointer mpi-comm))
  (array-of-errcodes (:pointer :int)))


(defcfun ("PMPI_Comm_split" %pmpi-comm-split)
    mpi-error-code
  (comm mpi-comm)
  (color :int)
  (key :int)
  (newcomm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_split_type" %pmpi-comm-split-type)
    mpi-error-code
  (comm mpi-comm)
  (split-type :int)
  (key :int)
  (info mpi-info)
  (newcomm (:pointer mpi-comm)))


(defcfun ("PMPI_Comm_test_inter" %pmpi-comm-test-inter)
    mpi-error-code
  (comm mpi-comm)
  (flag (:pointer :int)))


(defcfun ("PMPI_Compare_and_swap" %pmpi-compare-and-swap)
    mpi-error-code
  (origin-addr :pointer)
  (compare-addr :pointer)
  (result-addr :pointer)
  (datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (win mpi-win))


(defcfun ("PMPI_Dims_create" %pmpi-dims-create)
    mpi-error-code
  (nnodes :int)
  (ndims :int)
  (dims (:pointer :int)))


(defcfun ("PMPI_Errhandler_free" %pmpi-errhandler-free)
    mpi-error-code
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("PMPI_Error_class" %pmpi-error-class)
    mpi-error-code
  (errorcode :int)
  (errorclass (:pointer :int)))


(defcfun ("PMPI_Error_string" %pmpi-error-string)
    mpi-error-code
  (errorcode :int)
  (string (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("PMPI_Exscan" %pmpi-exscan)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("PMPI_Fetch_and_op" %pmpi-fetch-and-op)
    mpi-error-code
  (origin-addr :pointer)
  (result-addr :pointer)
  (datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (op mpi-op)
  (win mpi-win))


(defcfun ("PMPI_Iexscan" %pmpi-iexscan)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_call_errhandler" %pmpi-file-call-errhandler)
    mpi-error-code
  (fh mpi-file)
  (errorcode :int))


(defcfun ("PMPI_File_create_errhandler" %pmpi-file-create-errhandler)
    mpi-error-code
  #'(:pointer mpi-file-errhandler-function)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("PMPI_File_set_errhandler" %pmpi-file-set-errhandler)
    mpi-error-code
  (file mpi-file)
  (errhandler mpi-errhandler))


(defcfun ("PMPI_File_get_errhandler" %pmpi-file-get-errhandler)
    mpi-error-code
  (file mpi-file)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("PMPI_File_open" %pmpi-file-open)
    mpi-error-code
  (comm mpi-comm)
  (filename (:pointer :char))
  (amode :int)
  (info mpi-info)
  (fh (:pointer mpi-file)))


(defcfun ("PMPI_File_close" %pmpi-file-close)
    mpi-error-code
  (fh (:pointer mpi-file)))


(defcfun ("PMPI_File_delete" %pmpi-file-delete)
    mpi-error-code
  (filename (:pointer :char))
  (info mpi-info))


(defcfun ("PMPI_File_set_size" %pmpi-file-set-size)
    mpi-error-code
  (fh mpi-file)
  (size mpi-offset))


(defcfun ("PMPI_File_preallocate" %pmpi-file-preallocate)
    mpi-error-code
  (fh mpi-file)
  (size mpi-offset))


(defcfun ("PMPI_File_get_size" %pmpi-file-get-size)
    mpi-error-code
  (fh mpi-file)
  (size (:pointer mpi-offset)))


(defcfun ("PMPI_File_get_group" %pmpi-file-get-group)
    mpi-error-code
  (fh mpi-file)
  (group (:pointer mpi-group)))


(defcfun ("PMPI_File_get_amode" %pmpi-file-get-amode)
    mpi-error-code
  (fh mpi-file)
  (amode (:pointer :int)))


(defcfun ("PMPI_File_set_info" %pmpi-file-set-info)
    mpi-error-code
  (fh mpi-file)
  (info mpi-info))


(defcfun ("PMPI_File_get_info" %pmpi-file-get-info)
    mpi-error-code
  (fh mpi-file)
  (info-used (:pointer mpi-info)))


(defcfun ("PMPI_File_set_view" %pmpi-file-set-view)
    mpi-error-code
  (fh mpi-file)
  (disp mpi-offset)
  (etype mpi-datatype)
  (filetype mpi-datatype)
  (datarep (:pointer :char))
  (info mpi-info))


(defcfun ("PMPI_File_get_view" %pmpi-file-get-view)
    mpi-error-code
  (fh mpi-file)
  (disp (:pointer mpi-offset))
  (etype (:pointer mpi-datatype))
  (filetype (:pointer mpi-datatype))
  (datarep (:pointer :char)))


(defcfun ("PMPI_File_read_at" %pmpi-file-read-at)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_read_at_all" %pmpi-file-read-at-all)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_write_at" %pmpi-file-write-at)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_write_at_all" %pmpi-file-write-at-all)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_iread_at" %pmpi-file-iread-at)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_iwrite_at" %pmpi-file-iwrite-at)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_iread_at_all" %pmpi-file-iread-at-all)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_iwrite_at_all" %pmpi-file-iwrite-at-all)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_read" %pmpi-file-read)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_read_all" %pmpi-file-read-all)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_write" %pmpi-file-write)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_write_all" %pmpi-file-write-all)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_iread" %pmpi-file-iread)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_iwrite" %pmpi-file-iwrite)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_iread_all" %pmpi-file-iread-all)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_iwrite_all" %pmpi-file-iwrite-all)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_seek" %pmpi-file-seek)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (whence :int))


(defcfun ("PMPI_File_get_position" %pmpi-file-get-position)
    mpi-error-code
  (fh mpi-file)
  (offset (:pointer mpi-offset)))


(defcfun ("PMPI_File_get_byte_offset" %pmpi-file-get-byte-offset)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (disp (:pointer mpi-offset)))


(defcfun ("PMPI_File_read_shared" %pmpi-file-read-shared)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_write_shared" %pmpi-file-write-shared)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_iread_shared" %pmpi-file-iread-shared)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_iwrite_shared" %pmpi-file-iwrite-shared)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_File_read_ordered" %pmpi-file-read-ordered)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_write_ordered" %pmpi-file-write-ordered)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_seek_shared" %pmpi-file-seek-shared)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (whence :int))


(defcfun ("PMPI_File_get_position_shared" %pmpi-file-get-position-shared)
    mpi-error-code
  (fh mpi-file)
  (offset (:pointer mpi-offset)))


(defcfun ("PMPI_File_read_at_all_begin" %pmpi-file-read-at-all-begin)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("PMPI_File_read_at_all_end" %pmpi-file-read-at-all-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_write_at_all_begin" %pmpi-file-write-at-all-begin)
    mpi-error-code
  (fh mpi-file)
  (offset mpi-offset)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("PMPI_File_write_at_all_end" %pmpi-file-write-at-all-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_read_all_begin" %pmpi-file-read-all-begin)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("PMPI_File_read_all_end" %pmpi-file-read-all-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_write_all_begin" %pmpi-file-write-all-begin)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("PMPI_File_write_all_end" %pmpi-file-write-all-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_read_ordered_begin" %pmpi-file-read-ordered-begin)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("PMPI_File_read_ordered_end" %pmpi-file-read-ordered-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_write_ordered_begin" %pmpi-file-write-ordered-begin)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype))


(defcfun ("PMPI_File_write_ordered_end" %pmpi-file-write-ordered-end)
    mpi-error-code
  (fh mpi-file)
  (buf :pointer)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_File_get_type_extent" %pmpi-file-get-type-extent)
    mpi-error-code
  (fh mpi-file)
  (datatype mpi-datatype)
  (extent (:pointer mpi-aint)))


(defcfun ("PMPI_File_set_atomicity" %pmpi-file-set-atomicity)
    mpi-error-code
  (fh mpi-file)
  (flag :int))


(defcfun ("PMPI_File_get_atomicity" %pmpi-file-get-atomicity)
    mpi-error-code
  (fh mpi-file)
  (flag (:pointer :int)))


(defcfun ("PMPI_File_sync" %pmpi-file-sync)
    mpi-error-code
  (fh mpi-file))


(defcfun ("PMPI_Finalize" %pmpi-finalize)
    mpi-error-code)


(defcfun ("PMPI_Finalized" %pmpi-finalized)
    mpi-error-code
  (flag (:pointer :int)))


(defcfun ("PMPI_Free_mem" %pmpi-free-mem)
    mpi-error-code
  (base :pointer))


(defcfun ("PMPI_Gather" %pmpi-gather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("PMPI_Igather" %pmpi-igather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Gatherv" %pmpi-gatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("PMPI_Igatherv" %pmpi-igatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Get_address" %pmpi-get-address)
    mpi-error-code
  (location :pointer)
  (address (:pointer mpi-aint)))


(defcfun ("PMPI_Get_count" %pmpi-get-count)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count (:pointer :int)))


(defcfun ("PMPI_Get_elements" %pmpi-get-elements)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count (:pointer :int)))


(defcfun ("PMPI_Get_elements_x" %pmpi-get-elements-x)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count (:pointer mpi-count)))


(defcfun ("PMPI_Get" %pmpi-get)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (win mpi-win))


(defcfun ("PMPI_Get_accumulate" %pmpi-get-accumulate)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (result-addr :pointer)
  (result-count :int)
  (result-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (op mpi-op)
  (win mpi-win))


(defcfun ("PMPI_Get_library_version" %pmpi-get-library-version)
    mpi-error-code
  (version (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("PMPI_Get_processor_name" %pmpi-get-processor-name)
    mpi-error-code
  (name (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("PMPI_Get_version" %pmpi-get-version)
    mpi-error-code
  (version (:pointer :int))
  (subversion (:pointer :int)))


(defcfun ("PMPI_Graph_create" %pmpi-graph-create)
    mpi-error-code
  (comm-old mpi-comm)
  (nnodes :int)
  (index (:pointer :int))
  (edges (:pointer :int))
  (reorder :int)
  (comm-graph (:pointer mpi-comm)))


(defcfun ("PMPI_Graph_get" %pmpi-graph-get)
    mpi-error-code
  (comm mpi-comm)
  (maxindex :int)
  (maxedges :int)
  (index (:pointer :int))
  (edges (:pointer :int)))


(defcfun ("PMPI_Graph_map" %pmpi-graph-map)
    mpi-error-code
  (comm mpi-comm)
  (nnodes :int)
  (index (:pointer :int))
  (edges (:pointer :int))
  (newrank (:pointer :int)))


(defcfun ("PMPI_Graph_neighbors_count" %pmpi-graph-neighbors-count)
    mpi-error-code
  (comm mpi-comm)
  (rank :int)
  (nneighbors (:pointer :int)))


(defcfun ("PMPI_Graph_neighbors" %pmpi-graph-neighbors)
    mpi-error-code
  (comm mpi-comm)
  (rank :int)
  (maxneighbors :int)
  (neighbors (:pointer :int)))


(defcfun ("PMPI_Graphdims_get" %pmpi-graphdims-get)
    mpi-error-code
  (comm mpi-comm)
  (nnodes (:pointer :int))
  (nedges (:pointer :int)))


(defcfun ("PMPI_Grequest_complete" %pmpi-grequest-complete)
    mpi-error-code
  (request mpi-request))


(defcfun ("PMPI_Grequest_start" %pmpi-grequest-start)
    mpi-error-code
  (query-fn (:pointer mpi-grequest-query-function))
  (free-fn (:pointer mpi-grequest-free-function))
  (cancel-fn (:pointer mpi-grequest-cancel-function))
  (extra-state :pointer)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Group_compare" %pmpi-group-compare)
    mpi-error-code
  (group1 mpi-group)
  (group2 mpi-group)
  (result (:pointer :int)))


(defcfun ("PMPI_Group_difference" %pmpi-group-difference)
    mpi-error-code
  (group1 mpi-group)
  (group2 mpi-group)
  (newgroup (:pointer mpi-group)))


(defcfun ("PMPI_Group_excl" %pmpi-group-excl)
    mpi-error-code
  (group mpi-group)
  (n :int)
  (ranks (:pointer :int))
  (newgroup (:pointer mpi-group)))


(defcfun ("PMPI_Group_free" %pmpi-group-free)
    mpi-error-code
  (group (:pointer mpi-group)))


(defcfun ("PMPI_Group_incl" %pmpi-group-incl)
    mpi-error-code
  (group mpi-group)
  (n :int)
  (ranks (:pointer :int))
  (newgroup (:pointer mpi-group)))


(defcfun ("PMPI_Group_intersection" %pmpi-group-intersection)
    mpi-error-code
  (group1 mpi-group)
  (group2 mpi-group)
  (newgroup (:pointer mpi-group)))


(defcfun ("PMPI_Group_range_excl" %pmpi-group-range-excl)
    mpi-error-code
  (group mpi-group)
  (n :int)
  (ranges :pointer)
  (newgroup (:pointer mpi-group)))


(defcfun ("PMPI_Group_range_incl" %pmpi-group-range-incl)
    mpi-error-code
  (group mpi-group)
  (n :int)
  (ranges :pointer)
  (newgroup (:pointer mpi-group)))


(defcfun ("PMPI_Group_rank" %pmpi-group-rank)
    mpi-error-code
  (group mpi-group)
  (rank (:pointer :int)))


(defcfun ("PMPI_Group_size" %pmpi-group-size)
    mpi-error-code
  (group mpi-group)
  (size (:pointer :int)))


(defcfun ("PMPI_Group_translate_ranks" %pmpi-group-translate-ranks)
    mpi-error-code
  (group1 mpi-group)
  (n :int)
  (ranks1 (:pointer :int))
  (group2 mpi-group)
  (ranks2 (:pointer :int)))


(defcfun ("PMPI_Group_union" %pmpi-group-union)
    mpi-error-code
  (group1 mpi-group)
  (group2 mpi-group)
  (newgroup (:pointer mpi-group)))


(defcfun ("PMPI_Ibsend" %pmpi-ibsend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Improbe" %pmpi-improbe)
    mpi-error-code
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (flag (:pointer :int))
  (message (:pointer mpi-message))
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Imrecv" %pmpi-imrecv)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (type mpi-datatype)
  (message (:pointer mpi-message))
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Info_create" %pmpi-info-create)
    mpi-error-code
  (info (:pointer mpi-info)))


(defcfun ("PMPI_Info_delete" %pmpi-info-delete)
    mpi-error-code
  (info mpi-info)
  (key (:pointer :char)))


(defcfun ("PMPI_Info_dup" %pmpi-info-dup)
    mpi-error-code
  (info mpi-info)
  (newinfo (:pointer mpi-info)))


(defcfun ("PMPI_Info_free" %pmpi-info-free)
    mpi-error-code
  (info (:pointer mpi-info)))


(defcfun ("PMPI_Info_get" %pmpi-info-get)
    mpi-error-code
  (info mpi-info)
  (key (:pointer :char))
  (valuelen :int)
  (value (:pointer :char))
  (flag (:pointer :int)))


(defcfun ("PMPI_Info_get_nkeys" %pmpi-info-get-nkeys)
    mpi-error-code
  (info mpi-info)
  (nkeys (:pointer :int)))


(defcfun ("PMPI_Info_get_nthkey" %pmpi-info-get-nthkey)
    mpi-error-code
  (info mpi-info)
  (n :int)
  (key (:pointer :char)))


(defcfun ("PMPI_Info_get_valuelen" %pmpi-info-get-valuelen)
    mpi-error-code
  (info mpi-info)
  (key (:pointer :char))
  (valuelen (:pointer :int))
  (flag (:pointer :int)))


(defcfun ("PMPI_Info_set" %pmpi-info-set)
    mpi-error-code
  (info mpi-info)
  (key (:pointer :char))
  (value (:pointer :char)))


(defcfun ("PMPI_Init" %pmpi-init)
    mpi-error-code
  (argc (:pointer :int))
  (argv :pointer))


(defcfun ("PMPI_Initialized" %pmpi-initialized)
    mpi-error-code
  (flag (:pointer :int)))


(defcfun ("PMPI_Init_thread" %pmpi-init-thread)
    mpi-error-code
  (argc (:pointer :int))
  (argv :pointer)
  (required :int)
  (provided (:pointer :int)))


(defcfun ("PMPI_Intercomm_create" %pmpi-intercomm-create)
    mpi-error-code
  (local-comm mpi-comm)
  (local-leader :int)
  (bridge-comm mpi-comm)
  (remote-leader :int)
  (tag :int)
  (newintercomm (:pointer mpi-comm)))


(defcfun ("PMPI_Intercomm_merge" %pmpi-intercomm-merge)
    mpi-error-code
  (intercomm mpi-comm)
  (high :int)
  (newintercomm (:pointer mpi-comm)))


(defcfun ("PMPI_Iprobe" %pmpi-iprobe)
    mpi-error-code
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (flag (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Irecv" %pmpi-irecv)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Irsend" %pmpi-irsend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Isend" %pmpi-isend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Issend" %pmpi-issend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Is_thread_main" %pmpi-is-thread-main)
    mpi-error-code
  (flag (:pointer :int)))


(defcfun ("PMPI_Lookup_name" %pmpi-lookup-name)
    mpi-error-code
  (service-name (:pointer :char))
  (info mpi-info)
  (port-name (:pointer :char)))


(defcfun ("PMPI_Mprobe" %pmpi-mprobe)
    mpi-error-code
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (message (:pointer mpi-message))
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Mrecv" %pmpi-mrecv)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (type mpi-datatype)
  (message (:pointer mpi-message))
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Neighbor_allgather" %pmpi-neighbor-allgather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("PMPI_Ineighbor_allgather" %pmpi-ineighbor-allgather)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Neighbor_allgatherv" %pmpi-neighbor-allgatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("PMPI_Ineighbor_allgatherv" %pmpi-ineighbor-allgatherv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (displs (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Neighbor_alltoall" %pmpi-neighbor-alltoall)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("PMPI_Ineighbor_alltoall" %pmpi-ineighbor-alltoall)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Neighbor_alltoallv" %pmpi-neighbor-alltoallv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm))


(defcfun ("PMPI_Ineighbor_alltoallv" %pmpi-ineighbor-alltoallv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer :int))
  (recvtype mpi-datatype)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Neighbor_alltoallw" %pmpi-neighbor-alltoallw)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer mpi-aint))
  (sendtypes (:pointer mpi-datatype))
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer mpi-aint))
  (recvtypes (:pointer mpi-datatype))
  (comm mpi-comm))


(defcfun ("PMPI_Ineighbor_alltoallw" %pmpi-ineighbor-alltoallw)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (sdispls (:pointer mpi-aint))
  (sendtypes (:pointer mpi-datatype))
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (rdispls (:pointer mpi-aint))
  (recvtypes (:pointer mpi-datatype))
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Op_commutative" %pmpi-op-commutative)
    mpi-error-code
  (op mpi-op)
  (commute (:pointer :int)))


(defcfun ("PMPI_Op_create" %pmpi-op-create)
    mpi-error-code
  #'(:pointer mpi-user-function)
  (commute :int)
  (op (:pointer mpi-op)))


(defcfun ("PMPI_Open_port" %pmpi-open-port)
    mpi-error-code
  (info mpi-info)
  (port-name (:pointer :char)))


(defcfun ("PMPI_Op_free" %pmpi-op-free)
    mpi-error-code
  (op (:pointer mpi-op)))


(defcfun ("PMPI_Pack_external" %pmpi-pack-external)
    mpi-error-code
  (datarep (:pointer :char))
  (inbuf :pointer)
  (incount :int)
  (datatype mpi-datatype)
  (outbuf :pointer)
  (outsize mpi-aint)
  (position (:pointer mpi-aint)))


(defcfun ("PMPI_Pack_external_size" %pmpi-pack-external-size)
    mpi-error-code
  (datarep (:pointer :char))
  (incount :int)
  (datatype mpi-datatype)
  (size (:pointer mpi-aint)))


(defcfun ("PMPI_Pack" %pmpi-pack)
    mpi-error-code
  (inbuf :pointer)
  (incount :int)
  (datatype mpi-datatype)
  (outbuf :pointer)
  (outsize :int)
  (position (:pointer :int))
  (comm mpi-comm))


(defcfun ("PMPI_Pack_size" %pmpi-pack-size)
    mpi-error-code
  (incount :int)
  (datatype mpi-datatype)
  (comm mpi-comm)
  (size (:pointer :int)))


(defcfun ("PMPI_Pcontrol" %pmpi-pcontrol)
    mpi-error-code
  (level :int))


(defcfun ("PMPI_Probe" %pmpi-probe)
    mpi-error-code
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Publish_name" %pmpi-publish-name)
    mpi-error-code
  (service-name (:pointer :char))
  (info mpi-info)
  (port-name (:pointer :char)))


(defcfun ("PMPI_Put" %pmpi-put)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (win mpi-win))


(defcfun ("PMPI_Query_thread" %pmpi-query-thread)
    mpi-error-code
  (provided (:pointer :int)))


(defcfun ("PMPI_Raccumulate" %pmpi-raccumulate)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (op mpi-op)
  (win mpi-win)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Recv_init" %pmpi-recv-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Recv" %pmpi-recv)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (source :int)
  (tag :int)
  (comm mpi-comm)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Reduce" %pmpi-reduce)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (root :int)
  (comm mpi-comm))


(defcfun ("PMPI_Ireduce" %pmpi-ireduce)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Reduce_local" %pmpi-reduce-local)
    mpi-error-code
  (inbuf :pointer)
  (inoutbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op))


(defcfun ("PMPI_Reduce_scatter" %pmpi-reduce-scatter)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("PMPI_Ireduce_scatter" %pmpi-ireduce-scatter)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (recvcounts (:pointer :int))
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Reduce_scatter_block" %pmpi-reduce-scatter-block)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (recvcount :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("PMPI_Ireduce_scatter_block" %pmpi-ireduce-scatter-block)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (recvcount :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Register_datarep" %pmpi-register-datarep)
    mpi-error-code
  (datarep (:pointer :char))
  (read-conversion-fn (:pointer mpi-datarep-conversion-function))
  (write-conversion-fn (:pointer mpi-datarep-conversion-function))
  (dtype-file-extent-fn (:pointer mpi-datarep-extent-function))
  (extra-state :pointer))


(defcfun ("PMPI_Request_free" %pmpi-request-free)
    mpi-error-code
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Request_get_status" %pmpi-request-get-status)
    mpi-error-code
  (request mpi-request)
  (flag (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Rget" %pmpi-rget)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (win mpi-win)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Rget_accumulate" %pmpi-rget-accumulate)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (result-addr :pointer)
  (result-count :int)
  (result-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-count :int)
  (target-datatype mpi-datatype)
  (op mpi-op)
  (win mpi-win)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Rput" %pmpi-rput)
    mpi-error-code
  (origin-addr :pointer)
  (origin-count :int)
  (origin-datatype mpi-datatype)
  (target-rank :int)
  (target-disp mpi-aint)
  (target-cout :int)
  (target-datatype mpi-datatype)
  (win mpi-win)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Rsend" %pmpi-rsend)
    mpi-error-code
  (ibuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm))


(defcfun ("PMPI_Rsend_init" %pmpi-rsend-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Scan" %pmpi-scan)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm))


(defcfun ("PMPI_Iscan" %pmpi-iscan)
    mpi-error-code
  (sendbuf :pointer)
  (recvbuf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (op mpi-op)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Scatter" %pmpi-scatter)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("PMPI_Iscatter" %pmpi-iscatter)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Scatterv" %pmpi-scatterv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (displs (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm))


(defcfun ("PMPI_Iscatterv" %pmpi-iscatterv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcounts (:pointer :int))
  (displs (:pointer :int))
  (sendtype mpi-datatype)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (root :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Send_init" %pmpi-send-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Send" %pmpi-send)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm))


(defcfun ("PMPI_Sendrecv" %pmpi-sendrecv)
    mpi-error-code
  (sendbuf :pointer)
  (sendcount :int)
  (sendtype mpi-datatype)
  (dest :int)
  (sendtag :int)
  (recvbuf :pointer)
  (recvcount :int)
  (recvtype mpi-datatype)
  (source :int)
  (recvtag :int)
  (comm mpi-comm)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Sendrecv_replace" %pmpi-sendrecv-replace)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (sendtag :int)
  (source :int)
  (recvtag :int)
  (comm mpi-comm)
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Ssend_init" %pmpi-ssend-init)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm)
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Ssend" %pmpi-ssend)
    mpi-error-code
  (buf :pointer)
  (count :int)
  (datatype mpi-datatype)
  (dest :int)
  (tag :int)
  (comm mpi-comm))


(defcfun ("PMPI_Start" %pmpi-start)
    mpi-error-code
  (request (:pointer mpi-request)))


(defcfun ("PMPI_Startall" %pmpi-startall)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request)))


(defcfun ("PMPI_Status_set_cancelled" %pmpi-status-set-cancelled)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (flag :int))


(defcfun ("PMPI_Status_set_elements" %pmpi-status-set-elements)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count :int))


(defcfun ("PMPI_Status_set_elements_x" %pmpi-status-set-elements-x)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (datatype mpi-datatype)
  (count mpi-count))


(defcfun ("PMPI_Testall" %pmpi-testall)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request))
  (flag (:pointer :int))
  (array-of-statuses (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Testany" %pmpi-testany)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request))
  (index (:pointer :int))
  (flag (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Test" %pmpi-test)
    mpi-error-code
  (request (:pointer mpi-request))
  (flag (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Test_cancelled" %pmpi-test-cancelled)
    mpi-error-code
  (status (:pointer (:struct mpi-status)))
  (flag (:pointer :int)))


(defcfun ("PMPI_Testsome" %pmpi-testsome)
    mpi-error-code
  (incount :int)
  (array-of-requests (:pointer mpi-request))
  (outcount (:pointer :int))
  (array-of-indices (:pointer :int))
  (array-of-statuses (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Topo_test" %pmpi-topo-test)
    mpi-error-code
  (comm mpi-comm)
  (status (:pointer :int)))


(defcfun ("PMPI_Type_commit" %pmpi-type-commit)
    mpi-error-code
  (type (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_contiguous" %pmpi-type-contiguous)
    mpi-error-code
  (count :int)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_darray" %pmpi-type-create-darray)
    mpi-error-code
  (size :int)
  (rank :int)
  (ndims :int)
  (gsize-array (:pointer :int))
  (distrib-array (:pointer :int))
  (darg-array (:pointer :int))
  (psize-array (:pointer :int))
  (order :int)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_f90_complex" %pmpi-type-create-f90-complex)
    mpi-error-code
  (p :int)
  (r :int)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_f90_integer" %pmpi-type-create-f90-integer)
    mpi-error-code
  (r :int)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_f90_real" %pmpi-type-create-f90-real)
    mpi-error-code
  (p :int)
  (r :int)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_hindexed" %pmpi-type-create-hindexed)
    mpi-error-code
  (count :int)
  (array-of-blocklengths (:pointer :int))
  (array-of-displacements (:pointer mpi-aint))
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_hvector" %pmpi-type-create-hvector)
    mpi-error-code
  (count :int)
  (blocklength :int)
  (stride mpi-aint)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_keyval" %pmpi-type-create-keyval)
    mpi-error-code
  (type-copy-attr-fn (:pointer mpi-type-copy-attr-function))
  (type-delete-attr-fn (:pointer mpi-type-delete-attr-function))
  (type-keyval (:pointer :int))
  (extra-state :pointer))


(defcfun ("PMPI_Type_create_hindexed_block" %pmpi-type-create-hindexed-block)
    mpi-error-code
  (count :int)
  (blocklength :int)
  (array-of-displacements (:pointer mpi-aint))
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_indexed_block" %pmpi-type-create-indexed-block)
    mpi-error-code
  (count :int)
  (blocklength :int)
  (array-of-displacements (:pointer :int))
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_struct" %pmpi-type-create-struct)
    mpi-error-code
  (count :int)
  (array-of-block-lengths (:pointer :int))
  (array-of-displacements (:pointer mpi-aint))
  (array-of-types (:pointer mpi-datatype))
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_subarray" %pmpi-type-create-subarray)
    mpi-error-code
  (ndims :int)
  (size-array (:pointer :int))
  (subsize-array (:pointer :int))
  (start-array (:pointer :int))
  (order :int)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_create_resized" %pmpi-type-create-resized)
    mpi-error-code
  (oldtype mpi-datatype)
  (lb mpi-aint)
  (extent mpi-aint)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_delete_attr" %pmpi-type-delete-attr)
    mpi-error-code
  (type mpi-datatype)
  (type-keyval :int))


(defcfun ("PMPI_Type_dup" %pmpi-type-dup)
    mpi-error-code
  (type mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_free" %pmpi-type-free)
    mpi-error-code
  (type (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_free_keyval" %pmpi-type-free-keyval)
    mpi-error-code
  (type-keyval (:pointer :int)))


(defcfun ("PMPI_Type_get_attr" %pmpi-type-get-attr)
    mpi-error-code
  (type mpi-datatype)
  (type-keyval :int)
  (attribute-val :pointer)
  (flag (:pointer :int)))


(defcfun ("PMPI_Type_get_contents" %pmpi-type-get-contents)
    mpi-error-code
  (mtype mpi-datatype)
  (max-integers :int)
  (max-addresses :int)
  (max-datatypes :int)
  (array-of-integers (:pointer :int))
  (array-of-addresses (:pointer mpi-aint))
  (array-of-datatypes (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_get_envelope" %pmpi-type-get-envelope)
    mpi-error-code
  (type mpi-datatype)
  (num-integers (:pointer :int))
  (num-addresses (:pointer :int))
  (num-datatypes (:pointer :int))
  (combiner (:pointer :int)))


(defcfun ("PMPI_Type_get_extent" %pmpi-type-get-extent)
    mpi-error-code
  (type mpi-datatype)
  (lb (:pointer mpi-aint))
  (extent (:pointer mpi-aint)))


(defcfun ("PMPI_Type_get_extent_x" %pmpi-type-get-extent-x)
    mpi-error-code
  (type mpi-datatype)
  (lb (:pointer mpi-count))
  (extent (:pointer mpi-count)))


(defcfun ("PMPI_Type_get_name" %pmpi-type-get-name)
    mpi-error-code
  (type mpi-datatype)
  (type-name (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("PMPI_Type_get_true_extent" %pmpi-type-get-true-extent)
    mpi-error-code
  (datatype mpi-datatype)
  (true-lb (:pointer mpi-aint))
  (true-extent (:pointer mpi-aint)))


(defcfun ("PMPI_Type_get_true_extent_x" %pmpi-type-get-true-extent-x)
    mpi-error-code
  (datatype mpi-datatype)
  (true-lb (:pointer mpi-count))
  (true-extent (:pointer mpi-count)))


(defcfun ("PMPI_Type_indexed" %pmpi-type-indexed)
    mpi-error-code
  (count :int)
  (array-of-blocklengths (:pointer :int))
  (array-of-displacements (:pointer :int))
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_match_size" %pmpi-type-match-size)
    mpi-error-code
  (typeclass :int)
  (size :int)
  (type (:pointer mpi-datatype)))


(defcfun ("PMPI_Type_set_attr" %pmpi-type-set-attr)
    mpi-error-code
  (type mpi-datatype)
  (type-keyval :int)
  (attr-val :pointer))


(defcfun ("PMPI_Type_set_name" %pmpi-type-set-name)
    mpi-error-code
  (type mpi-datatype)
  (type-name (:pointer :char)))


(defcfun ("PMPI_Type_size" %pmpi-type-size)
    mpi-error-code
  (type mpi-datatype)
  (size (:pointer :int)))


(defcfun ("PMPI_Type_size_x" %pmpi-type-size-x)
    mpi-error-code
  (type mpi-datatype)
  (size (:pointer mpi-count)))


(defcfun ("PMPI_Type_vector" %pmpi-type-vector)
    mpi-error-code
  (count :int)
  (blocklength :int)
  (stride :int)
  (oldtype mpi-datatype)
  (newtype (:pointer mpi-datatype)))


(defcfun ("PMPI_Unpack" %pmpi-unpack)
    mpi-error-code
  (inbuf :pointer)
  (insize :int)
  (position (:pointer :int))
  (outbuf :pointer)
  (outcount :int)
  (datatype mpi-datatype)
  (comm mpi-comm))


(defcfun ("PMPI_Unpublish_name" %pmpi-unpublish-name)
    mpi-error-code
  (service-name (:pointer :char))
  (info mpi-info)
  (port-name (:pointer :char)))


(defcfun ("PMPI_Unpack_external" %pmpi-unpack-external)
    mpi-error-code
  (datarep (:pointer :char))
  (inbuf :pointer)
  (insize mpi-aint)
  (position (:pointer mpi-aint))
  (outbuf :pointer)
  (outcount :int)
  (datatype mpi-datatype))


(defcfun ("PMPI_Waitall" %pmpi-waitall)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request))
  (array-of-statuses (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Waitany" %pmpi-waitany)
    mpi-error-code
  (count :int)
  (array-of-requests (:pointer mpi-request))
  (index (:pointer :int))
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Wait" %pmpi-wait)
    mpi-error-code
  (request (:pointer mpi-request))
  (status (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Waitsome" %pmpi-waitsome)
    mpi-error-code
  (incount :int)
  (array-of-requests (:pointer mpi-request))
  (outcount (:pointer :int))
  (array-of-indices (:pointer :int))
  (array-of-statuses (:pointer (:struct mpi-status))))


(defcfun ("PMPI_Win_allocate" %pmpi-win-allocate)
    mpi-error-code
  (size mpi-aint)
  (disp-unit :int)
  (info mpi-info)
  (comm mpi-comm)
  (baseptr :pointer)
  (win (:pointer mpi-win)))


(defcfun ("PMPI_Win_allocate_shared" %pmpi-win-allocate-shared)
    mpi-error-code
  (size mpi-aint)
  (disp-unit :int)
  (info mpi-info)
  (comm mpi-comm)
  (baseptr :pointer)
  (win (:pointer mpi-win)))


(defcfun ("PMPI_Win_attach" %pmpi-win-attach)
    mpi-error-code
  (win mpi-win)
  (base :pointer)
  (size mpi-aint))


(defcfun ("PMPI_Win_call_errhandler" %pmpi-win-call-errhandler)
    mpi-error-code
  (win mpi-win)
  (errorcode :int))


(defcfun ("PMPI_Win_complete" %pmpi-win-complete)
    mpi-error-code
  (win mpi-win))


(defcfun ("PMPI_Win_create" %pmpi-win-create)
    mpi-error-code
  (base :pointer)
  (size mpi-aint)
  (disp-unit :int)
  (info mpi-info)
  (comm mpi-comm)
  (win (:pointer mpi-win)))


(defcfun ("PMPI_Win_create_dynamic" %pmpi-win-create-dynamic)
    mpi-error-code
  (info mpi-info)
  (comm mpi-comm)
  (win (:pointer mpi-win)))


(defcfun ("PMPI_Win_create_errhandler" %pmpi-win-create-errhandler)
    mpi-error-code
  #'(:pointer mpi-win-errhandler-function)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("PMPI_Win_create_keyval" %pmpi-win-create-keyval)
    mpi-error-code
  (win-copy-attr-fn (:pointer mpi-win-copy-attr-function))
  (win-delete-attr-fn (:pointer mpi-win-delete-attr-function))
  (win-keyval (:pointer :int))
  (extra-state :pointer))


(defcfun ("PMPI_Win_delete_attr" %pmpi-win-delete-attr)
    mpi-error-code
  (win mpi-win)
  (win-keyval :int))


(defcfun ("PMPI_Win_detach" %pmpi-win-detach)
    mpi-error-code
  (win mpi-win)
  (base :pointer))


(defcfun ("PMPI_Win_fence" %pmpi-win-fence)
    mpi-error-code
  (assert :int)
  (win mpi-win))


(defcfun ("PMPI_Win_flush" %pmpi-win-flush)
    mpi-error-code
  (rank :int)
  (win mpi-win))


(defcfun ("PMPI_Win_flush_all" %pmpi-win-flush-all)
    mpi-error-code
  (win mpi-win))


(defcfun ("PMPI_Win_flush_local" %pmpi-win-flush-local)
    mpi-error-code
  (rank :int)
  (win mpi-win))


(defcfun ("PMPI_Win_flush_local_all" %pmpi-win-flush-local-all)
    mpi-error-code
  (win mpi-win))


(defcfun ("PMPI_Win_free" %pmpi-win-free)
    mpi-error-code
  (win (:pointer mpi-win)))


(defcfun ("PMPI_Win_free_keyval" %pmpi-win-free-keyval)
    mpi-error-code
  (win-keyval (:pointer :int)))


(defcfun ("PMPI_Win_get_attr" %pmpi-win-get-attr)
    mpi-error-code
  (win mpi-win)
  (win-keyval :int)
  (attribute-val :pointer)
  (flag (:pointer :int)))


(defcfun ("PMPI_Win_get_errhandler" %pmpi-win-get-errhandler)
    mpi-error-code
  (win mpi-win)
  (errhandler (:pointer mpi-errhandler)))


(defcfun ("PMPI_Win_get_group" %pmpi-win-get-group)
    mpi-error-code
  (win mpi-win)
  (group (:pointer mpi-group)))


(defcfun ("PMPI_Win_get_info" %pmpi-win-get-info)
    mpi-error-code
  (win mpi-win)
  (info-used (:pointer mpi-info)))


(defcfun ("PMPI_Win_get_name" %pmpi-win-get-name)
    mpi-error-code
  (win mpi-win)
  (win-name (:pointer :char))
  (resultlen (:pointer :int)))


(defcfun ("PMPI_Win_lock" %pmpi-win-lock)
    mpi-error-code
  (lock-type :int)
  (rank :int)
  (assert :int)
  (win mpi-win))


(defcfun ("PMPI_Win_lock_all" %pmpi-win-lock-all)
    mpi-error-code
  (assert :int)
  (win mpi-win))


(defcfun ("PMPI_Win_post" %pmpi-win-post)
    mpi-error-code
  (group mpi-group)
  (assert :int)
  (win mpi-win))


(defcfun ("PMPI_Win_set_attr" %pmpi-win-set-attr)
    mpi-error-code
  (win mpi-win)
  (win-keyval :int)
  (attribute-val :pointer))


(defcfun ("PMPI_Win_set_errhandler" %pmpi-win-set-errhandler)
    mpi-error-code
  (win mpi-win)
  (errhandler mpi-errhandler))


(defcfun ("PMPI_Win_set_info" %pmpi-win-set-info)
    mpi-error-code
  (win mpi-win)
  (info mpi-info))


(defcfun ("PMPI_Win_set_name" %pmpi-win-set-name)
    mpi-error-code
  (win mpi-win)
  (win-name (:pointer :char)))


(defcfun ("PMPI_Win_shared_query" %pmpi-win-shared-query)
    mpi-error-code
  (win mpi-win)
  (rank :int)
  (size (:pointer mpi-aint))
  (disp-unit (:pointer :int))
  (baseptr :pointer))


(defcfun ("PMPI_Win_start" %pmpi-win-start)
    mpi-error-code
  (group mpi-group)
  (assert :int)
  (win mpi-win))


(defcfun ("PMPI_Win_sync" %pmpi-win-sync)
    mpi-error-code
  (win mpi-win))


(defcfun ("PMPI_Win_test" %pmpi-win-test)
    mpi-error-code
  (win mpi-win)
  (flag (:pointer :int)))


(defcfun ("PMPI_Win_unlock" %pmpi-win-unlock)
    mpi-error-code
  (rank :int)
  (win mpi-win))


(defcfun ("PMPI_Win_unlock_all" %pmpi-win-unlock-all)
    mpi-error-code
  (win mpi-win))


(defcfun ("PMPI_Win_wait" %pmpi-win-wait)
    mpi-error-code
  (win mpi-win))


(defcfun ("PMPI_Wtick" %pmpi-wtick)
    :double)


(defcfun ("PMPI_Wtime" %pmpi-wtime)
    :double)


(defcfun ("PMPI_T_init_thread" %pmpi-t-init-thread)
    mpi-error-code
  (required :int)
  (provided (:pointer :int)))


(defcfun ("PMPI_T_finalize" %pmpi-t-finalize)
    mpi-error-code)


(defcfun ("PMPI_T_cvar_get_num" %pmpi-t-cvar-get-num)
    mpi-error-code
  (num-cvar (:pointer :int)))


(defcfun ("PMPI_T_cvar_get_info" %pmpi-t-cvar-get-info)
    mpi-error-code
  (cvar-index :int)
  (name (:pointer :char))
  (name-len (:pointer :int))
  (verbosity (:pointer :int))
  (datatype (:pointer mpi-datatype))
  (enumtype (:pointer mpi-t-enum))
  (desc (:pointer :char))
  (desc-len (:pointer :int))
  (bind (:pointer :int))
  (scope (:pointer :int)))


(defcfun ("PMPI_T_cvar_get_index" %pmpi-t-cvar-get-index)
    mpi-error-code
  (name (:pointer :char))
  (cvar-index (:pointer :int)))


(defcfun ("PMPI_T_cvar_handle_alloc" %pmpi-t-cvar-handle-alloc)
    mpi-error-code
  (cvar-index :int)
  (obj-handle :pointer)
  (handle (:pointer mpi-t-cvar-handle))
  (count (:pointer :int)))


(defcfun ("PMPI_T_cvar_handle_free" %pmpi-t-cvar-handle-free)
    mpi-error-code
  (handle (:pointer mpi-t-cvar-handle)))


(defcfun ("PMPI_T_cvar_read" %pmpi-t-cvar-read)
    mpi-error-code
  (handle mpi-t-cvar-handle)
  (buf :pointer))


(defcfun ("PMPI_T_cvar_write" %pmpi-t-cvar-write)
    mpi-error-code
  (handle mpi-t-cvar-handle)
  (buf :pointer))


(defcfun ("PMPI_T_category_get_num" %pmpi-t-category-get-num)
    mpi-error-code
  (num-cat (:pointer :int)))


(defcfun ("PMPI_T_category_get_info" %pmpi-t-category-get-info)
    mpi-error-code
  (cat-index :int)
  (name (:pointer :char))
  (name-len (:pointer :int))
  (desc (:pointer :char))
  (desc-len (:pointer :int))
  (num-cvars (:pointer :int))
  (num-pvars (:pointer :int))
  (num-categories (:pointer :int)))


(defcfun ("PMPI_T_category_get_index" %pmpi-t-category-get-index)
    mpi-error-code
  (name (:pointer :char))
  (category-index (:pointer :int)))


(defcfun ("PMPI_T_category_get_cvars" %pmpi-t-category-get-cvars)
    mpi-error-code
  (cat-index :int)
  (len :int)
  (indices (:pointer :int)))


(defcfun ("PMPI_T_category_get_pvars" %pmpi-t-category-get-pvars)
    mpi-error-code
  (cat-index :int)
  (len :int)
  (indices (:pointer :int)))


(defcfun ("PMPI_T_category_get_categories" %pmpi-t-category-get-categories)
    mpi-error-code
  (cat-index :int)
  (len :int)
  (indices (:pointer :int)))


(defcfun ("PMPI_T_category_changed" %pmpi-t-category-changed)
    mpi-error-code
  (stamp (:pointer :int)))


(defcfun ("PMPI_T_pvar_get_num" %pmpi-t-pvar-get-num)
    mpi-error-code
  (num-pvar (:pointer :int)))


(defcfun ("PMPI_T_pvar_get_info" %pmpi-t-pvar-get-info)
    mpi-error-code
  (pvar-index :int)
  (name (:pointer :char))
  (name-len (:pointer :int))
  (verbosity (:pointer :int))
  (var-class (:pointer :int))
  (datatype (:pointer mpi-datatype))
  (enumtype (:pointer mpi-t-enum))
  (desc (:pointer :char))
  (desc-len (:pointer :int))
  (bind (:pointer :int))
  (readonly (:pointer :int))
  (continuous (:pointer :int))
  (atomic (:pointer :int)))


(defcfun ("PMPI_T_pvar_get_index" %pmpi-t-pvar-get-index)
    mpi-error-code
  (name (:pointer :char))
  (var-class :int)
  (pvar-index (:pointer :int)))


(defcfun ("PMPI_T_pvar_session_create" %pmpi-t-pvar-session-create)
    mpi-error-code
  (session (:pointer mpi-t-pvar-session)))


(defcfun ("PMPI_T_pvar_session_free" %pmpi-t-pvar-session-free)
    mpi-error-code
  (session (:pointer mpi-t-pvar-session)))


(defcfun ("PMPI_T_pvar_handle_alloc" %pmpi-t-pvar-handle-alloc)
    mpi-error-code
  (session mpi-t-pvar-session)
  (pvar-index :int)
  (obj-handle :pointer)
  (handle (:pointer mpi-t-pvar-handle))
  (count (:pointer :int)))


(defcfun ("PMPI_T_pvar_handle_free" %pmpi-t-pvar-handle-free)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle (:pointer mpi-t-pvar-handle)))


(defcfun ("PMPI_T_pvar_start" %pmpi-t-pvar-start)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle))


(defcfun ("PMPI_T_pvar_stop" %pmpi-t-pvar-stop)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle))


(defcfun ("PMPI_T_pvar_read" %pmpi-t-pvar-read)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle)
  (buf :pointer))


(defcfun ("PMPI_T_pvar_write" %pmpi-t-pvar-write)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle)
  (buf :pointer))


(defcfun ("PMPI_T_pvar_reset" %pmpi-t-pvar-reset)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle))


(defcfun ("PMPI_T_pvar_readreset" %pmpi-t-pvar-readreset)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle)
  (buf :pointer))


(defcfun ("PMPI_T_enum_get_info" %pmpi-t-enum-get-info)
    mpi-error-code
  (enumtype mpi-t-enum)
  (num (:pointer :int))
  (name (:pointer :char))
  (name-len (:pointer :int)))


(defcfun ("PMPI_T_enum_get_item" %pmpi-t-enum-get-item)
    mpi-error-code
  (enumtype mpi-t-enum)
  (index :int)
  (value (:pointer :int))
  (name (:pointer :char))
  (name-len (:pointer :int)))


(defcfun ("MPI_T_init_thread" %mpi-t-init-thread)
    mpi-error-code
  (required :int)
  (provided (:pointer :int)))


(defcfun ("MPI_T_finalize" %mpi-t-finalize)
    mpi-error-code)


(defcfun ("MPI_T_cvar_get_num" %mpi-t-cvar-get-num)
    mpi-error-code
  (num-cvar (:pointer :int)))


(defcfun ("MPI_T_cvar_get_info" %mpi-t-cvar-get-info)
    mpi-error-code
  (cvar-index :int)
  (name (:pointer :char))
  (name-len (:pointer :int))
  (verbosity (:pointer :int))
  (datatype (:pointer mpi-datatype))
  (enumtype (:pointer mpi-t-enum))
  (desc (:pointer :char))
  (desc-len (:pointer :int))
  (bind (:pointer :int))
  (scope (:pointer :int)))


(defcfun ("MPI_T_cvar_get_index" %mpi-t-cvar-get-index)
    mpi-error-code
  (name (:pointer :char))
  (cvar-index (:pointer :int)))


(defcfun ("MPI_T_cvar_handle_alloc" %mpi-t-cvar-handle-alloc)
    mpi-error-code
  (cvar-index :int)
  (obj-handle :pointer)
  (handle (:pointer mpi-t-cvar-handle))
  (count (:pointer :int)))


(defcfun ("MPI_T_cvar_handle_free" %mpi-t-cvar-handle-free)
    mpi-error-code
  (handle (:pointer mpi-t-cvar-handle)))


(defcfun ("MPI_T_cvar_read" %mpi-t-cvar-read)
    mpi-error-code
  (handle mpi-t-cvar-handle)
  (buf :pointer))


(defcfun ("MPI_T_cvar_write" %mpi-t-cvar-write)
    mpi-error-code
  (handle mpi-t-cvar-handle)
  (buf :pointer))


(defcfun ("MPI_T_category_get_num" %mpi-t-category-get-num)
    mpi-error-code
  (num-cat (:pointer :int)))


(defcfun ("MPI_T_category_get_info" %mpi-t-category-get-info)
    mpi-error-code
  (cat-index :int)
  (name (:pointer :char))
  (name-len (:pointer :int))
  (desc (:pointer :char))
  (desc-len (:pointer :int))
  (num-cvars (:pointer :int))
  (num-pvars (:pointer :int))
  (num-categories (:pointer :int)))


(defcfun ("MPI_T_category_get_index" %mpi-t-category-get-index)
    mpi-error-code
  (name (:pointer :char))
  (category-index (:pointer :int)))


(defcfun ("MPI_T_category_get_cvars" %mpi-t-category-get-cvars)
    mpi-error-code
  (cat-index :int)
  (len :int)
  (indices (:pointer :int)))


(defcfun ("MPI_T_category_get_pvars" %mpi-t-category-get-pvars)
    mpi-error-code
  (cat-index :int)
  (len :int)
  (indices (:pointer :int)))


(defcfun ("MPI_T_category_get_categories" %mpi-t-category-get-categories)
    mpi-error-code
  (cat-index :int)
  (len :int)
  (indices (:pointer :int)))


(defcfun ("MPI_T_category_changed" %mpi-t-category-changed)
    mpi-error-code
  (stamp (:pointer :int)))


(defcfun ("MPI_T_pvar_get_num" %mpi-t-pvar-get-num)
    mpi-error-code
  (num-pvar (:pointer :int)))


(defcfun ("MPI_T_pvar_get_info" %mpi-t-pvar-get-info)
    mpi-error-code
  (pvar-index :int)
  (name (:pointer :char))
  (name-len (:pointer :int))
  (verbosity (:pointer :int))
  (var-class (:pointer :int))
  (datatype (:pointer mpi-datatype))
  (enumtype (:pointer mpi-t-enum))
  (desc (:pointer :char))
  (desc-len (:pointer :int))
  (bind (:pointer :int))
  (readonly (:pointer :int))
  (continuous (:pointer :int))
  (atomic (:pointer :int)))


(defcfun ("MPI_T_pvar_get_index" %mpi-t-pvar-get-index)
    mpi-error-code
  (name (:pointer :char))
  (var-class :int)
  (pvar-index (:pointer :int)))


(defcfun ("MPI_T_pvar_session_create" %mpi-t-pvar-session-create)
    mpi-error-code
  (session (:pointer mpi-t-pvar-session)))


(defcfun ("MPI_T_pvar_session_free" %mpi-t-pvar-session-free)
    mpi-error-code
  (session (:pointer mpi-t-pvar-session)))


(defcfun ("MPI_T_pvar_handle_alloc" %mpi-t-pvar-handle-alloc)
    mpi-error-code
  (session mpi-t-pvar-session)
  (pvar-index :int)
  (obj-handle :pointer)
  (handle (:pointer mpi-t-pvar-handle))
  (count (:pointer :int)))


(defcfun ("MPI_T_pvar_handle_free" %mpi-t-pvar-handle-free)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle (:pointer mpi-t-pvar-handle)))


(defcfun ("MPI_T_pvar_start" %mpi-t-pvar-start)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle))


(defcfun ("MPI_T_pvar_stop" %mpi-t-pvar-stop)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle))


(defcfun ("MPI_T_pvar_read" %mpi-t-pvar-read)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle)
  (buf :pointer))


(defcfun ("MPI_T_pvar_write" %mpi-t-pvar-write)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle)
  (buf :pointer))


(defcfun ("MPI_T_pvar_reset" %mpi-t-pvar-reset)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle))


(defcfun ("MPI_T_pvar_readreset" %mpi-t-pvar-readreset)
    mpi-error-code
  (session mpi-t-pvar-session)
  (handle mpi-t-pvar-handle)
  (buf :pointer))


(defcfun ("MPI_T_enum_get_info" %mpi-t-enum-get-info)
    mpi-error-code
  (enumtype mpi-t-enum)
  (num (:pointer :int))
  (name (:pointer :char))
  (name-len (:pointer :int)))


(defcfun ("MPI_T_enum_get_item" %mpi-t-enum-get-item)
    mpi-error-code
  (enumtype mpi-t-enum)
  (index :int)
  (value (:pointer :int))
  (name (:pointer :char))
  (name-len (:pointer :int)))


(defcfun ("MPI_Attr_delete" %mpi-attr-delete)
    mpi-error-code
  (comm mpi-comm)
  (keyval :int))


(defcfun ("PMPI_Attr_delete" %pmpi-attr-delete)
    mpi-error-code
  (comm mpi-comm)
  (keyval :int))


(defcfun ("MPI_Attr_get" %mpi-attr-get)
    mpi-error-code
  (comm mpi-comm)
  (keyval :int)
  (attribute-val :pointer)
  (flag (:pointer :int)))


(defcfun ("PMPI_Attr_get" %pmpi-attr-get)
    mpi-error-code
  (comm mpi-comm)
  (keyval :int)
  (attribute-val :pointer)
  (flag (:pointer :int)))


(defcfun ("MPI_Attr_put" %mpi-attr-put)
    mpi-error-code
  (comm mpi-comm)
  (keyval :int)
  (attribute-val :pointer))


(defcfun ("PMPI_Attr_put" %pmpi-attr-put)
    mpi-error-code
  (comm mpi-comm)
  (keyval :int)
  (attribute-val :pointer))


(defcfun ("MPI_Keyval_create" %mpi-keyval-create)
    mpi-error-code
  (copy-fn (:pointer mpi-copy-function))
  (delete-fn (:pointer mpi-delete-function))
  (keyval (:pointer :int))
  (extra-state :pointer))


(defcfun ("PMPI_Keyval_create" %pmpi-keyval-create)
    mpi-error-code
  (copy-fn (:pointer mpi-copy-function))
  (delete-fn (:pointer mpi-delete-function))
  (keyval (:pointer :int))
  (extra-state :pointer))


(defcfun ("MPI_Keyval_free" %mpi-keyval-free)
    mpi-error-code
  (keyval (:pointer :int)))


(defcfun ("PMPI_Keyval_free" %pmpi-keyval-free)
    mpi-error-code
  (keyval (:pointer :int)))
