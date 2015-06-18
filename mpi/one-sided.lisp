#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI One-Sided Communications

Copyright (c) 2008,2009  Alex Fukunaga
Copyright (C) 2014,2015  Marco Heisig <marco.heisig@fau.de>

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

(in-package #:mpi)

#|
(defmpifun "MPI_Accumulate" (*origin_addr origin_count origin_datatype target_rank target_disp target_count target_datatype op win) :introduced "2.0")
(defmpifun "MPI_Compare_and_swap" (*origin_addr *compare_addr *result_addr datatype target_rank target_disp win) :introduced "3.0")
(defmpifun "MPI_Fetch_and_op" () :introduced "3.0")
(defmpifun "MPI_Get" () :introduced "2.0")
(defmpifun "MPI_Get_accumulate" () :introduced "3.0")
(defmpifun "MPI_Put" () :introduced "2.0")
(defmpifun "MPI_Raccumulate" () :introduced "3.0")
(defmpifun "MPI_Rget" () :introduced "3.0")
(defmpifun "MPI_Rget_accumulate" () :introduced "3.0")
(defmpifun "MPI_Rput" () :introduced "3.0")
(defmpifun "MPI_Win_allocate" () :introduced "3.0")
(defmpifun "MPI_Win_allocate_shared" () :introduced "3.0")
(defmpifun "MPI_Win_attach" () :introduced "3.0")
(defmpifun "MPI_Win_complete" () :introduced "2.0")
(defmpifun "MPI_Win_create" () :introduced "2.0")
(defmpifun "MPI_Win_create_dynamic" () :introduced "3.0")
(defmpifun "MPI_Win_detach" () :introduced "3.0")
(defmpifun "MPI_Win_fence" () :introduced "2.0")
(defmpifun "MPI_Win_flush" () :introduced "3.0")
(defmpifun "MPI_Win_flush_all" () :introduced "3.0")
(defmpifun "MPI_Win_flush_local" () :introduced "3.0")
(defmpifun "MPI_Win_flush_local_all" () :introduced "3.0")
(defmpifun "MPI_Win_free" (*win) :introduced "2.0")
(defmpifun "MPI_Win_get_group" (win *group) :introduced "2.0")
(defmpifun "MPI_Win_get_info" (win *info_used) :introduced "3.0")
(defmpifun "MPI_Win_lock" () :introduced "2.0")
(defmpifun "MPI_Win_lock_all" () :introduced "3.0")
(defmpifun "MPI_Win_post" (group assert win) :introduced "2.0")
(defmpifun "MPI_Win_set_info" (win info) :introduced "3.0")
(defmpifun "MPI_Win_shared_query" () :introduced "3.0")
(defmpifun "MPI_Win_start" () :introduced "2.0")
(defmpifun "MPI_Win_sync" (win) :introduced "3.0")
(defmpifun "MPI_Win_test" (win *flag) :introduced "2.0")
(defmpifun "MPI_Win_unlock" (rank win) :introduced "2.0")
(defmpifun "MPI_Win_unlock_all" (rank win) :introduced "3.0")
(defmpifun "MPI_Win_wait" (win) :introduced "2.0")
|#
