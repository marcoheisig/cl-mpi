#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI datatypes for Common Lisp

Copyright (C) 2015  Marco Heisig <marco.heisig@fau.de>

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

;;; A.2.2 Datatypes C Bindings

;; (defmpifun "MPI_Get_address")
;; (defmpifun "MPI_Get_elements")
;; (defmpifun "MPI_Get_elements_x")
(defmpifun "MPI_Pack" (*inbuf incount datatype *outbuf outsize *position comm))
;; (defmpifun "MPI_Pack_external")
;; (defmpifun "MPI_Pack_external_size")
(defmpifun "MPI_Pack_size" (incount datatype comm *size))
;; (defmpifun "MPI_Type_commit" (*datatype))
;; (defmpifun "MPI_Type_contiguous" (count oldtype *newtype))
;; (defmpifun "MPI_Type_create_darray")
;; (defmpifun "MPI_Type_create_hindexed")
;; (defmpifun "MPI_Type_create_hindexed_block")
;; (defmpifun "MPI_Type_create_hvector")
;; (defmpifun "MPI_Type_create_indexed_block")
;; (defmpifun "MPI_Type_create_resized")
;; (defmpifun "MPI_Type_create_struct")
;; (defmpifun "MPI_Type_create_subarray")
;; (defmpifun "MPI_Type_dup" (oldtype *newtype))
;; (defmpifun "MPI_Type_free" (*datatype))
;; (defmpifun "MPI_Type_get_contents")
;; (defmpifun "MPI_Type_get_envelope")
;; (defmpifun "MPI_Txpe_get_extent")
;; (defmpifun "MPI_Type_get_extent_x")
;; (defmpifun "MPI_Type_get_true_extent")
;; (defmpifun "MPI_Type_get_true_extent_x")
;; (defmpifun "MPI_Type_indexed")
(defmpifun "MPI_Type_size" (datatype *size))
;; (defmpifun "MPI_Type_size_x" (datatype *size))
;; (defmpifun "MPI_Type_vector")
(defmpifun "MPI_Unpack" (*inbuf insize *position *outbuf outcount datatype comm))
;; (defmpifun "MPI_Unpack_external")

(defun mpi-type-size (datatype)
  (with-foreign-results ((size :int))
    (%mpi-type-size datatype size)))
