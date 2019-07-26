#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

MPI file functions

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

(defun mpi-file-open (filename amode &key (info +mpi-info-null+)
                                       (comm *standard-communicator*))
  "Open file named FILENAME with file access mode AMODE passing information
object via INFO and using the communicator COMM.

Returns a MPI-FILE handle to the open file."
  (declare (type string filename)
           (type fixnum amode)
           (type mpi-info info)
           (type mpi-comm comm))
  (with-foreign-string (name filename)
    (mpi::with-foreign-results ((file-handle 'mpi::mpi-file))
      (%mpi-file-open comm name amode info file-handle))))

(defun mpi-file-close (file-handle)
  "Close file associated with FILE-HANDLE."
  (declare (type mpi-file file-handle))
  (with-foreign-object (fh 'mpi-file)
    (setf (mem-ref fh 'mpi-file) file-handle)
    (%mpi-file-close fh)))

(defmacro with-open-mpi-file ((file-handle filename amode &key (info +mpi-info-null+)
                                                            (comm *standard-communicator*))
                              &body body)
  `(let ((,file-handle (mpi-file-open ,filename ,amode :info ,info :comm ,comm)))
     (unwind-protect
          (progn ,@body)
       (mpi-file-close ,file-handle))))

(defun mpi-file-read (file-handle array &key start end)
  (declare (type mpi-file file-handle)
           (type simple-array array)
           (type index start end))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end)
    (with-foreign-object (status '(:struct mpi-status))
      (%mpi-file-read file-handle ptr count type status)
      (with-foreign-results ((count 'mpi-count))
        (%mpi-get-count status +mpi-int+ count)))))

(defun mpi-file-write (file-handle array &key start end)
  (declare (type mpi-file file-handle)
           (type simple-array array)
           (type index start end))
  (multiple-value-bind (ptr type count)
      (static-vector-mpi-data array start end)
    (with-foreign-object (status '(:struct mpi-status))
      (%mpi-file-write file-handle ptr count type status)
      (with-foreign-results ((count 'mpi-count))
        (%mpi-get-count status +mpi-int+ count)))))

(defun mpi-file-seek (file-handle offset whence)
  "Non-collectively update individual file pointer of FILE-HANDLE according
to WHENCE using OFFSET."
  (declare (type mpi-file file-handle))
  (%mpi-file-seek file-handle offset whence))
