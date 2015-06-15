#| -*- Mode: Lisp; indent-tabs-mode: nil -*-

Lispy extensions to MPI

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

(in-package :mpi)

(defun mpi-send-anything (object dest &key
                                        (tag 0)
                                        (comm *standard-communicator*))
  "MPI-SEND-ANYTHING is a slower but more general variant of MPI-SEND. It can
  transmit any object to a matching MPI-RECEIVE-ANYTHING."
  (let ((buffer (conspack:encode object :stream :static)))
    (mpi-send buffer dest :tag tag :comm comm)
    (free-static-vector buffer)))

(defun mpi-receive-anything (source &key
                                      (tag +mpi-any-tag+)
                                      (comm *standard-communicator*))
  "MPI-RECEIVE-ANYTHING returns an object that was passed to a matching
  MPI-SEND-ANYTHING."
  (let* ((len (mpi-probe source))
         (buffer (make-static-vector len :element-type '(unsigned-byte 8))))
    (mpi-receive buffer source :tag tag :comm comm)
    (prog1 (conspack:decode buffer)
      (free-static-vector buffer))))

(defun mpi-broadcast-anything (root &key
                                      (comm *standard-communicator*)
                                      (object))
  "The node with rank ROOT sends the given object to every other rank in the
  communicator COMM."
  (cond
    ((= root (mpi-comm-rank))
     (let* ((sendbuf (conspack:encode object :stream :static))
            (size (make-static-vector 1 :element-type '(signed-byte 64)
                                        :initial-element (length sendbuf))))
       (mpi-broadcast size root :comm comm)
       (mpi-broadcast sendbuf root :comm comm)
       (prog1 object
         (free-static-vector sendbuf)
         (free-static-vector size))))
    (t
     (let ((size (make-static-vector 1 :element-type '(signed-byte 64))))
       (mpi-broadcast size root :comm comm)
       (let ((recvbuf (make-static-vector (aref size 0))))
         (mpi-broadcast recvbuf root :comm comm)
         (prog1 (conspack:decode recvbuf)
           (free-static-vector recvbuf)
           (free-static-vector size)))))))
