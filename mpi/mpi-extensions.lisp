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

#+nil
(defun mpi-send-anything (object dest &key
                                        (tag 0)
                                        (comm *standard-communicator*))
  "MPI-SEND-ANYTHING is a slower but more general variant of MPI-SEND. It can
  transmit any object to a matching MPI-RECEIVE-ANYTHING."
  )
#+nil
(defun mpi-receive-anything (source &key
                                      (tag +mpi-any-tag+)
                                      (comm *standard-communicator*))
  "MPI-RECEIVE-ANYTHING returns an object that was passed to a matching
  MPI-SEND-ANYTHING." )
#+nil
(defun mpi-broadcast-anything (object root &key
                                             (tag +mpi-any-tag+)
                                             (comm *standard-communicator*))
  "The node with rank ROOT sends the given object to every other rank in the
  communicator COMM.")
