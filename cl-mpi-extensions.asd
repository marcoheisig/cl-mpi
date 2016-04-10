(defsystem #:cl-mpi-extensions
  :depends-on (#:cl-mpi #:cffi #:cl-conspack)
  :license "MIT"
  :components
  ((:module "extensions"
    :components
    ((:file "package")
     (:file "transmit-anything" :depends-on ("package"))))))
