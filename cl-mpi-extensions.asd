(defsystem #:cl-mpi-extensions
  :depends-on (#:cl-mpi #:cffi #:cl-conspack)
  :components
  ((:module "extensions"
            :components
            ((:file "package")
             (:file "transmit-anything" :depends-on ("package"))))))
