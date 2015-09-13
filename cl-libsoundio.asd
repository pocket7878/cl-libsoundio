#|
  This file is a part of cl-libsoundio project.
  Copyright (c) 2015 Masato Sogame (poketo7878@gmail.com)
|#

#|
  Author: Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-libsoundio-asd
  (:use :cl :asdf))
(in-package :cl-libsoundio-asd)

(defsystem cl-libsoundio
  :version "0.1"
  :author "Masato Sogame"
  :license ""
  :depends-on (:cffi
               :cl-annot)
  :components ((:module "src"
                :components
                ((:file "cl-libsoundio"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-libsoundio-test))))
