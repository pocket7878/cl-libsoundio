#|
  This file is a part of cl-libsoundio project.
  Copyright (c) 2015 Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-libsoundio-test-asd
  (:use :cl :asdf))
(in-package :cl-libsoundio-test-asd)

(defsystem cl-libsoundio-test
  :author "Masato Sogame"
  :license ""
  :depends-on (:cl-libsoundio
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-libsoundio"))))
  :description "Test system for cl-libsoundio"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
