(defpackage #:lpz407/tests/application
  (:use #:cl #:5am)
  (:import-from #:lpz407/tests/suite
                #:suite)
  (:export #:application-suite)
  (:documentation "Tests for the application module."))

(in-package #:lpz407/tests/application)


;;; Code:

(def-suite application-suite
  :description "Tests for code/application."
  :in suite)
