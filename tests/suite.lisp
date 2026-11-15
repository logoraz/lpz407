(defpackage #:lpz407/tests/suite
  (:use :cl :5am)
  (:export #:suite)
  (:documentation "Root test suite for lpz407."))

(in-package #:lpz407/tests/suite)


;;; Code:

(def-suite suite :description "lpz407 test suite")
