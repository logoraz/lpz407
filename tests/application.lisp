;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

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
