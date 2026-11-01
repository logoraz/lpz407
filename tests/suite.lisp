;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407/tests/suite
  (:use :cl :5am)
  (:export #:suite)
  (:documentation "Root test suite for lpz407."))

(in-package #:lpz407/tests/suite)


;;; Code:

(def-suite suite :description "lpz407 test suite")
