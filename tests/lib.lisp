;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407/tests/lib
  (:use #:cl #:5am)
  (:import-from #:lpz407/tests/suite
                #:suite)
  (:local-nicknames (#:gatt #:lpz407/lib/gatt))
  (:export #:lib-suite)
  (:documentation "Tests for the lib module."))

(in-package #:lpz407/tests/lib)


;;; Code:

(def-suite lib-suite
  :description "Tests for code/lib."
  :in suite)

(in-suite lib-suite)

;;; gatt

(gatt:define-protocol *fixture*
  "Entries for testing DEFINE-PROTOCOL."
  (volume-up
    :command "8002"
    :label "Volume Up"
    "Raise the speaker volume one step.")
  (volume-up-confirmed
    :signal "c002"
    :label "Volume Up"
    "Speaker confirms it received VOLUME-UP.")
  (handshake-connected
    :signal "d40003"
    :label "Connected"
    "Handshake finished; the speaker accepts commands.")
  (service-id
    :uuid "0000FDC2-0000-1000-8000-00805f9b34fb"
    :label "Service"
    "The service the speaker advertises.")
  (bare-command
    :command "ffff"
    "An entry that gives no label."))

(gatt:define-protocol *other*
  "A second protocol that reuses tags of *FIXTURE*."
  (other-confirmed
    :signal "c002"
    :label "Other"
    "A signal with the tag of VOLUME-UP-CONFIRMED.")
  (other-service
    :uuid "0000fdc2-0000-1000-8000-00805f9b34fb"
    :label "Other service"
    "A UUID that *FIXTURE* also defines."))

(test hex->octets-parses-pairs
  "HEX->OCTETS turns hex digit pairs into a vector of octets."
  (is (equalp #(128 2) (gatt:hex->octets "8002")))
  (is (equalp #(212 0 3) (gatt:hex->octets "D40003")))
  (is (typep (gatt:hex->octets "8002")
             '(simple-array (unsigned-byte 8) (*)))))

(test hex->octets-rejects-bad-input
  "HEX->OCTETS signals an error for odd lengths and non hex characters."
  (signals error (gatt:hex->octets "800"))
  (signals error (gatt:hex->octets "80zz"))
  (signals error (gatt:hex->octets "+802")))

(test octets->hex-writes-lower-case
  "OCTETS->HEX pads each octet to two digits and reverses HEX->OCTETS."
  (is (string= "8002" (gatt:octets->hex #(128 2))))
  (is (string= "0a0b" (gatt:octets->hex #(10 11))))
  (is (string= "d40003" (gatt:octets->hex (gatt:hex->octets "d40003")))))

(test define-protocol-command-encodes-tag
  "A :COMMAND entry defines a struct whose ENCODE returns its tag."
  (is (volume-up-p (make-volume-up)))
  (is (equalp #(128 2) (gatt:encode (make-volume-up))))
  (is (typep (gatt:encode (make-volume-up))
             '(simple-array (unsigned-byte 8) (*))))
  (is (string= "Raise the speaker volume one step."
               (documentation 'volume-up 'structure))))

(test define-protocol-signal-decodes
  "A :SIGNAL entry registers its struct so DECODE-SIGNAL can build it."
  (is (volume-up-confirmed-p
       (gatt:decode-signal *fixture* (gatt:hex->octets "c002"))))
  (is (handshake-connected-p
       (gatt:decode-signal *fixture* #(#xd4 #x00 #x03))))
  (is (handshake-connected-p
       (gatt:decode-signal *fixture* (gatt:hex->octets "D40003")))))

(test decode-signal-rejects-unknown-tags
  "DECODE-SIGNAL signals an error for unregistered tags and commands."
  (signals error (gatt:decode-signal *fixture* #(1 2)))
  (signals error (gatt:decode-signal *fixture* (gatt:hex->octets "8002"))))

(test define-protocol-uuid-encodes-and-decodes
  "A :UUID entry defines a struct that ENCODE and DECODE-UUID round trip."
  (is (service-id-p (make-service-id)))
  (is (string= "0000fdc2-0000-1000-8000-00805f9b34fb"
               (gatt:encode (make-service-id))))
  (is (service-id-p
       (gatt:decode-uuid *fixture* "0000fdc2-0000-1000-8000-00805f9b34fb")))
  (is (service-id-p
       (gatt:decode-uuid *fixture* "0000FDC2-0000-1000-8000-00805F9B34FB"))))

(test decode-uuid-ignores-unknown-uuids
  "DECODE-UUID returns NIL for a UUID that the protocol does not define."
  (is (null (gatt:decode-uuid *fixture*
                              "00001800-0000-1000-8000-00805f9b34fb"))))

(test define-protocol-includes-base-types
  "Each kind of entry includes the base type for its kind."
  (is (gatt:gatt-command-p (make-volume-up)))
  (is (gatt:gatt-signal-p (make-volume-up-confirmed)))
  (is (gatt:gatt-uuid-p (make-service-id)))
  (is (not (gatt:gatt-command-p (make-volume-up-confirmed))))
  (is (not (gatt:gatt-signal-p (make-volume-up)))))

(test encode-rejects-signals
  "ENCODE has no method for a signal, which the app never sends."
  (signals error (gatt:encode (make-volume-up-confirmed))))

(defgeneric describe-signal (signal)
  (:documentation "Name SIGNAL, to check dispatch on entry types."))

(defmethod describe-signal ((signal gatt:gatt-signal))
  :other)

(defmethod describe-signal ((signal handshake-connected))
  :connected)

(test define-protocol-types-support-dispatch
  "Methods can specialize on an entry type or its base type."
  (is (eq :connected (describe-signal (make-handshake-connected))))
  (is (eq :other (describe-signal (make-volume-up-confirmed)))))

(test define-protocol-stores-tag-and-label
  "Every instance holds its lower case tag and its label."
  (is (string= "8002" (gatt:gatt-command-tag (make-volume-up))))
  (is (string= "Volume Up" (gatt:gatt-command-label (make-volume-up))))
  (is (string= "c002" (gatt:gatt-signal-tag (make-volume-up-confirmed))))
  (is (string= "Connected"
               (gatt:gatt-signal-label (make-handshake-connected))))
  (is (string= "0000fdc2-0000-1000-8000-00805f9b34fb"
               (gatt:gatt-uuid-tag (make-service-id))))
  (is (string= "Service" (gatt:gatt-uuid-label (make-service-id))))
  (is (null (gatt:gatt-command-label (make-bare-command)))))

(test make-builds-entries-by-name
  "MAKE builds an instance from the name of an entry of the protocol."
  (is (volume-up-p (gatt:make *fixture* 'volume-up)))
  (is (handshake-connected-p (gatt:make *fixture* 'handshake-connected)))
  (is (service-id-p (gatt:make *fixture* 'service-id)))
  (is (equalp #(128 2) (gatt:encode (gatt:make *fixture* 'volume-up))))
  (signals error (gatt:make *fixture* 'cons))
  (signals error (gatt:make *fixture* 'gatt:gatt-command))
  (signals error (gatt:make *fixture* 'other-confirmed)))

(test decode-keeps-tag-and-label
  "A decoded instance carries the same data as a made one."
  (let ((signal (gatt:decode-signal *fixture* #(#xd4 #x00 #x03))))
    (is (string= "d40003" (gatt:gatt-signal-tag signal)))
    (is (string= "Connected" (gatt:gatt-signal-label signal)))))

(test protocols-are-independent
  "Two protocols may reuse a tag without affecting each other."
  (is (volume-up-confirmed-p
       (gatt:decode-signal *fixture* #(#xc0 #x02))))
  (is (other-confirmed-p
       (gatt:decode-signal *other* #(#xc0 #x02))))
  (is (service-id-p
       (gatt:decode-uuid *fixture* "0000fdc2-0000-1000-8000-00805f9b34fb")))
  (is (other-service-p
       (gatt:decode-uuid *other* "0000fdc2-0000-1000-8000-00805f9b34fb"))))

(test define-protocol-rejects-malformed-forms
  "DEFINE-PROTOCOL signals an error at expansion time for bad forms."
  (signals error
    (macroexpand-1 '(gatt:define-protocol p (a :command "80" :signal "c0"))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p (a "Only a docstring."))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p (a :command "80" :other "x"))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p (a :command "8"))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p (a :command))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p (a :command "80" :label 5))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p (a :uuid "not-a-uuid"))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p 5)))
  (signals error
    (macroexpand-1 '(gatt:define-protocol "p" (a :command "80")))))

(test define-protocol-rejects-duplicates
  "DEFINE-PROTOCOL signals an error for a reused name or tag."
  (signals error
    (macroexpand-1 '(gatt:define-protocol p
                     (a :command "80")
                     (a :command "81"))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p
                     (a :signal "c0")
                     (b :signal "C0"))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p
                     (a :command "80")
                     (b :command "80"))))
  (signals error
    (macroexpand-1 '(gatt:define-protocol p
                     (a :uuid "0000fdc2-0000-1000-8000-00805f9b34fb")
                     (b :uuid "0000FDC2-0000-1000-8000-00805f9b34fb"))))
  (is (consp (macroexpand-1 '(gatt:define-protocol p
                              (a :command "80")
                              (b :signal "80"))))))
