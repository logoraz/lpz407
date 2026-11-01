;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407/base/bluez
  (:use #:cl)
  (:import-from #:iolib
                #:with-event-base
                #:event-dispatch)
  (:import-from #:dbus
                #:with-open-bus
                #:bus-connection
                #:system-server-addresses
                #:get-managed-objects
                #:invoke-method
                #:add-match
                #:drain-pending-messages
                #:message-body)
  (:local-nicknames (#:z407 #:lpz407/base/z407-protocol)
                    (#:conc #:sb-concurrency))
  (:export #:*trace*
           #:session
           #:session-status
           #:session-detail
           #:connect
           #:disconnect
           #:send-command)
  (:documentation "BlueZ D-Bus transport for the Z407 protocol: discover and
connect to the speaker, write commands and report signals."))

(in-package #:lpz407/base/bluez)


;;; Code:

;;; Session
;;;
;;; One session thread owns the D-Bus connection for its whole life. The
;;; dbus library pumps a single IOLib event base, which is not safe to
;;; share, so no other thread ever calls into the bus. Other threads talk to
;;; the session through SEND-COMMAND, which queues onto a mailbox, and hear
;;; back through the two callbacks, which run on the session thread.

(defstruct (session (:constructor %make-session) (:copier nil))
  "A connection to one Z407, run by its own thread.
STATUS is one of :CONNECTING, :HANDSHAKING, :CONNECTED, :DISCONNECTED and
:FAILED, and DETAIL a string that says why it failed. ON-SIGNAL and
ON-STATUS are the callbacks given to CONNECT. OUTBOX holds the commands
waiting to be written, and RUNNING is true until DISCONNECT clears it."
  (status :connecting)
  (detail nil)
  (on-signal nil)
  (on-status nil)
  (outbox (conc:make-mailbox))
  (running t)
  (thread nil))

(defun set-status (session status &optional detail)
  "Record STATUS in SESSION and tell its ON-STATUS callback, if any.
DETAIL is a string that says more, used with :FAILED."
  (setf (session-status session) status
        (session-detail session) detail)
  (when (session-on-status session)
    (funcall (session-on-status session) status detail)))

(defun send-command (session name)
  "Queue the command NAME, such as Z407:VOLUME-UP, to be written to the
speaker by SESSION. Signal an error at once, in the calling thread, if NAME
is not a command of the Z407 protocol. Return NIL."
  (let ((command (z407:make z407:*z407* name)))
    (unless (z407:gatt-command-p command)
      (error "~S is not a Z407 command" name))
    (conc:send-message (session-outbox session) command)
    nil))


;;; Signals
;;;
;;; REACT is the protocol logic that belongs to the transport: the
;;; handshake. Every other signal is only passed on to ON-SIGNAL.

(defgeneric react (session signal)
  (:documentation "Act on SIGNAL, a signal from the speaker, for SESSION.")
  (:method (session (signal z407:gatt-signal))
    "Ignore the signals that need no reply."
    nil))

(defmethod react (session (signal z407:handshake-initiate-response))
  "Answer the first step of the handshake with the second."
  (send-command session 'z407:handshake-acknowledge))

(defun mark-connected (session)
  "Record that the speaker now accepts commands, unless already recorded."
  (unless (eq :connected (session-status session))
    (set-status session :connected)))

(defmethod react (session (signal z407:handshake-acknowledge-response))
  "Treat the acknowledge response as the end of the handshake. The speaker
accepts commands after it, but does not always send the Connected signal."
  (mark-connected session))

(defmethod react (session (signal z407:handshake-connected))
  "Record that the speaker now accepts commands."
  (mark-connected session))

(defun changed-value (body)
  "Return the octets of the Value property in BODY, the body of a
PropertiesChanged message, or NIL if BODY has none. The body is the list
\(INTERFACE CHANGED INVALIDATED), and CHANGED a list of (NAME VALUE) lists,
the shape that GET-MANAGED-OBJECTS also gives properties."
  (let ((changed (when (consp body)
                   (second body))))
    (when (listp changed)
      (loop :for entry :in changed
            :when (and (consp entry) (equal (car entry) "Value"))
              :return (coerce (second entry)
                              '(simple-array (unsigned-byte 8) (*)))))))

(defvar *trace* nil
  "NIL, or a function of one argument that is called with the octets of every
value the speaker sends, before they are decoded and also when they are not a
known signal. It is for finding out what the speaker says, for example
\(setf *trace* (lambda (octets) (format t \"~&rx ~{~2,'0X~}~%\" (coerce octets
'list)))). It runs on the session thread.")

(defun receive-signals (session messages)
  "Decode the signal in each of MESSAGES, the messages the bus delivered,
and hand it to REACT and to the ON-SIGNAL callback of SESSION. Messages
without a Value, and values that are not a Z407 signal, are skipped; the
second kind gets a warning."
  (loop :for message :in messages
        :for octets := (changed-value (message-body message))
        :for signal := (when octets
                         (when *trace*
                           (funcall *trace* octets))
                         (handler-case
                             (z407:decode-signal z407:*z407* octets)
                           (error (condition)
                             (warn "~A" condition)
                             nil)))
        :when signal
          :do (react session signal)
              (when (session-on-signal session)
                (funcall (session-on-signal session) signal))))


;;; BlueZ Calls

(defun call-bluez (connection path interface member
                   &optional (signature "") arguments)
  "Call MEMBER of INTERFACE on the BlueZ object at PATH over CONNECTION.
SIGNATURE and ARGUMENTS describe the parameters of the call. Return what
the call returns."
  (apply #'invoke-method connection member
         :path path
         :interface interface
         :destination "org.bluez"
         :signature signature
         (when arguments
           (list :arguments arguments))))

(defun uuid-of (name)
  "Return the UUID string of NAME, a UUID entry of the Z407 protocol."
  (z407:encode (z407:make z407:*z407* name)))

(defun interface-properties (interfaces name)
  "Return the properties that INTERFACES, the list of (NAME PROPERTIES)
lists of an object, holds for the interface NAME, or NIL."
  (second (assoc name interfaces :test #'string=)))

(defun property (properties name)
  "Return the value of the property NAME in PROPERTIES, a list of
\(NAME VALUE) lists, or NIL."
  (second (assoc name properties :test #'string=)))

(defun find-device-path (objects)
  "Return the object path of the first Device1 in OBJECTS, the value of
GET-MANAGED-OBJECTS, that advertises the Z407 service, or NIL."
  (let ((service (uuid-of 'z407:service-id)))
    (loop :for (path interfaces) :in objects
          :for properties := (interface-properties interfaces
                                                   "org.bluez.Device1")
          :when (member service (property properties "UUIDs")
                        :test #'string-equal)
            :return path)))

(defun find-characteristic-path (objects uuid)
  "Return the object path of the GattCharacteristic1 in OBJECTS that has
UUID, or NIL."
  (loop :for (path interfaces) :in objects
        :for properties := (interface-properties
                            interfaces "org.bluez.GattCharacteristic1")
        :when (string-equal uuid (property properties "UUID"))
          :return path))

(defun managed-objects (bus)
  "Return everything BlueZ knows, as GET-MANAGED-OBJECTS gives it."
  (get-managed-objects bus "org.bluez" "/"))

(defun poll (session thunk &key (attempts 10) (delay 1))
  "Call THUNK up to ATTEMPTS times, sleeping DELAY seconds between calls,
until it returns true, and return that value. Return NIL if it never does
or when SESSION stops running."
  (loop :repeat attempts
        :while (session-running session)
        :for result := (funcall thunk)
        :when result
          :return result
        :do (sleep delay)))

(defun discover-device (session bus adapter)
  "Return the object path of the Z407 known to ADAPTER, an adapter path.
If BlueZ does not know the speaker yet, start discovery and poll for it
and stop discovery again. Return NIL if the speaker never shows up."
  (or (find-device-path (managed-objects bus))
      (let ((connection (bus-connection bus)))
        (call-bluez connection adapter "org.bluez.Adapter1" "StartDiscovery")
        (unwind-protect
             (poll session (lambda ()
                             (find-device-path (managed-objects bus))))
          (ignore-errors
           (call-bluez connection adapter "org.bluez.Adapter1"
                       "StopDiscovery"))))))

(defun find-characteristics (session bus)
  "Return the object paths of the command and response characteristics as
two values. BlueZ lists them only after it has resolved the services of a
connected device, so poll for them."
  (poll session
        (lambda ()
          (let* ((objects (managed-objects bus))
                 (command (find-characteristic-path
                           objects (uuid-of 'z407:command-id)))
                 (response (find-characteristic-path
                            objects (uuid-of 'z407:response-id))))
            (when (and command response)
              (list command response))))))

(defun write-value (connection path octets)
  "Write OCTETS to the GATT characteristic at PATH over CONNECTION. The
options dictionary of WriteValue is passed empty."
  (call-bluez connection path "org.bluez.GattCharacteristic1" "WriteValue"
              "aya{sv}" (list octets nil)))

(defun flush-outbox (session connection path)
  "Write every queued command of SESSION to the command characteristic at
PATH over CONNECTION."
  (loop :for command := (conc:receive-message-no-hang (session-outbox session))
        :while command
        :do (write-value connection path (z407:encode command))))


;;; Session Thread

(defun pump (session event-base bus command-path)
  "Serve SESSION until it stops running. Each round writes the queued
commands, waits up to a tenth of a second on EVENT-BASE for the bus to
deliver messages, and handles the signals among them."
  (let ((connection (bus-connection bus)))
    (loop :while (session-running session)
          :do (flush-outbox session connection command-path)
              (event-dispatch event-base :one-shot t :timeout 0.1)
              (receive-signals session (drain-pending-messages connection)))))

(defun run-session (session adapter)
  "Open the system bus, find and connect to the Z407, run the handshake
and serve SESSION until it stops running. Disconnect from the speaker on
the way out. Signal an error if the speaker or its characteristics are not
found."
  (with-event-base (event-base)
    (with-open-bus (bus (system-server-addresses) :event-base event-base)
      (let ((connection (bus-connection bus))
            (device (discover-device session bus adapter)))
        (when (and (null device) (session-running session))
          (error "The Z407 was not found"))
        (when device
          (call-bluez connection device "org.bluez.Device1" "Connect")
          (let ((response nil))
            (unwind-protect
                 (destructuring-bind (command-path response-path)
                     (or (find-characteristics session bus)
                         (error "The Z407 GATT characteristics were not found"))
                   (setf response response-path)
                   (call-bluez connection response-path
                               "org.bluez.GattCharacteristic1" "StartNotify")
                   (add-match bus
                              :type :signal
                              :interface "org.freedesktop.DBus.Properties"
                              :member "PropertiesChanged"
                              :path response-path)
                   (set-status session :handshaking)
                   (send-command session 'z407:handshake-initiate)
                   (pump session event-base bus command-path))
              (when response
                (ignore-errors
                 (call-bluez connection response
                             "org.bluez.GattCharacteristic1" "StopNotify")))
              (ignore-errors
               (call-bluez connection device "org.bluez.Device1"
                           "Disconnect")))))))))

(defun connect (&key on-signal on-status (adapter "/org/bluez/hci0"))
  "Start a session with the Z407 on its own thread and return it.
ON-SIGNAL is called with each signal struct the speaker sends, and
ON-STATUS with the new status and a detail string or NIL. Both run on the
session thread, so a UI must hand the work to its own thread. ADAPTER is
the object path of the Bluetooth adapter."
  (let ((session (%make-session :on-signal on-signal :on-status on-status)))
    (setf (session-thread session)
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (progn (run-session session adapter)
                        (set-status session :disconnected))
               (error (condition)
                 (set-status session :failed (princ-to-string condition)))))
           :name "lpz407-bluez"))
    session))

(defun disconnect (session)
  "Stop SESSION and wait for its thread to disconnect from the speaker and
finish. Do not call this from a callback of SESSION. Return NIL."
  (setf (session-running session) nil)
  (when (session-thread session)
    (sb-thread:join-thread (session-thread session) :default nil))
  nil)
