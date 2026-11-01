;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407/base/app-state
  (:use #:cl)
  (:local-nicknames (#:z407 #:lpz407/base/z407-protocol))
  (:export #:speaker
           #:make-speaker
           #:speaker-p
           #:speaker-status
           #:speaker-detail
           #:speaker-input-source
           #:speaker-volume
           #:speaker-bass
           #:speaker-connected-at
           #:speaker-connected-p
           #:store
           #:make-store
           #:current
           #:subscribe
           #:unsubscribe
           #:update
           #:update-status
           #:*clock*
           #:+volume-max+
           #:+bass-max+)
  (:documentation "What the app believes about the speaker, and who is told
when it changes."))

(in-package #:lpz407/base/app-state)


;;; Code:

;;; State
;;;
;;; The Z407 never reports its volume or bass, only that a command worked.
;;; So these numbers are estimates: each confirmation moves them one step.

(defconstant +volume-step+ 5
  "How far one volume command moves the estimated volume.")

(defconstant +bass-step+ 1
  "How far one bass command moves the estimated bass level.")

(defconstant +volume-max+ 100
  "The top of the volume range, which starts at 0.")

(defconstant +bass-max+ 10
  "The top of the bass range, which starts at 0.")

(defstruct (speaker (:copier copy-speaker))
  "What the app believes about the speaker at one moment. STATUS is the
status of the session, one of :CONNECTING, :HANDSHAKING, :CONNECTED,
:DISCONNECTED and :FAILED, and DETAIL says why it failed. INPUT-SOURCE is
:INPUT-BT, :INPUT-AUX, :INPUT-USB or NIL when not known. VOLUME and BASS
are estimates. Treat a speaker as a value; change it through a STORE."
  (status :disconnected)
  (detail nil)
  (connected-at nil)
  (input-source nil)
  (volume 50)
  (bass 5))

(defun speaker-connected-p (speaker)
  "Return true if SPEAKER is connected and accepts commands."
  (eq :connected (speaker-status speaker)))

(defun clamp (value maximum)
  "Return VALUE limited to the range from 0 to MAXIMUM."
  (min maximum (max 0 value)))


;;; Store

(defstruct (store (:constructor make-store
                      (&optional (speaker (make-speaker))))
                  (:copier nil))
  "Holds the current SPEAKER and the listeners to tell when it changes.
MAKE-STORE takes the speaker to start from, for one restored from history.
LOCK guards every slot, because signals arrive on the session thread while
the UI reads on its own."
  (speaker (make-speaker))
  (lock (sb-thread:make-mutex :name "lpz407-store"))
  (listeners '())
  (next-id 0))

(defun current (store)
  "Return a copy of the speaker in STORE, safe to read at any time."
  (sb-thread:with-mutex ((store-lock store))
    (copy-speaker (store-speaker store))))

(defun subscribe (store function)
  "Call FUNCTION with a copy of the new speaker whenever STORE changes.
FUNCTION runs on the thread that caused the change, which may be the
session thread, so a UI must hand the work to its own thread. Return an id
for UNSUBSCRIBE."
  (sb-thread:with-mutex ((store-lock store))
    (let ((id (incf (store-next-id store))))
      (push (cons id function) (store-listeners store))
      id)))

(defun unsubscribe (store id)
  "Stop telling the listener that SUBSCRIBE returned ID about changes."
  (sb-thread:with-mutex ((store-lock store))
    (setf (store-listeners store)
          (remove id (store-listeners store) :key #'car))))

(defun notify (listeners speaker)
  "Call each function of LISTENERS, a list of (ID . FUNCTION), with
SPEAKER. A function that signals an error gets a warning, so it cannot stop
the others or the thread that caused the change."
  (loop :for (id . function) :in listeners
        :do (handler-case (funcall function speaker)
              (error (condition)
                (warn "State listener ~D failed: ~A" id condition)))))

(defun change (store modify)
  "Replace the speaker of STORE by the speaker that MODIFY returns, given a
copy of the current one. When that differs from the current one, tell the
listeners, after releasing the lock, and return true."
  (let ((listeners '())
        (new nil))
    (sb-thread:with-mutex ((store-lock store))
      (let ((candidate (funcall modify (copy-speaker (store-speaker store)))))
        (unless (equalp candidate (store-speaker store))
          (setf (store-speaker store) candidate
                new (copy-speaker candidate)
                listeners (store-listeners store)))))
    (when new
      (notify listeners new)
      t)))


;;; Updates

(defvar *clock* #'get-universal-time
  "A function of no arguments that returns the universal time now. Tests
bind it to a fixed time.")

(defun update-status (store status &optional detail)
  "Record in STORE that the session has STATUS, with DETAIL for a failure.
The time of the speaker is set when the status becomes :CONNECTED, kept
while it stays so, and cleared otherwise. This takes the arguments of the
ON-STATUS callback of BLUEZ:CONNECT."
  (change store (lambda (speaker)
                  (setf (speaker-connected-at speaker)
                        (and (eq status :connected)
                             (or (speaker-connected-at speaker)
                                 (funcall *clock*)))
                        (speaker-status speaker) status
                        (speaker-detail speaker) detail)
                  speaker)))

(defun move (store slot step maximum)
  "Add STEP, which may be negative, to the slot of the speaker in STORE that
SLOT names, keeping it within 0 and MAXIMUM."
  (change store (lambda (speaker)
                  (setf (slot-value speaker slot)
                        (clamp (+ (slot-value speaker slot) step)
                               maximum))
                  speaker)))

(defun set-input (store input)
  "Record in STORE that INPUT, such as :INPUT-AUX, is the active input."
  (change store (lambda (speaker)
                  (setf (speaker-input-source speaker) input)
                  speaker)))

(defgeneric update (store signal)
  (:documentation "Change STORE to match SIGNAL, a signal from the speaker.
Return true if that changed it. This takes the argument of the ON-SIGNAL
callback of BLUEZ:CONNECT.")
  (:method (store (signal z407:gatt-signal))
    "Ignore the signals that say nothing about the state."
    nil))

(defmethod update (store (signal z407:volume-up-confirmed))
  "The speaker raised its volume one step."
  (move store 'volume +volume-step+ +volume-max+))

(defmethod update (store (signal z407:volume-down-confirmed))
  "The speaker lowered its volume one step."
  (move store 'volume (- +volume-step+) +volume-max+))

(defmethod update (store (signal z407:bass-up-confirmed))
  "The speaker raised its bass one step."
  (move store 'bass +bass-step+ +bass-max+))

(defmethod update (store (signal z407:bass-down-confirmed))
  "The speaker lowered its bass one step."
  (move store 'bass (- +bass-step+) +bass-max+))

(defmethod update (store (signal z407:input-bt-confirmed))
  "The speaker accepted the switch to its Bluetooth input. It reports the
change itself only when the input really changed, so the confirmation is
what tells us when the speaker was on that input already."
  (set-input store :input-bt))

(defmethod update (store (signal z407:input-aux-confirmed))
  "The speaker accepted the switch to its AUX input."
  (set-input store :input-aux))

(defmethod update (store (signal z407:input-usb-confirmed))
  "The speaker accepted the switch to its USB input."
  (set-input store :input-usb))

(defmethod update (store (signal z407:status-bt-active))
  "The speaker switched to its Bluetooth input."
  (set-input store :input-bt))

(defmethod update (store (signal z407:status-aux-active))
  "The speaker switched to its AUX input."
  (set-input store :input-aux))

(defmethod update (store (signal z407:status-usb-active))
  "The speaker switched to its USB input."
  (set-input store :input-usb))
