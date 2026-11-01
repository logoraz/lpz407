;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407
  (:use #:cl)
  (:local-nicknames (#:bluez #:lpz407/base/bluez)
                    (#:state #:lpz407/base/app-state)
                    (#:config #:lpz407/base/config)
                    (#:history #:lpz407/base/history)
                    (#:application #:lpz407/ui/application))
  (:export #:main)
  (:documentation "Entry point: connects the speaker, the state and the
window."))

(in-package #:lpz407)


;;; Code:

(defun active-p (session)
  "Return true if SESSION exists and has not finished or failed."
  (and session
       (member (bluez:session-status session)
               '(:connecting :handshaking :connected))))

(defun main ()
  "Show the window until it closes. The store starts from the saved
history and keeps it up to date. Nothing connects until the connect button
is pressed; the session then reports into the store, and the window shows
the store and sends commands to the session. Disconnect from the speaker on
the way out."
  (let* ((config (config:load-config))
         (history (history:load-history))
         (store (state:make-store (history:restore history)))
         (session nil))
    (history:track store :previous history)
    (labels ((connect ()
               (unless (active-p session)
                 (setf session
                       (bluez:connect
                        :on-signal (lambda (signal)
                                     (state:update store signal))
                        :on-status (lambda (status detail)
                                     (state:update-status store status
                                                          detail)))))
               nil)
             (disconnect ()
               (let ((old session))
                 (when old
                   (sb-thread:make-thread
                    (lambda ()
                      (bluez:disconnect old))
                    :name "lpz407-disconnect")))
               nil)
             (command (name)
               (when (active-p session)
                 (bluez:send-command session name))
               nil))
      (unwind-protect
           (application:run store config
                            :connect #'connect
                            :disconnect #'disconnect
                            :command #'command)
        (when session
          (bluez:disconnect session))))))
