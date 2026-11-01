;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407/base/history
  (:use #:cl)
  (:local-nicknames (#:config #:lpz407/base/config)
                    (#:state #:lpz407/base/app-state))
  (:export #:history-file
           #:load-history
           #:save-history
           #:restore
           #:snapshot
           #:track)
  (:documentation "Remembers the last levels, input and connection result in
history.sexp, so the next start shows where the speaker was left."))

(in-package #:lpz407/base/history)


;;; Code:

(defparameter *schema*
  (let ((speaker (state:make-speaker)))
    (list :volume (list :type :integer
                        :min 0
                        :max state:+volume-max+
                        :default (state:speaker-volume speaker))
          :bass (list :type :integer
                      :min 0
                      :max state:+bass-max+
                      :default (state:speaker-bass speaker))
          :input (list :type :enum
                       :values '(nil :input-bt :input-aux :input-usb)
                       :default nil)
          :result (list :type :enum
                        :values '(nil :connected :failed :disconnected)
                        :default nil)
          :detail (list :type :string :default nil)))
  "The shape of history.sexp. The defaults are those of a new speaker, which
puts the volume and the bass in the middle of their ranges.")

(defun history-file ()
  "Return the pathname of history.sexp, next to config.sexp."
  (config:config-file "history.sexp"))

(defun load-history (&optional (path (history-file)))
  "Return the history stored in the file at PATH, a plist shaped like
*SCHEMA*. A missing file gives the defaults of a new speaker."
  (config:load-data path *schema*))

(defun write-history (history stream)
  "Write HISTORY, a plist, to STREAM as one form, leaving out the keys that
have no value."
  (let ((*print-case* :downcase)
        (*package* (find-package '#:cl-user)))
    (format stream "(~{~S ~S~^~% ~})~%"
            (loop :for (key value) :on history :by #'cddr
                  :when value
                    :append (list key value)))))

(defun save-history (history &optional (path (history-file)))
  "Save HISTORY, a plist shaped like *SCHEMA*, to the file at PATH. The new
file replaces the old one in a single step, so a crash cannot leave half a
file behind. Return PATH."
  (ensure-directories-exist path)
  (let ((temporary (make-pathname :type "tmp" :defaults path)))
    (with-open-file (out temporary :direction :output
                                   :if-exists :supersede
                                   :external-format :utf-8)
      (write-history history out))
    (uiop:rename-file-overwriting-target temporary path))
  path)

(defun restore (history)
  "Return a speaker with the levels and the input of HISTORY. The session
status is not restored, since no session survives a restart."
  (state:make-speaker :volume (getf history :volume)
                      :bass (getf history :bass)
                      :input-source (getf history :input)))

(defun snapshot (speaker previous)
  "Return the history for SPEAKER. PREVIOUS is the history before it. The
connection result changes only when the session reaches an end state, so
the steps in between leave it as it was."
  (let ((status (state:speaker-status speaker)))
    (list :volume (state:speaker-volume speaker)
          :bass (state:speaker-bass speaker)
          :input (state:speaker-input-source speaker)
          :result (if (member status '(:connected :failed :disconnected))
                      status
                      (getf previous :result))
          :detail (if (member status '(:connected :failed :disconnected))
                      (state:speaker-detail speaker)
                      (getf previous :detail)))))

(defun track (store &key (path (history-file)) (previous (load-history path)))
  "Save the history to the file at PATH whenever STORE changes it. PREVIOUS
is the history already stored. Return the id that STATE:UNSUBSCRIBE takes."
  (let ((saved previous))
    (state:subscribe store
                     (lambda (speaker)
                       (let ((history (snapshot speaker saved)))
                         (unless (equal history saved)
                           (save-history history path)
                           (setf saved history)))))))
