(defpackage #:lpz407/ui/view
  (:use #:cl)
  (:local-nicknames (#:state #:lpz407/base/app-state))
  (:export #:speaker-view
           #:escape-markup)
  (:documentation "Turns a speaker into plain data for the window to show,
so that what the window shows can be tested without GTK."))

(in-package #:lpz407/ui/view)


;;; Code:

(defun status-text (speaker)
  "Return the text that says what state the session of SPEAKER is in."
  (ecase (state:speaker-status speaker)
    (:disconnected "Disconnected")
    (:connecting "Connecting")
    (:handshaking "Connecting")
    (:connected "Connected")
    (:failed (let ((detail (state:speaker-detail speaker)))
               (if detail
                   (format nil "Failed: ~A" detail)
                   "Failed")))))

(defun clock-text (universal-time)
  "Return UNIVERSAL-TIME as local 12-hour time such as \"7:24 PM\", or NIL
when it is NIL."
  (when universal-time
    (multiple-value-bind (second minute hour)
        (decode-universal-time universal-time)
      (declare (ignore second))
      (format nil "~D:~2,'0D ~A" (1+ (mod (1- hour) 12)) minute
              (if (< hour 12) "AM" "PM")))))

(defun speaker-view (speaker)
  "Return a plist that describes what the window shows for SPEAKER:
:STATUS the status text, :ACTION which button the connection row offers,
:CONNECT or :DISCONNECT, :CONTROLS true if the command buttons should work,
:STATE how to color the status, :CONNECTED, :CONNECTING or :DISCONNECTED,
:VOLUME and :BASS the estimated levels, :INPUT the active input or NIL
and :ESTABLISHED the time the session became connected as \"H:MM AM\" or
\"H:MM PM\", or NIL while it is not."
  (list :status (status-text speaker)
        :state (ecase (state:speaker-status speaker)
                 (:connected :connected)
                 ((:connecting :handshaking) :connecting)
                 ((:disconnected :failed) :disconnected))
        :action (if (member (state:speaker-status speaker)
                            '(:connecting :handshaking :connected))
                    :disconnect
                    :connect)
        :controls (state:speaker-connected-p speaker)
        :volume (state:speaker-volume speaker)
        :bass (state:speaker-bass speaker)
        :input (state:speaker-input-source speaker)
        :established (clock-text (state:speaker-connected-at speaker))))

(defun escape-markup (text)
  "Return TEXT with the characters that Pango markup reads as syntax
replaced, so that text from outside, such as an error, shows as written."
  (with-output-to-string (out)
    (loop :for char :across text
          :do (case char
                (#\& (write-string "&amp;" out))
                (#\< (write-string "&lt;" out))
                (#\> (write-string "&gt;" out))
                (t (write-char char out))))))
