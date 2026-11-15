(defpackage #:lpz407/tests/base
  (:use #:cl #:5am)
  (:import-from #:lpz407/tests/suite
                #:suite)
  (:local-nicknames (#:z407 #:lpz407/base/z407-protocol)
                    (#:bluez #:lpz407/base/bluez)
                    (#:state #:lpz407/base/app-state)
                    (#:conf #:lpz407/base/config)
                    (#:hist #:lpz407/base/history))
  (:export #:base-suite)
  (:documentation "Tests for the base module."))

(in-package #:lpz407/tests/base)


;;; Code:

(def-suite base-suite
  :description "Tests for code/base."
  :in suite)

(in-suite base-suite)

;;; z407-protocol

(defun build (name)
  "Return a new instance of NAME, an entry of the Z407 protocol."
  (z407:make z407:*z407* name))

(defun decode (octets)
  "Return the Z407 signal that OCTETS describes."
  (z407:decode-signal z407:*z407* octets))

(defun lookup (uuid)
  "Return the Z407 entry for UUID, or NIL."
  (z407:decode-uuid z407:*z407* uuid))

(defparameter *commands*
  '((z407:bass-up #(#x80 #x00) "Bass Up")
    (z407:bass-down #(#x80 #x01) "Bass Down")
    (z407:volume-up #(#x80 #x02) "Volume Up")
    (z407:volume-down #(#x80 #x03) "Volume Down")
    (z407:play-pause #(#x80 #x04) "Play/Pause")
    (z407:next-track #(#x80 #x05) "Next Track")
    (z407:previous-track #(#x80 #x06) "Previous Track")
    (z407:input-bt #(#x81 #x01) "Switch to Bluetooth input")
    (z407:input-aux #(#x81 #x02) "Switch to AUX input")
    (z407:input-usb #(#x81 #x03) "Switch to USB input")
    (z407:bluetooth-pair #(#x82 #x00) "Enable Bluetooth pairing")
    (z407:factory-reset #(#x83 #x00) "Factory Reset")
    (z407:handshake-acknowledge #(#x84 #x00) "Pairing Acknowledge")
    (z407:handshake-initiate #(#x84 #x05) "Pairing Initiate")
    (z407:ghost #(#x85 #x00) "Unknown 1")
    (z407:disconnect-chime #(#x85 #x01) "Sound 1")
    (z407:switch-chime #(#x85 #x02) "Sound 2")
    (z407:connect-chime #(#x85 #x03) "Sound 3"))
  "Each command of the spec as (NAME OCTETS LABEL).")

(defparameter *signals*
  '((z407:handshake-initiate-response #(#xd4 #x05 #x01) "Initiate Response")
    (z407:handshake-acknowledge-response #(#xd4 #x00 #x01)
     "Acknowledge Response")
    (z407:handshake-connected #(#xd4 #x00 #x03) "Connected")
    (z407:status-bt-active #(#xcf #x04) "Switched to Bluetooth (complete)")
    (z407:status-aux-active #(#xcf #x05) "Switched to AUX (complete)")
    (z407:status-usb-active #(#xcf #x06) "Switched to USB (complete)")
    (z407:status-unknown #(#xcf #x0b) "Unknown 2")
    (z407:bass-up-confirmed #(#xc0 #x00) "Bass Up")
    (z407:bass-down-confirmed #(#xc0 #x01) "Bass Down")
    (z407:volume-up-confirmed #(#xc0 #x02) "Volume Up")
    (z407:volume-down-confirmed #(#xc0 #x03) "Volume Down")
    (z407:play-pause-confirmed #(#xc0 #x04) "Play/Pause")
    (z407:next-track-confirmed #(#xc0 #x05) "Next Track")
    (z407:previous-track-confirmed #(#xc0 #x06) "Previous Track")
    (z407:input-bt-confirmed #(#xc1 #x01) "Switch Bluetooth")
    (z407:input-aux-confirmed #(#xc1 #x02) "Switch AUX")
    (z407:input-usb-confirmed #(#xc1 #x03) "Switch USB")
    (z407:bt-pair-confirmed #(#xc2 #x00) "Pairing")
    (z407:factory-reset-confirmed #(#xc3 #x00) "Factory Reset")
    (z407:ghost-confirmed #(#xc5 #x00) "Unknown 1")
    (z407:connect-chime-confirmed #(#xc5 #x03) "Sound 3")
    (z407:switch-chime-confirmed #(#xc5 #x02) "Sound 2")
    (z407:disconnect-chime-confirmed #(#xc5 #x01) "Sound 1"))
  "Each signal of the spec as (NAME OCTETS LABEL).")

(defparameter *uuids*
  '((z407:service-id "0000fdc2-0000-1000-8000-00805f9b34fb")
    (z407:command-id "c2e758b9-0e78-41e0-b0cb-98a593193fc5")
    (z407:response-id "b84ac9c6-29c5-46d4-bba1-9d534784330f"))
  "Each UUID of the spec as (NAME STRING).")

(test protocol-commands-encode-spec-octets
  "Every command encodes to the octets the spec lists."
  (is (= 18 (length *commands*)))
  (loop :for (name octets) :in *commands*
        :do (is (equalp octets (z407:encode (build name))))))

(test protocol-commands-keep-spec-labels
  "Every command carries the spec's function name as its label."
  (loop :for (name nil label) :in *commands*
        :do (is (string= label
                         (z407:gatt-command-label (build name))))))

(test protocol-commands-are-commands
  "Every command is a command and never a signal or a UUID."
  (loop :for (name) :in *commands*
        :for entry := (build name)
        :do (is (z407:gatt-command-p entry))
            (is (not (z407:gatt-signal-p entry)))
            (is (not (z407:gatt-uuid-p entry)))))

(test protocol-signals-decode-to-their-structs
  "Every signal tag decodes to the struct of that signal."
  (is (= 23 (length *signals*)))
  (loop :for (name octets) :in *signals*
        :for signal := (decode octets)
        :do (is (typep signal name))
            (is (z407:gatt-signal-p signal))))

(test protocol-signals-keep-spec-labels
  "Every signal carries the spec's function name as its label."
  (loop :for (name octets label) :in *signals*
        :do (is (string= label
                         (z407:gatt-signal-label
                          (decode octets))))
            (is (string= label
                         (z407:gatt-signal-label (build name))))))

(test protocol-handshake-follows-spec
  "The handshake commands and their responses use the spec's tags."
  (is (equalp #(#x84 #x05) (z407:encode (build 'z407:handshake-initiate))))
  (is (typep (decode #(#xd4 #x05 #x01))
             'z407:handshake-initiate-response))
  (is (equalp #(#x84 #x00)
              (z407:encode (build 'z407:handshake-acknowledge))))
  (is (typep (decode #(#xd4 #x00 #x01))
             'z407:handshake-acknowledge-response))
  (is (typep (decode #(#xd4 #x00 #x03))
             'z407:handshake-connected)))

(test protocol-chime-confirmations-match-commands
      "Each chime confirmation has the number of its command."
      (is (typep (decode #(#xc5 #x01)) 'z407:disconnect-chime-confirmed))
      (is (typep (decode #(#xc5 #x02)) 'z407:switch-chime-confirmed))
      (is (typep (decode #(#xc5 #x03)) 'z407:connect-chime-confirmed))
      (is (equalp #(#x85 #x01) (z407:encode (build 'z407:disconnect-chime))))
      (is (equalp #(#x85 #x03) (z407:encode (build 'z407:connect-chime)))))

(test protocol-uuids-encode-and-decode
  "Every UUID encodes to its string and decodes back to its struct."
  (loop :for (name uuid) :in *uuids*
        :do (is (string= uuid (z407:encode (build name))))
            (is (typep (lookup uuid) name))
            (is (typep (lookup (string-upcase uuid)) name))
            (is (z407:gatt-uuid-p (build name)))))

(test protocol-rejects-unknown-input
  "Unknown signals error, unknown UUIDs give NIL, commands do not decode."
  (signals error (decode #(#xff #xff)))
  (signals error (decode #(#x80 #x02)))
  (is (null (lookup "00001800-0000-1000-8000-00805f9b34fb"))))

(test protocol-exports-every-entry
  "Every entry of the spec is exported, and no other name is."
  (let ((names (append (mapcar #'first *commands*)
                       (mapcar #'first *signals*)
                       (mapcar #'first *uuids*))))
    (is (= 44 (length (remove-duplicates names))))
    (loop :for name :in names
          :do (is (eq :external
                      (nth-value 1 (find-symbol (symbol-name name)
                                                :lpz407/base/z407-protocol)))))
    (is (= 63(let ((count 0))
                (do-external-symbols (symbol :lpz407/base/z407-protocol count)
                  (declare (ignorable symbol))
                  (incf count)))))))

;;; bluez

(defparameter *objects*
  '(("/org/bluez" (("org.bluez.AgentManager1" nil)))
    ("/org/bluez/hci0"
     (("org.bluez.Adapter1" (("Address" "08:A1:36:57:41:90") ("Powered" t)))))
    ("/org/bluez/hci0/dev_AA"
     (("org.bluez.Device1"
       (("Name" "Headset")
        ("UUIDs" ("0000180f-0000-1000-8000-00805f9b34fb"))))
      ("org.bluez.MediaControl1" (("Connected" t)))))
    ("/org/bluez/hci0/dev_BB"
     (("org.bluez.Device1"
       (("Name" "Logi Z407")
        ("UUIDs" ("0000110b-0000-1000-8000-00805f9b34fb"
                  "0000FDC2-0000-1000-8000-00805f9b34fb"))))))
    ("/org/bluez/hci0/dev_BB/service000c/char000d"
     (("org.bluez.GattCharacteristic1"
       (("UUID" "C2E758B9-0E78-41E0-B0CB-98A593193FC5")))))
    ("/org/bluez/hci0/dev_BB/service000c/char000f"
     (("org.bluez.GattCharacteristic1"
       (("UUID" "b84ac9c6-29c5-46d4-bba1-9d534784330f"))))))
  "Managed objects in the shape the D-Bus library returns them.")

(defun new-session ()
  "Return a new session with no callbacks."
  (bluez::%make-session))

(test bluez-changed-value-reads-octets
      "CHANGED-VALUE returns the Value property as a vector of octets."
      (let ((value (bluez::changed-value
                    '("org.bluez.GattCharacteristic1"
                      (("Value" (212 5 1)))
                      nil))))
        (is (equalp #(212 5 1) value))
        (is (typep value '(simple-array (unsigned-byte 8) (*)))))
      (is (equalp #(192 2)
                  (bluez::changed-value
                   '("i" (("Value" #(192 2))) nil)))))

(test bluez-changed-value-ignores-other-bodies
      "CHANGED-VALUE returns NIL for bodies that carry no Value."
      (is (null (bluez::changed-value nil)))
      (is (null (bluez::changed-value '(":1.42"))))
      (is (null (bluez::changed-value "text")))
      (is (null (bluez::changed-value
                 '("i" (("Notifying" t)) nil)))))

(test bluez-finds-the-device-by-service-uuid
      "FIND-DEVICE-PATH matches the Z407 service UUID, ignoring case."
      (is (string= "/org/bluez/hci0/dev_BB"
                   (bluez::find-device-path *objects*)))
      (is (null (bluez::find-device-path (subseq *objects* 0 2))))
      (is (null (bluez::find-device-path nil))))

(test bluez-finds-the-characteristics-by-uuid
      "FIND-CHARACTERISTIC-PATH matches a UUID of the protocol, ignoring case."
      (is (string= "/org/bluez/hci0/dev_BB/service000c/char000d"
                   (bluez::find-characteristic-path
                    *objects* (bluez::uuid-of 'z407:command-id))))
      (is (string= "/org/bluez/hci0/dev_BB/service000c/char000f"
                   (bluez::find-characteristic-path
                    *objects* (bluez::uuid-of 'z407:response-id))))
      (is (null (bluez::find-characteristic-path
                 *objects* "00002a00-0000-1000-8000-00805f9b34fb"))))

(test bluez-send-command-queues-the-command
      "SEND-COMMAND queues an instance of the command for the session thread."
      (let ((session (new-session)))
        (is (null (bluez:send-command session 'z407:volume-up)))
        (let ((command (sb-concurrency:receive-message-no-hang
                        (bluez::session-outbox session))))
          (is (typep command 'z407:volume-up))
          (is (equalp #(128 2) (z407:encode command))))
        (is (null (sb-concurrency:receive-message-no-hang
                   (bluez::session-outbox session))))))

(test bluez-send-command-rejects-non-commands
      "SEND-COMMAND signals an error in the caller for anything but a command."
      (let ((session (new-session)))
        (signals error (bluez:send-command session 'z407:volume-up-confirmed))
        (signals error (bluez:send-command session 'z407:service-id))
        (signals error (bluez:send-command session 'cons))
        (is (null (sb-concurrency:receive-message-no-hang
                   (bluez::session-outbox session))))))

(test bluez-handshake-answers-the-first-step
      "REACT answers the initiate response with the acknowledge command."
      (let ((session (new-session)))
        (bluez::react session (z407:make z407:*z407*
                                         'z407:handshake-initiate-response))
        (is (typep (sb-concurrency:receive-message-no-hang
                    (bluez::session-outbox session))
                   'z407:handshake-acknowledge))
        (is (eq :connecting (bluez:session-status session)))))

(test bluez-handshake-completes-the-connection
      "REACT sets the status to :CONNECTED and reports it."
      (let* ((seen '())
             (session (bluez::%make-session
                       :on-status (lambda (status detail)
                                    (push (list status detail) seen)))))
        (bluez::react session (z407:make z407:*z407* 'z407:handshake-connected))
        (is (eq :connected (bluez:session-status session)))
        (is (equal '((:connected nil)) seen))))

(test bluez-ignores-other-signals
      "REACT does nothing for signals that need no reply."
      (let ((session (new-session)))
        (bluez::react session (z407:make z407:*z407* 'z407:volume-up-confirmed))
        (bluez::react session (z407:make z407:*z407* 'z407:status-aux-active))
        (is (eq :connecting (bluez:session-status session)))
        (is (null (sb-concurrency:receive-message-no-hang
                   (bluez::session-outbox session))))))

(test bluez-acknowledge-response-completes-the-connection
      "REACT sets :CONNECTED on the acknowledge response, and only once."
      (let* ((seen '())
             (session (bluez::%make-session
                       :on-status (lambda (status detail)
                                    (push (list status detail) seen)))))
        (bluez::react session (z407:make z407:*z407*
                                         'z407:handshake-acknowledge-response))
        (bluez::react session (z407:make z407:*z407* 'z407:handshake-connected))
        (is (eq :connected (bluez:session-status session)))
        (is (equal '((:connected nil)) seen))))

;;; app-state

(defun apply-signal (store name &optional (times 1))
  "Update STORE with a new instance of the signal NAME, TIMES times."
  (loop :repeat times
        :do (state:update store (build name))))

(test state-starts-disconnected-with-defaults
      "A new store holds a disconnected speaker with the default estimates."
      (let ((speaker (state:current (state:make-store))))
        (is (eq :disconnected (state:speaker-status speaker)))
        (is (not (state:speaker-connected-p speaker)))
        (is (null (state:speaker-input-source speaker)))
        (is (= 50 (state:speaker-volume speaker)))
        (is (= 5 (state:speaker-bass speaker)))))

(test state-volume-follows-confirmations
      "Each volume confirmation moves the estimate one step, within 0 and 100."
      (let ((store (state:make-store)))
        (apply-signal store 'z407:volume-up-confirmed)
        (is (= 55 (state:speaker-volume (state:current store))))
        (apply-signal store 'z407:volume-down-confirmed 2)
        (is (= 45 (state:speaker-volume (state:current store))))
        (apply-signal store 'z407:volume-up-confirmed 30)
        (is (= 100 (state:speaker-volume (state:current store))))
        (apply-signal store 'z407:volume-down-confirmed 30)
        (is (= 0 (state:speaker-volume (state:current store))))))

(test state-bass-follows-confirmations
      "Each bass confirmation moves the estimate one step, within 0 and 10."
      (let ((store (state:make-store)))
        (apply-signal store 'z407:bass-up-confirmed)
        (is (= 6 (state:speaker-bass (state:current store))))
        (apply-signal store 'z407:bass-up-confirmed 20)
        (is (= 10 (state:speaker-bass (state:current store))))
        (apply-signal store 'z407:bass-down-confirmed 20)
        (is (= 0 (state:speaker-bass (state:current store))))))

(test state-input-follows-status-signals
      "The status signals set the active input."
      (let ((store (state:make-store)))
        (apply-signal store 'z407:status-aux-active)
        (is (eq :input-aux (state:speaker-input-source (state:current store))))
        (apply-signal store 'z407:status-usb-active)
        (is (eq :input-usb (state:speaker-input-source (state:current store))))
        (apply-signal store 'z407:status-bt-active)
        (is (eq :input-bt (state:speaker-input-source (state:current store))))))

(test state-records-when-the-session-connected
      "The time is set on connecting, kept while connected and cleared after."
      (let* ((now 100)
             (store (state:make-store))
             (state:*clock* (lambda () now)))
        (state:update-status store :connected)
        (is (= 100 (state:speaker-connected-at (state:current store))))
        (setf now 200)
        (state:update-status store :connected)
        (is (= 100 (state:speaker-connected-at (state:current store))))
        (state:update-status store :disconnected)
        (is (null (state:speaker-connected-at (state:current store))))))

(test state-input-follows-confirmed-switches
      "A confirmed input switch sets the input, even when nothing was reported."
      (let ((store (state:make-store)))
        (is (null (state:speaker-input-source (state:current store))))
        (state:update store (build 'z407:input-bt-confirmed))
        (is (eq :input-bt (state:speaker-input-source (state:current store))))
        (state:update store (build 'z407:input-aux-confirmed))
        (is (eq :input-aux (state:speaker-input-source (state:current store))))
        (state:update store (build 'z407:input-usb-confirmed))
        (is (eq :input-usb (state:speaker-input-source (state:current store))))))

(test state-status-follows-the-session
      "UPDATE-STATUS records the status and the detail of a failure."
      (let ((store (state:make-store)))
        (state:update-status store :connected)
        (is (state:speaker-connected-p (state:current store)))
        (state:update-status store :failed "The Z407 was not found")
        (let ((speaker (state:current store)))
          (is (eq :failed (state:speaker-status speaker)))
          (is (string= "The Z407 was not found" (state:speaker-detail speaker)))
          (is (not (state:speaker-connected-p speaker))))))

(test state-ignores-signals-without-news
      "Other signals leave the store unchanged and tell nobody."
      (let* ((store (state:make-store))
             (calls 0))
        (state:subscribe store (lambda (speaker)
                                 (declare (ignore speaker))
                                 (incf calls)))
        (is (null (state:update store (build 'z407:handshake-connected))))
        (is (null (state:update store (build 'z407:status-unknown))))
        (is (null (state:update store (build 'z407:play-pause-confirmed))))
        (is (null (state:update-status store :disconnected)))
        (is (= 0 calls))))

(test state-tells-subscribers-of-changes
      "A subscriber gets a copy of each new speaker until it unsubscribes."
      (let* ((store (state:make-store))
             (seen '())
             (id (state:subscribe store (lambda (speaker)
                                          (push speaker seen)))))
        (is (state:update store (build 'z407:volume-up-confirmed)))
        (is (= 1 (length seen)))
        (is (= 55 (state:speaker-volume (first seen))))
        (setf (state:speaker-volume (first seen)) 0)
        (is (= 55 (state:speaker-volume (state:current store))))
        (state:unsubscribe store id)
        (state:update store (build 'z407:volume-up-confirmed))
        (is (= 1 (length seen)))))

(test state-survives-a-failing-subscriber
      "A subscriber that fails gets a warning and the others still run."
      (let* ((store (state:make-store))
             (calls 0))
        (state:subscribe store (lambda (speaker)
                                 (declare (ignore speaker))
                                 (error "A broken listener")))
        (state:subscribe store (lambda (speaker)
                                 (declare (ignore speaker))
                                 (incf calls)))
        (handler-bind ((warning #'muffle-warning))
          (is (state:update store (build 'z407:volume-up-confirmed))))
        (is (= 1 calls))))

;;; config

(defun temp-path (name)
  "Return a pathname for NAME in the temporary directory, made unique."
  (merge-pathnames (format nil "lpz407-test-~D-~A" (random 1000000) name)
                   (uiop:temporary-directory)))

(defmacro with-temp-file ((var name &optional contents) &body body)
  "Bind VAR to a temporary pathname for NAME, holding CONTENTS when given,
run BODY and delete the file afterwards."
  `(let ((,var (temp-path ,name)))
     (unwind-protect
          (progn
            (when ,contents
              (with-open-file (out ,var :direction :output)
                (write-string ,contents out)))
            ,@body)
       (ignore-errors (delete-file ,var)))))

(defun count-warnings (thunk)
  "Call THUNK, muffling its warnings, and return how many it signalled."
  (let ((count 0))
    (handler-bind ((warning (lambda (condition)
                              (incf count)
                              (muffle-warning condition))))
      (funcall thunk))
    count))

(test config-gives-defaults-without-a-file
      "A missing config file is not a problem and gives the defaults."
      (with-temp-file (path "config.sexp")
        (let (config)
          (is (zerop (count-warnings
                      (lambda () (setf config (conf:load-config path))))))
          (is (= 0.8 (conf:config-ref config :theme :alpha)))
          (is (string= "#2e3440"
                       (conf:config-ref config :theme :background)))
          (is (= 360 (conf:config-ref config :window :width)))
          (is (eq t (conf:config-ref config :controls :bass)))
          (is (string= "Connect"
                       (conf:config-ref config :labels :connect))))))

(test config-reads-the-values-of-the-file
      "Values in the file replace the defaults and the rest stays."
      (with-temp-file (path "config.sexp"
                            "(:theme (:alpha 0.5) :controls (:bass nil))")
        (let ((config (conf:load-config path)))
          (is (= 0.5 (conf:config-ref config :theme :alpha)))
          (is (null (conf:config-ref config :controls :bass)))
          (is (eq t (conf:config-ref config :controls :volume)))
          (is (= 360 (conf:config-ref config :window :width))))))

(test config-replaces-invalid-values
      "An invalid value warns and takes the default."
      (with-temp-file (path "config.sexp"
                            "(:theme (:alpha 3 :scheme :sepia))")
        (let (config)
          (is (= 2 (count-warnings
                    (lambda () (setf config (conf:load-config path))))))
          (is (= 0.8 (conf:config-ref config :theme :alpha)))
          (is (eq :dark (conf:config-ref config :theme :scheme))))))

(test config-warns-about-unknown-keys
      "An unknown key warns once and is ignored."
      (with-temp-file (path "config.sexp" "(:theme (:colour \"red\"))")
        (is (= 1 (count-warnings
                  (lambda () (conf:load-config path)))))))

(test config-never-evaluates-the-file
      "Reading the file does not run code in it."
      (with-temp-file (path "config.sexp" "#.(error \"evaluated\")")
        (let (config)
          (is (= 1 (count-warnings
                    (lambda () (setf config (conf:load-config path))))))
          (is (= 0.8 (conf:config-ref config :theme :alpha))))))

(test config-ref-rejects-typos
      "A key the schema lacks is an error in the code."
      (let ((config (conf:load-config (temp-path "none.sexp"))))
        (signals error (conf:config-ref config :theme :alfa))))

;;; history

(test history-starts-in-the-middle
      "With no history file the levels are the middle of their ranges."
      (let ((history (hist:load-history (temp-path "none.sexp"))))
        (is (= 50 (getf history :volume)))
        (is (= 5 (getf history :bass)))
        (is (null (getf history :input)))
        (is (null (getf history :result)))))

(test history-round-trips
      "Saved history loads back the same, and nil keys are left out."
      (with-temp-file (path "history.sexp")
        (hist:save-history '(:volume 65 :bass 3 :input :input-aux
                                :result :failed :detail "Not found")
                              path)
        (let ((history (hist:load-history path)))
          (is (= 65 (getf history :volume)))
          (is (= 3 (getf history :bass)))
          (is (eq :input-aux (getf history :input)))
          (is (eq :failed (getf history :result)))
          (is (string= "Not found" (getf history :detail))))
        (hist:save-history '(:volume 10 :bass 0 :input nil
                                :result nil :detail nil)
                              path)
        (is (not (search "input" (uiop:read-file-string path))))))

(test history-saving-creates-the-directory
      "Saving makes the directory of the file when it is missing."
      (let* ((directory (temp-path "dir/"))
             (path (merge-pathnames "history.sexp" directory)))
        (unwind-protect
             (progn
               (hist:save-history '(:volume 20 :bass 2) path)
               (is (= 20 (getf (hist:load-history path) :volume))))
          (ignore-errors (delete-file path))
          (ignore-errors (uiop:delete-empty-directory directory)))))

(test history-restores-a-speaker
      "A speaker restored from history has its levels and input only."
      (let ((speaker (hist:restore '(:volume 70 :bass 8 :input :input-usb
                                        :result :connected))))
        (is (= 70 (state:speaker-volume speaker)))
        (is (= 8 (state:speaker-bass speaker)))
        (is (eq :input-usb (state:speaker-input-source speaker)))
        (is (eq :disconnected (state:speaker-status speaker)))))

(test history-snapshot-keeps-the-result-between-steps
      "Only an end state of the session changes the connection result."
      (let* ((previous '(:volume 50 :bass 5 :input nil
                         :result :failed :detail "Not found"))
             (steps (hist:snapshot
                     (state:make-speaker :status :handshaking) previous))
             (done (hist:snapshot
                    (state:make-speaker :status :connected) previous)))
        (is (eq :failed (getf steps :result)))
        (is (string= "Not found" (getf steps :detail)))
        (is (eq :connected (getf done :result)))
        (is (null (getf done :detail)))))

(test history-track-saves-changes
      "Tracking a store saves the history after each change."
      (with-temp-file (path "history.sexp")
        (let ((store (state:make-store)))
          (hist:track store :path path)
          (state:update store (z407:make z407:*z407* 'z407:volume-up-confirmed))
          (is (= 55 (getf (hist:load-history path) :volume)))
          (state:update-status store :failed "Not found")
          (let ((history (hist:load-history path)))
            (is (eq :failed (getf history :result)))
            (is (string= "Not found" (getf history :detail)))))))

(test history-track-skips-changes-that-leave-it-alone
      "A change that leaves the history the same writes nothing."
      (with-temp-file (path "history.sexp")
        (let ((store (state:make-store)))
          (hist:track store :path path)
          (state:update-status store :connected)
          (delete-file path)
          (state:update-status store :connecting)
          (is (not (probe-file path))))))

(test history-track-stops-on-unsubscribe
      "An unsubscribed tracker no longer saves."
      (with-temp-file (path "history.sexp")
        (let* ((store (state:make-store))
               (id (hist:track store :path path)))
          (state:unsubscribe store id)
          (state:update store (z407:make z407:*z407* 'z407:volume-up-confirmed))
          (is (not (probe-file path))))))
