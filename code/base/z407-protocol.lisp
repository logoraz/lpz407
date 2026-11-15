(defpackage #:lpz407/base/z407-protocol
  (:use #:cl)
  (:import-from #:lpz407/lib/gatt
                #:define-protocol
                #:make
                #:encode
                #:decode-signal
                #:decode-uuid
                #:hex->octets
                #:octets->hex
                #:gatt-command
                #:gatt-command-p
                #:gatt-command-tag
                #:gatt-command-label
                #:gatt-signal
                #:gatt-signal-p
                #:gatt-signal-tag
                #:gatt-signal-label
                #:gatt-uuid
                #:gatt-uuid-p
                #:gatt-uuid-tag
                #:gatt-uuid-label)
  ;; Types
  (:export #:gatt-command #:gatt-command-p #:gatt-command-tag
           #:gatt-command-label #:gatt-signal #:gatt-signal-p
           #:gatt-signal-tag #:gatt-signal-label #:gatt-uuid #:gatt-uuid-p
           #:gatt-uuid-tag #:gatt-uuid-label)
  ;; Encoding and Decoding
  (:export #:make #:encode #:decode-signal #:decode-uuid #:hex->octets
           #:octets->hex)
  ;; Protocol
  (:export #:*z407*)
  ;; UUIDs
  (:export #:service-id #:command-id #:response-id)
  ;; Commands
  (:export #:bass-up #:bass-down #:volume-up #:volume-down #:play-pause
           #:next-track #:previous-track #:input-bt #:input-aux #:input-usb
           #:bluetooth-pair #:factory-reset #:handshake-acknowledge
           #:handshake-initiate #:ghost #:disconnect-chime #:switch-chime
           #:connect-chime)
  ;; Signals
  (:export #:handshake-initiate-response #:handshake-acknowledge-response
           #:handshake-connected #:status-unknown #:status-bt-active
           #:status-aux-active #:status-usb-active #:bass-up-confirmed
           #:bass-down-confirmed
           #:volume-up-confirmed #:volume-down-confirmed
           #:play-pause-confirmed #:next-track-confirmed
           #:previous-track-confirmed #:input-bt-confirmed
           #:input-aux-confirmed #:input-usb-confirmed #:bt-pair-confirmed
           #:factory-reset-confirmed #:connect-chime-confirmed
           #:switch-chime-confirmed #:disconnect-chime-confirmed
           #:ghost-confirmed)
  (:documentation "Logitech Z407 Protocol."))

(in-package #:lpz407/base/z407-protocol)


;;; Code:

(define-protocol *z407*
  "Logitech Z407 GATT protocol: UUIDs, commands and signals."
  ;; Connection Establishment
  (service-id
   :uuid "0000fdc2-0000-1000-8000-00805f9b34fb"
   :label "Service"
   "When the speaker is not already connected to a remote it periodically
sends out BLE advertisements. The advertisements advertise a service with
this UUID.")

  (command-id
   :uuid "c2e758b9-0e78-41e0-b0cb-98a593193fc5"
   :label "Command characteristic"
   "The GATT characteristic (command characteristic) that the handshake
and all later commands are written to.")

  (response-id
   :uuid "b84ac9c6-29c5-46d4-bba1-9d534784330f"
   :label "Response characteristic"
   "The GATT characteristic (response characteristic) that the speaker's
responses are written to.")

  ;; Commands

  (bass-up
   :command "8000"
   :label "Bass Up"
   "Increases the bass level.")

  (bass-down
   :command "8001"
   :label "Bass Down"
   "Decreases the bass level.")

  (volume-up
   :command "8002"
   :label "Volume Up"
   "Increases the master volume.")

  (volume-down
   :command "8003"
   :label "Volume Down"
   "Decreases the master volume.")

  (play-pause
   :command "8004"
   :label "Play/Pause"
   "Toggles play/pause for the Bluetooth source.")

  (next-track
   :command "8005"
   :label "Next Track"
   "Skips to the next track.")

  (previous-track
   :command "8006"
   :label "Previous Track"
   "Goes to the previous track.")

  (input-bt
   :command "8101"
   :label "Switch to Bluetooth input"
   "Switches the active input to Bluetooth.")

  (input-aux
   :command "8102"
   :label "Switch to AUX input"
   "Switches the active input to 3.5mm AUX.")

  (input-usb
   :command "8103"
   :label "Switch to USB input"
   "Switches the active input to USB.")

  (bluetooth-pair
   :command "8200"
   :label "Enable Bluetooth pairing"
   "Puts the speaker into Bluetooth pairing mode.")

  (factory-reset
   :command "8300"
   :label "Factory Reset"
   "Resets the speaker to factory defaults.")

  (handshake-acknowledge
   :command "8400"
   :label "Pairing Acknowledge"
   "Second step of connection handshake.")

  (handshake-initiate
   :command "8405"
   :label "Pairing Initiate"
   "First step of connection handshake.")

  (ghost
   :command "8500"
   :label "Unknown 1"
   "Unknown 1. Has no noticeable effect.")

  (disconnect-chime
   :command "8501"
   :label "Sound 1"
   "Plays failure/disconnect chime.")

  (switch-chime
   :command "8502"
   :label "Sound 2"
   "Plays mode switch chime.")

  (connect-chime
   :command "8503"
   :label "Sound 3"
   "Plays connection chime.")


  ;; Signals

  (handshake-initiate-response
   :signal "d40501"
   :label "Initiate Response"
   "Sent in response to 0x8405 (INITIATE).")

  (handshake-acknowledge-response
   :signal "d40001"
   :label "Acknowledge Response"
   "Sent in response to 0x8400 (ACKNOWLEDGE).")

  (handshake-connected
   :signal "d40003"
   :label "Connected"
   "Sent after 0xd40001 to signal connection is
fully established.")

  (status-bt-active
   :signal "cf04"
   :label "Switched to Bluetooth (complete)"
   "Switched to Bluetooth (complete). Sent only if the source
actually changes.")

  (status-aux-active
   :signal "cf05"
   :label "Switched to AUX (complete)"
   "Switched to AUX (complete). Sent only if the source
actually changes.")

  (status-usb-active
   :signal "cf06"
   :label "Switched to USB (complete)"
   "Switched to USB (complete). Sent only if the source
actually changes.")

  (status-unknown
   :signal "cf0b"
   :label "Unknown 2"
   "Not in the spec. Sent once during the handshake, after the initiate
response. Meaning unknown.")

  (bass-up-confirmed
   :signal "c000"
   :label "Bass Up"
   "Confirming command: Bass Up.")

  (bass-down-confirmed
   :signal "c001"
   :label "Bass Down"
   "Confirming command: Bass Down.")

  (volume-up-confirmed
   :signal "c002"
   :label "Volume Up"
   "Confirming command: Volume Up.")

  (volume-down-confirmed
   :signal "c003"
   :label "Volume Down"
   "Confirming command: Volume Down.")

  (play-pause-confirmed
   :signal "c004"
   :label "Play/Pause"
   "Confirming command: Play/Pause.")

  (next-track-confirmed
   :signal "c005"
   :label "Next Track"
   "Confirming command: Next Track.")

  (previous-track-confirmed
   :signal "c006"
   :label "Previous Track"
   "Confirming command: Previous Track.")

  (input-bt-confirmed
   :signal "c101"
   :label "Switch Bluetooth"
   "Confirming command: Switch Bluetooth.")

  (input-aux-confirmed
   :signal "c102"
   :label "Switch AUX"
   "Confirming command: Switch AUX.")

  (input-usb-confirmed
   :signal "c103"
   :label "Switch USB"
   "Confirming command: Switch USB.")

  (bt-pair-confirmed
   :signal "c200"
   :label "Pairing"
   "Confirming command: Pairing.")

  (factory-reset-confirmed
   :signal "c300"
   :label "Factory Reset"
   "Confirming command: Factory Reset.")

  (connect-chime-confirmed
   :signal "c503"
   :label "Sound 3"
   "Confirming command: Sound 3.")

  (switch-chime-confirmed
   :signal "c502"
   :label "Sound 2"
   "Confirming command: Sound 2.")

  (disconnect-chime-confirmed
   :signal "c501"
   :label "Sound 1"
   "Confirming command: Sound 1.")

  (ghost-confirmed
   :signal "c500"
   :label "Unknown 1"
   "Confirming command: Unknown 1."))
