;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407/lib/gatt
  (:use #:cl)
  (:export #:define-protocol
           #:encode
           #:decode-signal
           #:decode-uuid
           #:hex->octets
           #:octets->hex
           #:make
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
  (:documentation "GATT UUIDs, commands and signals for the Z407 protocol."))

(in-package #:lpz407/lib/gatt)


;;; Hex and Octets

(defun hex->octets (hex)
  "Return HEX, a string of hex digit pairs such as \"8002\", as a vector
of octets, here #(128 2). Signal an error if HEX has an odd length or a
character that is not a hex digit."
  (unless (and (evenp (length hex))
               (every (lambda (char) (digit-char-p char 16)) hex))
    (error "Invalid hex string ~S; expected pairs of hex digits" hex))
  (let ((octets (make-array (floor (length hex) 2)
                            :element-type '(unsigned-byte 8))))
    (loop :for i :from 0 :below (length octets)
          :do (setf (aref octets i)
                    (parse-integer hex :start (* 2 i)
                                   :end (+ 2 (* 2 i))
                                   :radix 16)))
    octets))

(defun octets->hex (octets)
  "Return OCTETS as a lower case hex string, here #(128 2) gives \"8002\".
This reverses HEX->OCTETS."
  (format nil "~(~{~2,'0x~}~)" (coerce octets 'list)))

(defun valid-uuid-p (string)
  "Return true if STRING is a UUID written as 8-4-4-4-12 hex digits,
such as \"0000fdc2-0000-1000-8000-00805f9b34fb\"."
  (and (stringp string)
       (= (length string) 36)
       (loop :for char :across string
             :for i :from 0
             :always (if (member i '(8 13 18 23))
                         (char= char #\-)
                         (digit-char-p char 16)))))


;;; Types

(defstruct (gatt-command (:copier nil))
  "Base type of every :COMMAND entry, a message the app sends.
TAG is the lower case hex tag and LABEL the spec's name for the command."
  (tag nil)
  (label nil))

(defstruct (gatt-signal (:copier nil))
  "Base type of every :SIGNAL entry, a message the speaker sends.
TAG is the lower case hex tag and LABEL the spec's name for the signal."
  (tag nil)
  (label nil))

(defstruct (gatt-uuid (:copier nil))
  "Base type of every :UUID entry, a GATT service or characteristic.
TAG is the lower case UUID string and LABEL a short name for it."
  (tag nil)
  (label nil))

(defstruct (protocol (:copier nil))
  "The entries of one protocol, as built by DEFINE-PROTOCOL.
CONSTRUCTORS maps an entry's name to its constructor. SIGNALS and UUIDS
map a lower case tag to the name of the entry that has it."
  (name nil)
  (constructors (make-hash-table :test 'eq))
  (signals (make-hash-table :test 'equal))
  (uuids (make-hash-table :test 'equal)))


;;; Encoding and Decoding

(defun make (protocol name)
  "Return a new instance of NAME, the name of an entry that DEFINE-PROTOCOL
defined for PROTOCOL. Signal an error if PROTOCOL has no such entry."
  (let ((constructor (gethash name (protocol-constructors protocol))))
    (if constructor
        (funcall constructor)
        (error "~S is not an entry of ~S" name (protocol-name protocol)))))

(defgeneric encode (message)
  (:documentation "Return the wire form of MESSAGE, a struct defined by
DEFINE-PROTOCOL. A command gives a vector of octets and a UUID gives its
string. A signal has no wire form to send, so it signals an error."))

(defmethod encode ((message gatt-command))
  "Return the tag of the command MESSAGE as a vector of octets."
  (let ((tag (gatt-command-tag message)))
    (unless tag
      (error "~S has no tag; make it with MAKE" message))
    (hex->octets tag)))

(defmethod encode ((message gatt-uuid))
  "Return the UUID of MESSAGE as a lower case string."
  (or (gatt-uuid-tag message)
      (error "~S has no tag; make it with MAKE" message)))

(defun decode-signal (protocol octets)
  "Return the signal that OCTETS, a vector of octets from the speaker,
describes in PROTOCOL. The tag picks the entry. Signal an error naming the
tag if PROTOCOL has no such signal."
  (let* ((hex (octets->hex octets))
         (name (gethash hex (protocol-signals protocol))))
    (if name
        (make protocol name)
        (error "Unknown signal in ~S: ~A" (protocol-name protocol) hex))))

(defun decode-uuid (protocol uuid)
  "Return the entry of PROTOCOL for UUID, a string such as BlueZ reports,
or NIL if PROTOCOL has none. NIL is not an error, because BlueZ lists many
UUIDs that the Z407 protocol does not define. Ignore case."
  (let ((name (gethash (string-downcase uuid) (protocol-uuids protocol))))
    (when name
      (make protocol name))))


;;; Code

(defun parse-gatt-args (name args)
  "Split ARGS, the arguments of an entry for NAME, into four values: the
kind (:UUID, :COMMAND or :SIGNAL), its value string, the label string or
NIL, and the docstring or NIL. ARGS is a plist with exactly one of the
three kind keys and an optional :LABEL string, optionally followed by a
docstring. A :UUID value must be a UUID string, and a :COMMAND or :SIGNAL
value a hex string. Signal an error for anything else."
  (let* ((kinds '(:uuid :command :signal))
         (doc (when (oddp (length args))
                (car (last args))))
         (options (if doc (butlast args) args))
         (given (remove-if-not (lambda (kind) (getf options kind)) kinds))
         (label (getf options :label)))
    (when (and doc (not (stringp doc)))
      (error "DEFINE-PROTOCOL ~S: trailing ~S is not a docstring" name doc))
    (loop :for (key) :on options :by #'cddr
          :unless (member key (cons :label kinds))
          :do (error "DEFINE-PROTOCOL ~S: unknown key ~S" name key))
    (unless (= 1 (length given))
      (error "DEFINE-PROTOCOL ~S: give exactly one of ~S" name kinds))
    (when (and label (not (stringp label)))
      (error "DEFINE-PROTOCOL ~S: label ~S is not a string" name label))
    (let* ((kind (first given))
           (value (getf options kind)))
      (if (eq kind :uuid)
          (unless (valid-uuid-p value)
            (error "DEFINE-PROTOCOL ~S: ~S is not a UUID" name value))
          (hex->octets value))          ; Validate the tag at expansion time
      (values kind value label doc))))

(defun parse-entry (entry)
  "Return the list (NAME KIND KEY LABEL DOC) for ENTRY, a list (NAME . ARGS)
from a DEFINE-PROTOCOL form. KEY is the tag in lower case. Signal an error
if ENTRY is malformed."
  (unless (and (consp entry) (symbolp (first entry)) (first entry))
    (error "DEFINE-PROTOCOL: ~S is not an entry (NAME . ARGS)" entry))
  (multiple-value-bind (kind value label doc)
      (parse-gatt-args (first entry) (rest entry))
    (list (first entry) kind (string-downcase value) label doc)))

(defun check-unique (protocol parsed)
  "Signal an error if two entries in PARSED, a list of PARSE-ENTRY results
for PROTOCOL, share a name or share a tag of the same kind."
  (let ((names '())
        (tags '()))
    (loop :for (name kind key) :in parsed
          :for tag := (cons kind key)
          :do (when (member name names)
                (error "DEFINE-PROTOCOL ~S: ~S is defined twice"
                       protocol name))
              (when (member tag tags :test #'equal)
                (error "DEFINE-PROTOCOL ~S: ~S reuses the ~(~A~) tag ~S"
                       protocol name kind key))
              (push name names)
              (push tag tags))))

(defun entry-forms (protocol parsed)
  "Return the list of forms that define one entry of the variable PROTOCOL.
PARSED is the PARSE-ENTRY result for the entry."
  (destructuring-bind (name kind key label doc) parsed
    (let ((make (intern (format nil "MAKE-~A" name)))
          (base (ecase kind
                  (:uuid 'gatt-uuid)
                  (:command 'gatt-command)
                  (:signal 'gatt-signal))))
      `((defstruct (,name (:include ,base (tag ,key) (label ,label))
                          (:copier nil))
          ,@(when doc (list doc)))
        (setf (gethash ',name (protocol-constructors ,protocol)) #',make)
        ,@(ecase kind
            (:command
             nil)
            (:signal
             `((setf (gethash ,key (protocol-signals ,protocol)) ',name)))
            (:uuid
             `((setf (gethash ,key (protocol-uuids ,protocol)) ',name))))))))

(defmacro define-protocol (name &rest forms)
  "Define the variable NAME as a protocol and one struct for each entry.
FORMS is an optional docstring for NAME, then the entries. An entry is a
list (ENTRY-NAME . ARGS), where ARGS is a plist with one of :UUID, :COMMAND
or :SIGNAL and an optional :LABEL, followed by an optional docstring.

  :uuid     A UUID string naming a GATT service or characteristic. The
            struct includes GATT-UUID; ENCODE returns the string, and
            DECODE-UUID builds the struct from it.
  :command  A hex tag for a message the app sends. The struct includes
            GATT-COMMAND; ENCODE returns the tag as a vector of octets.
  :signal   A hex tag for a message the speaker sends. The struct includes
            GATT-SIGNAL; DECODE-SIGNAL builds the struct from the tag.
  :label    The spec's name for the entry, a string.

Each struct stores its tag and label in its slots, and NAME holds the
lookups that MAKE, DECODE-SIGNAL and DECODE-UUID use. Signal an error at
expansion time if an entry is malformed, or if two entries share a name or
a tag of the same kind.

Example:
  (define-protocol *speaker*
    \"A speaker.\"
    (volume-up
      :command \"8002\"
      :label \"Volume Up\"
      \"Increases the master volume.\"))"
  (unless (and name (symbolp name))
    (error "DEFINE-PROTOCOL: ~S is not a variable name" name))
  (let* ((doc (when (stringp (first forms))
                (first forms)))
         (parsed (mapcar #'parse-entry (if doc (rest forms) forms))))
    (check-unique name parsed)
    `(progn
       (defparameter ,name (make-protocol :name ',name)
         ,@(when doc (list doc)))
       ,@(loop :for entry :in parsed
               :append (entry-forms name entry))
       ',name)))
