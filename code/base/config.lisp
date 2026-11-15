(defpackage #:lpz407/base/config
  (:use #:cl)
  (:export #:*schema*
           #:config-file
           #:load-data
           #:load-config
           #:config-ref)
  (:documentation "Reads config.sexp as data, checked against a schema. The
same reader checks history.sexp against a schema of its own."))

(in-package #:lpz407/base/config)


;;; Code:

(defparameter *schema*
  '(:theme    (:background (:type :color :default "#2e3440")
               :surface    (:type :color :default "#3b4252")
               :foreground (:type :color :default "#eceff4")
               :accent     (:type :color :default "#88c0d0")
               :error      (:type :color :default "#bf616a")
               :border     (:type :color :default "#81a1c1")
               :bluetooth  (:type :color :default "#88c0d0")
               :aux        (:type :color :default "#a3be8c")
               :usb        (:type :color :default "#b48ead")
               :disconnect (:type :color :default "#d08770")
               :close      (:type :color :default "#bf616a")
               :connect    (:type :color :default "#8fbcbb")
               :connect-text (:type :color :default "#2e3440")
               :info       (:type :color :default "#5e81ac")
               :active     (:type :color :default "#d8dee9")
               :value      (:type :color :default "#81a1c1")
               :status-connected    (:type :color :default "#a3be8c")
               :status-connecting   (:type :color :default "#ebcb8b")
               :status-disconnected (:type :color :default "#d08770")
               :alpha      (:type :number :min 0 :max 1 :default 0.8)
               :header-shade (:type :number :min 0 :max 1 :default 0.4)
               :info-size  (:type :number :min 0.5 :max 2 :default 0.9)
               :level      (:type :color :default "#4c566a")
               :font       (:type :string :default "JetBrains Mono")
               :css        (:type :path :default nil)
               :logo       (:type :path :default nil)
               :scheme     (:type :enum
                            :values (:dark :light)
                            :default :dark))
    :window   (:width  (:type :integer :min 200 :max 2000 :default 360)
               :height (:type :integer :min 100 :max 2000 :default 320))
    :controls (:logo       (:type :boolean :default t)
               :info       (:type :boolean :default t)
               :connection (:type :boolean :default t)
               :volume     (:type :boolean :default t)
               :bass       (:type :boolean :default t)
               :inputs     (:type :boolean :default t))
    :labels   (:device     (:type :string :default "Logitech Z407")
               :status     (:type :string :default "status")
               :established (:type :string :default "established")
               :connect    (:type :string :default "Connect")
               :disconnect (:type :string :default "Disconnect")
               :volume     (:type :string :default "Volume")
               :bass       (:type :string :default "Bass")
               :bluetooth  (:type :string :default "Bluetooth")
               :aux        (:type :string :default "AUX")
               :usb        (:type :string :default "USB")))
  "The allowed settings, as a tree shaped like the config file. A leaf is
a plist with :type and :default, plus :values for :enum and optional :min
and :max for :number and :integer. No group may be named :type. The speaker
decides how far one volume or bass press moves, so no setting changes that.")

(defun config-file (name)
  "Return the pathname of the file NAME, a string, in the lpz407 directory
of the XDG config home, usually ~/.config/lpz407/."
  (merge-pathnames name (uiop:xdg-config-home "lpz407/")))

(defun leaf-p (node)
  "True if NODE, a schema node, describes one setting rather than a group."
  (and (consp node) (eq (first node) :type)))

(defun plist-p (object)
  "True if OBJECT is a proper list of keyword and value pairs."
  (and (listp object)
       (null (cdr (last object)))
       (evenp (length object))
       (loop :for key :in object :by #'cddr
             :always (keywordp key))))

(defun warn-config (path control &rest args)
  "Warn about a config problem. PATH is the list of keys leading to it,
or nil for the file as a whole; CONTROL and ARGS are a format control and
its arguments."
  (warn "config~@[ ~{~(~S~)~^ ~}~]: ~?" path control args))

(defun valid-value-p (leaf value)
  "True if VALUE is acceptable for LEAF, a leaf of the schema."
  (let ((min (getf leaf :min))
        (max (getf leaf :max))
        (type (getf leaf :type)))
    (ecase type
      ((:string :path :color) (stringp value))
      (:boolean (member value '(t nil)))
      ((:integer :number)
       (and (if (eq type :integer) (integerp value) (realp value))
            (or (null min) (>= value min))
            (or (null max) (<= value max))))
      (:enum (member value (getf leaf :values)))
      (:command (and (consp value)
                     (null (cdr (last value)))
                     (every #'stringp value))))))

(defun read-config-file (path)
  "Read the first form of the file at PATH as data and return it. Return
nil, after a warning, if the file cannot be read. A missing file is not a
problem and gives nil without a warning. Nothing is evaluated."
  (when (probe-file path)
    (handler-case
        (with-open-file (in path :external-format :utf-8)
          (let ((*read-eval* nil)
                (*package* (find-package '#:cl-user))
                (eof (list nil)))
            (let ((form (read in nil nil)))
              (unless (eq (read in nil eof) eof)
                (warn-config nil "ignoring forms after the first in ~A" path))
              form)))
      (error (condition)
        (warn-config nil "cannot read ~A: ~A" path condition)
        nil))))

(defun warn-unknown-keys (schema user path)
  "Warn about each key of the plist USER that SCHEMA, a group, lacks."
  (let ((known (loop :for (key) :on schema :by #'cddr :collect key)))
    (loop :for key :in user :by #'cddr
          :unless (member key known)
          :do (warn-config (append path (list key))
                           "unknown key, ignored"))))

(defun resolve-leaf (leaf value present-p path)
  "Return the value to use for LEAF. PRESENT-P says whether the user gave
VALUE. A missing or invalid value gives the default of LEAF."
  (let ((default (copy-tree (getf leaf :default))))
    (cond ((not present-p) default)
          ((valid-value-p leaf value) value)
          (t (warn-config path "invalid ~(~A~) value ~S; using ~S"
                          (getf leaf :type) value default)
             default))))

(defun walk-config (schema user path)
  "Return the plist for SCHEMA, a group, filled in from USER, the plist the
user wrote for it. PATH is the list of keys leading to the group, for
warnings. Missing and invalid values take defaults; unknown keys warn."
  (unless (plist-p user)
    (warn-config path "expected key and value pairs, got ~S; using defaults"
                 user)
    (setf user nil))
  (warn-unknown-keys schema user path)
  (loop :for (key child) :on schema :by #'cddr
        :for sub-path := (append path (list key))
        :append (multiple-value-bind (found value tail)
                    (get-properties user (list key))
                  (declare (ignore found))
                  (list key
                        (if (leaf-p child)
                            (resolve-leaf child value (and tail t) sub-path)
                            (walk-config child (and tail value) sub-path))))))

(defun load-data (path schema)
  "Read the file at PATH and return a plist shaped like SCHEMA. A missing
file gives the defaults; a bad value or unknown key only warns."
  (walk-config schema (read-config-file path) nil))

(defun load-config (&optional (path (config-file "config.sexp")))
  "Return the full config, a plist shaped like *SCHEMA*, from the file at
PATH. The defaults fill in whatever the file leaves out."
  (load-data path *schema*))

(defun config-ref (config &rest keys)
  "Return the value in CONFIG at the key path KEYS, for example
(config-ref config :theme :alpha). Signal an error for a key the schema
lacks, which is a typo in the code."
  (loop :for key :in keys
        :do (multiple-value-bind (found value tail)
                (get-properties config (list key))
              (declare (ignore found))
              (unless tail
                (error "No config key ~S in path ~S" key keys))
              (setf config value)))
  config)
