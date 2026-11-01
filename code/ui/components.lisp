;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407/ui/components
  (:use #:cl)
  (:local-nicknames (#:gtk #:gtk4)
                    (#:z407 #:lpz407/base/z407-protocol)
                    (#:view #:lpz407/ui/view))
  (:export #:component
           #:component-widget
           #:build
           #:refresh
           #:in-group-p
           #:connection-row
           #:device-info
           #:level-control
           #:input-selector
           #:logo)
  (:documentation "The parts of the window as CLOS classes. A component
builds one widget and refreshes it from the view of a speaker."))

(in-package #:lpz407/ui/components)


;;; Code:

;;; Protocol

(defclass component ()
  ((widget :reader component-widget
           :documentation "The widget that BUILD made."))
  (:documentation "A part of the window. Subclasses add a method to BUILD
and one to REFRESH; the widget is built when the component is made."))

(defgeneric build (component)
  (:documentation "Make and return the widget of COMPONENT. It is called
once, when COMPONENT is made."))

(defgeneric in-group-p (component)
  (:documentation "True if the widget of COMPONENT is a row of the list of
settings, and false if it stands on its own below the list.")
  (:method (component)
    (declare (ignore component))
    t))

(defgeneric refresh (component view)
  (:documentation "Show VIEW, a plist from SPEAKER-VIEW, in COMPONENT.")
  (:method (component view)
    "Leave a component that shows nothing of the speaker as it is."
    (declare (ignore component view))
    nil))

(defmethod initialize-instance :after ((component component) &key)
  (setf (slot-value component 'widget) (build component)))


;;; Helpers

(defun make-command-button (label name command)
  "Return a button with LABEL that calls COMMAND, a function of a command
name, with NAME when it is clicked."
  (let ((button (gtk:make-button :label label)))
    (gtk:connect button "clicked"
                 (lambda (button)
                   (declare (ignore button))
                   (funcall command name)))
    button))

(defun set-sensitive (widgets flag)
  "Make each of WIDGETS react to the user when FLAG is true, and not
otherwise."
  (dolist (widget widgets)
    (setf (gtk:widget-sensitive-p widget) (and flag t))))

(defun make-row (title)
  "Return an empty row of a list of settings, with the text TITLE."
  (let ((row (adw:make-action-row)))
    (setf (adw:preferences-row-title row) title)
    row))


;;; Connection

(defclass connection-row (component)
  ((texts :initarg :texts :reader texts
           :documentation "The labels plist of the config.")
   (connect :initarg :connect :reader connect
            :documentation "A function of no arguments, for connecting.")
   (disconnect :initarg :disconnect :reader disconnect
               :documentation "A function of no arguments, for ending.")
   (action :initform :connect :accessor action
           :documentation "What the button does now, :CONNECT or :DISCONNECT.")
   (row :accessor row)
   (button :accessor button))
  (:documentation "The status of the session and the button that starts or
ends it."))

(defmethod build ((component connection-row))
  (let ((row (make-row ""))
        (button (gtk:make-button :label "")))
    (setf (gtk:widget-valign button) gtk:+align-center+)
    (gtk:connect button "clicked"
                 (lambda (button)
                   (declare (ignore button))
                   (funcall (if (eq :disconnect (action component))
                                (disconnect component)
                                (connect component)))))
    (adw:action-row-add-suffix row button)
    (setf (row component) row
          (button component) button)
    row))

(defmethod refresh ((component connection-row) view)
  (dolist (state '(:connected :connecting :disconnected))
    (let ((class (format nil "lpz407-status-~(~A~)" state)))
      (if (eq state (getf view :state))
          (gtk:widget-add-css-class (row component) class)
          (gtk:widget-remove-css-class (row component) class))))
  (dolist (action '(:connect :disconnect))
    (let ((class (format nil "lpz407-~(~A~)" action)))
      (if (eq action (getf view :action))
          (gtk:widget-add-css-class (button component) class)
          (gtk:widget-remove-css-class (button component) class))))
  (setf (action component) (getf view :action)
        (adw:preferences-row-title (row component))
        (view:escape-markup (getf view :status))
        (gtk:button-label (button component))
        (getf (texts component) (getf view :action))))


;;; Levels

(defclass level-control (component)
  ((key :initarg :key :reader key
        :documentation "The level in the view, :VOLUME or :BASS.")
   (title :initarg :title :reader title
          :documentation "The text of the row.")
   (down :initarg :down :reader down
         :documentation "The name of the command that lowers the level.")
   (up :initarg :up :reader up
       :documentation "The name of the command that raises the level.")
   (command :initarg :command :reader command
            :documentation "A function of a command name.")
   (level :accessor level)
   (buttons :initform '() :accessor buttons))
  (:documentation "A row with a button that lowers a level, the level and a
button that raises it."))

(defmethod build ((component level-control))
  (let ((row (make-row (title component)))
        (level (gtk:make-label :str ""))
        (down (make-command-button "-" (down component)
                                   (command component)))
        (up (make-command-button "+" (up component) (command component))))
    (gtk:widget-add-css-class level "title-4")
    (gtk:widget-add-css-class level "lpz407-value")
    (dolist (button (list down up))
      (gtk:widget-add-css-class button "circular")
      (gtk:widget-add-css-class button "lpz407-level"))
    (setf (gtk:label-width-chars level) 4
          (level component) level
          (buttons component) (list down up))
    (dolist (widget (list down level up))
      (setf (gtk:widget-valign widget) gtk:+align-center+)
      (adw:action-row-add-suffix row widget))
    row))

(defmethod refresh ((component level-control) view)
  (setf (gtk:label-text (level component))
        (format nil "~D" (getf view (key component))))
  (set-sensitive (buttons component) (getf view :controls)))


;;; Inputs

(defparameter *inputs*
  '((:input-bt :bluetooth z407:input-bt)
    (:input-aux :aux z407:input-aux)
    (:input-usb :usb z407:input-usb))
  "The inputs of the speaker, each as the keyword of the view, the key of
its label and the name of the command that selects it.")

(defclass input-selector (component)
  ((texts :initarg :texts :reader texts
           :documentation "The labels plist of the config.")
   (command :initarg :command :reader command
            :documentation "A function of a command name.")
   (buttons :initform '() :accessor buttons
            :documentation "An alist of input keywords and their buttons."))
  (:documentation "A row of buttons, one for each input, with the active
input marked."))

(defmethod in-group-p ((component input-selector))
  nil)

(defmethod build ((component input-selector))
  (let ((box (gtk:make-box :orientation gtk:+orientation-horizontal+
                           :spacing 6)))
    (setf (gtk:widget-hexpand-p box) t)
    (loop :for (input key name) :in *inputs*
          :for button := (make-command-button (getf (texts component) key)
                                              name (command component))
          :do (setf (gtk:widget-hexpand-p button) t)
              (gtk:widget-add-css-class button "lpz407-input")
              (gtk:widget-add-css-class
               button (format nil "lpz407-~(~A~)" input))
              (gtk:box-append box button)
              (push (cons input button) (buttons component)))
    box))

(defmethod refresh ((component input-selector) view)
  (loop :for (input . button) :in (buttons component)
        :do (if (eq input (getf view :input))
                (gtk:widget-add-css-class button "lpz407-active")
                (gtk:widget-remove-css-class button "lpz407-active")))
  (set-sensitive (mapcar #'cdr (buttons component)) (getf view :controls)))


;;; Device

(defun info-lines (texts view)
  "Return the status line and the established line, two strings, for VIEW
with the labels TEXTS. The active input and the time show while the speaker
is connected, and a dash otherwise. The input shows as a dash too while it
is not known, which it is not until the speaker confirms a switch or reports
one."
  (let* ((connected (getf view :controls))
         (input (and connected (getf view :input)))
         (name (and input (getf texts (second (assoc input *inputs*))))))
    (list (format nil "~A: ~A" (getf texts :status) (or name "-"))
          (format nil "~A: ~A" (getf texts :established)
                  (or (and connected (getf view :established)) "-")))))

(defclass device-info (component)
  ((texts :initarg :texts :reader texts
          :documentation "The labels plist of the config.")
   (status :accessor status)
   (established :accessor established))
  (:documentation "The name of the speaker, with its status and the time
the connection was established below it, all in the same size of text, the
name in bold. The two lines below the name have the color of the :info key
of the theme."))

(defmethod in-group-p ((component device-info))
  nil)

(defmethod build ((component device-info))
  (let ((box (gtk:make-box :orientation gtk:+orientation-vertical+
                           :spacing 4))
        (name (gtk:make-label :str (getf (texts component) :device)))
        (status (gtk:make-label :str ""))
        (established (gtk:make-label :str "")))
    (gtk:widget-add-css-class name "heading")
    (dolist (widget (list name status established))
      (gtk:widget-add-css-class widget "lpz407-device"))
    (gtk:widget-add-css-class status "lpz407-info")
    (gtk:widget-add-css-class established "lpz407-info")
    (setf (status component) status
          (established component) established)
    (dolist (widget (list name status established))
      (setf (gtk:widget-halign widget) gtk:+align-center+)
      (gtk:box-append box widget))
    box))

(defmethod refresh ((component device-info) view)
  (destructuring-bind (status established)
      (info-lines (texts component) view)
    (setf (gtk:label-text (status component)) status
          (gtk:label-text (established component)) established)))


;;; Logo

(defmacro source-file ()
  "Return the pathname of the file being compiled, or loaded as source,
as a literal, so a program built from it remembers where the source tree
was."
  (or *compile-file-truename* *load-truename*))

(defparameter *source-file* (source-file)
  "The file this code came from when it was built, or NIL if not known.")

(defparameter *logo-file* "lisp-icon-sm.svg"
  "The file name of the logo.")

(defun parent-directory (pathname)
  "Return the directory above the directory of PATHNAME."
  (make-pathname :directory (butlast (pathname-directory pathname))
                 :name nil :type nil :version nil
                 :defaults pathname))

(defun logo-candidates (executable source)
  "Return the places to look for the default logo, best first: the data
directory next to EXECUTABLE, the share directory of the prefix it is
installed in, and the data directory of the source tree that SOURCE, the
file components.lisp, belongs to. Either may be NIL, which leaves out its
places. Nothing here asks ASDF, so a program that was built finds its logo
without a registry or a network."
  (flet ((in (directory subpath)
           (merge-pathnames subpath directory)))
    (append
     (when executable
       (let ((bin (make-pathname :name nil :type nil :version nil
                                 :defaults executable)))
         (list (in bin (format nil "data/graphics/~A" *logo-file*))
               (in (parent-directory bin)
                   (format nil "share/lpz407/graphics/~A" *logo-file*)))))
     (when source
       (list (in (parent-directory
                  (parent-directory
                   (make-pathname :name nil :type nil :version nil
                                  :defaults source)))
                 (format nil "data/graphics/~A" *logo-file*)))))))

(defun default-logo-path ()
  "Return the path of the first logo that exists among LOGO-CANDIDATES for
the running program and the source it was built from, or NIL."
  (find-if #'probe-file
           (logo-candidates (and sb-ext:*runtime-pathname*
                                 (parse-namestring
                                  sb-ext:*runtime-pathname*))
                            *source-file*)))

(defclass logo (component)
  ((path :initarg :path :initform nil :reader path
         :documentation "The SVG file to show, or NIL for the default.")
   (size :initarg :size :initform 96 :reader size
         :documentation "The width and height of the image in pixels."))
  (:documentation "The logo, an image above the list of settings."))

(defmethod in-group-p ((component logo))
  nil)

(defmethod build ((component logo))
  (let ((file (or (path component) (default-logo-path))))
    (if (and file (probe-file file))
        (let ((image (gtk:make-image)))
          (setf (gtk:image-from-file image) (namestring file)
                (gtk:image-pixel-size image) (size component)
                (gtk:widget-halign image) gtk:+align-center+)
          image)
        (progn (warn "logo: cannot find ~A; showing none" file)
               (gtk:make-box :orientation gtk:+orientation-vertical+
                             :spacing 0)))))
