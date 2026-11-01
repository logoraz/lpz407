;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407/ui/window
  (:use #:cl)
  (:local-nicknames (#:gtk #:gtk4)
                    (#:config #:lpz407/base/config)
                    (#:state #:lpz407/base/app-state)
                    (#:z407 #:lpz407/base/z407-protocol)
                    (#:view #:lpz407/ui/view)
                    (#:theme #:lpz407/ui/theme)
                    (#:components #:lpz407/ui/components))
  (:export #:make-window)
  (:documentation "The main window: a header bar with a close button above
the components that the config asks for."))

(in-package #:lpz407/ui/window)


;;; Code:

(defun make-components (config connect disconnect command)
  "Return the components that the :controls settings of CONFIG turn on, in
the order they show. CONNECT and DISCONNECT are functions of no arguments
and COMMAND is a function of a command name."
  (let ((texts (config:config-ref config :labels))
        (controls (config:config-ref config :controls))
        (components '()))
    (flet ((add (key class &rest initargs)
             (when (getf controls key)
               (push (apply #'make-instance class initargs)
                     components))))
      (add :logo 'components:logo
           :path (config:config-ref config :theme :logo))
      (add :info 'components:device-info :texts texts)
      (add :connection 'components:connection-row
           :texts texts :connect connect :disconnect disconnect)
      (add :volume 'components:level-control
           :key :volume :title (getf texts :volume) :command command
           :down 'z407:volume-down :up 'z407:volume-up)
      (add :bass 'components:level-control
           :key :bass :title (getf texts :bass) :command command
           :down 'z407:bass-down :up 'z407:bass-up)
      (add :inputs 'components:input-selector
           :texts texts :command command))
    (nreverse components)))

(defun make-header-bar ()
  "Return the header bar of the window. It has the close button."
  (let ((bar (adw:make-header-bar)))
    (setf (adw:header-bar-title-widget bar)
          (adw:make-window-title :title "lpz407" :subtitle "Logitech Z407"))
    bar))

(defun make-body (components)
  "Return a box that holds the widgets of COMPONENTS in order, with the rows
together in one list where the first of them stands. The box is limited in
width and kept at the top, so a window that a tiling manager makes large
still looks right."
  (let ((body (gtk:make-box :orientation gtk:+orientation-vertical+
                            :spacing 18))
        (group (adw:make-preferences-group))
        (clamp (adw:make-clamp))
        (group-added nil))
    (setf (gtk:widget-margin-all body) 18
          (gtk:widget-valign body) gtk:+align-start+
          (adw:clamp-maximum-size clamp) 460
          (adw:clamp-child clamp) body)
    (dolist (component components)
      (let ((widget (components:component-widget component)))
        (cond ((not (components:in-group-p component))
               (gtk:box-append body widget))
              (t (unless group-added
                   (gtk:box-append body group)
                   (setf group-added t))
                 (adw:preferences-group-add group widget)))))
    clamp))

(defun make-window (application store config connect disconnect command)
  "Return the main window of APPLICATION. It shows the speaker in STORE as
CONFIG says. CONNECT and DISCONNECT are functions of no arguments for the
connection button and COMMAND is a function of a command name for the
others. All three run on the GTK thread."
  (let* ((window (adw:make-application-window :app application))
         (box (gtk:make-box :orientation gtk:+orientation-vertical+
                            :spacing 0))
         (components (make-components config connect disconnect command)))
    (flet ((show (speaker)
             (let ((view (view:speaker-view speaker)))
               (dolist (component components)
                 (components:refresh component view)))))
      (gtk:box-append box (make-header-bar))
      (gtk:box-append box (make-body components))
      (setf (adw:window-content window) box
            (gtk:widget-size-request window)
            (list (config:config-ref config :window :width)
                  (config:config-ref config :window :height)))
      (gtk:widget-add-css-class window "lpz407")
      (theme:apply-theme (config:config-ref config :theme) window)
      (show (state:current store))
      (let ((id (state:subscribe store
                                 (lambda (speaker)
                                   (gtk:run-in-main-event-loop ()
                                     (show speaker))))))
        (gtk:connect window "destroy"
                     (lambda (window)
                       (declare (ignore window))
                       (state:unsubscribe store id)))))
    window))
