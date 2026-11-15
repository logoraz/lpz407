(defpackage #:lpz407/ui/application
  (:use #:cl)
  (:local-nicknames (#:gtk #:gtk4)
                    (#:gio #:gio)
                    (#:window #:lpz407/ui/window))
  (:export #:run)
  (:documentation "Starts libadwaita and runs the application until the
window closes."))

(in-package #:lpz407/ui/application)


;;; Code:

(defun run (store config &key connect disconnect command)
  "Show the main window for STORE, set up as CONFIG says, and run until it
closes. CONNECT and DISCONNECT are functions of no arguments, called by the
connection button. COMMAND is a function of the name of a command, called
by the other buttons. All three run on the GTK thread."
  (unless (adw:initialized-p)
    (adw:init))
  (let ((application (gtk:make-application
                      :application-id "org.lpz407.app"
                      :flags gio:+application-flags-flags-none+)))
    (gtk:connect application "activate"
                 (lambda (application)
                   (gtk:window-present
                    (window:make-window application store config
                                        connect disconnect command))))
    (gtk:application-run application nil)))
