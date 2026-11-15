(defsystem "lpz407"
  :description "Native GTK4 control app for the Logitech Z407"
  :author "Erik P. Almaraz"
  :license "MIT"
  :version (:read-file-form "data/version.sexp" :at (0 1))
  :depends-on ("sb-concurrency"
               "dbus"
               "iolib"
               "cl-gtk4"
               "cl-gdk4"
               "cl-gtk4.adw")
  :components ((:module "lib"
                :pathname "code/lib"
                :components ((:file "gatt")))
               (:module "base"
                :pathname "code/base"
                :depends-on ("lib")
                :components ((:file "z407-protocol")
                             (:file "bluez" :depends-on ("z407-protocol"))
                             (:file "app-state" :depends-on ("z407-protocol"))
                             (:file "config")
                             (:file "history" :depends-on ("app-state"
                                                           "config"))))
               (:module "ui"
                :pathname "code/ui"
                :depends-on ("base")
                :components ((:file "view")
                             (:file "theme")
                             (:file "components" :depends-on ("view"))
                             (:file "window" :depends-on ("view" "theme"
                                                          "components"))
                             (:file "application" :depends-on ("window"))))
               (:module "application"
                :pathname "code"
                :depends-on ("base" "ui")
                :components ((:file "lpz407"))))
  :in-order-to ((test-op (test-op "lpz407/tests")))
  :build-operation "program-op"
  :build-pathname "lpz407"
  :entry-point "lpz407:main")


;;; Subsystems

(defsystem "lpz407/tests"
  :description "Unit tests"
  :depends-on ("lpz407" "fiveam")
  :components
  ((:module "tests"
    :serial t
    :components ((:file "suite")
                 (:file "lib")
                 (:file "base")
                 (:file "ui")
                 (:file "application"))))
  :perform (test-op (o c) (symbol-call :fiveam :run!
                                       (find-symbol "SUITE"
                                                    :lpz407/tests/suite))))
