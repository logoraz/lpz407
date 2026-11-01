;; Guix Development Package Manifest for lpz407
;;
;; Run:
;;
;; guix shell -m manifest.scm
;; export LD_LIBRARY_PATH="$GUIX_ENVIRONMENT/lib:$LD_LIBRARY_PATH"
;; sbcl
;;

(use-modules (gnu packages))

(specifications->manifest
  (list
    "sbcl"                   ; Common Lisp implementation

    ;; CFFI / FFI substrate
    "libffi"                 ; runtime: CFFI's foreign-call backend
    "libfixposix"            ; build+runtime: iolib's POSIX bindings
                             ;   (transitive dep, pulled in by dbus)
    "pkg-config"             ; build: cffi-grovel asks it for libfixposix flags
    "gcc-toolchain"          ; build: cffi-grovel compiles C helper programs

    ;; GTK4 UI stack
    "gtk"                    ; runtime: GTK4 C library
    "glib"
    "gobject-introspection"
    "gtk4-layer-shell"        ; runtime: Wayland layer-shell protocol support
    "libadwaita"

    ;; Bluetooth / media-key subprocess tools
    "bluez"                  ; runtime: BlueZ daemon + D-Bus interfaces
    "xdotool"))              ; runtime: PC media-key simulation
