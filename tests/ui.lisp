(defpackage #:lpz407/tests/ui
  (:use #:cl #:5am)
  (:import-from #:lpz407/tests/suite
                #:suite)
  (:local-nicknames (#:z407  #:lpz407/base/z407-protocol)
                    (#:state #:lpz407/base/app-state)
                    (#:conf  #:lpz407/base/config)
                    (#:view  #:lpz407/ui/view)
                    (#:theme #:lpz407/ui/theme))
  (:export #:ui-suite)
  (:documentation "Tests for the ui module."))

(in-package #:lpz407/tests/ui)


;;; Code:

(def-suite ui-suite
  :description "Tests for code/ui."
  :in suite)

(in-suite ui-suite)


;;; view

(test view-describes-a-disconnected-speaker
      "A new speaker shows as disconnected, offers Connect, has no controls."
      (let ((view (view:speaker-view (state:current (state:make-store)))))
        (is (string= "Disconnected" (getf view :status)))
        (is (eq :connect (getf view :action)))
        (is (eq :disconnected (getf view :state)))
        (is (not (getf view :controls)))
        (is (= 50 (getf view :volume)))
        (is (= 5 (getf view :bass)))
        (is (null (getf view :input)))))

(test view-follows-the-session
      "The status text, action and controls follow the session status."
      (let ((store (state:make-store)))
        (state:update-status store :handshaking)
        (let ((view (view:speaker-view (state:current store))))
          (is (string= "Connecting" (getf view :status)))
          (is (eq :connecting (getf view :state)))
          (is (eq :disconnect (getf view :action)))
          (is (not (getf view :controls))))
        (state:update-status store :connected)
        (let ((view (view:speaker-view (state:current store))))
          (is (string= "Connected" (getf view :status)))
          (is (eq :connected (getf view :state)))
          (is (eq :disconnect (getf view :action)))
          (is (getf view :controls)))
        (state:update-status store :failed "The Z407 was not found")
        (let ((view (view:speaker-view (state:current store))))
          (is (string= "Failed: The Z407 was not found" (getf view :status)))
          (is (eq :disconnected (getf view :state)))
          (is (eq :connect (getf view :action)))
          (is (not (getf view :controls))))
        (state:update-status store :failed)
        (is (string= "Failed"
                     (getf (view:speaker-view (state:current store))
                           :status)))))

(test view-shows-levels-and-input
      "The levels and the active input come from the speaker."
      (let ((store (state:make-store)))
        (state:update store (z407:make z407:*z407* 'z407:volume-up-confirmed))
        (state:update store (z407:make z407:*z407* 'z407:bass-down-confirmed))
        (state:update store (z407:make z407:*z407* 'z407:status-aux-active))
        (let ((view (view:speaker-view (state:current store))))
          (is (= 55 (getf view :volume)))
          (is (= 4 (getf view :bass)))
          (is (eq :input-aux (getf view :input))))))

;;; Local time, so the test builds its time from parts.
(test view-shows-when-the-session-was-established
      "The time of the connection shows as HH:MM, and only while connected."
      (let ((store (state:make-store))
            (state:*clock* (lambda ()
                             (encode-universal-time 0 7 14 31 10 2026))))
        (is (null (getf (view:speaker-view (state:current store))
                        :established)))
        (state:update-status store :connected)
        (is (string= "2:07 PM"
                     (getf (view:speaker-view (state:current store))
                           :established)))
        (state:update-status store :disconnected)
        (is (null (getf (view:speaker-view (state:current store))
                        :established)))))

;;; Midnight and noon are the odd hours of a 12-hour clock.
(test view-shows-midnight-and-noon-in-12-hour-time
      "Midnight is 12 AM and noon is 12 PM."
      (let ((store (state:make-store)))
        (dolist (case '((0 5 "12:05 AM") (12 30 "12:30 PM")
                        (23 59 "11:59 PM")))
          (destructuring-bind (hour minute text) case
            (let ((state:*clock* (lambda ()
                                   (encode-universal-time
                                    0 minute hour 31 10 2026))))
              (state:update-status store :disconnected)
              (state:update-status store :connected)
              (is (string= text (getf (view:speaker-view
                                       (state:current store))
                                      :established))))))))

(test view-escapes-markup
      "Text with markup characters shows as written."
      (is (string= "a &amp; b &lt;c&gt;" (view:escape-markup "a & b <c>")))
      (is (string= "plain" (view:escape-markup "plain"))))

;;; theme

(defun default-theme ()
  "Return the :theme plist of the config when no config file exists."
  (conf:config-ref
   (conf:load-config (merge-pathnames "lpz407-none.sexp"
                                      (uiop:temporary-directory)))
   :theme))

(test theme-parses-colors
      "Colors in the forms #rrggbb and #rgb give red, green and blue."
      (is (equal '(46 52 64) (multiple-value-list (theme:hex->rgb "#2e3440"))))
      (is (equal '(255 255 255) (multiple-value-list (theme:hex->rgb "#fff"))))
      (is (equal '(17 34 51) (multiple-value-list (theme:hex->rgb "#123"))))
      (is (null (theme:hex->rgb "red")))
      (is (null (theme:hex->rgb "#12345")))
      (is (null (theme:hex->rgb "#gg0000")))
      (is (null (theme:hex->rgb nil))))

(test theme-css-gives-the-window-its-alpha
      "The window and its cards take the alpha, the text does not."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "@define-color window_bg_color rgba(46, 52, 64, 0.80);"
                    css))
        (is (search "@define-color card_bg_color rgba(59, 66, 82, 0.80);" css))
        (is (search "@define-color window_fg_color rgba(236, 239, 244, 1.00);"
                    css))
        (is (search "@define-color accent_bg_color rgba(136, 192, 208, 1.00);"
                    css))
        (is (search "--window-bg-color: rgba(46, 52, 64, 0.80);" css))
        (is (search "--accent-bg-color: rgba(136, 192, 208, 1.00);" css))
        (is (search "window.lpz407 { background-color: rgba(46, 52, 64, 0.80);"
                    css))))

(test theme-css-sets-the-font
      "A font in the theme becomes a rule for the window."
      (let ((css (theme:theme-css (list* :font "Iosevka" (default-theme)))))
        (is (search "window.lpz407 { font-family: \"Iosevka\"; }" css))))

(test theme-css-skips-colors-that-are-not-colors
      "A color that cannot be read warns and leaves its line out."
      (let ((warnings 0)
            (css nil))
        (handler-bind ((warning (lambda (condition)
                                  (incf warnings)
                                  (muffle-warning condition))))
          (setf css (theme:theme-css (list* :accent "teal"
                                            (default-theme)))))
        (is (= 1 warnings))
        (is (not (search "accent_bg_color" css)))
        (is (search "window_bg_color" css))))

(test theme-shades-colors
      "A shade multiplies each channel, and leaves other strings alone."
      (is (string= "#4B678A" (theme:shade "#5e81ac" 0.8)))
      (is (string= "#000000" (theme:shade "#ffffff" 0)))
      (is (string= "red" (theme:shade "red" 0.8))))

(test theme-css-fills-the-inputs
      "Each input button is filled with its color; the active one has a ring."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "window.lpz407 button.lpz407-input-bt {
  background: #88c0d0;" css))
        (is (search "button.lpz407-input-bt:hover {
  background: #5F8692;
  color: #eceff4;" css))
        (is (search "button.lpz407-input-aux {
  background: #a3be8c;" css))
        (is (search "button.lpz407-input-bt:active {
  background: #364D53;
  color: #eceff4;" css))
        (is (search "button.lpz407-input-aux.lpz407-active {
  box-shadow: inset 0 0 0 2px #d8dee9;" css))))

(test theme-css-fills-the-connect-button
      "The connect button has its color and text, and shades to 70% and 40%."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "button.lpz407-connect {
  background: #8fbcbb;
  color: #2e3440;" css))
        (is (search "button.lpz407-connect:hover {
  background: #648483;
  color: #eceff4;" css))
        (is (search "button.lpz407-connect:active {
  background: #394B4B;
  color: #eceff4;" css))))

(test theme-css-colors-the-info-lines
      "The status and established lines have the info color."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "window.lpz407 .lpz407-info {
  color: #5e81ac;" css))))

(test theme-css-sizes-the-device-info
      "The device info has its size relative to the other text."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "window.lpz407 .lpz407-device {
  font-size: 0.90em;" css))))

(test theme-css-colors-the-level-numbers
      "The volume and bass numbers have the value color."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "window.lpz407 .lpz407-value {
  color: #81a1c1;" css))))

(test theme-css-colors-the-status
      "The status text is green, yellow or red by the state of the session."
      (let ((css (theme:theme-css (default-theme))))
        (is (search ".lpz407-status-connected .title {
  color: #a3be8c;" css))
        (is (search ".lpz407-status-connecting .title {
  color: #ebcb8b;" css))
        (is (search ".lpz407-status-disconnected .title {
  color: #d08770;" css))))

(test theme-css-draws-the-border-and-font
      "The card has the border color and the window has the font."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "border: 1px solid #81a1c1;" css))
        (is (search "font-family: \"JetBrains Mono\";" css))))

(test theme-css-darkens-the-header-bar-when-focused
      "The header bar is darker by the header shade, until it loses focus."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "--headerbar-bg-color: rgba(28, 31, 38, 0.80);" css))
        (is (search "--headerbar-backdrop-color: rgba(46, 52, 64, 0.80);"
                    css))
        (is (search "lpz407 headerbar { background-color: rgba(28, 31, 38"
                    css))
        (is (search "backdrop headerbar { background-color: rgba(46, 52, 64"
                    css))))

(test theme-lightens-colors
      "A color moves toward white by the amount, and others are left alone."
      (is (string= "#FFFFFF" (theme:lighten "#2e3440" 1)))
      (is (string= "#2E3440" (theme:lighten "#2e3440" 0)))
      (is (string= "red" (theme:lighten "red" 0.5))))

(test theme-css-colors-the-action-buttons
      "The disconnect button is filled, the close button shows color on hover,
and the level buttons are #4c566a and get darker on hover."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "button.lpz407-disconnect {
  background: #d08770;" css))
        (is (search "button.lpz407-disconnect:hover {
  background: #A66C5A;
  color: #eceff4;" css))
        (is (search "windowcontrols button.close:hover image {
  background: #bf616a;" css))
        (is (not (search "windowcontrols button.close {" css)))
        (is (search "button.lpz407-level {
  background: #4c566a;" css))
        (is (search "button.lpz407-level:hover {
  background: #3D4555;" css))
        (is (search "button.lpz407-level:active {
  background: #2E3440;" css))))

(test theme-css-fades-disabled-buttons-and-unframes-rows
      "Disabled buttons fade, and the rows get no focus outline."
      (let ((css (theme:theme-css (default-theme))))
        (is (search "window.lpz407 button:disabled {
  opacity: 0.4;" css))
        (is (search "window.lpz407 row:focus-visible {
  outline: none;" css))))

(test components-look-for-the-logo-without-asdf
      "The logo is sought beside the program, in its share directory and in
the source tree, in that order."
      (is (equal '("/opt/lpz407/bin/data/graphics/lisp-icon-sm.svg"
                   "/opt/lpz407/share/lpz407/graphics/lisp-icon-sm.svg"
                   "/src/lpz407/data/graphics/lisp-icon-sm.svg")
                 (mapcar #'namestring
                         (lpz407/ui/components::logo-candidates
                          #p"/opt/lpz407/bin/lpz407"
                          #p"/src/lpz407/code/ui/components.lisp")))))

(test components-skip-the-logo-places-they-do-not-know
      "Without a program or a source file, those places are left out."
      (is (null (lpz407/ui/components::logo-candidates nil nil)))
      (is (= 1 (length (lpz407/ui/components::logo-candidates
                        nil #p"/src/lpz407/code/ui/components.lisp"))))
      (is (= 2 (length (lpz407/ui/components::logo-candidates
                        #p"/opt/lpz407/bin/lpz407" nil)))))
