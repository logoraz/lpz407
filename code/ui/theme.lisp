;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:lpz407/ui/theme
  (:use #:cl)
  (:local-nicknames (#:gtk #:gtk4))
  (:export #:hex->rgb
           #:shade
           #:lighten
           #:theme-css
           #:apply-theme)
  (:documentation "The look of the window: Nord colors and a transparent
background, as CSS made from the :theme settings of the config."))

(in-package #:lpz407/ui/theme)


;;; Code:

(defun hex->rgb (hex)
  "Return the red, green and blue of HEX, a string such as \"#2e3440\" or
\"#fff\", as three integers from 0 to 255. Return NIL for any other string."
  (let ((digits (and (stringp hex)
                     (plusp (length hex))
                     (char= #\# (char hex 0))
                     (subseq hex 1))))
    (when (and digits
               (member (length digits) '(3 6))
               (every (lambda (char) (digit-char-p char 16)) digits))
      (let ((size (/ (length digits) 3)))
        (values-list
         (loop :for start :from 0 :by size :repeat 3
               :collect (let ((value (parse-integer digits
                                                    :start start
                                                    :end (+ start size)
                                                    :radix 16)))
                          (if (= size 1) (* value 17) value))))))))

(defun color-css (hex alpha)
  "Return CSS for the color HEX at opacity ALPHA, a number from 0 to 1, or
NIL when HEX is not a color."
  (multiple-value-bind (red green blue) (hex->rgb hex)
    (and red (format nil "rgba(~D, ~D, ~D, ~,2F)" red green blue alpha))))

(defparameter *named-colors*
  '(("window_bg_color" :background t)
    ("headerbar_bg_color" :background t t)
    ("headerbar_backdrop_color" :background t)
    ("view_bg_color" :surface t)
    ("card_bg_color" :surface t)
    ("popover_bg_color" :surface nil)
    ("window_fg_color" :foreground nil)
    ("headerbar_fg_color" :foreground nil)
    ("view_fg_color" :foreground nil)
    ("card_fg_color" :foreground nil)
    ("popover_fg_color" :foreground nil)
    ("accent_bg_color" :accent nil)
    ("accent_color" :accent nil)
    ("accent_fg_color" :background nil)
    ("error_color" :error nil)
    ("destructive_bg_color" :error nil))
  "The named colors of libadwaita that the theme sets. Each is the name, the
key of the theme that gives its color, true if it takes the alpha and true
if the color is darkened by the :header-shade of the theme.")

(defun shade (color factor)
  "Return the hex COLOR with each of its channels multiplied by FACTOR, a
number from 0 to 1, as #RRGGBB. Return COLOR as it is when it is not a hex
color."
  (multiple-value-bind (red green blue) (hex->rgb color)
    (if red
        (format nil "#~{~2,'0X~}"
                (mapcar (lambda (channel) (round (* channel factor)))
                        (list red green blue)))
        color)))

(defun lighten (color amount)
  "Return the hex COLOR with each of its channels moved AMOUNT of the way
toward white, as #RRGGBB, where AMOUNT is a number from 0 to 1. Return COLOR
as it is when it is not a hex color."
  (multiple-value-bind (red green blue) (hex->rgb color)
    (if red
        (format nil "#~{~2,'0X~}"
                (mapcar (lambda (channel)
                          (round (+ channel (* (- 255 channel) amount))))
                        (list red green blue)))
        color)))

(defun css-rule (selector &rest declarations)
  "Return the CSS rule for SELECTOR holding the DECLARATIONS that are not
nil. Each declaration is a string such as \"color: red\"."
  (format nil "~A {~%~{  ~A;~%~}}~%" selector (remove nil declarations)))

(defparameter *inputs*
  '((:input-bt :bluetooth)
    (:input-aux :aux)
    (:input-usb :usb))
  "The inputs of the speaker, each as the keyword of the view and the key of
its color in the theme. The button of an input has the style class
lpz407-input, one named after the input, such as lpz407-input-bt, and
lpz407-active while the input is the active one.")

(defun button-rules (selector background text &optional (hover 0.8)
                                                        (pressed 0.6)
                                                        hover-text)
  "Return the CSS rules that give the buttons matching SELECTOR the color
BACKGROUND and the text color TEXT. The color is shaded by the factor HOVER
under the pointer and by PRESSED while pressed. When HOVER-TEXT is a color,
it is the text color under the pointer and while pressed, for a button whose
shaded color would hide TEXT. Return an empty string when BACKGROUND or TEXT
is not a color."
  (if (and (color-css background 1) (color-css text 1))
      (let ((over (and (color-css hover-text 1)
                       (format nil "color: ~A" hover-text))))
        (concatenate
         'string
         (css-rule selector
                   (format nil "background: ~A" background)
                   (format nil "color: ~A" text)
                   "transition: background 150ms")
         (css-rule (format nil "~A:hover" selector)
                   (format nil "background: ~A" (shade background hover))
                   over)
         (css-rule (format nil "~A:active" selector)
                   (format nil "background: ~A" (shade background pressed))
                   over)))
      ""))

(defun input-rules (input color text ring hover-text)
  "Return the CSS rules for the button of INPUT, a keyword of *INPUTS*. The
button is filled with COLOR and has TEXT as its text color, like the other
buttons, shaded to 70% under the pointer and 40% while pressed, when the text
turns to HOVER-TEXT. It gets a ring of the color RING while INPUT is the
active one."
  (let ((selector (format nil "window.lpz407 button.lpz407-~(~A~)" input)))
    (concatenate
     'string
     (button-rules selector color text 0.7 0.4 hover-text)
     (if (color-css ring 1)
         (css-rule (format nil "~A.lpz407-active" selector)
                   (format nil "box-shadow: inset 0 0 0 2px ~A" ring))
         ""))))

(defun status-rules (theme)
  "Return the CSS rules that color the status text of the connection row.
The row has the style class lpz407-status-connected, -connecting or
-disconnected, and the colors come from the :status keys of THEME."
  (apply #'concatenate 'string
         (loop :for state :in '(:connected :connecting :disconnected)
               :for color := (getf theme
                                   (ecase state
                                     (:connected :status-connected)
                                     (:connecting :status-connecting)
                                     (:disconnected :status-disconnected)))
               :when (color-css color 1)
                 :collect (css-rule
                           (format nil "window.lpz407 .lpz407-status-~(~A~) ~
                                        .title"
                                   state)
                           (format nil "color: ~A" color)))))

(defun disabled-rules ()
  "Return the CSS rule that fades the buttons of the window while they are
disabled, which they are until the speaker is connected. Without it they
look as ready as the rest."
  (css-rule "window.lpz407 button:disabled" "opacity: 0.4"))

(defun row-rules ()
  "Return the CSS rule that takes the focus outline from the rows of the
list. The rows only show the speaker, so a click on one is no reason to
frame it."
  (css-rule (format nil "window.lpz407 row, window.lpz407 row:focus, ~
                         window.lpz407 row:focus-visible")
            "outline: none"))

(defun device-rules (size)
  "Return the CSS rule that sets the text of the device info, which has the
style class lpz407-device, to SIZE times the size of the other text. Return an
empty string when SIZE is not a positive number."
  (if (and (realp size) (plusp size))
      (css-rule "window.lpz407 .lpz407-device"
                (format nil "font-size: ~,2Fem" size))
      ""))

(defun info-rules (color)
  "Return the CSS rule that gives COLOR to the status and established lines
of the device info, which have the style class lpz407-info. Return an empty
string when COLOR is not a color."
  (if (color-css color 1)
      (css-rule "window.lpz407 .lpz407-info" (format nil "color: ~A" color))
      ""))

(defun value-rules (color)
  "Return the CSS rule that gives COLOR to the volume and bass numbers,
which have the style class lpz407-value. Return an empty string when COLOR is
not a color."
  (if (color-css color 1)
      (css-rule "window.lpz407 .lpz407-value" (format nil "color: ~A" color))
      ""))

(defun level-rules (background text)
  "Return the CSS rules for the buttons that change a level. They have the
color BACKGROUND and the text color TEXT, and get darker under the pointer
and while pressed. Return an empty string when either is not a color."
  (button-rules "window.lpz407 button.lpz407-level" background text 0.8 0.6))

(defun close-rules (background text)
  "Return the CSS rules for the close button of the window. The button has
no color of its own; libadwaita draws a circle in its image, and that circle
becomes BACKGROUND, with the icon in TEXT, under the pointer. Return an empty
string when either is not a color."
  (let ((selector "window.lpz407 windowcontrols button.close"))
    (if (and (color-css background 1) (color-css text 1))
        (concatenate
         'string
         (css-rule (format nil "~A:hover image" selector)
                   (format nil "background: ~A" background)
                   (format nil "color: ~A" text))
         (css-rule (format nil "~A:active image" selector)
                   (format nil "background: ~A" (shade background 0.8))
                   (format nil "color: ~A" text)))
        "")))

(defun action-rules (theme)
  "Return the CSS rules for the buttons of THEME that are not inputs: the
connect and disconnect buttons, which are filled with their colors (the
connect button shades to 70% and 40%; both turn their text to the
foreground color then), the close button of the
window, which shows its color only under the pointer, the buttons that
change a level, which have the :level color, and the look of any button
that is disabled."
  (let ((dark (getf theme :background))
        (light (getf theme :foreground)))
    (concatenate
     'string
     (button-rules "window.lpz407 button.lpz407-connect"
                   (getf theme :connect) (getf theme :connect-text) 0.7 0.4
                   (getf theme :foreground))
     (button-rules "window.lpz407 button.lpz407-disconnect"
                   (getf theme :disconnect) dark 0.8 0.6 light)
     (info-rules (getf theme :info))
     (value-rules (getf theme :value))
     (device-rules (getf theme :info-size))
     (close-rules (getf theme :close) dark)
     (level-rules (getf theme :level) light)
     (disabled-rules)
     (row-rules))))

(defun check-colors (theme)
  "Warn about each color of THEME that is not a color like #2e3440."
  (loop :for key :in '(:background :surface :foreground :accent :error
                       :border :bluetooth :aux :usb :disconnect :close
                       :connect :connect-text :info :active :value
                       :level
                       :status-connected :status-connecting
                       :status-disconnected)
        :unless (color-css (getf theme key) 1)
          :do (warn "theme: ~S for ~(~S~) is not a color like #2e3440; ~
                     ignoring it"
                    (getf theme key) key)))

(defun theme-css (theme)
  "Return the CSS for THEME, the :theme plist of the config. It sets the
named colors of libadwaita both as CSS variables, which current versions
read, and as @define-color, which older ones read. Rules for the window, its
header bar and its cards then give them the alpha of the theme directly. The
header bar is darker by the :header-shade of the theme while the window has
the focus, and has the background color otherwise."
  (check-colors theme)
  (let ((alpha (getf theme :alpha))
        (font (getf theme :font))
        (focused (- 1 (getf theme :header-shade)))
        (colors '()))
    (loop :for (name key alpha-p shaded-p) :in *named-colors*
          :for css := (color-css (if shaded-p
                                     (shade (getf theme key) focused)
                                     (getf theme key))
                                 (if alpha-p alpha 1))
          :when css
            :do (push (cons name css) colors))
    (setf colors (nreverse colors))
    (flet ((paint (key)
             (color-css (getf theme key) alpha)))
      (with-output-to-string (out)
        (dolist (color colors)
          (format out "@define-color ~A ~A;~%" (car color) (cdr color)))
        (format out ":root {~%")
        (dolist (color colors)
          (format out "  --~A: ~A;~%"
                  (substitute #\- #\_ (car color)) (cdr color)))
        (format out "}~%")
        (when (paint :background)
          (format out "window.lpz407 { background-color: ~A; }~%"
                  (paint :background))
          (format out "window.lpz407 headerbar { background-color: ~A; ~
                       background-image: none; box-shadow: none; }~%"
                  (color-css (shade (getf theme :background) focused) alpha))
          (format out "window.lpz407:backdrop headerbar { ~
                       background-color: ~A; }~%"
                  (paint :background)))
        (when (paint :surface)
          (format out "window.lpz407 .boxed-list { background-color: ~A; ~
                       border: 1px solid ~A; border-radius: 10px; }~%"
                  (paint :surface) (getf theme :border)))
        (when font
          (format out "window.lpz407 { font-family: ~S; }~%" font))
        (loop :for (input key) :in *inputs*
              :do (write-string (input-rules input (getf theme key)
                                             (getf theme :background)
                                             (getf theme :active)
                                             (getf theme :foreground))
                                out))
        (write-string (status-rules theme) out)
        (write-string (action-rules theme) out)))))

(defun load-css (css priority widget)
  "Make the string CSS apply to the display of WIDGET at PRIORITY."
  (let ((provider (gtk:make-css-provider)))
    (gtk:css-provider-load-from-data provider css)
    (gtk:style-context-add-provider-for-display
     (gtk:widget-display widget) provider priority)))

(defun read-file (path)
  "Return the contents of the file at PATH as a string."
  (with-open-file (in path :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))

(defun apply-theme (theme widget)
  "Apply THEME, the :theme plist of the config, to the display of WIDGET.
The colors come first and the CSS file of the theme, if any, after them, so
that the file can override them. A failure only warns, since the window
works without its colors."
  (handler-case
      (let ((manager (adw:style-manager-default)))
        (setf (adw:style-manager-color-scheme manager)
              (if (eq :light (getf theme :scheme))
                  adw:+color-scheme-force-light+
                  adw:+color-scheme-force-dark+)))
    (error (condition)
      (warn "theme: cannot set the color scheme: ~A" condition)))
  (handler-case
      (progn
        (load-css (theme-css theme)
                  gtk:+style-provider-priority-application+ widget)
        (let ((file (getf theme :css)))
          (when file
            (load-css (read-file file)
                      gtk:+style-provider-priority-user+ widget))))
    (error (condition)
      (warn "theme: cannot load the CSS: ~A" condition))))
