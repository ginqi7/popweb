;;; popweb-buffer.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'popweb-url)
(require 'htmlize)
(require 'shr)

(defvar popweb-buffer--temp-file (make-temp-file "popweb-buffer-" nil ".html"))

(defcustom popweb-buffer-en-font ""
  "Font for en.")

(defcustom popweb-buffer-zh-font ""
  "Font for zh.")

(defcustom popweb-buffer-font-size '(9 . 20)
  "Font size: (width . height)")

(defcustom popweb-buffer-min-size '(200 . 100)
  "min size: (width . height)")

(defcustom popweb-buffer-max-size '(1000 . 500)
  "min size: (width . height)")

(defun popweb-buffer--show (&rest info)
  "Pop window to show current url preview INFO."
  (let* ((position (popweb-get-cursor-coordinate))
         (window-header-height
          (-
           (nth 1 (window-inside-pixel-edges))
           (nth 1 (window-absolute-pixel-edges))))
         (x (car position))
         (y (- (cdr position) window-header-height))
         (x-offset (popweb-get-cursor-x-offset))
         (y-offset (popweb-get-cursor-y-offset))
         (frame-x (car (frame-position)))
         (frame-y (cdr (frame-position)))
         (frame-w (frame-outer-width))
         (frame-h (frame-outer-height))
         (url (plist-get info :url))
         (width (plist-get info :width))
         (height (plist-get info :height)))
    (popweb-call-async "call_module_method" popweb-url-module-path
                       "pop_url_window"
                       (list
                        "url-preview"
                        x y x-offset y-offset
                        frame-x frame-y frame-w frame-h
                        popweb-url-web-window-width-scale
                        popweb-url-web-window-height-scale
                        width
                        height
                        popweb-url-web-window-size-use-absolute
                        url))
    (popweb-url-web-window-can-hide)))


(defun popweb-buffer--save-htmlize-buffer (buffer)
  "Save the buffer to htmlized html file."
  (let ((htmlized-buffer)
        (htmlize-head-tags
         (format "<style type=\"text/css\">
                     pre {
                          font-family: %s, %s, sans-serif;
                     }
                  </style>"
                 popweb-buffer-en-font popweb-buffer-zh-font)))
    (with-current-buffer buffer
      (setq htmlized-buffer (htmlize-region (point-min) (point-max))))
    (with-current-buffer htmlized-buffer
      (write-file popweb-buffer--temp-file))
    (kill-buffer htmlized-buffer)))

(defun popweb-buffer--height (buffer)
  (with-current-buffer buffer
    (goto-char (point-max))
    (min (cdr popweb-buffer-max-size)
         (max (cdr popweb-buffer-min-size)
              (* (cdr popweb-buffer-font-size) popweb-zoom-factor (array-current-line))))))


(defun popweb-buffer--width (buffer)
  (with-current-buffer buffer
    (min (car popweb-buffer-max-size)
         (max (car popweb-buffer-min-size)
              (* (car popweb-buffer-font-size) popweb-zoom-factor (shr-buffer-width))))))

(defun popweb-buffer (&optional buffer)
  "Preview a buffer."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (popweb-buffer--save-htmlize-buffer buffer)
  (popweb-buffer--show :url (concat "file://" popweb-buffer--temp-file)
                       :width (popweb-buffer--width buffer)
                       :height (popweb-buffer--height buffer))
  (add-hook 'post-command-hook #'popweb-url-web-window-hide-after-move))

(defun popweb-buffer-str (str)
  "Preview a string"
  (with-current-buffer (get-buffer-create "*popweb-buffer*")
    (erase-buffer)
    (insert str)
    (popweb-buffer)))

(provide 'popweb-buffer)
;;; popweb-buffer.el ends here
