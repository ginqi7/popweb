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

(defvar popweb-buffer--temp-file (make-temp-file "popweb-buffer-" nil ".html"))

(defun popweb-buffer--save-htmlize-buffer (buffer)
  "Save the buffer to htmlized html file."
  (let (htmlized-buffer)
    (with-current-buffer buffer
      (setq htmlized-buffer (htmlize-region (point-min) (point-max))))
    (with-current-buffer htmlized-buffer
      (write-file popweb-buffer--temp-file))
    (kill-buffer htmlized-buffer)))

(defun popweb-buffer (&optional buffer)
  "Preview a buffer."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (popweb-buffer--save-htmlize-buffer buffer)
  (popweb-url-input (concat "file://" popweb-buffer--temp-file)))


(provide 'popweb-buffer)
;;; popweb-buffer.el ends here
