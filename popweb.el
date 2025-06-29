;;; popweb.el --- Cyber dict  -*- lexical-binding: t; -*-

;; Filename: popweb.el
;; Description: Cyber dict
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: Sun Nov 21 04:35:02 2021 (-0500)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/manateelazycat/popweb
;; Keywords:
;; Compatibility: emacs-version >= 27
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Popweb
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET popweb RET
;;

;;; Change log:
;;
;; 2021/11/13
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'popweb-epc)

(defgroup popweb nil
  "POPWEB group."
  :group 'applications)

(defvar popweb-server nil
  "The POPWEB Server.")

(defvar popweb-python-file (expand-file-name "popweb.py" (file-name-directory load-file-name)))

(defvar popweb-server-port nil)

(defun popweb--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p popweb-server)
    (setq popweb-server
          (popweb-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (popweb-epc-define-method mngr 'eval-in-emacs 'popweb--eval-in-emacs-func)
               (popweb-epc-define-method mngr 'get-emacs-var 'popweb--get-emacs-var-func)
               (popweb-epc-define-method mngr 'get-emacs-vars 'popweb--get-emacs-vars-func)
               ))))
    (if popweb-server
        (setq popweb-server-port (process-contact popweb-server :service))
      (error "[POPWEB] popweb-server failed to start")))
  popweb-server)

(defun popweb--eval-in-emacs-func (&rest args)
  (apply (read (car args))
         (mapcar
          (lambda (arg)
            (let ((arg (popweb--decode-string arg)))
              (cond ((string-prefix-p "'" arg) ;; single quote
                     (read (substring arg 1)))
                    ((and (string-prefix-p "(" arg)
                          (string-suffix-p ")" arg)) ;; list
                     (split-string (substring arg 1 -1) " "))
                    (t arg))))
          (cdr args))))

(defun popweb--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun popweb--get-emacs-vars-func (&rest vars)
  (mapcar #'popweb--get-emacs-var-func vars))

(defvar popweb-epc-process nil)

(defvar popweb-internal-process nil)
(defvar popweb-internal-process-prog nil)
(defvar popweb-internal-process-args nil)

(defvar popweb--first-start-args nil)

(defvar popweb--first-start-callback nil)

(defcustom popweb-name "*popweb*"
  "Name of POPWEB buffer."
  :type 'string)

(defcustom popweb-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run popweb.py."
  :type 'string)

(defcustom popweb-proxy-host ""
  "Proxy Host used by POPWEB Browser."
  :type 'string)

(defcustom popweb-proxy-port ""
  "Proxy Port used by POPWEB Browser."
  :type 'string)

(defcustom popweb-proxy-type ""
  "Proxy Type used by POPWEB Browser.  The value is either \"http\" or \"socks5\"."
  :type 'string)

(defcustom popweb-popup-pos "point-bottom-right"
  "The position to display the poweb popup.

Available options:
- \"point-bottom\": bottom of the cursor
- \"point-bottom-right\": bottom-right of the cursor
- \"top-left\": top-left of the screen
- \"top-right\": top-right of the screen
- \"bottom-left\": bottom-left: of the screen
- \"bottom-right\": bottom-right of the screen
- \"middle\": middle of the screen"
  :type 'string)

(defcustom popweb-enable-debug nil
  "If you got segfault error, please turn this option.
Then POPWEB will start by gdb, please send new issue with `*popweb*' buffer content when next crash."
  :type 'boolean)

(defcustom popweb-enable-developer-tools nil
  "When you want use web developer tools, turn on this option."
  :type 'boolean)

(defcustom popweb-start-python-process-when-require t
  "Start POPWEB python process when require `popweb', default is turn on.

Turn on this option will improve start speed."
  :type 'boolean)

(defcustom popweb-zoom-factor 1
  "Zoom factor for web page."
  :type 'integer)

(defcustom popweb-config-location (expand-file-name (locate-user-emacs-file "popweb/"))
  "Directory where popweb will store configuration files."
  :type 'directory)

(defcustom popweb-use-mouse-position nil
  "Dose it use mouse-position to locate the popweb frame?"
  :type 'boolean)


(defun popweb-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (popweb-deferred-chain
    (popweb-epc-call-deferred popweb-epc-process (read method) args)))

(defun popweb-call-sync (method &rest args)
  "Call Python EPC function METHOD and ARGS synchronously."
  (popweb-epc-call-sync popweb-epc-process (read method) args))

(defun popweb-emacs-running-in-wayland-native ()
  (eq window-system 'pgtk))

(defun popweb--build-process-environment ()
  (let ((environments (seq-filter
                       (lambda (var)
                         (and (not (string-match-p "QT_SCALE_FACTOR" var))
                              (not (string-match-p "QT_SCREEN_SCALE_FACTOR" var))))
                       process-environment)))
    (when popweb-enable-debug
      (add-to-list 'environments "QT_DEBUG_PLUGINS=1" t))

    (unless (eq system-type 'darwin)
      (add-to-list 'environments
                   (cond
                    ((popweb-emacs-running-in-wayland-native)
                     ;; Wayland native need to set QT_AUTO_SCREEN_SCALE_FACTOR=1
                     ;; otherwise Qt window only have half of screen.
                     "QT_AUTO_SCREEN_SCALE_FACTOR=1")
                    (t
                     ;; XWayland need to set QT_AUTO_SCREEN_SCALE_FACTOR=0
                     ;; otherwise Qt which explicitly force high DPI enabling get scaled TWICE.
                     "QT_AUTO_SCREEN_SCALE_FACTOR=0"))
                   t)

      (add-to-list 'environments "QT_FONT_DPI=96" t)

      ;; Make sure EAF application scale support 4k screen.
      (add-to-list 'environments "QT_SCALE_FACTOR=1" t)

      ;; Fix CORS problem.
      (add-to-list 'environments "QTWEBENGINE_CHROMIUM_FLAGS=--disable-web-security" t)

      ;; Use XCB for input event transfer.
      ;; Only enable this option on Linux platform.
      (when (and (eq system-type 'gnu/linux)
                 (not (popweb-emacs-running-in-wayland-native)))
        (add-to-list 'environments "QT_QPA_PLATFORM=xcb" t)))
    environments))

;;;###autoload
(defun popweb-restart-process ()
  "Stop and restart POPWEB process."
  (interactive)
  (popweb-kill-process)
  (popweb-start-process)
  (message "Popweb process restarted."))

(defun popweb-start-process ()
  "Start POPWEB process if it isn't started."
  (unless (popweb-epc-live-p popweb-epc-process)
    ;; start epc server and set `popweb-server-port'
    (popweb--start-epc-server)
    (let* ((popweb-args (append
                         (list popweb-python-file)
                         (list (number-to-string popweb-server-port))
                         ))
           environments)

      ;; Folow system DPI.
      (setq environments (popweb--build-process-environment))

      ;; Set process arguments.
      (if popweb-enable-debug
          (progn
            (setq popweb-internal-process-prog "gdb")
            (setq popweb-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" popweb-python-command) popweb-args)))
        (setq popweb-internal-process-prog popweb-python-command)
        (setq popweb-internal-process-args popweb-args))

      ;; Start python process.
      (let ((process-connection-type (not (popweb--called-from-wsl-on-windows-p)))
            (process-environment environments))
        (setq popweb-internal-process
              (apply 'start-process
                     popweb-name popweb-name
                     popweb-internal-process-prog popweb-internal-process-args)))
      (set-process-query-on-exit-flag popweb-internal-process nil))))

(defun popweb--called-from-wsl-on-windows-p ()
  "Check whether popweb is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)
       (string-match-p ".exe" popweb-python-command)))

(run-with-idle-timer
 1 nil
 #'(lambda ()
     ;; Start POPWEB python process when load `popweb'.
     ;; It will improve start speed.
     (when popweb-start-python-process-when-require
       (popweb-start-process))))

(defvar popweb-stop-process-hook nil)

;;;###autoload
(defun popweb-kill-process ()
  "Stop POPWEB process and kill all POPWEB buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'popweb-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (popweb--kill-python-process))

;;;###autoload
(defun popweb--kill-python-process ()
  "Kill POPWEB background python process."
  (interactive)
  (when (popweb-epc-live-p popweb-epc-process)
    ;; Cleanup before exit POPWEB server process.
    (popweb-call-async "cleanup")
    ;; Delete POPWEB server process.
    (popweb-epc-stop-epc popweb-epc-process)
    ;; Kill *popweb* buffer.
    (when (get-buffer popweb-name)
      (kill-buffer popweb-name))
    (message "[POPWEB] Process terminated.")))

(defun popweb--decode-string (str)
  "Decode string STR with UTF-8 coding using Base64."
  (decode-coding-string (base64-decode-string str) 'utf-8))

(defun popweb--encode-string (str)
  "Encode string STR with UTF-8 coding using Base64."
  (base64-encode-string (encode-coding-string str 'utf-8)))

(defun popweb--first-start (popweb-epc-port)
  "Call `popweb--open-internal' upon receiving `start_finish' signal from server.

WEBENGINE-INCLUDE-PRIVATE-CODEC is only useful when app-name is video-player."
  ;; Make EPC process.
  (setq popweb-epc-process (make-popweb-epc-manager
                            :server-process popweb-internal-process
                            :commands (cons popweb-internal-process-prog popweb-internal-process-args)
                            :title (mapconcat 'identity (cons popweb-internal-process-prog popweb-internal-process-args) " ")
                            :port popweb-epc-port
                            :connection (popweb-epc-connect "127.0.0.1" popweb-epc-port)
                            ))
  (popweb-epc-init-epc-layer popweb-epc-process)
  (when popweb--first-start-args
    (funcall popweb--first-start-callback popweb--first-start-args))
  (setq popweb--first-start-args nil))

(defun popweb-get-cursor-coordinate ()
  (if (or (derived-mode-p 'eaf-mode)
          popweb-use-mouse-position)
      (mouse-absolute-pixel-position)
    (window-absolute-pixel-position)))

(defun popweb-get-cursor-x-offset ()
  (if (or (derived-mode-p 'eaf-mode)
          popweb-use-mouse-position)
      30
    0))

(defun popweb-get-cursor-y-offset ()
  (if (or (derived-mode-p 'eaf-mode)
          popweb-use-mouse-position)
      30
    (line-pixel-height)))

(defun popweb-start (first-start-callback args)
  (setq popweb--first-start-callback first-start-callback)

  (if (popweb-epc-live-p popweb-epc-process)
      (funcall first-start-callback args)

    (setq popweb--first-start-args args)
    (popweb-start-process)))

(defun popweb-get-theme-mode ()
  (format "%s" (frame-parameter nil 'background-mode)))

(defun popweb-get-theme-background ()
  (popweb-color-name-to-hex (face-attribute 'default :background)))

(defun popweb-color-int-to-hex (int)
  (substring (format (concat "%0" (int-to-string 4) "X") int) (- 2)))

(defun popweb-color-name-to-hex (color)
  (let ((components (x-color-values color)))
    (concat "#"
            (popweb-color-int-to-hex (nth 0 components))
            (popweb-color-int-to-hex (nth 1 components))
            (popweb-color-int-to-hex (nth 2 components)))))


;;;###autoload
(defun popweb-import-browser-cookies (browser domain-name)
  "Import cookies for the specified domain name."
  (interactive (list (completing-read "Which browser ? "
                                      '("chrome" "chromium" "opera" "opera_gx" "brave" "edge" "vivaldi" "firefox" "safari") nil t)
                     (read-string "Domain name: ")))
  (popweb-start #'(lambda (&rest _)
                    (popweb-call-async "import_browser_cookies" browser domain-name)) nil))


(provide 'popweb)

;;; popweb.el ends here
