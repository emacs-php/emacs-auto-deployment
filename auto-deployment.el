;;; auto-deployment.el --- Automatic syncronize when file saved. -*- lexical-binding: t -*-

;; Copyright (C) 2017 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 27 Jul 2017
;; Version: 0.0.1
;; Keywords: tools php dependency manager
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5") (f "0.17") (s "1.7.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put the following into your .dir-locals.el in project root directory.
;;
;;     ((nil . ((auto-deployment-dest-dir . "/scp:dest-server:/home/your/path/to/proj")
;;              (auto-deployment-ignore-patterns . ("/cache")))))
;;

;;; Code:
(require 'cl-lib)
(require 'projectile nil)

(defvar auto-deployment-lighter " AutoDeploy")

(defvar auto-deployment-method 'copy-file
  "Method to use deployment file.")
(make-local-variable 'auto-deployment-method)

(defvar auto-deployment-dest-dir nil
  "Path to deployment directory or convert (mapping) function.")
(make-local-variable 'auto-deployment-dest-dir)

(defvar auto-deployment-ignore-patterns '()
  "Ignore deploy when buffer-filename matched by these patterns.")
(make-local-variable 'auto-deployment-ignore-patterns)

(defvar auto-deployment-base-dir nil
  "Path to base directory for deployment.")

(defun auto-deployment--not-matches-ignore-patterns (filename)
  "Return t if FILENAME matched by `auto-deployment-ignore-patterns'."
  (cl-loop for pattern in auto-deployment-ignore-patterns
           never (string-match-p pattern filename)))

(defun auto-deployment--hook-after-save ()
  "Auto-deployment hook for after-save."
  (when (and buffer-file-name auto-deployment-dest-dir
             (auto-deployment--not-matches-ignore-patterns buffer-file-name))
    (auto-deployment--method auto-deployment-method)))

(defun auto-deployment--detect-project-root ()
  "Return path to project root directory."
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (file-truename (locate-dominating-file buffer-file-name ".dir-locals.el"))))

(defun auto-deployment--base-dir ()
  "Return path to project directory."
  (cond
   ((null auto-deployment-base-dir) (auto-deployment--detect-project-root))
   ((stringp auto-deployment-base-dir) auto-deployment-base-dir)
   ((fboundp auto-deployment-base-dir) (funcall auto-deployment-base-dir))
   (t (error "Variable auto-deployment-base-dir `%s' is invalid value" auto-deployment-base-dir))))

(defun auto-deployment--method (method-symbol)
  "Invoke auto-deployment method by `METHOD-SYMBOL'."
  (cl-case method-symbol
    (copy-file (auto-deployment--copy-file))
    (t (funcall method-symbol))))

(defun auto-deployment--copy-file ()
  "Deployment a file using `copy-file'."
  (let ((from-path buffer-file-name)
        (to-path   (auto-deployment--replace-path buffer-file-name)))
    (copy-file from-path to-path t)))

(defun auto-deployment--replace-path (src-file-path)
  "Return replace dest file path by `SRC-FILE-PATH'."
  (f-join auto-deployment-dest-dir (s-replace (auto-deployment--base-dir) "" src-file-path)))

;;;###autoload
(define-minor-mode auto-deployment-mode
  "Minormode for automatic deployment/syncronize file when saved."
  nil auto-deployment-lighter nil
  (if auto-deployment-mode
      (add-hook 'after-save-hook 'auto-deployment--hook-after-save nil t)
    (remove-hook 'after-save-hook 'auto-deployment--hook-after-save t)))

(provide 'auto-deployment)
;;; auto-deployment.el ends here
