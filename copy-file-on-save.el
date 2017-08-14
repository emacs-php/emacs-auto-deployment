;;; copy-file-on-save.el --- Automatic syncronize when file saved. -*- lexical-binding: t -*-

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
;;     ((nil . ((copy-file-on-save-dest-dir . "/scp:dest-server:/home/your/path/to/proj")
;;              (copy-file-on-save-ignore-patterns . ("/cache")))))
;;

;;; Code:
(require 'cl-lib)
(require 'projectile nil)

(defvar copy-file-on-save-lighter " CopyFS")

(defvar copy-file-on-save-method 'copy-file
  "Method to use deployment file.")
(make-local-variable 'copy-file-on-save-method)

(defvar copy-file-on-save-dest-dir nil
  "Path to deployment directory or convert (mapping) function.")
(make-local-variable 'copy-file-on-save-dest-dir)

(defvar copy-file-on-save-ignore-patterns '()
  "Ignore deploy when buffer-filename matched by these patterns.")
(make-local-variable 'copy-file-on-save-ignore-patterns)

(defvar copy-file-on-save-base-dir nil
  "Path to base directory for deployment.")

(defun copy-file-on-save--not-matches-ignore-patterns (filename)
  "Return t if FILENAME matched by `copy-file-on-save-ignore-patterns'."
  (cl-loop for pattern in copy-file-on-save-ignore-patterns
           never (string-match-p pattern filename)))

(defun copy-file-on-save--hook-after-save ()
  "copy-file-on-save hook for after-save."
  (when (and buffer-file-name copy-file-on-save-dest-dir
             (copy-file-on-save--not-matches-ignore-patterns buffer-file-name))
    (copy-file-on-save--method copy-file-on-save-method)))

(defun copy-file-on-save--detect-project-root ()
  "Return path to project root directory."
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (file-truename (locate-dominating-file buffer-file-name ".dir-locals.el"))))

(defun copy-file-on-save--base-dir ()
  "Return path to project directory."
  (cond
   ((null copy-file-on-save-base-dir) (copy-file-on-save--detect-project-root))
   ((stringp copy-file-on-save-base-dir) copy-file-on-save-base-dir)
   ((fboundp copy-file-on-save-base-dir) (funcall copy-file-on-save-base-dir))
   (t (error "Variable copy-file-on-save-base-dir `%s' is invalid value" copy-file-on-save-base-dir))))

(defun copy-file-on-save--method (method-symbol)
  "Invoke copy-file-on-save method by `METHOD-SYMBOL'."
  (cl-case method-symbol
    (copy-file (copy-file-on-save--copy-file))
    (t (funcall method-symbol))))

(defun copy-file-on-save--copy-file ()
  "Deployment a file using `copy-file'."
  (let ((from-path buffer-file-name)
        (to-path   (copy-file-on-save--replace-path buffer-file-name)))
    (copy-file from-path to-path t)))

(defun copy-file-on-save--replace-path (src-file-path)
  "Return replace dest file path by `SRC-FILE-PATH'."
  (f-join copy-file-on-save-dest-dir (s-replace (copy-file-on-save--base-dir) "" src-file-path)))

;;;###autoload
(define-minor-mode copy-file-on-save-mode
  "Minor mode for automatic deployment/syncronize file when saved."
  nil copy-file-on-save-lighter nil
  (if copy-file-on-save-mode
      (add-hook 'after-save-hook 'copy-file-on-save--hook-after-save nil t)
    (remove-hook 'after-save-hook 'copy-file-on-save--hook-after-save t)))

(provide 'copy-file-on-save)
;;; copy-file-on-save.el ends here
