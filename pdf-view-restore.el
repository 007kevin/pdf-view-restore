;;; pdf-view-restore.el --- Support for reopening pdf to last position opened

;; Copyright (C) 2019 Kevin Kim

;; Author: Kevin Kim <kevinkim1991@gmail.com>
;; Maintainer: Kevin Kim <kevinkim1991@gmail.com>
;; Keywords: pdf-view, pdf-tools,
;; Version: 0.1
;; Package-Requires: ((pdf-tools "0.80") (org-pdfview "0.10"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Add support to saving and reopening last known pdf position.
;;
;; To enable, add the following hooks:
;;   (add-hook 'kill-buffer-hook 'pdf-view-restore-save)
;;   (add-hook 'kill-emacs-hook 'pdf-view-restore-save)
;;   (add-hook 'pdf-view-mode-hook 'pdf-view-restore)


;;; Code:
(require 'pdf-view)
(require 'org-pdfview)

(defcustom pdf-view-restore-filename ".pdf-view-restore"
  "Filename to save last known pdf position"
  :group 'pdf-view-restore
  :type 'string)

;;; Serialization
(defun pdf-view-restore-serialize (data)
  "Serialize DATA to `pdf-view-restore-filename'.

The saved data can be restored with `pdf-view-restore-unserialize'."
  (when (file-writable-p pdf-view-restore-filename)
    (with-temp-file pdf-view-restore-filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun pdf-view-restore-unserialize ()
  "Read data serialized by `pdf-view-restore-serialize' from `pdf-view-restore-filename'."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p pdf-view-restore-filename)
      (with-temp-buffer
        (insert-file-contents pdf-view-restore-filename)
        ;; this will blow up if the contents of the file aren't
        ;; lisp data structures
        (read (buffer-string))))))

(defun pdf-view-restore-save ()
  "Save restore information"
  (when (eq major-mode 'pdf-view-mode)
    ;; This buffer is in pdf-view-mode
    (let* ((page (pdf-view-current-page))
           (link (concat (file-relative-name buffer-file-name) "::" (number-to-string page))))
      (pdf-view-restore-serialize link))))

(defun pdf-view-restore ()
  "Restore page"
  (when (eq major-mode 'pdf-view-mode)
    ;; This buffer is in pdf-view-mode
    (let ((link (pdf-view-restore-unserialize)))
      (org-pdfview-open link))))

(provide 'pdf-view-restore)
;;; pdf-view-restore.el ends here
