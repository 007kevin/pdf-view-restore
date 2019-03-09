;;; pdf-view-restore.el --- Support for reopening pdf to last position opened

;; Copyright (C) 2019 Kevin Kim

;; Author: Kevin Kim <kevinkim1991@gmail.com>
;; Maintainer: Kevin Kim <kevinkim1991@gmail.com>
;; Keywords: pdf-view, pdf-tools
;; Version: 0.1
;; Package-Requires: ((pdf-tools "0.80"))

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
;; Support for saving and opening last known pdf position. Information
;; will be saved relative to the pdf being viewed so ensure
;; `pdf-view-restore-filename' is in the same directory as the viewing pdf.
;;
;; To enable, add the following hooks:
;;   (add-hook 'kill-buffer-hook 'pdf-view-restore-save)
;;   (add-hook 'kill-emacs-hook 'pdf-view-restore-save)
;;   (add-hook 'pdf-view-mode-hook 'pdf-view-restore)


;;; Code:
(require 'pdf-view)
(require 'subr-x) ;; For if-let macro

(defcustom pdf-view-restore-filename ".pdf-view-restore"
  "Filename to save last known pdf position"
  :group 'pdf-view-restore
  :type 'string)

(defun pdf-view-restore ()
  "Restore page"
  (when (eq major-mode 'pdf-view-mode)
    ;; This buffer is in pdf-view-mode
    (pdf-view-goto-page (pdf-view-restore-get-page))))


(defun pdf-view-restore-save ()
  "Save restore information"
  (when (eq major-mode 'pdf-view-mode)
    ;; This buffer is in pdf-view-mode
    (let ((page (pdf-view-current-page)))
      (pdf-view-restore-set-page page))))

(defun pdf-view-restore-get-page ()
  "Return restore page"
  (let* ((alist (pdf-view-restore-unserialize))
         (key (pdf-view-restore-key)))
    (if alist (cdr (assoc key alist)) 0)))

(defun pdf-view-restore-set-page (page)
  "Save restore page"
  (let* ((alist (pdf-view-restore-unserialize))
         (key (pdf-view-restore-key)))
    (pdf-view-restore-serialize (pdf-view-restore-alist-set key page alist))))

(defun pdf-view-restore-alist-set (key val alist &optional symbol)
  "Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets
the cdr of the first matching association in the list. It does
not create duplicate associations. By default, key comparison is
done with `equal'. However, if SYMBOL is non-nil, then `eq' is
used instead.

This method may mutate the original alist, but you still need to
use the return value of this method instead of the original
alist, to ensure correct results."
  (if-let ((pair (if symbol (assq key alist) (assoc key alist))))
      (setcdr pair val)
    (push (cons key val) alist))
  alist)

(defun pdf-view-restore-key () (file-name-base buffer-file-name))

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


(provide 'pdf-view-restore)
;;; pdf-view-restore.el ends here
