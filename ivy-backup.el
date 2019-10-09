;;; ivy-backup.el --- An ivy frontend to helm-backup -*- lexical-binding: t -*-

;; Author: Sebastian Wålinder <s.walinder@gmail.com>
;; URL: https://github.com/walseb/ivy-backup
;; Version: 1.0
;; Package-Requires: ((ivy "0.12.0") (helm-backup "1.1.1") (emacs "24"))
;; Keywords: backup, convenience, files, tools, vc

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package lets you use ivy to control helm-backup

;; For options like ediff or open in new buffer, run `ivy-dispatching-done'
;; while running ivy-backup. By default this is bound to C-o

;;; Code:
(require 'ivy)
(require 'helm-backup)

;;;###autoload
(defun ivy-backup ()
  "Main function used to call `ivy-backup`."
  (interactive)
  (let ((candidates (helm-backup--list-file-change-time (buffer-file-name))))
    (if candidates
	(ivy-read
	 (format "Backup for %s" (buffer-file-name))
	 candidates
	 :action (lambda (candidate)
		   (with-helm-current-buffer
		     (helm-backup--replace-current-buffer (cdr candidate) (buffer-file-name)))))
      (error "No filename associated with buffer, file has no backup yet or filename is blacklisted"))))

(ivy-set-actions
 'ivy-backup
 '(("e" (lambda (candidate)
          (helm-backup--create-ediff (cdr candidate) (current-buffer))) "Ediff file with backup")
   ("f" (lambda (candidate)
          (helm-backup--open-in-new-buffer (cdr candidate) (buffer-file-name))) "Open in new buffer")))

(provide 'ivy-backup)

;;; ivy-backup.el ends here
