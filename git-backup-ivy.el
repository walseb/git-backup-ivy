;;; git-backup-ivy.el --- An ivy interface to git-backup -*- lexical-binding: t -*-

;; Author: Sebastian Wålinder <s.walinder@gmail.com>
;; URL: https://github.com/walseb/git-backup-ivy
;; Version: 1.0
;; Package-Requires: ((ivy "0.12.0") (git-backup "0.0.1") (emacs "24"))
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
;; This package is a interface to git-backup

;; For options like ediff or open in new buffer, run `ivy-dispatching-done'
;; while the interface is open. By default this is bound to C-o

;;; Code:
(require 'ivy)
(require 'git-backup)

(defgroup git-backup-ivy nil
  "Interface to backup system git-backup using ivy."
  :group 'ivy)

(defcustom git-backup-ivy-git-path (executable-find "git")
  "Path to a git binary."
  :group 'git-backup-ivy
  :type 'string)

(defcustom git-backup-ivy-backup-path (expand-file-name (concat user-emacs-directory ".git-backup"))
  "The path where backups are stored."
  :group 'git-backup-ivy
  :type 'string)

(defcustom git-backup-ivy-list-format "%cd, %ar"
  "Format use to display entries in ivy buffer, follow git log format."
  :group 'git-backup-ivy
  :type 'string)

;;;###autoload
(defun git-backup-ivy ()
  "Main function to bring up interface for interacting with git-backup."
  (interactive)
  (let ((candidates (git-backup-list-file-change-time git-backup-ivy-git-path git-backup-ivy-backup-path git-backup-ivy-list-format (buffer-file-name))))
    (if candidates
	(ivy-read
	 (format "Backup for %s" (buffer-file-name))
	 candidates
	 :action (lambda (candidate)
		   (git-backup-replace-current-buffer git-backup-ivy-git-path git-backup-ivy-backup-path (cdr candidate) (buffer-file-name))))
      (error "No filename associated with buffer, file has no backup yet or filename is blacklisted"))))

(ivy-set-actions
 'git-backup-ivy
 '(("e" (lambda (candidate)
          (git-backup-create-ediff git-backup-ivy-git-path git-backup-ivy-backup-path (cdr candidate) (current-buffer))) "Ediff file with backup")
   ("f" (lambda (candidate)
          (git-backup-open-in-new-buffer git-backup-ivy-git-path git-backup-ivy-backup-path (cdr candidate) (buffer-file-name))) "Open in new buffer")
   ("D" (lambda (candidate)
	  (when (yes-or-no-p "Really delete all backups of this file?")
	    (git-backup-remove-file-backups git-backup-ivy-git-path git-backup-ivy-backup-path (buffer-file-name)))) "Delete all backups of file")))

(provide 'git-backup-ivy)

;;; git-backup-ivy.el ends here
